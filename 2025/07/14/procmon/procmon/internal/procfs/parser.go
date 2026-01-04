package procfs

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"time"
)

const (
	// ProcFS mount point
	ProcFSPath = "/proc"
	
	// Clock ticks per second (typically 100 on most systems)
	ClockTicksPerSecond = 100
)

// Parser handles reading and parsing data from the /proc filesystem
type Parser struct {
	procPath string
	bootTime time.Time
}

// NewParser creates a new procfs parser
func NewParser() *Parser {
	return &Parser{
		procPath: ProcFSPath,
		bootTime: getBootTime(),
	}
}

// NewParserWithPath creates a new procfs parser with a custom proc path
func NewParserWithPath(procPath string) *Parser {
	return &Parser{
		procPath: procPath,
		bootTime: getBootTime(),
	}
}

// GetProcessList returns a list of all process IDs
func (p *Parser) GetProcessList() ([]int32, error) {
	entries, err := os.ReadDir(p.procPath)
	if err != nil {
		return nil, fmt.Errorf("failed to read proc directory: %w", err)
	}

	var pids []int32
	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}

		// Check if directory name is a number (PID)
		if pid, err := strconv.ParseInt(entry.Name(), 10, 32); err == nil {
			pids = append(pids, int32(pid))
		}
	}

	return pids, nil
}

// GetProcessInfo reads comprehensive process information for a given PID
func (p *Parser) GetProcessInfo(pid int32) (*RawProcessInfo, error) {
	procDir := filepath.Join(p.procPath, strconv.Itoa(int(pid)))

	// Check if process directory exists
	if _, err := os.Stat(procDir); os.IsNotExist(err) {
		return nil, fmt.Errorf("process %d does not exist", pid)
	}

	info := &RawProcessInfo{
		PID:            pid,
		CollectionTime: time.Now(),
		Reliability:    1.0,
	}

	// Read basic process information from /proc/[pid]/stat
	if err := p.parseStatFile(info); err != nil {
		return nil, fmt.Errorf("failed to parse stat file: %w", err)
	}

	// Read additional information from /proc/[pid]/status
	if err := p.parseStatusFile(info); err != nil {
		// Status file is optional, log but don't fail
		info.Reliability *= 0.9
	}

	// Read command line
	if err := p.parseCmdlineFile(info); err != nil {
		// Cmdline file is optional, log but don't fail
		info.Reliability *= 0.95
	}

	// Read thread information
	if err := p.parseThreads(info); err != nil {
		// Thread information is optional, log but don't fail
		info.Reliability *= 0.9
	}

	return info, nil
}

// parseStatFile parses the /proc/[pid]/stat file
func (p *Parser) parseStatFile(info *RawProcessInfo) error {
	statPath := filepath.Join(p.procPath, strconv.Itoa(int(info.PID)), "stat")
	
	data, err := os.ReadFile(statPath)
	if err != nil {
		return fmt.Errorf("failed to read stat file: %w", err)
	}

	// Parse the stat file format
	// See proc(5) man page for field descriptions
	fields := strings.Fields(string(data))
	if len(fields) < 44 {
		return fmt.Errorf("stat file has insufficient fields: %d", len(fields))
	}

	// Field 1: PID (already have this)
	
	// Field 2: Command name (in parentheses)
	info.Name = strings.Trim(fields[1], "()")
	
	// Field 3: State
	info.State = ProcessState(fields[2])
	
	// Field 4: PPID
	if ppid, err := strconv.ParseInt(fields[3], 10, 32); err == nil {
		info.PPID = int32(ppid)
	}
	
	// Field 18: Priority
	if priority, err := strconv.ParseInt(fields[17], 10, 32); err == nil {
		info.Priority = int32(priority)
	}
	
	// Field 19: Nice value
	if nice, err := strconv.ParseInt(fields[18], 10, 32); err == nil {
		info.Nice = int32(nice)
	}
	
	// Field 20: Number of threads
	if numThreads, err := strconv.ParseInt(fields[19], 10, 32); err == nil {
		info.ThreadCount = int32(numThreads)
	}
	
	// Field 22: Start time (in clock ticks since boot)
	if startTicks, err := strconv.ParseUint(fields[21], 10, 64); err == nil {
		startTime := p.bootTime.Add(time.Duration(startTicks) * time.Second / ClockTicksPerSecond)
		info.StartTime = startTime
	}
	
	// Fields 14-17: CPU time information
	var utime, stime, cutime, cstime uint64
	if val, err := strconv.ParseUint(fields[13], 10, 64); err == nil {
		utime = val
	}
	if val, err := strconv.ParseUint(fields[14], 10, 64); err == nil {
		stime = val
	}
	if val, err := strconv.ParseUint(fields[15], 10, 64); err == nil {
		cutime = val
	}
	if val, err := strconv.ParseUint(fields[16], 10, 64); err == nil {
		cstime = val
	}
	
	// Calculate CPU usage information
	info.UserTicks = utime
	info.SystemTicks = stime
	
	// Calculate total CPU time
	totalTicks := utime + stime + cutime + cstime
	info.CPUTime = time.Duration(totalTicks) * time.Second / ClockTicksPerSecond
	
	// Fields 23-24: Virtual and resident memory size
	if vsize, err := strconv.ParseUint(fields[22], 10, 64); err == nil {
		info.VirtualSize = vsize
	}
	if rss, err := strconv.ParseUint(fields[23], 10, 64); err == nil {
		// RSS is in pages, convert to bytes (assuming 4KB pages)
		info.ResidentSize = rss * 4096
	}

	return nil
}

// parseStatusFile parses the /proc/[pid]/status file for additional information
func (p *Parser) parseStatusFile(info *RawProcessInfo) error {
	statusPath := filepath.Join(p.procPath, strconv.Itoa(int(info.PID)), "status")
	
	file, err := os.Open(statusPath)
	if err != nil {
		return fmt.Errorf("failed to open status file: %w", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.SplitN(line, ":", 2)
		if len(parts) != 2 {
			continue
		}
		
		key := strings.TrimSpace(parts[0])
		value := strings.TrimSpace(parts[1])
		
		switch key {
		case "Uid":
			// Real UID is the first value
			fields := strings.Fields(value)
			if len(fields) > 0 {
				if uid, err := strconv.ParseUint(fields[0], 10, 32); err == nil {
					info.UID = uint32(uid)
				}
			}
		case "Gid":
			// Real GID is the first value
			fields := strings.Fields(value)
			if len(fields) > 0 {
				if gid, err := strconv.ParseUint(fields[0], 10, 32); err == nil {
					info.GID = uint32(gid)
				}
			}
		case "VmSize":
			if size, err := parseMemoryValue(value); err == nil {
				info.VirtualSize = size
			}
		case "VmRSS":
			if size, err := parseMemoryValue(value); err == nil {
				info.ResidentSize = size
			}
		case "VmSwap":
			if size, err := parseMemoryValue(value); err == nil {
				info.SwapSize = size
			}
		case "Threads":
			if threads, err := strconv.ParseInt(value, 10, 32); err == nil {
				info.ThreadCount = int32(threads)
			}
		}
	}

	return scanner.Err()
}

// parseCmdlineFile reads the command line arguments
func (p *Parser) parseCmdlineFile(info *RawProcessInfo) error {
	cmdlinePath := filepath.Join(p.procPath, strconv.Itoa(int(info.PID)), "cmdline")
	
	data, err := os.ReadFile(cmdlinePath)
	if err != nil {
		return fmt.Errorf("failed to read cmdline file: %w", err)
	}

	// Command line arguments are separated by null bytes
	cmdline := string(data)
	cmdline = strings.ReplaceAll(cmdline, "\x00", " ")
	cmdline = strings.TrimSpace(cmdline)
	
	info.CommandLine = cmdline
	
	// Extract executable path (first argument)
	if len(cmdline) > 0 {
		parts := strings.Fields(cmdline)
		if len(parts) > 0 {
			info.Executable = parts[0]
		}
	}

	return nil
}

// parseThreads reads thread information from /proc/[pid]/task/
func (p *Parser) parseThreads(info *RawProcessInfo) error {
	taskDir := filepath.Join(p.procPath, strconv.Itoa(int(info.PID)), "task")
	
	entries, err := os.ReadDir(taskDir)
	if err != nil {
		return fmt.Errorf("failed to read task directory: %w", err)
	}

	var threads []RawThreadInfo
	for _, entry := range entries {
		if !entry.IsDir() {
			continue
		}

		// Check if directory name is a number (TID)
		if tid, err := strconv.ParseInt(entry.Name(), 10, 32); err == nil {
			threadInfo, err := p.getThreadInfo(info.PID, int32(tid))
			if err != nil {
				// Log error but continue with other threads
				continue
			}
			threads = append(threads, *threadInfo)
		}
	}

	info.Threads = threads
	return nil
}

// getThreadInfo reads information for a specific thread
func (p *Parser) getThreadInfo(pid, tid int32) (*RawThreadInfo, error) {
	info := &RawThreadInfo{
		TID:            tid,
		PID:            pid,
		CollectionTime: time.Now(),
	}

	// Read thread stat file
	if err := p.parseThreadStatFile(info); err != nil {
		return nil, fmt.Errorf("failed to parse thread stat file: %w", err)
	}

	// Read thread name from comm file
	if err := p.parseThreadCommFile(info); err != nil {
		// Comm file is optional
	}

	return info, nil
}

// parseThreadStatFile parses the /proc/[pid]/task/[tid]/stat file
func (p *Parser) parseThreadStatFile(info *RawThreadInfo) error {
	statPath := filepath.Join(p.procPath, strconv.Itoa(int(info.PID)), "task", strconv.Itoa(int(info.TID)), "stat")
	
	data, err := os.ReadFile(statPath)
	if err != nil {
		return fmt.Errorf("failed to read thread stat file: %w", err)
	}

	fields := strings.Fields(string(data))
	if len(fields) < 44 {
		return fmt.Errorf("thread stat file has insufficient fields: %d", len(fields))
	}

	// Field 3: State
	info.State = ProcessState(fields[2])
	
	// Field 18: Priority
	if priority, err := strconv.ParseInt(fields[17], 10, 32); err == nil {
		info.Priority = int32(priority)
	}
	
	// Field 19: Nice value
	if nice, err := strconv.ParseInt(fields[18], 10, 32); err == nil {
		info.Nice = int32(nice)
	}
	
	// Field 39: Last CPU
	if lastCPU, err := strconv.ParseInt(fields[38], 10, 32); err == nil {
		info.LastCPU = int32(lastCPU)
	}
	
	// Fields 14-15: CPU time information
	var utime, stime uint64
	if val, err := strconv.ParseUint(fields[13], 10, 64); err == nil {
		utime = val
	}
	if val, err := strconv.ParseUint(fields[14], 10, 64); err == nil {
		stime = val
	}
	
	// Calculate CPU usage information
	info.UserTicks = utime
	info.SystemTicks = stime
	
	// Calculate time durations
	info.UserTime = time.Duration(utime) * time.Second / ClockTicksPerSecond
	info.SystemTime = time.Duration(stime) * time.Second / ClockTicksPerSecond

	return nil
}

// parseThreadCommFile reads the thread name from the comm file
func (p *Parser) parseThreadCommFile(info *RawThreadInfo) error {
	commPath := filepath.Join(p.procPath, strconv.Itoa(int(info.PID)), "task", strconv.Itoa(int(info.TID)), "comm")
	
	data, err := os.ReadFile(commPath)
	if err != nil {
		return fmt.Errorf("failed to read thread comm file: %w", err)
	}

	info.Name = strings.TrimSpace(string(data))
	return nil
}

// parseMemoryValue parses memory values from status file (e.g., "1234 kB")
func parseMemoryValue(value string) (uint64, error) {
	parts := strings.Fields(value)
	if len(parts) < 1 {
		return 0, fmt.Errorf("invalid memory value format")
	}

	size, err := strconv.ParseUint(parts[0], 10, 64)
	if err != nil {
		return 0, err
	}

	// Convert from kB to bytes if unit is specified
	if len(parts) > 1 && strings.ToLower(parts[1]) == "kb" {
		size *= 1024
	}

	return size, nil
}

// getBootTime reads the system boot time from /proc/stat
func getBootTime() time.Time {
	data, err := os.ReadFile("/proc/stat")
	if err != nil {
		// Fallback to current time minus uptime
		return time.Now().Add(-getUptime())
	}

	scanner := bufio.NewScanner(strings.NewReader(string(data)))
	for scanner.Scan() {
		line := scanner.Text()
		if strings.HasPrefix(line, "btime ") {
			fields := strings.Fields(line)
			if len(fields) >= 2 {
				if bootTime, err := strconv.ParseInt(fields[1], 10, 64); err == nil {
					return time.Unix(bootTime, 0)
				}
			}
		}
	}

	// Fallback
	return time.Now().Add(-getUptime())
}

// getUptime reads the system uptime from /proc/uptime
func getUptime() time.Duration {
	data, err := os.ReadFile("/proc/uptime")
	if err != nil {
		return 0
	}

	fields := strings.Fields(string(data))
	if len(fields) < 1 {
		return 0
	}

	if uptime, err := strconv.ParseFloat(fields[0], 64); err == nil {
		return time.Duration(uptime * float64(time.Second))
	}

	return 0
}

