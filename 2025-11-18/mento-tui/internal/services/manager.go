package services

import (
	"bufio"
	"fmt"
	"io"
	"mento-tui/internal/config"
	"mento-tui/internal/models"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"time"

	"github.com/shirou/gopsutil/v3/process"
)

type Manager struct {
	Services      []*models.Service
	GlobalLog     *models.LogBuffer
	SelectedIndex int
	mu            sync.RWMutex
}

func NewManager(cfg *config.AppConfig) *Manager {
	services := make([]*models.Service, 0, len(cfg.Services))
	
	for _, svcCfg := range cfg.Services {
		// Determine working directory with fallback
		workingDir := cfg.GetServiceWorkingDirectory(&svcCfg)
		
		// Determine log buffer size with fallback
		logBufferSize := cfg.GetServiceLogBufferSize(&svcCfg)
		
		// Set Port for backward compatibility (use first port)
		port := 0
		if len(svcCfg.Ports) > 0 {
			port = svcCfg.Ports[0]
		}
		
		svc := &models.Service{
			Name:             svcCfg.Name,
			Port:             port, // Backward compatibility
			Ports:            svcCfg.Ports,
			Status:           models.StatusStopped,
			LogBuffer:        models.NewLogBuffer(logBufferSize),
			BinaryPath:       svcCfg.BinaryPath,
			WorkingDirectory: workingDir,
			Args:             svcCfg.ArgsList,
			EnvVars:          svcCfg.EnvVars,
		}
		
		services = append(services, svc)
	}
	
	return &Manager{
		Services:      services,
		GlobalLog:     models.NewLogBuffer(cfg.GetGlobalLogBufferSize()),
		SelectedIndex: 0,
	}
}

func (m *Manager) GetService(index int) *models.Service {
	m.mu.RLock()
	defer m.mu.RUnlock()
	if index >= 0 && index < len(m.Services) {
		return m.Services[index]
	}
	return nil
}

func (m *Manager) StartService(index int) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	if index < 0 || index >= len(m.Services) {
		return fmt.Errorf("invalid service index")
	}

	svc := m.Services[index]
	if svc.Status == models.StatusRunning {
		return fmt.Errorf("service already running")
	}

	svc.Status = models.StatusStarting
	svc.StartTime = time.Now()

	// Start the service with args
	cmd := exec.Command(svc.BinaryPath, svc.Args...)
	
	// Set working directory with fallback precedence: service -> global -> default
	if svc.WorkingDirectory != "" {
		cmd.Dir = svc.WorkingDirectory
	}
	
	cmd.Env = append(os.Environ(), svc.EnvVars...)

	// Create pipes for stdout and stderr
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		svc.Status = models.StatusFailed
		return err
	}

	stderr, err := cmd.StderrPipe()
	if err != nil {
		svc.Status = models.StatusFailed
		return err
	}

	if err := cmd.Start(); err != nil {
		svc.Status = models.StatusFailed
		return err
	}

	svc.Cmd = cmd
	svc.PID = cmd.Process.Pid

	// Read logs in background
	go m.readLogs(svc, stdout, stderr)

	// Monitor process
	go m.monitorProcess(svc)

	// Wait a bit then mark as running
	go func() {
		time.Sleep(3 * time.Second)
		m.mu.Lock()
		if svc.Status == models.StatusStarting {
			svc.Status = models.StatusRunning
		}
		m.mu.Unlock()
	}()

	return nil
}

func (m *Manager) StopService(index int) error {
	m.mu.Lock()
	defer m.mu.Unlock()

	if index < 0 || index >= len(m.Services) {
		return fmt.Errorf("invalid service index")
	}

	svc := m.Services[index]
	if svc.Status == models.StatusStopped {
		return fmt.Errorf("service already stopped")
	}

	if svc.Cmd != nil && svc.Cmd.Process != nil {
		// Send SIGTERM
		if err := svc.Cmd.Process.Signal(syscall.SIGTERM); err != nil {
			// Force kill if SIGTERM fails
			svc.Cmd.Process.Kill()
		}
		svc.Cmd.Wait()
	}

	svc.Status = models.StatusStopped
	svc.PID = 0
	svc.Cmd = nil

	return nil
}

func (m *Manager) RestartService(index int) error {
	if err := m.StopService(index); err != nil && err.Error() != "service already stopped" {
		return err
	}
	time.Sleep(1 * time.Second)
	return m.StartService(index)
}

func (m *Manager) StartAll() error {
	for i := range m.Services {
		if err := m.StartService(i); err != nil {
			return err
		}
		time.Sleep(500 * time.Millisecond)
	}
	return nil
}

func (m *Manager) StopAll() error {
	for i := range m.Services {
		m.StopService(i)
	}
	return nil
}

func (m *Manager) readLogs(svc *models.Service, stdout, stderr io.ReadCloser) {
	// Read stdout
	go func() {
		scanner := bufio.NewScanner(stdout)
		for scanner.Scan() {
			line := scanner.Text()
			svc.LogBuffer.Add(svc.Name, line)
			m.GlobalLog.Add(svc.Name, line)
		}
	}()

	// Read stderr
	go func() {
		scanner := bufio.NewScanner(stderr)
		for scanner.Scan() {
			line := scanner.Text()
			svc.LogBuffer.Add(svc.Name, line)
			m.GlobalLog.Add(svc.Name, line)
		}
	}()
}

func (m *Manager) monitorProcess(svc *models.Service) {
	ticker := time.NewTicker(2 * time.Second)
	defer ticker.Stop()

	for range ticker.C {
		m.mu.Lock()
		if svc.Status != models.StatusRunning || svc.PID == 0 {
			m.mu.Unlock()
			return
		}

		// Get process stats
		proc, err := process.NewProcess(int32(svc.PID))
		if err != nil {
			svc.Status = models.StatusFailed
			m.mu.Unlock()
			return
		}

		// CPU usage
		if cpu, err := proc.CPUPercent(); err == nil {
			svc.CPUPercent = cpu
		}

		// Memory usage
		if mem, err := proc.MemoryInfo(); err == nil {
			svc.MemoryMB = int(mem.RSS / 1024 / 1024)
		}

		m.mu.Unlock()
	}
}

func (m *Manager) GetUptime() time.Duration {
	m.mu.RLock()
	defer m.mu.RUnlock()

	var earliest time.Time
	for _, svc := range m.Services {
		if svc.Status == models.StatusRunning {
			if earliest.IsZero() || svc.StartTime.Before(earliest) {
				earliest = svc.StartTime
			}
		}
	}

	if earliest.IsZero() {
		return 0
	}
	return time.Since(earliest)
}
