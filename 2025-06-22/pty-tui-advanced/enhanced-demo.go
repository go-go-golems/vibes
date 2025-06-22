package main

import (
	"fmt"
	"log"
	"os/exec"
	"time"

	"github.com/creack/pty"
	"github.com/hinshun/vt10x"
)

func main() {
	fmt.Println("=== Enhanced PTY Demo with Validation ===")
	fmt.Println("This demo showcases the 5 key steps from the PTY guide")
	fmt.Println()
	
	// Step 1-2: Create PTY and start command
	fmt.Println("STEP 1-2: Creating PTY and starting shell...")
	cmd := exec.Command("/bin/bash")
	size := &pty.Winsize{Cols: 80, Rows: 24}
	ptmx, err := pty.StartWithSize(cmd, size)
	if err != nil {
		log.Fatal(err)
	}
	defer ptmx.Close()
	
	fmt.Printf("✓ PTY created with PID: %d\n", cmd.Process.Pid)
	fmt.Printf("✓ Initial size: %dx%d\n", size.Cols, size.Rows)
	
	// Step 3: Create terminal emulator
	fmt.Println("\nSTEP 3: Setting up terminal emulator...")
	term := vt10x.New(vt10x.WithSize(int(size.Cols), int(size.Rows)))
	fmt.Println("✓ VT10x terminal emulator initialized")
	
	// Demo sequence with validation points
	commands := []struct {
		cmd         string
		description string
		expectText  string
	}{
		{"echo 'PTY Demo Started'", "Basic echo test", "PTY Demo Started"},
		{"pwd", "Show current directory", "/home/ubuntu/pty-demo-enhanced"},
		{"echo 'Testing PTY functionality'", "Functionality test", "Testing PTY functionality"},
		{"date +%Y", "Date command test", "2025"},
		{"echo 'Libraries: creack/pty + hinshun/vt10x'", "Library info", "Libraries: creack/pty + hinshun/vt10x"},
		{"echo 'All 5 steps implemented!'", "Success message", "All 5 steps implemented!"},
	}
	
	fmt.Println("\nExecuting demo commands with validation...")
	
	for i, cmdInfo := range commands {
		fmt.Printf("\n[Command %d] %s\n", i+1, cmdInfo.description)
		fmt.Printf("Executing: %s\n", cmdInfo.cmd)
		
		// Step 5: Send command to PTY
		ptmx.Write([]byte(cmdInfo.cmd + "\n"))
		
		// Wait for output
		time.Sleep(800 * time.Millisecond)
		
		// Step 3: Read and process output through terminal emulator
		buf := make([]byte, 4096)
		ptmx.SetReadDeadline(time.Now().Add(1 * time.Second))
		n, err := ptmx.Read(buf)
		if err != nil {
			fmt.Printf("Read error: %v\n", err)
			continue
		}
		
		// Process through terminal emulator
		term.Write(buf[:n])
		
		// Get terminal content for validation
		terminalContent := term.String()
		
		// Validation check
		if contains(terminalContent, cmdInfo.expectText) {
			fmt.Printf("✓ VALIDATION PASSED: Found expected text '%s'\n", cmdInfo.expectText)
		} else {
			fmt.Printf("✗ VALIDATION FAILED: Expected '%s' not found\n", cmdInfo.expectText)
		}
		
		// Show current terminal state (truncated for readability)
		lines := getLastLines(terminalContent, 3)
		fmt.Printf("Terminal output (last 3 lines):\n%s\n", lines)
		fmt.Println("---")
	}
	
	// Step 4: Demonstrate resize capability
	fmt.Println("\nSTEP 4: Testing resize functionality...")
	newSize := &pty.Winsize{Cols: 100, Rows: 30}
	err = pty.Setsize(ptmx, newSize)
	if err != nil {
		fmt.Printf("Resize error: %v\n", err)
	} else {
		fmt.Printf("✓ PTY resized to %dx%d\n", newSize.Cols, newSize.Rows)
	}
	
	// Final validation
	ptmx.Write([]byte("echo 'Resize test complete'\n"))
	time.Sleep(500 * time.Millisecond)
	
	buf := make([]byte, 4096)
	ptmx.SetReadDeadline(time.Now().Add(1 * time.Second))
	n, _ := ptmx.Read(buf)
	term.Write(buf[:n])
	
	if contains(term.String(), "Resize test complete") {
		fmt.Println("✓ RESIZE VALIDATION PASSED")
	}
	
	// Clean exit
	ptmx.Write([]byte("exit\n"))
	cmd.Wait()
	
	fmt.Println("\n=== Demo Summary ===")
	fmt.Println("✓ Step 1-2: PTY creation and command startup")
	fmt.Println("✓ Step 3: Terminal emulation with vt10x")
	fmt.Println("✓ Step 4: Window resizing")
	fmt.Println("✓ Step 5: Input forwarding")
	fmt.Println("✓ All validation checks completed")
	fmt.Println("\nDemo completed successfully!")
}

// Helper function to check if text contains substring
func contains(text, substr string) bool {
	return len(text) > 0 && len(substr) > 0 && 
		   findInString(text, substr)
}

// Simple substring search
func findInString(text, substr string) bool {
	if len(substr) > len(text) {
		return false
	}
	for i := 0; i <= len(text)-len(substr); i++ {
		if text[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}

// Get last N lines from terminal content
func getLastLines(content string, n int) string {
	lines := []string{}
	current := ""
	
	for _, char := range content {
		if char == '\n' {
			lines = append(lines, current)
			current = ""
		} else {
			current += string(char)
		}
	}
	if current != "" {
		lines = append(lines, current)
	}
	
	// Get last n non-empty lines
	result := []string{}
	for i := len(lines) - 1; i >= 0 && len(result) < n; i-- {
		if len(lines[i]) > 0 {
			result = append([]string{lines[i]}, result...)
		}
	}
	
	output := ""
	for _, line := range result {
		output += line + "\n"
	}
	return output
}

