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
	fmt.Println("Interactive PTY Demo")
	fmt.Println("===================")
	
	// Create a command to run
	cmd := exec.Command("/bin/bash")
	
	// Start the command with a PTY
	size := &pty.Winsize{Cols: 80, Rows: 24}
	ptmx, err := pty.StartWithSize(cmd, size)
	if err != nil {
		log.Fatal(err)
	}
	defer ptmx.Close()
	
	// Create terminal emulator
	term := vt10x.New(vt10x.WithSize(80, 24))
	
	fmt.Printf("PTY created with PID: %d\n", cmd.Process.Pid)
	fmt.Println("Running demo commands...")
	
	// Demo sequence
	commands := []string{
		"echo 'Welcome to PTY Demo!'",
		"pwd",
		"ls -la",
		"echo 'Current date:'",
		"date",
		"echo 'System info:'",
		"uname -a",
		"echo 'Demo completed!'",
		"exit",
	}
	
	for i, command := range commands {
		fmt.Printf("\n[Step %d] Executing: %s\n", i+1, command)
		
		// Send command
		ptmx.Write([]byte(command + "\n"))
		
		// Wait a bit for output
		time.Sleep(500 * time.Millisecond)
		
		// Read output
		buf := make([]byte, 4096)
		ptmx.SetReadDeadline(time.Now().Add(1 * time.Second))
		n, err := ptmx.Read(buf)
		if err != nil {
			fmt.Printf("Read error: %v\n", err)
			continue
		}
		
		// Process through terminal emulator
		term.Write(buf[:n])
		
		// Show current terminal state
		fmt.Printf("Terminal output:\n%s\n", term.String())
		fmt.Println("---")
	}
	
	// Wait for command to finish
	cmd.Wait()
	
	fmt.Println("\nDemo completed successfully!")
}

