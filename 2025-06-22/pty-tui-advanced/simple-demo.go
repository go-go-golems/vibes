package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"

	"github.com/creack/pty"
	"github.com/hinshun/vt10x"
)

func main() {
	fmt.Println("PTY Demo - Creating a simple interactive shell...")
	
	// Create a command to run
	shell := os.Getenv("SHELL")
	if shell == "" {
		shell = "/bin/bash"
	}
	cmd := exec.Command(shell)
	
	// Start the command with a PTY
	size := &pty.Winsize{Cols: 80, Rows: 24}
	ptmx, err := pty.StartWithSize(cmd, size)
	if err != nil {
		log.Fatal(err)
	}
	defer ptmx.Close()
	
	// Create terminal emulator
	term := vt10x.New(vt10x.WithSize(80, 24))
	
	fmt.Println("PTY created successfully!")
	fmt.Printf("PTY file descriptor: %v\n", ptmx.Fd())
	fmt.Printf("Command PID: %d\n", cmd.Process.Pid)
	
	// Send a simple command
	ptmx.Write([]byte("echo 'Hello from PTY!'\n"))
	
	// Read some output
	buf := make([]byte, 1024)
	n, err := ptmx.Read(buf)
	if err != nil {
		log.Printf("Error reading: %v", err)
	} else {
		fmt.Printf("Raw output (%d bytes): %q\n", n, string(buf[:n]))
		
		// Process through terminal emulator
		term.Write(buf[:n])
		fmt.Printf("Terminal content:\n%s\n", term.String())
	}
	
	// Send exit command
	ptmx.Write([]byte("exit\n"))
	
	// Wait for command to finish
	cmd.Wait()
	
	fmt.Println("Demo completed!")
}

