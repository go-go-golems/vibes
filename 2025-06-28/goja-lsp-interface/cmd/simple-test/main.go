// Simple test to verify LSP communication works
package main

import (
	"fmt"
	"log"
	"os"
	"os/exec"
	"time"
)

func main() {
	fmt.Println("=== Simple LSP Communication Test ===")
	
	// Test if gopls is available and responds
	cmd := exec.Command("gopls", "version")
	output, err := cmd.Output()
	if err != nil {
		log.Fatalf("gopls not available: %v", err)
	}
	
	fmt.Printf("gopls version: %s\n", string(output))
	
	// Test basic gopls startup
	fmt.Println("Testing gopls startup...")
	
	ctx := exec.Command("gopls")
	ctx.Dir = "/home/ubuntu/goja-lsp-interface"
	
	stdin, err := ctx.StdinPipe()
	if err != nil {
		log.Fatalf("Failed to create stdin pipe: %v", err)
	}
	
	stdout, err := ctx.StdoutPipe()
	if err != nil {
		log.Fatalf("Failed to create stdout pipe: %v", err)
	}
	
	if err := ctx.Start(); err != nil {
		log.Fatalf("Failed to start gopls: %v", err)
	}
	
	fmt.Printf("gopls started with PID: %d\n", ctx.Process.Pid)
	
	// Send a simple initialize request
	initRequest := `Content-Length: 500

{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"processId":` + fmt.Sprintf("%d", os.Getpid()) + `,"rootUri":"file:///home/ubuntu/goja-lsp-interface","capabilities":{}}}
`
	
	fmt.Println("Sending initialize request...")
	_, err = stdin.Write([]byte(initRequest))
	if err != nil {
		log.Fatalf("Failed to send request: %v", err)
	}
	
	// Read response with timeout
	done := make(chan bool)
	go func() {
		buffer := make([]byte, 4096)
		n, err := stdout.Read(buffer)
		if err != nil {
			fmt.Printf("Error reading response: %v\n", err)
		} else {
			fmt.Printf("Received response (%d bytes):\n%s\n", n, string(buffer[:n]))
		}
		done <- true
	}()
	
	select {
	case <-done:
		fmt.Println("Communication successful!")
	case <-time.After(5 * time.Second):
		fmt.Println("Timeout waiting for response")
	}
	
	// Clean up
	stdin.Close()
	stdout.Close()
	ctx.Process.Kill()
	ctx.Wait()
	
	fmt.Println("Test completed.")
}

