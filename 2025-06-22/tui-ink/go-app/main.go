package main

import (
	"bufio"
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"os/exec"
	"time"
	"syscall"
	"unsafe"

	"github.com/dop251/goja"
)

type TUIApp struct {
	vm       *goja.Runtime
	app      goja.Value
	running  bool
	scanner  *bufio.Scanner
}

// Terminal control structures
type termios struct {
	Iflag  uint32
	Oflag  uint32
	Cflag  uint32
	Lflag  uint32
	Cc     [20]uint8
	Ispeed uint32
	Ospeed uint32
}

const (
	TCGETS = 0x5401
	TCSETS = 0x5402
	ICANON = 0x2
	ECHO   = 0x8
)

func enableRawMode() (*termios, error) {
	var oldTermios termios
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(0), TCGETS, uintptr(unsafe.Pointer(&oldTermios)))
	if errno != 0 {
		return nil, errno
	}

	newTermios := oldTermios
	newTermios.Lflag &^= ICANON | ECHO

	_, _, errno = syscall.Syscall(syscall.SYS_IOCTL, uintptr(0), TCSETS, uintptr(unsafe.Pointer(&newTermios)))
	if errno != 0 {
		return nil, errno
	}

	return &oldTermios, nil
}

func restoreTerminal(oldTermios *termios) error {
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(0), TCSETS, uintptr(unsafe.Pointer(oldTermios)))
	if errno != 0 {
		return errno
	}
	return nil
}

func readChar() (byte, error) {
	var buf [1]byte
	n, err := os.Stdin.Read(buf[:])
	if err != nil {
		return 0, err
	}
	if n == 0 {
		return 0, fmt.Errorf("no input")
	}
	return buf[0], nil
}

func NewTUIApp() *TUIApp {
	return &TUIApp{
		vm:      goja.New(),
		running: true,
		scanner: bufio.NewScanner(os.Stdin),
	}
}

func (t *TUIApp) LoadJSBundle(filename string) error {
	// Read the JavaScript bundle
	jsCode, err := ioutil.ReadFile(filename)
	if err != nil {
		return fmt.Errorf("failed to read JS bundle: %v", err)
	}

	// Execute the JavaScript code in goja
	_, err = t.vm.RunString(string(jsCode))
	if err != nil {
		return fmt.Errorf("failed to execute JS code: %v", err)
	}

	// Get the SimpleTUILib from the global scope
	simpleTUILib := t.vm.Get("SimpleTUILib")
	if simpleTUILib == nil {
		return fmt.Errorf("SimpleTUILib not found in JS bundle")
	}

	// Create a new CounterApp instance
	counterAppConstructor := simpleTUILib.ToObject(t.vm).Get("CounterApp")
	if counterAppConstructor == nil {
		return fmt.Errorf("CounterApp constructor not found")
	}

	// Create new instance
	app, err := t.vm.New(counterAppConstructor)
	if err != nil {
		return fmt.Errorf("failed to create CounterApp instance: %v", err)
	}

	t.app = app
	return nil
}

func (t *TUIApp) clearScreen() {
	cmd := exec.Command("clear")
	cmd.Stdout = os.Stdout
	cmd.Run()
}

func (t *TUIApp) render() {
	if t.app == nil {
		return
	}

	// Call the render method on the app
	renderMethod := t.app.ToObject(t.vm).Get("render")
	if renderMethod == nil {
		log.Println("render method not found")
		return
	}

	callable, ok := goja.AssertFunction(renderMethod)
	if !ok {
		log.Println("render is not a function")
		return
	}

	result, err := callable(t.app)
	if err != nil {
		log.Printf("Error calling render: %v", err)
		return
	}

	// Clear screen and print the rendered output
	t.clearScreen()
	fmt.Print(result.String())
	fmt.Print("\n\nPress a key: ")
}

func (t *TUIApp) handleInput(input string) {
	if t.app == nil {
		return
	}

	// Call the handleInput method on the app
	handleInputMethod := t.app.ToObject(t.vm).Get("handleInput")
	if handleInputMethod == nil {
		log.Println("handleInput method not found")
		return
	}

	callable, ok := goja.AssertFunction(handleInputMethod)
	if !ok {
		log.Println("handleInput is not a function")
		return
	}

	result, err := callable(t.app, t.vm.ToValue(input))
	if err != nil {
		log.Printf("Error calling handleInput: %v", err)
		return
	}

	// Check if the result indicates we should quit
	if result != nil && result.String() == "quit" {
		t.running = false
	}
}

func (t *TUIApp) Run() {
	fmt.Println("Starting TUI App with Goja...")
	fmt.Println("Loading JavaScript bundle...")

	// Load the JavaScript bundle
	err := t.LoadJSBundle("../js-modules/dist/simple-tui-bundle.js")
	if err != nil {
		log.Fatalf("Failed to load JS bundle: %v", err)
	}

	fmt.Println("JavaScript bundle loaded successfully!")
	time.Sleep(1 * time.Second)

	// Check if we're in a terminal
	isTerminal := isatty()
	
	if isTerminal {
		// Use raw mode for interactive terminal
		t.runInteractive()
	} else {
		// Use line-based input for non-terminal (testing)
		t.runLineMode()
	}
}

func isatty() bool {
	var termios termios
	_, _, errno := syscall.Syscall(syscall.SYS_IOCTL, uintptr(0), TCGETS, uintptr(unsafe.Pointer(&termios)))
	return errno == 0
}

func (t *TUIApp) runInteractive() {
	// Enable raw mode for character input
	oldTermios, err := enableRawMode()
	if err != nil {
		log.Fatalf("Failed to enable raw mode: %v", err)
	}
	defer func() {
		restoreTerminal(oldTermios)
		fmt.Println("\nGoodbye!")
	}()

	// Initial render
	t.render()

	// Main input loop with character-based input
	for t.running {
		char, err := readChar()
		if err != nil {
			log.Printf("Error reading input: %v", err)
			continue
		}

		// Convert byte to string
		input := string(char)
		
		// Handle special characters
		if char == 3 { // Ctrl+C
			t.running = false
			break
		}
		
		if char == 27 { // ESC
			t.running = false
			break
		}

		// Handle regular input
		if input == "q" {
			t.running = false
			break
		}

		// Process the input
		t.handleInput(input)
		t.render()
	}
}

func (t *TUIApp) runLineMode() {
	// Use line-based input for testing
	scanner := bufio.NewScanner(os.Stdin)
	
	// Initial render
	t.render()

	// Main input loop with line-based input
	for t.running {
		if scanner.Scan() {
			line := scanner.Text()
			
			// Process each character in the line
			for _, char := range line {
				input := string(char)
				
				if input == "q" {
					t.running = false
					fmt.Println("\nGoodbye!")
					return
				}

				// Process the input
				t.handleInput(input)
				t.render()
			}
		} else {
			break
		}
	}
	
	fmt.Println("\nGoodbye!")
}

func main() {
	app := NewTUIApp()
	app.Run()
}

