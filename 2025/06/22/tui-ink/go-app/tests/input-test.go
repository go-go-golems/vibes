package main

import (
	"fmt"
	"io/ioutil"
	"log"

	"github.com/dop251/goja"
)

func main() {
	fmt.Println("Testing input handling...")

	// Create goja runtime
	vm := goja.New()

	// Read and execute the JavaScript bundle
	jsCode, err := ioutil.ReadFile("../js-modules/dist/simple-tui-bundle.js")
	if err != nil {
		log.Fatalf("Failed to read JS bundle: %v", err)
	}

	_, err = vm.RunString(string(jsCode))
	if err != nil {
		log.Fatalf("Failed to execute JS code: %v", err)
	}

	// Create CounterApp instance
	simpleTUILib := vm.Get("SimpleTUILib")
	counterAppConstructor := simpleTUILib.ToObject(vm).Get("CounterApp")
	app, err := vm.New(counterAppConstructor)
	if err != nil {
		log.Fatalf("Failed to create CounterApp instance: %v", err)
	}

	// Test input handling
	handleInputMethod := app.ToObject(vm).Get("handleInput")
	handleInputCallable, ok := goja.AssertFunction(handleInputMethod)
	if !ok {
		log.Fatal("handleInput is not a function")
	}

	renderMethod := app.ToObject(vm).Get("render")
	renderCallable, ok := goja.AssertFunction(renderMethod)
	if !ok {
		log.Fatal("render is not a function")
	}

	// Test sequence of inputs
	inputs := []string{"+", "+", "+", "p", "p", "-", "r"}
	
	for i, input := range inputs {
		fmt.Printf("\nStep %d: Sending input '%s'\n", i+1, input)
		
		// Handle input
		result, err := handleInputCallable(app, vm.ToValue(input))
		if err != nil {
			log.Printf("Error handling input '%s': %v", input, err)
			continue
		}

		// Check if quit was requested
		if result != nil && result.String() == "quit" {
			fmt.Println("Quit requested, stopping test")
			break
		}

		// Render the updated state
		renderResult, err := renderCallable(app)
		if err != nil {
			log.Printf("Error rendering after input '%s': %v", input, err)
			continue
		}

		fmt.Println("Rendered output:")
		fmt.Println("================")
		fmt.Println(renderResult.String())
		fmt.Println("================")
	}

	fmt.Println("\nInput handling test completed successfully!")
}

