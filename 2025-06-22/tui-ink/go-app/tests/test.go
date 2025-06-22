package main

import (
	"fmt"
	"io/ioutil"
	"log"

	"github.com/dop251/goja"
)

func main() {
	fmt.Println("Testing JavaScript bundle loading...")

	// Create goja runtime
	vm := goja.New()

	// Read the JavaScript bundle
	jsCode, err := ioutil.ReadFile("../js-modules/dist/simple-tui-bundle.js")
	if err != nil {
		log.Fatalf("Failed to read JS bundle: %v", err)
	}

	fmt.Println("JavaScript bundle loaded, size:", len(jsCode), "bytes")

	// Execute the JavaScript code
	_, err = vm.RunString(string(jsCode))
	if err != nil {
		log.Fatalf("Failed to execute JS code: %v", err)
	}

	fmt.Println("JavaScript code executed successfully!")

	// Test if SimpleTUILib is available
	simpleTUILib := vm.Get("SimpleTUILib")
	if simpleTUILib == nil {
		log.Fatal("SimpleTUILib not found in global scope")
	}

	fmt.Println("SimpleTUILib found in global scope")

	// Test creating a CounterApp instance
	counterAppConstructor := simpleTUILib.ToObject(vm).Get("CounterApp")
	if counterAppConstructor == nil {
		log.Fatal("CounterApp constructor not found")
	}

	fmt.Println("CounterApp constructor found")

	// Create new instance
	app, err := vm.New(counterAppConstructor)
	if err != nil {
		log.Fatalf("Failed to create CounterApp instance: %v", err)
	}

	fmt.Println("CounterApp instance created successfully!")

	// Test render method
	renderMethod := app.ToObject(vm).Get("render")
	if renderMethod == nil {
		log.Fatal("render method not found")
	}

	callable, ok := goja.AssertFunction(renderMethod)
	if !ok {
		log.Fatal("render is not a function")
	}

	result, err := callable(app)
	if err != nil {
		log.Fatalf("Error calling render: %v", err)
	}

	fmt.Println("Render method called successfully!")
	fmt.Println("Rendered output:")
	fmt.Println("================")
	fmt.Println(result.String())
	fmt.Println("================")

	fmt.Println("\nAll tests passed! The integration is working correctly.")
}

