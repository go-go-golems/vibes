package main

import (
	"fmt"
	"os"
	"sample/calculator"
)

// main is the entry point of the program
func main() {
	// Call functions from this file
	message := getGreeting("World")
	printMessage(message)
	
	// Call functions from another package
	result := calculator.Add(5, 7)
	fmt.Printf("5 + 7 = %d\n", result)
	
	diff := calculator.Subtract(10, 3)
	fmt.Printf("10 - 3 = %d\n", diff)
}

// getGreeting generates a greeting message
func getGreeting(name string) string {
	return fmt.Sprintf("Hello, %s!", name)
}

// printMessage prints a message to stdout
func printMessage(message string) {
	fmt.Println(message)
	
	// Call another function
	exitIfEmpty(message)
}

// exitIfEmpty exits the program if the message is empty
func exitIfEmpty(message string) {
	if message == "" {
		fmt.Println("Error: Empty message")
		os.Exit(1)
	}
}