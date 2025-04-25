package simple

import "fmt"

// Add adds two numbers.
func Add(a, b int) int {
	return a + b
}

func helper() {
	fmt.Println("Internal helper")
}

// Subtract subtracts b from a.
func Subtract(a, b int) int {
	c := a - b
	if c < 0 {
		helper()
	}
	return c
}
