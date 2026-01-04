package sample

import "fmt"

// SimpleFunction is a basic function for testing
func SimpleFunction(a int, b string) string {
	return b + UsedFunction(a)
}

// UsedFunction is called by SimpleFunction
func UsedFunction(num int) string {
	return fmt.Sprintf("%d", num)
}

// UnusedFunction is not called by any other function
func UnusedFunction() {
	fmt.Println("This function is not used")
}
