package subpkg

import "fmt"

// Greet generates a greeting.
func Greet(name string) string {
	return fmt.Sprintf("Hello, %s!", name)
}

// internalHelper is not exported.
func internalHelper() {
	fmt.Println("Subpkg helper")
}
