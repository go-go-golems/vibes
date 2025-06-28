// Package main demonstrates the usage of various Go language features
// for testing LSP functionality including hover, completion, definition, and references.
package main

import (
	"fmt"
	"log"
	"time"

	"goja-lsp-interface/demo/pkg/calculator"
	"goja-lsp-interface/demo/pkg/models"
)

// Config holds application configuration.
// This struct demonstrates configuration patterns.
type Config struct {
	AppName     string
	Version     string
	Debug       bool
	MaxUsers    int
	Precision   int
}

// DefaultConfig returns a default configuration.
func DefaultConfig() *Config {
	return &Config{
		AppName:   "Demo Application",
		Version:   "1.0.0",
		Debug:     true,
		MaxUsers:  1000,
		Precision: 2,
	}
}

// Application represents the main application structure.
// This demonstrates composition and dependency injection patterns.
type Application struct {
	config     *Config
	calculator *calculator.Calculator
	users      []models.User
}

// NewApplication creates a new Application instance.
func NewApplication(config *Config) *Application {
	return &Application{
		config:     config,
		calculator: calculator.NewCalculator(config.Precision),
		users:      make([]models.User, 0),
	}
}

// Run starts the application and demonstrates various operations.
func (app *Application) Run() error {
	fmt.Printf("Starting %s v%s\n", app.config.AppName, app.config.Version)
	
	// Demonstrate user operations
	if err := app.demonstrateUserOperations(); err != nil {
		return fmt.Errorf("user operations failed: %w", err)
	}
	
	// Demonstrate calculator operations
	if err := app.demonstrateCalculatorOperations(); err != nil {
		return fmt.Errorf("calculator operations failed: %w", err)
	}
	
	// Demonstrate advanced features
	app.demonstrateAdvancedFeatures()
	
	return nil
}

// demonstrateUserOperations shows various user-related operations.
func (app *Application) demonstrateUserOperations() error {
	fmt.Println("\n=== User Operations Demo ===")
	
	// Create some users
	users := []*models.User{
		models.NewUser("Alice Johnson", "alice@example.com", 25),
		models.NewUser("Bob Smith", "bob@example.com", 17),
		models.NewUser("Charlie Brown", "charlie@example.com", 30),
	}
	
	// Demonstrate user methods
	for i, user := range users {
		user.ID = i + 1
		
		// Validate user
		if err := user.Validate(); err != nil {
			return fmt.Errorf("user validation failed: %w", err)
		}
		
		// Add to application users
		app.users = append(app.users, *user)
		
		// Demonstrate user methods
		fmt.Printf("User: %s\n", user.String())
		fmt.Printf("Display Name: %s\n", user.GetDisplayName())
		fmt.Printf("Is Adult: %t\n", user.IsAdult())
		fmt.Println()
	}
	
	return nil
}

// demonstrateCalculatorOperations shows various calculator operations.
func (app *Application) demonstrateCalculatorOperations() error {
	fmt.Println("=== Calculator Operations Demo ===")
	
	calc := app.calculator
	
	// Basic arithmetic operations
	fmt.Printf("Addition: 10 + 5 = %.2f\n", calc.Add(10, 5))
	fmt.Printf("Subtraction: 10 - 5 = %.2f\n", calc.Subtract(10, 5))
	fmt.Printf("Multiplication: 10 * 5 = %.2f\n", calc.Multiply(10, 5))
	
	// Division with error handling
	if result, err := calc.Divide(10, 5); err != nil {
		fmt.Printf("Division error: %v\n", err)
	} else {
		fmt.Printf("Division: 10 / 5 = %.2f\n", result)
	}
	
	// Division by zero error handling
	if result, err := calc.Divide(10, 0); err != nil {
		fmt.Printf("Division by zero error: %v\n", err)
	} else {
		fmt.Printf("Division: 10 / 0 = %.2f\n", result)
	}
	
	// Power operation
	fmt.Printf("Power: 2^3 = %.2f\n", calc.Power(2, 3))
	
	// Square root operations
	if result, err := calc.SquareRoot(16); err != nil {
		fmt.Printf("Square root error: %v\n", err)
	} else {
		fmt.Printf("Square root: √16 = %.2f\n", result)
	}
	
	// Square root of negative number
	if result, err := calc.SquareRoot(-16); err != nil {
		fmt.Printf("Square root of negative error: %v\n", err)
	} else {
		fmt.Printf("Square root: √-16 = %.2f\n", result)
	}
	
	// Demonstrate operation dispatch
	operations := []struct {
		op       calculator.Operation
		operands []float64
		name     string
	}{
		{calculator.Add, []float64{7, 3}, "Add 7 + 3"},
		{calculator.Multiply, []float64{4, 6}, "Multiply 4 * 6"},
		{calculator.SquareRoot, []float64{25}, "Square root of 25"},
	}
	
	fmt.Println("\n--- Operation Dispatch Demo ---")
	for _, op := range operations {
		if result, err := calc.PerformOperation(op.op, op.operands...); err != nil {
			fmt.Printf("%s: Error - %v\n", op.name, err)
		} else {
			fmt.Printf("%s: %.2f\n", op.name, result)
		}
	}
	
	return nil
}

// demonstrateAdvancedFeatures shows advanced Go language features.
func (app *Application) demonstrateAdvancedFeatures() {
	fmt.Println("\n=== Advanced Features Demo ===")
	
	// Demonstrate history functionality
	history := app.calculator.GetHistory()
	fmt.Printf("Calculation history contains %d entries\n", len(history))
	
	if lastResult, err := app.calculator.GetLastResult(); err == nil {
		fmt.Printf("Last calculation: %s with result %.2f\n", 
			lastResult.Operation.String(), lastResult.Result)
	}
	
	// Demonstrate goroutines and channels (advanced concurrency)
	app.demonstrateConcurrency()
	
	// Demonstrate interfaces
	app.demonstrateInterfaces()
}

// demonstrateConcurrency shows basic concurrency patterns.
func (app *Application) demonstrateConcurrency() {
	fmt.Println("\n--- Concurrency Demo ---")
	
	// Channel for communication
	results := make(chan float64, 3)
	
	// Start goroutines for parallel calculations
	go func() {
		results <- app.calculator.Add(1, 2)
	}()
	
	go func() {
		results <- app.calculator.Multiply(3, 4)
	}()
	
	go func() {
		results <- app.calculator.Power(2, 4)
	}()
	
	// Collect results
	fmt.Println("Concurrent calculations:")
	for i := 0; i < 3; i++ {
		result := <-results
		fmt.Printf("Result %d: %.2f\n", i+1, result)
	}
	close(results)
}

// demonstrateInterfaces shows interface usage patterns.
func (app *Application) demonstrateInterfaces() {
	fmt.Println("\n--- Interface Demo ---")
	
	// Demonstrate fmt.Stringer interface
	user := models.NewUser("Demo User", "demo@example.com", 25)
	fmt.Printf("Stringer interface: %s\n", user)
	
	// Demonstrate custom interface usage
	var repo models.UserRepository
	if repo == nil {
		fmt.Println("UserRepository interface is nil (not implemented)")
	}
}

// processUsers demonstrates slice operations and iteration patterns.
func (app *Application) processUsers(filter func(*models.User) bool) []*models.User {
	var filtered []*models.User
	
	for i := range app.users {
		user := &app.users[i]
		if filter(user) {
			filtered = append(filtered, user)
		}
	}
	
	return filtered
}

// main is the entry point of the application.
func main() {
	// Initialize configuration
	config := DefaultConfig()
	
	// Create and run application
	app := NewApplication(config)
	
	// Measure execution time
	start := time.Now()
	
	if err := app.Run(); err != nil {
		log.Fatalf("Application failed: %v", err)
	}
	
	duration := time.Since(start)
	fmt.Printf("\nApplication completed in %v\n", duration)
	
	// Demonstrate user filtering
	fmt.Println("\n=== User Filtering Demo ===")
	adults := app.processUsers(func(u *models.User) bool {
		return u.IsAdult()
	})
	
	fmt.Printf("Found %d adult users:\n", len(adults))
	for _, user := range adults {
		fmt.Printf("- %s (age %d)\n", user.Name, user.Age)
	}
}

