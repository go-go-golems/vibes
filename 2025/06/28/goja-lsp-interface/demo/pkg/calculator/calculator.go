// Package calculator provides mathematical operations and utilities.
// This package demonstrates various Go language features for LSP testing.
package calculator

import (
	"errors"
	"math"
)

// Operation represents a mathematical operation type.
type Operation int

// Constants for different mathematical operations.
const (
	Add Operation = iota
	Subtract
	Multiply
	Divide
	Power
	SquareRoot
)

// String returns the string representation of an operation.
func (op Operation) String() string {
	switch op {
	case Add:
		return "addition"
	case Subtract:
		return "subtraction"
	case Multiply:
		return "multiplication"
	case Divide:
		return "division"
	case Power:
		return "power"
	case SquareRoot:
		return "square root"
	default:
		return "unknown"
	}
}

// Calculator provides mathematical calculation capabilities.
// This struct demonstrates method receivers and state management.
type Calculator struct {
	history []CalculationResult
	precision int
}

// CalculationResult represents the result of a mathematical operation.
type CalculationResult struct {
	Operation Operation
	Operands  []float64
	Result    float64
	Error     error
}

// NewCalculator creates a new Calculator instance.
// This demonstrates constructor patterns with configuration.
func NewCalculator(precision int) *Calculator {
	return &Calculator{
		history:   make([]CalculationResult, 0),
		precision: precision,
	}
}

// Add performs addition of two numbers.
// This method demonstrates basic arithmetic operations.
func (c *Calculator) Add(a, b float64) float64 {
	result := a + b
	c.recordResult(Add, []float64{a, b}, result, nil)
	return result
}

// Subtract performs subtraction of two numbers.
func (c *Calculator) Subtract(a, b float64) float64 {
	result := a - b
	c.recordResult(Subtract, []float64{a, b}, result, nil)
	return result
}

// Multiply performs multiplication of two numbers.
func (c *Calculator) Multiply(a, b float64) float64 {
	result := a * b
	c.recordResult(Multiply, []float64{a, b}, result, nil)
	return result
}

// Divide performs division of two numbers.
// This method demonstrates error handling for division by zero.
func (c *Calculator) Divide(a, b float64) (float64, error) {
	if b == 0 {
		err := errors.New("division by zero")
		c.recordResult(Divide, []float64{a, b}, 0, err)
		return 0, err
	}
	result := a / b
	c.recordResult(Divide, []float64{a, b}, result, nil)
	return result, nil
}

// Power calculates a raised to the power of b.
// This method demonstrates use of the math package.
func (c *Calculator) Power(a, b float64) float64 {
	result := math.Pow(a, b)
	c.recordResult(Power, []float64{a, b}, result, nil)
	return result
}

// SquareRoot calculates the square root of a number.
// This method demonstrates error handling for negative inputs.
func (c *Calculator) SquareRoot(a float64) (float64, error) {
	if a < 0 {
		err := errors.New("cannot calculate square root of negative number")
		c.recordResult(SquareRoot, []float64{a}, 0, err)
		return 0, err
	}
	result := math.Sqrt(a)
	c.recordResult(SquareRoot, []float64{a}, result, nil)
	return result, nil
}

// GetHistory returns the calculation history.
// This method demonstrates slice operations and data access.
func (c *Calculator) GetHistory() []CalculationResult {
	// Return a copy to prevent external modification
	history := make([]CalculationResult, len(c.history))
	copy(history, c.history)
	return history
}

// ClearHistory clears the calculation history.
func (c *Calculator) ClearHistory() {
	c.history = c.history[:0]
}

// GetLastResult returns the last calculation result.
// This method demonstrates slice indexing and error handling.
func (c *Calculator) GetLastResult() (*CalculationResult, error) {
	if len(c.history) == 0 {
		return nil, errors.New("no calculations in history")
	}
	return &c.history[len(c.history)-1], nil
}

// recordResult is a private method to record calculation results.
// This demonstrates private methods and internal state management.
func (c *Calculator) recordResult(op Operation, operands []float64, result float64, err error) {
	c.history = append(c.history, CalculationResult{
		Operation: op,
		Operands:  operands,
		Result:    result,
		Error:     err,
	})
}

// PerformOperation performs a calculation based on the operation type.
// This method demonstrates switch statements and method dispatch.
func (c *Calculator) PerformOperation(op Operation, operands ...float64) (float64, error) {
	switch op {
	case Add:
		if len(operands) != 2 {
			return 0, errors.New("add operation requires exactly 2 operands")
		}
		return c.Add(operands[0], operands[1]), nil
	case Subtract:
		if len(operands) != 2 {
			return 0, errors.New("subtract operation requires exactly 2 operands")
		}
		return c.Subtract(operands[0], operands[1]), nil
	case Multiply:
		if len(operands) != 2 {
			return 0, errors.New("multiply operation requires exactly 2 operands")
		}
		return c.Multiply(operands[0], operands[1]), nil
	case Divide:
		if len(operands) != 2 {
			return 0, errors.New("divide operation requires exactly 2 operands")
		}
		return c.Divide(operands[0], operands[1])
	case Power:
		if len(operands) != 2 {
			return 0, errors.New("power operation requires exactly 2 operands")
		}
		return c.Power(operands[0], operands[1]), nil
	case SquareRoot:
		if len(operands) != 1 {
			return 0, errors.New("square root operation requires exactly 1 operand")
		}
		return c.SquareRoot(operands[0])
	default:
		return 0, errors.New("unsupported operation")
	}
}

