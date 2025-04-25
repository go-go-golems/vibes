package main

import (
	"fmt"
	"strings"
)

// Person represents a person with name and age information
type Person struct {
	Name    string
	Age     int
	Address *Address
}

// Address represents a physical address
type Address struct {
	Street  string
	City    string
	State   string
	ZipCode string
}

// NewPerson creates a new Person instance
func NewPerson(name string, age int) *Person {
	return &Person{
		Name: name,
		Age:  age,
	}
}

// SetAddress sets the address for a person
func (p *Person) SetAddress(street, city, state, zip string) {
	p.Address = &Address{
		Street:  street,
		City:    city,
		State:   state,
		ZipCode: zip,
	}
}

// GetFullAddress returns the person's full address as a formatted string
func (p *Person) GetFullAddress() string {
	if p.Address == nil {
		return "No address"
	}
	
	return fmt.Sprintf("%s\n%s, %s %s",
		p.Address.Street,
		p.Address.City,
		p.Address.State,
		p.Address.ZipCode,
	)
}

// IsAdult checks if the person is an adult (age >= 18)
func (p Person) IsAdult() bool {
	return p.Age >= 18
}

// GetFormattedName returns the name in the format "LastName, FirstName"
// If the name doesn't contain a space, it returns the original name
func (p Person) GetFormattedName() string {
	parts := strings.Split(p.Name, " ")
	if len(parts) < 2 {
		return p.Name
	}
	
	firstName := parts[0]
	lastName := strings.Join(parts[1:], " ")
	
	return fmt.Sprintf("%s, %s", lastName, firstName)
}

// FormatAddress formats an address according to the provided format string
// The format string can contain placeholders: %s for Street, %c for City,
// %t for State, and %z for ZipCode
func FormatAddress(addr *Address, format string) string {
	if addr == nil {
		return "No address"
	}
	
	result := format
	result = strings.ReplaceAll(result, "%s", addr.Street)
	result = strings.ReplaceAll(result, "%c", addr.City)
	result = strings.ReplaceAll(result, "%t", addr.State)
	result = strings.ReplaceAll(result, "%z", addr.ZipCode)
	
	return result
}

// BuildAddress creates a new Address with the provided components
// This function demonstrates multiple return values
func BuildAddress(street, city, state, zip string) (*Address, error) {
	// Validate zip code (simple check for demonstration)
	if len(zip) != 5 && len(zip) != 9 {
		return nil, fmt.Errorf("invalid zip code format: %s", zip)
	}
	
	return &Address{
		Street:  street,
		City:    city,
		State:   state,
		ZipCode: zip,
	}, nil
}