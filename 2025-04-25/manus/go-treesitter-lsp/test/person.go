package sample

import "fmt"

// Person represents a simple struct
type Person struct {
	Name string
	Age  int
}

// NewPerson creates a new Person instance
func NewPerson(name string, age int) *Person {
	return &Person{
		Name: name,
		Age:  age,
	}
}

// Greet is a method on Person
func (p *Person) Greet() string {
	return fmt.Sprintf("Hello, my name is %s and I am %d years old", p.Name, p.Age)
}

// Birthday increments the person's age
func (p *Person) Birthday() {
	p.Age++
	fmt.Printf("%s is now %d years old\n", p.Name, p.Age)
}

// UsePersonMethods demonstrates using Person methods
func UsePersonMethods() {
	person := NewPerson("Alice", 30)
	fmt.Println(person.Greet())
	person.Birthday()
}
