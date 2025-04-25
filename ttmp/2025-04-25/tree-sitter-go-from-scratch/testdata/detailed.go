package detailed

import (
	"fmt"
	"io"
	"strings"
)

// Point represents a 2D point.
// It has X and Y coordinates.
type Point struct {
	X int
	Y int
}

// String returns a string representation of the Point.
// This is a method on the Point struct.
func (p Point) String() string {
	return fmt.Sprintf("(%d, %d)", p.X, p.Y)
}

// Move translates the point by dx and dy.
// This is a method on a *pointer* to Point.
// It modifies the receiver.
func (p *Point) Move(dx int, dy int) {
	p.X += dx
	p.Y += dy
}

// Add combines two points.
// This is a regular function.
func Add(p1 Point, p2 Point) Point {
	return Point{X: p1.X + p2.X, Y: p1.Y + p2.Y}
}

// ProcessData reads from a reader and returns bytes read and an error.
// It demonstrates multiple return values.
func ProcessData(r io.Reader) (int, error) {
	data := make([]byte, 1024)
	n, err := r.Read(data)
	if err != nil {
		return n, fmt.Errorf("read failed: %w", err)
	}
	// Some processing would happen here...
	fmt.Printf("Processed %d bytes\n", n)
	return n, nil
}

// internalHelper is not exported.
func internalHelper(msg string) {
	fmt.Println("Internal:", msg)
}

// VariadicFunction takes a variable number of strings.
func VariadicFunction(prefix string, values ...string) string {
	return prefix + strings.Join(values, ", ")
}
