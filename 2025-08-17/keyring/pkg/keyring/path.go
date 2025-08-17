package keyring

import (
	"strings"
)

// Path represents a hierarchical path with segments
type Path struct {
	Segments []string
}

// P creates a new Path from a string representation
func P(s string) Path {
	s = strings.Trim(s, "/")
	if s == "" {
		return Path{}
	}
	return Path{Segments: strings.Split(s, "/")}
}

// String returns the string representation of the path
func (p Path) String() string {
	return strings.Join(p.Segments, "/")
}

// IsZero returns true if the path is empty
func (p Path) IsZero() bool {
	return len(p.Segments) == 0
}

// Parent returns the parent path
func (p Path) Parent() Path {
	if len(p.Segments) <= 1 {
		return Path{}
	}
	return Path{Segments: p.Segments[:len(p.Segments)-1]}
}

// Child returns a new path with the given segment appended
func (p Path) Child(segment string) Path {
	segments := make([]string, len(p.Segments)+1)
	copy(segments, p.Segments)
	segments[len(p.Segments)] = segment
	return Path{Segments: segments}
}

// Last returns the last segment of the path
func (p Path) Last() string {
	if len(p.Segments) == 0 {
		return ""
	}
	return p.Segments[len(p.Segments)-1]
}

