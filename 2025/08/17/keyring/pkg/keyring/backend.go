package keyring

import "context"

// Backend is the interface for keyring storage backends
type Backend interface {
	Name() string
	Get(ctx context.Context, profile string, path Path) (Secret, error)
	Put(ctx context.Context, profile string, path Path, s Secret) error
	Delete(ctx context.Context, profile string, path Path) error
	// List returns immediate children of prefix. Leaf nodes should appear as children with no further nesting.
	List(ctx context.Context, profile string, prefix Path) ([]Path, error)
}

// ErrNotFound signals "key absent" (so the ring can try the next backend)
// ErrReadOnly signals "this backend won't accept writes"
var (
	ErrNotFound = errNotFound{}
	ErrReadOnly = errReadOnly{}
)

type errNotFound struct{}

func (errNotFound) Error() string { return "key not found" }

type errReadOnly struct{}

func (errReadOnly) Error() string { return "backend is read-only" }

// IsNotFound returns true if the error is ErrNotFound
func IsNotFound(err error) bool {
	_, ok := err.(errNotFound)
	return ok
}

// IsReadOnly returns true if the error is ErrReadOnly
func IsReadOnly(err error) bool {
	_, ok := err.(errReadOnly)
	return ok
}

