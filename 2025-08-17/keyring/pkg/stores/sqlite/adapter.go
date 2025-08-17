package sqlite

import "keyring/pkg/keyring"

// StateStoreAdapter adapts the Store to implement the StateStore interface
type StateStoreAdapter struct {
	store *Store
}

// NewStateStoreAdapter creates a new StateStore adapter
func NewStateStoreAdapter(store *Store) keyring.StateStore {
	return &StateStoreAdapter{store: store}
}

// Get retrieves key state
func (a *StateStoreAdapter) Get(profile string, path keyring.Path) (keyring.KeyState, error) {
	return a.store.GetKeyState(profile, path)
}

// Put stores key state
func (a *StateStoreAdapter) Put(profile string, path keyring.Path, ks keyring.KeyState) error {
	return a.store.PutKeyState(profile, path, ks)
}

// Delete removes key state
func (a *StateStoreAdapter) Delete(profile string, path keyring.Path) error {
	return a.store.DeleteKeyState(profile, path)
}

