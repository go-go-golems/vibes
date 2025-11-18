package ui

// max returns the maximum of two integers
// Note: Go 1.21+ has builtin max(), but keeping this for compatibility
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

