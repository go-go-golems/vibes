package main

import (
	"fmt"
)

type ContainerExecutor struct{}

func (e *ContainerExecutor) Execute(args []string) error {
	// This is a placeholder for the standalone bundler
	// The actual executor logic is generated in the bundled binary
	return fmt.Errorf("execute mode is only available in bundled binaries")
}

