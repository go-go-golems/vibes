// Package main implements the glazedlint driver
package main

import (
	"golang.org/x/tools/go/analysis/multichecker"

	"github.com/example/glazedlint/checks/parselayers"
	"github.com/example/glazedlint/checks/glazeinterface"
	"github.com/example/glazedlint/checks/configfields"
)

func main() {
	multichecker.Main(
		parselayers.Analyzer,
		glazeinterface.Analyzer,
		configfields.Analyzer,
	)
}
