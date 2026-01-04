package main

import (
	"context"
	"log"
	"os"

	"turn-inspector/cmd"
)

func main() {
	ctx := context.Background()
	if err := cmd.Execute(ctx); err != nil {
		log.Printf("Error: %v", err)
		os.Exit(1)
	}
}

