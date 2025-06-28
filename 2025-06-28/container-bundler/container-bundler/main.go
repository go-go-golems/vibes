package main

import (
	"flag"
	"fmt"
	"log"
	"os"
)

const (
	Version = "1.0.0"
)

func main() {
	var (
		imageRef    = flag.String("image", "", "Container image reference (e.g., docker://alpine:latest)")
		outputPath  = flag.String("output", "", "Output binary path")
		bundleMode  = flag.Bool("bundle", false, "Bundle mode: create a bundled binary")
		executeMode = flag.Bool("execute", false, "Execute mode: run the bundled container")
		showVersion = flag.Bool("version", false, "Show version information")
		showHelp    = flag.Bool("help", false, "Show help information")
	)
	flag.Parse()

	if *showVersion {
		fmt.Printf("Container Bundler v%s\n", Version)
		return
	}

	if *showHelp || len(os.Args) == 1 {
		printHelp()
		return
	}

	if *bundleMode {
		if *imageRef == "" || *outputPath == "" {
			log.Fatal("Bundle mode requires both -image and -output flags")
		}
		err := bundleContainer(*imageRef, *outputPath)
		if err != nil {
			log.Fatalf("Failed to bundle container: %v", err)
		}
		fmt.Printf("Successfully bundled container to: %s\n", *outputPath)
	} else if *executeMode {
		err := executeContainer(flag.Args())
		if err != nil {
			log.Fatalf("Failed to execute container: %v", err)
		}
	} else {
		printHelp()
	}
}

func printHelp() {
	fmt.Printf(`Container Bundler v%s

A tool to bundle container images into self-contained Go binaries.

USAGE:
    container-bundler [OPTIONS]

BUNDLE MODE:
    container-bundler -bundle -image <image-ref> -output <binary-path>

EXECUTE MODE (used by bundled binaries):
    container-bundler -execute [container-args...]

OPTIONS:
    -image string     Container image reference (e.g., docker://alpine:latest)
    -output string    Output binary path
    -bundle          Bundle mode: create a bundled binary
    -execute         Execute mode: run the bundled container
    -version         Show version information
    -help            Show this help message

EXAMPLES:
    # Bundle Alpine Linux into a binary
    container-bundler -bundle -image docker://alpine:latest -output alpine-bundle

    # Bundle a specific application
    container-bundler -bundle -image docker://nginx:latest -output nginx-bundle

    # The bundled binary can then be executed directly:
    ./alpine-bundle sh
    ./nginx-bundle

SUPPORTED IMAGE SOURCES:
    - docker://registry/image:tag  (Docker Hub and other registries)
    - docker-daemon:image:tag      (Local Docker daemon)
    - oci:path                     (OCI layout directory)

`, Version)
}

func bundleContainer(imageRef, outputPath string) error {
	fmt.Printf("Bundling container image: %s\n", imageRef)
	fmt.Printf("Output path: %s\n", outputPath)
	
	// TODO: Implement container bundling logic
	bundler := &ContainerBundler{}
	return bundler.Bundle(imageRef, outputPath)
}

func executeContainer(args []string) error {
	fmt.Printf("Executing bundled container with args: %v\n", args)
	
	// TODO: Implement container execution logic
	executor := &ContainerExecutor{}
	return executor.Execute(args)
}

