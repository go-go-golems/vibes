# Lisp Operating System User Guide

**Author:** Manus AI  
**Date:** July 13, 2025  
**Version:** 1.0

## Table of Contents

1. [Introduction](#introduction)
2. [System Requirements](#system-requirements)
3. [Installation and Setup](#installation-and-setup)
4. [Building the Operating System](#building-the-operating-system)
5. [Running the System](#running-the-system)
6. [Using the Lisp Environment](#using-the-lisp-environment)
7. [Available Commands and Functions](#available-commands-and-functions)
8. [File System Operations](#file-system-operations)
9. [System Administration](#system-administration)
10. [Troubleshooting](#troubleshooting)
11. [Development and Customization](#development-and-customization)
12. [Frequently Asked Questions](#frequently-asked-questions)

## Introduction

Welcome to the Lisp Operating System, a unique educational and research platform that demonstrates how high-level programming language concepts can be applied to operating system development. Unlike traditional operating systems that maintain strict separation between system and application code, the Lisp OS provides a unified programming environment where system services and user applications are implemented using the same language and tools.

This user guide provides comprehensive instructions for building, installing, and using the Lisp Operating System. Whether you are a student learning about operating system concepts, a researcher exploring alternative system architectures, or a developer interested in Lisp-based system programming, this guide will help you get started with the system and make the most of its unique capabilities.

### What Makes This System Special

The Lisp Operating System offers several unique features that distinguish it from traditional operating systems:

**Interactive Development Environment**: The system provides a complete Lisp development environment that allows real-time modification of system behavior without requiring recompilation or system restart. This capability makes it an excellent platform for experimenting with operating system concepts and exploring system behavior.

**Unified Programming Model**: System services and user applications use the same programming language and development tools, eliminating the traditional boundary between system and application programming. This unified approach simplifies system development and makes it easier to understand how different system components interact.

**Educational Focus**: The system is designed with education in mind, prioritizing clarity and understandability over performance optimization. The implementation includes extensive documentation and examples that make it suitable for classroom use and self-study.

**Research Platform**: The system provides a foundation for exploring advanced operating system concepts such as persistent object systems, distributed computing, and alternative user interface paradigms. The flexible architecture makes it easy to experiment with new ideas and approaches.

### Target Audience

This guide is written for users with varying levels of experience with operating systems and Lisp programming:

**Students and Educators**: The system serves as an excellent educational tool for understanding operating system concepts. The guide includes detailed explanations of system behavior and suggestions for educational exercises and experiments.

**Researchers**: The system provides a platform for exploring alternative approaches to operating system design. The guide includes information about the system architecture and extension points that can be used for research projects.

**Developers**: The system demonstrates how modern programming language concepts can be applied to system programming. The guide includes information about the development environment and tools for customizing and extending the system.

**Hobbyists**: The system offers a unique and interesting project for those interested in operating systems and Lisp programming. The guide provides step-by-step instructions for building and running the system on standard PC hardware.

## System Requirements

The Lisp Operating System is designed to run on standard x86 PC hardware and can be tested using virtual machine software. The system requirements are modest, making it accessible to users with a wide range of hardware configurations.

### Hardware Requirements

**Processor**: The system requires an x86-compatible processor with support for 32-bit protected mode operation. This includes virtually all processors manufactured since the Intel 80386, making the system compatible with both modern and legacy hardware.

**Memory**: A minimum of 256MB of RAM is recommended for basic operation, though the system can run with as little as 64MB. Additional memory improves performance and allows for larger Lisp programs and data structures.

**Storage**: The system requires minimal storage space, with the complete system image occupying less than 10MB. Any standard hard drive or solid-state drive with at least 100MB of free space is sufficient for development and experimentation.

**Display**: The system supports standard VGA-compatible display adapters and operates in text mode. No special graphics capabilities are required, making the system compatible with virtually all PC hardware.

**Input**: A standard PS/2 or USB keyboard is required for system interaction. Mouse support is not currently implemented, though it could be added as a future enhancement.

### Software Requirements

**Development Environment**: Building the system requires a Linux development environment with standard development tools. Ubuntu 22.04 LTS is the recommended platform, though other Linux distributions should work with appropriate package installations.

**Required Packages**: The following packages must be installed on the development system:
- `build-essential` (GCC compiler and related tools)
- `nasm` (Netwide Assembler for assembly language components)
- `gcc-multilib` (32-bit compilation support)
- `qemu-system-x86` (Virtual machine for testing)

**Virtual Machine Software**: While the system can run on physical hardware, virtual machine software is recommended for development and testing. QEMU is the preferred virtualization platform and is included in the build system, though other virtualization platforms such as VirtualBox or VMware can also be used.

### Development Tools

**Text Editor**: Any text editor capable of editing C and assembly language source files is suitable for system development. Popular choices include vim, emacs, Visual Studio Code, and Sublime Text.

**Version Control**: Git is recommended for tracking changes to the system source code, though it is not required for basic system building and operation.

**Debugging Tools**: GDB (GNU Debugger) can be used with QEMU for debugging system components, though this is primarily useful for advanced development work.

## Installation and Setup

Setting up the development environment for the Lisp Operating System is straightforward and can be completed in a few simple steps. The process involves installing required packages, obtaining the source code, and verifying that the build environment is properly configured.

### Installing Required Packages

On Ubuntu 22.04 LTS or compatible systems, install the required packages using the following commands:

```bash
# Update package list
sudo apt update

# Install development tools
sudo apt install -y build-essential nasm gcc-multilib qemu-system-x86

# Verify installations
gcc --version
nasm --version
qemu-system-x86_64 --version
```

The installation process should complete without errors. If you encounter package dependency issues, ensure that your system is up to date and that the universe repository is enabled.

### Obtaining the Source Code

The Lisp Operating System source code is organized in a clear directory structure that separates different system components:

```
lisp-os/
├── boot/           # Bootloader components
├── kernel/         # Kernel implementation
├── lisp/           # Lisp runtime system
├── docs/           # Documentation
├── build/          # Build output (created during compilation)
└── Makefile        # Build system
```

Create a working directory and set up the source code structure:

```bash
# Create working directory
mkdir ~/lisp-os-development
cd ~/lisp-os-development

# The source code structure should be set up as shown above
```

### Verifying the Development Environment

Before proceeding with system building, verify that the development environment is properly configured:

```bash
# Test 32-bit compilation
echo 'int main() { return 0; }' > test.c
gcc -m32 test.c -o test
./test && echo "32-bit compilation works"
rm test test.c

# Test assembly compilation
echo 'global _start; _start: mov eax, 1; int 0x80' > test.asm
nasm -f elf32 test.asm -o test.o
ld -m elf_i386 test.o -o test
rm test test.o test.asm

# Test QEMU
qemu-system-x86_64 --version
```

All tests should complete successfully. If any test fails, review the package installation and ensure that all required dependencies are properly installed.

### Directory Structure Overview

Understanding the source code organization will help you navigate the system and make modifications:

**boot/**: Contains the bootloader components including stage1.asm (first stage bootloader) and stage2.asm (second stage bootloader). These files are implemented in assembly language and handle the initial system startup.

**kernel/**: Contains the kernel implementation including memory management, interrupt handling, and device drivers. These files are primarily implemented in C with some assembly language components.

**lisp/**: Contains the Lisp runtime system including the parser, evaluator, and built-in functions. These files implement the core Lisp functionality that makes the system unique.

**docs/**: Contains system documentation including architecture descriptions, implementation guides, and user documentation.

**build/**: Created during the build process to contain compiled object files and the final system image. This directory is automatically created and managed by the build system.

## Building the Operating System

The build process for the Lisp Operating System is automated through a comprehensive Makefile that handles compilation, linking, and image creation. The build system is designed to be simple and reliable, with clear error messages and automatic dependency management.

### Basic Build Process

To build the complete operating system, use the following commands:

```bash
# Navigate to the source directory
cd ~/lisp-os-development/lisp-os

# Build the complete system
make all

# The build process will create build/lisp-os.img
```

The build process consists of several stages:

1. **Bootloader Compilation**: The assembly language bootloaders are compiled using NASM
2. **Kernel Compilation**: C source files are compiled with appropriate flags for kernel development
3. **Lisp Runtime Compilation**: The Lisp interpreter and runtime system are compiled
4. **Linking**: All components are linked together to create the kernel executable
5. **Image Creation**: The bootloaders and kernel are combined into a bootable disk image

### Build System Details

The Makefile includes several useful targets for different aspects of system development:

```bash
# Clean all build artifacts
make clean

# Build only the kernel (without creating disk image)
make kernel

# Install build dependencies (Ubuntu/Debian)
make install-deps

# Show help information
make help
```

### Compilation Flags and Options

The build system uses specific compilation flags optimized for kernel development:

- `-m32`: Generate 32-bit code for x86 compatibility
- `-ffreestanding`: Indicate that the code does not use standard library
- `-fno-stack-protector`: Disable stack protection (not available in kernel environment)
- `-fno-builtin`: Disable built-in function optimizations
- `-nostdlib`: Do not link against standard library
- `-Wall -Wextra`: Enable comprehensive warning messages

These flags ensure that the generated code is suitable for kernel execution and does not depend on runtime libraries that are not available in the kernel environment.

### Troubleshooting Build Issues

Common build problems and their solutions:

**Missing 32-bit Libraries**: If you encounter errors about missing 32-bit libraries, ensure that `gcc-multilib` is properly installed:
```bash
sudo apt install gcc-multilib
```

**Assembly Compilation Errors**: If NASM reports errors in assembly files, verify that NASM is properly installed and that the source files are not corrupted:
```bash
nasm --version
```

**Linking Errors**: If the linker reports undefined symbols, ensure that all source files are properly compiled and that the linker script is correct.

**Permission Errors**: If you encounter permission errors during the build process, ensure that you have write access to the build directory and that no files are locked by other processes.

### Build Output

A successful build produces several important files:

- `build/stage1.bin`: First stage bootloader (512 bytes)
- `build/stage2.bin`: Second stage bootloader (4KB)
- `build/kernel.bin`: Compiled kernel executable
- `build/lisp-os.img`: Complete bootable disk image (10MB)

The disk image (`lisp-os.img`) is the final product that can be used to boot the operating system in a virtual machine or on physical hardware.

## Running the System

The Lisp Operating System can be run in several different environments, from virtual machines for development and testing to physical hardware for demonstration purposes. This section provides detailed instructions for running the system in various configurations.

### Running in QEMU

QEMU is the recommended platform for running the Lisp Operating System during development and testing. The build system includes convenient targets for launching QEMU with appropriate configuration:

```bash
# Run the system in QEMU
make run

# Run with debugging support (GDB can attach)
make debug
```

The `make run` command launches QEMU with the following configuration:
- 256MB of RAM
- The Lisp OS disk image as the primary boot device
- Serial console output for debugging
- Standard VGA display

### Manual QEMU Execution

For more control over the QEMU configuration, you can launch the system manually:

```bash
# Basic QEMU execution
qemu-system-x86_64 -drive format=raw,file=build/lisp-os.img -m 256M

# With serial console output
qemu-system-x86_64 -drive format=raw,file=build/lisp-os.img -m 256M -serial stdio

# With debugging support
qemu-system-x86_64 -drive format=raw,file=build/lisp-os.img -m 256M -s -S
```

### Boot Process Observation

When the system boots successfully, you will see the following sequence of messages:

```
SeaBIOS (version 1.15.0-1)
Booting from Hard Disk..
Lisp OS Stage 1 Bootloader
Loading Stage 2...
Stage 2 loaded successfully!
Lisp OS Stage 2 Bootloader
Detecting memory...
Enabling A20 line...
Loading kernel...
Kernel loaded successfully!
Entering protected mode...

Lisp OS Kernel Starting...
Copyright (c) 2025 Manus AI

Initializing memory management...
Memory map entries: 6
Total pages: 65536
Used pages: 1024
Free pages: 64512
Memory management initialized.

Setting up interrupt handlers...
IDT initialized.
Exception handlers installed.
IRQ handlers installed.
Interrupt system initialized.

Starting Lisp runtime system...
Lisp heap: 8388608 bytes allocated
Symbol table initialized
Loading core Lisp functions...
Core functions loaded
Lisp runtime system initialized.

Initializing file system...
File system initialized

Kernel initialization complete!

Welcome to Lisp OS!
Type (help) for available commands.

lisp> 
```

### System Interaction

Once the system has booted successfully, you will be presented with a Lisp read-eval-print loop (REPL) that serves as the primary user interface. The REPL accepts Lisp expressions and evaluates them, displaying the results.

Basic interaction examples:

```lisp
lisp> (+ 1 2 3)
6

lisp> (cons 'hello 'world)
(hello . world)

lisp> (help)
```

The system responds to keyboard input and displays output on the console. Use standard Lisp syntax for all interactions with the system.

### Running on Physical Hardware

While virtual machine testing is recommended for development, the system can also run on physical x86 hardware. To create a bootable USB drive or CD-ROM:

```bash
# Create bootable USB drive (replace /dev/sdX with your USB device)
sudo dd if=build/lisp-os.img of=/dev/sdX bs=1M

# Create ISO image for CD-ROM burning
# (This requires additional tools and configuration)
```

**Warning**: Be extremely careful when using `dd` to write to physical devices. Specifying the wrong device can result in data loss. Always verify the device name before executing the command.

### Performance Considerations

The system is designed for educational and research use rather than performance optimization. Typical performance characteristics include:

- Boot time: 2-5 seconds in QEMU
- Memory usage: 50-100MB for basic operation
- Response time: Interactive for typical Lisp operations

Performance can be improved by allocating more memory to the virtual machine or by running on faster hardware, though the system is designed to be usable even on modest hardware configurations.

## Using the Lisp Environment

The Lisp environment is the heart of the operating system, providing both the user interface and the platform for system programming. Understanding how to use the Lisp environment effectively is essential for getting the most out of the system.

### Basic Lisp Syntax

The system implements a subset of Common Lisp with extensions for system programming. Basic syntax follows standard Lisp conventions:

**Atoms**: Numbers, symbols, and strings are basic data types:
```lisp
lisp> 42
42

lisp> 'hello
hello

lisp> "Hello, World!"
"Hello, World!"
```

**Lists**: Lists are the fundamental data structure in Lisp:
```lisp
lisp> '(1 2 3)
(1 2 3)

lisp> (list 'a 'b 'c)
(a b c)

lisp> (cons 1 (cons 2 (cons 3 nil)))
(1 2 3)
```

**Function Calls**: Functions are called using prefix notation:
```lisp
lisp> (+ 1 2 3 4)
10

lisp> (* 2 (+ 3 4))
14

lisp> (car '(a b c))
a

lisp> (cdr '(a b c))
(b c)
```

### Interactive Development

One of the key advantages of the Lisp environment is its support for interactive development. You can define functions, test them immediately, and modify them without restarting the system:

```lisp
lisp> (defun square (x) (* x x))
square

lisp> (square 5)
25

lisp> (defun factorial (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1)))))
factorial

lisp> (factorial 5)
120
```

### Error Handling

The system includes comprehensive error handling that provides useful diagnostic information:

```lisp
lisp> (+ 1 'hello)
Error: Type error in arithmetic operation

lisp> (car 42)
Error: CAR applied to non-list

lisp> (undefined-function)
Error: Undefined function: undefined-function
```

Error messages are designed to be helpful for debugging and learning, providing clear information about what went wrong and how to fix it.

### System Introspection

The Lisp environment provides powerful introspection capabilities that allow you to examine and modify system behavior:

```lisp
lisp> (memory-info)
Total memory: 268435456 bytes
Used memory: 12345678 bytes
Free memory: 256089778 bytes

lisp> (gc-stats)
Collections: 5
Objects allocated: 1234
Objects freed: 567

lisp> (symbol-table-stats)
Symbols: 89
Packages: 1
```

These introspection functions provide valuable information for understanding system behavior and debugging programs.

## Available Commands and Functions

The Lisp Operating System includes a comprehensive set of built-in functions that provide both standard Lisp functionality and system-specific operations. This section provides a complete reference to the available commands and functions.

### Arithmetic Functions

The system provides standard arithmetic operations with support for integer arithmetic:

```lisp
(+ arg1 arg2 ...)          ; Addition
(- arg1 arg2 ...)          ; Subtraction  
(* arg1 arg2 ...)          ; Multiplication
(/ arg1 arg2 ...)          ; Division
(= arg1 arg2)              ; Equality test
(< arg1 arg2)              ; Less than test
```

Examples:
```lisp
lisp> (+ 1 2 3 4 5)
15

lisp> (- 10 3)
7

lisp> (* 6 7)
42

lisp> (/ 20 4)
5

lisp> (= 5 5)
t

lisp> (< 3 7)
t
```

### List Manipulation Functions

Standard Lisp list operations are provided for working with list data structures:

```lisp
(cons car cdr)             ; Create cons cell
(car list)                 ; Get first element
(cdr list)                 ; Get rest of list
(list arg1 arg2 ...)       ; Create list
(quote expr)               ; Quote expression (prevent evaluation)
```

Examples:
```lisp
lisp> (cons 'a '(b c))
(a b c)

lisp> (car '(x y z))
x

lisp> (cdr '(x y z))
(y z)

lisp> (list 1 2 3)
(1 2 3)

lisp> (quote (+ 1 2))
(+ 1 2)

lisp> '(+ 1 2)
(+ 1 2)
```

### System Information Functions

The system provides functions for accessing system information and status:

```lisp
(help)                     ; Display help information
(memory-info)              ; Display memory usage
(gc-stats)                 ; Display garbage collection statistics
(system-info)              ; Display system information
```

### File System Functions

Basic file system operations are provided for managing files:

```lisp
(file-create "filename")   ; Create new file
(file-write "filename" "content")  ; Write content to file
(file-list)                ; List all files
```

Examples:
```lisp
lisp> (file-create "test.txt")
t

lisp> (file-write "test.txt" "Hello, World!")
t

lisp> (file-list)
Files:
  test.txt (13 bytes)
Total files: 1/64
t
```

### Control Flow Functions

Basic control flow operations are available for program logic:

```lisp
(if condition then-expr else-expr)  ; Conditional expression
(cond (test1 expr1) (test2 expr2) ...)  ; Multiple conditions
```

Examples:
```lisp
lisp> (if (< 3 5) 'yes 'no)
yes

lisp> (cond ((< 5 3) 'impossible)
            ((= 5 5) 'equal)
            (t 'default))
equal
```

### Function Definition

Functions can be defined using the `defun` special form:

```lisp
(defun name (parameters) body)  ; Define function
```

Examples:
```lisp
lisp> (defun double (x) (* x 2))
double

lisp> (double 21)
42

lisp> (defun greet (name)
        (list 'hello name))
greet

lisp> (greet 'world)
(hello world)
```

