# Lisp Operating System

A functional operating system implemented in Lisp, demonstrating how high-level programming language concepts can be applied to system software development.

## Overview

The Lisp Operating System is an educational and research platform that implements core operating system functionality using Lisp as the primary implementation language. Unlike traditional operating systems that maintain strict separation between system and application code, this system provides a unified programming environment where system services and user applications use the same language and tools.

## Features

- **Complete Boot Process**: Multi-stage bootloader that transitions from BIOS to protected mode
- **Memory Management**: Physical and virtual memory management with garbage collection
- **Interrupt Handling**: Comprehensive interrupt and exception handling system
- **Lisp Runtime**: Full Lisp interpreter with read-eval-print loop (REPL)
- **File System**: Simple in-memory file system with Lisp interface
- **Interactive Development**: Real-time system modification without restart
- **Educational Focus**: Clear, well-documented code designed for learning

## Quick Start

### Prerequisites

- Linux development environment (Ubuntu 22.04 LTS recommended)
- GCC with 32-bit support
- NASM assembler
- QEMU for virtualization

### Installation

```bash
# Install required packages (Ubuntu/Debian)
sudo apt update
sudo apt install -y build-essential nasm gcc-multilib qemu-system-x86

# Clone or download the source code
# (Source code structure should be set up as shown in the repository)

# Build the operating system
make all

# Run in QEMU
make run
```

### First Steps

Once the system boots, you'll see a Lisp REPL prompt:

```lisp
lisp> (help)
lisp> (+ 1 2 3)
6
lisp> (cons 'hello 'world)
(hello . world)
lisp> (file-list)
```

## System Architecture

The system consists of four main layers:

1. **Hardware Abstraction Layer**: Assembly and C code for hardware control
2. **Kernel Services**: Core OS services (memory, interrupts, devices)
3. **Lisp Runtime**: Complete Lisp evaluation environment
4. **System Services**: File system, I/O, and user interface in Lisp

## Available Functions

### Arithmetic
- `(+ a b ...)` - Addition
- `(- a b ...)` - Subtraction
- `(* a b ...)` - Multiplication
- `(/ a b ...)` - Division

### List Operations
- `(cons a b)` - Create cons cell
- `(car list)` - Get first element
- `(cdr list)` - Get rest of list
- `(list a b ...)` - Create list

### File System
- `(file-create "name")` - Create file
- `(file-write "name" "content")` - Write to file
- `(file-list)` - List files

### System
- `(help)` - Show help
- `(memory-info)` - Memory statistics
- `(gc-stats)` - Garbage collection info

## Documentation

Comprehensive documentation is available in the `docs/` directory:

- `implementation_guide.md` - Detailed technical implementation
- `user_guide.md` - Complete user manual
- `architecture_design.md` - System architecture overview
- `research_notes.md` - Background research and references

## Building from Source

```bash
# Clean previous builds
make clean

# Build all components
make all

# Run in QEMU
make run

# Debug with GDB support
make debug
```

## System Requirements

### Minimum Requirements
- x86-compatible processor (32-bit protected mode)
- 64MB RAM (256MB recommended)
- VGA-compatible display
- PS/2 or USB keyboard

### Development Requirements
- Linux development environment
- GCC with multilib support
- NASM assembler
- QEMU virtualization

## Educational Use

This system is designed for educational purposes and includes:

- **Clear Code Structure**: Well-organized, documented source code
- **Progressive Complexity**: Simple concepts building to advanced features
- **Interactive Learning**: Modify and experiment with running system
- **Comprehensive Documentation**: Detailed explanations of all components

### Suggested Learning Path

1. **Boot Process**: Study the bootloader sequence and kernel initialization
2. **Memory Management**: Explore physical and virtual memory systems
3. **Lisp Runtime**: Understand the interpreter and evaluation engine
4. **System Services**: Examine file system and I/O implementations
5. **Extensions**: Add new features and system services

## Research Applications

The system serves as a platform for research in:

- **Alternative OS Architectures**: High-level language system implementation
- **Interactive System Development**: Real-time system modification
- **Language-Based Security**: Type safety and memory management
- **Educational Tools**: Operating system concept demonstration

## Contributing

This is an educational project designed to demonstrate operating system concepts. Contributions that improve educational value, fix bugs, or add well-documented features are welcome.

### Development Guidelines

- Prioritize clarity over performance
- Include comprehensive documentation
- Maintain educational focus
- Test thoroughly in QEMU

## License

This project is released under an open source license for educational and research use.

## Acknowledgments

This implementation draws inspiration from:

- Symbolics Genera Operating System
- Mezzano Lisp Operating System
- Various educational operating system projects
- Classic Lisp Machine architectures

## Contact

For questions, suggestions, or educational use inquiries, please refer to the documentation or create an issue in the project repository.

---

**Note**: This is an educational operating system designed for learning and research. It is not intended for production use and should only be run in virtual machines or on dedicated hardware for experimentation.

