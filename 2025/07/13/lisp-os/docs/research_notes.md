# Lisp Operating System Research Notes

## Genera Operating System (Symbolics)

### Overview
- Commercial operating system and integrated development environment for Lisp machines
- Created by Symbolics, originally based on MIT AI Lab's Lisp machines
- Object-oriented operating system based on Lisp programming language
- Supports incremental and interactive development of complex software

### Key Features
- **Programming Languages**: Originally developed in Lisp Machine Lisp using Flavors object-oriented extension
- **Evolution**: Later supported Common Lisp and Common Lisp Object System (CLOS)
- **Current Implementation**: Mostly written in ZetaLisp and Symbolics Common Lisp
- **Object Systems**: Uses Flavors, New Flavors, and CLOS

### User Interface
- Primarily monochrome (black-and-white) interface
- Extensive color support available through color frame buffers or X Window System
- **Dynamic Lisp Listener**: Command line interface with full graphics and mouse interaction
- **Document Examiner**: Early hypertext browser for fully hyperlinked online documentation
- PostScript printing support with built-in PostScript interpreter (written in Lisp)

### Platform Support
- Originally ran on Symbolics Lisp Machines
- Later ported to DEC Alpha processors running Tru64 UNIX
- Current versions run on x86-64 and Arm64 Linux, macOS (including Apple Silicon)

### Architecture Insights
- Object-oriented operating system design
- Extensive use of Lisp for system implementation
- Interactive development environment integrated into the OS
- Hyperlinked documentation system
- Graphics and mouse support integrated at OS level

Source: https://en.wikipedia.org/wiki/Genera_(operating_system)



## Mezzano Operating System (Modern Implementation)

### Overview
- Modern operating system written entirely in Common Lisp
- Open source project (MIT License) with active development
- Inspired by historic Lisp machines
- 99.8% Common Lisp, 0.2% other languages

### Key Features
- **Platform Support**: x86-64 primary, AArch64 experimental
- **Virtualization**: Designed for VirtualBox and QEMU
- **Hardware Requirements**: 2GB RAM, virtio-net NIC, Intel HDA audio
- **Real Hardware**: Can boot from CD/USB on real hardware

### Major Components (Directory Structure)
- **applications/**: User applications and programs
- **compiler/**: Lisp compiler implementation
- **disk/**: Disk and storage management
- **drivers/**: Hardware drivers
- **file-server/**: File system services
- **gui/**: Graphical user interface
- **net/**: Networking stack
- **runtime/**: Runtime system and garbage collector
- **supervisor/**: Kernel/supervisor layer
- **system/**: Core system components

### Advanced Features
- **File Systems**: FAT32, EXT2/3/4 support
- **Networking**: TCP/IP stack, DHCP, server support
- **Graphics**: Hardware accelerated 3D via Virgl, GMA950 driver
- **Audio**: Intel HDA audio device support
- **Multicore**: SMP (Symmetric Multiprocessing) support
- **Memory Management**: Generational garbage collection
- **Development Tools**: McCLIM, Quicklisp integration, DISASSEMBLE, ED

### Development Environment
- **Compiler**: SSA-based compiler backend with unboxed value representations
- **CLOS**: Improved MOP (Meta-Object Protocol) conformance
- **Debugging**: Stack overflow and memory fault recovery
- **Introspection**: Source location tracking, weak hash tables

### Build System
- Uses MBuild repository for building from source
- Supports building on Windows
- Pre-built demo releases available

Source: https://github.com/froggey/Mezzano


## x86 Bootloader Implementation

### Boot Process Overview
- **BIOS Boot**: BIOS loads first 512 bytes (boot sector) ending with magic number 0xAA55
- **Load Address**: Bootloader loaded at physical address 0x7C00
- **Initial Mode**: CPU starts in 16-bit Real Mode for backwards compatibility

### Real Mode vs Protected Mode
**Real Mode (16-bit)**:
- Access to BIOS subroutines
- 16-bit operations only
- Limited to 1MB of memory access
- Direct hardware access

**Protected Mode (32-bit)**:
- Virtual memory and paging support
- 32-bit operations
- Memory protection between programs
- Fault handlers for program errors
- Four privilege levels (Ring 0-3)

### Bootloader Requirements
- Must be exactly 512 bytes
- Must end with magic number 0xAA55 (little endian)
- Written in assembly language (NASM recommended)
- Uses BIOS interrupts for basic I/O operations

### Example Bootloader Structure
```assembly
bits 16         ; 16-bit code
org 0x7c00      ; Starting location

boot:
    mov si, message
    mov ah, 0x0e    ; BIOS display character command
.loop:
    lodsb           ; Load character into AL
    cmp al, 0       ; Check for null terminator
    je halt
    int 0x10        ; BIOS video interrupt
    jmp .loop

halt:
    hlt

message:
    db "Hello World!", 0

times 510-($-$$) db 0  ; Pad to 510 bytes
dw 0xAA55              ; Boot signature
```

### Protected Mode Transition
To enter protected mode:
1. Set up Global Descriptor Table (GDT) with 3 entries:
   - Null descriptor
   - Code segment descriptor  
   - Data segment descriptor
2. Set protected mode bit in CR0 control register
3. Enable A20 line for >1MB memory access

Source: https://www.alanfoster.me/posts/writing-a-bootloader/

## Minimal Lisp Interpreter Implementation

### Core Components (MiniLisp Analysis)
**Data Types**:
- Integers
- Symbols (identifiers)
- Cons cells (pairs)
- Lists (linked cons cells)
- Functions and closures

**Essential Operations**:
- `cons`, `car`, `cdr` for list manipulation
- `+`, `-`, `=`, `<` for arithmetic and comparison
- `if` for conditionals
- `while` for loops
- `define`, `setq` for variable assignment
- `lambda`, `defun` for function definition

### Memory Management
- **Garbage Collection**: Copying garbage collector
- **Lexical Scoping**: Variables have lexical scope and indefinite extent
- **Closures**: Functions can capture outer variables

### Evaluation Engine
- **Read-Eval-Print Loop (REPL)**: Traditional Lisp interaction model
- **Expression Evaluation**: Recursive evaluation of nested expressions
- **Macro System**: Code transformation at compile time
- **Symbol Table**: Global and local variable environments

### Implementation Size
- Complete interpreter in ~1000 lines of C
- Includes comprehensive feature set
- Heavily commented for readability
- Portable across Unix-like systems

### Key Features for OS Implementation
- **Minimal Core**: Small enough to fit in kernel space
- **Interactive Development**: REPL for system interaction
- **Macro System**: Code generation and transformation
- **Garbage Collection**: Automatic memory management
- **Lexical Scoping**: Proper variable binding

Source: https://github.com/rui314/minilisp

## Architecture Insights for Lisp OS Design

### Historical Lessons (Genera)
- Object-oriented OS design using Lisp
- Integrated development environment
- Hyperlinked documentation system
- Interactive debugging and introspection
- System construction toolkit for modularity

### Modern Implementation (Mezzano)
- Microkernel-like architecture with supervisor layer
- Separate components for drivers, networking, GUI
- Common Lisp as system implementation language
- Hardware abstraction through driver layer
- Modern features: SMP, 3D graphics, networking

### Design Principles
1. **Everything in Lisp**: Minimize non-Lisp code
2. **Interactive Development**: Live system modification
3. **Modular Architecture**: Separate concerns into components
4. **Memory Safety**: Garbage collection prevents memory errors
5. **Introspection**: System can examine and modify itself

