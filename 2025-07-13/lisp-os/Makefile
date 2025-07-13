# Makefile for Lisp Operating System
# Author: Manus AI
# Date: July 13, 2025

# Tools
ASM = nasm
CC = gcc
LD = ld
DD = dd

# Directories
BOOT_DIR = boot
KERNEL_DIR = kernel
BUILD_DIR = build
LISP_DIR = lisp

# Flags
ASMFLAGS = -f bin
ASMFLAGS_ELF = -f elf32
CFLAGS = -m32 -ffreestanding -fno-stack-protector -fno-builtin -nostdlib -Wall -Wextra
LDFLAGS = -m elf_i386 -T kernel/linker.ld

# Output files
STAGE1_BIN = $(BUILD_DIR)/stage1.bin
STAGE2_BIN = $(BUILD_DIR)/stage2.bin
KERNEL_BIN = $(BUILD_DIR)/kernel.bin
OS_IMG = $(BUILD_DIR)/lisp-os.img

# Source files
BOOT_SOURCES = $(BOOT_DIR)/stage1.asm $(BOOT_DIR)/stage2.asm
KERNEL_C_SOURCES = $(wildcard $(KERNEL_DIR)/*.c)
KERNEL_ASM_SOURCES = $(wildcard $(KERNEL_DIR)/*.asm)
LISP_SOURCES = $(wildcard $(LISP_DIR)/*.c)

# Object files
KERNEL_C_OBJECTS = $(patsubst $(KERNEL_DIR)/%.c, $(BUILD_DIR)/%.o, $(KERNEL_C_SOURCES))
KERNEL_ASM_OBJECTS = $(patsubst $(KERNEL_DIR)/%.asm, $(BUILD_DIR)/%.o, $(KERNEL_ASM_SOURCES))
LISP_OBJECTS = $(patsubst $(LISP_DIR)/%.c, $(BUILD_DIR)/%.o, $(LISP_SOURCES))

# Default target
all: $(OS_IMG)

# Create build directory
$(BUILD_DIR):
	mkdir -p $(BUILD_DIR)

# Build stage 1 bootloader
$(STAGE1_BIN): $(BOOT_DIR)/stage1.asm | $(BUILD_DIR)
	$(ASM) $(ASMFLAGS) $< -o $@

# Build stage 2 bootloader
$(STAGE2_BIN): $(BOOT_DIR)/stage2.asm | $(BUILD_DIR)
	$(ASM) $(ASMFLAGS) $< -o $@

# Build kernel C objects
$(BUILD_DIR)/%.o: $(KERNEL_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Build kernel assembly objects
$(BUILD_DIR)/%.o: $(KERNEL_DIR)/%.asm | $(BUILD_DIR)
	$(ASM) $(ASMFLAGS_ELF) $< -o $@

# Build Lisp objects
$(BUILD_DIR)/%.o: $(LISP_DIR)/%.c | $(BUILD_DIR)
	$(CC) $(CFLAGS) -c $< -o $@

# Create linker script
$(KERNEL_DIR)/linker.ld:
	@echo "Creating linker script..."
	@echo "ENTRY(kernel_main)" > $@
	@echo "SECTIONS {" >> $@
	@echo "    . = 0x10000;" >> $@
	@echo "    .text : { *(.text) }" >> $@
	@echo "    .data : { *(.data) }" >> $@
	@echo "    .bss : { *(.bss) }" >> $@
	@echo "}" >> $@

# Link kernel
$(KERNEL_BIN): $(KERNEL_C_OBJECTS) $(KERNEL_ASM_OBJECTS) $(LISP_OBJECTS) $(KERNEL_DIR)/linker.ld
	$(LD) $(LDFLAGS) $(KERNEL_C_OBJECTS) $(KERNEL_ASM_OBJECTS) $(LISP_OBJECTS) -o $(BUILD_DIR)/kernel.elf
	objcopy -O binary $(BUILD_DIR)/kernel.elf $@

# Create OS image
$(OS_IMG): $(STAGE1_BIN) $(STAGE2_BIN) $(KERNEL_BIN)
	@echo "Creating OS image..."
	# Create empty 10MB image
	$(DD) if=/dev/zero of=$@ bs=1024 count=10240
	# Write stage 1 bootloader to first sector
	$(DD) if=$(STAGE1_BIN) of=$@ bs=512 count=1 conv=notrunc
	# Write stage 2 bootloader starting at sector 2
	$(DD) if=$(STAGE2_BIN) of=$@ bs=512 seek=1 count=8 conv=notrunc
	# Write kernel starting at sector 10
	$(DD) if=$(KERNEL_BIN) of=$@ bs=512 seek=9 conv=notrunc
	@echo "OS image created: $@"

# Run in QEMU
run: $(OS_IMG)
	qemu-system-x86_64 -drive format=raw,file=$(OS_IMG) -m 256M

# Run in QEMU with debugging
debug: $(OS_IMG)
	qemu-system-x86_64 -drive format=raw,file=$(OS_IMG) -m 256M -s -S

# Clean build files
clean:
	rm -rf $(BUILD_DIR)
	rm -f $(KERNEL_DIR)/linker.ld

# Install dependencies (for Ubuntu/Debian)
install-deps:
	sudo apt update
	sudo apt install -y nasm gcc-multilib qemu-system-x86

# Help
help:
	@echo "Lisp Operating System Build System"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Build the complete OS image"
	@echo "  run          - Build and run in QEMU"
	@echo "  debug        - Build and run in QEMU with GDB debugging"
	@echo "  clean        - Remove all build files"
	@echo "  install-deps - Install required dependencies"
	@echo "  help         - Show this help message"

.PHONY: all run debug clean install-deps help

