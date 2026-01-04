/*
 * Lisp Operating System Kernel
 * Main kernel initialization and core functions
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "kernel.h"

// Global variables
static char* video_memory = (char*)0xB8000;
static int cursor_x = 0;
static int cursor_y = 0;

// Function prototypes
void kernel_main(void);
void init_memory(void);
void init_interrupts(void);
void init_lisp_runtime(void);
void terminal_clear(void);
void terminal_putchar(char c);
void terminal_write(const char* str);
void terminal_writeline(const char* str);

/*
 * Kernel entry point - called from stage 2 bootloader
 */
void kernel_main(void)
{
    // Clear the screen
    terminal_clear();
    
    // Display kernel startup message
    terminal_writeline("Lisp OS Kernel Starting...");
    terminal_writeline("Copyright (c) 2025 Manus AI");
    terminal_writeline("");
    
    // Initialize core kernel subsystems
    terminal_writeline("Initializing memory management...");
    init_memory();
    
    terminal_writeline("Setting up interrupt handlers...");
    init_interrupts();
    
    terminal_writeline("Starting Lisp runtime system...");
    init_lisp_runtime();
    
    terminal_writeline("Initializing file system...");
    init_filesystem();
    
    terminal_writeline("Kernel initialization complete!");
    terminal_writeline("");
    terminal_writeline("Welcome to Lisp OS!");
    terminal_writeline("Type (help) for available commands.");
    terminal_writeline("");
    
    // Start the enhanced Lisp REPL
    enhanced_lisp_repl();
    
    // Should never reach here
    terminal_writeline("Kernel panic: REPL exited unexpectedly!");
    while(1) {
        asm volatile("hlt");
    }
}

/*
 * Initialize memory management system
 */
void init_memory(void)
{
    // Parse memory map from bootloader
    memory_map_entry_t* memory_map = (memory_map_entry_t*)0x8000;
    uint16_t num_entries = *(uint16_t*)0x7FFE;
    
    terminal_write("Memory map entries: ");
    terminal_write(itoa(num_entries));
    terminal_writeline("");
    
    // Initialize physical memory allocator
    init_physical_memory(memory_map, num_entries);
    
    // Set up virtual memory
    init_virtual_memory();
    
    // Initialize heap for kernel allocations
    init_kernel_heap();
    
    terminal_writeline("Memory management initialized.");
}

/*
 * Initialize interrupt handling
 */
void init_interrupts(void)
{
    // Set up IDT (Interrupt Descriptor Table)
    init_idt();
    
    // Install exception handlers
    install_exception_handlers();
    
    // Install IRQ handlers
    install_irq_handlers();
    
    // Enable interrupts
    asm volatile("sti");
    
    terminal_writeline("Interrupt system initialized.");
}

/*
 * Initialize Lisp runtime system
 */
void init_lisp_runtime(void)
{
    // Initialize Lisp heap
    init_lisp_heap();
    
    // Initialize symbol table
    init_symbol_table();
    
    // Initialize evaluator
    init_evaluator();
    
    // Load core Lisp functions
    load_core_functions();
    
    terminal_writeline("Lisp runtime system initialized.");
}

/*
 * Clear the terminal screen
 */
void terminal_clear(void)
{
    for (int i = 0; i < 80 * 25 * 2; i += 2) {
        video_memory[i] = ' ';
        video_memory[i + 1] = 0x07; // Light gray on black
    }
    cursor_x = 0;
    cursor_y = 0;
}

/*
 * Put a character on the screen
 */
void terminal_putchar(char c)
{
    if (c == '\n') {
        cursor_x = 0;
        cursor_y++;
    } else if (c == '\r') {
        cursor_x = 0;
    } else {
        int offset = (cursor_y * 80 + cursor_x) * 2;
        video_memory[offset] = c;
        video_memory[offset + 1] = 0x07; // Light gray on black
        cursor_x++;
    }
    
    // Handle line wrapping
    if (cursor_x >= 80) {
        cursor_x = 0;
        cursor_y++;
    }
    
    // Handle scrolling
    if (cursor_y >= 25) {
        // Scroll up one line
        for (int i = 0; i < 24 * 80 * 2; i++) {
            video_memory[i] = video_memory[i + 80 * 2];
        }
        // Clear last line
        for (int i = 24 * 80 * 2; i < 25 * 80 * 2; i += 2) {
            video_memory[i] = ' ';
            video_memory[i + 1] = 0x07;
        }
        cursor_y = 24;
    }
}

/*
 * Write a string to the terminal
 */
void terminal_write(const char* str)
{
    while (*str) {
        terminal_putchar(*str);
        str++;
    }
}

/*
 * Write a string followed by a newline
 */
void terminal_writeline(const char* str)
{
    terminal_write(str);
    terminal_putchar('\n');
}

/*
 * Simple integer to string conversion
 */
char* itoa(int value)
{
    static char buffer[12];
    char* ptr = buffer + 11;
    *ptr = '\0';
    
    if (value == 0) {
        *(--ptr) = '0';
        return ptr;
    }
    
    int negative = 0;
    if (value < 0) {
        negative = 1;
        value = -value;
    }
    
    while (value > 0) {
        *(--ptr) = '0' + (value % 10);
        value /= 10;
    }
    
    if (negative) {
        *(--ptr) = '-';
    }
    
    return ptr;
}

