/*
 * Lisp Operating System Kernel Header
 * Definitions and prototypes for kernel functions
 * Author: Manus AI
 * Date: July 13, 2025
 */

#ifndef KERNEL_H
#define KERNEL_H

#include <stdint.h>
#include <stddef.h>

// Basic type definitions
typedef unsigned char uint8_t;
typedef unsigned short uint16_t;
typedef unsigned int uint32_t;
typedef unsigned long long uint64_t;
typedef signed char int8_t;
typedef signed short int16_t;
typedef signed int int32_t;
typedef signed long long int64_t;

// Memory map entry structure (from BIOS E820)
typedef struct {
    uint64_t base_addr;
    uint64_t length;
    uint32_t type;
    uint32_t acpi_attrs;
} __attribute__((packed)) memory_map_entry_t;

// Memory management constants
#define PAGE_SIZE 4096
#define KERNEL_HEAP_START 0x100000
#define KERNEL_HEAP_SIZE 0x100000
#define LISP_HEAP_START 0x200000
#define LISP_HEAP_SIZE 0x800000

// Memory types from E820
#define MEMORY_TYPE_USABLE 1
#define MEMORY_TYPE_RESERVED 2
#define MEMORY_TYPE_ACPI_RECLAIMABLE 3
#define MEMORY_TYPE_ACPI_NVS 4
#define MEMORY_TYPE_BAD 5

// Interrupt and exception numbers
#define IRQ_TIMER 0
#define IRQ_KEYBOARD 1
#define IRQ_CASCADE 2
#define IRQ_COM2 3
#define IRQ_COM1 4
#define IRQ_LPT2 5
#define IRQ_FLOPPY 6
#define IRQ_LPT1 7
#define IRQ_RTC 8
#define IRQ_FREE1 9
#define IRQ_FREE2 10
#define IRQ_FREE3 11
#define IRQ_MOUSE 12
#define IRQ_FPU 13
#define IRQ_PRIMARY_ATA 14
#define IRQ_SECONDARY_ATA 15

// Lisp object types
typedef enum {
    LISP_NIL,
    LISP_INTEGER,
    LISP_SYMBOL,
    LISP_CONS,
    LISP_STRING,
    LISP_FUNCTION,
    LISP_BUILTIN
} lisp_type_t;

// Lisp object structure
typedef struct lisp_object {
    lisp_type_t type;
    union {
        int integer;
        char* string;
        struct {
            struct lisp_object* car;
            struct lisp_object* cdr;
        } cons;
        struct {
            char* name;
            struct lisp_object* value;
        } symbol;
        struct {
            struct lisp_object* (*func)(struct lisp_object*);
        } builtin;
    } data;
    struct lisp_object* next; // For garbage collection
} lisp_object_t;

// Function prototypes

// Terminal functions
void terminal_clear(void);
void terminal_putchar(char c);
void terminal_write(const char* str);
void terminal_writeline(const char* str);
char* itoa(int value);

// Memory management functions
void init_physical_memory(memory_map_entry_t* memory_map, uint16_t num_entries);
void init_virtual_memory(void);
void init_kernel_heap(void);
void* kmalloc(size_t size);
void kfree(void* ptr);

// Interrupt handling functions
void init_idt(void);
void install_exception_handlers(void);
void install_irq_handlers(void);
void irq_handler(int irq);
void exception_handler(int exception);

// Lisp runtime functions
void init_lisp_heap(void);
void init_symbol_table(void);
void init_evaluator(void);
void load_core_functions(void);
void lisp_repl(void);

// Lisp object functions
lisp_object_t* lisp_alloc(lisp_type_t type);
void lisp_free(lisp_object_t* obj);
lisp_object_t* lisp_make_integer(int value);
lisp_object_t* lisp_make_symbol(const char* name);
lisp_object_t* lisp_make_string(const char* str);
lisp_object_t* lisp_make_cons(lisp_object_t* car, lisp_object_t* cdr);
lisp_object_t* lisp_eval(lisp_object_t* expr);
lisp_object_t* lisp_read(const char* input);
void lisp_print(lisp_object_t* obj);

// Utility macros
#ifndef NULL
#define NULL ((void*)0)
#endif
#define TRUE 1
#define FALSE 0

// Assembly helper functions
static inline void outb(uint16_t port, uint8_t value) {
    asm volatile("outb %0, %1" : : "a"(value), "Nd"(port));
}

static inline uint8_t inb(uint16_t port) {
    uint8_t value;
    asm volatile("inb %1, %0" : "=a"(value) : "Nd"(port));
    return value;
}

static inline void io_wait(void) {
    outb(0x80, 0);
}

// Interrupt disable/enable
static inline void cli(void) {
    asm volatile("cli");
}

static inline void sti(void) {
    asm volatile("sti");
}

// Memory barriers
static inline void memory_barrier(void) {
    asm volatile("" ::: "memory");
}

#endif // KERNEL_H


// Additional function prototypes for built-ins
lisp_object_t* builtin_plus(lisp_object_t* args);
lisp_object_t* builtin_minus(lisp_object_t* args);
lisp_object_t* builtin_multiply(lisp_object_t* args);
lisp_object_t* builtin_divide(lisp_object_t* args);
lisp_object_t* builtin_cons(lisp_object_t* args);
lisp_object_t* builtin_car(lisp_object_t* args);
lisp_object_t* builtin_cdr(lisp_object_t* args);
lisp_object_t* builtin_list(lisp_object_t* args);
lisp_object_t* builtin_quote(lisp_object_t* args);
lisp_object_t* builtin_help(lisp_object_t* args);
lisp_object_t* builtin_system(lisp_object_t* args);
lisp_object_t* builtin_print(lisp_object_t* args);
lisp_object_t* builtin_equal(lisp_object_t* args);
lisp_object_t* builtin_less_than(lisp_object_t* args);

// Keyboard buffer functions
void keyboard_buffer_add(char c);
char keyboard_buffer_get(void);
int keyboard_buffer_available(void);

// String functions
int strcmp(const char* s1, const char* s2);
int strlen(const char* str);


// File system functions
void init_filesystem(void);
int fs_create(const char* filename);
int fs_open(const char* filename);
int fs_write(int file_handle, const void* data, uint32_t size);
int fs_read(int file_handle, void* buffer, uint32_t size);
uint32_t fs_size(int file_handle);
void fs_close(int file_handle);
int fs_delete(const char* filename);
void fs_list(void);

// File system Lisp functions
lisp_object_t* builtin_file_create(lisp_object_t* args);
lisp_object_t* builtin_file_write(lisp_object_t* args);
lisp_object_t* builtin_file_list(lisp_object_t* args);

// Enhanced console functions
void console_set_cursor(int x, int y);
void console_get_cursor(int* x, int* y);
void console_set_color(uint8_t color);
void console_putchar_color(char c, uint8_t color);
void console_putchar(char c);
void console_write_color(const char* str, uint8_t color);
void console_write(const char* str);
void console_writeline_color(const char* str, uint8_t color);
void console_writeline(const char* str);
int console_readline(char* buffer, int max_length);
void console_clear(void);
void console_print_int(int value);
void console_print_hex(uint32_t value);
void enhanced_lisp_repl(void);

