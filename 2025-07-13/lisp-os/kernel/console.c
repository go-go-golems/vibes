/*
 * Lisp Operating System - Console I/O
 * Enhanced console input/output handling
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "kernel.h"

// Console state
static int console_x = 0;
static int console_y = 0;
static uint8_t console_color = 0x07; // Light gray on black

// Input line buffer
#define INPUT_LINE_SIZE 256
static char input_line[INPUT_LINE_SIZE];
static int input_pos = 0;

/*
 * Set console cursor position
 */
void console_set_cursor(int x, int y)
{
    console_x = x;
    console_y = y;
    
    // Update hardware cursor
    uint16_t pos = y * 80 + x;
    outb(0x3D4, 0x0F);
    outb(0x3D5, (uint8_t)(pos & 0xFF));
    outb(0x3D4, 0x0E);
    outb(0x3D5, (uint8_t)((pos >> 8) & 0xFF));
}

/*
 * Get console cursor position
 */
void console_get_cursor(int* x, int* y)
{
    *x = console_x;
    *y = console_y;
}

/*
 * Set console color
 */
void console_set_color(uint8_t color)
{
    console_color = color;
}

/*
 * Enhanced putchar with color support
 */
void console_putchar_color(char c, uint8_t color)
{
    char* video_memory = (char*)0xB8000;
    
    if (c == '\n') {
        console_x = 0;
        console_y++;
    } else if (c == '\r') {
        console_x = 0;
    } else if (c == '\b') {
        if (console_x > 0) {
            console_x--;
            int offset = (console_y * 80 + console_x) * 2;
            video_memory[offset] = ' ';
            video_memory[offset + 1] = color;
        }
    } else if (c == '\t') {
        console_x = (console_x + 8) & ~7; // Tab to next 8-character boundary
    } else {
        int offset = (console_y * 80 + console_x) * 2;
        video_memory[offset] = c;
        video_memory[offset + 1] = color;
        console_x++;
    }
    
    // Handle line wrapping
    if (console_x >= 80) {
        console_x = 0;
        console_y++;
    }
    
    // Handle scrolling
    if (console_y >= 25) {
        // Scroll up one line
        for (int i = 0; i < 24 * 80 * 2; i++) {
            video_memory[i] = video_memory[i + 80 * 2];
        }
        // Clear last line
        for (int i = 24 * 80 * 2; i < 25 * 80 * 2; i += 2) {
            video_memory[i] = ' ';
            video_memory[i + 1] = color;
        }
        console_y = 24;
    }
    
    // Update hardware cursor
    console_set_cursor(console_x, console_y);
}

/*
 * Enhanced putchar using current color
 */
void console_putchar(char c)
{
    console_putchar_color(c, console_color);
}

/*
 * Write string with color
 */
void console_write_color(const char* str, uint8_t color)
{
    while (*str) {
        console_putchar_color(*str, color);
        str++;
    }
}

/*
 * Write string with current color
 */
void console_write(const char* str)
{
    console_write_color(str, console_color);
}

/*
 * Write line with color
 */
void console_writeline_color(const char* str, uint8_t color)
{
    console_write_color(str, color);
    console_putchar_color('\n', color);
}

/*
 * Write line with current color
 */
void console_writeline(const char* str)
{
    console_writeline_color(str, console_color);
}

/*
 * Read a line of input from keyboard
 */
int console_readline(char* buffer, int max_length)
{
    input_pos = 0;
    
    while (input_pos < max_length - 1) {
        // Wait for keyboard input
        while (!keyboard_buffer_available()) {
            asm volatile("hlt"); // Wait for interrupt
        }
        
        char c = keyboard_buffer_get();
        
        if (c == '\n' || c == '\r') {
            // End of line
            console_putchar('\n');
            buffer[input_pos] = '\0';
            return input_pos;
        } else if (c == '\b') {
            // Backspace
            if (input_pos > 0) {
                input_pos--;
                console_putchar('\b');
            }
        } else if (c >= 32 && c <= 126) {
            // Printable character
            buffer[input_pos] = c;
            input_pos++;
            console_putchar(c);
        }
    }
    
    buffer[max_length - 1] = '\0';
    return max_length - 1;
}

/*
 * Clear console screen
 */
void console_clear(void)
{
    char* video_memory = (char*)0xB8000;
    
    for (int i = 0; i < 80 * 25 * 2; i += 2) {
        video_memory[i] = ' ';
        video_memory[i + 1] = console_color;
    }
    
    console_set_cursor(0, 0);
}

/*
 * Print formatted integer
 */
void console_print_int(int value)
{
    console_write(itoa(value));
}

/*
 * Print formatted hex value
 */
void console_print_hex(uint32_t value)
{
    console_write("0x");
    
    char hex_chars[] = "0123456789ABCDEF";
    char buffer[9];
    buffer[8] = '\0';
    
    for (int i = 7; i >= 0; i--) {
        buffer[i] = hex_chars[value & 0xF];
        value >>= 4;
    }
    
    console_write(buffer);
}

/*
 * Enhanced REPL with proper input handling
 */
void enhanced_lisp_repl(void)
{
    char input_buffer[INPUT_LINE_SIZE];
    
    console_writeline("Enhanced Lisp REPL started.");
    console_writeline("Type expressions to evaluate, or (help) for assistance.");
    console_writeline("");
    
    while (1) {
        console_write("lisp> ");
        
        int length = console_readline(input_buffer, INPUT_LINE_SIZE);
        
        if (length > 0) {
            // Parse and evaluate the input
            lisp_object_t* expr = lisp_read(input_buffer);
            if (expr) {
                lisp_object_t* result = lisp_eval(expr);
                if (result) {
                    lisp_print(result);
                    console_writeline("");
                } else {
                    console_writeline("Error: Evaluation failed");
                }
            } else {
                console_writeline("Error: Parse failed");
            }
        }
        
        console_writeline("");
    }
}

