/*
 * Lisp Operating System Interrupt Handling
 * IDT setup and interrupt/exception handlers
 * Author: Manus AI
 * Date: July 13, 2025
 */

#include "kernel.h"

// IDT entry structure
typedef struct {
    uint16_t offset_low;
    uint16_t selector;
    uint8_t zero;
    uint8_t type_attr;
    uint16_t offset_high;
} __attribute__((packed)) idt_entry_t;

// IDT descriptor
typedef struct {
    uint16_t limit;
    uint32_t base;
} __attribute__((packed)) idt_descriptor_t;

// IDT table and descriptor
static idt_entry_t idt[256];
static idt_descriptor_t idt_desc;

// Exception names for debugging
static const char* exception_names[] = {
    "Division by Zero",
    "Debug",
    "Non-Maskable Interrupt",
    "Breakpoint",
    "Overflow",
    "Bound Range Exceeded",
    "Invalid Opcode",
    "Device Not Available",
    "Double Fault",
    "Coprocessor Segment Overrun",
    "Invalid TSS",
    "Segment Not Present",
    "Stack-Segment Fault",
    "General Protection Fault",
    "Page Fault",
    "Reserved",
    "x87 Floating-Point Exception",
    "Alignment Check",
    "Machine Check",
    "SIMD Floating-Point Exception",
    "Virtualization Exception",
    "Control Protection Exception"
};

// Forward declarations for assembly interrupt stubs
extern void isr0(void);
extern void isr1(void);
extern void isr2(void);
extern void isr3(void);
extern void isr4(void);
extern void isr5(void);
extern void isr6(void);
extern void isr7(void);
extern void isr8(void);
extern void isr9(void);
extern void isr10(void);
extern void isr11(void);
extern void isr12(void);
extern void isr13(void);
extern void isr14(void);
extern void isr15(void);
extern void isr16(void);
extern void isr17(void);
extern void isr18(void);
extern void isr19(void);
extern void isr20(void);
extern void isr21(void);

extern void irq0(void);
extern void irq1(void);
extern void irq2(void);
extern void irq3(void);
extern void irq4(void);
extern void irq5(void);
extern void irq6(void);
extern void irq7(void);
extern void irq8(void);
extern void irq9(void);
extern void irq10(void);
extern void irq11(void);
extern void irq12(void);
extern void irq13(void);
extern void irq14(void);
extern void irq15(void);

/*
 * Set an IDT entry
 */
static void idt_set_gate(int num, uint32_t handler, uint16_t selector, uint8_t flags)
{
    idt[num].offset_low = handler & 0xFFFF;
    idt[num].offset_high = (handler >> 16) & 0xFFFF;
    idt[num].selector = selector;
    idt[num].zero = 0;
    idt[num].type_attr = flags;
}

/*
 * Initialize the IDT
 */
void init_idt(void)
{
    terminal_writeline("Setting up Interrupt Descriptor Table...");
    
    // Set up IDT descriptor
    idt_desc.limit = sizeof(idt) - 1;
    idt_desc.base = (uint32_t)&idt;
    
    // Clear IDT
    for (int i = 0; i < 256; i++) {
        idt_set_gate(i, 0, 0, 0);
    }
    
    // Load IDT
    asm volatile("lidt %0" : : "m"(idt_desc));
    
    terminal_writeline("IDT initialized.");
}

/*
 * Install exception handlers
 */
void install_exception_handlers(void)
{
    terminal_writeline("Installing exception handlers...");
    
    // Install ISRs for CPU exceptions
    idt_set_gate(0, (uint32_t)isr0, 0x08, 0x8E);
    idt_set_gate(1, (uint32_t)isr1, 0x08, 0x8E);
    idt_set_gate(2, (uint32_t)isr2, 0x08, 0x8E);
    idt_set_gate(3, (uint32_t)isr3, 0x08, 0x8E);
    idt_set_gate(4, (uint32_t)isr4, 0x08, 0x8E);
    idt_set_gate(5, (uint32_t)isr5, 0x08, 0x8E);
    idt_set_gate(6, (uint32_t)isr6, 0x08, 0x8E);
    idt_set_gate(7, (uint32_t)isr7, 0x08, 0x8E);
    idt_set_gate(8, (uint32_t)isr8, 0x08, 0x8E);
    idt_set_gate(9, (uint32_t)isr9, 0x08, 0x8E);
    idt_set_gate(10, (uint32_t)isr10, 0x08, 0x8E);
    idt_set_gate(11, (uint32_t)isr11, 0x08, 0x8E);
    idt_set_gate(12, (uint32_t)isr12, 0x08, 0x8E);
    idt_set_gate(13, (uint32_t)isr13, 0x08, 0x8E);
    idt_set_gate(14, (uint32_t)isr14, 0x08, 0x8E);
    idt_set_gate(15, (uint32_t)isr15, 0x08, 0x8E);
    idt_set_gate(16, (uint32_t)isr16, 0x08, 0x8E);
    idt_set_gate(17, (uint32_t)isr17, 0x08, 0x8E);
    idt_set_gate(18, (uint32_t)isr18, 0x08, 0x8E);
    idt_set_gate(19, (uint32_t)isr19, 0x08, 0x8E);
    idt_set_gate(20, (uint32_t)isr20, 0x08, 0x8E);
    idt_set_gate(21, (uint32_t)isr21, 0x08, 0x8E);
    
    terminal_writeline("Exception handlers installed.");
}

/*
 * Install IRQ handlers
 */
void install_irq_handlers(void)
{
    terminal_writeline("Installing IRQ handlers...");
    
    // Remap PIC
    outb(0x20, 0x11); // Initialize PIC1
    outb(0xA0, 0x11); // Initialize PIC2
    outb(0x21, 0x20); // PIC1 offset (IRQ 0-7 -> INT 32-39)
    outb(0xA1, 0x28); // PIC2 offset (IRQ 8-15 -> INT 40-47)
    outb(0x21, 0x04); // PIC1 cascade
    outb(0xA1, 0x02); // PIC2 cascade
    outb(0x21, 0x01); // 8086 mode
    outb(0xA1, 0x01); // 8086 mode
    outb(0x21, 0x00); // Enable all IRQs on PIC1
    outb(0xA1, 0x00); // Enable all IRQs on PIC2
    
    // Install IRQ handlers
    idt_set_gate(32, (uint32_t)irq0, 0x08, 0x8E);
    idt_set_gate(33, (uint32_t)irq1, 0x08, 0x8E);
    idt_set_gate(34, (uint32_t)irq2, 0x08, 0x8E);
    idt_set_gate(35, (uint32_t)irq3, 0x08, 0x8E);
    idt_set_gate(36, (uint32_t)irq4, 0x08, 0x8E);
    idt_set_gate(37, (uint32_t)irq5, 0x08, 0x8E);
    idt_set_gate(38, (uint32_t)irq6, 0x08, 0x8E);
    idt_set_gate(39, (uint32_t)irq7, 0x08, 0x8E);
    idt_set_gate(40, (uint32_t)irq8, 0x08, 0x8E);
    idt_set_gate(41, (uint32_t)irq9, 0x08, 0x8E);
    idt_set_gate(42, (uint32_t)irq10, 0x08, 0x8E);
    idt_set_gate(43, (uint32_t)irq11, 0x08, 0x8E);
    idt_set_gate(44, (uint32_t)irq12, 0x08, 0x8E);
    idt_set_gate(45, (uint32_t)irq13, 0x08, 0x8E);
    idt_set_gate(46, (uint32_t)irq14, 0x08, 0x8E);
    idt_set_gate(47, (uint32_t)irq15, 0x08, 0x8E);
    
    terminal_writeline("IRQ handlers installed.");
}

/*
 * Exception handler called from assembly stubs
 */
void exception_handler(int exception)
{
    terminal_writeline("");
    terminal_writeline("*** EXCEPTION ***");
    terminal_write("Exception: ");
    terminal_write(itoa(exception));
    terminal_write(" (");
    if (exception < 22) {
        terminal_write(exception_names[exception]);
    } else {
        terminal_write("Unknown");
    }
    terminal_writeline(")");
    terminal_writeline("System halted.");
    
    // Halt the system
    cli();
    while (1) {
        asm volatile("hlt");
    }
}

/*
 * IRQ handler called from assembly stubs
 */
void irq_handler(int irq)
{
    // Handle specific IRQs
    switch (irq) {
        case IRQ_TIMER:
            // Timer interrupt - could be used for scheduling
            break;
            
        case IRQ_KEYBOARD:
            // Keyboard interrupt
            handle_keyboard_interrupt();
            break;
            
        default:
            // Unhandled IRQ
            break;
    }
    
    // Send EOI to PIC
    if (irq >= 8) {
        outb(0xA0, 0x20); // EOI to PIC2
    }
    outb(0x20, 0x20); // EOI to PIC1
}

/*
 * Handle keyboard interrupt
 */
void handle_keyboard_interrupt(void)
{
    uint8_t scancode = inb(0x60);
    
    // Simple scancode to ASCII conversion for basic keys
    static const char scancode_to_ascii[] = {
        0, 0, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',
        '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
        0, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',
        0, '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 0,
        '*', 0, ' '
    };
    
    if (scancode < sizeof(scancode_to_ascii) && scancode_to_ascii[scancode]) {
        char c = scancode_to_ascii[scancode];
        // Add character to input buffer (to be implemented)
        keyboard_buffer_add(c);
    }
}


// Keyboard buffer
#define KEYBOARD_BUFFER_SIZE 256
static char keyboard_buffer[KEYBOARD_BUFFER_SIZE];
static int keyboard_buffer_head = 0;
static int keyboard_buffer_tail = 0;

/*
 * Add character to keyboard buffer
 */
void keyboard_buffer_add(char c)
{
    int next_head = (keyboard_buffer_head + 1) % KEYBOARD_BUFFER_SIZE;
    if (next_head != keyboard_buffer_tail) {
        keyboard_buffer[keyboard_buffer_head] = c;
        keyboard_buffer_head = next_head;
    }
}

/*
 * Get character from keyboard buffer
 */
char keyboard_buffer_get(void)
{
    if (keyboard_buffer_head == keyboard_buffer_tail) {
        return 0; // Buffer empty
    }
    
    char c = keyboard_buffer[keyboard_buffer_tail];
    keyboard_buffer_tail = (keyboard_buffer_tail + 1) % KEYBOARD_BUFFER_SIZE;
    return c;
}

/*
 * Check if keyboard buffer has data
 */
int keyboard_buffer_available(void)
{
    return keyboard_buffer_head != keyboard_buffer_tail;
}

