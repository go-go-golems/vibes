; Interrupt Service Routines for Lisp Operating System
; Assembly stubs that call C interrupt handlers
; Author: Manus AI
; Date: July 13, 2025

bits 32

; External C functions
extern exception_handler
extern irq_handler

; Exception handlers (ISRs)
global isr0, isr1, isr2, isr3, isr4, isr5, isr6, isr7
global isr8, isr9, isr10, isr11, isr12, isr13, isr14, isr15
global isr16, isr17, isr18, isr19, isr20, isr21

; IRQ handlers
global irq0, irq1, irq2, irq3, irq4, irq5, irq6, irq7
global irq8, irq9, irq10, irq11, irq12, irq13, irq14, irq15

; Macro for ISRs without error code
%macro ISR_NOERRCODE 1
isr%1:
    cli
    push 0          ; Push dummy error code
    push %1         ; Push interrupt number
    jmp isr_common_stub
%endmacro

; Macro for ISRs with error code
%macro ISR_ERRCODE 1
isr%1:
    cli
    push %1         ; Push interrupt number
    jmp isr_common_stub
%endmacro

; Macro for IRQs
%macro IRQ 2
irq%1:
    cli
    push 0          ; Push dummy error code
    push %2         ; Push IRQ number
    jmp irq_common_stub
%endmacro

; Exception handlers
ISR_NOERRCODE 0     ; Division by zero
ISR_NOERRCODE 1     ; Debug
ISR_NOERRCODE 2     ; Non-maskable interrupt
ISR_NOERRCODE 3     ; Breakpoint
ISR_NOERRCODE 4     ; Overflow
ISR_NOERRCODE 5     ; Bound range exceeded
ISR_NOERRCODE 6     ; Invalid opcode
ISR_NOERRCODE 7     ; Device not available
ISR_ERRCODE   8     ; Double fault
ISR_NOERRCODE 9     ; Coprocessor segment overrun
ISR_ERRCODE   10    ; Invalid TSS
ISR_ERRCODE   11    ; Segment not present
ISR_ERRCODE   12    ; Stack-segment fault
ISR_ERRCODE   13    ; General protection fault
ISR_ERRCODE   14    ; Page fault
ISR_NOERRCODE 15    ; Reserved
ISR_NOERRCODE 16    ; x87 floating-point exception
ISR_ERRCODE   17    ; Alignment check
ISR_NOERRCODE 18    ; Machine check
ISR_NOERRCODE 19    ; SIMD floating-point exception
ISR_NOERRCODE 20    ; Virtualization exception
ISR_ERRCODE   21    ; Control protection exception

; IRQ handlers
IRQ 0, 0    ; Timer
IRQ 1, 1    ; Keyboard
IRQ 2, 2    ; Cascade
IRQ 3, 3    ; COM2
IRQ 4, 4    ; COM1
IRQ 5, 5    ; LPT2
IRQ 6, 6    ; Floppy
IRQ 7, 7    ; LPT1
IRQ 8, 8    ; RTC
IRQ 9, 9    ; Free
IRQ 10, 10  ; Free
IRQ 11, 11  ; Free
IRQ 12, 12  ; Mouse
IRQ 13, 13  ; FPU
IRQ 14, 14  ; Primary ATA
IRQ 15, 15  ; Secondary ATA

; Common ISR stub
isr_common_stub:
    pusha           ; Push all general-purpose registers
    
    mov ax, ds      ; Save data segment
    push eax
    
    mov ax, 0x10    ; Load kernel data segment
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    call exception_handler  ; Call C exception handler
    
    pop eax         ; Restore data segment
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    popa            ; Restore all general-purpose registers
    add esp, 8      ; Remove error code and interrupt number
    sti
    iret            ; Return from interrupt

; Common IRQ stub
irq_common_stub:
    pusha           ; Push all general-purpose registers
    
    mov ax, ds      ; Save data segment
    push eax
    
    mov ax, 0x10    ; Load kernel data segment
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    call irq_handler    ; Call C IRQ handler
    
    pop eax         ; Restore data segment
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    
    popa            ; Restore all general-purpose registers
    add esp, 8      ; Remove error code and IRQ number
    sti
    iret            ; Return from interrupt

