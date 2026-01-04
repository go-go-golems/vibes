; Interrupt handlers for the Lisp OS

[bits 32]
global keyboard_interrupt_handler
extern keyboard_handler

section .text

; Keyboard interrupt handler (IRQ1)
keyboard_interrupt_handler:
    pushad                  ; Save all registers
    
    call keyboard_handler   ; Call C handler
    
    popad                   ; Restore all registers
    iret                    ; Return from interrupt
