; Kernel entry point assembly stub
; This provides the proper entry point for the kernel

[bits 32]
global _start
extern kernel_main

section .text
_start:
    ; Set up stack
    mov esp, 0x9000
    
    ; Call the C kernel main function
    call kernel_main
    
    ; If kernel_main returns, halt
    cli
    hlt
    jmp $
