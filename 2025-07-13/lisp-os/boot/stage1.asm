; Stage 1 Bootloader for Lisp Operating System
; This bootloader fits in 512 bytes and loads the stage 2 bootloader
; Author: Manus AI
; Date: July 13, 2025

bits 16                     ; 16-bit real mode
org 0x7c00                  ; BIOS loads us at 0x7C00

; Constants
STAGE2_LOAD_ADDR equ 0x1000 ; Where to load stage 2
STAGE2_SECTORS equ 8        ; Number of sectors for stage 2
STACK_TOP equ 0x7c00        ; Stack grows down from bootloader

start:
    ; Initialize segment registers and stack
    cli                     ; Disable interrupts
    xor ax, ax             ; Clear AX
    mov ds, ax             ; Data segment = 0
    mov es, ax             ; Extra segment = 0
    mov ss, ax             ; Stack segment = 0
    mov sp, STACK_TOP      ; Set stack pointer
    sti                    ; Re-enable interrupts
    
    ; Clear the screen
    mov ah, 0x00           ; Set video mode
    mov al, 0x03           ; 80x25 color text mode
    int 0x10               ; BIOS video interrupt
    
    ; Display boot message
    mov si, boot_msg
    call print_string
    
    ; Reset disk system
    mov ah, 0x00           ; Reset disk function
    mov dl, 0x80           ; First hard drive
    int 0x13               ; BIOS disk interrupt
    jc disk_error          ; Jump if carry flag set (error)
    
    ; Load stage 2 bootloader
    mov si, loading_msg
    call print_string
    
    mov ah, 0x02           ; Read sectors function
    mov al, STAGE2_SECTORS ; Number of sectors to read
    mov ch, 0x00           ; Cylinder 0
    mov cl, 0x02           ; Sector 2 (sector 1 is this bootloader)
    mov dh, 0x00           ; Head 0
    mov dl, 0x80           ; First hard drive
    mov bx, STAGE2_LOAD_ADDR ; Where to load stage 2
    int 0x13               ; BIOS disk interrupt
    jc disk_error          ; Jump if error
    
    ; Verify we loaded the correct number of sectors
    cmp al, STAGE2_SECTORS
    jne disk_error
    
    ; Display success message
    mov si, success_msg
    call print_string
    
    ; Jump to stage 2 bootloader
    jmp STAGE2_LOAD_ADDR

; Print string function
; Input: SI = pointer to null-terminated string
print_string:
    mov ah, 0x0e           ; Teletype output function
.loop:
    lodsb                  ; Load byte from SI into AL, increment SI
    test al, al            ; Check if AL is zero
    jz .done               ; If zero, we're done
    int 0x10               ; BIOS video interrupt
    jmp .loop              ; Continue loop
.done:
    ret

; Error handling
disk_error:
    mov si, error_msg
    call print_string
    jmp halt

halt:
    mov si, halt_msg
    call print_string
    cli                    ; Disable interrupts
    hlt                    ; Halt the processor

; String constants
boot_msg db 'Lisp OS Stage 1 Bootloader', 0x0d, 0x0a, 0
loading_msg db 'Loading Stage 2...', 0x0d, 0x0a, 0
success_msg db 'Stage 2 loaded successfully!', 0x0d, 0x0a, 0
error_msg db 'Disk read error!', 0x0d, 0x0a, 0
halt_msg db 'System halted.', 0x0d, 0x0a, 0

; Pad to 510 bytes and add boot signature
times 510-($-$$) db 0      ; Fill remaining space with zeros
dw 0xaa55                  ; Boot signature (little endian)

