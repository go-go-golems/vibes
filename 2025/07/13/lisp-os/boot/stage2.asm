; Stage 2 Bootloader for Lisp Operating System
; This bootloader transitions to protected mode and loads the kernel
; Author: Manus AI
; Date: July 13, 2025

bits 16
org 0x1000

; Constants
KERNEL_LOAD_ADDR equ 0x8000     ; Where to load kernel (32KB)
KERNEL_SECTORS equ 50           ; Number of sectors for kernel (increased for larger kernel)
STACK_TOP equ 0x9000           ; Stack for protected mode

start:
    ; Display stage 2 message
    mov si, stage2_msg
    call print_string
    
    ; Detect memory using BIOS interrupt 0x15, function 0xe820
    call detect_memory
    
    ; Enable A20 line
    call enable_a20
    
    ; Load kernel from disk
    call load_kernel
    
    ; Enter protected mode
    call enter_protected_mode
    
    ; Should never reach here
    jmp halt

; Print string function (16-bit mode)
print_string:
    mov ah, 0x0e
.loop:
    lodsb
    test al, al
    jz .done
    int 0x10
    jmp .loop
.done:
    ret

; Detect memory using BIOS E820 function
detect_memory:
    mov si, memory_msg
    call print_string
    
    ; Memory map will be stored at 0x8000
    mov di, 0x8000
    xor ebx, ebx           ; EBX must be 0 to start
    mov edx, 0x534D4150    ; 'SMAP' signature
    
.loop:
    mov eax, 0xe820        ; E820 function
    mov ecx, 24            ; Size of buffer
    int 0x15               ; BIOS interrupt
    jc .error              ; If carry set, error or done
    
    ; Check if this is the last entry
    test ebx, ebx
    jz .done
    
    ; Move to next entry
    add di, 24
    jmp .loop
    
.done:
    ; Store number of entries at 0x7FFE
    mov word [0x7FFE], di
    sub word [0x7FFE], 0x8000
    shr word [0x7FFE], 4   ; Divide by 24 (size of each entry)
    ret
    
.error:
    mov si, memory_error_msg
    call print_string
    jmp halt

; Enable A20 line for access to memory above 1MB
enable_a20:
    mov si, a20_msg
    call print_string
    
    ; Try keyboard controller method
    call wait_8042
    mov al, 0xad           ; Disable keyboard
    out 0x64, al
    
    call wait_8042
    mov al, 0xd0           ; Read output port
    out 0x64, al
    
    call wait_8042_data
    in al, 0x60            ; Read output port data
    mov bl, al             ; Save it
    
    call wait_8042
    mov al, 0xd1           ; Write output port
    out 0x64, al
    
    call wait_8042
    mov al, bl
    or al, 2               ; Set A20 bit
    out 0x60, al
    
    call wait_8042
    mov al, 0xae           ; Enable keyboard
    out 0x64, al
    
    call wait_8042
    ret

wait_8042:
    in al, 0x64
    test al, 2
    jnz wait_8042
    ret

wait_8042_data:
    in al, 0x64
    test al, 1
    jz wait_8042_data
    ret

; Load kernel from disk
load_kernel:
    mov si, kernel_msg
    call print_string
    
    ; Reset disk
    mov ah, 0x00
    mov dl, 0x80
    int 0x13
    jc disk_error
    
    ; Load kernel - try loading fewer sectors to avoid BIOS issues
    mov ah, 0x02           ; Read sectors
    mov al, 32             ; Load 32 sectors (should be enough for kernel)
    mov ch, 0x00           ; Cylinder 0
    mov cl, 0x0a           ; Sector 10 (1-based)
    mov dh, 0x00           ; Head 0
    mov dl, 0x80           ; Drive 0x80
    mov bx, KERNEL_LOAD_ADDR
    int 0x13
    jc disk_error
    
    mov si, kernel_loaded_msg
    call print_string
    ret

disk_error:
    mov si, disk_error_msg
    call print_string
    ; Continue anyway for testing
    ret

; Enter protected mode
enter_protected_mode:
    cli                    ; Disable interrupts
    
    ; Load GDT
    lgdt [gdt_descriptor]
    
    ; Set protected mode bit in CR0
    mov eax, cr0
    or eax, 1
    mov cr0, eax
    
    ; Far jump to flush pipeline and enter protected mode
    jmp 0x08:protected_mode_start

[bits 32]
protected_mode_start:
    ; Set up segment registers for protected mode
    mov ax, 0x10           ; Data segment selector
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    
    ; Set up stack
    mov esp, 0x9000
    
    ; Clear screen and show success message
    mov edi, 0xB8000
    mov ecx, 80*25
    mov ax, 0x0720         ; Space with gray on black
    rep stosw
    
    ; Write success message
    mov esi, success_msg_32
    mov edi, 0xB8000
    mov ah, 0x0F           ; White on black
write_loop:
    lodsb
    test al, al
    jz kernel_jump
    stosb
    mov al, ah
    stosb
    jmp write_loop
    
kernel_jump:
    ; Jump to kernel
    jmp KERNEL_LOAD_ADDR
    
    ; If kernel returns, halt
    cli
    hlt
    jmp $

success_msg_32 db 'Protected mode active - Jumping to kernel...', 0

bits 16
halt:
    mov si, halt_msg
    call print_string
    cli
    hlt

; Global Descriptor Table
gdt_start:
    ; Null descriptor
    dd 0x0
    dd 0x0
    
    ; Code segment descriptor
    dw 0xffff              ; Limit (bits 0-15)
    dw 0x0000              ; Base (bits 0-15)
    db 0x00                ; Base (bits 16-23)
    db 10011010b           ; Access byte
    db 11001111b           ; Granularity byte
    db 0x00                ; Base (bits 24-31)
    
    ; Data segment descriptor
    dw 0xffff              ; Limit (bits 0-15)
    dw 0x0000              ; Base (bits 0-15)
    db 0x00                ; Base (bits 16-23)
    db 10010010b           ; Access byte
    db 11001111b           ; Granularity byte
    db 0x00                ; Base (bits 24-31)
gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1  ; Size of GDT
    dd gdt_start                ; Address of GDT

; String constants
stage2_msg db 'Lisp OS Stage 2 Bootloader', 0x0d, 0x0a, 0
memory_msg db 'Detecting memory...', 0x0d, 0x0a, 0
memory_error_msg db 'Memory detection failed!', 0x0d, 0x0a, 0
a20_msg db 'Enabling A20 line...', 0x0d, 0x0a, 0
kernel_msg db 'Loading kernel...', 0x0d, 0x0a, 0
kernel_loaded_msg db 'Kernel loaded successfully!', 0x0d, 0x0a, 0
disk_error_msg db 'Disk error loading kernel!', 0x0d, 0x0a, 0
protected_msg db 'Entering protected mode...', 0x0d, 0x0a, 0
halt_msg db 'System halted in stage 2.', 0x0d, 0x0a, 0

; Pad to fill the allocated sectors
times 4096-($-$$) db 0

