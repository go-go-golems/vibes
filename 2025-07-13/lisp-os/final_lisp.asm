; Final Working Lisp OS
[bits 16]
[org 0x7c00]

start:
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00
    
    ; Clear screen
    mov ax, 0x0003
    int 0x10
    
    ; Display banner
    mov si, banner
    call print
    
main:
    mov si, prompt
    call print
    call input
    call eval
    jmp main

print:
    mov ah, 0x0e
.l: lodsb
    test al, al
    jz .d
    int 0x10
    jmp .l
.d: ret

input:
    mov di, buf
    xor cx, cx
.r: mov ah, 0
    int 0x16
    cmp al, 13
    je .d
    cmp al, 8
    je .b
    cmp al, 32
    jb .r
    cmp cx, 30
    jae .r
    mov ah, 0x0e
    int 0x10
    stosb
    inc cx
    jmp .r
.b: test cx, cx
    jz .r
    mov ah, 0x0e
    mov al, 8
    int 0x10
    mov al, 32
    int 0x10
    mov al, 8
    int 0x10
    dec di
    dec cx
    jmp .r
.d: mov al, 0
    stosb
    mov ah, 0x0e
    mov al, 13
    int 0x10
    mov al, 10
    int 0x10
    ret

eval:
    mov si, buf
    cmp byte [si], '('
    jne .cmd
    inc si
    lodsb
    cmp al, '+'
    je .add
    cmp al, '*'
    je .mul
    mov si, err
    call print
    ret
.cmd:
    mov di, help
    call cmp
    jz .help
    mov di, hi
    call cmp
    jz .hello
    mov si, err
    call print
    ret
.help:
    mov si, hlp
    call print
    ret
.hello:
    mov si, hlo
    call print
    ret
.add:
    call num
    mov bx, ax
    call num
    add ax, bx
    call pnum
    ret
.mul:
    call num
    mov bx, ax
    call num
    mul bx
    call pnum
    ret

cmp:
    push si
.l: lodsb
    cmp al, [di]
    jne .n
    inc di
    test al, al
    jnz .l
    pop si
    xor ax, ax
    ret
.n: pop si
    mov ax, 1
    ret

num:
    xor ax, ax
    xor dx, dx
.s: lodsb
    cmp al, 32
    je .s
    cmp al, '0'
    jb .d
    cmp al, '9'
    ja .d
    sub al, '0'
    mov dl, al
    mov bx, 10
    mul bx
    add ax, dx
    jmp .s
.d: dec si
    ret

pnum:
    mov di, nbuf + 4
    mov byte [di], 0
    dec di
    mov bx, 10
.l: xor dx, dx
    div bx
    add dl, '0'
    mov [di], dl
    dec di
    test ax, ax
    jnz .l
    inc di
    mov si, di
    call print
    mov ah, 0x0e
    mov al, 13
    int 0x10
    mov al, 10
    int 0x10
    ret

banner db 'Lisp OS v1.0', 13, 10, 'REPL Ready!', 13, 10, 0
prompt db 'lisp> ', 0
help db 'help', 0
hi db 'hello', 0
hlp db '(+ a b) (* a b) hello help', 13, 10, 0
hlo db 'Hello from Lisp OS!', 13, 10, 0
err db 'Error', 13, 10, 0

buf times 32 db 0
nbuf times 6 db 0

times 510-($-$$) db 0
dw 0xaa55
