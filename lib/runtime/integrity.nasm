section .data
    align_error db "x86istimb integrity error: stack was not 16-byte aligned at function exit (right before prologue)", 0

section .text
    global _x86istmb_Nruntime_Scheck_stack_alignment
    extern _puts
    extern _exit

_x86istmb_Nruntime_Scheck_stack_alignment:
    push rdi
    push rsi

    ; if stack was not 16 byte aligned, this was called, so the call instruction pushes on the stack
    ; therefore we WILL be 16 byte aligned
    mov rdi, rsp
    and rdi, 0xF
    jnz .stack_aligned

    ; otherwise fail
    sub rsp, 8 ; to actually make stack aligned
    lea rdi, [rel align_error]
    call _puts
    mov edi, 1
    call _exit

.stack_aligned:
    pop rsi
    pop rdi
    ret
