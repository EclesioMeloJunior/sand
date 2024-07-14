.data
msg: .ascii "Sum = "
.equ len, . - msg
    
.bss
buffer: .zero 16

.text
.global _start
.align 4

_start:
    mov x0, #5
    mov x1, #10
    add x2, x0, x1

    adrp x0, buffer@PAGE
    add  x0, x0, buffer@PAGEOFF
    mov  x1, x2
    bl   itoa

    mov  x0, #1
    adrp x1, msg@PAGE
    add  x1, x1, msg@PAGEOFF
    mov x2, #len
    mov  x16, #4
    svc  0

    mov  x0, #1
    adrp x1, buffer@PAGE
    add  x1, x1, buffer@PAGEOFF
    mov  x2, #16
    mov  x16, #4
    svc 0

    mov x0, #0
    mov x16, #1
    svc 0

// Convert integer to ASCII (itoa function)
itoa:
    mov x2, #10          // Base 10
    mov x3, #15          // Index to store result (buffer size - 1)
    mov w4, #0x30        // ASCII offset ('0')

itoa_loop:
    udiv x5, x1, x2      // Divide x1 by 10, x5 = x1 / 10
    msub x6, x5, x2, x1  // Remainder = x1 - (x5 * 10)
    add w6, w6, w4       // Convert remainder to ASCII ('0' + remainder)
    strb w6, [x0, x3]    // Store the ASCII character in buffer
    sub x3, x3, #1       // Move to the next position in the buffer
    mov x1, x5           // Update x1 to the quotient
    cbnz x1, itoa_loop   // If x1 is not zero, continue looping

    // Null-terminate the string
    strb wzr, [x0, x3]   // Null-terminate the string

    ret                  // Return from function
