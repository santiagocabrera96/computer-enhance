; listing_0053_add_loop_challenge disassembly:
bits 16
mov dx, 6
mov bp, 1000
mov si, 0
mov [bp + si], si
add si, 2
cmp si, dx
jne $-7
mov bx, 0
mov si, dx
sub bp, 2
add bx, [bp + si]
sub si, 2
jne $-5
