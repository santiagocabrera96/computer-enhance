; listing_0054_draw_rectangle disassembly:
bits 16
mov bp, 256
mov dx, 0
mov cx, 0
mov [bp + 0], cx
mov [bp + 2], dx
mov byte [bp + 3], 255
add bp, 4
add cx, 1
cmp cx, 64
jne $-19
add dx, 1
cmp dx, 64
jne $-30
