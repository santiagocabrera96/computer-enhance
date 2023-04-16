; listing_0055_challenge_rectangle disassembly:
bits 16
mov bp, 256
mov dx, 64
mov cx, 64
mov [bp + 0], cl
mov byte [bp + 1], 0
mov [bp + 2], dl
mov byte [bp + 3], 255
add bp, 4
loop $-17
sub dx, 1
jne $-25
mov bp, 516
mov bx, bp
mov cx, 62
mov byte [bp + 1], 255
mov byte [bp + 15617], 255
mov byte [bx + 1], 255
mov byte [bx + 245], 255
add bp, 4
add bx, 256
loop $-25
