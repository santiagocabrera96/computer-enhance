; listing_0050_challenge_jumps disassembly:
bits 16
mov ax, 10
mov bx, 10
mov cx, 10
cmp bx, cx
je $+7
add ax, 1
jp $+7
sub bx, 5
jb $+5
sub cx, 2
loopnz $-17
