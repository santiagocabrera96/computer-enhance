; listing_0051_memory_mov disassembly:
bits 16
mov word [1000], 1
mov word [1002], 2
mov word [1004], 3
mov word [1006], 4
mov bx, 1000
mov word [bx + 4], 10
mov bx, [1000]
mov cx, [1002]
mov dx, [1004]
mov bp, [1006]
