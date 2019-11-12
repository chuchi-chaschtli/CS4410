L18:
lw 'd0, 4('s0)
li 'd0, 1ble t141, t142, L14
L15:
lw 'd0, 4('s0)
move 'd0, 's0
la t144, L13
lw 'd0, 0('s0)move 'd0, 's0
lw 'd0, 4('s0)
addi 'd0, 's0, ~1move 'd0, 's0
jal t144
move t139, t103
mul 'd0, 's0, 's1move 'd0, 's0
j 'j0
L14:
li 'd0, 1move 'd0, 's0
j 'j0
L19:
li 'd0, 0move 'd0, 's0
j 'j0
L17:
