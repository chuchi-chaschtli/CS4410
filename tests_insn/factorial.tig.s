L18:
la t145, L13
li 'd0, 8
move 'd0, 's0
jal t145
move t144, t103
li 'd0, 0
move 'd0, 's0
j 'j0
L17:
L20:
lw 'd0, 4('s0)
li 'd0, 1
ble t150, t151, L14
L15:
lw 'd0, 4('s0)
move 'd0, 's0
la t153, L13
lw 'd0, 0('s0)
move 'd0, 's0
lw 'd0, 4('s0)
addi 'd0, 's0, ~1
move 'd0, 's0
jal t153
move t148, t103
mul 'd0, 's0, 's1
move 'd0, 's0
j 'j0
L14:
li 'd0, 1
move 'd0, 's0
j 'j0
L21:
li 'd0, 0
move 'd0, 's0
j 'j0
L19:
