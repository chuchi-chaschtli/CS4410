L150:
lw 'd0, 4('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
beq t605, t606, L146
L147:
li 'd0, 0move 'd0, 's0
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1ble t481, t609, L144
L110:
li 'd0, 0move 'd0, 's0
j 'j0
L146:
la t613, L115
lw 'd0, 0('s0)move 'd0, 's0
jal t613
move t498, t103
j 'j0
L144:
lw 'd0, 0('s0)lw 'd0, ~8('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
lw 'd0, 0('s0)beqz t620, L131
L132:
li 'd0, 0move 'd0, 's0
j 'j0
L131:
li 'd0, 1move 'd0, 's0
lw 'd0, 0('s0)lw 'd0, ~16('s0)
lw 'd0, 4('s0)
add 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
lw 'd0, 0('s0)beqz t630, L134
L135:
li 'd0, 0move 'd0, 's0
L134:
j 'j0
L151:
li 'd0, 1beq t632, t484, L136
L137:
li 'd0, 0move 'd0, 's0
j 'j0
L136:
li 'd0, 1move 'd0, 's0
lw 'd0, 0('s0)lw 'd0, ~20('s0)
addi 'd0, 's0, 7lw 'd0, 4('s0)
sub 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
lw 'd0, 0('s0)beqz t643, L139
L140:
li 'd0, 0move 'd0, 's0
L139:
j 'j0
L152:
li 'd0, 1beq t645, t487, L141
L142:
li 'd0, 0move 'd0, 's0
j 'j0
L141:
lw 'd0, 0('s0)lw 'd0, ~20('s0)
addi 'd0, 's0, 7lw 'd0, 4('s0)
sub 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 0sw t655, (t495)
lw 'd0, 0('s0)lw 'd0, ~16('s0)
lw 'd0, 4('s0)
add 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 0sw t663, (t494)
lw 'd0, 0('s0)lw 'd0, ~8('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 0sw t669, (t493)
la t670, L129
lw 'd0, 0('s0)move 'd0, 's0
lw 'd0, 4('s0)
addi 'd0, 's0, 1move 'd0, 's0
jal t670
move t604, t103
lw 'd0, 0('s0)lw 'd0, ~12('s0)
lw 'd0, 4('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
sw t481, (t492)
lw 'd0, 0('s0)lw 'd0, ~20('s0)
addi 'd0, 's0, 7lw 'd0, 4('s0)
sub 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 1sw t688, (t491)
lw 'd0, 0('s0)lw 'd0, ~16('s0)
lw 'd0, 4('s0)
add 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 1sw t696, (t490)
lw 'd0, 0('s0)lw 'd0, ~8('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 1sw t702, (t489)
li 'd0, 0move 'd0, 's0
j 'j0
L153:
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1beq t481, t704, L110
L145:
addi 'd0, 's0, 1move 'd0, 's0
j 'j0
L154:
li 'd0, 0move 'd0, 's0
j 'j0
L149:
L156:
li 'd0, 0sw 's1, ~4('s0)
lw 'd0, ~4('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1ble t710, t711, L126
L110:
li 'd0, 0move 'd0, 's0
j 'j0
L126:
li 'd0, 0sw 's1, ~8('s0)
lw 'd0, ~8('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1ble t716, t717, L123
L116:
lw 'd0, ~4('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1beq t720, t721, L110
L127:
lw 'd0, ~4('s0)
addi 'd0, 's0, 1sw 's1, ~4('s0)
j 'j0
L123:
lw 'd0, ~8('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1beq t726, t727, L116
L124:
lw 'd0, ~8('s0)
addi 'd0, 's0, 1sw 's1, ~8('s0)
j 'j0
L155:


 . O