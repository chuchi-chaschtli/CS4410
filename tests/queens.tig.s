L144:
lw 'd0, 4('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
beq t831, t832, L140
L141:
li 'd0, 0move 'd0, 's0
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1ble t708, t835, L138
L104:
li 'd0, 0move 'd0, 's0
j 'j0
L140:
la t839, L109
lw 'd0, 0('s0)move 'd0, 's0
jal t839
move t725, t103
j 'j0
L138:
lw 'd0, 0('s0)lw 'd0, ~8('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
lw 'd0, 0('s0)beqz t846, L125
L126:
li 'd0, 0move 'd0, 's0
j 'j0
L125:
li 'd0, 1move 'd0, 's0
lw 'd0, 0('s0)lw 'd0, ~16('s0)
lw 'd0, 4('s0)
add 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
lw 'd0, 0('s0)beqz t856, L128
L129:
li 'd0, 0move 'd0, 's0
L128:
j 'j0
L145:
li 'd0, 1beq t858, t711, L130
L131:
li 'd0, 0move 'd0, 's0
j 'j0
L130:
li 'd0, 1move 'd0, 's0
lw 'd0, 0('s0)lw 'd0, ~20('s0)
addi 'd0, 's0, 7lw 'd0, 4('s0)
sub 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
lw 'd0, 0('s0)beqz t869, L133
L134:
li 'd0, 0move 'd0, 's0
L133:
j 'j0
L146:
li 'd0, 1beq t871, t714, L135
L136:
li 'd0, 0move 'd0, 's0
j 'j0
L135:
lw 'd0, 0('s0)lw 'd0, ~20('s0)
addi 'd0, 's0, 7lw 'd0, 4('s0)
sub 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 0sw t881, (t722)
lw 'd0, 0('s0)lw 'd0, ~16('s0)
lw 'd0, 4('s0)
add 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 0sw t889, (t721)
lw 'd0, 0('s0)lw 'd0, ~8('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 0sw t895, (t720)
la t896, L123
lw 'd0, 0('s0)move 'd0, 's0
lw 'd0, 4('s0)
addi 'd0, 's0, 1move 'd0, 's0
jal t896
move t830, t103
lw 'd0, 0('s0)lw 'd0, ~12('s0)
lw 'd0, 4('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
sw t708, (t719)
lw 'd0, 0('s0)lw 'd0, ~20('s0)
addi 'd0, 's0, 7lw 'd0, 4('s0)
sub 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 1sw t914, (t718)
lw 'd0, 0('s0)lw 'd0, ~16('s0)
lw 'd0, 4('s0)
add 'd0, 's0, 's1li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 1sw t922, (t717)
lw 'd0, 0('s0)lw 'd0, ~8('s0)
li 'd0, 4mul 'd0, 's0, 's1add 'd0, 's0, 's1move 'd0, 's0
li 'd0, 1sw t928, (t716)
li 'd0, 0move 'd0, 's0
j 'j0
L147:
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1beq t708, t930, L104
L139:
addi 'd0, 's0, 1move 'd0, 's0
j 'j0
L148:
li 'd0, 0move 'd0, 's0
j 'j0
L143:
L150:
li 'd0, 0sw 's1, ~4('s0)
lw 'd0, ~4('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1ble t936, t937, L120
L104:
li 'd0, 0move 'd0, 's0
j 'j0
L120:
li 'd0, 0sw 's1, ~8('s0)
lw 'd0, ~8('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1ble t942, t943, L117
L110:
lw 'd0, ~4('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1beq t946, t947, L104
L121:
lw 'd0, ~4('s0)
addi 'd0, 's0, 1sw 's1, ~4('s0)
j 'j0
L117:
lw 'd0, ~8('s0)
lw 'd0, 0('s0)lw 'd0, ~4('s0)
addi 'd0, 's0, ~1beq t952, t953, L110
L118:
lw 'd0, ~8('s0)
addi 'd0, 's0, 1sw 's1, ~8('s0)
j 'j0
L149:


 . O
