	.globl main
	.data

	.text
main:
    sw     $fp   0($sp)
    move   $fp     $sp
    addi   $sp     $sp     -16
L17:
sw $a0, 4($fp)
move $a0, $ra
move $a0, $s0
move $a0, $s1
move $a0, $s2
move $a0, $s3
move $a0, $s4
move $a0, $s5
move $a0, $s6
move $a0, $s7
move $a0, $t0
move $a0, $t1
move $a0, $t2
move $a0, $t3
move $a0, $t4
move $a0, $t5
move $a0, $t6
move $a0, $t7
move $a0, $t8
move $s0, $t9
la $a0, L10
move $a0, $fp
li $a0, 8
move $a1, $a0
jalr $a0
move $t9, $s0
move $t8, $a0
move $t7, $a0
move $t6, $a0
move $t5, $a0
move $t4, $a0
move $t3, $a0
move $t2, $a0
move $t1, $a0
move $t0, $a0
move $v0, $v0
move $s7, $a0
move $s6, $a0
move $s5, $a0
move $s4, $a0
move $s3, $a0
move $s2, $a0
move $s1, $a0
move $s0, $a0
move $ra, $a0
j L16
L16:
    move   $sp     $fp
    lw     $fp   0($sp)
    jr     $ra

L10:
    sw     $fp   0($sp)
    move   $fp     $sp
    addi   $sp     $sp     -16
L19:
sw $a0, 4($fp)
sw $a1, 8($fp)
move $a0, $ra
move $a0, $s0
move $a0, $s1
move $a0, $s2
move $a0, $s3
move $a0, $s4
move $a0, $s5
move $a0, $s6
move $a0, $s7
li $a0, 1
move $a0, $a0
lw $a0, 4($fp)
li $a1, 1
ble $a0, $a1, L14
L15:
li $a0, 0
move $a0, $a0
L14:
li $a1, 1
beq $a0, $a1, L11
L12:
lw $a0, 4($fp)
move $a0, $a0
move $a0, $t0
move $a0, $t1
move $a0, $t2
move $a0, $t3
move $a0, $t4
move $a0, $t5
move $a0, $t6
move $a0, $t7
move $a0, $t8
move $s0, $t9
la $a0, L10
lw $a0, 0($fp)
move $a0, $a0
lw $a0, 4($fp)
addi $a0, $a0, -1
move $a1, $a0
jalr $a0
move $t9, $s0
move $t8, $a0
move $t7, $a0
move $t6, $a0
move $t5, $a0
move $t4, $a0
move $t3, $a0
move $t2, $a0
move $t1, $a0
move $t0, $a0
move $a1, $v0
mul $a0, $a0, $a1
move $a0, $a0
L13:
move $v0, $a0
move $s7, $a0
move $s6, $a0
move $s5, $a0
move $s4, $a0
move $s3, $a0
move $s2, $a0
move $s1, $a0
move $s0, $a0
move $ra, $a0
j L18
L11:
li $a0, 1
move $a0, $a0
j L13
L18:
    move   $sp     $fp
    lw     $fp   0($sp)
    jr     $ra

.text
initArray:
	li $a2, 4
	mul $a0, $a0, $a2
	li $v0, 9
	syscall
	move $v1, $v0
	add $a0, $a0, $v0
	_initArray_0:
	sw $a1, ($v1)
	add $v1, $v1, 4
	bne $v1, $a0, _initArray_0
	jr $ra

allocRecord:
  li $a2, 4
  mul $a0, $a0, $a2
  li $v0, 9
  syscall
  jr $ra

printi:
    li $v0, 1
    syscall
    jr $ra

print:
    li $v0, 4
    syscall
    jr $ra

flush:
    jr $ra

strcmp:
    strcmptest:
    lb $a2 ($a0)
    lb $a3 ($a1)
    beq $a2, $zero, strcmpend
    beq $a3, $zero, strcmpend
    bgt $a2, $a3  strcmpgreat
    blt $a2, $a3  strcmpless
    add $a0, $a0, 1
    add $a1, $a1, 1
    j strcmptest
    strcmpgreat:
    li $v0, 1
    jr $ra
    strcmpless:
    li $v0, -1
    jr $ra
    strcmpend:
    bne $a2 $zero strcmpgreat
    bne $a3 $zero strcmpless
    li $v0, 0
    jr $ra

size:
    move $v0, $zero
    sizeloop:
    lb $a1 ($a0)
    beq $a1, $zero sizeexit
    add $v0, $v0, 1
    add $a0, $a0, 1
    j sizeloop
    sizeexit:
    jr $ra

ord:
    lb $a1,($a0)
    li $v0,-1
    beqz $a1,Lrunt5
    lb $v0,($a0)
    Lrunt5:
    jr $ra

getchar:
    li $v0, 9
    li $a0, 2
    syscall
    move $a0, $v0
    li $a1, 2
    li $v0, 8
    syscall
    move $v0, $a0
    jr $ra

chr:
    move $a1, $a0
    li $v0, 9
    li $a0, 2
    syscall
    sb $a1 ($v0)
    sb $zero 1($v0)
    jr $ra

exit:
    li $v0, 10
    syscall

substring:
    add $a1, $a0, $a1
    move $a3, $a1
    li $v0, 9
    add $a2, $a2, 1
    move $a0, $a2
    add $a0, $a0, 1
    syscall
    # got a new string in $v0
    add $a2,$a2,$a3
    add $a2,$a2,-1
    move $a0, $v0
    substringcopy:
    beq $a1 $a2 substringexit
    lb $a3 ($a1)
    sb $a3 ($a0)
    add $a1, $a1, 1
    add $a0, $a0, 1
    j substringcopy
    substringexit:
    sb $zero, ($a0)
    jr $ra

copy:
    copyloop:
    lb $a2, ($a1)
    beq $zero, $a2 copyexit
    sb $a2, ($a0)
    add $a0,$a0,1
    add $a1,$a1,1
    j copyloop
    copyexit:
    sb $zero, ($a0)
    move $v0, $a0
    jr $ra

concat:
    sw $a0, -4($sp)
    sw $a1, -8($sp)
    sw $ra, -12($sp)
    jal size
    li $a3, 1
    add $a3,$a3,$v0
    lw $a0, -8($sp)
    jal size
    add $a3, $a3, $v0
    move $a0, $a3
    li $v0, 9
    syscall
    move $a3, $v0
    move $a0, $v0
    lw   $a1, -4($sp)
    jal copy
    move $a0, $v0
    lw $a1, -8($sp)
    jal copy
    move $v0, $a3
    lw $ra, -12($sp)
    jr $ra