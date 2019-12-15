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

