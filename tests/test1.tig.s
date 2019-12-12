	.globl main
	.data

	.text
main:
sw $fp 0($sp)
move $fp $sp
addi $sp $sp -144
L11:
lw $s0, 4($s0)
move $s1, $s0
la $s0, initArray
li $s0, 10
move $s1, $s0
li $s0, 0
move $s2, $s0
jal $s0
move $s0, $t0
j L10
L10:
move $sp $fp
lw $fp 0($sp)
jr $ra

