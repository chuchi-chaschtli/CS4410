	.globl main
	.data

	.text
main:
sw $fp 0($sp)
move $fp $sp
addi $sp $sp -128
L11:
lw $s0, 4($s0)
move $s1, $s0
li $s0, 1
move $s0, $s0
j L10
L10:
move $sp $fp
lw $fp 0($sp)
jr $ra

