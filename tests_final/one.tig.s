	.globl main
	.data

	.text
L10:
sw $fp 0($sp)
move $fp $sp
addiu $sp $sp -~128
L12:

	lw $s0, 4($s0)

	move $s1, $s0

	
	
	
	
	
	
	
	
	
	li $s0, 1

	move $s0, $s0

	
	
	
	
	
	
	
	
	
	j L11

L11:

	
move $sp $fp
lw $fp 0($sp)
jr $ra

