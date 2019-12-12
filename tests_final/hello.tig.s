	.globl main
	.data
L11:
 .ascii "hello"

	.text
PROCEDURE L10
L10:

	sw $s7, -4($s0)

	move $s7, $s0

	addi $s0, $s7, -52

	sw $s0, -8($s7)

	sw $s1, -12($s7)

	sw $s2, -16($s7)

	sw $s3, -20($s7)

	sw $s4, -24($s7)

	sw $s5, -28($s7)

	sw $s6, -32($s7)

	sw $s7, -36($s7)

L13:

	lw $s0, 4($s7)

	move $s1, $s0

	sw $s0, ~4($s7)

	sw $s0, ~8($s7)

	sw $s1, ~12($s7)

	sw $s2, ~16($s7)

	sw $s3, ~20($s7)

	sw $s4, ~24($s7)

	sw $s5, ~28($s7)

	sw $s6, ~32($s7)

	sw $s7, ~36($s7)

	
	lw $s0, ~4($s7)

	move $s0, $s0

	lw $s0, ~8($s7)

	move $s0, $s0

	lw $s0, ~12($s7)

	move $s1, $s0

	lw $s0, ~16($s7)

	move $s2, $s0

	lw $s0, ~20($s7)

	move $s3, $s0

	lw $s0, ~24($s7)

	move $s4, $s0

	lw $s0, ~28($s7)

	move $s5, $s0

	lw $s0, ~32($s7)

	move $s6, $s0

	lw $s0, ~36($s7)

	move $s7, $s0

	j L12

L12:

	
	lw $s0, -8($s7)

	sw $s1, -12($s7)

	sw $s2, -16($s7)

	sw $s3, -20($s7)

	sw $s4, -24($s7)

	sw $s5, -28($s7)

	sw $s6, -32($s7)

	sw $s7, -36($s7)

	move $s0, $s7

	lw $s7, -4($s7)

	jr $s0

END L10
