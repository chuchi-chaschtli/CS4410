	.globl main
	.data
L10:
 .ascii "hello"

	.text
main:
sw $fp 0($sp)
move $fp $sp
addi $sp $sp -128
L12:
lw $s0, 4($s0)
move $s1, $s0
j L11
L11:
move $sp $fp
lw $fp 0($sp)
jr $ra

