	.globl main
	.data

	.text
main:
sw $fp 0($sp)
move $fp $sp
addi $sp $sp -52
L19:
sw $s0, 4($s0)
la $s1, L12
li $s0, 8
move $s2, $s0
jal $s1
move $s0, $t0
j L18
L18:
move $sp $fp
lw $fp 0($sp)
jr $ra

L10:
sw $fp 0($sp)
move $fp $sp
addi $sp $sp -52
L21:
sw $s1, 4($s0)
sw $s0, 8($s0)
li $s0, 1
move $s1, $s0
lw $s0, 4($s0)
li $s1, 1
ble $s0, $s1, L16
L17:
li $s0, 0
move $s1, $s0
L16:
li $s0, 1
beq $s1, $s0, L13
L14:
lw $s0, 4($s0)
move $s0, $s0
la $s1, L12
lw $s0, 0($s0)
move $s1, $s0
lw $s0, 4($s0)
addi $s0, $s0, -1
move $s0, $s0
jal $s1
move $s1, $t0
mul $s0, $s0, $s1
move $s0, $s0
L15:
j L20
L13:
li $s0, 1
move $s0, $s0
j L15
L20:
move $sp $fp
lw $fp 0($sp)
jr $ra

