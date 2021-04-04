	.text
	.file	"Bugsy"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	4631107791820423168     # double 42
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	x@GOTPCREL(%rip), %rax
	movabsq	$4630685579355357184, %rcx # imm = 0x4043800000000000
	movq	%rcx, (%rax)
	movq	y@GOTPCREL(%rip), %rax
	movabsq	$4613937818241073152, %rcx # imm = 0x4008000000000000
	movq	%rcx, (%rax)
	movq	z@GOTPCREL(%rip), %rax
	movabsq	$4631107791820423168, %rcx # imm = 0x4045000000000000
	movq	%rcx, (%rax)
	leaq	.Lfmt.1(%rip), %rdi
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movb	$1, %al
	callq	printf@PLT
	xorps	%xmm0, %xmm0
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	z,@object               # @z
	.bss
	.globl	z
	.p2align	3
z:
	.quad	0                       # double 0
	.size	z, 8

	.type	y,@object               # @y
	.globl	y
	.p2align	3
y:
	.quad	0                       # double 0
	.size	y, 8

	.type	x,@object               # @x
	.globl	x
	.p2align	3
x:
	.quad	0                       # double 0
	.size	x, 8

	.type	.Lfmt,@object           # @fmt
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmt:
	.asciz	"%d\n"
	.size	.Lfmt, 4

	.type	.Lfmt.1,@object         # @fmt.1
.Lfmt.1:
	.asciz	"%g\n"
	.size	.Lfmt.1, 4

	.type	.Lfmt.2,@object         # @fmt.2
.Lfmt.2:
	.asciz	"%s\n"
	.size	.Lfmt.2, 4


	.section	".note.GNU-stack","",@progbits
