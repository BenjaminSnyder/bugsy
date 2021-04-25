	.text
	.file	"Bugsy"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	4647503709213818880     # double 500
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$88, %rsp
	.cfi_def_cfa_offset 96
	movq	$0, 80(%rsp)
	movq	$0, 72(%rsp)
	movabsq	$4647503709213818880, %rax # imm = 0x407F400000000000
	movq	%rax, 64(%rsp)
	movq	%rax, 56(%rsp)
	movabsq	$4632233691727265792, %rax # imm = 0x4049000000000000
	movq	%rax, 48(%rsp)
	movabsq	$4647538893585907712, %rax # imm = 0x407F600000000000
	movq	%rax, 40(%rsp)
	movabsq	$4621819117588971520, %rax # imm = 0x4024000000000000
	movq	%rax, 32(%rsp)
	movq	$.Lstr, 24(%rsp)
	movq	$.Lstr.3, 16(%rsp)
	movq	$.Lstr.4, 8(%rsp)
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	xorps	%xmm2, %xmm2
	xorps	%xmm3, %xmm3
	movaps	%xmm0, %xmm1
	callq	add_canvas
	xorl	%eax, %eax
	addq	$88, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
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

	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	"0.0 0.3 0.5"
	.size	.Lstr, 12

	.type	.Lstr.3,@object         # @str.3
.Lstr.3:
	.asciz	"0.2 0.2 0.2"
	.size	.Lstr.3, 12

	.type	.Lstr.4,@object         # @str.4
.Lstr.4:
	.asciz	"qwerty"
	.size	.Lstr.4, 7

	.section	".note.GNU-stack","",@progbits
