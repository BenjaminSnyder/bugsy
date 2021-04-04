	.text
	.file	"Bugsy"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3               # -- Begin function main
.LCPI0_0:
	.quad	4625478292286210048     # double 17
.LCPI0_1:
	.quad	4627730092099895296     # double 25
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	m@GOTPCREL(%rip), %rax
	movabsq	$4625478292286210048, %rcx # imm = 0x4031000000000000
	movq	%rcx, (%rax)
	movq	n@GOTPCREL(%rip), %rax
	movabsq	$4627730092099895296, %rcx # imm = 0x4039000000000000
	movq	%rcx, (%rax)
	movsd	.LCPI0_0(%rip), %xmm0   # xmm0 = mem[0],zero
	movsd	.LCPI0_1(%rip), %xmm1   # xmm1 = mem[0],zero
	callq	add@PLT
	leaq	.Lfmt.1(%rip), %rdi
	movb	$1, %al
	callq	printf@PLT
	popq	%rax
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.globl	add                     # -- Begin function add
	.p2align	4, 0x90
	.type	add,@function
add:                                    # @add
	.cfi_startproc
# %bb.0:                                # %entry
	movsd	%xmm0, -8(%rsp)
	movsd	%xmm1, -16(%rsp)
	addsd	%xmm1, %xmm0
	retq
.Lfunc_end1:
	.size	add, .Lfunc_end1-add
	.cfi_endproc
                                        # -- End function
	.type	n,@object               # @n
	.bss
	.globl	n
	.p2align	3
n:
	.quad	0                       # double 0
	.size	n, 8

	.type	m,@object               # @m
	.globl	m
	.p2align	3
m:
	.quad	0                       # double 0
	.size	m, 8

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

	.type	.Lfmt.3,@object         # @fmt.3
.Lfmt.3:
	.asciz	"%d\n"
	.size	.Lfmt.3, 4

	.type	.Lfmt.4,@object         # @fmt.4
.Lfmt.4:
	.asciz	"%g\n"
	.size	.Lfmt.4, 4

	.type	.Lfmt.5,@object         # @fmt.5
.Lfmt.5:
	.asciz	"%s\n"
	.size	.Lfmt.5, 4


	.section	".note.GNU-stack","",@progbits
