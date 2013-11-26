		.include 'user.inc'

		.area PROG (REL)

		.org 0

		lds #USERSTACKEND

		lda #10
		leax message,pcr
again:		pshs a,x
		jsr jserialputstr
		ldy #0x4000
		jsr jdelay
		puls a,x
		deca
		bne again
		swi

message:	.asciz 'Hello from CompactFlash!!\r\n'
