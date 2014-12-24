		.include 'user.inc'

		.area PROG (REL)

		.org 0

		lds #USERSTACKEND

		ldx #0x4000
again:		lda ,x+
		cmpa #0
		beq out
		jsr jioputchar
		cmpa #LF
		bne again
		lda #CR
		jsr jioputchar
		bra again

out:		swi
