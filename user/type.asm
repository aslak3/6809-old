		.include 'user.inc'

		.area PROG (REL)

		.org 0

		lds #USERSTACKEND

		ldx #0x4000
again:		lda ,x+
		cmpa #0
		beq out
		jsr jserialputchar
		cmpa #LF
		bne again
		lda #CR
		jsr jserialputchar
		bra again

out:		swi
