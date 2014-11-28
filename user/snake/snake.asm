; Main snake source

		.include 'user.inc'
		.include 'snake.inc'

		.area PROG (REL)

		.org 0			; dummy. we are using pic here

		ldu #USERSTACKEND	; setup the user stack

		lbsr videoinit

menu:		lbsr clearscreen

		lda #11
		ldb #6
		leax pressfire,pcr
		lbsr printstrat

		lda #13
		ldb #6
		leax orexit,pcr
		lbsr printstrat

menupollloop:	jsr jreadjoystick
		bita #JOYFIRE1
		bne startgame
		bita #JOYUP
		bne exit

		bra menupollloop

startgame:	lbsr game

		bra menu

exit:		swi

pressfire:	.asciz 'Press FIRE to start'

orexit:		.asciz 'Or UP to exit'

		.include 'game.asm'
		.include 'graphics.asm'
		.include 'video.asm'
