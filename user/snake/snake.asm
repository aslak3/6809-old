; Main snake source

		.include 'user.inc'
		.include 'snake.inc'

		.area PROG (REL)

		.org 0			; dummy. we are using pic here

		ldu #USERSTACKEND	; setup the user stack

		lbsr videoinit
		lbsr soundinit

menu:		lbsr clearscreen

		lda #9
		ldb #2
		leax pressfire,pcr
		lbsr printstrat

		lda #11
		ldb #2
		leax orexit,pcr
		lbsr printstrat

		lda #13
		ldb #2
		leax orcalibrate,pcr
		lbsr printstrat

menupollloop:	jsr jreadjoystick
		bita #JOYFIRE1
		bne startgame
		bita #JOYUP
		bne exit
		bita #JOYDOWN
		bne startcalibrate

		bra menupollloop

startgame:	lbsr game

		bra menu

startcalibrate:	lbsr calibrate

		bra menu

exit:		swi

pressfire:	.asciz 'Press FIRE to start'

orexit:		.asciz 'Or UP to exit'

orcalibrate:	.asciz 'Or DOWN to calibrate screen'

		.include 'game.asm'
		.include 'graphics.asm'
		.include 'video.asm'
		.include 'sound.asm'
		.include 'calibrate.asm'
