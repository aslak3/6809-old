.macro		sleep
		nop
		nop
		nop
.endm

.macro		loadareg register
		sta VDIRECTPORT
		lda #register|0x80
		sta VDIRECTPORT
.endm

; stores the value in the register

.macro		loadconstreg register, value
		lda #value
		sta VDIRECTPORT
		lda #register|0x80
		sta VDIRECTPORT
.endm

; gets the value of the status register

.macro		getstatusreg register
		lda #register
		loadareg VSTATUSREG
		lda VSTATUSPORT
.endm
