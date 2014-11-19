; Main snake source

; This is a whole game

game:		lbsr randominit		; prepare the pseudo random numbers

		lda #2			; start at length of two
		sta snakelength,pcr	; save the length
		clr headpos,pcr		; snake snarts at the top of table

life:		lbsr clearscreen	; back to empty screen
		lbsr drawplayarea	; draw the border

		lda #12			; row coord of where snake starts
		leax rowsnake,pcr	; get the top of the row table
		clrb			; counter
rowinitloop:	sta ,x+			; save the same row along the table
		decb			; 256 bytess
		bne rowinitloop		; back for more

		lda #16			; col coord of whee snake starts
		leax colsnake,pcr	; get the top of the col table
		clrb			; counter
colinitloop:	sta ,x+			; save the smae col along the table
		decb			; 256 bytes
		bne colinitloop		; back for more

		clr rowdirection,pcr	; snake moves...
		lda #1			; across to the right...
		sta coldirection,pcr	; but not along at the start

		lbsr placenewfood	; place the first food on the map

		; main loop of the game

mainloop:	clra			; we are blanking out the far end
		ldb headpos,pcr		; get the current end index
		subb snakelength,pcr	; subtract the length (may wrap)
		lbsr drawsnakepart	; and rubout the far end of the snake

		lda #SNAKEBODYTILE	; draw over the last head tile
		ldb headpos,pcr		; with a body tile
		lbsr drawsnakepart	; snake is now headless :(

		lbsr movesnake		; move snake, inrementing headpos

		ldb headpos,pcr		; load the new headpos
		lbsr testcollide	; and see if theres been a collision
		beq nocollision		; if not, skip

		lbsr docollision	; process the collision
		beq death		; if it set zero, then snake is dead

nocollision:	lda #SNAKEHEADTILE	; finally we can draw the new head
		ldb headpos,pcr		; get the headposition again
		lbsr drawsnakepart	; and draw the head

		ldx #0x2000		; delay the game
controlloop:	lbsr controlsnake	; but in each delay loop, poll stick
		leax -1,x		; decrement delay
		bne controlloop		; until zero

		bra mainloop		; and back to the top again

death:		bra life

; draws a bit of the snake, tile in a. may be blank (0), maybe body or maybe
; head. b has position along snake array we want to draw.

drawsnakepart:	sta stamp,pcr		; save the tile to draw for stampat

		leax rowsnake,pcr	; setup row table pointer
		abx			; add (unsigned) the snake pos
		lda ,x			; and deref to get the row coord

		leax colsnake,pcr	; setup the col table pointer
		abx			; add (unsigned) the snake pos
		ldb ,x			; and deref to get the col coord

		lbsr stampat		; update screen with the new tile

		rts

; reads the current head position, and adds a new row and column to the
; position arrays, taking into account the direction the snake is moving.
; takes no parameters.

movesnake:	ldb headpos,pcr		; get the current head

		leax rowsnake,pcr	; setup row table pointer
		abx			; add (unsigned) the snake pos
		lda ,x			; and deref to get row of head
		adda rowdirection,pcr	; add the current direction (row)
		leax rowsnake,pcr	; setup table again
		incb			; because we need to wrap around
		abx			; but we are on the next index
		sta ,x			; and finally we can save into it

		ldb headpos,pcr		; same again but for cols

		leax colsnake,pcr	; ...
		abx			; ...
		lda ,x			; ...
		adda coldirection,pcr	; ...
		leax colsnake,pcr	; ...
		incb			; ...
		abx			; ...
		sta ,x			; ...

		inc headpos,pcr		; move the head to the next position
		rts			; will wrap 255->0



testcollide:	leax rowsnake,pcr
		abx
		lda ,x

		leax colsnake,pcr
		abx
		ldb ,x

		lbsr peekat
		lda peek,pcr

		rts

docollision:	cmpa #FOODTILE
		beq yumyum
		clra
docollisiono:	rts

yumyum:		inc snakelength,pcr
		lbsr placenewfood
		lda #1
		bra docollisiono

placenewfood:	lda #FOODTILE
		sta stamp,pcr
tryplaceagain:	lbsr randomnumber
		anda #31
		tfr a,b
		lbsr randomnumber
		cmpa #24
		bhs tryplaceagain
		lbsr peekat
		tst peek,pcr
		bne tryplaceagain
		lbsr stampat
		rts

controlsnake:	jsr jreadjoystick
		bita #JOYLEFT
		bne moveleft
		bita #JOYRIGHT
		bne moveright
		bita #JOYUP
		bne moveup
		bita #JOYDOWN
		bne movedown
controlsnakeo:	rts

moveleft:	lda #-1
		bra moveleftright
moveright:	lda #1
		bra moveleftright
moveup:		lda #-1
		bra moveupdown
movedown:	lda #1
		bra moveupdown

moveleftright:	sta coldirection,pcr
		clr rowdirection,pcr
		bra controlsnakeo
moveupdown:	clr coldirection,pcr
		sta rowdirection,pcr
		bra controlsnakeo

randominit:	lda uptimel+1
		sta randomseed
		rts

randomnumber:	lda randomseed
		beq doeor
		asla
		beq noeor		; if the input was $80, skip the eor
		bcc noeor
doeor:		eora #0x1d
noeor:		sta randomseed

		rts

drawplayarea:	clra			; top left is 0, 0
		clrb			; ..
		pshu a,b		; push this on the u stack
		lda #23			; bottom right is 23, 31
		ldb #31			; ...
		pshu a,b		; push this on
		lbsr drawbox		; call the drawbox function
		leau 4,u		; reset the stack

		; title message over the top border
		lda #0			; top row
		ldb #16-4		; roughly centered..
		leax titlemessage,pcr	; get the location of the message
		lbsr printstrat		; and print it

		rts

; Variables

rowsnake:	.rmb 256		; circular array for snake position
colsnake:	.rmb 256		; for rows and columns
headpos:	.rmb 1			; offset into arrays where head is
snakelength:	.rmb 1			; the length of the sanke

rowdirection:	.rmb 1			; either -1, 0 or 1 for row dir
coldirection:	.rmb 1			; either -1, 0 or 1 for col dir

randomseed:	.rmb 1			; seed, and last random made

