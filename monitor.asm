		include 'hardware.inc'

		section _main

; fast interrupt vector

		org $fff6

		fdb firqinterrupt

; normal interrupt vector

		org $fff8

		fdb irqinterrupt

; software interrupt vector

		org $fffa

		fdb moninterrupt

; non maskable interupt vector

		org $fffc

		fdb moninterrupt

; setup the reset vector, last location in rom

		org $fffe
	
		fdb reset

; this is the start of rom

		org $c000

; at the start of rom is the jump table for external (ram) programs to use
; to call into the rom

; START OF GLOBAL READ-ONLY DATA

greetingmsg:	fcn '\r\n6809 Monitor v0.4\r\n\r\n'
youtypedmsg:	fcn 'You typed: '
promptmsg:	fcn 'Monitor: > '
nosuchmsg:	fcn 'No such command\r\n'
commfailedmsg:	fcn 'Command failed, possibly bad syntax\r\n'
breakatmsg:	fcn '***Break at '
badexitmsg:	fcn 'Internal error; leaving monitor\r\n'
flashreadymsg:	fcn '+++'
resetmsg:	fcn '\r\n***flash with f or any other key to start\r\n'
newlinemsg:	fcn '\r\n'

; commandarray - subroutine address followed by command letter code

commandarray:	fdb dumpmemory
		fcc 'd'
		fdb writememory
		fcc 'w'
		fdb exitmonitor
		fcc 'e'
		fdb showregisters
		fcc 'r'
		fdb showhelp
		fcc 'h'
		fdb showhelp
		fcc '?'			; same command different letter
		fdb resetmonitor
		fcc 'q'
		fdb showuptime
		fcc 'u'
		fdb resetuptime
		fcc 'U'
		fdb latchout
		fcc 'c'
		fdb latchin
		fcc 'C'
		fdb spistore
		fcc '+'
		fdb ideidentify
		fcc 'y'
		fdb idereadsector
		fcc '<'
		fdb idewritesector
		fcc '>'
		fdb readblk
		fcc '{'
		fdb fsmount
		fcc 'm'
		fdb readinode
		fcc 'i'
		fdb readbyinode
		fcc 'f'
		fdb listdirbyinode
		fcc 'l'
		fdb playay
		fcc 'p'
		fdb xmodem
		fcc 'x'
		fdb disassemble
		fcc 's'
		fdb parsetest
		fcc 'z'
		fdb setbank
		fcc 'b'
		fdb getbank
		fcc 'B'
		fdb readbyte
		fcc 'R'
		fdb testvramread
		fcc 'L'
		fdb testvramwrite
		fcc 'S'
		fdb terminit
		fcc 'Y'
		fdb tclearscreen
		fcc 'V'
		fdb testreg
		fcc 'K'
		fdb showstick
		fcc 'j'
		fdb $0000
		fcb NULL

bootbeeps:	fcn 'abcdefg'

; END OF DATA

; setup stack to the end of ram so it can go grown backwards

memerror:	lda #$80
		sta SOUNDER		; change tone
memerrorloop:	bra memerrorloop		

reset:		lda #$10
		sta SOUNDER

		lda #$04

nextbank:	deca
		sta BANKLATCH
		ldx #HIRAMSTART

nextbyte:	clr ,x
		tst ,x+
		bne memerror

		cmpx #HIRAMEND+1
		bne nextbyte

		tsta
		bne nextbank

		lda #$01
		sta BANKLATCH

		lds #STACKEND+1		; setup hardware stack

		clr SOUNDER

init:		clr IRQFILTER		; clear interrupt regs
		clr FIRQFILTER		; they will be init'd in inits

		clr keyreadpointer
		clr keywritepointer
		lbsr serialinit		; setup the serial port
		lbsr spiinit		; prepare the SPI
		lbsr terminit
		lbsr timerinit

		lbsr serialactive

		ldx #resetmsg		; show prompt for flash
		lbsr ioputstr

		lbsr iogetwto		; wait for a key, getting the command

		bne normalstart		; timeout

		cmpa #$66		; 'f'
		beq flashing
		cmpa #$20		; space
		beq noscreen
		bra normalstart

flashing:	ldx #ROMSTART		; setup the rom copy to ram
		ldy #ROMCOPYSTART	; this is the second half of ram
romcopy:	lda ,x+			; read in
		sta ,y+			; and read out
		cmpx #ROMEND+1		; check to see if we are at the end
		bne romcopy		; copy more

		ldx #flasher		; get the location of the flasher code
		leax -ROMSTART,x	; offset it from the start of rom
		leax ROMCOPYSTART,x	; offset it forward where it now is 
		jmp ,x			; jump to the new location of flasher

normalstart:	lbsr tactive

noscreen:	ldx #greetingmsg	; greetings!
		lbsr ioputstr		; output the greeting

		ldx #bootbeeps
		lbsr ay8910playtune	; play booting beeps

		lda #$20		; beep frequency
		sta SOUNDER		; set the beeper beeping
		ldy #$d000		; small delay
		lbsr delay

		lda #$10		; higher pitched noise
		sta SOUNDER		; more beeps
		ldy #$7000		; shorter delay
		lbsr delay
		
		clr SOUNDER		; silence the beeper


		enableinter		; enable interrupts

		swi			; enter the monitor (mainloop)

; we should never get here - it means rti was done without fixing up
; the return address - print error and loop

		ldx #badexitmsg		; if we get here then setup a msg
		lbsr ioputstr	; print the message
badexitloop:	bra badexitloop		; loop on the spot

; fast irq routine - dispatch to device handlers

firqinterrupt:	pshs a
		lda IRQSTATUS
		bita #IRQ88C681
		beq timerhandlergo
		bra firqinterrupto
firqinterrupto:	puls a
		rti
timerhandlergo:	lbsr timerhandler
		bra firqinterrupto

; irq service routine - dispatch to device handlers

irqinterrupt:	lda IRQSTATUS
		bita #IRQ65C22
		beq keyhandlergo
		bra irqinterrupto
irqinterrupto:	rti
keyhandlergo:	lbsr keyhandler
		bra irqinterrupto

; monitor entry point

moninterrupt:	enableinter		; enable interrupts again

		ldx #outputbuffer	; setup the "break" message
		ldy #breakatmsg		; ...
		lbsr concatstr		; append it
		leay ,s			; get the new stack pointr
		sty spatentry		; save the new stack pointer
		ldd 10,y		; the pc is 10 bytes in
		lbsr wordtoaschex	; convert it to ascii
		ldy #newlinemsg		; adding a newline
		lbsr concatstr		; append it
		clr ,x+			; add a null

		ldx #outputbuffer	; reset the string pointer
		lbsr ioputstr		; so it can be output

mainloop:	ldx #promptmsg		; ">" etc
		lbsr ioputstr		; output that
		
		ldx #inputbuffer	; now we need a command
		lbsr iogetstr		; get the command (waiting as needed)

		ldx #newlinemsg
		lbsr ioputstr	; tidy up the output with a newline

		lda inputbuffer		; get the first char
		ldx #commandarray	; setup the command pointer
nextcommand:	ldy ,x++		; get the sub address
		ldb ,x			; get the command letter
		beq commandarraye	; end of command list?
		cmpa ,x+		; compare input letter with array item
		bne nextcommand		; no match? check next one
		jsr ,y			; jump to subroutine
		bne commanderror	; error check for zero
		bra mainloop		; back to top
commanderror:	ldx #commfailedmsg
		lbsr ioputstr	; show error
		bra mainloop

commandarraye:	ldx #nosuchmsg
		lbsr ioputstr	; show error message
		bra mainloop

; general error handler branch for commands that fail

generalerror:	lda #1
		rts

;;; COMMANDS ;;;

; dumpmemory - "d AAAA BBBB" - dumps from AAAA, BBBB bytes
;
; C000  48 65 6C 6C 6F 2C 20 74  68 69 73 20 69 73 20 61  [Hello, this is a]

dumpmemory:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		ldd ,y++		; start address
		andb #$f0		; round to nearest 16
		std dumppointer		; store it in the variable
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		bne generalerror	; yes, mark it as bad
		ldd ,y++		; length/count of bytes
		andb #$f0		; also rounded
		std dumpcounter		; store it in the variable

dumpnextrow:	ldx #outputbuffer	; x is persistent across the whole row

		ldd dumppointer		; get address of this row
		lbsr wordtoaschex	; print the address into the buffer
		lda #$20		; space
		sta ,x+			; add a space after the address
		sta ,x+			; and another

; hex version

		ldy dumppointer	; points at the start of the row
		ldb #0			; counts across 16 bytes

hexbyteloop:	cmpb #$08		; for pretty ness...
		bne noextraspace	; add a space after 8 bytes
		lda #$20		; space
		sta ,x+			; push it in
noextraspace:	lda b,y			; actually read the byte from memory
		lbsr bytetoaschex	; convert it to ascii
		lda #$20		; space
		sta ,x+			; add a space between each byte
		incb			; incrememnt offset
		cmpb #$10		; 16 bytes per row
		bne hexbyteloop		; and do the next byte

		lda #$20		; spaces
		sta ,x+			; add it in
		lda #$5b		; opening [
		sta ,x+			; add it in

; ascii version

		ldy dumppointer		; reset back to the start of the row
		ldb #0			; counts across 16 bytes

ascbyteloop:	lda b,y			; get the byte from memory
		lbsr printableasc	; nonprintable to a dot
		sta ,x+			; add it in
		incb			; increment offset
		cmpb #$10		; 16 bytes per row
		bne ascbyteloop		; and do the next byte

		lda #$5d		; closing ]
		sta ,x+			; add it in

		clr ,x+			; null terminator

		ldx #outputbuffer	; reset back to the start
		lbsr ioputstr	; so we can finally output it!

		ldx #newlinemsg		; newline
		lbsr ioputstr	; output it

; move onto the the next row

		ldx dumppointer		; load the pointer back in
		leax $10,x		; add $10
		stx dumppointer		; store it back
		ldx dumpcounter		; loead the remaning byte counter
		leax -$10,x		; subtract $10
		stx dumpcounter		; store it back

		bne dumpnextrow		; more rows?
		clra			; no error
		rts

writememory:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		ldx ,y++		; start address
nextwrite:	lda ,y+			; get the type
		beq writememoryout	; that's the end of the byte list
		cmpa #1			; is it a byte?
		beq writebyte		; yes, so process bytes
		cmpa #2
		beq writeword		; same for words
		cmpa #3
		beq writestring		; same for strings
		tsta
		beq writememoryout	; null? we are done
		lbra generalerror	; otherwise its unknown, so bail

writebyte:	ldb ,y+			; get the byte to write
		stb ,x+			; and load it at memory x
		bra nextwrite		; back for more
writeword:	ldd ,y++		; get the word to write
		std ,x++		; and load it
		bra nextwrite		; back for more
writestring:	lbsr concatstr		; use the concatstr operation
		bra nextwrite		; ...it copies y->x
	
writememoryout:	clra			; clean exit
		rts

; exitmonitor - fixup return address and leave monitor

exitmonitor:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		lds #STACKEND+1		; move the stack to the top
		leas -12,s		; then move it back one frame
		ldx ,y++		; sub address
		beq cleanexit		; if exit address is 0 dont modify
		stx 10,s		; before modifiying the pc in stack
cleanexit:	rti			; run the usercode at that address

; shows the registers as they were when the monitor was entered

ccmsg:		fcn 'CC: '
amsg:		fcn '  A: '
bmsg:		fcn '  B: '
dpmsg:		fcn '  DP: '
xmsg:		fcn '  X: '
ymsg:		fcn '  Y: '
umsg:		fcn '  U: '
pcmsg:		fcn '  PC: '

showregisters:	ldx #outputbuffer	; set output buffer
		ldu spatentry

		ldy #ccmsg		; get the 'C: '
		lbsr concatstr		; concat it onto outputbuffer
		lda 0,u			; get the register value
		lbsr bytetoaschex	; convert and concat it

		ldy #amsg		; same again
		lbsr concatstr		; ...
		lda 1,u			; ...
		lbsr bytetoaschex	; ...

		ldy #bmsg
		lbsr concatstr
		lda 2,u
		lbsr bytetoaschex

		ldy #dpmsg
		lbsr concatstr
		lda 3,u
		lbsr bytetoaschex

		ldy #xmsg
		lbsr concatstr
		ldd 4,u
		lbsr wordtoaschex

		ldy #ymsg
		lbsr concatstr
		ldd 6,u
		lbsr wordtoaschex

		ldy #umsg
		lbsr concatstr
		ldd 8,u
		lbsr wordtoaschex

		ldy #pcmsg
		lbsr concatstr
		ldd 10,u
		lbsr wordtoaschex

		ldy #newlinemsg
		lbsr concatstr
		clr ,x+

		ldx #outputbuffer	; and output it
		lbsr ioputstr
		clra			; we always succeed
		rts

; shows some help text

helpmsg:	fcc 'Commands:\r\n'
		fcc '  r : show registers\r\n'
		fcc '  w AAAA BB WWWW "STRING" ... : write to AAAA bytes, words, strings\r\n'
		fcc '  d AAAA LLLL : dump from AAAA count LLLL bytes in hex\r\n'
		fcc '  e EEEE : exit to user code at EEEE\r\n'
		fcc '  q : reset the monitor\r\n'
		fcc '  + SSSS WWWW RRRR : from address SSSS write WWWW spi bytes then read\r\n'
		fcc '    RRRR bytes\r\n'
		fcc '  u : show uptime\r\n'
		fcc '  U : clear uptime counter\r\n'
		fcc '  c OO : output OO on the latch\r\n'
		fcc '  m : set 8bit ide and read mbr\r\n'
		fcc '  y : send ide identify command and show basic info\r\n'
		fcc '  { MMMM NNNN : read 1k disk block NNNN into MMMM\r\n'
		fcc '  < L0 L1 NN MMMM : read NN sectors from L0 L1 into MMMM\r\n'
		fcc '  > L0 L1 NN MMMM : write NN sectors from L0 L1 from MMMM\r\n'
		fcc '  l IIII : list directory at inode IIII\r\n'
		fcc '  i IIII ; show info about inode IIII\r\n'
		fcc '  f MMMM IIII : read file(etc) at inode IIII into MMMM\r\n'
		fcc '  x MMMM : receive file over XMODEM starting at MMMM\r\n'
		fcc '  p MMMM or p "STRING" : play notes at MMMM or STRING\r\n'
		fcc '  b BB : set the memory bank to BB\r\n'
		fcc '  B : show the current memory bank\r\n'
		fcc '  h or ? : this help\r\n'
		fcn '\r\n'

showhelp:	ldx #greetingmsg	; show the greeting
		lbsr ioputstr	; for the version number
		ldx #helpmsg		; and the help text
		lbsr ioputstr
		clra			; we always suceed
		rts

; restart the monitor (so we can flash it with 'f', most likely)

resetmonitor:	jmp [$fffe]		; reset via the reset vector

; showuptime

showuptime:	ldx #outputbuffer	; setup the output buffer
		ldd uptimeh		; load the high word
		lbsr wordtoaschex	; turn it into hex in x
		ldd uptimel		; load the low word
		lbsr wordtoaschex	; and turn that into hex in x
		ldy #newlinemsg		; get the newline string
		lbsr concatstr		; concat that too
		clr ,x+			; terminate the string
		ldx #outputbuffer	; reset the pointer
		lbsr ioputstr	; output the string
		rts

; reset uptime back to 0

resetuptime:	clra			; reset uptime
		clrb			; both bytes
		std uptimeh		; and store high word
		std uptimel		; and low word
		clra
		rts

; latchout - "c OO" outputs a byte on the "latch"

latchout:	lbsr parseinput		; parse the input
		lda ,y+			; get the type
		cmpa #1			; see if it is a byte
		lbne generalerror	; if not then validation error

		lda ,y+			; get the byte itself
;		sta LATCH		; output the byte on the latch leds

		clra
		rts

latchin:	;lda LATCH
		ldx #outputbuffer
		lbsr bytetoaschex
		clr ,x+
		ldx #outputbuffer
		lbsr ioputstr
		ldx #newlinemsg
		lbsr ioputstr

		clra
		rts

; spi store - "+ FFFF WWW RRR..." - writes and reads to the spi bus

spistore:	lbsr parseinput		; parse hexes, filling out inputbuffer

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldu ,y++		; get the start of mpu memory

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldx ,y++		; get the count of bytes to read

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldy ,y			; get the count of bytes to read

		lbsr spistart		; mark with start

spistorenext:	leax ,x			; early exit loop if no bytes to send
		beq spistoredone

		ldb ,u+			; get data to write
		lbsr spiwrite		; write it

		leax -1,x		; decrement the counter
		bra spistorenext	; back for more if there is more

spistoredone:
spiloadnext:	leay ,y			; early exit loop if no bytes to recv
		beq spiloaddone

		lbsr spiread		; get the byte we have been sent
		stb ,u+			; store it in the buffer we have
		
		leay -1,y		; secrent the counter

		bra spiloadnext		; back for more?

spiloaddone:	lbsr spistop		; mark with stop

		clra
		rts

; ideidentify - print info about the device

serialnomsg:	fcn 'Serial number: '
firmwarerevmsg:	fcn 'Firmware revision: '
modelnomsg:	fcn 'Model number: '

ideidentify:	lda #$ec		; this is the identify command
		lbsr simpleidecomm	; send it

		ldx #idescratchsec	; setup our read sector buffer
		lbsr idellreadr		; 512 reads (byte swapped)

		ldx #outputbuffer
		ldy #serialnomsg
		lbsr concatstr
		ldy #idescratchsec+20
		lda #20
		lbsr concatstrn
		ldy #newlinemsg
		lbsr concatstr
		clr ,x
		ldx #outputbuffer
		lbsr ioputstr

		ldx #outputbuffer
		ldy #firmwarerevmsg
		lbsr concatstr
		ldy #idescratchsec+46
		lda #8
		lbsr concatstrn
		ldy #newlinemsg
		lbsr concatstr
		clr ,x
		ldx #outputbuffer
		lbsr ioputstr

		ldx #outputbuffer
		ldy #modelnomsg
		lbsr concatstr
		ldy #idescratchsec+54
		lda #40
		lbsr concatstrn
		ldy #newlinemsg
		lbsr concatstr
		clr ,x+
		ldx #outputbuffer
		lbsr ioputstr

		clra
		rts

; idereadsector - < L0 L1 NN MMMM - read NN sectors from L0 L1 into MMMM

idereadsector:	lbsr parseinput

		lda ,y+			; get the type
		cmpa #1			; byte?
		lbne generalerror	; validation error
		lda ,y+
		sta IDELBA0		; this is the lowest byte in lba

		lda ,y+			; get the type
		cmpa #1			; byte?
		lbne generalerror	; validation error
		lda ,y+
		sta IDELBA1		; this is the 2nd lowestbyte in lba

		clr IDELBA2		; other two lba are zero
		clr IDELBA3

		lda ,y+			; get the type
		cmpa #1			; byte?
		lbne generalerror	; validation error
		lda ,y+			; get the count	of sectors
		sta IDECOUNT		; store it

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldx ,y++		; finally where to store the read

		lda #$20		; this is read sector
		lbsr simpleidecomm	; send the command

readsectorloop:	lbsr idellread		; read into x

		lda IDECOUNT		; we can ask the disk if there are
		bne readsectorloop	; more sectors to read?

		clra
		rts

; idewritesector - > L0 L1 NN MMMM - read NN sectors from L0 L1 into MMMM

idewritesector:	lbsr parseinput

		lda ,y+			; get the type
		cmpa #1			; byte?
		lbne generalerror	; validation error
		lda ,y+
		sta IDELBA0		; this is the lowest byte in lba

		lda ,y+			; get the type
		cmpa #1			; byte?
		lbne generalerror	; validation error
		lda ,y+
		sta IDELBA1		; this is the 2nd lowestbyte in lba

		clr IDELBA2		; other two lba are zero
		clr IDELBA3

		lda ,y+			; get the type
		cmpa #1			; byte?
		lbne generalerror	; validation error
		lda ,y+			; get the count	of sectors
		sta IDECOUNT		; store it

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldx ,y++		; finally where to read from

		lda #$30		; this is write sector
		lbsr simpleidecomm	; send the command

writesectorloop:lbsr idellwrite		; write into x

		lda IDECOUNT		; we can ask the disk if there are
		bne writesectorloop	; more sectors to read?

		clra
		rts

; readblk - b MMMM NNNN - read 1k fs block NNNN into MMMM

readblk:	lbsr parseinput

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldx ,y++		; memory address to write into

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldy ,y++		; fs block number to read

		lbsr fsreadblk		; do the read

		clra
		rts

; fsmount - m - calls idemount then prepares the fs

magicmsg:	fcn 'Magic is: '
startinodemsg:	fcn 'Start of inodes at block: '

fsmount:	lbsr idemount		; do the mbr read etc

		ldx #scratchblk		; setup the super block pointer
		ldy #$0001		; it is at block 1 (2nd block)

		lbsr fsreadblk		; read it in

		ldx #scratchblk		; no. inodes
		lbsr wordswap		; little->big endian
		ldx #scratchblk+2	; device size
		lbsr wordswap		; little->big endian
		ldx #scratchblk+4	; count of blocks of inode bmap blocks
		lbsr wordswap		; little->big endian
		ldx #scratchblk+6	; count of blocks of data bmap blocks
		lbsr wordswap		; little->big endian
		ldx #scratchblk+8	; where data blocks begin (unused)
		lbsr wordswap		; little->big endian
		ldx #scratchblk+16	; magic!
		lbsr wordswap		; little->big endian

		ldd #2			; we are already at the 2nd block
		addd scratchblk+4	; add the inode bmap blocks
		addd scratchblk+6	; and the data bmap blocks
		std startofinodes	; to get the start of our inodes

		ldx #magicmsg		; display the magic value
		ldy #scratchblk+16	
		lbsr ioputlab	; with a handy sub

		ldx #startinodemsg	; display the block offset value
		ldy #startofinodes
		lbsr ioputlab	; with a handy sub

		rts

typemodemsg:	fcn 'Type and Mode: '
filesizemsg:	fcn 'File size: '

; readinode - i IIII - reads inode IIII and prints some info about it

readinode:	lbsr parseinput

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldy ,y++		; this is the inode number

		ldx #inode		; we use a our general inode store
		lbsr fsreadinode	; read it in

		ldx #typemodemsg	; print the inode type and mode
		ldy #inode
		lbsr ioputlab	; using our fancy label+data printer

		ldx #filesizemsg	; and the file size
		ldy #inode+4
		lbsr ioputlab	; using the same fancy printer

		rts

; readbyinode - f MMMM IIII - reads file at inode IIII into memory MMMM

readbyinode:	lbsr parseinput

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldx ,y++		; memory address

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldy ,y++		; inode number

		lbsr fsreadfile

		rts

notdirmsg:	fcn 'Not a directory at that inode\r\n'

listdirbyinode:	lbsr parseinput

		lda ,y+
		cmpa #2
		lbne generalerror
		ldy ,y++

		lbsr fsshowdirlist
		bne listdirnotdir
		clra
		rts

listdirnotdir:	ldx #notdirmsg
		lbsr ioputstr
		lda #1
		rts

playay:		lbsr parseinput

		lda ,y+
		cmpa #2
		beq playaymemory
		cmpa #3
		beq playaydirect
		lda #1
		rts

playaymemory:	ldx ,y++
		bra playaynow

playaydirect:	tfr y,x
		bra playaynow

playaynow:	lbsr ay8910playtune
		clra
		rts

; x MMMM - read a xmodem upload into memory starting at MMMM

xmodem:		lbsr parseinput

		lda ,y+
		cmpa #2
		lbne generalerror
		ldx ,y++

waitforstart:	lda #NAK		; ask for a start
		lbsr serialputchar
		lbsr serialgetwto	; wait 2 sec for the start
		beq blockloopnoget	; got a start, no need to re-read
		bra waitforstart	; still waiting, so poke again

blockloop:	lbsr serialgetchar	; get header byte	
blockloopnoget:	cmpa #EOT		; EOT for end of file
		beq xmodemout		; if so then we are done
		cmpa #SOH		; SOH for start of block
		bne xmodemerr		; if not then this is an error

		lbsr serialgetchar	; blocks so far
		sta xmodemblkcount	; store the number of blocks
		lbsr serialgetchar	; 255 less blocks so far

		clr xmodemchecksum	; clear the checksum

		ldb #$80		; 128 bytes per block
byteloop:	lbsr serialgetchar	; get the byte for th efile
		sta ,x+			; store the byte
		adda xmodemchecksum	; add the received byte the checksum
		sta xmodemchecksum	; store the checksum on each byte
		decb			; decrement our byte counter
		bne byteloop		; see if there are more bytes
		
		lbsr serialgetchar	; get the checksum from sender
		cmpa xmodemchecksum	; check it against the one we made
		bne blockbad		; if no good, handle it

		lda #ACK		; if good, then ACK the block
		lbsr serialputchar	; send the ACK

		bra blockloop		; get more blocks

blockbad:	lda #NAK		; oh no, it was bad
		lbsr serialputchar	; send a NAK; sender resends block

		leax -$80,x		; move back to start of the block
		
		bra blockloop		; try to get the same block again

xmodemout:	lda #ACK		; at end of file, ACK the whole file
		lbsr serialputchar

		clra
		rts

xmodemerr:	lda #1
		rts		

disassemble:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		ldu ,y++		; start address
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; yes, mark it as bad
		ldx ,y++		; length/count of bytes

		lbsr disassentry	; do the disassembly

		rts

; z BB WWWW "STRING" .... - test the parser by outputting what was parsed

bytefoundmsg:	fcn "byte: "
wordfoundmsg:	fcn "word: "
stringfoundmsg:	fcn "string: "

parsetest:	lbsr parseinput

parsetestloop:	lda ,y+
		cmpa #1
		beq bytefound
		cmpa #2
		beq wordfound
		cmpa #3
		beq stringfound
		tsta
		beq parsetestout
		lbra generalerror

parsetestout:	clra
		rts

bytefound:	ldx #bytefoundmsg
		lbsr ioputstr
		lda ,y+
		ldx #outputbuffer
		lbsr bytetoaschex
		ldx #outputbuffer
		lbsr ioputstr
		ldx #newlinemsg
		lbsr ioputstr
		bra parsetestloop

wordfound:	ldx #wordfoundmsg
		lbsr ioputstr
		ldd ,y++
		ldx #outputbuffer
		lbsr wordtoaschex
		ldx #outputbuffer
		lbsr ioputstr
		ldx #newlinemsg
		lbsr ioputstr
		bra parsetestloop

stringfound:	ldx #stringfoundmsg
		lbsr ioputstr
		tfr y,x
		lbsr ioputstr
		tfr x,y
		ldx #newlinemsg
		lbsr ioputstr
		bra parsetestloop		

; b BB - set memory bank

setbank:	lbsr parseinput
		lda ,y+
		cmpa #1			; is it a word?
		lbne generalerror	; validation error

		lda ,y+
		sta BANKLATCH

		clra
		rts

; B - show the membory bank

getbank:	lda BANKLATCH
		ldx #outputbuffer
		lbsr bytetoaschex
		clr ,x+
		ldx #outputbuffer
		lbsr ioputstr
		ldx #newlinemsg
		lbsr ioputstr
		clra
		rts

readbyte:	lbsr parseinput
		lda ,y+
		cmpa #2			; is it a word?
		lbne generalerror	; validation error

		ldy ,y
		lda ,y

		ldx #outputbuffer
		lbsr bytetoaschex
		clr ,x+
		ldx #outputbuffer
		lbsr ioputstr
		ldx #newlinemsg
		lbsr ioputstr
		clra
		rts

showstick:	lbsr readjoystick

		ldx #outputbuffer
		lbsr bytetoaschex
		clr ,x+
		ldx #outputbuffer
		lbsr ioputstr
		ldx #newlinemsg
		lbsr ioputstr
		clra
		rts

testvramread:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		ldx ,y++		; start address
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; yes, mark it as bad
		ldu ,y++		; length/count of bytes
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; yes, mark it as bad
		ldy ,y			; vram address

		lbsr vread

		rts

testvramwrite:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		ldx ,y++		; start address
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; yes, mark it as bad
		ldu ,y++		; length/count of bytes
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; yes, mark it as bad
		ldy ,y			; vram address

		lbsr vwrite

		rts

testreg:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #1			; is it a word?
		lbne generalerror	; validation error
		ldb ,y+			; start address
		lda ,y+			; get the type
		cmpa #1			; is it a word?
		lbne generalerror	; yes, mark it as bad
		lda ,y+			; length/count of bytes

		sta VDIRECTPORT
		stb VDIRECTPORT

		clra

		rts


;;; END OF HIGH LEVEL COMMANDS

; flasher

flasher:	ldx #flashreadymsg	; tell other end it can send now
		lbsr ioputstr

; read 64bytes from the serial port into ram

		ldu #ROMSTART		; setup the counter into rom
inflashblk:	ldx #flashblock		; this is the block in ram we...
		ldb #64			; are copying into
inflash:	lbsr iogetchar	; get the byte from the port
		sta ,x+			; store it
		decb			; we store 64bytes
		bne inflash		; back to the next byte

; then write them into rom

		ldx #flashblock		; after we have a block
		ldb #64			; of 64 bytes
outflash:	lda ,x+			; get the byte from ram
		sta ,u+			; and write it into the rom
		decb			; reduce byte counter
		bne outflash		; back for more if non zero

; circa 10ms delay between blocks

		ldx #20000		; setup delay counter
flashdelayloop:	leax -1,x		; dey
		bne flashdelayloop

; tell the uploader that we have written the block, and it can send the
; next one

		lda #$23		; '#'
		lbsr ioputchar	; send the char
		cmpu #ROMEND+1		; see if we are the end of rom
		bne inflashblk		; back to the next block

; send the content of the rom back, so the sender knows what was written -
; we can't do anything if it didn't write, but at least we know

		ldu #ROMSTART		; back to the start
verflash:	lda ,u+			; get the byte
		lbsr ioputchar	; output it
		cmpu #ROMEND+1
		bne verflash

; we could in theory try again but good or bad, do a reset on the new
; reset vector
		
		jmp [$fffe]		; jump through the new reset vector

		endsection
