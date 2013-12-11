		.include 'hardware.inc'

		.include 'ramvars.asm'

		.area ROM (ABS)

; software interrupt vector

		.org 0xfffa

		.word swinterrupt

; non maskable interupt vector

		.org 0xfffc

		.word nmiinterrupt

; setup the reset vector, last location in rom

		.org 0xfffe
	
		.word reset

; this is the start of rom

		.org 0xc000

; at the start of rom is the jump table for external (ram) programs to use
; to call into the rom

		.include 'jumptable.asm'

; START OF GLOBAL READ-ONLY DATA

greetingmsg:	.asciz '\r\n6809 Monitor v0.0.0.0.6.\r\n\r\n'
youtypedmsg:	.asciz 'You typed: '
promptmsg:	.asciz 'Monitor: > '
nosuchmsg:	.asciz 'No such command\r\n'
commfailedmsg:	.asciz 'Command failed, possibly bad syntax\r\n'
breakatmsg:	.asciz '***Break at '
badexitmsg:	.asciz 'Internal error; leaving monitor\r\n'
flashreadymsg:	.asciz '+++'
resetmsg:	.asciz '\r\n***flash with f or any other key to start\r\n'
newlinemsg:	.asciz '\r\n'

; commandarray - subroutine address followed by command letter code

commandarray:	.word dumpmemory
		.ascii 'd'
		.word writememory
		.ascii 'w'
		.word exitmonitor
		.ascii 'e'
		.word showregisters
		.ascii 'r'
		.word showhelp
		.ascii 'h'
		.word showhelp
		.ascii '?'			; same command different letter
		.word resetmonitor
		.ascii 'q'
		.word showuptime
		.ascii 'u'
		.word latchout
		.ascii 'c'
		.word spistore
		.ascii '+'
		.word ideidentify
		.ascii 'y'
		.word idereadsector
		.ascii '<'
		.word readblk
		.ascii 'b'
		.word fsmount
		.ascii 'm'
		.word readinode
		.ascii 'i'
		.word readbyinode
		.ascii 'f'
		.word listdirbyinode
		.ascii 'l'
		.word playay
		.ascii 'p'
		.word xmodem
		.ascii 'x'
		.word 0x0000
		.byte NULL

bootbeeps:	.asciz 'abcdefg'

; END OF DATA

; setup stack to the end of ram so it can go grown backwards

reset:		lds #STACKEND+1		; setup hardware stack

		ldx #RAMSTART		; clear from start of ram
zeroram:	clr ,x+
		cmpx #RAMEND+1		; to end
		bne zeroram

; setup the serial port

		lbsr serialinit		; setup the serial port
		lbsr spiinit		; prepare the via
		clr LATCH		; blank the latch

		ldx #resetmsg		; show prompt for flash
		lbsr serialputstr

		lbsr serialgetchar	; wait for a key, getting the command

		cmpa #0x66		; 'f'
		bne normalstart		; not one? then normal startup

		ldx #ROMSTART		; setup the rom copy to ram
		ldy #ROMCOPYSTART	; this is the second half of ram
romcopy:	lda ,x+			; read in
		sta ,y+			; and read out
		cmpx #ROMEND+1		; check to see if we are at the end
		bne romcopy		; copy more

		ldx #flasher		; get the location of the flasher code
		leax -ROMSTART,x	; offset it from the start of rom
		leax ROMCOPYSTART,x	; offset it forward where it now is 
		jmp ,x			; jump to the new location of flasher

normalstart:	ldx #greetingmsg	; greetings!
		lbsr serialputstr	; output the greeting

;		ldx #bootbeeps
;		lbsr ay8910playtune	; play booting beeps

		clra			; reset uptime
		clrb			; both bytes
		std uptimeh		; and store high word
		std uptimel		; and low word

		swi			; enter the monitor (mainloop)

; we should never get here - it means rti was done without fixing up
; the return address - print error and loop

		ldx #badexitmsg		; if we get here then setup a msg
		lbsr serialputstr	; print the message
badexitloop:	bra badexitloop		; loop on the spot

; nmi uptime counter - increment 32 bit counter

nmiinterrupt:	ldx uptimel		; get current lowword uptime
		leax 1,x		; add 1
		stx uptimel		; store it back
		bne nmiinterrupto	; if not 0 then done
		ldx uptimeh		; otherwise low current highword
		leax 1,x		; add 1
		stx uptimeh		; store it back
nmiinterrupto:	rti

; monitor entry point

swinterrupt:	ldx #outputbuffer	; setup the "break" message
		ldy #breakatmsg		; ...
		lbsr concatstr		; append it
		leay ,s			; get the new stack pointr
		sty spatentry
		ldd 10,y		; the pc is 10 bytes in
		lbsr wordtoaschex	; convert it to ascii
		ldy #newlinemsg		; adding a newline
		lbsr concatstr		; append it
		clr ,x+			; add a null

		ldx #outputbuffer	; reset the string pointer
		lbsr serialputstr	; so it can be output

mainloop:	ldx #promptmsg		; ">" etc
		lbsr serialputstr	; output that
		
		ldx #inputbuffer	; now we need a command
		lbsr serialgetstr	; get the command (waiting as needed)

		ldx #outputbuffer

		ldy #newlinemsg		; tidy up the console with a newline
		lbsr concatstr		; ...

		ldy #youtypedmsg	; tell the user ...
		lbsr concatstr		; ...
		ldy #inputbuffer	; ...
		lbsr concatstr		; ...
		ldy #newlinemsg		; ...
		lbsr concatstr		; ...
		clr ,x+			; (add a null)

		ldx #outputbuffer
		lbsr serialputstr	; ... what they typed

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
		lbsr serialputstr	; show error
		bra mainloop

commandarraye:	ldx #nosuchmsg
		lbsr serialputstr	; show error message
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
		andb #0xf0		; round to nearest 16
		std dumppointer		; store it in the variable
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		bne generalerror	; yes, mark it as bad
		ldd ,y++		; length/count of bytes
		andb #0xf0		; also rounded
		std dumpcounter		; store it in the variable

dumpnextrow:	ldx #outputbuffer	; x is persistent across the whole row

		ldd dumppointer		; get address of this row
		lbsr wordtoaschex	; print the address into the buffer
		lda #0x20		; space
		sta ,x+			; add a space after the address
		sta ,x+			; and another

; hex version

		ldy dumppointer	; points at the start of the row
		ldb #0			; counts across 16 bytes

hexbyteloop:	cmpb #0x08		; for pretty ness...
		bne noextraspace	; add a space after 8 bytes
		lda #0x20		; space
		sta ,x+			; push it in
noextraspace:	lda b,y			; actually read the byte from memory
		lbsr bytetoaschex	; convert it to ascii
		lda #0x20		; space
		sta ,x+			; add a space between each byte
		incb			; incrememnt offset
		cmpb #0x10		; 16 bytes per row
		bne hexbyteloop		; and do the next byte

		lda #0x20		; spaces
		sta ,x+			; add it in
		lda #0x5b		; opening [
		sta ,x+			; add it in

; ascii version

		ldy dumppointer		; reset back to the start of the row
		ldb #0			; counts across 16 bytes

ascbyteloop:	lda b,y			; get the byte from memory
		lbsr printableasc	; nonprintable to a dot
		sta ,x+			; add it in
		incb			; increment offset
		cmpb #0x10		; 16 bytes per row
		bne ascbyteloop		; and do the next byte

		lda #0x5d		; closing ]
		sta ,x+			; add it in

		clr ,x+			; null terminator

		ldx #outputbuffer	; reset back to the start
		lbsr serialputstr	; so we can finally output it!

		ldx #newlinemsg		; newline
		lbsr serialputstr	; output it

; move onto the the next row

		ldx dumppointer		; load the pointer back in
		leax 0x10,x		; add 0x10
		stx dumppointer		; store it back
		ldx dumpcounter		; loead the remaning byte counter
		leax -0x10,x		; subtract 0x10
		stx dumpcounter		; store it back

		bne dumpnextrow		; more rows?
		clra			; no error
		rts

writememory:	lbsr parseinput		; parse hexes, filling out inputbuffer
		lda ,y+			; get the type
		cmpa #2			; is it a word?
		lbne generalerror	; validation error
		ldx ,y++		; start address
nextwritebyte:	lda ,y+			; get the type
		beq writememoryout	; that's the end of the byte list
		cmpa #1			; is it a byte?
		lbne generalerror	; not a byte, so error
		ldb ,y+			; get the byte to write
		stb ,x+			; and load it at memory x
		bra nextwritebyte	; back for more
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

ccmsg:		.asciz 'CC: '
amsg:		.asciz '  A: '
bmsg:		.asciz '  B: '
dpmsg:		.asciz '  DP: '
xmsg:		.asciz '  X: '
ymsg:		.asciz '  Y: '
umsg:		.asciz '  U: '
pcmsg:		.asciz '  PC: '

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
		lbsr serialputstr
		clra			; we always succeed
		rts

; shows some help text

helpmsg:	.ascii 'Commands:\r\n'
		.ascii '  r : show registers\r\n'
		.ascii '  w AAAA B1 B2 B3 ... : write to AAAA bytes B1 B2 B3 ...\r\n'
		.ascii '  d AAAA LLLL : dump from AAAA count LLLL bytes in hex\r\n'
		.ascii '  e EEEE : exit to user code at EEEE\r\n'
		.ascii '  q : reset the monitor\r\n'
		.ascii '  + SSSS WWWW RRRR : from address SSSS write WWWW spi bytes then read\r\n'
		.ascii '    RRRR bytes\r\n'
		.ascii '  u : show uptime\r\n'
		.ascii '  c OO : output OO on the latch\r\n'
		.ascii '  m : set 8bit ide and read mbr\r\n'
		.ascii '  y : send ide identify command and show basic info\r\n'
		.ascii '  b MMMM NNNN : read 1k disk block NNNN into MMMM\r\n'
		.ascii '  < L0 L1 NN MMMM : read NN sectors from L0 L1 into MMMM\r\n'
		.ascii '  l IIII : list directory at inode IIII\r\n'
		.ascii '  i IIII ; show info about inode IIII\r\n'
		.ascii '  f MMMM IIII : read file(etc) at inode IIII into MMMM\r\n'
		.ascii '  x MMMM : receive file over XMODEM starting at MMMM\r\n'
		.ascii '  h or ? : this help\r\n'
		.asciz '\r\n'

showhelp:	ldx #greetingmsg	; show the greeting
		lbsr serialputstr	; for the version number
		ldx #helpmsg		; and the help text
		lbsr serialputstr
		clra			; we always suceed
		rts

; restart the monitor (so we can flash it with 'f', most likely)

resetmonitor:	jmp [0xfffe]		; reset via the reset vector

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
		lbsr serialputstr	; output the string
		rts

; latchout - "c OO" outputs a byte on the "latch"

latchout:	lbsr parseinput		; parse the input
		lda ,y+			; get the type
		cmpa #1			; see if it is a byte
		lbne generalerror	; if not then validation error

		lda ,y+			; get the byte itself
		sta LATCH		; output the byte on the latch leds

		clra
		rts

; spi store - "+ FFFF WWW RRR..." - writes and reads to the spi bus

spistore:	lbsr parseinput		; parse hexes, filling out inputbuffer

		lda ,y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldu ,y++		; get the start of mpu memory

		lda, y+			; get the type
		cmpa #2			; word?
		lbne generalerror	; validation error
		ldx ,y++		; get the count of bytes to read

		lda, y+			; get the type
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
spiloadnext:	leay, y			; early exit loop if no bytes to recv
		beq spiloaddone

		lbsr spiread		; get the byte we have been sent
		stb, u+			; store it in the buffer we have
		
		leay -1,y		; secrent the counter

		bra spiloadnext		; back for more?

spiloaddone:	lbsr spistop		; mark with stop

		clra
		rts

; ideidentify - print info about the device

serialnomsg:	.asciz 'Serial number: '
firmwarerevmsg:	.asciz 'Firmware revision: '
modelnomsg:	.asciz 'Model number: '

ideidentify:	lda #0xec		; this is the identify command
		lbsr simpleidecomm	; send it

		ldx #ideidentifysec	; setup our read sector buffer
		lbsr idellread		; fil it out by 512 reads

		ldx #outputbuffer
		ldy #serialnomsg
		lbsr concatstr
		ldy #ideidentifysec+20
		lda #20
		lbsr concatstrn
		ldy #newlinemsg
		lbsr concatstr
		clr ,x
		ldx #outputbuffer
		lbsr serialputstr

		ldx #outputbuffer
		ldy #firmwarerevmsg
		lbsr concatstr
		ldy #ideidentifysec+46
		lda #8
		lbsr concatstrn
		ldy #newlinemsg
		lbsr concatstr
		clr ,x
		ldx #outputbuffer
		lbsr serialputstr

		ldx #outputbuffer
		ldy #modelnomsg
		lbsr concatstr
		ldy #ideidentifysec+54
		lda #40
		lbsr concatstrn
		ldy #newlinemsg
		lbsr concatstr
		clr ,x+
		ldx #outputbuffer
		lbsr serialputstr

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
		ldx, y++		; finally where to store the read

		lda #0x20		; this is read sector
		lbsr simpleidecomm	; send the command

readsectorloop:	lbsr idellread		; read into x

		lda IDECOUNT		; we can ask the disk if there are
		bne readsectorloop	; more sectors to read?

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

magicmsg:	.asciz 'Magic is: '
startinodemsg:	.asciz 'Start of inodes at block: '

fsmount:	lbsr idemount		; do the mbr read etc

		ldx #fssuperblk		; setup the super block pointer
		ldy #0x0001		; it is at block 1 (2nd block)

		lbsr fsreadblk		; read it in

		ldx #fssuperblk		; no. inodes
		lbsr wordswap		; little->big endian
		ldx #fssuperblk+2	; device size
		lbsr wordswap		; little->big endian
		ldx #fssuperblk+4	; count of blocks of inode bmap blocks
		lbsr wordswap		; little->big endian
		ldx #fssuperblk+6	; count of blocks of data bmap blocks
		lbsr wordswap		; little->big endian
		ldx #fssuperblk+8	; where data blocks begin (unused)
		lbsr wordswap		; little->big endian
		ldx #fssuperblk+16	; magic!
		lbsr wordswap		; little->big endian

		ldd #2			; we are already at the 2nd block
		addd fssuperblk+4	; add the inode bmap blocks
		addd fssuperblk+6	; and the data bmap blocks
		std startofinodes	; to get the start of our inodes

		ldx #magicmsg		; display the magic value
		ldy #fssuperblk+16	
		lbsr serialputlab	; with a handy sub

		ldx #startinodemsg	; display the block offset value
		ldy #startofinodes
		lbsr serialputlab	; with a handy sub

		rts

typemodemsg:	.asciz 'Type and Mode: '
filesizemsg:	.asciz 'File size: '

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
		lbsr serialputlab	; using our fancy label+data printer

		ldx #filesizemsg	; and the file size
		ldy #inode+4
		lbsr serialputlab	; using the same fancy printer

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

notdirmsg:	.asciz 'Not a directory at that inode\r\n'

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
		lbsr serialputstr
		lda #1
		rts

playay:		lbsr parseinput

		lda ,y+
		cmpa #2
		lbne generalerror
		ldx ,y++

		lbsr ay8910playtune

		clra
		rts

; x MMMM - read a xmodem upload into memory starting at MMMM

xmodem:		lbsr parseinput

		lda ,y+
		cmpa #2
		lbne generalerror
		ldx ,y++

blockloop:	lbsr serialgetchar	; get the "header byte"
		cmpa #EOT		; EOT for end of file
		beq xmodemout		; if so then we are done
		cmpa #SOH		; SOH for start of block
		bne xmodemerr		; if not then this is an error

		lbsr serialgetchar	; blocks so far
		sta xmodemblkcount	; store the number of blocks
		lbsr serialgetchar	; 255 less blocks so far

		clr xmodemchecksum	; clear the checksum

		ldb #0x80		; 128 bytes per block
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

		leax -0x80,x		; move back to start of the block
		
		bra blockloop		; try to get the same block again

xmodemout:	lda #ACK		; at end of file, ACK the whole file
		lbsr serialputchar

		

		clra
		rts

xmodemerr:	lda #1
		rts		

;;; END OF HIGH LEVEL COMMANDS

; flasher

flasher:	ldx #flashreadymsg	; tell other end it can send now
		lbsr serialputstr

; read 64bytes from the serial port into ram

		ldy #ROMSTART		; setup the counter into rom
inflashblk:	ldx #flashblock		; this is the block in ram we...
		ldb #64			; are copying into
inflash:	lbsr serialgetchar	; get the byte from the port
		sta ,x+			; store it
		decb			; we store 64bytes
		bne inflash		; back to the next byte

; then write them into rom

		ldx #flashblock		; after we have a block
		ldb #64			; of 64 bytes
outflash:	lda ,x+			; get the byte from ram
		sta ,y+			; and write it into the rom
		decb			; reduce byte counter
		bne outflash		; back for more if non zero

; circa 10ms delay between blocks

		ldx #20000		; setup delay counter
flashdelayloop:	leax -1,x		; dey
		bne flashdelayloop

; tell the uploader that we have written the block, and it can send the
; next one

		lda #0x23		; '#'
		lbsr serialputchar	; send the char
		cmpy #ROMEND+1		; see if we are the end of rom
		bne inflashblk		; back to the next block

; send the content of the rom back, so the sender knows what was written -
; we can't do anything if it didn't write, but at least we know

		ldy #ROMSTART		; back to the start
verflash:	lda ,y+			; get the byte
		lbsr serialputchar	; output it
		cmpy #ROMEND+1
		bne verflash

; we could in theory try again but good or bad, do a reset on the new
; reset vector
		
		jmp [0xfffe]		; jump through the new reset vector

; include the various subsystem implementations

		.include 'storage.asm'
		.include 'spi.asm'
		.include 'serial.asm'
		.include 'strings.asm'
		.include 'misc.asm'
		.include 'ay8910.asm'
