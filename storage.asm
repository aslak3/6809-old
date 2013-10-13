;;; IDE

; run the trivial command in 'a', assuming other params are setup

simpleidecomm:	ldb #0b11100000
		stb IDEHEADS
		sta IDECOMMAND
		clra
		rts

idewaitnotbusy:	lda IDESTATUS
		anda #0x80
		bne idewaitnotbusy
		rts

idewaitfordata:	lda IDESTATUS
		anda #0x08
		beq idewaitfordata
		rts

idellread:	ldy #512
		lbsr idewaitnotbusy
		lbsr idewaitfordata
readbyteloop:	lda IDEDATA
		sta ,x+
		leay -1,y
		bne readbyteloop
		rts

partstartmsg:	.asciz 'Partition starts at: '
nombrmsg:	.asciz 'No MBR found, assuming single partition\r\n'

idemount:	lda #0x01		; 8 bit enable
		sta IDEFEATURES

		lda #0xef		; set features
		lbsr simpleidecomm
		lbsr idewaitnotbusy

		clr IDELBA0
		clr IDELBA1
		clr IDELBA2
		clr IDELBA3

		lda #1
		sta IDECOUNT

		lda #0x20
		lbsr simpleidecomm

		ldx #idembrsec
		lbsr idellread

		ldx #idembrsec+0x01fe
		lbsr wordswap
		ldd idembrsec+0x01fe
		cmpd #0xaa55
		beq idemountmbr

		ldx #nombrmsg
		lbsr serialputstr

		clra
		clrb
		std firstpartsects

		rts

idemountmbr:	ldx #idembrsec+0x01be+0x08
		lbsr wordswap
		ldd idembrsec+0x01be+0x08
		std firstpartsects

		ldx #partstartmsg
		ldy #firstpartsects
		lbsr serialputlab

		rts

;;; FS

; readblk - reads 1k block at y into block buffer at x

fsreadblk:	pshs d
		tfr y,d
		lslb
		rola			; multiple x by 2->sectors
		addd firstpartsects	; add sector offset for the partition
		
		stb IDELBA0
		sta IDELBA1
		clr IDELBA2
		clr IDELBA3

		lda #2
		sta IDECOUNT

		lda #0x20
		lbsr simpleidecomm

		lbsr idellread
		lbsr idellread

		puls d

		rts

; reads an inode y into memory at x

fsreadinode:	pshs x,y,d
		stx inodeptr
		tfr y,d
		subd #1			; inodes start from 1
		pshs b
		lbsr div32
		addd startofinodes

		ldx #scratchblk
		tfr d,y
		lbsr fsreadblk

		ldx inodeptr
		ldy #scratchblk

		clra
		puls b			; get original (low byte) inode back
		andb #0b00011111		; mask off the inode position in block
		lbsr mul32		; d now has byte offset start of inode
		leay d,y
		ldb #0x20		; copy 20 bytes
moreinode:	lda ,y+
		sta ,x+
		decb
		bne moreinode

		ldx inodeptr		; type+mode
		lbsr wordswap
		leax 4,x		; low word of size
		lbsr wordswap
		
		ldx inodeptr
		leax 14,x
		ldb #7
datawordswaps:	lbsr wordswap
		leax 2,x
		decb
		bne datawordswaps

		puls x,y,d

		rts

; reads all (upto 7 blocks) the data for the current inode into x

fsreaddata:	ldb #7
		ldu #inode+14
moredatablks:	ldy ,u++
		beq fsreaddataout
		lbsr fsreadblk
		decb
		bne moredatablks

fsreaddataout:	rts

; reads the file at inode y into x, using the inode and data read functions

fsreadfile:	pshs x,y
		ldx #inode
		lbsr fsreadinode

		puls x,y
		lbsr fsreaddata

		rts		

; outputs a file listing for the inode at dirinode

fsshowdirlist:	ldx #dirinode
		lbsr fsreadinode
		
		ldd dirinode
		anda #0x40
		cmpa #0x40
		beq showdirisdir
		lda #1
		rts

showdirisdir:	ldb #7
		ldu #dirinode+14

nextdirblk:	ldy ,u++
		beq fsshowdirlisto
		ldx #scratchdirblk
		lbsr fsreadblk
		lbsr fsshowdirblk
		decb
		bne nextdirblk

fsshowdirlisto:	rts
		
fsshowdirblk: 	pshs d,x,y,u
		ldu #scratchdirblk
nextdirent:	leax ,u++
		lbsr wordswap
		ldd ,x
		beq skipdir
		ldx #inode
		tfr d,y
		lbsr fsreadinode
		ldx #outputbuffer
		lbsr wordtoaschex	; convert it to ascii
		lda #0x20		; space char
		sta ,x+
		ldd inode
		lbsr wordtoaschex
		lda #0x20
		sta ,x+
		ldd inode+4
		lbsr wordtoaschex
		lda #0x20
		sta ,x+
		tfr u,y
		lbsr concatstr
		ldy #newlinemsg
		lbsr concatstr
		clr ,x+
		ldx #outputbuffer
		lbsr serialputstr
skipdir:	leau 30,u
		cmpu #scratchdirblk+1024
		bne nextdirent
		puls d,x,y,u
		rts
