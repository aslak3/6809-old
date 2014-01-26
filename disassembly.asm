;;; DISASSEMBLER

;;; Big old mnemonic table

abxn:		.asciz 'ABX'
adcan:		.asciz 'ADCA'
adcbn:		.asciz 'ADCB'
addan:		.asciz 'ADDA'
addbn:		.asciz 'ADDB'
adddn:		.asciz 'ADDD'
andan:		.asciz 'ANDA'
andbn:		.asciz 'ANDB'
andccn:		.asciz 'ANDCC'
asrn:		.asciz 'ASR'
asran:		.asciz 'ASRA'
asrbn:		.asciz 'ASRB'
beqn:		.asciz 'BEQ'
bgen:		.asciz 'BGE'
bgtn:		.asciz 'BGT'
bhin:		.asciz 'BHI'
bhsn:		.asciz 'BHS'
bitan:		.asciz 'BITA'
bitbn:		.asciz 'BITB'
blen:		.asciz 'BLE'
blon:		.asciz 'BLO'
blsn:		.asciz 'BLS'
bltn:		.asciz 'BLT'
bmin:		.asciz 'BMI'
bnen:		.asciz 'BNE'
bpln:		.asciz 'BPL'
bran:		.asciz 'BRA'
brnn:		.asciz 'BRN'
bsrn:		.asciz 'BSR'
bvcn:		.asciz 'BVC'
bvsn:		.asciz 'BVS'
clrn:		.asciz 'CLR'
clran:		.asciz 'CLRA'
clrbn:		.asciz 'CLRB'
cmpan:		.asciz 'CMPA'
cmpbn:		.asciz 'CMPB'
cmpdn:		.asciz 'CMPD'
cmpsn:		.asciz 'CMPS'
cmpun:		.asciz 'CMPU'
cmpxn:		.asciz 'CMPX'
cmpyn:		.asciz 'CMPY'
comn:		.asciz 'COM'
coman:		.asciz 'COMA'
combn:		.asciz 'COMB'
cwain:		.asciz 'CWAI'
daan:		.asciz 'DAA'
decn:		.asciz 'DEC'
decan:		.asciz 'DECA'
decbn:		.asciz 'DECB'
eoran:		.asciz 'EORA'
eorbn:		.asciz 'EORB'
exgn:		.asciz 'EXG'
incn:		.asciz 'INC'
incan:		.asciz 'INCA'
incbn:		.asciz 'INCB'
jmpn:		.asciz 'JMP'
jsrn:		.asciz 'JSR'
lbeqn:		.asciz 'LBEQ'
lbgen:		.asciz 'LBGE'
lbgtn:		.asciz 'LBGT'
lbhin:		.asciz 'LBHI'
lbhsn:		.asciz 'LBHS'
lblen:		.asciz 'LBLE'
lblon:		.asciz 'LBLO'
lblsn:		.asciz 'LBLS'
lbltn:		.asciz 'LBLT'
lbmin:		.asciz 'LBMI'
lbnen:		.asciz 'LBNE'
lbpln:		.asciz 'LBPL'
lbran:		.asciz 'LBRA'
lbrnn:		.asciz 'LBRN'
lbsrn:		.asciz 'LBSR'
lbvcn:		.asciz 'LBVC'
lbvsn:		.asciz 'LBVS'
ldan:		.asciz 'LDA'
ldbn:		.asciz 'LDB'
lddn:		.asciz 'LDD'
ldsn:		.asciz 'LDS'
ldun:		.asciz 'LDU'
ldxn:		.asciz 'LDX'
ldyn:		.asciz 'LDY'
leasn:		.asciz 'LEAS'
leaun:		.asciz 'LEAU'
leaxn:		.asciz 'LEAX'
leayn:		.asciz 'LEAY'
lslan:		.asciz 'LSLA'
lsln:		.asciz 'LSL'
lslbn:		.asciz 'LSLB'
lsrn:		.asciz 'LSR'
lsran:		.asciz 'LSRA'
lsrbn:		.asciz 'LSRB'
muln:		.asciz 'MUL'
negn:		.asciz 'NEG'
negan:		.asciz 'NEGA'
negbn:		.asciz 'NEGB'
nopn:		.asciz 'NOP'
oran:		.asciz 'ORA'
orbn:		.asciz 'ORB'
orccn:		.asciz 'ORCC'
pshsn:		.asciz 'PSHS'
pshun:		.asciz 'PSHU'
pulsn:		.asciz 'PULS'
pulun:		.asciz 'PULU'
roln:		.asciz 'ROL'
rolan:		.asciz 'ROLA'
rolbn:		.asciz 'ROLB'
rorn:		.asciz 'ROR'
roran:		.asciz 'RORA'
rorbn:		.asciz 'RORB'
rtin:		.asciz 'RTI'
rtsn:		.asciz 'RTS'
sbcan:		.asciz 'SBCA'
sbcbn:		.asciz 'SBCB'
sexn:		.asciz 'SEX'
stan:		.asciz 'STA'
stbn:		.asciz 'STB'
stdn:		.asciz 'STD'
stsn:		.asciz 'STS'
stun:		.asciz 'STU'
stxn:		.asciz 'STX'
styn:		.asciz 'STY'
suban:		.asciz 'SUBA'
subbn:		.asciz 'SUBB'
subdn:		.asciz 'SUBD'
swin:		.asciz 'SWI'
swi2n:		.asciz 'SWI2'
swi3n:		.asciz 'SWI3'
syncn:		.asciz 'SYNC'
tfrn:		.asciz 'TFR'
tstn:		.asciz 'TST'
tstan:		.asciz 'TSTA'
tstbn:		.asciz 'TSTB'

;;; Machine code instruction tables

; first struct is one with a first nibble prefix filter

.macro		prefix opcodeprefix, opcodetab, addrmodehandler
		.byte opcodeprefix	; low nibble in opcode
		.word opcodetab		; opcode table
		.word addrmodehandler	; post opcode bytes handler
.endm

; second struct has no prefix and is just a list of any opcodes

.macro		noprefix opcodetab, addrmodehandler
		.word opcodetab		; opcode table
		.word addrmodehandler	; post opcode bytes handler
.endm

; this is an actual list of opcodes

.macro		mc opcode, mn
		.byte opcode		; opcode byte value (maybe masked)
		.word mn		; opcode asciz string
.endm

; we sort these by addressing mode. opcode "blocks" make up the bulk of the
; number space, luckily

prefixtab:	prefix 0xb0, regaopstab, extendedhandle
		prefix 0xf0, regbopstab, extendedhandle
		prefix 0x70, memopstab, extendedhandle

		prefix 0x00, memopstab, directhandle
		prefix 0x90, regaopstab, directhandle
		prefix 0xd0, regbopstab, directhandle

		prefix 0x80, regaopstab, immedhandle
		prefix 0xc0, regbopstab, immedhandle

		prefix 0x60, memopstab, indexedhandle
		prefix 0xa0, regaopstab, indexedhandle
		prefix 0xe0, regbopstab, indexedhandle

		prefix 0xff, 0x0000, 0x0000

; these are the "randoms" which have no prefix block

noprefixtab:	noprefix inherenttab, nullhandle
		noprefix indexedtab, indexedhandle
		noprefix immedtab, immedhandle
		noprefix regimmedtab, regimmedhandle
		noprefix stkimmedtab, stkimmedhandle
		noprefix relbytetab, relbytehandle
		noprefix relwordtab, relwordhandle
		noprefix 0xffff, 0x0000

; inherent opcode table - opcodes with no parameter - not masked

inherenttab:	mc 0x12, nopn
		mc 0x13, syncn
		mc 0x19, daan
		mc 0x1d, sexn
		mc 0x39, rtsn
		mc 0x3a, abxn
		mc 0x3b, rtin
		mc 0x3d, muln
		mc 0x3f, swin
		mc 0x40, negan
		mc 0x43, coman
		mc 0x44, lsran
		mc 0x46, roran
		mc 0x47, asran
		mc 0x48, lslan
		mc 0x49, rolan
		mc 0x4a, decan
		mc 0x4c, incan
		mc 0x4d, tstan
		mc 0x4f, clran
		mc 0x50, negbn
		mc 0x53, combn
		mc 0x54, lsrbn
		mc 0x56, rorbn
		mc 0x57, asrbn
		mc 0x58, lslbn
		mc 0x59, rolbn
		mc 0x5a, decbn
		mc 0x5c, incbn
		mc 0x5d, tstbn
		mc 0x5f, clrbn
		mc 0xff, 0x0000

; special indexed opcode - no mask

indexedtab:	mc 0x30, leaxn
		mc 0x31, leayn
		mc 0x32, leasn
		mc 0x33, leaun
		mc 0xff, 0x0000

; special imediate (1 byte) opcodes - no mask

immedtab:	mc 0x1a, orccn
		mc 0x1c, andccn
		mc 0x3c, cwain
		mc 0xff, 0x0000

; special immediate (1 byte) opcodes used in reg moving - no mask

regimmedtab:	mc 0x1e, exgn
		mc 0x1f, tfrn
		mc 0xff, 0x0000

; special immediate (1 byte) opcodes sed in stacking - no mask

stkimmedtab:	mc 0x34, pshsn
		mc 0x35, pulsn
		mc 0x36, pshun
		mc 0x37, pulun
		mc 0xff, 0x0000

; special relative branches (1 byte offset)

relbytetab:	mc 0x20, bran
		mc 0x21, brnn
		mc 0x22, bhin
		mc 0x23, blsn
		mc 0x24, bhsn
		mc 0x25, blon
		mc 0x26, bnen
		mc 0x27, beqn
		mc 0x28, bvcn
		mc 0x29, bvsn
		mc 0x2a, bpln
		mc 0x2b, bmin
		mc 0x2c, bgen
		mc 0x2d, bltn
		mc 0x2e, bgtn
		mc 0x2f, blen
		mc 0x8d, bsrn
		mc 0xff, 0x0000

relwordtab:	mc 0x16, lbran
		mc 0x17, lbsrn
		mc 0xff, 0x0000

memopstab:	mc 0x00, negn
		mc 0x03, comn
		mc 0x04, lsrn
		mc 0x06, rorn
		mc 0x07, asrn
		mc 0x08, lsln
		mc 0x09, roln
		mc 0x0a, decn
		mc 0x0c, incn
		mc 0x0d, tstn
		mc 0x0e, jmpn
		mc 0x0f, clrn
		mc 0xff, 0x0000

regaopstab:	mc 0x00, suban
		mc 0x01, cmpan
		mc 0x02, sbcan
		mc 0x03, subdn
		mc 0x04, andan
		mc 0x05, bitan
		mc 0x06, ldan
		mc 0x07, stan
		mc 0x08, eoran
		mc 0x09, adcan
		mc 0x0a, oran
		mc 0x0b, addan
		mc 0x0c, cmpxn
		mc 0x0d, jsrn
		mc 0x0e, ldxn
		mc 0x0f, stxn
		mc 0xff, 0x0000

regbopstab:	mc 0x00, subbn
		mc 0x01, cmpbn
		mc 0x02, sbcbn
		mc 0x03, adddn
		mc 0x04, andbn
		mc 0x05, bitbn
		mc 0x06, ldbn
		mc 0x07, stbn
		mc 0x08, eorbn
		mc 0x09, adcbn
		mc 0x0a, orbn
		mc 0x0b, addbn
		mc 0x0c, lddn
		mc 0x0d, stdn
		mc 0x0e, ldun
		mc 0x0f, stun
		mc 0xff, 0x0000

; disassemble from u bytes

spacemsg:	.asciz ' '
ehmsg:		.asciz '???'
colonmsg:	.asciz ':'
commamsg:	.asciz ','
hexmsg:		.asciz '$'
directhexmsg:	.asciz '<$'
immedhexmsg:	.asciz '#$'

ccregmsg:	.asciz 'CC'
aregmsg:	.asciz 'A'
bregmsg:	.asciz 'B'
dpregmsg:	.asciz 'DP'
dregmsg:	.asciz 'D'
xregmsg:	.asciz 'X'
yregmsg:	.asciz 'Y'
sregmsg:	.asciz 'S'
uregmsg:	.asciz 'U'
pcregmsg:	.asciz 'PC'

disassentry:	lbsr outputinit		; setup the buffer and pointer

		stu statementstart	; save the address of the first byte
		tfr u,d			; get address of this line
		lbsr wordtoaschex	; print the address into the buffer
		stx outputpointer	; save it away
		ldx #colonmsg		; this is the "label"
		lbsr outputappend	; so it needs a colon
		ldx #spacemsg		; and a space would be nice
		lbsr outputappend	; add it
		lbsr outputappend	; and one more
		
		lda ,u+			; get the opcode byte
		sta opcode		; save it (we'll need it later)

		ldy #prefixtab		; set up our table of prefixes
		anda #0xf0		; we just want the prefix

prefixloop:	ldb ,y			; get this prefix
		cmpb #0xff		; 0xff marks the end of the table
		beq prefixout		; if we are at the end, leave
		cmpa ,y+		; and see if it matches the prefix
		bne prefixnotfound	; not found, so onward to next one

		lda opcode		; our opcode has the right prefix
		anda #0x0f		; mask off the interesting part
		lbsr procaddrmode	; and process the opcode table
		beq noprefixout		; if zero then we proc'd a opcode
		
prefixnotfound:	leay 4,y		; hop over to the next prefix
		bra prefixloop		; and go back for more

prefixout:	ldy #noprefixtab	; set up the non prefix table
		lda opcode		; get the full opcode back
		
noprefixloop:	ldx ,y			; get the opcode pointer
		cmpx #0xffff		; 0xffff marks this one's end
		beq nomatch		; if we are at the end, leave
		lbsr procaddrmode	; process this opcode table
		beq noprefixout		; if zero then we proc'd a opcode
		leay 4,y		; y points to the noprefix table
		bra noprefixloop	; not the opcode table

nomatch:	ldx #ehmsg
		lbsr outputappend

noprefixout:	stu statementend	; save the end address

		lda #40
		lbsr outputmoveto

		ldy statementstart

rawbyteloop:	lda ,y+
		ldx outputpointer
		lbsr bytetoaschex
		stx outputpointer

		ldx #spacemsg
		lbsr outputappend

		cmpy statementend
		bne rawbyteloop

		lda #60
		lbsr outputmoveto

		ldy statementstart

rawbyteascloop:	lda ,y+
		lbsr printableasc
		lbsr outputasc
		
		cmpy statementend
		bne rawbyteascloop

		ldx #newlinemsg		; we want a nice newline
		lbsr outputappend
		ldx outputpointer
		clr ,x

		ldx #outputbuffer	; send the whole line
		lbsr serialputstr	; to the terminal

		ldx disasscounter	; decrement the opcode
		leax -1,x		; ..
		stx disasscounter	; counter

		lbne disassentry	; back for more opcodes, maybe

		rts


; process a table of opcodes. y will point at the [no]prefix "description" -
; first word is the opcode table pointer, and 2nd is the handler for
; subsequent bytes

procaddrmode:	sty addrmode		; save the description pointer
		ldy ,y			; deref into te opcode table
procaddrloop:	ldb ,y			; load the first opcode
		cmpb #0xff		; end of the opcode table?
		beq procaddrmodeo	; if so exit
		cmpa ,y+		; matches our (maybe maked) opcode?
		beq opcodefound		; if so deal with it
		leay 2,y		; if not we need to hop the mn...
		bra procaddrloop	; and go back for more
opcodefound:	ldx ,y			; if it did, deref to mn string
		lbsr outputappend	; and append it
		ldx #spacemsg		; and follow with a space
		lbsr outputappend	; ...
		ldy addrmode		; restore the original desc pointer
		leay 2,y		; move along to the handler pointer
		ldx ,y			; which must be de-referened
;		ldx [2,y]		; derefence into the handler pointer
		jsr ,x			; and call it
		orcc #0x04		; we processed an opcode -> set zero
		rts			; so we can now output the line
procaddrmodeo:	ldy addrmode		; restore once more so caller
		andcc #0xfb		; and mark it as not proc'd -> !zero
		rts

; post opcode bytes handler that does nothing at all

nullhandle:	rts			; dummy for immediates

; post opode byte handler for single byte offsets

relbytehandle:	ldx #hexmsg		; $ etc
		lbsr outputappend	; append that
		lda ,u+			; get the byte offset
		leax a,u
		tfr x,d
		ldx outputpointer	; grab the curent postion
		lbsr wordtoaschex	; print the offset into the buffer
		stx outputpointer	; and save the new one
		rts

; post opode byte handler for word offsets

relwordhandle:	ldx #hexmsg		; $ etc
		lbsr outputappend	; append that
		ldd ,u++		; get the byte offset
		leax d,u
		tfr x,d
		ldx outputpointer	; grab the curent postion
		lbsr wordtoaschex	; print the offset into the buffer
		stx outputpointer	; and save the new one
		rts

; post opcode bytes handler for word address

extendedhandle:	ldx #hexmsg		; #$ etc
		lbsr outputappend	; append that
		ldd ,u++		; get the word, advancing u
		ldx outputpointer	; grab the current position
		lbsr wordtoaschex	; print the address into the buffer
		stx outputpointer	; and save the new one
		rts

directhandle:	ldx #directhexmsg	; <$ etc
		lbsr outputappend	; append that
		lda ,u+			; get the byte address, advancing u
		ldx outputpointer	; grab the current position
		lbsr bytetoaschex	; print the address into the buffer
		stx outputpointer	; and save the new one
		rts

immedhandle:	ldx #immedhexmsg	; #$ etc
		lbsr outputappend	; append that
		ldx outputpointer	; grab the current position
		lda opcode		; get the original opcode
		anda #0x0f		; mask off the low byte
		cmpa #0x03		; this is subd, 2 bytes
		beq immedword		; ...
		cmpa #0x0c		; cmpx is also 2 bytes
		beq immedword		; ...
		cmpa #0x0e		; ldx is also 2 bytes
		beq immedword		; ...
		lda ,u+			; otherwise it must be 1 byte
		lbsr bytetoaschex	; convert it
immedout:	stx outputpointer	; update the pointer
		rts			; done
immedword:	ldd ,u++		; for words, grab it to d
		lbsr wordtoaschex	; convert it
		bra immedout		; and back out the common path

; this is for exg and tfr - ehmsg is ??? - the index is the post byte,
; masked for source and destination

regmovetab:	.word dregmsg, xregmsg, yregmsg, uregmsg
		.word sregmsg, pcregmsg, ehmsg, ehmsg
		.word aregmsg, bregmsg, ccregmsg, dpregmsg
		.word ehmsg, ehmsg, ehmsg, ehmsg

regimmedhandle:	ldb ,u			; get the post opcode byte
		ldy #regmovetab		; init the table pointer
		andb #0xf0		; mask off the high nibble
		lsrb			; shift it down four times
		lsrb			; ...
		lsrb			; ...
		lsrb			; ...
		lslb			; then back one, we have word ptrs
		leax [b,y]		; add to the table and deref
		lbsr outputappend	; this is the source
		ldx #commamsg		; and a comma
		lbsr outputappend	; ...
		ldb ,u+			; now get the original postbyte
		andb #0x0f		; and mask off the low nibble
		lslb			; times 2 because of word ptrs
		leax [b,y]		; deref again to get the destination
		lbsr outputappend	; ...
		rts

; stacking - pshs, puls, pshu, pulu - the table is a table of bits this time
; not offsets

sstackingtab:	.word ccregmsg, aregmsg, bregmsg, dpregmsg
		.word xregmsg, yregmsg, uregmsg, pcregmsg
ustackingtab:	.word ccregmsg, aregmsg, bregmsg, dpregmsg
		.word xregmsg, yregmsg, sregmsg, pcregmsg

stkimmedhandle: ldb #8			; 8 shifts for a byte
		lda opcode		; get the original opcode
		bita #0x02		; bit 1 0 means s stacking , else u
		beq sstacking		; it is pshs or puls
		ldy #ustackingtab	; so set the table pointer
		bra stackingstart	; hop hop
sstacking:	ldy #sstackingtab	; it is pshu or pulu
stackingstart:	lda ,u			; advance at the end not now
stackingloop:	rora			; rorate into carry
		bcs stackingit		; if 1, then we are stacking
endstacking:	leay 2,y		; eitherway move index along
		decb			; and decrement bit counter
		bne stackingloop	; see if there is more bits
		tst ,u+			; now we can advance to next opcode
		beq stkimmedout		; if there was nothing to stack
		lbsr outputbackone	; don't move the cursor back one
stkimmedout:	rts			; out
stackingit:	ldx ,y			; deref to get the reg string
		lbsr outputappend	; append the reg name
		ldx #commamsg		; and we need a comma
		lbsr outputappend	; so add that
		bra endstacking		; back to common path

; TODO

indexedhandle:	ldd ,u+
		rts

;;;

outputinit:	ldx #outputbuffer
		lda #80				; fill width with spaces
		ldb #0x20			; space
outputinitloop:	stb ,x+
		deca
		bne outputinitloop
		clr ,x+				; add the terminator
		ldx #outputbuffer		; reset the buffer pointer
		stx outputpointer
		rts

outputappend:	pshs x,y,a
		tfr x,y
		ldx outputpointer
		lbsr concatstr
		stx outputpointer
		puls x,y,a
		rts

outputasc:	pshs x
		ldx outputpointer
		sta ,x+
		stx outputpointer
		puls x
		rts

outputbackone:	pshs x,a
		ldx outputpointer
		lda #0x20
		sta ,-x				; blank the last char
		stx outputpointer
		puls x,a
		rts

outputmoveto:	ldx #outputbuffer
		leax a,x
		stx outputpointer
		rts