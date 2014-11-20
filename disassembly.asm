		section _main

;;; DISASSEMBLER

;;; Big old mnemonic table

abxn:		fcn 'ABX'
adcan:		fcn 'ADCA'
adcbn:		fcn 'ADCB'
addan:		fcn 'ADDA'
addbn:		fcn 'ADDB'
adddn:		fcn 'ADDD'
andan:		fcn 'ANDA'
andbn:		fcn 'ANDB'
andccn:		fcn 'ANDCC'
asrn:		fcn 'ASR'
asran:		fcn 'ASRA'
asrbn:		fcn 'ASRB'
beqn:		fcn 'BEQ'
bgen:		fcn 'BGE'
bgtn:		fcn 'BGT'
bhin:		fcn 'BHI'
bhsn:		fcn 'BHS'
bitan:		fcn 'BITA'
bitbn:		fcn 'BITB'
blen:		fcn 'BLE'
blon:		fcn 'BLO'
blsn:		fcn 'BLS'
bltn:		fcn 'BLT'
bmin:		fcn 'BMI'
bnen:		fcn 'BNE'
bpln:		fcn 'BPL'
bran:		fcn 'BRA'
brnn:		fcn 'BRN'
bsrn:		fcn 'BSR'
bvcn:		fcn 'BVC'
bvsn:		fcn 'BVS'
clrn:		fcn 'CLR'
clran:		fcn 'CLRA'
clrbn:		fcn 'CLRB'
cmpan:		fcn 'CMPA'
cmpbn:		fcn 'CMPB'
cmpdn:		fcn 'CMPD'
cmpsn:		fcn 'CMPS'
cmpun:		fcn 'CMPU'
cmpxn:		fcn 'CMPX'
cmpyn:		fcn 'CMPY'
comn:		fcn 'COM'
coman:		fcn 'COMA'
combn:		fcn 'COMB'
cwain:		fcn 'CWAI'
daan:		fcn 'DAA'
decn:		fcn 'DEC'
decan:		fcn 'DECA'
decbn:		fcn 'DECB'
eoran:		fcn 'EORA'
eorbn:		fcn 'EORB'
exgn:		fcn 'EXG'
incn:		fcn 'INC'
incan:		fcn 'INCA'
incbn:		fcn 'INCB'
jmpn:		fcn 'JMP'
jsrn:		fcn 'JSR'
lbeqn:		fcn 'LBEQ'
lbgen:		fcn 'LBGE'
lbgtn:		fcn 'LBGT'
lbhin:		fcn 'LBHI'
lbhsn:		fcn 'LBHS'
lblen:		fcn 'LBLE'
lblon:		fcn 'LBLO'
lblsn:		fcn 'LBLS'
lbltn:		fcn 'LBLT'
lbmin:		fcn 'LBMI'
lbnen:		fcn 'LBNE'
lbpln:		fcn 'LBPL'
lbran:		fcn 'LBRA'
lbrnn:		fcn 'LBRN'
lbsrn:		fcn 'LBSR'
lbvcn:		fcn 'LBVC'
lbvsn:		fcn 'LBVS'
ldan:		fcn 'LDA'
ldbn:		fcn 'LDB'
lddn:		fcn 'LDD'
ldsn:		fcn 'LDS'
ldun:		fcn 'LDU'
ldxn:		fcn 'LDX'
ldyn:		fcn 'LDY'
leasn:		fcn 'LEAS'
leaun:		fcn 'LEAU'
leaxn:		fcn 'LEAX'
leayn:		fcn 'LEAY'
lslan:		fcn 'LSLA'
lsln:		fcn 'LSL'
lslbn:		fcn 'LSLB'
lsrn:		fcn 'LSR'
lsran:		fcn 'LSRA'
lsrbn:		fcn 'LSRB'
muln:		fcn 'MUL'
negn:		fcn 'NEG'
negan:		fcn 'NEGA'
negbn:		fcn 'NEGB'
nopn:		fcn 'NOP'
oran:		fcn 'ORA'
orbn:		fcn 'ORB'
orccn:		fcn 'ORCC'
pshsn:		fcn 'PSHS'
pshun:		fcn 'PSHU'
pulsn:		fcn 'PULS'
pulun:		fcn 'PULU'
roln:		fcn 'ROL'
rolan:		fcn 'ROLA'
rolbn:		fcn 'ROLB'
rorn:		fcn 'ROR'
roran:		fcn 'RORA'
rorbn:		fcn 'RORB'
rtin:		fcn 'RTI'
rtsn:		fcn 'RTS'
sbcan:		fcn 'SBCA'
sbcbn:		fcn 'SBCB'
sexn:		fcn 'SEX'
stan:		fcn 'STA'
stbn:		fcn 'STB'
stdn:		fcn 'STD'
stsn:		fcn 'STS'
stun:		fcn 'STU'
stxn:		fcn 'STX'
styn:		fcn 'STY'
suban:		fcn 'SUBA'
subbn:		fcn 'SUBB'
subdn:		fcn 'SUBD'
swin:		fcn 'SWI'
swi2n:		fcn 'SWI2'
swi3n:		fcn 'SWI3'
syncn:		fcn 'SYNC'
tfrn:		fcn 'TFR'
tstn:		fcn 'TST'
tstan:		fcn 'TSTA'
tstbn:		fcn 'TSTB'

;;; Machine code instruction tables

; first struct is a page definition (default or page 10, 11 etc)

pagedef		macro NOEXPAND
		fcb {1}
		fdb {2}
		endm

; second struct has no prefix and is just a list of any opcodes

noprefix	macro NOEXPAND
		fdb {1}		; opcode table
		fdb {2}		; post opcode bytes handler
		endm

; third struct is one with a first nibble prefix filter

prefix		macro NOEXPAND
		fcb {1}		; low nibble in opcode
		fdb {2}		; opcode table
		fdb {3}		; post opcode bytes handler
		endm

; this is an actual opcodes: binary value to opcode string

mc		macro NOEXPAND
		fcb {1}		; opcode byte value (maybe masked)
		fdb {2}		; opcode asciz string
		endm

; all the pages, mapping the first opcode byte to a page

pagedefs:	pagedef $10,page10
		pagedef $11,page11
		pagedef $00,$00

; "pages" are collections of noprefix'ed and prefix'ed tables

defaultpage:	fdb noprefixtab,prefixtab
page10:		fdb p10noprefixtab,p10prefixtab
page11:		fdb p11noprefixtab,p11prefixtab

; these are the "randoms" which have no prefix block

noprefixtab:	noprefix inherenttab,nullhandle
		noprefix indexedtab,indexedhandle
		noprefix immedtab,immedhandle
		noprefix regimmedtab,regimmedhandle
		noprefix stkimmedtab,stkimmedhandle
		noprefix relbytetab,relbytehandle
		noprefix relwordtab,relwordhandle
		noprefix $ffff,$0000

; we sort these by addressing mode. opcode "blocks" make up the bulk of the
; number space, luckily

prefixtab:	prefix $b0,regaopstab,extendedhandle
		prefix $f0,regbopstab,extendedhandle
		prefix $70,memopstab,extendedhandle

		prefix $00,memopstab,directhandle
		prefix $90,regaopstab,directhandle
		prefix $d0,regbopstab,directhandle

		prefix $80,regaopstab,immedhandle
		prefix $c0,regbopstab,immedhandle

		prefix $60,memopstab,indexedhandle
		prefix $a0,regaopstab,indexedhandle
		prefix $e0,regbopstab,indexedhandle

		prefix $ff,$0000,$0000

; inherent opcode table - opcodes with no parameter - not masked

inherenttab:	mc $12,nopn
		mc $13,syncn
		mc $19,daan
		mc $1d,sexn
		mc $39,rtsn
		mc $3a,abxn
		mc $3b,rtin
		mc $3d,muln
		mc $3f,swin
		mc $40,negan
		mc $43,coman
		mc $44,lsran
		mc $46,roran
		mc $47,asran
		mc $48,lslan
		mc $49,rolan
		mc $4a,decan
		mc $4c,incan
		mc $4d,tstan
		mc $4f,clran
		mc $50,negbn
		mc $53,combn
		mc $54,lsrbn
		mc $56,rorbn
		mc $57,asrbn
		mc $58,lslbn
		mc $59,rolbn
		mc $5a,decbn
		mc $5c,incbn
		mc $5d,tstbn
		mc $5f,clrbn
		mc $ff,$0000

; special indexed opcode - no mask

indexedtab:	mc $30,leaxn
		mc $31,leayn
		mc $32,leasn
		mc $33,leaun
		mc $ff,$0000

; special imediate (1 byte) opcodes - no mask

immedtab:	mc $1a,orccn
		mc $1c,andccn
		mc $3c,cwain
		mc $ff,$0000

; special immediate (1 byte) opcodes used in reg moving - no mask

regimmedtab:	mc $1e,exgn
		mc $1f,tfrn
		mc $ff,$0000

; special immediate (1 byte) opcodes used in stacking - no mask

stkimmedtab:	mc $34,pshsn
		mc $35,pulsn
		mc $36,pshun
		mc $37,pulun
		mc $ff,$0000

; special relative branches (1 byte offset)

relbytetab:	mc $20,bran
		mc $21,brnn
		mc $22,bhin
		mc $23,blsn
		mc $24,bhsn
		mc $25,blon
		mc $26,bnen
		mc $27,beqn
		mc $28,bvcn
		mc $29,bvsn
		mc $2a,bpln
		mc $2b,bmin
		mc $2c,bgen
		mc $2d,bltn
		mc $2e,bgtn
		mc $2f,blen
		mc $8d,bsrn
		mc $ff,$0000

; same again,but two bytes - still no mask

relwordtab:	mc $16,lbran
		mc $17,lbsrn
		mc $ff,$0000

; suffix (multiple prefixes) block of memery operations

memopstab:	mc $00,negn
		mc $03,comn
		mc $04,lsrn
		mc $06,rorn
		mc $07,asrn
		mc $08,lsln
		mc $09,roln
		mc $0a,decn
		mc $0c,incn
		mc $0d,tstn
		mc $0e,jmpn
		mc $0f,clrn
		mc $ff,$0000

; stuff that operates on the a register is common between 3 addr mode

regaopstab:	mc $00,suban
		mc $01,cmpan
		mc $02,sbcan
		mc $03,subdn
		mc $04,andan
		mc $05,bitan
		mc $06,ldan
		mc $07,stan
		mc $08,eoran
		mc $09,adcan
		mc $0a,oran
		mc $0b,addan
		mc $0c,cmpxn
		mc $0d,jsrn		; $8d is bsr with no prefix
		mc $0e,ldxn
		mc $0f,stxn
		mc $ff,$0000

; stuff that operates on the b register is common too

regbopstab:	mc $00,subbn
		mc $01,cmpbn
		mc $02,sbcbn
		mc $03,adddn
		mc $04,andbn
		mc $05,bitbn
		mc $06,ldbn
		mc $07,stbn
		mc $08,eorbn
		mc $09,adcbn
		mc $0a,orbn
		mc $0b,addbn
		mc $0c,lddn
		mc $0d,stdn
		mc $0e,ldun
		mc $0f,stun
		mc $ff,$0000

; page 10 noprefixe and prefixe tables

p10noprefixtab:	noprefix p10relwordtab,relwordhandle
		noprefix p10inherenttab,nullhandle
		noprefix $ffff,$0000

p10prefixtab:	prefix $80,p10regatab,immedhandle
		prefix $90,p10regatab,directhandle
		prefix $a0,p10regatab,indexedhandle
		prefix $b0,p10regatab,extendedhandle
		prefix $c0,p10regbtab,immedhandle
		prefix $d0,p10regbtab,directhandle
		prefix $e0,p10regbtab,indexedhandle
		prefix $f0,p10regbtab,extendedhandle
		prefix $ff,$0000,$0000

; page 10 word relative opcoes

p10relwordtab:	mc $21,lbrnn
		mc $22,lbhin
		mc $23,lblsn
		mc $24,lbhsn
		mc $25,lblon
		mc $26,lbnen
		mc $27,lbeqn
		mc $28,lbvcn
		mc $29,lbvsn
		mc $2a,lbpln
		mc $2b,lbmin
		mc $2c,lbgen
		mc $2d,lbltn
		mc $2e,lbgtn
		mc $2f,lblen
		mc $ff,$0000

; page 10 inherent opcodes

p10inherenttab:	mc $3f,swi2n
		mc $ff,$0000

p10regatab:	mc $03,cmpdn
		mc $0c,cmpyn
		mc $0e,ldyn
		mc $0f,styn
		mc $ff,$0000

p10regbtab:	mc $0e,ldsn
		mc $0f,stsn
		mc $ff,$0000

; page 11 noprefix and prefix tables

p11noprefixtab:	noprefix p11inherenttab,nullhandle
		noprefix $ffff,$000

p11prefixtab:	prefix $80,p11regtab,immedhandle
		prefix $90,p11regtab,directhandle
		prefix $a0,p11regtab,indexedhandle
		prefix $b0,p11regtab,extendedhandle
		prefix $ff,$0000,$0000

p11inherenttab:	mc $3f,swi3n
		mc $ff,$0000

p11regtab:	mc $03,cmpun
		mc $0c,cmpsn
		mc $ff,$0000

; handy strings

spacemsg:	fcn ' '
ehmsg:		fcn '???'
colonmsg:	fcn ':'
commamsg:	fcn ','
hexmsg:		fcn '$'
directmsg:	fcn '<'
immedmsg:	fcn '#'
negativemsg:	fcn '-'

; register names

ccregmsg:	fcn 'CC'
aregmsg:	fcn 'A'
bregmsg:	fcn 'B'
dpregmsg:	fcn 'DP'
dregmsg:	fcn 'D'
xregmsg:	fcn 'X'
yregmsg:	fcn 'Y'
sregmsg:	fcn 'S'
uregmsg:	fcn 'U'
pcregmsg:	fcn 'PC'

; disassemble from u, count of x instructions

disassentry:	stx disasscounter	; save the instruction count
		
		lbsr outputinit		; setup the buffer and pointer

		stu statementstart	; save the address of the first byte
		tfr u,d			; get address of this line
		lbsr wordtoaschex	; print the address into the buffer
		stx outputpointer	; save it away
		ldx #colonmsg		; this is the "label"
		lbsr outputappend	; so it needs a colon
		ldx #spacemsg		; and a space would be nice
		lbsr outputappend	; add it
		lbsr outputappend	; and one more

		ldy #defaultpage	; set the default page
		sty currentpage		; and save it away

		lda ,u+			; get the opcode byte
		sta opcode		; save it (we'll need it later)

		ldy #pagedefs		; we need to loop on the page defs
pagedefloop:	tst ,y			; end of the list?
		beq setcurrentpage	; if so then exit loop
		cmpa ,y+		; see if we have a match to the page
		bne nopagedefmatch	; if not...
		ldy ,y			; if we do, then deref to get page
		sty currentpage		; save it (pointer to nopreixtab)
		lda ,u+			; get the new opcode
		sta opcode		; and save it
		bra setcurrentpage	; hop forward to setting current page
nopagedefmatch:	leay 2,y		; hop over the noprefix pointer
		bra pagedefloop		; back for more (there's only 2...)

setcurrentpage:	ldy [currentpage]	; get the non prefix table
		lda opcode		; get the opcode again
		
noprefixloop:	ldx ,y			; get the opcode pointer
		cmpx #$ffff		; $ffff marks this one's end
		beq noprefixout		; if we are at the end, leave
		lbsr procaddrmode	; process this opcode table
		beq prefixout		; if zero then we proc'd a opcode
		leay 4,y		; y points to the noprefix table
		bra noprefixloop	; not the opcode table

noprefixout:	ldy currentpage		; get the current page again
		leay 2,y		; the preifxtab is next
		ldy ,y			; deref to get it
		lda opcode		; get the full opcode again
		anda #$f0		; we just want the prefix

prefixloop:	ldb ,y			; get this prefix
		cmpb #$ff		; $ff marks the end of the table
		beq nomatch		; if we are at the end ??? to output
		cmpa ,y+		; and see if it matches the prefix
		bne prefixnotfound	; not found, so onward to next one

		lda opcode		; our opcode has the right prefix
		anda #$0f		; mask off the interesting part
		lbsr procaddrmode	; and process the opcode table
		beq prefixout		; if zero then we proc'd a opcode
		
prefixnotfound:	leay 4,y		; hop over to the next prefix
		bra prefixloop		; and go back for more

nomatch:	ldx #ehmsg		; '???'
		lbsr outputappend	; oh dear

prefixout:	stu statementend	; save the end address

		lda #40			; move to the middle of the screen
		lbsr outputmoveto	; ...

		ldy statementstart	; reset instruction counter

rawbyteloop:	lda ,y+			; get the byte from teh stream
		ldx outputpointer	; get the current position
		lbsr bytetoaschex	; convert byte to ascii
		stx outputpointer	; save the new position

		ldx #spacemsg		; pad with a space
		lbsr outputappend	; ..

		cmpy statementend	; see if we are at the end
		bne rawbyteloop		; if not, back for more raw bytes

		lda #60			; move to 3/4 across the screen
		lbsr outputmoveto	; ...

		ldy statementstart	; reset instruction counter

rawbyteascloop:	lda ,y+			; get the byte from the stream
		lbsr printableasc	; convert it to printable ascii
		lbsr outputasc		; add it ot the output
		
		cmpy statementend	; see if we are the end 
		bne rawbyteascloop	; if not, back for more ascii chars

		ldx #newlinemsg		; we want a nice newline
		lbsr outputappend	; between the statements
		ldx outputpointer	; get the current pos
		clr ,x			; so we can (finally) add a null

		ldx #outputbuffer	; send the whole line
		lbsr ioputstr		; to the terminal

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
		cmpb #$ff		; end of the opcode table?
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
		orcc #$04		; we processed an opcode -> set zero
		rts			; so we can now output the line
procaddrmodeo:	ldy addrmode		; restore once more so caller
		andcc #$fb		; and mark it as not proc'd -> !zero
		rts

; post opcode bytes handler that does nothing at all

nullhandle:	rts			; dummy for immediates

; post opode byte handler for single byte offsets

relbytehandle:	lda ,u+			; get the byte offset
		leax a,u
		tfr x,d
		lbsr outputword		; print the offset into the buffer
		rts

; post opode byte handler for word offsets

relwordhandle:	ldd ,u++		; get the byte offset
		leax d,u
		tfr x,d
		lbsr outputword		; print the offset into the buffer
		rts

; post opcode bytes handler for word address

extendedhandle:	ldd ,u++		; get the word, advancing u
		lbsr outputword
		rts

directhandle:	ldx #directmsg		; '<'
		lbsr outputappend
		lda ,u+			; get the byte address, advancing u
		lbsr outputbyte
		rts

immedhandle:	ldx #immedmsg		; '#'
		lbsr outputappend	; append that
		lda opcode		; get the original opcode
		anda #$0f		; mask off the low byte
		cmpa #$03		; this is subd, 2 bytes
		beq immedword		; ...
		cmpa #$0c		; cmpx is also 2 bytes
		beq immedword		; ...
		cmpa #$0e		; ldx is also 2 bytes
		beq immedword		; ...
		lda ,u+			; otherwise it must be 1 byte
		lbsr outputbyte		; convert it
immedout:	rts			; done
immedword:	ldd ,u++		; for words, grab it to d
		lbsr outputword		; convert it
		rts

; this is for exg and tfr - ehmsg is ??? - the index is the post byte,
; masked for source and destination

regmovetab:	fdb dregmsg,xregmsg,yregmsg,uregmsg
		fdb sregmsg,pcregmsg,ehmsg,ehmsg
		fdb aregmsg,bregmsg,ccregmsg,dpregmsg
		fdb ehmsg,ehmsg,ehmsg,ehmsg

regimmedhandle:	ldb ,u			; get the post opcode byte
		ldy #regmovetab		; init the table pointer
		andb #$f0		; mask off the high nibble
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
		andb #$0f		; and mask off the low nibble
		lslb			; times 2 because of word ptrs
		leax [b,y]		; deref again to get the destination
		lbsr outputappend	; ...
		rts

; stacking - pshs, puls, pshu, pulu - the table is a table of bits this time
; not offsets

sstackingtab:	fdb ccregmsg,aregmsg,bregmsg,dpregmsg
		fdb xregmsg,yregmsg,uregmsg,pcregmsg
ustackingtab:	fdb ccregmsg,aregmsg,bregmsg,dpregmsg
		fdb xregmsg,yregmsg,sregmsg,pcregmsg

stkimmedhandle: ldb #8			; 8 shifts for a byte
		lda opcode		; get the original opcode
		bita #$02		; bit 1 0 means s stacking , else u
		beq sstacking		; it is pshs or puls
		ldy #ustackingtab	; so set the table pointer
		bra stackingstart	; hop hop
sstacking:	ldy #sstackingtab	; it is pshu or pulu
stackingstart:	lda ,u			; advance at the end not now
stackingloop:	rora			; rorate into carry
		bcc endstacking		; if 0, then we are not stacking
		ldx ,y			; deref to get the reg string
		lbsr outputappend	; append the reg name
		ldx #commamsg		; and we need a comma
		lbsr outputappend	; so add that
endstacking:	leay 2,y		; eitherway move index along
		decb			; and decrement bit counter
		bne stackingloop	; see if there is more bits
		tst ,u+			; now we can advance to next opcode
		beq stkimmedout		; if there was nothing to stack
		lbsr outputbackone	; don't move the cursor back one
stkimmedout:	rts			; out

; indexed mode decoder - the most complex handler by far

sindirectmsg:	fcn '['
eindirectmsg:	fcn ']'
incmsg:		fcn '+'
incincmsg:	fcn '++'
decmsg:		fcn '-'
decdecmsg:	fcn '--'

; the different indexing mode, 16 of them but a few are unused

offsettab:	fdb indexreginc,indexregincinc,indexregdec,indexregdecdec
		fdb indexreg,indexbreg,indexareg,indexinvalid
		fdb indexbytereg,indexwordreg,indexinvalid,indexdreg
		fdb indexbytepc,indexwordpc,indexinvalid,indexindirect

; the front end to the handler

indexedhandle:	lda ,u+			; grab the index code byte
		sta indexcode		; save it away
		bmi indexoffset		; bit7 set?
		anda #$1f		; if not->offset is in this byte
		lbsr outputsignfive	; output just the low 5 bits
		ldx #commamsg		; then a comma
		lbsr outputappend	; ...
		bsr showindexreg	; and the register we are offsetting
		rts			; done
indexoffset:	bita #$10		; bit4 set->
		beq notindirect		; means indirect mode is used
		ldx #sindirectmsg	; this is a '['
		lbsr outputappend	; add it to the output
notindirect:	anda #$0f		; mask iff the low 4 bits
		lsla			; shift up to get index into tab
		ldx #offsettab		; setup the offset mode table
		ldx a,x			; add the offset type
		jsr ,x			; jump into the handler
		lda indexcode		; restore the index code again
		bita #$10		; and test test for indirect
		beq indexout		; if not, then done
		ldx #eindirectmsg	; otherwise get the ']'
		lbsr outputappend	; and add it to the stream
indexout:	rts

; these are the four index registers

indexregtab:	fdb xregmsg,yregmsg,uregmsg,sregmsg

; turns the index code byte into a register and outputs it

showindexreg:	lda indexcode		; load the index code back
		anda #$7f		; mask off the high bit
		lsra			; shift down to the end
		lsra			; ...
		lsra			; ...
		lsra			; ,,,
		lsra			; ,,,
		lsla			; shift up,since tab is words
		ldx #indexregtab	; setup the tablee
		ldx a,x			; add the reg type and deref
		lbsr outputappend	; add the reg to the stream
		rts

; handlers for the difference indexing modes

indexinvalid:	ldx #ehmsg		; '???'
		lbsr outputappend	; output it
		rts

indexreginc:	ldx #commamsg		; ','
		lbsr outputappend	; output it
		bsr showindexreg	; then the register
		ldx #incmsg		; '+'
		lbsr outputappend	; output it
		rts

indexregincinc:	ldx #commamsg		; ','
		lbsr outputappend	; output it
		bsr showindexreg	; then the register
		ldx #incincmsg		; '++'
		lbsr outputappend	; output it
		rts

indexregdec:	ldx #commamsg		; ','
		lbsr outputappend	; output it
		ldx #decmsg		; '-'
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexregdecdec:	ldx #commamsg		; ','
		lbsr outputappend	; output it
		ldx #decdecmsg		; '--'
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexreg:	ldx #commamsg		; ','
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexbreg:	ldx #bregmsg		; 'B'
		lbsr outputappend	; output it
		ldx #commamsg		; ','
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexareg:	ldx #aregmsg		; 'A'
		lbsr outputappend	; output it
		ldx #commamsg		; ','
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexbytereg:	lda ,u+			; get the next byte
		lbsr outputsignbyte	; output the byte (with dollar)
		ldx #commamsg		; ','
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexwordreg:	ldd ,u++		; get the next two bytes
		lbsr outputsignword	; output the word
		ldx #commamsg		; ','
		lbsr outputappend	; output it
		lbsr showindexreg	; then the register
		rts

indexdreg:	ldx #dregmsg		; 'D'
		lbsr outputappend	; output it
		ldx #commamsg		; ','
		lbsr outputappend	; output it 
		lbsr showindexreg	; then the register
		rts

indexbytepc:	lda ,u+			; get the next byte
		lbsr outputsignbyte	; output the byte with dollar
		ldx #commamsg		; ','
		lbsr outputappend	; output it
		ldx #pcregmsg		; 'PC'
		lbsr outputappend	; output it
		rts

indexwordpc:	ldd ,u++		; get the next word
		lbsr outputsignword	; output the word with dollar
		ldx #commamsg		; ','
		lbsr outputappend	; output it
		ldx #pcregmsg		; 'PC'
		lbsr outputappend	; output it
		rts

indexindirect:	ldd ,u++		; get the next work
		lbsr outputword		; and output it
		rts

;;;

outputinit:	ldx #outputbuffer
		lda #80			; fill term width with spaces
		ldb #$20		; 'space'
outputinitloop:	stb ,x+			; fill with a space
		deca			; upto 80 chars
		bne outputinitloop	; back for more
		clr ,x+			; add the terminator
		ldx #outputbuffer	; reset the buffer pointer
		stx outputpointer	; and save it to the start
		rts

outputappend:	pshs x,y,a
		tfr x,y			; on input x is the string to append
		ldx outputpointer	; but we need it in y
		lbsr concatstr		; append y->x
		stx outputpointer	; save the new position
		puls x,y,a
		rts

outputasc:	ldx outputpointer	; get the current position
		sta ,x+			; put 'a' in the stream
		stx outputpointer	; save the new position
		rts

outputsignfive:	ldx #hexmsg
		lbsr outputappend
		bita #$10
		beq plusfive
		ldx #negativemsg
		lbsr outputappend
		eora #$1f
		inca
plusfive:	ldx outputpointer
		lbsr bytetoaschex
		stx outputpointer
		rts

outputbyte:	ldx #hexmsg		; '$'
		lbsr outputappend	; output it
		ldx outputpointer	; get the current pointer
		lbsr bytetoaschex	; add the byte to the output
		stx outputpointer	; save the new pointer
		rts

outputsignbyte:	ldx #hexmsg
		lbsr outputappend
		tsta
		bpl plusbyte
		ldx #negativemsg
		lbsr outputappend
		coma
		inca
plusbyte:	ldx outputpointer
		lbsr bytetoaschex
		stx outputpointer
		rts


outputword:	ldx #hexmsg		; '$'
		lbsr outputappend	; output it
		ldx outputpointer	; get the current pointer
		lbsr wordtoaschex	; add the word to the output
		stx outputpointer	; save the new pointer
		rts

outputsignword:	ldx #hexmsg
		lbsr outputappend
		tsta
		bpl plusword
		ldx #negativemsg
		lbsr outputappend
		coma
		comb
		addd #1
plusword:	ldx outputpointer
		lbsr wordtoaschex
		stx outputpointer
		rts

outputbackone:	pshs x,a
		ldx outputpointer	; get the current position
		lda #$20		; 'space'
		sta ,-x			; blank current pos and back move one
		stx outputpointer	; save the new position
		puls x,a
		rts

outputmoveto:	ldx #outputbuffer	; get the starting position
		leax a,x		; add the column offset
		stx outputpointer	; save it to the current position
		rts

		endsection
