; in ram, add our global variables

		.area RAM (ABS)

		.org 0

; interrupt handler routines

handleds3234::	.rmb 2
handle65spi::	.rmb 2
handle65c22::	.rmb 2
handlevdc::	.rmb 2
handle88c681::	.rmb 2
handlebuzzer::	.rmb 2

; the current open channel

iogetcharp:	.rmb 2
ioputcharp:	.rmb 2
iogetwtop:	.rmb 2

; monitor stack stuff

spatentry:	.rmb 2
userregs:	.rmb 11

; keyboard global

keyrawmode:	.rmb 1		; 0 for acii, 1 for raw
keyreadpointer:	.rmb 1
keywritepointer:.rmb 1
keybuffer:	.rmb 64		; ascii or raw

; ascii keymode

asciilshifton:	.rmb 1
asciirshifton:	.rmb 1
asciicontrolon:	.rmb 1

; monitor parsing

inputbuffer:	.rmb 256
inputcount:	.rmb 1
outputbuffer:	.rmb 256
parambuffer:	.rmb 32

dumppointer:	.rmb 2
dumpcounter:	.rmb 2

disasscounter:	.rmb 2
outputpointer:	.rmb 2
addrmode:	.rmb 2
opcode:		.rmb 1
currentpage:	.rmb 2
indexcode:	.rmb 1
statementstart:	.rmb 2
statementend:	.rmb 2

flashblock:	.rmb 64
timeoutput:	.rmb 8

uptimeh:	.rmb 2
uptimel:	.rmb 2

idescratchsec:	.rmb 512
firstpartsects:	.rmb 2
startofinodes:	.rmb 2
scratchblk:	.rmb 1024
scratchdirblk:	.rmb 1024
inode:		.rmb 32
dirinode:	.rmb 32
inodeptr:	.rmb 2

xmodemblkcount:	.rmb 1
xmodemchecksum:	.rmb 1

ayoctave:	.rmb 1
ayshape:	.rmb 1
ayduration:	.rmb 2
aysharp:	.rmb 1

tlinestarts:	.rmb 2*25
trow:		.rmb 1
tcol:		.rmb 1
tscrollline:	.rmb 80

buzzerpointer:	.rmb 2
