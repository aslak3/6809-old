; in ram, add our global variables

		.area RAM (ABS)

		.org 0

spatentry:	.rmb 2
userregs:	.rmb 11

inputbuffer:	.rmb 256
inputcount:	.rmb 1
outputbuffer:	.rmb 256
parambuffer:	.rmb 256

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

ideidentifysec:	.rmb 512
idembrsec:	.rmb 512
firstpartsects:	.rmb 2
fssuperblk:	.rmb 1024
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
