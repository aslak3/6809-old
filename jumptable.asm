jreset::		jmp reset

jioputchar::		jmp ioputchar
jioputstr::		jmp ioputstr
jioputlab::		jmp ioputlab
jiogetchar::		jmp iogetchar
jiogetstr::		jmp iogetstr
jiogetbyte::		jmp iogetbyte

jspistart::		jmp spistart
jspistop::		jmp spistop
jspiwrite::		jmp spiwrite
jspiread::		jmp spiread
jspiwriteblock::	jmp spiwriteblock
jspireadblock::	jmp spireadblock

jaschextobyte::		jmp aschextobyte
jaschextoword::		jmp aschextoword
jbytetoaschex::		jmp bytetoaschex
jwordtoaschex::		jmp wordtoaschex
jskipspaces::		jmp skipspaces
jprintableasc::		jmp printableasc
jconcatstr::		jmp concatstr
jconcatstrn::		jmp concatstrn

jfsreadblk::		jmp fsreadblk
jfsreadinode::		jmp fsreadinode
jfsreaddata::		jmp fsreaddata
jfsreadfile::		jmp fsreadfile

jdelay::		jmp delay
jwordswap::		jmp wordswap

jvinit::		jmp vinit
jvvread::		jmp vread
jvwrite::		jmp vwrite
jvseekcommon::		jmp vseekcommon
jvseekread::		jmp vseekread
jvseekwrite::		jmp vseekwrite
