jreset::		jmp reset

jserialputchar::	jmp ioputchar
jserialputstr::		jmp ioputstr
jserialputlab::		jmp ioputlab

jspistart::		jmp spistart
jspistop::		jmp spistop
jspiwrite::		jmp spiwrite
jspiread::		jmp spiread

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

;; 14/10/2013 extensions
jserialgetbyte::	jmp iogetbyte

;; 15/11/2013 extensions
jay8910playnote::	jmp ay8910playnote
jay8910playtune::	jmp ay8910playtune

;; 11/12/2013
jspiwriteblock::	jmp spiwriteblock
jspireadblock::		jmp spireadblock

;; 26/09/2014
jreadjoystick::		jmp readjoystick

;; 08/11/2014
jioputchar::		jmp ioputchar
jioputstr::		jmp ioputstr
jiogetchar::		jmp iogetchar
jiogetstr::		jmp iogetstr
jioputlab::		jmp ioputlab

jiogetbyte::		jmp iogetbyte

jvinit::		jmp vinit
jvvread::		jmp vread
jvwrite::		jmp vwrite
jvseekcommon::		jmp vseekcommon
jvseekread::		jmp vseekread
jvseekwrite::		jmp vseekwrite
