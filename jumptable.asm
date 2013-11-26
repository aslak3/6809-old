jreset::		jmp reset

jserialputchar::	jmp serialputchar
jserialputstr::		jmp serialputstr
jserialputlab::		jmp serialputlab

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
jserialgetbyte::	jmp serialgetbyte

;; 15/11/2013 extensions
jay8910playnote::	jmp ay8910playnote
jay8910playtune::	jmp ay8910playtune
