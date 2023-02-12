clear
close data
use st_iclpr

a=0
scan while !eof()
	w_codent = ALLTRIM(codent)
	w_codent2 = VAL(w_codent)
	REPL codent WITH str(w_codent2,11)
	a=a+1
	@ 10,20 say a
ends
close data