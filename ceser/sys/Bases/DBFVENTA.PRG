clear
close data
use gc_hve00

a=1
scan while !eof()
	IF substr(hve_codent,1,1) = " "
       w_codent = ALLTRIM(hve_codent)
	   w_codent2 = VAL(w_codent)
	   REPL hve_codent WITH str(w_codent2,11)
	ENDIF
	a=a+1
	@ 10,20 say a
ends
USE