close data
use gc_hve00

scan while !eof()
	IF LEN(ALLTRIM(hve_codent)) = 9
	   w_codent = VAL(HVE_codent)
	   REPL hve_codent WITH str(w_codent,11)
	ENDIF
ends
USE