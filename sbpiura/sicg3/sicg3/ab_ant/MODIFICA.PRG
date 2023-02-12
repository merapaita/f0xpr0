SELE 1
USE ITEART
SET ORDER TO ITEART3
selec 3
use nada
SELEC 2
USE STOCK
SET FILTER TO TIPDOC="O/C"
GO TOP
van=1
clea
DO while !EOF()
 BUSCA=CODART
 CANTW=CANTIDAD
 SELE 1
 SEEK BUSCA
 @ 10,20 say str(van,5)
 if .not. eof()   && lo encontro
 	if cantini=cantw  && verifica cantidades iguales
 		selec 3
 		append blank
 		replace codigo with busca
 	endif
 endif
 van=van+1
 SELEC 2
 skip
ENDDO
