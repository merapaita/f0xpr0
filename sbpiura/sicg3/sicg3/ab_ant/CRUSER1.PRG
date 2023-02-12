set excl off
USE H:\SIS97\DATA\HOJCON	     IN 1  &&USE H:\SIS97\DATAABAS\ordser     IN 2
USE H:\SIS97\DATA\ordser     IN 2
SELE 2
SET ORDER to ordser1
VINDX = SYS(3)+'.IDX'
SELE 1
INDEX ON NUMref+codfte+nummes TO (VINDX) FOR TIPDOC='O/S' AND ESTADO#'99' AND LEFT(NUMHC,1)$'0123456789'
SELE 2
set filter to estado='50'
GO TOP
SCAN
	vkey = numos
	VFTE = codfte
	sele 1
	seek vkey+VFTE
	if found() 
		replace ordser.numhc with numhc , ordser.perhc with nummes
	endif
	sele 2	
ENDSCAN