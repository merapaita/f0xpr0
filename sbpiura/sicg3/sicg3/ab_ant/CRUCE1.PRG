USE H:\SIS97\DATA\HOJCON   	 IN 1
USE H:\SIS97\DATAABAS\ordcom IN 2
SELE 2
SET ORDER to ordcom1
VINDX = SYS(3)+'.IDX'
SELE 1
INDEX ON NUMREF+codfte+nummes TO (VINDX) FOR TIPDOC='O/C' AND ESTADO#'99' AND LEFT(NUMHC,1)$'0123456789'
SELE 2
set filter to estado='50'
GO TOP
SCAN
	vkey = numoc
	VFTE = codfte
	sele 1
	seek vkey+VFTE
	if found() 
		replace ordcom.numhc with numhc , ordcom.perhc with nummes
	endif
	sele 2	
ENDSCAN