set echo off
set talk off
USE H:\SIS97\DATA\iteoc  IN 1
USE H:\SIS97\DATA\ORDCOM IN 2
SELE 2
SET ORDER TO ORDCOM1
VINDX = SYS(3)+'.IDX'
SELE 1
INDEX ON NUMoc+codfte TO (VINDX) FOR ESTADO#'99' &&AND LEFT(NUMHC,1)$'0123456789'
SELE 2
SET FILTER TO ESTADO='00'
GO TOP
SCAN
	VKEY = NUMOC
	*VFTE = codfte
	sele 1
	seek vkey &&+VFTE
	if found() 
		REPLACE ordcom.codfte with codfte , ordcom.codcad with codcad
	endif
	sele 2	
ENDSCAN