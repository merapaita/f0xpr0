USE H:\SIS97\DATAPR\iteoc 	IN 1
USE H:\SIS97\DATAPR\itepol	IN 2
SELE 2
SET ORDER to itepol4
VINDX = SYS(3)+'.IDX'
SELE 1
INDEX ON NUMOC TO (VINDX) FOR ESTADO='50'
SELE 2
set filter to estado='00'
GO TOP
qq
SCAN
	vkey = numoc
	sele 1
	seek vkey
	if found() 
		replace ITEPOL.CODCAD with CODCAD
	endif
	sele 2	
ENDSCAN