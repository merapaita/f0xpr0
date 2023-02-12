use maepre in 1 orde tag maepre6
use itepOL in 2 orde tag itepOL1
sele 2
go top
scan
	*vkey = CODFUN+CODPRG+CODSUBPR+CODPROY+CODACT+TIPFUN
	*if tipfun = 'F'
	if LEN(ALLTRIM(CODCAL))<=18
		vkey = subs(codcal,8,2)+subs(codcal,10,3)+subs(codcal,13,4)+'   '+subs(codcal,17,2)+'F'
	else
		vkey = subs(codcal,8,2)+subs(codcal,10,3)+subs(codcal,13,4)+subs(codcal,17,3)+'  '+'I'
	endif	
*	123456789012345678
*	9701RDR05003000602
	*vkey = PERIODO+UNIGES+UNIEJE+CODCAD
	sele 1
	seek vkey
	if found()
		REPLACE ITEPOL.CODCAD with codcad		
	endif
	sele 2
endscan