USE MAEPRE IN 1 ORDER TAG MAEPRE1
USE ITEPAR IN 2 ORDER TAG ITEPAR1
USE CALEN  IN 3 ORDER TAG CALEN1
SELE MAEPRE
GO TOP
SCAN
	m.estfun = uniges+unieje+codfun+codprg+codspr+actpry+codcom+codmet+codfin
	vkey = periodo+codcad
	sele 2
	seek vkey
	if found()
		replace estfun with m.estfun for periodo+codcad = vkey
	endif
	sele 3
	seek vkey
	if found()
		replace estfun with m.estfun for periodo+codcad = vkey
	endif
	sele 1
endscan
