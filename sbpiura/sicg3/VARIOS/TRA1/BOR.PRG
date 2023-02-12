
set dele off
use sos alias diario
INDEX ON SUBSTR(COMPROB,4,4)+LEFT(COMPROB,2)+CUENTA+TIPO TO XX FOR MONT(FECEMI)=7 AND (((CUENTA='93' OR CUENTA='96') AND TIPO='D') OR ((CUENTA='91' OR CUENTA='93') AND TIPO='H')) AND (ORDEN='08' OR ORDEN='30')
go top
set step on
SCAN
	exit
	cComp = SUBSTR(Comprob,4,4)
	nCon = 0
	SCAN WHILE cComp = SUBSTR(Comprob,4,4)
		nCon = nCon+1
	ENDSCAN
	skip -1
	nReg = RECNO()
	IF ncon=4
		DELE FOR SUBSTR(Comprob,4,4) = cComp
	ENDIF
	GO nReg
ENDSCAN

set dele on
DO REPORTE WITH 2, "Busca", ' ',1,.F.,.T.
