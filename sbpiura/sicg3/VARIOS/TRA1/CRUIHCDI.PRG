*---------------------------------------
* Cruza IteHC con Cuentas Presupuestales 
* del Diario
*---------------------------------------
use itehc  IN 1
use diario IN 2 ORDER TAG Diario11

vIdx = SYS(3)+'.Idx'

SELE Diario
set filter to cuenta='8'

SELE IteHc
INDEX ON NumMes+NumHC+CodFte+CodCad TO (vIdx) FOR nummes='08' AND Estado#'92' and codcad='0004'
SET RELATION TO '240'+NumHC+NumMes INTO Diario

SELE IteHC
SCAN
	xHC = NumHC
	xMes = NumMes
*	IF NumHC = '0018'
		set step on
*	ENDIF
	SCAN WHILE NumHC= xHC
		xCodFte = CodFte
		xCodCad = CodCad
		nValPart = 0
		SCAN WHILE NumHC= xHC AND CodFte+CodCad=xCodFte+'0004'		&&xCodCad
			nValPart = nValPart + ValPart
		ENDSCAN
		SKIP - 1
		SELE Diario
		xComp = '240'+xHC+ xMes
		xCtaD = '8301' + IIF(xCodFte='09','0201','0402') + RIGH(xCodCad,2)
		xCtaH = '8401' + IIF(xCodFte='09','0201','0402') + RIGH(xCodCad,2)
		xSumD = 0
		xSumH = 0
		SCAN WHILE Comprob = xComp
			IF Tipo = 'D' AND Cuenta = xCtaD
				xSumD = xSumD + Montos
			ENDIF
			IF Tipo = 'H' AND Cuenta = xCtaH
				xSumH = xSumH + Montos
			ENDIF
		ENDSCAN
		IF nValPart=xSumD AND xSumD= xSumH
			*
		ELSE
			WAIT WIND 'Error H/C '+ xHC
		ENDIF
	ENDSCAN
	SKIP -1
ENDSCAN