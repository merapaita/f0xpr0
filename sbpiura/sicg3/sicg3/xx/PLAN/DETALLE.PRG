CLOS DATA
use ..\xx\plan\plan3 in 1 order tag cuenta	ALIAS PLAN
set step on
sele plan
go top
xNivel = 0
xDet = Detalle
SCAN
	nReg = RECNO()
	DO CASE
		CASE Nivel > xNivel
			skip - 1
			REPLACE Detalle WITH 'N'
			skip
			REPLACE Detalle WITH 'S'
			xDet = Detalle
		CASE Nivel = xNivel
			REPLACE Detalle WITH xDet
	ENDCASE
	xNivel = Nivel
ENDSCAN