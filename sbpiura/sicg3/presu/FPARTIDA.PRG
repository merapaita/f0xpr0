PARAMETERS 	cPart
PRIVATE cCad
cCad = cPart
xlen = LEN(ALLTRIM(cPart))
nNivel = 0
DO CASE
	CASE xlen = 1
		nNivel = 1
	CASE xlen = 2
		nNivel = 2
	CASE xLen = 4
		nNivel = 3
	CASE xLen = 6
		nNivel = 4
	CASE xLen = 8
		nNivel = 5
	CASE xLen = 10
		nNivel = 6
	CASE xLen = 12
		nNivel = 7
	CASE xLen = 14
		nNivel = 8
ENDCASE

FOR i = 1 TO nNivel
	DO CASE
		CASE i = 1
			cCad = cCad + LEFT(cCad,1)
		CASE i = 2
			cCad = cCad + SUBSTR(cCad,2,1)
		OTHERWISE
			cCad = cCad + str(val(SUBSTR(cCad,i*2-3,2)),2)
	ENDCASE
ENDFOR

RETURN cCad