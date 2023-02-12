PARAMETERS CMES
cPeriodo = '1999'
*cMes='04'
USE Libro1 ORDER TAG Libro1
SEEK cPeriodo
nInicio = VAL(Nummes)
nIndice1 = Indice
SEEK cPeriodo+cMes
nFin = VAL(cMes)
nIndice2 = Indice
SEEK cPeriodo
sMes = 'MES_'+cMes
FOR i = nInicio to nFin
	IF SEEK(cPeriodo+padl(ALLTRIM(STR(i)),2,'0'))
		IF !FOUND()
			wait wind 'Falta Un Mes.... Revise'
			EXIT
		ENDIF
		nFactor = 1+(nIndice2-Indice)/Indice
		REPLACE (SMes) WITH nFactor
	ENDIF
ENDFOR
