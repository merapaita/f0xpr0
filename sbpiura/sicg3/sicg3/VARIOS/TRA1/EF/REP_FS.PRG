************************************
* Rep_Fs.prg
* Reporte de F_1 y F_2
************************************
PARAMETERS dFecha1,dFecha2,cRep
PUBLIC nTAC,nTAC2,nTAC3,nTAC4,nTANC,nTANC2,nTANC3,nTANC4
PUBLIC nTPC,nTPC2,nTPC3,nTPC4,nTPNC,nTPNC2,nTPNC3,nTPNC4
PUBLIC nTP,nTP2,nTP3,nTP4
PUBLIC nTI,nTI2,nTI3,nTI4
PUBLIC nTCG,nTCG2,nTCG3,nTCG4
PUBLIC nTOIG,nTOIG2,nTOIG3,nTOIG4

STORE 0 TO nTAC,nTAC2,nTAC3,nTAC4,nTANC,nTANC2,nTANC3,nTANC4
STORE 0 TO nTPC,nTPC2,nTPC3,nTPC4,nTPNC,nTPNC2,nTPNC3,nTPNC4
STORE 0 TO nTP,nTP2,nTP3,nTP4
STORE 0 TO nTI,nTI2,nTI3,nTI4
STORE 0 TO nTCG,nTCG2,nTCG3,nTCG4
STORE 0 TO nTOIG,nTOIG2,nTOIG3,nTOIG4


Cia='Sociedad de Beneficencia de Piura'
*dFecha1={31/01/98}
*dFecha2={31/01/97}
clos data
USE RepFin  IN 1 ORDER TAG RepFin2

DO REPORTE WITH 2, cRep, ' ',1,.F.,.T.

FUNCTION FilCol
*--------------
PARAMETERS cRep,cFila,cColumna
PRIVATE mColumna,mFila

cAno  = SUBSTR(STR(YEAR(dFecha1),4),3,2)
cAno1 = SUBSTR(STR(YEAR(dFecha2),4),3,2)
cMes  = PADL(MONT(dFecha1),2,'0')
cMes1 = PADL(MONT(dFecha2),2,'0')

nColumna=VAL(cColumna)
nFila=VAL(cFila)

nRet=0
IF MOD(nColumna,2)#0			&& '1,3,5,7'
	SEEK cRep+cAno+cMes+cFila+cColumna
	SCAN WHILE CodRep=cRep AND VAL(Periodo)=VAL(cAno) AND VAL(NumMes)=VAL(cMes) AND VAL(Fila)=VAL(cFila) AND VAL(Columna)=VAL(cColumna)
		IF !EMPTY(TotDeb)
			nRet = nRet + TotDeb
		 ELSE
			nRet = nRet + TotHab
		ENDIF
	ENDSCAN
	nRet = ROUND(nRet,0)
	DO CalcVar WITH 1
 ELSE
	SEEK cRep+cAno1+cMes1+cFila+cColumna
	SCAN WHILE CodRep=cRep AND Periodo=cAno1 AND NumMes=cMes1 AND Fila=cFila AND Columna=cColumna
		IF !EMPTY(TotDeb)
			nRet = nRet + TotDeb
		 ELSE
			nRet = nRet + TotHab
		ENDIF
	ENDSCAN
	nRet = ROUND(nRet,0)
	DO CalcVar WITH 2
ENDIF
RETURN nRet


FUNCTION CalcVar
*---------------
PARAMETERS nOpcion
PRIVATE mColumna,mFila
mColumna=VAL(cColumna)
mFila=VAL(cFila)
IF nOpcion=1
	IF cRep='F-1'
		DO CASE 
			CASE BETWEEN(mFila,1,9) AND (mColumna=1 OR mColumna=3)
				DO CASE
					CASE mColumna=1
						nTAC=nTAC+nRet
					CASE mColumna=3
						nTAC3=nTAC3+nRet
				ENDCASE

			CASE BETWEEN(mFila,11,20) AND (mColumna=1 OR mColumna=3)
				DO CASE
					CASE mColumna=1
						nTANC=nTANC+nRet
					CASE mColumna=3
						nTANC3=nTANC3+nRet
				ENDCASE

			CASE BETWEEN(mFila,1,4)  AND (mColumna=5 OR mColumna=7)
				DO CASE
					CASE mColumna=5
						nTPC=nTPC+nRet
					CASE mColumna=7
						nTPC3=nTPC3+nRet
				ENDCASE

			CASE BETWEEN(mFila,6,9)  AND (mColumna=5 OR mColumna=7)
				DO CASE
					CASE mColumna=5
						nTPNC=nTPNC+nRet
					CASE mColumna=7
						nTPNC3=nTPNC3+nRet
				ENDCASE

			CASE BETWEEN(mFila,12,15)  AND (mColumna=5 OR mColumna=7)
				DO CASE
					CASE mColumna=5
						nTP=nTP+nRet
					CASE mColumna=7
						nTP3=nTP3+nRet
				ENDCASE
		ENDCASE
	ENDIF
	IF cRep='F-2'
		DO CASE 
			CASE BETWEEN(mFila,1,4) AND (mColumna=1 OR mColumna=3)
				DO CASE
					CASE mColumna=1
						nTI=nTI+nRet
*						wait wind 'fila' + cfila
*						wait wind 'columna'+ ccolumna
*						wait wind str(nRet,15,3)
					CASE mColumna=3
						nTI3=nTI3+nRet
				ENDCASE

				
			CASE BETWEEN(mFila,6,9) AND (mColumna=1 OR mColumna=3)
				DO CASE
					CASE mColumna=1
						nTCG=nTCG+nRet
					CASE mColumna=3
						nTCG3=nTCG3+nRet
				ENDCASE
				
			CASE BETWEEN(mFila,12,21)  AND (mColumna=1 OR mColumna=3)
				DO CASE
					CASE mColumna=1
						nTOIG=nTOIG+nRet
					CASE mColumna=3
						nTOIG3=nTOIG3+nRet
				ENDCASE
		ENDCASE
	ENDIF
ENDIF



*						wait wind 'fila' + cfila
*						wait wind 'columna'+ ccolumna
*						wait wind str(nRet,15,3)
