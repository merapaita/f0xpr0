PARAMETERS  nMes

SET PATH TO F:\BENE2001\DATA
XANO='2001'

public nSuma
nTotOC = 0

CLOS DATA
USE OrdSer IN 1 ORDER TAG OrdSer1 ALIAS Orden 
USE IteOs1 IN 2 ORDER TAG IteOs11
USE Promae IN 3 ORDER TAG Promae1

SET RELATION TO periodo+ numOS INTO IteOS1 ADDITIVE
SET FILTER TO BETW(VAL(Orden.NumMes),1,nMes)

CIA='SOCIEDAD DE BENEFICENCIA DE PIURA'
DO Reporte WITH 2,"PlanAnuS","Plan Anual de Adquisiciones",2,.F.,.T.

CLOS DATA

FUNCTION SumaOC
*------------
PARAMETERS xComp
PRIVATE xRec
xRec   = RECNO()
xComp  = IteOC.NumOC

nTotOC = 0
IF !EOF()
	SCAN WHILE xComp = IteOC.NumOC
		nTotOC = nTotOC + IteOC.ValTot
	ENDSCAN
	GO xRec
ENDIF

RETURN nTotOC

*mFecha(val(Orden.NumMes),VAL("20"+periodo))