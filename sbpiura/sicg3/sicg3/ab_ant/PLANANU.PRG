PARAMETERS nMes

SET PATH TO F:\BENE2001\DATA
XANO='2001'

public nSuma, vIdx
vIdx   = SYS(3)+'.Idx'
nTotOC = 0

CLOS DATA
USE IteOC  IN 1			&& ORDER TAG IteOC1
USE OrdCom IN 2 ORDER TAG OrdCom1 ALIAS Orden
USE IteOC1 IN 3 ORDER TAG IteOC11
USE Promae IN 4 ORDER TAG Promae1
USE HojCon IN 5

SELE HojCon
INDEX ON NumRef TO XX FOR TIPDOC='O/C'

SELE IteOC
SET RELATION TO Periodo + NumOC INTO Orden ADDITIVE
SET RELATION TO NumOC INTO HojCon   ADDITIVE

INDEX ON Orden.CodPrv+Periodo+NumOC TO (vIdx) FOR BETW(VAL(Orden.NumMes),1,nMes) AND !EMPTY(CodArt) AND IteOC.Estado#'99'

*SET FILTER TO BETW(VAL(Orden.NumMes),1,nMes) AND !EMPTY(CodArt) AND IteOC.Estado#'99'

CIA='SOCIEDAD DE BENEFICENCIA DE PIURA'
DO Reporte WITH 2,"PlanAnu","Plan Anual de Adquisiciones",2,.F.,.T.

CLOS DATA
ERASE (vIdx)
RETURN

FUNCTION SumaOC
*--------------
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
