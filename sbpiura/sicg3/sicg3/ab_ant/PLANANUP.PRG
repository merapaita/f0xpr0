PARAMETERS nMes ,vpartida

PUBLIC nTotPart,nTotPart1

nTotPart = 0
nTotPart1 = 0

SET PATH TO F:\BENE2001\DATA
XANO='2001'
vIdx1 = SYS(3)+'.idx'

public nSuma
nTotOC = 0

CLOS DATA
USE IteOC1 IN 1
USE OrdCom IN 2 ORDER TAG OrdCom1 ALIAS Orden
USE IteOC  IN 3 ORDER TAG IteOC1  
USE Promae IN 4 ORDER TAG Promae1

INDEX ON PERIODO+NUMOC+CODPART TO &vIdx1

SET RELATION TO periodo+ numoc INTO Orden  ADDITIVE
SET RELATION TO periodo+ numoc INTO IteOC ADDITIVE

set skip to Iteoc

SET FILTER TO BETW(VAL(Orden.NumMes),1,nMes) AND !EMPTY(IteOC.CodArt) AND substr(CODPART,5,2)=vpartida AND Iteoc.Orden#'99'	CODPART=vpartida

* substr(CODPART,5,2)=vpartida

CIA='SOCIEDAD DE BENEFICENCIA DE PIURA'
DO Reporte WITH 2,"PlanAnuP","Plan Anual de Adquisiciones",2,.F.,.T.

CLOS DATA

FUNCTION Sum_VP
*--------------
PARAMETERS xValor,XX

DO CASE
	CASE XX='1'
		nTotPart  = nTotPart  + xValor
		nTotPart1 = nTotPart1 + xValor
	CASE XX='2'
		nTotPart = 0
	CASE XX='3'
		nTotPart  = 0
		nTotPart1 = 0
ENDCASE

RETURN 0
