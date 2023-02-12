worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Sai_lib
DO Fox_Ambi
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'

SET PATH TO F:\BENE2000\DATA
PUBLIC vNumMes,cAno,vOpcion,OkCancel,cProc

CLOS DATA

DO Pantalla
DO Escoge
IF !EMPTY(cProc)
	DO &cProc
ENDIF

RETURN

PROCEDURE Pantalla
*-----------------
USE Parmae IN 0 ORDER TAG Parmae1 ALIAS Parma
SELE Parma
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vOpcion,OkCancel
cMes  = SPAC(2)
cAno  = YEAR(DATE())
@ 02,01 SAY "       Opciones : " 
@ 01,20 GET vOpcion  FUNCTION '^ Ordenes de Compra;Ordenes de Servicio;O/C con Partida;O/S con Partida'
*@ 05,01 SAY "      Mes a Reportar : "
*@ 05,25 GET cMes Valid Val_para(cMes,'FECMES',' ',22)
@ 07,01 SAY "  Periodo a Reportar : "
@ 07,25 GET cAno PICTURE '9,999'
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
USE IN Parma
RETURN

PROCEDURE Escoge
*---------------
IF LASTKEY()=27 OR OkCancel = 2
	cProc = ''
	RETURN
ENDIF
IF !EMPTY(cAno)
	cRuta = "F:\Bene"+IIF(cAno<2000,RIGHT(STR(cano,4),2),STR(cano,4))+IIF(cAno#1996,"\Data","\DatBen")
 ELSE
	DO StandBy WITH "Ruta Especificada no es correcta"
	RETURN .T.
ENDIF
SET PATH TO &cRuta
DO CASE
	CASE vOpcion = 1
		cProc = 'ResOC'
	CASE vOpcion = 2
		cProc = 'ResOS'
	CASE vOpcion = 3
		cProc = 'ResOCP'
	CASE vOpcion = 4
		cProc = 'ResOSP'
	OTHERWISE
		cProc = ''
ENDCASE

PROCEDURE ResOC
*--------------
* Resumen de Ordenes de Compra
public nSuma, vIdx
vIdx   = SYS(3)+'.Idx'
nTotOC = 0
TTotOC = 0
CLOS DATA
USE IteOC  IN 1			&& ORDER TAG IteOC1
USE OrdCom IN 2 ORDER TAG OrdCom1 ALIAS Orden
USE IteOC1 IN 3 ORDER TAG IteOC11
USE Promae IN 4 ORDER TAG Promae1
USE HojCon IN 5
USE Parmae IN 6 ORDER TAG Parmae1 ALIAS Parma

DEFINE WINDOW ResOC FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResOC

STORE 1 TO vOpcion,OkCancel
vNumMes   = SPAC(2)
vNumMes1  = SPAC(2)
xAno = STR(cAno,4)
vOrden = 1
@ 03,01 SAY '         Ordenado por: '
@ 03,25 GET vOrden FUNCTION "^ Correlativo;Proveedor" 
@ 06,01 SAY "Reportar mes: "
@ 06,15 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',22)
@ 08,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResOC

IF lastkey()#27 AND OkCancel#2
	SELE HojCon					&& PARA VER LAS O/C QUE ESTAN PAGADAS
	INDEX ON NumRef TO XX FOR TIPDOC='O/C'
	
	SELE IteOC
	SET RELATION TO Periodo + NumOC INTO Orden ADDITIVE
	SET RELATION TO NumOC INTO HojCon          ADDITIVE
	
	IF vOrden = 1
		SET ORDER TO IteOC1
		SET FILTER TO  BETW(MONT(HojCon.Fechc),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(CodArt) AND PerPec#"LI" AND (Orden.Estado='20' OR Orden.Estado='22')
*		SET FILTER TO  BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(CodArt) AND Orden.Estado='20'
*		SET FILTER TO  BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(CodArt) AND Orden.Estado#'99'
		INDEX ON Orden.Periodo+Orden.PerHC+NumOc TO (vIdx) 
		DO Reporte WITH 2,"PlanAn1","Plan Anual de Adquisiciones",2,.F.,.T.
	 ELSE
		INDEX ON Orden.CodPrv+Periodo+NumOC TO (vIdx) FOR BETW(MONT(HojCon.FecHC),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(CodArt) AND PerPec#"LI" AND (Orden.Estado='20' OR Orden.Estado='22')
*		INDEX ON Orden.CodPrv+Periodo+NumOC TO (vIdx) FOR BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(CodArt) AND Orden.Estado='20'
*		INDEX ON Orden.CodPrv+Periodo+NumOC TO (vIdx) FOR BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(CodArt) AND Orden.Estado#'99'
		DO Reporte WITH 2,"PlanAnu","Plan Anual de Adquisiciones",2,.F.,.T.
	ENDIF
ENDIF


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
		IF EMPTY(CanReb)
			nTotOC = nTotOC + IteOC.ValTot
		 ELSE
			nTotOC = nTotOC + IteOC.AntTot
		ENDIF
	ENDSCAN
	GO xRec
ENDIF

RETURN nTotOC

PROCEDURE ResOS
*--------------
* Resumen de Ordenes de Servicio
public nSuma
vIdx   = SYS(3)+'.Idx'
nTotOC = 0

CLOS DATA
USE OrdSer IN 1 ORDER TAG OrdSer1 ALIAS Orden 
USE IteOs1 IN 2 ORDER TAG IteOs11
USE Promae IN 3 ORDER TAG Promae1
USE HojCon IN 4
USE Parmae IN 5 ORDER TAG Parmae1 ALIAS Parma

DEFINE WINDOW ResOS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResOS

STORE 1 TO vOpcion,OkCancel
vNumMes  = SPAC(2)
vNumMes1 = SPAC(2)
xAno = STR(cAno,4)
vOrden = 1

@ 03,01 SAY '         Ordenado por: '
@ 03,25 GET vOrden FUNCTION "^ Correlativo;Proveedor" 
@ 06,01 SAY "Reportar mes: "
@ 06,15 SAY "Desde:" GET vNumMes  Valid Val_para(vNumMes,'FECMES',' ',22)
@ 08,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResOS

IF lastkey()#27 AND OkCancel#2
	SELE HojCon					&& PARA VER LAS O/C QUE ESTAN PAGADAS
	INDEX ON NumRef TO (vIdx) FOR TIPDOC='O/S'
	
	SELE Orden
	SET RELATION TO periodo + numOS INTO IteOS1 ADDITIVE
	SET RELATION TO NumOS           INTO HojCon ADDITIVE
	
	IF vOrden = 1
		SET FILTER TO BETW(MONT(HojCon.Fechc),VAL(vNumMes),VAL(vNumMes1)) AND Orden.Estado='20'
*		SET FILTER TO BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND Orden.Estado='20'
*		SET FILTER TO BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND Orden.Estado#'99'
		INDEX ON Orden.Periodo+Orden.PerHC+NumOS TO (vIdx) 
		DO Reporte WITH 2,"PlaAnuS1","Plan Anual de Adquisiciones",2,.F.,.T.
	 ELSE
		INDEX ON Periodo+Orden.CodPrv+NumOS TO (vIdx) FOR BETW(MONT(HojCon.FecHC),VAL(vNumMes),VAL(vNumMes1)) AND Orden.Estado='20'
*		INDEX ON Periodo+Orden.CodPrv+NumOS TO (vIdx) FOR BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND Orden.Estado='20'
*		INDEX ON Periodo+Orden.CodPrv+NumOS TO (vIdx) FOR BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND Orden.Estado#'99'
		DO Reporte WITH 2,"PlanAnuS","Plan Anual de Adquisiciones",2,.F.,.T.
	ENDIF
ENDIF

CLOS DATA
ERASE (vIdx)
RETURN

PROCEDURE ResOCP
*---------------
PUBLIC nTotPart,nTotPart1

nTotPart = 0
nTotPart1 = 0

vIdx1 = SYS(3)+'.idx'

public nSuma
nTotOC = 0

CLOS DATA
USE IteOC1 IN 1
USE OrdCom IN 2 ORDER TAG OrdCom1 ALIAS Orden
USE IteOC  IN 3 ORDER TAG IteOC1  
USE Promae IN 4 ORDER TAG Promae1
USE Parmae IN 5 ORDER TAG Parmae1 ALIAS Parma

DEFINE WINDOW ResOCP FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResOCP

STORE 1 TO vOpcion,OkCancel
vNumMes  = SPAC(2)
vNumMes1 = SPAC(2)
vCodPart = SPACE(6)
xAno = STR(cAno,4)
@ 03,05 SAY "Partida : "GET vCodPart PICTURE "@!"
@ 05,01 SAY "Reportar Mes : "
@ 06,15 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',22)
@ 08,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE
RELEASE WINDOW ResOCP

IF lastkey()#27 AND OkCancel#2
*	INDEX ON PERIODO+NUMOC+CODPART TO &vIdx1
	
	SET RELATION TO periodo + numoc INTO Orden ADDITIVE
	SET RELATION TO periodo + numoc INTO IteOC ADDITIVE
	
	set skip to Iteoc
	SET FILTER TO BETW(Orden.Perhc,vNumMes,vNumMes1) AND !EMPTY(IteOC.CodArt) AND CodPart=vCodPart AND IteOC.PerPec#"LI" AND (Orden.Estado='20' OR Orden.Estado='22')

*	SET FILTER TO BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(IteOC.CodArt) AND CODPART=vCodPart AND Orden.Estado='20'
*	SET FILTER TO BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND !EMPTY(IteOC.CodArt) AND CODPART=vCodPart AND Iteoc.Estado#'99'
	INDEX ON Orden.Periodo+Orden.PerHC+NumOc+CodPart TO (vIdx1) 
SUSP
	DO Reporte WITH 2,"PlanAnuP","Plan Anual de Adquisiciones",2,.F.,.T.
	
ENDIF

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

PROCEDURE ResOSP
*---------------
public nSuma
nTotOC = 0

CLOS DATA
USE OrdSer IN 1 ORDER TAG OrdSer1 ALIAS Orden 
USE IteOs1 IN 2 ORDER TAG IteOs11
USE Promae IN 3 ORDER TAG Promae1
USE Parmae IN 4 ORDER TAG Parmae1 ALIAS Parma

DEFINE WINDOW ResOSP FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResOSP

STORE 1 TO vOpcion,OkCancel
vNumMes   = SPAC(2)
vNumMes1  = SPAC(2)
vCodPart = SPACE(6)
xAno = STR(cAno,4)
vIdx = SYS(3)+'.idx'

*****
SELE IteOS1
DIMENSION aPartida(20)
APartida=SPACE(6)
I = 0
SCAN
	vPartida  = IteOS1.CodPart
	lDescarta = .F.
	FOR J = 1 to I
		IF aPartida(j) = vPartida
			lDescarta = .T.
			EXIT
		ENDIF
	ENDFOR
	IF !lDescarta
		I = I + 1
		aPartida(i) = vPartida
	ENDIF
ENDSCAN

nCodPart = 1
SELE Orden
*****

@ 01,05 SAY "Selecione Partida : " GET nCodPart FROM aPartida SIZE 6,14 DEFAULT APartida(1)  COLOR SCHEME 5
@ 05,01 SAY "      Mes a Reportar : "
@ 06,15 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',22)
@ 08,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

IF lastkey()#27 AND OkCancel#2
	RELEASE WINDOW ResOSP
	
	SET RELATION TO periodo + numOS INTO IteOS1 ADDITIVE
	SET SKIP TO IteOs1
*	SET FILTER TO BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND IteOS1.CodPart=aPartida(nCodPart) AND Orden.Estado='20'

	SET FILTER TO BETW(Orden.PerHC,vNumMes,vNumMes1) AND IteOS1.CodPart=aPartida(nCodPart) AND Orden.Estado='20'

*	SET FILTER TO BETW(VAL(Orden.NumMes),VAL(vNumMes),VAL(vNumMes1)) AND IteOS1.CodPart=vCodPart AND Orden.Estado#'99'
	INDEX ON Orden.Periodo+Orden.PerHC+NumOS TO (vIdx) 
	DO Reporte WITH 2,"PlanAnSP","Plan Anual de Adquisiciones",2,.F.,.T.
ENDIF
CLOS DATA
RETURN



****

