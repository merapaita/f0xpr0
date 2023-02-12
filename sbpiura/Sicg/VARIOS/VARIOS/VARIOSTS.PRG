worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO TES_LIB
DO Fox_Ambi
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'

SET PATH TO H:\BENEDATA\SICGDATA\DATA2005
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
@ 01,20 GET vOpcion  FUNCTION '^ Resumen de C/P;Resumen de R/I;C/P con Retenciones'
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
	cRuta = "H:\BENEDATA\SICGDATA\DATA"+IIF(cAno<2000,RIGHT(STR(cano,4),2),STR(cano,4))
 ELSE
	DO StandBy WITH "Ruta Especificada no es correcta"
	RETURN .T.
ENDIF
SET PATH TO &cRuta
DO CASE
	CASE vOpcion = 1
		cProc = 'ResCP'
	CASE vOpcion = 2
		cProc = 'ResRI'
	CASE vOpcion = 3
		cProc = 'CPRet'
	OTHERWISE
		cProc = ''
ENDCASE


PROCEDURE ResRi
*--------------
CLOS DATA
USE RecIng  IN 1 ORDER TAG Recing1
USE IteRI   IN 2 ORDER TAG IteRI1
USE Ingreso IN 3 ORDER TAG Ingreso1
USE Parmae  IN 4 ORDER TAG Parmae1 ALIAS Parma

DEFINE WINDOW ResRI FROM 5,15 TO 22,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResRi

STORE 1 TO vOpcion,OkCancel
vNumMes  = SPAC(2)
vNumMes1 = SPAC(2)
vCodPart = SPACE(12)
vTipRI   = SPACE(3)
xAno = STR(cAno,4)
vTipo = 1

*****
SELE Iteri
DIMENSION aPartida(100)
APartida=SPACE(12)
I = 0
SCAN
	vPartida  = IteRI.CodPart
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
SELE RecIng
*****

@ 01,06 SAY 'Tipo de Reporte: '
@ 00,24 GET vTipo FUNCTION "^ Partida;Tipo de R/I" 
@ 03,01 SAY " Partida "
@ 04,01 GET nCodPart FROM aPartida SIZE 6,16 DEFAULT APartida(1)  COLOR SCHEME 5 WHEN vTipo=1
@ 04,24 SAY 'Tipo de Recibo: '
@ 06,24 GET vTipRI   VALID Val_para(vTipRI,'TIPRI ',' ',24) WHEN vTipo=2

@ 11,05 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',12)
@ 11,25 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',32)

@ 14,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResRI

IF lastkey()#27 AND OkCancel#2
	SET RELATION TO recing.periodo + recing.nummes + recing.numri INTO ITERI ADDITIVE
	SELE Iteri
	SET RELATION TO Iteri.CodPart INTO Ingreso ADDITIVE
	SELE RecIng
	SET SKIP TO IteRI
	
	
	IF vTipo=1
		SET FILTER TO BETWEEN(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND Iteri.COdPart=aPartida(nCodPart)
SUSP
		DO Reporte WITH 2,"ResRI","Resumen de recibos de Ingreso por Partida",2,.F.,.T.
	 ELSE
		SET FILTER TO BETWEEN(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1))		&& AND TipRI=vTipRI
		DO Reporte WITH 2,"ResRI1","Resumen de recibos de Ingreso por tipo de R/I ",2,.F.,.T.
	ENDIF
ENDIF

CLOS DATA

RETURN

PROCEDURE ResCP
*--------------
PARAMETERS vNumMes,vCodPart

USE COMPAG IN 1 ORDER TAG COMPAG1
USE ITECP  IN 2 ORDER TAG ITECP1
USE CHEQUE IN 3 ORDER TAG CHEQUE1
USE Parmae IN 4 ORDER TAG Parmae1 ALIAS Parma
USE Promae IN 5 ORDER TAG Promae2
SELECT COMPAG
SET RELATION TO compag.nummes + compag.numcp INTO ITECP  ADDITIVE
SET RELATION TO compag.nummes + compag.numcp INTO CHEQUE ADDITIVE
SET SKIP TO ITECP

DEFINE WINDOW ResCP FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResCP

STORE 1 TO vOpcion,OkCancel
vNumMes  = SPAC(2)
vNumMes1  = SPAC(2)
vCodPart = SPACE(8)
vCadena = SPACE(4)
xAno = STR(cAno,4)
vCodPrv = SPACE(4)

@ 01,01 SAY "Reportar mes: "
@ 01,15 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',22)
@ 03,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)

@ 05,01 SAY "  Cadena : "
@ 05,15 GET vCadena PICTURE "@!"
@ 07,01 SAY "  Partida : "
@ 07,15 GET vCodPart PICTURE "@!"
@ 09,01 SAY "  Cliente : "
@ 09,15 GET vCodPrv PICTURE "@!"	VALID IIF(!EMPTY(vCodPrv),val_fun('Promae', 'CodPrv', 'NomPro',vCodPrv,1),.T.)

@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResCP

IF lastkey()#27 AND OkCancel#2
*	SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND ITECP.CODPART=ALLTRIM(vCodPart) AND IteCP.CodCad=ALLTRIM(vCadena) AND COMPAG.CODOTR="01291"
	IF cAno=1996
		SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND ITECP.CODPART=ALLTRIM(vCodPart) AND COMPAG.CODPRV=ALLTRIM(vCodPrv)
	ELSE
		SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND ITECP.CODPART=ALLTRIM(vCodPart) AND IteCP.CodCad=ALLTRIM(vCadena) AND COMPAG.CODPRV=ALLTRIM(vCodPrv)
	ENDIF
*	SET FILTER TO NUMMES=vNumMes AND ITECP.CODPART=vCodPart AND IteCP.CodCad=vCadena
	DO Reporte WITH 2,"ResCP","Resumen de Comprobantes de Pago",2,.F.,.T.
ENDIF

CLOS DATA

return 

*PROCEDURE CPRet
*--------------
PARAMETERS vNumMes,vCodPart
*CPRET

USE COMPAG IN 1 ORDER TAG COMPAG1
USE ITECP  IN 2 ORDER TAG ITECP1
USE CHEQUE IN 3 ORDER TAG CHEQUE1
USE Parmae IN 4 ORDER TAG Parmae1 ALIAS Parma
USE Promae IN 5 ORDER TAG Promae2
SELECT COMPAG
SET RELATION TO compag.nummes + compag.numcp INTO ITECP  ADDITIVE
SET RELATION TO compag.nummes + compag.numcp INTO CHEQUE ADDITIVE
SET SKIP TO ITECP

DEFINE WINDOW ResCP FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResCP

STORE 1 TO vOpcion,OkCancel
vNumMes  = SPAC(2)
vNumMes1  = SPAC(2)
vCodPart = SPACE(8)
vCadena = SPACE(4)
xAno = STR(cAno,4)
vCodPrv = SPACE(4)

@ 01,01 SAY "Reportar mes: "
@ 01,15 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',22)
@ 03,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)

@ 05,01 SAY "  Cadena : "
@ 05,15 GET vCadena PICTURE "@!"
@ 07,01 SAY "  Partida : "
@ 07,15 GET vCodPart PICTURE "@!"
@ 09,01 SAY "  Cliente : "
@ 09,15 GET vCodPrv PICTURE "@!"	VALID IIF(!EMPTY(vCodPrv),val_fun('Promae', 'CodPrv', 'NomPro',vCodPrv,1),.T.)

@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResCP

IF lastkey()#27 AND OkCancel#2
*	SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND ITECP.CODPART=ALLTRIM(vCodPart) AND IteCP.CodCad=ALLTRIM(vCadena) AND COMPAG.CODOTR="01291"
	IF cAno=1996
		SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND ITECP.CODPART=ALLTRIM(vCodPart) AND COMPAG.CODPRV=ALLTRIM(vCodPrv)
	ELSE
		SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND ITECP.CODPART=ALLTRIM(vCodPart) AND IteCP.CodCad=ALLTRIM(vCadena) AND COMPAG.CODPRV=ALLTRIM(vCodPrv)
	ENDIF
*	SET FILTER TO NUMMES=vNumMes AND ITECP.CODPART=vCodPart AND IteCP.CodCad=vCadena
	DO Reporte WITH 2,"ResCP","Resumen de Comprobantes de Pago",2,.F.,.T.
ENDIF

CLOS DATA


********************
*******************

PROCEDURE CPRet
*--------------
CLOS DATA
USE COMPAG IN 1 ORDER TAG ComPag1
USE RETEN  IN 2 ORDER TAG Reten1
USE Cheque IN 3 ORDER TAG Cheque1
USE Parmae IN 4 ORDER TAG Parmae1 ALIAS Parma
USE Promae IN 5 ORDER TAG Promae2
SELE Compag
SET RELATION TO nummes+ numcp INTO RETEN ADDITIVE
SET RELATION TO nummes+ numcp INTO Cheque ADDITIVE
INDEX ON NumMes+NumCP TO xx FOR !EOF('reten')
SET SKIP TO Reten

DEFINE WINDOW ResCP FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW ResCP

STORE 1 TO vOpcion,OkCancel
vNumMes  = SPAC(2)
vNumMes1  = SPAC(2)
vCodPart = SPACE(8)
vCadena = SPACE(4)
xAno = STR(cAno,4)
vCodPrv = SPACE(4)

@ 01,01 SAY "Reportar mes: "
@ 01,15 SAY "Desde:" GET vNumMes Valid Val_para(vNumMes,'FECMES',' ',22)
@ 03,15 SAY "Hasta:" GET vNumMes1 Valid Val_para(vNumMes1,'FECMES',' ',22)

*@ 05,01 SAY "  Cadena : "
*@ 05,15 GET vCadena PICTURE "@!"
*@ 07,01 SAY "  Partida : "
*@ 07,15 GET vCodPart PICTURE "@!"
@ 09,01 SAY "  Cliente : "
@ 09,15 GET vCodPrv PICTURE "@!"	VALID IIF(!EMPTY(vCodPrv),val_fun('Promae', 'CodPrv', 'NomPro',vCodPrv,1),.T.)

@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW ResCP

IF lastkey()#27 AND OkCancel#2
	IF cAno=1996
		SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND COMPAG.CODPRV=ALLTRIM(vCodPrv)
	ELSE
		SET FILTER TO BETW(VAL(NUMMES),VAL(vNumMes),VAL(vNumMes1)) AND COMPAG.CODPRV=ALLTRIM(vCodPrv)
	ENDIF
	DO Reporte WITH 2,"CPRet","Resumen de Comprobantes de Pago",2,.F.,.T.
ENDIF

CLOS DATA