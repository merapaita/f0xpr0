worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO SAI_LIB
DO Fox_Ambi
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'
SET PATH TO H:\SICGDATA\DATA2005
PUBLIC cMes,cAno,vOpcion,OkCancel,cProc
CLOS DATA

DO Pantalla
DO Escoge
IF !EMPTY(cProc)
	DO &cProc
ENDIF

PROCEDURE NotCon
*-------------------------------------------
*BUSCA CUENTAS EN EL DIARIO PARA COMPARARLAS
*-------------------------------------------
PUBLIC cCuenta,cOrden,cTipo
vIdx = SYS(3)+".Idx"
DO PantBusca
IF LASTKEY()=27 OR OkCancel = 2
	RETURN
ENDIF
DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"
USE DIARIO IN 0

DO CASE
	CASE !EMPTY(cTipo) AND !EMPTY(cCuenta) AND !EMPTY(cOrden)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Comprob=ALLTRIM(cTipo) AND Cuenta=ALLTRIM(cCuenta) AND Diario.ORDEN=ALLTRIM(cOrden)
	CASE !EMPTY(cCuenta) AND !EMPTY(cOrden)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Cuenta=ALLTRIM(cCuenta) AND Diario.ORDEN=ALLTRIM(cOrden)
	CASE !EMPTY(cTipo) AND !EMPTY(cOrden)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Comprob=ALLTRIM(cTipo)  AND Diario.ORDEN=ALLTRIM(cOrden)
	CASE !EMPTY(cTipo) AND !EMPTY(cCuenta)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Comprob=ALLTRIM(cTipo) AND Cuenta=ALLTRIM(cCuenta)
	CASE !EMPTY(cOrden)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Diario.ORDEN=ALLTRIM(cOrden)
	CASE !EMPTY(cCuenta)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Cuenta=ALLTRIM(cCuenta)
	CASE !EMPTY(cTipo)
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes AND Comprob=ALLTRIM(cTipo)
	OTHERWISE
		INDEX ON COMPROB+CUENTA+TIPO TO (vIdx) FOR PADL(MONT(FECEMI),2,'0')=cMes
ENDCASE
DO Espera WITH 2

* DETALLADO
DO REPORTE WITH 2, "Busca", ' ',1,.F.,.T.

IF YesNo('EXPORTAR TABLA')
	COPY TO xlbusca	FIELDS comprob,comadi,orden,item,cuenta,tipo,montos,docref
ENDIF

CLOS DATA
ERASE (vIdx)
RETURN

PROCEDURE COR_HC
*--------------
* ABRE TABLAS PARA CORREJIR H/C 
*
CLOS DATA
USE HojCon ORDER TAG Reg_E51 IN 1
USE IteHC  ORDER TAG IteHC1  IN 2
USE AstPre ORDER TAG AstPre5 IN 3
USE IteCla ORDER TAG IteCla1 IN 4
USE Diario ORDER TAG Diario1 IN 5
SELE AstPre
SET FILTER TO TIPDOC='H/C'
SELECT HojCon
SET RELATION TO NUMMES+NUMHC INTO ITEHC
SET RELATION TO NUMMES+NUMHC INTO ASTPRE ADDITIVE
SET RELATION TO '240'+NUMHC+NUMMES INTO DIARIO ADDITIVE

SELE IteHC
SET RELATION TO CodPart INTO IteCla
SELECT HojCon

RETURN

PROCEDURE COR_CP
*--------------
* ABRE TABLAS PARA CORREJIR R/I
*
CLOS DATA
USE COMPAG ORDER TAG COMPAG1 IN 1
USE ITECP  ORDER TAG ITECP1  IN 2
USE ASTPAT					 IN 3
USE ASTPRE ORDER TAG ASTPRE5 IN 4
USE CHEQUE ORDER TAG CHEQUE1 IN 5
USE RETEN  ORDER TAG RETEN1  IN 6
USE DIARIO ORDER TAG DIARIO1 IN 7
SELE ASTPRE
SET FILTER TO TIPDOC='C/P'
SELE ASTPAT
INDEX ON NUMMES+NUMREF+TIPDOC TO XX FOR TIPDOC='C/P'
SELECT COMPAG
SET RELATION TO NUMMES+NUMCP INTO ITECP
SET RELATION TO NUMMES+NUMCP INTO CHEQUE ADDITIVE
SET RELATION TO NUMMES+NUMCP INTO RETEN  ADDITIVE
SET RELATION TO NUMMES+NUMCP INTO ASTPAT ADDITIVE
SET RELATION TO NUMMES+NUMCP INTO ASTPRE ADDITIVE
SET RELATION TO '260'+NUMCP+NUMMES INTO DIARIO ADDITIVE

RETURN

PROCEDURE COR_RI
*--------------
* ABRE TABLAS PARA CORREJIR C/P
*
CLOS DATA
USE RECING ORDER TAG RECING1 IN 1
USE ITERI  ORDER TAG ITERI1  IN 2
USE ASTPAT					 IN 3
USE ASTPRE ORDER TAG ASTPRE5 IN 4
USE DIARIO ORDER TAG DIARIO1 IN 5
SELE ASTPRE
SET FILTER TO TIPDOC='R/I'
SELE ASTPAT
INDEX ON NUMMES+NUMREF+TIPDOC TO XX FOR TIPDOC='R/I'
SELECT RECING
SET RELATION TO PERIODO+NUMMES+NUMRI INTO ITERI
SET RELATION TO NUMMES+NUMRI INTO ASTPAT ADDITIVE
SET RELATION TO NUMMES+NUMRI INTO ASTPRE ADDITIVE
SET RELATION TO '250'+NUMRI+NUMMES INTO DIARIO ADDITIVE

RETURN

PROCEDURE ASTPRE
*---------------
*SACA LAS CUENTAS PRESUPUESTALES DE ASTPRE
*
cTipDoc = SPACE(3)
DO EscojeDocumento
CLOS DATA
USE ASTPRE IN 1 ALIAS AstPre ORDER TAG AstPre5
USE PARMAE IN 2 ALIAS PARMA  ORDER TAG PARMAE1
USE MaePre IN 3 			 ORDER TAG Maepre1
DO Espera WITH 1,'Por Favor Espere un Momento'
INDEX ON CODCAD+NUMMES+NUMREF TO XX FOR NUMMES=cMes AND TipDoc=cTipDoc
*INDEX ON TIPDOC+CODCAD+NUMMES+NUMREF TO XX FOR NUMMES=cMes AND (TIPDOC='C/P' OR TIPDOC='H/C' OR TIPDOC='R/I' OR TIPDOC='N/C')
SET FILTER TO NUMMES=cMes AND TipDoc=cTipDoc
DO Espera WITH 2
DO REPORTE WITH 2, "ASTPRE", ' ',1,.T.,.T.
CLOS DATA
RETURN


PROCEDURE ASTPAT
*---------------
PUBLIC cTipDoc,cTipRI,vTipoRI

*SACA LAS CUENTAS PATRIMONIALES DE ASTPAT
*
cTipDoc = SPACE(3)
cTipRI  = SPACE(3)
vTipoRI = 0
DO EscojeDocumento
CLOS DATA
IF LASTKEY()#27 AND OkCancel#2
	USE ASTPAT IN 1
	USE PARMAE IN 2 ALIAS PARMA ORDER TAG PARMAE1
	DO Espera WITH 1,'Por Favor Espere un Momento'
	INDEX ON TIPDOC+NUMMES+NUMREF TO XX FOR NUMMES=cMes AND TipDoc=cTipDoc AND CodCta=ALLTRIM(cCuenta)
	DO Espera WITH 2
	DO REPORTE WITH 2, "ASTPAT", ' ',1,.T.,.T.
	CLOS DATA
ENDIF
RETURN

PROCEDURE ASTORD
*---------------
*SACA LAS CUENTAS PATRIMONIALES DE ASTORD
*
cTipDoc = SPACE(3)
DO EscojeDocumento
CLOS DATA
USE ASTORD IN 1 ALIAS AstOrd ORDER TAG AstOrd1
USE PARMAE IN 2 ALIAS PARMA  ORDER TAG PARMAE1
DO Espera WITH 1,'Por Favor Espere un Momento'
SET FILTER TO NUMMES=cMes AND TipDoc=cTipDoc
DO Espera WITH 2
DO REPORTE WITH 2, "ASTORD", ' ',1,.T.,.T.
CLOS DATA
RETURN

PROCEDURE EscojeDocumento
*------------------------
PUBLIC cCuenta, vCuentas
USE Parmae IN 0 ORDER TAG Parmae1 ALIAS Parma
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO OkCancel
cTipo   = SPACE(3)
cCuenta	= SPACE(15)
vCuentas= 1

@ 01,01 SAY "Tipos de Documentos"
@ 03,05 GET cTipDoc PICTURE "!!!" VALID Val_para(cTipDoc,'TIPDOC',' ',22)
@ 05,05 GET vCuentas PICTURE '@*RH Todas las Cuentas;Una Cuenta' 
@ 07,05 SAY "           Cuenta: " GET cCuenta  PICTURE "@!" WHEN vCuentas=2
@ 09,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8


READ CYCLE
RELEASE WINDOW LIS
USE IN Parma
RETURN

PROCEDURE AGRART
*---------------
*Agrega medicamenos al stock de farmacia
*
CLOS DATA
USE ITEART IN 1 ORDER TAG ITEART1
USE STKALM IN 2 ORDER TAG STKALM2
SET FILTER TO CODART='59'
SET RELATION TO ALLTRIM(DESCRI) INTO STKALM
COPY STRU TO XX
USE XX IN 3 ALIAS ARTIC
GO TOP
SCAN
	SELE STKALM
	IF EOF()
		SELE ITEART
		SCATTER MEMVAR
		SELE ARTIC
		APPEND BLANK
		GATHER MEMVAR
	ENDIF
	SELE ITEART
ENDSCAN
*QQ
CLOS DATA
USE XX IN 1 ALIAS ARTIC
SELE ARTIC
INDEX ON CODART TO XX
USE STKALM IN 2
SELE STKALM
SET ORDER TO TAG STKALM1
SELE ARTIC
REPLACE CODART WITH TIPART+ALLTRIM(CODART) ALL
SET RELATION TO CODART INTO STKALM
GO TOP
SCAN
	SCATTER MEMVAR
	SELE STKALM
	IF EOF()
		APPEND BLANK
		GATHER MEMVAR
	ENDIF	
	SELE ARTIC
ENDSCAN
CLOS DATA

RETURN

PROCEDURE CORR_NC
*----------------
*PARA CORREGIR NOTAS DE CONTABILIDAD
*-----------------
CLOS DATA
USE COMPRO IN 1 ORDER TAG COMPRO1
USE DIARIO IN 2 ORDER TAG DIARIO11
SET RELATION TO COMPROB+COMADI INTO DIARIO
RETURN

PROCEDURE OS_Cp
**************
PRIVATE cNumMes
CLOS DATA
cNumMes = '05'

USE ordser IN 1
USE hojcon IN 2 ORDER TAG hojcon1
USE compag IN 3 ORDER TAG compag1

SELE OrdSer
INDEX ON NUMOS TO XX
SET RELATION TO Perhc+NumHc INTO HojCon
SELE HojCon
SET FILTER TO TipDoc='O/S'
SET RELATION TO NumMesCP+NumCP INTO Compag
SELE OrdSer
SET FILTER TO ComPag.NumMes = cNumMes
DO REPORTE WITH 2, "Seg_cp", ' ',1,.F.,.T.

CLOS DATA

RETURN

PROCEDURE OrdCom
****************
USE OrdCom IN 1 ORDER TAG OrdCom1
USE HojCon IN 2 ORDER TAG HojCon1
USE ComPag IN 3
USE Auxil  IN 4 ORDER TAG Auxil1 Alias Auxi

vIdx = SYS(3)+'.Idx'
SELE COMPAG
INDEX ON NumMesHC+NumHC TO (vIdx)

SELE HOJCON
SET RELATION TO NUMMES+NUMHC INTO ComPag ADDITIVE
SET SKIP TO ComPag

SELE OrdCom
SET FILTER TO NumMes = cMes
SET RELATION TO PERHC+NUMHC INTO HOJCON

DO REPORTE WITH 2, "OrdCOm", ' ',1,.F.,.T.

CLOS DATA

RETURN

PROCEDURE OrdSer
****************
USE OrdSer IN 1 ORDER TAG OrdSer1
USE HojCon IN 2 ORDER TAG HojCon1
USE ComPag IN 3
USE Auxil  IN 4 ORDER TAG Auxil1 Alias Auxi

vIdx = SYS(3)+'.Idx'
SELE COMPAG
INDEX ON NumMesHC+NumHC TO (vIdx)

SELE HOJCON
SET RELATION TO NUMMES+NUMHC INTO ComPag ADDITIVE
SET SKIP TO ComPag

SELE OrdSer
SET FILTER TO NumMes = cMes
SET RELATION TO PERHC+NUMHC INTO HOJCON

DO REPORTE WITH 2, "OrdSer", ' ',1,.F.,.T.

CLOS DATA

RETURN

*PROCEDURE OrdCom
****************
CLOS DATA

USE OrdCom IN 1 ORDER TAG OrdCom1
USE AstOrd IN 2 ORDER TAG AstOrd1
USE Auxil  IN 3 ORDER TAG Auxil1 Alias Auxi

SELE OrdCom
SET FILTER TO NumMes = cMes
SET RELATION TO Periodo+NumMes+NumOC+'O/C' INTO AstOrd ADDITIVE

DO REPORTE WITH 2, "OrdCOm", ' ',1,.F.,.T.

CLOS DATA

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
@ 01,20 GET vOpcion  FUNCTION '^ Ordenes de Compra;Ordenes de Servicio;Asts Patrimoniales;Asts Presupuestales;Asts Orden;Notas de Contabilidad;Asignaciones Ingresos;Asignaciones Gastos;Cruce Ejec. Clase 8;Recinos de Ingreso;Saldos EE.FF.;Cruce Ing. Diario'
@ 05,01 SAY "      Mes a Reportar : "
@ 05,25 GET cMes Valid Val_para(cMes,'FECMES',' ',22)
@ 07,01 SAY "  Periodo a Reportar : "
@ 07,25 GET cAno PICTURE "9,999"
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
	cRuta = "H:\SicgData\Data"+IIF(cAno<2000,RIGHT(STR(cano,4),2),STR(cano,4))
 ELSE
	DO StandBy WITH "Ruta Especificada no es correcta"
	RETURN .T.
ENDIF
SET PATH TO &cRuta
DO CASE
	CASE vOpcion = 1
		cProc = 'OrdCom'
	CASE vOpcion = 2
		cProc = 'OrdSer'
	CASE vOpcion = 3
		cProc = 'AstPat'
	CASE vOpcion = 4
		cProc = 'AstPre'
	CASE vOpcion = 5
		cProc = 'AstOrd'
	CASE vOpcion = 6
		cProc = 'NotCon'
	CASE vOpcion = 7
		cProc = 'AsiIng'
	CASE vOpcion = 8
		cProc = 'AsiGas'
	CASE vOpcion = 9
		cProc = 'CruE5C8'
	CASE vOpcion = 10
		cProc = 'RecIng'
	CASE vOpcion = 11
		cProc = 'SalEEFF'
	CASE vOpcion = 12
		cProc = 'CruIngDi'
	OTHERWISE
		cProc = ''
ENDCASE

PROCEDURE PantBusca
*-----------------
PUBLIC vTipo,cTipo,vCuentas,cCuenta, vOrdenes, cOrden
USE Parmae IN 0 ORDER TAG Parmae1 ALIAS Parma
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vOpcion,OkCancel

vTipo = 1
cTipo = SPACE(2)
vCuentas = 1
cCuenta  = SPAC(12)
vOrdenes = 1
cOrden = SPAC(2)
@ 01,05 GET vTipo    PICTURE '@*RH Todas los Tipos;Una Tipo' 
@ 03,05 SAY "Tipo de Documento: " GET cTipo    PICTURE "@!" WHEN vTipo=2
@ 05,05 GET vCuentas PICTURE '@*RH Todas las Cuentas;Una Cuenta' 
@ 07,05 SAY "           Cuenta: " GET cCuenta  PICTURE "@!" WHEN vCuentas=2
@ 09,05 GET vOrdenes PICTURE '@*RH Todos los Ordenes;Un Orden'
@ 11,05 SAY "            Orden: " GET cOrden  PICTURE "!!" WHEN vOrdenes=2 
@ 13,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
USE IN Parma
RETURN

*--------------------------
* Funci?n Val_aux : Ubica el c?digo Auxiliar por medio
* de un popup que esta ordenado en forma alfab?tica
* 
*----------------
FUNCTION val_aux
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
*filtro=alltrim(filtro)
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()
morden  = ORDER()

SELECT Auxi
SEEK filtro+mvalor
*IF .NOT. FOUND() .AND. mVariable<>'V'
IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(filtro)
	    SET FILTER TO tipo = filtro
	ENDIF    
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
			SET ORDER TO (morden)
		ENDIF
		RETURN
	ENDIF
	SET ORDER TO Auxil10
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	SELE Auxi
	SET ORDER TO Auxil1
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = Auxi.codigo
mdescr = SUBSTR( Auxi.descri, 1, mlong )
IF !EMPTY( malias )
	SELECT (malias)
    SET ORDER TO (morden)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici?n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici?n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
*	OTHERWISE            && En browse de edici?n
*		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE

PROCEDURE AsiGas
*-------------------------------------------
*BUSCA Asignaciones Presup[uestales de Gastos
*-------------------------------------------
IF LASTKEY()=27 OR OkCancel = 2
	RETURN
ENDIF
DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"
vIdx = SYS(3)+'.Idx'
USE IteHC
INDEX ON CodCad+CodPart+CodFte+NumMes to (vIdx)
SET FILTER TO (NumMes = cMes OR NumMesHM=cMes) AND Estado#'99'

DO Espera WITH 2

DO REPORTE WITH 2, "AsiGas", ' ',1,.F.,.T.

CLOS DATA
ERASE (vIdx)
RETURN

PROCEDURE AsiIng
*-------------------------------------------
*BUSCA Asignaciones Presup[uestales de Gastos
*-------------------------------------------
IF LASTKEY()=27 OR OkCancel = 2
	RETURN
ENDIF
DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"
vIdx = SYS(3)+'.Idx'
USE IteRI
INDEX ON Tipo+CodPart+NumRI to (vIdx)
SET FILTER TO (NumMes = cMes) AND Estado#'99'

DO Espera WITH 2

DO REPORTE WITH 2, "AsiIng", ' ',1,.F.,.T.

CLOS DATA
ERASE (vIdx)
RETURN


*PROCEDURE CruE5C8
*----------------
*-------------------------------------
* CruE5C8.Prg
* Cruce de E5 con Clase 8 Diario
*-------------------------------------
nMes = Val(cMes)

CLOS DATA
USE Diario IN 1
USE IteHC   IN 2 
USE Lista8  IN 3 EXCLU

SELE Diario
INDEX ON Cuenta+PADL(MONT(FECEMI),2,'0') TO YY FOR MONT(FECEMI)=nMes

SELE IteHC
INDEX ON CodCad+CodFte+IIF(Estado='92',NumMesHM,NumMes) TO XX FOR Estado#'99' AND IIF(Estado='92',NumMesHM,NumMes)=cMes

SELE Lista8
ZAP

SELE Diario

DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"

FOR J = 1 TO 2
	IF J = 1
		* RDR
		cfte = '0201'
		cFte1 = '09'
	ENDIF
	IF J = 2
		* DYT
		cfte = '0402'
		cfte1 = '13'
	ENDIF
	FOR i = 1 to 20
		xCad = padl(alltrim(STR(i)),2,'0')
		xCta84 = padR("8401" + cFte + xCad,15,'0')
		xCta86 = padR("8601" + cFte + xCad,15,'0')
		
		SEEK xcta84+cMes
		xSaldo84 = 0
		scan for cuenta=xCta84 and MONT(FecEmi) <= nMes
			xSaldo84 = xSaldo84 + Montos*IIF(Tipo='D',1,-1)
		endscan
		
		SEEK xcta86+cMes
		xSaldo86 = 0
		scan for cuenta=xCta86 and MONT(FecEmi) <= nMes
			xSaldo86 = xSaldo86 + Montos*IIF(Tipo='D',1,-1)
		endscan
		
		SELE IteHC
		xEjec = 0
		cCad = padl(alltrim(str(i)),4,'0')
		SEEK cCad+cFte1
		scan WHILE CodCad = cCad AND CodFte = cFte1 AND NumMes <= cMes
			xEjec = xEjec + ValPart*IIF(TipOpe='-',-1,1)
		endscan
			
		SELE Lista8
		APPEND BLANK
		REPLACE Mes WITH nMes, Cadena WITH str(I,2), Fte WITH cFte1, c84 WITH xSaldo84, c86 WITH xSaldo86, Ejec WITH xEjec
		SELE Diario
	endfor
ENDFOR

DO Espera WITH 2

SELE Lista8

DO REPORTE WITH 2, "CRUE5BC", ' ',1,.F.,.T.
CLOS DATA

RETURN

PROCEDURE SalEEFF
*----------------
*-------------------------------------
* SalEEFF.Prg
* Notas a los Estados Financieros
*-------------------------------------
USE PARMAE IN 1 ALIAS PARMA  ORDER TAG PARMAE1

DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW Lis

STORE 1 TO OkCancel
cCuenta	= SPACE(15)
nMes = 0
cAnoAnt = 0
cAnoNew = 0

@ 01,05 SAY "Periodo Anterior:" GET cAnoAnt FUNCTION '^ 2009;2010;2011;2012'
@ 04,05 SAY "  Periodo Actual:" GET cAnoNew FUNCTION '^ 2009;2010;2011;2012'
@ 07,05 SAY "  Mes de Proceso:" GET cMes    Valid Val_para(cMes,'FECMES',' ',22)	&&WHEN vCuentas=2
@ 09,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

DEACTIVATE WINDOWS Lis
RELEASE WINDOW Lis

nMes = Val(cMes)

vSalAnt = "H:\SICGDATA\DATA" +ValAno(cAnoAnt)+"\Saldona.dbf"
vSalAct = "H:\SICGDATA\DATA" +ValAno(cAnoNew)+"\Saldona.dbf"
vCtaAnt = "H:\SICGDATA\DATA" +ValAno(cAnoAnt)+"\Cuentas.dbf"
vCtaAct = "H:\SICGDATA\DATA" +ValAno(cAnoNew)+"\Cuentas.dbf"
CLOS DATA
USE (vSalAnt) IN 1 ORDER TAG Saldona4 	ALIAS SalAnt
USE (vSalAct) IN 2 ORDER TAG Saldona4 	ALIAS SalAct
USE (vCtaAnt) IN 3 ORDER TAG Cuentas1 	ALIAS CtaAnt
USE (vCtaAct) IN 4 ORDER TAG Cuentas1 	ALIAS CtaAct
USE SalEEFF IN 5 ORDER TAG SalEEFF1	EXCLUSIVE

SELE SalAnt
SET FILTER TO Mes = nMes
SET RELATION TO Cuenta INTO CtaAnt

SELE SalAct
SET FILTER TO Mes = nMes
SET RELATION TO Cuenta INTO CtaAct

SELE SalEEFF
ZAP

DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"

SELE SalAnt
SCAN
	SELE SalEEFF
	IF !SEEK(SalAnt.Cuenta)
*		xcta = Cuenta(SalAnt.Cuenta)
		IF f_appd()
			REPLACE Cuenta WITH SalAnt.Cuenta
			REPLACE Descri WITH CtaAnt.Descri
			REPLACE DSalAnt WITH SalAnt.DSaldona
			REPLACE HSalAnt WITH SalAnt.HSaldona
		ENDIF
	ELSE
		REPLACE DSalAnt WITH SalAnt.DSaldona
		REPLACE HSalAnt WITH SalAnt.HSaldona
	ENDIF
	SELE SalAnt
ENDSCAN

SELE SalAct
SCAN
	SELE SalEEFF
	IF !SEEK(SalAct.Cuenta)
*		xcta = Cuenta(SalAct.Cuenta)
		IF f_appd()
			REPLACE Cuenta WITH SalAct.Cuenta
			REPLACE Descri WITH CtaAct.Descri
			REPLACE DSalAct WITH SalAct.DSaldona
			REPLACE HSalAct WITH SalAct.HSaldona
		ENDIF
	ELSE
		REPLACE DSalAct WITH SalAct.DSaldona
		REPLACE HSalAct WITH SalAct.HSaldona
	ENDIF
	SELE SalAct
ENDSCAN

SELE SalEEFF
SCAN
	REPLACE Cuenta WITH Cuenta(Cuenta)
ENDSCAN

DO Espera WITH 2
SELE SalEEFF
COPY TO EEFF TYPE XLS
DO StandBy WITH "Tabla Generada Satisfactoriamente"
RETURN

FUNCTION Cuenta
*--------------
PARAMETER xCuenta
DO CASE
	CASE SUBSTR(xCuenta,5,11)='00000000000'
	   RETURN LEFT(xCuenta,4)
	CASE SUBSTR(xCuenta,7,9)='000000000'
	   RETURN LEFT(xCuenta,6)
	CASE SUBSTR(xCuenta,9,7)='0000000'
	   RETURN LEFT(xCuenta,8)
	CASE SUBSTR(xCuenta,11,5)='00000'
	   RETURN LEFT(xCuenta,10)
	CASE SUBSTR(xCuenta,13,3)='000'
	   RETURN LEFT(xCuenta,12)
 	OTHERWISE
	   RETURN xCuenta
ENDCASE
SELE &xAli
RETURN mRet

PROCEDURE ValAno
*----------------
PARAMETERS xAno
DO CASE
	CASE xAno = 1
		mRet = "2009"
	CASE xAno = 2
		mRet = "2010"
	CASE xAno = 3
		mRet = "2011"
	CASE xAno = 4
		mRet = "2012"
ENDCASE
RETURN mRet

PROCEDURE CruE5C8
*----------------
*-------------------------------------
* CruE5C8.Prg
* Cruce de E5 con Clase 8 Diario
*-------------------------------------
nMes = Val(cMes)

CLOS DATA
USE Diario IN 1
USE IteHC  IN 2 
USE MaePre IN 3 ORDER TAG MaePre1
USE Lista8 IN 4 EXCLU

SELE Diario
INDEX ON Cuenta+PADL(MONT(FECEMI),2,'0') TO YY for VAL(LEFT(COMPROB,2))<95

SELE IteHC
INDEX ON CodCad+CodFte+IIF(Estado='92',NumMesHM,NumMes) TO XX FOR Estado#'99' AND cMes>=IIF(Estado='92',NumMesHM,NumMes)

SET RELATION TO RIGH(STR(cAno,4),2)+UniGes+UniEje+CodCad INTO MaePre

SELE Lista8
ZAP

SELE Diario

DO Espera WITH 1,"POR FAVOR ESPERE UN MOMENTO"

*set step on

FOR J = 1 TO 3
	IF J = 1
		* RDR
		cfte = '0201'
		cFte1 = '09'
	ENDIF
	IF J = 2
		* DYT
		cfte = '0402'
		cfte1 = '13'
	ENDIF
	IF J = 3
		* 
		cfte = '0301'
		cfte1 = '19'
	ENDIF
	FOR i = 1 to 25
		xCad = padl(alltrim(STR(i)),2,'0')
		xCta84 = padR("8401" + cFte + xCad,15,'0')
		xCta86 = padR("8601" + cFte + xCad,15,'0')
		
		DO Espera WITH 1,xCta84+" "+xCta86
		
		SEEK xcta84+cMes
		xSaldo84 = 0
		scan for cuenta=xCta84 and MONT(FecEmi) <= nMes
			IF YEAR(fecemi)#cAno
				DO StandBy WITH "Error en Fecha. N/C " + Comprob + ComAdi
				LOOP
			ENDIF
			xSaldo84 = xSaldo84 + Montos*IIF(Tipo='D',1,-1)
		endscan
		
		SEEK xcta86+cMes
		xSaldo86 = 0
		scan for cuenta=xCta86 and MONT(FecEmi) <= nMes
			IF YEAR(fecemi)#cAno
				DO StandBy WITH "Error en Fecha. N/C " + Comprob + ComAdi
				LOOP
			ENDIF
			xSaldo86 = xSaldo86 + Montos*IIF(Tipo='D',1,-1)
		endscan
		
		SELE IteHC
		xEjec = 0
		cCad = padl(alltrim(str(i)),4,'0')
		SEEK cCad+cFte1
		scan WHILE CodCad = cCad AND CodFte = cFte1 AND NumMes <= cMes
			IF !(IteHC.CodCom=MaePre.CodCom AND IteHC.CodMet=MaePre.CodMet)
				DO StandBy WITH "Error en Cadena. H/C " + NumHC +"."+ NumMes
				LOOP
			ENDIF
			xEjec = xEjec + ValPart*IIF(TipOpe='-',-1,1)
		endscan
			
		SELE Lista8
		IF EMPTY(xSaldo84) AND EMPTY(xSaldo86) AND EMPTY(xEjec)
			*
		ELSE
			APPEND BLANK
			REPLACE Mes WITH nMes, Cadena WITH str(I,2), Fte WITH cFte1, c84 WITH xSaldo84, c86 WITH xSaldo86, Ejec WITH xEjec
		ENDIF
		SELE Diario
*		SELE Saldona
	endfor
ENDFOR

DO Espera WITH 2

SELE Lista8

DO REPORTE WITH 2, "CRUE5BC", ' ',1,.F.,.T.
CLOS DATA

RETURN

PROCEDURE RecIng
*---------------
*SACA LA Recibos de Ingreso CUENTAS PATRIMONIALES DE ASTPAT
*
USE RecIng   IN 1						ALIAS RecIng
USE ASTPAT   IN 2 ORDER TAG AstPat14	ALIAS AstPat
USE AsiAutRI IN 3 ORDER TAG AsiAutRi1	ALIAS AsiAut
USE Parmae   IN 4 ORDER TAG Parmae1		ALIAS Parma

DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO OkCancel
m.TipRI  = SPACE(3)
vTipoRI = 0
*@ 03,05 GET cTipDoc PICTURE "!!!" VALID Val_para(cTipDoc,'TIPDOC',' ',22)
@ 05,05 GET vTipoRI PICTURE '@*RH Todos los tipos;Un tipo'
@ 07,05 SAY 'Tipo R/I'GET m.TipRI  PICTURE "!!!" WHEN vTipoRI=2 VALID Val_TipRI(7,18)
@ 09,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
IF LASTKEY()#27 AND OkCancel#2
	SELE RecIng
	DO Espera WITH 1,'Por Favor Espere un Momento'
	INDEX ON Periodo+NumMes+TipRI TO vIdx FOR NumMes=cMes AND IIF(vTipoRI=2,TipRI = m.TipRI,.T.)
	SET RELATION TO Periodo+NumMes+NumRI INTO AstPat
	SET SKIP TO AstPat
	DO Espera WITH 2
	
	DO REPORTE WITH 2, "RecIng", ' ',1,.T.,.T.
ENDIF
CLOS DATA
RETURN

FUNCTION Val_TipRI
*-----------------
PARAMETERS vFil,vCol
PRIVATE mBus,mImp,cAlias
STORE .F. TO MbUS,mImp
cAlias = ALIAS()
IF EMPTY(m.TipRI)
	mBus = .T.
 ELSE
	IF SEEK('R/I'+m.TipRI,"AsiAut")
		mImp = .T.
	 ELSE
		mBus = .T.
	ENDIF
ENDIF

IF mBus
	SELE AsiAut
	SET ORDER TO AsiAutRI2
	DEFINE POPUP v__xx FROM 1,30 TO 12,79 PROMPT FIELD Descri
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
	
	m.TipRI = Codigo
	
	SET ORDER TO AsiAutRi1
	mImp = .T.
ENDIF

IF mImp
	@ vFil,vCol SAY AsiAut.descri
ENDIF
SELE (cAlias)
RETURN

FUNCTION DesRI
*-------------
PRIVATE cAlias, mRet
cAlias = ALIAS()
IF SEEK("R/I"+RecIng.TipRI,'AsiAut')
	mRet = AsiAut.Descri
ENDIF

RETURN mRet

PROCEDURE CruIngDi
*-----------------
*PARAMETERS nMes
*nMes = 2
nMes = Val(cMes)
*set dele on
*set exclu off
use iteri in 1 order tag iteri1
use diario in 2 order tag diario11
SELE Iteri
SET FILTER TO VAL(NumMes) = nMes
SELE Diario
Index on Comprob+ComAdi TO xx FOR Comprob='25' and cuenta='8' AND MONT(FecEmi)=nMes

SELE Iteri
GO TOP
IF EOF()
	Do StandBy WITH "No Hay Registros para Procesar"
	CLOS DATA
	RETURN
ENDIF

SCAN
	m.NumRI = NumRI
	m.NumMes = NumMes
	m.ValPart = 0
	SCAN WHILE NumRI = m.NumRI
		DO CASE
			CASE EMPTY(Iteri.Tipo)
				lErr = .T.
			CASE Iteri.Tipo = 'P' AND EMPTY(CodPart)
				lErr = .T.
			CASE Iteri.Tipo = 'O' AND !EMPTY(CodPart)
				lErr = .T.
			OTHERWISE
				lErr = .F.
		ENDCASE
		IF lErr
			WAIT WIND "Error en R/I "+NumRI
		ELSE
			IF Iteri.Tipo = 'P' AND !EMPTY(CodPart)
				m.ValPart = m.ValPart + ImpParc
			ENDIF
		ENDIF
	ENDSCAN
	SELE Diario
	xComp = "25" + PADL(ALLTRIM(STR(VAL(m.NumRI))),5,'0') + m.NumMes
	SEEK xComp
	nMtos = 0
	IF FOUND()
		m.ComProb = Comprob
		SCAN WHILE Comprob = m.Comprob
			nMtos = nMtos + Montos
		ENDSCAN
		IF m.ValPart # nMtos/2
			WAIT WIND "Error en N/C " + m.Comprob
			wait wind str(m.valpart,2)+ ' '+str(nMtos/2,2)
		ENDIF
	ELSE
		WAIT WIND "No Existe Recibo "+xComp
	ENDIF
	SELE Iteri
	SKIP -1
ENDSCAN