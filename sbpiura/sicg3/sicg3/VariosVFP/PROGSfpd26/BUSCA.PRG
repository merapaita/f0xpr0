*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) =  ;
         'ON', .T., .F.)
SET PROCEDURE TO SAI_LIB
DO fox_ambi
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'
SET PATH TO ..\..\DATA2013
PUBLIC cmes, cano, vopcion,  ;
       okcancel, cproc
CLOSE DATABASES
DO pantalla
DO escoge
IF  .NOT. EMPTY(cproc)
     DO &cProc
ENDIF
*
PROCEDURE notcon
PUBLIC ccuenta, corden, ctipo
vidx = SYS(3) + '.Idx'
DO pantbusca
IF LASTKEY() = 27 .OR. okcancel = 2
     RETURN
ENDIF
DO espera WITH 1, 'POR FAVOR ESPERE UN MOMENTO'
USE IN 0 DIARIO
DO CASE
     CASE  .NOT. EMPTY(ctipo) .AND.  .NOT. EMPTY(ccuenta) .AND.  .NOT. EMPTY(corden)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. comprob = ALLTRIM(ctipo) .AND. cuenta = ALLTRIM(ccuenta) .AND. diario.orden = ALLTRIM(corden)
     CASE  .NOT. EMPTY(ccuenta) .AND.  .NOT. EMPTY(corden)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. cuenta = ALLTRIM(ccuenta) .AND. diario.orden = ALLTRIM(corden)
     CASE  .NOT. EMPTY(ctipo) .AND.  .NOT. EMPTY(corden)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. comprob = ALLTRIM(ctipo) .AND. diario.orden = ALLTRIM(corden)
     CASE  .NOT. EMPTY(ctipo) .AND.  .NOT. EMPTY(ccuenta)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. comprob = ALLTRIM(ctipo) .AND. cuenta = ALLTRIM(ccuenta)
     CASE  .NOT. EMPTY(corden)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. diario.orden = ALLTRIM(corden)
     CASE  .NOT. EMPTY(ccuenta)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. cuenta = ALLTRIM(ccuenta)
     CASE  .NOT. EMPTY(ctipo)
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes .AND. comprob = ALLTRIM(ctipo)
     OTHERWISE
          INDEX ON comprob + cuenta + tipo TO (vidx) FOR PADL(MONTH(fecemi), 2, '0') = cmes
ENDCASE
DO espera WITH 2
DO reporte WITH 2, 'Busca', ' ', 1, .F., .T.
IF yesno('EXPORTAR TABLA')
     COPY TO xlbusca FIELDS comprob, comadi, orden, item, cuenta, tipo, montos, docref
ENDIF
CLOSE DATABASES
ERASE (vidx)
RETURN
*
PROCEDURE cor_hc
CLOSE DATABASES
USE IN 1 HojCon ORDER Reg_E51
USE IN 2 IteHC ORDER IteHC1
USE IN 3 AstPre ORDER AstPre5
USE IN 4 IteCla ORDER IteCla1
USE IN 5 Diario ORDER Diario1
SELECT astpre
SET FILTER TO tipdoc = 'H/C'
SELECT hojcon
SET RELATION TO nummes + numhc INTO itehc
SET RELATION TO nummes + numhc INTO astpre;
ADDITIVE
SET RELATION TO '240' + numhc + nummes;
INTO diario ADDITIVE
SELECT itehc
SET RELATION TO codpart INTO itecla
SELECT hojcon
RETURN
*
PROCEDURE cor_cp
CLOSE DATABASES
USE IN 1 COMPAG ORDER COMPAG1
USE IN 2 ITECP ORDER ITECP1
USE IN 3 ASTPAT
USE IN 4 ASTPRE ORDER ASTPRE5
USE IN 5 CHEQUE ORDER CHEQUE1
USE IN 6 RETEN ORDER RETEN1
USE IN 7 DIARIO ORDER DIARIO1
SELECT astpre
SET FILTER TO tipdoc = 'C/P'
SELECT astpat
INDEX ON nummes + numref + tipdoc TO XX FOR tipdoc = 'C/P'
SELECT compag
SET RELATION TO nummes + numcp INTO itecp
SET RELATION TO nummes + numcp INTO cheque ADDITIVE
SET RELATION TO nummes + numcp INTO reten ADDITIVE
SET RELATION TO nummes + numcp INTO astpat ADDITIVE
SET RELATION TO nummes + numcp INTO astpre ADDITIVE
SET RELATION TO '260' + numcp + nummes INTO diario ADDITIVE
RETURN
*
PROCEDURE cor_ri
CLOSE DATABASES
USE IN 1 RECING ORDER RECING1
USE IN 2 ITERI ORDER ITERI1
USE IN 3 ASTPAT
USE IN 4 ASTPRE ORDER ASTPRE5
USE IN 5 DIARIO ORDER DIARIO1
SELECT astpre
SET FILTER TO tipdoc = 'R/I'
SELECT astpat
INDEX ON nummes + numref + tipdoc TO XX FOR tipdoc = 'R/I'
SELECT recing
SET RELATION TO periodo + nummes + numri INTO iteri
SET RELATION TO nummes + numri INTO astpat ADDITIVE
SET RELATION TO nummes + numri INTO astpre ADDITIVE
SET RELATION TO '250' + numri + nummes INTO diario ADDITIVE
RETURN
*
PROCEDURE astpre
ctipdoc = SPACE(3)
DO escojedocu
CLOSE DATABASES
USE IN 1 ASTPRE ALIAS astpre ORDER AstPre5
USE IN 2 PARMAE ALIAS parma ORDER PARMAE1
USE IN 3 MaePre ORDER Maepre1
DO espera WITH 1, 'Por Favor Espere un Momento'
INDEX ON codcad + nummes + numref TO XX FOR nummes = cmes .AND. tipdoc = ctipdoc
SET FILTER TO nummes = cmes .AND. tipdoc = ctipdoc
DO espera WITH 2
DO reporte WITH 2, 'ASTPRE', ' ', 1, .T., .T.
CLOSE DATABASES
RETURN
*
PROCEDURE astpat
PUBLIC ctipdoc, ctipri, vtipori
ctipdoc = SPACE(3)
ctipri = SPACE(3)
vtipori = 0
DO escojedocu
CLOSE DATABASES
IF LASTKEY() <> 27 .AND. okcancel <> 2
     USE IN 1 ASTPAT
     USE IN 2 PARMAE ALIAS parma ORDER PARMAE1
     DO espera WITH 1, 'Por Favor Espere un Momento'
     INDEX ON tipdoc + nummes + numref TO XX FOR nummes = cmes .AND. tipdoc = ctipdoc .AND. codcta = ALLTRIM(ccuenta)
     DO espera WITH 2
     DO reporte WITH 2, 'ASTPAT', ' ', 1, .T., .T.
     CLOSE DATABASES
ENDIF
RETURN
*
PROCEDURE astord
ctipdoc = SPACE(3)
DO escojedocu
CLOSE DATABASES
USE IN 1 ASTORD ALIAS astord ORDER AstOrd1
USE IN 2 PARMAE ALIAS parma ORDER PARMAE1
DO espera WITH 1, 'Por Favor Espere un Momento'
SET FILTER TO nummes = cmes .AND. tipdoc = ctipdoc
DO espera WITH 2
DO reporte WITH 2, 'ASTORD', ' ', 1, .T., .T.
CLOSE DATABASES
RETURN
*
PROCEDURE escojedocu
PUBLIC ccuenta, vcuentas
USE IN 0 Parmae ALIAS parma ORDER Parmae1
DEFINE WINDOW lis FROM 5, 15 TO 18, 65 FLOAT TITLE '' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO okcancel
ctipo = SPACE(3)
ccuenta = SPACE(15)
vcuentas = 1
@ 01, 01 SAY 'Tipos de Documentos'
@ 03, 05 GET ctipdoc PICTURE '!!!' VALID val_para(ctipdoc,'TIPDOC',' ',22)
@ 05, 05 GET vcuentas PICTURE '@*RH Todas las Cuentas;Una Cuenta'
@ 07, 05 SAY '           Cuenta: ' GET ccuenta PICTURE '@!' WHEN vcuentas = 2
@ 09, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
USE IN parma
RETURN
*
PROCEDURE agrart
CLOSE DATABASES
USE IN 1 ITEART ORDER ITEART1
USE IN 2 STKALM ORDER STKALM2
SET FILTER TO codart = '59'
SET RELATION TO ALLTRIM(descri) INTO stkalm
COPY TO XX STRUCTURE
USE IN 3 XX ALIAS artic
GOTO TOP
SCAN
     SELECT stkalm
     IF EOF()
          SELECT iteart
          SCATTER MEMVAR
          SELECT artic
          APPEND BLANK
          GATHER MEMVAR
     ENDIF
     SELECT iteart
ENDSCAN
CLOSE DATABASES
USE IN 1 XX ALIAS artic
SELECT artic
INDEX ON codart TO XX
USE IN 2 STKALM
SELECT stkalm
SET ORDER TO STKALM1
SELECT artic
REPLACE codart WITH tipart + ALLTRIM(codart) ALL
SET RELATION TO codart INTO stkalm
GOTO TOP
SCAN
     SCATTER MEMVAR
     SELECT stkalm
     IF EOF()
          APPEND BLANK
          GATHER MEMVAR
     ENDIF
     SELECT artic
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE corr_nc
CLOSE DATABASES
USE IN 1 COMPRO ORDER COMPRO1
USE IN 2 DIARIO ORDER DIARIO11
SET RELATION TO comprob + comadi INTO;
diario
RETURN
*
PROCEDURE os_cp
PRIVATE cnummes
CLOSE DATABASES
cnummes = '05'
USE IN 1 ordser
USE IN 2 hojcon ORDER hojcon1
USE IN 3 compag ORDER compag1
SELECT ordser
INDEX ON numos TO XX
SET RELATION TO perhc + numhc INTO hojcon
SELECT hojcon
SET FILTER TO tipdoc = 'O/S'
SET RELATION TO nummescp + numcp INTO compag
SELECT ordser
SET FILTER TO compag.nummes = cnummes
DO reporte WITH 2, 'Seg_cp', ' ',  1, .F., .T.
CLOSE DATABASES
RETURN
*
PROCEDURE ordcom
USE IN 1 OrdCom ORDER OrdCom1
USE IN 2 HojCon ORDER HojCon1
USE IN 3 ComPag
USE IN 4 Auxil ALIAS auxi ORDER Auxil1
vidx = SYS(3) + '.Idx'
SELECT compag
INDEX ON nummeshc + numhc TO (vidx)
SELECT hojcon
SET RELATION TO nummes + numhc INTO compag ADDITIVE
SET SKIP TO compag
SELECT ordcom
SET FILTER TO nummes = cmes
SET RELATION TO perhc + numhc INTO hojcon
DO reporte WITH 2, 'OrdCOm', ' ', 1, .F., .T.
CLOSE DATABASES
RETURN
*
PROCEDURE ordser
USE IN 1 OrdSer ORDER OrdSer1
USE IN 2 HojCon ORDER HojCon1
USE IN 3 ComPag
USE IN 4 Auxil ALIAS auxi ORDER Auxil1
vidx = SYS(3) + '.Idx'
SELECT compag
INDEX ON nummeshc + numhc TO (vidx)
SELECT hojcon
SET RELATION TO nummes + numhc INTO compag ADDITIVE
SET SKIP TO compag
SELECT ordser
SET FILTER TO nummes = cmes
SET RELATION TO perhc + numhc INTO hojcon
DO reporte WITH 2, 'OrdSer', ' ', 1, .F., .T.
CLOSE DATABASES
RETURN

*
PROCEDURE pantalla
USE IN 0 Parmae ALIAS parma ORDER Parmae1
SELECT parma
DEFINE WINDOW lis FROM 5, 15 TO 18, 65 FLOAT TITLE '' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vopcion, okcancel
cmes = SPACE(2)
cano = YEAR(DATE())
@ 02, 01 SAY '       Opciones : '
@ 01, 20 GET vopcion FUNCTION '^ Ordenes de Compra;Ordenes de Servicio;Asts Patrimoniales;Asts Presupuestales;Asts Orden;Notas de Contabilidad;Asignaciones Ingresos;Asignaciones Gastos;Cruce Ejec. Clase 8;Recinos de Ingreso;Saldos EE.FF.;Cruce Ing. Diario'
@ 05, 01 SAY '      Mes a Reportar : '
@ 05, 25 GET cmes VALID val_para(cmes,'FECMES',' ',22)
@ 07, 01 SAY '  Periodo a Reportar : '
@ 07, 25 GET cano PICTURE '9,999'
@ 10, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
USE IN parma
RETURN
*
FUNCTION escoge
IF LASTKEY() = 27 .OR. okcancel = 2
     cproc = ''
     RETURN
ENDIF
IF  .NOT. EMPTY(cano)
     cruta = '..\..\Data' + IIF(cano < 2000, RIGHT(STR(cano, 4), 2), STR(cano, 4))
*     cruta = 'H:\SicgData\Data' + IIF(cano < 2000, RIGHT(STR(cano, 4), 2), STR(cano, 4))
ELSE
     DO standby WITH 'Ruta Especificada no es correcta'
     RETURN .T.
ENDIF
SET PATH TO &cRuta
DO CASE
     CASE vopcion = 1
          cproc = 'OrdCom'
     CASE vopcion = 2
          cproc = 'OrdSer'
     CASE vopcion = 3
          cproc = 'AstPat'
     CASE vopcion = 4
          cproc = 'AstPre'
     CASE vopcion = 5
          cproc = 'AstOrd'
     CASE vopcion = 6
          cproc = 'NotCon'
     CASE vopcion = 7
          cproc = 'AsiIng'
     CASE vopcion = 8
          cproc = 'AsiGas'
     CASE vopcion = 9
          cproc = 'CruE5C8'
     CASE vopcion = 10
          cproc = 'RecIng'
     CASE vopcion = 11
          cproc = 'SalEEFF'
     CASE vopcion = 12
          cproc = 'CruIngDi'
     OTHERWISE
          cproc = ''
ENDCASE
*
PROCEDURE pantbusca
PUBLIC vtipo, ctipo, vcuentas, ccuenta, vordenes, corden
USE IN 0 Parmae ALIAS parma ORDER Parmae1
DEFINE WINDOW lis FROM 5, 15 TO 20, 65 FLOAT TITLE '' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vopcion, okcancel
vtipo = 1
ctipo = SPACE(2)
vcuentas = 1
ccuenta = SPACE(12)
vordenes = 1
corden = SPACE(2)
@ 01, 05 GET vtipo PICTURE '@*RH Todas los Tipos;Una Tipo'
@ 03, 05 SAY 'Tipo de Documento: ' GET ctipo PICTURE '@!' WHEN vtipo = 2
@ 05, 05 GET vcuentas PICTURE '@*RH Todas las Cuentas;Una Cuenta'
@ 07, 05 SAY '           Cuenta: ' GET ccuenta PICTURE '@!' WHEN vcuentas = 2
@ 09, 05 GET vordenes PICTURE '@*RH Todos los Ordenes;Un Orden'
@ 11, 05 SAY '            Orden: ' GET corden PICTURE '!!' WHEN vordenes = 2
@ 13, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
USE IN parma
RETURN
*
FUNCTION val_aux
PARAMETER mvalor, filtro, mvariable, mcol, mlong, mdist
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 4
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 5
          mdist = 6
ENDCASE
malias = ALIAS()
morden = ORDER()
SELECT auxi
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT. mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     IF  .NOT. EMPTY(filtro)
          SET FILTER TO tipo = filtro
     ENDIF
     GOTO TOP
     IF EOF()
          DO standby WITH 'No existen Registros para Procesar'
          SET FILTER TO
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
               SET ORDER TO (morden)
          ENDIF
          RETURN
     ENDIF
     SET ORDER TO Auxil10
     DEFINE POPUP parametro FROM 03, 40 PROMPT FIELDS SUBSTR(descri, 1, 40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     SELECT auxi
     SET ORDER TO Auxil1
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = auxi.codigo
mdescr = SUBSTR(auxi.descri, 1, mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
     SET ORDER TO (morden)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY mvalor
          @ ROW(), mcol + mdist SAY mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY mdescr
          RETURN ' '
     CASE mvariable = 'V'
          @ ROW(), COL() SAY mvalor
          RETURN mdescr
     CASE mvariable = 'D'
          RETURN mdescr
     CASE mvariable = 'Z'
          RETURN mdescr
     CASE mvariable = 'C'
          RETURN .T.
          RETURN .T.
ENDCASE
*
PROCEDURE asigas
IF LASTKEY() = 27 .OR. okcancel = 2
     RETURN
ENDIF
DO espera WITH 1, 'POR FAVOR ESPERE UN MOMENTO'
vidx = SYS(3) + '.Idx'
USE IteHC
INDEX ON codcad + codpart + codfte + nummes TO (vidx)
SET FILTER TO (nummes = cmes .OR. nummeshm = cmes);
.AND. estado <> '99'
DO espera WITH 2
DO reporte WITH 2, 'AsiGas', ' ', 1, .F., .T.
CLOSE DATABASES
ERASE (vidx)
RETURN
*
PROCEDURE asiing
IF LASTKEY() = 27 .OR. okcancel = 2
     RETURN
ENDIF
DO espera WITH 1, 'POR FAVOR ESPERE UN MOMENTO'
vidx = SYS(3) + '.Idx'
USE IteRI
INDEX ON tipo + codpart + numri TO (vidx)
SET FILTER TO (nummes = cmes);
.AND. estado <> '99'
DO espera WITH 2
DO reporte WITH 2, 'AsiIng', ' ', 1, .F., .T.
CLOSE DATABASES
ERASE (vidx)
RETURN
nmes = VAL(cmes)
CLOSE DATABASES
USE IN 1 Diario
USE IN 2 IteHC
USE IN 3 EXCLUSIVE Lista8
SELECT diario
INDEX ON cuenta + PADL(MONTH(fecemi), 2, '0') TO YY FOR MONTH(fecemi) = nmes
SELECT itehc
INDEX ON codcad + codfte + IIF(estado = '92', nummeshm, nummes) TO XX FOR estado <> '99' .AND. IIF(estado = '92', nummeshm, nummes) = cmes
SELECT lista8
ZAP
SELECT diario
DO espera WITH 1, 'POR FAVOR ESPERE UN MOMENTO'
FOR j = 1 TO 2
     IF j = 1
          cfte = '0201'
          cfte1 = '09'
     ENDIF
     IF j = 2
          cfte = '0402'
          cfte1 = '13'
     ENDIF
     FOR i = 1 TO 20
          xcad = PADL(ALLTRIM(STR(i)), 2, '0')
          xcta84 = PADR('8401' + cfte + xcad, 15, '0')
          xcta86 = PADR('8601' + cfte + xcad, 15, '0')
          SEEK xcta84 + cmes
          xsaldo84 = 0
          SCAN FOR cuenta = xcta84 .AND. MONTH(fecemi) <= nmes
               xsaldo84 = xsaldo84 + montos * IIF(tipo = 'D', 1, - 1)
          ENDSCAN
          SEEK xcta86 + cmes
          xsaldo86 = 0
          SCAN FOR cuenta = xcta86 .AND. MONTH(fecemi) <= nmes
               xsaldo86 = xsaldo86 + montos * IIF(tipo = 'D', 1, - 1)
          ENDSCAN
          SELECT itehc
          xejec = 0
          ccad = PADL(ALLTRIM(STR(i)), 4, '0')
          SEEK ccad + cfte1
          SCAN WHILE codcad = ccad .AND. codfte = cfte1 .AND. nummes <= cmes
               xejec = xejec + valpart * IIF(tipope = '-', -1, 1)
          ENDSCAN
          SELECT lista8
          APPEND BLANK
          REPLACE mes WITH nmes, cadena WITH STR(i, 2), fte WITH cfte1, c84 WITH xsaldo84, c86 WITH xsaldo86, ejec WITH xejec
          SELECT diario
     ENDFOR
ENDFOR
DO espera WITH 2
SELECT lista8
DO reporte WITH 2, 'CRUE5BC', ' ', 1, .F., .T.
CLOSE DATABASES
RETURN
*
PROCEDURE saleeff
USE IN 1 PARMAE ALIAS parma ORDER PARMAE1
DEFINE WINDOW lis FROM 5, 15 TO  18, 65 FLOAT TITLE '' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO okcancel
ccuenta = SPACE(15)
nmes = 0
canoant = 0
canonew = 0
@ 01, 05 SAY 'Periodo Anterior:' GET canoant FUNCTION '^ 2009;2010;2011;2012'
@ 04, 05 SAY '  Periodo Actual:' GET canonew FUNCTION '^ 2009;2010;2011;2012'
@ 07, 05 SAY '  Mes de Proceso:' GET cmes VALID val_para(cmes,'FECMES',' ',22)
@ 09, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
DEACTIVATE WINDOW lis
RELEASE WINDOW lis
nmes = VAL(cmes)
vsalant = 'H:\SICGDATA\DATA' + valano(canoant) + '\Saldona.dbf'
vsalact = 'H:\SICGDATA\DATA' +  valano(canonew) + '\Saldona.dbf'
vctaant = 'H:\SICGDATA\DATA' + valano(canoant) + '\Cuentas.dbf'
vctaact = 'H:\SICGDATA\DATA' + valano(canonew) + '\Cuentas.dbf'
CLOSE DATABASES
USE IN 1 (vsalant) ALIAS salant ORDER Saldona4
USE IN 2 (vsalact) ALIAS salact ORDER Saldona4
USE IN 3 (vctaant) ALIAS ctaant ORDER Cuentas1
USE IN 4 (vctaact) ALIAS ctaact ORDER Cuentas1
USE IN 5 EXCLUSIVE SalEEFF ORDER SalEEFF1
SELECT salant
SET FILTER TO mes = nmes
SET RELATION TO cuenta INTO ctaant
SELECT salact
SET FILTER TO mes = nmes
SET RELATION TO cuenta INTO ctaact
SELECT saleeff
ZAP
DO espera WITH 1, 'POR FAVOR ESPERE UN MOMENTO'
SELECT salant
SCAN
     SELECT saleeff
     IF  .NOT. SEEK(salant.cuenta)
          IF f_appd()
               REPLACE cuenta WITH salant.cuenta
               REPLACE descri WITH ctaant.descri
               REPLACE dsalant WITH salant.dsaldona
               REPLACE hsalant WITH salant.hsaldona
          ENDIF
     ELSE
          REPLACE dsalant WITH salant.dsaldona
          REPLACE hsalant WITH salant.hsaldona
     ENDIF
     SELECT salant
ENDSCAN
SELECT salact
SCAN
     SELECT saleeff
     IF  .NOT. SEEK(salact.cuenta)
          IF f_appd()
               REPLACE cuenta WITH salact.cuenta
               REPLACE descri WITH ctaact.descri
               REPLACE dsalact WITH salact.dsaldona
               REPLACE hsalact WITH salact.hsaldona
          ENDIF
     ELSE
          REPLACE dsalact WITH salact.dsaldona
          REPLACE hsalact WITH salact.hsaldona
     ENDIF
     SELECT salact
ENDSCAN
SELECT saleeff
SCAN
     REPLACE cuenta WITH cuenta(cuenta)
ENDSCAN
DO espera WITH 2
SELECT saleeff
COPY TO EEFF TYPE XLS
DO standby WITH 'Tabla Generada Satisfactoriamente'
RETURN
*
FUNCTION cuenta
PARAMETER xcuenta
DO CASE
     CASE SUBSTR(xcuenta, 5, 11) = '00000000000'
          RETURN LEFT(xcuenta, 4)
     CASE SUBSTR(xcuenta, 7, 9) =  '000000000'
          RETURN LEFT(xcuenta, 6)
     CASE SUBSTR(xcuenta, 9, 7) = '0000000'
          RETURN LEFT(xcuenta, 8)
     CASE SUBSTR(xcuenta, 11, 5) = '00000'
          RETURN LEFT(xcuenta, 10)
     CASE SUBSTR(xcuenta, 13, 3) = '000'
          RETURN LEFT(xcuenta, 12)
     OTHERWISE
          RETURN xcuenta
ENDCASE
SELE &xAli
RETURN mret
*
FUNCTION valano
PARAMETER xano
DO CASE
     CASE xano = 1
          mret = '2009'
     CASE xano = 2
          mret = '2010'
     CASE xano = 3
          mret = '2011'
     CASE xano = 4
          mret = '2012'
ENDCASE
RETURN mret
*
PROCEDURE crue5c8
nmes = VAL(cmes)
CLOSE DATABASES
USE IN 1 Diario
USE IN 2 IteHC
USE IN 3 MaePre ORDER MaePre1
USE IN 4 EXCLUSIVE Lista8
SELECT diario
INDEX ON cuenta + PADL(MONTH(fecemi), 2, '0') TO YY FOR VAL(LEFT(comprob, 2)) < 95
SELECT itehc
INDEX ON codcad + codfte + IIF(estado = '92', nummeshm, nummes) TO XX FOR estado <> '99' .AND. cmes >= IIF(estado = '92', nummeshm, nummes)
SET RELATION TO RIGHT(STR(cano, 4), 2) + uniges + unieje + codcad INTO maepre
SELECT lista8
ZAP
SELECT diario
DO espera WITH 1, 'POR FAVOR ESPERE UN MOMENTO'
FOR j = 1 TO 3
     IF j = 1
          cfte = '0201'
          cfte1 = '09'
     ENDIF
     IF j = 2
          cfte = '0402'
          cfte1 = '13'
     ENDIF
     IF j = 3
          cfte = '0301'
          cfte1 = '19'
     ENDIF
     FOR i = 1 TO 25
          xcad = PADL(ALLTRIM(STR(i)), 2, '0')
          xcta84 = PADR('8401' + cfte + xcad, 15, '0')
          xcta86 = PADR('8601' + cfte + xcad, 15, '0')
          DO espera WITH 1, xcta84+' '+xcta86
          SEEK xcta84 + cmes
          xsaldo84 = 0
          SCAN FOR cuenta = xcta84 .AND. MONTH(fecemi) <= nmes
               IF YEAR(fecemi) <> cano
                    DO standby WITH 'Error en Fecha. N/C '+ comprob+ comadi
                    LOOP
               ENDIF
               xsaldo84 = xsaldo84 + montos * IIF(tipo = 'D', 1, - 1)
          ENDSCAN
          SEEK xcta86 + cmes
          xsaldo86 = 0
          SCAN FOR cuenta = xcta86 .AND. MONTH(fecemi) <= nmes
               IF YEAR(fecemi) <> cano
                    DO standby WITH 'Error en Fecha. N/C '+ comprob+comadi
                    LOOP
               ENDIF
               xsaldo86 = xsaldo86 + montos * IIF(tipo = 'D', 1, -1)
          ENDSCAN
          SELECT itehc
          xejec = 0
          ccad = PADL(ALLTRIM(STR(i)), 4, '0')
          SEEK ccad + cfte1
          SCAN WHILE codcad = ccad .AND. codfte = cfte1 .AND. nummes <= cmes
               IF  .NOT. (itehc.codcom = maepre.codcom .AND. itehc.codmet = maepre.codmet)
                    DO standby WITH 'Error en Cadena. H/C '+ numhc+'.'+ nummes
                    LOOP
               ENDIF
               xejec = xejec + valpart * IIF(tipope = '-', -1, 1)
          ENDSCAN
          SELECT lista8
          IF EMPTY(xsaldo84) .AND. EMPTY(xsaldo86) .AND. EMPTY(xejec)
          ELSE
               APPEND BLANK
               REPLACE mes WITH nmes, cadena WITH STR(i, 2), fte WITH cfte1, c84 WITH xsaldo84, c86 WITH xsaldo86, ejec WITH xejec
          ENDIF
          SELECT diario
     ENDFOR
ENDFOR
DO espera WITH 2
SELECT lista8
DO reporte WITH 2, 'CRUE5BC', ' ', 1, .F., .T.
CLOSE DATABASES
RETURN
*
PROCEDURE recing
USE IN 1 RecIng ALIAS recing
USE IN 2 ASTPAT ALIAS astpat ORDER AstPat14
USE IN 3 AsiAutRI ALIAS asiaut ORDER AsiAutRi1
USE IN 4 Parmae ALIAS parma ORDER Parmae1
DEFINE WINDOW lis FROM 5, 15 TO 18, 65 FLOAT TITLE '' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO okcancel
m.tipri = SPACE(3)
vtipori = 0
@ 05, 05 GET vtipori PICTURE '@*RH Todos los tipos;Un tipo'
@ 07, 05 SAY 'Tipo R/I' GET m.tipri PICTURE '!!!' VALID val_tipri(7,18) WHEN vtipori = 2
@ 09, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
IF LASTKEY() <> 27 .AND. okcancel <> 2
     SELECT recing
     DO espera WITH 1, 'Por Favor Espere un Momento'
     INDEX ON periodo + nummes + tipri TO vIdx FOR nummes = cmes .AND. IIF(vtipori = 2, tipri = m.tipri, .T.)
     SET RELATION TO periodo + nummes + numri INTO astpat
     SET SKIP TO astpat
     DO espera WITH 2
     DO reporte WITH 2, 'RecIng', ' ', 1, .T., .T.
ENDIF
CLOSE DATABASES
RETURN
*
PROCEDURE val_tipri
PARAMETER vfil, vcol
PRIVATE mbus, mimp, calias
STORE .F. TO mbus, mimp
calias = ALIAS()
IF EMPTY(m.tipri)
     mbus = .T.
ELSE
     IF SEEK('R/I' + m.tipri, 'AsiAut')
          mimp = .T.
     ELSE
          mbus = .T.
     ENDIF
ENDIF
IF mbus
     SELECT asiaut
     SET ORDER TO AsiAutRI2
     DEFINE POPUP v__xx FROM 1, 30 TO 12, 79 PROMPT FIELDS descri
     ON SELECTION POPUP v__xx DEACTIVATE;
POPUP
     ACTIVATE POPUP v__xx
     RELEASE POPUP v__xx
     m.tipri = codigo
     SET ORDER TO AsiAutRi1
     mimp = .T.
ENDIF
IF mimp
     @ vfil, vcol SAY asiaut.descri
ENDIF
SELECT (calias)
RETURN
*
FUNCTION desri
PRIVATE calias, mret
calias = ALIAS()
IF SEEK('R/I' + recing.tipri, 'AsiAut')
     mret = asiaut.descri
ENDIF
RETURN mret
*
PROCEDURE cruingdi
nmes = VAL(cmes)
USE IN 1 iteri ORDER iteri1
USE IN 2 diario ORDER diario11
SELECT iteri
SET FILTER TO VAL(nummes) = nmes
SELECT diario
INDEX ON comprob + comadi TO xx FOR comprob = '25' .AND. cuenta = '8' .AND. MONTH(fecemi) = nmes
SELECT iteri
GOTO TOP
IF EOF()
     DO standby WITH 'No Hay Registros para Procesar'
     CLOSE DATABASES
     RETURN
ENDIF
SCAN
     m.numri = numri
     m.nummes = nummes
     m.valpart = 0
     SCAN WHILE numri = m.numri
          DO CASE
               CASE EMPTY(iteri.tipo)
                    lerr = .T.
               CASE iteri.tipo = 'P' .AND. EMPTY(codpart)
                    lerr = .T.
               CASE iteri.tipo = 'O' .AND. .NOT. EMPTY(codpart)
                    lerr = .T.
               OTHERWISE
                    lerr = .F.
          ENDCASE
          IF lerr
               WAIT WINDOW 'Error en R/I ' + numri
          ELSE
               IF iteri.tipo = 'P' .AND.  .NOT. EMPTY(codpart)
                    m.valpart = m.valpart + impparc
               ENDIF
          ENDIF
     ENDSCAN
     SELECT diario
     xcomp = '25' + PADL(ALLTRIM(STR(VAL(m.numri))), 5, '0') + m.nummes
     SEEK xcomp
     nmtos = 0
     IF FOUND()
          m.comprob = comprob
          SCAN WHILE comprob = m.comprob
               nmtos = nmtos + montos
          ENDSCAN
          IF m.valpart <> nmtos / 2
               WAIT WINDOW  ;
                    'Error en N/C ' + m.comprob
               WAIT WINDOW STR(m.valpart, 2) + ' ' + STR(nmtos / 2, 2)
          ENDIF
     ELSE
          WAIT WINDOW 'No Existe Recibo ' + xcomp
     ENDIF
     SELECT iteri
     SKIP -1
ENDSCAN
*
*** 
*** ReFox - retrace your steps ... 
***
