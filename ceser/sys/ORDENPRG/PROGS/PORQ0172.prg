*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
tit_prg = ' CONSULTA '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CENTRO DE INFORMACION AL CLIENTE '
DEFINE WINDOW detalle FROM 00, 00  ;
       TO 20, 79 IN screen COLOR  ;
       SCHEME 21
DEFINE WINDOW datos FROM 10, 00  ;
       TO 19, 76 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW ppto FROM 00, 00 TO  ;
       19, 79 IN screen COLOR  ;
       SCHEME 21
SELECT 1
USE SHARED st_iorep ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED st_iclpr ORDER codent
SELECT 4
USE SHARED st_itecn ORDER codigo
SELECT 5
USE SHARED st_iscic ORDER fecuse
SELECT 6
USE SHARED gc_hve00 ORDER nrdore
STORE .T. TO ppal
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
w_tipcam = ootc2(DATE(), ;
           rge_monbas,'DOL ', ;
           '2')
IF w_tipcam = -1
     DO error WITH  ;
        '*** No Existe Tipo de Cambio de esta Fecha ***'
     ppal = .F.
ENDIF
USE
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     w_facigv = tab_factor / 100
ELSE
     do error with "*** No Definido el &empre9 en Tablas ***"
     ppal = .F.
ENDIF
STORE DATE() TO w_codind,  ;
      w_codfid
DO WHILE ppal
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     CLEAR GETS
     @ 03, 01 TO 07, 75
     STORE 1 TO valor1, valor2
     STORE 0 TO w_cosmob, w_flete,  ;
           w_cosrep, w_totdes,  ;
           s_descue, w_desmob,  ;
           w_desrep
     STORE DATE() TO w_feconw,  ;
           w_feccom
     STORE TIME() TO w_hoconw,  ;
           w_horini
     STORE SPACE(04) TO w_codigo,  ;
           w_codin2, w_codfi2,  ;
           w_estado
     STORE SPACE(08) TO w_numsol,  ;
           w_numord, w_user
     DIMENSION arrhist( 1),  ;
               arrrep( 1), rep(  ;
               1), arrayt( 1)
     @ 06, 18 SAY SPACE(28)
     @ 04, 38 SAY SPACE(36)
     @ 05, 38 SAY SPACE(36)
     @ 04, 02 SAY 'Estado :'
     @ 04, 11 GET w_estado  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 05, 02 GET valor1 PICTURE  ;
       '@*RVN Tipo Atenci¢n;Usuario'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     READ
     IF LASTKEY() = 27 .AND.  ;
        valor1 = 1
          ppal = .F.
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY
DO sacawin
RETURN
*
PROCEDURE oowhen
PARAMETER opc
DO CASE
     CASE opc = 'W_ESTADO'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda with;
1
     CASE opc = 'W_CODIGO'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda with;
1
     CASE opc = 'W_CODIN2' .OR.  ;
          opc = 'W_CODFI2'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL f6 do ayuda with;
2
     CASE opc = 'W_CODIND' .OR.  ;
          opc = 'W_CODFID'
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 'VALOR1' .OR. opc =  ;
          'VALOR2'
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 'W_FECONW'
          DO ptecla
          DO colocaf
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'W_ESTADO'
          SELECT ge_tab0
          SEEK 'ESOR' + w_estado
          IF  .NOT.  ;
              EMPTY(w_estado)
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       'C¢digo de Estado No Existe'
                    RETURN .F.
               ENDIF
               @ 04, 16 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 20)
          ENDIF
     CASE opc = 'VALOR1'
          CLEAR GETS
          IF valor1 = 1
               IF EMPTY(w_estado)
                    DO error WITH  ;
                       'Ingrese Primero Estado'
                    RETURN .F.
               ENDIF
               @ 05, 20 GET  ;
                 w_codigo PICTURE  ;
                 '@!' VALID  ;
                 oovali2(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               READ
               IF LASTKEY() = 27
                    @ 05, 25 SAY  ;
                      SPACE(12)
                    RETURN
               ENDIF
          ELSE
               @ 06, 18 GET  ;
                 w_user PICTURE  ;
                 '@!' WHEN  ;
                 oowhen(VARREAD())
               READ
               IF LASTKEY() = 27
                    @ 06, 26 SAY  ;
                      SPACE(23)
                    RETURN
               ENDIF
               SELECT 20
               USE SHARED  ;
                   password ORDER  ;
                   usuario
               SEEK w_user
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       'C¢digo de Usuario No Existe'
                    RETURN .F.
               ENDIF
               @ 06, 26 SAY  ;
                 SUBSTR(nombre, 1,  ;
                 20)
               USE
               DO proceso WITH 2
          ENDIF
ENDCASE
RETURN
*
FUNCTION oovali2
PARAMETER opc
DO CASE
     CASE valor1 = 1
          SELECT ge_tab0
          SEEK 'INGA' + w_codigo
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Estado No Existe'
               RETURN .F.
          ENDIF
          @ 05, 25 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
          @ 04, 38 GET valor2  ;
            PICTURE  ;
            '@*RVTN Emisor;Fecha Compr.;Taller'  ;
            WHEN  ;
            oowhen(VARREAD())
          READ
          IF LASTKEY() = 27
               @ 04, 38 SAY  ;
                 SPACE(36)
               @ 05, 38 SAY  ;
                 SPACE(36)
               @ 06, 38 SAY  ;
                 SPACE(36)
               RETURN .F.
          ENDIF
          @ 04, 54 SAY SPACE(20)
          @ 05, 54 SAY SPACE(20)
          @ 04, 54 SAY 'Del :'
          @ 05, 54 SAY ' Al :'
          IF valor2 = 2
               @ 04, 59 GET  ;
                 w_codind PICTURE  ;
                 '@!' VALID  ;
                 oovali3(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               @ 05, 59 GET  ;
                 w_codfid RANGE  ;
                 w_codind PICTURE  ;
                 '@!' VALID  ;
                 oovali3(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
          ELSE
               @ 04, 59 GET  ;
                 w_codin2 PICTURE  ;
                 '@!' VALID  ;
                 oovali3(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
               @ 05, 59 GET  ;
                 w_codfi2 RANGE  ;
                 w_codin2 PICTURE  ;
                 '@!' VALID  ;
                 oovali3(VARREAD())  ;
                 WHEN  ;
                 oowhen(VARREAD())
          ENDIF
          READ
          IF LASTKEY() = 27
               @ 04, 54 SAY  ;
                 SPACE(20)
               @ 05, 54 SAY  ;
                 SPACE(20)
               RETURN .F.
          ENDIF
          DO proceso WITH 1
          CLEAR READ
          @ 04, 15 SAY SPACE(58)
          @ 05, 16 SAY SPACE(57)
          @ 06, 20 SAY SPACE(53)
ENDCASE
RETURN
*
FUNCTION oovali3
PARAMETER var
DO CASE
     CASE valor2 = 1
          IF var = 'W_CODIN2'
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    RETURN .F.
               ENDIF
          ENDIF
          SELECT ge_tab0
          IF var = 'W_CODIN2'
               SEEK 'EMIS' +  ;
                    w_codin2
          ELSE
               SEEK 'EMIS' +  ;
                    w_codfi2
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Estado No Existe'
               RETURN .F.
          ENDIF
          IF var = 'W_CODFI2'
               IF w_codfi2 <  ;
                  w_codin2
                    DO error WITH  ;
                       'C¢digo de Estado Inv lido'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 64 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            10)
     CASE valor2 = 2
          IF var = 'W_CODIND'
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    RETURN .F.
               ENDIF
               IF EMPTY(w_codind)
                    RETURN .F.
               ENDIF
          ELSE
               IF EMPTY(w_codfid)
                    RETURN .F.
               ENDIF
               IF w_codfid <  ;
                  w_codind
                    RETURN .F.
               ENDIF
          ENDIF
     CASE valor2 = 3
          IF var = 'W_CODIN2'
               IF LASTKEY() = 5  ;
                  .OR. LASTKEY() =  ;
                  19
                    RETURN .F.
               ENDIF
          ENDIF
          SELECT ge_tab0
          IF var = 'W_CODIN2'
               SEEK 'TALL' +  ;
                    w_codin2
          ELSE
               SEEK 'TALL' +  ;
                    w_codfi2
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Estado No Existe'
               RETURN .F.
          ENDIF
          IF var = 'W_CODFI2'
               IF w_codfi2 <  ;
                  w_codin2
                    DO error WITH  ;
                       'C¢digo de Estado Inv lido'
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 64 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            10)
ENDCASE
RETURN
*
PROCEDURE proceso
PARAMETER w_pro
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
CREATE CURSOR usuario (t C (01),  ;
       fecemi D, numsol C (8),  ;
       numord C (8), cliente C  ;
       (30), telef1 N (08),  ;
       telef2 N (08), numser C  ;
       (15), observ M (10),  ;
       codmar C (4), numpre C (8),  ;
       codmod C (15), infant M  ;
       (10), codemi C (04),  ;
       feccom D, indori C (4),  ;
       codtec C (9), auxest C (4),  ;
       indest C (1), flete N (9,  ;
       2), cosmob N (9, 2),  ;
       cosrep N (9, 2), totdes N  ;
       (9, 2), subtot N (9, 2),  ;
       desrep N (6, 2), desmob N  ;
       (6, 2))
INDEX ON numsol TAG numsol
SET ORDER TO numsol
IF w_pro = 2
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep
     SET ORDER TO ord_numsol
     SELECT st_iscic
     SET ORDER TO fecuse
     SET NEAR ON
     SEEK DTOS(DATE())
     SET NEAR OFF
     SCAN WHILE feccom >= DATE()  ;
          .AND.  .NOT. EOF()
          IF user = w_user
               SELECT st_iorep
               SEEK st_iscic.numsol
               IF FOUND()
                    IF indest <>  ;
                       'N' .AND.  ;
                       indest <>  ;
                       'B' .AND.  ;
                       indest <>  ;
                       'F'
                         STORE SPACE(80)  ;
                               TO  ;
                               w_info
                         DO busord
                         DO carga
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iscic
     ENDSCAN
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
ELSE
     SELECT st_iorep
     SET ORDER TO ord_esem
     SET NEAR ON
     SEEK w_estado
     SET NEAR OFF
     IF valor2 = 1 .OR. valor2 =  ;
        3
          DO proceso3
     ELSE
          DO proceso4
     ENDIF
ENDIF
SELECT usuario
COUNT TO w_valor
IF w_valor = 0
     DO error WITH  ;
        '***  No Existen registros a Listar  ***'
     w_fin2 = .F.
ELSE
     w_fin2 = .T.
ENDIF
GOTO TOP
DO WHILE w_fin2
     w_salir = 0
     ON KEY
     ON KEY LABEL enter do datos with;
numsol
     ON KEY LABEL f10 do salir
     ON KEY LABEL esc do salir
     ON KEY LABEL ctrl+w ??""
     BROWSE NOAPPEND NOEDIT  ;
            WINDOW datos
     IF w_salir = 1
          ON KEY
          w_fin2 = .F.
     ENDIF
ENDDO
RETURN
*
PROCEDURE salir
w_salir = 1
w_fin2 = .F.
KEYBOARD '{CTRL+Q}'
RETURN
*
PROCEDURE datos
PARAMETER w_numsol
ON KEY
SELECT usuario
ACTIVATE WINDOW detalle
STORE 0 TO i, s_cosrep, s_cosmob,  ;
      s_cosfle, s_toacta, s_total,  ;
      s_totpag
@ 00, 01 SAY  ;
  ' CONSULTA DE SERVICIOS' +  ;
  STR(usuario.telef1, 9) + '-' +  ;
  STR(usuario.telef2, 9) COLOR  ;
  SCHEME 8
@ 00, 43 SAY  ;
  ' CENTRO DE INFORMACION AL CLIENTE  '  ;
  COLOR SCHEME 8
@ 01, 23 SAY 'O/R : ' +  ;
  usuario.numord
@ 01, 01 SAY 'S/Servicio : ' +  ;
  w_numsol
@ 02, 01 SAY 'Fecha Emis.: ' +  ;
  DTOC(usuario.fecemi)
@ 03, 01 SAY 'Tipo Atenc.: ' +  ;
  usuario.indori + SPACE(6) +  ;
  SUBSTR(ootab2('INGA', ;
  usuario.indori), 1, 18)
@ 04, 01 SAY 'Estado     : '
@ 04, 14 SAY auxest + SPACE(6) +  ;
  SUBSTR(ootab2('ESOR', ;
  usuario.auxest), 1, 18) COLOR  ;
  SCHEME 8
@ 05, 01 SAY 'T‚cnico    : ' +  ;
  usuario.codtec + SPACE(1) +  ;
  SUBSTR(oodeste2(usuario.codtec),  ;
  1, 18)
@ 06, 01 SAY 'In:'
@ 06, 04 SAY observ SIZE 5, 38
@ 01, 43 SAY 'Sr(a):' +  ;
  SUBSTR(cliente, 1, 29) COLOR  ;
  SCHEME 8
@ 02, 43 SAY SUBSTR(ootab2('MARC', ;
  usuario.codmar), 1, 08) +  ;
  SPACE(1) + usuario.codmod +  ;
  SPACE(1) +  ;
  SUBSTR(usuario.numser, 1, 10)  ;
  COLOR SCHEME 22
@ 03, 43 SAY usuario.infant SIZE  ;
  5, 35
DIMENSION fecha( 1), docum( 1),  ;
          valor( 1)
DO montos
w_horini = TIME()
STORE SPACE(70) TO w_info01
@ 08, 43 SAY 'Fecha Comprom: '
@ 08, 58 GET w_feconw RANGE  ;
  DATE() PICTURE '@D' WHEN  ;
  oowhen(VARREAD()) COLOR SCHEME  ;
  10
@ 08, 70 GET w_hoconw PICTURE  ;
  '99:99:99' COLOR SCHEME 10
@ 09, 43 GET w_info01 SIZE 2, 35  ;
  PICTURE '@!' COLOR SCHEME 10
READ CYCLE
DEACTIVATE WINDOW detalle
DO principa
RETURN
*
PROCEDURE grabacion
IF  .NOT. EMPTY(w_info01)
     SELECT st_iscic
     APPEND BLANK
     DO rbloquea
     REPLACE numsol WITH w_numsol
     REPLACE numord WITH w_numord
     REPLACE inform WITH w_info01
     REPLACE fecini WITH DATE()
     REPLACE feccom WITH w_feconw
     REPLACE horcom WITH w_hoconw
     REPLACE horini WITH w_horini
     REPLACE horfin WITH TIME()
     REPLACE user WITH users
     REPLACE date WITH DATE()
     REPLACE time WITH TIME()
     UNLOCK
     SELECT usuario
     REPLACE t WITH 'þ'
     REPLACE infant WITH infant +  ;
             w_info01
     CLEAR READ
ELSE
     DO error WITH  ;
        '*** Esta vac¡o la Informaci¢n ***'
ENDIF
RETURN
*
PROCEDURE repuestos
SELECT 20
USE st_iprep ORDER rep_numord
SEEK usuario.numord
STORE 0 TO w_cosrep, a
DIMENSION arrayt( 1)
IF FOUND()
     SELECT 21
     USE st_idped ORDER codigo
     SELECT st_iprep
     SCAN WHILE numord =  ;
          usuario.numord .AND.   ;
          .NOT. EOF()
          IF indest <> 'N'
               SELECT st_idped
               SEEK st_iprep.numdoc
               SCAN WHILE numdoc =  ;
                    st_iprep.numdoc  ;
                    .AND. numord =  ;
                    usuario.numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF st_idped.canpro >  ;
                       0
                         w_cosrep =  ;
                          w_cosrep +  ;
                          totite
                         a = a +  ;
                             1
                         DIMENSION  ;
                          arrayt(  ;
                          a)
                         arrayt(  ;
                               a) =  ;
                               numdoc +  ;
                               '³' +  ;
                               codpro +  ;
                               '³' +  ;
                               SUBSTR(oodespro(codpro),  ;
                               1,  ;
                               22) +  ;
                               '³' +  ;
                               STR(canpro,  ;
                               5) +  ;
                               '³' +  ;
                               STR(valpro,  ;
                               10,  ;
                               2) +  ;
                               '³' +  ;
                               STR(totite,  ;
                               11,  ;
                               2)
                    ENDIF
               ENDSCAN
               SELECT st_iprep
          ENDIF
     ENDSCAN
     SELECT 21
     USE
ELSE
     arrayt( 1) = SPACE(10)
ENDIF
SELECT 20
USE
RETURN
*
PROCEDURE consumo
ACTIVATE WINDOW detalle
ON KEY
DO repuestos
IF EMPTY(arrayt(1))
     DO error WITH  ;
        '*** No existe Consumo de Repuestos ***'
     RETURN
ENDIF
DO sacaf7
@11,00 say " No.Pedido C¢digo         Descripci¢n            Cant.    P.Unit. P.Total &empre13 ";
color i
arrayitem = SPACE(14)
@ 12, 00 GET arrayitem SIZE 06,  ;
  78 FROM arrayt
READ
IF LASTKEY() = 27 .OR. LASTKEY() =  ;
   19 .OR. LASTKEY() = 4
     CLEAR GETS
     @ 12, 00 CLEAR TO 19, 77
     w_salida = 0
     DO pinta
ENDIF
DO ptecla
RETURN
*
PROCEDURE montos
w_numord = usuario.numord
w_indori = usuario.indori
w_indest = usuario.indest
w_codmar = usuario.codmar
w_codmod = usuario.codmod
w_cosmob = usuario.cosmob
w_auxest = usuario.auxest
w_desrep = usuario.desrep
w_desmob = usuario.desmob
w_totdes = usuario.totdes
w_flete = usuario.flete
DIMENSION fecha( 1), docum( 1),  ;
          valor( 1)
STORE 0 TO i, s_cosrep, s_cosmob,  ;
      s_cosfle, s_toacta, s_total,  ;
      s_totpag
DO pormcost WITH 'usuario'
DO pinta
RETURN
*
PROCEDURE pinta
@ 11, 00 TO 11, 79
@12,33 say "A CUENTA:     Fecha Nø Docume.    Importe &empre8";
color scheme 8
@12,01 say "Repuesto   &empre8:"
@ 13, 01 SAY 'M/obra        :'
@ 14, 01 SAY 'Flete         :'
@ 15, 01 SAY 'Subtotal      :'
@ 16, 01 SAY 'Descuento     :'
@ 17, 01 SAY 'Total Pagos   :'
@ 18, 01 SAY 'Saldo a Pagar :'
IF i > 5
     i = 5
ENDIF
IF  .NOT. EMPTY(fecha(1))
     FOR b = 1 TO i
          @ 12 + b, 42 SAY  ;
            fecha(b)
          @ 12 + b, 53 SAY  ;
            docum(b)
          @ 12 + b, 64 SAY  ;
            valor(b) PICTURE  ;
            '999,999,999.99'
     ENDFOR
ENDIF
@ 12, 17 SAY s_cosrep PICTURE  ;
  '999,999,999.99'
@ 13, 17 SAY s_cosmob PICTURE  ;
  '999,999,999.99'
@ 14, 17 SAY s_cosfle PICTURE  ;
  '999,999,999.99'
@ 15, 17 SAY s_total PICTURE  ;
  '999,999,999.99'
@ 16, 17 SAY s_descue PICTURE  ;
  '999,999,999.99'
@ 17, 17 SAY s_toacta PICTURE  ;
  '999,999,999.99'
@ 18, 17 SAY s_totpag PICTURE  ;
  '999,999,999.99' COLOR N/W 
IF usuario.indest <> 'C' .AND.  ;
   usuario.indest <> 'B' .AND.  ;
   usuario.indest <> 'F'
     @ 18, 33 SAY 'Aprox.'
ENDIF
RETURN
*
PROCEDURE estados
ON KEY
DO sacaf7
SELECT 20
USE SHARED st_mvord ORDER  ;
    eor_nroord
SEEK w_numord
campox = 'dtoc(dia)+" "+hora+" "+tecnico+" "+estado+" "+destado'
DEFINE WINDOW ayu4 FROM 08, 1 TO  ;
       17, 78 COLOR SCHEME 21
ACTIVATE WINDOW TOP ayu4
browse field cer=" ":H="",uno=&campox:H="Fecha      Hora       T‚cnico Situaci¢n";
key w_numord in window ayu4 freeze cer
RELEASE WINDOW ayu4
RETURN
*
PROCEDURE historia
ON KEY
SELECT st_iscic
SET ORDER TO numsol
SEEK w_numsol
a = 0
IF FOUND()
     DO sacaf7
     SCAN WHILE numsol = w_numsol  ;
          .AND.  .NOT. EOF()
          a = a + 1
          DIMENSION arrhist( a)
          arrhist( a) =  ;
                 DTOC(feccom) +  ;
                 '³' +  ;
                 SUBSTR(horcom, 1,  ;
                 5) + '³' +  ;
                 SUBSTR(inform, 1,  ;
                 60)
     ENDSCAN
ELSE
     DO error WITH  ;
        '*** No existe Historia Anterior ***'
     RETURN
ENDIF
ACTIVATE WINDOW detalle
@ 11, 00 SAY  ;
  ' Fec.compro. Hora  Informe                                                      '  ;
  COLOR N/W 
arrayi = SPACE(14)
@ 12, 00 GET arrayi SIZE 06, 78  ;
  FROM arrhist
READ
IF LASTKEY() = 27 .OR. LASTKEY() =  ;
   19 .OR. LASTKEY() = 4
     CLEAR GETS
     @ 12, 00 CLEAR TO 19, 77
     w_salida = 0
     DO pinta
ENDIF
RETURN
*
PROCEDURE pendiente
ACTIVATE WINDOW detalle
ON KEY
SELECT 20
USE gc_ord00 ORDER ord_refprp
SEEK ALLTRIM(usuario.numord)
IF FOUND()
     DO sacaf7
     a = 0
     SCAN WHILE  ;
          ALLTRIM(ord_docref) =  ;
          ALLTRIM(usuario.numord)  ;
          .AND.  .NOT. EOF()
          a = a + 1
          DIMENSION arrrep( a)
          arrrep( a) = ord_nrodoc +  ;
                '³' + ord_codprp +  ;
                '³' +  ;
                SUBSTR(ootab2('ESDO', ;
                ord_inorig), 1,  ;
                4) + '³' +  ;
                SUBSTR(ootab2('EPRO', ;
                ord_indest), 1,  ;
                7) + '³' +  ;
                ord_numfac + '³' +  ;
                ord_codpro + '³' +  ;
                DTOC(ord_feclle)
     ENDSCAN
ELSE
     USE
     DO error WITH  ;
        '*** No existen Repuestos Pendientes ***'
     RETURN
ENDIF
USE
@ 11, 00 SAY  ;
  '  Nro. Doc.  C¢digo        Origen Estado Nro. Fact. Reemplazo      Fch.Llegada'  ;
  COLOR N/W 
arr02 = SPACE(14)
@ 12, 00 GET arr02 SIZE 06, 78  ;
  FROM arrrep
READ
IF LASTKEY() = 27 .OR. LASTKEY() =  ;
   19 .OR. LASTKEY() = 4
     CLEAR GETS
     @ 12, 00 CLEAR TO 19, 77
     DO pinta
ENDIF
DO ptecla
RETURN
*
PROCEDURE presupuest
ON KEY
SELECT 20
USE st_ispre ORDER codigo
SEEK usuario.numpre
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** No Existe Presupuesto Registrado ***'
     RETURN
ELSE
     IF indest = 'N'
          DO error WITH  ;
             '*** No Existe Presupuesto Registrado ***'
          RETURN
     ENDIF
ENDIF
w_fecpre = fecemi
w_fecven = fecven
w_tecpre = codtec
w_moneda = codmon
w_reppre = monrep
w_mobpre = monman
w_totigv = totigv
w_pordes = pordes
w_infpr1 = SUBSTR(observ, 001,  ;
           228)
SELECT 20
USE SHARED st_idpre ORDER codigo
SEEK usuario.numpre
STORE 0 TO a, s_totrep
IF FOUND()
     DIMENSION rep( 1)
     SCAN WHILE numdoc =  ;
          usuario.numpre .AND.   ;
          .NOT. EOF()
          a = a + 1
          DIMENSION rep( a)
          IF w_moneda = 'DOL '
               rep( a) = codpro +  ;
                  '³' +  ;
                  SUBSTR(oodespro(codpro),  ;
                  1, 21) + '³' +  ;
                  STR(canpro, 5) +  ;
                  '³' +  ;
                  TRANSFORM(valpro,  ;
                  '9,999,999.99') +  ;
                  '³' +  ;
                  TRANSFORM(totite,  ;
                  '99,999,999.99' ;
                  ) + '³' +  ;
                  STR(stk_alm(codpro),  ;
                  5)
          ELSE
               w_item = ROUND(valpro *  ;
                        w_tipcam,  ;
                        2)
               s_totrep = s_totrep +  ;
                          (w_item *  ;
                          canpro)
               rep( a) = codpro +  ;
                  '³' +  ;
                  SUBSTR(oodespro(codpro),  ;
                  1, 21) + '³' +  ;
                  STR(canpro, 5) +  ;
                  '³' +  ;
                  TRANSFORM(w_item,  ;
                  '9,999,999.99') +  ;
                  '³' +  ;
                  TRANSFORM(w_item *  ;
                  canpro,  ;
                  '99,999,999.99' ;
                  ) + '³' +  ;
                  STR(stk_alm(codpro),  ;
                  5)
          ENDIF
     ENDSCAN
ENDIF
IF w_moneda = 'SOL '
     w_reppre = s_totrep
     w_mobpre = ROUND(w_mobpre *  ;
                w_tipcam, 2)
     w_reppre = ROUND(w_reppre -  ;
                (w_reppre *  ;
                w_pordes / 100),  ;
                2)
     w_totgrl = w_reppre +  ;
                w_mobpre
     w_totafe = ROUND(w_totgrl /  ;
                (1 + w_facigv),  ;
                2)
     w_totigv = w_totgrl -  ;
                w_totafe
ENDIF
USE
DO sacaf7
ACTIVATE WINDOW ppto
@ 00, 01 SAY 'PPTO.   :' COLOR N/ ;
  W 
@ 00, 11 SAY usuario.numpre COLOR  ;
  N/W 
@ 01, 01 SAY 'O/R     :'
@ 01, 11 SAY usuario.numord
@ 00, 27 SAY 'F. Emisi¢n :'
@ 00, 40 SAY w_fecpre
@ 01, 27 SAY 'F. Emisi¢n :'
@ 01, 40 SAY usuario.fecemi
@ 00, 52 SAY 'F. Vcmto.:'
@ 00, 63 SAY w_fecven
@ 01, 52 SAY 'S/S      :'
@ 01, 65 SAY usuario.numsol
@ 02, 00 TO 02, 79
@ 03, 01 SAY 'Emisor  :'
@ 03, 11 SAY usuario.codemi
@ 03, 16 SAY SUBSTR(ootab2('EMIS', ;
  usuario.codemi), 1, 10)
@ 03, 27 SAY 'Tipo Atenc.:'
@ 03, 40 SAY usuario.indori
@ 03, 52 SAY 'Moneda   :'
@ 03, 63 SAY w_moneda
@ 03, 67 SAY SUBSTR(ootab2('MONE', ;
  w_moneda), 1, 10)
@ 04, 01 SAY 'Marca   :'
@ 04, 11 SAY usuario.codmar
@ 04, 16 SAY SUBSTR(ootab2('MARC', ;
  usuario.codmar), 1, 10)
@ 04, 27 SAY 'Modelo     :'
@ 04, 40 SAY usuario.codmod
@ 04, 52 SAY 'Serie    :'
@ 04, 63 SAY usuario.numser
@ 05, 00 TO 05, 79
@ 06, 01 SAY 'Cliente :'
@ 06, 11 SAY usuario.cliente
@ 06, 40 SAY 'T‚cnico :'
@ 06, 50 SAY oodeste2(w_tecpre)
@ 07, 00 TO 07, 79
@ 08, 01 SAY 'I:' COLOR N/W 
@ 08, 03 SAY w_infpr1 SIZE 3, 76
@ 11, 00 SAY  ;
  '  C¢digo         Descripci¢n           Cant.       Precio         Total Stock '  ;
  COLOR N/W 
@ 17,00 say "Rep:                Mob:               &empre9:                Tot:";
color i
@ 17, 04 SAY w_reppre PICTURE  ;
  '999,999,999.99' COLOR N/W 
@ 17, 24 SAY w_mobpre PICTURE  ;
  '999,999,999.99' COLOR N/W 
@ 17, 44 SAY w_totigv PICTURE  ;
  '999,999,999.99' COLOR N/W 
@ 17, 64 SAY w_reppre + w_mobpre  ;
  PICTURE '999,999,999.99' COLOR  ;
  N/W 
IF a > 0
     w_ara = SPACE(14)
     @ 12, 00 GET w_ara SIZE 05,  ;
       78 FROM rep
     READ
     IF LASTKEY() = 27
          CLEAR GETS
     ENDIF
ELSE
     = INKEY(0, 'H')
ENDIF
DEACTIVATE WINDOW ppto
ACTIVATE WINDOW detalle
RETURN
*
PROCEDURE ayuda
PARAMETER opc
IF VARREAD() = 'W_ESTADO'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'ESOR'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA ESTADO DE ORDENES'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     RETURN
ENDIF
IF opc = 1
     DO CASE
          CASE valor1 = 1
               SELECT ge_tab0
               SET FILTER TO tab_codpre;
== 'INGA'
               campo = 'tab_codtab + "  " + tab_destab'
               titulo = 'AYUDA TIPO DE ATENCION'
               DO ayuda1 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
               SET FILTER TO
     ENDCASE
ELSE
     DO CASE
          CASE valor2 = 1
               SELECT ge_tab0
               SET FILTER TO tab_codpre;
== 'EMIS'
               campo = 'tab_codtab + "  " + tab_destab'
               titulo = 'AYUDA DE EMISORES'
               DO ayuda1 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
               SET FILTER TO
          CASE valor2 = 3
               SELECT ge_tab0
               SET FILTER TO tab_codpre;
== 'TALL'
               campo = 'tab_codtab + "  " + tab_destab'
               titulo = 'AYUDA DE TALLERES'
               DO ayuda1 WITH  ;
                  campo, titulo,  ;
                  'tab_codtab'
               SET FILTER TO
     ENDCASE
ENDIF
RETURN
*
PROCEDURE proceso3
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
IF valor2 = 1
     w_con = 'codemi>=w_codin2 and codemi<=w_codfi2 and indori=w_codigo'
ELSE
     w_con = 'codtall>=w_codin2 and codtall<=w_codfi2 and indori=w_codigo'
ENDIF
SELECT st_iscic
SET ORDER TO numsol
SELECT st_iorep
SCAN WHILE auxest = w_estado  ;
     .AND.  .NOT. EOF()
     if &w_con      
          IF indest <> 'N   '  ;
             .AND. indest <>  ;
             'B   ' .AND. indest <>  ;
             'F   '
               STORE SPACE(80) TO  ;
                     w_info
               DO busord
               DO carga
          ENDIF
     ENDIF
     SELECT st_iorep
ENDSCAN
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
RETURN
*
PROCEDURE proceso4
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT st_iscic
SET ORDER TO numsol
SELECT st_iorep
SCAN WHILE auxest = w_estado  ;
     .AND.  .NOT. EOF()
     IF indori = w_codigo
          IF indest <> 'N   '  ;
             .AND. indest <>  ;
             'B   ' .AND. indest <>  ;
             'F   '
               SELECT st_iscic
               SEEK st_iorep.numsol
               IF FOUND()
                    STORE SPACE(70)  ;
                          TO  ;
                          w_info
                    SCAN WHILE  ;
                         numsol =  ;
                         st_iorep.numsol  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF feccom >=  ;
                            w_codind  ;
                            .AND.  ;
                            feccom <=  ;
                            w_codfid
                              w_feccom =  ;
                               feccom
                         ENDIF
                         w_info =  ;
                          ALLTRIM(w_info) +  ;
                          ' ' +  ;
                          inform
                    ENDSCAN
                    DO carga
               ENDIF
          ENDIF
     ENDIF
     SELECT st_iorep
ENDSCAN
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'SACA'
RETURN
*
FUNCTION error
PARAMETER mensaje
DEFINE WINDOW error FROM 19, 10  ;
       TO 19, 66 NONE COLOR  ;
       SCHEME 8
ACTIVATE WINDOW error
CLEAR
colum = INT((56 - LEN(mensaje)) /  ;
        2)
@ 00, colum SAY mensaje COLOR N/W* 
key_aux2 = INKEY(0)
DEACTIVATE WINDOW error
IF key_aux2 == -9
     DO fcinco
ENDIF
RETURN .T.
*
PROCEDURE carga
SELECT st_iclpr
SEEK st_iorep.codent
IF FOUND()
     w_nombre = noment
     w_telef1 = numte1
     w_telef2 = numte2
ELSE
     w_nombre = SPACE(30)
     w_telef1 = 0
     w_telef2 = 0
ENDIF
SELECT usuario
SEEK st_iorep.numsol
IF  .NOT. FOUND()
     APPEND BLANK
ENDIF
REPLACE fecemi WITH  ;
        st_iorep.fecemi, indori  ;
        WITH st_iorep.indori,  ;
        codmod WITH  ;
        st_iorep.codmod
REPLACE numsol WITH  ;
        st_iorep.numsol, codtec  ;
        WITH st_iorep.codtec,  ;
        codmar WITH  ;
        st_iorep.codmar
REPLACE numord WITH  ;
        st_iorep.numdoc, auxest  ;
        WITH st_iorep.auxest,  ;
        numpre WITH  ;
        st_iorep.numpre
REPLACE numser WITH  ;
        st_iorep.numser, indest  ;
        WITH st_iorep.indest,  ;
        observ WITH  ;
        st_iorep.observ
REPLACE codemi WITH  ;
        st_iorep.codemi, flete  ;
        WITH st_iorep.flete,  ;
        cosmob WITH  ;
        st_iorep.cosmob
REPLACE desmob WITH  ;
        st_iorep.desmob, desrep  ;
        WITH st_iorep.desrep
REPLACE cliente WITH w_nombre,  ;
        infant WITH w_info,  ;
        cosrep WITH  ;
        st_iorep.cosrep, totdes  ;
        WITH st_iorep.totdes
REPLACE telef1 WITH w_telef1,  ;
        subtot WITH  ;
        st_iorep.subtot
REPLACE telef2 WITH w_telef2
REPLACE feccom WITH w_feccom
RETURN
*
FUNCTION sacaf7
ACTIVATE WINDOW indicar
@ 00, 20 SAY SPACE(108)
ACTIVATE WINDOW detalle
RETURN .T.
*
FUNCTION principa
ACTIVATE WINDOW indicar
@ 00, 13 SAY SPACE(81)
@ 01, 19 SAY '[ÄÙ] '
@ 01, 26 SAY 'Seleccionar'
@ 01, 37 SAY SPACE(16)
ACTIVATE WINDOW trabajo
RETURN .T.
*
FUNCTION colocaf
ACTIVATE WINDOW indicar
@ 00, 20 SAY  ;
  '[F2]  Grabar       [F3]  Comunicaci¢n [F5]  Estados'
@ 01, 01 SAY  ;
  '[F7]  Presupuesto  [F8]  Informaci¢n  [F9]  Consumo'
ACTIVATE WINDOW detalle
RETURN .T.
*
PROCEDURE ptecla
ON KEY
ON KEY LABEL f2 do grabacion
ON KEY LABEL f3 do historia
ON KEY LABEL f5 do estados
ON KEY LABEL f7 do presupuesto
ON KEY LABEL f8 do pendiente
ON KEY LABEL f9 do consumo
RETURN
*
FUNCTION busord
w_numsol = numsol
SELECT st_iscic
w_area = SELECT()
IF EOF()
     GOTO BOTTOM
ELSE
     IF BOF()
          GOTO TOP
     ENDIF
ENDIF
w_recno = RECNO()
SET ORDER TO numsol
SEEK w_numsol
SCAN WHILE numsol = w_numsol  ;
     .AND.  .NOT. EOF()
     w_feccom = feccom
     w_info = ALLTRIM(w_info) +  ;
              ' ' + SUBSTR(inform,  ;
              1, 70)
ENDSCAN
GOTO w_recno
SET ORDER TO w_area
RETURN w_info
*
*** 
*** ReFox - retrace your steps ... 
***
