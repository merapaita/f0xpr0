*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = ' INFORME '
wrk_progra = PROGRAM()
DO crea_win
ON KEY
@ 02, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INGRESO A SERVICIO POR VENTAS EN GARANTIA '
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_imode ORDER codigo
SELECT 3
USE SHARED st_iseri ORDER codigo
SELECT 4
USE SHARED st_isrep ORDER codigo
a = 1
DO WHILE a=1
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 03, 02 CLEAR TO 12, 73
     @ 03, 02 TO 12, 73
     STORE 1995 TO w_anovta
     STORE 0 TO w_pag, fila,  ;
           w_nmes, w_tipo
     STORE MONTH(DATE()) TO  ;
           w_mesvta
     STORE SPACE(4) TO w_mar1,  ;
           w_mar2, w_tipgar,  ;
           w_linea, w_emi1,  ;
           w_emi2
     STORE SPACE(15) TO w_modelo
     STORE SPACE(15) TO w_mod
     w_orden = ' AND ST_IMODE.LINEA <> SPACE(4) '
     w_fecha = ' AND (MONTH(ST_ISERI.FECVTA) = w_mesvta  AND YEAR(ST_ISERI.FECVTA) = w_anovta) '
     w_orden1 = ' ST_IMODE.LINEA, ST_ISERI.MODELO '
     w_tipo = 1
     SET CURSOR ON
     @ 04, 05 SAY  ;
       'Marca ..........:'
     @ 04, 40 SAY  ;
       'Emisor..........:'
     @ 08, 05 SAY  ;
       'Per¡odo de Venta:'
     @ 08, 40 SAY  ;
       'Clasificaci¢n ..:'
     @ 05, 05 GET w_mar1 PICTURE  ;
       '@!' VALID valida(1,1)  ;
       WHEN antes(1)
     @ 07, 05 GET w_mar2 RANGE  ;
       w_mar1 PICTURE '@!' VALID  ;
       valida(1,2) WHEN antes(1)
     @ 05, 40 GET w_emi1 PICTURE  ;
       '@!' VALID valida(5,1)  ;
       WHEN antes(1)
     @ 07, 40 GET w_emi2 RANGE  ;
       w_emi1 PICTURE '@!' VALID  ;
       valida(5,2) WHEN antes(1)
     READ
     IF LASTKEY() = 27
          a = 0
     ELSE
          sigue = .T.
          DO WHILE sigue
               ON KEY LABEL f6
               @ 09, 05 GET  ;
                 w_pervta DEFAULT  ;
                 1 SIZE 1, 4, 0  ;
                 PICTURE  ;
                 '@*RVTN Mes;A¤o'  ;
                 VALID valida(4, ;
                 w_pervta)
               READ
               IF LASTKEY() = 27
                    EXIT
               ENDIF
               sigue = .F.
          ENDDO
          IF LASTKEY() <> 27
               sigue = .T.
               DO WHILE sigue
                    @ 09, 40 GET  ;
                      w_opcion  ;
                      DEFAULT 1  ;
                      SIZE 1, 10,  ;
                      0 PICTURE  ;
                      '@*RVTN Todos;L¡nea;Modelo'  ;
                      VALID  ;
                      valida(2, ;
                      w_opcion)  ;
                      WHEN  ;
                      antes(2)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    DO procesa  ;
                       WITH  ;
                       w_pervta
                    sigue = .F.
               ENDDO
          ENDIF
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
RETURN
*
PROCEDURE procesa
PARAMETER opc3
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
w_mes = w_mesvta + 11
IF w_mes > 12
     w_anio = w_anovta + 1
ENDIF
SELECT DISTINCT ST_ISERI.CODMAR, ST_ISERI.NUMSER,;
ST_ISERI.MODELO, ST_ISERI.FECVTA, ST_ISREP.NUMDOC,;
ST_ISREP.FECEMI, ST_ISREP.CODEMI, ST_ISREP.INDORI,;
ST_ISREP.INDEST, ST_IMODE.CODCLA, ST_IMODE.LINEA,;
MONTH(ST_ISERI.FECVTA), MONTH(ST_ISREP.FECEMI);
FROM ST_ISREP, ST_ISERI, ST_IMODE WHERE;
ST_ISERI.CODMAR = ST_ISREP.CODMAR AND;
ST_ISERI.MODELO = ST_ISREP.CODMOD AND;
ST_ISERI.NUMSER = ST_ISREP.NUMSER AND;
ST_IMODE.CODMOD = ST_ISERI.MODELO AND;
ST_IMODE.CODMAR = ST_ISERI.CODMAR AND;
ST_ISREP.CODMAR>= w_mar1 and ST_ISREP.CODMAR<=;
w_mar2 AND ST_ISREP.CODEMI>= w_emi1 and;
ST_ISREP.CODEMI<= w_emi2 AND ST_ISREP.INDORI;
= "GARA" AND st_isrep.fecemi>=st_iseri.fecvta;
and st_isrep.fecemi<=(st_iseri.fecvta+365);
&w_fecha &w_orden ORDER BY &w_orden1 INTO;
CURSOR QUERY
COUNT TO w_valor
DO mensa WITH  ;
   '***  Un momento, Por Favor ...  ***',  ;
   'SACA'
IF w_valor = 0
     DO error WITH  ;
        '***  No Existen registros a Listar  ***'
     RETURN
ENDIF
CREATE CURSOR nuevo (mes1 N (3,  ;
       0), mes2 N (3, 0), mes3 N  ;
       (3, 0), mes4 N (3, 0),  ;
       mes5 N (3, 0), mes6 N (3,  ;
       0), mes7 N (3, 0), mes8 N  ;
       (3, 0), mes9 N (3, 0),  ;
       mes10 N (3, 0), mes11 N (3,  ;
       0), mes12 N (3, 0), mes13  ;
       N (3, 0), fecvta D, fecemi  ;
       D, linea C (4), modelo C  ;
       (20), marca C (4))
w_nmes = (12 - w_mesvta) + 1
IF YEAR(DATE()) = w_anovta + 1
     w_nmes = w_nmes +  ;
              MONTH(DATE())
ELSE
     w_nmes = w_nmes + 12
ENDIF
DIMENSION nmes( 13)
SELECT query
GOTO TOP
DO WHILE  .NOT. EOF()
     w_mes = exp_12
     DO WHILE exp_12=w_mes .AND.   ;
        .NOT. EOF()
          w_linea = linea
          FOR i = 1 TO 13
               nmes( i) = 0
          ENDFOR
          DO WHILE exp_12=w_mes  ;
             .AND. linea=w_linea  ;
             .AND.  .NOT. EOF()
               IF YEAR(fecemi) =  ;
                  w_anovta
                    i = (exp_13 -  ;
                        w_mes) +  ;
                        1
               ELSE
                    i = (12 -  ;
                        w_mes) +  ;
                        exp_13 +  ;
                        1
               ENDIF
               IF i > 0 .AND. i <=  ;
                  13
                    nmes( i) =  ;
                        nmes(i) +  ;
                        1
                    w_fecemi = fecemi
                    w_modelo = modelo
                    w_fecvta = fecvta
                    w_marca = codmar
               ENDIF
               SKIP
          ENDDO
          SELECT nuevo
          APPEND BLANK
          REPLACE linea WITH  ;
                  w_linea
          REPLACE modelo WITH  ;
                  w_modelo
          REPLACE fecemi WITH  ;
                  w_fecemi
          REPLACE fecvta WITH  ;
                  w_fecvta
          REPLACE marca WITH  ;
                  w_marca
          FOR i = 1 TO 13
               varm = 'mes' +  ;
                      ALLTRIM(STR(i))
               repl &varm with nmes(i)
          ENDFOR
          SELECT query
     ENDDO
ENDDO
DO esc_indica WITH 2, 'BBB',  ;
   'VER', 'IMP', 'ESC'
DO WHILE LASTKEY()<>27 .AND.  ;
   LASTKEY()<>-6 .AND. LASTKEY()<>- ;
   4
     w_inkey = INKEY(0, 'H')
ENDDO
IF w_inkey = -6
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO mensa WITH  ;
        '*** I m p r i m i e n d o ... ***',  ;
        'COLO'
     DO ooprint
     DO mensa WITH  ;
        '*** I m p r i m i e n d o ... ***',  ;
        'SACA'
ENDIF
w_ver = .T.
IF w_inkey = -4 .AND.  .NOT.  ;
   w_ver
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     w_file = SUBSTR(f_archivo(),  ;
              1, 8) + '.TXT'
     IF opc3 = 1
          REPO FORM PORL4116 TO FILE;
 &w_file  NOCONSOLE
     ELSE
          REPO FORM POR4116A TO FILE;
 &w_file  NOCONSOLE
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     MODI COMM &w_file NOEDIT WINDOW PANTALL
     DELE FILE &w_file
ENDIF
RETURN
*
PROCEDURE ooprint
DIMENSION submes( 13)
DIMENSION totmes( w_nmes)
FOR m = 1 TO w_nmes
     IF m <= 13
          submes( m) = 0
     ENDIF
     totmes( m) = 0
ENDFOR
w_esp = 08
fcol = (w_nmes - 1) * w_esp + 25
SET CONSOLE OFF
SET PRINTER ON
SET DEVICE TO PRINTER
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
? CHR(15)
DO encabe
SELECT nuevo
GOTO TOP
w_acum1 = 0
w_acum2 = 0
w_acum3 = 0
DO WHILE  .NOT. EOF()
     DO control
     FOR i = 1 TO 13
          submes( i) = 0
     ENDFOR
     w_tmes = MONTH(fecvta)
     @ fila, 02 SAY  ;
       'Mes de Venta:' +  ;
       oodesmes(w_tmes)
     w_acum2 = 0
     DO WHILE MONTH(fecvta)= ;
        w_tmes .AND.  .NOT.  ;
        EOF()
          DO control
          @ fila, 2 SAY  ;
            ootab('LINE',linea)
          x = 0
          col = 0
          w_acum1 = 0
          FOR i = 1 TO 13
               nomes = 'mes' +  ;
                       ALLTRIM(STR(i))
               col = 25 + (w_esp *  ;
                     (w_tmes -  ;
                     w_mesvta)) +  ;
                     x
               IF col <= fcol
                    @fila,col say &nomes
               ENDIF
               x = x + w_esp
               acum=&nomes
               submes( i) =  ;
                     submes(i) +  ;
                     acum
               w_acum1 = w_acum1 +  ;
                         acum
          ENDFOR
          @ fila, 220 SAY  ;
            STR(w_acum1, 4, 0)
          w_acum2 = w_acum2 +  ;
                    w_acum1
          SELECT nuevo
          SKIP
     ENDDO
     DO control
     @ fila, 2 SAY REPLICATE('-',  ;
       226)
     DO control
     @ fila, 2 SAY ' Sub Total :'
     x = 0
     FOR i = 1 TO 13
          col = 25 + (w_esp *  ;
                (w_tmes -  ;
                w_mesvta)) + x
          IF col <= fcol
               @ fila, col SAY  ;
                 STR(submes(i), 3,  ;
                 0)
          ENDIF
          x = x + w_esp
          y = (w_tmes - w_mesvta) +  ;
              i
          IF y <= w_nmes
               totmes( y) =  ;
                     totmes(y) +  ;
                     submes(i)
          ENDIF
     ENDFOR
     @ fila, 220 SAY STR(w_acum2,  ;
       4, 0)
     w_acum3 = w_acum3 + w_acum2
     DO control
     @ fila, 2 SAY REPLICATE('-',  ;
       226)
ENDDO
DO control
@ fila, 2 SAY REPLICATE('=', 226)
DO control
@ fila, 2 SAY ' TOTAL GENERAL:'
FOR i = 1 TO w_nmes
     xcol = 25 + (w_esp * (i -  ;
            1))
     IF xcol <= col
          @ fila, xcol SAY  ;
            STR(totmes(i), 3, 0)
     ENDIF
ENDFOR
@ fila, 217 SAY TRANSFORM(w_acum3,  ;
  '999,999')
DO control
@ fila, 2 SAY REPLICATE('=', 226)
?? CHR(18)
EJECT
SET PRINTER OFF
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
RETURN
*
PROCEDURE encabe
w_pag = w_pag + 1
@ 2, 002 SAY rge_razsoc
@ 2, 210 SAY 'P gina:' +  ;
  STR(w_pag, 2, 0)
@ 3, 002 SAY rge_abrev
@ 3, 090 SAY  ;
  'INGRESOS DE ARTICULOS EN GARANTIA'
@ 3, 210 SAY 'Fecha   :' +  ;
  DTOC(DATE())
@ 4, 002 SAY rge_calle
@ 4, 090 SAY  ;
  'CON A¥O DE VENTA ---->' +  ;
  STR(w_anovta, 4, 0)
@ 4, 210 SAY 'Hora    :' + TIME()
@ 5, 210 SAY 'Programa:' +  ;
  wrk_progra
@ 6, 002 SAY REPLICATE('-', 226)
x = 23
FOR i = 1 TO w_nmes
     l = i + (w_mesvta - 1)
     @ 7, x SAY nommes(l)
     x = x + 8
ENDFOR
@ 07, 222 SAY 'TOTAL'
@ 08, 02 SAY REPLICATE('-', 226)
@ 09, 02 SAY 'MARCA DESDE :' +  ;
  w_mar1 + SUBSTR(ootab('MARC', ;
  w_mar1), 1, 10)
@ 09, 32 SAY 'EMISOR DESDE:' +  ;
  w_emi1 + SUBSTR(ootab('EMIS', ;
  w_emi1), 1, 10)
@ 09, 60 SAY 'LINEA :'
DO CASE
     CASE w_opcion = 1
          @ 9, 72 SAY 'TODOS'
          @ 9, 100 SAY 'MODELO :'
          @ 9, 110 SAY 'TODOS'
     CASE w_opcion = 2
          @ 9, 72 SAY w_linea
          @ 9, 80 SAY  ;
            SUBSTR(ootab('LINE', ;
            w_linea), 1, 18)
          @ 9, 100 SAY 'MODELO :'
          @ 9, 110 SAY 'TODOS'
     CASE w_opcion = 3
          @ 9, 72 SAY 'TODOS'
          @ 9, 100 SAY 'MODELO :'
          @ 9, 110 SAY w_mod
ENDCASE
@ 10, 002 SAY 'MARCA HASTA :' +  ;
  w_mar2 + SUBSTR(ootab('MARC', ;
  w_mar2), 1, 10)
@ 10, 032 SAY 'EMISOR HASTA:' +  ;
  w_emi2 + SUBSTR(ootab('EMIS', ;
  w_emi1), 1, 10)
fila = 11
RETURN
*
PROCEDURE control
IF fila = 59
     DO encabe
ELSE
     fila = fila + 1
ENDIF
RETURN
*
FUNCTION nommes
PARAMETER opc
xopc = opc
IF opc >= 13
     opc = opc - 12
ENDIF
DO CASE
     CASE opc = 1
          detmes = 'ENE'
     CASE opc = 2
          detmes = 'FEB'
     CASE opc = 3
          detmes = 'MAR'
     CASE opc = 4
          detmes = 'ABR'
     CASE opc = 5
          detmes = 'MAY'
     CASE opc = 6
          detmes = 'JUN'
     CASE opc = 7
          detmes = 'JUL'
     CASE opc = 8
          detmes = 'AGO'
     CASE opc = 9
          detmes = 'SET'
     CASE opc = 10
          detmes = 'OCT'
     CASE opc = 11
          detmes = 'NOV'
     CASE opc = 12
          detmes = 'DIC'
ENDCASE
IF xopc >= 13
     detmes = detmes +  ;
              SUBSTR(STR(w_anovta +  ;
              1, 4, 0), 3, 2)
ELSE
     detmes = detmes +  ;
              SUBSTR(STR(w_anovta,  ;
              4, 0), 3, 2)
ENDIF
RETURN detmes
*
PROCEDURE antes
PARAMETER opc1
DO CASE
     CASE opc1 = 1
          ON KEY LABEL f6 do ayuda10
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
     CASE opc1 = 2
          ON KEY LABEL f6
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
ENDCASE
RETURN
*
PROCEDURE ayuda10
IF VARREAD() = 'W_MAR1' .OR.  ;
   VARREAD() = 'W_MAR2'
     SELECT ge_tab0
     SET FILTER TO tab_codpre = 'MARC'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() = 'W_EMI1' .OR.  ;
   VARREAD() = 'W_EMI2'
     SELECT ge_tab0
     SET FILTER TO tab_codpre = 'EMIS'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE EMISOR'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() = 'W_LINEA'
     SELECT ge_tab0
     SET ORDER TO codigo
     SET FILTER TO tab_codpre = 'LINE'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE LINEAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF VARREAD() == 'W_MOD'
     w_select = SELECT()
     SELECT st_imode
     SET FILTER TO (codmar >= w_mar1;
.AND. codmar <= w_mar2)
     w_selpro = SELECT()
     w_campo = SPACE(15)
     DO pro2 WITH w_campo,  ;
        w_select, w_selpro, 1
     IF LASTKEY() <> 27
          w_mod = w_campo
          KEYBOARD w_mod
     ENDIF
     SELECT st_imode
     SET FILTER TO
     SET ORDER TO codigo
     SELECT (w_select)
ENDIF
RETURN
*
FUNCTION valida
PARAMETER w_opc, opc
DO CASE
     CASE w_opc = 1
          SELECT ge_tab0
          IF opc = 1
               SEEK 'MARC' +  ;
                    w_mar1
               fil = 5
          ELSE
               SEEK 'MARC' +  ;
                    w_mar2
               fil = 7
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Marca No Existe ***'
               RETURN .F.
          ENDIF
          @ fil, 12 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE w_opc = 2
          DO CASE
               CASE opc = 1
                    w_orden = ' AND ST_IMODE.LINEA <> SPACE(4) '
               CASE opc = 2
                    @ 10, 52 GET  ;
                      w_linea  ;
                      PICTURE  ;
                      '@!' VALID   ;
                      .NOT.  ;
                      EMPTY(w_linea)  ;
                      WHEN  ;
                      antes(1)
                    READ
                    IF LASTKEY() =  ;
                       27
                         RETURN .F.
                    ENDIF
                    SELECT ge_tab0
                    SEEK 'LINE' +  ;
                         w_linea
                    IF  .NOT.  ;
                        FOUND()
                         DO error  ;
                            WITH  ;
                            '*** C¢digo de L¡nea No Existe ***'
                         RETURN .F.
                    ENDIF
                    @ 10, 57 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 18)
                    w_orden = ' AND ST_IMODE.LINEA = w_linea '
               CASE opc = 3
                    @ 11, 52 GET  ;
                      w_mod  ;
                      PICTURE  ;
                      '@!' VALID   ;
                      .NOT.  ;
                      EMPTY(w_mod)  ;
                      WHEN  ;
                      antes(1)
                    READ
                    IF LASTKEY() =  ;
                       27
                         RETURN .F.
                    ENDIF
                    SELECT st_imode
                    SEEK w_mar1 +  ;
                         w_mod
                    IF  .NOT.  ;
                        FOUND()
                         DO error  ;
                            WITH  ;
                            '*** C¢digo de Modelo No Existe ***'
                         RETURN .F.
                    ENDIF
                    w_orden = ' AND ST_IMODE.CODMOD = w_mod '
          ENDCASE
     CASE w_opc = 3
          w_desmes = oodesmes(w_mesvta)
          @ 05, 25 SAY w_desmes
     CASE w_opc = 4
          DO CASE
               CASE opc = 1
                    @ 09, 15 GET  ;
                      w_mesvta  ;
                      RANGE 1,12  ;
                      PICTURE  ;
                      '99' WHEN  ;
                      antes(2)
                    @ 09, 20 GET  ;
                      w_anovta  ;
                      PICTURE  ;
                      '9999' WHEN  ;
                      antes(2)
                    READ
                    IF LASTKEY() =  ;
                       27
                         RETURN .F.
                    ENDIF
                    w_fecha = ' AND (MONTH(ST_ISERI.FECVTA) = w_mesvta  AND YEAR(ST_ISERI.FECVTA) = w_anovta) '
                    w_orden1 = ' ST_IMODE.LINEA,ST_ISERI.MODELO,st_isrep.fecemi '
               CASE opc = 2
                    @ 10, 15 GET  ;
                      w_anovta  ;
                      PICTURE  ;
                      '9999' WHEN  ;
                      antes(2)
                    READ
                    IF LASTKEY() =  ;
                       27
                         RETURN .F.
                    ENDIF
                    w_mesvta = 1
                    w_fecha = ' AND YEAR(ST_ISERI.FECVTA) = w_anovta'
                    w_orden1 = ' 12, ST_IMODE.LINEA,st_isrep.fecemi '
          ENDCASE
     CASE w_opc = 5
          SELECT ge_tab0
          IF opc = 1
               SEEK 'EMIS' +  ;
                    w_emi1
               fil = 5
          ELSE
               SEEK 'EMIS' +  ;
                    w_emi2
               fil = 7
          ENDIF
          IF  .NOT. FOUND()
               DO error WITH  ;
                  ' *** C¢digo no existe ****'
               RETURN .F.
          ENDIF
          @ fil, 46 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
