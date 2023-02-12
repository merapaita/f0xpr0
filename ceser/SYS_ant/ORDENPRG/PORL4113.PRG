*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = ' INFORME '
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INGRESO DE MODELOS POR MARCA '
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE ST_IOREP ORDER CODIGO
a = 1
DO WHILE a=1
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 03, 02 CLEAR TO 09, 73
     @ 03, 02 TO 09, 73
     STORE 1 TO wrk_opcion,  ;
           wrk_copia
     STORE DATE() TO wrk_fecini,  ;
           wrk_fecfin
     STORE SPACE(4) TO wrk_marini,  ;
           wrk_marfin
     wrk_fecha = ' AND (ST_IOREP.FECEMI >= wrk_fecini  AND ST_IOREP.FECEMI <= wrk_fecfin) '
     SET CURSOR ON
     @ 04, 04 SAY  ;
       'Marca ...: De  : '
     @ 04, 40 SAY 'A  : '
     @ 05, 04 SAY  ;
       'Peri¢do .: Del :'
     @ 05, 40 SAY 'Al : '
     @ 06, 04 SAY 'Destino .:'
     @ 04, 21 GET wrk_marini  ;
       PICTURE '@!' VALID  ;
       valida(1,wrk_marini) WHEN  ;
       antes(1)
     @ 04, 45 GET wrk_marfin  ;
       PICTURE '@!' VALID  ;
       valida(1,wrk_marfin) WHEN  ;
       antes(1)
     @ 05, 21 GET wrk_fecini  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_fecini) WHEN  ;
       antes(2)
     @ 05, 45 GET wrk_fecfin  ;
       RANGE wrk_fecini PICTURE  ;
       '@!' VALID  .NOT.  ;
       EMPTY(wrk_fecini) WHEN  ;
       antes(2)
     READ
     IF LASTKEY() = 27
          DO saca_win
          RETURN
     ENDIF
     @ 06, 15 GET wrk_opcion  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RVTN Pantalla;Impresora'  ;
       VALID valida(2,0)
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     DO procesa
ENDDO
*
PROCEDURE procesa
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
SELECT ST_IOREP.NUMDOC, ST_IOREP.FECEMI,;
ST_IOREP.FECFIN, ST_IOREP.CODEMI, ST_IOREP.INDORI,;
ST_IOREP.INDEST, ST_IOREP.AUXEST, ST_IOREP.CODMAR,;
ST_IOREP.CODMOD, ST_IOREP.NUMSER, ST_IOREP.NUMSOL,;
ST_IOREP.CODTALL FROM ST_IOREP WHERE ST_IOREP.INDORI;
= "GARA" &wrk_fecha AND ST_IOREP.INDEST;
<> "N" AND (ST_IOREP.CODMAR = wrk_marini;
OR  ST_IOREP.CODMAR = wrk_marfin) ORDER;
BY ST_IOREP.CODMAR, ST_IOREP.CODTALL,;
ST_IOREP.CODMOD into cursor query
COUNT TO wrk_valor
DO mensa WITH  ;
   '***  Un momento, Por Favor ...  ***',  ;
   'SACA'
IF wrk_valor = 0
     DO error WITH  ;
        '***  No Existen registros a Listar  ***'
     RETURN
ENDIF
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
IF wrk_opcion = 2
     DO mensa WITH  ;
        '*** I m p r i m i e n d o ... ***',  ;
        'COLO'
     ??? CHR(15)
     REPORT FORMAT PORL4113 TO  ;
            PRINTER NOCONSOLE
     SET PRINTER TO
     DO mensa WITH  ;
        '*** I m p r i m i e n d o ... ***',  ;
        'SACA'
ENDIF
IF wrk_opcion = 1
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     wrk_file = SUBSTR(f_archivo(),  ;
                1, 8) + '.TXT'
     REPO FORM PORL4113 TO FILE;
 &wrk_file  NOCONSOLE
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     MODI COMM &wrk_file NOEDIT WINDOW;
PANTALL
     DELE FILE &wrk_file
ENDIF
*
PROCEDURE antes
PARAMETER opc1
DO CASE
     CASE opc1 = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 DO AYUDA
     CASE opc1 = 2
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
ENDCASE
*
FUNCTION valida
PARAMETER opc, opc1
DO CASE
     CASE opc = 1
          SELECT 1
          SEEK 'MARC' + opc1
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Marca No Existe ***'
               RETURN .F.
          ENDIF
          IF COL() < 40
               @ 04, 26 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 10)
          ELSE
               @ 04, 50 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 10)
          ENDIF
     CASE opc = 2
          IF wrk_opcion = 2
               @ 07, 40 SAY  ;
                 'Copias :' GET  ;
                 wrk_copia  ;
                 PICTURE '99'  ;
                 VALID  .NOT.  ;
                 EMPTY(wrk_copia)
               READ
               IF LASTKEY() = 27
                    wrk_copia = 1
                    @ 07, 40  ;
                      CLEAR TO 07,  ;
                      70
                    RETURN .F.
               ENDIF
          ENDIF
ENDCASE
*
PROCEDURE ayuda
SELECT 1
SET FILTER TO tab_codpre == 'MARC'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
*
*** 
*** ReFox - retrace your steps ... 
***
