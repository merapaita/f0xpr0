*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4127>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ANALISIS FECHA DE VENCIMIENTO '
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_IOREP ORDER CODIGO
pas4127 = da_nombre()
Create table &PAS4127 (pas_orden C(8),pas_fecemi;
D(8),pas_numsol C(8),pas_mode C(15),pas_esta;
C(4),pas_feccom D(8),pas_fecfin D(8))
SELECT 2
use &PAS4127 EXCLUSIVE   
DO WHILE ppas
     wk_hay = .F.
     STORE CTOD('  /  /  ') TO  ;
           wk_fec1, wk_fec2
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 5, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     @ 4, 5 SAY  ;
       'Fecha Desde :                  Fecha Hasta :        '
     @ 4, 20 GET wk_fec1 PICTURE  ;
       '!!/!!/!!' VALID fecha1()
     @ 4, 47 GET wk_fec2 PICTURE  ;
       '!!/!!/!!' VALID fecha2()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() <> -9
          pregunta = '(feccom>=wk_fec1).and.(feccom<=wk_fec2)'
          tit_impre = '"VENCIMIENTO DESDE "+" " +dtoc(wk_fec1)+" HASTA "+dtoc(wk_fec2)'
          DO CASE
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 1
                    ppas = .F.
                    LOOP
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 2
                    LOOP
          ENDCASE
          DO vali_exist
     ENDIF
     IF LASTKEY() <> 27 .AND.  ;
        LASTKEY() <> -9
          IF wk_hay = .F.
               DO error WITH  ;
                  '** No Se Registra Ninguna Fecha de Vencimiento **'
               KEYBOARD '{CTRL+Y}'  ;
                        PLAIN
               LOOP
          ENDIF
          key = INKEY()
          DO WHILE (key<>27)  ;
             .AND. (key<>-9)  ;
             .AND. (key<>-6)
               IF key == 0
                    key = 255
               ENDIF
               DO esc_modo WITH  ;
                  'S'
               DO esc_indica WITH  ;
                  1, 'AYU', 'IMP',  ;
                  'BBB', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
               key = INKEY(0)
          ENDDO
          IF key == -6
               DO lis_analis
               LOOP
          ENDIF
          IF key == -9
               SELECT 2
               ZAP
               LOOP
          ENDIF
          IF key == 27
               ppas = .F.
               LOOP
          ENDIF
     ENDIF
     IF LASTKEY() == -9
          SELECT 2
          ZAP
          ppas = .T.
     ENDIF
     IF LASTKEY() == 27
          ppas = .F.
          LOOP
     ENDIF
ENDDO
DO saca_win
@ 24, 69 SAY SPACE(10)
ON KEY LABEL F6
SELECT 3
USE
SELECT 2
USE
x4127 = pas4127 + '.DBF'
ERASE &X4127
x4127 = pas4127 + '.IDX'
ERASE &X4127
CLOSE DATABASES
RETURN
*
PROCEDURE vali_exist
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_numsol = SPACE(8)
SELECT 3
USE SHARED st_isrep ORDER CODIGO
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF indest = 'C' .OR. indest =  ;
        'F'
          wk_numord = numdoc
          wk_mode = codmod
          wk_numsol = numsol
          wk_fecfin = fecfin
          wk_estado = indest
          SELECT 3
          SET ORDER TO 1
          GOTO TOP
          SEEK wk_numsol
          IF FOUND()
               if &pregunta
                    wk_fecemi = fecemi
                    wk_feccom = feccom
                    SELECT 2
                    APPEND BLANK
                    REPLACE pas_orden  ;
                            WITH  ;
                            wk_numord
                    REPLACE pas_fecemi  ;
                            WITH  ;
                            wk_fecemi
                    REPLACE pas_numsol  ;
                            WITH  ;
                            wk_numsol
                    REPLACE pas_mode  ;
                            WITH  ;
                            wk_mode
                    REPLACE pas_esta  ;
                            WITH  ;
                            wk_estado
                    REPLACE pas_feccom  ;
                            WITH  ;
                            wk_feccom
                    REPLACE pas_fecfin  ;
                            WITH  ;
                            wk_fecfin
                    wk_hay = .T.
               ENDIF
          ENDIF
     ENDIF
     SELECT 1
     SKIP
ENDDO
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_orden to &PAS4127;
 
ENDIF
SELECT 3
USE
RETURN
*
PROCEDURE lis_analis
STORE SPACE(4) TO vie_marca,  ;
      vie_indori
STORE SPACE(15) TO vie_mode
STORE 0 TO wk_contado, acum
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag,  ;
      wk_garanti
STORE 0 TO wk_canti, wk_garmar,  ;
      tope
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
tit1 = 'No.ORDEN FECHA DE  No. DE       MODELO     ESTADO FECHA DE FECHA DE  DIFERENCIA'
tit2 = '         SOLICIT.  SOLICIT.                       COMPROM. REPARAC.  EN DIAS   '
con_lin = 12
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
DO PIE WITH PAG,tit1,tit2,wk_fec1,wk_fec2,"ANALISIS FECHA DE VENCIMIENTO",&tit_impre
SELECT 2
SET ORDER TO 1
GOTO TOP
DO WHILE  .NOT. EOF()
     @ con_lin, 1 SAY pas_orden
     @ con_lin, 10 SAY pas_fecemi
     @ con_lin, 20 SAY pas_numsol
     @ con_lin, 30 SAY pas_mode
     @ con_lin, 46 SAY pas_esta
     @ con_lin, 51 SAY pas_feccom
     @ con_lin, 60 SAY pas_fecfin
     @ con_lin, 69 SAY pas_fecfin -  ;
       pas_feccom
     con_lin = con_lin + 1
     IF con_lin = 50
          EJECT
          do pie with pag,tit1,TIT2,wk_fec1,WK_FEC2,"ANALISIS FECHA DE VENCIMIENTO",&tit_impre
          con_lin = 12
     ENDIF
     SELECT 2
     SKIP
ENDDO
EJECT
? CHR(18)
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
ZAP
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, titu2,  ;
          wk_fech, wk_fech2, pie1,  ;
          pie2
PRIVATE centro
centro = 0
pag = pag + 1
@ 1, 0 SAY empre1
@ 1, 60 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
centro = INT((80 - LEN(pie1)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 2, 60 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
centro = INT((80 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro SAY pie1
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 4, centro SAY pie2
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY titu2
@ 10, 0 SAY REPLICATE('=', 80)
RETURN
*
PROCEDURE fecha1
IF EMPTY(wk_fec1)
     wk_fec1 = DATE()
ENDIF
RETURN
*
PROCEDURE fecha2
IF EMPTY(wk_fec2)
     wk_fec2 = DATE()
ENDIF
RETURN
*
FUNCTION me
IF EMPTY(wk_mes)
     wk_mes = INT(MONTH(DATE()))
ELSE
     IF wk_mes < 1 .OR. wk_mes >  ;
        12
          RETURN .F.
     ENDIF
ENDIF
DO CASE
     CASE wk_mes = 1
          wk_meses = 'ENERO'
     CASE wk_mes = 2
          wk_meses = 'FEBRERO'
     CASE wk_mes = 3
          wk_meses = 'MARZO'
     CASE wk_mes = 4
          wk_meses = 'ABRIL'
     CASE wk_mes = 5
          wk_meses = 'MAYO'
     CASE wk_mes = 6
          wk_meses = 'JUNIO'
     CASE wk_mes = 7
          wk_meses = 'JULIO'
     CASE wk_mes = 8
          wk_meses = 'AGOSTO'
     CASE wk_mes = 9
          wk_meses = 'SEPTIEMBRE'
     CASE wk_mes = 10
          wk_meses = 'OCTUBRE'
     CASE wk_mes = 11
          wk_meses = 'NOVIEMBRE'
     CASE wk_mes = 12
          wk_meses = 'DICIEMBRE'
ENDCASE
@ 04, 17 SAY SPACE(20)
@ 04,17 say wk_meses color &color3
RETURN .T.
*
FUNCTION a¥
IF LASTKEY() = 5
     RETURN .T.
ENDIF
IF EMPTY(wk_year)
     wk_year = YEAR(DATE())
ELSE
     IF wk_year < 1
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
