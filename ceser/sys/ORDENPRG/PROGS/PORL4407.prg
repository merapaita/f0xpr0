*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0407>'
tit_prg = 'INFORME'
@ 4, 69 SAY ind_prg
wrk_progra = PROGRAM()
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
SET COLOR TO &COLOR3
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ESTADO DE ORDENES'
ppas = .T.
SELECT 1
USE SHARED st_iorep ORDER codigo
pas407 = da_nombre()
Create table &PAS407 (pas_nume N(8), PAS_NUMSOL;
C(8), pas_marca C(4),pas_indori C(4),pas_desmode;
C(35),pas_rutcli C(9),pas_nomcli C(30);
,pas_ingre D,pas_compre D,pas_estado C(4),pas_codaux;
C(4),pas_desaux C(35),pas_repa D,pas_dife;
N(7))
SELECT 2
use &PAS407 EXCLUSIVE     
DO WHILE ppas
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_feservi = CTOD( ;
                  '  /  /  ')
     wk_hay = .F.
     STORE SPACE(35) TO pregunta,  ;
           tit_impre
     STORE 0 TO retorna, wk_mes,  ;
           wk_year
     DO popap WITH 08, 29, op,  ;
        retorna
     efecin = 1
     IF retorna = 0
          CLEAR READ
          EXIT
          efecin = 1
     ENDIF
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     IF retorna = 1
          @ 4, 5 SAY  ;
            'Fecha de Proceso  :                          '
     ENDIF
     IF retorna = 2
          @ 4, 5 SAY  ;
            '    Mes :                          A¤o :     '
     ENDIF
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     DO vali
     IF wk_hay = .F.
          DO error WITH  ;
             '** No Se Ordenes Para Esta Fecha **'
          LOOP
     ENDIF
     ON KEY LABEL F7 DO FSIETE
     key = INKEY()
     DO WHILE key<>27 .AND. key<>- ;
        9 .AND. key<>-6
          IF key == 0
               key = 255
          ENDIF
          DO esc_modo WITH 'S'
          DO esc_indica WITH 1,  ;
             'AYU', 'IMP', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
          key = INKEY(0)
     ENDDO
     ON KEY LABEL F7
     IF key == -6
          DO lis_arti4
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
ENDDO
DO saca_win
@ 24, 69 SAY SPACE(10)
ON KEY LABEL F6
ON KEY LABEL F10
SELECT 3
USE
SELECT 2
USE
x407 = pas407 + '.DBF'
ERASE &X407
x407 = pas407 + '.IDX'
ERASE &X407
CLOSE DATABASES
RETURN
*
PROCEDURE fsiete
RETURN
*
PROCEDURE fecha
IF EMPTY(wk_feservi)
     wk_feservi = DATE()
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
PROCEDURE vali
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_marca = SPACE(4)
wk_numsol = SPACE(8)
wk_mode = SPACE(15)
STORE CTOD('  /  /  ') TO  ;
      wk_fecemi, wk_feccom,  ;
      wk_repa
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_imode ORDER codigo
SELECT 5
USE SHARED st_iclpr ORDER codigo
SELECT 6
USE SHARED st_isrep ORDER codigo
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta
          wk_nume = VAL(numdoc)
          wk_indori = indori
          wk_marca = codmar
          wk_mode = codmod
          wk_codcli = codent
          wk_fecemi = fecemi
          wk_repa = fecfin
          wk_codaux = auxest
          wk_numsol = numsol
          SELECT 6
          SET ORDER TO 1
          GOTO TOP
          SEEK wk_numsol
          IF FOUND()
               wk_feccom = feccom
          ENDIF
          SELECT 4
          SET ORDER TO 1
          GOTO TOP
          codaux = wk_marca +  ;
                   wk_mode
          SEEK '&codaux'
          wk_nommod = nommod
          SELECT 3
          GOTO TOP
          codaux = 'MARC' +  ;
                   wk_marca
          SEEK '&codaux'
          wk_desmarc = tab_destab
          SELECT 3
          GOTO TOP
          codaux = 'ESOR' +  ;
                   wk_codaux
          SEEK '&codaux'
          wk_desaux = tab_destab
          SELECT 5
          GOTO TOP
          SEEK 'C' + wk_codcli
          wk_nomcli = noment
          IF YEAR(wk_repa) = 0
               wk_dife = DATE() -  ;
                         wk_fecemi
          ELSE
               wk_dife = wk_repa -  ;
                         wk_fecemi
          ENDIF
          SELECT 2
          APPEND BLANK
          REPLACE pas_nume WITH  ;
                  wk_nume
          REPLACE pas_marca WITH  ;
                  wk_marca
          REPLACE pas_desmod WITH  ;
                  wk_nommod
          REPLACE pas_indori WITH  ;
                  wk_indori
          REPLACE pas_rutcli WITH  ;
                  wk_codcli
          REPLACE pas_nomcli WITH  ;
                  wk_nomcli
          REPLACE pas_ingre WITH  ;
                  wk_fecemi
          REPLACE pas_compre WITH  ;
                  wk_feccom
          REPLACE pas_repa WITH  ;
                  wk_repa
          REPLACE pas_dife WITH  ;
                  wk_dife
          REPLACE pas_codaux WITH  ;
                  wk_codaux
          REPLACE pas_desaux WITH  ;
                  wk_desaux
          REPLACE pas_numsol WITH  ;
                  wk_numsol
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_codaux + Str(pas_nume,8);
to &PAS407    
ENDIF
SELECT 3
USE
SELECT 4
USE
SELECT 5
USE
SELECT 6
USE
RETURN
*
PROCEDURE lis_arti4
tit1 = ' CODIGO      MODELO                               CLIENTE                          FECHA       FECHA        FECHA           TIEMPO '
tit2 = '                                                                                  RECEPCION   COMPROME.   REPARACION       REPARAC.'
con_lin = 11
STORE 0 TO tot_mode, sum_gen, pag,  ;
      wk_garmar, wk_dife
wk_antiguo = SPACE(2)
wk_viejora = 8
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(15)
SELECT 2
SET ORDER TO 1
GOTO TOP
DO PIE WITH PAG,tit1,tit2,wk_feservi,"ESTADO ORDENES DE REPARACION",&tit_impre
DO WHILE  .NOT. EOF()
     IF con_lin = 50
          EJECT
          DO PIE WITH PAG,tit1,tit2,wk_feservi,"ESTADO ORDENES DE REPARACION",&tit_impre
          con_lin = 11
     ENDIF
     IF pas_codaux <> wk_antiguo
          IF  .NOT.  ;
              EMPTY(wk_antiguo)
               con_lin = con_lin +  ;
                         1
               @ con_lin, 90 SAY  ;
                 'TOTAL ESTADO :' +  ;
                 wk_antiguo
               @ con_lin, 111 SAY  ;
                 tot_mode PICTURE  ;
                 '99,999,999'
               con_lin = con_lin +  ;
                         1
               sum_gen = sum_gen +  ;
                         tot_mode
               tot_mode = 0
          ENDIF
          @ con_lin, 1 SAY  ;
            pas_codaux PICTURE  ;
            '!!!!'
          @ con_lin, 8 SAY  ;
            pas_desaux
     ENDIF
     con_lin = con_lin + 1
     @ con_lin, 1 SAY pas_nume  ;
       PICTURE '99999999'
     @ con_lin, 11 SAY  ;
       SUBSTR(pas_desmod, 1, 20)
     @ con_lin, 35 SAY pas_nomcli  ;
       PICTURE '@!'
     @ con_lin, 80 SAY pas_ingre
     @ con_lin, 92 SAY pas_compre
     @ con_lin, 104 SAY pas_repa
     @ con_lin, 123 SAY pas_dife  ;
       PICTURE '999.99'
     @ con_lin, 130 SAY  ;
       pas_numsol
     wk_antiguo = pas_codaux
     wk_des2 = pas_desaux
     tot_mode = tot_mode + 1
     SKIP
ENDDO
sum_gen = sum_gen + tot_mode
IF con_lin = 50
     EJECT
     DO PIE WITH PAG,tit1,tit2,wk_feservi,"ESTADO ORDENES DE REPARACION",&tit_impre
     con_lin = 11
ENDIF
con_lin = con_lin + 1
@ con_lin, 90 SAY  ;
  'TOTAL ESTADO :' + wk_antiguo
@ con_lin, 111 SAY tot_mode  ;
  PICTURE '99,999,999'
con_lin = con_lin + 1
@ con_lin, 90 SAY  ;
  'TOTAL GENERAL....:'
@ con_lin, 111 SAY sum_gen  ;
  PICTURE '99,999,999'
EJECT
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
use &PAS407 EXCLUSIVE    
ZAP
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, titu2,  ;
          wk_fech, pie1, pie2
PRIVATE centro
centro = 0
pag = pag + 1
@ 1, 0 SAY empre1
@ 1, 110 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
centro = INT((132 - LEN(pie1)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 2, centro SAY pie1
@ 2, 110 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
centro = INT((132 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro SAY pie2
@ 3, 110 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 138)
@ 08, 0 SAY titu1
@ 09, 0 SAY titu2
@ 10, 0 SAY REPLICATE('=', 138)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
