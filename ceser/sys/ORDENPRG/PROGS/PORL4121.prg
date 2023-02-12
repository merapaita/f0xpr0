*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4121>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' FRECUENCIA DE ATENCION PUBLICO'
@ 3, 2 CLEAR TO 5, 77
@ 3, 2 TO 5, 77
@ 4, 5 SAY 'Fecha de Proceso  :'
ppas = .T.
SELECT 1
USE SHARED st_isrep ORDER CODIGO
pas4121 = da_nombre()
Create table &PAS4121 ( pas_marca C(4),;
pas_descri C(35), pas_fecemi D, pas_hora;
C(8))
SELECT 2
use &PAS4121 EXCLUSIVE   
DO WHILE ppas
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 5, 77
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     wk_feservi = CTOD( ;
                  '  /  /  ')
     wk_hay = .F.
     efecin = 1
     @ 4, 25 GET wk_feservi  ;
       PICTURE '!!/!!/!!' VALID  ;
       fecha()
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
             '** No Se Registra Atencion de Publico **'
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
          DO lis_soli
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
     DO CASE
          CASE key == -3
               DO esc_modo WITH  ;
                  'E'
     ENDCASE
     DO esc_modo WITH 'C'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     keyx = LASTKEY()
     DO WHILE keyx<>27 .AND. keyx<>- ;
        9
          keyx = INKEY(0)
     ENDDO
     IF keyx == -9
          LOOP
     ENDIF
     IF keyx == 27
          IF efecin = 1
               ppas = .F.
               LOOP
          ELSE
               LOOP
          ENDIF
     ENDIF
ENDDO
DO saca_win
@ 24, 69 SAY SPACE(10)
ON KEY LABEL F6
ON KEY LABEL F10
SELECT 2
USE
x4121 = pas4121 + '.DBF'
ERASE &X4121
x4121 = pas4121 + '.IDX'
ERASE &X4121
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
PROCEDURE vali
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_marca = SPACE(4)
wk_fecemi = CTOD('  /  /  ')
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF indest <> 'N' .AND.  ;
        fecemi = wk_feservi
          wk_marca = codmar
          wk_fecemi = fecemi
          wk_hora = horemi
          SELECT 3
          codaux = 'MARC' +  ;
                   wk_marca
          SEEK '&codaux'
          wk_descri = tab_destab
          SELECT 2
          APPEND BLANK
          REPLACE pas_marca WITH  ;
                  wk_marca
          REPLACE pas_fecemi WITH  ;
                  wk_fecemi
          REPLACE pas_hora WITH  ;
                  wk_hora
          REPLACE pas_descri WITH  ;
                  wk_descri
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_marca+pas_hora to &PAS4121;
   
     RETURN
ENDIF
SELECT 3
USE
RETURN
*
PROCEDURE lis_soli
tit1 = '                                   HORA                     C A N T I D A D   '
tit2 = '                                                                  D E         '
tit3 = '                                                         ORDENES DE REPARACION'
con_lin = 11
STORE 0 TO wk_tothora, general,  ;
      pag, totmar, imp_hora
STORE 0 TO acum8, acum9, acum10,  ;
      acum11, acum12, acum14,  ;
      acum15, acum16, acum17
wk_viejora = 8
wk_antiguo = SPACE(4)
STORE SPACE(15) TO wk_anti,  ;
      wk_antiguo
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
SELECT 2
SET ORDER TO 1
GOTO TOP
DO pie WITH pag, tit1, tit2, tit3,  ;
   wk_feservi,  ;
   'FRECUENCIA DE ATENCION AL PUBLICO',  ;
   'CORRESPONDIENTE AL DIA '
DO WHILE  .NOT. EOF()
     IF con_lin = 40
          EJECT
          DO pie WITH pag, tit1,  ;
             tit2, tit3,  ;
             wk_feservi,  ;
             'FRECUENCIA DE ATENCION AL PUBLICO',  ;
             'CORRESPONDIENTE AL DIA '
          con_lin = 11
     ENDIF
     totmar = totmar + wk_tothora
     IF pas_marca <> wk_antiguo
          IF  .NOT.  ;
              EMPTY(wk_antiguo)
               @ con_lin, 24 SAY  ;
                 '08 a 09 am'
               @ con_lin, 67 SAY  ;
                 acum8
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '09 a 10 am'
               @ con_lin, 67 SAY  ;
                 acum9
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '10 a 11 am'
               @ con_lin, 67 SAY  ;
                 acum10
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '11 a 12 am'
               @ con_lin, 67 SAY  ;
                 acum11
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '12 a 01 pm'
               @ con_lin, 67 SAY  ;
                 acum12
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '02 a 03 pm'
               @ con_lin, 67 SAY  ;
                 acum14
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '03 a 04 pm'
               @ con_lin, 67 SAY  ;
                 acum15
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '04 a 05 pm'
               @ con_lin, 67 SAY  ;
                 acum16
               con_lin = con_lin +  ;
                         1
               @ con_lin, 24 SAY  ;
                 '05 a 06 pm'
               @ con_lin, 67 SAY  ;
                 acum17
               con_lin = con_lin +  ;
                         1
               totmar = acum8 +  ;
                        acum9 +  ;
                        acum10 +  ;
                        acum11 +  ;
                        acum12 +  ;
                        acum14 +  ;
                        acum15 +  ;
                        acum16 +  ;
                        acum17
               con_lin = con_lin +  ;
                         1
               @ con_lin, 67 SAY  ;
                 '-------------'
               con_lin = con_lin +  ;
                         1
               @ con_lin, 20 SAY  ;
                 'TOTAL MARCA :' +  ;
                 wk_antiguo
               @ con_lin, 67 SAY  ;
                 totmar PICTURE  ;
                 '99,999,999'
               general = general +  ;
                         totmar
               wk_viejora = 8
               STORE 0 TO totmar
               con_lin = con_lin +  ;
                         1
          ENDIF
          con_lin = con_lin + 1
          @ con_lin, 00 SAY  ;
            'MARCA..:' +  ;
            pas_marca + '  ' +  ;
            pas_descri
          con_lin = con_lin + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 8.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 9.00 
          acum8 = acum8 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 9.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 10.00 
          acum9 = acum9 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 10.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 11.00 
          acum10 = acum10 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 11.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 12.00 
          acum11 = acum11 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 12.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 13.00 
          acum12 = acum12 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 14.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 15.00 
          acum14 = acum14 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 15.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 16.00 
          acum15 = acum15 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 16.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) < 17.00 
          acum16 = acum16 + 1
     ENDIF
     IF VAL(SUBSTR(pas_hora, 1,  ;
        5)) >= 17.00  .AND.  ;
        VAL(SUBSTR(pas_hora, 1,  ;
        5)) <= 18.00 
          acum16 = acum17 + 1
     ENDIF
     wk_antiguo = pas_marca
     SKIP
ENDDO
IF pas_marca <> wk_antiguo
     @ con_lin, 24 SAY  ;
       '08 a 09 am'
     @ con_lin, 67 SAY acum8
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '09 a 10 am'
     @ con_lin, 67 SAY acum9
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '10 a 11 am'
     @ con_lin, 67 SAY acum10
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '11 a 12 am'
     @ con_lin, 67 SAY acum11
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '12 a 01 pm'
     @ con_lin, 67 SAY acum12
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '02 a 03 pm'
     @ con_lin, 67 SAY acum14
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '03 a 04 pm'
     @ con_lin, 67 SAY acum15
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '04 a 05 pm'
     @ con_lin, 67 SAY acum16
     con_lin = con_lin + 1
     @ con_lin, 24 SAY  ;
       '05 a 06 pm'
     @ con_lin, 67 SAY acum17
     con_lin = con_lin + 1
     totmar = acum8 + acum9 +  ;
              acum10 + acum11 +  ;
              acum12 + acum14 +  ;
              acum15 + acum16 +  ;
              acum17
     @ con_lin, 67 SAY  ;
       '-------------'
     con_lin = con_lin + 1
     @ con_lin, 20 SAY  ;
       'TOTAL MARCA :' +  ;
       wk_antiguo
     @ con_lin, 67 SAY totmar  ;
       PICTURE '99,999,999'
     general = general + totmar
     STORE 0 TO wk_fornet,  ;
           wk_foriva, wk_fortot
     con_lin = con_lin + 1
ENDIF
IF con_lin = 40
     EJECT
     DO pie WITH pag, tit1, tit2,  ;
        tit3, wk_feservi,  ;
        'FRECUENCIA DE ATENCION AL PUBLICO',  ;
        'CORRESPONDIENTE AL DIA '
     con_lin = 11
ENDIF
con_lin = con_lin + 2
@ con_lin, 20 SAY  ;
  'T O T A L  G E N E R A L =============>'
@ con_lin, 67 SAY general PICTURE  ;
  '99,999,999'
EJECT
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
use &PAS4121  EXCLUSIVE  
ZAP
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, titu2,  ;
          titu3, wk_fech, pie1,  ;
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
@ 2, centro SAY pie1
@ 2, 60 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
centro = INT((80 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro - 4 SAY pie2 +  ;
  DTOC(wk_fech)
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY titu2
@ 10, 0 SAY titu3
@ 11, 0 SAY REPLICATE('=', 80)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
