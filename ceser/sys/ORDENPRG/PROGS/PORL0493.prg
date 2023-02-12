*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0493>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INFORME ORDENES PENDIENTES'
tipri = ' INFORME ORDENES PENDIENTES'
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_IOREP ORDER CODIGO
pas493 = da_nombre()
Create table &PAS493 (pas_marca C(4),pas_desmar;
C(25),pas_numord C(8),pas_fecemi D(8),pas_feccom;
d(8),pas_mode C(15),pas_emi C(4),pas_tec;
c(9),pas_serie c(20),pas_auxest c(4),pas_dauxest;
c(25) )
SELECT 2
use &PAS493 EXCLUSIVE      
DO WHILE ppas
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 5, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_fec = CTOD('  /  /  ')
     wk_hay = .F.
     STORE SPACE(35) TO pregunta,  ;
           tit_impre
     STORE 0 TO wk_mes, wk_year
     efecin = 1
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     @ 4, 5 SAY  ;
       'Fecha Hasta :                   '
     @ 4, 20 GET wk_fec PICTURE  ;
       '!!/!!/!!' VALID fecha()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     pregunta = 'fecemi<=wk_fec.and.indest#"N".and.indest#"F".and.indest#"C"'
     tit_impre = '" HASTA "+dtoc(wk_fec)'
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     DO vali_exist
     IF wk_hay = .F.
          DO error WITH  ;
             '** No Se Registran Ordenes Pendientes **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          LOOP
     ENDIF
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
     IF key == -6
          DO lis_promed
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
x493 = pas493 + '.DBF'
Erase &x493          
x493 = pas493 + '.IDX'
Erase &x493          
CLOSE DATABASES
RETURN
*
PROCEDURE vali_exist
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_marca = SPACE(4)
wk_mode = SPACE(15)
wk_fecemi = CTOD('  /  /  ')
SELECT 5
USE SHARED st_isrep ORDER CODIGO
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta
          wk_marca = codmar
          wk_mode = codmod
          wk_numord = numdoc
          wk_fecemi = fecemi
          wk_mode = codmod
          wk_emi = codemi
          wk_feccom = CTOD( ;
                      '  /  /  ' ;
                      )
          wk_tec = codtec
          wk_serie = numser
          wk_auxest = auxest
          SELECT 5
          GOTO TOP
          SEEK wk_numord
          IF FOUND()
               wk_feccom = feccom
          ENDIF
          SELECT 3
          GOTO TOP
          codaux = 'ESOR' +  ;
                   wk_auxest
          SEEK '&codaux'
          wk_dauxest = tab_destab
          SELECT 3
          GOTO TOP
          codaux = 'MARC' +  ;
                   wk_marca
          SEEK '&codaux'
          wk_desmarc = tab_destab
          SELECT 2
          APPEND BLANK
          REPLACE pas_marca WITH  ;
                  wk_marca
          REPLACE pas_desmar WITH  ;
                  wk_desmarc
          REPLACE pas_mode WITH  ;
                  wk_mode
          REPLACE pas_numord WITH  ;
                  wk_numord
          REPLACE pas_fecemi WITH  ;
                  wk_fecemi
          REPLACE pas_feccom WITH  ;
                  wk_feccom
          REPLACE pas_emi WITH  ;
                  wk_emi
          REPLACE pas_tec WITH  ;
                  wk_tec
          REPLACE pas_serie WITH  ;
                  wk_serie
          REPLACE pas_auxest WITH  ;
                  wk_auxest
          REPLACE pas_dauxes WITH  ;
                  wk_dauxest
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_marca+pas_auxest to;
&PAS493    
ENDIF
SELECT 3
USE
RETURN
*
PROCEDURE lis_promed
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
tit1 = 'No.ORDEN FECHA DE  FECHA   EMISOR  TECNICO     MODELO                SERIE      '
tit2 = '         EMISION   COMPR.                    DESCRIPCION'
con_lin = 12
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
DO PIE WITH PAG,tit1,TIT2,"&TIPRI",&tit_impre
SELECT 2
SET ORDER TO 1
GOTO TOP
DO WHILE  .NOT. EOF()
     wk_marca = pas_marca
     wk_desmarc = pas_desmar
     @ con_lin, 3 SAY 'MARCA...:' +  ;
       wk_marca + '  ' +  ;
       wk_desmarc
     con_lin = con_lin + 1
     DO WHILE wk_marca=pas_marca
          wk_auxest = pas_auxest
          wk_dauxest = pas_dauxes
          @ con_lin, 5 SAY  ;
            'ESTADO...:' +  ;
            wk_auxest + '  ' +  ;
            wk_dauxest
          con_lin = con_lin + 1
          DO WHILE pas_auxest= ;
             wk_auxest
               @ con_lin, 0 SAY  ;
                 pas_numord
               @ con_lin, 9 SAY  ;
                 pas_fecemi
               @ con_lin, 19 SAY  ;
                 pas_feccom
               @ con_lin, 28 SAY  ;
                 pas_emi
               @ con_lin, 32 SAY  ;
                 pas_tec
               @ con_lin, 43 SAY  ;
                 pas_mode
               @ con_lin, 59 SAY  ;
                 pas_serie
               IF con_lin = 50
                    EJECT
                    do pie with pag,tit1,TIT2,"&TIPRI",&tit_impre
                    con_lin = 12
               ENDIF
               SKIP
               con_lin = con_lin +  ;
                         1
          ENDDO
     ENDDO
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
PARAMETER pag, titu1, titu2, pie1,  ;
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
@ 3, centro SAY pie2
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY titu2
@ 10, 0 SAY REPLICATE('=', 80)
RETURN
*
PROCEDURE fecha
IF EMPTY(wk_fec)
     wk_fec = DATE()
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
