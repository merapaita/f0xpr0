*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4114>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PRODUCCION POR TECNICO '
@ 3, 2 CLEAR TO 5, 77
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_estad ORDER CODIGO
pas4114 = da_nombre()
Create table &PAS4114 ( pas_anorep N(2),;
pas_mesrep N(2), pas_codtec C(9), pas_noment;
C(30), pas_valmao N(9,2), pas_valrep N(9,2))
SELECT 2
use &PAS4114 EXCLUSIVE 
DO WHILE ppas
     @ 4, 30 SAY SPACE(30)
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_anorep = 0
     @ 4, 5 SAY  ;
       '  A¤o de Proceso  :                          '
     @ 4, 25 GET wk_anorep  ;
       PICTURE '9999' VALID a¥()
     efecin = 1
     SET CURSOR ON
     READ
     SET CURSOR OFF
     wk_year = INT(VAL(SUBSTR(STR(wk_anorep,  ;
               4), 3, 2)))
     tit_impre = 'REPARACION POR TECNICO'
     tit_centr = '"ESTADISTICAS A¥O "+str(wk_anorep, 4)'
     pregunta = 'wk_year = anorep '
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     wk_hay = .F.
     DO vali
     IF wk_hay = .F.
          DO error WITH  ;
             '** No Se Registran Producciones por Tecnico **'
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
SELECT 1
USE
SELECT 2
USE
x4114 = pas4114 + '.DBF'
ERASE &X4114
x4114 = pas4114 + '.IDX'
ERASE &X4114
CLOSE DATABASES
RETURN
*
PROCEDURE vali
PRIVATE wk_anorep, wk_mesrep,  ;
        wk_codtec, wk_noment,  ;
        wk_valmao, wk_valrep
STORE 0 TO wk_anorep, wk_mesrep,  ;
      wk_valmao, wk_valrep
wk_codtec = SPACE(09)
wk_noment = SPACE(30)
SELECT 3
USE SHARED st_itecn ORDER CODIGO
SELECT 1
GOTO TOP
SEEK wk_year
IF FOUND()
     DO WHILE wk_year=anorep .OR.   ;
        .NOT. EOF()
          IF &pregunta        ;
             
               wk_anorep = anorep
               wk_mesrep = mesrep
               wk_codtec = codtec
               wk_valmao = valmao
               wk_valrep = valrep
               SELECT 3
               GOTO TOP
               SEEK wk_codtec
               IF FOUND()
                    wk_noment = noment
               ENDIF
               SELECT 2
               APPEND BLANK
               REPLACE pas_anorep  ;
                       WITH  ;
                       wk_anorep
               REPLACE pas_mesrep  ;
                       WITH  ;
                       wk_mesrep
               REPLACE pas_codtec  ;
                       WITH  ;
                       wk_codtec
               REPLACE pas_noment  ;
                       WITH  ;
                       wk_noment
               REPLACE pas_valmao  ;
                       WITH  ;
                       wk_valmao
               REPLACE pas_valrep  ;
                       WITH  ;
                       wk_valrep
               wk_hay = .T.
          ENDIF
          SELECT 1
          SKIP
     ENDDO
ENDIF
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_codtec to &PAS4114;
  
ENDIF
SELECT 3
USE
RETURN
*
FUNCTION a¥
IF LASTKEY() = 5
     RETURN .T.
ENDIF
IF EMPTY(wk_anorep)
     wk_anorep = YEAR(DATE())
ELSE
     IF wk_anorep < 1
          RETURN .F.
     ENDIF
ENDIF
RETURN
*
PROCEDURE lis_arti4
PRIVATE ant_anorep, ant_mesrep,  ;
        ant_codtec, ant_noment,  ;
        ant_valmao, ant_valrep
STORE 0 TO ant_anorep, ant_mesrep,  ;
      ant_valmao, ant_valrep,  ;
      pag
ant_codtec = SPACE(09)
ant_desent = SPACE(30)
DIMENSION totano( 12)
DIMENSION totmes( 12)
DO limpia WITH totano
DO limpia WITH totmes
tit1 = '      ENE.    FEB.    MAR.      ABR.      MAY.      JUN.      JUL.      AGO.      SEP.      OCT.      NOV.      DIC.          TOTAL '
con_lin = 09
wk_viejora = 8
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RELEASE totano
     RELEASE totmes
     RETURN
ENDIF
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
?? CHR(15)
SELECT 2
SET ORDER TO 1
GOTO TOP
DO PIE WITH PAG, tit1,&tit_centr,tit_impre
DO WHILE  .NOT. EOF()
     IF con_lin = 50
          EJECT
          DO PIE WITH PAG,tit1, &tit_centr,;
tit_impre
          con_lin = 09
     ENDIF
     IF  .NOT. EMPTY(ant_codtec)
          IF pas_codtec <>  ;
             ant_codtec
               con_lin = con_lin +  ;
                         1
               totano(  ;
                     ant_mesrep) =  ;
                     totano(ant_mesrep) +  ;
                     (ant_valmao +  ;
                     ant_valrep)
               DO linea WITH  ;
                  totano
               DO acumm WITH  ;
                  totano
               DO limpia WITH  ;
                  totano
          ELSE
               totano(  ;
                     ant_mesrep) =  ;
                     totano(ant_mesrep) +  ;
                     (ant_valmao +  ;
                     ant_valrep)
          ENDIF
     ENDIF
     IF pas_codtec <> ant_codtec
          IF  .NOT.  ;
              EMPTY(ant_codtec)
               con_lin = con_lin +  ;
                         1
               @ con_lin, 0 SAY  ;
                 REPLICATE('-',  ;
                 132)
               con_lin = con_lin +  ;
                         1
          ENDIF
          @ con_lin, 0 SAY  ;
            'TECNICO...:'
          @ con_lin, 14 SAY  ;
            pas_codtec + ' ' +  ;
            pas_noment
     ENDIF
     ant_anorep = pas_anorep
     ant_mesrep = pas_mesrep
     ant_codtec = pas_codtec
     ant_noment = pas_noment
     ant_valmao = pas_valmao
     ant_valrep = pas_valrep
     SKIP
ENDDO
con_lin = con_lin + 1
totano( ant_mesrep) =  ;
      totano(ant_mesrep) +  ;
      (ant_valmao + ant_valrep)
DO linea WITH totano
DO acumm WITH totano
DO limpia WITH totano
con_lin = con_lin + 1
@ con_lin, 0 SAY REPLICATE('-',  ;
  132)
con_lin = con_lin + 1
@ con_lin, 1 SAY  ;
  'TOTAL POR TECNICO...:'
con_lin = con_lin + 1
DO linea WITH totmes
DO limpia WITH totmes
con_lin = con_lin + 2
IF con_lin = 50
     EJECT
     DO PIE WITH PAG,tit1, &tit_centr,;
tit_impre
     con_lin = 09
ENDIF
EJECT
?? CHR(18)
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
use &PAS4114 EXCLUSIVE 
ZAP
RELEASE totano
RELEASE totmes
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, pie1, pie2
PRIVATE centro
centro = 0
pag = pag + 1
@ 1, 0 SAY empre1
@ 1, 112 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
centro = INT((132 - LEN(pie1)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 2, centro SAY pie1
@ 2, 112 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
centro = INT((132 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro SAY pie2
@ 3, 112 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 06, 0 SAY REPLICATE('=', 132)
@ 07, 0 SAY titu1
@ 08, 0 SAY REPLICATE('=', 132)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
