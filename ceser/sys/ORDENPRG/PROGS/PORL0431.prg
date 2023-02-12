*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0427>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' REPARACION POR TIPO DE FALLA'
@ 3, 2 CLEAR TO 5, 77
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE st_estad INDEX st_1esta
CREATE DBF pas_arte.dbf  ;
       (pas_anorep N (2),  ;
       pas_mesrep N (2),  ;
       pas_codmod C (15),  ;
       pas_nommod C (30),  ;
       pas_codmar C (4),  ;
       pas_descri C (40),  ;
       pas_valmao N (9, 2),  ;
       pas_valrep N (9, 2))
SELECT 2
USE pas_arte.dbf
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
     tit_impre = 'REPARACION POR MODELO'
     tit_centr = '"ESTADISTICAS A¥O "+str(wk_anorep, 4)'
     efecin = 1
     SET CURSOR ON
     READ
     SET CURSOR OFF
     wk_year = INT(VAL(SUBSTR(STR(wk_anorep,  ;
               4), 3, 2)))
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
             '** No Se Registra Ninguna Falla **'
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
ERASE pas_arte.dbf
ERASE paso.idx
CLOSE DATABASES
RETURN
*
PROCEDURE vali
PRIVATE wk_anorep, wk_mesrep,  ;
        wk_codmod, wk_nommod,  ;
        wk_valmao, wk_valrep,  ;
        wk_codmar, wk_descri
STORE 0 TO wk_anorep, wk_mesrep,  ;
      wk_valmao, wk_valrep
wk_codmod = SPACE(15)
wk_nommod = SPACE(30)
wk_codmar = SPACE(04)
wk_descri = SPACE(40)
SELECT 3
USE st_itabl INDEX st_1tabl,  ;
    st_2tabl
SELECT 4
USE st_imode INDEX st_1mode,  ;
    st_2mode
SELECT 1
GOTO TOP
SEEK wk_year
IF FOUND()
     con = 0
     DO WHILE wk_year=anorep .OR.   ;
        .NOT. EOF()
          IF &pregunta
               con = con + 1
               vecto9( con) =  ;
                     numord
               wk_hay = .T.
          ENDIF
          SELECT 1
          SKIP
     ENDDO
     DO WHILE wk_year=anorep .OR.   ;
        .NOT. EOF()
          IF  &pregunta
               wk_anorep = anorep
               wk_mesrep = mesrep
               wk_codmod = codmod
               wk_codmar = codmar
               wk_valmao = valmao
               wk_valrep = valrep
               SELECT 4
               GOTO TOP
               codaux = wk_codmar +  ;
                        wk_codmod
               find '&codaux'
               wk_nommod = nommod
               SELECT 3
               GOTO TOP
               codaux = 'MARC' +  ;
                        wk_codmar
               find '&codaux'
               wk_descri = tab_descri
               SELECT 2
               APPEND BLANK
               REPLACE pas_anorep  ;
                       WITH  ;
                       wk_anorep
               REPLACE pas_mesrep  ;
                       WITH  ;
                       wk_mesrep
               REPLACE pas_codmod  ;
                       WITH  ;
                       wk_codmod
               REPLACE pas_nommod  ;
                       WITH  ;
                       wk_nommod
               REPLACE pas_codmar  ;
                       WITH  ;
                       wk_codmar
               REPLACE pas_descri  ;
                       WITH  ;
                       wk_descri
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
     INDEX ON pas_codmar +  ;
           pas_codmod TO  ;
           paso.idx
ENDIF
SELECT 3
USE
SELECT 4
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
        ant_codmod, ant_nommod,  ;
        ant_valmao, ant_valrep,  ;
        ant_codmar, ant_descri
STORE 0 TO ant_anorep, ant_mesrep,  ;
      ant_valmao, ant_valrep,  ;
      wk_totalme, pag
ant_codmod = SPACE(15)
ant_nommod = SPACE(30)
ant_codmar = SPACE(04)
ant_descri = SPACE(40)
tipo = SPACE(04)
DIMENSION totano( 12)
DIMENSION totmes( 12)
DO limpia WITH totano
DO limpia WITH totmes
tit1 = '      ENE.    FEB.    MAR.      ABR.      MAY.      JUN.      JUL.      AGO.      SEP.      OCT.      NOV.      DIC.          TOTAL '
con_lin = 09
wk_totprod = 1
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
     IF  .NOT. EMPTY(ant_codmar)
          IF pas_codmar <>  ;
             ant_codmar .OR.  ;
             pas_codmod <>  ;
             ant_codmod
               con_lin = con_lin +  ;
                         1
               @ con_lin, 1 SAY  ;
                 'MODELO ...:'
               @ con_lin, 15 SAY  ;
                 ant_codmod  ;
                 PICTURE '@!'
               @ con_lin, 32 SAY  ;
                 ant_nommod  ;
                 PICTURE '@!'
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
     IF pas_codmar <> ant_codmar
          IF  .NOT.  ;
              EMPTY(ant_codmar)
               con_lin = con_lin +  ;
                         1
               @ con_lin, 1 SAY  ;
                 'TOTAL POR MODELO ...:'
               con_lin = con_lin +  ;
                         1
               DO linea WITH  ;
                  totmes
               DO limpia WITH  ;
                  totmes
               con_lin = con_lin +  ;
                         1
               @ con_lin, 0 SAY  ;
                 REPLICATE('-',  ;
                 132)
               con_lin = con_lin +  ;
                         2
          ENDIF
          @ con_lin, 0 SAY  ;
            'MARCA ....:'
          @ con_lin, 14 SAY  ;
            pas_codmar + ' ' +  ;
            pas_descri
     ENDIF
     ant_anorep = pas_anorep
     ant_mesrep = pas_mesrep
     ant_codmod = pas_codmod
     ant_nommod = pas_nommod
     ant_codmar = pas_codmar
     ant_descri = pas_descri
     ant_valmao = pas_valmao
     ant_valrep = pas_valrep
     SKIP
ENDDO
con_lin = con_lin + 1
@ con_lin, 1 SAY 'MODELO ...:'
@ con_lin, 15 SAY ant_codmod  ;
  PICTURE '@!'
@ con_lin, 32 SAY ant_nommod  ;
  PICTURE '@!'
con_lin = con_lin + 1
totano( ant_mesrep) =  ;
      totano(ant_mesrep) +  ;
      (ant_valmao + ant_valrep)
DO linea WITH totano
DO acumm WITH totano
DO limpia WITH totano
con_lin = con_lin + 1
@ con_lin, 1 SAY  ;
  'TOTAL POR MODELO ...:'
con_lin = con_lin + 1
DO linea WITH totmes
DO limpia WITH totmes
con_lin = con_lin + 1
@ con_lin, 0 SAY REPLICATE('-',  ;
  132)
con_lin = con_lin + 2
IF con_lin = 50
     EJECT
     DO PIE WITH PAG,tit1, &tit_centr,;
tit_impre
     con_lin = 09
ENDIF
EJECT
?? CHR(18)
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
USE pas_arte
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
