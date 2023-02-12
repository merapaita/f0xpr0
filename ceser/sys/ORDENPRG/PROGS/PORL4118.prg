*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4118>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INFORME COSTO FACTURACION '
@ 3, 2 CLEAR TO 5, 77
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_estad ORDER CODIGO
pas4118 = da_nombre()
Create table &PAS4118 ( pas_anorep N(2),;
pas_mesrep N(2), pas_codmod C(15), pas_nommod;
C(30),  pas_codmar C(4), pas_descri C(40),;
pas_valmao N(9), pas_valrep N(9))
SELECT 2
use &PAS4118 EXCLUSIVE   
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
     tit_impre = 'INFORME COSTO FACTURACION'
     tit_centr = '"ESTADISTICAS A¥O "+str(wk_anorep, 4)'
     efecin = 1
     SET CURSOR ON
     READ
     SET CURSOR OFF
     wk_year = INT(VAL(SUBSTR(STR(wk_anorep,  ;
               4), 3, 2)))
     pregunta = 'wk_year = anorep'
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
             '** No Se Registra Ninguna Salida De Articulo **'
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
x4118 = pas4118 + '.DBF'
ERASE &X4118
x4118 = pas4118 + '.IDX'
ERASE &X4118
RELEASE tot_mes_r, tot_mes_m
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
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_imode ORDER CODIGO
SELECT 1
GOTO TOP
SEEK wk_year
IF FOUND()
     DO WHILE (wk_year=anorep)  ;
        .OR.  .NOT. EOF()
          IF &pregunta
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
               SEEK '&codaux'
               wk_nommod = nommod
               SELECT 3
               GOTO TOP
               codaux = 'MARC' +  ;
                        wk_codmar
               SEEK '&codaux'
               wk_descri = tab_destab
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
     Index on pas_codmar+pas_codmod to;
&PAS4118   
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
pag = 0
DIMENSION tot_mes_r( 12)
DIMENSION tot_mes_m( 12)
DIMENSION tot_mrm( 12)
FOR i = 1 TO 12
     tot_mes_m( i) = 0
     tot_mes_r( i) = 0
     tot_mrm( i) = 0
ENDFOR
acum_r = 0
acum_o = 0
tit1 = '      ENE.    FEB.    MAR.      ABR.      MAY.      JUN.      JUL.      AGO.      SEP.      OCT.      NOV.      DIC.          TOTAL '
con_lin = 09
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
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
     wk_marca = pas_codmar
     wk_descri = pas_descri
     @ con_lin, 2 SAY  ;
       'MARCA  ..:' + wk_marca +  ;
       ' ' + wk_descri
     con_lin = con_lin + 1
     DO WHILE pas_codmar=wk_marca
          wk_codmod = pas_codmod
          wk_nommod = pas_nommod
          @ con_lin, 6 SAY  ;
            'MODELO ..:' +  ;
            wk_codmod + '  ' +  ;
            wk_nommod
          con_lin = con_lin + 1
          DO WHILE pas_codmod= ;
             wk_codmod .AND.  ;
             wk_marca=pas_codmar
               IF pas_mesrep <> 0
                    tot_mes_m(  ;
                             pas_mesrep) =  ;
                             pas_valmao +  ;
                             tot_mes_m(pas_mesrep)
                    tot_mes_r(  ;
                             pas_mesrep) =  ;
                             pas_valrep +  ;
                             tot_mes_r(pas_mesrep)
                    tot_mrm(  ;
                           pas_mesrep) =  ;
                           tot_mes_m(pas_mesrep) +  ;
                           tot_mes_r(pas_mesrep)
               ENDIF
               SKIP
          ENDDO
          FOR i = 1 TO 12
               acum_r = acum_r +  ;
                        tot_mes_r(i)
               acum_o = acum_o +  ;
                        tot_mes_m(i)
          ENDFOR
          @ con_lin, 1 SAY  ;
            'COSTO EN REPUESTO:'
          con_lin = con_lin + 1
          @ con_lin, 1 SAY  ;
            tot_mes_r(1) PICTURE  ;
            '999999999'
          @ con_lin, 10 SAY  ;
            tot_mes_r(2) PICTURE  ;
            '999999999'
          @ con_lin, 20 SAY  ;
            tot_mes_r(3) PICTURE  ;
            '999999999'
          @ con_lin, 30 SAY  ;
            tot_mes_r(4) PICTURE  ;
            '999999999'
          @ con_lin, 40 SAY  ;
            tot_mes_r(5) PICTURE  ;
            '999999999'
          @ con_lin, 50 SAY  ;
            tot_mes_r(6) PICTURE  ;
            '999999999'
          @ con_lin, 60 SAY  ;
            tot_mes_r(7) PICTURE  ;
            '999999999'
          @ con_lin, 70 SAY  ;
            tot_mes_r(8) PICTURE  ;
            '999999999'
          @ con_lin, 80 SAY  ;
            tot_mes_r(9) PICTURE  ;
            '999999999'
          @ con_lin, 90 SAY  ;
            tot_mes_r(10) PICTURE  ;
            '999999999'
          @ con_lin, 100 SAY  ;
            tot_mes_r(11) PICTURE  ;
            '999999999'
          @ con_lin, 110 SAY  ;
            tot_mes_r(12) PICTURE  ;
            '999999999'
          @ con_lin, 120 SAY  ;
            acum_r PICTURE  ;
            '999999999'
          con_lin = con_lin + 1
          @ con_lin, 1 SAY  ;
            'COSTO MANO DE OBRA:'
          con_lin = con_lin + 1
          @ con_lin, 1 SAY  ;
            tot_mes_m(1) PICTURE  ;
            '999999999'
          @ con_lin, 10 SAY  ;
            tot_mes_m(2) PICTURE  ;
            '999999999'
          @ con_lin, 20 SAY  ;
            tot_mes_m(3) PICTURE  ;
            '999999999'
          @ con_lin, 30 SAY  ;
            tot_mes_m(4) PICTURE  ;
            '999999999'
          @ con_lin, 40 SAY  ;
            tot_mes_m(5) PICTURE  ;
            '999999999'
          @ con_lin, 50 SAY  ;
            tot_mes_m(6) PICTURE  ;
            '999999999'
          @ con_lin, 60 SAY  ;
            tot_mes_m(7) PICTURE  ;
            '999999999'
          @ con_lin, 70 SAY  ;
            tot_mes_m(8) PICTURE  ;
            '999999999'
          @ con_lin, 80 SAY  ;
            tot_mes_m(9) PICTURE  ;
            '999999999'
          @ con_lin, 90 SAY  ;
            tot_mes_m(10) PICTURE  ;
            '999999999'
          @ con_lin, 100 SAY  ;
            tot_mes_m(11) PICTURE  ;
            '999999999'
          @ con_lin, 110 SAY  ;
            tot_mes_m(12) PICTURE  ;
            '999999999'
          @ con_lin, 120 SAY  ;
            acum_o PICTURE  ;
            '999999999'
          con_lin = con_lin + 2
          @ con_lin, 1 SAY  ;
            'TOTAL GENERAL :'
          con_lin = con_lin + 1
          @ con_lin, 1 SAY  ;
            tot_mrm(1) PICTURE  ;
            '999999999'
          @ con_lin, 10 SAY  ;
            tot_mrm(2) PICTURE  ;
            '999999999'
          @ con_lin, 20 SAY  ;
            tot_mrm(3) PICTURE  ;
            '999999999'
          @ con_lin, 30 SAY  ;
            tot_mrm(4) PICTURE  ;
            '999999999'
          @ con_lin, 40 SAY  ;
            tot_mrm(5) PICTURE  ;
            '999999999'
          @ con_lin, 50 SAY  ;
            tot_mrm(6) PICTURE  ;
            '999999999'
          @ con_lin, 60 SAY  ;
            tot_mrm(7) PICTURE  ;
            '999999999'
          @ con_lin, 70 SAY  ;
            tot_mrm(8) PICTURE  ;
            '999999999'
          @ con_lin, 80 SAY  ;
            tot_mrm(9) PICTURE  ;
            '999999999'
          @ con_lin, 90 SAY  ;
            tot_mrm(10) PICTURE  ;
            '999999999'
          @ con_lin, 100 SAY  ;
            tot_mrm(11) PICTURE  ;
            '999999999'
          @ con_lin, 110 SAY  ;
            tot_mrm(12) PICTURE  ;
            '999999999'
          FOR i = 1 TO 12
               tot_mes_r( i) = 0
               tot_mes_m( i) = 0
          ENDFOR
          acum_r = 0
          acum_o = 0
          con_lin = con_lin + 1
          @ con_lin, 0 SAY  ;
            REPLICATE('-', 132)
          con_lin = con_lin + 1
     ENDDO
     IF con_lin = 50
          EJECT
          DO PIE WITH PAG,tit1, &tit_centr,;
tit_impre
          con_lin = 09
     ENDIF
ENDDO
EJECT
?? CHR(15)
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
use &PAS4118 EXCLUSIVE   
ZAP
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
