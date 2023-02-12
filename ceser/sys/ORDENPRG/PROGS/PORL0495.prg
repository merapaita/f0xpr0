*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0495>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INFORME DE GASTOS'
tipri = ' INFORME DE GASTOS'
@ 3, 2 TO 5, 77
ppas = .T.
pas495 = da_nombre()
Create table &pas495 (pas_numsol c(8),pas_fecemi;
D(8), pas_feccom d(8),pas_fecrep d(8),;
pas_pnume c(8),pas_pmano n(9),pas_prepu;
N(9), pas_onume c(8),pas_omano n(9),pas_orepu;
N(9), pas_fnume c(8),pas_fmano n(9),pas_frepu;
N(9), pas_mode C(15),pas_desmod c(20))
USE
SELECT 2
use &pas495 exclusive    
DO WHILE ppas
     SELECT 2
     ZAP
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 5, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_fec1 = CTOD('  /  /  ')
     wk_fec2 = CTOD('  /  /  ')
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
       'Fecha Desde :                Fecha Hasta :   '
     @ 4, 20 GET wk_fec1 PICTURE  ;
       '!!/!!/!!' VALID fecha1()
     @ 4, 46 GET wk_fec2 PICTURE  ;
       '!!/!!/!!' VALID fecha2()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     pregunta = '(fecfin>=wk_fec1.and.fecfin<=wk_fec2)'
     tit_impre = '"DESDE :"+dtoc(wk_fec1)+"  "+"  HASTA "+dtoc(wk_fec2)'
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
             '** No Se Registra Ningun Gasto **'
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
x495 = pas495 + '.DBF'
erase &x495
x495 = pas495 + '.IDX'
erase &x495
CLOSE DATABASES
RETURN
*
PROCEDURE vali_exist
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_marca = SPACE(4)
wk_mode = SPACE(15)
wk_fecemi = CTOD('  /  /  ')
SELECT 6
USE SHARED st_ispre ORDER CODIGO
SELECT 7
USE SHARED st_hfact ORDER CODIGO
SELECT 1
USE SHARED st_IOREP ORDER CODIGO
SELECT 5
USE SHARED st_isrep ORDER CODIGO
SELECT 4
USE SHARED st_imode ORDER CODIGO
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF TRIM(indest) = 'C' .OR.  ;
        TRIM(indest) = 'F'
          IF &pregunta
               STORE CTOD( ;
                     '  /  /  ')  ;
                     TO wk_fecrep,  ;
                     wk_fecemi,  ;
                     wk_feccom
               STORE ' ' TO  ;
                     wk_numord,  ;
                     wk_numpre,  ;
                     wk_numfac,  ;
                     wk_numsol,  ;
                     wk_mode,  ;
                     wk_nommod
               STORE 0 TO  ;
                     wk_omano,  ;
                     wk_orepu,  ;
                     wk_otota,  ;
                     wk_pmano,  ;
                     wk_prepu,  ;
                     wk_fmano,  ;
                     wk_frepu
               wk_marca = codmar
               wk_numsol = numsol
               wk_numpre = numpre
               wk_numfac = numfabo
               wk_numord = numdoc
               wk_fecemi = fecemi
               wk_feccom = CTOD( ;
                           '  /  /  ' ;
                           )
               wk_fecrep = fecfin
               wk_omano = cosmob
               wk_orepu = cosrep
               wk_mode = codmod
               SELECT 6
               SET ORDER TO 1
               GOTO TOP
               SEEK wk_numpre
               IF FOUND()
                    wk_pmano = monman
                    wk_prepu = monrep
               ENDIF
               SELECT 7
               SET ORDER TO 1
               GOTO TOP
               SEEK wk_numfac
               IF FOUND()
                    wk_fmano = cosmob
                    wk_frepu = cosrep
               ENDIF
               SELECT 4
               SET ORDER TO 1
               GOTO TOP
               codaux = wk_marca +  ;
                        wk_mode
               SEEK '&codaux'
               wk_nommod = nommod
               SELECT 5
               GOTO TOP
               SEEK wk_numord
               IF FOUND()
                    wk_feccom = feccom
               ENDIF
               SELECT 2
               APPEND BLANK
               REPLACE pas_mode  ;
                       WITH  ;
                       wk_mode
               REPLACE pas_desmod  ;
                       WITH  ;
                       wk_nommod
               REPLACE pas_fecrep  ;
                       WITH  ;
                       wk_fecrep
               REPLACE pas_onume  ;
                       WITH  ;
                       wk_numord
               REPLACE pas_omano  ;
                       WITH  ;
                       wk_omano
               REPLACE pas_orepu  ;
                       WITH  ;
                       wk_orepu
               REPLACE pas_pnume  ;
                       WITH  ;
                       wk_numpre
               REPLACE pas_pmano  ;
                       WITH  ;
                       wk_pmano
               REPLACE pas_prepu  ;
                       WITH  ;
                       wk_prepu
               REPLACE pas_fnume  ;
                       WITH  ;
                       wk_numfac
               REPLACE pas_fmano  ;
                       WITH  ;
                       wk_fmano
               REPLACE pas_frepu  ;
                       WITH  ;
                       wk_frepu
               REPLACE pas_fecemi  ;
                       WITH  ;
                       wk_fecemi
               REPLACE pas_feccom  ;
                       WITH  ;
                       wk_feccom
               REPLACE pas_numsol  ;
                       WITH  ;
                       wk_numsol
               wk_hay = .T.
          ENDIF
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_mode to &PAS495;
    
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
tit1 = 'No.SOLIC.  FECHA    FECHA    FECHA           P R E S U P U E S T O       O R.  R E P A R A C I O N            F A C T U  R A        '
tit2 = '           EMISION  COMPROM. REPARAC.     NUMERO    M. OBRA    REPTO.    NUMERO    M. OBRA    REPTO.    NUMERO    M. OBRA    REPTO. '
con_lin = 12
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(15)
DO PIE WITH PAG,tit1,TIT2,"&TIPRI",&tit_impre
SELECT 2
SET ORDER TO 1
GOTO TOP
STORE 0 TO acumg_oman, acumg_orep,  ;
      acumg_fman, acumg_frep,  ;
      acumg_pman, acumg_prep,  ;
      cont_gen
DO WHILE  .NOT. EOF()
     wk_mode = pas_mode
     wk_desmod = pas_desmod
     con_lin = con_lin + 1
     @ con_lin, 3 SAY  ;
       'MODELO ..:' + wk_mode +  ;
       '  ' + wk_desmod
     con_lin = con_lin + 1
     STORE 0 TO acum_fmano,  ;
           acum_frepu, cont_fmano,  ;
           cont_frepu
     STORE 0 TO acum_omano,  ;
           acum_orepu, cont_omano,  ;
           cont_orepu
     STORE 0 TO acum_pmano,  ;
           acum_prepu, cont_pmano,  ;
           cont_prepu
     DO WHILE wk_mode=pas_mode
          @ con_lin, 1 SAY  ;
            pas_numsol PICTURE  ;
            '999999999'
          @ con_lin, 10 SAY  ;
            pas_fecemi
          @ con_lin, 20 SAY  ;
            pas_feccom
          @ con_lin, 30 SAY  ;
            pas_fecrep
          @ con_lin, 40 SAY  ;
            pas_pnume PICTURE  ;
            '999999999'
          @ con_lin, 50 SAY  ;
            pas_pmano PICTURE  ;
            '999999999'
          @ con_lin, 60 SAY  ;
            pas_prepu PICTURE  ;
            '999999999'
          acum_pmano = acum_pmano +  ;
                       pas_pmano
          acum_prepu = acum_prepu +  ;
                       pas_prepu
          cont_pmano = cont_pmano +  ;
                       1
          cont_prepu = cont_prepu +  ;
                       1
          @ con_lin, 70 SAY  ;
            pas_onume PICTURE  ;
            '999999999'
          @ con_lin, 80 SAY  ;
            pas_omano PICTURE  ;
            '999999999'
          @ con_lin, 90 SAY  ;
            pas_orepu PICTURE  ;
            '999999999'
          acum_omano = acum_omano +  ;
                       pas_omano
          acum_orepu = acum_orepu +  ;
                       pas_orepu
          cont_omano = cont_omano +  ;
                       1
          cont_orepu = cont_orepu +  ;
                       1
          @ con_lin, 100 SAY  ;
            pas_fnume PICTURE  ;
            '999999999'
          @ con_lin, 110 SAY  ;
            pas_fmano PICTURE  ;
            '999999999'
          @ con_lin, 120 SAY  ;
            pas_frepu PICTURE  ;
            '999999999'
          acum_fmano = acum_fmano +  ;
                       pas_fmano
          acum_frepu = acum_frepu +  ;
                       pas_frepu
          cont_fmano = cont_fmano +  ;
                       1
          cont_frepu = cont_frepu +  ;
                       1
          con_lin = con_lin + 1
          IF con_lin = 50
               EJECT
               do pie with pag,tit1,TIT2,"&TIPRI",&tit_impre
               con_lin = 12
          ENDIF
          SKIP
     ENDDO
     acumg_oman = acumg_oman +  ;
                  acum_omano
     acumg_orep = acumg_orep +  ;
                  acum_orepu
     acumg_fman = acumg_fman +  ;
                  acum_fmano
     acumg_frep = acumg_frep +  ;
                  acum_frepu
     acumg_pman = acumg_pman +  ;
                  acum_pmano
     acumg_prep = acumg_prep +  ;
                  acum_prepu
     cont_gen = cont_gen +  ;
                cont_pmano
     con_lin = con_lin + 1
     @ con_lin, 1 SAY  ;
       'PROMEDIOS TOTALES SOBRE ' +  ;
       STR(cont_pmano) +  ;
       ' UNIDADES :'
     @ con_lin, 51 SAY acum_pmano /  ;
       cont_pmano PICTURE  ;
       '99999999'
     @ con_lin, 61 SAY acum_prepu /  ;
       cont_prepu PICTURE  ;
       '99999999'
     @ con_lin, 81 SAY acum_omano /  ;
       cont_omano PICTURE  ;
       '99999999'
     @ con_lin, 91 SAY acum_orepu /  ;
       cont_orepu PICTURE  ;
       '99999999'
     @ con_lin, 111 SAY  ;
       acum_fmano / cont_fmano  ;
       PICTURE '99999999'
     @ con_lin, 121 SAY  ;
       acum_frepu / cont_frepu  ;
       PICTURE '99999999'
     con_lin = con_lin + 1
ENDDO
con_lin = con_lin + 1
@ con_lin, 1 SAY  ;
  'PROMEDIOS GENERAL SOBRE ' +  ;
  STR(cont_gen) + ' UNIDADES :'
@ con_lin, 51 SAY acumg_pman /  ;
  cont_gen PICTURE '99999999'
@ con_lin, 61 SAY acumg_prep /  ;
  cont_gen PICTURE '99999999'
@ con_lin, 81 SAY acumg_oman /  ;
  cont_gen PICTURE '99999999'
@ con_lin, 91 SAY acumg_orep /  ;
  cont_gen PICTURE '99999999'
@ con_lin, 111 SAY acumg_fman /  ;
  cont_gen PICTURE '99999999'
@ con_lin, 121 SAY acumg_frep /  ;
  cont_gen PICTURE '99999999'
EJECT
? CHR(15)
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
@ 1, 110 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
centro = INT((130 - LEN(pie1)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 2, centro SAY pie1
@ 2, 110 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
centro = INT((130 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro SAY pie2
@ 3, 110 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 132)
@ 08, 0 SAY titu1
@ 09, 0 SAY titu2
@ 10, 0 SAY REPLICATE('=', 132)
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
