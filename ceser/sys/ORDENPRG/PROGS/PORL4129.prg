*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4129>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ARTEFACTOS NO REPARADOS EN EL TALLER DE ORIGEN'
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_IOREP ORDER CODIGO
pas4129 = da_nombre()
Create table &PAS4129 (pas_marca C(4),pas_indori;
C(4),pas_desmarc C(35),pas_mode C(15),pas_desmode;
C(35))
SELECT 2
use &PAS4129 EXCLUSIVE     
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
     wk_fecprom = CTOD( ;
                  '  /  /  ')
     wk_hay = .F.
     STORE SPACE(35) TO pregunta,  ;
           tit_impre
     STORE 0 TO retorna, wk_mes,  ;
           wk_year
     DIMENSION op( 2)
     op( 1) = 'Ingreso por Fecha'
     op( 2) = 'Ingreso por Mes  '
     DO popap WITH 08, 29, op,  ;
        retorna
     RELEASE op
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
          @ 4, 25 GET wk_fecprom  ;
            PICTURE '!!/!!/!!'  ;
            VALID fecha()
          pregunta = '(fecemi=wk_fecprom).and.(indest#"F").and.(indest#"N").and.(indest#"C")'
          tit_impre = '"CORRESPONDIENTE AL DIA "+dtoc(wk_fecprom)'
     ENDIF
     IF retorna = 2
          @ 4, 5 SAY  ;
            '    Mes :                          A¤o :     '
          @ 4, 14 GET wk_mes  ;
            PICTURE '99' VALID  ;
            me()
          @ 4, 45 GET wk_year  ;
            PICTURE '9999' VALID  ;
            a¥()
          pregunta = '(year(fecemi)=wk_year).and.(month(fecemi)=wk_mes).and.(indest#"F").and.(indest#"N").and.(indest#"C")'
          tit_impre = '"CORRESPONDIENTE AL MES "+Str(wk_mes,2)+" DE "+Str(wk_year,4)'
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
     DO vali_exist
     IF wk_hay = .F.
          DO error WITH  ;
             '** No Se Registra Ninguna Salida De Articulo **'
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
x4129 = pas4129 + '.DBF'
ERASE &X4129
x4129 = pas4129 + '.IDX'
ERASE &X4129
CLOSE DATABASES
RETURN
*
PROCEDURE vali_exist
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_marca = SPACE(4)
wk_mode = SPACE(15)
wk_fecemi = CTOD('  /  /  ')
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_imode ORDER CODIGO
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta
          wk_indori = indori
          wk_marca = codmar
          wk_mode = codmod
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
          SELECT 2
          APPEND BLANK
          REPLACE pas_marca WITH  ;
                  wk_marca
          REPLACE pas_desmar WITH  ;
                  wk_desmarc
          REPLACE pas_mode WITH  ;
                  wk_mode
          REPLACE pas_desmod WITH  ;
                  wk_nommod
          REPLACE pas_indori WITH  ;
                  wk_indori
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_marca+pas_indori+pas_mode;
to &PAS4129    
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
tit1 = '          MODELO             DESCRIPCION                       CANTIDAD'
con_lin = 11
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
?? CHR(18)
DO PIE WITH PAG,tit1,wk_fecprom,"ARTEFACTOS NO REPARADOS EN TALLER ORIGEN",&tit_impre
SELECT 2
SET ORDER TO 1
GOTO TOP
DO WHILE  .NOT. EOF()
     wk_marca = pas_marca
     wk_desmarc = pas_desmar
     con_lin = con_lin + 1
     @ con_lin, 3 SAY 'MARCA...:' +  ;
       wk_marca + '  ' +  ;
       wk_desmarc
     con_lin = con_lin + 1
     DO WHILE wk_marca=pas_marca
          wk_indori = pas_indori
          con_lin = con_lin + 1
          @ con_lin, 6 SAY  ;
            'TIPO DE GARANTIA...:' +  ;
            wk_indori
          con_lin = con_lin + 1
          @ con_lin, 9 SAY  ;
            'MODELO...:'
          con_lin = con_lin + 1
          DO WHILE wk_indori= ;
             pas_indori .AND.  ;
             wk_marca=pas_marca
               wk_mode = pas_mode
               wk_desmode = pas_desmod
               con_lin = con_lin +  ;
                         1
               c = 0
               DO WHILE wk_mode= ;
                  pas_mode .AND.  ;
                  wk_indori= ;
                  pas_indori  ;
                  .AND. wk_marca= ;
                  pas_marca
                    c = c + 1
                    SKIP
               ENDDO
               @ con_lin, 15 SAY  ;
                 wk_mode + '  ' +  ;
                 wk_desmode +  ;
                 STR(c)
               IF con_lin = 50
                    EJECT
                    do pie with pag,tit1,wk_fecprom,"ARTEFACTOS NO REPARADOS EN EL TALLER DE ORIGEN",&tit_impre
                    con_lin = 11
               ENDIF
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
PARAMETER pag, titu1, wk_fech,  ;
          pie1, pie2
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
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 4, centro SAY pie1
@ 5, centro SAY pie2
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY REPLICATE('=', 80)
RETURN
*
PROCEDURE fecha
IF EMPTY(wk_fecprom)
     wk_fecprom = DATE()
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
