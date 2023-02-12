*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0416>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIEMPO DEMORA EN MESA DE TALLER'
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_iorep INDEX  ;
    st_1orep, st_2orep
pas416 = da_nombre()
Create table &PAS416 (pas_marca C(4),pas_indori;
C(4),pas_desmarc C(35),pas_mode C(15),pas_desmode;
C(35),pas_falla C(4),pas_desfall C(35))
SELECT 2
use &PAS416 EXCLUSIVE   
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
     wk_feservi = CTOD( ;
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
          @ 4, 25 GET wk_feservi  ;
            PICTURE '!!/!!/!!'  ;
            VALID fecha()
          pregunta = 'fecrep=wk_servi'
          tit_impre = '"CORESPONDIENTE AL DIA "+dtoc(wk_feservi)'
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
          pregunta = '(year(fecrep)=wk_year).and.(month(fecrep)=wk_mes)'
          tit_impre = '"CORESPONDIENTE AL MES "+Str(wk_mes,2)+" DE "+Str(wk_year,4)'
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
          DO lis_arti3
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
x416 = pas416 + '.DBF'
ERASE &X416
x416 = pas416 + '.IDX'
ERASE &X416
x4162 = pas4162 + '.DBF'
ERASE &X4162
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
wk_mode = SPACE(15)
wk_fecemi = CTOD('  /  /  ')
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_imode INDEX  ;
    st_1mode, st_2mode
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta
          wk_indori = indori
          wk_marca = codmar
          wk_mode = codmod
          wk_falla = codfal
          SELECT 4
          SET ORDER TO 1
          GOTO TOP
          codaux = wk_marca +  ;
                   wk_mode
          find '&codaux'
          wk_nommod = nommod
          SELECT 3
          GOTO TOP
          codaux = 'MARC' +  ;
                   wk_marca
          find '&codaux'
          wk_desmarc = tab_descri
          SELECT 3
          GOTO TOP
          codaux = 'FALL' +  ;
                   wk_falla
          find '&codaux'
          wk_desfall = tab_descri
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
          REPLACE pas_falla WITH  ;
                  wk_falla
          REPLACE pas_desfal WITH  ;
                  wk_desfall
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_marca+pas_indori+pas_mode+pas_falla;
to &pas416   
ENDIF
SELECT 3
USE
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
@ 09, 0 SAY REPLICATE('=', 80)
RETURN
*
PROCEDURE lis_arti3
pas4162 = da_nombre()
Create table &pas4162 (pa2_marca C(4),;
pa2_indori C(4),pa2_desmarc C(35), pa2_mode;
C(15),pa2_desmode C(35),pa2_canti N(8))
SELECT 3
use &pas4162  EXCLUSIVE     
STORE SPACE(4) TO vie_marca,  ;
      vie_indori
STORE SPACE(15) TO vie_mode
STORE 0 TO wk_contado, acum
SELECT 2
SET ORDER TO 1
GOTO TOP
DO WHILE  .NOT. EOF()
     wk_contado = wk_contado + 1
     IF wk_contado = 1
          vie_marca = pas_marca
          vie_desmar = pas_desmar
          vie_mode = pas_mode
          vie_desmod = pas_desmod
          vie_indori = pas_indori
     ENDIF
     IF vie_marca = pas_marca  ;
        .AND. vie_indori =  ;
        pas_indori .AND. vie_mode =  ;
        pas_mode
          acum = acum + 1
     ELSE
          SELECT 3
          APPEND BLANK
          REPLACE pa2_marca WITH  ;
                  vie_marca,  ;
                  pa2_desmar WITH  ;
                  vie_desmar
          REPLACE pa2_mode WITH  ;
                  vie_mode,  ;
                  pa2_desmod WITH  ;
                  vie_desmod
          REPLACE pa2_indori WITH  ;
                  vie_indori,  ;
                  pa2_canti WITH  ;
                  acum
          acum = 1
     ENDIF
     SELECT 2
     vie_marca = pas_marca
     vie_desmar = pas_desmar
     vie_mode = pas_mode
     vie_desmod = pas_desmod
     vie_indori = pas_indori
     SKIP
ENDDO
SELECT 3
APPEND BLANK
REPLACE pa2_marca WITH vie_marca
REPLACE pa2_desmar WITH  ;
        vie_desmar
REPLACE pa2_mode WITH vie_mode
REPLACE pa2_desmod WITH  ;
        vie_desmod
REPLACE pa2_indori WITH  ;
        vie_indori
REPLACE pa2_canti WITH acum
SELECT 2
ZAP
tit1 = '                                                                     CANTIDAD '
con_lin = 11
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag, wk_garanti,  ;
      wk_garmar, tope
STORE 0 TO wk_canti, acum10
wk_viejora = 8
STORE SPACE(4) TO wk_antiguo,  ;
      wk_indori2
STORE SPACE(15) TO wk_anti1,  ;
      wk_anti3
STORE SPACE(40) TO wk_des1,  ;
      wk_des2, wk_desmarc, tipo,  ;
      wk_des3
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
SELECT 3
GOTO TOP
DO PIE WITH PAG,tit1,wk_feservi,"SALIDAS DE ARTICULOS POR UNIDADES",&tit_impre
DO WHILE  .NOT. EOF()
     acum10 = acum10 + 1
     IF acum10 = 1
          wk_antiguo = pa2_marca
          wk_desmarc = pa2_desmar
          wk_indori2 = pa2_indori
          wk_anti1 = pa2_mode
          wk_des1 = pa2_desmod
          wk_canti = pa2_canti
     ENDIF
     IF con_lin = 50
          EJECT
          DO PIE WITH PAG,tit1,wk_feservi,"SALIDAS DE ARTICULOS POR UNIDADES",&tit_impre
          con_lin = 11
     ENDIF
     IF pa2_marca <> wk_antiguo  ;
        .OR. acum10 = 1
          IF  .NOT.  ;
              EMPTY(wk_antiguo)  ;
              .AND. acum10 <> 1
               con_lin = con_lin +  ;
                         1
               @ con_lin, 69 SAY  ;
                 '-----------'
               con_lin = con_lin +  ;
                         1
               @ con_lin, 35 SAY  ;
                 'RESUMEN MARCA :' +  ;
                 wk_antiguo
               @ con_lin, 67 SAY  ;
                 wk_garmar  ;
                 PICTURE  ;
                 '99,999,999'
               con_lin = con_lin +  ;
                         1
               @ con_lin, 0 SAY  ;
                 REPLICATE('-',  ;
                 80)
               con_lin = con_lin +  ;
                         1
               sum_gen = sum_gen +  ;
                         wk_garmar
               wk_garmar = 0
          ENDIF
          @ con_lin, 1 SAY  ;
            'MARCA...:' +  ;
            pa2_marca + '  ' +  ;
            pa2_desmar
          tope = 0
     ENDIF
     con_lin = con_lin + 1
     @ con_lin, 3 SAY pa2_mode  ;
       PICTURE '@!'
     @ con_lin, 20 SAY pa2_desmod  ;
       PICTURE '@!'
     @ con_lin, 67 SAY pa2_canti  ;
       PICTURE '99,999,999'
     wk_garanti = wk_garanti +  ;
                  pa2_canti
     wk_antiguo = pa2_marca
     wk_desmarc = pa2_desmar
     wk_indori2 = pa2_indori
     wk_anti1 = pa2_mode
     wk_des1 = pa2_desmod
     wk_canti = pa2_canti
     SKIP
     IF pa2_indori <> wk_indori2  ;
        .OR. pa2_marca <>  ;
        wk_antiguo
          con_lin = con_lin + 1
          @ con_lin, 69 SAY  ;
            '-----------'
          con_lin = con_lin + 1
          IF wk_indori2 = 'FGAR'
               tipo = 'FUERA DE GARANTIA'
          ELSE
               tipo = 'EN GARANTIA'
          ENDIF
          @ con_lin, 40 SAY  ;
            'TOTAL ' + tipo
          @ con_lin, 67 SAY  ;
            wk_garanti PICTURE  ;
            '99,999,999'
          con_lin = con_lin + 1
          wk_garmar = wk_garmar +  ;
                      wk_garanti
          wk_garanti = 0
     ENDIF
ENDDO
con_lin = con_lin + 1
@ con_lin, 69 SAY '-----------'
con_lin = con_lin + 1
@ con_lin, 35 SAY  ;
  'RESUMEN MARCA :' + wk_antiguo
@ con_lin, 67 SAY wk_garmar  ;
  PICTURE '99,999,999'
con_lin = con_lin + 2
sum_gen = sum_gen + wk_garmar
wk_garmar = 0
IF con_lin = 50
     EJECT
     DO PIE WITH PAG,tit1,wk_feservi,"SALIDAS DE ARTICULOS POR UNIDADES",&tit_impre
     con_lin = 11
ENDIF
con_lin = con_lin + 2
@ con_lin, 35 SAY  ;
  'TOTAL GENERAL    ====>'
@ con_lin, 67 SAY sum_gen PICTURE  ;
  '99,999,999'
EJECT
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 3
use &pas4162    
ZAP
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
