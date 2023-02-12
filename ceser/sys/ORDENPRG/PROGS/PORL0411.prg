*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0411>'
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
USE SHARED st_iorep ORDER CODIGO
CREATE DBF pas_arte.dbf  ;
       (pas_codent C (9),  ;
       pas_noment C (30),  ;
       pas_desmar C (30),  ;
       pas_nommod C (30),  ;
       pas_fecori D)
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
          pregunta = 'indest#"N".and.fecemi=wk_feservi'
          tit_impre = '"CORRESPONDIENTE AL DIA "+dtoc(wk_feservi)'
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
          pregunta = 'indest#"N".and.(year(fecemi)=wk_year).and.(month(fecemi)=wk_mes)'
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
ERASE pas_arte.dbf
ERASE paso.idx
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
wk_numsol = SPACE(8)
STORE CTOD('  /  /  ') TO  ;
      wk_fecemi, wk_feccom
SELECT 3
USE ge_tab0 ORDER codigo
SELECT 4
USE SHARED st_imode ORDER CODIGO
SELECT 5
USE SHARED st_Itecn ORDER CODIGO
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta              
          wk_codmar = codmar
          wk_codmod = codmod
          wk_codtec = codtec
          wk_fecori = fecemi
          SELECT 5
          SET ORDER TO 1
          GOTO TOP
          SEEK wk_codtec
          IF FOUND()
               wk_codent = codent
               wk_noment = noment
          ENDIF
          SELECT 4
          SET ORDER TO 1
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
          wk_desmar = tab_destab
          SELECT 2
          APPEND BLANK
          REPLACE pas_codent WITH  ;
                  wk_codent
          REPLACE pas_noment WITH  ;
                  wk_noment
          REPLACE pas_desmar WITH  ;
                  wk_desmar
          REPLACE pas_nommod WITH  ;
                  wk_nommod
          REPLACE pas_fecori WITH  ;
                  wk_fecori
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     INDEX ON pas_codent +  ;
           DTOC(pas_fecori) TO  ;
           paso.idx
ENDIF
SELECT 3
USE
SELECT 4
USE
SELECT 5
USE
RETURN
*
PROCEDURE lis_arti4
tit1 = ' FECHA     PRODUCTO                     MODELO                        CANTIDAD '
tit2 = ' '
con_lin = 11
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag, wk_garmar,  ;
      wk_dife
STORE SPACE(9) TO ant_codent,  ;
      wk_codent
STORE SPACE(30) TO ant_noment,  ;
      ant_desmar, ant_nommod,  ;
      tipo
STORE 0 TO ant_fecori, wk_total,  ;
      wk_ttotal
wk_totprod = 1
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
?? CHR(18)
SELECT 2
SET ORDER TO 1
GOTO TOP
DO PIE WITH PAG,tit1,tit2,wk_feservi,"PRODUCCION POR TECNICO",&tit_impre
DO WHILE  .NOT. EOF()
     IF con_lin = 50
          EJECT
          DO PIE WITH PAG,tit1,tit2,wk_feservi,"PRODUCCION POR TECNICO",&tit_impre
          con_lin = 11
     ENDIF
     im = .F.
     IF ( .NOT. EMPTY(ant_fecori)  ;
        .AND.  .NOT.  ;
        EMPTY(ant_desmar)) .AND.   ;
        .NOT. EMPTY(ant_nommod)
          IF (pas_fecori =  ;
             ant_fecori)
               IF (pas_desmar =  ;
                  ant_desmar)
                    IF (pas_nommod =  ;
                       ant_nommod)
                         wk_totprod =  ;
                          wk_totprod +  ;
                          1
                    ENDIF
               ELSE
                    im = .T.
               ENDIF
          ELSE
               im = .T.
          ENDIF
          tipo = ant_desmar
     ENDIF
     IF im
          DO linea WITH  ;
             ant_fecori,  ;
             ant_desmar,  ;
             ant_nommod,  ;
             wk_totprod
     ENDIF
     IF pas_codent <> ant_codent
          IF  .NOT.  ;
              EMPTY(ant_codent)
               con_lin = con_lin +  ;
                         1
               @ con_lin, 73 SAY  ;
                 '-------'
               con_lin = con_lin +  ;
                         1
               @ con_lin, 45 SAY  ;
                 'TOTAL UNIDADES :' +  ;
                 ant_codent
               @ con_lin, 70 SAY  ;
                 wk_total PICTURE  ;
                 '99,999,999'
               con_lin = con_lin +  ;
                         1
               @ con_lin, 0 SAY  ;
                 REPLICATE('-',  ;
                 80)
               con_lin = con_lin +  ;
                         1
               wk_total = 0
          ENDIF
          @ con_lin, 1 SAY  ;
            'TECNICO...:'
          @ con_lin, 12 SAY  ;
            pas_codent PICTURE  ;
            '999999999'
          @ con_lin, 23 SAY  ;
            pas_noment PICTURE  ;
            '@!'
     ENDIF
     ant_codent = pas_codent
     ant_desmar = pas_desmar
     ant_noment = pas_noment
     ant_nommod = pas_nommod
     ant_fecori = pas_fecori
     wk_total = wk_total + 1
     wk_ttotal = wk_ttotal + 1
     SKIP
ENDDO
IF  .NOT. EMPTY(tipo)
     DO linea WITH ant_fecori,  ;
        ant_desmar, ant_nommod,  ;
        wk_totprod
ENDIF
con_lin = con_lin + 1
@ con_lin, 73 SAY '-------'
con_lin = con_lin + 1
@ con_lin, 45 SAY  ;
  'TOTAL UNIDADES :' +  ;
  ant_codent
@ con_lin, 70 SAY wk_total  ;
  PICTURE '99,999,999'
con_lin = con_lin + 1
@ con_lin, 0 SAY REPLICATE('-',  ;
  80)
con_lin = con_lin + 1
IF con_lin = 50
     EJECT
     DO PIE WITH PAG,tit1,tit2,wk_feservi,"ORDENES DE REPARACION",&tit_impre
     con_lin = 11
ENDIF
con_lin = con_lin + 1
@ con_lin, 45 SAY  ;
  'TOTAL GENERAL  ====>'
@ con_lin, 70 SAY wk_ttotal  ;
  PICTURE '99,999,999'
EJECT
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
USE EXCLUSIVE pas_arte
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
@ 1, 61 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
centro = INT((80 - LEN(pie1)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 2, centro SAY pie1
@ 2, 61 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
centro = INT((80 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro SAY pie2
@ 3, 61 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY REPLICATE('=', 80)
RETURN
*
PROCEDURE linea
PARAMETER a_fecori, a_desmar,  ;
          a_nommod, w_totprod
con_lin = con_lin + 1
@ con_lin, 1 SAY a_fecori
@ con_lin, 10 SAY a_desmar  ;
  PICTURE '@!'
@ con_lin, 42 SAY a_nommod  ;
  PICTURE '@!'
@ con_lin, 74 SAY w_totprod  ;
  PICTURE '99,999'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
