*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0421>'
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ORDENES DE REPARACION'
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE st_iorep INDEX st_1orep,  ;
    st_2orep
CREATE DBF pas_arte.dbf (pas_nume  ;
       N (8), pas_marca C (4),  ;
       pas_indori C (4),  ;
       pas_desmod C (35),  ;
       pas_rutcli C (9),  ;
       pas_nomcli C (30),  ;
       pas_ingre D, pas_compre D,  ;
       pas_estado C (4))
SELECT 2
USE pas_arte.dbf
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
          pregunta = 'indest#"N".and.fecemi=wk_feservi'
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
          pregunta = 'indest#"N".and.(year(fecemi)=wk_year).and.(month(fecemi)=wk_mes)'
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
ERASE pas_arte.dbf
ERASE pa2_arte.dbf
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
USE st_itabl INDEX st_1tabl
SELECT 4
USE st_imode INDEX st_1mode,  ;
    st_2mode
SELECT 5
USE st_iclpr INDEX st_1clpr
SELECT 6
USE st_isrep INDEX st_1srep
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
          find '&codaux'
          wk_nommod = nommod
          SELECT 3
          GOTO TOP
          codaux = 'MARC' +  ;
                   wk_marca
          find '&codaux'
          wk_desmarc = tab_descri
          SELECT 5
          GOTO TOP
          SEEK 'C' + wk_codcli
          wk_nomcli = noment
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
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     INDEX ON STR(pas_nume, 8) TO  ;
           paso.idx
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
tit1 = ' NUMERO DE ORDEN      CLIENTE                         FECHA      MARCA       MODELO                 ESTADO      FECHA        FECHA '
tit2 = '                                                     DE INGRESO                                             COMPROMETIDA   REPARAC.'
con_lin = 11
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag, wk_garmar,  ;
      wk_dife
wk_viejora = 8
SET PRINTER ON
SET DEVICE TO FILE 421.txt
SET CONSOLE OFF
SELECT 2
SET ORDER TO 1
GOTO TOP
DO PIE WITH PAG,tit1,tit2,wk_feservi,"ORDENES DE REPARACION",&tit_impre
DO WHILE  .NOT. EOF()
     IF con_lin = 50
          DO PIE WITH PAG,tit1,tit2,wk_feservi,"ORDENES DE REPARACION",&tit_impre
          con_lin = 11
     ENDIF
     con_lin = con_lin + 1
     @ con_lin, 1 SAY pas_nume  ;
       PICTURE '99999999'
     @ con_lin, 11 SAY pas_rutcli  ;
       PICTURE '@!'
     @ con_lin, 22 SAY pas_nomcli  ;
       PICTURE '@!'
     @ con_lin, 54 SAY pas_ingre
     @ con_lin, 64 SAY pas_marca  ;
       PICTURE '@!'
     @ con_lin, 70 SAY pas_desmod
     @ con_lin, 105 SAY  ;
       pas_indori
     @ con_lin, 112 SAY  ;
       pas_compre
     wk_garmar = wk_garmar + 1
     SKIP
ENDDO
sum_gen = sum_gen + wk_garmar
wk_garmar = 0
IF con_lin = 50
     EJECT
     DO PIE WITH PAG,tit1,tit2,wk_feservi,"ORDENES DE REPARACION",&tit_impre
     con_lin = 11
ENDIF
EJECT
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
USE pas_arte
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
@ 07, 0 SAY REPLICATE('=', 132)
@ 08, 0 SAY titu1
@ 09, 0 SAY titu2
@ 10, 0 SAY REPLICATE('=', 132)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
