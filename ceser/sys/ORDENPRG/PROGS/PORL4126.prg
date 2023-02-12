*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON ERROR
ind_prg = '<PORL4126>'
tit_prg = 'INFORME'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIEMPO PROMEDIO DE REPARACION EN MESA DE TALLER '
@ 3, 2 CLEAR TO 5, 77
@ 3, 2 TO 5, 77
ppas = .T.
SELECT 1
USE SHARED st_estad ORDER CODIGO
pas4126 = da_nombre()
Create table &PAS4126 (pas_marca C(4),pas_indori;
C(4),pas_desmarc C(35),pas_mode C(15),pas_desmode;
C(35),pas_falla C(4),pas_desfall C(35),pas_dias;
n(10),pas_estado C(40))
SELECT 2
use &PAS4126 EXCLUSIVE   
DO WHILE ppas
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
          pregunta = 'fecrep=wk_fecprom'
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
          pregunta = '(year(fecrep)=wk_year).and.(month(fecrep)=wk_mes)'
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
             '** No Se Registran Movimientos **'
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
x4126 = pas4126 + '.DBF'
ERASE &X4126
x4126 = pas4126 + '.IDX'
ERASE &X4126
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
SELECT 5
USE SHARED st_mvord ORDER CODIGO
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta
          wk_indori = indori
          wk_marca = codmar
          wk_mode = codmod
          wk_falla = codfal
          wk_dias = fecdes -  ;
                    fecrep
          wk_numord = numord
          WAIT STR(wk_numord, 8)
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
          SELECT 3
          GOTO TOP
          codaux = 'FALL' +  ;
                   wk_falla
          SEEK '&codaux'
          wk_desfall = tab_destab
          SELECT 5
          SET ORDER TO 1
          SEEK STR(wk_numord, 8)
          IF FOUND()
               DO WHILE  .NOT.  ;
                  EOF()
                    IF orden =  ;
                       STR(wk_numord,  ;
                       8)
                         wk_estado =  ;
                          destado
                         SELECT 2
                         APPEND BLANK
                         REPLACE pas_marca  ;
                                 WITH  ;
                                 wk_marca
                         REPLACE pas_desmar  ;
                                 WITH  ;
                                 wk_desmarc
                         REPLACE pas_mode  ;
                                 WITH  ;
                                 wk_mode
                         REPLACE pas_desmod  ;
                                 WITH  ;
                                 wk_nommod
                         REPLACE pas_indori  ;
                                 WITH  ;
                                 wk_indori
                         REPLACE pas_falla  ;
                                 WITH  ;
                                 wk_falla
                         REPLACE pas_desfal  ;
                                 WITH  ;
                                 wk_desfall
                         REPLACE pas_dias  ;
                                 WITH  ;
                                 wk_dias
                         REPLACE pas_estado  ;
                                 WITH  ;
                                 wk_estado
                         wk_hay =  ;
                          .T.
                    ENDIF
                    SELECT 5
                    SKIP
               ENDDO
          ENDIF
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     Index on pas_marca+pas_indori+pas_mode+pas_falla;
to &PAS4126   
ENDIF
SELECT 3
USE
SELECT 5
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
tit1 = '                                                        TIEMPO PROM.EN DIAS'
con_lin = 11
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO FILE printer
SET CONSOLE OFF
? CHR(18)
DO PIE WITH PAG,tit1,wk_fecprom,"TIEMPO DE DEMORA EN MESA DE TALLER",&tit_impre
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
          @ con_lin, 6 SAY  ;
            'TIPO DE GARANTIA...:' +  ;
            wk_indori
          con_lin = con_lin + 1
          DO WHILE wk_indori= ;
             pas_indori .AND.  ;
             wk_marca=pas_marca
               wk_mode = pas_mode
               wk_desmode = pas_desmod
               con_lin = con_lin +  ;
                         1
               @ con_lin, 9 SAY  ;
                 'MODELO...:' +  ;
                 wk_mode + '  ' +  ;
                 wk_desmode
               con_lin = con_lin +  ;
                         1
               @ con_lin, 12 SAY  ;
                 'TIPO DE FALLA...:'
               DO WHILE wk_mode= ;
                  pas_mode .AND.  ;
                  wk_indori= ;
                  pas_indori  ;
                  .AND. wk_marca= ;
                  pas_marca
                    wk_falla = pas_falla
                    wk_desfall = pas_desfal
                    wk_estado = pas_estado
                    acum_dias = 0
                    c = 0
                    DO WHILE  ;
                       wk_falla= ;
                       pas_falla  ;
                       .AND.  ;
                       wk_mode= ;
                       pas_mode  ;
                       .AND.  ;
                       wk_indori= ;
                       pas_indori  ;
                       .AND.  ;
                       wk_marca= ;
                       pas_marca
                         c = c +  ;
                             1
                         wk_dias =  ;
                          pas_dias
                         acum_dias =  ;
                          acum_dias +  ;
                          wk_dias
                         wk_estado =  ;
                          pas_estado
                         @ con_lin,  ;
                           15 SAY  ;
                           'DESCRIPCION ESTADO ' +  ;
                           wk_estado
                         SKIP
                    ENDDO
                    @ con_lin, 15  ;
                      SAY  ;
                      wk_falla +  ;
                      '  ' +  ;
                      wk_desfall +  ;
                      STR(acum_dias /  ;
                      c)
                    con_lin = con_lin +  ;
                              1
                    IF con_lin =  ;
                       50
                         EJECT
                         do pie with pag,tit1,wk_fecprom,"TIEMPO DE DEMORA EN MESA DE TALLER",&tit_impre
                         con_lin =  ;
                          11
                    ENDIF
               ENDDO
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
