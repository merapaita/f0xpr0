*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = ' REPORTE '
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TARIFA DE MANO DE OBRA '
SELECT 1
USE ST_MOBRA ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
ppas = .T.
DO WHILE ppas
     DO esc_modo WITH 'P'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     STORE SPACE(4) TO w_marini,  ;
           w_marfin
     STORE 1 TO w_copia
     @ 07, 01 CLEAR TO 18, 77
     @ 04, 00 TO 14, 77
     SET CURSOR ON
     @ 05, 01 SAY 'Marca   :'  ;
       SIZE 01, 10, 0
     @ 06, 10 SAY 'Del :' SIZE 01,  ;
       10, 0
     @ 07, 10 SAY ' Al :' SIZE 01,  ;
       10, 0
     @ 09, 01 SAY 'Destino :'  ;
       SIZE 01, 10, 0
     @ 06, 14 GET w_marini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     @ 07, 14 GET w_marfin RANGE  ;
       w_marini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       WHEN oowhen(VARREAD())
     @ 10, 10 GET w_salida  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RVTN Pantalla;Impresora'  ;
       WHEN oowhen(VARREAD())
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF w_salida = 2
          @ 11, 24 SAY  ;
            'Copias  :' GET  ;
            w_copia RANGE 1,10  ;
            PICTURE '99' VALID   ;
            .NOT. EMPTY(w_copia)
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     SELECT st_mobra
     IF w_salida = 1
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          w_fildoc = SUBSTR(f_archivo(),  ;
                     1, 8) +  ;
                     '.DOC'
          REPO FORM PORL0415 TO FILE;
 &w_fildoc FOR MO_CODMAR >= w_marini AND;
MO_CODMAR <= w_marfin  NOCONSOLE
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM &w_fildoc NOEDIT WINDOW;
PANTALL
          SET SYSMENU OFF
          dele file &w_fildoc
     ELSE
          ??? CHR(18)
          FOR a = 1 TO w_copia
               REPORT FORMAT  ;
                      PORL0415 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR  ;
                      mo_codmar >=  ;
                      w_marini  ;
                      .AND.  ;
                      mo_codmar <=  ;
                      w_marfin
          ENDFOR
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL F6
CLOSE DATABASES
RETURN
*
PROCEDURE oowhen
PARAMETER opc
DO CASE
     CASE opc = 'W_MARINI' .OR.  ;
          opc = 'W_MARFIN'
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BUS', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA
     CASE opc = 'W_SALIDA'
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
PROCEDURE oovalid
PARAMETER opc
DO CASE
     CASE opc = 'W_MARINI'
          SELECT ge_tab0
          SEEK 'MARC' + w_marini
          IF FOUND()
               @ 06, 19 SAY  ;
                 tab_destab
          ENDIF
     CASE opc = 'W_MARFIN'
          SELECT ge_tab0
          SEEK 'MARC' + w_marfin
          IF FOUND()
               @ 07, 19 SAY  ;
                 tab_destab
          ENDIF
ENDCASE
*
PROCEDURE ayuda
ON KEY LABEL F6
SELECT ge_tab0
SET FILTER TO tab_codpre == 'MARC'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE MARCAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
ON KEY LABEL F6 do ayuda
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
