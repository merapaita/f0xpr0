*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DEFINE WINDOW tempo FROM 01, 00  ;
       TO 18, 79
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ESTADO DE ARTICULOS (ESTADO ACTUAL) '
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER CODIGO
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED ST_MVORD ORDER CODIGO
SELECT 4
USE ST_ICLPR ORDER CODIGO
DO WHILE .T.
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 07, 01 CLEAR TO 15, 77
     STORE 1 TO wrk_opcion,  ;
           wrk_salida
     STORE SPACE(4) TO wrk_estado,  ;
           wrk_tabini,  ;
           wrk_tabfin
     STORE 1 TO wrk_copia
     wrk_orden = " AND ST_IOREP.CODTALL BETWEEN '001 ' AND '001 ' "
     wrk_orden2 = 'ST_IOREP.CODTALL'
     wrk_destab = SPACE(30)
     @ 04, 01 SAY  ;
       'Estado         :' SIZE 01,  ;
       16, 0
     @ 06, 01 SAY  ;
       'Ordenado por   :' SIZE 01,  ;
       16, 0
     @ 10, 01 SAY  ;
       'Tipo de Salida :' SIZE 01,  ;
       16, 0
     SET CURSOR ON
     @ 04, 16 GET wrk_estado  ;
       PICTURE '@!' VALID  ;
       valida2(wrk_estado,1) WHEN  ;
       antes(1)
     @ 07, 16 GET wrk_opcion  ;
       DEFAULT 1 SIZE 1, 10, 0  ;
       PICTURE  ;
       '@*RVTN Taller;Emisor;Marca'  ;
       VALID valida() WHEN  ;
       antes(2)
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     @ 11, 16 GET wrk_salida  ;
       DEFAULT 1 SIZE 1, 11, 0  ;
       PICTURE  ;
       '@*RVTN Detalle;Resumen'  ;
       WHEN antes(2)
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 1
     SET ORDER TO ORD_ESEM
     SET FILTER TO auxest = wrk_estado;
.AND. (codemi >= wrk_tabini;
.AND. codemi <= wrk_tabfin);
.AND. indest <> 'N';
.AND. indest <> 'F';
.AND. indest <> 'B';
.AND. codtall <> '004 ';
.AND. YEAR(fecemi) > 1994;
.AND. MONTH(fecest < 7)
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO mensa WITH  ;
             '***  Un momento, Por Favor ...  ***',  ;
             'SACA'
          DO error WITH  ;
             '***  No Existen registros a Listar  ***'
          LOOP
     ENDIF
     GOTO TOP
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'SACA'
     DO esc_indica WITH 2, 'BBB',  ;
        'VER', 'IMP', 'ESC'
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>-4
          wrk_inkey = INKEY(0,  ;
                      'H')
     ENDDO
     IF wrk_inkey = -6
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(18)
               REPORT FORMAT  ;
                      PORL4001 TO  ;
                      PRINTER  ;
                      NOCONSOLE
               SET PRINTER TO
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
     ENDIF
     IF wrk_inkey = -4
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          wrk_file = SUBSTR(f_archivo(),  ;
                     1, 8) +  ;
                     '.TXT'
          REPO FORM PORL4001 TO FILE;
 &wrk_file  NOCONSOLE
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          MODI FILE  &wrk_file;
 NOEDIT WINDOW TEMPO
          DELE FILE &wrk_file
          RELEASE WINDOW tempo
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT 2
DO CASE
     CASE opc = 1
          SET FILTER TO tab_codpre ==;
'ESOR'
     CASE opc = 2
          DO CASE
               CASE wrk_opcion =  ;
                    1
                    SET FILTER TO tab_codpre;
== 'TALL'
               CASE wrk_opcion =  ;
                    2
                    SET FILTER TO tab_codpre;
== 'EMIS'
               CASE wrk_opcion =  ;
                    3
                    SET FILTER TO tab_codpre;
== 'MARC'
          ENDCASE
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
*
PROCEDURE antes
PARAMETER opc1
DO CASE
     CASE opc1 = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE opc1 = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
     CASE opc1 = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 DO AYUDA WITH;
2
ENDCASE
RETURN
*
FUNCTION valida
ON KEY
@ 07, 29 SAY 'Del :'
@ 08, 29 SAY 'Al  :'
@ 07, 35 GET wrk_tabini PICTURE  ;
  '@!' VALID valida2(wrk_tabini, ;
  2) WHEN antes(3)
@ 08, 35 GET wrk_tabfin PICTURE  ;
  '@!' VALID valida2(wrk_tabfin, ;
  2) WHEN antes(3)
READ
IF LASTKEY() = 27
     RETURN .F.
ENDIF
*
FUNCTION valida2
PARAMETER wrk_codtab, opc3
SELECT 2
DO CASE
     CASE opc3 = 1
          SEEK 'ESOR' +  ;
               wrk_codtab
          IF FOUND()
               wrk_destab = tab_destab
          ENDIF
     CASE opc3 = 2
          IF EMPTY(wrk_codtab)
               DO error WITH  ;
                  'No se Permiten Blancos'
               RETURN .F.
          ENDIF
          SELECT 2
          DO CASE
               CASE wrk_opcion =  ;
                    1
                    SEEK 'TALL' +  ;
                         wrk_codtab
                    wrk_orden = ' AND ST_IOREP.CODTALL BETWEEN wrk_tabini AND wrk_tabfin '
                    wrk_orden2 = 'ST_IOREP.CODTALL'
               CASE wrk_opcion =  ;
                    2
                    SEEK 'EMIS' +  ;
                         wrk_codtab
                    wrk_orden2 = 'ST_IOREP.CODEMI'
                    wrk_orden = ' CODEMI BETWEEN wrk_tabini AND wrk_tabfin '
               CASE wrk_opcion =  ;
                    3
                    SEEK 'MARC' +  ;
                         wrk_codtab
                    wrk_orden = ' AND ST_IOREP.CODMAR BETWEEN wrk_tabini AND wrk_tabfin '
                    wrk_orden2 = 'ST_IOREP.CODMAR'
          ENDCASE
ENDCASE
IF  .NOT. FOUND()
     DO error WITH  ;
        'C¢digo de Tabla No Existe'
     RETURN .F.
ENDIF
@ ROW(), 40 SAY SUBSTR(tab_destab,  ;
  1, 20)
*
*** 
*** ReFox - retrace your steps ... 
***
