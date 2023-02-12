*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL f6 do ayuda01
ON KEY LABEL f10 do fcinco
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INGRESO DE ORDENES POR DISTRITOS '
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_fecdoc
SELECT 3
USE SHARED st_iclpr ORDER codigo
SELECT 2
w_copia = 1
output = 'Impresora'
STORE 'Detalle' TO tipo
w_fecha1 = DATE()
w_fecha2 = DATE()
w_tall1 = SPACE(4)
w_tall2 = SPACE(4)
w_dist1 = SPACE(4)
w_dist2 = SPACE(4)
STORE SPACE(4) TO w_tipini,  ;
      w_tipfin
DO esc_modo WITH 'S'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
sigue = .T.
DO WHILE sigue
     STORE 0 TO opc1, opc2
     opc = SPACE(10)
     w_text = SPACE(10)
     @ 08, 01 CLEAR TO 13, 77
     @ 04, 30 SAY SPACE(30)
     @ 03, 02 TO 10, 77
     @ 04, 05 SAY SPACE(50)
     @ 04, 03 SAY  ;
       'Desde Fecha   :'
     @ 04, 40 SAY  ;
       'Hasta Fecha   :'
     @ 05, 03 SAY  ;
       'Desde Taller  :'
     @ 05, 40 SAY  ;
       'Hasta Taller  :'
     @ 06, 03 SAY  ;
       'Desde Distrito:'
     @ 06, 40 SAY  ;
       'Hasta Distrito:'
     @ 07, 03 SAY  ;
       'Desde Tipo At.:'
     @ 07, 40 SAY  ;
       'Hasta Tipo At.:'
     @ 08, 03 SAY  ;
       'Por Pantalla/Impresora:'
     @ 09, 03 SAY  ;
       'Por Detalle/Res£men   :'
     @ 09, 40 SAY  ;
       'Copias        :'
     SET CURSOR ON
     @ 04, 18 GET w_fecha1 WHEN  ;
       antes(VARREAD())
     @ 04, 55 GET w_fecha2 RANGE  ;
       w_fecha1 WHEN  ;
       antes(VARREAD())
     @ 05, 18 GET w_tall1 PICTURE  ;
       '@!' VALID  ;
       valida(VARREAD(),1) WHEN  ;
       antes(VARREAD())
     @ 05, 55 GET w_tall2 RANGE  ;
       w_tall1 PICTURE '@!' VALID  ;
       valida(VARREAD(),2) WHEN  ;
       antes(VARREAD())
     @ 06, 18 GET w_dist1 PICTURE  ;
       '@!' VALID  ;
       valida(VARREAD(),1) WHEN  ;
       antes(VARREAD())
     @ 06, 55 GET w_dist2 RANGE  ;
       w_dist1 PICTURE '@!' VALID  ;
       valida(VARREAD(),2) WHEN  ;
       antes(VARREAD())
     @ 07, 18 GET w_tipini  ;
       PICTURE '@!' VALID  ;
       valida(VARREAD(),1) WHEN  ;
       antes(VARREAD())
     @ 07, 55 GET w_tipfin RANGE  ;
       w_tipini PICTURE '@!'  ;
       VALID valida(VARREAD(),2)  ;
       WHEN antes(VARREAD())
     @ 08, 28 GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 09, 28 GET tipo PICTURE  ;
       '@m Detalle,Resumen'
     @ 09, 55 GET w_copia RANGE 1, ;
       10 PICTURE '99'
     READ
     IF LASTKEY() = 27
          sigue = .F.
          LOOP
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     DO llena
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     GOTO TOP
     COUNT TO w_valor
     IF w_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     GOTO TOP
     IF output = 'Impresora'
          ??? CHR(15)
          FOR a = 1 TO w_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               IF tipo =  ;
                  'Detalle'
                    REPORT FORMAT  ;
                           PORL414X  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           porl0414  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
          SET PRINTER TO
     ELSE
          w_texto = f_texto() +  ;
                    '.TXT'
          IF tipo = 'Detalle'
               REPO FORM porl414X TO FILE;
&W_TEXTO NOCONSOLE
          ELSE
               SET SYSMENU ON
               REPO FORM porl0414 TO FILE;
&W_TEXTO NOCONSOLE SUMMARY
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &w_texto window pantall;
noedit
          SET SYSMENU OFF
     ENDIF
ENDDO
CLOSE DATABASES
erase &w_texto
DO sacawin
RETURN
*
FUNCTION valida
PARAMETER opcion, pos
DO CASE
     CASE opcion = 'W_TALL1' .OR.  ;
          opcion = 'W_TALL2'
          IF LASTKEY() = 27
               RETURN
          ENDIF
          if empty(&opcion)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          seek "TALL"+&opcion
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
     CASE opcion = 'W_DIST1' .OR.  ;
          opcion = 'W_DIST2'
          if empty(&opcion)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          seek "DIST"+&opcion
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
     CASE opcion = 'W_TIPINI'  ;
          .OR. opcion =  ;
          'W_TIPFIN'
          if empty(&opcion)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          seek "INGA"+&opcion
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          IF opcion = 'W_TIPINI'
               @ ROW(), 23 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 15)
          ENDIF
          IF opcion = 'W_TIPFIN'
               @ ROW(), 59 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 15)
          ENDIF
ENDCASE
DO CASE
     CASE pos = 1
          @ ROW(), 23 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE pos = 2
          @ ROW(), 59 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER num
SELECT ge_tab0
DO CASE
     CASE num = 1
          SET FILTER TO tab_codpre ==;
'TALL'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE TALLER'
     CASE num = 2
          SET FILTER TO tab_codpre ==;
'DIST'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE DISTRITOS'
     CASE num = 3
          SET FILTER TO tab_codpre ==;
'INGA'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE TIPO DE ATENCION'
ENDCASE
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 'W_FECHA1' .OR.  ;
          opc = 'W_FECHA2'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6
     CASE opc = 'W_TALL1' .OR.  ;
          opc = 'W_TALL2'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE opc = 'W_DIST1' .OR.  ;
          opc = 'W_DIST2'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
2
     CASE opc = 'W_TIPINI' .OR.  ;
          opc = 'W_TIPFIN'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
3
ENDCASE
RETURN
*
PROCEDURE llena
CREATE CURSOR ordis (numdoc C (8),  ;
       fecemi D, codemi C (4),  ;
       codtall C (4), codent C  ;
       (11), indori C (4), codmar  ;
       C (4), codmod C (15),  ;
       numser C (20), codtec C  ;
       (9), numsol C (8), auxest  ;
       C (4), nomdis C (4),  ;
       desent C (30), zona C  ;
       (4))
SELECT st_iorep
SET NEAR ON
SEEK DTOS(w_fecha1)
SET NEAR OFF
SCAN WHILE fecemi >= w_fecha1  ;
     .AND. fecemi <= w_fecha2  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N   ' .AND.  ;
        (codtall >= w_tall1 .AND.  ;
        codtall <= w_tall2) .AND.  ;
        (indori >= w_tipini .AND.  ;
        indori <= w_tipfin)
          SELECT st_iclpr
          SEEK 'C' +  ;
               st_iorep.codent
          IF FOUND() .AND.  ;
             (nomdis >= w_dist1  ;
             .AND. nomdis <=  ;
             w_dist2)
               SELECT ordis
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc
               REPLACE fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi
               REPLACE codtall  ;
                       WITH  ;
                       st_iorep.codtall
               REPLACE codent  ;
                       WITH  ;
                       st_iorep.codent
               REPLACE desent  ;
                       WITH  ;
                       st_iclpr.noment
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori
               REPLACE codmar  ;
                       WITH  ;
                       st_iorep.codmar
               REPLACE codmod  ;
                       WITH  ;
                       st_iorep.codmod
               REPLACE numser  ;
                       WITH  ;
                       st_iorep.numser
               REPLACE codtec  ;
                       WITH  ;
                       st_iorep.codtec
               REPLACE numsol  ;
                       WITH  ;
                       st_iorep.numsol
               REPLACE auxest  ;
                       WITH  ;
                       st_iorep.auxest
               REPLACE nomdis  ;
                       WITH  ;
                       st_iclpr.nomdis
               SELECT ge_tab0
               SEEK 'DIST' +  ;
                    st_iclpr.nomdis
               w_zona = SUBSTR(tab_parame,  ;
                        1, 4)
               SELECT ordis
               REPLACE zona WITH  ;
                       w_zona
          ENDIF
          SELECT st_iorep
     ENDIF
ENDSCAN
w_idx = f_indice() + '.idx'
SELECT ordis
IF tipo = 'Detalle'
     INDEX ON codtall + nomdis +  ;
           DTOS(fecemi) + codemi  ;
           TO w_idx
ELSE
     INDEX ON zona + nomdis TO  ;
           w_idx
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
