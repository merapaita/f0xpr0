*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INGRESO DE ORDENES POR MARCA '
STORE DATE() TO fecha1, fecha2
STORE 1 TO copia
STORE SPACE(4) TO marc, w_tall1,  ;
      w_tall2
STORE 'Impresora' TO output
STORE 'Detalle' TO w_tipo
CLOSE DATABASES
SELECT 1
USE SHARED st_imode ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_fecdoc
SELECT 7
USE SHARED ge_tab0 ORDER codigo
STORE .T. TO pas
DO WHILE pas
     @ 07, 01 CLEAR TO 13, 77
     @ 03, 02 TO 9, 77
     DO esc_modo WITH 'S'
     @ 04, 03 SAY 'Marca     :'  ;
       GET marc PICTURE '@!'  ;
       VALID valida(VARREAD(),1)  ;
       WHEN antes(VARREAD())
     @ 05, 03 SAY 'Del Fecha :'  ;
       GET fecha1 WHEN  ;
       antes(VARREAD())
     @ 05, 40 SAY 'Al:' GET  ;
       fecha2 RANGE fecha1 WHEN  ;
       antes(VARREAD())
     @ 06, 03 SAY 'Del Taller:'  ;
       GET w_tall1 PICTURE '@!'  ;
       VALID valida(VARREAD(),1)  ;
       WHEN antes(VARREAD())
     @ 06, 40 SAY 'Al:' GET  ;
       w_tall2 RANGE w_tall1  ;
       VALID valida(VARREAD(),2)  ;
       WHEN antes(VARREAD())
     @ 07, 03 SAY  ;
       'Detalle/Res£men :' GET  ;
       w_tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 08, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     READ
     IF LASTKEY() = 27
          STORE .F. TO pas
     ELSE
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          IF w_tipo = 'Detalle'
               SELECT st_iorep.numdoc,  ;
                      st_iorep.codemi,  ;
                      st_iorep.codent,  ;
                      st_iorep.indori,  ;
                      st_iorep.auxest,  ;
                      st_iorep.codmar,  ;
                      st_iorep.codmod,  ;
                      st_iorep.numser,  ;
                      st_iorep.codtec,  ;
                      st_iorep.fecest,  ;
                      st_iorep.numsol,  ;
                      st_iorep.fecemi,  ;
                      st_iorep.observ  ;
                      FROM  ;
                      ST_IOREP  ;
                      WHERE  ;
                      st_iorep.codmar =  ;
                      marc AND  ;
                      st_iorep.fecemi >=  ;
                      fecha1 AND  ;
                      st_iorep.fecemi <=  ;
                      fecha2 AND  ;
                      st_iorep.indest <>  ;
                      'N' AND  ;
                      BETWEEN(st_iorep.codtall,  ;
                      w_tall1,  ;
                      w_tall2)  ;
                      ORDER BY  ;
                      st_iorep.codmod,  ;
                      st_iorep.indori,  ;
                      st_iorep.fecemi,  ;
                      st_iorep.numdoc  ;
                      INTO CURSOR  ;
                      QUERY
               SET RELATION TO codmod;
INTO st_imode
          ELSE
               SELECT st_iorep.numdoc,  ;
                      st_iorep.codemi,  ;
                      st_iorep.codent,  ;
                      st_iorep.indori,  ;
                      st_iorep.auxest,  ;
                      st_iorep.codmar,  ;
                      st_iorep.codmod,  ;
                      st_iorep.numser,  ;
                      st_iorep.codtec,  ;
                      st_iorep.fecest,  ;
                      st_iorep.numsol,  ;
                      st_iorep.fecemi,  ;
                      st_iorep.observ  ;
                      FROM  ;
                      ST_IOREP  ;
                      WHERE  ;
                      st_iorep.codmar =  ;
                      marc AND  ;
                      st_iorep.fecemi >=  ;
                      fecha1 AND  ;
                      st_iorep.fecemi <=  ;
                      fecha2 AND  ;
                      st_iorep.indest <>  ;
                      'N' AND  ;
                      BETWEEN(st_iorep.codtall,  ;
                      w_tall1,  ;
                      w_tall2)  ;
                      ORDER BY  ;
                      st_iorep.indori,  ;
                      st_iorep.codmod,  ;
                      st_iorep.fecemi  ;
                      INTO CURSOR  ;
                      QUERY
          ENDIF
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               IF w_tipo =  ;
                  'Detalle'
                    REPORT FORMAT  ;
                           POR40922  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4092  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               SET PRINTER TO
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ELSE
               w_fildoc = SUBSTR(f_archivo(),  ;
                          1, 8) +  ;
                          '.DOC'
               IF w_tipo =  ;
                  'Detalle'
                    REPO FORM POR40922;
TO FILE &w_fildoc NOCONSOLE
               ELSE
                    REPO FORM PORL4092;
TO FILE &w_fildoc NOCONSOLE
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               MODI COMM &w_fildoc NOEDIT;
WINDOW PANTALL
               SET SYSMENU OFF
               dele file &w_fildoc
          ENDIF
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
ON KEY
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
'MARC'
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE MARCAS'
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
     CASE opc = 'FECHA1' .OR. opc =  ;
          'FECHA2'
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
     CASE opc = 'MARC'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA WITH;
2
ENDCASE
RETURN
*
FUNCTION valida
PARAMETER opcion, pos
DO CASE
     CASE opcion = 'W_TALL1' .OR.  ;
          opcion = 'W_TALL2'
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
          IF opcion = 'W_TALL2'
               IF w_tall2 <  ;
                  w_tall1
                    RETURN .F.
               ENDIF
          ENDIF
     CASE opcion = 'MARC'
          if empty(&opcion)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          seek "MARC"+&opcion
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
ENDCASE
DO CASE
     CASE pos = 1
          @ ROW(), 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
     CASE pos = 2
          @ ROW(), 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            17)
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
