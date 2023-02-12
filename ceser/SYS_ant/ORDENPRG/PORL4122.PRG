*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
ON KEY LABEL F6 DO AYUDA
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIEMPO DESDE COMPRA A GEN S/S'
STORE 'GARA' TO tipgar1, tipgar2
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
DO WHILE .T.
     @ 07, 01 CLEAR TO 13, 77
     @ 04, 30 SAY SPACE(30)
     @ 03, 01 TO 11, 77
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY 'Desde T.Aten:'
     @ 05, 03 SAY 'Hasta T.Aten:'
     @ 06, 03 SAY  ;
       'Desde  Fecha  :'
     @ 07, 03 SAY  ;
       'Hasta  Fecha  :'
     @ 08, 03 SAY  ;
       'Detalle/Res£men   :'
     @ 09, 03 SAY  ;
       'Pantalla/Impresora:'
     @ 04, 18 GET tipgar1 PICTURE  ;
       '@!' VALID valida(tipgar1, ;
       1) WHEN antes(1)
     @ 05, 18 GET tipgar2 RANGE  ;
       tipgar1 PICTURE '@!' VALID  ;
       valida(tipgar2,1)
     @ 06, 18 GET fecha1
     @ 07, 18 GET fecha2 RANGE  ;
       fecha1
     @ 08, 22 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 09, 22 GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_isrep.numdoc,  ;
            st_isrep.fecemi,  ;
            st_isrep.codemi,  ;
            st_isrep.indori,  ;
            st_isrep.codmar,  ;
            st_isrep.codmod,  ;
            st_isrep.numser,  ;
            ge_tab0.tab_empres,  ;
            st_iseri.fecvta,  ;
            st_iseri.fecgar,  ;
            st_iseri.docgar FROM  ;
            ST_ISREP, GE_TAB0,  ;
            ST_ISERI WHERE 'EMIS' +  ;
            st_isrep.codemi =  ;
            ge_tab0.tab_codpre +  ;
            ge_tab0.tab_codtab  ;
            AND st_isrep.codmar +  ;
            st_isrep.codmod +  ;
            st_isrep.numser =  ;
            st_iseri.codmar +  ;
            st_iseri.modelo +  ;
            st_iseri.numser AND  ;
            BETWEEN(st_iseri.fecvta,  ;
            fecha1, fecha2) AND  ;
            st_isrep.indest <>  ;
            'N' AND  ;
            BETWEEN(st_isrep.indori,  ;
            tipgar1, tipgar2)  ;
            ORDER BY  ;
            st_isrep.indori,  ;
            ge_tab0.tab_empres,  ;
            st_iseri.fecvta,  ;
            st_isrep.numdoc INTO  ;
            CURSOR GARA
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          ??? CHR(15)
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      PORL4122  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4122 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
          SET PRINTER TO
     ELSE
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      PORL4122  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4122 TO  ;
                      FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 text4.txt NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, val
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .F.
ENDIF
IF LASTKEY() = 27
     RETURN
ENDIF
IF EMPTY(wrk_codtab)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
SELECT ge_tab0
SEEK 'INGA' + wrk_codtab
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C¢digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), 25 SAY tab_destab
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
SET FILTER TO tab_codpre == 'INGA'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TIPO DE ATENCION'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER opc
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
