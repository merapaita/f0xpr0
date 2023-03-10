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
   ' INGRESO DE ARTICULOS POR DISTRITOS '
STORE SPACE(4) TO tipgar1,  ;
      tipgar2
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
     @ 04, 03 SAY 'Desde Emisor:'
     @ 05, 03 SAY 'Hasta Emisor:'
     @ 06, 03 SAY  ;
       'Desde  Fecha  :'
     @ 07, 03 SAY  ;
       'Hasta  Fecha  :'
     @ 08, 03 SAY  ;
       'Detalle/Res?men   :'
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
       '@m Detalle,Res?men'
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
     SELECT DISTINCT  ;
            st_isrep.numdoc,  ;
            st_isrep.fecemi,  ;
            st_isrep.codemi,  ;
            st_isrep.indori,  ;
            st_isrep.indest,  ;
            st_isrep.codmar,  ;
            st_isrep.codmod,  ;
            st_isrep.coddes,  ;
            st_iclpr.nomdis,  ;
            st_imode.codcla,  ;
            ge_tab0.tab_empres  ;
            FROM ST_ISREP,  ;
            ST_ICLPR, ST_IMODE,  ;
            GE_TAB0 WHERE  ;
            st_iclpr.codent =  ;
            st_isrep.codent AND  ;
            'EMIS' +  ;
            st_isrep.codemi =  ;
            ge_tab0.tab_codpre +  ;
            ge_tab0.tab_codtab  ;
            AND st_imode.codmod =  ;
            st_isrep.codmod AND  ;
            BETWEEN(st_isrep.fecemi,  ;
            fecha1, fecha2) AND  ;
            BETWEEN(st_isrep.codemi,  ;
            tipgar1, tipgar2) AND  ;
            st_isrep.indest <>  ;
            'N   ' ORDER BY  ;
            ge_tab0.tab_empres,  ;
            st_iclpr.nomdis,  ;
            st_imode.codcla,  ;
            st_isrep.codmar INTO  ;
            CURSOR QUERY
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF tipo = 'Res?men'
               REPORT FORMAT  ;
                      PORL4139  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4139 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
          SET PRINTER TO
     ELSE
          IF tipo = 'Res?men'
               REPORT FORMAT  ;
                      PORL4139  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4139 TO  ;
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
SEEK 'EMIS' + wrk_codtab
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C?digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), 25 SAY tab_destab
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
SET FILTER TO tab_codpre == 'EMIS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE EMISOR'
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
