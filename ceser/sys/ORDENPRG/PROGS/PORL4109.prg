*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = PROGRAM()
tit_prg = 'REPORTES'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' RECHAZADOS POR LINEA DE CLASIFICACION '
STORE SPACE(4) TO codtec1,  ;
      codtec2
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
ON KEY LABEL F6 DO AYUDA
DO WHILE .T.
     CLOSE DATABASES
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 01 CLEAR TO 07, 73
     @ 04, 01 TO 11, 73
     SET CURSOR ON
     @ 05, 03 SAY 'Desde L죒ea:'  ;
       GET codtec1 PICTURE '!!!!'  ;
       VALID valtab('CLAT', ;
       codtec1,25,30) WHEN  ;
       colocaf6()
     @ 06, 03 SAY 'Hasta L죒ea:'  ;
       GET codtec2 PICTURE '!!!!'  ;
       VALID valtab('CLAT', ;
       codtec2,25,30) WHEN  ;
       colocaf6()
     @ 07, 03 SAY  ;
       'Desde Fecha  :' GET  ;
       fecha1
     @ 08, 03 SAY  ;
       'Hasta Fecha  :' GET  ;
       fecha2 RANGE fecha1
     @ 09, 03 SAY  ;
       'Por Detalle/Res즡en   :'  ;
       GET tipo PICTURE  ;
       '@m Detalle,Res즡en'
     @ 10, 03 SAY  ;
       'Por Impresora/Pantalla:'  ;
       GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT DISTINCT  ;
            st_iorep.numdoc,  ;
            st_itecn.codent,  ;
            st_iorep.fecemi,  ;
            st_iorep.observ,  ;
            st_iorep.codent,  ;
            st_iorep.indori,  ;
            st_iorep.indest,  ;
            st_iorep.auxest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.numsol,  ;
            st_itecn.codcla,  ;
            st_iorep.fecest,  ;
            st_iorep.codtec FROM  ;
            ST_ITECN, ST_IOREP  ;
            WHERE st_iorep.codtec =  ;
            st_itecn.codent AND  ;
            (BETWEEN(st_itecn.codcla,  ;
            codtec1, codtec2))  ;
            AND  ;
            (BETWEEN(st_iorep.fecemi,  ;
            fecha1, fecha2)) AND  ;
            st_iorep.indest <>  ;
            'N   ' AND  ;
            (st_iorep.indori =  ;
            'GREC' OR  ;
            st_iorep.indori =  ;
            'FREC') ORDER BY  ;
            st_itecn.codcla,  ;
            st_iorep.codtec,  ;
            st_iorep.fecemi,  ;
            st_iorep.indori INTO  ;
            CURSOR QUERY
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF tipo = 'Res즡en'
               REPORT FORMAT  ;
                      porl4106  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4106 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          SET PRINTER TO
          @ PROW(), PCOL() SAY  ;
            CHR(15)
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          IF tipo = 'Res즡en'
               REPORT FORMAT  ;
                      porl4106  ;
                      SUMMARY TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4106 TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE
          ENDIF
          SET SYSMENU ON
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          MODIFY FILE TEXT4.TXT  ;
                 NOEDIT WINDOW  ;
                 pantall
          SET SYSMENU OFF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDDO
CLOSE DATABASES
DO saca_win
@ 24, 69 SAY '같같같같같�'
*
PROCEDURE antes
PARAMETER opc
DO esc_indica WITH 1, 'AYU',  ;
   'BUS', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(codtec1)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_itecn
          SET ORDER TO 1
          SEEK STR(codtec1, 9)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C줰igo de T괹nico No Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 33 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
     CASE opc = 2
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(codtec2)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_itecn
          SET ORDER TO 1
          SEEK STR(codtec2, 9)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C줰igo de T괹nico No Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 33 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
ENDCASE
RETURN
*
PROCEDURE ayuda
USE SHARED ge_tab0 ORDER codigo
SET FILTER TO tab_codpre == 'CLAT'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE CLASIFICACION'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
USE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
