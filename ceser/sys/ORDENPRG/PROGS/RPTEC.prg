*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET ESCAPE ON
CLEAR
ind_prg = PROGRAM()
tit_prg = 'REPORTES'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
STORE SPACE(4) TO codtec1,  ;
      codtec2
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
ON KEY LABEL F6 DO AYUDA
DO WHILE .T.
     CLOSE DATABASES
     @ 04, 01 CLEAR TO 07, 73
     @ 04, 01 TO 11, 73
     SET CURSOR ON
     @ 05, 03 SAY 'Desde L죒ea:'  ;
       GET codtec1 PICTURE  ;
       '!!!!'
     @ 06, 03 SAY 'Hasta L죒ea:'  ;
       GET codtec2 PICTURE  ;
       '!!!!'
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
     SELECT 1
     USE SHARED st_itecn
     SET ORDER TO 3
     SELECT 2
     USE SHARED st_iorep
     SET ORDER TO 10
     SELECT 1
     GOTO TOP
     CLEAR
     DO WHILE  .NOT. EOF()
          IF VAL(codtec) > 0  ;
             .AND. (VAL(codcla) >=  ;
             VAL(codtec1) .AND.  ;
             VAL(codcla) <=  ;
             VAL(codtec2))
               STORE codtec TO  ;
                     mcodtec
               STORE noment TO  ;
                     mnoment
               ? '   ' + mcodtec +  ;
                 '  ' +  ;
                 st_itecn.codcla +  ;
                 '  ' + mnoment
               WAIT ' '
               DO tecnico
               WAIT '** '
               SELECT 1
               SKIP
               LOOP
          ELSE
               SKIP
               LOOP
          ENDIF
     ENDDO
     WAIT ' '
ENDDO
CLOSE DATABASES
@ 24, 69 SAY '같같같같같�'
*
PROCEDURE tecnico
SELECT 2
SET FILTER TO codtec = mcodtec
BROWSE
WAIT ' '
*
PROCEDURE xxxx
DO WHILE .T.
     SEEK (mcodtec)
     IF (fecemi >= fecha1 .AND.  ;
        fecfin <= fecha2)
          ? DTOC(fecemi) + '  ' +  ;
            DTOC(fecfin, 1) +  ;
            '   ' + codtec + '  ' +  ;
            st_itecn.codcla +  ;
            '  ' + mnoment
          IF  .NOT. EOF()
               SKIP
          ELSE
               EXIT
          ENDIF
     ELSE
          IF  .NOT. EOF()
               SKIP
          ELSE
               EXIT
          ENDIF
     ENDIF
ENDDO
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
PROCEDURE nada
IF output = 'Impresora'
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     DO mensa WITH  ;
        '*** I m p r i m i e n d o ... ***',  ;
        'COLO'
     IF tipo = 'Res즡en'
          REPORT FORMAT porl4106  ;
                 SUMMARY TO  ;
                 PRINTER  ;
                 NOCONSOLE
     ELSE
          REPORT FORMAT porl4106  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ENDIF
     SET PRINTER TO
     @ PROW(), PCOL() SAY CHR(15)
     DO mensa WITH  ;
        '*** I m p r i m i e n d o ... ***',  ;
        'SACA'
ELSE
     IF tipo = 'Res즡en'
          REPORT FORMAT porl4106  ;
                 SUMMARY TO FILE  ;
                 text4.txt  ;
                 NOCONSOLE
     ELSE
          REPORT FORMAT porl4106  ;
                 TO FILE  ;
                 text4.txt  ;
                 NOCONSOLE
     ENDIF
     SET SYSMENU ON
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     MODIFY FILE TEXT4.TXT NOEDIT  ;
            WINDOW pantall
     SET SYSMENU OFF
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***
