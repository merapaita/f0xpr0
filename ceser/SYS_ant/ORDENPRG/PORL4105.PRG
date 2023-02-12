*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = PROGRAM()
tit_prg = 'REPORTES'
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PENDIENTES POR TECNICOS'
CLOSE DATABASES
SELECT 1
USE ST_IOREP ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE ST_ICLPR ORDER CODIGO
SELECT 4
USE ST_ITECN ORDER CODIGO
STORE 0 TO codtec1, codtec2
STORE SPACE(4) TO estado1,  ;
      estado2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
STORE DATE() TO fecha1, fecha2
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 01 CLEAR TO 07, 73
     @ 04, 01 TO 11, 73
     SET CURSOR ON
     @ 05, 03 SAY  ;
       'Desde T‚cnico:' GET  ;
       codtec1 PICTURE  ;
       '999999999' VALID  ;
       despues(1) WHEN antes(1)
     @ 06, 03 SAY  ;
       'Hasta T‚cnico:' GET  ;
       codtec2 PICTURE  ;
       '999999999' VALID  ;
       despues(2) WHEN antes(1)
     @ 07, 03 SAY  ;
       'Desde Estado :' GET  ;
       estado1 PICTURE '@!' VALID  ;
       despues(3) WHEN antes(2)
     @ 07, 40 SAY 'Hasta:' GET  ;
       estado2 PICTURE '@!' VALID  ;
       despues(4) WHEN antes(2)
     @ 08, 03 SAY  ;
       'Desde Fecha  :' GET  ;
       fecha1
     @ 08, 40 SAY 'Hasta:' GET  ;
       fecha2 RANGE fecha1
     @ 09, 03 SAY  ;
       'Por Detalle/Res£men   :'  ;
       GET tipo PICTURE  ;
       '@m Detalle,Res£men'
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
            st_itecn.noment,  ;
            st_iorep.fecest,  ;
            st_iorep.codtec FROM  ;
            ST_ITECN, ST_IOREP  ;
            WHERE st_iorep.codtec =  ;
            st_itecn.codent AND  ;
            (st_iorep.auxest >=  ;
            estado1 AND  ;
            st_iorep.auxest <=  ;
            estado2 AND  ;
            st_iorep.codtec >=  ;
            STR(codtec1, 9) AND  ;
            st_iorep.codtec <=  ;
            STR(codtec2, 9) AND  ;
            st_iorep.fecemi >=  ;
            fecha1 AND  ;
            st_iorep.fecemi <=  ;
            fecha2 AND  ;
            st_iorep.indest <>  ;
            'N   ' AND  ;
            st_iorep.indest <>  ;
            'F   ' AND  ;
            st_iorep.indest <>  ;
            'B   ') ORDER BY  ;
            st_itecn.noment,  ;
            st_iorep.auxest,  ;
            st_iorep.indori,  ;
            st_iorep.fecemi INTO  ;
            CURSOR QUERY
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
                      porl4105  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4105 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          SET PRINTER TO
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      porl4105  ;
                      SUMMARY TO  ;
                      FILE  ;
                      t4105.txt  ;
                      NOCONSOLE
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               MODIFY COMMAND  ;
                      T4105.TXT  ;
                      NOEDIT  ;
                      WINDOW  ;
                      pantall
               SET SYSMENU OFF
          ELSE
               REPORT FORMAT  ;
                      porl4105 TO  ;
                      FILE  ;
                      t4105.txt  ;
                      NOCONSOLE
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               MODIFY COMMAND  ;
                      T4105.TXT  ;
                      NOEDIT  ;
                      WINDOW  ;
                      pantall
               SET SYSMENU OFF
          ENDIF
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDDO
CLOSE DATABASES
DO saca_win
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA02
ENDCASE
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
                  'C¢digo de T‚cnico No Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 33 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
     CASE opc = 2
          ON KEY
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
                  'C¢digo de T‚cnico No Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 33 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
     CASE opc = 3
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(estado1)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'ESOR' + estado1
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Estado No Existe'
               RETURN .F.
          ENDIF
          @ 07, 22 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            18)
     CASE opc = 4
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(estado2)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'ESOR' + estado2
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Estado No Existe'
               RETURN .F.
          ENDIF
          @ 07, 51 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            21)
ENDCASE
RETURN
*
PROCEDURE ayuda
SELECT st_itecn
campoa = '" "+codent+" "+noment+" "+CODTEC'
campob = '" "+noment+" "+codent+" "+CODTEC'
titulo = 'AYUDA DE TECNICOS'
DO ayuda2 WITH campoa, campob,  ;
   titulo,  ;
   'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
RETURN
*
PROCEDURE ayuda02
SELECT ge_tab0
SET FILTER TO tab_codpre == 'ESOR'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA ESTADO OPERACION'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
PROCEDURE nuevo
CREATE CURSOR PEND (p_numdoc C  ;
       (8), p_codent C (11),  ;
       p_fecemi D, p_observ C  ;
       (20), p_indori C (4),  ;
       p_auxest C (4), p_codmar C  ;
       (4), p_codmod C (15),  ;
       p_fecest D, p_numser C  ;
       (12), p_numsol C (8),  ;
       p_codtec C (9))
SELECT st_iorep
SET ORDER TO ORD_TECEST
SET NEAR ON
SEEK STR(codtec1, 9) + estado1
SET NEAR OFF
SCAN FOR codtec <= STR(codtec2,  ;
     9) .AND. auxest <= estado2
     SELECT pend
     APPEND BLANK
     REPLACE p_numdoc WITH  ;
             st_iorep.numdoc
     REPLACE p_numsol WITH  ;
             st_iorep.numsol
     REPLACE p_fecemi WITH  ;
             st_iorep.fecemi
     REPLACE p_fecest WITH  ;
             st_iorep.fecest
     REPLACE p_codent WITH  ;
             st_iorep.codent
     REPLACE p_indori WITH  ;
             st_iorep.indori
     REPLACE p_auxest WITH  ;
             st_iorep.auxest
     REPLACE p_codtec WITH  ;
             st_iorep.codtec
     REPLACE p_codmar WITH  ;
             st_iorep.codmar
     REPLACE p_codmod WITH  ;
             st_iorep.codmod
     REPLACE p_numser WITH  ;
             st_iorep.numser
     REPLACE p_observ WITH  ;
             st_iorep.observ
     SELECT st_iorep
ENDSCAN
SELECT pend
BROWSE
*
*** 
*** ReFox - retrace your steps ... 
***
