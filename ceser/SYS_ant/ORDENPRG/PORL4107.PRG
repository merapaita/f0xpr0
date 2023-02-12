*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = PROGRAM()
tit_prg = 'REPORTES'
@ 24, 69 SAY ind_prg
SET ESCAPE OFF
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep
SET ORDER TO 1
SELECT 3
USE st_itecn ALIAS st_itecn
SET ORDER TO codigo
SELECT 2
USE SHARED st_mvord
SET ORDER TO mvo_tecnic
SET RELATION TO orden INTO st_iorep, tecnico;
INTO st_itecn
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' RECHAZOS POR TECNICO '
STORE 0 TO codtec1, codtec2
STORE DATE() TO fecha1, fecha2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
ON KEY LABEL F6 DO AYUDA
DO WHILE .T.
     SELECT st_itecn
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 01 CLEAR TO 07, 73
     @ 04, 01 TO 11, 73
     SET CURSOR ON
     @ 05, 03 SAY  ;
       'Desde T괹nico:' GET  ;
       codtec1 VALID despues(1)  ;
       WHEN colocaf6()
     @ 06, 03 SAY  ;
       'Hasta T괹nico:' GET  ;
       codtec2 RANGE codtec1  ;
       VALID despues(2) WHEN  ;
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
          SET RELATION TO
          CLOSE DATABASES
          DO saca_win
          ON KEY LABEL f6
          RETURN
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_mvord
     codte1 = STR(codtec1, 9)
     codte2 = STR(codtec2, 9)
     SEEK '008'
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF tipo = 'Res즡en'
               REPORT FORMAT  ;
                      por41072  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR  ;
                      (tecnico >=  ;
                      codte1  ;
                      .AND.  ;
                      tecnico <=  ;
                      codte2)  ;
                      .AND. (dia >=  ;
                      fecha1  ;
                      .AND. dia <=  ;
                      fecha2)  ;
                      WHILE  ;
                      estado =  ;
                      '008'
          ELSE
               REPORT FORMAT  ;
                      porl4107 TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR  ;
                      (tecnico >=  ;
                      codte1  ;
                      .AND.  ;
                      tecnico <=  ;
                      codte2)  ;
                      .AND. (dia >=  ;
                      fecha1  ;
                      .AND. dia <=  ;
                      fecha2)  ;
                      WHILE  ;
                      estado =  ;
                      '008'
          ENDIF
          SET DEVICE TO SCREEN
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          IF tipo = 'Res즡en'
               REPORT FORMAT  ;
                      por41072  ;
                      SUMMARY TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE  ;
                      FOR  ;
                      (tecnico >=  ;
                      codte1  ;
                      .AND.  ;
                      tecnico <=  ;
                      codte2)  ;
                      .AND. (dia >=  ;
                      fecha1  ;
                      .AND. dia <=  ;
                      fecha2)  ;
                      WHILE  ;
                      estado =  ;
                      '008'
          ELSE
               REPORT FORMAT  ;
                      porl4107 TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE  ;
                      FOR  ;
                      (tecnico >=  ;
                      codte1  ;
                      .AND.  ;
                      tecnico <=  ;
                      codte2)  ;
                      .AND. (dia >=  ;
                      fecha1  ;
                      .AND. dia <=  ;
                      fecha2)  ;
                      WHILE  ;
                      estado =  ;
                      '008'
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 TEXT4.TXT NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
@ 24, 69 SAY '같같같같같�'
RETURN
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
          IF EMPTY(codtec2)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_itecn
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
SELECT st_itecn
campoa = '" "+codent+" "+noment+" "+CODTEC'
campob = '" "+noment+" "+codent+" "+CODTEC'
titulo = 'AYUDA DE TECNICOS'
DO ayuda2 WITH campoa, campob,  ;
   titulo,  ;
   'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
