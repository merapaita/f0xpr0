*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep
SET ORDER TO ord_tecn
SELECT 2
USE SHARED GE_TAB0
SET ORDER TO CODIGO
SELECT 3
USE SHARED st_itecn
SET ORDER TO codigo
ON KEY LABEL F6 DO AYUDA 
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIEMPO DE REPARACION POR TECNICO '
STORE 0 TO codtec1, codtec2
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
     @ 04, 03 SAY  ;
       'Desde T‚cnico:'
     @ 05, 03 SAY  ;
       'Hasta T‚cnico:'
     @ 06, 03 SAY 'Desde  Fecha:'
     @ 07, 03 SAY 'Hasta  Fecha:'
     @ 08, 03 SAY  ;
       'Detalle/Res£men   :'
     @ 09, 03 SAY  ;
       'Pantalla/Impresora:'
     @ 04, 18 GET codtec1 VALID  ;
       valida(codtec1) WHEN  ;
       antes(1)
     @ 05, 18 GET codtec2 RANGE  ;
       codtec1 VALID  ;
       valida(codtec2)
     @ 06, 18 GET fecha1
     @ 07, 18 GET fecha2 RANGE  ;
       fecha1
     @ 08, 22 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 09, 22 GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep
     SET NEAR ON
     SEEK STR(codtec1, 9)
     SET NEAR OFF
     codte1 = STR(codtec1, 9)
     codte2 = STR(codtec2, 9)
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
                      PORL4104.FR  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2  ;
                      .AND.   ;
                      .NOT.  ;
                      EMPTY(fecfin)  ;
                      .AND.  ;
                      indest <>  ;
                      'N' WHILE  ;
                      (codtec >=  ;
                      codte1  ;
                      .AND.  ;
                      codtec <=  ;
                      codte2)
          ELSE
               REPORT FORMAT  ;
                      PORL4104.FRX  ;
                      TO PRINTER  ;
                      NOCONSOLE  ;
                      FOR fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2  ;
                      .AND.   ;
                      .NOT.  ;
                      EMPTY(fecfin)  ;
                      .AND.  ;
                      indest <>  ;
                      'N' WHILE  ;
                      (codtec >=  ;
                      codte1  ;
                      .AND.  ;
                      codtec <=  ;
                      codte2)
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
          SET PRINTER TO
     ELSE
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      PORL4104.FR  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE  ;
                      FOR fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2  ;
                      .AND.   ;
                      .NOT.  ;
                      EMPTY(fecfin)  ;
                      .AND.  ;
                      indest <>  ;
                      'N' WHILE  ;
                      (codtec >=  ;
                      codte1  ;
                      .AND.  ;
                      codtec <=  ;
                      codte2)
          ELSE
               REPORT FORMAT  ;
                      PORL4104.FRX  ;
                      TO FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE  ;
                      FOR fecemi >=  ;
                      fecha1  ;
                      .AND.  ;
                      fecemi <=  ;
                      fecha2  ;
                      .AND.   ;
                      .NOT.  ;
                      EMPTY(fecfin)  ;
                      .AND.  ;
                      indest <>  ;
                      'N' WHILE  ;
                      (codtec >=  ;
                      codte1  ;
                      .AND.  ;
                      codtec <=  ;
                      codte2)
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
CLOSE DATABASES
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab
IF EMPTY(wrk_codtab)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
SELECT st_itecn
SEEK STR(wrk_codtab, 9)
IF  .NOT. FOUND()
     DO error WITH  ;
        'C¢digo de T‚cnico No Existe'
     RETURN .F.
ENDIF
@ ROW(), 33 SAY SUBSTR(noment, 1,  ;
  30)
RETURN
*
PROCEDURE ayuda
SELECT st_itecn
campoa = '" "+codent+" "+noment+" "+CODTEC'
campob = '" "+noment+" "+codent+" "+CODTEC'
titulo = 'AYUDA DE TECNICOS'
DO ayuda2 WITH campoa, campob,  ;
   titulo,  ;
   'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
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
FUNCTION oodestec
PARAMETER ccodtec
SELECT st_itecn
SEEK ccodtec
IF FOUND()
     wrk_destec = st_itecn.noment
ELSE
     wrk_destec = ' '
ENDIF
SELECT st_iorep
RETURN wrk_destec
*
*** 
*** ReFox - retrace your steps ... 
***
