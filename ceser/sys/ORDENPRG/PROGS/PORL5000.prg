*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
SET CENTURY ON
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INGRESO DE ORDENES POR FECHAS '
CLOSE DATABASES
SELECT 1
USE SHARED st_iorep ORDER  ;
    ord_fchemi
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED st_iseri ORDER  ;
    ser_codmar
SELECT st_iorep
STORE SPACE(4) TO emisor1,  ;
      emisor2
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
ON KEY LABEL f6 do ayuda
@ 03, 2 TO 9, 77
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
DO WHILE .T.
     @ 07, 01 CLEAR TO 13, 77
     @ 03, 02 TO 9, 77
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY  ;
       'Desde Fecha  :' GET  ;
       fecha1
     @ 04, 30 SAY  ;
       'Hasta fecha  :' GET  ;
       fecha2 RANGE fecha1
     @ 05, 03 SAY  ;
       'Desde Emisor :' GET  ;
       emisor1 PICTURE '@!' VALID  ;
       valida(emisor1,1) WHEN  ;
       antes(1)
     @ 06, 03 SAY  ;
       'Hasta Emisor :' GET  ;
       emisor2 RANGE emisor1  ;
       PICTURE '@1' VALID  ;
       valida(emisor2,1) WHEN  ;
       antes(1)
     @ 07, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 08, 03 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 4
     CREATE CURSOR ing (numdoc C  ;
            (8), fecemi D, horemi  ;
            C (8), codemi C (4),  ;
            codmar C (4), codmod  ;
            C (15), numser C (20),  ;
            codent C (9), indori  ;
            C (4), indest C (4),  ;
            codtall C (4), numsol  ;
            C (8), infor C  ;
            (120))
     INDEX ON DTOS(fecemi) +  ;
           codtall + codemi TO  ;
           codigo
     SELECT st_iorep
     SET NEAR ON
     SEEK dtoc2(fecha1)
     SET NEAR OFF
     SCAN WHILE fecemi <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (st_iorep.codemi >=  ;
             emisor1 .AND.  ;
             st_iorep.codemi <=  ;
             emisor2) .AND.  ;
             indest <> 'N'
               SELECT ing
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi,  ;
                       horemi  ;
                       WITH  ;
                       st_iorep.horemi,  ;
                       codmar  ;
                       WITH  ;
                       st_iorep.codmar
               REPLACE codmod  ;
                       WITH  ;
                       st_iorep.codmod,  ;
                       numser  ;
                       WITH  ;
                       st_iorep.numser,  ;
                       codemi  ;
                       WITH  ;
                       st_iorep.codemi,  ;
                       indest  ;
                       WITH  ;
                       st_iorep.indest
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       codent  ;
                       WITH  ;
                       st_iorep.codent,  ;
                       codtall  ;
                       WITH  ;
                       st_iorep.codtall,  ;
                       infor WITH  ;
                       SUBSTR(st_iorep.observ,  ;
                       1, 120)
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT ing
     GOTO TOP
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          SET DEVICE TO PRINTER
          SET PRINTER ON
          ?? CHR(27) + CHR(15)
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      porl6000  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl6000 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          SET PRINTER TO
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          filtxt = SYS(3) +  ;
                   '.TXT'
          IF tipo = 'Res£men'
               repo form porl6000 to file;
&filtxt noconsole summary
          ELSE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &filtxt noedit window;
pantall 
          dele file &filtxt
          SET SYSMENU OFF
     ENDIF
ENDDO
DO sacawin
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, val
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
        '*** C¢digo de Tabla No Existe ***'
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
SET FILTER TO
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
