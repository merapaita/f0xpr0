*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL4134>'
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ARTICULOS REPARADOS POR FECHAS '
SELECT 3
USE SHARED GE_TAB0 ORDER CODIGO
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
STORE '100 ' TO emisor1, emisor2
DO WHILE .T.
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 9, 77
     @ 4, 5 SAY SPACE(50)
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
       emisor2 PICTURE '@1' VALID  ;
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
     SELECT DISTINCT st_mvord.dia,  ;
            st_mvord.tecnico,  ;
            st_mvord.estado,  ;
            st_mvord.orden,  ;
            st_iorep.codemi,  ;
            st_iorep.codent,  ;
            st_iorep.indest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.codtec,  ;
            st_iorep.codcca,  ;
            st_iorep.observ,  ;
            st_iorep.codtall,  ;
            st_iorep.indori,  ;
            st_iorep.numsol,  ;
            st_isrep.coddes FROM  ;
            ST_MVORD, ST_IOREP,  ;
            ST_ISREP WHERE  ;
            st_iorep.numdoc =  ;
            st_mvord.orden AND  ;
            st_isrep.numdoc =  ;
            st_iorep.numsol AND  ;
            (st_mvord.dia >=  ;
            fecha1 AND  ;
            st_mvord.dia <=  ;
            fecha2 AND  ;
            st_mvord.estado =  ;
            '010 ') AND  ;
            st_iorep.codemi >=  ;
            emisor1 AND  ;
            st_iorep.codemi <=  ;
            emisor2 ORDER BY  ;
            st_mvord.dia,  ;
            st_iorep.codemi,  ;
            st_iorep.indori,  ;
            st_mvord.orden INTO  ;
            CURSOR REPAR
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     IF output = 'Impresora'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          ??? CHR(15)
          IF tipo = 'Detalle'
               REPORT FORMAT  ;
                      porl4138 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4138  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
          SET PRINTER TO
     ELSE
          IF tipo = 'Detalle'
               REPORT FORMAT  ;
                      porl4138 TO  ;
                      FILE  ;
                      text4.txt  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      porl4138  ;
                      SUMMARY TO  ;
                      FILE  ;
                      text4.txt  ;
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
PARAMETER cobta, opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(cobta)
               DO error WITH  ;
                  ' *** No se Permiten Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + cobta
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Tabla No Existe ***'
               RETURN .F.
          ENDIF
          @ ROW(), 25 SAY  ;
            tab_destab
ENDCASE
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
*** 
*** ReFox - retrace your steps ... 
***
