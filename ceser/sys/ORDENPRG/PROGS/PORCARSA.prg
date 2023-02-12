*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
ON KEY LABEL F6 DO AYUDA
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ARTICULOS POR TIPO DE GARANTIA '
STORE SPACE(4) TO tipgar1,  ;
      tipgar2, emisor1, emisor2
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
       'Desde Atenci¢n:'
     @ 05, 03 SAY  ;
       'Hasta Atenci¢n:'
     @ 06, 03 SAY  ;
       'Desde Emisor  :'
     @ 07, 03 SAY  ;
       'Hasta Emisor  :'
     @ 08, 03 SAY  ;
       'Desde  Fecha  :'
     @ 08, 30 SAY  ;
       'Hasta  Fecha  :'
     @ 09, 03 SAY  ;
       'Detalle/Res£men   :'
     @ 10, 03 SAY  ;
       'Pantalla/Impresora:'
     @ 04, 18 GET tipgar1 PICTURE  ;
       '@!' VALID valida(tipgar1, ;
       1) WHEN antes(1)
     @ 05, 18 GET tipgar2 RANGE  ;
       tipgar1 PICTURE '@!' VALID  ;
       valida(tipgar2,1)
     @ 06, 18 GET emisor1 PICTURE  ;
       '@!' VALID valida(emisor1, ;
       2) WHEN antes(1)
     @ 07, 18 GET emisor2 RANGE  ;
       emisor1 PICTURE '@!' VALID  ;
       valida(emisor2,2)
     @ 08, 18 GET fecha1
     @ 08, 48 GET fecha2 RANGE  ;
       fecha1
     @ 09, 22 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 10, 22 GET output PICTURE  ;
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
            st_iorep.numdoc,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numsol,  ;
            st_iorep.codtec,  ;
            st_iorep.fecfabo,  ;
            st_iorep.numser,  ;
            st_iorep.fecemi,  ;
            st_iorep.indest,  ;
            st_iorep.indori,  ;
            st_iorep.codtall,  ;
            st_iorep.codemi,  ;
            st_iseri.codent,  ;
            st_iorep.codfabo,  ;
            st_iorep.cosrep,  ;
            st_iorep.cosmob,  ;
            st_iorep.numfabo,  ;
            st_iorep.observ,  ;
            st_iorep.codent,  ;
            st_iclpr.noment FROM  ;
            ST_ISERI, ST_IOREP,  ;
            ST_ICLPR WHERE  ;
            st_iorep.numser =  ;
            st_iseri.numser AND  ;
            st_iorep.codmar =  ;
            st_iseri.codmar AND  ;
            st_iorep.codmod =  ;
            st_iseri.modelo AND  ;
            st_iorep.codent =  ;
            st_iclpr.codent AND  ;
            st_iorep.indest = 'F'  ;
            AND st_iorep.indori =  ;
            'GARA' AND  ;
            st_iseri.codent =  ;
            ' 25360443' AND  ;
            MONTH(st_iorep.fecemi) >  ;
            0 AND  ;
            MONTH(st_iorep.fecemi) <  ;
            6 AND  ;
            BETWEEN(st_iorep.codemi,  ;
            emisor1, emisor2)  ;
            ORDER BY  ;
            st_iorep.numser INTO  ;
            CURSOR GARA
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      PORCARSA  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORCARSA TO  ;
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
                      PORCARSA  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT5.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORCARSA TO  ;
                      FILE  ;
                      TEXT5.TXT  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          MODIFY COMMAND  ;
                 text5.txt WINDOW  ;
                 pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, val
IF val = 1
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
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
ELSE
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
ENDIF
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
IF ROW() < 6
     SET FILTER TO tab_codpre == 'INGA'
     titulo = 'AYUDA DE TIPO DE ATENCION'
ELSE
     SET FILTER TO tab_codpre == 'EMIS'
     titulo = 'AYUDA DE EMISORES'
ENDIF
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
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
