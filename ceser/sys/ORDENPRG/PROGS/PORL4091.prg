*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'INGRESO DE ARTICULOS POR EMISOR'
STORE DATE() TO fecha1, fecha2
STORE 1 TO copia
STORE SPACE(4) TO marc
STORE 'Impresora' TO output
CLOSE DATABASES
SELECT 7
USE SHARED ge_tab0 ORDER codigo
ON KEY LABEL F6 do ayuda
STORE .T. TO pas
STORE 'Detalle' TO opc
DO WHILE pas
     @ 07, 01 CLEAR TO 13, 77
     @ 3, 2 TO 9, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 03 SAY 'Emisor     :'  ;
       GET marc VALID  ;
       valta('EMIS',marc,30,30)
     @ 05, 03 SAY 'Desde Fecha:'  ;
       GET fecha1
     @ 06, 03 SAY 'Hasta Fecha:'  ;
       GET fecha2 RANGE fecha1
     @ 07, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 08, 03 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       opc PICTURE  ;
       '@m Detalle,Res£men'
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
            st_isrep.horemi,  ;
            st_isrep.codemi,  ;
            st_isrep.codent,  ;
            st_isrep.indori,  ;
            st_isrep.indest,  ;
            st_isrep.codmar,  ;
            st_isrep.codmod,  ;
            st_isrep.numser,  ;
            st_isrep.monabo,  ;
            st_isrep.observ,  ;
            st_isrep.desace,  ;
            st_isrep.coddes FROM  ;
            ST_ISREP WHERE  ;
            st_isrep.fecemi >=  ;
            fecha1 AND  ;
            st_isrep.fecemi <=  ;
            fecha2 AND  ;
            st_isrep.codemi =  ;
            marc AND  ;
            st_isrep.indest <>  ;
            'N' ORDER BY  ;
            st_isrep.indori,  ;
            st_isrep.fecemi INTO  ;
            CURSOR ING
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          IF opc = 'Detalle'
               REPORT FORMAT  ;
                      PORL4091 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4091  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          IF opc = 'Detalle'
               REPORT FORMAT  ;
                      PORL4091 TO  ;
                      FILE  ;
                      EMISOR.TXT  ;
                      ADDITIVE  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4091  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT1.TXT  ;
                      NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 TEXT1.TXT NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SET FILTER TO tab_codpre == 'EMIS'
titulo = 'AYUDA DE EMISORES'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
FUNCTION valta
PARAMETER clave, codig, colu,  ;
          largo
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
SELECT 7
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '** Codigo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
DO sacaf6
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
