*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'ESTADO DE ARTICULOS POR MARCA (ESTADO ACTUAL)'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BUS', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
STORE DATE() TO fecha1, fecha2
STORE 'Impresora' TO output
STORE 1 TO copia
STORE SPACE(4) TO marc, esta
CLOSE DATABASES
SELECT 7
USE SHARED ge_tab0 ORDER codigo
ON KEY LABEL F6 do ayuda
pas = .T.
DO WHILE pas
     @ 07, 01 CLEAR TO 13, 77
     @ 03, 02 TO 07, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_key = 0
     SET CURSOR ON
     @ 04, 04 SAY 'Marca      :'  ;
       GET marc VALID  ;
       valda('MARC',marc,23,17)
     @ 04, 40 SAY 'Estado:' GET  ;
       esta VALID valda('ESOR', ;
       esta,53,20)
     @ 05, 04 SAY 'Desde Fecha:'  ;
       GET fecha1
     @ 05, 40 SAY 'Hasta Fecha:'  ;
       GET fecha2 RANGE fecha1
     @ 06, 04 SAY  ;
       'Por Impresora/Pantalla:'  ;
       GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          DO saca_win
          RETURN
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT DISTINCT  ;
            st_iorep.codemi,  ;
            st_iorep.indori,  ;
            st_iorep.auxest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.fecest FROM  ;
            ST_IOREP WHERE  ;
            st_iorep.codmar =  ;
            marc AND  ;
            st_iorep.auxest =  ;
            esta AND  ;
            st_iorep.fecemi >=  ;
            fecha1 AND  ;
            st_iorep.fecemi <=  ;
            fecha2 ORDER BY  ;
            st_iorep.codmod,  ;
            st_iorep.indori INTO  ;
            CURSOR QUERY
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          REPORT FORMAT PORL4072  ;
                 TO PRINTER  ;
                 NOCONSOLE
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          REPORT FORMAT PORL4072  ;
                 TO FILE  ;
                 text3.txt  ;
                 NOCONSOLE
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODIFY COMMAND  ;
                 TEXT3.TXT NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT 7
IF COL() < 40
     SET FILTER TO tab_codpre == 'MARC'
     titulo = 'AYUDA DE MARCAS'
ELSE
     SET FILTER TO tab_codpre == 'ESOR'
     titulo = 'AYUDA DE ESTADOS'
ENDIF
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
FUNCTION valda
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
