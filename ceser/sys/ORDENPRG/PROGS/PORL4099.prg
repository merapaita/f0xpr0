*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   'ORDENES CON REPUESTOS POR MARCA '
STORE DATE() TO fecha1, fecha2
STORE 1 TO copia
STORE SPACE(4) TO marc
STORE 'Impresora' TO output
CLOSE DATABASES
ON KEY LABEL F6 do ayuda
STORE .T. TO pas
DO WHILE pas
     @ 07, 01 CLEAR TO 13, 77
     @ 3, 2 TO 8, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 03 SAY 'Marca      :'  ;
       GET marc VALID  ;
       valtab2('MARC',marc,30, ;
       30)
     @ 05, 03 SAY 'Desde Fecha:'  ;
       GET fecha1
     @ 06, 03 SAY 'Hasta Fecha:'  ;
       GET fecha2 RANGE fecha1
     @ 07, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep.numdoc,  ;
            st_iorep.fecemi,  ;
            st_iorep.indori,  ;
            st_iorep.indest,  ;
            st_iorep.auxest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.observ,  ;
            st_idped.numord,  ;
            st_idped.codpro,  ;
            st_idped.canpro,  ;
            gc_pro00.pro_descri,  ;
            gc_pro00.pro_codpro,  ;
            st_imode.codmod,  ;
            st_imode.nommod,  ;
            st_iprep.indest FROM  ;
            ST_IOREP, ST_IDPED,  ;
            GC_PRO00, ST_IMODE,  ;
            ST_IPREP WHERE  ;
            st_iorep.numdoc =  ;
            st_idped.numord AND  ;
            st_idped.numdoc =  ;
            st_iprep.numdoc AND  ;
            st_idped.codpro =  ;
            gc_pro00.pro_codpro  ;
            AND st_iorep.codmod =  ;
            st_imode.codmod AND  ;
            st_iorep.fecemi >=  ;
            fecha1 AND  ;
            st_iorep.fecemi <=  ;
            fecha2 AND  ;
            st_iorep.indest <>  ;
            'N   ' AND  ;
            st_iorep.indest <>  ;
            'A   ' AND  ;
            st_iprep.indest <>  ;
            'N   ' AND  ;
            st_iorep.codmar =  ;
            marc ORDER BY  ;
            st_iorep.codmod,  ;
            st_iorep.fecemi INTO  ;
            CURSOR QUERY
     IF output = 'Impresora'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          REPORT FORMAT PORL4099  ;
                 TO PRINTER  ;
                 NOCONSOLE
          SET PRINTER TO
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          REPORT FORMAT PORL4099  ;
                 TO FILE  ;
                 TEXT1.TXT  ;
                 NOCONSOLE
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
SELECT 7
USE SHARED ge_tab0 ORDER codigo
SET FILTER TO tab_codpre == 'MARC'
titulo = 'AYUDA DE MARCAS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
