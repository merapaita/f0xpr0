*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ORDENES DE REPARACION NO CERRADAS '
SELECT 1
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED ST_ISREP ORDER  ;
    SOL_FCHEMI
SELECT 4
USE SHARED ST_IOREP ORDER  ;
    ORD_FECDOC
STORE SPACE(4) TO taller1,  ;
      taller2, emisor1, emisor2
STORE SPACE(8) TO filtxt
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
STORE '00:00:00' TO hora1, hora2
@ 3, 2 TO 9, 77
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
sigue = .T.
DO WHILE sigue
     @ 08, 01 CLEAR TO 13, 77
     @ 3, 2 TO 10, 77
     @ 04, 30 SAY SPACE(30)
     @ 04, 05 SAY SPACE(50)
     STORE 0 TO nco, w_reg
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     SET CURSOR ON
     @ 04, 03 SAY  ;
       'Desde Fecha  :' GET  ;
       fecha1 WHEN antes(2)
     @ 04, 30 SAY  ;
       'Hasta fecha  :' GET  ;
       fecha2 RANGE fecha1 WHEN  ;
       antes(2)
     @ 05, 03 SAY  ;
       'Desde Emisor :' GET  ;
       emisor1 PICTURE '9999'  ;
       VALID valida(emisor1,1)  ;
       WHEN antes(1)
     @ 06, 03 SAY  ;
       'Hasta Emisor :' GET  ;
       emisor2 RANGE emisor1  ;
       PICTURE '9999' VALID  ;
       valida(emisor2,1) WHEN  ;
       antes(1)
     @ 07, 03 SAY  ;
       'Desde Taller :' GET  ;
       taller1 PICTURE '9999'  ;
       VALID valtab2('TALL', ;
       taller1,23,15) WHEN  ;
       antes(1)
     @ 07, 38 SAY  ;
       'Hasta Taller :' GET  ;
       taller2 RANGE taller1  ;
       PICTURE '9999' VALID  ;
       valtab2('TALL',taller2,58, ;
       15) WHEN antes(1)
     @ 08, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'  ;
       WHEN antes(2)
     @ 09, 03 SAY  ;
       'Por Detalle/Res£men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res£men' WHEN  ;
       antes(2)
     READ
     IF LASTKEY() = 27
          sigue = .F.
          LOOP
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     IF output = 'Impresora'
          nco = 1
          @ 09, 40 SAY  ;
            'Copias Nro.:' GET  ;
            nco PICTURE '99'  ;
            VALID nco >= 0 WHEN  ;
            antes(2)
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     CREATE CURSOR DOMI3 (numdoc  ;
            C (8), fecemi D,  ;
            codemi C (4), codent  ;
            C (11), indori C (4),  ;
            indest C (4), codmar  ;
            C (4), codmod C (15),  ;
            numser C (20), nomcli  ;
            C (30), telcli1 N (8),  ;
            telcli2 N (8), auxest  ;
            C (4), numsol C (8),  ;
            codtall C (4))
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SET NEAR ON
     SELECT st_iorep
     SEEK DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE fecemi >= fecha1  ;
          .AND. fecemi <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (codtall >= taller1  ;
             .AND. codtall <=  ;
             taller2) .AND.  ;
             (codemi >= emisor1  ;
             .AND. codemi <=  ;
             emisor2) .AND.  ;
             (indest <> 'C   '  ;
             .AND. indest <>  ;
             'N   ' .AND. indest <>  ;
             'F   ' .AND. indest <>  ;
             'B   ')
               w_reg = w_reg + 1
               SELECT domi3
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc,  ;
                       fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi,  ;
                       codent  ;
                       WITH  ;
                       st_iorep.codent
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori,  ;
                       indest  ;
                       WITH  ;
                       st_iorep.indest
               REPLACE codmar  ;
                       WITH  ;
                       st_iorep.codmar,  ;
                       codmod  ;
                       WITH  ;
                       st_iorep.codmod
               REPLACE numser  ;
                       WITH  ;
                       st_iorep.numser,  ;
                       auxest  ;
                       WITH  ;
                       st_iorep.auxest
               REPLACE numsol  ;
                       WITH  ;
                       st_iorep.numsol,  ;
                       codtall  ;
                       WITH  ;
                       st_iorep.codtall
               SELECT st_iclpr
               SEEK 'C' +  ;
                    domi3.codent
               IF FOUND()
                    SELECT domi3
                    REPLACE nomcli  ;
                            WITH  ;
                            st_iclpr.noment
                    IF  .NOT.  ;
                        EMPTY(st_iclpr.numte1)
                         REPLACE telcli1  ;
                                 WITH  ;
                                 st_iclpr.numte1
                    ENDIF
                    IF  .NOT.  ;
                        EMPTY(st_iclpr.numte2)
                         REPLACE telcli2  ;
                                 WITH  ;
                                 st_iclpr.numte2
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     IF w_reg = 0
          DO error WITH  ;
             '*** No hay informaci¢n para el Reporte ***'
     ELSE
          filidx = SYS(3)
          SELECT domi3
          INDEX ON  CODEMI+AUXEST+INDORI+NUMDOC;
to &filidx
          GOTO TOP
          IF output = 'Impresora'
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               SET DEVICE TO PRINTER
               SET PRINTER ON
               ?? CHR(27) +  ;
                  CHR(15)
               FOR i = 1 TO nco
                    IF tipo =  ;
                       'Res£men'
                         REPORT FORMAT  ;
                                porl4083  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl4083  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDFOR
               ?? CHR(27) +  ;
                  CHR(18)
               SET PRINTER OFF
               SET DEVICE TO SCREEN
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ELSE
               filtxt = SYS(3) +  ;
                        '.TXT'
               IF tipo =  ;
                  'Res£men'
                    repo form PORL4083;
to file &FILTXT noconsole summary
               ELSE
                    repo form PORL4083;
to file &FILTXT noconsole 
               ENDIF
               DO mensa WITH  ;
                  '** Un momento, Por Favor ... **',  ;
                  'SACA'
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &FILTXT;
 window pantall NOEDIT
               DELE FILE &FILTXT
               SET SYSMENU OFF
          ENDIF
     ENDIF
ENDDO
DO sacawin
CLOSE DATABASES
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
@ ROW(), 23 SAY tab_destab
RETURN
*
FUNCTION valtab2
PARAMETER clave, codig, colu,  ;
          largo
SELECT ge_tab0
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** C¢digo de Tabla no Existe **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
RETURN .T.
*
PROCEDURE ayuda
PARAMETER var
SELECT ge_tab0
IF VARREAD() = 'EMISOR1' .OR.  ;
   VARREAD() = 'EMISOR2'
     SET FILTER TO tab_codpre == 'EMIS'
     titulo = 'AYUDA DE EMISOR'
ENDIF
IF VARREAD() = 'TALLER'
     SET FILTER TO tab_codpre == 'TALL'
     titulo = 'AYUDA DE TALLER'
ENDIF
campo = 'tab_codtab + "  " + tab_destab'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'ESC'
RETURN
*
PROCEDURE antes
PARAMETER opc
IF opc = 1
     ON KEY LABEL f6 do ayuda
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ELSE
     ON KEY LABEL f6
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
