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
   ' RECEPCIONADOS POR ZONAS '
SELECT 1
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 3
USE SHARED ST_ISREP ORDER  ;
    SOL_FCHEMI
SELECT 5
USE ST_IMODE ORDER CODIGO
STORE SPACE(4) TO emisor1,  ;
      emisor2
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE DATE() TO fecha1, fecha2
STORE '00:00:00' TO hora1, hora2
ON KEY LABEL f6 do ayuda
@ 3, 2 TO 9, 77
DO esc_modo WITH 'S'
@ 04, 30 SAY SPACE(30)
sigue = .T.
DO WHILE sigue
     @ 07, 01 CLEAR TO 13, 77
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
       fecha1
     @ 04, 30 SAY  ;
       'Hasta fecha  :' GET  ;
       fecha2 RANGE fecha1
     @ 05, 03 SAY  ;
       'Desde Hora   :' GET hora1  ;
       PICTURE '99:99:99' VALID  ;
       hora1 > '00:00:00:'
     @ 05, 30 SAY  ;
       'Hasta Hora   :' GET hora2  ;
       RANGE hora1 PICTURE  ;
       '99:99:99' VALID hora2 >  ;
       '00:00:00:'
     @ 06, 03 SAY  ;
       'Desde Emisor :' GET  ;
       emisor1 PICTURE '@!' VALID  ;
       valida(emisor1,1) WHEN  ;
       antes(1)
     @ 07, 03 SAY  ;
       'Hasta Emisor :' GET  ;
       emisor2 RANGE emisor1  ;
       PICTURE '@!' VALID  ;
       valida(emisor2,1) WHEN  ;
       antes(1)
     @ 08, 03 SAY  ;
       'Por Pantalla/Impresora:'  ;
       GET output PICTURE  ;
       '@m Pantalla,Impresora'  ;
       WHEN antes(2)
     @ 09, 03 SAY  ;
       'Por Detalle/Res�men:' GET  ;
       tipo PICTURE  ;
       '@m Detalle,Res�men'
     READ
     IF LASTKEY() = 27
          sigue = .F.
          LOOP
     ENDIF
     IF output = 'Impresora'
          nco = 1
          @ 09, 40 SAY  ;
            'Copias Nro.:' GET  ;
            nco PICTURE '99'  ;
            VALID nco >= 0
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     CREATE CURSOR R_DOMI (numdoc  ;
            C (8), fecemi D,  ;
            horemi C (8), codemi  ;
            C (4), codent C (11),  ;
            indori C (4), indest  ;
            C (4), codmar C (4),  ;
            codmod C (15), numser  ;
            C (20), observ MEMO,  ;
            coddis C (4), nomdis  ;
            C (30), zona C (4),  ;
            nomcli C (30), linea  ;
            C (4))
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SET NEAR ON
     SELECT st_isrep
     SEEK dtoc2(fecha1)
     SET NEAR OFF
     SCAN WHILE fecemi >= fecha1  ;
          .AND. fecemi <= fecha2  ;
          .AND.  .NOT. EOF()
          IF coddes = 'D' .AND.  ;
             (codemi >= emisor1  ;
             .AND. codemi <=  ;
             emisor2) .AND.  ;
             (horemi >= hora1  ;
             .AND. horemi <=  ;
             hora2) .AND. indest <>  ;
             'N  '
               w_reg = w_reg + 1
               SELECT r_domi
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_isrep.numdoc,  ;
                       fecemi  ;
                       WITH  ;
                       st_isrep.fecemi
               REPLACE horemi  ;
                       WITH  ;
                       st_isrep.horemi,  ;
                       codemi  ;
                       WITH  ;
                       st_isrep.codemi
               REPLACE codent  ;
                       WITH  ;
                       st_isrep.codent,  ;
                       indori  ;
                       WITH  ;
                       st_isrep.indori
               REPLACE indest  ;
                       WITH  ;
                       st_isrep.indest,  ;
                       codmar  ;
                       WITH  ;
                       st_isrep.codmar
               REPLACE codmod  ;
                       WITH  ;
                       st_isrep.codmod,  ;
                       numser  ;
                       WITH  ;
                       st_isrep.numser
               REPLACE observ  ;
                       WITH  ;
                       st_isrep.observ
               SELECT st_iclpr
               SEEK 'C' +  ;
                    r_domi.codent
               IF FOUND()
                    SELECT r_domi
                    REPLACE nomcli  ;
                            WITH  ;
                            st_iclpr.noment
                    REPLACE coddis  ;
                            WITH  ;
                            st_iclpr.nomdis
               ENDIF
               SELECT ge_tab0
               SEEK 'DIST' +  ;
                    r_domi.coddis
               IF FOUND()
                    SELECT r_domi
                    REPLACE nomdis  ;
                            WITH  ;
                            ge_tab0.tab_destab
                    REPLACE zona  ;
                            WITH  ;
                            ge_tab0.tab_parame
               ENDIF
               SELECT st_imode
               SEEK st_isrep.codmar +  ;
                    st_isrep.codmod
               IF FOUND()
                    SELECT r_domi
                    REPLACE linea  ;
                            WITH  ;
                            st_imode.linea
               ENDIF
          ENDIF
          SELECT st_isrep
     ENDSCAN
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     IF w_reg = 0
          DO error WITH  ;
             '*** No hay informaci�n para el Reporte ***'
     ELSE
          filidx = SYS(3)
          SELECT r_domi
          GOTO TOP
          index on LINEA + codemi + zona;
+ INDORI + coddis + numdoc to &filidx;
 
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
                       'Res�men'
                         REPORT FORMAT  ;
                                porl4081  ;
                                SUMMARY  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                porl4081  ;
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
                  'Res�men'
                    repo form porl4081;
to file &FILTXT noconsole summary
               ELSE
                    repo form porl4081;
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
        '*** C�digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), 25 SAY tab_destab
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT ge_tab0
SET FILTER TO tab_codpre == 'EMIS'
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE EMISOR'
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
     ON KEY LABEL F6 DO AYUDA
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ELSE
     ON KEY LABEL F6
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
