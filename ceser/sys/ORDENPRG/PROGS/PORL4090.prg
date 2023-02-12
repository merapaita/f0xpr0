*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_fecdoc
ON KEY LABEL F6 DO AYUDA
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ARTICULOS POR TIPO DE ATENCION '
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
     CREATE CURSOR gara (numdoc C  ;
            (8), fecemi D, codemi  ;
            C (4), codent C (11),  ;
            indori C (4), indest  ;
            C (4), auxest C (4),  ;
            codmar C (4), codmod  ;
            C (15), numser C (20),  ;
            codtec C (9), numsol  ;
            C (8), fecest D,  ;
            numfabo C (10),  ;
            fecfabo D, empres C  ;
            (25))
     SELECT st_iorep
     SET NEAR ON
     SEEK DTOS(fecha1)
     SET NEAR OFF
     SCAN WHILE fecemi >= fecha1  ;
          .AND. fecemi <= fecha2  ;
          .AND.  .NOT. EOF()
          IF (codemi >= emisor1  ;
             .AND. codemi <=  ;
             emisor2) .AND.  ;
             (indori >= tipgar1  ;
             .AND. indori <=  ;
             tipgar2) .AND.  ;
             indest <> 'N'
               SELECT gara
               APPEND BLANK
               REPLACE numdoc  ;
                       WITH  ;
                       st_iorep.numdoc
               REPLACE fecemi  ;
                       WITH  ;
                       st_iorep.fecemi
               REPLACE codemi  ;
                       WITH  ;
                       st_iorep.codemi
               REPLACE codent  ;
                       WITH  ;
                       st_iorep.codent
               REPLACE indori  ;
                       WITH  ;
                       st_iorep.indori
               REPLACE indest  ;
                       WITH  ;
                       st_iorep.indest
               REPLACE auxest  ;
                       WITH  ;
                       st_iorep.auxest
               REPLACE codmar  ;
                       WITH  ;
                       st_iorep.codmar
               REPLACE codmod  ;
                       WITH  ;
                       st_iorep.codmod
               REPLACE codtec  ;
                       WITH  ;
                       st_iorep.codtec
               REPLACE numsol  ;
                       WITH  ;
                       st_iorep.numsol
               REPLACE fecest  ;
                       WITH  ;
                       st_iorep.fecest
               REPLACE numfabo  ;
                       WITH  ;
                       st_iorep.numfabo
               REPLACE fecfabo  ;
                       WITH  ;
                       st_iorep.fecfabo
               REPLACE numser  ;
                       WITH  ;
                       st_iorep.numser
               SELECT ge_tab0
               SEEK 'EMIS' +  ;
                    st_iorep.codemi
               IF FOUND()
                    SELECT gara
                    REPLACE empres  ;
                            WITH  ;
                            ge_tab0.tab_empres
               ENDIF
               SELECT st_iorep
          ENDIF
     ENDSCAN
     w_ind = f_indice() + '.idx'
     SELECT gara
     index on indori+empres+dtos(fecemi)+numdoc;
to &w_ind
     GOTO TOP
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
                      PORL4090  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      PORL4090 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          ??? CHR(80)
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
          SET PRINTER TO
     ELSE
          w_texto = f_texto() +  ;
                    '.txt'
          IF tipo = 'Res£men'
               REPO FORM  PORL4090 TO;
FILE &w_texto NOCONSOLE SUMMARY
          ELSE
               REPO FORM  PORL4090 TO;
FILE &w_texto NOCONSOLE 
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &w_texto window pantall;
noedit
          SET SYSMENU OFF
          delete file &w_texto
     ENDIF
ENDDO
CLOSE DATABASES
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
SET FILTER TO
SET ORDER TO codigo
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
