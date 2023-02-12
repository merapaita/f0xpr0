*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = PROGRAM()
tit_prg = ' REPORTES '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PENDIENTES POR CLIENTE '
CLOSE DATABASES
SELECT 1
USE ST_IOREP ORDER CODENT
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE ST_ICLPR ORDER CODENT
SELECT 4
USE ST_ITECN ORDER CODIGO
STORE 0 TO codcli1, codcli2
STORE 1 TO w_copia
STORE SPACE(4) TO estado1,  ;
      estado2
STORE 'Detalle' TO tipo
STORE 'Impresora' TO output
STORE DATE() TO fecha1, fecha2
SET SYSMENU ON
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 01 CLEAR TO 07, 73
     @ 04, 01 TO 11, 73
     SET CURSOR ON
     @ 05, 03 SAY 'De Cliente :'  ;
       GET codcli1 PICTURE  ;
       '99999999999' VALID  ;
       despues(1) WHEN antes(1)
     @ 06, 03 SAY 'A  Cliente :'  ;
       GET codcli2 PICTURE  ;
       '99999999999' VALID  ;
       despues(2) WHEN antes(1)
     @ 08, 03 SAY 'Del Fecha  :'  ;
       GET fecha1
     @ 08, 40 SAY 'Al :' GET  ;
       fecha2 RANGE fecha1
     @ 09, 03 SAY  ;
       'Por Detalle/Res£men    :'  ;
       GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 10, 03 SAY  ;
       'Por Impresora/Pantalla :'  ;
       GET output PICTURE  ;
       '@m Impresora,Pantalla'
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'COLO'
     SELECT 10
     CREATE CURSOR cliente  ;
            (tnumord C (8),  ;
            tfecdoc D, tcodtal C  ;
            (4), tindori C (4),  ;
            tcodemi C (4),  ;
            tfecest D, tcodest C  ;
            (4), tindest C (1),  ;
            tcodtab C (1),  ;
            tcodmar C (4),  ;
            tcodmod C (15),  ;
            tnumser C (20),  ;
            tcodent C (11))
     INDEX ON tcodent + tcodtab +  ;
           tcodest + tindori TAG  ;
           codigo
     a = 0
     b = 0
     SELECT st_iorep
     SET NEAR ON
     SEEK STR(codcli1, 11)
     SET NEAR OFF
     SCAN WHILE codent <=  ;
          STR(codcli2, 11) .AND.   ;
          .NOT. EOF()
          a = a + 1
          @ 00, 00 SAY a
          IF indest <> 'N   '  ;
             .AND. (codent >=  ;
             STR(codcli1, 11)  ;
             .AND. codent <=  ;
             STR(codcli2, 11))
               IF codtall <  ;
                  '010 ' .OR.  ;
                  (codtall >  ;
                  '050 ' .AND.  ;
                  codtall <  ;
                  '060 ')
                    w_estado = auxest
                    SELECT ge_tab0
                    SEEK 'ESOR' +  ;
                         w_estado
                    IF SUBSTR(tab_parame,  ;
                       1) = 'T'  ;
                       .OR.  ;
                       SUBSTR(tab_parame,  ;
                       1) = 'A'
                         w_codtab =  ;
                          SUBSTR(tab_parame,  ;
                          1)
                         b = b +  ;
                             1
                         @ 00, 40  ;
                           SAY b
                         SELECT st_iorep
                         w_numord =  ;
                          numdoc
                         w_fecdoc =  ;
                          fecemi
                         w_codtal =  ;
                          codtall
                         w_indori =  ;
                          indori
                         w_codemi =  ;
                          codemi
                         w_fecest =  ;
                          fecest
                         w_codest =  ;
                          auxest
                         w_indest =  ;
                          indest
                         w_codmar =  ;
                          codmar
                         w_codmod =  ;
                          codmod
                         w_numser =  ;
                          numser
                         w_codent =  ;
                          codent
                         SELECT cliente
                         APPEND BLANK
                         REPLACE tnumord  ;
                                 WITH  ;
                                 w_numord,  ;
                                 tfecdoc  ;
                                 WITH  ;
                                 w_fecdoc
                         REPLACE tcodtal  ;
                                 WITH  ;
                                 w_codtal,  ;
                                 tindori  ;
                                 WITH  ;
                                 w_indori
                         REPLACE tcodemi  ;
                                 WITH  ;
                                 w_codemi,  ;
                                 tfecest  ;
                                 WITH  ;
                                 w_fecest
                         REPLACE tcodest  ;
                                 WITH  ;
                                 w_codest,  ;
                                 tindest  ;
                                 WITH  ;
                                 w_indest
                         REPLACE tcodtab  ;
                                 WITH  ;
                                 w_codtab,  ;
                                 tcodmar  ;
                                 WITH  ;
                                 w_codmar
                         REPLACE tcodmod  ;
                                 WITH  ;
                                 w_codmod,  ;
                                 tnumser  ;
                                 WITH  ;
                                 w_numser
                         REPLACE tcodent  ;
                                 WITH  ;
                                 w_codent
                    ENDIF
               ENDIF
          ENDIF
          SELECT st_iorep
     ENDSCAN
     SELECT cliente
     COUNT TO w_valor
     IF w_valor = 0
          DO mensa WITH  ;
             '***  Un momento, Por Favor ...  ***',  ;
             'SACA'
          DO error WITH  ;
             '***  No Existen registros a Listar  ***'
          LOOP
     ENDIF
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'SACA'
     DO esc_indica WITH 2, 'BBB',  ;
        'VER', 'IMP', 'ESC'
     IF output = 'Impresora'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          FOR a = 1 TO w_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(15)
               IF tipo =  ;
                  'Resumen'
                    REPORT FORMAT  ;
                           PORL4003  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           PORL4003  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
               SET PRINTER TO
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
     ENDIF
     IF output = 'Pantalla'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'COLO'
          w_file02 = SUBSTR(f_archivo(),  ;
                     1, 8) +  ;
                     '.TXT'
          IF tipo = 'Resumen'
               REPO FORM PORL4003 TO FILE;
 &w_file02  SUMMARY NOCONSOLE
          ELSE
               REPO FORM PORL4003 TO FILE;
 &w_file02  NOCONSOLE
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          MODI COMM &w_file02 NOEDIT WINDOW;
PANTALL
          SET SYSMENU OFF
          DELE FILE &w_file02
     ENDIF
     EXIT
ENDDO
CLOSE DATABASES
DO saca_win
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          ON KEY LABEL F6 DO AYUDA02
ENDCASE
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(codcli1)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_iclpr
          SEEK STR(codcli1, 11)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Cliente No Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 30 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
     CASE opc = 2
          ON KEY
          IF EMPTY(codcli2)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_iclpr
          SEEK STR(codcli2, 11)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Cliente No Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 30 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
ENDCASE
RETURN
*
PROCEDURE ayuda
RETURN
SELECT st_iclpr
campoa = '" "+codent+" "+noment+" "+CODTEC'
campob = '" "+noment+" "+codent+" "+CODTEC'
titulo = 'AYUDA DE TECNICOS'
DO ayuda2 WITH campoa, campob,  ;
   titulo,  ;
   'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
RETURN
*
PROCEDURE ayuda02
SELECT ge_tab0
SET FILTER TO tab_codpre == 'ESOR'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA ESTADO OPERACION'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
