*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
tit_prg = ' CONSULTA '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PENDIENTES POR TECNICO '
DO esc_modo WITH 'C'
DEFINE POPUP detalle FROM 10, 02  ;
       TO 16, 74 PROMPT FIELDS  ;
       numord + ' ' +  ;
       DTOC(fecdoc) + ' ' +  ;
       SUBSTR(descli, 1, 18) +  ;
       ' ' + indori + '  ' +  ;
       SUBSTR(codmod, 1, 10) +  ;
       ' ' + SUBSTR(numser, 1,  ;
       10) + ' ' + codtal TITLE  ;
       'O/R컴컴횲.Creac.컴횮liente컴컴컴컴컴횳tia.횺odelo컴컴훁erie컴컴Taller'  ;
       IN screen COLOR SCHEME 8
SELECT 1
USE ST_IOREP ORDER ORD_TECEST
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE ST_ICLPR ORDER CODIGO
SELECT 4
USE ST_ITECN ORDER CODIGO
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE SPACE(4) TO wrk_estado
     STORE 0 TO wrk_codtec
     @ 04, 01 CLEAR TO 07, 73
     @ 04, 01 TO 07, 73
     @ 05, 04 SAY  ;
       'T괹nico....... :'
     @ 06, 04 SAY  ;
       'Estado........ :'
     ON KEY
     SET CURSOR ON
     @ 05, 22 GET wrk_codtec  ;
       PICTURE '999999999' VALID  ;
       despues(1) WHEN antes(1)
     @ 06, 22 GET wrk_estado  ;
       PICTURE '@!' VALID  ;
       despues(2) WHEN antes(2)
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDDO
CLOSE DATABASES
DO saca_win
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
          IF EMPTY(wrk_codtec)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_itecn
          SET ORDER TO 1
          SEEK STR(wrk_codtec, 9)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C줰igo de T괹nico No Existe'
               RETURN .F.
          ENDIF
          @ 05, 33 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
     CASE opc = 2
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_estado)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'ESOR' +  ;
               wrk_estado
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C줰igo de Estado No Existe'
               RETURN .F.
          ENDIF
          @ 06, 28 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            30)
          ON KEY LABEL F7 DO IMPRIME
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'IMP', 'BBB',  ;
             'ESC'
          DO mensa WITH  ;
             '*** Espere un momento, por favor... ***',  ;
             'COLO'
          CREATE CURSOR PEND  ;
                 (numord C (8),  ;
                 numsol C (8),  ;
                 fecdoc D, fecest  ;
                 D, indori C (4),  ;
                 descli C (30),  ;
                 codmar C (4),  ;
                 codmod C (15),  ;
                 numser C (15),  ;
                 codtal C (4))
          SELECT st_iorep
          SET NEAR ON
          SEEK STR(wrk_codtec, 9) +  ;
               wrk_estado
          SET NEAR OFF
          DO WHILE codtec= ;
             STR(wrk_codtec, 9)
               IF auxest =  ;
                  wrk_estado  ;
                  .AND. indest <>  ;
                  'N' .AND.   ;
                  .NOT. EOF()
                    wrk_numord = numdoc
                    wrk_numsol = numsol
                    wrk_fecdoc = fecemi
                    wrk_fecest = fecest
                    wrk_codent = codent
                    wrk_codtal = codtall
                    wrk_codmar = codmar
                    wrk_codmod = codmod
                    wrk_numser = numser
                    wrk_indori = indori
                    SELECT pend
                    APPEND BLANK
                    REPLACE numord  ;
                            WITH  ;
                            wrk_numord
                    REPLACE numsol  ;
                            WITH  ;
                            wrk_numsol
                    REPLACE fecdoc  ;
                            WITH  ;
                            wrk_fecdoc
                    REPLACE fecest  ;
                            WITH  ;
                            wrk_fecest
                    REPLACE codmar  ;
                            WITH  ;
                            wrk_codmar
                    REPLACE codmod  ;
                            WITH  ;
                            wrk_codmod
                    REPLACE numser  ;
                            WITH  ;
                            wrk_numser
                    REPLACE codtal  ;
                            WITH  ;
                            wrk_codtal
                    REPLACE indori  ;
                            WITH  ;
                            wrk_indori
                    REPLACE descli  ;
                            WITH  ;
                            oodescli(wrk_codent)
               ENDIF
               SELECT st_iorep
               SKIP
          ENDDO
          DO mensa WITH  ;
             '*** Espere un momento, por favor... ***',  ;
             'SACA'
          SELECT pend
          DO WHILE LASTKEY()<>27
               ACTIVATE POPUP  ;
                        detalle
          ENDDO
          KEYBOARD '{ENTER}'
ENDCASE
*
PROCEDURE ayuda
SELECT st_itecn
campoa = '" "+codent+" "+noment+" "+CODTEC'
campob = '" "+noment+" "+codent+" "+CODTEC'
titulo = 'AYUDA DE TECNICOS'
DO ayuda2 WITH campoa, campob,  ;
   titulo,  ;
   'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
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
*
PROCEDURE imprime
SELECT pend
COUNT TO a
IF a = 0
     DO error WITH  ;
        '*** No existen registros a Listar ***'
     RETURN
ENDIF
SET SYSMENU ON
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
DO esc_modo WITH 'P'
DO mensa WITH  ;
   '*** I m p r i m i e n d o ... ***',  ;
   'COLO'
??? CHR(15)
REPORT FORMAT PORQ0379 TO PRINTER  ;
       NOCONSOLE
DO mensa WITH  ;
   '*** I m p r i m i e n d o ... ***',  ;
   'SACA'
*
*** 
*** ReFox - retrace your steps ... 
***
