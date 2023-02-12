*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET SYSMENU ON
CLOSE DATABASES
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
tit_prg = ' CONSULTA '
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONSUMO DE REPUESTOS '
DEFINE POPUP detalle FROM 11, 02  ;
       TO 16, 74 PROMPT FIELDS  ;
       codpro + ' ' +  ;
       SUBSTR(oodespro(codpro), 1,  ;
       15) + ' ' +  ;
       TRANSFORM(cantid, '999') +  ;
       ' ' + TRANSFORM(preite,  ;
       '99,999.99') + ' ' +  ;
       TRANSFORM(totite,  ;
       '99,999.99') + numped +  ;
       ' ' + DTOC(fecha) TITLE  ;
       'Producto컴컴컴Descripci줻컴컴Cant.컴횾recio컴컴컴Total컴횾ed.컴Fecha컴'  ;
       IN screen COLOR SCHEME 8
SELECT 1
USE ST_IOREP ORDER CODIGO
SELECT 2
USE ST_IPREP ORDER CODIGO
SELECT 3
USE ST_IDPED ORDER DRE_NUMORD
SELECT 4
USE ST_ICLPR ORDER CODIGO
SELECT 5
USE GE_TAB0 ORDER CODIGO
valor = .T.
DO WHILE valor
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE SPACE(1) TO wrk_tipdoc
     STORE 0 TO wrk_numord
     @ 03, 01 CLEAR TO 10, 73
     @ 03, 01 TO 10, 73
     @ 04, 04 SAY  ;
       'N즡ero ....... :'
     @ 04, 43 SAY  ;
       'Fecha ....... :'
     @ 05, 04 SAY  ;
       'Cliente ...... :'
     @ 06, 04 SAY  ;
       'Repuestos .... :'
     @ 07, 04 SAY  ;
       'M. de Obra ... :'
     @ 08, 04 SAY  ;
       'Flete ........ :'
     @ 06, 43 SAY  ;
       'Total ....... :'
     @ 07, 43 SAY  ;
       'IGV ......... :'
     @ 08, 43 SAY  ;
       'T. General .. :'
     SET CURSOR ON
     @ 04, 22 GET wrk_numord  ;
       PICTURE '99999999' VALID  ;
       despues(1) WHEN antes(1)
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     ON KEY
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE .T.
          = INKEY(0, 'H')
          IF LASTKEY() = 27
               EXIT
          ENDIF
     ENDDO
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
ENDCASE
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_numord)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT st_iorep
          SET ORDER TO CODIGO
          SEEK STR(wrk_numord, 8)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'Nro. de Orden No Existe'
               RETURN .F.
          ENDIF
          @ 04, 63 SAY fecemi
          @ 06, 22 SAY cosrep  ;
            PICTURE  ;
            '$999,999.99'
          @ 06, 60 SAY totnet  ;
            PICTURE  ;
            '$999,999.99'
          @ 07, 22 SAY cosmob  ;
            PICTURE  ;
            '$999,999.99'
          @ 07, 60 SAY totigv  ;
            PICTURE  ;
            '$999,999.99'
          @ 08, 22 SAY flete  ;
            PICTURE  ;
            '$999,999.99'
          @ 08, 60 SAY totbru  ;
            PICTURE  ;
            '$999,999.99'
          wrk_estado = auxest
          wrk_codcli = codent
          wrk_fecest = fecest
          SELECT ge_tab0
          SEEK 'ESOR' +  ;
               wrk_estado
          IF FOUND()
               @ 03, 03 SAY  ;
                 ALLTRIM(tab_destab)  ;
                 COLOR N/W* 
               @ 03, 64 SAY  ;
                 wrk_fecest COLOR  ;
                 N/W* 
          ENDIF
          SELECT st_iclpr
          SEEK 'C' + wrk_codcli
          IF  .NOT. FOUND()
               wrk_codcli = SPACE(9)
          ELSE
               wrk_codcli = noment
          ENDIF
          @ 05, 22 SAY  ;
            SUBSTR(wrk_codcli, 1,  ;
            30)
          wrk_file = f_archivo()
          CREA TABLE  &wrk_file (NUMPED;
C(8),   NUMORD C(8),   CODPRO C(14), CANTID;
N(4,0), PREITE N(9,2), TOTITE N(9,2),;
FECHA D) 
          SELECT 6
          USE &wrk_file
          SELECT st_idped
          SEEK STR(wrk_numord, 8)
          IF FOUND()
               DO WHILE numord= ;
                  STR(wrk_numord,  ;
                  8)
                    wrk_numped = numdoc
                    SELECT st_iprep
                    SEEK wrk_numped
                    IF FOUND()
                         IF indest <>  ;
                            'N'
                              SELECT  ;
                               st_idped
                              wrk_codpro =  ;
                               codpro
                              wrk_cantid =  ;
                               canpro
                              wrk_import =  ;
                               valpro
                              wrk_total =  ;
                               totite
                              wrk_fecha =  ;
                               date
                              SELECT  ;
                               6
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               numped  ;
                               WITH  ;
                               wrk_numped
                              REPLACE  ;
                               numord  ;
                               WITH  ;
                               STR(wrk_numord,  ;
                               8)
                              REPLACE  ;
                               codpro  ;
                               WITH  ;
                               wrk_codpro
                              REPLACE  ;
                               cantid  ;
                               WITH  ;
                               wrk_cantid
                              REPLACE  ;
                               preite  ;
                               WITH  ;
                               wrk_import
                              REPLACE  ;
                               totite  ;
                               WITH  ;
                               wrk_total
                              REPLACE  ;
                               fecha  ;
                               WITH  ;
                               wrk_fecha
                         ENDIF
                    ENDIF
                    SELECT st_idped
                    SKIP
               ENDDO
          ENDIF
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'IMP',  ;
             'BBB'
          ON KEY LABEL f7 do imprime
          SELECT 6
          DO WHILE LASTKEY()<>27
               ACTIVATE POPUP  ;
                        detalle
          ENDDO
          SELECT 6
          USE
          ERASE FILE &wrk_file
          KEYBOARD '{ENTER}'
ENDCASE
RETURN
*
PROCEDURE ayuda
ON KEY
SELECT st_iorep
wrk_origen = 'OR'
campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
DO ayuda4 WITH campoa, wrk_origen
ON KEY LABEL F6 do ayuda
RETURN
*
PROCEDURE imprime
DO mensa WITH  ;
   '*** I m p r i m i e n d o ... ***',  ;
   'COLO'
??? CHR(15)
REPORT FORMAT porq0377 TO PRINTER  ;
       NOCONSOLE
DO mensa WITH  ;
   '*** I m p r i m i e n d o ... ***',  ;
   'SACA'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
