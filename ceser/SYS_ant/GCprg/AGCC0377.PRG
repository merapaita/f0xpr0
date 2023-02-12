*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
SET SYSMENU ON
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 NONE COLOR  ;
       SCHEME 7
DEFINE POPUP detalle FROM 12, 03  ;
       TO 20, 76 PROMPT FIELDS  ;
       codpro + ' ' +  ;
       SUBSTR(oodespro(codpro), 1,  ;
       15) + ' ' +  ;
       TRANSFORM(cantid, '999') +  ;
       ' ' + TRANSFORM(preite,  ;
       '99,999.99') + ' ' +  ;
       TRANSFORM(totite,  ;
       '999,999.99') + numped +  ;
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
SELECT 6
USE GC_PRO00
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'CONSUMO DE REPUESTOS',  ;
   'CONSULTA'
@ 2, 63 SAY PROGRAM()
STORE SPACE(14) TO wrk_codigo
DO WHILE .T.
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     STORE SPACE(1) TO wrk_tipdoc
     STORE 0 TO wrk_numord
     CLEAR
     @ 00, 01 TO 06, 75
     @ 01, 04 SAY  ;
       'N즡ero ....... :'
     @ 01, 41 SAY  ;
       'Fecha O/R .... :'
     @ 02, 04 SAY  ;
       'Cliente ...... :'
     @ 03, 04 SAY  ;
       'Repuestos .... :'
     @ 04, 04 SAY  ;
       'M. de Obra ... :'
     @ 05, 04 SAY  ;
       'Flete ........ :'
     @ 03, 41 SAY  ;
       'Total ....... :'
     @ 04, 41 SAY  ;
       'IGV ......... :'
     @ 05, 41 SAY  ;
       'T. General .. :'
     SET CURSOR ON
     @ 01, 22 GET wrk_numord  ;
       PICTURE '99999999' VALID  ;
       despues(1) WHEN antes(1)  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          RELEASE WINDOW cabecera
          DEACTIVATE WINDOW  ;
                     tablas
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     wrk_file = f_archivo()
     CREA TABLE  &wrk_file (NUMPED C(8),;
  NUMORD C(8),   CODPRO C(14),;
 CANTID N(4,0), PREITE N(9,2), TOTITE;
N(9,2), FECHA D) 
     SELECT 7
     USE &wrk_file
     SELECT 3
     SEEK STR(wrk_numord, 8)
     IF FOUND()
          DO WHILE numord= ;
             STR(wrk_numord, 8)
               wrk_numped = numdoc
               SELECT 2
               SEEK wrk_numped
               IF indest <> 'N'
                    SELECT 3
                    wrk_codpro = codpro
                    wrk_cantid = canpro
                    wrk_import = valpro
                    wrk_total = totite
                    wrk_fecha = date
                    SELECT 7
                    APPEND BLANK
                    REPLACE numped  ;
                            WITH  ;
                            wrk_numped
                    REPLACE numord  ;
                            WITH  ;
                            STR(wrk_numord,  ;
                            8)
                    REPLACE codpro  ;
                            WITH  ;
                            wrk_codpro
                    REPLACE cantid  ;
                            WITH  ;
                            wrk_cantid
                    REPLACE preite  ;
                            WITH  ;
                            wrk_import
                    REPLACE totite  ;
                            WITH  ;
                            wrk_total
                    REPLACE fecha  ;
                            WITH  ;
                            wrk_fecha
               ENDIF
               SELECT 3
               SKIP
          ENDDO
     ENDIF
     SELECT 7
     DO WHILE LASTKEY()<>27
          ACTIVATE POPUP detalle
     ENDDO
     SELECT 7
     USE
     ERASE FILE &wrk_file
ENDDO
CLOSE DATABASES
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
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
               DO p_mensaje WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT 1
          SET ORDER TO 1
          SEEK STR(wrk_numord, 8)
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Nro. de Orden No Existe'
               RETURN .F.
          ENDIF
          @ 01, 59 SAY fecemi
          @ 03, 22 SAY cosrep  ;
            PICTURE  ;
            '$999,999.99'
          @ 04, 22 SAY cosmob  ;
            PICTURE  ;
            '$999,999.99'
          @ 05, 22 SAY flete  ;
            PICTURE  ;
            '$999,999.99'
          @ 03, 56 SAY totnet  ;
            PICTURE  ;
            '$999,999.99'
          @ 04, 56 SAY totigv  ;
            PICTURE  ;
            '$999,999.99'
          @ 05, 56 SAY totbru  ;
            PICTURE  ;
            '$999,999.99'
          wrk_estado = auxest
          wrk_codcli = codent
          wrk_fecest = fecest
          SELECT 5
          SEEK 'ESOR' +  ;
               wrk_estado
          IF FOUND()
               @ 06, 05 SAY  ;
                 tab_destab COLOR  ;
                 SCHEME 8
               @ 06, 65 SAY  ;
                 wrk_fecest COLOR  ;
                 SCHEME 8
          ENDIF
          SELECT 4
          SEEK 'C' + wrk_codcli
          IF  .NOT. FOUND()
               wrk_codcli = SPACE(9)
          ELSE
               wrk_codcli = noment
          ENDIF
          @ 02, 22 SAY  ;
            SUBSTR(wrk_codcli, 1,  ;
            30)
ENDCASE
*
PROCEDURE ayuda
ON KEY
SELECT 1
wrk_origen = 'OR'
campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
DO ayuda4 WITH campoa, wrk_origen
RETURN
*
PROCEDURE busc
PARAMETER wrk_tipdoc
ON KEY
ACTIVATE WINDOW tipos
STORE SPACE(4) TO item
@ 00, 00 GET item DEFAULT tipo(1)  ;
  SIZE 02, 25 FROM tipo
READ
wrk_tipo = item
wrk_tipdoc = SUBSTR(wrk_tipo, 1,  ;
             4)
DEACTIVATE WINDOW tipos
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
