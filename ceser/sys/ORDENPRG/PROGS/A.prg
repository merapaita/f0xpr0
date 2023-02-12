*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLEAR ALL
SET TALK OFF
DEFINE WINDOW example FROM 4, 3  ;
       TO 21, 76 TITLE  ;
       ' TRANSFERENCIA DE S/S '  ;
       DOUBLE COLOR SCHEME 7
DEFINE POPUP campo PROMPT FIELDS  ;
       numdoc + ' º ' +  ;
       SUBSTR(codmod, 1, 12) +  ;
       ' º ' + numser TITLE  ;
       'ÄS/SÄÄÄÄÄÄMODELOÄÄÄÄÄÄÄÄÄSERIEÄÄÄÄ'  ;
       MARK CHR(16) MARGIN  ;
       SCROLL
SELECT 1
USE ST_ISREP ORDER CODIGO
SELECT 2
USE ST_ISERI ORDER SER_CODMAR
SELECT 3
USE ST_ICLPR ORDER CODENT
SELECT 4
USE ST_SICLI ORDER CODIGO
DO creabases
DO actibases
ACTIVATE WINDOW example
DO WHILE .T.
     STORE 1 TO opcion, valor
     STORE 'A' TO wk_unidad
     STORE 0 TO wk_adisol,  ;
           wk_elisol
     @ 01, 02 SAY  ;
       'Unidad a Copiar.:'
     @ 01, 41 SAY  ;
       'S/S Seleccionadas :'
     @ 05, 02 SAY  ;
       'Adicionar S/S...:'
     @ 09, 02 SAY  ;
       'Eliminar  S/S...:'
     @ 02, 30 TO 13, 69
     SET CURSOR ON
     @ 01, 20 GET wk_unidad  ;
       PICTURE '@!' VALID  ;
       (wk_unidad $ 'AB') .AND.   ;
       .NOT. EMPTY(wk_unidad)  ;
       COLOR SCHEME 8
     @ 05, 20 GET wk_adisol  ;
       PICTURE '99999999' VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 8
     @ 09, 20 GET wk_elisol  ;
       PICTURE '99999999' VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 8
     @ 14, 35 GET opcion SIZE 1,  ;
       8 FUNCTION  ;
       '*H Transferir;Revisar'  ;
       VALID oovalid(VARREAD())
     READ CYCLE
     IF LASTKEY() = 27 .AND.  ;
        valor = 1
          RELEASE WINDOW example
          RELEASE POPUP campo
          CLOSE DATABASES
          DELETE FILE  ;
                 TR_ISREP.DBF
          DELETE FILE  ;
                 TR_ISERI.DBF
          DELETE FILE  ;
                 TR_ICLPR.DBF
          DELETE FILE  ;
                 TR_SICLI.DBF
          DELETE FILE  ;
                 TR_ISREP.CDX
          DELETE FILE  ;
                 TR_ISERI.CDX
          DELETE FILE  ;
                 TR_ICLPR.CDX
          DELETE FILE  ;
                 TR_SICLI.CDX
          EXIT
     ENDIF
ENDDO
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'WK_ADISOL'
          IF  .NOT.  ;
              EMPTY(wk_adisol)
               SELECT st_isrep
               SEEK STR(wk_adisol,  ;
                    8)
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** Solicitud no encontrada ***'
                    RETURN .F.
               ENDIF
               SELECT tr_isrep
               SEEK STR(wk_adisol,  ;
                    8)
               IF FOUND()
                    DO error WITH  ;
                       '*** Solicitud seleccionada ***'
                    RETURN .F.
               ENDIF
               DO mensa2 WITH  ;
                  '*** Espere un momento, por favor ***',  ;
                  'COLO'
               SELECT st_isrep
               SCATTER MEMO  ;
                       MEMVAR
               SELECT tr_isrep
               APPEND BLANK
               GATHER MEMVAR MEMO
               SELECT st_iseri
               SEEK m.codmar +  ;
                    m.codmod +  ;
                    m.numser
               IF FOUND()
                    SCATTER MEMO  ;
                            MEMVAR
                    SELECT tr_iseri
                    APPEND BLANK
                    GATHER MEMVAR  ;
                           MEMO
               ENDIF
               SELECT st_iclpr
               SEEK m.codent
               IF FOUND()
                    SCATTER MEMO  ;
                            MEMVAR
                    SELECT tr_iclpr
                    APPEND BLANK
                    GATHER MEMVAR  ;
                           MEMO
               ENDIF
               SET NEAR ON
               SELECT st_sicli
               SEEK m.numdoc
               DO WHILE numdoc= ;
                  m.numdoc .AND.   ;
                  .NOT. EOF()
                    SCATTER MEMO  ;
                            MEMVAR
                    SELECT tr_sicli
                    APPEND BLANK
                    GATHER MEMVAR  ;
                           MEMO
                    SELECT st_sicli
                    SKIP
               ENDDO
               SET NEAR OFF
               DO mensa2 WITH  ;
                  '*** Espere un momento, por favor ***',  ;
                  'SACA'
               SELECT tr_isrep
               @ 02, 30 GET  ;
                 listfield  ;
                 DEFAULT  ;
                 tr_isrep.numdoc  ;
                 SIZE 12, 40  ;
                 POPUP campo  ;
                 COLOR SCHEME 9
               RETURN .F.
          ENDIF
     CASE opc = 'WK_ELISOL'
          IF  .NOT.  ;
              EMPTY(wk_elisol)
               SELECT tr_isrep
               SEEK STR(wk_elisol,  ;
                    8)
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** Solicitud No Seleccionada ***'
               ELSE
                    w_codmar = codmar
                    w_codmod = codmod
                    w_numser = numser
                    w_codent = codent
                    DO mensa2  ;
                       WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'COLO'
                    DELETE
                    SELECT tr_iseri
                    SEEK w_codmar +  ;
                         w_codmod +  ;
                         w_numser
                    IF FOUND()
                         DELETE
                    ENDIF
                    SELECT tr_iclpr
                    SEEK w_codent
                    IF FOUND()
                         DELETE
                    ENDIF
                    SET NEAR ON
                    SELECT tr_sicli
                    SEEK STR(wk_elisol,  ;
                         8)
                    DO WHILE  ;
                       numdoc= ;
                       STR(wk_elisol,  ;
                       8) .AND.   ;
                       .NOT.  ;
                       EOF()
                         DELETE
                    ENDDO
                    DO mensa2  ;
                       WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'SACA'
                    SELECT tr_isrep
                    @ 02, 30 GET  ;
                      listfield  ;
                      DEFAULT  ;
                      tr_isrep.numdoc  ;
                      SIZE 12, 40  ;
                      POPUP campo  ;
                      COLOR  ;
                      SCHEME 9
               ENDIF
               RETURN .F.
          ENDIF
     CASE opc = 'OPCION'
          DO CASE
               CASE opcion = 1
                    DO mensa2  ;
                       WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'COLO'
                    DO cierrabase
                    ruta = wk_unidad +  ;
                           ':' +  ;
                           'trans'
                    !PKZIP -ex -& &ruta;
tr_*.dbf tr_*.cdx
                    DO actibases
                    DO mensa2  ;
                       WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'SACA'
               CASE opcion = 2
                    SELECT tr_isrep
                    @ 02, 30 GET  ;
                      listfield  ;
                      DEFAULT  ;
                      tr_isrep.numdoc  ;
                      SIZE 12, 40  ;
                      POPUP campo  ;
                      COLOR  ;
                      SCHEME 9
                    READ CYCLE
                    valor = 0
                    RETURN .F.
          ENDCASE
ENDCASE
*
PROCEDURE creabases
SELECT 1
COPY TO TR_ISREP STRUCTURE
SELECT 5
USE EXCLUSIVE TR_ISREP
INDEX ON numdoc TAG codigo2
SELECT 2
COPY TO TR_ISERI STRUCTURE
SELECT 6
USE EXCLUSIVE TR_ISERI
INDEX ON codmar + modelo + numser  ;
      TAG codigo2
SELECT 3
COPY TO TR_ICLPR STRUCTURE
SELECT 7
USE EXCLUSIVE TR_ICLPR
INDEX ON codent TAG codigo2
SELECT 4
COPY TO TR_SICLI STRUCTURE
SELECT 8
USE EXCLUSIVE TR_SICLI
INDEX ON numdoc TAG codigo2
*
PROCEDURE actibases
SELECT 5
USE TR_ISREP ORDER CODIGO2
SELECT 6
USE TR_ISERI ORDER CODIGO2
SELECT 7
USE TR_ICLPR ORDER CODIGO2
SELECT 8
USE TR_SICLI ORDER CODIGO2
*
PROCEDURE cierrabase
SELECT 5
USE
SELECT 6
USE
SELECT 7
USE
SELECT 8
USE
*
*** 
*** ReFox - retrace your steps ... 
***
