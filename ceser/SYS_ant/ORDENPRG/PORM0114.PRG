*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
tit_prg = ' ACTUALIZACION '
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ACTUALIZA S/SERVICIOS '
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
DO WHILE .T.
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 1 TO valor
     STORE 'A' TO wk_unidad
     STORE 0 TO wk_elisol
     @ 04, 00 TO 06, 25
     @ 05, 02 SAY  ;
       'Unidad a Copiar.:'
     @ 04, 41 SAY  ;
       'S/S Seleccionadas :'
     @ 09, 02 SAY  ;
       'Eliminar  S/S...:'
     @ 05, 33 TO 16, 72
     @ 05, 36 SAY  ;
       'ÄS/SÄÄÄÄÄÄMODELOÄÄÄÄÄÄÄÄÄSERIE'
     SET CURSOR ON
     @ 05, 20 GET wk_unidad  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 8
     READ
     IF LASTKEY() = 27 .AND.  ;
        valor = 1
          EXIT
     ENDIF
     DO WHILE .T.
          STORE 1 TO opcion,  ;
                valor
          @ 09, 20 GET wk_elisol  ;
            PICTURE '99999999'  ;
            VALID  ;
            oovalid(VARREAD())  ;
            COLOR SCHEME 8
          @ 12, 04 GET opcion  ;
            SIZE 1, 8 FUNCTION  ;
            '*H Actualizar;Revisar'  ;
            VALID  ;
            oovalid(VARREAD())  ;
            COLOR B/W 
          READ CYCLE
          IF LASTKEY() = 27 .AND.  ;
             valor = 1
               EXIT
          ENDIF
     ENDDO
ENDDO
RELEASE WINDOW example
RELEASE POPUP campo
CLOSE DATABASES
DELETE FILE TR_ISREP.DBF
DELETE FILE TR_ISERI.DBF
DELETE FILE TR_ICLPR.DBF
DELETE FILE TR_SICLI.DBF
DELETE FILE TR_ISREP.CDX
DELETE FILE TR_ISERI.CDX
DELETE FILE TR_ICLPR.CDX
DELETE FILE TR_SICLI.CDX
DELETE FILE TR_ISREP.FPT
DO saca_win
RETURN
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'WK_UNIDAD'
          IF wk_unidad <> 'A'  ;
             .AND. wk_unidad <>  ;
             'B'
               DO error WITH  ;
                  '*** Unidad No permitida ***'
               RETURN .F.
          ENDIF
          IF EMPTY(wk_unidad)
               DO error WITH  ;
                  '*** No se permiten blancos ***'
               RETURN .F.
          ENDIF
          DO mensa WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'COLO'
          ruta = wk_unidad + ':' +  ;
                 'trans'
          !PKUNZIP -o &ruta > NULL
          DO actibases
          DO mensa WITH  ;
             '*** Espere un momento, por favor ***',  ;
             'SACA'
          SELECT tr_isrep
          @ 05, 33 GET listfield  ;
            DEFAULT  ;
            tr_isrep.numdoc SIZE  ;
            12, 40 POPUP campo  ;
            COLOR SCHEME 9
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
                    DO mensa WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'COLO'
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SELECT tr_iseri
                    SEEK w_codmar +  ;
                         w_codmod +  ;
                         w_numser
                    IF FOUND()
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDIF
                    SELECT tr_iclpr
                    SEEK w_codent
                    IF FOUND()
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDIF
                    SELECT tr_sicli
                    SET NEAR ON
                    SEEK STR(wk_elisol,  ;
                         8)
                    SET NEAR OFF
                    DO WHILE  ;
                       numdoc= ;
                       STR(wk_elisol,  ;
                       8) .AND.   ;
                       .NOT.  ;
                       EOF()
                         DO rbloquea
                         DELETE
                         UNLOCK
                         SKIP
                    ENDDO
                    DO mensa WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'SACA'
                    SELECT tr_isrep
                    @ 05, 33 GET  ;
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
                    DO mensa WITH  ;
                       '*** Espere un momento, por favor ***',  ;
                       'COLO'
                    SELECT tr_isrep
                    GOTO TOP
                    DO WHILE   ;
                       .NOT.  ;
                       EOF()
                         wk_numdoc =  ;
                          numdoc
                         wk_codmar =  ;
                          codmar
                         wk_codmod =  ;
                          codmod
                         wk_numser =  ;
                          numser
                         wk_codent =  ;
                          codent
                         SELECT st_isrep
                         SEEK wk_numdoc
                         IF  .NOT.  ;
                             FOUND()
                              SELECT  ;
                               tr_isrep
                              SCATTER  ;
                               MEMO  ;
                               MEMVAR
                              SELECT  ;
                               st_isrep
                              APPEND  ;
                               BLANK
                              DO rbloquea
                              GATHER  ;
                               MEMVAR  ;
                               MEMO
                              UNLOCK
                         ENDIF
                         SELECT st_iseri
                         SEEK wk_codmar +  ;
                              wk_codmod +  ;
                              wk_numser
                         IF  .NOT.  ;
                             FOUND()
                              SELECT  ;
                               tr_iseri
                              SEEK  ;
                               wk_codmar +  ;
                               wk_codmod +  ;
                               wk_numser
                              IF FOUND()
                                   SCATTER MEMO MEMVAR
                                   SELECT st_iseri
                                   APPEND BLANK
                                   DO rbloquea
                                   GATHER MEMVAR MEMO
                                   UNLOCK
                              ENDIF
                         ENDIF
                         SELECT st_iclpr
                         SEEK wk_codent
                         IF  .NOT.  ;
                             FOUND()
                              SELECT  ;
                               tr_iclpr
                              SEEK  ;
                               wk_codent
                              IF FOUND()
                                   SCATTER MEMO MEMVAR
                                   SELECT st_iclpr
                                   APPEND BLANK
                                   DO rbloquea
                                   GATHER MEMVAR MEMO
                                   UNLOCK
                              ENDIF
                         ENDIF
                         SELECT tr_sicli
                         SET NEAR ON
                         SEEK wk_numdoc
                         SET NEAR OFF
                         DO WHILE  ;
                            numdoc= ;
                            wk_numdoc  ;
                            .AND.   ;
                            .NOT.  ;
                            EOF()
                              wk_sicli =  ;
                               codsin
                              SELECT  ;
                               st_sicli
                              SEEK  ;
                               wk_numdoc +  ;
                               wk_sicli
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   SELECT tr_sicli
                                   SCATTER MEMO MEMVAR
                                   SELECT st_sicli
                                   APPEND BLANK
                                   DO rbloquea
                                   GATHER MEMVAR MEMO
                                   UNLOCK
                              ENDIF
                              SELECT  ;
                               tr_sicli
                              SKIP
                         ENDDO
                         SELECT tr_isrep
                         SKIP
                         DO mensa  ;
                            WITH  ;
                            '*** Espere un momento, por favor ***',  ;
                            'SACA'
                    ENDDO
               CASE opcion = 2
                    SELECT tr_isrep
                    @ 05, 33 GET  ;
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
PROCEDURE actibases
SELECT 5
USE TR_ISREP ORDER CODIGO2
SELECT 6
USE TR_ISERI ORDER CODIGO2
SELECT 7
USE TR_ICLPR ORDER CODIGO2
SELECT 8
USE TR_SICLI ORDER CODIGO2
RETURN
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
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
