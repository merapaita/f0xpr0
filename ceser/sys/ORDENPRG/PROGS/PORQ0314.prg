*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
PUBLIC proveed, estades, pedido,  ;
       docu, descri
STORE SPACE(30) TO proveed,  ;
      estades, pedido, descri
STORE SPACE(10) TO docu
DEFINE WINDOW marco FROM 07, 15  ;
       TO 19, 69 DOUBLE
DEFINE WINDOW busqueda FROM 08,  ;
       17 TO 18, 68 NONE
DEFINE WINDOW orden FROM 9, 30 TO  ;
       16, 48 NONE COLOR SCHEME  ;
       20
DEFINE WINDOW detalle FROM 08, 00  ;
       TO 17, 76 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW pant1 FROM 05, 27  ;
       TO 7, 55 IN screen
DEFINE WINDOW ven2 FROM 18, 10 TO  ;
       20, 30 TITLE  ;
       'INGRESE CODIGO' IN screen  ;
       COLOR SCHEME 12
tit_prg = 'CONSULTA'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONSULTA DE BACKORDERS'
ON KEY
SET CURSOR ON
SELECT 1
USE SHARED gc_hco00 ORDER CODIGO
SELECT 2
USE SHARED GC_CLI00 ORDER CODIGO
SELECT 3
USE SHARED ge_tab0 ORDER CODIGO
SELECT 4
USE SHARED gc_pro00 ORDER Codigo
wrk_selpro = SELECT()
SELECT 5
USE SHARED GC_ALM00 ORDER CODIGO
SELECT 6
USE SHARED gc_dco00 ORDER CODPRO
STORE .T. TO sigue
wrk_tcod = 'CODIGO    :'
wrk_codigo = SPACE(14)
wrk_prove = SPACE(11)
STORE 0 TO n, narea, nord, m
STORE 1 TO choice
DIMENSION choices( 4)
DO WHILE sigue
     ON KEY LABEL F6 DO ORDENA
     DO esc_indica WITH 1, 'AYU',  ;
        'BUS', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     ACTIVATE WINDOW pant1
     @ 00, 01 SAY wrk_tcod COLOR  ;
       W/N 
     @ 00, 13 GET wrk_codigo  ;
       PICTURE '@!'
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW pant1
          sigue = .F.
     ELSE
          DO CASE
               CASE LASTKEY() = 1
                    DO CASE
                         CASE choice =  ;
                              1
                              SELECT  ;
                               gc_pro00
                              GOTO  ;
                               TOP
                              wrk_codigo =  ;
                               pro_codpro
                         CASE choice =  ;
                              2
                              SELECT  ;
                               gc_hco00
                              GOTO  ;
                               TOP
                              wrk_codigo =  ;
                               hco_nrodoc
                         CASE choice =  ;
                              3
                              SELECT  ;
                               gc_dco00
                              GOTO  ;
                               TOP
                              wrk_codigo =  ;
                               dco_feclle
                         CASE choice =  ;
                              4
                              SELECT  ;
                               gc_cli00
                              SEEK  ;
                               'P'
                              wrk_codigo =  ;
                               cli_codigo
                    ENDCASE
               CASE LASTKEY() = 6
                    DO CASE
                         CASE choice =  ;
                              1
                              SELECT  ;
                               gc_pro00
                              GOTO  ;
                               BOTTOM
                              wrk_codigo =  ;
                               pro_codpro
                         CASE choice =  ;
                              2
                              SELECT  ;
                               gc_hco00
                              GOTO  ;
                               BOTTOM
                              wrk_codigo =  ;
                               hco_nrodoc
                         CASE choice =  ;
                              3
                              SELECT  ;
                               gc_dco00
                              GOTO  ;
                               BOTTOM
                              wrk_codigo =  ;
                               dco_feclle
                         CASE choice =  ;
                              4
                              SELECT  ;
                               gc_cli00
                              GOTO  ;
                               BOTTOM
                              wrk_codigo =  ;
                               cli_codigo
                    ENDCASE
               CASE LASTKEY() = 3
                    DO CASE
                         CASE choice =  ;
                              1
                              SELECT  ;
                               gc_pro00
                              IF EOF()
                                   GOTO BOTTOM
                              ELSE
                                   SKIP 1
                              ENDIF
                              wrk_codigo =  ;
                               pro_codpro
                         CASE choice =  ;
                              2
                              SELECT  ;
                               gc_hco00
                              IF EOF()
                                   GOTO BOTTOM
                              ELSE
                                   SKIP 1
                              ENDIF
                              wrk_codigo =  ;
                               hco_nrodoc
                         CASE choice =  ;
                              3
                              SELECT  ;
                               gc_dco00
                              IF EOF()
                                   GOTO BOTTOM
                              ENDIF
                              wrk_codigo =  ;
                               dco_feclle
                         CASE choice =  ;
                              4
                              SELECT  ;
                               gc_cli00
                              IF EOF()
                                   GOTO BOTTOM
                              ELSE
                                   SKIP 1
                              ENDIF
                              wrk_codigo =  ;
                               cli_codigo
                    ENDCASE
               CASE LASTKEY() =  ;
                    18
                    DO CASE
                         CASE choice =  ;
                              1
                              SELECT  ;
                               gc_pro00
                              IF BOF()
                                   GOTO TOP
                              ELSE
                                   SKIP -1
                              ENDIF
                              wrk_codigo =  ;
                               pro_codpro
                         CASE choice =  ;
                              2
                              SELECT  ;
                               gc_hco00
                              IF BOF()
                                   GOTO TOP
                              ELSE
                                   SKIP -1
                              ENDIF
                              wrk_codigo =  ;
                               hco_nrodoc
                         CASE choice =  ;
                              3
                              SELECT  ;
                               gc_dco00
                              IF BOF()
                                   GOTO TOP
                              ELSE
                                   m = m + 1
                                   SKIP -m
                              ENDIF
                              wrk_codigo =  ;
                               dco_feclle
                         CASE choice =  ;
                              4
                              SELECT  ;
                               gc_cli00
                              IF BOF()
                                   GOTO TOP
                              ELSE
                                   SKIP -1
                              ENDIF
                              wrk_codigo =  ;
                               cli_codigo
                    ENDCASE
          ENDCASE
          @ 00, 13 SAY wrk_codigo  ;
            COLOR N/W 
          flag = .T.
          IF choice = 3
               IF wrk_codigo =  ;
                  CTOD(SPACE(8))
                    flag = .F.
               ENDIF
          ELSE
               IF wrk_codigo =  ;
                  SPACE(14) .OR.  ;
                  wrk_codigo =  ;
                  SPACE(10) .OR.  ;
                  wrk_codigo =  ;
                  SPACE(11)
                    flag = .F.
               ENDIF
          ENDIF
          IF flag
               CREATE CURSOR  ;
                      BACKOR  ;
                      (nrodoc C  ;
                      (10),  ;
                      codpro C  ;
                      (14),  ;
                      canbor N (9,  ;
                      2), descri  ;
                      C (40),  ;
                      inorig C  ;
                      (1), docref  ;
                      C (10),  ;
                      codent C  ;
                      (11),  ;
                      totbor N (9,  ;
                      2), fecdoc  ;
                      D, feclle  ;
                      D)
               DO llena
               IF n = 0
                    DO mensa WITH  ;
                       'NO HAY REGISTROS PARA CONSULTAR',  ;
                       'COLO'
               ELSE
                    DO muestra
               ENDIF
          ENDIF
     ENDIF
ENDDO
RELEASE WINDOW cabecera, detalle
DEACTIVATE WINDOW tablas
DEACTIVATE WINDOW pant1
DO mensa WITH '   ', 'SACA'
DO saca_win
CLEAR READ
CLEAR GETS
ON KEY LABEL F6
CLOSE DATABASES
ON KEY
ACTIVATE SCREEN
RETURN
*
PROCEDURE muestra
DO esc_indica WITH 1, 'AYU',  ;
   'INI', 'ARR', 'BBB'
DO esc_indica WITH 2, 'BUS',  ;
   'FIN', 'ABA', 'ESC'
ACTIVATE WINDOW pant1, detalle
ON KEY LABEL HOME DO INICIO
ON KEY LABEL END DO FINAL
ON KEY LABEL F6 DO BUSCAR
SELECT backor
GOTO TOP
BROWSE FIELDS codpro : 14 :H =  ;
       '  CODIGO   ', descri : 20  ;
       :H = 'DESCRIPCION ',  ;
       canbor :H = 'CANTID. B/O.',  ;
       inorig : 2 :H = 'ORIGEN',  ;
       docref : 10 :H = 'DOC.REF',  ;
       nrodoc :H = 'N§ PEDIDO',  ;
       feclle :H = 'FECHA LLEG.',  ;
       fecdoc :H = 'FECHA PED.',  ;
       codent :H = 'COD. ENT.',  ;
       totbor :H =  ;
       'CANT TOT. B/O' NOEDIT IN  ;
       detalle NOLGRID
IF LASTKEY() = 27
     SET FILTER TO
     DEACTIVATE WINDOW detalle
ENDIF
ON KEY LABEL HOME
ON KEY LABEL END
ON KEY LABEL F6
RETURN
*
PROCEDURE inicio
GOTO TOP
RETURN
*
PROCEDURE final
GOTO BOTTOM
RETURN
*
PROCEDURE llena
SELECT gc_dco00
STORE 0 TO n, m
DO CASE
     CASE choice = 1
          SET ORDER TO CODPRO
          GOTO TOP
          SEEK wrk_codigo
          SCAN WHILE dco_codpro =  ;
               wrk_codigo
               IF dco_canbor > 0
                    n = n + 1
                    SELECT backor
                    APPEND BLANK
                    REPLACE codpro  ;
                            WITH  ;
                            gc_dco00.dco_codpro,  ;
                            canbor  ;
                            WITH  ;
                            gc_dco00.dco_canbor,  ;
                            inorig  ;
                            WITH  ;
                            gc_dco00.dco_inorig,  ;
                            docref  ;
                            WITH  ;
                            gc_dco00.dco_docref
                    REPLACE feclle  ;
                            WITH  ;
                            gc_dco00.dco_feclle
                    SELECT gc_pro00
                    SEEK gc_dco00.dco_codpro
                    IF FOUND()
                         REPLACE backor.descri  ;
                                 WITH  ;
                                 pro_descri,  ;
                                 backor.totbor  ;
                                 WITH  ;
                                 pro_stkbor
                    ENDIF
                    SELECT gc_hco00
                    SEEK gc_dco00.dco_nrodoc
                    IF FOUND()
                         REPLACE backor.nrodoc  ;
                                 WITH  ;
                                 hco_nrodoc,  ;
                                 backor.fecdoc  ;
                                 WITH  ;
                                 hco_fecdoc,  ;
                                 backor.codent  ;
                                 WITH  ;
                                 hco_codent
                    ENDIF
               ENDIF
          ENDSCAN
     CASE choice = 2
          SET ORDER TO CODIGO
          GOTO TOP
          SEEK wrk_codigo
          SCAN WHILE dco_nrodoc =  ;
               wrk_codigo
               IF dco_canbor > 0
                    n = n + 1
                    SELECT backor
                    APPEND BLANK
                    REPLACE codpro  ;
                            WITH  ;
                            gc_dco00.dco_codpro,  ;
                            canbor  ;
                            WITH  ;
                            gc_dco00.dco_canbor,  ;
                            inorig  ;
                            WITH  ;
                            gc_dco00.dco_inorig,  ;
                            docref  ;
                            WITH  ;
                            gc_dco00.dco_docref
                    REPLACE feclle  ;
                            WITH  ;
                            gc_dco00.dco_feclle
                    SELECT gc_pro00
                    SEEK gc_dco00.dco_codpro
                    IF FOUND()
                         REPLACE backor.descri  ;
                                 WITH  ;
                                 pro_descri,  ;
                                 backor.totbor  ;
                                 WITH  ;
                                 pro_stkbor
                    ENDIF
                    SELECT gc_hco00
                    SEEK gc_dco00.dco_nrodoc
                    IF FOUND()
                         REPLACE backor.nrodoc  ;
                                 WITH  ;
                                 hco_nrodoc,  ;
                                 backor.fecdoc  ;
                                 WITH  ;
                                 hco_fecdoc,  ;
                                 backor.codent  ;
                                 WITH  ;
                                 hco_codent
                    ENDIF
               ENDIF
          ENDSCAN
     CASE choice = 3
          DO mensa WITH  ;
             'ESPERE UN MOMENTO',  ;
             'COLO'
          SET ORDER TO DCO_FECLLE
          GOTO TOP
          SEEK DTOS(wrk_codigo)
          SCAN WHILE dco_feclle =  ;
               wrk_codigo
               m = m + 1
               IF dco_canbor > 0
                    n = n + 1
                    SELECT backor
                    APPEND BLANK
                    REPLACE codpro  ;
                            WITH  ;
                            gc_dco00.dco_codpro,  ;
                            canbor  ;
                            WITH  ;
                            gc_dco00.dco_canbor,  ;
                            inorig  ;
                            WITH  ;
                            gc_dco00.dco_inorig,  ;
                            docref  ;
                            WITH  ;
                            gc_dco00.dco_docref
                    REPLACE feclle  ;
                            WITH  ;
                            gc_dco00.dco_feclle
                    SELECT gc_pro00
                    SEEK gc_dco00.dco_codpro
                    IF FOUND()
                         REPLACE backor.descri  ;
                                 WITH  ;
                                 pro_descri,  ;
                                 backor.totbor  ;
                                 WITH  ;
                                 pro_stkbor
                    ENDIF
                    SELECT gc_hco00
                    SEEK gc_dco00.dco_nrodoc
                    IF FOUND()
                         REPLACE backor.nrodoc  ;
                                 WITH  ;
                                 hco_nrodoc,  ;
                                 backor.fecdoc  ;
                                 WITH  ;
                                 hco_fecdoc,  ;
                                 backor.codent  ;
                                 WITH  ;
                                 hco_codent
                    ENDIF
               ENDIF
          ENDSCAN
     CASE choice = 4
          DO mensa WITH  ;
             'ESPERE UN MOMENTO',  ;
             'SACA'
          SELECT gc_dco00
          SET ORDER TO CODIGO
          SELECT gc_hco00
          SET ORDER TO HCO_CODENT
          GOTO TOP
          SEEK wrk_codigo
          SCAN WHILE hco_codent =  ;
               wrk_codigo
               SELECT gc_dco00
               SEEK gc_hco00.hco_nrodoc
               SCAN WHILE  ;
                    dco_nrodoc =  ;
                    gc_hco00.hco_nrodoc
                    IF dco_canbor >  ;
                       0
                         n = n +  ;
                             1
                         SELECT backor
                         APPEND BLANK
                         REPLACE nrodoc  ;
                                 WITH  ;
                                 gc_dco00.dco_nrodoc,  ;
                                 fecdoc  ;
                                 WITH  ;
                                 gc_hco00.hco_fecdoc,  ;
                                 codent  ;
                                 WITH  ;
                                 gc_hco00.hco_codent
                         REPLACE codpro  ;
                                 WITH  ;
                                 gc_dco00.dco_codpro,  ;
                                 canbor  ;
                                 WITH  ;
                                 gc_dco00.dco_canbor,  ;
                                 inorig  ;
                                 WITH  ;
                                 gc_dco00.dco_inorig,  ;
                                 docref  ;
                                 WITH  ;
                                 gc_dco00.dco_docref
                         REPLACE feclle  ;
                                 WITH  ;
                                 gc_dco00.dco_feclle
                         SELECT gc_pro00
                         SEEK gc_dco00.dco_codpro
                         IF FOUND()
                              REPLACE  ;
                               backor.descri  ;
                               WITH  ;
                               pro_descri,  ;
                               backor.totbor  ;
                               WITH  ;
                               pro_stkbor
                         ENDIF
                    ENDIF
               ENDSCAN
          ENDSCAN
          SET ORDER TO CODIGO
ENDCASE
*
PROCEDURE ordena
STORE 'CODIGO     ' TO choices(  ;
      1)
STORE 'N§PED.B/O  ' TO choices(  ;
      2)
STORE 'FECHA      ' TO choices(  ;
      3)
STORE 'PROVEEDOR  ' TO choices(  ;
      4)
STORE 1 TO choice
ACTIVATE WINDOW orden
DEFINE POPUP ayu0 FROM 0, 0 TO 06,  ;
       15 COLOR SCHEME 8
FOR i = 1 TO 4
     DEFINE BAR i OF ayu0 PROMPT  ;
            choices(i)
ENDFOR
ON SELECTION POPUP ayu0 do choice0 WITH;
PROMPT()
ACTIVATE POPUP ayu0 NOWAIT
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
DEACTIVATE WINDOW orden
ACTIVATE WINDOW pant1
@ 00, 01 SAY wrk_tcod
@ 00, 13 SAY SPACE(14)
CLEAR READ
@ 00, 13 GET wrk_codigo PICTURE  ;
  '@!'
READ
RETURN
*
PROCEDURE choice0
PARAMETER clave
DO CASE
     CASE SUBSTR(clave, 1, 1) =  ;
          'C'
          choice = 1
          wrk_tcod = 'CODIGO    :'
          wrk_codigo = SPACE(14)
          ON KEY LABEL F6 DO AYUDA
     CASE SUBSTR(clave, 1, 1) =  ;
          'N'
          choice = 2
          wrk_codigo = SPACE(10)
          wrk_tcod = 'N§ PEDIDO :'
          ON KEY LABEL F6 DO OOBUSCAR;
WITH 1
     CASE SUBSTR(clave, 1, 1) =  ;
          'F'
          choice = 3
          wrk_tcod = 'FECHA     :'
          wrk_codigo = CTOD(SPACE(8))
          ON KEY LABEL F6 DO OOBUSCAR;
WITH 2
     CASE SUBSTR(clave, 1, 1) =  ;
          'P'
          choice = 4
          wrk_tcod = 'PROVEEDOR :'
          wrk_codigo = SPACE(11)
          ON KEY LABEL F6 DO OOBUSCAR;
WITH 3
ENDCASE
DEACTIVATE POPUP ayu0
RETURN
*
PROCEDURE buscar
STORE SPACE(14) TO codi
ACTIVATE WINDOW ven2
@ 00, 00 GET codi PICTURE '@!'
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW ven2
     RETURN
ENDIF
SELECT backor
GOTO TOP
LOCATE FOR codpro = codi
DEACTIVATE WINDOW ven2
RETURN
*
PROCEDURE oobuscar
PARAMETER num
DO CASE
     CASE num = 1
          SELECT DISTINCT  ;
                 hco_nrodoc + '³' +  ;
                 DTOC(hco_feclle)  ;
                 FROM gc_HCO00  ;
                 WHERE hco_nrodoc <>  ;
                 hco_numfac INTO  ;
                 ARRAY arrayprov
          titulo = '   N§ DOCUMENTO        PRODUCTO         FECHA     '
     CASE num = 2
          SELECT DISTINCT  ;
                 DTOC(dco_feclle) +  ;
                 '³' + dco_nrodoc +  ;
                 '³' + dco_codpro  ;
                 FROM gc_DCO00  ;
                 WHERE  ;
                 DTOC(dco_feclle) <>  ;
                 '  /  /  ' INTO  ;
                 ARRAY arrayprov
          titulo = '   FECHA        N§ DOCUMENTO       PRODUCTO       '
     CASE num = 3
          SELECT DISTINCT  ;
                 cli_codigo +  ;
                 ' ³ ' +  ;
                 cli_razsoc FROM  ;
                 gc_cli00 WHERE  ;
                 cli_tpper = 'P'  ;
                 INTO ARRAY  ;
                 arrayprov
          titulo = '   CODIGO                DESCRIPCION              '
ENDCASE
ACTIVATE WINDOW marco
ACTIVATE WINDOW busqueda
@ 00, 00 SAY titulo COLOR N/W 
@ 01, 00 GET getorden DEFAULT  ;
  arrayprov SIZE 12, 51 FROM  ;
  arrayprov VALID oovalor(1, ;
  getorden) COLOR SCHEME 8
READ
DEACTIVATE WINDOW busqueda, marco
RETURN
*
PROCEDURE oovalor
PARAMETER opc, cdato
IF LASTKEY() = 13
     DO CASE
          CASE num = 1
               wrk_codigo = SUBSTR(cdato,  ;
                            1,  ;
                            10)
          CASE num = 2
               wrk_codigo = SUBSTR(cdato,  ;
                            1,  ;
                            8)
          CASE num = 3
               wrk_codigo = SUBSTR(cdato,  ;
                            1,  ;
                            11)
               SELECT gc_cli00
               SEEK 'P' +  ;
                    wrk_codigo
     ENDCASE
ENDIF
RETURN
*
PROCEDURE ayuda
wrk_select = SELECT()
SELECT gc_pro00
wrk_selpro = SELECT()
wrk_campo = gc_pro00.pro_codpro
DO produc WITH wrk_campo,  ;
   wrk_select, wrk_selpro
IF LASTKEY() <> 27
     KEYBOARD wrk_campo
ENDIF
SELECT (wrk_select)
ON KEY LABEL F6 do ayuda
RETURN
*
PROCEDURE xxayuda
wrk_selec = SELECT()
wrk_campo = 'PRO_CODPRO'
DO produc WITH wrk_campo,  ;
   wrk_selec, wrk_selpro
IF LASTKEY() = 13
     wrk_codigo = gc_pro00.pro_codpro
ELSE
     IF LASTKEY() = 27
          ON KEY LABEL F6 DO ORDENA
     ENDIF
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
