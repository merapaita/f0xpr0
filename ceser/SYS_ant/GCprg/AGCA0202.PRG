*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
DEFINE WINDOW tablas FROM 00, 00  ;
       TO 24, 79 IN screen  ;
       DOUBLE
DEFINE POPUP tablax FROM 10, 20  ;
       TO 18, 60 PROMPT FIELDS  ;
       ' ' + tab_codtab + ' ?' +  ;
       tab_destab IN screen COLOR  ;
       SCHEME 8
ON SELECTION POPUP tablax DEAC POPUP tablax
DEFINE POPUP dettab FROM 10, 18  ;
       TO 18, 63 PROMPT FIELDS  ;
       ' ' + tab_codtab + ' ?' +  ;
       tab_destab + '?' +  ;
       STR(tab_factor, 6, 2) IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP dettab DEAC POPUP DETTAB
DEFINE POPUP dettax FROM 10, 20  ;
       TO 18, 60 PROMPT FIELDS  ;
       ' ' + tab_codtab + ' ?' +  ;
       tab_destab IN screen COLOR  ;
       SCHEME 8
ON SELECTION POPUP dettax DEAC POPUP DETTAX
DEFINE WINDOW titpass FROM 06, 20  ;
       TO 09, 62 IN screen NONE  ;
       COLOR SCHEME 20
DEFINE WINDOW titdeta FROM 06, 18  ;
       TO 09, 63 IN screen NONE  ;
       COLOR SCHEME 20
DEFINE WINDOW pidpass FROM 19, 20  ;
       TO 21, 62 IN screen NONE  ;
       COLOR SCHEME 20
DEFINE WINDOW piddeta FROM 19, 18  ;
       TO 21, 70 IN screen NONE  ;
       COLOR SCHEME 20
ON KEY LABEL ENTER DO P_CONTENIDO WITH;
TAB_CODTAB
ON KEY LABEL F4 DO P_BORRA WITH 1
ACTIVATE WINDOW tablas
DO p_prestab WITH 'MANTENCION',  ;
   'TABLAS GENERALES',  ;
   'MANTENCION'
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000111010110101', 2
nom_arc = 'GE_TAB0'
ACTIVATE WINDOW titpass
@ 00, 00 SAY  ;
  '                                         '
@ 01, 00 SAY  ;
  '???????????????????????????????????????Ŀ'
@ 02, 00 SAY  ;
  '?CODIGO? N O M B R E                    ?'
@ 03, 00 SAY  ;
  '?????????????????????????????????????????'
nomb = f_archivo()
SELECT 1
USE GE_TAB0 ORDER CODIGO
SEEK 'TABL'
COPY TO &NOMB WHILE TAB_CODPRE="TABL"
SELECT 2
USE &NOMB EXCLUSIVE
INDEX ON tab_codpre + tab_codtab  ;
      TAG codigo
art = SELECT()
ACTIVATE POPUP tablax
DEACTIVATE WINDOW tablas
RELEASE WINDOW titpass, titdeta,  ;
        pidpass, piddeta
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE p_borra
PARAMETER var
ON KEY
IF  .NOT. DELETED()
     swt = .T.
     DO WHILE swt
          IF RLOCK()
               SELECT (art)
               cod1 = tab_codpre
               cod2 = tab_codtab
               SELECT 1
               SEEK cod2
               IF  .NOT. FOUND()
                    SEEK cod1 +  ;
                         cod2
                    DELETE
                    SELECT (art)
                    DELETE
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       'EXISTEN DATOS EN ESA TABLA....VERIFIQUE POR FAVOR'
               ENDIF
               UNLOCK
               EXIT
          ELSE
               swt = f_yesno( ;
                     'Registro bloqueado. Intentar nuevamente ' ;
                     )
          ENDIF
     ENDDO
ELSE
     RECALL
ENDIF
ON KEY
IF var = 1
     ON KEY LABEL ENTER DO P_CONTENIDO;
WITH TAB_CODTAB
     ON KEY LABEL F4 DO P_BORRA WITH 1
ELSE
     ON KEY LABEL F3 DO P_LEE WITH 1,2,CODTAB,TAB_CODTAB,WK1_FACTOR
     ON KEY LABEL ENTER DO P_LEE WITH;
2,2,CODTAB,TAB_CODTAB,WK1_FACTOR
     ON KEY LABEL F4 DO P_BORRA WITH 2
     ON KEY LABEL F9 DO P_CAMBIA WITH;
WK1_FACTOR
ENDIF
SELECT (art)
RETURN
*
PROCEDURE p_lee
PARAMETER op, cual, cdg, ctemp,  ;
          wk1_factor
DO p_footer WITH  ;
   '100000000000000000001', 2
ON KEY
IF xnnn < '3'
     SET CURSOR OFF
     IF cual = 2
          ON KEY LABEL F3 DO P_LEE WITH;
1,2,CODTAB,TAB_CODTAB,WK1_FACTOR
          ON KEY LABEL ENTER DO P_LEE;
WITH 2,2,CODTAB,TAB_CODTAB,WK1_FACTOR
          ON KEY LABEL F4 DO P_BORRA WITH;
2
          ON KEY LABEL F9 DO P_CAMBIA;
WITH WK1_FACTOR
     ELSE
          ON KEY LABEL ENTER DO P_CONTENIDO;
WITH TAB_CODTAB
          ON KEY LABEL F4 DO P_BORRA WITH;
1
     ENDIF
     SELECT (art)
     RETURN
ENDIF
SET CURSOR ON
ON KEY
swt = .T.
SELECT (art)
DO WHILE swt
     IF RLOCK()
          IF op = 2
               COUNT TO ctos
               IF ctos <= 0
                    IF cual = 2
                         ON KEY LABEL;
F3 DO P_LEE WITH 1,2,CODTAB,TAB_CODTAB,WK1_FACTOR
                         ON KEY LABEL;
ENTER DO P_LEE WITH 2,2,CODTAB,TAB_CODTAB,WK1_FACTOR
                         ON KEY LABEL;
F4 DO P_BORRA WITH 2
                         ON KEY LABEL;
F9 DO P_CAMBIA WITH WK1_FACTOR
                    ELSE
                         ON KEY LABEL;
ENTER DO P_CONTENIDO WITH TAB_CODTAB
                         ON KEY LABEL;
F4 DO P_BORRA WITH 1
                    ENDIF
                    SELECT (art)
                    RETURN
               ELSE
                    GOTO TOP
               ENDIF
          ENDIF
          IF cual = 1
               ACTIVATE WINDOW  ;
                        pidpass
               @ 00, 00 SAY  ;
                 '???????????????????????????????????????Ŀ'
               @ 01, 00 SAY  ;
                 '?      ?                                ?'
               @ 02, 00 SAY  ;
                 '?????????????????????????????????????????'
          ELSE
               IF wk1_factor = 0
                    ACTIVATE WINDOW  ;
                             pidpass
                    @ 00, 00 SAY  ;
                      '???????????????????????????????????????Ŀ'
                    @ 01, 00 SAY  ;
                      '?      ?                                ?'
                    @ 02, 00 SAY  ;
                      '?????????????????????????????????????????'
               ELSE
                    ACTIVATE WINDOW  ;
                             piddeta
                    @ 00, 00 SAY  ;
                      '?????????????????????????????????????????????????Ŀ'
                    @ 01, 00 SAY  ;
                      '?      ?                              ?           ?'
                    @ 02, 00 SAY  ;
                      '???????????????????????????????????????????????????'
               ENDIF
          ENDIF
          SELECT (art)
          DIMENSION a1( FCOUNT())
          DO CASE
               CASE op = 1
                    IF cual = 1
                         cdg = 'TABL'
                    ENDIF
                    DO WHILE .T.
                         xcod = SPACE(4)
                         @ 01, 02  ;
                           GET  ;
                           xcod  ;
                           VALID  ;
                           xcod <>  ;
                           '0000'
                         READ
                         IF LASTKEY() =  ;
                            27  ;
                            .OR.  ;
                            xcod =  ;
                            SPACE(4)
                              DEACTIVATE  ;
                               WINDOW  ;
                               pidpass
                              DEACTIVATE  ;
                               WINDOW  ;
                               piddeta
                              IF cual =  ;
                                 2
                                   ON KEY LABEL F3 DO P_LEE WITH 1,2,CODTAB,TAB_CODTAB,WK1_FACTOR
                                   ON KEY LABEL ENTER DO P_LEE WITH 2,2,CODTAB,TAB_CODTAB,WK1_FACTOR
                                   ON KEY LABEL F4 DO P_BORRA WITH 2
                                   ON KEY LABEL F9 DO P_CAMBIA WITH WK1_FACTOR
                              ELSE
                                   ON KEY LABEL ENTER DO P_CONTENIDO WITH TAB_CODTAB
                                   ON KEY LABEL F4 DO P_BORRA WITH 1
                              ENDIF
                              aux_indica =  ;
                               1
                              DO p_footer  ;
                                 WITH  ;
                                 '101100010000100110001',  ;
                                 2
                              SELECT  ;
                               (art)
                              RETURN
                         ENDIF
                         SELECT 1
                         SEEK cdg +  ;
                              xcod
                         IF  .NOT.  ;
                             FOUND()
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               tab_codpre  ;
                               WITH  ;
                               cdg
                              REPLACE  ;
                               tab_codtab  ;
                               WITH  ;
                               xcod
                              SELECT  ;
                               (art)
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               tab_codpre  ;
                               WITH  ;
                               cdg
                              REPLACE  ;
                               tab_codtab  ;
                               WITH  ;
                               xcod
                              REPLACE  ;
                               tab_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               tab_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               tab_hora  ;
                               WITH  ;
                               SUBSTR(TIME(),  ;
                               1,  ;
                               5)
                              EXIT
                         ELSE
                              DO p_mensaje  ;
                                 WITH  ;
                                 'CODIGO YA EXISTE...VERIFIQUE POR FAVOR'
                         ENDIF
                    ENDDO
                    SCATTER TO a1
               CASE op = 2
                    SELECT (art)
                    SEEK ctemp
                    SCATTER TO a1
          ENDCASE
          IF cual = 2
               @ 01, 02 SAY a1(2)
               @ 01, 08 SAY a1(3)
               IF wk1_factor <> 0
                    @ 01, 32 SAY  ;
                      ALLTRIM(STR(a1(5),  ;
                      5, 2))
               ENDIF
          ENDIF
          @ 01, 02 SAY a1(2)
          @ 01, 08 GET a1( 3)  ;
            PICTURE '@!' VALID  ;
            a1(3) <> SPACE(30)
          READ
          IF cual = 2 .AND.  ;
             wk1_factor = 1
               @ 01, 39 GET a1(  ;
                 5) PICTURE  ;
                 '9999999.99'
               READ
          ENDIF
          IF LASTKEY() <> 27
               SELECT 1
               IF op = 1
                    SEEK cdg +  ;
                         xcod
               ELSE
                    SEEK cdg +  ;
                         a1(2)
               ENDIF
               GATHER FROM a1
               REPLACE tab_usuari  ;
                       WITH  ;
                       clave
               REPLACE tab_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE tab_hora  ;
                       WITH  ;
                       SUBSTR(TIME(),  ;
                       1, 5)
               SELECT (art)
               GATHER FROM a1
          ELSE
               IF op = 1
                    IF cual = 1
                         SELECT (art)
                         SEEK a1(1) +  ;
                              a1(2)
                    ELSE
                         SELECT (art)
                         SEEK a1(2) +  ;
                              a1(1)
                    ENDIF
                    DELETE
                    SELECT 1
                    SEEK a1(1) +  ;
                         a1(2)
                    DELETE
               ENDIF
          ENDIF
          DEACTIVATE WINDOW  ;
                     pidpass
          DEACTIVATE WINDOW  ;
                     piddeta
          UNLOCK
          EXIT
     ELSE
          swt = f_yesno( ;
                'Registro bloqueado. Intentar nuevamente ' ;
                )
     ENDIF
ENDDO
SET CURSOR OFF
ON KEY
IF cual = 2
     ON KEY LABEL F3 DO P_LEE WITH 1,2,CODTAB,TAB_CODTAB,WK1_FACTOR
     ON KEY LABEL ENTER DO P_LEE WITH;
2,2,CODTAB,TAB_CODTAB,WK1_FACTOR
     ON KEY LABEL F4 DO P_BORRA WITH 2
     ON KEY LABEL F9 DO P_CAMBIA WITH;
WK1_FACTOR
ELSE
     ON KEY LABEL ENTER DO P_CONTENIDO;
WITH TAB_CODTAB
     ON KEY LABEL F4 DO P_BORRA WITH 1
ENDIF
aux_indica = 1
DO p_footer WITH  ;
   '101100010000100110001', 2
SELECT (art)
RETURN
*
PROCEDURE p_contenid
PARAMETER codtab
ON KEY
xdesc = tab_destab
wk1_factor = tab_factor
SELECT 1
SEEK codtab
nomb1 = f_archivo()
COPY TO &NOMB1 WHILE TAB_CODPRE=CODTAB
SELECT 3
USE &NOMB1 EXCLUSIVE
INDEX ON tab_codtab + tab_codpre  ;
      TAG codigo
art = SELECT()
ON KEY LABEL F3 DO P_LEE WITH 1,2,CODTAB,TAB_CODTAB,WK1_FACTOR
ON KEY LABEL ENTER DO P_LEE WITH 2,2,CODTAB,TAB_CODTAB,WK1_FACTOR
ON KEY LABEL F4 DO P_BORRA WITH 2
ON KEY LABEL F9 DO P_CAMBIA WITH WK1_FACTOR
aux_indica = 1
DO p_footer WITH  ;
   '101100010000100110001', 2
SELECT (art)
IF wk1_factor = 1
     ACTIVATE WINDOW titdeta
     @ 00, 00 SAY ' TABLA DE ' +  ;
       xdesc
     @ 01, 00 SAY  ;
       '????????????????????????????????????????????Ŀ'
     @ 02, 00 SAY  ;
       '?CODIGO? N O M B R E                  ?FACTOR?'
     @ 03, 00 SAY  ;
       '??????????????????????????????????????????????'
     ACTIVATE POPUP dettab
ELSE
     ACTIVATE WINDOW titpass
     @ 00, 00 SAY ' TABLA DE ' +  ;
       xdesc
     @ 01, 00 SAY  ;
       '???????????????????????????????????????Ŀ'
     @ 02, 00 SAY  ;
       '?CODIGO? N O M B R E                    ?'
     @ 03, 00 SAY  ;
       '?????????????????????????????????????????'
     ACTIVATE POPUP dettax
ENDIF
ON KEY
ON KEY LABEL ENTER DO P_CONTENIDO WITH;
TAB_CODTAB
ON KEY LABEL F4 DO P_BORRA WITH 1
art = 2
SELECT (art)
DEACTIVATE WINDOW titdeta
DO p_footer WITH  ;
   '100000000111010110001', 2
ACTIVATE WINDOW titpass
@ 00, 00 SAY  ;
  '                                         '
@ 01, 00 SAY  ;
  '???????????????????????????????????????Ŀ'
@ 02, 00 SAY  ;
  '?CODIGO? N O M B R E                    ?'
@ 03, 00 SAY  ;
  '?????????????????????????????????????????'
RETURN
*
PROCEDURE p_cambia
PARAMETER aux_factor
IF aux_indica = 1
     aux_indica = 0
     DO p_footer WITH  ;
        '000000010110001000001',  ;
        2
ELSE
     aux_indica = 1
     DO p_footer WITH  ;
        '101100010000100110001',  ;
        2
ENDIF
RETURN
*
PROCEDURE p_pintar
PARAMETER aux_pinta, aux_factor
SELECT (art)
IF aux_factor = 1
     ACTIVATE WINDOW titdeta
     @ 00, 00 SAY ' TABLA DE ' +  ;
       xdesc
     @ 01, 00 SAY  ;
       '????????????????????????????????????????????Ŀ'
     @ 02, 00 SAY  ;
       '?CODIGO? N O M B R E                  ?FACTOR?'
     @ 03, 00 SAY  ;
       '??????????????????????????????????????????????'
     SHOW POPUP dettab
ELSE
     ACTIVATE WINDOW titpass
     @ 00, 00 SAY ' TABLA DE ' +  ;
       xdesc
     @ 01, 00 SAY  ;
       '???????????????????????????????????????Ŀ'
     @ 02, 00 SAY  ;
       '?CODIGO? N O M B R E                    ?'
     @ 03, 00 SAY  ;
       '?????????????????????????????????????????'
     SHOW POPUP dettax
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
