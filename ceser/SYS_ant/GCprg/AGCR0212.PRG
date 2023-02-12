*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 11, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW impresion FROM 13,  ;
       25 TO 15, 55 COLOR SCHEME  ;
       7
SELECT 1
USE GC_HLP00 ORDER CODIGO
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_DLP00 ORDER CODIGO
SELECT 4
USE GC_ALM00
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'PRODUCTO POR LISTA',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
DIMENSION lista[ 8]
lista[ 1] = 'General'
lista[ 2] = 'L¡nea '
lista[ 3] = 'Categor¡a'
lista[ 4] = 'Sub-Categoria'
lista[ 5] = 'Marca'
lista[ 6] = 'Rubro'
lista[ 7] = 'Sub_Rubro'
lista[ 8] = 'C. Comercial'
wrk_progra = PROGRAM()
wrk_codalm = SPACE(4)
STORE 0 TO wrk_stk
DO WHILE .T.
     STORE SPACE(4) TO wrk_lista,  ;
           wrk_tipbus, wrk_desde,  ;
           wrk_hasta
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'C¢digo de Lista :'
     @ 01, 01 SAY  ;
       'Opciones Por    :'
     @ 01, 42 SAY 'Rango :'
     @ 02, 50 SAY 'Desde :'
     @ 03, 50 SAY 'Hasta :'
     ON KEY
     wrk_var = 'wrk_lista'
     ON KEY LABEL F6 DO BUSHLP;
 WITH WRK_VAR
     @ 00, 20 GET wrk_lista  ;
       PICTURE '@!'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  detalle,  ;
                  impresion
          DO p_footer WITH  ;
             '100000000001001110001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     SELECT 1
     SEEK wrk_lista
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'C¢digo de Lista No Existe'
          LOOP
     ENDIF
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     @ 00, 25 SAY hlp_deslis
     DO WHILE LASTKEY()<>27
          STORE 0 TO item
          @ 01, 19 GET item  ;
            DEFAULT lista(1) SIZE  ;
            04, 20 FROM lista  ;
            COLOR SCHEME 7
          READ
          IF LASTKEY() = 27 .OR.  ;
             LASTKEY() = 13
               EXIT
          ENDIF
     ENDDO
     IF LASTKEY() = 27
          LOOP
     ENDIF
     DO CASE
          CASE item = 1
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime1
          CASE item = 2
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_LINEA'
               wrk_busca = 'LINE'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'LINE' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de L¡nea No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'LINE' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de L¡nea No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
          CASE item = 3
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_CATEGO'
               wrk_busca = 'CATE'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'CATE' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Categor¡a No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'CATE' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Categor¡a No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
          CASE item = 4
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_SUBCAT'
               wrk_busca = 'SUCA'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'SUCA' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Sub_Categor¡a No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'SUCA' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Sub-Categor¡a No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
          CASE item = 5
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_MARCA'
               wrk_busca = 'MARC'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'MARC' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Marca No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'MARC' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Marca No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
          CASE item = 6
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_RUBRO'
               wrk_busca = 'RUBR'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'RUBR' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Rubro No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'RUBR' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Rubro No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
          CASE item = 7
               DO p_footer WITH  ;
                  '100010000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_SUBRUB'
               wrk_busca = 'SURU'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'SURU' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Sub-Rubro No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'SURU' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Sub-Rubro No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
          CASE item = 8
               DO p_footer WITH  ;
                  '100000000000000000001',  ;
                  2
               ACTIVATE WINDOW  ;
                        cabecera
               wrk_param = 'GC_PRO00.PRO_CLACOM'
               wrk_busca = 'CLCO'
               wrk_var = 'wrk_desde'
               ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
               DO WHILE .T.
                    @ 02, 58 GET  ;
                      wrk_desde  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_desde)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    SELECT 2
                    SEEK 'CLCO' +  ;
                         wrk_desde
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Clase Comercial No Existe'
                         LOOP
                    ENDIF
                    wrk_var = 'wrk_hasta'
                    @ 03, 58 GET  ;
                      wrk_hasta  ;
                      VALID   ;
                      .NOT.  ;
                      EMPTY(wrk_hasta)
                    READ
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
                    ON KEY
                    SELECT 2
                    SEEK 'CLCO' +  ;
                         wrk_hasta
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'C¢digo de Clase Comercial No Existe'
                         LOOP
                    ENDIF
                    IF wrk_desde >  ;
                       wrk_hasta
                         DO p_mensaje  ;
                            WITH  ;
                            'Rango Pedido Incorrecto'
                         LOOP
                    ENDIF
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             cabecera
                    EXIT
               ENDDO
               IF LASTKEY() = 27
                    LOOP
               ENDIF
               ACTIVATE WINDOW  ;
                        impresion
               DO imprime WITH  ;
                  wrk_param,  ;
                  wrk_desde,  ;
                  wrk_hasta
     ENDCASE
     CLOSE DATABASES
     ON KEY
     DEACTIVATE WINDOW tablas,  ;
                codigo, footer
     RELEASE WINDOW cabecera,  ;
             detalle
     RESTORE SCREEN
     DO p_footer WITH  ;
        '100000000001001110001',  ;
        1
     RETURN
ENDDO
*
PROCEDURE indica
PARAMETER wrk_opcion, wrk_desde,  ;
          wrk_hasta
SELECT ge_tab0.tab_codtab FROM  ;
       GE_TAB0 WHERE  ;
       ge_tab0.tab_codpre =  ;
       wrk_opcion ORDER BY  ;
       ge_tab0.tab_codtab INTO  ;
       ARRAY dd
wrk_desde = dd(1)
wrk_hasta = dd(1)
RETURN
*
PROCEDURE imprime1
@ 00, 04 SAY  ;
  'PREPARANDO IMPRESION'
SELECT gc_pro00.pro_codpro,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_modelo,  ;
       gc_pro00.pro_unimed,  ;
       gc_pro00.pro_marca,  ;
       gc_pro00.pro_stktra,  ;
       gc_dlp00.dlp_prcigv,  ;
       gc_pro00.pro_linea,  ;
       gc_pro00.pro_catego,  ;
       gc_pro00.pro_subcat,  ;
       ge_tab0.tab_destab FROM  ;
       GC_HLP00, GC_DLP00,  ;
       GC_PRO00, GE_TAB0 WHERE  ;
       gc_dlp00.dlp_codlis =  ;
       gc_hlp00.hlp_codlis AND  ;
       gc_pro00.pro_codpro =  ;
       gc_dlp00.dlp_codpro AND  ;
       ge_tab0.tab_codtab =  ;
       gc_hlp00.hlp_molipr AND  ;
       (gc_hlp00.hlp_codlis =  ;
       wrk_lista) ORDER BY  ;
       gc_pro00.pro_linea,  ;
       gc_pro00.pro_catego,  ;
       gc_pro00.pro_subcat,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_codpro INTO  ;
       CURSOR QUERY
COUNT TO wrk_conta
IF wrk_conta = 0
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     DEACTIVATE WINDOW impresion
     RETURN
ENDIF
DO WHILE LASTKEY()<>27
     IF SYS(13) = 'OFFLINE'
          swt = f_yesno( ;
                'Impresora fuera de Linea. Intentar nuevamente ' ;
                )
          IF swt
               LOOP
          ELSE
               DEACTIVATE WINDOW  ;
                          impresion
               RETURN
          ENDIF
     ENDIF
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     ??? CHR(27) + CHR(18)
     REPORT FORMAT agcr2122 TO  ;
            FILE doc.txt  ;
            NOCONSOLE
     DEACTIVATE WINDOW impresion
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
PROCEDURE imprime
PARAMETER wrk_param, wrk_desde,  ;
          wrk_hasta
@ 00, 04 SAY  ;
  'PREPARANDO IMPRESION'
SELECT  GC_PRO00.PRO_CODPRO, GC_PRO00.PRO_DESCRI,;
GC_PRO00.PRO_MODELO,  GC_PRO00.PRO_UNIMED,;
GC_PRO00.PRO_MARCA, GC_PRO00.PRO_STKTRA,;
 GC_DLP00.DLP_PRCIGV, GC_PRO00.PRO_LINEA,;
GC_PRO00.PRO_CATEGO,  GC_PRO00.PRO_SUBCAT,;
GE_TAB0.TAB_DESTAB, GC_ALM00.ALM_STKFIS,;
 GC_ALM00.ALM_STKRES  FROM  GC_HLP00,;
GC_DLP00, GC_PRO00, GE_TAB0, GC_ALM00;
 WHERE  GC_DLP00.DLP_CODLIS = GC_HLP00.HLP_CODLIS;
 AND  GC_PRO00.PRO_CODPRO = GC_DLP00.DLP_CODPRO;
 AND  GE_TAB0.TAB_CODTAB  = GC_HLP00.HLP_MOLIPR;
 AND     GC_PRO00.PRO_CODPRO = GC_ALM00.ALM_CODPRO;
 AND  GC_ALM00.ALM_CODALM = RGE_CODALM;
 AND  (GC_HLP00.HLP_CODLIS = WRK_lista;
 AND     (&WRK_param >= WRK_desde AND;
&WRK_param <= WRK_hasta))  ORDER BY GC_PRO00.PRO_LINEA,;
GC_PRO00.PRO_CATEGO,  GC_PRO00.PRO_SUBCAT,;
GC_PRO00.PRO_DESCRI, GC_PRO00.PRO_CODPRO;
 INTO CURSOR QUERY
COUNT TO wrk_conta
IF wrk_conta = 0
     DEACTIVATE WINDOW impresion
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     RETURN
ENDIF
DO WHILE LASTKEY()<>27
     IF SYS(13) = 'OFFLINE'
          swt = f_yesno( ;
                'Impresora fuera de Linea. Intentar nuevamente ' ;
                )
          IF swt
               LOOP
          ELSE
               DEACTIVATE WINDOW  ;
                          impresion
               RETURN
          ENDIF
     ENDIF
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     REPORT FORMAT agcr2122 TO  ;
            FILE doc.txt
     DEACTIVATE WINDOW impresion
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
*
FUNCTION stk_alm
PARAMETER wrk_codpro, num
narea = SELECT()
= ooareao('GC_ALM00','CODIGO')
DO CASE
     CASE num = 1
          wrk_codalm = rge_codalm
     CASE num = 2
          wrk_codalm = '0100'
     CASE num = 3
          wrk_codalm = '0020'
     CASE num = 4
          wrk_codalm = '0021'
     CASE num = 5
          wrk_codalm = '0030'
     CASE num = 6
          wrk_codalm = '0040'
     CASE num = 7
          wrk_codalm = '0050'
     CASE num = 8
          wrk_codalm = '0060'
     CASE num = 9
          wrk_codalm = '0070'
     CASE num = 10
          wrk_codalm = '0090'
ENDCASE
SEEK wrk_codpro + wrk_codalm
IF FOUND()
     wrk_stk = alm_stkfis -  ;
               alm_stkres
ELSE
     wrk_stk = 0
ENDIF
SELECT (narea)
RETURN wrk_stk
*
*** 
*** ReFox - retrace your steps ... 
***
