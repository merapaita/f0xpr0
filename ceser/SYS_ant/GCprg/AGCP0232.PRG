*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 11, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 7
SELECT 1
USE GC_ALM00 ORDER CODIGO
SELECT 2
USE GC_PRO00 ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'STICKERS DE INVENTARIO',  ;
   'REPORTE'
@ 2, 63 SAY PROGRAM()
wrk_progra = PROGRAM()
DO WHILE .T.
     ON KEY
     STORE SPACE(4) TO wrk_desde,  ;
           wrk_hasta
     STORE DATE() TO wrk_fecinv
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'Fecha de Inventario :'
     @ 02, 01 SAY  ;
       'Tipo  de Impresi¢n  :'
     @ 00, 40 SAY 'Almacenes :'
     @ 01, 46 SAY 'Del :'
     @ 02, 46 SAY 'Al  :'
     @ 03, 35 SAY  ;
       'Tipo de Salida :'
     STORE 1 TO opc
     @ 00, 23 GET wrk_fecinv  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_fecinv) .AND.  ;
       oovalid() COLOR SCHEME 8
     @ 02, 23 GET opc DEFAULT 1  ;
       SIZE 1, 10, 0 FUNCTION  ;
       '*RV Rangos;Total' VALID  ;
       almacen(opc)
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
                  winmensaje
          DO p_footer WITH  ;
             '100000000001001110001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
ENDDO
RETURN
*
FUNCTION oovalid
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .F.
ENDIF
*
FUNCTION almacen
PARAMETER wrk_opc
IF wrk_opc = 1
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     wrk_busca = 'ALMA'
     wrk_var = 'wrk_desde'
     DO WHILE .T.
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
          @ 01, 52 GET wrk_desde  ;
            VALID  .NOT.  ;
            EMPTY(wrk_desde)  ;
            COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SET ORDER TO CODIGO
          SEEK 'ALMA' + wrk_desde
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               LOOP
          ENDIF
          @ 01, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
          wrk_var = 'wrk_hasta'
          @ 02, 52 GET wrk_hasta  ;
            RANGE wrk_desde VALID   ;
            .NOT.  ;
            EMPTY(wrk_hasta)  ;
            COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SET ORDER TO CODIGO
          SEEK 'ALMA' + wrk_hasta
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               LOOP
          ENDIF
          IF wrk_desde >  ;
             wrk_hasta
               DO p_mensaje WITH  ;
                  'Rango Pedido Incorrecto'
               LOOP
          ENDIF
          @ 03, 52 GET opc3  ;
            DEFAULT 2 SIZE 1, 10,  ;
            0 FUNCTION  ;
            '*RVT Impresora ;Pantalla '
          READ
          IF LASTKEY() = 27
               RETURN .F.
          ENDIF
          @ 02, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          EXIT
     ENDDO
     DO imprime WITH 2
ELSE
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     @ 03, 52 GET opc3 DEFAULT 2  ;
       SIZE 1, 10, 0 FUNCTION  ;
       '*RVT Impresora ;Pantalla '
     READ
     IF LASTKEY() = 27
          RETURN .F.
     ENDIF
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     DO imprime WITH 1
ENDIF
*
PROCEDURE imprime
PARAMETER wrk_o
= ooaviso( ;
  ' T R A B A J A N D O . . .')
IF wrk_o = 2
     wrk_orden = ' GC_ALM00.ALM_CODALM BETWEEN wrk_desde AND wrk_hasta '
ELSE
     wrk_orden = ' GC_ALM00.ALM_CODALM <> SPACE(4) '
ENDIF
SELECT  GC_PRO00.PRO_CODPRO, GC_PRO00.PRO_DESCRI,;
GC_PRO00.PRO_SUBCAT,  GC_PRO00.PRO_MARCA,;
GC_ALM00.ALM_CODALM, GC_ALM00.ALM_STKFIS,;
 GC_ALM00.ALM_UBICAC  FROM  GC_PRO00,;
GC_ALM00  WHERE  GC_ALM00.ALM_CODPRO =;
GC_PRO00.PRO_CODPRO  AND ( &wrk_orden;
 AND  GC_ALM00.ALM_STKFIS > 0);
 ORDER  BY GC_ALM00.ALM_CODALM, GC_ALM00.ALM_UBICAC,;
GC_PRO00.PRO_CODPRO  INTO   CURSOR STICKER
COUNT TO wrk_conta
IF wrk_conta = 0
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     RETURN
ENDIF
IF opc3 = 1
     DO WHILE LASTKEY()<>27
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    DEACTIVATE WINDOW  ;
                               winmensaje
                    RETURN
               ENDIF
          ENDIF
          = ooaviso( ;
            'I M P R I M I E N D O . . .' ;
            )
          ??? CHR(18)
          LABEL FORMAT AGCP0232  ;
                TO PRINTER  ;
                NOCONSOLE
          SET PRINTER TO
          DEACTIVATE WINDOW  ;
                     winmensaje
          EXIT
     ENDDO
     DO p_mensaje WITH  ;
        ' FIN DE IMPRESION '
ELSE
     w_txt = f_texto()
     label FORM AGCP0232 TO FILE &w_txt;
NOCONSOLE
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     modi comm &W_TXT WINDOW PANTALL 
     SET SYSMENU OFF
     delete file &w_txt
     DEACTIVATE WINDOW winmensaje
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
