*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 12, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 13, 02  ;
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
   'ARTICULOS DE INVENTARIO',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
STORE SPACE(10) TO wrk_ubi1,  ;
      wrk_ubi2
wrk_progra = PROGRAM()
forma = 1
STORE DATE() TO wrk_fecinv
STORE 0 TO wrk_ubimod, wrk_nrodig
STORE SPACE(4) TO wrk_desde,  ;
      wrk_hasta
DO WHILE .T.
     ON KEY
     STORE 0 TO wrk_fin
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'Tipo de Emisi¢n :'
     @ 04, 01 SAY  ;
       'Fecha de Invent.:'
     @ 05, 01 SAY  ;
       'Ubic. de M¢dulo :'
     @ 06, 01 SAY  ;
       'Nro. de D¡gitos :'
     @ 00, 35 SAY 'Almacenes :'
     @ 01, 46 SAY 'Del :'
     @ 02, 46 SAY 'Al  :'
     @ 03, 35 SAY 'Ubicaci¢n :'
     @ 04, 46 SAY 'Del :'
     @ 05, 46 SAY 'Al  :'
     STORE 1 TO opc, opc3
     @ 01, 18 GET opc DEFAULT 1  ;
       SIZE 1, 10, 0 FUNCTION  ;
       '*RVT Sin saldos;Con Saldos;En Blanco'  ;
       VALID almacen(opc)
     READ
     IF LASTKEY() = 27 .OR.  ;
        wrk_fin = 1
          CLOSE DATABASES
          if file('&w_idx1')
               erase &w_idx1
          ENDIF
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
     ELSE
          DO imprime
     ENDIF
ENDDO
RETURN
*
FUNCTION almacen
PARAMETER wrk_opc
IF wrk_opc = 3
     @ 00, 35 CLEAR TO 05, 70
     wrk_pagina = 0
     @ 00, 40 SAY 'Formato :'
     @ 01, 49 GET forma DEFAULT 1  ;
       SIZE 1, 10, 0 FUNCTION  ;
       '*RVT Principal;Sucursal'
     READ
     @ 05, 49 SAY 'Copias :' GET  ;
       wrk_pagina PICTURE '99'  ;
       VALID wrk_pagina > 0 .AND.  ;
       wrk_pagina < 100
     READ
     IF LASTKEY() = 27
          RETURN
     ENDIF
     w_pag = 1
     IF forma = 2
          IF opc3 = 1
               REPORT FORMAT  ;
                      AGCP231B TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR w_pag <=  ;
                      wrk_pagina
          ELSE
               w_txt = f_texto()
               REPO FORM AGCP231B TO FILE;
&w_txt NOCONSOLE
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &W_TXT WINDOW;
PANTALL 
               SET SYSMENU OFF
               delete file &w_txt
          ENDIF
     ELSE
          IF opc3 = 1
               REPORT FORMAT  ;
                      AGCP231A TO  ;
                      PRINTER  ;
                      NOCONSOLE  ;
                      FOR w_pag <=  ;
                      wrk_pagina
          ELSE
               w_txt = f_texto()
               REPO FORM AGCP231A TO FILE;
&w_txt NOCONSOLE
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &W_TXT WINDOW;
PANTALL 
               SET SYSMENU OFF
               delete file &w_txt
          ENDIF
     ENDIF
     wrk_fin = 1
     RETURN
ENDIF
DO p_footer WITH  ;
   '100010000000000000001', 2
ACTIVATE WINDOW cabecera
wrk_busca = 'ALMA'
wrk_var = 'wrk_desde'
DO WHILE .T.
     @ 04, 18 GET wrk_fecinv  ;
       PICTURE '!D' VALID  .NOT.  ;
       EMPTY(wrk_fecinv) COLOR  ;
       SCHEME 8
     @ 05, 18 GET wrk_ubimod  ;
       PICTURE '99' VALID  .NOT.  ;
       EMPTY(wrk_ubimod) COLOR  ;
       SCHEME 8
     @ 06, 18 GET wrk_nrodig  ;
       PICTURE '99' VALID  .NOT.  ;
       EMPTY(wrk_nrodig) COLOR  ;
       SCHEME 8
     ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,;
wrk_var
     @ 01, 52 GET wrk_desde VALID   ;
       .NOT. EMPTY(wrk_desde)  ;
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
       SUBSTR(tab_destab, 1, 12)
     ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,wrk_var
     wrk_var = 'wrk_hasta'
     @ 02, 52 GET wrk_hasta RANGE  ;
       wrk_desde VALID  .NOT.  ;
       EMPTY(wrk_hasta) COLOR  ;
       SCHEME 8
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
     IF wrk_desde > wrk_hasta
          DO p_mensaje WITH  ;
             'Rango Pedido Incorrecto'
          LOOP
     ENDIF
     @ 02, 57 SAY  ;
       SUBSTR(tab_destab, 1, 12)
     @ 04, 52 GET wrk_ubi1  ;
       PICTURE '@!' COLOR SCHEME  ;
       8
     @ 05, 52 GET wrk_ubi2 RANGE  ;
       wrk_ubi1 PICTURE '@!'  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
          LOOP
     ENDIF
     opc3 = 1
     @ 06, 30 SAY  ;
       'Tipo de Salida :'
     @ 06, 46 GET opc3 DEFAULT 1  ;
       SIZE 1, 10, 0 FUNCTION  ;
       '*RHT Impresora ;Pantalla '
     READ
     IF LASTKEY() = 5 .OR.  ;
        LASTKEY() = 19
          LOOP
     ENDIF
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     EXIT
ENDDO
RETURN
*
PROCEDURE imprime
wrk_conta = 0
= ooaviso( ;
  ' T R A B A J A N D O . . .')
IF wrk_desde < '0029' .OR.  ;
   wrk_desde = '0100' .OR.  ;
   wrk_desde = '0060' .OR.  ;
   wrk_desde = '0090'
     SELECT gc_pro00.pro_codpro,  ;
            gc_pro00.pro_descri,  ;
            gc_pro00.pro_subcat,  ;
            gc_pro00.pro_marca,  ;
            gc_alm00.alm_codalm,  ;
            gc_alm00.alm_stkfis,  ;
            gc_alm00.alm_ubicac  ;
            FROM GC_PRO00,  ;
            GC_ALM00 WHERE  ;
            gc_alm00.alm_codpro =  ;
            gc_pro00.pro_codpro  ;
            AND  ;
            BETWEEN(gc_alm00.alm_codalm,  ;
            wrk_desde, wrk_hasta)  ;
            AND  ;
            gc_alm00.alm_stkfis >  ;
            0 AND  ;
            BETWEEN(gc_alm00.alm_ubicac,  ;
            wrk_ubi1, wrk_ubi2)  ;
            ORDER BY  ;
            gc_alm00.alm_codalm,  ;
            gc_alm00.alm_ubicac,  ;
            gc_pro00.pro_codpro  ;
            INTO CURSOR QUERY
ELSE
     SELECT gc_pro00.pro_codpro,  ;
            gc_pro00.pro_descri,  ;
            gc_pro00.pro_subcat,  ;
            gc_pro00.pro_marca,  ;
            gc_alm00.alm_codalm,  ;
            gc_alm00.alm_stkfis,  ;
            gc_alm00.alm_ubicac  ;
            FROM GC_PRO00,  ;
            GC_ALM00 WHERE  ;
            gc_alm00.alm_codpro =  ;
            gc_pro00.pro_codpro  ;
            AND  ;
            BETWEEN(gc_alm00.alm_codalm,  ;
            wrk_desde, wrk_hasta)  ;
            AND  ;
            gc_alm00.alm_stkfis >  ;
            0 ORDER BY  ;
            gc_alm00.alm_codalm,  ;
            gc_alm00.alm_ubicac,  ;
            gc_pro00.pro_codpro  ;
            INTO CURSOR QUERY
ENDIF
SELECT query
IF wrk_desde <> '0001' .OR.  ;
   wrk_desde = '0100' .OR.  ;
   wrk_desde <> '0090' .OR.  ;
   wrk_desde <> '0021' .OR.  ;
   wrk_desde <> '0060'
     w_idx1 = f_indice()
     INDE ON ALM_CODALM + SUBSTR(ALM_UBICAC,wrk_ubimod,wrk_nrodig);
+ ALM_UBICAC + PRO_CODPRO TO &w_idx1
ENDIF
COUNT TO wrk_conta
IF wrk_conta = 0
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     RETURN
ENDIF
GOTO TOP
DO WHILE LASTKEY()<>27
     IF opc3 = 1
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
     ENDIF
     IF wrk_desde < '0029' .OR.  ;
        wrk_desde = '0100' .OR.  ;
        wrk_desde = '0060' .OR.  ;
        wrk_desde = '0090'
          IF opc3 = 1
               REPORT FORMAT  ;
                      AGCP0231 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               w_txt = f_texto()
               REPORT FORMAT  ;
                      AGCP0231 TO  ;
                      FILE  ;
                      inv99.txt  ;
                      NOCONSOLE
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &W_TXT WINDOW;
PANTALL NOEDIT
               SET SYSMENU OFF
               delete file &w_txt
          ENDIF
     ELSE
          IF opc3 = 1
               REPORT FORMAT  ;
                      AGCP0231 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               w_txt = f_texto()
               REPO FORM AGCP0231 TO FILE;
&w_txt NOCONSOLE
               SET SYSMENU ON
               KEYBOARD '{CTRL+F10}'
               modi comm &W_TXT WINDOW;
PANTALL NOEDIT
               SET SYSMENU OFF
               delete file &w_txt
          ENDIF
          SET FILTER TO
     ENDIF
     SET PRINTER TO
     DEACTIVATE WINDOW winmensaje
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
*
*** 
*** ReFox - retrace your steps ... 
***
