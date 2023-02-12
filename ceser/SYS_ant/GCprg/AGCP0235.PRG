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
USE GC_INV00 ORDER UBICA
SELECT 2
USE GC_PRO00 ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'DIFERENCIA DE INVENTARIO',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
wrk_progra = PROGRAM()
DO WHILE .T.
     ON KEY
     STORE SPACE(4) TO wrk_desde,  ;
           wrk_hasta
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'Tipo de Emisi¢n :'
     @ 00, 35 SAY 'Almacenes :'
     @ 01, 46 SAY 'Del :'
     @ 02, 46 SAY 'Al  :'
     STORE 1 TO opc, w_pagina
     @ 01, 18 GET opc DEFAULT 1  ;
       SIZE 1, 10, 0 FUNCTION  ;
       '*RVT Solo Stock;Valorizado'  ;
       VALID almacen(opc)
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
FUNCTION almacen
PARAMETER wrk_opc
DO p_footer WITH  ;
   '100010000000000000001', 2
ACTIVATE WINDOW cabecera
wrk_busca = 'ALMA'
wrk_var = 'wrk_desde'
DO WHILE .T.
     ON KEY LABEL F6 DO BUSCA  WITH WRK_BUSCA,wrk_var
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
     @ 02, 52 GET wrk_hasta VALID   ;
       .NOT. EMPTY(wrk_hasta)  ;
       COLOR SCHEME 8
     @ 03, 35 SAY 'Copias    :'  ;
       GET w_pagina PICTURE '99'  ;
       VALID w_pagina > 0 .AND.  ;
       w_pagina < 100
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
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     EXIT
ENDDO
DO imprime WITH wrk_opc
*
PROCEDURE imprime
PARAMETER wrk_o
= ooaviso( ;
  ' T R A B A J A N D O . . .')
SELECT 1
SET FILTER TO alm_stkfis - alm_stkres;
<> 0;
.AND. (alm_codalm >= wrk_desde;
.AND. alm_codalm <= wrk_hasta)
COUNT TO wrk_conta
IF wrk_conta = 0
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        'No Existen Registros a Listar'
     RETURN
ENDIF
GOTO TOP
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
     SET SYSMENU ON
     ??? CHR(15)
     IF wrk_o = 1
          FOR x = 1 TO w_pagina
               REPORT FORMAT  ;
                      AGCP0235 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDFOR
     ELSE
          FOR x = 1 TO w_pagina
               REPORT FORMAT  ;
                      AGCP235A TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDFOR
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
