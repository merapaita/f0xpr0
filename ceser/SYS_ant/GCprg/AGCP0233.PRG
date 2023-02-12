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
SELECT 4
USE GC_INV00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESO',  ;
   'CREACION DE ARCHIVOS',  ;
   'PROCESO'
@ 2, 63 SAY PROGRAM()
wrk_progra = PROGRAM()
DO WHILE .T.
     ON KEY
     STORE SPACE(4) TO w_desde,  ;
           w_hasta
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
     wrk_opc = 1
     @ 01, 18 GET wrk_opc DEFAULT  ;
       1 SIZE 1, 10, 0 FUNCTION  ;
       '*RVT Total;Por Grupos'  ;
       VALID almacen(wrk_opc)
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
IF wrk_opc = 2
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     wrk_busca = 'ALMA'
     wrk_var = 'w_desde'
     DO WHILE .T.
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
          @ 01, 52 GET w_desde  ;
            VALID  .NOT.  ;
            EMPTY(w_desde) COLOR  ;
            SCHEME 8
          READ
          IF LASTKEY() = 27
               RETURN .F.
          ENDIF
          SELECT 3
          SET ORDER TO CODIGO
          SEEK 'ALMA' + w_desde
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               LOOP
          ENDIF
          @ 01, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
          wrk_var = 'w_hasta'
          @ 02, 52 GET w_hasta  ;
            VALID  .NOT.  ;
            EMPTY(w_hasta) COLOR  ;
            SCHEME 8
          READ
          IF LASTKEY() = 27
               RETURN .F.
          ENDIF
          SELECT 3
          SET ORDER TO CODIGO
          SEEK 'ALMA' + w_hasta
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               LOOP
          ENDIF
          IF w_desde > w_hasta
               DO p_mensaje WITH  ;
                  'Rango Pedido Incorrecto'
               LOOP
          ENDIF
          @ 02, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          EXIT
     ENDDO
     DO procesa WITH 2
ELSE
     DO procesa WITH 1
ENDIF
*
PROCEDURE procesa
PARAMETER opc
IF  .NOT. FILE('GC_INV00.DBF')
     DO p_mensaje WITH  ;
        'No Existe Archivo de Inventario'
     RETURN
ENDIF
rpt = f_yesno( ;
      'CONFIRMA EL PROCESO ?')
IF  .NOT. rpt
     RETURN
ENDIF
IF opc = 1
     = ooaviso( ;
       ' P R O C E S A N D O . . . 1 ' ;
       )
     SELECT gc_alm00
     COUNT TO wrk_conta
     IF wrk_conta = 0
          DEACTIVATE WINDOW  ;
                     winmensaje
          DO p_mensaje WITH  ;
             'No Existen Registros a Procesar'
          RETURN
     ENDIF
     SELECT gc_inv00
     DELETE ALL
     APPEND FROM GC_ALM00 FOR  ;
            alm_stkfis > 0 FIELDS  ;
            alm_codalm,  ;
            alm_codpro,  ;
            alm_stkfis,  ;
            alm_ubicac
     REPLACE alm_stkres WITH  ;
             alm_stkfis ALL
     SELECT gc_inv00
     GOTO TOP
     SCAN WHILE  .NOT. EOF()
          SELECT gc_pro00
          SEEK gc_inv00.alm_codpro
          IF FOUND()
               SELECT gc_inv00
               DO rbloquea
               REPLACE alm_coprdo  ;
                       WITH  ;
                       gc_pro00.pro_coprmo,  ;
                       alm_propie  ;
                       WITH  ;
                       gc_pro00.pro_propie
               REPLACE alm_coprso  ;
                       WITH  ;
                       gc_pro00.pro_coprmb,  ;
                       alm_usuari  ;
                       WITH clave,  ;
                       alm_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       alm_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT gc_inv00
     ENDSCAN
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        ' FIN DE PROCESO '
     RETURN
ELSE
     = ooaviso( ;
       ' P R O C E S A N D O . . . 2 ' ;
       )
     rango = ' (ALM_CODALM >= w_desde AND ALM_CODALM <= w_hasta) '
     SELECT gc_alm00
     COUNT TO wrk_conta FOR &rango
     IF wrk_conta = 0
          DEACTIVATE WINDOW  ;
                     winmensaje
          DO p_mensaje WITH  ;
             'No Existen Registros a Procesar'
          RETURN
     ENDIF
     SELECT gc_inv00
     DELE FOR &rango
     APPE FROM GC_ALM00 FIELDS ALM_CODALM,;
ALM_CODPRO, ALM_STKFIS, ALM_UBICAC;
 FOR  &rango AND ALM_STKFIS > 0
     repl alm_stkres with alm_stkfis for;
alm_stkfis>0 and &rango
     SELECT gc_inv00
     SET NEAR ON
     SEEK w_desde
     SET NEAR OFF
     SCAN WHILE alm_codalm <=  ;
          w_hasta .AND.  .NOT.  ;
          EOF()
          SELECT gc_pro00
          SEEK gc_inv00.alm_codpro
          IF FOUND()
               SELECT gc_inv00
               DO rbloquea
               REPLACE alm_coprdo  ;
                       WITH  ;
                       gc_pro00.pro_coprmo,  ;
                       alm_propie  ;
                       WITH  ;
                       gc_pro00.pro_propie
               REPLACE alm_coprso  ;
                       WITH  ;
                       gc_pro00.pro_coprmb,  ;
                       alm_usuari  ;
                       WITH clave,  ;
                       alm_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       alm_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT gc_inv00
     ENDSCAN
     DEACTIVATE WINDOW winmensaje
     DO p_mensaje WITH  ;
        ' FIN DE PROCESO '
     RETURN
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***
