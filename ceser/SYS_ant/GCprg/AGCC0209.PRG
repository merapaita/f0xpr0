*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 08, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 IN screen COLOR  ;
       SCHEME 7
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 3
USE GC_HLP00 ORDER CODIGO
SELECT 4
USE GC_DLP00 ORDER CODIGO
SELECT 5
USE GC_ALM00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'PRODUCTO POR LISTA',  ;
   'CONSULTA'
@ 2, 63 SAY PROGRAM()
DO WHILE .T.
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODIGO'
     ON KEY LABEL F6 DO PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 30 SAY SPACE(40)
     STORE SPACE(14) TO  ;
           wrk_codigo
     @ 01, 01 SAY 'Producto : '  ;
       GET wrk_codigo PICTURE  ;
       '@!'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  detalle
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          DO p_tempos
          RETURN
     ENDIF
     ON KEY
     SELECT 1
     SET ORDER TO CODIGO
     SEEK wrk_codigo
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'Codigo no Existe'
          CLEAR
          LOOP
     ENDIF
     @ 01, 30 SAY pro_descri
     DO p_footer WITH  ;
        '100000000000001110001',  ;
        2
     SELECT 4
     COUNT FOR dlp_codpro =  ;
           wrk_codigo TO  ;
           wrk_conta
     IF wrk_conta > 0
          SELECT 4
          SET FILTER TO gc_dlp00.dlp_codpro;
= wrk_codigo
          GOTO TOP
          SET RELATION TO gc_dlp00.dlp_codlis;
INTO gc_hlp00
          ACTIVATE WINDOW detalle
          BROWSE FIELDS  ;
                 gc_hlp00.hlp_codlis  ;
                 :H = 'Codigo',  ;
                 gc_hlp00.hlp_deslis  ;
                 : 22 :H =  ;
                 '     Descripcion  ',  ;
                 gc_hlp00.hlp_vigdes  ;
                 :H = ' Desde ',  ;
                 gc_hlp00.hlp_vighas  ;
                 :H = ' Hasta ',  ;
                 gc_dlp00.dlp_unimed  ;
                 :H = 'Und',  ;
                 gc_dlp00.dlp_prsigv  ;
                 :H = '  Valor  ',  ;
                 gc_dlp00.dlp_prcigv  ;
                 :H =  ;
                 '  Precio  '  ;
                 NOAPPEND NOEDIT  ;
                 IN detalle  ;
                 NOLGRID
          DEACTIVATE WINDOW  ;
                     detalle
     ELSE
          DO p_mensaje WITH  ;
             'No Existe Lista de Precios para este Producto'
     ENDIF
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***
