*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 COLOR SCHEME  ;
       7
DEFINE POPUP detalle FROM 10, 02  ;
       TO 21, 77 PROMPT FIELDS  ;
       DTOC(kardex.fecing) + ' ' +  ;
       DTOC(kardex.fecdoc) + ' ' +  ;
       SUBSTR(kardex.tipdoc, 1,  ;
       4) + ' ' + kardex.nrodoc +  ;
       ' ' + SUBSTR(kardex.almrec,  ;
       1, 4) +  ;
       SUBSTR(kardex.almdes, 1,  ;
       4) + ' ' +  ;
       SUBSTR(kardex.codmov, 1,  ;
       4) + ' ' +  ;
       TRANSFORM(kardex.stkant,  ;
       '9999') +  ;
       TRANSFORM(kardex.cantid,  ;
       '999') + ' ' +  ;
       SUBSTR(kardex.tidore, 1,  ;
       3) + ' ' +  ;
       ALLTRIM(kardex.nrdore)  ;
       TITLE  ;
       'FecIngÄÄÄÄÄFecDocÄÄÄTipÄÄDocÄÄÄÄÄÄÄÄIngÄSalÄÄMovÄStAntÄCantÄÄÄDoc.RefÄÄÄÄÄ'  ;
       IN screen COLOR SCHEME 8
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_KAR00 ORDER KAR_PROIN2
SELECT 3
USE GC_ALM00 ORDER CODIGO
SELECT 4
USE GC_HVE00 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'MOVIMIENTO DE ARTICULO',  ;
   'CONSULTA'
@ 2, 63 SAY PROGRAM()
wrk_fchdes = DATE()
wrk_fchhas = DATE()
STORE SPACE(14) TO wrk_codigo
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
     @ 00, 01 SAY  ;
       'Producto     :'
     @ 00, 16 GET wrk_codigo  ;
       PICTURE '@!' COLOR SCHEME  ;
       8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     SELECT gc_pro00
     SET ORDER TO CODIGO
     SEEK wrk_codigo
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'C¢digo no Existe'
          LOOP
     ENDIF
     DO p_footer WITH  ;
        '100000000000001110001',  ;
        2
     ACTIVATE WINDOW cabecera
     @ 00, 35 SAY pro_descri
     wrk_cosact = pro_coremo
     SELECT gc_alm00
     SUM FOR alm_codpro =  ;
         wrk_codigo alm_stkfis TO  ;
         wrk_total
     @ 01, 01 SAY  ;
       'Stock Actual : '
     @ 01, 16 SAY wrk_total
     @ 01, 40 SAY  ;
       'Fob   Actual : '
     @ 01, 55 SAY wrk_cosact
     @ 02, 01 SAY  ;
       'Fecha Desde  : ' GET  ;
       wrk_fchdes PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(wrk_fchdes)
     @ 02, 40 SAY  ;
       'Fecha Hasta  : ' GET  ;
       wrk_fchhas PICTURE '@D'  ;
       VALID  .NOT.  ;
       EMPTY(wrk_fchhas)
     READ
     IF LASTKEY() = 27
          CLEAR
          LOOP
     ENDIF
     ON KEY
     CREATE CURSOR KARDEX (fecdoc  ;
            D, horing C (5),  ;
            tipdoc C (4), nrodoc  ;
            C (10), codmov C (4),  ;
            tidore C (4), stkant  ;
            N (9, 2), cantid N (7,  ;
            2), import N (9, 2),  ;
            cosuni N (9, 2),  ;
            almrec C (4), almdes  ;
            C (4), fecing D,  ;
            nrdore C (10), cosprd  ;
            N (9, 2), cosprs N (9,  ;
            2))
     SELECT gc_kar00
     COUNT FOR kar_codpro =  ;
           wrk_codigo .AND.  ;
           (gc_kar00.kar_fecing >=  ;
           wrk_fchdes .AND.  ;
           gc_kar00.kar_fecing <=  ;
           wrk_fchhas) TO  ;
           wrk_conta
     SEEK wrk_codigo
     SCAN WHILE kar_codpro =  ;
          wrk_codigo
          IF kar_fecing >=  ;
             wrk_fchdes .AND.  ;
             kar_fecing <=  ;
             wrk_fchhas
               SELECT kardex
               APPEND BLANK
               REPLACE fecdoc  ;
                       WITH  ;
                       gc_kar00.kar_fecdoc,  ;
                       horing  ;
                       WITH  ;
                       gc_kar00.kar_horing,  ;
                       tipdoc  ;
                       WITH  ;
                       gc_kar00.kar_tipdoc,  ;
                       nrodoc  ;
                       WITH  ;
                       gc_kar00.kar_nrodoc,  ;
                       codmov  ;
                       WITH  ;
                       gc_kar00.kar_codmov,  ;
                       tidore  ;
                       WITH  ;
                       gc_kar00.kar_tidore,  ;
                       stkant  ;
                       WITH  ;
                       gc_kar00.kar_stkant,  ;
                       cantid  ;
                       WITH  ;
                       gc_kar00.kar_cantid,  ;
                       import  ;
                       WITH  ;
                       gc_kar00.kar_import,  ;
                       cosuni  ;
                       WITH  ;
                       gc_kar00.kar_cosuni,  ;
                       almrec  ;
                       WITH  ;
                       gc_kar00.kar_almrec,  ;
                       almdes  ;
                       WITH  ;
                       gc_kar00.kar_almdes,  ;
                       fecing  ;
                       WITH  ;
                       gc_kar00.kar_fecing,  ;
                       nrdore  ;
                       WITH  ;
                       gc_kar00.kar_nrdore,  ;
                       cosprd  ;
                       WITH  ;
                       gc_kar00.kar_cosant,  ;
                       cosprs  ;
                       WITH  ;
                       gc_kar00.kar_cosanb
          ENDIF
          SELECT gc_kar00
     ENDSCAN
     IF wrk_conta > 0
          DO WHILE LASTKEY()<>27
               ACTIVATE POPUP  ;
                        detalle
          ENDDO
     ELSE
          DO p_mensaje WITH  ;
             'No existe Movimiento a Consultar'
     ENDIF
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***
