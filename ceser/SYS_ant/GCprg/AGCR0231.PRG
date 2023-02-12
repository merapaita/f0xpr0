*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW venta02 FROM 16, 31  ;
       TO 18, 46 IN screen COLOR  ;
       SCHEME 20
SELECT 1
USE GC_PRO00 ORDER CODIGO
SELECT 2
USE GC_HIP00 ORDER CODIGO
SELECT 3
USE GC_HVE00 ORDER CODIGO
SELECT 4
USE GC_ALM00 ORDER CODIGO
DO WHILE .T.
     ACTIVATE WINDOW venta02
     wrk_dia = DATE()
     @ 00, 00 SAY 'D¡a :'
     @ 00, 06 GET wrk_dia PICTURE  ;
       '@D'
     READ
     IF LASTKEY() = 27
          CLEAR
          DEACTIVATE WINDOW  ;
                     venta02
          RELEASE WINDOW venta02
          CLOSE DATABASES
          RETURN
     ENDIF
     SELECT gc_kar00.kar_codpro,  ;
            gc_kar00.kar_fecing,  ;
            gc_kar00.kar_tipdoc,  ;
            gc_kar00.kar_nrodoc,  ;
            gc_kar00.kar_codmov,  ;
            gc_kar00.kar_cantid,  ;
            gc_kar00.kar_import,  ;
            gc_kar00.kar_almrec,  ;
            gc_kar00.kar_almdes,  ;
            gc_pro00.pro_descri,  ;
            gc_kar00.kar_cosuni  ;
            FROM GC_KAR00,  ;
            GC_PRO00 WHERE  ;
            gc_pro00.pro_codpro =  ;
            gc_kar00.kar_codpro  ;
            AND  ;
            gc_kar00.kar_fecing =  ;
            wrk_dia ORDER BY  ;
            gc_kar00.kar_codmov,  ;
            gc_kar00.kar_tipdoc,  ;
            gc_kar00.kar_nrodoc  ;
            INTO CURSOR KAR_ALM
     COUNT TO wrk_cant
     IF wrk_cant > 0
          REPORT FORMAT AGCR0231  ;
                 TO PRINTER  ;
                 NOCONSOLE
          SET PRINTER TO
     ENDIF
ENDDO
*
FUNCTION movim
PARAMETER opc
DO CASE
     CASE SUBSTR(opc, 1, 1) = 'I'
          RETURN 'RECE'
     CASE SUBSTR(opc, 1, 1) = 'A'
          RETURN 'AJUS'
     CASE SUBSTR(opc, 1, 1) = 'C'
          RETURN 'CONS'
     CASE SUBSTR(opc, 1, 1) = 'T'
          RETURN 'TRAS'
     CASE opc = 'EMER'
          RETURN 'AJUS'
     CASE SUBSTR(opc, 1, 1) = 'E'
          RETURN 'DESP'
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***
