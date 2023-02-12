*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW venta1 FROM 15, 00  ;
       TO 21, 37 COLOR SCHEME 20
SELECT 1
USE SHARED GC_HVE00 ALIAS  ;
    gc_hve00 ORDER CODIGO
SELECT 7
USE SHARED GE_TAB0 ALIAS gc_tab00  ;
    ORDER CODIGO
STORE DATE() TO fecha1, fecha2
STORE 'Pantalla ' TO output
STORE 'Res£men' TO opcion
DO WHILE .T.
     ACTIVATE WINDOW venta1
     CLEAR
     @ 00, 00 SAY 'Desde Fecha:'  ;
       COLOR W/N,N/W  GET fecha1
     @ 01, 00 SAY 'Hasta Fecha:'  ;
       COLOR W/N,N/W  GET fecha2  ;
       RANGE fecha1
     @ 02, 00 SAY  ;
       'por Pantalla/Impresora:'  ;
       COLOR W/N,N/W  GET output  ;
       PICTURE  ;
       '@m Pantalla ,Impresora'
     @ 03, 00 SAY  ;
       'Res£men/Detalle:' COLOR W/ ;
       N,N/W  GET opcion PICTURE  ;
       '@m Res£men,Detalle'
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW  ;
                     venta1
          CLOSE DATABASES
          RETURN
     ELSE
          @ 04, 00 SAY  ;
            'Procesando...'
          SELECT gc_hve00.hve_fecdoc,  ;
                 gc_hve00.hve_tipdoc,  ;
                 gc_dve00.dve_tipdoc,  ;
                 gc_hve00.hve_codemi,  ;
                 gc_hve00.hve_estdoc,  ;
                 gc_hve00.hve_indori,  ;
                 gc_hve00.hve_nrodoc,  ;
                 gc_dve00.dve_nrodoc,  ;
                 gc_dve00.dve_propar,  ;
                 gc_dve00.dve_cantid,  ;
                 gc_pro00.pro_coremo,  ;
                 gc_dve00.dve_total  ;
                 FROM GC_HVE00,  ;
                 GC_DVE00,  ;
                 GC_PRO00 WHERE  ;
                 gc_hve00.hve_nrodoc =  ;
                 gc_dve00.dve_nrodoc  ;
                 AND  ;
                 gc_pro00.pro_codpro =  ;
                 gc_dve00.dve_propar  ;
                 AND  ;
                 gc_hve00.hve_fecdoc >=  ;
                 fecha1 AND  ;
                 gc_hve00.hve_fecdoc <=  ;
                 fecha2 AND  ;
                 gc_hve00.hve_estdoc <>  ;
                 'A' ORDER BY  ;
                 gc_hve00.hve_indori,  ;
                 gc_hve00.hve_codemi,  ;
                 gc_dve00.dve_propar,  ;
                 gc_hve00.hve_fecdoc  ;
                 INTO CURSOR  ;
                 QUERY
          IF output = 'Impresora'
               IF opcion =  ;
                  'Detalle'
                    ?? CHR(15)
                    REPORT FORMAT  ;
                           AGCR4206  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    ?? CHR(15)
                    REPORT FORMAT  ;
                           AGCR4205  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ELSE
               IF opcion =  ;
                  'Detalle'
                    REPORT FORMAT  ;
                           AGCR4206  ;
                           TO  ;
                           FILE  ;
                           TEXT2.TXT  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           AGCR4205  ;
                           TO  ;
                           FILE  ;
                           TEXT2.TXT  ;
                           NOCONSOLE
               ENDIF
               SET SYSMENU ON
               MODIFY FILE  ;
                      TEXT2.TXT  ;
                      NOEDIT  ;
                      WINDOW  ;
                      pantall
               SET SYSMENU OFF
          ENDIF
     ENDIF
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***
