*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 08,  ;
       13 TO 16, 68 COLOR SCHEME  ;
       24
DEFINE POPUP buscar FROM 09, 45  ;
       TO 13, 65 PROMPT FIELDS  ;
       cliente.cli_codigo + '³' +  ;
       cliente.cli_razsoc MARK  ;
       CHR(16) MARGIN IN screen  ;
       SCROLL COLOR SCHEME 7
ON SELECTION POPUP buscar DEAC POPUP buscar
SELECT 3
USE SHARED GC_CLI00 ORDER CODIGO
SELECT 5
USE SHARED gc_pro00 ORDER codigo
SELECT 6
USE SHARED ge_tab0 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'PRODUCTOS POR PROVEEDOR ',  ;
   'REPORTE'
@ 2, 63 SAY PROGRAM()
wrk_desde = DATE()
wrk_hasta = DATE()
STORE SPACE(11) TO wrk_entida,  ;
      wrk_var
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
STORE 'B   ' TO wrk_estad
STORE SPACE(10) TO titul
ACTIVATE SCREEN
DO WHILE .T.
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 03 SAY  ;
       'C¢digo Proveedor  :'
     @ 02, 03 SAY  ;
       'Estado de Producto:'
     @ 03, 03 SAY  ;
       'Fecha  Desde      :'
     @ 04, 03 SAY  ;
       'Fecha  Hasta      :'
     @ 05, 03 SAY  ;
       'Por Detalle/Res£men   :'
     @ 06, 03 SAY  ;
       'Por Pantalla/Impresora:'
     @ 00, 27 GET wrk_entida  ;
       PICTURE '@!' VALID  ;
       busca('P',wrk_entida,1)  ;
       WHEN oowhen(1)
     READ
     IF LASTKEY() = 27
          SET RELATION TO
          CLOSE DATABASES
          ON KEY LABEL F6
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer,  ;
                     cabecera
          RELEASE WINDOW  ;
                  winmensaje
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     wrk_desent = cli_razsoc
     @ 02, 27 GET wrk_estad  ;
       PICTURE '@!' VALID  ;
       busca('EPRO',wrk_estad,2)  ;
       WHEN oowhen(2)
     @ 03, 27 GET wrk_desde
     @ 04, 27 GET wrk_hasta
     @ 05, 27 GET output PICTURE  ;
       '@m Pantalla,Impresora'
     @ 06, 27 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     IF wrk_estad = 'B'
          estad = 'GC_DCO00.DCO_CANBOR>0'
     ELSE
          IF wrk_estad = 'C'
               estad = 'GC_DCO00.DCO_CANCON>0'
          ELSE
               IF wrk_estad = 'S'
                    estad = 'GC_DCO00.CANSOL>0'
               ELSE
                    estad = 'GC_DCO00.DCO_CANCEL>0'
               ENDIF
          ENDIF
     ENDIF
     = ooaviso( ;
       'PREPARANDO INFORMACION . .' ;
       )
     SELECT disti GC_HCO00.HCO_NRODOC,;
GC_HCO00.HCO_FECDOC, GC_HCO00.HCO_CODENT,;
 GC_HCO00.HCO_CODMON, GC_DCO00.DCO_CODPRO,;
GC_DCO00.DCO_CODUNI,  GC_DCO00.DCO_CANSOL,;
GC_DCO00.DCO_CONPRE, GC_DCO00.DCO_CANBOR,;
 GC_DCO00.DCO_CODPRP, GC_DCO00.DCO_CANDES,;
GC_DCO00.DCO_CANCON,  GC_HCO00.HCO_CODTRA,;
GC_HCO00.HCO_FECLLE, GC_HCO00.HCO_FECCON,;
GC_DCO00.DCO_CANCEL, GC_DCO00.DCO_FECLLE;
FROM  GC_HCO00, GC_DCO00 WHERE;
 GC_HCO00.HCO_NRODOC = GC_DCO00.DCO_NRODOC;
AND  GC_HCO00.HCO_CODENT = wrk_entidad;
AND  (GC_HCO00.HCO_FECDOC >= wrk_desde;
AND GC_HCO00.HCO_FECDOC <= wrk_hasta);
AND     &estad ORDER BY GC_DCO00.DCO_CODPRP,;
GC_DCO00.DCO_NRODOC INTO CURSOR QUERY
     COUNT TO wrk_conta
     IF wrk_conta = 0
          DEACTIVATE WINDOW  ;
                     winmensaje
          DO p_mensaje WITH  ;
             'No Existen Registros a Listar'
          LOOP
     ENDIF
     IF output = 'Impresora'
          = ooaviso( ;
            'I M P R I M I E N D O . . .' ;
            )
          SET RELATION TO dco_codprp INTO;
gc_pro00
          ??? CHR(15)
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      AGCR2032  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR2032 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DEACTIVATE WINDOW  ;
                     winmensaje
     ELSE
          SET RELATION TO dco_codprp INTO;
gc_pro00
          wrk_txt = f_texto()
          IF tipo = 'Res£men'
               REPO FORM AGCR2032 TO FILE;
&wrk_txt NOCONSOLE SUMMARY
          ELSE
               REPO FORM AGCR2032 TO FILE;
&wrk_txt NOCONSOLE
          ENDIF
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &wrk_txt window pantall;
noedit
          SET SYSMENU OFF
          delete file &wrk_txt
     ENDIF
ENDDO
RETURN
*
FUNCTION busca
PARAMETER wrk_busca, wrk_codent,  ;
          opcion
IF opcion = 1
     SELECT gc_cli00
     IF  .NOT. SEEK(wrk_busca +  ;
         wrk_codent)
          DO p_mensaje WITH  ;
             'No existe C¢digo de Proveedor'
          RETURN .F.
     ELSE
          @ 1, 03 SAY REPLICATE( ;
            ' ', 20)
          @ 1, 03 SAY  ;
            LEFT(gc_cli00.cli_razsoc,  ;
            20)
     ENDIF
ELSE
     SELECT ge_tab0
     IF  .NOT. SEEK(wrk_busca +  ;
         wrk_codent)
          DO p_mensaje WITH  ;
             'No existe'
          RETURN .F.
     ELSE
          @ 02, 32 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            13)
          titul = SUBSTR(tab_destab,  ;
                  1, 10)
     ENDIF
ENDIF
RETURN
*
PROCEDURE oowhen
PARAMETER opcion
IF opcion = 1
     wrk_busca = 'P'
     wrk_var = 'M.HCO_CODENT'
     ON KEY LABEL F6 DO entida WITH WRK_BUSCA,WRK_VAR
ELSE
     wrk_busca = 'EPRO'
     wrk_var = 'WRK_ESTAD'
     ON KEY LABEL F6 DO ootab  WITH WRK_BUSCA,WRK_VAR
ENDIF
RETURN
*
PROCEDURE entida
PARAMETER wrk_busca, wrk_var
DO entidad WITH wrk_busca,  ;
   wrk_var
wrk_entida = cli_codigo
RETURN
*
FUNCTION ootab
PARAMETER wrk_busca, wrk_var
des_tab = ALLTRIM(b_tit_tab(wrk_busca))
DEFINE WINDOW bus_tab FROM 08, 23  ;
       TO 18, 63 TITLE des_tab IN  ;
       screen FOOTER  ;
       ' [F6] B£squeda x Descr. '  ;
       COLOR SCHEME 8
DEFINE WINDOW b_tabla FROM 12, 31  ;
       TO 14, 54 TITLE ' Tablas '  ;
       IN screen
SELECT ge_tab0
SET FILTER TO tab_codpre = wrk_busca
GOTO TOP
ACTIVATE WINDOW bus_tab
ON KEY LABEL ENTER DO CARGA_CODT
ON KEY LABEL F6 DO BUSCA_CODT
BROWSE FIELDS tab_codtab :R :H =  ;
       'C¢digo', tab_destab :R :H =  ;
       '  Descripci¢n ' IN  ;
       bus_tab COLOR SCHEME 7
ON KEY
RELEASE WINDOW bus_tab
SET FILTER TO
SET ORDER TO 1
RETURN wrk_var
*
*** 
*** ReFox - retrace your steps ... 
***
