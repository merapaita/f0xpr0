*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 08,  ;
       20 TO 15, 60 COLOR SCHEME  ;
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
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'PEDIDOS POR PROVEEDOR',  ;
   'REPORTE'
@ 2, 63 SAY PROGRAM()
wrk_desde = DATE()
wrk_hasta = DATE()
STORE SPACE(11) TO wrk_entida,  ;
      wrk_var
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
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
       'Fecha  Desde      :'
     @ 03, 03 SAY  ;
       'Fecha  Hasta      :'
     @ 04, 03 SAY  ;
       'Por Detalle/Res£men   :'
     @ 05, 03 SAY  ;
       'Por Pantalla/Impresora:'
     @ 00, 27 GET wrk_entida  ;
       PICTURE '@!' VALID  ;
       busca('P',wrk_entida) WHEN  ;
       oowhen()
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
     @ 02, 27 GET wrk_desde
     @ 03, 27 GET wrk_hasta
     @ 04, 27 GET tipo PICTURE  ;
       '@m Detalle,Res£men'
     @ 05, 27 GET output PICTURE  ;
       '@m Pantalla,Impresora'
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     = ooaviso( ;
       'PREPARANDO INFORMACION . .' ;
       )
     SELECT DISTINCT  ;
            gc_hco00.hco_nrodoc,  ;
            gc_hco00.hco_fecdoc,  ;
            gc_hco00.hco_codent,  ;
            gc_hco00.hco_codmon,  ;
            gc_dco00.dco_codpro,  ;
            gc_dco00.dco_coduni,  ;
            gc_dco00.dco_cansol,  ;
            gc_dco00.dco_conpre,  ;
            gc_dco00.dco_canbor,  ;
            gc_dco00.dco_codprp,  ;
            gc_dco00.dco_candes,  ;
            gc_dco00.dco_cancon,  ;
            gc_hco00.hco_codtra,  ;
            gc_hco00.hco_feclle,  ;
            gc_hco00.hco_feccon  ;
            FROM GC_HCO00,  ;
            GC_DCO00 WHERE  ;
            gc_hco00.hco_nrodoc =  ;
            gc_dco00.dco_nrodoc  ;
            AND  ;
            gc_hco00.hco_codent =  ;
            wrk_entida AND  ;
            (gc_hco00.hco_fecdoc >=  ;
            wrk_desde AND  ;
            gc_hco00.hco_fecdoc <=  ;
            wrk_hasta) ORDER BY  ;
            gc_hco00.hco_codent,  ;
            gc_hco00.hco_nrodoc,  ;
            gc_dco00.dco_codprp  ;
            INTO CURSOR QUERY
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
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      AGCR2031  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR2031 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDIF
          DEACTIVATE WINDOW  ;
                     winmensaje
     ELSE
          SET RELATION TO dco_codprp INTO;
gc_pro00
          IF tipo = 'Res£men'
               REPORT FORMAT  ;
                      AGCR2031  ;
                      SUMMARY TO  ;
                      FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR2031 TO  ;
                      FILE  ;
                      TEXT4.TXT  ;
                      NOCONSOLE
          ENDIF
          SET SYSMENU ON
          MODIFY COMMAND  ;
                 text4.txt NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
ENDDO
RETURN
*
FUNCTION busca
PARAMETER wrk_busca, wrk_codent
SELECT gc_cli00
IF  .NOT. SEEK(wrk_busca +  ;
    wrk_codent)
     DO p_mensaje WITH  ;
        'No existe C¢digo de Proveedor'
     RETURN .F.
ELSE
     @ 1, 03 SAY REPLICATE(' ',  ;
       20)
     @ 1, 03 SAY  ;
       LEFT(gc_cli00.cli_razsoc,  ;
       20)
ENDIF
RETURN
*
PROCEDURE oowhen
wrk_busca = 'P'
wrk_var = 'M.HCO_CODENT'
ON KEY LABEL F6 DO entida WITH WRK_BUSCA,WRK_VAR
RETURN
*
PROCEDURE entida
PARAMETER wrk_busca, wrk_var
DO entidad WITH wrk_busca,  ;
   wrk_var
wrk_entida = cli_codigo
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
