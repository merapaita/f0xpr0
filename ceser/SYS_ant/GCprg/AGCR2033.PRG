*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 06,  ;
       10 TO 18, 70 COLOR SCHEME  ;
       24
DEFINE POPUP buscar FROM 12, 30  ;
       TO 18, 65 PROMPT FIELDS  ;
       cli_codigo + '³' +  ;
       cli_razsoc MARK CHR(16)  ;
       MARGIN IN screen SCROLL  ;
       COLOR SCHEME 7
ON SELECTION POPUP buscar DO CHOICE0
SELECT 1
USE SHARED GC_CLI00 ORDER CODIGO
SELECT 2
USE SHARED gc_pro00 ORDER codigo
SELECT 3
USE SHARED GC_DCO00 ORDER CODIGO
SELECT 4
USE SHARED GC_HCO00 ORDER  ;
    HCO_CODEN
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'NIVEL DE SERVICIO',  ;
   'REPORTE'
@ 2, 63 SAY PROGRAM()
wrk_desde = DATE()
wrk_hasta = DATE()
STORE SPACE(11) TO wrk_enti1,  ;
      wrk_var, wrk_enti2
STORE 0 TO num
ACTIVATE SCREEN
sigue = .T.
wrk_desde = DATE()
wrk_hasta = DATE()
STORE SPACE(11) TO wrk_enti1,  ;
      wrk_var, wrk_enti2
STORE 'IMPRESORA' TO output
STORE 'DETALLE' TO tipo
DO WHILE sigue
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     ON KEY LABEL F6 DO OOBUSCAR;
 WITH 1
     @ 00, 01 SAY  ;
       'COD. PROVEEDOR INI.:'
     @ 02, 01 SAY  ;
       'COD. PROVEEDOR FIN.:'
     @ 04, 01 SAY  ;
       'FECHA DESDE        :'
     @ 06, 01 SAY  ;
       'FECHA HASTA        :'
     @ 08, 01 SAY  ;
       'DETALLE/RESUMEN    :'
     @ 10, 01 SAY  ;
       'PANTALLA/IMPRESORA :'
     @ 00, 24 GET wrk_enti1  ;
       PICTURE '@!' VALID  ;
       busca('P',wrk_enti1,0)
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
          sigue = .F.
          RETURN
     ENDIF
     ON KEY LABEL F6 DO OOBUSCAR WITH;
2
     wrk_desent = cli_razsoc
     @ 02, 24 GET wrk_enti2  ;
       PICTURE '@!' VALID  ;
       busca('P',wrk_enti2,2)
     @ 04, 24 GET wrk_desde
     @ 06, 24 GET wrk_hasta
     @ 08, 24 GET tipo PICTURE  ;
       '@m DETALLE,RESUMEN'
     @ 10, 24 GET output PICTURE  ;
       '@m PANTALLA,IMPRESORA'
     READ
     IF LASTKEY() = 27
          LOOP
     ENDIF
     DO llena
     SELECT query
     COUNT TO wrk_conta
     IF wrk_conta = 0
          DEACTIVATE WINDOW  ;
                     winmensaje
          DO p_mensaje WITH  ;
             'NO EXISTEN REGISTROS A LISTAR'
          LOOP
     ENDIF
     GOTO TOP
     SET RELATION TO dco_codprp INTO gc_pro00,;
'P' + hco_codent INTO gc_cli00 ADDITIVE
     IF output = 'IMPRESORA'
          = ooaviso( ;
            'I M P R I M I E N D O . . .' ;
            )
          IF tipo = 'RESUMEN'
               REPORT FORMAT  ;
                      AGCR2033.FR1  ;
                      SUMMARY TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR2033.FRX  ;
                      TO PRINTER  ;
                      NOCONSOLE
          ENDIF
          DEACTIVATE WINDOW  ;
                     winmensaje
     ELSE
          IF tipo = 'RESUMEN'
               REPORT FORMAT  ;
                      AGCR2033.FR1  ;
                      SUMMARY TO  ;
                      FILE  ;
                      T2033.TXT  ;
                      NOCONSOLE
          ELSE
               REPORT FORMAT  ;
                      AGCR2033.FRX  ;
                      TO FILE  ;
                      T2033.TXT  ;
                      NOCONSOLE
          ENDIF
          SET SYSMENU ON
          MODIFY COMMAND  ;
                 t2033.txt NOEDIT  ;
                 WINDOW pantall
          SET SYSMENU OFF
     ENDIF
     DEACTIVATE WINDOW winmensaje
ENDDO
SET RELATION TO
CLOSE DATABASES
ON KEY LABEL F6
DEACTIVATE WINDOW tablas, codigo,  ;
           footer, cabecera
RELEASE WINDOW winmensaje
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION busca
PARAMETER wrk_busca, wrk_codent,  ;
          fila
IF fila = 2 .AND. wrk_enti2 <  ;
   wrk_enti1
     DO p_mensaje WITH  ;
        'CODIGO PROVEEDOR MENOR AL INICIAL'
     RETURN .F.
ENDIF
SELECT gc_cli00
IF  .NOT. SEEK(wrk_busca +  ;
    wrk_codent)
     DO p_mensaje WITH  ;
        'NO EXISTE CODIGO DE PROVEEDOR'
     RETURN .F.
ELSE
     @ fila, 38 SAY REPLICATE(' ',  ;
       20)
     @ fila, 38 SAY  ;
       LEFT(gc_cli00.cli_razsoc,  ;
       20) COLOR N/W 
ENDIF
RETURN
*
PROCEDURE oobuscar
PARAMETER num
SELECT DISTINCT cli_codigo +  ;
       ' ³ ' + cli_razsoc FROM  ;
       gc_cli00 WHERE cli_tpper =  ;
       'P' INTO ARRAY arrayprov
titulo = '   CODIGO                DESCRIPCION              '
ACTIVATE WINDOW marco
ACTIVATE WINDOW busqueda
@ 00, 00 SAY titulo COLOR N/W 
@ 01, 00 GET getorden DEFAULT  ;
  arrayprov SIZE 12, 51 FROM  ;
  arrayprov VALID oovalor(num, ;
  getorden) COLOR SCHEME 8
READ
DEACTIVATE WINDOW busqueda, marco
RETURN
*
PROCEDURE oovalor
PARAMETER opc, cdato
IF LASTKEY() == 13
     IF opc = 1
          wrk_enti1 = SUBSTR(cdato,  ;
                      1, 11)
     ELSE
          wrk_enti2 = SUBSTR(cdato,  ;
                      1, 11)
     ENDIF
ENDIF
RETURN
*
PROCEDURE llena
= ooaviso( ;
  'PREPARANDO INFORMACION . .')
SELECT DISTINCT  ;
       gc_hco00.hco_nrodoc,  ;
       gc_hco00.hco_fecdoc,  ;
       gc_hco00.hco_codent,  ;
       gc_hco00.hco_feclle,  ;
       gc_hco00.hco_feccon,  ;
       gc_hco00.hco_indest,  ;
       gc_dco00.dco_codpro,  ;
       gc_dco00.dco_coduni,  ;
       gc_dco00.dco_cansol,  ;
       gc_dco00.dco_conpre,  ;
       gc_dco00.dco_codprp,  ;
       gc_dco00.dco_candes,  ;
       gc_dco00.dco_feclle FROM  ;
       GC_HCO00, GC_DCO00 WHERE  ;
       gc_hco00.hco_nrodoc =  ;
       gc_dco00.dco_nrodoc AND  ;
       (gc_hco00.hco_codent >=  ;
       wrk_enti1 AND  ;
       gc_hco00.hco_codent <=  ;
       wrk_enti2) AND  ;
       (gc_hco00.hco_fecdoc >=  ;
       wrk_desde AND  ;
       gc_hco00.hco_fecdoc <=  ;
       wrk_hasta) AND  NOT  ;
       EMPTY(gc_dco00.dco_feclle)  ;
       AND gc_hco00.hco_indest =  ;
       'C' ORDER BY  ;
       gc_hco00.hco_codent,  ;
       gc_hco00.hco_nrodoc,  ;
       gc_dco00.dco_codprp INTO  ;
       CURSOR QUERY
RETURN
*
PROCEDURE llenatem
CREATE CURSOR QUERY (hco_nrodoc C  ;
       (10), hco_fecdoc D,  ;
       hco_codent C (9),  ;
       hco_feclle D, hco_feccon D,  ;
       hco_indest C (1),  ;
       dco_codpro C (14),  ;
       dco_coduni C (4),  ;
       dco_cansol N (9, 2),  ;
       dco_conpre N (9, 2),  ;
       dco_codprp C (14),  ;
       dco_candes N (9, 2),  ;
       dco_feclle D)
SELECT gc_hco00
SEEK wrk_enti1
SCAN WHILE (hco_codent >=  ;
     wrk_enti1 .AND. hco_codent <=  ;
     wrk_enti2)
     IF (hco_fecdoc >= wrk_desde  ;
        .AND. hco_fecdoc <=  ;
        wrk_hasta) .AND.  ;
        hco_indest = 'C'
          SELECT gc_dco00
          SEEK gc_hco00.hco_nrodoc
          BROWSE
          SCAN WHILE dco_nrodoc =  ;
               gc_hco00.hco_nrodoc  ;
               .AND.  .NOT.  ;
               EOF()
               IF  .NOT.  ;
                   EMPTY(gc_dco00.dco_feclle)
                    SELECT query
                    REPLACE hco_nrodoc  ;
                            WITH  ;
                            gc_hco00.hco_nrodoc,  ;
                            hco_fecdoc  ;
                            WITH  ;
                            gc_hco00.hco_fecdoc,  ;
                            hco_codent  ;
                            WITH  ;
                            gc_hco00.hco_codent,  ;
                            hco_feclle  ;
                            WITH  ;
                            gc_hco00.hco_feclle
                    REPLACE hco_feccon  ;
                            WITH  ;
                            gc_hco00.hco_feccon,  ;
                            hco_indest  ;
                            WITH  ;
                            gc_hco00.hco_indest,  ;
                            dco_codpro  ;
                            WITH  ;
                            gc_dco00.dco_codpro,  ;
                            dco_coduni  ;
                            WITH  ;
                            gc_dco00.dco_coduni
                    REPLACE dco_cansol  ;
                            WITH  ;
                            gc_dco00.dco_cansol,  ;
                            dco_conpre  ;
                            WITH  ;
                            gc_dco00.dco_conpre,  ;
                            dco_codprp  ;
                            WITH  ;
                            gc_dco00.dco_codprp,  ;
                            dco_candes  ;
                            WITH  ;
                            gc_dco00.dco_candes
                    REPLACE dco_feclle  ;
                            WITH  ;
                            gc_dco00.dco_feclle
                    BROWSE
               ENDIF
          ENDSCAN
     ENDIF
     SELECT gc_hco00
ENDSCAN
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
