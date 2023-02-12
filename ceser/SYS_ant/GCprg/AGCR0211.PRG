*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
DEFINE POPUP tablas FROM 07, 00  ;
       TO 14, 30 PROMPT FIELDS  ;
       tab_destab IN screen COLOR  ;
       SCHEME 20
ON SELECTION POPUP tablas DO PROCESO1
DEFINE WINDOW venta01 FROM 15, 10  ;
       TO 18, 30 IN screen COLOR  ;
       SCHEME 20
wrk_progra = PROGRAM()
SELECT ge_tab0.tab_destab,  ;
       ge_tab0.tab_codpre,  ;
       ge_tab0.tab_codtab FROM  ;
       GE_TAB0 WHERE  ;
       ge_tab0.tab_codpre =  ;
       'CENC' ORDER BY  ;
       ge_tab0.tab_destab INTO  ;
       CURSOR QUERY
ACTIVATE POPUP tablas
CLOSE DATABASES
RELEASE POPUP tablas
ACTIVATE SCREEN
RETURN
*
PROCEDURE proceso1
ACTIVATE WINDOW venta01
wrk_cotab = tab_codtab
wrk_fecdes = DATE()
wrk_fechas = DATE()
SET CURSOR ON
@ 0, 0 SAY ' Desde :'
@ 1, 0 SAY ' Hasta :'
@ 0, 9 GET wrk_fecdes PICTURE  ;
  '99/99/99'
@ 1, 9 GET wrk_fechas PICTURE  ;
  '99/99/99' VALID proceso2()
READ CYCLE
DEACTIVATE WINDOW venta01
SELECT ge_tab0.tab_destab,  ;
       ge_tab0.tab_codpre,  ;
       ge_tab0.tab_codtab FROM  ;
       GE_TAB0 WHERE  ;
       ge_tab0.tab_codpre =  ;
       'CENC' ORDER BY  ;
       ge_tab0.tab_destab INTO  ;
       CURSOR QUERY
RETURN
*
PROCEDURE proceso2
SELECT gc_kar00.kar_fecing,  ;
       gc_kar00.kar_tipdoc,  ;
       gc_kar00.kar_nrodoc,  ;
       gc_kar00.kar_lindet,  ;
       gc_kar00.kar_codpro,  ;
       gc_kar00.kar_unimed,  ;
       gc_kar00.kar_cantid,  ;
       gc_hip00.hip_cencon,  ;
       ge_tab0.tab_destab,  ;
       gc_pro00.pro_descri,  ;
       gc_pro00.pro_unimed FROM  ;
       GC_KAR00, GC_HIP00,  ;
       GE_TAB0, GC_PRO00 WHERE  ;
       gc_hip00.hip_tipdoc =  ;
       gc_kar00.kar_tipdoc AND  ;
       gc_hip00.hip_nrodoc =  ;
       gc_kar00.kar_nrodoc AND  ;
       gc_pro00.pro_codpro =  ;
       gc_kar00.kar_codpro AND  ;
       (gc_kar00.kar_codmov =  ;
       'COI ' AND  ;
       gc_hip00.hip_cencon =  ;
       wrk_cotab AND  ;
       ge_tab0.tab_codpre =  ;
       'CENC' AND  ;
       ge_tab0.tab_codtab =  ;
       wrk_cotab AND  ;
       gc_kar00.kar_fecing >=  ;
       wrk_fecdes AND  ;
       gc_kar00.kar_fecing <=  ;
       wrk_fechas) INTO CURSOR  ;
       CONSUMO2
COUNT TO wrk_reg
IF wrk_reg = 0
     DO p_mensaje WITH  ;
        'No hay Datos entre estas Fechas'
ELSE
     GOTO TOP
     DO imprime
ENDIF
RETURN
*
PROCEDURE imprime
ON KEY
STORE 70 TO aux_lin
ON KEY
rpt = .T.
rpt = f_yesno( ;
      'PREPARE LA IMPRESORA Y CONFIRME ACCION' ;
      )
IF rpt
     stat = SYS(13)
     DO WHILE stat='OFFLINE'
          swt = f_yesno( ;
                'LA IMPRESORA NO ESTA LISTA, VERIFIQUE POR FAVOR' ;
                )
          IF swt
               stat = SYS(13)
          ELSE
               DEACTIVATE POPUP  ;
                          impresora
               SET CURSOR OFF
               RETURN
          ENDIF
     ENDDO
     SET DEVICE TO PRINTER
     DO imprimir
ENDIF
RETURN
*
PROCEDURE imprimir
DO r_titulo
@ aux_lin, 00 SAY CHR(27) + 'F'
wrk_despro = SPACE(14)
wrk_unipro = SPACE(4)
wrk_produc = SPACE(14)
DO WHILE  .NOT. EOF()
     IF aux_lin > 60
          DO r_titulo
     ENDIF
     aux_lin = aux_lin + 1
     @ aux_lin, 03 SAY kar_fecing
     @ aux_lin, 15 SAY kar_tipdoc
     @ aux_lin, 31 SAY kar_nrodoc
     @ aux_lin, 43 SAY kar_codpro
     @ aux_lin, 60 SAY pro_unimed
     @ aux_lin, 72 SAY pro_descri
     @ aux_lin, 115 SAY  ;
       kar_cantid
     SKIP
ENDDO
@ 61, 00 SAY ''
@ 00, 00 SAY CHR(27) + 'P'
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE r_titulo
@ 00, 00 SAY CHR(27) + 'F'
@ 00, 00 SAY CHR(20)
@ 00, 00 SAY CHR(15)
@ 00, 00 SAY rge_razsoc
@ 00, 107 SAY 'P═gina   :'
@ 00, 116 SAY _PAGENO
@ 01, 00 SAY rge_abrev
@ 01, 50 SAY  ;
  'CONSUMOS POR CENTRO DE COSTOS'
@ 01, 107 SAY 'Fecha    :'
@ 01, 118 SAY DATE()
@ 02, 50 SAY  ;
  'ммммммммммммммммммммммммммммм'
@ 02, 107 SAY 'Programa :'
@ 02, 118 SAY wrk_progra
@ 03, 01 SAY  ;
  'Centro de Consumo : ' +  ;
  hip_cencon + ' ' + tab_destab
@ 04, 01 SAY  ;
  'Fecha Desde       : ' +  ;
  DTOC(wrk_fecdes)
@ 05, 01 SAY  ;
  'Fecha Hasta       : ' +  ;
  DTOC(wrk_fechas)
@ 06, 00 SAY  ;
  'здддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©'
@ 07, 00 SAY  ;
  'Ё    FECHA     CODIGO       NUMERO           CODIGO         UND.             DESCRIPCION                        CANTIDAD       Ё'
@ 08, 00 SAY  ;
  'Ё   CONSUMO   DOCUMENTO    DOCUMENTO        PRODUCTO        INV.                                                               Ё'
@ 09, 00 SAY  ;
  'юдддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды'
aux_lin = 10
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
