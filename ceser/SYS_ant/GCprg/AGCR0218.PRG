*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PUBLIC wrk_campo, wrk_selpro,  ;
       wrk_selec, wrk_cadpro,  ;
       wrk_codpro, wrk_cadran,  ;
       wrk_estado, wrk_titulo
DEFINE POPUP busca FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       tab_codtab + ' ³' +  ;
       tab_destab IN screen COLOR  ;
       SCHEME 12
DEFINE POPUP clien FROM 12, 27 TO  ;
       18, 53 PROMPT FIELDS ' ' +  ;
       cli_codigo + ' ³' +  ;
       cli_razsoc IN screen COLOR  ;
       SCHEME 12
ON SELECTION POPUP clien DEAC POPUP CLIEN;
      
ON SELECTION POPUP busca DEAC POPUP BUSCA
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'RANKING DE VENTA', 'REPORTE'
wrk_progra = PROGRAM()
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000001000011', 2
= ooopen('GC_CLI00',1)
= ooopen('GC_VND00',1)
= ooopen('GC_PRO00',1)
wrk_selpro = SELECT()
DEFINE WINDOW winpedido FROM 4, 1  ;
       TO 21, 78 NOFLOAT NOCLOSE  ;
       NOMINIMIZE NONE COLOR  ;
       SCHEME 1
wrk_codpro = SPACE(14)
nop1 = 1
wrk_titulo = 'RANKING DE VENTA POR PRODUCTO'
= ooscreen(1)
SET CURSOR ON
@ 2, 2 GET nop1 DEFAULT 1 SIZE 1,  ;
  16, 0 PICTURE  ;
  '@*RVN Por Producto ;Por Cliente;Por Proveedor;Por Vendedor'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
@ 2, 33 GET dfecini DEFAULT  ;
  DATE() SIZE 1, 10, 0 VALID  ;
  oovalid(VARREAD())
@ 3, 33 GET dfecfin DEFAULT  ;
  DATE() SIZE 1, 10, 0 VALID  ;
  oovalid(VARREAD())
@ 5, 36 GET nop2 DEFAULT 1 SIZE 1,  ;
  10, 1 PICTURE '@*HN Procesar'  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
READ CYCLE
RELEASE WINDOW winpedido
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen, nsalta
DO CASE
     CASE cwhen = 'WRK_CODPRO'
          wrk_selec = SELECT()
          wrk_campo = 'wrk_CodPro'
          ON KEY LABEL F6 DO PRODUC WITH;
WRK_CAMPO,WRK_SELEC,WRK_SELPRO
ENDCASE
RETURN
*
PROCEDURE oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'NOP1'
          DO CASE
               CASE nop1 = 1
                    wrk_titulo = 'RANKING DE VENTA  POR PRODUCTO'
               CASE nop1 = 2
                    wrk_cadran = ' GROUP BY GC_HVE00.HVE_CODENT '
                    wrk_titulo = 'RANKING  DE VENTA  POR CLIENTE'
               CASE nop1 = 3
                    wrk_cadran = ' GROUP BY GC_HVE00.HVE_CODENT '
                    wrk_titulo = 'RANKING DE VENTA POR PROVEEDOR'
               CASE nop1 = 4
                    wrk_cadran = ' GROUP BY GC_HVE00.HVE_CODVND '
                    wrk_titulo = 'RANKING DE VENTA  POR VENDEDOR'
          ENDCASE
     CASE cvalid = 'NOP2'
          IF nop2 = 2
               CLEAR READ
               RETURN
          ENDIF
          = ooaviso( ;
            ' T R A B A J A N D O . . .' ;
            )
          IF (nop1 = 1)
               SELECT DISTINCT  ;
                      gc_hve00.hve_fecdoc,  ;
                      gc_dve00.dve_tipdoc,  ;
                      gc_dve00.dve_nrodoc,  ;
                      gc_dve00.dve_propar,  ;
                      gc_dve00.dve_unimed,  ;
                      gc_dve00.dve_import,  ;
                      gc_dve00.dve_total,  ;
                      SUM(gc_dve00.dve_cantid)  ;
                      FROM  ;
                      GC_DVE00,  ;
                      GC_HVE00  ;
                      WHERE  ;
                      gc_hve00.hve_tipdoc =  ;
                      gc_dve00.dve_tipdoc  ;
                      AND  ;
                      gc_dve00.dve_nrodoc =  ;
                      gc_hve00.hve_nrodoc  ;
                      AND  ;
                      gc_hve00.hve_fecdoc >  ;
                      dfecini AND  ;
                      gc_hve00.hve_fecdoc <  ;
                      dfecfin  ;
                      GROUP BY  ;
                      gc_dve00.dve_propar  ;
                      ORDER BY 8  ;
                      DESCENDING  ;
                      INTO CURSOR  ;
                      RepRanking
          ENDIF
          IF (nop1 = 3)
               SELECT DISTINCT  ;
                      gc_hve00.*,  ;
                      gc_pro00.pro_codpve,  ;
                      SUM(gc_hve00.hve_totgen)  ;
                      FROM  ;
                      GC_HVE00,  ;
                      GC_DVE00,  ;
                      GC_PRO00  ;
                      WHERE  ;
                      gc_hve00.hve_tipdoc =  ;
                      gc_dve00.dve_tipdoc  ;
                      AND  ;
                      gc_hve00.hve_nrodoc =  ;
                      gc_dve00.dve_nrodoc  ;
                      AND  ;
                      gc_dve00.dve_propar =  ;
                      gc_pro00.pro_codpro  ;
                      GROUP BY  ;
                      gc_pro00.pro_codpve  ;
                      ORDER BY 28  ;
                      DESCENDING  ;
                      INTO CURSOR  ;
                      RepRanking
          ENDIF
          IF (nop1 = 2) .OR.  ;
             (nop1 = 4)
               SELECT DISTINCT GC_HVE00.HVE_TIPDOC,;
GC_HVE00.HVE_NRODOC, GC_HVE00.HVE_FECDOC,;
GC_HVE00.HVE_FECVCT, GC_HVE00.HVE_ALMDES,;
GC_HVE00.HVE_TIPENT, GC_HVE00.HVE_CODENT,;
GC_HVE00.HVE_TIDORE, GC_HVE00.HVE_NRDORE,;
GC_HVE00.HVE_TOTNET, GC_HVE00.HVE_TOTIGV,;
GC_HVE00.HVE_TOTOIM, GC_HVE00.HVE_PORDES,;
GC_HVE00.HVE_TOTDES, GC_HVE00.HVE_TOTGEN,;
GC_HVE00.HVE_CODEMI, GC_HVE00.HVE_CODVND,;
GC_HVE00.HVE_TIPPAG, GC_HVE00.HVE_LISPRE,;
GC_HVE00.HVE_FLETE, GC_HVE00.HVE_EMBALA,;
GC_HVE00.HVE_CARGO, GC_HVE00.HVE_ESTDOC,;
GC_DVE00.DVE_TIPDOC, GC_DVE00.DVE_NRODOC,;
GC_DVE00.DVE_PROPAR, GC_DVE00.DVE_UNIMED,;
GC_DVE00.DVE_CANTID, GC_DVE00.DVE_PORDES,;
GC_DVE00.DVE_IMPORT, GC_DVE00.DVE_TOTAL,;
SUM(GC_HVE00.HVE_TOTGEN) FROM GC_HVE00,;
GC_DVE00 WHERE GC_HVE00.HVE_TIPDOC = GC_DVE00.DVE_TIPDOC;
AND GC_HVE00.HVE_NRODOC = GC_DVE00.DVE_NRODOC;
 AND GC_HVE00.HVE_FECDOC > dFecIni;
 AND GC_HVE00.HVE_FECDOC < dFecFin &wrk_CadRan;
 ORDER BY 32 DESCENDING  INTO CURSOR RepRanking
          ENDIF
          COUNT TO nreg
          IF nreg = 0
               DEACTIVATE WINDOW  ;
                          winmensaje
               DO p_mensaje WITH  ;
                  ' NO SE ENCONTRO INFORMACION '
          ENDIF
          = ooimprime()
          ACTIVATE WINDOW  ;
                   winpedido
     CASE cvalid = 'WRK_CODPRO'
          wrk_area = SELECT()
          = ooareat('GC_PRO00', ;
            'CODIGO')
          SEEK wrk_codpro
          IF FOUND()
               wrk_descri = gc_pro00.pro_descri
               SHOW GET  ;
                    wrk_descri
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE PRODUCTO NO EXISTE '
               SELECT (wrk_area)
          ENDIF
          SELECT (wrk_area)
ENDCASE
RETURN
*
PROCEDURE ooimprime
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
               EXIT
          ENDIF
     ENDIF
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     REPORT FORMAT AGCR0218.FRX  ;
            ENVIRONMENT PDSETUP  ;
            NOEJECT TO PRINTER  ;
            NOCONSOLE
     RELEASE WINDOW winmensaje
     EXIT
ENDDO
DO p_mensaje WITH  ;
   ' FIN DE IMPRESION '
RETURN
*
FUNCTION ootipdes
PARAMETER ntipo, ccodigo
DO CASE
     CASE ntipo = 1
          RETURN oodespro(ccodigo)
     CASE ntipo = 2
          RETURN oodescli(ccodigo)
     CASE ntipo = 3
          RETURN oodespve(ccodigo)
     CASE ntipo = 4
          RETURN oodesvnd(ccodigo)
ENDCASE
RETURN
*
PROCEDURE ooscreen
PARAMETER wrk_screen
DO CASE
     CASE wrk_screen = 1
          ACTIVATE WINDOW  ;
                   winpedido
          @ 1, 22 SAY 'Fechas :'  ;
            SIZE 1, 8, 0
          @ 2, 25 SAY 'Desde :'  ;
            SIZE 1, 7, 0
          @ 3, 25 SAY 'Hasta :'  ;
            SIZE 1, 7, 0
          @ 1, 1 SAY  ;
            'Tipos Ranking :'  ;
            SIZE 1, 16, 0
          @ 6, 0 TO 17, 77
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
