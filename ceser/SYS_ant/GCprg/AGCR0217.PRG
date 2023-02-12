*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PUBLIC wrk_campo, wrk_selpro,  ;
       wrk_selec, wrk_cadpro,  ;
       wrk_codpro, wrk_cadbas,  ;
       wrk_estado, wrk_titulo,  ;
       danobase, wrk_tip1,  ;
       wrk_tip2, wrk_ifc,  ;
       wrk_evta
DIMENSION wrk_ifc( 12), wrk_evta(  ;
          12)
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
   'ESTADISTICAS DE COMPRA/VENTA',  ;
   'REPORTE'
wrk_progra = PROGRAM()
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000001000011', 2
= ooopen('GC_CLI00',1)
= ooopen('GC_VND00',1)
= ooopen('GE_TAB0',1)
= ooopen('GC_KAR00',1)
= ooopen('GC_PRO00',1)
wrk_selpro = SELECT()
DEFINE WINDOW winpedido FROM 4, 1  ;
       TO 21, 78 NOFLOAT NOCLOSE  ;
       NOMINIMIZE NONE COLOR  ;
       SCHEME 1
wrk_codpro = SPACE(14)
danobase = 0
= ooscreen(1)
wrk_titulo = ' ESTADISTICAS DE COMPRAS '
danobase = YEAR(DATE())
SET CURSOR ON
@ 2, 2 GET nop1 DEFAULT 1 SIZE 1,  ;
  10, 0 PICTURE  ;
  '@*RVN Compra ;Venta  ;Ambos  '  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
@ 2, 33 GET danobase SIZE 1, 4, 0  ;
  VALID oovalid(VARREAD())
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
                    wrk_titulo = ' ESTADISTICAS DE COMPRAS '
               CASE nop1 = 2
                    wrk_titulo = '  ESTADISTICAS DE VENTA  '
               CASE nop1 = 3
                    wrk_titulo = 'ESTADISTICA COMPRA/VENTA '
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
               SELECT DISTINCT *  ;
                      FROM  ;
                      GC_KAR00  ;
                      WHERE  ;
                      gc_kar00.kar_codmov =  ;
                      'IFC' = .T.  ;
                      ORDER BY  ;
                      gc_kar00.kar_codpro  ;
                      INTO CURSOR  ;
                      REPVENTAS
          ENDIF
          IF (nop1 = 2)
               SELECT DISTINCT *  ;
                      FROM  ;
                      GC_KAR00  ;
                      WHERE  ;
                      gc_kar00.kar_codmov =  ;
                      'EVTA' =  ;
                      .T. ORDER  ;
                      BY  ;
                      gc_kar00.kar_codpro  ;
                      INTO CURSOR  ;
                      REPVENTAS
          ENDIF
          IF (nop1 = 3)
               SELECT DISTINCT *  ;
                      FROM  ;
                      GC_KAR00  ;
                      WHERE  ;
                      YEAR(gc_kar00.kar_fecing) =  ;
                      danobase  ;
                      AND  ;
                      (gc_kar00.kar_codmov =  ;
                      'EVTA' =  ;
                      .T. OR  ;
                      gc_kar00.kar_codmov =  ;
                      'IFC' =  ;
                      .T.) ORDER  ;
                      BY  ;
                      gc_kar00.kar_codpro,  ;
                      gc_kar00.kar_codmov  ;
                      INTO CURSOR  ;
                      REPVENTAS
          ENDIF
          COUNT TO nreg
          IF nreg = 0
               DEACTIVATE WINDOW  ;
                          winmensaje
               DO p_mensaje WITH  ;
                  ' NO SE ENCONTRO INFORMACION '
               RETURN
          ENDIF
          = ooimprime()
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
       ' I M P R I M I E N D O . . .' ;
       )
     IF (nop1 = 1) .OR. (nop1 =  ;
        2)
          REPORT FORMAT  ;
                 AGCR0217.FRX  ;
                 ENVIRONMENT  ;
                 PDSETUP NOEJECT  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ELSE
          REPORT FORMAT  ;
                 AGCR217A.FRX  ;
                 ENVIRONMENT  ;
                 PDSETUP NOEJECT  ;
                 TO PRINTER  ;
                 NOCONSOLE
     ENDIF
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
          @ 1, 22 SAY  ;
            'A¤o de Proceso :'  ;
            SIZE 1, 18, 0
          @ 2, 25 SAY 'Para :'  ;
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
