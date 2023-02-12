*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE SPACE(30) TO estades
STORE 0 TO wrk_totalf, wrk_totals,  ;
      wrk_totalr, ml
STORE SPACE(10) TO estades
CLOSE DATABASES
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER CODIGO
SELECT 3
USE SHARED gc_hco00 ORDER codigo
SELECT 4
USE SHARED GC_DCO00 ORDER  ;
    dco_codprp
SET RELATION TO dco_nrodoc INTO gc_hco00,;
'IMPO' + dco_indest INTO ge_tab0
SELECT 5
USE SHARED gc_alm00 ORDER codigo
SET RELATION TO 'ALMA' + alm_codalm INTO;
ge_tab0 ADDITIVE
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_dco00
wrk_selec = SELECT()
wrk_selpro = SELECT()
SET NEAR ON
DEFINE WINDOW ven1 FROM 00, 00 TO  ;
       21, 79 TITLE  ;
       'Consulta de pedido por c¢digo'  ;
       IN screen COLOR SCHEME 12
DEFINE WINDOW ven2 FROM 07, 01 TO  ;
       20, 78 IN screen NONE
DEFINE WINDOW ven3 FROM 08, 10 TO  ;
       19, 73 IN screen COLOR  ;
       SCHEME 10
DO p_footer WITH  ;
   '1000100000000001100011101',  ;
   2
codigo = SPACE(14)
wrk_codigo = SPACE(14)
ACTIVATE WINDOW ven1
GOTO TOP
DO dato
tmp_x = 0
DO WHILE tmp_x<>27
     tmp_x = INKEY(0, 'H')
     DO CASE
          CASE tmp_x = -5
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               wrk_campo = 'pro_codpro'
               DO produc WITH  ;
                  wrk_campo,  ;
                  wrk_selec,  ;
                  wrk_selpro
               DO dato
          CASE tmp_x = 1
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               GOTO TOP
               DO dato
          CASE tmp_x = 6
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               GOTO BOTTOM
               DO dato
          CASE tmp_x = 3
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               IF EOF()
                    GOTO BOTTOM
               ELSE
                    SKIP
               ENDIF
               DO dato
          CASE tmp_x = 18
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               IF BOF()
                    GOTO TOP
               ELSE
                    SKIP -1
               ENDIF
               DO dato
          CASE tmp_x = -4
               DO alma
               DEACTIVATE WINDOW  ;
                          ven3
     ENDCASE
ENDDO
SET NEAR OFF
SELECT gc_dco00
SET RELATION TO
SELECT gc_pro00
SET RELATION TO
SELECT gc_alm00
SET RELATION TO
DEACTIVATE WINDOW ven3
DEACTIVATE WINDOW ven1
DEACTIVATE WINDOW ven2
ON KEY LABEL f6
ON KEY LABEL esc
ON KEY
CLEAR READ
CLEAR GETS
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE dato
campo = SPACE(10)
STORE 0 TO wrk_totalf, wrk_totals,  ;
      wrk_totalr
ACTIVATE WINDOW ven1
SELECT gc_pro00
@ 00, 00 SAY 'C¢digo.......:' +  ;
  pro_codpro COLOR W+/N 
wrk_codigo = pro_codpro
SELECT gc_alm00
GOTO TOP
SUM FOR (alm_codpro = wrk_codigo  ;
    .AND. alm_codalm <> '0001')  ;
    alm_stkfis TO wrk_totalf
GOTO TOP
SUM FOR (alm_codpro = wrk_codigo  ;
    .AND. alm_codalm = '0001')  ;
    alm_stkfis TO wrk_totals
GOTO TOP
SUM FOR alm_codpro = wrk_codigo  ;
    alm_stkres TO wrk_totalr
SELECT gc_pro00
@ 01, 00 SAY 'Descripci¢n..:' +  ;
  pro_descri COLOR W+/N 
@ 01, 45 SAY  ;
  'Cod.de Reemplazo..:' +  ;
  pro_codree COLOR W+/N 
@ 02, 00 SAY  ;
  'Stock Solicitado..:' +  ;
  TRANSFORM(pro_stksol,  ;
  '999,999.99') COLOR W/N 
@ 03, 00 SAY  ;
  'Stock Tr nsito....:' +  ;
  TRANSFORM(pro_stktra,  ;
  '999,999.99') COLOR W/N 
@ 04, 00 SAY  ;
  'Stock Backorder...:' +  ;
  TRANSFORM(pro_stkbor,  ;
  '999,999.99') COLOR W/N 
@ 02, 45 SAY  ;
  'Stock Surquillo...:' +  ;
  TRANSFORM(wrk_totals,  ;
  '999,999.99') COLOR W/N 
@ 03, 45 SAY  ;
  'Stock Sucursales..:' +  ;
  TRANSFORM(wrk_totalf,  ;
  '999,999.99') COLOR W/N 
@ 04, 45 SAY  ;
  'Stock Reservado...:' +  ;
  TRANSFORM(wrk_totalr,  ;
  '999,999.99') COLOR W/N 
@ 05, 00 SAY  ;
  ' N§ Ped.   Cant.Ped. Estado Fec.Ped. Fec.Con.  Fec.LLeg. Factura   N§Orden   '  ;
  COLOR N/W 
ACTIVATE WINDOW ven2
DO p_mensaje WITH  ;
   'ESPERE UN MOMENTO'
SELECT gc_dco00
SET ORDER TO dco_codprp
GOTO TOP
SEEK wrk_codigo
ml = 0
DO WHILE dco_codprp=wrk_codigo  ;
   .AND.  .NOT. EOF()
     IF dco_codprp = dco_codpro  ;
        .OR. EMPTY(dco_codpro)
          ml = ml + 1
     ENDIF
     SKIP
ENDDO
SET ORDER TO dco_codpro
GOTO TOP
SEEK wrk_codigo
DO WHILE dco_codpro=wrk_codigo  ;
   .AND.  .NOT. EOF()
     IF dco_codprp <> dco_codpro
          ml = ml + 1
     ENDIF
     SKIP
ENDDO
IF ml = 0
     @ 05, 20 SAY SPACE(35)
     DO p_mensaje WITH  ;
        ' Repuesto no tiene pedido '
     SELECT gc_pro00
     RETURN
ENDIF
lm = 0
DIMENSION detalle( ml)
SELECT gc_dco00
SET ORDER TO dco_codprp
GOTO TOP
SEEK wrk_codigo
SCAN WHILE dco_codprp =  ;
     wrk_codigo .AND.  .NOT.  ;
     EOF()
     IF dco_codprp = dco_codpro  ;
        .OR. dco_codpro =  ;
        SPACE(14)
          lm = lm + 1
          detalle( lm) =  ;
                 dco_nrodoc + '³' +  ;
                 TRANSFORM(dco_cansol,  ;
                 '999.99') + '³' +  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 10) + '³' +  ;
                 DTOC(gc_hco00.hco_fecdoc) +  ;
                 '³' +  ;
                 DTOC(gc_hco00.hco_feccon) +  ;
                 '³' +  ;
                 DTOC(dco_feclle) +  ;
                 '³' + dco_numfac +  ;
                 '³' + dco_inorig +  ;
                 dco_docref
     ENDIF
ENDSCAN
SET ORDER TO dco_codpro
GOTO TOP
SEEK wrk_codigo
SCAN WHILE dco_codpro =  ;
     wrk_codigo .AND.  .NOT.  ;
     EOF()
     IF dco_codprp <> dco_codpro
          lm = lm + 1
          detalle( lm) =  ;
                 dco_nrodoc + '³' +  ;
                 TRANSFORM(dco_cansol,  ;
                 '999.99') + '³' +  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 10) + '³' +  ;
                 DTOC(gc_hco00.hco_fecdoc) +  ;
                 '³' +  ;
                 DTOC(gc_hco00.hco_feccon) +  ;
                 '³' +  ;
                 DTOC(dco_feclle) +  ;
                 '³' + dco_numfac +  ;
                 '³' + dco_inorig +  ;
                 dco_docref
     ENDIF
ENDSCAN
@ 00, 00 GET detalles DEFAULT  ;
  detalle(1) SIZE 15, 78 FROM  ;
  detalle
SELECT gc_pro00
RETURN
*
PROCEDURE alma
ACTIVATE WINDOW ven3
SELECT gc_alm00
SEEK wrk_codigo
BROWSE FIELDS alm_codalm : 6 :H =  ;
       'CODIGO',  ;
       ge_tab0.tab_destab : 20 :H =  ;
       'ALMACEN ', alm_stkfis :  ;
       10 :P = '999,999' :H =  ;
       'FISICO    ', alm_stkres :  ;
       10 :P = '999,999' :H =  ;
       'RESERVA  ', v =  ;
       alm_stkfis - alm_stkres :  ;
       11 :P = '999,999' :H =  ;
       'DISPONIBLE' NOEDIT KEY  ;
       wrk_codigo IN ven3
DEACTIVATE WINDOW ven3
SELECT gc_pro00
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
