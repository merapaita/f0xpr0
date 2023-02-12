*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE SPACE(30) TO estades
STORE 0 TO w_totalf, w_totals,  ;
      w_totalr, ml
STORE SPACE(10) TO estades
CLOSE DATABASES
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_hco00 ORDER codigo
SELECT 4
USE SHARED gc_dco00 ORDER  ;
    dco_codprp
SET RELATION TO dco_nrodoc INTO gc_hco00
SELECT 5
USE SHARED gc_alm00 ORDER codigo
SET RELATION TO 'ALMA' + alm_codalm INTO;
ge_tab0 ADDITIVE
SELECT 6
USE SHARED gc_ord00 ORDER  ;
    ord_nrodoc
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_dco00
w_selec = SELECT()
STORE SELECT() TO wrk_selec,  ;
      wrk_selpro
w_selpro = SELECT()
DEFINE WINDOW ven1 FROM 00, 00 TO  ;
       21, 79 TITLE  ;
       'Consulta de pedido por c¢digo'  ;
       IN screen COLOR SCHEME 12
DEFINE WINDOW ven2 FROM 06, 00 TO  ;
       21, 79 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW ven3 FROM 12, 04 TO  ;
       22, 75 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW ven4 FROM 12, 12 TO  ;
       22, 69 IN screen COLOR  ;
       SCHEME 10
DO p_footer WITH  ;
   '1000100000000001100011101',  ;
   2
@ 00, 01 SAY '[ F1 ]' COLOR N/W 
@ 00, 08 SAY 'Ayuda      '
@ 00, 21 SAY '[ F5 ]' COLOR N/W 
@ 00, 28 SAY 'Almac‚n    '
@ 00, 41 SAY '[ F6 ]' COLOR N/W 
@ 00, 48 SAY 'Buscar     '
@ 00, 61 SAY '[ F8 ]' COLOR N/W 
@ 00, 68 SAY 'Doc. Ref.  '
@ 01, 01 SAY '[ Up ]' COLOR N/W 
@ 01, 08 SAY 'Avanza P g.'
@ 01, 21 SAY '[Home]' COLOR N/W 
@ 01, 28 SAY 'Principio  '
@ 01, 41 SAY '[End ]' COLOR N/W 
@ 01, 48 SAY 'Ultimo    '
codigo = SPACE(14)
w_codigo = SPACE(14)
w_desalm = SUBSTR(ootab('ALMA', ;
           rge_codalm), 1, 11)
ACTIVATE WINDOW ven1
GOTO TOP
DO dato
tmp_x = 0
DO WHILE tmp_x<>27
     tmp_x = INKEY(0, 'H')
     DO CASE
          CASE tmp_x = 13
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               SELECT gc_pro00
               DO dato
          CASE tmp_x = -5
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               w_campo = 'pro_codpro'
               DO produc WITH  ;
                  w_campo,  ;
                  w_selec,  ;
                  w_selpro
               DO dato
          CASE tmp_x = 1
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               SELECT gc_pro00
               GOTO TOP
               DO dato
          CASE tmp_x = 6
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               SELECT gc_pro00
               GOTO BOTTOM
               DO dato
          CASE tmp_x = 18
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               ACTIVATE WINDOW  ;
                        ven1
               SELECT gc_pro00
               IF BOF()
                    GOTO TOP
               ELSE
                    SKIP -1
               ENDIF
               DO dato
          CASE tmp_x = 3
               CLEAR
               DEACTIVATE WINDOW  ;
                          ven2
               ACTIVATE WINDOW  ;
                        ven1
               SELECT gc_pro00
               IF EOF()
                    GOTO BOTTOM
               ELSE
                    SKIP
               ENDIF
               DO dato
          CASE tmp_x = -4
               DO alma
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
if file('&w_idx')
     erase &w_idx
ENDIF
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE dato
campo = SPACE(10)
STORE 0 TO w_totalf, w_totals,  ;
      w_totalr
ACTIVATE WINDOW ven1
SELECT gc_pro00
@ 00, 00 SAY 'C¢digo.......:' +  ;
  pro_codpro COLOR W+/N 
@ 00, 45 SAY  ;
  'Nro. Parte .......:' +  ;
  pro_numpar COLOR W/N 
w_codigo = pro_codpro
SELECT gc_alm00
GOTO TOP
SUM FOR (alm_codpro = w_codigo  ;
    .AND. alm_codalm <>  ;
    rge_codalm) alm_stkfis TO  ;
    w_totalf
GOTO TOP
SUM FOR (alm_codpro = w_codigo  ;
    .AND. alm_codalm =  ;
    rge_codalm) alm_stkfis TO  ;
    w_totals
GOTO TOP
SUM FOR alm_codpro = w_codigo  ;
    alm_stkres TO w_totalr
SELECT gc_pro00
@ 01, 00 SAY 'Descripci¢n..:' +  ;
  SUBSTR(pro_descri, 1, 29) COLOR  ;
  W/N 
@ 01, 45 SAY  ;
  'Cod.de Reemplazo..:' +  ;
  pro_codree COLOR W/N 
@ 02, 00 SAY 'C.Solicitada.:' +  ;
  TRANSFORM(pro_stksol,  ;
  '999,999.99') COLOR W/N 
@ 03, 00 SAY 'C.Tr nsito...:' +  ;
  TRANSFORM(pro_stktra,  ;
  '999,999.99') COLOR W/N 
@ 04, 00 SAY 'C.Backorder..:' +  ;
  TRANSFORM(pro_stkbor,  ;
  '999,999.99') COLOR W/N 
@ 02, 45 SAY 'Stock ' + w_desalm +  ;
  '.:' + TRANSFORM(w_totals,  ;
  '999,999.99') COLOR W/N 
@ 03, 45 SAY  ;
  'Stock Otros.......:' +  ;
  TRANSFORM(w_totalf,  ;
  '999,999.99') COLOR W/N 
@ 04, 45 SAY  ;
  'Stock Reservado...:' +  ;
  TRANSFORM(w_totalr,  ;
  '999,999.99') COLOR W/N 
ACTIVATE WINDOW ven2
DO p_mensaje WITH  ;
   'Espere un Momento...'
SELECT gc_dco00
SET ORDER TO dco_codprp
GOTO TOP
SEEK w_codigo
ml = 0
SCAN WHILE dco_codprp = w_codigo  ;
     .AND.  .NOT. EOF()
     IF dco_codprp = dco_codpro  ;
        .OR. EMPTY(dco_codpro)
          ml = ml + 1
     ENDIF
ENDSCAN
SET ORDER TO dco_codpro
GOTO TOP
SEEK w_codigo
SCAN WHILE dco_codpro = w_codigo  ;
     .AND.  .NOT. EOF()
     IF dco_codprp <> dco_codpro
          ml = ml + 1
     ENDIF
ENDSCAN
IF ml = 0 .OR. EMPTY(w_codigo)
     @ 05, 20 SAY SPACE(35)
     DO p_mensaje WITH  ;
        ' Repuesto no tiene pedido '
     SELECT gc_pro00
     RETURN
ENDIF
lm = 0
CREATE CURSOR detalle (dco_nrodoc  ;
       C (10), dco_cansol N (9,  ;
       2), dco_indest C (1),  ;
       hco_fecdoc D (8),  ;
       hco_feccon D (8),  ;
       hco_feclle D (8),  ;
       dco_numfac C (10),  ;
       dco_inorig C (1),  ;
       dco_docref C (10),  ;
       dco_cancon N (9, 2),  ;
       dco_canbor N (9, 2),  ;
       estado C (10), dco_cancel  ;
       N (9, 2), dco_codpro C  ;
       (14), dco_codprp C (14))
SET RELATION TO 'IMPO' + dco_indest INTO;
ge_tab0 ADDITIVE
SELECT gc_dco00
SET ORDER TO dco_codprp
SEEK w_codigo
SCAN WHILE dco_codprp = w_codigo  ;
     .AND.  .NOT. EOF()
     IF dco_codprp = dco_codpro  ;
        .OR. dco_codpro =  ;
        SPACE(14)
          lm = lm + 1
          SELECT detalle
          APPEND BLANK
          REPLACE dco_nrodoc WITH  ;
                  gc_dco00.dco_nrodoc
          REPLACE dco_cansol WITH  ;
                  gc_dco00.dco_cansol
          REPLACE dco_indest WITH  ;
                  gc_dco00.dco_indest
          REPLACE hco_fecdoc WITH  ;
                  gc_hco00.hco_fecdoc
          REPLACE hco_feccon WITH  ;
                  gc_hco00.hco_feccon
          REPLACE hco_feclle WITH  ;
                  gc_dco00.dco_feclle
          REPLACE dco_numfac WITH  ;
                  gc_dco00.dco_numfac
          REPLACE dco_inorig WITH  ;
                  gc_dco00.dco_inorig
          REPLACE dco_docref WITH  ;
                  gc_dco00.dco_docref
          REPLACE dco_cancon WITH  ;
                  gc_dco00.dco_cancon
          REPLACE dco_canbor WITH  ;
                  gc_dco00.dco_canbor
          REPLACE estado WITH  ;
                  ge_tab0.tab_destab
          REPLACE dco_cancel WITH  ;
                  gc_dco00.dco_cancel
          REPLACE dco_codprp WITH  ;
                  gc_dco00.dco_codprp
          REPLACE dco_codpro WITH  ;
                  gc_dco00.dco_codpro
          SELECT gc_dco00
     ENDIF
ENDSCAN
SET ORDER TO dco_codpro
SEEK w_codigo
SCAN WHILE dco_codpro = w_codigo  ;
     .AND.  .NOT. EOF()
     IF dco_codprp <> dco_codpro
          lm = lm + 1
          SELECT detalle
          APPEND BLANK
          REPLACE dco_nrodoc WITH  ;
                  gc_dco00.dco_nrodoc
          REPLACE dco_cansol WITH  ;
                  gc_dco00.dco_cansol
          REPLACE dco_indest WITH  ;
                  gc_dco00.dco_indest
          REPLACE hco_fecdoc WITH  ;
                  gc_hco00.hco_fecdoc
          REPLACE hco_feccon WITH  ;
                  gc_hco00.hco_feccon
          REPLACE hco_feclle WITH  ;
                  gc_dco00.dco_feclle
          REPLACE dco_numfac WITH  ;
                  gc_dco00.dco_numfac
          REPLACE dco_inorig WITH  ;
                  gc_dco00.dco_inorig
          REPLACE dco_docref WITH  ;
                  gc_dco00.dco_docref
          REPLACE dco_cancon WITH  ;
                  gc_dco00.dco_cancon
          REPLACE dco_canbor WITH  ;
                  gc_dco00.dco_canbor
          REPLACE estado WITH  ;
                  ge_tab0.tab_destab
          REPLACE dco_cancel WITH  ;
                  gc_dco00.dco_cancel
          REPLACE dco_codprp WITH  ;
                  gc_dco00.dco_codprp
          REPLACE dco_codpro WITH  ;
                  gc_dco00.dco_codpro
          SELECT gc_dco00
     ENDIF
ENDSCAN
SELECT detalle
w_idx = f_indice() + '.idx'
index on dtos(hco_fecdoc)+dco_nrodoc to;
&w_idx
GOTO TOP
DO paso2
RETURN
*
PROCEDURE paso2
ON KEY
ON KEY LABEL f8 do docref
ON KEY LABEL f5 do alma
SELECT detalle
BROWSE FIELDS dco_nrodoc :H =  ;
       'Nø Pedido' :W = .F.,  ;
       dco_cansol :P = '99,999'  ;
       :H = 'C.Soli.', dco_cancon  ;
       :P = '99,999' :H =  ;
       'C.Conf.', dco_canbor :P =  ;
       '99,999' :H = 'C.Back.',  ;
       dco_cancel :P = '99,999'  ;
       :H = 'C.Canc.', estado :H =  ;
       'Estado' : 10, hco_fecdoc  ;
       :H = 'Fec.Ped.',  ;
       hco_feccon :H = 'Fec.Con.',  ;
       hco_feclle :H = 'Fec.LLe.',  ;
       dco_numfac :H =  ;
       'Nro.Factura', dco_inorig  ;
       : 1 :H = 'O', dco_docref  ;
       :H = 'Doc. Ref.',  ;
       dco_codprp : 14 :H =  ;
       'C¢digo Origen' NOEDIT IN  ;
       ven2
SELECT gc_pro00
ON KEY LABEL f8
RETURN
*
PROCEDURE alma
ACTIVATE WINDOW ven3
ON KEY
SELECT gc_alm00
SEEK w_codigo
BROWSE FIELDS alm_codalm : 4 :H =  ;
       'CODI', ge_tab0.tab_destab  ;
       : 19 :H = 'ALMACEN',  ;
       alm_ubicac : 10 :H =  ;
       'UBICACION', alm_stkfis :  ;
       9 :P = '9,999,999' :H =  ;
       ' FISICO ', alm_stkres : 9  ;
       :P = '9,999,999' :H =  ;
       'RESERVADO', v =  ;
       alm_stkfis - alm_stkres :  ;
       10 :P = '99,999,999' :H =  ;
       'DISPONIBLE' NOEDIT KEY  ;
       w_codigo IN ven3
DEACTIVATE WINDOW ven3
SELECT gc_pro00
ON KEY LABEL f8 do docref
ON KEY LABEL f5 do alma
RETURN
*
PROCEDURE docref
CREATE CURSOR refe (numfac C (10),  ;
       inorig C (1), desinor C  ;
       (10), docu C (10), estado  ;
       C (10), feclle D)
SELECT gc_ord00
IF EMPTY(detalle.dco_codpro) .OR.  ;
   detalle.dco_codpro =  ;
   detalle.dco_codprp
     SET ORDER TO ord_docprp
     w_val = ' ord_nrodoc=detalle.dco_nrodoc and ord_codprp=w_codigo '
ELSE
     SET ORDER TO ord_nrodoc
     w_val = ' ord_nrodoc=detalle.dco_nrodoc and ord_codpro=w_codigo '
ENDIF
SEEK detalle.dco_nrodoc +  ;
     w_codigo
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        ' No Hay Documento de Referencia '
     SELECT detalle
     RETURN
ELSE
     scan while &w_val and !eof()
          SELECT refe
          APPEND BLANK
          REPLACE numfac WITH  ;
                  gc_ord00.ord_numfac
          REPLACE inorig WITH  ;
                  gc_ord00.ord_inorig
          REPLACE docu WITH  ;
                  gc_ord00.ord_docref
          REPLACE feclle WITH  ;
                  gc_ord00.ord_feclle
          SELECT ge_tab0
          SEEK 'IMPO' +  ;
               gc_ord00.ord_indest
          IF FOUND()
               SELECT refe
               REPLACE estado  ;
                       WITH  ;
                       SUBSTR(ge_tab0.tab_destab,  ;
                       1, 10)
          ENDIF
          SELECT ge_tab0
          SEEK 'OPED' +  ;
               gc_ord00.ord_inorig
          IF FOUND()
               SELECT refe
               REPLACE desinor  ;
                       WITH  ;
                       SUBSTR(ge_tab0.tab_destab,  ;
                       1, 10)
          ENDIF
          SELECT gc_ord00
     ENDSCAN
     ACTIVATE WINDOW ven4
     ON KEY
     SELECT refe
     GOTO TOP
     BROWSE FIELDS numfac :H =  ;
            'Nro.Factura', feclle  ;
            :P = '99/99/99' :H =  ;
            'Fec.LLe.', desinor :  ;
            10 :H = ' Origen ',  ;
            docu :H = 'Doc. Ref.',  ;
            estado :H =  ;
            '  Estado  ' NOEDIT  ;
            IN ven4
     DEACTIVATE WINDOW ven4
     SELECT detalle
     ON KEY LABEL f8 do docref
     ON KEY LABEL f5 do alma
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
