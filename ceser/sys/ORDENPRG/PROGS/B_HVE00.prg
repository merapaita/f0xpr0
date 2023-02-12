*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR ALL
CLEAR
SET CURSOR ON
SET CENTURY ON
w_facigv = 1.18 
rge_monbas = 'SOL '
SELECT 1
USE gc_hve00 ORDER codigo
SELECT 2
USE gc_cmv00 ORDER cmv_feinmo
SELECT 3
USE GE_TAB0 ORDER CODIGO
SELECT 4
USE ST_ICLPR ORDER CODIGO
SELECT 5
USE GC_CLI00 ORDER CODIGO
@ 02, 02 TO 23, 78 DOUBLE
@ 22, 60 SAY DATE()
w_tipdoc = SPACE(4)
STORE DATE() TO w_fecdoc
DO WHILE .T.
     STORE SPACE(4) TO w_almdes,  ;
           w_almrec, w_tipent,  ;
           w_codmov, w_tidore,  ;
           w_codemi, w_tippag,  ;
           w_lispre, w_estdoc,  ;
           w_indori, w_codcta,  ;
           w_codmon
     STORE SPACE(10) TO w_nrodoc,  ;
           w_nrdore
     STORE SPACE(9) TO w_codent
     STORE SPACE(5) TO w_codvnd
     STORE SPACE(8) TO w_numore
     STORE 0 TO w_flete, w_rptos,  ;
           w_mobra, w_dscto,  ;
           w_acta, w_cliente
     w_codmov = 'EVTA'
     w_estdoc = 'C'
     w_almdes = '0002'
     w_codmon = 'SOL '
     w_tippag = '001 '
     w_indori = 'VENT'
     w_codemi = '001 '
     @ 03, 04 SAY 'Tipo Doc. '
     @ 03, 40 SAY 'Nro. doc. '
     @ 04, 04 SAY 'Fecha     '
     @ 04, 40 SAY 'Almacen   '
     @ 05, 04 SAY 'Cliente   '
     @ 05, 40 SAY 'Movim.    '
     @ 06, 04 SAY 'Tip. Ref  '
     @ 06, 40 SAY 'Nro. Ref. '
     @ 07, 04 SAY 'Moneda    '
     @ 07, 40 SAY 'Emisor    '
     @ 08, 04 SAY 'Vendedor  '
     @ 08, 40 SAY 'Tipo Pago '
     @ 09, 04 SAY 'Est.Docum '
     @ 09, 40 SAY 'Garantia  '
     @ 10, 04 SAY 'O/R       '
     @ 10, 40 SAY 'Flete     '
     @ 11, 04 SAY 'Rptos.    '
     @ 11, 40 SAY 'M. Obra   '
     @ 12, 04 SAY 'Dsctos    '
     @ 12, 40 SAY 'A Cuenta  '
     @ 14, 40 SAY 'Total     '
     @ 03, 15 GET w_tipdoc  ;
       PICTURE '@!' VALID  ;
       despues(1) MESSAGE  ;
       'FACT o BOLE'
     @ 03, 50 GET w_nrodoc  ;
       PICTURE '@!' VALID  ;
       despues(2)
     @ 04, 15 GET w_fecdoc  ;
       PICTURE '@D' VALID  ;
       despues(3)
     @ 04, 50 GET w_almdes  ;
       PICTURE '@!' VALID  ;
       despues(4)
     @ 05, 15 GET w_codent  ;
       PICTURE '@!' VALID  ;
       despues(5)
     @ 05, 50 GET w_codmov  ;
       PICTURE '@!' VALID  ;
       despues(6) MESSAGE  ;
       'EVTA = Cancelaciones o PCTA = Pagos a cuenta'
     @ 06, 15 GET w_tidore  ;
       PICTURE '@!' VALID  ;
       despues(7) MESSAGE  ;
       'SOLI = Solicitud  u  ORDE = Orden de reparacion'
     @ 06, 50 GET w_nrdore  ;
       PICTURE '@!' VALID  ;
       despues(8) MESSAGE  ;
       'Numero de S/S en pago a cuenta'
     @ 07, 15 GET w_codmon  ;
       PICTURE '@!' VALID  ;
       despues(9) MESSAGE  ;
       'SOL = Soles  o  DOL = Dolares'
     @ 07, 50 GET w_codemi  ;
       PICTURE '@!' VALID  ;
       despues(10)
     @ 08, 15 GET w_codvnd  ;
       PICTURE '@!' VALID  ;
       despues(11)
     @ 08, 50 GET w_tippag  ;
       PICTURE '@!' VALID  ;
       despues(12) MESSAGE  ;
       '001 = Contado,  002 = Credito'
     @ 09, 15 GET w_estdoc  ;
       PICTURE '@!' VALID  ;
       despues(13) MESSAGE  ;
       'C = Ventas,  O = Orden de reparacion,  A = Anulada,  V = Pago a cuenta de Rptos.'
     @ 09, 50 GET w_indori  ;
       PICTURE '@!' VALID  ;
       despues(14) MESSAGE  ;
       'FGAR = Fuera de garantia,  GARA = Garantia,  VENT = Repuestos'
     @ 10, 15 GET w_numore  ;
       PICTURE '@!' VALID  ;
       despues(15) MESSAGE  ;
       'Numero de O/R cancelada'
     @ 10, 50 GET w_flete PICTURE  ;
       '999,999.99'
     @ 11, 15 GET w_rptos PICTURE  ;
       '999,999.99'
     @ 11, 50 GET w_mobra PICTURE  ;
       '999,999.99'
     @ 12, 15 GET w_dscto PICTURE  ;
       '999,999.99'
     @ 12, 50 GET w_acta PICTURE  ;
       '999,999.99'
     w_total = (w_flete + w_rptos +  ;
               w_mobra) -  ;
               (w_dscto +  ;
               w_acta)
     @ 14, 50 SAY w_total PICTURE  ;
       '999,999.99'
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DEFINE WINDOW grabar FROM 15,  ;
            25 TO 17, 52
     ACTIVATE WINDOW grabar
     @ 00, 02 GET okcancel  ;
       DEFAULT 1 SIZE 1, 10, 1  ;
       FUNCTION  ;
       '*H \!Graba;\?Cancela'
     READ CYCLE
     IF okcancel = 2
          DEACTIVATE WINDOW  ;
                     grabar
          LOOP
     ELSE
          DEACTIVATE WINDOW  ;
                     grabar
          DO calcula
          WAIT WINDOW NOWAIT  ;
               'Registro grabado'
     ENDIF
ENDDO
CLOSE DATABASES
*
PROCEDURE calcula
SELECT gc_cmv00
SEEK DTOS(w_fecdoc) + '1' +  ;
     'SOL ' + 'DOL '
IF  .NOT. FOUND()
     w_tipcam = 0
ELSE
     w_tipcam = cmv_tipcav
ENDIF
@ 22, 10 SAY w_tipcam
IF w_codmon = 'SOL '
     w_actaso = w_acta
     IF w_codmov = 'PCTA'
          w_solmob = ROUND(w_acta /  ;
                     w_facigv,  ;
                     2)
          w_mobra = w_acta
     ELSE
          w_solmob = ROUND(w_mobra /  ;
                     w_facigv,  ;
                     2)
     ENDIF
     w_solrep = ROUND(w_rptos /  ;
                w_facigv, 2)
     w_solfle = w_flete
     w_soldes = w_dscto
     w_solimp = w_mobra + w_rptos +  ;
                w_flete
     w_solmon = (w_mobra +  ;
                w_rptos +  ;
                w_flete) -  ;
                w_dscto
     IF w_codmov = 'PCTA'
          w_solpag = w_solmon
     ELSE
          w_solpag = w_solmon -  ;
                     w_acta
     ENDIF
     w_solvta = ROUND(w_solpag /  ;
                w_facigv, 2)
     w_soligv = w_solpag -  ;
                w_solvta
     w_dolmob = ROUND((w_solmob /  ;
                w_tipcam), 2)
     w_dolrep = ROUND((w_solrep /  ;
                w_tipcam), 2)
     w_dolfle = ROUND((w_solfle /  ;
                w_tipcam), 2)
     w_doldes = ROUND((w_soldes /  ;
                w_tipcam), 2)
     w_dolimp = ROUND(w_dolmob *  ;
                w_facigv, 2) +  ;
                ROUND(w_dolrep *  ;
                w_facigv, 2) +  ;
                w_dolfle
     w_dolmon = (w_dolimp -  ;
                w_doldes)
     w_actado = ROUND(w_acta /  ;
                w_tipcam, 2)
     IF w_codmov = 'PCTA'
          w_doltot = w_dolmon
     ELSE
          w_doltot = w_dolmon -  ;
                     w_actado
     ENDIF
     w_dolvta = ROUND(w_doltot /  ;
                w_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
ELSE
     IF w_codmov = 'PCTA'
          w_dolmob = ROUND(w_acta /  ;
                     w_facigv,  ;
                     2)
          w_mobra = w_acta
     ELSE
          w_dolmob = ROUND(w_mobra /  ;
                     w_facigv,  ;
                     2)
     ENDIF
     w_dolrep = ROUND(w_rptos /  ;
                w_facigv, 2)
     w_dolfle = w_flete
     w_doldes = w_dscto
     w_dolimp = w_mobra + w_rptos +  ;
                w_dolfle
     w_dolmon = w_dolimp -  ;
                w_doldes
     w_actado = w_acta
     IF w_codmov = 'PCTA'
          w_doltot = w_dolmon
     ELSE
          w_doltot = w_dolmon -  ;
                     w_actado
     ENDIF
     w_dolvta = ROUND(w_doltot /  ;
                w_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
     w_solmob = ROUND(w_dolmob *  ;
                w_tipcam, 2)
     w_solrep = ROUND(w_dolrep *  ;
                w_tipcam, 2)
     w_solfle = ROUND(w_dolfle *  ;
                w_tipcam, 2)
     w_soldes = ROUND(w_dscto *  ;
                w_tipcam, 2)
     w_solimp = ROUND(w_solmob *  ;
                w_facigv, 2) +  ;
                ROUND(w_solrep *  ;
                w_facigv, 2) +  ;
                w_solfle
     w_solmon = w_solimp -  ;
                w_soldes
     w_actaso = ROUND(w_acta *  ;
                w_tipcam, 2)
     IF w_codmov = 'PCTA'
          w_solpag = w_solmon
     ELSE
          w_solpag = w_solmon -  ;
                     w_actaso
     ENDIF
     w_solvta = ROUND(w_solpag /  ;
                w_facigv, 2)
     w_soligv = w_solpag -  ;
                w_solvta
ENDIF
SELECT gc_hve00
APPEND BLANK
REPLACE hve_tipdoc WITH w_tipdoc
REPLACE hve_nrodoc WITH w_nrodoc
REPLACE hve_fecdoc WITH w_fecdoc
REPLACE hve_fecvct WITH w_fecdoc
REPLACE hve_almdes WITH w_almdes
REPLACE hve_tipent WITH 'C'
REPLACE hve_codent WITH w_codent
REPLACE hve_codmov WITH w_codmov
REPLACE hve_tidore WITH w_tidore
REPLACE hve_nrdore WITH w_nrdore
REPLACE hve_codmon WITH w_codmon
REPLACE hve_codemi WITH w_codemi
REPLACE hve_codvnd WITH w_codvnd
REPLACE hve_tippag WITH w_tippag
REPLACE hve_lispre WITH 'PUBL'
REPLACE hve_estdoc WITH w_estdoc
REPLACE hve_indori WITH w_indori
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_fechtc WITH w_fecdoc
REPLACE hve_cosmob WITH w_dolmob
REPLACE hve_cosrep WITH w_dolrep
REPLACE hve_flete WITH w_dolfle
REPLACE hve_totnet WITH w_dolimp
REPLACE hve_totdes WITH w_doldes
REPLACE hve_totgen WITH w_dolmon
REPLACE hve_pagctd WITH w_actado
REPLACE hve_totvta WITH w_dolvta
REPLACE hve_totigv WITH w_doligv
REPLACE hve_totoim WITH w_doltot
REPLACE hve_solmob WITH w_solmob
REPLACE hve_solrep WITH w_solrep
REPLACE hve_solfle WITH w_solfle
REPLACE hve_solnet WITH w_solimp
REPLACE hve_soldes WITH w_soldes
REPLACE hve_solvta WITH w_solvta
REPLACE hve_soligv WITH w_soligv
REPLACE hve_solgen WITH w_solmon
REPLACE hve_pagcts WITH w_actaso
REPLACE hve_mtocan WITH w_solpag
REPLACE hve_usuari WITH 'SYS'
REPLACE hve_hora WITH TIME()
REPLACE hve_fecha WITH DATE()
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_tipdoc)
               WAIT WINDOW  ;
                    'No se aceptan blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'DOCU' + w_tipdoc
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'Documento de Venta No Existe'
               RETURN .F.
          ENDIF
     CASE opc = 2
          IF EMPTY(w_nrodoc)
               RETURN .F.
          ENDIF
          SELECT gc_hve00
          SEEK w_tipdoc +  ;
               w_nrodoc
          IF FOUND()
               WAIT WINDOW  ;
                    'Numero ya existe'
               RETURN .F.
          ENDIF
     CASE opc = 3
          IF EMPTY(w_fecdoc)
               WAIT WINDOW  ;
                    'No se aceptan blancos'
               RETURN .F.
          ENDIF
          SELECT gc_cmv00
          SEEK DTOS(w_fecdoc) +  ;
               '1' + 'SOL ' +  ;
               'DOL '
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'No Existe Tipo de Cambio de esta Fecha'
               RETURN .F.
          ENDIF
          w_tipcam = cmv_tipcav
     CASE opc = 4
          SELECT ge_tab0
          SEEK 'ALMA' + w_almdes
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'Almacen No Existe'
               RETURN .F.
          ENDIF
     CASE opc = 5
          IF EMPTY(w_codent)
               WAIT WINDOW  ;
                    'No se aceptan Blancos'
               RETURN .F.
          ENDIF
          SELECT gc_cli00
          SEEK 'C' + w_codent
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'No existe cliente'
               RETURN .F.
          ENDIF
     CASE opc = 6
     CASE opc = 7
     CASE opc = 8
     CASE opc = 9
          IF EMPTY(w_codmon)
               WAIT WINDOW  ;
                    'No se aceptan Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'MONE' + w_codmon
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'C¢digo de Moneda No Existe'
               RETURN .F.
          ENDIF
     CASE opc = 10
          IF EMPTY(w_codemi)
               WAIT WINDOW  ;
                    'No se aceptan Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_codemi
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'C¢digo de Emisor No Existe'
               RETURN .F.
          ENDIF
     CASE opc = 11
     CASE opc = 12
          IF EMPTY(w_tippag)
               WAIT WINDOW  ;
                    'No se aceptan Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'FPAG' + w_tippag
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'C¢digo de Pago No Existe'
               RETURN .F.
          ENDIF
     CASE opc = 13
          IF EMPTY(w_estdoc)
               WAIT WINDOW  ;
                    'No se aceptan Blancos'
               RETURN .F.
          ENDIF
     CASE opc = 14
          IF EMPTY(w_indori)
               WAIT WINDOW  ;
                    'No se aceptan Blancos'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_indori
          IF  .NOT. FOUND()
               WAIT WINDOW  ;
                    'Tipo de garantia no Existe'
               RETURN .F.
          ENDIF
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***
