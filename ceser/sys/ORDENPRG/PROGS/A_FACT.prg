*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR ALL
CLEAR
SET CURSOR ON
SELECT 1
USE VENTAS
@ 02, 02 TO 23, 78 DOUBLE
w_facigv = 1.18 
rge_monbas = 'SOL '
SELECT 2
USE gc_cmv00 ORDER cmv_feinmo
DO WHILE .T.
     STORE SPACE(4) TO w_tipdoc,  ;
           w_almdes, w_almrec,  ;
           w_tipent, w_codmov,  ;
           w_tidore, w_codemi,  ;
           w_tippag, w_lispre,  ;
           w_estdoc, w_indori,  ;
           w_codcta, w_codmon
     STORE SPACE(10) TO w_nrodoc,  ;
           w_nrdore
     STORE SPACE(9) TO w_codent
     STORE SPACE(5) TO w_codvnd
     STORE SPACE(8) TO w_numore
     STORE 0 TO w_flete, w_rptos,  ;
           w_mobra, w_dscto,  ;
           w_acta
     STORE ('  /  /  ') TO  ;
           w_fecdoc
     @ 03, 04 SAY 'Tipo Doc. '
     @ 03, 40 SAY 'Nro. doc. '
     @ 04, 04 SAY 'Fecha :   '
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
       PICTURE '@!'
     @ 03, 50 GET w_nrodoc  ;
       PICTURE '@!'
     @ 04, 15 GET w_fecdoc  ;
       PICTURE '@D'
     @ 04, 50 GET w_almdes  ;
       PICTURE '@!'
     @ 05, 15 GET w_codent  ;
       PICTURE '@999999999'
     @ 05, 50 GET w_codmov  ;
       PICTURE '@!'
     @ 06, 15 GET w_tidore  ;
       PICTURE '@!'
     @ 06, 50 GET w_nrdore  ;
       PICTURE '@!'
     @ 07, 15 GET w_codmon  ;
       PICTURE '@!'
     @ 07, 50 GET w_codemi  ;
       PICTURE '@!'
     @ 08, 15 GET w_codvnd  ;
       PICTURE '@!'
     @ 08, 50 GET w_tippag  ;
       PICTURE '@!'
     @ 09, 15 GET w_estdoc  ;
       PICTURE '@!'
     @ 09, 50 GET w_indori  ;
       PICTURE '@!'
     @ 10, 15 GET w_numore  ;
       PICTURE '@!'
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
               'Grabando'
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
IF w_codmon = 'SOL '
     w_actaso = w_acta
     w_solmob = ROUND(w_mobra /  ;
                w_facigv, 2)
     w_solrep = ROUND(w_rptos /  ;
                w_facigv, 2)
     w_solfle = w_flete
     w_soldes = w_dscto
     w_solimp = w_mobra + w_rptos
     w_solmon = (w_mobra +  ;
                w_rptos) -  ;
                w_dscto
     w_solpag = w_solmon - w_acta
     w_solvta = ROUND(w_solpag /  ;
                w_facigv, 2)
     w_soligv = w_solpag -  ;
                w_solvta
     w_dolmob = ROUND((w_solmob /  ;
                w_tipcam), 2)
     w_dolmib = ROUND((w_solrep /  ;
                w_tipcam), 2)
     w_dolfle = ROUND((w_solfle /  ;
                w_tipcam), 2)
     w_doldes = ROUND((w_soldes /  ;
                w_tipcam), 2)
     w_dolimp = ROUND(w_dolmob *  ;
                w_facigv, 2) +  ;
                ROUND(w_dolmib *  ;
                w_facigv, 2) +  ;
                w_dolfle
     w_dolmon = (w_dolimp -  ;
                w_doldes)
     w_actado = ROUND(w_acta /  ;
                w_tipcam, 2)
     w_doltot = w_dolmon -  ;
                w_actado
     w_dolvta = ROUND(w_doltot /  ;
                w_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
ELSE
     w_dolmob = ROUND(w_mobra /  ;
                w_facigv, 2)
     w_dolmib = ROUND(w_rptos /  ;
                w_facigv, 2)
     w_dolfle = w_flete
     w_doldes = w_dscto
     w_dolimp = w_mobra + w_rptos +  ;
                w_dolfle
     w_dolmon = w_dolimp -  ;
                w_doldes
     w_actado = w_acta
     w_doltot = w_dolmon -  ;
                w_actado
     w_dolvta = ROUND(w_doltot /  ;
                w_facigv, 2)
     w_doligv = w_doltot -  ;
                w_dolvta
     w_solmob = ROUND(w_dolmob *  ;
                w_tipcam, 2)
     w_solrep = ROUND(w_dolmib *  ;
                w_tipcam, 2)
     w_solfle = ROUND(w_dolfle *  ;
                w_tipcam, 2)
     w_soldes = ROUND(w_mondes *  ;
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
     w_solpag = w_solmon -  ;
                w_actaso
     w_solvta = ROUND(w_solpag /  ;
                w_facigv, 2)
     w_soligv = w_solpag -  ;
                w_solvta
ENDIF
SELECT ventas
APPEND BLANK
REPLACE hve_tipdoc WITH w_tipdoc
REPLACE hve_nrodoc WITH w_nrodoc
REPLACE hve_fecdoc WITH w_fecdoc
REPLACE hve_fecvct WITH w_fecdoc
REPLACE hve_almdes WITH w_almdes
REPLACE hve_tipent WITH 'C'
REPLACE hve_codent WITH w_codent
REPLACE hve_codmov WITH w_codmov
REPLACE hve_tidore WITH 'ORDE'
REPLACE hve_nrdore WITH w_nrdore
REPLACE hve_codmon WITH w_codmon
REPLACE hve_codemi WITH w_codemi
REPLACE hve_codvnd WITH w_codvnd
REPLACE hve_tippag WITH w_tipag
REPLACE hve_lispre WITH 'PUBL'
REPLACE hve_estdoc WITH w_estdoc
REPLACE hve_indori WITH w_tipgar
REPLACE hve_tipcam WITH w_tipcam
REPLACE hve_fechtc WITH w_fecdoc
REPLACE hve_cosmob WITH w_dolmob
REPLACE hve_cosrep WITH w_dolmib
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
*** 
*** ReFox - retrace your steps ... 
***
