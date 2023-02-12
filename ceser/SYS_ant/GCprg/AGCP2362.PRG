*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CENTURY ON
ON KEY
SET CURSOR ON
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW clientes FROM 12,  ;
       02 TO 20, 76 NONE
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW totales FROM 20, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW tipos FROM 05, 24  ;
       TO 09, 48 NONE
CLOSE DATABASES
SELECT 1
USE gc_hve00 ORDER codigo
SELECT 2
USE gc_cli00 ORDER codigo
SELECT 3
USE ST_ICLPR ORDER CODIGO
SELECT 4
USE ge_tab0 ORDER codigo
SELECT 5
USE gc_dve00 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESOS',  ;
   'ANULACION DE N/CREDITO',  ;
   'PROCESOS'
@ 2, 63 SAY PROGRAM()
w_simb = sys_simomb
STORE 0 TO w_facigv
= facigv()
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'Tipo de Doc.: NOTA DE CREDITO'
     @ 00, 40 SAY 'N즡ero   :'
     @ 01, 01 SAY 'Cliente     :'
     @ 02, 01 SAY 'Fecha Docum.:'
     @ 02, 40 SAY 'Moneda   :'
     @ 03, 01 SAY 'Doc. de Ref.:'
     @ 03, 40 SAY 'Nro. Doc.:'
     @ 04, 01 SAY 'S/Servicio  :'
     @ 04, 40 SAY 'Monto    :'
     STORE SPACE(10) TO w_nrodoc
     w_tipdoc = 'NCRE'
     w_busca = w_tipdoc
     w_var = 'w_nrodoc'
     ON KEY LABEL f6 do ayuda12 with w_busca,w_var
     @ 00, 52 GET w_nrodoc  ;
       PICTURE '@!' VALID  ;
       numero() COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  totales
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     SELECT gc_hve00
     SEEK w_tipdoc + w_nrodoc
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'Nro. de Documento no Existe'
          LOOP
     ELSE
          IF hve_tipdoc <> 'NCRE'
               DO p_mensaje WITH  ;
                  'Documento no es N/Cr괺ito'
               LOOP
          ENDIF
     ENDIF
     DO p_footer WITH  ;
        '100000000000001000001',  ;
        2
     ACTIVATE WINDOW cabecera
     w_tipent = hve_tipent
     w_codent = hve_codent
     w_tipago = hve_tippag
     w_codmon = hve_codmon
     w_fchemi = hve_fecdoc
     w_estdoc = hve_estdoc
     w_numore = SUBSTR(hve_tidore,  ;
                1, 1) +  ;
                ALLTRIM(hve_nrdore)
     w_tipdoc = hve_tidore
     w_nroref = hve_nrdore
     w_numsol = hve_numore
     IF w_codmon = 'DOL'
          w_cosrep = hve_cosrep
          w_cosmob = hve_cosmob
          w_cosfle = hve_flete +  ;
                     hve_embala
          w_totnet = hve_totnet
          w_totdes = hve_totdes
          w_totgen = hve_totgen
          w_toacta = hve_pagctd
          w_mtocan = hve_totoim
          w_tipcam = hve_tipcam
     ELSE
          w_cosrep = hve_solrep
          w_cosmob = hve_solmob
          w_cosfle = hve_solfle +  ;
                     ROUND(hve_embala *  ;
                     hve_tipcam,  ;
                     2)
          w_totnet = hve_solnet
          w_totdes = hve_soldes
          w_totgen = hve_solgen
          w_toacta = hve_pagcts
          w_mtocan = hve_mtocan
          w_tipcam = hve_tipcam
     ENDIF
     IF w_estdoc = 'A'
          @ 01, 45 SAY  ;
            'A  N  U  L  A  D  O'  ;
            COLOR W+/N* 
     ENDIF
     SELECT gc_cli00
     SEEK 'C' + w_codent
     IF FOUND()
          @ 01, 15 SAY w_codent
          @ 01, 27 SAY cli_razsoc
     ELSE
          SELECT st_iclpr
          SEEK 'C' + w_codent
          IF FOUND()
               @ 01, 15 SAY  ;
                 w_codent
               @ 01, 27 SAY  ;
                 noment
          ELSE
               @ 01, 15 SAY ' '
               @ 01, 27 SAY ' '
          ENDIF
     ENDIF
     @ 02, 15 SAY w_fchemi
     SEEK 'MONE' + w_codmon
     IF FOUND()
          @ 02, 52 SAY w_codmon
          @ 02, 57 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
     ENDIF
     @ 03, 15 SAY w_tipdoc
     @ 03, 52 SAY w_nroref
     @ 04, 15 SAY w_numsol
     ACTIVATE WINDOW totales
     @ 00, 00 SAY  ;
       '  Repuestos    M.Obra    Flete     SubTotal    Dscto      A cta     T. Pagar'
     SELECT 14
     CREATE CURSOR fact  ;
            (fac_codpro C (14),  ;
            fac_despro C (40),  ;
            fac_cantid N (9, 0),  ;
            fac_import N (12, 2),  ;
            fac_total N (13, 2))
     w_numero = w_nrodoc
     SELECT gc_dve00
     SEEK w_tipdoc + w_numero
     IF FOUND()
          w_cosrep = 0
          SCAN WHILE dve_tipdoc =  ;
               w_tipdoc .AND.  ;
               dve_nrodoc =  ;
               w_numero .AND.   ;
               .NOT. EOF()
               SELECT fact
               APPEND BLANK
               REPLACE fac_codpro  ;
                       WITH  ;
                       gc_dve00.dve_propar
               REPLACE fac_cantid  ;
                       WITH  ;
                       gc_dve00.dve_cantid
               IF w_codmon =  ;
                  'DOL '
                    REPLACE fac_import  ;
                            WITH  ;
                            ROUND(gc_dve00.dve_import *  ;
                            w_facigv,  ;
                            2)
               ELSE
                    REPLACE fac_import  ;
                            WITH  ;
                            ROUND(ROUND(gc_dve00.dve_import *  ;
                            w_facigv,  ;
                            2) *  ;
                            w_tipcam,  ;
                            2)
               ENDIF
               REPLACE fac_total  ;
                       WITH  ;
                       ROUND(fac_import *  ;
                       fac_cantid,  ;
                       2)
               w_cosrep = w_cosrep +  ;
                          fac_total
               SELECT gc_dve00
          ENDSCAN
     ENDIF
     @ 01, 00 SAY  ;
       TRANSFORM(w_cosrep,  ;
       '9999,999.99') + ' ' +  ;
       TRANSFORM(ROUND(w_cosmob *  ;
       w_facigv, 2), '99,999.99') +  ;
       ' ' + TRANSFORM(w_cosfle,  ;
       '9,999.99') + ' ' +  ;
       TRANSFORM(w_totnet,  ;
       '9,999,999.99') + ' ' +  ;
       TRANSFORM(w_totdes,  ;
       '9,999.99') + ' ' +  ;
       TRANSFORM(w_toacta,  ;
       '999,999.99') + ' ' +  ;
       TRANSFORM(w_mtocan,  ;
       '9,999,999.99')
     IF w_cosrep = 0
          DEFINE POPUP detalle  ;
                 FROM 11, 02 TO  ;
                 18, 77 PROMPT  ;
                 FIELDS '   ' +  ;
                 fac_codpro +  ;
                 '   ' +  ;
                 SUBSTR(fac_despro,  ;
                 1, 30) + '  ' +  ;
                 TRANSFORM(fac_cantid,  ;
                 '999,999') TITLE  ;
                 '컴컴Producto컴컴컴컴컴컴Descripci줻컴컴컴컴컴컴컴컴Cantidad컴컴컴컴'  ;
                 IN screen COLOR  ;
                 SCHEME 8
     ELSE
          DEFINE POPUP detalle  ;
                 FROM 11, 02 TO  ;
                 18, 77 PROMPT  ;
                 FIELDS ' ' +  ;
                 fac_codpro + ' ' +  ;
                 SUBSTR(fac_despro,  ;
                 1, 25) +  ;
                 TRANSFORM(fac_cantid,  ;
                 '999,999') + ' ' +  ;
                 TRANSFORM(fac_import,  ;
                 '9999,999.99') +  ;
                 ' ' +  ;
                 TRANSFORM(fac_total,  ;
                 '99,999,999.99')  ;
                 TITLE  ;
                 'Producto컴컴컴Descripci줻컴컴컴컴컴컴컴Cant.컴컴컴Precio컴컴컴Total'  ;
                 IN screen COLOR  ;
                 SCHEME 8
     ENDIF
     DO p_footer WITH  ;
        '100100000000000000001',  ;
        2
     ON KEY LABEL f4 do p_borra
     w_sigue = .T.
     DO WHILE w_sigue
          SELECT fact
          ACTIVATE POPUP detalle
          IF LASTKEY() = 27
               w_sigue = .F.
          ENDIF
     ENDDO
     DEACTIVATE WINDOW totales
     DEACTIVATE POPUP detalle
     ON KEY LABEL f4
ENDDO
*
PROCEDURE busc
PARAMETER w_tipdoc
ON KEY
ACTIVATE WINDOW tipos
STORE SPACE(4) TO item
item = 'FACT'
@ 00, 00 GET item DEFAULT tipo(1)  ;
  SIZE 02, 25 FROM tipo
READ
w_tipo = item
w_tipdoc = SUBSTR(w_tipo, 1, 4)
DEACTIVATE WINDOW tipos
ON KEY LABEL f6 do busc with w_tipdoc
KEYBOARD CHR(13)
RETURN
*
PROCEDURE p_borra
IF gc_hve00.hve_estdoc = 'A'
     DO p_mensaje WITH  ;
        'Documento ya fue Anulado'
ELSE
     HIDE WINDOW footer
     rpt = f_yesno( ;
           '  Desea Anular?  ')
     IF  .NOT. rpt
          ON KEY
          ON KEY LABEL f4 do p_borra 
          RETURN
     ENDIF
     IF LASTKEY() = 27
          ON KEY LABEL f4 do p_borra 
          RETURN
     ENDIF
     = ooaviso( ;
       ' A N U L A N D O . . .')
     w_linea = 0
     w_sigue = .F.
     SELECT gc_hve00
     DO rbloquea
     REPLACE hve_estdoc WITH 'A',  ;
             hve_usuari WITH  ;
             clave
     REPLACE hve_fecha WITH  ;
             DATE(), hve_hora  ;
             WITH TIME()
     UNLOCK
     SELECT gc_hve00
     SEEK w_tipdoc + w_nroref
     DO rbloquea
     REPLACE hve_ncredi WITH  ;
             SPACE(10)
     REPLACE hve_fecncr WITH {}
     UNLOCK
ENDIF
SELECT fact
DEACTIVATE WINDOW winmensaje
DEACTIVATE POPUP detalle
ACTIVATE WINDOW cabecera
RETURN
*
PROCEDURE numero
w_nrodoc = f_ceros(w_nrodoc,10,2)
@ 00, 52 GET w_nrodoc
CLEAR GETS
RETURN
*
PROCEDURE ayuda12
PARAMETER w_busca, w_var
ON KEY
w_selec = SELECT()
w_order = ORDER()
DO agcpdoc WITH w_busca, w_var
SELECT (w_selec)
SET ORDER TO w_order
ON KEY LABEL f6 do ayuda12 with w_busca,w_var
RETURN
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'W_CODIGO'
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .F.
          ENDIF
          IF EMPTY(w_codigo)
               DO p_mensaje WITH  ;
                  '  Ingrese Dato...'
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE imp_anula
SET PRINTER ON
SET DEVICE TO PRINTER
set print to &rge_lptped
@ PROW(), PCOL() SAY CHR(18)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
linea = 13
w_it = 0
@ 01, 31 SAY 'ANULACION DE VENTA'
@ 01, 65 SAY gc_hve00.hve_tipdoc +  ;
  ' ' + gc_hve00.hve_nrodoc
@ 02, 31 SAY '=================='
@ 04, 60 SAY DATE()
@ 04, 72 SAY TIME()
SELECT gc_cli00
SEEK 'C' + gc_hve00.hve_codent
IF  .NOT. FOUND()
     @ 05, 03 SAY ' '
ELSE
     @ 05, 03 SAY cli_razsoc
     @ 05, 42 SAY  ;
       gc_hve00.hve_codent
ENDIF
@ 07, 05 SAY 'Almac굈 : ' +  ;
  rge_codalm
@ 11, 00 SAY  ;
  '-----------------------------------------------------------------------------'
@ 12, 00 SAY  ;
  'IT NRO. DE PARTE    DESCRIPCION                      UBICACION          CANT.'
@ 13, 00 SAY  ;
  '-----------------------------------------------------------------------------'
SELECT gc_dve00
SEEK w_tipdoc + w_nrodoc
SCAN WHILE (dve_tipdoc = w_tipdoc  ;
     .AND. dve_nrodoc = w_nrodoc)  ;
     .AND.  .NOT. EOF()
     linea = linea + 1
     w_it = w_it + 1
     @ linea, 00 SAY w_it PICTURE  ;
       '99'
     @ linea, 03 SAY dve_propar
     SELECT gc_pro00
     SEEK gc_dve00.dve_propar
     @ linea, 20 SAY  ;
       SUBSTR(pro_descri, 1, 26)
     @ linea, 55 SAY  ;
       ooubica(gc_dve00.dve_propar, ;
       rge_codalm)
     @ linea, 68 SAY  ;
       gc_dve00.dve_cantid  ;
       PICTURE '999,999'
     SELECT gc_dve00
ENDSCAN
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
SET PRINTER OFF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
