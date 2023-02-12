*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
CLOSE DATABASES
STORE 1 TO gc_tipo
wrk_progra = PROGRAM()
titu1 = ' CONSULTA '
titu2 = ' DOCUMENTOS DE VENTA '
DEFINE WINDOW tablas FROM 00, 00  ;
       TO 24, 79 IN screen  ;
       DOUBLE
DEFINE WINDOW footer FROM 22, 01  ;
       TO 23, 78 IN screen NONE
DEFINE WINDOW cabecera FROM 05,  ;
       02 TO 10, 74 COLOR SCHEME  ;
       7
DEFINE WINDOW totales FROM 16, 02  ;
       TO 17, 74 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW tipos FROM 05, 24  ;
       TO 09, 48 NONE
DEFINE POPUP detalle FROM 11, 02  ;
       TO 15, 74 PROMPT FIELDS  ;
       ' ' + fac_codpro + ' ' +  ;
       SUBSTR(oodespro(fac_codpro),  ;
       1, 25) +  ;
       TRANSFORM(fac_cantid,  ;
       '9,999') + ' ' +  ;
       TRANSFORM(fac_import *  ;
       1.18 , '999,999.99') + ' ' +  ;
       TRANSFORM(fac_total * 1.18 ,  ;
       '999,999.99') TITLE  ;
       'ProductoÄÄÄÄÄÄDescripci¢nÄÄÄÄÄÄÄÄÄÄÄÄÄÄCant.ÄÄÄÄÄP.UNITÄÄÄÄÄTOTAL '  ;
       IN screen COLOR SCHEME 8
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GC_HVE00 ORDER CODIGO
SELECT 3
USE GC_DVE00 ORDER CODIGO
SELECT 4
USE GC_HLP00 ORDER CODIGO
SELECT 5
USE ST_ICLPR ORDER CODIGO
SELECT 6
USE GC_VND00 ORDER CODIGO
SELECT 7
USE GE_TAB0 ORDER CODIGO
SELECT 8
USE GC_CMV00 ORDER codigo_2
SELECT 9
USE GC_CLI00 ORDER CODIGO
DIMENSION tipo[ 2]
tipo[ 1] = 'FACT ³FACTURA'
tipo[ 2] =  ;
    'BOLE ³BOLETA DE VENTA'
DO crea_win
@ 02, 07 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
DO esc_modo WITH 'I'
wrk_tipcam = 0
DO WHILE .T.
     wrk_selec = SELECT()
     wrk_campo = 'WRK_CODIGO'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     ACTIVATE WINDOW cabecera
     SET CURSOR ON
     CLEAR
     STORE SPACE(4) TO wrk_tipdoc
     ON KEY LABEL F6 DO BUSC WITH wrk_tipdoc
     @ 00, 01 SAY 'Documento   :'
     @ 00, 15 GET wrk_tipdoc  ;
       PICTURE '@!' COLOR SCHEME  ;
       8
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
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'INT'
          ACTIVATE SCREEN
          DO saca_win
          RETURN
     ENDIF
     ON KEY
     SELECT 2
     SEEK wrk_tipdoc
     IF  .NOT. FOUND()
          DO error WITH  ;
             '*** Tipo de Documento no Existe ***'
          LOOP
     ENDIF
     SELECT 7
     SEEK 'DOCU' + wrk_tipdoc
     IF FOUND()
          @ 00, 15 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            22)
     ENDIF
     SELECT 2
     DO WHILE .T.
          STORE SPACE(10) TO  ;
                wrk_nrodoc
          ON KEY
          @ 00, 40 SAY  ;
            'N£mero        :'
          @ 00, 56 GET wrk_nrodoc  ;
            PICTURE '@!' COLOR  ;
            SCHEME 8
          READ
          IF LASTKEY() = 27
               EXIT
          ENDIF
          @ 00, 56 SAY  ;
            f_ceros(wrk_nrodoc,10, ;
            2)
          ON KEY
          SEEK wrk_tipdoc +  ;
               f_ceros(wrk_nrodoc, ;
               10,2)
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Nro. de Documento no Existe ***'
               LOOP
          ENDIF
          EXIT
     ENDDO
     IF LASTKEY() = 27
          CLEAR
          LOOP
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     ACTIVATE WINDOW cabecera
     wrk_tipent = hve_tipent
     wrk_codent = hve_codent
     wrk_tipago = hve_tippag
     wrk_fchemi = hve_fecdoc
     wrk_codmon = hve_codmon
     wrk_estdoc = hve_estdoc
     wrk_codemi = hve_codemi
     wrk_numore = SUBSTR(hve_tidore,  ;
                  1, 1) + '/' +  ;
                  ALLTRIM(hve_nrdore)
     IF hve_codmon = 'DOL '
          wrk_cosrep = hve_cosrep
          wrk_cosmob = hve_cosmob
          wrk_fleemb = hve_flete
          wrk_totnet = hve_totnet
          wrk_totdes = hve_totdes
          wrk_toacta = hve_pagctd
          wrk_totgen = hve_totgen
          wrk_mtocan = hve_totgen -  ;
                       hve_pagctd
     ELSE
          wrk_cosrep = hve_solrep
          wrk_cosmob = hve_solmob
          wrk_totnet = hve_solnet
          wrk_totdes = hve_soldes
          wrk_fleemb = hve_solfle
          wrk_totvta = hve_solvta
          wrk_totigv = hve_soligv
          wrk_totgen = hve_solgen
          wrk_toacta = hve_pagcts
          wrk_mtocan = hve_mtocan
     ENDIF
     wrk_tipcam = hve_tipcam
     @ 01, 01 SAY 'Cliente     :'
     @ 01, 40 SAY  ;
       'Doc.Ref.      :'
     @ 02, 01 SAY 'Emisor      :'
     @ 02, 40 SAY  ;
       'Forma de Pago :'
     @ 03, 01 SAY 'F. Emisi¢n  :'
     @ 03, 40 SAY  ;
       'Moneda        :'
     SELECT 5
     SEEK 'C' + wrk_codent
     IF FOUND()
          @ 01, 15 SAY  ;
            SUBSTR(noment, 1,  ;
            18)
     ELSE
          SELECT 9
          SEEK 'C' + wrk_codent
          IF FOUND()
               @ 01, 15 SAY  ;
                 SUBSTR(cli_razsoc,  ;
                 1, 26)
          ENDIF
     ENDIF
     IF wrk_estdoc = 'A'
          SET COLOR TO W+/N*
          @ 00, 45 SAY  ;
            'A  N  U  L  A  D  O'
          SET COLOR TO W+/N
     ENDIF
     SELECT 7
     SEEK 'EMIS' + wrk_codemi
     IF FOUND()
          @ 02, 15 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     ENDIF
     SEEK 'FPAG' + wrk_tipago
     IF FOUND()
          @ 02, 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
     ENDIF
     SEEK 'MONE' + wrk_codmon
     IF FOUND()
          @ 03, 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     ELSE
          @ 03, 56 SAY wrk_codmon
     ENDIF
     @ 01, 56 SAY wrk_numore
     @ 03, 15 SAY wrk_fchemi
     SELECT 8
     SEEK DTOC(wrk_fchemi) + '1' +  ;
          'SOL ' + 'DOL '
     IF FOUND()
          wrk_tipcam = cmv_tipcav
     ENDIF
     DO mensa WITH  ;
        '*** Espere un momento, por favor ***',  ;
        'COLO'
     wrk_file = da_nombre()
     CREA TABLE  &wrk_file (FAC_CODPRO;
C(14),  FAC_CANTID N(9,2), FAC_IMPORT;
N(9,2), FAC_TOTAL  N(9,2), FAC_COPRMB;
N(9,2), FAC_COPRMO N(9,2) )
     SELECT 10
     USE &wrk_file
     SELECT 3
     SEEK wrk_tipdoc +  ;
          f_ceros(wrk_nrodoc,10, ;
          2)
     IF FOUND()
          DO WHILE dve_tipdoc= ;
             wrk_tipdoc .AND.  ;
             dve_nrodoc= ;
             f_ceros(wrk_nrodoc, ;
             10,2)
               wrk_codpro = dve_propar
               wrk_cantid = dve_cantid
               IF wrk_codmon =  ;
                  'DOL '
                    wrk_import = dve_import
                    wrk_total = dve_total
               ELSE
                    wrk_import = dve_impors
                    wrk_total = dve_totals
               ENDIF
               SELECT 10
               APPEND BLANK
               REPLACE fac_codpro  ;
                       WITH  ;
                       wrk_codpro
               REPLACE fac_cantid  ;
                       WITH  ;
                       wrk_cantid
               REPLACE fac_import  ;
                       WITH  ;
                       wrk_import
               REPLACE fac_total  ;
                       WITH  ;
                       wrk_total
               SELECT 3
               SKIP
          ENDDO
     ENDIF
     DO mensa WITH  ;
        '*** Espere un momento, por favor ***',  ;
        'SACA'
     ACTIVATE WINDOW totales
     @ 00, 00 SAY  ;
       'Repuestos    M.Obra      Flete     Total     Dscto.    A cta.   Cancelado'
     @ 01, 00 SAY  ;
       TRANSFORM((wrk_cosrep *  ;
       1.18 ), '99,999.99') + ' ' +  ;
       TRANSFORM((wrk_cosmob *  ;
       1.18 ), '99,999.99') + ' ' +  ;
       TRANSFORM(wrk_fleemb,  ;
       '999,999.99') + ' ' +  ;
       TRANSFORM(wrk_totgen,  ;
       '99,999.99') + ' ' +  ;
       TRANSFORM(wrk_totdes,  ;
       '999,999.99') + ' ' +  ;
       TRANSFORM(wrk_toacta,  ;
       '99,999.99') + ' ' +  ;
       TRANSFORM(wrk_mtocan,  ;
       '999,999.99')
     SELECT 10
     DO WHILE LASTKEY()<>27
          ACTIVATE POPUP detalle
     ENDDO
     SELECT 10
     USE
     ERASE FILE &wrk_file
     DEACTIVATE WINDOW totales
ENDDO
*
PROCEDURE busc
PARAMETER wrk_tipdoc
ON KEY
ACTIVATE WINDOW tipos
STORE SPACE(4) TO item
@ 00, 00 GET item DEFAULT 1 SIZE  ;
  02, 25 FROM tipo
READ
wrk_tipo = item
wrk_tipdoc = SUBSTR(wrk_tipo, 1,  ;
             4)
DEACTIVATE WINDOW tipos
ON KEY LABEL F6 DO BUSC WITH wrk_tipdoc
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
