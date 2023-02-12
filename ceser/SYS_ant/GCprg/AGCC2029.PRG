*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 12, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW totales FROM 20, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 7
DEFINE WINDOW tipos FROM 06, 24  ;
       TO 09, 48 NONE
DEFINE POPUP deta01 FROM 13, 02  ;
       TO 19, 77 PROMPT FIELDS  ;
       SPACE(4) + codigo +  ;
       SPACE(4) + descrip +  ;
       SPACE(4) +  ;
       TRANSFORM(precio,  ;
       '999,999.99') TITLE  ;
       'C줰igo컴컴컴컴컴컴Descripci줻컴컴컴컴컴컴컴컴컴컴컴컴컴컴Monto컴�'  ;
       IN screen COLOR SCHEME 8
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE GC_HVE00 ORDER CODIGO
SELECT 3
USE GC_DVE00 ORDER CODIGO
SELECT 4
USE GC_CLI00 ORDER CODIGO
SELECT 5
USE GC_VND00 ORDER CODIGO
SELECT 6
USE ST_ICLPR ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'CONSULTAS',  ;
   'NOTA DE CREDITO', 'CONSULTA'
@ 2, 63 SAY PROGRAM()
DO WHILE .T.
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE SPACE(10) TO  ;
           wrk_nrodoc
     ON KEY
     wrk_busca = 'NCRE'
     wrk_var = 'wrk_nrodoc'
     ON KEY LABEL F6 DO ooVerDocum WITH;
wrk_busca,wrk_var
     @ 00, 01 SAY  ;
       'Tipo de Documento : NOTA DE CREDITO'
     @ 00, 40 SAY  ;
       'Nro. de Documento :' GET  ;
       wrk_nrodoc PICTURE '@!'  ;
       VALID numero()
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
          DO p_tempos
          RETURN
     ENDIF
     ON KEY
     SELECT gc_hve00
     SEEK 'NCRE' + wrk_nrodoc
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'Nro. de Documento no Existe'
          LOOP
     ENDIF
     IF LASTKEY() = 27
          CLEAR
          LOOP
     ENDIF
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     wrk_codmov = hve_codmov
     wrk_fchemi = hve_fecdoc
     wrk_fchvct = hve_fecvct
     w_tipent = 'C'
     w_codent = hve_codent
     wrk_codref = hve_tidore
     wrk_nroref = hve_nrdore
     wrk_vended = hve_codvnd
     wrk_tipago = hve_tippag
     wrk_estdoc = hve_estdoc
     w_moneda = hve_codmon
     IF w_moneda = 'DOL'
          w_vvneto = hve_totvta
          w_igv = hve_totigv
          w_totgen = hve_totgen
     ELSE
          w_vvneto = hve_solvta
          w_igv = hve_soligv
          w_totgen = hve_mtocan
     ENDIF
     @ 01, 01 SAY  ;
       'Fecha Documento   :'
     @ 01, 40 SAY  ;
       'Fecha Vcmto       :'
     @ 02, 01 SAY  ;
       'Cliente           :'
     @ 03, 00 TO 03, 73
     @ 04, 01 SAY  ;
       'Doc. Referencia   :'
     @ 04, 40 SAY  ;
       'Nro. Doc. Refer.  :'
     @ 05, 01 SAY  ;
       'Fecha doc. Refer. :'
     @ 05, 40 SAY  ;
       'Moneda            :'
     @ 06, 01 SAY  ;
       'Forma de Pago     :'
     @ 06, 40 SAY  ;
       'Monto             :'
     @ 01, 21 SAY wrk_fchemi
     @ 01, 60 SAY wrk_fchvct
     @ 04, 21 SAY wrk_codref
     @ 04, 60 SAY wrk_nroref
     IF hve_estdoc = 'A'
          @ 01, 40 SAY  ;
            '***** A  N  U  L  A  D  O *****'  ;
            COLOR N+/W 
     ENDIF
     @ 02, 21 SAY w_codent
     SELECT st_iclpr
     SEEK w_tipent + w_codent
     IF FOUND()
          @ 02, 33 SAY  ;
            SUBSTR(noment, 1,  ;
            40)
     ELSE
          SELECT gc_cli00
          SEEK w_tipent +  ;
               w_codent
          IF FOUND()
               @ 02, 33 SAY  ;
                 SUBSTR(cli_razsoc,  ;
                 1, 40)
          ELSE
               @ 02, 33 SAY ' '
          ENDIF
     ENDIF
     SELECT gc_hve00
     SEEK wrk_codref + wrk_nroref
     IF FOUND()
          @ 05, 21 SAY hve_fecdoc
          @ 05, 60 SAY hve_codmon
          @ 06, 60 SAY hve_mtocan  ;
            PICTURE '999,999.99'
     ELSE
          @ 05, 21 SAY ' '
          @ 05, 60 SAY ' '
          @ 06, 60 SAY 0
     ENDIF
     SELECT ge_tab0
     SEEK 'FPAG' + wrk_tipago
     IF FOUND()
          @ 06, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
     ELSE
          @ 06, 21 SAY ' '
     ENDIF
     ACTIVATE WINDOW totales
     @ 00, 10 SAY  ;
       'Total Neto               Total IGV              Total Gral.'
     @ 01, 10 SAY  ;
       TRANSFORM(w_vvneto,  ;
       '999,999.99') + SPACE(14) +  ;
       TRANSFORM(w_igv,  ;
       '999,999.99') + SPACE(14) +  ;
       TRANSFORM(w_totgen,  ;
       '999,999.99')
     CREATE CURSOR NCREDITO  ;
            (codigo C (14),  ;
            descrip C (30),  ;
            precio N (10, 2))
     SELECT gc_dve00
     SEEK 'NCRE' + wrk_nrodoc
     IF FOUND()
          w_codigo = dve_propar
          w_total = dve_total
     ELSE
          w_codigo = SPACE(14)
          w_total = 0
     ENDIF
     SELECT ge_tab0
     SEEK 'NCRE' +  ;
          SUBSTR(w_codigo, 1, 4)
     IF FOUND()
          w_descri = tab_destab
     ELSE
          w_descri = SPACE(30)
     ENDIF
     SELECT ncredito
     APPEND BLANK
     REPLACE codigo WITH w_codigo,  ;
             precio WITH w_total
     REPLACE descrip WITH  ;
             w_descri
     DO WHILE LASTKEY()<>27
          ACTIVATE POPUP deta01
     ENDDO
     DEACTIVATE WINDOW totales
ENDDO
*
PROCEDURE numero
wrk_nrodoc = f_ceros(wrk_nrodoc, ;
             10,2)
@ 00, 60 GET wrk_nrodoc
CLEAR GETS
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
