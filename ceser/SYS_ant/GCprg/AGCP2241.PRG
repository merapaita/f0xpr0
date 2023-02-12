*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opcion
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
USE SHARED gc_pro00 ORDER codigo
wrk_selpro = SELECT()
SELECT 2
USE SHARED gc_hve00 ORDER codigo
SELECT 3
USE SHARED gc_dve00 ORDER codigo
SELECT 5
USE SHARED gc_cli00 ORDER codigo
SELECT 6
USE SHARED ge_tab0 ORDER codigo
ACTIVATE WINDOW tablas
IF opcion = 1
     DO p_prestab WITH 'PROCESOS',  ;
        'ANULACION DE DOC. DE VENTA',  ;
        'PROCESOS'
ELSE
     DO p_prestab WITH 'CONSULTA',  ;
        'CONSULTAS DE DOC. DE VENTA',  ;
        'CONSULTA'
ENDIF
@ 02, 63 SAY PROGRAM()
w_simb = sys_simomb
DIMENSION tipo[ 2]
tipo[ 1] = 'FACT 쿑ACTURA'
tipo[ 2] =  ;
    'BOLE 쿍OLETA DE VENTA'
ON KEY LABEL F4 do p_borra 
STORE 0 TO w_facigv
= facigv()
DO WHILE .T.
     w_selec = SELECT()
     w_campo = 'w_codigo'
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     STORE SPACE(4) TO w_tipdoc,  ;
           w_codmon
     w_sigue = .T.
     ON KEY LABEL F6 do busc with w_tipdoc
     @ 00, 01 SAY  ;
       'Tipo de Documento :'
     @ 00, 47 SAY 'N즡ero :'
     @ 00, 21 GET w_tipdoc  ;
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
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     ON KEY
     SELECT gc_hve00
     SEEK w_tipdoc
     IF  .NOT. FOUND()
          DO p_mensaje WITH  ;
             'Tipo de Documento no Existe'
          LOOP
     ENDIF
     SELECT ge_tab0
     SEEK 'DOCU' + w_tipdoc
     IF FOUND()
          @ 00, 27 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ELSE
          @ 00, 27 SAY SPACE(20)
     ENDIF
     SELECT gc_hve00
     DO WHILE .T.
          STORE SPACE(10) TO  ;
                w_nrodoc
          w_busca = w_tipdoc
          w_var = 'w_nrodoc'
          ON KEY LABEL f6 do ayuda12 with;
w_busca,w_var
          @ 00, 56 GET w_nrodoc  ;
            PICTURE '@!' VALID  ;
            numero() COLOR SCHEME  ;
            8
          READ
          IF LASTKEY() = 27
               EXIT
          ENDIF
          ON KEY
          SEEK w_tipdoc +  ;
               w_nrodoc
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'Nro. de Documento no Existe'
               LOOP
          ELSE
               IF hve_estdoc =  ;
                  'O' .OR.  ;
                  hve_indori <>  ;
                  'VENT' .OR.  ;
                  hve_tipdoc <>  ;
                  'NCRE'
                    DO p_mensaje  ;
                       WITH  ;
                       'Documento no es de Ventas de Repuestos'
                    LOOP
               ENDIF
               IF gc_hve00.hve_fecdoc <>  ;
                  DATE() .AND.  ;
                  xnnn <> 'A7'  ;
                  .AND. opcion =  ;
                  1
                    DO p_mensaje  ;
                       WITH  ;
                       'Documento no se puede anular en otro d죂'
                    LOOP
               ENDIF
          ENDIF
          EXIT
     ENDDO
     IF LASTKEY() = 27
          CLEAR
          LOOP
     ENDIF
     DO p_footer WITH  ;
        '100000000000001000001',  ;
        2
     ACTIVATE WINDOW cabecera
     w_tipent = hve_tipent
     w_codent = hve_codent
     w_tipago = hve_tippag
     w_codalm = hve_almdes
     w_codmon = ALLTRIM(hve_codmon)
     w_fchemi = hve_fecdoc
     w_estdoc = hve_estdoc
     w_codemi = hve_codemi
     w_numore = SUBSTR(hve_tidore,  ;
                1, 1) +  ;
                ALLTRIM(hve_nrdore)
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
     @ 01, 01 SAY  ;
       'Fecha de Emisi줻  :'
     @ 02, 01 SAY  ;
       'Cliente           :'
     @ 02, 47 SAY 'Forma P:'
     @ 03, 01 SAY  ;
       'Emisor            :'
     @ 03, 47 SAY 'Doc.Ref:'
     @ 04, 01 SAY  ;
       'Almac굈           :'
     @ 04, 47 SAY 'Moneda :'
     @ 01, 21 SAY w_fchemi
     IF w_estdoc = 'A'
          @ 01, 45 SAY  ;
            'A  N  U  L  A  D  O'  ;
            COLOR W+/N* 
     ENDIF
     SELECT gc_cli00
     SEEK ALLTRIM(w_tipent) +  ;
          ALLTRIM(w_codent)
     IF FOUND()
          @ 02, 21 SAY  ;
            SUBSTR(cli_razsoc, 1,  ;
            25)
     ELSE
          DO p_mensaje WITH  ;
             ' C줰igo No Existe  '
     ENDIF
     SELECT ge_tab0
     SEEK 'FPAG' + w_tipago
     IF FOUND()
          @ 02, 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
     ENDIF
     SEEK 'EMIS' + w_codemi
     IF FOUND()
          @ 03, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ENDIF
     @ 03, 56 SAY w_numore
     SEEK 'ALMA' + w_codalm
     IF FOUND()
          @ 04, 21 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     ENDIF
     SEEK 'MONE' + w_codmon
     IF FOUND()
          @ 04, 56 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            12)
     ENDIF
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
     IF gc_hve00.hve_codmov =  ;
        'PCTA'
          SELECT 21
          USE SHARED gc_dpv00  ;
              ORDER codigo
          SEEK w_tipdoc +  ;
               w_numero
          IF FOUND()
               w_cosrep = 0
               SCAN WHILE  ;
                    dpv_tipdoc =  ;
                    w_tipdoc  ;
                    .AND.  ;
                    dpv_nrodoc =  ;
                    w_numero  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SELECT fact
                    APPEND BLANK
                    REPLACE fac_codpro  ;
                            WITH  ;
                            gc_dpv00.dpv_codpro
                    REPLACE fac_cantid  ;
                            WITH  ;
                            gc_dpv00.dpv_cantid
                    SELECT gc_pro00
                    SEEK fact.fac_codpro
                    IF FOUND()
                         SELECT fact
                         REPLACE fac_despro  ;
                                 WITH  ;
                                 gc_pro00.pro_descri
                    ENDIF
                    SELECT gc_dpv00
               ENDSCAN
          ENDIF
     ELSE
          SELECT gc_dve00
          SEEK w_tipdoc +  ;
               w_numero
          IF FOUND()
               w_cosrep = 0
               SCAN WHILE  ;
                    dve_tipdoc =  ;
                    w_tipdoc  ;
                    .AND.  ;
                    dve_nrodoc =  ;
                    w_numero  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SELECT fact
                    APPEND BLANK
                    REPLACE fac_codpro  ;
                            WITH  ;
                            gc_dve00.dve_propar
                    REPLACE fac_cantid  ;
                            WITH  ;
                            gc_dve00.dve_cantid
                    IF w_tipdoc =  ;
                       'FACT'
                         IF w_codmon =  ;
                            'DOL '
                              REPLACE  ;
                               fac_import  ;
                               WITH  ;
                               gc_dve00.dve_import
                         ELSE
                              REPLACE  ;
                               fac_import  ;
                               WITH  ;
                               gc_dve00.dve_impors
                         ENDIF
                    ELSE
                         IF w_codmon =  ;
                            'DOL '
                              REPLACE  ;
                               fac_import  ;
                               WITH  ;
                               ROUND(gc_dve00.dve_import *  ;
                               w_facigv,  ;
                               2)
                         ELSE
                              REPLACE  ;
                               fac_import  ;
                               WITH  ;
                               ROUND(ROUND(gc_dve00.dve_import *  ;
                               w_facigv,  ;
                               2) *  ;
                               w_tipcam,  ;
                               2)
                         ENDIF
                    ENDIF
                    REPLACE fac_total  ;
                            WITH  ;
                            ROUND(fac_import *  ;
                            fac_cantid,  ;
                            2)
                    w_cosrep = w_cosrep +  ;
                               fac_total
                    SELECT gc_pro00
                    SEEK fact.fac_codpro
                    IF FOUND()
                         SELECT fact
                         REPLACE fac_despro  ;
                                 WITH  ;
                                 gc_pro00.pro_descri
                    ENDIF
                    SELECT gc_dve00
               ENDSCAN
               IF w_tipdoc =  ;
                  'FACT'
                    w_cosrep = ROUND(w_cosrep *  ;
                               w_facigv,  ;
                               2)
               ENDIF
          ENDIF
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
     IF opcion = 1
          DO p_footer WITH  ;
             '100100000000000000001',  ;
             2
          ON KEY LABEL f4 do p_borra
     ELSE
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
     ENDIF
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
     IF gc_hve00.hve_codmov =  ;
        'PCTA' .AND.  ;
        gc_hve00.hve_indori =  ;
        'VENT' .AND.  ;
        gc_hve00.hve_estdoc <>  ;
        'V'
          DO p_mensaje WITH  ;
             'No puede Anularse,tiene que anular primero la Liquidaci줻'
          SELECT fact
          RETURN
     ENDIF
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
     IF hve_indori = 'VENT'
          DO CASE
               CASE hve_codmov =  ;
                    'PCTA'
                    SELECT 20
                    USE SHARED  ;
                        gc_hpv00  ;
                        ORDER  ;
                        codigo
                    SEEK w_tipdoc +  ;
                         w_nrodoc
                    IF FOUND()  ;
                       .AND.  ;
                       hpv_indest =  ;
                       'U'
                         DO rbloquea
                         REPLACE hpv_indest  ;
                                 WITH  ;
                                 'A'
                         REPLACE hpv_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE hpv_fecha  ;
                                 WITH  ;
                                 DATE()
                         REPLACE hpv_hora  ;
                                 WITH  ;
                                 TIME()
                         UNLOCK
                    ENDIF
                    SELECT gc_hve00
                    DO rbloquea
                    REPLACE hve_estdoc  ;
                            WITH  ;
                            'A',  ;
                            hve_usuari  ;
                            WITH  ;
                            clave
                    REPLACE hve_fecha  ;
                            WITH  ;
                            DATE(),  ;
                            hve_hora  ;
                            WITH  ;
                            TIME()
                    UNLOCK
               CASE hve_codmov =  ;
                    'EVTA'
                    SELECT 20
                    USE SHARED  ;
                        gc_hip00  ;
                        ORDER  ;
                        codigo
                    SEEK w_tipdoc +  ;
                         w_nrodoc
                    IF FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Documento ya existe'
                         SELECT fact
                         RETURN
                    ELSE
                         IF gc_hve00.hve_estdoc =  ;
                            'C'
                              APPEND  ;
                               BLANK
                              DO rbloquea
                              REPLACE  ;
                               hip_tipdoc  ;
                               WITH  ;
                               w_tipdoc
                              REPLACE  ;
                               hip_nrodoc  ;
                               WITH  ;
                               w_nrodoc
                              REPLACE  ;
                               hip_fecdoc  ;
                               WITH  ;
                               gc_hve00.hve_fecdoc
                              REPLACE  ;
                               hip_fecvct  ;
                               WITH  ;
                               gc_hve00.hve_fecdoc
                              REPLACE  ;
                               hip_fecmon  ;
                               WITH  ;
                               gc_hve00.hve_fecdoc
                              REPLACE  ;
                               hip_codmov  ;
                               WITH  ;
                               'IFD'
                              REPLACE  ;
                               hip_almrec  ;
                               WITH  ;
                               gc_hve00.hve_almdes
                              REPLACE  ;
                               hip_tipent  ;
                               WITH  ;
                               gc_hve00.hve_tipent
                              REPLACE  ;
                               hip_codent  ;
                               WITH  ;
                               gc_hve00.hve_codent
                              REPLACE  ;
                               hip_mondoc  ;
                               WITH  ;
                               gc_hve00.hve_codmon
                              REPLACE  ;
                               hip_tidore  ;
                               WITH  ;
                               gc_hve00.hve_tipdoc
                              REPLACE  ;
                               hip_nrdore  ;
                               WITH  ;
                               gc_hve00.hve_nrodoc
                              REPLACE  ;
                               hip_totnet  ;
                               WITH  ;
                               gc_hve00.hve_totvta
                              REPLACE  ;
                               hip_totigv  ;
                               WITH  ;
                               gc_hve00.hve_totigv
                              REPLACE  ;
                               hip_totoim  ;
                               WITH  ;
                               gc_hve00.hve_totoim
                              REPLACE  ;
                               hip_totgen  ;
                               WITH  ;
                               gc_hve00.hve_totoim
                              REPLACE  ;
                               hip_codemi  ;
                               WITH  ;
                               gc_hve00.hve_codemi
                              REPLACE  ;
                               hip_codvnd  ;
                               WITH  ;
                               gc_hve00.hve_codvnd
                              REPLACE  ;
                               hip_tippag  ;
                               WITH  ;
                               gc_hve00.hve_tippag
                              REPLACE  ;
                               hip_lispre  ;
                               WITH  ;
                               gc_hve00.hve_lispre
                              REPLACE  ;
                               hip_estdoc  ;
                               WITH  ;
                               gc_hve00.hve_estdoc
                              REPLACE  ;
                               hip_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               hip_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               hip_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                    ENDIF
                    w_fla = 0
                    SELECT gc_hve00
                    IF hve_tidore <>  ;
                       SPACE(4)  ;
                       .AND.  ;
                       hve_nrdore <>  ;
                       SPACE(10)  ;
                       .AND.  ;
                       hve_pagcts >  ;
                       0
                         w_tidore =  ;
                          gc_hve00.hve_tidore
                         w_nrdore =  ;
                          gc_hve00.hve_nrdore
                         SELECT gc_hve00
                         SEEK w_tidore +  ;
                              w_nrdore
                         IF FOUND()
                              DO rbloquea
                              REPLACE  ;
                               hve_estdoc  ;
                               WITH  ;
                               'V'
                              REPLACE  ;
                               hve_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               hve_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               hve_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                         SELECT 20
                         USE SHARED  ;
                             gc_hpv00  ;
                             ORDER  ;
                             codigo
                         SEEK w_tidore +  ;
                              w_nrdore
                         IF FOUND()  ;
                            .AND.  ;
                            hpv_indest =  ;
                            'F'
                              w_fla =  ;
                               1
                              DO rbloquea
                              REPLACE  ;
                               hpv_indest  ;
                               WITH  ;
                               'S'
                              REPLACE  ;
                               hpv_fecfin  ;
                               WITH  ;
                               CTOD(SPACE(8))
                              REPLACE  ;
                               hpv_horfin  ;
                               WITH  ;
                               SPACE(8)
                              REPLACE  ;
                               hpv_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               hpv_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               hpv_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                    ENDIF
                    SELECT gc_hve00
                    SEEK w_tipdoc +  ;
                         w_nrodoc
                    DO rbloquea
                    REPLACE hve_estdoc  ;
                            WITH  ;
                            'A',  ;
                            hve_usuari  ;
                            WITH  ;
                            clave
                    REPLACE hve_fecha  ;
                            WITH  ;
                            DATE(),  ;
                            hve_hora  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    SELECT 20
                    USE SHARED  ;
                        gc_dip00  ;
                        ORDER  ;
                        codigo
                    SELECT 21
                    USE SHARED  ;
                        gc_dpv00  ;
                        ORDER  ;
                        codigo
                    SELECT 22
                    USE SHARED  ;
                        gc_kar00  ;
                        ORDER  ;
                        codigo
                    SELECT 23
                    USE SHARED  ;
                        gc_alm00  ;
                        ORDER  ;
                        codigo
                    SELECT gc_dve00
                    SEEK w_tipdoc +  ;
                         w_nrodoc
                    IF FOUND()
                         SCAN WHILE  ;
                              (dve_tipdoc =  ;
                              w_tipdoc  ;
                              .AND.  ;
                              dve_nrodoc =  ;
                              w_nrodoc)  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              SELECT  ;
                               gc_pro00
                              SEEK  ;
                               c.dve_propar
                              IF FOUND()
                                   DO rbloquea
                                   REPLACE pro_ultmov WITH DATE(), pro_hora WITH TIME()
                                   REPLACE pro_fecha WITH DATE(), pro_usuari WITH clave
                                   UNLOCK
                                   w_cosant = pro_coprmo
                                   w_cosanb = pro_coprmb
                                   w_cosuni = pro_ulcomb
                                   w_cosuns = pro_ultcos
                              ELSE
                                   w_cosant = 0
                                   w_cosanb = 0
                                   w_cosuni = 0
                                   w_cosuns = 0
                              ENDIF
                              w_linea =  ;
                               w_linea +  ;
                               1
                              w_stkfis =  ;
                               0
                              SELECT  ;
                               gc_alm00
                              SEEK  ;
                               c.dve_propar +  ;
                               b.hve_almdes
                              IF FOUND()
                                   DO rbloquea
                                   w_stkfis = alm_stkfis
                                   REPLACE alm_stkfis WITH alm_stkfis + c.dve_cantid
                                   REPLACE alm_fecha WITH DATE()
                                   REPLACE alm_hora WITH TIME()
                                   REPLACE alm_usuari WITH clave
                                   UNLOCK
                              ENDIF
                              SELECT  ;
                               gc_kar00
                              APPEND  ;
                               BLANK
                              DO rbloquea
                              REPLACE  ;
                               kar_tipdoc  ;
                               WITH  ;
                               w_tipdoc,  ;
                               kar_nrodoc  ;
                               WITH  ;
                               w_nrodoc
                              REPLACE  ;
                               kar_fecing  ;
                               WITH  ;
                               DATE(),  ;
                               kar_horing  ;
                               WITH  ;
                               TIME()
                              REPLACE  ;
                               kar_lindet  ;
                               WITH  ;
                               STR(w_linea,  ;
                               2),  ;
                               kar_fecdoc  ;
                               WITH  ;
                               gc_hve00.hve_fecdoc
                              REPLACE  ;
                               kar_codmov  ;
                               WITH  ;
                               'IFD',  ;
                               kar_unimed  ;
                               WITH  ;
                               gc_dve00.dve_unimed
                              REPLACE  ;
                               kar_stkant  ;
                               WITH  ;
                               w_stkfis,  ;
                               kar_cantid  ;
                               WITH  ;
                               gc_dve00.dve_cantid
                              REPLACE  ;
                               kar_almrec  ;
                               WITH  ;
                               gc_hve00.hve_almdes,  ;
                               kar_import  ;
                               WITH  ;
                               gc_dve00.dve_import
                              REPLACE  ;
                               kar_cosant  ;
                               WITH  ;
                               w_cosant,  ;
                               kar_cosanb  ;
                               WITH  ;
                               w_cosanb
                              REPLACE  ;
                               kar_cosuni  ;
                               WITH  ;
                               w_cosuni,  ;
                               kar_cosuns  ;
                               WITH  ;
                               w_cosuns
                              REPLACE  ;
                               kar_tipent  ;
                               WITH  ;
                               gc_hve00.hve_tipent
                              REPLACE  ;
                               kar_codent  ;
                               WITH  ;
                               gc_hve00.hve_codent,  ;
                               kar_tidore  ;
                               WITH  ;
                               gc_hve00.hve_tidore
                              REPLACE  ;
                               kar_nrdore  ;
                               WITH  ;
                               gc_hve00.hve_nrdore,  ;
                               kar_codmon  ;
                               WITH  ;
                               gc_hve00.hve_codmon
                              REPLACE  ;
                               kar_almrec  ;
                               WITH  ;
                               gc_hve00.hve_almdes,  ;
                               kar_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               kar_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               kar_hora  ;
                               WITH  ;
                               TIME()
                              REPLACE  ;
                               kar_codpro  ;
                               WITH  ;
                               gc_dve00.dve_propar
                              UNLOCK
                              SELECT  ;
                               gc_dip00
                              APPEND  ;
                               BLANK
                              DO rbloquea
                              REPLACE  ;
                               dip_tipdoc  ;
                               WITH  ;
                               w_tipdoc,  ;
                               dip_propar  ;
                               WITH  ;
                               gc_dve00.dve_propar
                              REPLACE  ;
                               dip_nrodoc  ;
                               WITH  ;
                               gc_dve00.dve_nrodoc,  ;
                               dip_unimed  ;
                               WITH  ;
                               gc_dve00.dve_unimed
                              REPLACE  ;
                               dip_cantid  ;
                               WITH  ;
                               gc_dve00.dve_cantid,  ;
                               dip_pordes  ;
                               WITH  ;
                               gc_dve00.dve_pordes
                              REPLACE  ;
                               dip_import  ;
                               WITH  ;
                               gc_dve00.dve_import,  ;
                               dip_total  ;
                               WITH  ;
                               gc_dve00.dve_total
                              REPLACE  ;
                               dip_usuari  ;
                               WITH  ;
                               clave,  ;
                               dip_fecha  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               dip_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                              IF w_fla =  ;
                                 1
                                   SELECT gc_dpv00
                                   SEEK gc_hve00.hve_tidore + gc_hve00.hve_nrdore + c.dve_propar
                                   IF FOUND()
                                        DO rbloquea
                                        IF c.dve_cantid < dpv_antres
                                             w_can = c.dve_cantid
                                        ELSE
                                             w_can = dpv_antres
                                        ENDIF
                                        REPLACE dpv_candes WITH dpv_candes - c.dve_cantid
                                        REPLACE dpv_indest WITH 'C'
                                        REPLACE dpv_fecfin WITH CTOD(SPACE(8))
                                        REPLACE dpv_horfin WITH SPACE(8)
                                        REPLACE dpv_usuari WITH clave
                                        REPLACE dpv_fecha WITH DATE()
                                        REPLACE dpv_hora WITH TIME()
                                        UNLOCK
                                        IF (gc_alm00.alm_stkfis - gc_alm00.alm_stkres) >= w_can
                                             DO rbloquea
                                             REPLACE dpv_stkres WITH dpv_stkres + w_can
                                             UNLOCK
                                             SELECT gc_alm00
                                             DO rbloquea
                                             REPLACE alm_stkres WITH alm_stkres + w_can
                                             UNLOCK
                                        ENDIF
                                   ENDIF
                              ENDIF
                              SELECT  ;
                               gc_dve00
                         ENDSCAN
                    ENDIF
                    IF gc_hve00.hve_almdes =  ;
                       rge_codalm
                         DO imp_anula
                    ENDIF
          ENDCASE
          DEACTIVATE WINDOW  ;
                     winmensaje
          DEACTIVATE POPUP  ;
                     detalle
          ACTIVATE WINDOW  ;
                   cabecera
          RETURN
     ENDIF
ENDIF
SELECT fact
RETURN
*
PROCEDURE numero
w_nrodoc = f_ceros(w_nrodoc,10,2)
@ 00, 56 GET w_nrodoc
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
