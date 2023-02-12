*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opcion
SET CENTURY ON
ON KEY
DEFINE WINDOW t0 FROM 02, 64 TO  ;
       02, 73 IN screen NONE
DEFINE WINDOW a0 FROM 04, 01 TO  ;
       11, 78 IN screen
DEFINE WINDOW a3 FROM 11, 01 TO  ;
       21, 78 IN screen
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
ACTIVATE WINDOW tablas
IF opcion = 1
     DO p_prestab WITH 'PROCESO',  ;
        'PAGOS A CUENTA AUTOMATICO',  ;
        'SELECCION', .F.
ELSE
     DO p_prestab WITH 'PROCESO',  ;
        'PAGOS A CUENTA MANUAL',  ;
        'SELECCION', .F.
ENDIF
@ 02, 63 SAY PROGRAM()
CLOSE DATABASES
SELECT 1
USE gc_hve00 ORDER codigo
SELECT 2
USE ge_tab0 ORDER codigo
SELECT 3
USE gc_cli00 ORDER codigo
SELECT 4
USE gc_vnd00 ORDER codigo
SELECT 5
USE gc_hpv00 ORDER codigo
SELECT 6
USE gc_dpv00 ORDER codigo
SELECT 7
USE gc_pro00 ORDER codigo
SELECT 8
USE gc_alm00 ORDER codigo
SELECT 9
USE gc_dlp00 ORDER codigo
SELECT 20
USE SHARED gc_cmv00 ORDER  ;
    cmv_feinmo
restc = ootc2(DATE(),'SOL ', ;
        'DOL ','2')
IF restc = -1
     DO p_mensaje WITH  ;
        'NO EXISTE TIPO DE CAMBIO'
     STORE 1 TO w_fin
ELSE
     STORE 0 TO imp_sol, imp_dol,  ;
           w_facigv, w_fin,  ;
           w_total, w_pagcta,  ;
           w_simbo, w_f5
ENDIF
w_facigv = facigv()
STORE SPACE(20) TO w_desmon,  ;
      w_desven
DO WHILE w_fin<>1
     STORE 0 TO w_fin, w_exi,  ;
           rep_sol, rep_dol, p,  ;
           w_paso, aux_opci
     STORE SPACE(01) TO w_campo
     STORE SPACE(10) TO w_selec
     ACTIVATE WINDOW t0
     @ 00, 00 SAY 'SELECCION'  ;
       COLOR N+/W 
     IF w_f5 = 0
          STORE 0 TO m
          DO pinta1
          SELECT gc_hve00
          SCATTER BLANK MEMVAR
     ENDIF
     ON KEY
     ACTIVATE WINDOW a0
     @ 00, 12 GET m.hve_tipdoc  ;
       DEFAULT ' ' SIZE 1, 04  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
     IF opcion = 2
          @ 00, 51 GET  ;
            m.hve_nrodoc DEFAULT  ;
            ' ' SIZE 1, 10  ;
            PICTURE '@!' VALID  ;
            oovalid(VARREAD())  ;
            WHEN  ;
            oowhen(VARREAD())
     ENDIF
     READ CYCLE
     IF LASTKEY() <> 27
          DO det_cab WITH 0
     ELSE
          w_fin = 1
     ENDIF
ENDDO
DEACTIVATE WINDOW tablas
RELEASE WINDOW t0, a0, a1, a3, a4
CLOSE DATABASES
ON KEY
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE det_cab
PARAMETER w_opc
@ 01, 12 GET m.hve_codent DEFAULT  ;
  ' ' SIZE 1, 11 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 02, 12 GET m.hve_codvnd DEFAULT  ;
  ' ' SIZE 1, 5 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
m.hve_codemi = rge_punemi
IF opcion = 2
     @ 02, 51 GET m.hve_fecdoc  ;
       DEFAULT DATE() SIZE 1, 8  ;
       PICTURE '@D' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD())
ENDIF
@ 03, 12 GET m.hve_codemi DEFAULT  ;
  rge_punemi SIZE 1, 4 PICTURE  ;
  '@!' VALID oovalid(VARREAD())  ;
  WHEN oowhen(VARREAD())
@ 04, 12 GET m.hve_almdes SIZE 1,  ;
  4 PICTURE '@!' VALID  ;
  oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
@ 05, 12 GET m.hve_codmon DEFAULT  ;
  'SOL ' SIZE 1, 4 PICTURE '@!'  ;
  VALID oovalid(VARREAD()) WHEN  ;
  oowhen(VARREAD())
IF w_opc = 0
     READ CYCLE
     IF w_f5 = 0
          SELECT gc_hve00
          SCATTER BLANK MEMVAR
          = limpia()
     ENDIF
ELSE
     CLEAR GETS
ENDIF
RETURN
*
PROCEDURE graba
ON KEY
IF m.hve_codmon = 'DOL'
     imp_dol = ROUND(w_pagcta, 2)
     rep_dol = ROUND(w_pagcta /  ;
               w_facigv, 2)
     rep_sol = ROUND(rep_dol *  ;
               restc, 2)
     imp_sol = ROUND(rep_sol *  ;
               w_facigv, 2)
ELSE
     imp_sol = ROUND(w_pagcta, 2)
     rep_sol = ROUND(w_pagcta /  ;
               w_facigv, 2)
     rep_dol = ROUND(rep_sol /  ;
               restc, 2)
     imp_dol = ROUND(rep_dol *  ;
               w_facigv, 2)
ENDIF
mto_sol = ROUND(imp_sol /  ;
          w_facigv, 2)
igv_sol = imp_sol - mto_sol
mto_dol = ROUND(imp_dol /  ;
          w_facigv, 2)
igv_dol = imp_dol - mto_dol
SELECT gc_hve00
IF LASTKEY() = 27
     RETURN
ENDIF
m.hve_tippag = '001'
IF opcion = 1
     DO status_imp
     DO actual1
     m.hve_nrodoc = ALLTRIM(STR(m.hve_nrodoc,  ;
                    10))
     m.hve_nrodoc = f_ceros(m.hve_nrodoc, ;
                    10,2)
     DO imprime
     SET DEVICE TO SCREEN
     SET PRINTER TO
     CLEAR READ
     = ooactprn()
ENDIF
m.hve_tipent = 'C'
m.hve_codmov = 'PCTA'
m.hve_fechtc = DATE()
m.hve_indori = 'VENT'
SELECT gc_hve00
APPEND BLANK
DO rbloquea
GATHER MEMVAR
REPLACE hve_solrep WITH rep_sol,  ;
        hve_solnet WITH imp_sol,  ;
        hve_solvta WITH mto_sol,  ;
        hve_soligv WITH igv_sol,  ;
        hve_solgen WITH imp_sol,  ;
        hve_mtocan WITH imp_sol
REPLACE hve_totnet WITH imp_dol,  ;
        hve_totvta WITH mto_dol,  ;
        hve_totigv WITH igv_dol,  ;
        hve_totgen WITH imp_dol,  ;
        hve_totoim WITH imp_dol
REPLACE hve_tipcam WITH restc,  ;
        hve_codent WITH  ;
        m.hve_codent, hve_tipdoc  ;
        WITH m.hve_tipdoc
REPLACE hve_nrodoc WITH  ;
        m.hve_nrodoc, hve_codemi  ;
        WITH m.hve_codemi,  ;
        hve_codvnd WITH  ;
        m.hve_codvnd, hve_estdoc  ;
        WITH 'V'
REPLACE hve_usuari WITH clave,  ;
        hve_fecha WITH DATE(),  ;
        hve_hora WITH TIME()
UNLOCK
SELECT gc_hpv00
APPEND BLANK
DO rbloquea
REPLACE hpv_tipdoc WITH  ;
        m.hve_tipdoc, hpv_nrodoc  ;
        WITH m.hve_nrodoc,  ;
        hpv_fecdoc WITH  ;
        m.hve_fecdoc, hpv_hordoc  ;
        WITH TIME()
REPLACE hpv_codalm WITH  ;
        m.hve_almdes, hpv_pagcta  ;
        WITH imp_sol, hpv_porcta  ;
        WITH ROUND((100 *  ;
        w_pagcta) / w_totnet, 2)
REPLACE hpv_indest WITH 'U',  ;
        hpv_usuari WITH clave,  ;
        hpv_fecha WITH DATE(),  ;
        hpv_hora WITH TIME()
REPLACE hpv_codent WITH  ;
        m.hve_codent, hpv_codmon  ;
        WITH 'SOL '
UNLOCK
SELECT pcta
GOTO TOP
SCAN WHILE  .NOT. EOF()
     SELECT gc_dpv00
     APPEND BLANK
     DO rbloquea
     REPLACE dpv_tipdoc WITH  ;
             m.hve_tipdoc,  ;
             dpv_nrodoc WITH  ;
             m.hve_nrodoc,  ;
             dpv_codprp WITH  ;
             pcta.codpro,  ;
             dpv_codpro WITH  ;
             pcta.codpro
     REPLACE dpv_cantid WITH  ;
             pcta.cantid,  ;
             dpv_fecdoc WITH  ;
             m.hve_fecdoc,  ;
             dpv_hordoc WITH  ;
             TIME(), dpv_indest  ;
             WITH 'U'
     REPLACE dpv_usuari WITH  ;
             clave, dpv_fecha  ;
             WITH DATE(),  ;
             dpv_hora WITH  ;
             TIME()
     UNLOCK
     SELECT pcta
ENDSCAN
RETURN
*
PROCEDURE imprime
ON KEY
STORE 0 TO aux_sw1, aux_con1,  ;
      aux_con2, aux_tv, aux_ta
SET DEVICE TO PRINTER
SET PRINTER TO
IF m.hve_tipdoc = 'BOLE'
     set print to &rge_lptbol
ELSE
     set print to &rge_lptfac
ENDIF
DO imprimir
RETURN
*
PROCEDURE imprimir
DO r_titulo
@ 33, 00 SAY  ;
  'PAGO A CUENTA POR PEDIDO DIRECTO DE REPUESTOS'
IF m.hve_tipdoc = 'FACT'
     @ 36, 62 SAY w_simbo
     IF m.hve_codmon = 'DOL '
          @ 36, 66 SAY mto_dol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 37, 66 SAY igv_dol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY imp_dol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 40, 01 SAY  ;
            oonumlet(imp_dol, ;
            m.hve_codmon)
     ELSE
          @ 36, 66 SAY mto_sol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 37, 66 SAY igv_sol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY imp_sol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 40, 01 SAY  ;
            oonumlet(imp_sol, ;
            m.hve_codmon)
     ENDIF
     @ 44, 63 SAY m.hve_nrodoc
ELSE
     @ 36, 62 SAY w_simbo
     IF m.hve_codmon = 'DOL '
          @ 36, 66 SAY imp_dol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY imp_dol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 40, 01 SAY  ;
            oonumlet(imp_dol, ;
            m.hve_codmon)
     ELSE
          @ 36, 66 SAY imp_sol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY imp_sol  ;
            PICTURE  ;
            '999,999,999.99'
          @ 40, 01 SAY  ;
            oonumlet(imp_sol, ;
            m.hve_codmon)
     ENDIF
     @ 44, 63 SAY m.hve_nrodoc
ENDIF
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE r_titulo
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(51)
@ 03, 56 SAY 'R.U.C.  ' +  ;
  rge_numruc
IF m.hve_tipdoc = 'FACT'
     @ 11, 00 SAY m.hve_fecdoc  ;
       PICTURE '99/99/9999'
     @ 11, 15 SAY m.hve_codemi
     SELECT gc_cli00
     SEEK 'C' + m.hve_codent
     IF  .NOT. FOUND()
          @ 12, 05 SAY ' '
     ELSE
          @ 13, 00 SAY cli_razsoc
          @ 13, 57 SAY  ;
            m.hve_codent
          @ 15, 00 SAY  ;
            gc_cli00.cli_calle
          @ 15, 16 SAY  ;
            gc_cli00.cli_numero
          @ 15, 60 SAY  ;
            STR(gc_cli00.cli_telefo,  ;
            10)
     ENDIF
     SELECT ge_tab0
     SEEK 'FPAG' + m.hve_tippag
     @ 17, 00 SAY m.hve_almdes
     @ 17, 12 SAY SUBSTR(w_desven,  ;
       1, 14)
     @ 17, 28 SAY w_desmon
     @ 17, 45 SAY  ;
       SUBSTR(tab_destab, 1, 15)
ELSE
     @ 11, 00 SAY m.hve_fecdoc  ;
       PICTURE '99/99/9999'
     @ 11, 15 SAY m.hve_codemi
     SELECT gc_cli00
     SEEK 'C' + m.hve_codent
     IF  .NOT. FOUND()
          @ 13, 05 SAY ' '
     ELSE
          @ 13, 00 SAY cli_razsoc
          @ 13, 57 SAY  ;
            m.hve_codent
          @ 15, 00 SAY  ;
            gc_cli00.cli_calle
          @ 15, 16 SAY  ;
            gc_cli00.cli_numero
          @ 15, 60 SAY  ;
            STR(gc_cli00.cli_telefo,  ;
            10)
     ENDIF
     SELECT ge_tab0
     SEEK 'FPAG' + m.hve_tippag
     @ 17, 00 SAY m.hve_almdes
     @ 17, 12 SAY SUBSTR(w_desven,  ;
       1, 14)
     @ 17, 28 SAY w_desmon
     @ 17, 45 SAY  ;
       SUBSTR(tab_destab, 1, 15)
ENDIF
SELECT pcta
GOTO TOP
IF m.hve_tipdoc = 'FACT'
     aux_lin = 21
ELSE
     aux_lin = 21
ENDIF
STORE 1 TO mitem
SCAN WHILE  .NOT. EOF()
     @ aux_lin, 00 SAY mitem  ;
       PICTURE '999'
     @ aux_lin, 15 SAY  ;
       SUBSTR(descri, 1, 28)
     @ aux_lin, 47 SAY cantid  ;
       PICTURE '999,999'
     aux_lin = aux_lin + 1
     mitem = mitem + 1
ENDSCAN
RETURN
*
PROCEDURE pinta1
DEACTIVATE WINDOW titposs
ACTIVATE WINDOW a0
@ 00, 00 CLEAR TO 08, 74
@ 00, 00 SAY 'Tipo Doc. :'
@ 00, 64 SAY 'TC:'
@ 00, 68 SAY restc PICTURE  ;
  '9,999.99'
@ 01, 00 SAY 'Cliente   :'
@ 02, 00 SAY 'Vendedor  :'
IF opcion = 2
     @ 00, 40 SAY 'Nro. Doc.:'
     @ 02, 40 SAY 'Fech Doc.:'
ENDIF
@ 03, 00 SAY 'Emisor    :'
@ 04, 00 SAY 'Almac‚n   :'
@ 05, 00 SAY 'C¢d.Moneda:'
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'HVE_TIPDOC'
          wrk_busca = 'DOCU'
          wrk_var = 'M.HVE_TIPDOC'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW a0
          ON KEY LABEL f6 do busca with;
wrk_busca,wrk_var 
          STORE DATE() TO  ;
                m.hve_fecdoc,  ;
                m.hve_fecvct,  ;
                m.hve_fecent
          IF w_f5 = 0
               DO det_cab WITH 1
          ENDIF
     CASE cwhen = 'HVE_NRODOC'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW a0
     CASE cwhen = 'HVE_CODENT'  ;
          .OR. cwhen =  ;
          'HVE_CODVND'
          DO CASE
               CASE cwhen =  ;
                    'HVE_CODENT'
                    wrk_busca = 'C'
                    wrk_var = 'M.HVE_CODENT'
               OTHERWISE
                    wrk_busca = 'V'
                    wrk_var = 'M.HVE_CODVND'
          ENDCASE
          ACTIVATE WINDOW a0
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do entidad with;
wrk_busca,wrk_var
     CASE cwhen = 'HVE_CODEMI'
          wrk_busca = 'EMIS'
          wrk_var = 'M.HVE_CODEMI'
          ON KEY LABEL f6 do busca with;
wrk_busca,wrk_var 
     CASE cwhen = 'HVE_CODMON'
          wrk_busca = 'MONE'
          wrk_var = 'M.HVE_CODMON'
          ON KEY LABEL f6 do busca with;
wrk_busca,wrk_var
     CASE cwhen = 'HVE_ALMDES'
          wrk_busca = 'ALMA'
          wrk_var = 'M.HVE_ALMDES'
          ON KEY LABEL f6 do busca with;
wrk_busca,wrk_var
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'HVE_TIPDOC'
          IF (LASTKEY() = 5) .OR.  ;
             (LASTKEY() = 19)
               RETURN .F.
          ENDIF
          wrk_busca = 'DOCU'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_tipdoc
          @ 00, 17 SAY  ;
            REPLICATE(' ', 23)
          IF FOUND()
               IF m.hve_tipdoc =  ;
                  'FACT' .OR.  ;
                  m.hve_tipdoc =  ;
                  'BOLE'
                    @ 00, 17 SAY  ;
                      SUBSTR(ge_tab0.tab_destab,  ;
                      1, 23)
               ELSE
                    DO p_mensaje  ;
                       WITH  ;
                       ' TIPO DE DOCUMENTO NO VALIDO '
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' TIPO DE DOCUMENTO NO EXISTE '
               RETURN .F.
          ENDIF
          IF opcion = 1
               CLEAR READ
          ENDIF
     CASE cvalid = 'HVE_NRODOC'
          w_exi = 0
          SELECT gc_hve00
          m.hve_nrodoc = f_ceros(m.hve_nrodoc, ;
                         10,2)
          SEEK m.hve_tipdoc +  ;
               m.hve_nrodoc
          IF  .NOT.  ;
              EMPTY(m.hve_nrodoc)
               IF FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       ' NUMERO DE DOCUMENTO YA EXISTE '
                    w_exi = 1
                    RETURN .F.
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  ' ES NECESARIO NUMERO DE DOCUMENTO '
               RETURN .F.
          ENDIF
          @ 00, 51 SAY  ;
            m.hve_nrodoc PICTURE  ;
            '@!'
          CLEAR READ
     CASE cvalid = 'HVE_CODENT'
          wrk_busca = 'C'
          SELECT gc_cli00
          SEEK wrk_busca +  ;
               m.hve_codent
          @ 01, 22 SAY  ;
            REPLICATE(' ', 54)
          IF FOUND()
               @ 01, 24 SAY  ;
                 gc_cli00.cli_razsoc +  ;
                 SPACE(07) +  ;
                 'Tel‚f:' +  ;
                 STR(cli_telefo,  ;
                 9)
          ELSE
               swt = f_yesno( ;
                     ' Cliente no Existe, Desea ingresarlo ' ;
                     )
               IF swt
                    SCATTER BLANK  ;
                            MEMVAR
                    m.cli_tpper =  ;
                     'C'
                    m.cli_codigo =  ;
                     hve_codent
                    IF  .NOT.  ;
                        EMPTY(m.cli_codigo)
                         ON KEY
                         = ooclientes()
                    ENDIF
               ENDIF
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_CODVND'
          wrk_busca = 'V'
          SELECT gc_vnd00
          SEEK wrk_busca +  ;
               m.hve_codvnd
          @ 02, 22 SAY  ;
            REPLICATE(' ', 17)
          IF FOUND()
               @ 02, 22 SAY  ;
                 SUBSTR(gc_vnd00.vnd_nombre,  ;
                 1, 17)
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE VENDEDOR NO EXISTE '
               RETURN .F.
          ENDIF
          w_desven = gc_vnd00.vnd_nombre
     CASE cvalid = 'HVE_CODEMI'
          wrk_busca = 'EMIS'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_codemi
          @ 03, 17 SAY  ;
            REPLICATE(' ', 24)
          IF FOUND()
               @ 03, 17 SAY  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 24)
          ELSE
               DO p_mensaje WITH  ;
                  ' CODIGO DE EMISOR NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_ALMDES'
          wrk_busca = 'ALMA'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_almdes
          @ 04, 17 SAY  ;
            REPLICATE(' ', 24)
          IF FOUND()
               @ 04, 17 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 24)
          ELSE
               DO p_mensaje WITH  ;
                  'CODIGO DE ALMACEN NO EXISTE '
               RETURN .F.
          ENDIF
     CASE cvalid = 'HVE_CODMON'
          wrk_busca = 'MONE'
          SELECT ge_tab0
          SEEK wrk_busca +  ;
               m.hve_codmon
          @ 05, 17 SAY  ;
            REPLICATE(' ', 24)
          IF FOUND()
               @ 05, 17 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 24)
          ELSE
               DO p_mensaje WITH  ;
                  'CODIGO DE MONEDA NO EXISTE '
               RETURN .F.
          ENDIF
          w_desmon = tab_destab
          IF w_f5 = 0
               DO carga
          ELSE
               w_f5 = 0
               = pre_pro(2)
          ENDIF
          DO detalle
ENDCASE
RETURN
*
PROCEDURE limpia
ACTIVATE WINDOW a0
@ 00, 17 SAY REPLICATE(' ', 24)
@ 01, 25 SAY REPLICATE(' ', 53)
@ 02, 25 SAY REPLICATE(' ', 24)
@ 03, 17 SAY REPLICATE(' ', 24)
@ 03, 59 SAY REPLICATE(' ', 15)
@ 04, 17 SAY REPLICATE(' ', 24)
@ 04, 59 SAY REPLICATE(' ', 15)
@ 05, 17 SAY REPLICATE(' ', 24)
RETURN
*
PROCEDURE carga
CREATE CURSOR pcta (codpro C (14),  ;
       descri C (40), cantid N (9,  ;
       2), preund N (12, 2),  ;
       preuns N (12, 2))
SET RELATION TO codpro INTO gc_pro00
RETURN
*
PROCEDURE detalle
ON KEY
SELECT pcta
DO cartec
ACTIVATE WINDOW a3
IF m.hve_codmon = 'DOL '
     BROWSE FIELDS codpro : 14 :H =  ;
            'CODIGO',  ;
            gc_pro00.pro_descri  ;
            :R :H = 'DESCRIPCION'  ;
            : 24, cantid :P =  ;
            '999,999' :R :H =  ;
            'CANTID.', preund :P =  ;
            '9,999,999.99' :H =  ;
            '      PRECIO', total =  ;
            cantid * preund :P =  ;
            '999,999,999.99' :R  ;
            :H = '        TOTAL'  ;
            FREEZE cantid NOEDIT  ;
            NOCLEAR SAVE IN a3
ELSE
     BROWSE FIELDS codpro : 14 :H =  ;
            'CODIGO',  ;
            gc_pro00.pro_descri  ;
            :R :H = 'DESCRIPCION'  ;
            : 24, cantid :P =  ;
            '999,999' :R :H =  ;
            'CANTID.', preuns :P =  ;
            '9,999,999.99' :H =  ;
            '      PRECIO', total =  ;
            cantid * preuns :P =  ;
            '999,999,999.99' :R  ;
            :H = '        TOTAL'  ;
            FREEZE cantid NOEDIT  ;
            NOCLEAR SAVE IN a3
ENDIF
IF w_f5 = 0
     DEACTIVATE WINDOW a3
ENDIF
DO p_footer WITH  ;
   '100010000000000001', 2
ACTIVATE WINDOW a0
CLEAR GETS
CLEAR READ
ON KEY
RETURN
*
PROCEDURE cartec
ON KEY
IF m = 0
     DO p_footer WITH  ;
        '101000000000000000001',  ;
        2
     ON KEY LABEL f3 do p_lee with 1
ELSE
     DO p_footer WITH  ;
        '111100000000101000001001',  ;
        2
     ON KEY LABEL f3 do p_lee with 1
     ON KEY LABEL f2 do crea 
     ON KEY LABEL f4 do p_borra 
     ON KEY LABEL f5 do modific
ENDIF
ON KEY LABEL enter do p_lee with 2
ON KEY LABEL ctrl+w ??
RETURN
*
FUNCTION p_lee
PARAMETER aux_opci
ON KEY
CLEAR GETS
w_sale = 0
w_paso = 1
IF m = 0 .AND. aux_opci = 2
     DO cartec
     RETURN
ENDIF
DO WHILE w_sale=0
     w_sale = 1
     ON KEY
     IF aux_opci = 2
          DEFINE WINDOW a4 FROM  ;
                 12 + ROW(), 02  ;
                 TO 12 + ROW(),  ;
                 77 IN screen  ;
                 NONE
          SELECT pcta
          w_recno = RECNO()
          w_codpro = pcta.codpro
          w_descri = pcta.descri
          w_cantid = pcta.cantid
          w_preund = pcta.preund
          w_preuns = pcta.preuns
          ACTIVATE WINDOW a4
          @ 00, 01 SAY w_codpro
          @ 00, 16 SAY  ;
            SUBSTR(w_descri, 1,  ;
            24)
          IF m.hve_codmon =  ;
             'DOL '
               @ 00, 49 SAY  ;
                 preund PICTURE  ;
                 '9,999,999.99'
               @ 00, 63 SAY  ;
                 cantid * preund  ;
                 PICTURE  ;
                 '99,999,999.99'
          ELSE
               @ 00, 49 SAY  ;
                 preuns PICTURE  ;
                 '9,999,999.99'
               @ 00, 63 SAY  ;
                 cantid * preuns  ;
                 PICTURE  ;
                 '99,999,999.99'
          ENDIF
     ELSE
          IF m < 7
               DEFINE WINDOW a4  ;
                      FROM 13 + m +  ;
                      1, 02 TO 13 +  ;
                      m + 1, 77  ;
                      IN screen  ;
                      NONE
          ELSE
               DEFINE WINDOW a4  ;
                      FROM 020,  ;
                      02 TO 020,  ;
                      77 IN  ;
                      screen  ;
                      NONE
          ENDIF
          w_codpro = SPACE(14)
          w_descri = SPACE(40)
          STORE 0 TO w_cantid,  ;
                w_preund,  ;
                w_preuns
          ACTIVATE WINDOW a4
          @ 00, 01 SAY  ;
            '              ³                        ³       ³         .00³           .00³'
          w_sale = 0
     ENDIF
     IF aux_opci = 1
          IF m.hve_tipdoc =  ;
             'FACT'
               IF m = 13
                    DO p_mensaje  ;
                       WITH  ;
                       'Grabe --- no Puede Ingresar Mas de 13 Productos'
                    DO cartec
                    DEACTIVATE WINDOW  ;
                               a4
                    ACTIVATE WINDOW  ;
                             a3
                    RETURN .F.
               ENDIF
          ELSE
               IF m = 14
                    DO p_mensaje  ;
                       WITH  ;
                       'Grabe --- no Puede Ingresar M s de 14 Productos'
                    DO cartec
                    DEACTIVATE WINDOW  ;
                               a4
                    ACTIVATE WINDOW  ;
                             a3
                    RETURN .F.
               ENDIF
          ENDIF
          @ 00, 01 GET w_codpro  ;
            PICTURE '@!' VALID  ;
            vali2a(1) WHEN  ;
            when2a(1)
     ENDIF
     @ 00, 41 GET w_cantid  ;
       PICTURE '999,999' VALID  ;
       vali2a(2) WHEN when2a(2)
     READ
     IF LASTKEY() <> 27
          IF w_preund = 0
               DO p_mensaje WITH  ;
                  'EL PRECIO NO PUEDE SER CERO'
               LOOP
          ENDIF
          IF m.hve_codmon =  ;
             'DOL '
               @ 00, 47 SAY  ;
                 w_preund PICTURE  ;
                 '999,999.99'
               @ 00, 66 SAY  ;
                 w_cantid *  ;
                 w_preund PICTURE  ;
                 '999,999,999.99'
          ELSE
               @ 00, 47 SAY  ;
                 w_preuns PICTURE  ;
                 '999,999.99'
               @ 00, 66 SAY  ;
                 w_cantid *  ;
                 w_preuns PICTURE  ;
                 '999,999,999.99'
          ENDIF
          IF aux_opci = 1
               m = m + 1
               SELECT pcta
               APPEND BLANK
               REPLACE codpro  ;
                       WITH  ;
                       w_codpro,  ;
                       descri  ;
                       WITH  ;
                       gc_pro00.pro_descri,  ;
                       cantid  ;
                       WITH  ;
                       w_cantid
               REPLACE preund  ;
                       WITH  ;
                       w_preund,  ;
                       preuns  ;
                       WITH  ;
                       w_preuns
               p = m
               DEACTIVATE WINDOW  ;
                          a4
               ACTIVATE WINDOW a3
               SKIP -7
               IF m.hve_codmon =  ;
                  'DOL '
                    BROWSE FIELDS  ;
                           codpro  ;
                           : 14  ;
                           :H =  ;
                           'CODIGO',  ;
                           gc_pro00.pro_descri  ;
                           :R :H =  ;
                           'DESCRIPCION'  ;
                           : 24,  ;
                           cantid  ;
                           :P =  ;
                           '999,999'  ;
                           :R :H =  ;
                           'CANTID.',  ;
                           preund  ;
                           :P =  ;
                           '9,999,999.99'  ;
                           :H =  ;
                           '      PRECIO',  ;
                           total =  ;
                           cantid *  ;
                           preund  ;
                           :P =  ;
                           '999,999,999.99'  ;
                           :R :H =  ;
                           '        TOTAL'  ;
                           FREEZE  ;
                           cantid  ;
                           NOEDIT  ;
                           NOWAIT  ;
                           SAVE  ;
                           IN a3
               ELSE
                    BROWSE FIELDS  ;
                           codpro  ;
                           : 14  ;
                           :H =  ;
                           'CODIGO',  ;
                           gc_pro00.pro_descri  ;
                           :R :H =  ;
                           'DESCRIPCION'  ;
                           : 24,  ;
                           cantid  ;
                           :P =  ;
                           '999,999'  ;
                           :R :H =  ;
                           'CANTID.',  ;
                           preuns  ;
                           :P =  ;
                           '9,999,999.99'  ;
                           :H =  ;
                           '      PRECIO',  ;
                           total =  ;
                           cantid *  ;
                           preuns  ;
                           :P =  ;
                           '999,999,999.99'  ;
                           :R :H =  ;
                           '        TOTAL'  ;
                           FREEZE  ;
                           cantid  ;
                           NOEDIT  ;
                           NOWAIT  ;
                           SAVE  ;
                           IN a3
               ENDIF
          ELSE
               SELECT pcta
               REPLACE cantid  ;
                       WITH  ;
                       w_cantid,  ;
                       preund  ;
                       WITH  ;
                       w_preund,  ;
                       preuns  ;
                       WITH  ;
                       w_preuns
               DEACTIVATE WINDOW  ;
                          a4
               w_sale = 1
          ENDIF
     ELSE
          w_sale = 1
     ENDIF
ENDDO
DO cartec
DEACTIVATE WINDOW a4
ACTIVATE WINDOW a3
RETURN
*
PROCEDURE pre_pro
PARAMETER opci
DO CASE
     CASE opci = 1
          SELECT gc_dlp00
          SEEK 'PUBL' + w_codpro
          IF FOUND()
               IF m.hve_tipdoc =  ;
                  'FACT'
                    w_preund = dlp_prsigv
                    w_preuns = ROUND(dlp_prsigv *  ;
                               restc,  ;
                               2)
               ELSE
                    w_preund = dlp_prcigv
                    w_preuns = ROUND(dlp_prcigv *  ;
                               restc,  ;
                               2)
               ENDIF
          ENDIF
     CASE opci = 2
          SELECT pcta
          w_recno = RECNO()
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SELECT gc_dlp00
               SEEK 'PUBL' +  ;
                    pcta.codpro
               IF FOUND()
                    IF m.hve_tipdoc =  ;
                       'FACT'
                         SELECT pcta
                         REPLACE preund  ;
                                 WITH  ;
                                 gc_dlp00.dlp_prsigv
                         REPLACE preuns  ;
                                 WITH  ;
                                 ROUND(gc_dlp00.dlp_prsigv *  ;
                                 restc,  ;
                                 2)
                    ELSE
                         SELECT pcta
                         REPLACE preund  ;
                                 WITH  ;
                                 gc_dlp00.dlp_prcigv
                         REPLACE preuns  ;
                                 WITH  ;
                                 ROUND(gc_dlp00.dlp_prcigv *  ;
                                 restc,  ;
                                 2)
                    ENDIF
               ENDIF
               SELECT pcta
          ENDSCAN
          GOTO w_recno
ENDCASE
SELECT pcta
RETURN
*
PROCEDURE when2a
PARAMETER w_opc2
DO CASE
     CASE w_opc2 = 1
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          w_campo = 'w_codpro'
          w_selec = SELECT()
          ON KEY LABEL f6 do produc2 with;
w_campo,w_selec,m.hve_almdes
     CASE w_opc2 = 2
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
ENDCASE
RETURN
*
FUNCTION vali2a
PARAMETER w_opc2
DO CASE
     CASE w_opc2 = 1
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .F.
          ENDIF
          SELECT gc_pro00
          SET ORDER TO codigo
          SEEK w_codpro
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'PRODUCTO NO EXISTE'
               RETURN .F.
          ELSE
               w_descri = gc_pro00.pro_descri
               @ 00, 16 SAY  ;
                 SUBSTR(w_descri,  ;
                 1, 24)
               IF pro_estope =  ;
                  'B'
                    DO p_mensaje  ;
                       WITH  ;
                       'PRODUCTO ESTA BLOQUEADO'
                    RETURN .F.
               ENDIF
               IF pro_rcom = 'H'
                    DO p_mensaje  ;
                       WITH  ;
                       'PRODUCTO TIENE REEMPLAZO'
                    RETURN .F.
               ENDIF
               IF pro_rcom = 'Y'
                    DO p_mensaje  ;
                       WITH  ;
                       'PRODUCTO ESTA FIN DE SERVICIO'
                    RETURN .F.
               ENDIF
               IF m > 0 .AND.  ;
                  aux_opci = 1
                    SELECT pcta
                    w_recno = RECNO()
                    GOTO TOP
                    LOCATE FOR  ;
                           codpro =  ;
                           w_codpro
                    IF FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'PRODUCTO YA FUE INGRESADO'
                         RETURN .F.
                    ENDIF
                    GOTO w_recno
               ENDIF
          ENDIF
     CASE w_opc2 = 2
          IF w_cantid < 0
               DO p_mensaje WITH  ;
                  'CANTIDAD NEGATIVA'
               RETURN .F.
          ELSE
               IF w_cantid = 0
                    DO p_mensaje  ;
                       WITH  ;
                       'FALTA PONER CANTIDAD'
                    RETURN .F.
               ENDIF
          ENDIF
          = pre_pro(1)
     CASE w_opc2 = 3
          IF w_porcen < 0 .OR.  ;
             w_porcen >= 100.01 
               DO p_mensaje WITH  ;
                  'RANGO INVALIDO'
               RETURN .F.
          ENDIF
          w_pagcta = ROUND((w_totnet *  ;
                     w_porcen) /  ;
                     100, 2)
          @ 03, 15 SAY w_totnet -  ;
            w_pagcta PICTURE  ;
            '99,999,999.99'
     CASE w_opc2 = 4
          IF w_pagcta <= 0
               DO p_mensaje WITH  ;
                  'FALTA PONER PAGO A CUENTA'
               RETURN .F.
          ENDIF
          IF w_pagcta > w_totnet
               DO p_mensaje WITH  ;
                  'PAGO A CUENTA MAYOR QUE EL TOTAL'
               RETURN .F.
          ENDIF
          w_porcen = ROUND((w_pagcta *  ;
                     100) /  ;
                     w_totnet,  ;
                     2)
          @ 01, 15 SAY w_porcen  ;
            PICTURE '999.99'  ;
            COLOR N/W 
          @ 03, 15 SAY w_totnet -  ;
            w_pagcta PICTURE  ;
            '99,999,999.99'
          DO p_footer WITH  ;
             '110000000000000000001',  ;
             2
          ACTIVATE WINDOW crea
          = INKEY(0, 'H')
          IF LASTKEY() = -1
               CLEAR READ
               DO graba
               DEACTIVATE WINDOW  ;
                          crea
               DEACTIVATE WINDOW  ;
                          a3
               ACTIVATE WINDOW a0
          ELSE
               IF LASTKEY() = 27
                    CLEAR READ
                    DEACTIVATE WINDOW  ;
                               crea
                    ACTIVATE WINDOW  ;
                             a3
               ELSE
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    ACTIVATE WINDOW  ;
                             crea
               ENDIF
          ENDIF
ENDCASE
RETURN
*
PROCEDURE modific
w_f5 = 1
KEYBOARD '{esc}'
RETURN
*
PROCEDURE p_borra
SELECT pcta
DELETE
m = m - 1
DO cartec
ACTIVATE WINDOW a3
IF m.hve_codmon = 'DOL '
     BROWSE FIELDS codpro : 14 :H =  ;
            'CODIGO',  ;
            gc_pro00.pro_descri  ;
            :R :H = 'DESCRIPCION'  ;
            : 24, cantid :P =  ;
            '999,999' :H =  ;
            'CANTID.', preund :P =  ;
            '9,999,999.99' :H =  ;
            '      PRECIO', total =  ;
            cantid * preund :P =  ;
            '999,999,999.99' :R  ;
            :H = '        TOTAL'  ;
            IN a3
ELSE
     BROWSE FIELDS codpro : 14 :H =  ;
            'CODIGO',  ;
            gc_pro00.pro_descri  ;
            :R :H = 'DESCRIPCION'  ;
            : 24, cantid :P =  ;
            '999,999' :H =  ;
            'CANTID.', preuns :P =  ;
            '9,999,999.99' :H =  ;
            '      PRECIO', total =  ;
            cantid * preuns :P =  ;
            '999,999,999.99' :R  ;
            :H = '        TOTAL'  ;
            IN a3
ENDIF
RETURN
*
PROCEDURE crea
ON KEY
STORE 0 TO w_tempo, w_presol,  ;
      imp_subtot, subtot
DEFINE WINDOW crea FROM 09, 20 TO  ;
       14, 63 IN screen
DO p_footer WITH  ;
   '100000000000000000001', 2
ACTIVATE WINDOW crea
@ 00, 00 SAY 'Total a Pagar:'
@ 01, 00 SAY 'A Cuenta    %:'
IF m.hve_codmon = 'DOL '
     w_simbo = sys_simomb
ELSE
     w_simbo = sys_simmon
ENDIF
@02,00 say "A Cuenta  &w_simbo:"
@03,00 say "Saldo     &w_simbo:"
SELECT pcta
w_recno = RECNO()
GOTO TOP
STORE 0 TO w_total, w_porcen,  ;
      w_pagcta
SCAN WHILE  .NOT. EOF()
     IF m.hve_codmon = 'DOL '
          w_total = w_total +  ;
                    (preund *  ;
                    cantid)
     ELSE
          w_total = w_total +  ;
                    (preuns *  ;
                    cantid)
     ENDIF
ENDSCAN
IF m.hve_tipdoc = 'FACT'
     w_totnet = ROUND(w_total *  ;
                w_facigv, 2)
ELSE
     w_totnet = w_total
ENDIF
@ 00, 15 SAY w_totnet PICTURE  ;
  '99,999,999.99'
@ 01, 15 GET w_porcen PICTURE  ;
  '999.99' VALID vali2a(3)
@ 02, 15 GET w_pagcta RANGE 0.01   ;
  PICTURE '99,999,999.99' VALID  ;
  vali2a(4)
READ CYCLE
IF LASTKEY() = 27
     GOTO w_recno
     DEACTIVATE WINDOW crea
     DO detalle
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
