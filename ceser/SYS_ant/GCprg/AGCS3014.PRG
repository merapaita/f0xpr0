*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 08, 77 IN screen  ;
       COLOR SCHEME 7
ACTIVATE WINDOW tablas
DO p_prestab WITH  ;
   'ADMINISTRACION',  ;
   'ACTUALIZACION MOV. DE SUCURSAL',  ;
   'TRASFERENCIA'
@ 2, 63 SAY PROGRAM()
STORE DATE() TO wrk_fecfin,  ;
      wrk_fecini
STORE SPACE(4) TO wrk_codalm
STORE SPACE(1) TO wrk_pro
ACTIVATE WINDOW cabecera
DO WHILE .T.
     USE GE_TAB0 ORDER CODIGO
     SEEK 'RUTA' + 'COMM'
     IF FOUND()
          wrk_ruta = tab_destab
     ELSE
          wrk_ruta = SPACE(30)
     ENDIF
     CLOSE DATABASES
     @ 00, 04 SAY  ;
       'Almac‚n de Origen  :'
     @ 01, 04 SAY  ;
       'Direct. de Comunic.:'
     @ 02, 04 SAY  ;
       'PROCESAR [S/N]     :'
     @ 00, 25 GET wrk_codalm  ;
       PICTURE '@!' VALID  ;
       oovalid() COLOR SCHEME 8
     @ 01, 25 GET wrk_ruta  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_ruta) COLOR  ;
       SCHEME 8
     @ 02, 25 GET wrk_pro PICTURE  ;
       '@!' VALID wrk_pro $ 'SN'  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF wrk_pro = 'N'
          LOOP
     ENDIF
     DO proceso
ENDDO
DEACTIVATE WINDOW cabecera,  ;
           tablas
RELEASE WINDOW cabecera
CLOSE DATABASES
ON KEY
RETURN
*
PROCEDURE proceso
wrk_ruta = ALLTRIM(wrk_ruta)
wrk_file = wrk_ruta + 'CES' +  ;
           wrk_codalm + '.ZIP'
IF !FILE("&wrk_file")
     DO P_MENSAJE WITH "ARCHIVO &wrk_file. NO EXISTE"
     RETURN
ENDIF
RUN PKUNZIP -o &wrk_file > NUL
= ooaviso('INVENTARIOS...')
@ 01, 23 SAY 'GC_ALM00.DBF'
IF FILE('C_ALM00.DBF')
     IF FILE('GC_ALM00.DBF')
          SELECT 1
          USE SHARED GC_ALM00  ;
              ORDER CODIGO
          SELECT 2
          USE C_ALM00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_alm00
               SEEK c_alm00.alm_codpro +  ;
                    c_alm00.alm_codalm
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_alm00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_ALM00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_CLI00.DBF'
IF FILE('C_CLI00.DBF')
     IF FILE('GC_CLI00.DBF')
          SELECT 1
          USE SHARED GC_CLI00  ;
              ORDER CODIGO
          SELECT 2
          USE C_CLI00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_cli00
               SEEK c_cli00.cli_tpper +  ;
                    c_cli00.cli_codigo
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_cli00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_CLI00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_PAR00.DBF'
IF FILE('C_PAR00.DBF')
     IF FILE('GC_PAR00.DBF')
          SELECT 1
          USE SHARED GC_PAR00
          SELECT 2
          USE C_PAR00
          SCATTER MEMVAR
          SELECT gc_par00
          GATHER MEMVAR
          rge_monbas = par_monbas
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_PAR00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_CMV00.DBF'
IF FILE('C_CMV00.DBF')
     IF FILE('GC_CMV00.DBF')
          SELECT 1
          USE SHARED GC_CMV00  ;
              ORDER CODIGO_1
          SELECT 2
          USE C_CMV00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_cmv00
               SEEK DTOC(c_cmv00.cmv_fechac) +  ;
                    c_cmv00.cmv_indica +  ;
                    c_cmv00.cmv_monref
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_cmv00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_CMV00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_DCO00.DBF'
IF FILE('C_DCO00.DBF')
     IF FILE('GC_DCO00.DBF')
          SELECT 1
          USE SHARED GC_DCO00  ;
              ORDER CODIGP
          SELECT 2
          USE C_DCO00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_dco00
               SEEK c_dco00.dco_nrodoc +  ;
                    c_dco00.dco_codprp
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_dco00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DCO00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_DIP00.DBF'
IF FILE('C_DIP00.DBF')
     IF FILE('GC_DIP00.DBF')
          SELECT 1
          USE SHARED GC_DIP00  ;
              ORDER CODIGO
          SELECT 2
          USE C_DIP00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_dip00
               SEEK c_dip00.dip_tipdoc +  ;
                    c_dip00.dip_nrodoc +  ;
                    c_dip00.dip_propar +  ;
                    c_dip00.dip_unimed
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_dip00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DIP00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_DVE00.DBF'
IF FILE('C_DVE00.DBF')
     IF FILE('GC_DVE00.DBF')
          SELECT 1
          USE SHARED GC_DVE00  ;
              ORDER CODIGO
          SELECT 2
          USE C_DVE00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_dve00
               SEEK c_dve00.dve_tipdoc +  ;
                    c_dve00.dve_nrodoc +  ;
                    c_dve00.dve_propar
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_dve00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DVE00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_DVT00.DBF'
IF FILE('C_DVT00.DBF')
     IF FILE('GC_DVT00.DBF')
          SELECT 1
          USE SHARED GC_DVT00  ;
              ORDER CODIGO
          SELECT 2
          USE C_DVT00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_dvt00
               SEEK c_dvt00.dvt_numdoc +  ;
                    c_dvt00.dvt_codpro +  ;
                    c_dvt00.dvt_unimed
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_dvt00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_DVT00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_EST00.DBF'
IF FILE('C_EST00.DBF')
     IF FILE('GC_EST00.DBF')
          SELECT 1
          USE SHARED GC_EST00  ;
              ORDER CODIGO
          SELECT 2
          USE C_EST00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_est00
               SEEK c_est00.est_indest +  ;
                    c_est00.est_tipdoc +  ;
                    c_est00.est_nrodoc +  ;
                    c_est00.est_codpro
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_est00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_EST00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_GAS00.DBF'
IF FILE('C_GAS00.DBF')
     IF FILE('GC_GAS00.DBF')
          SELECT 1
          USE SHARED GC_GAS00  ;
              ORDER CODIGO
          SELECT 2
          USE C_GAS00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_gas00
               SEEK c_gas00.gas_tipdoc +  ;
                    c_gas00.gas_nrodoc +  ;
                    c_gas00.gas_codgas
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_gas00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_GAS00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_HCO00.DBF'
IF FILE('C_HCO00.DBF')
     IF FILE('GC_HCO00.DBF')
          SELECT 1
          USE SHARED GC_HCO00  ;
              ORDER CODIGO
          SELECT 2
          USE C_HCO00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_hco00
               SEEK c_hco00.hco_nrodoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_hco00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_HCO00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_HIP00.DBF'
IF FILE('C_HIP00.DBF')
     IF FILE('GC_HIP00.DBF')
          SELECT 1
          USE SHARED GC_HIP00  ;
              ORDER CODIGO
          SELECT 2
          USE C_HIP00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_hip00
               SEEK c_hip00.hip_tipdoc +  ;
                    c_hip00.hip_nrodoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_hip00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_HIP00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_HVE00.DBF'
IF FILE('C_HVE00.DBF')
     IF FILE('GC_HVE00.DBF')
          SELECT 1
          USE SHARED GC_HVE00  ;
              ORDER CODIGO
          SELECT 2
          USE C_HVE00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_hve00
               SEEK c_hve00.hve_tipdoc +  ;
                    c_hve00.hve_nrodoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_hve00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_HVE00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_HVT00.DBF'
IF FILE('C_HVT00.DBF')
     IF FILE('GC_HVT00.DBF')
          SELECT 1
          USE SHARED GC_HVT00  ;
              ORDER CODIGO
          SELECT 2
          USE C_HVT00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_hvt00
               SEEK c_hvt00.hvt_numdoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_hvt00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_HVT00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 29 SAY 'GC_IMP00.DBF'
IF FILE('C_IMP00.DBF')
     IF FILE('GC_IMP00.DBF')
          SELECT 1
          USE SHARED GC_IMP00  ;
              ORDER IMP_TIPPRO
          SELECT 2
          USE C_IMP00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_imp00
               SEEK c_imp00.imp_tippro +  ;
                    c_imp00.imp_codent +  ;
                    c_imp00.imp_rango
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_imp00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_IMP00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_KAR00.DBF'
IF FILE('C_KAR00.DBF')
     IF FILE('GC_KAR00.DBF')
          SELECT 1
          USE SHARED GC_KAR00  ;
              ORDER KAR_PROING
          SELECT 2
          USE C_KAR00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_kar00
               SEEK c_kar00.kar_codpro +  ;
                    DTOS(c_kar00.kar_fecing) +  ;
                    c_kar00.kar_horing +  ;
                    c_kar00.kar_tipdoc +  ;
                    c_kar00.kar_nrodoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_kar00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_KAR00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_NFA00.DBF'
IF FILE('C_NFA00.DBF')
     IF FILE('GC_NFA00.DBF')
          SELECT 1
          USE SHARED GC_NFA00  ;
              ORDER NFA_NUMFAC
          SELECT 2
          USE C_NFA00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_nfa00
               SEEK c_nfa00.nfa_numfac +  ;
                    c_nfa00.nfa_nrodoc +  ;
                    c_nfa00.nfa_codpro
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_nfa00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_NFA00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_NVE00.DBF'
IF FILE('C_NVE00.DBF')
     IF FILE('GC_NVE00.DBF')
          SELECT 1
          USE SHARED GC_NVE00  ;
              ORDER CODIGO
          SELECT 2
          USE C_NVE00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_nve00
               SEEK c_nve00.nve_codpro
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_nve00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_NVE00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_ORD00.DBF'
IF FILE('C_ORD00.DBF')
     IF FILE('GC_ORD00.DBF')
          SELECT 1
          USE SHARED GC_ORD00  ;
              ORDER ORD_NRODOC
          SELECT 2
          USE C_ORD00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_ord00
               SEEK c_ord00.ord_nrodoc +  ;
                    c_ord00.ord_codpro
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_ord00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_ORD00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_TPE00.DBF'
IF FILE('C_TPE00.DBF')
     IF FILE('GC_TPE00.DBF')
          SELECT 1
          USE SHARED GC_TPE00  ;
              ORDER TPE_KEY
          SELECT 2
          USE C_TPE00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_tpe00
               SEEK c_tpe00.tpe_indreg +  ;
                    c_tpe00.tpe_indori +  ;
                    c_tpe00.tpe_codpro +  ;
                    DTOC(c_tpe00.tpe_feclle)
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_tpe00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_TPE00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_VND00.DBF'
IF FILE('C_VND00.DBF')
     IF FILE('GC_VND00.DBF')
          SELECT 1
          USE SHARED GC_VND00  ;
              ORDER CODIGO
          SELECT 2
          USE C_VND00
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT gc_vnd00
               SEEK c_vnd00.vnd_tpper +  ;
                    c_vnd00.vnd_code
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT c_vnd00
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GC_VND00 NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'GC_TAB0.DBF'
IF FILE('E_TAB0.DBF')
     IF FILE('GE_TAB0.DBF')
          SELECT 1
          USE SHARED GE_TAB0  ;
              ORDER CODIGO
          SELECT 2
          USE E_TAB0
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT ge_tab0
               SEEK e_tab0.tab_codpre +  ;
                    e_tab0.tab_codtab
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT e_tab0
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO GE_TAB0 NO  EXISTE'
     ENDIF
ENDIF
= ooaviso('REPARACIONES...')
@ 01, 23 SAY 'ST_ASIEM.DBF'
IF FILE('T_ASIEM.DBF')
     IF FILE('ST_ASIEM.DBF')
          SELECT 1
          USE ST_ASIEM ORDER  ;
              ASI_COD
          SELECT 2
          USE T_ASIEM
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_asiem
               SEEK t_asiem.codemp +  ;
                    t_asiem.anio
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_asiem
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ICLPR NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ESTAD.DBF'
IF FILE('T_ESTAD.DBF')
     IF FILE('ST_ESTAD.DBF')
          SELECT 1
          USE SHARED ST_ESTAD  ;
              ORDER EST_NUMORD
          SELECT 2
          USE T_ESTAD
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_estad
               SEEK t_estad.numord
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_estad
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ESTAD NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ESTFA.DBF'
IF FILE('T_ESTFA.DBF')
     IF FILE('ST_ESTFA.DBF')
          SELECT 1
          USE SHARED ST_ESTFA  ;
              ORDER CODIGO
          SELECT 2
          USE T_ESTFA
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_estfa
               SEEK t_estfa.numord
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_estfa
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ESTFA NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ICLPR.DBF'
IF FILE('T_ICLPR.DBF')
     IF FILE('ST_ICLPR.DBF')
          SELECT 1
          USE SHARED ST_ICLPR  ;
              ORDER CODIGO
          SELECT 2
          USE T_ICLPR
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_iclpr
               SEEK t_iclpr.indent +  ;
                    t_iclpr.codent
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_iclpr
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ICLPR NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IDPED.DBF'
IF FILE('T_IDPED.DBF')
     IF FILE('ST_IDPED.DBF')
          SELECT 1
          USE SHARED ST_IDPED  ;
              ORDER CODIGO
          SELECT 2
          USE T_IDPED
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_idped
               SEEK t_idped.numdoc +  ;
                    t_idped.numord +  ;
                    t_idped.codpro
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_idped
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IDPED NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IDPRE.DBF'
IF FILE('T_IDPRE.DBF')
     IF FILE('ST_IDPRE.DBF')
          SELECT 1
          USE SHARED ST_IDPRE  ;
              ORDER CODIGO
          SELECT 2
          USE T_IDPRE
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_idpre
               SEEK t_idpre.numdoc +  ;
                    t_idpre.numord +  ;
                    t_idpre.codpro
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_idpre
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IDPRE NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IMODE.DBF'
IF FILE('T_IMODE.DBF')
     IF FILE('ST_IMODE.DBF')
          SELECT 1
          USE SHARED ST_IMODE  ;
              ORDER CODIGO
          SELECT 2
          USE T_IMODE
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_imode
               SEEK t_imode.codmar +  ;
                    t_imode.codmod
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_imode
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IMODE NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_MOVCA.DBF'
IF FILE('T_MOVCA.DBF')
     IF FILE('ST_MOVCA.DBF')
          SELECT t_movca
          USE ST_MOVCA ORDER  ;
              CODIGO
          SELECT 2
          USE T_MOVCA
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_movca
               SEEK t_movca.numord +  ;
                    t_movca.codcau
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_movca
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_MOVCA NO EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_MOVSO.DBF'
IF FILE('T_MOVSO.DBF')
     IF FILE('ST_MOVSO.DBF')
          SELECT t_movso
          USE ST_MOVSO ORDER  ;
              CODIGO
          SELECT 2
          USE T_MOVSO
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_movso
               SEEK t_movso.numord +  ;
                    t_movso.codcau
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_movso
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_MOVSO NO EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IOREP.DBF'
IF FILE('T_IOREP.DBF')
     IF FILE('ST_IOREP.DBF')
          SELECT 1
          USE SHARED ST_IOREP  ;
              ORDER CODIGO
          SELECT 2
          USE T_IOREP
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_iorep
               SEEK t_iorep.numdoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_iorep
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IOREP NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IPARG.DBF'
IF FILE('T_IPARG.DBF')
     IF FILE('ST_IPARG.DBF')
          SELECT 1
          USE SHARED ST_IPARG
          SELECT 2
          USE T_IPARG
          SCATTER MEMVAR
          SELECT st_iparg
          GATHER MEMVAR
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IPARG NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IPREP.DBF'
IF FILE('T_IPREP.DBF')
     IF FILE('ST_IPREP.DBF')
          SELECT 1
          USE SHARED ST_IPREP  ;
              ORDER CODIGO
          SELECT 2
          USE T_IPREP
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_iprep
               SEEK t_iprep.numdoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_iprep
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IPREP NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_IREDO.DBF'
IF FILE('T_IREDO.DBF')
     IF FILE('ST_IREDO.DBF')
          SELECT 1
          USE SHARED ST_IREDO  ;
              ORDER IRE_INDNUM
          SELECT 2
          USE T_IREDO
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_iredo
               SEEK t_iredo.indodo +  ;
                    t_iredo.numodo +  ;
                    t_iredo.indddo +  ;
                    t_iredo.numddo
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_iredo
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_IREDO NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ISCIC.DBF'
IF FILE('T_ISCIC.DBF')
     IF FILE('ST_ISCIC.DBF')
          SELECT 1
          USE SHARED ST_ISCIC  ;
              ORDER FECSOL
          SELECT 2
          USE T_ISCIC
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_iscic
               SEEK t_iscic.numsol +  ;
                    DTOS(t_iscic.fecini)
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_iscic
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ISCIC NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ISERI.DBF'
IF FILE('T_ISERI.DBF')
     IF FILE('ST_ISERI.DBF')
          SELECT 1
          USE SHARED ST_ISERI  ;
              ORDER SER_CODMAR
          SELECT 2
          USE T_ISERI
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_iseri
               SEEK t_iseri.codmar +  ;
                    t_iseri.modelo +  ;
                    t_iseri.numser
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_iseri
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ISERI NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ISPRE.DBF'
IF FILE('T_ISPRE.DBF')
     IF FILE('ST_ISPRE.DBF')
          SELECT 1
          USE SHARED ST_ISPRE  ;
              ORDER CODIGO
          SELECT 2
          USE T_ISPRE
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_ispre
               SEEK t_ispre.numdoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_ispre
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ISPRE NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ISREP.DBF'
IF FILE('T_ISREP.DBF')
     IF FILE('ST_ISREP.DBF')
          SELECT 1
          USE SHARED ST_ISREP  ;
              ORDER CODIGO
          SELECT 2
          USE T_ISREP
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_isrep
               SEEK t_isrep.numdoc
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_isrep
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ISREP NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_ITECN.DBF'
IF FILE('T_ITECN.DBF')
     IF FILE('ST_ITECN.DBF')
          SELECT 1
          USE SHARED ST_ITECN  ;
              ORDER CODIGO
          SELECT 2
          USE T_ITECN
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_itecn
               SEEK t_itecn.codent
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_itecn
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_ITECN NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_MOBRA.DBF'
IF FILE('T_MOBRA.DBF')
     IF FILE('ST_MOBRA.DBF')
          SELECT 1
          USE SHARED ST_MOBRA  ;
              ORDER CODIGO
          SELECT 2
          USE T_MOBRA
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_mobra
               SEEK t_mobra.mo_codmar +  ;
                    t_mobra.mo_codart
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_mobra
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_MOBRA NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_MVORD.DBF'
IF FILE('T_MVORD.DBF')
     IF FILE('ST_MVORD.DBF')
          SELECT 1
          USE SHARED ST_MVORD  ;
              ORDER ORDIA
          SELECT 2
          USE T_MVORD
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_mvord
               SEEK t_mvord.orden +  ;
                    t_mvord.estado +  ;
                    DTOC(t_mvord.dia) +  ;
                    t_mvord.hora
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_mvord
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_MVORD NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_SICLI.DBF'
IF FILE('T_SICLI.DBF')
     IF FILE('ST_SICLI.DBF')
          SELECT 1
          USE SHARED ST_SICLI  ;
              ORDER CODIGO
          SELECT 2
          USE T_SICLI
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_sicli
               SEEK t_sicli.numdoc +  ;
                    t_sicli.codsin
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_sicli
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_SICLI NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_SINT.DBF'
IF FILE('T_SINT.DBF')
     IF FILE('ST_SINT.DBF')
          SELECT 1
          USE SHARED ST_SINT  ;
              ORDER CODIGO
          SELECT 2
          USE T_SINT
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_sint
               SEEK t_sint.codsin
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_sint
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_SINT NO  EXISTE'
     ENDIF
ENDIF
@ 01, 23 SAY 'ST_USERS.DBF'
IF FILE('T_USERS.DBF')
     IF FILE('ST_USERS.DBF')
          SELECT 1
          USE SHARED ST_USERS  ;
              ORDER CODIGO
          SELECT 2
          USE T_USERS
          GOTO TOP
          SCAN WHILE  .NOT. EOF()
               SCATTER MEMVAR
               SELECT st_users
               SEEK t_users.codemp +  ;
                    t_users.estado
               IF FOUND()
                    GATHER MEMVAR
               ELSE
                    APPEND BLANK
                    GATHER MEMVAR
               ENDIF
               SELECT t_users
          ENDSCAN
          CLOSE DATABASES
     ELSE
          DO p_mensaje WITH  ;
             'ARCHIVO ST_USERS NO  EXISTE'
     ENDIF
ENDIF
RUN ERASE C_*.DBF > NUL
RUN ERASE T_*.DBF > NUL
RUN ERASE E_*.DBF > NUL
RUN ERASE T_*.FPT > NUL
DEACTIVATE WINDOW winmensaje
RETURN
*
FUNCTION oovalid
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF EMPTY(wrk_codalm)
     DO p_mensaje WITH  ;
        'No se Permiten Blancos'
     RETURN .F.
ENDIF
IF rge_codalm = wrk_codalm
     DO p_mensaje WITH  ;
        'Almac‚n es igual al de Trabajo'
     RETURN .F.
ENDIF
USE GE_TAB0 ORDER CODIGO
SEEK 'ALMA' + wrk_codalm
IF FOUND()
     @ 00, 30 SAY tab_destab
     RETURN
ELSE
     DO p_mensaje WITH  ;
        'C¢digo de Almac‚n No Existe'
     RETURN .F.
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
