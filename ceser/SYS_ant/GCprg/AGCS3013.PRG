*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 IN screen  ;
       COLOR SCHEME 7
ACTIVATE WINDOW tablas
DO p_prestab WITH  ;
   'ADMINISTRACION',  ;
   'GENERACION MOV. SUCURSAL',  ;
   'TRASFERENCIA'
@ 2, 63 SAY PROGRAM()
STORE DATE() TO wrk_fecfin,  ;
      wrk_fecini
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
       'Peri¢do Inicial    :'
     @ 01, 04 SAY  ;
       'Peri¢do Final      :'
     @ 02, 04 SAY  ;
       'Direct. de Comunic.:'
     @ 03, 04 SAY  ;
       'PROCESAR [S/N]     :'
     @ 00, 25 GET wrk_fecini  ;
       PICTURE '@D' COLOR SCHEME  ;
       8
     @ 01, 25 GET wrk_fecfin  ;
       PICTURE '@D' VALID  ;
       wrk_fecfin >= wrk_fecini  ;
       COLOR SCHEME 8
     @ 02, 25 GET wrk_ruta  ;
       PICTURE '@!' VALID  .NOT.  ;
       EMPTY(wrk_ruta) COLOR  ;
       SCHEME 8
     @ 03, 25 GET wrk_pro PICTURE  ;
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
= ooaviso('INVENTARIOS...')
@ 01, 23 SAY 'GC_ALM00.DBF'
IF FILE('GC_ALM00.DBF')
     USE SHARED GC_ALM00 ORDER 1
     COPY TO C_ALM00 STRUCTURE
     USE C_ALM00
     APPEND FROM GC_ALM00 FOR  ;
            alm_fecha >=  ;
            wrk_fecini .AND.  ;
            alm_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_ALM00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_CLI00.DBF'
IF FILE('GC_CLI00.DBF')
     USE SHARED GC_CLI00 ORDER 1
     COPY TO C_CLI00 STRUCTURE
     USE C_CLI00
     APPEND FROM GC_CLI00 FOR  ;
            cli_fecha >=  ;
            wrk_fecini .AND.  ;
            cli_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_CLI00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_PAR00.DBF'
IF FILE('GC_PAR00.DBF')
     USE SHARED GC_PAR00
     COPY TO C_PAR00 STRUCTURE
     USE C_PAR00
     APPEND FROM GC_PAR00
     rge_monbas = par_monbas
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_PAR00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_CMV00.DBF'
IF FILE('GC_CMV00.DBF')
     USE SHARED GC_CMV00 ORDER 1
     COPY TO C_CMV00 STRUCTURE
     USE C_CMV00
     APPEND FROM GC_CMV00 FOR  ;
            cmv_fecha >=  ;
            wrk_fecini .AND.  ;
            cmv_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_CMV00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_DCO00.DBF'
IF FILE('GC_DCO00.DBF')
     USE SHARED GC_DCO00 ORDER 1
     COPY TO C_DCO00 STRUCTURE
     USE C_DCO00
     APPEND FROM GC_DCO00 FOR  ;
            dco_fecha >=  ;
            wrk_fecini .AND.  ;
            dco_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_DCO00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_DIP00.DBF'
IF FILE('GC_DIP00.DBF')
     USE SHARED GC_DIP00 ORDER 1
     COPY TO C_DIP00 STRUCTURE
     USE C_DIP00
     APPEND FROM GC_DIP00 FOR  ;
            dip_fecha >=  ;
            wrk_fecini .AND.  ;
            dip_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_DIP00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_DVE00.DBF'
IF FILE('GC_DVE00.DBF')
     USE SHARED GC_DVE00 ORDER 1
     COPY TO C_DVE00 STRUCTURE
     USE C_DVE00
     APPEND FROM GC_DVE00 FOR  ;
            dve_fecha >=  ;
            wrk_fecini .AND.  ;
            dve_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_DVE00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_DVT00.DBF'
IF FILE('GC_DVT00.DBF')
     USE SHARED GC_DVT00 ORDER 1
     COPY TO C_DVT00 STRUCTURE
     USE C_DVT00
     APPEND FROM GC_DVT00 FOR  ;
            dvt_fecha >=  ;
            wrk_fecini .AND.  ;
            dvt_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_DVT00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_EST00.DBF'
IF FILE('GC_EST00.DBF')
     USE SHARED GC_EST00 ORDER 1
     COPY TO C_EST00 STRUCTURE
     USE C_EST00
     APPEND FROM GC_EST00 FOR  ;
            est_fecha >=  ;
            wrk_fecini .AND.  ;
            est_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_EST00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_GAS00.DBF'
IF FILE('GC_GAS00.DBF')
     USE SHARED GC_GAS00 ORDER 1
     COPY TO C_GAS00 STRUCTURE
     USE C_GAS00
     APPEND FROM GC_GAS00 FOR  ;
            gas_fecha >=  ;
            wrk_fecini .AND.  ;
            gas_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_GAS00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_HCO00.DBF'
IF FILE('GC_HCO00.DBF')
     USE SHARED GC_HCO00 ORDER 1
     COPY TO C_HCO00 STRUCTURE
     USE C_HCO00
     APPEND FROM GC_HCO00 FOR  ;
            hco_fecha >=  ;
            wrk_fecini .AND.  ;
            hco_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_HCO00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_HIP00.DBF'
IF FILE('GC_HIP00.DBF')
     USE SHARED GC_HIP00 ORDER 1
     COPY TO C_HIP00 STRUCTURE
     USE C_HIP00
     APPEND FROM GC_HIP00 FOR  ;
            hip_fecha >=  ;
            wrk_fecini .AND.  ;
            hip_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_HIP00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_HVE00.DBF'
IF FILE('GC_HVE00.DBF')
     USE SHARED GC_HVE00 ORDER 1
     COPY TO C_HVE00 STRUCTURE
     USE C_HVE00
     APPEND FROM GC_HVE00 FOR  ;
            hve_fecha >=  ;
            wrk_fecini .AND.  ;
            hve_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_HVE00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_HVT00.DBF'
IF FILE('GC_HVT00.DBF')
     USE SHARED GC_HVT00 ORDER 1
     COPY TO C_HVT00 STRUCTURE
     USE C_HVT00
     APPEND FROM GC_HVT00 FOR  ;
            hvt_fecha >=  ;
            wrk_fecini .AND.  ;
            hvt_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_HVT00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_IMP00.DBF'
IF FILE('GC_IMP00.DBF')
     USE SHARED GC_IMP00 ORDER 1
     COPY TO C_IMP00 STRUCTURE
     USE C_IMP00
     APPEND FROM GC_IMP00 FOR  ;
            imp_fecha >=  ;
            wrk_fecini .AND.  ;
            imp_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_IMP00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_KAR00.DBF'
IF FILE('GC_KAR00.DBF')
     USE SHARED GC_KAR00 ORDER 1
     COPY TO C_KAR00 STRUCTURE
     USE C_KAR00
     APPEND FROM GC_KAR00 FOR  ;
            kar_fecha >=  ;
            wrk_fecini .AND.  ;
            kar_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_KAR00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_NFA00.DBF'
IF FILE('GC_NFA00.DBF')
     USE SHARED GC_NFA00 ORDER 1
     COPY TO C_NFA00 STRUCTURE
     USE C_NFA00
     APPEND FROM GC_NFA00 FOR  ;
            nfa_fecha >=  ;
            wrk_fecini .AND.  ;
            nfa_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_NFA00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_NVE00.DBF'
IF FILE('GC_NVE00.DBF')
     USE SHARED GC_NVE00 ORDER 1
     COPY TO C_NVE00 STRUCTURE
     USE C_NVE00
     APPEND FROM GC_NVE00 FOR  ;
            nve_fecha >=  ;
            wrk_fecini .AND.  ;
            nve_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_NVE00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_ORD00.DBF'
IF FILE('GC_ORD00.DBF')
     USE SHARED GC_ORD00 ORDER 1
     COPY TO C_ORD00 STRUCTURE
     USE C_ORD00
     APPEND FROM GC_ORD00 FOR  ;
            ord_fecha >=  ;
            wrk_fecini .AND.  ;
            ord_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_ORD00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_TPE00.DBF'
IF FILE('GC_TPE00.DBF')
     USE SHARED GC_TPE00 ORDER 1
     COPY TO C_TPE00 STRUCTURE
     USE C_TPE00
     APPEND FROM GC_TPE00 FOR  ;
            tpe_fecha >=  ;
            wrk_fecini .AND.  ;
            tpe_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_TPE00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_VND00.DBF'
IF FILE('GC_VND00.DBF')
     USE SHARED GC_VND00 ORDER 1
     COPY TO C_VND00 STRUCTURE
     USE C_VND00
     APPEND FROM GC_VND00 FOR  ;
            vnd_fecha >=  ;
            wrk_fecini .AND.  ;
            vnd_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GC_VND00 NO EXISTE'
ENDIF
@ 01, 23 SAY 'GC_TAB0.DBF'
IF FILE('GE_TAB0.DBF')
     USE SHARED GE_TAB0 ORDER 1
     COPY TO E_TAB0 STRUCTURE
     USE E_TAB0
     APPEND FROM GE_TAB0 FOR  ;
            tab_fecha >=  ;
            wrk_fecini .AND.  ;
            tab_fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO GE_TAB0 NO EXISTE'
ENDIF
CLOSE DATABASES
= ooaviso('REPARACIONES..')
@ 01, 23 SAY 'ST_ASIEM.DBF'
IF FILE('ST_ASIEM.DBF')
     USE SHARED ST_ASIEM ORDER 1
     COPY TO T_ASIEM STRUCTURE
     USE T_ASIEM
     APPEND FROM ST_ASIEM FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ASIEM NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ESTAD.DBF'
IF FILE('ST_ESTAD.DBF')
     USE SHARED ST_ESTAD ORDER 1
     COPY TO T_ESTAD STRUCTURE
     USE T_ESTAD
     APPEND FROM ST_ESTAD FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ESTAD NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ESTFA.DBF'
IF FILE('ST_ESTFA.DBF')
     USE SHARED ST_ESTFA ORDER 1
     COPY TO T_ESTFA STRUCTURE
     USE T_ESTFA
     APPEND FROM ST_ESTFA FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ESTFA NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ICLPR.DBF'
IF FILE('ST_ICLPR.DBF')
     USE SHARED ST_ICLPR ORDER 1
     COPY TO T_ICLPR STRUCTURE
     USE T_ICLPR
     APPEND FROM ST_ICLPR FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ICLPR NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IDPED.DBF'
IF FILE('ST_IDPED.DBF')
     USE SHARED ST_IDPED ORDER 1
     COPY TO T_IDPED STRUCTURE
     USE T_IDPED
     APPEND FROM ST_IDPED FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IDPED NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IDPRE.DBF'
IF FILE('ST_IDPRE.DBF')
     USE SHARED ST_IDPRE ORDER 1
     COPY TO T_IDPRE STRUCTURE
     USE T_IDPRE
     APPEND FROM ST_IDPRE FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IDPRE NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IMODE.DBF'
IF FILE('ST_IMODE.DBF')
     USE SHARED ST_IMODE ORDER 1
     COPY TO T_IMODE STRUCTURE
     USE T_IMODE
     APPEND FROM ST_IMODE FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IMODE NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_MOVCA.DBF'
IF FILE('ST_MOVCA.DBF')
     USE ST_MOVCA ORDER 1
     COPY TO T_MOVCA STRUCTURE
     USE T_MOVCA
     APPEND FROM ST_MOVCA FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_MOVCA NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_MOVSO.DBF'
IF FILE('ST_MOVSO.DBF')
     USE ST_MOVSO ORDER 1
     COPY TO T_MOVSO STRUCTURE
     USE T_MOVSO
     APPEND FROM ST_MOVSO FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_MOVSO NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IOREP.DBF'
IF FILE('ST_IOREP.DBF')
     USE SHARED ST_IOREP ORDER 1
     COPY TO T_IOREP STRUCTURE
     USE T_IOREP
     APPEND FROM ST_IOREP FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IOREP NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IPARG.DBF'
IF FILE('ST_IPARG.DBF')
     USE SHARED ST_IPARG
     COPY TO T_IPARG STRUCTURE
     USE T_IPARG
     APPEND FROM ST_IPARG
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IPARG NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IPREP.DBF'
IF FILE('ST_IPREP.DBF')
     USE SHARED ST_IPREP ORDER 1
     COPY TO T_IPREP STRUCTURE
     USE T_IPREP
     APPEND FROM ST_IPREP FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IPREP NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_IREDO.DBF'
IF FILE('ST_IREDO.DBF')
     USE SHARED ST_IREDO ORDER 1
     COPY TO T_IREDO STRUCTURE
     USE T_IREDO
     APPEND FROM ST_IREDO FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_IREDO NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ISCIC.DBF'
IF FILE('ST_ISCIC.DBF')
     USE SHARED ST_ISCIC ORDER 1
     COPY TO T_ISCIC STRUCTURE
     USE T_ISCIC
     APPEND FROM ST_ISCIC FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ISCIC NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ISERI.DBF'
IF FILE('ST_ISERI.DBF')
     USE SHARED ST_ISERI ORDER 1
     COPY TO T_ISERI STRUCTURE
     USE T_ISERI
     APPEND FROM ST_ISERI FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ISERI NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ISPRE.DBF'
IF FILE('ST_ISPRE.DBF')
     USE SHARED ST_ISPRE ORDER 1
     COPY TO T_ISPRE STRUCTURE
     USE T_ISPRE
     APPEND FROM ST_ISPRE FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ISPRE NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ISREP.DBF'
IF FILE('ST_ISREP.DBF')
     USE SHARED ST_ISREP ORDER 1
     COPY TO T_ISREP STRUCTURE
     USE T_ISREP
     APPEND FROM ST_ISREP FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ISREP NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_ITECN.DBF'
IF FILE('ST_ITECN.DBF')
     USE SHARED ST_ITECN ORDER 1
     COPY TO T_ITECN STRUCTURE
     USE T_ITECN
     APPEND FROM ST_ITECN FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_ITECN NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_MOBRA.DBF'
IF FILE('ST_MOBRA.DBF')
     USE SHARED ST_MOBRA ORDER 1
     COPY TO T_MOBRA STRUCTURE
     USE T_MOBRA
     APPEND FROM ST_MOBRA FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_MOBRA NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_MVORD.DBF'
IF FILE('ST_MVORD.DBF')
     USE SHARED ST_MVORD ORDER 1
     COPY TO T_MVORD STRUCTURE
     USE T_MVORD
     APPEND FROM ST_MVORD FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_MVORD NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_SICLI.DBF'
IF FILE('ST_SICLI.DBF')
     USE SHARED ST_SICLI ORDER 1
     COPY TO T_SICLI STRUCTURE
     USE T_SICLI
     APPEND FROM ST_SICLI FOR  ;
            date >= wrk_fecini  ;
            .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_SICLI NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_SINT.DBF'
IF FILE('ST_SINT.DBF')
     USE SHARED ST_SINT ORDER 1
     COPY TO T_SINT STRUCTURE
     USE T_SINT
     APPEND FROM ST_SINT FOR date >=  ;
            wrk_fecini .AND. date <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_SINT NO EXISTE'
ENDIF
@ 01, 23 SAY 'ST_USERS.DBF'
IF FILE('ST_USERS.DBF')
     USE SHARED ST_USERS ORDER 1
     COPY TO T_USERS STRUCTURE
     USE T_USERS
     APPEND FROM ST_USERS FOR  ;
            fecha >= wrk_fecini  ;
            .AND. fecha <=  ;
            wrk_fecfin
ELSE
     DO p_mensaje WITH  ;
        'ARCHIVO ST_USERS NO EXISTE'
ENDIF
CLOSE DATABASES
= ooaviso( ;
  'EMPAQUETANDO ARCHIVOS...')
wrk_ruta = ALLTRIM(wrk_ruta)
wrk_file = wrk_ruta + 'CES' +  ;
           rge_codalm + '.ZIP'
!PKZIP -exo &wrk_file  *.DBF *.FPT;
 > NUL
RUN ERASE C*.DBF
RUN ERASE T*.DBF
RUN ERASE E*.DBF
RUN ERASE T*.FPT
RELEASE WINDOW winmensaje
*
*** 
*** ReFox - retrace your steps ... 
***
