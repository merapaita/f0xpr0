*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DO WHILE .T.
     rpt = f_yesno( ;
           'SEGURO DE CONTINUAR...?' ;
           )
     IF  .NOT. rpt
          RETURN
     ELSE
          EXIT
     ENDIF
ENDDO
ON KEY
DEFINE WINDOW temporal FROM 14,  ;
       05 TO 20, 66 IN screen  ;
       NONE COLOR SCHEME 19
ACTIVATE WINDOW temporal
dirdf = SYS(5) + SYS(2003)
@ 00, 00 SAY  ;
  '…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª '
@ 01, 00 SAY  ;
  '∫  Archivo :                                                 ∫ '
@ 02, 00 SAY  ;
  '∫  Descripci¢n :                                             ∫ '
@ 03, 00 SAY  ;
  '∫  ⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø  ∫ '
@ 04, 00 SAY  ;
  '∫  ≥                                                      ≥  ∫ '
@ 05, 00 SAY  ;
  '∫  ¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ  ∫ '
@ 06, 00 SAY  ;
  '»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº '
abrir = .T.
DO abre WITH 'GE_TAB0.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GE_TAB0.DBF    '
     @ 2, 17 SAY  ;
       'Archivo de Tablas del Sistema     '
     DO caso1
     @ 4, 4 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
RETURN
DO abre WITH 'GC_PRO00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_PRO00.DBF    '
     @ 2, 17 SAY  ;
       'Archivo de Productos              '
     DO caso2
     @ 4, 6 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HLP00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HLP00.DBF    '
     @ 2, 17 SAY  ;
       'Cabecera de Lista de Precios      '
     DO caso3
     @ 4, 8 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_DLP00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_DLP00.DBF    '
     @ 2, 17 SAY  ;
       'Detalle de Lista de Precios       '
     DO caso4
     @ 4, 10 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_PAL00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_PAL00.DBF       '
     @ 2, 17 SAY  ;
       'Productos Alternativos            '
     DO caso5
     @ 4, 12 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_CLI00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_CLI00.DBF    '
     @ 2, 17 SAY  ;
       'Archivo de Clientes               '
     DO caso6
     @ 4, 14 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_UNI00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_UNI00.DBF    '
     @ 2, 17 SAY  ;
       'Unidades Alternativas             '
     DO caso7
     @ 4, 16 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_CMV00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_CMV00.DBF    '
     @ 2, 17 SAY  ;
       'Archivo de Tipo de Cambio         '
     DO caso8
     @ 4, 18 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_ALM00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_ALM00.DBF    '
     @ 2, 17 SAY  ;
       'Almacenes o Bodegas               '
     DO caso9
     @ 4, 20 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_VND00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_VND00.DBF    '
     @ 2, 17 SAY  ;
       'Archivo de Vendedores             '
     DO caso10
     @ 4, 22 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HIP00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HIP00.DBF    '
     @ 2, 17 SAY  ;
       'Cabecera de Productos             '
     DO caso11
     @ 4, 24 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_DIP00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_DIP00.DBF    '
     @ 2, 17 SAY  ;
       'Detalle de Productos              '
     DO caso12
     @ 4, 26 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_GAS00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_GAS00.DBF    '
     @ 2, 17 SAY  ;
       'Gastos                            '
     DO caso13
     @ 4, 28 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
= ooclose('PASSWORD')
DO abre WITH 'PASSWORD.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'PASSWORD.DBF    '
     @ 2, 17 SAY  ;
       'Archivo de Usuarios del Sistema   '
     DO caso14
     @ 4, 30 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HCO00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HCO00.DBF    '
     @ 2, 17 SAY  ;
       'Cabecera de Pedidos               '
     DO caso15
     @ 4, 32 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_DCO00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_DCO00.DBF    '
     @ 2, 17 SAY  ;
       'Detalle de Pedidos                '
     DO caso16
     @ 4, 35 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_KAR00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_KAR00.DBF    '
     @ 2, 17 SAY  ;
       'K A R D E X                       '
     DO caso17
     @ 4, 36 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HVE00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HVE00.DBF    '
     @ 2, 17 SAY  ;
       'Cabecera de Ventas                '
     DO caso18
     @ 4, 38 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_DVE00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_DVE00.DBF    '
     @ 2, 17 SAY  ;
       'Detalle de Ventas                 '
     DO caso19
     @ 4, 40 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HVT00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HVT00.DBF    '
     @ 2, 17 SAY  ;
       'Cabecera de Pedidos               '
     DO caso20
     @ 4, 42 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_DVT00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_DVT00.DBF    '
     @ 2, 17 SAY  ;
       'Detalle de Pedidos                '
     DO caso21
     @ 4, 44 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_DIN00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_DIN00.DBF    '
     @ 2, 17 SAY  ;
       'Descripciones en Ingles            '
     DO caso23
     @ 4, 48 SAY '€€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_EST00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_EST00.DBF    '
     @ 2, 17 SAY  ;
       'Estad°sticas                      '
     DO caso25
     @ 4, 50 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_NVE00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_NVE00.DBF    '
     @ 2, 17 SAY  ;
       'No Ventas                          '
     DO caso26
     @ 4, 51 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_ORD00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_ORD00.DBF    '
     @ 2, 17 SAY  ;
       'N£mero de Orden de Reparaci¢n      '
     DO caso27
     @ 4, 52 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_NFA00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_NFA00.DBF    '
     @ 2, 17 SAY  ;
       'N£mero de Orden de Reparaci¢n      '
     DO caso28
     @ 4, 53 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_IMP00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_IMP00.DBF    '
     @ 2, 17 SAY  ;
       'Valores de Importaci¢n             '
     DO caso29
     @ 4, 54 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_INV00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_INV00.DBF    '
     @ 2, 17 SAY  ;
       'Inventario                         '
     DO caso30
     @ 4, 55 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_MODUL.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_MODUL.DBF    '
     @ 2, 17 SAY  ;
       'M¢dulos                            '
     DO caso31
     @ 4, 56 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_MOV00.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_MOV00.DBF    '
     @ 2, 17 SAY  ;
       'Movimientos                        '
     DO caso32
     @ 4, 57 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HVEST.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HVEST.DBF    '
     @ 2, 17 SAY  ;
       'Resumen de Ventas                   '
     DO caso33
     @ 4, 57 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO abre WITH 'GC_HPRES.DBF'
IF abrir
     @ 1, 17 SAY  ;
       'GC_HPRES.DBF    '
     @ 2, 17 SAY  ;
       'Presupuestos                       '
     DO caso34
     @ 4, 57 SAY '€'
ELSE
     DO termina WITH 1
     RETURN
ENDIF
DO termina WITH 2
CLOSE DATABASES
ACTIVATE SCREEN
RETURN
*
PROCEDURE termina
PARAMETER estado
CLOSE DATABASES
IF estado = 2
     = csound()
     DO p_mensaje WITH  ;
        'PROCESO TERMINADO...PRESIONE <Enter> PARA CONTINUAR'
ELSE
     = msound(20,600,20,1,-1)
ENDIF
RELEASE WINDOW temporal
ON ERROR
RETURN
*
PROCEDURE abre
PARAMETER archivo
abrir = .T.
rut = ALLTRIM(mruta)
Use &archivo Exclusive
BROWSE
RETURN
*
PROCEDURE userror
RELEASE WINDOW temporal
DO p_mensaje WITH  ;
   'ERROR: File en uso por otra estaci¢n...'
ON ERROR
abrir = .F.
RETURN
*
PROCEDURE caso1
DELETE TAG all
INDEX ON tab_codpre + tab_codtab  ;
      TAG codigo
INDEX ON tab_destab TAG descri
INDEX ON tab_codtab TAG codtab
INDEX ON tab_codpre + destab TAG  ;
      descr
INDEX ON DTOS(tab_fecha) +  ;
      tab_hora TAG fecha
RETURN
*
PROCEDURE caso2
DELETE TAG all
INDEX ON pro_codpro TAG codigo
INDEX ON pro_descri TAG descri
INDEX ON pro_modelo TAG modelo
INDEX ON pro_codpve TAG codpve
INDEX ON pro_moneda TAG moneda
INDEX ON pro_parara TAG parara
INDEX ON pro_codpro + pro_unimed  ;
      TAG medida
INDEX ON pro_numpar TAG numpar
INDEX ON pro_subcat TAG subcat
INDEX ON pro_codpve + pro_codpro  ;
      TAG pro_codpve
INDEX ON pro_marca TAG marca
INDEX ON pro_nivcit + pro_codpve  ;
      TAG pro_nivpro
INDEX ON DTOS(pro_fecha) +  ;
      pro_hora TAG fecha
RETURN
*
PROCEDURE caso3
DELETE TAG all
INDEX ON hlp_codlis TAG codigo
INDEX ON DTOS(hlp_fecha) +  ;
      hlp_hora TAG fecha
RETURN
*
PROCEDURE caso4
DELETE TAG all
INDEX ON dlp_codlis + dlp_codpro +  ;
      dlp_unimed TAG codigo
INDEX ON DTOS(dlp_fecha) +  ;
      dlp_hora TAG fecha
RETURN
*
PROCEDURE caso5
DELETE TAG all
INDEX ON pal_codpro + pal_copral  ;
      TAG codigo
INDEX ON DTOS(pal_fecha) +  ;
      pal_hora TAG fecha
RETURN
*
PROCEDURE caso6
DELETE TAG all
INDEX ON cli_tpper + cli_codigo  ;
      TAG codigo
INDEX ON cli_razsoc TAG nombre
INDEX ON DTOS(cli_feccre) +  ;
      cli_tpper + cli_codigo TAG  ;
      cli_feccre
INDEX ON DTOS(cli_fecha) +  ;
      cli_hora TAG fecha
RETURN
*
PROCEDURE caso7
DELETE TAG all
INDEX ON uni_codpro + uni_unialt  ;
      TAG codigo
INDEX ON DTOS(uni_fecha) +  ;
      uni_hora TAG fecha
RETURN
*
PROCEDURE caso8
DELETE TAG all
INDEX ON DTOC(cmv_fechac) +  ;
      cmv_indica TAG codigo
INDEX ON DTOC(cmv_fechac) +  ;
      cmv_indica + cmv_monref TAG  ;
      codigo_1
INDEX ON DTOC(cmv_fechac) +  ;
      cmv_indica + rge_monbas +  ;
      cmv_monref TAG codigo_2
INDEX ON DTOS(cmv_fechac) +  ;
      cmv_indica + rge_monbas +  ;
      cmv_monref TAG cmv_feinmo
INDEX ON DTOS(cmv_fecha) +  ;
      cmv_hora TAG fecha
RETURN
*
PROCEDURE caso9
DELETE TAG all
INDEX ON alm_codpro + alm_codalm  ;
      TAG codigo
INDEX ON alm_codpro TAG codpro
INDEX ON alm_codalm TAG codalm
INDEX ON alm_codalm + alm_ubicac  ;
      TAG ubicac
INDEX ON DTOS(alm_fecha) +  ;
      alm_hora TAG fecha
RETURN
*
PROCEDURE caso10
DELETE TAG all
INDEX ON vnd_tpper + vnd_code TAG  ;
      codigo
INDEX ON vnd_nombre TAG nombre
INDEX ON DTOS(vnd_fecha) +  ;
      vnd_hora TAG fecha
RETURN
*
PROCEDURE caso11
DELETE TAG all
INDEX ON hip_tipdoc + hip_nrodoc  ;
      TAG codigo
INDEX ON hip_codmov +  ;
      DTOS(hip_fecdoc) +  ;
      hip_tipdoc + hip_nrodoc TAG  ;
      hip_cofeti
INDEX ON hip_tipent + hip_codent +  ;
      DTOS(hip_fecdoc) TAG  ;
      hip_ticofe
INDEX ON DTOS(hip_fecha) +  ;
      hip_hora TAG fecha
RETURN
*
PROCEDURE caso12
DELETE TAG all
INDEX ON dip_tipdoc + dip_nrodoc +  ;
      dip_propar + dip_unimed +  ;
      dip_docref TAG codigo
INDEX ON DTOS(dip_fecha) +  ;
      dip_hora TAG fecha
RETURN
*
PROCEDURE caso13
DELETE TAG all
INDEX ON gas_tipdoc + gas_nrodoc +  ;
      gas_codgas TAG codigo
INDEX ON DTOS(gas_fecha) +  ;
      gas_hora TAG fecha
RETURN
*
PROCEDURE caso14
DELETE TAG all
INDEX ON ALLTRIM(abrevid) +  ;
      ALLTRIM(codigo) TAG codigo
INDEX ON ALLTRIM(codigo) TAG  ;
      usuario
INDEX ON nombre TAG nombre
INDEX ON DTOS(fecha) + hora TAG  ;
      fecha
RETURN
*
PROCEDURE caso15
DELETE TAG all
INDEX ON hco_nrodoc TAG codigo
INDEX ON hco_numfac + hco_nrodoc  ;
      TAG hco_numfac
INDEX ON hco_codent + hco_numfac  ;
      TAG hco_codent
INDEX ON DTOS(hco_fecdoc) +  ;
      hco_numfac TAG hco_fecdoc
INDEX ON hco_codent + hco_nrodoc  ;
      TAG hco_coden
INDEX ON DTOS(hco_fecha) +  ;
      hco_hora TAG fecha
RETURN
*
PROCEDURE caso16
DELETE TAG all
INDEX ON dco_nrodoc + dco_codpro  ;
      TAG codigo
INDEX ON dco_nrodoc TAG nrodoc
INDEX ON dco_codpro TAG codpro
INDEX ON dco_nrodoc + dco_codprp  ;
      TAG codigp
INDEX ON dco_numfac + dco_nrodoc +  ;
      dco_codpro TAG dco_numfac
INDEX ON DTOS(dco_feclle) +  ;
      dco_nrodoc TAG dco_feclle
INDEX ON dco_codprp + dco_nrodoc  ;
      TAG dco_codprp
INDEX ON dco_codpro + dco_nrodoc  ;
      TAG dco_codpro
INDEX ON DTOS(dco_fecha) +  ;
      dco_hora TAG fecha
RETURN
*
PROCEDURE caso17
DELETE TAG all
INDEX ON kar_codpro TAG codigo
INDEX ON kar_tipdoc + kar_nrodoc +  ;
      kar_codpro TAG docpro
INDEX ON kar_codpro +  ;
      DTOS(kar_fecdoc) TAG  ;
      kar_profec DESCENDING
INDEX ON kar_almdes +  ;
      DTOS(kar_fecdoc) +  ;
      kar_tipdoc + kar_nrodoc TAG  ;
      kar_almfec
INDEX ON kar_codpro + kar_tipore  ;
      TAG kar_codpro
INDEX ON kar_codpro +  ;
      DTOS(kar_fecing) +  ;
      kar_horing + kar_tipdoc +  ;
      kar_nrodoc TAG kar_proing  ;
      DESCENDING
INDEX ON kar_codpro +  ;
      DTOS(kar_fecing) +  ;
      kar_horing + kar_tipdoc +  ;
      kar_nrodoc TAG kar_proin2
INDEX ON kar_tipdoc + kar_nrodoc  ;
      TAG kar_tinrli
INDEX ON DTOS(kar_fecdoc) TAG  ;
      kar_fecdoc
INDEX ON DTOS(kar_fecing) +  ;
      kar_horing + kar_codmov TAG  ;
      kar_fecing
INDEX ON DTOS(kar_fecha) +  ;
      kar_hora TAG fecha
RETURN
*
PROCEDURE caso18
DELETE TAG all
INDEX ON hve_tipdoc + hve_nrodoc  ;
      TAG codigo
INDEX ON hve_tipdoc TAG abono FOR  ;
      hve_tipdoc = 'ABON'
INDEX ON hve_nrodor TAG nrdore
INDEX ON DTOS(hve_fecdoc) TAG  ;
      hve_fecdoc
INDEX ON hve_tipent + hve_codent +  ;
      DTOS(hve_fecdoc) TAG  ;
      hve_ticofe
INDEX ON DTOS(hve_fecha) +  ;
      hve_hora TAG fecha
RETURN
*
PROCEDURE caso19
DELETE TAG all
INDEX ON dve_tipdoc + dve_nrodoc +  ;
      dve_propar TAG codigo
INDEX ON dve_propar +  ;
      STR(dve_cantid, 9, 2) TAG  ;
      cantid
INDEX ON DTOS(dve_fecha) +  ;
      dve_hora TAG fecha
RETURN
*
PROCEDURE caso20
DELETE TAG all
INDEX ON hvt_numdoc TAG codigo
INDEX ON DTOS(hvt_fecha) +  ;
      hvt_hora TAG fecha
RETURN
*
PROCEDURE caso21
DELETE TAG all
INDEX ON dvt_numdoc + dvt_codpro +  ;
      dvt_unimed TAG codigo
INDEX ON DTOS(dve_fecha) +  ;
      dvt_hora TAG fecha
RETURN
*
PROCEDURE caso22
RETURN
*
PROCEDURE caso23
DELETE TAG all
INDEX ON din_codpro TAG codigo
INDEX ON DTOS(din_fecha) +  ;
      din_hora TAG fecha
RETURN
*
PROCEDURE caso25
DELETE TAG all
INDEX ON est_indest + est_tipdoc +  ;
      est_nrodoc + est_codpro TAG  ;
      codigo
INDEX ON est_tipdoc + est_nrodoc  ;
      TAG est_tipnro
INDEX ON DTOS(est_fecha) +  ;
      est_hora TAG fecha
RETURN
*
PROCEDURE caso26
DELETE TAG all
INDEX ON nve_codpro TAG codigo
INDEX ON nve_cmes1 + nve_cmes2 +  ;
      nve_cmes3 + nve_cmes4 +  ;
      nve_cmes5 + nve_cmes6 TAG  ;
      nve_c6mes DESCENDING
INDEX ON DTOS(nve_fecha) +  ;
      nve_hora TAG fecha
RETURN
*
PROCEDURE caso27
DELETE TAG all
INDEX ON ord_nrodoc + ord_codpro  ;
      TAG ord_nrodoc
INDEX ON ord_docref + ord_codpro  ;
      TAG ord_docref
INDEX ON ord_codpro + ord_nrodoc  ;
      TAG ord_codpro
INDEX ON ord_nrodoc + ord_codprp  ;
      TAG ord_docprp
INDEX ON ord_numfac + ord_nrodoc +  ;
      ord_codpro TAG ord_numfac
INDEX ON ord_codprp + ord_nrodoc +  ;
      ord_docref TAG ord_prdore
INDEX ON ord_docref + ord_nrodoc +  ;
      ord_codprp TAG ord_refprp
INDEX ON ord_codalm + ord_inorig +  ;
      ord_docref + ord_codpro TAG  ;
      ord_codino
INDEX ON ord_codalm + ord_inorig +  ;
      ord_docref + ord_codprp TAG  ;
      ord_codipr
INDEX ON DTOS(ord_fecha) +  ;
      ord_hora TAG fecha
RETURN
*
PROCEDURE caso28
DELETE TAG all
INDEX ON nfa_numfac + nfa_nrodoc +  ;
      nfa_codpro TAG nfa_numfac
INDEX ON nfa_numfac + nfa_nrodoc +  ;
      nfa_codprp TAG nfa_facprp
INDEX ON nfa_nrodoc + nfa_codprp  ;
      TAG nfa_docprp
INDEX ON DTOS(nfa_fecha) +  ;
      nfa_hora TAG fecha
RETURN
*
PROCEDURE caso29
DELETE TAG all
INDEX ON imp_tippro + imp_codent +  ;
      imp_rango TAG imp_tippro
INDEX ON DTOS(imp_fecha) +  ;
      imp_hora TAG fecha
RETURN
*
PROCEDURE caso30
DELETE TAG all
INDEX ON alm_codalm + alm_codpro  ;
      TAG codigo
INDEX ON alm_codalm + alm_ubicac  ;
      TAG ubica
INDEX ON DTOS(alm_fecha) +  ;
      alm_hora TAG fecha
RETURN
*
PROCEDURE caso31
DELETE TAG all
INDEX ON DTOS(mod_fecha) +  ;
      mod_hora TAG fecha
RETURN
*
PROCEDURE caso32
DELETE TAG all
INDEX ON mov_ano + mov_codpro +  ;
      mov_almrec TAG codigo
INDEX ON mov_ano + mov_codpro +  ;
      mov_almdes TAG codigo1
INDEX ON DTOS(mov_fecha) +  ;
      mov_hora TAG fecha
RETURN
*
PROCEDURE caso33
DELETE TAG all
INDEX ON ano + zona + sucu +  ;
      indori TAG codigo
INDEX ON DTOS(fecha) + hora TAG  ;
      fecha
RETURN
*
PROCEDURE caso34
DELETE TAG all
INDEX ON ano + zona + sucu +  ;
      indori TAG codigo
INDEX ON DTOS(fecha) + hora TAG  ;
      fecha
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
