*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
SET CONSOLE OFF
05,10 to 20,70 double
08,20 say "Archivo :"
10,20 say "Indice  :"
12,15 to 14,60
USE EXCLUSIVE GE_TAB0
@ 08, 30 SAY DBF()
PACK
DELETE TAG all
INDEX ON tab_codpre + tab_codtab  ;
      TAG codigo
@ 10,30 say CDX()
INDEX ON tab_destab TAG descri
@ 10,30 say CDX()
INDEX ON tab_codtab TAG codtab
@ 10,30 say CDX()
INDEX ON tab_codpre + tab_destab  ;
      TAG descr
@ 10,30 say CDX()
INDEX ON DTOS(tab_fecha) +  ;
      tab_hora TAG fecha
USE
RETURN
USE EXCLUSIVE GC_ALM00
PACK
DELETE TAG all
INDEX ON alm_codpro + alm_codalm  ;
      TAG codigo
INDEX ON alm_codpro TAG codpro
INDEX ON alm_codalm TAG codalm
INDEX ON alm_codalm + alm_ubicac  ;
      TAG ubicac
INDEX ON DTOS(alm_fecha) +  ;
      alm_hora TAG fecha
USE
USE EXCLUSIVE GC_CLI00
PACK
DELETE TAG all
INDEX ON cli_tpper + cli_codigo  ;
      TAG codigo
INDEX ON cli_razsoc TAG nombre
INDEX ON DTOS(cli_feccre) TAG  ;
      cli_feccre
INDEX ON DTOS(cli_fecha) +  ;
      cli_hora TAG fecha
USE
rge_monbas = 'SOL '
USE EXCLUSIVE GC_CMV00
PACK
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
USE
USE EXCLUSIVE GC_DCO00
PACK
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
USE
USE EXCLUSIVE GC_DIN00
PACK
DELETE TAG all
INDEX ON din_codpro TAG codigo
INDEX ON DTOS(din_fecha) +  ;
      din_hora TAG fecha
USE
USE EXCLUSIVE GC_DIP00
PACK
DELETE TAG all
INDEX ON dip_tipdoc + dip_nrodoc +  ;
      dip_propar + dip_unimed +  ;
      dip_docref TAG codigo
INDEX ON DTOS(dip_fecha) +  ;
      dip_hora TAG fecha
USE
USE EXCLUSIVE GC_DLP00
PACK
DELETE TAG all
INDEX ON dlp_codlis + dlp_codpro +  ;
      dlp_unimed TAG codigo
INDEX ON DTOS(dlp_fecha) +  ;
      dlp_hora TAG fecha
USE
USE EXCLUSIVE GC_DPV00
PACK
DELETE TAG all
INDEX ON dpv_tipdoc + dpv_nrodoc +  ;
      dpv_codpro TAG codigo
INDEX ON dpv_codpro +  ;
      DTOS(dpv_fecdoc) TAG  ;
      dpv_codpro
INDEX ON DTOS(dpv_fecha) +  ;
      dpv_hora TAG fecha
USE
USE EXCLUSIVE GC_DRE00
PACK
DELETE TAG all
INDEX ON dre_tiprep + dre_nrorep +  ;
      dre_codmov + dre_tipdoc +  ;
      dre_nrodoc TAG codigo
INDEX ON DTOS(dre_fecha) +  ;
      dre_hora TAG fecha
USE
USE EXCLUSIVE GC_DTR00
PACK
DELETE TAG all
INDEX ON dtr_numdoc + dtr_codpro  ;
      TAG codigo
INDEX ON DTOS(dtr_fecha) +  ;
      dtr_hora TAG fecha
USE
USE EXCLUSIVE GC_DVE00
PACK
DELETE TAG all
INDEX ON dve_tipdoc + dve_nrodoc +  ;
      dve_propar TAG codigo
INDEX ON dve_propar +  ;
      STR(dve_cantid, 9, 2) TAG  ;
      cantid
INDEX ON DTOS(dve_fecha) +  ;
      dve_hora TAG fecha
USE
USE EXCLUSIVE GC_DVT00
PACK
DELETE TAG all
INDEX ON dvt_numdoc + dvt_codpro +  ;
      dvt_unimed TAG codigo
INDEX ON DTOS(dvt_fecha) +  ;
      dvt_hora TAG fecha
USE EXCLUSIVE GC_EST00
PACK
DELETE TAG all
INDEX ON est_indest + est_tipdoc +  ;
      est_nrodoc + est_codpro TAG  ;
      codigo
INDEX ON est_tipdoc + est_nrodoc  ;
      TAG est_tipnro
INDEX ON DTOS(est_fecha) +  ;
      est_hora TAG fecha
USE
USE EXCLUSIVE GC_GAS00
PACK
DELETE TAG all
INDEX ON gas_tipdoc + gas_nrodoc +  ;
      gas_codgas TAG codigo
INDEX ON DTOS(gas_fecha) +  ;
      gas_hora TAG fecha
USE
USE EXCLUSIVE GC_HCO00
PACK
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
USE
USE EXCLUSIVE GC_HIP00
PACK
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
USE
USE EXCLUSIVE GC_HLP00
PACK
DELETE TAG all
INDEX ON hlp_codlis TAG codigo
INDEX ON DTOS(hlp_fecha) +  ;
      hlp_hora TAG fecha
USE
USE EXCLUSIVE GC_HPRES
PACK
DELETE TAG all
INDEX ON ano + zona + sucu +  ;
      indori + codemi + linea TAG  ;
      codigo
INDEX ON DTOS(fecha) + hora TAG  ;
      fecha
USE
USE EXCLUSIVE GC_HPV00
PACK
DELETE TAG all
INDEX ON hpv_tipdoc + hpv_nrodoc  ;
      TAG codigo
INDEX ON DTOS(hpv_fecdoc) +  ;
      hpv_hordoc TAG hpv_fecdoc
INDEX ON hpv_codent TAG  ;
      hpv_codent
INDEX ON DTOS(hpv_fecha) +  ;
      hpv_hora TAG fecha
USE
USE EXCLUSIVE GC_HRE00
PACK
DELETE TAG all
INDEX ON hre_tiprep + hre_nrorep  ;
      TAG codigo
INDEX ON hre_codalm + hre_tiprep +  ;
      hre_nrorep TAG hre_codtip
INDEX ON DTOS(hre_fecha) +  ;
      hre_hora TAG fecha
USE
USE EXCLUSIVE GC_HTR00
PACK
DELETE TAG all
INDEX ON htr_numdoc TAG codigo
INDEX ON DTOS(htr_fecha) +  ;
      htr_hora TAG fecha
USE
USE EXCLUSIVE GC_HVE00
PACK
DELETE TAG all
INDEX ON hve_tipdoc + hve_nrodoc  ;
      TAG codigo
Index  on  HVE_TIPDOC   For:  ;
       HVE_TIPDOC="ABON"    tag    ;
       ABONO      
INDEX ON hve_nrdore TAG nrdore
INDEX ON hve_tipent + hve_codent +  ;
      DTOS(hve_fecdoc) TAG  ;
      hve_ticofe
INDEX ON DTOS(hve_fecha) +  ;
      hve_hora TAG fecha
INDEX ON DTOS(hve_fecdoc) TAG  ;
      hve_fecdoc
USE
USE EXCLUSIVE GC_HVEST
PACK
DELETE TAG all
INDEX ON ano + zona + sucu +  ;
      indori + codemi + linea TAG  ;
      codigo
INDEX ON DTOS(fecha) + hora TAG  ;
      fecha
USE
USE EXCLUSIVE GC_HVT00
PACK
DELETE TAG all
INDEX ON hvt_numdoc TAG codigo
INDEX ON DTOS(hvt_fecha) +  ;
      hvt_hora TAG fecha
USE
USE EXCLUSIVE GC_IMP00
PACK
DELETE TAG all
INDEX ON imp_tippro + imp_codent +  ;
      imp_rango TAG imp_tippro
INDEX ON DTOS(imp_fecha) +  ;
      imp_hora TAG fecha
USE
USE EXCLUSIVE GC_INV00
PACK
DELETE TAG all
INDEX ON alm_codalm + alm_codpro  ;
      TAG codigo
INDEX ON alm_codalm + alm_ubicac  ;
      TAG ubica
INDEX ON DTOS(alm_fecha) +  ;
      alm_hora TAG fecha
USE
USE EXCLUSIVE GC_KAR00
PACK
DELETE TAG all
INDEX ON kar_codpro TAG codigo
INDEX ON kar_tipdoc + kar_nrodoc +  ;
      kar_codpro TAG docpro
Index  on  KAR_CODPRO+ ;
       DTOS(KAR_FECDOC)  ;
       (Descending)                                   ;
       tag   KAR_PROFEC 
INDEX ON kar_almdes +  ;
      DTOS(kar_fecdoc) +  ;
      kar_tipdoc + kar_nrodoc TAG  ;
      kar_almfec
INDEX ON kar_codpro + kar_tidore  ;
      TAG kar_codpro
INDEX ON kar_codpro +  ;
      DTOS(kar_fecing) +  ;
      kar_horing + kar_tipdoc +  ;
      kar_nrodoc(descending) TAG  ;
      kar_proing
INDEX ON kar_codpro +  ;
      DTOS(kar_fecing) +  ;
      kar_horing + kar_tipdoc +  ;
      kar_nrodoc TAG kar_proin2
INDEX ON kar_tipdoc + kar_nrodoc  ;
      TAG kar_tinrli
INDEX ON DTOS(kar_fecdoc) TAG  ;
      fecdoc
INDEX ON DTOS(kar_fecing) +  ;
      kar_horing + kar_codmov TAG  ;
      kar_fecing
INDEX ON DTOS(kar_fecha) +  ;
      kar_hora TAG fecha
USE
use GC_MODUL use exclu
PACK
DELETE TAG all
INDEX ON DTOS(mod_fecha) +  ;
      mod_hora TAG fecha
USE
USE EXCLUSIVE GC_MOV00
PACK
DELETE TAG all
INDEX ON mov_ano + mov_codpro +  ;
      mov_almdes TAG codigo1
INDEX ON mov_ano + mov_codpro +  ;
      mov_almrec TAG codigo
INDEX ON DTOS(mov_fecha) +  ;
      mov_hora TAG fecha
USE
USE EXCLUSIVE GC_NFA00
PACK
DELETE TAG all
INDEX ON nfa_codpro TAG codpro
INDEX ON nfa_numfac + nfa_nrodoc +  ;
      nfa_codpro TAG nfa_numfac
INDEX ON nfa_numfac + nfa_nrodoc +  ;
      nfa_codprp TAG nfa_facprp
INDEX ON nfa_nrodoc + nfa_codprp +  ;
      nfa_indest TAG nfa_docprp
INDEX ON DTOS(nfa_fecha) +  ;
      nfa_hora TAG fecha
USE
USE EXCLUSIVE GC_NVE00
PACK
DELETE TAG all
INDEX ON nve_codpro TAG codigo
INDEX ON nve_cmes1 + nve_cmes2 +  ;
      nve_cmes3 + nve_cmes4 +  ;
      nve_cmes5 +  ;
      nve_cmes6(descending) TAG  ;
      nve_c6mes
INDEX ON DTOS(nve_fecha) +  ;
      nve_hora TAG fecha
USE
USE EXCLUSIVE GC_ORD00
PACK
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
      ord_codprp TAG ord_numfac
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
USE
USE EXCLUSIVE GC_PAL00
PACK
DELETE TAG all
INDEX ON pal_codpro + pal_copral  ;
      TAG codigo
INDEX ON DTOS(pal_fecha) +  ;
      pal_hora TAG fecha
USE
USE EXCLUSIVE GC_PAR00
PACK
DELETE TAG all
INDEX ON DTOS(par_fecha) +  ;
      par_hora TAG fecha
USE
USE EXCLUSIVE GC_PRO00
PACK
DELETE TAG all
INDEX ON pro_codpro TAG codigo
INDEX ON pro_descri TAG descri
INDEX ON pro_modelo TAG modelo
INDEX ON pro_codpve TAG codpve
INDEX ON pro_moneda TAG moneda
INDEX ON pro_codpro + pro_unimed  ;
      TAG medida
INDEX ON pro_numpar TAG numpar
INDEX ON pro_subcat TAG subcat
INDEX ON pro_parara TAG parara
INDEX ON pro_codpve + pro_codpro  ;
      TAG pro_codpve
INDEX ON pro_proced + pro_codpve +  ;
      pro_codpro TAG pro_proced
INDEX ON pro_marca TAG marca
INDEX ON pro_nivcit + pro_codpve  ;
      TAG pro_nivpro
INDEX ON DTOS(pro_fecha) +  ;
      pro_hora TAG fecha
USE
USE EXCLUSIVE GC_TPE00
PACK
DELETE TAG all
INDEX ON tpe_indreg TAG  ;
      tpe_indreg
INDEX ON tpe_indori TAG  ;
      tpe_indori
INDEX ON tpe_codpro TAG  ;
      tpe_codpro
INDEX ON tpe_feclle TAG  ;
      tpe_feclle
INDEX ON tpe_indreg + tpe_indori +  ;
      tpe_codpro +  ;
      DTOC(tpe_feclle) TAG  ;
      tpe_key
INDEX ON DTOS(tpe_fecha) +  ;
      tpe_hora TAG fecha
USE
USE EXCLUSIVE GC_UNI00
PACK
DELETE TAG all
INDEX ON uni_codpro + uni_unialt  ;
      TAG codigo
INDEX ON DTOS(uni_fecha) +  ;
      uni_hora TAG fecha
USE
USE EXCLUSIVE GC_VND00
PACK
DELETE TAG all
INDEX ON vnd_tpper + vnd_code TAG  ;
      codigo
INDEX ON vnd_nombre TAG nombre
INDEX ON DTOS(vnd_fecha) +  ;
      vnd_hora TAG fecha
USE
USE EXCLUSIVE ST_ASIEM
PACK
DELETE TAG all
INDEX ON codemp + anio TAG  ;
      asi_cod
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ESTAD
PACK
DELETE TAG all
INDEX ON anorep TAG codigo
INDEX ON numord TAG est_numord
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ICLPR
PACK
DELETE TAG all
INDEX ON indent + codent TAG  ;
      codigo
INDEX ON indent + noment TAG  ;
      cli_noment
INDEX ON noment TAG noment
INDEX ON codent TAG codent
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_IDFAC
PACK
DELETE TAG all
INDEX ON tipdoc + nrodoc TAG  ;
      codigo
INDEX ON DTOS(fechad) + codemi  ;
      TAG fecemi
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_IDPED
PACK
DELETE TAG all
Index  NUMORD               tag    ;
       DRE_NUMORD 
Index  NUMDOC+NUMORD+CODPRO tag    ;
       CODIGO     
Index  DTOS(DATE)+TIME      tag    ;
       FECHA      
USE
USE EXCLUSIVE ST_IDPRE
PACK
DELETE TAG all
INDEX ON numdoc + numord + codpro  ;
      TAG codigo
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_IMODE
PACK
DELETE TAG all
INDEX ON codmar + codmod TAG  ;
      codigo
INDEX ON nommod TAG mod_nommod
INDEX ON linea + codmar + codmod  ;
      TAG linea
INDEX ON codcla + codmar + codmod  ;
      TAG clase
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_INV00
PACK
DELETE TAG all
INDEX ON numsol TAG numsol
INDEX ON numdoc TAG numord
INDEX ON auxest TAG estado
INDEX ON codtall TAG taller
INDEX ON codmar TAG marca
INDEX ON codemi TAG emisor
INDEX ON numser TAG numser
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_IOREP
PACK
DELETE TAG all
INDEX ON numdoc TAG codigo
INDEX ON codent + dtoc2(fecemi)  ;
      TAG ord_codent
INDEX ON dtoc2(fecemi) TAG  ;
      ord_fchemi
INDEX ON codmar + codmod +  ;
      dtoc2(fecemi) TAG  ;
      ord_codmar
INDEX ON numsol TAG ord_numsol
INDEX ON numser TAG ord_numser
INDEX ON codtec + DTOS(fecemi) +  ;
      numdoc TAG ord_tecn
INDEX ON DTOS(fecemi) + numdoc  ;
      TAG ord_fecdoc
INDEX ON auxest + codemi +  ;
      DTOS(fecest) + numdoc TAG  ;
      ord_esem
INDEX ON codfabo + numfabo +  ;
      numdoc TAG ord_numfab
INDEX ON indori + indest +  ;
      codtall + DTOS(fecemi) +  ;
      numdoc TAG ord_inesta
INDEX ON codtec + auxest TAG  ;
      ord_tecest
INDEX ON DTOS(fecfin) + indori  ;
      TAG ord_fecind
INDEX ON codmar + codmod + numser  ;
      TAG ord_mamose
INDEX ON DTOS(fecent) + indori  ;
      TAG ord_entind
INDEX ON DTOS(fecfabo) TAG  ;
      ord_fecfac
INDEX ON codent TAG codent
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_IPARG
PACK
USE
USE EXCLUSIVE ST_IPREP
PACK
DELETE TAG all
INDEX ON numdoc TAG codigo
INDEX ON numord TAG rep_numord
INDEX ON DTOS(fecemi) + numdoc  ;
      TAG rep_fecnum
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_IREDO
PACK
DELETE TAG all
INDEX ON indodo + numodo TAG  ;
      codigo
INDEX ON indddo + numddo TAG  ;
      rel_indica
INDEX ON indodo + numodo + indddo +  ;
      numddo TAG ire_indnum
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISCIC
PACK
DELETE TAG all
INDEX ON numsol TAG numsol
INDEX ON numord TAG numord
INDEX ON numsol + DTOS(fecini)  ;
      TAG fecsol
INDEX ON DTOS(feccom) + user TAG  ;
      fecuse
INDEX ON DTOS(fecini) + user TAG  ;
      fecini
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISERI
PACK
DELETE TAG all
INDEX ON codent + codmar +  ;
      DTOC(fecing) TAG codigo
INDEX ON codmar + modelo + numser  ;
      TAG ser_codmar
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISINT
PACK
DELETE TAG all
INDEX ON codcla + codsin TAG  ;
      codigo
INDEX ON codcla + dessin TAG  ;
      msi_codcla
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISORE
PACK
DELETE TAG all
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISPRE
PACK
DELETE TAG all
INDEX ON numdoc TAG codigo
INDEX ON codent + dtoc2(fecemi)  ;
      TAG pre_codent
INDEX ON dtoc2(fecemi) TAG  ;
      pre_fecemi
INDEX ON codmar + codmod +  ;
      dtoc2(fecemi) TAG  ;
      pre_codmar
INDEX ON numsol TAG st_numsol
INDEX ON numser TAG st_numser
INDEX ON indest + numdoc TAG  ;
      pre_infenu
INDEX ON numord TAG pre_numord
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISREP
PACK
DELETE TAG all
INDEX ON numdoc TAG codigo
INDEX ON codent + dtoc2(fecemi)  ;
      TAG sol_codent
INDEX ON dtoc2(fecemi) TAG  ;
      sol_fchemi
INDEX ON codmar + codmod +  ;
      dtoc2(fecemi) TAG  ;
      sol_marmod
INDEX ON codstk + numstk TAG  ;
      sol_codstk
INDEX ON numser TAG sol_serie
INDEX ON codemi + indori TAG  ;
      sol_emisor
INDEX ON codent TAG codent
INDEX ON codemi + codmod +  ;
      DTOC(fecemi) TAG emimod
INDEX ON codmar + codmod + numser  ;
      TAG sol_mamose
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ISSRE
PACK
DELETE TAG all
INDEX ON numdoc + codsin TAG  ;
      codigo
INDEX ON codsin TAG dsi_codsin
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_ITECN
PACK
DELETE TAG all
INDEX ON codent TAG codigo
INDEX ON noment TAG tec_nomtec
INDEX ON codtec TAG tec_codtec
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_MOBRA
PACK
DELETE TAG all
INDEX ON mo_codmar + mo_codart  ;
      TAG codigo
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_MOVCA
PACK
DELETE TAG all
INDEX ON numord + codcau TAG  ;
      codigo
INDEX ON DTOS(date) + time TAG  ;
      fecha
INDEX ON numsol TAG numsol
USE
USE EXCLUSIVE ST_MOVSO
PACK
DELETE TAG all
INDEX ON numord + codsol TAG  ;
      codigo
INDEX ON DTOS(date) + time TAG  ;
      fecha
INDEX ON numsol TAG numsol
USE
USE EXCLUSIVE ST_MVORD
PACK
DELETE TAG all
INDEX ON DTOC(dia) + hora + orden  ;
      TAG codigo
INDEX ON orden TAG eor_nroord
INDEX ON orden + estado TAG  ;
      estado
INDEX ON estado + DTOS(dia) +  ;
      orden TAG mvo_tecnic
INDEX ON DTOS(date) + time TAG  ;
      fecha
INDEX ON orden + estado +  ;
      DTOS(dia) + hora TAG ordia
USE
USE EXCLUSIVE ST_SICLI
PACK
DELETE TAG all
INDEX ON numdoc + codsin TAG  ;
      codigo
INDEX ON codsin TAG codsin
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_SINT
PACK
DELETE TAG all
INDEX ON codsin TAG codigo
INDEX ON dessin + codsin TAG  ;
      dessin
INDEX ON linea + codsin TAG  ;
      sin_lincod
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
USE EXCLUSIVE ST_USERS
PACK
DELETE TAG all
INDEX ON codemp + estado TAG  ;
      codigo
INDEX ON numsol TAG numsol
INDEX ON DTOS(fecha) + hora TAG  ;
      fecha
USE
*
FUNCTION dtoc2
PARAMETER wk_par
wk_aux = STR(YEAR(wk_par), 4) +  ;
         STR(MONTH(wk_par), 2) +  ;
         STR(DAY(wk_par), 2)
RETURN wk_aux
*
*** 
*** ReFox - retrace your steps ... 
***
