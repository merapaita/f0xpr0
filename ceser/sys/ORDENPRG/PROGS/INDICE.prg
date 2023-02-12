*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
Index ALM_CODPRO+ALM_CODALM      ;
      tag   CODIGO     
Index ALM_CODPRO                 ;
      tag   CODPRO     
Index ALM_CODALM                 ;
      tag   CODALM     
Index ALM_CODALM+ALM_UBICAC      ;
      tag   UBICAC     
Index DTOS(ALM_FECHA)+ALM_HORA   ;
      tag   FECHA      
Index CLI_TPPER+CLI_CODIGO                    ;
      tag   CODIGO     
Index CLI_RAZSOC                              ;
      tag   NOMBRE     
Index DTOS(CLI_FECCRE)+CLI_TPPER+ ;
      CLI_CODIGO  tag    ;
      CLI_FECCRE 
Index DTOS(CLI_FECHA)+CLI_HORA                ;
      tag   FECHA      
Index DTOC(CMV_FECHAC)+CMV_INDICA                        ;
      tag   CODIGO     
Index DTOC(CMV_FECHAC)+CMV_INDICA+ ;
      CMV_MONREF            tag    ;
      CODIGO_1   
Index DTOC(CMV_FECHAC)+CMV_INDICA+ ;
      RGE_MONBAS+CMV_MONREF tag    ;
      CODIGO_2   
Index DTOS(CMV_FECHAC)+CMV_INDICA+ ;
      RGE_MONBAS+CMV_MONREF tag    ;
      CMV_FEINMO 
Index DTOS(CMV_FECHA)+CMV_HORA                           ;
      tag   FECHA      
Index DCO_NRODOC+DCO_CODPRO             ;
      tag   CODIGO     
Index DCO_NRODOC                        ;
      tag   NRODOC     
Index DCO_CODPRO                        ;
      tag   CODPRO     
Index DCO_NRODOC+DCO_CODPRP             ;
      tag   CODIGP     
Index DCO_NUMFAC+DCO_NRODOC+ ;
      DCO_CODPRO tag   DCO_NUMFAC 
Index DTOS(DCO_FECLLE)+DCO_NRODOC       ;
      tag   DCO_FECLLE 
Index DCO_CODPRP+DCO_NRODOC             ;
      tag   DCO_CODPRP 
Index DCO_CODPRO+DCO_NRODOC             ;
      tag   DCO_CODPRO 
Index DTOS(DCO_FECHA)+DCO_HORA          ;
      tag   FECHA      
Index DIP_TIPDOC+DIP_NRODOC+ ;
      DIP_PROPAR+DIP_UNIMED+ ;
      DIP_DOCREF tag   CODIGO     
Index DTOS(DIP_FECHA)+DIP_HORA                                ;
      tag   FECHA      
Index DLP_CODLIS+DLP_CODPRO+ ;
      DLP_UNIMED tag   CODIGO     
Index DTOS(DLP_FECHA)+DLP_HORA          ;
      tag   FECHA      
Index DPV_TIPDOC+DPV_NRODOC+ ;
      DPV_CODPRO tag   CODIGO     
Index DPV_CODPRO+DTOS(DPV_FECDOC)       ;
      tag   DPV_CODPRO 
Index DTOS(DPV_FECHA)+DPV_HORA          ;
      tag   FECHA      
Index DRE_TIPREP+DRE_NROREP+ ;
      DRE_CODMOV+DRE_TIPDOC+ ;
      DRE_NRODOC tag   CODIGO     
Index DTOS(DRE_FECHA)+DRE_HORA                                ;
      tag   FECHA      
Index DTR_NUMDOC+DTR_CODPRO      ;
      tag   CODIGO     
Index DTOS(DTR_FECHA)+DTR_HORA   ;
      tag   FECHA      
Index  DVE_TIPDOC+DVE_NRODOC+ ;
       DVE_PROPAR  tag   CODIGO     
Index  DVE_PROPAR+STR(DVE_CANTID, ;
       9,2)    tag   CANTID     
Index  DTOS(DVE_FECHA)+DVE_HORA           ;
       tag   FECHA      
Index  DVT_NUMDOC+DVT_CODPRO+ ;
       DVT_UNIMED  tag   CODIGO     
Index  DTOS(DVT_FECHA)+DVT_HORA           ;
       tag   FECHA      
Index  EST_INDEST+EST_TIPDOC+ ;
       EST_NRODOC+EST_CODPRO   ;
       tag:   CODIGO     
Index  EST_TIPDOC+EST_NRODOC                         ;
       tag:   EST_TIPNRO 
Index  DTOS(EST_FECHA)+EST_HORA                      ;
       tag:   FECHA      
Index  GAS_TIPDOC+GAS_NRODOC+ ;
       GAS_CODGAS  tag   CODIGO     
Index  DTOS(GAS_FECHA)+GAS_HORA           ;
       tag   FECHA      
Index  HCO_NRODOC                     ;
       tag   CODIGO     
Index  HCO_NUMFAC+HCO_NRODOC          ;
       tag   HCO_NUMFAC 
Index  HCO_CODENT+HCO_NUMFAC          ;
       tag   HCO_CODENT 
Index  DTOS(HCO_FECDOC)+ ;
       HCO_NUMFAC   tag    ;
       HCO_FECDOC 
Index  HCO_CODENT+HCO_NRODOC          ;
       tag   HCO_CODEN  
Index  DTOS(HCO_FECHA)+HCO_HORA       ;
       tag   FECHA      
Index  HIP_TIPDOC+HIP_NRODOC                               ;
       tag   CODIGO     
Index  HIP_CODMOV+ ;
       DTOS(HIP_FECDOC)+ ;
       HIP_TIPDOC+HIP_NRODOC  tag    ;
       HIP_COFETI 
Index  HIP_TIPENT+HIP_CODENT+ ;
       DTOS(HIP_FECDOC)              ;
       tag   HIP_TICOFE 
Index  DTOS(HIP_FECHA)+HIP_HORA                            ;
       tag   FECHA      
Index  HLP_CODLIS                 ;
       tag   CODIGO     
Index  DTOS(HLP_FECHA)+HLP_HORA   ;
       tag   FECHA      
Index  ANO+ZONA+SUCU+INDORI+ ;
       CODEMI+LINEA  tag   CODIGO     
Index  DTOS(FECHA)+HORA                    ;
       tag   FECHA      
Index  HPV_TIPDOC+HPV_NRODOC         ;
       tag   CODIGO     
Index  DTOS(HPV_FECDOC)+ ;
       HPV_HORDOC  tag    ;
       HPV_FECDOC 
Index  HPV_CODENT                    ;
       tag   HPV_CODENT 
Index  DTOS(HPV_FECHA)+HPV_HORA      ;
       tag   FECHA      
Structural CDX file:    ;
           CESER\DATA:BASES\GC_HRE00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: HRE_TIPREP+ ;
      HRE_NROREP
Index tag:   HRE_CODTIP Collate:  ;
      Machine   Key: HRE_CODALM+ ;
      HRE_TIPREP+HRE_NROREP
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(HRE_FECHA)+HRE_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_HTR00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: HTR_NUMDOC
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(HTR_FECHA)+HTR_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_HVE00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: HVE_TIPDOC+ ;
      HVE_NRODOC
Index tag:   ABONO      Collate:  ;
      Machine   Key: HVE_TIPDOC    ;
      For: HVE_TIPDOC="ABON"
Index tag:   NRDORE     Collate:  ;
      Machine   Key: HVE_NRDORE
Index tag:   HVE_FECDOC Collate:  ;
      Machine   Key:  ;
      DTOS(HVE_FECDOC)
Index tag:   HVE_TICOFE Collate:  ;
      Machine   Key: HVE_TIPENT+ ;
      HVE_CODENT+ ;
      DTOS(HVE_FECDOC)
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(HVE_FECHA)+HVE_HORA
Index tag:   VENTAS     Collate:  ;
      Machine   Key:  ;
      DTOS(HVE_FECDOC)+HVE_TIPDOC+ ;
      SUBSTR(HVE_NRODOC,4)
Structural CDX file:    ;
           CESER\DATA:BASES\GC_HVEST.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: ANO+ZONA+ ;
      SUCU+INDORI+CODEMI+LINEA
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(FECHA)+ ;
      HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_HVT00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: HVT_NUMDOC
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(HVT_FECHA)+HVT_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_IMP00.CDX
Index tag:   IMP_TIPPRO Collate:  ;
      Machine   Key: IMP_TIPPRO+ ;
      IMP_CODENT+IMP_RANGO
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(IMP_FECHA)+IMP_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_INV00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: ALM_CODALM+ ;
      ALM_CODPRO
Index tag:   UBICA      Collate:  ;
      Machine   Key: ALM_CODALM+ ;
      ALM_UBICAC
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(ALM_FECHA)+ALM_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_KAR00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: KAR_CODPRO
Index tag:   DOCPRO     Collate:  ;
      Machine   Key: KAR_TIPDOC+ ;
      KAR_NRODOC+KAR_CODPRO
Index tag:   KAR_PROFEC Collate:  ;
      Machine   Key: KAR_CODPRO+ ;
      DTOS(KAR_FECDOC)  ;
      (Descending)
Index tag:   KAR_ALMFEC Collate:  ;
      Machine   Key: KAR_ALMDES+ ;
      DTOS(KAR_FECDOC)+KAR_TIPDOC+ ;
      KAR_NRODOC
Index tag:   KAR_CODPRO Collate:  ;
      Machine   Key: KAR_CODPRO+ ;
      KAR_TIDORE
Index tag:   KAR_PROING Collate:  ;
      Machine   Key: KAR_CODPRO+ ;
      DTOS(KAR_FECING)+KAR_HORING+ ;
      KAR_TIPDOC+KAR_NRODOC  ;
      (Descending)
Index tag:   KAR_PROIN2 Collate:  ;
      Machine   Key: KAR_CODPRO+ ;
      DTOS(KAR_FECING)+KAR_HORING+ ;
      KAR_TIPDOC+KAR_NRODOC
Index tag:   KAR_TINRLI Collate:  ;
      Machine   Key: KAR_TIPDOC+ ;
      KAR_NRODOC
Index tag:   FECDOC     Collate:  ;
      Machine   Key:  ;
      DTOS(KAR_FECDOC)
Index tag:   KAR_FECING Collate:  ;
      Machine   Key:  ;
      DTOS(KAR_FECING)+KAR_HORING+ ;
      KAR_CODMOV
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(KAR_FECHA)+KAR_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_MODUL.CDX
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(MOD_FECHA)+MOD_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_MOV00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: MOV_ANO+ ;
      MOV_CODPRO+MOV_ALMREC
Index tag:   CODIGO1    Collate:  ;
      Machine   Key: MOV_ANO+ ;
      MOV_CODPRO+MOV_ALMDES
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(MOV_FECHA)+MOV_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_NFA00.CDX
Index tag:   NFA_NUMFAC Collate:  ;
      Machine   Key: NFA_NUMFAC+ ;
      NFA_NRODOC+NFA_CODPRO
Index tag:   NFA_FACPRP Collate:  ;
      Machine   Key: NFA_NUMFAC+ ;
      NFA_NRODOC+NFA_CODPRP
Index tag:   NFA_DOCPRP Collate:  ;
      Machine   Key: NFA_NRODOC+ ;
      NFA_CODPRP+NFA_INDEST
Index tag:   CODPRO     Collate:  ;
      Machine   Key: NFA_CODPRO
Structural CDX file:    ;
           CESER\DATA:BASES\GC_NVE00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NVE_CODPRO
Index tag:   NVE_C6MES  Collate:  ;
      Machine   Key: NVE_CMES1+ ;
      NVE_CMES2+NVE_CMES3+ ;
      NVE_CMES4+NVE_CMES5+ ;
      NVE_CMES6 (Descending)
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(NVE_FECHA)+NVE_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_ORD00.CDX
Index tag:   ORD_NRODOC Collate:  ;
      Machine   Key: ORD_NRODOC+ ;
      ORD_CODPRO
Index tag:   ORD_DOCREF Collate:  ;
      Machine   Key: ORD_DOCREF+ ;
      ORD_CODPRO
Index tag:   ORD_CODPRO Collate:  ;
      Machine   Key: ORD_CODPRO+ ;
      ORD_NRODOC
Index tag:   ORD_DOCPRP Collate:  ;
      Machine   Key: ORD_NRODOC+ ;
      ORD_CODPRP
Index tag:   ORD_NUMFAC Collate:  ;
      Machine   Key: ORD_NUMFAC+ ;
      ORD_NRODOC+ORD_CODPRP
Index tag:   ORD_PRDORE Collate:  ;
      Machine   Key: ORD_CODPRP+ ;
      ORD_NRODOC+ORD_DOCREF
Index tag:   ORD_REFPRP Collate:  ;
      Machine   Key: ORD_DOCREF+ ;
      ORD_NRODOC+ORD_CODPRP
Index tag:   ORD_CODINO Collate:  ;
      Machine   Key: ORD_CODALM+ ;
      ORD_INORIG+ORD_DOCREF+ ;
      ORD_CODPRO
Index tag:   ORD_CODIPR Collate:  ;
      Machine   Key: ORD_CODALM+ ;
      ORD_INORIG+ORD_DOCREF+ ;
      ORD_CODPRP
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(ORD_FECHA)+ORD_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_PAL00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: PAL_CODPRO+ ;
      PAL_COPRAL
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(PAL_FECHA)+PAL_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_PAR00.CDX
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(PAR_FECHA)+PAR_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_PRO00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: PRO_CODPRO
Index tag:   DESCRI     Collate:  ;
      Machine   Key: PRO_DESCRI
Index tag:   MODELO     Collate:  ;
      Machine   Key: PRO_MODELO
Index tag:   CODPVE     Collate:  ;
      Machine   Key: PRO_CODPVE
Index tag:   MONEDA     Collate:  ;
      Machine   Key: PRO_MONEDA
Index tag:   MEDIDA     Collate:  ;
      Machine   Key: PRO_CODPRO+ ;
      PRO_UNIMED
Index tag:   NUMPAR     Collate:  ;
      Machine   Key: PRO_NUMPAR
Index tag:   SUBCAT     Collate:  ;
      Machine   Key: PRO_SUBCAT
Index tag:   PARARA     Collate:  ;
      Machine   Key: PRO_PARARA
Index tag:   PRO_CODPVE Collate:  ;
      Machine   Key: PRO_CODPVE+ ;
      PRO_CODPRO
Index tag:   PRO_PROCED Collate:  ;
      Machine   Key: PRO_PROCED+ ;
      PRO_CODPVE+PRO_CODPRO
Index tag:   MARCA      Collate:  ;
      Machine   Key: PRO_MARCA
Index tag:   PRO_NIVPRO Collate:  ;
      Machine   Key: PRO_NIVCIT+ ;
      PRO_CODPVE
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(PRO_FECHA)+PRO_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_TPE00.CDX
Index tag:   TPE_INDREG Collate:  ;
      Machine   Key: TPE_INDREG
Index tag:   TPE_INDORI Collate:  ;
      Machine   Key: TPE_INDORI
Index tag:   TPE_CODPRO Collate:  ;
      Machine   Key: TPE_CODPRO
Index tag:   TPE_FECLLE Collate:  ;
      Machine   Key: TPE_FECLLE
Index tag:   TPE_KEY    Collate:  ;
      Machine   Key: TPE_INDREG+ ;
      TPE_INDORI+TPE_CODPRO+ ;
      DTOC(TPE_FECLLE)
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(TPE_FECHA)+TPE_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_UNI00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: UNI_CODPRO+ ;
      UNI_UNIALT
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(UNI_FECHA)+UNI_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GC_VND00.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: VND_TPPER+ ;
      VND_CODE
Index tag:   NOMBRE     Collate:  ;
      Machine   Key: VND_NOMBRE
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(VND_FECHA)+VND_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\GE_TAB0.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: TAB_CODPRE+ ;
      TAB_CODTAB
Index tag:   DESCRI     Collate:  ;
      Machine   Key: TAB_DESTAB
Index tag:   CODTAB     Collate:  ;
      Machine   Key: TAB_CODTAB
Index tag:   DESCR      Collate:  ;
      Machine   Key: TAB_CODPRE+ ;
      TAB_DESTAB
Index tag:   FECHA      Collate:  ;
      Machine   Key:  ;
      DTOS(TAB_FECHA)+TAB_HORA
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ASIEM.CDX
Index tag:   ASI_COD    Collate:  ;
      Machine   Key: CODEMP+ANIO
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_CAMBI.CDX
Index tag:   FECHA      Collate:  ;
      Machine   Key: FECDOC
Index tag:   TIENDA     Collate:  ;
      Machine   Key: TIENDA
Index tag:   NUMSOL     Collate:  ;
      Machine   Key: NUMSOL
Index tag:   NUMORD     Collate:  ;
      Machine   Key: NUMORD
Index tag:   CODIGO     Collate:  ;
      Machine   Key: TIPDOC+ ;
      NRODOC
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ESTAD.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: ANOREP
Index tag:   EST_NUMORD Collate:  ;
      Machine   Key: NUMORD
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ICLPR.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: INDENT+ ;
      CODENT
Index tag:   CLI_NOMENT Collate:  ;
      Machine   Key: INDENT+ ;
      NOMENT
Index tag:   NOMENT     Collate:  ;
      Machine   Key: NOMENT
Index tag:   CODENT     Collate:  ;
      Machine   Key: CODENT
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IDFAC.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: TIPDOC+ ;
      NRODOC
Index tag:   FECEMI     Collate:  ;
      Machine   Key: DTOS(FECHAD)+ ;
      CODEMI
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IDPED.CDX
Index tag:   DRE_NUMORD Collate:  ;
      Machine   Key: NUMORD
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC+ ;
      NUMORD+CODPRO
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IDPRE.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC+ ;
      NUMORD+CODPRO
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IMODE.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: CODMAR+ ;
      CODMOD
Index tag:   MOD_NOMMOD Collate:  ;
      Machine   Key: NOMMOD
Index tag:   LINEA      Collate:  ;
      Machine   Key: LINEA+CODMAR+ ;
      CODMOD
Index tag:   CLASE      Collate:  ;
      Machine   Key: CODCLA+ ;
      CODMAR+CODMOD
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_INV00.CDX
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Index tag:   NUMSOL     Collate:  ;
      Machine   Key: NUMSOL
Index tag:   NUMORD     Collate:  ;
      Machine   Key: NUMDOC
Index tag:   NUMSER     Collate:  ;
      Machine   Key: NUMSER
Index tag:   ESTADO     Collate:  ;
      Machine   Key: AUXEST
Index tag:   TALLER     Collate:  ;
      Machine   Key: CODTALL
Index tag:   MARCA      Collate:  ;
      Machine   Key: CODMAR
Index tag:   EMISOR     Collate:  ;
      Machine   Key: CODEMI
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IOREP.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC
Index tag:   ORD_CODENT Collate:  ;
      Machine   Key: CODENT+ ;
      DTOC2(FECEMI)
Index tag:   ORD_FCHEMI Collate:  ;
      Machine   Key:  ;
      DTOC2(FECEMI)
Index tag:   ORD_CODMAR Collate:  ;
      Machine   Key: CODMAR+ ;
      CODMOD+DTOC2(FECEMI)
Index tag:   ORD_NUMSOL Collate:  ;
      Machine   Key: NUMSOL
Index tag:   ORD_NUMSER Collate:  ;
      Machine   Key: NUMSER
Index tag:   ORD_TECN   Collate:  ;
      Machine   Key: CODTEC+ ;
      DTOS(FECEMI)+NUMDOC
Index tag:   ORD_FECDOC Collate:  ;
      Machine   Key: DTOS(FECEMI)+ ;
      NUMDOC
Index tag:   ORD_ESEM   Collate:  ;
      Machine   Key: AUXEST+ ;
      CODEMI+DTOS(FECEST)+NUMDOC
Index tag:   ORD_NUMFAB Collate:  ;
      Machine   Key: CODFABO+ ;
      NUMFABO+NUMDOC
Index tag:   ORD_INESTA Collate:  ;
      Machine   Key: INDORI+ ;
      INDEST+CODTALL+DTOS(FECEMI)+ ;
      NUMDOC
Index tag:   ORD_TECEST Collate:  ;
      Machine   Key: CODTEC+ ;
      AUXEST
Index tag:   ORD_FECIND Collate:  ;
      Machine   Key: DTOS(FECFIN)+ ;
      INDORI
Index tag:   ORD_MAMOSE Collate:  ;
      Machine   Key: CODMAR+ ;
      CODMOD+NUMSER
Index tag:   ORD_ENTIND Collate:  ;
      Machine   Key: DTOS(FECENT)+ ;
      INDORI
Index tag:   ORD_FECFAC Collate:  ;
      Machine   Key:  ;
      DTOS(FECFABO)
Index tag:   CODENT     Collate:  ;
      Machine   Key: CODENT
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Memo file:    ;
     CESER\DATA:BASES\ST_IOREP.FPT
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IPREP.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC
Index tag:   REP_NUMORD Collate:  ;
      Machine   Key: NUMORD
Index tag:   REP_FECNUM Collate:  ;
      Machine   Key: DTOS(FECEMI)+ ;
      NUMDOC
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_IREDO.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: INDODO+ ;
      NUMODO
Index tag:   REL_INDICA Collate:  ;
      Machine   Key: INDDDO+ ;
      NUMDDO
Index tag:   IRE_INDNUM Collate:  ;
      Machine   Key: INDODO+ ;
      NUMODO+INDDDO+NUMDDO
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ISCIC.CDX
Index tag:   NUMSOL     Collate:  ;
      Machine   Key: NUMSOL
Index tag:   NUMORD     Collate:  ;
      Machine   Key: NUMORD
Index tag:   FECSOL     Collate:  ;
      Machine   Key: NUMSOL+ ;
      DTOS(FECINI)
Index tag:   FECUSE     Collate:  ;
      Machine   Key: DTOS(FECCOM)+ ;
      USER
Index tag:   FECINI     Collate:  ;
      Machine   Key: DTOS(FECINI)+ ;
      USER
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Memo file:    ;
     CESER\DATA:BASES\ST_ISCIC.FPT
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ISERI.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: CODENT+ ;
      CODMAR+DTOC(FECING)
Index tag:   SER_CODMAR Collate:  ;
      Machine   Key: CODMAR+ ;
      MODELO+NUMSER
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ISINT.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: CODCLA+ ;
      CODSIN
Index tag:   MSI_CODCLA Collate:  ;
      Machine   Key: CODCLA+ ;
      DESSIN
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ISPRE.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC
Index tag:   PRE_CODENT Collate:  ;
      Machine   Key: CODENT+ ;
      DTOC2(FECEMI)
Index tag:   PRE_FECEMI Collate:  ;
      Machine   Key:  ;
      DTOC2(FECEMI)
Index tag:   PRE_CODMAR Collate:  ;
      Machine   Key: CODMAR+ ;
      CODMOD+DTOC2(FECEMI)
Index tag:   ST_NUMSOL  Collate:  ;
      Machine   Key: NUMSOL
Index tag:   ST_NUMSER  Collate:  ;
      Machine   Key: NUMSER
Index tag:   PRE_INFENU Collate:  ;
      Machine   Key: INDEST+ ;
      NUMDOC
Index tag:   PRE_NUMORD Collate:  ;
      Machine   Key: NUMORD
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Memo file:    ;
     CESER\DATA:BASES\ST_ISPRE.FPT
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ISREP.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC
Index tag:   SOL_CODENT Collate:  ;
      Machine   Key: CODENT+ ;
      DTOC2(FECEMI)
Index tag:   SOL_FCHEMI Collate:  ;
      Machine   Key:  ;
      DTOC2(FECEMI)
Index tag:   SOL_MARMOD Collate:  ;
      Machine   Key: CODMAR+ ;
      CODMOD+DTOC2(FECEMI)
Index tag:   SOL_CODSTK Collate:  ;
      Machine   Key: CODSTK+ ;
      NUMSTK
Index tag:   SOL_SERIE  Collate:  ;
      Machine   Key: NUMSER
Index tag:   SOL_EMISOR Collate:  ;
      Machine   Key: CODEMI+ ;
      INDORI
Index tag:   CODENT     Collate:  ;
      Machine   Key: CODENT
Index tag:   EMIMOD     Collate:  ;
      Machine   Key: CODEMI+ ;
      CODMOD+DTOC(FECEMI)
Index tag:   SOL_MAMOSE Collate:  ;
      Machine   Key: CODMAR+ ;
      CODMOD+NUMSER
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Memo file:    ;
     CESER\DATA:BASES\ST_ISREP.FPT
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ISSRE.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC+ ;
      CODSIN
Index tag:   DSI_CODSIN Collate:  ;
      Machine   Key: CODSIN
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_ITECN.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: CODENT
Index tag:   TEC_NOMTEC Collate:  ;
      Machine   Key: NOMENT
Index tag:   TEC_CODTEC Collate:  ;
      Machine   Key: CODTEC
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_MOBRA.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: MO_CODMAR+ ;
      MO_CODART
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_MOVCA.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMORD+ ;
      CODCAU
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Index tag:   NUMSOL     Collate:  ;
      Machine   Key: NUMSOL
Structural CDX file:    ;
           CESER\DATA:BASES\ST_MOVSO.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMORD+ ;
      CODSOL
Index tag:   NUMSOL     Collate:  ;
      Machine   Key: NUMSOL
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_MVORD.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: DTOC(DIA)+ ;
      HORA+ORDEN
Index tag:   EOR_NROORD Collate:  ;
      Machine   Key: ORDEN
Index tag:   ESTADO     Collate:  ;
      Machine   Key: ORDEN+ ;
      ESTADO
Index tag:   MVO_TECNIC Collate:  ;
      Machine   Key: ESTADO+ ;
      DTOS(DIA)+ORDEN
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Index tag:   ORDIA      Collate:  ;
      Machine   Key: ORDEN+ESTADO+ ;
      DTOS(DIA)+HORA
Memo file:    ;
     CESER\DATA:BASES\ST_MVORD.FPT
Structural CDX file:    ;
           CESER\DATA:BASES\ST_SICLI.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: NUMDOC+ ;
      CODSIN
Index tag:   CODSIN     Collate:  ;
      Machine   Key: CODSIN
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_SINT.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: CODSIN
Index tag:   DESSIN     Collate:  ;
      Machine   Key: DESSIN+ ;
      CODSIN
Index tag:   SIN_LINCOD Collate:  ;
      Machine   Key: LINEA+ ;
      CODSIN
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(DATE)+ ;
      TIME
Structural CDX file:    ;
           CESER\DATA:BASES\ST_USERS.CDX
Index tag:   CODIGO     Collate:  ;
      Machine   Key: CODEMP+ ;
      ESTADO
Index tag:   FECHA      Collate:  ;
      Machine   Key: DTOS(FECHA)+ ;
      HORA
Index tag:   NUMSOL     Collate:  ;
      Machine   Key: NUMSOL
*
*** 
*** ReFox - retrace your steps ... 
***
