 
    * SE PROCESAN LAS RETENCIONES DE LOS COMPROBANTES REGULARIZADOS Y NORMALES 
    
    *------------
    * RETENCIONES
    *------------
    
    IF !EMPTY(vcodpart)             
	    SET FILTER TO Reten.periodo=alltrim(vPeriodo) and ;
	                  between(val(Reten.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. ;
	                  Compag.CodFTE = ALLTRIM(vCodFte) .AND.;	    
	                  SUBSTR(Reten.CODCAL, 8,2) = ALLTRIM(vCodPrg) .AND. ;
	                  SUBSTR(Reten.CodCal,10,3) = alltrim(vCodSubPr) .and. ;	                  
                      iif(!EMPTY(vCodPry),SUBSTR(Reten.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                      iif(!EMPTY(vCodSpy),SUBSTR(Reten.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND ;
                      IIF(!empty(vcodact),SUBSTR(Reten.CODCAL,13,2)=vcodact,.t.) AND ;                      
                      COMPAG.tipfun = alltrim(vtipfun) AND ;
	                  iif(alltrim(vTipFun)='F',RETEN.PARTRET=vcodpart,.T.) AND ;
                      Reten.estado#'99'   and ;
                      Reten.Conpago='û'   and ;
                      Compag.Reten > 0    and ;
                      (Hoja.Estado="50" OR Hoja.Estado="52") AND ;
                      !EMPTY(Reten.nummeshc) AND 
                      !EMPTY(Reten.Numhc) AND
                      !EMPTY(Reten.NUMCP) 
                  
    ELSE
	    SET FILTER TO Reten.periodo=alltrim(vperiodO) AND;
	                  between(val(Reten.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. ;
                      between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2)) and ;	                  
                      Compag.CodFTE = ALLTRIM(vCodFte) .AND. ;
	                  SUBSTR(reten.CODCAL, 8,2)=ALLTRIM(vCodPrg) AND;                      
                      SUBSTR(Reten.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                      iif(!EMPTY(vCodPry),SUBSTR(Reten.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) and ;
                      iif(!EMPTY(vCodSpy),SUBSTR(Reten.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND ;
                      IIF(!empty(vcodact),SUBSTR(Reten.CODCAL,13,2)=vcodact,.t.) AND ;
                      COMPAG.tipfun = alltrim(vtipfun) AND ;                      
                      !EMPTY(Reten.NUMCP) AND ;
                      Reten.estado#'99' AND ;                      
                      Reten.Conpago='û' AND ;
                      Compag.Reten>0 and ;
                      (Hoja.Estado="50" OR Hoja.Estado="52") AND ;
                      !EMPTY(Reten.nummeshc) 
                      AND !EMPTY(Reten.Numhc)
    ENDIF 
    
    SELE RETEN
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	 repla   fecref with M.fecpag,;
          	        CODANAL WITH M.PARTRET,;
          	        CODPART WITH HOJA.CODPART,;
          	         NUMMES WITH m.MEScppg,;
          	         tipdoc with 'C/P',;
          	         numref with m.NumCppg,;
          	         codcal with m.codcal,;
          	         valdeb with m.valret,;
          	         valhab with m.valret,;
          	           TIPO with '2',;
          	         DESCRI with 'RET - '+FTE(),;
          	         tipfte with compag.docref,;
          	         numfte with compag.numref,;
          	         TIPOPE with 1,;
          	         mesfte with COMPAG.NUMMESHC,;
          	         codprg with SUBSTR(codcal,8,2),;
          	       codsubpr with substr(codcal,10,3),;
          	        Codproy with substr(codcal,13,3),;
          	       codsupry with substr(codcal,16,2),;
          	          TOTCP with m.valret
          else
          	repla     fecref with m.fecpag,;
          	         cODANAL WITH M.partret,;
          	          NUMMES WITH m.MEScppg,;
          	          tipdoc with 'C/P',;
          	          numref with m.NumCpPG,;
          	          codcal with m.codcal,;
          	          valdeb with m.valret,;
          	          valhab with m.valret,;
          	            TIPO with '2',;
          	          DESCRI with 'RET - '+FTE(),;
          	          tipfte with compag.docref,;
          	          numfte with compag.numref,;
          	          TIPOPE with 1,;
          	          mesfte WITH COMPAG.NUMMESHC,;
          	          codprg with SUBSTR(codcal,8,2),;
          	        codsubpr with substr(codcal,10,3),;
          	          codact with substr(codcal,13,2),;
          	           TOTCP WITH m.valret
          endif
       ENDIF
       UNLOCK
       SELEC RETEN
      @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*   
    ENDSCAN 
    
    SET RELA TO
    
    SELE HOJA
	SET RELATION OFF INTO Reten
	SET RELATION OFF INTO Compag 
    SELE Reten
    SET RELATION OFF INTO Compag
    
    USE IN 1              
    USE IN 5
    USE IN 2
    
    *-------------------------
    *Regularizaciones Con Retencion 
    *----------------------------------
    
  	USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
  	USE ComPag   IN 5   ORDER TAG ComPag1   ALIAS ComPag
	USE MaeReg   IN 2   ORDER TAG Maereg2   ALIAS MaeReg
	
	wINDi=SYS(3)+'.IDX'
	
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
    SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag
    
    IF !EMPTY(vcodpart)                
	    SET FILTER TO between(val(MaeReg.NummesCP),val(vNumMes1),val(vnuMmes2)) .AND.;
	               SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
	               Compag.CodFTE=ALLTRIM(vCodFte) .AND. ;
	               iif(alltrim(vTipFun)='F',Maereg.PARTRET=vcodpart,MaeReg.PARTRET=vcodpart) AND ;
	               SUBSTR(Compag.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                   Maereg.estado#'99' AND :
                   iif(!EMPTY(vCodPry),SUBSTR(Compag.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                   iif(!EMPTY(vCodSpy),SUBSTR(Compag.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND ;
                   !EMPTY(Maereg.NUMCP) AND ;
                   IIF(!empty(vcodact),SUBSTR(Compag.CODCAL,13,2)=vcodact,.t.) AND ;
                   COMPAG.tipfun = alltrim(vtipfun) AND ;
                   Maereg.periodo=alltrim(vperiodO) and ;
                   Hoja.Estado="52" and ;
                   Maereg.reten >0 AND ;
                   (!EMPTY(Maereg.nummeshc) AND !EMPTY(Maereg.Numhc))
    ELSE
	    SET FILTER TO between(val(Maereg.Nummescp),val(vNumMes1),val(vnuMmes2))  .AND.
	              SUBSTR(cOMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) AND;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. 
                  SUBSTR(Compag.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  Maereg.estado#'99' AND 
                  iif(!EMPTY(vCodPry),SUBSTR(Compag.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) and ;
                  iif(!EMPTY(vCodSpy),SUBSTR(Compag.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND 
                  IIF(!empty(vcodact),SUBSTR(Compag.CODCAL,13,2)=vcodact,.t.)  AND 
                  !EMPTY(Maereg.NUMCP) AND 
                  COMPAG.tipfun = alltrim(vtipfun) AND 
                  Maereg.periodo=alltrim(vperiodO) AND 
                  between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2)) and
                  Hoja.Estado="52" and
                  Maereg.reten >0 and 
                  (!EMPTY(Maereg.nummeshc) AND 
                  !EMPTY(Maereg.Numhc))
    ENDIF 
    
    SELE Maereg
    GO TOP
    
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
           	repla fecref with compag.feccp,
           	     cODANAL WITH M.PARTRET, 
           	      NUMMES WITH Compag.NUMMES,
           	      tipdoc with 'C/P',
           	      numref with m.NumCp,
           	      codcal with m.codcal,
           	      valdeb with m.importe,
           	      valhab with m.importe,
           	        TIPO WITH '2',
           	      DESCRI WITH 'REG - '+FTE(),
           	      tipfte with compag.docref,
           	      numfte with compag.numref,
           	      TIPOPE WITH 1,
           	      mesfte WITH COMPAG.NUMMESHC,
           	      codprg with SUBSTR(codcal,8,2),
           	    codsubpr with substr(codcal,10,3),
           	     Codproy with substr(codcal,13,3),
           	    codsupry with substr(codcal,16,2),
           	       TOTCP WITH m.importe-m.Reten
          else
      	repla fecref with compag.feccp,
           CODPART WITH M.PARTRET, 
           	NUMMES WITH Compag.NUMMES,
           	tipdoc with 'C/P',
           	numref with m.NumCp,
           	codcal with m.codcal,
           	valdeb with m.importe,
           	valhab with m.importe,
              TIPO WITH '2',
           	DESCRI WITH 'REG - '+FTE(),
           	tipfte with compag.docref,
           	numfte with compag.numref,
           	TIPOPE WITH 1,
           	mesfte WITH COMPAG.NUMMESHC,
           	codprg with SUBSTR(codcal,8,2),
          codsubpr with substr(codcal,10,3),
           	codact with substr(codcal,13,2),
             TOTCP WITH m.importe-m.Reten
          endif
       ENDIF
       UNLOCK
       SELEC Maereg
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN    
    
               
    SET RELA TO
    SELE MAEREG
    SET INDEX TO
    ERASE (wINDi)
    
	SELE Hoja
	SET RELATION OFF INTO Maereg
	SET RELATION OFF INTO Compag 
	
    SELE Maereg
    SET RELATION OFF INTO Compag
    
  	USE IN 1   
  	USE IN 5   
	USE IN 2   
	*-------------------------------------------------------
    *-------------------------
    *Regularizaciones Sin Retencion 
    *----------------------------------
    
  	USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
  	USE ComPag   IN 5   ORDER TAG ComPag1   ALIAS ComPag
	USE MaeReg   IN 2   ORDER TAG Maereg2   ALIAS MaeReg
	USE IteReg   IN 6   ORDER TAG Itereg1   ALIAS IteReg
	
	wINDi=SYS(3)+'.IDX'
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
	SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDI
	
    SELE Itereg    
    SET RELATION TO NUMMES+NUMero         INTO Maereg  ADDI

    
    SELE IteReg
    
    IF !EMPTY(vcodpart)                
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. 
	              SUBSTR(COMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  COMPAG.CodFTE=ALLTRIM(vCodFte) .AND. 
                  iif(alltrim(vTipFun)='F',IteREg.CODANAL=vcodpart,IteREg.CODPART=vcodpart) AND 
                  SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND 
                  iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND !EMPTY(COMPAG.NUMCP) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO) and
                  Hoja.Estado="52" and 
                  Maereg,Reten =0 AND 
                  (!EMPTY(Maereg.nummeshc) AND 
                  !EMPTY(Maereg.Numhc))
    ELSE
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. 
	    			SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
 	                Compag.CodFTE=ALLTRIM(vCodFte) .AND. 
 	                SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                    COMPAG.estado#'99' AND 
                    iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                    iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND 
                    IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.)  AND 
                    !EMPTY(COMPAG.NUMCP) AND 
                    COMPAG.TIPDOC$'HCRG' AND 
                    COMPAG.tipfun = alltrim(vtipfun) AND 
                    COMPAG.periodo=alltrim(vperiodO) and
                    Hoja.Estado="52" and 
                    Maereg,Reten =0 AND 
                    (!EMPTY(Maereg.nummeshc) AND 
                    !EMPTY(Maereg.Numhc))
    ENDIF 
    
    SELE IteREg
    
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
             	repla fecref with compag.feccp,
             	     cODANAL WITH M.CODANAL,
             	     CODPART WITH M.CODPART,
             	      NUMMES WITH Compag.NUMMES,
             	      tipdoc with 'C/P',
             	      numref with m.NumCp,
             	      codcal with m.codcal,
             	      valdeb with m.impparc,
             	      valhab with m.impparc,
             	        TIPO WITH '2',
             	      DESCRI WITH 'REG -'+FTE(),
             	      tipfte with compag.docref,
             	      numfte with compag.numref,
             	      TIPOPE WITH 1,
             	      mesfte WITH Maereg.NUMMESHC,
             	      codprg with SUBSTR(codcal,8,2),
             	    codsubpr with substr(codcal,10,3),
             	     Codproy with substr(codcal,13,3),
             	    codsupry with substr(codcal,16,2),
             	       TOTCP WITH m.impparc
          else
          	repla     fecref with compag.feccp,
          	         cODANAL WITH M.CODANAL,
          	         CODPART WITH M.CODPART,
          	          NUMMES WITH Compag.NUMMES,
          	          tipdoc with 'C/P',
          	          numref with m.NumCp,
          	          codcal with m.codcal,
          	          valdeb with m.impparc,
          	          valhab with m.impparc,
          	            TIPO WITH '2',
          	          DESCRI WITH 'REG -'+FTE(),
          	          tipfte with compag.docref,
          	          numfte with compag.numref,
          	          TIPOPE WITH 1,
          	          mesfte WITH maereg.NUMMESHC,
          	          codprg with SUBSTR(codcal,8,2),
          	        codsubpr with substr(codcal,10,3),
          	          codact with substr(codcal,13,2),
          	           TOTCP WITH m.impparc
          endif
       ENDIF
       UNLOCK
       SELEC IteREg
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN    
    SET RELA TO
	SELE Maereg
    SET RELATION OFF  INTO Compag
    SELE MAEREG
    SET INDEX TO
    ERASE (wINDi)
	SELE Hoja
	SET RELATION OFF INTO Maereg
	SET RELATION OFF INTO Compag ADI
	
  	USE IN 1   
  	USE IN 5   
	USE IN 2   
	USE IN 6
	*-------------------------------------------------------
    * DE COMPROBANTES DE PAGO
    * Cuando el Comprobante de Pago 
    * ose Compag.Reten=0
    *-----------------------
    USE HojCon   IN 1   ORDER TAG HojCon1  ALIAS Hoja
	USE ComPag   IN 5   ORDER TAG ComPag1  ALIAS ComPag
	USE IteCP    IN 6   ORDER TAG IteCP1   ALIAS IteCP
	
	SELE Hoja 
	SET RELATION TO NUMMEScp+NUMCP+CODCTC INTO IteCp
    SET RELATION TO NUMMEScp+NUMCP+CODCTC INTO COMPAG ADDITIVE
    
  	SELE IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG  additive

*   SELE IteCP
    IF !EMPTY(vcodpart)                &&Nummeshc
    
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(COMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  COMPAG.CodFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',ITECP.CODANAL=vcodpart,ITECP.CODPART=vcodpart) AND SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND !EMPTY(COMPAG.NUMCP) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO) ;
                  and Hoja.Estado="50" AND (!EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc))
    ELSE
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.)  AND !EMPTY(COMPAG.NUMCP) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO);
                  and Hoja.Estado="50" AND (!EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc))
    ENDIF 
    
*   SELE IteCP
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	repla fecref with compag.feccp,cODANAL WITH M.CODANAL,;
          	      CODPART WITH M.CODPART, NUMMES WITH Compag.NUMMES,;
          	      tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,;
          	      valdeb with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),valhab with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),TIPO WITH '2',;
          	      DESCRI WITH FTE(),tipfte with compag.docref,numfte with compag.numref,;
          	      TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc)
          else
          	repla fecref with compag.feccp,cODANAL WITH M.CODANAL,CODPART WITH M.CODPART, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,TIPO WITH '2',DESCRI WITH FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2)
          	REPLA valdeb with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),valhab with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),TOTCP WITH IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc)
          endif
       ENDIF
       UNLOCK
       SELE IteCP
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN    
               
    SET RELA TO
	SELE Hoja 
	SET RELATION OFF INTO IteCp
    SET RELATION OFF INTO COMPAG ADDI
    
  	SELE IteCp
    SET RELATION OFF INTO COMPAG ADDI
    

    USE IN 5
    USE IN 6
    USE IN 1
