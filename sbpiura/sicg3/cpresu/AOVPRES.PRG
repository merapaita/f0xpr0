* ---------------------------------------------------
** REPOE5.PRG
** Reporte Auxiliar de Compromisos por partidas
** ---------------------------------------------------
* Abrir Archivos de trabajo

CLOSE DATA
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
*USE ComPag   IN 4   order tag ComPag1  ALIAS ComPag
*USE IteCP    IN 5   ORDER TAG IteCP1   ALIAS IteCP
USE Calen    IN 6   order tag calen1   ALIAS calen
*USE Maepre   IN 7   order tag Maepre1  ALIAS maepre
USE itepar   In 8   order tag itepar1  ALIAS Itepar 
*USE hojmod   IN 9   order tag hojmod1  ALIAS HOJMOD
*USE PteAnu   IN 10  order tag PteAnu9  ALIAS AnuPA
USE Astaux_P IN 11                     ALIAS AST_P
USE Astaux   IN 12                     ALIAS AST
*USE Reten    IN 13  order tag Reten1   ALIAS Reten
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
USE Promae   IN 15  order tag Promae1  ALIAS Promae
USE cheque   IN 16  order tag Cheque1  ALIAS Cheque

public v1,v2,v3,A1,A2,A3,A4,A5,A6,A7,A8,v11,v12
DO Inicia
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------
IF EOF()
   DO standby WITH 'No Existe registros a Procesar'
   RETURN
ENDIF
vtemp    = RECNO()
as=order()
vTipfun   = ' '
vPeriodo  = '  '
vNumMes1  = '  '
vNumMes2  = '  '
vCodPrg   = '  '
vCodFte   = '   '
vCodSub   = '   '
vCodAct   = '  '
vSubpry   = '   '
vProyec   = '   '


 DEFINE WINDOW Lis FROM 5,12 TO 17,70 DOUBLE ;
 TITLE ' °° Movimientos Presupuestales °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '        Mes del : '
 @  2,25 say 'al : '
 @  3,2 say '           Tipo : ' 
 @  4,2 say '         Fuente : '
 @  5,2 say '       Programa : '
 @  6,2 say '    SubPrograma : '
 @  7,2 say '       Proyecto : '
 @  8,2 say '    SubProyecto : '
 @  9,2 say '      Actividad : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes1   PICTURE '!!'  VALID Val_Para(vNumMes1 ,'FECMES','C',21,30,4) AND !EMPTY(vNumMes1)
 @  2,31  GET vNumMes2   PICTURE '!!'  VALID Val_Para(vNumMes2 ,'FECMES','C',31,20,4) AND !EMPTY(vNumMes2) 
 @  3,21  GET vtipfun    PICTURE '!'   VALID VAL_PARA(vTiPFun  ,'TIPFUN',' ',21,30) 
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30,4) AND !EMPTY(vCodFte) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30,4) AND !EMPTY(vCodPrg) 
 @  6,21  GET vCodSub    PICTURE '!!!' VALID IIF(!EMPTY(vCodSub),VAL_SUBP(vCodSub,'CODSUB'+VCodPrg,' ',21,28),.t.) &&OR EMPTY(vCodPrg)
 @  7,21  GET vProyec    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I'
 @  8,21  GET vSubpry    PICTURE '!!' WHEN ALLTRIM(VTIPFUN)='I'
 @  9,21  GET vCodAct    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'
 
 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF EMPTY(vNumMES1) OR EMPTY(vNumMES2) .or. LASTKEY()=27
    RETURN
 ELSE
    ACTIVATE WINDOW STANDBY
    @ 1,14 SAY 'Espere un Momento ....' color W*
    m.tipfun = alltrim(vtipfun)
    m.periodo = alltrim(vperiodo)
    m.codfte = alltrim(vcodfte)

    SELECT AST
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 12  ALIAS AST EXCLU
    
    SELECT AST_P
    vdbf = sys(3)+'.dbf'
    xidx = sys(3)+'.idx'
    copy stru to (vdbf)
    USE (VDBF) IN 11  ALIAS AST_P EXCLU
    
    SELECT AST_P
    if ALLTRIM(vtipfun) = 'I'
       INDEX ON CODSUBPR+CODPROY+CODSUPRY+CodPart TO (xidx)
    ELSE   
       INDEX ON CODSUBPR+CODACT+codanal TO (xidx)
    ENDIF   

  
    VIND1=SYS(3)+'.IDX'
    VIND2=SYS(3)+'.IDX'
    VIND3=SYS(3)+'.IDX'
    VIND4=SYS(3)+'.IDX'
    VIND5=SYS(3)+'.IDX'                
    
    IF ALLTRIM(vtipfun)='F'
		xvalor = vperiodo+alltrim(vcodprg)+alltrim(vcodsub)              
		proact = alltrim(vcodact)
	ELSE	
		xvalor = vperiodo+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec)
		proact = alltrim(vSubpry)
	ENDIF
    
    ******** AFECTACIONES ********
    * INCLUYE=>H/C , P/A, P/R, H/M
    ******************************    
    
    SELEC ITEHC
	IF ALLTRIM(vtipfun)='I'
	   SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,itehc.tipdoc,;
		      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL, ITEHC.NUMMESHM, itehc.numhm, ITEHC.MESPA,ITEHC.NUMPA,ITEHC.MESPR,ITEHC.NUMPR,ITEHC.OPERAC,ITEHC.TIPCOM,ITEHC.MESOCOS ;
			  FROM ITEHC ;
			  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
	  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND  ITEHC.ESTADO#'99' AND ITEHC.TIPFUN='I' AND BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND IIF(!EMPTY(MESPA),VAL(MESPA)=VAL(vnummes2),.t.) and IIF(!EMPTY(MESPR),VAL(MESPR)=VAL(vnummes2),.t.) and IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.) ;
					INTO CURSOR Hojax
	ELSE
	 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART, itehc.tipdoc, ;
	     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm, ITEHC.MESPA,ITEHC.NUMPA,ITEHC.MESPR,ITEHC.NUMPR ,ITEHC.OPERAC,ITEHC.TIPCOM,ITEHC.MESOCOS;
	           FROM ITEHC ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ITEHC.ESTADO#'99' AND ITEHC.TIPFUN='F' AND BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND IIF(!EMPTY(MESPA),VAL(MESPA)=VAL(vnummes2),.t.) and IIF(!EMPTY(MESPR),VAL(MESPR)=VAL(vnummes2),.t.) and IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.);
					INTO CURSOR HOJAX
	ENDIF

	vInd = SYS(3) + '.DBF'
	jInd = SYS(3) + '.IDX'
	COPY TO (vInd)
	use (vind) in 24 alias hoja1 exclu
	select hoja1
	
	INDEX ON NUMMES+NUMHC TO (JIND) 
	set rela to NUMMES+NUMHC into hoja
	
    GO TOP   
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla Nummes WITH M.NUMMES,;
                tipdoc with iif(m.tipdoc='P/A','P/A',iif(m.tipdoc='P/R','P/R',iif(m.tipdoc='H/M','H/M','H/C'))),;
                numref with iif(m.tipdoc='P/A',m.NumPA,iif(m.tipdoc='P/R',m.NumPr,iif(m.tipdoc='H/M',m.NumHm,m.numhc))),;
                codprg with m.codprg,;
                codsubpr with m.Codsubpr,;
                codproy with iif(vtipfun='I',m.codproy,'   '),;
                codsupry with iif(vtipfun='I',m.codsupry,'   '),;
                codact with iif(vtipfun='F',m.codact,'  '),;
                valpart with m.valpart,;
                mesocos with m.mesocos,;
                mesPr with m.mespr,;
                mesPa with m.mespa,;
                tipo with iif(m.tipdoc='P/A','4',iif(m.tipdoc='P/R','5',iif(m.tipdoc='H/M','3','1'))),;
                tipfte with hoja.tipDOC,;
                fecref with hoja.fechc,;
                codcal with hoja.codcal,;
                numfte with HOJA.NUMref,;
                mesfte with iif(m.tipdoc='P/',m.mesocos,IIF(m.tipdoc='H/M',m.Nummeshm,m.nummes)),;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with iif(m.tipdoc='P/A',-1,iif(m.tipdoc='P/R',-1,iif(m.tipdoc='H/M',IIF(m.tipope='-',-1,1),1)))
                IF alltrim(vtipfun)='F'
	                replace Codanal with m.CodAnal
	            else    
                	replace Codpart WITH M.CodPart
                endif	
       ENDIF
       UNLOCK
       IF !EMPTY(m.numpa) and mespa <= alltrim(vnummes2)
          IF F_APPD()
          repla Nummes WITH M.NUMMES,;
                tipdoc with 'P/A',;
                numref with m.NumPA,;
                codprg with m.codprg,;
                codsubpr with m.Codsubpr,;
                codproy with iif(vtipfun='I',m.codproy,'   '),;
                codsupry with iif(vtipfun='I',m.codsupry,'   '),;
                codact with iif(vtipfun='F',m.codact,'  '),;
                valpart with m.valpart,;
                mesocos with m.mesocos,;
                mesPa with m.mespa,;
                TIPO WITH '4',;
                tipfte with hoja.tipDOC,;
                fecref with hoja.fechc,;
                codcal with hoja.codcal,;
                numfte with HOJA.NUMref,;
                mesfte with iif(m.tipdoc='P/',m.mesocos,hoja.nummes),;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with -1
                IF alltrim(vtipfun)='F'
	                replace Codanal with m.CodAnal
	            else    
                	replace Codpart WITH M.CodPart
                endif	
	       ENDIF
    	   UNLOCK
       endif
       SELEC HOJA1
    ENDSCAN 
    SET RELATION TO 
        
    ************************
    * DE COMPROBANTE DE PAGO
    ************************
    USE IN 2
    *USE IN 8
    USE IN 9
    USE IN 10
    USE IN 24
    
    USE ComPag   IN 4   order tag ComPag1  ALIAS ComPag
    USE IteCP    IN 5   ORDER TAG IteCP1   ALIAS IteCP

    SELECT IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG  additive
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO CHEQUE  additive
    
    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(COMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  SUBSTR(COMPAG.CodCal,5,3)=ALLTRIM(vCodFte) .AND.  SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSub) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vProyec),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vProyec),.t.) .and. ;
                  iif(!EMPTY(vSubPry),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vSubpry),.t.) AND ;
                  !EMPTY(COMPAG.NUMCP) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.) AND COMPAG.TIPDOC$'HCRG' AND ;
                  COMPAG.TIPFUN=ALLTRIM(VTIPFUN) AND COMPAG.periodo=alltrim(vperiodO) AND;
                  !EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc) AND CHEQUE.ESTADO#'99' AND IIF(CHEQUE.ESTADO='99',CHEQUE.NUMMESHA=ALLTRIM(VNUMMES2),.T.) && AND Hoja.Estado="50" 

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with compag.feccp,;
          		cODANAL WITH M.CODANAL,;
          		CODPART WITH M.CODPART,;
          		NUMMES WITH M.NUMMES,;
          		tipdoc with 'C/P',;
          		numref with m.NumCp,;
          		codcal with m.codcal,;
          		valpart with IIF(COMPAG.PARTRET=m.CodAnal,m.impparc-compag.Reten,m.impparc),;
         		TIPO WITH '2',;
          		tipfte with compag.docref,;
          		numfte with compag.numref,;
          		MESFTE WITH COMPAG.NUMMESHC,;
          		codprg with SUBSTR(COMPAG.CodCal,8,2),;
          		codsubpr with SUBSTR(COMPAG.CodCal,10,3)
          IF ALLTRIM(m.tipfun)='I'
             replace codproy with SUBSTR(COMPAG.CodCal,13,3),codsupry with SUBSTR(COMPAG.CodCal,16,2)
          else
             replace codact with SUBSTR(COMPAG.CodCal,13,2)
          endif   
       ELSE
       ENDIF
       
       UNLOCK
       SELEC ITECP
    ENDSCAN               
    SET RELATION TO
    SELECT HOJA
    SET RELATION TO               
    
    ****************
    * DE RETENCIONES
    ****************
    
    USE Reten    IN 13  order tag Reten1   ALIAS Reten
    
    SELE RETEN
    SET RELATION TO NUMMES+NUMCP+CODCTC  INTO Compag  
	SET RELATION TO NUMMESHC+NUMHC 		 INTO hoja   ADDITIVE
    SET RELATION TO NUMMES+NUMCP+CODCTC  INTO CHEQUE  additive
        
    SET FILTER TO Reten.periodo=alltrim(vperiodO) AND;
                  between(val(Reten.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. ;
                  between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2)) and ;	                  
                  Compag.CodFTE = ALLTRIM(vCodFte) .AND. ;
	              SUBSTR(reten.CODCAL, 8,2)=ALLTRIM(vCodPrg) AND;                      
                  SUBSTR(Reten.CodCal,10,3)=alltrim(vCodSub) .and. ;
                  iif(!EMPTY(vProyec),SUBSTR(Reten.CODCAL,13,3)=ALLTRIM(vProyec),.t.) and ;
                  iif(!EMPTY(vSubpry),SUBSTR(Reten.CODCAL,16,2)=ALLTRIM(vSubpry),.t.) AND ;
                  IIF(!empty(vcodact),SUBSTR(Reten.CODCAL,13,2)=vcodact,.t.) AND ;
                  COMPAG.tipfun = alltrim(vtipfun) AND ;                      
                  !EMPTY(Reten.NUMCP) AND ;
                  Reten.estado#'99' AND ;                      
                  Reten.Conpago='û' AND ;
                  Compag.Reten>0 and ;
                  (Hoja.Estado="50" OR Hoja.Estado="52") AND ;
                  !EMPTY(Reten.nummeshc) AND !EMPTY(Reten.Numhc) AND CHEQUE.ESTADO#'99' AND IIF(CHEQUE.ESTADO='99',CHEQUE.NUMMESHA=ALLTRIM(VNUMMES2),.T.) 
    
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
          	         valpart with m.valret,;
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
          	          valpart with m.valret,;
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
    ENDSCAN 
    SET FILTER TO
    SET RELATION TO
    
    *************************************
    * DE REGULARIZACIONES CON RETENCIONES
    *************************************
    
    USE IN 13
    USE MaeReg   IN 9   ORDER TAG Maereg2   ALIAS MaeReg
	
	Windi=SYS(3)+'.IDX'
	
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
    SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO CHEQUE  additive
    
	    SET FILTER TO between(val(Maereg.Nummescp),val(vNumMes1),val(vnuMmes2))  .AND. ;
	              SUBSTR(cOMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) AND;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. ;
                  SUBSTR(Compag.CodCal,10,3)=alltrim(vCodSub) .and. ;
                  Maereg.estado#'99' AND ;
                  iif(!EMPTY(vProyec),SUBSTR(Compag.CODCAL,13,3)=ALLTRIM(vProyec),.t.) and ;
                  iif(!EMPTY(vSubPry),SUBSTR(Compag.CODCAL,16,2)=ALLTRIM(vSubPry),.t.) AND ;
                  IIF(!empty(vcodact),SUBSTR(Compag.CODCAL,13,2)=vcodact,.t.)  AND ;
                  !EMPTY(Maereg.NUMCP) AND ;
                  COMPAG.tipfun = alltrim(vtipfun) AND ;
                  Maereg.periodo=alltrim(vperiodO) AND ;
                  between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2)) and ;
                  Hoja.Estado="52" and ;
                  Maereg.reten >0 and ;
                  (!EMPTY(Maereg.nummeshc) AND ;
                  !EMPTY(Maereg.Numhc)) AND CHEQUE.ESTADO#'99' AND AND IIF(CHEQUE.ESTADO='99',CHEQUE.NUMMESHA=ALLTRIM(VNUMMES2),.T.) 
    
    SELE Maereg
    GO TOP
    
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
           	repla fecref with compag.feccp,;
           	     cODANAL WITH M.PARTRET, ;
           	      NUMMES WITH Compag.NUMMES,;
           	      tipdoc with 'C/P',;
           	      numref with m.NumCp,;
           	      codcal with m.codcal,;
           	      valpart with m.importe,;
           	        TIPO WITH '2',;
           	      DESCRI WITH 'REG - '+FTE(),;
           	      tipfte with compag.docref,;
           	      numfte with compag.numref,;
           	      TIPOPE WITH 1,;
           	      mesfte WITH COMPAG.NUMMESHC,;
           	      codprg with SUBSTR(codcal,8,2),;
           	    codsubpr with substr(codcal,10,3),;
           	     Codproy with substr(codcal,13,3),;
           	    codsupry with substr(codcal,16,2),;
           	       TOTCP WITH m.importe-m.Reten
          else
      	repla fecref with compag.feccp,;
           CODPART WITH M.PARTRET, ;
           	NUMMES WITH Compag.NUMMES,;
           	tipdoc with 'C/P',;
           	numref with m.NumCp,;
           	codcal with m.codcal,;
           	valpart with m.importe,;
              TIPO WITH '2',;
           	DESCRI WITH 'REG - '+FTE(),;
           	tipfte with compag.docref,;
           	numfte with compag.numref,;
           	TIPOPE WITH 1,;
           	mesfte WITH COMPAG.NUMMESHC,;
           	codprg with SUBSTR(codcal,8,2),;
          codsubpr with substr(codcal,10,3),;
           	codact with substr(codcal,13,2),;
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
    
    *************************************
    * DE REGULARIZACIONES SIN RETENCIONES
    *************************************
  
  	USE IteReg   IN 10   ORDER TAG Itereg1   ALIAS IteReg
	
	WINDi=SYS(3)+'.IDX'
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
	SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDI
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO CHEQUE  additive	
    SELE Itereg    
    SET RELATION TO NUMMES+NUMero         INTO Maereg  ADDI

    
    SELE IteReg
    
    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. ;
	    			SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
 	                Compag.CodFTE=ALLTRIM(vCodFte) .AND. ;
 	                SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSub) .and. ;
                    COMPAG.estado#'99' AND ;
                    iif(!EMPTY(vProyec),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vProyec),.t.) .and. ;
                    iif(!EMPTY(vSubPry),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vSubPry),.t.) AND ;
                    IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.)  AND ;
                    !EMPTY(COMPAG.NUMCP) AND ;
                    COMPAG.TIPDOC$'HCRG' AND ;
                    COMPAG.tipfun = alltrim(vtipfun) AND ;
                    COMPAG.periodo=alltrim(vperiodO) and;
                    Hoja.Estado="52" and ;
                    Maereg,Reten =0 AND ;
                    (!EMPTY(Maereg.nummeshc) AND ;
                    !EMPTY(Maereg.Numhc)) AND CHEQUE.ESTADO#'99' AND AND IIF(CHEQUE.ESTADO='99',CHEQUE.NUMMESHA=ALLTRIM(VNUMMES2),.T.) 
    SELE IteREg
    
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
             	repla fecref with compag.feccp,;
             	     cODANAL WITH M.CODANAL,;
             	     CODPART WITH M.CODPART,;
             	      NUMMES WITH Compag.NUMMES,;
             	      tipdoc with 'C/P',;
             	      numref with m.NumCp,;
             	      codcal with m.codcal,;
             	      valpart with m.impparc,;
             	        TIPO WITH '2',;
             	      DESCRI WITH 'REG -'+FTE(),;
             	      tipfte with compag.docref,;
             	      numfte with compag.numref,;
             	      TIPOPE WITH 1,;
             	      mesfte WITH Maereg.NUMMESHC,;
             	      codprg with SUBSTR(codcal,8,2),;
             	    codsubpr with substr(codcal,10,3),;
             	     Codproy with substr(codcal,13,3),;
             	    codsupry with substr(codcal,16,2),;
             	       TOTCP WITH m.impparc
          else
          	repla     fecref with compag.feccp,;
          	         cODANAL WITH M.CODANAL,;
          	         CODPART WITH M.CODPART,;
          	          NUMMES WITH Compag.NUMMES,;
          	          tipdoc with 'C/P',;
          	          numref with m.NumCp,;
          	          codcal with m.codcal,;
          	          valpart with m.impparc,;
          	            TIPO WITH '2',;
          	          DESCRI WITH 'REG -'+FTE(),;
          	          tipfte with compag.docref,;
          	          numfte with compag.numref,;
          	          TIPOPE WITH 1,;
          	          mesfte WITH maereg.NUMMESHC,;
          	          codprg with SUBSTR(codcal,8,2),;
          	        codsubpr with substr(codcal,10,3),;
          	          codact with substr(codcal,13,2),;
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
    SELECT MAEREG
    SET INDEX TO
    ERASE (wINDi)
	SELECT Hoja
	SET RELATION OFF INTO Maereg
	SET RELATION OFF INTO Compag ADI
	
    *********************************************
    * RESUMEN EN EL ARCHIVO
    *********************************************
    USE IN 5
    USE IN 6
    *USE IN 8
    USE IN 9
    USE IN 10
    USE IN 13
    USE IN 14
    USE IN 15
    SELE AST
    GO TOP
	IF !EOF()
    	 vTotAfe = 0
         GO TOP                                     
         SCAN 
	    	vPart1 = iif(ALLTRIM(vtipfun) = 'I',AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+AST.CodPart,AST.CODSUBPR+AST.CODACT+ast.codanal)
		    vPart2 = iif(ALLTRIM(vtipfun) = 'I',AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+AST.CodPart,AST.CODSUBPR+AST.CODACT+ast.codanal)
    	    vComBru= 0
    	    vComLiqac= 0
    	    vEjebru = 0
    	    vEjereb = 0
    	    vEjeLiqac = 0
    	    vCompBru  = 0
       		vComLiqac = 0
	   		vrebmes = 0
	   		vrebant = 0
        	DO WHILE  vPart1 = vPart2
               do case
                  case AST.TIPO='1'   && DE H/C
                       vcompbru = vcompbru + IIF(ast.nummes=alltrim(vnummes2),ast.valpart,0)
                       vcomliqac = vcomliqac + ast.valpart
                  case AST.TIPO='2'   && DE C/P               
                       vejebru =  vejebru + IIF(ast.nummes=alltrim(vnummes2),ast.valpart,0)
                       vejeliqac =  vejeliqac + IIF(ast.nummes<=alltrim(vnummes2),ast.valpart,0)
                  case AST.TIPO='3'   && DE H/M
                       vrebmes  = vrebmes  + iif(ast.tipcom='C' and ast.tipope=-1,IIF(ast.nummes=alltrim(vnummes2),ast.valpart,0),0)
                       vrebant =  vrebant  + iif(ast.tipcom='C' and ast.Nummes#alltrim(vnummes2),IIF(ast.mesfte=alltrim(vnummes2),ast.valpart,0),0)
                       vcompbru = vcompbru + IIF(ast.nummes=alltrim(vnummes2) and ast.tipope=1 and ast.tipcom='C',ast.valpart,0)
                       vcomliqac = vcomliqac + iif(ast.tipcom='C',ast.tipope*ast.valpart,0)
                       vejebru =  vejebru + iif((ast.tipcom='P' OR ast.tipcom='CYP') AND ast.nummes#alltrim(vnummes2) AND ast.tipope=1 ,ast.valpart,0)
                       vejereb  = vejereb  + iif((ast.tipcom='P' OR ast.tipcom='CYP') and ast.nummes=alltrim(vnummes2) AND ast.tipope=-1 ,ast.valpart,0)
                       vejeliqac =  vejeliqac + iif((ast.tipcom='P' OR ast.tipcom='CYP') AND ast.nummes#alltrim(vnummes2) AND ast.tipope=-1 ,ast.valpart*-1,0)
                  case AST.TIPO='4'   && DE P/A 
                       vrebmes =  vrebmes + IIF(ast.mesPA=alltrim(vnummes2),ast.valpart,0)
                       vrebant =  vrebant + IIF(ast.mesPA#alltrim(vnummes2),ast.valpart,0)
                       vcomliqac = vcomliqac + ast.tipope*ast.valpart
                  case AST.TIPO='5'   && DE  P/R
                       vrebmes =  vrebmes + IIF(ast.mesPR=alltrim(vnummes2),ast.valpart,0)
                       vrebant =  vrebant + IIF(ast.mesPR#alltrim(vnummes2),ast.valpart,0)
                       vcomliqac = vcomliqac + ast.tipope*ast.valpart
            endcase   		                 		                 		                     
	        SKIP
	        vPart1 = iif(ALLTRIM(vtipfun) = 'I',AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+AST.CodPart,AST.CODSUBPR+AST.CODACT+ast.codanal)
	        ENDD
	        SKIP -1
		    sele ast_p
		    seek iif(ALLTRIM(m.tipfun) = 'I',AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+AST.CodPart,AST.CODSUBPR+AST.CODACT+ast.codanal)
		    if found()
		       if rlock()
                 replace comPbru with compBru + vCompbru,; 
                         rebmes  with rebmes + vRebmes ,;
                         rebant  with rebant + vRebAnt,;
                         comliq  with comliq + vCompBru - (vRebmes + vRebant),;
                       comliqac  with comliqac + vcomliqac ,;
                         Ejebru  with Ejebru + vejebru ,;
                         EjeReb  with EjeReb + vejereb ,;
                         EjeLiq  with Ejeliq + vEjebru - vEjereb,;
                       ejeliqac  with Ejeliqac + vEjeliqac - vEjereb 
			   endif
			   unlock	                     
            else
               if f_appd()
                  replace codpart with ast.codpart,;
                          codanal with ast.codanal,;
                          comPbru with vCompbru,; 
                          rebmes  with vRebmes ,;
                          rebant  with vRebAnt,; 
                          comliq  with vCompBru - (vRebmes + vRebant),;
                        comliqac  with vcomliqac ,;
                          Ejebru  with vejebru ,;
                          EjeReb  with vejereb ,;
                          EjeLiq  with vEjebru - vEjereb,;
                        ejeliqac  with vEjeliqac - vEjereb ,;
                     	  codprg  with AST.codprg,;
          		        codsubpr  with AST.Codsubpr,;
                         codproy  with AST.codproy,;
                        codsupry  with AST.codsupry,;
				          codact  with AST.codact   
               endif   
               unlock
	        endif   
	        SELE ast
 	   ENDSCAN
    else
       do standby with 'No hay registros para pocesar..'
    endiF
    
    QIDX = sys(3)+'.IDX'
    SELECT AST_p
    SET UNIQ ON
    if ALLTRIM(vtipfun) = 'I'
       INDEX ON CODSUBPR+CODPROY+CODSUPRY+LEFT(CodPart,2) TO (Qidx)
    ELSE   
       INDEX ON CODSUBPR+CODACT+LEFT(codanal,2) TO (Qidx)
    ENDIF   

    Qdbf = sys(3)+'.dbf'
    copy to (Qdbf)
    USE (QDBF) IN 20  ALIAS AST_A
    SET UNIQ OFF
    SELECT AST_p 
    set index to (XIDX)

    SELECT AST_A

    GO TOP
    ***********************************
    * AGREGA '00'
    ***********************************
    SCAN 
        vp1 = iif(ALLTRIM(vtipfun) = 'I',AST_A.CODSUBPR+AST_A.CODPROY+AST_A.CODSUPRY+left(ast_a.codpart,2),AST_A.CODSUBPR+AST_A.CODACT+left(ast_A.codanal,2))
        SELE AST_P
	   	SUM CompBru,Rebmes,Rebant,Comliq,Comliqac,Ejebru,Ejereb,Ejeliq,Ejeliqac TO A1,A2,A3,A4,A5,A6,A7,A8,A9 FOR iif(ALLTRIM(vtipfun) = 'I',AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+LEFT(AST_P.CODPART,2),AST_P.CODSUBPR+AST_P.CODACT+LEFT(AST_P.CODANAL,2)) = VP1
	   	IF F_APPD()
	   	   if ALLTRIM(vtipfun) = 'I'
	   	      repla codpart with left(ast_a.codpart,2)+'.00'
	   	   else
	   	      repla codAnal with left(ast_a.codanal,2)+'.00'
	   	   endif   
           REPLACE compbru with A1,Rebmes with a2,rebant with A3,Comliq with A4,ComLiqac with A5,Ejebru with A6, Ejereb with A7 ,Ejeliq with A8, Ejeliqac with A9,;
               	   codprg  with AST_A.codprg,codsubpr  with AST_A.Codsubpr,codproy  with AST_A.codproy,codsupry  with AST_A.codsupry,codact  with AST_A.codact   
        endif
        unlock   
        sele ast_a
    ENDsca
    DEACTIVATE WINDOW STANDBY       
    sele ast_p
    USE Maepre   IN 7   order tag Maepre1  ALIAS maepre
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
	    DO CASE 
    	   CASE alltrim(vtipfun) = 'F'
		       DO REPORTE WITH 2,"MOVPREF",' Movimiento Presupuestal'    	   
	       CASE alltrim(vtipfun) = 'I'
		       DO REPORTE WITH 2,"MOVPREI",' Movimiento Presupuestal'	       
	    ENDCASE           
    ENDIF
 ENDIF
 SET FILTER TO
 SET INDEX TO
 CLOSE INDEX 
 CLOSE DATA
 ERASE (VIND1)
 ERASE (VIND2)
 ERASE (VIND3)
 ERASE (VIND4)  
RETURN

PROCEDURE FTE
*------------
DEVUL='**'
DO CASE
   CASE TIPDOC='H/C' 
        DEVUL = IIF(HOJA.TIPPRV='P',val_prv(HOJA.Codprv),IIF(HOJA.TipPRV='E',val_pro(HOJA.Codemp),HOJA.NOMBRE))
   CASE TIPDOC='C/P'
        *DEVUL = '*********'  &&COMPAG.CCNOMBRE
        DEVUL = IIF(COMPAG.TIPPRV='P',val_prv(ComPag.Codprv),IIF(COMPAG.TIPPRV='E',val_pro(compag.Codemp),'PPP'))
ENDCASE        
RETURN DEVUL                       

FUNCTION VAL_PRO
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
private medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (parameters()>=2)
mmsg   = (parameters()=4) .and.  _tipo

_campo = varread()

malias = alias()
select AUXIL
_oldwnd = woutput()

If !medita
   SET ORDE TO 2
   seek xcod
   v_fun = iif(found(),Descri,"")
*  QQ
*   if mmsg
*      @ _x,_y say descri
*   endif
else
   if empty(xcod)
      set orde to 3
      on key label ENTER keyboard chr(23)
      define window _xx from 3,22 to 22,77
      browse window _xx title ' ®Enter¯  Selecciona   ' nolgrid noedit noappend nodelete nomenu fields;
         codigo   :h='C¢digo'     ,;
         descri   :h='Nombre'
       * dirpro   :h='Direccci¢n' :25
      on key label ENTER
      release window _xx
      set order to 2
      if !empty(_oldwnd)
         activate window &_oldwnd
      endif
      if lastkey()=27
         v_fun = .f.
      else
         xcod = codigo
         if mmsg
            @ _x,_y say descri
         endif
         select (malias)
         if !_tipo
            replace &_campo with  xcod
         endif
         v_fun = .t.
      endif
   else
      seek xcod
      if mmsg .and. found()
         @ _x,_y say descri
      endif
      v_fun = found()
   endif
endif
m.Codprv='    '
select (malias)
return v_fun

PROCEDURE CALCUL
*---------------
*  ACUMULA LAS 
SELECT AST
SET UNIQ ON
IF ALLTRIM(VTIPFUN)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (VIND2) FOR TIPDOC$'H/C' 
ELSE	
   INDEX ON CODSUBPR+CODACT+CODANAL TO (VIND2) FOR TIPDOC='H/C'
ENDIF
GO TOP
SCAN 
     SELE AST_P
     IF F_appd()
      IF ALLTRIM(VTIPFUN)='F' 
         REPLACE CODANAL WITH AST.CODANAL,CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODACT WITH AST.CODACT
      ELSE   
         REPLACE CODPART WITH AST.CODPART,CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODPROY WITH AST.CODPROY,CODSUPRY WITH AST.CODSUPRY
      ENDIF   
   ENDIF 
   SELE AST
ENDSCAN
SET UNIQ OFF

*2* ACUMULA COMPROMISO BRUTO

SELE AST
INDEX ON IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+CODANAL,CODSUBPR+CODPROY+CODSUPRY+CODPART) TO (VIND3)

SELE AST_P
INDEX ON IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+CODANAL,CODSUBPR+CODPROY+CODSUPRY+CODPART) TO (VIND4)
GO TOP

*** hojas de control

SCAN  
     cod =  IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb*iif(tipOpe=-1,-1,1) to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/'
     GO TOP
     sum valdeb to valorX for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/M' AND TIPOPE=-1  AND TIPFTE='C'  AND VAL(MESFTE) # VAL(VNUMMES2) 
     GO TOP
     sum valdeb to valorZ for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC$'P/AP/R' AND TIPOPE=-1  AND TIPFTE='C'  AND VAL(MESFTE) # VAL(VNUMMES2)
     SELE AST_P                                                                                                                                                                && MESFTE         
     replace Comliqac with valor,Rebant with Rebant+valorx+valorZ

endscan

SELE AST_P
GO TOP
SCAN  
     cod =  IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb*iif(tipope=-1,0,1) to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/' and nummes=ALLTRIM(vnummes2)
     GO TOP
     sum valdeb to valory for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/M' and MESFTE=alltrim(vnummes2) and tipope = -1 AND TIPFTE='C'
     GO TOP
     sum valdeb to valorg for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC$'P/AP/R' and MESFTE=alltrim(vnummes2) and tipope = -1 AND TIPFTE='C'
     sele ast_p                                                                                                                              && MESFTE    
     replace ComPBru with valor,Rebmes with RebMes+valorY+valorG
    
endscan

GO TOP
****** comprobante de pagos

SCAN  
     cod =  IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='C/P'
     sele ast_p
     replace Ejeliq with valor
endscan

SELE AST_P
GO TOP
SCAN  
     cod =   IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='C/P' and MESFTE <= ALLTRIM(vnummes2)
     sele ast_p
     replace ejebru with valor
endscan

*------
SELE AST
SET UNIQ ON
IF ALLTRIM(VTIPFUN)='I'
	INDEX ON CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) TO (VIND2) FOR TIPDOC='H/C'
ELSE	
	INDEX ON CODSUBPR+CODACT+LEFT(CODANAL,2) TO (VIND2) FOR TIPDOC='H/C'
ENDIF
GO TOP
SCAN 
   COD_P = IIF(ALLTRIM(VTIPFUN)='F',AST.CODSUBPR+AST.CODACT+LEFT(ast.CODANAL,2),AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+LEFT(ast.CODPART,2))
   COD_X = IIF(ALLTRIM(VTIPFUN)='F',LEFT(ast.CODANAL,2),LEFT(ast.CODPART,2))
   SELE AST_P
   GO TOP
   SUM CompBru,Rebmes,Rebant,Comliqac,EJEBRU,EJEREB,Ejeliq to v1,v11,v12,v2,v21,v22,v3 for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+LEFT(CODANAL,2),CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2))=cod_p
   IF F_appd()
      IF ALLTRIM(VTIPFUN)='F' 
         REPLACE CODANAL WITH COD_X+'.00',CompBru with v1,Rebmes with v11,Rebant with v12,ComLiqac with v2,EJEBRU with v21,EJEREB with v22,Ejeliq with v3,tipo with 'X',CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODACT WITH AST.CODACT
      ELSE   
         REPLACE CODPART WITH COD_X+'.00',CompBru with v1,Rebmes with v11,Rebant with v12,ComLiqac with v2,EJEBRU with v21,EJEREB with v22,Ejeliq with v3 tipo with 'X',CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODPROY WITH AST.CODPROY,CODSUPRY WITH AST.CODSUPRY
      ENDIF   
   ENDIF 
   SELE AST
ENDSCAN
SET UNIQ OFF
RETURN


PROCEDURE SUMATOT
*----------------
GO TOP
SUM COMPBRU,REBMES,REBANT,COMLIQ,COMLIQAC,EJEBRU,EJEREB,EJELIQ TO A1,A2,A3,A4,A5,A6,A7,A8 FOR IIF(ALLTRIM(VTIPFUN)='I',RIGHT(CODPART,2)#'00',RIGHT(CODANAL,2)#'00')
GO TOP
RETURN
