* ---------------------------------------------------
** REPOE5.PRG
** Reporte Auxiliar de Compromisos por partidas
** UDEP
** Autor : fmc
** ---------------------------------------------------
* Abrir Archivos de trabajo

CLOSE DATA
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Calen    IN 6   order tag calen1   ALIAS calen
USE Maepre   IN 7   order tag Maepre1  ALIAS maepre
USE itepar   in 8   order tag itepar1  ALIAS Itepar 
USE hojmod   IN 9   order tag hojmod1  ALIAS HOJMOD
USE PteAnu   IN 10  order tag PteAnu9  ALIAS AnuPA
USE Astaux_P IN 11                     ALIAS AST_P
USE Astaux   IN 12                     ALIAS AST
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
USE Promae   IN 15  order tag Promae1  ALIAS Promae

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
vCodPart  = '     '

 DEFINE WINDOW Lis FROM 5,12 TO 18,70 DOUBLE ;
 TITLE ' °° Auxiliar de Compromisos °° ' FLOAT COLOR SCHEME 5

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
 @ 10,2 say '        Partida : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes1   PICTURE '!!'  VALID Val_Para(vNumMes1 ,'FECMES','C',21,30,4) AND !EMPTY(vNumMes1)
 @  2,31  GET vNumMes2   PICTURE '!!'  VALID Val_Para(vNumMes2 ,'FECMES','C',31,20,4) AND !EMPTY(vNumMes2) 
 @  3,21  GET vTipfun    PICTURE '!'   VALID Val_Para(vTipfun  ,'TIPFUN','C',31,20,4) 
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30,4) AND !EMPTY(vCodFte) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30,4) AND !EMPTY(vCodPrg)
 @  6,21  GET vCodSub    PICTURE '!!!' VALID IIF(EMPTY(vCodSub),.T.,VAL_SUBP(vCodSub,'CODSUB'+VCodPrg,' ',21,28))
 @  7,21  GET vProyec    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I'
 @  8,21  GET vSubpry    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='I'
 @  9,21  GET vCodact    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'
 @ 10,21  GET vCodPart   PICTURE '!!.!!'

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
	  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND  ITEHC.ESTADO#'99' AND ITEHC.TIPFUN='I' AND BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND IIF(!EMPTY(MESPA),VAL(MESPA)<=VAL(vnummes2),.t.) and IIF(!EMPTY(MESPR),VAL(MESPR)<=VAL(vnummes2),.t.) and IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.) ;
					INTO CURSOR Hojax
					
	ELSE
	 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART, itehc.tipdoc, ;
	     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm, ITEHC.MESPA,ITEHC.NUMPA,ITEHC.MESPR,ITEHC.NUMPR ,ITEHC.OPERAC,ITEHC.TIPCOM,ITEHC.MESOCOS;
	           FROM ITEHC ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ITEHC.ESTADO#'99' AND ITEHC.TIPFUN='F' AND BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND IIF(!EMPTY(MESPA),VAL(MESPA)<=VAL(vnummes2),.t.) and IIF(!EMPTY(MESPR),VAL(MESPR)<=VAL(vnummes2),.t.) and IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.);
					INTO CURSOR HOJAX
	ENDIF

	vInd = SYS(3) + '.DBF'
	jInd = SYS(3) + '.IDX'
	COPY TO (vInd)
	use (vind) in 24 alias hoja1 exclu
	select hoja1
	
	BROW
	SUSP
	
	INDEX ON NUMMES+NUMHC TO (JIND) 
	set rela to NUMMES+NUMHC into hoja
	
    GO TOP   
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla Nummes WITH iif(m.estado='94',m.nummes,IIF(m.estado='92',m.nummeshm,m.nummes)),;
                numref with iif(m.tipdoc='P/R',m.NumPr,iif(m.tipdoc='H/M',m.NumHm,m.numhc)),;
                mesfte with m.nummes,;                
                numfte with iif(m.tipdoc$'H/MP/R',hoja.NumHC,Hoja.NumRef),;                
                tipfte with iif(m.tipdoc$'H/MP/R','H/C',hoja.tipDoc),;                
                tipdoc with iif(m.tipdoc='P/R','P/R',iif(m.tipdoc='H/M','H/M','H/C')),;
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
                fecref with hoja.fechc,;
                codcal with hoja.codcal,;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with iif(m.tipdoc='P/A',-1,iif(m.tipdoc='P/R',-1,iif(m.tipdoc='H/M',IIF(m.tipope='-',-1,1),1))),;
                descri with fte()
                IF alltrim(vtipfun)='F'
	                replace Codanal with m.CodAnal
	            else    
                	replace Codpart WITH M.CodPart
                endif	
       ENDIF
       UNLOCK
       IF !EMPTY(m.numpa) and mespa <= alltrim(vnummes2)
          IF F_APPD()
          repla Nummes WITH M.MESPA,;
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
                tipfte with 'H/C',;
                fecref with hoja.fechc,;
                codcal with hoja.codcal,;
                numfte with HOJA.NUMHC,;
                mesfte with hoja.nummes,;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with -1,;
                descri with fte()                
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
    
    **************************
    * DE HOJAS DE MODIFICACION
    **************************
    USE IN 10
    USE IN 13
    SELECT ITEHC
    
    SET RELA TO NUMMES+NUMHC INTO HOJMOD
	IF ALLTRIM(vtipfun)='I'
       SET FILT TO tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
	  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND HOJMOD.ESTADO='50' AND ITEHC.TIPFUN='I' AND BETWEEN(VAL(NUMMESHM),VAL(VNUMMES1),VAL(VNUMMES2)) AND ITEHC.OPERAC='T' AND ITEHC.TIPOPE='+'
  	ELSE				 
       SET FILT TO tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND HOJMOD.ESTADO='50' AND ITEHC.TIPFUN='F' AND BETWEEN(VAL(NUMMESHM),VAL(VNUMMES1),VAL(VNUMMES2)) AND ITEHC.OPERAC='T' AND ITEHC.TIPOPE='+'
	ENDIF

	SInd = SYS(3) + '.DBF'
	PInd = SYS(3) + '.IDX'
	COPY TO (SInd)
	use (Sind) in 20 alias hoJM1 exclu
	select hojM1

	INDEX ON NUMMES+NUMHC TO (PIND) 
	
    GO TOP   
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla Nummes WITH IIF(m.estado='92',m.nummeshm,M.NUMMES),;
                tipdoc with 'C/P',;
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
                tipo with '2',;
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
       
    ENDSCA
    SELE ITEHC
    SET RELA TO        
        
    ************************
    * DE COMPROBANTE DE PAGO
    ************************
    USE IN 8
    USE IN 9
    USE IN 10
	USE ComPag   IN 9   order tag ComPag1  ALIAS ComPag
	USE IteCP    IN 10  ORDER TAG IteCP1   ALIAS IteCP
    USE Reten    IN 13  order tag Reten1   ALIAS Reten    
    
    SELECT IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG  additive
    
    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(COMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  SUBSTR(COMPAG.CodCal,5,3)=ALLTRIM(vCodFte) .AND.  SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSub) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vProyec),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vProyec),.t.) .and. ;
                  iif(!EMPTY(vSubPry),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vSubpry),.t.) AND ;
                  !EMPTY(COMPAG.NUMCP) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.) AND COMPAG.TIPDOC$'HCRG' AND ;
                  COMPAG.TIPFUN=ALLTRIM(VTIPFUN) AND COMPAG.periodo=alltrim(vperiodO) AND;
                  !EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc) && AND Hoja.Estado="50" 
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
         		descri with fte(),;                
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
    
    SELE RETEN
    SET RELATION TO NUMMES+NUMCP+CODCTC  INTO Compag  
	SET RELATION TO NUMMESHC+NUMHC 		 INTO hoja   ADDITIVE
    
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
                  !EMPTY(Reten.nummeshc) AND !EMPTY(Reten.Numhc)
    
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	 repla   fecref with M.fecpag,;
          	        CODANAL WITH M.PARTRET,;
          	        CODPART WITH HOJA.CODPART,;
          	          nummes with m.mescppg,;
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
          	          nummes with m.mescppg,;
          	          tipdoc with 'C/P',;
          	         numref with m.NumCppg,;
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
*         	                     	          numref with m.NumCpPG
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
    USE IN 14
    USE IN 15
	USE MaeReg   IN 14  ORDER TAG Maereg2   ALIAS MaeReg
	
	Windi=SYS(3)+'.IDX'
	
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
    SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag
    
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
                  !EMPTY(Maereg.Numhc))
    
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
    USE IN 5
    USE IN 10
    USE IN 13
    USE IN 15
 	USE IteReg   IN 10   ORDER TAG Itereg1   ALIAS IteReg
	
	WINDi=SYS(3)+'.IDX'
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
                    Maereg.Reten =0 AND ;
                    (!EMPTY(Maereg.nummeshc) AND ;
                    !EMPTY(Maereg.Numhc))
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
	
    USE IN 6
    USE IN 8
    USE IN 9
    USE IN 13
    USE IN 14
    USE IN 15
    USE itepar   in 8   order tag itepar1  ALIAS Itepar 
    USE clase   IN 15  order tag Clase1  ALIAS Clase
    SELECT AST
    VIND=SYS(3)+'.IDX'
    IF ALLTRIM(vtipfun)='F'
       INDEX ON CODSUBPR+CODACT+codANAL+NUMMES+TIPO+TIPDOC+NUMREF TO (VIND) FOR IIF(!EMPTY(vCodPart),iif(right(vCodpart,2)='00',left(vcodpart,2)=left(codanal,2),codanal=vcodpart),.t.)
    ELSE
       INDEX ON CODSUBPR+CODPROY+CODSUPRY+codPART+NUMMES+TIPO+TIPDOC+NUMREF TO (VIND) FOR IIF(!EMPTY(vCodPart),iif(right(vCodpart,2)='00',left(vcodpart,2)=left(codpart,2),codPart=vcodpart),.t.)
    ENDIF
    SET INDEX TO (VIND)
    DEACTIVATE WINDOW STANDBY
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
	    DO CASE 
    	   CASE alltrim(vtipfun) = 'F'
		       DO REPORTE WITH 2,"AUXCOMF",' Movimiento Presupuestal'    	   
	       CASE alltrim(vtipfun) = 'I'
		       DO REPORTE WITH 2,"AUXCOMI",' Movimiento Presupuestal'	       
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
        DEVUL = IIF(HOJA.TIPPRV='P',val_pro(HOJA.Codprv),IIF(HOJA.TipPRV='E',val_pro(HOJA.Codemp),HOJA.NOMBRE))
   CASE TIPDOC='C/P'
        DEVUL = IIF(compag.TIPPRV='P',val_pro(compag.Codprv),IIF(compag.TipPRv='E',val_pro(compag.Codemp),compag.NOMpRE))
   CASE TIPDOC='P/A'   
        DEVUL = 'PARTE DE ANULACION DE LA '+TIPFTE+' '+NUMFTE
   CASE TIPDOC='PCH'   
        DEVUL = 'PARTE DE ANULACION DE CHEQUE DE '+TIPFTE+' '+NUMFTE
   CASE TIPDOC='P/R'   
        DEVUL = 'PARTE DE REBAJA DE LA '+TIPFTE+' '+NUMFTE
   CASE TIPDOC='H/M'        
         DO CASE
            CASE OPERAC='T'  
                 Devul = 'Transf.Int.de la H/C: '+nummes+'.'+numhc+' '+val_para(tipcom,'HOJMOD','D',22,18)
            CASE OPERAC='R'  
                 Devul = 'Rebaja de la H/C : '+nummes+'.'+numhc+' '+val_para(tipcom,'HOJMOD','D',22,18)
            CASE OPERAC='C'  
                 Devul = 'Cambio Fte. de la H/C: '+nummes+'.'+numhc+' '+val_para(tipcom,'HOJMOD','D',22,18)
            CASE OPERAC='A'  
                 Devul = 'Anulaci¢n del Compromiso de la H/C: '+nummes+'.'+numhc+' '+val_para(tipcom,'HOJMOD','D',22,18)
         ENDCASE
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

*******************************
*CUADRO DE REBAJAS H/M,P/A,P/R
*******************************

PROCEDURE TIPHMCD   &&  COMPROMISOS DEBE
*----------------
PRIVATE vfun
vfun = .F.
IF TIPO ='3'
	IF (operac='C' and tipcom='CYP' and tipope#-1) or;
	   (operac='C' and tipcom='C  ' and tipope#-1) or;
	   (operac='T' and tipcom='CYP' and tipope#-1) or;
	   (operac='T' and tipcom='C  ' and tipope#-1) 
*      (operac='R' and tipcom='C  ' and tipope=-1) 
	   vfun = .T.
	endif
ENDIF
return vfun
   
PROCEDURE TIPHMCH    && COMPROMISOS HABER
*----------------
PRIVATE vfun
vfun = .F.
IF TIPO ='3'
	if (operac='C' and tipcom='CYP' and tipope=-1) or;
	   (operac='C' and tipcom='C  ' and tipope=-1) or;
	   (operac='T' and tipcom='CYP' and tipope=-1) or;
	   (operac='T' and tipcom='C  ' and tipope=-1) or;
	   (operac='R' and tipcom='CYP' and tipope=-1) or;
	   (operac='R' and tipcom='C  ' and tipope=-1) or;
 	   (operac='A' and tipcom='C  ')
  	   vfun = .T.
	endif
endif
return vfun 

PROCEDURE TIPHMAD   && ASIGNACIONES DEBE
*----------------
PRIVATE vfun
vfun = .F.
IF TIPO ='3'
   if (operac='C' and tipcom='CYP') or;
      (operac='C' and tipcom='C  ') or;
      (operac='T' and tipcom='CYP') or;
      (operac='T' and tipcom='C  ' and tipope=-1) or;
      (operac='T' and tipcom='P  ' and tipope#-1) or;
      (operac='R' and tipcom='CYP' and tipope=-1) or;
      (operac='R' and tipcom='C  ' and tipope=-1) or;
      (operac='A' and tipcom='C  ')
	  vfun = .T.
	endif
endif
return vfun
      
PROCEDURE TIPHMAH   && ASIGNACIONES HABER
*----------------
PRIVATE vfun
vfun = .F.
IF TIPO ='3'
	if (operac='C' and tipcom='CYP') or;
	   (operac='C' and tipcom='P  ') or;
	   (operac='T' and tipcom='CYP') or;
	   (operac='T' and tipcom='C  ' and tipope#-1) or;
	   (operac='T' and tipcom='P  ' and tipope=-1) or;
	   (operac='R' and tipcom='CYP' and tipope=-1) or;
	   (operac='R' and tipcom='P  ' and tipope=-1)  
	   vfun = .T.
	endif
ENDIF	
return vFun

PROCEDURE TIPHMED    && EJECUCION DEBE
*----------------
PRIVATE vfun
vfun = .F.
IF TIPO ='3'
   If (operac='C' and tipcom='CYP' and tipope=-1) or;
      (operac='C' and tipcom='P  ' and tipope=-1) or;
      (operac='T' and tipcom='CYP' and tipope=-1) or;
      (operac='T' and tipcom='P  ' and tipope=-1) or;
      (operac='R' and tipcom='CYP' and tipope=-1) or;
      (operac='R' and tipcom='P  ' and tipope=-1) 
	  vfun = .T.
	endif
ENDIF	
return vfun

      
PROCEDURE TIPHMEH   && EJECUCION HABER
*----------------
PRIVATE vfun
vfun = .F.
IF TIPO ='3'              
   If (operac='C' and tipcom='CYP' and tipope#-1) or;
      (operac='C' and tipcom='P  ' and tipope#-1) or;
      (operac='T' and tipcom='CYP' and tipope#-1) or;
      (operac='T' and tipcom='P  ' and tipope#-1)  
	  vfun = .T.
	endif
endif
return vfun


PROCEDURE SUMATOT
*----------------
GO TOP
SUM COMPBRU,REBMES,REBANT,COMLIQ,COMLIQAC,EJEBRU,EJEREB,EJELIQ TO A1,A2,A3,A4,A5,A6,A7,A8 FOR IIF(ALLTRIM(VTIPFUN)='I',RIGHT(CODPART,2)#'00',RIGHT(CODANAL,2)#'00')
GO TOP
RETURN
