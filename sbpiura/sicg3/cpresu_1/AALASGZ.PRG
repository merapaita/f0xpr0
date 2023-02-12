** ---------------------------------------------------
** REPOE5.PRG
** Reporte Auxiliar de Compromisos por partidas
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------
* Abrir Archivos de trabajo
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE ItePar   IN 5 ORDER TAG ItePar1         ALIAS ItePar
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE Calen    IN 8   order tag calen1   ALIAS calen
*USE Clase    IN 10  order tag Clase1   ALIAS Clase
USE Maepre   IN 11 order tag Maepre1   ALIAS Maepre
*USE astpre   IN 13  order tag Astpre5  ALIAS AstPre
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
USE ComPag   IN 15   order tag ComPag4  ALIAS ComPag
USE hojmod   IN 16  order tag hojmod1  ALIAS HOJMOD
USE ASTAUX   IN 17                     ALIAS AST
DO Inicia
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------
vtemp    = RECNO()
as=order()
vPeriodo  = '  '
vNumMes1  = '  '
vNumMes2  = '  '
vTipFun   = ' '
vCodPrg   = '  '
vCodFte   = '   '
vCodsub = '   '
vCodPart  = '     '
vCodPry   = '   '
vCodSpy   = '  '
vCodact   = '  '

 DEFINE WINDOW Lis FROM 5,12 TO 18,70 DOUBLE ;
 TITLE ' °° Saldos Asignaci¢n por Partidas °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '            Mes : '
 @  3,2 say '           Tipo : ' 
 @  4,2 say '         Fuente : ' 
 @  5,2 say '       Programa : '
 @  6,2 say '    SubPrograma : '
 @  7,2 say '       Proyecto : '
 @  8,2 say '    SubProyecto : '
 @  9,2 say '      Actividad : '
 @ 10,2 say ' Part. Anal¡tica: '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes1   PICTURE '!!'  VALID Val_Para(vNumMes1 ,'FECMES','C',21,30,4) AND !EMPTY(vNumMes1)
 @  3,21  GET vtipfun    PICTURE '!'   valid Val_Para(vTipfun  ,'TIPFUN',' ',21,20) 
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30,4) AND !EMPTY(vCodFte) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30,4) AND !EMPTY(vCodPrg)
 @  6,21  GET vCodsub    PICTURE '!!!' VALID IIF(!EMPTY(vcodSub),VAL_SUBP(vCodsub,'CODSUB'+VCodPrg,' ',21,28),.t.)
 @  7,21  GET vCodPry    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I'
 @  8,21  GET vCodSPy    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='I'
 @  9,21  GET vCodact    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'
 @ 10,21  GET vCodPart   PICTURE '!!.!!'

 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF EMPTY(vNumMES1)  .or. LASTKEY()=27
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
    USE (VDBF) IN 17 ALIAS AST

    VIND=SYS(3)+'.IDX'
    IF ALLTRIM(vtipfun)='F'
       INDEX ON NUMMES+CODSUBPR+CODACT+codANAL+TIPO+TIPDOC+NUMREF TO (VIND)
    ELSE
       INDEX ON NUMMES+CODSUBPR+CODPROY+CODSUPRY+codPART+TIPO+TIPDOC+NUMREF TO (VIND) 
    ENDIF
    SET INDEX TO (VIND)
    
    IF ALLTRIM(vtipfun)='F'
		xvalor = alltrim(vcodprg)+alltrim(vcodsub)              
		proact = alltrim(vcodact)
	ELSE	
		xvalor = Alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec)
		proact = alltrim(vSubpry)
	ENDIF
    
    ******** AFECTACIONES ********
    * INCLUYE=>H/C , P/A, P/R, H/M
    ******************************    
    
    SELEC ITEHC
	SET RELATION TO NUMMES+NUMHC INTO hoja    
	SET RELATION TO NUMMES+NUMHC INTO compag addi	
    
	IF ALLTRIM(vtipfun)='I'
	   SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,itehc.tipdoc,;
		      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL, ITEHC.NUMMESHM, itehc.numhm, ITEHC.MESPA,ITEHC.NUMPA,ITEHC.MESPR,ITEHC.NUMPR,ITEHC.OPERAC,ITEHC.TIPCOM,ITEHC.MESOCOS ;
			  FROM ITEHC ;
			  WHERE  tipfun+codfte+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND ;
			         ITEHC.ESTADO#'99' AND ;
			         ITEHC.TIPFUN='I' AND ;
			         BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND ;
			         IIF(!EMPTY(COMPAG.NUMMES),COMPAG.NUMMES#ITEHC.NUMMES,.T.) and ;
			         IIF(!EMPTY(MESPR),VAL(MESPR)=VAL(vnummes2),.t.) and ;
			         IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.) ;
					 INTO CURSOR Hojax
	ELSE
	 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART, itehc.tipdoc, ;
	     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm, ITEHC.MESPA,ITEHC.NUMPA,ITEHC.MESPR,ITEHC.NUMPR ,ITEHC.OPERAC,ITEHC.TIPCOM,ITEHC.MESOCOS;
	           FROM ITEHC ;
		 	   WHERE tipfun+codfte+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ;
		 	         ITEHC.ESTADO#'99' AND ;
		 	         ITEHC.TIPFUN='F' AND ;
		 	         VAL(NUMMES)<=VAL(VNUMMES1) AND ;
		 	         IIF(!EMPTY(COMPAG.NUMMES),COMPAG.NUMMES#ITEHC.NUMMES,.T.) ;
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
          repla Nummes WITH m.Nummes,;
                tipdoc with 'H/C',;
                numref with m.numhc,;
                codprg with m.codprg,;
                codsubpr with m.Codsubpr,;
                codproy with iif(vtipfun='I',m.codproy,'   '),;
                codsupry with iif(vtipfun='I',m.codsupry,'   '),;
                codact with iif(vtipfun='F',m.codact,'  '),;
                valpart with m.valpart,;
                mesocos with m.mesocos,;
                mesPr with m.mespr,;
                mesPa with m.mespa,;
                tipo with '1',;
                tipfte with hoja.tipDOC,;
                desfte with hoja.desref,;
                fecref with hoja.fechc,;
                codcal with hoja.codcal,;
                numfte with HOJA.NUMref,;
                mesfte with m.nummes,;
                operac with m.operac,;
                tipcom with m.tipcom,;
                descri with fte(),;
                tipope with 1,;
               destino with hoja.destino,;
               tothc with hoja.imptot,;
               desfte with hoja.desfte
                IF alltrim(vtipfun)='F'
	                replace Codanal with m.CodAnal
	            else    
                	replace Codpart WITH M.CodPart
                endif	
       ENDIF
       UNLOCK
       SELEC HOJA1
    ENDSCAN 
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
    USE MaeReg   IN 8   ORDER TAG Maereg2   ALIAS MaeReg
	
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
    
	USE IteReg   IN 9   ORDER TAG Itereg1   ALIAS IteReg
	
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
                    COMPAG.estado#'9' AND ;
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



    
    SELE AST
    GO TOP
    DEACTIVATE WINDOW STANDBY       
    m.codfte = alltrim(vcodfte)
    SET UNIQ ON
    
    IF ALLTRIM(vtipfun)='F'
       INDEX ON LEFT(codANAL,2) TO (VIND)
    ELSE
       INDEX ON LEFT(COdPART,2) TO (VIND) 
    ENDIF
    SET INDEX TO (VIND)

    xdbf = sys(3)+'.dbf'
    copy to (xdbf)
    USE (XDBF) IN 18 ALIAS AST1

    SET UNIQ OFF
    
    VIND=SYS(3)+'.IDX'
    IF ALLTRIM(vtipfun)='F'
       INDEX ON CodANAL TO (VIND)
    ELSE
       INDEX ON codPART TO (VIND) 
    ENDIF
    SET INDEX TO (VIND)

    SELE AST1
	GO TOP
	SCAN 
	    IF ALLTRIM(vtipfun)='F'
	       VF = LEFT(CODANAL,2)
	       SELE AST
  	       SUM VALDEB*TIPOPE TO SW FOR LEFT(CODANAL,2) = VF
	       IF F_APPD()
	          REPLA CODANAL WITH VF+'.00',VALDEB WITH SW,TIPOPE WITH 1
	       ENDIF
	    ENDIF
	    SELE AST1
	ENDSCAN	           
	SELECT AST

	GO TOP
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       IF ALLTRIM(VTIPFUN)='F'
       	DO REPORTE WITH 2,"ASIGF",' Auxiliar de Standar' 
       ELSE	
        INDEX ON NUMMES+CODSUBPR+CODPROY+CODSUPRY+codPART+TIPO+TIPDOC+NUMREF TO (VIND) 
       	DO REPORTE WITH 2,"ASIGI",' Auxiliar de Standar' 
       ENDIF	
    ENDIF
 ENDIF
  SET FILTER TO
  CLOSE INDEX 
  ERASE (VIND)
CLOSE DATA  
RETURN

PROCEDURE FTE
*------------
DEVUL='**'
DO CASE
   CASE TIPDOC='H/C' 
        DEVUL = IIF(HOJA.TIPPRV='P',val_pro(HOJA.Codprv,'20'),IIF(HOJA.TipPRV='E',val_pro(HOJA.Codemp,'30'),HOJA.NOMBRE))
   CASE TIPDOC='C/P'
        DEVUL = IIF(compag.TIPPRV='P',val_pro(compag.Codprv,'20'),IIF(compag.TipPRv='E',val_pro(compag.Codemp,'30'),compag.NOMpRE))
   CASE TIPDOC='P/A'   
        DEVUL = 'PARTE DE ANULACION DE LA '+TIPFTE+' '+NUMFTE
   CASE TIPDOC='PCH'   
        DEVUL = 'PARTE DE ANULACION DE CHEQUE DE '+TIPFTE+' '+NUMFTE
ENDCASE        
RETURN DEVUL                       

PROCEDURE FTEHM
*------------
vDescr = ' ** '
DO CASE
   CASE HOJMOD.OPERAC='T'  
        vDescr = 'Transf.Int.de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
   CASE HOJMOD.OPERAC='R'  
        vDescr = 'Rebaja de la H/C : '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
   CASE HOJMOD.OPERAC='C'  
        vDescr = 'Cambio Fte. de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
ENDCASE
RETURN vDescr

PROCEDURE xFTE
*------------
DEVUL='**'
DO CASE
   CASE TIPDOC='H/C' 
        DEVUL = IIF(HOJA.TIPPRV='P',val_pro(HOJA.Codprv,'20'),IIF(HOJA.TipPRV='E',val_pro(HOJA.Codemp,'30'),HOJA.NOMBRE))
   CASE TIPDOC='C/P'
        DEVUL = COMPAG.CCNOMBRE
        DEVUL = IIF(compag.TIPPRV='P',val_prv(compag.Codprv),IIF(compag.TipPRV='E',val_pro(compag.Codemp),compag.NOMpRE))
   *     DEVUL = IIF(COMPAG.CodPrv='0000' and empty(COMPAG.codemp),Compag.Nombre,IIF(Compag.TipDoc='O/',val_prv(ComPag.Codprv),IIF(Compag.CodEmp='00000' and empty(Compag.Codprv),Compag.Nombre,val_pro(compag.Codemp))))
   CASE TIPDOC='P/A'   
        DEVUL = 'PARTE DE ANULACION DE LA '+TIPFTE+' '+NUMFTE
   CASE TIPDOC='PCH'   
        DEVUL = 'PARTE DE ANULACION DE CHEQUE DE '+TIPFTE+' '+NUMFTE
ENDCASE        
RETURN DEVUL                       

FUNCTION VAL_PRO
*---------------
parameter xcod,_tipo
private medita, mmsg, malias, v_fun, _oldwind,_campo

_campo = varread()

malias = alias()
select AUXIL
_oldwnd = woutput()

SET ORDE TO AUXIL1
seek _TIPO+xcod
v_fun = iif(found(),Descri,"No encontrado")
select (malias)
return v_fun

PROCEDURE TIPHMCD
*----------------
PRIVATE vfun
IF (operac='C' and tipfte='CYP' and tipope#-1) or (operac='C' and tipfte='C  ' and tipope#-1) or (operac='T' and tipfte='CYP' and tipope#-1) or (operac='T' and tipfte='C  ' and tipope#-1) or (operac='R' and tipfte='C  ' and tipope=-1) &&or (operac='R' and tipfte='CYP')
   return .T.
endif
retur .f.
   
PROCEDURE TIPHMCH
*----------------
PRIVATE vfun
if (operac='C' and tipfte='CYP' and tipope=-1) or (operac='C' and tipfte='C  ' and tipope=-1) or (operac='T' and tipfte='CYP' and tipope=-1) or (operac='T' and tipfte='C  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='C  ' and tipope=-1) 
   retur .t.
endif
retur .f.
   
PROCEDURE TIPHMAD
*----------------
PRIVATE vfun
if (operac='C' and tipfte='CYP') or (operac='C' and tipfte='C  ') or (operac='T' and tipfte='CYP') or (operac='T' and tipfte='C  ' and tipope=-1) or (operac='T' and tipfte='P  ' and tipope#-1) or (operac='R' and tipfte='CYP' and tipope=-1) &&or (operac='R' and tipfte='P  ') 
   retur .t.
endif
retur .f.
      
PROCEDURE TIPHMAH
*----------------
PRIVATE vfun
if (operac='C' and tipfte='CYP') or (operac='C' and tipfte='P  ') or (operac='T' and tipfte='CYP') or (operac='T' and tipfte='C  ' and tipope#-1) or (operac='T' and tipfte='P  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='P  ' and tipope=-1) 
   retur .t.
endif
retur .f.
      
PROCEDURE TIPHMED
*----------------
PRIVATE vfun
if (operac='C' and tipfte='CYP' and tipope=-1) or (operac='C' and tipfte='P  ' and tipope=-1) or (operac='T' and tipfte='CYP' and tipope=-1) or (operac='T' and tipfte='P  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='P  ' and tipope=-1) 
   retur .t.
endif
retur .f.
      
PROCEDURE TIPHMEH
*----------------
PRIVATE vfun
if (operac='C' and tipfte='CYP' and tipope#-1) or (operac='C' and tipfte='P  ' and tipope#-1) or (operac='T' and tipfte='CYP' and tipope#-1) or (operac='T' and tipfte='P  ' and tipope#-1)
   retur .t.
endif
retur .f.
*---
     

PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vcODPRY)
SELE MAEPRE
SET ORDE TO maepre2
seek vkey
vProyecto = maepre.Descri
select (zx)
set order to (qw)
return (vProyecto)

PROCEDURE buscact
*----------------
PARAMETER vact
PRIVATE AS
AS=ALIAS()
**BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(Vperiodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO maepre3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE (AS)
RETURN (vactividad)
