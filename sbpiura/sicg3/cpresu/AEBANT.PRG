** --------------------------------
** REPOE5.PRG
** Reporte de Rebjas Anteriores de Mes
** UDEP
** Autor : Ing. Fernando Mu¤oz Cho
** -------------------------------
* Abrir Archivos de trabajo

CLOSE DATA
filaux = SYS(3) + '.DBF'

SELECT 8
CREATE TABLE (filaux);
  (Periodo  C(02),;
   Nummes   C(02),;
   Tipfun   C(01),;
   Codfte   C(03),;
   codprg   C(02),;
   CodSubpr C(03),;
   Codact   C(02),;
   CodProy  C(03),;
   CodSupry C(02),;
   CodPart  C(05),;
   Codanal  C(05),;
   valpart  N(18,3))
USE


vdbfaux2=SYS(03)+'.DBF'
xidxAUX2=SYS(03)+'.IDX'

SELECT 10
CREATE TABLE (vdbfaux2);
  (CodPart  C(05),;
   Codanal  C(05),;
   ValEne   N(18,3),;
   ValFeb   N(18,3),;
   ValMar   N(18,3),;
   ValAbr   N(18,3),;
   ValMay   N(18,3),;
   ValJun   N(18,3),;
   ValJul   N(18,3),;
   ValAgo   N(18,3),;
   ValSet   N(18,3),;
   ValOct   N(18,3),;
   ValNov   N(18,3),;
   ValDic   N(18,3),;
   ValTot   N(18,3))
USE

USE (vdbfaux2) IN 10  					 ALIAS Tempo  EXCLUSI
USE (filaux)   IN 8                      ALIAS Tempo1 EXCLUSI
USE HojCon     IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc      IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae     IN 3   order tag Parmae1  ALIAS Parma
USE hojmod     IN 4   order tag hojmod2  ALIAS HOJMOD
USE PteAnu     IN 5   order tag PteAnu9  ALIAS AnuPA

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
vPeriodo  = RIGHT(STR(YEAR(DATE()),4),2)
vNumMes1  = '01'
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
 @  2,21  SAY vNumMes1      
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
 *-------------------------
 SELE Tempo
 IF ALLTRIM(vtipfun) = 'I'
	* INDEX ON CodPart TAG (xidxAUX2)
	INDEX ON CodPart to (xidxAUX2)
 ELSE
	* INDEX ON Codanal TAG (xidxAUX2)
	INDEX ON Codanal to (xidxAUX2)
 ENDIF   
 * SET ORDER TO (xidxAUX2)
 *---------------------------
 
 
 IF EMPTY(vNumMES2) .or. LASTKEY()=27
    RETURN
 ELSE
    ACTIVATE WINDOW STANDBY
    @ 1,14 SAY 'Espere un Momento ....' color W*
    m.tipfun = alltrim(vtipfun)
    m.periodo = alltrim(vperiodo)
    m.codfte = alltrim(vcodfte)
    xidx=SYS(03)+'.IDX'
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
    * 1era. Modificaci¢n
    *----------------------------------
    * SELEC ITEHC
	* IF ALLTRIM(vtipfun)='I'
	*    SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODPART, ITEHC.VALPART, ITEHC.CODPROY,itehc.tipdoc,;
	*	      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
	*		  FROM ITEHC ;
	*		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
	*  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND  ITEHC.ESTADO#'99' AND  BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND (!EMPTY(NUMPR) OR  (!EMPTY(NUMHM) AND OPERAC='R'));
	*				INTO CURSOR Hojax
	* ELSE
	* 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , ITEHC.VALPART, itehc.tipdoc, ;
	*     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL;
	*           FROM ITEHC ;
	*	 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
	*		         = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ITEHC.ESTADO#'99' AND BETWEEN(VAL(NUMMES),VAL(VNUMMES1),VAL(VNUMMES2)) AND (!EMPTY(NUMPR) and !EMPTY(NUMPR) OR  (!EMPTY(NUMHM)  AND OPERAC='R'));
	*				 INTO CURSOR HOJAX
	* ENDIF
	*-------------------------------------------------------
    * 2da Modificaci¢n
	*-------------------------------------------
	* SELEC ITEHC
	* IF ALLTRIM(vtipfun)='I'
	*   SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODPART, ITEHC.VALPART, ITEHC.CODPROY,itehc.tipdoc,;
    *   ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
	*		  FROM ITEHC ;
	*		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
	*  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND  ITEHC.ESTADO#'99' AND  (VAL(MESPR)=VAL(VNUMMES2) OR VAL(NUMMESHM)=VAL(VNUMMES2)) AND TIPOPE#'+';
	*				INTO CURSOR Hojax
	* ELSE
	* 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , ITEHC.VALPART, itehc.tipdoc, ;
	*     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL;
	*           FROM ITEHC ;
	*	 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
	*		         = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ITEHC.ESTADO#'99' AND (VAL(MESPR)=VAL(VNUMMES2) OR VAL(NUMMESHM)=VAL(VNUMMES2)) AND TIPOPE#'+';
	*				 INTO CURSOR HOJAX
	* ENDIF
	*-------------------------------------------------------
	
	SELEC ITEHC
	IF ALLTRIM(vtipfun)='I'
	   SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODPART, ITEHC.VALPART, ITEHC.CODPROY,itehc.tipdoc,;
		      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
			  FROM ITEHC ;
			  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
	  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND  ITEHC.ESTADO#'99' AND  ((VAL(MESPR)=VAL(VNUMMES2) OR VAL(NUMMESHM)=VAL(VNUMMES2)) and VAL(NUMMES)<VAL(VNUMMES2)) AND TIPOPE#'+' AND (!EMPTY(NUMPR) OR (!EMPTY(NUMHM) AND OPERAC$'R' AND LEFT(TIPCOM,1)$'C'));
					INTO CURSOR Hojax
	ELSE
	 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , ITEHC.VALPART, itehc.tipdoc, ;
	     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL;
	           FROM ITEHC ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
			         = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ITEHC.ESTADO#'99' AND ((VAL(MESPR)=VAL(VNUMMES2) OR VAL(NUMMESHM)=VAL(VNUMMES2)) and VAL(NUMMES)<VAL(VNUMMES2)) AND TIPOPE#'+' AND (!EMPTY(NUMPR) OR (!EMPTY(NUMHM) AND OPERAC$'R' AND LEFT(TIPCOM,1)$'C'));
					 INTO CURSOR HOJAX
	ENDIF
	*-----------------------------------------------------------------------------------

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
       SELECT Tempo1
       IF F_APPD()
          repla Nummes   WITH m.nummes,;
                Periodo  WITH M.periodo,;
                Codfte   WITH m.codfte,;  
                tipfun   WITH M.tipfun,;
                codprg   with m.codprg,;
                codsubpr with m.Codsubpr,;
                codproy  with iif(vtipfun='I',m.codproy,'   '),;
                codsupry with iif(vtipfun='I',m.codsupry,'   '),;
                codact   with iif(vtipfun='F',m.codact,'  '),;
                valpart  with m.valpart
                IF alltrim(vtipfun)='F'
	                replace Codanal with m.CodAnal
	            else    
                	replace Codpart WITH M.CodPart
                endif	
       ENDIF
    UNLOCK
    ENDSCAN
    SET INDEX TO
    
    SELECT Tempo1
    if ALLTRIM(vtipfun) = 'I'
       INDEX ON CODSUBPR+CODPROY+CODSUPRY+CodPart TO (xidx)
    ELSE   
       INDEX ON CODSUBPR+CODACT+codanal TO (xidx)
    ENDIF   
    GO TOP
    
    *-------------------------------------------
    * Suma por Partida 
    *-------------------------------------------
    SELE Tempo1
    GO TOP
    SCAN
    
         SELE Tempo
         IF ALLTRIM(vtipfun) = 'I'
            SEEK(Tempo1.CodPart)
         ELSE   
            SEEK(Tempo1.codanal)
         ENDIF   
         
	     IF FOUND()
		     Bloquea = f_lock(1)
		 ELSE
		     Bloquea = f_appd()
		 ENDIF
		 
		 IF Bloquea
  	         IF  alltrim(vtipfun)='F'
	             REPLACE Codanal WITH Tempo1.CodAnal
	         ELSE
                 REPLACE Codpart WITH Tempo1.CodPart
	         ENDIF
	         DO CASE
	            CASE Tempo1.Nummes='01'
                     REPLACE  ValEne  WITH ValEne+Tempo1.valpart
	            CASE Tempo1.Nummes='02'
                     REPLACE  ValFeb  WITH ValFeb+Tempo1.valpart
	            CASE Tempo1.Nummes='03'
                     REPLACE  ValMar  WITH ValMar+Tempo1.valpart
	            CASE Tempo1.Nummes='04'
                     REPLACE  ValAbr  WITH ValAbr+Tempo1.valpart
	            CASE Tempo1.Nummes='05'
                     REPLACE  ValMay  WITH ValMay+Tempo1.valpart
	            CASE Tempo1.Nummes='06'
                     REPLACE  ValJun  WITH ValJun+Tempo1.valpart
	            CASE Tempo1.Nummes='07'
                     REPLACE  ValJul  WITH ValJul+Tempo1.valpart
	            CASE Tempo1.Nummes='08'
                     REPLACE  ValAgo  WITH ValAgo+Tempo1.valpart
	            CASE Tempo1.Nummes='09'
                     REPLACE  ValSet  WITH ValSet+Tempo1.valpart
	            CASE Tempo1.Nummes='10'
                     REPLACE  ValOct  WITH ValOct+Tempo1.valpart
	            CASE Tempo1.Nummes='11'
                     REPLACE  ValNov  WITH ValNov+Tempo1.valpart
	            CASE Tempo1.Nummes='12'
                     REPLACE  ValDic  WITH ValDic+Tempo1.valpart
             ENDCASE       
             REPLACE ValTot WITH ValTot+Tempo1.valpart
		ENDIF  
		
        SELE Tempo1
    ENDSCAN
    
    *-----------------------------------------------------------
    DEACTIVATE WINDOW STANDBY       
    SELE TEMPO
    GO TOP
    DO REPORTE WITH 2,"Rebant",' Rebajas de Meses Anteriores '    	 
    CLOS DATA
    ERASE (xidx)
    ERASE (vdbfaux2)
    ERASE (xidxAUX2)
    ERASE (FILAUX)
ENDIF    
RETURN