* ---------------------------------------------------
** SALPRE.PRG
** Reporte Ejecucion ESTADOS
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------

* Abrir Archivos de trabajo

USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Calen   IN 2 ORDER TAG Calen1          ALIAS Calen
USE Maepre  IN 3 order tag Maepre1         ALIAS Maepre
USE MaePar  IN 4 ORDER TAG MaePar1         ALIAS MaePar
USE ItePar  IN 5 ORDER TAG ItePar1         ALIAS ItePar
USE Clase   IN 6 ORDER TAG Clase1          ALIAS Clase
USE HojCon  IN 7 ORDER TAG Hojcon1         ALIAS Hoja
USE IteHc   IN 8 ORDER TAG Itehc1          ALIAS Itehc
USE HOJMOD  IN 11 ORDER TAG hojmod1         ALIAS hojmod
USE Repoe5  IN 9                           ALIAS repo
USE Repopre IN 10                          ALIAS repoP

*----
PUBLIC TOT,VACUM,TOT1,TOTCOM
TOT = 0
TOT1 = 0
DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------

PUBLIC vPeriodo,VTipFun,VCodPrg,vCodFte,vCodSub,vCodAct,vProyec,vSubpry,vCalend 

vPeriodo  = '  '
vTipFun   = ' '
vCodPrg   = '  '
vCodFte   = '   '
vCodSub   = '   '
vCodAct   = '  '
vProyec   = '   '
vSubpry   = '  '
vCalend   = '  '

 DEFINE WINDOW Lis FROM 6,10 TO 17,70 DOUBLE ;
 TITLE ' °° SALDOS PRESUPUESTAL °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '     Calendario : '
 @  3,2 say '         Fuente : '
 @  4,2 say '           Tipo : '
 @  5,2 say '       Programa : ' 
 @  6,2 say '    SubPrograma : '
 @  7,2 say '       Proyecto : ' 
 @  8,2 say '      Actividad : '

 Vcalend = MONTH(DATE())
 IF Vcalend<10
    vcalend  ='0'+ALLTRIM(STR(MONTH(DATE())))
 ELSE
    vcalend  =STR(Vcalend,2)   
 ENDIF   
 vperiodo = RIGHT(STR(YEAR(DATE())),2)

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vCalend    PICTURE '!!'  valid VAL_PARA(vCalend  ,'FECMES',' ',21,30)
 @  3,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30) AND !EMPTY(vCodFte)
 @  4,21  GET vtipfun    PICTURE '!'   VALID VAL_PARA(vTiPFun  ,'TIPFUN',' ',21,30)
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30) AND !EMPTY(vCodPrg) 
 @  6,21  GET vCodsub    PICTURE '!!!' &&VALID VAL_CALE(vCodsub,'CODSUB'+VCodPrg,' ',21,30)
 @  7,21  GET vProyec    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I' 
 @  8,21  GET vCodAct    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'


 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF LASTKEY()=27
    RETURN
 ELSE
    m.tipfun=vtipfun
    m.periodo = vperiodo
    m.nummes = alltrim(vCalend)
	m.codfte = alltrim(vcodfte)
    cd = analpre(0,3,.t.)
    FDX = SYS(3)+'.IDX'
    IF CD
		IF YESNO('¨ Imprime consulta ?')
	    select presu1    
		    IF EOF()
		       DEACTIVATE WINDOW STANDBY
		       DO STANDBY WITH 'No se tiene registros a Procesar'
		    ELSE
		       GO TOP
		       IF ALLTRIM(VTIPFUN)='F'
		           INDEX ON CODPART TO (FDX)
			       DO REPORTE WITH 2,"salpreT",' Listado Saldo Presupuestal ',1,.F.,.T.
			   ELSE
			       DO REPORTE WITH 2,"salpreS",' Listado Saldo Presupuestal ',1,.F.,.T.
			   ENDIF			       
		    ENDIF
		ENDIF    
    ENDIF
 ENDIF
*CLOSE DATA 
RETURN

PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec)
SELE MAEPRE
SET ORDE TO 2
seek vkey
vProyecto = maepre.Descri
select (zx)
set order to (qw)
return (vProyecto)

PROCEDURE Salida
*---------------
RELEASE    WINDOW LibDir
ACTIVATE   SCREEN
*CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)


PROCEDURE RepDiar
*----------------
SELE Diar
vIndi1=SYS(3)+".IDX"
INDEX ON LEFT(COMPROB,2)+CUENTA+TIPO TO (vInd1)

DO REPORTE WITH 2, "REPOE5", 'Reporte de Ejecuci¢n E-5',1,.F.,.T.

CLOSE DATA
ERASE (vIndi1)
RETURN



PROCEDURE buscact
*----------------
PARAMETER vact
private as
as=alias()
**BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(periodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO maepre3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE (as)
RETURN (vactividad)
*--------------------------------------------------------------------------------------

PROCEDURE ANALPRE
*----------------
PARAMETERS vCantOC,vtipO,vmues
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=Xrolea_PRE(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)                , alltrim(vcodact),vcantoc,VTIPO,vmues)
ELSE	
	WDF=Xrolea_PRE(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO,vmues)
ENDIF
USE IN 24
RETURN WDF


PROCEDURE Xrolea_PRE
*-----------------------
parameter xvalor,proact,vtotoc,vtipo,vmues
AS=ALIAS()
Private vdf
vdf = .t.
ACTIVATE SCREEN
on key label f5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10

RDX = SYS(3)+'.IDX'
SDX = SYS(3)+'.IDX'
TDX = SYS(3)+'.IDX'


IF ALLTRIM(m.tipfun)='I'

   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
		  FROM ITEHC ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND ESTADO#'99' AND TIPFUN='I' AND val(nummes)<=val(vcalend) AND EMPTY(NUMPA) and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.); 
				INTO CURSOR Hojax
ELSE

 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
           FROM ITEHC ;
	 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,CODANAL='02',IIF(VTIPO=2,CODANAL='03',.T.)) AND ESTADO#'99' AND TIPFUN='F'  AND val(nummes)<=val(vcalend) AND EMPTY(NUMPA) and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.); 
					INTO CURSOR HOJAX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 17 alias hoja1 exclu

IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ,CALEN.TOTPRE , CALEN.ESTADO;
		  FROM CALEN ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I'  AND val(nummes)<=val(vcalend)  AND ESTADO#'20';
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS,CALEN.TOTPRE,CALEN.ESTADO ;
               FROM CALEN ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='F'  AND val(nummes)<=val(vcalend) AND ESTADO#'20';
					INTO CURSOR CalenX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 19 alias calen1 exclu

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODPROY, ITEPAR.DESSUPRY ,itepar.cresup,itepar.transf ,;
	      ITEPAR.CODSUPRY, ITEPAR.CODPRG,ITEPAR.CODSUBPR,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
		  FROM ITEPAR ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I';
				INTO CURSOR PRESUX
ELSE
	 	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO,itepar.cresup,itepar.transf, ;
	     	   ITEPAR.CODSUBPR, ITEPAR.CODPRG,ITEPAR.CODACT,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
               FROM ITEPAR ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,ITEPAR.CODPART='02',IIF(VTIPO=2,ITEPAR.CODPART='03',.T.)) AND TIPFUN='F';
					INTO CURSOR PRESUX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 18 alias PRESU1 exclu

SELE HOJA1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (RDX)
Else
   INDEX ON CODSUBPR+CODACT+CODANAL TO (RDX)
endif

SELE CALEN1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (SDX)
Else
   INDEX ON CODSUBPR+CODACT+CODPART TO (SDX)
endif

SELE PRESU1
If alltrim(m.tipfun)='I'
	index on Periodo+CodPrg+CodFte+CodPart+CodSubpr+CodProy+CodSuPry to (TDX)
ELSE
	index on Periodo+CodPrg+CodFte+Codpart+CodSubpr+CodAct to (TDX)
ENDIF

GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	vTotcal = 0
	IF ALLTRIM(m.tipfun) = 'I'
	   vKeyI = Calen1.NumMes+ Calen1.TipFun+ Calen1.CodFte+ Calen1.Codprg  &&+Calen1.Codsubpr+ Calen1.CodProy+ Calen1.CodSuPry
	   vOrd = ORDER()
       ***------- ACUMULA AFECTACIONES	
       SELECT HOJA1
	   SET FILTER TO ESTADO#'99'       
       SCAN 
	      vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      vPart2 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      vTotAfe = 0
	      DO WHILE  vPart1 = vPart2
	         do case
	           case HOJA1.estado='92'
                     Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
*	               vPor = iif(HOJA1.tipope='-',-1,1)
	           case HOJA1.estado='94'
	               vPor = -1
	           case HOJA1.estado='99'
	               vPor = 0
	           OTHER
	               vPor = 1
	         endcase   		                 		                 		
	         vTotafe = vTotafe + vpor*Valpart
	         SKIP
	         vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      ENDD
	      SKIP -1
	      sele PRESU1
	      seek m.Periodo+HOJA1.CodPrg+HOJA1.CodFte+HOJA1.CodPart+HOJA1.CodSubpr+HOJA1.CodProy+HOJA1.CodSuPry
		  if found()
			 if rlock()
	            replace totAfe with vtotafe
	         endif
	         unlock
	      endif
	      SELE HOJA1
	    ENDSCAN
	    SET FILTER TO 
	    
	    ***---- ACUMULA CALENDARIOS
	    
        SELE CALEN1

        GO TOP
        SCAN WHILE CALEN1.ESTADO#'99'
              vPart1 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              vPart2 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              vTotAfe = 0
              DO WHILE  vPart1 = vPart2
                  do case
                     case Calen1.estado='99'
                          vPor = 0
                     OTHER
                          vPor = 1
                  endcase   		                 		                 		
                  vTotcal = vTotcal + vpor*Valpart
                  SKIP
                  vPart1 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              ENDD
              SKIP -1
              sele PRESU1
			  seek m.Periodo+calen1.CodPrg+calen1.CodFte+calen1.CodPart+calen1.CodSubpr+calen1.CodProy+calen1.CodSuPry
		      if found()
		        if rlock()
	              replace totcal with vtotcal
	            endif
	            unlock
                vTotcal = 0	
	         endif
	         SELE Calen1
     	ENDSCAN
	    
	ELSE
	   	vKeyF = Calen1.NumMes+ Calen1.TipFun+ Calen1.CodFte+ Calen1.Codprg+Calen1.Codsubpr+ Calen1.CodAct
		vOrd = ORDER()
		SELECT HOJA1
		SET FILTER TO ESTADO#'99'
		GO TOP
      	SCAN while Estado # '99'
	       vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	       vPart2 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	       vTotAfe = 0
	       DO WHILE  vPart1 = vPart2
	          do case
	             case HOJA1.estado='92'
                     Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
*	                  vPor = iif(HOJA1.tipope='-',-1,1)
	             case HOJA1.estado='94'
	                  vPor = -1
	             case HOJA1.estado='99'
	                  vPor = 0
	             OTHER
	                  vPor = 1
	          endcase   		                 		                 		
	          vTotafe = vTotafe + vpor*Valpart
              SKIP
		      vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		    ENDD
		    SKIP -1
 		    sele presu1 	
			seek m.Periodo+HOJA1.CodPrg+HOJA1.CodFte+HOJA1.CodAnal+HOJA1.CodSubpr+HOJA1.CodAct
			if found()
			   replace totAfe with vtotafe
			endif
			unlock
		    SELE HOJA1
		  ENDSCAN
		  SET FILTER TO 
		***---- ACUMULA CALENDARIOS 
		
        SELE CALEN1
        GO TOP 
        SCAN WHILE CALEN1.ESTADO#'99'
              vPart1 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              vPart2 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              vTotCal = 0
              DO WHILE  vPart1 = vPart2
                  do case
                     case Calen1.estado='99'
                          vPor = 0
                     OTHER
                          vPor = 1
                  endcase   		                 		                 		
                  vTotcal = vTotcal + vpor*Valpart
                  SKIP
                  vPart1 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              ENDD
              SKIP -1
              sele PRESU1
			  seek m.Periodo+calen1.CodPrg+calen1.CodFte+calen1.CodPart+calen1.CodSubpr+calen1.CodAct
			  if found()
		        if rlock()
	              replace totcal with vtotcal
	            endif
	            unlock
	         endif
	         SELE Calen1
     	ENDSCAN
	ENDIF
	
	SELECT PRESU1
    GO TOP
    SCAN
      IF RIGHT(CODPART,2)='00'
         CV=RECNO()
         if alltrim(m.tipfun)='F'
  	    	VPAR = CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2)
   	    	SELE PRESU1
			SUM TOTAFE TO VSUMA FOR CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2) = VPAR
         ELSE
			VPAR = CODPRG+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2)
           	SELE PRESU1				
		    SUM TOTAFE TO VSUMA FOR CODPRG+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) = VPAR
     	 ENDIF
		 GO CV
		 IF RLOCK()
			REPLACE TOTAFE WITH VSUMA
		 ENDIF
		 UNLOCK
	  ENDIF
	ENDSCAN	
	GO TOP
    if vmues
		DO VAL_PRE
	endif	
ELSE
	    ******  CUANDO NO EXISTE CALENDARIO
		DO standby WITH 'No se tiene Calendario para este Mes'
		vdf =.f.
ENDIF
SELECT Itehc
set order to itehc1
ON KEY LABEL enter
RELEASE WINDOW _yy

RETURN vdf


PROCEDURE VAL_AFE
*----------------
PARAMETER VCPT,VTOT,VPART
PRIVATE OSW,VRET
VRET = .T.
ON KEY LABE F7
OSW = ALIAS()
SELECT CALEN1
LOCATE FOR CODPART=VCPT
IF FOUN()
	IF VALPART < TOTAFE+(VTOT-VPART)
	   DO STANDBY WITH 'No existe calendario para Asignar..'
	   DO VAL_PRE
	   sele (osw)
	   REPLACE VALPART WITH 0	
	   Vret=.f.
	else
	   replace totafe with  TOTAFE+(VTOT-VPART)
	endif
ELSE
   	DO STANDBY WITH 'No existe calendario Asignado..'
   	DO VAL_PRE
   	sele (osw)
    REPLACE VALPART WITH 0   	
   	Vret=.f.
ENDIF
sele (osw)
ON KEY LABEL F7 DO VAL_PRE
return &&VRET

PROCEDURE VAL_PRE
*----------------
PRIVATE ALS
ALS=ALIAS()
ON KEY LABEL F7
SELEC PRESU1
GO TOP
DEACTIVATE WINDOW Standby
vCodSub   = '   '
vCodAct   = '  '

FDX = SYS(3)+'.IDX'

IF ALLTRIM(m.tipfun)='F'
    INDEX ON CODSUBPR+codact+CODPART TO (FDX)
    GO TOP
    DEFINE WINDOW _yy FROM 04,01 TO 19,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	        XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':04,;
	        FRO=IIF(RIGHT(CODPART,2)='00' OR VALPART=0,CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' '),'') :H='Act':6  ,;	
			vv=' '+Codpart  :H='Partida' ,;
			cc=Valpart+cresup+transf   :H='Marco +/- Mod':P='99,999,999.99',;
			TotCal             :H='Con Calendar.':P='99,999,999.99',;
			Totafe             :H='   Afectado  ':P='99,999,999.99',;
			zz=valpart+cresup+transf-Totcal :H='    Saldo    ' :P='99,999,999.99'
	RELEASE WINDOW _YY			
ELSE
    INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (FDX)
    GO TOP
    DEFINE WINDOW _ww FROM 04,01 TO 19,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10
 
	BROWSE WINDOW _ww TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	        FRO = CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' ') : H='  SubPry  ' :10 ,;
			vv=' '+Codpart  :H='Partida' ,;                                       
			cc=Valpart+cresup+transf   :H='Marco +/- Mod':P='99,999,999.99',;
			TotCal             :H='Con Calendar.':P='99,999,999.99',;
			Totafe             :H='   Afectado  ':P='99,999,999.99',;
			zz=valpart+cresup+transf-Totcal :H='    Saldo    ' :P='99,999,999.99'
	RELEASE WINDOW _ww			
ENDIF		
SELECT (ALS)			
*ON KEY LABEL F10
ON KEY LABEL F7 DO VAL_PRE
return
