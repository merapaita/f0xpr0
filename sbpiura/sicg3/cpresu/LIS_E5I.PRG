** ---------------------------------------------------
** REPOE5.PRG
** Reporte Ejecucion E-5
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------
* Abrir Archivos de trabajo
USE Parmae  IN 1  ORDER TAG Parmae1         ALIAS parma
USE Calen   IN 2  ORDER TAG Calen1          ALIAS Calen
USE Maepre  IN 3  order tag Maepre1         ALIAS Maepre
USE MaePar  IN 4  ORDER TAG MaePar1         ALIAS MaePar
USE ItePar  IN 5  ORDER TAG ItePar1         ALIAS ItePar
USE Clase   IN 6  ORDER TAG Clase1          ALIAS Clase
USE HojCon  IN 7  ORDER TAG Hojcon1         ALIAS Hoja
USE IteHc   IN 8  ORDER TAG Itehc2          ALIAS Itehc
USE Hojmod  IN 9  ORDER TAG Hojmod1         ALIAS HojMod
public vnummes
*----
DO Inicia
DO salida

RETURN


PROCEDURE Inicia
*--------------
vtemp    = RECNO()
as=order()
vPeriodo  = '  '
vNumMes   = '  '
vCodFte   = '   '
vTipFun   = 'I'
vCodPrg   = '  '
vCodSub   = '   '
vCodPry   = '   '
vCodSpy   = '  '

 DEFINE WINDOW Lis FROM 6,10 TO 17,70 DOUBLE ;
 TITLE ' °° Listado E-5 °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '            Mes : '
 @  3,2 say '         Fuente : '
 @  4,2 say '         TipFun : ' 
 @  5,2 say '       Programa : ' 
 @  6,2 say '    SubPrograma : '
 @  7,2 say '       Proyecto : '
 @  8,2 say '    SubProyecto : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes    PICTURE '!!'  VALID Val_Para(vNumMes  ,'FECMES',' ',21,30) AND !EMPTY(vNumMes)
 @  3,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30) AND !EMPTY(vCodFte)
 @  4,21  GET vTipFun    PICTURE '!'   VALID VAL_PARA(vTipFun  ,'TIPFUN',' ',21,30) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30) AND !EMPTY(vCodPrg) 
 @  6,21  GET vCodSub    PICTURE '!!!' VALID IIF(!EMPTY(vCodSub),VAL_SUBP(vCodSub,'CODSUB'+VCodPrg,' ',21,30),.t.)
 @  7,21  GET vCodPry    PICTURE '!!!' WHEN VTIPFUN='I'
 @  8,21  GET vCodSpy    PICTURE '!!' WHEN VTIPFUN='I'
 
 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF EMPTY(vNumMES) .or. LASTKEY()=27
    RETURN
 ELSE
    vInd = SYS(3) + '.IDX'
    xInd = SYS(3) + '.IDX'
    vAcumul = 0
    vAcuAnu = 0
    m.Periodo  = vperiodo
	m.TipFun   = vTipfun 
	m.Codfte   = vcodfte
	vNummes    = ALLTRIM(vnummes)
	m.Nummes   = vnummes
    DO acumula with alltrim(vtipfun)
    
    USE IN 2
    USE IN 4
    USE IN 6
    USE IN 7
    USE IN 8
    USE IN 9
	USE cresup  IN 10 ORDER TAG cresup1        ALIAS cresup
	USE Itecre  IN 11 ORDER TAG itecre1         ALIAS itecre
	USE TraPar  IN 12 ORDER TAG trapar1        ALIAS trapar
	USE Itetra  IN 13 ORDER TAG itetra1         ALIAS itetra
	
    SELECT iteCre 
    SET FILTER TO MONTH(FECHA)<=VAL(VNumMes) 
    SELECT itetra
    SET FILTER TO MONTH(FECHA)<=VAL(VNumMes)
    
    SELECT PRESU1
    SET FILT TO VALPART+TOTAFE+TOTMES#0
    DO REPORTE WITH 2,"LIS_E5I",' Listado E-5 '
    GO TOP
    
    IF YESNO('¨Imprime Detalle de Anulaciones?')
       SET FILTER TO (MES_01+MES_02+MES_03+MES_04+MES_05+MES_06+MES_07+MES_08+MES_09+MES_10+MES_11+MES_12) > 0.00
       GO TOP
       DO REPORTE WITH 2,"LIS_ANi",' Listado Anulaciones '
    ENDIF
    
 ENDIF
RETURN

PROCEDURE ACUMULA
*----------------
PARAMETER XTIPFUN
DO=ANALIZA(3)
SELECT PRESU1
RETURN


PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodSub)+ALLTRIM(vCodPry)
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
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)


PROCEDURE buscact
*----------------
PARAMETER vact
**BUSCAR ACTIVIDAD
AS=ALIAS()
PRIVATE vkey
vkey=ALLTRIM(periodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO MAEPRE3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE (AS)
RETURN (vactividad)
      
*---------------------------------------------------------------------------

PROCEDURE ANALIZA
*----------------
PARAMETERS vtipO
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=roleo(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)                , alltrim(vcodact),VTIPO)
ELSE	
	WDF=roleo(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vCodPry) , alltrim(vCodSpy),VTIPO)
ENDIF
RETURN WDF


PROCEDURE roleo
*--------------
parameter xvalor,proact,vtipo
AS=ALIAS()
Private vdf 
vdf = .t.
ACTIVATE SCREEN
on key label F5 on key
*ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
RInd = SYS(3) + '.IDX'
SInd = SYS(3) + '.IDX'
TInd = SYS(3) + '.IDX'
ZInd = SYS(3) + '.IDX'

SELE ITEPAR
IF RLOCK()
   REPLACE mes_01 with 0,mes_02 with 0,mes_03 with 0,mes_04 with 0,mes_05 with 0,mes_06 with 0,mes_07 with 0,mes_08 with 0,mes_09 with 0,mes_10 with 0,mes_11 with 0,mes_12 with 0   
ENDIF
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10
SELECT ITEPAR.CODPART ,ITEPAR.VALPART,ITEPAR.PERIODO ,ITEPAR.CODPROY,ITEPAR.DESSUPRY,itepar.cresup,itepar.transf,;
	      ITEPAR.CODSUPRY,ITEPAR.CODPRG ,ITEPAR.CODSUBPR,ITEPAR.Codfte ,ITEPAR.Tipfun  ,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.TOTMES,;
	      ITEPAR.MES_01  ,ITEPAR.MES_02 ,ITEPAR.MES_03  ,ITEPAR.MES_04 ,ITEPAR.MES_05  ,ITEPAR.MES_06,ITEPAR.MES_07,ITEPAR.MES_08,ITEPAR.MES_09,ITEPAR.MES_10,ITEPAR.MES_11,ITEPAR.MES_12 ;
		  FROM ITEPAR ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vCodPry),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I';
				INTO CURSOR PRESUX

vInd = SYS(3) + '.DBF'
xInd = SYS(3) + '.DBF'
COPY TO (vInd)
COPY STRU TO (xInd)
use (vind) in 20 alias PRESU2
use (xind) in 29 alias PRESU1

select presu1
INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (RIND)
select presu2
INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (SIND)
   
GO TOP
SCAN 
   vPart1 =CODSUBPR+CODPROY+CODSUPRY+CODPART
   vPart2 =CODSUBPR+CODPROY+CODSUPRY+CodPart
   vTotPre = 0
   DO WHILE  vPart1 = vPart2
      vTotpre = vTotpre + Valpart
      SKIP
      vPart1 =CODSUBPR+CODPROY+CODSUPRY+CODPART
   ENDD
   SKIP -1	 
   *----------
   scatter memvar
   sele presu1
   seek VPART2
   if found()
	  if rlock()
	     gather memvar
         replace valpart with valpart+vtotpre,CODPART WITH PRESU1.CODPART
      endif
   else   
   	  if f_appD()
	     gather memvar
         replace valpart with vtotpre,CODPART WITH PRESU1.CODPART
      endif
   endif
   unlock
   *----------
   SELE presu2
ENDSCAN

SELECT ITEHC.NUMMES, ITEHC.CODPART, ITEHC.ESTADO,ITEHC.TIPOPE, ITEHC.VALPART, ITEHC.CODPROY,ITEHC.NUMPA,ITEHC.MESPA,ITEHC.MESPR,;
	      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ,ITEHC.NUMMESHM,itehc.numhm ;
		  FROM ITEHC ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vCodPry),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND ESTADO#'99' AND TIPFUN='I' and between(val(nummes),1,val(m.nummes)) AND iif(!empty(NumPr),MesPr<=alltrim(m.Nummes),.t.) AND iif(!empty(NumhM),NumMesHM<=alltrim(vNummes) and tipcom#'P',.t.) ;  
				INTO CURSOR Hojax
				
				
vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 21 alias hoja1

SELE HOJA1
INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (TIND)
GO TOP
IF !EOF()
	vTotAfe = 0
       SCAN 
          vPart1 =HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
          vPart2 =HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
          vTotAfe = 0
          vtotmes = 0
          vTotReb = 0
          DO WHILE  vPart1 = vPart2
             do case
                case hoja1.estado='92'
                     Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
      	             If hoja1.nummeshm=alltrim(vnummes) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)
                        vTotReb = vTotReb + iif(hoja1.tipope='-',1,0)*valpart
                        DO REBA
                     endif   
                case hoja1.estado='93'
                     vpor = iif(nummeshm<alltrim(m.nummes),iif(hoja1.tipope='-',-1,1),0)
                     vToTmes = vTotmes + vpor*valpart
                case hoja1.estado='94'
                     IF mespr=ALLTRIM(Vnummes) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)
	                     vTotReb = vTotReb + valpart
	                     DO REBA
	                  ENDIF   
	                  vPor = -1	                  
                case hoja1.estado='99'
                     vPor = 0 
	             case hoja1.estado='00' AND !empty(hoja1.numPA) 
                     if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)
                        vTotReb = vTotReb + valpart
                        DO REBA
                     ELSE
                        vpor = 1   
                     endif   
                     vPor = -1	             
                OTHER
                     vPor = 1
             endcase   		       
             iF (empty(hoja1.numPA) or mespa>Vnummes) AND mespa#vnummes 
             *                           10     05         10     11   
                vTotafe = vTotafe + vpor*Valpart
             ENDIF  
 	         if Iif(!empty(nummeshm) AND TIPOPE='+',nummeshm = alltrim(m.nummes),nummes = alltrim(m.nummes)) AND (empty(hoja1.numPA) or mespa>nummes)
                vToTmes = vTotmes + vpor*valpart
             endif

             SKIP
             vPart1 =HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
          ENDD
          SKIP -1	 
          *----------
          vNmes=PADL(hoja1.NUMMES,2,'0')
          sele presu1
		  seek hoja1.CODSUBPR+hoja1.CODPROY+hoja1.CODSUPRY+HOJA1.CODPART
		  if found()
		     if rlock()
                replace totafe with vtotafe,Totmes WITH vtotmes 
                *replace mes_&vnmes with mes_&vnmes+vtotreb
             endif
             unlock
          endif   
          *----------
          SELE hoja1
       ENDSCAN
	
	SELE HOJA1
	go top
	SCAN 
	    SELE PRESU1
	    SEEK HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
	    IF !FOUND()
	        sele hoja1
			vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
			vPart2 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
			vTotAfe = 0
	        vtotmes = 0			
			DO WHILE  vPart1 = vPart2 
				do case
				   case hoja1.estado='92'
                     Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
*			         vPor = iif(itehc.tipope='-',-1,1) 
				   case hoja1.estado='94'
   	              vPor = -1
   	 	       OTHER
   	              vPor = 1
		        endcase   		                 		                 		                     
   				vTotafe = vTotafe + vpor*hoja1.Valpart
				if nummes = alltrim(m.nummes)
                   vToTmes = vTotmes + vpor*valpart
                endif
				SKIP	
				vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
			ENDD
			SKIP -1
			vtipfun = m.tipfun
			scatter memvar
		    sele PRESU1
		    IF F_APPD()
		       gather memvar
			   REPLACE totafe with vtotafe,totmes with vtotmes
	           REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
	        ENDIF    	
	        UNLOCK
			m.tipfun = vtipfun
		    sele hoja1
		 endif
	     sele hoja1 
		endscan
		use in 21
		
		IF ALLTRIM(m.TipFun)='I'
		   SELECT PRESU2
		   SET UNIQ ON
		   INDEX ON CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) TO (ZIND)
		   GO TOP
		   SCAN
		      SCATTER MEMVAR
		      SELE PRESU1
		      IF F_APPD()
		         m.codpart = left(presu2.codpart,2)+'.00'
		         m.valpart = 0
		         GATHEr memvar         
		      endif
		      unlock
		      select presu2 
		   ENDSCAN
		   set uniq off
		   SELE PRESU1
		ENDIF
		
	    SELECT PRESU1
	    GO TOP
	    SCAN
      		IF RIGHT(PRESU1.CODPART,2)='00'
	           DONDE = RECNO()
			   VPAR = PRESU1.CODSUBPR+PRESU1.CODPROY+PRESU1.CODSUPRY+LEFT(PRESU1.CODPART,2) 
	           SUM VALPART,CRESUP,TRANSF,TOTAFE,totmes,mes_01,mes_02,mes_03,mes_04,mes_05,mes_06,mes_07,mes_08,mes_09,mes_10,mes_11,mes_12 TO vpartd,VCRE,VTRA,VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR PRESU1.CODSUBPR+PRESU1.CODPROY+PRESU1.CODSUPRY+LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
			   GO DONDE
			   IF RLOCK()
				  REPLACE TOTAFE WITH VSUMA,totmes WITH VMES,mes_01 with v1,mes_02 with v2,mes_03 with v3,mes_04 with v4,mes_05 with v5,mes_06 with v6,mes_07 with v7,mes_08 with v8,mes_09 with v9,mes_10 with v10,mes_11 with v11,mes_12 with v12
				  if alltrim(m.tipfun)='I'
				     replace valpart with vpartd,CRESUP WITH IIF(M.tipfun='I',Vcre,0),TRANSF WITH IIF(M.tipfun='I',VTRA,0)
				  endif   
			   ENDIF
			   UNLOCK
		    ENDIF
		ENDSCAN	
        
		GO TOP
ELSE
    ******  CUANDO NO EXISTE CALENDARIO
    DO standby WITH 'No se tiene Calendario para este Mes'
	vdf =.f.
ENDIF
ON KEY LABEL enter
DEACTIVATE WINDOW Standby	
RELEASE WINDOW _yy
RETURN vdf

PROCEDURE REBA
*-------------
PRIVATE ALS
ALS=ALIAS()
vNmes=PADL(hoja1.NUMMES,2,'0')
sele presu1
Seek HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CODPART
IF FOUND()
   replace mes_&vnmes with mes_&vnmes+vtotREB
ENDIF   
SELE (ALS)
vtotreb=0
RETURN

FUNCTION Xcresupi
*--------------
private vkey
VALIAS = ALIAS()
vCodpart=codpart
vKey= IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,''),CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,''))

* PRIMERO CON CREDITOS SUPLEMENTARIOS

SELECT itecre
SET ORDE TO IIF(ALLTRIM(vTipFun)='I',2,3)

SELECT cresup
SET RELATION TO PERIODO+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+ALLTRIM(TIPFUN) INTO itecre

SELECT itecre
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
IF RIGHT(vCodpart,2)='00'
	SUM monasig TO vcresup FOR left(Codpart,2)=left(vcodpart,2) AND RIGHT(Codpart,2) # '00' AND MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Cresup.estado # '00'  AND RIGHT(CRESUP.Codpart,2)#'00'
ELSE	
	SUM monasig TO vcresup FOR Codpart=vcodpart	AND MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Cresup.estado # '00' AND RIGHT(CRESUP.Codpart,2)#'00'
ENDIF	

* AHORA CON TRANSFERENCIAS

SELECT itetra
SET ORDE TO IIF(ALLTRIM(vTipFun)='I',2,3)
SELECT trapar
SET RELATION TO PERIODO+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+ALLTRIM(TIPFUN) INTO itetra

SELECT itetra
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
vtransf=0
IF RIGHT(vCodpart,2)='00'
	SCAN 
		IF MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Trapar.estado # '00' AND LEFT(Codpart,2)=LEFT(vCodpart,2)
			vtransf=vtransf+iif(tipope='-',-1,1)*montra
		ENDIF
	ENDSCAN
ELSE	
	SCAN 
		IF MONTH(fecha)<=VAL(vNumMes) AND IIF(ALLTRIM(vTipFun)='I',CODPRG+CODFTE+IIF(!empty(vCodSub),CODSUBPR,'')+IIF(!empty(vCodPry),CODPROY,'')=vkey,CODPRG+CODFTE+IIF(!EMPTY(vCodSub),CODSUBPR,'')+IIF(!EMPTY(vCodAct),CODACT,'')=vkey) AND Trapar.estado # '00' AND Codpart=vCodpart
			vtransf=vtransf+iif(tipope='-',-1,1)*montra
		ENDIF
	ENDSCAN
ENDIF	
vtotalct=vcresup+vtransf
SELECT (vAlias)
RETURN vtotalct
