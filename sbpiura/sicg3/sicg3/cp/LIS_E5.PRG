** ---------------------------------------------------
** REPOE5.PRG
** Reporte Ejecucion E-5
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
USE IteHc   IN 8 ORDER TAG Itehc2          ALIAS Itehc
USE Hojmod  IN 9 ORDER TAG Hojmod1         ALIAS HojMod

*USE cresup  IN 10 ORDER TAG cresup1        ALIAS cresup
*USE itecre  IN 11 ORDER TAG itecre1        ALIAS itecre
*USE TraPar  IN 14 ORDER TAG trapar1        ALIAS trapar
*USE IteTra  IN 15 ORDER TAG IteTra1        ALIAS IteTra

public vnummes
*----
DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------
vtemp    = RECNO()
as=order()
vPeriodo  = '  '
vNumMes   = '  '
vCodPrg   = '  '
vCodFte   = '   '
vTipFun   = ' '
vCodSub   = '   '
vCodAct   = '  '
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
 @  9,2 say '      Actividad : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes    PICTURE '!!'  VALID Val_Para(vNumMes  ,'FECMES',' ',21,30) AND !EMPTY(vNumMes)
 @  3,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30) AND !EMPTY(vCodFte)
 @  4,21  GET vTipFun    PICTURE '!'   valid IIF(!empty(vtipfun),VAL_PARA(vTipFun  ,'TIPFUN',' ',21,30),.t.) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30) AND !EMPTY(vCodPrg) 
* @  6,21  GET vCodSub    PICTURE '!!!' VALID VAL_PARA(vCodSUB  ,'CODSUB'+VCODPRG,' ',21,30) AND !EMPTY(vCodPrg) 
 @  6,21  GET vCodSub    PICTURE '!!!' VALID IIF(!EMPTY(vCodSub),VAL_SUBP(vCodSub,'CODSUB'+VCodPrg,' ',21,30),.t.) WHEN !empty(vtipfun)
 @  7,21  GET vCodPry    PICTURE '!!!' WHEN VTIPFUN='I'
 @  8,21  GET vCodSpy    PICTURE '!!'  WHEN VTIPFUN='I'
 @  9,21  GET vCodAct    PICTURE '!!'  WHEN VTIPFUN='F'
 
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

    USE IN 4
    USE IN 7
    USE IN 8
    USE IN 9
    USE cresup  IN 10 ORDER TAG cresup1        ALIAS cresup
    USE itecre  IN 11 ORDER TAG itecre1        ALIAS itecre
    USE TraPar  IN 14 ORDER TAG trapar1        ALIAS trapar
    USE IteTra  IN 15 ORDER TAG IteTra1        ALIAS IteTra
    SELE PRESU1
    DO REPORTE WITH 2,"LIS_E5",' Listado E-5 '
    GO TOP
    IF YESNO('¨Imprime Detalle de Anulaciones?')
*       SET FILT TO IIF(RIGHT(CODPART,2)#'00',MES_01+MES_02+MES_03+MES_04+MES_05+MES_06+MES_07+MES_08+MES_09+MES_10+MES_11+mes_12#0,.T.) 
       SET FILT TO IIF(RIGHT(CODPART,2)#'00',M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+m_12#0,.T.) 
       DO REPORTE WITH 2,"LIS_AN",' Listado Anulaciones '
    ENDIF
 ENDIF
RETURN

PROCEDURE ACUMULA
*----------------
PARAMETER XTIPFUN

if !xtipfun$'IF'
   DO=sinfun(3)
else   
   DO=ANALIZA(3)
endif   
SELECT PRESU1
RETURN


PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodSub)+ALLTRIM(VCodPry)
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

PROCEDURE ANALIZA  && CON I o f
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
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*

RInd = SYS(3) + '.IDX'
SInd = SYS(3) + '.IDX'
TInd = SYS(3) + '.IDX'
ZInd = SYS(3) + '.IDX'

SELE ITEPAR
IF RLOCK()
*   REPLACE mes_01 with 0,mes_02 with 0,mes_03 with 0,mes_04 with 0,mes_05 with 0,mes_06 with 0,mes_07 with 0,mes_08 with 0,mes_09 with 0,mes_10 with 0,mes_11 with 0,mes_12 with 0   
    REPLACE m_01 with 0,m_02 with 0,m_03 with 0,m_04 with 0,m_05 with 0,m_06 with 0,m_07 with 0,m_08 with 0,m_09 with 0,m_10 with 0,m_11 with 0,m_12 with 0   
ENDIF
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10
IF ALLTRIM(m.tipfun)='I'
   SELECT ITEPAR.CODPART ,ITEPAR.VALPART,ITEPAR.PERIODO ,ITEPAR.CODPROY,ITEPAR.DESSUPRY,itepar.cresup,itepar.transf,;
	      ITEPAR.CODSUPRY,ITEPAR.CODPRG ,ITEPAR.CODSUBPR,ITEPAR.Codfte ,ITEPAR.Tipfun  ,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.TOTMES,;
	      ITEPAR.M_01  ,ITEPAR.M_02 ,ITEPAR.M_03  ,ITEPAR.M_04 ,ITEPAR.M_05  ,ITEPAR.M_06,ITEPAR.M_07,ITEPAR.M_08,ITEPAR.M_09,ITEPAR.M_10,ITEPAR.M_11,ITEPAR.M_12 ;
		  FROM ITEPAR ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vCodPry),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I';
				INTO CURSOR PRESUX
ELSE
 	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO,itepar.cresup,itepar.transf, ;
	     	   ITEPAR.CODSUBPR, ITEPAR.CODPRG,ITEPAR.CODACT,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.TOTMES,;
		       ITEPAR.M_01,ITEPAR.M_02,ITEPAR.M_03,ITEPAR.M_04,ITEPAR.M_05,ITEPAR.M_06,ITEPAR.M_07,ITEPAR.M_08,ITEPAR.M_09,ITEPAR.M_10,ITEPAR.M_11,ITEPAR.M_12 ;
               FROM ITEPAR ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,ITEPAR.CODPART='02',IIF(VTIPO=2,ITEPAR.CODPART='03',.T.)) AND TIPFUN='F';
					INTO CURSOR PRESUX
ENDIF
vInd = SYS(3) + '.DBF'
xInd = SYS(3) + '.DBF'

COPY TO (vInd)
COPY STRU TO (xInd)
use (vind) in 20 alias PRESU2 exclu 
use (xind) in 19 alias PRESU1 exclu

select presu1
INDEX ON CODPART TO (RIND)
select presu2
INDEX ON CODPART TO (SIND)
   
GO TOP


SCAN 
   vPart1 =CODPART
   vPart2 =CodPart
   vTotPre = 0
   DO WHILE  vPart1 = vPart2
      vTotpre = vTotpre + Valpart
      SKIP
      vPart1 =CODPART
   ENDDO
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

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEHC.NUMMES, ITEHC.CODPART, ITEHC.ESTADO,ITEHC.TIPOPE, ITEHC.VALPART, ITEHC.CODPROY,ITEHC.NUMPA,ITEHC.MESPA,ITEHC.MESPR,;
	      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ,ITEHC.NUMMESHM,itehc.numhm;
		  FROM ITEHC ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vCodPry),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND ESTADO#'99' AND TIPFUN='I' and between(val(nummes),1,val(m.nummes)) and iif(!empty(NumPr),MesPr<=alltrim(m.Nummes),.t.) AND iif(!empty(NumhM),NumMesHM<=alltrim(vNummes) and tipcom#'P',.t.) ;  
				INTO CURSOR Hojax
ELSE
 SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,ITEHC.NUMMESHM,itehc.numhm,ITEHC.NUMPA,ITEHC.MESPA,ITEHC.MESPR,;
     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
           FROM ITEHC ;
	 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,CODANAL='02',IIF(VTIPO=2,CODANAL='03',.T.)) AND ESTADO#'99' AND TIPFUN='F' and between(val(nummes),1,val(m.nummes)) and iif(!empty(NumPr),MesPr<=alltrim(m.Nummes),.t.) AND iif(!empty(NumhM),NumMesHM<=alltrim(vNummes) and tipcom#'P',.t.) ;
				INTO CURSOR HOJAX
ENDIF



vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 21 alias hoja1 exclu

SELE HOJA1

If alltrim(m.tipfun)='I'
   INDEX ON CODPART+CODSUBPR+CODPROY+CODSUPRY TO (TIND)
Else   
   INDEX ON CODANAL+CODSUBPR+CODACT TO (TIND)
endif   

GO TOP

DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	IF ALLTRIM(m.tipfun) = 'I'
       SCAN 
          vPart1 =HOJA1.CODPART &&OJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY
          vPart2 =HOJA1.CodPart &&+HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY
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
                case hoja1.estado='94'
                     IF mespr=ALLTRIM(Vnummes) and !empty(mespR) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)
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
                        vPor = -1	             
                     else   
                        vPor = 0	               
                     endif   
                OTHER
                     vPor = 1
             endcase   		                 		                 		                     
             iF (empty(hoja1.numPA) or mespa>nummes) and mespa#vnummes
               vTotafe = vTotafe + vpor*Valpart
             ENDIF  
			  if Iif(!empty(nummeshm) AND TIPOPE='+',nummeshm = alltrim(m.nummes),nummes = alltrim(m.nummes)) AND (empty(hoja1.numPA) or mespa>nummes) &&OR hoja1.nummeshm=alltrim(vnummes)  &&&
                vToTmes = vTotmes + vpor*valpart
             endif
             SKIP
             vPart1 =HOJA1.CODPART &&+HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY
          ENDD
          SKIP -1	 
          *----------
          vNmes=PADL(hoja1.NUMMES,2,'0')
          sele presu1
		  seek HOJA1.CODPART &&+hoja1.CODSUBPR+hoja1.CODPROY+hoja1.CODSUPRY
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
	ELSE  
       SCAN
	       vPart1 = HOJA1.CODANAL &&+hoja1.CODSUBPR+hoja1.CODACT
	       vPart2 = HOJA1.CODANAL &&+hoja1.CODSUBPR+hoja1.CODACT
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
                      SELECT HOJA1
      	              If hoja1.nummeshm=alltrim(vnummes) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)
			             vTotReb = vTotReb + iif(hoja1.tipope='-',1,0)*valpart	                  
			             DO REBA
			          endif
	             case hoja1.estado='94'
                     IF mespr=ALLTRIM(Vnummes) and !empty(mespR) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
	                     vTotReb = vTotReb + valpart
	                     DO REBA
	                  ENDIF   
	                  vPor = -1	                  
	             case hoja1.estado='99'
	                  vPor = 0 
	             case hoja1.estado='00'  AND !empty(hoja1.numPA) 
                     if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
                         vTotReb = vTotReb + valpart
                         DO REBA
                         vPor = -1 
                      ELSE 
                         vpor = 0
                      ENDIF   
                        
	             OTHER
	                  vPor = 1
	          endcase   		              
	          IF (empty(hoja1.numPA) or mespa>nummes) and mespa#vnummes
	            vTotafe = vTotafe + vpor*Valpart
	          ENDIF
			  if Iif(!empty(nummeshm) AND TIPOPE='+',nummeshm = alltrim(m.nummes),nummes = alltrim(m.nummes)) AND (empty(hoja1.numPA) or mespa>nummes) &&OR hoja1.nummeshm=alltrim(vnummes)  &&&
                 vToTmes = vTotmes + vpor*valpart
              endif
	          SKIP
	          vPart1 = HOJA1.CODANAL &&+hoja1.CODSUBPR+hoja1.CODACT
	       ENDD
	       SKIP -1
	       *--------------
	       vNmes=PADL(hoja1.NUMMES,2,'0')
           sele presu1
		   seek HOJA1.CODANAL &&+hoja1.CODSUBPR+hoja1.CODACT
		   if found()
			  if rlock()
	             replace totafe with vtotafe,totmes WITH vtotmes
                 *replace mes_&vnmes with mes_&vnmes+vtotREB
	          endif
	          unlock
	       endif   
		   *-------------------   
	       SELECT HOJA1
	    ENDSCAN
	ENDIF
	
	SELE HOJA1
	go top
	SCAN 
	    SELE PRESU1
	    IF alltrim(m.tipfun) = 'I'
		    SEEK Hoja1.CODPART &&+HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY
	    ELSE
		    SEEK Hoja1.Codanal &&+HOJA1.CODSUBPR+HOJA1.CODACT
	    ENDIF
	    IF !FOUND()
	        sele hoja1
			vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODPART,Hoja1.Codanal)
			vPart2 = iif(alltrim(m.tipfun)='I',HOJA1.CODPART,Hoja1.Codanal)
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
                     SELE HOJA1
				   case hoja1.estado='94'
                     IF mespr=ALLTRIM(Vnummes) and !empty(mespR) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
	                     vTotReb = vTotReb + valpart
	                  ENDIF   
	                  vPor = -1	                  
   	               case hoja1.estado='00'  AND !empty(hoja1.numPA) 
                     if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
                         vTotReb = vTotReb + valpart
                         vPor = -1	             
                     else   
                         vPor = 0
                     endif   
   		 	       OTHER
	  	              vPor = 1
		        endcase   		                 		                 		                     
	            iF (empty(hoja1.numPA) or mespa>nummes) and mespa#vnummes
	               vTotafe = vTotafe + vpor*Valpart
	            ENDIF
			    if Iif(!empty(nummeshm) AND TIPOPE='+',nummeshm = alltrim(m.nummes),nummes = alltrim(m.nummes)) AND (empty(hoja1.numPA) or mespa>nummes) &&OR hoja1.nummeshm=alltrim(vnummes)  &&&
                   vToTmes = vTotmes + vpor*valpart
                endif
		        
				SKIP	
				vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODPART,Hoja1.Codanal)
			ENDD
			SKIP -1
			vtipfun = m.tipfun
			scatter memvar
		    sele PRESU1
		    IF F_APPD()
		       gather memvar
			   REPLACE totafe with vtotafe,totmes with vtotmes
		       if alltrim(m.tipfun)='I'
		          REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
		       ELSE   
		          REPLACE CODPART WITH HOJA1.CODANAL ,valpart with 0
		       ENDIF   
		     ENDIF    	
	         UNLOCK
			 m.tipfun = vtipfun
		     sele hoja1
		 endif
	     sele hoja1 
		endscan
		use in 21

 ELSE
     ******  CUANDO NO EXISTE CALENDARIO
     DO standby WITH 'No se tiene Calendario para este Mes'
 	vdf =.f.
 ENDIF
 IF ALLTRIM(m.TipFun)='I'
   SELECT PRESU2
   SET UNIQ ON
   INDEX ON LEFT(CODPART,2) TO (ZIND)
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
	           if alltrim(m.tipfun)='F'
	  	    	  VPAR = LEFT(PRESU1.CODPART,2)
*				  SUM TOTAFE,totmes,mes_01,mes_02,mes_03,mes_04,mes_05,mes_06,mes_07,mes_08,mes_09,mes_10,mes_11,mes_12 TO VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
				  SUM TOTAFE,totmes,m_01,m_02,m_03,m_04,m_05,m_06,m_07,m_08,m_09,m_10,m_11,m_12 TO VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
	           ELSE
				  VPAR = LEFT(PRESU1.CODPART,2) 
*	           	  SUM VALPART,CRESUP,TRANSF,TOTAFE,totmes,mes_01,mes_02,mes_03,mes_04,mes_05,mes_06,mes_07,mes_08,mes_09,mes_10,mes_11,mes_12 TO vpartd,VCRE,VTRA,VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
	           	  SUM VALPART,CRESUP,TRANSF,TOTAFE,totmes,m_01,m_02,m_03,m_04,m_05,m_06,m_07,m_08,m_09,m_10,m_11,m_12 TO vpartd,VCRE,VTRA,VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
			   ENDIF    
			   GO DONDE
			   IF RLOCK()
*				  REPLACE TOTAFE WITH VSUMA,totmes WITH VMES,mes_01 with v1,mes_02 with v2,mes_03 with v3,mes_04 with v4,mes_05 with v5,mes_06 with v6,mes_07 with v7,mes_08 with v8,mes_09 with v9,mes_10 with v10,mes_11 with v11,mes_12 with v12
				  REPLACE TOTAFE WITH VSUMA,totmes WITH VMES,m_01 with v1,m_02 with v2,m_03 with v3,m_04 with v4,m_05 with v5,m_06 with v6,m_07 with v7,m_08 with v8,m_09 with v9,m_10 with v10,m_11 with v11,m_12 with v12
				  if alltrim(m.tipfun)='I'
				     replace valpart with vpartd,CRESUP WITH IIF(M.tipfun='I',Vcre,0),TRANSF WITH IIF(M.tipfun='I',VTRA,0)
				  endif   
			   ENDIF
			   UNLOCK
		    ENDIF
		ENDSCAN	
	
		GO TOP
*ELSE
*    ******  CUANDO NO EXISTE CALENDARIO
*    DO standby WITH 'No se tiene Calendario para este Mes'
*	vdf =.f.
*ENDIF
ON KEY LABEL enter
RELEASE WINDOW _yy
RETURN vdf

PROCEDURE REBA
*-------------
PRIVATE ALS
ALS=ALIAS()
vNmes=PADL(hoja1.NUMMES,2,'0')
sele presu1
Seek IIF(ALLTRIM(M.TIPFUN)='F',HOJA1.CODANAL,HOJA1.CODPART)
IF FOUND()
   replace mes_&vnmes with mes_&vnmes+vtotREB
ENDIF   
SELE (ALS)
vtotreb=0
RETURN

PROCEDURE REBAS
*-------------
PRIVATE ALS
ALS=ALIAS()
vNmes=PADL(hoja1.NUMMES,2,'0')
sele presu1
Seek IIF(ALLTRIM(HOJA1.TIPFUN)='F',HOJA1.CODANAL,HOJA1.CODPART)

IF FOUND()
   replace mes_&vnmes with mes_&vnmes+vtotREB
ENDIF   
SELE (ALS)
vtotreb=0
RETURN

**************************************************************************
* PARA TIPO DE GASTO = ' '
**************************************************************************

PROCEDURE sinfun
*----------------
PARAMETERS vtipO
PRIVATE WDF
WDF=.T.
wDF=roleoS(m.periodo+alltrim(vcodprg),VTIPO)
RETURN WDF


PROCEDURE roleoS
*--------------
parameter xvalor,vtipo
AS=ALIAS()
Private vdf 
vdf = .t.
ACTIVATE SCREEN
on key label F5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*

RInd = SYS(3) + '.IDX'
SInd = SYS(3) + '.IDX'
TInd = SYS(3) + '.IDX'
ZInd = SYS(3) + '.IDX'

SELE ITEPAR
IF RLOCK()
*  REPLACE mes_01 with 0,mes_02 with 0,mes_03 with 0,mes_04 with 0,mes_05 with 0,mes_06 with 0,mes_07 with 0,mes_08 with 0,mes_09 with 0,mes_10 with 0,mes_11 with 0,mes_12 with 0   
   REPLACE m_01 with 0,m_02 with 0,m_03 with 0,m_04 with 0,m_05 with 0,m_06 with 0,m_07 with 0,m_08 with 0,m_09 with 0,m_10 with 0,m_11 with 0,m_12 with 0   
ENDIF
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10
SELECT ITEPAR.CODPART ,ITEPAR.VALPART,ITEPAR.PERIODO ,itepar.cresup,itepar.transf,;
	      ITEPAR.CODPRG,ITEPAR.CODSUBPR,ITEPAR.CODACT,ITEPAR.CODPROY,ITEPAR.CODSUPRY,ITEPAR.Codfte ,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.TOTMES,;
	      ITEPAR.M_01  ,ITEPAR.M_02  ,ITEPAR.M_03  ,ITEPAR.M_04  ,ITEPAR.M_05  ,ITEPAR.M_06  ,ITEPAR.M_07  ,ITEPAR.M_08  ,ITEPAR.M_09  ,ITEPAR.M_10  ,ITEPAR.M_11  ,ITEPAR.M_12 ;
		  FROM ITEPAR ;
		  WHERE  codfte+periodo+codprg ;
  				 = alltrim(m.codfte) + xvalor AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) ;
				INTO CURSOR PRESUX
vInd = SYS(3) + '.DBF'
xInd = SYS(3) + '.DBF'

COPY TO (vInd)
COPY STRU TO (xInd)
use (vind) in 20 alias PRESU2 exclu 
use (xind) in 29 alias PRESU1 exclu

select presu1
INDEX ON CODPART TO (RIND)
select presu2
INDEX ON CODPART TO (SIND)
   
GO TOP
SCAN 
   vPart1 =CODPART
   vPart2 =CodPart
   vTotPre = 0
   DO WHILE  vPart1 = vPart2
      vTotpre = vTotpre + Valpart
      SKIP
      vPart1 =CODPART
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

SELECT ITEHC.TIPFUN,ITEHC.NUMMES,ITEHC.CODPART,ITEHC.CODANAL,itehc.estado,itehc.tipope , ITEHC.VALPART,ITEHC.NUMMESHM,ITEHC.NUMHM,ITEHC.NUMPA,ITEHC.MESPA,ITEHC.MESPR,;
   	   ITEHC.CODPRG,ITEHC.Codfte,ITEHC.TOTCAL ;
       FROM ITEHC ;
	 	   WHERE codfte+periodo+codprg ;
  				 = alltrim(m.codfte) + xValor  AND iif(VTIPO=1,CODANAL='02',IIF(VTIPO=2,CODANAL='03',.T.)) AND ESTADO#'99' and between(val(nummes),1,val(m.nummes)) and iif(!empty(NumPr),MesPr<=alltrim(m.Nummes),.t.) AND iif(!empty(NumhM),NumMesHM<=alltrim(vNummes) and tipcom#'P',.t.) ; 
				INTO CURSOR HOJAX



vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 21 alias hoja1 exclu

SELE HOJA1

INDEX ON IIF(tipfun='I',CODPART,CODANAL) TO (TIND)
GO TOP


DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
    SCAN 
       vPart1 = IIF(tipfun='I',HOJA1.CODPART,HOJA1.CODANAL)
       vPart2 = IIF(tipfun='I',HOJA1.CODPART,HOJA1.CODANAL)
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
                  SELECT HOJA1
                  If hoja1.nummeshm=alltrim(vnummes) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)
     	             vTotReb = vTotReb + iif(hoja1.tipope='-',1,0)*valpart	                  
			         DO REBAS
			      endif
             case hoja1.estado='94'
                     IF mespr=ALLTRIM(Vnummes) and !empty(mespR) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
	                     vTotReb = vTotReb + valpart
	                     DO REBAS
	                  ENDIF   
	                  vPor = -1	                  
             case hoja1.estado='99'
                  vPor = 0 
             case hoja1.estado='00' AND !empty(hoja1.numPA) 
                  if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
                        vTotReb = vTotReb + valpart
                        DO REBA
                        vPor = -1	             
                   else   
                        vPor = 0	               
                   endif   
             OTHER
                  vPor = 1
          endcase   		                 		                 		                     

          IF (empty(hoja1.numPA) or mespa>nummes) and mespa#vnummes
             vTotafe = vTotafe + vpor*Valpart
          ENDIF  
 		  if Iif(!empty(nummeshm) AND TIPOPE='+',nummeshm = alltrim(m.nummes),nummes = alltrim(m.nummes)) AND (empty(hoja1.numPA) or mespa>nummes) &&OR hoja1.nummeshm=alltrim(vnummes)  &&&
             vToTmes = vTotmes + vpor*valpart
          endif
          SKIP
          vPart1 =IIF(tipfun='I',HOJA1.CODPART,HOJA1.CODANAL)
       ENDD
       SKIP -1	 
          *----------
       vNmes=PADL(hoja1.NUMMES,2,'0')
       sele presu1
       seek IIF(hoja1.tipfun='I',HOJA1.CODPART,HOJA1.CODANAL)
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
	
	go top
	SCAN 
	    SELE PRESU1
	    SEEK IIF(HOJA1.tipfun='I',HOJA1.CODPART,HOJA1.CODANAL)
	    IF !FOUND()
	        sele hoja1
			vPart1 = iif(HOJA1.tipfun='I',HOJA1.CODPART,Hoja1.Codanal)
			vPart2 = iif(HOJA1.tipfun='I',HOJA1.CODPART,Hoja1.Codanal)
			vTotAfe = 0
	        vtotmes = 0			
	        
			DO WHILE  vPart1 = vPart2 
				do case
				   case hoja1.estado='92'
			         vPor = iif(Hoja1.tipope='-',-1,1) 
				   case hoja1.estado='94'
   	      		     vPor = -1
		           case hoja1.estado='00' AND !empty(hoja1.numPA) 
                        if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
                           vTotReb = vTotReb + valpart
                           DO REBA
                           vPor = -1	             
                        else   
                           vPor = 0	               
                        endif   
			         * vTotReb = vTotReb + valpart	                  
	                 * vPor = -1	             
   	 	           OTHER
   	              vPor = 1
		        endcase   		                 		                 		                     
  	            iF (empty(hoja1.numPA) or mespa>nummes) and mespa#vnummes
	               vTotafe = vTotafe + vpor*Valpart
	            ENDIF
                if Iif(!empty(nummeshm) AND TIPOPE='+',nummeshm = alltrim(m.nummes),nummes = alltrim(m.nummes)) AND (empty(hoja1.numPA) or mespa>nummes) &&OR hoja1.nummeshm=alltrim(vnummes)  &&&
                   vToTmes = vTotmes + vpor*valpart
                endif

				SKIP	
				vPart1 = iif(tipfun='I',HOJA1.CODPART,Hoja1.Codanal)
				
			ENDD
			SKIP -1
			scatter memvar
		    sele PRESU1
		    IF F_APPD()
		       gather memvar
			   REPLACE totafe with vtotafe,totmes with vtotmes
               if hoja1.tipfun='I'
		          REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
		       ELSE   
		          REPLACE CODPART WITH HOJA1.CODANAL ,valpart with 0
		       ENDIF   
		     ENDIF    	
	         UNLOCK
		     sele hoja1
		 endif
	     sele hoja1 
		endscan
		use in 21
	    SELECT PRESU2
		SET UNIQ ON
		INDEX ON LEFT(CODPART,2) TO (ZIND)
		GO TOP
		SCAN
		   SCATTER MEMVAR
		   SELE PRESU1
		   SEEK left(presu2.codpart,2)+'.00'
		   IF !FOUN()
		      IF F_APPD()
		         m.codpart = left(presu2.codpart,2)+'.00'
		         m.valpart = 0
		         GATHEr memvar         
		      endif
		      unlock
		   ENDIF   
		   select presu2 
		ENDSCAN
		set uniq off
	    SELECT PRESU1
	    GO TOP
	    SCAN
      		IF RIGHT(PRESU1.CODPART,2)='00'
			   dONDE = RECNO()
			   VPAR = LEFT(PRESU1.CODPART,2) 
*              SUM VALPART,TOTAFE,totmes,mes_01,mes_02,mes_03,mes_04,mes_05,mes_06,mes_07,mes_08,mes_09,mes_10,mes_11,mes_12 TO vpartd,VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
               SUM VALPART,TOTAFE,totmes,m_01,m_02,m_03,m_04,m_05,m_06,m_07,m_08,m_09,m_10,m_11,m_12 TO vpartd,VSUMA,VMES,v1,v2,v3,v4,v5,v6,v7,v8,v9,v10,v11,v12 FOR LEFT(PRESU1.CODPART,2) = VPAR AND RIGHT(PRESU1.CODPART,2)#'00'
			   GO DONDE
			   IF RLOCK()
*				  REPLACE TOTAFE WITH VSUMA,totmes WITH VMES,mes_01 with v1,mes_02 with v2,mes_03 with v3,mes_04 with v4,mes_05 with v5,mes_06 with v6,mes_07 with v7,mes_08 with v8,mes_09 with v9,mes_10 with v10,mes_11 with v11,mes_12 with v12
				  REPLACE TOTAFE WITH VSUMA,totmes WITH VMES,m_01 with v1,m_02 with v2,m_03 with v3,m_04 with v4,m_05 with v5,m_06 with v6,m_07 with v7,m_08 with v8,m_09 with v9,m_10 with v10,m_11 with v11,m_12 with v12
 				  if valpart=0
 				     replace valpart with vpartd
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
RELEASE WINDOW _yy
RETURN vdf
