*-------------------------------------------------------------------------
* RegCal.Prg
* Registra Calendarios
* Autor: Ing. Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE calen    IN 2   ORDER TAG calen3   ALIAS calen
USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar2  ALIAS itepar
USE clase    IN 6   ORDER TAG clase1   ALIAS clase
USE Itetra   IN 10 ORDER TAG Itetra1   ALIAS Itetra
USE Itecre   IN 11 ORDER TAG Itecre1   ALIAS Itecre
USE IteHC    IN 12 ORDER TAG IteHC1   ALIAS IteHC
USE HOJMOD   IN 13 ORDER TAG hojmod1         ALIAS hojmod
USE repocal  IN 7                    &&  ALIAS REPO   
USE repcal1  IN 8                    &&  ALIAS rep1   

SELE 7
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 18 alias REPO EXCLUSIVE

SELE 8
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 19 alias REP1 EXCLUSIVE

SELECT calen
vtemp =RECNO()
vperiodo = RIGHT(STR(YEAR(DATE()),4),2)
vnummes  = '  '
vcodfte  = '   '
vtipfun  = ' '
vcodprg  = '  '
vcodpart = '  .  '
vcodsub = '   '
	   	*------Listado Marco Ejecucion ----
		DEFINE WINDOW lis_1 FROM 08,13 TO 16,67 DOUBLE ;
		TITLE ' °°  Marco Ejecuci¢n  °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1
		SET MESSA TO 23

		@  1,2 SAY '   Periodo : ' GET vperiodo   PICTURE '!!'
		@  2,2 SAY '   Fte Fin : ' GET vcodfte    PICTURE '!!!' VALID val_para(vcodfte,'CODFTE',' ',16,30)
		@  3,2 SAY '   Programa: ' GET vcodprg    PICTURE '!!'  VALID val_para(vcodprg,'CODPRG',' ',16,30)
		@  4,2 SAY 'Subprograma: ' GET vcodsub    PICTURE '!!!' valid iif(!empty(vcodsub),VAL_SUBP(vcodsub,'CODSUB'+vcodprg,' ',16,30),.t.) MESS '[blanco] =>Todos'
		@  5,2 SAY '     Nivel : ' GET vtipfun    PICTURE '!'   VALID val_para(vtipfun,'TIPFUN',' ',16,30)
		
		READ
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
		   CLOSE DATA
*-	DO vista
			RETURN
		ENDIF
	    ACTIVATE WINDOW Standby
		@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
		SELECT rep1
		vind = SYS(3) + '.IDX'                                                                   
		Zind = SYS(3) + '.IDX'                                                                   
		INDEX ON codprg+codsubpr+IIF(ALLTRIM(vtipfun)='I',codproy+codsupry,codact) + Codpart TO (vind)

		SELE ITEPAR
		IF ALLTRIM(vtipfun)='I'
		   SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODPROY, ITEPAR.DESSUPRY ,itepar.cresup,itepar.transf ,;
		      ITEPAR.CODSUPRY, ITEPAR.CODPRG,ITEPAR.CODSUBPR,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.UBICAC ;
			  FROM ITEPAR ;
			  WHERE  codfte=ALLTRIM(vcodfte)  AND periodo=ALLTRIM(vperiodo) AND codprg=ALLTRIM(vcodprg) AND tipfun = ALLTRIM(vtipfun) AND IIF(!EMPTY(VCODSUB),codsubpr=alltrim(vcodsub),.t.) ;
				INTO CURSOR PRESUX
		ELSE
		 	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO,itepar.cresup,itepar.transf, ;
		     	   ITEPAR.CODSUBPR, ITEPAR.CODPRG,ITEPAR.CODACT,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
	               FROM ITEPAR ;
			 	   WHERE codfte=ALLTRIM(vcodfte)  AND periodo=ALLTRIM(vperiodo) AND codprg=ALLTRIM(vcodprg) AND tipfun = ALLTRIM(vtipfun) AND IIF(!EMPTY(VCODSUB),codsubpr=alltrim(vcodsub),.t.) ;
					INTO CURSOR PRESUX
		ENDIF

		vInd = SYS(3) + '.DBF'
		COPY TO (vInd)
		use (vind) in 17 alias PRESU1 EXCLU
		SELE PRESU1
		IF ALLTRIM(vtipfun)='I'
		   INDEX ON codprg+codsubpr+codproy+codsupry+codpart TO (Zind)
        else		   
		   INDEX ON codprg+codsubpr+codact+Codpart TO (Zind)
		endif   
	
		IF ALLTRIM(vtipfun)='I'
		   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	    	  ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMPA,ITEHC.NUMMESHM,itehc.numhm ;
			  FROM ITEHC ;
			  WHERE  codfte=ALLTRIM(vcodfte)  AND codprg=ALLTRIM(vcodprg) AND tipfun = ALLTRIM(vtipfun) AND IIF(!EMPTY(VCODSUB),codsubpr=alltrim(vcodsub),.t.) AND ESTADO#'99';
					INTO CURSOR Hojax
		ELSE
		   SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,ITEHC.NUMPA,  ;
     	      ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
              FROM ITEHC ;
			  WHERE  codfte=ALLTRIM(vcodfte)  AND codprg=ALLTRIM(vcodprg) AND tipfun = ALLTRIM(vtipfun) AND IIF(!EMPTY(VCODSUB),codsubpr=alltrim(vcodsub),.t.) AND ESTADO#'99' ;
			  		INTO CURSOR HOJAX
		ENDIF

        zInd = SYS(3) + '.DBF'
        COPY TO (zInd)
        use (zind) in 18 alias hoja1 EXCLU
	
		SELE PRESU1
		GO TOP
		SCAN
			SCATTER MEMVAR		
			SELECT rep1
			m.valpres=m.valpart		
			IF F_APPD()
               GATHER MEMVAR
			ENDIF
			SELE PRESU1
	    ENDSCAN
		SELE REP1 
		GO TOP
		SELE hoja1
		GO TOP	
		
		SCAN
			SCATTER MEMVAR
            do case
               case m.estado='92'
                    Sele hojmod
                    seek hoja1.nummeshm+hoja1.numhm
                    if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                    else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                    endif  
                    selec hoja1 
*                    vPor = iif(m.tipope='-',-1,1) 
               case m.estado='94'
                    vPor = -1
               case m.estado='99'
                    vPor = 0 
               other
                    IF !empty(m.numpa)
                       vPor = -1
                    ELSE   
                       vPor = 1      
                    ENDIF 	
            endcase   		                 		                 		                     
			IF ALLTRIM(vtipfun)='I'
     		   vrep2 = m.codprg+m.codsubpr+m.codproy+m.codsupry+m.codpart
     		ELSE   
     		   vrep2 = m.codprg+m.codsubpr+m.codact+m.codanal
     		ENDIF   
		    STORE 0 TO 	m.C_01,m.C_02,m.C_03,m.C_04,m.C_05,m.C_06,m.C_07,m.C_08,m.C_09,m.C_10,m.C_11,m.C_12
			SELECT rep1
			SEEK ALLTRIM(vrep2)
			IF !FOUND()
				vcod='C_'+ALLTRIM(m.nummes)
				m.&vcod=vpor*m.valpart
				vkey = IIF(ALLTRIM(vtipfun)='I',m.codprg+m.codsubpr+m.codproy+m.codsupry+m.codpart,m.Codprg+m.Codsubpr+m.Codact+m.codanal)
				m.codpart=IIF(ALLTRIM(vtipfun)='I',m.codpart,m.codanal)
				APPEND BLANK
				GATHER MEMVAR
				SELE presu1
				GO TOP
				SEEK vkey
				IF FOUND()
					SELE rep1
					REPLACE valpres WITH presu1.valpart,CRESUP WITH presu1.CRESUP ,transf with presu1.transf
				else
    				SELE rep1
					REPLACE valpres WITH 0
	 			ENDIF
				m.valpart=0
			ELSE
				vcod='C_'+ALLTRIM(m.nummes)
				REPLACE &vcod WITH &vcod + vpor*m.VALPART 
				m.valpart = 0
            endif
			SELECT HOJA1
		ENDSCAN
        SELE REP1 
		
		SELE rep1
	    DEACTIVATE WINDOW Standby
		IF ALLTRIM(VTIPFUN)='I'
			DO reporte WITH 2,"LisCalEJ",' A nivel de Marco de Ejecucion '
		ELSE		
		   *LOCA FOR !EMPTY(CODACT)
		   *IF EOF() 	
		   *   GO TOP
		   *   DO reporte WITH 2,"MarEjef",' A nivel de Marco Presupuestal '		
		   *ELSE
			   GO TOP
			   DO reporte WITH 2,"MarEjefA",' A nivel de Marco Presupuestal '		
		   *ENDIF   
		endif	
CLOSE DATA
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
ON KEY LABEL F9
RESTORE SCREEN FROM principal
RETURN


PROCEDURE valcal
*-----------------
PARAMETER vnumcal,vsino
PRIVATE vfun
vfun = .T.
IF ALLTRIM(vsino)='S'
	m.codobra=PADL(ALLTRIM(STR(vnumcal,2)),2,'0')
	IF m.codobra = '00' OR EMPTY(m.codobra)
		vfun = .F.
	ENDIF
ENDIF
RETURN vfun

PROCEDURE buscprg
*----------------
*BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(m.periodo)+ALLTRIM(m.codprg)+ALLTRIM(m.codsubpr)+ALLTRIM(m.codproy)
SELECT maepre
SET ORDER TO 2
SEEK vkey
vproyecto = maepre.descri
SELECT calen
RETURN (vproyecto)


PROCEDURE buscpro
*----------------
*BUSCAR PROYECTO
*BROW
RC=RECNO()
PRIVATE vkey
vkey=ALLTRIM(periodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(codproy)
SELECT maepre
SET ORDER TO 2
SEEK vkey
vproyecto = maepre.descri
SELECT calen
GO RC
*BROW
RETURN (vproyecto)

FUNCTION sumaprg
*---------------
PARAMETER PART
totact = 0
prg = repo.codprg
vrec = RECNO()
GO TOP
SUM &part TO TOTAL FOR codprg = prg
GO vrec
RETURN TOTAL

FUNCTION sumact
*--------------
PARAMETER PART
prg  = repo.codprg+repo.codsubpr
vrec = RECNO()
GO TOP
SUM &part TO total1 FOR repo.codprg+repo.codsubpr = prg AND !EMPTY(codact)
GO vrec
RETURN total1


FUNCTION totact
*--------------
total2 = c_01+c_02+c_03+c_04+c_05+c_06+c_07+c_08+c_09+c_10+c_11+c_12+c_14
RETURN total2

FUNCTION totnew
*----------------
totnew = sumact('C_01')+sumact('C_02')+sumact('C_03')+sumact('C_04')+sumact('C_05')+sumact('C_06')+sumact('C_07')+sumact('C_09')+sumact('C_10')+sumact('C_11')+sumact('C_12')+sumact('C_14')
RETURN totnew

FUNCTION sumtotprg
*-----------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM valpart TO suma FOR codprg = vkey
GO vrec
RETURN suma

FUNCTION sumasuprg
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM valpart TO sumaq FOR codprg+codsubpr = vkey
GO vrec
RETURN sumaq

FUNCTION sumasubpy
*---------------
vkey = codprg + codsubpr + codproy + codsupry + codpart
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR codprg+codsubpr+codproy+codsupry + codpart = vkey
GO vrec
RETURN sumt

FUNCTION totsupry
*--------------
totalm = C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12
RETURN totalm

FUNCTION totsalspry
*--------------
PUBLIC totals
totals = valpres+CRESUP+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)
RETURN totals

FUNCTION sumsupry
*-----------------
PARAMETER PART
prg =rep1.codprg+rep1.codsubpr+rep1.codproy
vrec = RECNO()
GO TOP
SUM &part TO sumam FOR rep1.codprg+rep1.codsubpr+rep1.codproy=prg
GO vrec
RETURN sumam

FUNCTION sumspytot
*---------------
vkey = codprg + codsubpr + codproy &&+ codpart
vrec = RECNO()
GO TOP
*BROW
*SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12 TO sumt FOR codprg+codsubpr+codproy+codpart = vkey
SUM VALPART TO sumt FOR codprg+codsubpr+codproy+codpart = vkey
GO vrec
RETURN sumt

FUNCTION sumspyau
*---------------
vkey = codprg + codsubpr+codproy
vrec = RECNO()
GO TOP
SUM valpres+cresup TO sumau FOR codprg+codsubpr+codproy = vkey
GO vrec
RETURN sumau

FUNCTION salpry
*---------------
vkey = codprg + codsubpr+codproy
vrec = RECNO()
GO TOP
*SUM valpart+transf TO sum1 FOR codprg+codsubpr+codproy = vkey
*GO TOP
*SUM valpres+transf TO sum2 FOR codprg+codsubpr+codproy = vkey
*SUM valpRES+cresup+transf-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO SUM2 FOR codprg+codsubpr+codproy = vkey
SUM valpRES+cresup-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO SUM2 FOR codprg+codsubpr+codproy = vkey
GO vrec
RETURN sum2  &&-sum1

FUNCTION salsprg
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
*SUM valpart+transf TO sum1 FOR codprg+codsubpr = vkey
*GO TOP
*SUM valpres+transf TO sum2 FOR codprg+codsubpr = vkey
SUM valpRES+cresup-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO sum2 FOR codprg+codsubpr = vkey
GO vrec
RETURN sum2  &&- sum1

FUNCTION salprg
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
*SUM valpart+transf TO sum1 FOR codprg = vkey
*GO TOP
*SUM valpres+transf TO sum2 FOR codprg = vkey
SUM valpRES+cresup-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO sum2 FOR codprg = vkey
GO vrec
*RETURN sum2-sum1
RETURN sum2

FUNCTION sumspgau
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM valpres TO sumsg FOR codprg+codsubpr = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION crespgau
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM cresup TO sumsg FOR codprg+codsubpr = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION TRAspgau
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM TRANSF TO sumsg FOR codprg+codsubpr = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION sumactau
*---------------
vkey = codprg + codsubpr + codact
vrec = RECNO()
GO TOP
SUM valpres TO sumsg FOR codprg+codsubpr+codact = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION CREactau
*---------------
vkey = codprg + codsubpr + codact
vrec = RECNO()
GO TOP
SUM CRESUP TO sumsg FOR codprg+codsubpr+codact = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION TRAactau
*---------------
vkey = codprg + codsubpr + codact
vrec = RECNO()
GO TOP
SUM TRANSF TO sumsg FOR codprg+codsubpr+codact = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION VALCRE
*---------------
vkey = codprg + codsubpr + codact + LEFT(CODPART,2)
vrec = RECNO()
GO TOP
SUM CRESUP TO sumsg FOR codprg+codsubpr+codact+LEFT(CODPART,2)= vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION VALtra
*---------------
vkey = codprg + codsubpr + codact + LEFT(CODPART,2)
vrec = RECNO()
GO TOP
SUM transf TO sumsg FOR codprg+codsubpr+codact+LEFT(CODPART,2)= vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg


FUNCTION sumspg
*-----------------
PARAMETER PART
prg =rep1.codprg+rep1.codsubpr
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR rep1.codprg+rep1.codsubpr=prg AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION sumactx
*-----------------
PARAMETER PART
prg =rep1.codprg+rep1.codsubpr+rep1.codact
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR rep1.codprg+rep1.codsubpr+rep1.codact=prg AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg


FUNCTION sumspgtot
*---------------
vkey = codprg + codsubpr
vrec = RECNO()
GO TOP
SUM  C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12 TO sumt FOR codprg+codsubpr = vkey AND RIGHT(codpart,2)#'00' 
*SUM valpart+cresup+transf TO sumt FOR codprg+codsubpr = vkey AND RIGHT(codpart,2)#'00' 
GO vrec
RETURN sumt

FUNCTION sumprgau
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM valpres TO sumsg FOR codprg = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION CREprgau
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM CRESUP TO sumsg FOR codprg = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION TRAprgau
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM TRANSF TO sumsg FOR codprg = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg


FUNCTION sumar_prg
*-----------------
PARAMETER PART
prg =rep1.codprg
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR rep1.codprg=prg AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg

FUNCTION sumprgtot
*---------------
vkey = codprg
vrec = RECNO()
GO TOP
SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO sumt FOR codprg = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumt

FUNCTION sumSACtot
*---------------
vkey = CodSubPr+CodAct
vrec = RECNO()
GO TOP
SUM valpart TO sumt FOR CODSUBPR+codACT = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumt

PROCEDURE valpreS
*----------------
as = alias()
select itepar
set orde to iif(alltrim(vtipfun)='I',2,7)
vkeyF=alltrim(vPeriodo)+alltrim(vCodprg)+alltrim(vCodfte)+alltrim(vTipfun)+rep1.codsubpr+rep1.codact
vkeyI=alltrim(vPeriodo)+alltrim(vCodfte)+alltrim(vCodprg)+rep1.Codsubpr+rep1.codproy+rep1.codsupry
seek iif(alltrim(vtipfun)='I',vKeyI,vKeyF)
vtotal = 0
vcresu = 0
vtransf = 0
scan while iif(alltrim(vtipfun)='F',Periodo+CodPrg  +CodFte+TipFun  +CodSubpr+CodAct = vkeyF,Periodo+CodFte  +CodPrg+CodSubpr+CodProy +CodSuPry=vkeyI)
    if right(itepar.codpart,2) # '00'
  		vtotal = vtotal + valpart 
		vcresu = vcresu + cresup
		vtransf = vtransf + transf
	endif		
endscan
sele (as)
return (vtotal+vcresu+vtransf)      


PROCEDURE TOTALPRG
*-----------------
PARAMETER VHAY
vkey = &VHAY
TOTALT = 0
vrec = RECNO()
GO TOP
SCAN FOR &VHAY=VKEY AND RIGHT(codpart,2)#'00'
	 totalm = C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12     
	 TOTALT=TOTALT+TOTALM
ENDSC
GO TOP
GO vrec
RETURN TOTALT


PROCEDURE buscact
*----------------
PARAMETER vact
**BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(rep1.periodo)+ALLTRIM(rep1.codprg)+ALLTRIM(rep1.codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO maepre3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE rep1
RETURN (vactividad)

PROCEDURE PENDIENTE
*------------------
DO CASE
   CASE ESTADO = '00' &&aprobado
       REPLACE ESTADO WITH '10'
   CASE ESTADO = '10' &&solicitado
       REPLACE ESTADO WITH '20'
 * CASE ESTADO = '20' &&no aprobado
 *     REPLACE ESTADO WITH '00'       
ENDCAS
DO VISTA       
RETURN


PROCEDURE SUMTRN
*---------------
PARAMETER vkey,VLLAVE 
vrec = RECNO()
GO TOP
SUM FTETRN TO suma FOR &vLLave = vkey 
GO vrec
RETURN suma

PROCEDURE SUMprp
*---------------
PARAMETER vkey,VLLAVE 
vrec = RECNO()
GO TOP
SUM FTEprp TO suma FOR &vLLave = vkey 
GO vrec
RETURN suma

FUNCTION sumpat
*--------------
PARAMETER PART
prg =rep1.codprg+rep1.codsubpr+rep1.codact+left(rep1.codpart,2)
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR rep1.codprg+rep1.codsubpr+rep1.codact+left(rep1.codpart,2)=prg AND RIGHT(codpart,2)#'00' 
GO vrec
RETURN sumsg

