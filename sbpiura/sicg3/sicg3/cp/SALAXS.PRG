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
USE ItePar   IN 5   ORDER TAG ItePar1  ALIAS ItePar
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE Calen    IN 8   order tag calen1   ALIAS calen
USE Clase    IN 10  order tag Clase1   ALIAS Clase
USE Maepre   IN 11  order tag Maepre1  ALIAS Maepre
USE astpre   IN 13  order tag Astpre5  ALIAS AstPre
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
*USE hojmod   IN 15  order tag hojmod1  ALIAS HOJMOD
*USE PteAnu   IN 16  order tag PteAnu7  ALIAS AnuPA
USE Astaux_P IN 15                     ALIAS AST_P
USE Astaux   IN 16                     ALIAS AST

*USE ASTAUX   IN 17                     ALIAS AST

SELECT Astpre
SET RELA TO NUMMES+NUMREF INTO ITEHC

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
vPeriodo  = '  '
vNumMes2  = '  '
vTipFun   = ' '
vCodPrg   = '  '
vCodFte   = '   '
vCodSub   = '   '
vSubpry   = '  '
vProyec   = '   '
vCodact   = '  '
vCodPart  = '00.00'

 DEFINE WINDOW Lis FROM 5,12 TO 18,70 DOUBLE ;
 TITLE ' °° Auxiliar Standard °° ' FLOAT COLOR SCHEME 5

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
 @  2,21  GET vNumMes2   PICTURE '!!'  VALID Val_Para(vNumMes2 ,'FECMES','C',31,20,4) AND !EMPTY(vNumMes2) 
 @  3,21  GET vtipfun    PICTURE '!'   valid Val_Para(vTipfun  ,'TIPFUN',' ',21,20) 
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30,4) AND !EMPTY(vCodFte) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30,4) AND !EMPTY(vCodPrg)
 @  6,21  GET vCodSub    PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+VCodPrg,' ',21,28) OR EMPTY(vCodPrg)
 @  7,21  GET vProyec    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I'
 @  8,21  GET vSubpry    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='I'
 @  9,21  GET vCodact    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'
 @ 10,21  GET vCodPart   PICTURE '!!.!!'

 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF  EMPTY(vNumMES2) .or. LASTKEY()=27
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
    USE (VDBF) IN 16  ALIAS AST EXCLU
    
    SELECT AST_P
    vdbf = sys(3)+'.dbf'
    xidx = sys(3)+'.idx'
    copy stru to (vdbf)
    USE (VDBF) IN 15  ALIAS AST_P EXCLU
    
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
	  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND  ITEHC.ESTADO#'99' AND ITEHC.TIPFUN='I' AND IIF(ESTADO='92',VAL(NUMMESHM)=VAL(VNUMMES2),VAL(NUMMES)=VAL(VNUMMES2)) AND IIF(!EMPTY(MESPA),VAL(MESPA)=VAL(vnummes2),.t.) and IIF(!EMPTY(MESPR),VAL(MESPR)=VAL(vnummes2),.t.) and IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.)  AND ;
	  				 IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart) ;
					INTO CURSOR Hojax
	ELSE
	 	SELECT ITEHC.NUMHC,ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART, itehc.tipdoc, ;
	     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm, ITEHC.MESPA,ITEHC.NUMPA,ITEHC.MESPR,ITEHC.NUMPR ,ITEHC.OPERAC,ITEHC.TIPCOM,ITEHC.MESOCOS;
	           FROM ITEHC ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND ITEHC.ESTADO#'99' AND ITEHC.TIPFUN='F' AND IIF(ESTADO='92',VAL(NUMMESHM)=VAL(VNUMMES2),VAL(NUMMES)=VAL(VNUMMES2)) AND IIF(!EMPTY(MESPA),VAL(MESPA)=VAL(vnummes2),.t.) and IIF(!EMPTY(MESPR),VAL(MESPR)=VAL(vnummes2),.t.) and IIF(!EMPTY(NUMMESHM),VAL(nummeshm)<=VAL(vnummes2),.T.) AND ;
                     IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart) ;
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
                descri with fte(),;
                DESFTE WITH HOJA.DESREF,DESTINO WITH HOJA.DESTINO
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
                tipfte with 'H/C',;
                fecref with hoja.fechc,;
                codcal with hoja.codcal,;
                numfte with HOJA.NUMHC,;
                mesfte with hoja.nummes,;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with -1,;
                descri with fte(),;
                DESFTE WITH HOJA.DESREF,DESTINO WITH HOJA.DESTINO
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
    

    USE IN 1
	USE IN 2

    SELE AST
    GO TOP
    DEACTIVATE WINDOW STANDBY       
    m.codfte = alltrim(vcodfte)
*   SET UNIQ ON
    AIND = sys(3)+'.idx'
    IF righ(vcodpart,2)='00'
       INDEX ON NUMMES+TIPO+TIPDOC+NUMREF+iif(alltrim(vtipfun)='I',left(CODpart,2),LEFT(CODANAL,2)) TO (AIND)
    ELSE
       INDEX ON NUMMES+TIPO+TIPDOC+NUMREF+iif(alltrim(vtipfun)='I',codpart,codanal) TO (AIND)
    ENDIF   
    
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       DO REPORTE WITH 2,"SALAXS",' Auxiliar de Standar'
    ENDIF
    SET UNIQ OFF
 ENDIF
SET FILTER TO
CLOSE INDEX 
ERASE (AIND)
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


PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodSub)+ALLTRIM(vProyec)
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


PROCEDURE BUSCAL
*---------------
private vkey,ali
ali = alias()
vKeyi = vPeriodo + alltrim(vNummes2)+ alltrim(vCodFte) + alltrim(vCodPrg) + alltrim(vCodSub) + vProyec + vSubpry 
vKeyF = vPeriodo + alltrim(vNummes2)+ alltrim(vCodFte) + alltrim(vCodPrg) + alltrim(vCodSub) + vCodact

select calen
if alltrim(vtipfun) = 'I'
	set orde to calen1
	seek vkeyi
else
	set orde to calen2
	seek vkeyf
endif	

vtotal = 0 
if found()
  if alltrim(vtipfun) = 'I'
     scan while periodo+nummes+codfte+codprg+codsubpr+codproy+codsupry = vkeyI
          vTotal = vtotal + IIF(codpart=Vcodpart,valpart,0)
     endscan      
  else          
     scan while periodo+nummes+codfte+codprg+codsubpr+codact = vkeyF
	     vTotal = vtotal + iif(vcodpart=codpart,valpart,0)          
	 endscan  
  endif	 
endif     
select (ali)
return vtotal

