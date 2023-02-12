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
    *-------------------
    * DE HOJA DE CONTROL
    *-------------------    
    SELEC ITEHC
	SET RELA TO NUMMES+NUMHC INTO hoja    
	set RELA TO NUMMES+NUMHC INTO compag addi	
    IF vcodpart='     '
		SET FILTER TO val(Nummes)=val(vnuMmes1) AND IIF(!empty(Compag.Nummes),Compag.Nummes#ITEHC.NumMes,.t.) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodsub) .and.;
  	              Iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  and iif(!EMPTY(vCodAct),CodAct=ALLTRIM(vCodact),.t.) AND ESTADO # '9' AND TIPFUN=ALLTRIM(VTIPFUN)
    ELSE
    	SET FILTER TO VAL(Nummes)=val(vNumMes1) AND IIF(!empty(Compag.Nummes),Compag.Nummes#ITEHC.NumMes,.t.) AND CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )) AND CodSubPr=alltrim(vCodsub) .and.;
                  iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) and iif(!EMPTY(vCodAct),CodAct=ALLTRIM(vCodact),.t.) AND ESTADO # '9' AND TIPFUN=ALLTRIM(VTIPFUN) 
    ENDIF                  
    GO TOP  

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'       
	          repla fecref with hoja.fechc,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH M.NUMMES,tipdoc with 'H/C',numref with m.Numhc,codprg with m.codprg,codsubpr with m.Codsubpr,Codproy with m.codproy,codsupry with m.codsupry,valdeb with valpart,valhab with m.valpart,TIPO WITH '1',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH 1,DESTINO WITH HOJA.DESTINO,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF
	      else    
    	      repla fecref with hoja.fechc,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH M.NUMMES,tipdoc with 'H/C',numref with m.Numhc,codprg with m.codprg,codsubpr with m.Codsubpr,codact with m.codact,valdeb with valpart,valhab with m.valpart,TIPO WITH '1',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH 1,DESTINO WITH HOJA.DESTINO,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF
    	  endif    
       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               
    SET RELA TO
	*---------------------------------
    * HOJAS DE MODIFICACION
    *---------------------------------
      
    SELEC ITEHC
    SET ORDE TO ITEHC4
    SET RELA TO NUMMESHM+NUMHM INTO hojMOD
    SET FILTER TO nummeshm=alltrim(vnummes1) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CODSUBPR=alltrim(vCodSub) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND TIPFUN = alltrim(vtipfun) and hojmod.tiphm = 'C'  &&AND TIPOPE='-' between(val(Nummeshm),val(vNumMes1),val(vnuMmes2))
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with hojMOD.fechM ,CODANAL WITH m.CODanal, CODPART WITH m.CODPART, NUMMES WITH m.NUMMEShm, tipdoc with 'H/M', numref with m.NumhM, codprg with m.codprg, codsubpr with m.Codsubpr, valdeb with m.VALPART, valhab with m.VALPART,TIPO WITH '3',TIPFTE WITH Hojmod.Tiphm,OPERAC WITH HOJMOD.OPERAC,TIPOPE WITH IIF(m.tipope='-',-1,1),mesfte with M.nummes, descri with fteHM()
          IF ALLTRIM(m.tipfun)='I'
            replace codproy with m.codproy,codsupry with codsupry
          else
            replace codact with m.codact   
          endif   
       ENDIF
       UNLOCK
       SELEC ITEHC
    ENDSCAN               
	
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
