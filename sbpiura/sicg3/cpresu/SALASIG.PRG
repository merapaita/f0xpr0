** ---------------------------------------------------
** SALASIG.PRG
** Reporte Saldo de Asignacion de partidas
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
USE Clase    IN 10  order tag Clase1   ALIAS Clase
USE Maepre   IN 11 order tag Maepre1   ALIAS Maepre
USE astpre   IN 13  order tag Astpre5  ALIAS AstPre
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
USE ComPag   IN 15   order tag ComPag1  ALIAS ComPag
*USE hojmod   IN 15  order tag hojmod1  ALIAS HOJMOD
*USE PteAnu   IN 16  order tag PteAnu7  ALIAS AnuPA
USE ASTAUX   IN 17                     ALIAS AST

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
 @  2,2 say '        Mes del : '
 @  2,25 say 'al : '
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
 @  2,31  GET vNumMes2   PICTURE '!!'  VALID Val_Para(vNumMes2 ,'FECMES','C',31,20,4) AND !EMPTY(vNumMes2) 
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

 IF EMPTY(vNumMES1) OR EMPTY(vNumMES2) .or. LASTKEY()=27
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
   *INDEX ON IIF(ALLTRIM(vtipfun)='F',codANAL,codPART)+NUMMES+TIPO+TIPDOC+NUMREF TO (VIND)
    IF ALLTRIM(vtipfun)='F'
       INDEX ON codANAL+NUMMES+CODSUBPR+CODACT+TIPO+TIPDOC+NUMREF TO (VIND)
    ELSE
       INDEX ON NUMMES+CODSUBPR+CODPROY+CODSUPRY+CODPART+TIPO+TIPDOC+NUMREF TO (VIND) 
    ENDIF
    SET INDEX TO (VIND)
    *-------------------
    * DE HOJA DE CONTROL
    *-------------------    
    SELEC ITEHC
	SET RELA TO NUMMES+NUMHC INTO hoja    
	set RELA TO NUMMES+NUMHC INTO compag addi
    IF vcodpart='     '
     	SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodsub) .and.;
                  EMPTY(HOJA.NUMCP) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  and iif(!EMPTY(vCodAct),CodAct=ALLTRIM(vCodact),.t.) AND ESTADO # '99' AND TIPFUN=ALLTRIM(VTIPFUN) and val(compag.nummes) = val(vnummes2) 
    ELSE
    	SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )) AND CodSubPr=alltrim(vCodsub) .and.;
                  EMPTY(HOJA.NUMCP) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) and iif(!EMPTY(vCodAct),CodAct=ALLTRIM(vCodact),.t.) AND ESTADO # '99' AND TIPFUN=ALLTRIM(VTIPFUN) and val(compag.nummes) = val(vnummes2) 
    ENDIF                  
    GO TOP
QQ
    DO WHILE !EOF()

       vNumMes=Nummes
       vHojCon=NumHc
       vtotpart=0
       do while (vNumMes=Nummes and vHojCon=NumHc) 
       		vtotpart=vtotpart+valpart
       		skip
       enddo	
       skip -1
       SELECT AST
       IF F_APPD()
          repla fecref with hoja.fechc 
          IF ALLTRIM(vtipfun)='F'
             replace codact with itehc.codact
          else
             replace codproy with itehc.codproy,codsupry with itehc.codsupry
          endif
          IF VCODPART='     '
	          IF ALLTRIM(vtipfun)='F'
    	         repla CODANAL WITH ITEHC.CODANAL
	          else
    	         repla CODPART WITH ITEHC.codpart
	          endif   
	      ELSE
	          IF ALLTRIM(vtipfun)='F'
    	         repla CODANAL WITH vcodpart
	          else
    	         repla CODPART WITH vcodpart
	          endif   
	      ENDIF    
          repla NUMMES WITH vNUMMES,tipdoc with 'H/C',numref with vHojCon,codprg with itehc.codprg,codsubpr with itehc.Codsubpr,;
                valdeb with vtotpart,valhab with vtotpart,TIPO WITH '1',DESCRI WITH FTE(),tipfte with hoja.tipdoc,;
                numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH 1
       ENDIF
       UNLOCK
       SELEC ITEHC         
       SKIP
       IF EOF()
          EXIT
       ENDIF   
    ENDDO
    
	SET RELA TO
	*----------	
    SELE AST
    GO TOP
    DEACTIVATE WINDOW STANDBY       
    m.codfte = alltrim(vcodfte)
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       IF ALLTRIM(VTIPFUN)='I'
          DO REPORTE WITH 2,"SALASIG",' Auxiliar de Standar'
       ELSE
          DO REPORTE WITH 2,"SALASIF",' Auxiliar de Standar' 
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
     
     
*------------------------
procedure novale
*---------------

    
    *-----------------------
    * DE HOJAS DE MODIFICACION
    *-----------------------
    SELEC ITEHC
    SET ORDE TO 4
    IF !EMPTY(vcodpart)
    SET FILTER TO between(val(NummesHm),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )) AND CODSUBPR=alltrim(vCodsub) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  &&AND TIPOPE='-'
    ELSE
    SET FILTER TO between(val(NummesHm),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CODSUBPR=alltrim(vCodsub) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  &&AND TIPOPE='-'
    ENDIF                  
    GO TOP
    SET RELA TO NUMMESHM+NUMHM INTO hojMOD
    vtotpart = 0
    DO WHILE !EOF()
       vNumMes=Nummes
       vHojCon=Numhc
       vNumMeshm=Nummeshm
       vHojMod=Numhm
       vtotpart=vtotpart+valpart
       do while (vNumMes=Nummes and vHojCon=NumHc)
       		IF TIPOPE='-'
       	    	vtotpart=vtotpart+valpart
       	    ENDIF	
      		skip
       enddo	
       skip -1
       SELECT AST
       vDescr = 'Rebaja de la H/C : '+hojmod.nummeshc+'.'+hojmod.numhc+' '+hojmod.tiphm
       IF F_APPD()
         repla fecref with hojMOD.fechM 
         If vtipfun='F'
             repla CODANAL WITH vcodpart
         else
             repla CODPART WITH vcodpart
         endif
         repla NUMMES WITH vNUMMESHM, tipdoc with 'H/M', numref with vHojMod
         repla codprg with vcodprg, codsubpr with vCodsub, valdeb with vtotpart
         repla valhab with vtotpart,TIPO WITH '3',TIPFTE WITH Hojmod.Tiphm
         IF hojmod.OPERAC='T'
            repla OPERAC WITH HOJMOD.OPERAC &&(HOJMOD.OPERAC/2)
         else
            repla OPERAC WITH HOJMOD.OPERAC
         endif   
         DO CASE
            CASE HOJMOD.OPERAC='T'  
                 vDescr = 'Transf.Int.de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
            CASE HOJMOD.OPERAC='R'  
                 vDescr = 'Rebaja de la H/C : '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
            CASE HOJMOD.OPERAC='C'  
                 vDescr = 'Cambio Fte. de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
         ENDCASE
         REPLACE  DESCRI WITH vDescr
       ENDIF
       UNLOCK
       SELEC itehc
       skip
       if eof()
          exit
       endif   
    ENDdo
    
    *------------------------------
    * DE PARTES DE ANULACION/REBAJA
    *------------------------------
    SELEC ITEHC
    SET ORDE TO ITEHC1
    SET RELA TO NUMMES+NUMHC INTO hoja                      
    SET RELA TO NUMMES+NUMHC INTO ANUPA ADDI                      
    IF !EMPTY(vcodpart)
    SET FILTER TO between(val(ANUPA.NUMMESTR),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )) AND CodSubPr=alltrim(vCodsub) .and.;
                  estado#'9' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND !empty(HOJA.numanu)
    ELSE
	SET FILTER TO between(val(ANUPA.NUMMESTR),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodsub) .and.;
                  estado#'9' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND !empty(HOJA.numanu)
    ENDIF              
                  
    GO TOP
    DO WHILE !EOF()
       vNumMes=Nummes
       vHojCon=Numhc
       vNumMeshm=Nummeshm
       vHojMod=Numhm
       do while (vNumMes=Nummes and vHojCon=NumHc)
       		skip
       enddo	
       skip -1
       SELECT AST
       IF F_APPD()
          repla fecref with ANUPA.fecPA
          If vtipfun='F'
             repla CODANAL WITH vcodpart
          else
              repla CODPART WITH vcodpart
          endif
	      repla NUMMES WITH ANUPA.NUMMESTR,tipdoc with 'P/A',numref with ANUPA.NumPA
	      repla codprg with vcodprg,codsubpr with vCodsub,valdeb with anupa.valtot,valhab with anupa.valtot
	      repla TIPO WITH '4',DESCRI WITH FTE(),TIPFTE WITH HOJA.TipDOC,NUMFTE WITH HOJA.NUMREF,mesfte with hoja.nummes,TIPOPE WITH -1
       ENDIF
       UNLOCK
       SELEC ITEHC        
       skip
       if eof()
          exit
       endif    
    ENDdo          
     
     