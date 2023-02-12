** ---------------------------------------------------
** SASINA.PRG
** Reporte Saldos de Asiganaci¢n
** Region Grau
** Autor : Ing. Fernando Mu¤oz Cho 
** Versi¢n : 4.05
** Fecha   : 08 de Julio de 1996 
** ------------------------------
* Abrir Archivos de trabajo
USE ItePar   IN 5 ORDER TAG ItePar1         ALIAS ItePar
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE Maepre   IN 11 order tag Maepre1   ALIAS Maepre
USE HojCon   IN 1   ORDER TAG HojCon1  ALIAS Hoja
USE Itehc    IN 2   ORDER TAG Itehc1   ALIAS Itehc
USE Parmae   IN 3   ORDER TAG Parmae1  ALIAS Parma
USE Auxil    IN 14  ORDER TAG Auxil2   ALIAS Auxil
USE ComPag   IN 15  ORDER TAG ComPag4  ALIAS ComPag
USE Hojmod   IN 16  ORDER TAG Hojmod2  ALIAS Hojmod
USE Reten    IN 18  ORDER TAG Reten1   ALIAS Reten
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

 Vnummes1= MONTH(DATE())
 IF Vnummes1<10
    vnummes1  ='0'+ALLTRIM(STR(MONTH(DATE())))
 ELSE
    vnummes1  =STR(Vnummes1,2)   
 ENDIF   
 vperiodo = RIGHT(STR(YEAR(DATE())),2)

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
    * Caso no existe Comprobante de Pago
    * - No Existe H/M                  => Saldo
    * - Exiete H/M de operacion R o C  => Saldo
    *-----------------------------------
    
    vindi1=SYS(3)+'.idx'

	SELE HOJA
	SET FILTER TO ESTADO#'99'
	GO TOP
	
	
	SELE HOJMOD
	SET FILTER TO ESTADO#'99'
	GO TOP
	
	
	SELE COMPAG
	SET FILTER TO ESTADO#'99'
    GO TOP

    
    SELEC ITEHC
	SET RELA TO NUMMES+NUMHC INTO hoja    
	SET RELA TO NUMMES+NUMHC INTO Compag addi	
    SET RELA TO NUMMES+NUMHC INTO HOJMOD addi	
	
	SELE ITEHC
    INDEX ON NUMMES+NUMHC TO (vindi1) FOR  EMPTY(Hoja.Numcp) AND val(HOJA.Nummes)<=val(vnuMmes1) AND Itehc.estado#'9' AND  Itehc.tipfun = ALLTRIM(vTipfun)  AND  EMPTY(HOJA.NUMANU)
    
    *INDEX ON NUMMES+NUMHC TO (vindi1) FOR  EMPTY(Hoja.Numcp) AND val(HOJA.Nummes)<=val(vnuMmes1) AND Itehc.estado#'9' AND  Itehc.tipfun = ALLTRIM(vTipfun)  
    
    GO TOP

	SET FILTER TO  IIF(!EMPTY(HOJA.NUMHM) AND HOJMOD.OPERAC$'RC',.F.,IIF(EMPTY(HOJA.NUMHM),.T.,.F.)) AND;
		           CODPRG=ALLTRIM(vCodPrg) AND;
                   CODFTE=ALLTRIM(vCodFte) AND;
                   CodSubPr=alltrim(vCodsub) AND;
  	               IIF(!EMPTY(vCodPry),Codproy =ALLTRIM(vCodPry),.t.)  AND;
                   IIF(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  AND;
                   IIF(!EMPTY(vCodAct),CodAct  =ALLTRIM(vCodact),.t.)  AND;
                   IIF(vcodpart='     ',.T.,iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )))
   GO TOP  
   
   SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref   with hoja.fechc,;
                CODANAL  WITH M.CODANAL,;
                CODPART  WITH M.CODPART,;
                NUMMES   WITH M.NUMMES, ;
                tipdoc   with 'H/C',;
                numref   with m.Numhc, ;
                codprg   with m.codprg, ;
                codsubpr with m.Codsubpr, ;
                Codproy  with IIF(alltrim(vtipfun)='I',m.codproy,'   '),;
                codsupry with IIF(alltrim(vtipfun)='I',m.codsupry,'   '),;
                codact   with IIF(alltrim(vtipfun)='I','   ',m.codact), ;
                valPart  with m.valpart,;
                TIPO     WITH '1',;
                DESCRI   WITH FTE(),;
                tipfte   with hoja.tipdoc,;
                numfte   with hoja.numref,;
                mesfte   with hoja.nummes,;
                TIPOPE   WITH 1,;
                DESTINO  WITH HOJA.DESTINO, ;
                TOTHC    WITH HOJA.IMPTOT,;
                DESFTE   WITH HOJA.DESREF
       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               
    
    
    *------------------------------------------------------
    * Caso Existe Comprobante de Pago
    * *- Mescp > al Mesa Saldo => Saldo
    * *- Con Hoja de Modificacion
    *-----------------------------------
    
    SELEC ITEHC
    SET FILTER TO
    SET INDEX TO
    ERASE(vindi1)

	SELE HOJA
	SET FILTER TO ESTADO#'99'
	GO TOP
	
	
	SELE HOJMOD
	SET FILTER TO ESTADO#'99'
	GO TOP
	
	
	SELE COMPAG
	SET FILTER TO ESTADO#'99'
    GO TOP

    
    SELEC ITEHC
	SET RELA TO NUMMES+NUMHC INTO hoja    
	SET RELA TO NUMMES+NUMHC INTO Compag addi	
    SET RELA TO NUMMES+NUMHC INTO HOJMOD addi	    

    SELE ITEHC
    vindi1=SYS(3)+'.idx' 
    INDEX ON NUMMES+NUMHC TO (vindi1) FOR  !EMPTY(Hoja.Numcp) AND val(HOJA.Nummes)<=val(vnuMmes1) AND !EMPTY(HOJA.NUMHM) AND HOJMOD.OPERAC#'RC'
    GO TOP
    
    * VAL(HOJMOD.NUMMESCP>VAL(vnuMmes1) OR 
    
	SET FILTER TO  IIF(!EMPTY(HOJA.NUMCP) AND VAL(Hoja.Nummescp)>VAL(vnuMmes1),.T.,.F.) .AND.;
		           IIF(HOJMOD.OPERAC$'T' AND EMPTY(HOJMOD.NUMCP),.T.,.F.) AND;
		           CODPRG=ALLTRIM(vCodPrg) AND;
                   CODFTE=ALLTRIM(vCodFte) AND;
                   CodSubPr=alltrim(vCodsub) .and.;
  	               IIF(!EMPTY(vCodPry),Codproy =ALLTRIM(vCodPry),.t.)  and ;
                   IIF(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  and ;
                   IIF(!EMPTY(vCodAct),CodAct  =ALLTRIM(vCodact),.t.)  and ;
                   Itehc.estado#'9' AND; 
                   tipfun = ALLTRIM(vTipfun) AND ;
                   IIF(vcodpart='     ',.T.,iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )))
   GO TOP  
   
   SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref   with hoja.fechc,;
                CODANAL  WITH M.CODANAL,;
                CODPART  WITH M.CODPART,;
                NUMMES   WITH M.NUMMES, ;
                tipdoc   with 'H/C',;
                numref   with m.Numhc, ;
                codprg   with m.codprg, ;
                codsubpr with m.Codsubpr, ;
                Codproy  with IIF(alltrim(vtipfun)='I',m.codproy,'   '),;
                codsupry with IIF(alltrim(vtipfun)='I',m.codsupry,'   '),;
                codact   with IIF(alltrim(vtipfun)='I','   ',m.codact), ;
                valPart  with IIF(HOJMOD.OPERAC$'T' AND EMPTY(HOJMOD.NUMCP),m.valpart*-1,m.valpart),;
                TIPO     WITH '1',;
                DESCRI   WITH FTE(),;
                tipfte   with hoja.tipdoc,;
                numfte   with hoja.numref,;
                mesfte   with hoja.nummes,;
                TIPOPE   WITH 1,;
                DESTINO  WITH HOJA.DESTINO, ;
                TOTHC    WITH HOJA.IMPTOT,;
                DESFTE   WITH HOJA.DESREF
       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               
    
   *------------------------------------------------------
    
    SET RELA TO
    SET INDEX TO
    ERASE(vindi1)
    *-------------------

    *------------------------------------------------------
    * Caso Existe Comprobante de Pago
    * *- Mescp > al Mesa Saldo => Saldo
    * *- SIN Hoja de Modificacion
    *-----------------------------------
    
    SELEC ITEHC
    SET FILTER TO
    SET INDEX TO
    ERASE(vindi1)

	SELE HOJA
	SET FILTER TO ESTADO#'99'
	GO TOP
	
	
	SELE HOJMOD
	SET FILTER TO ESTADO#'99'
	GO TOP
	
	
	SELE COMPAG
	SET FILTER TO ESTADO#'99'
    GO TOP

    
    SELEC ITEHC
	SET RELA TO NUMMES+NUMHC INTO hoja    
	SET RELA TO NUMMES+NUMHC INTO Compag addi	
    SET RELA TO NUMMES+NUMHC INTO HOJMOD addi	
	
    SELE ITEHC
    vindi1=SYS(3)+'.idx' 
    INDEX ON NUMMES+NUMHC TO (vindi1) FOR  !EMPTY(Hoja.Numcp) AND VAL(HOJA.Nummes)<=VAL(vnuMmes1) AND EMPTY(HOJA.NUMHM) AND VAL(Hoja.Nummescp)>VAL(vnuMmes1) 
    GO TOP
    
	SET FILTER TO  CODPRG=ALLTRIM(vCodPrg) AND;
                   CODFTE=ALLTRIM(vCodFte) AND;
                   CodSubPr=alltrim(vCodsub) .and.;
				   Itehc.estado#'9' AND; 
				   tipfun = ALLTRIM(vTipfun) AND;
  	               IIF(!EMPTY(vCodPry),Codproy =ALLTRIM(vCodPry),.t.)  and ;
                   IIF(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  and ;
                   IIF(!EMPTY(vCodAct),CodAct  =ALLTRIM(vCodact),.t.)  and ;
                   IIF(vcodpart='     ',.T.,iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )))
   GO TOP  
   
   SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref   with hoja.fechc,;
                CODANAL  WITH M.CODANAL,;
                CODPART  WITH M.CODPART,;
                NUMMES   WITH M.NUMMES, ;
                tipdoc   with 'H/C',;
                numref   with m.Numhc, ;
                codprg   with m.codprg, ;
                codsubpr with m.Codsubpr, ;
                Codproy  with IIF(alltrim(vtipfun)='I',m.codproy,'   '),;
                codsupry with IIF(alltrim(vtipfun)='I',m.codsupry,'   '),;
                codact   with IIF(alltrim(vtipfun)='I','   ',m.codact), ;
                TIPO     WITH '1',;
                DESCRI   WITH FTE(),;
                tipfte   with hoja.tipdoc,;
                numfte   with hoja.numref,;
                mesfte   with hoja.nummes,;
                TIPOPE   WITH 1,;
                DESTINO  WITH HOJA.DESTINO, ;
                TOTHC    WITH HOJA.IMPTOT,;
                DESFTE   WITH HOJA.DESREF
                
                IF Compag.Partret = m.Codanal AND Compag.Reten > 0
                   REPLACE valPart  with m.valpart - Compag.Reten
                ELSE 
                   REPLACE valPart  with m.valpart 
                ENDIF   
       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               
   *------------------------------------------------------
       
    SET INDEX TO
    ERASE(vindi1)
    
    *------------------------------
    * Del Pago de las Retenciones
    *------------------------------
    SELE RETEN
    vindi2=SYS(3)+'.idx' 
    INDEX ON NUMMESHC+NUMHC TO (vindi2) FOR (EMPTY(Reten.Conpago) OR VAL(Reten.Mescppg)>VAL(vnuMmes1)) AND !EMPTY(Reten.NUMHC) 
    GO TOP
    
    
    SELE ITEHC
    SET RELA TO NUMMES+NUMHC INTO RETEN addi	
    
    
    SELE ITEHC
    vindi1=SYS(3)+'.idx' 
    INDEX ON NUMMES+NUMHC TO (vindi1) FOR  !EMPTY(Hoja.Numcp) AND VAL(HOJA.Nummes)<=VAL(vnuMmes1)  AND !EMPTY(Reten.NUMHC)
    GO TOP
    
    
	SET FILTER TO  CODPRG=ALLTRIM(vCodPrg) AND;
                   CODFTE=ALLTRIM(vCodFte) AND;
                   CodSubPr=alltrim(vCodsub) .and.;
				   Itehc.estado#'9' AND; 
				   tipfun = ALLTRIM(vTipfun) AND;
  	               IIF(!EMPTY(vCodPry),Codproy =ALLTRIM(vCodPry),.t.)  and ;
                   IIF(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.)  and ;
                   IIF(!EMPTY(vCodAct),CodAct  =ALLTRIM(vCodact),.t.)  and ;
                   IIF(vcodpart='     ',.T.,iif(alltrim(vTipFun)='F', IIF(righ(vcodpart,2)='00',left(CODANAL,2)=left(vcodpart,2),codanal=vcodpart),IIF(righ(vcodpart,2)='00',left(CODpart,2)=left(vcodpart,2),CODPART=vcodpart )))
                   
   GO TOP  
   
   SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref   with hoja.fechc,;
                CODANAL  WITH M.CODANAL,;
                CODPART  WITH M.CODPART,;
                NUMMES   WITH M.NUMMES, ;
                tipdoc   with 'H/C',;
                numref   with m.Numhc, ;
                codprg   with m.codprg, ;
                codsubpr with m.Codsubpr, ;
                Codproy  with IIF(alltrim(vtipfun)='I',m.codproy,'   '),;
                codsupry with IIF(alltrim(vtipfun)='I',m.codsupry,'   '),;
                codact   with IIF(alltrim(vtipfun)='I','   ',m.codact), ;
                valPart  with Reten.Valret,;
                TIPO     WITH '1',;
                DESCRI   WITH 'RET - '+FTE(),;
                tipfte   with hoja.tipdoc,;
                numfte   with hoja.numref,;
                mesfte   with hoja.nummes,;
                TIPOPE   WITH 1,;
                DESTINO  WITH HOJA.DESTINO, ;
                TOTHC    WITH HOJA.IMPTOT,;
                DESFTE   WITH HOJA.DESREF
       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               
    
    SET RELA TO
    SET INDEX TO
    ERASE(vindi1)
    *-------------------------
    
    *---------------------------      	
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
  	       SUM VALPART*TIPOPE TO SW FOR LEFT(CODANAL,2) = VF
	       IF F_APPD()
	          REPLA CODANAL WITH VF+'.00',VALPART WITH SW,TIPOPE WITH 1
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
