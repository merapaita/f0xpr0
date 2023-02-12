* ---------------------------------------------------
** REPOE5.PRG
** Reporte Auxiliar de Compromisos por partidas
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------
* Abrir Archivos de trabajo

CLOSE DATA
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE ComPag   IN 4   order tag ComPag1  ALIAS ComPag
USE IteCP    IN 5   ORDER TAG IteCP1   ALIAS IteCP
USE Calen    IN 6   order tag calen1   ALIAS calen
USE Maepre   IN 7   order tag Maepre1  ALIAS maepre
USE itepar   in 8   order tag itepar1  ALIAS Itepar 
USE hojmod   IN 9   order tag hojmod1  ALIAS HOJMOD
USE PteAnu   IN 10  order tag PteAnu9  ALIAS AnuPA
USE ASTAUX_P IN 11   ALIAS AST_P
USE ASTAUX   IN 12   ALIAS AST


*USE astpre   IN 11  order tag Astpre5  ALIAS AstPre
*USE Auxil    IN 12  order tag Auxil2   ALIAS Auxil
*USE Promae   IN 7   order tag Promae1  ALIAS Promae
*SELECT Astpre
*SET RELA TO NUMMES+NUMREF INTO ITEHC
*SET RELA TO NUMMES+NUMREF INTO ITECP ADDI
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
vPeriodo  = '  '
vNumMes1  = '  '
vNumMes2  = '  '
vCodPrg   = '  '
vCodFte   = '   '
vCodSubPr = '   '
vCodAct   = '  '
vCodPry   = '   '
vCodSpy   = '  '


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
 @  2,21  GET vNumMes1   PICTURE '!!'  VALID Val_Para(vNumMes1 ,'FECMES','C',21,30,4) AND !EMPTY(vNumMes1)
 @  2,31  GET vNumMes2   PICTURE '!!'  VALID Val_Para(vNumMes2 ,'FECMES','C',31,20,4) AND !EMPTY(vNumMes2) 
 @  3,21  GET vtipfun    PICTURE '!'   VALID VAL_PARA(vTiPFun  ,'TIPFUN',' ',21,30) 
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30,4) AND !EMPTY(vCodFte) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30,4) AND !EMPTY(vCodPrg) 
 @  6,21  GET vCodSubPr  PICTURE '!!!' VALID IIF(!EMPTY(vCodSubPr),VAL_SUBP(vCodSubPr,'CODSUB'+VCodPrg,' ',21,28),.t.) &&OR EMPTY(vCodPrg)
 @  7,21  GET vCodPry    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I'
 @  8,21  GET vCodSPy    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='I'
 @  9,21  GET vCodAct    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'
 
 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF EMPTY(vNumMES1) OR EMPTY(vNumMES2) .or. LASTKEY()=27
    RETURN
 ELSE
    ACTIVATE WINDOW STANDBY
    @ 1,14 SAY 'Espere un Momento ....' color W*
    
    SELECT AST_P
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 11  ALIAS AST_P

    SELECT AST
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 12  ALIAS AST
  
   
    VIND1=SYS(3)+'.IDX'
    VIND2=SYS(3)+'.IDX'
    VIND3=SYS(3)+'.IDX'
    VIND4=SYS(3)+'.IDX'
    VIND5=SYS(3)+'.IDX'                
    
    INDEX ON NUMMES+TIPO+TIPDOC+NUMREF TO (VIND1)
    SET INDEX TO (VIND1)
    
    SELEC ITEHC

    if alltrim(vtipfun) = 'I'
	   SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. ;
                  Codfte   = ALLTRIM(vCodFte)   .and. ;
                  tipfun   = ALLTRIM(vtipfun)   .and. ;                                    
                  Codprg   = ALLTRIM(vCodPrg)   .and. ;
        		  iif(!EMPTY(vCodSubPr),CodSubPr = ALLTRIM(vCodSubPr),.T.) .and. ;                                    
                  iif(!EMPTY(vCodPry)  ,Codproy=ALLTRIM(vCodPry)  ,.t.) .and. ;
                  iif(!EMPTY(vCodSpy)  ,CodSupry=ALLTRIM(vCodSpy)  ,.t.) .and. ;
                  estado # '9'
	else
	    SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. ;
                  Codfte   = ALLTRIM(vCodFte)   .and. ;
                  Codprg   = ALLTRIM(vCodPrg)   .and. ;
                  tipfun   = ALLTRIM(vtipfun)   .and. ;                                    
 	    		  iif(!EMPTY(vCodSubPr),CodSubPr = ALLTRIM(vCodSubPr),.T.) .and. ;                                    
                  iif(!EMPTY(vCodAct)  ,CodAct=ALLTRIM(vCodAct) ,.t.) .and. ;
                  estado # '9'
	endif                  
    SET RELATION TO NUMMES+NUMHC INTO hoja         
    INDEX ON CODPRG+CODSUBPR+IIF(alltrim(vtipfun)='I',CODPROY+CODSUPRY,CODACT) TO (VIND5)
    SET INDEX TO (VIND5)
    GO TOP   
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with hoja.fechc,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH M.NUMMES,tipdoc with 'H/C',numref with m.Numhc,codprg with m.codprg,codsubpr with m.Codsubpr,codproy with iif(vtipfun='I',m.codproy,'   '),codsupry with iif(vtipfun='I',m.codsupry,'   '),codact with iif(vtipfun='F',m.codact,'  '),;
                valdeb with m.valpart,valhab with m.valpart,TIPO WITH '1',tipfte with hoja.tipdoc,numfte with hoja.numref  &&,DESCRI WITH FTE()
       ELSE

       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               

    *------------------------------
    * DE PARTES DE ANULACION/REBAJA
    *------------------------------
    SELEC ITEHC
    SET ORDE TO ITEHC1
    SET RELA TO NUMMES+NUMHC INTO hoja                      
    SET RELA TO NUMMES+NUMHC INTO ANUPA ADDI                      
    SET FILTER TO between(val(ANUPA.NUMMESTR),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodSubPr) .and. iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND !empty(HOJA.numanu) .and. estado#'9' 
    GO TOP

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with ANUPA.fecPA,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH ANUPA.NUMMESTR,tipdoc with 'P/A',numref with ANUPA.NumPA,codprg with m.codprg,codsubpr with m.Codsubpr,valdeb with m.valpart,valhab with m.valpart,TIPO WITH '4',TIPFTE WITH AnuPA.TipCom,NUMFTE WITH HOJA.NUMREF,mesfte with hoja.nummes,TIPOPE WITH -1,; 
                         codsubpr with m.Codsubpr,codproy with iif(alltrim(vtipfun)='I',m.codproy,'   '),codsupry with iif(alltrim(vtipfun)='I',m.codsupry,'   '),codact with iif(alltrim(vtipfun)='F',m.codact,'  ') &&,DESCRI WITH FTE()
       ENDIF
       UNLOCK
       SELEC ITEHC         
    ENDSCAN               

	SET FILTER TO
	SET RELA TO
	*----------	
	
    *---------------------------------
    * HOJAS DE MODIFICACION
    *---------------------------------
       
    SELEC ITEHC
    SET ORDE TO ITEHC4
    SET RELA TO NUMMESHM+NUMHM INTO hojMOD
    SET FILTER TO nummeshm=alltrim(vnummes2) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CODSUBPR=alltrim(vCodSubPr) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND TIPFUN = alltrim(vtipfun) and hojmod.tiphm = 'C'  &&AND TIPOPE='-' between(val(Nummeshm),val(vNumMes1),val(vnuMmes2))
    GO TOP

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
         repla fecref with hojMOD.fechM , CODANAL WITH m.CODanal, CODPART WITH m.CODPART, NUMMES WITH m.NUMMEShm, tipdoc with 'H/M', numref with m.NumhM, codprg with m.codprg, codsubpr with m.Codsubpr, valdeb with m.VALPART, valhab with m.VALPART,TIPO WITH '3',TIPFTE WITH Hojmod.Tiphm,OPERAC WITH HOJMOD.OPERAC,TIPOPE WITH IIF(m.tipope='-',-1,1),mesfte with M.nummes			
       ENDIF
       UNLOCK
       SELEC ITEHC
    ENDSCAN               
    
    USE IN 1
    USE IN 2
    USE IN 16

*----------------------------    
* DE COMPROBANTE DE PAGO
*----------------------------    
    SELEC ITECP              
    GO TOP
    SET RELA TO NUMMES+NUMCP INTO COMPAG

    SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  SUBSTR(CodCal,5,3)=ALLTRIM(vCodFte) AND SUBSTR(CodCal,10,3)=alltrim(vCodSubPr) .and. COMPAG.tipfun=alltrim(vtipfun) and ;
                  COMPAG.estado#'99' AND COMPAG.DOCREF='H/C' AND !EMPTY(COMPAG.NUMCP)
    SET SKIP TO COMPAG                  
    INDEX ON ITECP.CODCAL TO (VIND3)
    SET INDEX TO (VIND3)

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with compag.feccp,cODANAL WITH M.CODANAL,CODPART WITH M.CODPART, NUMMES WITH M.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.impparc,valhab with m.impparc,TIPO WITH '2',tipfte with compag.docref,numfte with compag.numref ,;
                codprg with SUBSTR(m.codcal,8,2),codsubpr with substr(m.Codcal,10,3),codproy with iif(vtipfun='I',substr(m.codcal,13,3),'   '),codsupry with iif(vtipfun='I',substr(m.codcal,16,2),'   '),codact with iif(vtipfun='F',substr(m.codcal,16,2),'  ') &&,DESCRI WITH FTE()
       ELSE
       ENDIF
       UNLOCK
       SELEC ITECP
    ENDSCAN               

    USE IN 5
    USE IN 6
    
    DO CALCUL
    SELE AST_P
    
    DO SUMATOT
    GO TOP
    DEACTIVATE WINDOW STANDBY       
  
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
	    DO CASE 
    	   CASE alltrim(vtipfun) = 'F'
		       DO REPORTE WITH 2,"MOVPREF",' Movimiento Presupuestal'    	   
	       CASE alltrim(vtipfun) = 'I'
		       DO REPORTE WITH 2,"MOVPREI",' Movimiento Presupuestal'	       
	    ENDCASE           
    ENDIF
 ENDIF
 SET FILTER TO
 SET INDEX TO
 CLOSE INDEX 
 CLOSE DATA
 ERASE (VIND1)
 ERASE (VIND2)
 ERASE (VIND3)
 ERASE (VIND4)  
RETURN

PROCEDURE FTE
*------------
DEVUL='**'
DO CASE
   CASE TIPDOC='H/C' 
        DEVUL = IIF(HOJA.TIPPRV='P',val_prv(HOJA.Codprv),IIF(HOJA.TipPRV='E',val_pro(HOJA.Codemp),HOJA.NOMBRE))
   CASE TIPDOC='C/P'
        DEVUL = COMPAG.CCNOMBRE
   *     DEVUL = IIF(COMPAG.CodPrv='0000' and empty(COMPAG.codemp),Compag.Nombre,IIF(Compag.TipDoc='O/',val_prv(ComPag.Codprv),IIF(Compag.CodEmp='00000' and empty(Compag.Codprv),Compag.Nombre,val_pro(compag.Codemp))))
ENDCASE        
RETURN DEVUL                       

FUNCTION VAL_PRO
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
private medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (parameters()>=2)
mmsg   = (parameters()=4) .and.  _tipo

_campo = varread()

malias = alias()
select AUXIL
_oldwnd = woutput()

If !medita
   SET ORDE TO 2
   seek xcod
   v_fun = iif(found(),Descri,"")
*  QQ
*   if mmsg
*      @ _x,_y say descri
*   endif
else
   if empty(xcod)
      set orde to 3
      on key label ENTER keyboard chr(23)
      define window _xx from 3,22 to 22,77
      browse window _xx title ' ®Enter¯  Selecciona   ' nolgrid noedit noappend nodelete nomenu fields;
         codigo   :h='C¢digo'     ,;
         descri   :h='Nombre'
       * dirpro   :h='Direccci¢n' :25
      on key label ENTER
      release window _xx
      set order to 2
      if !empty(_oldwnd)
         activate window &_oldwnd
      endif
      if lastkey()=27
         v_fun = .f.
      else
         xcod = codigo
         if mmsg
            @ _x,_y say descri
         endif
         select (malias)
         if !_tipo
            replace &_campo with  xcod
         endif
         v_fun = .t.
      endif
   else
      seek xcod
      if mmsg .and. found()
         @ _x,_y say descri
      endif
      v_fun = found()
   endif
endif
m.Codprv='    '
select (malias)
return v_fun

PROCEDURE CALCUL
*---------------
*  ACUMULA LAS 
SELECT AST
SET UNIQ ON
IF ALLTRIM(VTIPFUN)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (VIND2) FOR TIPDOC$'H/C' 
ELSE	
   INDEX ON CODSUBPR+CODACT+CODANAL TO (VIND2) FOR TIPDOC='H/C'
ENDIF
GO TOP
SCAN 
     SELE AST_P
     IF F_appd()
      IF ALLTRIM(VTIPFUN)='F' 
         REPLACE CODANAL WITH AST.CODANAL,CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODACT WITH AST.CODACT
      ELSE   
         REPLACE CODPART WITH AST.CODPART,CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODPROY WITH AST.CODPROY,CODSUPRY WITH AST.CODSUPRY
      ENDIF   
   ENDIF 
   SELE AST
ENDSCAN
SET UNIQ OFF

*2* ACUMULA COMPROMISO BRUTO

SELE AST
INDEX ON IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+CODANAL,CODSUBPR+CODPROY+CODSUPRY+CODPART) TO (VIND3)

SELE AST_P
INDEX ON IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+CODANAL,CODSUBPR+CODPROY+CODSUPRY+CODPART) TO (VIND4)
GO TOP

*** hojas de control

SCAN  
     cod =  IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb*iif(tipOpe=-1,0,1) to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/'
     GO TOP
     sum valdeb to valorX for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/M' AND TIPOPE=-1  AND TIPFTE='C'  AND VAL(NUMMES) # VAL(VNUMMES2) 
     GO TOP
     sum valdeb to valorZ for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='P/A' AND TIPOPE=-1  AND TIPFTE='C'  AND VAL(NUMMES) # VAL(VNUMMES2)
     SELE AST_P                                                                                                                                                                && MESFTE         
     replace Comliqac with valor,Rebant with Rebant+valorx+valorZ

endscan

SELE AST_P
GO TOP
SCAN  
     cod =  IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb*iif(tipope=-1,0,1) to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/' and nummes=ALLTRIM(vnummes2)
     GO TOP
     sum valdeb to valory for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='H/M' and NUMMES=alltrim(vnummes2) and tipope = -1 AND TIPFTE='C'
     GO TOP
     sum valdeb to valorg for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='P/A' and NUMMES=alltrim(vnummes2) and tipope = -1 AND TIPFTE='C'
     sele ast_p                                                                                                                              && MESFTE    
     replace ComPBru with valor,Rebmes with RebMes+valorY+valorG
    
endscan


****** comprobante de pagos

SCAN  
     cod =  IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='C/P'
     sele ast_p
     replace Ejeliqac with valor
endscan

SELE AST_P
GO TOP
SCAN  
     cod =   IIF(ALLTRIM(VTIPFUN)='F',AST_P.CODSUBPR+AST_P.CODACT+ast_p.CODANAL,AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+ast_p.CODPART)
     sele ast
     go top
     sum valdeb to valor for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+cODANAL=cod,CODSUBPR+CODPROY+CODSUPRY+CODPART=cod) AND TIPDOC='C/P' and nummes=ALLTRIM(vnummes2)
     sele ast_p
     replace ejebru with valor
endscan

*------
SELE AST
SET UNIQ ON
IF ALLTRIM(VTIPFUN)='I'
	INDEX ON CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) TO (VIND2) FOR TIPDOC='H/C'
ELSE	
	INDEX ON CODSUBPR+CODACT+LEFT(CODANAL,2) TO (VIND2) FOR TIPDOC='H/C'
ENDIF
GO TOP
SCAN 
   COD_P = IIF(ALLTRIM(VTIPFUN)='F',AST.CODSUBPR+AST.CODACT+LEFT(ast.CODANAL,2),AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+LEFT(ast.CODPART,2))
   COD_X = IIF(ALLTRIM(VTIPFUN)='F',LEFT(ast.CODANAL,2),LEFT(ast.CODPART,2))
   SELE AST_P
   GO TOP
   SUM CompBru,Rebmes,Rebant,Comliqac,Ejeliqac to v1,v11,v12,v2,v3 for IIF(ALLTRIM(VTIPFUN)='F',CODSUBPR+CODACT+LEFT(CODANAL,2),CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2))=cod_p
   IF F_appd()
      IF ALLTRIM(VTIPFUN)='F' 
         REPLACE CODANAL WITH COD_X+'.00',CompBru with v1,Rebmes with v11,Rebant with v12,ComLiqac with v2,Ejeliqac with v3,tipo with 'X',CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODACT WITH AST.CODACT
      ELSE   
         REPLACE CODPART WITH COD_X+'.00',CompBru with v1,Rebmes with v11,Rebant with v12,ComLiqac with v2,Ejeliqac with v3 tipo with 'X',CODPRG WITH AST.CODPRG,CODSUBPR WITH AST.CODSUBPR,CODPROY WITH AST.CODPROY,CODSUPRY WITH AST.CODSUPRY
      ENDIF   
   ENDIF 
   SELE AST
ENDSCAN
SET UNIQ OFF
RETURN


RETU


PROCEDURE SUMATOT
*----------------
GO TOP
SUM COMPBRU,REBMES,REBANT,COMLIQ,COMLIQAC,EJEBRU,EJEREB,EJELIQAC TO A1,A2,A3,A4,A5,A6,A7,A8 FOR IIF(ALLTRIM(VTIPFUN)='I',RIGHT(CODPART,2)#'00',RIGHT(CODANAL,2)#'00')
GO TOP
RETURN
