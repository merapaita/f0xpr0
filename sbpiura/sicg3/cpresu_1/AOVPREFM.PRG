** ---------------------------------------------------
** ACP2.PRG
** Reporte Auxiliar de Compromisos por partidas
** UDEP
**        Autor : Ing. Luis Castillo Dur n 
** Modificaci¢n : Prg. Javier Fr¡as Gomez
** 
** ---------------------------------------------------

sistema=vsistema
public vperiodo
* Abrir Archivos de trabajo
CLOSE DATA
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Maepre   IN 7   order tag Maepre1  ALIAS maepre
USE itepar   in 8   order tag itepar1  ALIAS Itepar 
USE hojmod   IN 9   order tag hojmod1  ALIAS HOJMOD
USE PteAnu   IN 10  order tag PteAnu9  ALIAS AnuPA
USE Astaux_P IN 11                     ALIAS AST_P
USE Astaux   IN 12                     ALIAS AST
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
USE Promae   IN 15  order tag Promae1  ALIAS Promae

public v1,v2,v3,A1,A2,A3,A4,A5,A6,A7,A8,v11,v12,vespgas,vcodcom
DO Inicia
CLOSE DATA
RELEASE WINDOW lis_1
RETURN

PROCEDURE Inicia
*--------------
IF EOF()
   DO standby WITH 'No Existe registros a Procesar'
   RETURN
ENDIF

vtemp     = RECNO()
as        = order()
vperiodo  = RIGHT(STR(YEAR(DATE()),4),2)
VTOTAL    = 2
vtipo     = 2
vUniges   = '01'
vUnieje   = '001'
vNumMes1  = '01'
vNumMes2  = '  '

store space(2) to vcodfte,vcalend,VCODFUN,VESPGAS
store space(3) to vcodprg
store space(4) to vcodSpr,vcodcad
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry,vcodpart

store space(2) to VCODFUN
store space(3) to vcodprg
store space(4) to vcodSpr
store space(6) to vactpry
store space(6) to vcodcom

DEFINE WINDOW lis_1 FROM 4,7 TO 19,73 DOUBLE ;
	TITLE ' ±  Movimiento Presupuestal ± ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1

@  1,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	    VALID !EMPTY(vperiodo)
@  2,2 SAY '  Del mes de : ' GET vnummes1   PICTURE '!!'  		VALID val_para(vnummes1,'FECMES',' ',18,30)
@  3,2 SAY '   Al mes de : ' GET vcalend    PICTURE '!!'  		VALID val_para(vcalend,'FECMES',' ',18,30)
@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.)
@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)

@  6,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		
@  7,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) 
@  8,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)
@  9,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			
@ 10,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			

@ 11,2 SAY 'P.Espec¡fica : ' GET vespgas    PICTURE '!!'        VALID IIF(!EMPTY(vespgas),val_para(vespgas,'ESPGAS',' ',18,30),.T.)
@ 12,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID val_para(vcodfte,'CODFTE',' ',18,30)

READ VALID val_read()
vnummes2 = ALLT(vcalend)
*vcodcom  = maepre.codcom
DEACTIVATE WINDOW lis_1

IF LASTKEY()=27
    RETURN
ELSE
    DO ESPERA1 WITH 1,'Espere un momento por favor'
    m.periodo = alltrim(vperiodo)
    m.codfte  = alltrim(vcodfte)

    SELECT AST
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 12  ALIAS AST EXCLU
    
    
    SELECT AST_P
    vdbf = sys(3)+'.dbf'
    xidx = sys(3)+'.idx'
    copy stru to (vdbf)
    USE (VDBF) IN 11  ALIAS AST_P EXCLU
    
    SELECT AST_P
    INDEX ON periodo+uniges+unieje+codcad  TO (xidx)

    VIND1=SYS(3)+'.IDX'
    VIND2=SYS(3)+'.IDX'
    VIND3=SYS(3)+'.IDX'
    VIND4=SYS(3)+'.IDX'
    VIND5=SYS(3)+'.IDX'                
    
    xvalor = vperiodo+ALLTRIM(vUniges)+ALLTRIM(vUnieje)+ALLTRIM(vCodcad)
	proact = alltrim(vcodCom)  
    
    ******** AFECTACIONES ********
    * INCLUYE=>H/C , P/A, P/R, H/M
    ******************************    
  
  SELEC ITEHC
  SET RELATION TO VPERIODO+ALLTRIM(ITEHC.UNIGES)+ALLTRIM(ITEHC.UNIEJE)+ALLTRIM(ITEHC.CODCAD) INTO MAEPRE
  
  SET FILT TO     itehc.codfte=alltrim(vcodfte) AND ;
    		      itehc.uniges=alltrim(vuniges) AND ;
 	  	          itehc.unieje=alltrim(vunieje) AND ;
 		          iif(empty(vespgas),.t.,subs(itehc.codpart,5,2)=alltrim(vespgas)) AND ;
 		          IIF(EMPTY(vcodfun),.t.,maepre.codfun=alltrim(vcodfun))  and ;
 		          IIF(EMPTY(vcodprg),.t.,maepre.codprg=alltrim(vcodprg))  and ;
 		          IIF(EMPTY(vcodspr),.t.,maepre.codspr=alltrim(vcodspr))  and ;
 		          IIF(EMPTY(vactpry),.t.,maepre.actpry=alltrim(vactpry))  and ;
 		          iif(empty(vcodcom),.t.,itehc.codcom=alltrim(vcodcom)) AND ;
 		          itehc.estado#'99' AND BETWEEN(VAL(itehc.NUMMES),VAL(VNUMMES1),VAL(VNUMMES2))

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
                codcad with m.codcad,;
                codcom with m.codcom,;
                codfte with m.codfte,;
                valpart with m.valpart,;
                mesocos with m.mesocos,;
                mesPr with m.mespr,;
                mesPa with m.mespa,;
                tipo with iif(m.tipdoc='P/A','4',iif(m.tipdoc='P/R','5',iif(m.tipdoc='H/M','3','1'))),;
                fecref with hoja.fechc,;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with iif(m.tipdoc='P/A',-1,iif(m.tipdoc='P/R',-1,iif(m.tipdoc='H/M',IIF(m.tipope='-',-1,1),1))),;
                descri with fte(),;
                Codpart WITH M.CodPart
       ENDIF
       UNLOCK
       IF !EMPTY(m.numpa) and mespa <= alltrim(vnummes2)
          IF F_APPD()
          repla Nummes WITH M.MESPA,;
                tipdoc with 'P/A',;
                numref with m.NumPA,;
                codcad with m.codcad,;
                codcom with m.codcom,;
                codfte with m.codfte,;
                valpart with m.valpart,;
                mesocos with m.mesocos,;
                mesPa with m.mespa,;
                TIPO WITH '4',;
                tipfte with 'H/C',;
                fecref with hoja.fechc,;
                numfte with HOJA.NUMHC,;
                mesfte with hoja.nummes,;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with -1,;
                descri with fte(),;
                Codpart WITH M.CodPart
	       ENDIF
    	   UNLOCK
       endif
       SELEC HOJA1
    ENDSCAN 
    SET RELATION OFF INTO HOJA
   
    **************************
    * DE HOJAS DE MODIFICACION
    **************************
    USE IN 10
    USE IN 13
    SELECT ITEHC
    SET RELA TO NUMMES+NUMHC INTO HOJMOD
    SET FILT TO codfte+periodo+uniges+unieje+codcad = alltrim(m.codfte) + xvalor and HOJMOD.ESTADO='50' AND BETWEEN(VAL(NUMMESHM),VAL(VNUMMES1),VAL(VNUMMES2)) AND ITEHC.OPERAC='T' AND ITEHC.TIPOPE='+'

	SInd = SYS(3) + '.DBF'
	PInd = SYS(3) + '.IDX'
	COPY TO (SInd)
	use (Sind) in 20 alias hoJM1 exclu
	select hojM1

	INDEX ON NUMMES+NUMHC TO (PIND) 
	
    GO TOP   
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla Nummes WITH IIF(m.estado='92',m.nummeshm,M.NUMMES),;
                tipdoc with 'C/P',;
                numref with iif(m.tipdoc='P/A',m.NumPA,iif(m.tipdoc='P/R',m.NumPr,iif(m.tipdoc='H/M',m.NumHm,m.numhc))),;
                codcad with m.codcad,;
                codcom with m.codcom,;
                valpart with m.valpart,;
                mesocos with m.mesocos,;
                mesPr with m.mespr,;
                mesPa with m.mespa,;
                tipo with '2',;
                tipfte with hoja.tipDOC,;
                fecref with hoja.fechc,;
                numfte with HOJA.NUMref,;
                mesfte with iif(m.tipdoc='P/',m.mesocos,IIF(m.tipdoc='H/M',m.Nummeshm,m.nummes)),;
                operac with m.operac,;
                tipcom with m.tipcom,;
                tipope with iif(m.tipdoc='P/A',-1,iif(m.tipdoc='P/R',-1,iif(m.tipdoc='H/M',IIF(m.tipope='-',-1,1),1))),;
                Codpart WITH M.CodPart
       ENDIF
       UNLOCK
       
    ENDSCAN
    SELE ITEHC
    SET RELATION OFF INTO HOJMOD
        
    ************************
    * DE COMPROBANTE DE PAGO
    ************************
    USE IN 8
    USE IN 9
    USE IN 10
	USE ComPag   IN 9   order tag ComPag1  ALIAS ComPag
	USE IteCP    IN 10  ORDER TAG IteCP1   ALIAS IteCP
    USE Reten    IN 13  order tag Reten1   ALIAS Reten    
    
    SELECT IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG  additive
    set relation to VPERIODO+'01001'+ALLTRIM(ITECp.CODCAD) INTO MAEPRE additive
    
    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. ;
                  COMPAG.Codfte=ALLTRIM(vCodFte) and COMPAG.estado#'99' AND ;
                  !EMPTY(COMPAG.NUMCP) AND COMPAG.TIPDOC$'HCRGSHSR' AND ;
                  COMPAG.periodo=alltrim(vperiodo) AND !EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc) and ;
                  iif(empty(vespgas),.t.,subs(codpart,5,2)=alltrim(vespgas)) and ;
 		          IIF(EMPTY(vcodfun),.t.,maepre.codfun=alltrim(vcodfun))  and ;
 		          IIF(EMPTY(vcodprg),.t.,maepre.codprg=alltrim(vcodprg))  and ;
 		          IIF(EMPTY(vcodspr),.t.,maepre.codspr=alltrim(vcodspr))  and ;
 		          IIF(EMPTY(vactpry),.t.,maepre.actpry=alltrim(vactpry))  and ;
 		          iif(empty(vcodcom),.t.,itecp.codcom=alltrim(vcodcom)) 

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref   with compag.feccp,;
          		CODPART  WITH M.CODPART,;
          		codcad   with m.codcad,;
                codcom with m.codcom,;
          		NUMMES   WITH M.NUMMES,;
          		tipdoc   with 'C/P',;
          		numref   with m.NumCp,;
          		valpart  with m.impparc-IIF(m.impparc>0,compag.Reten,0),;
         		TIPO     WITH '2',;
         		descri   with fte(),;                
          		tipfte   with compag.docref,;
          		numfte   with compag.numref,;
          		MESFTE   WITH COMPAG.NUMMESHC
       
       UNLOCK
       ENDIF
       SELEC ITECP
    ENDSCAN               
    
    SET RELATION OFF INTO COMPAG
    * SELECT HOJA
    * SET RELATION TO               
    
    ****************
    * DE RETENCIONES
    ****************
    
    SELE RETEN
    SET RELATION TO NUMMES+NUMCP+CODCTC  INTO Compag  
	SET RELATION TO NUMMESHC+NUMHC 		 INTO hoja   ADDITIVE
    set relation to VPERIODO+'01001'+ALLTRIM(ITECp.CODCAD) INTO MAEPRE additive
    
    SET FILTER TO Reten.periodo=alltrim(vperiodO) AND;
                  between(val(Reten.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. ;
                  between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2)) and ;	                  
                  Compag.CodFTE = ALLTRIM(vCodFte) .AND. ;
                  reten.codcad  = alltrim(vcodcad) and ;
                  !EMPTY(Reten.NUMCP) AND ;
                  Reten.estado#'99' AND ;                      
                  Reten.Conpago='û' AND ;
                  Compag.Reten>0 and ;
                  (Hoja.Estado="50" OR Hoja.Estado="52") AND ;
                  !EMPTY(Reten.nummeshc) AND !EMPTY(Reten.Numhc) AND ;
                  Compag.Tipdoc # 'RU' and ;
                  IIf(!EMPTY(vespgas),IIF(len(allt(reten.partret))=6,subs(reten.partRET,5,2)=alltrim(vespgas),.f.),.t.) and ;
 		          IIF(EMPTY(vcodfun),.t.,maepre.codfun=alltrim(vcodfun))  and ;
 		          IIF(EMPTY(vcodprg),.t.,maepre.codprg=alltrim(vcodprg))  and ;
 		          IIF(EMPTY(vcodspr),.t.,maepre.codspr=alltrim(vcodspr))  and ;
 		          IIF(EMPTY(vactpry),.t.,maepre.actpry=alltrim(vactpry))  and ;
 		          iif(empty(vcodcom),.t.,reten.codcom=alltrim(vcodcom)) 

                  
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          	 replace   fecref  with  M.fecpag,;
          	          CODPART  WITH  m.partRET,;
          	           nummes  with  m.mescppg,;
          	           tipdoc  with  'C/P',;
          	           numref  with  m.NumCppg,;
          	          valpart  with  m.valret,;
      	                 TIPO  with  '2',;
          	           DESCRI  with  'RET - '+FTE(),;
          	           tipfte  with  compag.docref,;
          	           numfte  with  compag.numref,;
          	           TIPOPE  with  1,;
          	           mesfte  with  COMPAG.NUMMESHC,;
          	           codcad  with  m.codcad,;
          	           codcom  with  m.codcom,;
          	           TOTCP   with  m.valret
       ENDIF
       UNLOCK
       SELEC RETEN
    ENDSCAN 
    SET FILTER TO
    SET RELATION OFF  INTO Compag  
	SET RELATION OFF  INTO hoja   
    
    *-------------------------------------------------------
    * C/P X  Hojas de Modificaci¢n  con retencion 
    *-------------------------------------------------------
    USE IN 8
    USE IN 9
    USE IN 10
    USE IN 13
    
	USE ComPag   IN 9   order tag ComPag1  ALIAS ComPag
	USE IteCP    IN 10  ORDER TAG IteCP1   ALIAS IteCP
    USE Hojmod   IN 13  order tag Hojmod1  ALIAS Hojmod
    
    
    Sele Compag
    SET RELATION TO NUMMESHM+NUMHM INTO Hojmod
    
    SELECT IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG  
    
    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. ;
                  itecp.codfte=ALLTRIM(vCodFte) .AND. ;
                  COMPAG.estado#'99'  AND  !EMPTY(COMPAG.NUMCP) AND ;
                  COMPAG.periodo=alltrim(vperiodo) AND Hojmod.Estado='50' and ;
                  codcad = alltrim(vcodcad) and ;
                  IIF(!EMPTY(vespgas),subs(codpart,5,2)=alltrim(vespgas),.t.)
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with compag.feccp,;
          		CODPART WITH M.CODPART,;
          		NUMMES WITH M.NUMMES,;
          		tipdoc with 'C/P',;
          		numref with m.NumCp,;
          		codcad with m.codcad,;
          		codcom with m.codcom,;
          		valpart with m.impparc-compag.Reten,;
         		TIPO WITH '2',;
         		descri with 'C/P por H/M'+ fte(),;                
          		tipfte with compag.docref,;
          		numfte with compag.numref,;
          		MESFTE WITH COMPAG.NUMMESHC
       ENDIF
       
       UNLOCK
       SELEC ITECP
    ENDSCAN               
    
    Sele Compag
    SET RELATION OFF INTO Hojmod
    
    SELECT IteCp
    SET RELATION OFF INTO COMPAG  

    *------------------------------
    * DE RETENCIONES
    * aun no definido por teso
    *------------------------------
    
    *------------------------------------------
    * De las Hojas de Anulaci¢n de Cheque H/A
    *------------------------------------------
    
	USE ComPag   IN 9   ORDER TAG ComPag1 ALIAS ComPag
	USE IteCP    IN 10  ORDER TAG IteCP1  ALIAS IteCP
    USE Hojanu   IN 13  ORDER TAG Hojanu3 ALIAS Hojanu
    
    SELECT IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO Compag
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO Hojanu ADDITIVE
    
    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. ;
                  itecp.codfte=ALLTRIM(vCodFte) .AND. ;
                  itecp.codcad = alltrim(vcodcad) and;
                  COMPAG.estado#'99' And ALLT(Compag.Pres)$'*' AND ;
                  !EMPTY(COMPAG.NUMCP) AND COMPAG.TIPDOC$'HCRGSHSR' AND ;
                  COMPAG.periodo=alltrim(vperiodO) AND;
                  !EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc) and ;
                  IIF(!EMPTY(vespgas),subs(codpart,5,2)=alltrim(vespgas),.t.)
     GO TOP
     SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with compag.feccp,;
          		CODPART WITH M.CODPART,;
          		codcad  with m.codcad,;
          		codcom  with m.codcom,;
          		NUMMES  WITH M.NUMMES,;
          		tipdoc  with 'H/A',;
          		numref  with Hojanu.Numha,;
          		valpart with m.impparc-IIF(m.impparc>0,compag.Reten,0),;
         		TIPO WITH '9',;
         		descri with fte(),;                
          		tipfte with compag.docref,;
          		numfte with Compag.Numhc,;
          		MESFTE WITH Compag.NummesHC,;
          		tipope WITH -1 
       ENDIF
*      valpart with IIF(COMPAG.PARTRET=m.CodAnal,m.impparc-compag.Reten,m.impparc),;
       UNLOCK
       SELEC ITECP
    ENDSCAN               
    
    SET RELATION OFF INTO Compag
    SET RELATION OFF INTO Hojanu
    
    
    *------------------------------------
    * De Hojas de Control con movimiento
    * Bancario Hojas.Estado='54'
    *-------------------------------
    ************************
    * DE COMPROBANTE DE PAGO
    ************************
    USE IN 8
    USE IN 9
    USE IN 10
    USE IN 13
	USE Movbco   IN 9   order tag MovBco8   ALIAS Movbco

    
    SELECT Movbco
    SET RELATION TO NUMMESHC+NUMHC INTO HojA Additive
    
    SET FILTER TO between(val(Movbco.Nummes),val(vNumMes1),val(vnuMmes2)) .AND.;
                  Movbco.CodFTE=ALLTRIM(vCodFte) .AND. ;
                  Movbco.estado#'99' AND ;
                  Movbco.periodo=alltrim(vperiodO) ;
                  Movbco.codcad = alltrim(vcodcad) ;
                  and hoja.estado$'54' and ;
                  IIF(!EMPTY(vespgas),subs(codpart,5,2)=alltrim(vespgas),.t.)
    SEle Movbco
    Go Top    
                
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with Movbco.Fecha,;
          		CODPART WITH M.CODPART,;
          		codcad  WITH m.codcad,;
          		codcom  with m.codcom,;
          		NUMMES WITH M.NUMMES,;
          		tipdoc with 'N/C',;
          		numref with m.Numero,;
          		valpart with m.monto,;
         		TIPO WITH '2',;
         		descri with fte(),;                
          		MESFTE WITH m.NUMMESHC,;
          		tipfte with 'H/C',;
          		numfte with m.numhc
       ENDIF
       UNLOCK
       SELEC Movbco
    ENDSCAN   
                
    SELECT Movbco
    SET RELATION OFF INTO HojA 
    *-------------------------------
*    *************************************
*    * DE REGULARIZACIONES CON RETENCIONES
*    *************************************
*    
*    USE IN 15
*    
*	USE MaeReg   IN 15  ORDER TAG Maereg2  ALIAS MaeReg
* 	USE IteReg   IN 10  ORDER TAG Itereg1  ALIAS IteReg
* 	USE ComPag   IN 9   order tag ComPag1  ALIAS ComPag
	
*	Windi=SYS(3)+'.IDX'
	
*	SELE Maereg
*	INDEX ON NUMMESHC+NUMHC TO (windi)
	
*	SELE Hoja
*	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	
*    SELE Maereg    
*    SET RELATION TO NUMMES+NUMero                  INTO Itereg  
*  	SET RELATION TO NUMMESCP+NUMCP+ALLTRIM(CODCTC) INTO Compag  ADDITIVE

*    SET FILTER TO between(val(Maereg.Nummes),val(vNumMes1),val(vnuMmes2)) AND;
* 	                Itereg.CodFTE=ALLTRIM(vCodFte) AND;
*                    Itereg.estado#'9' AND;
*                    itereg.codcad=alltrim(vcodcad) and ;
*                    Maereg.Periodo=alltrim(vperiodO) AND;
*                    Maereg.Reten >0     
*                    
*    SELE Maereg
*    GO TOP
*    SCAN
*       SCATTER MEMVAR 
*       SELECT AST
*       IF F_APPD()
*           	repla fecref  with Maereg.fecreg,;
*           	      NUMMES  WITH Maereg.NUMMESCp,;
*           	      tipdoc  with 'C/P',;
*           	      numref  with m.NumCp,;
*           	      valpart with m.importe,;
*           	        TIPO  WITH '2',;
*           	      DESCRI  WITH 'REG - '+FTE(),;
*           	      tipfte  with compag.docref,;
*           	      numfte  with compag.numref,;
*           	      TIPOPE  WITH 1,;
*           	      mesfte  WITH COMPAG.NUMMESHC,;
*           	      codcad  WITH m.codcad,;
*                  TOTCP WITH m.importe-m.Reten
*       ENDIF
*       UNLOCK
*       SELEC Maereg
*       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
*    ENDSCAN    
    
               
*    SET RELA TO
*    SELE MAEREG
*    SET INDEX TO
*    ERASE (wINDi)
    
*	SELE Hoja
*	SET RELATION OFF INTO Maereg
*	SET RELATION OFF INTO Compag 
	
*    SELE Maereg
*    SET RELATION OFF INTO Itereg
    
    *************************************
    * DE REGULARIZACIONES SIN RETENCIONES
    *************************************
*    USE IN 5
*    USE IN 13
	
*	WINDi=SYS(3)+'.IDX'
	
*	SELE Maereg
*	INDEX ON NUMMESHC+NUMHC TO (windi)
	
*	SELE Hoja
*	SET RELATION TO NUMMES+NUMHC          INTO Maereg
*	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
*	SELE MAEREG
*	SET RELATION TO NUMMES+NUMero         INTO ITEREG  
*   SELE IteReg
    
*    SET FILTER TO between(val(Itereg.Nummes),val(vNumMes1),val(vnuMmes2)) AND;
* 	                Itereg.CodFTE=ALLTRIM(vCodFte) AND;
*                    Itereg.estado#'9' AND;
*                    Itereg.periodo=alltrim(vperiodO) and ;
*                    itereg.codcad=alltrim(vcodcad)
*    SELE IteREg
*    GO TOP
    
*   SELE MAEREG   
*   SCATTER MEMVAR 

*    SELE IteREg    
*    SCATTER MEMVAR 
*    SCAN
    
*	   SELE MAEREG   
*      SCATTER MEMVAR 
       
*       SELE IteREg    
*       SCATTER MEMVAR 
*       SELECT AST
*       IF F_APPD()
*             	repla fecref with M.fecreg,;
*             	     cODANAL WITH M.CODANAL,;
*             	     CODPART WITH M.CODPART,;
*           	         NUMMES  WITH M.NUMMESCp,;
*             	      tipdoc with 'C/P',;
*             	      numref with m.NumCp,;
*             	      valpart with m.impparc,;
*             	        TIPO WITH '2',;
*             	      DESCRI WITH 'REG -'+FTE(),;
*             	      tipfte with compag.docref,;
*             	      numfte with compag.numref,;
*             	      TIPOPE WITH 1,;
*             	      mesfte WITH m.NUMMESHC,;
*             	      codcad with m.codcad,;
*             	      TOTCP WITH m.impparc
*       ENDIF
*       UNLOCK
       
*      SELEC IteREg
*       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
       
*    ENDSCAN    
    
*   SELECT MAEREG
*    SET INDEX TO
*    ERASE (wINDi)
    
*	SELECT Hoja
*	SET RELATION OFF INTO Maereg
*	SET RELATION OFF INTO Compag 
*	SET RELATION OFF INTO ITEREG  
	
	
    USE IN 6
    USE IN 8
    USE IN 9
    USE IN 13
    USE IN 15
    USE itepar   in 8   order tag itepar1  ALIAS Itepar 
    USE clase   IN 15  order tag Clase1  ALIAS Clase
    SELECT AST
    VIND=SYS(3)+'.IDX'
    INDEX ON codpart+NUMMES+CODcad+TIPO+TIPDOC+NUMREF TO (VIND) 
    SET RELATION TO ALLTRIM(VPERIODO)+ALLTRIM(VUNIGES)+ALLTRIM(VUNIEJE)+CODCAD INTO MAEPRE
    SET INDEX TO (VIND)
    do espera1 with 2
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       USE IN 1
       USE IN 2
       SET RELATION TO ALLT(vperiodo) + ALLT(vUniGes) + ALLT(vUniEje) + ast.codcad INTO maepre
       SET FILT TO !EMPTY(CODCAD) AND CODCAD#'0000' AND LEN(ALLT(CODPART))=6
       GO TOP
       SCAN
             Replace ast.Uniges With maepre.uniges
             Replace ast.Unieje With maepre.unieje
             Replace ast.Codfun With maepre.codfun
             Replace ast.codprg With maepre.codprg
             Replace ast.Codspr With maepre.codspr
             Replace ast.Actpry With maepre.actpry
       ENDSCAN
       
       
    *-----------------------------------------------------------------------------
    *********************************************
    * RESUMEN EN EL ARCHIVO
    *********************************************
    USE IN 5
    USE IN 6
    USE IN 8
    USE IN 9
    USE IN 13
    USE IN 15
    
    SELE AST
    GO TOP
	IF !EOF()
    	 vTotAfe = 0
         GO TOP                                     
         SCAN 
	    	vPart1    = ast.uniges+ ast.unieje+ ast.codfun+ ast.codprg+ ast.codspr+ ast.actpry+ ast.codcom+ ast.codpart
		    vPart2    = ast.uniges+ ast.unieje+ ast.codfun+ ast.codprg+ ast.codspr+ ast.actpry+ ast.codcom+ ast.codpart
    	    vComBru   = 0
    	    vComLiqac = 0
    	    vEjebru   = 0
    	    vEjereb   = 0
    	    vEjeLiqac = 0
    	    vCompBru  = 0
       		vComLiqac = 0
	   		vrebmes   = 0
	   		vrebant   = 0
	   		ValorHA   = 0
	   		
        	DO WHILE  vPart1 = vPart2
               do case
                  case AST.TIPO='1'   && DE H/C
                       vcompbru = vcompbru + IIF(ast.nummes=alltrim(vnummes2),ast.valpart,0)
                       vcomliqac = vcomliqac + ast.valpart
                  case AST.TIPO='2'   && DE C/P               
                       vejebru   =  vejebru + IIF(ast.nummes=alltrim(vnummes2),ast.valpart,0)
                       vejeliqac =  vejeliqac + IIF(ast.nummes<=alltrim(vnummes2),ast.valpart,0)
                  case AST.TIPO='3'   && DE H/M
                       vrebmes   = vrebmes  + iif(ast.tipcom='C' and ast.tipope=-1 AND ast.MesFte=alltrim(vnummes2) AND ast.NumMes=alltrim(vnummes2),ast.valpart,0)
                       vrebant   = vrebant  + iif(ast.tipcom='C' and ast.tipope=-1 AND ast.Mesfte<alltrim(vnummes2) AND ast.NumMes=alltrim(vnummes2),ast.valpart,0)
                       vcompbru  = vcompbru  + IIF(ast.nummes=alltrim(vnummes2) and ast.tipope=1 and ast.tipcom='C',ast.valpart,0)
                       vcomliqac = vcomliqac + iif(ast.tipcom='C',ast.tipope*ast.valpart,0)
                       ** EJECUCION
                       vejebru   =  vejebru + iif((ast.tipcom='P' OR ast.tipcom='CYP') AND ast.nummes=alltrim(vnummes2) AND ast.tipope= 1, ast.valpart,0)
                       vejereb   =  vejereb + iif((ast.tipcom='P' OR ast.tipcom='CYP') and ast.nummes=alltrim(vnummes2) AND ast.tipope=-1 ,ast.valpart,0)
                       vejeliqac =  vejeliqac + iif((ast.tipcom='P' OR ast.tipcom='CYP') AND ast.nummes#alltrim(vnummes2) AND ast.tipope=-1 ,ast.valpart*-1,iif((ast.tipcom='P' OR ast.tipcom='CYP') AND ast.nummes<=alltrim(vnummes2) AND ast.tipope=1,AST.VALPART,0))
                  case AST.TIPO='4'   && DE P/A 
                       vrebmes =  vrebmes + IIF(ast.mesfte=alltrim(vnummes2) and ast.Nummes=alltrim(vnummes2),ast.valpart,0)
                       vrebant =  vrebant + IIF(ast.mesfte<alltrim(vnummes2) AND ast.Nummes=alltrim(vnummes2),ast.valpart,0)
                       vcomliqac = vcomliqac + ast.tipope*ast.valpart
                  case AST.TIPO='5'   && DE  P/R
                       vrebmes =  vrebmes + IIF(ast.mesfte=alltrim(vnummes2) AND AST.NUMMES=ALLTRIM(VNUMMES2),ast.valpart,0)
                       vrebant =  vrebant + IIF(ast.mesfte<alltrim(vnummes2) AND AST.NUMMES=alltrim(vnummes2),ast.valpart,0)
                       vcomliqac = vcomliqac + ast.tipope*ast.valpart
                  case AST.TIPO='9'   && DE H/A               
                       * Disminuye la ejecuci¢n
                       vejebru   =  vejebru - IIF(ast.nummes=alltrim(vnummes2),ast.valpart,0)
                       vejeliqac =  vejeliqac - IIF(ast.nummes<=alltrim(vnummes2),ast.valpart,0)
            endcase   		                 		                 		                     
	        SKIP
	        vPart1 = iif(ALLTRIM(vtipfun) = 'I',AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+AST.CodPart,AST.CODSUBPR+AST.CODACT+ast.codanal)
	        ENDD
	        SKIP -1
		    sele ast_p
		    seek iif(ALLTRIM(m.tipfun) = 'I',AST.CODSUBPR+AST.CODPROY+AST.CODSUPRY+AST.CodPart,AST.CODSUBPR+AST.CODACT+ast.codanal)
		    if found()
		       if rlock()
                 replace comPbru with compBru + vCompbru,; 
                         rebmes  with rebmes + vRebmes ,;
                         rebant  with rebant + vRebAnt,;
                         comliq  with comliq + vCompBru - (vRebmes + vRebant),;
                       comliqac  with comliqac + vcomliqac ,;
                         Ejebru  with Ejebru + vejebru ,;
                         EjeReb  with EjeReb + vejereb ,;
                         EjeLiq  with Ejeliq + vEjebru - vEjereb,;
                       ejeliqac  with Ejeliqac + vEjeliqac - vEjereb 
			   endif
			   unlock	                     
            else
               if f_appd()
                  replace codpart with ast.codpart,;
                          codanal with ast.codanal,;
                          comPbru with vCompbru,; 
                          rebmes  with vRebmes ,;
                          rebant  with vRebAnt,; 
                          comliq  with vCompBru - (vRebmes + vRebant),;
                        comliqac  with vcomliqac ,;
                          Ejebru  with vejebru ,;
                          EjeReb  with vejereb ,;
                          EjeLiq  with vEjebru - vEjereb,;
                        ejeliqac  with vEjeliqac - vEjereb ,;
                     	  codprg  with AST.codprg,;
          		        codsubpr  with AST.Codsubpr,;
                         codproy  with AST.codproy,;
                        codsupry  with AST.codsupry,;
				          codact  with AST.codact   
               endif   
               unlock
	        endif   
	        SELE ast
 	   ENDSCAN
    else
       do standby with 'No hay registros para pocesar..'
    endiF
    
    QIDX = sys(3)+'.IDX'
    SELECT AST_p
    SET UNIQ ON
    if ALLTRIM(vtipfun) = 'I'
       INDEX ON CODSUBPR+CODPROY+CODSUPRY+LEFT(CodPart,2) TO (Qidx)
    ELSE   
       INDEX ON CODSUBPR+CODACT+LEFT(codanal,2) TO (Qidx)
    ENDIF   

    Qdbf = sys(3)+'.dbf'
    copy to (Qdbf)
    USE (QDBF) IN 20  ALIAS AST_A
    SET UNIQ OFF
    SELECT AST_p 
    set index to (XIDX)

    SELECT AST_A

    GO TOP
    ***********************************
    * AGREGA '00'
    ***********************************
    SCAN 
        vp1 = iif(ALLTRIM(vtipfun) = 'I',AST_A.CODSUBPR+AST_A.CODPROY+AST_A.CODSUPRY+left(ast_a.codpart,2),AST_A.CODSUBPR+AST_A.CODACT+left(ast_A.codanal,2))
        SELE AST_P
	   	SUM CompBru,Rebmes,Rebant,Comliq,Comliqac,Ejebru,Ejereb,Ejeliq,Ejeliqac TO A1,A2,A3,A4,A5,A6,A7,A8,A9 FOR iif(ALLTRIM(vtipfun) = 'I',AST_P.CODSUBPR+AST_P.CODPROY+AST_P.CODSUPRY+LEFT(AST_P.CODPART,2),AST_P.CODSUBPR+AST_P.CODACT+LEFT(AST_P.CODANAL,2)) = VP1
	   	IF F_APPD()
	   	   if ALLTRIM(vtipfun) = 'I'
	   	      repla codpart with left(ast_a.codpart,2)+'.00'
	   	   else
	   	      repla codAnal with left(ast_a.codanal,2)+'.00'
	   	   endif   
           REPLACE compbru with A1,Rebmes with a2,rebant with A3,Comliq with A4,ComLiqac with A5,Ejebru with A6, Ejereb with A7 ,Ejeliq with A8, Ejeliqac with A9,;
               	   codprg  with AST_A.codprg,codsubpr  with AST_A.Codsubpr,codproy  with AST_A.codproy,codsupry  with AST_A.codsupry,codact  with AST_A.codact   
        endif
        unlock   
        sele ast_a
    ENDsca
    DEACTIVATE WINDOW STANDBY       
    sele ast_p
    USE Maepre   IN 7   order tag Maepre1  ALIAS maepre
    USE itepar   in 8   order tag itepar1  ALIAS Itepar 
    
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
       
       *-----------------------------------------------------------------------------
       
       
       SELECT asT
       INDEX ON ast.uniges+ ast.unieje+ ast.codfun+ ast.codprg+ ast.codspr+ ast.actpry+ ast.codcom+ ast.codpart + nummes + tipo + numref to (vind)       
       DO REPORTE WITH 2,"AUXCOM1",' Movimiento Presupuestal '
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
   CASE TIPDOC='H/A'     
        DEVUL = 'Anulaci¢n de Cheque '+Hojanu.Nummescp+' '+Hojanu.Numcp+' '+Hojanu.Codctc
   CASE TIPDOC='H/C' 
        DEVUL = IIF(HOJA.TIPPRV='P',val_pro(HOJA.Codprv),IIF(HOJA.TipPRV='E',val_pro(HOJA.Codemp),HOJA.NOMBRE))
   CASE TIPDOC='C/P'
        DEVUL = IIF(compag.TIPPRV='P',val_pro(compag.Codprv),IIF(compag.TipPRv='E',val_pro(compag.Codemp),compag.NOMpRE))
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
         
   CASE TIPDOC='N/C'   
        DEVUL = 'N/C : '+TIPFTE+' '+NUMFTE
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



PROCEDURE XXTIPHMAD   && ASIGNACIONES DEBE
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


PROCEDURE TIPHMAD   && ASIGNACIONES DEBE
*----------------
PRIVATE vfun
vfun = .F.
DO CASE 
   CASE TIPO ='3'
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
	CASE TIPO ='9' and Tipdoc='H/A' and  tipope#-1
		 vfun = .T.
ENDCASE	    
return vfun



      
PROCEDURE XXTIPHMAH   && ASIGNACIONES HABER
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

PROCEDURE TIPHMAH   && ASIGNACIONES HABER
*----------------
PRIVATE vfun
vfun = .F.
DO CASE
   CASE TIPO ='3'
		if (operac='C' and tipcom='CYP') or;
		   (operac='C' and tipcom='P  ') or;
		   (operac='T' and tipcom='CYP') or;
		   (operac='T' and tipcom='C  ' and tipope#-1) or;
		   (operac='T' and tipcom='P  ' and tipope=-1) or;
		   (operac='R' and tipcom='CYP' and tipope=-1) or;
		   (operac='R' and tipcom='P  ' and tipope=-1)  
		   vfun = .T.
		endif
	CASE TIPO ='9' and Tipdoc='H/A' and tipope=-1
		 vfun = .T.
ENDCASE	
return vFun


PROCEDURE XXTIPHMED    && EJECUCION DEBE
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


PROCEDURE TIPHMED    && EJECUCION DEBE
*----------------
PRIVATE vfun
vfun = .F.
DO CASE 
   CASE TIPO ='3'
	   If (operac='C' and tipcom='CYP' and tipope=-1) or;
    	  (operac='C' and tipcom='P  ' and tipope=-1) or;
	      (operac='T' and tipcom='CYP' and tipope=-1) or;
    	  (operac='T' and tipcom='P  ' and tipope=-1) or;
	      (operac='R' and tipcom='CYP' and tipope=-1) or;
    	  (operac='R' and tipcom='P  ' and tipope=-1) 
		  vfun = .T.
		endif
	CASE TIPO ='9' and Tipdoc='H/A' and tipope=-1
		 vfun = .T.
ENDCASE
return vfun


      
PROCEDURE XXTIPHMEH   && EJECUCION HABER
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

PROCEDURE TIPHMEH   && EJECUCION HABER
*----------------
PRIVATE vfun
vfun = .F.

DO CASE
   CASE TIPO ='3'              
	   If (operac='C' and tipcom='CYP' and tipope#-1) or;
    	  (operac='C' and tipcom='P  ' and tipope#-1) or;
	      (operac='T' and tipcom='CYP' and tipope#-1) or;
    	  (operac='T' and tipcom='P  ' and tipope#-1)  
		  vfun = .T.
		endif
	CASE TIPO ='9' and Tipdoc='H/A' and tipope#-1
		 vfun = .T.
ENDCASE	
return vfun



PROCEDURE SUMATOT
*----------------
GO TOP
SUM COMPBRU,REBMES,REBANT,COMLIQ,COMLIQAC,EJEBRU,EJEREB,EJELIQ TO A1,A2,A3,A4,A5,A6,A7,A8 FOR IIF(ALLTRIM(VTIPFUN)='I',RIGHT(CODPART,2)#'00',RIGHT(CODANAL,2)#'00')
GO TOP
RETURN

FUNCTION pinta_estr
*------------------
vcodfun = maepre.codfun
vcodprg = maepre.codprg
vcodspr = maepre.codspr
vactpry = maepre.actpry
vcodcom = maepre.codcom

@  6,25 SAY VAL_PARA(Vcodfun,'CODFUN','D',18,40)
@  7,25 SAY VAL_PARA1(VCodPrg,'CODPRG'+VCodFun,'D',18,40) 
@  8,25 SAY VAL_PARA1(VCodSPr,'CODSPR'+VCodPrg,'D',18,40)
@  9,25 SAY VAL_PARA(Vactpry,'ACTPRY','D',18,40)
@ 10,25 SAY VAL_PARA(Vcodcom,'CODCOM','D',18,40)

@  6,18 GET vcodfun PICTURE '!!'     WHEN .F.
@  7,18 GET vcodprg PICTURE '!!!'    WHEN .F.
@  8,18 GET vcodspr PICTURE '!!!!'   WHEN .F.
@  9,18 GET vactpry PICTURE '!!!!!!' WHEN .F.
@ 10,18 GET vcodcom PICTURE '!!!!!'  WHEN .F. 

RETURN .T.


FUNCTION buscad
*--------------
PARAMETER vcodigo,vcontrol
DO CASE
   case vcontrol='1'   && Funci¢n
        v = (maepre.codfun = alltrim(vcodigo))
   case vcontrol='2'   && Programa
        v = (maepre.codprg = alltrim(vcodigo))
   case vcontrol='3'   && Subprograma
        v = (maepre.codspr = alltrim(vcodigo))
   case vcontrol='4'   && Actividad proyecto
        v = (maepre.actpry = alltrim(vcodigo))
   other
        v = .f.
endcase
return(v)
       
           
