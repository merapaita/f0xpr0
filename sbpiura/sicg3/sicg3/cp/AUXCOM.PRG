** ---------------------------------------------------
** REPOE5.PRG
** Reporte Auxiliar de Compromisos por partidas
** ---------------------------------------------------
* Abrir Archivos de trabajo
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE ItePar   IN 4   ORDER TAG ItePar1  ALIAS ItePar
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE Calen    IN 8   order tag calen1   ALIAS calen
USE Maepre   IN 9   order tag Maepre1  ALIAS Maepre
USE Clase    IN 10  order tag Clase1   ALIAS Clase
USE Auxil    IN 14  order tag Auxil2   ALIAS Auxil
USE hojmod   IN 15  order tag hojmod1  ALIAS HOJMOD
USE PteAnu   IN 16  order tag PteAnu9  ALIAS AnuPA
USE ASTAUX   IN 17                     ALIAS AST



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
vCodSubPr = '   '
vCodPart  = '     '
vCodPry   = '   '
vCodSpy   = '  '
vCodact   = '  '

 DEFINE WINDOW Lis FROM 5,12 TO 18,70 DOUBLE ;
 TITLE ' °° Auxiliar de Compromisos °° ' FLOAT COLOR SCHEME 5

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
 @ 10,2 say '        Partida : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vNumMes1   PICTURE '!!'  VALID Val_Para(vNumMes1 ,'FECMES','C',21,30,4) AND !EMPTY(vNumMes1)
 @  2,31  GET vNumMes2   PICTURE '!!'  VALID Val_Para(vNumMes2 ,'FECMES','C',31,20,4) AND !EMPTY(vNumMes2) 
 @  3,21  GET vTipfun    PICTURE '!'   VALID Val_Para(vTipfun  ,'TIPFUN','C',31,20,4) 
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30,4) AND !EMPTY(vCodFte) 
 @  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30,4) AND !EMPTY(vCodPrg)
 @  6,21  GET vCodSubPr  PICTURE '!!!' VALID IIF(EMPTY(vCodSubPr),.T.,VAL_SUBP(vCodSubPr,'CODSUB'+VCodPrg,' ',21,28))
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
    
    SELECT AST
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 17 ALIAS AST

    VIND=SYS(3)+'.IDX'
    IF ALLTRIM(vtipfun)='F'
       INDEX ON NUMMES+CODSUBPR+CODACT+codANAL+TIPO+TIPDOC+NUMREF TO (VIND)
    ELSE
       *INDEX ON codPART+NUMMES+CODSUBPR+CODPROY+CODSUPRY+TIPO+TIPDOC+NUMREF TO (VIND) 
       INDEX ON NUMMES+CODSUBPR+CODPROY+CODSUPRY+codPART+TIPO+TIPDOC+NUMREF TO (VIND) 
    ENDIF
    SET INDEX TO (VIND)
    *-------------------
    * DE HOJA DE CONTROL
    *-------------------    
    * SELE Anupa 
    * SET ORDER TO TAG Pteanu9
    
    SELEC ITEHC
    SET RELA TO NUMMES+NUMHC INTO hoja                      
    SET RELA TO NUMMES+NUMHC INTO AnuPa ADDITIVE
    
    IF !EMPTY(vcodpart)   
    	SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',CODANAL=vcodpart,CODPART=vcodpart) AND CodSubPr=alltrim(vCodSubPr) .and.;
                  estado#'9' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),codact=vcodact,.t.) AND tipfun = alltrim(vtipfun)
    ELSE
     	SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodSubPr) .and.;
                  estado#'9' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),codact=vcodact,.t.) AND tipfun = alltrim(vtipfun)
    ENDIF                  
                  
    GO TOP
    IF EOF()
       RETURN
    ENDIF   
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'       
	          repla fecref with hoja.fechc,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH M.NUMMES,tipdoc with 'H/C',numref with m.Numhc,codprg with m.codprg,codsubpr with m.Codsubpr,Codproy with m.codproy,codsupry with m.codsupry,valdeb with valpart,valhab with m.valpart,TIPO WITH '1',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH 1,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF,DESTINO WITH HOJA.DESTINO
	      else    
    	      repla fecref with hoja.fechc,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH M.NUMMES,tipdoc with 'H/C',numref with m.Numhc,codprg with m.codprg,codsubpr with m.Codsubpr,codact with m.codact,valdeb with valpart,valhab with m.valpart,TIPO WITH '1',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH 1,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF,DESTINO WITH HOJA.DESTINO
    	  endif    
       ENDIF
       UNLOCK
       SELEC ITEHC         
     @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN               

    *-----------------------
    * DE PARTES DE REBAJA
    *-----------------------
    
    SELEC ITEHC
    
    IF !EMPTY(vcodpart)   
    	SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',CODANAL=vcodpart,CODPART=vcodpart) AND CodSubPr=alltrim(vCodSubPr) .and.;
                  estado='94' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),codact=vcodact,.t.) AND tipfun = alltrim(vtipfun)
    ELSE
     	SET FILTER TO between(val(Nummes),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodSubPr) .and.;
                  estado='94' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),codact=vcodact,.t.) AND tipfun = alltrim(vtipfun)
    ENDIF                  
                  
    GO TOP
   SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'       
	          * repla fecref with hoja.fechc,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH M.NUMMES,tipdoc with 'P/R',numref with m.NumPR,codprg with m.codprg,codsubpr with m.Codsubpr,Codproy with m.codproy,codsupry with m.codsupry,valdeb with valpart,valhab with m.valpart,TIPO WITH '5',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH -1,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF
	          repla fecref with AnuPa.fecpa,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH PADL(ALLTRIM(STR(MONTH(AnuPa.Fecpa),2)),2,'0'),tipdoc with 'P/R',numref with m.NumPR,codprg with m.codprg,codsubpr with m.Codsubpr,Codproy with m.codproy,codsupry with m.codsupry,valdeb with valpart,valhab with m.valpart,TIPO WITH '5',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH -1,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF
	      else
    	      repla fecref with AnuPa.fecpa,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH PADL(ALLTRIM(STR(MONTH(AnuPa.Fecpa),2)),2,'0'),tipdoc with 'P/R',numref with m.NumPR,codprg with m.codprg,codsubpr with m.Codsubpr,codact with m.codact,valdeb with valpart,valhab with m.valpart,TIPO WITH '5',DESCRI WITH FTE(),tipfte with hoja.tipdoc,numfte with hoja.numref,mesfte with hoja.nummes,TIPOPE WITH -1,TOTHC WITH HOJA.IMPTOT,DESFTE WITH HOJA.DESREF
    	  endif    
       ENDIF
       UNLOCK
       SELEC ITEHC         
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*     
    ENDSCAN               

    *-----------------------
    * DE HOJAS DE MODIFICACION
    *-----------------------
    SELEC ITEHC
    SET ORDE TO 4
    IF !EMPTY(vcodpart)
    SET FILTER TO between(val(NummesHm),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. TIPFUN=ALLTRIM(VTIPFUN) AND ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',CODANAL=vcodpart,CODPART=vcodpart) AND CODSUBPR=alltrim(vCodSubPr) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),codact=vcodact,.t.)  &&AND TIPOPE='-'
    ELSE
    SET FILTER TO between(val(NummesHm),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. TIPFUN=ALLTRIM(VTIPFUN) AND ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CODSUBPR=alltrim(vCodSubPr) AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),codact=vcodact,.t.)  &&AND TIPOPE='-'
    ENDIF                  
    GO TOP
    SET RELA TO NUMMESHM+NUMHM INTO hojMOD
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       vDescr = 'Rebaja de la H/C : '+hojmod.nummeshc+'.'+hojmod.numhc+' '+hojmod.tiphm
       IF F_APPD()
         repla fecref with hojMOD.fechM , CODANAL WITH m.CODanal, CODPART WITH m.CODPART, NUMMES WITH m.NUMMESHM, tipdoc with 'H/M', numref with m.NumhM, codprg with m.codprg, codsubpr with m.Codsubpr, valdeb with m.VALPART, valhab with m.VALPART,TIPO WITH '3',TIPFTE WITH Hojmod.Tiphm,OPERAC WITH HOJMOD.OPERAC,TIPOPE WITH IIF(m.tipope='-',-1,1)
         DO CASE
            CASE HOJMOD.OPERAC='T'  
                 vDescr = 'Transf.Int.de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
            CASE HOJMOD.OPERAC='R'  
                 vDescr = 'Rebaja de la H/C : '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
            CASE HOJMOD.OPERAC='C'  
                 vDescr = 'Cambio Fte. de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
            CASE HOJMOD.OPERAC='A'  
                 vDescr = 'Anulaci¢n del Compromiso de la H/C: '+hojmod.nummeshc+'.'+hojmod.numhc+' '+val_para(hojmod.tiphm,'HOJMOD','D',22,18)
         ENDCASE
         if alltrim(vtipfun)='I'
            REPLACE  DESCRI WITH vDescr,codprg with m.codprg,codsubpr with m.Codsubpr,Codproy with m.codproy,codsupry with m.codsupry
         else   
            REPLACE  DESCRI WITH vDescr,codprg with m.codprg,codsubpr with m.Codsubpr,codact with m.codact
         endif               
       ENDIF
       UNLOCK
       SELEC itehc
     @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*    
    ENDSCAN  
                 
    USE IN 15
           
    *-----------------------
    * DE PARTES DE ANULACION
    *-----------------------
    SELEC ITEHC
    SET ORDE TO ITEHC1
    SET RELA TO NUMMES+NUMHC INTO hoja                      
    SET RELA TO NUMMES+NUMHC INTO ANUPA ADDI                      
    IF !EMPTY(vcodpart)
    SET FILTER TO between(val(ANUPA.NUMMESTR),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',CODANAL=vcodpart,CODPART=vcodpart) AND CodSubPr=alltrim(vCodSubPr) .and.;
                  estado#'9' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND !empty(HOJA.numanu) AND IIF(!empty(vcodact),codact=vcodact,.t.) AND TIPFUN=ALLTRIM(VTIPFUN) 
    ELSE
	SET FILTER TO between(val(ANUPA.NUMMESTR),val(vNumMes1),val(vnuMmes2)) .AND. CODPRG=ALLTRIM(vCodPrg) .AND. ;
                  CODFTE=ALLTRIM(vCodFte) .AND. CodSubPr=alltrim(vCodSubPr) .and.;
                  estado#'9' AND iif(!EMPTY(vCodPry),Codproy=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),CodSupry=ALLTRIM(vCodSpy),.t.) AND !empty(HOJA.numanu) AND IIF(!empty(vcodact),codact=vcodact,.t.) AND TIPFUN=ALLTRIM(VTIPFUN) 
    ENDIF              
                  
    GO TOP

    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          repla fecref with ANUPA.fecPA,CODANAL WITH M.CODANAL,CODPART WITH M.CODPART,NUMMES WITH ANUPA.NUMMESTR,tipdoc with 'P/A',numref with ANUPA.NumPA,codprg with m.codprg,codsubpr with m.Codsubpr,valdeb with m.valpart,valhab with m.valpart,TIPO WITH '4',DESCRI WITH FTE(),TIPFTE WITH HOJA.TipDOC,NUMFTE WITH HOJA.NUMREF,mesfte with hoja.nummes,TIPOPE WITH -1,codprg with m.codprg,codsubpr with m.Codsubpr,Codproy with m.codproy,codsupry with m.codsupry,codact with m.codact
       ENDIF
       UNLOCK
       SELEC ITEHC         
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN 
                  
    SET RELA TO    
    
  
    USE IN 16
    USE IN 1
	USE IN 2
	*----------	
	
	USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
	USE ComPag   IN 5   ORDER TAG ComPag1  ALIAS ComPag
	USE Reten    IN 2   ORDER TAG Reten1   ALIAS Reten
	
*   SELE Hoja 
*   SET RELATION TO NUMMEScp+NUMCP+CODCTC INTO Reten
*   SET RELATION TO NUMMEScp+NUMCP+CODCTC INTO COMPAG ADDITIVE
	
    SELE RETEN
    SET RELATION TO NUMMES+NUMCP+CODCTC  INTO Compag  
    SET RELATION TO NUMMESHC+NUMHC INTO hoja ADDI
    
*   SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG ADDITIVE
    
    
    * SE PROCESAN LAS RETENCIONES DE LOS COMPROBANTES REGULARIZADOS Y NORMALES 
    
    *------------
    * RETENCIONES
    *------------
    
    IF !EMPTY(vcodpart)                &&Nummeshc
	    SET FILTER TO between(val(Reten.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(Reten.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',RETEN.PARTRET=vcodpart,.T.) AND SUBSTR(Reten.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  Reten.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(Reten.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(Reten.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND !EMPTY(Reten.NUMCP) AND IIF(!empty(vcodact),SUBSTR(Reten.CODCAL,13,2)=vcodact,.t.) AND COMPAG.tipfun = alltrim(vtipfun) AND Reten.periodo=alltrim(vperiodO) and  Reten.Conpago='û';
                  and Compag.Reten>0 and (Hoja.Estado="50" OR Hoja.Estado="52") AND (!EMPTY(Reten.nummeshc) AND !EMPTY(Reten.Numhc))
                  
    ELSE
	    SET FILTER TO between(val(Reten.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) AND;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. SUBSTR(Reten.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  Reten.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(Reten.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) and ;
                  iif(!EMPTY(vCodSpy),SUBSTR(Reten.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),SUBSTR(Reten.CODCAL,13,2)=vcodact,.t.)  AND !EMPTY(Reten.NUMCP) AND COMPAG.tipfun = alltrim(vtipfun) AND Reten.periodo=alltrim(vperiodO);
                  AND between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2)) and Reten.Conpago='û';
                  and Compag.Reten>0 and (Hoja.Estado="50" OR Hoja.Estado="52") AND (!EMPTY(Reten.nummeshc) AND !EMPTY(Reten.Numhc))
    ENDIF 
    
    SELE RETEN
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	*repla fecref with M.fecpag,cODANAL WITH M.PARTRET,CODPART WITH Compag.CODPART, NUMMES WITH m.NUMcppg,tipdoc with 'C/P',numref with m.NumCppg,codcal with m.codcal,valdeb with m.valret,valhab with m.valret,TIPO WITH '2',DESCRI WITH 'RET - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH m.valret
          	repla fecref with M.fecpag,CODANAL WITH M.PARTRET, CODPART WITH HOJA.CODPART,NUMMES WITH m.MEScppg,tipdoc with 'C/P',numref with m.NumCppg,codcal with m.codcal,valdeb with m.valret,valhab with m.valret,TIPO WITH '2',DESCRI WITH 'RET - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH m.valret
          else
          	*repla fecref with m.fecpag,cODANAL WITH M.partret,CODPART WITH Compag.CODPART, NUMMES WITH m.numcppg,tipdoc with 'C/P',numref with m.NumCpPG,codcal with m.codcal,valdeb with m.valret,valhab with m.valret,TIPO WITH '2',DESCRI WITH 'RET - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2),TOTCP WITH m.valret
          	repla fecref with m.fecpag,cODANAL WITH M.partret, NUMMES WITH m.MEScppg,tipdoc with 'C/P',numref with m.NumCpPG,codcal with m.codcal,valdeb with m.valret,valhab with m.valret,TIPO WITH '2',DESCRI WITH 'RET - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2),TOTCP WITH m.valret
          endif
       ENDIF
       UNLOCK
       SELEC RETEN
      @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*   
    ENDSCAN 
    
    SET RELA TO
    
    SELE HOJA
	SET RELATION OFF INTO Reten
	SET RELATION OFF INTO Compag 
    SELE Reten
    SET RELATION OFF INTO Compag
    
    USE IN 1              
    USE IN 5
    USE IN 2
    
    *-------------------------
    *Regularizaciones Con Retencion 
    *----------------------------------
    
  	USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
  	USE ComPag   IN 5   ORDER TAG ComPag1   ALIAS ComPag
	USE MaeReg   IN 2   ORDER TAG Maereg2   ALIAS MaeReg
	
	wINDi=SYS(3)+'.IDX'
	
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
    SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag
    
    IF !EMPTY(vcodpart)                &&Nummeshc
	    SET FILTER TO between(val(MaeReg.NummesCP),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',Maereg.PARTRET=vcodpart,MaeReg.PARTRET=vcodpart) AND SUBSTR(Compag.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  Maereg.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(Compag.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(Compag.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND !EMPTY(Maereg.NUMCP) AND IIF(!empty(vcodact),SUBSTR(Compag.CODCAL,13,2)=vcodact,.t.) AND COMPAG.tipfun = alltrim(vtipfun) AND Maereg.periodo=alltrim(vperiodO);
                  and Hoja.Estado="52" and Maereg.reten >0 AND (!EMPTY(Maereg.nummeshc) AND !EMPTY(Maereg.Numhc))
    ELSE
	    SET FILTER TO between(val(Maereg.Nummescp),val(vNumMes1),val(vnuMmes2))  .AND. SUBSTR(cOMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) AND;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. SUBSTR(Compag.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  Maereg.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(Compag.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) and ;
                  iif(!EMPTY(vCodSpy),SUBSTR(Compag.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),SUBSTR(Compag.CODCAL,13,2)=vcodact,.t.)  AND !EMPTY(Maereg.NUMCP) AND COMPAG.tipfun = alltrim(vtipfun) AND Maereg.periodo=alltrim(vperiodO);
                  AND between(val(COMPAG.NummesHC),val(vNumMes1),val(vnuMmes2));
                  and Hoja.Estado="52" and Maereg.reten >0 and (!EMPTY(Maereg.nummeshc) AND !EMPTY(Maereg.Numhc))
    ENDIF 
    
    SELE Maereg
    GO TOP
    
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	*repla fecref with compag.feccp,cODANAL WITH M.PARTRET,CODPART WITH Compag.CODPART, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.importe,valhab with m.importe,TIPO WITH '2',DESCRI WITH 'REG - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH m.importe          	
          	repla fecref with compag.feccp,cODANAL WITH M.PARTRET, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.importe,valhab with m.importe,TIPO WITH '2',DESCRI WITH 'REG - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH m.importe-m.Reten
          else
          	*repla fecref with compag.feccp,cODANAL WITH M.partret,CODPART WITH Compag.CODPART, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.importe,valhab with m.importe,TIPO WITH '2',DESCRI WITH 'REG - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2),TOTCP WITH m.importe
          	repla fecref with compag.feccp,CODPART WITH M.PARTRET, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.importe,valhab with m.importe,TIPO WITH '2',DESCRI WITH 'REG - '+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2),TOTCP WITH m.importe-m.Reten
          endif
       ENDIF
       UNLOCK
       SELEC Maereg
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN    
    
               
    SET RELA TO
    SELE MAEREG
    SET INDEX TO
    ERASE (wINDi)
    
	SELE Hoja
	SET RELATION OFF INTO Maereg
	SET RELATION OFF INTO Compag 
	
    SELE Maereg
    SET RELATION OFF INTO Compag
    
  	USE IN 1   
  	USE IN 5   
	USE IN 2   
	*-------------------------------------------------------
    *Regularizaciones Sin Retencion 
    *----------------------------------
    
  	USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
  	USE ComPag   IN 5   ORDER TAG ComPag1   ALIAS ComPag
	USE MaeReg   IN 2   ORDER TAG Maereg2   ALIAS MaeReg
	USE IteReg   IN 6   ORDER TAG Itereg1   ALIAS IteReg
	
	wINDi=SYS(3)+'.IDX'
	SELE Maereg
	INDEX ON NUMMESHC+NUMHC TO (windi)
	
	SELE Hoja
	SET RELATION TO NUMMES+NUMHC          INTO Maereg
	SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDITIVE
	
	SELE Maereg
    SET RELATION TO NUMMESCP+NUMCP+CODCTC INTO Compag ADDI
	
    SELE Itereg    
    SET RELATION TO NUMMES+NUMero         INTO Maereg  ADDI

    
    SELE IteReg
    
    IF !EMPTY(vcodpart)                &&Nummeshc
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(COMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  COMPAG.CodFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',IteREg.CODANAL=vcodpart,IteREg.CODPART=vcodpart) AND SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND !EMPTY(COMPAG.NUMCP) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO) ;
                  and Hoja.Estado="52" and Maereg,Reten =0 AND (!EMPTY(Maereg.nummeshc) AND !EMPTY(Maereg.Numhc))
    ELSE
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.)  AND !EMPTY(COMPAG.NUMCP) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO);
                  and Hoja.Estado="52" and Maereg,Reten =0 AND (!EMPTY(Maereg.nummeshc) AND !EMPTY(Maereg.Numhc))
    ENDIF 
    
    SELE IteREg
    
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	repla fecref with compag.feccp,cODANAL WITH M.CODANAL,CODPART WITH M.CODPART, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.impparc,valhab with m.impparc,TIPO WITH '2',DESCRI WITH 'REG -'+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH Maereg.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH m.impparc
          else
          	repla fecref with compag.feccp,cODANAL WITH M.CODANAL,CODPART WITH M.CODPART, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,valdeb with m.impparc,valhab with m.impparc,TIPO WITH '2',DESCRI WITH 'REG -'+FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH maereg.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2),TOTCP WITH m.impparc
          endif
       ENDIF
       UNLOCK
       SELEC IteREg
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN    
               
    SET RELA TO


	SELE Maereg
    SET RELATION OFF  INTO Compag
    
    SELE MAEREG
    SET INDEX TO
    ERASE (wINDi)
    
    	
	SELE Hoja
	SET RELATION OFF INTO Maereg
	SET RELATION OFF INTO Compag ADI
	
  	USE IN 1   
  	USE IN 5   
	USE IN 2   
	USE IN 6
	*-------------------------------------------------------
    * DE COMPROBANTES DE PAGO
    * Cuando el Comprobante de Pago 
    * ose Compag.Reten=0
    *-----------------------
    USE HojCon   IN 1   ORDER TAG HojCon1  ALIAS Hoja
	USE ComPag   IN 5   ORDER TAG ComPag1  ALIAS ComPag
	USE IteCP    IN 6   ORDER TAG IteCP1   ALIAS IteCP
	
	SELE Hoja 
	SET RELATION TO NUMMEScp+NUMCP+CODCTC INTO IteCp
    SET RELATION TO NUMMEScp+NUMCP+CODCTC INTO COMPAG ADDITIVE
    
  	SELE IteCp
    SET RELATION TO NUMMES+NUMCP+CODCTC INTO COMPAG  additive

*   SELE IteCP
    IF !EMPTY(vcodpart)                &&Nummeshc
    
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2)) .AND. SUBSTR(COMPAG.CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  COMPAG.CodFTE=ALLTRIM(vCodFte) .AND. iif(alltrim(vTipFun)='F',ITECP.CODANAL=vcodpart,ITECP.CODPART=vcodpart) AND SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND !EMPTY(COMPAG.NUMCP) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO) ;
                  and Hoja.Estado="50" AND (!EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc))
    ELSE
	    SET FILTER TO between(val(COMPAG.Nummes),val(vNumMes1),val(vnuMmes2))  .AND. SUBSTR(CODCAL,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  Compag.CodFTE=ALLTRIM(vCodFte) .AND. SUBSTR(COMPAG.CodCal,10,3)=alltrim(vCodSubPr) .and. ;
                  COMPAG.estado#'99' AND iif(!EMPTY(vCodPry),SUBSTR(COMPAG.CODCAL,13,3)=ALLTRIM(vCodPry),.t.) .and. ;
                  iif(!EMPTY(vCodSpy),SUBSTR(COMPAG.CODCAL,16,2)=ALLTRIM(vCodSpy),.t.) AND IIF(!empty(vcodact),SUBSTR(COMPAG.CODCAL,13,2)=vcodact,.t.)  AND !EMPTY(COMPAG.NUMCP) AND COMPAG.TIPDOC$'HCRG' AND COMPAG.tipfun = alltrim(vtipfun) AND COMPAG.periodo=alltrim(vperiodO);
                  and Hoja.Estado="50" AND (!EMPTY(Compag.nummeshc) AND !EMPTY(Compag.Numhc))
    ENDIF 
    
*   SELE IteCP
    GO TOP
    SCAN
       SCATTER MEMVAR 
       SELECT AST
       IF F_APPD()
          if alltrim(vtipfun)='I'
          	repla fecref with compag.feccp,cODANAL WITH M.CODANAL,;
          	      CODPART WITH M.CODPART, NUMMES WITH Compag.NUMMES,;
          	      tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,;
          	      valdeb with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),valhab with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),TIPO WITH '2',;
          	      DESCRI WITH FTE(),tipfte with compag.docref,numfte with compag.numref,;
          	      TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),Codproy with substr(codcal,13,3),codsupry with substr(codcal,16,2),TOTCP WITH IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc)
          else
          	repla fecref with compag.feccp,cODANAL WITH M.CODANAL,CODPART WITH M.CODPART, NUMMES WITH Compag.NUMMES,tipdoc with 'C/P',numref with m.NumCp,codcal with m.codcal,TIPO WITH '2',DESCRI WITH FTE(),tipfte with compag.docref,numfte with compag.numref,TIPOPE WITH 1,mesfte WITH COMPAG.NUMMESHC,codprg with SUBSTR(codcal,8,2),codsubpr with substr(codcal,10,3),codact with substr(codcal,13,2)
          	REPLA valdeb with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),valhab with IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc),TOTCP WITH IIF(COMPAG.PARTRET=M.CODanal,m.impparc-COMPAG.RETEN,m.impparc)
          endif
       ENDIF
       UNLOCK
       SELE IteCP
       @ 1,14 SAY 'Espere un Momento ....' +STR(RECNO(),5) color W*  
    ENDSCAN    
               
    SET RELA TO
	SELE Hoja 
	SET RELATION OFF INTO IteCp
    SET RELATION OFF INTO COMPAG ADDI
    
  	SELE IteCp
    SET RELATION OFF INTO COMPAG ADDI
    

    USE IN 5
    USE IN 6
    USE IN 1
            
    SELE AST
    SCATTER MEMVAR
    GO TOP
    
    DEACTIVATE WINDOW STANDBY       
    IF EOF()
       DO STANDBY WITH 'No se tiene registros a Procesar'
    ELSE
       IF ALLTRIM(vTipfun)='I'
            INDEX ON CODSUBPR+CODPROY+CODSUPRY+codPART+NUMMES+TIPO+TIPDOC+NUMREF TO (VIND)
       		DO REPORTE WITH 2,"AUXCOMI",' Auxiliar de Compromisos de Partidas'
       ELSE
            INDEX ON CODSUBPR+CODACT+codANAL+NUMMES+TIPO+TIPDOC+NUMREF TO (VIND)           
       		*DO REPORTE WITH 2,"AUXCOMF",' Auxiliar de Compromisos de Partidas'
       		DO REPORTE WITH 2,"AUXF",' Auxiliar de Compromisos de Partidas'
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
   CASE TIPDOC='P/R'   
        DEVUL = 'PARTE DE REBAJA DE LA '+TIPFTE+' '+NUMFTE
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

FUNCTION xVAL_PRO
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
*  
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
if (operac='C' and tipfte='CYP' and tipope=-1) or (operac='C' and tipfte='C  ' and tipope=-1) or (operac='T' and tipfte='CYP' and tipope=-1) or (operac='T' and tipfte='C  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='C  ' and tipope=-1)  OR (operac='A' and tipfte='C  ')
   retur .t.
endif
retur .f.
   
PROCEDURE TIPHMAD
*----------------
PRIVATE vfun
* if (operac='C' and tipfte='CYP') or (operac='C' and tipfte='C  ') or (operac='T' and tipfte='CYP') or (operac='T' and tipfte='C  ' and tipope=-1) or (operac='T' and tipfte='P  ' and tipope#-1) or (operac='R' and tipfte='CYP' and tipope=-1) &&or (operac='R' and tipfte='P  ') 
if (operac='C' and tipfte='CYP') or (operac='C' and tipfte='C  ') or (operac='T' and tipfte='CYP') or (operac='T' and tipfte='C  ' and tipope=-1) or (operac='T' and tipfte='P  ' and tipope#-1) or (operac='R' and tipfte='CYP' and tipope=-1) OR (operac='A' and tipfte='C  ')
   retur .t.
endif
retur .f.
      
PROCEDURE TIPHMAH
*----------------

PRIVATE vfun
* if (operac='C' and tipfte='CYP') or (operac='C' and tipfte='P  ') or (operac='T' and tipfte='CYP') or (operac='T' and tipfte='C  ' and tipope#-1) or (operac='T' and tipfte='P  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='P  ' and tipope=-1) 
if (operac='C' and tipfte='CYP') or (operac='C' and tipfte='P  ') or (operac='T' and tipfte='CYP') or (operac='T' and tipfte='C  ' and tipope#-1) or (operac='T' and tipfte='P  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='P  ' and tipope=-1)  
   retur .t.
endif
retur .f.
      
PROCEDURE TIPHMED
*----------------
PRIVATE vfun
* if (operac='C' and tipfte='CYP' and tipope=-1) or (operac='C' and tipfte='P  ' and tipope=-1) or (operac='T' and tipfte='CYP' and tipope=-1) or (operac='T' and tipfte='P  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='P  ' and tipope=-1) 
if (operac='C' and tipfte='CYP' and tipope=-1) or (operac='C' and tipfte='P  ' and tipope=-1) or (operac='T' and tipfte='CYP' and tipope=-1) or (operac='T' and tipfte='P  ' and tipope=-1) or (operac='R' and tipfte='CYP' and tipope=-1) or (operac='R' and tipfte='P  ' and tipope=-1) 
   retur .t.
endif
retur .f.
      
PROCEDURE TIPHMEH
*----------------
PRIVATE vfun
*if (operac='C' and tipfte='CYP' and tipope#-1) or (operac='C' and tipfte='P  ' and tipope#-1) or (operac='T' and tipfte='CYP' and tipope#-1) or (operac='T' and tipfte='P  ' and tipope#-1)
if (operac='C' and tipfte='CYP' and tipope#-1) or (operac='C' and tipfte='P  ' and tipope#-1) or (operac='T' and tipfte='CYP' and tipope#-1) or (operac='T' and tipfte='P  ' and tipope#-1)  
   retur .t.
endif
retur .f.
*---  