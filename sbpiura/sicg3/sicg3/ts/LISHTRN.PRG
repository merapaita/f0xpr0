* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ LisHtrn.PRG       04/01/96                         L: 0486   ³	
* ³ Hojas de Trabajo Tesoro P£blico                              ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

PARAMETERS vctacte,xano,vmes
xmes=val(vmes)
*-Abriendo archivos
USE MovBco   IN 2 	ORDER 	 Movbco1   ALIAS Movb
USE ComPag   IN 3 	ORDER 	 ComPag1   ALIAS ComPag
USE IteCp    IN 4 	ORDER 	 IteCp1    ALIAS IteCp
USE Cheque   IN 9 	ORDER 	 Cheque1   ALIAS Cheque
USE Reten    IN 10 	ORDER 	 Reten1    ALIAS Reten

PUBLIC _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12,_tmto13,_tmto14,_tmto15,_tmto16
PUBLIC _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16

STORE 0 TO _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12,_tmto13,_tmto14,_tmto15,_tmto16
STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
Vmens08 = 'No hay registros para procesar'
 vsubsidio=.F.
  IF (ALLTRIM(vCtaCte)='631017290' OR ALLTRIM(vctacte)='631017312') AND yesno('Imprime Hoja de Trabajo de Subsidios [Si] u Otros [No]?')
     vsubsidio=.T.
  ENDIF   
  IF LASTKEY() # 27
	 DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	 ACTIVATE WINDOW Xwait
     vind1= SYS(3) +'.idx'
     vind2= SYS(3) +'.idx'
     vind3= SYS(3) +'.idx'
	 @0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
	filaux = SYS(3) + '.DBF'
	SELE 15
	CREATE TABLE (filaux);
	  (TipDoc    C(003),;
	   NumRef    C(004),;
	   Nummes    C(002),;
	   Numdoc    C(004),;
	   Periodo   C(002),;
	   CodCtc    C(014),;
	   Estado    C(002),;
	   Numchq    C(014),;
	   Nomgir    C(028),;
	   Valchq    N(18,2),;
	   Import    N(18,2),;
	   CodPart   C(005),;
	   Pres      C(001),;
	   Codcal    C(022))
	USE 
	USE (filaux) IN 15 EXCLUSIVE ALIAS Temp
	 SELE COMPAG
	 INDEX ON ALLTRIM(CODCAL)+NUMCP TO (VIND1) FOR CodCtC = vCtaCte .AND. ;
	 		Nummes=vmes AND PERIODO=STR(XANO,2) AND ;
	 		IIF(vsubsidio=.F.,!TIPDOC$'SURU',TIPDOC$'SURU') AND TipDOC#'IN'
	 SET RELA TO NUMMES+NUMCP+CODCTC INTO CHEQUE
	 SET SKIP TO cheque
     GO TOP
     if !eof()
        DO Tempcp
	 ELSE
        do standby with vmens08
        close data
        return
     ENDIF
     IF !Vsubsidio
		 SELE MOVB
		 INDEX ON ALLTRIM(CODCAL)+NUMERO TO (VIND2) FOR CodCtC = vCtaCte .AND. ;
	 			Nummes=vmes and periodo=str(xano,2) AND ESTADO<>'99' AND ;
		 		TIPDOC='N/C' AND (Motivo='15' OR motivo = '14')
		 GO TOP
		 IF !EOF()
		    DO proCnc	
			deactivate window xwait
			DO REPPRG  WITH "HOJTRA"," Hoja de Trabajo "
		 ELSE
			deactivate window xwait
			DO REPPRG  WITH "HOJTRA"," Hoja de Trabajo "
		 ENDIF	
	 ELSE	 
		 deactivate window xwait
		    DO REPPRG  WITH "HOJTRA"," Hoja de Trabajo "
	 ENDIF      
 	 IF Yesno('Desea visualizar los inutilizados?')
	 	DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
	 	ACTIVATE WINDOW Xwait
	 	@0,10 SAY " Espere un Momento...Reporte en Proceso!" COLOR W+/BR*
	 	SELECT Compag
	 	vind1 = SYS(3)+'.IDX'
	 	INDEX ON NUMCP TO (VIND1) FOR CodCtC = vCtaCte .AND. ;
	 		Nummes=vmes AND PERIODO=STR(XANO,2) AND TipDOC='IN'
	 	DEACTIVATE WINDOW Xwait
	 	IF !EOF()
	 		DO Reporte WITH 2,"LisInu","Listado de Comprobantes Inutilizados"
	 	ENDIF
	 ENDIF
	 
 ENDIF
 SELECT COMPAG  
 SET INDEX TO
RELEASE    WINDOW lisprg
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION Tempcp
*---------------
vTipDoc = "C/P"
vNUMREF = COMPAG.NUMHC
vNumMes = COMPAG.NUMMES
vNumDOC = COMPAG.NUMCP
vPERIODO = COMPAG.PERIODO
vCODCTC = COMPAG.CODCTC
vESTADO = COMPAG.ESTADO
vNUMCHQ = CHEQUE.NUMCHQ
VNOMGIR = IIF(Estado='99','A N U L A D O',CHEQUE.NOMGIR)
VVALCHQ = IIF(Compag.Estado='99' OR Compag.Pres='*',0,CHEQUE.VALCHQ)
VImport = IIF(Compag.Estado='99' OR Compag.Pres='*',0,COMPAG.Import)
VCODPAR = CHEQUE.CodPart
VCODCAL = COMPAG.CODCAL
Vpres   = Compag.Pres  
SCAN
	vTipDoc = "C/P"
	vNUMREF = COMPAG.NUMHC
	vNumMes = COMPAG.NUMMES
	vNumDOC = COMPAG.NUMCP
	vPERIODO =COMPAG.PERIODO
	vCODCTC = COMPAG.CODCTC
	vESTADO = COMPAG.ESTADO
	vNUMCHQ = CHEQUE.NUMCHQ
    Vpres   = Compag.Pres  
	VNOMGIR = IIF(Estado='99' OR pres='*','A N U L A D O',CHEQUE.NOMGIR)
	VVALCHQ = IIF(Compag.Estado='99' OR pres='*',0,CHEQUE.VALCHQ)
	VImport = IIF(Compag.Estado='99' OR pres='*',0,COMPAG.Import)
	VCODPAR = CHEQUE.CodPart
    VCODCAL = COMPAG.CODCAL
    SELECT Temp
    IF F_APPD()
            REPLACE TipDoc WITH vTipDoc        
            REPLACE NUMREF WITH vNUMREF        
            REPLACE NumMes WITH vNumMes        
            REPLACE NumDOC WITH vNumDOC        
            REPLACE PERIODO WITH vPERIODO     
            REPLACE CODCTC WITH vCODCTC        
            REPLACE ESTADO WITH vESTADO
            REPLACE NUMCHQ WITH vNUMCHQ        
            REPLACE NOMGIR WITH vNOMGIR        
            REPLACE VALCHQ WITH IIF(Estado='99' OR pres='*',0,vVALCHQ)
            REPLACE Import WITH IIF(Estado='99' OR pres='*',0,vImport)
            REPLACE CodPart WITH vCODPAR        
            REPLACE CODcal WITH vCODcal        
            REPLACE pres   WITH vpres
     ENDIF
     SELE COMPAG
ENDSCAN
RETURN


FUNCTION PROCnc
*---------------
vTipDoc = "N/C"
vNUMREF = MOVB.NUMHC
vNumMes = MOVB.NUMMES
vNumDOC = MOVB.NUMERO
vPERIODO = MOVB.PERIODO
vCODCTC = MOVB.CODCTC
vESTADO = MOVB.ESTADO
VNOMGIR  =MOVB.NOMBRE
VImport = MOVB.MONTO
VCODPAR = MOVB.CodPart
VCODCAL = movb.CODCAL
SCAN
vTipDoc = "N/C"
vNUMREF = MOVB.NUMHC
vNumMes = MOVB.NUMMES
vNumDOC = MOVB.NUMERO
vPERIODO = MOVB.PERIODO
vCODCTC = MOVB.CODCTC
vESTADO = MOVB.ESTADO
VNOMGIR = MOVB.NOMBRE
VImport = MOVB.MONTO
VCODPAR = MOVB.CodPart
VCODCAL = movb.CODCAL
    SELECT Temp
    IF F_APPD()
            REPLACE TipDoc WITH vTipDoc        
            REPLACE NumMes WITH vNumMes        
            REPLACE NumDOC WITH vNumDOC        
            REPLACE PERIODO WITH vPERIODO     
            REPLACE CODCTC WITH vCODCTC        
            REPLACE ESTADO WITH vESTADO
            REPLACE NOMGIR WITH vNOMGIR        
            REPLACE Import WITH vImport        
            REPLACE CodPart WITH vCODPAR        
            REPLACE CODcal WITH vCODcal        
            REPLACE NUMREF WITH vNUMREF        
     ENDIF
     SELE MOVB
ENDSCAN
RETURN


PROCEDURE HOJTRA	
*----------------
PARAMETERS XCOP
PRIVATE FILA,VCAL,VKEY,_qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
PRIVATE _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12
STORE 0 TO _qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
STORE 0 TO _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
SELE Temp
INDEX ON ALLTRIM(SUBSTR(CODCAL,10,18))+ALLTRIM(TIPDOC)+NUMDOC TO (VIND3) ;
		FOR CodCtC = vCtaCte .AND. Nummes=vmes AND PERIODO=STR(XANO,2)
@1,1 SAY CHR(15)
fila=8
DO imp_header  
VCAL=Temp.CODCAL
@FILA,02 SAY Val_SUBP(substr(VCAL,10,3),'CODSUB'+substr(VCAL,8,2)+'    ','D',22,60)
FILA=FILA+1
@FILA,2 SAY REPLICATE("- ",115)
FILA=FILA+1
SCAN
	IF SUBSTR(Temp.CODCAL,10,3)<>SUBSTR(VCAL,10,3)
	    @FILA,2 SAY REPLICATE("-",230)
	    FILA=FILA+1
	    @FILA,02 SAY "TOTAL SUBP.=>"+Val_SUBP(substr(VCAL,10,3),'CODSUB'+substr(VCAL,8,2)+'    ','D',22,20)
		@FILA,52 SAY _pmto1 PICTURE "@Z 999,999,999.99"
		@FILA,65 SAY _pmto2 PICTURE "@Z 999,999,999.99"
		@FILA,79 SAY _pmto3 PICTURE "@Z 999,999,999.99"
		@FILA,93 SAY _pmto4 PICTURE "@Z 999,999,999.99"
		@FILA,107 SAY _pmto5 PICTURE "@Z 999,999,999.99"
		@FILA,121 SAY _pmto7 PICTURE "@Z 999,999,999.99"
		@FILA,135 SAY _pmto8 PICTURE "@Z 999,999,999.99"
		@FILA,149 SAY _pmto11 PICTURE "@Z 999,999,999.99"
		@FILA,163 SAY _pmto12 PICTURE "@Z 999,999,999.99"
		@FILA,177 SAY _pmto13 PICTURE "@Z 999,999,999.99"
		@FILA,191 SAY _pmto14 PICTURE "@Z 999,999,999.99"
		@FILA,205 SAY _pmto15 PICTURE "@Z 999,999,999.99"
		@FILA,219 SAY _pmto16 PICTURE "@Z 999,999,999.99"
		STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
	    FILA=FILA+1
	    @FILA,2 SAY REPLICATE("Ä",230)
	    FILA=FILA+1
		IF FILA>=60
		   FILA=8
		   DO IMP_HEADER
		ENDIF   
		@FILA,02 SAY vAl_SUBP(substr(Temp.CODCAL,10,3),'CODSUB'+substr(Temp.CODCAL,8,2)+'    ','D',22,60)
	    FILA=FILA+1
		@FILA,2 SAY REPLICATE("- ",115)
		FILA=FILA+1
	ENDIF    
    @FILA,3 SAY Temp.TIPDOC
    @FILA,7 SAY Temp.NUMDOC
    @FILA,12 SAY Temp.NUMREF
    @FILA,17 SAY Temp.NUMCHQ
    @FILA,33 SAY LEFT(Temp.NOMGIR,15)
    IF ALLTRIM(Temp.TIPDOC)='C/P'
		VKEY=Temp.NUMMES+Temp.NUMDOC+ALLTRIM(Temp.CODCTC)
		SELE IteCp
		SEEK VKEY
		IF FOUND()
			SCAN WHILE IteCp.NUMMES+IteCp.NUMCP+ALLTRIM(IteCp.CODCTC)=VKEY
			IF Temp.pres#'*'
			DO CASE
			   CASE IteCp.TIPFUN="F"
				DO CASE
				   CASE LEFT(IteCp.CODANAL,2)='01'
		    	        _qmto1=_qmto1+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='02'
			            _qmto2=_qmto2+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='03'
		        	    _qmto3=_qmto3+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='04'
			            _qmto4=_qmto4+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='05'
			            _qmto5=_qmto5+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='07'
			            _qmto7=_qmto7+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='08'
		    	        _qmto8=_qmto8+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CODANAL,2)='09'
			            _qmto11=_qmto11+IIF(Estado='99',0,IteCp.ImpParc)
			       OTHERWISE    
    			        _qmto12=_qmto12+IIF(Estado='99',0,IteCp.ImpParc)
			       ENDCASE     
			       
			   CASE IteCp.TIPFUN="I"
				DO CASE
				   CASE LEFT(IteCp.CodPart,2)='01'
	    		        _qmto1=_qmto1+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='02'
	    	    	    _qmto2=_qmto2+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='03'
		            _qmto3=_qmto3+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='04'
		        	    _qmto4=_qmto4+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='05'
		    	        _qmto5=_qmto5+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='07'
		        	    _qmto7=_qmto7+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='08'
		    	        _qmto8=_qmto8+IIF(Estado='99',0,IteCp.ImpParc)
				   CASE LEFT(IteCp.CodPart,2)='09'
		        	    _qmto11=_qmto11+IIF(Estado='99',0,IteCp.ImpParc)
			       OTHERWISE
    			        _qmto12=_qmto12+IIF(Estado='99',0,IteCp.ImpParc)
		    	   ENDCASE     
			 ENDCASE
			 endif
		ENDSCAN
		ENDIF   	
	SELE Temp
	_tmto1=_tmto1+_qmto1
	_tmto2=_tmto2+_qmto2
	_tmto3=_tmto3+_qmto3
	_tmto4=_tmto4+_qmto4
	_tmto5=_tmto5+_qmto5
	_tmto7=_tmto7+_qmto7
	_tmto8=_tmto8+_qmto8
	_tmto11=_tmto11+_qmto11
	_tmto12=_tmto12+_qmto12
	_pmto1=_pmto1+_qmto1
	_pmto2=_pmto2+_qmto2
	_pmto3=_pmto3+_qmto3
	_pmto4=_pmto4+_qmto4
	_pmto5=_pmto5+_qmto5
	_pmto7=_pmto7+_qmto7
	_pmto8=_pmto8+_qmto8
	_pmto11=_pmto11+_qmto11
	_pmto12=_pmto12+_qmto12
	@FILA,51 SAY _qmto1 PICTURE "@Z 999,999,999.99"
	@FILA,65 SAY _qmto2 PICTURE "@Z 999,999,999.99"
	@FILA,79 SAY _qmto3 PICTURE "@Z 999,999,999.99"
	@FILA,93 SAY _qmto4 PICTURE "@Z 999,999,999.99"
	@FILA,107 SAY _qmto5 PICTURE "@Z 999,999,999.99"
	@FILA,121 SAY _qmto7 PICTURE "@Z 999,999,999.99"
	@FILA,135 SAY _qmto8 PICTURE "@Z 999,999,999.99"
	@FILA,149 SAY _qmto11 PICTURE "@Z 999,999,999.99"
	@FILA,163 SAY _qmto12 PICTURE "@Z 999,999,999.99"
	@FILA,177 SAY Temp.VALCHQ PICTURE "@Z 999,999,999.99"
	@FILA,191 SAY IIF(Temp.CodPart<>'16.01',Temp.Import,0) PICTURE "@Z 999,999,999.99"
	@FILA,205 SAY IIF(Temp.CodPart="16.01",Temp.Import,0) PICTURE "@Z 999,999,999.99"
	@FILA,219 SAY IIF(Temp.Import-Temp.VALCHQ>0,Temp.Import-Temp.VALCHQ,0) PICTURE "@Z 999,999,999.99"
	_tmto13=_tmto13+Temp.VALCHQ
	_tmto14=_tmto14+IIF(Temp.CodPart<>'16.01',Temp.Import,0)
	_tmto15=_tmto15+IIF(Temp.CodPart="16.01",Temp.Import,0) 
	_tmto16=_tmto16+IIF(Temp.Import-Temp.VALCHQ>0,Temp.Import-Temp.VALCHQ,0) 
	_pmto13=_pmto13+Temp.VALCHQ
	_pmto14=_pmto14+IIF(Temp.CodPart<>'16.01',Temp.Import,0)
	_pmto15=_pmto15+IIF(Temp.CodPart="16.01",Temp.Import,0) 
	_pmto16=_pmto16+IIF(Temp.Import-Temp.VALCHQ>0,Temp.Import-Temp.VALCHQ,0) 
ELSE &&Mov.Bco		
    DO CASE
	   CASE LEFT(Temp.CodPart,2)='01'
	        _qmto1=_qmto1+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='02'
	        _qmto2=_qmto2+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='03'
	   	    _qmto3=_qmto3+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='04'
	        _qmto4=_qmto4+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='05'
	        _qmto5=_qmto5+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='07'
	        _qmto7=_qmto7+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='08'
	        _qmto8=_qmto8+Temp.Import
	   CASE LEFT(Temp.CodPart,2)='09'
	        _qmto11=_qmto11+Temp.Import
	   OTHERWISE    
            _qmto12=_qmto12+Temp.Import
	 ENDCASE     
	_tmto1=_tmto1+_qmto1
	_tmto2=_tmto2+_qmto2
	_tmto3=_tmto3+_qmto3
	_tmto4=_tmto4+_qmto4
	_tmto5=_tmto5+_qmto5
	_tmto7=_tmto7+_qmto7
	_tmto8=_tmto8+_qmto8
	_tmto11=_tmto11+_qmto11
	_tmto12=_tmto12+_qmto12
	_pmto1=_pmto1+_qmto1
	_pmto2=_pmto2+_qmto2
	_pmto3=_pmto3+_qmto3
	_pmto4=_pmto4+_qmto4
	_pmto5=_pmto5+_qmto5
	_pmto7=_pmto7+_qmto7
	_pmto8=_pmto8+_qmto8
	_pmto11=_pmto11+_qmto11
	_pmto12=_pmto12+_qmto12
	@FILA,51 SAY _qmto1 PICTURE "@Z 999,999,999.99"
	@FILA,65 SAY _qmto2 PICTURE "@Z 999,999,999.99"
	@FILA,79 SAY _qmto3 PICTURE "@Z 999,999,999.99"
	@FILA,93 SAY _qmto4 PICTURE "@Z 999,999,999.99"
	@FILA,107 SAY _qmto5 PICTURE "@Z 999,999,999.99"
	@FILA,121 SAY _qmto7 PICTURE "@Z 999,999,999.99"
	@FILA,135 SAY _qmto8 PICTURE "@Z 999,999,999.99"
	@FILA,149 SAY _qmto11 PICTURE "@Z 999,999,999.99"
	@FILA,163 SAY _qmto12 PICTURE "@Z 999,999,999.99"
	@FILA,177 SAY Temp.Import PICTURE "@Z 999,999,999.99"
	@FILA,191 SAY Temp.Import PICTURE "@Z 999,999,999.99"
	_tmto13=_tmto13+Temp.Import
	_tmto14=_tmto14+Temp.Import
	_pmto13=_pmto13+Temp.Import
	_pmto14=_pmto14+Temp.Import 
	
	ENDIF
	FILA=FILA+1
	IF FILA>=60
	   FILA=8
	   DO IMP_HEADER
	ENDIF   
	STORE 0 TO _qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
	VCAL=Temp.CODCAL
ENDSCAN
@FILA,2 SAY REPLICATE("Ä",230)
FILA=FILA+1
@FILA,02 SAY "TOTAL SUBP.=>"+Val_SUBP(substr(VCAL,10,3),'CODSUB'+substr(VCAL,8,2)+'    ','D',22,20)
@FILA,51 SAY _pmto1 PICTURE "@Z 999,999,999.99"
@FILA,65 SAY _pmto2 PICTURE "@Z 999,999,999.99"
@FILA,79 SAY _pmto3 PICTURE "@Z 999,999,999.99"
@FILA,93 SAY _pmto4 PICTURE "@Z 999,999,999.99"
@FILA,107 SAY _pmto5 PICTURE "@Z 999,999,999.99"
@FILA,121 SAY _pmto7 PICTURE "@Z 999,999,999.99"
@FILA,135 SAY _pmto8 PICTURE "@Z 999,999,999.99"
@FILA,149 SAY _pmto11 PICTURE "@Z 999,999,999.99"
@FILA,163 SAY _pmto12 PICTURE "@Z 999,999,999.99"
@FILA,177 SAY _pmto13 PICTURE "@Z 999,999,999.99"
@FILA,191 SAY _pmto14 PICTURE "@Z 999,999,999.99"
@FILA,205 SAY _pmto15 PICTURE "@Z 999,999,999.99"
@FILA,219 SAY _pmto16 PICTURE "@Z 999,999,999.99"
FILA=FILA+1
@FILA,2 SAY REPLICATE("Ä",230)
FILA=FILA+1
@FILA,02 SAY "TOTAL GENERAL =>"
@FILA,51 SAY _tmto1 PICTURE "@Z 999,999,999.99"
@FILA,65 SAY _tmto2 PICTURE "@Z 999,999,999.99"
@FILA,79 SAY _tmto3 PICTURE "@Z 999,999,999.99"
@FILA,93 SAY _tmto4 PICTURE "@Z 999,999,999.99"
@FILA,107 SAY _tmto5 PICTURE "@Z 999,999,999.99"
@FILA,121 SAY _tmto7 PICTURE "@Z 999,999,999.99"
@FILA,135 SAY _tmto8 PICTURE "@Z 999,999,999.99"
@FILA,149 SAY _tmto11 PICTURE "@Z 999,999,999.99"
@FILA,163 SAY _tmto12 PICTURE "@Z 999,999,999.99"
@FILA,177 SAY _tmto13 PICTURE "@Z 999,999,999.99"
@FILA,191 SAY _tmto14 PICTURE "@Z 999,999,999.99"
@FILA,205 SAY _tmto15 PICTURE "@Z 999,999,999.99"
@FILA,219 SAY _tmto16 PICTURE "@Z 999,999,999.99"
FILA=FILA+1
@FILA,2 SAY REPLICATE("Ä",230)
SET DEVICE TO SCREEN 
RETURN


PROCEDURE imp_header
*----------------
@1,3   SAY ALLTRIM(CIA)
@1,225 say "PAG:"
@1,230 SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisHojTR"
@2,95  say 'REPORTE HOJA DE TRABAJO '
@2,120 SAY MFecha(xmes, xano)
@2,215 SAY "FECHA:"
@2,225 SAY DATE()           
@3,75  say "CUENTA CORRIENTE : "+ALLTRIM(Val_Fun('Caja','CodCtc',"ALLTRIM(Codctc)+' '+Descri",CodCtc))
@4,2 SAY REPLICATE("Ä",230)
@5,3 SAY "DOC NRO H/C     CHEQUE          RAZON SOCIAL           01.00         02.00         03.00        04.00        05.00         07.00         08.00          09.00            OTROS      LIQUIDO     BRUTO                RETENCIONES"
@6,3 SAY "                                                                                                                                                                                    GIRADO    COMPROMETIDO       DEBE          HABER"
@7,2 SAY REPLICATE("Ä",230) 
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)