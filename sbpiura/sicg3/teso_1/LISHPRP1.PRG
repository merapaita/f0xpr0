* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ LisHprp.PRG       04/01/96                         L: 0466   ³	
* ³ Hojas de Trabajo                                             ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.   06/1997   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*-Abriendo archivos
PARAMETERS  vctacte,xmes,xano,sede
USE MovBco   IN  2  ORDER  Movbco1   ALIAS Movb
USE ComPag   IN  3  ORDER  ComPag1   ALIAS ComPag
USE IteCp    IN  4  ORDER  IteCp1    ALIAS IteCp
USE Cheque   IN  9  ORDER  Cheque1   ALIAS Cheque
USE Reten    IN 10  ORDER  Reten1    ALIAS Reten

PUBLIC _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12,_tmto13,_tmto14,_tmto15,_tmto16
PUBLIC _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16

STORE 0 TO _tmto1,_tmto2,_tmto3,_tmto4,_tmto5,_tmto7,_tmto8,_tmto11,_tmto12,_tmto13,_tmto14,_tmto15,_tmto16
STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
Vmens08 = 'No hay registros para procesar'

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
	   Pres      C(001),;
	   Numchq    C(014),;
	   Nomgir    C(028),;
	   Valchq    N(18,2),;
	   Import    N(18,2),;
	   Codpart   C(006),;
	   ImpParc   N(18,2),;
	   Fecha     D(008),;
   	   Reten     N(18,2),;
	   Codcad    C(022))
	   
	USE 
	
   filau1 = SYS(3) + '.DBF'
   SELE 16
   CREATE TABLE (filau1);
	  (Numdoc    C(004),;
	   Tipdoc    C(003),;
	   NumHc     C(004),;
	   Numchq    C(014),;
	   Nomgir    C(028),;
	   MonP1     N(12,2),;	 
	   MonP2     N(12,2),;	 
	   MonP3     N(12,2),;	 
	   MonP4     N(12,2),;	 
	   MonP5     N(12,2),;	 
	   MonP6     N(12,2),;	 
	   MonP7     N(12,2),;	 
	   MonP8     N(12,2),;	 
	   MonP9     N(12,2),;	 
	   Ret       C(001),;
	   Monliq    N(18,2),;	 
	   Monbru    N(18,2),;	 
	   Monret    N(18,2))

	USE 
	
	USE (filaux) IN 15 EXCLUSIVE ALIAS Temp
	USE (filau1) IN 16 EXCLUSIVE ALIAS TempRep
    SELE ComPag
    INDEX ON NumCP TO (vind1) FOR CodCtC=vctacte .AND. NumMes=xmes AND Periodo=xano
	SET RELA TO NumMes+NumCP+CodCtc INTO Cheque
	SET SKIP TO Cheque
	SET RELA TO NumMes+NumCP+CodCtc INTO IteCp ADDITIVE
	SET SKIP TO IteCp
    GO TOP
    IF !EOF()
        DO Tempcp
	ELSE
        DO standby WITH vmens08
        CLOSE DATA
        RETURN
    ENDIF

DO temprep

ENDIF
SELE ComPag  
SET INDEX TO
RELEASE  WINDOW lisprg
ACTIVATE SCREEN
CLOSE DATABASE
RETURN


FUNCTION Tempcp
*---------------
vTipDoc  = 	Compag.Tipdoc
vNumRef  = 	ComPag.NumHC
vNumMes  = 	ComPag.NumMes
vNumdoc  = 	ComPag.NumCP
vPeriodo = 	ComPag.Periodo
vCodCtc  = 	ComPag.CodCtc
vestado  = 	ComPag.Estado
vNumChq  = 	Cheque.NumChq
VNomGir  =	Cheque.NomGir
VValchq  = 	Cheque.Valchq
VImport  = 	ComPag.Import
Vcodpar  = 	 IteCP.CodPart
vimpparc =   IteCP.ImpParc
Vcodcad  = 	ComPag.codcad
vfecha   = 	ComPag.FecCP
vpres    =  Compag.Pres
vreten   =  Compag.Reten
SCAN
	vTipDoc	 =  Compag.Tipdoc
	vNumRef	 = 	ComPag.NumHC
	vNumMes	 = 	ComPag.NumMes
	vNumDOC	 = 	ComPag.NumCP
	vPeriodo =	ComPag.Periodo
	vCodCtc  = 	ComPag.CodCtc
	vestado  = 	ComPag.Estado
	vNumChq  = 	Cheque.NumChq
	VNomGir  =	Cheque.NomGir
	VValchq  = 	Cheque.Valchq
	VImport  = 	ComPag.Import
	Vcodpar  = 	 IteCP.CodPart
	vimpparc =   IteCP.ImpParc
    Vcodcad  = 	ComPag.codcad
    vfecha   =	ComPag.FecCP
    vpres    =  Compag.Pres
    vreten   =  Compag.Reten
    SELECT Temp
    IF F_appd()
            REPLACE TipDoc 	WITH 	vTipDoc        
            REPLACE NumRef 	WITH 	vNumRef        
            REPLACE NumMes 	WITH 	vNumMes        
            REPLACE NumDoc 	WITH 	vNumDoc        
            REPLACE Periodo WITH 	vPeriodo     
            REPLACE CodCtc	WITH 	vCodCtc        
            REPLACE estado 	WITH 	vestado
            REPLACE NumChq 	WITH 	vNumChq        
            REPLACE NomGir 	WITH	vNomGir        
            REPLACE VALCHQ WITH IIF(Estado='99' OR pres='*',0,vVALCHQ)
            REPLACE Import WITH IIF(Estado='99' OR pres='*',0,vImport)
            REPLACE CodPart WITH 	vCodpar        
            REPLACE ImpPARC	WITH 	vImpPARC       
            REPLACE codcad 	WITH 	vcodcad        
            REPLACE fecha  	WITH 	vfecha
            REPLACE pres   WITH vpres
            REPLACE reten  WITH vreten
     ENDIF
     SELE ComPag
ENDSCAN
RETURN


PROCEDURE temprep
*-----------------
SELECT temp
GO TOP
IF eof()
    DO standby WITH vmens08
    CLOSE DATA
    RETURN
ELSE
	SCAN WHILE !eof()
	  IF !EMPTY(codpart)
		xxdoc=numdoc
		xxref=numref
		SELE temprep
		APPEND BLANK
		REPLACE temprep.tipdoc WITH temp.tipdoc
		REPLACE temprep.numdoc WITH temp.numdoc
		REPLACE temprep.numhc  WITH temp.numref
		REPLACE temprep.numchq WITH temp.numchq
		REPLACE temprep.nomgir WITH IIF(temp.Estado='99','A N U L A D O',temp.nomgir)
		REPLACE temprep.monliq WITH IIF(temp.Estado='99',0,temp.valchq)
		REPLACE temprep.monbru WITH IIF(temp.Estado='99',0,temp.import)
		REPLACE temprep.monret WITH IIF(temp.Estado='99',0,temp.reten)
		IF temp.codpart='0000RE' .AND. TEMP.TIPDOC<>'ME'
			REPLACE temprep.ret    WITH 'S' 
		ENDIF
		IF temp.codpart='0000SU'
			REPLACE temprep.ret    WITH 'U'
		ENDIF
		STORE 0 TO p1,p2,p3,p4,p5,p6,p7,p8,p9
		SELE temp
		DO WHILE numdoc=xxdoc .AND. numref=xxref
			IF codpart<>'0000RE' .AND. codpart<>'0000SU' .AND. estado<>'99'
				DO CASE
				   CASE SUBSTR(codpart,2,1)='1'
				   		p1=p1+impparc
				   CASE SUBSTR(codpart,2,1)='2'
				   		p2=p2+impparc
				   CASE SUBSTR(codpart,2,1)='3'
				   		p3=p3+impparc
				   CASE SUBSTR(codpart,2,1)='4'
				   		p4=p4+impparc
				   CASE SUBSTR(codpart,2,1)='5'
				   		p5=p5+impparc
				   CASE SUBSTR(codpart,2,1)='6'
				   		p6=p6+impparc
				   CASE SUBSTR(codpart,2,1)='7'
				   		p7=p7+impparc
				   CASE SUBSTR(codpart,2,1)='8'
				   		p8=p8+impparc
				   CASE SUBSTR(codpart,2,1)='9'
				   		p9=p9+impparc
				ENDCASE
			ENDIF
			IF  !eof()
				SKIP
			ENDIF
		ENDDO
		SKIP -1
		REPLACE temprep.monp1 WITH p1
		REPLACE temprep.monp2 WITH p2
		REPLACE temprep.monp3 WITH p3
		REPLACE temprep.monp4 WITH p4
		REPLACE temprep.monp5 WITH p5
		REPLACE temprep.monp6 WITH p6
		REPLACE temprep.monp7 WITH p7
		REPLACE temprep.monp8 WITH p8
		REPLACE temprep.monp9 WITH p9
		SELE temp
	  ENDIF
	ENDSCAN
ENDIF

IF sede=1
	SELE temprep
	INDEX ON numdoc TO (vind2) FOR LEFT(numdoc,1)$'0123456789' .AND. ret<>'U' .AND. !(ALLTRIM(tipdoc)$'RUSUSSRS')
	GO TOP
    DEACTIVATE WINDOW Xwait	
	IF !eof()
	
		DO Reporte WITH 2,"HOJTRA",' HOJA DE TRABAJO ',1,.F.,.T.
	ENDIF

	INDEX ON numdoc TO (vind3) FOR LEFT(numdoc,1)$'0123456789' .AND. (ret='U' .OR. ALLTRIM(tipdoc)$'RUSUSSRS')

	GO TOP
    DEACTIVATE WINDOW Xwait	
	IF !eof()
		DO Reporte WITH 2,"HOJTRA",' HOJA DE TRABAJO DE SUBSIDIOS',1,.F.,.T.
	ENDIF
ELSE
	SELE temprep
	INDEX ON LEFT(numdoc,1)+numdoc TO (vind2) FOR !(LEFT(numdoc,1)$'0123456789')  .AND. ret<>'U' .AND. !(ALLTRIM(TIPDOC)='SS') .AND. !(ALLTRIM(TIPDOC)='RS')	
**	INDEX ON LEFT(numdoc,1)+numdoc TO (vind2) FOR !(LEFT(numdoc,1)$'0123456789')  .AND. ret<>'U'
	GO TOP
    DEACTIVATE WINDOW Xwait	
	IF !eof()
		DO Reporte WITH 2,"HOJTRA1",' HOJA DE TRABAJO ',1,.F.,.T.
	ENDIF
	
*	INDEX ON LEFT(numdoc,1)+numdoc TO (vind2) FOR !(LEFT(numdoc,1)$'0123456789')  .AND. ret='U' 
	INDEX ON LEFT(numdoc,1)+numdoc TO (vind2) FOR !(LEFT(numdoc,1)$'0123456789')  .AND. (ret='U' .OR. ALLTRIM(TIPDOC)='SS' .OR. ALLTRIM(TIPDOC)='RS')
	GO TOP
	IF !eof()
		DO Reporte WITH 2,"HOJTRA1",' HOJA DE TRABAJO ',1,.F.,.T.
	ENDIF
ENDIF

RETURN


FUNCTION Procnc
*---------------
vTipDoc  = 	"N/C"
vNumMes  = 	MovB.NumMes
vNumDoc  = 	MovB.Numero
vPeriodo = 	MovB.Periodo
vCodCtc  = 	MovB.CodCtc
vestado  = 	MovB.estado
VNomGir  =	MovB.Nombre
VImport  = 	MovB.Monto
VCodPar  = 	MovB.CodPart
Vcodcal  = 	MovB.codcal
vfecha   = 	MovB.fecha
vNumRef  = 	MovB.NumHC
SCAN
	vTipDoc  = "N/C"
	vNumMes  = MovB.NumMes
	vNumDoc  = MovB.Numero
	vPeriodo = MovB.Periodo
	vCodCtc  = MovB.CodCtc
	vestado  = MovB.estado
	VNomGir  = MovB.Nombre 
	VImport  = MovB.Monto
	VCodPar  = MovB.CodPart
	Vcodcal  = MovB.codcal
	vfecha   = MovB.fecha
    vNumRef  = 	MovB.NumHC
	SELECT Temp
    IF F_appd()
            REPLACE TipDoc  WITH 	vTipDoc        
            REPLACE NumMes  WITH 	vNumMes        
            REPLACE NumDoc  WITH 	vNumDoc        
            REPLACE Periodo WITH 	vPeriodo     
            REPLACE CodCtc  WITH 	vCodCtc        
            REPLACE estado  WITH 	vestado
            REPLACE NomGir  WITH 	vNomGir        
            REPLACE Import  WITH 	vImport        
            REPLACE CodPart WITH 	vCodPar        
            REPLACE codcal  WITH 	vcodcal        
            REPLACE fecha   WITH 	vfecha
            REPLACE NumRef 	WITH 	vNumRef        
    ENDIF
	SELE MovB
ENDSCAN
RETURN


PROCEDURE HOJTRA	
*----------------
PARAMETERS xcop
PRIVATE Fila,Vcal,Vkey,_qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
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
INDEX ON ALLTRIM(SUBSTR(codcal,10,18))+ALLTRIM(TipDoc)+NumDoc TO (vind3) FOR CodCtc = vctacte .AND. BETWEEN(Fecha,vFecIni,vFecFin) AND estado<>'99'
@1,1 SAY CHR(15)
Fila=8
DO imp_header  
Vcal = Temp.codcal
@Fila,02 SAY Val_SUBP(substr(Vcal,10,3),'CODSUB'+substr(Vcal,8,2)+'    ','D',22,60)
Fila = Fila+1
@Fila,2 SAY REPLICATE("- ",115)
Fila = Fila+1
SCAN
	IF SUBSTR(Temp.codcal,10,3)<>SUBSTR(Vcal,10,3)
	    @Fila,2 SAY REPLICATE("-",230)
	    Fila=Fila+1
	    @Fila,02  SAY "TOTAL SUBP.=>"+Val_SUBP(substr(Vcal,10,3),'CODSUB'+substr(Vcal,8,2)+'    ','D',22,20)
		@Fila,52  SAY _pmto1  PICTURE "@Z 999,999,999.99"
		@Fila,65  SAY _pmto2  PICTURE "@Z 999,999,999.99"
		@Fila,79  SAY _pmto3  PICTURE "@Z 999,999,999.99"
		@Fila,93  SAY _pmto4  PICTURE "@Z 999,999,999.99"
		@Fila,107 SAY _pmto5  PICTURE "@Z 999,999,999.99"
		@Fila,121 SAY _pmto7  PICTURE "@Z 999,999,999.99"
		@Fila,135 SAY _pmto8  PICTURE "@Z 999,999,999.99"
		@Fila,149 SAY _pmto11 PICTURE "@Z 999,999,999.99"
		@Fila,163 SAY _pmto12 PICTURE "@Z 999,999,999.99"
		@Fila,177 SAY _pmto13 PICTURE "@Z 999,999,999.99"
		@Fila,191 SAY _pmto14 PICTURE "@Z 999,999,999.99"
		@Fila,205 SAY _pmto15 PICTURE "@Z 999,999,999.99"
		@Fila,219 SAY _pmto16 PICTURE "@Z 999,999,999.99"
		STORE 0 TO _pmto1,_pmto2,_pmto3,_pmto4,_pmto5,_pmto7,_pmto8,_pmto11,_pmto12,_pmto13,_pmto14,_pmto15,_pmto16
	    Fila = Fila+1
	    @Fila,2 SAY REPLICATE("Ä",230)
	    Fila = Fila+1
		IF Fila >= 60
		   Fila = 8
		   DO Imp_Header
		ENDIF   
		@Fila,02 SAY vAl_SUBP(substr(Temp.codcal,10,3),'CODSUB'+substr(Temp.codcal,8,2)+'    ','D',22,60)
	    Fila = Fila+1
		@Fila,2 SAY REPLICATE("- ",115)
		Fila= Fila+1
	ENDIF    
    @Fila,3  SAY  Temp.TipDoc
    @Fila,7  SAY  Temp.NumDoc
    @Fila,12 SAY  Temp.NumRef
    @Fila,17 SAY  Temp.NumChq
    @Fila,33 SAY  LEFT(Temp.NomGir,15)
    IF ALLTRIM(Temp.TipDoc) = 'C/P'
		Vkey = Temp.NumMes+Temp.NumDoc+ALLTRIM(Temp.CodCtc)
		SELE IteCp
		SEEK Vkey
		IF FOUND() AND temp.pres#'*'
			SCAN WHILE IteCp.NumMes+IteCp.NumCP+ALLTRIM(IteCp.CodCtc) = Vkey
				DO CASE

				   CASE IteCp.TipFun = "F"
					DO CASE
					   CASE LEFT(IteCp.CodAnal,2) = '01'
			    	        _qmto1 = _qmto1 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '02'
				            _qmto2 = _qmto2 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '03'
			        	    _qmto3 = _qmto3 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '04'
				            _qmto4 = _qmto4 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '05'
				            _qmto5 = _qmto5 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '07'
				            _qmto7 = _qmto7 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '08'
			    	        _qmto8 = _qmto8 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodAnal,2) = '11'
				            _qmto11 = _qmto11 + IteCp.ImpParc
				       OTHERWISE    
    				        _qmto12 = _qmto12 + IteCp.ImpParc
			       ENDCASE     
			       
				   CASE IteCp.TipFun = "I"
					DO CASE
					   CASE LEFT(IteCp.CodPart,2) = '01'
		    		        _qmto1 = _qmto1 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '02'
	    		    	    _qmto2 = _qmto2 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '03'
				            _qmto3 = _qmto3 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '04'
			        	    _qmto4 = _qmto4 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '05'
			    	        _qmto5 = _qmto5 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '07'
			        	    _qmto7 = _qmto7 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '08'
			    	        _qmto8 = _qmto8 + IteCp.ImpParc
					   CASE LEFT(IteCp.CodPart,2) = '11'
			        	    _qmto11 = _qmto11 + IteCp.ImpParc
				       OTHERWISE
    				        _qmto12 = _qmto12 + IteCp.ImpParc
			    	   ENDCASE     
				 ENDCASE
			ENDSCAN
		ENDIF   	
		SELE Temp
		_tmto1  =	_tmto1 + _qmto1
		_tmto2  =	_tmto2 + _qmto2
		_tmto3  =	_tmto3 + _qmto3
		_tmto4  =	_tmto4 + _qmto4
		_tmto5  =	_tmto5 + _qmto5
		_tmto7  =	_tmto7 + _qmto7
		_tmto8  =	_tmto8 + _qmto8
		_tmto11 =	_tmto11 + _qmto11
		_tmto12 =	_tmto12 + _qmto12
		_pmto1  =	_pmto1 + _qmto1
		_pmto2  =	_pmto2 + _qmto2
		_pmto3  =	_pmto3 + _qmto3
		_pmto4  =	_pmto4 + _qmto4
		_pmto5  =	_pmto5 + _qmto5
		_pmto7  =	_pmto7 + _qmto7
		_pmto8  =	_pmto8 + _qmto8
		_pmto11 =	_pmto11 + _qmto11
		_pmto12 =	_pmto12 + _qmto12
		@Fila,51  SAY _qmto1 		PICTURE 	"@Z 999,999,999.99"
		@Fila,65  SAY _qmto2 		PICTURE 	"@Z 999,999,999.99"
		@Fila,79  SAY _qmto3 		PICTURE 	"@Z 999,999,999.99"
		@Fila,93  SAY _qmto4 		PICTURE 	"@Z 999,999,999.99"
		@Fila,107 SAY _qmto5 		PICTURE 	"@Z 999,999,999.99"
		@Fila,121 SAY _qmto7 		PICTURE 	"@Z 999,999,999.99"
		@Fila,135 SAY _qmto8 		PICTURE 	"@Z 999,999,999.99"
		@Fila,149 SAY _qmto11 		PICTURE 	"@Z 999,999,999.99"
		@Fila,163 SAY _qmto12 		PICTURE 	"@Z 999,999,999.99"
		@Fila,177 SAY Temp.Valchq 	PICTURE 	"@Z 999,999,999.99"
		@Fila,191 SAY IIF(Temp.CodPart <> '16.01',Temp.Import,0) 					    PICTURE "@Z 999,999,999.99"
		@Fila,205 SAY IIF(Temp.CodPart = "16.01",Temp.Import,0)  						PICTURE "@Z 999,999,999.99"
		@Fila,219 SAY IIF(Temp.Import - Temp.Valchq > 0,Temp.Import - Temp.Valchq,0)  	PICTURE "@Z 999,999,999.99"
		_tmto13	=	_tmto13 + Temp.Valchq
		_tmto14	=	_tmto14 + IIF(Temp.CodPart <> '16.01',Temp.Import,0)
		_tmto15	=	_tmto15 + IIF(Temp.CodPart = "16.01",Temp.Import,0) 
		_tmto16	=	_tmto16 + IIF(Temp.Import - Temp.Valchq > 0,Temp.Import - Temp.Valchq,0) 
		_pmto13	=	_pmto13 + Temp.Valchq
		_pmto14	=	_pmto14 + IIF(Temp.CodPart <> '16.01',Temp.Import,0)
		_pmto15	=	_pmto15 + IIF(Temp.CodPart = "16.01",Temp.Import,0) 
		_pmto16	=	_pmto16 + IIF(Temp.Import - Temp.Valchq > 0,Temp.Import - Temp.Valchq,0) 
    ELSE &&Mov.Bco		
	    DO CASE
		   CASE LEFT(Temp.CodPart,2) = '01'
		        _qmto1 = _qmto1 + Temp.Import
		   CASE LEFT(Temp.CodPart,2)='02'
		        _qmto2 = _qmto2 + Temp.Import
		   CASE LEFT(Temp.CodPart,2) = '03'
		   	    _qmto3 = _qmto3 + Temp.Import
		   CASE LEFT(Temp.CodPart,2) = '04'
		        _qmto4 = _qmto4 + Temp.Import
		   CASE LEFT(Temp.CodPart,2) = '05'
		        _qmto5 = _qmto5 + Temp.Import
		   CASE LEFT(Temp.CodPart,2) = '07'
	    	    _qmto7 = _qmto7 + Temp.Import
		   CASE LEFT(Temp.CodPart,2) = '08'
		        _qmto8 = _qmto8 + Temp.Import
		   CASE LEFT(Temp.CodPart,2) = '11'
		        _qmto11 = _qmto11 + Temp.Import
		   OTHERWISE    
	            _qmto12 = _qmto12 + Temp.Import
		 ENDCASE     
		_tmto1  =  _tmto1  + _qmto1
		_tmto2  =  _tmto2  + _qmto2
		_tmto3  =  _tmto3  + _qmto3
		_tmto4  =  _tmto4  + _qmto4
		_tmto5  =  _tmto5  + _qmto5
		_tmto7  =  _tmto7  + _qmto7
		_tmto8  =  _tmto8  + _qmto8
		_tmto11 = _tmto11  + _qmto11
		_tmto12 = _tmto12  + _qmto12
		_pmto1  =  _pmto1  + _qmto1
		_pmto2  =  _pmto2  + _qmto2
		_pmto3  =  _pmto3  + _qmto3
		_pmto4  =  _pmto4  + _qmto4
		_pmto5  =  _pmto5  + _qmto5
		_pmto7  =  _pmto7  + _qmto7
		_pmto8  =  _pmto8  + _qmto8
		_pmto11 = _pmto11  + _qmto11
		_pmto12 = _pmto12  + _qmto12
	
		@Fila,51 SAY _qmto1 		PICTURE "@Z 999,999,999.99"
		@Fila,65 SAY _qmto2 		PICTURE "@Z 999,999,999.99"
		@Fila,79 SAY _qmto3 		PICTURE "@Z 999,999,999.99"
		@Fila,93 SAY _qmto4 		PICTURE "@Z 999,999,999.99"
		@Fila,107 SAY _qmto5 		PICTURE "@Z 999,999,999.99"
		@Fila,121 SAY _qmto7 		PICTURE "@Z 999,999,999.99"
		@Fila,135 SAY _qmto8 		PICTURE "@Z 999,999,999.99"
		@Fila,149 SAY _qmto11 		PICTURE "@Z 999,999,999.99"
		@Fila,163 SAY _qmto12 		PICTURE "@Z 999,999,999.99"
		@Fila,177 SAY Temp.Import 	PICTURE "@Z 999,999,999.99"
		@Fila,191 SAY Temp.Import 	PICTURE "@Z 999,999,999.99"
		_tmto13	=_tmto13	+	Temp.Import
		_tmto14	=_tmto14	+	Temp.Import
		_pmto13	=_pmto13	+	Temp.Import
		_pmto14	=_pmto14	+	Temp.Import 
	ENDIF
	Fila = Fila + 1
	IF Fila >= 60
		   Fila = 8
		   DO Imp_Header
	ENDIF   
	STORE 0 TO _qmto1,_qmto2,_qmto3,_qmto4,_qmto5,_qmto7,_qmto8,_qmto11,_qmto12
	Vcal = Temp.codcal
ENDSCAN
@Fila,2 SAY REPLICATE("Ä",230)
Fila=Fila+1
@Fila,02 SAY "TOTAL SUBP.=>"+Val_SUBP(substr(Vcal,10,3),'CODSUB'+substr(Vcal,8,2)+'    ','D',22,20)
@Fila,51 SAY _pmto1	 	PICTURE "@Z 999,999,999.99"
@Fila,65 SAY _pmto2	 	PICTURE "@Z 999,999,999.99"
@Fila,79 SAY _pmto3	 	PICTURE "@Z 999,999,999.99"
@Fila,93 SAY _pmto4	 	PICTURE "@Z 999,999,999.99"
@Fila,107 SAY _pmto5 	PICTURE "@Z 999,999,999.99"
@Fila,121 SAY _pmto7 	PICTURE "@Z 999,999,999.99"
@Fila,135 SAY _pmto8	PICTURE "@Z 999,999,999.99"
@Fila,149 SAY _pmto11 	PICTURE "@Z 999,999,999.99"
@Fila,163 SAY _pmto12 	PICTURE "@Z 999,999,999.99"
@Fila,177 SAY _pmto13 	PICTURE "@Z 999,999,999.99"
@Fila,191 SAY _pmto14 	PICTURE "@Z 999,999,999.99"
@Fila,205 SAY _pmto15 	PICTURE "@Z 999,999,999.99"
@Fila,219 SAY _pmto16 	PICTURE "@Z 999,999,999.99"
Fila = Fila + 1
@Fila,2 SAY REPLICATE("Ä",230)
Fila = Fila + 1
@Fila,02 SAY "TOTAL GENERAL =>"
@Fila,51 SAY _tmto1		PICTURE "@Z 999,999,999.99"
@Fila,65 SAY _tmto2 	PICTURE "@Z 999,999,999.99"
@Fila,79 SAY _tmto3 	PICTURE "@Z 999,999,999.99"
@Fila,93 SAY _tmto4 	PICTURE "@Z 999,999,999.99"
@Fila,107 SAY _tmto5 	PICTURE "@Z 999,999,999.99"
@Fila,121 SAY _tmto7 	PICTURE "@Z 999,999,999.99"
@Fila,135 SAY _tmto8 	PICTURE "@Z 999,999,999.99"
@Fila,149 SAY _tmto11 	PICTURE "@Z 999,999,999.99"
@Fila,163 SAY _tmto12 	PICTURE "@Z 999,999,999.99"
@Fila,177 SAY _tmto13 	PICTURE "@Z 999,999,999.99"
@Fila,191 SAY _tmto14 	PICTURE "@Z 999,999,999.99"
@Fila,205 SAY _tmto15 	PICTURE "@Z 999,999,999.99"
@Fila,219 SAY _tmto16 	PICTURE "@Z 999,999,999.99"
Fila = Fila + 1
@Fila,2 SAY REPLICATE("Ä",230)
SET DEVICE TO SCREEN 
RETURN


PROCEDURE imp_header
*----------------
@1,3   SAY ALLTRIM(CIA)
@1,225 SAY "PAG:"
@1,230 SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisHojTR"
@2,75  SAY 'REPORTE HOJA DE TRABAJO  DEL '+DTOC(Vfecini)+' AL  '+DTOC(vfecfin)
@2,215 SAY "FECHA:"
@2,225 SAY DATE()           
@3,75  SAY "CUENTA CORRIENTE : "+ALLTRIM(Val_Fun('Caja','CodCtc',"ALLTRIM(CodCtc)+' '+Descri",CodCtc))
@4,2 SAY REPLICATE("Ä",230)
@5,3 SAY "Doc   N§H/C     Cheque          Raz¢n Social           01.00         02.00         03.00        04.00        05.00         07.00         08.00          11.00            OTROS      LIQUIDO     BRUTO                RETENCIONES"
@6,3 SAY "                                                                                                                                                                                    GIRADO    COMPROMETIDO       DEBE          HABER"
@7,2 SAY REPLICATE("Ä",230) 
RETURN



