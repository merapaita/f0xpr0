* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? TIPGAS.PRG        24/05/99                                   ?	
* ? TIPO DE GASTO (CORRIENTE O CAPITAL)                          ?
* ? PILAR PACHERRE NAVARRO                             05/1999   ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁
*-Abriendo archivos
*
USE Parmae   IN  1  ORDER  TAG Parmae1   ALIAS Parma
USE Detcta   IN  2  ORDER  TAG Detcta5   ALIAS Detcta
USE ComPag   IN  3  ORDER  TAG ComPag1   ALIAS ComPag
USE IteCp    IN  4  ORDER  TAG IteCp1    ALIAS IteCp
USE Cheque   IN  5  ORDER  TAG Cheque4   ALIAS Cheque
USE Reten    IN  6  ORDER  TAG Reten5    ALIAS Reten
USE Cajas    IN  8  ORDER  TAG Cajas1    ALIAS Caja
*
Vmens08 = 'No hay registros para procesar'
vctacte  = SPACE(14)
xMes     = SPACE(2)
xAno     = RIGHT(DTOC(DATE()),2)
*
SELECT Caja
DEFINE WINDOW TIPGAS FROM 03,09 TO 16,70  DOUBLE SHADOW COLOR SCHEME 5 
*
ACTIVATE WINDOW TIPGAS
@ 0, 1 SAY '        Mes y A쨚 : ' GET xMes PICTURE '!!'  VALID Val_Para(xMes,'FECMES',' ',22,9)   &&WHEN velec # 1
@ 0,25 GET xAno PICTURE '!!' 
@ 2, 1 SAY ' Cuenta Corriente : ' GET vCtaCte PICTURE '!!!!!!!!!!!!!!' ; 
VALID  Val_fun('Caja','CodCtc','codctc+descri',vCtaCte,1,1,22,'LEFT(DESCRI,38)')  &&AND  Val_Tipo()
@10,12 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
READ CYCLE VALID exi()
DEACTIVATE WINDOW TIPGAS
IF LASTKEY()=27 OR okcancel = 2
	CLOSE DATA
  	RETURN
ENDIF
DEFINE WINDOW Xwait FROM 12,06 TO 14,78 COLOR SCHEME 05 
ACTIVATE WINDOW Xwait
@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
vind1= SYS(3) +'.idx'
filaux = SYS(3) + '.DBF'
SELE 7
CREATE TABLE (filaux);
	  (Mes		 C(002),;
	   CodCtc    C(014),;
	   nummes    C(002),;
	   Estado    C(002),;
	   Pres      C(001),;
	   Numchq    C(014),;
	   Numcp	 C(004),;
	   TIPDOC	 C(003),;
	   Valchq    N(18,2),;
	   ValEXT    N(18,2),;
	   TIPO		 C(002),;
	   Codpart   C(006),;
   	   Retencio  c(018))
USE 
USE (filaux) IN 15 EXCLUSIVE ALIAS Temp
*
SELE Detcta
*INDEX ON MES+CODCTC TO vind1
SEEK ALLTRIM(Xmes)+ALLTRIM(vCtaCte)
IF !EOF()
		SET FILT TO XMES=MES AND CODCTC=VCTACTE AND TIPDOC='CHQ'
        DO Tempcp
	    DEACTIVATE WINDOW xwait
        sele temp
   		DO REPORTE WITH 2, "tipgasto", 'Tipo de Gasto ',1,.F.,.T.			
ELSE
        DO standby WITH vmens08
        CLOSE DATA
        RETURN
ENDIF
SET INDEX TO
RELEASE  WINDOW lisprg
ACTIVATE SCREEN
CLOSE DATABASE
RETURN


FUNCTION Tempcp
*---------------
*vNUMCHQ  = 	detcta.numchq
DO standby WITH FILAUX
presw=' '
SCAN
	VVALCHQ=0.00    
	vreten= space(18)
	vcodpart= space(6)
	presw=' '
	sw=' '
	WValchq  = 	0.00
	vNumChq  = 	detcta.NumChq
	VValchq  = 	detcta.CANCAR
	VTIPDOC  =  DETCTA.TIPDOC
    SELE CHEQUE
    SEEK vnumchq
    IF !EOF()
    	vnummes=cheque.nummes
    	vnumcp=cheque.numcp
    	wnumcp=cheque.numcp
		vestado=cheque.estado
		VTIPO=CHEQUE.TIPDOC
    	do WHILE cheque.numchq=vnumchq AND NOT EOF()
			WVALCHQ=WVALCHQ+VALCHQ
			SKIP
		ENDDO
		IF VTIPO='RE' OR VTIPO='SR'
			DO CARRET
		ELSE
			do carITC
		ENDIF
	else
		sele 5
		use
		use h:\sis98\data\cheque in 9 order tag cheque4 alias chequee
		sele chequee
	    SEEK vnumchq
	    IF !EOF()
	    	vnummes=chequee.nummes
	    	vnumcp=chequee.numcp
			vestado=chequee.estado
			VTIPO=CHEQUEE.TIPDOC
			WVALCHQ=WVALCHQ+CHEQUEe.VALCHQ
			IF VTIPO='RE' OR VTIPO='SR'
				DO CARRET
			ELSE
				do carITC
			ENDIF
		endif
		sele 5
  		use 
		use cheque in 5 order tag cheque4 alias cheque
	endiF
     SELE detcta
ENDSCAN
return
	
FUNCTION CarRET
*---------------
sele reten
seek vnummes+vnumcp+ALLTRIM(vctacte)
IF !EOF()
	vnummes=reten.nummes
	vnumcp=reten.numcp
	sw='1'
Else
	vreten='NO ESTA EN RETEN'
endif
DO CARITC				
RETURN

FUNCTION Caritc
*---------------
sele itecp
	seek vnummes+vnumcp+ALLTRIM(vctacte)
    IF !EOF()
		do while itecp.nummes=vnummes and itecp.numcp=vnumcp and itecp.codctc=vctacte and not eof()
			vcodpart=itecp.codpart
			IF (VCODPART<>'0000RE' AND VCODPART<>'0000SU') OR ESTADO<>'99'
				if SUBSTR(Vcodpart,2,1)$'1234'
					presw='C'
				else
					if substr(Vcodpart,2,1)$'57'
						presw='K'
					eLSE
				        presw=' '
				    ENDIF
				endif			
*			ELSE
*				IF itecp.codpart='0000RE'
*					DO standby WITH vCODPART
*					DO CARRET
*				ENDIF
			ENDIF
			skip
		enddo
		do cargda
	else
		sele 4
		use
		use h:\sis98\data\itecp  in 10 order tag itecp1 alias itecpe
		sele itecpe
		seek vnummes+vnumcp+ALLTRIM(vctacte)
    	IF !EOF()
			do while itecpe.nummes=vnummes and itecpe.numcp=vnumcp and itecpe.codctc=vctacte and not eof()
				vcodpart=itecpe.codpart
				IF (vCODPART<>'0000RE' AND vCODPART<>'0000SU') OR ESTADO<>'99'
					if SUBSTR(vcodpart,2,1)$'1234'
						presw='C'
					else
						if substr(vcodpart,2,1)$'57'
							presw='K'
						ELSE
					        presw=' '
						endif
					endif
				endif
				skip
			enddo
			do cargda
		eLSE
			VCODPART=SPACE(6)
		ENDIF			
		sele 4
		use
		use itecp in 4 order tag itecp1 alias itecp
	endif
sele detcta	
return

FUNCTION cargda
*---------------
    SELECT Temp
    	IF F_appd()
            REPLACE mes    	WITH 	xmes
            replace nummes  with    vnummes
            REPLACE codctc 	WITH 	vctacte
            REPLACE estado 	WITH 	vestado        
            REPLACE numchq 	WITH 	vnumchq        
            if sw='1'
	            REPLACE numcp   WITH 	wnumcp
	        else
	            REPLACE numcp   WITH 	vnumcp
	        endif
            replace pres    with    presw
            REPLACE valchq	WITH 	Wvalchq        
            REPLACE TIPDOC  WITH    VTIPDOC
            REPLACE VALEXT  WITH    VVALCHQ
            REPLACE codpart	WITH 	vcodpart
            REPLACE TIPO    WITH    VTIPO
            replaCE retencio with    vreten
        endif
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
	    @Fila,2 SAY REPLICATE("?",230)
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
@Fila,2 SAY REPLICATE("?",230)
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
@Fila,2 SAY REPLICATE("?",230)
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
@Fila,2 SAY REPLICATE("?",230)
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
@4,2 SAY REPLICATE("?",230)
@5,3 SAY "Doc   N쬓/C     Cheque          Raz줻 Social           01.00         02.00         03.00        04.00        05.00         07.00         08.00          11.00            OTROS      LIQUIDO     BRUTO                RETENCIONES"
@6,3 SAY "                                                                                                                                                                                    GIRADO    COMPROMETIDO       DEBE          HABER"
@7,2 SAY REPLICATE("?",230) 
RETURN

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,VAL(xMes)*9-8,9)) + ' ' + xAno
