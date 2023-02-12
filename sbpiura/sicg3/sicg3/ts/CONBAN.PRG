*
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ ConBan.Prg  22/08/96                                L: 1779  ³	
* ³ Proceso de Conciliaciones Bancarias - Emisi¢n                ³
* ³ AUTORES :  Julio Cruz Ortiz  (Versi¢n Inicial)   		     ³
* ³            Ing. Federico Montero Valdiviezo REGION GRAU      ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.   07/1997   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Estados :
* 			Conciliados        						(*)
*			N/C sin afectaci¢n 						(+)
*			Cargos indebidos						(I)
*			Documentos Devengados                   (-)
*			Cheques a£n no conciliados				( )
*			N/A , B/D.Depositos no contabilizados   (|)
*			Cheques no v lidos						(X)
*			Pagos menos por el banco  				(<)
*			Pagos de m s por el banco  				(>)
*			Cargo indebido extornado                (E)
*           N/C,N/A,B/D Conciliado                  ()
*			Cheques certificados pendientes         (C)
*			Abonos pendientes                       ()
*			Cargos pendientes                       ()

PARAMETERS vOpc

*- Abre archivos y se posiciona en £ltimo registro

USE parmae   IN 1  ORDER TAG parmae1                   ALIAS Parma
USE BcoCta   IN 2  ORDER TAG BcoCta2                   ALIAS BcoCta
USE DetCta   IN 3  ORDER TAG DetCta1                   ALIAS DetCta
USE MovBco   IN 4  ORDER TAG MovBco3                   ALIAS MovBco
USE Cheque   IN 5  ORDER TAG Cheque1                   ALIAS Cheque
USE Cajas    IN 6  ORDER TAG Cajas1                    ALIAS Caja
USE Salbcos  IN 9  ORDER TAG SalBcos1				   ALIAS Salbcos
USE Chqpen   IN 10 ORDER TAG Chqpen1				   ALIAS Chqpen
USE HChqpen  IN 11 ORDER TAG HChqpen1				   ALIAS HChqpen
USE ChqCert  IN 12                  				   ALIAS Chqcert
USE ConBan   IN 7 									   ALIAS ConBan
*USE hSalbcos IN 13  ORDER TAG hSalBco1				   ALIAS hSalbcos

*-Ambiente

vExact = SET('EXACT')
SET EXACT ON

*- Declara variables

PUBLIC vCodCtc, vNCheque, vMonto, vCuenta, vNumMes, vMes, xFecha, vMancom;
		vperiodo, vflag, vtit, vRev, xmes, vper, vcodcta,vF1,vF2,Vuser_id,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
			'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')

STORE SPACE(17) TO vCodCtc, vCuenta
STORE SPACE(2)  TO  vNumMes
STORE SPACE(14) TO vNCheque

STORE 0 TO vMonto,vf1,vf2
STORE SPACE(40) TO vConcept
vMes     = PADL(ALLTRIM(STR(MONT(DATE()))),2,'0')
vperiodo = RIGHT(STR(YEAR(m.Fecsis)),2)
VFLAG =  .T.

*- Definici¢n de ventanas

DEFINE WINDOW _verDif FROM 14,35 TO 21,79 COLOR SCHEME 10 TITLE " Reporte de Conciliaci¢n " DOUBLE

ACTIVATE WINDOW _VerDif
SELECT Caja
@ 1, 1  SAY "   Fec.Conc.: " GET vMes    PICTURE '!!' VALID Val_Para(vMes,'FECMES',' ',19,9)
@ 1,19  GET vPeriodo PICTURE '!!'
@ 2, 1  SAY "      Cuenta: " GET vCuenta PICTURE '@!';
		VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCuenta,1,3,16,'descri',27)
@ 4, 7  GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 15
READ CYCLE VALID ExiCon()

RELEASE WINDOW _VerDif
vmes = ALLTRIM(vmes)
bcow=banco

IF LASTKEY()=27 OR Okcancel = 2
	RELEASE vCodCtc, vNCheque, vMonto, vCuenta, vNumMes, vMes, xFecha, vperiodo, vflag, vRev, xmes, vper, vcodcta
	CLOSE DATA
	DEACTIVATE WINDOW  xwait
	RETURN
ENDIF

vInd = SYS(3) + '.idx'

IF Vflag
	DO Valida1
	IF vrev
		SET BELL TO 2000,19
		FOR I=1 TO 1
			?''
		ENDFOR
		SET BELL TO
		DEACTIVATE WINDOW Xwait
		DEFINE WINDOW Ywait FROM 9,20 TO 15,60 NONE COLOR SCHEME 5 
		ACTIVATE WINDOW Ywait
		@ 0, 0 SAY SPACE(41)  COLOR W+/N*
		@ 1, 0 SAY PADC(" REVISE EL REPORTE DE DOCUMENTOS ",41,' ') COLOR W+/N*
		@ 2, 0 SAY PADC(" POR REGULARIZAR. HAY DOCUMENTOS ",41,' ') COLOR W+/N*
		@ 3, 0 SAY PADC(" NO VALIDOS",41,' ')  COLOR W+/N*
		@ 4, 0 SAY SPACE(41) COLOR W+/N*
		@ 5, 0 SAY SPACE(41) COLOR W+/N*
		@ 5,15 GET okcancel FUNCTION '*TH \!\<OK' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 3
		@ 6, 0 SAY SPACE(41) COLOR W+/N*
		READ
		DEACTIVATE WINDOW Ywait
	ENDIF
ENDIF

DEACTIVATE WINDOW Xwait

*- Conciliaci¢n Bancaria
IF escolor
   DEFINE POPUP POP_04a FROM 10,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP POP_04a FROM 10,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  POP_04a PROMPT 'Cheques \<Pendientes  '   MESSAGE ''
DEFINE BAR 2  OF  POP_04a PROMPT 'Conc.Banc. \<Resumido '   MESSAGE ''
DEFINE BAR 3  OF  POP_04a PROMPT 'Conc.Banc. \<Detallado'   MESSAGE ''
DEFINE BAR 4  OF  POP_04a PROMPT 'Docum. \<X Regularizar'   MESSAGE ''
DEFINE BAR 5  OF  POP_04a PROMPT 'Docum. De\<vengados  '   MESSAGE ''
DEFINE BAR 6  OF  POP_04a PROMPT '\<Cierre de Conciliaci¢n' MESSAGE '' 

ON SELECTION POPUP pop_04a  DO  menu_04a
ACTIVATE POPUP pop_04a

CLOSE DATA
ERASE (vInd)

SET EXACT OFF

RELEASE vCodCtc, vNCheque, vMonto, vCuenta, vNumMes, vMes, xFecha, vperiodo, vflag, vRev;
		, xmes, vper, vcodcta

RETURN


FUNCTION Valida1  && Cheques vs Extracto Bancario
*---------------
	@0,30 SAY 'Chq-Ext' COLOR W+/BR*
	SELECT DetCta
	vInd = SYS(3) + '.idx'
	INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind) FOR codctc = vcuenta AND mes=vmes 
	SELECT Cheque
	vInd1 = SYS(3) + '.Idx'
	INDEX ON CodCtc+ALLTRIM(numchq) TO (vInd1) FOR CodCtc=vCuenta .AND.;
		Nummes=vMes AND (IIF(!EMPTY(numha),nummesha>vmes,estado<>'99'))
	GO TOP
	vNumCP   = Cheque.NumCP
	vNumMes  = Cheque.NumMes
	vNCheque = ALLTRIM(EliAlf2(Cheque.NumChq))
	vCodCtc  = Cheque.CodCtc
	vFecha   = Cheque.FecChq
	vMonto   = Cheque.ValChq
	vNomGir  = Cheque.NomGir
	vCodCad  = Cheque.CodCad
	VNUMHA   = CHEQUE.NUMHA
	SCAN
		vNumCP   = Cheque.NumCP
		vNumMes  = Cheque.NumMes
		vNCheque = ALLTRIM(EliAlf2(Cheque.NumChq))
		NUMCHW   = NUMCHQ
		vCodCtc  = Cheque.CodCtc
		vFecha   = Cheque.FecChq
		vMonto   = Cheque.ValChq
		vNomGir  = Cheque.NomGir
		vCodCad  = Cheque.CodCad
		VNUMHA   = CHEQUE.NUMHA
		VMONTO   = 0.00
		XPILW    = RECNO()
		DO WHILE CODCTC=VCODCTC AND NUMCHQ=NUMCHW AND NOT EOF()
			VMONTO=VMONTO+CHEQUE.VALCHQ
			SKIP
		ENDDO
		GO XPILW
		SELECT DetCta
		SEEK vCodCtc+vNCheque
		IF !FOUND()
			SELECT ChqPen
			IF f_appd()
				REPLACE ChqPen.CodCtc  WITH vCodCtc;
						ChqPen.NumCP   WITH vNumCP;
						ChqPen.NumMes  WITH vNumMes;
						ChqPen.NumChq  WITH vNCheque;
						ChqPen.ValChq  WITH vMonto;
						ChqPen.NomGir  WITH vNomGir;
						ChqPen.CodCad  WITH vCodCad;
						chqpen.periodo WITH vPeriodo;
						ChqPen.Fecha   WITH vFecha;
						Chqpen.tipdoc  WITH 'CHQ';
						Chqpen.Conban  WITH ' ';
						Chqpen.tipope  WITH '1';
						CHEQUE.NUMHA   WITH VNUMHA
			ENDIF
			UNLOCK
		ELSE
			SELECT Detcta
			IF RLOCK()
				REPLACE conban WITH '*'
			ENDIF
			UNLOCK
			SELECT Cheque
			IF RLOCK()
				REPLACE conban WITH '*'
			ENDIF
			UNLOCK
		ENDIF
		SELECT Cheque
	ENDSCAN
	SELECT Cheque
	SET INDEX TO
	SELECT Detcta
	SET INDEX TO
    DO Valida2
RETURN


FUNCTION Valida2  && Cruza Extracto Bancario vs CHEQUES - CHEQUES PENDIENTES.
*---------------
@0,30 SAY 'Ext-Chq' COLOR W+/BR*
SELECT HChqPen
vInd1 = SYS(3) + '.idx'
INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind1) FOR codctc = vcuenta
SELECT Cheque
vInd = SYS(3) + '.idx'
INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind) FOR codctc = vcuenta AND nummes= vmes
SELECT DetCta
SET ORDER TO Detcta6
SET FILTER TO Codctc = vCuenta AND Mes = vmes AND RIGHT(ALLTRIM(STR(YEAR(Fecbco))),2)=vperiodo ;
			AND (Tipdoc='CHQ' OR Tipdoc='C/O' OR Tipdoc='C/I')
GO TOP
vCodCtc  = DetCta.CodCtc
vNumMes  = DetCta.Mes
vNCheque = ALLTRIM(Elialf2(DetCta.NumChq))
vFecha   = DetCta.FecBco
vMonto   = ABS(DetCta.Monto)
VTIPDOC  = DetCta.Tipdoc

SCAN
	vCodCtc  = DetCta.CodCtc
	vCodCtc  = DetCta.CodCtc
	vNumMes  = DetCta.Mes
	VNOMB    = DETCTA.CONCEPTO
	vNCheque = ALLTRIM(Elialf2(DetCta.NumChq))
	vFecha   = DetCta.FecBco
	vMonto   = ABS(DetCta.Monto)
	VTIPDOC  = DetCta.Tipdoc
	SW=0
	
	IF vtipdoc = 'C/O'
		LOOP
	ENDIF
	
	IF vtipdoc # 'C/I'
		SELECT Cheque
		SEEK vCodCtc+vNCheque
		
		IF !FOUND()
			SELECT HChqPen
			SEEK vCodCtc+vNCheque
			IF !FOUND()
				SELECT Chqpen
				IF f_appd()
					REPLACE ChqPen.CodCtc  WITH vCodCtc;
							ChqPen.NumMes  WITH vMes;
							ChqPen.NumChq  WITH vNCheque;
							ChqPen.Fecha   WITH vFecha;
							ChqPen.ValChq  WITH vMonto;
							Chqpen.conban  WITH 'X' ;
							chqpen.periodo WITH vPeriodo ;
							Chqpen.Tipdoc  WITH vTipdoc
					vRev = .T.
    			ENDIF
    			UNLOCK
    		ELSE
    			SELE Hchqpen
    			DO CASE
    				CASE HChqpen.Valchq > Vmonto
    					SELECT Chqpen
    					IF f_appd()
    						REPLACE Chqpen.Codctc WITH vcodctc ;
    								Chqpen.Nummes WITH vmes ;
    								Chqpen.Numchq WITH vncheque;
    								Chqpen.fecha  WITH vfecha ;
    								Chqpen.Valchq WITH ABS(HChqpen.Valchq-Vmonto) ;
    								Chqpen.conban WITH '<' ;
    								chqpen.periodo WITH vPeriodo ;
    								Chqpen.Tipdoc  WITH 'PME';
    								Chqpen.tipope  WITH '1'
    						
							SELE Hchqpen
    						IF RLOCK()
    							REPLACE HChqpen.conban  WITH '-'
    							REPLACE HChqpen.MesReb  WITH vMes
    						ENDIF
    						UNLOCK
    						
    						SELECT Detcta   &&Concilia en el Extracto
    						IF RLOCK()
    							REPLACE Conban WITH '*'
    						ENDIF
    						UNLOCK
    					ENDIF
    					SELE chqpen
    					UNLOCK
    	           			
	    	     	CASE HChqpen.Valchq < Vmonto
	    	   	 		SELECT Chqpen
					    IF f_appd()
    	     				REPLACE Chqpen.Codctc WITH vcodctc ;
         							Chqpen.Nummes WITH vmes ;
			         				Chqpen.Numchq WITH vncheque;
	    		     				Chqpen.fecha  WITH vfecha ;
        			 				Chqpen.Valchq WITH ABS(HChqpen.Valchq-Vmonto) ;
         							Chqpen.conban WITH '>' ;
                					chqpen.periodo WITH vPeriodo ;
                					Chqpen.Tipdoc  WITH 'PMA';
	                				Chqpen.tipope  WITH '2'                		
								    SELE Hchqpen
							     	IF RLOCK()
					        	 		REPLACE HChqpen.conban  WITH '-'
							       	 	REPLACE HChqpen.MesReb  WITH vMes
							       	ENDIF	
						       	 	UNLOCK
       					
       						 	    SELECT Detcta   &&Concilia en el Extracto
							     	IF RLOCK()
						    	        REPLACE Conban WITH '*'
						    	    ENDIF    
					    	        UNLOCK
							ENDIF		
	                		SELE chqpen
	                		UNLOCK
						
					OTHERWISE
						SELE Hchqpen
						IF RLOCK()
				        	REPLACE HChqpen.conban  WITH '-'
					       	REPLACE HChqpen.MesReb  WITH vMes
					    ENDIF
					    UNLOCK 	
       				 	SELECT Detcta   &&Concilia en el Extracto
       				 	IF RLOCK()
				    	    REPLACE Conban WITH '*'
				    	ENDIF
						UNLOCK    
				ENDCASE    	
    		ENDIF
    	    
		ELSE   						&& Si encuentra en cheque
	     	
	    	SELECT Cheque
	    	
	    	DO CASE
	    		CASE Cheque.Valchq > Vmonto
	    			SELECT Chqpen
	    			IF f_appd()
	    				REPLACE Chqpen.Codctc WITH vcodctc ;
	         					Chqpen.Nummes WITH vmes ;
	    	     				Chqpen.Numchq WITH vncheque;
	    	    	 			Chqpen.fecha  WITH vfecha ;
		    	   				Chqpen.Valchq WITH ABS(Cheque.Valchq-Vmonto) ;
    	    	 				Chqpen.conban WITH '<' ;
        			        	chqpen.periodo WITH vPeriodo ;
            	    			Chqpen.Tipdoc  WITH 'PME';
                				Chqpen.tipope  WITH '1'
                				
                		SELE cheque
						IF RLOCK()
							REPLACE Conban WITH '*'
						ENDIF
						UNLOCK
						SELECT Detcta   &&Concilia en el Extracto
						IF RLOCK()
							REPLACE Conban WITH '*'
						ENDIF
						UNLOCK
                	ENDIF		
	                SELE chqpen
	                UNLOCK
	                
		    	CASE Cheque.Valchq < Vmonto
					SELECT Chqpen
					IF f_appd()
						REPLACE Chqpen.Codctc WITH vcodctc ;
	         					Chqpen.Nummes WITH vmes ;
		         				Chqpen.Numchq WITH vncheque;
    		     				Chqpen.fecha  WITH vfecha ;
        		 				Chqpen.Valchq WITH ABS(cheque.Valchq-Vmonto) ;
         						Chqpen.conban WITH '>' ;
	                			chqpen.periodo WITH vPeriodo ;
    	            			Chqpen.Tipdoc  WITH 'PMA';
	    	            		Chqpen.tipope  WITH '2'
	    	            		
						SELE cheque
						IF RLOCK()
							REPLACE Conban WITH '*'
						ENDIF
			        	UNLOCK
			        	SELECT Detcta   &&Concilia en el Extracto
			        	
			        	IF RLOCK()
			        		REPLACE Conban WITH '*'
			        	ENDIF
			        	
			        	UNLOCK
			        ENDIF
			        SELE chqpen
	                UNLOCK
	                
				OTHERWISE
					SELE cheque
					IF RLOCK()
						REPLACE Conban WITH '*'
					ENDIF
					
					UNLOCK
					
					SELECT Detcta   &&Concilia en el Extracto
					
					IF RLOCK()
						REPLACE Conban WITH '*'
					ENDIF
					
					UNLOCK
			ENDCASE
		ENDIF
	
	ELSE                                         (TIPDOC=C/I)
		SELECT Chqpen
		IF f_appd()
			REPLACE ChqPen.CodCtc  WITH vCodCtc;
    	  	        ChqPen.NumMes  WITH vMes;
        	   	    ChqPen.NumChq  WITH vNCheque;
            	   	ChqPen.Fecha   WITH vFecha;
                	ChqPen.ValChq  WITH vMonto;
                	CHQPEN.NOMGIR  WITH VNOMB;
	           	    Chqpen.conban  WITH 'I' ;
    	           	chqpen.periodo WITH vPeriodo ;
        	       	Chqpen.Tipdoc  WITH vTipdoc ;
            	   	Chqpen.tipope  WITH '2'
	    ENDIF       	
    	UNLOCK
    ENDIF 											(TIPDOC=C/I)
    
    SELECT Detcta
ENDSCAN

SET ORDER TO detcta1
SET FILTER TO
SELECT HChqpen
SET INDEX TO
SELECT Cheque
SET INDEX TO
DO Valida3
RETURN


FUNCTION Valida3     && Cruza Extracto con Movimiento Bancos
*--------------
@0,30 SAY 'Ext-Bco' COLOR W+/BR*

SELECT HChqPen
vInd1 = SYS(3) + '.idx'
INDEX ON DTOC(Fecha)+Tipo+ALLTRIM(STR(valchq,18,2)) To (vind1) FOR codctc = vcuenta AND (conban='' OR conban='') AND tipdoc#'CHQ'

SELECT Movbco
vInd = SYS(3) + '.Idx'
INDEX ON DTOC(Fecha)+LEFT(Transa,1)+ALLTRIM(STR(Monto,18,2))+conban TO (vInd) ;
		 FOR Periodo = vPeriodo and NUMMES=ALLTRIM(VMES) ;
		 	 AND codctc = vcuenta AND Tipdoc#'NOC' AND estado#'99'

SELECT DetCta
vInd2 = SYS(3) + '.Idx'
INDEX ON DTOC(Fecbco)+Tipo+ALLTRIM(STR(Monto)) TO (vInd2) ;
		 FOR RIGHT(ALLTRIM(STR(YEAR(Fecbco))),2)+mes+Codctc ;
		 	 = vPeriodo+vmes+vcuenta AND Tipdoc#'OTR' AND Tipdoc#'CHQ' AND Tipdoc#'C/O';
		 	 AND Tipdoc#'EXT' AND Tipdoc#'NOC' AND tipdoc#'C/I'
GO TOP		 	 
vCodCtc  = DetCta.CodCtc
vNumMes  = DetCta.Mes
vNCheque = ALLTRIM(DetCta.NumChq)
vFecha   = DetCta.FecBco
vMonto   = ABS(DetCta.Monto)
VTipdoc  = DetCta.Tipdoc
vTipo    = Detcta.Tipo
venc     = .F.
SCAN
     vCodCtc  = DetCta.CodCtc
     vNumMes  = DetCta.Mes
     vNCheque = ALLTRIM(DetCta.NumChq)
     vFecha   = DetCta.FecBco
     vMonto   = ABS(DetCta.Monto)
     VNOMB    = DETCTA.CONCEPTO
     VTIPDOC  = DetCta.Tipdoc
     vTipo    = Detcta.Tipo
     venc     = .F.
	 SELECT Movbco
     SEEK DTOC(vFecha)+vTipo+ALLTRIM(STR(vMonto,18,2))+' '
     IF FOUND() 
		&&revisar cuando son repetidos (rr)    
  		IF Conban # '' 
		     	IF RLOCK()
    		 		REPLACE conban WITH ''
    		 	ENDIF	
    	 		SELECT Detcta
		     	IF RLOCK()
    		 		REPLACE Conban WITH '*'
    		 	ENDIF	
    	 		venc = .T.
 	    ENDIF
   	 	SELECT Movbco
     ENDIF
     IF !venc	 
		 SELECT hchqpen
    	 SEEK DTOC(vFecha)+vTipo+ALLTRIM(STR(vMonto,18,2))
	     IF FOUND() 
	     	IF RLOCK()
	 		   REPLACE conban WITH '-'
   		 	ENDIF	
   	 		SELECT Detcta
	     	IF RLOCK()
   		 		REPLACE Conban WITH '*'
   		 	ENDIF	
 	     ELSE
			 SELECT detcta
		    IF RLOCK()
	   			 REPLACE conban WITH '#'
   			ENDIF	 
        	 SELECT ChqPen
		     IF f_appd()
    	     	IF Vtipdoc = 'N/C'
        	    	REPLACE ChqPen.CodCtc  WITH vCodCtc;
	        	        ChqPen.NumMes  WITH vNumMes;
    	        	    ChqPen.NumChq  WITH vNCheque;
	        	        ChqPen.ValChq  WITH vMonto;
    	        	    ChqPen.conban  WITH '+';
    	        	    CHQPEN.NOMGIR  WITH VNOMB;
        	        	chqpen.periodo WITH vPeriodo ;
	        	        CHQPEN.Fecha   WITH vfecha;
    	        	    Chqpen.tipdoc  WITH vtipdoc ;
	        	        Chqpen.tipope  WITH '2';
    	    	        Chqpen.tipo    WITH vtipo
	    	     ELSE
    		    	    REPLACE ChqPen.CodCtc  WITH vCodCtc;
	            	    ChqPen.NumMes  WITH vNumMes;
	    	            ChqPen.NumChq  WITH vNCheque;
        		        ChqPen.ValChq  WITH vMonto;
            		    ChqPen.conban  WITH '|';
            		 	CHQPEN.NOMGIR  WITH VNOMB;   
	            	    chqpen.periodo WITH vPeriodo ;
    	            	CHQPEN.Fecha   WITH vfecha;
	        	        Chqpen.tipdoc  WITH vtipdoc ;
    	        	    Chqpen.tipope  WITH '1' ;               
        		        Chqpen.tipo    WITH vtipo
	        	 ENDIF
		     ENDIF    
		  ENDIF   
   	 ENDIF
     SELECT DetCta
ENDSCAN
SELECT Movbco
SET INDEX TO
SELECT hchqpen
SET INDEX TO
SELECT Detcta
SET INDEX TO
DO VALIDA4
RETURN



FUNCTION Valida4     && Cruza Movimiento de Bancos (Afectaci¢n Presupuestal) - Hchqpen
*--------------
@0,30 SAY 'Reb.N/C' COLOR W+/BR*
SELE Hchqpen		 	 
vInd2 = SYS(3) + '.Idx'
INDEX ON ALLTRIM(STR(valchq,18,2)) TO (vInd2) ;
		 FOR (Periodo = vPeriodo OR periodo='06')  AND codctc = vcuenta AND conban='+'

SELECT Movbco
vInd = SYS(3) + '.Idx'
INDEX ON ALLTRIM(STR(Monto,18,2)) TO (vInd) ;
		 FOR Periodo = vPeriodo and NUMMES=ALLTRIM(VMES) ;
		 	 AND codctc = vcuenta AND Tipdoc#'NOC' AND EMPTY(conban) AND tipdoc='N/C'  AND estado#'99'    &&AND hojcon='S'
GO TOP
vCodCtc  = movbco.CodCtc
vNumMes  = movbco.nummes
vFecha   = movbco.Fecha
vMonto   = movbco.Monto
VTipdoc  = movbco.Tipdoc
vTipo    = LEFT(movbco.Transa,1)
SCAN
	vCodCtc  = movbco.CodCtc
	vNumMes  = movbco.nummes
	vFecha   = movbco.Fecha
	vMonto   = movbco.Monto
	VTipdoc  = movbco.Tipdoc
	vTipo    = LEFT(movbco.Transa,1)
	 SELECT hchqpen
     SEEK ALLTRIM(STR(vMonto,18,2))
     IF FOUND() 
     	IF RLOCK()
	   		REPLACE HChqpen.conban  WITH '-'
    	   	REPLACE HChqpen.MesReb  WITH vMes
    	ENDIF   	
       	SELE movbco
     	IF RLOCK()
	 		REPLACE conban WITH ''
	 	ENDIF	
     ENDIF
     SELECT movbco
ENDSCAN
SELECT Movbco
SET INDEX TO
SELECT hchqpen
SET INDEX TO
DO VALIDA5
RETURN



FUNCTION Valida5     && Cruza Movimiento de Bancos (Dep¢sitos no contabilizados) - Hchqpen
*--------------
@0,30 SAY 'Reb.N/A' COLOR W+/BR*
SELE Hchqpen		 	 
vInd2 = SYS(3) + '.Idx'
INDEX ON ALLTRIM(STR(valchq,18,2)) TO (vInd2) ;
		 FOR (Periodo = vPeriodo OR periodo='06') AND codctc = vcuenta AND conban='|'

SELECT Movbco
vInd = SYS(3) + '.Idx'
INDEX ON ALLTRIM(STR(Monto,18,2)) TO (vInd) ;
		 FOR Periodo = vPeriodo and NUMMES=ALLTRIM(VMES) ;
		 	 AND codctc = vcuenta AND Tipdoc#'NOC' AND EMPTY(conban) AND (tipdoc='N/A' OR tipdoc='B/D') AND estado#'99'
GO TOP		 	 
vCodCtc  = movbco.CodCtc
vNumMes  = movbco.nummes
vFecha   = movbco.Fecha
vMonto   = movbco.Monto
VTipdoc  = movbco.Tipdoc
vTipo    = LEFT(movbco.Transa,1)
SCAN
	vCodCtc  = movbco.CodCtc
	vNumMes  = movbco.nummes
	vFecha   = movbco.Fecha
	vMonto   = movbco.Monto
	VTipdoc  = movbco.Tipdoc
	vTipo    = LEFT(movbco.Transa,1)
	 SELECT hchqpen
     SEEK ALLTRIM(STR(vMonto,18,2))
     IF FOUND() 
     	IF RLOCK()
	   		REPLACE HChqpen.conban  WITH '-'
    	   	REPLACE HChqpen.MesReb  WITH vMes
    	ENDIF   	
       	SELE movbco
     	IF RLOCK()
	 		REPLACE conban WITH ''
	 	ENDIF	
     ENDIF
     SELECT movbco
ENDSCAN
SELECT Movbco
SET INDEX TO
SELECT hchqpen
SET INDEX TO
DO VALIDA6
RETURN


FUNCTION Valida6  && Mov.Bco Dep¢sitos vs Extracto Bancario
*--------------
@0,30 SAY 'Abo-Ext' COLOR W+/BR*
SELECT DetCta
vInd1 = SYS(3) + '.Idx'
INDEX ON DTOC(Fecbco)+Tipo+ALLTRIM(STR(Monto)) TO (vInd1) ;
		 FOR RIGHT(ALLTRIM(STR(YEAR(Fecbco))),2)+mes+Codctc ;
		 	 = vPeriodo+vmes+vcuenta AND Tipdoc#'OTR' AND Tipdoc#'CHQ' AND Tipdoc#'C/O' AND tipdoc#'N/C';
		 	 AND Tipdoc#'EXT' AND Tipdoc#'NOC' AND tipdoc#'C/I' AND EMPTY(conban)

SELECT Movbco
vInd = SYS(3) + '.Idx'
INDEX ON DTOC(Fecha)+LEFT(Transa,1)+ALLTRIM(STR(Monto,18,2)) TO (vInd) ;
		 FOR Periodo = vPeriodo and NUMMESC=ALLTRIM(VMES) ;
		 	 AND codctc = vcuenta AND (Tipdoc='B/D' OR tipdoc='N/A')  AND EMPTY(conban)  AND estado#'99'

GO TOP		 	 
vCodCtc  = Movbco.CodCtc
vNumMes  = Movbco.NumMes
vFechaC  = Movbco.FechaC
vFecha   = Movbco.Fecha
vMonto   = ABS(Movbco.Monto)
VTipdoc  = Movbco.Tipdoc
vTipo    = LEFT(Movbco.transa,1)
venc     = .F.
SCAN
     vCodCtc  = Movbco.CodCtc
     vNumMes  = Movbco.NumMes
	 vFechaC  = Movbco.FechaC
     vFecha   = Movbco.Fecha
     vMonto   = ABS(Movbco.Monto)
     VTIPDOC  = Movbco.Tipdoc
     vTipo    = LEFT(Movbco.Transa,1)
     venc     = .F.
	 SELECT detcta
     SEEK DTOC(vFecha)+vTipo+ALLTRIM(STR(vMonto,18,2))
     IF FOUND() 
  		IF Conban # '*' 
		     	IF RLOCK()
    		 		REPLACE conban WITH '*'
    		 	ENDIF	
    	 		SELECT Movbco
		     	IF RLOCK()
    		 		REPLACE Conban WITH ''
    		 	ENDIF	
    	 		venc = .T.
	 	ENDIF
&&ok 	
	 	SELECT detcta
     ENDIF
     IF !venc	 
         SELECT ChqPen
		     IF f_appd()
    	        REPLACE ChqPen.CodCtc  WITH vCodCtc;
	                ChqPen.NumMes  WITH vNumMes;
        	        ChqPen.ValChq  WITH vMonto;
            	    ChqPen.conban  WITH '';
	                chqpen.periodo WITH vPeriodo ;
    	            CHQPEN.Fecha   WITH vfecha;
        	        Chqpen.tipdoc  WITH vtipdoc ;
            	    Chqpen.tipope  WITH '2' ;               
        	        Chqpen.tipo    WITH vtipo
	         ENDIF
	 		SELECT Movbco
	    	IF RLOCK()
		 		REPLACE Conban WITH ''
		 	ENDIF	
     ENDIF
ENDSCAN
SELECT Movbco
SET INDEX TO
SELECT hchqpen
SET INDEX TO
SELECT Detcta
SET INDEX TO
DO VALIDA7
RETURN

FUNCTION Valida7  && Mov.Bco Cargos vs Extracto Bancario
*--------------
@0,30 SAY 'Car-Ext' COLOR W+/BR*
SELECT DetCta
vInd1 = SYS(3) + '.Idx'
INDEX ON DTOC(Fecbco)+Tipo+ALLTRIM(STR(Monto)) TO (vInd1);
		 FOR RIGHT(ALLTRIM(STR(YEAR(Fecbco))),2)+mes+Codctc;
		 	 =vPeriodo+vmes+vcuenta AND Tipdoc#'OTR' AND Tipdoc#'CHQ' AND TipDoc#'B/D' AND TipDoc#'N/A';
		 	 AND Tipdoc#'EXT' AND Tipdoc#'NOC' AND tipdoc#'C/I' AND EMPTY(conban)

SELECT Movbco
vInd = SYS(3) + '.Idx'
INDEX ON DTOC(Fecha)+LEFT(Transa,1)+ALLTRIM(STR(Monto,18,2)) TO (vInd) ;
		 FOR Periodo = vPeriodo and NUMMESC=ALLTRIM(VMES) ;
		 	 AND codctc = vcuenta AND Tipdoc='N/C'  AND EMPTY(conban) AND estado#'99'
		 	 
GO TOP		 	 
vCodCtc  = Movbco.CodCtc
vNumMes  = Movbco.NumMes
vFechaC  = Movbco.FechaC
vFecha   = Movbco.Fecha
vMonto   = ABS(Movbco.Monto)
VTipdoc  = Movbco.Tipdoc
vTipo    = LEFT(Movbco.transa,1)
venc     = .F.
SCAN
     vCodCtc  = Movbco.CodCtc
     vNumMes  = Movbco.NumMes
	 vFechaC  = Movbco.FechaC
     vFecha   = Movbco.Fecha
     vMonto   = ABS(Movbco.Monto)
     VTIPDOC  = Movbco.Tipdoc
     vTipo    = LEFT(Movbco.Transa,1)
     venc     = .F.
	 SELECT detcta
     SEEK DTOC(vFecha)+vTipo+ALLTRIM(STR(vMonto,18,2))
     IF FOUND() 
  		IF Conban # '*' 
		     	IF RLOCK()
    		 		REPLACE conban WITH '*'
    		 	ENDIF	
    	 		SELECT Movbco
		     	IF RLOCK()
    		 		REPLACE Conban WITH ''
    		 	ENDIF	
    	 		venc = .T.
	 	ENDIF
&&ok 	
	 	SELECT detcta
     ENDIF
     IF !venc	 
         SELECT ChqPen
		     IF f_appd()
    	        REPLACE ChqPen.CodCtc  WITH vCodCtc;
	                ChqPen.NumMes  WITH vNumMes;
        	        ChqPen.ValChq  WITH vMonto;
            	    ChqPen.conban  WITH '';
	                chqpen.periodo WITH vPeriodo ;
    	            CHQPEN.Fecha   WITH vfecha;
        	        Chqpen.tipdoc  WITH vtipdoc ;
            	    Chqpen.tipope  WITH '1' ;               
        	        Chqpen.tipo    WITH vtipo
	         ENDIF
	 		SELECT Movbco
	    	IF RLOCK()
		 		REPLACE Conban WITH ''
		 	ENDIF	
     ENDIF
ENDSCAN
SELECT Movbco
SET INDEX TO
SELECT hchqpen
SET INDEX TO
SELECT Detcta
SET INDEX TO
DO VALIDA8
RETURN


PROCEDURE Valida8  && Revierte los Cargos indebidos.
*----------------
@0,30 SAY 'Reb.C/I' COLOR W+/BR*
vind = SYS(3)+'.IDX'
SELECT Chqpen
INDEX  ON ALLTRIM(STR(valchq)) to (vind) FOR Conban='I' AND codctc = vcuenta AND nummes=ALLT(vmes)

vind = SYS(3)+'.IDX'
SELECT hChqpen
INDEX ON ALLTRIM(STR(valchq)) to (vind) FOR Conban='I' AND codctc = vcuenta

vind = SYS(3)+'.IDX'
SELECT Detcta
INDEX ON DTOC(Fecbco)+ALLTRIM(STR(Monto)) to (vind) FOR Tipdoc='EXT' AND codctc = vcuenta AND mes=vmes
GO TOP
SCAN
	SELECT Chqpen
	SEEK ALLT(STR(detcta.monto))
	IF FOUND() AND RLOCK()
		   REPLACE Conban WITH 'E'
	ELSE
		SELECT hChqpen
		SEEK ALLT(STR(detcta.monto))
		IF FOUND() AND RLOCK()
		   REPLACE Conban WITH 'E'
		   REPLACE mesreb WITH vmes
        ENDIF
	ENDIF
	SELECT Detcta
ENDSCAN	
SET INDEX TO
SELECT hChqpen
SET INDEX TO
SELECT Chqpen
SET INDEX TO
DO Valida9
RETURN


PROCEDURE Valida9    && Extracto - Chq.Certificados
*------------------
	@0,30 SAY 'Ext-Cer' COLOR W+/BR*
	SELECT Cheque
	Vind = SYS(3)+'.IDX'
	INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind) FOR codctc = vcuenta AND NUMMES=vmes
	
	SELECT HChqPen
	vInd1 = SYS(3) + '.idx'
	INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind1) FOR codctc = vcuenta AND tipdoc='CHQ' AND conban$'C- '
	
	SELECT chqcert
	vInd2 = SYS(3) + '.idx'
	INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind2) FOR codctc = vcuenta AND EMPTY(conban)
	
	SELECT Detcta
	SET ORDER TO Detcta6
	SET FILT TO certi='S' AND RIGHT(ALLTRIM(STR(YEAR(Fecbco))),2)+mes+Codctc ;
				 = vPeriodo+vmes+vcuenta AND Tipdoc#'OTR';
				 AND Tipdoc#'EXT' AND Tipdoc#'NOC' AND tipdoc#'C/I'
	GO TOP
	vcodctc = Codctc
	vnumchq = ALLTRIM(ELIALF2(Numchq))
	vnummes = Mes
	vmonto  = abs(monto)
	vfecha  = fecbco
	SCAN 
		vcodctc = Codctc
		vnumchq = ALLTRIM(ELIALF2(Numchq))
		vnummes = Mes
		vmonto  = abs(monto)
		vfecha  = fecbco

		SELECT Chqcert
		SEEK VCodctc+vnumchq
	
		IF !FOUND()
	
			SELECT HChqpen
			SEEK vcodctc+vNumchq
			
			IF FOUND()
		   	    vNumCP   = Hchqpen.NumCP
	   			vnumMes  = Hchqpen.NumMes
		    	vNCheque = ALLTRIM(EliAlf2(Hchqpen.NumChq))
				vCodCtc  = Hchqpen.CodCtc
			    vFecha   = Hchqpen.Fecha
	    		vMonto   = Hchqpen.ValChq
	   	    	vNomGir  = Hchqpen.NomGir
			    vCodCad  = Hchqpen.CodCad
			*	vNomGir  = Cheque.NomGir
			*   vCodCad  = Cheque.CodCad
		    
		    	IF !EMPTY(hchqpen.conban)	 
	    			 SELECT Chqpen
				     IF f_appd()
			    		 REPLACE ChqPen.CodCtc  WITH vCodCtc;
				    	     ChqPen.NumCP   WITH vNumCP;
			            	 ChqPen.NumMes  WITH vNumMes;
					         ChqPen.NumChq  WITH vNCheque;
    				         ChqPen.ValChq  WITH vMonto;
				    	     chqpen.periodo WITH vPeriodo;
		    		         ChqPen.Fecha   WITH vFecha;
					         ChqPen.NomGir  WITH vNomGir;
    			   		     ChqPen.CodCad  WITH vCodCad;
					         Chqpen.tipdoc  WITH 'CHQ';
	  			   	    	 Chqpen.Conban  WITH ' ';
    	    		    	 Chqpen.tipope  WITH '1'      
        	    	 ENDIF    	     
            	 ENDIF    	 
		   		 SELECT Chqpen  &&S¢lo adiciona el Chq.Cert. pendiente
		         IF f_appd()
				    	 REPLACE ChqPen.CodCtc  WITH vCodCtc;
	   			         ChqPen.NumCP   WITH vNumCP;
		        	     ChqPen.NumMes  WITH vNumMes;
	   		        	 ChqPen.NumChq  WITH vNCheque;
			             ChqPen.ValChq  WITH vMonto;
			   	         chqpen.periodo WITH vPeriodo;
			             ChqPen.Fecha  WITH vFecha;
				         ChqPen.NomGir  WITH vNomGir;
 			   	    	 ChqPen.CodCad  WITH vCodCad;
    		    	     Chqpen.tipdoc  WITH 'CHQ';
		    	         Chqpen.Conban  WITH 'C';
    		    	     Chqpen.tipope  WITH '2'      
			    ENDIF	     
			    SELE detcta
	    	 	IF RLOCK()
				    REPLACE Conban WITH '*'
				ENDIF    
		    	
		   	ELSE
   	
				SELECT Cheque
				SEEK vcodctc+vNumchq
				IF FOUND()
	    			 vNumCP   = Cheque.NumCP
			    	 vNumMes  = Cheque.NumMes
				     vNCheque = ALLTRIM(EliAlf2(Cheque.NumChq))
					 vCodCtc  = Cheque.CodCtc
			    	 vFecha   = Cheque.FecChq
			    	 vMonto   = Cheque.ValChq
   		    	     vNomGir  = Cheque.NomGir
					 vCodCad  = Cheque.CodCad
					 VNUMHA   = CHEQUE.NUMHA			 
	    			 SELECT Chqpen
				     IF f_appd()
				    	 REPLACE ChqPen.CodCtc  WITH vCodCtc;
					         ChqPen.NumCP   WITH vNumCP;
				             ChqPen.NumMes  WITH vNumMes;
					         ChqPen.NumChq  WITH vNCheque;
    			    	     ChqPen.ValChq  WITH vMonto;
					         ChqPen.NomGir  WITH vNomGir;
    			   	    	 ChqPen.CodCad  WITH vCodCad;
				    	     chqpen.periodo WITH vPeriodo;
			    	         ChqPen.Fecha   WITH vFecha;
					         Chqpen.tipdoc  WITH 'CHQ';
    				   	     Chqpen.Conban  WITH ' ';
        		    		 Chqpen.tipope  WITH '1'      
	            	 ENDIF    	     
		   	         IF f_appd()
				    	 REPLACE ChqPen.CodCtc  WITH vCodCtc;
   					         ChqPen.NumCP   WITH vNumCP;
		    		         ChqPen.NumMes  WITH vNumMes;
			    		     ChqPen.NumChq  WITH vNCheque;
		    	        	 ChqPen.ValChq  WITH vMonto;
		    		         ChqPen.NomGir  WITH vNomGir;
					         ChqPen.CodCad  WITH vCodCad;
	   				         chqpen.periodo WITH vPeriodo;
				             ChqPen.Fecha  WITH vFecha;
	    			         Chqpen.tipdoc  WITH 'CHQ';
				    	     Chqpen.Conban  WITH 'C';
	    			    	 Chqpen.tipope  WITH '2'      
				     ENDIF	     
					 SELECT Detcta
			     	 IF RLOCK()
						 REPLACE Conban WITH '*'
					 ENDIF	 
				 
			    ENDIF	
			ENDIF    
			
		ELSE    &&Si encuentra en chqcert
	
			    SELE chqcert
	    	 	IF RLOCK()
    	    	    REPLACE Conban WITH '*'
	    	    ENDIF    
		ENDIF	
		SELECT Detcta
	ENDSCAN
	SELECT chqcert
	SET INDEX TO 
	SELECT Cheque
	SET ORDER TO Cheque1
	SELECT hchqpen
	SET INDEX TO
DO Valida10
RETURN

PROCEDURE valida10  &&Chq.Certificados - Chq.Certificados pendientes
*----------------

	@0,30 SAY 'Cer-Pen' COLOR W+/BR*
	SELECT HChqPen
	vInd1 = SYS(3) + '.idx'
	INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind1) FOR codctc = vcuenta AND tipdoc='CHQ' AND tipdoc='C/O'

	SELECT chqcert
	vInd2 = SYS(3) + '.idx'
	INDEX ON CodCtc+ALLTRIM(EliAlf2(NumChq)) To (vind2) FOR codctc = vcuenta AND EMPTY(conban)
	GO TOP
	Vcodctc = codctc
	vnumchq = ALLTRIM(ELIALF2(Numchq))
	vnummes = nummes 
	SCAN
		Vcodctc = codctc
		vnumchq = ALLTRIM(ELIALF2(Numchq))
		vnummes = nummes 
		SELECT HChqpen
		SEEK vcodctc+vNumchq
			IF FOUND()
			   SCAN WHILE Hchqpen.codctc+ALLT(Hchqpen.numchq)=vcodctc+vnumchq
				   	IF RLOCK()
    	     			REPLACE HChqpen.conban  WITH '-'
		         		REPLACE HChqpen.MesReb  WITH vnumMes
			         ENDIF	
			   ENDSCAN		
   			   SELECT chqcert  &&Concilia en el Extracto
		       IF RLOCK()
    		       REPLACE Conban WITH '*'
    		   ENDIF    
	        ENDIF     
		    SELE chqcert
	ENDSCAN
	SELECT chqcert
	SET INDEX TO 
	SELECT hchqpen
	SET INDEX TO
IF vflag
   DO actualiza
   DO gra_conc
ENDIF
RETURN


PROCEDURE ExiCon
*---------------
IF LASTKEY()=27 OR okcancel = 2
	RETURN .T.
ENDIF
DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5
SELECT Caja
vOrd = ORDER()
SET ORDER TO Cajas1
SEEK vCuenta
IF FOUND()
	vf1 = IIF(ALLT(Caja.Clase)='A',1,0)
	vf2 = IIF(EMPTY(ALLT(caja.codscta)),1,0)
	vMancom=Caja.Ctaman
	IF VAL(Caja.percie+caja.mescie)>=VAL(vperiodo+vmes)
		vFlag=.F.
		RETURN .T.
	ENDIF
	
*	IF (VAL(Caja.Mescon)<=VAL(vmes)  AND VAL(caja.mescie)+1=VAL(vmes))

	IF ((VAL(Caja.Mescon)<=VAL(vmes)  AND VAL(caja.mescie)+1=VAL(vmes))) OR ((VAL(caja.mescie)=12 AND VAL(Caja.percie)+1=VAL(vperiodo)))
		DO STANDBY WITH 'Importante: Ultimo mes de cierre de la Cta. '+Vcuenta+' -->'+IIF(EMPTY(Caja.mescie),'Vac¡o',VAL_PARA(caja.mescie,'FECMES','D',19,9))
		IF YesNo('¨ Procesar conciliaci¢n ?') .AND. vMancom<>'S'
			OK=VE_PASSW('OPECB')
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Procesando Conciliaci¢n....." COLOR W+/BR*
			
			IF OK AND LASTKEY()#27
				SELECT Chqpen
				@0,30 SAY PROPER(ALIAS()) COLOR W+/BR*
				SET FILT TO (Periodo <= vperiodo ) AND Codctc = vCuenta
				GO TOP
				SCAN
					IF RLOCK()
						DELETE NEXT 1
					ENDIF
					UNLOCK
				ENDSCAN
				SET FILT TO
				
				SELECT conban
				@0,30 SAY PROPER(ALIAS()) COLOR W+/BR*
				SET FILT TO Percon = vperiodo AND Codctc = vCuenta  AND mescon = Vmes
				GO TOP
				SCAN
					IF RLOCK()
						DELETE NEXT 1
					ENDIF
					UNLOCK
				ENDSCAN
				SET FILT TO
				
				SELECT HChqpen
				@0,30 SAY PROPER(ALIAS()) COLOR W+/BR*
				SET FILT TO (Periodo <= vperiodo) AND Codctc = vCuenta AND VAL(mesreb)=VAL(vmes)
				GO TOP
				SCAN
				   IF RLOCK()
				      REPLACE conban WITH IIF(conban='E','I',IIF((conban='-' AND tipdoc='N/C'),'+',IIF((conban='-' AND (tipdoc='N/A' OR tipdoc='B/D')),'|',' ')))
				      REPLACE mesreb WITH '  '
				   ENDIF
				   UNLOCK
				ENDSCAN
				SET FILT TO
				
				SELECT movbco
				@0,30 SAY PROPER(ALIAS())+SPACE(1) COLOR W+/BR*
				SET FILT TO ALLTRIM(codctc)=vcuenta AND VAL(Nummes)>=VAL(vmes) AND !EMPTY(conban)
				GO TOP
				SCAN
					IF RLOCK()
						REPLACE conban WITH ' '
					ENDIF
					UNLOCK
				ENDSCAN
				SET FILT TO
				
				SELECT Detcta
				
				@0,30 SAY PROPER(ALIAS())+SPACE(1) COLOR W+/BR*
				
				SET FILT TO ALLTRIM(codctc)=vcuenta AND VAL(mes)>=VAL(vmes) AND !EMPTY(conban)
				
				GO TOP
				SCAN
					IF RLOCK()
						REPLACE conban WITH ' '
					ENDIF
					UNLOCK
				ENDSCAN
				SET FILT TO
				
				SELECT Cheque
				@0,30 SAY PROPER(ALIAS())+SPACE(1) COLOR W+/BR*    
				SET FILT TO  ALLTRIM(codctc)=vcuenta AND VAL(nummes)>=VAL(vmes) AND !EMPTY(conban)
				GO TOP
				SCAN
					IF RLOCK()
						REPLACE conban WITH ' '
					ENDIF
					UNLOCK
				ENDSCAN
				SET FILT TO
				
				SELECT ChqCert
				@0,30 SAY PROPER(ALIAS()) COLOR W+/BR*
				SET FILT TO  ALLTRIM(codctc)=vcuenta AND VAL(nummes)>=VAL(vmes) AND !EMPTY(conban)
				GO TOP
				SCAN
				   IF RLOCK()
				   	  REPLACE conban WITH ' '
				   ENDIF
				   UNLOCK
				ENDSCAN
				SET FILT TO
				SELECT Caja
				SEEK vCuenta
				IF RLOCK()
					REPLACE Mescon WITH vmes
				ENDIF	
				vflag = .T.
				RETURN .T.
			 ELSE
				vflag = .F.		
				RETURN .T.
			ENDIF	
		ELSE
			vflag = .F.		
			RETURN .T.
	 	ENDIF
	 ELSE
		DO STANDBY WITH "Conciliaci¢n ya fue realizada o Falta conciliaci¢n mes anterior" 
		vflag = .F.	 	
		RETURN .T.
	ENDIF	
ELSE
	RETURN .T.
ENDIF

PROCEDURE Menu_04a
*------------------
PRIVATE choice
Choice  = BAR()
SELECT ChqPen
vTiplis = 2
vCodcta = vcuenta
xmes    = vmes
xper    = STR(YEAR(m.Fecsis),4)

DO CASE

	CASE choice = 1      && CHEQUES PENDIENTES DE PAGO
		DEFINE WINDOW Wind_5 FROM 7,5 TO 19,75  TITLE " Listado de Cheques Pendientes " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY " Tipo Listado : " GET vTiplis FUNCTION "^ General;Por Cta.Cte."
		@ 5, 1 SAY "     Cta.Cte. : " GET vCodcta WHEN vtiplis=2 PICTURE '@!';
				 VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,5,37,'descri',27)
		@ 7, 1 SAY "          Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',19,9,1)
		@ 7,30 GET xper PICTURE '!!!!'
		@ 9,20 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE VALID EXI()
		xmes = ALLTRIM(xmes)
		vper = RIGHT(xPer,2)
		
		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		IF vflag 
			SELE chqpen
			Vind3=SYS(3)+'.IDX'
			IF vTipLis=2
				INDEX ON fecha TO (Vind3) FOR (Periodo = vPer OR Periodo = '06');
				               AND Codctc = vCodcta AND conban=' ' AND TIPDOC='CHQ'
			ELSE
				INDEX ON fecha TO (Vind3) FOR (Periodo = vPer OR Periodo = '06') AND conban=' ' AND ESTADO<>'99'  AND TIPDOC='CHQ'
			ENDIF
			GO TOP
			vTit = 'LISTADO DE CHEQUES PENDIENTES DE PAGO'		
			IF !EOF()			
				DEACTIVATE WINDOW Xwait
				DO REPORTE WITH 2, "LisCon", ' Cheques Pendientes de Pago ',1,.F.,.T.
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ELSE	
			SELE conban
			Vind3=SYS(3)+'.IDX'
			IF vTipLis=2
				INDEX ON fecha TO (Vind3) FOR Percon = vPer AND mescon= xmes;
				               AND Codctc = vCodcta AND conban=' ' AND TIPDOC='CHQ' 
			ELSE
				INDEX ON fecha TO (Vind3) FOR Percon = vPer  AND mescon= xmes AND conban=' ' AND Estado<>'99'  AND tipdoc='CHQ' 
			ENDIF
			GO TOP
			vTit = 'LISTADO DE CHEQUES PENDIENTES DE PAGO'		
			IF !EOF()			
				DEACTIVATE WINDOW Xwait
				DO REPORTE WITH 2, "LisConF", ' Cheques Pendientes de Pago ',1,.F.,.T.
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ENDIF

	CASE choice = 2       && CONCILIACION RESUMIDA
		DEFINE WINDOW Wind_5 FROM 9,5 TO 16,75  TITLE " Conciliaci¢n Resumida " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5
		
		@ 1, 1 SAY "    Cta.Cte. : " GET vCodcta PICTURE '@!';
   	    		VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,1,35,'descri',27)
		@ 2, 1 SAY "         Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',18,9,1)
		@ 2,29 GET xper PICTURE '!!!!'
		@ 4,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()
		xmes = ALLTRIM(xmes)
		vper = RIGHT(xPer,2)
		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		IF vflag 
			vInd = SYS(3) + '.Idx'
			SELECT CHQPEN
			INDEX ON TipOpe+Tipdoc+periodo+numcp TO (vInd) FOR  ALLT(CODCTC)=vcodcta AND CONBAN $ 'CX+ I><|' AND ESTADO<>'99'
			GO TOP
			IF !EOF() .AND. vMancom<>'S'
				IF Yesno("Formal ?")
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon3b", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ELSE
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon3a", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ENDIF
			ELSE
        			SELECT Chqpen
				    IF f_appd()
    	     			REPLACE Chqpen.Codctc WITH vcodcta, ;
         					Chqpen.Nummes WITH xmes, ;
	         				Chqpen.Numchq WITH ' ',;
    	     				Chqpen.Valchq WITH 0,;
        	 				Chqpen.conban WITH ' ', ;
            	    		chqpen.periodo WITH vPer, ;
                			Chqpen.Tipdoc  WITH 'CHQ';
                			Chqpen.tipope  WITH '1'                		
	                ENDIF		
				DEACTIVATE WINDOW Xwait
				DO REPORTE WITH 2, "LisCon3b", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				DELETE 
			ENDIF
			SET INDEX TO
		ELSE
			vInd = SYS(3) + '.Idx'
			SELECT Conban
			INDEX ON TipOpe+Tipdoc+periodo+NUMMES+numcp TO (vInd) FOR  ALLT(CODCTC)=vcodcta AND CONBAN $ 'CX+ I><|' AND ESTADO<>'99' AND percon=vper AND mescon=xmes
			GO TOP
			IF !EOF() .AND. vMancom<>'S'			
				IF Yesno("Formal ?")
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon3d", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ELSE
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon3c", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ENDIF
			ELSE
        			SELECT Conban
				    IF f_appd()
    	     			REPLACE conban.Codctc WITH vcodcta, ;
         						conban.Nummes WITH xmes, ;
		         				conban.Numchq WITH ' ',;
	    	     				conban.Valchq WITH 0,;
    	    	 				conban.conban WITH ' ', ;
        	    	    		conban.periodo WITH vPer, ;
            	    			conban.Tipdoc  WITH 'CHQ';
                				conban.tipope  WITH '1';
                				conban.mescon  WITH xmes;
                				conban.percon  WITH vper                		
	                ENDIF		
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon3d", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				DELETE 
			ENDIF
			SET INDEX TO
		ENDIF			

	CASE choice = 3       && CONCILIACION DETALLADA
		DEFINE WINDOW Wind_5 FROM 9,5 TO 16,75  TITLE " Conciliaci¢n Detallada " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY "    Cta.Cte. : " GET vCodcta PICTURE '@!';
   	    		VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,1,35,'descri',27)
		@ 2, 1 SAY "         Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',18,9,1)
		@ 2,29 GET xper PICTURE '!!!!'
		@ 4,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()

		vper    = RIGHT(xPer,2)

		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		IF vflag 
			vInd = SYS(3) + '.Idx'
			SELECT CHQPEN
			INDEX ON TipOpe+Tipdoc+periodo+NUMMES+numcp TO (vInd) FOR ALLT(CODCTC)=vcodcta  ;
							AND Periodo <= vPer AND CONBAN $ 'C+ I><|' AND ESTADO<>'99'
			GO TOP
			IF !EOF() .AND. vMancom<>'S'
				IF Yesno("Formal ?")
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon4B", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.

				ELSE
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon4A", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ENDIF
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ELSE
			vInd = SYS(3) + '.Idx'
			SELECT Conban
			INDEX ON TipOpe+Tipdoc+periodo+NUMMES+numcp TO (vInd) FOR ALLT(CODCTC)=vcodcta  ;
							AND CONBAN $ 'C+ I><|'  AND ESTADO<>'99' AND percon=vper AND mescon=xmes
			GO TOP
			IF !EOF() .AND. vMancom<>'S'
				IF Yesno("Formal ?")
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon4d", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ELSE
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCon4c", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
				ENDIF
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ENDIF	
				
	CASE choice = 4       && DOCUMENTOS POR REGULARIZAR
		DEFINE WINDOW Wind_5 FROM 9,5 TO 15,75  TITLE " Listado Documentos x Regularizar " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY "        Cta.Cte. : " GET vCodcta PICTURE '@!';
		        VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,1,39,'descri',27)
		@ 3,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()
		
		vper = RIGHT(STR(YEAR(DATE()),4),2)
		
		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		IF vflag 
			SELE chqpen
			vInd3= SYS(3) + '.Idx'
			INDEX ON fecha TO (Vind3) FOR  (Periodo = vPer OR periodo='06') AND Codctc = vCodcta and conban='X'
			GO TOP
			IF !EOF()
				DEACTIVATE WINDOW Xwait
				DO REPORTE WITH 2, "LisCon2", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ELSE
			vInd3= SYS(3) + '.Idx'
			SELE conban
			INDEX ON fecha TO (Vind3) FOR  Percon = vPer  AND Codctc = vCodcta and conban='X' AND mescon=xmes
			GO TOP
			IF !EOF()
				DEACTIVATE WINDOW Xwait
				DO REPORTE WITH 2, "LisCon2f", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ENDIF
		
	CASE choice = 5       && documentos DEVENGADOS
		DEFINE WINDOW Wind_5 FROM 7,5 TO 18,75 TITLE " Listado de Cheques Devengados " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY " Tipo de Listado : " GET vTiplis FUNCTION "^ General;Por Cta.Cte."
		@ 4, 1 SAY "        Cta.Cte. : " GET vCodcta WHEN vtiplis=2 PICTURE '@!';
			VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,4,39,'descri',27)
		@ 6, 1 SAY "             Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',22,9,1)
		@ 6,32 GET vper PICTURE '!!'
		@ 8,20 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()
		
		RELEASE WINDOW Wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		vind3=SYS(3)+'.IDX'
		SELE hchqpen
		IF vTiplis = 2
			INDEX ON fecha TO (VIND3) FOR Mesreb = xmes  AND Codctc = vCodcta and conban='-'
		ELSE
			INDEX ON fecha TO (VIND3) FOR Mesreb = xmes  AND conban='-'
		ENDIF
		GO TOP
		vTit = 'LISTADO DE DOCUMENTOS DEVENGADOS'
		IF !EOF()			
			DEACTIVATE WINDOW Xwait
			DO REPORTE WITH 2, "LisDeVen", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
		ELSE
			DO STANDBY WITH 'No hay datos...'
			DEACTIVATE WINDOW Xwait
		ENDIF
		SET INDEX TO

	CASE choice = 6      && Cierre de Conciliaci¢n 'OK'
		DEFINE WINDOW Wind_5 FROM 7,5 TO 19,75  TITLE " Cierre de Conciliaci¢n " FLOAT DOUBLE COLOR SCHEME 5
		DO STANDBY WITH 'Este proceso s¢lo debe hacerse si la conciliaci¢n est  OK'
		OK=VE_PASSW('CLOCB')
		IF OK AND LASTKEY()#27
			ACTIVATE WINDOW wind_5
			@ 5, 1 SAY "     Cta.Cte. : "
			@ 5,20 SAY vCodcta  PICTURE '@!'
			@ 7, 1 SAY "          Mes : "
*			@ 7,20 SAY xmes PICTURE '!!'
*			@ 7,30 SAY vper PICTURE '!!'
			@ 7,20 SAY VMES PICTURE '!!'
			@ 7,30 SAY VPERIODO PICTURE '!!'
			@ 9,20 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
			
			READ CYCLE VALID EXI()
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Procesando cierre de conciliaci¢n...." COLOR W+/BR*
			xmes = ALLTRIM(vmes)
			vper=vperiodo
			SELE Caja
			SEEK vcodcta  &&Cheque cierre de Conciliaci¢n
			
*		    IF VAL(Caja.percie+caja.mescie)+1=VAL(vper+xmes)
			IF (VAL(Caja.percie)=VAL(vper) AND VAL(caja.mescie)+1=VAL(xmes)) OR (VAL(Caja.percie)+1=VAL(vper) AND VAL(caja.mescie)=12)
				RELEASE WINDOW wind_5
				IF LASTKEY() = 27 OR okcancel = 2
					RETURN .T.
				ENDIF
				DO cie_conc
				SELE caja
				SEEK vcodcta
				IF FOUND()
					IF RLOCK()
						REPLACE mescon WITH xmes;
								mescie WITH xmes;
								percie WITH vper
					ENDIF
					UNLOCK
				ENDIF
			ELSE
				DO stanDby WITH 'Mes de conciliaci¢n ya fue cerrado o falta mes anterior'
				DEACTIVATE WINDOW wind_5
			ENDIF
		ENDIF
		DEACTIVATE WINDOW Xwait
		SELE conban
ENDCASE
RETURN


PROCEDURE actualiza
*-------------------
@0,30 SAY 'Grabando' COLOR W+/BR*
SELE hchqpen
SET FILTER TO (Periodo <= vPeriodo) AND Codctc = vCuenta;
					 AND conban$'+ I|X><CE'  AND ESTADO<>'99'   
					 
	GO TOP
     vNumCP   = Hchqpen.NumCP
     vNumMes  = hChqpen.NumMes
     vNCheque = Hchqpen.NumChq
	 xCodCtc  = Hchqpen.CodCtc
     vFecha   = Hchqpen.Fecha 
     vMonto   = Hchqpen.ValChq
     vNomGir  = Hchqpen.NomGir
     vCodCad  = Hchqpen.CodCad
     xperiodo = Hchqpen.periodo
     vfecha   = Hchqpen.fecha
     vtipdoc  = Hchqpen.tipdoc
     vconban  = hchqpen.conban
     vtipope  = hchqpen.tipope
     Vtipo    = hchqpen.tipo
	IF !EOF()
	   SCAN
	     vNumCP   = Hchqpen.NumCP
	     vNumMes  = hChqpen.NumMes
	     vNCheque = Hchqpen.NumChq
		 xCodCtc  = Hchqpen.CodCtc
	     vFecha   = Hchqpen.Fecha  
	     vMonto   = Hchqpen.ValChq
	     vNomGir  = Hchqpen.NomGir
	     vCodCad  = Hchqpen.CodCad
	     xperiodo = Hchqpen.periodo
	     vfecha   = Hchqpen.fecha
	     vtipdoc  = Hchqpen.tipdoc
	     vconban  = hchqpen.conban
	     vtipope  = hchqpen.tipope
	     vtipo    = hchqpen.tipo
	      SELE chqpen
	      IF f_appd()
           	 REPLACE    chqPen.NumCP   WITH vnumcp;
	            	    ChqPen.NumMes  WITH vNumMes;
		                ChqPen.NumChq  WITH vNcheque;
   			            ChqPen.ValChq  WITH vmonto;
	        	        ChqPen.NomGir  WITH vNomGir;
    		            ChqPen.CodCad  WITH vCodCad;
    		            ChqPen.CodCtc  WITH xCodCtc;
	    	            chqpen.periodo WITH xPeriodo;
       			        ChqPen.Fecha   WITH vFecha;
		                Chqpen.tipdoc  WITH vtipdoc;
   	    		        Chqpen.Conban  WITH vconban;
	        	        Chqpen.tipope  WITH vtipope;      
	        	        Chqpen.tipo    WITH vtipo;      
	        	        Chqpen.flag    WITH 'A'
	      ENDIF
	      UNLOCK
	      SELE Hchqpen
	   ENDSCAN
ENDIF            	        
SET FILT TO
RETURN


PROCEDURE gra_conc  &&Guarda Conciliaci¢n Procesada en Conban
*-----------------
SELE chqpen
SET FILT TO codctc=vcuenta AND estado<>'99'
GO TOP
     vNumCP   = chqpen.NumCP
     vNumMes  = Chqpen.NumMes
     vNCheque = chqpen.NumChq
	 xCodCtc  = chqpen.CodCtc
     vFecha   = chqpen.Fecha 
     vMonto   = chqpen.ValChq
     vNomGir  = chqpen.NomGir
     vCodCad  = chqpen.CodCad
     xperiodo = chqpen.periodo
     vfecha   = chqpen.fecha
     vtipdoc  = chqpen.tipdoc
     vconban  = chqpen.conban
     vtipope  = chqpen.tipope
     Vtipo    = chqpen.tipo
     xflag    = chqpen.flag
	IF !EOF()
	   SCAN
	     vNumCP   = chqpen.NumCP
	     vNumMes  = Chqpen.NumMes
	     vNCheque = chqpen.NumChq
		 xCodCtc  = chqpen.CodCtc
	     vFecha   = chqpen.Fecha  
	     vMonto   = chqpen.ValChq
	     vNomGir  = chqpen.NomGir
	     vCodCad  = chqpen.CodCad
	     xperiodo = chqpen.periodo
	     vfecha   = chqpen.fecha
	     vtipdoc  = chqpen.tipdoc
	     vconban  = chqpen.conban
	     vtipope  = chqpen.tipope
	     vtipo    = chqpen.tipo
	     xflag    = chqpen.flag
	      SELE conban
	      IF f_appd()
           	 REPLACE    conban.NumCP   WITH vnumcp;
	            	    conban.NumMes  WITH vNumMes;
		                conban.NumChq  WITH vNcheque;
   			            conban.ValChq  WITH vmonto;
	        	        conban.NomGir  WITH vNomGir;
    		            conban.CodCad  WITH vCodCad;
    		            conban.CodCtc  WITH xCodCtc;
	    	            conban.periodo WITH xPeriodo;
       			        conban.Fecha   WITH vFecha;
		                conban.tipdoc  WITH vtipdoc;
   	    		        conban.Conban  WITH vconban;
	        	        conban.tipope  WITH vtipope;      
	        	        conban.tipo    WITH vtipo;      
	        	        conban.flag    WITH xflag;
	        	        conban.percon  WITH vperiodo;
	        	        conban.mescon  WITH vmes,;
	        	        conban.usuario WITH wuser_id,;
	        	        conban.fecpro  WITH DATE()
	      ENDIF
	      UNLOCK
	      SELE chqpen
	   ENDSCAN
	   SET FILT TO
	ENDIF            	        
RETURN


PROCEDURE cie_conc   &&Pasa Doc. pendientes al Historico
*-------------------
SELE conban
SET FILTER TO EMPTY(flag) AND percon=vper AND mescon= xmes;
					  AND Codctc = vCodcta
GO TOP
     vNumCP   = ConBan.NumCP
     vNumMes  = ConBan.NumMes
     vNCheque = ConBan.NumChq
	 xCodCtc  = ConBan.CodCtc
     vFecha   = ConBan.Fecha 
     vMonto   = ConBan.ValChq
     vNomGir  = ConBan.NomGir
     vCodCad  = ConBan.CodCad
     xperiodo = ConBan.periodo
     vfecha   = ConBan.fecha
     vtipdoc  = ConBan.tipdoc
     vconban  = ConBan.conban
     vtipope  = ConBan.tipope
     Vtipo    = ConBan.tipo
	IF !EOF()
	   SCAN
	     vNumCP   = ConBan.NumCP
	     vNumMes  = ConBan.NumMes
	     vNCheque = ConBan.NumChq
		 xCodCtc  = ConBan.CodCtc
	     vFecha   = ConBan.Fecha  
	     vMonto   = ConBan.ValChq
	     vNomGir  = ConBan.NomGir
	     vCodCad  = ConBan.CodCad
	     xperiodo = ConBan.periodo
	     vfecha   = ConBan.fecha
	     vtipdoc  = ConBan.tipdoc
	     vconban  = ConBan.conban
	     vtipope  = ConBan.tipope
	     vtipo    = ConBan.tipo
	      SELE hchqpen
	      IF f_appd()
           	 REPLACE    hchqpen.NumCP   WITH vnumcp;
	            	    hchqpen.NumMes  WITH vNumMes;
		                hchqpen.NumChq  WITH vNcheque;
   			            hchqpen.ValChq  WITH vmonto;
	        	        hchqpen.NomGir  WITH vNomGir;
    		            hchqpen.CodCad  WITH vCodCad;
    		            hchqpen.CodCtc  WITH xCodCtc;
	    	            hchqpen.periodo WITH xPeriodo;
       			        hchqpen.Fecha   WITH vFecha;
		                hchqpen.tipdoc  WITH vtipdoc;
   	    		        hchqpen.Conban  WITH vconban;
	        	        hchqpen.tipope  WITH vtipope;      
	        	        hchqpen.tipo    WITH vtipo      
	      ENDIF
	      UNLOCK
	      SELE ConBan
	   ENDSCAN
ENDIF      
SET FILT TO
SELE chqpen
SET FILT TO codctc= vcodcta
GO TOP
SCAN
	 IF RLOCK()
	     DELETE NEXT 1
     ENDIF
	 UNLOCK
ENDSCAN
SET FILT TO

SELE conban
RETURN

FUNCTION SalExt     && funci¢n utilizada en LisCon2.frx
*----------------
PARAMETERS Mmes
SET EXACT ON
vSel = ALIAS()
SELECT BcoCta
SET ORDER TO Bcocta2
SEEK ALLTRIM(Mmes)+ALLTRIM(vCuenta)
	if found()
		vSaldo = Bcocta.Salfin
	else
		vsaldo = 0
	endif
SELECT (vSel)
RETURN VSALDO

FUNCTION Salbcos     && funci¢n utilizada en LisCon2.frx
*----------------
PARAMETERS a
vSel = ALIAS()
*IF a=1
   SELECT Salbcos
   SEEK vCodcta+xPer+ALLTRIM(xmes)
*ELSE   
*   SELECT HSalbcos
*   SEEK vCodcta+vPer+ALLTRIM(xmes)
*ENDIF   
IF FOUND()
	vSaldo = Saldeb-Salhab
ELSE
	vsaldo = 0
ENDIF
SELECT (vSel)
RETURN vSaldo

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.

FUNCTION Descri
*--------------
vdes = IIF(conban='+','N/C pendientes de afectacion presup.', ;
	   IIF(conban='I','Cargos indebidos', ;
	   IIF(conban=' ','Cheq. pendientes de pago', ; 
	   IIF(conban='<','Pago menos por el Banco',  ;
	   IIF(Conban='>','Pago de mas x el Banco',   ;
	   IIF(Conban='|' AND (Tipdoc='B/D' OR Tipdoc = 'N/A'), ;
	   'Dep.no contab. x falta doc.', ;
	   IIF(Conban='C','Cheques certificados pendientes ', ;
	   IIF(Conban='A','Cheques certificados Anulados ', ;
	   IIF(Conban='','Abonos pendientes por el Banco ', ;
	   IIF(Conban='','Cargos pendientes por el Banco ', ;
	   ''))))))))))
	   
RETURN vdes
