* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ T4.Prg  22/08/96                             	    L: 1779  ³	
* ³ Proceso de Elaboraci¢n del T4				                 ³
* ³ AUTORES :  Julio Cruz Ortiz  (Versi¢n Inicial)   		     ³
* ³            Ing. Federico Montero Valdiviezo REGION GRAU      ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.   07/1997   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾


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
*USE ConBan1   IN 7 									   ALIAS ConBan
USE T4 		 IN 13 									   ALIAS t4

*-Ambiente

vExact = SET('EXACT')
SET EXACT ON

*- Declara variables

PUBLIC vCodCtc, vNCheque, vMonto, vCuenta, vNumMes, vMes, xFecha, vMancom;
		vperiodo, vflag, vtit, vRev, xmes, vper, vcodcta,vF1,vF2,Vuser_id,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')

STORE SPACE(14) TO vCodCtc, vNCheque, vCuenta, vNumMes
STORE 0 TO vMonto,vf1,vf2
STORE SPACE(40) TO vConcept
VMes     = MONTH(DATE())
IF VMES<10
	VMES  ='0'+ALLTRIM(STR(MONTH(DATE())))
ELSE
    VMES=STR(VMES,2)   
ENDIF   
vperiodo = RIGHT(STR(YEAR(DATE())),2)
VFLAG =  .T.

*- Definici¢n de ventanas
DEFINE WINDOW VerSal FROM 04,16 TO 20,79 ;
COLOR SCHEME 10 TITLE "®F8¯ Eliminar C/P que tienen H/A en el T-4 ®F9¯ Recuperar H/A del T-4 ®F10¯ para salir"

DEFINE WINDOW _verDif FROM 14,35 TO 21,79 COLOR SCHEME 10 TITLE " Reporte T-4 " DOUBLE


ACTIVATE WINDOW _VerDif
SELECT Caja
@ 1, 1   SAY "   Fec.Conc.: " GET vMes    PICTURE '!!' VALID Val_Para(vMes,'FECMES',' ',19,9)
@ 1,19   GET vPeriodo PICTURE '!!'
@ 2, 1   SAY "      Cuenta: " GET vCuenta PICTURE '!!!!!!!!!!!!!!';
        VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCuenta,1,3,16,'descri',27)
@ 4, 7   GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 15
READ CYCLE VALID ExiCon()

 RELEASE WINDOW _VerDif
vmes = ALLTRIM(vmes)

IF LASTKEY()=27 OR Okcancel = 2
   RELEASE vCodCtc, vNCheque, vMonto, vCuenta, vNumMes, vMes, xFecha, vperiodo, vflag, vRev;
   			, xmes, vper, vcodcta
   CLOSE DATA
   DEACTIVATE WINDOW  xwait
   RETURN
ENDIF


vInd = SYS(3) + '.idx'

IF Vflag

   DO GRA_T4

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
		@ 2, 0 SAY PADC(" POR REGULARIZAR. HAY DOCUMENTOS ",41,' ')  COLOR W+/N*
		@ 3, 0 SAY PADC(" NO VALIDOS",41,' ')  COLOR W+/N*
		@ 4, 0 SAY SPACE(41)  COLOR W+/N*
		@ 5, 0 SAY SPACE(41)  COLOR W+/N*
		@ 5,15 GET okcancel FUNCTION '*TH \!\<OK' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 3
		@ 6, 0 SAY SPACE(41)  COLOR W+/N*
		READ
		DEACTIVATE WINDOW Ywait
	ENDIF
ENDIF

DEACTIVATE WINDOW Xwait

*- T4
IF escolor
   DEFINE POPUP POP_04a FROM 10,56 SHADOW COLOR &l_col
ELSE
   DEFINE POPUP POP_04a FROM 10,56 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1  OF  POP_04a PROMPT 'Reporte T4  '   MESSAGE ''
DEFINE BAR 2  OF  POP_04a PROMPT 'H/A procesadas '   MESSAGE ''
*DEFINE BAR 3  OF  POP_04a PROMPT 'Concil. Movimiento Acumulado '   MESSAGE ''
ON SELECTION POPUP pop_04a  DO  menu_04a
ACTIVATE POPUP pop_04a

CLOSE DATA
ERASE (vInd)

SET EXACT OFF

RELEASE vCodCtc, vNCheque, vMonto, vCuenta, vNumMes, vMes, xFecha, vperiodo, vflag, vRev;
		, xmes, vper, vcodcta

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
*	VSCTA=Caja.CODSCTA
	vMancom=Caja.Ctaman
	IF VAL(Caja.percie+caja.mescie)>=VAL(vperiodo+vmes)
        vFlag=.F.
        RETURN .T.
    ENDIF    
		IF (VAL(Caja.Mescon)<=VAL(vmes)  AND VAL(caja.mescie)+1=VAL(vmes)) 
		*OR (ALLTRIM(vmes)='06' AND ALLTRIM(vperiodo)='97')
		   DO STANDBY WITH 'Importante: Ultimo mes de cierre de la Cta. '+Vcuenta+' -->'+IIF(EMPTY(Caja.mescie),'Vac¡o',VAL_PARA(caja.mescie,'FECMES','D',19,9))
		 	
    	   IF YesNo('¨ Procesar T4 ?') .AND. vMancom<>'S'
		   	  OK=VE_PASSW('OPET4')
				ACTIVATE WINDOW Xwait
				@0,0 SAY " Procesando T4 ...." COLOR W+/BR*
				
				IF OK AND LASTKEY()#27 

					SELECT T4
					@0,30 SAY PROPER(ALIAS()) COLOR W+/BR*
		            SET FILT TO Percon = vperiodo  ;
						  AND Codctc = vCuenta  AND mescon = Vmes
					GO TOP
					SCAN
					   IF RLOCK()
					      DELETE NEXT 1
					   ENDIF
					   UNLOCK
					ENDSCAN
					SET FILT TO
				      			
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
		DO STANDBY WITH "T4 ya fue realizada o Falta T4 del mes anterior" 
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
SELECT CONBAN
vTiplis = 2
vCodcta = vcuenta
xmes    = vmes
vper    = RIGHT(STR(YEAR(DATE())),2)

DO CASE

	CASE choice = 1       && REPORTE T4
		DEFINE WINDOW Wind_5 FROM 9,5 TO 16,75  TITLE " Reporte del T-4 " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY "    Cta.Cte. : " GET vCodcta PICTURE '!!!!!!!!!!!!!!';
   	    		VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,1,32,'descri',27)
		@ 2, 1 SAY "         Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',18,9,1)
		@ 2,29 GET vper PICTURE '!!'
		@ 4,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()

		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		IF vflag 
			*vInd = SYS(3) + '.Idx'
			SELECT t4
			*INDEX ON TipOpe+Tipdoc+periodo+NUMMES+numcp TO (vInd) FOR ALLT(CODCTC)=vcodcta  ;
			*				AND CONBAN $ 'C+ I><|'  AND ESTADO<>'99'
			GO TOP
			IF !EOF() .AND. vMancom<>'S'
				DEACTIVATE WINDOW Xwait
				IF VCODCTA='631018599' .OR. VCODCTA='631018017'
					DO REPORTE WITH 2, "LisT41", ' Reporte T4 ',1,.F.,.T.
				ENDIF
				IF VCODCTA='631019218'
					DO REPORTE WITH 2, "LisT42", ' Reporte T4 ',1,.F.,.T.
				ENDIF
					
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ELSE
			vInd = SYS(3) + '.Idx'
			SELECT T4
			INDEX ON TipOpe+Tipdoc+periodo+NUMMES+numcp TO (vInd) FOR ALLT(CODCTC)=vcodcta  ;
							AND CONBAN $ 'C+ I><|'  AND ESTADO<>'99' AND percon=vper AND mescon=xmes
			GO TOP
			IF !EOF() .AND. vMancom<>'S'
				DEACTIVATE WINDOW Xwait
				IF VCODCTA='631018599' .OR. VCODCTA='631018017'
					DO REPORTE WITH 2, "LisT41", ' Reporte T4 ',1,.F.,.T.
				ENDIF
				IF VCODCTA='631019218'
					DO REPORTE WITH 2, "LisT42", ' Reporte T4 ',1,.F.,.T.
				ENDIF
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ENDIF	
				
	CASE choice = 2       && 
		DEFINE WINDOW Wind_5 FROM 9,5 TO 16,75  TITLE " C/P con H/A " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY "    Cta.Cte. : " GET vCodcta PICTURE '!!!!!!!!!!!!!!';
   	    		VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,1,32,'descri',27)
		@ 2, 1 SAY "         Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',18,9,1)
		@ 2,29 GET vper PICTURE '!!'
		@ 4,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()
		
		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		SELECT t4
		GO TOP
	    IF !EOF()
	       DO rebaja_ha
	    ELSE
	       DO standby WITH 'No existen registros'
	    ENDIF
	   
 CASE choice = 3       && MOVIMIENTO ACUMULADO
		DEFINE WINDOW Wind_5 FROM 9,5 TO 16,75  TITLE " Conciliaci¢n Movimiento Acumulado " FLOAT DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW wind_5

		@ 1, 1 SAY "    Cta.Cte. : " GET vCodcta PICTURE '!!!!!!!!!!!!!!';
   	    		VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodcta,1,1,32,'descri',27)
		@ 2, 1 SAY "         Mes : " GET xmes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',18,9,1)
		@ 2,29 GET vper PICTURE '!!'
		@ 4,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
		READ CYCLE  VALID EXI()

		RELEASE WINDOW wind_5
		IF LASTKEY() = 27 OR okcancel = 2
			RETURN
		ENDIF
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Procesando reporte...." COLOR W+/BR*
		IF vflag 
			*vInd = SYS(3) + '.Idx'
			SELECT salbcos
			vOrd1 = ORDER()
			SET ORDER TO SALBCOS3
			SELE BCOCTA
			vOrd2 = ORDER()
			SET ORDER TO BCOCTA3
			SELE SALBCOS
			SET RELATION TO CODCTC INTO BCOCTA ADDITIVE
			*SET FILT TO codctc=vCODCTA .AND. NUMMES<=XMES .AND. NUMMES<>'00'
			*GO TOP
			IF !EOF()
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCMA", ' Reporte T4 ',1,.F.,.T.
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ELSE
			SELECT salbcos
			vOrd1 = ORDER()
			SET ORDER TO SALBCOS3
			SELE BCOCTA
			vOrd2 = ORDER()
			SET ORDER TO BCOCTA3
			SELE SALBCOS
			SET RELATION TO CODCTC INTO BCOCTA ADDITIVE
			SET FILT TO codctc=vCODCTA .AND. NUMMES<=XMES .AND. NUMMES<>'00'
			GO TOP
			IF !EOF() 
					DEACTIVATE WINDOW Xwait
					DO REPORTE WITH 2, "LisCMA", ' Reporte Conciliaci¢n Bancaria ',1,.F.,.T.
			ELSE
				DO STANDBY WITH 'No hay datos...'
				DEACTIVATE WINDOW Xwait
			ENDIF
			SET INDEX TO
		ENDIF	

		SELE SALBCOS
		SET ORDER TO VORD1
		SET RELATION TO 
		SET FILT TO
		SELE BCOCTA
		SET ORDER TO VORD2
			    
	    SET INDEX TO
		SET FILTER TO
		RELEASE WINDOW Versal
		DEACTIVATE WINDOW Xwait
		SELE T4
ENDCASE
RETURN


PROCEDURE gra_T4  &&Guarda T4 
*---------------
SELE conban

SET FILT TO codctc=vcuenta .AND. mescon=vmes .AND. estado<>'99' 
GO TOP
     vNumCP   = conban.NumCP
     vNumMes  = conban.NumMes
     vNCheque = conban.NumChq
	 xCodCtc  = conban.CodCtc
     vFecha   = conban.Fecha 
     vMonto   = conban.ValChq
     vNomGir  = conban.NomGir
     vCodCad  = conban.CodCad
     xperiodo = conban.periodo
     vfecha   = conban.fecha
     vtipdoc  = conban.tipdoc
     vconban  = conban.conban
     vtipope  = conban.tipope
     Vtipo    = conban.tipo
     xflag    = conban.flag
	IF !EOF()
	   SCAN
	     vNumCP   = conban.NumCP
	     vNumMes  = conban.NumMes
	     vNCheque = conban.NumChq
		 xCodCtc  = conban.CodCtc
	     vFecha   = conban.Fecha  
	     vMonto   = conban.ValChq
	     vNomGir  = conban.NomGir
	     vCodCad  = conban.CodCad
	     xperiodo = conban.periodo
	     vfecha   = conban.fecha
	     vtipdoc  = conban.tipdoc
	     vconban  = conban.conban
	     vtipope  = conban.tipope
	     vtipo    = conban.tipo
	     xflag    = conban.flag
	      SELE T4
	      IF f_appd()
           	 REPLACE    T4.NumCP   WITH vnumcp;
	            	    T4.NumMes  WITH vNumMes;
		                T4.NumChq  WITH vNcheque;
   			            T4.ValChq  WITH vmonto;
	        	        T4.NomGir  WITH vNomGir;
    		            T4.CodCad  WITH vCodCad;
    		            T4.CodCtc  WITH xCodCtc;
	    	            T4.periodo WITH xPeriodo;
       			        T4.Fecha   WITH vFecha;
		                T4.tipdoc  WITH vtipdoc;
   	    		        T4.Conban  WITH vconban;
	        	        T4.tipope  WITH vtipope;      
	        	        T4.tipo    WITH vtipo;      
	        	        T4.flag    WITH xflag;
	        	        T4.percon  WITH vperiodo;
	        	        T4.mescon  WITH vmes,;
	        	        T4.usuario WITH wuser_id,;
	        	        T4.fecpro  WITH DATE()
	      ENDIF
	      UNLOCK
	      SELE CONBAN
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
   SEEK vCodcta+vPer+ALLTRIM(xmes)
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

PROCEDURE rebaja_ha
*-------------------
  ON KEY LABEL F8  DO borra
  ON KEY LABEL F9  DO desborra
  ON KEY LABEL F10 KEYBOARD CHR(23)
  DEACTIVATE WINDOW xwait  
  ACTIVATE WINDOW versal
  SELE T4
  set filter to codctc=vcodcta and !empty(numha)
  GO TOP
  BROWSE NOEDIT NOAPPEND FIELDS ;
       nummes :R,;
       numcp  :R,;
       numchq :R,;
       codctc :R,;
       nomgir :R,;
       valchq :R,;
       fecha  :R ;
	  WINDOW VerSal 
  ON KEY LABEL F8
  ON KEY LABEL F9
  ON KEY LABEL F10 
  SELE T4
RETURN


PROCEDURE BORRA
*--------------
SELECT T4
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN


PROCEDURE DESBORRA
*-----------------
SELECT T4
IF RLOCK()
	RECALL NEXT 1
ENDIF
RETURN



PROCEDURE S1
*-----------
SELE SALBCOS
SET FILT TO codctc=vCODCTA .AND. NUMMES<=XMES .AND. NUMMES<>'00'
RETURN

PROCEDURE S2
*-----------
SELE BCOCTA
SET FILT TO codctc=vCODCTA .AND. MES<=XMES .AND. MES<>'00'
RETURN