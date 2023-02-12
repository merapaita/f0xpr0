* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ LibBan.Prg  11/08/96                                L: 0515  ³	
* ³ Emisi¢n del Libro Bancos                                     ³
* ³ AUTORES :  Ing. Federico Montero Valdiviezo (Versi¢n Inicial)³
* ³            Julio Cruz Ortiz  REGION GRAU                     ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.     07/1997 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

* Abrir Archivos de trabajo
USE MovBco   IN 1 ORDER TAG MovBco11        ALIAS MovB
USE Compag   IN 2 ORDER TAG Compag1         ALIAS Comp
USE Hojanu   IN 3 ORDER TAG hojanu1		    ALIAS hojanu	
USE Cajas    IN 4 ORDER TAG Cajas1          ALIAS Caja
USE parmae   IN 5 ORDER TAG Parmae1         ALIAS Parma
USE Cheque   IN 6 ORDER TAG Cheque1         ALIAS Cheque
USE itehA    IN 7 ORDER TAG itehA1       	ALIAS itehA 
USE Salbcos  IN 9 ORDER TAG salbcos1 		ALIAS salbco
USE ASTPAT   IN 10 ORDER TAG ASTPAT3	    ALIAS ASTPAT

filaux = SYS(3) + '.DBF'
zInd   = SYS(3) + '.Idx'

SELECT 8
CREATE TABLE (filaux);
  (CodCtc    C(017),;
   FecChq    D(008),;
   TipDoc    C(003),;
   NumChq    C(014),;
   NumREF    C(004),;
   NumMes    C(002),;
   Concept   C(045),;
   Cuenta    C(12),;
   ValChqD   N(18,2),;
   ValChqH   N(18,2))
USE

USE (filaux) IN 8  EXCLUSIVE   ALIAS Temp

** Declaraci¢n de variables

PUBLIC vCodfte,vtip,vsec,vctacte,vtipo,vliscta,vtiplis,xano,rAno,wmes,vlissec,vclase,vcuenta,NCUENTA

STORE 0 TO xano, vMontoD, vMontoH,VSALDO,VSALDEB,VSALHAB,VSALDEBA,VSALHABA,VDEBACU,VHABACU,vsalant,vsalacu,fmes
STORE SPACE(14)  TO vCtaCte
STORE SPACE(2)  TO WMES,ames
STORE ' ' TO vNUMERO, vCodCal, vConcept, vNumCHQ, vNumMes,VTIPDOC
STORE .F. TO vtipo
STORE 1   TO vTiplis

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
IF Escolor
	DEFINE WINDOW LibBan FROM 03,09 TO 18,70  DOUBLE SHADOW COLOR SCHEME 5 
 ELSE
 	DEFINE WINDOW LibBan FROM 03,09 TO 18,70  DOUBLE COLOR SCHEME c_popup
ENDIF
ACTIVATE WINDOW LibBan
vMes     = MONTH(DATE())
xMes     = PADL(MONTH(DATE()),2,'0')

vAno     = YEAR(DATE())
xAno     = STR(YEAR(DATE()),4)

STORE 1 TO vliscta,vlisfte,vlistip,vlissec
STORE 2 TO vTiplis,vMancom
STORE SPACE(1) TO vtip,vvsec,vsec,vvact,vclase
STORE SPACE(12) TO vcuenta,NCUENTA
STORE SPACE(3) TO vcodfte
vvsec = 'N'
vvact = 'N'
SELECT Caja
@ 0, 1 SAY '        Mes y A¤o : ' GET xMes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',22,9) 
@ 0,25 GET xAno PICTURE '!!!!'
@ 2, 1 SAY 'Todas las Cuentas : ' GET vliscta FUNCTION '*RNH Si;No' VALID VALTOD(vliscta,4,20)
@ 4, 1 SAY 'Ctas. Mancomunadas? ' GET vmancom FUNCTION '*RNH Si;No'
@ 6, 1 SAY ' Cuenta Corriente : ' GET vCtaCte WHEN vliscta=2 PICTURE '!!!!!!!!!!!!!!' ; 
			VALID  Val_fun2('Caja','CodCtc','codctc+descri',vCtaCte,1,5,22,'LEFT(DESCRI,38)')  AND Val_Tipo()
@ 8,01 SAY ' Incluye sectores : ' GET vTiplis  WHEN vvsec = 'S'   FUNCTION '*RNH Si;No' 
@12,12 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
READ CYCLE VALID exi()

WMES=PADL(ALLTRIM(XMES),2,'0')
DEACTIVATE WINDOW LibBan

IF LASTKEY()#27 AND okcancel = 1
	DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	ACTIVATE WINDOW Xwait
	@0,0 SAY " Procesando Libro Bancos....." COLOR W+/BR*
	
	IF vliscta = 1
		SELECT M.* ;
  		FROM Cajas C, Movbco M ;
  		WHERE  M.codctc=C.codctc .AND. C.ctaman=iif(vmancom=1,'S','N') .AND. C.sector<>'S';
  		INTO CURSOR movbco1
  		
  	  	SELECT M.* ;
  		FROM Cajas C,Compag M ;
  		WHERE M.codctc=C.codctc .AND. C.ctaman=iif(vmancom=1,'S','N') .AND. C.sector<>'S';
  		INTO CURSOR comp1
  		
  	  	SELECT M.* ;
  		FROM Cajas C,Hojanu M ;
  		WHERE M.codctc=C.codctc .AND. C.ctaman=iif(vmancom=1,'S','N') .AND. C.sector<>'S';
  		INTO CURSOR hojanu1
  		
  		SELECT C.* ;
  		FROM Cajas C ;
  		WHERE C.ctaman=iif(vmancom=1,'S','N') .AND. C.sector<>'S';
  		INTO CURSOR Cajtmp
  		
  	ELSE
  	
  	  	SELECT M.* ;
  		FROM Movbco M ;
   		WHERE CodCtc=vCtaCte  ;
  		INTO CURSOR movbco1
			
  	  	SELECT M.* ;
  		FROM Compag M ;
  		WHERE CodCtc=vCtaCte ;
  		INTO CURSOR comp1
  		
  	  	SELECT M.* ;
  		FROM hojanu M ;
  		WHERE CodCtc=vCtaCte ;
  		INTO CURSOR hojanu1
  		
	  	SELECT Caja
	  	SEEK ALLT(vctacte)
	  	IF FOUND()
  			vcodfte = CodFte
  			vclase  = clase
  			vcuenta = cuentah
	  	ENDIF
	ENDIF
	
    DO Valida
	SELE ASTPAT
	SET ORDER TO ASTPAT3
	
	SELECT temp
	INDEX ON Tipdoc+nummes+numref TO (zInd) FOR IIF(vLisCta=2,CodCtc=vCtaCte,.T.)
	IF vliscta = 2 
		SUM TEMP.valchqd TO VSALDEB
		SUM TEMP.valchqh TO VSALHAB
		
		SELE SALBCO
*		SEEK ALLTRIM(vctacte+rano+wmes)
		SEEK ALLTRIM(vctacte+xano+wmes)
		
		IF FOUND()
			IF RLOCK()
*				REPLACE periodo WITH rano, nummes WITH wmes, codctc WITH vctacte
				REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
				REPLACE saldeb WITH vsaldeb, salhab WITH vsalhab
				UNLOCK
			ENDIF
		 ELSE
			If f_appd()
*				REPLACE periodo WITH rano, nummes WITH wmes, codctc WITH vctacte
				REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
				REPLACE saldeb WITH vsaldeb, salhab WITH vsalhab
			ENDIF
		ENDIF
		
		SKIP -1
		fmes=VAL(ALLTRIM(xmes))-1
		ames=IIF(fmes=0,'12',STR(fmes,2))
		
		IF ALLTRIM(salbco.codctc)=ALLTRIM(vctacte)
			vsaldeba=salbco.saldeb
			vsalhaba=salbco.salhab
			vsalant=vsaldeba-vsalhaba
  			IF ames='12'
  				IF vsalant<=0
  						vsaldeba=0
  	    		   		vsalhaba=ABS(vsalant)
  		    		ELSE
  		    			vsaldeba=vsalant
  		       			vsalhaba=0
		  	    	ENDIF
		  	    ENDIF
		  	    
   				vdebacu=vsaldeb+vsaldeba
	   			vhabacu=vsalhab+vsalhaba
	   			vsalacu=vsaldeb-vsalhab+vsaldeba-vsalhaba
			ELSE
   				vdebacu=vsaldeb
		   		vhabacu=vsalhab
		   		vsalacu=vsaldeb-vsalhab+vsaldeba-vsalhaba
			ENDIF
			
		  	SKIP
    		IF RLOCK()
*		       REPLACE periodo WITH rano, nummes WITH wmes, codctc WITH vctacte
		       REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
	  		   REPLACE saldeb WITH vdebacu, salhab WITH vhabacu
		  	   UNLOCK
	  		ENDIF
  	ELSE
  	
	  SELECT Cajtmp
	  
	  SCAN
	  
	  	STORE 0 TO vsaldeb,vsalhab,vsaldeba,vsalhaba,vdebacu,vhabacu,;
	  				vsalant,vsalacu
	  	vCtacte = Cajtmp.codctc
	 	SELECT TEMP
	 	SUM TEMP.valchqd TO vsaldeb FOR ALLTRIM(Codctc) = ALLTRIM(vctacte)
  		SUM TEMP.Valchqh TO vsalhab FOR ALLTRIM(Codctc) = ALLTRIM(vctacte)
  		
		  	SELE SALBCO
*	  		SEEK ALLTRIM(vctacte+rano+wmes)
	  		SEEK ALLTRIM(vctacte+xano+wmes)
	  		
		  	IF FOUND()
	  		    IF RLOCK()
*	  	    	  REPLACE periodo WITH rano, nummes WITH wmes, codctc WITH vctacte
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
		  	      REPLACE saldeb WITH vsaldeb, salhab WITH vsalhab
	  		      UNLOCK
	  	    	ENDIF
			ELSE
				If f_appd()
*	  	    	  REPLACE periodo WITH rano, nummes WITH wmes, codctc WITH vctacte
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
		  	      REPLACE saldeb WITH vsaldeb, salhab WITH vsalhab
	  		    ENDIF
		  	ENDIF
		  	
		  	SKIP -1
	  		FMES=VAL(XMES)-1
		  	AMES=IIF(FMES=0,'12',STR(FMES,2))
	  		IF ALLTRIM(SALBCO.CODCTC)=ALLTRIM(VCTACTE)
	   			vsaldeba=salbco.salDEB
		   		vsalhaba=salbco.salHAB
	   			vsalant=vsaldeba-vsalhaba
	  			IF AMES='12'
	  	    		IF VSALANT<=0
		  	       		VSALDEBA=0
	  	    	   		VSALHABA=ABS(VSALANT)
	  		    	ELSE
	  	       			VSALDEBA=VSALANT
	  	       			VSALHABA=0
		  	    	ENDIF
	  		 	ENDIF
	   			vdebacu=vsaldeb+vsaldeba
		   		vhabacu=vsalhab+vsalhaba
	   			vsalacu=vsaldeb-vsalhab+vsaldeba-vsalhaba
			ELSE
   				vdebacu=vsaldeb
		   		vhabacu=vsalhab
		   		vsalacu=vsaldeb-vsalhab+vsaldeba-vsalhaba
			ENDIF
			
		  	skip
	    	IF RLOCK()
*		       REPLACE PERIODO WITH rAno, NUMMES WITH WMES, CODCTC WITH VCTACTE
		       REPLACE PERIODO WITH XANO, NUMMES WITH WMES, CODCTC WITH VCTACTE
	  		   REPLACE SALDEB WITH Vdebacu, SALHAB WITH Vhabacu
		  	   UNLOCK
	  		ENDIF
	  		SELECT Cajtmp
	  	  ENDSCAN 
  	ENDIF (vliscta)
	
	SELE TEMP
	GO TOP
	PP = SYS(3) + '.Idx'
	yy = SYS(3) + '.Idx'
	QQ = SYS(3) + '.Dbf'
	deactivate window Xwait
	
  	IF EOF()
		SELE Salbco
  		FMES=VAL(XMES)-1
	  	AMES=IIF(FMES=0,'12',STR(FMES,2))
*  		SEEK ALLTRIM(vctacte+rano+wmes)
  		SEEK ALLTRIM(vctacte+xano+wmes)
* 		SKIP -1
	  	IF ALLTRIM(SALBCO.CODCTC)=ALLTRIM(VCTACTE)
	       COPY NEXT 1 TO (QQ) 
           USE (QQ) IN 0 ALIAS QQ
		   SELE QQ
		   DO REPORTE WITH 2, "salant", ' Reporte Libro Banco ',1,.F.,.T.				
           USE
        ENDIF
        	   
   	ELSE
		DO CASE 
			CASE vtiplis = 2
				INDEX ON CODCTC+CUENTA+DTOC(fecchq)+NumRef  FOR LEFT(numref,1)$'0123456789' AND IIF(vLisCta=2,CodCtc=vCtaCte,.T.) TO (PP)
				DO REPORTE WITH 2, "LibBanI", ' Reporte Libro Banco ',1,.F.,.T.
			CASE vtiplis = 1
				INDEX ON LEFT(NumRef,1)+CODCTC+DTOC(fecchq)+numref AND IIF(vLisCta=2,CodCtc=vCtaCte,.T.) TO (YY)     &&FOR !(LEFT(numref,1)$'0123456789') 
				if vvsec='S' AND VVACT='N'
				   DO REPORTE WITH 2, "LibBanS1", ' Reporte Libro Banco ',1,.F.,.T.
				else
					if vvact='S'
					   DO REPORTE WITH 2, "LibBana1", ' Reporte Libro Banco ',1,.F.,.T.
					endif
				endif					
		ENDCASE
 			
   	ENDIF
    CLOSE DATA
    ERASE (filaux)
  ENDIF
  RETURN


FUNCTION Valida
*--------------
SELECT Movbco1
vInd = SYS(3) + '.Idx'
INDEX ON TIPDOC+NUMMES+NUMERO TO (vInd) FOR 	NUMMESC=WMes
				* AND TIPDOC<>"NOC"  
				*AND ESTADO<>'99'
GO TOP
vCodCtc = Movbco1.CodCtc
vFecha  = Movbco1.FechaC
vTipDoc = Movbco1.TipDoc
vNUMERO = Movbco1.NumERO
vNumMes = Movbco1.NumMesC
*PPNvConcept= IIF(MovBco1.ESTADO='99','A N U L A D O',IIF(Movbco1.TipDoc='N/A' .OR. Movbco1.TipDoc='N/C',Val_paraB (Movbco1.Motivo, 'MOTIVO', 'D',22),Movbco1.NOMBRE))
vConcept=IIF(MovBco1.ESTADO='99','A N U L A D O',Movbco1.Nombre)
*vConcept=IIF(MovBco1.ESTADO='99','A N U L A D O',IIF(Movbco1.Nombre<>SPACE(45),MOVBCO1.NOMBRE,Val_paraB(Movbco1.Motivo, 'MOTIVO', ' ',22)))
    vCuenta  = "110104"
    vCuentax = "110103"

*																				              Val_paraB (m.Motivo, 'MOTIVO', ' ',22)	
****PPN
STORE SPACE(12) TO NCUENTA
DO WHILE .NOT. EOF()
	TIPDOCW=TIPDOC
	SELE ASTPAT
	DO CASE 
		CASE MOVBCO1.TIPDOC='B/D'
			SET ORDER TO ASTPAT7
		CASE MOVBCO1.TIPDOC='N/C'
			SET ORDER TO ASTPAT5
		CASE MOVBCO1.TIPDOC='N/A'
			SET ORDER TO ASTPAT6
		CASE MOVBCO1.TIPDOC='REV'
			SET ORDER TO ASTPAT15
		CASE MOVBCO1.TIPDOC='NOC'
			SET ORDER TO ASTPAT9
	ENDCASE			
	SELE MOVBCO1
	DO WHILE MOVBCO1.TIPDOC=TIPDOCW AND NOT EOF()
		SW=' '
	    vCodCtc = Movbco1.CodCtc
	    vnummes = movbco1.nummesc
	    VNUMERO = MOVBCO1.NUMERO
	    vCuenta  = "110104"
	    vCuentax = "110103"
		CLAW=MOVBCO1.NUMMES+ALLTRIM(MOVBCO1.NUMREF)+MOVBCO1.CODCTC
		SELE ASTPAT
		SEEK VNUMMES+ALLTRIM(VNUMERO)+VCODCTC
		IF EOF()
			SELE MOVBCO1
			SKIP
			LOOP
		ELSE
			DO WHILE NUMMES=VNUMMES AND NUMREF=VNUMERO AND CODCTC=VCODCTC AND NOT EOF()
				IF SUBSTR(CODCTA,1,6)=SUBSTR(VCUENTA,1,6) OR SUBSTR(CODCTA,1,6)=SUBSTR(VCUENTAX,1,6)				&&  ya avsar n!!! OR SUBSTR(CODCTA,1,3)=SUBSTR(VCUENTAY,1,3)
					 SW='1'
					 NCUENTA=CODCTA
				     IF TipDocw = 'NOC'
    				 	vTipCta = IIF(TipCta = 'D',"D","H")
    				 ENDIF
				ENDIF
				SKIP
			ENDDO
		ENDIF
		IF SW='1'													
			SELE MOVBCO1
*	
*SCAN 
		     vCodCtc = Movbco1.CodCtc
		     vnummes = movbco1.nummesc
	    	 vFecha  = Movbco1.FechaC
		     vTipDoc = Movbco1.TipDoc
    		 STORE 0 TO vMontoD,vMontoH
		     IF vTipDoc = 'B/D'
    		    vMontoD = Movbco1.Monto
		     ENDIF
    		 IF vTipDoc = 'N/A'
        		vMontoD = Movbco1.Monto
		     ENDIF
	    	 IF vTipDoc = 'N/C'
		        vMontoH = Movbco1.Monto
    		 ENDIF
		     IF vTipDoc = 'NOC'
    		 	vMontoD = IIF(vTipCta = 'D',Movbco1.Monto,0)
        		vMontoH = IIF(vTipCta = 'H',Movbco1.Monto,0)
        		vCodCtc = Movbco1.CodCtc
		     ENDIF
	    	 vNUMERO = Movbco1.NumERO
		     *PPNvConcept= IIF(MovBco1.ESTADO='99','A N U L A D O',IIF(Movbco1.TipDoc='N/A' .OR. Movbco1.TipDoc='N/C',Val_paraB (Movbco1.Motivo, 'MOTIVO', 'D',22),Movbco1.NOMBRE))
    		 *vConcept = IIF(Movbco1.TipDoc='N/A' .OR. Movbco1.TipDoc='N/C',Val_paraB (Movbco1.Motivo, 'MOTIVO', 'D',22),Movbco1.NOMBRE)
		     *PPN
    		vConcept=IIF(MovBco1.ESTADO='99','A N U L A D O',Movbco1.Nombre)
			*vConcept=IIF(MovBco1.ESTADO='99','A N U L A D O',IIF(Movbco1.Nombre<>SPACE(45),MOVBCO1.NOMBRE,Val_paraB(Movbco1.Motivo, 'MOTIVO', ' ',22)))
		     SELECT Temp
    		 APPEND BLANK
		
	    	 REPLACE Temp.CodCtc  WITH vCodCtc;
    	    	     Temp.FecChq  WITH vFecha;
	    	         Temp.NumMes  WITH vNumMes;
    	    	     Temp.TipDoc  WITH vTipDoc;
        	    	 Temp.NumREF  WITH vNUMERO;
	            	 Temp.Concept WITH vConcept;
		             Temp.ValChqD WITH vMontoD;
    		         Temp.ValChqH WITH vMontoH;
        		     TEMP.CUENTA  WITH NCUENTA
		ENDIF
    	 SELE MOVBCO1
		 SKIP
	 ENDDO             
* USE IN 10
ENDDO	 
*ENDSCAN
DO Valida1
RETURN


FUNCTION Valida1
*--------------
SELECT COMP1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumCP TO (vInd1) FOR NUMMES=WMes 
*SET RELATION TO NumMes+NumCP+CodCtc INTO CHEQUE

GO TOP
vCodCtc = comp1.CodCtc
vFecha  = comp1.FecCP
vNUMCHQ= Cheque.NumChq
vNumERO = comp1.NumCP
vNumMes = comp1.NumMes
vMontoH = IIF(comp1.TIPDOC='IN',0,IIF(Comp1.ESTADO='99',0,Cheque.ValChq))
vConcept= IIF(comp1.TIPDOC='IN','I N U T I L I Z A D O',IIF(CHEQUE.ESTADO='99','A N U L A D O',CHEQUE.NOMGIR))
vTipDoc = "C/P"
****PPN
STORE SPACE(12) TO NCUENTA
*USE ASTPAT IN 10 ORDER TAG ASTPAT3 ALIAS ASTPAT
SELE ASTPAT
SET ORDER TO ASTPAT3
SELE COMP1
DO WHILE NOT EOF()
*SCAN 
	SW=' '
    vCodCtc  = comp1.CodCtc
    vNUMMES  = comp1.NUMMES
    vNumERO  = comp1.NumCP
    VESTADO  = comp1.estado
    vFecha   = comp1.FecCP
    vtipdoc  = comp1.tipdoc
	CLAW=COMP1.NUMMES+ALLTRIM(COMP1.NUMREF)+COMP1.CODCTC
	SELE ASTPAT
	SEEK VNUMMES+ALLTRIM(VNUMERO)			&&+VCODCTC
	IF EOF()
	    if vestado='99'
	    	sw='1'
	    	NCUENTA=VCUENTA
		else
			SELE COMP1
			SKIP
			LOOP
		endif			
	ELSE
		DO WHILE NUMMES=VNUMMES AND NUMREF=VNUMERO AND !EOF()		&&AND CODCTC=VCODCTC
			IF SUBSTR(CODCTA,1,6)=SUBSTR(VCUENTA,1,6) OR SUBSTR(CODCTA,1,6)=SUBSTR(VCUENTAX,1,6) 			&& ya avsar n!!! OR SUBSTR(CODCTA,1,3)=SUBSTR(VCUENTAY,1,3)
				SW='1'
				NCUENTA=CODCTA
				
				SELE Cheque
				SEEK VNUMMES+VNUMERO		&&+VCODCTC
				DO WHILE NUMMES=VNUMMES AND NUMcp=VNUMERO AND NOT EOF()			&& AND CODCTC=VCODCTC
					if delete()
						skip
						loop
					endif
					vNUMCHQ  = Cheque.NumChq
					IF AstPat.TipCta='D'
						vMontoD  = IIF(vTIPDOC='IN',0,IIF(vESTADO='99',0,Cheque.ValChq))
						vMontoH  = 0
					 ELSE
						vMontoD  = 0
						vMontoH  = IIF(vTIPDOC='IN',0,IIF(vESTADO='99',0,Cheque.ValChq))
					ENDIF
					vConcept= IIF(vTIPDOC='IN','I N U T I L I Z A D O',IIF(vESTADO='99','A N U L A D O',CHEQUE.NOMGIR))
					vTipDoc = "C/P"
					SELECT Temp
					APPEND BLANK
					
					REPLACE Temp.CodCtc  WITH AstPat.CodCtc;
							Temp.TipDoc  WITH vTipDoc;
							Temp.FecChq  WITH vFecha;
							Temp.NumChq  WITH vNUMCHQ;
							Temp.NumREF  WITH vNumero;
							Temp.NumMes  WITH vNumMes;
							Temp.ValChqD WITH vMontoD;
							Temp.ValChqH WITH vMontoH;
							Temp.CUENTA  WITH NCUENTA;
							Temp.Concept WITH vConcept
				
					SELE CHEQUE
					skip
				enddo
			ENDIF
			SELE AstPat
			SKIP
		ENDDO
	ENDIF
*	IF SW='1'													
*		SELE Cheque
*		SEEK VNUMMES+VNUMERO+VCODCTC
*		DO WHILE NUMMES=VNUMMES AND NUMcp=VNUMERO AND CODCTC=VCODCTC AND NOT EOF()
*			if delete()
*				skip
*				loop
*			endif				
*	     	vNUMCHQ  = Cheque.NumChq
*	     	vMontoH  = IIF(vTIPDOC='IN',0,IIF(vESTADO='99',0,Cheque.ValChq))
*	 		vConcept= IIF(vTIPDOC='IN','I N U T I L I Z A D O',IIF(vESTADO='99','A N U L A D O',CHEQUE.NOMGIR))
*		 	vTipDoc = "C/P"
*	     	SELECT Temp
*	     	APPEND BLANK
*	
*   	 	REPLACE Temp.CodCtc  WITH vCodCtc;
*             Temp.TipDoc  WITH vTipDoc;
*            Temp.FecChq  WITH vFecha;
*             Temp.NumChq  WITH vNUMCHQ;
*             Temp.NumREF  WITH vNumero;
*             Temp.NumMes  WITH vNumMes;
*             Temp.ValChqH WITH vMontoH;
*             Temp.CUENTA  WITH NCUENTA;
*             Temp.Concept WITH vConcept
*             SELE CHEQUE
*             skip
*		enddo             
*	ENDIF             
	SELE COMP1
	SKIP
ENDDO	
*SE IN 10
*ENDSCAN

DO Valida2
RETURN

FUNCTION Valida2
*--------------
SELECT hojanu1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumHA TO (vInd1) FOR NUMMES=WMes 
SET RELATION TO NumMes+NumHA INTO ITEHA
*SET RELATION TO NumMes+NumHA INTO ITHA
SET SKIP TO IteHA
GO TOP
vCodCtc = hojanu1.CodCtc
vFecha  = hojanu1.Fechan
vNUMCHQ= itEha.NumChq
vNumERO = hojanu1.Numha
vNumMes = hojanu1.NumMes
vMontoD = itEha.ValChq
vConcept= ITEHA.NOMGIR
VTIPDOC="H/A"
****PPN
STORE SPACE(12) TO NCUENTA
SELE ASTPAT
SET ORDER TO ASTPAT18
SELE HOJANU1
DO WHILE NOT EOF()
*SCAN 
    vCodCtc  = hojanu1.CodCtc
    vNumERO  = hojanu1.Numha
    vNumMes  = hojanu1.NumMes
	CLAW=HOJANU1.NUMMES+ALLTRIM(HOJANU1.NUMREF)+HOJANU1.CODCTC
	SELE ASTPAT
	SEEK VNUMMES+VNUMERO+VCODCTC
	IF EOF()
		SELE COMP1
		SKIP
		LOOP
	ELSE
		DO WHILE NUMMES=VNUMMES AND NUMREF=VNUMERO AND CODCTC=VCODCTC AND NOT EOF()
			IF SUBSTR(CODCTA,1,6)=SUBSTR(VCUENTA,1,6) OR SUBSTR(CODCTA,1,6)=SUBSTR(VCUENTAX,1,6) 			&& ya avsar n!!! OR SUBSTR(CODCTA,1,3)=SUBSTR(VCUENTAY,1,3)
				SW='1'
				NCUENTA=CODCTA
			ENDIF
			SKIP
		ENDDO
	ENDIF
	IF SW='1'
		SELE HOJANU1
		vFecha   = hojanu1.Fechan
	    vNUMCHQ  = itEha.NumChq
    	vMontoD  = itEha.ValChq
     	vConcept = ITEHA.NOMGIR
	 	VTIPDOC="H/A"
     	SELECT Temp
     	APPEND BLANK
     	REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.TipDoc  WITH vTipDoc;
             Temp.FecChq  WITH vFecha;
             Temp.NumChq  WITH vNUMCHQ;
             Temp.NumREF  WITH vNumero;
             Temp.NumMes  WITH vNumMes;
             Temp.ValChqD WITH vMontoD;
             Temp.CUENTA  WITH NCUENTA;
             Temp.CONCEPT WITH vConcept
	ENDIF
	SELE HOJANU1
	SKIP
ENDDO	             
*ENDSCAN
RETURN


PROCEDURE Salida
*---------------
RELEASE  WINDOW LibBan
ACTIVATE SCREEN
CLOSE    DATABASES
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,VAL(xMes)*9-8,9)) + ' ' + xAno

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.

FUNCTION Val_tipo
*------------------
SELECT Caja
SEEK vctacte
IF FOUND()
	IF Sector = 'S'
	    IF CODFTE='00' .OR. CODFTE='01'
			vvsec = 'S'
		ELSE			
		    vvact = 'S'
		ENDIF    
		vtiplis = 1
	ELSE
		g = 'N'
		vtiplis = 2
	ENDIF
	SHOW GETS
	RETURN .T.
ENDIF
RETURN .F.

FUNCTION Salbcos     && funci¢n utilizada en LIBBAN.frx
*----------------
PARAMETERS vcodcta
vSel = ALIAS()
STORE 0 TO vdebacu,vhabacu,vsalacu
SELECT Salbco

*WAIT WIND STR(LEN(vCodcta))
IF xAno = "00"
	rAno = "2000"
ENDIF

*SEEK vCodcta+rAno+ALLTRIM(wmes)
SEEK vCodcta+xAno+ALLTRIM(wmes)
IF FOUND()
	SKIP -1
	IF ALLTRIM(SALBCO.CODCTC)=ALLTRIM(VCodcta)
		vsaldeba=salbco.salDEB
   		vsalhaba=salbco.salHAB
		vsalant=vsaldeba-vsalhaba
		IF SALBCO.NUMMES='12'
    		IF VSALANT<=0
  	       		VSALDEBA=0
    	   		VSALHABA=ABS(VSALANT)
	    	ELSE
       			VSALDEBA=VSALANT
       			VSALHABA=0
  	    	ENDIF
	 	ENDIF
		SKIP
		vdebacu=salbco.saldeb
  		vhabacu=salbco.salhab
		vsalacu=salbco.saldeb-ABS(salbco.salhab)
     ENDIF
ELSE
	STORE 0 TO vsalhaba,vsaldeba,vsalant
		vdebacu=salbco.saldeb
  		vhabacu=salbco.salhab
		vsalacu=salbco.saldeb-salbco.salhab
ENDIF
SELECT (vSel)
RETURN vSaldo