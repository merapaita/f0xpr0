* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ LibBan.Prg  11/08/96                                L: 0515  ³	
* ³ Emisi¢n del Libro Bancos                                     ³
* ³ AUTORES :  Ing. Federico Montero Valdiviezo (Versi¢n Inicial)³
* ³            Julio Cruz Ortiz  REGION GRAU                     ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.     07/1997 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

* Abrir Archivos de trabajo
USE MovBco   IN 1 ORDER TAG MovBco1        ALIAS MovB
USE Compag   IN 2 ORDER TAG Compag1         ALIAS Comp
USE Hojanu   IN 3 ORDER TAG hojanu1		    ALIAS hojanu	
USE Cajas    IN 4 ORDER TAG Cajas1          ALIAS Caja
USE Parmae   IN 5 ORDER TAG Parmae1         ALIAS Parma
USE Cheque   IN 6 ORDER TAG Cheque1         ALIAS Cheque
USE itehA    IN 7 ORDER TAG itehA1       	ALIAS itehA 
USE Salbcos  IN 9 ORDER TAG salbcos1 		ALIAS salbco
**
*USE Comp2    IN 10 ORDER TAG Compag1         ALIAS PAGO
*USE Chequ2   IN 11 ORDER TAG Cheque1         ALIAS Chequ2
*USE Hoja     IN 12 ORDER TAG hojanu1		 ALIAS hoja

*USE H:\EMER98\DATA\CompAG  IN 10 ORDER TAG Compag1   ALIAS COMP2
*USE H:\EMER98\DATA\ChequE  IN 11 ORDER TAG Cheque1   ALIAS Chequ2
*USE H:\EMER98\DATA\HojANU  IN 12 ORDER TAG hojanu1   ALIAS hoja


**

filaux = SYS(3) + '.DBF'
zInd   = SYS(3) + '.Idx'

SELECT 8
CREATE TABLE (filaux);
  (CodCtc    C(014),;
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

PUBLIC vCodfte,vtip,vsec,vctacte,vtipo,vliscta,vtiplis,xano,wmes,vlissec,VCUENTA,NCUENTA

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
  VMes     = MONTH(DATE())
  xMes     = SPACE(2)
  IF VMES<10
	 XMES  ='0'+ALLTRIM(STR(MONTH(DATE())))
  ELSE
     xMES=STR(VMES,2)   
  ENDIF   
  xAno     = RIGHT(DTOC(DATE()),2)
  STORE 1 TO vliscta,vlisfte,vlistip,vlissec
  STORE 2 TO vTiplis,vMancom
  STORE SPACE(1) TO vtip,vvsec,vsec
  STORE SPACE(3) TO vcodfte
  STORE SPACE(12) TO vcuenta,NCUENTA
  vvsec = 'N'
  SELECT Caja
  @ 0, 1 SAY '        Mes y A¤o : ' GET xMes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',22,9) 
  @ 0,25 GET xAno PICTURE '!!'
  @ 2, 1 SAY 'Todas las Cuentas : ' GET vliscta FUNCTION '*RNH Si;No' VALID VALTOD(vliscta,4,20)
  @ 4, 1 SAY 'Ctas. Mancomunadas? ' GET vmancom FUNCTION '*RNH Si;No'
  @ 6, 1 SAY ' Cuenta Corriente : ' GET vCtaCte WHEN vliscta=2 PICTURE '!!!!!!!!!!!!!!' ; 
  			VALID  Val_fun2('Caja','CodCtc','codctc+descri',vCtaCte,1,5,22,'LEFT(DESCRI,38)')    AND Val_Tipo()
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
*  	IF vCtaCte='720270234'
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
 
*  		SELECT M.* ;
*  		FROM COMP2 M ;
*  		WHERE CodCtc=vCtaCte ;
*  		INTO CURSOR compA2

	  	SELECT Caja
	  	SEEK ALLT(vctacte)
	  	IF FOUND()
	  		vcodfte = CodFte
  			vclase  = clase
  			vcuenta = cuentah

	  	ENDIF


*  	ELSE
*  	  	SELECT M.* ;
*  		FROM Movbco M ;
*   		WHERE CodCtc=vCtaCte  ;
*  		INTO CURSOR movbco1
			
*  	  	SELECT M.* ;
*  		FROM Compag M ;
*  		WHERE CodCtc=vCtaCte ;
*  		INTO CURSOR comp1

*  	  	SELECT M.* ;
*  		FROM hojanu M ;
*  		WHERE CodCtc=vCtaCte ;
*  		INTO CURSOR hojanu1

*	  	SELECT Caja
*	  	SEEK ALLT(vctacte)
*	  	IF FOUND()
*  			vcodfte = CodFte
*	  	ENDIF
*	ENDIF
ENDIF
    DO Valida

    SELECT temp
  	INDEX ON Tipdoc+nummes+numref TO (zInd) 
	IF vliscta = 2 
	 	SUM TEMP.valchqd TO VSALDEB
  		SUM TEMP.valchqh TO VSALHAB

		  	SELE SALBCO
	  		SEEK ALLTRIM(vctacte+xano+wmes)

		  	IF FOUND()
	  		    IF RLOCK()
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
		  	      REPLACE saldeb WITH vsaldeb, salhab WITH vsalhab
	  		      UNLOCK
	  	    	ENDIF
			ELSE
				If f_appd()
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
	  		SEEK ALLTRIM(vctacte+xano+wmes)

		  	IF FOUND()
	  		    IF RLOCK()
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codctc WITH vctacte
		  	      REPLACE saldeb WITH vsaldeb, salhab WITH vsalhab
	  		      UNLOCK
	  	    	ENDIF
			ELSE
				If f_appd()
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
  		SEEK ALLTRIM(vctacte+xano+wmes)
  		SKIP -1
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
				INDEX ON CODCTC+DTOC(fecchq)+NumRef  FOR LEFT(numref,1)$'0123456789' TO (PP)
				DO REPORTE WITH 2, "LibBanI", ' Reporte Libro Banco ',1,.F.,.T.
			CASE vtiplis = 1
				INDEX ON LEFT(NumRef,1)+CODCTC+DTOC(fecchq)+numref TO (YY)     &&FOR !(LEFT(numref,1)$'0123456789') 
				DO REPORTE WITH 2, "LibBanS1", ' Reporte Libro Banco ',1,.F.,.T.
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
INDEX ON TIPDOC+NUMMES+NUMERO TO (vInd) FOR 	NUMMESC=WMes AND ;
				TIPDOC<>"NOC"  
				*AND ESTADO<>'99'
GO TOP
vCodCtc = Movbco1.CodCtc
vFecha  = Movbco1.FechaC
vTipDoc = Movbco1.TipDoc
vNUMERO= Movbco1.NumERO
vNumMes = Movbco1.NumMesC
vConcept= IIF(MovBco1.ESTADO='99','A N U L A D O',IIF(Movbco1.TipDoc='N/A' .OR. Movbco1.TipDoc='N/C',Val_paraB (Movbco1.Motivo, 'MOTIVO', 'D',22),Movbco1.NOMBRE))
*vConcept=IIF(MovBco1.ESTADO='99','A N U L A D O',Movbco1.Nombre)
SCAN 
     vCodCtc = Movbco1.CodCtc
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
     vNUMERO = Movbco1.NumERO
     vConcept= IIF(MovBco1.ESTADO='99','A N U L A D O',IIF(Movbco1.TipDoc='N/A' .OR. Movbco1.TipDoc='N/C',Val_paraB (Movbco1.Motivo, 'MOTIVO', 'D',22),Movbco1.NOMBRE))
     *vConcept = IIF(Movbco1.TipDoc='N/A' .OR. Movbco1.TipDoc='N/C',Val_paraB (Movbco1.Motivo, 'MOTIVO', 'D',22),Movbco1.NOMBRE)
     *vConcept=IIF(MovBco1.ESTADO='99','A N U L A D O',Movbco1.Nombre)
     SELECT Temp
     APPEND BLANK

     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.FecChq  WITH vFecha;
             Temp.TipDoc  WITH vTipDoc;
             Temp.NumREF  WITH vNUMERO;
             Temp.Concept WITH vConcept;
             Temp.ValChqD WITH vMontoD;
             Temp.ValChqH WITH vMontoH
ENDSCAN

DO Valida1
RETURN


FUNCTION Valida1
*--------------
SELECT COMP1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumCP TO (vInd1) FOR NUMMES=WMes 
SET RELATION TO NumMes+NumCP+CodCtc INTO CHEQUE

GO TOP
vCodCtc = comp1.CodCtc
vFecha  = comp1.FecCP
vNUMCHQ= Cheque.NumChq
vNumERO = comp1.NumCP
vNumMes = comp1.NumMes
vMontoH = IIF(comp1.TIPDOC='IN',0,IIF(Comp1.ESTADO='99',0,Cheque.ValChq))
vConcept= IIF(comp1.TIPDOC='IN','I N U T I L I Z A D O',IIF(CHEQUE.ESTADO='99','A N U L A D O',CHEQUE.NOMGIR))
vTipDoc = "C/P"
SCAN 
     vCodCtc  = comp1.CodCtc
     vFecha   = comp1.FecCP
     vNUMCHQ  = Cheque.NumChq
     vNumERO  = comp1.NumCP
     vNumMes  = comp1.NumMes
     vMontoH  = IIF(comp1.TIPDOC='IN',0,IIF(Comp1.ESTADO='99',0,Cheque.ValChq))
	 vConcept= IIF(comp1.TIPDOC='IN','I N U T I L I Z A D O',IIF(comp1.ESTADO='99','A N U L A D O',CHEQUE.NOMGIR))
	 vTipDoc = "C/P"
     SELECT Temp
     APPEND BLANK

     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.TipDoc  WITH vTipDoc;
             Temp.FecChq  WITH vFecha;
             Temp.NumChq  WITH vNUMCHQ;
             Temp.NumREF  WITH vNumero;
             Temp.NumMes  WITH vNumMes;
             Temp.ValChqH WITH vMontoH;
             Temp.Concept WITH vConcept

ENDSCAN
**
DO Valida2
RETURN

*****

FUNCTION Valida2A
*--------------
SELECT COMPA2
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumCP TO (vInd1) FOR NUMMES=WMes 
SET RELATION TO NumMes+NumCP+CodCtc INTO CHEQU2

GO TOP
vCodCtc = compA2.CodCtc
vFecha  = compA2.FecCP
vNUMCHQ= Chequ2.NumChq
vNumERO = compA2.NumCP
vNumMes = compA2.NumMes
vMontoH = IIF(compA2.TIPDOC='IN',0,IIF(CompA2.ESTADO='99',0,Chequ2.ValChq))
vConcept= IIF(compA2.TIPDOC='IN','I N U T I L I Z A D O',IIF(CHEQU2.ESTADO='99','A N U L A D O',CHEQU2.NOMGIR))
vTipDoc = "C/P"
SCAN 
     vCodCtc  = compA2.CodCtc
     vFecha   = compA2.FecCP
     vNUMCHQ  = Chequ2.NumChq
     vNumERO  = compA2.NumCP
     vNumMes  = compA2.NumMes
     vMontoH  = IIF(compA2.TIPDOC='IN',0,IIF(CompA2.ESTADO='99',0,Chequ2.ValChq))
	 vConcept= IIF(compA2.TIPDOC='IN','I N U T I L I Z A D O',IIF(compA2.ESTADO='99','A N U L A D O',CHEQU2.NOMGIR))
	 vTipDoc = "C/P"
     SELECT Temp
     APPEND BLANK

     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.TipDoc  WITH vTipDoc;
             Temp.FecChq  WITH vFecha;
             Temp.NumChq  WITH vNUMCHQ;
             Temp.NumREF  WITH vNumero;
             Temp.NumMes  WITH vNumMes;
             Temp.ValChqH WITH vMontoH;
             Temp.Concept WITH vConcept

ENDSCAN


DO Valida2
RETURN

******


FUNCTION Valida2
*--------------
SELECT hojanu1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumHA TO (vInd1) FOR NUMMES=WMes 
SET RELATION TO NumMes+NumHA INTO ITEHA
SET SKIP TO
GO TOP
vCodCtc = hojanu1.CodCtc
vFecha  = hojanu1.Fechan
vNUMCHQ= iteha.NumChq
vNumERO = hojanu1.Numha
vNumMes = hojanu1.NumMes
vMontoD = iteha.ValChq
vConcept= ITEHA.NOMGIR
VTIPDOC="H/A"
SCAN 
     vCodCtc  = hojanu1.CodCtc
     vFecha   = hojanu1.Fechan
     vNUMCHQ  = iteha.NumChq
     vNumERO  = hojanu1.Numha
     vNumMes  = hojanu1.NumMes
     vMontoD  = iteha.ValChq
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
             Temp.CONCEPT WITH vConcept
ENDSCAN

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
		vvsec = 'S'
		vtiplis = 1
	ELSE
		vvsec = 'N'
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
SEEK vCodcta+xano+ALLTRIM(wmes)
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