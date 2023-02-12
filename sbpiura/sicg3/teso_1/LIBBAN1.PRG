* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ LibBan.Prg  11/08/96                                L: 0515  ³	
* ³ Emisi¢n del Libro Bancos                                     ³
* ³ AUTORES :  Ing. Federico Montero Valdiviezo (Versi¢n Inicial)³
* ³            Julio Cruz Ortiz  REGION GRAU                     ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

* Abrir Archivos de trabajo
USE MovBco   IN 1 ORDER TAG MovBco11        ALIAS MovB
USE Compag   IN 2 ORDER TAG Compag1         ALIAS Comp
USE Hojanu   IN 3 ORDER TAG hojanu1		    ALIAS hojanu	
USE Cajas    IN 4 ORDER TAG Cajas1          ALIAS Caja
USE Parmae   IN 5 ORDER TAG Parmae1         ALIAS Parma
USE Cheque   IN 6 ORDER TAG Cheque1         ALIAS Cheque
USE itehA    IN 7 ORDER TAG itehA1       	ALIAS itehA 
USE Salbcos  IN 9 ORDER TAG salbcos1 		ALIAS salbco

filaux = SYS(3) + '.DBF'
zInd   = SYS(3) + '.Idx'

SELECT 8
CREATE TABLE (filaux);
  (CodCtc    C(014),;
   CodCal    C(019),;
   FecChq    D(008),;
   TipDoc    C(003),;
   NumChq    C(014),;
   NumREF    C(004),;
   NumMes    C(002),;
   Concept   C(045),;
   ValChqD   N(18,2),;
   ValChqH   N(18,2))
USE

USE (filaux) IN 8  EXCLUSIVE   ALIAS Temp

** Declaraci¢n de variables

PUBLIC vCodfte,vtip,vsec,vctacte,vtipo,vliscta,vtiplis,xano,wmes,vlissec

STORE 0 TO xano, vMontoD, vMontoH,VSALDO,VSALDEB,VSALHAB,VSALDEBA,VSALHABA,VDEBACU,VHABACU,vsalant,vsalacu,fmes
STORE SPACE(14)  TO vCtaCte
STORE SPACE(2)  TO WMES,ames
STORE ' ' TO vNUMERO, vCodCal, vConcept, vNumCHQ, vNumMes,VTIPDOC
STORE .F. TO vtipo
STORE 1   TO vTiplis

Vcodprg = '02'
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
  STORE SPACE(1) TO vtip
  STORE SPACE(3) TO vcodfte,vsec
  SELECT Caja
  @ 0, 1 SAY '        Mes y A¤o : ' GET xMes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',22,9) 
  @ 0,25 GET xAno PICTURE '!!'
  @ 2, 1 SAY 'Todas las Cuentas : ' GET vliscta FUNCTION '*RNH Si;No' VALID VALTOD(vliscta,4,20)
  @ 4, 1 SAY ' Cuenta Corriente : ' GET vCtaCte WHEN vliscta=2 PICTURE '!!!!!!!!!!!!!!' ; 
  			VALID  Val_fun('Caja','CodCtc','codctc+descri',vCtaCte,1,5,22,'LEFT(DESCRI,38)') AND Val_Tipo()
  @ 6,10 GET vTiplis  WHEN vliscta = 1   FUNCTION '*RNH Sectorial;TRN;PRP' 
  @ 8, 1 SAY 'Todos los Sectores: ' GET vlissec WHEN VLISCTA = 2 AND vtipo FUNCTION '*RNH Si;No' VALID VALTOD(vliscta,16,20)
  @10, 1 SAY '           Sector : ' GET vsec    WHEN vlissec = 2 AND vtipo ;
  				VALID VAL_SUBP(vSec,'CODSUB'+PADR(vCodPrg,6,' '),' ',22,40)  
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
  		WHERE IIF(vTiplis = 1,c.sector='S',IIF(vTiplis = 3,c.codfte = ;
  				'PRP' AND c.sector='N', c.codfte = 'TRN' AND c.sector='N'));
  				AND M.codctc=C.codctc ;
  		INTO CURSOR movbco1

  	  	SELECT M.* ;
  		FROM Cajas C,Compag M ;
  		WHERE IIF(vTiplis = 1,c.sector='S',IIF(vTiplis = 3,c.codfte = 'PRP';
  			 	AND c.sector='N', c.codfte = 'TRN' AND c.sector='N'));
  			 	AND M.codctc=C.codctc ;
  		INTO CURSOR comp1

  	  	SELECT M.* ;
  		FROM Cajas C,Hojanu M ;
  		WHERE IIF(vTiplis = 1,c.sector='S',IIF(vTiplis = 3,c.codfte = 'PRP' ;
  				AND c.sector='N', c.codfte = 'TRN' AND c.sector='N'));
  				AND M.codctc=C.codctc ;
  		INTO CURSOR hojanu1

  		SELECT C.* ;
  		FROM Cajas C ;
  		WHERE IIF(vTiplis = 1,c.sector='S',IIF(vTiplis = 3,c.codfte = 'PRP' ;
  				AND c.sector='N', c.codfte = 'TRN' AND c.sector='N'));
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
	  	ENDIF
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
		   DO REPORTE WITH 2, "salant", ' Reporte Libro Banco ',2,.F.,.T.				
           USE
        ENDIF
        	   
   	ELSE
		DO CASE
			CASE vtiplis = 2
	   			INDEX ON CODCTC+SUBST(CODCAL,10,3)+DTOC(fecchq)+NumRef TO (PP)
				DO REPORTE WITH 2, "LibBanTa", ' Reporte Libro Banco ',2,.F.,.T.			

			CASE vtiplis = 3
				INDEX ON CODCTC+DTOC(fecchq)+NumRef  TO (PP)
				DO REPORTE WITH 2, "LibBanI", ' Reporte Libro Banco ',2,.F.,.T.				

			CASE vtiplis = 1
				INDEX ON CODCTC+SUBST(CODCAL,10,3)+DTOC(fecchq)+numref TO (YY) FOR IIF(vlissec = 2, SUBST(Codcal,10,3) = ALLTRIM(vsec),.T.)
				IF vlissec = 2
					DO REPORTE WITH 2, "LibBanSE", ' Reporte Libro Banco ',2,.F.,.T.
				ELSE
					DO REPORTE WITH 2, "LibBanS1", ' Reporte Libro Banco ',2,.F.,.T.
				ENDIF
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
INDEX ON CodCal+TIPDOC+NUMMES+NUMERO TO (vInd) FOR 	NUMMESC=WMes AND ;
				TIPDOC<>"NOC"  AND ESTADO<>'99'
GO TOP
vCodCtc = Movbco1.CodCtc
vCodCal = Movbco1.CodCal
vFecha  = Movbco1.FechaC
vTipDoc = Movbco1.TipDoc
vNUMERO= Movbco1.NumERO
vNumMes = Movbco1.NumMesC
vConcept= Movbco1.NOMBRE
SCAN 
     vCodCtc = Movbco1.CodCtc
     vCodCal = Movbco1.CodCal     
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
     vConcept = Movbco1.NOMBRE
     SELECT Temp
     APPEND BLANK
     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.CodCal  WITH vCodCal;
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
INDEX ON CodCal+NumMes+NumCP TO (vInd1) FOR NUMMES=WMes 
SET RELATION TO NumMes+NumCP+CodCtc INTO CHEQUE

GO TOP
vCodCtc = comp1.CodCtc
vCodCal = Cheque.CodCal
vFecha  = comp1.FecCP
vNUMCHQ= Cheque.NumChq
vNumERO = comp1.NumCP
vNumMes = comp1.NumMes
vMontoH = IIF(comp1.TIPDOC='IN',0,IIF(Comp1.ESTADO='99',0,Cheque.ValChq))
vConcept= IIF(comp1.TIPDOC='IN','I N U T I L I Z A D O',IIF(CHEQUE.ESTADO='99','A N U L A D O',CHEQUE.NOMGIR))
vTipDoc = "C/P"
SCAN 
     vCodCtc  = comp1.CodCtc
     vCodCal  = Cheque.CodCal
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
             Temp.CodCal  WITH vCodCal;
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

FUNCTION Valida2
*--------------
SELECT hojanu1
vInd1 = SYS(3) + '.Idx'
INDEX ON CodCal+NumMes+NumHA TO (vInd1) FOR NUMMES=WMes 
SET RELATION TO NumMes+NumHA INTO ITEHA
SET SKIP TO
GO TOP
vCodCtc = hojanu1.CodCtc
vCodCal = iteha.CodCal
vFecha  = hojanu1.Fechan
vNUMCHQ= iteha.NumChq
vNumERO = hojanu1.Numha
vNumMes = hojanu1.NumMes
vMontoD = iteha.ValChq
vConcept= ITEHA.NOMGIR
VTIPDOC="H/A"
SCAN 
     vCodCtc  = hojanu1.CodCtc
     vCodCal  = iteha.CodCal
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
             Temp.CodCal  WITH vCodCal;
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
	IF Sector = 'N'
		vtipo = .F.
		IF Codfte = 'TRN'
			vtiplis = 2
		ELSE
			vtiplis = 3
		ENDIF
	ELSE
		vtipo = .T.
		vtiplis = 1
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