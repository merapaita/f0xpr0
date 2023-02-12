** ----------------------------------------------
** LIBBAN.PRG
** Reporte Libro de Bancos Para todos los casos
** UDEP - 1995. FMV/JCO
** ----------------------------------------------

* Abrir Archivos de trabajo
CLOS DATA
USE MovBco   IN 1 ORDER TAG MovBco1         ALIAS MovB
USE Compag   IN 2 ORDER TAG Compag1         ALIAS Comp
USE Hojanu   IN 3 ORDER TAG hojanu1		    ALIAS hojanu	
USE Cajas    IN 4 ORDER TAG Cajas1          ALIAS Caja
USE Parmae   IN 5 ORDER TAG Parmae1         ALIAS Parma
USE Cheque   IN 6 ORDER TAG Cheque1         ALIAS Cheque
USE itehA    IN 7 ORDER TAG itehA1       	ALIAS itehA 
USE Salbcos  IN 9 ORDER TAG salbcos1 		ALIAS salbco
USE Compro  IN 10 ORDER TAG compro1 		ALIAS compro
USE DIARIO  IN 11 ORDER TAG DIARIO14 		ALIAS DIAR

filaux = SYS(3) + '.DBF'
zInd   = SYS(3) + '.Idx'

SELE Caja
set filter to MONEDA<>'N'

SELECT 8
CREATE TABLE (filaux);
  (CodCtc    C(017),;
   CodCaD    C(04),;
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
STORE SPACE(17)  TO vCtaCte
STORE SPACE(2)  TO WMES,ames
STORE ' ' TO vNUMERO, vCodCaD, vConcept, vNumCHQ, vNumMes,VTIPDOC
STORE .F. TO vtipo
STORE 1   TO vTiplis

VCODPRG = '02'
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
  xMes     = SPACE(2)
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
	@0,0 SAY " Espere un momento...." COLOR W+/N*
  	IF vliscta = 1

		SELECT A.* ;
  		FROM Cajas C, AstPat A ;
  		WHERE (A.tipdoc='N/C' OR A.tipdoc='NOC' OR A.tipdoc='N/A' OR A.tipdoc='B/D') AND MONTH(A.FECHA)=VAL(WMES) AND A.codctc=C.codctc AND A.Codcta='104';
  		INTO CURSOR movbco1

		xCadena='A.CODCTA,A.CODCTC,C.PERIODO,C.NUMMES,C.NUMCP,C.NUMCHQ,C.FECCHQ,'
		xCadena= xCadena + 'C.NOMGIR,C.VALCHQ,A.MTODEB,A.MTOHAB,C.ESTADO,C.CONBAN,C.CODCAD,C.CODPART,'
		xCadena= xCadena + 'C.CHEQUE, C.FECENT, C.FECANU,C.NUMMESHA,C.NUMHA,'
		xCadena= xCadena + 'C.VALREB,C.CONENT,C.CAJERO,C.TIPDOC'        && C.FECAUTT,C.CONAUT,
		SELECT &xCadena ;
			FROM AstPat A, CheQue C ;
			WHERE A.numref = C.numcp and (A.tipdoc='C/P') AND  A.Codcta='104' AND A.NUMMES=WMES;
			INTO CURSOR comp1

  	  	SELECT M.* ;
  		FROM Cajas C,Hojanu M ;
  		WHERE M.codctc=C.codctc ;
  		INTO CURSOR hojanu1

  	  	SELECT M.*;
  		FROM Cajas C,DIARIO M;
  		WHERE (m.comprob = '67' OR m.comprob = '80' OR m.comprob = '97' OR M.COMPROB='37')  AND M.codctc=C.codctc;
  	    AND m.CUENTA='104';	
  		INTO CURSOR compro1

  	  	SELECT M.* ;
  		FROM Cajas C,DIARIO M ;
  		WHERE m.comprob = '00'AND M.codctc=C.codctc;
  		INTO CURSOR DIARIO1

  		SELECT C.* ;
  		FROM Cajas C ;
  		INTO CURSOR Cajtmp

  	ELSE
			
  	  	SELECT A.* ;
  		FROM AstPat A ;
   		WHERE (A.tipdoc='N/C' OR A.tipdoc='NOC' OR A.tipdoc='N/A' OR A.tipdoc='B/D') AND ALLTRIM(A.codctc)=ALLTRIM(vCtaCte) AND  A.Codcta='104' AND MONTH(A.FECHA)=VAL(WMES);
  		INTO CURSOR movbco1
		

		xCadena='A.CODCTA,A.CODCTC,C.PERIODO,C.NUMMES,C.NUMCP,C.NUMCHQ,C.FECCHQ,'
		xCadena= xCadena + 'C.NOMGIR,C.VALCHQ,A.MTODEB,A.MTOHAB,C.ESTADO,C.CONBAN,C.CODCAD,C.CODPART,'
		xCadena= xCadena + 'C.CHEQUE, C.FECENT, C.FECANU,C.NUMMESHA,C.NUMHA,'
		xCadena= xCadena + 'C.VALREB,C.CONENT,C.CAJERO,C.TIPDOC'		&&C.FECAUTT,C.CONAUT
		SELECT &xCadena ;
			FROM AstPat A, CheQue C ;
			WHERE A.numref = C.numcp and (A.tipdoc='C/P') AND  A.Codcta='104' AND A.NUMMES=WMES AND ALLTRIM(A.codctc)=ALLTRIM(vCtaCte);
			INTO CURSOR comp1

  	  	SELECT M.* ;
  		FROM hojanu M ;
  		WHERE CodCtc=vCtaCte ;
  		INTO CURSOR hojanu1

  	  	SELECT M.*;
  		FROM DIARIO M ;
  		WHERE (m.comprob = '67' OR m.comprob = '80' OR m.comprob = '97' OR M.COMPROB='37')  and M.CodCtc=vCtaCte ;
  		AND M.CUENTA='104';
  		INTO CURSOR compro1

  	  	SELECT M.* ;
  		FROM DIARIO M ;
  		WHERE m.comprob = '00'AND M.codctc=Vctacte;
  		INTO CURSOR DIARIO1

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
	  		SEEK vctacte+xano+wmes
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
	    QQ = SYS(3) + '.Dbf'
	    deactivate window Xwait
  	IF EOF()
       DO STANDBY WITH "No hay registros para procesar"
   	ELSE
		INDEX ON CODCTC+TIPDOC+NUMREF  TO (PP)
		DO REPORTE WITH 2, "LibBanI", ' Reporte Libro Banco ',2,.F.,.T.
   	ENDIF
    CLOSE DATA
    ERASE (filaux)
  ENDIF
  RETURN


FUNCTION Valida
*--------------
SELECT compro1
vInd1 = SYS(3) + '.Idx'
INDEX ON COMPROB TO (vInd1) FOR SUBST(COMPROB,8,2)=WMes 
GO TOP
SCAN 
     vCodCtc  = COMPRO1.CodCtc
     vCodCaD  = ""
     vFecha   = COMPRO1.fecemi
     vNUMCHQ  = COMPRO1.NumChq
	 vNumERO = SUBST(COMPRO1.COMPROB,4,4)
	 vNumMes = SUBST(COMPRO1.COMPROB,8,2)
     vMontoH = IIF(COMPRO1.TIPO='H',COMPRO1.MONTOS,0)
     VMONTOD = IIF(COMPRO1.TIPO='D',COMPRO1.MONTOS,0)
     DO CASE
     	CASE comprob="67"
	 		vConcept= "AJUSTE POR REGULARIZACION DE CHEQUE"+vNUMCHQ
	 		vTipdoc = 'N/C'
     	CASE comprob="80"
	 		vConcept= "AJUSTE POR TRASLADO DE SUBCTENTA"
	 		vTipdoc = 'N/C'
     	CASE comprob="97"
	 		vConcept= "Por Comprobante No."+ vnumero+"fuera de presup."
	 		vTipdoc = 'C/P'
	 	CASE comprob="37"
	 		vConcept="Asientos de ajuste de Caja"
	 		vTipdoc = 'CAJ'
 		CASE comprob="18"
 			vConcept="Anulaci¢n de Cheque"
	 		vTipdoc = 'HAC'
	 ENDCASE		
     SELECT Temp
     APPEND BLANK
     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.CodCaD  WITH vCodCaD;
             Temp.TipDoc  WITH vTipDoc;
             Temp.FecChq  WITH vFecha;
             Temp.NumChq  WITH vNUMCHQ;
             Temp.NumREF  WITH vNumero;
             Temp.NumMes  WITH vNumMes;
             Temp.ValChqH WITH vMontoH;
             Temp.ValChqD WITH vMontoD;
             Temp.CONCEPT WITH vConcept
    SELEC COMPRO1
ENDSCAN

*ERASE(vInd1)
DO Valida1
RETURN

FUNCTION Valida1
*--------------
SELECT Movbco1
vInd = SYS(3) + '.Idx'
SET RELATION TO Movbco1.nummes+ Movbco1.numref INTO MOVB ADDITIVE
INDEX ON TIPDOC+NUMMES+NUMREF TO (vInd) FOR MONTH(FECHA)=VAL(WMes) AND ;
				MOVB.ESTADO<>'99'
GO TOP
SCAN 
     SELECT Temp
     APPEND BLANK
     REPLACE Temp.CodCtc  WITH Movbco1.CodCtc;
     		 Temp.NumMes  WITH Movbco1.NumMes;
             Temp.FecChq  WITH Movbco1.Fecha;
             Temp.TipDoc  WITH Movbco1.TipDoc;
             Temp.NumREF  WITH Movbco1.Numref;
             Temp.Concept WITH IIF(EMPTY(Movb.NOMBRE),'Verif. Mov. Bco.',Movb.NOMBRE);
             Temp.ValChqD WITH Movbco1.MtoDeb;
             Temp.ValChqH WITH Movbco1.MtoHab
             
    SELECT Movbco1
ENDSCAN
*ERASE(vInd)
DO Valida2
RETURN


FUNCTION Valida2
*--------------
SELECT comp1
vInd = SYS(3) + '.Idx'
SET RELATION TO Comp1.nummes+Comp1.numcp+Comp1.codctc INTO COMP ADDITIVE
*SET RELATION TO Comp1.nummes+Comp1.numref              INTO CHEQUE ADDITIVE
INDEX ON NUMMES+NUMCP TO (vInd) 
GO TOP
SELE astpat
SET ORDER TO AstPat3
SELE Comp1
SCAN 
	SELE astpat
	SEEK Comp1.NumMes+Comp1.NumCP+Comp1.CodCtc
    SELECT Temp
    APPEND BLANK
    REPLACE  Temp.CodCtc  WITH comp1.CodCtc;
    		 Temp.CodCaD  WITH comp.CodCaD;
     		 Temp.NumMes  WITH comp1.NumMes;
             Temp.FecChq  WITH comp1.Fecchq;
             Temp.TipDoc  WITH 'C/P';
             Temp.NumChq  WITH comp1.NumChq;
             Temp.NumREF  WITH comp1.Numcp;
             Temp.Concept WITH IIF(comp.TIPDOC='IN','I N U T I L I Z A D O',IIF(COMP.ESTADO='99','A N U L A D O',Comp1.NOMGIR));
             Temp.ValChqD WITH IIF(COMP.ESTADO#'99',IIF(EMPTY(COMP1.MtoHAB),COMP1.VALCHQ,0),0),;
             Temp.ValChqH WITH IIF(COMP.ESTADO#'99',IIF(EMPTY(COMP1.MtoDEB),COMP1.VALCHQ,0),0)
             
    SELECT Comp1
ENDSCAN
*ERASE(vInd)
DO Valida3
RETURN

FUNCTION xValida2
*--------------
SELECT COMP1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumCP TO (vInd1) FOR NUMMES=WMES
GO TOP
vCodCtc = comp1.CodCtc
vCodCaD = COMP1.CodCaD
vFecha  = comp1.FecCP
vNUMCHQ= IIF(EMPTY(COMP1.NUMCHQ_B),comp1.NUMCHQ,comp1.NumChq_B)
vNumERO = comp1.NumCP
vNumMes = comp1.NumMes
vMontoH = IIF(comp1.TIPDOC='IN',0,IIF(Comp1.ESTADO='99',0,COMP1.ValChq))
vConcept= IIF(comp1.TIPDOC='IN','I N U T I L I Z A D O',IIF(COMP1.ESTADO='99','A N U L A D O',COMP1.NOMGIR))
vTipDoc = "C/P"
SCAN 
     vCodCtc  = comp1.CodCtc
     vCodCaD  = COMP1.CodCaD
     vFecha   = comp1.FecCP
     vNUMCHQ  = COMP1.NumChq_B
     vNumERO  = comp1.NumCP
     vNumMes  = comp1.NumMes
     vMontoH  = IIF(comp1.TIPDOC='IN',0,IIF(Comp1.ESTADO='99',0,COMP1.ValChq))
	 vConcept= IIF(comp1.TIPDOC='IN','I N U T I L I Z A D O',IIF(comp1.ESTADO='99','A N U L A D O',COMP1.NOMGIR))
	 vTipDoc = "C/P"
     SELECT Temp
     APPEND BLANK
     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.CodCaD  WITH vCodCaD;
             Temp.TipDoc  WITH vTipDoc;
             Temp.FecChq  WITH vFecha;
             Temp.NumChq  WITH vNUMCHQ;
             Temp.NumREF  WITH vNumero;
             Temp.NumMes  WITH vNumMes;
             Temp.ValChqH WITH vMontoH;
             Temp.Concept WITH vConcept
   SELECT COMP1
ENDSCAN

*ERASE(vInd1)
DO Valida3
RETURN

FUNCTION Valida3
*--------------
SELECT hojanu1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumHA TO (vInd1) FOR NUMMES=WMes 
*INDEX ON CodCal+NumMes+NumHA TO (vInd1) FOR NUMMES=WMes 
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
     SELECT hojanu1
ENDSCAN
*ERASE(vInd1)
DO VALIDA4
RETURN

FUNCTION Valida4
*--------------
SELECT DIARIO1
vInd1 = SYS(3) + '.Idx'
INDEX ON COMPROB TO (vInd1) FOR SUBST(COMPROB,8,2)=WMes 
GO TOP
SCAN 
     vCodCtc  = DIARIO1.CodCtc
     vFecha   = DIARIO1.FecEMI
     vNUMCHQ  = DIARIO1.NumChq
	 vNumERO = SUBST(DIARIO1.COMPROB,4,4)
	 vNumMes = SUBST(DIARIO1.COMPROB,8,2)
	 vMontoH = IIF(DIARIO1.TIPO='H',DIARIO1.MONTOS,0)
     VMONTOD = IIF(DIARIO1.TIPO='D',DIARIO1.MONTOS,0)
	 vConcept= 'SALDO ANTERIOR'
	 VTIPDOC=  'APE'
  	 FMES=VAL(XMES)-1
	 FANO=VAL(XANO)-1
  	 AMES=IIF(FMES=0,'12',STR(FMES,2))
	 AANO=STR(FANO,2)
     IF AMES='12'
           	SELE SALBCO
			GO TOP
	  		SEEK vcodctC+Aano+Ames
		  	IF FOUND()
	  		    IF RLOCK()
	  	    	  REPLACE periodo WITH AANO, nummes WITH AMES, codctc WITH vcODCTC
		  	      REPLACE saldeb WITH vMONTOD, salhab WITH vMONTOH
	  		      UNLOCK
	  	    	ENDIF
			ELSE
				If f_appd()
	  	    	  REPLACE periodo WITH AANO, nummes WITH AMES, codctc WITH vcODCTC
		  	      REPLACE saldeb WITH vMONTOD, salhab WITH vMONTOH
	  		    ENDIF
		  	ENDIF
     ENDIF   
     SELEC DIARIO1
ENDSCAN
*ERASE(vInd1)
RETURN


PROCEDURE Salida
*---------------
RELEASE  WINDOW LibBan
ACTIVATE SCREEN
CLOSE    DATABASES
!DEL *.IDX
*!DEL *.CDX
*!DEL *.DBF
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