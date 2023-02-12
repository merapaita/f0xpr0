*- SegChq1.PRG
*- Seguimiento del Cheque
*- JCO & FMV
*- 15 de Febrero de 1996
*----------------------------

USE Parmae   IN 1  ORDER Parmae1  ALIAS Parma
USE ComPag   IN 2  ORDER ComPag5  ALIAS ComPag
USE Cheque   IN 4  ORDER Cheque4  ALIAS Cheque
USE Auxil    IN 5  ORDER Auxil6   ALIAS Auxil

DEFINE WINDOW Wind_1 FROM 10,20 TO 14,60  TITLE " Periodo de Consulta " FLOAT DOUBLE COLOR SCHEME 5
DEFINE WINDOW Wind_2 FROM 09,15 TO 17,68  FLOAT DOUBLE COLOR SCHEME 5
DEFINE WINDOW Wind_3 FROM 02,16 TO 22,64  TITLE "±± F2=BUSCAR ±±  PROVEEDORES  ±± F10=SALIR ±±" FLOAT DOUBLE COLOR SCHEME 10
DEFINE WINDOW Wind_4 FROM 17,16 TO 21,64  TITLE " ±± ESC = SALIR ±± " FLOAT DOUBLE COLOR SCHEME 5
DEFINE WINDOW Wind_8 FROM 08,26 TO 19,76  TITLE " B£squeda de Cheques " FLOAT DOUBLE COLOR SCHEME 5
DEFINE WINDOW Wind_9 FROM 1,0 TO 23,79;
COLOR SCHEME 02 TITLE "®®  Revisi¢n  de  Cheques  ¯¯"
STORE 0 TO vmtochq,vtpo
vnumchq=SPACE(14)
PRIVATE vchoice
DO WHILE .T.
 ACTIVATE WINDOW Wind_8
 vbus     = 1
 vest     = 1
 okcancel = 1
 @ 01,01  SAY " Tipo de B£squeda : " GET vBus PICTURE "@^ Por Monto;Por Proveedor;"
 @ 05,01  SAY " Monto del cheque : " 
 @ 05,22  GET   vmtochq PICTURE "999,999,999.99" WHEN vbus=1
 @ 08,10 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE VALID exi()
 DEACTIVATE WINDOW Wind_8
 vChoice = vBus
 IF LASTKEY()=27 OR okcancel = 2
    CLOSE DATA
    EXIT
 ENDIF
 DO CASE
 	CASE vChoice=1
	   DO Mtochq
 	CASE vChoice=2
	   DO Prov
 ENDCASE
ENDDO
RELEASE WINDOW Wind_2
RELEASE WINDOW wind_1
RELEASE WINDOW Wind_3
CLOSE DATA
SET EXACT OFF
RETURN

PROCEDURE MtoChq
*--------------
SET EXACT ON
vindm   = SYS(3)+'.IDX'
SELE CHEQUE
xord =ORDER()
INDEX ON Valchq TO (Vindm) FOR VAL(LEFT(CODPART,2))<15 AND VAL(LEFT(CODPART,2))>0
SEEK vmtochq
IF FOUND()
	DEFINE WINDOW Wind_16 FROM 02,00 TO 18,79  ;
	TITLE ' '+'Cheques con monto = '+ str(vmtochq,15,2)+' ' ;
    FOOTER " [F4] LISTAR " FLOAT DOUBLE COLOR SCHEME 02
    ON KEY LABEL F4 DO Imprimir 
    ON KEY LABEL F10 KEYBOARD CHR(23)
	ACTIVATE WINDOW Wind_16
    BROWSE IN WINDOW Wind_16 NOEDIT NODELETE NOMENU NOAPPEND KEY vmtochq FIELDS;
	Numchq                     :H='  Cheque', ;
	X1=Nummes+'.'+Numcp        :H='C/P',;
	Codctc					   ,;
	Fecchq					   :H='Fecha',;
	vestado = UPPER(verchest(estado)) :3 :H='  Estado', ; 
	nomgir  				   :25 :H= 'Girado a'
	DEACTIVATE WINDOW wind_16
ELSE
	DO STANDBY WITH " Cheque no existe "
ENDIF
SELE CHEQUE
SET INDEX TO
ERASE (vindm)
SET ORDER TO xord
RETURN


PROCEDURE Prov
*---------------
SET CONFIR OFF
vNom = SPACE(1)
vFecini = DATE()
vFecfin = DATE()

SELECT Auxil
SET ORDER TO AUXIL6
ON KEY LABEL F10  KEYBOARD CHR(23)
ON KEY LABEL F2   DO BUSCA
BROWSE WINDOW wind_3 FIELD ;
			DESCRI :H='                       NOMBRE' :47 NOEDIT NODELETE NOREFRE NOMENU
IF LASTKEY() = 27
	RETURN
ENDIF
vCodprv = Codigo
VDESCRI = Descri
ON KEY LABEL F10  
ON KEY LABEL F2   

DEFINE WINDOW Wind_6 FROM 06,00 TO 19,79  TITLE ' '+ALLTRIM(vdescri)+' ' ;
					 FOOTER " [F4] LISTAR " FLOAT DOUBLE COLOR SCHEME 02
ACTIVATE WINDOW Wind_1
@ 1,1  SAY " Desde : " GET vFecini
@ 1,18 SAY "    al : " GET vFecFin
READ 
DEACTIVATE WINDOW Wind_1

DEFINE WINDOW Xwait FROM 21,50 TO 23,75 COLOR SCHEME 05 
ACTIVATE WINDOW Xwait
@0,0 SAY " Consulta en Proceso...." COLOR W+/N*

*** Busca el proveedor o persona en compag

vdbf = SYS(3)
SELECT Compag
SET ORDER TO Compag5
SEEK ALLTRIM(vcodprv)
IF FOUND()
	CREATE TABLE (vdbf) ( Nummes C(2), Numcp C(4),CODCTC C(14))
	SELECT Compag
	SCAN WHILE Codprv  = ALLT(vCodprv)
		IF BETWEEN(Feccp,vFecini,vFecfin)
			SELECT C
			APPEND BLANK
			REPLACE Nummes  WITH Compag.Nummes
			REPLACE Numcp  WITH Compag.Numcp
			REPLACE Codctc WITH Compag.Codctc
		ENDIF
		SELECT Compag
	ENDSCAN
ELSE
	RELEASE WINDOW Xwait
	DO STANDBY WITH " No hay cheques para dicho proveedor "
	RETURN
ENDIF	

SELECT Ch.Numchq, Ch.Estado, Ch.Valchq, Ch.Fecchq, Ch.nummes, Ch.numcp, Ch.codctc ; 
FROM Cheque ch ,(vdbf) cp ; 
WHERE cp.Nummes = ch.nummes AND cp.Numcp = ch.Numcp  AND cp.codctc=ch.codctc; 
INTO CURSOR chq

RELEASE WINDOW Xwait
SELECT Chq
ON KEY LABEL F4 DO Imprimir 
ACTIVATE WINDOW Wind_6
BROWSE IN WINDOW Wind_6 NOEDIT NODELETE NOMENU NOAPPEND FIELD ;
	Numchq                     :H='  Cheque', ;
	Mes=Nummes				   ,;
	Numcp					   ,;		
	Codctc					   ,;
	Fecchq					   :H='Fecha',;
	vestado = verchest(estado) :H='  Estado', ;
	Valchq					   :H='  Monto ' :P='999,999,999.99'

USE
SELECT C
USE
DEACTIVATE WINDOW Wind_6
ON KEY LABEL F4 
RETURN


PROCEDURE Salir
*---------------
PARAMETERS vSal
IF LASTKEY()=-9
	vsal = .T.
ENDIF
RETURN .T.

PROCEDURE BUSCA
*-----------------
SET EXAC OFF
ACTIVATE WINDOW Wind_4
VNOM = SPACE(25)
@ 1,1 SAY " Nombre : " GET vNom 
READ && VALID salir(vsalir)
DEACTIVATE WINDOW Wind_4
vReg = RECNO()
SEEK ALLTRIM(vnom)
IF !FOUND()
	GO vReg
ENDIF
SET EXACT ON
RETURN

PROCEDURE IMPRIMIR
*-----------------
DO CASE
	CASE VBus=2 
		SELECT Chq
		DO REPORTE WITH 2,'ChqPrv','Consulta de cheques'
	CASE Vbus=1
	    SELECT CHEQUE
	    SET FILT TO valchq=vmtochq
		DO REPORTE WITH 2,'ChqMTo','Consulta de cheques'
		SET FILT TO
ENDCASE
RETURN

FUNCTION Novacio
*---------------
IF EMPTY(vnumchq) AND LASTKEY()#27 AND Okcancel#2
	DO standby WITH "N£mero de cheque est  en blanco"	
	RETURN .F.
ENDIF
RETURN .T.

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.


FUNCTION VerChest
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00'
      vFun ='Girado'
   CASE vEst='10'
      vFun ='Autorizado'
   CASE vEst='40'
      vFun ='Entregado'
   CASE vEst='99'
      vFun ='Anulado     '
   ENDCASE
RETURN vFun
