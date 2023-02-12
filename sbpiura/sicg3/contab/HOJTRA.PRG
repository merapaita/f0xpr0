* Realiza la Hoja de trabajo que se origina del proceso de ajuste del mes
* se muestra el saldo ajustado , Dajauno o Hajuano.
* Autor : 
PUBLIC nCampo1,nCampo2,nCampo3,nCampo4,cCuenta,cDescri,cCuenta1,cDescri1,nDif
STORE 0  TO nCampo1,nCampo2,nCampo3,nCampo4,nDif,nTAju
STORE "" TO cCuenta,cDescri,cCuenta1,cDescri1

USE HojTra  IN 1 ORDER TAG Hojtra1 ALIAS HojTra
USE Parmae  IN 2 ORDER TAG Parmae1 ALIAS Parma
USE Cuentas IN 3 ORDER TAG Cuentas1 ALIAS Cuen

xmes = 0
nDif = 0

DO Inicia
DO Fin_Opcion

PROCEDURE Inicia
*---------------
DEFINE WINDOW HojTra FROM 8,15 TO 14,65 FLOAT DOUBLE TITLE ' Hoja de Trabajo de Ajuste  ' COLOR SCHEME 5
ACTIVATE WINDOW HojTra

SELECT parma
SET ORDER TO Parmae1
SEEK 'MESANOACTUA'
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xAno     = IIF(FOUND(), YEAR(vmesact), YEAR(DATE()))

@ 1, 03 SAY '            Mes y A¤o : ' GET xMes     PICTURE '99' ;
			VALID xMes >= 0 .AND. xMes <= 12
@ 1, 31 GET xAno                           PICTURE '9,999'
@ 03,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

IF LASTKEY() # 27 AND OKCANCEL = 1
	SELE HojTra
	SET FILTER TO VAL(nummes) <= xmes AND VAL(ano)= xano 		&AND SaldoMo<>0
	GO TOP
	IF EOF()
		DO STANDBY WITH ' No hay Registros para Procesar'
	 ELSE
		* DO REPORTE WITH 2, "HojTra", 'Hoja de Trabajo',1,.F.,.T.
		DO REPORTE WITH 2, "HojTra2", 'Hoja de Trabajo',1,.F.,.T.
	ENDIF
ENDIF
RETURN


FUNCTION DESCRI
*--------------
PARAMETER vCuenta
vAli=ALIAS()
SELECT Cuen
SEEK vCuenta
IF FOUND()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN


PROCEDURE Fin_Opcion
*---------------------
RELEASE    WINDOW Hojtra
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN

FUNCTION fCampos
*---------------
cCampo1 = "Fac"+IIF(xMes<2,"   ",SUBSTR(cMes(xMes-1),1,3))
cCampo2 = "Aju"+IIF(xMes<2,"   ",SUBSTR(cMes(xMes-1),1,3))
*cCampo1 = "Fac"+IIF(xMes<1,"   ",IIF(xMes<1,'Ene',SUBSTR(cMes(xMes-1),1,3)))
*cCampo2 = "Aju"+IIF(xMes<1,"   ",IIF(xMes<1,'Ene',SUBSTR(cMes(xMes-1),1,3)))
cCampo3 = "Fac"+IIF(xMes<1,'   ',SUBSTR(cMes(xMes),1,3))
cCampo4 = "Aju"+IIF(xMes<1,'   ',SUBSTR(cMes(xMes),1,3))

nCampo1 = IIF(TYPE(cCampo1)='U',0,&cCampo1)
nCampo2 = IIF(TYPE(cCampo2)='U',0,&cCampo2)
nCampo3 = IIF(TYPE(cCampo3)='U',0,&cCampo3)
nCampo4 = IIF(TYPE(cCampo4)='U',0,&cCampo4)

RETURN nCampo1

FUNCTION fSuma
*-------------
PARAMETERS 	xNum
IF xNum = 0
	nTAju = nTAju + nCampo4
	nDif  = nDif  + (nCampo4-nCampo2)
 ELSE
	nTAju = 0
	nDif  = 0
ENDIF
RETURN 0


FUNCTION Ast
*-----------

IF nCampo4-ncampo2>0
	cCuenta  = Hojtra.CuenAju
	cDescri  = DESCRI(hojtra.cuenaju)
	cCuenta1 = "8980000000"
	cDescri1 = DESCRI("8980000000")
 ELSE
	cCuenta  = "8980000000"
	cDescri  = DESCRI("8980000000")
	cCuenta1 = Hojtra.CuenAju
	cDescri1 = DESCRI(Hojtra.cuenaju)
ENDIF
RETURN 0