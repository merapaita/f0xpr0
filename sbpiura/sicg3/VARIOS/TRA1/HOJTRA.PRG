* Realiza la Hoja de trabajo que se origina del proceso de ajuste del mes
* se muestra el saldo ajustado , Dajauno o Hajuano.
* Autor : Virna Alicia Vel squez Encalada

*CIA="SOCIEDAD DE BENEFICENCIA DE PIURA"


USE HojTra  IN 1 ORDER TAG Hojtra1 ALIAS HojTra
USE Parmae  IN 2 ORDER TAG Parmae1 ALIAS Parma
USE Cuentas IN 3 ORDER TAG Cuentas1 ALIAS Cuen

DECLARE NOMMES[12]
NomMes[01]="ENERO"
NomMes[02]="FEBRERO"
NomMes[03]="MARZO"
NomMes[04]="ABRIL"
NomMes[05]="MAYO"
NomMes[06]="JUNIO"
NomMes[07]="JULIO"
NomMes[08]="AGOSTO"
NomMes[09]="SETIEMBRE"
NomMes[10]="OCTUBRE"
NomMes[11]="NOVIEMBRE"
NomMes[12]="DICIEMBRE"
xmes=0
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
  xAno     = VAL(RIGHT(STR(IIF(FOUND(), YEAR(vmesact), YEAR(DATE())),4),2))
  
  @ 1, 03 SAY '            Mes y A¤o : ' GET xMes     PICTURE '99' ;
  VALID xMes > 0 .AND. xMes <= 12
  @ 1, 31 GET xAno                           PICTURE '99'
   @ 03,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
  READ
  
  IF LASTKEY() # 27 AND OKCANCEL = 1
	SELE HojTra
	SET FILTER TO IIF(xmes#12,VAL(nummes) = xmes,VAL(nummes) <= xmes ) AND VAL(ano)= xano AND SaldoMo<>0
	GO TOP
	IF EOF()
		DO STANDBY WITH ' No hay Registros para Procesar'
	ELSE
		DO REPORTE WITH 2, "HojTra1", 'Hoja de Trabajo',1,.F.,.T.
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
*   =ZZ()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN

FUNCTION ZZ
*----------
RETURN xCuenta


PROCEDURE Fin_Opcion
*---------------------
RELEASE    WINDOW Hojtra
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN
