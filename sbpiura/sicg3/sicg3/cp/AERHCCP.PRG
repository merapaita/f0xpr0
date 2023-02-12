
*--------------------------------------------------------------------------
* VerHcCp.Prg
* Verifica el importe entre la h/c y el c/p
* Autor: Julio Cruz Ortiz
*--------------------------------------------------------------------------
USE HojCon   IN 1  ORDER TAG HojCon1  ALIAS Hoja
USE Compag   IN 2  ORDER TAG Compag1  ALIAS compag

ACTIVATE WINDOW STANDBY
@ 1,14 SAY 'Espere un Momento ...' color W*
SELECT compag
vEsc = SET('ESCAPE')
SET ESCAPE OFF

CREATE TABLE vTemp ( Numcp C(7), NumHCX C(7),Numhc C(7), CodFte C(3),;
					 TotHc N(10,2),Totcp N(10,2), Reten N(10,2), Cta C(10))
SELE HOJA
SCAN FOR estado#'99' AND !EMPTY(NumHC)
	vNummes   = hoja.nummes
	vNummescp = hoja.nummescp
	vnumcp    = hoja.numcp
	vcodctc   = hoja.codctc
	vimptot   = hoja.imptot
	SELE COMPAG
	SEEK vnummescp+vnumcp+vcodctc
	IF FOUND()
		IF IMPORT!=vimptot
			SELE vTemp
			APPEND BLANK
			REPLACE Numcp WITH Compag.numcp+'.'+vnummescp
			REPLACE numhc WITH compag.numhc+'.'+vnummes
			REPLACE numhcX WITH HOJA.numhc+'.'+vnummes
			REPLACE CodFte WITH Hoja.CodFte
			REPLACE tothc WITH vimptot
			REPLACE reten WITH compag.reten
			REPLACE totcp WITH compag.import
			REPLACE cta	  WITH compag.codctc
		ENDIF
	ENDIF
	SELE HOJA
ENDSCAN	
DEACTIVATE WINDOW STANDBY
SELECT vTemp
IF !EOF()
	DO Reporte WITH 2,"VerHcCp","Verificaci¢n de H/C vs C/P"
ELSE
	DO STANDBY WITH "No se encontraron datos"
ENDIF
SET ESCAPE &vEsc
CLOSE DATA
RETURN