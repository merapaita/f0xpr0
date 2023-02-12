CLEA ALL
CLEA
CLOSE ALL
set escape off
set procedure to sai_lib
SET PATH TO ..\DATA
USE HojCon   IN 15  ORDER TAG HojCon1  ALIAS Hoja
USE Compag   IN 17  ORDER TAG Compag1  ALIAS compag

SET PATH TO ..\DATAPR
USE CRUCEPR IN 1 EXCL
SELE CRUCEPR 
ZAP

SELE compag
*index on NumMes+numcp+codctc to cruce1
SET INDEX TO CRUCE1
SELE HOJA
SCAN FOR estado#'99'
	if lastkey()=27
		exit
	endif
	vNummescp = hoja.nummescp
	vnumcp    = hoja.numcp
	vcodctc   = hoja.codctc
	vimptot   = hoja.imptot
	SELE COMPAG
	SEEK vnummescp+vnumcp+vcodctc
	?NUMCP+'            '+numhc
	IF FOUND()
		IF IMPORT!=vimptot
			SELE CRUCEPR
			APPEND BLANK
			REPLACE Numcp WITH Compag.numcp
			REPLACE mescp WITH COMPAG.NUMMES
			REPLACE numhc WITH compag.numhc
			REPLACE tothc WITH vimptot
			REPLACE reten WITH compag.reten
			REPLACE totcp WITH compag.import
			REPLACE cta	  WITH compag.codctc
*			IF YESNO("DESEA VERIFICAR ?")
*				susp
*				BROW
*			ENDIF
		ENDIF
	ENDIF
	SELE HOJA
ENDSCAN	
	

