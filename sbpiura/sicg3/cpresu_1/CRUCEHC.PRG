USE AstPre IN 1 ORDER TAG AstPre1 ALIAS Astp
USE HojCon IN 2 ORDER TAG HojCon1 ALIAS Hoja
SELECT Hoja
GO TOP
SCAN 
	SCATTER MEMVAR 
	* guarda variables de memoria para luego adicionar
	SELECT AstP
	FOR i=1 TO 2
		* se deben de adicionar 2 movimientos por cada H/C
		APPEND BLANK
		REPLACE Periodo WITH m.Periodo, Nummes WITH m.Nummes , Numref WITH m.Numhc
		REPLACE Tipdoc  WITH 'H/C' , Fecref WITH m.Fechc, Codcal WITH m.Codcal
		REPLACE Codpart WITH m.Codpart, Codctc WITH m.Codctc
		IF i=1
			REPLACE Tipo with 'D', Ctadeb WITH m.Ctadeb , Cuenta WITH m.Ctadeb 
			REPLACE Valdeb WITH m.Valdeb 
		ELSE
			REPLACE Tipo with 'H', Ctahab WITH m.Ctahab , Cuenta WITH m.Ctahab 
			REPLACE Valhab WITH m.Valhab
		ENDIF	
	ENDFOR
	SELECT Hoja
ENDSCAN
Close All
Clear All