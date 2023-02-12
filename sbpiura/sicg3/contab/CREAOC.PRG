USE ComPag   IN  1  ORDER TAG ComPag1      ALIAS ComPag 
USE HojCon   IN  2  ORDER TAG HojCon1      ALIAS Hoja   
USE ordcom   IN  3  ORDER TAG ordcom1      ALIAS orden
SELECT compag
SET FILTER TO TIPDOC='HC' AND DOCREF='H/C'
GO TOP
SCAN
	scatter memvar
	SELECT hoja
	seek compag.nummeshc+compag.numhc
	IF hoja.tipdoc='O/C'
		SELECT orden
		SEEK '98'+hoja.numref
		IF FOUND()
			REPLACE Percp WITH m.nummes,;
		        numcp WITH m.numcp
		ENDIF
	ENDIF
	SELECT compag
ENDSCAN


