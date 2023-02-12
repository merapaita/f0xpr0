SET ESCAPE OFF
*set step on
CLEAR
CLOSE ALL

CLEAR ALL

worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )

SET PROCEDURE TO Con_lib

DO fox_ambi

SET CENTURY ON
SET DATE ymd
SET MARK TO '-'

SET PATH TO "H:\SICGDATA\DATA2011"

USE Compro  IN 1  ORDER TAG Compro1  ALIAS Compro


arcCompro = FCREATE('compro.xml')
IF arcCompro < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcCompro, linea)
	linea = "<sicg2011>"
	=FPUTS(arcCompro, linea)
ENDIF

SELE Compro
GO TOP
*IF SEEK(xDoc)
	SCAN
		linea = "<compro>"
		=FPUTS(arcCompro, linea)
		linea = "<comprob>" + comprob + "</comprob>"
		=FPUTS(arcCompro, linea)
		linea = "<comadi>" + comadi + "</comadi>"
		=FPUTS(arcCompro, linea)
		linea = "<tipdoc>" + tipdoc + "</tipdoc>"
		=FPUTS(arcCompro, linea)
		linea = "<fecha>" + DTOC(fecha) + "</fecha>"
		=FPUTS(arcCompro, linea)
		linea = "<glosa1>" + alltrim(glosa1) + SPACE(1) + "</glosa1>"
		=FPUTS(arcCompro, linea)
		linea = "<glosa2>" + alltrim(glosa2) + SPACE(1) + "</glosa2>"
		=FPUTS(arcCompro, linea)
		linea = "<glosa3>" + alltrim(glosa3) + SPACE(1) + "</glosa3>"
		=FPUTS(arcCompro, linea)
		linea = "<glosa4>" + alltrim(glosa4) + SPACE(1) + "</glosa4>"
		=FPUTS(arcCompro, linea)
		linea = "<totdeb>" + STR(totdeb,15,3) + "</totdeb>"
		=FPUTS(arcCompro, linea)
		linea = "<tothab>" + STR(tothab,15,3) + "</tothab>"
		=FPUTS(arcCompro, linea)
		linea = "<estado>" + estado + "</estado>"
		=FPUTS(arcCompro, linea)
		linea = "</compro>"
		=FPUTS(arcCompro, linea)
	ENDSCAN
	
	linea = "</sicg2011>"
	=FPUTS(arcCompro, linea)
	=FCLOSE(arcCompro)
*ENDIF

*RUN FOXSWAP java -jar dist\migNCon.jar

*CORRER PRG DE JAVA

*<?xml version="1.0"?>

*<kardex>
*	<usuarios>
*		<idusuario>3</idusuario>
* 		<nombre>Manuel Rodriguez Abad</nombre>
*  		<login>marco</login>
*  		<password>123</password>
*	</usuarios>
*	<usuarios>
*		<idusuario>4</idusuario>
*  		<nombre>Diego Vera Chinchay</nombre>
*  		<login>juan</login>
*  		<password>123</password>
*	</usuarios>
*</kardex>