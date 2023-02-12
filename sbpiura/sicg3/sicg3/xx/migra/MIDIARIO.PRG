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

USE DIARIO IN 1 ORDER TAG DIARIO11 ALIAS DIAR

arcDiario = FCREATE('diario.xml')
IF arcDiario < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
*	linea = "<?xml version='1.0'?>"
	=FPUTS(arcDiario, linea)
	linea = "<sicg2011>"
	=FPUTS(arcDiario, linea)
ENDIF

SELE Diar
GO TOP
SCAN
	linea = "<diario>"
	=FPUTS(arcDiario, linea)
	linea = "<comprob>" + comprob + "</comprob>"
	=FPUTS(arcDiario, linea)
	linea = "<comadi>" + comadi + "</comadi>"
	=FPUTS(arcDiario, linea)
	linea = "<orden>" + diar.orden + "</orden>"
	=FPUTS(arcDiario, linea)
	linea = "<item>" + str(item,4) + "</item>"
	=FPUTS(arcDiario, linea)
	linea = "<fecemi>" + DTOC(fecemi) + "</fecemi>"
	=FPUTS(arcDiario, linea)
	linea = "<cuenta>" + cuenta + "</cuenta>"
	=FPUTS(arcDiario, linea)
	linea = "<tipo>" + tipo + "</tipo>"
	=FPUTS(arcDiario, linea)
	linea = "<monto>" + STR(montos,18,4) + "</monto>"
	=FPUTS(arcDiario, linea)
	linea = "<coddep>" + coddep + "</coddep>"
	=FPUTS(arcDiario, linea)
	linea = "<estado>" + estado + "</estado>"
	=FPUTS(arcDiario, linea)
	linea = "</diario>"
	=FPUTS(arcDiario, linea)
ENDSCAN

linea = "</sicg2011>"
=FPUTS(arcDiario, linea)
=FCLOSE(arcDiario)

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