PARAMETERS xDoc

arcCompro = FCREATE('compro.xml')
IF arcCompro < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
*             <?xml version="1.0" encoding="ISO-8859-1"?>
	=FPUTS(arcCompro, linea)
	linea = "<sicg2011>"
	=FPUTS(arcCompro, linea)
ENDIF

arcDiario = FCREATE('diario.xml')
IF arcCompro < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
*	linea = "<?xml version='1.0'?>"
	=FPUTS(arcDiario, linea)
	linea = "<sicg2011>"
	=FPUTS(arcDiario, linea)
ENDIF

SELE Compro
IF SEEK(xDoc)
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
	linea = "</sicg2011>"
	=FPUTS(arcCompro, linea)
	=FCLOSE(arcCompro)
	
	SELE Diar
	IF SEEK(xDoc)
		SCAN WHILE Comprob+Comadi=xDoc
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
	ENDIF
	linea = "</sicg2011>"
	=FPUTS(arcDiario, linea)
	=FCLOSE(arcDiario)
ENDIF

RUN FOXSWAP java -jar dist\migNCon.jar

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