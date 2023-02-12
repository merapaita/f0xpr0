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

USE Cuentas IN 1 ORDER TAG Cuentas1
USE Glosas  IN 2 ORDER TAG Glosas1
USE folios  IN 3 ORDER TAG Folios1
USE Parmae  IN 4 ORDER TAG Parmae1
USE AstPat  IN 5 ORDER TAG AstPat1
USE AstPre  IN 6 ORDER TAG AstPre5
USE CatAsi  IN 7 ORDER TAG CatAsi5

arcCuentas = FCREATE('Cuentas.xml')
IF arcCuentas < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcCuentas, linea)
	linea = "<sicg2011>"
	=FPUTS(arcCuentas, linea)
	SELE Cuentas
	GO TOP
	SCAN WHILE !EOF()
		linea = "<Cuentas>"
		=FPUTS(arcCuentas, linea)
		linea = "<Cuenta>" + cuenta + "</cuenta>"
		=FPUTS(arcCuentas, linea)
		linea = "<descri>" + descri + "</descri>"
		=FPUTS(arcCuentas, linea)
		linea = "<nivel>" + str(nivel,2) + "</nivel>"
		=FPUTS(arcCuentas, linea)
		linea = "<detalle>" + detalle + "</detalle>"
		=FPUTS(arcCuentas, linea)
		linea = "<tipo>" + tipo + "</tipo>"
		=FPUTS(arcCuentas, linea)
		linea = "<dcontra>" + dcontra + "</dcontra>"
		=FPUTS(arcCuentas, linea)
		linea = "<hcontra>" + hcontra + "</hcontra>"
		=FPUTS(arcCuentas, linea)
		linea = "</Cuentas>"
		=FPUTS(arcCuentas, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcCuentas, linea)
	=FCLOSE(arcCuentas)
ENDIF

arcGlosas = FCREATE('Glosas.xml')
IF arcGlosas < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcGlosas, linea)
	linea = "<sicg2011>"
	=FPUTS(arcGlosas, linea)
	SELE Glosas
	GO TOP
	SCAN WHILE !EOF()
		linea = "<glosas>"
		=FPUTS(arcGlosas, linea)
		linea = "<ano>" + ano + "</ano>"
		=FPUTS(arcGlosas, linea)
		linea = "<tipo>" + tipo + "</tipo>"
		=FPUTS(arcGlosas, linea)
		linea = "<orden>" + orden + "</orden>"
		=FPUTS(arcGlosas, linea)
		linea = "<glosa>" + ALLTRIM(glosa)+' ' + "</glosa>"
		=FPUTS(arcGlosas, linea)
		linea = "</glosas>"
		=FPUTS(arcGlosas, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcGlosas, linea)
	=FCLOSE(arcGlosas)
ENDIF

arcFolios = FCREATE('Folios.xml')
IF arcFolios < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcFolios, linea)
	linea = "<sicg2011>"
	=FPUTS(arcFolios, linea)
	SELE Folios
	GO TOP
	SCAN WHILE !EOF()
		linea = "<folios>"
		=FPUTS(arcFolios, linea)
		linea = "<reporte>" + reporte + "</reporte>"
		=FPUTS(arcFolios, linea)
		linea = "<mes>" + mes + "</mes>"
		=FPUTS(arcFolios, linea)
		linea = "<periodo>" + periodo + "</periodo>"
		=FPUTS(arcFolios, linea)
		linea = "<orden>" + orden +' ' + "</orden>"
		=FPUTS(arcFolios, linea)
		linea = "<tipdoc>" + tipdoc +' ' + "</tipdoc>"
		=FPUTS(arcFolios, linea)
		linea = "<cuenta>" + cuenta +' ' + "</cuenta>"
		=FPUTS(arcFolios, linea)
		linea = "<dfolio>" + str(dfolio,5) +' ' + "</dfolio>"
		=FPUTS(arcFolios, linea)
		linea = "<hfolio>" + str(hfolio,5) +' ' + "</hfolio>"
		=FPUTS(arcFolios, linea)
		linea = "</folios>"
		=FPUTS(arcFolios, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcFolios, linea)
	=FCLOSE(arcFolios)
ENDIF

arcParmae = FCREATE('Parmae.xml')
IF arcParmae < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcParmae, linea)
	linea = "<sicg2011>"
	=FPUTS(arcParmae, linea)
	SELE Parmae
	GO TOP
	SCAN WHILE !EOF()
		linea = "<parmae>"
		=FPUTS(arcParmae, linea)
		linea = "<tipo>" + tipo + "</tipo>"
		=FPUTS(arcParmae, linea)
		linea = "<codigo>" + codigo + "</codigo>"
		=FPUTS(arcParmae, linea)
		linea = "<codigoaux>" + codigoaux + " " + "</codigoaux>"
		=FPUTS(arcParmae, linea)
		linea = "<descri>" + descri + " " + "</descri>"
		=FPUTS(arcParmae, linea)
		linea = "</parmae>"
		=FPUTS(arcParmae, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcParmae, linea)
	=FCLOSE(arcParmae)
ENDIF

arcAstPat = FCREATE('AstPat.xml')
IF arcAstPat < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcAstPat, linea)
	linea = "<sicg2011>"
	=FPUTS(arcAstPat, linea)
	SELE Astpat
	GO TOP
	SCAN WHILE !EOF()
		linea = "<astpat>"
		=FPUTS(arcAstPat, linea)
		linea = "<periodo>" + periodo + "</periodo>"
		=FPUTS(arcAstPat, linea)
		linea = "<tipdoc>" + tipdoc + "</tipdoc>"
		=FPUTS(arcAstPat, linea)
		linea = "<fecha>" + dtoc(fecha) + "</fecha>"
		=FPUTS(arcAstPat, linea)
		linea = "<nummes>" + nummes + " " + "</nummes>"
		=FPUTS(arcAstPat, linea)
		linea = "<numref>" + numref + " " + "</numref>"
		=FPUTS(arcAstPat, linea)
		linea = "<codcta>" + codcta + " " + "</codcta>"
		=FPUTS(arcAstPat, linea)
		linea = "<tipcta>" + tipcta + " " + "</tipcta>"
		=FPUTS(arcAstPat, linea)
		linea = "<mtodeb>" + STR(mtodeb,18,4) + "</mtodeb>"
		=FPUTS(arcAstPat, linea)
		linea = "<mtohab>" + STR(mtohab,18,4) + "</mtohab>"
		=FPUTS(arcAstPat, linea)
		linea = "<ret>" + ret + " " + "</ret>"
		=FPUTS(arcAstPat, linea)
		linea = "<codctc>" + codctc + " " + "</codctc>"
		=FPUTS(arcAstPat, linea)
		linea = "</astpat>"
		=FPUTS(arcAstPat, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcAstPat, linea)
	=FCLOSE(arcAstPat)
ENDIF

arcAstPre = FCREATE('AstPre.xml')
IF arcAstPre < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcAstPre, linea)
	linea = "<sicg2011>"
	=FPUTS(arcAstPre, linea)
	SELE AstPre
	GO TOP
	SCAN WHILE !EOF()
		linea = "<astpre>"
		=FPUTS(arcAstPre, linea)
		linea = "<periodo>" + periodo + "</periodo>"
		=FPUTS(arcAstPre, linea)
		linea = "<nummes>" + nummes + "</nummes>"
		=FPUTS(arcAstPre, linea)
		linea = "<numref>" + numref + "</numref>"
		=FPUTS(arcAstPre, linea)
		linea = "<tipdoc>" + tipdoc + "</tipdoc>"
		=FPUTS(arcAstPre, linea)
		linea = "<fecref>" + DTOC(fecref) + "</fecref>"
		=FPUTS(arcAstPre, linea)
		linea = "<codcad>" + codcad + "</codcad>"
		=FPUTS(arcAstPre, linea)
		linea = "<tipo>" + tipo + "</tipo>"
		=FPUTS(arcAstPre, linea)
		linea = "<ctadeb>" + ctadeb + "</ctadeb>"
		=FPUTS(arcAstPre, linea)
		linea = "<ctahab>" + ctahab + "</ctahab>"
		=FPUTS(arcAstPre, linea)
		linea = "<cuenta>" + cuenta + "</cuenta>"
		=FPUTS(arcAstPre, linea)
		linea = "<valdeb>" + STR(valdeb,18,2) + "</valdeb>"
		=FPUTS(arcAstPre, linea)
		linea = "<valhab>" + STR(valhab,18,2) + "</valhab>"
		=FPUTS(arcAstPre, linea)
		linea = "</astpre>"
		=FPUTS(arcAstPre, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcAstPre, linea)
	=FCLOSE(arcAstPre)
ENDIF

arcCatAsi = FCREATE('CatAsi.xml')
IF arcCatAsi < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ELSE
	linea = "<?xml version='1.0' encoding='ISO-8859-1'?>"
	=FPUTS(arcCatAsi, linea)
	linea = "<sicg2011>"
	=FPUTS(arcCatAsi, linea)
	SELE CatAsi
	GO TOP
	SCAN WHILE !EOF()
		linea = "<catasi>"
		=FPUTS(arcCatAsi, linea)
		linea = "<tippre>" + tippre + "</tippre>"
		=FPUTS(arcCatAsi, linea)
		linea = "<generic>" + generic + "</generic>"
		=FPUTS(arcCatAsi, linea)
		linea = "<sgn1>" + sgn1 + "</sgn1>"
		=FPUTS(arcCatAsi, linea)
		linea = "<sgn2>" + sgn2 + "</sgn2>"
		=FPUTS(arcCatAsi, linea)
		linea = "<espn1>" + espn1 + "</espn1>"
		=FPUTS(arcCatAsi, linea)
		linea = "<espn2>" + espn2 + "</espn2>"
		=FPUTS(arcCatAsi, linea)
		linea = "<espn3>" + espn3 + "</espn3>"
		=FPUTS(arcCatAsi, linea)
		linea = "<espn4>" + espn4 + "</espn4>"
		=FPUTS(arcCatAsi, linea)
		linea = "<detalle>" + detalle + "</detalle>"
		=FPUTS(arcCatAsi, linea)
		linea = "<codpart>" + codpart + "</codpart>"
		=FPUTS(arcCatAsi, linea)
		linea = "<descri>" + descri + "</descri>"
		=FPUTS(arcCatAsi, linea)
		linea = "</catasi>"
		=FPUTS(arcCatAsi, linea)
	ENDSCAN
	linea = "</sicg2011>"
	=FPUTS(arcCatAsi, linea)
	=FCLOSE(arcCatAsi)
ENDIF

