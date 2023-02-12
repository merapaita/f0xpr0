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

arcDiario = FCREATE('diario.sql')
IF arcDiario < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ENDIF

SELE Diar
GO TOP
SCAN
	Linea = "INSERT INTO Diario (comprob,comadi,orden,item,fecemi,cuenta,tipo,monto,coddep,estado)"
	Linea = Linea + " VALUES ('" + ALLTRIM(comprob) + "','" + ComAdi + "','" + Diar.Orden + "'," + str(Item,3) + ",'" + DTOC(FecEmi) + "','" + Cuenta + "','" + Tipo + "'," + str(montos,18,4) + ",'" + coddep + "','" + estado + "');"
	=FPUTS(arcDiario, linea)
ENDSCAN
=FCLOSE(arcDiario)
