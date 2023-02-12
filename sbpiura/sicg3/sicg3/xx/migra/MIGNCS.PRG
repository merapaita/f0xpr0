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
USE Diario  IN 2  ORDER TAG Diario11 ALIAS Diar

SELE Compro
go top
xDoc = "170007703"
seek (xDoc)
SCAN while !EOF("Compro")
	xDoc = Comprob+ComAdi
	DO MigNC WITH xDoc
	SELE Compro
ENDSCAN