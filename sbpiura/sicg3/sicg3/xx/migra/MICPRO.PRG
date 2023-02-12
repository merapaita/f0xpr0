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


arcCompro = FCREATE('compro.sql')
IF arcCompro < 0
	DO StandBy WITH 'No puedo abrir o crear el archivo de salida'
ENDIF

SELE Compro
GO TOP
SCAN
	Linea = "INSERT INTO compro (comprob,comadi,tipdoc,fecha,glosa1,glosa2,glosa3,glosa4,totdeb,tothab,estado)"
	Linea = Linea + " VALUES ('" + ALLTRIM(comprob) + "','" + ComAdi + "','" + tipdoc + "','" + dtoc(fecha) + "','" + alltrim(glosa1) + "','" + alltrim(glosa2) + "','" + alltrim(glosa3) + "','" + alltrim(glosa4) + "'," + STR(totdeb,15,3) + "," + STR(tothab,15,3) + ",'" + estado +"');"
	=FPUTS(arcCompro, linea)
ENDSCAN
=FCLOSE(arcCompro)
