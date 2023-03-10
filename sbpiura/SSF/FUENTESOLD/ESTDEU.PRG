*****************************************
* EstDeu.Prg
* Reporte Estadistico de Deudas
*
* En este Reporte no salen los que tienen
* estado 90 es decir los recindidos.
******************************************

CLOS DATA

USE MaeSoc IN 1 ORDER TAG MaeSoc1
USE IteCob IN 2 ORDER TAG IteCob1

vDbf = SYS(3)+'.Dbf'
vIdx = SYS(3)+'.Idx'

SELE IteCob
COPY STRU TO (vDbf)

USE (vDbf) IN 3 ALIAS ResCob

*set step on
****************
STORE 1  TO vTi
DEFINE WINDOW LIS FROM 2,15 TO 20,65 FLOAT DOUBLE TITLE 'Estadistica de Deudas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 03,01 SAY "   Tiempo : " GET vTi PICTURE '@^De 0 a 3 Meses;De 3 a 6 Meses;De 6 a 9 Meses;De 9 a 12 Meses;M?s de 12 Meses' SIZE 1,15,6
*@ 13,01 SAY " Desde-Hasta : "
*@ 13,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN vtoFUE=2
*@ 13,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini) WHEN vtoFUE=2

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	
	DO Espera WITH 1,"Procesando Informaci?n"
	
	SELE IteCob
	GO TOP
	SCAN
		m.CodCon = CodCon
		SCAN WHILE m.CodCon = CodCon
			*
		ENDSCAN
		IF m.CodCon = CodCon
			*
		ELSE
			SKIP -1
			SCATTER MEMVAR
			SELE ResCob
			IF f_Appd()
				GATHER MEMVAR
			ENDIF
			SELE IteCob
		ENDIF
	ENDSCAN
	
	SELE ResCob
	
	SET RELATION TO CodCon INTO MaeSoc
	
	vIdx = SYS(3)+'.Idx'
	
*	xTiem = 2
	DO CASE
		CASE vTi = 1
			index on DTOC(feccob,1) TO (vIdx) FOR (DATE()-FECCOB)<=90 AND MaeSoc.Estado#'90' AND MaeSoc.Estado#'99'
			xTit = "REPORTE DE DEUDORES DE CERO A TRES MESES"
		CASE vTi = 2
			index on DTOC(feccob,1) TO (vIdx) FOR (DATE()-FECCOB)>90 AND (DATE()-FECCOB)<=180 AND MaeSoc.Estado#'90' AND MaeSoc.Estado#'99'
			xTit = "REPORTE DE DEUDORES DE TRES A SEIS MESES"
		CASE vTi = 3
			index on DTOC(feccob,1) TO (vIdx) FOR (DATE()-FECCOB)>180 AND (DATE()-FECCOB)<=270 AND MaeSoc.Estado#'90' AND MaeSoc.Estado#'99'
			xTit = "REPORTE DE DEUDORES DE SEIS A NUEVE MESES"
		CASE vTi = 4
			index on DTOC(feccob,1) TO (vIdx) FOR (DATE()-FECCOB)>270 AND (DATE()-FECCOB)<=365 AND MaeSoc.Estado#'90' AND MaeSoc.Estado#'99'
			xTit = "REPORTE DE DEUDORES DE NUEVE A DOCE MESES"
		CASE vTi = 5
			index on DTOC(feccob,1) TO (vIdx) FOR (DATE()-FECCOB)>365 AND MaeSoc.Estado#'90' AND MaeSoc.Estado#'99'
			xTit = "REPORTE DE DEUDORES MAYORES A UN A?O"
	ENDCASE
	
	DO Espera WITH 2
	
	GO TOP
	
	COPY TO XX FIELD CODCON,MAESOC.APELLI,MAESOC.NOMBRE,NUMCOB,RECCOB,FECCOB,FECPAG,CANCOB
			
	do reporte with 2,"EstDeu",'Estadistica de Deudas',2
	
ENDIF

USE IN ResCob
ERASE (vDbf)
ERASE (vIdx)
CLOS DATA
RETURN 
