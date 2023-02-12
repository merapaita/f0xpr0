* Actualizando Mausoleos

DO Espera WITH 1,"Actualizando Tabla Mausoleo"

USE ..\DATACE~1\Cuartel IN 1 ALIAS MauAnt
USE Mausoleo            IN 2 ALIAS MauNew
SELE MauAnt
SET FILTER TO BETW(Cod_tipo,3,4)
GO TOP

SCAN
	m.CodCem  = PADL(ALLTRIM(STR(Cod_Cement)),2,'0')
	m.CodMau  = PADL(ALLTRIM(STR(Cod_Cement)),2,'0') + PADL(ALLTRIM(STR(Cod_Cuarte)),4,'0')
	m.Tipo    = IIF(Cod_Tipo=4,1,2)
	m.Familia = Nom_Cuarte
	m.TotDif  = Tot_Nichos

	SELE MauNew
	APPEND BLANK
	GATHER MEMVAR
	SELE MauAnt
ENDSCAN
USE in 1
USE IN 2

* Actualizando Difuntos

DO Espera WITH 1,"Actualizando Difuntos enterrados en mausoleos"

USE ..\DATACE~1\Fallecid IN 1
USE ..\DATACE~1\Cuartel  IN 2 ORDER TAG CUARTEL01
USE Difunto              IN 3

SELE Fallecid
GO TOP
SCAN
	IF SEEK (STR(Fallecid.Cod_Cement,1)+PADL(ALLTRIM(STR(Fallecid.Cod_Cuarte)),3,'0'),'Cuartel') AND BETW(Cuartel.Cod_tipo,3,4)
		SELE Fallecid
		m.CodCem  = PADL(ALLTRIM(STR(Cod_Cement)),2,'0')
		m.CodMau  = m.CodCem + PADL(ALLTRIM(STR(Cod_Cuarte)),4,'0')
		m.CodDif  = m.CodCem + PADL(ALLTRIM(STR(Cod_Fallec)),6,'0')
		m.Fila    = ""
		m.Columna = ""
		m.ApePat  = Ape_patern
		m.ApeMat  = Ape_matern
		m.NomDif  = Nombres
		m.SexoDif = Sexo
		m.FecFall = Fec_Fallec
		
		SELE Difunto
		APPEND BLANK
		GATHER MEMVAR
	ENDIF
	SELE Fallecid
ENDSCAN
USE in 1
USE IN 2
USE IN 3

USE Difunto IN 1
USE mausoleo IN 2 ORDER TAG Mausoleo1

SELE Difunto
INDEX ON codmau TO xx FOR !EMPTY("CodMau")
GO TOP
SCAN
	xMau = CodMau
	nCon = 0
	SCAN WHILE CodMau = xMau
		nCon = nCon + 1
		IF SEEK(xMau,"Mausoleo")
			SELE Mausoleo
			REPLACE NumDif WITH nCon
		ELSE
			WAIT WIND "Error Al agregar num de difunto en mausoleo"
		ENDIF
		SELE Difunto
	ENDSCAN
	SKIP -1
ENDSCAN
