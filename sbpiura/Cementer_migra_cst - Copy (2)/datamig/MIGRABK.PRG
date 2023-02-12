SET PROCEDURE TO libsis

worker = IIF(LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .t.,.f. )

DO Fox_Ambi

SET EXCLUSIVE ON

CLOS DATA

* Borrando Data

USE CAJA.DBF
ZAP
USE CEMENT.DBF
ZAP
USE CLIENTES.DBF
ZAP
USE CREDITOS.DBF
ZAP
USE CUARTEL.DBF
ZAP
USE DIFUNTO.DBF
ZAP
USE DONAC.DBF
ZAP
USE ITEPS.DBF
ZAP
USE ITEUSU.DBF
ZAP
USE ITEUSUOP.DBF
ZAP
USE KARCRE.DBF
ZAP
USE MAEACC.DBF
ZAP
USE MAUSOLEO.DBF
ZAP
USE NICHOS.DBF
ZAP
USE NICTMP.DBF
ZAP
USE NICTMP2.DBF
ZAP
USE OCUPFUT.DBF
ZAP
USE PAGACTA.DBF
ZAP
USE PAGSUB.DBF
ZAP
USE RECIBO.DBF
ZAP
USE RECIBOS.DBF
ZAP
USE SUBVENC.DBF
ZAP
USE TRANSFER.DBF
ZAP
USE TRASLADO.DBF
ZAP
USE VENTAS.DBF
ZAP

* Actualizando Cement

DO Espera WITH 1,"Actualizando Tabla Cementerios"

USE ..\DATACE~1\Maestro IN 1
USE Cement IN 2
SELE Maestro
GO TOP
SCAN
	m.CodCem = PADL(ALLTRIM(STR(Cod_Cement)),2,'0')
	m.NomCem = Nom_Cement
	SELE Cement
	APPEND BLANK
	GATHER MEMVAR
	SELE Maestro
ENDSCAN
USE in 1
USE IN 2

* Actualizando Cuartel

DO Espera WITH 1,"Actualizando Tabla Cuartel"

USE ..\DATACE~1\Cuartel IN 1 ALIAS CuarAnt
USE Cuartel IN 2 ALIAS CuarNew
SELE CuarAnt
SET FILTER TO Cod_tipo = 1
GO TOP

SCAN
	m.CodCem  = PADL(ALLTRIM(STR(Cod_Cement)),2,'0')
	m.CodCuar = PADL(ALLTRIM(STR(Cod_Cuarte)),4,'0')
	m.NomCuar = Nom_Cuarte
	
* por definir
*	m.Filas
*	m.Columnas

	SELE CuarNew
	APPEND BLANK
	GATHER MEMVAR
	SELE CuarAnt
ENDSCAN
USE in 1
USE IN 2

* Actualizando Difuntos

DO Espera WITH 1,"Actualizando Tabla Difuntos"

USE ..\DATACE~1\Fallecid IN 1
USE ..\DATACE~1\Cuartel  IN 2 ORDER TAG CUARTEL01
USE Difunto              IN 3

SELE Fallecid
GO TOP
SCAN
	SELE Cuartel
	IF SEEK (STR(Fallecid.Cod_Cement,1)+PADL(ALLTRIM(STR(Fallecid.Cod_Cuarte)),3,'0')) AND (Cod_tipo = 1)
		SELE Fallecid
		m.CodCem  = PADL(ALLTRIM(STR(Cod_Cement)),2,'0')
		m.CodCuar = PADL(ALLTRIM(STR(Cod_Cuarte)),4,'0')
		m.CodDif  = m.CodCem + PADL(ALLTRIM(STR(Cod_Fallec)),6,'0')
		m.Fila    = Fila
		m.Columna = PADL(ALLTRIM(STR(Columna)),3,'0')
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

*Actualizando NiTmp2
*Debe Copiarse de Nictmp y 
*agregarse los campos CodCem y CodCuar

DO Espera WITH 1,"Actualizando Tabla NicTmp2"

USE Difunto IN 1
USE NicTmp2 IN 2
USE Cuartel IN 3 ORDER TAG Cuartel1

SELE Difunto
INDEX ON CodCem+CodCuar+Fila+Columna TO xx

GO TOP
SCAN
	xCem = CodCem
	SCAN WHILE CodCem = xCem
		xCuar = CodCuar
		nCol = 0
		nfil = 0
		SCAN WHILE CodCuar = xCuar
			SELE NicTmp2
			APPEND BLANK
			REPLACE CodCem  WITH Difunto.CodCem,;
					CodCuar WITH Difunto.CodCuar,;
					Fila2	WITH Difunto.Fila
					
			SELE Difunto
			xFil = Difunto.Fila
			nFil = nFil + 1
			i = 0
			SCAN WHILE Fila = xFil
				SELE NicTmp2
				i = i + 1
				Campo = "Col" + PADL(ALLTRIM(STR(i)),3,'0')+'N'
				REPLACE &Campo WITH val(Difunto.Columna)
				IF nCol <  i
					nCol = i
				ENDIF
				SELE Difunto
			ENDSCAN
			SKIP -1
		ENDSCAN
		SKIP -1
		IF SEEK(Difunto.CodCem + Difunto.CodCuar,"Cuartel")
			SELE Cuartel
			REPLACE Filas WITH nFil,Columnas WITH nCol
		ELSE
			WAIT WIND "Existe un Error"
		ENDIF
		SELE Difunto
	ENDSCAN
ENDSCAN
USE IN 1
USE IN 2
USE IN 3

* Actualizando Nichos

DO Espera WITH 1,"Actualizando Tabla Nichos"

USE NicTmp2 IN 1
USE Nichos  IN 2 ORDER TAG Nichos1
USE Cuartel IN 3 ORDER TAG Cuartel1

SELE NicTmp2
SCAN
	xCem = CodCem
	SCAN WHILE CodCem = xCem
		xCuar = CodCuar
		nMin = 125
		nMax = 0
		nFil = 0
		SCAN WHILE CodCuar = xCuar
			nFil = nFil + 1
			FOR i = 1 TO 125
				Campo = "Col" + PADL(ALLTRIM(STR(i)),3,'0')+'N'
				IF EMPTY(&Campo)
					EXIT
				ENDIF
				IF nMin > &campo
					nMin = &campo
				ENDIF
				IF nMax < &Campo
					nMax = &Campo
				ENDIF
			ENDFOR
		ENDSCAN
		SKIP -1
		nCol = (nMax - nMin) + 1
		IF SEEK(CodCem + CodCuar,"Cuartel")
			SELE Cuartel
			REPLACE Filas WITH nFil,Columnas WITH nCol
		ELSE
			WAIT WIND "Existe un Error"
		ENDIF
		Sele Nichos
		FOR j = 1 TO nFil
			APPEND BLANK
			REPLACE CodCem WITH xCem, CodCuar WITH xCuar
			xNic = nMin
			FOR k = 1 TO nCol
				Campo  = "Col" + PADL(ALLTRIM(STR(k)),3,'0')+'N'
				Campo2 = "Col" + PADL(ALLTRIM(STR(k)),3,'0')+'E'
				REPLACE &Campo  WITH xNic
				REPLACE &Campo2 WITH "1"
				xNic = xNic + 1
			ENDFOR
		ENDFOR
		SELE NicTmp2
	ENDSCAN
	SKIP -1
ENDSCAN

USE IN 1
USE IN 2
USE IN 3

*Actualizando Campos Fila1 y Fila2

DO Espera WITH 1,"Actualizando Campos Fila1 y fila2"

USE Nichos IN 1
GO TOP
SCAN
	xCem = CodCem
	xCua = CodCuar
	xFil = 0
	cLet = 64
	SCAN WHILE CodCem+CodCuar = xCem + xCua
		xFil = xFil + 1
		cLet = cLet + 1
		REPLACE Fila1 WITH STR(xFil,1)
		REPLACE Fila2 WITH CHR(cLet)
	ENDSCAN
	SKIP -1
ENDSCAN

USE IN 1

* Agregando el Codigo de Nicho

DO Espera WITH 1,"Agregando Codigo de Nicho"

USE Difunto IN 1
USE Nichos  IN 2

SELE Difunto
INDEX ON CodCem+CodCuar+Fila+Columna TO xx
SELE Nichos
INDEX ON CodCem+CodCuar+Fila2 TO yy

SELE Difunto

*SET STEP ON

SCAN
	xCem = CodCem
	xCua = CodCuar
	xFil = Fila
	xCol = Columna
	SELE Nichos
	xCN = ''
	xF2 = ''
	xC2 = ''
	IF SEEK(xCem+xCua+xFil)
		xF2 = Fila1
		FOR i = 1 TO 255
			Campo  = "Col"+PADL(ALLTRIM(STR(i)),3,'0')+'N'
			Campo2 = "Col"+PADL(ALLTRIM(STR(i)),3,'0')+'E'
			IF VAL(xCol) = &Campo
				REPLACE &Campo2 WITH "3"
				xC2 = PADL(ALLTRIM(STR(i)),3,'0')
				EXIT
			ENDIF
		ENDFOR
	ENDIF
	xCN = xF2 + xC2
	SELE Difunto
	REPLACE CodNic WITH xCN
ENDSCAN

USE IN 1
USE IN 2

DO Espera WITH 2
