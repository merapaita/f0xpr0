*------------------------------
* CorVF.Prg
* Corrige Venta de Focos
* Cruzada con Nifocos
*-------------------------------
clos data
USE CUARTEL IN 1 ORDER TAG CUARTEL1
USE NIFOCOS IN 2 ORDER TAG NIFOCOS1
USE vtafcos IN 3 ORDER TAG VtaFcos1

SELE cuartel
GO TOP

SCAN
	SELE Nifocos
	SEEK Cuartel.CodCem + Cuartel.CodCuar
	IF FOUND()
		mCol = Cuartel.Columnas
		SCAN WHILE CodCem + CodCuar = Cuartel.CodCem + Cuartel.codCuar
			FOR i = 1 TO mCol
				Campo1  = "Col"+ PADL(ALLTRIM(STR((i))),3,'0') + 'C'
				REPLACE &Campo1 WITH 0
			ENDFOR
		ENDSCAN
	ENDIF
ENDSCAN

SELE VtaFcos
GO TOP

SCAN
	cFila1 = LEFT(VtaFcos.CodNic,1)
	SELE Nifocos
	SEEK (VtaFcos.CodCem+VtaFcos.CodCuar+cFila1)
	IF FOUND()
		Campo1  = "Col"+ SUBSTR(VtaFcos.CodNic,2,3) + 'C'
		REPLACE &Campo1 WITH &Campo1 + 1
	ENDIF
ENDSCAN