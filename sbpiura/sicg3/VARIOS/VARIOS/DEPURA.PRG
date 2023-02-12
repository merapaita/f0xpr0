PUBLIC mSalir,mErr
beep = chr(7)
escape = 27

ON ERROR DO ctrlerr WITH ;
	ERROR(), MESSAGE(), MESSAGE(1), PROGRAM(), LINENO()

ON KEY LABEL F10 DO Salir

SET EXCLUSIVE ON
SET TALK ON

mSalir = .T.
m.Err = 0

USE Depura

GO TOP

SCAN
	USE (Depura.Ruta) IN 2
	IF mErr = 1705
		?? Beep
		KTecla    = 0
		DO WHILE (!USED(Depura.Ruta)) OR KTecla <> Escape
			?"Tabla "+ Depura.ruta + ' Sin Acceso'
			USE (depura.Ruta) IN 2
			KTecla    = INKEY()
		ENDDO
	ENDIF
	SELE 2
	?"depurando "+ Depura.ruta
	REIN
	PACK
	SELE Depura
ENDSCAN

ON ERROR


PROCEDURE ctrlerr
PARAMETER merror, mens, mens1, mprog, mn£ml¡n

mErr = mError

DO Case
	CASE mError = 1705
		RETURN .T.
	OTHERWISE
		? 'N£mero de error: ' + LTRIM(STR(merror))
		? 'Mensaje de error: ' + mens
		? 'L¡nea de c¢digo con error: ' + mens1
		? 'N£mero de l¡nea de error: ' + LTRIM(STR(mn£ml¡n))
		? 'Programa con error: ' + mprog 
ENDCASE

PROCEDURE Salir
*--------------
mSalir = .F.
