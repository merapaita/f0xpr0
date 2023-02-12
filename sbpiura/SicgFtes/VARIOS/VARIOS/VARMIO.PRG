worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO Con_LIB
DO Fox_ambi
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'

SET PATH TO H:\BeneData\SicgData\DATA2005
PUBLIC vNumMes,cAno,vOpcion,OkCancel,cProc

CLOS DATA

DO Pantalla
DO Escoge
IF !EMPTY(cProc)
	DO &cProc
ENDIF

RETURN

PROCEDURE Pantalla
*-----------------
USE Parmae IN 0 ORDER TAG Parmae1 ALIAS Parma
SELE Parma
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vOpcion,OkCancel
cMes  = SPAC(2)
cAno  = YEAR(DATE())
@ 02,01 SAY "       Opciones : " 
@ 01,20 GET vOpcion  FUNCTION '^ Depurar;Blanquear DBS'
@ 07,01 SAY "  Periodo : "
@ 07,25 GET cAno PICTURE '9,999'
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
USE IN Parma
RETURN

PROCEDURE Escoge
*---------------
IF LASTKEY()=27 OR OkCancel = 2
	cProc = ''
	RETURN
ENDIF
IF !EMPTY(cAno)
	cRuta = "H:\BeneData\SicgData\DATA"+IIF(cAno<2000,RIGHT(STR(cano,4),2),STR(cano,4))
 ELSE
	DO StandBy WITH "Ruta Especificada no es correcta"
	RETURN .T.
ENDIF
SET PATH TO &cRuta
DO CASE
	CASE vOpcion = 1
		cProc = 'Depura'
	CASE vOpcion = 2
		cProc = 'Limpia'
	OTHERWISE
		cProc = ''
ENDCASE

RETURN

PROCEDURE Depura
*---------------
CLOS DATA
beep = chr(7)
escape = 27

ON ERROR DO ctrlerr WITH ;
	ERROR(), MESSAGE(), MESSAGE(1), PROGRAM(), LINENO()

ON KEY LABEL F10 DO Salir

SET EXCLUSIVE ON
SET TALK ON

mSalir = .T.
mErr = 0

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
USE IN Depura
RETURN

PROCEDURE Limpia
*---------------

SET PATH TO "H:\BeneData\SicgData\DATA2007"

CLOS DATA
beep = chr(7)
escape = 27

ON ERROR DO ctrlerr WITH ;
	ERROR(), MESSAGE(), MESSAGE(1), PROGRAM(), LINENO()

ON KEY LABEL F10 DO Salir

SET EXCLUSIVE ON
SET TALK ON

mSalir = .T.
mErr = 0

USE LimPia

GO TOP

SCAN
	USE (LimPia.Ruta) IN 2
	IF mErr = 1705
		?? Beep
		KTecla    = 0
		DO WHILE !USED(ALLTRIM(LimPia.Ruta)) OR KTecla <> Escape
			?" Abriendo Tabla "+ LimPia.ruta
			USE (LimPia.Ruta) IN 2
			KTecla    = INKEY()
		ENDDO
	ENDIF
	SELE 2
	?"depurando "+ LimPia.ruta
	ZAP
	SELE LimPia
ENDSCAN

ON ERROR
USE IN LimPia
RETURN


PROCEDURE ctrlerr
*----------------
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
