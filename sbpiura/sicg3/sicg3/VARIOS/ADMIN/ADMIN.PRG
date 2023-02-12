worker = IIF( LTRIM(UPPER(GETENV('WORKER'))) = 'ON', .T.,.F. )
SET PROCEDURE TO SAI_LIB
DO Fox_Ambi
PUBLIC cia,OkCancel,vOpcion,cAno
cia = 'SOCIEDAD DE BENEFICENCIA DE PIURA'
CLOS DATA

RUTA='H:\BeneData\SicgData\DATA2005'
RutaBk='..\BACKUP'
RUTApr='..\DataPr'

SET PATH TO &ruta

Prueba = .F.
IF WorKer
	IF YesNo("¨ Datos de Prueba ?")
		SET PATH TO &rutapr
		Prueba = .T.
	ENDIF
ENDIF

Rotulo1 ='Opciones de Administraci¢n'
c_fondo = 1
c_borde = 10

DO Pantalla
DO Escoge

PROCEDURE Pantalla
*-----------------
DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1 TO vOpcion,OkCancel
cMes  = SPAC(2)
cAno  = YEAR(DATE())
@ 02,01 SAY "       Opciones : " 
@ 01,20 GET vOpcion  FUNCTION '^ Sistemas;Modulos;Opciones;Niveles Acceso;Ctrl. Acceso '
@ 05,01 SAY "  Periodo a Reportar : "
@ 05,25 GET cAno PICTURE "9,999"
@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
RETURN

PROCEDURE Escoge
*---------------
IF LASTKEY()=27 OR OkCancel = 2
	cProc = ''
	RETURN
ENDIF

IF !Prueba
	IF !EMPTY(cAno)
		cRuta = "F:\Bene"+IIF(cAno<2000,RIGHT(STR(cano,4),2),STR(cano,4))+IIF(cAno#1996,"\Data","\DatBen")
	 ELSE
		DO StandBy WITH "Ruta Especificada no es correcta"
		RETURN .T.
	ENDIF
	SET PATH TO &cRuta
ENDIF

DO CASE
	CASE vOpcion = 1
		DO manpar1 WITH "Sistemas","SISTEM","Codigo","","Sistema"
	CASE vOpcion = 2
		DO manpar1 WITH "Modulos de Sistemas","MODULO","Cod. Sist.","Cod. Modulo","Modulo"
	CASE vOpcion = 3
		DO manpar1 WITH "Opciones de los Modulos","OPCION","Cod. Opc.","","Opci¢n"
	CASE vOpcion = 4
		DO CatAcc
	CASE vOpcion = 5
		DO CtrlAcc WITH "ADMIN"
	OTHERWISE
		cProc = ''
ENDCASE

