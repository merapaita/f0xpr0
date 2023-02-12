*---------------------------------------------
* CamTip.Prg
* Cambio de Tipos para efectos de presentacion
* en reportes del libro diario resumen y mayor
* detallado
* A. S. Marco Rodriguez Abad
*---------------------------------------------
PUBLIC cPeriodo,vIndPar
CLOS DATA

SAVE SCREEN TO pta_comp

USE Tipos  IN 1 ORDER TAG Tipos1
USE Parmae IN 2 ORDER TAG Parmae1  ALIAS Parma

Vmens01 = ' Cat logo de Tipos '
Vmens08 = 'No hay registros para procesar'
Vmens04 = 'Dicho Tipo no fue encontrado'
Vmens05 = 'No existe Cambio anterior'
Vmens06 = 'No existe Cambio Siguiente'
vIndPar = SYS(3)+'.Idx'


SELE Parma
SEEK "MESANOACTUAL"
cPeriodo = LEFT(Descri,4)

DO Indexa_Par

SELE Tipos

DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista
*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO
DO Fin_opcion
SHOW POPUP ALL

RESTORE SCREEN FROM pta_comp

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE " °   Tipos:  ®F10¯ Selecciona  ° " COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD Elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD Elimi  OF mMenu DO Elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  3, 2 SAY "                            Periodo :"
 @  5, 2 SAY "           Tipo Original del Diario :"
 @  7, 2 SAY " Tipo para efecto de Reporte Diario :"
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Tipos
DO Pantalla
IF EOF()
	RETURN
ENDIF
ACTIVATE WINDOW Wind_0
SCATTER MEMVAR

@  3,42 SAY m.Periodo
@  5,42 SAY m.TipDoc1
@  5,44 SAY v_Tdoc(m.TipDoc1)
@  7,42 SAY m.TipDoc2
@  7,44 SAY v_Tdoc(m.TipDoc2)

RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
PRIVATE vTemp

SELECT Tipos
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF

HIDE MENU mMenu
ACTIVATE SCREEN
Vtemp = RECNO()
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)

BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Periodo :H='Periodo',;
	TipDoc1 :H='Tipo # 1',;
	TipDoc2 :H='Tipo # 2'
	
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo 
IF LASTKEY()=27
	GOTO Vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
DO Vista

RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
PRIVATE vTemp
SELECT Tipos
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vPeriodo = cPeriodo
vTipo = SPACE(2)
ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese Periodo y Tipo a buscar : '
@ 1,33 GET vPeriodo PICTURE '@!'
@ 1,39 GET vTipo PICTURE '@!'

READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
	RETURN
 ELSE
	SEEK vPeriodo + vTipo
	IF !FOUND()
		DO standby WITH Vmens04
		GOTO Vtemp
	 ELSE
		DO Vista
	ENDIF
ENDIF
RETURN

PROCEDURE Anter
*--------------
SELE Tipos
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH Vmens05
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Proxi
*--------------
SELE Tipos
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH Vmens06
	GO BOTTOM
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Corri
*--------------
PRIVATE VTEP
SELE Tipos
vTep = RECNO()
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
DO PANTALLA
SCATTER MEMVAR
@  3,42 GET m.Periodo 
@  5,42 GET m.Tipdoc1 VALID v_Tdoc(m.TipDoc1)
@  7,42 GET m.TipDoc2 VALID v_Tdoc(m.TipDoc2)

READ VALID Val_Read()

IF LASTKEY()=27
	DO STANDBY WITH 'Proceso cancelado'
	UNLOCK ALL
	GO VTEP
	DO Vista
	RETURN
ENDIF

IF LASTKEY()#27
	IF f_lock(1)
		GATHER MEMVAR
		UNLOCK
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
ENDIF

GO VTEP
DO Vista
RETURN


PROCEDURE Ingre
*--------------
PRIVATE VTEP
DO PANTALLA
SCATTER MEMVAR BLANK
m.Periodo = cPeriodo
m.Tipdoc1 = SPACE(2)
m.Tipdoc2 = SPACE(2)
SELE Tipos

vTep = RECNO()

@  3,42 GET m.Periodo
@  5,42 GET m.Tipdoc1 VALID v_Tdoc(m.TipDoc1)
@  7,42 GET m.TipDoc2 VALID v_Tdoc(m.TipDoc2)

READ VALID Val_Read()

IF LASTKEY()=27
	DO STANDBY WITH 'Proceso cancelado'
	UNLOCK ALL
	GO VTEP
	DO Vista
	RETURN
ENDIF
IF LASTKEY()#27
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
ENDIF
SELE Tipos
DO Vista
RETURN

PROCEDURE Elimi
*--------------
PRIVATE mCod
SELE Tipos
IF f_lock(1)
	DELETE NEXT 1
	UNLOCK
ENDIF

SKIP

IF EOF()
	GO BOTT
ENDIF
DO Vista

RETURN


PROCEDURE Lista
*--------------
SELE Parma
SET INDEX TO (vIndPar)
SELE Tipos
DO Reporte WITH 2,"Tipos",' Relacion de Tipos '
SELE Parma
SET ORDER TO Parmae1
DO Vista
RETURN

PROCEDURE TERMI
*--------------
vEn_accion = .F.
DEACTIVATE WINDOW Wind_0
DEACTIVATE WINDOW Wind_1
DEACTIVATE MENU
RETURN

PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
ERASE (vIndPar)
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RETURN

PROCEDURE Indexa_Par
*-------------------
SELECT parma
INDEX ON CodigoAux TO (vIndPar) FOR parma.tipo='TIPDOC' AND !EMPTY(parma.codigoaux)
SET ORDER TO Parmae1
RETURN .T.

FUNCTION v_Tdoc
*--------------
PARAMETERS cTipo
IF EMPTY(cTipo)
	vAli=ALIAS()
	SELECT Parma
	vOrd=ORDER()
	SET INDEX TO (vIndPar)
	DEFINE POPUP parametro FROM 0,40 PROMPT FIELD ALLTRIM(CodigoAUX)+' '+Descri TITLE ' Tipos de Documento '
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	RELEASE POPUP parametro
	SET ORDER TO vOrd
	cTipo=ALLTRIM(Parma.CodigoAux) &&SEGUNDO CODIGO DE PARMAE
	SELECT (vAli)
 ELSE
	vAli=ALIAS()
	SELECT Parma
	SET FILTER TO Tipo='TIPDOC'
	LOCATE FOR TIPO='TIPDOC' AND ALLTRIM(CODIGOAUX)=ALLTRIM(cTipo) 
	IF !FOUND()
		SET FILTER TO
		SELECT (vAli)
		RETURN .F.
	ENDIF
	SET FILTER TO
	SELECT (vAli)
ENDIF
IF !EOF("Parma")
	@ ROW(),COL()+2 SAY LEFT(Parma.descri,50)
 ELSE
	@ ROW(),COL()+2 SAY "Tipo de Documento no Existe."
ENDIF
RETURN

FUNCTION v_Tip
*-------------
PARAMETERS nOpc
cAlias = ALIAS()
SELE Parma
IF nOpc = 1
	SEEK Tipos.TipDoc1
 ELSE
	SEEK Tipos.TipDoc2
ENDIF

IF FOUND()
	mRet = LEFT(Parma.Descri,60)
 ELSE
	mRet = "Tipo no Existe. Revise"
ENDIF
SELE &cAlias
RETURN mRet