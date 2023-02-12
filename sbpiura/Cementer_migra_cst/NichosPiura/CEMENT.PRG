*------------------------------------
* Cement.Prg
* Registra Cementerios tanto locales 
* como de otras juridicciones
* AUTOR : A. S. Marco Rodriguez Abad
*------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Cement IN  1 ORDER TAG Cement1 ALIAS Cement
USE Parmae IN  2 ORDER TAG Parmae1 ALIAS Parma
*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Cementerios'
vmens02 = ' Cemnterios : REVISION '
vmens04 = 'Dicho Cementerio no fue encontrado'
vmens05 = 'No existe Cementerio anterior'
vmens06 = 'No existe Cementerio siguiente'
vmens07 = '¨ Desea Anular ‚ste Cementerio ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Cementerio ha sido anulado'
vmens10 = 'El Cementerio ya est  Atendido'
vmens11 = 'El Cementerio ha sido devuelto'

SELECT Cement
GO TOP

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mmenu
ENDDO

DO fin_opcion

RETURN

PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
DEFINE PAD elimi   OF mmenu PROMPT '\<Eliminar'   AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
ON SELECTION PAD elimi  OF mmenu DO elimi
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi
RETURN

PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

 @  2, 10 SAY "        Codigo  :"
 @  4, 10 SAY "     Cementerio :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Cement

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@  2,30 SAY m.CodCem
@  4,30 SAY m.NomCem

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Cement
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Cement.CodCem   :H='C¢digo',;
	Cement.NomCem   :H='Cementerio'

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE Cement
DO vista
RETURN


PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vCodCem  = SPACE(2)

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA
@ 1,2 SAY 'Codigo :' GET vCodCem   PICTURE '!!'

READ VALID val_read()

DEACTIVATE WINDOW lisTA

IF EMPTY(vCodCem) .OR. LASTKEY()=27
	RETURN
ELSE
	SEEK vCodCem
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ELSE
		DO vista
	ENDIF
ENDIF
RETURN

PROCEDURE anter
*--------------
SELE Cement
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH vmens05
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE proxi
*--------------
SELE Cement
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH vmens06
	GO BOTTOM
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE corri
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
*****
SELECT Cement
SCATTER MEMVAR
DO pantalla

@  2,30 GET m.CodCem	PICTURE '!!' 	DISABLE
@  4,30 GET m.NomCem	FUNCTION 'S30'

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Cement
		GATHER MEMVAR
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT Cement
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Cement
op = ORDER()
vtemp = RECNO()

SCATTER MEMVAR BLANK
SELE Parma
SEEK "CORRELCEMENT"
m.CodCem = PADL(ALLTRIM(STR(Parma.NumEnt+1)),2,'0')
SELECT Cement

DO pantalla

@  2,30 GET m.CodCem	PICTURE '!!'   VALID Val_Cem()
@  4,30 GET m.NomCem	FUNCTION 'S30'

READ VALID val_read()

IF LASTKEY()=27
	go vtemp
	DO vista
	RETURN
ENDIF

IF !FOUND()
	IF F_Appd()
		GATHER MEMVAR
		UNLOCK
		SELE Parma
		SEEK "CORRELCEMENT"
		IF F_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
		UNLOCK
	ENDIF
 ELSE
	DO standby WITH 'Ya esta Registrado este Cementerio'
	DO vista
	RETURN
ENDIF

SELECT Cement

DO vista
RETURN

FUNCTION Val_Cem
*---------------
IF EMPTY(m.CodCem)
	DO StandBy WITH "El Codigo esta vacio"
	RETURN .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodCem)
		DO standby WITH 'Ya esta Registrado este Cementerio'
		RETURN .F.
	ENDIF
	RETURN .T.
ENDIF
RETURN


PROCEDURE elimi
*--------------
SELECT Cement
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Cementerio ?')
USE CUARTEL IN 0 ORDER TAG Cuartel1

IF SEEK(m.CodCem,"Cuartel")
	DO STANDBY WITH "No Puede Borrar este Cementerio por que tiene cuarteles"
	USE IN Cuartel
	SELECT Cement
	RETURN
ENDIF

USE IN Cuartel
SELECT Cement

IF velimina .AND. f_lock(1)
	DELE NEXT 1
	SKIP -1
	IF BOF()
		GO TOP
	ENDIF
ENDIF
UNLOCK ALL

DO vista
RETURN

PROCEDURE lista
*--------------
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW wind_0
RELEASE MENU   mmenu
*RESTORE SCREEN FROM principal
RETURN

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0069","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0070","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0071","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0072","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0073","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0074","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0075","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0076","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
