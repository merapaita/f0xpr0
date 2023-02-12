*- CatCaj.prg
*- Universidad de Piura - 96
*- Permite el registro de nuevas cuentas
*- autor MMDELA
*- UDEP-1996
****************************************
PUBLIC vIndPar

USE Parmae IN 1 ORDER TAG Parmae1 ALIAS Parma
USE GLOSAS IN 3 ORDER TAG GLOSAS1 ALIAS GLOSA

DO Indexa_Par

*- Variables de trabajo (registro a trabajar)
SELECT glosa
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Mensajes de aviso al usuario
Vmens01 = ' Registro de Glosas '
Vmens02 = 'Revisi¢n de Glosas'
Vmens04 = 'Dicha Glosa no fue encontrada'
Vmens05 = 'No existe Glosa anterior'
Vmens06 = 'No existe Glosa siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚sta Glosa ?'
Vmens08 = 'No hay registros para procesar'

*Vmens09 = 'Esta Caja ha sido anulada'
*Vmens10 = 'La Caja ya fue atendida'


*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79  ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @ 03,02 SAY "        A¤o :"
 @ 05,02 SAY "       Tipo :"
 @ 07,02 SAY "      Orden :"
 @ 09,02 SAY "      Glosa :"
 
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT glosa
IF EOF()
	DO Pantalla
	RETURN
ENDIF
ACTIVATE WINDOW Wind_0
SCATTER MEMVAR
@ 03,18 SAY m.ano
@ 05,18 SAY m.tipo
@ 07,18 SAY m.orden
@ 09,18 SAY SUBSTR(m.GLOSA,01,54)
@ 10,18 SAY SUBSTR(m.GLOSA,55,54)
@ 11,18 SAY SUBSTR(m.GLOSA,110,54)
@ 12,18 SAY SUBSTR(m.GLOSA,165,35)
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE glosa
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
Vtemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	ano   :H='a¤o' ,;
	tipo  :H='Tipo',;
	ORDEN :H='Orden',;
	GLOSA :H='GLOSA'
	vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO Logos WITH Rotulo1,vTempo
	
IF LASTKEY()=27
	GOTO Vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
DO Vista
RETURN

PROCEDURE Busca
*--------------
SELECT glosa

IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

Vtemp = RECNO()

STORE SPACE(02) TO vGen
STORE SPACE(04) TO vano

ACTIVATE WINDOW StandBy
@ 1,03 SAY "   A¤o:" GET vANO
@ 1,14 SAY "  Tipo:" GET vGen
READ
DEACTIVATE WINDOWS StanBy
RELEASE WIND STANDBY
IF EMPTY(vano) and EMPTY(vGen) .OR. LASTKEY()=27
	GOTO Vtemp
 ELSE
	SEEK vano+vGen
	IF !FOUND()
		DO standby WITH "Dicho tipo no est  registrada"
		GOTO Vtemp
	 ELSE
		DO Vista
	ENDIF
ENDIF

RETURN

PROCEDURE Anter
*--------------
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
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF

ACTIVATE WINDOW Wind_0
SELECT glosa
SCATTER MEMVAR
IF RLOCK() OR F_LOCK(1)
	@ 03,18 GET m.ano   DISABLE
	@ 05,18 GET m.tipo  DISABLE
	@ 07,18 GET m.orden DISABLE
	@ 09,18 GET m.glosa PICTURE '@S54'
	@ 10,18 CLEAR TO 12,70
	READ VALID Val_Read()
	IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
		SELECT glosa
		GATHER MEMVAR
	ENDIF
	DO Vista                    && Muestra nuevos datos
ENDIF
UNLOCK
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT glosa

nRegistro = RECNO()

DO Pantalla
SCATTER MEMVAR BLANK

m.Ano = IIF(SEEK("MESANOACTUAL","Parma"), LEFT(Parma.Descri,4), STR(YEAR(DATE()),4))

@ 03,18 GET m.ano   
@ 05,18 GET m.tipo	VALID v_Tdoc()
@ 07,18 GET m.orden	WHEN  w_Ord()  VALID v_Ord()
@ 09,18 GET m.glosa	PICTURE '@S54'
@ 10,18 CLEAR TO 12,70
READ VALID Val_Read()
IF LASTKEY() # 27
	IF F_Appd()
		GATHER MEMVAR
	 ELSE
		GO BOTTOM
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado, no se graba nada'
	GO nRegistro
ENDIF
UNLOCK ALL
SELECT glosa
DO Vista
RETURN

PROCEDURE Elimi
*--------------
SELECT glosa
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚sta glosa ?')
IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	DELETE NEXT 1
	GO BOTTOM
	DO Vista
ENDIF
UNLOCK
RETURN

PROCEDURE Lista
*--------------
select glosa
vtemp =recno()
if eof()
	do standby with vmens08
	return
 else
	SET ORDER TO GLOSAS1
	do reporte with 2,"Lisglo",' Listado de Glosas '
Endif

SELE glosa
GO vtemp
do Vista
RETURN


PROCEDURE Termi
*--------------
vEn_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE MENU   mMenu
RESTORE SCREEN FROM PRINCIPAL
RETURN

FUNCTION v_Tdoc
*--------------
IF EMPTY(m.Tipo)
	vAli=ALIAS()
	vOrd=ORDER()
	SELECT Parma
	SET INDEX TO (vIndPar) 
	DEFINE POPUP parametro FROM 0,40 PROMPT FIELD ALLTRIM(CodigoAUX)+' '+Descri TITLE ' Tipos de Documento '
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	RELEASE POPUP parametro
	SET ORDER TO PARMAE1
	m.Tipo=ALLTRIM(Parma.CodigoAux) &&SEGUNDO CODIGO DE PARMAE
	@ 05,21 SAY ALLTRIM(Parma.Descri)
	SELECT (vAli)
	SET ORDER TO vOrd
 ELSE
	vAli=ALIAS()
	vOrd=ORDER()
	SELECT Parma
	LOCATE FOR TIPO='TIPDOC' AND ALLTRIM(CODIGOAUX)=ALLTRIM(m.Tipo)
	IF !FOUND()
		SELECT (vAli)
		SET ORDER TO vOrd
		RETURN .F.
	ENDIF
	@ 05,21 SAY ALLTRIM(Parma.Descri)
	SELECT (vAli)
	SET ORDER TO vOrd
ENDIF
RETURN

PROCEDURE Indexa_Par
*-------------------
SELECT parma
cOrd = ORDER()
vIndPar   = SYS(3)+".IDX"
INDEX ON CodigoAux TO (vIndPar) FOR parma.tipo='TIPDOC' AND !EMPTY(parma.codigoaux)
SET ORDER TO &cOrd
RETURN .T.

FUNCTION w_Ord
*-------------
SELE Glosa
SEEK m.Ano+m.Tipo
IF !FOUND()
	m.Orden = '01'
 ELSE
	SCAN WHILE Glosa.Tipo=m.Tipo
		m.Orden = Glosa.Orden
	ENDSCAN
	m.Orden = PADL(ALLTRIM(STR(VAL(m.Orden)+1)),2,'0')
ENDIF

RETURN .T.

FUNCTION v_Ord
*-------------
mRet = .F.
SELE Glosa
SEEK m.Ano+m.Tipo+m.Orden
IF FOUND()
	DO StandBy WITH "Glosa ya Existe. Por favor Revise"
	mRet = .F.
 ELSE
	mRet = .T.
ENDIF

RETURN mRet
