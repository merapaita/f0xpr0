*-----------------------------------------
* GruEsp.Prg
* Registra Grupos especificos de Articulos
* Autor: Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos
USE ArtMae   IN  1  ORDER TAG ArtMae1  ALIAS ArtMae
USE Parmae   IN  2  ORDER TAG Parmae1  ALIAS Parma

*- Mensajes de aviso al usuario
Vmens01 = 'Grupos Especificos de Articulos : REVISION '
Vmens02 = 'Registro de Grupos Especificos '
Vmens04 = 'Dicho Grupo Espec¡fico no fue encontrado'
Vmens05 = 'No existe Grupo Espec¡fico anterior'
Vmens06 = 'No existe Grupo Espec¡fico siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Grupo Espec¡fico ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Grupo Espec¡fico ha sido anulado'

SELECT ArtMae
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
HIDE POPUP ALL
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
vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO Logos WITH Rotulo1,vTempo

DEFINE WINDOW WINDO_0 FROM 00,00 TO 23,79  DOUBLE ;
TITLE Vmens01 COLOR SCHEME 10

DEFINE WINDOW Wind_1 FROM 14,01 TO 16,79 ;
TITLE ' Destino ' 

DEFINE MENU mMenu COLOR SCHEME 3
DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mMenu DO revis
ON SELECTION PAD busca  OF mMenu DO busca
ON SELECTION PAD anter  OF mMenu DO anter
ON SELECTION PAD proxi  OF mMenu DO proxi
ON SELECTION PAD corri  OF mMenu DO corri
ON SELECTION PAD ingre  OF mMenu DO ingre
ON SELECTION PAD anula  OF mMenu DO anula
ON SELECTION PAD lista  OF mMenu DO lista
ON SELECTION PAD termi  OF mMenu DO termi
RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW WINDO_0
CLEAR

 @  1, 2 SAY "         Tipo B/S :"
 @  3, 2 SAY "   Grupo Generico :"
 @  5, 2 SAY " Grupo Especifico :"
 @  7, 2 SAY "      Descripci¢n :"
 @  9, 2 SAY "            Fecha :"

RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT ArtMae
IF EOF()
	DO Pantalla
	RETURN
ENDIF
ACTIVATE WINDOW WINDO_0

SCATTER MEMVAR
vTip	= SUBSTR(m.CodArt,1,1)
vGruGen = SUBSTR(m.CodArt,2,2)
vGruEsp = SUBSTR(m.CodArt,5,3)

@  1,22 SAY Val_para(vTip ,'TIPPRO',' ',22,20)
@  3,22 SAY Val_GG(vGruGen,'CODGEB',' ',22)
@  5,22 SAY vGruEsp pICTURE '!!!'
@  7,22 SAY m.Descri
@  9,22 SAY m.fecReg

RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT ArtMae
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
BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	CodArt :H='  Codigo ' ,;
	Descri :h='   Descripci¢n  ':40

vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
	GOTO Vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
SET RELATION TO
DO Vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = RECNO()

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo      '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre      '

ON SELECTION POPUP pLista  DEACTIVATE POPUP

ACTIVATE POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		vBusca = SPACE(7)
		vNombre = "Codigo :"
		SET ORDER TO TAG ArtMae1
	CASE BAR() =  2
		vBusca = SPACE(30)
		vNombre = "Nombre : "
		SET ORDER TO TAG ArtMae2
	OTHERWISE
		vBusca = ''
		vNombre =''
		SET ORDER TO &cOrd1
ENDCASE

IF LASTKEY()#27
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
	
	ACTIVATE WINDOW lisTA
	@ 3,2 SAY vNombre GET vBusca
	READ VALID val_read()
	DEACTIVATE WINDOW lista
ENDIF

IF EMPTY(vBusca) OR LASTKEY()=27
	* No hace nada
 ELSE
	SEEK ALLTRIM(vBusca)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	 ELSE
		DO vista
	ENDIF
ENDIF
SET ORDER TO &cOrd1
RETURN


PROCEDURE Anter
*--------------
SELE ArtMae
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
SELE ArtMae
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

ACTIVATE WINDOW WINDO_0
DO PANTALLA

SELECT ArtMae
IF RLOCK() OR F_LOCK(1)
	SCATTER MEMVAR
	
	vTip	= SUBSTR(m.CodArt,1,1)
	vGruGen = SUBSTR(m.CodArt,2,2)
	vGruEsp = SUBSTR(m.CodArt,5,3)
	
	@ 1,22 GET vTip	PICTURE '!'   VALID Val_para(vTip ,'TIPPRO',' ',22,20)
	@ 3,22 GET vGruGen PICTURE '!!'  VALID Val_GG(vGruGen,'CODGEB',' ',22)
	@ 5,22 GET vGruEsp pICTURE '!!!'
	@ 7,22 GET m.Descri
	@ 9,22 GET m.fecReg
	
	READ VALID val_read()
	
	m.CodArt =vTip+vGruGen+'.'+GruEsp
	
	IF LASTKEY()=27 
		DO vista
		RETURN
	 ELSE
		GATHER MEMVAR
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	SELE ArtMae
	GO TOP
	GO vNum
ENDIF
UNLOCK ALL
FLUSH
SELECT ArtMae
DO Vista  
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
IF LASTKEY()=27
	DO STANDBY WITH 'Proceso cancelado'
	DO VISTA
	RETURN
ENDIF

DO PANTALLA
SELECT ArtMae
SCATTER MEMVAR BLANK
vNum = RECNO()

vTip	= SPACE(1)
vGruGen = SPACE(2)
vGruEsp = SPACE(3)

@ 1,22 GET vTip	PICTURE '!'   VALID Val_para(vTip ,'TIPPRO',' ',22,20)
@ 3,22 GET vGruGen PICTURE '!!'  VALID Val_GG(vGruGen,'CODGEB',' ',22)
@ 5,22 GET vGruEsp pICTURE '!!!'
@ 7,22 GET m.Descri
@ 9,22 GET m.fecReg

READ VALID val_read()

m.CodArt =vTip+vGruGen+'.'+GruEsp

IF LASTKEY()=27
	DO vista
	RETURN
ENDIF

SELE ArtMae
IF LASTKEY() # 27
	IF F_Appd()
		GATHER MEMVAR
	ENDIF
	
	*- Aumento el correlativo
	SELECT Parma
	SEEK 'CORREL'+'ORDENS'
	REPLACE NumEnt WITH NumEnt + 1
	IF vSino='S'
		SEEK 'CORREL'+'NUMCON'
		REPLACE NumEnt WITH NumEnt + 1
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	SELE ArtMae
	GO vNum
ENDIF
UNLOCK ALL
FLUSH
SELECT ArtMae
DO Vista
RETURN


PROCEDURE Anula
*--------------
do standby with "opcion todavia en proceso"
return

SELE ArtMae
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF

velimina = YESNO('¨ Desea ANULAR ‚sta Orden de Detalle ?')
IF vElimina
	SELE ArtMae
	IF RLOCK()
		REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
	ENDIF
	DO Vista
ENDIF
UNLOCK ALL
RETURN


PROCEDURE Lista
*--------------
do standby with "opcion todavia en proceso"
return

SELECT ArtMae
SCATTER MEMVAR

vtemp = recno()
if eof()
	do standby with vmens08
	return
 else
	DO LISSER
endif
select Ordse
go vtemp
DO VISTA
RETURN

PROCEDURE Termi
*--------------
vEn_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW WINDO_0
RELEASE WINDOW WIND_1
RELEASE MENU   mMenu
RESTORE SCREEN FROM PRINCIPAL
RETURN
