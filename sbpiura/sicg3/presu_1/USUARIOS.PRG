*------------------------
* Usuarios.Prg
* Catalogo de Usuarios
*------------------------

USE Usuario IN 1 ORDER TAG Usuario1 ALIAS Usu
USE IteUsu  IN 2 ORDER TAG IteUsu1  ALIAS IteU
USE Parmae  IN 3 ORDER TAG Parmae1  ALIAS Parma

Vmens01 = 'Cat logo de Usuarios '
Vmens04 = 'Dicho Usuario no fue encontrado'
Vmens05 = 'No existe Usuario anterior'
Vmens06 = 'No existe Usuario siguiente'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'M¢dulos     [F3] Agregar Clave'

SELECT iteu
*SET FILT TO Sistema=vSistema
SELECT Usu
*SET FILT TO CODDEP='040' 
GO BOTTOM

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

RETURN

PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 12,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 13,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE " °   Alumnos:  ®F10¯ Selecciona  ° " COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula   '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO Elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @ 00, 02 SAY "                 Usuario :"
 @ 02, 02 SAY "                  Nombre :"
 @ 04, 02 SAY "             Dependencia :"
 @ 06, 02 SAY "           Observaciones :"
 @ 08, 02 SAY "         Clave de Acceso :"
 @ 10, 02 SAY " Repetir Clave de Acceso :"
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
ON KEY LABEL F9 DO Vista_Det

SELECT Usu
DO Pantalla
IF EOF()
	RETURN
ENDIF
ACTIVATE WINDOW Wind_0
SCATTER MEMVAR

@  0,30 SAY m.Usuario
@  2,30 SAY m.Nombre
@  4,30 SAY m.Observ

DO Vista_Hijo

RETURN

PROCEDURE Vista_Hijo
*-------------------
SELECT IteU
xUsu = ALLTRIM(Usu.Usuario)
SEEK xUsu
BROWSE;
	NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT TIMEOUT 0.001;
	WINDOW Wind_1 KEY xUsu FIELDS;
	Sistema  :H='M¢dulo',;
	xSist   = Val_sis() :H='Descripci¢n    ',;
	Modulo   :H='M¢dulo',;
	xMod    = Val_Mod() :H='Descripci¢n    ',;
	Opcion  :H='Opci¢n'
RETURN

FUNCTION Val_sis
*---------------
cAli = ALIAS()
SELE Parma
SEEK "SISTEM"+ALLTRIM(IteU.Sistema)
IF FOUND()
	cRet = ALLTRIM(Descri)
 ELSE
	cRet = "Sistema no Existe"
ENDIF
SELE (cAli)
RETURN cRet

FUNCTION Val_Mod
*---------------
cAli = ALIAS()
SELE Parma
SEEK "MODULO"+ALLTRIM(IteU.Sistema)+ALLTRIM(IteU.Modulo)
IF FOUND()
	cRet = ALLTRIM(Descri)
 ELSE
	cRet = "M¢dulo no Existe"
ENDIF
SELE (cAli)
RETURN cRet

PROCEDURE Vista_Det
*-------------------
SELECT IteU
xUsu = ALLTRIM(Usu.Usuario)
SEEK xUsu
BROWSE;
	NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT TIMEOUT .001;
		WINDOW Wind_1 KEY xUsu        FIELDS;
		Sistema  :H='M¢dulo',;
		xSist   = Val_sis() :H='Descripci¢n    ',;
		Modulo   :H='M¢dulo',;
		xMod    = Val_Mod() :H='Descripci¢n    ',;
		Opcion   :H='Opci¢n'
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
PRIVATE vTemp

SELECT Usu
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

BROWSE WINDOW Wind_2 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Usuario :H='Codigo',;
	Nombre  :H='Nombre',;
	Observ  :H='Observaci¢n'

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
SELECT Usu
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
vtemp    = RECNO()
ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese Nombre : '
@ 1,20 GET vNombre PICTURE '@!'

READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
	RETURN
 ELSE
	SEEK alltrim(vNombre)
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
SELE Usu
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
SELE Usu
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
ON KEY LABEL F9
SELE Usu
vTep = RECNO()
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
DO PANTALLA
SCATTER MEMVAR
@  0,30 GET m.Usuario DISABLED
@  2,30 GET m.Nombre
@  4,30 GET m.CodDep
@  6,30 GET m.Observ
READ VALID Val_Read()

IF LASTKEY()#27
	IF f_lock(1)
		GATHER MEMVAR
		UNLOCK
	ENDIF
	DO Trabaja_Hijo
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	GO VTEP
ENDIF

DO Vista
RETURN

PROCEDURE Ingre
*--------------
PRIVATE VTEP
ON KEY LABEL F9
SELE Usu
vTep = RECNO()
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
DO PANTALLA
SCATTER MEMVAR BLANK
m.Clave1 = m.Clave

@  0,30 GET m.Usuario
@  2,30 GET m.Nombre
@  4,30 GET m.CodDep
@  6,30 GET m.Observ
@  8,30 GET m.Clave 	VALID Repite()  COLOR ,X/W
@ 10,30 GET m.Clave1 					COLOR ,X/W
READ VALID Val_Cla()

m.Clave = chrtran(m.Clave,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
						  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')

IF LASTKEY()#27
	IF f_Appd()
		GATHER MEMVAR
		UNLOCK
	ENDIF
	DO Trabaja_Hijo
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	GO VTEP
ENDIF

DO Vista
RETURN

PROCEDURE Trabaja_Hijo
*---------------------
ON KEY LABEL F3 DO AgrCla
ON KEY LABEL F5 DO AgrItem
ON KEY LABEL F8 DO EliItem
ON KEY LABEL F10 KEYBOARD CHR(23)

SELE IteU
xUsu = ALLTRIM(Usu.Usuario)
SEEK xUsu
IF !FOUND()
	DO AgrItem
ENDIF

BROWSE NOAPPEND NODELETE NOMENU NOCLEAR WINDOW Wind_1 ;
		KEY xUsu    FIELDS;
		Sistema  :H='M¢dulo':V=V_Sis() :F,;
		xSist   = Val_sis() :H='Descripci¢n',;
		Modulo   :H='M¢dulo':V=V_Mod() :F,;
		xMod    = Val_Mod() :H='Descripci¢n',;
		Opcion   :H='Opci¢n'

ON KEY LABEL F3
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
RETURN

PROCEDURE AgrCla
*---------------
cClave3 = SPACE(10)
cClave4 = SPACE(10)
DEFINE WINDOW Pass FROM 05,10 TO 15,50 DOUBLE ;
	TITLE "Clave de Opciones del Sistema" COLOR SCHEME 10
	
ACTIVATE WINDOWS Pass

@ 01,03 SAY "       Sistema : "
@ 01,20 SAY Val_sis()
@ 03,03 SAY "        Modulo : "
@ 03,20 SAY Val_Mod()
@ 05,03 SAY "         Clave : "
@ 05,20 GET cClave3	 	VALID Repite() COLOR ,X/W
@ 07,03 SAY " Repetir Clave : "
@ 07,20 GET cClave4 	COLOR ,X/W
READ VALID Val_Cla1()

IF LASTKEY()#27
	cClave3 = chrtran(cClave3,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
							  
	REPLACE IteU.ClaMod WITH cClave3
 ELSE
	DO StandBy WITH "Proceso Cancelado"
ENDIF

DEACTIVATE WINDOWS Pass
RELEASE    WINDOWS Pass

PROCEDURE Repite
*---------------
DO StandBy WITH "Por Favor Repetir la Clave"
RETURN .T.


FUNCTION Val_Cla
*---------------
IF m.Clave # m.Clave1
	DO StandBy WITH "Clave Ingresada es Incorrecta. Intente de nuevo."
	m.Clave  = SPACE(10)
	m.Clave1 = SPACE(10)
	_CUROBJ  = OBJNUM(m.Clave)
	SHOW GETS
	RETURN .F.
 ELSE
	RETURN Val_Read()
ENDIF
RETURN .T.

FUNCTION Val_Cla1
*----------------
IF cClave3 # cClave4
	DO StandBy WITH "Clave Ingresada es Incorrecta. Intente de nuevo."
	cClave3 = SPACE(10)
	cClave4 = SPACE(10)
	_CUROBJ  = OBJNUM(cClave3)
	SHOW GETS
	RETURN .F.
 ELSE
	RETURN Val_Read()
ENDIF
RETURN .T.

PROCEDURE V_Sis
*--------------
SELE Parma
IF !SEEK("SISTEM"+ALLTRIM(IteU.Sistema)) OR EMPTY(IteU.Sistema)
	SET FILTER TO TIPO='SISTEM'
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD Codigo+ALLTRIM(Descri)
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
	SET FILTER TO
ENDIF
SELE IteU
REPLACE Sistema WITH ALLTRIM(Parma.Codigo)
RETURN

PROCEDURE V_Mod
*--------------
SELE Parma
IF !SEEK("MODULO"+ALLTRIM(IteU.Sistema)+ALLTRIM(IteU.Modulo)) OR EMPTY(IteU.Modulo)
	SET FILTER TO TIPO='MODULO'
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD CodigoAux+ALLTRIM(Descri)
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
	SET FILTER TO
ENDIF
SELE IteU
REPLACE Modulo WITH ALLTRIM(Parma.CodigoAux)
RETURN

PROCEDURE AgrItem
*----------------
IF f_Appd()
	REPLACE Usuario with Usu.Usuario
ENDIF
RETURN

PROCEDURE EliItem
*----------------
DELETE NEXT 1
RETURN


PROCEDURE TERMI
*--------------
vEn_accion = .F.
DEACTIVATE WINDOW Wind_0
DEACTIVATE WINDOW Wind_1
DEACTIVATE WINDOW Wind_2
DEACTIVATE MENU
ON KEY LABEL F9
RETURN

PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW Wind_2
RETURN


FUNCTION Password
*----------------
PARAMETERS pw_len
PRIVATE pw_len, pass_str, KEY, cur_curs

*PUSH KEY CLEAR

IF PARAMETERS() = 0           &&  if no parameters passed
	pw_len = 6                  &&  use password length of 6
ENDIF
IF pw_len <= 0                &&  Valida la longitud del password
	pw_len = 6
ELSE
	IF pw_len > 30
		pw_len =   30
	ENDIF
ENDIF
pass_str = ''
KEY = 0

cur_curs = SET('cursor') = 'ON'   &&  current cursor state
SET CURSOR ON
cur_win = WOUTPUT()

DEFINE WINDOW pass_win FROM 08,15 TO 15,65 DOUBLE ;
	COLOR SCHEME 21
ACTIVATE WINDOW pass_win
m.Usuario=SPACE(15)
@ 01,03 SAY 'USUARIO'
@ 01,11 GET m.Usuario VALID Val_Fun("Usu","Usuario","Nombre",@m.Usuario,1,2,11)
READ
@ 03,09 SAY 'Ingrese su password'
@ 04,12 SAY '(' + LTRIM(STR(pw_len)) + ' caracteres)'
@ 05,15 SAY ''
DO WHILE m.key # 13 .AND. m.key#27             &&  accept until car_return
	SET COLOR OF NORMAL TO x
	m.key = INKEY(0)

	DO CASE
		CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122)
			pass_str = pass_str + CHR(m.key)
		CASE m.key = 19 .OR. m.key = 127 .OR. m.key = 7
			@ ROW(),COL()-1 SAY ' '
			@ ROW(),COL()-1 SAY ''
			pass_str = SUBSTR(pass_str,1,LEN(pass_str)-1)
		OTHERWISE
	ENDCASE

	IF LEN(pass_str) >= pw_len
		m.key = 13
	ENDIF

	SET COLOR OF NORMAL TO
	@ 05,25 SAY SPACE(pw_len)
	@ 05,25 SAY REPLICATE('',LEN(pass_str))
ENDDO

RELEASE WINDOW pass_win

IF EMPTY(cur_win)
	ACTIVATE SCREEN
ENDIF

IF .NOT. cur_curs             &&  if cursor was off
	SET CURSOR OFF            &&  set it back on
ENDIF

*POP KEY
vEncrip = chrtran(UPPER(pass_str),'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                 'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')

IF vEncrip=ALLTRIM(Usua.Clave)
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF

RETURN(mRet)
