**------------------------------------------------------------------------
** CtrlAcc.PRG - 2003
** Control de Accesos al sistema
** AUTOR: M.E.R.A
** 
**------------------------------------------------------------------------
PARAMETERS v_Us

SET PATH TO ..\KD\DATA;..\KD\DATA\2003

*- Mensajes de aviso al usuario
Vmens01 = 'ÍÍ Seguridad ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'+Cia+'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
Vmens02 = 'Revisi¢n de Seguridad de '+Cia
Vmens03 = 'Digite C¢digo de Usuario que desea :'
Vmens04 = 'Dicho Usuario no fue encontrado'
Vmens05 = 'No existe Usuario anterior'
Vmens06 = 'No existe Usuario siguiente'
Vmens07 = '¨ Est  seguro que desea ANULAR este Usuario ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'M¢dulos'
Vmens10 = '¨ Est  seguro que desea ANULAR este m¢dulo ?'

*- Abre archivos y se posiciona en £ltimo registro
USE Usuario  IN 1  ORDER TAG Usuario1  ALIAS Usua
USE IteUsuOp IN 2  ORDER TAG IteUsuOp1 ALIAS IteU
USE MaeAcc   IN 3  ORDER TAG MaeAcc1   ALIAS MaeAcc
USE Parmae   IN 4  order tag Parmae1   ALIAS Parma
SAVE SCREEN TO Pantalla
HIDE POPUP ALL
SELECT Usua
IF v_Us#"ADMIN"
	SET FILT TO Usuario=v_Us
ENDIF
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
DO Inicia                    && Define ventanas, men£s, t¡tulos

*- Inicia proceso
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
	ACTIVATE SCREEN
	ACTIVATE MENU fMenu
ENDDO

DO Fin_opcion

RETURN

PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW Wind_F0
RELEASE WINDOW Wind_F1
RELEASE WINDOW Wind_F2
RELEASE MENU   fMenu
RESTORE SCREEN FROM Pantalla
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vtempo

 DEFINE WINDOW Wind_F0 FROM 00,00 TO 08,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_F1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_F2 FROM 08,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE MENU fMenu COLOR SCHEME 3
 DEFINE PAD revis   OF fMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF fMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF fMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF fMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF fMenu PROMPT '\<Corrige'    AT 24,36		SKIP FOR v_Us#'ADMIN'
 DEFINE PAD ingre   OF fMenu PROMPT '\<Ingresa'    AT 24,45		SKIP FOR v_Us#'ADMIN'
 DEFINE PAD elimi   OF fMenu PROMPT '\<Elimina'    AT 24,54		SKIP FOR v_Us#'ADMIN'
 DEFINE PAD lista   OF fMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF fMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF fMenu DO revis
 ON SELECTION PAD busca  OF fMenu DO busca
 ON SELECTION PAD anter  OF fMenu DO anter
 ON SELECTION PAD proxi  OF fMenu DO proxi
 ON SELECTION PAD corri  OF fMenu DO corri
 ON SELECTION PAD ingre  OF fMenu DO ingre
 ON SELECTION PAD elimi  OF fMenu DO elimi
 ON SELECTION PAD lista  OF fMenu DO lista
 ON SELECTION PAD termi  OF fMenu DO termi

RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_F0
 CLEAR
 @  1, 2 SAY "          Usuario :"
 @  3, 2 SAY "           Nombre :"
 @  4, 2 SAY "      Dependencia :"
* @  5, 2 SAY "            Nivel :" 
 @  6, 2 SAY "    Observaciones :"
 RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta dato
*--------------
 ON KEY LABEL F3 DO PerCla
 ON KEY LABEL F9 DO Vista_Det
 ACTIVATE WINDOW Wind_F0
 SELECT Usua
 SCATTER MEMVAR
 @  1,22 SAY m.Usuario
 @  3,22 SAY m.Nombre
 @  4,22 SAY val_para(m.CodDep,'CODDEP','A',22,50,7)
* @  5,22 SAY m.Numdep PICT '99'  
 @  6,22 SAY m.Observ

 DO Vista_Hijo
 RETURN

PROCEDURE Vista_Hijo
*-------------------
SELECT IteU
vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                               'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')

SEEK ALLTRIM(vUsuCla)
BROWSE;
	NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT TIMEOUT .001;
	WINDOW Wind_F2 KEY ALLTRIM(vUsuCla)    FIELDS;
		xSis = IIF(!EMPTY(Sistema),val_para(Sistema,'SISTEM','D'),Sistema):30 :H='Sistema',;
		xMod = IIF(!EMPTY(Modulo),val_para1(modulo,'MODULO'+Sistema,'D'),Modulo):30 :H='M¢dulo',;
		xOpc = IIF(!EMPTY(Opcion),val_para(opcion,'OPCION','D'),Opcion):10 :H='Opci¢n'
RETURN


PROCEDURE Vista_Det
*------------------
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteU
vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                               'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vUsuCla)
IF FOUND()
	ACTIVATE WINDOW Wind_F2
	BROWSE;
		NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT;
		WINDOW Wind_F2 KEY ALLTRIM(vUsuCla)    FIELDS;
			xSis = IIF(!EMPTY(Sistema),val_para(Sistema,'SISTEM','D'),Sistema):30 :H='Sistema',;
			xMod = IIF(!EMPTY(Modulo),val_para1(modulo,'MODULO'+Sistema,'D'),Modulo):30 :H='M¢dulo',;
			xOpc = IIF(!EMPTY(Opcion),val_para(opcion,'OPCION','D'),Opcion):10 :H='Opci¢n'
 ELSE
	ACTIVATE WINDOW Wind_F2
	CLEAR
	@ 5,10 SAY "USUARIO NO TIENE DETALLE..."
ENDIF
ON KEY LABEL F10

RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F3
ON KEY LABEL F9
SELECT Usua
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
Vtemp = RECNO()
HIDE MENU fMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW Wind_F1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	Usuario :H='Usuario',;
	Nombre  :H='Nombre',;
	Observ  :H='Observaciones'
	
vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
	GOTO Vtemp
 ELSE
	SCATTER MEMVAR
ENDIF
SHOW MENU fMenu
ON KEY LABEL F10
DO Vista
RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
ON KEY LABEL F3
ON KEY LABEL F9
SELECT Usua
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
Vtemp = RECNO()
ACTIVATE WINDOW standby
STORE SPACE(LEN(m.Usuario))  TO Vbusca
@ 1,3 SAY Vmens03 GET Vbusca COLOR SCHEME 1
READ
DEACTIVATE WINDOW standby
IF EMPTY(Vbusca) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   IF !SEEK(ALLTRIM(Vbusca))
     DO standby WITH Vmens04
     GOTO Vtemp
   ELSE
     SCATTER MEMVAR
     DO Vista
   ENDIF
ENDIF
RETURN

PROCEDURE Anter
*--------------
SELECT Usua
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
SELECT Usua
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
ON KEY LABEL F3
ON KEY LABEL F9
SELECT Usua
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF

SELECT Usua
IF f_lock(1)
   ACTIVATE WINDOW Wind_F0
   @  4,22 CLEAR TO 4,79
   @  1,22 SAY m.Usuario
   @  3,22 GET m.Nombre PICTURE '@!'
   @  4,22 GET m.CodDep PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,8)
*   @  5,22 GET m.numdeP PICT '99'
   @  6,22 GET m.Observ PICTURE '@!'
   READ
   IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
      SELE ITEU
      OK=Trabaja_Hijo()
      IF OK
          SELECT Usua
          GATHER MEMVAR
      ENDIF
    ELSE
     SELECT Usua
     SCATTER MEMVAR
   ENDIF
   DO Vista                    && Muestra nuevos datos
ENDIF
UNLOCK ALL
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
ON KEY LABEL F3
ON KEY LABEL F9
ACTIVATE WINDOW Wind_F0
SELECT Usua
vLastkey= 0
SCATTER MEMVAR BLANK
@  4,22 CLEAR TO 4,79
@  1,22 GET m.Usuario     PICTURE 'NNNNNNNNNN'
READ
m.Usuario = UPPER(m.Usuario)
IF LASTKEY() = 27 .OR. EMPTY(m.Usuario)
	DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
	GO BOTTOM
 ELSE
	SELECT Usua
	IF SEEK(ALLTRIM(m.Usuario))
		DO standby WITH "El C¢digo del Usuario ya est  registrado. Proceda a corregir datos."
		DO Vista
		DO Corri
	 ELSE
		@  1,22 SAY m.Usuario
		@  3,22 GET m.Nombre PICTURE '@!'
		@  4,22 GET m.CodDep  PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,8)
*		@  5,22 GET m.numdeP  PICT '99'
		@  6,22 GET m.Observ PICTURE '@!'
	 	m.Flag = IIF(m.Usuario='ADMIN',"*","")
		
		READ
		IF LASTKEY() <> 27
			SELECT IteU
			ok=Trabaja_hijo()
			select Usua
			if ok
				if f_appd()
					GATHER MEMVAR
				endif
			endif
		 ELSE
			GO BOTTOM
		ENDIF
	ENDIF
ENDIF
UNLOCK ALL
DO Vista
RETURN


PROCEDURE Trabaja_Hijo
*---------------------
private vfun
vfun = .t.
HIDE MENU fMenu
ACTIVATE SCREEN
DO Logos WITH Rotulo1,'°°°°F5->Agregar°°°°°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°°°°°F10->Terminar°°°°'
ON KEY LABEL F10 KEYBOARD CHR(23)

SELE MaeAcc
SET ORDER TO MaeAcc2
REPLACE Acceso WITH '' ALL

SELECT IteU
vMens   = ''
vUsuCla = CHRTRAN(m.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                            'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vUsuCla)

if found()
*	SET FILTER TO User=ALLTRIM(vUsuCla)
	SCAN WHILE User=ALLTRIM(vUsuCla)
		SELE MaeAcc
		SEEK IteU.CodAcc
		IF FOUND()
			REPLACE Acceso WITH 'S'
		 ELSE
			DO StandBy WITH "Error Opci¢n no definida"
		ENDIF
		SELE IteU
	ENDSCAN
endif

*SUSP

SELE MaeAcc
SET ORDER TO MaeAcc1
BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_F2 FIELDS;
	DesSist:25 :H='Sistema':R,;
	DesMod :25 :H='M¢dulo' :R,;
	DesOpc :10 :H='Opci¢n' :R,;
	Acceso     :H='Acceso' :P="@M S, "

SELECT IteU
SET ORDER TO IteUsuOp2
DELE FOR User = ALLTRIM(vUsuCla)
SELE MaeAcc
GO TOP
SCAN
	IF UPPER(MaeAcc.Acceso)='S'
		SELE IteU
		SEEK ALLTRIM(vUsuCla)+MaeAcc.CodAcc
		IF !FOUND()
			IF f_Appd()
				REPLACE User    WITH ALLTRIM(vUsuCla),;
						Sistema WITH MaeAcc.Sistema,;
						Modulo  WITH MaeAcc.Modulo,;
						Opcion  WITH MaeAcc.Opcion,;
						CodAcc  WITH MaeAcc.CodAcc
				UNLOCK
			ENDIF
		ENDIF
	ENDIF

ENDSCAN
*SELE MaeAcc
*SET ORDER TO MaeAcc1
SELE IteU
SET ORDER TO IteUsuOp1

SEEK ALLTRIM(vUsuCla)
FLUSH
ACTIVATE SCREEN
DO Logos WITH Rotulo1, ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
SHOW MENU fMenu

ON KEY LABEL F10

RETURN vfun


PROCEDURE Elimi_item
*-------------------
IF f_lock(1)
	STORE YESNO(Vmens10) TO vElimina
	IF vElimina
		SELECT IteU
		DELETE NEXT 1
	ENDIF
ENDIF
RETURN

PROCEDURE Lista
*--------------
 ON KEY LABEL F3
 ON KEY LABEL F9
 ACTIVATE WINDOW standby
 SELECT UsuA
 vReg = RECNO()
 GO TOP
 numini = Usua.Usuario
 GO BOTTOM
 numfin = Usua.Usuario
 @ 0, 1 SAY 'Usuario Inicial:' GET numini VALID numini<=numfin;
        ERROR 'Usuario inicial mayor que el final'
 @ 1, 1 SAY '          Final:' GET numfin VALID numini<=numfin;
        ERROR 'Usuario inicial mayor que el final'
 READ
 DEACTIVATE WINDOW standby
 DO RepPrg WITH 'ImpUsu', ' Relaci¢n de Usuarios '
 SELECT Usua
 GO vReg
 DO Vista
 RETURN


PROCEDURE Termi
*--------------
vEn_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE PERCLA
*-------------------
* Personaliza claves
*-------------------
SELE Usua
cclave1 = "A"
cClave2 = SPACE(10)
cClave3 = SPACE(10)
DEFINE WINDOW Pass FROM 05,10 TO 15,50 DOUBLE ;
	TITLE "Clave de Opciones del Sistema" COLOR SCHEME 10
	
ACTIVATE WINDOWS Pass 

@ 01,03 SAY " Usuario : "
@ 01,20 SAY m.Usuario


IF LASTKEY()#27
	
	DO WHILE !(ALLTRIM(m.Clave) == ALLTRIM(cClave1))
		@ 03,03 SAY IIF(!EMPTY(m.Clave),"Ingrese su clave anterior :",SPACE(10))
		@ 03,30 SAY SPACE(10)
		cClave1 = IIF(!EMPTY(m.Clave),Aster(3,30),"")
		IF LASTKEY()=27
			DEACTIVATE WINDOWS Pass
			RELEASE    WINDOWS Pass
			RETURN
		ENDIF
		
		cClave1 = chrtran(cClave1,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
		
		IF !(ALLTRIM(m.Clave) == ALLTRIM(cClave1))
			DO StandBy WITH "Error al digitar su clave. Intente de nuevo"
			LOOP
		ENDIF
	ENDDO
	
	cClave2 = "A"
	cClave3 = "B"
	DO WHILE !(cClave2 == cClave3)
		@ 05,03 SAY "Ingrese su Nueva Clave :"
		@ 05,30 SAY SPACE(10)
		@ 07,30 SAY SPACE(10)
		cClave2 = Aster(5,30)
		@ 07,03 SAY "Repita su Nueva Clave: "
		@ 07,30 SAY SPACE(10)
		cClave3 = Aster(7,30)
		IF !(cClave2 == cClave3)
			DO StandBy WITH "Clave Incorrecta Intente de nuevo"
		ENDIF
	ENDDO
	
	cClave3 = chrtran(cClave3,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
	IF LASTKEY()#27
		REPLACE Usua.Clave WITH cClave3
	ENDIF
 ELSE
	DO StandBy WITH "Proceso Cancelado"
ENDIF

DEACTIVATE WINDOWS Pass
RELEASE    WINDOWS Pass

RETURN

PROCEDURE Aster
*--------------
PARAMETERS xFil,xCol
m.Key = 0
xPass = ""
IF LASTKEY()#27
	@ xFil,xCol SAY ''
	DO WHILE m.key # 13 .AND. m.key#27             &&  accept until car_return
		m.key = INKEY(0)
		DO CASE
			CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122) .OR. BETWEEN(m.key,48,57) .OR. BETWEEN(m.key,164,165)
				xPass = xPass + CHR(m.key)
			CASE m.key = 19 .OR. m.key = 127 .OR. m.key = 7
				@ ROW(),COL()-1 SAY ' '
				@ ROW(),COL()-1 SAY ''
				xPass = SUBSTR(xPass,1,LEN(xPass)-1)
			OTHERWISE
		ENDCASE
		
		IF LASTKEY()#27
			@ xFil,xCol SAY REPLICATE('',LEN(xPass))
		ENDIF
	ENDDO
ENDIF
RETURN xPass

