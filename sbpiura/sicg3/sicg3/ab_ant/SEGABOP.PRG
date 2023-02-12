**------------------------------------------------------------------------
** SEGABop.PRG - 2002
** Maestro de Conceptos
** AUTOR: MERA - 2002. 
**------------------------------------------------------------------------
PARAMETERS v_Us
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
Vmens11 = 'EDICION'

*- Abre archivos y se posiciona en £ltimo registro
USE Usuario  IN 1  ORDER TAG Usuario1   ALIAS Usua
USE IteUsuOp IN 2  ORDER TAG IteUsuoP1  ALIAS IteU
USE Parmae   IN 3  order tag Parmae1    ALIAS Parma
SAVE SCREEN TO Pantalla
HIDE POPUP ALL
SELE iteu
SELECT Usua
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


PROCEDURE Termi
*--------------
vEn_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW Wind_F0
RELEASE WINDOW Wind_F1
RELEASE WINDOW Wind_F2
RELEASE WINDOW Wind_F3
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

 DEFINE WINDOW Wind_F3 FROM 03,00 TO 15,50 DOUBLE ;
 TITLE Vmens11 COLOR SCHEME 10

 DEFINE MENU fMenu COLOR SCHEME 3
 DEFINE PAD revis   OF fMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF fMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF fMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF fMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF fMenu PROMPT '\<Corrige'    AT 24,36		SKIP FOR v_Us#'ADMIN'
 DEFINE PAD ingre   OF fMenu PROMPT '\<Ingresa'    AT 24,45		SKIP FOR v_Us#'ADMIN'
 DEFINE PAD elimi   OF fMenu PROMPT '\<Elimina'    AT 24,54
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

ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_F0
 CLEAR
 @  1, 2 SAY "          Usuario :"
 @  3, 2 SAY "           Nombre :"
 @  4, 2 SAY "      Dependencia :"
 @  5, 2 SAY "            Nivel :" 
 @  6, 2 SAY "    Observaciones :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta dato
*--------------
 ACTIVATE WINDOW Wind_F0
 ON KEY LABEL F3 DO PerCla
 ON KEY LABEL F9 DO Vista_Det
 SELECT Usua
 SCATTER MEMVAR
 @  1,22 SAY m.Usuario
 @  3,22 SAY m.Nombre
 @  4,22 SAY val_para(m.CodDep,'CODDEP','A',22,50,7)
 @  5,22 SAY m.Numdep PICT '99'  
 @  6,22 SAY m.Observ

 DO Vista_Hijo
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


PROCEDURE Vista_Hijo
*-------------------
SELECT IteU
vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                               'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vUsuCla)
IF FOUND()
	BROWSE;
		NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT TIMEOUT .001;
		WINDOW Wind_F2 KEY ALLTRIM(vUsuCla)    FIELDS;
			xSis = IIF(!EMPTY(Sistema),val_para(Sistema,'SISTEM','D'),Sistema):30 :H='Sistema',;
			xMod = IIF(!EMPTY(Modulo),val_para1(modulo,'MODULO'+Sistema,'D'),Modulo):30 :H='M¢dulo',;
			xOpc = IIF(!EMPTY(Opcion),val_para(opcion,'OPCION','D'),Opcion):10 :H='Opci¢n'
 ELSE
	ACTIVATE WINDOW Wind_F2
	CLEAR
	@ 5,10 SAY "USUARIO NO TIENE DETALLE..."
ENDIF

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


PROCEDURE ModCla
*---------------
SELE Usua
cCla1 = SPACE(10)
cCla2 = SPACE(10)
DEFINE WINDOW Pass FROM 05,10 TO 15,50 DOUBLE ;
	TITLE "Clave de Usuarios del Sistema" COLOR SCHEME 10
	
ACTIVATE WINDOWS Pass 

@ 01,03 SAY "         Clave : "
@ 02,20 GET cCla1	COLOR ,X/W
@ 03,03 SAY " Repetir Clave : "
@ 04,20 GET cCla2	COLOR ,X/W
READ VALID Val_Cla()

IF LASTKEY()#27
	cCla2 = chrtran(cCla2,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
		                  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
							  
	REPLACE Usua.Clave WITH cCla2
 ELSE
	DO StandBy WITH "Proceso Cancelado"
ENDIF

DEACTIVATE WINDOWS Pass
RELEASE    WINDOWS Pass


FUNCTION Val_Cla
*---------------
IF cCla1 # cCla2
	DO StandBy WITH "Clave Ingresada es Incorrecta. Intente de nuevo."
	cCla1 = SPACE(10)
	cCla2 = SPACE(10)
	_CUROBJ  = OBJNUM(cCla1)
	SHOW GETS
	RETURN .F.
 ELSE
	RETURN Val_Read()
ENDIF
RETURN .T.

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 ON KEY LABEL F3
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
   @  4,22 GET m.CodDep  PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,8)
   @  5,22 GET m.numdeP  PICT '99'
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
		@  5,22 GET m.numdeP  PICT '99'
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
 ON KEY LABEL F2  DO Edita_item
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteU
vMens   = ''
vUsuCla = CHRTRAN(m.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                            'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
SEEK ALLTRIM(vUsuCla)

if !found()
	do agreg_item
endif

BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_F2 KEY ALLTRIM(vUsuCla) FIELDS;
	xSis = IIF(!EMPTY(Sistema),val_para(Sistema,'SISTEM','D'),Sistema):30 :H='Sistema',;
	xMod = IIF(!EMPTY(Modulo),val_para1(modulo,'MODULO'+Sistema,'D'),Modulo):30 :H='M¢dulo',;
	xOpc = IIF(!EMPTY(Opcion),val_para(opcion,'OPCION','D'),Opcion):10 :H='Opci¢n'

SELECT IteU
SEEK ALLTRIM(vUsuCla)
SCAN WHILE  vUsuCla = Iteu.User
	IF EMPTY(IteU.User) OR EMPTY(IteU.Sistema) OR EMPTY(IteU.Modulo) OR EMPTY(IteU.Opcion)
		DELETE NEXT 1
	ENDIF
ENDSCAN
SEEK ALLTRIM(vUsuCla)
FLUSH
ACTIVATE SCREEN
DO Logos WITH Rotulo1, ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
SHOW MENU fMenu

 ON KEY LABEL F2
 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 RETURN vfun


PROCEDURE Agreg_item
*-------------------
SELECT IteU
IF f_appd()
   REPLACE User   WITH vUsuCla
ENDIF
DO Edita_Item
RETURN

PROCEDURE Edita_item
*-------------------
ACTIVATE WINDOW Wind_F3
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

*SELE IteU

SCATTER MEMVAR

@  1,1 SAY '  Usuario: ' + Usua.Nombre
@  3,1 SAY '  Sistema:' GET m.Sistema	PICTURE "!!" VALID val_para(m.Sistema,'SISTEM',' ',12)
@  5,1 SAY '   Modulo:' GET m.Modulo	PICTURE "!!" VALID val_para1(m.modulo,'MODULO'+m.Sistema,' ',12)
@  7,1 SAY '   Opci¢n:' GET m.Opcion	PICTURE "!"  VALID val_para(m.opcion,'OPCION',' ',12) AND vOpcion()

READ VALID val_read()

IF LASTKEY()#27
	IF f_Lock(1)
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

DEACTIVATE WINDOW Wind_F3

ON KEY LABEL F2  DO Edita_item
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)

RETURN

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


FUNCTION vOpcion
*---------------
PRIVATE vSistema
vMens    = ''
vRetorno = .F.
vSistema = ALLTRIM(m.Sistema)
vModulo  = ALLTRIM(m.Modulo)
vOpcion  = ALLTRIM(m.Opcion)
vLastkey  = LASTKEY()
IF vLastkey = -7  && F8
	vRetorno  = .T.
 ELSE
	vReg    = RECNO()
	IF SEEK(ALLTRIM(vUsuCla) + vSistema + vModulo + vOpcion )
		IF vReg # RECNO()
			vRetorno = .F.
			vMens    = 'M¢dulo ya seleccionado'
			DO standby WITH vMens
			GO vReg
		 ELSE
			vRetorno  = .T.
		ENDIF
	 ELSE
		GO vReg
		vRetorno  = .T.
	ENDIF
ENDIF
RETURN vRetorno

FUNCTION OpcDescri
*-----------------
 DO CASE
   CASE IteU.Modulo == 'A'
        RETURN  ' Revisa '
   CASE IteU.Modulo == 'B'
		RETURN  ' Busca'
   CASE IteU.Modulo == 'C'
		RETURN  ' Anterior'
   CASE IteU.Modulo == 'D'
		RETURN  ' Siguiente'
   CASE IteU.Modulo == 'E'
		RETURN  ' Corrige'
   CASE IteU.Modulo == 'F'
		RETURN  ' Ingresa'
   CASE IteU.Modulo == 'G'
        RETURN  ' Anula'
   CASE IteU.Modulo == 'H'
		RETURN  ' Lista'
ENDCASE
RETURN ''

PROCEDURE Elimi
*--------------
ON KEY LABEL F3
STORE YESNO(Vmens07) TO vElimina
IF !vElimina
	RETURN
ENDIF
IF f_lock(1)
	vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
	                               'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
	SELECT IteU
	SEEK ALLTRIM(vUsuCla)
	SCAN WHILE vUsuCla = IteU.UsuCla
		IF f_lock(1)
			DELETE
		ENDIF
	ENDSCAN
	SELECT Usua
	DELETE
	UNLOCK ALL
ENDIF
SELECT Usua
IF !BOF()
	SKIP -1
 ELSE
	GO BOTTOM
ENDIF
DO Vista
RETURN


PROCEDURE Lista
*--------------
 ON KEY LABEL F3
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

PROCEDURE ImpUsu
*---------------
PARAMETER _desti
IF _desti=2
  SET PRINTER TO (p_fil)
ENDIF

SET DEVICE TO PRINT
STORE 0 TO lineas

 SELECT Usua
 SEEK numini
 SCAN WHILE numini <= Usua.Usuario .AND. numfin >= Usua.Usuario
    SCATTER MEMVAR
    IF lineas > 50 .OR. lineas = 0
      lineas = 0
      DO ImpTit WITH lineas, .T.
    ELSE
      DO ImpTit WITH lineas, .F.
    ENDIF
    vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                                   'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
    SELECT IteU
    SEEK ALLTRIM(vUsuCla)
    SCAN WHILE vUsuCla = IteU.UsuCla
       IF lineas > 50 .OR. lineas = 0
         lineas = 0
         DO ImpTit WITH lineas, .T.
       ENDIF
       SELECT IteU
       @ lineas, 0 SAY IteU.Modulo
       @ lineas,10 SAY xDescri()
       lineas = lineas + 1
    ENDSCAN

    SELECT Usua
 ENDSCAN
 SET DEVICE TO SCREEN
 SET PRINTER TO
 RETURN

PROCEDURE ImpTit
*---------------
PARAMETERS vlineas, Cabecera
IF vlineas = 0
   vtitulo = 'Relaci¢n de Usuarios'
   @  1,0  SAY SUBSTR(Cia,1,20)
   @  1,INT((80 - LEN(vtitulo))/2) SAY vTitulo
   @  1,73 SAY DATE()
   vlineas = 2
ENDIF
vlineas = vlineas + 1
@  vlineas,0  SAY "   Usuario: " + m.Usuario
vlineas = vlineas + 1
@  vlineas,0  SAY "    Nombre: " + m.Nombre
vlineas = vlineas + 1
xClave  = CHRTRAN(m.Clave,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#',;
                          'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789')
@  vlineas,0  SAY "     Clave: " + xClave
vlineas = vlineas + 1
IF Cabecera
   @ vlineas,00 SAY REPLICATE('-',80)
   vlineas = vlineas + 1
   @ vlineas,00 SAY 'M¢dulo'
   @ vlineas,10 SAY '    Descripci¢n     '
   vlineas = vlineas + 1
   @ vlineas,00 SAY REPLICATE('-',80)
   vlineas = vlineas + 1
   SELECT IteU
ELSE
  vlineas = vlineas + 1
ENDIF
RETURN

FUNCTION Valida
*--------------
PARAMETERS Valor, Compara
Valor   = UPPER(Valor)
Compara = UPPER(Compara)
IF Valor # Compara
  Valor = SPACE(LEN(Valor))
  @  4,22 SAY Valor  PICTURE '!!!!!!!!!!'
  RETURN -1
ENDIF
RETURN 1
