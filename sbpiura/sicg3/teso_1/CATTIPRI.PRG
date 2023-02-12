* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CatTipRi.PRG  06/02/2003                            L: 0408  ³	
* ³ Cat logo de Conceptos de Recibos de Ingreso                  ³
* ³ AUTOR   : A.S. Marco Rodriguez Abad S.B.P.                   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE AsiAutRI IN 1   ORDER TAG AsiAutRI1 	ALIAS AsiAutRI
USE Cuentas  IN 7   ORDER TAG Cuentas1	 	ALIAS cuen
USE Ingreso  IN 10  ORDER TAG Ingreso1	 	ALIAS IngR
USE CatAsi   IN 17  ORDER TAG CatAsi3       ALIAS CatAsi


*- Variables de trabajo (registro a trabajar)
SELECT AsiAutRI
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Mensajes de aviso al usuario
Vmens01 = ' ± Registro de Conceptos de Recibo de Ingresos y/o Facturas± '
Vmens02 = 'Revisi¢n de Rubros'
Vmens05 = 'No existe Rubro anterior'
Vmens06 = 'No existe Rubro siguiente'
Vmens08 = 'No hay registros para procesar'

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*PUBLIC vtipo, vclas
*STORE SPACE(1) TO vtipo
*STORE SPACE(9) TO vclas

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

DEFINE WINDOW Wind_c1 FROM 00,00 TO 23,79 ;
COLOR SCHEME 10

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

@ 01,02 SAY "       Tipo de Documento :"
@ 03,02 SAY "                  Codigo :"
@ 05,02 SAY "             Descripci¢n :"
@ 07,02 SAY "      Clasif. de Ingreso :"
@ 09,02 SAY "  Asientos Patrimoniales :"
@ 11,02 SAY " Asientos Presupuestales :"
@ 13,02 SAY "       Asientos de Orden :"
@ 15,02 SAY "  Enl. Pat. con IGV(S/N) :"

RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT AsiAutRI
IF EOF()
	DO Pantalla
	RETURN
ENDIF
ACTIVATE WINDOW Wind_0
SCATTER MEMVAR

@ 01,30 SAY m.Tipo
@ 03,30 SAY m.Codigo
@ 05,30 SAY m.Descri
@ 07,30 SAY IIF(!EMPTY(m.Partida),VAL_INGE(SUBSTR(m.Partida,10,3),LEFT(m.Partida,9),' ',30),SPACE(40)) 
@ 09,30 SAY m.dAstPat
@ 09,42 SAY m.hAstPat
@ 11,30 SAY m.dAstPre
@ 11,42 SAY m.hAstPre
@ 13,30 SAY m.dAstOrd
@ 13,42 SAY m.hAstOrd
@ 15,30 SAY m.igv
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE AsiAutRI
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
	Tipo ,;
	Codigo,;
	descri :50 :H='Detalle'

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
SELECT AsiAutRI
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

Vtemp = RECNO()
STORE SPACE(03) TO vTip,vCod
ACTIVATE WINDOW standby

@ 0,01 SAY "Tipo   :" GET vTip
@ 2,01 SAY "C¢digo :" GET vCod
READ

DEACTIVATE WINDOW standby
IF (EMPTY(vTip) AND EMPTY(vCod)) .OR. LASTKEY()=27
	GOTO Vtemp
 ELSE
	SEEK vTip+vCod
	IF !FOUND()
		DO standby WITH "Dicho Tipo no est  registrado"
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
SELECT AsiAutRI
DO pantalla
SCATTER MEMVAR
IF RLOCK() OR F_LOCK(1)
	@ 01,30 GET m.Tipo		DISABLE
	@ 03,30 GET m.Codigo	DISABLE
	@ 05,30 GET m.descri	PICTURE '@S36'
	@ 07,30 GET m.Partida	PICTURE "!!!!!!!!!!" VALID IIF(!EMPTY(m.Partida),v_part(m.partida),.T.)
*	@ 07,30 GET m.Partida	PICTURE "!!!!!!!!!!!!" VALID IIF(!EMPTY(m.Partida),VAL_INGE(SUBSTR(m.Partida,10,3),LEFT(m.Partida,9),' ',30),.T.) 
	@ 09,30 GET m.DAstPat	VALID IIF(!EMPTY(m.DAstPat),v_Cta(m.DAstPat),.T.) 
	@ 09,42 GET m.HAstPat	VALID IIF(!EMPTY(m.HAstPat),v_Cta(m.HAstPat),.T.)
	@ 11,30 GET m.DAstPre	PICTURE '!!!!' 
	@ 11,42 GET m.HAstPre	PICTURE '!!!!'
	@ 13,30 GET m.DAstOrd	VALID IIF(!EMPTY(m.DAstOrd),v_Cta(m.DAstOrd),.T.) 
	@ 13,42 GET m.HAstOrd	VALID IIF(!EMPTY(m.HAstOrd),v_Cta(m.HAstOrd),.T.) 
	@ 15,30 GET m.IGV		FUNCTION "M S,N"  WHEN m.Tipo='FAC'
	
	READ VALID Val_Read()
	
	IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
		GATHER MEMVAR
	ENDIF
	DO Vista                    && Muestra nuevos datos
ENDIF
UNLOCK
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT AsiAutRI
DO Pantalla
SCATTER MEMVAR BLANK
@ 01,30 GET m.Tipo    FUNCTION "M R/I,FAC"
@ 03,30 GET m.Codigo  PICTURE '999' VALID vret() 
@ 05,30 GET m.Descri  PICTURE '@S36'
@ 07,30 GET m.Partida	PICTURE "!!!!!!!!!!!!!!" VALID IIF(!EMPTY(m.Partida),v_part(m.partida),.T.)
*@ 07,30 GET m.Partida PICTURE "!!!!!!!!!!!!" VALID IIF(!EMPTY(m.Partida),VAL_INGE(SUBSTR(m.Partida,10,3),LEFT(m.Partida,9),' ',30),.T.) 
@ 09,30 GET m.DAstPat	VALID IIF(!EMPTY(m.DAstPat),v_Cta(m.DAstPat),.T.) 
@ 09,42 GET m.HAstPat	VALID IIF(!EMPTY(m.HAstPat),v_Cta(m.HAstPat),.T.)
@ 11,30 GET m.DAstPre	PICTURE '!!!!' 
@ 11,42 GET m.HAstPre	PICTURE '!!!!'
@ 13,30 GET m.DAstOrd	VALID IIF(!EMPTY(m.DAstOrd),v_Cta(m.DAstOrd),.T.) 
@ 13,42 GET m.HAstOrd	VALID IIF(!EMPTY(m.HAstOrd),v_Cta(m.HAstOrd),.T.) 
@ 15,30 GET m.IGV		FUNCTION "M S,N"  WHEN m.Tipo='FAC'

READ VALID Val_Read()
IF LASTKEY() # 27
	IF F_Appd()
		GATHER MEMVAR
	 ELSE
		GO BOTTOM
	ENDIF
 ELSE
	DO STANDBY WITH 'Proceso cancelado, no se graba nada'
	GO BOTTOM
ENDIF
UNLOCK ALL
SELECT AsiAutRI
DO Vista
RETURN

FUNCTION v_part
*--------------
PARAMETERS vPar
IF !EMPTY(vPar)
	IF SEEK(vPar,"CatAsi")
		*
	 ELSE
		vPar = SPACE(14)
		= ValAsi1(ALIAS(),"m.partida","8","CodPart",'R')
		vPar = CatAsi.CodPart
	ENDIF
 ELSE
	vPar = SPACE(10)
ENDIF

*IF Cuen.Detalle#'S'
*	=STANDBY('La Cuenta No es de Detalle. Por Favor Intente de Nuevo')
*	RETURN .F.
*ENDIF

RETURN .T.

FUNCTION v_Cta
*------------
PARAMETERS vCta
IF !EMPTY(vCta)
	IF SEEK(vCta,"Cuen")
		*
	 ELSE
		vCta = SPACE(10)
		= val_fun('Cuen','Cuenta',"Cuenta+' '+Descri",vCta,1)
	ENDIF
 ELSE
	vCta = SPACE(10)
ENDIF

IF Cuen.Detalle#'S'
	=STANDBY('La Cuenta No es de Detalle. Por Favor Intente de Nuevo')
	RETURN .F.
ENDIF

RETURN .T.


PROCEDURE Elimi
*--------------
SELECT AsiAutRI
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF

velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚ste Rubro ?')
IF AsiAutRI.Tipo = "R/I"
	SELE 0
	USE RecIng ORDER TAG RECING1
	GO TOP
	LOCATE FOR AsiAutri.Codigo = RecIng.TipRi
	IF !FOUND()
		SELE AsiAutRI
		IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
			DELETE NEXT 1
			SKIP 1
			IF EOF()
				GO BOTTOM
			ENDIF
			DO Vista
		ENDIF
	 ELSE
		DO StandBy WITH "Dicho rubro no se puede eliminar por que ya fue usado"
	ENDIF
	USE IN RecIng
	SELE AsiAutRI
 ELSE
	SELE AsiAutRI
	IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
		DELETE NEXT 1
		SKIP 1
		IF EOF()
			GO BOTTOM
		ENDIF
		DO Vista
	ENDIF
ENDIF
UNLOCK
RETURN

PROCEDURE Lista
*--------------
SELECT AsiAutRI
Vind = SYS(3)+'.idx'
vReg = RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
 ELSE
	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
			TITLE ' °°  Listado de Tipos de Ingreso  °° ' FLOAT COLOR SCHEME 5
	
	ACTIVATE WINDOW lis_1
	
	vTipo = 1
	vOrd  = 2
	@  2,2 SAY '        Tipo : ' GET vTipo		FUNCTION "^ R/I;FAC"
	@  6,2 SAY 'Ordenado por : ' GET vOrd	 	FUNCTION "^ Correlativo;Tipo de Documento" 
	@ 10,10 GET OkCancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
	READ CYCLE
	
	DEACTIVATE WINDOW lis_1
	
	IF lastkey()#27 AND OkCancel#2
		SELE AsiAutRI
		SET FILTER TO Tipo = IIF(vTipo = 1,'R/I','FAC')

		IF vOrd = 2
			INDEX ON descri to (vInd)
		ENDIF
		DO Reporte WITH 2,"LisTiprC",' Listado de Tipos de Recibos de Ingreso '
		SET INDEX TO
		SET ORDER TO AsiAutRI1
	 ELSE
		DO vista
		RETURN
	ENDIF
ENDIF
***
GO vReg
DO Vista
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

FUNCTION vret
*------------
SELE AsiAutRI
SEEK m.tipo+m.codigo
IF FOUND()
	DO STANDBY WITH 'Concepto de R/I ya existe' 
	RETURN  .F.
ENDIF
RETURN .T.

FUNCTION correl
*--------------
SELE Parma
SEEK 'CORREL'+'TIPORI'
m.codigo = PADL(NumEnt+1,4,' ')

RETURN 



FUNCTION Val_IngE
*---------------
PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
PRiVATE mAlias
DO CASE
	CASE PARAMETERS() = 2
		mCol = 0
		mVaRiable = ' '
		mLong = 40
	CASE PARAMETERS() = 3
		mCol = 0
		mLong = 40
	CASE PARAMETERS() = 4
		mLong = 40               && Longitud campo DESCRi
ENDCASE
mAlias  = ALIAS()
SELECT IngR
SEEK Filtro+mValor

IF !FOUND() AND !mVaRiable $'VZ' AND SubIng#'S'
	_OldWnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(FILTRO)
		SET FILTER TO CodIng >= Filtro
	ENDIF
	GO TOP
	IF EOF()
		DO STANDBY WITH 'No existen partidas definidas'
		SET FILTER TO
		sele (malias)
		return &&.f.
	endif
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD CODING+' '+LEFT(SUBING,1)+' '+SUBSTR(DESING,1,40)
			ON SELECTION POPUP parametro DEACTIVATE POPUP
			ACTIVATE POPUP parametro
	
	IF !EMPTY( _OldWnd)
		ACTIVATE WINDOW &_OldWnd
	ENDIF
	
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF

IF SubIng#'S'
	DO STANDBY WITH 'La partida no es de Detalle'
	SELE (malias)
	RETURN .F.
ENDIF

mValor = IngR.CodIng 
mDescr = SUBSTR( IngR.DesIng, 1, mLong )
IF !EMPTY( mAlias )
	SELECT (mAlias)
ENDIF
DO CASE
	CASE mVaRiable==' '   && En edici¢n
		@ ROW(),mCol    SAY mValor
		@ ROW(),mCol+14 SAY mDescr
		m.Partida = mvalor
		RETURN .T.
	CASE mVaRiable=='A'   && En edici¢n SOLO DESCRiPCION
		@ ROW(),mCol SAY mDescr
		RETURN
	CASE mVaRiable=='V'   && En vista
		@ ROW(),COL()  SAY mValor
		RETURN mDescr
	CASE mVaRiable=='D'   && En vista
		RETURN mDescr
	CASE mVaRiable=='Z'   && En vista SIN PINTAR
		RETURN mDescr
	CASE mVaRiable=='C'   && Solo codigo
		RETURN .T.
	CASE mVaRiable=='T'
		&mVaRiable = mValor
		@ ROW(),mCol+7 SAY mDescr
		RETURN  mValor
	OTHERWISE   
		vclas= mvalor
		RETURN .T.
ENDCASE