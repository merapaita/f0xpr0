*-------------------------------------------
* PagSub.Prg
* Registra Pagos de Subvenciones a entidades
* Autor : A.S. Marco Rodriguez Abad
*-------------------------------------------
*- Abriendo Archivos

*DO StandBy WITH "Revisar estas opciones, estan mejor trabajadas en Ventas"

CLOS DATA
USE PagSub		IN 1  ORDER TAG PagSub1		ALIAS PagSub
USE ItePS		IN 2  ORDER TAG ItePS1		ALIAS ItePS
USE Subvenc		IN 3  ORDER TAG Subvenc1	ALIAS Subvenc
USE Parmae		IN 4  ORDER TAG Parmae1		ALIAS Parma
USE Clientes	IN 5  ORDER TAG Clientes1	ALIAS Clien
USE Caja		IN 6  ORDER TAG Caja1		ALIAS Caja

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Pago de Subvenciones'
vmens02 = 'Pago de Subvenciones : REVISION '
vmens04 = 'Dicho Pago no fue encontrado'
vmens05 = 'No existe Pago anterior'
vmens06 = 'No existe Pago siguiente'
vmens07 = '? Desea Anular este Pago ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Pago ha sido anulado'
vmens10 = 'Edici?n'

SELECT PagSub

GO BOTT

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Inicia proceso
DO inicia                    && Define ventanas, men?s, t?tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista

*- Activa men? mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mmenu
ENDDO

DO fin_opcion

RETURN

PROCEDURE inicia             && Crea ventanas, men?s y t?tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula  Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 11,79  DOUBLE ;
	FOOTER '[F9] Detalle : Item' TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Pago Subvenciones' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens02 COLOR SCHEME 10


DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
DEFINE PAD anula   OF mmenu PROMPT ' A\<nula '    AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar'     AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
ON SELECTION PAD anula  OF mmenu DO anula
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi
RETURN

PROCEDURE pantalla           && Pinta m?scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

@  0,59 SAY "Estado :"
@  1, 2 SAY " Pago Subvencion :"
@  1,50 SAY " Fecha P/S :"
@  2, 2 SAY "  Ent. q cancela :"
@  3, 2 SAY "  Factura Girada :"
@  4, 2 SAY "   Cheque Girado :"
@  5, 2 SAY "     Monto Total :"
@  7, 2 SAY "   Observaciones :"

RETURN


PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT PagSub
ON KEY LABEL F9 DO VISTA_DET

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

@  0,68 SAY m.estado
@  1,22 SAY m.CodPS
@  1,65 SAY m.FecPS
@  2,22 SAY m.CodEnt
@  2,22 SAY VAl_para(m.CodEnt,'ENTSUB',' ',22,30)
@  3,22 SAY m.CodCP
@  4,22 SAY m.Cheque
@  5,22 SAY m.MtoTot
@  7,22 SAY LEFT(m.Observ,55)

DO vista_hijo

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT ItePS
GO TOP
SEEK m.CodPS
IF FOUND()
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.CodPS ;
		TIMEOUT 0.0001 WINDOW wind_1 FIELDS;
		CodSub	: H= 'Subvenc',;
		x = RetCli(CodCli)	: H= 'Cliente',;
		MtoSub	: H= 'Monto'

 Else
	ACTIVATE WINDOW wind_1
	CLEA
	@ 4,25 say 'No hay Detalles de este Documento'
	ACTIVATE WINDOW wind_0
endif
SELE PagSub

RETURN

PROCEDURE vista_det
*------------------
SELECT ItePS
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GO TOP
SEEK m.CodPS
IF FOUND()
	BROWSE NOAPPEND NODELETE NOMODIFY NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.CodPS ;
	WINDOW wind_1 TITLE vtempo FIELDS;
		CodSub	: H= 'Subvenc',;
		x = RetCli(CodCli)	: H= 'Cliente',;
		MtoSub	: H= 'Monto'
 Else
	ACTIVATE WINDOW wind_1
	CLEAR
	@ 4,25 say 'No hay Detalles de este Ingreso'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELE PagSub
DO VISTA
RETURN

PROCEDURE revis              && Revisi?n de BD en browse
*--------------
SELE PagSub
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()

HIDE MENU mmenu
ACTIVATE SCREEN

vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
DO logos WITH rotulo1,vtempo
SET RELATION TO CodPS INTO ItePS
SET SKIP TO ItePS
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_2 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	CodPS			:H='Pago',;
	FecPS			:H='Fecha',;
	CodEnt			:H='Entidad',;
	ItePS.CodSub	:H='Subvenc.',;
	ItePS.CodCli	:H='Cliente',;
	ItePS.MtoSub	:H='Monto'

vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE PagSub
SET RELA TO
DO vista
RETURN


PROCEDURE Busca              && Realiza b?squeda directa
*--------------
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vNum_PS = SPACE(4)

ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese Codigo Pago : ' GET vNum_PS PICTURE '!!!!' VALID vBusca()
READ
DEACTIVATE WINDOW standby

IF EMPTY(vNum_PS) .or. LASTKEY()=27
	RETURN
 ELSE
	SEEK vNum_PS
	IF !FOUND()
		DO standby WITH Vmens04
		GOTO Vtemp
	 ELSE
		DO Vista
	ENDIF
ENDIF
RETURN


PROCEDURE vBusca
*---------------
vNum_PS=Padl(alltrim(vNum_PS),4,'0')
retur .t.


PROCEDURE anter
*--------------
SELE PagSub
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
SELE PagSub
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
ON KEY LABEL F9
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SELECT PagSub
SCATTER MEMVAR
DO pantalla

@  0,68 GET m.estado	DISABLE
@  1,22 GET m.CodPS		DISABLE
@  1,65 GET m.FecPS
@  2,22 GET m.CodEnt	VALID VAl_para(m.CodEnt,'ENTSUB',' ',22,30)
@  3,22 GET m.CodCP
@  4,22 GET m.Cheque
@  5,22 GET m.MtoTot	WHEN .F.
@  7,22 GET m.Observ	FUNCTION "S50"

READ VALID val_read()

IF LASTKEY()#27
	ok = trabaja_hijo()
	IF Ok AND LASTKEY()#27
		SELECT PagSub
		IF f_Lock(1)
			m.tUser   = 'C'
			m.UserCR  = vUser_Id
			m.DUserCR = DATE()
			m.HUserCR = TIME()
			GATHER MEMVAR
		ENDIF
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT PagSub
DO vista
RETURN

PROCEDURE Ingre
*--------------
ON KEY LABEL F9
SELECT PagSub
nReg = IIF(!EOF(),RECNO(),-1)
DO pantalla

SCATTER MEMVAR BLANK

m.Estado  = '00'
m.FecPS   = DATE()

=CorPS()

SELE PagSub

@  0,68 GET m.estado	DISABLE
@  1,22 GET m.CodPS		DISABLE
@  1,65 GET m.FecPS
@  2,22 GET m.CodEnt	VALID VAl_para(m.CodEnt,'ENTSUB',' ',22,30)
@  3,22 GET m.CodCP
@  4,22 GET m.Cheque
@  5,22 GET m.MtoTot	WHEN .F.
@  7,22 GET m.Observ	FUNCTION "S50"

READ VALID val_read()

IF LASTKEY()#27
	ok = trabaja_hijo()
	IF Ok AND LASTKEY()#27
		SELECT PagSub
		IF f_Appd()
			m.tUser = 'I'
			m.User  = vUser_Id
			m.DUser = DATE()
			m.HUser = TIME()
			
			GATHER MEMVAR
		ENDIF
		
		* Parmae
		SELE Parma
		cAno = RIGHT(STR(YEAR(m.FecPS),4),2)
		
		IF !SEEK("CORPSB"+cAno,"Parma")
			IF f_Appd()
				REPLACE Tipo WITH 'CORPSB', Codigo WITH cAno, Descri WITH "CORRELATIVO DE Pago de Subvenciones A?o :" + cAno
				UNLOCK
			ENDIF
		ENDIF
		
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
		
		UNLOCK ALL
	 ELSE
		IF nReg # -1
			GO nReg
		ENDIF
		SELE ItePS
		IF SEEK(m.CodPS)
			DELE WHILE CodPS = m.CodPS
		ENDIF
		DO standby WITH 'Proceso cancelado'
	ENDIF
 ELSE
	IF nReg # -1
		GO nReg
	ENDIF
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT PagSub
DO vista
RETURN

FUNCTION CorPS
*--------------
PRIVATE cAno,cAlias,mRet,nReg
cAlias = ALIAS()
SELE &cAlias
mRet = .T.
cAno = RIGH(STR(YEAR(m.FecPS),4),2)

IF SEEK("CORPSB"+cAno,"Parma")
	m.codPS =  cAno+PADL(ALLTRIM(STR(Parma.NumEnt+1)),2,'0')
	SHOW GET m.CodPS
 ELSE
	m.codPS =  cAno+PADL(ALLTRIM(STR(Parma.NumEnt+1)),2,'0')
	SHOW GET m.CodPS
ENDIF

SELE (calias)

IF EMPTY(m.CodPS)
	DO StandBy WITH "El C?digo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodPS)
		DO standby WITH 'Ya esta Registrado este Pago'
		mRet = .F.
	ENDIF
ENDIF

RETURN mRet


PROCEDURE trabaja_hijo
*---------------------
PRIVATE mRet
vsun=.T.
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '??????????????F5->Agregar?????????F8->Eliminar????????F10->Terminar??????????' 
DO logos WITH rotulo1,vtempo
ON KEY LABEL F5  DO agrega_Sub
ON KEY LABEL F8  DO elimi_Sub
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT ItePS
SET ORDER TO itePS1
SEEK m.CodPS
IF !FOUND()
	vSun = agrega_Sub()
ENDIF

IF vSun
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOEDIT WHEN .F.;
			KEY m.CodPS WINDOW wind_1 FIELDS;
			CodSub			: H= 'Subvenc',;
			x = RetCli(CodCli)	: H= 'Cliente':30,;
			MtoSub			: H= 'Monto'
	
	IF LASTKEY()#27
		m.MtoTot = 0
		SEEK m.CodPS
		IF FOUND()
			SCAN WHILE CodPS = m.CodPS
				m.MtoTot = m.MtoTot + MtoSub
			ENDSCAN
		ENDIF
		vsun = .T.
	ELSE
		DO StandBy WITH "Proceso cancelado"
		vsun = .F.
	ENDIF
ENDIF

ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
SELECT PagSub
RETURN vsun

FUNCTION RetCli
*--------------
PARAMETER mCli
PRIVATE mRet
IF SEEK(mCli,'Clien')
	mRet = Clien.NomCli
ELSE
	mRet = "Sin Descripci?n"
ENDIF
RETURN mRet

PROCEDURE Agrega_Sub
*-------------------
PRIVATE cAlias,mRet
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F11 DO Liquida
ON KEY LABEL F12 DO QuitLiq

mRet = .F.
cAlias = ALIAS()
SELE Caja
SET FILTER TO Estado#'99'

SELE Subvenc
SET FILTER TO Estado='20'
GO TOP

DEFINE WINDOW wind_3 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE '????[F11] Liquida????????[F12] Quita Liquidacion????' ;
	COLOR SCHEME 10

BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT WHEN .F.;
		WINDOW wind_3 FIELDS;
		Pagado	:H='*':W=.F.,;
		CodSub	:H='Subvenc',;
		FecSub	:H='Fecha',;
		CodCli	:H='Cliente',;
		CodDif  :H='Difunto',;
		MtoSub	:H='Monto'

IF LASTKEY()#27
	SET FILTER TO Pagado = 'x' AND Estado='20'
	GO TOP
	IF !EOF()
		SCAN FOR Pagado = 'x' AND Estado#'10'
			xCodSub = CodSub
			xFecSub = FecSub
			xCodCli = CodCli
			xMtoSub = MtoSub
			xEstado = '00'
			
			*******
			* agregar caja
			SELE caja
			IF !SEEK("3"+xCodSub)
				m.TipCaj  = '3'
				m.FecReg  = m.FecPS
				m.CorIng  = Subvenc.CodSub
*				*m.CodCli
				m.Importe = xMtoSub
				
				IF f_Appd()
					GATHER MEMVAR
					REPLACE Estado WITH '00',;
							tUser WITH 'I',;
							User  WITH vUser_Id,;
							DUser WITH DATE(),;
							HUser WITH TIME()
					UNLOCK
				ENDIF
				mRet = .T.
			ELSE
				DO StandBy WITH "Este registro ya esta en ventas. Consulte al Area de Sistemas."
				mRet = .F.
			ENDIF
			*******
			
			IF mRet
				SELE Subvenc
				REPLACE Estado WITH '10'
				
				SELE ItePS
				
				IF f_appd()
					REPLACE CodPS  WITH m.CodPS,;
							FecPS  WITH m.FecPS,;
							CodEnt WITH m.CodEnt,;
							CodSub WITH xCodSub,;
							FecSub WITH xFecSub,;
							CodCli WITH xCodCli,;
							MtoSub WITH xMtoSub,;
							Estado WITH xEstado,;
							tUser  WITH 'I',;
							User   WITH vUser_Id,;
							dUser  WITH DATE(),;
							hUser  WITH TIME()
				ENDIF
			ENDIF
		ENDSCAN
	ELSE
		DO StandBy WITH "No se ha seleccionado Informacion para procesar"
		mRet = .F.
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
	mRet = .F.
ENDIF

DEACTIVATE WINDOW Wind_3
RELEASE    WINDOW Wind_3

SELE Caja
SET FILTER TO

SELE Subvenc
SET FILTER TO

SELE &cAlias

ON KEY LABEL F11
ON KEY LABEL F12
ON KEY LABEL F5  DO agrega_Sub
ON KEY LABEL F8  DO elimi_Sub
RETURN mRet

PROCEDURE Liquida
*----------------
REPLACE Pagado WITH 'x',CodPs WITH m.CodPS
RETURN .T.

PROCEDURE QuitLiq
*----------------
REPLACE Pagado WITH ' ',CodPs WITH ''
RETURN .T.

PROCEDURE elimi_Sub
*------------------
PRIVATE mRet
mRet = .F.
IF YesNo("Estas seguro de Eliminar este Item")
	mRet = .T.
ENDIF

IF mRet
	SELE Caja
	SET FILTER TO Estado#'99'
	IF SEEK('3'+ItePS.CodSub)
		REPLACE estado  WITH 'El',;
				TUser   WITH 'C',;
				UserCr  WITH vUser_Id,;
				HUserCr WITH TIME(),;
				DUserCr WITH DATE()
		mRet = .T.
	ELSE
		IF YesNo('Subvencion no Registrada en caja. Desea Continuar?')
			mRet = .T.
		ELSE
			mRet = .F.
		ENDIF
	ENDIF
	SET FILTER TO
ENDIF
	
IF mRet
	SELE Subvenc
	IF SEEK(ItePS.CodSub)
		REPLACE estado  WITH '20',;
				Pagado	WITH ' ',;
				CodPs WITH '',;
				TUser   WITH 'C',;
				UserCr  WITH vUser_Id,;
				HUserCr WITH TIME(),;
				DUserCr WITH DATE()
		mRet = .T.
	ELSE
		IF YesNo('Subvencion no Registrada. Desea Continuar?')
			mRet = .T.
		ELSE
			mRet = .F.
		ENDIF
	ENDIF
ENDIF

IF mRet
	SELE ItePS
	DELE NEXT 1
	DO StandBy WITH 'Subvencion Eliminada del Pago'
ENDIF
RETURN

PROCEDURE Lista
*--------------
PRIVATE nReg
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

nReg = RECNO()
DEFINE WINDOW wLista FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Pedidos' COLOR SCHEME 5
ACTIVATE WINDOW wLista

vToPa = 1
mFecha1 = CTOD("01/01/"+STR(YEAR(m.FecSis),4))
mFecha2 = DATE()

@ 01,01 SAY "    Todos los Pagos : " GET vToPa	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2
@ 03,01 SAY "         Subvencion : " GET m.CodPS	WHEN vToPa = 2
@ 12,01 SAY "     Rango de Fechas: " GET mFecha1	WHEN vToPa = 1
@ 12,36 GET mFecha2		WHEN vToPa = 1
@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

lFil = .T.
IF LASTKEY()#27 AND OkCancel#2
	IF vToPa = 2
		lFil = "CodPS = m.CodPS"
	ENDIF
	
	SET FILTER TO &lFil AND BETW(FecPS,mFecha1,mFecha2)
	SET RELATION TO CodPS INTO ItePS ADDITIVE
	SET SKIP TO ItePS
	DO REPORTE WITH 2,"PagSub","Pago de Subvenciones" ,2,.F.,.T.
	SET RELATION TO
	SET FILTER TO
ENDIF

SELECT PagSub
GO nReg
DO vista
RETURN

PROCEDURE Anula
*--------------
DO STANDBY WITH "Opci?n todavia en proceso. Crear reporte si es necesario"
return


PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE MENU   mmenu
RESTORE SCREEN FROM principal
RETURN

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0020","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0021","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0022","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0023","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0024","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0025","IteUsuOp")
set skip of PAD Anula of mMenu !SEEK(vUsucla+"0026","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0027","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
