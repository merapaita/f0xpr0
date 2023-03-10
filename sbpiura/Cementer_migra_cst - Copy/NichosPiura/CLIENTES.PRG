*------------------------------------
* Cliente.Prg
* Registra los Clientes
* 
* AUTOR : A. S. Marco Rodriguez Abad
*------------------------------------
*- Abriendo Archivos
CLOS DATA
USE Clientes IN  1 ORDER TAG Clientes1 ALIAS Clien
USE Parmae   IN  3 ORDER TAG Parmae1   ALIAS Parma

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Clientes'
vmens02 = ' Clientes : REVISION '
vmens04 = 'Dicho Cliente no fue encontrado'
vmens05 = 'No existe Cliente anterior'
vmens06 = 'No existe Cliente siguiente'
vmens07 = '? Desea Anular ?ste Cliente ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Cliente ha sido anulado'

SELECT Clien
GO TOP

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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 FOOTER "F3 -> Act. Estado Cliente" COLOR SCHEME 10

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

PROCEDURE pantalla           && Pinta m?scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

@  2, 10 SAY "       Codigo :"
@  4, 10 SAY "      Cliente :"
@  6, 10 SAY "       D.N.I. :"
@  8, 10 SAY "    Direcci?n :"
@ 10, 10 SAY " Razon Social :"
@ 12, 10 SAY "       R.U.C. :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Clien

ON KEY LABEL F3 DO ActEstCl

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

@  1,50 SAY EstCli()
@  2,30 SAY m.CodCli
@  4,30 SAY m.NomCli
@  6,30 SAY m.DNICli
@  8,30 SAY m.Direccion
@ 10,30 SAY m.RazSoc
@ 12,30 SAY m.RUCCli

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi?n de BD en browse
*--------------
ON KEY LABEL F3
SELE Clien
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
DO logos WITH rotulo1,vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	NomCli :H='Cliente',;
	RazSoc :H='Raz. Social'

vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE Clien
DO vista
RETURN

PROCEDURE busca              && Realiza b?squeda directa
*--------------
ON KEY LABEL F3
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp    = RECNO()
cOrd1	 = ORDER()
vNomCli  = SPACE(50)

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo      '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre      '

ON SELECTION POPUP pLista  DEACTIVATE POPUP

ACTIVATE POPUP pLista

DO CASE
	CASE BAR() =  1
		vBusca = SPACE(6)
		vNombre = "Codigo :"
		SET ORDER TO TAG Clientes1
	CASE BAR() =  2
		vBusca = SPACE(30)
		vNombre = "Nombre : "
		SET ORDER TO TAG Clientes2
	OTHERWISE
		vBusca = ''
		vNombre =''
		SET ORDER TO
ENDCASE

IF LASTKEY()#27
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5
	
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

PROCEDURE anter
*--------------
SELE Clien
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
SELE Clien
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
ON KEY LABEL F3
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SELECT Clien
SCATTER MEMVAR
DO pantalla

@  2,30 SAY m.CodCli
@  4,30 GET m.NomCli	FUNCTION '!S30'
@  6,30 GET m.DNICli	PICTURE '99999999'
@  8,30 GET m.Direccion	FUNCTION '!S30'
@ 10,30 GET m.RazSoc	FUNCTION '!S30'
@ 12,30 GET m.RUCCli	PICTURE '99999999999'

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Clien
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT Clien
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
ON KEY LABEL F3
SELECT Clien
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

DO pantalla

=Val_Clien()

@  2,30 SAY m.CodCli
@  4,30 GET m.NomCli	FUNCTION '!S30'
@  6,30 GET m.DNICli	PICTURE '99999999'
@  8,30 GET m.Direccion	FUNCTION '!S30'
@ 10,30 GET m.RazSoc	FUNCTION '!S30'
@ 12,30 GET m.RUCCli	PICTURE '99999999999'

READ VALID val_read()

IF LASTKEY()=27
	IF vTemp = -1
		GO TOP
	 ELSE
		GO vtemp
	ENDIF
	
	DO vista
	RETURN
ENDIF

IF F_Appd()
	GATHER MEMVAR
	SELE Parma
	SEEK "CORRELCLIENT"
	REPLACE NumEnt WITH Parma.NumEnt+1
ENDIF

UNLOCK ALL
SELECT Clien

DO vista
RETURN

FUNCTION EstCli
*--------------
DO CASE
	CASE m.Estado ='00'
		@ ROW(),COL() SAY SPACE(9)
	CASE m.Estado ='10'
		@ ROW(),COL() SAY "CON SALDO" COLOR SCHEME 5
ENDCASE
RETURN

*FUNCTION Val_Clien
*-----------------
IF SEEK("CORRELCLIENT"+m.codcli,"Parma")
	m.codcli = PADL(ALLTRIM(STR(Parma.NumEnt+1)),6,'0')
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodCli)
	DO StandBy WITH "El Codigo esta vacio"
	RETURN .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodCli)
		DO standby WITH 'Ya esta Registrado este Cliente'
		RETURN .F.
	ENDIF
ENDIF
RETURN .T.

PROCEDURE elimi
*--------------
ON KEY LABEL F3

DO StandBy WITH "revisar antes opciones de difuntos y ocup. fut. "

SELECT Clien
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
velimina = yesno('? Desea ELIMINAR ?ste Cliente ?')

velimina = .T.

IF vElimina
	USE Mausoleo IN 0 ORDER TAG Mausoleo3 ALIAS Mausol
	SELE Mausol
	IF SEEK(m.CodCli)
		DO StandBy WITH 'Existe Cliente en Mausoleos. No se puede Eliminar cliente'
		velimina = .F.
	 ELSE
		velimina = .T.
	ENDIF
	USE IN Mausol
	
	IF vElimina
		USE Difunto IN 0 ORDER TAG Difunto4 ALIAS Difunto
		SELE Difunto
		IF SEEK(m.CodCli)
			DO StandBy WITH 'Existe Cliente en Difuntos. No se Puede Eliminar Cliente'
			velimina = .F.
		 ELSE
			velimina = .T.
		ENDIF
		USE IN Difunto
	ENDIF
	
	IF vElimina
		USE OcupFut IN 0 ORDER TAG OcupFut3 ALIAS OcuFut
		SELE OcuFut
		IF SEEK(m.CodCli)
			DO StandBy WITH 'Existe Cliente en Ocupaciones Futuras. No se Puede Eliminar Cliente'
			velimina = .F.
		 ELSE
			velimina = .T.
		ENDIF
		USE IN OcuFut
	ENDIF
	
	IF velimina
		SELE Clien
		IF SEEK(m.CodCli)
			DELE NEXT 1
			SKIP -1
			IF BOF()
				GO TOP
			ENDIF
		ENDIF
	ELSE
		DO STANDBY WITH "No Puede Borrar este Cliente por encontrarse registrado con una venta"
	ENDIF
	
ENDIF

SELECT Clien
UNLOCK ALL

DO vista
RETURN


PROCEDURE lista
*--------------
PRIVATE vOrd,nReg,cOrd

SELECT Clien
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE 'Listado de Clientes' COLOR SCHEME 5
ACTIVATE WINDOW LIS

vOrd = 1
cOrd = ORDER()
nReg = RECNO()
@ 02,10 SAY "Ordenado por : " GET vOrd  FUNCTION '^ Codigo;Nombre' SIZE 1,10,6

@ 10,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

IF LASTKEY()#27
	DO CASE
		CASE vOrd = 1
			SET ORDER TO Clientes1
		CASE vOrd = 2
			SET ORDER TO Clientes2
	ENDCASE
	DO REPORTE WITH 2,"Clientes","Listado de Clientes"
ENDIF

SELE Clien
SET ORDER TO &cOrd
GO nReg
DO vista

RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE ActEstCl
*-----------------
PRIVATE cAlias
cAlias = ALIAS()
set step on
USE Ventas IN 0 ORDER TAG Ventas2
cEst = m.Estado
SELE Ventas

IF cEst = '00'
	IF SEEK(m.CodCli)
		SCAN WHILE CodCli=m.CodCli
			IF Estado = '20'
				cEst = '10'
			ENDIF
		ENDSCAN
	ENDIF
ELSE
	IF SEEK(m.CodCli)
		SCAN WHILE CodCli=m.CodCli
			IF Estado # '20'
				cEst = '00'
			ENDIF
		ENDSCAN
	ENDIF
ENDIF

USE IN Ventas
SELE (calias)
REPLACE Estado WITH cEst

DO VISTA

RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE WINDOW wind_0
RELEASE MENU   mmenu
*RESTORE SCREEN FROM principal
RETURN

**************************************
**************************************


*FUNCTION VV_Cem
*---------------
PARAMETERS wFila,wColumna
cAlias = ALIAS()
SELE Cement
lBusca = .F.
IF vToCem#1
	IF !EMPTY(mCodCem)
		IF !SEEK(mCodCem)
			lBusca = .T.
		ENDIF
	 ELSE
		lBusca = .T.
	ENDIF
	
ENDIF

IF lBusca
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD NomCem		&& &v__bus
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
ENDIF
mCodCem = Cement.CodCem

@ wFila,wColumna SAY mCodCem + "   " + Cement.NomCem

SET FILTER TO
SELE (cAlias)

RETURN

*FUNCTION VW_Cuar
*---------------
cAlias = ALIAS()
SELE Cuartel
IF vToCem#1
	SET FILTER TO CodCem=mCodCem
ENDIF
SELE (cAlias)
RETURN

*FUNCTION VV_Cuar
*---------------
PARAMETERS wFila,wColumna
cAlias = ALIAS()
SELE Cuartel
lBusca = .F.
IF vToCem#1
	IF vToCuar#1
		IF !EMPTY(mCodCuar)
			IF !SEEK(mCodCem+mCodCuar)
				lBusca = .T.
			ENDIF
		 ELSE
			lBusca = .T.
		ENDIF
	ENDIF
ENDIF

IF lBusca
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD NomCuar		&& &v__bus
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
ENDIF
mCodCuar = Cuartel.CodCuar

@ wFila,wColumna SAY mCodCuar + "   " + Cuartel.NomCuar

SET FILTER TO
SELE (cAlias)

RETURN


*FUNCTION v_Cem
*-------------
IF SEEK(m.CodCem,"Cement")
	mRet = Cement.NomCem
 ELSE
	mRet = "Error; revise"
ENDIF
RETURN mRet

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0096","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0097","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0098","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0099","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0100","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0101","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0102","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0103","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
