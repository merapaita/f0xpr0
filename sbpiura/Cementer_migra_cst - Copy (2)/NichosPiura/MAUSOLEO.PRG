*------------------------------------
* Mausoleo.Prg
* Registra los Mausoleos de los Cementerios 
* 
* AUTOR : A. S. Marco Rodriguez Abad
*------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Mausoleo IN 1 ORDER TAG Mausoleo1	ALIAS Mausoleo
USE Cement	 IN 2 ORDER TAG Cement1		ALIAS Cement
USE Parmae	 IN 3 ORDER TAG Parmae1		ALIAS Parma
USE Clientes IN 4 ORDER TAG Clientes1	ALIAS Clien
*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Mausoleos'
vmens02 = ' Mausoleo : REVISION '
vmens04 = 'Dicho Mausoleo no fue encontrado'
vmens05 = 'No existe Mausoleo anterior'
vmens06 = 'No existe Mausoleo siguiente'
vmens07 = '¨ Desea Anular ‚ste Mausoleo ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Mausoleo ha sido anulado'

SELECT Mausoleo
GO BOTT

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

@  1,50 SAY " Estado :"
@  2,10 SAY "      Cementerio :"
@  4,10 SAY " C¢digo Mausoleo :"
@  5,10 SAY "  Lotizado (S/N) :"
@  6,10 SAY "     Nombre Lote :"
@  7,10 SAY "         Familia :"
@  8,10 SAY "            Tipo :"
@ 10,10 SAY "       Ubicaci¢n :"
@ 12,10 SAY "  µrea Adquirida :"
@ 13,10 SAY " µrea Construida :"
@ 14,10 SAY "    µrea Cercada :"
@ 16,10 SAY "  Tope Max. Dif. :"
@ 18,10 SAY " Dif. Enterrados :"
@ 20,10 SAY "     Solicitante :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Mausoleo

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

DO CASE
	CASE m.EstVta ='00'
		@  0, 10 SAY SPACE(15)
	CASE m.EstVta ='10'
		@  0, 10 SAY "Cancelado"	COLOR SCHEME 5
	CASE m.EstVta ='20'
		@  0, 10 SAY "Con Saldo"	COLOR SCHEME 5
	CASE m.EstVta ='99'
		@  0, 10 SAY "Anulado"		COLOR SCHEME 5
ENDCASE

@  1,60 SAY EstMau()
@  2,30 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,2,30)	
@  4,30 SAY m.CodMau
@  5,30 SAY m.Lotizado
@  6,30 SAY LEFT(m.NomLote,50)
@  7,30 SAY m.Familia
@  8,30 SAY VAL_PARA(m.Tipo,'TIPMAU','V',30,20)
@ 10,30 SAY m.Ubicacion
@ 12,30 SAY m.Area_Adq
@ 13,30 SAY m.Area_Const
@ 14,30 SAY m.Area_Cerc
@ 16,30 SAY m.TotDif
@ 18,30 SAY m.NumDif
@ 20,30 SAY m.CodCli
@ 20,30 SAY IIF(EMPTY(m.codcli),SPACE(50),Val_Fun('Clien','CodCli','NomCli',m.CodCli,1,20,38))

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Mausoleo
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
	x=v_Cem(Mausoleo.CodCem)	:H='Cementerio':30,;
	Mausoleo.CodMau				:H='Codigo',;
	Mausoleo.Familia			:H='Familia o Cong.':30

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE Mausoleo
DO vista
RETURN

FUNCTION v_Cem
*-------------
PARAMETERS xCem
IF SEEK(xCem,"Cement")
	mRet = Cement.NomCem
 ELSE
	mRet = "Error; revise"
ENDIF
RETURN mRet

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vCodCem  = SPACE(2)
vCodMau  = SPACE(4)

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA
@ 1,2 SAY 'Cementerio :' GET vCodCem   PICTURE '!!'	VALID Val_Fun('Cement','CodCem','NomCem',vCodCem,1,1,20)
@ 3,2 SAY '  Mausoleo :' GET vCodMau   PICTURE '!!!!'

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vCodCem) OR EMPTY(vCodMau) OR LASTKEY()=27
	RETURN
 ELSE
	SEEK vCodCem+vCodMau
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
SELE Mausoleo
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
SELE Mausoleo
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

SELECT Mausoleo
SCATTER MEMVAR
DO pantalla

@  2,30 GET m.CodCem	PICTURE '!!'	VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,2,30)	DISABLE
@  4,30 GET m.CodMau	PICTURE '!!!!' 	DISABLE
@  5,30 GET m.Lotizado	FUNCTION 'M S,N'
@  6,30 GET m.NomLote	WHEN m.Lotizado = 'S'	FUNCTION '!S40'
@  7,30 GET m.Familia	FUNCTION '!S40'
@  8,30 GET m.Tipo						VALID VAL_PARA(m.Tipo,'TIPMAU',' ',30,20)
@ 10,30 GET m.Ubicacion	FUNCTION '!S30'
@ 12,30 GET m.Area_Adq
@ 13,30 GET m.Area_Const
@ 14,30 GET m.Area_Cerc
@ 16,30 GET m.TotDif
@ 18,30 GET m.NumDif	DISABLE
@ 20,30 GET m.Codcli	VALID IIF(EMPTY(m.Codcli),.T.,Val_Cli(m.CodCli))

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Mausoleo
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
FLUSH
SELECT Mausoleo
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Mausoleo
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK
m.estado = '10'

DO pantalla

@  2,30 GET m.CodCem	PICTURE '!!'	VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,2,30) AND Cor_Maus()
@  4,30 GET m.CodMau	WHEN .F.
@  5,30 GET m.Lotizado	FUNCTION 'M S,N'
@  6,30 GET m.NomLote	WHEN m.Lotizado = 'S'	FUNCTION '!S40'
@  7,30 GET m.Familia	FUNCTION '!S40'
@  8,30 GET m.Tipo						VALID VAL_PARA(m.Tipo,'TIPMAU',' ',30,20)
@ 10,30 GET m.Ubicacion	FUNCTION '!S30'
@ 12,30 GET m.Area_Adq
@ 13,30 GET m.Area_Const
@ 14,30 GET m.Area_Cerc
@ 16,30 GET m.TotDif
@ 18,30 GET m.NumDif	DISABLE
@ 20,30 GET m.Codcli	VALID Val_Cli(m.CodCli,20,35)

READ VALID val_read()

IF LASTKEY()#27
	IF F_Appd()
		GATHER MEMVAR
		SELE Parma
		SEEK "CORRELMAUSOL"+m.codcem
		REPLACE NumEnt WITH Parma.NumEnt+1
	ENDIF
	UNLOCK ALL
 ELSE
	IF vtemp = -1
		GO TOP
	 ELSE
		GO vtemp
	ENDIF
ENDIF

FLUSH

SELECT Mausoleo

DO vista
RETURN


PROCEDURE elimi
*--------------
SELECT Mausoleo

IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

lBorra = yesno('¨ Desea ELIMINAR ‚ste Mausoleo ?')

IF lBorra
	SELE Mausoleo
	IF SEEK(m.CodMau)
		IF m.Estado = '10'
			USE Difunto  IN  0 ORDER TAG Difunto3	ALIAS Difunto
			SELE Difunto
			IF !SEEK(m.CodMau)
				lBorra = .T.
			 ELSE
				lBorra = .F.
			ENDIF
			USE IN Difunto
		 ELSE
			lBorra = .F.
		ENDIF
		
		IF lBorra
			SELE Mausoleo
			DELE NEXT 1
			SKIP -1
			IF BOF()
				GO TOP
			ENDIF
		 ELSE
			DO StandBy WITH "El Mausoleo no puede ser borado por que ya esta ocupado"
		ENDIF
	ENDIF

ENDIF
SELECT Mausoleo
UNLOCK ALL

DO vista
RETURN

PROCEDURE lista
*--------------
PRIVATE nReg,lDif,cord
vIdx = SYS(3)+'.Idx'

SELE Mausoleo

IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
nReg = RECNO()
cOrd = ORDER()
lDif = .F.

IF !USED("Difunto")
	lDif = .T.
	SELE 0
	USE difunto IN 0
	INDEX ON codCem+CodMau+codDif to (vIdx) FOR !EMPTY(codmau)
ENDIF

SELE Mausoleo
SET RELATION TO CodCem+CodMau INTO Difunto ADDITIVE
SET SKIP TO Difunto

DEFINE WINDOW LIS FROM 07,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Mausoleos' COLOR SCHEME 5
ACTIVATE WINDOW LIS

vToCem  = 1
vOr = 2
mCodCem  = SPACE(2)
lDif = .F.
xDif = .F.

@ 01,01 SAY "Todas los Cementerios : " GET vToCem  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 03,01 SAY "    Cementerio : "
@ 03,19 GET mCodCem PICTURE '@!' WHEN vToCem = 2  VALID Val_Fun('Cement','CodCem','NomCem',mCodCem,1,3,23)
@ 05,01 SAY "      Ordenado : " GET vOr  PICTURE '@^ codigo;Familias' SIZE 1,15,6
@ 09,01 SAY "Incluir Difuntos.?" GET xDif FUNCTION '*C'

@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

IF OkCancel = 1 AND LASTKEY()#27
	IF vOr = 2
		SET ORDER TO Mausoleo2
	ENDIF
	IF xDif
		DO REPORTE WITH 2,"Mausol1","Listado de Mausoleos con difuntos"
	ELSE
		DO REPORTE WITH 2,"Mausoleo","Listado de Mausoleos"
	ENDIF
ENDIF

SET RELATION TO
SET ORDER TO (cOrd)
IF lDif
	USE IN Difunto
	ERASE (vIdx)
ENDIF

SELE Mausoleo
GO nReg
DO Vista
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


FUNCTION EstMau
*--------------
IF m.Estado ='00'
	@ ROW(),COL() SAY SPACE(15)
 ELSE
	@ ROW(),COL() SAY "OCUPADO" COLOR SCHEME 5
ENDIF
RETURN

FUNCTION Cor_Maus
*----------------
cAlias = ALIAS()
SELE Mausoleo
*SET FILTER TO CodCem=m.codcem
SELE &cAlias
mRet = .T.
IF SEEK("CORRELMAUSOL"+m.codcem,"Parma")
	m.codMau = PADL(ALLTRIM(STR(Parma.NumEnt+1)),4,'0')
	m.CodMau = m.CodCem + m.CodMau
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodMau)
	DO StandBy WITH "El Codigo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodMau)
		DO standby WITH 'Ya esta Registrado este Codigo'
		mRet = .F.
	ENDIF
ENDIF
SHOW GET m.codMau
SELE (calias)
IF mRet
	RETURN .T.
 ELSE
	RETURN .F.
ENDIF

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0087","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0088","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0089","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0090","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0091","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0092","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0093","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0094","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN

