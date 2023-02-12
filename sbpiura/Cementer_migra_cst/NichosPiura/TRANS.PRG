*-----------------------------------------
* Trans.Prg
* Registra Ingreso de Transferencias de 
* Nichos vendidos a ocupacion futuras
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

PUBLIC m.CodCem,m.CodOcu,m.NomOcu,m.ApeOcu,m.SexoOcu,m.EdadOcu,m.DNICli,cCli
PUBLIC m.CodCuar,m.codNic,m.Fila,m.Columna


CLOS DATA
USE Transfer	IN  1 ORDER TAG Transfer1	ALIAS Trans
USE OcupFut		IN  2 ORDER TAG OcupFut1	ALIAS OcuFut
USE Cuartel		IN  3 ORDER TAG Cuartel1	ALIAS Cuartel
USE Cement		IN  4 ORDER TAG Cement1		ALIAS Cement
USE Parmae		IN  5 ORDER TAG Parmae1		ALIAS Parma
USE Clientes	IN  6 ORDER TAG Clientes1	ALIAS Clien

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Transferencias'
vmens02 = ' Transferencias : REVISION '
vmens04 = 'Dicha Transferencia no fue encontrada'
vmens05 = 'No existe Transferencia anterior'
vmens06 = 'No existe Transferencia siguiente'
vmens07 = '¨ Desea Eliminar ‚sta Transferencia ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Transferencia ha sido anulada'
vmens10 = 'La Transferencia ya est  Atendido'
vmens11 = 'La Transferencia ha sido devuelto'

SELECT Trans

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

*@  0, 50 SAY " Estado :"
@  0, 5 SAY "        Titular :"
@  2, 5 SAY "         Codigo :"
@  3, 5 SAY "        Cliente :"
@  4, 5 SAY "Suple./Heredero :"
@  5, 5 SAY "  Fecha Transf. :"
@  6, 5 SAY "       Doc. Ref :"
@  7, 5 SAY "C¢digo Doc. Ref :"
@  8, 5 SAY " Fecha Doc. Ref.:"

@  9, 5 SAY " Ocup. Anterior :"

@ 11, 5 SAY "    Ocup. Nuevo :"

@ 13, 5 SAY "  Observaciones :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Trans

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

*@  0, 60 SAY EstTras()
@  0, 26 SAY m.Titular
@  2, 26 SAY m.CodTrn
@  3, 26 SAY m.codCli
@  3, 33 SAY IIF(m.Titular='S',Val_Cli(m.CodCli,3,33),SPACE(30))
@  4, 26 SAY m.codCli2
@  4, 33 SAY IIF(m.Titular='N',Val_Cli(m.CodCli,4,33),SPACE(30))
@  5, 26 SAY m.FecTrn
@  6, 26 SAY m.DocRef
@  6, 26 SAY VAL_PARA(m.DocRef,'TIPDOC',' ',26,30)
@  7, 26 SAY m.CodDocRf
@  8, 26 SAY m.FecDocRf

@  9, 26 SAY m.CodOcuAn
@  9, 26 SAY Pin_Ocu(m.CodOcuAn,9,35)		&&m.CodOcuAn

@ 11, 26 SAY m.CodOcuNw
@ 11, 26 SAY Pin_Ocu(m.CodOcuNw,11,35)		&&m.CodOcuNw

@ 13, 26 SAY m.Observ

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Trans
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
ON KEY LABEL F10 KEYBOARD CHR(23)
SET RELATION TO CodOcuAn INTO OcuFut
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Trans.CodTrn     :H='Codigo',;
	OcuFut.NomOcu	 :H='Ocupante'

SET RELATION TO
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Trans
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
cOrd1 = ORDER()

vBusca = SPACE(6)
vNombre = "Codigo :"

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA
@ 3,2 SAY vNombre GET vBusca

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vBusca) OR LASTKEY()=27
	* RETURN
 ELSE
	SEEK ALLTRIM(vBusca)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ENDIF
ENDIF
DO vista
SET ORDER TO &cOrd1
RETURN

PROCEDURE anter
*--------------
SELE Trans
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
SELE Trans
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

SELECT Trans
SCATTER MEMVAR
DO pantalla

*@  0, 60 SAY EstTras()

@  0, 26 GET m.Titular	WHEN .F.
@  2, 26 GET m.CodTrn	WHEN .F.
@  3, 26 GET m.codCli	WHEN .F.
@  3, 33 SAY IIF(m.Titular='S',Val_Cli(m.CodCli,3,33),SPACE(30))
@  4, 26 GET m.codCli2	WHEN .F.
@  4, 33 SAY IIF(m.Titular='N',Val_Cli(m.CodCli2,4,33),SPACE(30))
@  5, 26 GET m.FecTrn
@  6, 26 GET m.DocRef	WHEN .F.
@  6, 26 SAY VAL_PARA(m.DocRef,'TIPDOC','A',30,30)
@  7, 26 GET m.CodDocRf	
@  8, 26 GET m.FecDocRf

@  9, 26 GET m.CodOcuAn	WHEN	.F.
@  9, 26 SAY Pin_Ocu(m.CodOcuAn,9,35)		&&m.CodOcuAn

@ 11, 26 GET m.CodOcuNw	WHEN	.F.
@ 11, 26 SAY Pin_Ocu(m.CodOcuNw,11,35)		&&m.CodOcuNw

@ 13, 26 GET m.Observ	FUNCTION "S50"

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Trans
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT Trans
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PRIVATE mAct
SELECT Trans
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

wTras    = Val_Trans()

IF wTras

	DO pantalla

	m.FecTrn   = DATE()
	m.FecDocRf = DATE()
	
	*@  0, 60 SAY EstTras()
	@  0, 26 GET m.Titular	FUNCTION 'M S,N'	VALID Val_Tit()
	@  2, 26 GET m.CodTrn	WHEN	.F.						
	@  3, 26 GET m.CodCli	                    	VALID	Val_OF() AND Val_Cli(m.CodCli,3,33) AND Ver_Cli()
	@  4, 26 GET m.CodCli2	WHEN m.Titular = 'N'	VALID	Val_Cli(m.CodCli2,4,33)
	@  5, 26 GET m.FecTrn
	@  6, 26 GET m.DocRef	VALID	VAL_PARA(m.DocRef,'TIPDOC',' ',26,30)
	@  7, 26 GET m.CodDocRf
	@  8, 26 GET m.FecDocRf
	@  9, 26 GET m.CodOcuAn VALID	Val_OcuAn(m.CodOcuAn,9,35)
	
	@ 11, 26 GET m.CodOcuNw	VALID	Val_OcuNw() AND Pin_ON(11,35)
	
	@ 13, 26 GET m.Observ	FUNCTION "S50"
	
	READ VALID val_read()
	
ENDIF

IF LASTKEY()#27 AND m.wTras
	SELE Trans
	
	mAct = .T.
	
	IF SEEK(m.CodOcuAn,'OcuFut')
		SELE OcuFut
		IF mAct
			IF f_Lock(1)
				REPLACE OcuFut.Estado WITH '30', CodTrn WITH m.CodTrn
			ENDIF
		ENDIF
	 ELSE
		DO STANDBY WITH "No se Encontr¢ Dicha Ocupaci¢n futura"
		mAct = .F.
	ENDIF
	
	IF mAct
		mAct = Act_NwCod()
	ENDIF
	
	IF mAct
		IF SEEK('CORRELTRANSF','Parma')
			SELE Parma
			IF f_Lock(1)
				REPLACE NumEnt WITH Parma.NumEnt+1
			ENDIF
		ENDIF
	ENDIF
	
	SELE Trans
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
	UNLOCK ALL
 ELSE
	IF EOF()
		GO BOTT
	 ELSE
		go vtemp
	ENDIF
ENDIF

FLUSH

SELECT Trans

DO vista

RETURN

FUNCTION Val_OF
*--------------
PRIVATE mRet,cAlias,cOrd1,cOrd2

mRet = .T.
cAlias = ALIAS()
vIdx1  = SYS(3)+ '.Idx'

SELECT OcuFut.*, Clien.NomCli;
	FROM OcuFut, Clien;
	WHERE OcuFut.CodCli = Clien.CodCli AND OcuFut.Estado='10';
	ORDER BY NomCli;
	INTO Cursor tmpTrn

SELE TmpTrn

IF !EOF()
	vTempo = '°°°°°°°°°°° ®F10¯ Seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DEFINE WINDOW lista FROM 00,00 TO 24,79 DOUBLE ;
	TITLE vTempo FLOAT COLOR SCHEME 2
	ACTIVATE WINDOWS Lista
	
	ON KEY LABEL F10 KEYBOARD CHR(23)
*	ON KEY LABEL F2 DO Busdetcr
	
	BROWSE WINDOWS Lista NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
			NomCli	:H='Cliente':30,;
			ApeOcu	:H='Ap. Ocup.':15,;
			NomOcu	:H='Nom. Ocup.':15
	
	ON KEY LABEL F10
*	ON KEY LABEL F2
	DEACTIVATE WINDOW Lista
	RELEASE WINDOW Lista
	
	IF LASTKEY()#27
		m.CodCli   = CodCli
	ELSE
		mRet = .F.
	ENDIF
ELSE
	DO standBy WITH "No Existen Ocupaciones Futuras"
	mRet = .F.
ENDIF

USE IN TmpTrn
*ERASE (vIdx1)

SELE (cAlias)
SHOW GETS

RETURN mRet

FUNCTION Pin_ON
*--------------
PARAMETERS _Fil,_Col
PRIVATE cAlias

cAlias = ALIAS()
SELE OcuFut
IF SEEK(m.CodOcuNw)
	@ _Fil,_Col SAY ALLTRIM(ApeOcu) + ', ' + ALLTRIM(NomOcu)
 ELSE
	@ _Fil,_Col SAY ALLTRIM(m.ApeOcu) + ', ' + ALLTRIM(m.NomOcu)
ENDIF
SELE (cAlias)

RETURN



FUNCTION Val_Tit
*---------------
IF m.Titular = 'S'
	m.CodCli2 = SPACE(6)
	SHOW GET m.CodCli2
 ELSE
	m.CodCli  = SPACE(6)
	SHOW GET m.CodCli
ENDIF
RETURN

FUNCTION Ver_Cli
*---------------
PRIVATE mRet,cAlias,cOrd
cAlias = ALIAS()
SELE OcuFut
cOrd = ORDER()
SET ORDER TO OcupFut3
SET FILTER TO Estado = '10'
IF !SEEK(m.CodCli)
	DO standBy WITH "El solicitante no tiene nicho reservado"
	m.codcli = SPACE(6)
	mRet = .F.
 ELSE
	mRet = .T.
ENDIF
SET FILTER TO
SET ORDER TO cOrd
SELE (cAlias)

RETURN mRet

FUNCTION Val_Trans
*-----------------
IF SEEK("CORRELTRANSF","Parma")
	m.codTrn = PADL(ALLTRIM(STR(Parma.NumEnt+1)),6,'0')
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodTrn)
	DO StandBy WITH "El Codigo esta vacio"
	RETURN .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodTrn)
		DO standby WITH 'Ya esta Registrado este Codigo'
		RETURN .F.
	ENDIF
ENDIF
SHOW GET m.CodTrn
RETURN .T.

PROCEDURE Elimi
*--------------
PRIVATE lElimina

DO standBy WITH "Ojo. poner claves de Acceso, y revisar bien bien"

*RETURN

lElimina = .T.
IF YesNo("Desea Eliminar este registro")
*	IF !ve_passw("ELIDIF")
*		DO STANDBY WITH "Proceso Cancelado"
*		lElimina = .F.
*	ENDIF
 ELSE
	DO STANDBY WITH "Proceso Cancelado"
	lElimina = .F.
ENDIF

IF lElimina
	IF SEEK(m.CodOcuAn,'OcuFut')
		SELE OcuFut
		IF lElimina
			SELE OcuFut
			REPLACE Estado WITH '00',CodTrn WITH SPACE(6)
		 ELSE
			lElimina = .F.
		ENDIF
	 ELSE
		DO STANDBY WITH "No se Encontr¢ Ocupaci¢n futura Anterior"
	ENDIF
ENDIF

IF lElimina
	IF SEEK(m.CodOcuNw,'OcuFut')
		SELE OcuFut
		DELE NEXT 1
		SKIP -1
		IF BOF()
			GO TOP
		ENDIF
	ENDIF
	
	SELE Trans
	DELE NEXT 1
	SKIP -1
	IF BOF()
		GO TOP
	ENDIF
ENDIF

SELE Trans

DO VISTA
RETURN

**PROCEDURE lista
*--------------
PRIVATE nReg
SELE Difunto 

IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
nReg = RECNO()

SET ORDER TO Difunto3
SELE Cuartel
SET ORDER TO Cuartel1
SELE Difunto
SET RELATION TO CodCem		   INTO Cement  ADDITIVE
SET RELATION TO CodCem+CodCuar INTO Cuartel ADDITIVE

DEFINE WINDOW LIS FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Nichos' COLOR SCHEME 5
ACTIVATE WINDOW LIS

vToCem  = 1
vToCuar = 1
vToFila = 1
mCodCem  = SPACE(2)
mCodCuar = SPACE(4)
mFila    = SPACE(1)

@ 01,01 SAY "Todas los Cementerios : " GET vToCem  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 03,01 SAY "Cementerio : "
@ 03,15 GET mCodCem PICTURE '@!' WHEN vToCem = 2  VALID Val_Fun('Cement','CodCem','NomCem',mCodCem,1,3,19)
@ 05,01 SAY "Todas los Cuarteles : " GET vToCuar  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6 WHEN vToCem = 2
@ 07,01 SAY "Cuartel : "
@ 07,15 GET mCodCuar PICTURE '@!' WHEN vToCem = 2 AND vToCuar = 2 VALID VV_Cuar(7,15)
@ 09,01 SAY "Todas los Filas : " GET vToFila  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6 WHEN vToCem = 2 AND vToCuar = 2
@ 11,01 SAY "Fila : "
@ 11,15 GET mFila PICTURE '@!' WHEN vToCem = 2 AND vToCuar = 2 AND vToFila = 2  VALID VV_Fila(11,15)

@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

IF OkCancel = 1 AND LASTKEY()#27
	SET FILTER TO CodCem = ALLTRIM(mCodCem) AND CodCuar = ALLTRIM(mcodcuar) AND Fila = ALLTRIM(mFila)
	DO REPORTE WITH 2,"Difuntos","Listado de Difuntos"
	SET FILTER TO
	SET RELATION TO
	SET ORDER TO Difunto1
ENDIF

SELE Cuartel
SET ORDER TO Cuartel1
SELE Difunto
GO nReg
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


*FUNCTION EstDif
*--------------
IF m.Estado ='10'
	@ ROW(),COL() SAY SPACE(15)
ENDIF
IF m.Estado ='10'
	@ ROW(),COL() SAY "TRASLADADO" COLOR SCHEME 5
ENDIF
RETURN

****************************


FUNCTION Pin_Ocu
*---------------
PARAMETERS xCod,_Fil,_Col
PRIVATE cAlias
cAlias = ALIAS()
IF SEEK(xCod,'OcuFut')
	SELE OcuFut
	@ _Fil, _Col SAY alltrim(ApeOcu) + ', ' + alltrim(NomOcu)
 ELSE
	@ _Fil, _Col SAY SPACE(30)
ENDIF
SELE (cAlias)

RETURN


FUNCTION Val_OcuAn
*-----------------
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PARAMETERS xcod,_Fil,_Col
PRIVATE cOrd

mAlias = ALIAS()
v_fun = .F.
v_Ent = .F.

SELE OcuFut
GO TOP
cOrd = ORDER()
SET ORDER TO OcupFut1

SET FILTER TO Estado='10' AND CodCli = m.Codcli

GO TOP

IF !EOF()
	IF !EMPTY(xcod)
		SEEK xcod
		v_Ent = FOUND()
		v_Fun  = .T.
	ENDIF
	
	IF !V_ENT
		SET ORDER TO OcupFut2
		GO TOP
		ACTIVATE SCREEN
		ON KEY LABEL F10 KEYBOARD CHR(23)
		ON KEY LABEL F2 DO BusOcu		&&BusDet
		
		DEFINE WINDOW _BusCli FROM 2,01 TO 22,78
		ACTIVATE WINDOWS _BusCli
		
		BROWSE WINDOW _BusCli TITLE '²²²²²² [F10] Selecciona  ²²  [F2] Buscar ²²²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		 FIELDS;
			ApeOcu   :H='Apellidos',;
			NomOcu   :H='Nombres'
		
		IF !Lastkey()=27
			v_ent = .T.
		ENDIF
		
		ON KEY LABEL F10
		ON KEY LABEL F2
		DEACTIVATE WINDOWS _BusCli
		RELEASE    WINDOWS _BusCli
	ENDIF
	
	
	IF V_ENT
		xCod		= CodOcu
		m.NomOcu	= NomOcu
		m.ApeOcu	= ApeOcu
		m.SexoOcu	= SexoOcu
		m.EdadOcu	= EdadOcu
		m.DNICli	= DNICli
		m.CodCem	= CodCem
		m.CodCuar	= CodCuar
		m.codNic	= CodNic
		m.Fila		= Fila
		m.Columna	= Columna
		
		SHOW GETS
		v_fun = .T.
		
		IF !EMPTY(_Fil) OR !EMPTY(_Col)
			@ _Fil, _Col SAY ALLTRIM(m.ApeOcu) + ', ' + ALLTRIM(m.NomOcu)
		ENDIF
	ENDIF
 ELSE
	DO standby WITH "No hay Registros para Procesar"
ENDIF

SET FILTER TO

SET ORDER TO &cOrd

ON KEY LABEL F8
ON KEY LABEL F10
UNLOCK ALL

SELECT (mAlias)

RETURN v_Fun

****
PROCEDURE BusOcu
*---------------
ON KEY LABEL F2

DEFINE WINDOW wlista FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW wlisTA

vBusca = SPACE(30)

@ 3,2 SAY "Apellidos + Nombre " GET vBusca

READ VALID val_read()

DEACTIVATE WINDOW wlista

IF EMPTY(vBusca) OR LASTKEY()=27
	* RETURN
 ELSE
	SEEK ALLTRIM(vBusca)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ENDIF
ENDIF
ON KEY LABEL F2 do BusOcu
RETURN

****

FUNCTION Val_OcuNw
*-----------------
PRIVATE xAlias,mRet

xAlias = ALIAS()

SELE OcuFut

DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
	TITLE "Ingresando Nuevo Ocupante" COLOR SCHEME 5

ACTIVATE WIND w_Cli

cCli = IIF(m.Titular='S',m.CodCli,m.CodCli2)

*m.codCem  = SPACE(2)
=Cor_OF()
*m.CodOcu  = SPACE(8)
*m.CodCuar = SPACE(4)
*m.CodNic  = SPACE(6)
*m.Fila    = SPACE(1)
*m.Columna = SPACE(3)
*m.DNICli  = SPACE(8)
m.NomOcu  = SPACE(30)
m.ApeOcu  = SPACE(30)
m.SexoOcu = ' '
m.EdadOcu = 0

@ 01,01 SAY "  Cementerios :" GET m.CodCem		WHEN .F. 
@ 01,01 SAY	Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,31)
@ 02,01 SAY " Nuevo Codigo :" GET m.CodOcu		WHEN .F.
@ 03,01 SAY "      Cliente :" GET cCli			WHEN .F.
@ 03,01 SAY Val_Cli(cCli,3,33)
@ 04,01 SAY "  DNI Cliente :" GET m.DNICli		WHEN .F.
@ 05,01 SAY "Nvo.Ape.Ocup. :" GET m.ApeOcu		PICTURE "@!S40"
@ 06,01 SAY "Nvo.Nom.Ocup. :" GET m.NomOcu		PICTURE "@!S40"
@ 07,01 SAY "Sexo Ocupante :" GET m.SexoOcu		FUNCTION "M M,F"	VALID VAL_PARA(m.SexoOcu,'SEXO  ',' ',17,20)
@ 08,01 SAY "Edad Ocupante :" GET m.EdadOcu
@ 09,01 SAY "      Cuartel :" GET m.CodCuar		WHEN .F.
@ 09,01 SAY Val_Cuar(08,31)
@ 10,01 SAY "         Fila :" GET m.Fila		WHEN .F.
@ 11,01 SAY "      Columna :" GET m.Columna		WHEN .F.

READ VALID val_read()

DEACTIVATE WINDOW w_Cli
RELEASE WINDOW w_Cli

IF LASTKEY()#27
	m.CodOcuNw	= m.CodOcu
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF

SELE (xAlias)

RETURN mRet

PROCEDURE Act_NwCod
*------------------
* Actualiza el nuevo codigo de Ocup. Futura
*******************************************

PRIVATE mAct

mAct = .T.

IF LastKey()#27
	IF mAct
		SELE OcuFut
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH '10',CodCli WITH cCli, CodTrn WITH SPACE(6)
		ENDIF
		
		SELE Parma
		SEEK "CORRELOCUFUT"+m.codcem
		
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
		
	ENDIF
 ELSE
	DO StandBy WITH "No agrego ningun codigo "
ENDIF

RETURN mAct

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0056","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0057","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0058","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0059","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0060","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0061","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0062","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0063","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
