*-----------------------------------------
* Traslado.Prg
* Registra Ingreso de Traslados de Difuntos
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

PUBLIC m.fecFall,m.FecSep,m.CodDif,m.ApeDif,m.NomDif,m.SexoDif,m.EdadDif,m.CodCem
PUBLIC m.CodCuar,m.codNic,m.Fila,m.Columna,m.TipEnt,m.CodMau
PUBLIC m.CodOcu,m.Reservado

CLOS DATA
USE Traslado	IN  1 ORDER TAG Traslado1	ALIAS Traslado
USE Difunto		IN  2 ORDER TAG Difunto1	ALIAS Difunto
USE Cuartel		IN  3 ORDER TAG Cuartel1	ALIAS Cuartel
USE Cement		IN  4 ORDER TAG Cement1		ALIAS Cement
USE Nichos		IN  5 ORDER TAG Nichos1		ALIAS Nichos
USE Parmae		IN  6 ORDER TAG Parmae1		ALIAS Parma
USE Mausoleo	IN  7 ORDER TAG Mausoleo1	ALIAS Mausol
USE Clientes	IN  8 ORDER TAG Clientes1	ALIAS Clien

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Traslados'
vmens02 = ' Traslados : REVISION '
vmens04 = 'Dicho Traslado no fue encontrado'
vmens05 = 'No existe Traslado anterior'
vmens06 = 'No existe Traslado siguiente'
vmens07 = '? Desea Eliminar ?ste Traslado ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Traslado ha sido anulado'
vmens10 = 'El Traslado ya est? Atendido'
vmens11 = 'El Traslado ha sido devuelto'

SELECT Traslado

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

PROCEDURE pantalla           && Pinta m?scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

*@  0, 50 SAY " Estado :"
@  1, 5 SAY "         Codigo :"
@  2, 5 SAY "        Cliente :"
@  3, 5 SAY " Fecha Traslado :"
@  4, 5 SAY "       Doc. Ref :"
@  5, 5 SAY "C?digo Doc. Ref :"
@  6, 5 SAY " Fecha Doc. Ref.:"
@  7, 5 SAY " Tipo  Traslado :"
@  8, 5 SAY " Traslado local :"

@ 10, 5 SAY "  Cod. Anterior :"
@ 11, 5 SAY "  Cem. Anterior :"
@ 12, 5 SAY "  Cuartel  Ant. :"
@ 13, 5 SAY "  Mausoleo Ant. :"

@ 15, 5 SAY "  Cod.   Nuevo :"
@ 16, 5 SAY "  Cem.   Nuevo :"
@ 17, 5 SAY "  Cuartel  Nw. :"
@ 18, 5 SAY "  Mausoleo Nw. :"

@ 20, 5 SAY "  Observaciones :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Traslado

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

*@  0, 60 SAY EstTras()
@  1, 26 SAY m.CodTras
@  2, 26 SAY m.codCli
@  2, 33 SAY Val_Cli(m.CodCli,2,33)
@  3, 26 SAY m.FecTras
@  4, 26 SAY m.DocRef
@  4, 26 SAY VAL_PARA(m.DocRef,'TIPDOC',' ',26,30)
@  5, 26 SAY m.CodDocRf
@  6, 26 SAY m.FecDocRf
@  7, 26 SAY VAL_PARA(m.TipTras,'TIPTRS',' ',26,30)
@  8, 26 SAY m.Local

@ 10, 26 SAY m.CodDifAn
@ 10, 26 SAY Nom_DifAn(m.CodDifAn,10,35)
@ 11, 26 SAY IIF(!EMPTY(m.CodCemAn),Val_Fun('Cement','CodCem','NomCem',m.CodCemAn,1,11,26),SPACE(15)) 
@ 12, 26 SAY IIF(!EMPTY(m.CodCuarAn),Val_CuarAn(12,26),SPACE(15))
@ 12, 65 SAY IIF(!EMPTY(m.CodCuarAn),m.FilaAn+' '+m.ColumnaAn,SPACE(10)) COLOR SCHEME 5
@ 13, 26 SAY IIF(!EMPTY(m.CodMauAn),Val_MauAn(13,26),SPACE(15))

@ 15, 26 SAY m.CodDifNw
@ 16, 26 SAY IIF(!EMPTY(m.CodCemNw),Val_Fun('Cement','CodCem','NomCem',m.CodCemNw,1,16,26),SPACE(15))
@ 17, 26 SAY IIF(!EMPTY(m.CodCuarNw),Val_CuarNw(17,26),SPACE(15))
@ 17, 65 SAY IIF(!EMPTY(m.CodCuarNw),m.FilaNw+' '+m.ColumnaNw,SPACE(10)) COLOR SCHEME 5
@ 18, 26 SAY IIF(!EMPTY(m.CodMauNw),Val_MauNw(18,26),SPACE(15))

@ 20, 26 SAY m.Observ

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi?n de BD en browse
*--------------
SELE Traslado
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
ON KEY LABEL F10 KEYBOARD CHR(23)
SET RELATION TO CodDifAn INTO Difunto
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Traslado.CodTras   :H='Codigo',;
	Difunto.Nomdif

SET RELATION TO
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Traslado
DO vista
RETURN

PROCEDURE busca              && Realiza b?squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
cOrd1 = ORDER()

vBusca = SPACE(8)
vNombre = "Codigo :"

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5

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
SELE Traslado
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
SELE Traslado
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

SELECT Traslado
SCATTER MEMVAR
DO pantalla

*@  0, 60 SAY EstTras()

@  1, 26 GET m.CodTras	WHEN	.F.
@  2, 26 GET m.CodCli	VALID	Val_Cli(m.CodCli,2,33)
@  3, 26 GET m.FecTras
@  4, 26 GET m.DocRef	VALID	VAL_PARA(m.DocRef,'TIPDOC',' ',26,30)
@  5, 26 GET m.CodDocRf	
@  6, 26 GET m.FecDocRf
@  7, 26 SAY VAL_PARA(m.TipTras,'TIPTRS',' ',26,30)
@  8, 26 SAY Whe_Local()
@ 10, 26 SAY m.CodDifAn
@ 10, 36 SAY Val_dat(1)
@ 15, 26 SAY m.CoddifNw
@ 15, 36 SAY Val_dat(2)
@ 20, 26 GET m.Observ	FUNCTION "S50"

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Traslado
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT Traslado
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PRIVATE mAct
SELECT Traslado
vtemp = IIF(EOF(),-1,RECNO())

wait wind "hola"

SCATTER MEMVAR BLANK

wTras    = Val_Tras()

IF wTras

	DO pantalla

	m.FecTras  = DATE()
	m.FecDocRf = DATE()
	
	*@  0, 60 SAY EstTras()
	@  1, 26 GET m.CodTras	WHEN	.F.
	@  2, 26 GET m.CodCli	VALID	Val_Cli(m.CodCli,2,33)
	@  3, 26 GET m.FecTras
	@  4, 26 GET m.DocRef	VALID	VAL_PARA(m.DocRef,'TIPDOC',' ',26,30)
	@  5, 26 GET m.CodDocRf
	@  6, 26 GET m.FecDocRf
	@  7, 26 GET m.Tiptras	VALID	VAL_PARA(m.TipTras,'TIPTRS',' ',26,30)
	@  8, 26 GET m.Local	WHEN	Whe_Local() PICTURE "@M S,N"	VALID V_local()
	@ 10, 26 GET m.CodDifAn VALID	Val_DifAn(m.CodDifAn,10,35) AND Val_dat(1)
	
	@ 15, 26 GET m.CodDifNw	WHEN	BETWE(VAL(m.TipTras),1,3) OR (BETWE(VAL(m.TipTras),4,6) AND m.local = 'S') VALID Val_DifNw() AND Val_dat(2)
*	@ 16, 26 GET m.Observ	FUNCTION "S50"
	
	READ VALID val_read()
	
ENDIF
	
IF LASTKEY()#27 AND m.wTras
	SELE Traslado
	
	mAct = .T.
	
	IF SEEK(m.CodDifAn,'Difunto')
		SELE Difunto
		DO CASE
			CASE TipEnt = '1'
				IF SEEK(Difunto.CodCem+Difunto.CodCuar+LEFT(Difunto.CodNic,1),'Nichos')
					SELE Nichos
					Campo = "Col"+SUBSTR(Difunto.Codnic,2,3)+'E'
					IF f_lock(1)
						REPLACE &Campo WITH "4"
					ENDIF
				 ELSE
					mAct = .F.
				ENDIF
			CASE TipEnt = '2'
				IF SEEK(Difunto.CodMau,'Mausol')
					SELE Mausol
					REPLACE NumDif WITH NumDif + 1,Estado WITH '20'
				 ELSE
					DO STANDBY WITH "No se Encontr? Mausoleo"
					mAct = .F.
				ENDIF
		ENDCASE
		IF mAct
			IF f_Lock(1)
				REPLACE Difunto.Estado WITH '30',Difunto.CodTras WITH m.CodTras
			ENDIF
		ENDIF
	 ELSE
		DO STANDBY WITH "No se Encontr? Difunto"
		mAct = .F.
	ENDIF
	
	IF mAct
		IF m.Local = 'S'
			mAct = Act_NwCod()
		ENDIF
	ENDIF
	
	IF mAct
		IF SEEK('CORRELTRASLD','Parma')
			SELE Parma
			IF f_Lock(1)
				REPLACE NumEnt WITH Parma.NumEnt+1
			ENDIF
		ENDIF
	ENDIF
	SELE Traslado
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

SELECT Traslado

DO vista

RETURN


FUNCTION Whe_local
*-----------------
IF m.Tiptras = '1' OR m.Tiptras = '2' OR m.Tiptras ='3'
	m.Local = "S"
	mret = .F.
 ELSE
	mret = .T.
ENDIF
SHOW GET m.Local
RETURN mRet

FUNCTION V_local
*---------------
DO CASE
	CASE BETW(VAL(m.TipTras),1,3)
		m.Local = 'N'
	CASE  BETW(VAL(m.TipTras),4,6)
		IF YesNo("Considerar solo si tenemos m?s Cementerios. ?Tenemos m? Cementerios?")
			m.Local = 'S'
		ELSE
			m.Local = 'N'
		ENDIF
ENDCASE
RETURN

FUNCTION Val_Tras
*----------------
IF SEEK("CORRELTRASLD","Parma")
	m.codTras = PADL(ALLTRIM(STR(Parma.NumEnt+1)),6,'0')
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodTras)
	DO StandBy WITH "El Codigo esta vacio"
	RETURN .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodTras)
		DO standby WITH 'Ya esta Registrado este Codigo'
		RETURN .F.
	ENDIF
ENDIF
SHOW GET m.CodTras
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
	IF SEEK(m.CodDifAn,'Difunto')
		SELE Difunto
		DO CASE
			CASE TipEnt = '1'
				IF SEEK(Difunto.CodCem+Difunto.CodCuar+LEFT(Difunto.CodNic,1),'Nichos')
					SELE Nichos
					Campo = "Col"+SUBSTR(m.CodnicAn,2,3)+'E'
					IF f_lock(1)
						REPLACE &Campo WITH "3"
					ENDIF
				 ELSE
					DO STANDBY WITH "No se Encontr? Nicho"
					lElimina = .F.
				ENDIF
			CASE TipEnt = '2'
				IF SEEK(Difunto.CodMau,'Mausol')
					SELE Mausol
					REPLACE NumDif WITH NumDif + 1,Estado WITH '20'
				 ELSE
					DO STANDBY WITH "No se Encontr? Mausoleo"
					lElimina = .F.
				ENDIF
		ENDCASE
		
		IF lElimina
			SELE Difunto
			REPLACE Estado WITH '00',CodTras WITH SPACE(6)
		 ELSE
			lElimina = .F.
		ENDIF
	 ELSE
		DO STANDBY WITH "No se Encontr? Difunto Anterior"
	ENDIF
	
	IF lElimina
		IF SEEK(m.CodCemAn+m.CodCuarAn+LEFT(m.CodNicAn,1),'Nichos')
			SELE Nichos
			Campo = "Col"+SUBSTR(m.CodnicAn,2,3)+'E'
			IF f_lock(1)
				REPLACE &Campo WITH "3"
			ENDIF
		 ELSE
			lElimina = .F.
		ENDIF
	ENDIF
ENDIF

IF lElimina
	IF SEEK(m.CodDifNw,'Difunto')
		SELE Difunto
		DO CASE
			CASE Tipent = '1'
				IF !EMPTY(Fila) and !EMPTY(Columna)
					fCampo = "Col"+PADL(SUBSTR(CodNic,2,3),3,'0')+'E'
					IF !EMPTY(Difunto.CodOcu)
						SELE OcuFut
						SEEK Difunto.CodOcu
						REPLACE ESTADO WITH "10"
						
						SELE Nichos
						SEEK Difunto.CodCem+Difunto.CodCuar+LEFT(Difunto.CodNic,1)
						REPLACE &fCampo WITH "2"
					 ELSE
						SELE Nichos
						SEEK Difunto.CodCem+Difunto.CodCuar+LEFT(Difunto.CodNic,1)
						REPLACE &fCampo WITH "1"
					ENDIF
				 ELSE
					DO STANDBY WITH "Error en nicho no se puede eliminar"
					lElimina = .F.
				ENDIF
			CASE Tipent = '2'
				SELE Mausol
				IF SEEK(Difunto.CodMau)
					REPLACE NumDif WITH NumDif - 1
					IF NumDif = 0
						REPLACE Estado WITH "10"
					ENDIF
				 ELSE
					DO STANDBY WITH "No se Encontr? Mausoleo"
					lElimina = .F.
				ENDIF
			CASE m.Tipent = '3'
				*
		ENDCASE
		
		IF lElimina
			SELE Difunto
			DELE NEXT 1
			SKIP -1
			IF BOF()
				GO TOP
			ENDIF
		ENDIF
	ENDIF
	
	IF lElimina
		SELE Traslado
		DELE NEXT 1
		SKIP -1
		IF BOF()
			GO TOP
		ENDIF
	ENDIF
ENDIF

SELE Traslado

DO VISTA
RETURN

*PROCEDURE lista
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

FUNCTION Nom_DifAn
*-----------------
PARAMETERS xcod,_Fil,_Col
PRIVATE cOrd

mAlias = ALIAS()
v_fun = .F.
v_Ent = .F.

SELE Difunto
GO TOP
cOrd = ORDER()
SET ORDER TO Difunto1

IF !EOF()
	IF SEEK(xcod)
		IF !EMPTY(_Fil) OR !EMPTY(_Col)
			@ _Fil, _Col SAY ALLTRIM(Difunto.ApeDif) +', '+ ALLTRIM(Difunto.NomDif)
		ENDIF
	ELSE
		@ _Fil, _Col SAY "Nombre No Existe. Revise..."
	ENDIF
ENDIF
SELE (mAlias)

RETURN

FUNCTION Val_DifAn
*-----------------
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PARAMETERS xcod,_Fil,_Col			&&,_tipo,_x,_y
PRIVATE cOrd

mAlias = ALIAS()
v_fun = .F.
v_Ent = .F.

SELE Difunto
GO TOP
cOrd = ORDER()
SET ORDER TO Difunto1

DO CASE
	CASE m.TipTras = '1' OR m.TipTras = '4'
		SET FILTER TO TipEnt = '3' AND Estado#'30'
	CASE m.TipTras = '2' OR m.TipTras = '3' OR m.TipTras = '5'
		SET FILTER TO TipEnt = '1' AND Estado#'30'
	CASE m.TipTras = '6'
		SET FILTER TO TipEnt = '2' AND Estado#'30'
ENDCASE

GO TOP

IF !EOF()
	IF !EMPTY(xcod)
		SEEK xcod
		v_Ent = FOUND()
		v_Fun  = .T.
	ENDIF
	
	IF !V_ENT
		SET ORDER TO Difunto2
		GO TOP
		ACTIVATE SCREEN
		ON KEY LABEL F10 KEYBOARD CHR(23)
		ON KEY LABEL F2 DO BusDif		&&BusDet
		
		DEFINE WINDOW _BusCli FROM 2,01 TO 22,78
		ACTIVATE WINDOWS _BusCli
		
		BROWSE WINDOW _BusCli TITLE '?????? [F10] Selecciona  ??  [F2] Buscar ??????' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		 FIELDS;
			ApeDif   :H='Apellidos',;
			NomDif   :H='Nombres'
		
		IF !Lastkey()=27
			v_ent = .T.
		ENDIF
		
		ON KEY LABEL F10
		ON KEY LABEL F2
		DEACTIVATE WINDOWS _BusCli
		RELEASE    WINDOWS _BusCli
	ENDIF
	
	IF V_ENT
		xCod		= CodDif
		m.fecFall   = FecFall
		m.FecSep	= FecSep
		m.ApeDif	= ApeDif
		m.NomDif	= NomDif
		m.SexoDif	= SexoDif
		m.EdadDif	= EdadDif
		m.CodCem	= CodCem
		m.CodCuar	= CodCuar
		m.codNic	= CodNic
		m.Fila		= Fila
		m.Columna	= Columna
		m.TipEnt	= TipEnt
		m.CodMau	= CodMau
		m.CodOcu	= CodOcu
		m.Reservado = Reservado
		
		m.CodCemAn	= m.CodCem
		m.CodCuarAn	= m.CodCuar
		m.codNicAn	= m.CodNic
		m.FilaAn	= m.Fila
		m.ColumnaAn	= m.Columna
		m.CodMauAn	= m.CodMau
		
		SHOW GETS
		v_fun = .T.
		
		IF !EMPTY(_Fil) OR !EMPTY(_Col)
			@ _Fil, _Col SAY m.NomDif
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

PROCEDURE BusDif
*---------------
PRIVATE vReg
ON KEY LABEL F2

DEFINE WINDOW wlista FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW wlisTA

vBusca  = SPACE(30)
vBusca1 = SPACE(30)

@ 2,3 SAY "Apellidos :" GET vBusca
@ 3,3 SAY "   Nombre :" GET vBusca1

READ VALID val_read()

DEACTIVATE WINDOW wlista

IF EMPTY(ALLTRIM(vBusca)+ALLTRIM(vBusca1)) OR LASTKEY()=27
	* RETURN
 ELSE
	SEEK ALLTRIM(UPPER(vBusca))+ALLTRIM(UPPER(vBusca1))
	IF !FOUND()
		DO standby WITH vmens04
		GOTO Reg
	ENDIF
ENDIF
ON KEY LABEL F2 do BusDif
RETURN

FUNCTION Val_CuarAn
*------------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Cuartel

IF SEEK(m.CodCemAn+m.codcuarAn)
	@ _Fil,_Col SAY NomCuar
	mRet = .T.
 ELSE
	m.CodCuarAn = SPACE(4)
	SET FILTER TO CodCem=m.codcemAn
	mRet = Val_Fun('Cuartel','CodCuar','NomCuar',m.codCuarAn,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet


FUNCTION Val_CuarNw
*------------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Cuartel

IF SEEK(m.CodCemNw+m.codcuarNw)
	@ _Fil,_Col SAY NomCuar
	mRet = .T.
 ELSE
	m.CodCuarNw = SPACE(4)
	SET FILTER TO CodCem=m.codcemNw
	mRet = Val_Fun('Cuartel','CodCuar','NomCuar',m.codCuarNw,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet


FUNCTION Val_MauAn
*-----------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Mausol

IF SEEK(m.codMauAn)
	@ _Fil,_Col SAY Familia
	mRet = .T.
 ELSE
	m.CodMauAn = SPACE(6)
	SET FILTER TO CodCem=m.codcemAn		&& AND TotDif>NumDif
	mRet = Val_Fun('Mausol','CodMau','Familia',m.CodMauAn,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet

FUNCTION Val_MauNw
*-----------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Mausol

IF SEEK(m.codMauNw)
	@ _Fil,_Col SAY Familia
	mRet = .T.
 ELSE
	m.CodMauNw = SPACE(6)
	SET FILTER TO CodCem=m.codcemNw		&& AND TotDif>NumDif
	mRet = Val_Fun('Mausol','CodMau','Familia',m.CodMauNw,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet

FUNCTION Val_dat
*---------------
PARAMETERS mOpc
IF mOpc = 1
	@ 11, 26 SAY IIF(!EMPTY(m.CodCemAn),Val_Fun('Cement','CodCem','NomCem',m.CodCemAn,1,11,26),SPACE(15))
	@ 12, 26 SAY IIF(!EMPTY(m.CodCuarAn),Val_CuarAn(12,26),SPACE(15))
	@ 12, 65 SAY IIF(!EMPTY(m.CodCuarAn),m.FilaAn+' '+m.ColumnaAn,SPACE(10)) COLOR SCHEME 5
	@ 13, 26 SAY IIF(!EMPTY(m.CodMauAn),Val_MauAn(13,26),SPACE(15))
 ELSE
	@ 16, 26 SAY IIF(!EMPTY(m.CodCemNw),Val_Fun('Cement','CodCem','NomCem',m.CodCemNw,1,16,26),SPACE(15))
	@ 17, 26 SAY IIF(!EMPTY(m.CodCuarNw),Val_CuarNw(17,26),SPACE(15))
	@ 17, 65 SAY IIF(!EMPTY(m.CodCuarNw),m.FilaNw+' '+m.ColumnaNw,SPACE(10)) COLOR SCHEME 5
	@ 18, 26 SAY IIF(!EMPTY(m.CodMauNw),Val_MauNw(18,26),SPACE(15))
ENDIF

RETURN .T.


FUNCTION Val_DifNw
*-----------------
PRIVATE mAlias,mRet

mAlias = ALIAS()

vDbf = SYS(3)+".Dbf"
CREATE TABLE &vDbf (CodCem C(2), CodCuar C(4), Fila1 C(1), Fila2 C(1), Col1 C(3), Col2 C(3),Est C(1))
USE &vDbf ALIAS Tempo EXCLUSIVE

SELE Difunto

DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
	TITLE "Ingresando Nuevo Difunto" COLOR SCHEME 5

ACTIVATE WIND w_Cli

m.codCem  = SPACE(2)
m.CodDif  = SPACE(8)
m.CodCuar = SPACE(4)
m.CodNic  = SPACE(6)
m.Fila    = SPACE(1)
m.Columna = SPACE(3)
m.Reservado = 'N'
m.TipEnt = SPACE(1)

@ 00,01 SAY "   Nic. res.? :" GET m.Reservado	WHEN WRes()		PICTURE "@M S,N"	VALID ValResNw()
@ 01,01 SAY "  Cementerios :" GET m.CodCem		WHEN WCem()     VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30) AND Cor_Dif()
@ 02,01 SAY " Nuevo Codigo :" GET m.CodDif		WHEN .F.
@ 03,01 SAY "Apell.Difunto :" GET m.Apedif		WHEN .F.		&&PICTURE "@S40"
@ 04,01 SAY " Nomb.Difunto :" GET m.Nomdif		WHEN .F.		&&PICTURE "@S40"
@ 05,01 SAY " Fecha Fallec.:" GET m.FecFall		WHEN .F.
@ 06,01 SAY "Tipo Entierro :" GET m.TipEnt		WHEN WTipE()	VALID VAL_PARA(m.TipEnt,'TIPENT',' ',17,15)	AND IniVar()
@ 09,01 SAY "      Cuartel :" GET m.CodCuar		WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_Cuar(9,31) AND Tab_Nic()
@ 10,01 SAY "         Fila :" GET m.Fila		WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_F()
@ 11,01 SAY "      Columna :" GET m.Columna		WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_C()
@ 12,01 SAY "     Mausoleo :" GET m.CodMau		WHEN m.TipEnt='2'  AND m.Reservado = 'N' VALID Val_Mau(12,34)
@ 13,01 SAY "      Cliente :" GET m.Codcli		WHEN .F.		&&VALID Val_Cli(m.CodCli,13,33)


READ VALID val_read()

DEACTIVATE WINDOW w_Cli
RELEASE WINDOW w_Cli

IF LASTKEY()#27
	m.CodDifNw	= m.CodDif
	m.CodCemNw	= m.CodCem
	m.CodCuarNw	= m.CodCuar
	m.CodNicNw	= m.CodNicNw
	m.FilaNw	= m.Fila
	m.ColumnaNw = m.Columna
	m.CodMauNw	= m.CodMau
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF

USE IN Tempo
ERASE &vDbf

SELE (mAlias)

RETURN mRet

PROCEDURE Act_NwCod
*------------------
* Actualiza el nuevo codigo del difunto
***************************************

PRIVATE mAct

mAct = .T.

IF LastKey()#27
	IF m.TipEnt="1"
		SELE Nichos
		***************
		*por verificar
		***************
		SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
		Campo = "Col"+SUBSTR(m.Codnic,2,3)+'E'
		IF &Campo = "2"
			IF m.Reservado='S'
				SELE OcuFut
				SEEK m.CodOcu
				IF FOUND()
					IF Estado # '20'
						IF f_Lock(1)
							REPLACE Estado WITH "20"
						ENDIF
					 ELSE
						DO StandBy WITH "Este Nicho ya est? Ocupado. Se cancelara el Ingreso "
						mAct = .F.
					ENDIF
				ENDIF
			 ELSE
				DO StandBy WITH "Existe un error en la reservaci?n. Se cancelara el Ingreso "
				mAct = .F.
			ENDIF
		ENDIF
		IF mAct
			SELE Nichos
			IF f_lock(1)
				REPLACE &Campo WITH "3"
			ENDIF
		ENDIF
	ENDIF
	
	IF m.TipEnt="2"
		SELE Mausol
		SEEK m.CodMau
		IF FOUND()
			IF Mausol.TotDif # Mausol.NumDif
				mND = Mausol.NumDif + 1
				IF f_lock(1)
					REPLACE Estado WITH "20",NumDif WITH mND
				ENDIF
			 ELSE
				DO StandBy WITH "El Difunto no Puede ser agregado por que el Mausoleo llego a su tope"
				mAct = .F.
			ENDIF
		 ELSE
			DO StandBy WITH "El Mausoleo no puede ser ubicado. No se actualizaran los cambios"
			mAct = .F.
		ENDIF
	ENDIF
	
	IF mAct
		SELE Difunto
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH '00',CodTras WITH SPACE(6),CodOcu WITH SPACE(8)
		ENDIF
		
		SELE Parma
		SEEK "CORRELDIFUNT"+m.codcem
		
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
		
	ENDIF
 ELSE
	DO StandBy WITH "No agrego ningun codigo "
ENDIF

RETURN mAct

FUNCTION WRes
*------------
IF BETWE(VAL(m.TipTras),2,3) OR (BETWE(VAL(m.TipTras),4,6) AND m.Local = 'S')
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF
RETURN mRet


FUNCTION WCem
*------------
PRIVATE mRet
DO CASE
	CASE (BETWE(VAL(m.TipTras),1,3) AND m.Local = 'S') AND m.Reservado = 'N'
		m.CodCem = m.CodCemAn
		mRet = .F.
	CASE (BETWE(VAL(m.TipTras),4,6) AND m.Local = 'S') AND m.Reservado = 'N'
		m.CodCem = SPACE(2)
		mRet = .T.
	OTHERWISE
		mRet = .F.
ENDCASE

IF !mRet
	SHOW GET m.CodCem
	@ 01,01 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30) AND Cor_Dif()
ENDIF

RETURN mRet

FUNCTION WTipE
*-------------

DO CASE
	CASE (BETWE(VAL(m.TipTras),1,3) AND m.Local = 'S') AND m.Reservado = 'N'
		IF m.TipTras = '3'
			m.TipEnt = '2'
		 ELSE
			m.TipEnt = '1'
		ENDIF
		mRet = .F.
	CASE (BETWE(VAL(m.TipTras),4,6) AND m.Local = 'S') AND m.Reservado = 'N'
		m.TipEnt = SPACE(1)
		mRet = .T.
	OTHERWISE
		mRet = .F.
ENDCASE

IF !mRet
	SHOW GET m.TipEnt
	@ 05,30 SAY VAL_PARA(m.TipEnt,'TIPENT',' ',17,15)	AND IniVar()
ENDIF

RETURN mRet


*FUNCTION ValResNw
*----------------
mret = .T.
IF m.Reservado = 'N'
	m.Estado = "00"
	
	mRet = .T.
 ELSE
	xx = EscojeOFNw()
	IF !xx
		mRet = .F.
	ENDIF
ENDIF
RETURN mRet

*PROCEDURE EscojeOFNw
*-------------------
PRIVATE cAlias,vFun
vFun = .F.
cAlias=ALIAS()
nReg = RECNO()

USE OcupFut  IN  0 ORDER TAG OcupFut1	ALIAS OcuFut

SELECT OcuFut

DEFINE POPUP pLista FROM 10,20 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Solicitante  '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Ocupante '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

IF LASTKEY()#27
	DO CASE
		CASE BAR() =  1
			SET ORDER TO TAG OcupFut3
		CASE BAR() =  2
			SET ORDER TO TAG OcupFut2
	ENDCASE
	
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5
	
	GO TOP
	
	IF EOF()
		DO StandBy WITH "No Existen Registros Para Procesar"
		vFun = .F.
	 ELSE
		SET FILTER TO ESTADO='10'
		GO TOP
		IF EOF()
			DO StandBy WITH "No Existen Ocupaciones Futuras"
			vFun = .F.
		 ELSE
			vTempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
			DO Logos WITH Rotulo1,vTempo
			ON KEY LABEL F10 KEYBOARD CHR(23)
			
			BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem   :H='Cementerio' :30,;
					OcuFut.CodCli   :H='Solicitante',;
					OcuFut.NomOcu   :H='Ocupante'
					
			vTempo = '????????????????????????????????????????????????????????????????????????????????'
			DO Logos WITH Rotulo1,vTempo
			IF LASTKEY()=27
				vFun = .F.
			 ELSE
		 		m.CodOCu	= OcuFut.CodOcu
				m.CodCem	= OcuFut.CodCem
				m.CodCuar	= OcuFut.CodCuar
				m.CodNic	= OcuFut.CodNic
				m.Fila		= OcuFut.Fila
				m.Columna	= OcuFut.Columna
				m.Estado	= "00"
				m.CodCli	= OcuFut.CodCli
				m.TipEnt	= "1"
				vFun = .T.
			ENDIF
		ENDIF
	ENDIF
ENDIF

*SHOW MENU mMenu
SET FILTER TO
SET ORDER TO &cOrd1
USE IN OcuFut
ON KEY LABEL F10
SELE (cAlias)
RETURN vFun

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0047","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0048","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0049","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0050","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0051","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0052","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0053","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0054","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
