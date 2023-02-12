*-----------------------------------------
* Traslado.Prg
* Registra Ingreso de Traslados de Difuntos
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

PUBLIC m.fecFall,m.FecSep,m.NomDif,m.SexoDif,m.EdadDif,m.CodCem
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
vmens07 = '¨ Desea Eliminar ‚ste Traslado ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Traslado ha sido anulado'
vmens10 = 'El Traslado ya est  Atendido'
vmens11 = 'El Traslado ha sido devuelto'

SELECT Traslado

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
@  1, 5 SAY "         Codigo :"
@  2, 5 SAY "        Cliente :"
@  3, 5 SAY " Fecha Traslado :"
@  4, 5 SAY "       Doc. Ref :"
@  5, 5 SAY "C¢digo Doc. Ref :"
@  6, 5 SAY " Fecha Doc. Ref.:"
@  7, 5 SAY " Tipo  Traslado :"
@  8, 5 SAY " Traslado local :"
@  9, 5 SAY "  Cod. Anterior :"

@ 11, 5 SAY "  Cem.     Ant. :"
@ 12, 5 SAY "  Cuartel  Ant. :"
@ 13, 5 SAY "  Mausoleo Ant. :"

@ 15, 5 SAY "     Cod. Nuevo :"
@ 16, 5 SAY "  Observaciones :"

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
@  2, 26 SAY m.CodCli
@  3, 26 SAY m.FecTras
@  4, 26 SAY m.DocRef
@  5, 26 SAY m.CodDocRf
@  6, 26 SAY m.FecDocRf
@  7, 26 SAY m.TipTras
@  8, 26 SAY m.Local
@  9, 26 SAY m.CodDifAn

@ 11, 26 SAY IIF(!EMPTY(m.CodCemAn),Val_Fun('Cement','CodCem','NomCem',m.CodCemAn,1,11,26),SPACE(15)) 
@ 12, 26 SAY IIF(!EMPTY(m.CodCuarAn),Val_CuarAn(12,26),SPACE(15))
@ 12, 65 SAY IIF(!EMPTY(m.CodCuarAn),m.FilaAn+' '+m.ColumnaAn,SPACE(10)) COLOR SCHEME 5
@ 13, 26 SAY IIF(!EMPTY(m.CodMauAn),Val_MauAn(13,26),SPACE(15))

@ 15, 26 SAY m.CodDifNw
@ 16, 26 SAY m.Observ

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Traslado
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
SET RELATION TO CodDifAn INTO Difunto
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Traslado.CodTras   :H='Difunto',;
	Difunto.Nomdif

SET RELATION TO
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Traslado
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

vBusca = SPACE(8)
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
@  1, 26 SAY m.CodTras
@  2, 26 GET m.CodCli
@  3, 26 GET m.FecTras
@  4, 26 GET m.DocRef
@  5, 26 GET m.CodDocRf
@  6, 26 GET m.FecDocRf
@  7, 26 GET m.TipTras
@  8, 26 GET m.Local
@  9, 26 GET m.CodDifAn
@ 14, 26 GET m.CodDifNw
@ 15, 26 GET m.Observ

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
SELECT Traslado
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Traslado
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

DO pantalla

           =Val_Tras()
m.FecTras  = DATE()
m.FecDocRf = DATE()

*@  0, 60 SAY EstTras()
@  1, 26 GET m.CodTras	WHEN .F.
@  2, 26 GET m.CodCli	VALID Val_Cli(m.CodCli,2,31)
@  3, 26 GET m.FecTras
@  4, 26 GET m.DocRef	VALID VAL_PARA(m.DocRef,'TIPDOC',' ',26,15)
@  5, 26 GET m.CodDocRf	
@  6, 26 GET m.FecDocRf
@  7, 26 GET m.Tiptras	VALID VAL_PARA(m.TipTras,'TIPTRS',' ',26,15)
@  8, 26 GET m.Local	WHEN Whe_Local() PICTURE "@M S,N"
@  9, 26 GET m.CodDifAn VALID Val_DifAn(m.CodDifAn,9,31) AND Val_dat()

@ 15, 26 GET m.CodDifNw	WHEN ((m.Tiptras = '1' OR m.Tiptras = '2' OR m.Tiptras = '3') OR local = 'S') VALID Val_DifNw()
@ 16, 26 GET m.Observ

READ VALID val_read()

susp

SELE Traslado

mAct = .T.

IF LASTKEY()#27
	
 ELSE
	IF EOF()
		GO BOTT
	 ELSE
		go vtemp
	ENDIF
ENDIF

*IF LASTKEY()#27
*	IF m.TipEnt="1"
*		SELE Nichos
*		***************
*		*por verificar
*		***************
*		SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
*		Campo = "Col"+SUBSTR(m.Codnic,2,3)+'E'
*		
*		IF &Campo = "2"
*			IF m.Reservado='S'
*				SELE OcuFut
*				SEEK m.CodOcu
*				IF FOUND()
*					IF Estado # '20'
*						IF f_Lock(1)
*							REPLACE Estado WITH "20"
*						ENDIF
*					 ELSE
*						DO StandBy WITH "Este Nicho ya est  Ocupado. Se cancelara el Ingreso "
*						mAct = .F.
*					ENDIF
*				ENDIF
*			 ELSE
*				DO StandBy WITH "Existe un error en la reservaci¢n. Se cancelara el Ingreso "
*				mAct = .F.
*			ENDIF
*		ENDIF
*		
*		IF mAct
*			SELE Nichos
*			IF f_lock(1)
*				REPLACE &Campo WITH "3"
*			ENDIF
*		ENDIF
*	ENDIF
*	
*	IF m.TipEnt="2"
*		SELE Mausol
*		SEEK m.CodMau
*		IF FOUND()
*			IF Mausol.TotDif # Mausol.NumDif
*				mND = Mausol.NumDif + 1
*				IF f_lock(1)
*					REPLACE Estado WITH "20",NumDif WITH mND
*				ENDIF
*			 ELSE
*				DO StandBy WITH "El Difunto no Puede ser agregado por que el Mausoleo llego a su tope"
*				mAct = .F.
*			ENDIF
*		 ELSE
*			DO StandBy WITH "El Mausoleo no puede ser ubicado. No se actualizaran los cambios"
*			mAct = .F.
*		ENDIF
*	ENDIF
*	
*	IF mAct
*		SELE Difunto
*		IF f_Appd()
*			GATHER MEMVAR
*		ENDIF
*		
*		SELE Parma
*		SEEK "CORRELDIFUNT"+m.codcem
*		
*		IF f_Lock(1)
*			REPLACE NumEnt WITH Parma.NumEnt+1
*		ENDIF
*		
*		UNLOCK ALL
*	ENDIF
* ELSE
*ENDIF

SELECT Traslado

DO vista
RETURN && 437


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
		DO standby WITH 'Ya esta Registrado este Cuartel'
		RETURN .F.
	ENDIF
ENDIF
RETURN .T.



























***************************************







FUNCTION IniVar
*--------------
DO CASE
	CASE m.TipEnt = "1"
		m.CodMau  = SPACE(6)
	CASE m.TipEnt = "2"
		m.CodCuar = SPACE(4)
		m.CodNic  = SPACE(4)
		m.Fila    = SPACE(1)
		m.Columna = SPACE(3)
	CASE m.TipEnt = "3"
		m.CodMau  = SPACE(6)
		m.CodCuar = SPACE(4)
		m.CodNic  = SPACE(4)
		m.Fila    = SPACE(1)
		m.Columna = SPACE(3)
ENDCASE

SHOW GETS

RETURN .T.


FUNCTION Val_Dif
*---------------
cAlias = ALIAS()
SELE &cAlias
mRet = .T.
IF SEEK("CORRELDIFUNT"+m.codcem,"Parma")
	m.codDif = m.CodCem + PADL(ALLTRIM(STR(Parma.NumEnt+1)),6,'0')
	SHOW GET m.CodDif
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodDif)
	DO StandBy WITH "El Codigo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodDif)
		DO standby WITH 'Ya esta Registrado este Difunto'
		mRet = .F.
	ENDIF
ENDIF
SELE (calias)
IF mRet
	RETURN .T.
 ELSE
	RETURN .F.
ENDIF


FUNCTION Val_Cuar
*---------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Cuartel

IF SEEK(m.CodCem+m.codcuar)
	@ _Fil,_Col SAY NomCuar
	mRet = .T.
 ELSE
	m.CodCuar = SPACE(4)
	SET FILTER TO CodCem=m.codcem
	mRet = Val_Fun('Cuartel','CodCuar','NomCuar',m.codCuar,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet

FUNCTION VV_Cuar
*---------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Cuartel

IF SEEK(mCodCem+mcodcuar)
	@ _Fil,_Col SAY NomCuar
	mRet = .T.
 ELSE
	m.CodCuar = SPACE(4)
	SET FILTER TO CodCem=mcodcem
	mRet = Val_Fun('Cuartel','CodCuar','NomCuar',mcodCuar,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet

FUNCTION Val_Mau
*---------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Mausol

IF SEEK(m.codMau)
	@ _Fil,_Col SAY Familia
	mRet = .T.
 ELSE
	m.CodMau = SPACE(6)
	SET FILTER TO CodCem=m.codcem
	mRet = Val_Fun('Mausol','CodMau','Familia',m.CodMau,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet


FUNCTION Tab_Nic
*---------------
SELE Nichos
SEEK m.CodCem+m.CodCuar
IF FOUND()
	Sele Tempo
	ZAP
	SELE Nichos
	SCAN While CodCem = m.CodCem AND CodCuar = m.CodCuar
		FOR I = 1 TO 100
			Campo1 = "Nichos.Col"+ PADL(ALLTRIM(STR(I)),3,'0')+'E'
			
			Campo2 = "Nichos.Col"+ PADL(ALLTRIM(STR(I)),3,'0')+'N'
			
			DO CASE
				CASE m.Reservado = 'N'
					IF &Campo1 = "1"
						SELE Tempo
						APPEND BLANK
						REPLACE CodCem WITH m.CodCem, CodCuar WITH m.CodCuar, Fila1 WITH Nichos.Fila1, Fila2 WITH Nichos.Fila2, Col1 WITH PADL(ALLTRIM(STR(i)),3,'0'), Col2 WITH PADL(ALLTRIM(STR(&Campo2)),3,'0')
					ENDIF
				CASE m.Reservado = 'S'
					IF &Campo1 = "2"
						SELE Tempo
						APPEND BLANK
						REPLACE CodCem WITH m.CodCem, CodCuar WITH m.CodCuar, Fila1 WITH Nichos.Fila1, Fila2 WITH Nichos.Fila2, Col1 WITH PADL(ALLTRIM(STR(i)),3,'0'), Col2 WITH PADL(ALLTRIM(STR(&Campo2)),3,'0')
					ENDIF
			ENDCASE
			SELE Nichos
		ENDFOR
	ENDSCAN
	
	SELE Tempo
	
	GO TOP
	IF EOF()
		DO StandBy WITH "Este cuartel no tiene nichos disponibles."
		m.codcuar = SPACE(4)
		RETURN .F.
	ENDIF
ENDIF
SELE Difunto
RETURN

FUNCTION Val_F
*--------------
cAlias = ALIAS()

vIdx = SYS(3)+'.IDX'
SELE Tempo
INDEX ON CodCem+CodCuar+Fila2 UNIQ TO vIdx FOR CodCem = m.CodCem AND CodCuar = m.CodCuar
GO TOP
IF !SEEK(m.CodCem+m.CodCuar+m.Fila)
	INDEX ON CodCem+CodCuar+Fila1 UNIQ TO vIdx FOR CodCem = m.CodCem AND CodCuar = m.CodCuar
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Fila2,1,10)
		ON SELECTION POPUP parametro DEACTIVATE POPUP
		ACTIVATE POPUP parametro
		RELEASE POPUP parametro
ENDIF

m.CodNic = Fila1
m.Fila   = Fila2
SHOW GET m.codnic
SHOW GET m.Fila


SET INDEX TO
ERASE (vIDX)

SELE (cAlias)

RETURN

FUNCTION Val_C
*-------------
cAlias = ALIAS()

vIdx = SYS(3)+'.IDX'
SELE Tempo
INDEX ON CodCem+CodCuar+Fila2+col2 TO vIdx FOR CodCem = m.CodCem AND CodCuar = m.CodCuar AND Fila2 = m.Fila
IF !SEEK(m.CodCem+m.CodCuar+m.Fila+m.Columna)
	INDEX ON CodCem+CodCuar+Fila1+col1 TO vIdx FOR CodCem = m.CodCem AND CodCuar = m.CodCuar AND Fila2 = m.Fila
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD Fila2+' '+Col2
		ON SELECTION POPUP parametro DEACTIVATE POPUP
		ACTIVATE POPUP parametro
		RELEASE POPUP parametro
ENDIF
	
m.CodNic = Fila1+Col1
m.Fila   = Fila2
m.Columna = Col2

SHOW GET m.codnic
SHOW GET m.Fila
SHOW GET m.Columna

SET INDEX TO
ERASE (vIDX)

SELE (cAlias)

RETURN

PROCEDURE Elimi
*--------------
PRIVATE lElimina

DO standBy WITH "Ojo. poner claves de Acceso, y revisar bien bien"

lElimina = .T.
*IF YesNo("Desea Eliminar este registro")
*	IF !ve_passw("ELIDIF")
*		DO STANDBY WITH "Proceso Cancelado"
*		lElimina = .F.
*	ENDIF
* ELSE
*	DO STANDBY WITH "Proceso Cancelado"
*	lElimina = .F.
*ENDIF

IF lElimina
	DO CASE
		CASE m.Tipent = '1'
			IF !EMPTY(m.Fila) and !EMPTY(m.Columna)
				fCampo = "Col"+PADL(SUBSTR(m.CodNic,2,3),3,'0')+'E'
				
				IF !EMPTY(m.CodOcu)
					SELE OcuFut
					SEEK m.CodOcu
					REPLACE ESTADO WITH "10"
					
					SELE Nichos
					SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
					REPLACE &fCampo WITH "2"
				 ELSE
					SELE Nichos
					SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
					REPLACE &fCampo WITH "1"
				ENDIF
				
			 ELSE
				DO STANDBY WITH "Error en nicho no se puede eliminar"
				lElimina = .F.
			ENDIF
		CASE m.Tipent = '2'
			SELE Mausol
			IF SEEK(m.CodMau)
				REPLACE NumDif WITH NumDif - 1
				IF NumDif = 0
					REPLACE Estado WITH "10"
				ENDIF
			 ELSE
				DO STANDBY WITH "No se Encontr¢ Mausoleo"
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

DO VISTA
RETURN

PROCEDURE lista
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

FUNCTION VV_Cem
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

FUNCTION VW_Cuar
*---------------
cAlias = ALIAS()
SELE Cuartel
IF vToCem#1
	SET FILTER TO CodCem=mCodCem
 ELSE
	SET FILTER TO
ENDIF
SELE (cAlias)
RETURN

FUNCTION VV_Cuar
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

FUNCTION VW_Fila
*---------------
cAlias = ALIAS()
SELE Nichos
IF vToCem#1
	IF vToCuar#1
		SET FILTER TO CodCem=mCodCem AND CodCuar=mCodCuar
	ENDIF
ENDIF
SELE (cAlias)
RETURN

FUNCTION VV_Fila
*---------------
PARAMETERS wFila,wColumna
PRIVATE cOrd

cAlias = ALIAS()
SELE Nichos
cOrd   = ORDER()

INDEX ON CodCem+CodCuar+Fila2 UNIQ TO vIdx FOR CodCem = mCodCem AND CodCuar = mCodCuar
lBusca = .F.

IF vToCem#1
	IF vToCuar#1
		IF vToFila#1
			IF !EMPTY(mFila)
				IF !SEEK(mCodCem+mCodCuar+mFila)
					lBusca = .T.
				ENDIF
			 ELSE
				lBusca = .T.
			ENDIF
		ENDIF
	ENDIF
ENDIF



IF lBusca
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD Nichos.Fila2		&& &v__bus
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
ENDIF
mFila = Nichos.Fila2

@ wFila,wColumna SAY mFila

*SET FILTER TO
SET ORDER TO (cOrd)
SELE (cAlias)

RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
ERASE &vDbf
RELEASE WINDOW wind_0
RELEASE MENU   mmenu
*RESTORE SCREEN FROM principal
RETURN

PROCEDURE EscojeOF
*-------------------
PRIVATE cAlias,vFun
vFun = .F.
cAlias=ALIAS()
nReg = RECNO()

SELECT OcuFut

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Solicitante  '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Ocupante '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		SET ORDER TO TAG OcupFut3
	CASE BAR() =  2
		SET ORDER TO TAG OcupFut2
ENDCASE

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

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
		vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		DO Logos WITH Rotulo1,vTempo
		ON KEY LABEL F10 KEYBOARD CHR(23)
		
		BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
				Cement.NomCem   :H='Cementerio' :30,;
				OcuFut.CodCli   :H='Solicitante',;
				OcuFut.NomOcu   :H='Ocupante'
				
		vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
		DO Logos WITH Rotulo1,vTempo
		IF LASTKEY()=27
			vFun = .F.
		 ELSE
		 	m.CodOCu	= OcuFut.CodOcu
			m.FecFall	= DATE()
			m.FecSep	= DATE()
			m.NomDif	= OcuFut.NomOcu
			m.SexoDif   = OcuFut.SexoOcu
			m.EdadDif   = OcuFut.EdadOcu
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
*SHOW MENU mMenu
SET FILTER TO
SET ORDER TO &cOrd1
ON KEY LABEL F10
SELE (cAlias)
RETURN vFun


*FUNCTION Val_Lot
*---------------
** _tipo = .F. ---> Campo
**         .T. ---> Variable.

PARAMETERS xcod
PRIVATE cOrd,malias
ON KEY LABEL F5  
ON KEY LABEL F8  

mAlias = ALIAS()

v_fun = .F.
v_Ent = .F.

SELE Mausoleo
cOrd = ORDER()
SET ORDER TO Mausoleo1

cFilt = FILTER()
SET FILTER TO CodCem = m.CodCem
GO TOP
IF !EMPTY(xcod)
	SEEK m.CodCem+xcod
	v_fun = IIF(FOUND(),Familia,"")
	v_Ent = FOUND()
ENDIF

IF EMPTY(xcod) OR !V_ENT
	GO TOP
	ACTIVATE SCREEN
	ON KEY LABEL F10 KEYBOARD CHR(23)
	DEFINE WINDOW _BusMau FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusMau
	BROWSE WINDOW _BusMau TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²   ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		FIELDS;
			Familia	:H='Familia o Congregaci¢n'
	ON KEY LABEL F10
	DEACTIVATE WINDOWS _BusMau
	RELEASE    WINDOW _BusMau
	
	IF Lastkey()=27
		V_FUN = .f.
	 ELSE
		xCod     = CodMau
		v_fun = Familia
	ENDIF
 ELSE
	V_FUN = Familia
ENDIF

ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SET FILTER TO &cFilt
SET ORDER TO &cOrd

SELECT (mAlias)

RETURN v_FUN

RETURN


*FUNCTION Val_FunNi
*-----------------
** _tipo = .F. ---> Campo
**         .T. ---> Variable.

PARAMETERS xcod,xOrd,xLista,xDeb,xFil,xCol
PRIVATE mAlias,vKeyf2,vKeyf5,vKeyf8,vKeyf10,mOrd

vKeyf2  = ON("KEY","f2")
vKeyf5  = ON("KEY","f5")
vKeyf8  = ON("KEY","f8")
vKeyf10 = ON("KEY","f10")

mAlias = ALIAS()
v_fun = .F.
v_Ent = .F.

SELE Cuartel
GO TOP

IF !EMPTY(xDeb)
	SEEK(m.CodCem+xDeb)
	v_fun = FOUND()
	v_Ent = FOUND()
ENDIF

IF EMPTY(xcod) OR !V_ENT
	GO TOP
	ACTIVATE SCREEN
	ON KEY LABEL F2
	ON KEY LABEL F5
	ON KEY LABEL F8
	ON KEY LABEL F10 KEYBOARD CHR(23)
	mOrd = ORDER()
	SET ORDER TO &xOrd
	DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusArt
	BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega Art. ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		FIELDS &xLista
	
	ON KEY LABEL F10
	ON KEY LABEL F2
	ON KEY LABEL F5
	DEACTIVATE WINDOW _BusArt
	RELEASE    WINDOW _BusArt
	
	SET ORDER TO &mOrd
	
	IF Lastkey()=27
		V_FUN = .f.
	 ELSE
		xDeb = &xCod
		v_Fun = .T.
		@ xFil,xCol SAY &xLista
	ENDIF
 ELSE
	@ xFil,xCol SAY &xLista
ENDIF

ON KEY LABEL F2  &vKeyf2
ON KEY LABEL F5  &vKeyf5
ON KEY LABEL F8  &vKeyf8
ON KEY LABEL F10 &vKeyf10

SELECT (mAlias)

RETURN v_FUN

FUNCTION Val_Mausol
*-----------------
** _tipo = .F. ---> Campo
**         .T. ---> Variable.

PARAMETERS xcod,xOrd,xLista,xDeb,xFil,xCol
PRIVATE mAlias,vKeyf2,vKeyf5,vKeyf8,vKeyf10,mOrd

vKeyf2  = ON("KEY","f2")
vKeyf5  = ON("KEY","f5")
vKeyf8  = ON("KEY","f8")
vKeyf10 = ON("KEY","f10")

mAlias = ALIAS()
v_fun = .F.
v_Ent = .F.

SELE Mausoleo
GO TOP

IF !EMPTY(xDeb)
	SEEK(m.CodCem+xDeb)
	v_fun = FOUND()
	v_Ent = FOUND()
ENDIF

IF EMPTY(xcod) OR !V_ENT
	GO TOP
	ACTIVATE SCREEN
	ON KEY LABEL F2
	ON KEY LABEL F5
	ON KEY LABEL F8
	ON KEY LABEL F10 KEYBOARD CHR(23)
	mOrd = ORDER()
	SET ORDER TO &xOrd
	DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusArt
	BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega Art. ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		FIELDS &xLista
	
	ON KEY LABEL F10
	ON KEY LABEL F2
	ON KEY LABEL F5
	DEACTIVATE WINDOW _BusArt
	RELEASE    WINDOW _BusArt
	
	SET ORDER TO &mOrd
	
	IF Lastkey()=27
		V_FUN = .f.
	 ELSE
		xDeb = &xCod
		v_Fun = .T.
		@ xFil,xCol SAY &xLista
	ENDIF
 ELSE
	@ xFil,xCol SAY &xLista
ENDIF

ON KEY LABEL F2  &vKeyf2
ON KEY LABEL F5  &vKeyf5
ON KEY LABEL F8  &vKeyf8
ON KEY LABEL F10 &vKeyf10

SELECT (mAlias)

RETURN v_FUN

*FUNCTION Nom_Cuar
*----------------
cAlias = ALIAS()
SELE Cuartel
cOrd = ORDER()
SET ORDER TO Cuartel1
SEEK m.CodCem+m.CodCuar
IF FOUND()
	cNomCuar = Cuartel.NomCuar
	@ 13,31 SAY cNomCuar
ENDIF
SET ORDER TO cOrd
SELE &cAlias
RETURN

FUNCTION EstDif
*--------------
IF m.Estado ='10'
	@ ROW(),COL() SAY SPACE(15)
ENDIF
IF m.Estado ='10'
	@ ROW(),COL() SAY "TRASLADADO" COLOR SCHEME 5
ENDIF
RETURN

****************************


FUNCTION Val_DifAn
*-----------------

* AQUI ME QUEDO

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
		SET FILTER TO TipEnt = '3'
	CASE m.TipTras = '2' OR m.TipTras = '3' OR m.TipTras = '5'
		SET FILTER TO TipEnt = '1'
	CASE m.TipTras = '6'
		SET FILTER TO TipEnt = '2'
ENDCASE

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
	
	IF !EOF()
		BROWSE WINDOW _BusCli TITLE '²²²²²² [F10] Selecciona  ²²  [F2] Buscar ²²²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		 FIELDS;
			NomDif   :H='Difunto'
			
		IF !Lastkey()=27
			v_ent = .T.
		ENDIF
	 ELSE
		DO StandBy WITH "No existen registros para procesar"
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

SET FILTER TO

SET ORDER TO &cOrd

ON KEY LABEL F8
ON KEY LABEL F10
UNLOCK ALL

SELECT (mAlias)

RETURN v_Fun


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

FUNCTION Val_dat
*---------------

@ 11, 26 SAY IIF(!EMPTY(m.CodCemAn),Val_Fun('Cement','CodCem','NomCem',m.CodCemAn,1,11,26),SPACE(15)) 
@ 12, 26 SAY IIF(!EMPTY(m.CodCuarAn),Val_CuarAn(12,26),SPACE(15))
@ 12, 65 SAY IIF(!EMPTY(m.CodCuarAn),m.FilaAn+' '+m.ColumnaAn,SPACE(10)) COLOR SCHEME 5
@ 13, 26 SAY IIF(!EMPTY(m.CodMauAn),Val_MauAn(13,26),SPACE(15))

RETURN .T.


FUNCTION Val_DifNw
*-----------------
*aqui me quedo
susp
PRIVATE mAlias
mAlias = ALIAS()
SELE Difunto

DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
	TITLE "Ingresando Articulo" COLOR SCHEME 5

ACTIVATE WIND w_Cli

m.codCem  = SPACE(2)
m.CodDif  = SPACE(8)
m.CodCuar = SPACE(4)
m.CodNic  = SPACE(6)
m.Fila    = SPACE(1)
m.Columna = SPACE(3)

lValCem = .T.

DO CASE
	CASE m.TipTras = '1' OR m.TipTras = '2' OR m.TipTras = '3'
*	WAIT WIND M.CODCEM
		m.codCem  = m.CodCemAn
		=Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30) AND Val_Dif()
		SHOW GET m.CodCem
		SHOW GET m.CodDif
		lValCem = .F.
ENDCASE

@ 01,01 SAY "  Cementerios :" GET m.CodCem	WHEN lValCem VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30) AND Val_Dif()
@ 02,01 SAY " Nuevo Codigo :" GET m.CodDif	WHEN .F.
@ 03,01 SAY "      Difunto :" GET m.Nomdif
@ 04,01 SAY " Fecha Fallec.:" GET m.FecFall	WHEN .F.
@ 05,01 SAY "Tipo Entierro :" GET m.TipEnt	VALID VAL_PARA(m.TipEnt,'TIPENT',' ',26,15)	AND IniVar()
@ 06,01 SAY "      Cuartel :" GET m.CodCuar	WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_Cuar(9,31) AND Tab_Nic()
@ 07,01 SAY "         Fila :" GET m.Fila	WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_F()
@ 08,01 SAY "      Columna :" GET m.Columna	WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_C()
@ 09,01 SAY "     Mausoleo :" GET m.CodMau	WHEN m.TipEnt='2'  AND m.Reservado = 'N' VALID Val_Mau(13,34)

@ 10,01 SAY "      Cliente :" GET m.Codcli


READ VALID val_read()

DEACTIVATE WINDOW w_Cli
RELEASE WINDOW w_Cli

IF LastKey()#27
	IF f_Appd()
		GATHER MEMVAR
		SELE Parma
		SEEK "CORRELCLIENT"
		REPLACE NumEnt WITH Parma.NumEnt+1
	ENDIF
ENDIF

SELE (mAlias)

RETURN

