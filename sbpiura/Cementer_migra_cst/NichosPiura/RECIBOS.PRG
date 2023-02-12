*-----------------------------------------
* Recibos.Prg
* Registra Ingreso de Difuntos
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

PUBLIC m.TipEnt,m.codMau,m.FecFall,m.Fecsep,m.NomDif,m.SexoDif,m.EdadDif
PUBLIC m.Coddif,m.codCem,m.CodCuar,m.codNic,m.Fila,m.columna,m.Codcli,m.CodOcu
PUBLIC m.Reservado,m.EstVta

CLOS DATA
USE Recibos  IN  1 ORDER TAG Recibos1	ALIAS Recibos
USE Difunto  IN  2 ORDER TAG Difunto1	ALIAS Difunto
USE Cuartel  IN  3 ORDER TAG Cuartel1	ALIAS Cuartel
USE Cement   IN  4 ORDER TAG Cement1	ALIAS Cement
USE Nichos	 IN  5 ORDER TAG Nichos1	ALIAS Nichos
USE Parmae   IN  6 ORDER TAG Parmae1	ALIAS Parma
USE OcupFut  IN  7 ORDER TAG OcupFut1	ALIAS OcuFut
USE Mausoleo IN  8 ORDER TAG Mausoleo1	ALIAS Mausol
USE Clientes IN  9 ORDER TAG Clientes1	ALIAS Clien
USE Caja     IN 10 ORDER TAG Caja1		ALIAS Caja

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Recibos'
vmens02 = ' Recibos : REVISION '
vmens04 = 'Dicho Recibo no fue encontrado'
vmens05 = 'No existe Recibo anterior'
vmens06 = 'No existe Recibo siguiente'
vmens07 = '¨ Desea Eliminar ‚ste Recibo ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Recibo ha sido anulado'
vmens10 = 'El Recibo ya est  Atendido'
vmens11 = 'El Recibo ha sido devuelto'

SELECT Recibos
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula    Listar  Termina '
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
DEFINE PAD elimi   OF mmenu PROMPT '\<Anula  '    AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
ON SELECTION PAD elimi  OF mmenu DO anula
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi
RETURN

PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

@ 00, 50 SAY " Estado :"
@ 01, 5 SAY "       Codigo :"
@ 02, 5 SAY "        Fecha :"
@ 03, 5 SAY "  Tipo Recibo :"
@ 04, 5 SAY "      Difunto :"
@ 05, 5 SAY "      Cliente :"
@ 06, 5 SAY "     Traslado :"
@ 07, 5 SAY "Transferencia :"
@ 08, 5 SAY "     Concepto :"
@ 09, 5 SAY "      Importe :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Recibos

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

@  0, 60 SAY EstRec()
@  1, 26 GET m.CodRec	WHEN .F.
@  2, 26 GET m.FecRec
@  3, 26 GET m.TipRec	VALID VAL_PARA(m.TipRec,'TIPREC',' ',26,15)
@  4, 26 GET m.CodDif
@  5, 26 GET m.CodCli	VALID Val_Cli(m.CodCli,4,31)
@  6, 26 GET m.CodTras
@  7, 26 GET m.CodTrn
@  8, 26 GET m.ConCepto	FUNCTION 'S30'
@  9, 26 GET m.Importe

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Recibos
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
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	CodRec    :H='Cementerio' :30,;
	CodCli    :H='C¢digo',;
	Importe   :H='Difunto'

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Difunto
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()

vCodigo = SPACE(7)

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA
@ 3,2 SAY "Codigo : " GET vCodigo

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vCodigo) OR LASTKEY()=27
	* RETURN
 ELSE
	SEEK ALLTRIM(vCodigo)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ENDIF
ENDIF
DO vista
RETURN

PROCEDURE anter
*--------------
SELE Recibos
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
SELE Recibos
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

*PROCEDURE corri
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SELECT Difunto
SCATTER MEMVAR
DO pantalla

@  0, 60 SAY VAL_PARA(m.Estado,'ESTDIF',' ',60,20)		&&m.Estado
@  1, 26 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30)
@  2, 26 GET m.CodDif	DISABLE
@  3, 26 GET m.NomDif	WHEN IIF(m.Reservado='S',.F.,.T.)	FUNCTION 'S30'
@  4, 26 GET m.SexoDif	FUNCTION "M M,F" VALID VAL_PARA(m.SexoDif,'SEXO  ',' ',26,20)
@  5, 26 GET m.EdadDif
@  6, 26 GET m.FecFall
@  7, 26 GET m.FecSep
@  8, 26 GET m.TipEnt	VALID VAL_PARA(m.TipEnt,'TIPENT',' ',26,15) DISABLE
@  9, 26 GET m.CodCuar	WHEN m.TipEnt='2'		DISABLE
@ 10, 26 GET m.CodNic   WHEN m.TipEnt='1'		DISABLE
@ 11, 26 GET m.Fila     WHEN m.TipEnt='1'		DISABLE
@ 12, 26 GET m.Columna  WHEN m.TipEnt='1'		DISABLE
*@ 13, 26 GET m.CodMau	WHEN m.TipEnt='2'  AND m.Reservado = 'N' VALID Val_Mau(13,34)
@ 13, 26 GET m.CodMau	WHEN .F.
@ 14, 26 GET m.CodCli	VALID IIF(EMPTY(m.Codcli),.T.,Val_Cli(m.CodCli,14,31))


READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Difunto
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT Recibos
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Recibos
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

DO pantalla

m.FecRec = DATE()
********************
m.TipEnt  = SPACE(1)
m.codMau  = SPACE(6)
m.FecFall = {  /  /  }
m.Fecsep  = {  /  /  }
m.NomDif  = SPACE(30)
m.SexoDif = SPACE(1)
m.EdadDif = 0
*m.Coddif = SPACE(8)
m.codCem  = SPACE(2)
m.CodCuar = SPACE(4)
m.codNic  = SPACE(4)
m.Fila    = SPACE(1)
m.columna = SPACE(3)
m.Codcli  = SPACE(6)
m.CodOcu  = SPACE(8)
m.Reservado = SPACE(1)
m.EstVta  = SPACE(2)
********************

=Cor_Rec()

@  0, 60 SAY EstRec()
@  1, 26 GET m.CodRec	WHEN .F.
@  2, 26 GET m.FecRec	WHEN .F.		&&VALID Val_Fecha()
@  3, 26 GET m.TipRec	VALID VAL_PARA(m.TipRec,'TIPREC',' ',26,15)
@  4, 26 GET m.CodDif	WHEN wCodDif()	VALID Val_DifR(4,35)
@  5, 26 GET m.CodCli	VALID Val_Cli(m.CodCli,5,35)
@  6, 26 GET m.CodTras	WHEN .F.
@  7, 26 GET m.CodTrn	WHEN .F.
@  8, 26 GET m.Concepto	FUNCTION 'S30'
@  9, 26 GET m.Importe

READ VALID val_read()

SELE Recibos

mAct = .T.

set step on

IF LASTKEY()#27
	
	IF m.TipRec = '1' OR TipRec = '2'
		DO CASE
		CASE m.TipEnt="1"
			SELE Nichos
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
							DO StandBy WITH "Este Nicho ya est  Ocupado. Se cancelara el Ingreso "
							mAct = .F.
						ENDIF
					ENDIF
				 ELSE
					DO StandBy WITH "Existe un error en la reservaci¢n. Se cancelara el Ingreso "
					mAct = .F.
				ENDIF
			ENDIF
			IF mAct
				SELE Nichos
				IF f_lock(1)
					REPLACE &Campo WITH "3"
				ENDIF
			ENDIF
		
		CASE m.TipEnt="2"
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
		ENDCASE
		
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
	ENDIF
	
	* Actualizar Caja
	
	SELE Caja
	m.TipCaj  = '3'
	m.FecReg  = m.FecRec
	m.CorIng  = m.CodRec
	*m.CodCli 
*	m.Importe = m.Importe
	
	IF f_Appd()
		GATHER MEMVAR
		REPLACE Estado WITH '00'
		UNLOCK
	ENDIF
	
	
	IF mAct
		SELE Recibos
		IF f_Appd()
			GATHER MEMVAR
		ENDIF
		
		* Parmae
		cMes = PADL(ALLTRIM(STR(MONT(m.FecRec))),2,'0')
		cAno = RIGHT(STR(YEAR(m.FecRec),4),2)
		
		IF !SEEK("RECIBO"+LEFT(m.CodRec,4),"Parma")
			SELE Parma
			IF f_Appd()
				REPLACE Tipo WITH 'RECIBO', Codigo WITH cAno, CodigoAux WITH cMes, Descri WITH "CORRELATIVO DE RECIBOS: " + cMes + '/' + cAno
			ENDIF
		ENDIF
		
		SELE Parma
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
			UNLOCK
		ENDIF
		
		UNLOCK ALL
	ENDIF
 ELSE
	IF EOF()
		GO BOTT
	 ELSE
		go vtemp
	ENDIF
ENDIF

SELECT Recibos

FLUSH

DO vista
RETURN


FUNCTION wCodDif
*---------------
PRIVATE mRet
DO CASE
	CASE m.TipRec = '1' OR m.TipRec = '2'
		mRet = .T.
	OTHERWISE
		mRet = .F.
ENDCASE

RETURN mRet


FUNCTION Val_Fecha
*-----------------
PRIVATE mRet
IF PADL(ALLTRIM(STR(MONTH(m.FecRec))),2,'0') = SUBSTR(m.CodRec,3,2)
	mRet = .T.
 ELSE
	DO StandBy WITH 'La fecha no corresponde al correlativo generado'
	mRet = .F.
ENDIF
RETURN mRet

******

FUNCTION Val_DifR
*----------------
PARAMETERS cFil,cCol

PRIVATE cAlias
vDbf = SYS(3)+".Dbf"
cAlias = ALIAS()
CREATE TABLE &vDbf (CodCem C(2), CodCuar C(4), Fila1 C(1), Fila2 C(1), Col1 C(3), Col2 C(3))
USE &vDbf ALIAS Tempo EXCLUSIVE

SELE Difunto

mRet = .T.
mEnc = .F.

IF SEEK(m.CodDif,'Difunto')
*	@ cFil,cCol SAY Difunto.NomDif
	mEnc = .T.
 ELSE
	DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
		TITLE "Ingresando Difunto" COLOR SCHEME 5
	
	ACTIVATE WIND w_Cli
	
	*m.codCem  = SPACE(2)
	*m.CodDif  = SPACE(8)
	*m.NomDif  = SPACE(50)
	m.FecFall = DATE()
	*m.CodCuar = SPACE(4)
	*m.CodNic  = SPACE(6)
	*m.Fila    = SPACE(1)
	*m.Columna = SPACE(3)
	*m.CodMau  = SPACE(6)
	m.Reservado = 'N'
	*m.TipEnt = SPACE(1)			&& IIF(m.TipRI='1','3',SPACE(1)) 
	
	wReser    = IIF(m.TipRec='1',.T.,.F.)
	
	@ 00,01 SAY "   Nic. res.? :" GET m.Reservado	WHEN wReser 	   PICTURE "@M S,N"	VALID ValResR()
	@ 01,01 SAY "  Cementerios :" GET m.CodCem		WHEN WCemR()       VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30) AND Cor_Dif()
	@ 02,01 SAY "       Codigo :" GET m.CodDif		WHEN .F.
	@ 03,01 SAY "      Difunto :" GET m.Nomdif		WHEN wNomDif()		   PICTURE "@S40"
	@ 04,01 SAY " Fecha Fallec.:" GET m.FecFall		
	@ 05,01 SAY "Tipo Entierro :" GET m.TipEnt		WHEN WTipER() 	   VALID VAL_PARA(m.TipEnt,'TIPENT',' ',17,15)	AND IniVar()
	@ 08,01 SAY "      Cuartel :" GET m.CodCuar		WHEN wCodcuar()    VALID Val_Cuar(8,31) AND Tab_Nic()
	@ 09,01 SAY "         Fila :" GET m.Fila		WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_F()
	@ 10,01 SAY "      Columna :" GET m.Columna		WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_C()
	@ 11,01 SAY "     Mausoleo :" GET m.CodMau		WHEN m.TipEnt='2'  AND m.Reservado = 'N' VALID Val_Mau(11,34)
	@ 12,01 SAY "      Cliente :" GET m.Codcli		WHEN .F.		&&VALID Val_Cli(m.CodCli,12,33)
	
	READ VALID val_read()
	
	DEACTIVATE WINDOW w_Cli
	RELEASE WINDOW w_Cli
	
	mAct = .T.
	
	IF LASTKEY()#27
		mRet = .T.
	 ELSE
		mRet = .F.
	ENDIF
	
	USE IN Tempo
	ERASE &vDbf
	
	SELE (cAlias)
ENDIF

DO CASE
	CASE mEnc
		@ cFil,cCol SAY ALLTRIM(Difunto.NomDif)
	CASE mRet
		@ cFil,cCol SAY ALLTRIM(m.NomDif)
ENDCASE


RETURN mRet

FUNCTION wCodCuar
*----------------
IF m.TipEnt='1'  AND m.Reservado = 'N'
	mRet = .F.
 ELSE
	SHOW GET m.CodCuar
	@ 08,31 SAY Val_Cuar(8,31)
	mRet = .F.
ENDIF

RETURN mRet

FUNCTION ValResR
*---------------
PRIVATE mRet
mRet = .T.
IF m.Reservado = 'N'
	m.Estado = "00"
	mRet = .T.
 ELSE
	xx = EscojeOFR()
	IF !xx
		mRet = .F.
	ENDIF
ENDIF
RETURN mRet

PROCEDURE EscojeOFR
*------------------
PRIVATE cAlias,vFun
vFun = .F.
cAlias=ALIAS()
nReg = RECNO()

*USE OcupFut  IN  0 ORDER TAG OcupFut1	ALIAS OcuFut

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
			
			SET RELATION TO Codcem INTO Cement
			
			BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem   :H='Cementerio' :30,;
					OcuFut.CodCli   :H='Solicitante',;
					OcuFut.NomOcu   :H='Ocupante'
			
			SET RELATION TO
					
			vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
			DO Logos WITH Rotulo1,vTempo
			IF LASTKEY()=27
				vFun = .F.
			 ELSE
		 		m.CodOCu	= OcuFut.CodOcu
		 		m.NomDif    = OcuFut.NomOcu
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
*SHOW GETS
SET FILTER TO
SET ORDER TO &cOrd1
*USE IN OcuFut
ON KEY LABEL F10
SELE (cAlias)
RETURN vFun

FUNCTION wNomDif
*---------------
PRIVATE mRet
DO CASE
	CASE m.Reservado = 'S'
		SHOW GET m.NomDif
		mRet = .F.
	OTHERWISE
		mRet = .T.
ENDCASE

RETURN mRet


FUNCTION WCemR
*-------------
PRIVATE mRet
DO CASE
	CASE m.Reservado = 'N'
		m.CodCem = SPACE(2)
		mRet = .T.
	OTHERWISE
		mRet = .F.
ENDCASE

IF !mRet
	SHOW GET m.CodCem
	@ 01,01 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30)	AND Cor_Dif()
ENDIF

RETURN mRet


FUNCTION WTipER
*--------------

DO CASE
	CASE m.TipRec = '1'
		IF m.reservado = 'S'
			m.TipEnt = '1'
		 ELSE
			m.TipEnt = '2'
		ENDIF
		mRet = .F.
	CASE m.TipRec = '2'
		m.TipEnt = '3'
		mRet = .F.
	OTHERWISE
		mRet = .F.
ENDCASE

IF !mRet
	SHOW GET m.TipEnt
	@ 05,30 SAY VAL_PARA(m.TipEnt,'TIPENT',' ',17,15)	AND IniVar()
ENDIF

RETURN mRet

******



**FUNCTION Val_Dif
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


*FUNCTION Val_Cuar
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

*FUNCTION VV_Cuar
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

**FUNCTION Val_Mau
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
	SET FILTER TO CodCem=m.codcem AND TotDif>NumDif
	mRet = Val_Fun('Mausol','CodMau','Familia',m.CodMau,1,_Fil,_Col)
	SET FILTER TO
ENDIF

SELE (cAli)

RETURN mRet


**FUNCTION Tab_Nic
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

**FUNCTION Val_F
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

**FUNCTION Val_C
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

*PROCEDURE Elimi
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

*PROCEDURE lista
*--------------
PRIVATE nReg,vIdx
vIdx = SYS(3)+'.Idx'
SELE Difunto 

IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
nReg = RECNO()

INDEX ON TipEnt+CodCem+CodCuar+CodNic TO (vIdx)

SELE Cuartel
SET ORDER TO Cuartel1
SELE Difunto
SET RELATION TO CodCem		   INTO Cement  ADDITIVE
SET RELATION TO CodCem+CodCuar INTO Cuartel ADDITIVE

DEFINE WINDOW LIS FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Difuntos' COLOR SCHEME 5
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
	SET INDEX TO
	SET ORDER TO Difunto1
	ERASE (vIdx)
ENDIF

SELE Cuartel
SET ORDER TO Cuartel1
SELE Difunto
GO nReg
RETURN

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
 ELSE
	SET FILTER TO
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

*FUNCTION VW_Fila
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

*FUNCTION VV_Fila
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

*PROCEDURE EscojeOF
*-------------------
PRIVATE cAlias,vFun
vFun = .F.
cAlias=ALIAS()
nReg = RECNO()

SELECT OcuFut

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Ocupante '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Solicitante  '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		SET ORDER TO TAG OcupFut2
		xOpc = 1
	CASE BAR() =  2
		SET ORDER TO TAG OcupFut3
		xOpc = 2
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
		
		SET RELATION TO CodCem INTO Cement
		SET RELATION TO CodCli INTO Clien	ADDITIVE
		
		IF xOpc = 1
			BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem   :H='Cementerio' :30,;
					OcuFut.NomOcu   :H='Ocupante'
		 ELSE
			BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem   :H='Cementerio' :30 ,;
					Clien.Nomcli    :H='Solicitante':30 ,;
					OcuFut.NomOcu   :H='Ocupante'
		ENDIF
				
		SET RELATION TO
		
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


**FUNCTION Val_Lot
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


**FUNCTION Val_Mausol
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

**FUNCTION Nom_Cuar
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

FUNCTION EstRec
*--------------
DO CASE
	CASE m.Estado ='00'
		@ ROW(),COL() SAY "Emitido"
	CASE m.Estado ='99'
		@ ROW(),COL() SAY "Anulado" COLOR SCHEME 5
ENDCASE
RETURN

*PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0015","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0016","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0017","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0018","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0019","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0020","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0021","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0022","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
