*-----------------------------------------
* Ventas.Prg
* Registra Ventas de Nichos y Mausoleos y entierros en zanja
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Ventas   IN  1 ORDER TAG Ventas1	ALIAS Ventas
USE Difunto  IN  2 ORDER TAG Difunto1	ALIAS Difunto
USE Cuartel  IN  3 ORDER TAG Cuartel1	ALIAS Cuartel
USE Cement   IN  4 ORDER TAG Cement1	ALIAS Cement
USE Nichos	 IN  5 ORDER TAG Nichos1	ALIAS Nichos
USE Parmae   IN  6 ORDER TAG Parmae1	ALIAS Parma
USE OcupFut  IN  7 ORDER TAG OcupFut1	ALIAS OcuFut
USE Mausoleo IN  8 ORDER TAG Mausoleo1	ALIAS Mausol
USE Clientes IN  9 ORDER TAG Clientes1	ALIAS Clien
USE Creditos IN 10 ORDER TAG Creditos1	ALIAS Creditos
USE Caja     IN 11 ORDER TAG Caja1      ALIAS Caja
USE Subvenc  IN 12 ORDER TAG Subvenc1   ALIAS Subvenc
USE Donac    IN 13 ORDER TAG Donac1		ALIAS Donac
USE KarCre   IN 14 ORDER TAG KarCre1    ALIAS KarCre

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Ventas'
vmens02 = ' Ventas : REVISION '
vmens04 = 'Dicha Venta no fue encontrada'
vmens05 = 'No existe Venta anterior'
vmens06 = 'No existe Venta siguiente'
vmens07 = '¨ Desea Eliminar ‚sta Venta ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Venta ha sido anulado'
vmens10 = 'La Venta ya est  Atendido'
vmens11 = 'La Venta ha sido devuelto'

SELECT Ventas
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
DEFINE PAD elimi   OF mmenu PROMPT 'A\<nula  '    AT 24,54
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
@ 01, 5 SAY "             Venta :"
@ 02, 5 SAY "     Tipo de Venta :"
@ 03, 5 SAY "         Condicion :"
@ 04, 5 SAY "               C/P :"    && cp   codigo   Fecha C/p
@ 05, 5 SAY "           Cliente :"
@ 06, 5 SAY "              Aval :"

@ 07, 5 SAY "        Cementerio :"
@ 08, 5 SAY "           Difunto :"
@ 09, 5 SAY "          Mausoleo :"
@ 10, 5 SAY "          Ocupante :"

@ 12, 5 SAY "Subvencionado (S/N):"
@ 13, 5 SAY "     Donacion (S/N):"

@ 11,38 SAY " Cod. Credito :"
@ 12,38 SAY "Cod. Subvenc. :"
@ 13,38 SAY "Cod. Donaci¢n :"

@ 14, 5 SAY "     Importe Total :"
@ 15, 5 SAY " Mto Subvencionado :"
@ 16, 5 SAY "        Mto Donado :"
@ 17, 5 SAY "     Mto Cancelado :"
@ 18, 5 SAY "           Cr‚dito :"
@ 19, 5 SAY "     Observaciones :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Ventas

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

@ 00, 60 SAY EstVta()
@ 01, 26 SAY m.CodVta
@ 01, 50 SAY m.FecVta
@ 02, 26 SAY m.TipVta
@ 02, 32 SAY VAL_PARA(m.TipVta,'TIPVTA','A',32,20)
@ 03, 26 SAY m.ConVta
@ 03, 32 SAY VAL_PARA(m.ConVta,'CONVTA','A',32,10)
@ 04, 26 SAY m.ComPag
@ 04, 36 SAY m.CodCP
@ 04, 50 SAY m.FecCP

@ 05, 26 SAY m.CodCli
@ 05, 32 SAY Val_Cli(m.CodCli,5,33)
@ 06, 26 SAY m.CodAvl
@ 06, 32 SAY IIF(ConVta='2',Val_Avl(m.CodAvl,6,33),SPACE(30))


@ 07, 26 SAY m.CodCem
@ 07, 31 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,7,31)

@ 08, 26 SAY IIF(!EMPTY(m.CodDif),m.CodDif,SPACE(8))
@ 08, 26 SAY IIF(!EMPTY(m.CodDif),vCodDif(8,35),SPACE(30))
@ 09, 26 SAY IIF(!EMPTY(m.CodMau),m.CodMau,SPACE(6))
@ 09, 26 SAY IIF(!EMPTY(m.CodMau),vCodMau(09,35),SPACE(30))
@ 10, 26 SAY IIF(!EMPTY(m.CodOcu),m.CodOcu,SPACE(8))
@ 10, 26 SAY IIF(!EMPTY(m.CodOcu),vCodOcu(10,35),SPACE(30))

@ 12, 26 SAY m.Subvenc
@ 13, 26 SAY m.Donacion

@ 11, 54 SAY m.CodCre
@ 12, 54 SAY m.CodSub
@ 13, 54 SAY m.CodDon

@ 14, 26 SAY m.MtoVta	PICTURE "99,999.99"
@ 15, 26 SAY m.MtoSub	PICTURE "99,999.99"
@ 16, 26 SAY m.MtoDon	PICTURE "99,999.99"
@ 17, 26 SAY m.MtoCan	PICTURE "99,999.99"
@ 18, 26 SAY m.MtoCre	PICTURE "99,999.99"
@ 19, 26 SAY LEFT(m.Observ,50)
@ 20, 26 SAY SUBSTR(m.Observ,51,50)

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Ventas
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

DEFINE POPUP pRevis FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pRevis PROMPT '\<a. Por Cliente '
DEFINE BAR 2  OF pRevis PROMPT '\<a. Por Codigo  '

ON SELECTION POPUP pRevis  DEACTIVATE POPUP
ACTIVATE POPUP pRevis

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		SET ORDER TO TAG Ventas2
	CASE BAR() =  2
		SET ORDER TO TAG Ventas1
ENDCASE

vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
SET RELATION TO CodCli INTO Clien
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	CodVta    :H='C¢digo',;
	Codcli    :H='Cliente',;
	Clien.NomCli :H='Nombre',;
	ComPag    :H='Comprobante',;
	CodCP     :H='N§'

SET RELATION TO
SET ORDER TO &cOrd1
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo

IF LASTKEY()=27
	GOTO vtemp
ENDIF

SHOW MENU mmenu
ON KEY LABEL F10
SELE Ventas
DO vista
RETURN


PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo  '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		vCodCem = SPACE(2)
		vBusca = SPACE(8)
		vNombre = "Codigo :"
		SET ORDER TO TAG Ventas1
ENDCASE

IF LASTKEY()#27
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
ENDIF
DO vista
SET ORDER TO &cOrd1
RETURN


PROCEDURE anter
*--------------
SELE Ventas
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
SELE Ventas
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

SELECT Ventas
SCATTER MEMVAR
DO pantalla

@ 00, 60 SAY VAL_PARA(m.Estado,'ESTVTA',' ',60,10)		&&m.Estado
@ 01, 26 GET m.CodVta	WHEN .F.
@ 01, 50 GET m.FecVta	WHEN .F.
@ 02, 26 GET m.TipVta	WHEN .F.
@ 02, 32 SAY VAL_PARA(m.TipVta,'TIPVTA','A',32,20)
@ 03, 26 GET m.ConVta	WHEN .F.
@ 03, 32 SAY VAL_PARA(m.ConVta,'CONVTA','A',32,10)
@ 04, 26 GET m.ComPag	WHEN .F.
@ 04, 36 GET m.CodCP	WHEN .F.
@ 04, 50 GET m.FecCP	WHEN .F.
@ 05, 26 GET m.CodCli	WHEN .F.
@ 05, 32 SAY Val_Cli(m.CodCli,5,33)
@ 06, 26 GET m.CodAvl	WHEN .F.
@ 06, 32 SAY IIF(m.ConVta='2',Val_Avl(m.CodAvl,6,33),SPACE(30))
@ 07, 26 GET m.CodCem	WHEN .F.
@ 07, 26 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,7,29)
@ 08, 26 GET m.CodDif	WHEN .F.
@ 08, 32 SAY IIF(!EMPTY(m.CodDif),vCodDif(08,35),SPACE(8))
@ 09, 26 GET m.CodMau	WHEN .F.
@ 09, 32 SAY IIF(!EMPTY(m.CodMau),vCodMau(09,35),SPACE(6))
@ 10, 26 GET m.CodOcu	WHEN .F.
@ 10, 26 SAY IIF(!EMPTY(m.CodOcu),vCodOcu(10,35),SPACE(8))

@ 12, 26 GET m.Subvenc	WHEN .F.
@ 13, 26 GET m.Donacion	WHEN .F.

@ 11, 54 GET m.CodCre	WHEN .F.
@ 12, 54 GET m.CodSub	WHEN .F.
@ 13, 54 GET m.CodDon	WHEN .F.

@ 14, 26 GET m.MtoVta	PICTURE "99,999.99" WHEN .F.
@ 15, 26 GET m.MtoSub	PICTURE "99,999.99" WHEN .F.
@ 16, 26 GET m.MtoDon	PICTURE "99,999.99" WHEN .F.
@ 17, 26 GET m.MtoCan	PICTURE "99,999.99"	WHEN .F.
@ 18, 26 GET m.MtoCre	PICTURE "99,999.99"	WHEN .F.
@ 19, 26 GET m.Observ	FUNCTION "!S50"

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Ventas
		m.tUser   = 'C'
		m.UserCR  = vUser_Id
		m.DUserCR = DATE()
		m.HUserCR = TIME()
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT Ventas
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Ventas
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

m.codEnt = SPACE(2)

DO pantalla

IF YesNo('Desea escoger registro de difunto o ocup. fut. ya Ingresado?')
	m.EscReg = 'S'
 ELSE
	m.EscReg = 'N'
ENDIF

m.Estado = '00'
STORE DATE() TO m.FecVta,m.FecCP,m.FecSub,m.FecDon

=CorVta()

@ 00, 60 SAY VAL_PARA(m.Estado,'ESTVTA',' ',60,10)		&&m.Estado
@ 01, 26 GET m.CodVta	WHEN .F.
@ 01, 50 GET m.FecVta	WHEN .F.
@ 02, 26 GET m.TipVta	VALID VAL_PARA(m.TipVta,'TIPVTA',' ',26,10)
@ 03, 26 GET m.ConVta	VALID VAL_PARA(m.ConVta,'CONVTA',' ',26,10)
@ 04, 26 GET m.ComPag	VALID VAL_PARA(m.ComPag,'COMPAG','C',26,10)
@ 04, 36 GET m.CodCP
@ 04, 50 GET m.FecCP
@ 05, 26 GET m.CodCli	VALID Val_Cli(m.CodCli,5,33) AND ValEstCli() AND Val_Ruc()
@ 06, 26 GET m.CodAvl	VALID Val_Avl(m.CodAvl,6,33) AND ValEstAvl() 		WHEN m.ConVta='2'

@ 07, 26 GET m.CodCem	VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,7,29)
@ 08, 26 GET m.CodDif	WHEN wCodDif()		VALID vCodDif(08,35)
@ 09, 26 GET m.CodMau	WHEN wCodMau()		VALID vCodMau(09,35)
@ 10, 26 GET m.CodOcu	WHEN wCodOcu()		VALID vCodOcu(10,35)

@ 12, 26 GET m.Subvenc	PICTURE "@M N,S"
@ 13, 26 GET m.Donacion	PICTURE "@M N,S"

@ 11, 54 GET m.CodCre	WHEN wCodCre()
@ 12, 54 GET m.CodSub	WHEN IIF(m.Subvenc='S',wCodSub(),.F.)
@ 13, 54 GET m.CodDon	WHEN IIF(m.Donacion='S',wCodDon(),.F.)

@ 14, 26 GET m.MtoVta	PICTURE "99,999.99"		VALID vMtoVta()
@ 15, 26 GET m.MtoSub	PICTURE "99,999.99"		WHEN .F.
@ 16, 26 GET m.MtoDon	PICTURE "99,999.99"		WHEN .F.
@ 17, 26 GET m.MtoCan	PICTURE "99,999.99"		VALID vMtoCan()
@ 18, 26 GET m.MtoCre	PICTURE "99,999.99"		WHEN .F.
@ 19, 26 GET m.Observ	FUNCTION "!S50"

READ VALID val_read() AND ValVta()

SELE Ventas

*susp

mAct = .T.

IF LASTKEY()#27
	
	m.tUser = 'I'
	m.User  = vUser_Id
	m.DUser = DATE()
	m.HUser = TIME()
	
	DO CASE
		CASE m.MtoCre = 0
			m.EstVta = '10'
		CASE m.MtoCre > 0
			m.EstVta = '20'
	ENDCASE
	
	IF m.EscReg = 'S'
		DO CASE
			CASE m.TipVta = "1"		&& Nicho
				SELE Difunto
				IF f_Lock(1)
					REPLACE Estado WITH '00', EstVta WITH m.EstVta
					UNLOCK
				ENDIF
			CASE m.TipVta = "2"		&& Mausoleo
				SELE Mausol
				IF f_Lock(1)
					REPLACE Estado WITH '00', EstVta WITH m.EstVta
					UNLOCK
				ENDIF
			CASE m.TipVta = "3"		&& Ocupacion Futura
				SELE OcuFut
				IF f_Lock(1)
					REPLACE Estado WITH '10', EstVta WITH m.EstVta
					UNLOCK
				ENDIF
		ENDCASE
	 ELSE
		DO CASE
			CASE m.TipVta = "1"	&& Nicho
				SELE Nichos
				SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
				Campo = "Col"+SUBSTR(m.Codnic,2,3)+'E'
				
				IF f_lock(1)
					REPLACE &Campo WITH "3"
					UNLOCK
				ENDIF
				
				SELE Difunto
				
				IF f_Appd()
					GATHER MEMVAR
					REPLACE Estado WITH '00', EstVta WITH m.EstVta
					UNLOCK
				ENDIF
				
				SELE Parma
				SEEK "CORRELDIFUNT"+m.codcem
				
				IF f_Lock(1)
					REPLACE NumEnt WITH Parma.NumEnt+1
					UNLOCK
				ENDIF
				
			CASE m.TipVta = "2"	&& Mausoleo
				
				SELE Mausol
				IF F_Appd()
					GATHER MEMVAR
					REPLACE Estado WITH '00', EstVta WITH m.EstVta
					UNLOCK
				ENDIF
				
				SELE Parma
				SEEK "CORRELMAUSOL"+m.codcem
				REPLACE NumEnt WITH Parma.NumEnt+1
				UNLOCK
				
			CASE m.TipVta = "3"	&& Ocupacion Futura
				
				SELE Nichos
				SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
				Campo = "Col"+SUBSTR(m.Codnic,2,3)+'E'
				
				IF f_lock(1)
					REPLACE &Campo WITH "2"
					UNLOCK
				ENDIF
				
				SELE OcuFut
				IF F_Appd()
					GATHER MEMVAR
					REPLACE Estado WITH '10', EstVta WITH m.EstVta
					UNLOCK
				ENDIF
				
				SELE Parma
				SEEK "CORRELOCUFUT"+ALLTRIM(m.Codcem)
				IF f_lock(1)
					REPLACE NumEnt WITH Parma.NumEnt+1
					UNLOCK
				ENDIF
		ENDCASE
	ENDIF
	
	* Actualizar Caja
	
	IF m.MtoCan > 0
		SELE Caja
		m.TipCaj  = '1'
		m.FecReg  = m.FecVta
		m.CorIng  = m.CodVta
		m.Importe = m.MtoCan
		
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH '00'
			UNLOCK
		ENDIF
	ENDIF
	
	* Actualizar Credito (C x C)
	
	IF m.MtoCre > 0
		m.FecCre  = m.FecVta
		m.CodMov  = m.CodCre
		m.fUltPag = m.FecVta
		
		xx = ActKarCre('CRE',m.CodMov,m.FecCre)
		
*		SELE Clien
*		IF f_Lock(1)
*			REPLACE Estado WITH '10'
*			UNLOCK
*		ENDIF
		
		* Parmae
		cMes = PADL(ALLTRIM(STR(MONT(m.FecCre))),2,'0')
		cAno = RIGHT(STR(YEAR(m.FecCre),4),2)
		
		SELE Parma
		
		IF !SEEK("CREDIT"+cAno+cMes)
			IF f_Appd()
				REPLACE Tipo WITH 'CREDIT', Codigo WITH cAno, CodigoAux WITH cMes, Descri WITH "CORRELATIVO DE CREDITOS: " + cMes +'/'+ cAno NumEnt WITH 1
			ENDIF
		 ELSE
			IF f_lock(1)
				REPLACE NumEnt WITH Parma.NumEnt+1
			ENDIF
		ENDIF
		UNLOCK
	ENDIF
	
	* Actualizar Subvenci¢n
	
	IF m.Subvenc='S'
		SELE Subvenc
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH '20'
			UNLOCK
		ENDIF
		
		SELE Parma
		
		* Parmae
		cMes = PADL(ALLTRIM(STR(MONT(m.FecSub))),2,'0')
		cAno = RIGHT(STR(YEAR(m.FecSub),4),2)
		
		IF !SEEK("CORSUB"+cAno+cMes)
			IF f_Appd()
				REPLACE Tipo WITH 'CORSUB', Codigo WITH cAno, CodigoAux WITH cMes, Descri WITH "CORRELATIVO DE SUBVENCIONES: " + cMes + '/' + cAno
				UNLOCK
			ENDIF
		ENDIF
		
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
		
		UNLOCK
		
	ENDIF
	
	* Actualizar Donacion
	
	IF m.Donacion='S'
		SELE Donac
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH '20'
			UNLOCK
		ENDIF
		
		SELE Parma
		
		* Parmae
		cMes = PADL(ALLTRIM(STR(MONT(m.FecDon))),2,'0')
		cAno = RIGHT(STR(YEAR(m.FecDon),4),2)
		
		IF !SEEK("CORDON "+cAno+cMes)
			IF f_Appd()
				REPLACE Tipo WITH 'CORDON', Codigo WITH cAno, CodigoAux WITH cMes, Descri WITH "CORRELATIVO DE SUBVENCIONES: " + cMes + '/' + cAno
				UNLOCK
			ENDIF
		ENDIF
		
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
		
		UNLOCK
		
	ENDIF
	
	* AQUI ME QUEDO
	* OJO agregar Subvenciones
	*     y para ma¤ana revisar corrige
	*     y la opcion elimina O ANULA
	
	* Agregar Ventas
	
	IF mAct
		
		SELE Ventas
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH m.EstVta
		ENDIF
		
		SELE Parma
		
		* Parmae
		cMes = PADL(ALLTRIM(STR(MONT(m.FecVta))),2,'0')
		cAno = RIGHT(STR(YEAR(m.FecVta),4),2)
		
		IF !SEEK("VENTA "+cAno+cMes)
			IF f_Appd()
				REPLACE Tipo WITH 'VENTA ', Codigo WITH cAno, CodigoAux WITH cMes, Descri WITH "CORRELATIVO DE VENTAS: " + cMes + '/' + cAno
				UNLOCK
			ENDIF
		ENDIF
		
		IF f_Lock(1)
			REPLACE NumEnt WITH Parma.NumEnt+1
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

SELECT Ventas

FLUSH

DO vista
RETURN


FUNCTION CorVta
*--------------
PRIVATE cMes,cAno,cAlias,mRet
cAlias = ALIAS()
SELE &cAlias
mRet = .T.
cMes = PADL(ALLTRIM(STR(MONT(m.FecVta))),2,'0')
cAno = RIGH(STR(YEAR(m.FecVta),4),2)

IF SEEK("VENTA "+cAno+cMes,"Parma")
	m.codVta =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	SHOW GET m.CodVta
 ELSE
*	SELE Parma
*	IF f_Appd()
*		REPLACE Tipo WITH 'VENTA ', Codigo WITH cAno, CodigoAux WITH cMes
*		UNLOCK
*	ENDIF
	m.codVta =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	SHOW GET m.CodVta
ENDIF

SELE (calias)

IF EMPTY(m.CodVta)
	DO StandBy WITH "El C¢digo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodVta)
		DO standby WITH 'Ya esta Registrado esta Venta'
		mRet = .F.
	ENDIF
ENDIF

RETURN mRet

FUNCTION wCorSub
*---------------
PRIVATE cAlias,cMes,cAno

cMes = PADL(ALLTRIM(STR(MONT(m.FecVta))),2,'0')
cAno = RIGH(STR(YEAR(m.FecVta),4),2)
cAlias = ALIAS()

IF SEEK("CORSUB"+cAno+cMes,"Parma")
	m.CodSub =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
 ELSE
	m.CodSub =  cAno+cMes+PADL(ALLTRIM(STR(1)),3,'0')
ENDIF

SHOW GET m.CodSub

RETURN .F.

FUNCTION wCodCre
*---------------
PRIVATE cAlias,cMes,cAno

cMes = PADL(ALLTRIM(STR(MONT(m.FecVta))),2,'0')
cAno = RIGH(STR(YEAR(m.FecVta),4),2)
cAlias = ALIAS()

IF m.ConVta = '2'
	IF SEEK("CREDIT"+cAno+cMes,"Parma")
		m.codCre =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	 ELSE
		m.CodCre =  cAno+cMes+PADL(ALLTRIM(STR(1)),3,'0')
	ENDIF
 ELSE
	m.CodCre =  SPACE(7)
ENDIF

SHOW GET m.CodCre

RETURN .F.

FUNCTION vMtoCan
*---------------
m.MtoCre = m.MtoVta - m.MtoCan - m.MtoSub - m.MtoDon
mRet = .T.

SHOW GET m.MtoCre

RETURN mRet

FUNCTION Valvta
*--------------
mRet = .T.
IF m.MtoCre < 0
	DO standBy WITH "El Mto del Credito no debe ser menor que 0"
	mRet = .F.
ENDIF

IF m.ConVta = '1' AND m.MtoCre > 0
	DO standBy WITH "La venta es al contado; por lo tanto no hay credito"
	mRet = .F.
ENDIF

IF m.ConVta = '2' AND m.MtoCre = 0
	DO standBy WITH "El Credito debe ser mayor que cero, de lo contrario es una venta al contado"
	mRet = .F.
ENDIF

IF m.Subvenc='S' AND m.MtoSub=0
	DO standBy WITH "La Subvenci¢n debe ser mayor que cero, de lo contrario no hay Subvenci¢n"
	mRet = .F.
ENDIF

IF m.Donacion='S' AND m.MtoDon=0
	DO standBy WITH "La Donaci¢n debe ser mayor que cero, de lo contrario no hay Donaci¢n"
	mRet = .F.
ENDIF

RETURN mRet

*FUNCTION vMtoCan
*---------------
m.MtoCre = m.MtoVta - m.MtoCan - m.MtoSub
IF m.MtoCre < 0
	DO standBy WITH "El Mto del Credito no debe ser menor que 0"
	mRet = .F.
ELSE
	mRet = .T.
ENDIF
SHOW GET m.MtoCre
RETURN mRet


FUNCTION vMtoVta
*---------------
IF m.MtoVta<m.MtoSub
	DO StandBy WITH "El Importe de la Venta no debe ser menor a la Subvencion"
	mRet = .F.
ELSE
	mRet = .T.
ENDIF
RETURN mRet

FUNCTION Val_Ruc
*---------------
PRIVATE cAlias
calias = ALIAS()
IF SEEK(m.CodCli,'Clien')
	m.RucCli = Clien.RucCli
 ELSE
	m.RucCli = SPACE(11)
ENDIF

SHOW GET m.RucCli

RETURN .T.


FUNCTION WCodDif
*---------------
PRIVATE mRet
IF m.TipVta = '1'
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF

RETURN mRet

FUNCTION WCodMau
*---------------
PRIVATE mRet
IF m.TipVta = '2'
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF

RETURN mRet

FUNCTION WCodOcu
*---------------
PRIVATE mRet
IF m.TipVta= '3'
	mRet = .T.
 ELSE
	mRet = .F.
ENDIF

RETURN mRet

FUNCTION wCodSub
*---------------
mRet = .T.

mAlias = ALIAS()

SELE Subvenc
	
DEFINE WINDOW w_Sub FROM 05,05 TO 20,70  DOUBLE ;
	TITLE "Ingresando Detalles de Subvencion" COLOR SCHEME 5
	
ACTIVATE WIND w_Sub

m.CodSub = SPACE(7)
m.FecSub = m.FecVta
*m.CodEnt = SPACE(2)
*m.CodVta = SPACE(7)
*m.CodCli = SPACE(6)
*m.CodDif = SPACE(8)
m.MtoSub = 0
	
@ 01,01 SAY "     Subvencion N§ :" GET m.CodSub		WHEN wCorSub()
@ 03,01 SAY "             Fecha :" GET m.FecSub		WHEN .F.
@ 05,01 SAY " Ent. Beneficiaria :" GET m.CodEnt		FUNCTION "!S40"	VALID VAL_PARA(m.CodEnt,'ENTSUB',' ',22,40)
@ 07,01 SAY "      C¢digo Venta :" GET m.CodVta		WHEN .F.

@ 09,01 SAY "    C¢digo Cliente :" GET m.CodCli		WHEN .F.
@ 11,01 SAY "    C¢digo Difunto :" GET m.Coddif		WHEN .F.
@ 13,01 SAY "  Monto Subvenci¢n :" GET m.MtoSub

READ VALID val_read()

DEACTIVATE WINDOW w_Sub
RELEASE WINDOW w_Sub

SHOW GET m.CodSub
SHOW GET m.MtoSub
mRet = .F.

SELE (mAlias)

*DO CASE
*	CASE mEnc
*		@ cFil,cCol SAY ALLTRIM(Difunto.NomDif)
*	CASE mRet
*		@ cFil,cCol SAY ALLTRIM(m.NomDif)
*ENDCASE

RETURN mRet

FUNCTION VCodDif
*---------------
PARAMETERS cFil,cCol

PUBLIC m.CodCuar, m.CodNic, m.FecFall, m.Fecsep, m.NomDif, m.ApeDif, m.SexoDif, m.EdadDif, m.EdadMes, m.EdadDia
PUBLIC m.Fila, m.Columna, m.CodTras, m.TipEnt, m.Reservado

PRIVATE mAlias,mRet

mRet = .T.
mEnc = .F.

mAlias = ALIAS()


IF SEEK(m.CodDif,'Difunto')
*	@ cFil,cCol SAY Difunto.NomDif
	mEnc = .T.
 ELSE
	IF m.EscReg = 'S'
		
		mAlias = ALIAS()
		SELE Difunto
		
		vTempo = '°°°°°°°°°°° ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		
		DEFINE WINDOW lisTA FROM 00,00 TO 24,79 DOUBLE ;
		TITLE vTempo FLOAT COLOR SCHEME 2
		
		ACTIVATE WINDOWS Lista
		
		SET FILTER TO EstVta = '00' AND TipEnt = '1'
		
		GO TOP
		IF !EOF()
			ON KEY LABEL F10 KEYBOARD CHR(23)
			
			SET RELATION TO CodCem INTO Cement
			
			BROWSE WINDOWS Lista NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem   :H='Cementerio'   :20,;
					Difunto.ApeDif  :H='Apll. Difunto':20,;
					Difunto.NomDif  :H='Nom. Difunto' :20
			
			ON KEY LABEL F10
			SET RELATION TO
			
			IF LASTKEY()#27
				m.CodDif  = Difunto.CodDif
				m.NomDif  = Difunto.NomDif
				m.ApeDif  = Difunto.ApeDif
			 ELSE
				mRet = .F.
			ENDIF
			
			vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
		 ELSE
			IF YesNo("No Existen No Existen Difuntos Registrados.")
				mRet = .F.
			ENDIF
		ENDIF
		
		DEACTIVATE WINDOW Lista
		RELEASE WINDOW Lista
		
		SET FILTER TO
		SELE (mAlias)
		
	 ELSE
		mAlias = ALIAS()
		
		vDbf = SYS(3)+".Dbf"
		CREATE TABLE (vDbf) (CodCem C(2), CodCuar C(4), Fila1 C(1), Fila2 C(1), Col1 C(3), Col2 C(3), Est C(1))
		USE &vDbf ALIAS Tempo EXCLUSIVE
		
		SELE Difunto
		
		DEFINE WINDOW w_Cli FROM 05,05 TO 21,70  DOUBLE ;
			TITLE "Ingresando Datos del Difunto" COLOR SCHEME 5
		
		ACTIVATE WIND w_Cli
		
*		m.codCem  = SPACE(2)
		m.CodDif  = SPACE(8)
		m.ApeDif  = SPACE(30)
		m.NomDif  = SPACE(30)
		m.SexoDif = SPACE(1)
		m.EdadDif = 0
		m.EdadMes = 0
		m.EdadDia = 0
		m.FecFall = m.FecVta
		m.Fecsep  = m.FecVta
		m.CodCuar = SPACE(4)
		m.CodNic  = SPACE(4)
		m.Fila    = SPACE(1)
		m.Columna = SPACE(3)
		m.CodMau  = SPACE(6)
		m.Reservado = 'N'
		m.TipEnt  = IIF(m.TipVta='1','1',IIF(m.TipVta='2','2',SPACE(1)))
		m.CodTras = SPACE(6)
		
		=IniVar()
		
		@ 01,01 SAY "  Cementerios :" GET m.CodCem		WHEN .F.
		@ 01,01 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,22) AND Cor_Dif()
		@ 02,01 SAY "       Codigo :" GET m.CodDif		WHEN .F.
		@ 03,01 SAY "Apell.Difunto :" GET m.Apedif		FUNCTION "!S40"
		@ 04,01 SAY " Nom. Difunto :" GET m.Nomdif		FUNCTION "!S40"
		@ 05,01 SAY " Sexo Difunto :" GET m.SexoDif		FUNCTION 'M M,F'	VALID VAL_PARA(m.SexoDif,'SEXO  ',' ',17,10)
		@ 06,01 SAY " Edad Difunto :" GET m.EdadDif		PICTURE "999"
		@ 06,20 SAY "A." 			  GET m.EdadMes		PICTURE "99"		VALID empty(m.edadMes) or m.edadMes < 12
		@ 06,25 SAY "M."			  GET m.EdadDia		PICTURE "99"		VALID empty(m.edadDia) or m.edadDia < 31
		@ 06,30 SAY "D."
		@ 07,01 SAY " Fecha Fallec.:" GET m.FecFall
		@ 08,01 SAY "Fec. Entierro :" GET m.FecSep
		@ 09,01 SAY "Tipo Entierro :" GET m.TipEnt		WHEN .F.
		@ 09,01 SAY VAL_PARA(m.TipEnt,'TIPENT','A',22,10)
		@ 10,01 SAY "      Cuartel :" GET m.CodCuar		WHEN m.TipEnt='1'	AND m.Reservado = 'N' VALID Val_Cuar(9,22) AND Tab_Nic()
		@ 11,01 SAY "         Fila :" GET m.Fila		WHEN m.TipEnt='1'	AND m.Reservado = 'N' VALID Val_F()
		@ 12,01 SAY "      Columna :" GET m.Columna		WHEN m.TipEnt='1'	AND m.Reservado = 'N' VALID Val_C()
		@ 13,01 SAY "     Mausoleo :" GET m.CodMau		WHEN m.TipEnt='2'	AND m.Reservado = 'N'
		@ 14,01 SAY "      Cliente :" GET m.Codcli		WHEN .F.
		@ 14,22 SAY Val_Cli(m.CodCli,14,24)
		
		READ VALID val_read()
		
		DEACTIVATE WINDOW w_Cli
		RELEASE WINDOW w_Cli
		
		IF LASTKEY()#27
			mRet = .T.
		 ELSE
			mRet = .F.
		ENDIF
		
		USE IN Tempo
		ERASE &vDbf
		
		SELE (mAlias)
		
	ENDIF
ENDIF

DO CASE
	CASE mEnc
		@ cFil,cCol SAY PADR(ALLTRIM(Difunto.ApeDif)+', '+ALLTRIM(Difunto.NomDif),40,' ')
	CASE mRet
		@ cFil,cCol SAY PADR(ALLTRIM(m.ApeDif)+', '+ALLTRIM(m.NomDif),40,' ')
ENDCASE

RETURN mRet


FUNCTION vCodMau
*---------------
PARAMETERS cFil,cCol

PUBLIC m.Lotizado,m.NomLote,m.Tipo,m.Ubicacion,m.Familia,m.Area_Adq
PUBLIC m.Area_Const,Area_Cerc,TotDif

PRIVATE mAlias,mRet

mRet = .T.
mEnc = .F.

IF SEEK(m.CodMau,'Mausol')
*	@ cFil,cCol SAY Mausol.Familia
	mEnc = .T.
 ELSE
	IF m.EscReg = 'S'
		
		mAlias = ALIAS()
		SELE Mausol
		
		vTempo = '°°°°°°°°°°° ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		
		DEFINE WINDOW lisTA FROM 00,00 TO 24,79 DOUBLE ;
		TITLE vTempo FLOAT COLOR SCHEME 2
		
		ACTIVATE WINDOWS Lista
		
		SET FILTER TO EstVta = '00'

		GO TOP
		IF !EOF()
			ON KEY LABEL F10 KEYBOARD CHR(23)
			
			SET RELATION TO CodCem INTO Cement
			
			BROWSE WINDOWS Lista NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem	:H='Cementerio':25,;
					Mausol.Familia	:H='Familia' :25,;
					Mausol.NomLote	:H='Lote':25
			
			ON KEY LABEL F10
			SET RELATION TO
			
			IF LASTKEY()#27
				m.CodMau  = Mausol.CodMau
				m.Familia = Mausol.Familia
			 ELSE
				mRet = .F.
			ENDIF
			
		 ELSE
			IF YesNo("No Existen No Existen Mausoleos Registrados.")
				mRet = .F.
			ENDIF
		ENDIF
		
		DEACTIVATE WINDOW Lista
		RELEASE WINDOW Lista
		
		SET FILTER TO
		SELE (mAlias)
		
	 ELSE
		
		mAlias = ALIAS()
		
		SELECT Mausol
		vtemp = IIF(EOF(),-1,RECNO())
		
		DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
			TITLE "Ingresando Mausoleos" COLOR SCHEME 5
		
		ACTIVATE WIND w_Cli
		
		m.estado     = '10'
		m.Lotizado   = SPACE(1)
		m.NomLote    = SPACE(50)
		m.Familia    = SPACE(40)
		m.Tipo       = SPACE(1)
		m.Ubicacion  = SPACE(50)
		m.Area_Adq   = SPACE(10)
		m.Area_Const = SPACE(10)
		m.Area_Cerc  = SPACE(10)
		m.TotDif     = 0
		
		=Cor_Maus()
		
		@ 01,05 SAY "Cementerio: " GET m.CodCem	WHEN .F.
		@ 01,05 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,25)
		@ 02,05 SAY "    Codigo: " GET m.CodMau	WHEN .F.
		@ 03,05 SAY "Lotizado? : " GET m.Lotizado	FUNCTION 'M S,N'
		@ 04,05 SAY "Nomb. Lote: " GET m.NomLote	WHEN m.Lotizado = 'S'	FUNCTION '!S40'
		@ 05,05 SAY "  Familia : " GET m.Familia	FUNCTION '!S40'
		@ 06,05 SAY "      Tipo: " GET m.Tipo						VALID VAL_PARA(m.Tipo,'TIPMAU',' ',18,10)
		@ 07,05 SAY " Ubicaci¢n: " GET m.Ubicacion	FUNCTION '!S30'
		@ 08,05 SAY " Area adq.: " GET m.Area_Adq
		@ 09,05 SAY "Area Const.:" GET m.Area_Const
		@ 10,05 SAY "Area Cerc.: " GET m.Area_Cerc
		@ 11,05 SAY "Total Dif.: " GET m.TotDif		PICTURE "99"
		@ 12,05 SAY "   Cliente: " GET m.Codcli		WHEN .F.
		@ 12,22 SAY Val_Cli(m.CodCli,12,26)
		
		READ VALID val_read()
		
		DEACTIVATE WINDOW w_Cli
		RELEASE WINDOW w_Cli
		
		IF LASTKEY()#27
			mRet = .T.
		 ELSE
			mRet = .F.
		ENDIF
		
		SELECT (mAlias)
	
	ENDIF
ENDIF

DO CASE
	CASE mEnc
		@ cFil,cCol SAY ALLTRIM(Mausol.Familia)
	CASE mRet
		@ cFil,cCol SAY ALLTRIM(m.Familia)
ENDCASE

RETURN mRet


FUNCTION vCodOcu
*---------------
PARAMETERS cFil,cCol

PUBLIC m.NomOcu,m.ApeOcu,m.EdadOcu,m.EdadOMe,m.EdadODi,m.SexoOcu,m.DNICli,m.CodCuar,m.CodNic,m.Fila,m.Columna

PRIVATE mAlias,mRet

mRet = .T.
mEnc = .F.

IF SEEK(m.CodOcu,'OcuFut')
*	@ cFil,cCol SAY OcuFut.NomOcu
	mEnc = .T.
 ELSE
	IF m.EscReg = 'S'
		
		mAlias = ALIAS()
		SELE OcuFut
		
		vTempo = '°°°°°°°°°°° ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		
		DEFINE WINDOW lisTA FROM 00,00 TO 24,79 DOUBLE ;
		TITLE vTempo FLOAT COLOR SCHEME 2
		
		ACTIVATE WINDOWS Lista
		
		SET FILTER TO Estado = '10' AND EstVta = '00'
		
		GO TOP
		IF !EOF()
			ON KEY LABEL F10 KEYBOARD CHR(23)
			
			SET RELATION TO CodCem INTO Cement
			
			BROWSE WINDOWS Lista NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem	:H='Cementerio':30,;
					OcuFut.NomOcu	:H='Ocupante' :30
			
			ON KEY LABEL F10
			SET RELATION TO
			
			IF LASTKEY()#27
				m.CodOcu  = OcuFut.CodOcu
				m.NomOcu  = OcuFut.NomOcu
			 ELSE
				mRet = .F.
			ENDIF
			
		 ELSE
			IF YesNo("No Existen No Existen Ocupaciones Registradas.")
				mRet = .F.
			ENDIF
		ENDIF
		
		DEACTIVATE WINDOW Lista
		RELEASE WINDOW Lista
		
		SET FILTER TO
		SELE (mAlias)
	
	 ELSE
		
		mAlias = ALIAS()
		
		vDbf = SYS(3)+".Dbf"
		CREATE TABLE &vDbf (CodCem C(2), CodCuar C(4), Fila1 C(1), Fila2 C(1), Col1 C(3), Col2 C(3), Est C(1))
		USE &vDbf ALIAS Tempo EXCLUSIVE
		
		SELECT OcuFut
		vtemp = IIF(EOF(),-1,RECNO())
		
		DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
			TITLE "Ingresando Venta de Ocupaci¢n Futura" COLOR SCHEME 5
		
		ACTIVATE WIND w_Cli
		
		=Cor_OF()
		m.DniCli  = SPACE(8)
		m.NomOcu  = SPACE(50)
		m.ApeOcu  = SPACE(50)
		m.SexoOcu = SPACE(1)
		m.EdadOcu = 0
		m.EdadOMe = 0
		m.EdadODi = 0
		m.CodCuar = SPACE(4)
		m.CodNic  = SPACE(4)
		m.Fila	  = SPACE(1)
		m.Columna = SPACE(3)
		
		@  1, 05 SAY " Cementerio: " GET m.CodCem	WHEN .F.
		@  1, 05 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,31)
		@  2, 05 SAY "   Ocupante: " GET m.CodOcu	WHEN .F.
		@  3, 05 SAY "    Cliente: " GET m.CodCli	WHEN .F.
		@  3, 05 SAY Val_Cli(m.CodCli,3,33)
		@  4, 05 SAY "DNI Cliente: " GET m.DNICli	WHEN .F.
		@  5, 05 SAY "Ap.Ocupante: " GET m.ApeOcu	FUNCTION '!S30'
		@  6, 05 SAY "No.Ocupante: " GET m.NomOcu	FUNCTION '!S30'
		@  7, 05 SAY " Sexo Ocup.: " GET m.SexoOcu	FUNCTION 'M M,F' VALID VAL_PARA(m.SexoOcu,'SEXO  ',' ',34,20)
		@  8, 05 SAY " Edad Ocup.: " GET m.EdadOcu	PICTURE "999"
		@  8, 22 SAY "A." GET m.EdadOMe	PICTURE "99"		VALID empty(m.edadOMe) or m.edadOMe < 12
		@  8, 27 SAY "M." GET m.EdadODi	PICTURE "99"		VALID empty(m.edadODi) or m.edadOdi < 31
		@  8, 32 SAY "D."
		@ 10, 05 SAY "    Cuartel: " GET m.CodCuar	VALID Val_Cuar(10,31) AND Tab_NIC()
		@ 11, 05 SAY "      Nicho: " GET m.CodNic	DISABLE
		@ 12, 05 SAY "       Fila: " GET m.Fila		VALID Val_F()
		@ 13, 05 SAY "    Columna: " GET m.Columna	VALID Val_C()
		
		READ VALID val_read()
		
		DEACTIVATE WINDOW w_Cli
		RELEASE WINDOW w_Cli
		
		IF LASTKEY()#27
			mRet = .T.
		 ELSE
			mRet = .F.
		ENDIF
	
		USE IN Tempo
		ERASE &vDbf
	
		SELECT (mAlias)
	
	ENDIF
ENDIF

DO CASE
	CASE mEnc
		@ cFil,cCol SAY OcuFut.NomOcu
	CASE mRet
		@ cFil,cCol SAY m.NomOcu
ENDCASE

RETURN mRet

FUNCTION Cor_Maus
*----------------
cAlias = ALIAS()
SELE Mausol
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

RETURN mRet






*******************************************************************






**FUNCTION IniVar
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

PROCEDURE Anula
*--------------
PRIVATE lAnula

lAnula = .T.
IF YesNo("Desea Eliminar este registro")
	IF !ve_passw("ELIDIF")
		DO STANDBY WITH "Proceso Cancelado"
		lAnula = .F.
	ENDIF
 ELSE
	DO STANDBY WITH "Proceso Cancelado"
	lAnula = .F.
ENDIF

IF lAnula
	IF m.Estado = '99'
		DO StandBy WITH "La Venta ya ha sido anulado."
		lAnula = .F.
	ENDIF
ENDIF

IF lAnula
	SELE KarCre
	xCre = m.CodCre
	IF SEEK(m.CodCre)
		xCon = 0
		SCAN WHILE CodCre = m.CodCre
			xCon = xCon + 1
		ENDSCAN
		IF xCon > 1
			DO StandBy WITH "Este Credito no se puede anular por que ya tiene pagos a cuenta. Avise a Area de Sistemas"
			lAnula = .F.
		ENDIF
	ENDIF
ENDIF

IF lAnula
	DO CASE
		CASE m.TipVta = "1"		&& Nicho
			SELE Difunto
			IF f_Lock(1)
				REPLACE EstVta WITH "00"
				UNLOCK
			ENDIF
		CASE m.TipVta = "2"		&& Mausoleo
			SELE Mausol
			IF f_Lock(1)
				REPLACE EstVta WITH "00"
				UNLOCK
			ENDIF
		CASE m.TipVta = "3"		&& Ocupacion Futura
			SELE OcuFut
			IF f_Lock(1)
				REPLACE EstVta WITH "00"
				UNLOCK
			ENDIF
	ENDCASE
	
	IF m.MtoCan > 0
		SELE Caja
		IF SEEK('1'+m.CodVta)
			IF f_Lock(1)
				REPLACE Estado WITH '99'
				UNLOCK
			ENDIF
		 ELSE
			lAnula = .F.
		ENDIF
	ENDIF
	
	IF lAnula
		IF m.MtoCre > 0
			SELE KarCre
			IF SEEK(m.CodCre)
				IF f_Lock(1)
					REPLACE Estado WITH "99"
					UNLOCK
				ENDIF
			 ELSE
				lAnula = .F.
			ENDIF
			SELE Creditos
			IF SEEK(m.CodCre)
				IF f_Lock(1)
					REPLACE Estado WITH "99"
					UNLOCK
				ENDIF
			 ELSE
				lAnula = .F.
			ENDIF
		ENDIF
	ENDIF
	
	IF lAnula
		IF m.Subvenc = 'S'
			SELE Subvenc
			IF SEEK(m.CodSub)
				IF f_Lock(1)
					REPLACE Estado WITH "99"
					UNLOCK
				ENDIF
			 ELSE
				lAnula = .F.
			ENDIF
		ENDIF
	ENDIF
	
	IF lAnula
		IF m.Donacion = 'S'
			SELE Donac
			IF SEEK(m.CodDon)
				IF f_Lock(1)
					REPLACE Estado WITH "99"
					UNLOCK
				ENDIF
			 ELSE
				lAnula = .F.
			ENDIF
		ENDIF
	ENDIF
	
	IF lAnula
		SELE Ventas
		IF f_lock(1)
			REPLACE Estado WITH '99'
			UNLOCK ALL
		ENDIF
	 ELSE
		DO StandBy WITH "Ocurrio un Error. Avise a Area de Sistemas"
	ENDIF
ENDIF

DO VISTA
RETURN

PROCEDURE lista
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

nReg = RECNO()

SELE Ventas
SET Relation to CodCli INTO Clien

DEFINE WINDOW wLista FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Ventas' COLOR SCHEME 5
ACTIVATE WINDOW wLista

vToVe = 1
vToTi = 1
ltCon = 1
mFecha1 = CTOD("01/01/"+STR(YEAR(m.FecSis),4))
mFecha2 = DATE()

@ 01,01 SAY "  Todos las Ventas : " GET vToVe	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2
@ 03,01 SAY "              Venta: " GET m.CodVta	WHEN vToVe = 2
@ 05,01 SAY "   Todos los Tipos : " GET vToTi	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2	WHEN vToVe = 1
@ 07,01 SAY "      Tipo de Venta: " GET m.TipVta	WHEN vToTi = 2	VALID VAL_PARA(m.TipVta,'TIPVTA',' ',23,20)
@ 09,01 SAY "  Modalid. de Pago : " GET ltCon    FUNCTION '^ Todos;Ventas Contado;Ventas Credito'	WHEN vToVe = 1
@ 13,01 SAY "    Rango de Fechas: " GET mFecha1	WHEN vToVe = 1
@ 13,36 GET mFecha2		WHEN vToVe = 1

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

IF LASTKEY()#27 AND OkCancel#2
	IF vToVe = 2
		IF SEEK(m.CodVta)
			* eSTE rEPORTE pODRIA cAMBIAR SI SE QUIERE ALGO DIFERENTE Y MAS DETALLADO
			SET FILTER TO CodVta = m.CodVta
			DO REPORTE WITH 2,"Ventas","Listado de Ventas"
			SET FILTER TO
		ELSE
			DO StandBy WITH 'Venta no encontrada'
		ENDIF
	ELSE
		lFil = ".T."
		IF vToTi = 2
			lFil = lFil + " AND TipVta = m.TipVta"
		ENDIF
		DO CASE
			CASE ltCon = 1
				lFil = lFil + " AND .T."
			CASE ltCon = 2
				lFil = lFil + " AND ConVta = '1'"
			CASE ltCon = 3
				lFil = lFil + " AND ConVta = '2'"
		ENDCASE
		
		SET FILTER TO &lFil AND BETW(FecVta,mFecha1,mFecha2)
		DO REPORTE WITH 2,"Ventas","Listado de Ventas"
		SET FILTER TO
	ENDIF
ENDIF

SELECT Ventas
SET Relation to
GO nReg
DO vista

RETURN

FUNCTION wCorDon
*---------------
PRIVATE cAlias,cMes,cAno

cMes = PADL(ALLTRIM(STR(MONT(m.FecVta))),2,'0')
cAno = RIGH(STR(YEAR(m.FecVta),4),2)
cAlias = ALIAS()

IF SEEK("CORDON"+cAno+cMes,"Parma")
	m.CodDon =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
 ELSE
	m.CodDon =  cAno+cMes+PADL(ALLTRIM(STR(1)),3,'0')
ENDIF

SHOW GET m.CodDon

RETURN .F.

FUNCTION wCodDon
*---------------
mRet = .T.

mAlias = ALIAS()

SELE Donac
	
DEFINE WINDOW w_Don FROM 05,05 TO 20,70  DOUBLE ;
	TITLE "Ingresando Detalles de Donaci¢n" COLOR SCHEME 5
	
ACTIVATE WIND w_Don

m.CodDon = SPACE(7)
m.FecDon = m.FecVta
*m.NomBen = SPACE(30)
*m.CodEnt = SPACE(2)
*m.CodVta = SPACE(7)
*m.CodCli = SPACE(6)
*m.CodDif = SPACE(8)
m.MtoDon = 0
	
@ 01,01 SAY "     Donaci¢n N§ :" GET m.CodDon		WHEN wCorDon()
@ 03,01 SAY "           Fecha :" GET m.FecDon		WHEN .F.
@ 07,01 SAY "    C¢digo Venta :" GET m.CodVta		WHEN .F.

@ 09,01 SAY "  C¢digo Cliente :" GET m.CodCli		WHEN .F.
@ 11,01 SAY "         Difunto :" GET m.CodDif		WHEN .F.
@ 13,01 SAY "  Monto Donaci¢n :" GET m.MtoDon

READ VALID val_read()

DEACTIVATE WINDOW w_Don
RELEASE WINDOW w_Don

SHOW GET m.CodDon
SHOW GET m.MtoDon
mRet = .F.

SELE (mAlias)

*DO CASE
*	CASE mEnc
*		@ cFil,cCol SAY ALLTRIM(Difunto.NomDif)
*	CASE mRet
*		@ cFil,cCol SAY ALLTRIM(m.NomDif)
*ENDCASE

RETURN mRet

FUNCTION RetTV
*-------------
PARAMETER mTv
PRIVATE mRet
IF SEEK('TIPVTA'+mTv,'Parma')
	mRet = Parma.Descri
ELSE
	mRet = "Sin Descripci¢n"
ENDIF
RETURN mRet


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

FUNCTION EstVta
*--------------
DO CASE
	CASE m.Estado ='10'
		@ ROW(),COL() SAY "CANCELADO" COLOR SCHEME 5
	CASE m.Estado ='20'
		@ ROW(),COL() SAY "CON SALDO" COLOR SCHEME 5
	CASE m.Estado ='99'
		@ ROW(),COL() SAY "ANULADO  " COLOR SCHEME 5
	OTHERWISE
		@ ROW(),COL() SAY "         "
ENDCASE
RETURN

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0002","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0003","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0004","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0005","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0006","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0007","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0008","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0009","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
