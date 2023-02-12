*------------------------------------
* Boletas.Prg
* Opcion para revisar las Boletas de 
* venta emitidas en velaciones
* 
* AUTOR : A. S. Marco Rodriguez Abad
*------------------------------------
*- Abriendo Archivos

PUBLIC worker,Rotulo1

DO ambiente
SET PROCEDURE TO LIBSIS
*SET PATH TO Y:\CEMENTER

CLOS DATA
USE Velacion IN  1 ORDER TAG Velacion1	ALIAS Velacion
USE Maestro  IN  2 ORDER TAG Cementerio	ALIAS Maestro
USE Fallecid IN  3 ORDER TAG Fallecid03	ALIAS Fallecid
USE Cuartel  IN  4 ORDER TAG cuartel01  ALIAS Cuartel
USE MasterVe IN  5 ORDER
*USE NiFocos IN  5 ORDER TAG NiFocos1 ALIAS NiFocos
*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Comprobantes (Velaciones)'
vmens02 = ' Comprobantes : REVISION '
vmens03 = ' DETALLES '
vmens04 = 'Dicho Comprobante no fue encontrado'
vmens05 = 'No existe Comprobante anterior'
vmens06 = 'No existe Comprobante siguiente'
vmens07 = '¨ Desea Anular ‚ste Comprobante ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Comprobante ha sido anulado'
vmens10 = 'El Comprobante ya est  Atendido'
vmens11 = 'El Comprobante ha sido devuelto'

SELECT Velacion
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
	TITLE vmens01 COLOR rgb(192,192,192,0,0,0) &&SCHEME 10

DEFINE MENU mmenu COLOR rgb(192,192,192,192,192,192)	&&SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
*DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
*DEFINE PAD elimi   OF mmenu PROMPT '\<Eliminar'   AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
*ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
*ON SELECTION PAD elimi  OF mmenu DO elimi
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi
RETURN

PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_0
CLEAR

 @  1, 3 SAY "        Periodo :"
 @  1,50 SAY " Estado :"
 @  2, 3 SAY "         Codigo :"
 @  3, 3 SAY "      N§ Boleta :"
 @  3,50 SAY " Fecha :"
 @  5, 3 SAY " Nombre Cliente :"
 @  7, 3 SAY " Direc. Cliente :"
 @  9, 3 SAY "     Cementerio :"
 @ 11, 3 SAY "        Difunto :"
 @ 13, 3 SAY "        Cuartel :"
 @ 13,40 SAY "          Nicho :"
 @ 15, 3 SAY "       Mausoleo :"
 @ 17, 3 SAY "          Costo :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
ON KEY LABEL F4 DO ImpBol
SELECT Velacion
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
 @  1, 21 SAY m.Anyo_Velac
 @  1, 60 SAY IIF(m.Anulado,"Anulado","       ")
 @  2, 21 SAY m.cod_Fallec
 @  3, 21 SAY m.Num_Recibo
 @  3, 60 SAY m.Fec_Recibo
 @  5, 21 SAY m.Nom_Adquir
 @  7, 21 SAY m.Dir_Adquir
 @  9, 21 SAY Val_Ce(m.Cod_Cement)						&& m.Cod_Cement
 @ 11, 21 SAY Val_Fall(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))					&& m.Cod_Fallec
 @ 13, 21 SAY Val_Cuar(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
 @ 13, 60 SAY Val_Nic(STR(m.Cod_Cement,1) + PADL(m.Cod_Fallec,6,'0'))
 @ 15, 21 SAY Val_Mau(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
 @ 17, 21 SAY m.Costo

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE Velacion
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SET RELATION TO STR(Cod_Cement,1)+PADL(ALLTRIM(STR(Cod_Fallec)),6,'0') INTO Fallecid
vTemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Num_Recibo  :H='B/V',;
	Nom_Adquir  :H='Cliente':25,;
	x = ALLTRIM(Fallecid.Ape_Patern) + ' ' + ALLTRIM(Fallecid.Ape_Matern) + ', ' + aLLTRIM(Fallecid.Nombres):H='Difunto':45

SET RELATION TO

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Velacion
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vNumBV   = SPACE(7)

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR rgb(192,192,192,0,0,0)		&&SCHEME 5

ACTIVATE WINDOW lista
@ 1,2 SAY 'N§ B/V :' GET vNumBV			&& PICTURE '!!'

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vNumBV) OR LASTKEY()=27
	RETURN
 ELSE
	SEEK vNumBV
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
SELE Velacion
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
SELE Velacion
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

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Velacion
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

DO pantalla

m.Anyo_Velac = YEAR(DATE())
m.Fec_Recibo = DATE()

@  1, 21 GET m.Anyo_Velac							color rgb(255,255,0,0,0,128)
@  1, 60 SAY IIF(m.Anulado,"Anulado","       ")
@  2, 21 GET m.Cod_Fallec		VALID Val_CFall()	color rgb(255,255,0,0,0,128)
*@  3, 21 GET m.Num_Recibo
*@  3, 60 GET m.Fec_Recibo
*@  5, 21 GET m.Nom_Adquir
*@  7, 21 GET m.Dir_Adquir
READ

sele MasterVe
loca for MasterVe.anyo=str(m.Anyo_Velac,4) and MasterVe.cod_cement = m.Cod_Cement
vn = Valor_Nich
vm = Valor_Maus
SELE Fallecid
IF SEEK(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
	SELE Cuartel
	IF SEEK(STR(m.Cod_Cement,1)+PADL(Fallecid.Cod_Cuarte,3,'0'))
		DO CASE
			CASE Cod_Tipo = 3 OR Cod_Tipo = 4
				m.Costo = vm
			CASE Cod_Tipo = 1
				m.Costo = vn
			OTHERWISE
				m.Costo = 0
		ENDCASE
	ELSE
		*xRet =SPACE(40)
	ENDIF
ELSE
	*xRet =SPACE(40)
ENDIF
SELE Velacion

@  9, 21 SAY Val_Ce(m.Cod_Cement)										color rgb(0,0,0,192,192,192)
@ 11, 21 SAY Val_Fall(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))		color rgb(0,0,0,192,192,192)			&& m.Cod_Fallec
@ 13, 21 SAY Val_Cuar(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))		color rgb(0,0,0,192,192,192)
@ 13, 60 SAY Val_Nic(STR(m.Cod_Cement,1) + PADL(m.Cod_Fallec,6,'0'))	color rgb(0,0,0,192,192,192)
@ 15, 21 SAY Val_Mau(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))		color rgb(0,0,0,192,192,192)

@  3, 21 GET m.Num_Recibo	VALID Val_Rec()								color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@  3, 60 GET m.Fec_Recibo												color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@  5, 21 GET m.Nom_Adquir												color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@  7, 21 GET m.Dir_Adquir												color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)

@ 17, 21 GET m.Costo													color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)

READ VALID val_read()

SELE Velacion

mAct = .T.

IF LASTKEY()#27
	IF mAct
		SELE Velacion
		IF f_Appd()
			GATHER MEMVAR
		ENDIF
		
*		sele MasterVe
*		loca for MasterVe.anyo=str(m.Anyo_Velac,4) and MasterVe.cod_cement = m.Cod_Cement
		
*		IF f_Lock(1)
*			REPLACE NumEnt WITH Parma.NumEnt+1
*		ENDIF
		
		UNLOCK ALL
	ENDIF
 ELSE
	IF EOF()
		GO BOTT
	 ELSE
		go vtemp
	ENDIF
	DO vista
	RETURN
ENDIF

SELECT Velacion

FLUSH

DO vista
RETURN

function Val_Rec
*---------------
PRIVATE mRet,nRec
m.Num_Recibo = PADL(ALLTRIM(m.Num_Recibo),7,'0')
nRec = RECNO()
IF !SEEK(m.Num_Recibo) AND m.Num_Recibo#"0000000"
	mRet = .T.
ELSE
	mRet = .F.
ENDIF

IF !mRet
	DO StandBy WITH "Reguistr Duplicado o erroneo"
ENDIF

RETURN mRet

FUNCTION Val_CFall
*-----------------
PRIVATE ap,am,no,mRet
mRet = .T.
ap = SPACE(30)
am = SPACE(30)
no = SPACE(30)

cAlias = ALIAS()
vIdx1  = SYS(3)+ '.Idx'

define   window ingr01	from 02, 3 to 14,75		DOUBLE	TITLE "" COLOR rgb(192,192,192,0,0,0)
activate window ingr01
@1,02   say "Datos a Buscar :    "	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@3,02 say 'Apellido Paterno    '	color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@3,35 say 'Apellido Materno   '		color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@4,02 get ap pict '@(!'
@4,35 get am pict '@(!'
@6,02 say 'Nombres '				color rgb(255,255,0,0,0,128),rgb(0,0,0,192,192,192)
@7,02 get no pict '@(!'
read
DEACTIVATE WINDOW ingr01
RELEASE    WINDOW ingr01

IF lastkey()#27

*	SELECT *;
*		FROM Fallecid;
*		WHERE Ape_patern = alltrim(ap) AND Ape_Matern = alltrim(am) AND Nombres = ALLTRIM(no) ;
*		ORDER BY Ape_Patern,Ape_Matern,Nombres;
*		INTO Cursor tmpCre

*   PARA AGREGAR NOMBRE DEL CUARTEL EN LA CONSULTA

	SELECT Fallecid.Cod_Cement,Fallecid.Cod_Cuarte,Fallecid.Cod_Fallec,Columna,Fila,Ape_Patern,Ape_Matern,Nombres,Cod_Tipo,Nom_Cuarte;
		FROM Fallecid,Cuartel ;
		WHERE Fallecid.Cod_Cement=Cuartel.Cod_cement and Fallecid.Cod_Cuarte = Cuartel.Cod_Cuarte and Ape_patern = alltrim(ap) AND Ape_Matern = alltrim(am) AND Nombres = ALLTRIM(no) ;
		ORDER BY Ape_Patern,Ape_Matern,Nombres;
		INTO Cursor tmpCre

	
	SELE TmpCre
	
	IF !EOF()
		vTempo = '°°°°°°°°°°° ®F2¯ Buscar   ®F10¯ Seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
		DEFINE WINDOW lista FROM 00,00 TO 24,79 DOUBLE ;
		TITLE vTempo FLOAT COLOR RGB(192,192,192,0,0,0)
		ACTIVATE WINDOWS Lista
		
		ON KEY LABEL F10 KEYBOARD CHR(23)
*		ON KEY LABEL F2 DO BusdetFa
		
		BROWSE WINDOWS Lista NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
				Ape_Patern	:H='A Paterno':15,;
				Ape_Matern	:H='A Materno':20,;
				Nombres		:H='Nombres':20,;
				Nom_Cuarte	:H="Cuartel"
		
		ON KEY LABEL F10
		ON KEY LABEL F2
		SET RELATION TO
		DEACTIVATE WINDOW Lista
		RELEASE WINDOW Lista
		
		IF LASTKEY()#27
		
			m.Cod_Cement = Cod_Cement
			m.Cod_Fallec = Cod_Fallec
			
		ELSE
			mRet = .F.
		ENDIF
	ELSE
		DO standBy WITH "No Existen Difuntos"
		mRet = .F.
	ENDIF
	
	USE IN TmpCre
	ERASE (vIdx1)
ENDIF

SELE (cAlias)
SHOW GETS

RETURN mRet

*FUNCTION BusDetFa			&& Realiza b£squeda directa
*----------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = IIF(!EOF(),RECNO(),-1)

DEFINE POPUP pLista FROM 15,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Cliente     '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Difunto     '

ON SELECTION POPUP pLista  DEACTIVATE POPUP

ACTIVATE POPUP pLista
RELEASE  POPUP pLista

cOrd1 = ORDER()
vBusca = SPACE(30)

DO CASE
	CASE BAR() =  1
		vNombre = "Cliente :"
		INDEX ON UPPER(NomCli) TO (vIdx1)
	CASE BAR() =  2
		vNombre = "Difunto : "
		INDEX ON Upper(NomBen) TO (vIdx1)
	OTHERWISE
		vBusca  = ''
		vNombre = ''
		SET ORDER TO
ENDCASE

IF LASTKEY()#27
	DEFINE WINDOW lisTA FROM 13,12 TO 16,68 DOUBLE ;
		TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 10
	
	ACTIVATE WINDOW lisTA
	@ 1,2 SAY vNombre GET vBusca FUNCTION "!"
	READ VALID val_read()
	DEACTIVATE WINDOW lista
	RELEASE WINDOW LISTA
ENDIF

IF EMPTY(vBusca) OR LASTKEY()=27
	* No hace nada
 ELSE
	SEEK ALLTRIM(vBusca)
	IF !FOUND()
		DO standby WITH "Cliente no Existe"
		IF vtemp = -1
			GO BOTT
		 ELSE
			GOTO vtemp
		ENDIF
	 ELSE
		* No hace Nada
	ENDIF
ENDIF

*SET ORDER TO &cOrd1

RETURN


*FUNCTION BusDetCr			&& Realiza b£squeda directa
*----------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = IIF(!EOF(),RECNO(),-1)

DEFINE POPUP pLista FROM 15,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Cliente     '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Difunto     '

ON SELECTION POPUP pLista  DEACTIVATE POPUP

ACTIVATE POPUP pLista
RELEASE  POPUP pLista

cOrd1 = ORDER()
vBusca = SPACE(30)

DO CASE
	CASE BAR() =  1
		vNombre = "Cliente :"
		INDEX ON UPPER(NomCli) TO (vIdx1)
	CASE BAR() =  2
		vNombre = "Difunto : "
		INDEX ON Upper(NomBen) TO (vIdx1)
	OTHERWISE
		vBusca  = ''
		vNombre = ''
		SET ORDER TO
ENDCASE

IF LASTKEY()#27
	DEFINE WINDOW lisTA FROM 13,12 TO 16,68 DOUBLE ;
		TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 10
	
	ACTIVATE WINDOW lisTA
	@ 1,2 SAY vNombre GET vBusca FUNCTION "!"
	READ VALID val_read()
	DEACTIVATE WINDOW lista
	RELEASE WINDOW LISTA
ENDIF

IF EMPTY(vBusca) OR LASTKEY()=27
	* No hace nada
 ELSE
	SEEK ALLTRIM(vBusca)
	IF !FOUND()
		DO standby WITH "Cliente no Existe"
		IF vtemp = -1
			GO BOTT
		 ELSE
			GOTO vtemp
		ENDIF
	 ELSE
		* No hace Nada
	ENDIF
ENDIF

*SET ORDER TO &cOrd1

RETURN

*FUNCTION ValCre
*--------------
PRIVATE mRet,cAlias,cOrd1,cOrd2

mRet = .T.
cAlias = ALIAS()
vIdx1  = SYS(3)+ '.Idx'

IF TYPE("m.TipVta")="U"
	m.TipVta = ""
ENDIF

SELECT Creditos.*, Clien.NomCli;
	FROM Creditos, Clien;
	WHERE Creditos.CodCli = Clien.CodCli AND Creditos.TipVta = m.TipVta; 
	ORDER BY NomCli;
	INTO Cursor tmpCre

SELE TmpCre

IF !EOF()
	vTempo = '°°°°°°°°°°° ®F2¯ Buscar   ®F10¯ Seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DEFINE WINDOW lista FROM 00,00 TO 24,79 DOUBLE ;
	TITLE vTempo FLOAT COLOR SCHEME 2
	ACTIVATE WINDOWS Lista
	
	ON KEY LABEL F10 KEYBOARD CHR(23)
*	ON KEY LABEL F2 DO Busdetcr
	
	BROWSE WINDOWS Lista NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
			NomCli	:H='Cliente':30,;
			NomBen	:H='Difunto':30,;
			FecCre	:H='F.Cred.',;
			MtoCre	:H='Credito',;
			TotInt	:H='Intereses',;
			TotAmo	:H='Amort.',;
			Saldo	:H='Saldo'
	
	ON KEY LABEL F10
	ON KEY LABEL F2
	SET RELATION TO
	DEACTIVATE WINDOW Lista
	RELEASE WINDOW Lista
	
	IF LASTKEY()#27
		m.CodCre   = codcre
		m.FecCre   = FecCre
		m.CodVta   = CodVta
		m.CodCli   = CodCli
		m.CodAvl   = CodAvl
		m.MtoCre   = MtoCre
		m.SaldoAF  = Saldo
		
		xfUltPag   = fUltPag
	ELSE
		mRet = .F.
	ENDIF
ELSE
	DO standBy WITH "No Existen Difuntos Pendientes de Pago"
	mRet = .F.
ENDIF

USE IN TmpCre
ERASE (vIdx1)

SELE (cAlias)
SHOW GETS

RETURN mRet


PROCEDURE lista
*--------------
PRIVATE nReg
nReg = RECNO()

mFecha1  = DATE()
mFecha2  = DATE()

DEFINE WINDOW wLista FROM 5,15 TO 17,70 FLOAT DOUBLE TITLE 'Reporte de Ventas Diarias' COLOR SCHEME 5
ACTIVATE WINDOW wLista

@ 01,01 SAY "      Rango de Fechas: " GET mFecha1
@ 01,36 GET mFecha2

@ 03,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

clear
*RELEASE WINDOW wLista

DO Espera WITH 1, "Procesando Ventas"

IF LASTKEY()#27 AND OkCancel#2

	SELE Velacion.Cod_Cement,Anyo_velac,Velacion.Cod_Fallec,Fec_Recibo,Num_Recibo,Nom_Adquir,Dir_Adquir,Anulado,costo,Fallecid.Cod_Cuarte,Columna,Fila,Ape_Patern,Ape_Matern,Nombres,Fec_Fallec,Cod_Tipo,Nom_Cuarte;
		FROM Velacion,fallecid,Cuartel;
		WHERE STR(Velacion.Cod_Cement,1)+ PADL(ALLTRIM(STR(Velacion.Cod_Fallec)),6,'0') == STR(Fallecid.Cod_Cement,1)+PADL(ALLTRIM(STR(Fallecid.Cod_Fallec)),6,'0') AND STR(Fallecid.Cod_Cement,1)+ PADL(ALLTRIM(STR(Fallecid.Cod_Cuarte)),6,'0') == STR(Cuartel.Cod_Cement,1)+PADL(ALLTRIM(STR(Cuartel.Cod_Cuarte)),6,'0') and BETW(Fec_Recibo,mFecha1,mFecha2);
		INTO CURSOR xTbl
	
	vIdx = SYS(3)+ '.Idx'
	INDEX ON STR(Cod_Cement)+PADL(ALLTRIM(STR(Cod_Cuarte)),3,'0')+Fila+PADL(ALLTRIM(STR(columna)),3,'0') TO (vIdx)
	
*	SET FILTER TO BETW(Fec_Recibo,mFecha1,mFecha2)
*	GO TOP
	DO Espera WITH 2
	IF !EOF()
		@5,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 1,11,8 default 'Pantalla'
		read
		if pantimp = 'Pantalla'
			REPORT FORM nicFocos PREVIEW
		else
			REPORT FORM NicFocos to print prompt
		endif
	 ELSE
		DO StandBy WITH "No Existe Informaci¢n para Procesar."
	ENDIF
	SET FILTER TO
	USE IN xTbl

 ELSE
	DO Espera WITH 2
	DO StandBy WITH "Proceso Cancelado."
ENDIF

RELEASE WINDOW wLista

sele Velacion
go nReg
DO vista
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

PROCEDURE ImpBol
*---------------
PRIVATE vDbf,cAlias
cAlias = ALIAS()
SCATTER MEMVAR
vDbf = SYS(3)+'.Dbf'
COPY STRU TO (vDbf)
SELE 0
USE (vDbf) ALIAS BVta
APPEND BLANK
GATHER MEMVAR

DEFINE WINDOW wLista FROM 5,15 TO 17,70 FLOAT DOUBLE TITLE 'Reporte de Ventas Diarias' COLOR SCHEME 5
ACTIVATE WINDOW wLista

@5,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 1,11,8 default 'Pantalla'
read
if pantimp = 'Pantalla'
	REPORT FORM Boleta PREVIEW
else
	REPORT FORM Boleta to print prompt
endif
DEACTIVATE WINDOWS wLista
RELEASE WINDOWS wLista

USE IN BVta
ERASE (vDbf)
SELE (cAlias)
RETURN