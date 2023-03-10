*-----------------------------------------
* Difuntos.Prg
* Registra Ingreso de Difuntos
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos
PUBLIC mLote,mUbic

CLOS DATA
USE Difunto  IN  1 ORDER TAG Difunto1	ALIAS Difunto
USE Cuartel  IN  2 ORDER TAG Cuartel1	ALIAS Cuartel
USE Cement   IN  3 ORDER TAG Cement1	ALIAS Cement
USE Nichos	 IN  4 ORDER TAG Nichos1	ALIAS Nichos
USE Parmae   IN  5 ORDER TAG Parmae1	ALIAS Parma
USE OcupFut  IN  6 ORDER TAG OcupFut1	ALIAS OcuFut
USE Mausoleo IN  7 ORDER TAG Mausoleo1	ALIAS Mausol
USE Clientes IN  8 ORDER TAG Clientes1	ALIAS Clien

vDbf = SYS(3)+".Dbf"
CREATE TABLE (vDbf) (CodCem C(2), CodCuar C(4), Fila1 C(1), Fila2 C(1), Col1 C(3), Col2 C(3),Est C(1))
USE &vDbf IN 9 ALIAS Tempo EXCLUSIVE

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Difuntos'
vmens02 = ' Difuntos : REVISION '
vmens04 = 'Dicho Difunto no fue encontrado'
vmens05 = 'No existe Difunto anterior'
vmens06 = 'No existe Difunto siguiente'
vmens07 = '? Desea Eliminar ?ste Difunto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Difunto ha sido anulado'
vmens10 = 'El Difunto ya est? Atendido'
vmens11 = 'El Difunto ha sido devuelto'

SELECT Difunto
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

*@ 0,50 SAY "      Traslado N?:"
*@ 1,50 SAY "Ocup. Fut."

@  0,50 SAY " Estado :"
@  1, 5 SAY "    Cementerio :"
@  2, 5 SAY "        Codigo :"
@  3, 5 SAY "Apell. Difunto :"
@  4, 5 SAY " Nomb. Difunto :"
@  5, 5 SAY "  Sexo Difunto :"
@  6, 5 SAY "   Edad Difunto:"
@  6,29 SAY "A."
@  6,34 SAY "M."
@  6,39 SAY "D."
@  7, 5 SAY " Fecha Fallec. :"
@  8, 5 SAY " Fecha Sepelio :"
@ 09, 5 SAY " Tipo Entierro :"
@ 10, 5 SAY "       Cuartel :"
@ 11, 5 SAY "         Nicho :"
@ 12, 5 SAY "          Fila :"
@ 13, 5 SAY "       Columna :"
@ 14, 5 SAY "      Mausoleo :"
@ 15, 5 SAY "          Lote :"
@ 16, 5 SAY "     Ubicacion :"
@ 17, 5 SAY "       Cliente :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Difunto

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

IF !EMPTY(m.CodOcu)
	@  0, 05 SAY "Ocup. Fut.: " + m.CodOcu  COLOR SCHEME 5
 ELSE
	@  0, 05 SAY SPACE(20)
ENDIF

DO CASE
	CASE m.Estado = '00'
		@  0, 60 SAY SPACE(15)
	CASE m.Estado ='30'
		@  0, 60 SAY "TRASLADADO"	COLOR SCHEME 5
ENDCASE

@  1, 26 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,26)

DO CASE
	CASE m.EstVta ='00'
		@  1, 60 SAY SPACE(15)
	CASE m.EstVta ='10'
		@  1, 60 SAY "Cancelado"	COLOR SCHEME 5
	CASE m.EstVta ='20'
		@  1, 60 SAY "Con Saldo"	COLOR SCHEME 5
	CASE m.EstVta ='99'
		@  1, 60 SAY "Anulado"		COLOR SCHEME 5
ENDCASE

@  2, 26 SAY m.CodDif
@  3, 26 SAY m.ApePat
@  3, 50 SAY m.ApeMat
@  4, 26 SAY m.NomDif
@  5, 26 SAY VAL_PARA(m.SexoDif,'SEXO  ',' ',26,9)		&&m.Sexo
@  6, 26 SAY m.EdadDif	PICTURE '999'
@  6, 32 SAY m.EdadMes	PICTURE '99'
@  6, 37 SAY m.EdadDia	PICTURE '99'
@  7, 26 SAY m.FecFall
@  8, 26 SAY m.FecSep
@  9, 26 SAY VAL_PARA(m.TipEnt,'TIPENT','V',26,15)
@ 10, 26 SAY IIF(m.TipEnt='1',Val_Cuar(10,26),SPACE(30))
@ 11, 26 SAY IIF(m.TipEnt='1',m.CodNic,SPACE(5))
@ 12, 26 SAY IIF(m.TipEnt='1',m.Fila,SPACE(1))
@ 13, 26 SAY IIF(m.TipEnt='1',m.Columna,SPACE(3))
@ 14, 26 SAY IIF(m.TipEnt='2',Val_Mau(14,26),SPACE(30))
@ 15, 26 SAY IIF(m.TipEnt='2',mLote,SPACE(30))
@ 16, 26 SAY IIF(m.TipEnt='2',mUbic,SPACE(30))
@ 17, 26 SAY IIF(EMPTY(m.codcli),SPACE(50),Val_Fun('Clien','CodCli','NomCli',m.CodCli,1,17,26))

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE revis              && Revisi?n de BD en browse
*--------------
SELE Difunto
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo  '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre  '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

IF LASTKEY()#27
ENDIF
cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		SET ORDER TO TAG Difunto1
	CASE BAR() =  2
		SET ORDER TO TAG Difunto2
	OTHERWISE
		SET ORDER TO &cOrd1
ENDCASE

vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
DO logos WITH rotulo1,vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
SET RELATION TO CodCem INTO Cement
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Cement.NomCem    :H='Cementerio' :20,;
	Difunto.CodDif   :H='C?digo',;
	Difunto.ApePat   :H='Ap. Paterno',;
	Difunto.ApeMat   :H='Ap. Materno',;
	Difunto.NomDif   :H='Nom.  Difunto'

SET RELATION TO
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Difunto
SET ORDER TO &cOrd1
DO vista
RETURN

PROCEDURE busca              && Realiza b?squeda directa
*--------------
PRIVATE cOrd,vTemp,vAP,vAM,vNom
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = RECNO()
vAP   = SPACE(20)
vAM   = SPACE(20)
vNom  = SPACE(30)

cOrd  = ORDER()
lBusca = .F.

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lista
@ 1,2  SAY 'Apellidos:' GET vAP		FUNCTION '!S20'
@ 1,34 GET vAM		FUNCTION '!S20'
@ 3,2  SAY '  Nombres:' GET vNom	FUNCTION '!S30'

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vAp+vAM+vNom) OR LASTKEY()=27
    SET ORDER TO (cOrd)
	RETURN
 ELSE
 	SELECT *;
 		FROM Difunto;
 		WHERE Difunto.ApePat = ALLTRIM(vAp) AND Difunto.ApeMat = ALLTRIM(vAm) AND Difunto.NomDif = ALLTRIM(vNom);
		ORDER BY ApePat,ApeMat,NomDif;
		INTO CURSOR xTbl

* 	SELECT Difunto.CodCem,Difunto.CodCuar,Difunto.Estado,CodDif,ApeDif,NomDif;
* 		FROM Difunto,Cuartel;
*		WHERE Difunto.CodCem+Difunto.CodCuar=Cuartel.CodCem+Cuartel.CodCuar and Difunto.ApeDif = ALLTRIM(vAp) AND Difunto.NomDif = ALLTRIM(vNom);
*		ORDER BY ApeDif,NomDif;
*		INTO CURSOR xTbl
	
	vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL F10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
		ApePat :H='A.Paterno':25,;
		ApeMat :H='A.Materno':25,;
		NomDif :H='Nombres':25
	
	vtempo = '????????????????????????????????????????????????????????????????????????????????'
	DO logos WITH rotulo1,vtempo
	
	IF lastKey()#27
		SELE Difunto
		IF !SEEK(xTbl.CodDif)
	    	DO StandBy WITH "Parametros de busqueda Erroneos"
		ENDIF
	ELSE
		DO StandBy WITH "Proceso Cancelado"
	ENDIF
	
ENDIF

DO VISTA
RETURN

*PROCEDURE busca              && Realiza b?squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo  '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre  '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		vCodCem = SPACE(2)
		vBusca = SPACE(8)
		vNombre = "Codigo :"
		SET ORDER TO TAG Difunto1
	CASE BAR() =  2
		vCodCem = SPACE(2)
		vBusca = SPACE(30)
		vNombre = "Apellidos : "
		SET ORDER TO TAG Difunto2
	OTHERWISE
		DO vista
		SET ORDER TO &cOrd1
		RETURN
ENDCASE

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
SELE Difunto
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
SELE Difunto
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

SELECT Difunto
SCATTER MEMVAR
DO pantalla

@  0, 60 SAY VAL_PARA(m.Estado,'ESTDIF',' ',60,20)
@  1, 26 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30)
@  1, 60 SAY VAL_PARA(m.EstVta,'ESTVTA',' ',60,20)
@  2, 26 GET m.CodDif	DISABLE
@  3, 26 GET m.ApePat	WHEN IIF(m.Reservado='S',.F.,.T.)	FUNCTION '!S23'
@  3, 50 GET m.ApeMat	WHEN IIF(m.Reservado='S',.F.,.T.)	FUNCTION '!S23'
@  4, 26 GET m.NomDif	WHEN IIF(m.Reservado='S',.F.,.T.)	FUNCTION '!S30'
@  5, 26 GET m.SexoDif	FUNCTION "M M,F" VALID VAL_PARA(m.SexoDif,'SEXO  ',' ',26,20)
@  6, 26 GET m.EdadDif	PICTURE '999'
@  6, 32 GET m.EdadMes	PICTURE '99'	VALID empty(m.edadMes) or m.edadMes < 12
@  6, 37 GET m.EdadDia	PICTURE '99'	VALID empty(m.edadDia) or m.edadDia < 31
@  7, 26 GET m.FecFall
@  8, 26 GET m.FecSep
@  9, 26 GET m.TipEnt	VALID VAL_PARA(m.TipEnt,'TIPENT',' ',26,15) DISABLE
@ 10, 26 GET m.CodCuar	WHEN m.TipEnt='2'		DISABLE
@ 11, 26 GET m.CodNic   WHEN m.TipEnt='1'		DISABLE
@ 12, 26 GET m.Fila     WHEN m.TipEnt='1'		DISABLE
@ 13, 26 GET m.Columna  WHEN m.TipEnt='1'		DISABLE
*@ 13, 26 GET m.CodMau	WHEN m.TipEnt='2'  AND m.Reservado = 'N' VALID Val_Mau(13,34)
@ 14, 26 GET m.CodMau	WHEN .F.
@ 15, 26 GET mLote		WHEN .F.
@ 16, 26 GET mUbic		WHEN .F.
@ 17, 26 GET m.CodCli	VALID IIF(EMPTY(m.Codcli),.T.,Val_Cli(m.CodCli,17,31))


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

SELECT Difunto
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT Difunto
vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK

IF escolor
	DEFINE POPUP xRes  FROM 17,55 SHADOW COLOR &L_COL
 ELSE
	DEFINE POPUP xRes  FROM 17,55 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF xRes PROMPT ' \<Sin Nicho reservado'
DEFINE BAR 2 OF xRes PROMPT ' \<Con Nicho Reservado'
ON SELECTION POPUP xRes  DEACTIVATE POPUP
ACTIVATE POPUP XRes

IF LASTKEY()#27
	DO CASE
		CASE BAR() = 1
			m.FecFall = DATE()
			m.FecSep = DATE()
			m.Estado = "00"
			m.EstVta = "00"
			m.Reservado = 'N'
			
			*PARCHE PARA FACILITAR LA CORRECCION. COJIENDO DATA DE IBAN
*			xx = Escoje_NOM()
			
*			IF !xx
*				RELEASE POPUP xRes
*				RETURN .T.
*			ENDIF
			
		CASE BAR() = 2
			xx = EscojeOF()
			IF !xx
				RELEASE POPUP xRes
				RETURN .T.
			ENDIF
			m.Reservado = 'S'
		OTHERWISE
	ENDCASE
	RELEASE POPUP xRes
 ELSE
	DO VISTA
	RETURN
ENDIF
****
DO pantalla

IF m.Reservado = 'N'
	@  0, 60 SAY VAL_PARA(m.Estado,'ESTDIF',' ',60,20)
	@  1, 26 GET m.CodCem	VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30) AND Cor_Dif()
	@  1, 60 SAY VAL_PARA(m.EstVta,'ESTVTA',' ',60,20)
	@  2, 26 GET m.CodDif	WHEN .F.
	@  3, 26 GET m.ApePat	FUNCTION '!S23'
	@  3, 50 GET m.ApeMat	FUNCTION '!S23'
	@  4, 26 GET m.NomDif	FUNCTION '!S30'
	@  5, 26 GET m.SexoDif	FUNCTION 'M M,F'	VALID VAL_PARA(m.SexoDif,'SEXO  ',' ',26,20)
	@  6, 26 GET m.EdadDif	PICTURE '999'
	@  6, 32 GET m.EdadMes	PICTURE '99'	VALID empty(m.edadMes) or m.edadMes < 12
	@  6, 37 GET m.EdadDia	PICTURE '99'	VALID empty(m.edadDia) or m.edadDia < 31
	@  7, 26 GET m.FecFall
	@  8, 26 GET m.FecSep
	@  9, 26 GET m.TipEnt	VALID VAL_PARA(m.TipEnt,'TIPENT',' ',26,15)	AND IniVar()
	@ 10, 26 GET m.CodCuar	WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_Cuar(10,31) AND Tab_Nic()
	@ 11, 26 GET m.CodNic	DISABLE
	@ 12, 26 GET m.Fila		WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_F()
	@ 13, 26 GET m.Columna	WHEN m.TipEnt='1'  AND m.Reservado = 'N' VALID Val_C()
	@ 14, 26 GET m.CodMau	WHEN m.TipEnt='2'  AND m.Reservado = 'N' VALID Val_Mau(14,34)
	@ 15, 26 GET mLote		WHEN .F.
	@ 16, 26 GET mUbic		WHEN .F.
	@ 17, 26 GET m.CodCli	VALID IIF(EMPTY(m.Codcli),.T.,Val_Cli(m.CodCli,17,31))
 ELSE
	@  0, 60 SAY VAL_PARA(m.Estado,'ESTDIF',' ',60,20)
	@  1, 26 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,26) AND Cor_Dif()
	@  1, 60 SAY VAL_PARA(m.EstVta,'ESTVTA',' ',60,20)
	@  2, 26 GET m.CodDif	WHEN .F.
	@  3, 26 GET m.ApePat	WHEN .F.
	@  3, 26 GET m.ApeMat	WHEN .F.
	@  4, 26 GET m.NomDif	WHEN .F.
	@  5, 26 GET m.SexoDif	FUNCTION "M M,F" VALID VAL_PARA(m.SexoDif,'SEXO  ',' ',26,20)
	@  6, 26 GET m.EdadDif	PICTURE '999'
	@  6, 32 GET m.EdadMes	PICTURE '99'	VALID empty(m.edadMes) or m.edadMes < 12
	@  6, 37 GET m.EdadDia	PICTURE '99'	VALID empty(m.edadDia) or m.edadDia < 31
	@  7, 26 GET m.FecFall
	@  8, 26 GET m.FecSep
	@  9, 26 SAY VAL_PARA(m.TipEnt,'TIPENT',' ',26,15)	AND IniVar()
	@ 10, 26 SAY m.codCuar
	@ 10, 31 SAY Val_Cuar(10,31)
	@ 11, 26 SAY m.CodNic
	@ 12, 26 SAY m.Fila
	@ 13, 26 SAY m.Columna
	@ 14, 26 SAY m.CodMau
	@ 15, 26 SAY mLote
	@ 16, 26 SAY mUbic
	@ 17, 26 SAY m.codcli
	@ 17, 31 SAY IIF(EMPTY(m.Codcli),.T.,Val_Cli(m.CodCli,17,33))
ENDIF

READ VALID val_read()

SELE Difunto

mAct = .T.

IF LASTKEY()#27
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
							REPLACE Estado WITH "20",CodDif WITH m.CodDif
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
		ENDIF
		
		SELE Parma
		SEEK "CORRELDIFUNT"+m.codcem
		
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
	DO vista
	RETURN
ENDIF

SELECT Difunto

FLUSH

DO vista
RETURN


*FUNCTION IniVar
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


*FUNCTION Val_Dif
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
*----------------
PARAMETERS _Fil,_Col
PRIVATE cali

cali = ALIAS()

SELE Cuartel

mLote = SPACE(30)
mUbic = SPACE(30)

IF SEEK(m.CodCem+m.codcuar)
	@ _Fil,_Col SAY ALLTRIM(NomCuar)
	mRet = .T.
 ELSE
	cOrd = ORDER()
	SET ORDER TO Cuartel2
	m.CodCuar = SPACE(4)
	SET FILTER TO CodCem=m.codcem
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD NomCuar		&& &v__bus
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx

	IF lastkey()#27
		m.CodCuar = Cuartel.CodCuar
		@ _Fil,_Col SAY m.CodCuar + "   " + Cuartel.NomCuar
		mRet = .T.
	ELSE
		mRet = .F.
	ENDIF
	SET ORDER TO &cOrd
	SET FILTER TO

*	mRet = Val_Fun('Cuartel','CodCuar','NomCuar',m.codCuar,1,_Fil,_Col)
*	SET FILTER TO
ENDIF

SHOW GET mLote
SHOW GET mUbic

SELE (cAli)

RETURN mRet


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

*FUNCTION Val_Mau
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


*FUNCTION Tab_Nic
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

*FUNCTION Val_F
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

*FUNCTION Val_C
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

lElimina = .T.
IF YesNo("Desea Eliminar este registro")
	IF !ve_passw("ELIDIF")
		DO STANDBY WITH "Proceso Cancelado"
		lElimina = .F.
	ENDIF
 ELSE
	DO STANDBY WITH "Proceso Cancelado"
	lElimina = .F.
ENDIF

IF estVta = '20'
	DO StandBy WITH "ESTE DIFUNTO TIENE UN COMPROBANTE DE PAGO. NO PUEDE ELIMINARSE"
	lElimina = .F.
ENDIF

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

DO VISTA
RETURN

PROCEDURE lista
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

DEFINE WINDOW LIS FROM 0,15 TO 23,70 FLOAT DOUBLE TITLE 'Listado de Difuntos' COLOR SCHEME 5
ACTIVATE WINDOW LIS

vToCem  = 1
vToCuar = 1
vToFila = 1
mCodCem  = SPACE(2)
mCodCuar = SPACE(4)
mApePat  = SPACE(30)
mApeMAt  = SPACE(30)
mFila    = SPACE(1)
mFecha1  = DATE()
mFecha2  = DATE()
lFec = .F.
lPat = .F.

@ 01,01 SAY "Todas los Cementerios : " GET vToCem  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 03,01 SAY "Cementerio : "
@ 03,15 GET mCodCem PICTURE '@!' WHEN vToCem = 2  VALID Val_Fun('Cement','CodCem','NomCem',mCodCem,1,3,19)
@ 05,01 SAY "Todas los Cuarteles : " GET vToCuar  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6 WHEN vToCem = 2
@ 07,01 SAY "Cuartel : "
@ 07,15 GET mCodCuar PICTURE '@!' WHEN vToCem = 2 AND vToCuar = 2 VALID VV_Cuar(7,15)
@ 09,01 SAY "Todas los Filas : " GET vToFila  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6 WHEN vToCem = 2 AND vToCuar = 2
@ 11,01 SAY "Fila : "
@ 11,15 GET mFila PICTURE '@!' WHEN vToCem = 2 AND vToCuar = 2 AND vToFila = 2  VALID VV_Fila(11,15)
@ 13,01 SAY "Considerar Fecha Fallec.?" GET lFec FUNCTION '*C'
@ 15,01 SAY "Fecha de Fallec : "
@ 15,20 GET mFecha1		WHEN lFec
@ 15,32 GET mFecha2		WHEN lFec
@ 17,01 SAY "Patron Busqueda?" GET lPat FUNCTION '*C'
@ 19,01 SAY "Apellidos Difunto? :" GET mApePat 	WHEN lPat	PICTURE '@!S15'
@ 19,38 GET mApeMat 	WHEN lPat	PICTURE '@!S15'

@ 21,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

IF OkCancel = 1 AND LASTKEY()#27
	SET FILTER TO CodCem = ALLTRIM(mCodCem) AND CodCuar = ALLTRIM(mcodcuar) AND Fila = ALLTRIM(mFila) AND IIF(lFec,BETW(FecFall,mFecha1,mFecha2),.T.) AND IIF(lPat,ApePat=ALLTRIM(mApePat),.T.)  AND IIF(lPat,ApeMat=ALLTRIM(mApeMat),.T.)
	DO REPORTE WITH 2,"Difuntos","Listado de Difuntos"
	SET FILTER TO
	SET RELATION TO
	SET INDEX TO
	SET ORDER TO Difunto1
	ERASE (vIdx)
 ELSE
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

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Ocupante '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Solicitante  '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

IF LASTKEY()#27
	DO CASE
		CASE BAR() =  1
			SET ORDER TO TAG OcupFut2
			xOpc = 1
		CASE BAR() =  2
			SET ORDER TO TAG OcupFut3
			xOpc = 2
	ENDCASE
	
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5
	
	DEFINE WINDOW wind_OF FROM 00,00 TO 23,79  DOUBLE ;
	TITLE "OCUPACIONES FUTURAS" FOOTER "[F10] Selecciona"  COLOR SCHEME 10
	
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
			
			SET RELATION TO CodCem INTO Cement
			SET RELATION TO CodCli INTO Clien	ADDITIVE
			
			ACTIVATE WINDOWS WIND_OF
			
			IF xOpc = 1
				BROWSE IN WINDOWS Wind_OF NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
						Cement.NomCem   :H='Cementerio' :30,;
						OcuFut.ApePatOc :H='Apell.Pat.Ocup.':20,;
						OcuFut.ApeMatOc :H='Apell.Mat.Ocup.':20,;
						OcuFut.NomOcu   :H='Nombre Ocup.'
			 ELSE
				BROWSE IN WINDOWS Wind_OF NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
						Cement.NomCem   :H='Cementerio' :30 ,;
						Clien.Nomcli    :H='Solicitante':20 ,;
						x = ALLTRIM(OcuFut.ApePatOc) + ' '+ ALLTRIM(OcuFut.ApeMatOc) + ' ' + ALLTRIM(OcuFut.NomOcu) :H='Ocupante'
			ENDIF
			
			DEACTIVATE WINDOWS WIND_OF
			
			SET RELATION TO
			
			vTempo = '????????????????????????????????????????????????????????????????????????????????'
			DO Logos WITH Rotulo1,vTempo
			IF LASTKEY()=27
				vFun = .F.
			 ELSE
				m.CodOCu	= OcuFut.CodOcu
				m.FecFall	= DATE()
				m.FecSep	= DATE()
				m.ApePat	= OcuFut.ApePatOc
				m.ApeMat	= OcuFut.ApeMatOc
				m.NomDif	= OcuFut.NomOcu
				m.SexoDif   = OcuFut.SexoOcu
				m.EdadDif   = OcuFut.EdadOcu
				m.EdadMes   = OcuFut.EdadOMe
				m.EdadDia   = OcuFut.EdadODi
				m.CodCem	= OcuFut.CodCem
				m.CodCuar	= OcuFut.CodCuar
				m.CodNic	= OcuFut.CodNic
				m.Fila		= OcuFut.Fila
				m.Columna	= OcuFut.Columna
				m.Estado	= "00"
				m.EstVta	= OcuFut.EstVta
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
ON KEY LABEL F10
SELE (cAlias)
RETURN vFun

PROCEDURE Escoje_NOM
*-------------------
PRIVATE cAlias,vFun
vFun = .F.
cAlias=ALIAS()
nReg = RECNO()

USE Y:\CEMENTER\Fallecid ORDER TAG Fallecid02 IN 0 ALIAS Fallecid

SELECT Fallecid


cOrd1 = ORDER()

IF LASTKEY()#27
	
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5
	
	GO TOP
	IF EOF()
		DO StandBy WITH "No Existen Registros Para Procesar"
		vFun = .F.
	 ELSE

		GO TOP
		IF EOF()
			DO StandBy WITH "No Existen Datos para Mostrar"
			vFun = .F.
		 ELSE
			vTempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
			DO Logos WITH Rotulo1,vTempo
			
			ON KEY LABEL F2  DO Busca_Nom
			ON KEY LABEL F10 KEYBOARD CHR(23)
			
				BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
						Ape_Patern   :H='A. Paterno' :20,;
						Ape_Matern   :H='A. Materno' :20,;
						Nombres      :H='Nombre Ocup.'
			
			vTempo = '????????????????????????????????????????????????????????????????????????????????'
			DO Logos WITH Rotulo1,vTempo
			IF LASTKEY()=27
				vFun = .F.
			 ELSE
				m.FecFall	= Fec_Fallec
				m.FecSep	= {//}
				m.ApeDif	= ALLTRIM(Ape_Patern)+ ' ' + ALLTRIM(Ape_Matern)
				m.NomDif	= Nombres
				m.SexoDif   = Sexo
				m.CodCem	= PADL(ALLTRIM(STR(Cod_Cement)),2,'0')
				m.CodCuar	= STR(Cod_Cuarte,4)
				m.Fila		= Fila
				m.Columna	= PADL(ALLTRIM(STR(Columna)),3,'0')
				m.Estado	= "00"
				m.TipEnt	= "1"
				vFun = .T.
			ENDIF
		ENDIF
	ENDIF
ENDIF

*SHOW MENU mMenu
SET FILTER TO
SET ORDER TO &cOrd1
ON KEY LABEL F10
USE IN Fallecid

SELE (cAlias)
RETURN vFun

FUNCTION Busca_Nom
*-----------------
PRIVATE vBusca
vBusca  = SPACE(30) 
vBusca1 = SPACE(30) 

DEFINE WINDOW Bus_Nom FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW Bus_Nom

@ 2,1 SAY "Apellidos P.: " GET vBusca
@ 3,1 SAY "Apellidos M.: " GET vBusca1

READ VALID val_read()

DEACTIVATE WINDOW Bus_Nom

IF EMPTY(vBusca) OR LASTKEY()=27
	* RETURN
 ELSE
	SEEK vBusca + vBusca1
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ENDIF
ENDIF

RETURN 

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
	BROWSE WINDOW _BusMau TITLE '???? [F10] Selecciona   [F2] Buscar ????   ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
		FIELDS;
			Familia	:H='Familia o Congregaci?n'
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
	BROWSE WINDOW _BusArt TITLE '???? [F10] Selecciona   [F2] Buscar ????   [F5] Agrega Art. ????' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
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

*FUNCTION Val_Mausol
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
	BROWSE WINDOW _BusArt TITLE '???? [F10] Selecciona   [F2] Buscar ????   [F5] Agrega Art. ????' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
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
PRIVATE mRet
IF m.Estado ='00'
	mRet = SPACE(15)
ENDIF
IF m.Estado ='30'
	mRet = "TRASLADADO"
ENDIF
RETURN mRet

FUNCTION EstVta
*--------------
PRIVATE mRet
DO CASE
	CASE m.EstVta ='00'
		mRet = SPACE(15)
	CASE m.EstVta ='10'
		mRet = "Cancelado"
	CASE m.EstVta ='20'
		mRet = "Con Saldo"
	CASE m.EstVta ='99'
		mRet = "Anulado"
ENDCASE
RETURN mRet

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0029","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0030","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0031","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0032","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0033","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0034","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0035","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0036","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
