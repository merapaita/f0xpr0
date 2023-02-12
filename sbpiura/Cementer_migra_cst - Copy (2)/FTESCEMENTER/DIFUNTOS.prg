*------------------------------------
* Difuntos.Prg
* Opcion para Ingresar difuntos en 
* el sistema de iban
* 
* AUTOR : A. S. Marco Rodriguez Abad
*------------------------------------
*- Abriendo Archivos

PUBLIC worker,Rotulo1

DO ambiente
SET PROCEDURE TO LIBSIS
*SET PATH TO Y:\CEMENTER

CLOS DATA
USE Fallecid IN  1 ORDER TAG Fallecid03	ALIAS Fallecid
USE Cuartel  IN  2 ORDER TAG cuartel01  ALIAS Cuartel
USE Maestro  IN  3 ORDER TAG maestro1	ALIAS Maestro
USE UltiFall IN  4
USE UltiFal2 IN  5
*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Difuntos (Velaciones)'
vmens02 = ' Difuntos : REVISION '
vmens03 = ' DETALLES '
vmens04 = 'Dicho Difunto no fue encontrado'
vmens05 = 'No existe Difunto anterior'
vmens06 = 'No existe Difunto siguiente'
vmens07 = '¨ Desea Anular ‚ste Difunto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Difunto ha sido anulado'
vmens10 = 'El Difunto ya est  Atendido'
vmens11 = 'El Difunto ha sido devuelto'

SELECT Fallecid
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
	TITLE vmens01 COLOR rgb(192,192,192,0,0,0)		&&SCHEME 10

DEFINE MENU mmenu COLOR rgb(192,192,192,192,192,192)		&& CHEME 3

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

 @  1, 3 SAY "       Cementerio :"
 @  2, 2 SAY "Difunto (Ap Am No) :"
 @  3, 3 SAY "          Cuartel :"
 @  4, 3 SAY "      Nicho (F/C) :"
 @  5, 3 SAY " Apellido Paterno :"
 @  6, 3 SAY " Apellido Materno :"
 @  7, 3 SAY "          Nombres :"
 @  8, 3 SAY "  Fecha Fallecim. :"
 @  9, 3 SAY " Fecha Nacimiento :"
 @ 10, 3 SAY "       Sexo (M/F) :"
 @ 11, 3 SAY " Adulto / P rvulo :"
 @ 12, 3 SAY "      Solicitante :"
 @ 13, 3 SAY "    Observaciones :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Fallecid
DO pantalla

IF EOF()
*	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR MEMO
 @  1, 23 SAY Val_Ce(STR(m.Cod_Cement,1))									&&m.Cod_Cement
 @  2, 23 SAY Val_Fall(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))		&&m.Cod_Fallec
 @  3, 23 SAY Val_Cuar(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))		&&m.Cod_Cuarte
 @  4, 23 SAY Val_Nic(STR(m.Cod_Cement,1) + PADL(m.Cod_Fallec,6,'0'))		&&m.Fila
* @  4, 31 SAY m.Columna
 @  5, 23 SAY m.Ape_patern
 @  6, 23 SAY m.Ape_matern
 @  7, 23 SAY m.Nombres
 @  8, 23 SAY m.Fec_Fallec
 @  9, 23 SAY m.Fec_Nacim
 @ 10, 23 SAY m.Sexo
 @ 11, 23 SAY m.Adul_parv
 @ 12, 23 SAY m.Solicitant
 @ 13, 23 SAY m.Antec_fami

RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
PRIVATE cOrd
SELE Fallecid
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

cOrd = ORDER()

DEFINE POPUP Busmenu FROM 15,50  SHADOW COLOR  rgb(192,192,192,192,192,192)		&&4
DEFINE BAR 1 OF Busmenu PROMPT ' ordenado por: Ap. Paterno   '
DEFINE BAR 2 OF Busmenu PROMPT ' ordenado por: Ap. Materno   '
DEFINE BAR 3 OF Busmenu PROMPT ' ordenado por: Nombre   '
ON SELECTION POPUP Busmenu DEACTIVATE POPUP
ACTIVATE POPUP Busmenu
DO CASE
    CASE BAR()=1
         SET ORDER TO Fallecid02
    CASE BAR()=2
         SET ORDER TO Fallecid05
    CASE BAR()=3
         SET ORDER TO Fallecid06
    OTHERWISE
         SET ORDE TO (cOrd)
         DO VISTA
         RETURN
ENDCASE
IF LASTKEY()=27
   SET ORDE TO (cOrd)
   DO VISTA
   RETURN
ENDIF

vTemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Ape_patern :H='A.Paterno':25,;
	Ape_Matern :H='A.Materno':25,;
	Nombres    :H='Nombres':25

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Fallecid
SET ORDER TO (cOrd)
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
PRIVATE cOrd,vTemp,vAP,vAM,vNom
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = RECNO()
vAP   = SPACE(30)
vAM   = SPACE(30)
vNom  = SPACE(30)

cOrd  = ORDER()
lBusca = .F.
*SET ORDER TO Fallecid02

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR  rgb(192,192,192,192,192,192)		&&SCHEME 5

ACTIVATE WINDOW lista
@ 1,2 SAY 'A.Paterno :' GET vAp		color rgb(255,255,0,0,0,128)
@ 2,2 SAY 'A.Materno :' GET vAm		color rgb(255,255,0,0,0,128)
@ 3,2 SAY '  Nombres :' GET vNom	color rgb(255,255,0,0,0,128)

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vAp+vAm+vNom) OR LASTKEY()=27
    SET ORDER TO (cOrd)
	RETURN
 ELSE
 	SELECT *;
 		FROM Fallecid;
 		WHERE Fallecid.Ape_Patern = ALLTRIM(vAp) AND Fallecid.Ape_Matern = ALLTRIM(vAm) AND Nombres = ALLTRIM(vNom);
		ORDER BY Ape_Patern,Ape_Matern,Nombres;
		INTO CURSOR xTbl
		
	vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL F10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
		Ape_patern :H='A.Paterno':25,;
		Ape_Matern :H='A.Materno':25,;
		Nombres    :H='Nombres':25
		
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	
	IF lastKey()#27
		SELE Fallecid
		IF !SEEK(STR(xTbl.Cod_Cement,1)+PADL(ALLTRIM(STR(xTbl.Cod_Fallec)),6,'0'))
	    	DO StandBy WITH "Parametros de busqueda Erroneos"
		ENDIF
	ELSE
		DO StandBy WITH "Proceso Cancelado"
	ENDIF
	
ENDIF

*IF !(FOUND() and lBusca)
*	DO standby WITH vmens04
*	GOTO vtemp
*ENDIF

*SET ORDER TO (cOrd)
DO VISTA
RETURN


*PROCEDURE busca              && Realiza b£squeda directa
*--------------
PRIVATE cOrd,vTemp,vAP,vAM,vNom
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = RECNO()
vAP   = SPACE(30)
vAM   = SPACE(30)
vNom  = SPACE(30)

cOrd  = ORDER()
lBusca = .F.
SET ORDER TO Fallecid02

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR  rgb(192,192,192,192,192,192)		&&SCHEME 5

ACTIVATE WINDOW lista
@ 1,2 SAY 'A.Paterno :' GET vAp		color rgb(255,255,0,0,0,128)
@ 2,2 SAY 'A.Materno :' GET vAm		color rgb(255,255,0,0,0,128)
@ 3,2 SAY '  Nombres :' GET vNom	color rgb(255,255,0,0,0,128)

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vAp+vAm+vNom) OR LASTKEY()=27
    SET ORDER TO (cOrd)
	RETURN
 ELSE
	DO CASE
    	CASE (!EMPTY(vAp) AND !EMPTY(vAm) AND !EMPTY(vNom)) OR (!EMPTY(vAp) AND !EMPTY(vAm)) OR (!EMPTY(vAp) AND EMPTY(vAM) AND EMPTY(vNom))
        	 SET ORDER TO Fallecid02
			 SEEK RTRIM(vAp+vAm+vNom)
			 lBusca = .T.
	    CASE !EMPTY(vAm) AND EMPTY(vAp+vNom)
    	     SET ORDER TO Fallecid05
			 SEEK ALLTRIM(vAm)
			 lBusca = .T.
	    CASE !EMPTY(vNom) AND EMPTY(vAp+vAm)
    	     SET ORDER TO Fallecid06
			 SEEK ALLTRIM(vNom)
			 lBusca = .T.
	    OTHERWISE
	    	DO StandBy WITH "Parametros de busqueda Erroneos"
	ENDCASE     
ENDIF

IF !(FOUND() and lBusca)
	DO standby WITH vmens04
	GOTO vtemp
ENDIF

SET ORDER TO (cOrd)
DO VISTA
RETURN

PROCEDURE anter
*--------------
SELE Fallecid
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
SELE Fallecid
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
SELE Fallecid
vtemp = Iif(!EOF(),RECNO(),-1)

SCATTER MEMVAR BLANK

DO pantalla

*m.Estado = "00"
m.cod_cement = SPACE(1)
m.Cod_Cuarte = SPACE(3)
m.Columna    = SPACE(3)
m.Antec_fami = SPACE(200)

@  1,23 GET m.Cod_Cement	PICTURE "9"		VALID Val_Ce1(m.Cod_Cement,' ',28)				color rgb(255,255,0,0,0,128) && AND CamCod("m.Cod_Cement")			&&Val_Fun('Maestro','Cod_Cement','Nom_Cement',@m.Cod_Cement,1,1,34)		&& AND Val_Cuar()
*@  2,23 GET m.Ape_Patern	FUNCTION "S15"
*@  2,40 GET m.Ape_matern	FUNCTION "S15"
*@  2,57 GET m.Nombres		FUNCTION "S15"
@  3,23 GET m.Cod_Cuarte	PICTURE "999"	VALID Val_Cu1(m.Cod_Cement+m.Cod_Cuarte,' ',28)	color rgb(255,255,0,0,0,128)				&&AND CamCod("m.Cod_Cuarte")
@  4,23 GET m.Fila			PICTURE "@!"													color rgb(255,255,0,0,0,128)
@  4,25 SAY "/" 
@  4,27 GET m.Columna		PICTURE "999"	VALID Lle_col()		color rgb(255,255,0,0,0,128)
@  5,23 GET m.Ape_patern	PICTURE "@!"						color rgb(255,255,0,0,0,128)
@  6,23 GET m.Ape_matern	PICTURE "@!"						color rgb(255,255,0,0,0,128)
@  7,23 GET m.Nombres		PICTURE "@!"						color rgb(255,255,0,0,0,128)
@  8,23 GET m.Fec_Fallec										color rgb(255,255,0,0,0,128)
@  9,23 GET m.Fec_Nacim											color rgb(255,255,0,0,0,128)
@ 10,23 GET m.Sexo			PICTURE "@M M,F"					color rgb(255,255,0,0,0,128)
@ 11,23 GET m.Adul_parv		PICTURE "@M A,P"					color rgb(255,255,0,0,0,128)
@ 12,23 GET m.Solicitant										color rgb(255,255,0,0,0,128)
@ 13,23 GET m.Antec_fami	FUNCTION "S50"						color rgb(255,255,0,0,0,128)

READ VALID val_read()

IF LASTKEY()#27
	IF F_Appd()
		DO CASE
			CASE m.Cod_Cement = "1"
				m.Cod_Fallec = UltiFall.Ultimo
				GATHER MEMVAR MEMO
				REPLACE UltiFall.Ultimo WITH UltiFall.ultimo + 1
			CASE m.Cod_Cement = "2"
				m.Cod_Fallec = UltiFal2.Ultimo
				GATHER MEMVAR MEMO
				REPLACE UltiFal2.Ultimo WITH UltiFal2.ultimo + 1
			OTHERWISE
				DO STANDBY WITH "ERROR. Con correlativos. Avise al Area de sistemas."
		ENDCASE
	ENDIF
 ELSE
	IF vTemp=-1
		GO TOP
	 ELSE
		go vtemp
	ENDIF
ENDIF

UNLOCK ALL

FLUSH

SELECT Fallecid

DO vista
RETURN

PROCEDURE corri
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SELECT Fallecid
SCATTER MEMVAR MEMO
DO pantalla

m.Cod_Cement = STR(m.Cod_cement,1)
m.Cod_Cuarte = PADL(ALLTRIM(STR(m.Cod_Cuarte)),3,'0')
m.Columna    = PADL(ALLTRIM(STR(m.Columna)),3,'0')
*m.Antec_fami = SPACE(200)

@  1,23 GET m.Cod_Cement	PICTURE "9"		WHEN .F. 
@  1,23 SAY Val_Ce1(m.Cod_Cement,' ',28)
*@  2,23 GET m.Ape_Patern	FUNCTION "S15"
*@  2,40 GET m.Ape_matern	FUNCTION "S15"
*@  2,57 GET m.Nombres		FUNCTION "S15"
@  3,23 GET m.Cod_Cuarte	PICTURE "999"	WHEN .F.
@  3,23 SAY Val_Cu1(m.Cod_Cement+m.Cod_Cuarte,' ',28)
@  4,23 GET m.Fila
@  4,25 SAY "/" 
@  4,27 GET m.Columna		PICTURE "999"	VALID Lle_col()
@  5,23 GET m.Ape_patern
@  6,23 GET m.Ape_matern
@  7,23 GET m.Nombres
@  8,23 GET m.Fec_Fallec
@  9,23 GET m.Fec_Nacim
@ 10,23 GET m.Sexo			PICTURE "@M M,F"
@ 11,23 GET m.Adul_parv		PICTURE "@M A,P"
@ 12,23 GET m.Solicitant
@ 13,23 GET m.Antec_fami	FUNCTION "S50"

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT Fallecid
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT Fallecid
DO vista
RETURN

FUNCTION CamCod
*--------------
PARAMETERS xVar
DO CASE
	CASE xVar = "m.Cod_Cement"
		m.Cod_Cement = STR(m.Cod_Cement,1)
	CASE xVar = "m.Cod_Cuarte"
		m.Cod_Cuarte = PADL(ALLTRIM(STR(m.Cod_Cuarte)),3,'0')
ENDCASE

RETURN .T.

PROCEDURE Lle_col
*----------------
m.Columna = PADL(ALLTRIM(m.Columna),3,'0')
RETURN

PROCEDURE elimi
*--------------
SELECT Fallecid
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

IF yesno('¨ Desea ELIMINAR ‚ste Difunto ?')
	lBorra = .T.
	DELE NEXT 1
	SKIP -1
	IF BOF()
		GO TOP
	ENDIF
 ELSE
	DO STANDBY WITH "No Puede Borrar este Cuartel por que tiene Nichos ocupados"
ENDIF

UNLOCK ALL
SELECT Fallecid
DO vista
RETURN

PROCEDURE lista
*--------------
RETURN
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
	SET FILTER TO BETW(Fec_Recibo,mFecha1,mFecha2)
	GO TOP
	DO Espera WITH 2
	IF !EOF()
		@5,5 get pantimp function '*H' picture 'Impresora;Pantalla' size 1,11,8 default 'Pantalla'
		read
		if pantimp = 'Pantalla'
			REPORT FORM Ventas PREVIEW
		else
			REPORT FORM Ventas to print prompt
		endif
	 ELSE
		DO StandBy WITH "No Existe Informaci¢n para Procesar."
	ENDIF
	SET FILTER TO
 ELSE
	DO Espera WITH 2
	DO StandBy WITH "Proceso Cancelado."
ENDIF

RELEASE WINDOW wLista

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




*    1  COD_CEMENT  Num‚rico       2                  
*    3  COD_FALLEC  Num‚rico       7                  
*    2  COD_CUARTE  Num‚rico       3                  
*    5  FILA        Car cter       1                  
*    4  COLUMNA     Num‚rico       3                  
*    6  APE_PATERN  Car cter      30                  
*    7  APE_MATERN  Car cter      30                  
*    8  NOMBRES     Car cter      30                  
*    9  FEC_FALLEC  Fecha          8                  
*   10  FEC_NACIM   Fecha          8                  
*   17  SEXO        Car cter       1                  
*   28  ADUL_PARV   Car cter       1                  
*   22  SOLICITANT  Car cter      35                  
*   19  ANTEC_FAMI  Memo          10                  
