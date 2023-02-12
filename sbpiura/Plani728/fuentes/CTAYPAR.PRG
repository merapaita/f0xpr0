** --------------------------------------------------------
** CtayPar
** Cuentas y Partidas para resumen de Planillas de Personal
** AUTOR: 
** --------------------------------------------------------
*- Guarda el menu

HIDE POPUP pop_01,menu
*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09,vMens20
Vmens01 = ' Mantenimiento de Cuentas y Partidas '
Vmens08 = 'No hay registros para procesar'
Vmens09 = ' °Detalle° '
Vmens20 = ''

xPos    = INT((80 - (LEN(ALLTRIM(cia)) + 2))/2)
*Vmens01 = SUBSTR(Vmens01, 1, xPos+1) + ' ' + cia + ' ' + SUBSTR(Vmens01,xPos + LEN(ALLTRIM(cia)) + 3,80)

* Abrir Archivos de trabajo

CLOS DATA
USE Parmae  IN 1 ORDER TAG Parmae1  ALIAS Parma
USE Cuentas IN 2 ORDER TAG Cuentas1 ALIAS Cuenta
USE Clase   IN 3 ORDER TAG Clase2	ALIAS Partida

SELE Cuenta
SET FILTER TO Cuenta="17" OR Cuenta="40" OR Cuenta="41" OR Cuenta="42"
SELE Partida
SET FILTER TO CodPart="51" OR CodPart="52" 
SELE Parma
SET FILTER TO Tipo="AUMLEY" OR Tipo="DESLEY"
GO BOTT

DO Inicia
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
   ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion
SHOW POPUP Menu

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
* vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
* DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_01 FROM 00,00 TO 23,78 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_02 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 23,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 23,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 23,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 23,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 23,36
* DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 23,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 23,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 23,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 23,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
* ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO Elimi
 ON SELECTION PAD lista  OF mMenu DO Lista 
 ON SELECTION PAD termi  OF mMenu DO termi
 
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_01
 CLEAR
 
 @  1,05 SAY "         Tipo:"
 @ 03,05 SAY "       Codigo:" 
 @ 05,05 SAY "      Partida:" 
 @ 07,05 SAY "       Cuenta:" 


RETURN

PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
DO Pantalla
SCATTER MEMVAR
m.Descriaux = ALLTRIM(m.Descriaux)
m.Descriau2 = ALLTRIM(m.Descriau2)
@  1,21 SAY IIF(m.Tipo="AUMLEY","Aumento","Descuento")
@  3,21 SAY m.Codigo
@  3,25 SAY m.Descri
@  5,21 SAY m.Descriaux
@  5,28 SAY IIF(!EMPTY(m.Descriaux),Val_Fun("Partida","CodPart","CodPart+' '+DesPar",m.Descriaux,1,5,21),"")
@  7,21 SAY m.Descriau2
@  7,32 SAY IIF(!EMPTY(m.Descriau2),Val_Fun("Cuenta","Cuenta","Cuenta+' '+Descri",m.Descriau2,1,7,21),"")

RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT Parma
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
Vtemp = RECNO()
HIDE MENU mMenu

ON KEY LABEL F10 KEYBOARD CHR(23)

BROWSE WINDOW Wind_02;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   X=IIF(Tipo="AUMLEY","Aumento","Descuento")   :H= 'Tipo.',;
   Codigo			:H= 'Codigo'          ,;
   Descri			:H= 'Descripcion'  :20,;
   DescriAux		:H= 'Partida'      :06,; 
   DescriAu2		:H= 'Cuenta'       :10


IF LASTKEY()=27
	GO vTemp
ENDIF

ON KEY LABEL F10

DEACTIVATE WINDOW Wind_01

DO Vista
RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*---------------
PRIVATE vCodigo,vTipo
STORE SPACE(1) TO vTipo
STORE SPACE(2) TO vCodigo
SELECT Parma
IF EOF()
	DO standby WITH VMens08
	RETURN
ENDIF
IF LASTKEY()=27
	RETURN
ENDIF

Vtemp = RECNO()
DEFINE WINDOW WBusca FROM 20,20 TO 24,75 DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW WBusca

@ 1, 05 SAY 'Tipo:' GET vTipo   PICTURE '!' VALID val_para(vTipo,"TIPPLA","S",25)
@ 1, 22 SAY ' Codigo:' GET vCodigo   PICTURE '!!'
READ 
RELEASE WINDOW WBusca
IF LASTKEY()=27
	SELECT Parma
	RETURN
ENDIF
xBusca= IIF(vTipo=' ',"",IIF(vTipo='1',"AUMLEY","DESLEY"))+ALLTRIM(vCodigo)
IF !SEEK(xBusca)
	DO standby WITH "Error en la Busqueda Por favor intente de Nuevo."
	GOTO Vtemp
 ELSE
	DO Vista
ENDIF
RETURN


PROCEDURE Anter              && Documento anterior
*--------------
SELECT Parma
IF EOF()
	DO standby WITH VMens08
	RETURN
ENDIF

IF !BOF()
	SKIP -1
ENDIF

IF BOF()
	GO TOP
	DO standby WITH "No existe Documento anterior."
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Proxi              && Documento siguiente
*--------------
SELECT Parma
IF EOF()
	DO standby WITH VMens08
	RETURN
ENDIF

IF !EOF()
	SKIP
ENDIF

IF EOF()
	DO standby WITH "No existe Documento siguiente."
	GO BOTTOM
 ELSE
	DO Vista
ENDIF
RETURN

PROCEDURE Corri
*--------------
DO Pantalla
SELECT Parma
nReg = RECNO()
SCATTER MEMVAR
m.Descriaux = IIF(EMPTY(m.Descriaux),SPACE(6),ALLTRIM(m.Descriaux))
m.Descriau2 = IIF(EMPTY(m.Descriau2),SPACE(10),ALLTRIM(m.Descriau2))
@  1,21 SAY IIF(m.Tipo="AUMLEY","Aumento","Descuento")
@  3,21 SAY m.Codigo
@  3,25 SAY m.Descri
@  5,21 GET m.Descriaux VALID Val_Des()
@  7,21 GET m.Descriau2 VALID Val_Des2()

READ VALID Val_Read()

IF LASTKEY()=27
	DO STANDBY WITH "Proceso Cancelado. No se graba el Documento"
	GO nReg
 ELSE
	SELE Parma
	GATHER MEMVAR
ENDIF
DO Vista

RETURN


PROCEDURE Ingre
*--------------
DO Pantalla
SELECT RepFin
nReg = RECNO()
SCATTER MEMVAR BLANK
@  1,21 GET m.CodRep VALID val_para(m.CodRep,"FORMAT","A",16)
@  3,21 GET m.Periodo
@  5,21 GET m.NumMes VALID val_para(m.NumMes,"FECMES","A",16)
@  7,21 GET m.Fila
@  9,21 GET m.Columna
@ 11,21 GET m.Cuenta VALID Val_Fun("Cuentas","Cuenta","Cuenta+' '+Descri",m.Cuenta,1,11,21)
@ 13,21 GET m.TotDeb
@ 15,21 GET m.TotHab 

READ VALID Val_Read()

IF LASTKEY()=27
	DO STANDBY WITH "Proceso Cancelado. No se graba el Documento"
	GO nReg
 ELSE
	SELE Parma
	GATHER MEMVAR
ENDIF
DO Vista

RETURN


PROCEDURE Elimi
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF

IF yesno("¨Desea eliminar este Item ?")
	vOrd=ORDER()
	SELECT RepFin
	IF f_lock(1)
		DELETE
		IF !BOF()
			SKIP -1
		 ELSE
			IF !EOF()
				SKIP
			ENDIF
		ENDIF
	ENDIF
ENDIF
UNLOCK ALL
DO Vista
RETURN




PROCEDURE Lista
*--------------
PRIVATE vFormat,cPeriodo,cMes
STORE SPACE(5) TO vFormat
STORE SPACE(4) TO cPeriodo
STORE SPACE(2) TO cMes
OkCancel = 1
DEFINE WINDOW Lis FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Detralles de Estados Financieros' COLOR SCHEME 5
ACTIVATE WINDOW Lis
@ 01,01 SAY '    Formato :' GET vformat    VALID val_para(vformat,"FORMAT"," ",16)
@ 03,01 SAY "    Periodo :" GET cPeriodo   PICTURE '!!!!' 
@ 06,01 SAY "        Mes :" GET cMes       PICTURE '!!'

@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

IF OkCancel = 1
	SET FILTER TO ALLTRIM(RepFin.CodRep)=ALLTRIM(vFormat) AND RepFin.Periodo=ALLTRIM(cPeriodo) AND RepFin.NumMes=ALLTRIM(cMes)
	SUSP
	DO CASE
		CASE vFormat="F-1"
			cRep = "DetF1"
		CASE vFormat="F-2"
			cRep = "DetF2"
	ENDCASE
	DO REPORTE WITH 2,cRep," Consistencia de Documentos ",2,.T.,.T.
ENDIF
RETURN

PROCEDURE Termi
*--------------
 vEn_accion = .F.
 DEACTIVATE MENU
RETURN

PROCEDURE Fin_opcion
*-------------------
 DO Logos WITH Rotulo1,Rotulo2
 RELEASE    MENU   mMenu
 RELEASE WINDOW Wind_01
 RELEASE WINDOW Wind_02
 CLOSE DATA

RETURN

FUNCTION Val_Des
*---------------
PRIVATE cAlias
cAlias = ALIAS()
IF !EMPTY(m.Descriaux)
	SELE Partida
	IF SEEK(m.Descriaux)
		= Val_Fun("Partida","CodPart","CodPart+' '+DesPar",m.Descriaux,1,05,21)
	 ELSE
		m.Descriaux = "      "
		= Val_Fun("Partida","CodPart","CodPart+' '+DesPar",m.Descriaux,1,05,21)
	ENDIF
 ELSE
	m.Descriaux = "      "
ENDIF
SELE (cAlias)
RETURN 

FUNCTION Val_Des2
*---------------
PRIVATE cAlias
cAlias = ALIAS()
IF !EMPTY(m.Descriau2)
	SELE Cuenta
	IF SEEK(m.Descriaux)
		=Val_Fun("Cuenta","Cuenta","Cuenta+' '+Descri",m.Descriau2,1,07,21)
	 ELSE
		m.Descriau2 = "          "
		=Val_Fun("Cuenta","Cuenta",  "Cuenta+' '+Descri",m.Descriau2,1,07,21)
	ENDIF
 ELSE
	m.Descriau2 = "          "
ENDIF
SELE (cAlias)
RETURN