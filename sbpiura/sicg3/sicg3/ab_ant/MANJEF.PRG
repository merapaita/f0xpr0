*--------------------------------------------------------------------
*- ManJef.prg
*- Mantenimiento general de par metros del sistema (del CDRNEC.DBF)
*- Actualiza los nombres de las Jefaturas de Oficinas
*--------------------------------------------------------------------
PARAMETER mTipo,pantalla

SAVE SCREEN TO pantalla

USE Parmae   IN  2 ORDER Parmae1 ALIAS parma
USE Cdrnec   IN  1 ORDER cdrnec1 ALIAS cdr

*set relation to coddep into  cdr additive
mperiodo= RIGHT(DTOC(DATE()),2)
*- Variables de trabajo (registro a trabajar)
PUBLIC mCodigo, mDescri1, mAux1, mAux2,mDescri2

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 vTitulo = 'Tabla de Jefaturas '
 vTitul2 = 'Revisi¢n de Jefaturas     Presione F10 para concluir          '

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE vTitulo COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 24,79 DOUBLE ;
 TITLE vTitul2 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  3,20 SAY "Codigo  : "
 @  5,20 SAY "Oficina : "
 @  7,20 say "Jefatura: "	
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT cdr
 SCATTER MEMVAR
 
 @  3,32 SAY m.CodDep
 @  5,32 SAY Val_Para(m.CodDep,'CODDEP','A',32)
 @  7,32 SAY m.Atte
 RETURN


PROCEDURE Revis
*--------------
SELEC PARMA
SET FILTER TO TIPO="CODDEP"
SELECT cdr
IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
ENDIF
ACTIVATE SCREEN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOWS Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
    cdr.Coddep    :H='C¢digo',;
    cdr.Atte	  :H='Jefe '
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca
*--------------
SELECT cdr
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

Vtemp = RECNO()
DEFINE WINDOW WBusca FROM 14,12 TO 17,68 DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW WBusca
STORE SPACE(6) TO Vbusca
@ 1,3 SAY "Dependencia que desea    : " GET Vbusca PICT '@!' COLOR SCHEME 5
READ
RELEASE WINDOW WBusca
IF EMPTY(Vbusca) .OR. LASTKEY()=27
	GOTO Vtemp
 ELSE
	SEEK mperiodo+Vbusca
	IF !FOUND()
		DO standby WITH "Dependencia "+ vbusca +" no Existe."
		GOTO Vtemp
	 ELSE
		DO Vista
	ENDIF
ENDIF
RETURN


PROCEDURE Anter
*--------------
 SELECT  cdr
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 IF !BOF()
    SKIP -1
 ENDIF
 IF BOF()
    GO TOP
    DO standby WITH "Inicio de archivo: no existe &mTipo anterior."
 ELSE
    DO Vista
 ENDIF
RETURN
 

PROCEDURE Proxi
*--------------
 SELECT cdr
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 IF !EOF()
    SKIP
 ENDIF
 IF EOF()
    DO standby WITH "Fin de archivo: no existe Dependencia siguiente."
    GO BOTTOM
 ELSE
    DO Vista
 ENDIF

 RETURN


PROCEDURE Corri
*--------------
SELECT cdr
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF
SCATTER MEMVAR
ACTIVATE WINDOW Wind_0
@  3,32 SAY  m.Coddep         && Campo que forma llave primaria
@  5,32 say  Val_Para(m.CodDep,'CODDEP','A',32)
@  7,32 GET  m.Atte
READ
IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
	IF f_lock(1)
		GATHER MEMVAR
		UNLOCK
	ENDIF
ENDIF
DO Vista                    && Muestra nuevos datos
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
*- Aqu¡ coloca valores por omisi¢n (default)
ACTIVATE WINDOW Wind_0
SELECT cdr
SCATTER MEMVAR BLANK
m.Periodo = RIGHT(PADL(YEAR(DATE()),4),2)
@ 3,32 GET m.CodDep   VALID val_para(m.CodDep,'CODDEP',' ',32,30) AND ValCod() PICTURE '@!'
@ 5,32 say Val_Para(m.CodDep,'CODDEP','A',32)
@ 7,32 GET m.atte
READ
IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
	IF f_Appd()
		GATHER MEMVAR
		UNLOCK
	ENDIF
ENDIF
DO Vista                    && Muestra nuevos datos

RETURN

FUNCTION ValCod
*---------------
PRIVATE nReg
IF SEEK(m.Periodo+m.Coddep)
	DO standby WITH "ElCodigo ya Existe."
	return .F.
 ELSE
	Return .T.
ENDIF


PROCEDURE Elimi
*--------------
SELECT cdr
IF EOF()
	DO standby WITH "Archivo Vac¡o. No hay registros para procesar."
	RETURN
ENDIF
STORE YESNO("¨Est  seguro que desea ELIMINAR ? ") TO vElimina
IF vElimina
	IF f_lock(1)
		DELETE NEXT 1
		IF !BOF()
			SKIP -1
		ENDIF
	ENDIF
ENDIF
DO Vista
RETURN


PROCEDURE Lista
*--------------
SELECT cdr
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
 ELSE
	*GO TOP
	*do reporte with 2,"ManPar7","CATALOGO DE JEFATURAS "
	WAIT WINDOWS "Proceso en Implementaci¢n"
	RETURN
ENDIF


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  SELECT     cdr
  SET FILTER TO
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    MENU   mMenu
  RESTORE    SCREEN FROM Pantalla
  CLOSE DATA

  RETURN


FUNCTION Asg_Val
*---------------
PARAMETER Tipman, Otro

   PRIVATE v_fun
   DO CASE
   CASE TipMan = 1
      v_fun = Descri
   CASE TipMan = 2
      v_fun = DescriAux
   CASE TipMan = 3
      v_fun = Fecha
   CASE TipMan = 4 OR Tipman=8
      v_fun = NumEnt
   CASE TipMan = 5
      v_fun = NumDec
   CASE TipMan = 6 .AND. Otro=1
      v_fun = NumEnt
   CASE TipMan = 6 .AND. Otro=2
      v_fun = Fecha
   CASE TipMan = 7
      v_fun = CodigoAux
   ENDCASE

RETURN v_fun

PROCEDURE BUSCPRG
*----------------
PARAMETER VCODIGO
CV=ALLTRIM(VCODIGO)
AX=PRG_&CV
RETURN AX

PROCEDURE BUSCA1
*--------------
SELEC PARMA
SET FILTER TO TIPO="CODDEP" TO PARX
set order to probar2
SEEK MCODIGO
if !EOF()
	MDESCRI1=DESCRI
else
	MDESCRI1="Dependencia NO TIENE NOMBRE"
endif
SELEC CDR
RETURN
