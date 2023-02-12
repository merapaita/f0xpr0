*--------------------------------------------------------------------
*- ManPar1.prg
*- Mantenimiento general de par metros del sistema (del Parmae.DBF)
*- Marco Rodriguez Abad
*- Marzo 2001
*--------------------------------------------------------------------
*DO Val_Cod
*RETURN
PARAMETER cTitulo,cTipo,cCodigo,cCodigoAux,cDescri,cDescriaux,cDescriAu2,cNumDec,cNumEnt,cFecha

* cTitulo   : Titulo de la Pantalla
* mTipo     : Filtro de Parmae : TIPO
* cCodigo   : Etiqueta del :Codigo
* cCodigoAux: Etiqueta del Codigo auxiliar :CodigoAux
* cDescri   : Etiqueta de Descri
* cDescriAux: Etiqueta de Descripcion Auxiliar :DescriAux
* cDescriAu2: Etiqueta de Descripcion Auxiliar 2 :DescriAu2
* cNunDec   : Etiqueta de Numeros Decimales :NumDec
* cNumEnt   : Etiqueta de Numeros Enteros :NumEnt
* cFecha    : Etiqueta de Fecha

SAVE SCREEN TO pantalla

USE Parmae   IN  1 ORDER Parmae1 ALIAS Parma

SET FILTER TO Tipo=cTipo
GO TOP

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

 vTitulo = 'Tabla de '+cTitulo
 vTitul2 = 'Revisi¢n de '+cTitulo+'    Presione F10 para concluir          '

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

IF !EMPTY(cCodigo)
	@ 3,7 SAY PADL(cCodigo+":",15," ")
ENDIF

IF !EMPTY(cCodigoAux)
	@ 5,7 SAY PADL(cCodigoAux+":",15," ")
ENDIF

IF !EMPTY(cDescri)
	@ 7,7 SAY PADL(cDescri+":",15," ")
ENDIF

IF !EMPTY(cDescriAux)
	@ 9,7 SAY PADL(cDescriaux+":",15," ")
ENDIF

IF !EMPTY(cDescriAu2)
	@ 11,7 SAY PADL(cDescriAu2+":",15," ")
ENDIF

IF !EMPTY(cNumDec)
	@ 13,7 SAY PADL(cNumDec+":",15," ")
ENDIF

IF !EMPTY(cNumEnt)
	@ 15,7 SAY PADL(cNumEnt+":",15," ")
ENDIF

IF !EMPTY(cFecha)
	@ 17,7 SAY PADL(cFecha+":",15," ")
ENDIF

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
DO Pantalla
SELECT Parma
SCATTER MEMVAR

m.Codigo     = Codigo
m.CodigoAux  = CodigoAux
m.Descri     = Descri
m.DescriAux  = DescriAux
m.DescriAu2  = DescriAu2
m.NumDec     = NumDec
m.NumEnt     = NumEnt
m.Fecha      = Fecha

IF !EMPTY(cCodigo)
	@  3,28 SAY m.Codigo      PICTURE '@S30'
ENDIF

IF !EMPTY(cCodigoAux)
	@  5,28 SAY m.CodigoAux
ENDIF

IF !EMPTY(cDescri)
	@  7,28 SAY m.Descri
ENDIF

IF !EMPTY(cDescriAux)
	@  9,28 SAY m.DescriAux
ENDIF

IF !EMPTY(cDescriAu2)
	@ 11,28 SAY m.DescriAu2
ENDIF

IF !EMPTY(cNumDec)
	@ 13,28 SAY m.NumDec
ENDIF

IF !EMPTY(cNumEnt)
	@ 15,28 SAY m.NumEnt
ENDIF

IF !EMPTY(cFecha)
	@ 17,28 SAY m.Fecha
ENDIF

RETURN

PROCEDURE Revis
*--------------
SELECT Parma
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE KEY cTipo WINDOW Wind_1;
		NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		Codigo    :H=IIF(!EMPTY(cCodigo),cCodigo,"C¢digo"),;
		CodigoAUX :H=IIF(!EMPTY(cCodigoAux),cCodigoAux,"CodigoAux"),;
		Descri    :H=IIF(!EMPTY(cDescri),cDescri,"Descri"):50 ,;
		Nument    :H=IIF(!EMPTY(cNumEnt),cNumEnt,"NumEnt") :P='99,999'

ON KEY LABEL F10
DO Vista
RETURN

PROCEDURE Busca
*--------------
SELECT Parma

IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

Vtemp = RECNO()

DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 6
ACTIVATE WINDOW WBusca
STORE SPACE(6) TO Vbusca

@ 1,3 SAY "Digite c¢digo de "+cDescri+" que desea :" GET Vbusca PICT '@!' COLOR SCHEME 7
READ
RELEASE WINDOW WBusca

IF EMPTY(Vbusca) .OR. LASTKEY()=27
	GOTO Vtemp
 ELSE
	SEEK cTipo+Vbusca
	IF !FOUND()
		DO standby WITH "Dicho "+cDescri+" no fue encontrado."
		GOTO Vtemp
	 ELSE
		DO Vista
	ENDIF
ENDIF

RETURN

PROCEDURE Anter
*--------------
SELECT Parma
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH "Inicio de archivo: no existe &cdescri anterior."
 ELSE
	DO Vista
ENDIF
RETURN
 

PROCEDURE Proxi
*--------------
SELECT Parma
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

IF !EOF()
	SKIP
ENDIF

IF EOF()
	DO standby WITH "Fin de archivo: no existe &cDescri siguiente."
	GO BOTTOM
 ELSE
	DO Vista
ENDIF

RETURN

PROCEDURE Corri
*--------------
SELECT Parma
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF
ACTIVATE WINDOW Wind_0
SCATTER MEMVAR

IF !EMPTY(cCodigo)
	@  3,28 GET m.Codigo DISABLE        && Campo que forma llave primaria
ENDIF

IF !EMPTY(cCodigoAux)
	@  5,28 GET m.CodigoAux DISABLE
ENDIF

IF !EMPTY(cDescri)
	@  7,28 GET m.Descri
ENDIF

IF !EMPTY(cDescriAux)
	@  9,28 GET m.DescriAux
ENDIF

IF !EMPTY(cDescriAu2)
	@ 11,28 GET m.DescriAu2
ENDIF

IF !EMPTY(cNumDec)
	@ 13,28 GET m.NumDec
ENDIF

IF !EMPTY(cNumEnt)
	@ 15,28 GET m.NumEnt
ENDIF

IF !EMPTY(cFecha)
	@ 17,28 GET m.Fecha
ENDIF

READ

IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
	IF f_lock(1)
		IF !EMPTY(cDescri)
			REPLACE Descri		WITH m.Descri
		ENDIF
		
		IF !EMPTY(cDescriAux)
			REPLACE DescriAux	WITH m.DescriAux
		ENDIF
		
		IF !EMPTY(cDescriAu2)
			REPLACE DescriAu2	WITH m.DescriAu2
		ENDIF
		
		IF !EMPTY(cNumDec)
			REPLACE NumDec		WITH m.NumDec
		ENDIF
		
		IF !EMPTY(cNumEnt)
			REPLACE NumEnt		WITH m.NumEnt
		ENDIF
		
		IF !EMPTY(cFecha)
			REPLACE Fecha		WITH m.Fecha
		ENDIF
		
		UNLOCK
	ENDIF
ENDIF
DO Vista                    && Muestra nuevos datos
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
*- Aqu¡ coloca valores por omisi¢n (default)
ACTIVATE WINDOW Wind_0
STORE SPACE(6)  TO mCodigo
STORE SPACE(60) TO mDescri

SELECT Parma
SCATTER MEMVAR BLANK
nReg = RECNO()

IF !EMPTY(cCodigo)
	@  3,28 GET m.Codigo	PICTURE '@!' VALID ValCod()
ENDIF

IF !EMPTY(cCodigoAux)
	@  5,28 GET m.CodigoAux	PICTURE '@!' VALID ValCodAux()
ENDIF

IF !EMPTY(cDescri)
	@  7,28 GET m.Descri
ENDIF

IF !EMPTY(cDescriAux)
	@  9,28 GET m.DescriAux
ENDIF

IF !EMPTY(cDescriAu2)
	@ 11,28 GET m.DescriAu2
ENDIF

IF !EMPTY(cNumDec)
	@ 13,28 GET m.NumDec
ENDIF

IF !EMPTY(cNumEnt)
	@ 15,28 GET m.NumEnt
ENDIF

IF !EMPTY(cFecha)
	@ 17,28 GET m.Fecha
ENDIF

READ

IF LASTKEY() <> 27
	IF f_appd()
		REPLACE Tipo		WITH cTipo
		
		IF !EMPTY(cCodigo)
			REPLACE Codigo		WITH m.Codigo
		ENDIF
		
		IF !EMPTY(cCodigoAux)
			REPLACE CodigoAux	WITH m.CodigoAux
		ENDIF
		
		IF !EMPTY(cDescri)
			REPLACE Descri		WITH m.Descri
		ENDIF
		
		IF !EMPTY(cDescriAux)
			REPLACE DescriAux	WITH m.DescriAux
		ENDIF
		
		IF !EMPTY(cDescriAu2)
			REPLACE DescriAu2	WITH m.DescriAu2
		ENDIF
		
		IF !EMPTY(cNumDec)
			REPLACE NumDec		WITH m.NumDec
		ENDIF
		
		IF !EMPTY(cNumEnt)
			REPLACE NumEnt		WITH m.NumEnt
		ENDIF
		
		IF !EMPTY(cFecha)
			REPLACE Fecha		WITH m.Fecha
		ENDIF
		
		UNLOCK
	ENDIF
 ELSE
	GO nReg
ENDIF
DO Vista
RETURN

FUNCTION ValCod
*--------------
SELE Parma
IF !EMPTY(cCodigoAux)
	*
 ELSE
	IF SEEK(cTipo+ALLTRIM(m.Codigo))
		DO StandBy WITH "El Codigo ya Existe Intente de nuevo"
		RETURN .F.
	 ELSE
		RETURN .T.
	ENDIF
ENDIF


FUNCTION ValCodAux
*--------------
SELE Parma
IF !EMPTY(cCodigoAux)
	IF SEEK(cTipo+ALLTRIM(m.Codigo)+m.CodigoAux)
		DO StandBy WITH "El Codigo ya Existe Intente de nuevo"
		RETURN .F.
	 ELSE
		RETURN .T.
	ENDIF
ENDIF


PROCEDURE Elimi
*--------------
SELECT Parma

IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

STORE YESNO("¨Est  seguro que desea ELIMINAR este "+cCodigo+" ?") TO vElimina

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
SELECT Parma

IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

vTemp = RECNO()

IF EMPTY(cCodigoAux)
	DO REPORTE WITH 2,"ManPar","CATALOGO DE "+UPPER(cTitulo)
 ELSE
	DO REPORTE WITH 2,"ManPar1","CATALOGO DE "+UPPER(cTitulo)
ENDIF

*DO CASE
*	CASE TipMan = 2
*		campo3 = "Descriaux"
*		DO REPORTE WITH 2,"ManPar2","CATALOGO DE "+UPPER(DescS)
*	CASE TipMan = 3
*		campo3 = "Fecha"
*		DO REPORTE WITH 2,"ManPar3","CATALOGO DE "+UPPER(DescS)
*	CASE TipMan = 4 OR Tipman=8
*		campo3 = "NumEnt"
*		DO REPORTE WITH 2,"ManPar4","CATALOGO DE "+UPPER(DescS)
*	CASE TipMan = 5
*		campo3 = "NumDec"
*		DO REPORTE WITH 2,"ManPar5","CATALOGO DE "+UPPER(DescS)
*	CASE TipMan = 6
*		campo3 = "NumEnt"
*		campo4 = "Fecha"
*		DO REPORTE WITH 2,"ManPar6","CATALOGO DE "+UPPER(DescS)
*	casE TipMan = 7
*		SET FILTER TO TIPO='CODSUB'
*		GO TOP
*		SCAN
*			SCATTER MEMVAR
*			SELE PARMAX
*			append blank
*			GATHER MEMVAR
*			SELE PARMA
*		ENDSCAN
*		SET FILTER TO
*		SELE PARMAX
*		GO TOP
*		do reporte with 2,"ManPar7","CATALOGO DE "+UPPER(DescS)
*		sele parma
*	OTHERWISE
*		DO REPORTE WITH 2,"ManPar","CATALOGO DE "+UPPER(DescS)
*ENDCASE

GOTO vTemp

RETURN

PROCEDURE Termi
*--------------
vEn_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE Fin_opcion
*-------------------
SELECT     Parma
SET FILTER TO
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    MENU   mMenu
RESTORE    SCREEN FROM Pantalla
CLOSE DATA

FUNCTION Val_Cod
*---------------
*--------------------------------
* Funcion que puede manipularse 
* segun los parametros a Utilizar
*--------------------------------
PRIVATE mRet,mCodigo,mfiltro

nReg    = RECNO()
mFiltro = FILTER()
mCodigo = ALLTRIM(Parma.Codigo)
SET FILTER TO

DO CASE
	CASE cTipo = 'UNIEJE'
		SEEK Tipo+Codigo="UNIGES"+mcodigo
		mRet = val_para(mCodigo,'UNIGES','Z')
	CASE cTipo = 'CODPRG'
		SEEK "CODFUN"+mcodigo
		mRet = val_para(ALLTRIM(codigo),'CODFUN','Z')
	CASE cTipo = 'CODSPR'
		SEEK "CODPRG"+mcodigo
		mRet = val_para(ALLTRIM(mcodigo),'CODPRG05','Z')
ENDCASE
SET FILTER TO &mFiltro
GO nReg

RETURN mRet
