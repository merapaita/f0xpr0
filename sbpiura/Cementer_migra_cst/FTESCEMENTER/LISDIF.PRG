*---------------------------
* RepDif.Prg    
* Para Visualizar y Corregir Difuntos por cuarteles
* Autor: Marco Rodriguez
*---------------------------

PUBLIC worker
SET PROCEDURE TO LIBSIS1
*DO Fox_Ambi
SET TALK OFF
SET CENT ON
SET EXCLU OFF
SET DELETE OFF
SET CONFIRM ON
SET DATE FRENCH

worker = .T.

*SET PATH TO ..\CSMA\CEMENTER
*SET PATH TO Y:\CEMENTER

CLOS DATA
USE fallecid IN 1
USE Cuartel  IN 2
USE Maestro  IN 3

SELE Fallecid
vIdx1 = SYS(3)+'.Idx'
INDEX ON padl(alltrim(str(COD_CEMENT)),2,'0') + padl(alltrim(str(COD_CUARTE)),3,'0') + FILA + padl(alltrim(str(COLUMNA)),3,'0') TO (vIdx1)

SELE Cuartel
vIdx2 = SYS(3)+'.Idx'
INDEX ON padl(alltrim(str(COD_CEMENT)),2,'0') + padl(alltrim(str(COD_CUARTE)),3,'0') TO (vIdx2)

SELE Maestro
vIdx3 = SYS(3)+'.Idx'
INDEX ON padl(alltrim(str(COD_CEMENT)),2,'0') TO (vIdx3)

SELE Fallecid

DO Inicia
DO salida

PROCEDURE Inicia
*---------------

vToCe  = 1
vToCu  = 1
m.Cod_Cement = SPACE(2)
m.Cod_Cuarte = SPACE(3)


DEFINE WINDOW wLista FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Reporte de Difuntos' COLOR SCHEME 5
ACTIVATE WINDOW wLista
@ 01,01 SAY "Todas los Cementerios:" GET vToCe	 	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2
@ 03,01 SAY "  Cementerio: " GET m.Cod_Cement	WHEN vToCe=2				VALID Val_CeX()
@ 05,01 SAY "  Todas los Cuarteles: " GET vToCu	 	FUNCTION '*RNH \<Si;\<No' SIZE 1,10,2	WHEN vToCe=2
@ 07,01 SAY "  Cuartel: "    GET m.Cod_Cuarte 	WHEN vToCe=2 AND vToCu=2	VALID Val_CuX()

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW wLista

DO Espera WITH 1, "Procesando Reporte de Difuntos"

IF LASTKEY()#27 AND OkCancel#2

	SELE Fallecid
	SET RELATION TO PADL(ALLTRIM(STR(COD_CEMENT)),2,"0")+PADL(ALLTRIM(STR(COD_CUARTE)),3,"0") INTO CUARTEL
	
	SELE Cuartel
	SET RELATION TO PADL(ALLTRIM(STR(COD_CEMENT)),2,"0") INTO Maestro
	
	sele fALLECID 

	SET FILTER TO PADL(ALLTRIM(STR(Cod_Cement)),2,'0') = ALLTRIM(m.Cod_Cement) AND PADL(ALLTRIM(STR(Cod_Cuarte)),3,'0') = ALLTRIM(m.Cod_Cuarte)
	
	GO TOP
	DO Espera WITH 2
	
	IF !EOF()
		ON KEY LABEL F8 DO Elimina
		BROW FIELD  Cod_Cement	:h="Cem",;
					Cod_Cuarte	:h="Cuart",;
					Cod_Fallec	:h="Codigo",;
					Fila		:h="F",;
					Columna		:h="Col",;
					Ape_patern	:10:h="A.P.",;
					Ape_matern	:10:h="A.M.",;
					Fec_Fallec	:h="fecha",;
					Sexo		:h="Sexo",;
					Nombres		:15
					
		ON KEY LABEL F8
	 ELSE
		DO StandBy WITH "No Existe Informaci줻 para Procesar."
	ENDIF
	*****
	IF Yesno("Desea copiar este reporte")
		SET DELE ON
		vDbf1 = "dif" + PADL(ALLTRIM(STR(Cod_Cement)),2,'0') + PADL(ALLTRIM(STR(Cod_Cuarte)),3,'0') +'.Dbf'
		
		COPY TO (vDbf1) FIELD  Maestro.Nom_Cement,Cuartel.Nom_Cuarte,;
							Cod_Fallec,Fila,Columna,Ape_patern,	Ape_matern,;
							Nombres,Fec_Fallec,sexo
	ENDIF

	*****
 ELSE
	DO Espera WITH 2
	DO StandBy WITH "Proceso Cancelado."
ENDIF

RETURN

FUNCTION Val_CeX
*----------------
PRIVATE mRet

=Val_Ce(m.Cod_Cement,' ',23)

IF SEEK(m.Cod_Cement,'Maestro')
	mRet = .T.
 ELSE
	DO StandBy WITH "El Cementerio Especificado no tiene Difuntos"
	m.CodCla = SPACE(3)
	mret = .F.
ENDIF
RETURN mRet


PROCEDURE Val_Ce
*---------------
PARAMETERS xcod, mVariable, mCol 		&&,_tipo,_x,_y
PRIVATE mFiltro
mAlias = ALIAS()
v_fun = .F.

DO CASE
	CASE PARAMETERS() = 1
		mvariable = ' '
		mCol = 0
	CASE PARAMETERS() = 2
		mCol = 0
ENDCASE

SELE Maestro
GO TOP

IF !EMPTY(xcod)
	SEEK xcod
	v_fun = FOUND()
ENDIF

IF EMPTY(xcod) OR !v_Fun
	GO TOP
	ACTIVATE SCREEN
	ON KEY LABEL F10 KEYBOARD CHR(23)
	DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusArt
	BROWSE WINDOW _BusArt TITLE '께께 [F10] Selecciona 께께' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU ;
	 FIELDS;
			Cod_Cement	:H='C줰igo'     ,;
			Nom_Cement	:H='Nombre':40
	ON KEY LABEL F10
	DEACTIVATE WINDOWS _BusArt
	RELEASE    WINDOW _BusArt
	
	IF Lastkey()#27
		m.Cod_Cement = PADL(ALLTRIM(STR(Maestro.Cod_Cement)),2,'0')
		mDescri  = Nom_Cement
		v_fun = .T.
	ELSE
		V_FUN = .f.
		ON KEY LABEL F10 KEYBOARD CHR(23)
		UNLOCK ALL
		SELECT (mAlias)
		RETURN V_FUN
	ENDIF
 ELSE
	mDescri  = Nom_Cement
ENDIF

ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL

SELECT (mAlias)

DO CASE
	CASE mvariable==' '   && En edici줻
		@ ROW(),MCOL SAY mdescri
		RETURN .T.
	CASE mvariable=='A'   && En edici줻 SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescri
		RETURN m.CodCla
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescri
	CASE mvariable=='D'   && En vista
		RETURN mdescri
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescri
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici줻
*		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE
RETURN

FUNCTION Val_CuX
*----------------
PRIVATE mRet

=Val_Cu(m.Cod_Cement+m.Cod_Cuarte,' ',23)

IF SEEK(m.Cod_Cement+m.Cod_Cuarte,'Cuartel')
	mRet = .T.
 ELSE
	DO StandBy WITH "El Cuartel Especificado no tiene Difuntos"
	m.CodCla = SPACE(3)
	mret = .F.
ENDIF
RETURN mRet


PROCEDURE Val_Cu
*---------------
PARAMETERS xcod, mVariable, mCol 		&&,_tipo,_x,_y
PRIVATE mFiltro,cOrd

mAlias = ALIAS()
v_fun = .F.

DO CASE
	CASE PARAMETERS() = 1
		mvariable = ' '
		mCol = 0
	CASE PARAMETERS() = 2
		mCol = 0
ENDCASE

SELE Cuartel
GO TOP

IF !EMPTY(xcod)
	SEEK xcod
	v_fun = FOUND()
ENDIF

IF EMPTY(xcod) OR !v_Fun
	cOrd = ORDER()
	vIdx4 = SYS(3)+'.Idx'
	INDEX ON PADL(ALLTRIM(STR(Cod_Cement)),2,'0') + Nom_Cuarte TO (vIdx4)  FOR COD_TIPO#3
	GO TOP
	ACTIVATE SCREEN
	ON KEY LABEL F2  DO Buscuar
	ON KEY LABEL F10 KEYBOARD CHR(23)
	DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusArt
	BROWSE WINDOW _BusArt TITLE '께께 [F10] Selecciona 께께' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU key m.Cod_Cement;
	 FIELDS;
			Cod_Cement	:H='Cem'     ,;
			Cod_Cuarte	:H='Cuartel' ,;
			Nom_Cuarte	:H='Nombre':40
	ON KEY LABEL F10
	ON KEY LABEL F2

	DEACTIVATE WINDOWS _BusArt
	RELEASE    WINDOW _BusArt
	
	IF Lastkey()#27
		m.Cod_Cuarte = PADL(ALLTRIM(STR(Cuartel.Cod_Cuarte)),3,'0')
		mDescri  = Nom_Cuarte
		v_fun = .T.
		SET INDEX TO (vIdx2)
		ERASE (vIdx4)
	ELSE
		V_FUN = .f.
		ON KEY LABEL F10 KEYBOARD CHR(23)
		UNLOCK ALL
		SET INDEX TO (vIdx2)
		ERASE (vIdx4)
		SELECT (mAlias)
		RETURN V_FUN
	ENDIF
 ELSE
	mDescri  = Nom_Cuarte
ENDIF

ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL

SELECT (mAlias)

DO CASE
	CASE mvariable==' '   && En edici줻
		@ ROW(),MCOL SAY mdescri
		RETURN .T.
	CASE mvariable=='A'   && En edici줻 SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescri
		RETURN m.CodCla
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescri
	CASE mvariable=='D'   && En vista
		RETURN mdescri
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescri
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici줻
*		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE
RETURN

FUNCTION Buscuar		&& Realiza b즧queda directa
*-----------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = IIF(!EOF(),RECNO(),-1)

vBusca = SPACE(30)
vNombre = "Nombre : "

IF LASTKEY()#27
	DEFINE WINDOW lisTA FROM 13,12 TO 16,68 DOUBLE ;
		TITLE ' 같 B즧queda 같 ' FLOAT COLOR SCHEME 10
	
	ACTIVATE WINDOW lisTA
	@ 1,2 SAY vNombre GET vBusca
	READ 									&&VALID val_read()
	DEACTIVATE WINDOW lista
	RELEASE WINDOW LISTA
ENDIF

IF EMPTY(vBusca) OR LASTKEY()=27
	* No hace nada
 ELSE
	SEEK m.Cod_Cement + ALLTRIM(UPPER(vBusca))
	IF !FOUND()
		DO standby WITH "Cuartel no Existe"
		IF vtemp = -1
			GO BOTT
		 ELSE
			GOTO vtemp
		ENDIF
	 ELSE
		* No hace Nada
	ENDIF
ENDIF

RETURN


PROCEDURE Salida
*---------------
ACTIVATE SCREEN
CLOSE DATABASE
ERASE (vIdx1)
ERASE (vIdx2)
ERASE (vIdx3)

RETURN

PROCEDURE Elimina
*----------------
IF !DELETE()
	DELETE NEXT 1
 ELSE
	RECALL NEXT 1
ENDIF
