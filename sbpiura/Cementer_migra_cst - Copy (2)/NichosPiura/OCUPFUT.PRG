*-----------------------------------------
* OcupFut.Prg
* Registra Ingreso de Ocupaciones Futuras
* Autor : A. S. Marco Rodriguez Abad
*-----------------------------------------
*- Abriendo Archivos
CLOS DATA
USE OcupFut  IN 1 ORDER TAG OcupFut1  ALIAS OcuFut
USE Cuartel  IN 2 ORDER TAG Cuartel1  ALIAS Cuartel
USE Cement   IN 3 ORDER TAG Cement1   ALIAS Cement
USE Nichos	 IN 4 ORDER TAG Nichos1   ALIAS Nichos
USE Clientes IN 5 ORDER TAG Clientes1 ALIAS Clien
USE Parmae   IN 6 ORDER TAG Parmae1   ALIAS Parma
*-
*- Mensajes de aviso al usuario

vmens01 = 'Registro de Ocupaciones Futuras'
vmens02 = ' REVISION '
vmens04 = 'Dicho registro no fue encontrado'
vmens05 = 'No existe Registro anterior'
vmens06 = 'No existe registro siguiente'
vmens07 = '¨ Desea Eliminar ‚ste registro ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este registro ha sido anulado'
*vmens10 = 'El Difunto ya est  Atendido'
*vmens11 = 'El Difunto ha sido devuelto'

vDbf = SYS(3)+".Dbf"
CREATE TABLE (vDbf) (CodCem C(2), CodCuar C(4), Fila1 C(1), Fila2 C(1), Col1 C(3), Col2 C(3),Est C(1))
USE &vDbf IN 7 ALIAS Tempo EXCLUSIVE

SELECT OcuFut
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
	TITLE vmens01 FOOTER "[F11] --> Agregar a Difuntos" COLOR SCHEME 10

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

@  0, 40 SAY " Estado :"
@  1,  7 SAY "     Cementerio :"
@  2,  7 SAY "         Codigo :"
@  3,  7 SAY "        Cliente :"
@  4,  7 SAY " D.N.I. Cliente :"
@  6,  7 SAY "Apell. Ocupante :"
@  7,  7 SAY " Nomb. Ocupante :"
@  8,  7 SAY "  Sexo Ocupante :"
@  9,  7 SAY "  Edad Ocupante :"
@  9, 29 SAY "A."
@  9, 34 SAY "M."
@  9, 39 SAY "D."
@ 11,  7 SAY "        Cuartel :"
@ 12,  7 SAY "   Codigo Nicho :"
@ 13,  7 SAY "           Fila :"
@ 14,  7 SAY "        Columna :"
RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT OcuFut

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR

DO CASE
	CASE m.EstVta ='00'
		@  0, 10 SAY SPACE(15)
	CASE m.EstVta ='10'
		@  0, 10 SAY "Cancelado"	COLOR SCHEME 5
	CASE m.EstVta ='20'
		@  0, 10 SAY "Con Saldo"	COLOR SCHEME 5
	CASE m.EstVta ='99'
		@  0, 10 SAY "Anulado"		COLOR SCHEME 5
ENDCASE

@  0, 50 SAY VAL_PARA(m.Estado,'ESTOCF',' ',50,30)
@  1, 26 SAY m.CodCem
@  1, 31 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,31)
@  2, 26 SAY m.CodOcu
@  3, 26 SAY m.Codcli
@  3, 33 SAY Val_Fun('Clien','CodCli','NomCli',m.CodCli,1,3,33)
@  4, 26 SAY m.DNICli
@  6, 26 SAY m.ApePatOc
@  6, 50 SAY m.ApeMatOc
@  7, 26 SAY m.NomOcu
@  8, 26 SAY VAL_PARA(m.SexoOcu,'SEXO  ',' ',26,15)		&&m.Sexo
@  9, 26 SAY m.EdadOcu PICTURE '999'
@  9, 32 SAY m.EdadOMe PICTURE '99'
@  9, 37 SAY m.EdadODi PICTURE '99'
@ 11, 26 SAY m.CodCuar
@ 11, 31 SAY Val_Cuar(11,31)
@ 12, 26 SAY m.CodNic
@ 13, 26 SAY m.Fila
@ 14, 26 SAY m.Columna

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

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


*FUNCTION Nom_Cuar
*----------------
cAlias = ALIAS()
SELE Cuartel
cOrd = ORDER()
SET ORDER TO Cuartel1
SEEK m.Cement+m.Cuartel
IF FOUND()
	cNomCuar = Cuartel.NomCuar
	@ 12,31 SAY cNomCuar
ENDIF
SET ORDER TO cOrd
SELE &cAlias
RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE OcuFut
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
ON KEY LABEL f10 KEYBOARD CHR(23)
SET RELATION TO CodCem INTO Cement
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	OcuFut.CodOcu	:H='Codigo'	,;
	OcuFut.CodCli	:H='Cliente',;
	OcuFut.ApePatOc	:H='Ape. Pat Oc.'	:30,;
	OcuFut.NomMatOc	:H='Nom. Mat Oc.'	:30

SET RELATION TO
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE OcuFut
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

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lista
@ 1,2  SAY '      Apell. Oc.:' GET vAP		FUNCTION '!S15'
@ 1,36 GET vAM		FUNCTION '!S15'
@ 3,2  SAY 'Nombres Ocupante:' GET vNom	FUNCTION '!S30'

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vAp+vAm+vNom) OR LASTKEY()=27
    SET ORDER TO (cOrd)
	RETURN
 ELSE

 	SELECT OcuFut.CodCem,OcuFut.CodCuar,OcuFut.Estado,CodOcu,ApePatOc,ApeMatOc,NomOcu,NomCuar;
 		FROM OcuFut,Cuartel;
 		WHERE OcuFut.CodCem+OcuFut.CodCuar=Cuartel.CodCem+Cuartel.CodCuar and OcuFut.ApePatOc = ALLTRIM(vAp) AND OcuFut.ApeMatOc = ALLTRIM(vAM) AND OcuFut.NomOcu = ALLTRIM(vNom);
		ORDER BY ApePatOc,ApeMatOc,NomOcu;
		INTO CURSOR xTbl
	
	vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL F10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
		CodOcu	:H='Codigo'	,;
		ApePatOc:H='Ape. Paterno'	:30,;
		ApeMatOc:H='Ape. Materno'	:30,;
		NomOcu	:H='Nom. Ocupante'	:30,;
		NomCuar :H='Cuartel'		:30
	
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	
	IF lastKey()#27
		SELE OcuFut
		IF !SEEK(xTbl.CodOcu)
	    	DO StandBy WITH "Parametros de busqueda Erroneos"
		ENDIF
	ELSE
		DO StandBy WITH "Proceso Cancelado"
	ENDIF
	
ENDIF

DO VISTA
RETURN

*PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()

DEFINE POPUP pLista FROM 20,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo Ocupaci¢n '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre Ocupante '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

IF LASTKEY()#27
	
	DO CASE
		CASE BAR() =  1
			vBusca = SPACE(8)
			vNombre = "Codigo :"
			SET ORDER TO TAG OcupFut1
		CASE BAR() =  2
			vBusca = SPACE(30)
			vNombre = "Nombre : "
			SET ORDER TO TAG OcupFut2
	ENDCASE
	
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
	
	ACTIVATE WINDOW lisTA
	@ 1,2 SAY vNombre GET vBusca
	
	READ VALID val_read()
	
	DEACTIVATE WINDOW lista
	
	IF EMPTY(vBusca) OR LASTKEY()=27
		* RETURN
	 ELSE
		SEEK ALLTRIM(vBusca)
		IF !FOUND()
			DO standby WITH vmens04
			GOTO vtemp
		 ELSE
			DO vista
		ENDIF
	ENDIF
	
 ELSE
	DO StandBy WITH "Proceso Cancelado"
ENDIF


SET ORDER TO &cOrd1
RETURN

PROCEDURE anter
*--------------
SELE OcuFut
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
SELE OcuFut
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

SELE OcuFut
SCATTER MEMVAR
DO pantalla

@  0, 50 SAY VAL_PARA(m.Estado,'ESTOCF',' ',50,15)		&&m.Estado
@  1, 26 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,31)
@  2, 26 GET m.CodOcu	DISABLE
@  3, 26 GET m.CodCli	VALID Val_Fun('Clien','CodCli','NomCli',m.CodCli,1,3,31) FUNCTION 'S40' DISABLE
@  3, 26 SAY Val_Fun('Clien','CodCli','NomCli',m.CodCli,1,3,33) FUNCTION 'S40'
@  4, 26 GET m.DNICli	WHEN .F.
@  6, 26 GET m.ApePatOc	FUNCTION '!S23'
@  6, 50 GET m.ApeMatOc	FUNCTION '!S23'
@  7, 26 GET m.NomOcu	FUNCTION '!S30'
@  8, 26 GET m.SexoOcu	FUNCTION 'M M,F' VALID VAL_PARA(m.SexoOcu,'SEXO  ',' ',31,15)
@  9, 26 GET m.EdadOcu	PICTURE '999'	VALID empty(m.edadOMe) or m.edadMes < 12
@  9, 32 GET m.EdadOMe	PICTURE '99'	VALID empty(m.edadODi) or m.edadODi < 31
@  9, 37 GET m.EdadODi	PICTURE '99'
@ 11, 26 GET m.CodCuar  VALID Val_Fun('Cuartel','CodCuar','NomCuar',m.CodCuar,1,11,36) AND VAL_NIC() DISABLE
@ 12, 26 GET m.CodNic	DISABLE
@ 13, 26 GET m.Fila     DISABLE
@ 14, 26 GET m.Columna  DISABLE

READ VALID val_read()

IF LASTKEY() # 27
	IF f_Lock(1)
		SELECT OcuFut
		GATHER MEMVAR
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT OcuFut
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------

SELECT OcuFut

vtemp = IIF(EOF(),-1,RECNO())

SCATTER MEMVAR BLANK
m.Estado='10'
m.EstVta='00'
DO pantalla

@  0, 50 SAY VAL_PARA(m.Estado,'ESTOCF',' ',60,15)		&&m.Estado
@  1, 26 GET m.CodCem	VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,31) AND Cor_OF()
@  2, 26 GET m.CodOcu	WHEN .F.		&&WHEN Val_Cli()
@  3, 26 GET m.CodCli	VALID Val_Cli(m.CodCli,3,33) 		&& FUNCTION 'S40'
@  4, 26 GET m.DNICli	WHEN .F.							&& VALID VAL_DNI()
@  6, 26 GET m.ApePatOc	FUNCTION '!S23'
@  6, 50 GET m.ApeMatOc	FUNCTION '!S23'
@  7, 26 GET m.NomOcu	FUNCTION '!S30'
@  8, 26 GET m.SexoOcu	FUNCTION 'M M,F' VALID VAL_PARA(m.SexoOcu,'SEXO  ',' ',34,20)
@  9, 26 GET m.EdadOcu	PICTURE '999'
@  9, 32 GET m.EdadOMe	PICTURE '99'	VALID empty(m.edadOMe) or m.edadMes < 12
@  9, 37 GET m.EdadODi	PICTURE '99'	VALID empty(m.edadODi) or m.edadODi < 31
@ 11, 26 GET m.CodCuar	VALID Val_Cuar(11,31) AND VAL_NIC()
@ 12, 26 GET m.CodNic	DISABLE
@ 13, 26 GET m.Fila		VALID Val_F()
@ 14, 26 GET m.Columna	VALID Val_C()

READ VALID val_read()

SELE OcuFut

IF LASTKEY()#27
	IF F_Appd()
		GATHER MEMVAR
	ENDIF
	
	SELE Parma
	SEEK "CORRELOCUFUT"+ALLTRIM(m.Codcem)
	IF f_lock(1)
		REPLACE NumEnt WITH Parma.NumEnt+1
	ENDIF
	
	SELE Nichos
	SEEK m.CodCem+m.CodCuar+LEFT(m.CodNic,1)
	Campo = "Col"+SUBSTR(m.Codnic,2,3)+'E'
	
	IF f_lock(1)
		REPLACE &Campo WITH "2"
	ENDIF
	
 ELSE
	IF vTemp = -1
		GO TOP
	 ELSE
		go vtemp
	ENDIF
	
	DO vista
	RETURN
ENDIF

UNLOCK ALL

FLUSH

SELECT OcuFut
DO vista
RETURN

FUNCTION VAL_DNI
*---------------
PRIVATE LenDni,i,mRet
Lendni = LEN(m.DNICli)
mRet = .T.
FOR i = 1 to LenDni
	IF BETW(ASC(SUBSTR(m.DNICli,i,1)),ASC("0"),ASC("9"))
	 ELSE
		mRet = .F.
	ENDIF
ENDFOR
RETURN mRet


FUNCTION Val_Nic
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
			
			IF &Campo1 = "1" OR &Campo1 = "4"
				SELE Tempo
				APPEND BLANK
				REPLACE CodCem WITH m.CodCem, CodCuar WITH m.CodCuar, Fila1 WITH Nichos.Fila1, Fila2 WITH Nichos.Fila2, Col1 WITH PADL(ALLTRIM(STR(i)),3,'0'), Col2 WITH PADL(ALLTRIM(STR(&Campo2)),3,'0'), Est WITH IIF(&Campo1 = "4",'*','')
				SELE Nichos
			ENDIF
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
SELE OcuFut
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
lElimina = .T.
IF YesNo("Desea Eliminar este registro")
*	IF !ve_passw("ELIOF")
*		DO STANDBY WITH "Proceso Cancelado"
*		lElimina = .F.
*	ENDIF
 ELSE
	DO STANDBY WITH "Proceso Cancelado"
	lElimina = .F.
ENDIF

IF lElimina
	IF Estado='20'
		DO StandBy WITH "Este Registro ya tiene Servicio."
		lElimina = .F.
	ENDIF
ENDIF

IF lElimina
	IF !EMPTY(m.Fila) and !EMPTY(m.Columna)
		fCampo = "Col"+PADL(SUBSTR(m.CodNic,2,3),3,'0')+'E'
		SELE Nichos
		SEEK m.CodCem+m.CodCuar+SUBSTR(m.CodNic,1,1)
		IF FOUND()
			REPLACE &fCampo WITH "1"
		ENDIF
		
		SELE OcuFut
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
SELE OcuFut

IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

nReg = RECNO()

INDEX ON CodCem+CodCuar+CodNic TO (vIdx)

SELE Cuartel
SET ORDER TO Cuartel1
SELE OcuFut
SET RELATION TO CodCem		   INTO Cement  ADDITIVE
SET RELATION TO CodCem+CodCuar INTO Cuartel ADDITIVE

DEFINE WINDOW LIS FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Ocupaciones Futuras' COLOR SCHEME 5
ACTIVATE WINDOW LIS

vToCem  = 1
vToCuar = 1
vToFila = 1
mCodCem  = SPACE(2)
mCodCuar = SPACE(4)
mFila    = SPACE(1)

@ 01,01 SAY "Todas los Cementerios : " GET vToCem  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 03,01 SAY "Cementerio : "
@ 03,15 GET mCodCem PICTURE '@!' WHEN vToCem = 2  VALID VV_Cem(3,15)
@ 05,01 SAY "Todas los Cuarteles : " GET vToCuar  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 07,01 SAY "Cuartel : "
@ 07,15 GET mCodCuar PICTURE '@!' WHEN VW_Cuar() AND vToCuar = 2  VALID VV_Cuar(7,15)
@ 09,01 SAY "Todas los Filas : " GET vToFila  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 11,01 SAY "Fila : "
@ 11,15 GET mFila PICTURE '@!' WHEN VW_Fila() AND vToCuar = 2  VALID VV_Fila(11,15)

@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOWS LIS

IF OkCancel = 1 AND LASTKEY()#27
	SET FILTER TO CodCem = ALLTRIM(mCodCem) AND CodCuar = ALLTRIM(mcodcuar) AND Fila = ALLTRIM(mFila)
	DO REPORTE WITH 2,"OcupFut","Listado de Ocupaciones Futuras"
ENDIF

SET FILTER TO
SET RELATION TO
SET INDEX TO
SET ORDER TO OcupFut1
ERASE (vIdx)
GO nReg

SELE Cuartel
SET ORDER TO Cuartel1
SELE OcuFut
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
cAlias = ALIAS()
SELE Nichos
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
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD Nichos.Fila		&& &v__bus
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
ENDIF
mFila = Nichos.Fila

@ wFila,wColumna SAY mFila

SET FILTER TO
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
ON KEY LABEL F3
ON KEY LABEL F11
ERASE &vDbf
RELEASE WINDOW wind_0
RELEASE MENU   mmenu
*RESTORE SCREEN FROM principal
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
	SEEK(m.Cement+xDeb)
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

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0038","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0039","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0040","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0041","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0042","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0043","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0044","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0045","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
