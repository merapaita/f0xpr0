*------------------------------------
* Cuartel.Prg
* Registra los Cuarteles de los Cementerios 
* 
* AUTOR : A. S. Marco Rodriguez Abad
*------------------------------------
*- Abriendo Archivos

CLOS DATA
USE Cuartel IN  1 ORDER TAG Cuartel1 ALIAS Cuartel
USE Cement  IN  2 ORDER TAG Cement1  ALIAS Cement
USE Parmae  IN  3 ORDER TAG Parmae1  ALIAS Parma
USE Nichos	IN  4 ORDER TAG Nichos1  ALIAS Nichos
*USE NiFocos IN  5 ORDER TAG NiFocos1 ALIAS NiFocos
*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Cuarteles'
vmens02 = ' Cuarteles : REVISION '
vmens03 = ' DETALLES '
vmens04 = 'Dicho Cuartel no fue encontrado'
vmens05 = 'No existe Cuartel anterior'
vmens06 = 'No existe Cuartel siguiente'
vmens07 = '? Desea Anular ?ste Cuartel ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Cuartel ha sido anulado'
vmens10 = 'El Cuartel ya est? Atendido'
vmens11 = 'El Cuartel ha sido devuelto'

SELECT Cuartel
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

DEFINE WINDOW wind_0 FROM 00,00 TO 12,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 13,00 TO 23,79  DOUBLE ;
	TITLE vmens03 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens02 COLOR SCHEME 10

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

 @  1, 10 SAY "      Cementerio :"
 @  3, 10 SAY "  Codigo Cuartel :"
 @  5, 10 SAY "  Nombre Cuartel :"
 @  7, 10 SAY "           Filas :"
 @  9, 10 SAY "        Columnas :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
ON KEY LABEL F9 DO vista_det
SELECT Cuartel

IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@  1,30 SAY Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,30)	
@  3,30 SAY m.CodCuar
@  5,30 SAY m.NomCuar
@  7,30 SAY m.Filas
@  9,30 SAY m.Columnas

DO Vista_Hijo

IF !vFlag$'J*'
	DO SubOpc
ENDIF

RETURN

PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT Nichos
GO TOP
SEEK m.CodCem+m.CodCuar
IF FOUND()
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.CodCem + m.CodCuar ;
		TIMEOUT 0.0001 WINDOW wind_1 FIELDS;
		Fila2		: H= 'Fila',;
		Col001N		: H= 'Columna',;	
		Col002N		: H= 'Columna',;	
		Col003N		: H= 'Columna',;	
		Col004N		: H= 'Columna',;	
		Col005N		: H= 'Columna',;	
		Col006N		: H= 'Columna',;	
		Col007N		: H= 'Columna'
Else
    ACTIVATE WINDOW wind_1
    CLEA
	@ 4,25 say 'No hay Nichos Asignados'
    ACTIVATE WINDOW wind_0
endif
SELE Cuartel

RETURN

PROCEDURE vista_det
*------------------
SELECT Nichos
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GO TOP
SEEK m.CodCem+m.CodCuar

IF FOUND()
	BROWSE NOAPPEND NODELETE NOMODIFY NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.CodCem+m.CodCuar ;
	WINDOW wind_1 TITLE vtempo
Else
	ACTIVATE WINDOW wind_1
	CLEAR
	@ 4,25 say 'No hay Nichos asignados'
ENDIF
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELE Cuartel
DO VISTA
RETURN


*FUNCTION v_Cem
**-------------
*IF SEEK(m.CodCem,"Cement")
*	mRet = Cement.NomCem
* ELSE
*	mRet = "Error; revise"
*ENDIF
*RETURN mRet


PROCEDURE revis              && Revisi?n de BD en browse
*--------------
ON KEY LABEL F9
SELE Cuartel
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SET RELATION TO codcem INTO CEMENT ADDITIVE

vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
DO logos WITH rotulo1,vtempo
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW wind_2 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	Cement.NomCem    :H='Cementerio':30,;
	Cuartel.NomCuar  :H='Cuartel':30,;
	Cuartel.Filas    :H='Filas',;
	Cuartel.Columnas :H='Columnas'

SET RELATION TO

vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SELE Cuartel
DO vista
RETURN

PROCEDURE busca              && Realiza b?squeda directa
*--------------
ON KEY LABEL F9
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vCodCem  = SPACE(2)
vCodCuar = SPACE(4)

DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' ?? B?squeda ?? ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lista
@ 1,2 SAY 'Cementerio :' GET vCodCem   PICTURE '!!'
@ 3,2 SAY '   Cuartel :' GET vCodCuar  PICTURE '!!!!'

READ VALID val_read()

DEACTIVATE WINDOW lista

IF EMPTY(vCodCem) OR EMPTY(vCodCuar) OR LASTKEY()=27
	RETURN
 ELSE
	SEEK vCodCem+vCodCuar
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
SELE Cuartel
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
SELE Cuartel
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
ON KEY LABEL F9
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

SELECT Cuartel
SCATTER MEMVAR
DO pantalla

@  1,30 GET m.CodCem	PICTURE '!!' 	DISABLE
@  3,30 GET m.CodCuar	PICTURE '!!!!' 	DISABLE
@  5,30 GET m.NomCuar	FUNCTION 'S30'
@  7,30 GET m.Filas		DISABLE
@  9,30 GET m.Columnas 	DISABLE

READ VALID val_read()

IF LASTKEY() # 27
	dd = trabaja_hijo()
	IF dd
		IF f_Lock(1)
			SELECT Cuartel
			GATHER MEMVAR
		ENDIF
	ENDIF
 ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL

FLUSH

SELECT Cuartel
DO vista
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PRIVATE dd

SELE Cuartel
vtemp = Iif(!EOF(),RECNO(),-1)

SCATTER MEMVAR BLANK

DO pantalla

m.Estado = "00"
m.Orden  = 1

@  1,30 GET m.CodCem	VALID Val_Fun('Cement','CodCem','NomCem',m.CodCem,1,1,34) AND Val_Cuar()
@  3,30 GET m.CodCuar
@  5,30 GET m.NomCuar	FUNCTION 'S30'
@  7,30 GET m.Filas
@  9,30 GET m.Columnas

READ VALID val_read()

IF LASTKEY()#27
	dd = trabaja_hijo()
	IF dd
		IF F_Appd()
			GATHER MEMVAR
			SELE Parma
			SEEK "CORRELCUARTE"+m.codcem
			REPLACE NumEnt WITH Parma.NumEnt+1
		ENDIF
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

SELECT Cuartel

DO vista
RETURN

PROCEDURE trabaja_hijo
*---------------------
vsun=.T.
ACTIVATE SCREEN
HIDE MENU mmenu

vtempo = '????????????????????[F11] ORDEN DESCENDENTE/ASCENDENTE/OTRO????????????????????'
DO logos WITH rotulo1,vtempo
SELECT Nichos
SET ORDER TO Nichos1

SEEK m.CodCem+m.CodCuar

err = .F.
mCorr = .F.

IF !FOUND()
	DO Agr_Nichos
 ELSE
	mcorr = .T.
	IF m.estado = '00'
		SELE Nichos
		SEEK m.CodCem+m.CodCuar
		SCAN WHILE CodCem+CodCuar = m.CodCem+m.CodCuar
			FOR i = 1 to m.columnas
				Campo = "Col"+ PADL(ALLTRIM(STR(i)),3,'0')+"e"
				IF &Campo # '1'
					err = .T.
					EXIT
				ENDIF
			ENDFOR
		ENDSCAN
	 ELSE
		err = .T.
	ENDIF
ENDIF

IF err
	DO StandBy WITH "Error no se puede modificar la tabla de nichos"
	vsun=.F.
 ELSE
	xx = TblTmp()
	IF !xx
		vsun=.F.
	ENDIF
ENDIF

DEACTIVate WINDOWS wind_1
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '????????????????????????????????????????????????????????????????????????????????'
DO logos WITH rotulo1,vtempo
SELECT Cuartel
RETURN vsun


FUNCTION TblTmp
*--------------
PRIVATE mRet, mOrd
STORE 0 TO mOrd 

ON KEY LABEL F11 DO OrdCol
ON KEY LABEL F10 KEYBOARD(CHR(23))

SELE 0
USE NicTmp	ALIAS Tempo 	EXCLUSIVE
ZAP

SELE Nichos
SEEK m.CodCem+m.CodCuar
SCAN WHILE CodCem+CodCuar=m.CodCem+m.CodCuar
	SCATTER MEMVAR
	SELE Tempo
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
	SELE Nichos
ENDSCAN

SELE Tempo
GO TOP

BROWSE NOAPPEND NODELETE NOMENU NOOPTIMIZE WINDOW wind_1

IF LASTKEY()=27 AND !mCorr
	DO StandBy WITH "Escapo y no se realizaron cambios en la Tabla Nichos"
	SELE Nichos
	SEEK m.CodCem+m.CodCuar
	IF FOUND()
		SCAN WHILE CodCem+CodCuar = m.CodCem+m.CodCuar
			DELETE NEXT 1
		ENDSCAN
	ENDIF
	***********
	* rutina para venta de focos
*	SELE NiFocos
*	SEEK m.CodCem+m.CodCuar
*	IF FOUND()
*		SCAN WHILE CodCem+CodCuar = m.CodCem+m.CodCuar
*			DELETE NEXT 1
*		ENDSCAN
*	ENDIF
	************
	mRet=.F.
 ELSE
	* ACTUALIZAR TABLA NICHOS
	SELE Tempo
	GO TOP
	SELE Nichos
	SEEK m.CodCem+m.CodCuar
	IF FOUND()
		SCAN WHILE CodCem+CodCuar = m.CodCem+m.CodCuar
			REPLACE Nichos.Fila2 WITH Tempo.Fila2
			FOR i = 1 TO m.Columnas
				Campo1 = "Nichos.Col" + PADL(ALLTRIM(STR(i)),3,'0') + 'N'
				Campo2 = "Tempo.Col"  + PADL(ALLTRIM(STR(i)),3,'0') + 'N'
				REPLACE &Campo1 WITH &Campo2
			ENDFOR
			SELE Tempo
			SKIP
			SELE Nichos
		ENDSCAN
	 ELSE
		* ERROR NO ENCONTRO
	ENDIF
	**************************
	* ACTUALIZAR TABLA NIfocos
*	SELE Tempo
*	GO TOP
*	SELE NiFocos
*	SEEK m.CodCem+m.CodCuar
*	IF FOUND()
*		SCAN WHILE CodCem+CodCuar = m.CodCem+m.CodCuar
*			REPLACE Nifocos.Fila2 WITH Tempo.Fila2
*			FOR i = 1 TO m.Columnas
*				Campo1 = "NiFocos.Col" + PADL(ALLTRIM(STR(i)),3,'0') + 'N'
*				Campo2 = "Tempo.Col"  + PADL(ALLTRIM(STR(i)),3,'0') + 'N'
*				REPLACE &Campo1 WITH &Campo2
*			ENDFOR
*			SELE Tempo
*			SKIP
*			SELE NiFocos
*		ENDSCAN
*	 ELSE
*		* ERROR NO ENCONTRO
*	ENDIF

	**************************	

	DO standby with 'Presione Tecla para Continuar',19,2
	mret = .T.
ENDIF

SELE Tempo
*ZAP
USE
ON KEY LABEL F11

RETURN mRet

PROCEDURE OrdCol
*---------------
* Define el Orden de las Columnas:
* Ascendente, Descendente, otro
*******************************

GO TOP
DO CASE
	CASE m.Orden = 0 OR m.Orden = 11
		m.Orden = 1
		FOR j = 1 to m.Filas
			FOR i = m.Columnas TO 1 STEP -1
				Campo  = "m." + FIELD(i+1)
				&Campo = m.Columnas - (i-1)
*				&Campo = (m.Columnas+1) - i
			ENDFOR
			DO Inserta
		ENDFOR
	CASE m.Orden = 1
		m.Orden = 2
		FOR j = 1 to m.Filas
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = i
			ENDFOR
			DO Inserta
		ENDFOR
	CASE m.Orden = 2
		m.Orden = 3
		x1 = 1
		FOR j = 1 TO m.Filas
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = x1
				x1 = x1 + 1
			ENDFOR
			DO Inserta
		ENDFOR
	CASE m.Orden = 3
		m.Orden = 4
		x1 = 0
		xi = x1
		FOR j = 1 TO m.Filas
			x1 = xi + m.Columnas
			xi = x1
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = x1
				x1 = x1 - 1
			ENDFOR
			DO Inserta
		ENDFOR
	CASE m.Orden = 4
		m.Orden = 5
		x1 = 0
		xi = x1
*		xn = m.Filas * m.Columnas
		FOR j = 1 TO m.Filas
			x1 = xi + 1
			xi = x1
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = x1
				x1 = x1 + m.Filas
			ENDFOR
			DO Inserta
		ENDFOR

	CASE m.Orden = 5
		m.Orden = 6
		x1 = m.Filas - 1
		xi = x1
*		xn = m.Filas * m.Columnas
		FOR j = 1 TO m.Filas
			x1 = xi + 1
*			xi = x1
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = x1
				x1 = x1 + m.Filas
			ENDFOR
			xi = xi - 1
			DO Inserta
		ENDFOR

	CASE m.Orden = 6
		m.Orden = 7
		x1 = m.Filas * m.Columnas - (m.Filas)
		xi = x1
		FOR j = 1 TO m.Filas
			x1 = xi + 1
			xi = x1
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = x1
				x1 = x1 - m.Filas
			ENDFOR
			DO Inserta
		ENDFOR
	CASE m.Orden = 7
		m.Orden = 8
		x1 = m.Filas * m.Columnas - 1
		xi = x1
		FOR j = 1 TO m.Filas
			x1 = xi + 1
*			xi = x1
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = x1
				x1 = x1 - m.Filas
			ENDFOR
			xi = xi - 1
			DO Inserta
		ENDFOR
	CASE m.Orden = 8
		m.Orden = 9
		x1 = 0
		xi = x1
*		xn = m.Filas * m.Columnas
		FOR j = 1 TO m.Filas
			IF j = 1
				FOR i = 1 TO m.Columnas
					Campo  = "m." + FIELD(i+1)
					&Campo = 0
					x1 = x1 + m.Filas
				ENDFOR
			ELSE
				x1 = xi + 1
				xi = x1
				FOR i = 1 TO m.Columnas
					Campo  = "m." + FIELD(i+1)
					&Campo = x1
					x1 = x1 + (m.Filas - 1)
				ENDFOR
			ENDIF
			DO Inserta
		ENDFOR
	CASE m.Orden = 9
		m.Orden = 10
		x1 = (m.Filas - 1) * m.Columnas - (m.Filas - 1)
		xi = x1
		FOR j = 1 TO m.Filas
			IF j = 1
				FOR i = 1 TO m.Columnas
					Campo  = "m." + FIELD(i+1)
					&Campo = 0
					x1 = x1 - m.Filas
				ENDFOR
			ELSE
				x1 = xi + 1
				xi = x1
				FOR i = 1 TO m.Columnas
					Campo  = "m." + FIELD(i+1)
					&Campo = x1
					x1 = x1 - (m.Filas - 1)
				ENDFOR
			ENDIF
			DO Inserta
		ENDFOR
	CASE m.Orden = 10
		m.Orden = 11
		FOR j = 1 TO m.Filas
			FOR i = 1 TO m.Columnas
				Campo  = "m." + FIELD(i+1)
				&Campo = 0
			ENDFOR
			DO Inserta
		ENDFOR
ENDCASE

GO TOP

RETURN

****
PROCEDURE Inserta
*----------------
FOR k = m.Columnas+1 TO FCOUNT()-1
	Campo  = "m." + FIELD(k+1)
	&Campo = 0
ENDFOR

SELE Tempo

IF f_Lock(1)
*WAIT WIND M.ORDEN
	GATHER MEMVAR
	SKIP
ENDIF

RETURN
****


FUNCTION Val_Cuar
*----------------
IF SEEK("CORRELCUARTE"+m.codcem,"Parma")
	m.codcuar = PADL(ALLTRIM(STR(Parma.NumEnt+1)),4,'0')
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodCuar)
	DO StandBy WITH "El Codigo esta vacio"
	RETURN .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodCem+m.CodCuar)
		DO standby WITH 'Ya esta Registrado este Cuartel'
		RETURN .F.
	ENDIF
ENDIF
RETURN .T.

PROCEDURE Agr_Nichos
*-------------------
SELE Nichos
FOR i = 1 TO m.Filas
	FOR j = 1 TO m.Columnas
		Campo1  = "m.Col"+ PADL(ALLTRIM(STR((j))),3,'0') + 'N'
		Campo2  = "m.Col"+ PADL(ALLTRIM(STR((j))),3,'0') + 'E'
		&Campo1 = (m.Columnas-(j-1))
		&Campo2 = ALLTRIM(STR(1))
	ENDFOR
	m.Fila1 = i
*	m.Fila2 = CHR(64+(m.filas-(i-1)))
	IF f_Appd()
		GATHER MEMVAR
	ENDIF
ENDFOR

******
* rutina especial para venta de focos
*SELE NiFocos
*FOR i = 1 TO m.Filas
*	FOR j = 1 TO m.Columnas
*		Campo1  = "m.Col"+ PADL(ALLTRIM(STR((j))),3,'0') + 'N'
*		Campo2  = "m.Col"+ PADL(ALLTRIM(STR((j))),3,'0') + 'C'
*		&Campo1 = (m.Columnas-(j-1))
*		&Campo2 = 0
*	ENDFOR
*	m.Fila1 = i
*	IF f_Appd()
*		GATHER MEMVAR
*	ENDIF
*ENDFOR
******

RETURN


PROCEDURE elimi
*--------------
SELECT Cuartel
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

IF yesno('? Desea ELIMINAR ?ste Cuartel ?')
	lBorra = .T.
	SELE Nichos
	IF SEEK(m.CodCem+m.CodCuar)
		SCAN WHILE m.CodCem = CodCem AND m.CodCuar = CodCuar
			FOR i = 1 TO m.Columnas
				campo = 'Col'+PADL(ALLTRIM(STR(i)),3,'0')+'E'
				IF !EMPTY(&Campo)
					IF &Campo # "1"
						lBorra = .F.
					ENDIF
				ENDIF
			ENDFOR
		ENDSCAN
		IF lBorra
			SEEK m.CodCem+m.CodCuar
			DELE FOR m.CodCem = CodCem AND m.CodCuar = CodCuar
			SELE Cuartel
			DELE NEXT 1
			SKIP -1
			IF BOF()
				GO TOP
			ENDIF
		 ELSE
			DO STANDBY WITH "No Puede Borrar este Cuartel por que tiene Nichos ocupados"
		ENDIF
	ENDIF
ENDIF
SELECT Cuartel
UNLOCK ALL
DO vista
RETURN

PROCEDURE lista
*--------------
PRIVATE nReg
nReg = RECNO()
SET RELATION TO CodCem		   INTO Cement  ADDITIVE

DEFINE WINDOW LIS FROM 3,15 TO 20,70 FLOAT DOUBLE TITLE 'Listado de Cuarteles' COLOR SCHEME 5
ACTIVATE WINDOW LIS

vToCem  = 1
vToCuar = 1
mCodCem  = SPACE(2)
mCodCuar = SPACE(4)
lNic = 0

@ 01,01 SAY "Todas los Cementerios : " GET vToCem  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 03,01 SAY "Cementerio : "
@ 03,15 GET mCodCem PICTURE '@!' WHEN VW_Cuar() AND vToCem = 2  VALID VV_Cem(3,15)
@ 05,01 SAY "Todas los Cuarteles : " GET vToCuar  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
@ 07,01 SAY "Cuartel : "
@ 07,15 GET mCodCuar PICTURE '@!' WHEN VW_Cuar() AND vToCuar = 2  VALID VV_Cuar(7,15)
@ 09,15 GET lNic FUNCTION '*C Incluir Nichos'
@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE
RELEASE WINDOWS LIS

IF LASTKEY()#27 AND OkCancel#2
	SET FILTER TO CodCem = ALLTRIM(mCodCem) AND CodCuar = ALLTRIM(mcodcuar)
	GO TOP
	IF EOF()
		DO StandBy WITH 'No Existe Informacion a procesar'
	 ELSE
		IF lNic=1
			SET RELATION TO cuartel.codcem+ cuartel.codcuar INTO NICHOS ADDITIVE
			SET SKIP TO NICHOS
			DO REPPRG WITH "Nichos",' Listado de Cuarteles ',2
			SET SKIP TO
			SET RELATION OFF INTO NICHOS
		 ELSE
			DO REPORTE WITH 2,"Cuartel","Listado de Cuarteles"
		ENDIF
	ENDIF
	SET FILTER TO
ENDIF
SET RELATION TO
SELE Cuartel
go nReg
DO vista
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
PRIVATE cAlias,cOrd,lBusca
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
	cOrd = ORDER()
	SET ORDER TO Cuartel2
	DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD NomCuar		&& &v__bus
	ON SELECTION POPUP v__xx DEACTIVATE POPUP
	ACTIVATE POPUP v__xx
	RELEASE POPUP v__xx
	SET ORDER TO &cOrd
ENDIF
mCodCuar = Cuartel.CodCuar

@ wFila,wColumna SAY mCodCuar + "   " + Cuartel.NomCuar

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
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE MENU   mmenu
*RESTORE SCREEN FROM principal
RETURN

*FUNCTION Val_nic
*---------------
PARAMETERS xNum
Campo = "Nichos.Columna"+ALLTRIM(STR(xNum))
DO CASE
	CASE &Campo="1"
		mRet = PADL(ALLTRIM(STR(xNum)),2,'0')
	CASE &Campo="2"
		mRet = ""
	CASE &Campo="3"
		mRet = "RR"
	CASE &Campo=""
		mRet = ""
ENDCASE
RETURN mRet


PROCEDURE Nichos
*---------------
PARAMETER _desti
PRIVATE nReg,xReg,xErr

IF _desti=2
   SET PRINTER TO (p_fil)
ENDIF

SET DEVICE TO PRINT

Impri    = .F.
GO TOP
xRCol = 15
xErr = .F.

STORE 0 TO pagina, linea
@ 00,00 SAY CHR(27)+CHR(64)

SCAN
	nReg = RECNO()
	
	IF pagina = 0 .OR. linea > 57
		DO Titulo
	ENDIF
	
	xC    = 1
	xcol  = Cuartel.Columnas
	xFil  = Cuartel.Filas
	xCem  = CodCem
	xCuar = CodCuar
	xReg = RECNO()
	
	FOR i = xC TO xCol
		ii = i
		IF linea > 55
			DO Titulo
		ENDIF
		
		@ Linea,01 SAY "CEMENTERIO " + Cement.NomCem
		Linea = Linea +1
		@ Linea,01 SAY "Cuartel " + Cuartel.NomCuar
		Linea = Linea +2
		
		k = 1
		FOR k = 1 TO xFil
			
			IF Nichos.fila1 # STR(k,1)
				DO StandBy WITH "Error en Codigo de fila. avise al area de sistemas."
				xErr = .T.
				EXIT
			ENDIF
			
			@ Linea,01 SAY CHR(15)
			@ Linea,02 SAY Nichos.fila2
			Columna = 4
			
			FOR j = 1 TO xRCol
				IF i > xCol
					EXIT
				ENDIF
				
				Campo1 = 'Nichos.Col' + PADL(ALLTRIM(STR(i)),3,'0')+'N'
				Campo2 = 'Nichos.Col' + PADL(ALLTRIM(STR(i)),3,'0')+'E'
				
				DO CASE
					CASE &Campo2 = '1'
						@ Linea,Columna SAY PADL(ALLTRIM(STR(&Campo1)),3,'0')
					CASE &Campo2 = '2'
						@ Linea,Columna SAY "OF"
					CASE &Campo2 = '3'
						@ Linea,Columna SAY CHR(178)+CHR(178)+CHR(178)
					CASE &Campo2 = '4'
						@ Linea,Columna SAY CHR(178)+" "+CHR(178)
				ENDCASE
				
				i = i + 1
				Columna = Columna + 7
			ENDFOR
			linea = linea + 1
			IF !EOF()
				SKIP 1
			ENDIF
			IF k < xFil
				i = ii
			ENDIF
		ENDFOR
		IF xErr
			EXIT
		ENDIF
		LINEA = LINEA + 1
		IF i < xCol
			GO xReg
			xC = i
		ENDIF
	ENDFOR
	IF xErr
		EXIT
	ENDIF
	SKIP -1
ENDSCAN
@ Linea, 1 SAY CHR(12)

SET DEVICE TO SCREEN
SET PRINTER TO
RETURN



*PROCEDURE Nichos
*---------------
PARAMETER _desti
PRIVATE nReg

IF _desti=2
   SET PRINTER TO (p_fil)
ENDIF

SET DEVICE TO PRINT

Impri    = .F.
GO TOP

xRCol = 15

STORE 0 TO pagina, linea
@ 00,00 SAY CHR(27)+CHR(64)

SCAN
	nReg = RECNO()
	
	IF pagina = 0 .OR. linea > 57
		DO Titulo
	ENDIF
	
	xC = 1
	xcol = Cuartel.Columnas
	xFil = Cuartel.Filas
	xCem  = CodCem
	xCuar = CodCuar
	
	FOR i = xC TO xCol
		ii = i
		
		IF linea > 55
			DO Titulo
		ENDIF
		
		@ Linea,01 SAY "CEMENTERIO " + Cement.NomCem
		Linea = Linea +1
		@ Linea,01 SAY "Cuartel " + Cuartel.NomCuar
		Linea = Linea +2
		
		SCAN WHILE CodCem+CodCuar=xCem+xCuar
			
			@ Linea,01 SAY CHR(15)
			@ Linea,02 SAY nichos.fila2
			Columna = 4
			FOR j = 1 TO xRCol
				IF i > xCol
					EXIT
				ENDIF
				Campo1 = 'Nichos.Col' + PADL(ALLTRIM(STR(i)),3,'0')+'N'
				Campo2 = 'Nichos.Col' + PADL(ALLTRIM(STR(i)),3,'0')+'E'
				
				DO CASE
					CASE &Campo2 = '1'
						@ Linea,Columna SAY PADL(ALLTRIM(STR(&Campo1)),3,'0')
					CASE &Campo2 = '2'
						@ Linea,Columna SAY "OF"
					CASE &Campo2 = '3'
						@ Linea,Columna SAY CHR(178)+CHR(178)+CHR(178)
					CASE &Campo2 = '4'
						@ Linea,Columna SAY CHR(178)+" "+CHR(178)
				ENDCASE
				
				i = i + 1
				Columna = Columna + 4
			ENDFOR
			Linea = Linea + 1
			DO CASE
				CASE xFil = VAL(Nichos.Fila1) AND i < xCol
					* no hace nada
				CASE xFil = VAL(Nichos.Fila1) AND i > xCol
					* no hace nada
				OTHER
					i = ii
			ENDCASE
		ENDSCAN
		linea = linea + 1
		SKIP -1
		IF xFil = VAL(Nichos.Fila1) AND i < xCol
			SEEK xCem+xCuar
			i = i - 1
		ENDIF
	ENDFOR
ENDSCAN
@ Linea, 1 SAY CHR(12)

SET DEVICE TO SCREEN
SET PRINTER TO
RETURN

PROCEDURE Titulo
*---------------
pagina = pagina  + 1

@ 0,3   SAY cia
@ 0,70  SAY 'P?g.'
@ 0,74  SAY pagina   PICTURE '##,###'
@ 1,70  SAY DATE()
@ 2,1   SAY CHR(18)+CHR(14)
@ 2,15  SAY "LISTADO DE CUARTELES"  +CHR(18)
@ 3,15  SAY "~~~~~~~~~~~~~~~~~~~~"
 
linea = 5
RETURN

PROCEDURE SubOpc
*---------------
PRIVATE cAlias

cAlias = ALIAS()

USE IteUsuOp IN 0 ORDER TAG IteUsuOp2

set skip of PAD Revis of mMenu !SEEK(vUsucla+"0055","IteUsuOp")
set skip of PAD Busca of mMenu !SEEK(vUsucla+"0056","IteUsuOp")
set skip of PAD Anter of mMenu !SEEK(vUsucla+"0057","IteUsuOp")
set skip of PAD Proxi of mMenu !SEEK(vUsucla+"0058","IteUsuOp")
set skip of PAD Corri of mMenu !SEEK(vUsucla+"0059","IteUsuOp")
set skip of PAD Ingre of mMenu !SEEK(vUsucla+"0060","IteUsuOp")
set skip of PAD Elimi of mMenu !SEEK(vUsucla+"0061","IteUsuOp")
set skip of PAD Lista of mMenu !SEEK(vUsucla+"0062","IteUsuOp")

USE IN IteUsuOp

SELE (cAlias)

RETURN
