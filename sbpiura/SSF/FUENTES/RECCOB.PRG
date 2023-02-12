*------------------------------------------------------------------
* InsSoc.Prg
* Registra Socios
* Autor: Analista Segundo Correa Sandoval
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maecob   IN 2  ORDER TAG maecob1  ALIAS cobra
USE iterec   IN 3  ORDER TAG iterec1  ALIAS iterec

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Cobradores'
vmens02 = ' Cobrador : REVISION '
vmens04 = 'Dicho Cobrador no fue encontrado'
vmens05 = 'No existe Cobrador anterior'
vmens06 = 'No existe Cobrador siguiente'
vmens07 = '¨ Desea Anular ‚ste Cobrador ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Cobrador ha sido anulado'
SELECT cobra
GO TOP
ON KEY LABEL F9 DO VISTA_DET
*- Variables de trabajo (registro a trabajar)
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
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 12,79  DOUBLE ;
	TITLE vmens02 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Socios        ®F9¯ Detalle : Item ' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_2a FROM 01,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Socios         ®F10¯ Salir ' ;
	COLOR SCHEME 10

DEFINE WINDOW Wind_3 FROM 20,64 TO 22,78 ;
 TITLE ' TOTAL ' COLOR SCHEME 10

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
ACTIVATE WINDOW wind_1
CLEAR

 @  0, 2 SAY "           C¢digo :"
 @  2, 2 SAY "        Apellidos :"
 @  4, 2 SAY "          Nombres :"
 @  6, 2 SAY "        Direcci¢n :"
 @  8, 2 SAY "         Tel‚fono :"
 @ 10, 2 SAY "             Edad :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT cobra

SET FILT TO
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@  0, 60 SAY Verest(m.estado)
@  0,22 SAY m.codigo
@  2,22 SAY m.apelli
@  4,22 SAY m.nombre
@  6,22 SAY m.direcc
@  8,22 SAY m.telefo
@ 10,22 SAY m.edad

DO vista_hijo
RETURN

PROCEDURE vista_hijo
*-------------------
SELECT iterec
SEEK m.codigo
IF FOUND()
	BROWSE ;
		NOAPPEND NODELETE NOEDIT NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.codigo TIMEOUT 0.0001 ;
		WINDOW wind_2 ;
		FIELDS;
		recini	: H= 'Desde',;	
		recfin	: H= 'Hasta',;
		fecent  : H= 'Fecha',;
		observ  : H= 'Observaci¢n'
Else
	ACTIVATE WINDOWS WIND_2
	@ 4,30 say 'No hay Recibos'
endif
SELE cobra
RETURN

PROCEDURE vista_det
*------------------
SELECT iterec
SEEK m.codigo
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
IF FOUND()
	BROWSE ;
		LOCK 1 NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.codigo;
		WINDOW wind_2a TITLE vtempo ;
		FIELDS;
		recini	: H= 'Desde',;	
		recfin	: H= 'Hasta',;
		fecent  : H= 'Fecha',;
		observ  : H= 'Observaci¢n'
Else
	@ 4,30 say 'No hay Afiliados'
endif
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELE cobra
DO VISTA
RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE cobra
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SET RELATION TO codigo INTO iterec
SET SKIP TO iterec
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	cobra.codigo    :H='Codigo',;
	cobra.apelli    :H='Apellido',;
	cobra.nombre    :H='Nombre',;	
	iterec.recini	:H='Desde',;	
	iterec.recfin	:H='Hasta',;	
	iterec.fecENT	:H='Fecha'

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SET RELA TO
SELE cobra
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
STore SPACE(4) TO vcodcon

DEFINE WINDOW lisTA FROM 09,12 TO 13,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA

@  2,2 SAY 'Condigo : ' GET vcodcon  PICTURE '9999'

READ VALID val_read()

DEACTIVATE WINDOW lisTA

IF EMPTY(vcodcon) .OR. LASTKEY()=27
	RETURN
ELSE
	SEEK vcodcon
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
SELE cobra
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
SELE cobra
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
SELECT cobra
SCATTER MEMVAR
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF M.estado = '99'
	* Anulada
	DO standby WITH vmens09
	RETURN
ENDIF
DO pantalla

@  0,22 SAY m.codigo
@  2,22 SAY m.apelli PICTURE "@!" 
@  4,22 SAY m.nombre PICTURE "@!" 
@  6,22 SAY m.direcc PICTURE "@!"
@  8,22 SAY m.telefo PICTURE "999999" 
@ 10,22 SAY m.edad   PICTURE "99" 

IF LASTKEY() # 27
	ok=trabaja_hijo()
	IF ok AND LASTKEY()#27
		SELECT COBRA
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT COBRA
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT COBRA
op=ORDER()
vtemp = RECNO()
DO pantalla
SCATTER MEMVAR

@  0,22 SAY m.codigo
@  2,22 SAY m.apelli PICTURE "@!" 
@  4,22 SAY m.nombre PICTURE "@!" 
@  6,22 SAY m.direcc PICTURE "@!"
@  8,22 SAY m.telefo PICTURE "999999" 
@ 10,22 SAY m.edad   PICTURE "99" 

IF LASTKEY()=27
	IF !EOF() AND !BOF()
		go vtemp
		DO vista
	ENDIF	
	RETURN
Else
	DO WHILE .T.
		ok=trabaja_hijo()
		IF LASTKEY() # 27 AND OK
			IF yesno('¨ Confirme el ingreso ?')
				ok=.T.
				EXIT
			ENDIF
		ELSE
			IF yesno('¨ Cancela el ingreso ?')
				DO standby WITH ' Cancelado el Ingreso ..'
				ok = .F.
				EXIT
			ELSE
				LOOP
			ENDIF
		ENDIF
	ENDDO
	IF ok AND LASTKEY()#27
		SELECT COBRA
	ELSE
		SELE iteREC
		SEEK m.codigo
		SCAN WHILE codigo=m.codigo
			IF RLOCK()
				DELETE NEXT 1
			ENDIF
		ENDSCAN
		GO VTEMP
	ENDIF
ENDIF
UNLOCK ALL
SELECT COBRA
do pantalla
DO vista
RETURN

PROCEDURE trabaja_hijo
*---------------------
vsun=.T.
PUBLIC ak
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f5  DO agreg_item
ON KEY LABEL f8  DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELECT iteREC
SEEK m.codigo
IF !FOUND()
	DO agreg_item
ENDIF
BROWSE ;
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.codigo ;
	WINDOW wind_2 ;
	FIELDS;
		recini	: H= 'Desde',;	
		recfin	: H= 'Hasta',;
		fecent  : H= 'Fecha',;
		observ  : H= 'Observaci¢n'

ON KEY LABEL f5
ON KEY LABEL f8
ON KEY LABEL f10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SELECT COBRA
RETURN vsun

PROCEDURE elimi
*---------------
SELECT COBRA
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Socio?')
IF velimina .AND.  f_lock(1)
	DELE NEXT 1
	SELECT iteREC
	SEEK m.codigo
	IF FOUND()
		SCAN WHILE m.codigo=iteREc.codigo
			IF f_lock(1)
				DELETE NEXT 1
			ENDIF
		ENDSCAN
	ENDIF
	SELECT COBRA
	IF !BOF()
		SKIP -1
	ELSE
		IF !EOF()
			SKIP
		ENDIF
	ENDIF
ENDIF
UNLOCK ALL

DO vista
RETURN

PROCEDURE agreg_item
*-----------------
SELECT iteREC
IF f_appd()
	REPLACE codigo WITH m.codigo
	RETURN .T.
ENDIF
RETURN .F.

PROCEDURE elimi_item
*-------------------
SELECT iteREC
IF RLOCK()
	DELETE NEXT 1
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
UNLOCK
RETURN

PROCEDURE lista
*--------------
vAli = Alias()
vord = Order()
vrec = Recno()
SELECT ITEREC
SET RELATION TO CODIGO INTO COBRA
SET SKIP TO COBRA
STORE 1  TO vToCli,vOrden,vTipPro,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin
vCODIGO = SPACE(4)

DEFINE WINDOW LIS FROM 6,15 TO 18,65 FLOAT DOUBLE TITLE 'Recibos de Cobranzas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY "       Todos : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,1,22)

@ 03,01 SAY "      C¢digo : " 
@ 03,22 GET vCodigo    PICTURE '!!!!' VALID Val_cobra() when vTOCLI=2

@ 05,01 SAY "     Recibos : " GET vToFue  FUNCTION '*RNH \<Todas;\<Por fechas' SIZE 1,10,6  VALID VALTOD(VTOfue,5,15) 
@ 07,01 SAY "      Fechas : "
@ 07,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN vtofue=2 
@ 07,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini)  WHEN vtofue=2 

@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	IF vtocli=2
		SET FILTER TO codigo=vcodigo and iif(vtofue=2,BETWEEN(fecent,vfecini,vfecfin),.t.)
	Else
		SET FILTER TO iif(vtofue=2,BETWEEN(fecent,vfecini,vfecfin),.t.)
	endif
	do reporte with 2,"LisRec",'Listado de Recibos',2
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAli)
SET ORDER TO (vord)
GO vrec
DO VISTA
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_2A
RELEASE WINDOW wind_3
RELEASE MENU   mmenu
RESTORE SCREEN FROM principal
RETURN

PROCEDURE VAL_HC             && Revisi¢n de BD en browse
*---------------
 ON KEY LABEL F9	
 SELECT cobra
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK vCli
 IF !FOUND()
    Vtemp = RECNO()
    HIDE MENU mMenu
    ACTIVATE SCREEN
    vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
    ON KEY LABEL F10 KEYBOARD CHR(23)
    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	codigo    :H='Codigo',;
	apelli    :H='Apellido',;
	nombre    :H='Nombre'
    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    IF LASTKEY()=27
       GO BOTT
    ENDIF
 ENDIF
 vcli = codigo
 ON KEY LABEL F10
 ON KEY LABEL F9 DO VISTA_DET
 SELECT iterec
RETURN

