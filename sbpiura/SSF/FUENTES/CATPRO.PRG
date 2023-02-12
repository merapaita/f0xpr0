*------------------------------------------------------------------
* InsSoc.Prg
* Registra Socios
* Autor: Analista Segundo Correa Sandoval
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE MAEPRO   IN 2  ORDER TAG maePRO1  ALIAS PROMO
*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Promotor'
vmens02 = ' Promotor : REVISION '
vmens04 = 'Dicho Promotor no fue encontrado'
vmens05 = 'No existe Promotor anterior'
vmens06 = 'No existe Promotor siguiente'
vmens07 = '¨ Desea Anular ‚ste Promotor ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Promotor ha sido anulado'
SELECT Promo
GO TOP
ON KEY LABEL F9 DO VISTA_DET

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
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 23,79  DOUBLE ;
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


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR

 @  2, 2 SAY "           C¢digo :"
 @  4, 2 SAY "        Apellidos :"
 @  6, 2 SAY "          Nombres :"
 @  8, 2 SAY "        Direcci¢n :"
 @ 10, 2 SAY "         Tel‚fono :"
 @ 12, 2 SAY "             Edad :"
 @ 14, 2 SAY "             L.E. :"
 
RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT promo

SET FILT TO
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@  0,60 SAY Verest(m.estado)
@  2,22 SAY m.codigo
@  4,22 SAY m.apelli
@  6,22 SAY m.nombre
@  8,22 SAY m.direcc
@ 10,22 SAY m.telefo
@ 12,22 SAY m.edad
@ 14,22 SAY m.LIBELE
RETURN
PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE promo
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
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	codigo    :H='C¢digo',;
	apelli    :H='Apellido',;
	nombre    :H='Nombre',;	
	direcc    :H='Direcci¢n',;
	edad      :H='Edad',;
	LIBELE    :H='L.E.'
	
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SET RELA TO
SELE promo
SET ORDE TO 1
DO vista
RETURN


PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vcodigo  = '    '
DEFINE WINDOW lisTA FROM 11,12 TO 14,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA

@  1,2 SAY '  C¢digo : ' GET vcodigo   PICTURE '!!!!'
READ VALID val_read()

DEACTIVATE WINDOW lisTA

IF EMPTY(vcodigo) .OR. LASTKEY()=27
	RETURN
ELSE
	SET ORDER TO 1
	SEEK vcodigo
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
SELE promo
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
SELE promo
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
IF estado = '99'
	* Anulada
	DO standby WITH vmens09
	RETURN
ENDIF
*****
SELECT prOMO
SCATTER MEMVAR
DO pantalla
@  2,22 SAY m.codigo
@  4,22 GET m.apelli PICTURE "@!" 
@  6,22 GET m.nombre PICTURE "@!" 
@  8,22 GET m.direcc PICTURE "@S50!"
@ 10,22 GET m.telefo PICTURE "999999" 
@ 12,22 GET m.edad   PICTURE "99" 
@ 14,22 GET m.LIBELE PICTURE "99999999" 
READ VALID val_read()
IF LASTKEY() # 27
	IF LASTKEY()#27
		SELECT prOMO
		GATHER MEMVAR
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT prOMO
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT prOMO
SCATTER MEmVAR BLANK
op=ORDER()
vtemp = RECNO()
DO pantalla
SELE Parma
SEEK 'CORRELPROMOT'
IF FOUND()
	m.codigo = PADL(nument+1,4,'0')
ELSE
	m.codigo = '0001'
ENDIF
@  2,22 SAY m.codigo
@  4,22 GET m.apelli PICTURE "@!" 
@  6,22 GET m.nombre PICTURE "@!" 
@  8,22 GET m.direcc PICTURE "@!"
@ 10,22 GET m.telefo PICTURE "999999" 
@ 12,22 GET m.edad   PICTURE "99" 
@ 14,22 GET m.LIBELE PICTURE "99999999" 
READ VALID val_read()
IF LASTKEY()=27
	IF !EOF() AND !BOF()
		go vtemp
		DO vista
	ENDIF	
	RETURN
ENDIF
SELECT parma
replace nument with nument+1
SELECT promo
M.ESTADO = '00'
APPEND BLANK
GATHER MEmVAR
UNLOCK ALL
DO PANTALLA
DO vista
RETURN

PROCEDURE elimi
*---------------
SELECT promo
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Promotor?')
IF velimina .AND.  f_lock(1)
	*DELE NEXT 1
	REPLACE ESTADO WITH '99'
ENDIF
UNLOCK ALL
DO vista
RETURN


PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE lista
*--------------
vAlias = Alias()
vorder = Order()
vrecno = Recno()
SELECT promo
STORE 1  TO vToCli,vOrden,vTipPro,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin
vCli = SPACE(4)

DEFINE WINDOW LIS FROM 5,15 TO 15,65 FLOAT DOUBLE TITLE 'Listado Promotores' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY "Por Promotor : " GET vTOCLI  FUNCTION '*RNH \<No;\<Si' SIZE 1,10,6   VALID VALTOD(VTOCLI,1,22)

@ 03,01 SAY "      C¢digo : "
@ 03,22 GET vCli    PICTURE '!!!!' VALID Val_Hc() when vTOCLI=2

@ 07,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	IF vtocli=2
		SET FILTER TO codigo=vcli 
	endif
	do reporte with 2,"Lispro",'Listado de Promotores',2
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAlias)
SET ORDER TO (vorder)
GO vrecno
DO VISTA
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
 SELECT pROMO
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
 SELECT PROMO
RETURN
