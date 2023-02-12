*------------------------------------------------------------------
* GaSTOS.Prg
* REGISTRO DE EGRESOS
* Autor: Analista Segundo Correa Sandoval
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE gastos   IN 2  ORDER TAG gastos1  ALIAS gasto
USE maeper   IN 3  ORDER TAG maeper1  ALIAS perso


*- Mensajes de aviso al usuario
vmens01 = 'Registro de Gastos'
vmens02 = ' Gastos : REVISION '
vmens04 = 'Dicho Gasto no fue encontrado'
vmens05 = 'No existe Gasto anterior'
vmens06 = 'No existe Gasto siguiente'
vmens07 = '¨ Desea Anular ‚ste Gasto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Gasto ha sido anulado'
SELECT gasto
GO TOP
ON KEY LABEL F9 
ON KEY LABEL F2 DO VISOBS
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
	TITLE vmens01 FOOTER 'F2 ==> Observaci¢n' COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens02 FOOTER 'F2 ==> Observaci¢n' COLOR SCHEME 10

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
 @  4, 2 SAY "  Fecha del Gasto :"
 @  6, 2 SAY "  C¢digo Personal :"
 @  8, 2 SAY "    Tipo de Gasto :"
 @ 10, 2 SAY "            Monto :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT gasto
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@  0, 60 SAY Verest(m.estado)
@  2,22 SAY m.codigo
@  4,22 SAY m.fecgas
@  6,22 SAY Val_fun('perso','codigo',"codigo+' '+apelli",m.codper,3,6,22)
@  8,22 SAY Val_Para(m.tipgas,'TIPGAS','V',8,22)
@ 10,22 SAY m.cangas PICTURE '99,999.99'

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE gasto
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
	fecgas    :H='fecha',;
	codper    :H='Personal',;	
	tipgas    :H='Gasto',;
	cangas    :H='Monto'

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE gasto
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

@  2,2 SAY '  C¢digo : ' GET vcodigo   PICTURE '!!!!'
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
SELE gasto
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
SELE gasto
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
SELECT gasto
SCATTER MEMVAR
DO pantalla
@  2,22 SAY m.codigo
@  4,22 GET m.fecgas
@  6,22 GET m.CodPer PICTURE '9999'	VALID Val_fun('perso','codigo',"codigo+' '+apelli",m.codper,1,6,22)
@  8,22 GET m.tipgas PICTURE '999' 	VALID Val_Para(m.tipgas,'TIPGAS',' ',22,40)
@ 10,22 GET m.cangas PICTURE '99,999.99'
READ VALID val_read()
IF LASTKEY() # 27
	IF LASTKEY()#27
		SELECT gasto
		GATHER MEMVAR
		= Observa()
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT gasto
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT gasto
SCATTER MEmVAR BLANK
op=ORDER()
vtemp = RECNO()
DO pantalla
SELE Parma
SEEK 'CORRELGASTOS'
IF FOUND()
	m.codigo = PADL(nument+1,4,'0')
ELSE
	m.codigo = '0001'
ENDIF
@  2,22 SAY m.codigo
@  4,22 GET m.fecgas
@  6,22 GET m.CodPer PICTURE '9999'	VALID Val_fun('perso','codigo',"codigo+' '+apelli",m.codper,1,6,22)
@  8,22 GET m.tipgas PICTURE '999' 	VALID Val_Para(m.tipgas,'TIPGAS',' ',22,40)
@ 10,22 GET m.cangas PICTURE '99,999.99'
READ VALID val_read()
IF LASTKEY()=27
	IF !EOF() AND !BOF()
		go vtemp
		DO PANTALLA
		DO vista
	ENDIF	
	RETURN
ENDIF
SELECT parma
replace nument with nument+1
SELECT gasto
APPEND BLANK
M.ESTADO='00'
GATHER MEmVAR
=observa()
UNLOCK ALL
DO PANTALLA
DO vista
RETURN

PROCEDURE elimi
*---------------
SELECT gasto
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste gasto?')
IF velimina .AND.  f_lock(1)
	*DELE NEXT 1
	REPLACE ESTADO WITH '99'
	*IF !BOF()
    *	SKIP -1
	*ELSE
	*	IF !EOF()
	*		SKIP
	*	ENDIF
	*ENDIF
ENDIF
UNLOCK ALL
DO vista
RETURN

PROCEDURE lista
*--------------
vAlias = Alias()
vorder = Order()
vrecno = Recno()
SELECT GASTO
SET RELATION TO CODPER INTO PERSO
SET SKIP TO PERSO
STORE 1  TO vToCli,vOrden,vTipPro,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin
vCli = SPACE(6)

DEFINE WINDOW LIS FROM 6,15 TO 18,65 FLOAT DOUBLE TITLE 'Listado Cobranzas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY " Por Personal: " GET vTOCLI  FUNCTION '*RNH \<No;\<Si' SIZE 1,10,6   VALID VALTOD(VTOCLI,1,22)

@ 03,01 SAY "      C¢digo : "
@ 03,22 GET vCli    PICTURE '!!!!' VALID Val_Hc() when vTOCLI=2

@ 05,01 SAY "  Por Fechas : " GET vToFue  FUNCTION '*RNH \<No;\<Si' SIZE 1,10,6  VALID VALTOD(VTOfue,5,15) 
@ 07,01 SAY "      Fechas : "
@ 07,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN vtofue=2 
@ 07,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini)  WHEN vtofue=2 

@ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	IF vtocli=2
		SET FILTER TO codper=vcli and iif(vtofue=2,BETWEEN(fecgas,vfecini,vfecfin),.t.)
		do reporte with 2,"Lisgas1",'Listado de Gastos',2
	Else
		SET FILTER TO iif(vtofue=2,BETWEEN(fecgas,vfecini,vfecfin),.t.)
		do reporte with 2,"Lisgas2",'Listado de Gastos',2
	endif
	
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAlias)
SET ORDER TO (vorder)
IF !BOF() OR !EOF()
	GO vrecno
ENDIF
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
ON KEY LABEL F2
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE MENU   mmenu
RESTORE SCREEN FROM principal
RETURN


FUNCTION Observa
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 34
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,22 TO 20,57 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERV WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF

RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,22 TO 20,57 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERV NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

PROCEDURE VAL_HC             && Revisi¢n de BD en browse
*---------------
 SELECT perso
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
 SELECT gasto
RETURN
