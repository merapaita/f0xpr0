* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  LibSis.PRG                                                         º
* º                                                                     º
* º                 LIBRERIA DEL SISTEMA                                º
* º                                                                     º
* º                                                                     º
* º  Observaci¢n: para Fox Pro 2.x                                      º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

PROCEDURE Fox_ambi
*-----------------
* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º                                                                     º
* º         AMBIENTE DE TRABAJO DEL SISTEMA DE ANSHIN                   º
* º                                                                     º
* º                                                                     º
* º  SAIKI Consultores   Av. Arequipa 1130 Dpto 02  Telf.  709612       º
* º                                                                     º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

* COMPLETE LIST OF SET COMMANDS

* Procedimineto general para los errores de entrada.
ON READERROR         DO   Err_input
*

SET ALTERNATE        OFF  && Default
SET ALTERNATE TO          && Default
SET AUTOSAVE         OFF
SET BELL             ON   && Default
SET BLOCKSIZE TO     33   && Default
SET BLINK            ON  && Allows use of bright backgrounds
SET BORDER TO        SINGLE
* SET CARRY               && NOT RELEVANT TO SYSTEM ENVIRONMENT
* SET CARRY TO            && NOT RELEVANT TO SYSTEM ENVIRONMENT
SET CENTURY          ON
SET CLEAR            ON
SET CLOCK            ON   && Default
SET CLOCK TO         00,69
* SET COLOR               && ALL COLOR SETTINGS HANDLED IN COLORSET.PRG
SET COMPATIBLE       OFF  && Default
SET CONFIRM          ON
SET CONSOLE          ON   && Default
SET CURRENCY         LEFT && Default
SET DATE             BRITISH  && Default
IF !worker
	SET DEBUG          OFF  && Default
ELSE
	* THE WORKER VARIABLE CONTROLS HOW VARIOUS SETTINGS ARE HANDLED THAT ARE
	* HELPFUL DURING PROGRAM DEVELOPMENT. EXAMPLES: ESCAPE, DEVELOPMENT, ETC.
	* Here's some examples of how you can use the WORKER environmental and
	* FoxPro variables for your convenience.
	* SET DISPLAY TO     EGA43
	* SET DEBUG          ON
	* SET STEP ON
ENDIF
SET DECIMALS TO      2   && Default
SET DEFAULT TO           && LEAVE SET TO CURRENT DEFAULT DRIVE
SET DELETED          ON
IF worker
	SET DEVELOPMENT    ON
ELSE
	SET DEVELOPMENT    OFF && Default
ENDIF
SET DEVICE TO        SCREEN && Default
*SET DISPLAY         && ACCEPT DEFAULT, WHICH MEANS 25 LINE MODE COLOR OR MONO
SET DOHISTORY        OFF  && Default
SET ECHO             OFF  && Default
IF worker            && TURN OFF FOR LIVE USE, ON FOR PROGRAMMING & TESTING
	SET ESCAPE         ON
ELSE
	SET ESCAPE         OFF  && Default
ENDIF
SET EXACT            Off
*-------------------------------------------------------------OJO
SET EXCLUSIVE        OFF
SET MULTILOCKS       ON
SET FIELDS           OFF
*	SET FIELDS         NOT RELEVANT TO SYSTEM ENVIRONMENT
*	SET FILTER         NOT RELEVANT TO SYSTEM ENVIRONMENT
SET FIXED            ON
SET FORMAT           TO
SET FULLPATH         ON
*	SET FUNCTION       && LEAVE DEFAULTS
SET FUNCTION  1 TO
SET FUNCTION  2 TO
SET FUNCTION  3 TO
SET FUNCTION  4 TO
SET FUNCTION  5 TO
SET FUNCTION  6 TO
SET FUNCTION  7 TO
SET FUNCTION  8 TO
SET FUNCTION  9 TO
SET FUNCTION 10 TO
SET HEADING          OFF
SET HELP             OFF
SET HOURS TO         12
*	SET INDEX          NOT RELEVANT TO SYSTEM ENVIRONMENT
SET INSTRUCT         ON
SET INTENSITY        ON
IF worker
	SET logerror       ON
ELSE
	SET logerror       OFF
ENDIF
SET MARGIN TO        0
SET MARK TO          "-"  && THIS IS THE DATE FIELD SEPARATOR
*	THE DEFAULT IS "/"
*	WE'RE USING SOMETHING DIFFERENT HERE
SET MEMOWIDTH TO     80
SET MENU             ON
SET MESSAGE TO       23
SET notify           ON
SET NEAR             OFF
SET ODOMETER TO      1  && SET UP TO 10 OR 100 IN LARGER APPLICATIONS
* SET ORDER TO       NOT RELEVANT TO SYSTEM ENVIRONMENT
SET PATH TO          && ACCEPT DEFAULT PATHS
SET POINT TO         "."
SET PRECISION TO     3
SET PRINT            OFF
SET PRINTER TO       lpt1
* SET procedure TO   && THIS COULD BE HANDLED HERE, BUT WE PREFER NOT TO
*                       "BURY" IMPORTANT INFO LIKE THIS IN A SUBROUTINE.
* SET RELATION TO    NOT RELEVANT TO SYSTEM ENVIRONMENT
SET SAFETY           OFF
SET SCOREBOARD       OFF
SET SYSMENU          OFF
SET SEPARATOR TO     ","
SET SPACE            OFF
SET STATUS           OFF
IF .NOT. worker
	SET STEP           OFF
ENDIF
SET sticky           ON
SET TALK             OFF
SET TOPIC TO
SET TYPEAHEAD TO     0    && CLEAR AWAY ANY OVERANXIOUS INPUT!
SET TYPEAHEAD TO     25
SET UDFPARMS  TO REFERENCE
SET UNIQUE           OFF

* SET VIEW             ON &&EE
* SET WINDOW OF MEMO TO &&EE


* SET THE INSERT / CAPS / NUMLOCK KEYS
* NOTE: LOOKUP POPUPS WORK BETTER WITH INSMODE OFF, AS BELOW

= INSMODE(.F.)
= CAPSLOCK(.T.)
= NUMLOCK(.T.)


* Define working windows
DEFINE WINDOW Err_input  FROM 01,60 TO 03,77

* Define standard system communication windows
DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5
* Typical system messages:
DEFINE WINDOW msg2user   FROM 04,02 TO 08,77 DOUBLE FLOAT SHADOW

 * Ventana de Proceso
 DEFINE WINDOW Espera     FROM 12,06 TO 14,78 COLOR SCHEME 05 
   
* Press any key or click mouse to continue messages:
DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5

DEFINE WINDOW _FUNBUS    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW
* VARIABLE PARA EL BLOQUEO
PUBLIC Escape
ESCAPE = 27
* SYSTEM VARIABLE SETTINGS
_alignment =  "LEFT"
_box =        .t.
_indent =     0
_lmargin =    0
_padvance =   "FORMFEED"
_pageno =     1
_pbpage =     1
* _PCOLNO     SYSTEM MAINTAINED VARIABLE
_pcopies =    1
*_PDRIVER =    &&GENERIC.PR2
_pecode =     ""
_peject =     "AFTER"
_pepage =     32767
_pform =      ""
_plength =    60
* _PLINENO =  SYSTEM MAINTAINED VARIABLE
_ploffset =   0
_ppitch =     "PICA"
_pquality =   .t.
_pscode =     ""
_pspacing =   1
_pwait =      .f.
_rmargin =    80
_tabs =       "5, 10, 15, 20, 25, 30, 35"
_wrap =       .t.
RETURN


FUNCTION f_lock
*--------------
PARAMETERS btipo

beep = CHR(7)

DO CASE
	CASE btipo = 1    && Bloquea registro hasta conseguirlo o escape
		v_fun   = .F.
		IF RLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH "Registro del archivo " + ALIAS() + " ocupado. Espere un momento por favor o presione <Esc> para cancelar"
			?? beep
			ktecla    = 0

			DO WHILE ktecla <> ESCAPE .AND. ( .NOT. RLOCK())
				ktecla    = INKEY()
			ENDDO

			IF ktecla <> ESCAPE
				v_fun = .T.
			ENDIF
		ENDIF
	CASE btipo = 2     && Intenta bloquear registro solo una vez
		v_fun   = .F.
		IF RLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH "El registro del file" + ALIAS() + " est  siendo utilizado. Se cancela la operaci¢n"
			?? beep
		ENDIF
	CASE btipo = 3        && Bloquea archivo hasta conseguirlo a cancela
		v_fun   = .F.
		IF FLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH ALIAS() + "Archivo ocupado.  Espere un instante por favor o presione <Esc> para cancelar"
			?? beep
			ktecla    = 0
			DO WHILE ktecla <> ESCAPE .AND. ( .NOT. FLOCK())
				ktecla    = INKEY()
			ENDDO
			IF ktecla <> ESCAPE
				v_fun = .T.
			ENDIF
		ENDIF
	CASE btipo = 4   && Intenta bloquear archivo solo una vez
		v_fun   = .F.
		IF FLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH ALIAS() + " Archivo ocupado. El proceso se cancela"
			?? beep
		ENDIF
ENDCASE
RETURN v_fun

FUNCTION f_appd
*--------------
APPEND BLANK
v_fun = RLOCK() .OR. f_lock(1)

RETURN v_fun


PROCEDURE logos
*--------------
PARAMETERS rotulo1,rotulo2,tiempo

ACTIVATE SCREEN

DO CASE
	CASE PARAMETERS() = 0
		STORE SPACE(80) TO rotulo1,rotulo2
		tiempo = 0
	CASE PARAMETERS() = 1
		STORE SPACE(80) TO rotulo2
		tiempo = 0
	CASE PARAMETERS() = 2
		tiempo = 0
ENDCASE

FOR i=0 TO 39 STEP -1
	@ 00,i    SAY  C                     COLOR SCHEME  c_borde
	@ 00,79-i SAY  LEFT( rotulo1,i+1 )   COLOR SCHEME  c_fondo

	@ 24,79-i SAY  C                     COLOR SCHEME  c_borde
	@ 24,00   SAY  RIGHT( rotulo2,i+1 )  COLOR SCHEME  c_fondo

	FOR y = 0 TO tiempo
	ENDFOR

ENDFOR

FOR i=40 TO 0 STEP -1    &&  -8
	@ 00,79-i SAY  SUBSTR( rotulo1,2*(40-i),i+1)   COLOR SCHEME  c_fondo
	@ 00,i    SAY  LEFT( rotulo1,2*(40-i))         COLOR SCHEME  c_borde
	@ 24,00   SAY  SUBSTR( rotulo2,i,i)            COLOR SCHEME  c_fondo
	@ 24,i    SAY  RIGHT( rotulo2,2*(40-i))        COLOR SCHEME  c_borde

	FOR y = 0 TO tiempo
	ENDFOR
ENDFOR

** SET HOUR TO 24
** SET CLOCK ON

RETURN


FUNCTION yesno
*-------------
PARAMETERS msgwords,posi,posc

IF !WEXIST('yesno')
	DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5
ENDIF
 
IF PARAMETERS()>1
	posi = IIF(posi > 19, 19, posi)
	colp = 27
	IF PARAMETERS()>2
		colp = IIF(posc>28,29,posc)
	ENDIF
	MOVE WINDOW yesno TO posi,colp
ENDIF

ACTIVATE WINDOW yesno

msgwords = ALLTRIM(msgwords)
_ln      = LEN(msgwords)
_colw    = WCOLS()
msgwords = IIF(_ln>2*_colw,PADR(msgwords,2*_colw,' '),msgwords)
_ln      = LEN(msgwords)

msg1     = IIF(_ln>_colw,LEFT(msgwords,_colw),msgwords)
msg2     = IIF(_ln>_colw,SUBSTR(msgwords,_colw+1),"")

IF _ln>_colw
	FOR i=0 TO _colw/3-1
		IF SUBSTR(msg1,_colw-i,1) == " "
			msg1 = LEFT(msg1,_colw-i)
			msg2 = SUBSTR(msgwords,_colw-i+1)
			msg2 = IIF(LEN(msg2)>_colw,LEFT(msg2,_colw),msg2)
			EXIT
		ENDIF
	ENDFOR
ENDIF

@ 00,WCOLS()/2-LEN(msg1)/2  SAY msg1
IF !EMPTY(msg2)
	@ 01,WCOLS()/2-LEN(msg2)/2  SAY msg2
ENDIF
v_fun = .F.
@ 02,10 GET _nosi FUNCTION '*TH \!\<No;\?\<Si' DEFAULT 1;
	SIZE 1,10,8
READ CYCLE
IF LASTKEY() # 27
	v_fun = IIF(_nosi = 1,.F.,.T.)
ENDIF
DEACTIVATE WINDOW yesno
RETURN v_fun

PROCEDURE standby
*----------------
PARAMETERS msgwords,posi,posc

IF !WEXIST('standby')
	DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5
ENDIF

IF PARAMETERS()>1
	posi = IIF(posi > 19, 19, posi)
	colp = 27
	IF PARAMETERS()>2
		colp = IIF(posc>28,29,posc)
	ENDIF
	MOVE WINDOW standby    TO posi,colp
ENDIF

ACTIVATE WINDOW standby IN SCREEN

msgwords = ALLTRIM(msgwords)
_ln      = LEN(msgwords)
_colw    = WCOLS()
msgwords = IIF(_ln>2*_colw,PADR(msgwords,2*_colw,' '),msgwords)
_ln      = LEN(msgwords)

msg1     = IIF(_ln>_colw,LEFT(msgwords,_colw),msgwords)
msg2     = IIF(_ln>_colw,SUBSTR(msgwords,_colw+1),"")

IF _ln>_colw
	FOR i=0 TO _colw/3-1
		IF SUBSTR(msg1,_colw-i,1) == " "
			msg1 = LEFT(msg1,_colw-i)
			msg2 = SUBSTR(msgwords,_colw-i+1)
			msg2 = IIF(LEN(msg2)>_colw,LEFT(msg2,_colw),msg2)
			EXIT
		ENDIF
	ENDFOR
ENDIF

@ 00,WCOLS()/2-LEN(msg1)/2  SAY msg1
IF !EMPTY(msg2)
	@ 01,WCOLS()/2-LEN(msg2)/2  SAY msg2
ENDIF

@ 02,_colw/2-16 SAY '<Pres. una tecla para continuar>'
_ss = INKEY(0)

DEACTIVATE WINDOW standby

MOVE WINDOW standby TO 19,27

RETURN             && standby

******************************************************************
* PROCEDURE fox_errs
* Rutina que controla el error al agregar un nuevo registro
* Para los demas casos genera un archivo core.fox que contiene
* el estado de la memoria al momento de ocurrir el error
*
* Parametros : Enviar a esta rutina la funcion SYS(16)
******************************************************************

PROCEDURE fox_errs
*-----------------
PARAMETERS prg_error
DO CASE
	CASE ERROR() = 108
		DO standby WITH "Un momento por favor el archivo est  en uso"
		RETRY
	CASE ERROR() = 109
		*- Record used by another
		WAIT WINDOW 'Registro est  siendo modificado por otro usuario.' NOWAIT
		RETURN
	CASE ERROR() = 130
		*- Record is not locked
		KEYBOARD CHR(13)
		RETURN
	CASE ERROR() = 125
		IF yesno('La Impresora no est  lista. ¨Se reintenta impresi¢n?')
			SET DEVICE TO PRINT
			RETRY
		ELSE
			SET DEVICE TO SCREEN
			SET PRINTER TO
			RETURN
		ENDIF

	OTHERWISE
		nom_usr   = SYS(30)
		num_error = ERROR()
		des_error = MESSAGE()
		lin_error = MESSAGE(1)
		dbf_actua = DBF()
		IF SYS(21) <> '0'
			idx_actua = NDX(VAL(SYS(21)))
		ENDIF
		SAVE SCREEN TO pnt_error
		CREATE VIEW core.viw
		SAVE TO "core.fox"

		DO poperror WITH "El sistema acaba de detectar un error interno. Avise al  rea de Sistemas." + CHR(13) + CHR(13) + ;
			"Tome nota de la siguiente descripci¢n:" + CHR(13) + ;
			"Programa: " + ALLTRIM( prg_error ) + CHR(13) + ;
			"L¡nea   : " + ALLTRIM( lin_error ) + CHR(13) + ;
			"Error   : " + ALLTRIM( des_error )

		IF worker
			ON ERROR
			CANCEL
		ENDIF
		QUIT
ENDCASE
RETURN

FUNCTION val_fun
*---------------
PARAMETER v__al,v__dev,v__bus,v__cod,v__tipo,v__x,v__y
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo, mvali, mrec
* Alias :     v__al
* Var.Devol.: v__dev
* Var.Mostr.: v__bus
* Var.Modif.: v__cod
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y
medita = (PARAMETERS()>=5) .AND.  (v__tipo # 3)
mmsg   = (PARAMETERS()=7)  .AND.  (v__tipo # 3)
mvali  = (PARAMETERS()=5)  .AND.  (v__tipo = 3)
mrec   = IIF(EOF(),-1,RECNO())
malias = ALIAS()
*SUSP
IF medita
	_campo = TRIM(malias)+'.'+IIF(v__tipo >1, VARREAD(), v__dev)
ENDIF
SELECT &v__al
_oldwnd = WOUTPUT()

IF !medita
	SEEK v__cod
	v_fun = IIF(mvali,FOUND(),IIF(FOUND(),&v__bus,""))
 ELSE
	IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
		GO TOP
		IF EOF()
			DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
			v_fun = .F.
		 ELSE
			IF (v__tipo#2)
				ACTIVATE SCREEN
			ENDIF
			DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD &v__bus
			ON SELECTION POPUP v__xx DEACTIVATE POPUP
			ACTIVATE POPUP v__xx
			RELEASE POPUP v__xx
			
			IF !EMPTY(_oldwnd) .AND. v__tipo#2
				ACTIVATE WINDOW &_oldwnd
			ENDIF
			IF LASTKEY()=27
				v_fun = .F.
			 ELSE
				v__cod = &v__dev
				IF mmsg
					@ v__x,v__y SAY &v__bus
				ENDIF
				IF !EMPTY(malias)
					SELECT (malias)
					IF (v__tipo  =2)
						IF mrec>0
							GO mrec
							REPLACE &_campo WITH v__cod
							v_fun = .T.
						ELSE
							DO standby WITH " El archivo est  vac¡o "
							v_fun = .F.
						ENDIF
					ELSE
						v_fun = .T.
					ENDIF
				ELSE
					DO standby WITH "²²²²²²²²²² NO HAY ARCHIVO ABIERTO ²²²²²²²²²²"
				ENDIF
			ENDIF
		ENDIF
	ELSE
		SEEK IIF(v__tipo#2,v__cod,&_campo)
		IF mmsg .AND. FOUND()
			@ v__x,v__y SAY &v__bus
		ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF

IF EMPTY(malias)
	SELECT 0
ELSE
	SELECT (malias)
ENDIF

IF !EMPTY(ALIAS())
	IF mrec>0
		GO mrec
	ELSE
		GO BOTTOM
	ENDIF
ENDIF
RETURN v_fun

FUNCTION val_para
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
DO CASE

	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma

SEEK filtro+mvalor
*IF .NOT. FOUND() .AND. mVariable<>'V'
IF !FOUND() .AND. !mvariable $'VZ'

	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	SET CONFIRM ON
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = ALLTRIM(parma.codigo)
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF

DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable=='B'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mvalor
		RETURN ' '
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE

FUNCTION val_read
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
PRIVATE _fun
_fun = .T.
IF LASTKEY() # 27
	IF .NOT. yesno('¨ Est n correctos los datos ?')
		_fun = .F.
	ENDIF
ENDIF

RETURN (_fun)

PROCEDURE reporte
**-----------------------------------------------------------------------
** Programa   : REPORTE.PRG
** Descripcion: Impresion de reportes utilizando archivos de reporte.
** Par metros :
**               _Tipo : tipo de reporte (1=con ambiente, 2=sin ambiente)
**               _Form : archivo de reporte
**               _Tit  : t¡tulo de la ventana
**               Num_C : N£mero de Copias.
**                 _wp : Si se envia a Word Perfect
**               Ran_pg: Si se pregunta un rango de paginas.
** -----------------------------------------------------------------------

PARAMETER _tipo, _form, _tit, num_c, _wp, ran_pg

**-- Environment
PRIVATE _wndold

_wndold = WOUTPUT()

SET ESCAPE ON
ON ESCAPE STORE .F. TO printing
_conso = SET("CONSOLE")
SET CONSOLE ON

**-- Verifica Existencia de windows

DEFINE WINDOW msg2use   FROM 07,02 TO 11,77 DOUBLE FLOAT SHADOW

*- Si no va a WP
_wp = .F.
IF PARAMETERS()<5
	_wp = .F.
ENDIF

*- Numero de copias
IF PARAMETER()<4
	num_c = 1
ENDIF
IF num_c # 1
	DEFINE WINDOW _xyx FROM 15,45 TO 19,70 COLOR SCHEME 10 TITLE " # DE COPIAS "
	ACTIVATE WINDOW _xyx
	CLEAR
	@ 1,5 SAY " N§ Copias : " GET num_c VALID(num_c>0) PICTURE "99"
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
	_ncopies = num_c
ENDIF

*- Rango de Paginas
IF PARAMETER() >= 6 .AND. ran_pg
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " Rango de P ginas "
	ACTIVATE WINDOW _xyx
	CLEAR
	vpbpage = 1
	vpepage = 32767
	@ 0,5 SAY "Inicio : " GET vpbpage PICTURE "99,999" VALID vpbpage <= vpepage
	@ 1,5 SAY "   Fin : " GET vpepage PICTURE "99,999" VALID vpbpage <= vpepage;
		.AND. vpepage <= 32767
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA

_dest = 'Pantalla '
IF !_wp
	ACTIVATE WINDOW msg2use
	_dest = 'Impresora'
	TITLE = ' DESTINO DE IMPRESION '
	p_fil = SPACE(8)
	_dest1 = 1
	@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _dest PICTURE "@M Impresora,Pantalla,Archivo"
	READ
	IF _dest = 'Archivo  '
		CLEAR
		@ 01,20 SAY 'Nombre del Archivo :' GET p_fil PICTURE "NNNNNNNN" VALID !EMPTY(p_fil)
		READ
	ENDIF
	p_fil = ALLTRIM(p_fil) + ".LST"
	RELEASE WINDOW msg2use
ENDIF

IF LASTKEY() = 27
	IF !EMPTY(_wndold)
		ACTIVATE WINDOW &_wndold
	ELSE
		ACTIVATE SCREEN
	ENDIF
	SET CONSOLE &_conso
	RETURN
ENDIF
impre = (_dest='Impresora')

IF !impre .AND. _dest = "Pantalla "
	p_fil = SYS(3)+".LST"
ENDIF

printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),15))
		IF !yesno("¨Imprime en impresora local?")
			**-- Impresora de red.
			SET PRINTER TO \\win98info\lq1050-siste
*			SET PRINTER TO \\spooler\nb
		ENDIF
	ENDIF
	IF !ready2pr()
		printing = .F.
	ENDIF
ENDIF

IF printing
	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		xDirImp = SET("PRINT",1)
		p_fil = SYS(3) + ".LST"
		SET PRINTER TO &p_fil

		IF _tipo = 1
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form ENVIRONMENT NOEJECT TO PRINT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form ENVIRONMENT NOEJECT TO pri
			ENDIF
		ELSE
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form NOEJECT TO PRINT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form NOEJECT TO PRINT
			ENDIF
		ENDIF
		SET PRINTER TO           && Si era la impresora server, libera el spool
		SET PRINTER TO &xDirImp
		
		IF !_wp
			FOR i=1 TO num_c
				IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
					!TYPE &p_fil > &xDirImp
*					!TYPE &p_fil >prn
				ENDIF
				IF LASTKEY()=27
					EXIT
				ENDIF
			ENDFOR
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF

		ERASE &p_fil

	ELSE
		IF _tipo = 1
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
			ENDIF
		ELSE
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
			ENDIF
		ENDIF

		IF !_wp
			IF _dest = "Pantalla "
				MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
				IF yesno("¨Imprime el reporte ? ")
				****
					IF !EMPTY(LEFT(SYS(0),15))
						IF !yesno("¨Imprime en impresora local?") AND LASTKEY()#27
							**-- Impresora de red.
							SET PRINTER TO \\win98info\LQ1050-SISTE
						ENDIF
					ENDIF
					xDirImp = SET("PRINT",1)
				****
					SET PRINTER TO &xDirImp

					FOR i=1 TO num_c
						IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
							!TYPE &p_fil > &xDirImp
*							!TYPE &p_fil >prn
						ENDIF
						IF LASTKEY()=27
							EXIT
						ENDIF
					ENDFOR
				ENDIF
			ENDIF
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF

		IF _dest # "Archivo  "
			ERASE (p_fil)
		ENDIF
	ENDIF

	*   SS = INKEY(10)
	DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'EL REPORTE HA SIDO CANCELADO.'
ENDIF
RELEASE WINDOW msg2use
ON ESCAPE
SET ESCAPE OFF
IF _tipo = 1
	CLOSE DATA
ENDIF
IF !EMPTY(_wndold)
	ACTIVATE WINDOW &_wndold
ELSE
	ACTIVATE SCREEN
ENDIF
SET CONSOLE &_conso
RETURN

PROCEDURE repprg
*---------------
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**               _PrgRpt : Programa reporteador.

PARAMETER  _prgrpt, _tit, _copia, _wp, ran_pg
PRIVATE  resul
_wndold = WOUTPUT()

_conso = SET("CONSOLE")


resul = 'OK'

IF PARAMETERS()<3
	_copia = 1
ENDIF
IF _copia # 1
	DEFINE WINDOW _xyx FROM 15,45 TO 19,70 COLOR SCHEME 10 TITLE " # DE COPIAS "
	ACTIVATE WINDOW _xyx
	CLEAR
	@ 1,5 SAY " N§ Copias : " GET _copia VALID(_copia>0) PICTURE "99"
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
	_ncopies = _copia
ENDIF

IF PARAMETERS()=3
	_wp = .F.
ENDIF

*- Rango de Paginas
IF PARAMETER() >= 5 .AND. ran_pg
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " Rango de P ginas "
	ACTIVATE WINDOW _xyx
	CLEAR
	vpbpage = 1
	vpepage = 32767
	@ 0,5 SAY "Inicio : " GET vpbpage PICTURE "99,999" VALID vpbpage <= vpepage
	@ 1,5 SAY "   Fin : " GET vpepage PICTURE "99,999" VALID vpbpage <= vpepage;
		.AND. vpepage <= 32767
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
ENDIF

**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USE")
	DEFINE WINDOW msg2use   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
_dest = 'Pantalla '
IF !_wp

ACTIVATE WINDOW msg2use
_dest = 'Impresora'
TITLE = ' DESTINO DE IMPRESION '
p_fil = SPACE(8)
_dest1 = 1
@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _dest PICTURE "@M Impresora,Pantalla,Archivo"
READ
IF _dest = 'Archivo  '
	CLEAR
	@ 01,20 SAY 'Nombre del Archivo :' GET p_fil PICTURE "NNNNNNNN" VALID !EMPTY(p_fil)
	READ
ENDIF
p_fil = ALLTRIM(p_fil) + ".LST"
RELEASE WINDOW msg2use
endif

IF MOD(READKEY(),256)=12
	ON KEY
	SET ESCAPE OFF
	ACTIVATE SCREEN
	RETURN
ENDIF
impre = (_dest='Impresora')

IF !impre .AND. _dest = "Pantalla "
	p_fil = SYS(3)+".LST"
ENDIF

printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),15))
		IF !yesno("¨Imprime en impresora local?")
			**-- Impresora de red.
			SET PRINTER TO \\ibm_pc\printq_0=lpt1
			SET PRINTER TO \\spooler\nb
		ENDIF
	ENDIF
	IF !ready2pr()
		printing = .F.
	ENDIF
ENDIF


IF printing

	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit ;
		FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
IF impre
			FOR v = 1 TO _copia
				IF ready2pr() .AND. IIF(_copia>1,yesno("Copia "+STR(v,2)+" .¨Prepare el papel. Listo?"),.T.)
				*!TYPE &p_fil>prn
				    DO &_prgrpt WITH 3					
				ENDIF
			ENDFOR


*     DO &_prgrpt WITH 3

ELSE

		@ 02,20  SAY  '** Reporte en ejecuci¢n **'

		DO &_prgrpt WITH 2

	IF _wp 
		ACTIVATE SCREEN
		RESTORE SCREEN FROM pantalla
		RUN FOXSWAP wp &p_fil
	ELSE
		MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
	ENDIF
	IF resul = 'OK'
		IF yesno("¨ Imprime el Reporte ?")
			FOR v = 1 TO _copia
				IF ready2pr() .AND. IIF(_copia>1,yesno("Copia "+STR(v,2)+" .¨Prepare el papel. Listo?"),.T.)
					!TYPE &p_fil>prn
				ENDIF
			ENDFOR
		ENDIF
	ELSE
	resul = 'OK'
	ENDIF
	ERASE (p_fil)
ENDIF
DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN

RETURN

FUNCTION ready2pr
*----------------
	DO WHILE .NOT. PRINTSTATUS()
		IF !yesno('La Impresora no esta lista! Se reintentar  la impresi¢n...')
			RETURN .f.
		ENDIF
	ENDDO
RETURN .t.

PROCEDURE err_input
*------------------
* window definido en objects

ACTIVATE WINDOW err_input
@ 00,01 SAY 'DATO NO VALIDO'
aa = INKEY(1,'H')
DEACTIVATE WINDOW err_input
RETURN

PROCEDURE clrscr
*---------------
* Autor: CCA
* Borra la pantalla desde el centro
*
PRIVATE fil,COL
fil = 11
COL = 39
DO WHILE (COL > 0) .AND. (fil > 0)
	@ fil,COL CLEAR TO 23-fil,79-COL
	fil = fil - 1
	COL = COL - 3
	= INKEY(.001,'H')
ENDDO
CLEAR
RETURN

FUNCTION poperror
*----------------
PARAMETERS err_mess, _xtime
PRIVATE cur_color, cur_curs, bord_str, err_mess, say_mess
PRIVATE num_lines, start_line, cur_width, i, rvalue
PRIVATE cur_win, _mens

************************************************************
*  poperror(<expC>)                                        *
*  Version # 2.0  FP          Date: 07-20-91               *
*  Programmed by: Malcolm C. Rubel                         *
*                                                          *
*  Copyright (c) 1991                                      *
*  Performance Dynamics Associates                         *
*  All Rights Reserved                                     *
*                                                          *
*  Note: Procedure pops up an error in lower right corner  *
*  of screen and waits for a keypress to go back to screen.*
*  Modified 7-20-91 to include PUSH/POP key.               *
* Modificado por Cesar Chavarry A.: Pintado de letras      *
************************************************************

PUSH KEY CLEAR
**= beep(1)

cur_win = WOUTPUT()
cur_width = SET('memowidth')  &&  save the old setting
SET MEMOWIDTH TO 48

num_lines = MEMLINES(err_mess) &&  how long is message?
Altura    = num_lines + 3 + 2  &&  Altura de la ventana a mostrar.
start_line= (25 - Altura)/2    &&  starting row for box

DEFINE WINDOW poperr FROM start_line,13 TO start_line+Altura,66 DOUBLE ;
              SHADOW COLOR SCHEME 5
ACTIVATE WINDOW poperr

IF num_lines = 1
  _mens = PADC(err_mess,48)
  FOR j = 1 TO LEN(_mens)
    @ 1,1+j SAY SUBSTR(_mens,j,1)
    IF (PARAMETERS() >= 2 .AND. _xtime) .OR. PARAMETERS < 2
      =INKEY(0.001,' ')
    ENDIF
  ENDFOR
  i = 2
ELSE
  FOR i = 1 TO num_lines
    say_mess = LTRIM( MLINE(err_mess,i) )
    * @ i,02 SAY LTRIM(say_mess)
    FOR j = 1 TO LEN(say_mess)
       @ i,1+j SAY SUBSTR( say_mess,j,1)
       IF (PARAMETERS() >= 2 .AND. _xtime) .OR. PARAMETERS() < 2
         =INKEY(0.001,' ')
       ENDIF
    ENDFOR

  ENDFOR
ENDIF

@ i+1,01 SAY replicate(chr(196),49)      &&  draw line
_mens = 'Presione una tecla para continuar ....'
FOR j = 1 TO LEN(_mens)
   @ i+2,1+j SAY SUBSTR(_mens,j,1)
   =INKEY(0.001,' ')
ENDFOR

rvalue = inkey(0,'hm')        &&  hide the cursor

IF wexist('poperr')
  RELEASE WINDOW poperr
ENDIF

IF empty(cur_win)
  ACTIVATE SCREEN
ENDIF

SET MEMOWIDTH TO cur_width
POP KEY

RETURN(rvalue)

FUNCTION isdisket
*----------------
PARAMETER _drive
PRIVATE m.drive, vret

vret = .T.
m.drive = _drive
LOAD isdiskin.bin
CALL isdiskin WITH m.drive
DO WHILE m.drive='0:'
	??CHR(7) + CHR(7)
	DO standby WITH '! El drive '+_drive+ ' no est  listo !'
	IF LASTKEY()=27
		vret = .F.
		EXIT
	ENDIF
	m.drive = _drive
	CALL isdiskin WITH m.drive
ENDDO
_drive = m.drive
RELEASE MODULE isdiskin.bin

RETURN vret

PROCEDURE ESPERA 
*-----------------------------------------------------------
* Esta funcion activa una pantalla de Espera 
* en caso de que se procesada a indexar o demorar el proceso
* Desarrollado por : FMC Saiki. Sucursal Piura
* Fecha : 11/01/96
*-----------------------------------------------------------
PARAMETER festado,fmensaje   
 DEFINE WINDOW Espera  FROM 21,05 TO 23,75 COLOR SCHEME 1
*DEFINE WINDOW Espera  FROM 11,05 TO 13,75 COLOR SCHEME 5
   DO CASE 
      CASE festado=1
      	   ACTIVATE WINDOW Espera
	  	   @ 0,0 SAY PADC( ALLTRIM(fmensaje) , WCOLS() )COLOR W+/BR*
	  	   RETURN
	  CASE festado=2
		   DEACTIVATE WINDOW Espera
		   RETURN 
	ENDCASE
RETURN

PROCEDURE Clave
** --------------------------------
** Clave de Acceso
** AUTOR : M.E.R.A. 2,002.  
** --------------------------------
PRIVATE xPassword,m.key,vRetorno,m.cur_curs,m.cur_win,m.Usuario

*-- Regresa las siguientes variables
*-- El archivo de usuarios se asume que debe estar abierto.

xPassword = ''
vRetorno = .F.
m.KEY = 0

m.cur_curs = SET('cursor') = 'ON'   &&  current cursor state
SET CURSOR ON

m.cur_win = WOUTPUT()

SELECT Usua
DEFINE WINDOW pass_win FROM 08,15 TO 15,65 DOUBLE ;
	TITLE '  Usuario  ' SHADOW COLOR SCHEME 21
ACTIVATE WINDOW pass_win

m.Usuario = SPACE(15)
@ 01,03 SAY 'USUARIO'
@ 01,11 GET m.Usuario VALID Val_Fun("Usua","Usuario","Nombre",@m.Usuario,1,2,11)
READ
IF LASTKEY()#27
	SEEK m.Usuario
	@ 03,09 SAY 'Ingrese su password'
	@ 05,15 SAY ''
	DO WHILE m.key # 13 .AND. m.key#27             &&  accept until car_return
*		SET COLOR OF NORMAL TO X
		m.key = INKEY(0)
		
		DO CASE
			CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122) .OR. BETWEEN(m.key,48,57) .OR. BETWEEN(m.key,164,165)
				xPassword = xPassword + CHR(m.key)
			CASE m.key = 19 .OR. m.key = 127 .OR. m.key = 7
				@ ROW(),COL()-1 SAY ' '
				@ ROW(),COL()-1 SAY ''
				xPassword = SUBSTR(xPassword,1,LEN(xPassword)-1)
			OTHERWISE
		ENDCASE
		
*		SET COLOR OF NORMAL TO
		@ 05,15 SAY REPLICATE('',LEN(xPassword))
	ENDDO
ENDIF

RELEASE WINDOW pass_win

IF EMPTY(cur_win)
	ACTIVATE SCREEN
ENDIF

IF .NOT. cur_curs             &&  if cursor was off
	SET CURSOR OFF            &&  set it back on
ENDIF

IF LASTKEY()#27
	vEncrip = chrtran(UPPER(xPassword),'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
    	                               'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
    	             
	IF vEncrip==ALLTRIM(Usua.Clave)
		@ 22,25 SAY IIF(VAL(SUBSTR(TIME(),1,2)) < 12,'Buenos Dias ',;
			'Buenas Tardes ') + ALLTRIM(Usua.Nombre)
		@ 23,25 SAY 'USUARIO : ' + ALLTRIM(Usua.Nombre)
		WAIT '' TIMEOUT 2
		@  23,25,23,80  BOX "°°°°°°°°°" COLOR SCHEME c_fondo
		vCoddep = Usua.Coddep
		mRet = .T.
	 ELSE
		mRet = .F.
	ENDIF
 ELSE
	mRet = .F.
ENDIF

RETURN(mRet)


FUNCTION SavCon
*---------------
PARAMETERS xNum
USE Bitacora IN 0 ALIAS Bitacora
PRIVATE _Hora
SELECT Bitacora
_hora  = TIME()
IF xNum = 1
	IF F_Appd()
		REPLACE CodUsu  WITH vUser_id,;
				Conex	WITH vconex,;
				machine	WITH vmaq,;
				tipmaq	with substr(sys(17),3),;
				Prog	WITH SYS(16,1),;
				Hora	WITH _hora,;
				Fecha	WITH DATE(),;
				Llave	WITH vLlav
		UNLOCK
	ENDIF
	vUsurec = recno()
 ELSE
	GO vUsurec
	REPLACE Salida WITH _hora
ENDIF
*vUsurec = recno()
USE
RETURN


FUNCTION ValEstCli
*-----------------
PRIVATE cAli,mRet,cOrd,lBorra
cAli = ALIAS()

IF !USED("Creditos")
	USE Creditos IN 0 ORDER TAG Creditos2
	lborra = .T.
ELSE
	SELE Creditos
	cOrd = ORDER()
	SET ORDER TO Creditos2
	lBorra = .F.
ENDIF

mRet = .T.
SELE Creditos
IF SEEK(m.CodCli)
	lDeuda = .F.
	SCAN WHILE CodCli = m.CodCli
		IF Creditos.Saldo > 0 AND Creditos.Estado#'99'
			lDeuda = .T.
		ENDIF
	ENDSCAN
	
	IF lDeuda
		IF YesNo("El Cliente Tiene Deuda Pendiente. desea Continuar")
			* mRet = .T.
		ELSE
			m.CodCli = SPACE(6)
			mRet = .F.
		ENDIF
	ENDIF
ENDIF

IF lBorra
	USE IN Creditos
ELSE
	SET ORDER TO cOrd
ENDIF

SELE &cAli

RETURN mRet

*FUNCTION ValEstCli
*-----------------
PRIVATE cAli,mRet

cAli = ALIAS()
mRet = .T.
IF SEEK(m.CodCli,'Clien')
	SELE Clien
	IF Estado = '10'
		IF YesNo("El Cliente Tiene Deuda Pendiente. desea Continuar")
			mRet = .T.
		ELSE
			m.CodCli = SPACE(6)
			mRet = .F.
		ENDIF
	ENDIF
ENDIF

RETURN mRet


FUNCTION FunBusDet			&& Realiza b£squeda directa
*-----------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = IIF(!EOF(),RECNO(),-1)

DEFINE POPUP pLista FROM 15,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Codigo      '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre      '

ON SELECTION POPUP pLista  DEACTIVATE POPUP

ACTIVATE POPUP pLista
RELEASE  POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		vBusca = SPACE(6)
		vNombre = "Codigo :"
		SET ORDER TO TAG Clientes1
	CASE BAR() =  2
		vBusca = SPACE(30)
		vNombre = "Nombre : "
		SET ORDER TO TAG Clientes2
	OTHERWISE
		vBusca  = ''
		vNombre = ''
		SET ORDER TO
ENDCASE

IF LASTKEY()#27
	DEFINE WINDOW lisTA FROM 13,12 TO 16,68 DOUBLE ;
		TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 10
	
	ACTIVATE WINDOW lisTA
	@ 1,2 SAY vNombre GET vBusca
	READ VALID val_read()
	DEACTIVATE WINDOW lista
	RELEASE WINDOW LISTA
ENDIF

IF EMPTY(vBusca) OR LASTKEY()=27
	* No hace nada
 ELSE
	SEEK ALLTRIM(vBusca)
	IF !FOUND()
		DO standby WITH "Cliente no Existe"
		IF vtemp = -1
			GO BOTT
		 ELSE
			GOTO vtemp
		ENDIF
	 ELSE
		* No hace Nada
	ENDIF
ENDIF

SET ORDER TO &cOrd1

RETURN


FUNCTION Val_Clien
*-----------------
IF SEEK("CORRELCLIENT"+m.codcli,"Parma")
	m.codcli = PADL(ALLTRIM(STR(Parma.NumEnt+1)),6,'0')
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodCli)
	DO StandBy WITH "El Codigo esta vacio"
	RETURN .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodCli)
		DO standby WITH 'Ya esta Registrado este Cliente'
		RETURN .F.
	ENDIF
ENDIF
RETURN .T.

***************************
* PARA DIFUNTOS Y TRASLADOS
***************************
FUNCTION Cor_Dif
*---------------
PRIVATE mRet
cAlias = ALIAS()
SELE &cAlias
mRet = .T.
IF SEEK("CORRELDIFUNT"+m.codcem,"Parma")
	m.codDif = m.CodCem + PADL(ALLTRIM(STR(Parma.NumEnt+1)),6,'0')
	SHOW GET m.CodDif
 ELSE
	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF
IF EMPTY(m.CodDif)
	DO StandBy WITH "El Codigo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodDif)
		DO standby WITH 'Ya esta Registrado este Difunto'
		mRet = .F.
	ENDIF
ENDIF
SELE (calias)
RETURN mRet

FUNCTION IniVar
*--------------
DO CASE
	CASE m.TipEnt = "1"
		m.CodMau  = SPACE(6)
	CASE m.TipEnt = "2"
		m.CodCuar = SPACE(4)
		m.CodNic  = SPACE(4)
		m.Fila    = SPACE(1)
		m.Columna = SPACE(3)
	CASE m.TipEnt = "3"
		m.CodMau  = SPACE(6)
		m.CodCuar = SPACE(4)
		m.CodNic  = SPACE(4)
		m.Fila    = SPACE(1)
		m.Columna = SPACE(3)
ENDCASE

SHOW GETS

RETURN .T.

*--- Para Recibos

FUNCTION Cor_Rec
*---------------
PRIVATE cMes,cAno,cAlias,mRet
cAlias = ALIAS()
SELE &cAlias
mRet = .T.
cMes = PADL(ALLTRIM(STR(MONT(m.FecRec))),2,'0')
cAno = RIGH(STR(YEAR(m.FecRec),4),2)

IF SEEK("RECIBO"+cAno+cMes,"Parma")
	m.codRec =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	SHOW GET m.CodRec
 ELSE
*	SELE Parma
*	IF f_Appd()
*		REPLACE Tipo WITH 'RECIBO', Codigo WITH cAno, CodigoAux WITH cMes
*		UNLOCK
*	ENDIF
	m.codRec =  cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	SHOW GET m.CodRec

*	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF

SELE (calias)

IF EMPTY(m.CodRec)
	DO StandBy WITH "El C¢digo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodRec)
		DO standby WITH 'Ya esta Registrado este Difunto'
		mRet = .F.
	ENDIF
ENDIF

RETURN mRet

*FUNCTION CorCre
*--------------
PRIVATE cMes,cAno,cAlias,mRet
cAlias = ALIAS()
*SELE &cAlias
mRet = .T.
cMes = PADL(ALLTRIM(STR(MONT(m.FecCre))),2,'0')
cAno = RIGH(STR(YEAR(m.FecCre),4),2)

IF SEEK("CTAXCO"+cAno+cMes,"Parma")
	m.CodCre = cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	SHOW GET m.CodCre
 ELSE
	SELE Parma
	IF f_Appd()
		REPLACE Tipo WITH 'CTAXCO', Codigo WITH cAno, CodigoAux WITH cMes
		UNLOCK
	ENDIF
	m.CodCre = cAno+cMes+PADL(ALLTRIM(STR(Parma.NumEnt+1)),3,'0')
	SHOW GET m.CodCre
	
*	DO StandBy WITH "Parametro de correlativo no existe; favor avisar a sistemas"
ENDIF

SELE (calias)

IF EMPTY(m.CodCre)
	DO StandBy WITH "El C¢digo esta vacio"
	mRet = .F.
 ELSE
	nreg = RECNO()
	IF SEEK(m.CodCre)
		DO standby WITH 'Ya esta Registrado este Credito'
		mRet = .F.
	ENDIF
ENDIF

RETURN mRet

FUNCTION ValResNw
*----------------
mret = .T.
IF m.Reservado = 'N'
	m.Estado = "00"
	mRet = .T.
 ELSE
	xx = EscojeOFNw()
	IF !xx
		mRet = .F.
	ENDIF
ENDIF
RETURN mRet

PROCEDURE EscojeOFNw
*-------------------
PRIVATE cAlias,vFun
vFun = .F.
cAlias=ALIAS()
nReg = RECNO()

USE OcupFut  IN  0 ORDER TAG OcupFut1	ALIAS OcuFut

SELECT OcuFut

DEFINE POPUP pLista FROM 10,20 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por Solicitante  '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Ocupante '

ON SELECTION POPUP pLista  DEACTIVATE POPUP
ACTIVATE POPUP pLista

cOrd1 = ORDER()

IF LASTKEY()#27
	DO CASE
		CASE BAR() =  1
			SET ORDER TO TAG OcupFut3
		CASE BAR() =  2
			SET ORDER TO TAG OcupFut2
	ENDCASE
	
	DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
		TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
	
	GO TOP
	
	IF EOF()
		DO StandBy WITH "No Existen Registros Para Procesar"
		vFun = .F.
	 ELSE
		SET FILTER TO ESTADO='10'
		GO TOP
		IF EOF()
			DO StandBy WITH "No Existen Ocupaciones Futuras"
			vFun = .F.
		 ELSE
			vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
			DO Logos WITH Rotulo1,vTempo
			ON KEY LABEL F10 KEYBOARD CHR(23)
			
			BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					Cement.NomCem   :H='Cementerio' :30,;
					OcuFut.CodCli   :H='Solicitante',;
					OcuFut.NomOcu   :H='Ocupante'
					
			vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
			DO Logos WITH Rotulo1,vTempo
			IF LASTKEY()=27
				vFun = .F.
			 ELSE
		 		m.CodOCu	= OcuFut.CodOcu
				m.CodCem	= OcuFut.CodCem
				m.CodCuar	= OcuFut.CodCuar
				m.CodNic	= OcuFut.CodNic
				m.Fila		= OcuFut.Fila
				m.Columna	= OcuFut.Columna
				m.Estado	= "00"
				m.CodCli	= OcuFut.CodCli
				m.TipEnt	= "1"
				vFun = .T.
			ENDIF
		ENDIF
	ENDIF
ENDIF

*SHOW MENU mMenu
SET FILTER TO
SET ORDER TO &cOrd1
USE IN OcuFut
ON KEY LABEL F10
SELE (cAlias)
RETURN vFun

PROCEDURE EstMorCl
*-----------------
PRIVATE lBorra1,cOrd1,lBorra2,cOrd2
*set step on
DO Espera WITH 1,'Actualizando Estado de Clientes'
USE TmpCli IN 0
IF !USED("Ventas")
	USE Ventas IN 0 ORDER TAG Ventas2
	lborra1 = .T.
ELSE
	SELE Ventas
	cOrd1 = ORDER()
	lBorra1 = .F.
ENDIF

IF !USED("Clientes")
	USE Clientes IN 0 ORDER TAG Clientes1 ALIAS Clien
	lborra2 = .T.
ELSE
	SELE Clien
	cOrd2 = ORDER()
	lBorra2 = .F.
ENDIF

SELE TmpCli
GO TOP
IF !EOF()
	SCAN
		cCli = CodCli
		IF SEEK(cCli,"Ventas")
			SELE Ventas
			lDeuda = .F.
			SCAN WHILE Codcli = cCli
				IF Estado = '20'
					ldeuda = .T.
				ENDIF
			ENDSCAN
			IF !lDeuda
				SELE Clien
				IF SEEK(cCli)
					IF f_Lock(1)
						REPLACE Estado WITH '00'
					ENDIF
				ENDIF
				SELE TmpCli
				DELE NEXT 1
			ENDIF
		ENDIF
		SELE TmpCli
	ENDSCAN
ENDIF

USE IN TmpCli

IF lBorra1
	USE IN Ventas
ELSE
	SET ORDER TO cOrd1
ENDIF

IF lBorra2
	USE IN Clien
ELSE
	SET ORDER TO cOrd2
ENDIF

DO Espera WITH 2

RETURN

PROCEDURE ActKarCre
*------------------
PARAMETERS m.TipMov,m.CodMov,m.Fecha
PRIVATE mAlias
PUBLIC nSalFin

mAlias     = ALIAS()

SELE KarCre
SET ORDER TO KarCre2
mRet = .F.
lcancel = .F.
IF !SEEK(m.TipMov+m.CodMov)
	IF F_Appd()
		m.Correl = "A"
		GATHER MEMVAR
		REPLACE Estado WITH '00'
	ENDIF
	
	SET ORDER TO KarCre1
	
	SEEK m.CodCre
	IF FOUND()
		m.Correl = "00"
		SCAN WHILE Correl#"A"
			m.Correl = Correl
		ENDSCAN
		
		m.Correl = PADL(ALLTRIM(STR(VAL(m.Correl)+1)),2,"0")
		
		REPLACE Correl WITH m.Correl
	ENDIF
	
	SEEK m.CodCre			&&+m.Correl
	
	IF FOUND()
		m.TotInt = 0
		m.TotAmo = 0
		SCAN WHILE CodCre=m.CodCre			&& AND Correl=m.Correl
			m.TotInt = m.TotInt + MtoInt
			m.TotAmo = m.TotAmo + MtoAmo
			m.Saldo = (m.MtoCre + m.TotInt ) - m.TotAmo
			IF Saldo # m.Saldo OR totInt # m.TotInt OR TotAmo # m.TotAmo
				REPLACE Saldo  WITH m.Saldo;
						totInt WITH m.TotInt;
						TotAmo WITH m.TotAmo
			ENDIF
		ENDSCAN
	ENDIF
	mret = .T.
 ELSE
	SET ORDER TO KarCre1
	SEEK m.CodCre		&&+m.Correl
	IF FOUND()
		m.TotInt = 0
		m.TotAmo = 0
		SCAN WHILE CodCre=m.CodCre		&& AND Correl=m.Correl
			m.TotInt = m.TotInt + MtoInt
			m.TotAmo = m.TotAmo + MtoAmo
			m.Saldo = (m.MtoCre + m.TotInt ) - m.TotAmo
			IF Saldo#m.Saldo OR totInt # m.TotInt OR TotAmo # m.TotAmo
				REPLACE Saldo  WITH m.Saldo;
						totInt WITH m.TotInt;
						TotAmo WITH m.TotAmo
			ENDIF
		ENDSCAN
	ENDIF
	mret = .T.
ENDIF

IF mRet
	SELE Creditos
	IF SEEK(m.CodCre)
		REPLACE Estado	WITH IIF(m.Saldo=0.0,'10','20');
				TotAmo	WITH m.TotAmo;
				TotInt	WITH m.TotInt;
				Saldo	WITH m.Saldo;
				fUltPag	WITH m.fUltPag
	ELSE
		IF f_Appd()
			GATHER MEMVAR
			REPLACE Estado WITH IIF(m.Saldo=0.0,'10','20');
					Saldo WITH m.Saldo
		ENDIF
	ENDIF
ENDIF

nSalFin = m.Saldo
SELE (mAlias)
RETURN mRet

PROCEDURE RECALCULA
*------------------
PARAMETERS cCodCre
PRIVATE cOrd

cOrd = ORDER()
SET ORDER TO KarCre1
SEEK cCodCre
m.TotInt = 0
m.TotAmo = 0

IF FOUND()
	SCAN WHILE CodCre = cCodCre
		m.TotInt = m.TotInt + MtoInt
		m.TotAmo = m.TotAmo + MtoAmo
	ENDSCAN
	
	SKIP -1
	m.Saldo = (m.MtoCre+TotInt )-m.TotAmo
	REPLACE Saldo  WITH m.Saldo;
			totInt WITH m.TotInt;
			TotAmo WITH m.TotAmo
	
	SELE Creditos
	IF SEEK(cCodCre)
		REPLACE Saldo  WITH m.Saldo;
				totInt WITH m.TotInt;
				TotAmo WITH m.TotAmo
	ENDIF
 ELSE
ENDIF
SELE KarCre
SET ORDER TO (cOrd)
RETURN

PROCEDURE Val_Ce
*---------------
PARAMETERS xCadena
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Maestro
IF SEEK(xCadena)
*IF SEEK(m.Cod_Cement)
	xRet = Maestro.Nom_Cement
ELSE
	xRet =SPACE(30)
ENDIF

*@ ROW(),COL() SAY xRet

SELE (calias)

RETURN xRet

PROCEDURE Val_Cex
*---------------
PARAMETERS xCadena,XfIL,XcOL
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Maestro
IF SEEK(xCadena)
*IF SEEK(m.Cod_Cement)
	xRet = Maestro.Nom_Cement
ELSE
	xRet =SPACE(30)
ENDIF

@ xFil,xCol SAY xRet

SELE (calias)

RETURN xRet

PROCEDURE Val_Fall
*---------------
PARAMETERS xCadena
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Fallecid
*WAIT WIND XcADENA
IF SEEK(xCadena)
*IF SEEK(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
	xRet = PADR(ALLTRIM(Ape_Patern) + ' ' + ALLTRIM(Ape_Matern) + ', ' + ALLTRIM(Nombres),40,' ')
ELSE
	xRet =SPACE(40)
ENDIF

*@ ROW(),COL() SAY xRet

SELE (calias)

RETURN xRet

PROCEDURE Val_Fallx
*---------------
PARAMETERS xCadena,XfIL,XcOL
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Fallecid
*WAIT WIND XcADENA
IF SEEK(xCadena)
*IF SEEK(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
	xRet = PADR(ALLTRIM(Ape_Patern) + ' ' + ALLTRIM(Ape_Matern) + ', ' + ALLTRIM(Nombres),40,' ')
ELSE
	xRet =SPACE(40)
ENDIF

@ xFil,xCol SAY xRet

SELE (calias)

RETURN xRet

FUNCTION Val_Cuar
*----------------
PARAMETERS xCadena
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Fallecid
IF SEEK(xCadena)
*IF SEEK(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
	SELE Cuartel
	IF SEEK(STR(m.Cod_Cement,1)+PADL(Fallecid.Cod_Cuarte,3,'0'))
		IF Cod_Tipo = 1
			xRet = PADR(ALLTRIM(Nom_Cuarte),40,' ')
		ELSE
			xRet =SPACE(40)
		ENDIF
	ELSE
		xRet =SPACE(40)
	ENDIF
ELSE
	xRet =SPACE(40)
ENDIF

*@ ROW(),COL() SAY xRet

SELE (calias)

RETURN xRet

FUNCTION Val_Nic
*---------------
PARAMETERS xCadena
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Fallecid
IF SEEK(xCadena)
*IF SEEK(STR(m.Cod_Cement,1) + PADL(m.Cod_Fallec,6,'0'))
	SELE Cuartel
	IF SEEK(STR(m.Cod_Cement,1)+PADL(Fallecid.Cod_Cuarte,3,'0'))
		IF Cod_Tipo = 1
			xRet = Fallecid.Fila + ' / '+ ALLTRIM(STR(Fallecid.Columna))
		ELSE
			xRet =SPACE(40)
		ENDIF
	ELSE
		xRet =SPACE(40)
	ENDIF
ELSE
	xRet =SPACE(40)
ENDIF

*@ ROW(),COL() SAY xRet

SELE (calias)

RETURN xRet

FUNCTION Val_Mau
*---------------
PARAMETERS xCadena
PRIVATE cAlias,xRet
cAlias = ALIAS()
SELE Fallecid
IF SEEK(xCadena)
*IF SEEK(STR(m.Cod_Cement,1)+PADL(m.Cod_Fallec,6,'0'))
	SELE Cuartel
	IF SEEK(STR(m.Cod_Cement,1)+PADL(Fallecid.Cod_Cuarte,3,'0'))
		IF Cod_Tipo = 3 OR Cod_Tipo = 4
			xRet = PADR(ALLTRIM(Nom_Cuarte),40,' ')
		ELSE
			xRet =SPACE(40)
		ENDIF
	ELSE
		xRet =SPACE(40)
	ENDIF
ELSE
	xRet =SPACE(40)
ENDIF

*@ ROW(),COL() SAY xRet

SELE (calias)

RETURN xRet

PROCEDURE Val_Cu1
*----------------
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
	INDEX ON STR(Cod_Cement,1) + Nom_Cuarte TO (vIdx4)  FOR COD_TIPO#3 AND COD_TIPO#4
	GO TOP
	ACTIVATE SCREEN
*	ON KEY LABEL F2  DO Buscuar
	ON KEY LABEL F10 KEYBOARD CHR(23)
	DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusArt
	BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU key m.Cod_Cement;
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
		SET INDEX TO
		SET ORDER TO (cOrd)
		ERASE (vIdx4)
	ELSE
		V_FUN = .f.
		ON KEY LABEL F10 KEYBOARD CHR(23)
		UNLOCK ALL
		SET INDEX TO
		SET ORDER TO (cOrd)
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
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL SAY mdescri
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
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
	OTHERWISE            && En browse de edici¢n
*		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE
RETURN

PROCEDURE Val_Ce1
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
	BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU ;
	 FIELDS;
			Cod_Cement	:H='C¢digo'     ,;
			Nom_Cement	:H='Nombre':40
	ON KEY LABEL F10
	DEACTIVATE WINDOWS _BusArt
	RELEASE    WINDOW _BusArt
	
	IF Lastkey()#27
		m.Cod_Cement = STR(Maestro.Cod_Cement,1)
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
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL SAY mdescri
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
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
	OTHERWISE            && En browse de edici¢n
*		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE
RETURN

PROCEDURE ESPERA 
*-----------------------------------------------------------
* Esta funcion activa una pantalla de Espera 
* en caso de que se procesada a indexar o demorar el proceso
* Desarrollado por : FMC Saiki. Sucursal Piura
* Fecha : 11/01/96
*-----------------------------------------------------------
PARAMETER festado,fmensaje   
 DEFINE WINDOW Espera  FROM 21,05 TO 23,75 COLOR SCHEME 1
*DEFINE WINDOW Espera  FROM 11,05 TO 13,75 COLOR SCHEME 5
   DO CASE 
      CASE festado=1
      	   ACTIVATE WINDOW Espera
	  	   @ 0,0 SAY PADC( ALLTRIM(fmensaje) , WCOLS() )COLOR W+/BR*
	  	   RETURN
	  CASE festado=2
		   DEACTIVATE WINDOW Espera
		   RETURN 
	ENDCASE
RETURN
