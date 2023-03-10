* 靈컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴?
* ?  Adm_LIB.PRG                                                        ?
* ?                                                                     ?
* ?                 LIBRERIA DEL SISTEMA DE LA SBP                      ?
* ?                                                                     ?
* ?  Observaci줻: para Fox Pro 2.x                                      ?
* 聃컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴?
PROCEDURE Fox_ambi
*-----------------
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
SET CENTURY          ON  && Default
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
*SET DEFAULT TO           && LEAVE SET TO CURRENT DEFAULT DRIVE
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
* SET FIELDS         NOT RELEVANT TO SYSTEM ENVIRONMENT
* SET FILTER         NOT RELEVANT TO SYSTEM ENVIRONMENT
SET FIXED            ON
SET FORMAT           TO
SET FULLPATH         ON
* SET FUNCTION       && LEAVE DEFAULTS
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
* SET INDEX          NOT RELEVANT TO SYSTEM ENVIRONMENT
SET INSTRUCT         ON
SET INTENSITY        ON
IF worker
  SET logerror       ON
ELSE
  SET logerror       OFF
ENDIF
SET MARGIN TO        0
SET MARK TO          "-"  && THIS IS THE DATE FIELD SEPARATOR
*  THE DEFAULT IS "/"
*  WE'RE USING SOMETHING DIFFERENT HERE
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
_tabs =       "10, 20, 30, 40, 50, 60, 70"
_wrap =       .t.
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
			DO standby WITH "El registro del file" + ALIAS() + " est? siendo utilizado. Se cancela la operaci줻"
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

FUNCTION Val_Fun
*---------------
PARAMETER v__al,v__dev,v__bus,v__cod,v__tipo,v__x,v__y

PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo, mVali, mRec
* Alias :     v__al
* Var.Devol.: v__dev
* Var.Mostr.: v__bus
* Var.Modif.: v__cod
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y
mEdita = (parameters()>=5) .and.  (v__tipo # 3)
mMsg   = (parameters()=7)  .and.  (v__tipo # 3)
mVali  = (parameters()=5)  .and.  (v__tipo = 3)
mRec   = IIF(EOF(),-1,RECNO())
mAlias = ALIAS()

IF mEdita
   _campo = TRIM(mAlias)+'.'+IIF(v__tipo >1, VARREAD(), v__dev)
ENDIF

SELECT &v__al
_OldWnd = WOUTPUT()
IF !mEdita
   SEEK v__cod
   v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__bus,""))
ELSE
   IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
      GO TOP
      IF EOF()
         DO standby WITH "께께께께 NO HAY VALORES PARA ELEGIR 께께께께"
         v_fun = .F.
      ELSE
         IF (v__tipo#2)
            ACTIVATE SCREEN
         ENDIF
         DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD &v__bus
         ON SELECTION POPUP v__xx DEACTIVATE POPUP
         ACTIVATE POPUP v__xx
         RELEASE POPUP v__xx

         IF !EMPTY(_OldWnd) .AND. v__tipo#2
            ACTIVATE WINDOW &_OldWnd
         ENDIF
         IF Lastkey()=27
            v_fun = .f.
         ELSE
            v__cod = &v__dev
            IF mMsg
               @ v__x,v__y SAY &v__bus
            ENDIF
            IF !EMPTY(mAlias)
               SELECT (mAlias)
               IF (v__tipo  =2)
                  IF mRec>0
                     GO mRec
                     REPLACE &_campo WITH v__cod
                     v_fun = .T.
                  ELSE
                     DO standby WITH " El archivo est? vac죓 "
                     v_fun = .F.
                  ENDIF
               ELSE
                  v_fun = .T.
               ENDIF
            ELSE
               DO standby WITH "께께께께께 NO HAY ARCHIVO ABIERTO 께께께께께"
            ENDIF
         ENDIF
      ENDIF
   ELSE
      SEEK IIF(v__tipo#2,v__cod,&_campo)
      IF mMsg .AND. FOUND()
         @ v__x,v__y SAY &v__bus
      ENDIF
      v_fun = FOUND()
   ENDIF
ENDIF

IF EMPTY(mAlias)
   SELECT 0
ELSE
   SELECT (mAlias)
ENDIF

IF !EMPTY(ALIAS())
   IF mRec>0
      GO mRec
   ELSE
      GO BOTTOM
   ENDIF
ENDIF
RETURN v_fun

PROCEDURE STANDBY
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

  ACTIVATE WINDOW standby in screen

  msgwords = ALLTRIM(msgwords)
  _ln      = LEN(msgwords)
  _colW    = WCOLS()
  msgwords = iif(_ln>2*_colW,PADR(msgwords,2*_colW,' '),msgwords)
  _ln      = LEN(msgwords)

  msg1     = IIF(_ln>_colW,LEFT(msgwords,_colW),msgwords)
  msg2     = IIF(_ln>_colW,SUBSTR(msgwords,_colW+1),"")

  IF _ln>_colW
     FOR I=0 TO _colW/3-1
	IF SUBSTR(msg1,_colW-i,1) == " "
	   msg1 = LEFT(msg1,_colW-i)
	   msg2 = SUBSTR(msgwords,_colW-i+1)
	   msg2 = IIF(LEN(msg2)>_colW,LEFT(msg2,_colW),msg2)
	   EXIT
	ENDIF
     ENDFOR
  ENDIF

  @ 00,wcols()/2-LEN(msg1)/2  SAY msg1
  IF !EMPTY(msg2)
     @ 01,wcols()/2-LEN(msg2)/2  SAY msg2
  ENDIF

  @ 02,_colW/2-16 SAY '<Pres. una tecla para continuar>'
  _ss = INKEY(0)

  DEACTIVATE WINDOW standby

  MOVE WINDOW standby TO 19,27

RETURN             && standby
