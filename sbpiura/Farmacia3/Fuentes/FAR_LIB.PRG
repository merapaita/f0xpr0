***********************
* Far_Lib.Prg
***********************

PROCEDURE fox_ambi
*-----------------
DEACTIVATE WINDOW ALL
DEACTIVATE POPUP
DEACTIVATE MENU
ACTIVATE SCREEN
CLEAR WINDOW
RELEASE ALL
CLEAR ALL
PUBLIC worker
worker = IIF(TRIM(UPPER(GETENV('WORKER'))) = 'ON', .T., .F.)
ACTIVATE SCREEN
SET ALTERNATE OFF
SET ALTERNATE TO
SET AUTOSAVE OFF
SET BELL ON
SET BLOCKSIZE TO 33
SET BLINK ON
SET BORDER TO SINGLE
SET CENTURY ON
SET CLEAR ON
SET CLOCK ON
SET CLOCK TO 00, 68
SET COMPATIBLE OFF
SET CONFIRM ON
SET CONSOLE OFF
SET CURRENCY LEFT
SET DATE BRITISH
IF !worker
     SET DEBUG OFF
ELSE
     SET DEBUG ON
ENDIF
SET DECIMALS TO 2
SET DELETED ON
IF worker
     SET DEVELOPMENT ON
ELSE
     SET DEVELOPMENT OFF
ENDIF
SET DEVICE TO SCREEN
SET DOHISTORY OFF
SET ECHO OFF
IF worker
     SET ESCAPE ON
ELSE
     SET ESCAPE OFF
ENDIF
SET EXACT OFF
SET EXCLUSIVE OFF
SET FIELDS OFF
SET FIXED ON
SET FORMAT TO
SET FULLPATH ON
SET FUNCTION 1 TO
SET FUNCTION 2 TO
SET FUNCTION 3 TO
SET FUNCTION 4 TO
SET FUNCTION 5 TO
SET FUNCTION 6 TO
SET FUNCTION 7 TO
SET FUNCTION 8 TO
SET FUNCTION 9 TO
SET FUNCTION 10 TO
SET HEADING OFF
SET HELP ON
SET HOURS TO 12
SET INSTRUCT ON
SET INTENSITY ON
IF worker
     SET LOGERRORS ON
ELSE
     SET LOGERRORS OFF
ENDIF
SET MARGIN TO 0
SET MARK TO '/'
SET MEMOWIDTH TO 255
SET MENU ON
SET MESSAGE TO 23
SET NOTIFY ON
SET NEAR OFF
SET ODOMETER TO 1
SET PATH TO
SET POINT TO '.'
SET PRECISION TO 3
SET PRINTER OFF
SET PRINTER TO LPT1
SET REFRESH TO 5
SET SAFETY OFF
SET SCOREBOARD OFF
SET SEPARATOR TO ','
SET SPACE OFF
SET STATUS OFF
SET SYSMENU OFF
IF !worker
     SET STEP OFF
ENDIF
SET STICKY ON
SET TALK OFF
SET TOPIC TO
SET TYPEAHEAD TO 0
SET TYPEAHEAD TO 25
SET UDFPARMS TO REFERENCE
SET UNIQUE OFF
= INSMODE(.T.)
= CAPSLOCK(.T.)
= NUMLOCK(.T.)
_ALIGNMENT = 'LEFT'
_BOX = .T.
_INDENT = 0
_LMARGIN = 0
_PADVANCE = 'FORMFEED'
_PAGENO = 1
_PBPAGE = 1
_PCOPIES = 1
_PECODE = ''
_PEJECT = 'AFTER'
_PEPAGE = 32767
_PFORM = ''
_PLENGTH = 60
_PLOFFSET = 0
_PPITCH = 'PICA'
_PQUALITY = .T.
_PSCODE = ''
_PSPACING = 1
_PWAIT = .F.
_RMARGIN = 85
_TABS = '10, 20, 30, 40, 50, 60, 70'
_WRAP = .T.
ON KEY LABEL RIGHTMOUSE KEYBOARD CHR(13)
ON KEY LABEL F10 KEYBOARD CHR(23)
DEFINE WINDOW yesno  FROM 19, 27 TO 23, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME 5
DEFINE WINDOW espera FROM 12, 06 TO 14, 78 COLOR SCHEME 05
DEFINE WINDOW msg2user FROM 12, 02 TO 16, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME 5
DEFINE WINDOW standby FROM 19, 23 TO 23, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME 5
IF worker
     ON ERROR
ELSE
     ON ERROR
ENDIF
RETURN
*
FUNCTION yesno
*-------------
PARAMETER msgwords, posi, posc
IF !WEXIST('yesno')
     DEFINE WINDOW yesno FROM 19,27 TO 23,77 GROW FLOAT SHADOW DOUBLE COLOR SCHEME 5
ENDIF
DO CASE
     CASE PARAMETERS() = 3
          colp = IIF(posc > 28,29, posc)
          MOVE WINDOW yesno TO posi, colp
     CASE PARAMETERS() = 2
          posi = IIF(posi > 19,19, posi)
          colp = 27
          MOVE WINDOW yesno TO posi, colp
ENDCASE
ACTIVATE WINDOW yesno
msgwords = ALLTRIM(msgwords)
_ln = LEN(msgwords)
_colw = WCOLS()
msgwords = IIF(_ln > 2 * _colw,PADR(msgwords, 2 * _colw, ' '),msgwords)
_ln = LEN(msgwords)
msg1 = IIF(_ln > _colw,LEFT(msgwords, _colw),msgwords)
msg2 = IIF(_ln > _colw,  ;
       SUBSTR(msgwords, _colw +  ;
       1), '')
IF _ln > _colw
     FOR i = 0 TO _colw / 3 - 1
          IF SUBSTR(msg1, _colw -  ;
             i, 1) == ' '
               msg1 = LEFT(msg1,  ;
                      _colw - i)
               msg2 = SUBSTR(msgwords,  ;
                      _colw - i +  ;
                      1)
               msg2 = IIF(LEN(msg2) >  ;
                      _colw,  ;
                      LEFT(msg2,  ;
                      _colw),  ;
                      msg2)
               EXIT
          ENDIF
     ENDFOR
ENDIF
@ 00, WCOLS() / 2 - LEN(msg1) / 2  ;
  SAY msg1
IF !EMPTY(msg2)
     @ 01, WCOLS() / 2 -  ;
       LEN(msg2) / 2 SAY msg2
ENDIF
v_fun = .F.
@ 02, 10 GET _nosi DEFAULT 1 SIZE  ;
  1, 10, 8 FUNCTION  ;
  '*TH \!\<No;\?\<Si'
READ CYCLE
IF LASTKEY() <> 27
     v_fun = IIF(_nosi = 1, .F.,  ;
             .T.)
ENDIF
DEACTIVATE WINDOW yesno
RETURN v_fun
*
PROCEDURE standby
PARAMETER msgwords, posi, posc
PRIVATE _ln, _colw, msg1, msg2
IF !WEXIST('standby')
     DEFINE WINDOW standby FROM  ;
            19, 27 TO 23, 77  ;
            FLOAT SHADOW DOUBLE  ;
            COLOR SCHEME 5
ENDIF
IF PARAMETERS() > 1
     posi = IIF(posi > 19, 19,  ;
            posi)
     colp = 27
     IF PARAMETERS() > 2
          colp = IIF(posc > 28,  ;
                 29, posc)
     ENDIF
     MOVE WINDOW standby TO posi,  ;
          colp
ENDIF
ACTIVATE WINDOW standby
msgwords = ALLTRIM(msgwords)
_ln = LEN(msgwords)
_colw = WCOLS()
msgwords = IIF(_ln > 2 * _colw,  ;
           PADR(msgwords, 2 *  ;
           _colw, ' '),  ;
           msgwords)
_ln = LEN(msgwords)
msg1 = IIF(_ln > _colw,  ;
       LEFT(msgwords, _colw),  ;
       msgwords)
msg2 = IIF(_ln > _colw,  ;
       SUBSTR(msgwords, _colw +  ;
       1), '')
IF _ln > _colw
     FOR i = 0 TO _colw / 3 - 1
          IF SUBSTR(msg1, _colw -  ;
             i, 1) == ' '
               msg1 = LEFT(msg1,  ;
                      _colw - i)
               msg2 = SUBSTR(msgwords,  ;
                      _colw - i +  ;
                      1)
               msg2 = IIF(LEN(msg2) >  ;
                      _colw,  ;
                      LEFT(msg2,  ;
                      _colw),  ;
                      msg2)
               EXIT
          ENDIF
     ENDFOR
ENDIF
@ 00, WCOLS() / 2 - LEN(msg1) / 2  ;
  SAY msg1
IF !EMPTY(msg2)
     @ 01, WCOLS() / 2 -  ;
       LEN(msg2) / 2 SAY msg2
ENDIF
@ 02, _colw / 2 - 16 SAY  ;
  '<Pres. una tecla para continuar>'
_ss = INKEY(0)
DEACTIVATE WINDOW standby
MOVE WINDOW standby TO 19, 27
RETURN
*
FUNCTION val_para
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong,  ;
          mdist
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 4
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 5
          mdist = 6
ENDCASE
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
IF !FOUND() AND ! ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo = filtro
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          IF !EMPTY(malias)
               SELECT (malias)
          ENDIF
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF !EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = ALLTRIM(parma.codigo)
mcuenta = ALLTRIM(parma.descriau2)
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
IF !EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == 'W'
          REPLACE coddep WITH  ;
                  mvalor
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable == 'D'
          RETURN mdescr
     CASE mvariable == 'Z'
          RETURN mdescr
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*
FUNCTION val_para1
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlon
PRIVATE malias, filtro, mvariable,  ;
        mcol, mlon, _oldwnd
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = 'D'
     CASE PARAMETERS() = 3
          mcol = 0
ENDCASE
IF PARAMETERS() < 5
     mlon = 40
ENDIF
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
IF !FOUND() AND mvariable <>  ;
    'D'
     SET FILTER TO tipo = filtro
     GOTO TOP
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            descri
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     DO WHILE !FOUND()
          ACTIVATE POPUP  ;
                   parametro
          SEEK tipo + codigo
     ENDDO
     RELEASE POPUP parametro
     IF !EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
ENDIF
mvalor = parma.codigo
mdescr = parma.descri
SET FILTER TO
IF SUBSTR(VERSION(), 12) = '2.0'
     SELECT IIF(EMPTY(malias),  ;
            SELECT(0), (malias))
ELSE
     IF !EMPTY(malias)
          SELECT (malias)
     ENDIF
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            PADR(mdescr, mlon,  ;
            ' ')
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN '  '
     CASE mvariable = 'D'
          RETURN PADR(mdescr,  ;
                 mlon, ' ')
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
RETURN
*
FUNCTION val_fun
PARAMETER v__al, v__dev, v__bus,  ;
          v__cod, v__tipo, v__x,  ;
          v__y, v__ver, v__long
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwnd, _campo,  ;
        mvali, mrec, v__x, v__y,  ;
        v__lon
medita = (PARAMETERS() >= 5)  ;
         AND (v__tipo <> 3)
mmsg = (PARAMETERS() >= 7) AND  ;
       (v__tipo <> 3)
mvali = (PARAMETERS() = 5) AND  ;
        (v__tipo = 3)
v__ver = IIF(PARAMETERS() = 8,  ;
         v__ver, v__bus)
mrec = IIF(EOF(), -1, RECNO())
malias = ALIAS()
_campo = TRIM(malias) + '.' +  ;
         VARREAD()
SELECT &v__al
_oldwnd = WOUTPUT()
IF !medita
     SEEK v__cod
     v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__ver,""))
ELSE
     IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
          GOTO TOP
          IF EOF()
               DO standby WITH  ;
                  '²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²'
               v_fun = .F.
          ELSE
               _ln = LEN(&v__bus)
               _ln = IIF(_ln > 65,  ;
                     65, _ln)
               ACTIVATE SCREEN
               DEFINE POPUP v__xx FROM;
1,80-_ln TO 12,79 PROMPT FIELD &v__bus
               ON SELECTION POPUP v__xx;
DEACTIVATE POPUP
               ACTIVATE POPUP  ;
                        v__xx
               RELEASE POPUP  ;
                       v__xx
               IF ! ;
                   EMPTY(_oldwnd)
                    ACTIVATE WINDOW &_OldWnd
               ENDIF
               IF LASTKEY() = 27
                    v_fun = .F.
               ELSE
                    v__cod = &v__dev
                    IF mmsg
                         @ v__x,v__y SAY;
IIF(PARAMETERS()#9, TRIM(&v__ver), SUBSTR(&v__ver,1,v__Long))
                    ENDIF
                    IF ! ;
                        EMPTY(malias)
                         SELECT (malias)
                         IF (v__tipo =  ;
                            2)
                              IF mrec >  ;
                                 0
                                   GOTO mrec
                                   REPLACE &_campo WITH v__cod
                                   v_fun = .T.
                              ELSE
                                   DO standby WITH ' El archivo est  vac¡o '
                                   v_fun = .F.
                              ENDIF
                         ELSE
                              v_fun =  ;
                               .T.
                         ENDIF
                    ELSE
                         DO standby  ;
                            WITH  ;
                            ' NO HAY ARCHIVO ABIERTO '
                    ENDIF
               ENDIF
          ENDIF
     ELSE
          SEEK IIF(v__tipo#2,v__cod,&_campo)
          IF mmsg AND FOUND()
               @ v__x,v__y SAY ALLT(&v__ver)
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
     IF mrec > 0
          GOTO mrec
     ELSE
          GOTO BOTTOM
     ENDIF
ENDIF
RETURN v_fun
*
FUNCTION f_appd
*--------------
APPEND BLANK
v_fun = f_lock(1)
RETURN v_fun
*
FUNCTION f_lock
*--------------
PARAMETER btipo
beep = CHR(7)
DO CASE
	CASE btipo = 1
		v_fun = .F.
		IF RLOCK()
			v_fun = .T.
		ELSE
			DO standby WITH 'Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar.'
			?? beep
			ktecla = 0
			DO WHILE ktecla<> escape AND (!RLOCK())
				ktecla = INKEY()
			ENDDO
			IF ktecla<>escape
				v_fun = .T.
			ENDIF
		ENDIF
	CASE btipo = 2
		v_fun = .F.
		IF RLOCK()
			v_fun = .T.
		ELSE
			DO standby WITH 'El registro est  siendo utilizado. Se cancela la operaci¢n. '
			?? beep
		ENDIF
	CASE btipo = 3
		v_fun = .F.
		IF FLOCK()
			v_fun = .T.
		ELSE
			DO standby WITH 'Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar.'
			?? beep
			ktecla = 0
			DO WHILE ktecla<>escape AND (!FLOCK())
				ktecla = INKEY()
			ENDDO
			IF ktecla <> escape
				v_fun = .T.
			ENDIF
		ENDIF
	CASE btipo = 4
		v_fun = .F.
		IF FLOCK()
			v_fun = .T.
		ELSE
			DO standby WITH 'Archivo ocupado.  El proceso se cancela.'
			?? beep
		ENDIF
ENDCASE
RETURN v_fun
*
PROCEDURE fox_errs
PARAMETER prg_error
DO CASE
     CASE ERROR() = 108
          DO standby WITH  ;
             'Un momento por favor para agregar registro'
          RETRY
     CASE ERROR() = 5
          DO standby WITH  ;
             'Registro fuera de rango. No se hizo ninguna operaci¢n'
          RETRY
     OTHERWISE
          nom_usr = SYS(30)
          num_error = ERROR()
          des_error = MESSAGE()
          lin_error = MESSAGE(1)
          dbf_actua = DBF()
          IF SYS(21) <> '0'
               idx_actua = NDX(VAL(SYS(21)))
          ENDIF
          SAVE SCREEN TO  ;
               pnt_error
          CREATE VIEW core.viw
          DO standby WITH  ;
             des_error+ ;
             ' ERROR no recuperable presione ENTER para terminar'
          WAIT ''
          IF worker
               ON ERROR
               CANCEL
          ENDIF
          QUIT
ENDCASE
RETURN
*
PROCEDURE logos
PARAMETER rotulo1, rotulo2,  ;
          tiempo
ACTIVATE SCREEN
DO CASE
     CASE PARAMETERS() = 0
          STORE SPACE(80) TO  ;
                rotulo1, rotulo2
          tiempo = 0
     CASE PARAMETERS() = 1
          STORE SPACE(80) TO  ;
                rotulo2
          tiempo = 0
     CASE PARAMETERS() = 2
          tiempo = 0
ENDCASE
FOR i = 0 TO 39 STEP -1
     @ 00, i SAY c COLOR SCHEME  ;
       c_borde
     @ 00, 79 - i SAY  ;
       LEFT(rotulo1, i + 1) COLOR  ;
       SCHEME c_fondo
     @ 24, 79 - i SAY c COLOR  ;
       SCHEME c_borde
     @ 24, 00 SAY RIGHT(rotulo2,  ;
       i + 1) COLOR SCHEME  ;
       c_fondo
     FOR y = 0 TO tiempo
     ENDFOR
ENDFOR
FOR i = 40 TO 0 STEP -1
     @ 00, 79 - i SAY  ;
       SUBSTR(rotulo1, 2 * (40 -  ;
       i), i + 1) COLOR SCHEME  ;
       c_fondo
     @ 00, i SAY LEFT(rotulo1, 2 *  ;
       (40 - i)) COLOR SCHEME  ;
       c_borde
     @ 24, 00 SAY SUBSTR(rotulo2,  ;
       41 - i, i) COLOR SCHEME  ;
       c_fondo
     @ 24, i SAY RIGHT(rotulo2, 2 *  ;
       (40 - i)) COLOR SCHEME  ;
       c_borde
     FOR y = 0 TO tiempo
     ENDFOR
ENDFOR
SET CLOCK ON
RETURN
*
FUNCTION poperror
PARAMETER err_mess, _xtime
PRIVATE cur_color, cur_curs,  ;
        bord_str, err_mess,  ;
        say_mess
PRIVATE num_lines, start_line,  ;
        cur_width, i, rvalue
PRIVATE cur_win, _mens
PUSH KEY CLEAR
cur_win = WOUTPUT()
cur_width = SET('memowidth')
SET MEMOWIDTH TO 48
num_lines = MEMLINES(err_mess)
altura = num_lines + 3 + 2
start_line = (25 - altura) / 2
DEFINE WINDOW poperr FROM  ;
       start_line, 13 TO  ;
       start_line + altura, 66  ;
       SHADOW DOUBLE COLOR SCHEME  ;
       5
ACTIVATE WINDOW poperr
IF num_lines = 1
     _mens = PADC(err_mess, 48)
     FOR j = 1 TO LEN(_mens)
          @ 1, 1 + j SAY  ;
            SUBSTR(_mens, j, 1)
          IF (PARAMETERS() >= 2  ;
             AND _xtime) OR  ;
             PARAMETERS() < 2
               = INKEY(0.001 ,  ;
                 ' ')
          ENDIF
     ENDFOR
     i = 2
ELSE
     FOR i = 1 TO num_lines
          say_mess = LTRIM(MLINE(err_mess,  ;
                     i))
          FOR j = 1 TO  ;
              LEN(say_mess)
               @ i, 1 + j SAY  ;
                 SUBSTR(say_mess,  ;
                 j, 1)
               IF (PARAMETERS() >=  ;
                  2 AND _xtime)  ;
                  OR  ;
                  PARAMETERS() <  ;
                  2
                    = INKEY(0.001 ,  ;
                      ' ')
               ENDIF
          ENDFOR
     ENDFOR
ENDIF
@ i + 1, 01 SAY  ;
  REPLICATE(CHR(196), 49)
_mens = 'Presione una tecla para continuar ....'
FOR j = 1 TO LEN(_mens)
     @ i + 2, 1 + j SAY  ;
       SUBSTR(_mens, j, 1)
     = INKEY(0.001 , ' ')
ENDFOR
rvalue = INKEY(0, 'hm')
IF WEXIST('poperr')
     RELEASE WINDOW poperr
ENDIF
IF EMPTY(cur_win)
     ACTIVATE SCREEN
ENDIF
SET MEMOWIDTH TO cur_width
POP KEY
RETURN (rvalue)
*
FUNCTION isdisket
PARAMETER _drive
PRIVATE m.drive, vret
vret = .T.
m.drive = _drive
LOAD IsDiskIn.BIN
CALL isdiskin WITH m.drive
DO WHILE m.drive='0:'
     ?? CHR(7) + CHR(7)
     DO standby WITH  ;
        '! El drive '+_drive+ ;
        ' no est  listo !'
     IF LASTKEY() = 27
          vret = .F.
          EXIT
     ENDIF
     m.drive = _drive
     CALL isdiskin WITH m.drive
ENDDO
_drive = m.drive
RELEASE MODULE IsDiskIn.BIN
RETURN vret
*
FUNCTION clave
PRIVATE xpassword, m.key,  ;
        vretorno, m.cur_curs,  ;
        m.cur_win, m.usuario
xpassword = ''
vretorno = .F.
m.key = 0
m.cur_curs = SET('cursor') = 'ON'
SET CURSOR ON
m.cur_win = WOUTPUT()
SELECT usua
DEFINE WINDOW pass_win FROM 08,  ;
       15 TO 15, 65 SHADOW TITLE  ;
       '  Usuario  ' DOUBLE COLOR  ;
       SCHEME 21
ACTIVATE WINDOW pass_win
m.usuario = SPACE(15)
@ 01, 03 SAY 'USUARIO'
@ 01, 11 GET m.usuario VALID  ;
  val_fun('Usua','Usuario', ;
  'Nombre',@m.usuario,1,2,11)
READ
IF LASTKEY() <> 27
     SEEK m.usuario
     @ 03, 09 SAY  ;
       'Ingrese su password'
     @ 05, 15 SAY ''
     DO WHILE m.key<>13 AND  ;
        m.key<>27
          m.key = INKEY(0)
          DO CASE
               CASE BETWEEN(m.key,  ;
                    65, 90) OR  ;
                    BETWEEN(m.key,  ;
                    97, 122) OR  ;
                    BETWEEN(m.key,  ;
                    48, 57) OR  ;
                    BETWEEN(m.key,  ;
                    164, 165)
                    xpassword = xpassword +  ;
                                CHR(m.key)
               CASE m.key = 19  ;
                    OR m.key =  ;
                    127 OR  ;
                    m.key = 7
                    @ ROW(),  ;
                      COL() - 1  ;
                      SAY ' '
                    @ ROW(),  ;
                      COL() - 1  ;
                      SAY ''
                    xpassword = SUBSTR(xpassword,  ;
                                1,  ;
                                LEN(xpassword) -  ;
                                1)
               OTHERWISE
          ENDCASE
          @ 05, 15 SAY  ;
            REPLICATE('',  ;
            LEN(xpassword))
     ENDDO
ENDIF
RELEASE WINDOW pass_win
IF EMPTY(cur_win)
     ACTIVATE SCREEN
ENDIF
IF !cur_curs
     SET CURSOR OFF
ENDIF
IF LASTKEY() <> 27
     vencrip = CHRTRAN(UPPER(xpassword),;
'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789', 'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
     IF vencrip ==  ;
        ALLTRIM(usua.clave)
          @ 22, 25 SAY  ;
            IIF(VAL(SUBSTR(TIME(),  ;
            1, 2)) < 12,  ;
            'Buenos Dias ',  ;
            'Buenas Tardes ') +  ;
            ALLTRIM(usua.nombre)
          @ 23, 25 SAY  ;
            'USUARIO : ' +  ;
            ALLTRIM(usua.nombre)
          WAIT TIMEOUT 2 ''
          @ 23, 25, 23, 80 BOX  ;
            '°°°°°°°°°'
          vcoddep = usua.coddep
          mret = .T.
     ELSE
          mret = .F.
     ENDIF
ELSE
     mret = .F.
ENDIF
RETURN (mret)
*
PROCEDURE savcon
PARAMETER xnum
USE IN 0 Bitacora ALIAS bitacora
PRIVATE _hora
SELECT bitacora
_hora = TIME()
IF xnum = 1
     IF f_appd()
          REPLACE codusu WITH  ;
                  vuser_id, conex  ;
                  WITH vconex,  ;
                  machine WITH  ;
                  vmaq, tipmaq  ;
                  WITH  ;
                  SUBSTR(SYS(17),  ;
                  3), prog WITH  ;
                  SYS(16, 1),  ;
                  hora WITH _hora,  ;
                  fecha WITH  ;
                  DATE(), llave  ;
                  WITH vllav
          UNLOCK
     ENDIF
     vusurec = RECNO()
ELSE
     GOTO vusurec
     REPLACE salida WITH _hora
ENDIF
USE
RETURN
*
PROCEDURE espera
PARAMETER festado, fmensaje
DEFINE WINDOW espera FROM 21, 05  ;
       TO 23, 75 COLOR SCHEME 1
DO CASE
     CASE festado = 1
          ACTIVATE WINDOW espera
          @ 0, 0 SAY  ;
            PADC(ALLTRIM(fmensaje),  ;
            WCOLS()) COLOR W+/RB* 
          RETURN
     CASE festado = 2
          DEACTIVATE WINDOW  ;
                     espera
          RETURN
ENDCASE
RETURN
*
FUNCTION selper
*--------------
PUBLIC m.fecsis
SAVE SCREEN TO pantalla
DEFINE WINDOW wlista FROM 03,18 TO 6,60 TITLE 'Periodo a Trabajar'  ;
       DOUBLE COLOR SCHEME 05
ACTIVATE WINDOW wlista
cpath1 = SET('PATH')
m.fecsis = DATE()
@ 01, 01 SAY 'Fecha del Sistema :' GET m.fecsis
READ
DEACTIVATE WINDOW wlista
RELEASE WINDOW wlista
IF LASTKEY() = 13
     newruta = STR(YEAR(m.fecsis),4)
     cpath   = cpath1 + '\' + newruta
     cpath1  = cpath1 + ';' + cpath1 + '\' + newruta
     SET PATH TO &cPath1
     IF !FILE(cpath + '\Parmae.Dbf')
          DO poperror WITH "El periodo a trabajar '"+ newruta+ ;
             "' no existen si desea trabajar con la opci¢n escojida continue con el proceso"
          IF yesno('Desea Preparar Informacion para este Periodo ' + newruta)
               RUN CD..			> NUL
               RUN CD data		> NUL
               !MD &NewRuta		> NUL
               !CD &NewRuta		> NUL
               RUN ..\..\FUENTES\Pkunzip ..\data		>NUL
               RUN CD ..\..\FUENTES						>NUL
               mret = .T.
          ELSE
               mret = .F.
          ENDIF
     ELSE
          mret = .T.
     ENDIF
ELSE
     mret = .F.
ENDIF
RESTORE SCREEN FROM pantalla
RETURN mret
*
FUNCTION val_read
PRIVATE _fun
_fun = .T.
IF LASTKEY() <> 27
     IF !yesno( ;
         '¨ Est n correctos los datos ?' ;
         )
          _fun = .F.
     ENDIF
ENDIF
RETURN (_fun)
*
PROCEDURE val_ge
PRIVATE mret, mfila, mcolumna
mret = ''
mfila = ROW()
mcolumna = COL() + 9
IF EMPTY(mge)
     mret = val_fun('ArtMae', ;
            'CodArt','Descri', ;
            @m.coddet,1,mfila, ;
            mcolumna)
ELSE
     SELECT artmae
     IF !SEEK(mret,  ;
         'ArtMae')
          mret = val_fun('ArtMae', ;
                 'CodArt', ;
                 'Descri', ;
                 @m.coddet,1, ;
                 mfila,mcolumna)
     ELSE
          mret = val_fun('ArtMae', ;
                 'CodArt', ;
                 'Descri', ;
                 @m.coddet,1, ;
                 mfila,mcolumna)
     ENDIF
ENDIF
m.coddet = SUBSTR(artmae.codart,  ;
           2)
RETURN
*
PROCEDURE val_gg
PRIVATE mret, mcolumna
mret = ''
mcolumna = COL()
IF EMPTY(m.codgen)
     mret = val_para(m.codgen, ;
            'CODGEB','A',mcolumna, ;
            30)
ELSE
     IF !SEEK('CODGEBB' +  ;
         m.codgen, 'Parma')
          mret = val_para(m.codgen, ;
                 'CODGEB','A', ;
                 mcolumna,30)
     ELSE
          mret = val_para('B' +  ;
                 m.codgen, ;
                 'CODGEB','A', ;
                 mcolumna,30)
     ENDIF
ENDIF
m.codgen = SUBSTR(parma.codigo, 2,  ;
           2)
RETURN
*
PROCEDURE val_gg1
PARAMETER _y
PRIVATE mret
mret = ''
IF EMPTY(m.codgen)
     mret = val_para(m.codgen, ;
            'CODGEB','A',_y,30)
ELSE
     IF !SEEK('CODGEBB' +  ;
         m.codgen, 'Parma')
          mret = val_para(m.codgen, ;
                 'CODGEB','A',_y, ;
                 30)
     ELSE
          mret = val_para('B' +  ;
                 m.codgen, ;
                 'CODGEB','A',_y, ;
                 30)
     ENDIF
ENDIF
m.codgen = SUBSTR(parma.codigo, 2,  ;
           2)
RETURN
*
PROCEDURE val_esp2
RETURN
*
FUNCTION val_esp
PARAMETER xcod, mvariable, mcol
PRIVATE mfiltro
malias = ALIAS()
v_fun = .F.
DO CASE
     CASE PARAMETERS() = 1
          mvariable = ' '
          mcol = 0
     CASE PARAMETERS() = 2
          mcol = 0
ENDCASE
SELECT artmae
GOTO TOP
IF !EMPTY(xcod)
     SEEK xcod
     v_fun = FOUND()
ENDIF
IF EMPTY(xcod) OR !v_fun
     SELECT iteart
     SET RELATION TO
     SELECT artmae
     SET ORDER TO ArtMae2
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     DEFINE WINDOW _busart FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _busart
     BROWSE FIELDS codart :H =  ;
            'C¢digo', descri :H =  ;
            'Nombre' : 40,  ;
            descri1 :H =  ;
            'Descripcion' : 40  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _busart TITLE  ;
            '²²²² [F10] Selecciona ²²²²'
     ON KEY LABEL F10
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() <> 27
          m.codcla = SUBSTR(artmae.codart,  ;
                     5, 3)
          mdescri = descri
          v_fun = .T.
          SET ORDER TO ArtMae1
          SELECT iteart
          SET RELATION TO tipart + LEFT(codart,;
6) INTO artmae
     ELSE
          v_fun = .F.
          ON KEY LABEL F10 KEYBOARD CHR(23)
          UNLOCK ALL
          SET ORDER TO ArtMae1
          SELECT iteart
          SET RELATION TO tipart + LEFT(codart,;
6) INTO artmae
          SELECT (malias)
          RETURN v_fun
     ENDIF
ELSE
     mdescri = descri
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT (malias)
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN m.codcla
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescri
     CASE mvariable == 'D'
          RETURN mdescri
     CASE mvariable == 'Z'
          RETURN mdescri
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          RETURN .T.
ENDCASE
RETURN
*
FUNCTION val_det
PRIVATE malias
malias = ALIAS()
SELECT iteart
xord = ORDER()
SET ORDER TO IteArt1
SEEK m.tipart + m.codgen + '.' +  ;
     m.codcla
lagr = .F.
IF FOUND()
     xcoddet = 0
     SCAN WHILE iteart.tipart +  ;
          iteart.codgen + '.' +  ;
          iteart.codcla =  ;
          m.tipart + m.codgen +  ;
          '.' + m.codcla
          xcoddet = xcoddet + 1
          IF xcoddet =  ;
             VAL(coddet)
          ELSE
               m.coddet = PADL(ALLTRIM(STR(xcoddet)),  ;
                          4,  ;
                          '0')
               lagr = .T.
               EXIT
          ENDIF
     ENDSCAN
     IF !lagr
          m.coddet = PADL(ALLTRIM(STR(xcoddet +  ;
                     1)), 4,  ;
                     '0')
     ENDIF
ELSE
     m.coddet = '0001'
ENDIF
SET ORDER TO (xord)
m.tipart = m.tipart
m.codart = m.codgen + '.' +  ;
           m.codcla + '.' +  ;
           m.coddet
SHOW GETS
RETURN .T.
*
FUNCTION cproper
PARAMETER ctexto
ctexto = PROPER(ctexto)
RETURN .T.
*
FUNCTION val_art
*---------------
PARAMETER xcod
PRIVATE cord
ON KEY LABEL F5
ON KEY LABEL F8
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT iteart
GOTO TOP
cord = ORDER()
SET ORDER TO IteArt1
IF !EMPTY('B' + xcod)
     SEEK 'B' + xcod
     v_fun = IIF(FOUND(), desart,'')
     IF FOUND()
          m.lmenart = lfracc
          m.fracart = fraccion
     ENDIF
     v_ent = FOUND()
ENDIF
IF EMPTY(xcod) OR !v_ent
     SET ORDER TO IteArt2
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO FunBusDet
     ON KEY LABEL F5 DO Agr_Art
     ON KEY LABEL F8
     DEFINE WINDOW _busart FROM 2,01 TO 22, 78
     ACTIVATE WINDOW _busart
     DO funbusdet
     BROWSE FIELDS codart :H = ;
            'C¢digo', descri :H = 'Nombre' : 60, coduni  ;
            :H = 'Unidad' : 7,  ;
            unifrac :H =  ;
            'U.Fracc.', fraccion  ;
            :H = 'Fraccion'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _busart TITLE  ;
            '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega  ²²²²'
     ON KEY LABEL F10
     ON KEY LABEL F2
     ON KEY LABEL F5
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() = 27
          v_fun = .F.
          v_ent = .F.
     ELSE
          xcod = codart
          m.desart = descri
          m.unidad = coduni
          m.coduni = coduni
          m.unifrac = unifrac
          m.lmenart = lfracc
          m.fracart = fraccion
          SHOW GETS
          v_fun = .T.
          v_ent = .T.
     ENDIF
ENDIF
SET ORDER TO cOrd
ON KEY LABEL F5 DO Agrega_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT (malias)
IF !v_ent
     RETURN v_fun
ELSE
     RETURN v_ent
ENDIF
RETURN
*
FUNCTION actkarpr
PARAMETER m.tipdoc, m.numdoc,m.fecha
PRIVATE malias
malias = ALIAS()
DO CASE
     CASE tipdoc = 'INV' OR tipdoc = 'OCF'
          m.tipmov = '+'
     CASE tipdoc = 'OPF' OR tipdoc = 'DON'
          m.tipmov = '-'
     OTHERWISE
          m.tipmov = ' '
          DO standby WITH 'Error en tipo de movimiento'
          RETURN .T.
ENDCASE
SELECT kardex
SET ORDER TO KardexV2
mret = .F.
lcancel = .F.
IF !SEEK(m.periodo + m.tipdoc + m.numdoc + m.item + m.codart)
     IF m.tipmov = '+'
          m.correl = '0001'
     ENDIF
     IF f_appd()
          m.coraux = 'A'
          GATHER MEMVAR
     ENDIF
     SET ORDER TO Kardexv1
     SEEK m.periodo + m.codart + m.correl
     IF FOUND()
          m.coraux = coraux
          SCAN WHILE coraux <> 'A'
               m.coraux = coraux
          ENDSCAN
          m.coraux = PADL(ALLTRIM(STR(VAL(m.coraux) + 1)), 4,'0')
          REPLACE coraux WITH m.coraux
     ENDIF
     SEEK m.periodo + m.codart + m.correl
     IF FOUND()
          m.saldo = 0
          xpuan = 0.00 
          xcan = 0
          SCAN WHILE periodo = m.periodo AND codart = m.codart  ;
               AND correl = m.correl
               soper = IIF(tipmov = '+', 1, - 1)
               m.saldo = m.saldo + soper * totcan
               IF tipmov = '+'
                    m.prepro = ROUND((xpuan*xcan+preuni*totcan)/(xcan+totcan),2)
               ENDIF
               IF m.saldo <>  ;
                  saldo
                    REPLACE saldo WITH m.saldo
               ENDIF
               IF m.prepro <> prepro
                    REPLACE prepro WITH m.prepro
               ENDIF
               xpuan = prepro
               xcan = saldo
          ENDSCAN
          SKIP -1
          IF m.saldo >= 0
               REPLACE saldo WITH m.saldo
               REPLACE prepro WITH m.prepro
               SELECT stkalmv
               IF !SEEK(m.periodo + m.codart + m.correl)
                    SELECT iteart
                    _ord = ORDER()
                    SET ORDER TO IteArt1
                    SEEK 'B' +  ;
                         m.codart
                    IF FOUND()
                         _lfracc = lfracc
                         _unid = coduni
                         _unif = unifrac
                         _frac = fraccion
                    ELSE
                         DO standby WITH 'Err. con catalogo de Articulos. No Actualizo Stock'
                         lcancel = .T.
                    ENDIF
                    SET ORDER TO (_ord)
                    SELECT stkalmv
                    IF ! ;
                        lcancel
                         IF f_appd()
                              GATHER MEMVAR
                              REPLACE lfracc WITH _lfracc,unidad WITH _unid,  ;
                               unifrac WITH _unif, fraccion WITH _frac
                         ENDIF
                    ENDIF
               ENDIF
               IF !lcancel
                    REPLACE salfrac WITH m.saldo, salcant WITH INT(m.saldo/fraccion),;
                            prepro WITH m.prepro, valpro WITH ROUND(prepro/1.18 ,2),;
                            valfrpro WITH ROUND(prepro / fraccion, 2)
                    IF m.tipmov = '+'
                         REPLACE preven WITH m.preven, prevenfr WITH m.prevenfr,  ;
                                 preven2 WITH m.preven2, preven2fr WITH m.preven2fr
                    ENDIF
               ENDIF
               mret = .T.
          ELSE
               REPLACE saldo WITH m.saldo
               REPLACE prepro WITH xprepro
               SELECT stkalmv
               IF !SEEK(m.periodo + m.codart + m.correl)
                    REPLACE salfrac WITH m.saldo, salcant WITH INT(m.saldo/fraccion),;
                            prepro WITH m.prepro, valpro WITH ROUND(prepro/1.18,2),;
                            valfrpro WITH ROUND(prepro / fraccion, 2)
                    IF m.tipmov = '+'
                         REPLACE preven WITH m.preven, prevenfr WITH m.prevenfr,;
                                 preven2 WITH m.preven2, preven2fr WITH m.preven2fr
                    ENDIF
               ENDIF
               DO standby WITH 'La Cantidad est  Excediendo Kardex. Kardex Se actualiz¢ con inconsistencias'
               mret = .F.
          ENDIF
     ENDIF
     mret = .T.
ELSE
     REPLACE fecha WITH m.fecha,  ;
             lfracc WITH m.lfracc,  ;
             cantidad WITH m.cantidad,;
             fraccion WITH m.fraccion,  ;
             totcan WITH m.totcan,  ;
             usercr WITH m.usercr,  ;
             husercr WITH  ;
             m.husercr, dusercr  ;
             WITH m.dusercr
     m.correl = correl
     SET ORDER TO Kardexv1
     SEEK m.periodo + m.codart +  ;
          m.correl
     IF FOUND()
          m.saldo = 0
          xpuan = 0.00 
          xcan = 0
          SCAN WHILE periodo = m.periodo AND codart = m.codart AND correl = m.correl
               soper = IIF(tipmov = '+', 1, - 1)
               m.saldo = m.saldo + soper * totcan
               m.prepro = ROUND((xpuan * xcan + preuni * totcan) / (xcan + totcan), 2)
               IF m.saldo <> saldo
                    REPLACE saldo WITH m.saldo
               ENDIF
               IF m.prepro <> prepro
                    REPLACE prepro WITH m.prepro
               ENDIF
               xpuan = prepro
               xcan = saldo
          ENDSCAN
          SKIP -1
          IF m.saldo >= 0
               REPLACE saldo WITH m.saldo
               REPLACE prepro WITH m.prepro
               SELECT stkalmv
               SEEK m.periodo + m.codart + m.correl
               IF FOUND()
                    REPLACE fecha  ;
                            WITH  ;
                            m.fecha,  ;
                            salfrac  ;
                            WITH  ;
                            m.saldo,  ;
                            salcant  ;
                            WITH  ;
                            INT(m.saldo /  ;
                            fraccion),  ;
                            prepro  ;
                            WITH  ;
                            m.prepro,  ;
                            valpro  ;
                            WITH  ;
                            ROUND(prepro /  ;
                            1.18 ,  ;
                            2),  ;
                            valfrpro  ;
                            WITH  ;
                            ROUND(prepro /  ;
                            fraccion,  ;
                            2)
                    IF m.tipmov =  ;
                       '+'
                         REPLACE preven  ;
                                 WITH  ;
                                 m.preven,  ;
                                 prevenfr  ;
                                 WITH  ;
                                 m.prevenfr,  ;
                                 preven2  ;
                                 WITH  ;
                                 m.preven2,  ;
                                 preven2fr  ;
                                 WITH  ;
                                 m.preven2fr
                    ENDIF
               ELSE
               ENDIF
               mret = .T.
          ELSE
               REPLACE saldo WITH  ;
                       m.saldo
               DO standby WITH  ;
                  'La Cantidad est  Excediendo Kardex. No Actualizo Stock'
               mret = .F.
          ENDIF
     ENDIF
     mret = .T.
ENDIF
SELECT (malias)
RETURN mret
PARAMETER m.tipdoc, m.numdoc,  ;
          m.fecha
PRIVATE malias
malias = ALIAS()
DO CASE
     CASE tipdoc = 'INV' OR  ;
          tipdoc = 'OCF'
          m.tipmov = '+'
     CASE tipdoc = 'OPF' OR  ;
          tipdoc = 'DON'
          m.tipmov = '-'
     OTHERWISE
          m.tipmov = ' '
          DO standby WITH  ;
             'Error en tipo de movimiento'
          RETURN .T.
ENDCASE
SELECT kardex
SET ORDER TO KardexV2
mret = .F.
lcancel = .F.
IF !SEEK(m.periodo +  ;
    m.tipdoc + m.numdoc + m.item +  ;
    m.codart)
     IF m.tipmov = '+'
          m.correl = 'A'
     ENDIF
     IF f_appd()
          m.coraux = 'A'
          GATHER MEMVAR
     ENDIF
     SET ORDER TO Kardexv1
     IF m.tipmov = '+'
          SEEK m.periodo +  ;
               m.codart
          IF FOUND()
               m.correl = '0001'
               npreuni = preuni
               SCAN WHILE correl <>  ;
                    'A'
                    npreuni = preuni
                    m.correl = correl
               ENDSCAN
               IF m.preuni =  ;
                  npreuni
                    m.correl = PADL(ALLTRIM(STR(VAL(m.correl))),  ;
                               4,  ;
                               '0')
               ELSE
                    m.correl = PADL(ALLTRIM(STR(VAL(m.correl) +  ;
                               1)),  ;
                               4,  ;
                               '0')
               ENDIF
               REPLACE correl  ;
                       WITH  ;
                       m.correl
          ENDIF
     ENDIF
     SEEK m.periodo + m.codart +  ;
          m.correl
     IF FOUND()
          m.coraux = coraux
          SCAN WHILE coraux <>  ;
               'A'
               m.coraux = coraux
          ENDSCAN
          m.coraux = PADL(ALLTRIM(STR(VAL(m.coraux) +  ;
                     1)), 4,  ;
                     '0')
          REPLACE coraux WITH  ;
                  m.coraux
     ENDIF
     SEEK m.periodo + m.codart +  ;
          m.correl
     IF FOUND()
          m.saldo = 0
          SCAN WHILE periodo =  ;
               m.periodo AND  ;
               codart = m.codart  ;
               AND correl =  ;
               m.correl
               soper = IIF(tipmov =  ;
                       '+', 1, - ;
                       1)
               m.saldo = m.saldo +  ;
                         soper *  ;
                         totcan
               IF m.saldo <>  ;
                  saldo
                    REPLACE saldo  ;
                            WITH  ;
                            m.saldo
               ENDIF
          ENDSCAN
          SKIP -1
          IF m.saldo >= 0
               REPLACE saldo WITH  ;
                       m.saldo
               SELECT stkalmv
               IF ! ;
                   SEEK(m.periodo +  ;
                   m.codart +  ;
                   m.correl)
                    SELECT iteart
                    _ord = ORDER()
                    SET ORDER TO IteArt1
                    SEEK 'B' +  ;
                         m.codart
                    IF FOUND()
                         _lfracc =  ;
                          lfracc
                         _unid = coduni
                         _unif = unifrac
                         _frac = fraccion
                    ELSE
                         DO standby  ;
                            WITH  ;
                            'Err. con catalogo de Articulos. No Actualizo Stock'
                         lcancel =  ;
                          .T.
                    ENDIF
                    SET ORDER TO (_ord)
                    SELECT stkalmv
                    IF ! ;
                        lcancel
                         IF f_appd()
                              GATHER  ;
                               MEMVAR
                              REPLACE  ;
                               lfracc  ;
                               WITH  ;
                               _lfracc,  ;
                               unidad  ;
                               WITH  ;
                               _unid,  ;
                               unifrac  ;
                               WITH  ;
                               _unif,  ;
                               fraccion  ;
                               WITH  ;
                               _frac
                         ENDIF
                    ENDIF
               ENDIF
               IF !lcancel
                    REPLACE salfrac  ;
                            WITH  ;
                            m.saldo,  ;
                            salcant  ;
                            WITH  ;
                            INT(m.saldo /  ;
                            fraccion),  ;
                            preven  ;
                            WITH  ;
                            m.preven,  ;
                            prevenfr  ;
                            WITH  ;
                            m.prevenfr
               ENDIF
               mret = .T.
          ELSE
               REPLACE saldo WITH  ;
                       m.saldo
               SELECT stkalmv
               IF ! ;
                   SEEK(m.periodo +  ;
                   m.codart +  ;
                   m.correl)
                    REPLACE salfrac  ;
                            WITH  ;
                            m.saldo,  ;
                            salcant  ;
                            WITH  ;
                            INT(m.saldo /  ;
                            fraccion),  ;
                            preven  ;
                            WITH  ;
                            m.preven,  ;
                            prevenfr  ;
                            WITH  ;
                            m.prevenfr
               ENDIF
               DO standby WITH  ;
                  'La Cantidad est  Excediendo Kardex. Kardex Se actualiz¢ con inconsistencias'
               mret = .F.
          ENDIF
     ENDIF
     mret = .T.
ELSE
     REPLACE fecha WITH m.fecha,  ;
             lfracc WITH m.lfracc,  ;
             cantidad WITH  ;
             m.cantidad, fraccion  ;
             WITH m.fraccion,  ;
             totcan WITH m.totcan,  ;
             usercr WITH m.usercr,  ;
             husercr WITH  ;
             m.husercr, dusercr  ;
             WITH m.dusercr
     m.correl = correl
     SET ORDER TO Kardexv1
     SEEK m.periodo + m.codart +  ;
          m.correl
     IF FOUND()
          m.saldo = 0
          SCAN WHILE periodo =  ;
               m.periodo AND  ;
               codart = m.codart  ;
               AND correl =  ;
               m.correl
               soper = IIF(tipmov =  ;
                       '+', 1, - ;
                       1)
               m.saldo = m.saldo +  ;
                         soper *  ;
                         totcan
               IF m.saldo <>  ;
                  saldo
                    REPLACE saldo  ;
                            WITH  ;
                            m.saldo
               ENDIF
          ENDSCAN
          SKIP -1
          IF m.saldo >= 0
               REPLACE saldo WITH  ;
                       m.saldo
               SELECT stkalmv
               SEEK m.periodo +  ;
                    m.codart +  ;
                    m.correl
               IF FOUND()
                    REPLACE fecha  ;
                            WITH  ;
                            m.fecha,  ;
                            salfrac  ;
                            WITH  ;
                            m.saldo,  ;
                            salcant  ;
                            WITH  ;
                            INT(m.saldo /  ;
                            fraccion),  ;
                            preven  ;
                            WITH  ;
                            m.preven,  ;
                            prevenfr  ;
                            WITH  ;
                            m.prevenfr
               ELSE
               ENDIF
               mret = .T.
          ELSE
               REPLACE saldo WITH  ;
                       m.saldo
               DO standby WITH  ;
                  'La Cantidad est  Excediendo Kardex. No Actualizo Stock'
               mret = .F.
          ENDIF
     ENDIF
     mret = .T.
ENDIF
SELECT (malias)
RETURN mret
*
PROCEDURE recalpr
PARAMETER cperiodo, ccodart,  ;
          ccorrel
PRIVATE cord
cord = ORDER()
SET ORDER TO KardexV1
SEEK cperiodo + ccodart + ccorrel
nsaldo = 0
xpuan = 0.00 
xcan = 0
nprepro = 0
IF FOUND()
     SCAN WHILE periodo =  ;
          cperiodo AND codart =  ;
          ccodart AND correl =  ;
          ccorrel
          soper = IIF(tipmov =  ;
                  '+', 1, -1)
          nsaldo = nsaldo + soper *  ;
                   totcan
          nprepro = ROUND((xpuan *  ;
                    xcan + preuni *  ;
                    totcan) /  ;
                    (xcan +  ;
                    totcan), 2)
          xpuan = prepro
          xcan = saldo
     ENDSCAN
     SKIP -1
     REPLACE saldo WITH nsaldo
     REPLACE prepro WITH nprepro
     SELECT stkalmv
     IF SEEK(cperiodo + ccodart +  ;
        ccorrel)
          REPLACE salfrac WITH  ;
                  nsaldo
          REPLACE salcant WITH  ;
                  nsaldo /  ;
                  fraccion
          REPLACE prepro WITH  ;
                  nprepro
          REPLACE valpro WITH  ;
                  ROUND(prepro /  ;
                  1.18 , 2)
          REPLACE valfrpro WITH  ;
                  ROUND(prepro /  ;
                  fraccion, 2)
     ENDIF
ELSE
     SELECT stkalmv
     IF SEEK(cperiodo + ccodart +  ;
        ccorrel)
          REPLACE salfrac WITH  ;
                  nsaldo
          REPLACE salcant WITH  ;
                  nsaldo /  ;
                  fraccion
          REPLACE prepro WITH  ;
                  nprepro
          REPLACE valpro WITH  ;
                  ROUND(prepro /  ;
                  1.18 , 2)
          REPLACE valfrpro WITH  ;
                  ROUND(prepro /  ;
                  fraccion, 2)
          IF nsaldo = 0.00 
               DELETE NEXT 1
          ENDIF
     ELSE
          DO standby WITH  ;
             'Error no se actualizo Exisencias'
     ENDIF
ENDIF
SELECT kardex
SET ORDER TO (cord)
RETURN
PARAMETER cperiodo, ccodart,  ;
          ccorrel
PRIVATE cord
cord = ORDER()
SET ORDER TO KardexV1
SEEK cperiodo + ccodart + ccorrel
nsaldo = 0
IF FOUND()
     SCAN WHILE periodo =  ;
          cperiodo AND codart =  ;
          ccodart AND correl =  ;
          ccorrel
          soper = IIF(tipmov =  ;
                  '+', 1, -1)
          nsaldo = nsaldo + soper *  ;
                   totcan
     ENDSCAN
     SKIP -1
     REPLACE saldo WITH nsaldo
     SELECT stkalmv
     IF SEEK(cperiodo + ccodart +  ;
        ccorrel)
          REPLACE salfrac WITH  ;
                  nsaldo
          REPLACE salcant WITH  ;
                  nsaldo /  ;
                  fraccion
     ENDIF
ELSE
     SELECT stkalmv
     IF SEEK(cperiodo + ccodart +  ;
        ccorrel)
          REPLACE salfrac WITH  ;
                  nsaldo
          REPLACE salcant WITH  ;
                  nsaldo /  ;
                  fraccion
          IF nsaldo = 0.00 
               DELETE NEXT 1
          ENDIF
     ELSE
          DO standby WITH  ;
             'Error no se actualizo Exisencias'
     ENDIF
ENDIF
SELECT kardex
SET ORDER TO (cord)
RETURN
*
PROCEDURE agr_art
*----------------
PRIVATE malias
ON KEY LABEL F10
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
malias = ALIAS()
SELECT iteart
IF !USED('Cuenta')
     USE IN 0 Cuentas ALIAS  ;
         cuenta ORDER Cuentas1
ENDIF
DEFINE WINDOW w_art FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Medicina'  ;
       DOUBLE COLOR SCHEME 5
m.tipart = 'B'
m.codgen = '62'
m.codcla = SPACE(3)
m.coddet = SPACE(4)
m.codart = SPACE(12)
m.descri = SPACE(60)
m.coduni = SPACE(10)
m.unifrac = SPACE(10)
m.lfracc = SPACE(1)
m.fraccion = 0
m.cuenta = SPACE(10)
ACTIVATE WINDOW w_art
@ 01, 1 SAY '             Tipo: ' +  ;
  m.tipart
@ 02, 1 SAY '   Grupo Generico:'  ;
  GET m.codgen WHEN .F.
@ 03, 1 SAY '      Laboratorio:'  ;
  GET m.codcla VALID val_esp('B' +  ;
  m.codgen + '.' + m.codcla,' ', ;
  23) AND val_det()
@ 04, 1 SAY '          Detalle:'  ;
  GET m.coddet DISABLE
@ 05, 1 SAY '      Correlativo:'  ;
  GET m.codart DISABLE
@ 06, 1 SAY '         Medicina:'  ;
  GET m.descri FUNCTION 'S40'  ;
  VALID cproper(m.descri)
@ 07, 1 SAY '         Menudeo?:'  ;
  GET m.lfracc PICTURE '@M S,N'  ;
  VALID val_lote1()
@ 08, 1 SAY '    Unidad (Lote):'  ;
  GET m.coduni VALID  ;
  cproper(m.coduni)
@ 09, 1 SAY '  Unidad Fraccion:'  ;
  GET m.unifrac VALID  ;
  cproper(m.unifrac)
@ 10, 1 SAY '         Fraccion:'  ;
  GET m.fraccion VALID (m.lfracc =  ;
  'S' AND m.fraccion >= 1) OR  ;
  (m.lfracc = 'N' AND  ;
  m.fraccion = 1)
READ VALID val_read()
DEACTIVATE WINDOW w_art
RELEASE WINDOW w_art
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
     ENDIF
ENDIF
USE IN cuenta
SELECT (malias)
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F2 DO FunBusDet
ON KEY LABEL F5 DO Agr_Art
ON KEY LABEL F8
RETURN
*
PROCEDURE val_lote1
IF UPPER(m.lfracc) <> 'S'
     m.fraccion = 1
ENDIF
SHOW GET m.fraccion
RETURN
*
PROCEDURE reporte
PARAMETER _tipo, _form, _tit,  ;
          num_c, _wp, ran_pg
PRIVATE _wndold
_wndold = WOUTPUT()
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing
_conso = SET('CONSOLE')
SET CONSOLE ON
DEFINE WINDOW msg2use FROM 07, 02  ;
       TO 11, 77 FLOAT SHADOW  ;
       DOUBLE
_wp = .F.
IF PARAMETERS() < 5
     _wp = .F.
ENDIF
IF PARAMETERS() < 4
     num_c = 1
ENDIF
IF num_c <> 1
     DEFINE WINDOW _xyx FROM 15,  ;
            40 TO 18, 75 TITLE  ;
            ' # DE COPIAS '  ;
            COLOR SCHEME 10
     ACTIVATE WINDOW _xyx
     CLEAR
     @ 1, 5 SAY 'Copias : ' GET  ;
       num_c PICTURE '99' VALID  ;
       (num_c > 0)
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF ! ;
              EMPTY(_wndold)
               ACTIVATE WINDOW &_wndold
          ELSE
               ACTIVATE SCREEN
          ENDIF
          SET CONSOLE &_conso
          RETURN
     ENDIF
     _ncopies = num_c
ENDIF
IF PARAMETERS() >= 6 AND ran_pg
     DEFINE WINDOW _xyx FROM 15,  ;
            40 TO 18, 75 TITLE  ;
            ' Rango de P ginas '  ;
            COLOR SCHEME 10
     ACTIVATE WINDOW _xyx
     CLEAR
     vpbpage = 1
     vpepage = 32767
     @ 0, 5 SAY 'Inicio : ' GET  ;
       vpbpage PICTURE '99,999'  ;
       VALID vpbpage <= vpepage
     @ 1, 5 SAY '   Fin : ' GET  ;
       vpepage PICTURE '99,999'  ;
       VALID vpbpage <= vpepage  ;
       AND vpepage <= 32767
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF ! ;
              EMPTY(_wndold)
               ACTIVATE WINDOW &_wndold
          ELSE
               ACTIVATE SCREEN
          ENDIF
          SET CONSOLE &_conso
          RETURN
     ENDIF
ENDIF
_dest = 'Pantalla '
IF !_wp
     ACTIVATE WINDOW msg2use
     _dest = 'Impresora'
     title = ' DESTINO DE IMPRESION '
     p_fil = SPACE(8)
     _dest1 = 1
     @ 01, 5 SAY  ;
       'Destino de impresi¢n : Pantalla/Impresora/Archivo'  ;
       GET _dest PICTURE  ;
       '@M Impresora,Pantalla,Archivo'
     READ
     IF _dest = 'Archivo  '
          CLEAR
          @ 01, 20 SAY  ;
            'Nombre del Archivo :'  ;
            GET p_fil PICTURE  ;
            'NNNNNNNN' VALID   ;
           !EMPTY(p_fil)
          READ
     ENDIF
     p_fil = ALLTRIM(p_fil) +  ;
             '.LST'
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
impre = (_dest = 'Impresora')
IF !impre AND _dest =  ;
    'Pantalla '
     p_fil = SYS(3) + '.LST'
ENDIF
printing = .T.
IF impre
     IF !EMPTY(LEFT(SYS(0),  ;
         15))
          IF ! ;
              yesno( ;
              '¨Imprime en impresora local?' ;
              )
               SET PRINTER TO \\ibm_pc\printq_0=lpt1
               SET PRINTER TO \\spooler\nb
          ENDIF
     ENDIF
     IF !ready2pr()
          printing = .F.
     ENDIF
ENDIF
IF printing
     DEFINE WINDOW _repo FROM 0,  ;
            0 TO 24, 79 TITLE  ;
            _tit COLOR SCHEME 10
     ACTIVATE WINDOW _repo
     CLEAR
     IF impre
          p_fil = SYS(3) + '.LST'
          SET PRINTER TO &p_fil
          IF _tipo = 1
               IF PARAMETERS() >=  ;
                  6 AND ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_form;
ENVIRONMENT NOEJECT TO PRINT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_form;
ENVIRONMENT NOEJECT TO pri
               ENDIF
          ELSE
               IF PARAMETERS() >=  ;
                  6 AND ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_form;
NOEJECT TO PRINT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_form;
NOEJECT TO PRINT
               ENDIF
          ENDIF
          SET PRINTER TO
          IF !_wp
               FOR i = 1 TO num_c
                    IF ready2pr()  ;
                       AND  ;
                       IIF(num_c >  ;
                       1,  ;
                       yesno( ;
                       'Copia ' +  ;
                       STR(i, 2) +  ;
                       ' .¨Prepare el papel. Listo?' ;
                       ), .T.)
                         !TYPE &p_fil;
>prn
                    ENDIF
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
               ENDFOR
          ELSE
               !FOXSWAP wp &p_fil
          ENDIF
          ERASE &p_fil
     ELSE
          IF _tipo = 1
               IF PARAMETERS() >=  ;
                  6 AND ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_form;
ENVIRONMENT TO FILE (p_fil) NOEJECT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_form;
ENVIRONMENT TO FILE (p_fil) NOEJECT
               ENDIF
          ELSE
               IF PARAMETERS() >=  ;
                  6 AND ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_form;
TO FILE (p_fil) NOEJECT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_form;
TO FILE (p_fil) NOEJECT
               ENDIF
          ENDIF
          IF !_wp
               IF _dest =  ;
                  'Pantalla '
                    MODIFY COMMAND  ;
                           (p_fil)  ;
                           NOEDIT  ;
                           WINDOW  ;
                           _repo
                    IF yesno( ;
                       '¨Imprime el reporte ? ' ;
                       )
                         FOR i =  ;
                             1 TO  ;
                             num_c
                              IF ready2pr()  ;
                                 AND  ;
                                 IIF(num_c >  ;
                                 1,  ;
                                 yesno( ;
                                 'Copia ' +  ;
                                 STR(i,  ;
                                 2) +  ;
                                 ' .¨Prepare el papel. Listo?' ;
                                 ),  ;
                                 .T.)
                                   !TYPE &p_fil >prn
                              ENDIF
                              IF LASTKEY() =  ;
                                 27
                                   EXIT
                              ENDIF
                         ENDFOR
                    ENDIF
               ENDIF
          ELSE
               !FOXSWAP wp &p_fil
          ENDIF
          IF _dest <> 'Archivo  '
               ERASE (p_fil)
          ENDIF
     ENDIF
     DEACTIVATE WINDOW _repo
ELSE
     DO standby WITH  ;
        'EL REPORTE HA SIDO CANCELADO.'
ENDIF
RELEASE WINDOW msg2use
ON ESCAPE
SET ESCAPE OFF
IF _tipo = 1
     CLOSE DATABASES
ENDIF
IF !EMPTY(_wndold)
     ACTIVATE WINDOW &_wndold
ELSE
     ACTIVATE SCREEN
ENDIF
SET CONSOLE &_conso
RETURN
*
PROCEDURE repprg
*---------------
PARAMETER _prgrpt, _tit, _copia, _wp, ran_pg
PRIVATE resul
resul = 'OK'
IF PARAMETERS() < 3
     _copia = 1
ENDIF
IF _copia <> 1
     DEFINE WINDOW _xyx FROM 15,45 TO 19, 70 TITLE ' # DE COPIAS ' COLOR SCHEME 10
     ACTIVATE WINDOW _xyx
     CLEAR
     @ 1, 5 SAY ' N§ Copias : ' GET _copia PICTURE '99' VALID (_copia > 0)
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF ! ;
              EMPTY(_wndold)
               ACTIVATE WINDOW &_wndold
          ELSE
               ACTIVATE SCREEN
          ENDIF
          SET CONSOLE &_conso
          RETURN
     ENDIF
     _ncopies = _copia
ENDIF
IF PARAMETERS() = 3
     _wp = .F.
ENDIF
IF PARAMETERS() >= 5 AND ran_pg
     DEFINE WINDOW _xyx FROM 15,40 TO 18, 75 TITLE ' Rango de P ginas ' COLOR SCHEME 10
     ACTIVATE WINDOW _xyx
     CLEAR
     vpbpage = 1
     vpepage = 32767
     @ 0, 5 SAY 'Inicio : ' GET vpbpage PICTURE '99,999' VALID vpbpage <= vpepage
     @ 1, 5 SAY '   Fin : ' GET vpepage PICTURE '99,999' VALID vpbpage <= vpepage AND vpepage <= 32767
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF ! ;
              EMPTY(_wndold)
               ACTIVATE WINDOW &_wndold
          ELSE
               ACTIVATE SCREEN
          ENDIF
          SET CONSOLE &_conso
          RETURN
     ENDIF
ENDIF
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing
IF !WEXIST('MSG2USE')
     DEFINE WINDOW msg2use FROM 12, 02 TO 16, 77 FLOAT SHADOW DOUBLE
ENDIF
_dest = 'Pantalla '
IF !_wp
     ACTIVATE WINDOW msg2use
     _dest = 'Impresora'
     title = ' DESTINO DE IMPRESION '
     p_fil = SPACE(8)
     _dest1 = 1
     @ 01, 5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _dest PICTURE '@M Impresora,Pantalla,Archivo'
     READ
     IF _dest = 'Archivo  '
          CLEAR
          @ 01, 20 SAY 'Nombre del Archivo :' GET p_fil PICTURE 'NNNNNNNN' VALID !EMPTY(p_fil)
          READ
     ENDIF
     p_fil = ALLTRIM(p_fil) + '.LST'
     RELEASE WINDOW msg2use
ENDIF
IF MOD(READKEY(), 256) = 12
     ON KEY
     SET ESCAPE OFF
     ACTIVATE SCREEN
     RETURN
ENDIF
impre = (_dest = 'Impresora')
IF !impre AND _dest = 'Pantalla '
     p_fil = SYS(3) + '.LST'
ENDIF
printing = .T.
IF impre
     IF !EMPTY(LEFT(SYS(0),18))
          IF !yesno('IMPRIME EN IMPRESORA LOCAL ?  <NO = IMPRESORA DE RED>')
               SET PRINTER TO \\ibm_pc\printq_0=lpt1
               SET PRINTER TO \\spooler\nb
          ENDIF
          IF !ready2pr()
               printing = .F.
          ENDIF
     ENDIF
ENDIF
IF printing
     DEFINE WINDOW _repo FROM 0,0 TO 24, 79 TITLE _tit FOOTER  ;
            ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] ' COLOR SCHEME 10
     ACTIVATE WINDOW _repo
     CLEAR
     IF impre
          DO &_prgrpt WITH 1
     ELSE
          @ 02, 20 SAY '** Reporte en ejecuci¢n **'
          DO &_prgrpt WITH 2
          IF _wp
               ACTIVATE SCREEN
               RESTORE SCREEN FROM pantalla
               RUN FOXSWAP wp &p_fil
          ELSE
               MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
          ENDIF
          IF resul = 'OK'
               IF yesno('¨ Imprime el Reporte ?')
                    FOR v = 1 TO _copia
                         IF ready2pr() AND IIF(_copia>1, yesno('Copia '+STR(v,2)+' .¨Prepare el papel. Listo?'),.T.)
                              !TYPE &p_fil>prn
                         ENDIF
                    ENDFOR
               ENDIF
          ELSE
               resul = 'OK'
          ENDIF
          IF _dest = 'Impresora' OR _dest = 'Pantalla '
               ERASE (p_fil)
          ENDIF
     ENDIF
     DEACTIVATE WINDOW _repo
ELSE
     DO standby WITH 'El reporte ha sido cancelado'
ENDIF
ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN
RETURN
*
FUNCTION val_prv
*---------------
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) AND  ;
       _tipo
xcod = ALLTRIM(xcod)
_campo = VARREAD()
malias = ALIAS()
SELECT promae
_oldwnd = WOUTPUT()
IF !medita
     SEEK xcod
     v_fun = IIF(FOUND(), nompro,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 2
          ON KEY LABEL enter KEYBOARD;
CHR(23)
          DEFINE WINDOW _xx FROM  ;
                 3, 3 TO 22, 77
          BROWSE FIELDS codprv :H =  ;
                 'C¢digo', nompro  ;
                 :H = 'Nombre' :  ;
                 45, dirpro :H =  ;
                 'Direccci¢n' :  ;
                 25 NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' ®Enter¯  Selecciona    ®F2¯ Busca  '
          ON KEY LABEL enter
          ON KEY LABEL f2
          RELEASE WINDOW _xx
          SET ORDER TO 1
          IF ! ;
              EMPTY(_oldwnd)
               ACTIVATE WINDOW &_oldwnd
          ENDIF
          IF LASTKEY() = 27
               v_fun = .F.
          ELSE
               xcod = codprv
               IF mmsg
                    @ _x, _y SAY  ;
                      nompro
               ENDIF
               SELECT (malias)
               IF !_tipo
                    REPLACE &_campo WITH;
 xcod
               ENDIF
               v_fun = .T.
          ENDIF
     ELSE
          SEEK xcod
          IF mmsg AND FOUND()
               @ _x, _y SAY  ;
                 nompro
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
SELECT (malias)
RETURN v_fun
*
PROCEDURE clrscr
PRIVATE fil, col
fil = 11
col = 39
DO WHILE (col>0) AND (fil>0)
     @ fil, col CLEAR TO 23 - fil,  ;
       79 - col
     fil = fil - 1
     col = col - 3
     = INKEY(0.001 , 'H')
ENDDO
CLEAR
RETURN
*
PROCEDURE funbusdet
PRIVATE kf2, kf5, kf8
kf2 = ON('KEY', 'F2')
kf5 = ON('KEY', 'F5')
kf8 = ON('KEY', 'F8')
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8

IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF(!EOF(), RECNO(), - ;
        1)
cord1 = ORDER()
vbusca = SPACE(30)
vnombre = 'Nombre : '
SET ORDER TO IteArt2
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 13,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            10
     ACTIVATE WINDOW lista
     @ 1, 2 SAY vnombre GET  ;
       vbusca
     READ
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) OR LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(UPPER(vbusca))
     IF !FOUND()
          DO standby WITH  ;
             'Medicamento no Existe'
          IF vtemp = -1
               GOTO BOTTOM
          ELSE
               GOTO vtemp
          ENDIF
     ELSE
     ENDIF
ENDIF
SET ORDER TO &cOrd1
ON KEY LABEL F2 &kf2
ON KEY LABEL F5 &kf5
ON KEY LABEL F8 &kf8
RETURN
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF(!EOF(), RECNO(), - ;
        1)
cord1 = ORDER()
vbusca = SPACE(30)
vnombre = 'Nombre : '
SET ORDER TO IteArt2
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 13,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            10
     ACTIVATE WINDOW lista
     @ 1, 2 SAY vnombre GET  ;
       vbusca
     READ
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) OR LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(UPPER(vbusca))
     IF !FOUND()
          DO standby WITH  ;
             'Medicamento no Existe'
          IF vtemp = -1
               GOTO BOTTOM
          ELSE
               GOTO vtemp
          ENDIF
     ELSE
     ENDIF
ENDIF
SET ORDER TO &cOrd1
RETURN
*
FUNCTION verest
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
     CASE vest = 'VG'
          vfun = 'Vigente     '
     CASE vest = 'RG'
          vfun = 'Regularizar '
     CASE vest = 'PD'
          vfun = 'Pendiente   '
     CASE vest = 'AN'
          vfun = 'Anulado     '
ENDCASE
RETURN vfun
*
FUNCTION art_alm
*---------------
PARAMETER xcod
PRIVATE cord
PUBLIC m.correl
ON KEY LABEL F5
ON KEY LABEL F6
ON KEY LABEL F8
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT stkalmv
cord = ORDER()
SET ORDER TO StkAlmV3
GOTO TOP
IF !EMPTY(xcod)
     SEEK m.periodo + xcod
     v_fun = IIF(FOUND(), desart,  ;
             '')
     IF FOUND()
          m.lmenart = lfracc
          m.fracart = fraccion
          m.preven2 = preven2
          m.preven2fr = preven2fr
     ENDIF
     v_ent = FOUND()
ENDIF
IF EMPTY(xcod) OR !v_ent
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO BusDetKar
     ON KEY LABEL F5
     ON KEY LABEL F8
     DEFINE WINDOW _busart FROM 2,01 TO 22, 78
     ACTIVATE WINDOW _busart
     SET ORDER TO StkAlmV4
     DO busdetkar
     BROWSE FIELDS desart :H = 'Nombre' : 30,;
     				lfracc :H = 'Fr',;
     				unidad :H = 'Unidad' : 4,;
     				unifrac :H = 'U. Fr.' : 4,;
     				salcant :H = 'Stk' :P = '9999',;
     				salfrac :H = 'Stk Fr' :P = '9999',;
     				prevenfr :H = 'P.U. Fr.', ;
     				preven :H = 'P. U.',;
     				preven2fr :H = '2 PU Fr.', ;
     				preven2 :H = '2 PU.';
     				NOMENU NOAPPEND NOEDIT NODELETE WINDOW _busart;
     				TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²'
     SET ORDER TO StkAlmV3
     ON KEY LABEL F10
     ON KEY LABEL F2
     ON KEY LABEL F5
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() = 27
          v_fun = .F.
          v_ent = .F.
     ELSE
          xcod = codart
          m.correl = correl
          m.desart = desart
          m.unidad = unidad
          m.unifrac = unifrac
          m.lmenart = lfracc
          m.fracart = fraccion
          m.preuni = preUni
*          m.preuni = prepro
          m.igv = igv
          m.valuni = valuni
          m.valunifr = valunifr
          m.preven   = IIF(m.CodPre=1,preven,preven2)
          m.prevenfr = IIF(m.CodPre=1,prevenfr,preven2fr)
*          m.preven2 = preven2
*          m.preven2fr = preven2fr
          SHOW GETS
          v_fun = .T.
          v_ent = .T.
     ENDIF
ELSE
     xcod = codart
     m.correl = correl
     m.desart = desart
     m.unidad = unidad
     m.unifrac = unifrac
     m.lmenart = lfracc
     m.fracart = fraccion
     m.preuni = preUni
*     m.preuni = prepro
     m.igv = igv
     m.valuni = valuni
     m.valunifr = valunifr
     m.preven   = IIF(m.CodPre=1,preven,preven2)
     m.prevenfr = IIF(m.CodPre=1,prevenfr,preven2fr)
*     m.preven2 = preven2
*     m.preven2fr = preven2fr
     SHOW GETS
     v_fun = .T.
     v_ent = .T.
ENDIF
ON KEY LABEL F5 DO Agrega_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT stkalmv
SET ORDER TO (cord)
SELECT (malias)
IF !v_ent
     RETURN v_fun
ELSE
     RETURN v_ent
ENDIF
RETURN
PARAMETER xcod
PRIVATE cord
PUBLIC m.correl
ON KEY LABEL F5
ON KEY LABEL F8
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT stkalmv
cord = ORDER()
SET ORDER TO StkAlmV3
GOTO TOP
IF !EMPTY(xcod)
     SEEK m.periodo + xcod
     v_fun = IIF(FOUND(), desart,  ;
             '')
     v_ent = FOUND()
ENDIF
IF EMPTY(xcod) OR !v_ent
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO BusDetKar
     ON KEY LABEL F5
     ON KEY LABEL F8
     DEFINE WINDOW _busart FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _busart
     SET ORDER TO StkAlmV4
     DO busdetkar
     BROWSE FIELDS desart :H =  ;
            'Nombre' : 30, lfracc  ;
            :H = 'Fr', unidad :H =  ;
            'Unidad' : 4, unifrac  ;
            :H = 'U. Fr.' : 4,  ;
            salcant :H = 'Stk' :P =  ;
            '9999', salfrac :H =  ;
            'Stk Fr' :P = '9999',  ;
            prevenfr :H =  ;
            'P.U. Fr.', preven :H =  ;
            'P. U.' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            _busart TITLE  ;
            '²²²² [F10] Selecciona   [F2] Buscar ²²²²'
     SET ORDER TO StkAlmV3
     ON KEY LABEL F10
     ON KEY LABEL F2
     ON KEY LABEL F5
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() = 27
          v_fun = .F.
          v_ent = .F.
     ELSE
          xcod = codart
          m.correl = correl
          m.desart = desart
          m.unidad = unidad
          m.unifrac = unifrac
          m.lfracc = lfracc
          m.fraccion = fraccion
          m.preuni = preuni
          m.igv = igv
          m.valuni = valuni
          m.valunifr = valunifr
          m.preven = preven
          m.prevenfr = prevenfr
          SHOW GETS
          v_fun = .T.
          v_ent = .T.
     ENDIF
ELSE
     xcod = codart
     m.correl = correl
     m.desart = desart
     m.unidad = unidad
     m.unifrac = unifrac
     m.lfracc = lfracc
     m.fraccion = fraccion
     m.preuni = preuni
     m.igv = igv
     m.valuni = valuni
     m.valunifr = valunifr
     m.preven = preven
     m.prevenfr = prevenfr
     SHOW GETS
     v_fun = .T.
     v_ent = .T.
ENDIF
ON KEY LABEL F5 DO Agrega_item
ON KEY LABEL F8 DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT stkalmv
SET ORDER TO (cord)
SELECT (malias)
IF !v_ent
     RETURN v_fun
ELSE
     RETURN v_ent
ENDIF
RETURN
*
PROCEDURE busdetkar
PRIVATE kf2, kf5, kf8
kf2 = ON('KEY', 'F2')
kf5 = ON('KEY', 'F5')
kf8 = ON('KEY', 'F8')
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF(!EOF(), RECNO(), - ;
        1)
cord1 = ORDER()
vperiodo = RIGHT(STR(YEAR(DATE()),  ;
           4), 2)
vbusca = SPACE(30)
vnombre = 'Nombre :'
SET ORDER TO StkAlmV4
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 13,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            10
     ACTIVATE WINDOW lista
     @ 0, 2 SAY 'Periodo:' GET  ;
       vperiodo WHEN .F.
     @ 1, 2 SAY vnombre GET  ;
       vbusca PICTURE '@!'
     READ
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) OR LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vperiodo +  ;
          vbusca)
     IF !FOUND()
          DO standby WITH  ;
             'Articulo no Existe'
          IF vtemp = -1
               GOTO BOTTOM
          ELSE
               GOTO vtemp
          ENDIF
     ELSE
     ENDIF
ENDIF
SET ORDER TO &cOrd1
ON KEY LABEL F2 &kf2
ON KEY LABEL F5 &kf5
ON KEY LABEL F8 &kf8
RETURN
*
FUNCTION ready2pr
DO WHILE !PRINTSTATUS()
     IF !yesno( ;
         'La Impresora est  apagada.¨ Continua ?' ;
         )
          RETURN .F.
     ENDIF
ENDDO
RETURN .T.
*
FUNCTION det_kar
PARAMETER xcod, xcod1, mvariable,  ;
          mcol
PRIVATE mfiltro
malias = ALIAS()
v_fun = .F.
DO CASE
     CASE PARAMETERS() = 1
          mvariable = ' '
          mcol = 0
     CASE PARAMETERS() = 2
          mcol = 0
ENDCASE
SELECT stkalmv
GOTO TOP
IF !EMPTY(xcod)
     SEEK xcod + '.' + xcod1
     v_fun = FOUND()
ENDIF
IF EMPTY(xcod) OR EMPTY(xcod1)  ;
   OR !v_fun
     SET FILTER TO periodo + codart =;
xcod
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     DEFINE WINDOW _busart FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _busart
     BROWSE FIELDS codart :H =  ;
            'C¢digo', desart :H =  ;
            'Nombre' : 40 NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            _busart KEY LEFT(xcod,  ;
            6) TITLE  ;
            '²²²² [F10] Selecciona ²²²²'
     ON KEY LABEL F10
     SET FILTER TO
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() <> 27
          xcod1 = SUBSTR(codart,  ;
                  8, 4)
          mdescri = desart
          v_fun = .T.
     ELSE
          v_fun = .F.
          ON KEY LABEL F10 KEYBOARD CHR(23)
          UNLOCK ALL
          SELECT (malias)
          RETURN v_fun
     ENDIF
ELSE
     mdescri = desart
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT (malias)
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN m.codcla
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescri
     CASE mvariable == 'D'
          RETURN mdescri
     CASE mvariable == 'Z'
          RETURN mdescri
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          RETURN .T.
ENDCASE
RETURN
PARAMETER xcod, mvariable, mcol
PRIVATE mfiltro
malias = ALIAS()
v_fun = .F.
DO CASE
     CASE PARAMETERS() = 1
          mvariable = ' '
          mcol = 0
     CASE PARAMETERS() = 2
          mcol = 0
ENDCASE
SELECT stkalmv
GOTO TOP
IF !EMPTY(xcod)
     SEEK xcod
     v_fun = FOUND()
ENDIF
IF EMPTY(xcod) OR !v_fun
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     DEFINE WINDOW _busart FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _busart
     BROWSE FIELDS codart :H =  ;
            'C¢digo', desart :H =  ;
            'Nombre' : 40 NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            _busart KEY LEFT(xcod,  ;
            6) TITLE  ;
            '²²²² [F10] Selecciona ²²²²'
     ON KEY LABEL F10
     DEACTIVATE WINDOW _busart
     RELEASE WINDOW _busart
     IF LASTKEY() <> 27
          m.coddet = SUBSTR(codart,  ;
                     8, 4)
          mdescri = desart
          v_fun = .T.
     ELSE
          v_fun = .F.
          ON KEY LABEL F10 KEYBOARD CHR(23)
          UNLOCK ALL
          SELECT (malias)
          RETURN v_fun
     ENDIF
ELSE
     mdescri = desart
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
SELECT (malias)
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescri
          RETURN m.codcla
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescri
     CASE mvariable == 'D'
          RETURN mdescri
     CASE mvariable == 'Z'
          RETURN mdescri
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          RETURN .T.
ENDCASE
RETURN
*
PROCEDURE progreso
*-----------------
PARAMETER control, cmens, total, actual
DO CASE
	CASE control = 1
		DEFINE WINDOW vview FROM 08, 11 TO 14, 68 DOUBLE COLOR SCHEME 5
		ACTIVATE WINDOW vview
		@ 02, 00 TO 04, 57
		@ 00, 00 SAY cmens
	CASE control = 2
		vtotc = 54
		porca = ROUND((actual * 100) / total, 2)
		numc = ROUND((porca * 54) / 100, 2)
		@ 03, 01 SAY REPLICATE('Û', numc)
		@ 01, 52 SAY porca PICTURE '999%'
	CASE control = 3
		RELEASE WINDOW vview
ENDCASE
RETURN
*
PROCEDURE ctrldb
*---------------
PARAMETER nopcion
USE parmae ALIAS parma ORDER Parmae1
DO CASE
	IF !SEEK('CTRLDB')
		DO standby WITH 'Error de Parametro Avise al Area de Sistemas'
	ENDIF
	CASE nopcion = 1
		SEEK 'CTRLDB'
		IF codigo = '02'
			DO standby WITH 'No salio Correctamente del sistema'
			DO indexa WITH 1
		ELSE
			IF SEEK('CTRLDB')
				REPLACE codigo WITH '02'
			ENDIF
		ENDIF
	CASE nopcion = 2
		IF SEEK('CTRLDB')
			REPLACE codigo WITH '01'
		ENDIF
ENDCASE
*
FUNCTION ve_passw
*----------------
PARAMETER _psw
PRIVATE _ln, xxx
_ln = LEN(_psw)
xxx = SPACE(_ln)
DEFINE WINDOW _pasw FROM 12, 15 TO 17, 65 TITLE ' Password de seguridad ' COLOR SCHEME 5
ACTIVATE WINDOW _pasw
CLEAR
@ 1, 4 SAY 'Opci¢n restringida, ingrese password: '
@ 2, 20 SAY '[' + SPACE(_ln) + ']'
@ 2, 21 GET xxx PICTURE '@!' COLOR ,N 
READ
RELEASE WINDOW _pasw
IF LASTKEY() <> 27 AND xxx = _psw
     RETURN .T.
ELSE
     DO standby WITH 'Password incorrecto. Acceso denegado !'
ENDIF
RETURN .F.
*
PROCEDURE nada
*-------------
RETURN
*
FUNCTION letras
*--------------
PARAMETER wmonto, simbolo
wletras = ''
DIMENSION letras( 4, 9)
letras( 1, 1) = 'UN '
letras( 1, 2) = 'DOS '
letras( 1, 3) = 'TRES '
letras( 1, 4) = 'CUATRO '
letras( 1, 5) = 'CINCO '
letras( 1, 6) = 'SEIS '
letras( 1, 7) = 'SIETE '
letras( 1, 8) = 'OCHO '
letras( 1, 9) = 'NUEVE '
letras( 2, 1) = 'ONCE '
letras( 2, 2) = 'DOCE '
letras( 2, 3) = 'TRECE '
letras( 2, 4) = 'CATORCE '
letras( 2, 5) = 'QUINCE '
letras( 2, 6) = 'DIECISEIS '
letras( 2, 7) = 'DIECISIETE '
letras( 2, 8) = 'DIECIOCHO '
letras( 2, 9) = 'DIECINUEVE '
letras( 3, 1) = 'DIEZ'
letras( 3, 2) = 'VEINTI'
letras( 3, 3) = 'TREINTA'
letras( 3, 4) = 'CUARENTA'
letras( 3, 5) = 'CINCUENTA'
letras( 3, 6) = 'SESENTA'
letras( 3, 7) = 'SETENTA'
letras( 3, 8) = 'OCHENTA'
letras( 3, 9) = 'NOVENTA'
letras( 4, 1) = 'CIENTO '
letras( 4, 2) = 'DOSCIENTOS '
letras( 4, 3) = 'TRESCIENTOS '
letras( 4, 4) = 'CUATROCIENTOS '
letras( 4, 5) = 'QUINIENTOS '
letras( 4, 6) = 'SEISCIENTOS '
letras( 4, 7) = 'SETECIENTOS '
letras( 4, 8) = 'OCHOCIENTOS '
letras( 4, 9) = 'NOVECIENTOS '
centavos = wmonto - INT(wmonto)
w2 = STR(INT(wmonto), 18)
i = 1
DO WHILE i<=18
	w1 = 0
	n = SUBSTR(w2, i, 3)
	IF VAL(n) > 0
          x = VAL(n)
          y = 1
          DO WHILE y<=3
               x1 = SUBSTR(n, y,1)
               IF VAL(x1) <> 0
                    DO CASE
                         CASE y = 1
                              IF VAL(n) = 100
                                   wletras = wletras + 'CIEN '
                              ELSE
                                   wletras = wletras + letras(4,VAL(x1))
                              ENDIF
                         CASE y = 2
                              DO CASE
                                   CASE VAL(SUBSTR(n, y, 2)) = 20
                                        wletras = wletras + 'VEINTE'
                                   CASE VAL(SUBSTR(n, y, 2)) > 10 AND VAL(SUBSTR(n, y, 2)) < 20
                                        wletras = wletras + letras(2,VAL(SUBSTR(n, 3, 1)))
                                   OTHERWISE
                                        wletras = wletras + letras(3,VAL(x1))
                              ENDCASE
                         CASE y = 3
                              IF !SUBSTR(n,2,1)$'1'
                                   IF !SUBSTR(n, 2, 1) $ '02' AND !EMPTY(wletras)
                                        wletras = wletras + ' Y '
                                   ENDIF
                                   wletras = wletras + letras(1,VAL(x1))
                              ENDIF
                    ENDCASE
               ENDIF
               y = y + 1
          ENDDO
          DO CASE
               CASE i = 1 OR i = 7 OR i = 13
                    wletras = wletras + 'MIL '
               CASE i = 4
                    wletras = wletras + 'BILLON '
               CASE i = 10
                    IF x = 1
                         wletras = wletras + 'MILLON '
                    ELSE
                         wletras = wletras + 'MILLONES '
                    ENDIF
          ENDCASE
     ENDIF
     i = i + 3
ENDDO
wletras = ALLTRIM(wletras) + ' Y  ' + PADL(ALLTRIM(STR(centavos * 100, 2)), 2, '0') + '/100 ' + simbolo
ll = SUBSTR(wletras, 85, 15)
l = 0
t = 16
r = LEN(wletras)
IF r > 100
     DO WHILE t<>20
          t = t - 1
          p = SUBSTR(ll, t, 1)
          IF p = ' '
               numlet1 = SUBSTR(wletras,1, 85) + SUBSTR(ll,1, t)
               numlet2 = SUBSTR(wletras,(86 + t),(r - (85 + t)))
               t = 20
          ENDIF
     ENDDO
ELSE
     numlet1 = wletras
ENDIF
RETURN numlet1
*
FUNCTION Val_Ruc
*---------------
** _tipo = .F. ---> Campo
**         .T. ---> Variable.

PARAMETERS xRuc,_Fil,_Col			&&,_tipo,_x,_y
PRIVATE cOrd, mval

mAlias = ALIAS()
v_fun = .F.
v_Ent = .F.

SELE Clien
GO TOP
cOrd = ORDER()
SET ORDER TO Clientes3

IF !EMPTY(xRuc)
	SEEK xRuc
	v_Ent = FOUND()
	v_Fun  = .T.
ENDIF

IF !V_ENT
	SET ORDER TO Clientes2
	GO TOP
	ACTIVATE SCREEN
	ON KEY LABEL F10 KEYBOARD CHR(23)
	ON KEY LABEL F2 DO BusDetRuc
	ON KEY LABEL F5 DO Agr_Cli
	
	DEFINE WINDOW _BusCli FROM 2,01 TO 22,78
	ACTIVATE WINDOWS _BusCli
	BROWSE WINDOW _BusCli TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega Cliente ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU;
	 FIELDS	NomCli   :H='Nombre'      :40,;
			RazSoc   :H='Razon Social':40
	
	ON KEY LABEL F10
	ON KEY LABEL F2
	ON KEY LABEL F5
	DEACTIVATE WINDOWS _BusCli
	RELEASE    WINDOW _BusCli
	
	IF Lastkey()=27
		V_FUN = .F.
		v_ent = .F.
	 ELSE
		v_ent = .T.
	ENDIF
ENDIF
		
IF V_ENT
*	xRuc		= RucCli
	m.RucCli	= RucCli
*	m.NomCli	= NomCli
	m.Cliente	= NomCli
*	m.DNICli	= DNICli
*	SELECT (mAlias)
	SHOW GETS
	v_fun = .T.
	
	IF !EMPTY(_Fil) OR !EMPTY(_Col)
		@ _Fil,_Col SAY m.NomCli
	ENDIF
ENDIF

SET ORDER TO &cOrd

ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
UNLOCK ALL

SELECT (mAlias)

RETURN v_Fun

FUNCTION BusDetRuc		&& Realiza b£squeda directa
*-----------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF

vtemp = IIF(!EOF(),RECNO(),-1)

DEFINE POPUP pLista FROM 15,40 COLOR SCHEME c_popup

DEFINE BAR 1  OF pLista PROMPT '\<a. Por RUC         '
DEFINE BAR 2  OF pLista PROMPT '\<b. Por Nombre      '

ON SELECTION POPUP pLista  DEACTIVATE POPUP

ACTIVATE POPUP pLista
RELEASE  POPUP pLista

cOrd1 = ORDER()

DO CASE
	CASE BAR() =  1
		vBusca = SPACE(11)
		vNombre = "RUC :"
		SET ORDER TO TAG Clientes3
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

FUNCTION Agr_Cli
*---------------
PRIVATE mAlias,m.Codcli,m.NomCli,m.DNICli,m.RazSoc,m.RUCCli,m.DirCli

ON KEY LABEL F2
ON KEY LABEL F5
mAlias = ALIAS()
SELE Clien

DEFINE WINDOW w_Cli FROM 05,05 TO 20,70  DOUBLE ;
	TITLE "Ingresando Cliente" COLOR SCHEME 5

m.Codcli	= SPACE(6)
m.NomCli	= SPACE(50)
m.DirCli    = SPACE(50)
m.DNICli	= SPACE(8)
m.RazSoc	= SPACE(50)
m.RUCCli	= SPACE(11)
m.TipCli	= "C"

=Cor_Clien()

ACTIVATE WIND w_Cli

@ 01,1  SAY '       Codigo: ' + m.CodCli
@ 02,1  SAY '       Nombre:' GET m.NomCli		FUNCTION '!S30'
@ 03,1  SAY '    Direccion:' GET m.DirCli		FUNCTION '!S30'
@ 04,1  SAY '       D.N.I.:' GET m.DNICli		PICTURE  '99999999'
@ 05,1  SAY ' Razon Social:' GET m.RazSoc		FUNCTION '!S30'
@ 06,1  SAY "         RUC.:" GET m.RUCCli		PICTURE  '99999999999'

READ VALID val_read()

DEACTIVATE WINDOW w_Cli
RELEASE WINDOW w_Cli

IF LastKey()#27
	IF f_Appd()
		GATHER MEMVAR
		SELE Parma
		SEEK "CORRELCLIENT"
		REPLACE NumEnt WITH Parma.NumEnt+1
	ENDIF
ENDIF

SELE (mAlias)

ON KEY LABEL F5 DO Agr_Cli
ON KEY LABEL F2 DO BusDetRuc

RETURN

FUNCTION Cor_Clien
*-----------------
IF SEEK("CORRELCLIENT","Parma")
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

