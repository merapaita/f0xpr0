*
PROCEDURE fox_ambi
DEACTIVATE WINDOW ALL
DEACTIVATE POPUP
DEACTIVATE MENU
ACTIVATE SCREEN
CLEAR WINDOW
RELEASE ALL
CLEAR ALL
PUBLIC worker
worker = IIF(TRIM(UPPER(GETENV('WORKER'))) =  ;
         'ON', .T., .F.)
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
IF  .NOT. worker
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
IF  .NOT. worker
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
DEFINE WINDOW yesno FROM 19, 27  ;
       TO 23, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME  ;
       5
DEFINE WINDOW espera FROM 12, 06  ;
       TO 14, 78 COLOR SCHEME 05
DEFINE WINDOW msg2user FROM 12,  ;
       02 TO 16, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME  ;
       5
DEFINE WINDOW standby FROM 19, 23  ;
       TO 23, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME  ;
       5
IF worker
     ON ERROR
ELSE
     ON ERROR
ENDIF
RETURN
*
FUNCTION yesno
PARAMETER msgwords, posi, posc
IF  .NOT. WEXIST('yesno')
     DEFINE WINDOW yesno FROM 19,  ;
            27 TO 23, 77 GROW  ;
            FLOAT SHADOW DOUBLE  ;
            COLOR SCHEME 5
ENDIF
DO CASE
     CASE PARAMETERS() = 3
          colp = IIF(posc > 28,  ;
                 29, posc)
          MOVE WINDOW yesno TO  ;
               posi, colp
     CASE PARAMETERS() = 2
          posi = IIF(posi > 19,  ;
                 19, posi)
          colp = 27
          MOVE WINDOW yesno TO  ;
               posi, colp
ENDCASE
ACTIVATE WINDOW yesno
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
IF  .NOT. EMPTY(msg2)
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
IF  .NOT. WEXIST('standby')
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
IF  .NOT. EMPTY(msg2)
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
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo = filtro
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          IF  .NOT. EMPTY(malias)
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
     IF  .NOT. EMPTY(_oldwnd)
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
IF  .NOT. EMPTY(malias)
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
IF  .NOT. FOUND() .AND. mvariable <>  ;
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
     DO WHILE  .NOT. FOUND()
          ACTIVATE POPUP  ;
                   parametro
          SEEK tipo + codigo
     ENDDO
     RELEASE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
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
     IF  .NOT. EMPTY(malias)
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
         .AND. (v__tipo <> 3)
mmsg = (PARAMETERS() >= 7) .AND.  ;
       (v__tipo <> 3)
mvali = (PARAMETERS() = 5) .AND.  ;
        (v__tipo = 3)
v__ver = IIF(PARAMETERS() = 8,  ;
         v__ver, v__bus)
mrec = IIF(EOF(), -1, RECNO())
malias = ALIAS()
_campo = TRIM(malias) + '.' +  ;
         VARREAD()
SELECT &v__al
_oldwnd = WOUTPUT()
IF  .NOT. medita
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
               IF  .NOT.  ;
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
                    IF  .NOT.  ;
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
          IF mmsg .AND. FOUND()
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
IF  .NOT. EMPTY(ALIAS())
     IF mrec > 0
          GOTO mrec
     ELSE
          GOTO BOTTOM
     ENDIF
ENDIF
RETURN v_fun
*
FUNCTION f_appd
APPEND BLANK
v_fun = f_lock(1)
RETURN v_fun
*
FUNCTION f_lock
PARAMETER btipo
beep = CHR(7)
DO CASE
     CASE btipo = 1
          v_fun = .F.
          IF RLOCK()
               v_fun = .T.
          ELSE
               DO standby WITH  ;
                  'Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar.'
               ?? beep
               ktecla = 0
               DO WHILE ktecla<> ;
                  escape .AND. (  ;
                  .NOT. RLOCK())
                    ktecla = INKEY()
               ENDDO
               IF ktecla <>  ;
                  escape
                    v_fun = .T.
               ENDIF
          ENDIF
     CASE btipo = 2
          v_fun = .F.
          IF RLOCK()
               v_fun = .T.
          ELSE
               DO standby WITH  ;
                  'El registro est  siendo utilizado. Se cancela la operaci¢n. '
               ?? beep
          ENDIF
     CASE btipo = 3
          v_fun = .F.
          IF FLOCK()
               v_fun = .T.
          ELSE
               DO standby WITH  ;
                  'Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar.'
               ?? beep
               ktecla = 0
               DO WHILE ktecla<> ;
                  escape .AND. (  ;
                  .NOT. FLOCK())
                    ktecla = INKEY()
               ENDDO
               IF ktecla <>  ;
                  escape
                    v_fun = .T.
               ENDIF
          ENDIF
     CASE btipo = 4
          v_fun = .F.
          IF FLOCK()
               v_fun = .T.
          ELSE
               DO standby WITH  ;
                  'Archivo ocupado.  El proceso se cancela.'
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
             .AND. _xtime) .OR.  ;
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
                  2 .AND. _xtime)  ;
                  .OR.  ;
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
     DO WHILE m.key<>13 .AND.  ;
        m.key<>27
          m.key = INKEY(0)
          DO CASE
               CASE BETWEEN(m.key,  ;
                    65, 90) .OR.  ;
                    BETWEEN(m.key,  ;
                    97, 122) .OR.  ;
                    BETWEEN(m.key,  ;
                    48, 57) .OR.  ;
                    BETWEEN(m.key,  ;
                    164, 165)
                    xpassword = xpassword +  ;
                                CHR(m.key)
               CASE m.key = 19  ;
                    .OR. m.key =  ;
                    127 .OR.  ;
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
IF  .NOT. cur_curs
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
FUNCTION val_read
PRIVATE _fun
_fun = .T.
IF LASTKEY() <> 27
     IF  .NOT. yesno( ;
         '¨ Est n correctos los datos ?' ;
         )
          _fun = .F.
     ENDIF
ENDIF
RETURN (_fun)
*
FUNCTION cproper
PARAMETER ctexto
ctexto = PROPER(ctexto)
RETURN .T.
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
          IF  .NOT.  ;
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
IF PARAMETERS() >= 6 .AND. ran_pg
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
       .AND. vpepage <= 32767
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF  .NOT.  ;
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
IF  .NOT. _wp
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
            .NOT. EMPTY(p_fil)
          READ
     ENDIF
     p_fil = ALLTRIM(p_fil) +  ;
             '.LST'
     RELEASE WINDOW msg2use
ENDIF
IF LASTKEY() = 27
     IF  .NOT. EMPTY(_wndold)
          ACTIVATE WINDOW &_wndold
     ELSE
          ACTIVATE SCREEN
     ENDIF
     SET CONSOLE &_conso
     RETURN
ENDIF
impre = (_dest = 'Impresora')
IF  .NOT. impre .AND. _dest =  ;
    'Pantalla '
     p_fil = SYS(3) + '.LST'
ENDIF
printing = .T.
IF impre
     IF  .NOT. EMPTY(LEFT(SYS(0),  ;
         15))
          IF  .NOT.  ;
              yesno( ;
              '¨Imprime en impresora local?' ;
              )
               SET PRINTER TO \\ibm_pc\printq_0=lpt1
               SET PRINTER TO \\spooler\nb
          ENDIF
     ENDIF
     IF  .NOT. ready2pr()
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
                  6 .AND. ran_pg
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
                  6 .AND. ran_pg
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
          IF  .NOT. _wp
               FOR i = 1 TO num_c
                    IF ready2pr()  ;
                       .AND.  ;
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
                  6 .AND. ran_pg
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
                  6 .AND. ran_pg
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
          IF  .NOT. _wp
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
                                 .AND.  ;
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
IF  .NOT. EMPTY(_wndold)
     ACTIVATE WINDOW &_wndold
ELSE
     ACTIVATE SCREEN
ENDIF
SET CONSOLE &_conso
RETURN
*
PROCEDURE repprg
PARAMETER _prgrpt, _tit, _copia,  ;
          _wp, ran_pg
PRIVATE resul
resul = 'OK'
IF PARAMETERS() < 3
     _copia = 1
ENDIF
IF _copia <> 1
     DEFINE WINDOW _xyx FROM 15,  ;
            45 TO 19, 70 TITLE  ;
            ' # DE COPIAS '  ;
            COLOR SCHEME 10
     ACTIVATE WINDOW _xyx
     CLEAR
     @ 1, 5 SAY ' N§ Copias : '  ;
       GET _copia PICTURE '99'  ;
       VALID (_copia > 0)
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF  .NOT.  ;
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
IF PARAMETERS() >= 5 .AND. ran_pg
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
       .AND. vpepage <= 32767
     READ
     RELEASE WINDOW _xyx
     IF LASTKEY() = 27
          IF  .NOT.  ;
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
IF  .NOT. WEXIST('MSG2USE')
     DEFINE WINDOW msg2use FROM  ;
            12, 02 TO 16, 77  ;
            FLOAT SHADOW DOUBLE
ENDIF
_dest = 'Pantalla '
IF  .NOT. _wp
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
            .NOT. EMPTY(p_fil)
          READ
     ENDIF
     p_fil = ALLTRIM(p_fil) +  ;
             '.LST'
     RELEASE WINDOW msg2use
ENDIF
IF MOD(READKEY(), 256) = 12
     ON KEY
     SET ESCAPE OFF
     ACTIVATE SCREEN
     RETURN
ENDIF
impre = (_dest = 'Impresora')
IF  .NOT. impre .AND. _dest =  ;
    'Pantalla '
     p_fil = SYS(3) + '.LST'
ENDIF
printing = .T.
IF impre
     IF  .NOT. EMPTY(LEFT(SYS(0),  ;
         18))
          IF  .NOT.  ;
              yesno( ;
              'IMPRIME EN IMPRESORA LOCAL ?  <NO = IMPRESORA DE RED>' ;
              )
               SET PRINTER TO \\ibm_pc\printq_0=lpt1
               SET PRINTER TO \\spooler\nb
          ENDIF
          IF  .NOT. ready2pr()
               printing = .F.
          ENDIF
     ENDIF
ENDIF
IF printing
     DEFINE WINDOW _repo FROM 0,  ;
            0 TO 24, 79 TITLE  ;
            _tit FOOTER  ;
            ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '  ;
            COLOR SCHEME 10
     ACTIVATE WINDOW _repo
     CLEAR
     IF impre
          DO &_prgrpt WITH 1
     ELSE
          @ 02, 20 SAY  ;
            '** Reporte en ejecuci¢n **'
          DO &_prgrpt WITH 2
          IF _wp
               ACTIVATE SCREEN
               RESTORE SCREEN  ;
                       FROM  ;
                       pantalla
               RUN FOXSWAP wp &p_fil
          ELSE
               MODIFY COMMAND  ;
                      (p_fil)  ;
                      NOEDIT  ;
                      WINDOW  ;
                      _repo
          ENDIF
          IF resul = 'OK'
               IF yesno( ;
                  '¨ Imprime el Reporte ?' ;
                  )
                    FOR v = 1 TO  ;
                        _copia
                         IF ready2pr()  ;
                            .AND.  ;
                            IIF(_copia >  ;
                            1,  ;
                            yesno( ;
                            'Copia ' +  ;
                            STR(v,  ;
                            2) +  ;
                            ' .¨Prepare el papel. Listo?' ;
                            ),  ;
                            .T.)
                              !TYPE &p_fil>prn
                         ENDIF
                    ENDFOR
               ENDIF
          ELSE
               resul = 'OK'
          ENDIF
          IF _dest = 'Impresora'  ;
             .OR. _dest =  ;
             'Pantalla '
               ERASE (p_fil)
          ENDIF
     ENDIF
     DEACTIVATE WINDOW _repo
ELSE
     DO standby WITH  ;
        'El reporte ha sido cancelado'
ENDIF
ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN
RETURN
*
PROCEDURE clrscr
PRIVATE fil, col
fil = 11
col = 39
DO WHILE (col>0) .AND. (fil>0)
     @ fil, col CLEAR TO 23 - fil,  ;
       79 - col
     fil = fil - 1
     col = col - 3
     = INKEY(0.001 , 'H')
ENDDO
CLEAR
RETURN
*
FUNCTION ready2pr
DO WHILE  .NOT. PRINTSTATUS()
     IF  .NOT. yesno( ;
         'La Impresora est  apagada.¨ Continua ?' ;
         )
          RETURN .F.
     ENDIF
ENDDO
RETURN .T.
*
PROCEDURE progreso
PARAMETER control, cmens, total,  ;
          actual
DO CASE
     CASE control = 1
          DEFINE WINDOW vview  ;
                 FROM 08, 11 TO  ;
                 14, 68 DOUBLE  ;
                 COLOR SCHEME 5
          ACTIVATE WINDOW vview
          @ 02, 00 TO 04, 57
          @ 00, 00 SAY cmens
     CASE control = 2
          vtotc = 54
          porca = ROUND((actual *  ;
                  100) / total,  ;
                  2)
          numc = ROUND((porca *  ;
                 54) / 100, 2)
          @ 03, 01 SAY  ;
            REPLICATE('Û', numc)
          @ 01, 52 SAY porca  ;
            PICTURE '999%'
     CASE control = 3
          RELEASE WINDOW vview
ENDCASE
RETURN
*
PROCEDURE ctrldb
PARAMETER nopcion
USE parmae ALIAS parma ORDER  ;
    Parmae1
DO CASE
          IF  .NOT.  ;
              SEEK('CTRLDB')
               DO standby WITH  ;
                  'Error de Parametro Avise al Area de Sistemas'
          ENDIF
     CASE nopcion = 1
          SEEK 'CTRLDB'
          IF codigo = '02'
               DO standby WITH  ;
                  'No salio Correctamente del sistema'
               DO standby WITH  ;
                  'OJO NO OLVIDAR INDEXA'
               DO indexa WITH 1
          ELSE
               IF SEEK('CTRLDB')
                    REPLACE codigo  ;
                            WITH  ;
                            '02'
               ENDIF
          ENDIF
     CASE nopcion = 2
          IF SEEK('CTRLDB')
               REPLACE codigo  ;
                       WITH '01'
          ENDIF
ENDCASE
RETURN
*
FUNCTION ve_passw
PARAMETER _psw
PRIVATE _ln, xxx
_ln = LEN(_psw)
xxx = SPACE(_ln)
DEFINE WINDOW _pasw FROM 12, 15  ;
       TO 17, 65 TITLE  ;
       ' Password de seguridad '  ;
       COLOR SCHEME 5
ACTIVATE WINDOW _pasw
CLEAR
@ 1, 4 SAY  ;
  'Opci¢n restringida, ingrese password: '
@ 2, 20 SAY '[' + SPACE(_ln) +  ;
  ']'
@ 2, 21 GET xxx PICTURE '@!'  ;
  COLOR ,N 
READ
RELEASE WINDOW _pasw
IF LASTKEY() <> 27 .AND. xxx =  ;
   _psw
     RETURN .T.
ELSE
     DO standby WITH  ;
        'Password incorrecto. Acceso denegado !'
ENDIF
RETURN .F.
*
PROCEDURE nada
RETURN
*
FUNCTION actkarcre
*-----------------
PARAMETER m.tipmov, m.codmov, m.fecha
PRIVATE malias
PUBLIC nsalfin
malias = ALIAS()
SELECT karcre
SET ORDER TO KarCre2
mret = .F.
lcancel = .F.
IF  .NOT. SEEK(m.tipmov + m.codmov)
     IF f_appd()
          m.correl = 'A'
          GATHER MEMVAR
          REPLACE estado WITH '00'
     ENDIF
     SET ORDER TO KarCre1
     SEEK m.codcre
     IF FOUND()
          m.correl = '00'
          SCAN WHILE correl <> 'A'
               m.correl = correl
          ENDSCAN
          m.correl = PADL(ALLTRIM(STR(VAL(m.correl) + 1)), 2, '0')
          REPLACE correl WITH m.correl
     ENDIF
     SEEK m.codcre
     IF FOUND()
          m.totint = 0
          m.totaju = 0
          m.totamo = 0
          SCAN WHILE codcre = m.codcre
             m.totint = m.totint + mtoint
             m.totamo = m.totamo + mtoamo
             m.totaju = m.totaju + mtoaju
             m.saldo = (m.mtocre + m.totint + m.totaju) - m.totamo
             IF saldo <> m.saldo .OR. totint <> m.totint .OR. totamo <> m.totamo .OR. totaju <> m.totaju
                REPLACE saldo WITH m.saldo, ;
                       totint WITH m.totint, ;
                       totaju WITH m.totaju, ;
                       totamo WITH m.totamo
               ENDIF
          ENDSCAN
     ENDIF
     mret = .T.
ELSE
     SET ORDER TO KarCre1
     SEEK m.codcre
     IF FOUND()
          m.totint = 0
          m.totaju = 0
          m.totamo = 0
          SCAN WHILE codcre = m.codcre
            m.totint = m.totint + mtoint
            m.totaju = m.totaju + mtoaju
            m.totamo = m.totamo + mtoamo
            m.saldo = (m.mtocre + m.totint + m.totaju) - m.totamo
            IF saldo <> m.saldo .OR. totint <> m.totint .OR. totamo <> m.totamo .OR. totaju <> m.totaju
               REPLACE saldo  WITH m.saldo, ;
                       totint WITH m.totint, ;
                       totaju WITH m.totaju, ;
                       totamo WITH m.totamo
            ENDIF
          ENDSCAN
     ENDIF
     mret = .T.
ENDIF
IF mret
     SELECT creditos
     IF SEEK(m.codcre)
          REPLACE estado  WITH IIF(m.saldo=0.0,'10','20'),;
                  totamo  WITH m.totamo, ;
                  totint  WITH m.totint, ;
                  totaju  WITH m.totaju, ;
                  saldo   WITH m.saldo,  ;
                  fultpag WITH m.fultpag
     ELSE
          IF f_appd()
               GATHER MEMVAR
               REPLACE estado WITH IIF(m.saldo=0.0,'10','20'),  ;
                       saldo  WITH m.saldo
          ENDIF
     ENDIF
ENDIF
nsalfin = m.saldo
SELECT (malias)
RETURN mret
*
PROCEDURE recalcula
*------------------
PARAMETER ccodcre
PRIVATE cord
cord = ORDER()
SET ORDER TO KarCre1
SEEK ccodcre
m.totint = 0
m.totaju = 0
m.totamo = 0
IF FOUND()
   SCAN WHILE codcre = ccodcre
      m.totint = m.totint + mtoint
      m.totaju = m.totaju + mtoaju
      m.totamo = m.totamo + mtoamo
   ENDSCAN
   SKIP -1
   m.saldo = (m.mtocre + m.totint + m.totaju) - m.totamo
   REPLACE saldo WITH m.saldo, ;
           totint WITH m.totint, ;
           totaju WITH m.totaju, ;
           totamo WITH m.totamo
   SELECT creditos
   IF SEEK(ccodcre)
      REPLACE saldo  WITH m.saldo,;
              totint WITH m.totint,  ;
              totaju WITH m.totaju,  ;
              totamo WITH m.totamo
   ENDIF
ELSE
ENDIF
SELECT karcre
SET ORDER TO (cord)
RETURN
*
FUNCTION val_cli
*---------------
PARAMETER xcod, _fil, _col
PRIVATE cord, mval
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT clien
GOTO TOP
cord = ORDER()
SET ORDER TO Clientes1
IF  .NOT. EMPTY(xcod)
     SEEK xcod
     v_ent = FOUND()
     v_fun = .T.
ENDIF
IF  .NOT. v_ent
     SET ORDER TO Clientes2
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO FunBusDet
     ON KEY LABEL F5 DO Agr_Cli
     DEFINE WINDOW _buscli FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _buscli
     BROWSE FIELDS nomcli :H =  ;
            'Nombre' : 40, razsoc  ;
            :H = 'Razon Social' :  ;
            40 NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _buscli TITLE  ;
            '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega Cliente ²²²²'  ;
            NOLGRID
     ON KEY LABEL F10
     ON KEY LABEL F2
     ON KEY LABEL F5
     DEACTIVATE WINDOW _buscli
     RELEASE WINDOW _buscli
     IF LASTKEY() = 27
          v_fun = .F.
          v_ent = .F.
     ELSE
          v_ent = .T.
     ENDIF
ENDIF
IF v_ent
     xcod = codcli
     m.nomcli = nomcli
     m.dnicli = dnicli
     SHOW GETS
     v_fun = .T.
     IF  .NOT. EMPTY(_fil) .OR.   ;
         .NOT. EMPTY(_col)
          @ _fil, _col SAY  ;
            m.nomcli
     ENDIF
ENDIF
SET ORDER TO &cOrd
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
UNLOCK ALL
SELECT (malias)
RETURN v_fun
*
PROCEDURE funbusdet
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF( .NOT. EOF(), RECNO(), - ;
        1)
DEFINE POPUP plista FROM 15, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Codigo      '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Nombre      '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
RELEASE POPUP plista
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          vbusca = SPACE(6)
          vnombre = 'Codigo :'
          SET ORDER TO Clientes1
     CASE BAR() = 2
          vbusca = SPACE(30)
          vnombre = 'Nombre : '
          SET ORDER TO Clientes2
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
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
     READ VALID val_read()
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Cliente no Existe'
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
PROCEDURE agr_cli
PRIVATE malias, m.codcli,  ;
        m.nomcli, m.dnicli,  ;
        m.razsoc, m.ruccli,  ;
        m.dircli
ON KEY LABEL F2
ON KEY LABEL F5
malias = ALIAS()
SELECT clien
DEFINE WINDOW w_cli FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Cliente'  ;
       DOUBLE COLOR SCHEME 5
m.codcli = SPACE(6)
m.nomcli = SPACE(70)
m.dircli = SPACE(70)
m.dnicli = SPACE(8)
m.razsoc = SPACE(50)
m.ruccli = SPACE(11)
m.tipcli = 'C'
= cor_clien()
ACTIVATE WINDOW w_cli
@ 01, 1 SAY '       Codigo: ' +  m.codcli
@ 02, 1 SAY '       Nombre:' GET m.nomcli FUNCTION '!S30'
@ 03, 1 SAY '    Direccion:' GET m.dircli FUNCTION '!S30'
@ 04, 1 SAY '       D.N.I.:' GET m.dnicli PICTURE '99999999'
@ 05, 1 SAY ' Razon Social:' GET m.razsoc FUNCTION '!S30'
@ 06, 1 SAY '         RUC.:' GET m.ruccli PICTURE '99999999999'
READ VALID val_read()
DEACTIVATE WINDOW w_cli
RELEASE WINDOW w_cli
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
          SELECT parma
          SEEK 'CORRELCLIENT'
          REPLACE nument WITH  ;
                  parma.nument +  ;
                  1
     ENDIF
ENDIF
SELECT (malias)
ON KEY LABEL F5 DO Agr_Cli
ON KEY LABEL F2 DO FunBusDet
RETURN
*
FUNCTION valestcli
PRIVATE cali, mret, cord, lborra
cali = ALIAS()
ldeuda = .F.
IF  .NOT. USED('Creditos')
     USE IN 0 Creditos ORDER  ;
         Creditos2
     lborra = .T.
ELSE
     SELECT creditos
     cord = ORDER()
     SET ORDER TO Creditos2
     lborra = .F.
ENDIF
mret = .T.
SELECT creditos
IF  .NOT. ldeuda
     IF SEEK(m.codcli)
          SCAN WHILE codcli =  ;
               m.codcli
               IF creditos.saldo >  ;
                  0 .AND.  ;
                  creditos.estado <>  ;
                  '99'
                    ldeuda = .T.
               ENDIF
          ENDSCAN
     ENDIF
ENDIF
IF  .NOT. ldeuda
     SET ORDER TO Creditos3
     IF SEEK(m.codcli)
          SCAN WHILE codavl =  ;
               m.codcli
               IF creditos.saldo >  ;
                  0 .AND.  ;
                  creditos.estado <>  ;
                  '99'
                    ldeuda = .T.
               ENDIF
          ENDSCAN
     ENDIF
ENDIF
IF ldeuda
     IF yesno( ;
        'El Cliente Tiene Deuda Pendiente. desea Continuar' ;
        )
     ELSE
          m.codcli = SPACE(6)
          mret = .F.
     ENDIF
ENDIF
IF lborra
     USE IN creditos
ELSE
     SET ORDER TO (cord)
ENDIF
SELE &cAli
RETURN mret
*
FUNCTION valestavl
PRIVATE cali, mret, cord, lborra
cali = ALIAS()
ldeuda = .F.
IF  .NOT. USED('Creditos')
     USE IN 0 Creditos ORDER  ;
         Creditos3
     lborra = .T.
ELSE
     SELECT creditos
     cord = ORDER()
     SET ORDER TO Creditos3
     lborra = .F.
ENDIF
mret = .T.
SELECT creditos
IF  .NOT. ldeuda
     IF SEEK(m.codavl)
          SCAN WHILE codavl =  ;
               m.codavl
               IF creditos.saldo >  ;
                  0 .AND.  ;
                  creditos.estado <>  ;
                  '99'
                    ldeuda = .T.
               ENDIF
          ENDSCAN
     ENDIF
ENDIF
IF  .NOT. ldeuda
     SET ORDER TO Creditos2
     IF SEEK(m.codavl)
          SCAN WHILE codcli =  ;
               m.codavl
               IF creditos.saldo >  ;
                  0 .AND.  ;
                  creditos.estado <>  ;
                  '99'
                    ldeuda = .T.
               ENDIF
          ENDSCAN
     ENDIF
ENDIF
IF ldeuda
     IF yesno( ;
        'El Aval Tiene Deuda Pendiente. desea Continuar' ;
        )
     ELSE
          m.codavl = SPACE(6)
          mret = .F.
     ENDIF
ENDIF
IF lborra
     USE IN creditos
ELSE
     SET ORDER TO (cord)
ENDIF
SELE &cAli
RETURN mret
*
FUNCTION val_avl
*---------------
PARAMETER xcod, _fil, _col
PRIVATE cord, mval
malias = ALIAS()
v_fun = .F.
v_ent = .F.
SELECT clien
GOTO TOP
cord = ORDER()
SET ORDER TO Clientes1
IF  .NOT. EMPTY(xcod)
     SEEK xcod
     v_ent = FOUND()
     v_fun = .T.
ENDIF
IF  .NOT. v_ent
     SET ORDER TO Clientes2
     GOTO TOP
     ACTIVATE SCREEN
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO FunBusAvl
     ON KEY LABEL F5 DO Agr_Avl
     DEFINE WINDOW _buscli FROM 2,  ;
            01 TO 22, 78
     ACTIVATE WINDOW _buscli
     BROWSE FIELDS nomcli :H =  ;
            'Nombre' : 40, dircli  ;
            :H = 'Direcci¢n' : 40  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _buscli TITLE  ;
            '²²²² [F10] Selecciona   [F2] Buscar ²²²²   [F5] Agrega Cliente ²²²²'  ;
            NOLGRID
     ON KEY LABEL F10
     ON KEY LABEL F2
     ON KEY LABEL F5
     DEACTIVATE WINDOW _buscli
     RELEASE WINDOW _buscli
     IF LASTKEY() = 27
          v_fun = .F.
          v_ent = .F.
     ELSE
          v_ent = .T.
     ENDIF
ENDIF
IF v_ent
     xcod = codcli
     m.nomavl = nomcli
     m.dniavl = dnicli
     SHOW GETS
     v_fun = .T.
     IF  .NOT. EMPTY(_fil) .OR.   ;
         .NOT. EMPTY(_col)
          @ _fil, _col SAY  ;
            m.nomavl
     ENDIF
ENDIF
SET ORDER TO &cOrd
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
UNLOCK ALL
SELECT (malias)
RETURN v_fun
*
PROCEDURE funbusavl
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF( .NOT. EOF(), RECNO(), - ;
        1)
DEFINE POPUP plista FROM 15, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Codigo      '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Nombre      '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
RELEASE POPUP plista
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          vbusca = SPACE(6)
          vnombre = 'Codigo :'
          SET ORDER TO Clientes1
     CASE BAR() = 2
          vbusca = SPACE(30)
          vnombre = 'Nombre : '
          SET ORDER TO Clientes2
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
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
     READ VALID val_read()
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Aval no Existe'
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
FUNCTION agr_avl
*---------------
PRIVATE malias, m.codavl,  ;
        m.nomavl, m.dniavl,  ;
        m.razsoc, m.rucavl,  ;
        m.centrb
ON KEY LABEL F2
ON KEY LABEL F5
malias = ALIAS()
SELECT clien
DEFINE WINDOW w_cli FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Aval' DOUBLE  ;
       COLOR SCHEME 5
m.codavl = SPACE(6)
m.nomavl = SPACE(70)
m.dniavl = SPACE(8)
m.diravl = SPACE(70)
m.centrb = SPACE(50)
m.tipavl = 'A'
= cor_avl()
ACTIVATE WINDOW w_cli
@ 01, 1 SAY '       Codigo: ' +  ;
  m.codavl
@ 02, 1 SAY '  Nombre Aval:' GET  ;
  m.nomavl FUNCTION '!S30'
@ 03, 1 SAY '    Direccion:' GET  ;
  m.diravl FUNCTION '!S30'
@ 04, 1 SAY '       D.N.I.:' GET  ;
  m.dniavl PICTURE '99999999'
@ 05, 1 SAY ' Ctro.Trabajo:' GET  ;
  m.centrb FUNCTION '!S30'
READ VALID val_read()
DEACTIVATE WINDOW w_cli
RELEASE WINDOW w_cli
IF LASTKEY() <> 27
     IF f_appd()
          REPLACE codcli WITH  ;
                  m.codavl,  ;
                  nomcli WITH  ;
                  m.nomavl,  ;
                  dircli WITH  ;
                  m.diravl,  ;
                  dnicli WITH  ;
                  m.dniavl,  ;
                  centrb WITH  ;
                  m.centrb,  ;
                  estado WITH  ;
                  '00', tipcli  ;
                  WITH m.tipavl
          SELECT parma
          SEEK 'CORRELCLIENT'
          REPLACE nument WITH  ;
                  parma.nument +  ;
                  1
     ENDIF
ENDIF
SELECT (malias)
ON KEY LABEL F5 DO Agr_Avl
ON KEY LABEL F2 DO FunBusAvl
RETURN
PRIVATE cali, mret
cali = ALIAS()
mret = .T.
IF SEEK(m.codcli, 'Clien')
     SELECT clien
     IF estado = '10'
          IF yesno( ;
             'El Cliente Tiene Deuda Pendiente. desea Continuar' ;
             )
               mret = .T.
          ELSE
               m.codcli = SPACE(6)
               mret = .F.
          ENDIF
     ENDIF
ENDIF
RETURN mret
*
FUNCTION cor_avl
IF SEEK('CORRELCLIENT' + m.codavl,  ;
   'Parma')
     m.codavl = PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 6, '0')
ELSE
     DO standby WITH  ;
        'Parametro de correlativo no existe; favor avisar a sistemas'
ENDIF
IF EMPTY(m.codavl)
     DO standby WITH  ;
        'El Codigo esta vacio'
     RETURN .F.
ELSE
     nreg = RECNO()
     IF SEEK(m.codavl)
          DO standby WITH  ;
             'Ya esta Registrado este Aval'
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION cor_clien
IF SEEK('CORRELCLIENT' + m.codcli,  ;
   'Parma')
     m.codcli = PADL(ALLTRIM(STR(parma.nument +  ;
                1)), 6, '0')
ELSE
     DO standby WITH  ;
        'Parametro de correlativo no existe; favor avisar a sistemas'
ENDIF
IF EMPTY(m.codcli)
     DO standby WITH  ;
        'El Codigo esta vacio'
     RETURN .F.
ELSE
     nreg = RECNO()
     IF SEEK(m.codcli)
          DO standby WITH  ;
             'Ya esta Registrado este Cliente'
          RETURN .F.
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION valcre
*--------------
PRIVATE mret, calias, cord1, cord2
mret = .T.
calias = ALIAS()
vidx1 = SYS(3) + '.Idx'
IF TYPE('m.TipVta') = 'U'
     m.tipvta = ''
ENDIF
SELECT creditos.*, clien.nomcli  ;
  FROM Creditos, Clien;
 WHERE creditos.codcli = clien.codcli AND creditos.tipvta = m.tipvta  ;
       AND creditos.estado <> '99' AND saldo > 0;
 ORDER BY nomcli ;
  INTO CURSOR tmpCre
  
SELECT tmpcre
IF  .NOT. EOF()
  vtempo = '°°°°°°°°°°° ®F2¯ Buscar   ®F10¯ Seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
  DEFINE WINDOW lista FROM 00, 00 TO 24, 79 FLOAT TITLE vtempo DOUBLE  ;
            COLOR SCHEME 2
  ACTIVATE WINDOW lista
  ON KEY LABEL F10 KEYBOARD CHR(23)
  ON KEY LABEL F2 DO Busdetcr
  BROWSE FIELDS nomcli :H = 'Cliente' : 30,  ;
                nomben :H = 'Difunto' : 30, ;
                feccre :H = 'F.Cred.',;
                mtocre :H = 'Credito',;
                totint :H = 'Intereses',;
                totaju :H = 'Ajustes  ',;
                totamo :H = 'Amort.', ;
                saldo  :H = 'Saldo' ;
         NOMENU NOAPPEND NOEDIT NODELETE WINDOW  ;
            lista
     ON KEY LABEL F10
     ON KEY LABEL F2
     SET RELATION TO
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
     IF LASTKEY() <> 27
          m.codcre = codcre
          m.feccre = feccre
          m.codvta = codvta
          m.codcli = codcli
          m.codavl = codavl
          m.mtocre = mtocre
          m.saldoaf = saldo
          xfultpag = fultpag
     ELSE
          mret = .F.
     ENDIF
ELSE
     DO standby WITH 'No Existen Difuntos Pendientes de Pago'
     mret = .F.
ENDIF
USE IN tmpcre
ERASE (vidx1)
SELECT (calias)
SHOW GETS
RETURN mret
*
PROCEDURE busdetcr
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF( .NOT. EOF(), RECNO(), - ;
        1)
DEFINE POPUP plista FROM 15, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Cliente     '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Difunto     '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
RELEASE POPUP plista
cord1 = ORDER()
vbusca = SPACE(30)
DO CASE
     CASE BAR() = 1
          vnombre = 'Cliente :'
          INDEX ON UPPER(nomcli)  ;
                TO (vidx1)
     CASE BAR() = 2
          vnombre = 'Difunto : '
          INDEX ON UPPER(nomben)  ;
                TO (vidx1)
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 13,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            10
     ACTIVATE WINDOW lista
     @ 1, 2 SAY vnombre GET  ;
       vbusca FUNCTION '!'
     READ VALID val_read()
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Cliente no Existe'
          IF vtemp = -1
               GOTO BOTTOM
          ELSE
               GOTO vtemp
          ENDIF
     ELSE
     ENDIF
ENDIF
RETURN
*
FUNCTION letras
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
               x1 = SUBSTR(n, y,  ;
                    1)
               IF VAL(x1) <> 0
                    DO CASE
                         CASE y =  ;
                              1
                              IF VAL(n) =  ;
                                 100
                                   wletras = wletras + 'CIEN '
                              ELSE
                                   wletras = wletras + letras(4,VAL(x1))
                              ENDIF
                         CASE y =  ;
                              2
                              IF VAL(SUBSTR(n,  ;
                                 y,  ;
                                 2)) >  ;
                                 10  ;
                                 .AND.  ;
                                 VAL(SUBSTR(n,  ;
                                 y,  ;
                                 2)) <  ;
                                 20
                                   wletras = wletras + letras(2,VAL(SUBSTR(n, 3, 1)))
                              ELSE
                                   wletras = wletras + letras(3,VAL(x1))
                              ENDIF
                         CASE y =  ;
                              3
                              IF   ;
                               .NOT.  ;
                               SUBSTR(n,  ;
                               2,  ;
                               1) $  ;
                               '1'
                                   IF  .NOT. SUBSTR(n, 2, 1) $ '0' .AND.  .NOT. EMPTY(wletras)
                                        wletras = wletras + ' Y '
                                   ENDIF
                                   wletras = wletras + letras(1,VAL(x1))
                              ENDIF
                    ENDCASE
               ENDIF
               y = y + 1
          ENDDO
          DO CASE
               CASE i = 1 .OR. i =  ;
                    7 .OR. i =  ;
                    13
                    wletras = wletras +  ;
                              'MIL '
               CASE i = 4
                    wletras = wletras +  ;
                              'BILLON '
               CASE i = 10
                    IF x = 1
                         wletras =  ;
                          wletras +  ;
                          'MILLON '
                    ELSE
                         wletras =  ;
                          wletras +  ;
                          'MILLONES '
                    ENDIF
          ENDCASE
     ENDIF
     i = i + 3
ENDDO
wletras = ALLTRIM(wletras) +  ;
          ' Y  ' +  ;
          PADL(ALLTRIM(STR(centavos *  ;
          100, 2)), 2, '0') +  ;
          '/100 ' + simbolo
ll = SUBSTR(wletras, 85, 15)
l = 0
t = 16
r = LEN(wletras)
IF r > 100
     DO WHILE t<>20
          t = t - 1
          p = SUBSTR(ll, t, 1)
          IF p = ' '
               numlet1 = SUBSTR(wletras,  ;
                         1, 85) +  ;
                         SUBSTR(ll,  ;
                         1, t)
               numlet2 = SUBSTR(wletras,  ;
                         (86 + t),  ;
                         (r - (85 +  ;
                         t)))
               t = 20
          ENDIF
     ENDDO
ELSE
     numlet1 = wletras
ENDIF
RETURN numlet1
*
FUNCTION selper
SAVE SCREEN TO pantalla
DEFINE WINDOW wlista FROM 03, 18  ;
       TO 6, 60 TITLE  ;
       'Periodo a Trabajar'  ;
       DOUBLE COLOR SCHEME 05
ACTIVATE WINDOW wlista
m.fecsis = DATE()
@ 01, 01 SAY  ;
  'Fecha del Sistema :' GET  ;
  m.fecsis
READ
DEACTIVATE WINDOW wlista
RELEASE WINDOW wlista
IF LASTKEY() = 13
     mret = .T.
ELSE
     mret = .F.
ENDIF
RESTORE SCREEN FROM pantalla
RETURN mret
*
