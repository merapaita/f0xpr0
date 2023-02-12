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
                  'Registro del archivo '+ ;
                  ALIAS()+ ;
                  ' ocupado. Espere un momento por favor o presione <Esc> para cancelar'
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
                  'El registro del file'+ ;
                  ALIAS()+ ;
                  ' est  siendo utilizado. Se cancela la operaci¢n'
               ?? beep
          ENDIF
     CASE btipo = 3
          v_fun = .F.
          IF FLOCK()
               v_fun = .T.
          ELSE
               DO standby WITH  ;
                  ALIAS()+ ;
                  'Archivo ocupado.  Espere un instante por favor o presione <Esc> para cancelar'
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
                  ALIAS()+ ;
                  ' Archivo ocupado. El proceso se cancela'
               ?? beep
          ENDIF
ENDCASE
RETURN v_fun
*
FUNCTION sigue
PARAMETER xx
SKIP 1
RETURN .T.
*
FUNCTION f_appd
APPEND BLANK
v_fun = RLOCK() .OR. f_lock(1)
RETURN v_fun
*
PROCEDURE fox_lock
PARAMETER btipo, bloquea
beep = CHR(7)
DO CASE
     CASE btipo = 1
          insiste = .T.
          bloquea = .F.
          DO WHILE insiste
               IF RLOCK()
                    bloquea = .T.
                    insiste = .F.
               ELSE
                    DO standby  ;
                       WITH  ;
                       'Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar.'
                    ?? beep
                    ktecla = 0
                    DO WHILE  ;
                       ktecla<>27  ;
                       .AND. (  ;
                       .NOT.  ;
                       RLOCK())
                         ktecla =  ;
                          INKEY()
                    ENDDO
                    IF ktecla <>  ;
                       27
                         bloquea =  ;
                          .T.
                    ENDIF
                    insiste = .F.
               ENDIF
          ENDDO
     CASE btipo = 2
          bloquea = .F.
          IF RLOCK()
               bloquea = .T.
          ELSE
               DO standby WITH  ;
                  'El registro esta en uso. Presione <Esc> para cancelar. '
               ?? beep
               ktecla = 0
               DO WHILE ktecla<> ;
                  27
                    ktecla = INKEY()
               ENDDO
          ENDIF
     CASE btipo = 3
          insiste = .T.
          bloquea = .F.
          DO WHILE insiste
               IF FLOCK()
                    bloquea = .T.
                    insiste = .F.
               ELSE
                    DO standby  ;
                       WITH  ;
                       'Archivo ocupado.  Espere un instante por favor o'+ ;
                       ' presiona <Esc> para cancelar.'
                    ?? beep
                    ktecla = 0
                    DO WHILE  ;
                       ktecla<>27  ;
                       .AND. (  ;
                       .NOT.  ;
                       FLOCK())
                         ktecla =  ;
                          INKEY()
                    ENDDO
                    IF ktecla <>  ;
                       27
                         bloquea =  ;
                          .T.
                    ENDIF
                    insiste = .F.
               ENDIF
          ENDDO
     CASE btipo = 4
          bloquea = .F.
          IF FLOCK()
               bloquea = .T.
          ELSE
               DO standby WITH  ;
                  'Archivo ocupado.  Presione <Esc> para cancelar.'
               ?? beep
               ktecla = 0
               DO WHILE ktecla<> ;
                  27
                    ktecla = INKEY()
               ENDDO
          ENDIF
ENDCASE
RETURN
*
PROCEDURE fox_appd
PARAMETER lappd
lappd = .F.
bloquea = .F.
gagrega = .T.
APPEND BLANK
DO fox_lock WITH 1, bloquea
IF bloquea
     lappd = .T.
ENDIF
gagrega = .F.
RETURN
*
PROCEDURE fox_errs
PARAMETER prg_error
DO CASE
     CASE ERROR() = 108
          DO standby WITH  ;
             'Un momento por favor el archivo est  en uso'
          RETRY
     CASE ERROR() = 109
          WAIT WINDOW NOWAIT  ;
               'Registro est  siendo modificado por otro usuario.'
          RETURN
     CASE ERROR() = 130
          KEYBOARD CHR(13)
          RETURN
     CASE ERROR() = 125
          IF yesno( ;
             'La Impresora no est  lista. ¨Se reintenta impresi¢n?' ;
             )
               SET DEVICE TO PRINTER
               RETRY
          ELSE
               SET DEVICE TO SCREEN
               SET PRINTER TO
               RETURN
          ENDIF
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
          CREATE VIEW CORE.VIW
          SAVE TO 'core.fox'
          DO poperror WITH  ;
             'El sistema acaba de detectar un error interno. Avise al  rea de Sistemas.'+ ;
             CHR(13)+CHR(13)+ ;
             'Tome nota de la siguiente descripci¢n:'+ ;
             CHR(13)+'Programa: '+ ;
             ALLTRIM(prg_error)+ ;
             CHR(13)+'L¡nea   : '+ ;
             ALLTRIM(lin_error)+ ;
             CHR(13)+'Error   : '+ ;
             ALLTRIM(des_error)
          IF worker
               ON ERROR
               CANCEL
          ENDIF
ENDCASE
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
          ' Y  ' + STR(centavos *  ;
          100, 2) + '/100 ' +  ;
          simbolo
ll = SUBSTR(wletras, 51, 15)
l = 0
t = 16
r = LEN(wletras)
IF r > 65
     DO WHILE t<>20
          t = t - 1
          p = SUBSTR(ll, t, 1)
          IF p = ' '
               numlet1 = SUBSTR(wletras,  ;
                         1, 50) +  ;
                         SUBSTR(ll,  ;
                         1, t)
               numlet2 = SUBSTR(wletras,  ;
                         (51 + t),  ;
                         (r - (50 +  ;
                         t)))
               t = 20
          ENDIF
     ENDDO
ELSE
     numlet1 = wletras
ENDIF
RETURN numlet1
*
FUNCTION pticker
STORE SET('CONSOLE') TO setcon
STORE SET('PRINTER') TO setprn
SET CONSOLE ON
SET PRINTER OFF
@ 00, 00 SAY SUBSTR(ticker_str,  ;
  seg, 77)
IF setcon = 'OFF'
     SET CONSOLE OFF
ENDIF
IF setprn = 'ON'
     SET PRINTER ON
ENDIF
STORE IIF(seg = 11, 1, seg + 1)  ;
      TO seg
RETURN ''
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
PROCEDURE standby
PARAMETER msgwords, posi, posc
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
ACTIVATE WINDOW IN screen standby
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
FUNCTION yesno
PARAMETER msgwords, posi, posc
IF  .NOT. WEXIST('yesno')
     DEFINE WINDOW yesno FROM 19,  ;
            27 TO 23, 77 GROW  ;
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
     MOVE WINDOW yesno TO posi,  ;
          colp
ENDIF
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
FUNCTION wintitle
PARAMETER ctitle
STORE WCOLS() TO clen
STORE INT((clen - LEN(ctitle)) /  ;
      2) TO padding
STORE SPACE(padding) + ctitle +  ;
      SPACE((clen - LEN(ctitle)) -  ;
      padding) TO padtitle
RETURN padtitle
*
PROCEDURE msg2user
PARAMETER message1, message2,  ;
          message3
IF message1 = 'CLOSE'
     DEACTIVATE WINDOW msg2user
ELSE
     ACTIVATE WINDOW msg2user
     CLEAR
     @ 00, 00 SAY SPACE(1) +  ;
       LEFT(message1 + SPACE(71),  ;
       71) + SPACE(1)
     @ 01, 00 SAY SPACE(1) +  ;
       LEFT(IIF(PARAMETERS() < 2,  ;
       SPACE(71), message2) +  ;
       SPACE(71), 71) + SPACE(1)
     @ 02, 00 SAY SPACE(1) +  ;
       LEFT(IIF(PARAMETERS() < 3,  ;
       SPACE(71), message3) +  ;
       SPACE(71), 71) + SPACE(1)
ENDIF
RETURN
*
FUNCTION parametro
PARAMETER vtipo, vkey
PRIVATE malias
malias = ALIAS()
SELECT parma
SEEK vtipo + vkey
IF  .NOT. FOUND()
     vnombre = ' '
ELSE
     vnombre = descri
ENDIF
SELECT (malias)
RETURN (vnombre)
*
FUNCTION valtod
PARAMETER _toda, _x, _y
PRIVATE _toda, _x, _y
IF _toda = 1
     @ _x, _y SAY SPACE(40)
ENDIF
RETURN .T.
*
FUNCTION fecha
PARAMETER vfecha
IF EMPTY(vfecha)
     vfecha = DATE()
ENDIF
vnumero = MONTH(vfecha)
DO CASE
     CASE vnumero = 1
          vmes = 'Enero'
     CASE vnumero = 2
          vmes = 'Febrero'
     CASE vnumero = 3
          vmes = 'Marzo'
     CASE vnumero = 4
          vmes = 'Abril'
     CASE vnumero = 5
          vmes = 'Mayo'
     CASE vnumero = 6
          vmes = 'Junio'
     CASE vnumero = 7
          vmes = 'Julio'
     CASE vnumero = 8
          vmes = 'Agosto'
     CASE vnumero = 9
          vmes = 'Setiembre'
     CASE vnumero = 10
          vmes = 'Octubre'
     CASE vnumero = 11
          vmes = 'Noviembre'
     CASE vnumero = 12
          vmes = 'Diciembre'
ENDCASE
vdia = STR(DAY(vfecha), 2)
vano = TRANSFORM(YEAR(vfecha),  ;
       '9,999')
vffecha = 'Piura, ' + vdia +  ;
          ' de ' + vmes + ' de ' +  ;
          vano
RETURN (vffecha)
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
       i, i) COLOR SCHEME  ;
       c_fondo
     @ 24, i SAY RIGHT(rotulo2, 2 *  ;
       (40 - i)) COLOR SCHEME  ;
       c_borde
     FOR y = 0 TO tiempo
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE err_input
ACTIVATE WINDOW err_input
@ 00, 01 SAY 'DATO NO VALIDO'
aa = INKEY(1, 'H')
DEACTIVATE WINDOW err_input
RETURN
*
PROCEDURE repprg
PARAMETER _prgrpt, _tit, _copia,  ;
          _wp
PRIVATE resul
resul = 'OK'
IF PARAMETERS() = 2
     _copia = 1
ENDIF
IF PARAMETERS() = 3
     _wp = .F.
ENDIF
_conso = SET('CONSOLE')
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
IF  .NOT. WEXIST('MSG2USER')
     DEFINE WINDOW msg2user FROM  ;
            12, 02 TO 16, 77  ;
            FLOAT SHADOW DOUBLE  ;
            COLOR SCHEME 7
ENDIF
ACTIVATE WINDOW msg2user
_dest = 'IMPRESORA'
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
       'Nombre del Archivo :' GET  ;
       p_fil PICTURE 'NNNNNNNN'  ;
       VALID  .NOT. EMPTY(p_fil)
     READ
ENDIF
p_fil = ALLTRIM(p_fil) + '.LST'
DEACTIVATE WINDOW msg2user
IF MOD(READKEY(), 256) = 12
     ON KEY
     SET ESCAPE OFF
     ACTIVATE SCREEN
     RETURN
ENDIF
impre = (_dest = 'IMPRESORA')
IF  .NOT. impre
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
               SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
               SET PRINTER TO \\SPOOLER\NB
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
          DO &_PrgRpt WITH 1
     ELSE
          @ 02, 20 SAY  ;
            '** Reporte en ejecuci¢n **'
          DO &_PrgRpt WITH 2
          IF _wp
               ACTIVATE SCREEN
               RESTORE SCREEN  ;
                       FROM  ;
                       pantalla
               RUN FOXSWAP WP &P_FIL
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
                         IF ready2pr()
                              !TYPE &P_FIL>PRN
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
     DO standby WITH  ;
        'El reporte ha sido cancelado'
ENDIF
ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN
RETURN
*
PROCEDURE reporte
PARAMETER _tipo, _form, _tit,  ;
          num_c, _wp, ran_pg
PRIVATE _wndold
_wndold = WOUTPUT()
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
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
               ACTIVATE WINDOW &_WndOld
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
               ACTIVATE WINDOW &_WndOld
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
          ACTIVATE WINDOW &_WndOld
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
               SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
               SET PRINTER TO \\SPOOLER\NB
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
                    REPORT FORM &_Form;
ENVIRONMENT NOEJECT TO PRINT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_Form;
ENVIRONMENT NOEJECT TO PRI
               ENDIF
          ELSE
               IF PARAMETERS() >=  ;
                  6 .AND. ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_Form;
NOEJECT TO PRINT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_Form;
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
                         !TYPE &P_FIL;
>PRN
                    ENDIF
                    IF LASTKEY() =  ;
                       27
                         EXIT
                    ENDIF
               ENDFOR
          ELSE
               !FOXSWAP WP &p_fil
          ENDIF
          ERASE &p_fil
     ELSE
          IF _tipo = 1
               IF PARAMETERS() >=  ;
                  6 .AND. ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_Form;
ENVIRONMENT TO FILE (P_FIL) NOEJECT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_Form;
ENVIRONMENT TO FILE (P_FIL) NOEJECT
               ENDIF
          ELSE
               IF PARAMETERS() >=  ;
                  6 .AND. ran_pg
                    PRINTJOB
                    _PBPAGE = vpbpage
                    _PEPAGE = vpepage
                    REPORT FORM &_Form;
TO FILE (P_FIL) NOEJECT
                    ENDPRINTJOB
               ELSE
                    REPORT FORM &_Form;
TO FILE (P_FIL) NOEJECT
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
                                   !TYPE &P_FIL >PRN
                              ENDIF
                              IF LASTKEY() =  ;
                                 27
                                   EXIT
                              ENDIF
                         ENDFOR
                    ENDIF
               ENDIF
          ELSE
               !FOXSWAP WP &p_fil
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
     ACTIVATE WINDOW &_WndOld
ELSE
     ACTIVATE SCREEN
ENDIF
SET CONSOLE &_CONSO
RETURN
*
PROCEDURE xreporte
PARAMETER _tipo, _form, _tit,  ;
          _condicion, _wp
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
IF  .NOT. WEXIST('MSG2USER')
     DEFINE WINDOW msg2user FROM  ;
            12, 02 TO 16, 77  ;
            FLOAT SHADOW DOUBLE  ;
            COLOR SCHEME 7
ENDIF
ACTIVATE WINDOW msg2user
_dest = 'Impresora'
@ 01, 5 SAY  ;
  'Destino de impresi¢n : Pantalla/Impresora'  ;
  GET _dest PICTURE  ;
  '@M Impresora,Pantalla'
READ
DEACTIVATE WINDOW msg2user
IF MOD(READKEY(), 256) = 12
     ACTIVATE SCREEN
     RETURN
ENDIF
impre = (_dest = 'Impresora')
IF  .NOT. impre
     p_fil = SYS(3) + '.LST'
ENDIF
printing = .T.
IF impre
     IF  .NOT. EMPTY(LEFT(SYS(0),  ;
         18))
          IF yesno( ;
             '¨ Imprime en RED ?' ;
             )
               SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
               SET PRINTER TO \\SPOOLER\NB
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
          DO CASE
               CASE _tipo = 1
                    REPORT FORM &_Form;
ENVIRONMENT NOEJECT TO PRINT
               CASE _tipo = 2
                    REPORT FORM &_Form;
NOEJECT TO PRINT
               CASE _tipo = 3
                    REPORT FORM &_Form;
WHILE &_Condicion NOEJECT TO PRINT
          ENDCASE
          SET PRINTER TO
     ELSE
          DO CASE
               CASE _tipo = 1
                    REPORT FORM &_Form;
ENVIRONMENT TO FILE (P_FIL) NOEJECT
               CASE _tipo = 2
                    REPORT FORM &_Form;
TO FILE (P_FIL) NOEJECT
               CASE _tipo = 3
                    REPORT FORM &_Form;
WHILE &_Condicion TO FILE (P_FIL) NOEJECT
          ENDCASE
          MODIFY COMMAND (p_fil)  ;
                 NOEDIT WINDOW  ;
                 _repo
          IF  .NOT. _wp
               IF yesno( ;
                  '¨ Imprime el Reporte ?' ;
                  )
                    IF ready2pr()
                         !TYPE &P_FIL>PRN
                    ENDIF
               ENDIF
          ELSE
               !FOXSWAP WP &p_fil
          ENDIF
          ERASE (p_fil)
     ENDIF
     DEACTIVATE WINDOW _repo
     RELEASE WINDOW _repo
ELSE
     DO standby WITH  ;
        'El reporte ha sido cancelado'
ENDIF
ON ESCAPE
SET ESCAPE OFF
IF _tipo = 1
     CLOSE DATABASES
ENDIF
ACTIVATE SCREEN
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
            LEFT(codigo, 4) + ' ' +  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = ALLTRIM(parma.codigo)
mcuenta = ALLTRIM(parma.descriau2)
mcodaux = ALLTRIM(parma.codigoaux)
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            LEFT(mvalor, 3)
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'S'
          @ ROW(), mcol SAY  ;
            mcodaux
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN mcodaux
     CASE mvariable == 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable == 'D'
          RETURN LEFT(mvalor, 3) +  ;
                 SPACE(5) +  ;
                 mdescr
     CASE mvariable == 'Z'
          RETURN mdescr
     CASE mvariable == 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_para1
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
     CASE PARAMETERS() = 4
          mlong = 40
ENDCASE
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo + codigo = filtro
     GOTO TOP
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            ALLTRIM(codigoaux) +  ;
            ' ' + SUBSTR(descri,  ;
            1, 40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = parma.codigoaux
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
SET ORDER TO PARMAE1
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 6 SAY  ;
            mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN
     CASE mvariable = 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable = 'D'
          RETURN mdescr
     CASE mvariable = 'Z'
          RETURN mdescr
     CASE mvariable = 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_cale
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
     CASE PARAMETERS() = 4
          mlong = 40
ENDCASE
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo + codigo = filtro
     GOTO TOP
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            ALLTRIM(codigoaux) +  ;
            ' ' + SUBSTR(descri,  ;
            1, 40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = parma.codigoaux
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
SET ORDER TO 1
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 6 SAY  ;
            mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN
     CASE mvariable = 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable = 'D'
          RETURN mdescr
     CASE mvariable = 'Z'
          RETURN mdescr
     CASE mvariable = 'C'
          RETURN .T.
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_part
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
     CASE PARAMETERS() = 4
          mlong = 40
ENDCASE
malias = ALIAS()
SELECT itepar
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     IF  .NOT. EMPTY(filtro)
          SET FILTER TO periodo + uniges;
+ unieje + codcad + codfte = filtro
     ENDIF
     GOTO TOP
     IF EOF()
          SET FILTER TO
     ENDIF
     IF EOF()
          DO standby WITH  ;
             'No existen partidas definidas'
          SET FILTER TO
          SELECT (malias)
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            11, 40 PROMPT FIELDS  ;
            codpart + ' ' +  ;
            STR(valpart, 10, 2)  ;
            TITLE  ;
            'Partida Asignado'
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = itepar.codpart
mdescr = val_para(RIGHT(mvalor,  ;
         2),'ESPGAS','D')
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 7 SAY  ;
            mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN
     CASE mvariable = 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable = 'D'
          RETURN mdescr
     CASE mvariable = 'Z'
          RETURN mdescr
     CASE mvariable = 'C'
          RETURN .T.
     CASE mvariable = 'T'
          &mVariable = mValor
          @ ROW(), mcol + 7 SAY  ;
            mdescr
          RETURN mvalor
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_part1
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
     CASE PARAMETERS() = 4
          mlong = 40
ENDCASE
malias = ALIAS()
SELECT itepar
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     IF LEN(ALLTRIM(mvalor)) = 6
          v1 = val_para(LEFT(mvalor,  ;
               1),'CATGAS','C')
          v2 = val_para(SUBSTR(mvalor,  ;
               2, 1),'GRUGEN', ;
               'C')
          v3 = val_para(SUBSTR(mvalor,  ;
               3, 2),'MODAPL', ;
               'C')
          v4 = val_para(RIGHT(mvalor,  ;
               2),'ESPGAS','C')
          IF v1 .AND. v2 .AND. v3  ;
             .AND. v4
               IF yesno( ;
                  'Partida no Presupuestada .. ¨ Deseas Ingresarla ?' ;
                  )
               ELSE
                    SELECT (malias)
                    RETURN
               ENDIF
          ELSE
               DO standby WITH  ;
                  'Existe un error en la partida a asignar...verifique'
               SELECT (malias)
               RETURN
          ENDIF
          SELECT presu
          SEEK filtro
          IF  .NOT. FOUND()
               IF f_appd()
                    REPLACE periodo  ;
                            WITH  ;
                            LEFT(filtro,  ;
                            2),  ;
                            uniges  ;
                            WITH  ;
                            SUBSTR(filtro,  ;
                            3, 2),  ;
                            unieje  ;
                            WITH  ;
                            SUBSTR(filtro,  ;
                            5, 3),  ;
                            codcad  ;
                            WITH  ;
                            SUBSTR(filtro,  ;
                            8, 4),  ;
                            codfte  ;
                            WITH  ;
                            RIGHT(filtro,  ;
                            2),  ;
                            estado  ;
                            WITH  ;
                            '00',  ;
                            fecemi  ;
                            WITH  ;
                            m.fecha
               ENDIF
          ENDIF
          SELECT itepar
          IF f_appd()
               vestfun = maepre.uniges +  ;
                         maepre.unieje +  ;
                         maepre.codfun +  ;
                         maepre.codprg +  ;
                         maepre.codspr +  ;
                         maepre.actpry +  ;
                         maepre.codcom +  ;
                         maepre.codmet +  ;
                         maepre.codfin
               REPLACE periodo  ;
                       WITH  ;
                       LEFT(filtro,  ;
                       2), uniges  ;
                       WITH  ;
                       SUBSTR(filtro,  ;
                       3, 2),  ;
                       unieje  ;
                       WITH  ;
                       SUBSTR(filtro,  ;
                       5, 3),  ;
                       codcad  ;
                       WITH  ;
                       SUBSTR(filtro,  ;
                       8, 4),  ;
                       codfte  ;
                       WITH  ;
                       RIGHT(filtro,  ;
                       2), catgas  ;
                       WITH  ;
                       LEFT(mvalor,  ;
                       1), grugen  ;
                       WITH  ;
                       SUBSTR(mvalor,  ;
                       2, 1),  ;
                       modapl  ;
                       WITH  ;
                       SUBSTR(mvalor,  ;
                       3, 2),  ;
                       espgas  ;
                       WITH  ;
                       RIGHT(mvalor,  ;
                       2),  ;
                       codpart  ;
                       WITH  ;
                       mvalor,  ;
                       estfun  ;
                       WITH  ;
                       vestfun
          ENDIF
     ELSE
          IF  .NOT. EMPTY(filtro)
               SET FILTER TO periodo +;
uniges + unieje + codcad + codfte = filtro
          ENDIF
          GOTO TOP
          IF EOF()
               DEACTIVATE WINDOW  ;
                          standby
               DO standby WITH  ;
                  'No existen partidas definidas'
               SET FILTER TO
               SELECT (malias)
               RETURN
          ENDIF
          DEFINE POPUP parametro  ;
                 FROM 11, 40  ;
                 PROMPT FIELDS  ;
                 codpart + ' ' +  ;
                 STR((valpart +  ;
                 cresup + tra001 +  ;
                 tra003 + tra004 +  ;
                 tra005), 10, 2)  ;
                 TITLE  ;
                 'Partida Asignado'
          ON SELECTION POPUP parametro;
DEACTIVATE POPUP
          ACTIVATE POPUP  ;
                   parametro
          RELEASE POPUP parametro
          SET FILTER TO
     ENDIF
ENDIF
mvalor = itepar.codpart
mdescr = val_para(RIGHT(mvalor,  ;
         2),'ESPGAS','D')
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 7 SAY  ;
            mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN
     CASE mvariable = 'V'
          @ ROW(), COL() SAY  ;
            mvalor
          RETURN mdescr
     CASE mvariable = 'D'
          RETURN mdescr
     CASE mvariable = 'Z'
          RETURN mdescr
     CASE mvariable = 'C'
          RETURN .T.
     CASE mvariable = 'T'
          &mVariable = mValor
          @ ROW(), mcol + 7 SAY  ;
            mdescr
          RETURN mvalor
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_prv
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT promae
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), nompro,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 2
          ON KEY LABEL ENTER keyboard;
chr(23)
          ON KEY LABEL F2 do funbus
          DEFINE WINDOW _xx FROM  ;
                 3, 22 TO 22, 77
          BROWSE FIELDS codprv :H =  ;
                 'C¢digo', nompro  ;
                 :H = 'Nombre' :  ;
                 25, dirpro :H =  ;
                 'Direccci¢n' :  ;
                 25 NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' ®Enter¯  Selecciona     ®F2¯ Busca '  ;
                 NOLGRID
          ON KEY LABEL ENTER
          ON KEY LABEL f2
          RELEASE WINDOW _xx
          SET ORDER TO 1
          IF  .NOT.  ;
              EMPTY(_oldwnd)
               activate window &_oldwnd
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
               IF  .NOT. _tipo
                    replace &_campo with;
 xcod
               ENDIF
               v_fun = .T.
          ENDIF
     ELSE
          SEEK xcod
          IF mmsg .AND. FOUND()
               @ _x, _y SAY  ;
                 nompro
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
SELECT (malias)
RETURN v_fun
*
FUNCTION val_art
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT produ
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF EMPTY(xcod)
          SET ORDER TO 1
          SET FILTER TO SUBSTR(codart,;
2, 2) = SUBSTR(ALLTRIM(parma.codigo),;
2, 2)
          ACTIVATE WINDOW standby
          @ 1, 14 SAY  ;
            'Espere un Momento ....'  ;
            COLOR W/N* 
          GOTO TOP
          IF EOF()
               DEACTIVATE WINDOW  ;
                          standby
               ACTIVATE SCREEN
               SET FILTER TO
               v_fun = .F.
          ELSE
               DEACTIVATE WINDOW  ;
                          standby
               ACTIVATE SCREEN
               ON KEY LABEL F10 KEYBOARD;
CHR(23)
               ON KEY LABEL F8 DO BorArt
               ON KEY LABEL F5 DO AgrArt
               ON KEY LABEL F2 DO FunBus
               DEFINE WINDOW  ;
                      _busart  ;
                      FROM 2, 02  ;
                      TO 22, 77
               BROWSE FIELDS  ;
                      codart :H =  ;
                      'C¢digo' :W =  ;
                      EMPTY(SUBSTR(codart,  ;
                      5, 3)),  ;
                      descri :H =  ;
                      'Nombre' :  ;
                      60 :W =  ;
                      EMPTY(descri)  ;
                      NOMENU  ;
                      NOAPPEND  ;
                      NODELETE  ;
                      WINDOW  ;
                      _busart  ;
                      TITLE  ;
                      '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²'  ;
                      NOLGRID
               ON KEY LABEL F10
               ON KEY LABEL F8
               ON KEY LABEL F5
               ON KEY LABEL F2
               RELEASE WINDOW  ;
                       _busart
               SET ORDER TO 1
               SET FILTER TO
               IF  .NOT.  ;
                   EMPTY(_oldwnd)
                    ACTIVATE WINDOW &_OldWnd
               ENDIF
               IF LASTKEY() = 27
                    v_fun = .F.
               ELSE
                    xcod = codart
                    IF mmsg
                         @ _x, _y  ;
                           SAY  ;
                           descri
                    ENDIF
                    SELECT (malias)
                    IF  .NOT.  ;
                        _tipo
                         REPLACE &_campo;
WITH  xcod
                    ENDIF
                    v_fun = .T.
               ENDIF
          ENDIF
     ELSE
          SEEK xcod
          IF mmsg .AND. FOUND()
               @ _x, _y SAY  ;
                 descri
          ENDIF
          v_fun = FOUND()
     ENDIF
ENDIF
SELECT (malias)
RETURN v_fun
*
FUNCTION val_artdet
PARAMETER xcod, _tipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT iteart
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SEEK xcod
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF  .NOT. EMPTY(xcod)
          SET ORDER TO 1
          SEEK 'B' + xcod
          IF  .NOT. FOUND()
               IF  .NOT.  ;
                   yesno( ;
                   'Desea registrar un producto?' ;
                   )
                    UNLOCK ALL
                    SELECT (malias)
                    v_fun = .F.
                    RETURN v_fun
               ELSE
                    DO agredet
               ENDIF
          ENDIF
          ACTIVATE SCREEN
          ON KEY LABEL F10 KEYBOARD CHR(23)
          ON KEY LABEL F8 DO BorrDet
          ON KEY LABEL F5 DO AgreDet
          ON KEY LABEL F2 DO FunBusDet
          DEFINE WINDOW _busart  ;
                 FROM 2, 02 TO 22,  ;
                 77
          BROWSE FIELDS codart :H =  ;
                 'C¢digo' :W =  ;
                 EMPTY(SUBSTR(codart,  ;
                 8, 3)), descri  ;
                 :H = 'Nombre' :  ;
                 50 :W =  ;
                 EMPTY(descri),  ;
                 coduni :H =  ;
                 'Unidad' : 7 :W =  ;
                 EMPTY(coduni)  ;
                 NOMENU NOAPPEND  ;
                 NODELETE WINDOW  ;
                 _busart KEY 'B' +  ;
                 xcod TITLE  ;
                 '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²'  ;
                 NOLGRID
          ON KEY LABEL F10
          ON KEY LABEL F8
          ON KEY LABEL F5
          ON KEY LABEL F2
          RELEASE WINDOW _busart
          SET ORDER TO 1
          IF  .NOT.  ;
              EMPTY(_oldwnd)
               ACTIVATE WINDOW &_OldWnd
          ENDIF
          IF LASTKEY() = 27
               v_fun = .F.
          ELSE
               xcod = codart
               IF mmsg
                    @ _x, _y SAY  ;
                      descri
               ENDIF
               SELECT (malias)
               IF  .NOT. _tipo
                    REPLACE &_campo WITH;
 xcod
               ENDIF
               v_fun = .T.
          ENDIF
     ENDIF
ENDIF
SELECT (malias)
UNLOCK ALL
RETURN v_fun
*
FUNCTION agrart
vcodart = LEFT(produ.codart, 4)
IF f_appd()
     REPLACE codart WITH vcodart,  ;
             fecreg WITH DATE()
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION borart
IF RLOCK()
     IF yesno( ;
        'Desea borrar esta Clasificaci¢n?' ;
        )
          DELETE NEXT 1
     ENDIF
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN .T.
*
FUNCTION agredet
vcodart = SUBSTR(produ.codart, 2,  ;
          6)
vtipart = SUBSTR(produ.codart, 1,  ;
          1)
IF f_appd()
     REPLACE codart WITH vcodart +  ;
             '.', tipart WITH  ;
             vtipart, fecreg WITH  ;
             DATE()
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION borrdet
IF RLOCK()
     IF yesno( ;
        'Desea borrar esta Clasificaci¢n?' ;
        )
          DELETE NEXT 1
     ENDIF
ELSE
     DO standby WITH  ;
        'No puede eliminar este Item.'
ENDIF
RETURN .T.
*
FUNCTION val_fun
PARAMETER v__al, v__dev, v__bus,  ;
          v__cod, v__tipo, v__x,  ;
          v__y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind, _campo,  ;
        mvali, mrec
medita = (PARAMETERS() >= 5)  ;
         .AND. (v__tipo <> 3)
mmsg = (PARAMETERS() = 7) .AND.  ;
       (v__tipo <> 3)
mvali = (PARAMETERS() = 5) .AND.  ;
        (v__tipo = 3)
mrec = IIF(EOF(), -1, RECNO())
malias = ALIAS()
IF medita
     _campo = TRIM(malias) + '.' +  ;
              IIF(v__tipo > 1,  ;
              VARREAD(), v__dev)
ENDIF
SELECT &v__al
_oldwnd = WOUTPUT()
IF  .NOT. medita
     SEEK v__cod
     v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__bus,""))
ELSE
     IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
          GOTO TOP
          IF EOF()
               DO standby WITH  ;
                  '²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²'
               v_fun = .F.
          ELSE
               IF (v__tipo <> 2)
                    ACTIVATE SCREEN
               ENDIF
               DEFINE POPUP v__xx FROM;
2,40 TO 17,79 PROMPT FIELD &v__bus
               ON SELECTION POPUP v__xx;
DEACTIVATE POPUP
               ACTIVATE POPUP  ;
                        v__xx
               RELEASE POPUP  ;
                       v__xx
               IF  .NOT.  ;
                   EMPTY(_oldwnd)  ;
                   .AND. v__tipo <>  ;
                   2
                    ACTIVATE WINDOW &_OldWnd
               ENDIF
               IF LASTKEY() = 27
                    v_fun = .F.
               ELSE
                    v__cod = &v__dev
                    IF mmsg
                         @ v__x,v__y SAY;
&v__bus
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
                            '²²²²²²²²²² NO HAY ARCHIVO ABIERTO ²²²²²²²²²²'
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
IF  .NOT. EMPTY(ALIAS())
     IF mrec > 0
          GOTO mrec
     ELSE
          GOTO BOTTOM
     ENDIF
ENDIF
RETURN v_fun
*
PROCEDURE funbus
IF escolor
     DEFINE POPUP _MM FROM 16,54 SHADOW;
COLOR &L_COL
ELSE
     DEFINE POPUP _mm FROM 16, 54  ;
            COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF _mm PROMPT  ;
       ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT  ;
       ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW _funbus
          _cod = '000.000'
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!!.!!!'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO 1
               SEEK ALLTRIM(_cod)
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW _funbus
          _cod = SPACE(40)
          @ 01, 02 SAY 'Nombre: '  ;
            GET _cod PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO 2
               SEEK UPPER(ALLTRIM(_cod))
          ENDIF
     OTHERWISE
          RELEASE POPUP _mm
          SET ORDER TO (orden)
          RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
RETURN
*
PROCEDURE funbusdet
IF escolor
     DEFINE POPUP _MM FROM 16,54 SHADOW;
COLOR &L_COL
ELSE
     DEFINE POPUP _mm FROM 16, 54  ;
            COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF _mm PROMPT  ;
       ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT  ;
       ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
     CASE BAR() = 1
          ACTIVATE WINDOW _funbus
          _cod = '00.000.000'
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!.!!!.!!!'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO 1
               SEEK ALLTRIM(_cod)
          ENDIF
     CASE BAR() = 2
          ACTIVATE WINDOW _funbus
          _cod = SPACE(40)
          @ 01, 02 SAY 'Nombre: '  ;
            GET _cod PICTURE  ;
            '@S30'
          READ
          DEACTIVATE WINDOW  ;
                     _funbus
          IF LASTKEY() <> 27
               SET ORDER TO 2
               SEEK UPPER(ALLTRIM(_cod))
          ENDIF
     OTHERWISE
          RELEASE POPUP _mm
          SET ORDER TO (orden)
          RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
RETURN
*
FUNCTION val_cuen
PARAMETER xcod, xtipo, _x, _y
PRIVATE medita, mmsg, malias,  ;
        v_fun, _oldwind
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 4) .AND.  ;
       xtipo
xcod = SUBSTR(xcod, 1, 10)
v_fun = .F.
_campo = VARREAD()
malias = ALIAS()
SELECT cuen
_oldwnd = WOUTPUT()
SEEK xcod
IF  .NOT. medita
     v_fun = IIF(FOUND(), descri,  ;
             '')
ELSE
     IF EMPTY(xcod) .OR.  .NOT.  ;
        FOUND()
          ACTIVATE SCREEN
          DEFINE POPUP _xx FROM 5,  ;
                 40 PROMPT FIELDS  ;
                 descri
          ON SELECTION POPUP _xx DEACTIVATE;
POPUP
          ACTIVATE POPUP _xx
          IF  .NOT.  ;
              EMPTY(_oldwnd)
               ACTIVATE WINDOW &_OldWnd
          ENDIF
          IF LASTKEY() = 27
               v_fun = .F.
               xcod = SPACE(10)
          ELSE
               xcod = cuenta
          ENDIF
     ENDIF
     IF  .NOT. EMPTY(xcod)
          v_fun = .T.
          IF mmsg
               @ _x, _y SAY  ;
                 TRIM(cuenta)
               @ _x, _y + 11 SAY  ;
                 descri
          ENDIF
          SELECT (malias)
          IF  .NOT. xtipo
               REPLACE &_campo WITH;
 Cuen.Cuenta
          ENDIF
          v_fun = .T.
     ENDIF
ENDIF
SELECT (malias)
RETURN v_fun
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
PROCEDURE clrscr1
PRIVATE fil, col
fil = 0
col = 39
col1 = 40
DO WHILE (col>0)
     @ fil, col CLEAR TO 24, col
     @ fil, col1 CLEAR TO 24,  ;
       col1
     col = col - 1
     col1 = col1 + 1
     FOR y = 1 TO 150
     ENDFOR
ENDDO
CLEAR
RETURN
*
FUNCTION poperror
PARAMETER err_mess
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
FUNCTION ve_passw
PARAMETER _psw
PRIVATE xxx
xxx = SPACE(5)
DEFINE WINDOW _pasw FROM 12, 15  ;
       TO 17, 65 TITLE  ;
       ' Password de seguridad '  ;
       COLOR SCHEME 5
ACTIVATE WINDOW _pasw
CLEAR
@ 1, 4 SAY  ;
  'Opci¢n restringida, ingrese password: '
@ 2, 20 SAY '[     ]'
@ 2, 21 GET xxx PICTURE '!!!!!'  ;
  COLOR ,N 
READ
RELEASE WINDOW _pasw
IF LASTKEY() <> 27 .AND. xxx =  ;
   _psw
     RETURN .T.
ELSE
     DO standby WITH  ;
        'Password incorrecto. ! Acceso denegado !'
ENDIF
RETURN .F.
*
FUNCTION password
PARAMETER pw_len
PRIVATE pw_len, pass_str, key,  ;
        cur_curs
IF PARAMETERS() = 0
     pw_len = 6
ENDIF
IF pw_len <= 0
     pw_len = 6
ELSE
     IF pw_len > 30
          pw_len = 30
     ENDIF
ENDIF
pass_str = ''
key = 0
cur_curs = SET('cursor') = 'ON'
SET CURSOR ON
cur_win = WOUTPUT()
DEFINE WINDOW pass_win FROM 08,  ;
       20 TO 15, 60 DOUBLE COLOR  ;
       SCHEME 21
ACTIVATE WINDOW pass_win
@ 02, 09 SAY  ;
  'Ingrese su password'
@ 03, 12 SAY '(' +  ;
  LTRIM(STR(pw_len)) +  ;
  ' caracteres)'
@ 05, 15 SAY ''
DO WHILE m.key<>13 .AND. m.key<> ;
   27
     SET COLOR OF NORMAL TO N
     m.key = INKEY(0)
     DO CASE
          CASE BETWEEN(m.key, 65,  ;
               90) .OR.  ;
               BETWEEN(m.key, 97,  ;
               122)
               pass_str = pass_str +  ;
                          CHR(m.key)
          CASE m.key = 19 .OR.  ;
               m.key = 127 .OR.  ;
               m.key = 7
               @ ROW(), COL() - 1  ;
                 SAY ' '
               @ ROW(), COL() - 1  ;
                 SAY ''
               pass_str = SUBSTR(pass_str,  ;
                          1,  ;
                          LEN(pass_str) -  ;
                          1)
          OTHERWISE
     ENDCASE
     IF LEN(pass_str) >= pw_len
          m.key = 13
     ENDIF
     SET COLOR OF NORMAL TO
     @ 05, 15 SAY SPACE(pw_len)
     @ 05, 15 SAY REPLICATE('',  ;
       LEN(pass_str))
ENDDO
RELEASE WINDOW pass_win
IF EMPTY(cur_win)
     ACTIVATE SCREEN
ENDIF
IF  .NOT. cur_curs
     SET CURSOR OFF
ENDIF
RETURN (pass_str)
*
PROCEDURE val_tc
PARAMETER vfecha, vretval
malias = ALIAS()
SELECT tipcam
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No existen tipos de cambio registrados'
     SELECT IIF(EMPTY(malias), 0,  ;
            (malias))
     RETURN
ENDIF
SEEK DTOC(vfecha, 1)
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL ENTER DO Sel_TC
ON KEY LABEL F10 DO Sel_TC
ON KEY LABEL RIGHTARROW KEYBOARD CHR(9);
   
DEFINE WINDOW _wintc FROM 05, 32  ;
       TO 13, 78 TITLE  ;
       ' Para seleccionar presione   Enter  ¢  F10 '  ;
       COLOR SCHEME 8
BROWSE FIELDS fecha :W = .F.,  ;
       compar :H = 'Compra ML' :P =  ;
       '9,999.99', venpar :H =  ;
       'Venta ML' :P = '9,999.99',  ;
       combco :H = 'Compra Bco'  ;
       :P = '9,999.99', venbco :H =  ;
       'Venta Bco' :P =  ;
       '9,999.99' NOAPPEND NOEDIT  ;
       NODELETE WINDOW _wintc
ON KEY
IF  .NOT. EMPTY(_oldwnd)
     ACTIVATE WINDOW &_OldWnd
ENDIF
IF SUBSTR(VERSION(), 12) = '2.0'
     SELECT IIF(EMPTY(malias),  ;
            SELECT(0), (malias))
ELSE
     IF  .NOT. EMPTY(malias)
          SELECT (malias)
     ENDIF
ENDIF
RETURN
*
PROCEDURE sel_tc
ON KEY
vcampo = VARREAD()
vRetVal = &vcampo
KEYBOARD CHR(23)
RETURN
*
PROCEDURE fox_ambi
ON READERROR DO   Err_input
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
SET CLOCK TO 00, 69
SET COMPATIBLE OFF
SET CONFIRM ON
SET CONSOLE ON
SET CURRENCY LEFT
SET DATE BRITISH
IF  .NOT. worker
     SET DEBUG OFF
ELSE
ENDIF
SET DECIMALS TO 2
SET DEFAULT TO
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
SET MULTILOCKS ON
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
SET HELP OFF
SET HOURS TO 24
SET INSTRUCT ON
SET INTENSITY ON
IF worker
     SET LOGERRORS ON
ELSE
     SET LOGERRORS OFF
ENDIF
SET MARGIN TO 0
SET MARK TO '-'
SET MEMOWIDTH TO 80
SET MENU ON
SET MESSAGE TO 23
SET NOTIFY ON
SET NEAR OFF
SET ODOMETER TO 1
SET PATH TO
SET POINT TO '.'
SET PRECISION TO 3
SET PRINTER OFF
SET PRINTER TO lpt1
SET SAFETY OFF
SET SCOREBOARD OFF
SET SYSMENU OFF
SET SEPARATOR TO ','
SET SPACE OFF
SET STATUS OFF
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
= INSMODE(.F.)
= CAPSLOCK(.T.)
= NUMLOCK(.T.)
DEFINE WINDOW err_input FROM 01,  ;
       60 TO 03, 77
DEFINE WINDOW yesno FROM 19, 27  ;
       TO 23, 77 GROW FLOAT  ;
       SHADOW DOUBLE COLOR SCHEME  ;
       5
DEFINE WINDOW msg2user FROM 04,  ;
       02 TO 08, 77 FLOAT SHADOW  ;
       DOUBLE
DEFINE WINDOW standby FROM 19, 27  ;
       TO 23, 77 FLOAT SHADOW  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW _funbus FROM 19, 27  ;
       TO 23, 77 FLOAT SHADOW  ;
       DOUBLE
PUBLIC escape
escape = 27
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
_RMARGIN = 80
_TABS = '10, 20, 30, 40, 50, 60, 70'
_WRAP = .T.
RETURN
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
FUNCTION pidefec
PARAMETER _titu, vfecini, vfecfin
IF PARAMETERS() = 1
     RETURN .F.
ENDIF
PRIVATE _retval
_retval = .T.
ff = IIF(PARAMETERS() = 2, 20,  ;
     21)
ACTIVATE WINDOW standby
@ 00, 00 SAY PADC(_titu, WCOLS(),  ;
  ' ')
IF PARAMETERS() = 2
     @ 01, 04 SAY 'Fecha  : ' GET  ;
       vfecini ERROR  ;
       'Fecha Inv lida'
ELSE
     @ 01, 04 SAY 'Inicio : ' GET  ;
       vfecini ERROR  ;
       'Fecha Inv lida'
     @ 02, 04 SAY 'Final  : ' GET  ;
       vfecfin VALID vfecini <=  ;
       vfecfin ERROR  ;
       'Fecha Inv lida'
ENDIF
READ
DEACTIVATE WINDOW standby
IF LASTKEY() = 27
     _retval = .F.
ENDIF
RETURN _retval
*
PROCEDURE msgpro
PARAMETER _sw, _lin, _msg
PRIVATE _sw, _msg, vnumpara
vnumpara = PARAMETERS()
_lin = IIF(vnumpara = 1, 23,  ;
       _lin)
IF vnumpara = 1 .OR. vnumpara = 2
     IF _sw
          _msg = 'Procesando, por favor espere un momento..'
     ENDIF
ENDIF
IF _sw
     PUBLIC _oldmsg, _oldwnd
     _msg = PADC(_msg, 2 +  ;
            LEN(_msg), ' ')
     _oldmsg = _msg
     _oldwnd = WOUTPUT()
     _pos = (80 - LEN(_oldmsg)) /  ;
            2
     SET BLINK ON
     IF  .NOT. WEXIST('_xyz')
          DEFINE WINDOW _xyz FROM  ;
                 _lin, _pos TO  ;
                 _lin,  ;
                 LEN(_oldmsg) +  ;
                 _pos IN screen  ;
                 NONE
     ENDIF
     ACTIVATE WINDOW SAME _xyz
     @ 00, 00 SAY _msg COLOR N/W* 
ELSE
     DEACTIVATE WINDOW _xyz
     RELEASE WINDOW _xyz
     SET BLINK OFF
ENDIF
IF TYPE('_OldWnd') <> 'U'
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ELSE
          ACTIVATE SCREEN
     ENDIF
ENDIF
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
FUNCTION val_lee
PRIVATE _fun
_fun = .T.
IF LASTKEY() <> 27
     IF  .NOT. yesno( ;
         '¨ Est n correctos los datos ?', ;
         11,15)
          _fun = .F.
     ENDIF
ENDIF
RETURN (_fun)
*
FUNCTION verestcp
PARAMETER vest
PRIVATE vfun
vfun = SPACE(10)
DO CASE
     CASE vest = '00'
          vfun = 'Pendiente   '
     CASE vest = '10'
          vfun = 'Retencion OK'
     CASE vest = '20'
          vfun = 'Chq.Girado  '
     CASE vest = '50'
          vfun = 'C/P Aprobado'
     CASE vest = '99'
          vfun = 'Anulado     '
ENDCASE
RETURN vfun
*
PROCEDURE estado
PARAMETER vdoc, vllave1
PRIVATE valias, vperhc, vnumhc,  ;
        vfun
valias = ALIAS()
ON KEY LABEL F7
DEFINE WINDOW estado FROM 5, 3 TO  ;
       9, 77 TITLE  ;
       '®ESC¯ Salir    °°° Seguimiento de ' +  ;
       IIF(vdoc = 'PE', 'PECOSA',  ;
       IIF(vdoc = 'SS',  ;
       'Solicitud de Servicio',  ;
       IIF(vdoc = 'OC', 'O/C',  ;
       'O/S'))) +  ;
       ' °°°               '
ACTIVATE WINDOW standby
@ 1, 14 SAY  ;
  'Espere un momento ...' COLOR W+/ ;
  RB* 
vfun = .T.
DO CASE
     CASE UPPER(vdoc) = 'PE'
          SELECT orden
          vord = ORDER()
          SET ORDER TO OrdCom1
          SEEK &vLlave1 
          IF FOUND() .AND.  .NOT.  ;
             estado $ '0099'
               vperhc = orden.perhc
               vnumhc = orden.numhc
          ELSE
               vperhc = ''
               vnumhc = ''
               vfun = .F.
          ENDIF
          SET ORDER TO (vord)
     CASE UPPER(vdoc) = 'SS'
          SELECT ordse
          vord = ORDER()
          SET ORDER TO OrdSer1
          SEEK &vLlave1
          IF FOUND() .AND.  .NOT.  ;
             estado $ '0099'
               vperhc = ordse.perhc
               vnumhc = ordse.numhc
          ELSE
               vperhc = ''
               vnumhc = ''
               vfun = .T.
          ENDIF
          SET ORDER TO (vord)
ENDCASE
SELECT hoja
vord = ORDER()
SET ORDER TO hojcon1
SEEK IIF(vdoc='OC' OR vdoc='OS',&vllave1,vPerHc+vnumhc)
IF FOUND() .AND.  .NOT. estado $  ;
   '0099' .AND. vfun
     IF  .NOT. EMPTY(hoja.numcp)
          vnumcp = hoja.numcp
          vmescp = hoja.nummescp
          vcta = hoja.codctc
     ELSE
          vnumcp = ''
          vmescp = ''
          vcta = ''
          vfun = .F.
     ENDIF
ELSE
     vnumcp = ''
     vmescp = ''
     vcta = ''
     vfun = .F.
ENDIF
SET ORDER TO (vord)
SELECT compag
vord = ORDER()
SET ORDER TO Compag1
SEEK ALLTRIM(vmescp) + vnumcp +  ;
     ALLTRIM(vcta)
IF FOUND() .AND. vfun
     vpercp = periodo
     vimport = compag.import
     vreten = compag.reten
ELSE
     vpercp = ''
     vfun = .F.
     vimport = 0
     vreten = 0
ENDIF
SET ORDER TO (vord)
SELECT cheque
vord = ORDER()
SET ORDER TO Cheque1
SEEK vmescp + vnumcp + vcta
IF FOUND() .AND. vfun
     vnumch = cheque.numchq
ELSE
     vnumch = ''
ENDIF
SET ORDER TO (vord)
DO CASE
     CASE vdoc = 'OC' .OR. vdoc =  ;
          'OS'
          IF FOUND() .AND. vfun
               DEACTIVATE WINDOW  ;
                          standby
               BROWSE NOOPTIMIZE  ;
                      FIELDS p =  ;
                      IIF(EMPTY(m.numhc),  ;
                      'No afect',  ;
                      m.numhc +  ;
                      '/' +  ;
                      m.perhc) :H =  ;
                      ' H/C ' : 8,  ;
                      cp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      'Sin pago',  ;
                      vnumcp +  ;
                      '/' +  ;
                      vmescp) :H =  ;
                      '   C/P ' :  ;
                      8, cheq =  ;
                      IIF(EMPTY(vnumch),  ;
                      'No girado',  ;
                      vnumch) :H =  ;
                      '  Cheq.' :  ;
                      12, imp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      '    ---.--',  ;
                      vimport) :H =  ;
                      '  Total' :  ;
                      12, ret =  ;
                      IIF(EMPTY(vnumcp),  ;
                      '    ---.--',  ;
                      vreten) :H =  ;
                      ' Retenido'  ;
                      : 12, tp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      '    ---.--',  ;
                      vimport -  ;
                      vreten) :H =  ;
                      'Total Pagado'  ;
                      : 12 NOMENU  ;
                      NOAPPEND  ;
                      NOEDIT  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      estado KEY  ;
                      vmescp +  ;
                      vnumcp +  ;
                      vcta  ;
                      NOREFRESH
          ELSE
               SELECT numchq FROM  ;
                      Cheque  ;
                      WHERE  ;
                      RECNO() = 1  ;
                      INTO CURSOR  ;
                      query
               SELECT query
               DEACTIVATE WINDOW  ;
                          standby
               BROWSE NOOPTIMIZE  ;
                      FIELDS p =  ;
                      IIF(EMPTY(m.numhc),  ;
                      'No afect',  ;
                      m.numhc +  ;
                      '/' +  ;
                      m.perhc) :H =  ;
                      ' H/C ' : 8,  ;
                      cp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      'Sin pago',  ;
                      vnumcp +  ;
                      '/' +  ;
                      vmescp) :H =  ;
                      '   C/P ' :  ;
                      8, cheq =  ;
                      IIF(EMPTY(vnumch),  ;
                      'No girado',  ;
                      vnumch) :H =  ;
                      '  Cheq.' :  ;
                      12, imp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      '    ---.--',  ;
                      vimport) :H =  ;
                      '  Total' :  ;
                      12, ret =  ;
                      IIF(EMPTY(vnumcp),  ;
                      '    ---.--',  ;
                      vreten) :H =  ;
                      ' Retenido'  ;
                      : 12, tp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      '    ---.--',  ;
                      vimport -  ;
                      vreten) :H =  ;
                      'Total Pagado'  ;
                      : 12 NOMENU  ;
                      NOAPPEND  ;
                      NOEDIT  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      estado  ;
                      NOREFRESH
          ENDIF
     OTHERWISE
          IF FOUND() .AND. vfun
               DEACTIVATE WINDOW  ;
                          standby
               BROWSE NOOPTIMIZE  ;
                      FIELDS oc =  ;
                      IIF(EMPTY(IIF(vdoc =  ;
                      'SS',  ;
                      solser.numos,  ;
                      itepec.numoc)),  ;
                      'Pendiente',  ;
                      IIF(UPPER(vdoc) =  ;
                      'SS',  ;
                      solser.numos,  ;
                      itepec.numoc))  ;
                      :H =  ;
                      IIF(UPPER(vdoc) =  ;
                      'SS',  ;
                      '  O/S  ',  ;
                      '   O/C  ')  ;
                      : 14, hc =  ;
                      IIF(EMPTY(vnumhc),  ;
                      'Sin afectaci¢n',  ;
                      vnumhc +  ;
                      '/' +  ;
                      vperhc) :H =  ;
                      '   H/C ' :  ;
                      14, cp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      'Sin pago',  ;
                      vnumcp +  ;
                      '/' +  ;
                      vmescp) :H =  ;
                      '   C/P ' :  ;
                      14, cheq =  ;
                      IIF(EMPTY(vnumch),  ;
                      'No girado',  ;
                      vnumch) :H =  ;
                      '  Cheq.' :  ;
                      14 NOMENU  ;
                      NOAPPEND  ;
                      NOEDIT  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      estado KEY  ;
                      vmescp +  ;
                      vnumcp +  ;
                      vcta  ;
                      NOREFRESH
          ELSE
               SELECT numchq FROM  ;
                      Cheque  ;
                      WHERE  ;
                      RECNO() = 1  ;
                      INTO CURSOR  ;
                      query
               SELECT query
               DEACTIVATE WINDOW  ;
                          standby
               BROWSE NOOPTIMIZE  ;
                      FIELDS oc =  ;
                      IIF(EMPTY(IIF(vdoc =  ;
                      'SS',  ;
                      solser.numos,  ;
                      itepec.numoc)),  ;
                      'Pendiente',  ;
                      IIF(UPPER(vdoc) =  ;
                      'SS',  ;
                      solser.numos,  ;
                      itepec.numoc))  ;
                      :H =  ;
                      IIF(UPPER(vdoc) =  ;
                      'SS',  ;
                      '  O/S  ',  ;
                      '   O/C  ')  ;
                      : 14, hc =  ;
                      IIF(EMPTY(vnumhc),  ;
                      'Sin afectaci¢n',  ;
                      vnumhc +  ;
                      '/' +  ;
                      vperhc) :H =  ;
                      '   H/C ' :  ;
                      14, cp =  ;
                      IIF(EMPTY(vnumcp),  ;
                      'Sin pago',  ;
                      vnumcp +  ;
                      '/' +  ;
                      vmescp) :H =  ;
                      '   C/P ' :  ;
                      14, cheq =  ;
                      IIF(EMPTY(vnumch),  ;
                      'No girado',  ;
                      vnumch) :H =  ;
                      '  Cheq.' :  ;
                      14 NOMENU  ;
                      NOAPPEND  ;
                      NOEDIT  ;
                      NODELETE  ;
                      NOCLEAR  ;
                      WINDOW  ;
                      estado  ;
                      NOREFRESH
          ENDIF
ENDCASE
SELECT &vAlias
DO CASE
     CASE UPPER(vdoc) = 'PE'
          ON KEY LABEL F7 DO Estado WITH;
'PE','ItePec.Periodo+Itepec.Numoc+Itepec.Codfte'
     CASE UPPER(vdoc) = 'SS'
          ON KEY LABEL F7 DO Estado WITH;
'SS','Solser.Periodo+solser.Numos+solser.Codfte'
     CASE UPPER(vdoc) = 'OC'
          ON KEY LABEL F7 DO Estado WITH;
'OC','m.perhc+m.numhc'
     CASE UPPER(vdoc) = 'OS'
          ON KEY LABEL F7 DO Estado WITH;
'OS','m.perhc+m.numhc'
ENDCASE
RETURN
IF vtipo = 1
     IF ALLTRIM(m.tipfun) = 'I'
          SELECT iteoc.periodo,  ;
                 iteoc.tipfun,  ;
                 iteoc.estado,  ;
                 iteoc.codfte,  ;
                 iteoc.codcal,  ;
                 iteoc.valtot,  ;
                 iteoc.codpart,  ;
                 iteoc.codanal  ;
                 FROM ITEOC WHERE  ;
                 nummes + tipfun +  ;
                 codfte + periodo +  ;
                 SUBSTR(iteoc.codcal,  ;
                 8, 2) + IIF( NOT  ;
                 EMPTY(vcodsub),  ;
                 SUBSTR(iteoc.codcal,  ;
                 10, 3), '') +  ;
                 IIF( NOT  ;
                 EMPTY(vproyec),  ;
                 SUBSTR(iteoc.codcal,  ;
                 13, 3), '') +  ;
                 IIF( NOT  ;
                 EMPTY(proact),  ;
                 SUBSTR(iteoc.codcal,  ;
                 16, 2), '') =  ;
                 ALLTRIM(m.nummes) +  ;
                 ALLTRIM(m.tipfun) +  ;
                 ALLTRIM(m.codfte) +  ;
                 xvalor + IIF(  ;
                 NOT  ;
                 EMPTY(proact),  ;
                 proact, '') AND  ;
                 IIF(vtipo = 1  ;
                 AND  ;
                 ALLTRIM(m.tipfun) =  ;
                 'F', codpart =  ;
                 '02', IIF(vtipo =  ;
                 2 AND  ;
                 ALLTRIM(m.tipfun) =  ;
                 'F', codpart =  ;
                 '03', .T.)) AND  ;
                 tipfun = 'I'  ;
                 INTO CURSOR  ;
                 ITEOCX
     ELSE
          SELECT iteoc.periodo,  ;
                 iteoc.tipfun,  ;
                 iteoc.estado,  ;
                 iteoc.codfte,  ;
                 iteoc.codcal,  ;
                 iteoc.valtot,  ;
                 iteoc.codpart,  ;
                 iteoc.codanal  ;
                 FROM ITEOC WHERE  ;
                 nummes + tipfun +  ;
                 codfte + periodo +  ;
                 SUBSTR(iteoc.codcal,  ;
                 8, 2) + IIF( NOT  ;
                 EMPTY(vcodsub),  ;
                 SUBSTR(iteoc.codcal,  ;
                 10, 3), '') +  ;
                 IIF( NOT  ;
                 EMPTY(proact),  ;
                 SUBSTR(iteoc.codcal,  ;
                 13, 2), '') =  ;
                 ALLTRIM(m.nummes) +  ;
                 ALLTRIM(m.tipfun) +  ;
                 ALLTRIM(m.codfte) +  ;
                 xvalor + IIF(  ;
                 NOT  ;
                 EMPTY(proact),  ;
                 proact, '') AND  ;
                 IIF(vtipo = 1,  ;
                 codpart = '02',  ;
                 IIF(vtipo = 2,  ;
                 codpart = '03',  ;
                 .T.)) AND tipfun =  ;
                 'F' INTO CURSOR  ;
                 ITEOCX
     ENDIF
     vind = SYS(3) + '.DBF'
     COPY TO (vind)
     USE IN 27 EXCLUSIVE (vind)  ;
         ALIAS orden1
ELSE
     IF ALLTRIM(m.tipfun) = 'I'
          SELECT ordse.periodo,  ;
                 ordse.tipfun,  ;
                 ordse.estado,  ;
                 ordse.codfte,  ;
                 ordse.codcal,  ;
                 ordse.valtot,  ;
                 ordse.codpart,  ;
                 ordse.codanal  ;
                 FROM ORDSE WHERE  ;
                 nummes + tipfun +  ;
                 codfte + periodo +  ;
                 SUBSTR(codcal, 8,  ;
                 2) + IIF( NOT  ;
                 EMPTY(vcodsub),  ;
                 SUBSTR(codcal,  ;
                 10, 3), '') +  ;
                 IIF( NOT  ;
                 EMPTY(vproyec),  ;
                 SUBSTR(codcal,  ;
                 13, 3), '') +  ;
                 IIF( NOT  ;
                 EMPTY(proact),  ;
                 SUBSTR(codcal,  ;
                 16, 2), '') =  ;
                 ALLTRIM(m.nummes) +  ;
                 ALLTRIM(m.tipfun) +  ;
                 ALLTRIM(m.codfte) +  ;
                 xvalor + IIF(  ;
                 NOT  ;
                 EMPTY(proact),  ;
                 proact, '') AND  ;
                 IIF(vtipo = 1  ;
                 AND  ;
                 ALLTRIM(m.tipfun) =  ;
                 'F', codpart =  ;
                 '02', IIF(vtipo =  ;
                 2 AND  ;
                 ALLTRIM(m.tipfun) =  ;
                 'F', codpart =  ;
                 '03', .T.)) AND  ;
                 tipfun = 'I'  ;
                 INTO CURSOR  ;
                 ORDSEX
     ELSE
          SELECT ordse.periodo,  ;
                 ordse.tipfun,  ;
                 ordse.estado,  ;
                 ordse.codfte,  ;
                 ordse.codcal,  ;
                 ordse.valtot,  ;
                 ordse.codpart,  ;
                 ordse.codanal  ;
                 FROM ORDSE WHERE  ;
                 nummes + tipfun +  ;
                 codfte + periodo +  ;
                 SUBSTR(codcal, 8,  ;
                 2) + IIF( NOT  ;
                 EMPTY(vcodsub),  ;
                 SUBSTR(codcal,  ;
                 10, 3), '') +  ;
                 IIF( NOT  ;
                 EMPTY(proact),  ;
                 SUBSTR(codcal,  ;
                 13, 2), '') =  ;
                 ALLTRIM(m.nummes) +  ;
                 ALLTRIM(m.tipfun) +  ;
                 ALLTRIM(m.codfte) +  ;
                 xvalor + IIF(  ;
                 NOT  ;
                 EMPTY(proact),  ;
                 proact, '') AND  ;
                 IIF(vtipo = 1,  ;
                 codpart = '02',  ;
                 IIF(vtipo = 2,  ;
                 codpart = '03',  ;
                 .T.)) AND tipfun =  ;
                 'F' INTO CURSOR  ;
                 ORDSEX
     ENDIF
     vind = SYS(3) + '.DBF'
     COPY TO (vind)
     USE IN 27 EXCLUSIVE (vind)  ;
         ALIAS ordse1
ENDIF
*
FUNCTION vestpec
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
     CASE vest = '00'
          vfun = 'Emitido      '
     CASE vest = '20'
          vfun = 'Con S/C      '
     CASE vest = '30'
          vfun = 'Con O/C      '
     CASE vest = '40'
          vfun = 'Despachado   '
     CASE vest = '50'
          vfun = 'Liquidado    '
     CASE vest = '51'
          vfun = 'Contabilizado'
     CASE vest = '99'
          vfun = 'Anulado      '
ENDCASE
RETURN vfun
*
PROCEDURE savcon
USE IN 0 UsuGRAU ALIAS usu
PUBLIC _hora
SELECT usu
APPEND BLANK
_hora = TIME()
REPLACE codusu WITH vconex
REPLACE machine WITH vmaq
REPLACE tipmaq WITH  ;
        SUBSTR(SYS(17), 3)
REPLACE prog WITH SYS(16, 1)
REPLACE hora WITH _hora
REPLACE dia WITH DAY(DATE())
REPLACE llave WITH vllav
vusurec = RECNO()
USE
RETURN
*
PROCEDURE borcon
USE IN 0 UsuGRAU ALIAS usu
SELECT usu
GOTO TOP
vprg = SYS(16, 1)
SET EXACT ON
DO WHILE  .NOT. EOF()
     LOCATE FOR ALLTRIM(codusu) =  ;
            vconex .AND.  ;
            ALLTRIM(machine) =  ;
            vmaq .AND. llave =  ;
            vllav
     IF FOUND()
          DELETE NEXT 1
     ENDIF
     CONTINUE
ENDDO
SET EXACT OFF
USE
RETURN
*
PROCEDURE elicon
USE IN 0 UsuGRAU ALIAS usu
SELECT usu
GOTO vusurec
DELETE NEXT 1
USE
RETURN
*
FUNCTION val_boy
PARAMETER mvalor, filtro,  ;
          mvariable
PRIVATE malias
malias = ALIAS()
SELECT parma
SEEK filtro + mvalor
mvalor = parma.codigo
mdescr = parma.descri
mdescriaux = parma.descriaux
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
RETURN mdescr + mdescriaux
*
FUNCTION getcre
PRIVATE vkey, valias
valias = ALIAS()
vcodpart = codpart
vkey = codcad + codfte
SELECT itecre
SET RELATION TO periodo + codope + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) INTO cresup
GOTO TOP
SUM FOR codpart = vcodpart .AND.  ;
    RIGHT(numdoc, 2) <=  ;
    ALLTRIM(vcalend) .AND. codcad +  ;
    codfte = vkey monasig TO  ;
    vcresup
vtotalc = vcresup
SELECT (valias)
RETURN vtotalc
*
FUNCTION gettra
PRIVATE vkey, valias
valias = ALIAS()
vcodpart = codpart
vkey = codcad + codfte
SELECT itetra
SET RELATION TO periodo + codope + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) INTO trapar
GOTO TOP
vtransf = 0
SCAN
     IF MONTH(fecha) <=  ;
        VAL(vcalend) .AND. codcad +  ;
        codfte = vkey .AND.  ;
        codpart = vcodpart
          vtransf = vtransf +  ;
                    IIF(tipope =  ;
                    '-', -1, 1) *  ;
                    montra
     ENDIF
ENDSCAN
vtotalt = vtransf
SELECT (valias)
RETURN vtotalt
*
FUNCTION val_codcad
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
SELECT maepre
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd1 = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO periodo + uniges +;
unieje = filtro
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
     ON KEY LABEL f2 DO busCodCad
     ON KEY LABEL f3 DO busDesCad
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad FROM  ;
            02, 01 TO 23, 78  ;
            TITLE  ;
            '[F2]Cadena   [F3]Componente   [F10] seleccionar'  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad
     BROWSE NOOPTIMIZE FIELDS  ;
            codcad :H = 'CodCad',  ;
            codfun :H = 'Fn',  ;
            codprg :H = 'Prg',  ;
            codspr :H = 'SPrg',  ;
            actpry :H = 'Act/Pry',  ;
            codcom :H = 'CodComp',  ;
            codmet :H = 'Meta',  ;
            descri :H =  ;
            'Descripci¢n' : 40  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relaci¢n de Cadenas Funcionales '  ;
            IN wind_cad  ;
            NOREFRESH
     RELEASE WINDOW wind_cad
     ON KEY LABEL f2
     ON KEY LABEL f3
     SET FILTER TO
ENDIF
mvalor = maepre.codcad
mdescr = SUBSTR(maepre.descri, 1,  ;
         mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'A'
          RETURN mcodaux
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
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
PROCEDURE buscodcad
vcursor = RECNO()
DEFINE WINDOW lis FROM 09, 12 TO  ;
       16, 68 FLOAT TITLE  ;
       ' °° B£squeda X C¢digo °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
vcodcad = SPACE(4)
@ 3, 2 SAY '  Cadena : ' GET  ;
  vcodcad PICTURE '!!!!'
READ
SEEK filtro + vcodcad
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No existe Codigo de Cadena...'
     GOTO vcursor
ENDIF
RELEASE WINDOW lis
RETURN
*
PROCEDURE busdescad
vcursor = RECNO()
vorder = ORDER()
DEFINE WINDOW lis FROM 09, 12 TO  ;
       16, 68 FLOAT TITLE  ;
       ' °° B£squeda °° ' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lis
vcodcom = SPACE(60)
@ 3, 2 SAY ' Componente : ' GET  ;
  vcodcom PICTURE '!!!!!' VALID  ;
  val_para(vcodcom,'CODCOM',' ', ;
  18,30)
READ
LOCATE FOR codcom =  ;
       ALLTRIM(vcodcom)
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No existe Codigo de Componente...'
     GOTO vcursor
ENDIF
RELEASE WINDOW lis
RETURN
*
FUNCTION ver_pres
PARAMETER vkey
valias = ALIAS()
SELECT presu
SEEK vkey
IF  .NOT. FOUND()
     IF  .NOT. yesno( ;
         'Cadena y fte. no est n presupuestadas...¨ Continua ?' ;
         )
          SELECT (valias)
          RETURN .F.
     ENDIF
ENDIF
SELECT (valias)
RETURN .T.
*
FUNCTION val_parti
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
     CASE PARAMETERS() = 4
          mlong = 40
ENDCASE
malias = ALIAS()
SELECT ingreso
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     IF  .NOT. EMPTY(filtro)
          SET FILTER TO coding = filtro
     ENDIF
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen partidas definidas'
          SET FILTER TO
          SELECT (malias)
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            coding + ' ' +  ;
            SUBSTR(desing, 1,  ;
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
mvalor = ingreso.coding
mdescr = SUBSTR(ingreso.desing, 1,  ;
         mlong)
m.codpart = mvalor
SET ORDER TO 1
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 7 SAY  ;
            mdescr
          RETURN .T.
     CASE mvariable == 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN
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
     CASE mvariable == 'T'
          &mvariable = mvalor
          @ ROW(), mcol + 7 SAY  ;
            mdescr
          RETURN mvalor
     OTHERWISE
          REPLACE &mvariable WITH mvalor
ENDCASE
*
FUNCTION val_busper
PARAMETER vtipo, vorden
vfun = .T.
SET ORDER TO vorden
SEEK vtipo
IF FOUND()
     DO standby WITH  ;
        'Ya existe esta '+ ;
        IIF(vorden=1, 'L.E.',  ;
        'Cta. de Ahor.')
     vfun = .F.
ENDIF
SET ORDER TO 1
RETURN vfun
*
PROCEDURE espera
PARAMETER festado, fmensaje
IF  .NOT. WEXIST('Espera')
     DEFINE WINDOW espera FROM 12,  ;
            06 TO 14, 78 COLOR  ;
            SCHEME 05
ENDIF
DO CASE
     CASE festado = 1
          ACTIVATE WINDOW espera
          @ 0, 10 SAY fmensaje  ;
            COLOR W+/RB* 
          RETURN
     CASE festado = 2
          DEACTIVATE WINDOW  ;
                     espera
          RETURN
ENDCASE
RETURN
*
FUNCTION cmes
PARAMETER _n_mes
PRIVATE v_fun
IF  .NOT. BETWEEN(_n_mes, 1, 12)
     RETURN 'Inv lido'
ENDIF
v_fun = 'Enero    Febrero  Marzo    Abril    Mayo     Junio    Julio    Agosto   SetiembreOctubre  NoviembreDiciembre'
RETURN TRIM(SUBSTR(v_fun, 9 *  ;
       (_n_mes - 1) + 1, 9))
*
