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
IF vfecha = ''
     vfecha = DATE()
ENDIF
vnumero = MONTH(DATE())
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
vffecha = 'Lima, ' + vdia +  ;
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
          _wp, ran_pg
PRIVATE resul
_wndold = WOUTPUT()
_conso = SET('CONSOLE')
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
         15))
          IF  .NOT.  ;
              yesno( ;
              '¨Imprime en impresora local?' ;
              ) .AND. LASTKEY() <>  ;
              27
               SET PRINTER TO \\Abastecimiento\fx1180-abas
          ENDIF
     ENDIF
     IF  .NOT. ready2pr()
          printing = .F.
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
          FOR v = 1 TO _copia
               xdirimp = SET('PRINT',  ;
                         1)
               IF ready2pr()  ;
                  .AND.  ;
                  IIF(_copia > 1,  ;
                  yesno('Copia ' +  ;
                  STR(v, 2) +  ;
                  ' .¨Prepare el papel. Listo?' ;
                  ), .T.)
                    p_fil = SYS(3) +  ;
                            '.LST'
                    DO &_prgrpt WITH 2
                    !TYPE &p_fil>&xDirImp
               ENDIF
          ENDFOR
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
                    IF  .NOT.  ;
                        EMPTY(LEFT(SYS(0),  ;
                        15))
                         IF  .NOT.  ;
                             yesno( ;
                             '¨Imprime en impresora local?' ;
                             )  ;
                             .AND.  ;
                             LASTKEY() <>  ;
                             27
                              SET PRINTER;
TO \\Abastecimiento\fx1180-abas
                         ENDIF
                    ENDIF
                    xdirimp = SET('PRINT',  ;
                              1)
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
                              !TYPE &p_fil>&xDirImp
                         ENDIF
                    ENDFOR
               ENDIF
          ELSE
               resul = 'OK'
          ENDIF
          IF _dest <> 'Archivo  '
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
@ 01, 5 SAY  ;
  'DESTINO DE IMPRESION : PANTALLA/IMPRESORA'  ;
  GET _dest PICTURE  ;
  '@M IMPRESORA,PANTALLA'
READ
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
                              !TYPE &P_FIL>LPT1
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
>LPT1
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
                                   !TYPE &P_FIL >LPT1
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
          IF _tipo = 4
               MODIFY COMMAND  ;
                      PRUEBA.TXT  ;
                      NOEDIT  ;
                      WINDOW  ;
                      _repo
          ELSE
               MODIFY COMMAND  ;
                      (p_fil)  ;
                      NOEDIT  ;
                      WINDOW  ;
                      _repo
          ENDIF
          IF  .NOT. _wp
               IF yesno( ;
                  '¨ Imprime el Reporte ?' ;
                  )
                    IF ready2pr()
                         !TYPE &P_FIL>LPT1
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
PRIVATE malias, mperiodo
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
SET ORDER TO parmae1
SEEK filtro + IIF( .NOT.  ;
     EMPTY(mvalor),  ;
     ALLTRIM(mvalor), mvalor)
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
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = parma.codigo
mcuenta = parma.descriau2
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
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
SEEK ALLTRIM(filtro) + mvalor
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
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 6 SAY  ;
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
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_prv1
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong,  ;
          mdist
PRIVATE malias, mperiodo
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
SELECT auxil
SET ORDER TO auxil1
SEEK filtro + IIF( .NOT.  ;
     EMPTY(mvalor),  ;
     ALLTRIM(mvalor), mvalor)
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET ORDER TO auxil10
     SET FILTER TO tipo = filtro
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          SET ORDER TO auxil1
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
          ENDIF
          RETURN
     ENDIF
     ON KEY LABEL ENTER keyboard chr(23)
     ON KEY LABEL F2 do funbus1 
     DEFINE WINDOW _xx FROM 3, 02  ;
            TO 22, 60 COLOR  ;
            SCHEME 10
     BROWSE FIELDS codigo :H =  ;
            'C¢digo', descri :H =  ;
            'Descripci¢n' : 50  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            WINDOW _xx TITLE  ;
            ' ±± Cat logo de ' +  ;
            IIF(filtro = '20',  ;
            'Proveedores',  ;
            IIF(filtro = '30',  ;
            'Empleados',  ;
            'Otros')) +  ;
            ' ±±  ®F2¯ Busca '  ;
            NOLGRID
     ON KEY LABEL ENTER
     ON KEY LABEL f2
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = auxil.codigo
mdescr = SUBSTR(auxil.descri, 1,  ;
         mlong)
SET ORDER TO auxil1
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN
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
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_prgm
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong,  ;
          mdist
PRIVATE malias, mperiodo
mperiodo = m.periodo
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
SEEK mperiodo + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO periodo = mperiodo;
.AND. EMPTY(tipfun)
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
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = maepre.codprg + '    '
mdescr = SUBSTR(maepre.descri, 1,  ;
         mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
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
FUNCTION val_subp
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong
PRIVATE malias, mvalor, filtro
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
          ACTIVATE WINDOW &_oldwnd
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
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*
FUNCTION val_pyac
PARAMETER nvalor, nfiltro,  ;
          nvariable, ncol, nlong,  ;
          nancho
PRIVATE nalias
DO CASE
     CASE PARAMETERS() = 2
          ncol = 0
          nvariable = ' '
          nlong = 40
          nancho = 6
     CASE PARAMETERS() = 3
          ncol = 0
          nlong = 40
          nancho = 6
     CASE PARAMETERS() = 4
          nlong = 40
          nancho = 6
     CASE PARAMETERS() = 5
          nancho = 6
ENDCASE
nalias = ALIAS()
SELECT maepre
SET ORDER TO IIF(ALLTRIM(m.tipfun)='I',6,7)
SEEK ALLTRIM(m.tipfun) + nfiltro +  ;
     nvalor
IF  .NOT. FOUND() .OR.  .NOT.  ;
    nvariable $ 'V'
     SET FILTER TO periodo + codprg +;
codsubpr = nfiltro
     GOTO TOP
     IF  .NOT. EOF()
          IF  .NOT. EMPTY(nvalor)
               SEEK ALLTRIM(m.tipfun) +  ;
                    nfiltro +  ;
                    nvalor
               IF  .NOT. FOUND()
                    DO rolea
               ENDIF
          ELSE
               DO rolea
          ENDIF
     ELSE
          DO standby WITH  ;
             'Error en Codificaci¢n program tica'
          SET FILTER TO
          IF  .NOT. EMPTY(nalias)
               SELECT (nalias)
          ENDIF
          RETURN .F.
     ENDIF
ENDIF
nvalor = IIF(ALLTRIM(m.tipfun) =  ;
         'I', maepre.codproy,  ;
         maepre.codact)
ndescr = SUBSTR(maepre.descri, 1,  ;
         nlong)
SET FILTER TO
IF  .NOT. EMPTY(nalias)
     SELECT (nalias)
ENDIF
DO CASE
     CASE nvariable = ' '
          @ ROW(), ncol SAY  ;
            PADL(nvalor, nancho,  ;
            ' ')
          @ ROW(), ncol SAY  ;
            ndescr
          RETURN .T.
     CASE nvariable = 'A'
          @ ROW(), ncol SAY  ;
            ndescr
          RETURN
     CASE nvariable = 'V'
          @ ROW(), COL() SAY  ;
            PADR(nvalor, nancho,  ;
            ' ')
          RETURN ndescr
     CASE nvariable = 'D'
          RETURN ndescr
     CASE nvariable = 'Z'
          RETURN ndescr
     CASE nvariable = 'C'
          RETURN .T.
     OTHERWISE
          &nvariable = nvalor
          RETURN .T.
ENDCASE
*
PROCEDURE yrolea
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06, 10 TO  ;
       17, 69 FLOAT SHADOW DOUBLE  ;
       COLOR SCHEME 10
GOTO TOP
DO CASE
     CASE ALLTRIM(hoja.tipfun) =  ;
          'I' .AND.  .NOT. EOF()  ;
          .AND.  .NOT.  ;
          EMPTY(codproy)
          BROWSE FIELDS codproy  ;
                 :H = 'Pry',  ;
                 descri :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' PROYECTOS :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
     CASE ALLTRIM(hoja.tipfun) =  ;
          'F' .AND.  .NOT. EOF()  ;
          .AND.  .NOT.  ;
          EMPTY(codact)
          BROWSE FIELDS codact :H =  ;
                 'Act', descri :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' ACTIVIDAD :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
     OTHERWISE
          IF  .NOT. EMPTY(nvalor)
               DO standby WITH  ;
                  'No se tiene '+ ;
                  IIF(ALLTRIM(hoja.tipfun)= ;
                  'F',  ;
                  'Actividad',  ;
                  'Proyecto')+ ;
                  ' en referencia ---> '+ ;
                  m.tipfun
          ENDIF
ENDCASE
ON KEY LABEL ENTER
RELEASE WINDOW _xx
IF  .NOT. EMPTY(_oldwnd)
     ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN
*
PROCEDURE rolea
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06, 10 TO  ;
       17, 69 FLOAT SHADOW DOUBLE  ;
       COLOR SCHEME 10
GOTO TOP
DO CASE
     CASE ALLTRIM(m.tipfun) = 'I'  ;
          .AND.  .NOT. EOF()  ;
          .AND.  .NOT.  ;
          EMPTY(codproy)
          BROWSE FIELDS codproy  ;
                 :H = 'Pry',  ;
                 descri :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' PROYECTOS :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
     CASE ALLTRIM(m.tipfun) = 'F'  ;
          .AND.  .NOT. EOF()  ;
          .AND.  .NOT.  ;
          EMPTY(codact)
          BROWSE FIELDS codact :H =  ;
                 'Act', descri :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' ACTIVIDAD :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
     OTHERWISE
          IF  .NOT. EMPTY(nvalor)
               DO standby WITH  ;
                  'No se tiene '+ ;
                  IIF(ALLTRIM(m.tipfun)= ;
                  'F',  ;
                  'Actividad',  ;
                  'Proyecto')+ ;
                  ' en referencia ---'+ ;
                  m.tipfun
          ENDIF
ENDCASE
ON KEY LABEL ENTER
RELEASE WINDOW _xx
IF  .NOT. EMPTY(_oldwnd)
     ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN
*
FUNCTION val_supy
PARAMETER cvalor, cfiltro,  ;
          cvariable, ccol, clong,  ;
          cancho
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          ccol = 0
          cvariable = ' '
          clong = 40
          cancho = 6
     CASE PARAMETERS() = 3
          ccol = 0
          clong = 40
          cancho = 6
     CASE PARAMETERS() = 4
          clong = 40
          cancho = 6
     CASE PARAMETERS() = 5
          cancho = 6
ENDCASE
calias = ALIAS()
SELECT itepar
SET ORDER TO 8
SEEK cfiltro + cvalor
IF  .NOT. FOUND() .OR.  .NOT.  ;
    cvariable $ 'V'
     SET FILTER TO codfte + periodo +;
codprg + codsubpr + codproy + codsupry;
= ALLTRIM(m.codfte) + cfiltro
     GOTO TOP
     IF  .NOT. EMPTY(cvalor)  ;
         .AND.  .NOT. EOF()
          SEEK cfiltro + cvalor
          IF  .NOT. FOUND()
               DO rolea_1
          ENDIF
     ELSE
          DO rolea_1
     ENDIF
ENDIF
cvalor = itepar.codsupry
cdescr = SUBSTR(itepar.dessupry,  ;
         1, clong)
SET FILTER TO
IF  .NOT. EMPTY(calias)
     SELECT (calias)
ENDIF
DO CASE
     CASE cvariable = ' '
          @ ROW(), ccol SAY  ;
            PADR(cvalor, cancho,  ;
            ' ')
          @ ROW(), ccol SAY  ;
            cdescr
          RETURN .T.
     CASE cvariable = 'A'
          @ ROW(), ccol SAY  ;
            cdescr
          RETURN
     CASE cvariable = 'V'
          @ ROW(), COL() SAY  ;
            PADR(cvalor, cancho,  ;
            ' ')
          RETURN cdescr
     CASE cvariable = 'D'
          RETURN cdescr
     CASE cvariable = 'Z'
          RETURN cdescr
     CASE cvariable = 'C'
          RETURN .T.
     OTHERWISE
          &cvariable = cvalor
          RETURN .T.
ENDCASE
*
PROCEDURE rolea_1
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 03, 10 TO  ;
       20, 69 FLOAT SHADOW DOUBLE  ;
       COLOR SCHEME 10
GOTO TOP
DO CASE
     CASE  .NOT. EOF()
          BROWSE FIELDS codsupry  ;
                 :H = 'SubPry',  ;
                 codpart :H =  ;
                 'Partida',  ;
                 dessupry :H =  ;
                 'Detalle' NOMENU  ;
                 NOAPPEND NOEDIT  ;
                 NODELETE WINDOW  ;
                 _xx TITLE  ;
                 ' SUBPROYECTOS :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
     OTHERWISE
          IF  .NOT. EMPTY(cvalor)
               DO standby WITH  ;
                  'No se tiene Suproyecto en referencia'
          ENDIF
ENDCASE
ON KEY LABEL enter
RELEASE WINDOW _xx
IF  .NOT. EMPTY(_oldwnd)
     ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN
*
PROCEDURE val_afe
PARAMETER vcpt, vtot
PRIVATE osw, vret
vret = .T.
osw = ALIAS()
SELECT calen
vret = .T.
IF valpart < totafe + vtot
     IF  .NOT. yesno( ;
         'No existe Calendario para asignar. ¨Contin£a?' ;
         )
          SELECT (osw)
          REPLACE valpart WITH 0
          vret = .F.
     ENDIF
ENDIF
SELECT (osw)
RETURN
*
PROCEDURE xanalcal
IF ALLTRIM(m.tipfun) = 'F'
     DO rolea_cal WITH  ;
        maepre.periodo+ ;
        maepre.codprg+ ;
        maepre.codsubpr,  ;
        maepre.codact
ELSE
     DO rolea_cal WITH  ;
        itepar.periodo+ ;
        itepar.codprg+ ;
        itepar.codsubpr+ ;
        itepar.codproy,  ;
        itepar.codsupry
ENDIF
RETURN
*
PROCEDURE rolea_cal
PARAMETER xvalor, proact
IF yesno( ;
   ' ¨ Valida Calendario? ')
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     ON KEY LABEL enter KEYBOARD CHR(23)
     DEFINE WINDOW _yy FROM 04,  ;
            09 TO 20, 71 FLOAT  ;
            SHADOW DOUBLE COLOR  ;
            SCHEME 10
     SELECT calen
     SET ORDER TO IIF(ALLTRIM(m.tipfun)='I',6,5)
     IF ALLTRIM(m.tipfun) = 'I'
          SET FILTER TO nummes + tipfun;
+ codfte + periodo + codprg + codsubpr;
+ codproy + IIF(;
.NOT. EMPTY(proact), codsupry, ' ') =;
ALLTRIM(m.nummes) + ALLTRIM(m.tipfun);
+ ALLTRIM(m.codfte) + xvalor + IIF(;
.NOT. EMPTY(proact), proact, ' ')
     ELSE
          SET FILTER TO nummes + tipfun;
+ codfte + periodo + codprg + codsubpr;
+ IIF(;
.NOT. EMPTY(proact), codact, ' ') = ALLTRIM(m.nummes);
+ ALLTRIM(m.tipfun) + ALLTRIM(m.codfte);
+ xvalor + IIF(;
.NOT. EMPTY(proact), proact, ' ');
.AND. LEFT(codpart, 2) = IIF(ALIAS() =;
'ORDSE', '03', '02')
     ENDIF
     GOTO TOP
     IF  .NOT. EOF()
          BROWSE FIELDS xx =  ;
                 val_para(nummes, ;
                 'FECMES','D') :H =  ;
                 'Mes' : 10, vv =  ;
                 ' ' + codpart :H =  ;
                 'Partida',  ;
                 valpart :H =  ;
                 '     Asignado'  ;
                 :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 '     Afectado'  ;
                 :P =  ;
                 '99,999,999.99',  ;
                 zz = valpart -  ;
                 totafe :H =  ;
                 '       Saldo'  ;
                 :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW _yy TITLE  ;
                 ' CALENDARIOS :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
          m.nummes = calen.nummes
          m.codpart = calen.codpart
          vpart = calen.codpart
     ELSE
          DO standby WITH  ;
             'No se tiene Calendario para este Mes'
          IF ALLTRIM(m.tipfun) =  ;
             'I'
               SET FILTER TO tipfun +;
codfte + periodo + codprg + codsubpr +;
codproy + IIF(;
.NOT. EMPTY(proact), codsupry, ' ') =;
ALLTRIM(m.tipfun) + ALLTRIM(m.codfte);
+ xvalor + IIF(;
.NOT. EMPTY(proact), proact, ' ')
          ELSE
               SET FILTER TO tipfun +;
codfte + periodo + codprg + codsubpr +;
IIF(;
.NOT. EMPTY(proact), codact, ' ') = ALLTRIM(m.tipfun);
+ ALLTRIM(m.codfte) + xvalor + IIF(;
.NOT. EMPTY(proact), proact, ' ');
.AND. LEFT(codpart, 2) = IIF(ALIAS() =;
'ORDSE', '03', '02')
          ENDIF
          GOTO TOP
          BROWSE FIELDS xx =  ;
                 val_para(nummes, ;
                 'FECMES','D') :H =  ;
                 'Mes' : 10, vv =  ;
                 ' ' + codpart :H =  ;
                 'Partida',  ;
                 valpart :H =  ;
                 '     Asignado'  ;
                 :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 '     Afectado'  ;
                 :P =  ;
                 '99,999,999.99',  ;
                 zz = valpart -  ;
                 totafe :H =  ;
                 '       Saldo'  ;
                 :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW _yy TITLE  ;
                 ' CALENDARIOS :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
          m.nummes = calen.nummes
          m.codpart = calen.codpart
          vpart = calen.codpart
     ENDIF
     ON KEY LABEL enter
     RELEASE WINDOW _yy
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
ENDIF
RETURN
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
          SET FILTER TO periodo + codcad;
+ codfte = filtro
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
                 3, 02 TO 22, 77
          BROWSE FIELDS codprv :H =  ;
                 'C¢digo', nompro  ;
                 :H = 'Nombre' :  ;
                 50, dirpro :H =  ;
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
          _cod = '0000'
          @ 01, 02 SAY 'C¢digo: '  ;
            GET _cod PICTURE  ;
            '!!!!'
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
PROCEDURE funbus1
orden = ORDER()
ACTIVATE WINDOW _funbus
_cod = SPACE(40)
@ 01, 02 SAY 'Nombre: ' GET _cod  ;
  PICTURE '@S30'
READ
DEACTIVATE WINDOW _funbus
IF LASTKEY() <> 27
     SET ORDER TO auxil11
     SEEK UPPER(ALLTRIM(_cod))
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No existe...'
     ENDIF
ENDIF
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
          = INKEY(0.001 , ' ')
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
SET HOURS TO 12
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
DEFINE WINDOW espera FROM 12, 06  ;
       TO 14, 78 COLOR SCHEME 05
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
          ACTIVATE WINDOW &_oldwnd
     ELSE
          ACTIVATE SCREEN
     ENDIF
ENDIF
RETURN
*
FUNCTION veresthc
PARAMETER vest
PRIVATE vfun
vfun = SPACE(10)
DO CASE
     CASE vest = '00' .OR. vest =  ;
          '  '
          vfun = 'Emitido     '
     CASE vest = '20'
          vfun = 'Con Cta.Cte. '
     CASE vest = '52'
          vfun = 'Regularizado '
     CASE vest = '54'
          vfun = 'Con N/C '
     CASE vest = '50'
          vfun = 'C/P:' + m.numcp +  ;
                 '.' + m.nummescp +  ;
                 '.' +  ;
                 ALLTRIM(m.codctc)
     CASE vest = '70'
          vfun = 'P/A:' +  ;
                 m.numanu
     CASE vest = '80'
          vfun = 'P/R:' +  ;
                 m.numreb
     CASE vest = '90'
          vfun = 'Con H/Modifc'
     CASE vest = '99'
          vfun = 'Anulado     '
ENDCASE
RETURN vfun
*
FUNCTION veresthm
PARAMETER vest
PRIVATE vfun
vfun = SPACE(10)
DO CASE
     CASE vest = '00' .OR. vest =  ;
          '  '
          vfun = 'Emitido     '
     CASE vest = '51'
          vfun = 'Contabilizado'
     CASE vest = '99'
          vfun = 'Anulado     '
     CASE vest = '50'
          vfun = 'C/P:' + m.numcp +  ;
                 '.' + m.nummescp +  ;
                 ' ' +  ;
                 ALLTRIM(m.codctc)
ENDCASE
RETURN vfun
*
FUNCTION vestoc
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
     CASE vest = '00'
          vfun = 'Emitido      '
     CASE vest = '20'
          vfun = 'Afectado     '
     CASE vest = '40'
          vfun = 'En Almac‚n   '
     CASE vest = '50'
          vfun = 'Liquidado    '
     CASE vest = '51'
          vfun = 'Contabilizado'
     CASE vest = '90'
          vfun = 'Con Pte Anulc'
     CASE vest = '95'
          vfun = 'Con Pte Rebaj'
     CASE vest = '99'
          vfun = 'Anulado      '
ENDCASE
RETURN vfun
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
*
FUNCTION val_obra
PARAMETER cvalor, cfiltro,  ;
          cvariable, ccol, clong,  ;
          cancho
PRIVATE malias, vfun
DO CASE
     CASE PARAMETERS() = 2
          ccol = 0
          cvariable = ' '
          clong = 40
          cancho = 6
     CASE PARAMETERS() = 3
          ccol = 0
          clong = 40
          cancho = 6
     CASE PARAMETERS() = 4
          clong = 40
          cancho = 6
     CASE PARAMETERS() = 5
          cancho = 6
ENDCASE
calias = ALIAS()
SELECT obra
ok = .F.
vfun = .F.
IF  .NOT. EMPTY(cvalor)
     SET ORDER TO OBRA1
     SEEK cfiltro +  ;
          ALLTRIM(cvalor)
     ok = FOUND()
     vfun = FOUND()
ENDIF
IF  .NOT. ok
     SET ORDER TO 1
     SEEK cfiltro +  ;
          ALLTRIM(cvalor)
     IF  .NOT. FOUND() .OR.   ;
         .NOT. cvariable $ 'V'
          SET FILTER TO periodo + codprg;
+ codsubpr + codproy + codsupry = ALLTRIM(cfiltro)
          GOTO TOP
          IF  .NOT. EOF()
               IF  .NOT.  ;
                   EMPTY(cvalor)  ;
                   .AND.  .NOT.  ;
                   EOF()
                    SEEK cfiltro +  ;
                         cvalor
                    IF  .NOT.  ;
                        FOUND()
                         DO rolea_ob
                    ENDIF
               ELSE
                    DO rolea_ob
               ENDIF
               vfun = .T.
          ENDIF
     ENDIF
ENDIF
IF vfun
     cvalor = obra.codobra
     cdescr = SUBSTR(obra.descri,  ;
              1, clong)
     vcodprg = obra.codprg
     vcodsub = obra.codsubpr
     vproyec = obra.codproy
     vsubpry = obra.codsupry
ELSE
     m.codobra = SPACE(4)
     cvalor = '    '
     cdescr = '**'
ENDIF
SET FILTER TO
IF  .NOT. EMPTY(calias)
     SELECT (calias)
ENDIF
DO CASE
     CASE cvariable = ' '
          @ ROW(), ccol SAY  ;
            PADR(cvalor, cancho,  ;
            ' ')
          @ ROW(), ccol SAY  ;
            cdescr
          RETURN .T.
     CASE cvariable = 'A'
          @ ROW(), ccol SAY  ;
            cdescr
          RETURN
     CASE cvariable = 'V'
          @ ROW(), COL() SAY  ;
            PADR(cvalor, cancho,  ;
            ' ')
          RETURN cdescr
     CASE cvariable = 'D'
          RETURN cdescr
     CASE cvariable = 'Z'
          RETURN cdescr
     CASE cvariable = 'C'
          RETURN .T.
     OTHERWISE
          &cvariable = cvalor
          RETURN .T.
ENDCASE
*
PROCEDURE rolea_ob
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 03, 10 TO  ;
       20, 69 FLOAT SHADOW DOUBLE  ;
       COLOR SCHEME 10
GOTO TOP
DO CASE
     CASE a = 'A'
          BROWSE FIELDS numobra  ;
                 :H = 'Obr',  ;
                 codobra :H =  ;
                 'Cont', descri  ;
                 :H = 'Detalle'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 WINDOW _xx TITLE  ;
                 ' OBRAS :  ®Enter¯  Selecciona  '  ;
                 NOLGRID
          valobra = .T.
     OTHERWISE
          IF  .NOT. EMPTY(cvalor)
               DO standby WITH  ;
                  'No se tiene Obra en referencia'
          ENDIF
          valobra = .F.
ENDCASE
ON KEY LABEL enter
RELEASE WINDOW _xx
IF  .NOT. EMPTY(_oldwnd)
     ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN
*
FUNCTION cresup
PRIVATE vkey, valias
valias = ALIAS()
vcodpart = codpart
vkey = IIF(ALLTRIM(vtipfun) = 'I',  ;
       codprg + codfte + IIF(  ;
       .NOT. EMPTY(vcodsub),  ;
       codsubpr, '') + IIF( .NOT.  ;
       EMPTY(vcodpry), codproy,  ;
       '') + IIF( .NOT.  ;
       EMPTY(vcodspy), codsupry,  ;
       ''), codprg + codfte +  ;
       IIF( .NOT. EMPTY(vcodsub),  ;
       codsubpr, '') + IIF( .NOT.  ;
       EMPTY(vcodact), codact,  ;
       ''))
SELECT itecre
SET ORDER TO itecre1
SET RELATION TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) + ALLTRIM(tipfun) INTO;
cresup
GOTO TOP
IF RIGHT(vcodpart, 2) = '00'
     SUM FOR LEFT(codpart, 2) =  ;
         LEFT(vcodpart, 2) .AND.  ;
         RIGHT(codpart, 2) <>  ;
         '00' .AND. MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodpry), codproy,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodspy), codsupry,  ;
         '') = vkey, codprg +  ;
         codfte + IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodact), codact,  ;
         '') = vkey) .AND.  ;
         cresup.estado = '10'  ;
         monasig TO vcresup
ELSE
     SUM FOR codpart = vcodpart  ;
         .AND. MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodpry), codproy,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodspy), codsupry,  ;
         '') = vkey, codprg +  ;
         codfte + IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodact), codact,  ;
         '') = vkey) .AND.  ;
         cresup.estado = '10'  ;
         monasig TO vcresup
ENDIF
SET RELATION TO
SELECT itetra
SET ORDER TO itetra1
SET RELATION TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) + ALLTRIM(tipfun) INTO;
trapar
GOTO TOP
vtransf = 0
IF RIGHT(vcodpart, 2) = '00'
     SUM FOR MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodpry), codproy,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodspy), codsupry,  ;
         '') = vkey, codprg +  ;
         codfte + IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodact), codact,  ;
         '') = vkey) .AND.  ;
         trapar.estado = '10'  ;
         .AND. LEFT(codpart, 2) =  ;
         LEFT(vcodpart, 2) .AND.  ;
         RIGHT(codpart, 2) <>  ;
         '00' montra * IIF(tipope =  ;
         '-', -1, 1) TO vtransf
ELSE
     SUM FOR MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodpry), codproy,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodspy), codsupry,  ;
         '') = vkey, codprg +  ;
         codfte + IIF( .NOT.  ;
         EMPTY(vcodsub), codsubpr,  ;
         '') + IIF( .NOT.  ;
         EMPTY(vcodact), codact,  ;
         '') = vkey) .AND.  ;
         trapar.estado = '10'  ;
         .AND. codpart = vcodpart  ;
         montra * IIF(tipope =  ;
         '-', -1, 1) TO vtransf
ENDIF
vtotalct = vcresup + vtransf
SELECT (valias)
RETURN vtotalct
*
FUNCTION cresup_i
PRIVATE vkey, valias
valias = ALIAS()
vcodpart = codpart
vkey = IIF(ALLTRIM(vtipfun) = 'I',  ;
       codprg + codfte + codsubpr +  ;
       codproy + codsupry, codprg +  ;
       codfte + codsubpr +  ;
       codact)
SELECT itecre
SET ORDER TO ITECRE1
SET RELATION TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) + ALLTRIM(tipfun) INTO;
cresup
GOTO TOP
IF RIGHT(vcodpart, 2) = '00'
     SUM FOR LEFT(codpart, 2) =  ;
         LEFT(vcodpart, 2) .AND.  ;
         RIGHT(codpart, 2) <>  ;
         '00' .AND. MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         codsubpr + codproy +  ;
         codsupry = vkey, codprg +  ;
         codfte + codsubpr +  ;
         codact = vkey) .AND.  ;
         cresup.estado = '10'  ;
         monasig TO vcresup
ELSE
     SUM FOR codpart = vcodpart  ;
         .AND. MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         codsubpr + codproy +  ;
         codsupry = vkey, codprg +  ;
         codfte + codsubpr +  ;
         codact = vkey) .AND.  ;
         cresup.estado = '10'  ;
         monasig TO vcresup
ENDIF
SELECT itetra
SET ORDER TO ITETRA1
SET RELATION TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) + ALLTRIM(tipfun) INTO;
trapar
SELECT itetra
GOTO TOP
vtransf = 0
IF RIGHT(vcodpart, 2) = '00'
     SUM FOR MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         codsubpr + codproy +  ;
         codsupry = vkey, codprg +  ;
         codfte + codsubpr +  ;
         codact = vkey) .AND.  ;
         trapar.estado = '10'  ;
         .AND. LEFT(codpart, 2) =  ;
         LEFT(vcodpart, 2) .AND.  ;
         RIGHT(codpart, 2) <>  ;
         '00' montra * IIF(tipope =  ;
         '-', -1, 1) TO vtransf
ELSE
     SUM FOR MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         IIF(ALLTRIM(vtipfun) =  ;
         'I', codprg + codfte +  ;
         codsubpr + codproy +  ;
         codsupry = vkey, codprg +  ;
         codfte + codsubpr +  ;
         codact = vkey) .AND.  ;
         trapar.estado = '10'  ;
         .AND. codpart = vcodpart  ;
         montra * IIF(tipope =  ;
         '-', -1, 1) TO vtransf
ENDIF
vtotalct = vcresup + vtransf
SELECT (valias)
RETURN vtotalct
*
FUNCTION cresup_v
PRIVATE vkey, valias
valias = ALIAS()
vcodpart = codpart
vkey = codprg + codfte
SELECT itecre
SET ORDER TO itecre1
SET RELATION TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) + ALLTRIM(tipfun) INTO;
cresup
GOTO TOP
IF RIGHT(vcodpart, 2) = '00'
     SUM FOR LEFT(codpart, 2) =  ;
         LEFT(vcodpart, 2) .AND.  ;
         RIGHT(codpart, 2) <>  ;
         '00' .AND. MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         codprg + codfte = vkey  ;
         .AND. cresup.estado =  ;
         '10' monasig TO vcresup
ELSE
     SUM FOR codpart = vcodpart  ;
         .AND. MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         codprg + codfte = vkey  ;
         .AND. cresup.estado =  ;
         '10' monasig TO vcresup
ENDIF
SET RELATION TO
SELECT itetra
SET ORDER TO itetra1
SET RELATION TO periodo + ALLTRIM(tipdoc);
+ ALLTRIM(numdoc) + ALLTRIM(tipfun) INTO;
trapar
GOTO TOP
vtransf = 0
IF RIGHT(vcodpart, 2) = '00'
     SUM FOR MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         codprg + codfte = vkey  ;
         .AND. trapar.estado =  ;
         '10' .AND. LEFT(codpart,  ;
         2) = LEFT(vcodpart, 2)  ;
         .AND. RIGHT(codpart, 2) <>  ;
         '00' IIF(tipope = '-', - ;
         1, 1) * montra TO  ;
         vtransf
ELSE
     SUM FOR MONTH(fecha) <=  ;
         VAL(vnummes) .AND.  ;
         codprg + codfte = vkey  ;
         .AND. trapar.estado =  ;
         '10' .AND. codpart =  ;
         vcodpart IIF(tipope =  ;
         '-', -1, 1) * montra TO  ;
         vtransf
ENDIF
vtotalct = vcresup + vtransf
SELECT (valias)
RETURN vtotalct
*
FUNCTION val_auxf
PARAMETER mvalor, mvariable, mcol,  ;
          mlong, mdist
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 1
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 2
          mcol = 0
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 3
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 4
          mdist = 6
ENDCASE
malias = ALIAS()
morden = ORDER()
SELECT auxil
SEEK mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     IF  .NOT. EMPTY(LEFT(mvalor,  ;
         2))
          SET FILTER TO tipo = LEFT(mvalor,;
2)
     ENDIF
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
               SET ORDER TO (morden)
          ENDIF
          RETURN
     ENDIF
     SET ORDER TO Auxil10
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     SELECT auxil
     SET ORDER TO Auxil1
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = auxil.tipo + auxi.codigo
mdescr = SUBSTR(auxil.descri, 1,  ;
         mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
     SET ORDER TO (morden)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
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
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*
FUNCTION val_aux
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
morden = ORDER()
SELECT auxil
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     IF  .NOT. EMPTY(filtro)
          SET FILTER TO tipo = filtro
     ENDIF
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
               SET ORDER TO (morden)
          ENDIF
          RETURN
     ENDIF
     SET ORDER TO Auxil10
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     SELECT auxil
     SET ORDER TO Auxil1
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = auxil.codigo
mdescr = SUBSTR(auxil.descri, 1,  ;
         mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
     SET ORDER TO (morden)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
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
          RETURN .T.
ENDCASE
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
                  WITH vllave
          UNLOCK
     ENDIF
     vusurec = RECNO()
ELSE
     GOTO vusurec
     REPLACE salida WITH _hora
ENDIF
USE
RETURN
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
REPLACE llave WITH vllave
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
            vllave
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
FUNCTION verestch
PARAMETER vest
PRIVATE vfun
vfun = SPACE(10)
DO CASE
     CASE vest == '00'
          vfun = 'Cheque Girado'
     CASE vest == '10'
          vfun = 'Cheque Autorizado'
     CASE vest == '40'
          vfun = 'Cheque Entregado'
     CASE vest == '99'
          vfun = 'Anulado'
     OTHERWISE
          vfun = 'Sin Cheque'
ENDCASE
RETURN vfun
*
FUNCTION val_auxi
PARAMETER mvalor, filtro,  ;
          mvariable, mcol
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 10
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
          mdist = 10
     CASE PARAMETERS() = 4
          mlong = 30
          mtip = 24
          mdist = 10
     CASE PARAMETERS() = 5
          mdist = 10
ENDCASE
malias = ALIAS()
velig = .F.
SELECT auxil
SEEK ALLTRIM(filtro) + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     SET FILTER TO tipo = ALLTRIM(filtro)
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
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
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     mtipo = auxil.tipo
     mvalor = auxil.codigo
     mdescr = SUBSTR(auxil.descri,  ;
              1, mlong)
     velig = .T.
ENDIF
IF velig = .F.
     mtipo = auxil.tipo
     mvalor = auxil.codigo
     mdescr = SUBSTR(auxil.descri,  ;
              1, mlong)
ENDIF
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable == 'A'
          m.codprv = mvalor
          m.tipaux = mtipo
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
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
          REPACE &mVaRiable WITH mValor
          RETURN .T.
ENDCASE
*
FUNCTION val_pret
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
SELECT clase
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     IF  .NOT. EMPTY(filtro)
          SET FILTER TO codpart = filtro
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
            ALLTRIM(codpart) +  ;
            '.' +  ;
            ALLTRIM(codsubp) +  ;
            ' ' + SUBSTR(despar,  ;
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
mvalor = ALLTRIM(clase.codpart) +  ;
         '.' + clase.codsubp
mdescr = SUBSTR(clase.despar, 1,  ;
         mlong)
m.partret = mvalor
SET ORDER TO 1
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
ENDCASE
*
FUNCTION val_parad
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
     SET ORDER TO PARMAE2
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
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
     SET ORDER TO PARMAE1
ENDIF
mvalor = parma.codigo
mcuenta = parma.descriau2
mdescr = SUBSTR(parma.descri, 1,  ;
         mlong)
mdescriaux = SUBSTR(parma.descriaux,  ;
             1, mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable = 'A'
          @ ROW(), mcol SAY  ;
            mdescr
          RETURN ' '
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
FUNCTION letras1
PARAMETER num, simbolo
PRIVATE cad1
cad1 = ''
IF num = 0
     RETURN (cad1)
ENDIF
deci = INT(MOD(num, 1) * 100)
cmm = INT(num / 1000000)
cmu = MOD(num, 1000000)
cmu = INT(cmu)
DO CASE
     CASE (cmm = 1)
          cad1 = ' UN MILLON'
     CASE (cmm = 0)
          cad1 = ''
     CASE (cmm > 1)
          cad1 = num6(cmm) +  ;
                 ' MILLONES'
     OTHERWISE
          cad1 = '*****'
ENDCASE
cad1 = cad1 + num6(cmu)
cad1 = cad1 + ' Y ' +  ;
       PADL(ALLTRIM(STR(deci, 2)),  ;
       2, '0') + '/100 ' +  ;
       simbolo
RETURN (cad1)
*
FUNCTION agr2det
DO standby WITH  ;
   'Consulte con almacen,para a¤adir nuevo C¢digo'
RETURN .T.
*
FUNCTION num6
PARAMETER cm
PRIVATE cad2
cmx = INT(cm / 1000)
cuu = MOD(cm, 1000)
cad2 = ''
DO CASE
     CASE (cmx = 1)
          cad2 = ' MIL'
     CASE (cmx = 0)
          cad2 = ''
     CASE (cmx > 1)
          cad2 = num3(cmx) +  ;
                 ' MIL'
     OTHERWISE
          cad2 = '****'
ENDCASE
cad2 = cad2 + num3(cuu)
RETURN (cad2)
*
FUNCTION num3
PARAMETER cu
DIMENSION centena( 9)
PRIVATE cad3
centena( 1) = ' CIENTO'
centena( 2) = ' DOSCIENTOS'
centena( 3) = ' TRECIENTOS'
centena( 4) = ' CUATROCIENTOS'
centena( 5) = ' QUINIENTOS'
centena( 6) = ' SEISCIENTOS'
centena( 7) = ' SETECIENTOS'
centena( 8) = ' OCHOCIENTOS'
centena( 9) = ' NOVECIENTOS'
cad3 = ''
ce = INT(cu / 100)
de = MOD(cu, 100)
IF (ce = 1 .AND. de = 0)
     cad3 = ' CIEN'
ELSE
     IF ce <> 0
          cad3 = centena(ce)
     ENDIF
     IF de <> 0
          cad3 = cad3 + num2(de)
     ENDIF
ENDIF
RETURN (cad3)
*
FUNCTION num2
PARAMETER du
PRIVATE cad2
DIMENSION dece( 7)
DIMENSION unidad( 9)
DIMENSION unid( 6)
dece( 1) = ' TREINTA'
dece( 2) = ' CUARENTA'
dece( 3) = ' CINCUENTA'
dece( 4) = ' SESENTA'
dece( 5) = ' SETENTA'
dece( 6) = ' OCHENTA'
dece( 7) = ' NOVENTA'
unidad( 1) = ' UN'
unidad( 2) = ' DOS'
unidad( 3) = ' TRES'
unidad( 4) = ' CUATRO'
unidad( 5) = ' CINCO'
unidad( 6) = ' SEIS'
unidad( 7) = ' SIETE'
unidad( 8) = ' OCHO'
unidad( 9) = ' NUEVE'
unid( 1) = ' DIEZ'
unid( 2) = ' ONCE'
unid( 3) = ' DOCE'
unid( 4) = ' TRECE'
unid( 5) = ' CATORCE'
unid( 6) = ' QUINCE'
cad2 = ''
d = INT(du / 10)
u = MOD(du, 10)
DO CASE
     CASE (d >= 3 .AND. d <= 9)
          IF (u = 0)
               cad2 = dece(d - 2)
          ELSE
               cad2 = dece(d - 2) +  ;
                      'I' +  ;
                      LTRIM(unidad(u))
          ENDIF
     CASE (d = 2)
          IF (u = 0)
               cad2 = ' VEINTE'
          ELSE
               cad2 = ' VEINTI' +  ;
                      LTRIM(unidad(u))
          ENDIF
     CASE (d = 1)
          DO CASE
               CASE (u <= 5)
                    cad2 = LTRIM(unid(u +  ;
                           1))
               CASE (u >= 6)
                    cad2 = ' DIECI' +  ;
                           LTRIM(unidad(u))
               OTHERWISE
                    cad2 = '****'
          ENDCASE
     CASE (d = 0)
          IF (u <> 0)
               cad2 = ' ' +  ;
                      LTRIM(unidad(u))
          ENDIF
     OTHERWISE
          cad2 = '****'
ENDCASE
RETURN (cad2)
*
FUNCTION val_auxid
PARAMETER mvalor, filtro,  ;
          mvariable, mcol
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 10
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
          mdist = 10
     CASE PARAMETERS() = 4
          mlong = 30
          mtip = 24
          mdist = 10
     CASE PARAMETERS() = 5
          mdist = 10
ENDCASE
malias = ALIAS()
velig = .F.
SELECT auxil
SEEK ALLTRIM(filtro) + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     SET FILTER TO tipo = ALLTRIM(filtro)
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
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
     SET ORDER TO auxil10
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_OldWnd
     ENDIF
     RELEASE POPUP parametro
     mtipo = auxil.tipo
     mvalor = auxil.codigo
     mdescr = SUBSTR(auxil.descri,  ;
              1, mlong)
     velig = .T.
     SET ORDER TO Auxil1
ENDIF
IF velig = .F.
     mtipo = auxil.tipo
     mvalor = auxil.codigo
     mdescr = SUBSTR(auxil.descri,  ;
              1, mlong)
ENDIF
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable = ' '
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     CASE mvariable = 'A'
          m.codprv = mvalor
          m.tipaux = mtipo
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
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
          REPACE &mVaRiable WITH mValor
          RETURN .T.
ENDCASE
*
PROCEDURE espera
PARAMETER festado, fmensaje
DEFINE WINDOW espera FROM 11, 05  ;
       TO 13, 75 COLOR SCHEME 5
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
PROCEDURE espera1
PARAMETER festado, fmensaje
IF PARAMETERS() = 1
     fmensaje = 'Espere Un Momento...'
ENDIF
DEFINE WINDOW espera FROM 21, 79 -  ;
       LEN(ALLTRIM(fmensaje)) - 1  ;
       TO 23, 79 COLOR SCHEME 5
DO CASE
     CASE festado = 1
          ACTIVATE WINDOW espera
          @ 0, 0 SAY  ;
            ALLTRIM(fmensaje)  ;
            COLOR W+/RB* 
          RETURN
     CASE festado = 2
          DEACTIVATE WINDOW  ;
                     espera
          RETURN
ENDCASE
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
IF vsistema = '1'
     SET FILTER TO maepre.periodo + maepre.uniges;
+ maepre.unieje = filtro + '01001'
     SEEK filtro + '01001' +  ;
          mvalor
ELSE
     SET FILTER TO LEFT(codcad, 1) = 'C';
.OR. (uniges = '01';
.AND. unieje = '001';
.AND. codfun = '05';
.AND. codprg = '024';
.AND. codspr = '0066')
     SEEK filtro + mvalor
ENDIF
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd1 = WOUTPUT()
     ACTIVATE SCREEN
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
            uniges :H = 'GEST.',  ;
            unieje :H = 'EJEC.',  ;
            codfun :H = 'Fn',  ;
            codprg :H = 'Prg',  ;
            codspr :H = 'SPrg',  ;
            actpry :H = 'Act/Pry',  ;
            codcom :H = 'CodComp',  ;
            xx = IIF(VAL(codcom) >  ;
            0, val_para(codcom, ;
            'CODCOM','D'), '') :H =  ;
            'Descripci¢n' : 25,  ;
            codmet :H = 'Meta',  ;
            descri :H =  ;
            'Descripci¢n' : 20  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relaci¢n de Cadenas Funcionales '  ;
            IN wind_cad  ;
            NOREFRESH
     RELEASE WINDOW wind_cad
     ON KEY LABEL f2
     ON KEY LABEL f3
     IF  .NOT. EMPTY(_oldwnd1)
          ACTIVATE WINDOW &_OldWnd1
     ENDIF
ENDIF
SET FILTER TO
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
DEFINE WINDOW lista FROM 09, 12  ;
       TO 16, 68 FLOAT TITLE  ;
       ' °° B£squeda X C¢digo °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lista
vcodcad = SPACE(4)
@ 3, 2 SAY '  Cadena : ' GET  ;
  vcodcad PICTURE '!!!!'
READ
RELEASE WINDOW lista
SEEK filtro + vcodcad
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No existe Codigo de Cadena...'
     GOTO vcursor
ENDIF
RETURN
*
PROCEDURE busdescad
vcursor = RECNO()
vorder = ORDER()
DEFINE WINDOW lista FROM 09, 12  ;
       TO 16, 68 FLOAT TITLE  ;
       ' °° B£squeda °° ' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW lista
vcodcom = SPACE(5)
@ 3, 2 SAY ' Componente : ' GET  ;
  vcodcom PICTURE '!!!!!' VALID  ;
  val_para(vcodcom,'CODCOM',' ', ;
  18,30)
READ
RELEASE WINDOW lista
LOCATE FOR codcom =  ;
       ALLTRIM(vcodcom)
IF  .NOT. FOUND()
     DO standby WITH  ;
        'No existe Codigo de Componente...'
     GOTO vcursor
ENDIF
RETURN
*
FUNCTION val_comp
PARAMETER vfiltro, vbusca,  ;
          mvariable
valias = ALIAS()
SELECT maepre
vrecno = RECNO()
vorder = ORDER()
IF sistema = '2'
     SET ORDER TO MAEPRE1
ENDIF
SET FILTER TO periodo + uniges + unieje;
+ codcad = vfiltro
SEEK vfiltro
vbusca1 = periodo + uniges +  ;
          unieje + codfun +  ;
          codprg + codspr +  ;
          actpry
SET ORDER TO maepre4
SEEK vbusca1 + vbusca
IF  .NOT. FOUND()
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SEEK vbusca1
     vkey = periodo + uniges +  ;
            unieje + codfun +  ;
            codprg + codspr +  ;
            actpry
     GOTO TOP
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad FROM  ;
            05, 20 TO 15, 60  ;
            TITLE  ;
            ' ± Componentes ± '  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad
     BROWSE NOOPTIMIZE FIELDS  ;
            codcom :H = 'CodComp',  ;
            xx = val_para(codcom, ;
            'CODCOM','D') :H =  ;
            'Descripci¢n' : 25  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relaci¢n de Cadenas Funcionales '  ;
            IN wind_cad  ;
            NOREFRESH
     RELEASE WINDOW wind_cad
     SET FILTER TO
ENDIF
mvalor = maepre.codcom
mdescr = val_para(maepre.codcom, ;
         'CODCOM','D')
IF sistema = '2'
     SET ORDER TO maepre3
ELSE
     SET ORDER TO maepre1
ENDIF
SET FILTER TO
GOTO vrecno
IF  .NOT. EMPTY(valias)
     SELECT (valias)
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
RETURN
*
FUNCTION val_meta
PARAMETER vfiltro, vbusca,  ;
          mvariable
valias = ALIAS()
SELECT maepre
vrecno = RECNO()
IF sistema = '2'
     SET ORDER TO MAEPRE1
ENDIF
SET FILTER TO periodo + uniges + unieje;
+ codcad = vfiltro
SEEK vfiltro
vbusca1 = periodo + uniges +  ;
          unieje + codfun +  ;
          codprg + codspr +  ;
          actpry
SET ORDER TO maepre4
SEEK vbusca1 + vbusca
IF  .NOT. FOUND()
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SEEK vbusca1
     vord1 = periodo + uniges +  ;
             unieje + codfun +  ;
             codprg + codspr +  ;
             actpry + codcom
     GOTO TOP
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad1 FROM  ;
            05, 20 TO 15, 60  ;
            TITLE ' ± Metas ± '  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad1
     BROWSE NOOPTIMIZE FIELDS  ;
            codmet :H = 'META',  ;
            descri :H =  ;
            'Descripci¢n' : 30  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR TITLE  ;
            'Relaci¢n de Cadenas Funcionales '  ;
            IN wind_cad1  ;
            NOREFRESH
     RELEASE WINDOW wind_cad1
     SET FILTER TO
ENDIF
SET FILTER TO
mvalor = maepre.codmet
mdescr = maepre.descri
IF sistema = '2'
     SET ORDER TO maepre3
ELSE
     SET ORDER TO maepre1
ENDIF
GOTO vrecno
IF  .NOT. EMPTY(valias)
     SELECT (valias)
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
     CASE mvariable = 'V'
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
RETURN
*
FUNCTION val_cla
PARAMETER vfiltro, vbusca,  ;
          mvariable
valias = ALIAS()
SELECT itecla
vrecno = RECNO()
vorder = ORDER()
SET ORDER TO ITECLA1
SEEK vbusca
IF  .NOT. FOUND()
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO codpart = vfiltro
     GOTO TOP
     ckeyf5 = ON('KEY', 'F5')
     ON KEY LABEL F5 DO Agr_Enl
     ON KEY LABEL f10 KEYBOARD CHR(23)
     DEFINE WINDOW wind_cad FROM  ;
            05, 10 TO 15, 70  ;
            TITLE ' ± CLASE ± '  ;
            FOOTER  ;
            ' ° ®F10¯ Graba ° '  ;
            DOUBLE COLOR SCHEME  ;
            15
     ACTIVATE WINDOW wind_cad
     BROWSE NOOPTIMIZE FIELDS  ;
            codcla :H = 'CLASE',  ;
            descri :H =  ;
            'Descripci¢n' : 20,  ;
            cuentad :H = 'Debe',  ;
            cuentah :H = 'Haber'  ;
            NOMENU NOAPPEND  ;
            NOEDIT NODELETE  ;
            NOCLEAR WINDOW  ;
            wind_cad NOREFRESH
     RELEASE WINDOW wind_cad
     SET FILTER TO
     ON KEY LABEL F5 &cKeyF5
ENDIF
mvalor = itecla.codcla
IF  .NOT. EMPTY(valias)
     SELECT (valias)
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
RETURN
*
PROCEDURE agr_enl
PRIVATE malias, m.codcla,  ;
        m.descri, m.cuentad,  ;
        m.cuentah, tipdoc
ON KEY LABEL F5
malias = ALIAS()
SELECT cuenta
ordcta = ORDER()
SET ORDER TO Cuentas4
SELECT itecla
DEFINE WINDOW w_cli FROM 05, 05  ;
       TO 20, 70 TITLE  ;
       'Ingresando Cliente'  ;
       DOUBLE COLOR SCHEME 5
m.codpart = itehc.codpart
m.codcla = correnl(m.codpart)
m.descri = SPACE(40)
m.cuentad = SPACE(15)
m.cuentah = SPACE(15)
m.tipdoc = 'OTR'
ACTIVATE WINDOW w_cli
@ 01, 1 SAY '      Partida: ' +  ;
  m.codpart
@ 02, 1 SAY '       Enlace: ' +  ;
  m.codcla
@ 04, 1 SAY '  Descripci¢n:' GET  ;
  m.descri FUNCTION '!S30'
@ 06, 1 SAY '  Cuenta Debe:' GET  ;
  m.cuentad PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.cuentad,6,16)
@ 08, 1 SAY ' Cuenta Haber:' GET  ;
  m.cuentah PICTURE  ;
  '999999999999999' VALID  ;
  val_cta(m.cuentah,8,16)
READ VALID val_read()
DEACTIVATE WINDOW w_cli
RELEASE WINDOW w_cli
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
          REPLACE itecla.tipdoc  ;
                  WITH 'OTR'
     ENDIF
ENDIF
SELECT cuenta
SET ORDER TO &OrdCta
SELECT (malias)
ON KEY LABEL F5 DO Agr_Enl
RETURN
*
FUNCTION val_cta
PARAMETER xcta, xfil, xcol
PRIVATE calias
calias = ALIAS()
IF EMPTY(xcta)
ELSE
     SELECT cuenta
     IF  .NOT. SEEK(xcta)
          xcta = SPACE(15)
     ENDIF
     = val_fun('Cuenta','Cuenta', ;
       "Cuenta+' '+Descri",xcta,1, ;
       xfil,xcol)
ENDIF
SELECT (calias)
RETURN .T.
*
FUNCTION correnl
PARAMETER xpart
IF SEEK(xpart)
     SCAN WHILE codpart = xpart
          mret = codcla
     ENDSCAN
     mret = PADL(ALLTRIM(STR(VAL(mret) +  ;
            1)), 2, '0')
ELSE
     mret = '01'
ENDIF
RETURN mret
*
FUNCTION val_cale
PARAMETER mperiodo, mnummes
PRIVATE malias
DEFINE WINDOW xwait FROM 20, 06  ;
       TO 22, 78 COLOR SCHEME 05
ACTIVATE WINDOW xwait
@ 0, 10 SAY  ;
  ' Verificando saldos de partidas(techos) '  ;
  COLOR W+/RB* 
malias = ALIAS()
zdbf = SYS(3) + '.DBF'
SELECT calen
SET ORDER TO CALEN4
COPY TO (zdbf) FOR calen.periodo +  ;
     calen.nummes + calen.uniges +  ;
     calen.unieje = mperiodo +  ;
     mnummes + '01001'
vdbf = SYS(3) + '.DBF'
COPY TO (vdbf) STRUCTURE
vind = SYS(3) + '.IDX'
zind = SYS(3) + '.IDX'
yind = SYS(3) + '.IDX'
USE
USE IN 13 (vdbf) ALIAS calen1
USE IN 0 (zdbf) ALIAS calen2
SELECT itehc
INDEX ON codpart TO (yind) FOR  ;
      nummes = vnummes .AND.  ;
      estado <> '99' .AND. uniges +  ;
      unieje = '01001'
GOTO TOP
SCAN
     SELECT maepre
     SEEK mperiodo + itehc.uniges +  ;
          itehc.unieje +  ;
          itehc.codcad
     vkey = uniges + unieje +  ;
            codfun + codprg +  ;
            codspr + actpry +  ;
            itehc.codcom +  ;
            itehc.codmet +  ;
            itehc.codpart +  ;
            itehc.codfte
     SELECT calen2
     LOCATE FOR LEFT(estfun, 30) +  ;
            codpart + codfte =  ;
            vkey
     IF FOUND()
          REPLACE totafe WITH  ;
                  totafe +  ;
                  IIF(itehc.tipope =  ;
                  '-',  ;
                  itehc.valpart * - ;
                  1,  ;
                  itehc.valpart)
     ELSE
          APPEND BLANK
          REPLACE codpart WITH  ;
                  itehc.codpart,  ;
                  periodo WITH  ;
                  m.periodo,  ;
                  codcad WITH  ;
                  itehc.codcad,  ;
                  codfte WITH  ;
                  itehc.codfte,  ;
                  estfun WITH  ;
                  LEFT(vkey, 30),  ;
                  totafe WITH  ;
                  itehc.valpart
     ENDIF
     SELECT itehc
ENDSCAN
SET ORDER TO ITEHC1
SELECT calen2
INDEX ON LEFT(estfun, 10) +  ;
      codfte + codpart TO (zind)
GOTO TOP
SCAN
     vkey1 = LEFT(calen2.estfun,  ;
             10) + calen2.codfte +  ;
             LEFT(calen2.codpart,  ;
             2)
     vkey2 = LEFT(calen2.estfun,  ;
             10) + calen2.codfte +  ;
             LEFT(calen2.codpart,  ;
             2)
     STORE 0 TO vvalpart, vtotafe
     DO WHILE vkey1=vkey2 .AND.   ;
        .NOT. EOF()
          vvalpart = vvalpart +  ;
                     calen2.valpart
          vtotafe = vtotafe +  ;
                    calen2.totafe
          SKIP
          vkey2 = LEFT(calen2.estfun,  ;
                  10) +  ;
                  calen2.codfte +  ;
                  LEFT(calen2.codpart,  ;
                  2)
     ENDDO
     SKIP -1
     SELECT calen1
     APPEND BLANK
     REPLACE codpart WITH  ;
             LEFT(calen2.codpart,  ;
             2)
     REPLACE valpart WITH  ;
             vvalpart
     REPLACE totafe WITH vtotafe
     REPLACE estfun WITH  ;
             calen2.estfun
     REPLACE codfte WITH  ;
             calen2.codfte
     SELECT calen2
ENDSCAN
SELECT calen1
INDEX ON LEFT(estfun, 10) +  ;
      codfte + LEFT(codpart, 2)  ;
      TO (vind)
SELECT calen2
USE
ERASE (zdbf)
RELEASE WINDOW xwait
USE IN 6 Calen ALIAS calen ORDER  ;
    calen4
SELECT calen
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
RETURN .T.
*
FUNCTION val_cale1
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
SELECT calen
SET ORDER TO CALEN4
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SEEK filtro
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No existe Calendario a la Fecha'
          SET ORDER TO CALEN4
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
               RETURN .F.
          ENDIF
     ELSE
          vestfun = LEFT(calen.estfun,  ;
                    10)
          vestfun1 = LEFT(calen.estfun,  ;
                     20)
          vcodfte = calen.codfte
          vnummes = calen.nummes
          vdbf = SYS(3) + '.DBF'
          vind = SYS(3) + '.IDX'
          COPY TO (vdbf)  ;
               STRUCTURE
          zdbf = SYS(3) + '.DBF'
          zind = SYS(3) + '.IDX'
          xind = SYS(3) + '.IDX'
          yind = SYS(3) + '.IDX'
          COPY TO (zdbf) FOR  ;
               LEFT(calen.estfun,  ;
               10) = vestfun  ;
               .AND. calen.codfte =  ;
               vcodfte .AND.  ;
               calen.nummes =  ;
               vnummes
          USE
          USE IN 0 (zdbf) ALIAS  ;
              calen2
          USE IN 0 (vdbf) ALIAS  ;
              calen1
          SELECT itehc
          vrecno = IIF(EOF(), -1,  ;
                   RECNO())
          INDEX ON codpart TO  ;
                (yind) FOR codfte =  ;
                vcodfte .AND.  ;
                nummes = vnummes
          GOTO TOP
          SCAN
               SELECT maepre
               SEEK m.periodo +  ;
                    itehc.uniges +  ;
                    itehc.unieje +  ;
                    itehc.codcad
               vkey = uniges +  ;
                      unieje +  ;
                      codfun +  ;
                      codprg +  ;
                      codspr +  ;
                      actpry +  ;
                      itehc.codcom +  ;
                      itehc.codmet +  ;
                      itehc.codpart +  ;
                      itehc.codfte
               SELECT calen2
               LOCATE FOR  ;
                      LEFT(estfun,  ;
                      30) +  ;
                      codpart +  ;
                      codfte =  ;
                      vkey
               IF FOUND()
                    REPLACE totafe  ;
                            WITH  ;
                            totafe +  ;
                            IIF(itehc.tipope =  ;
                            '-',  ;
                            itehc.valpart * - ;
                            1,  ;
                            itehc.valpart)
               ENDIF
               SELECT itehc
          ENDSCAN
          SET ORDER TO ITEHC1
          IF vrecno <> -1
               GOTO vrecno
          ENDIF
          SELECT calen2
          INDEX ON codpart TO  ;
                (zind)
          GOTO TOP
          SCAN
               vkey1 = LEFT(calen2.codpart,  ;
                       2)
               vkey2 = LEFT(calen2.codpart,  ;
                       2)
               STORE 0 TO  ;
                     vvalpart,  ;
                     vtotafe,  ;
                     vtotoc,  ;
                     vtotos
               DO WHILE vkey1= ;
                  vkey2 .AND.   ;
                  .NOT. EOF()
                    vvalpart = vvalpart +  ;
                               calen2.valpart
                    vtotafe = vtotafe +  ;
                              calen2.totafe
                    SKIP
                    vkey2 = LEFT(calen2.codpart,  ;
                            2)
               ENDDO
               SKIP -1
               SELECT calen1
               APPEND BLANK
               REPLACE codpart  ;
                       WITH  ;
                       LEFT(calen2.codpart,  ;
                       2)
               REPLACE valpart  ;
                       WITH  ;
                       vvalpart
               REPLACE totafe  ;
                       WITH  ;
                       vtotafe
               REPLACE estfun  ;
                       WITH  ;
                       calen2.estfun
               SELECT calen2
          ENDSCAN
          SELECT calen1
          INDEX ON LEFT(estfun,  ;
                10) + codfte +  ;
                LEFT(codpart, 2)  ;
                TO (vind)
          SELECT calen2
          INDEX ON LEFT(estfun,  ;
                10) + codfte +  ;
                LEFT(codpart, 2)  ;
                TO (xind)
          SET FILTER TO periodo + LEFT(estfun,;
30) = LEFT(filtro, 32)
          GOTO TOP
          DEFINE WINDOW wind_tec  ;
                 FROM 00, 01 TO  ;
                 10, 78 FLOAT  ;
                 SHADOW TITLE  ;
                 'Techos en : ' +  ;
                 vestfun +  ;
                 SPACE(3) +  ;
                 'Fte. Fto. : ' +  ;
                 ALLTRIM(m.codfte)  ;
                 DOUBLE COLOR  ;
                 SCHEME 10
          SHOW WINDOW wind_tec
          SELECT calen1
          GOTO TOP
          BROWSE NOOPTIMIZE  ;
                 FIELDS codpart  ;
                 :H = 'PG',  ;
                 valpart :H =  ;
                 'Asignado' :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 'Afectado' :P =  ;
                 '99,999,999.99',  ;
                 salpar =  ;
                 IIF(totoc +  ;
                 totos <> 0,  ;
                 valpart - (totoc +  ;
                 totos), valpart -  ;
                 totafe) :H =  ;
                 'Saldos' :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR WINDOW  ;
                 wind_tec TIMEOUT  ;
                 0.00001 
          ON KEY LABEL F10 KEYBOARD CHR(23)
          DEFINE WINDOW wind_cal  ;
                 FROM 11, 01 TO  ;
                 24, 78 FLOAT  ;
                 SHADOW TITLE  ;
                 'Calendario Asignado  '  ;
                 FOOTER  ;
                 '[F10] Seleccionar'  ;
                 DOUBLE COLOR  ;
                 SCHEME 15
          ACTIVATE WINDOW  ;
                   wind_cal
          SELECT calen2
          BROWSE NOOPTIMIZE  ;
                 FIELDS codfte :H =  ;
                 'Fn', codpart :H =  ;
                 'Partida', xx =  ;
                 valasi1(ALIAS(), ;
                 codpart,'8', ;
                 'Descri','R') :H =  ;
                 'Descripci¢n' :  ;
                 20, valpart :H =  ;
                 'Asignado' :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 'Afectado' :P =  ;
                 '99,999,999.99',  ;
                 salpar =  ;
                 IIF(totoc +  ;
                 totos <> 0,  ;
                 valpart - (totoc +  ;
                 totos), valpart -  ;
                 totafe) :H =  ;
                 'Saldos' :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR IN  ;
                 wind_cal  ;
                 NOREFRESH
          vvalpart = valpart
          vtotafe = totafe
          vtotoc = totoc
          vtotos = totos
          mvalor = codpart
          mdescr = valasi1(ALIAS(), ;
                   codpart,'8', ;
                   'Descri','R')
          USE
          ERASE (zdbf)
          SELECT calen1
          USE
          ERASE (vdbf)
          RELEASE WINDOW wind_cal
          RELEASE WINDOW wind_tec
          RELEASE POPUP parametro
     ENDIF
     USE IN 9 Calen ALIAS calen  ;
         ORDER calen4
     SELECT calen
ELSE
     mvalor = calen.codpart
     mdescr = valasi1(ALIAS(), ;
              codpart,'8', ;
              'Descri','R')
ENDIF
SET ORDER TO CALEN4
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 6 SAY  ;
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
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
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
SELECT calen
SET ORDER TO CALEN4
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     SEEK filtro
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No existe Calendario a la Fecha'
          SET ORDER TO CALEN4
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
               RETURN .F.
          ENDIF
     ELSE
          vestfun = LEFT(calen.estfun,  ;
                    10)
          vestfun1 = LEFT(calen.estfun,  ;
                     20)
          vcodfte = calen.codfte
          vnummes = calen.nummes
          vdbf = SYS(3) + '.DBF'
          vind = SYS(3) + '.IDX'
          COPY TO (vdbf)  ;
               STRUCTURE
          zdbf = SYS(3) + '.DBF'
          zind = SYS(3) + '.IDX'
          xind = SYS(3) + '.IDX'
          yind = SYS(3) + '.IDX'
          COPY TO (zdbf) FOR  ;
               LEFT(calen.estfun,  ;
               10) = vestfun  ;
               .AND. calen.codfte =  ;
               vcodfte .AND.  ;
               calen.nummes =  ;
               vnummes
          USE
          USE IN 0 (zdbf) ALIAS  ;
              calen2
          USE IN 13 (vdbf) ALIAS  ;
              calen1
          SELECT maepre
          vrec = RECNO()
          SELECT itehc
          vrecno = RECNO()
          INDEX ON codpart TO  ;
                (yind) FOR codfte =  ;
                vcodfte .AND.  ;
                nummes = vnummes
          GOTO TOP
          SCAN
               SELECT maepre
               SEEK m.periodo +  ;
                    itehc.uniges +  ;
                    itehc.unieje +  ;
                    itehc.codcad
               vkey = uniges +  ;
                      unieje +  ;
                      codfun +  ;
                      codprg +  ;
                      codspr +  ;
                      actpry +  ;
                      itehc.codcom +  ;
                      itehc.codmet +  ;
                      itehc.codpart +  ;
                      itehc.codfte
               SELECT calen2
               LOCATE FOR  ;
                      LEFT(estfun,  ;
                      30) +  ;
                      codpart +  ;
                      codfte =  ;
                      vkey
               IF FOUND()
                    REPLACE totafe  ;
                            WITH  ;
                            totafe +  ;
                            IIF(itehc.tipope =  ;
                            '-',  ;
                            itehc.valpart * - ;
                            1,  ;
                            itehc.valpart)
               ELSE
                    APPEND BLANK
                    REPLACE codpart  ;
                            WITH  ;
                            itehc.codpart,  ;
                            periodo  ;
                            WITH  ;
                            m.periodo,  ;
                            codcad  ;
                            WITH  ;
                            itehc.codcad,  ;
                            codfte  ;
                            WITH  ;
                            itehc.codfte,  ;
                            estfun  ;
                            WITH  ;
                            LEFT(vkey,  ;
                            30),  ;
                            totafe  ;
                            WITH  ;
                            itehc.valpart
               ENDIF
               SELECT itehc
          ENDSCAN
          SET ORDER TO ITEHC1
          GOTO vrecno
          SELECT maepre
          GOTO vrec
          SELECT calen2
          INDEX ON codpart TO  ;
                (zind)
          DELETE FOR  .NOT.  ;
                 EMPTY(codpart)
          GOTO TOP
          SCAN
               vkey1 = LEFT(calen2.codpart,  ;
                       2)
               vkey2 = LEFT(calen2.codpart,  ;
                       2)
               STORE 0 TO  ;
                     vvalpart,  ;
                     vtotafe
               DO WHILE vkey1= ;
                  vkey2 .AND.   ;
                  .NOT. EOF()
                    vvalpart = vvalpart +  ;
                               calen2.valpart
                    vtotafe = vtotafe +  ;
                              calen2.totafe
                    SKIP
                    vkey2 = LEFT(calen2.codpart,  ;
                            2)
               ENDDO
               SKIP -1
               SELECT calen1
               APPEND BLANK
               REPLACE codpart  ;
                       WITH  ;
                       LEFT(calen2.codpart,  ;
                       2)
               REPLACE valpart  ;
                       WITH  ;
                       vvalpart
               REPLACE totafe  ;
                       WITH  ;
                       vtotafe
               REPLACE estfun  ;
                       WITH  ;
                       calen2.estfun
               SELECT calen2
          ENDSCAN
          SELECT calen1
          INDEX ON LEFT(estfun,  ;
                10) + codfte +  ;
                LEFT(codpart, 2)  ;
                TO (vind)
          SELECT calen2
          INDEX ON LEFT(estfun,  ;
                10) + codfte +  ;
                LEFT(codpart, 2)  ;
                TO (xind)
          SET FILTER TO periodo + LEFT(estfun,;
30) = LEFT(filtro, 32)
          GOTO TOP
          DEFINE WINDOW wind_tec  ;
                 FROM 00, 01 TO  ;
                 10, 78 FLOAT  ;
                 SHADOW TITLE  ;
                 'Techos en : ' +  ;
                 vestfun +  ;
                 SPACE(3) +  ;
                 'Fte. Fto. : ' +  ;
                 ALLTRIM(m.codfte)  ;
                 DOUBLE COLOR  ;
                 SCHEME 10
          SHOW WINDOW wind_tec
          SELECT calen1
          GOTO TOP
          BROWSE NOOPTIMIZE  ;
                 FIELDS codpart  ;
                 :H = 'PG',  ;
                 valpart :H =  ;
                 'Asignado' :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 'Afectado' :P =  ;
                 '99,999,999.99',  ;
                 salpar =  ;
                 (valpart -  ;
                 totafe) :H =  ;
                 'Saldos' :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR WINDOW  ;
                 wind_tec TIMEOUT  ;
                 0.00001 
          ON KEY LABEL F10 KEYBOARD CHR(23)
          DEFINE WINDOW wind_cal  ;
                 FROM 11, 01 TO  ;
                 24, 78 FLOAT  ;
                 SHADOW TITLE  ;
                 'Calendario Asignado  '  ;
                 FOOTER  ;
                 '[F10] Seleccionar'  ;
                 DOUBLE COLOR  ;
                 SCHEME 15
          ACTIVATE WINDOW  ;
                   wind_cal
          SELECT calen2
          BROWSE NOOPTIMIZE  ;
                 FIELDS codfte :H =  ;
                 'Fn', codpart :H =  ;
                 'Partida', xx =  ;
                 IIF(LEN(ALLTRIM(codpart)) =  ;
                 6,  ;
                 val_para(SUBSTR(codpart,  ;
                 5, 2),'ESPGAS', ;
                 'D'),  ;
                 val_para1(SUBSTR(codpart,  ;
                 7, 2),'SUBESP' +  ;
                 SUBSTR(codpart,  ;
                 5, 2),'D')) :H =  ;
                 'Descripci¢n' :  ;
                 20, valpart :H =  ;
                 'Asignado' :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 'Afectado' :P =  ;
                 '99,999,999.99',  ;
                 salpar =  ;
                 (valpart -  ;
                 totafe) :H =  ;
                 'Saldos' :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR IN  ;
                 wind_cal  ;
                 NOREFRESH
          vvalpart = valpart
          vtotafe = totafe
          mvalor = codpart
          mdescr = val_para(SUBSTR(mvalor,  ;
                   5, 2),'ESPGAS', ;
                   'D')
          USE
          ERASE (zdbf)
          SELECT calen1
          USE
          ERASE (vdbf)
          RELEASE WINDOW wind_cal
          RELEASE WINDOW wind_tec
          RELEASE POPUP parametro
     ENDIF
     USE IN 6 Calen ALIAS calen  ;
         ORDER calen4
     SELECT calen
ELSE
     ACTIVATE WINDOW standby
     @ 1, 0 SAY  ;
       'Verificando Calendarios'
     mvalor = calen.codpart
     mdescr = val_para(SUBSTR(mvalor,  ;
              5, 2),'ESPGAS', ;
              'D')
     vvalpart = valpart
     vtotafe = 0
     vestfun = LEFT(calen.estfun,  ;
               10)
     vestfun1 = LEFT(calen.estfun,  ;
                20)
     vcodfte = calen.codfte
     vnummes = calen.nummes
     zdbf = SYS(3) + '.DBF'
     yind = SYS(3) + '.IDX'
     COPY TO (zdbf) FOR  ;
          LEFT(calen.estfun, 10) =  ;
          vestfun .AND.  ;
          calen.codfte = vcodfte  ;
          .AND. calen.nummes =  ;
          vnummes
     USE IN 0 (zdbf) ALIAS calen2
     SELECT calen2
     REPLACE totafe WITH 0 ALL
     SELECT itehc
     vrecno = RECNO()
     SELECT maepre
     vrec = RECNO()
     SEEK m.periodo +  ;
          itehc.uniges +  ;
          itehc.unieje +  ;
          itehc.codcad
     vkey0 = uniges + unieje +  ;
             codfun + codprg +  ;
             codspr + actpry +  ;
             itehc.codcom +  ;
             itehc.codmet +  ;
             itehc.codpart +  ;
             itehc.codfte
     SELECT itehc
     INDEX ON codpart TO (yind)  ;
           FOR codfte = vcodfte  ;
           .AND. nummes =  ;
           vnummes
     GOTO TOP
     SCAN
          SELECT maepre
          SEEK m.periodo +  ;
               itehc.uniges +  ;
               itehc.unieje +  ;
               itehc.codcad
          vkey1 = uniges + unieje +  ;
                  codfun + codprg +  ;
                  codspr + actpry +  ;
                  itehc.codcom +  ;
                  itehc.codmet +  ;
                  itehc.codpart +  ;
                  itehc.codfte
          SELECT calen2
          LOCATE FOR LEFT(estfun,  ;
                 30) + codpart +  ;
                 codfte = vkey1
          IF FOUND()
               REPLACE totafe  ;
                       WITH  ;
                       totafe +  ;
                       IIF(itehc.tipope =  ;
                       '-',  ;
                       itehc.valpart * - ;
                       1,  ;
                       itehc.valpart)
          ENDIF
          SELECT itehc
     ENDSCAN
     SET ORDER TO ITEHC1
     GOTO vrecno
     SELECT maepre
     GOTO vrec
     SELECT calen2
     LOCATE FOR LEFT(estfun, 30) +  ;
            codpart + codfte =  ;
            vkey0
     IF FOUND()
          vtotafe = totafe
     ENDIF
     USE
     ERASE (zdbf)
     RELEASE WINDOW standby
     SELECT calen
ENDIF
SET ORDER TO CALEN4
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + 6 SAY  ;
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
     OTHERWISE
          REPLACE &mVariable WITH mValor
          RETURN .T.
ENDCASE
RETURN
*
FUNCTION xval_cale
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
SELECT calen
SET ORDER TO CALEN4
SEEK filtro + mvalor
IF  .NOT. FOUND() .AND.  .NOT.  ;
    mvariable $ 'VZ'
     SEEK filtro
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'No existe Calendario a la Fecha'
          SET ORDER TO CALEN4
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
               RETURN .F.
          ENDIF
     ELSE
          vestfun = LEFT(calen.estfun,  ;
                    10)
          vestfun1 = LEFT(calen.estfun,  ;
                     20)
          vcodfte = calen.codfte
          vnummes = calen.nummes
          vdbf = SYS(3) + '.DBF'
          vind = SYS(3) + '.IDX'
          COPY TO (vdbf)  ;
               STRUCTURE
          zdbf = SYS(3) + '.DBF'
          zind = SYS(3) + '.IDX'
          xind = SYS(3) + '.IDX'
          yind = SYS(3) + '.IDX'
          COPY TO (zdbf) FOR  ;
               LEFT(calen.estfun,  ;
               10) = vestfun  ;
               .AND. calen.codfte =  ;
               vcodfte .AND.  ;
               calen.nummes =  ;
               vnummes
          USE
          USE IN 0 (zdbf) ALIAS  ;
              calen2
          USE IN 13 (vdbf) ALIAS  ;
              calen1
          SELECT maepre
          vrec = RECNO()
          SELECT itehc
          vrecno = RECNO()
          INDEX ON codpart TO  ;
                (yind) FOR codfte =  ;
                vcodfte .AND.  ;
                nummes = vnummes
          GOTO TOP
          SCAN
               SELECT maepre
               SEEK m.periodo +  ;
                    itehc.uniges +  ;
                    itehc.unieje +  ;
                    itehc.codcad
               vkey = uniges +  ;
                      unieje +  ;
                      codfun +  ;
                      codprg +  ;
                      codspr +  ;
                      actpry +  ;
                      itehc.codcom +  ;
                      itehc.codmet +  ;
                      itehc.codpart +  ;
                      itehc.codfte
               SELECT calen2
               LOCATE FOR  ;
                      LEFT(estfun,  ;
                      30) +  ;
                      codpart +  ;
                      codfte =  ;
                      vkey
               IF FOUND()
                    REPLACE totafe  ;
                            WITH  ;
                            totafe +  ;
                            IIF(itehc.tipope =  ;
                            '-',  ;
                            itehc.valpart * - ;
                            1,  ;
                            itehc.valpart)
               ELSE
                    APPEND BLANK
                    REPLACE codpart  ;
                            WITH  ;
                            itehc.codpart,  ;
                            periodo  ;
                            WITH  ;
                            m.periodo,  ;
                            codcad  ;
                            WITH  ;
                            itehc.codcad,  ;
                            codfte  ;
                            WITH  ;
                            itehc.codfte,  ;
                            estfun  ;
                            WITH  ;
                            LEFT(vkey,  ;
                            30),  ;
                            totafe  ;
                            WITH  ;
                            itehc.valpart
               ENDIF
               SELECT itehc
          ENDSCAN
          SET ORDER TO ITEHC1
          GOTO vrecno
          SELECT maepre
          GOTO vrec
          SELECT calen2
          INDEX ON codpart TO  ;
                (zind)
          DELETE FOR  .NOT.  ;
                 EMPTY(codpart)
          GOTO TOP
          SCAN
               vkey1 = LEFT(calen2.codpart,  ;
                       2)
               vkey2 = LEFT(calen2.codpart,  ;
                       2)
               STORE 0 TO  ;
                     vvalpart,  ;
                     vtotafe
               DO WHILE vkey1= ;
                  vkey2 .AND.   ;
                  .NOT. EOF()
                    vvalpart = vvalpart +  ;
                               calen2.valpart
                    vtotafe = vtotafe +  ;
                              calen2.totafe
                    SKIP
                    vkey2 = LEFT(calen2.codpart,  ;
                            2)
               ENDDO
               SKIP -1
               SELECT calen1
               APPEND BLANK
               REPLACE codpart  ;
                       WITH  ;
                       LEFT(calen2.codpart,  ;
                       2)
               REPLACE valpart  ;
                       WITH  ;
                       vvalpart
               REPLACE totafe  ;
                       WITH  ;
                       vtotafe
               REPLACE estfun  ;
                       WITH  ;
                       calen2.estfun
               SELECT calen2
          ENDSCAN
          SELECT calen1
          INDEX ON LEFT(estfun,  ;
                10) + codfte +  ;
                LEFT(codpart, 2)  ;
                TO (vind)
          SELECT calen2
          INDEX ON LEFT(estfun,  ;
                10) + codfte +  ;
                LEFT(codpart, 2)  ;
                TO (xind)
          SET FILTER TO periodo + LEFT(estfun,;
30) = LEFT(filtro, 32)
          GOTO TOP
          DEFINE WINDOW wind_tec  ;
                 FROM 00, 01 TO  ;
                 10, 78 FLOAT  ;
                 SHADOW TITLE  ;
                 'Techos en : ' +  ;
                 vestfun +  ;
                 SPACE(3) +  ;
                 'Fte. Fto. : ' +  ;
                 ALLTRIM(m.codfte)  ;
                 DOUBLE COLOR  ;
                 SCHEME 10
          SHOW WINDOW wind_tec
          SELECT calen1
          GOTO TOP
          BROWSE NOOPTIMIZE  ;
                 FIELDS codpart  ;
                 :H = 'PG',  ;
                 valpart :H =  ;
                 'Asignado' :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 'Afectado' :P =  ;
                 '99,999,999.99',  ;
                 salpar =  ;
                 (valpart -  ;
                 totafe) :H =  ;
                 'Saldos' :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR WINDOW  ;
                 wind_tec TIMEOUT  ;
                 0.00001 
          ON KEY LABEL F10 KEYBOARD CHR(23)
          DEFINE WINDOW wind_cal  ;
                 FROM 11, 01 TO  ;
                 24, 78 FLOAT  ;
                 SHADOW TITLE  ;
                 'Calendario Asignado  '  ;
                 FOOTER  ;
                 '[F10] Seleccionar'  ;
                 DOUBLE COLOR  ;
                 SCHEME 15
          ACTIVATE WINDOW  ;
                   wind_cal
          SELECT calen2
          BROWSE NOOPTIMIZE  ;
                 FIELDS codfte :H =  ;
                 'Fn', codpart :H =  ;
                 'Partida', xx =  ;
                 val_para(RIGHT(codpart,  ;
                 2),'ESPGAS','D', ;
                 28,20) :H =  ;
                 'Descripci¢n' :  ;
                 20, valpart :H =  ;
                 'Asignado' :P =  ;
                 '99,999,999.99',  ;
                 totafe :H =  ;
                 'Afectado' :P =  ;
                 '99,999,999.99',  ;
                 salpar =  ;
                 (valpart -  ;
                 totafe) :H =  ;
                 'Saldos' :P =  ;
                 '99,999,999.99'  ;
                 NOMENU NOAPPEND  ;
                 NOEDIT NODELETE  ;
                 NOCLEAR IN  ;
                 wind_cal  ;
                 NOREFRESH
          vvalpart = valpart
          vtotafe = totafe
          mvalor = codpart
          mdescr = val_para(RIGHT(mvalor,  ;
                   2),'ESPGAS', ;
                   'D')
          USE
          ERASE (zdbf)
          SELECT calen1
          USE
          ERASE (vdbf)
          RELEASE WINDOW wind_cal
          RELEASE WINDOW wind_tec
          RELEASE POPUP parametro
     ENDIF
     USE IN 6 Calen ALIAS calen  ;
         ORDER calen4
     SELECT calen
ELSE
     ACTIVATE WINDOW standby
     @ 1, 0 SAY  ;
       'Verificando Calendarios'
     mvalor = calen.codpart
     mdescr = val_para(RIGHT(mvalor,  ;
              2),'ESPGAS','D')
     vvalpart = valpart
     vtotafe = 0
     vestfun = LEFT(calen.estfun,  ;
               10)
     vestfun1 = LEFT(calen.estfun,  ;
                20)
     vcodfte = calen.codfte
     vnummes = calen.nummes
     zdbf = SYS(3) + '.DBF'
     yind = SYS(3) + '.IDX'
     COPY TO (zdbf) FOR  ;
          LEFT(calen.estfun, 10) =  ;
          vestfun .AND.  ;
          calen.codfte = vcodfte  ;
          .AND. calen.nummes =  ;
          vnummes
     USE IN 0 (zdbf) ALIAS calen2
     SELECT calen2
     REPLACE totafe WITH 0 ALL
     SELECT itehc
     vrecno = RECNO()
     SELECT maepre
     vrec = RECNO()
     SEEK m.periodo +  ;
          itehc.uniges +  ;
          itehc.unieje +  ;
          itehc.codcad
     vkey0 = uniges + unieje +  ;
             codfun + codprg +  ;
             codspr + actpry +  ;
             itehc.codcom +  ;
             itehc.codmet +  ;
             itehc.codpart +  ;
             itehc.codfte
     SELECT itehc
     INDEX ON codpart TO (yind)  ;
           FOR codfte = vcodfte  ;
           .AND. nummes =  ;
           vnummes
     GOTO TOP
     SCAN
          SELECT maepre
          SEEK m.periodo +  ;
               itehc.uniges +  ;
               itehc.unieje +  ;
               itehc.codcad
          vkey1 = uniges + unieje +  ;
                  codfun + codprg +  ;
                  codspr + actpry +  ;
                  itehc.codcom +  ;
                  itehc.codmet +  ;
                  itehc.codpart +  ;
                  itehc.codfte
          SELECT calen2
          LOCATE FOR LEFT(estfun,  ;
                 30) + codpart +  ;
                 codfte = vkey1
          IF FOUND()
               REPLACE totafe  ;
                       WITH  ;
                       totafe +  ;
                       IIF(itehc.tipope =  ;
                       '-',  ;
                       itehc.valpart * - ;
                       1,  ;
                       itehc.valpart)
          ENDIF
          SELECT itehc
     ENDSCAN
     SET ORDER TO ITEHC1
     GOTO vrecno
     SELECT maepre
     GOTO vrec
     SELECT calen2
     LOCATE FOR LEFT(estfun, 30) +  ;
            codpart + codfte =  ;
            vkey0
     IF FOUND()
          vtotafe = totafe
     ENDIF
     USE
     ERASE (zdbf)
     RELEASE WINDOW standby
     SELECT calen
ENDIF
SET ORDER TO CALEN4
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
ON KEY LABEL F10 KEYBOARD CHR(23)
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
    MONTH(fecha) <= VAL(vcalend)  ;
    .AND. codcad + codfte = vkey  ;
    monasig TO vcresup
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
FUNCTION geteje
PRIVATE vkey, valias
valias = ALIAS()
vcodpart = codpart
vkey = codcad + codfte
SELECT itehc
SET RELATION TO nummes + numhc INTO hoja
GOTO TOP
RETURN .T.
*
FUNCTION selper
PUBLIC m.fecsis
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
     newruta = 'h:\sicgdata\DATA' +  ;
               STR(YEAR(m.fecsis),  ;
               4)
     SET PATH TO &NewRuta
     mret = .T.
ELSE
     mret = .F.
ENDIF
RESTORE SCREEN FROM pantalla
RETURN mret
*
FUNCTION valasi
PARAMETER mvalor, mtipo, nivel,  ;
          mvariable, tipret
PRIVATE calias, cord, vidx, mret
calias = ALIAS()
vidx = SYS(3) + '.Idx'
lidx = .F.
DO CASE
     CASE nivel = '1'
          ccampos = 'CatAsi.TipPre'
          lfor = '.T.'
     CASE nivel = '2'
          ccampos = 'CatAsi.TipPre + CatAsi.Generic'
          lfor = "CatAsi.TipPre = '" +  ;
                 ALLTRIM(itepar.tippre) +  ;
                 "' AND !EMPTY(CatAsi.Generic)"
     CASE nivel = '3'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.SGN1'
          lfor = "CatAsi.TipPre+CatAsi.Generic = '" +  ;
                 ALLTRIM(itepar.tippre) +  ;
                 ALLTRIM(itepar.generic) +  ;
                 "' AND !EMPTY(CatAsi.SGN1)"
     CASE nivel = '4'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.SGN1+CatAsi.SGN2'
          lfor = "CatAsi.TipPre+CatAsi.Generic+CatAsi.Sgn1 = '" +  ;
                 itepar.tippre +  ;
                 itepar.generic +  ;
                 itepar.sgn1 +  ;
                 "'AND !EMPTY(CatAsi.SGN2)"
     CASE nivel = '5'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.SGN1+CatAsi.SGN2+CatAsi.EspN1'
          lfor = "CatAsi.TipPre+CatAsi.Generic+CatAsi.Sgn1+CatAsi.Sgn2 = '" +  ;
                 itepar.tippre +  ;
                 itepar.generic +  ;
                 itepar.sgn1 +  ;
                 itepar.sgn2 +  ;
                 "' AND !EMPTY(CatAsi.EspN1)"
     CASE nivel = '6'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.SGN1+CatAsi.SGN2+CatAsi.EspN1+CatAsi.EspN2'
          lfor = "CatAsi.TipPre+CatAsi.Generic+CatAsi.Sgn1+CatAsi.Sgn2+CatAsi.EspN1 = '" +  ;
                 itepar.tippre +  ;
                 itepar.generic +  ;
                 itepar.sgn1 +  ;
                 itepar.sgn2 +  ;
                 itepar.espn1 +  ;
                 "' AND !EMPTY(CatAsi.EspN2)"
ENDCASE
SELECT catasi
cord = ORDER()
SEEK mtipo + mvalor
IF  .NOT. FOUND() .OR.  ;
    EMPTY(mvalor)
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     INDEX ON &cCampos UNIQ TO (vIdx);
FOR &lFor
     lidx = .T.
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET INDEX TO
          SET ORDER TO (cord)
          ERASE (vidx)
          IF  .NOT. EMPTY(calias)
               SELECT (calias)
          ENDIF
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            tippre + generic +  ;
            sgn1 + sgn2 + espn1 +  ;
            espn2 + ' ' +  ;
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
mRet = &mvariable
mdescr = IIF(FOUND(),  ;
         catasi.descri,  ;
         'Error en Partida. ')
SET INDEX TO
SET ORDER TO (cord)
IF lidx
     ERASE (vidx)
ENDIF
IF  .NOT. EMPTY(calias)
     SELECT (calias)
ENDIF
DO CASE
     CASE tipret == 'C'
          REPLACE &mVariable WITH mRet
          RETURN .T.
     CASE tipret == 'R'
          RETURN mret
ENDCASE
RETURN mret
*
FUNCTION valasi1
PARAMETER vali, mvalor, nivel,  ;
          mvariable, tipret
PRIVATE calias, cord, vidx, mret
calias = ALIAS()
vidx = SYS(3) + '.Idx'
lidx = .F.
DO CASE
     CASE nivel = '1'
          ccampos = 'CatAsi.TipPre'
          lfor = 'CatAsi.TipPre = LEFT(' +  ;
                 calias +  ;
                 '.Codpart,1)'
     CASE nivel = '2'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic'
          lfor = 'CatAsi.TipPre+CatAsi.Generic = LEFT(' +  ;
                 calias +  ;
                 '.Codpart,2)'
     CASE nivel = '6'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.SGN1+CatAsi.SGN2+CatAsi.EspN1+CatAsi.EspN2'
          lfor = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.Sgn1+CatAsi.Sgn2+CatAsi.EspN1+CatAsi.EspN2 = ' +  ;
                 vali +  ;
                 '.Codpart'
     CASE nivel = '8'
          ccampos = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.SGN1+CatAsi.SGN2+CatAsi.EspN1+CatAsi.EspN2+CatAsi.EspN3+CatAsi.EspN4'
          lfor = 'CatAsi.TipPre+CatAsi.Generic+CatAsi.Sgn1+CatAsi.Sgn2+CatAsi.EspN1+CatAsi.EspN2+CatAsi.EspN3+CatAsi.EspN4 = ' +  ;
                 vali +  ;
                 '.Codpart'
ENDCASE
SELECT catasi
cord = ORDER()
SEEK mvalor
lidx = .F.
IF  .NOT. FOUND() .OR.  ;
    EMPTY(mvalor)
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     INDEX ON &cCampos UNIQ TO (vIdx);
   
     lidx = .T.
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET INDEX TO
          SET ORDER TO (cord)
          ERASE (vidx)
          IF  .NOT. EMPTY(calias)
               SELECT (calias)
          ENDIF
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            tippre + generic +  ;
            sgn1 + sgn2 + espn1 +  ;
            espn2 + ' ' +  ;
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
mRet = &mvariable
mdescr = IIF(FOUND(),  ;
         catasi.descri,  ;
         'Error en Partida. ')
SET INDEX TO
SET ORDER TO (cord)
IF lidx
     ERASE (vidx)
ENDIF
IF  .NOT. EMPTY(calias)
     SELECT (calias)
ENDIF
DO CASE
     CASE tipret == 'C'
          REPLACE &mVariable WITH mRet
          RETURN .T.
     CASE tipret == 'R'
          RETURN mret
ENDCASE
RETURN mret
*
FUNCTION fpartida
PARAMETER cpart
PRIVATE ccad
ccad = ''
xlen = LEN(ALLTRIM(cpart))
nnivel = 0
DO CASE
     CASE xlen = 1
          nnivel = 1
     CASE xlen = 2
          nnivel = 2
     CASE xlen = 4
          nnivel = 3
     CASE xlen = 6
          nnivel = 4
     CASE xlen = 8
          nnivel = 5
     CASE xlen = 10
          nnivel = 6
     CASE xlen = 12
          nnivel = 7
     CASE xlen = 14
          nnivel = 8
ENDCASE
FOR i = 1 TO nnivel
     DO CASE
          CASE i = 1
               ccad = ccad +  ;
                      LEFT(cpart,  ;
                      1)
          CASE i = 2
               ccad = ccad + '.' +  ;
                      SUBSTR(cpart,  ;
                      2, 1)
          OTHERWISE
               ccad = ccad + '.' +  ;
                      ALLTRIM(STR(VAL(SUBSTR(cpart,  ;
                      i * 2 - 3,  ;
                      2)), 2))
     ENDCASE
ENDFOR
RETURN ccad
*
