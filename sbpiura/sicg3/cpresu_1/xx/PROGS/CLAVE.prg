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
IF vretorno
     @ 22, 25 SAY  ;
       IIF(VAL(SUBSTR(TIME(), 1,  ;
       2)) < 12, 'Buenos Dias ',  ;
       'Buenas Tardes ') +  ;
       ALLTRIM(usua.nombre)
     @ 23, 25 SAY 'USUARIO : ' +  ;
       ALLTRIM(usua.nombre)
     WAIT TIMEOUT 2 ''
     @ 23, 25, 23, 80 BOX  ;
       '°°°°°°°°°'
     vcoddep = usua.coddep
ENDIF
RETURN vretorno
*
