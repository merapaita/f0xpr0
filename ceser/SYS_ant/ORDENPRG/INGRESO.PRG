*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW pie FROM 21, 0 TO  ;
       24, 79 IN screen
DEFINE WINDOW ingreso FROM 10, 22  ;
       TO 14, 52 SHADOW IN screen  ;
       COLOR SCHEME 8
DEFINE WINDOW empresa FROM 14, 23  ;
       TO 17, 53 SHADOW IN screen  ;
       COLOR SCHEME 8
SELECT 20
USE SHARED gc_PAR00
USE
clave = SPACE(10)
PUBLIC palqz, nivell, clavee,  ;
       users
palqz = .T.
DO mueve_1
DO mueve_2
error = 0
DO WHILE palqz
     wrk_acc = '  '
     w_password = SPACE(0)
     ACTIVATE WINDOW ingreso
     @ 0, 0 SAY  ;
       ' USUARIO  : [          ]'
     @ 1, 0 SAY  ;
       ' PASSWORD : [          ]'
     SET CURSOR ON
     w_password = SPACE(0)
     @ 00, 13 GET w_usuario  ;
       DEFAULT SPACE(10) PICTURE  ;
       '@!' VALID  ;
       entrada(VARREAD())
     READ CYCLE
     IF LASTKEY() = 27
          rpt = f_yesno( ;
                'DESEA SALIR DEL SISTEMA?' ;
                )
          IF LASTKEY() = 27 .OR.  ;
             rpt
               palqz = .F.
               ppalqz = .F.
               DEACTIVATE WINDOW  ;
                          ALL
               = f_fondo1( ;
                 '    Repuestos ', ;
                 'W+/N')
               DO salida
               = f_cierra()
               RETURN ppalqz
          ENDIF
     ELSE
          FOR i = 1 TO 10
               w_p = INKEY(0,  ;
                     'HM')
               IF w_p = 13
                    EXIT
               ENDIF
               IF w_p = 27
                    LOOP
               ELSE
                    @ 1, 12 + i  ;
                      SAY ''
                    w_password = w_password +  ;
                                 UPPER(CHR(w_p))
               ENDIF
          ENDFOR
          = entrada('PASSWORD')
     ENDIF
ENDDO
RETURN ppalqz
*
PROCEDURE salida
CLOSE DATABASES
DEACTIVATE WINDOW ALL
DEACTIVATE MENU ALL
DEACTIVATE POPUP ALL
fin1 = 'ORDEN  REPARACION'
DEFINE WINDOW cid FROM 10, 10 TO  ;
       14, 70 IN screen DOUBLE  ;
       COLOR SCHEME 12
ACTIVATE WINDOW cid
@ 0, (WCOLS() - LEN(fin1)) / 2  ;
  SAY fin1
@ 1, (WCOLS() - LEN(rge_razsoc)) /  ;
  2 SAY rge_razsoc
@ 2, (WCOLS() - LEN(rge_abrev)) /  ;
  2 SAY rge_abrev
ACTIVATE SCREEN
= INKEY(3)
RELEASE WINDOW cid
CLEAR MACROS
RETURN
*
FUNCTION f_pan01
PARAMETER char, tiempo
PRIVATE j
cad = REPLICATE(char, 8) + ' '
FOR j = 0 TO 39
     @ (3.0/10) * j, j, 24 - (3.0/ ;
       10) * j, 79 - j BOX cad
     FOR t = 1 TO tiempo
     ENDFOR
ENDFOR
RETURN .T.
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
= INKEY(1.5 )
RELEASE WINDOW mensj
SET CURSOR ON
RETURN
*
PROCEDURE f_fondo1
PARAMETER cadena, col1
q = STR(PARAMETERS(), 1)
IF q = '2'
     set color to &col1
ENDIF
STORE 0 TO x, d
lc = LEN(cadena)
x = 1
fl = .T.
DO WHILE fl
     s = 0
     FOR r = 1 TO lc
          IF d + s < 80
               @ x - 1, (s + d)  ;
                 SAY  ;
                 SUBSTR(cadena, r,  ;
                 1)
               s = s + 1
          ELSE
               IF x > 24 .AND. d +  ;
                  s > 79
                    fl = .F.
                    EXIT
               ENDIF
               x = x + 1
               STORE 0 TO s, d
               r = r - 1
          ENDIF
     ENDFOR
     d = d + s
ENDDO
RETURN
*
FUNCTION f_cierra
FOR j = 1 TO 39
     @ 12 - (3.0/10) * j, 39 - j,  ;
       13 + (3.0/10) * j, 40 + j  ;
       BOX SPACE(9)
     FOR e = 1 TO 250
     ENDFOR
ENDFOR
RETURN .T.
*
PROCEDURE mueve_1
= f_pan01('∞',150)
ACTIVATE WINDOW ingreso
FOR i = 1 TO 08
     MOVE WINDOW ingreso TO 10 +  ;
          i, 40 - i
     FOR x = 1 TO 1500
     ENDFOR
ENDFOR
FOR i = 1 TO 10
     MOVE WINDOW ingreso TO 18,  ;
          33 - i
     FOR x = 1 TO 1500
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE mueve_2
ACTIVATE WINDOW empresa
FOR i = 1 TO 13
     MOVE WINDOW empresa TO 14 -  ;
          i, 23
     FOR x = 1 TO 1500
     ENDFOR
ENDFOR
FOR i = 1 TO 15
     SIZE WINDOW empresa TO 01 +  ;
          i, 31
     FOR x = 1 TO 1500
     ENDFOR
ENDFOR
TEXT
 ﬂﬂﬂﬂ  ﬂﬂﬂﬂ ﬂﬂﬂﬂﬂ ﬂﬂﬂﬂ ﬂﬂﬂﬂ€
 €     €‹‹‹ €‹‹‹‹ €‹‹‹ €‹‹‹€
 €‹‹‹  €‹‹‹ ‹‹‹‹€ €‹‹‹ €  €
ENDTEXT
SELECT 20
USE SHARED gc_par00
@ 04, ((30 - LEN(ALLTRIM(par_razsoc))) / 2) SAY ALLTRIM(par_razsoc)
@ 05, ((30 - LEN(ALLTRIM(par_provin))) / 2) SAY ALLTRIM(par_provin)
@ 07, ((30 - LEN('Sistema')) / 2) SAY 'Sistema'
@ 08, ((30 - LEN('de')) / 2) SAY 'de'
@ 09, ((30 - LEN('Ordenes de Reparaci¢n')) / 2) SAY '     Reparaciones'
@ 12, ((30 - LEN(DTOC(DATE()))) / 2) SAY DATE()
RETURN
*
FUNCTION chequeo_1
ON KEY
ACTIVATE WINDOW ingreso
@ 0, 0 SAY ' USUARIO  : [          ]'
@ 1, 0 SAY ' PASSWORD : [          ]'
SET CURSOR ON
w_password = SPACE(0)
@ 0, 13 GET w_usuario DEFAULT SPACE(10) PICTURE '@!' VALID entrada(VARREAD())
READ CYCLE
IF LASTKEY() = 27
     palqz = .T.
     RETURN palqz
ENDIF
FOR i = 1 TO 10
     w_p = INKEY(0, 'HM')
     IF w_p = 13
          EXIT
     ENDIF
     IF w_p = 27
          palqz = .T.
          RETURN palqz
     ENDIF
     @ 1, 12 + i SAY ''
     w_password = w_password + UPPER(CHR(w_p))
ENDFOR
RETURN
*
FUNCTION entrada
PARAMETER dato
DO CASE
     CASE dato = 'W_USUARIO'
          IF LASTKEY() = 27
               palqz = .T.
               RETURN palqz
          ENDIF
          SELECT 20
          USE SHARED password ORDER usuario
          SEEK &DATO
          IF  .NOT. FOUND()
               DO p_mensaje WITH 'Usuario no reconocido en el Sistema'
               RETURN .F.
          ELSE
               CLEAR READ
          ENDIF
     CASE dato = 'PASSWORD'
          IF LASTKEY() = 5 .OR. LASTKEY() = 19
               @ 1, 0 SAY ' PASSWORD : [          ]'
               RETURN
          ENDIF
          IF LASTKEY() = 27
               @ 1, 0 SAY ' PASSWORD : [          ]'
               palqz = .T.
               RETURN palqz
          ENDIF
          w_password = ALLTRIM(w_password)
          num = LEN(w_password)
          STORE SPACE(10) TO w_passwor
          FOR x = 1 TO LEN(w_password)
               w_passwor = w_passwor + SUBSTR(w_password, num, 1)
               num = num - 1
          ENDFOR
          SELECT 20
          USE SHARED password ORDER codigo
          SET EXACT ON
          SEEK ALLTRIM(w_passwor) + ALLTRIM(w_usuario)
          SET EXACT OFF
          IF  .NOT. FOUND()
               DO p_mensaje WITH 'Password Incorrecto'
               @ 1, 0 SAY ' PASSWORD : [          ]'
               error = error + 1
               IF error = 3
                    DEFINE WINDOW peligro FROM 10, 10 TO 12, 70 IN screen
                    ACTIVATE WINDOW peligro
                    SET COLOR TO W/N*
                    @ 0, 1 SAY '  PELIGRO !!!!!   SE DETECTO UN INTRUSO EN EL SISTEMA'
                    SET ESCAPE OFF
                    DO WHILE .T.
                         = msound(10,500,10,1,0)
                    ENDDO
               ENDIF
          ELSE
               CLEAR READ
               users = codigo
               nivell = nivelo
               clavee = abrevid
               @ 0, 0 SAY '  BIENVENIDO(A) :            '
               @ 1, 0 SAY '  ' + nombre
               wrk_acc = nivel
               USE
               DEACTIVATE WINDOW ingreso, empresa
               palqz = .F.
               ppalqz = .T.
          ENDIF
ENDCASE
*
FUNCTION msound
PARAMETER num, ini, dta, lon, ida
PRIVATE q
q = STR(PARAMETERS(), 1)
num = IIF(q = '0', 9, num)
ini = IIF(q $ '01', 0, ini)
dta = IIF(q $ '012', 500, dta)
lon = IIF(q $ '0123', 1, lon)
ida = IIF(q $ '01234', 1, ida)
IF ida = 0
     FOR i = 1 TO INT(num / 2)
          SET BELL TO ini + i * dta, lon
          ?? CHR(7)
     ENDFOR
     FOR i = INT(num / 2) TO 1 STEP -1
          SET BELL TO ini + i * dta, lon
          ?? CHR(7)
     ENDFOR
ELSE
     IF (ida = -1) .OR. (ida = 1)
          FOR i = 1 TO num
               SET BELL TO ini + (ida) * i * dta, lon
               ?? CHR(7)
          ENDFOR
     ENDIF
ENDIF
SET BELL TO
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
