CLEAR
DEFINE POPUP x_modulo FROM 01, 38 TO 22, 40 PROMPT FIELDS  ;
       mod_descri TITLE ' MODULOS ' SHADOW IN  ;
       screen FOOTER 'Presione ENTER para Seleccionar'  ;
       COLOR SCHEME 8
DEFINE WINDOW pantalla FROM 00, 00 TO 24, 79 IN screen NONE
DEFINE WINDOW ingreso FROM 10, 22 TO 14, 52 SHADOW IN screen COLOR SCHEME 8
DEFINE WINDOW empresa FROM 14, 03 TO 17, 33 SHADOW IN screen COLOR SCHEME 8
wrk_acc = '  '
w_password = SPACE(0)
DO mueve_1
DO mueve_2
w_fin = 0
DO WHILE w_fin=0
     DO chequeo_1
     DO chequeo_2
     DO mueve_3
ENDDO
DEACTIVATE WINDOW ingreso, empresa, menus
DEACTIVATE POPUP x_modulo
SET CLOCK OFF
ON KEY
CLOSE DATABASES
*
PROCEDURE mueve_1
= f_pan01('°',150)
ACTIVATE WINDOW ingreso
FOR i = 1 TO 08
     MOVE WINDOW ingreso TO 10 + i, 20 - i
     FOR x = 1 TO 3000
     ENDFOR
ENDFOR
FOR i = 1 TO 10
     MOVE WINDOW ingreso TO 18, 13 - i
     FOR x = 1 TO 3000
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE mueve_2
ACTIVATE WINDOW empresa
FOR i = 1 TO 13
     MOVE WINDOW empresa TO 14 - i, 03
     FOR x = 1 TO 3000
     ENDFOR
ENDFOR
FOR i = 1 TO 15
     SIZE WINDOW empresa TO 01 + i, 31
     FOR x = 1 TO 3000
     ENDFOR
ENDFOR
USE gc_par00
@ 00, ((30 - LEN(ALLTRIM(par_razsoc))) / 2) SAY ALLTRIM(par_razsoc)
@ 01, ((30 - LEN(ALLTRIM(par_abrev))) / 2)  SAY ALLTRIM(par_abrev)
@ 05, ((30 - LEN('Sistema')) / 2) SAY 'Sistema'
@ 06, ((30 - LEN('de')) / 2) SAY  'de'
@ 07, ((30 - LEN('Gestion Comercial')) / 2) SAY 'Gestion Comercial'
@ 12, ((30 - LEN(DTOC(DATE()))) / 2) SAY DATE()
RETURN
*
PROCEDURE chequeo_1
ON KEY
ACTIVATE WINDOW ingreso
@ 0, 0 SAY ' USUARIO  : [          ]'
@ 1, 0 SAY ' PASSWORD : [          ]'
SET CURSOR ON
error = 0
w_sal = 0
DO WHILE w_sal=0
     w_password = SPACE(0)
     w_sal = 0
     @ 0, 13 GET w_usuario DEFAULT SPACE(10) PICTURE '@!' VALID entrada(VARREAD())
     READ CYCLE
     IF LASTKEY() = 27
          w_fin = 1
          RETURN
     ENDIF
     FOR i = 1 TO 10
          w_p = INKEY(0, 'HM')
          IF w_p = 13
               EXIT
          ENDIF
          IF w_p = 27
               w_fin = 1
               RETURN
          ENDIF
          @ 1, 12 + i SAY ''
          w_password = w_password + UPPER(CHR(w_p))
     ENDFOR
     = entrada('PASSWORD')
ENDDO
RETURN
*
FUNCTION entrada
PARAMETER dato
DO CASE
     CASE dato = 'W_USUARIO'
          IF LASTKEY() = 27
               w_fin = 1
               RETURN
          ENDIF
          USE SHARED password ORDER usuario
          seek &dato
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
               RETURN
          ENDIF
          w_password = ALLTRIM(w_password)
          num = LEN(w_password)
          STORE SPACE(10) TO w_passwor
          FOR x = 1 TO LEN(w_password)
               w_passwor = w_passwor + SUBSTR(w_password,num,1)
               num = num - 1
          ENDFOR
          USE SHARED password ORDER codigo
          SET EXACT ON
          SEEK ALLTRIM(w_passwor) + ALLTRIM(w_usuario)
          SET EXACT OFF
          IF  .NOT. FOUND()
               DO p_mensaje WITH 'Password Incorrecto'
               @ 1, 0 SAY ' PASSWORD : [          ]'
               error = error + 1
               IF error = 3
                    DEFINE WINDOW peligro FROM 10,10 TO 12,70 IN screen
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
               clave = codigo
               xnnn = nivel
               @ 0, 0 SAY '  BIENVENIDO(A) :            '
               @ 1, 0 SAY '  ' + nombre
               wrk_acc = nivel
               w_sal = 1
               USE
          ENDIF
ENDCASE
*
PROCEDURE chequeo_2
IF LASTKEY() = 27
     RETURN
ENDIF
ON SELECTION POPUP x_modulo deac popup;
p_modulo
USE SHARED gc_modulo
IF SUBSTR(wrk_acc, 2, 1) <> ' '
     SET FILTER TO SUBSTR(mod_acceso, 1, 1) = SUBSTR(wrk_acc, 1, 1) .AND. SUBSTR(mod_acceso, 2, 1) = SUBSTR(wrk_acc,2, 1)
ELSE
     SET FILTER TO SUBSTR(mod_acceso,1, 1) = SUBSTR(wrk_acc, 1, 1)
ENDIF
GOTO TOP
RETURN
*
PROCEDURE mueve_3
IF LASTKEY() = 27
     RETURN
ENDIF
ACTIVATE POPUP x_modulo NOWAIT
FOR i = 1 TO 36
     SIZE POPUP x_modulo TO 20, 1 + i
     FOR x = 1 TO 3000
     ENDFOR
ENDFOR
DO WHILE LASTKEY()<>27
     ON KEY LABEL enter do c_modulos with mod_modulo
     ACTIVATE POPUP x_modulo
ENDDO
RETURN
*
PROCEDURE c_modulos
PARAMETER modulo
ON KEY
ACTIVATE WINDOW pantalla
= f_pan01('°',150)
do &modulo
ON KEY LABEL enter do c_modulos with mod_modulo
DEACTIVATE WINDOW pantalla
DO chequeo_2
RETURN
*
