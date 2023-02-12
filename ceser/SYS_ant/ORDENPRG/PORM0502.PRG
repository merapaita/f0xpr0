*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'ADMINISTRACION'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CAMBIO DE PASSWORD '
CLOSE DATABASES
SELECT 1
USE SHARED PASSWORD ORDER CODIGO
= INSMODE(.F.)
DO WHILE .T.
     STORE SPACE(10) TO pass1,  ;
           pass2
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 6, 77
     @ 4, 6 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 05 SAY  ;
       'Password Actual:'
     @ 05, 05 SAY  ;
       'Password Nuevo :'
     @ 04, 24 GET pass1 PICTURE  ;
       '@!' VALID valida(pass1)  ;
       COLOR N,W/W 
     @ 05, 24 GET pass2 PICTURE  ;
       '@!' COLOR N,W/W 
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     w_password = ALLTRIM(pass1)
     num = LEN(w_password)
     STORE SPACE(10) TO w_passwor
     FOR x = 1 TO LEN(w_password)
          w_passwor = w_passwor +  ;
                      SUBSTR(w_password,  ;
                      num, 1)
          num = num - 1
     ENDFOR
     SELECT password
     SET EXACT ON
     SEEK ALLTRIM(w_passwor) +  ;
          ALLTRIM(users)
     SET EXACT OFF
     IF FOUND()
          w_password = ALLTRIM(pass2)
          num = LEN(w_password)
          STORE SPACE(10) TO  ;
                w_passwor
          FOR x = 1 TO  ;
              LEN(w_password)
               w_passwor = w_passwor +  ;
                           SUBSTR(w_password,  ;
                           num,  ;
                           1)
               num = num - 1
          ENDFOR
          REPLACE abrevid WITH  ;
                  ALLTRIM(w_passwor)
     ENDIF
     @ 04, 40 SAY SPACE(20)
     @ 05, 40 SAY SPACE(20)
     @ 10, 10 SAY  ;
       'Se Establecio el Nuevo Password'  ;
       COLOR N+/W 
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
ENDDO
DO saca_win
RETURN
*
FUNCTION valida
PARAMETER opc
w_password = ALLTRIM(opc)
num = LEN(w_password)
STORE SPACE(10) TO w_passwor
FOR x = 1 TO LEN(w_password)
     w_passwor = w_passwor +  ;
                 SUBSTR(w_password,  ;
                 num, 1)
     num = num - 1
ENDFOR
SELECT password
SET EXACT ON
SEEK ALLTRIM(w_passwor) +  ;
     ALLTRIM(users)
SET EXACT OFF
IF  .NOT. FOUND()
     @ 04, 40 SAY  ;
       'Password Incorrecto'
     @ 10, 10 SAY SPACE(32)
     RETURN .F.
ELSE
     pas1 = w_passwor
ENDIF
@ 04, 40 SAY SPACE(30)
@ 05, 40 SAY  ;
  'Ingrese New Passowrd'
@ 10, 10 SAY SPACE(32)
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
