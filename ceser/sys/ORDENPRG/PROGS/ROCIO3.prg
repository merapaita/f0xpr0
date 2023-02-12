*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
SELECT 1
USE st_iorep
SET FILTER TO codmar = '01  '
SELECT 2
USE st_mvord
SELECT 1
GOTO BOTTOM
wrk_cant = 0
b = 0
DIMENSION orden[ 1]
STORE SPACE(8) TO orden[ 1]
FOR cont = 1 TO 500
     b = b + 1
     @ 05, 10 SAY 'Registro  ' +  ;
       STR(b) + STR(cont)
     @ 10, 10 SAY codmar +  ;
       '------' + numdoc
     wrk_numord = numdoc
     wrk_codmar = codmar
     SELECT 2
     SET FILTER TO orden = wrk_numord;
.AND. estado = '004 '
     COUNT TO a
     IF a > 0
          wrk_cant = wrk_cant + 1
          DIMENSION orden[  ;
                    wrk_cant]
          orden[ wrk_cant] =  ;
               wrk_numord
     ENDIF
     SET FILTER TO
     SELECT 1
     SKIP -1
ENDFOR
@ 15, 10 SAY wrk_cant
SET DEVICE TO PRINTER
x = 2
FOR z = 1 TO wrk_cant
     x = x + 1
     @ x, 45 SAY orden(z)
ENDFOR
x = x + 1
@ x, 10 SAY  ;
  'TOTAL DE ARTICULOS QUE ESTUVIERON EN PDTE. x REPUESTO  = ' +  ;
  STR(wrk_cant)
x = x + 1
@ x, 50 SAY STR(wrk_cant) +  ;
  ' -------> ' + ootab('MARC', ;
  wrk_codmar)
x = x + 1
@ x, 05 SAY  ;
  'TOTAL DE ARTICULOS INGRESADOS A TALLER  = ' +  ;
  STR(cont - 1) + '  ' +  ;
  STR((wrk_cant / (cont - 1)) *  ;
  100) + '%'
SET PRINTER TO
SET DEVICE TO SCREEN
EJECT
*
*** 
*** ReFox - retrace your steps ... 
***
