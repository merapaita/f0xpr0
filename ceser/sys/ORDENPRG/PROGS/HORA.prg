*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE TIME() TO wtime
CLEAR
SET CLOCK ON
@ 10, 10 SAY 'Hora ' GET wtime
READ
IF LASTKEY() = 27
     ?? 'Cancelado'
     RETURN
ENDIF
DO WHILE TIME()<wtime
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***
