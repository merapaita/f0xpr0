*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET TALK ON
SET BELL ON
CLEAR
SELECT 1
USE basro
APPEND BLANK
@ 10, 30 SAY ' Ingreso de Datos '
@ 12, 20 GET nombre PICTURE '@!'
@ 14, 20 GET obser
@ 16, 20 GET edad PICTURE '9999'
READ
IF LASTKEY() = 27
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***
