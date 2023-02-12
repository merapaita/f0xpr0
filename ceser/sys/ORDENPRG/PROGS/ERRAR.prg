*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SELECT 1
USE error
SELECT 2
USE gc_pro00
FOR a = 1 TO 100
     SELECT 2
     wrk_codpro = pro_codpro
     wrk_descri = pro_descri
     SKIP
     SELECT 1
     APPEND BLANK
     DO rbloquea
     REPLACE codigo WITH  ;
             wrk_codpro
     REPLACE descrip WITH  ;
             wrk_descri
     REPLACE fecha WITH DATE()
     REPLACE cantid WITH 100
     REPLACE estado WITH .T.
     REPLACE movim WITH  ;
             'Prueba de errores'
     UNLOCK
ENDFOR
*
PROCEDURE stop
RETRY
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
