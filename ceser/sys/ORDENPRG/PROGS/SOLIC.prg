*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLOSE DATABASES
USE SHARED st_isrep
a = 0
SCAN WHILE  .NOT. EOF()
     w_codent = ALLTRIM(codent)
     w_codent2 = VAL(w_codent)
     REPLACE codent WITH  ;
             STR(w_codent2, 11)
     a = a + 1
     @ 10, 20 SAY a
ENDSCAN
CLOSE DATABASES
*
*** 
*** ReFox - retrace your steps ... 
***
