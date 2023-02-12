*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
USE st_iclpr
SCAN WHILE  .NOT. EOF()
     w_codent = ALLTRIM(codent)
     w_codent2 = VAL(w_codent)
     REPLACE codent WITH  ;
             STR(w_codent2, 11)
ENDSCAN
*
*** 
*** ReFox - retrace your steps ... 
***
