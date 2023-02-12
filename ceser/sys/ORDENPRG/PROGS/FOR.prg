*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
z = 'MARTIN'
wrk_total = LEN(z)
b = wrk_total
wrk_clafin = ''
FOR c = 1 TO wrk_total
     wrk_clafin = wrk_clafin +  ;
                  SUBSTR(z, b,  ;
                  1)
     ? wrk_clafin + ' - ' +  ;
       SUBSTR(z, wrk_total, 1)
     ? wrk_total
     b = b - 1
ENDFOR
? ALLTRIM(wrk_clafin)
*
*** 
*** ReFox - retrace your steps ... 
***
