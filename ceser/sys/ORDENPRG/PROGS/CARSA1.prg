*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
wrk_total = 0
MODIFY REPORT CARSA
*
FUNCTION total
PARAMETER opc
COUNT FOR numdoc = opc TO  ;
      wrk_total
RETURN wrk_total
*
*** 
*** ReFox - retrace your steps ... 
***
