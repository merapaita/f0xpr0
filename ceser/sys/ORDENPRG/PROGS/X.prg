*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
SET DISPLAY TO VGA50
DEFINE WINDOW informa FROM 10, 10  ;
       TO 30, 70
w_numsol = '  920944'
CLOSE DATABASES
USE ST_ISCIC ORDER NUMORD
ACTIVATE WINDOW informa
BROWSE FIELDS fecini :H =  ;
       'FCH INIC.', feccom :H =  ;
       'FCH COMP.', inform :H =  ;
       'INFORMAC.', horcom :H =  ;
       'HORA COMP', horini :H =  ;
       'HORA INIC', horfin :H =  ;
       'HORA FIN', numsol :H =  ;
       'S. SERV.' FREEZE inform  ;
       KEY w_numsol
USE
DEACTIVATE WINDOW informa
SET DISPLAY TO VGA25
*
*** 
*** ReFox - retrace your steps ... 
***
