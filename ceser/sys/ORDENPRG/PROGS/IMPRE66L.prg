*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(67) + CHR(66)
FOR a = 1 TO 50
     ? a
ENDFOR
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
*
*** 
*** ReFox - retrace your steps ... 
***
