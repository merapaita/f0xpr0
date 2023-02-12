*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CONSOLE OFF
SET DEVICE TO PRINTER
SET PRINTER ON
@ PROW(), PCOL() SAY CHR(15)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(15)
? ' 20cpp = @prow(),pcol() say chr(27) + chr(77) + chr(27) + chr(15)'
?
FOR a = 1 TO 5
     ? '1234567890  ABCDEFGHIJKLMNOPQRSUVWXYZ   abcdefghijklmnopqrstuvwxyz'
ENDFOR
?
? '--------------------------------------------------------'
?
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
*
*** 
*** ReFox - retrace your steps ... 
***
