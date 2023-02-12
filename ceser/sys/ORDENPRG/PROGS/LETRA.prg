*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CONSOLE OFF
SET DEVICE TO PRINTER
SET PRINTER ON
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(64)
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(77) + CHR(27) + CHR(15)
? ' 20cpp = @prow(),pcol() say chr(27) + chr(77) + chr(27) + chr(15)'
?
FOR a = 1 TO 5
     ? '1 2 3 4 5 6 7 8 9 0'
ENDFOR
?
? '--------------------------------------------------------'
?
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(80) + CHR(27) + CHR(15)
? '17cpp = @prow(),pcol() say chr(27) + chr(80) + chr(27) + chr(15)'
?
FOR a = 1 TO 5
     ? '1 2 3 4 5 6 7 8 9 0'
ENDFOR
?
? '--------------------------------------------------------'
?
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(77) + CHR(27) + CHR(18)
? '12cpp = @prow(),pcol() say chr(27) + chr(77) + chr(27) + chr(18)'
?
FOR a = 1 TO 5
     ? '1 2 3 4 5 6 7 8 9 0'
ENDFOR
?
? '--------------------------------------------------------'
?
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(80) + CHR(27) + CHR(18)
? '10cpp = @prow(),pcol() say chr(27) + chr(80) + chr(27) + chr(18)'
?
FOR a = 1 TO 5
     ? '1 2 3 4 5 6 7 8 9 0'
ENDFOR
?
? '--------------------------------------------------------'
?
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(77) + CHR(27) + CHR(14)
? '06cpp = @prow(),pcol() say chr(27) + chr(77) + chr(27) + chr(14)'
?
FOR a = 1 TO 5
     ? '1 2 3 4 5 6 7 8 9 0'
ENDFOR
?
? '--------------------------------------------------------'
?
@ PROW(), PCOL() SAY CHR(27) +  ;
  CHR(80) + CHR(27) + CHR(14)
? '05cpp = @prow(),pcol() say chr(27) + chr(80) + chr(27) + chr(14)'
FOR a = 1 TO 5
     ? '1 2 3 4 5 6 7 8 9 0'
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
