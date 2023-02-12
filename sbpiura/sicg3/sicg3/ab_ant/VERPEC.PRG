
USE PECOSA   IN 1   order tag  Pecosa1  ALIAS pecosa
set talk off
set echo off
go top
select pecosa
set orde to pecosa1
*SET DEVICE TO PRINT
*SET PRINT ON
go top
numr = 1
? 'Relacion de Correlativos no ingresados al Sistema'
scan
   if val(numpec)#numr       
      ?'     => '+PADL(ALLTRIM(STR(numr,4)),4,'0')
      numR = numr + 1
      SKIP -1
   ELSE   
   numR = numr + 1
   endif   
endscan
*SET PRINT OFF
*SET DEVICE TO SCREE
CLEAR
CLOSE DATA   



