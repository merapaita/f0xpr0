CLEAR
SELE 1
USE ITER INDEX A1
*INDEX ON CODGEN+CODCLA+UPPER(ALLTRIM(SUBSTR(DESCRI,1,40)))+CODDET TO A1
*INDEX ON CODGEN+CODCLA+CODDET TO A1
SELE 2
USE COD INDEX C1
*INDEX ON LETRA TO C1

*SELE 3
*USE ITER INDEX B1
*INDEX ON CODARTNEW TO B1

PUBLIC PR,XT

SELE 1
GO TOP
SCAN FOR EMPTY(CODDET)
   ? LEFT(DESCRI,1),' ',CODARTNEW
   OK=ASIGNA (LEFT(ITER.DESCRI,1))  &&,substr(ITER.codartNEW,1,7))
   if OK
      if rlock()
         ? 'OK',' ',CODARTNEW+padl(alltrim(str(pr,3)),3,'0')
        *repla CODARTNEW with ALLTRIM(CODARTNEW)+padl(alltrim(str(pr,3)),3,'0'),inv1297trn with 1
         repla CODDET with XT,inv1297trn with 1
      endif
   endif
  
ENDS 




PROCEDURE ASIGNA
*---------------
parameter vcod
vCodgen=substr(ITER.codartNEW,1,2)
vCodCla=substr(ITER.codartNEW,4,3)
vCodDet=substr(ITER.codartNEW,8,3)
DOQ = RECNO()
sele 2
seek vcod
pr = cod.inter_1
pt = cod.inter_2
SELE 1
IF PR<PT
   do while .t. 
      *seek ALLTRIM(cod+padl(alltrim(str(pr,3)),3,'0'))
      XT = padl(alltrim(str(pr,3)),3,'0')
      seek vCodGen+vcodcla+XT
      if !found() 
         vfun = .T.
         exit
      endif      
      PR = PR + 1
   enddo  
ELSE
  VFUN = .F.   
ENDIF 
SELE 1
GO TOP
GO DOQ

return  VFUN