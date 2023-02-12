*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
USE SHARED gc_pro00 ORDER codigo
SET FILTER TO pro_cosrep < CTOD('01/01/95')
MODIFY REPORT inven
CLOSE DATABASES
RETURN
*
FUNCTION kardex
= ooopen('gc_kar00',1)
SET FILTER TO MONTH(kar_fecdoc) = 1
GOTO TOP
SEEK pro_codpro
IF FOUND()
     wrk_stock = kar_stkant
ELSE
     wrk_stock = 0
ENDIF
= ooclose('gc_kar00')
RETURN wrk_stock
*
FUNCTION almacen
= ooopen('gc_alm00',1)
SEEK pro_codpro
IF FOUND()
     wrk_stock = alm_stkfis
ELSE
     wrk_stock = 0
ENDIF
= ooclose('gc_alm00')
RETURN wrk_stock
*
*** 
*** ReFox - retrace your steps ... 
***
