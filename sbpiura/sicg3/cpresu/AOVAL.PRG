set exclu on
SET PATH TO ..\DATA
  ?'Cuentas'
  USE Cuentas EXCLUSIVE
  PACK
  =ordena("Cuenta","Cuentas1")
  =ordena("DContra","Cuentas2","!EMPTY(DContra)")
  =ordena("HContra","Cuentas3","!EMPTY(HContra)")
  =ordena("Cuenta","Cuentas4","UPPER(Detalle)='S'")
  =ordena("UPPER(Descri)","Cuentas5")
  =ordena("Cuenta","Cuentas6","Cuenta='9'")
  USE
  

RETURN


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""
