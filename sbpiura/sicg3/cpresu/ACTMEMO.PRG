* Actualiza el campo Observaciones de las Hojas de Control
* de Enero a Mayo.
*---------------------------------------------------------

USE HojCon           In 1 ORDER TAG Hojcon1 ALIAS hoja1
USE H:\TABLAS\Hojcon In 2 ORDER TAG Hojcon1 ALIAS hoja2

* actualizaci¢n: Hoja2->Hoja1

SELECT hoja2
SET FILTER TO BETWEEN(hoja2.nummes,'01','05') AND periodo='97'
SCAN
     SCATTER FIELD observ MEMO MEMVAR
     vnummes = hoja2.nummes
     vnumhc  = hoja2.numhc
     SELECT hoja1
     SEEK vnummes+vnumhc
     WAIT WINDOW "Procesando ==> "+vnummes+vnumhc NOWAIT
     IF FOUND()
        IF F_LOCK(1)
           GATHER MEMVAR FIELD observ MEMO
           UNLOCK ALL
        ENDIF
     ENDIF
     SELECT hoja2
ENDSCAN
SET FILTER TO
CLOS DATA
RETURN
     
     