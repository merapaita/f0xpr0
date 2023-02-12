*- 	Traslada informaci¢n de H/C
*-  a la nueva estructura.


USE MAEPRE IN 1 ORDER TAG maepre1 Alias Maep
USE HOJCON IN 2 ORDER TAG hojcon1 Alias HojaN
USE ITEHC  IN 3 ORDER TAG itehc1  Alias ItehN
*-Estructuras anteriores:
USE H:\TABLAS\HOJCON IN 4 ORDER TAG HOJCON1 ALIAS hojaA 
USE H:\TABLAS\ITEHC  IN 5 ORDER TAG itehc1  ALIAS ItehA

vidx=SYS(3)+'.IDX'
SELECT maep
INDEX ON uniges+unieje+codfun+codprg+codspr+actpry TO (vidx)

SELECT HojaA
SET RELATION TO SUBS(HojaA.codcal,10,20) INTO Maep
SCAN
    SCATTER MEMVAR
    wait window 'Procesando HOJCON-> '+m.nummes+m.numhc nowait
    m.codcad=maep.codcad
    SELECT HojaN
    APPEND BLANK
    IF RLOCK()
       GATHER MEMVAR
    ENDIF
    UNLOCK ALL
    SELEC HojaA
ENDSCAN

SELECT itehA
SET RELATION TO nummes+numhc INTO HojaN
SCAN
    SCATTER MEMVAR
    m.codcad=hojaN.codcad
    m.codcom=STRTRAN(m.compone,'.','')
    m.codpart=m.codanal
    WAIT WINDOW 'Procesando ITEHC-> '+m.nummes+m.numhc nowait
    SELECT itehN
    APPEND BLANK
    IF flock()
       GATHER MEMVAR
    ENDIF
    UNLOCK ALL
    SELECT itehA
ENDSCAN    
CLOS DATA
ERASE(vidx)
RETURN
