*- Verifica el detalle de las H/C

USE HOJCON IN 1 ORDER TAG HOJCON1 ALIAS HOJA
USE ITEHC  IN 2 ORDER TAG ITEHC1  ALIAS ITEH

VDBF=SYS(3)+'.DBF'

SELECT HOJA
COPY STRUCTURE TO (VDBF)
USE (VDBF) IN 3 ALIAS TEMP

GO TOP
SELECT HOJA
SCAN
    SCATTER MEMVAR
    WAIT WINDOW 'Verificando HC: '+m.nummes+'.'+m.numhc NOWAIT
    SELECT ITEH
    SEEK m.nummes + m.numhc
    IF !FOUND()
       SELECT temp
       APPEND BLANK
       GATHER MEMVAR
   ENDIF
   SELECT HOJA
ENDSCAN

SELECT TEMP
IF !EOF()
   set filt to !empty(numhc)
   DO REPORTE WITH 2,'Verifdhc',' H/C sin detalle '
endif      
clos data
return

