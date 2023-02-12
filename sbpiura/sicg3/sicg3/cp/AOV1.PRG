SET PROC TO SAI_LIB
USE HOJCON IN 1 ALIAS HOJA
USE PARMAE IN 2
SELE 1
INDEX ON TIPDOC+NUMMES TO KK
SELE 2
INDEX ON CODIGO FOR TIPO='TIPDOC' TO ZZ

SELE 1
SET RELA TO TIPDOC INTO 2
DO REPORTE WITH 2,'MOV1','MOV.PRE'
RETURN





FUNCTION enMes
*-------------
parameter xmes
vnum=0
vreg=recno()
xtipdoc=hoja.tipdoc
seek xtipdoc+xmes
scan while xmes=hoja.nummes and xtipdoc=hoja.tipdoc
     if hoja.estado#'9'
        vnum=vnum+ 1
     endif
endscan
goto vreg
return vnum

     