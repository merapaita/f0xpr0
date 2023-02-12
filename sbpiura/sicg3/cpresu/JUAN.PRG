USE HOJCON IN 1 ORDER TAG HOJCON1 ALIAS HOJA
USE AUXIL  IN 2 ORDER TAG AUXIL1  ALIAS AUXI
USE PARMAE IN 3 ORDER TAG PARMAE1 ALIAS PARMA


SELE HOJA
INDEX ON TIPDOC+TIPPRV+iif(tipprv='P',codprv,iif(tipprv='E',codemp,codotr))+NUMMES+NUMHC TO ZZ FOR ALLT(TIPDOC)$'O/C O/S AE AM' AND ESTADO='50'
SET RELA TO IIF(TIPPRV='P','20',IIF(TIPPRV='E','30','09')) + IIF(TIPPRV='P',CODPRV,IIF(TIPPRV='E',CODEMP,CODOTR)) INTO AUXI
DO REPORTE WITH 2,'JUAN',''
CLOS DATA
RETURN


FUNCTION SUMA
*-----------
vret=0
vreg=recno()
scatter memvar
sum imptot to vret for m.TIPPRV+iif(m.tipprv='P',m.codprv,iif(m.tipprv='E',m.codemp,m.codotr)) = hoja.TIPPRV+iif(hoja.tipprv='P',hoja.codprv,iif(hoja.tipprv='E',hoja.codemp,hoja.codotr))
GOTO vreG
RETURN vret
