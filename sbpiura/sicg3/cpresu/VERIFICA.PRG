USE astpre   IN 1  order tag Astpre8  ALIAS AstPre
USE Parmae   IN 2  order tag Parmae1  ALIAS Parma
USE Compag   IN 5  order tag Compag1  ALIAS Compag
USE Cuentas  IN 3  order tag Cuentas1 ALIAS Cuenta
USE Hojcon   in 4  order tag Hojcon1  ALIAS hoja
USE Recing   in 6  order tag Recing1  ALIAS Recing
USE Hojmod   in 7  order tag HojMod1  ALIAS Hojmod
USE PteAnu   in 8  order tag PteAnu1  ALIAS Pteanu
USE Hojanu   in 10  order tag Hojanu2  ALIAS Hojanu
USE Movbco   in 11  order tag MOVBCO1  ALIAS MOVBCO

vcodprg='  '
vperiodo='  '
vmes='  '
DEFINE WINDOW LIS_1 FROM 10,13 TO 16,67 DOUBLE ;
TITLE ' °° Listado Nota de Contabilidad °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW LIS_1
 @  1,2 Say '  Periodo : ' GET vPeriodo PICTURE '!!'  VALID !empty(vperiodo)
 @  2,2 Say ' Programa : ' GET vCodPrg  PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',15,30)
 @  3,2 Say '      Mes : ' GET vMes     PICTURE '!!'   VALID VAL_PARA(vMes   ,'FECMES',' ',15,30)
READ
DEACTIVATE WINDOW LIS_1
 
 vCodPrg=ALLTRIM(vCodPrg)
 vMes=ALLTRIM(vMes)    
 
 if lastkey()= 27 
      CLOSE DATA
      return
 else
 	do corrige
 endif


Procedure corrige
*---------------- 	
vIdxhc = sys(3)+".idx"
vInd   = sys(3)+".DBF"


SELE 9
CREATE TABLE (vInd);
  (Tipo      C(03),;
   Numref    C(04),;
   TipErr    C(01))

USE (vind) in 9 alias vtempo

*HOJA DE CONTROL
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='H/C' and PERIODO=vPeriodo and Nummes=vmes
SELE Hoja
SET FILTER TO Hoja.Nummes=vmes AND Hoja.Estado#'99' and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP
SCAN 
   @ 1,10 SAY 'Leyendo Hojas de Control...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK Hoja.Nummes+Hoja.Numhc
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH 'H/C',NUMREF WITH HOJA.NUMHC, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE Hoja.Nummes+Hoja.Numhc = AstPre.Nummes+AstPre.Numref 
         	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (HOJA.IMPTOT*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH 'H/C',NUMREF WITH HOJA.NUMHC, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele Hoja
ENDSCAN

*COMPROBANTE
SELE Astpre
INDEX ON NUMMES+NUMREF+CODCTC TO (vidxhc) FOR Tipdoc='C/P' and PERIODO=vperiodo and Nummes=vmes
SELE Compag
SET FILTER TO Compag.Nummes=vmes AND Compag.Estado#'99 92'  AND Compag.Tipdoc#'SUMERU' and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN 
   @ 1,10 SAY 'Leyendo Comprobantes de Pago...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK Compag.Nummes+compag.Numcp+ALLTRIM(Compag.Codctc)
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH compag.NUMCP, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE  Compag.Nummes+compag.Numcp+ALLTRIM(Compag.Codctc)= AstPre.Nummes+AstPre.Numref + ALLTRIM(AstPRe.Codctc)
         	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (compag.IMPOrT*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH compag.NUMCP, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele compag
ENDSCAN

*RECIBOS DE INGRESO
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='R/I' and PERIODO=vPeriodo and Nummes=vmes
SELE RECING
SET FILTER TO Recing.Nummes=vmes AND Recing.Estado#'99' AND SUBSTR(Recing.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN 
   @ 1,10 SAY 'Leyendo Recibos de Ingresos...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   seek Recing.Nummes+Recing.NumRI
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH Recing.NUMri, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE Recing.Nummes+Recing.NumRI = AstPre.Nummes+AstPre.Numref 
         	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (recing.canri*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH Recing.NumRI, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele Recing
ENDSCAN

*Partes de Anulaci¢n 
SELE Astpre
INDEX ON NUMMES+NUMREF+TIPDOC+ANUREB TO (vidxhc) FOR Tipdoc='P/A  P/R' and PERIODO=vperiodo and Nummes=vmes
SELE PteAnu
INDEX ON Periodo+Tipo+Tipdoc+Mespa+Numpa TO (VIDXHC2) FOR MONTH(PteAnu.fecpa)=VAL(vmes) AND PteAnu.Estado#'99' and SUBSTR(Pteanu.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN 
   @ 1,10 SAY 'Leyendo Partes de Anulaci¢n ...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK vmes+PteAnu.Numpa+Pteanu.Tipdoc+Pteanu.Anureb
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH PteAnu.Numpa, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE vmes+PteAnu.Numpa+Pteanu.Tipdoc+Pteanu.Anureb = AstPre.Nummes+AstPre.Numref+AstPre.Tipdoc+AstPre.Anureb
   		   	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (pteanu.valtot*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH PteAnu.Numpa, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele PteAnu
ENDSCAN

*Hojas de Modificaci¢n 
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc='H/M' and PERIODO=vperiodo and Nummes=vmes
SELE Hojmod
SET FILTER TO HojMod.Nummes=vmes AND HojMod.Estado#'99'  and SUBSTR(hojmod.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN 
   @ 1,10 SAY 'Leyendo Hojas de Modificaci¢n  ...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK HojMod.Nummes+HojMod.NumHM
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH HojMod.NumHM, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE HojMod.Nummes+HojMod.NumHM = AstPre.Nummes+AstPre.Numref 
   		   	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (HojMod.imptot*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH HojMod.NumHM, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele HojMod
ENDSCAN


*hojas de anulacion de cheque
SELE Astpre
INDEX ON NUMMES+NUMREF+alltrim(CODCTC) TO (vidxhc) FOR Tipdoc='H/A' and PERIODO=vperiodo and Nummes=vmes
SELE Hojanu
SET FILTER TO HojAnu.Nummes=vmes AND HojAnu.Estado#'99' AND SUBSTR(hojanu.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN 
   @ 1,10 SAY 'Leyendo hojas de anulacion de cheque ...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK Hojanu.Nummes+Hojanu.NumHa+ALLTRIM(hojanu.Codctc)
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH Hojanu.NumHa, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE Hojanu.Nummes+Hojanu.NumHa+ALLTRIM(hojanu.Codctc)= AstPre.Nummes+AstPre.Numref + Alltrim(astpre.Codctc)
   		   	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (Hojanu.import*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH Hojanu.NumHa, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele Hojanu
ENDSCAN

*movimiento bancario
SELE Astpre
INDEX ON NUMMES+NUMREF TO (vidxhc) FOR Tipdoc$'N/C  N/A  NOC  B/D' and PERIODO=vperiodo and Nummes=vmes
SELE Movbco
SET FILTER TO Movbco.Nummes=vmes AND Movbco.Estado#'99' AND SUBSTR(Movbco.CODCAL,8,2)=ALLTRIM(vCODprg)
GO TOP

SCAN 
   @ 1,10 SAY 'Leyendo movimiento bancario ...'+STR(RECNO(),4) color W* 
   SELE AstPRe
   SEEK Movbco.Nummes+Movbco.Numero
   IF !FOUND()
   		SELE VTEMPO
   		APPEND BLANK
   		REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH Movbco.Numero, TIPERR WITH '1'
   ELSE			
   		VSUMA=0
   		SCAN WHILE Movbco.Nummes+Movbco.Numero= AstPre.Nummes+AstPre.Numref 
		   	VSUMA=VSUMA+ValDeb+ValHAB
   		ENDSCAN
   		IF (Movbco.monto*2)<>VSUMA
	   		SELE VTEMPO
	   		APPEND BLANK
   			REPLACE TIPO WITH astpre.tipdoc,NUMREF WITH Movbco.Numero, TIPERR WITH '2'
   		ENDIF			
   	ENDIF	
	Sele Movbco
ENDSCAN

SELE ASTPRE
SET INDEX TO
* CLOS DATA
* ERASE (vind) 
ERASE (vIdxhc)