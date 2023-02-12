
**Recrea Astpre de M/B **
**FMV**

set procedure to tes_lib
SET TALK ON
set escape on
USE MOVBCO IN 1 ORDER MOVBCO11 ALIAS MOVB
USE ASTPre IN 2 ORDER ASTPRE14 ALIAS ASTPRE
USE PARMAE IN 3 ORDER PARMAE1 ALIAS PARMA

DEFINE WINDOW XX FROM 01,01 TO 20,70 COLOR SCHEME 05 TITLE 'Procesando Contingencias'
ACTIVATE WINDOW XX
SELE MOVB
set filt to tipdoc='N/C' AND hojcon='S'
SET RELA TO NUMMES+NUMERO+CODCTC INTO ASTPRE
GO TOP
SCAN
	   sele   astpre
	   seek MOVB.nummes+MOVB.numero+MOVB.codctc
	   if !found()   
		   	IF f_appd()
				REPLACE nummes  WITH MOVB.nummesc,;
			        tipdoc  WITH movb.tipdoc ,;
		    	    numref  WITH MOVB.numero ,;
			        cuenta  WITH IIF(val(MOVB.nummesc)<=3,IIF(MOVB.codfte='PRP','9120101000','9140101000'),IIF(MOVB.codfte='PRP','9030101020','9040101040')),;
			        cTADEB  WITH IIF(val(MOVB.nummesc)<=3,IIF(MOVB.codfte='PRP','9120101000','9140101000'),IIF(MOVB.codfte='PRP','9030101020','9040101040')),;
			        tipo    WITH 'D',;
		    	    fecref  WITH MOVB.fechac,;
			        CodCtc  WITH MOVB.CodCtc;
			        CodCal  WITH MOVB.CodCal,;
			        CodPart WITH movb.codpart,;
		    	    periodo WITH MOVB.periodo,;
		    	    valdeb  WITH MOVB.monto ,;
		        	valhab  WITH 0 
			ENDIF
		   	IF f_appd()
			REPLACE nummes  WITH MOVB.nummesc,;
			        tipdoc  WITH MOVB.tipdoc,;
			        numref  WITH MOVB.numero ,;
		    	    cuenta  WITH IIF(val(MOVB.nummes)<=3,IIF(MOVB.codfte='PRP','9020101000','9040101000'),IIF(MOVB.codfte='PRP','9020101020','9020101040')),;
		        	cTAHAB  WITH IIF(val(MOVB.nummes)<=3,IIF(MOVB.codfte='PRP','9020101000','9040101000'),IIF(MOVB.codfte='PRP','9020101020','9020101040')),;
			        tipo    WITH 'H',;
			        fecref  WITH MOVB.fecha,;
			        CodPart WITH movb.codpart,;
			        CodCtc  WITH MOVB.CodCtc;
			        CodCal  WITH MOVB.CodCal,;
			        periodo WITH MOVB.periodo,;
				    valdeb WITH 0 ,;
				    valhab WITH MOVB.monto
			ENDIF
		    ?CHR(7)  
			?CHR(7)  
	ELSE
	    ?'E N C O N T R A D O'	
	ENDIF	   
SELE MOVB
?STR(RECNO(),10)
?NUMMES+NUMero+CODCTC
ENDSCAN
DEACTIVATE WINDOW XX
CLOSE ALL



