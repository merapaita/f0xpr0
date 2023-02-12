
**Recrea Astpre de H/A **
**FMV**

set procedure to tes_lib
SET TALK ON
set escape on
USE HOJANU IN 1 ORDER HOJANU1 ALIAS HOJANU
USE ASTPre IN 2 ORDER ASTPRE19 ALIAS ASTPRE
DEFINE WINDOW XX FROM 01,01 TO 20,70 COLOR SCHEME 05 TITLE 'Procesando Contingencias'
ACTIVATE WINDOW XX
SELE HOJANU
SET FILT TO HOJANU.Tipdoc#'SU' AND HOJANU.tipdoc#'ME' AND hojanu.estado<>'99'
SET RELA TO NUMMES+NUMha+CODCTC INTO ASTPRE
GO TOP
SCAN
   sele astpre
   seek HOJANU.nummes+HOJANU.numha+HOJANU.codctc
   if !found()   
   	IF f_appd()
		REPLACE nummes  WITH HOJANU.nummes,;
		        tipdoc  WITH 'H/A',;
		        numref  WITH HOJANU.numha ,;
		        cuenta  WITH HOJANU.ctadeb,;
		        cTADEB  WITH HOJANU.ctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH HOJANU.fecHAN,;
		        CodPart WITH HOJANU.CodPart,;
		        CodCtc  WITH HOJANU.CodCtc;
		        CodCal  WITH HOJANU.CodCal,;
		        periodo WITH HOJANU.periodo,;
	    	    valdeb WITH HOJANU.valdeb ,;
	        	valhab WITH 0 
	ENDIF
   	IF f_appd()
		REPLACE nummes  WITH HOJANU.nummes,;
		        tipdoc  WITH 'H/A',;
		        numref  WITH HOJANU.numha ,;
		        cuenta  WITH HOJANU.ctaHAB,;
		        cTAHAB  WITH HOJANU.ctaHAB,;
		        tipo    WITH 'H',;
		        fecref  WITH HOJANU.fecHAN,;
		        CodPart WITH HOJANU.CodPart,;
		        CodCtc  WITH HOJANU.CodCtc;
		        CodCal  WITH HOJANU.CodCal,;
		        periodo WITH HOJANU.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH HOJANU.valhab
	ENDIF
    ?CHR(7)  
	?CHR(7)  
ELSE
    ?'E N C O N T R A D O'	
ENDIF
SELE HOJANU
?STR(RECNO(),10)
?NUMMES+numha+CODCTC
ENDSCAN
DEACTIVATE WINDOW XX
CLOSE ALL