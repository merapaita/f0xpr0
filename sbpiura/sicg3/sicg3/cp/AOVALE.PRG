USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc

SELE HOJA
SCAN
?NUMMES,'-',NUMHC
       SELECT ITEHC
       SEEK ALLTRIM(HOJA.NumMes) + HOJA.NumHc
	       SCAN WHILE NumMes = ALLTRIM(HOJA.NumMes) and NumHc = HOJA.NumHc
	           
       		    REPLACE CodFte  WITH  substr(HOJA.codcal,05,3),;
 			     		CodPrg  WITH  substr(HOJA.codcal,08,2),;
                      CodSubPR  WITH  substr(HOJA.codcal,10,3),;
                      TIPFUN WITH HOJA.TIPFUN
   				IF ALLTRIM(HOJA.TIPFUN)='I'
                  REPLACE CodProy WITH substr(HOJA.codcal,13,3),;
                   		  CodSupry With substr(HOJA.codcal,16,2),;
				  		  CodObra with substr(HOJA.codcal,18,2)
			    ELSE
				  REPLACE Codact with substr(HOJA.codcal,13,2)
		   	    ENDIF	  	  
	       ENDSCAN
    	   GO TOP
ENDSCAN