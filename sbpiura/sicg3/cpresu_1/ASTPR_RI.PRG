
**Recrea Astpre de R/I **
**FMV**

set procedure to tes_lib
SET TALK ON
set escape on
USE RECING IN 1 ORDER RECING1 ALIAS RI
USE ASTPre IN 2 ORDER ASTPRE20 ALIAS ASTPRE
USE PARMAE IN 3 ORDER PARMAE1 ALIAS PARMA
use iteri  in 4 order iteri1 alias iter

DEFINE WINDOW XX FROM 01,01 TO 20,70 COLOR SCHEME 05 TITLE 'Procesando Contingencias'
ACTIVATE WINDOW XX
SELE RI
SET RELA TO PERIODO+NUMMES+NUMRI+CODCTC INTO ASTPRE
SET RELA TO PERIODO+NUMMES+NUMRI INTO iter addi
GO TOP
SCAN
   vtipo=RI.tipri
   SELE  parma
   SEEK 'TIPRI '+vtipo
   IF FOUND()
      vsi=LEFT(ALLT(parma.descriau2),1)
      Vcla=IIF(vsi='S',SUBS(ALLT(parma.descriau2),4,9),space(11))
   ELSE
      ?'NO ENCUENTRA PARMAE'
   ENDIF
   IF vsi='S'      
       sele iter
       SCAN WHILE iter.periodo=ri.periodo and iter.nummes=ri.nummes and iter.numri=ri.numri 
			replace iter.tipo with 'P'
			replace iter.codpart with vcla
	   ENDSCAN		      		
	   sele astpre
	   seek ri.periodo+ri.nummes+ri.numri+ri.codctc
	   if !found()   
		   	IF f_appd()
				REPLACE nummes  WITH ri.nummes,;
			        tipdoc  WITH 'R/I',;
		    	    numref  WITH ri.numri ,;
			        cuenta  WITH IIF(val(ri.nummes)<=3,IIF(ri.codfte='PRP','9120101000','9140101000'),IIF(ri.codfte='PRP','9010101020','9010101040')),;
			        cTADEB  WITH IIF(val(ri.nummes)<=3,IIF(ri.codfte='PRP','9120101000','9140101000'),IIF(ri.codfte='PRP','9010101020','9010101040')),;
			        tipo    WITH 'D',;
		    	    fecref  WITH ri.fecRI,;
			        CodCtc  WITH ri.CodCtc;
			        CodCal  WITH ri.CodCal,;
			        CodPart WITH VCLA,;
		    	    periodo WITH ri.periodo,;
		    	    valdeb  WITH ri.Canri ,;
		        	valhab  WITH 0 
			ENDIF
		   	IF f_appd()
			REPLACE nummes  WITH ri.nummes,;
			        tipdoc  WITH 'R/I',;
			        numref  WITH ri.numRI ,;
		    	    cuenta  WITH IIF(val(ri.nummes)<=3,IIF(ri.codfte='PRP','9020101000','9040101000'),IIF(ri.codfte='PRP','9020101020','9020101040')),;
		        	cTAHAB  WITH IIF(val(ri.nummes)<=3,IIF(ri.codfte='PRP','9020101000','9040101000'),IIF(ri.codfte='PRP','9020101020','9020101040')),;
			        tipo    WITH 'H',;
			        fecref  WITH ri.fecRI,;
			        CodPart WITH VCLA,;
			        CodCtc  WITH ri.CodCtc;
			        CodCal  WITH ri.CodCal,;
			        periodo WITH ri.periodo,;
				    valdeb WITH 0 ,;
				    valhab WITH ri.CANRI
			ENDIF
		    ?CHR(7)  
			?CHR(7)  
	ELSE
	    ?'E N C O N T R A D O'	
	ENDIF
  ELSE
     ?'SIN ASIENTOS'
ENDIF     	
	
SELE ri
?STR(RECNO(),10)
?NUMMES+NUMri+CODCTC
ENDSCAN
DEACTIVATE WINDOW XX
CLOSE ALL