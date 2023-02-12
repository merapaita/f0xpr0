*------------------------------------------------------
* Sistema de Contabilidad Presupuestal
* Programa de Reconstrucci¢n de Asientos Presupuestales 
* Por Tipo de Documentos 
* Autor : FMC
* Fecha : 04/08/96
* ----- Para Comprobantes de Pago ------
*------------------------------------------------------
xMes     = '  '
xAno     = '  '
xMes     = PADL(ALLTRIM(STR(MONTH(DATE()),2)),2,'0')
xAno     = SUBSTR(STR(YEAR(DATE()),4),3,2)
vCodPrg  = '01'
vindidx  = SYS(03)+'.IDX'
OKCANCEL = 2
vtipdoc=1
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE '    An lisis   de   Cuentas    ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

	@ 01,01 SAY "           Periodo : " 
	@ 01,22 GET xAno
	@ 02,01 SAY "               Mes : " 
	@ 02,22 GET xMes
	@ 03,01 SAY "          Programa : " 
	@ 03,22 SAY vCodPrg
	
	@ 05,01 SAY "Tipo de Documeneto : " 
	@ 05,22 GET vtipdoc FUNCTION '^ Comprobantes de Pago;Hojas de Control ' 
	
	@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
	
	READ 
	RELEASE WINDOW LIS
	
 IF LASTKEY() # 27 .AND. OKCANCEL = 1
    DO CASE 
       CASE  vtipdoc=1
	   		 DO ESPERA WITH 1,' Recreando Asientos Presupuestales  de Comprobantes de Pago ... '
		     USE COMPAG  IN 1 ORDER COMPAG1  ALIAS Compag				
		     USE AstPre  IN 2 ORDER ASTPRE7  ALIAS Astpre
	
			 SELE Astpre
			 INDEX ON NUMMES+NUMREF+CODCTC+TIPO TO (vindidx) FOR Tipdoc='C/P'  AND SUBSTR(Astpre.CodCal,8,2)=ALLTRIM(vCodprg) and Nummes=xMes

	         * Falta la Rutina del Borrado de los C/P
	         
			 SELE COMPAG
		  	 SET FILTER TO Compag.Nummes=Xmes AND Compag.Estado#'9992'  AND (Compag.Tipdoc<>'ME' AND  Compag.Tipdoc<>'SU' AND Compag.Tipdoc<>'IN') and SUBSTR(Compag.CODCAL,8,2)=ALLTRIM(vCODprg) 
			 GO TOP

		SCAN
    		 SELE AstPre
	    	 IF SEEK (Compag.Nummes+Compag.Numcp+Compag.Codctc+'D')
	    	    Bloquea = f_lock(1)
		     ELSE
    		    Bloquea = f_appd()
		     ENDIF
    		 IF Bloquea
    		    REPLACE Periodo WITH xano
    	    	REPLACE Nummes  WITH Compag.Nummes
	    	    REPLACE Numref  WITH Compag.NumCp
		        REPLACE Codctc  WITH Compag.Codctc
    		    REPLACE TipDoc  WITH 'C/P'
		        REPLACE Tipo    WITH 'D'
    		    REPLACE CodCal  WITH Compag.Codcal
        		REPLACE Fecref  WITH compag.Fecref
		        REPLACE Codpart WITH Compag.Codpart
    		    REPLACE Ctadeb  WITH IIF(Compag.Codfte='PRP','9030101020','9030101040')
	    	    REPLACE Cuenta  WITH IIF(Compag.Codfte='PRP','9030101020','9030101040')
				REPLACE valdeb  WITH Compag.valdeb-COMPAG.RETEN
		     ENDIF
		     UNLOCK
     
    		 IF SEEK (Compag.Nummes+Compag.Numcp+Compag.Codctc+'H')
	    	    Bloquea = f_lock(1)
	    	 ELSE
    	    	Bloquea = f_appd()
	    	 ENDIF
	    	 IF Bloquea
    		    REPLACE Periodo WITH xano
        		REPLACE Nummes  WITH Compag.Nummes
	        	REPLACE Numref  WITH Compag.NumCp
	    	    REPLACE Codctc  WITH Compag.Codctc
    	    	REPLACE TipDoc  WITH 'C/P'
	    	    REPLACE Tipo    WITH 'H'
	    	    REPLACE CodCal  WITH Compag.Codcal
		        REPLACE Fecref  WITH compag.Fecref
    		    REPLACE Codpart WITH Compag.Codpart
		        REPLACE Ctahab  WITH IIF(Compag.Codfte='PRP','9040101020','9040101040')
    		    REPLACE Cuenta  WITH IIF(Compag.Codfte='PRP','9040101020','9040101040')
				REPLACE valhab  WITH Compag.Valhab-Compag.Reten
		     ENDIF
		    UNLOCK  
	    	@ 0,0 SAY  Compag.Nummes+' '+Compag.Numcp+' '+Compag.Codctc
	    	SELE compag 
	ENDSCAN
	DO ESPERA WITH 2
	
	CASE vtipdoc=2

	   		 DO ESPERA WITH 1,' Recreando Asientos Presupuestales  de Hojas de Control ... '
	   		 
		     USE Hojcon  IN 1 ORDER hojcon1  ALIAS Hojcon
		     USE AstPre  IN 2 ORDER ASTPRE5  ALIAS Astpre
	
			 SELE Astpre
			 INDEX ON NUMMES+NUMREF+TIPO TO (vindidx) FOR Tipdoc='H/C'  AND SUBSTR(Astpre.CodCal,8,2)=ALLTRIM(vCodprg) and Nummes=xMes

 			 * Falta la Rutina del Borrado de los H/C
			 SELE Hojcon
		  	 SET FILTER TO  Hojcon.Estado#'99'  AND SUBSTR(Hojcon.CODCAL,8,2)=ALLTRIM(vCODprg) AND Nummes=xMes
			 GO TOP

		SCAN
		
    		 SELE AstPre
	    	 IF SEEK (Hojcon.Nummes+Hojcon.Numhc+'D')
	    	    Bloquea = f_lock(1)
		     ELSE
    		    Bloquea = f_appd()
		     ENDIF
	    	 IF Bloquea
    		    REPLACE Periodo WITH xano
        		REPLACE Nummes  WITH Hojcon.Nummes
	        	REPLACE Numref  WITH Hojcon.Numhc
    	    	REPLACE TipDoc  WITH 'H/C'
	    	    REPLACE Tipo    WITH 'D'
	    	    REPLACE CodCal  WITH Hojcon.Codcal
		        REPLACE Fecref  WITH Hojcon.Fechc
    		    REPLACE Codpart WITH Hojcon.Codpart
		        REPLACE CtaDEB  WITH IIF(Hojcon.Codfte='PRP','9010101020','9010101040')
    		    REPLACE Cuenta  WITH IIF(Hojcon.Codfte='PRP','9010101020','9010101040')
				REPLACE valDEB  WITH Hojcon.Imptot
		     ENDIF
		     UNLOCK
     
    		 IF SEEK (Hojcon.Nummes+Hojcon.Numhc+'H')
	    	    Bloquea = f_lock(1)
	    	 ELSE
    	    	Bloquea = f_appd()
	    	 ENDIF
	    	 IF Bloquea
    		    REPLACE Periodo WITH xano
        		REPLACE Nummes  WITH Hojcon.Nummes
	        	REPLACE Numref  WITH Hojcon.Numhc
    	    	REPLACE TipDoc  WITH 'H/C'
	    	    REPLACE Tipo    WITH 'H'
	    	    REPLACE CodCal  WITH Hojcon.Codcal
		        REPLACE Fecref  WITH Hojcon.Fechc
    		    REPLACE Codpart WITH Hojcon.Codpart
		        REPLACE Ctahab  WITH IIF(Hojcon.Codfte='PRP','9030101020','9030101040')
    		    REPLACE Cuenta  WITH IIF(Hojcon.Codfte='PRP','9030101020','9030101040')
				REPLACE valhab  WITH Hojcon.Imptot
		     ENDIF
		    UNLOCK  
	    	@ 0,0 SAY  Hojcon.Nummes+' '+Hojcon.NumHC+' '+Hojcon.CodFte
	    	SELE Hojcon 
	ENDSCAN
	DO ESPERA WITH 2
	
	ENDCASE
	
ENDIF
CLOS DATA

RETURN
