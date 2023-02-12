CLOSE DATA
USE HojMOD   IN 1   order tag HojMOD1  ALIAS Hoja
USE Astpre   IN 2   order tag ASTPRE5  ALIAS ASTPRE
SELE HOJA
SET RELATION TO NUMMES+NUMHC+'H/M' INTO ASTPRE
VREC = RECNO()
GO TOP
DO WHILE !EOF()
       IF !EOF('ASTPRE')
       
          IF IIF(ASTPRE.TIPO='D',HOJA.ImpTot = ASTPRE.VALDEB,HOJA.ImpTot = ASTPRE.VALHAB) 
          	  REPLA ASTPRE.ESTADO WITH 'OK'	
          	  SKIP 1 IN 2
          	  IF IIF(ASTPRE.TIPO='D',HOJA.ImpTot = ASTPRE.VALDEB,HOJA.ImpTot = ASTPRE.VALHAB)
          	      REPLA ASTPRE.ESTADO WITH 'OK'
          	  ENDIF    
          ENDIF	   
       ELSE
      	    ?'->',CTADEB,CTAHAB
    	    ? 'NO HALLADO..'
       ENDIF
       SELE HOJA
       SKIP
       IF EOF()
       	  EXIT
       ENDIF	  
ENDDO
GO TOP
GO VREC
RETURN