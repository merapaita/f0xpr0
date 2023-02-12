*** CUADRE DE INFORMACION CONTABLE
*
** DEL C/P AL ASTPAT
clear
set talk off
set echo off
*
USE ParMae   IN  1  ORDER TAG ParMae1      ALIAS ParMa 
USE ComPag   IN  3  ORDER TAG ComPag1      ALIAS ComPag 
USE Cajas    IN 10  ORDER TAG Cajas2       ALIAS Caja
USE IteCp    IN  4  ORDER TAG IteCp1       ALIAS IteCp  
*
*- Variables de trabajo (registro a trabajar)
PUBLIC m.CodCtc,w_tipctc,w_monto,xmri,xnri,vcodprg,vcodsub,vcodact,vcodanal,mmonto,sw_z,w_ctaD,mcta10,mcond,mctaf,mctai
PUBLIC vctadeb,vctahab,vvaldeb,vvalhab,vuser_id,wuser_id
STORE SPACE(1)  TO mcta10,mcond
STORE SPACE(5)  TO vcodanal
STORE SPACE(2)  TO vcodprg,xmri
STORE SPACE(3)  TO vcodsub,vcodact
STORE SPACE(4)  TO xnri
STORE SPACE(12) TO w_ctaD,mctai,vctadeb,vctahab
STORE SPACE(12) TO mctaf
STORE 0 	    TO w_monto,mmonto,sw_z,vvaldeb,vvalhab
SCATTER MEMVAR
*
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT compag
*
** DEL C/P AL ASTPAT
*
mesw='09'
seek mesw
do while nummes=mesw and .not. eof()
	if estado='99' OR DELETE()
		skip
		loop
	endif		
	m.nummes=nummes
	m.numcp=numcp
	m.codctc=codctc
	m.tipdoc=tipdoc
	m.incpres=incpres
	m.fecref=fecrec
	m.feccp=feccp
	m.periodo=periodo
	sele itecp
	SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
	if not !found()
		w_part=IteCp.CodPart
	endif		
	SELE Caja
	SEEK m.codctc
	w_tipctc=Caja.Tipo
	sele astpat
	SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
	IF !FOUND()
		DO agriaut				&&&&&& ok.
	ELSE
		store 0 to vdebe,vhaber
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
        	DO CASE
	          CASE SUBS(codcta,1,3)='104' OR SUBS(codcta,1,3)='108' .OR. SUBS(CODCTA,1,2)='44'
         		mmonto=Mtohab
    	      CASE SUBS(codcta,1,3)='102'
           		m.flagcon='F'
        	  CASE SUBS(codcta,1,3)='384'
           		m.flagcon='A'
           	  CASE SUBS(codcta,1,3)='385'
           		m.flagcon='E'
			ENDCASE
		ENDSCAN
		* si es corrije solo modifica
		sw=0
		SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
		IF FOUND()
			SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
			    IF codcta='101010101000'
			    	sw=sw+1
		        	REPLA Mtodeb WITH mmonto, Fecha WITH m.feccp
		        	REPLA Mtohab WITH mmonto, Fecha WITH m.feccp
		        	tipw=tipcta
		        ENDIF
			ENDSCAN
	    endif
	    DO CASE 
    		CASE SW=1
	    	  if tipw='D'
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtohab WITH mmonto,;
			        Tipcta WITH 'H',;
			        periodo WITH m.periodo
				ENDIF
			  ELSE
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtodeb WITH mmonto,;
			        Tipcta WITH 'D',;
			        periodo WITH m.periodo
				ENDIF
			  ENDIF				
			CASE SW=0
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtodeb WITH mmonto,;
			        Tipcta WITH 'D',;
			        periodo WITH m.periodo
				ENDIF
				IF f_appd()
					REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '101010101000',;
			        Mtohab WITH mmonto,;
			        Tipcta WITH 'H',;
			        periodo WITH m.periodo
				ENDIF
		ENDCASE
	ENDIF
	SELE COMPAG
	SKIP
ENDDO
*RETURN
**
*** DEL ASTPAT AL C/P		
*
SELE ASTPAT
seek mesw
do while nummes=mesw and .not. eof()
	if DELETE()
		skip
		loop
	endif		
	m.nummes=NUMMES
	m.numREF=NUMREF
	m.codctc=CODCTC
	SELE COMPAG
	SEEK ALLTRIM(m.nummes)+m.numREF+ALLTRIM(m.codctc)
	IF !FOUND()
		@ 05,10 SAY M.NUMMES
		@ 05,13 SAY M.NUMREF
		@ 05,20 SAY M.CODCTC
		WAIT ' '
	ENDIF
	SELE ASTPAT
	SKIP
ENDDO
RETURN	
******
				
