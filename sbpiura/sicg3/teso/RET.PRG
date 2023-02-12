	SELE ComPag
	FILA=FILA-1
	@fila,62 SAY xval PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+2
	@FILA,40 SAY "      TOTAL :"    
	@fila,62 SAY Tval PICTURE '999,999,999,999.99'
	FILA=FILA+1	
	@FILA,40 SAY "DEDUCCIONES :"    
	@FILA,62 SAY Reten PICTURE '999,999,999,999.99'
	FILA=FILA+1
	@FILA,40 SAY "    LIQUIDO :"    
	@FILA,62 SAY IMPORT-reten PICTURE '999,999,999,999.99'
	FILA=FILA+2
	&&&
	IF FILA >=52
		FILA =3
	ENDIF
	@FILA,25 SAY "<< CONTABILIDAD PATRIMONIAL >>"
	FILA=FILA+1
	@FILA,08 SAY "Cuenta D"
	@FILA,32 SAY "Importe"
	@FILA,46 SAY "Cuenta H"
	@FILA,72 SAY "Importe"
	FILA=FILA+1
	SELE ASTPAT
	SEEK vkey
	SCAN WHILE ASTPAT.NUMMES=ALLTRIM(VANO) AND ASTPAT.NUMREF=VCLI AND ASTPAT.CODCTC=ALLTRIM(VCTA)  
	IF ASTPAT.RET='N'	
		IF ASTPAT.TIPCTA="D" 
		   @FILA,06 SAY ASTPAT.CODCTA
	       @FILA,20 SAY IIF(LEFT(ALLT(ASTPAT.CODCTA),2)='10',ASTPAT.MTODEB,ASTPAT.MTODEB-ComPag.RETEN) PICTURE '999,999,999,999.99'
	   	ELSE
	   	   @FILA,44 SAY ASTPAT.CODCTA
	   	   @FILA,60 SAY ASTPAT.MTOHAB PICTURE '999,999,999,999.99'
	   ENDIF
	   FILA=FILA+1
   ENDIF	   
   ENDSCAN



**************

PROCEDURE ingap   && F5
*--------------
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
IF !FOUND()
      DO agriaut
ENDIF

ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)

BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber ,vret
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
		IF RET='S'
		   vret=vret+MTOHAB
        ENDIF		   
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
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
IF ingreso .AND. m.IncPres='S'

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
				Tipcta WITH 'D', periodo WITH m.periodo
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
	DO vis_cen
ELSE
	* si es corrije solo modifica
	SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND()
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
		    IF codcta='101010101000'
	        	REPLA Mtodeb WITH mmonto, Fecha WITH m.feccp
	        	REPLA Mtohab WITH mmonto, Fecha WITH m.feccp
	        ENDIF
		ENDSCAN
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
	ENDIF	
	DO vis_cen
ENDIF	
USE IN 09 
USE IN 14
ON KEY
m.Reten = vret
m.PartRet = vvpartret
IF !EMPTY(m.reten)
	ACTIVATE WINDOW wind_12
	@0,1 SAY "Partida" GET m.partret PICTURE '!!!!!!'
    @0,30 SAY "Reten. "+STR(m.reten,10,2)
	READ
ENDIF	
DEACTIVATE WINDOW wind_12
UNLOCK
RETURN
