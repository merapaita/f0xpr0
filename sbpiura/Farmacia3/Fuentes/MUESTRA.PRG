PROCEDURE imp_cp
*---------------
PRIVATE VCON

SELECT ComPag
VCON = RECNO()
SCATTER MEMVAR
vNumCP = m.Nummes+m.NumCP
SET RELATION TO NUMMES + NUMCP INTO ITECP
*set filt to Nummes+NumCP=vNumCP
SET SKIP TO ITECP

if eof()
	do standby with vmens08
	return
 else
	do RepPrg  with "LisCP",' Comprobantes de Pago'
endif
set skip to
*set filter to
SET RELATION TO
SELECT IteCP
SET RELATION OFF INTO Maepre
*SELECT Reten
SET RELATION OFF INTO Maepre

SELECT ComPag
GO VCON
DO VISTA
RETURN


PROCEDURE LisCP  			&&Programa Reporte DEL C/P
*--------------
PARAMETER _desti
private nReg,cNum,xVal,cOrd,X_SW1,lVal
PUBLIC Tiene
IF _desti=2
	SET PRINTER TO (p_fil)
ENDIF
SET DEVICE TO PRINT
Impri    = .F.

xColumna = SPACE(7)

SELE ComPag
SEEK vNumCP

@ 00,00 SAY CHR(27)+CHR(64)
STORE 0 TO pagina,linea,xVal,X_SW1,lVal

nReg = RECNO()
SELE IteCP

SELECT 9
IF !USED("AstPat")
	USE astpat   IN 09  ORDER TAG astpat3  ALIAS astpat
ENDIF

SELE MaePre
cOrd1 = ORDER()
SET ORDER TO Maepre3
SELE IteCP
SET RELATION TO periodo+codcad INTO Maepre ADDITIVE

*USE cuentas  IN 14  ORDER TAG cuentas1 ALIAS cuenta
*USE reten    IN 12  ORDER TAG reten5   ALIAS reten
*USE astpre   IN 13  ORDER TAG astpre7  ALIAS astpre

lIni = .T.
SELE IteCP

SCAN WHILE IteCP.NumCP = ComPag.NumCP
	IF pagina = 0 .OR. linea > 60
		IF TIPDOC='IN'
			RETURN
		ENDIF
		DO Titulo
	ENDIF
	IF pagina = 1 AND lIni
		@Linea,08 SAY IIF(EMPTY(DESTINO),' ','Destino :')
		@Linea,19 SAY CHR(15)
		@Linea,22 SAY DESTINO
		@Linea,31 SAY CHR(18)
		Linea = Linea + 1
*		@Linea,0 SAY REPLICATE('Ä',80)
*		Linea = Linea + 1
		@Linea,0 SAY PADC("C O N C E P T O",80)
		Linea = Linea + 1
		@Linea,0 SAY REPLICATE('Ä',80)
		Linea = Linea + 1
		@Linea,01 SAY CHR(15)
		@Linea,03 SAY SUBSTR(GLOSA,1,130)
		Linea = Linea + 1
		@Linea,03 SAY SUBSTR(GLOSA,131,250)
		@Linea,32 SAY CHR(18)
		Linea = Linea + 1
*		@Linea,0 SAY REPLICATE('Ä',80)
*		Linea = Linea + 1
		@Linea,00 SAY PADC("OBSERVACIONES",80)
		Linea = Linea + 1
		@Linea,0 SAY REPLICATE('Ä',80)
		Linea = Linea + 1
		@Linea,01 SAY CHR(15)
		@Linea,03 SAY SUBSTR(OBSERV,1,130)
		@Linea,24 SAY CHR(18)
		Linea = Linea + 1
*		@Linea,0 SAY REPLICATE('Ä',80)
*		Linea = Linea + 1
		@Linea,00 SAY PADC("<< ESTADISTICA DIARIA OBJETO DEL GASTO >>",80)
		Linea=Linea+1
*		@Linea,0 SAY REPLICATE('Ä',84)
*		Linea = Linea + 1
		@ Linea,00  SAY CHR(15)
		@Linea,05 SAY "Cad.       UG UE Fun Prg Sprg Act/Pry                  Componente                          Partida           Parcial            Total"
		Linea = Linea + 1
		@Linea,00 SAY REPLICATE('Ä',137)
		Linea = Linea + 1
		lIni = .F.
	ENDIF
	DO CASE
		CASE linea < 48
			@ LINEA,00  SAY CHR(15)
			@ LINEA,05  SAY IteCP.CodCad
			@ LINEA,16  SAY MaePre.UniGes
			@ LINEA,19  SAY MaePre.UniEje
			@ LINEA,23  SAY Maepre.CODFUN
			@ LINEA,26  SAY Maepre.CodPrg
			@ LINEA,30  SAY MaePre.CodSpr
			@ LINEA,35  SAY MaePre.ActPry
			
			@ Linea,48  SAY IteCP.CodCom
			@ Linea,55  SAY IIF(EMPTY(itecp.codpart) ," ",IIF(CodPart = '0000RE','Pago de retenciones',VAL_PARA(iteCP.codcom,'CODCOM','D')))
*			@ Linea,55  SAY IIF(      iteCP.codPart='0000RE','Pago de retenciones',VAL_PARA(iteCP.codcom,'CODCOM','D'))
			@ Linea,97  SAY iteCP.CODPART				&&left(iteCP.CODPART,1)+'.'+SUBST(iteCP.CODPART,2,1)+'.'+SUBST(iteCP.CODPART,3,2)+'.'+SUBSTR(iteCP.codpart,5,2)+'.'+RIGHT(iteCP.CODPART,2)
			@ Linea,112 SAY iteCP.ImpParc PICTURE "999,999.99"
			xval=xval+IteCp.impparc
			linea = linea + 1
		CASE linea >= 48
			SKIP -1
			xReg = RECNO()
			@ linea,1 say Chr(18)
*			@ linea,00 say repli('-',80)
*			linea = linea + 1
			DO SUMARIO
			DO TITULO
			SELE IteCP
			GO xReg
			@ linea,01 say chr(18)
		OTHER
	ENDCASE
ENDSCAN
IF linea >= 45
	@ linea,00 say repli('-',80)
	linea = linea + 1
	DO SUMARIO
	DO TITULO
	linea = linea + 1
ENDIF

SELE ComPag
Linea=Linea-1
@Linea,0 SAY CHR(15)
@Linea,121 SAY xval PICTURE '@Z 999,999,999.99'
Linea=Linea+1
@Linea,110 SAY repli('-',25)
Linea=Linea+1
@Linea,109 SAY "      TOTAL :"    
@Linea,120 SAY xVal PICTURE '999,999,999.99'
Linea=Linea+1	
@Linea,109 SAY "DEDUCCIONES :"    
@Linea,120 SAY Reten PICTURE '999,999,999.99'
Linea=Linea+1
@Linea,109 SAY "    LIQUIDO :"    
@Linea,120 SAY xVal-reten PICTURE '999,999,999.99'
Linea = Linea + 1
SELE AstPre

cOrd = ORDER()

SET ORDER TO AstPre7
SEEK Compag.NumMes+Compag.NumCP+m.CodCtc

IF FOUND()
	IF linea >= 48
		@ linea,05 say repli('-',80)
		linea = linea + 1
		DO SUMARIO
		DO TITULO
		linea = linea + 1
	ENDIF
	Linea = Linea + 1
	@Linea,0 SAY CHR(18)
	@ linea,00 say repli('-',80)
	Linea = Linea + 1
	@Linea,0 SAY PADC("<< CONTABILIDAD PRESUPUESTAL >>",80)
	Linea = Linea + 1
	@ linea,00 say repli('-',80)
	Linea = Linea + 1
	@Linea,10 SAY "Cuentas"
	@Linea,52 SAY "Debe"
	@Linea,72 SAY "Haber"
	Linea = Linea + 1
	SCAN WHILE NumMes=ComPag.NumMes AND NumRef=ComPag.NumCP AND CodCtc=m.CodCtc
		IF linea > 60
			DO Titulo
		ENDIF
		DO CASE
			CASE linea < 48
				@ Linea,00  SAY CHR(18)
				@ Linea,04  SAY IIF(Tipo='D',CtaDeb,"")  PICTURE '!!!!!!!!!!!!!!!'
				@ Linea,14  SAY IIF(Tipo='H',CtaHab,"")  PICTURE '!!!!!!!!!!!!!!!'
				@ Linea,40  SAY IIF(Tipo='D',ValDeb,"")  PICTURE '99,999,999.99'
				@ Linea,60  SAY IIF(Tipo='H',ValHab,"")  PICTURE '99,999,999.99'
				linea = linea + 1
*				@Linea,13 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,26)',substr(CtaDeb,1,3))
			CASE linea >= 48
				SKIP -1
				@ linea,05 say repli('-',80)
				linea = linea + 1
				DO SUMARIO
				DO TITULO
				linea = linea + 1
		ENDCASE
	ENDSCAN
ENDIF

SELE Maepre
SET ORDER TO (cOrd1)

IF linea >= 48
	@ linea,05 say repli('-',80)
	linea = linea + 1
	DO SUMARIO
	DO TITULO
	linea = linea + 1
ENDIF

SELE AstPat

SEEK ComPag.NumMes+Compag.NumCP			&&+Compag.CodCtc

IF FOUND()
	IF linea >= 45
		@ linea,00 say repli('-',80)
		linea = linea + 1
		DO SUMARIO
		DO TITULO
		linea = linea + 1
	ENDIF
	SELE astpat
	Linea = Linea + 1
	@Linea,00 SAY CHR(18)
*	Linea = Linea + 1
	@ linea,00 say repli('-',80)
	Linea = Linea + 1
	@Linea,00 SAY PADC("<< CONTABILIDAD PATRIMONIAL >>",80)
	Linea=Linea+1
	@ linea,00 say repli('-',80)
	Linea = Linea + 1
	@Linea,05 SAY "Cta. Debe"
	@Linea,29 SAY "Importe"
	@Linea,43 SAY "Cuenta H"
	@Linea,68 SAY "Importe"
	Linea=Linea+1
	SCAN WHILE ASTPAT.NUMMES=Compag.NumMes AND ASTPAT.NUMREF=ComPag.NumCP		&& AND ASTPAT.CODCTC=ALLTRIM(VCTA)  
		IF linea > 60
			DO Titulo
		ENDIF
		DO CASE
			CASE linea < 48
				@Linea,00 SAY CHR(18)
				IF ASTPAT.TIPCTA="D" 
					@Linea,05 SAY ASTPAT.CODCTA
					@Linea,17 SAY ASTPAT.MTODEB PICTURE '999,999,999,999.99' + IIF(AstPat.Ret='S','  Ret.','')
				 ELSE
					@Linea,43 SAY ASTPAT.CODCTA
					@Linea,56 SAY ASTPAT.MTOHAB PICTURE '999,999,999,999.99' + IIF(AstPat.Ret='S','  Ret.','')
				ENDIF
				Linea=Linea+1
			CASE linea >= 48
*				@ linea,05 say repli('-',80)
*				linea = linea + 1
				DO SUMARIO
				DO TITULO
				SELE AstPat
				@ linea,00 say repli('-',80)
				Linea = Linea + 1
				@Linea,00 SAY PADC("<< CONTABILIDAD PATRIMONIAL >>",80)
				Linea=Linea+1
				@ linea,00 say repli('-',80)
				Linea = Linea + 1
				SKIP -1
		ENDCASE
	ENDSCAN
ENDIF

*IF linea >= 48
*	@ linea,05 say repli('-',80)
*	linea = linea + 1
	DO SUMARIO
*	DO TITULO
*	linea = linea + 1
*ENDIF

IF TIPDOC$'RESRRURSAR' AND ESTADO#'99'
	USE reten  IN 12  ORDER TAG reten5  ALIAS reten
	X_SW1=1
 ELSE
	USE reten  IN 12  ORDER TAG reten1  ALIAS reten
ENDIF

*SELE Maepre
*cOrd=ORDER()
*SET ORDER TO maepre1

SELE Reten
SET RELATION TO periodo+codcad INTO Maepre ADDITIVE
SELECT ComPag

SELECT RETEN	
SEEK ComPag.NumMes+Compag.NumCP

IF FOUND()
	DO cab_ret
	linea = linea + 1
	
	DO CASE
		CASE X_SW1=1
			YZ=1
			SCAN WHILE MESCPPG=ComPag.NumMes AND NUMCPPG=ComPag.NumCP AND CONPAGO='x'		&&AND CODCTC=VCTA 
*			SCAN WHILE MESCPPG=VANO AND NUMCPPG=VCLI AND CODCTC=VCTA AND CONPAGO='x'
				@Linea,01 SAY RETEN.NUMMES PICTURE '@J'
				@Linea,03 SAY '.'
				@Linea,04 SAY RETEN.NUMCP  PICTURE '@J'
				IF newsistem='1'
					=val_codcad(ALLTRIM(Reten.CodCad),Reten.Periodo+'01001','C')
				 ELSE
					=val_codca1(ALLTRIM(Reten.codcad),Reten.Periodo,'C')
				ENDIF
				IF NEWSISTEM='1'
					@Linea,12 SAY DISPHC()
					@Linea,30 SAY RETEN.CODCAD
				 ELSE
					@Linea,10 SAY RETEN.CODCAD+' '+maepre.codspr+'  '+maepre.actpry+' '+reten.codcom
				ENDIF
				
				@Linea,42 SAY dispglo(yZ)
				@Linea,72 SAY RETEN.VALRET PICTURE '@Z 9,999,999.99'
				
				IF Linea>=56
					DO cab_ret
					Linea=Linea-1
				ENDIF
				
				Linea=Linea+1
				YZ=YZ+1
				@Linea,42 SAY dispglo(YZ)
				Linea=Linea+1
				YZ=YZ+1
				@Linea,42 SAY dispglo(YZ)
				Linea=Linea+1
				YZ=1
				LVAL=LVAL+RETEN.VALRET
			ENDSCAN
		CASE X_SW1=0
			SCAN WHILE NUMMES=Compag.NumMes AND NUMCP=ComPag.NumCP		&& AND CODCTC=VCTA
				@Linea,03 SAY RETEN.CODRET PICTURE '@J'
				@Linea,15 SAY RETEN.NOMRET
				@Linea,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
				Linea=Linea+1
				LVAL=LVAL+RETEN.VALRET
			ENDSCAN
	ENDCASE
	Linea=Linea+1
	@Linea,01 SAY REPLICATE("-",78)
	Linea=Linea+1
	@Linea,30 SAY 'T O T A L  ---->'
	@Linea,67 SAY Lval PICTURE '@Z 9,999,999.99'
	Linea=Linea+1
	@Linea,01 SAY REPLICATE("-",78)
	IF vref='**********'
		DO cab_ret1
		LVAL=0
		SELECT RETEN
		SET ORDER TO RETEN1
		SEEK vkey
		SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
			@Linea,03 SAY RETEN.CODRET PICTURE '@J'
			@Linea,15 SAY RETEN.NOMRET
			@Linea,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
			Linea=Linea+1
			LVAL=LVAL+RETEN.VALRET
		ENDSCAN
		Linea=Linea+1
		@Linea,01 SAY REPLICATE("-",71)
		Linea=Linea+1
		@Linea,20 SAY 'T O T A L  ---->'
		@Linea,53 SAY Lval PICTURE '@Z 999,999,999.99'
		Linea=Linea+1
		@Linea,01 SAY REPLICATE("-",71)
	ENDIF
	@60,0 SAY CHR(12)
ENDIF

SELE Maepre
*SET ORDER TO (vord1)

if tiene > 1
	DO cab_cheq
	SELECT CHEQUE
	SEEK m.NumMes+m.NumCP
*	SEEK vkey
	XSON=1
	STORE 0 TO XTOT
	SCAN WHILE NUMMES=m.NumMes AND NUMCP=m.NumCP AND CODCTC=m.CodCtc
		@Linea,01 SAY XSON  PICTURE "999"
		@Linea,05 SAY CHEQUE.NUMCHQ 
		@Linea,20 SAY ALLTRIM(substr(CHEQUE.NOMGIR,1,30))
		@Linea,52 SAY CHEQUE.VALCHQ  PICTURE "99,999.99"
		@Linea,65 SAY "............"
		IF Linea>=56
			DO cab_cheq
			Linea=Linea-2
		ENDIF
		Linea=Linea+2
		XSON=XSON+1
		XTOT=XTOT+VALCHQ
	ENDSCA
	Linea=Linea+2
	@Linea,40 SAY "TOTAL : "
	@Linea,52 SAY XTOT   PICTURE "99,999.99"
	Linea=Linea+1
	@Linea,52 SAY "==========="
	Liena = Linea + 1
	@Linea,0 SAY CHR(12)
endif

SELE ComPag

SET DEVICE TO SCREEN
SET PRINTER TO
USE Clase    IN  8  ORDER TAG Clase1       ALIAS Clase

RETURN

PROCEDURE Titulo
*---------------
Pagina = Pagina + 1
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,66  SAY "PAG:"
@1,74  SAY ALLTRIM(STR(Pagina,8))
@2,3   SAY "LisCp"
*@2,64 SAY "FECHA:"
*@2,70 SAY DATE()      && timoteo no quiere
IF TIPDOC#'IN'
	@03,14 SAY CHR(14)
	@03,03 SAY '<< COMPROBANTE DE PAGO N§ ' + NUMCP + '.' + NUMMES + ' >>'
	@03,44 SAY CHR(27)+CHR(18)
*	@07,03 SAY "      N§ C/P :"
*	@07,20 SAY NUMCP+'.'+NUMMES
	@04,66 SAY "Estado :"
	@04,74 SAY IIF(Compag.Estado='00','Emitido',IIF(Compag.Estado='20','Girado',IIF(Compag.Estado='99','Anulado',IIF(Compag.Estado='50','Pagado',IIF(Compag.Estado='92','Inutilizado',IIF(Compag.Estado="10","Girado",' -  '))))))
	@05,03 SAY "   Fecha C/P :"
	@05,20 SAY FECCP picture "@D"
	@06,03 SAY "         Son :"
	@06,20 SAY LETRAS(Import-Reten,'SOLES')
	@07,03 SAY "Raz¢n Social :"
	@07,20 SAY IIF(tipdoc$"RESRRURSAR",codret,IIF((TIPPRV='O' OR EMPTY(TIPPRV) OR TIPDOC='RG' OR tipdoc='HM')," ",IIF((TIPPRV='P'and tipdoc<>'RG' AND tipdoc<>'HM'),Codprv,IIF(TIPDOC<>'RG' AND tipdoc<>'HM',Codemp,' '))))
	@07,27 SAY IIF(tipdoc$"RESRRURSAR",VAL_PARA(Codret,'CODRET','D',28,40),;
			IIF((tipprv='O' AND EMPTY(codotr) OR EMPTY(tipprv) OR tipdoc='RG' OR tipdoc='HM'),ComPag.nompre,;
			IIF((tipprv='P' AND Tipdoc<>'RG' AND tipdoc<>'HM'),val_auxi(ALLT(codprv),'20','D',40),;
			IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='E', VAL_AUXI(allt(Codemp),'03','D',40),;
			IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='I', VAL_AUXI(allt(Codpre),'80','D',40),;
			IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='O', VAL_AUXI(allt(Codotr),'09','D',40),;
			IIF(tipprv='R' AND !EMPTY(codotr) AND tipdoc='ME',VAL_PARA(ALLTRIM(Codotr),'CODRET','D',28,40),' ')))))))
	@08,03 SAY "  Referencia :"
	@08,20 SAY docref
	@08,24 SAY numref+' '+refer
	@09,03 SAY "     Cta.Cte.:"
	@09,20 SAY codctc
	@09,40 SAY val_fun('Caja','codctc','Descri',codctc)
	@10,03 SAY "     Fte.Fto.:"
	@10,20 SAY codfte
	@10,25 SAY ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))
	Linea = 11
 ELSE
	@05,14 SAY CHR(14)
	@5,03 SAY '<< COMPROBANTE DE PAGO N§ ' + NUMCP + '.' + NUMMES + ' >>'
	@05,44 SAY CHR(27)+CHR(18)
*	@07,03 SAY "      N§ C/P :"
*	@07,20 SAY NUMCP+'.'+NUMMES
	@06,03 SAY "   Fecha C/P :"
	@06,20 SAY FECCP picture "@D"
	@07,03 SAY "     Cta cte :"
	@07,20 SAY codctc
	@07,35 SAY val_fun('Caja','codctc','Descri',codctc)
	@10,14 SAY CHR(14)
	@10,18 SAY "I N U T I L I Z A D O"
	@11,01 SAY CHR(27)+CHR(18)
	Linea = 12
	SET DEVICE TO SCREEN
	SET PRINTER TO
ENDIF


*	IF incpres='S'
*		IF newsistem='1'
*			=val_codcad(ALLTRIM(vcad),vper+'01001','C')
*		 ELSE
*			=val_codca1(ALLTRIM(m.codcad),vper,'C')
*		ENDIF
*		@15,04 SAY "Cadena Func.:   UG  UE  FN PRG SBPRG ACTPRY"
*		@16,20 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
*		IF newsistem='2'
*			@17,04 SAY "        Meta: "
*			SELE IteCp
*			SEEK  vkey
*			@17,20 SAY IteCp.CodMet +' '+ Val_met1(vper+m.codcad,IteCp.codcom+IteCp.codmet,'D')
*			SELE ComPag
*		ENDIF
*		
*	ENDIF


PROCEDURE Sumario
*----------------
lINEA = 48
@Linea,3 SAY CHR(18)
@Linea,3 SAY "Ú"
@Linea,4 SAY REPLICATE("Ä",30)
@Linea,34 SAY "¿" 
@Linea,37 SAY "Ú"
@Linea,38 SAY REPLICATE("Ä",40)
@Linea,78 SAY "¿" 

Linea=Linea+1

@Linea,3 SAY "³"
@Linea,10 SAY " RECIBI CONFORME"
@Linea,34 SAY "³"
@Linea,37 SAY "³"
@Linea,78 SAY "³"

Linea=Linea+1

@Linea,3 SAY "³"
@Linea,34 SAY "³"
	
@Linea,37 SAY "³"
*@Linea,38 SAY " -----------------"
@Linea,59 SAY " -----------------"
@Linea,78 SAY "³"

Linea=Linea+1

@Linea,3 SAY "³"
@Linea,34 SAY "³"
@Linea,37 SAY "³"
*@Linea,39 SAY "CONTROL INTERNO "
@Linea,61 SAY "CAJERO GENERAL"
@Linea,78 SAY "³"

Linea=Linea+1

@Linea,3 SAY "³"
@Linea,6 SAY " ------------------------"
@Linea,34 SAY "³"
@Linea,37 SAY "³"
@Linea,78 SAY "³"

Linea=Linea+1

@Linea,3 SAY "³"
@Linea,34 SAY "³"
@Linea,37 SAY "³"
@Linea,78 SAY "³"

Linea=Linea+1
   
@Linea,3 SAY "³"
@Linea,4 SAY " DNI. :"
@Linea,34 SAY "³"
@Linea,37 SAY "³"
@Linea,38 SAY " -----------------"
@Linea,59 SAY " -----------------"
@Linea,78 SAY "³"

Linea=Linea+1
@Linea,3 SAY "³"
@Linea,4 SAY " FECHA :"
@Linea,12  SAY "  /  /"         
@Linea,34 SAY "³"
@Linea,37 SAY "³"
@Linea,39 SAY "VISACION CONTADOR"
@Linea,61 SAY "AUTORIZACION"
@Linea,78 SAY "³"
Linea=Linea+1
@Linea,3 SAY "À"
@Linea,4 SAY REPLICATE("Ä",30) 
@Linea,34 SAY "Ù"
@Linea,37 SAY "À"
@Linea,38 SAY REPLICATE("Ä",40)
@Linea,78 SAY "Ù" 
Linea=Linea+1
@Linea,12 SAY "N§ Cheque  :"
este=0     
SELE Cheque
SEEK m.NumMes+m.NumCP+m.CodCtc
*SEEK compag.NumMes+compag.NumCP+compag.CodCtc
*SEEK vkey
if found()   									&&  clave de busqueda
	este = recno()
endif
tiene = 0
do while NumMes = m.Nummes AND NumCP = m.Numcp AND CodCtc = m.Codctc	&&  si existe mas de un cheque		!eof() .and.
	SKIP
	Tiene=tiene+1
enddo
if tiene < 2
	if este<>0
		go este
	endif
	@ Linea,24 SAY iif(empty(Cheque.numchq),' Sin Cheque',Cheque.numchq)
 else
	@ Linea,24 SAY iif(empty(Cheque.numchq),' Sin Cheque',Cheque.numchq)
ENDIF

SELE ComPag
@Linea,40 SAY "LOGIN:"
@Linea,50 SAY disp_usu()
@Linea+1,03 say "N O T A:"
@Linea+1,12 say "SR.USUARIO SU CHEQUE"
*@Linea+1,40 SAY "Fecha:"
*@Linea+1,50 SAY Compag.FecDig
@Linea+2,03 say "VENCE A LOS 30 DIAS DE EMITIDO"
@Linea+2,40 SAY "Hora :"
@Linea+2,50 SAY Compag.HorDig
@Linea+3,0 SAY CHR(12)
linea = 1
RETURN



