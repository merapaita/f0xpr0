*
use pili
INDEX ON ALLTRIM(NUMMES)+ALLTRIM(NUMERO) TO PILI1
*set index to pili1
*reindex
use iteri 
INDEX ON PERIODO+ALLTRIM(NUMMES)+ALLTRIM(NUMRI) TO ITERI1
*set index to iteri1
*reindex
use cajas
INDEX ON ALLTRIM(CODCTC) TO CAJAS1
*set index to cajas1
*reindex
use recing
INDEX ON ALLTRIM(NUMMES)+ALLTRIM(CODCTC) TO RECING1
*set index to recing1
*reindex
clos data
SELECT 1
use iteri index iteri1
sele 2
use pili index pili1
sele 3
use cajas index cajas1
sele 4
use recing INDEX RECING1
CLAW='087305006533'
SEEK '&CLAW.'
do while NUMMES+CODCTC=CLAW AND .not. eof()
    vpeRiodo = Periodo
    vnummes  = nummes
    vnumRi   = numRi
    ftew     = codfte
    ctcw     = codctc
    usuw     = usuario
    cadw     = codcad
    sele 3
    seek alltrim(ctcw)
    if .not. eof()
    	bcow=banco
	else
		bcow="***"
	endif
	sele 1
	SEEK vPeriodo+alltrim(vNumMes)+alltrim(vNumRi)
    *
    do while PERiODO+ALLTRiM(numMes) + ALLTRiM(NumRi) =vPERiODO+ALLTRiM(vNumMes) + ALLTRiM(vNumRi)
	    refw     = docref
	    fecw     = fecdep
	    bolw     = boldep
	    impw     = impparc
	    wperiodo = periodo
	    wnummes  = VNUMMES
	    wnumri   = numri
		SELECT 2
		SEEK ALLTRIM(VNUMMES)+ALLTRIM(refw)
		IF .NOT. eof()
			IF MONTO=IMPW
				SELE 1
				SKIP
				LOOP
			ENDIF
		ENDIF							
		numw=4100
		do while .t.
			numw=numw+1
			refw=str(numw,4)
			SEEK ALLTRIM(VNUMMES)+ALLTRIM(refw)
			if eof()
				exit
			endif
		enddo
		APPEND BLANK
		REPLACE PeRiodo 	WITH vpeRiodo ,;
		nummes  	WITH vnummes ,;
		NummesC 	WITH vnummes ,;
		numero  	WITH Refw ,;
		Transa  	WITH 'ABO' ,;
		Fecha   	WITH fecw,;
		FechaC  	WITH fecw,;
		Tipdoc  	WITH 'B/D' ,;
		Numref  	WITH bolw,;
		codfte  	WITH ftew ,;
		codctc  	WITH Ctcw ,;
		nombre  	WITH 'DEPOSITO AUTOMATICO R/I '+wnumRi+'.'+wnummes ,;
		forma   	WITH 'EF' ,;
		Monto   	WITH impw,;
		codcad  	WITH cadw ,;
		perRi   	WITH wpeRiodo,;
		nummesRi    WITH wnummes ,;
		numRi   	WITH wnumRi ,;
		estado      WITH '00' ,;
		motivo      WITH '19',;				
		codban   	WITH bcow,;
		usuario     WITH usuw
		sele 1
		repla docref with refw
		skip
	enddo
	sele 4
	skip
enddo
clos data
return	
