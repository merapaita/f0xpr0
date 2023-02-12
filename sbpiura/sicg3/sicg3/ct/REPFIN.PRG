*----------------------------------------
*- RepFin.prg
*- Programa que genera el repfin.dbf
*- Autor : JCO
*------------------------------------------

*- Abrir tablas

USE Estfin  IN 1  ORDER Estfin1 	ALIAS estfin
USE saldona IN 2  ORDER saldona2    ALIAS saldona
USE Repfin  IN 3  					ALIAS repfin
USE Parmae  IN 4  ORDER parmae1     ALIAS parma

PRIVATE vmesef,vanoef,vformat

SELECT Parma
SEEK "MESANO"+"REAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
vmesef   = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
vanoef   = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
vformat  = SPACE(5)

DO inicio
CLOSE DATA
RETURN

PROCEDURE inicio
*----------------
  DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Generaci¢n de estados financieros   ' COLOR SCHEME 5
  ACTIVATE WINDOW LIS

  @ 1, 0 SAY '    Mes y A¤o:' GET vmesef     PICTURE '99' VALID vmesef > 0 .AND. vmesef <= 12
  @ 1,20 GET vanoef                          PICTURE '99'
  @ 3, 0 SAY '     Formato :' GET vformat     VALID val_para(vformat,"FORMAT"," ",16)
  @ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8    
  READ
  DEACTIVATE WINDOW Lis
  IF LASTKEY() = 27 OR OKCANCEL = 2
     CLOSE DATA
     RETURN
  ENDIF
  IF  verrepfin()     && verificar existencia del formato en repfin
	DO ESPERA WITH 1,PADC(' Generando Estado Financiero ',50)
 	IF !ALLTRIM(vformat)='F-1'
  		DO Genrepfin	  && Generar el formato en repfin
  	ELSE
  		DO genf1
  	ENDIF
  	DO ESPERA WITH 2
  	DO Standby WITH 'Proceso Finalizado...!'      
  ENDIF
  RETURN
  
PROCEDURE verrepfin
*--------------------
SELECT repfin
SET ORDER TO repfin4
SEEK PADR(vformat,5," ")+PADL(vanoef,2,"0")+PADL(vmesef,2,"0")
IF FOUND()
  IF yesno('Proceso ya realizado, Reprocesa?') 
	SCAN WHILE codrep+periodo = PADR(vformat,5," ")+PADL(vanoef,2,"0") ;
				AND VAL(Nummes)<=vmesef
    	IF !f_lock(1)            
    		UNLOCK ALL
            RETURN
        ENDIF
        IF RLOCK()
            DELETE NEXT 1
        ENDIF   
    ENDSCAN
  ELSE
  	RETURN .F.
  ENDIF
ENDIF
RETURN .T.
  
PROCEDURE genrepfin
*---------------------
SELECT estfin
SET FILTER TO codrep = PADR(vformat,5," ")
SCAN
	vdeb = 0
	vhab = 0
	SELECT saldona
	IF SEEK(estfin.cuenta)
		IF LEFT(estfin.Cuenta,2) = '89'
		  SCAN WHILE saldona.cuenta = estfin.cuenta 
		  	IF saldona.ano = vanoef AND saldona.mes = vmesef
				IF estfin.debhab = 'D'
					vdeb = vdeb + saldona.dsaldoaju 
				ELSE
					vhab = vhab + saldona.hsaldoaju
				ENDIF
		  	ENDIF	
		  ENDSCAN	
		ELSE
		  SCAN WHILE saldona.cuenta = estfin.cuenta 
			IF saldona.ano = vanoef AND ;
				saldona.mes<=vmesef
				IF estfin.debhab = 'D'
					vdeb = vdeb + saldona.dsumaju 
				ELSE
					vhab = vhab + saldona.hsumaju
				ENDIF
			ENDIF
		  ENDSCAN	
		ENDIF
		SELECT repfin
		IF f_appd()
			REPLACE fecha   WITH  DATE()
			REPLACE hora	WITH  TIME()
			REPLACE Periodo WITH  PADL(vanoef,2,"0")
			REPLACE nummes  WITH  PADL(vmesef,2,"0")
			REPLACE codrep  WITH  estfin.codrep
			REPLACE cuenta  WITH  estfin.cuenta
			REPLACE fila    WITH  estfin.fila
			REPLACE columna WITH  estfin.columna
			REPLACE totdeb  WITH  vdeb
			REPLACE tothab  WITH  vhab
			REPLACE estado  WITH  "GE"
		ENDIF
	ENDIF
	SELECT estfin
ENDSCAN
RETURN

PROCEDURE genf1
*---------------------

SELECT Saldona
SET ORDER TO saldona1
SELECT estfin
SET FILTER TO codrep = PADR(vformat,5," ")
SCAN
	vdeb = 0
	vhab = 0
	IF ALLTRIM(columna) = '3' AND ALLTRIM(fila) = '1' AND ;
				LEFT(cuenta,3)='104'  && Para el caso de
									  && sobregiros
		SELECT saldona
		IF SEEK(estfin.cuenta+STR(vmesef,2)+STR(vanoef,2))
			SCAN WHILE LEFT(cuenta,3)='104'
				IF RIGHT(cuenta,5)#'00000' AND ano = vanoef AND mes= vmesef
					vhab = vhab + (saldona.hsaldoaju+saldona.hajumes)
				ENDIF
			ENDSCAN 
			SELECT repfin
			IF f_appd()
				REPLACE fecha   WITH  DATE()
				REPLACE hora	WITH  TIME()
				REPLACE Periodo WITH  PADL(vanoef,2,"0")
				REPLACE nummes  WITH  PADL(vmesef,2,"0")
				REPLACE codrep  WITH  estfin.codrep
				REPLACE cuenta  WITH  estfin.cuenta
				REPLACE fila    WITH  estfin.fila
				REPLACE columna WITH  estfin.columna
				REPLACE totdeb  WITH  vdeb
				REPLACE tothab  WITH  vhab
				REPLACE estado  WITH  "GE"
			ENDIF
		ENDIF  && seek()
	ELSE  && (columna,fila)
	 SELECT saldona
	 IF SEEK(estfin.cuenta+STR(vmesef,2)+STR(vanoef,2))
		IF RIGHT(estfin.cuenta,7)='0000000'
			vdeb = vdeb + (saldona.dsaldoaju)
			vhab = vhab + (saldona.hsaldoaju)
		ELSE
			DO WHILE  !BOF() AND saldona.cuenta = estfin.cuenta   AND ;
					Saldona.ano = vanoef
				SKIP -1
			ENDDO
			SKIP
			vdeb1 = saldona.dsumaju
			vhab1 = saldona.hsumaju
			vdeb  = vdeb1
			vhab  = vhab1
			SCAN WHILE saldona.cuenta = estfin.cuenta 
				IF saldona.ano = vanoef AND saldona.mes <= vmesef 
					vdeb = vdeb + saldona.dajumes
					vhab = vhab + saldona.hajumes
				ENDIF
			ENDSCAN
		ENDIF
		SELECT repfin
		IF f_appd()
			REPLACE fecha   WITH  DATE()
			REPLACE hora	WITH  TIME()
			REPLACE Periodo WITH  PADL(vanoef,2,"0")
			REPLACE nummes  WITH  PADL(vmesef,2,"0")
			REPLACE codrep  WITH  estfin.codrep
			REPLACE cuenta  WITH  estfin.cuenta
			REPLACE fila    WITH  estfin.fila
			REPLACE columna WITH  estfin.columna
			REPLACE totdeb  WITH  vdeb
			REPLACE tothab  WITH  vhab
			REPLACE estado  WITH  "GE"
		ENDIF
	  ENDIF 
	ENDIF (Columna,fila)
	SELECT estfin
ENDSCAN
RETURN
