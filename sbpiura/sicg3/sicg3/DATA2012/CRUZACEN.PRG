SET DELE ON
SET EXCLU OFF
SET TALK OFF
SET CENTURY ON
nMes = 9

clos data
??"para cruzar ASIENTO QUE NO CUADRA DEBE CON HABER"
use diario
INDEX ON COMPROB+COMADI TO XX FOR MONT(FECEMI)=nMes
GO TOP
SCAN
	cComp = Comprob
	nCtaD = 0
	nCtaH = 0
	SCAN WHILE Comprob = cComp
		IF Tipo = 'D'
			nCtaD = nCtaD + Montos
		ENDIF
		IF Tipo = 'H'
			nCtaH = nCtaH + Montos
		ENDIF
	ENDSCAN
	skip -1
	
	IF nCtaD = nCtaH
	ELSE
		WAIT WIND "Error en N/C :" + Comprob + ' '+ ComAdi
*		DO StandBy with "Error en N/C :" + Comprob + ' '+ ComAdi
	ENDIF
ENDSCAN
return

??"para cruzar ASIENTO centralizacion QUE NO CUADRA DEBE CON HABER"
set dele on
use diario
INDEX ON COMPROB+COMADI TO XX FOR ORDEN='58' AND MONT(FECEMI)=nMes AND CUENTA#'9'
*set step on
GO TOP
SCAN
	cComp = Comprob
	nCtaD = 0
	nCtaH = 0
	SCAN WHILE Comprob = cComp
		IF Tipo = 'D'
			nCtaD = nCtaD + Montos
		ENDIF
		IF Tipo = 'H'
			nCtaH = nCtaH + Montos
		ENDIF
	ENDSCAN
	skip -1
	
	IF nCtaD = nCtaH
	ELSE
		WAIT WIND "Error en N/C :" + Comprob + ' '+ ComAdi
*		DO StandBy with "Error en N/C :" + Comprob + ' '+ ComAdi
	ENDIF
ENDSCAN
return

??"para cruzar la 104 con la centralizacion"
USE DIARIO
INDEX ON COMPROB+COMADI TO XX FOR COMPROB = '26' AND ORDEN#'56' AND CUENTA#'9' AND MONT(FECEMI)=nMes
GO TOP
SCAN
	cComp = Comprob
	n104H = 0
	n101D = 0
	n101H = 0
	SCAN WHILE Comprob = cComp
		IF (Cuenta = '104' AND Tipo = 'H') or (Cuenta = '107' AND Tipo = 'H')
			n104H = n104H + Montos
		ENDIF
	ENDSCAN
	SEEK cComp
	SCAN WHILE Comprob = cComp
		IF Cuenta = '101' AND Tipo = 'D'
			n101D = n101D + Montos
		ENDIF
		IF Cuenta = '101' AND Tipo = 'H'
			n101H = n101H + Montos
		ENDIF
	ENDSCAN
	skip -1
	IF (((n101D - n101H) = 0.0) and (n104H = n101D)) or (((n101D - n101H) # 0.0) and (n104H = n101D))
	ELSE
		WAIT WIND "Error en N/C :" + Comprob + ' '+ ComAdi
*		DO StandBy with "Error en N/C :" + Comprob + ' '+ ComAdi
	ENDIF
ENDSCAN
return

??"para cruzar la CHEQUES GIRADOS CON CHEQUES ENTREGADOS"
USE DIARIO
INDEX ON SUBSTR(COMPROB,3,5) TO XX FOR (COMPROB = '27' OR COMPROB='28') AND MONT(FECEMI)=nMes		&&AND (CUENTA='0' OR CUENTA='104')
*SET STEP ON
GO TOP
SCAN
	cComp = SUBSTR(COMPROB,3,5)
	n05D = 0
	n06H = 0
	n05H = 0
	n06D = 0
	SCAN WHILE SUBSTR(COMPROB,3,5) = cComp
		IF Cuenta = '05' AND Tipo = 'D'
			n05D = n05D + Montos
		ENDIF
		
		IF Cuenta = '06' AND Tipo = 'H'
			n06H = n06H + Montos
		ENDIF

		IF Cuenta = '05' AND Tipo = 'H'
			n05H = n05H + Montos
		ENDIF
		
		IF Cuenta = '06' AND Tipo = 'D'
			n06D = n06D + Montos
		ENDIF

	ENDSCAN
	skip -1
*	IF ((n05D - n06H) = 0.0) and ((n05H - n06D) = 0.0)
	IF ((n05D - n05H) = 0.0) and ((n06H - n06D) = 0.0)
	ELSE
		WAIT WIND "Error en N/C :" + Comprob + ' '+ ComAdi
		*DO StandBy with "Error en N/C :" + Comprob + ' '+ ComAdi
	ENDIF
ENDSCAN
*RETURN

??"para cruzar la C/P CON CHEQUES GIRADOS"
USE DIARIO
INDEX ON SUBSTR(COMPROB,3,5) TO XX FOR (COMPROB = '26' OR COMPROB='27') AND (CUENTA='0' OR CUENTA='104') AND MONT(FECEMI)=nMes
*SET STEP ON
GO TOP
SCAN
	cComp = SUBSTR(COMPROB,3,5)
	n104H = 0
	n05D = 0
	n06H = 0
	SCAN WHILE SUBSTR(COMPROB,3,5) = cComp
		IF (Cuenta = '104' AND Tipo = 'H')
			n104H = n104H + Montos
		ENDIF
		
		IF Cuenta = '05' AND Tipo = 'D'
			n05D = n05D + Montos
		ENDIF
		
		IF Cuenta = '06' AND Tipo = 'H'
			n06H = n06H + Montos
		ENDIF
	ENDSCAN
	skip -1
	IF (((n05D - n06H) = 0.0) and (n104H = n05D))			&& or (((n05D - n06H) # 0.0) and (n104H = n05D))
	ELSE
		WAIT WIND "Error en N/C :" + Comprob + ' '+ ComAdi
		*DO StandBy with "Error en N/C :" + Comprob + ' '+ ComAdi
	ENDIF
ENDSCAN
*RETURN
