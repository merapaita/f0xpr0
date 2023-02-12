*-------------------------------------------------
*- REPEEFF
*- AUTOR : JCO
*- Programa que permite mostrar los reportes de 
*- los estados financieros
*---------------------------------------------------
PARAMETERS vtip,reporte

USE Repfin IN 1  ORDER repfin6     ALIAS repfin
USE Parmae IN 2  ORDER Parmae1     ALIAS parma

PRIVATE vmesact,vmesef,vanoef

SELECT Parma
SEEK "MESANO"+"REAL"

vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
fe2      = DATE()

DO inicio

RETURN

PROCEDURE inicio
*----------------
  DEFINE WINDOW LIS FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Generaci¢n de estados financieros   ' COLOR SCHEME 5
  ACTIVATE WINDOW LIS

  @ 1, 0 SAY '          Al :' GET fe2     PICTURE '@D' VALID fe2<=DATE()
  @ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8    
  READ
  DEACTIVATE WINDOW Lis
  IF LASTKEY() = 27 OR OKCANCEL = 2
     CLOSE DATA
     RETURN
  ENDIF

  SELECT Repfin

  DO Reporte  WITH 2,reporte,"Estado Financiero "+vtip
  CLOSE DATA
  RETURN
  
FUNCTION FilCol
*-----------------
*- Devuelve el monto que corresponde a una fila y columna espec¡ficas
*- para un determinado formato.

PARAMETERS tiprep,fil,col
	tiprep=ALLTRIM(tiprep)
	fil   =ALLTRIM(fil)	
	col   =ALLTRIM(col)
	SELE repfin
	STORE 0 TO tot
	IF SEEK(PADR(fil,3,' ')+PADR(tiprep,5,' ')+PADR(col,3,' '))
		SCAN WHILE RepFin.CodRep=tiprep .AND. RepFin.Fila=fil .AND.;
				 RepFin.Columna=col  &&
			IF Repfin.nummes = PADL(MONTH(fe2),2,'0')
				IF RepFin.totdeb <> 0 
					IF repfin.columna = '3' 
						tot = tot + RepFin.totdeb*(-1)
					ELSE
	   					tot=tot+RepFin.totdeb
	   				ENDIF
				ELSE
					IF repfin.columna = '1'
		   				tot=tot+RepFin.tothab*(-1)
		   			ELSE
		   				tot=tot+RepFin.tothab
		   			ENDIF	
				ENDIF
			ENDIF
		ENDSCAN
		RETURN ROUND(Tot,0)
	ELSE
		RETURN 0	
	ENDIF

FUNCTION comprime
*----------------
vcomprime = CHR(27) + CHR(77) + CHR(27) + CHR(15) + CHR(27) + CHR(48)
RETURN vcomprime 

FUNCTION VerifNeg
*------------------
*- Verifica si un n£mero es negativo y lo devuelve con signo positivo.

PARAMETERS num
IF num<0
	RETURN num*(-1)
ELSE
	RETURN num
ENDIF

FUNCTION SumCol
*-----------------
*- Devuelve el monto total de una columna espec¡fica para un formato
*- seleccionado.

PARAMETERS tiprep,col
	tiprep=ALLTRIM(tiprep)
	col   =ALLTRIM(col)
suma=0
store 0 to suma1,suma2
SELE repfin
GO TOP
SUM RepFin.totdeb FOR ALLTRIM(codrep)=tiprep .AND. ALLTRIM(columna)=col TO suma1
GO TOP
SUM RepFin.tothab FOR ALLTRIM(codrep)=tiprep .AND. ALLTRIM(columna)=col TO suma2
IF suma1=0
	suma=suma2
ELSE
	suma=suma1
ENDIF
RETURN suma2-suma1

FUNCTION filcol1
*----------------
*- Devuelve el monto que corresponde a una fila y columna espec¡ficas
*- para un determinado formato.

PARAMETERS tiprep,fil,col
	tiprep=ALLTRIM(tiprep)
	fil   =ALLTRIM(fil)	
	col   =ALLTRIM(col)
	SELE repfin
	STORE 0 TO tot
	IF SEEK(PADR(fil,3,' ')+PADR(tiprep,5,' ')+PADR(col,3,' '))
		SCAN WHILE RepFin.CodRep=tiprep .AND. RepFin.Fila=fil .AND.;
				 RepFin.Columna=col  &&
			IF Repfin.nummes = PADL(MONTH(fe2),2,'0')
				IF RepFin.totdeb <> 0 
	   				tot=tot+RepFin.totdeb
				ELSE
		   			tot=tot+RepFin.tothab
				ENDIF
			ENDIF
		ENDSCAN
		RETURN tot
	ELSE
		RETURN 0	
	ENDIF

FUNCTION leeRes
*------------------

IF SEEK("RESULT"+"EJERCI"+PADL(MONTH(fe2),2,"0")+SUBSTR(STR(YEAR(fe2),4),3,2),"Parma")
	RETURN parma.numdec
ELSE
	RETURN 0	
ENDIF

FUNCTION FilCol2
*-----------------
*- Devuelve el monto que corresponde a una fila y columna espec¡ficas
*- para un determinado formato.

PARAMETERS tiprep,fil,col
	tiprep=ALLTRIM(tiprep)
	fil   =ALLTRIM(fil)	
	col   =ALLTRIM(col)
	SELE repfin
	STORE 0 TO tot
	IF SEEK(PADR(fil,3,' ')+PADR(tiprep,5,' ')+PADR(col,3,' '))
		SCAN WHILE RepFin.CodRep=tiprep .AND. RepFin.Fila=fil .AND.;
				 RepFin.Columna=col  &&
			IF Repfin.nummes = PADL(MONTH(fe2),2,'0')
				IF RepFin.totdeb <> 0 
	   					tot=tot+RepFin.totdeb
				ELSE
		   				tot=tot+RepFin.tothab
				ENDIF
			ENDIF
		ENDSCAN
		RETURN ROUND(Tot,0)
	ELSE
		RETURN 0	
	ENDIF
