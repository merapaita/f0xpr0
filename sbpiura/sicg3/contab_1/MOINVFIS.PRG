PUBLIC Monto
filaux = SYS(3) + '.DBF'
SELECT 8

CREATE TABLE (filaux);
   (MES         N(02,0), ;                 
   ANO         N(02,0), ;                 
   CuentaA      C(10),;                  
   DescriA      C(30),;
   montoA       N(18,4), ;
   tipoA		C(30), ;
   CuentaP      C(10),;
   DescriP      C(30),;
   montoP       N(18,4),;
   tipoP 		C(30))
USE
DO PROCESO


PROCEDURE Proceso
*----------------
USE (filaux)  IN  3 EXCLUSIVE                         ALIAS Temp

SELE Saln
SET FILTER TO (BETWEEN(PADL(ALLT(STR(MES)),2,'0'),vMes1,vmes2)) AND (ALLTRIM(STR(Ano))=vAno) AND (SUBSTR(Cuenta,3,1)='0') AND  (cuenta<'60')
GO TOP
SCAN FOR BETWEEN(SUBSTR(saln.Cuenta,1,2),'10','39')
	vcuenta1 = saln.cuenta
	vcuenta2 = saln.cuenta
	vdsaldona = 0
	vhsaldona = 0
	DO WHILE vCuenta1 = vCuenta2
		*------------------------------------
		*vdsaldona=vdSaldona+dsaldona+dajuano
		*vhsaldona=vhsaldona+hsaldona+hajuano
		*------------------------------------
		vdsaldona=vdSaldona +dsumana+dajumes
		vhsaldona=vhsaldona +hsumana+hajumes
		IF !EOF()
			skip
		ENDIF
		vcuenta2=cuenta
	ENDDO
	SKIP-1
	SCATTER MEMVAR
	SELE temp
	IF SalN.Cuenta = '3000000000'
		Monto = vdsaldona - vhsaldona
	ELSE
		APPEND BLANK
    	REPLACE mes WITH saln.mes
	    REPLACE ano WITH saln.ano
		REPLACE CuentaA WITH saln.cuenta
		REPLACE DescriA WITH Bus_cta(SUBSTR(temp.CuentaA,1,2)+'00000000')
		REPLACE MontoA  WITH vdsaldona - vhsaldona
		SELE Saln
	ENDIF
ENDSCAN
GO TOP
SELE temp
GO TOP
SELE SalN

SCAN FOR BETWEEN(SUBSTR(saln.Cuenta,1,2),'40','60')
	vcuenta1 = saln.cuenta
	vcuenta2 = saln.cuenta
	vdsaldona = 0
	vhsaldona = 0
	DO WHILE vCuenta1 = vCuenta2
		*-----------------------------------
		*vdsaldona=vdSaldona+dsaldona+dajuano
		*vhsaldona=vhsaldona+hsaldona+hajuano
		*------------------------------------
		vdsaldona =vdsaldona + dsumana+dajumes
		vhsaldona =vhsaldona + hsumana+hajumes
		IF !EOF()
			skip
		ENDIF
		vcuenta2=cuenta
	ENDDO
	SKIP-1
	SELE temp
	APPEND BLANK
	IF  RLOCK()
		REPLACE CuentaP WITH saln.cuenta
		REPLACE DescriP WITH Bus_cta(SUBSTR(temp.CuentaP,1,2)+'00000000')
	ENDIF	
	IF Saln.Cuenta='5900000000' AND  RLOCK()
  		REPLACE MontoP  WITH (vhSaldona - vdSaldona)+(DEFICIT(vano))
	ELSE
		IF Saln.Cuenta='5600000000' AND  RLOCK()
			REPLACE MontoP  WITH (vhSaldona - vdSaldona) - (MONTO)
		ELSE
			REPLACE MontoP  WITH (vhSaldona - vdSaldona)
		ENDIF
	ENDIF
	UNLOCK
	IF !EOF()
		skip
	   ELSE
*	    IF !EOF('SalN')
*			APPEND BLANK
*		ENDIF
	ENDIF
	SELE Saln
ENDSCAN
DO Recorre
SELE Temp
GO TOP
DO ESPERA WITH 2
DO REPORTE WITH 2, "InvFis2", ' Balance de Inventario ',1,.F.,.T.
RETURN


PROCEDURE Recorre
*-----------------
SELE Temp
GO TOP
SCAN
	REPLACE TipoA WITH 'ACTIVO CORRIENTE'    FOR BETWEEN(SUBSTR(CuentaA,1,2),'10','29')
	REPLACE TipoA WITH 'ACTIVO NO CORRIENTE' FOR BETWEEN(SUBSTR(CuentaA,1,2),'30','39')			
	REPLACE TipoP WITH 'PASIVO CORRIENTE'    FOR BETWEEN(SUBSTR(CuentaP,1,2),'40','48')			
	REPLACE TipoP WITH 'PASIVO NO CORRIENTE' FOR BETWEEN(SUBSTR(CuentaP,1,2),'50','53')			
	REPLACE TipoP WITH 'PATRIMONIO'          FOR BETWEEN(SUBSTR(CuentaP,1,2),'54','89')			
ENDSCAN


FUNCTION bus_cta
*----------------
PARAMETERS vcuenta 
vAlias = SELECT()
SELECT Cuen
SEEK vcuenta 
IF FOUND()
   SELE(VAlias)
   RETURN  ALLTRIM(Cuen.descri) 
ELSE
   SELE(VAlias)
   RETURN  'Cuenta no Existe'    
ENDIF
RETURN 



FUNCTION MontoR
*---------------
PARAMETER vcuenta
GO TOP
LOCATE FOR vcuenta=temp.cuentaA OR vCuenta=temp.CuentaP
IF FOUND()
	IF BETWEEN(SUBSTR(vCuenta,1,2),'10','39')
	  	mto= montoA
	ENDIF
	IF BETWEEN(SUBSTR(vCuenta,1,2),'40','89')
		mto= MontoP
	ENDIF
   ELSE 
    RETURN 0
ENDIF
RETURN Mto


FUNCTION Ttipo
*-------------
PARAMETER monto,opcion
PRIVATE total
SUM &monto TO total FOR (TipoA = opcion) OR (TipoP = opcion)
IF upper(monto) = "MONTOA"
	totala = TotalA + total
   ELSE 
    TotalP = TotalP + total
ENDIF
RETURN total


FUNCTION Deficit
*---------------
PARAMETER VANO
VALI=ALIAS()
VORD=ORDER()
SELE PARMA
SET ORDE TO PARMAE1
SEEK 'OPERAT'+'DS'+vano
IF FOUND()
	Valor =  Nument
ENDIF
SELE (VALI)
SET ORDER TO VORD
RETURN Valor


