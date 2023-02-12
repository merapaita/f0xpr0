** --------------------------------------
** LIBCAJ.PRG
** Reporte Libro Caja (Mensual o Anual)
** AUTOR: UDEP - 1994
** --------------------------------------

* Abrir Archivos de trabajo

USE Parmae  IN 1  ORDER TAG Parmae1         ALIAS parma
USE Diario  IN 3  ORDER TAG Diario1         ALIAS Diar
USE Compro  IN 4  ORDER TAG Compro7         ALIAS Comp
USE Cuentas IN 5  ORDER TAG Cuentas1        ALIAS Cuen
PUBLIC vdebe, vhaber,vsaLDO,sAper,Muestra, saper1, SumaRet, Tiene
PUBLIC vComp11,vcomp22,vcomp1,vcomp2,VCUEN
TOT=0
Muestra = 1
STORE 0 TO xano, xMes, sAper, sAper1
STORE 0 TO cajad,cajah

xMes = VAL(cMes)
xAno = VAL(cAno)

SELECT parma
SEEK 'MESANOACTUAL'
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))

SEEK "CORRELASTING"
IF FOUND()
	xAstIng = PADL(ALLTRIM(STR(Parma.NumEnt)),2,"0")
 ELSE
	WAIT WINDOW "El Correlativo de Asientos de Ingresos de Caja no esta disponible. Por Favor Avise al Area de Sistemas"
*	=STANDBY("El Correlativo de Asientos de Ingresos de Caja no esta disponible. Por Favor Avise al Area de Sistemas")
ENDIF

SEEK "CORRELASTGAS"
IF FOUND()
	xAstGas = PADL(ALLTRIM(STR(Parma.NumEnt)),2,'0')
 ELSE
	WAIT WINDOW "El Correlativo de Asientos de Gastos de Caja no esta disponible. Por Favor Avise al Area de Sistemas"
*	=STANDBY("El Correlativo de Asientos de Gastos de Caja no esta disponible. Por Favor Avise al Area de Sistemas")
ENDIF
vind1= SYS(3) + '.idx'
xruta=SET('PATH')

vindX= SYS(3) + '.DBF'
vindY= SYS(3) + '.DBF'

SELECT Diar
SAper  = SaldoApe()
SAper1 = sAper

WAIT WINDOW NOWAIT 'Espere un Momento; Procesando Informaci¢n '
*DO ESPERA  WITH 1,'Espere un Momento; Procesando Informaci¢n '
*---------------------------------------
SELE DIAR
COPY TO &vindx FOR cuenta<>'0' and cuenta<>'110101' and cuenta<>'8' and cuenta<>'9' and Coddep='113000' and comprob<>'99' AND COMPROB<>'06' AND COMPROB<>'00'  ;
								AND CAJCHI<>'S' AND TIPOBR<>'N' AND MONTH(FECEMI)>=XMES AND YEAR(FECEMI)>=XANO
USE IN 3
USE &vindx IN 3 ALIAS DIAR EXCLUSIVE
*---------------------------------------
*---------------
* DO QuitaReten
*---------------
SELE Diar
SET RELATION TO Diar.Cuenta  						  INTO Cuen
SET RELATION TO Diar.Comprob+Diar.ComAdi INTO Comp ADDITIVE
INDEX ON DTOC(Diar.FECEMI,1) + COMPROB + Diar.COMADI + Diar.CODFTE + Diar.CUENTA  TO (vind1) FOR MONTH(Diar.FecEmi) = xMes;
          .AND. YEAR(Diar.FecEmi) = xAno .AND. Diar.Coddep='113000' AND Reten<>'S' AND Estcon<>"55" AND Orden#"56"			&&AND (Orden=xAstIng OR Orden=xAstGas)

*DO  ESPERA WITH 2,' '

GO TOP

IF EOF()
	WAIT WINDOW " No hay registros para procesar"
*	DO STANDBY WITH " No hay registros para procesar"
    ELSE
    SET STEP ON 
	   	REPORT FORM LibCaj1.frx TO PRINTER PROMPT PREVIEW
*	   DO REPORTE WITH 2, "LibCaj1", 'Libro Caja Mensual',1,.F.,.T.
ENDIF

SELECT Parma
SEEK 'LISCONCAJA'
bloquea = .T.
DO Fox_lock WITH 1, Bloquea
IF Bloquea .AND. FOUND()
    REPLACE parma.Descri WITH 'Si'
ENDIF
CLOSE DATABASE
ERASE (vind1)



*DO Inicia
*DO salida
RETURN



PROCEDURE Inicia
*---------------

  fReporte = 'LibCajX'
  xbalance = 'Mensual'
  xdigitos = 2
  vsedpli  ='Sede   '
  vFormat  = 1

RETURN

FUNCTION Descri
*--------------
PUBLIC _x,vCue
vCue = LEFT(Diar.Cuenta,4)+'00000000000'
_x = des_cuenta(vCue)
*_x=Val_Fun('Cuen','Cuenta','Descri',vCue)
RETURN _x


FUNCTION Descri1
*---------------
PUBLIC _y,vCue1
vCue1= LEFT(Diar.Cuenta,6)+'000000000'
_y = des_cuenta(vCue1)
*_y=Val_Fun('Cuen','Cuenta','Descri',vCue1)
RETURN _y


FUNCTION SALDO
*-------------
SUM montos to Vdebe  for tipo='D'
SUM montos to Vhaber for tipo='H'
vHaber = vhaber + saper1
VSALDO = VDEBE-VHABER
RETUR VSALDO


FUNCTION SaldoApe			&& Para el mes de enero es el saldo inicial. Para los otros es el sado anterior (saldona)
*----------------
vali= ALIAS()
vord= ORDER()
TOTD=0
TOTH=0
USE Saldona IN 12 ORDER TAG Saldona4 ALIAS SalN
SELE SalN
IF xmes=1
	zmes = 0
	zano = xano
*		zmes = 12
*		zano = xano-1
 ELSE
	zmes = xmes - 1
	zano = xAno
ENDIF
	
*---------------------------------------------------------------
*- SIEMPRE DEBE IR AL MES DE ENERO
SEEK '110101000000000'+STR(zano,4)
*---------------------------------------------------------------
SCAN WHILE  SalN.Cuenta='110101000000000' AND SalN.Mes<=zMes AND SalN.Ano=zAno
	TOTD = TOTD + SalN.DSumana
	TOTH = TOTH + SalN.HSumana
ENDSCAN
	
IF TOTD>TOTH
	TOT = TOTD-TOTH
 ELSE
	TOT = TOTH-TOTD
ENDIF

USE IN 12
SELE (vali)
SET ORDE TO vOrd
RETURN TOT

FUNCTION SALDOA
*--------------
*mRet = 0
*IF _PAGENO = 1
*	mRet = sAper
*ENDIF
*RETURN sAper

IF Muestra = 1 .OR. Muestra = 2
	Muestra = Muestra  + 1
	RETURN sAper
ELSE
	sAper = 0
ENDIF
RETURN Saper

PROCEDURE QUITARETEN
*--------------------
SELE Diar
*SET FILTER TO Comprob='98'
SET FILTER TO LEFT(Comprob,2)$'98 97'
vind3= SYS(3) + '.idx'
vind4= SYS(3) + '.idx'
INDEX ON ALLTRIM(Comprob)+Tipo  TO (vind3)
INDEX ON ALLTRIM(Comprob)+cuenta+Tipo TO (vind4)
*SET INDEX TO (vind4)

* Para acumular el monto de todas las retenciones del c/p.
SET INDEX TO (vind3)
GO TOP
SCAN
    Tiene   = .F.
	vComp1  = ALLT(Diar.Comprob)
	vComp2  = ALLT(Diar.Comprob)
    vRecno  = RECNO()	
	STORE 0 TO SumaRet
	SCAN WHILE vComp1=vComp2
		IF Reten='S'
			Tiene   = .T.
			SumaRet = SumaRet + Diar.Montos
		ENDIF
		vComp2=ALLT(Diar.Comprob)
	ENDSCAN
	SKIP -1
	vRec1  = RECNO()
	
    *----------------------- 
	* Si tiene retenciones
	IF Tiene
		SELE DIAR
		GO (vRecno)
		SEEK vcomp1+'D'
		* Para encontrar la cuentas del Debe con mayor monto
		vComp11  = ALLT(Diar.Comprob)
		vComp22  = ALLT(Diar.ComproB)
		STORE 0 TO Mayor 

		SCAN WHILE vComp11=vComp22 AND Tipo='D'
			IF Diar.Montos > Mayor
				Mayor = Diar.Montos
				vComp = Diar.Comprob
				vCuen = Diar.Cuenta
			ENDIF
			vComp22=ALLT(Diar.Comprob)
		ENDSCAN
		
		* Ahora se reemplaza el monto mayor del debe menos las retenciones. 
		SELE Diar
		SET INDEX TO (vind4)
		SEEK vComp11+vCuen+'D'
		IF FOUND()
			*IF F_LOCK(1)
				REPLACE Diar.Montos WITH Diar.Montos-SumaRet
			*ENDIF
		ENDIF
		SET INDEX TO (vind3)
	ENDIF
	*---------------------------
	GO (VREC1)		
ENDSCAN
SELE DIAR
SET FILTER TO
RETURN
	


PROCEDURE CajaCHica	
*------------------
SELE Diar
GO TOP
SCAN
	DO CASE
		CASE (Cuenta='4' AND tipo='H')
			DELETE
		CASE (CUENTA='6' AND TIPO='D')
			DELETE
	ENDCASE
ENDSCAN
PACK
RETURN
	
		
