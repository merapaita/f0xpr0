*-------------------------------
*- ASIECENT.PRG
*- Asiento de Centralizaci¢n
*- AUTOR : UDEP 1994 - JECHS
*-------------------------------

USE Compro  IN 1  ORDER TAG Compro1               ALIAS Comp
USE diario  IN 2  ORDER TAG Diario3               ALIAS Diar
USE Parmae  IN 3  ORDER TAG Parmae1               ALIAS Parma
USE Cuentas IN 4  ORDER TAG Cuentas1              ALIAS Cuen

PUBLIC vQui,vDMonto,vHMonto,vKey,vDTot,vHTot,vDSal,vHSal,vMonDeb,vMonHab,debe100,haber100,debe200,haber200
PUBLIC _y,_z,vUno,vUnos
STORE 0 TO xano,xMes,vdebe,vhaber,vQui,vMonDeb,vMonHab,debe100,haber100,debe200,haber200
STORE 0 TO vDMonto,vHMonto,vDTot,vHTot,vDSal,vHSal,vUno,vUnos
STORE .T. TO NoPaso, NoPaso1
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
DEFINE WINDOW AsieCent FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE ' Asiento de Centralizaci¢n ' COLOR SCHEME 5
ACTIVATE WINDOW AsieCent

xbalance = 'Mensual'
xdigitos = 2
SELECT parma
SEEK 'MESANOACTUAL'
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xAno     = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))

SEEK "CORRELASTING"
IF FOUND()
	xAstIng = PADL(ALLTRIM(STR(Parma.NumEnt)),2,"0")
 ELSE
	=STANDBY("El Correlativo de Asientos de Ingresos de Caja no esta disponible. Por Favor Avise al Area de Sistemas")
ENDIF

SEEK "CORRELASTGAS"
IF FOUND()
	xAstGas = PADL(ALLTRIM(STR(Parma.NumEnt)),2,'0')
 ELSE
	=STANDBY("El Correlativo de Asientos de Gastos de Caja no esta disponible. Por Favor Avise al Area de Sistemas")
ENDIF

@ 2, 1 SAY '                 Mes: ' GET xMes     PICTURE '99'  VALID xMes > 0 .AND. xMes <= 12
@ 4, 1 SAY '                 A¤o: ' GET xAno     PICTURE '9,999'
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE
RELEASE WINDOW AsieCent
IF LASTKEY() # 27 AND OKCANCEL=1
	DO ESPERA WITH 1,PADC("Asiento de centralizaci¢n - en Proceso",50)
	vind1= SYS(3) +'.idx'
	DO CASE
		CASE xbalance = 'Mensual'
			IF xMes = MONTH(vmesact) .AND. xAno = YEAR(vmesact)
				&& Es lo mismo que el libro caja pero sin el saldo de apertura
				vindX= SYS(3) + '.DBF'
				SELECT Diar
				
				SUM montos TO Vdebe101  FOR Tipo='D' AND cuenta='101' AND comprob<>'00'
				SUM montos TO Vhaber101 FOR Tipo='H' AND cuenta='101' AND comprob<>'00'
				
				COPY TO &vindx FOR cuenta<>'0' and cuenta<>'101' and cuenta<>'9' and Coddep='113000' and comprob<>'99' AND COMPROB<>'06' AND COMPROB<>'00'  ;
						AND CAJCHI<>'S' AND TIPOBR<>'N' AND MONTH(FECEMI)>=XMES AND YEAR(FECEMI)>=XANO
				USE IN 2
				USE &vindx IN 2 ALIAS DIAR
				
				*---------------
*				DO QuitaReten
				*-----------------
				
				SELE Diar  && Se graba a la base Temp
				GO TOP
				SCAN
					IF Diar.tipo='H'
						REPLACE COMPROB WITH '100'
					 ELSE
						REPLACE COMPROB WITH '200'
					ENDIF
				ENDSCAN
				SELE Diar
				GO BOTTOM
				m.FecEmi = Diar.Fecemi
				m.CodDep = Diar.CodDep
				FOR I=1 TO 2
					IF F_APPD()
						GATHER MEMVAR
						REPLACE COMPROB WITH IIF(I=1,'100','200')
						REPLACE FecEmi  WITH m.FecEmi
						REPLACE CodDep  WITH m.Coddep
						REPLACE CUENTA  WITH '1010000000'
						REPLACE TIPO    WITH IIF(I=1,'D','H')
						REPLACE MONTOS  WITH IIF(I=1,vDebe101,vHaber101)
					ENDIF
				ENDFOR
				
				INDEX ON COMPROB + Cuenta   TO (vind1) FOR MONTH(Diar.FecEmi) = MONTH(vmesact);
				        .AND. YEAR(Diar.FecEmi) = YEAR(vmesact) .AND. Diar.Coddep='113000' AND Reten<>'S' AND Estcon<>"55" AND (Orden=xAstIng OR Orden=xAstGas)

				&& Para acumular los saldos
				SUM Montos TO Debe100  FOR TIPO='D' AND comprob='100'
				SUM Montos TO Haber100 FOR TIPO='H' AND comprob='100'
				SUM Montos TO Debe200  FOR TIPO='D' AND comprob='200'
				SUM Montos TO Haber200 FOR TIPO='H' AND comprob='200'
				DO espera WITH 2
				DO REPORTE WITH 2, "AsieCen", 'Asiento de Centralizaci¢n',1,.F.,.T.
				CLOSE DATA
			 ELSE
				vpath = '..\'+ALLTRIM(STR(xmes,2))+STR(xano,2)
				vdir    = '..\'+ALLTRIM(STR(xmes,2)) + STR(xano,2) + '\'
				vbackup = ALLTRIM(STR(xmes,2)) + STR(xano,2)+'.zip'
				IF !FILE(vdir + vbackup)
					DO Standby WITH 'Archivo : de BackUp en Disco del mes ' ;
						+ ALLTRIM(STR(xmes)) + ' '+ALLTRIM(STR(xano,2)) + ' no existe'
					RETURN
				ENDIF
				!CD &vpath >NUL
				!FOXSWAP PKUNZIP -o &vbackup Parmae.dbf Diario.dbf Compro.dbf Cuentas.dbf >NUL
				!FOXSWAP PKUNZIP -o &vbackup Parmae.cdx Diario.cdx Compro.cdx Cuentas.cdx >NUL
				!CD ..\FUENTES
				CLOSE DATA
				SET PATH TO &vpath
				USE Parmae  IN 1 ORDER TAG Parmae1     ALIAS parma
				USE Diario  IN 2                       ALIAS Diar
				USE Cuentas IN 4 ORDER TAG Cuentas1    ALIAS Cuen
				USE Compro  IN 5 ORDER TAG Compro1     ALIAS Comp
				SELECT Diar
				
				*----------------------------------------------------------------------
				*SET RELATION TO Diar.Cuenta  INTO Cuen
				*SET RELATION TO Diar.Comprob+Diar.ComAdi INTO Comp ADDITIVE
				*INDEX ON FecEmi TO (vind1) FOR MONTH(Comp.Fecha) = MONTH(vmesact);
				*.AND. YEAR(Comp.Fecha) = YEAR(vmesact) &&.AND. Diar.CodDep='113000'
				*----------------------------------------------------------------------
				
				vindX= SYS(3) + '.DBF'
				SELECT Diar
				COPY TO &vindx FOR Cuenta<>'9' AND cuenta<>'101' AND Comprob<>'99' AND Comprob<>'06' AND Comprob<>'00' 
				USE IN 2
				USE &vindx IN 2 ALIAS DIAR
				
				SELE Diar  && Se graba a la base Temp
				GO TOP
				SCAN
					IF Diar.tipo='H'
						REPLACE COMPROB WITH '100'
					 ELSE
						REPLACE COMPROB WITH '200'
					ENDIF
				ENDSCAN
				SELE Diar
				GO TOP
				SCATTER MEMVAR
				FOR I=1 TO 2
					IF F_APPD()
						GATHER MEMVAR
						REPLACE COMPROB WITH IIF(I=1,'100','200')
						REPLACE CUENTA  WITH '1010000000'
						REPLACE TIPO    WITH IIF(I=1,'D','H')
						REPLACE MONTOS  WITH IIF(I=1,vDebe101,vHaber101)
					ENDIF
				ENDFOR
				INDEX ON ALLTR(Comprob)+CUENTA TO (vind1) FOR MONTH(Diar.Fecha) = xMes;
						.AND. VAL(SUBSTR(STR(YEAR(Diar.Fecha),4),3,2)) = xAno .AND. Diar.CodDep='113000' 
				&& Para acumular los saldos
				SUM Montos TO Debe100  FOR TIPO='D' AND comprob='100'
				SUM Montos TO Haber100 FOR TIPO='H' AND comprob='100'
				SUM Montos TO Debe200  FOR TIPO='D' AND comprob='200'
				SUM Montos TO Haber200 FOR TIPO='H' AND comprob='200'
				
				DO espera WITH 2
				DO REPORTE WITH 2, "AsieCent", 'Asiento de Centralizaci¢n',1,.F.,.T.
				CLOSE DATA
				SET PATH TO &xruta
				vdbf = vdir + '*.dbf'
				vidx = vdir + '*.idx'
				vcdx = vdir + '*.cdx'
				!ERASE &vdbf >NUL
				!ERASE &vidx >NUL
				!ERASE &vcdx >NUL
			ENDIF
	ENDCASE
	CLOSE DATABASE
	ERASE (vind1)
ENDIF
DEACTIVATE WINDOW AsieCent

IF !escolor
	RESTORE SCREEN FROM principal
ENDIF
SHOW POPUP menu, pop_03

RETURN


PROCEDURE Salida
*---------------
USE IN 2
ERASE &vindx

RELEASE    WINDOW AsieCent
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)


FUNCTION DESCRI1
*---------------
vAli=ALIAS()
xCuenta=LEFT(Diar.Cuenta,3)
vCuenta=LEFT(Diar.Cuenta,3)+'0000000'
SELECT Cuen
SEEK vCuenta
IF FOUND()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN


*FUNCTION DSUMA
*--------------
*vKey = substr(Cuenta,1,3)
*STORE 0 TO vDMonto,vHMonto
*SUSP
*SCAN WHILE substr(Cuenta,1,3) = vKey
*     IF Tipo='D'
*        vDMonto = vDMonto + Montos
*        vDTot =  vDTot + Montos
*     ELSE
*        vHMonto = vHMonto + Montos
*        vHTot =  vHTot + Montos
*        =HSUMA()
*     ENDIF
*ENDSCAN
*RETURN vDMonto


FUNCTION DSUMA
*-------------
PARAMETER vKey,vcomprob
*vKey = substr(Diar.Cuenta,1,3)
STORE 0 TO vDMonto
vRec=RECNO()
SCAN WHILE ALLTRIM(Diar.comprob)=vcomprob AND substr(Diar.Cuenta,1,3) = vKey .AND. Diar.Tipo = 'D'
        vDMonto = vDMonto + Montos
        vRec    = RECNO()
ENDSCAN
GO vRec
RETURN vDMonto


*FUNCTION HSUMA
*-------------
*RETURN vHMonto


FUNCTION HSUMA
*-------------
PARAMETER vKey,vcomprob
*vKey = substr(Diar.Cuenta,1,3)
STORE 0 TO vHMonto
vRec=RECNO()
SCAN WHILE ALLTRIM(Diar.comprob)=vcomprob AND SUBSTR(Diar.Cuenta,1,3) = vKey .AND. Diar.Tipo = 'H'
        vHmonto = vHMonto + Montos      
        vRec=RECNO()
ENDSCAN
GO vRec
RETURN vHMonto


FUNCTION UNO
*-----------
SELECT Diar
GO TOP
SUM Diar.Montos TO vUno FOR Diar.Tipo='D'
RETURN vUno


FUNCTION UNOS
*------------
SELECT Diar
GO TOP
SUM Diar.Montos TO vUnos FOR Diar.Tipo='H'
RETURN vUnos


FUNCTION vDebe
*-------------
SELECT Diar
GO TOP
SCAN
    IF Diar.Tipo='D'
       vMonDeb = vMonDeb + Diar.Montos
    ELSE
       vMonHab = vMonHab + Diar.Montos
    ENDIF
ENDSCAN
=vHaber()
RETURN vMonDeb


FUNCTION vHaber
*--------------
RETURN vMonHab


FUNCTION SumaDeb
*---------------
PARAMETER vComprob
IF VCOMPROB='100'
	SUM Montos TO ValorD FOR TIPO='D' AND comprob='100'
ELSE
	SUM Montos TO ValorD FOR TIPO='D' AND comprob='200'
ENDIF
RETURN ValorD


FUNCTION SumaHab
*---------------
PARAMETER vComprob
IF VCOMPROB='100'
	SUM Montos TO ValorH FOR TIPO='H' AND comprob='100'
ELSE
	SUM Montos TO ValorH FOR TIPO='H' AND comprob='200'
ENDIF
RETURN ValorH




PROCEDURE QuitaReten
*-------------------
SELE Diar
SET FILTER TO Comprob='98'
vind3= SYS(3) + '.idx'
INDEX ON ALLTRIM(Comprob)+Tipo  TO (vind3)
GO TOP
SCAN
	IF Reten ='S'
		Vrec = RECNO()
		vComprob = Diar.Comprob
		vMto     = Diar.Montos
		vTipo    = 'D'
		SEEK alltrim(vComprob)+vTipo
		IF FOUND()
			REPLACE Diar.montos WITH (Diar.Montos - vMto)
		ENDIF
	GO vRec
	ENDIF
ENDSCAN
SELE DIAR
SET FILTER TO
SET ORDER TO
RETURN

