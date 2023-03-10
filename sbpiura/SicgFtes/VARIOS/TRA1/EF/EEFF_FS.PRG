****************************
* EEFF_Fs.prg
* Crea F-1,F_2
****************************
PARAMETERS cRep,nMes,nAno

*cRep = 'F-1'
*nMes = 2
*nAno = 98
CLOS DATA
SET STEP ON
USE Cuentas IN 1 ORDER TAG cuentas1
USE \BENE2000\DATA\Saldona IN 2 ORDER TAG Saldona4
USE EstFin  IN 3 ORDER TAG EstFin1
USE RepFin  IN 4 ORDER TAG RepFin4
USE Parmae  IN 5 ORDER TAG Parmae1
SELE Saldona
SET RELATION TO Cuenta INTO Cuentas
SELE RepFin
IF SEEK(PADR(cRep,5,' ')+PADL(nAno,2,'0')+PADL(nMes,2,'0'))
	IF YesNo('El '+cRep+'ya ha Sido Generado. ?Desea Generar Otra Vez?' )
		DELE FOR CodRep=cRep AND Periodo=PADL(nAno,2,'0') AND NumMes=PADL(nMes,2,'0')
	 ELSE
		CLOS DATA
		RETURN
	ENDIF
ENDIF
SELE EstFin
SET FILTER TO CodRep=cRep
GO TOP
SCAN
	nTotDeb=0
	nTotHab=0
*	IF CUENTA='399'
*		SUSP
*	ENDIF
	SELE Saldona
	SEEK EstFin.Cuenta+STR(nAno,2)+STR(nMes,2)
	IF FOUND()
		IF EstFin.Columna$'1  2  5  6' 
			IF EstFin.DebHab='D'
				IF Cuenta#'104'
					nTotDeb = nTotDeb+(DsaldoAju-HSaldoAju)
				 ELSE
					nTotDeb = nTotDeb+(DsalDoAju)
				ENDIF
			 ELSE
				IF Cuenta#'104'
					nTotHab = nTotHab+(HsalDoAju-DSaldoAju)
				 ELSE
					nTotHab = nTotHab+(HsalDoAju)
				ENDIF
			ENDIF
		 ELSE
			IF EstFin.DebHab='D'
				IF Cuenta#'104'
					nTotDeb = nTotDeb+(DsalDona-HSaldona)
				 ELSE
					nTotDeb = nTotDeb+(DsalDona)
				ENDIF
			 ELSE
				IF Cuenta#'104'
					nTotHab = nTotHab+(HsalDona-DSaldona)
				 ELSE
					nTotHab = nTotHab+(HsalDona)
				ENDIF
			ENDIF
		ENDIF
	ENDIF
	SELE RepFin
	SEEK EstFin.CodRep+    PADL(nAno,2,'0')+PADL(nMes,2,'0')+EstFin.Cuenta+EstFin.Fila+EstFin.Columna
	IF FOUND()
		REPLACE Fecha	WITH DATE()
		REPLACE Hora	WITH TIME()
		REPLACE TotDeb WITH TotDeb+nTotDeb
		REPLACE TotHab WITH TotHab+nTotHab
	 ELSE
	 	APPEND BLANK
		REPLACE Fecha	WITH DATE()
		REPLACE Hora	WITH TIME()
		REPLACE Periodo WITH PADL(nAno,2,'0')
		REPLACE NumMes	WITH PADL(nMes,2,'0')
		REPLACE CodRep	WITH cRep
		REPLACE Cuenta	WITH EstFin.Cuenta
		REPLACE Fila	WITH EstFin.Fila
		REPLACE Columna	WITH EstFin.Columna
		REPLACE TotDeb WITH nTotDeb
		REPLACE TotHab WITH nTotHab
	ENDIF
ENDSCAN

SELE RepFin
SEEK PADR(cRep,5,' ')+PADL(nAno,2,'0')+PADL(nMes,2,'0')+'59'

IF FOUND()
	nSaldEjer = SaldEjer()
 ELSE
	nSaldEjer = 0
ENDIF

SCAN WHILE CodRep=PADR(cRep,5,' ') AND Periodo=PADL(nAno,2,'0') AND NumMes=PADL(nMes,2,'0') AND Cuenta='59'
	IF EMPTY(TotDeb)
		REPLACE TotHab WITH TotHab+nSaldEjer
	 ELSE
		REPLACE TotDeb WITH TotDeb+nSaldEjer
	ENDIF
ENDSCAN
RETURN

FUNCTION SaldEjer
*----------------
*Calcula el Saldo del Ejercicio 
*Para quitarlo del Saldo Acumulado
**********************************
PRIVATE cFiltro,cAlias,nRet
cAlias = ALIAS()
nRet=0
SELE Saldona
cFiltro=FILTER()
nRec = RECNO()
SET FILTER TO Mes=nMes AND Ano=nAno AND RIGHT(Cuenta,8)='00000000'
GO TOP
SCAN FOR (Cuentas.Tipo='1' OR Cuentas.Tipo='2')
	nRet = nRet+(DsaldoAju-HSaldoAju)
ENDSCAN
SET FILTER TO &cFiltro
GO nRec
SELE (cAlias)
RETURN nRet
