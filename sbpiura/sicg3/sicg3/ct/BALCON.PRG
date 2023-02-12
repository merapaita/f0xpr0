** -----------------------------------
** BALCONBE.PRG
** Reporte Balance de Constructivo 
** Autor  : Ing. Virna Alicia Vel squez - UDEP 
** Inicio : 13 de Diciembre de 1995
** -----------------------------------
** Abrir Archivos de trabajo

***
ON KEY LABEL F12 CANCEL
***

USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Cuentas IN 2 ORDER TAG Cuentas1        ALIAS Cuen
USE SaldoNa IN 3 ORDER TAG SaldoNa1        ALIAS SalN
USE Diario  IN 5

SELE SALN
SET RELATION TO CUENTA INTO CUEN

PUBLIC vDMovAcu,vHMovAcu,vDMovreg,vHMovreg,vDAjuste,vHAjuste,vDSalanoant,vHSalanoant,vniv,vDebe898,vHaber898,H89,D89,HAJU,DAJU,DSuma898,HSuma898
PUBLIC Vdsumana,Vhsumana,xdsaldoan,xhsaldoan,xdsaldoac,xhsaldoac,xdsaldore,xhsaldore,vdpatri,vhpatri,ZMES,var,mCta1
PUBLIC xdsalaju,xhsalaju,xdajuano,xhajuano,xdsaldo,xhsaldo,Result,SaldoD89,SaldoH89,VFECHA,FE2,acumDeb,acumHab,SIGUE
STORE 0 TO vano,vano, vsaldo, vMes, vDMovAcu,vHMovAcu,vDMovreg,vHMovreg,vDAjuste,vHAjuste,vDSalanoant,vHSalanoant,vDebe898,vHaber898,DSuma898,HSuma898
STORE 0 TO H89,D89,HAJU,DAJU,vdpatri,vhpatri,vdpresu,vhpresu,Result,SaldoD89,SaldoH89,var

filaux = SYS(3) + '.DBF'
vindx  = SYS(3) + '.Idx'

Xdsaldoac1=0
Xhsaldoac1=0
xdsaldo1=0
xhsaldo1=0
xdsalaju1=0
xhsalaju1=0




SELECT 8
CREATE TABLE (filaux);
    (cuenta   c(010),;
    mes       n(02,0),;     
    ano       n(02,0),;
    dsaldoan  n(18,4),;
    Hsaldoan  n(18,4),;
    dsumana   n(18,4),;
    hsumana   n(18,4),;
    dsaldoac  n(18,4),;
    Hsaldoac  n(18,4),;
    dsaldona  n(18,4),;
    Hsaldona  n(18,4),;
	Dajumes   n(18,4),;
	Hajumes   n(18,4),;
	Dajuano   n(18,4),;
	Hajuano   n(18,4),;
    dsaldore  n(18,4),;
    Hsaldore  n(18,4),;
	DSalaju   n(18,4),;
	HSalaju   n(18,4),;
	Dpatri    n(18,4),;
	Hpatri    n(18,4),;
    DGesRe    n(18,4),;
	HGesRe    n(18,4),;
	Dpresu    n(18,4),;
	Hpresu    n(18,4),;
	dSaldo    n(18,4),;
	hSaldo    n(18,4),;
    descri    c(030))

USE

USE (filaux) IN 8  EXCLUSIVE   ALIAS Temp



DO Inicia
DO salida
RETURN

PROCEDURE Inicia
*---------------
  SELECT parma
  SEEK 'OPERATCONSTR'
  IF ALLTRIM(Parma.Descri)='No'
     DO STANDBY WITH "El Proceso de Mayorizaci¢n del Constructivo Aun no ha sido Realizado"
     RETURN 
  ENDIF   

  DEFINE WINDOW BalComp FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Balance Constructivo  ' COLOR SCHEME 5
  ACTIVATE WINDOW BalComp
  
  xbalance = 'Mensual'
  vdigito = 1
  SELECT parma
  SEEK 'MESANOACTUAL'
  vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
  vMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
  vAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
  @ 1,01 SAY '          # D¡gitos : ' GET vdigito  FUNCTION '^ Dos D¡gitos;Detallado' 
  @ 5,01 SAY '          Mes y A¤o : ' GET vMes     PICTURE '99' VALID vMes > 0 .AND. vMes <= 12
  @ 5,28 GET vAno                           PICTURE '99'
  @ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
  READ
  DEACTIVATE WINDOW BalComp
  FE2 = finmes() 
  IF LASTKEY() # 27 AND OKCANCEL = 1
     SELECT Cuen     
          DO ESPERA WITH 1,'Reporte del Balance Constructivo en Proceso ....!'
     IF vdigito=1
        SET FILTER TO RIGHT(Cuen.Cuenta,8) = '00000000'
		SELE SALN
		SET FILTER TO RIGHT(Cuenta,8) = '00000000'
     ENDIF   
	 DO NUEVO
     SET FILTER TO
     CLOSE DATA
  ENDIF
  
  * DEACTIVATE WINDOW BalComp
  
  IF !escolor
     RESTORE SCREEN FROM principal
  ENDIF
*  SHOW POPUP menu, pop_03
RETURN

PROCEDURE NUEVO
*---------------

*MARCO
******
SELE Diario
COPY TO DiaTem FOR Comprob='00'
USE DiaTem
INDEX ON COMPROB+CUENTA TO vInd FOR Comprob='00'
*****

SELEC SALN
COPY TO aa field SALN.CUENTA, SALN.MES, SALN.ANO, SALN.DSUMANA,;
SALN.HSUMANA, SALN.DSALDONA, SALN.HSALDONA, SALN.DAJUMES, SALN.HAJUMES,;
SALN.DAJUANO, SALN.HAJUANO, CUEN.DESCRI, SALN.DSUMAJU, SALN.HSUMAJU;
FOR (mes<=vmes AND ano=vano) OR (mes=12 AND ano=vano-1)
USE AA IN 0 EXCLUSIVE
SELE aa
INDEX ON cuenta+STR(mes,2)+STR(ano,2) tag AA FOR ANO=VANO
GO TOP
DO NU1
RETURN


*AND (IIF(vmes=1,mes=vmes or mes=12,mes<=vmes or mes=12)=.T.;
*AND  IIF(vmes=1,ano=vano or ano=vano-1,ano=vano) = .T.) ;

PROCEDURE NU1
*------------
SELE AA
SCAN
	SIGUE       = .T.
 	vCuenta     = AA.Cuenta
 	varCuen     = AA.Cuenta
 	
	mcta=Cuenta()
	mCta1=Cuenta1('aa')
	SELE DiaTem
	SUM Montos TO mDSaldoAp FOR Tipo='D' AND Cuenta=mCta
	SUM Montos TO mHSaldoAp FOR Tipo='H' AND Cuenta=mCta

	SELE AA

	IF MES=1
		vDsaldoant  = SaldoAntD(vcuenta) && Saldos del mes anterior
 		vHsaldoant  = SaldoAntH(vcuenta)
	ENDIF
	
 	* vDsaldona   = Dacumact(vCuenta)     && Guardan los saldos acumulados
 	* vHsaldona   = Hacumact(vCuenta)
 	*----------
 	vHajuste=0
 	vDajuste=0
 	*----------
	=AcumMov()
	
	vDsaldona   = IIF(acumDeb>acumHab,acumdeb-acumhab,0)
 	vHsaldona   = IIF(acumDeb<acumHab,acumHab-acumDeb,0)
 	vDajuste    = Dajuste(vcuenta)
	IF vdajuste<=0           && Si es negativo
		IF vDajuste<0
			vhajuste = ABS(vDajuste)
			vdajuste=0
			SIGUE =.F.
		ENDIF
	ENDIF
	
	IF SIGUE
		vHajuste    = Hajuste(vcuenta)
		IF vHajuste<0  && Si es negativo
			vDajuste = ABS(vHajuste)
			vHajuste=0
		ENDIF
	ENDIF
	*---------
	
	IF mCta1#'898' AND mCta1#'89' AND SUBSTR(mCta1,3,1)#'8' and vDajuste>0
		DSuma898=DSuma898+vDajuste
	ENDIF
	IF mCta1#'898' AND mCta1#'89' AND SUBSTR(mCta1,3,1)#'8' and vHajuste>0
		HSuma898=HSuma898+vHajuste
	ENDIF

 	SELEC TEMP

	APPEND BLANK
   REPLACE cuenta   WITH varCuen
   REPLACE mes      WITH AA.mes
   REPLACE ano      WITH AA.ano

    *- Saldos del mes anterior
	REPLACE dsaldoan WITH  vdsaldoant
   REPLACE Hsaldoan WITH  vhsaldoant
	
    *- Movimientos del mes
	REPLACE dsumana  WITH acumDeb
   	REPLACE hsumana  WITH acumHab

	*- Saldos actuales del mes

   	REPLACE dsaldoac WITH IIF(DSUMANA>HSUMANA,DSUMANA-HSUMANA,0)
   	REPLACE Hsaldoac WITH IIF(HSUMANA>DSUMANA,HSUMANA-DSUMANA,0)


	*- Es necesario crear un campo mas a saldona, donde se guarden estos montos
	*- ajustes y regularizaciones. se ingresaran por nota contable
	
	REPLACE dsaldore  WITH 0
	REPLACE Hsaldore  WITH 0

   	*_ Saldos Ajustados
    
	REPLACE dsalaju  WITH IIF(dsaldoac>0,(dsaldoac+dsaldore)-(hsaldoac-hsaldore),0)
	REPLACE Hsalaju  WITH IIF(hsaldoac>0,(hsaldoac+hsaldore)-(dsaldoac+dsaldore),0)
	*- Saldos ajustados del mes. ACM.

	
	IF CUENTA>='2'
		REPLACE dajuano WITH vdajuste
		REPLACE Hajuano WITH vhajuste
     ELSE
	   	REPLACE dajuano WITH 0
   		REPLACE Hajuano WITH 0
	ENDIF

*	IF SUBST(CUENTA,3,1)='8' AND CUENTA>='2'
*		REPLACE dajuano WITH vdajuste
*		REPLACE Hajuano WITH vhajuste
*     ELSE
*   	REPLACE dajuano WITH 0
*   	REPLACE Hajuano WITH 0
*	ENDIF
   	

	*- Saldos actualizados incluyendo los ajustes
	*------------------------------------
   *REPLACE dsaldo WITH vsaldoD
   *REPLACE Hsaldo WITH vsaldoH
	*------------------------------------
	
	VAR = (DSALAJU+DAJUANO)-(HSALAJU+HAJUANO)

	IF var>0
		REPLACE dsaldo WITH var
		REPLACE Hsaldo WITH 0
	 ELSE
		REPLACE Hsaldo WITH ABS(var)
	   REPLACE Dsaldo WITH 0
	ENDIF
	
	
	*- Saldos de las cuentas Patrimoniales
	REPLACE dpatri WITH (IIF(aa.Cuenta<'6',DSALDO,0))
	REPLACE hpatri WITH (IIF(aa.Cuenta<'6',HSALDO,0))

	*- Saldos de las cuentas Presupuestales
	REPLACE dpresu WITH (IIF(aa.Cuenta>='9',DSALDO,0))
	REPLACE hpresu WITH (IIF(aa.Cuenta>='9',HSALDO,0))
	
	
	*- Saldos de las cuentas de Gestion y Resultado
	REPLACE dgesre WITH (IIF(aa.Cuenta>'5' and aa.cuenta<'9',DSALDO,0))
	REPLACE hgesre WITH (IIF(aa.Cuenta>'5' and aa.cuenta<'9',HSALDO,0))
	
	REPLACE descri WITH AA.descri

	SELE aa
ENDSCAN

INDEX ON CUENTA+STR(12-MES) UNIQUE TAG TEMPO1 ADDITIVE
GO TOP
SCAN
	VCUENTA1=CUENTA
	VCUENTA2=CUENTA
	DO WHILE VCUENTA1=VCUENTA2 AND !EOF()
		SKIP
		VCUENTA2=CUENTA
		IF VCUENTA1=VCUENTA2 AND !EOF()
			DELETE
			VCUENTA2=CUENTA
		ENDIF
	ENDDO
	SKIP -1
ENDSCAN 

SELE Temp   
INDEX ON cuenta+STR(mes,2)+STR(ano,2) TO (vindx)
* Ahora se graban los saldos actualizados finales de las cuentas
* a la base de inventario (INVENT), para los reportes de 
* Inv. Inicial e Iin. Final

IF VDIGITO=2
	DO PASAINV
ENDIF
* HACER DICIEMBRE
DO AsienRef

SUM dPatri   TO vDpatri FOR LEN(ALLTR(Cuenta))=2
SUM hPatri   TO vhpatri FOR LEN(ALLTR(Cuenta))=2

SUM dGesRe   TO vDGesRe FOR LEN(ALLTR(Cuenta))=2
SUM hGesRe   TO vhGesRe FOR LEN(ALLTR(Cuenta))=2

* Esta variable guarda el resultado de la cta 89, 
Result = vHgesre-vDgesre

*-

* SUM dpresu   TO vDpresu FOR LEN(ALLTR(Cuenta))=2
* SUM hPresu   TO vhpresu FOR LEN(ALLTR(Cuenta))=2
SUM dpresu   TO vDpresu FOR LEN(ALLTR(Cuenta))=3
SUM hPresu   TO vhpresu FOR LEN(ALLTR(Cuenta))=3
*-----------------------------
* 


*------------------------
SUM dsaldoan TO xdsaldoan FOR LEN(ALLTR(Cuenta))=2
SUM hsaldoan TO xhsaldoan FOR LEN(ALLTR(Cuenta))=2
SUM dsumana  TO xdsumana  FOR LEN(ALLTR(Cuenta))=2
SUM hsumana  TO xhsumana  FOR LEN(ALLTR(Cuenta))=2

SUM dsaldoac TO xdsaldoac FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)<'9'
SUM hsaldoac TO xhsaldoac FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)<'9'
SUM dsaldo   TO xdsaldo   FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)<'9'
SUM hsaldo   TO xhsaldo   FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)<'9'
SUM dsalaju  TO xdsalaju  FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)<'9'
SUM hsalaju  TO xhsalaju  FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)<'9'
IF vano>=97
	SUM dsaldoac TO xdsaldoac1 FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
	SUM hsaldoac TO xhsaldoac1 FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
	SUM dsaldo   TO xdsaldo1   FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
	SUM hsaldo   TO xhsaldo1   FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
	SUM dsalaju  TO xdsalaju1  FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
	SUM hsalaju  TO xhsalaju1  FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
ELSE
	IF Vmes>=4
  		SUM dsaldoac TO xdsaldoac1 FOR LEN(ALLTR(Cuenta))=3  and left(cuenta,1)='9'
  		SUM hsaldoac TO xhsaldoac1 FOR LEN(ALLTR(Cuenta))=3  and left(cuenta,1)='9'
		SUM dsaldo   TO xdsaldo1   FOR LEN(ALLTR(Cuenta))=3  and left(cuenta,1)='9'
		SUM hsaldo   TO xhsaldo1   FOR LEN(ALLTR(Cuenta))=3  and left(cuenta,1)='9'
		SUM dsalaju  TO xdsalaju1  FOR LEN(ALLTR(Cuenta))=3  and left(cuenta,1)='9'
		SUM hsalaju  TO xhsalaju1  FOR LEN(ALLTR(Cuenta))=3  and left(cuenta,1)='9'
  	* ELSE		
	*	SUM dsaldoac TO xdsaldoac1 FOR LEN(ALLTR(Cuenta))=2    and left(cuenta,1)='9'  
	*	SUM hsaldoac TO xhsaldoac1 FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)='9'  
	*	SUM dsaldo   TO xdsaldo1   FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)='9'
	*	SUM hsaldo   TO xhsaldo1   FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)='9'
	*	SUM dsalaju  TO xdsalaju1  FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)='9'
	*	SUM hsalaju  TO xhsalaju1  FOR LEN(ALLTR(Cuenta))=2   && and left(cuenta,1)='9'
	ENDIF
ENDIF
xdsaldoac2 = xdsaldoac + Xdsaldoac1
xhsaldoac2 = xhsaldoac + Xhsaldoac1
xdsaldo2   = xdsaldo + xdsaldo1
xhsaldo2   = xhsaldo + xhsaldo1
xdsalaju2  = xdsalaju + xdsalaju1
xhsalaju2  = xhsalaju + xhsalaju1

SUM dsaldore TO xdsaldore FOR LEN(ALLTR(Cuenta))=2
SUM hsaldore TO xhsaldore FOR LEN(ALLTR(Cuenta))=2
SUM ROUND(dajuano,2)  TO xdajuano  FOR LEN(ALLTR(Cuenta))=2
SUM ROUND(hajuano,2)  TO xhajuano  FOR LEN(ALLTR(Cuenta))=2


IF vdigito=1
	SET FILTER TO LEN(ALLT(CUENTA))=2
ENDIF

DO ESPERA WITH 2
*-------------------
SEEK "90"
IF FOUND()
    REPLACE dsaldoac WITH vdpresu
    REPLACE hsaldoac WITH vhpresu
    REPLACE dsalaju  WITH vdpresu
    REPLACE hsalaju  WITH vhpresu
    REPLACE dsaldo   WITH vdpresu
    REPLACE hsaldo   WITH vhpresu
    REPLACE dpresu   WITH vdpresu
    REPLACE hpresu   WITH vhpresu
ENDIF

*-------------------
GO TOP
* edgar
DO adicional
*-----
DO REPORTE WITH 2, "BalCons1", ' Reporte Balance Constructivo parte 1 ',2,.F.,.T.				

GO TOP
DO REPORTE WITH 2, "BalConsP", ' Reporte Balance Constructivo parte 2 ',2,.F.,.T.				

SELE Parma
SEEK 'RESULT'+'EJERCI'+PADL(ALLTRIM(STR(VMES,2)),2,'0')+STR(VANO,2)

IF FOUND()
	*REPLACE NumDec WITH IIF(SaldoD89<>0,saldoD89+Result,SaldoH89+Result)
	REPLACE NumDec WITH VDPATRI-VHPATRI
ELSE
	APPEND BLAN
	REPLACE Tipo      WITH 'RESULT'
	REPLACE Codigo    WITH 'EJERCI'
	REPLACE Codigoaux WITH PADL(ALLTRIM(STR(VMES,2)),2,'0')+STR(VANO,2)
	REPLACE Descri    WITH 'RESULTADO DEL EJERCICIO'
	REPLACE NumDec WITH VDPATRI-VHPATRI
ENDIF	
RETURN


* En este reporte, esta funcion me retorna el saldo actual de la cuenta en
* el mes actual, osea Dsaldona
FUNCTION dacumact
*----------------
PARAMETER vcuenta
REC =RECNO()
VALI=ALIAS()
vdsumana=dsumana
STORE 0 TO vdsaldona
SEEK Cuenta+STR(vmes,2)+STR(vano,2) 
   IF FOUND()
      vdsaldona = dsaldona
   ENDIF
   SELE (vAli)
   GO REC
RETURN vdsaldona



* En este reporte esta funcion me retorna el saldo actual de la cuenta en
* el mes actual, osea Hsaldona
FUNCTION Hacumact
*----------------
PARAMETER vcuenta
VREC=RECNO()
VALI=ALIAS()
vhsumana=hsumana
STORE 0 to vhsaldona
	SEEK cuenta+STR(vmes,2)+STR(vano,2)
	IF FOUND()
      vhsaldona = hsaldona
   	ENDIF
	SELE (vAli)
	GO VREC
RETURN vhsaldona


* Jala el saldo del mes de anterior (Para 01 viene a ser los saldos de apertura
* que estan incluidos en el movimiento de enero.)

FUNCTION SaldoAntD
*------------------
PARAMETER vcuenta
PRIVATE vSaldoAD
VREC=RECNO()
VALI=ALIAS()
IF vmes=1
    fMes=12
    fAno=vano-1
ELSE
    fMes=vmes-1   
    fAno=vano
ENDIF
SELE SALN
SEEK vcuenta+STR(fmes,2)+STR(fano,2)
IF FOUND()
	vSaldoAD = Dsaldona
	SELE (vali)
	RETURN vSaldoAD
ELSE
	vSaldoAD = 0
	SELE (vali)
    RETURN vSaldoAD
ENDIF



* Jala el saldo del mes de anterior que viene a ser los saldos de apertura
* que estan incluidos ene el movimiento de enero.
FUNCTION SaldoAntH
*------------------
PARAMETER vcuenta
PRIVATE vSaldoAH
VREC  = RECNO()
VALI  = ALIAS()
IF vmes=1
    fMes=12
    fAno=vano-1
ELSE
    fMes=vmes-1   
    fAno=vano
ENDIF
SELE SALN
SEEK vcuenta+STR(fmes,2)+STR(fano,2)
IF FOUND()
	vSaldoAh = Hsaldona
	SELE (vAli)
	RETURN vSaldoAH
ELSE 
	vSaldoAh = 0
	SELE (vAli)
	RETURN vSaldoAH
ENDIF


* Esta funcion calcula los saldos actualizados
* suma los saldos de las cuentas + su ajuste
FUNCTION dacum
*----------------
PARAMETER vcuenta
REC = RECNO()
VALI= ALIAS()
vdsumana = dsumana
zmes     = aa.mes
STORE 0 TO vdsaldo
SELE SalN
*SEEK vcuenta+STR(vmes,2)+STR(vano,2) 
SEEK  vcuenta+STR(zmes,2)+STR(vano,2) 
   IF FOUND()
      vDsaldo = Dsaldoaju
   ENDIF
   SELE (VALI)
   GO REC
RETURN vdsaldo


* Esta funcion calcula los saldos actualizados
* suma los saldos de las cuentas + su ajuste
FUNCTION hacum
*----------------
PARAMETER vcuenta
REC = RECNO()
VALI= ALIAS()
vdsumana=dsumana
STORE 0 to vHsaldo
SELE SalN
SEEK cuenta+STR(vmes,2)+STR(vano,2) 
   IF FOUND()
      vHsaldo = Hsaldoaju
   ENDIF
   SELE (VALI)
   GO REC
RETURN vHsaldo


PROCEDURE QuitaCero
*------------------
PARAMETER vcuen1
PRIVATE vCuen
VALI = ALIAS()
REC  = RECNO()
SELE Cuen
SEEK vCuen1
IF FOUND()
	vniv = nivel
ELSE
	vniv = 0
ENDIF
DO CASE
	CASE vniv = 1
		vCuen = SUBSTR(vCuen1,1,2)
	CASE vniv = 2
		vCuen = SUBSTR(vCuen1,1,3)		
	CASE vniv = 3
		vCuen = SUBSTR(vCuen1,1,5)
	CASE vniv = 4
		vCuen = SUBSTR(vCuen1,1,7)		
	OTHERWISE
		vCuen = vCuen1
ENDCASE
SELE (VALI)
GO REC
RETURN vCuen

FUNCTION dajuste
*---------------
PARAMETER vCuenta
PRIVATE valor,valacu
valor=0
valacu=0
	vAli = ALIAS()
	vOrd = ORDER()
	SELE SalN
   SET ORDER TO Saldona4
    * Se ubica el Primero para que sea acumulado  a la Fecha
	SEEK vcuenta+STR(vano,2)                &&+STR(vMes,2)
   SCAN WHILE vcuenta+STR(vano,2)=SalN.Cuenta+STR(vano,2) AND SalN.mes<=vmes
    * IIF(vmes=12,SalN.mes<vmes,SalN.mes<=vmes)
		IF FOUND()
			IF SUBSTR(CUENTA,3,1)='8'
				IF CUENTA='89'
					valor=HSuma898 	 			 &&DSUMAJU
					valacu=valacu+valor
					EXIT
				ELSE	
					IF dsumaju>hsumaju
						* valor = DSUMAJU-HSUMAJU
						valor = (DSUMAJU-DSUMANA)-(HSUMAJU-HSUMANA)
						valacu=valacu+valor
					ELSE
						VALOR =0
					ENDIF
				ENDIF
			ELSE
				IF cuenta<>'89'
					IF (Hsumana=0 and dsumana=0) 
						IF dsumaju>hsumaju  && Es una cuenta que no tiene mov. en el mes
							* valor = DSUMAJU-HSUMAJU
							valor = (DSUMAJU-DSUMANA)-(HSUMAJU-HSUMANA)
							valacu=valacu+valor
						ELSE
							valor = 0
						ENDIF
					ELSE
						* Si el valor es negativo, el ajuste se guarda
						* en el haber.
						valor = (DSUMAJU-DSUMANA)-(HSUMAJU-HSUMANA)
*						Valor = Dsaldoaju-Dsaldona
					    valacu=valacu+valor
					ENDIF
				 ELSE
					valor = (DSUMAJU-DSUMANA)-(HSUMAJU-HSUMANA)
*					valor = DSUMAJU 
					valacu=valacu+valor
				ENDIF
			ENDIF
		ELSE
			valor=0
		ENDIF
	ENDSCAN
	*--------------------
	SELE SalN
    SET ORDER TO Saldona1
    *--------------------
	SELE (vAli)
	SET ORDE TO (vOrd)
RETURN valacu

FUNCTION hajuste
*---------------
PARAMETER vCuenta
PRIVATE valor,valacu
valor=0
valacu=0
	vAli = ALIAS()
	vOrd = ORDER()
	vCuenta  = Cuenta
    SELE SalN
    SET ORDER TO Saldona4
    * Se ubica el Primero para que sea acumulado  a la Fecha
	SEEK vcuenta+STR(vano,2)			&&+STR(vMes,2)
    SCAN WHILE vcuenta+STR(vano,2)=SalN.Cuenta+STR(vano,2) AND SalN.mes<=vmes
		IF FOUND()
			IF SUBSTR(CUENTA,3,1)='8'
				IF CUENTA='89'
					valor = DSuma898				&&HSUMAJU
					valacu=valacu+valor
					EXIT
				ELSE
					IF hsumaju>dsumaju
						* valor = HSUMAJU-DSUMAJU
						valor = (HSUMAJU-HSUMANA)-(DSUMAJU-DSUMANA)
						valacu=valacu+valor
					ELSE
						VALOR =0
					ENDIF
				ENDIF	
			ELSE		
				IF cuenta<>'89'
					IF (Hsumana=0 and dsumana=0) 
						IF Hsumaju>Dsumaju
							*valor = hSUMAJU-dSUMAJU
							valor = (HSUMAJU-HSUMANA)-(DSUMAJU-DSUMANA)
							valacu=valacu+valor
						ELSE
							valor = 0
						ENDIF	
					ELSE
						valor = (HSUMAJU-HSUMANA)-(DSUMAJU-DSUMANA)
*						Valor = HsaldoAju - Hsaldona
 						valacu=valacu+valor
					ENDIF
				 ELSE
					valor = (HSUMAJU-HSUMANA)-(DSUMAJU-DSUMANA)
*					valor = HSUMAJU
					valacu=valacu+valor
				ENDIF
			
			ENDIF
		ELSE
			Valor=0
		ENDIF
	ENDSCAN
	*--------------------
	SELE SalN
    SET ORDER TO Saldona1
    *--------------------
	SELE (vAli)
	SET ORDE TO (vOrd)
RETURN valacu


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vmes*9-8,9)) + ' ' + STR(vano,2)


PROCEDURE Salida
*---------------
RELEASE    WINDOW BALCOMP
ACTIVATE   SCREEN
CLOSE DATABASE
IF FILE("AA.DBF")
   !ERASE AA.*
ENDIF   
RETURN


* Calcula el fin de mes
FUNCTION FinMes
*-------------
zmes = vmes
XANO = VANO
DO CASE 
	CASE  zMes= 2
		IF MOD(xano, 4)=0
			vFecha = '29/02/' + STR(xAno,2)
		 ELSE
		 	vFecha = '28/02/'+ STR(xAno,2)
		ENDIF
	CASE (MOD(zmes, 2)=0 AND zmes<7) OR (MOD(zmes, 2)=1 AND BETWEEN(zmes,7,12))
		vFecha = '30/' + PADL(ALLTRIM(STR(zmes,2)),2,'0') + '/' + STR(VANO,2)
	OTHERWISE
		vFecha = '31/' + PADL(alltrim(STR(zmes,2)),2,'0') + '/' + STR(VANO,2)
ENDCASE		
RETURN vFecha


FUNCTION AcumMov
*---------------
SELE aa
vcuenta1 = cuenta
vcuenta2 = cuenta
STORE 0 TO acumDeb,acumHab
DO WHILE vcuenta1 = vcuenta2 and mes<=vmes AND ANO=VANO AND !EOF()
	   acumDeb = acumDeb + dsumana
	   acumHab = acumHab + Hsumana
	SKIP
	vcuenta2 = aa.cuenta
ENDDO
SKIP-1
RETURN


*-Asiento referencial de apertura
* siempre so los saldos inicialies
* de las cuentas al principio de a¤o
PROCEDURE AsienRef
*------------------
PRIVATE mcta
***MARCO
*Esto se realiz¢ por que el plan de cuentas se modific¢
*SELE Diario
*COPY TO DiaTem FOR Comprob='00'
*USE DiaTem
*INDEX ON COMPROB+CUENTA TO vInd FOR Comprob='00'
SELE TEMP
REPLACE ALL DSALDOAN  WITH 0
REPLACE ALL HSALDOAN  WITH 0
GO TOP
SCAN
	mcta=Cuenta()
	SELE DiaTem
	SUM Montos TO mDSaldoAn FOR Tipo='D' AND Cuenta=mCta
	SUM Montos TO mHSaldoAn FOR Tipo='H' AND Cuenta=mCta
	SELE Temp
	REPLACE dSaldoAn WITH mDSaldoAn
	REPLACE hSaldoAn WITH mHSaldoAn
ENDSCAN

***
* No borrar esto
*SELE TEMP
*REPLACE ALL DSALDOAN  WITH 0
*REPLACE ALL HSALDOAN  WITH 0
*SELE aa
*SET INDEX TO
*INDEX ON cuenta+STR(mes,2)+STR(ano,2) tag AA FOR (ANO=VANO-1 AND MES=12)
*GO TOP
*SCAN
*	SELE Temp
*	SEEK aa.cuenta
*	IF FOUND()
*		REPLACE dSaldoAn WITH AA.dsaldona
*		REPLACE hSaldoAn WITH AA.hsaldona
*	ELSE
*		APPEND BLANK
*	 	REPLACE Cuenta   WITH AA.Cuenta
*		REPLACE dSaldoAn WITH AA.dsaldona
*		REPLACE hSaldoAn WITH AA.hsaldona
*		REPLACE	dsaldoaC WITH AA.dsaldona
*		REPLACE	HsaldoaC WITH AA.Hsaldona
*		REPLACE	dsaldo   WITH AA.dsaldona
*		REPLACE	Hsaldo   WITH AA.Hsaldona
*		REPLACE dpatri   WITH (IIF(aa.Cuenta<'6',AA.dsaldona,0))
*		REPLACE hpatri   WITH (IIF(aa.Cuenta<'6',AA.hsaldona,0))
*		REPLACE dgesre   WITH (IIF(aa.Cuenta>'5' and aa.cuenta<'9',AA.dsaldona,0))
*		REPLACE hgesre   WITH (IIF(aa.Cuenta>'5' and aa.cuenta<'9',AA.hsaldona,0))
*	ENDIF
*	sele aa
*ENDSCAN	

* Ahora se quitan los ceros a la cuenta

SELE Temp
GO TOP
SCAN
 	VarCuen     = QuitaCero(Cuenta)	
 	REPLACE Cuenta WITH varCuen
ENDSCAN
*DO REASAL
RETURN
	
PROCEDURE PASAINV
*-----------------
USE INVENT IN 9 ALIAS INVENT EXCLU
SELE INVENT
DELE FOR MES=VMES AND ANO=VANO
*PACK
SELE TEMP
GO TOP
SCAN
	SCATTER MEMVAR
	SELE INVENT
	IF F_APPD()
		REPLACE CUENTA  WITH TEMP.CUENTA
		REPLACE MES     WITH TEMP.MES
		REPLACE ANO     WITH TEMP.ANO
		REPLACE DSALAJU WITH TEMP.DSALDO
		REPLACE HSALAJU WITH TEMP.HSALDO
		REPLACE MESACT  WITH  VMES
	ENDIF
	SELE TEMP
ENDSCAN
RETURN

PROCEDURE XREASAL
*-----------------
* PROCESO PARA REASIGNAR SALDOS 
SELE TEMP
SET FILTER TO LEN(ALLT(CUENTA))<=3 AND CUENTA<'9'
GO TOP

SCAN
	IF LEN(ALLTRIM(CUENTA))=2
		vrecno = recno()
	endif
	vcuenta =left(cuenta,2)
	skip
	vcuenta1=left(cuenta,2)
	vnum=2
		STORE 0 TO VDSALDOAN,VHSALDOAN,VDSUMANA,VHSUMANA,VDSALDOAC,VHSALDOAC,VDSALDONA,VHSALDONA
		STORE 0 TO VDAJUANO,VHAJUANO
		tiene=.F.
		DO WHILE vcuenta=vcuenta1	
			VDSALDOAN = VDSALDOAN + DSALDOAN
			VHSALDOAN = VHSALDOAN + HSALDOAN
			VDSUMANA  = VDSUMANA  + DSUMANA
			VHSUMANA  = VHSUMANA  + HSUMANA
			VDSALDOAC = VDSALDOAC + DSALDOAC
			VHSALDOAC = VHSALDOAC + HSALDOAC
			VDSALDONA = VDSALDONA + DSALDONA
			VHSALDONA = VHSALDONA + HSALDONA
			VDAJUANO  = VDAJUANO  + DAJUANO
			VHAJUANO  = VHAJUANO  + HAJUANO
			SKIP
			vcuenta1=left(cuenta,vnum)
		ENDDO
		SKIP -1
		VRECNO1 = RECNO()
    	GO VRECNO
    		REPLACE DSUMANA   WITH VDSUMANA
			REPLACE HSUMANA   WITH VHSUMANA

			vmonto  = VDSALDOAN-VHSALDOAN
			REPLACE DSALDOAN  WITH IIF(vmonto>0,VMONTO,0)
			REPLACE HSALDOAN  WITH IIF(vmonto<0,VMONTO*-1,0)

			vmonto1 = VDSALDOAC-VHSALDOAC
			REPLACE DSALDOAC  WITH IIF(vmonto1>0,VMONTO1,0)
			REPLACE HSALDOAC  WITH IIF(vmonto1<0,VMONTO1*-1,0)

			* IF CUENTA<>'89'
			*	vmonto2 = VDAJUANO-VHAJUANO
			*	REPLACE DAJUANO  WITH IIF(vmonto2>0,VMONTO2,0)
			*	REPLACE HAJUANO  WITH IIF(vmonto2<0,VMONTO2*-1,0)
			* ELSE
				REPLACE DAJUANO  WITH VDAJUANO
				REPLACE HAJUANO  WITH VHAJUANO
			*ENDIF	
	
			Vmonto3= (DSALAJU+DAJUANO)-(HSALAJU+HAJUANO)
			REPLACE Dsaldo  WITH IIF(vmonto3>0,VMONTO3,0)
			REPLACE Hsaldo  WITH IIF(vmonto3<0,VMONTO3*-1,0)
			REPLACE dpatri  WITH (IIF(Cuenta<'6',DSALDO,0))
			REPLACE hpatri  WITH (IIF(Cuenta<'6',HSALDO,0))
			
			REPLACE dpresu  WITH (IIF(Cuenta>='9',DSALDO,0))
			REPLACE hpresu  WITH (IIF(Cuenta>='9',HSALDO,0))
			
			REPLACE dgesre WITH (IIF(Cuenta>'5' and cuenta<'9',DSALDO,0))
			REPLACE hgesre WITH (IIF(Cuenta>'5' and cuenta<'9',HSALDO,0))
			
		GO VRECNO1
	ENDSCAN	
	SET FILTER TO
RETURN

PROCEDURE REASAL
*-----------------
* PROCESO PARA REASIGNAR SALDOS 
USE DIARIO IN 0 ALIAS DIAR
SELE DIAR
SUM MONTOS TO DEBE  FOR TIPO='D' AND CUENTA='1010000000' AND MONTH(FECEMI)<=VMES
SUM MONTOS TO HABER FOR TIPO='H' AND CUENTA='1010000000' AND MONTH(FECEMI)<=VMES
USE
SELE TEMP
DELETE FOR MES=0
FOR I=1 TO 4
	DO CASE
		CASE I=1
			SET FILTER TO LEN(ALLT(CUENTA))>6 AND CUENTA<'9'
		CASE I=2
			SET FILTER TO LEN(ALLT(CUENTA))>=5 and LEN(ALLT(CUENTA))<8  AND CUENTA<'9'
		CASE I=3
			SET FILTER TO LEN(ALLT(CUENTA))>=3 and LEN(ALLT(CUENTA))<6  AND CUENTA<'9'
		CASE I=4
			SET FILTER TO LEN(ALLT(CUENTA))<=3 AND CUENTA<'9'
	ENDCASE		
	GO TOP

	SCAN
		DO CASE
			CASE I=1
				IF LEN(ALLTRIM(CUENTA))=7
					vrecno = recno()
				endif
				vcuenta =left(cuenta,7)
				skip
				vcuenta1=left(cuenta,7)
				vnum=7
			CASE I=2
				IF LEN(ALLTRIM(CUENTA))=5
					vrecno = recno()
				endif
				vcuenta =left(cuenta,5)
				skip
				vcuenta1=left(cuenta,5)
				vnum=5
			CASE I=3
				IF LEN(ALLTRIM(CUENTA))=3
					vrecno = recno()
				endif
				vcuenta =left(cuenta,3)
				skip
				vcuenta1=left(cuenta,3)
				vnum=3
			CASE I=4
				IF LEN(ALLTRIM(CUENTA))=2
					vrecno = recno()
				endif
				vcuenta =left(cuenta,2)
				skip
				vcuenta1=left(cuenta,2)
				vnum=2
		ENDCASE		
		STORE 0 TO VDSALDOAN,VHSALDOAN,VDSUMANA,VHSUMANA,VDSALDOAC,VHSALDOAC,VDSALDONA,VHSALDONA
		STORE 0 TO VDAJUANO,VHAJUANO,VDSALAJU,VHSALAJU,vdsaldo,vhsaldo
		tiene=.F.
		DO WHILE vcuenta=vcuenta1	
			VDSALDOAN = VDSALDOAN + DSALDOAN
			VHSALDOAN = VHSALDOAN + HSALDOAN
			
			VDSUMANA  = VDSUMANA  + DSUMANA
			VHSUMANA  = VHSUMANA  + HSUMANA
			
			REPLACE DSALDOAC WITH IIF(DSUMANA>HSUMANA,DSUMANA-HSUMANA,0)
			REPLACE HSALDOAC WITH IIF(DSUMANA<HSUMANA,HSUMANA-DSUMANA,0)
			
  			REPLACE dsalaju  WITH IIF(dsaldoac>0,(dsaldoac+dsaldore)-(hsaldoac-hsaldore),0)
   			REPLACE Hsalaju  WITH IIF(hsaldoac>0,(hsaldoac+hsaldore)-(dsaldoac+dsaldore),0)

			VDSALAJU = VDSALAJU + DSALAJU
			VHSALAJU = VHSALAJU + HSALAJU

			monsal= (DSALAJU+DAJUANO)-(HSALAJU+HAJUANO)

			REPLACE Dsaldo  WITH IIF(monsal>0,monsal,0)
			REPLACE Hsaldo  WITH IIF(monsal<0,monsal*-1,0)
			VDSALDO = VDSALDO + DSALDO
			VHSALDO = VHSALDO + HSALDO
			
			VDSALDONA = VDSALDONA + DSALDONA
			VHSALDONA = VHSALDONA + HSALDONA
			
			VDAJUANO  = VDAJUANO  + DAJUANO
			VHAJUANO  = VHAJUANO  + HAJUANO
			
			tiene=.t.
			SKIP
			vcuenta1=left(cuenta,vnum)
		ENDDO
		SKIP -1
		VRECNO1 = RECNO()
    	GO VRECNO
		if tiene
			
    		REPLACE DSUMANA   WITH VDSUMANA
			REPLACE HSUMANA   WITH VHSUMANA

			REPLACE DSALDOAC WITH IIF(DSUMANA>HSUMANA,DSUMANA-HSUMANA,0)
			REPLACE HSALDOAC WITH IIF(DSUMANA<HSUMANA,HSUMANA-DSUMANA,0)

			vmonto  = VDSALDOAN-VHSALDOAN
			REPLACE DSALDOAN  WITH IIF(vmonto>0,VMONTO,0)
			REPLACE HSALDOAN  WITH IIF(vmonto<0,VMONTO*-1,0)
	
			vmonto4 = VDSALAJU-VHSALAJU
			REPLACE DSALAJU  WITH IIF(vmonto4>0,VMONTO4,0)
			REPLACE HSALAJU  WITH IIF(vmonto4<0,VMONTO4*-1,0)

			* IF CUENTA<>'89'
			* 	vmonto2 = VDAJUANO-VHAJUANO
			*	REPLACE DAJUANO  WITH IIF(vmonto2>0,VMONTO2,0)
			* 	REPLACE HAJUANO  WITH IIF(vmonto2<0,VMONTO2*-1,0)
			* ELSE
			REPLACE DAJUANO  WITH VDAJUANO
			REPLACE HAJUANO  WITH VHAJUANO
			*ENDIF	
			
			monsal= VDSALDO-VHSALDO
			REPLACE Dsaldo  WITH IIF(monsal>0,monsal,0)
			REPLACE Hsaldo  WITH IIF(monsal<0,monsal*-1,0)
			REPLACE dpatri  WITH (IIF(Cuenta<'6',DSALDO,0))
			REPLACE hpatri  WITH (IIF(Cuenta<'6',HSALDO,0))
			REPLACE dpresu  WITH (IIF(Cuenta>='9',DSALDO,0))
			REPLACE hpresu  WITH (IIF(Cuenta>='9',HSALDO,0))
			REPLACE dgesre WITH (IIF(Cuenta>'5' and cuenta<'9',DSALDO,0))
			REPLACE hgesre WITH (IIF(Cuenta>'5' and cuenta<'9',HSALDO,0))
		ENDIF	
		GO VRECNO1
	ENDSCAN	
	SET FILTER TO
ENDFOR
GO TOP
REPLA DSUMANA WITH DSUMANA+DEBE
REPLA HSUMANA WITH HSUMANA+HABER
REPLACE DSALDOAC WITH IIF(DSUMANA>HSUMANA,DSUMANA-HSUMANA,0)
REPLACE HSALDOAC WITH IIF(DSUMANA<HSUMANA,HSUMANA-DSUMANA,0)
REPLACE DSALDONA WITH IIF(DSALDOAC>HSALDOAC,DSALDOAC-HSALDOAC,0)
REPLACE HSALDONA WITH IIF(DSALDOAC<HSALDOAC,HSALDOAC-DSALDOAC,0)
SKIP
REPLA DSUMANA WITH DSUMANA+DEBE
REPLA HSUMANA WITH HSUMANA+HABER
REPLACE DSALDOAC WITH IIF(DSUMANA>HSUMANA,DSUMANA-HSUMANA,0)
REPLACE HSALDOAC WITH IIF(DSUMANA<HSUMANA,HSUMANA-DSUMANA,0)
REPLACE DSALDONA WITH IIF(DSALDOAC>HSALDOAC,DSALDOAC-HSALDOAC,0)
REPLACE HSALDONA WITH IIF(DSALDOAC<HSALDOAC,HSALDOAC-DSALDOAC,0)


* vOk=.F.
* IF VANO>=97
* 	vOk=.t.
* ELSE
*	IF Vmes>=4	
*		vOk=.T.
*	ENDIF
* endif
* if vOk
*	locate for cuenta='900'
*	scatter memvar
*	append blan
*	gather memvar
*	replace cuenta with '90'
*	vrecno1 = recno()
*	Sum dsaldoan to vdsaldoan for len(allt(cuenta))=3 and cuenta='90'
*	Sum hsaldoan to vhsaldoan for len(allt(cuenta))=3 and cuenta='90'
*	Sum dsumana  to vdsumana  for len(allt(cuenta))=3 and cuenta='90'
*	Sum hsumana  to vhsumana  for len(allt(cuenta))=3 and cuenta='90'
*	Sum dsaldoac to vdsaldoac for len(allt(cuenta))=3 and cuenta='90'
*	Sum hsaldoac to vhsaldoac for len(allt(cuenta))=3 and cuenta='90'
*	Sum dsalaju  to vdsalaju  for len(allt(cuenta))=3 and cuenta='90'
*	Sum hsalaju  to vhsalaju  for len(allt(cuenta))=3 and cuenta='90'
*	Sum dsaldo   to vdsaldo   for len(allt(cuenta))=3 and cuenta='90'
*	Sum hsaldo   to vhsaldo   for len(allt(cuenta))=3 and cuenta='90'
*	Sum dpresu   to vdpresu   for len(allt(cuenta))=3 and cuenta='90'
*	Sum hpresu   to vhpresu   for len(allt(cuenta))=3 and cuenta='90'
*	
*	go vrecno1
*	replace dsaldoan with Vdsaldoan 
*	replace hsaldoan with Vhsaldoan 
*	replace dsumana  with vdsumana
*	replace hsumana  with vhsumana
*	replace dsaldoac with vdsaldoac
*	replace hsaldoac with vhsaldoac
*	replace dsalaju  with vdsalaju
*	replace hsalaju  with vhsalaju	
*	replace dsaldo   with vdsaldo
*	replace hsaldo   with vhsaldo	
*	replace dpresu   with vdpresu
*	replace hpresu   with vhpresu
* endif	

RETURN


FUNCTION Cuenta
*--------------
DO CASE
	CASE SUBSTR(Temp.Cuenta,3,8)='00000000'
	   RETURN LEFT(Temp.Cuenta,2)
	CASE SUBSTR(Temp.Cuenta,4,7)='0000000'
	   RETURN LEFT(Temp.Cuenta,3)
	CASE SUBSTR(Temp.Cuenta,6,5)='00000'
	   RETURN LEFT(Temp.Cuenta,5)
	CASE SUBSTR(Temp.Cuenta,8,3)='000'
	   RETURN LEFT(Temp.Cuenta,7)
 	OTHERWISE
	   RETURN Temp.Cuenta
ENDCASE

FUNCTION Cuenta1
*--------------
PARAMETER xC1
xAli=ALIAS()
SELE &xC1
DO CASE
	CASE SUBSTR(Cuenta,3,8)='00000000'
	   mRet = LEFT(Cuenta,2)
	CASE SUBSTR(Cuenta,4,7)='0000000'
	   mRet = LEFT(Cuenta,3)
	CASE SUBSTR(Cuenta,6,5)='00000'
	   mRet = LEFT(Cuenta,5)
	CASE SUBSTR(Cuenta,8,3)='000'
	   mRet = LEFT(Cuenta,7)
 	OTHERWISE
	   mRet = Cuenta
ENDCASE
SELE &xAli
RETURN mRet

function adicional
*-----------------
select temp
SUM DAJUANO TO DEBE FOR CUENTA<>'89' and SUBS(CUENTA,3,1)='8' AND LEN(ALLT(CUENTA))=3 AND CUENTA>='2'
SUM HAJUANO TO HABER FOR CUENTA<>'89' and SUBS(CUENTA,3,1)='8' AND LEN(ALLT(CUENTA))=3 AND CUENTA>='2'
LOCATE FOR CUENTA='898'
REPLACE DAJUANO WITH HABER
REPLACE HAJUANO WITH DEBE
GO TOP
RETURN
