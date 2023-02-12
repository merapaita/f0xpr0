** -----------------------------------
** BALCONBE.PRG
** Reporte Balance de Constructivo 
** Autor  : Ing. Virna Alicia Vel squez - UDEP 
** Inicio : 13 de Diciembre de 1995
** -----------------------------------
** Abrir Archivos de trabajo

USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Cuentas IN 2 ORDER TAG Cuentas1        ALIAS Cuen
USE SaldoNa IN 3 ORDER TAG SaldoNa1        ALIAS SalN

SELE SALN
SET RELATION TO CUENTA INTO CUEN

PUBLIC vDMovAcu,vHMovAcu,vDMovreg,vHMovreg,vDAjuste,vHAjuste,vDSalanoant,vHSalanoant,vniv,vDebe898,vHaber898,H89,D89,HAJU,DAJU,DSuma898,HSuma898
PUBLIC Vdsumana,Vhsumana,xdsaldoan,xhsaldoan,xdsaldoac,xhsaldoac,xdsaldore,xhsaldore,vdpatri,vhpatri,ZMES,var,mCta1
PUBLIC xdsalaju,xhsalaju,xdajuano,xhajuano,xdsaldo,xhsaldo,Result,SaldoD89,SaldoH89,VFECHA,FE2,acumDeb,acumHab,SIGUE,acumdar,acumhar
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
    dsumana01 n(18,4),;
    hsumana01 n(18,4),;
    dsaldoac  n(18,4),;
    Hsaldoac  n(18,4),;
    dsaldona  n(18,4),;
    Hsaldona  n(18,4),;
    dsaldona01 n(18,4),;
    Hsaldona01 n(18,4),;
	Dajumes   n(18,4),;
	Hajumes   n(18,4),;
	Dajuano   n(18,4),;
	Hajuano   n(18,4),;
    dsaldore  n(18,4),;
    Hsaldore  n(18,4),;
    dsumaux   n(18,4),;
    Hsumaux   n(18,4),;
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

  DEFINE WINDOW BalComp FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Balance Hist¢rico  ' COLOR SCHEME 5
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
     DO ESPERA WITH 1,'Reporte del Balance Hist¢rico en Proceso ....!'
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

SELEC SALN
COPY TO aa field CUENTA, MES, ANO, DSUMANA,HSUMANA, DSALDONA, HSALDONA,;
DSUMANA01,HSUMANA01, DSALDONA01, HSALDONA01,SALN.DAJUMES, SALN.HAJUMES,;
SALN.DAJUANO, SALN.HAJUANO, CUEN.DESCRI, SALN.DSUMAJU, SALN.HSUMAJU,SALN.DSaldore, SALN.Hsaldore,SALN.DSumaux, SALN.Hsumaux;
FOR (mes<=vmes AND ano=vano)
USE AA IN 0 EXCLUSIVE
SELE aa
INDEX ON cuenta+STR(mes,2)+STR(ano,2) tag AA 
REPLACE ALL DSUMANA WITH  DSUMANA -DSUMANA01 , HSUMANA  WITH HSUMANA-HSUMANA01 ,;
 			DSALDONA WITH DSALDONA-DSALDONA01, HSALDONA WITH HSALDONA-HSALDONA01 ;
 			FOR DSUMANA01>0 OR HSUMANA01>0
GO TOP
DO NU1
RETURN

PROCEDURE NU1
*------------
SELE AA
xsalant = .F.
SCAN
	SIGUE       = .T.
 	vCuenta     = AA.Cuenta
 	varCuen     = AA.Cuenta
 	
	mCta1=Cuenta1('aa')

	SELE AA

	IF MES=0
		vDsaldoant  = SaldoAntD(vcuenta) && Saldos del mes anterior
 		vHsaldoant  = SaldoAntH(vcuenta)
 		xsalant     = .T.
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
	vdajuste=0
	vHajuste=0
	
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
    
	IF xsalant
		REPLACE dsaldoan WITH  vdsaldoant
		REPLACE Hsaldoan WITH  vhsaldoant
 		xsalant     = .F.
	ENDIF
	
	*- Movimientos del mes
	REPLACE dsumana  WITH acumDeb
   	REPLACE hsumana  WITH acumHab

	*- Saldos actuales del mes
   	REPLACE dsaldoac WITH IIF(DSUMANA>HSUMANA,DSUMANA-HSUMANA,0)
   	REPLACE Hsaldoac WITH IIF(HSUMANA>DSUMANA,HSUMANA-DSUMANA,0)


	*- Es necesario crear un campo mas a saldona, donde se guarden estos montos
	*- ajustes y regularizaciones. se ingresaran por nota contable
	
	REPLACE dsaldore  WITH acumDAR
	REPLACE Hsaldore  WITH acumHAR

   	*_ Saldos Ajustados
	VAR3 = (dsaldoac+dsaldore)-(hsaldoac+hsaldore)
	IF var3>0
	   REPLACE dsalaju  WITH var3
	else
		REPLACE Hsalaju  WITH var3*-1
	endif

	*- Saldos ajustados del mes. ACM.

	IF SUBST(CUENTA,3,1)='8' AND CUENTA>='2'
		REPLACE dajuano WITH vdajuste
		REPLACE Hajuano WITH vhajuste
    ELSE
   		REPLACE dajuano WITH 0
   		REPLACE Hajuano WITH 0
	ENDIF
   	

	*- Saldos actualizados incluyendo los ajustes
	
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
*DO AsienRef

SELE Temp
GO TOP
SCAN
 	VarCuen     = QuitaCero(Cuenta)	
 	REPLACE Cuenta WITH varCuen
ENDSCAN

*-
SUM dpresu   TO vDpresu FOR LEN(ALLTR(Cuenta))=3
SUM hPresu   TO vhpresu FOR LEN(ALLTR(Cuenta))=3
*-----------------------------
* 
SUM dsaldoan TO xdsaldoan FOR LEN(ALLTR(Cuenta))=2
SUM hsaldoan TO xhsaldoan FOR LEN(ALLTR(Cuenta))=2
SUM dsumana  TO xdsumana  FOR LEN(ALLTR(Cuenta))=2
SUM hsumana  TO xhsumana  FOR LEN(ALLTR(Cuenta))=2

SUM dsaldoac TO xdsaldoac FOR LEN(ALLTR(Cuenta))=2   
SUM hsaldoac TO xhsaldoac FOR LEN(ALLTR(Cuenta))=2   
SUM dsaldo   TO xdsaldo   FOR LEN(ALLTR(Cuenta))=2   
SUM hsaldo   TO xhsaldo   FOR LEN(ALLTR(Cuenta))=2  
SUM dsalaju  TO xdsalaju  FOR LEN(ALLTR(Cuenta))=2   
SUM hsalaju  TO xhsalaju  FOR LEN(ALLTR(Cuenta))=2   
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
	ENDIF
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
DO adicional
DO reasal  && actualiza saldos del ajuste
GO TOP
* edgar
SUM dsaldoan TO xdsaldoan FOR LEN(ALLTR(Cuenta))=2
SUM hsaldoan TO xhsaldoan FOR LEN(ALLTR(Cuenta))=2
SUM dsumana  TO xdsumana  FOR LEN(ALLTR(Cuenta))=2
SUM hsumana  TO xhsumana  FOR LEN(ALLTR(Cuenta))=2

SUM dsaldoac TO xdsaldoac FOR LEN(ALLTR(Cuenta))=2   and left(cuenta,1)<'9'
SUM hsaldoac TO xhsaldoac FOR LEN(ALLTR(Cuenta))=2   and left(cuenta,1)<'9'

SUM dPatri   TO vDpatri FOR LEN(ALLTR(Cuenta))=2
SUM hPatri   TO vhpatri FOR LEN(ALLTR(Cuenta))=2

SUM dGesRe   TO vDGesRe FOR LEN(ALLTR(Cuenta))=2
SUM hGesRe   TO vhGesRe FOR LEN(ALLTR(Cuenta))=2

* Esta variable guarda el resultado de la cta 89, 
Result = vHgesre-vDgesre

SUM ROUND(dajuano,2)  TO xdajuano  FOR LEN(ALLTR(Cuenta))=2
SUM ROUND(hajuano,2)  TO xhajuano  FOR LEN(ALLTR(Cuenta))=2
SUM dsaldo   TO xdsaldo   FOR LEN(ALLTR(Cuenta))=2  and left(cuenta,1)<>'9'
SUM hsaldo   TO xhsaldo   FOR LEN(ALLTR(Cuenta))=2  and left(cuenta,1)<>'9'
SUM dsaldo   TO xdsaldo1  FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
SUM hsaldo   TO xhsaldo1  FOR LEN(ALLTR(Cuenta))=3 and left(cuenta,1)='9'
SUM dsalaju  TO xdsalaju  FOR LEN(ALLTR(Cuenta))=2  and left(cuenta,1)<>'9' 
SUM hsalaju  TO xhsalaju  FOR LEN(ALLTR(Cuenta))=2  and left(cuenta,1)<>'9' 
*-----
xdsaldoac2 = xdsaldoac + Xdsaldoac1
xhsaldoac2 = xhsaldoac + Xhsaldoac1
xdsaldo2   = xdsaldo + xdsaldo1
xhsaldo2   = xhsaldo + xhsaldo1
xdsalaju2  = xdsalaju + xdsalaju1
xhsalaju2  = xhsalaju + xhsalaju1
SUM dsaldore TO xdsaldore FOR LEN(ALLTR(Cuenta))=2
SUM hsaldore TO xhsaldore FOR LEN(ALLTR(Cuenta))=2

IF vdigito=1
	SET FILTE TO IIF(CUENTA#'90',LEN(ALLT(CUENTA))=2,.T.)
ENDIF
DO REPORTE WITH 2, "BalCons2", ' Reporte Balance Constructivo SIN A.C.M.',2,.F.,.T.
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
fMes=0
fAno=vano
SELE SALN
SEEK vcuenta+STR(fmes,2)+STR(fano,2)
IF FOUND()
	vSaldoAD = DSumaNa
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
fMes=0
fAno=vano
SELE SALN
SEEK vcuenta+STR(fmes,2)+STR(fano,2)
IF FOUND()
	vSaldoAh = HSumaNa
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
STORE 0 TO acumDeb,acumHab,acumDAR,acumHAR
DO WHILE vcuenta1 = vcuenta2 AND !EOF()
	   acumDeb = acumDeb + dsumana
	   acumHab = acumHab + Hsumana
	   && ACUMULA SALDOS DE AJUSTE DE REGULARIZACION
	   acumDAR = acumDAR + (dsaldore-dsumaux)
	   acumHAR = acumHAR + (Hsaldore-Hsumaux)
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

FUNCTION NIVEL
PARAMETER xCT
PRIVATE xAlias
xAlias=ALIAS()
SELE Cuen
SEEK xCT
SELE (xAlias)
RETURN Cuen.nivel

PROCEDURE REASAL
*-----------------
* PROCESO PARA REASIGNAR SALDOS 
SELE TEMP
set filter to cuenta>='2' and len(allt(cuenta))<=3
go top
scan
	vcuenta = cuenta
	vrecnoi = recno()
	STORE 0 TO VDAJUANO,VHAJUANO,VDSALDORE,VHSALDORE,VDSumana,VHSumana
	STORE 0 TO VDsaldona,VHsaldona
	skip
	IF allt(vcuenta) = LEFT(cuenta,2)
		DO WHILE allt(vcuenta) = LEFT(cuenta,2)
			VDAJUANO  = VDAJUANO  + DAJUANO
			VHAJUANO  = VHAJUANO  + HAJUANO
			VDSALDORE  = VDSALDORE  + DSALDORE
			VHSALDORE  = VHSALDORE  + HSALDORE
			VDSumana   = VdSumana  + dSumana
			VHSumana   = VHSumana  + HSumana
			VDSaldona  = VdSaldona  + dSaldona
			VHSaldona  = VHSaldona  + HSaldona
			skip
		enddo	
		IF !EOF()
			vrecnof = recno()	
		ELSE
			EXIT
		ENDIF
		go vrecnoi
		
		REPLACE dsumana WITH vdsumana , dsaldona WITH vdsaldona
		REPLACE hsumana WITH vhsumana , hsaldona WITH vhsaldona
   		REPLACE dsaldoac WITH IIF(vDSUMANA>vHSUMANA,vDSUMANA-vHSUMANA,0)
   		REPLACE Hsaldoac WITH IIF(vHSUMANA>vDSUMANA,vHSUMANA-vDSUMANA,0)
		VAR0 = VDAJUANO - VHAJUANO 
		VAR1 = VDSALDORE - VHSALDORE 
		IF var0>0
			REPLACE DAJUANO WITH var0
			REPLACE HAJUANO WITH 0
	 	ELSE
			REPLACE HAJUANO WITH ABS(var0)
	   		REPLACE DAJUANO WITH 0
		ENDIF
		IF var1>0
			REPLACE DSALDORE WITH var1
			REPLACE HSALDORE WITH 0
	 	ELSE
			REPLACE HSALDORE WITH ABS(var1)
	   		REPLACE DSALDORE WITH 0
		ENDIF
		VAR3 = (dsaldoac+dsaldore)-(hsaldoac+hsaldore)
		IF var3>0
		   REPLACE dsalaju  WITH var3
		else
			REPLACE Hsalaju  WITH var3*-1
		endif
		VAR2 = (DSALAJU+DAJUANO)-(HSALAJU+HAJUANO)
		IF var2>0
			REPLACE dsaldo WITH var2
			REPLACE Hsaldo WITH 0
	 	ELSE
			REPLACE Hsaldo WITH ABS(var2)
	   		REPLACE Dsaldo WITH 0
		ENDIF
	
		*- Saldos de las cuentas Patrimoniales
		
		REPLACE dpatri WITH (IIF(Cuenta<'6',DSALDO,0))
		REPLACE hpatri WITH (IIF(Cuenta<'6',HSALDO,0))

		*- Saldos de las cuentas Presupuestales
		REPLACE dpresu WITH (IIF(Cuenta>='9',DSALDO,0))
		REPLACE hpresu WITH (IIF(Cuenta>='9',HSALDO,0))

		*- Saldos de las cuentas de Gestion y Resultado
		REPLACE dgesre WITH (IIF(Cuenta>'5' and cuenta<'9',DSALDO,0))
		REPLACE hgesre WITH (IIF(Cuenta>'5' and cuenta<'9',HSALDO,0))

		IF !EOF()
			go vrecnof
		ENDIF	
		skip -1
	endif
endscan
SET FILTER TO
GO TOP
RETURN
