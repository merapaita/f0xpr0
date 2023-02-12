** ---------------------------------------------------
** LIBDIR.PRG
** Reporte Resumen del Libro Diario (Mensual o Anual)
** AUTOR: UDEP - 1996. Virna ALicia Velasquez Encalada.
** ---------------------------------------------------

* Abrir Archivos de trabajo

SET ESCAPE OFF
USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Diario  IN 2 ORDER TAG Diario11        ALIAS Diar
USE Cuentas IN 4 ORDER TAG Cuentas1        ALIAS Cuen
USE Compro  IN 5 ORDER TAG Compro7         ALIAS Comp EXCLUSIVE
USE Glosas  IN 6 ORDER TAG Glosas1          ALIAS Glos
USE Folios  IN 10 ORDER TAG Folios1         ALIAS Folios

PUBLIC vQui,vDMonto,vHMonto,vKey,vDTot,vHTot,vDSal,vHSal,vMonDeb,vMonHab,valor,Debe,haber,vMto,vDebe101,vHaber101
PUBLIC _y,_z,wglosa,VDMONTO,VHMONTO,vDvan,vHvan
wglosa=SPACE(130)

STORE 0 TO xano,xMes,vdebe,vhaber,vQui,vMonDeb,vMonHab,valor,vDvan,vHvan
STORE 0 TO vDMonto,vHMonto,vDTot,vHTot,vDSal,vHSal,vMto,vDebe101,vHaber101
STORE .T. TO NoPaso, NoPaso1


DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  DEFINE WINDOW LibDir FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE '  Listado de Libro Diario Resumen  ' COLOR SCHEME 5
  ACTIVATE WINDOW LibDir
  
  xbalance = 'Mensual'
  xdigitos = 2
  vFecha   = DATE()
  zmes='  '
  zano='  '

  
  SELECT parma
  SET ORDER TO Parmae1
  SEEK 'MESANOACTUA'
  vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
  xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
  xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
  vajuste  = 'Si'
  * @ 0, 0 SAY ' Genera Diario:' + xBalance
  
  @ 1, 01 SAY '         Mes y A¤o : ' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
             VALID xMes > 0 .AND. xMes <= 12
  @ 1, 26 GET xAno                           PICTURE '99'
  * @ 3, 01 SAY '             Fecha : ' GET vFecha   PICTURE '@E'
*  @ 5, 01 SAY '          Ajustado : ' GET vajuste  PICTURE '@M Si,No'
  @ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
  READ
  
  DEACTIVATE WINDOW LibDir

  zmes=PADL(ALLTRIM(STR(xmes,2)),2,'0')
  zano=PADL(ALLTRIM(STR(xano,2)),2,'0')

  IF xmes=12
	vajuste  = 'Si'
   ELSE
	vajuste  = 'No'
  ENDIF
  
  IF LASTKEY() # 27 AND OKCANCEL = 1
     DO ESPERA WITH 1,PADC(' Diario Resumen en Proceso ',50)
	vind4  = SYS(3) +'.idx'
    vind1  = SYS(3) +'.idx'
	vind5  = SYS(3) +'.idx'
    vfil1  = SYS(3) +'.dbf'
    vfil2  = SYS(3) +'.dbf'
    vind   = SYS(3) +'.idx'
	vindX  = SYS(3) + '.DBF'
	vIndi2 = SYS(3) + ".IDX"
    vindY  = SYS(3) + ".DBF"
    vindZ  = SYS(3) + ".DBF"
    vindV  = SYS(3) + ".DBF"
    xruta=SET('PATH')
	vindW  = SYS(3) + '.DBF'
	vindT  = SYS(3) + '.DBF'
	vindU  = SYS(3) + '.DBF'
	vindO  = SYS(3) + '.DBF'
	vindP  = SYS(3) + '.DBF'
	vindC  = SYS(3) + '.DBF'
	vindQ  = SYS(3) + '.DBF'
	vVqui  = SYS(3) + '.DBF'
	vindF  = SYS(3) + '.DBF'
	vindG  = SYS(3) + '.DBF'
	vindM  = SYS(3) + '.DBF'
	vindS  = SYS(3) + '.DBF'
	VINDJ =  SYS(3) + '.DBF'	
	
SELECT 25
CREATE TABLE (vVqui);
    (comprob   c(010),;
	 Asiento   c(03))
USE

USE (vVqui) IN 25 ALIAS Asiento EXCLUSIVE
    DO CASE
      CASE xbalance = 'Mensual'
        IF xMes >= MONTH(vmesact) .AND. xAno = VAL(SUBSTR(STR(YEAR(vmesact),4),3,2))
          SELECT Diar
          SET RELATION TO Comprob+ComAdi+codfte INTO Comp          
          DO RepDiar
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
          !FOXSWAP PKUNZIP -o &vbackup Parmae.dbf SaldoNa.dbf Diario.dbf Compro.dbf CompDo.dbf Origen.dbf Cuentas.dbf >NUL
          !FOXSWAP PKUNZIP -o &vbackup Parmae.cdx SaldoNa.cdx Diario.cdx Compro.cdx CompDo.cdx Origen.cdx Cuentas.cdx >NUL
          !CD ..\FUENTES
          CLOSE DATA
          SET PATH TO &vpath
          USE Parmae  IN 1 ORDER TAG Parmae2     ALIAS parma
          USE Diario  IN 2 ORDER TAG Diario11    ALIAS Diar
          USE Cuentas IN 4 ORDER TAG Cuentas1    ALIAS Cuen
          USE Compro  IN 5 ORDER TAG Compro7     ALIAS Comp
          USE Folios  IN 10 ORDER TAG Folios1         ALIAS Folios
          SELECT Diar
          SET RELATION TO Diar.Cuenta  INTO Cuen
          SET RELATION TO Diar.Comprob+Diar.ComAdi+Diar.codfte INTO Comp ADDITIVE
          * INDEX ON SUBSTR(Comprob,1,2) + Cuenta  TO (vind1) FOR MONTH(Comp.Fecha) = xMes;
          * .AND. VAL(SUBSTR(STR(YEAR(Comp.Fecha),4),3,2)) = xAno
          DO RepDiar
          CLOSE DATA
          SET PATH TO &xruta
          *vdbf = vdir + '*.dbf'
          *vidx = vdir + '*.idx'
          *vcdx = vdir + '*.cdx'
          *!ERASE &vdbf >NUL
          *!ERASE &vidx >NUL
          *!ERASE &vcdx >NUL
        ENDIF
    ENDCASE
    CLOSE DATABASE
    ERASE (vind)
    ERASE (vind1)
    ERASE (vfil1)
    ERASE (vfil2)

  ENDIF
  DEACTIVATE WINDOW LibDir

  IF !escolor
    RESTORE SCREEN FROM principal
  ENDIF
  SHOW POPUP menu, pop_03
RETURN


PROCEDURE Salida
*---------------
RELEASE    WINDOW LibDir
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)


PROCEDURE RepDiar
*----------------
SELE Parma
SET ORDER TO Parmae2
SELE Diar
SUM Montos TO vDebe101  FOR CUENTA='101'  AND Tipo='D' AND Comprob<>'00';
AND MONTH(FECEMI)=XMES
SUM Montos TO vHaber101 FOR CUENTA='101'  AND Tipo='H' AND Comprob<>'00';
AND MONTH(FECEMI)=XMES
vIndi1=SYS(3)+".IDX"
INDEX ON LEFT(COMPROB,2)+CUENTA+CODFTE TO (vInd1) FOR IIF(ALLTRIM(vajuste)='Si',IIF(LEFT(Comprob,2)#'64',MONTH(Fecemi)=xMes,MONTH(Fecemi)<=xMes),MONTH(Fecemi)=xMes);
 .AND. (YEAR(Fecemi)-1900) = xAno  AND Comprob<>'05'

COPY STRU TO Temp
COPY STRU TO Tempo3
COPY STRU TO (vindz)
COPY STRU TO (vindW)
COPY STRU TO (vindM)

USE AUXDIAR IN 7 ALIAS Temp EXCLUSIVE
SELE TEMP
ZAP

*-----------
DO AsiPre
SELE TEMP
IF FILE((VINDV))
	APPEND FROM (vindv)
ENDIF
IF FILE((VINDV))
	SELE Presu
	USE 
	ERASE (VINDV)
ENDIF

*DO Certificado
DO AsiObra1
*DO AsiObra2 
DO AsiPla
DO AsiCts 
DO Cruce  && Se graba a la base temporal las cuentas con sus saldos
DO AsieCent
DO NotConRet
DO CajaChica
*SELE Temp1

IF FILE((VINDM))
	SELE TempO5
	DELE FOR Cuenta='101'
	PACK
ENDIF

	
SELE Temp

IF FILE(('Temp1.DBF'))
	APPEND FROM Temp1
ENDIF


IF FILE((VINDM))
	APPEND FROM (vindM)
	SELE TEMPO5
	USE
	ERASE (vindM)
ENDIF

SELE Temp
IF FILE((VINDS))
	APPEND FROM (vindS)
	SELE Caja
	USE
	ERASE (vindS)
ENDIF

SELE Temp
IF FILE((VINDO))
	APPEND FROM (vindO)
	IF USED("obra")
		SELE Obra
		USE
		ERASE (vindO)
	ENDIF	
ENDIF

SELE Temp
IF FILE((VINDJ))
	APPEND FROM (vindJ)
	SELE CERTF
	USE
	ERASE (vindJ)
ENDIF

SELE Temp
IF FILE((VINDP))
	APPEND FROM (vindP)
	SELE Planilla
	USE
	ERASE (vindp)
ENDIF

SELE Temp
IF FILE((VINDc))
	APPEND FROM (vindc)
	SELE Cts
	USE
	ERASE (vindc)
ENDIF

SELE Temp
IF FILE((VINDQ))
	APPEND FROM (vindQ)
	SELE Mano
	USE
	ERASE (vindQ)
ENDIF

*-------------------
USE Folios  IN 10 ORDER TAG Folios1         ALIAS Folios

SELE TEMP

REPLACE ALL COMPROB WITH 'SS' FOR Comprob='64'

INDEX ON LEFT(COMPROB,2)+TIPO+CUENTA TO (vind5)
xDia = CTOD('31/12/'+ALLTRIM(STR(XANO)))
REPLACE FecEmi WITH xDia FOR Comprob='SS'
GO TOP
IF EOF()
  DO STANDBY WITH "No hay registros para procesar"
ELSE
	if vajuste='No'
		SET FILTER TO COMPROB<>'SS'
	ENDIF
	SUM ROUND(MONTOS,2) TO DEBE  FOR TIPO='D' AND MONTH(FECEMI)=XMES
	SUM ROUND(MONTOS,2) TO HABER FOR TIPO='H' AND MONTH(FECEMI)=XMES
	DEBE  = DEBE  + vDebe101
	HABER = (HABER+ vHaber101)*(-1)

	GO TOP
	DO ESPERA WITH 2
	INDEX ON ORDEN+TIPO+CUENTA TO ORDEN
	
	GO TOP
	DO REPORTE WITH 2, "LibDir2", 'Resumen del Libro Diario Mensual',1,.F.,.T.
	if vajuste='No'
		SET FILTER TO 
	ENDIF	
	
	*	ENDIF	
	*ENDIF	
ENDIF
DO GrabaAsi
CLOSE DATA
!DEL *.IDX
RETURN


FUNCTION leeglo
*--------------
PARAMETER wano,wdoc
SELE glos
SEEK (wano+wdoc)
IF FOUND()
   STORE Glos.Glosa TO wglosa 
ENDIF
RETURN wglosa




FUNCTION Fin
*-----------
IF !EOF()
  SKIP
  IF EOF()
    SKIP - 1
    RETURN .T.
  ENDIF
  SKIP - 1
  RETURN .F.
ENDIF
RETURN .T.


FUNCTION DSUMA
*--------------
vRec=RECNO()
vkey = substr(Cuenta,1,2)
vkey1= LEFT(comprob,2)
STORE 0 TO vMontoD
SCAN WHILE SUBSTR(Cuenta,1,2) = vKey  and LEFT(comprob,2)=vkey1
	IF Tipo ='D'
        vMontoD = vMontoD + Montos
	ENDIF
ENDSCAN
GO vRec
RETURN vMontoD


FUNCTION HSUMA
*---------------
PARAMETER vkey,VCOMP
vkey = substr(Cuenta,1,2)
vComp= LEFT(comprob,2)
vRec=RECNO()
STORE 0 TO vMontoH
SCAN WHILE LEFT(COMPROB,2)=VCOMP AND SUBSTR(Cuenta,1,2) = vKey 
     IF Tipo='H'
        vMontoH = vMontoH + Montos
     ENDIF
ENDSCAN
GO vRec
RETURN vMontoH

FUNCTION DSUMA1
*--------------
vRec=RECNO()
DO CASE
	CASE LEFT(CUENTA,1)='0'
		vkey = lEFT(Cuenta,4)
		vComp= LEFT(comprob,2)
	OTHERWISE
		vkey = LEFT(Cuenta,3)
		vComp= LEFT(comprob,2)
ENDCASE
STORE 0 TO vMontoD
SCAN WHILE IIF(LEFT(CUENTA,1)='0',LEFT(Cuenta,4),LEFT(Cuenta,3)) = vKey  and LEFT(comprob,2)=vCOMP
	IF Tipo ='D'
        vMontoD = vMontoD + Montos
	ENDIF
ENDSCAN
GO vRec
RETURN vMontoD


FUNCTION HSUMA1
*---------------
PARAMETER vkey,VCOMP
DO CASE
	CASE LEFT(CUENTA,1)='0'
		vkey = lEFT(Cuenta,4)
		vComp= LEFT(comprob,2)
	OTHERWISE
		vkey = LEFT(Cuenta,3)
		vComp= LEFT(comprob,2)
ENDCASE
vRec=RECNO()
STORE 0 TO vMontoH
SCAN WHILE LEFT(COMPROB,2)=VCOMP AND IIF(LEFT(CUENTA,1)='0',LEFT(Cuenta,4),LEFT(Cuenta,3)) = vKey 
     IF Tipo='H'
        vMontoH = vMontoH + Montos
     ENDIF
ENDSCAN
GO vRec
RETURN vMontoH


FUNCTION DTot
*------------
vDSal=vDTot+vDebe
RETURN vDSal


FUNCTION HTot
*------------
vHSal=vHTot+vHaber
RETURN vHSal


FUNCTION QUI
*-----------
vAli = ALIAS()
vOrd = ORDER()
vQUI = vQUI + 1
vComp = Comprob
SELE Asiento
APPEND BLANK
REPLACE Comprob WITH vComp
REPLACE Asiento WITH STR(vQui,3)
SELE (vAli)
SET ORDE TO (vOrd)
RETURN vQUI   

PROCEDURE GrabaAsi
*-------------------
SELE Asiento
GO TOP
SCAN
	vComp = LEFT(Comprob,2)
	vAsie = Asiento
	SELE Temp
	REPLACE ALL File WITH vAsie FOR LEFT(Comprob,2)=vComp
	SELE Asiento
ENDSCAN
RETURN

FUNCTION DESCRI
*--------------
vAli=ALIAS()
IF XANO>96 
	xCuenta=LEFT(Cuenta,2)
	vCuenta=LEFT(Cuenta,2)+'00000000'
ELSE
	IF XMES>=4
		IF CUENTA='9'
			xCuenta=LEFT(Cuenta,3)
			vCuenta=LEFT(Cuenta,3)+'0000000'
		ELSE
			xCuenta=LEFT(Cuenta,2)
			vCuenta=LEFT(Cuenta,2)+'00000000'
		ENDIF
	ELSE	
		xCuenta=LEFT(Cuenta,2)
		vCuenta=LEFT(Cuenta,2)+'00000000'
	ENDIF	
ENDIF	
SELECT Cuen
SEEK vCuenta
IF FOUND()
   =ZZ()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN


FUNCTION ZZ
*----------
RETURN xCuenta


FUNCTION DESCRI1
*---------------
vAli=ALIAS()
DO CASE
	CASE LEFT(CUENTA,1)='0'
			xCuenta=LEFT(Cuenta,4)
			vCuenta=LEFT(Cuenta,4)+'000000'
	OTHERWISE
		IF SUBSTR(CUENTA,4,1)='O'
			xCuenta=LEFT(Cuenta,3)
			vCuenta=LEFT(Cuenta,3)+'O000000'
		ELSE
			IF SUBSTR(CUENTA,5,1)='O'
				xCuenta=LEFT(Cuenta,3)
				vCuenta=LEFT(Cuenta,4)+'O00000'
			ELSE
				xCuenta=LEFT(Cuenta,3)
				vCuenta=LEFT(Cuenta,3)+'0000000'
			ENDIF
		ENDIF
ENDCASE
SELECT Cuen
SEEK vCuenta
IF FOUND()
   =YY()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN

FUNCTION DESCRI2
*---------------
vAli=ALIAS()
DO CASE
	CASE LEFT(CUENTA,1)='0'
		xCuenta=LEFT(Cuenta,4)
		vCuenta=LEFT(Cuenta,4)+'000000'
	OTHERWISE
		xCuenta=LEFT(Cuenta,5)
		vCuenta=LEFT(Cuenta,5)+'00000'
ENDCASE
SELECT Cuen
SEEK vCuenta
IF FOUND()
   =YY()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN

FUNCTION YY
*----------
RETURN xCuenta


FUNCTION vDebe
*-------------
SELECT Temp
GO TOP
SCAN
    IF Diar.Tipo='D' AND RIGHT(Cuenta,8)='00000000'
       vMonDeb = vMonDeb + Diar.Montos
    ELSE
		IF Diar.Tipo='H' AND RIGHT(Cuenta,8)='00000000'
	       vMonHab = vMonHab + Diar.Montos
		ENDIF
    ENDIF
ENDSCAN
=vHaber()
RETURN vMonDeb


FUNCTION vHaber
*--------------
RETURN vMonHab


FUNCTION Mto
*----------
PARAMETER vCuenta
SUM Montos TO Debe FOR LEFT(Cuenta,3)=vCuenta
SUM Montos TO Haber FOR LEFT(Cuenta,3)=vCuenta
IF vCuenta<'40'
	Valor = Debe - Haber
ELSE
	Valor = Haber - Debe
ENDIF
RETURN Valor


FUNCTION SaldoD
*---------------
PARAMETER vCuenta,vComp1
STORE 0 TO vDMonto
SCAN WHILE (LEFT(Comprob,2)=vComp1) AND IIF(LEN(vCuenta)=3,(SUBSTR(Cuenta,1,3) = vCUENTA),(SUBSTR(Cuenta,1,2) = vCUENTA))
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ENDIF
ENDSCAN
RETURN 



FUNCTION SaldoH
*---------------
PARAMETER vCuenta,vComp1
PRIVATE vHmonto
STORE 0 TO vHmonto
SCAN WHILE (LEFT(Comprob,2)=vComp1) AND IIF(LEN(vCuenta)=3,(SUBSTR(Cuenta,1,3) = vCUENTA),(SUBSTR(Cuenta,1,2) = vCUENTA))
		IF Tipo = 'H'
	       	vhMonto = vhMonto + Montos
		ENDIF
ENDSCAN
RETURN vhmonto


FUNCTION Saldo
*---------------
PARAMETER vCuenta,vComp1
PRIVATE vDMonto,vHmonto,vSaldo
STORE 0 TO vDMonto,vHmonto
SCAN WHILE (LEFT(Comprob,2)=vComp1) AND IIF(LEN(vCuenta)=3,(SUBSTR(Cuenta,1,3) = vCUENTA),(SUBSTR(Cuenta,1,2) = vCUENTA))
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
			vHMonto = vHMonto + Montos
		ENDIF
ENDSCAN
vSaldo = (vDmonto - vHMonto)
RETURN vSaldo


FUNCTION Saldo2
*---------------
PARAMETER vCuenta,vComp1
PRIVATE vDMonto,vHmonto,vSaldo
STORE 0 TO vDMonto,vHmonto
SCAN WHILE (ALLTRIM(Comprob)=vComp1) AND IIF(LEN(vCuenta)=3,(SUBSTR(Cuenta,1,3) = vCUENTA),(SUBSTR(Cuenta,1,2) = vCUENTA))
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
			vHMonto = vHMonto + Montos
		ENDIF
ENDSCAN
vSaldo = (vDmonto - vHMonto)
RETURN vSaldo


PROCEDURE CRUCE
*--------------
Graba = .T.
SELE Diar
SET FILTER TO Cuenta<>'9' AND tipdoc<>'PLA' AND LEFT(CodPart,2)<>'65' ;
AND TIPOBR<>'N' AND CAJCHI<>'S' AND IIF(ALLTRIM(vajuste)='Si',IIF(LEFT(Comprob,2)#'64',MONTH(Fecemi)=xMes,MONTH(Fecemi)<=xMes),MONTH(Fecemi)=xMes)
GO TOP
SCAN
	SCATTER MEMVAR
	DO CASE
		CASE LEFT(Diar.Cuenta,1)='0'
			vCuen    = LEFT(Diar.Cuenta,4)
			vComprob = LEFT(Diar.Comprob,2)
		OTHERWISE
			vCuen    = LEFT(Diar.Cuenta,5)
			vComprob = LEFT(Diar.Comprob,2)
	ENDCASE
	
	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(LEFT(Cuenta,5) = vCUEN),(LEFT(Cuenta,4) = vCUEN))
	  IF LEFT(Diar.Comprob,2)$'18 73' and Diar.CodDep="113000"
	     * No debe de pasar por ser  del Asicent
	  ELSE
	    IF LEFT(Diar.Comprob,2)$'18 73'
	       m.Coddep=Diar.Coddep
	    ENDIF
	       
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
		   	vHMonto = vHMonto + Montos
		ENDIF
	ENDIF	
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------
	
	SELE Temp
		IF m.Coddep='113000' AND m.Cuenta<>'9' AND m.Comprob<>'99' AND m.Comprob<>'06'
		&& No los graba, porque Aparecen en el Asiento de Centralizacion.
    		Graba = .F.
		ELSE
			if m.tipdoc='RD' and m.codanal='01.15'
				Graba = .F.
			else
				Graba = .T.
			endif	
		ENDIF           
		
		IF Graba 
			IF  vMontoD #0 
				APPEND BLANK
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF  vMontoH # 0
				APPEND BLANK
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		ENDIF
	SELE DIAR
	SKIP-1
ENDSCAN
SELE Diar
SET FILTER TO
RETURN


PROCEDURE AsieCent && Genera el asiento de Centralizacion
*-----------------
Graba = .T.
SELE Diar

*-Esta base temporal guarda solo el asiento de centralizacion
COPY STRU TO Temp1 
USE Temp1 IN 8 ALIAS Temp1 EXCLUSIVE
*-
SELE Diar

COPY TO &vindx FOR  (Coddep='113000') AND (Cuenta<>'9') AND Comprob<>'99' AND Comprob<>'76' AND LEFT(Comprob,1)<>'N' AND LEFT(Comprob,1)<>'Z' AND Comprob<>'06' AND Comprob<>'18' AND Comprob<>'11' AND Comprob<>'68' AND Comprob<>'00';
AND Comprob<>'98' AND Cuenta<>'0' AND TIPOBR<>'N' AND CAJCHI<>'S'  AND MONTH(FECEMI)=XMES

* No se considera los c/p porque se les hace un proceso aparte sin retenciones.

USE &vindx IN 9 ALIAS Temp3 EXCLUSIVE

SELE Temp3
*-------------
DO QuitaReten
*-------------

SELE Temp3
APPEND FROM (vindg)

* Ahora se graban con diferente Comprob  para diferenciar los ingresos y los egresos de caja.
* Se crea esta base para no modificar diario.

GO TOP
SCAN
	IF Temp3.tipo='H'
		IF Temp3.Cuenta='101' 
			REPLACE COMPROB WITH 'RR'+LEFT(Comprob,2)
		ELSE
		 	REPLACE COMPROB WITH 'GG'+LEFT(Comprob,2)
		ENDIF
	ELSE
 		 IF Temp3.Cuenta='101' 
		 	REPLACE COMPROB WITH 'GG'+LEFT(Comprob,2)
		 ELSE
			REPLACE COMPROB WITH 'RR'+LEFT(Comprob,2)
		 ENDIF
	ENDIF				 				 				 
ENDSCAN

* Ahora se graba a temp1 
SELE Temp3

INDEX ON LEFT(COMPROB,2)+CUENTA+CODFTE TO (vIndi2) FOR MONTH(Fecemi) = xMes;
 .AND. (YEAR(Fecemi)-1900) = xAno   AND Reten<>'S'
GO TOP
SCAN
	SCATTER MEMVAR
	vCuen    = LEFT(Temp3.Cuenta,5)
	vComprob = LEFT(Temp3.Comprob,2)
	*vMonto   = Saldo1(vCuen,Vcomprob)

	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
		   	vHMonto = vHMonto + Montos
		ENDIF
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------

	SELE Temp1
		IF Graba  
			IF  vMontoD >0 
				APPEND BLANK
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF  vMontoH >0 
				APPEND BLANK			
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		ENDIF
	SELE Temp3
	SKIP-1
ENDSCAN
*SELE Temp3
SELE TEMP1
*---------------------
DELE FOR CUENTA='101'
PACK
*--------------------
GO TOP
RETURN


FUNCTION Saldo1 
*--------------
PARAMETER vCuenta,vComp1
PRIVATE vDMonto,vHmonto,vSaldo
STORE 0 TO vDMonto,vHmonto
SCAN WHILE (LEFT(Temp3.Comprob,2)=vComp1) AND IIF(LEN(vCuenta)=5,(SUBSTR(Temp3.Cuenta,1,5) = vCUENTA),(SUBSTR(Temp3.Cuenta,1,2) = vCUENTA))
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Temp3.Montos
		ELSE
			vHMonto = vHMonto + Temp3.Montos
		ENDIF
ENDSCAN
vSaldo = (vDmonto - vHMonto)
RETURN vSaldo


*QUITA EL MONTO DE LA RETENCION A LOS C/P QUE TIENEN RETENCION
PROCEDURE QuitaReten
*-------------------
* Se quita al monto mayor del c/p la suma  de las retenciones
SELE Diar
COPY STRU TO (vindF)
COPY STRU TO (vindG)
USE &vindf IN 12 ALIAS CP  EXCLUSIVE
USE &vindg IN 13 ALIAS CP1 EXCLUSIVE

* Se graban los datos a la temporal para no modificar diario
SELE Diar
*SET FILTER TO Comprob='98' AND CUENTA<>'101' AND CUENTA<>'9';
AND MONTH(FECEMI)=XMES

SET FILTER TO Comprob='98' AND CUENTA<>'101' AND CUENTA<>'9' AND CajChi<>'S';
AND MONTH(FECEMI)=XMES


GO TOP
SCAN
	SCATTER MEMVAR
	SELE Cp
	APPEND BLANK
	GATHER MEMVAR
	SELE DIAR
ENDSCAN

SELE CP
vind3= SYS(3) + '.idx'
vind4= SYS(3) + '.idx'
INDEX ON ALLTRIM(Comprob)+Tipo  TO (vind3)
INDEX ON ALLTRIM(Comprob)+cuenta+Tipo TO (vind4)

* Para acumular el monto de todas las retenciones del c/p.
SET INDEX TO (vind3)
GO TOP
SCAN
    Tiene   = .F.
	vComp1  = ALLT(CP.Comprob)
	vComp2  = ALLT(CP.Comprob)
    vRecno  = RECNO()	
	STORE 0 TO SumaRet

	SCAN WHILE vComp1=vComp2
		IF Reten='S'
			Tiene   = .T.
			SumaRet = SumaRet + CP.Montos
		ENDIF
		vComp2=ALLT(CP.Comprob)
	ENDSCAN
	SKIP -1
	vRec1  = RECNO()
	
    *----------------------- 
	* Si tiene retenciones
	IF Tiene
		SELE CP
		GO (vRecno)
		SEEK vcomp1+'D'
		* Para encontrar la cuentas del Debe con mayor monto
		vComp11  = ALLT(CP.Comprob)
		vComp22  = ALLT(CP.ComproB)
		mayor   = 0
		SCAN WHILE vComp11=vComp22 AND Tipo='D'
			IF CP.Montos > Mayor
				Mayor = CP.Montos
				vComp = CP.Comprob
				vCuen = CP.Cuenta
			ENDIF
			vComp22=ALLT(CP.Comprob)
		ENDSCAN

		* Ahora se reemplaza el monto mayor del debe menos las retenciones. 
		SELE CP
		SET INDEX TO (vind4)
		SEEK vComp11+vCuen+'D'
		IF FOUND()
			REPLACE CP.Montos WITH CP.Montos-SumaRet
		ENDIF
		SET INDEX TO (vind3)
	ENDIF
	*---------------------------
	GO (VREC1)		
ENDSCAN

SELE CP
SET FILTER TO
SET INDEX TO (vind4)

* Se acumula los saldos a nivel de divisionaria
SET FILTER TO Comprob='98' AND CUENTA<>'9' AND CUENTA<>'101' AND RETEN<>'S'

GO TOP
SCAN
	SCATTER MEMVAR
	vCuen    = LEFT(CP.Cuenta,5)
	vComprob = CP.Comprob
	*vMonto   = Saldo2(vCuen,Vcomprob)	
		
	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE Comprob=vComprob AND SUBSTR(Cuenta,1,5) = vCUEN
		IF Tipo = 'D'
	       	vDMonto = vDMonto + CP.Montos
		ELSE
		   	vHMonto = vHMonto + CP.Montos
		ENDIF
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------

	SELE CP1
			IF  vMontoD >0 
				APPEND BLANK		
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF  vMontoH > 0 
				APPEND BLANK
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
	SELE CP
	SKIP-1
ENDSCAN
RETURN

PROCEDURE NotConRet
*-------------------

*SET FILTER TO  Comprob='98' AND Cuenta<>'101' AND cuenta<>'104' AND Cuenta<>'9';

SELE Diar
SET FILTER TO
USE &vindz   IN 15 ALIAS Tempo  EXCLUSIVE
USE &vindW   IN 16 ALIAS Tempo1 EXCLUSIVE
USE &vindM   IN 17 ALIAS Tempo5 EXCLUSIVE

SELE Diar

SET FILTER TO (Comprob='98') AND (Cuenta<>'101'  AND Cuenta<>'9' AND MONTH(FECEMI)=XMES)

GO TOP
SCAN
	SCATTER MEMVAR
	SELE Tempo
	APPEND BLANK
	GATHER MEMVAR
	SELE Diar
ENDSCAN

SELE Tempo
vind9 = SYS(3) + '.idx'
vind10= SYS(3) + '.idx'
INDEX ON ALLTRIM(Comprob)+Tipo  TO (vind9)
*INDEX ON ALLTRIM(Comprob)+Tipo+LEFT(Cuenta,3)  TO (vind10)
*INDEX ON ALLTRIM(Comprob)+cuenta+Tipo TO (vind10)
SET INDEX TO (vind9)

* Se verifica cada c/p

SCAN
	vComp1  = ALLT(tempo.Comprob)
	vComp2  = ALLT(tempo.ComproB)
	mayor   = 0
	* Para encontrar la cuentas del Debe con mayor monto
	SCAN WHILE vComp1=vComp2 AND Tipo='D'
		IF tempo.Montos > Mayor
			Mayor = tempo.Montos
			vComp = tempo.Comprob
			vCuen = tempo.Cuenta
		ENDIF
		vComp2=ALLT(tempo.Comprob)
	ENDSCAN

	* La contracuenta de las retenciones es la cuenta del debe, con
	* mayor monto

	SELE Tempo
	vComp11  = VCOMP1
	vComp22  = VCOMP1
	VRECNO = RECNO()

	SEEK allt(vcomp1) +'H'
	IF FOUND()
		DO WHILE allt(vComp11) = allt(vComp22)

			IF RETEN='S' 
				RCuenta = Tempo.Cuenta && Cuenta de Retencion
				RMontos = Tempo.Montos
				SELE Tempo1
				FOR I=1 TO 2
					IF I=1
						APPEND BLANK
						REPLACE Comprob WITH 'EE'++LEFT(Tempo.Comprob,2)     && EE=Retenciones
						REPLACE orden with '34'
						REPLACE FecEmi  WITH Tempo.FecEmi
						REPLACE Tipo    WITH 'D'		
						REPLACE Cuenta  WITH vCuen	
						REPLACE Montos  WITH Rmontos
					ELSE
						APPEND BLANK
						REPLACE Comprob WITH 'EE'+LEFT(Tempo.Comprob,2)     && EE=Retenciones
						REPLACE FecEmi  WITH Tempo.FecEmi
						REPLACE orden with '34'
						REPLACE Tipo    WITH 'H'		
						REPLACE Cuenta  WITH RCuenta		
						REPLACE Montos  WITH Rmontos
					ENDIF
				ENDFOR
			ENDIF
			SELE Tempo
			IF !EOF()
				SKIP
			ELSE
				EXIT
			ENDIF
			vComp22 = allt(Tempo.Comprob)

		ENDDO
		SELE TEMPO
		SKIP-1
	ELSE
		GO (VRECNO)
	ENDIF
ENDSCAN

SELE Tempo1
* Se acumula los saldos a nivel de divisionaria
*SET FILTER TO  Comprob='98' AND Cuenta<>'101' AND cuenta<>'104' AND Cuenta<>'9'
GO TOP
SCAN
	SCATTER MEMVAR
	vCuen    = LEFT(tempo1.Cuenta,5)
	vComprob = ALLTR(tempo1.Comprob)
		
	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE Comprob=vComprob AND SUBSTR(Cuenta,1,5) = vCUEN
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
		   	vHMonto = vHMonto + Montos
		ENDIF
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------
	
	SELE Tempo5
		IF  vMontoD >0 
			APPEND BLANK
			GATHER MEMVAR
			REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
			REPLACE  tipo   WITH 'D'
			REPLACE  Montos WITH vMontoD
		ENDIF
		IF  vMontoH > 0 
			APPEND BLANK
			GATHER MEMVAR
			REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
			REPLACE  tipo   WITH 'H'
			REPLACE  Montos WITH vMontoH
		ENDIF
	SELE tempo1
	SKIP-1
ENDSCAN

PROCEDURE REIE
*-------------
SELE Diar
SET FILTER TO Comprob='01' AND MONTH(FECEMI)=XMES
GO TOP
IF !EOF()
	COPY STRU TO (vindT)
	USE (vindt) IN 18 ALIAS Reie EXCLUSIVE
	SELE Diar
	GO TOP
	SCAN
		SCATTER MEMVAR
		SELE Reie
		APPEND BLANK
		GATHER MEMVAR
		IF m.Tipo = 'D'
			REPLACE Comprob WITH '01A'
		ELSE
			REPLACE Comprob WITH '01B'
		ENDIF
	SELE Diar
	ENDSCAN
ENDIF


SELE Diar
SET FILTER TO Comprob='02'  AND MONTH(FECEMI)=XMES
GO TOP
IF !EOF()
	COPY STRU TO (vindU)
	USE (vindU) IN 19 ALIAS Reie1 EXCLUSIVE
	SELE Diar
	GO TOP
	SCAN
		SCATTER MEMVAR
		SELE Reie1
		IF F_APPD()
			GATHER MEMVAR
			IF m.Tipo = 'D'
				REPLACE Comprob WITH '02A'
			ELSE
				REPLACE Comprob WITH '02B'
			ENDIF
		ENDIF
		SELE Diar
	ENDSCAN
	SELE Diar
ENDIF
SET FILTER TO
RETURN


PROCEDURE AsiPre
*---------------
SELE Diar
SET FILTER TO CUENTA='9' AND MONTH(FECEMI)=XMES 
COPY STRU TO (vindv)
USE (vindv) IN 20  ALIAS Presu EXCLUSIVE
SELE Diar
GO TOP
SCAN
	SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,4)
		vComprob = LEFT(Diar.Comprob,2)
		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=4,(SUBSTR(Cuenta,1,4) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto

	     SELE Presu
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				IF COMPROB='18'
					REPLACE COMPROB WITH '18'
				ELSE
					REPLACE COMPROB WITH '69'
				ENDIF	
				REPLACE  Cuenta WITH LEFT(m.Cuenta,4)+'000000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and F_APPD()
				GATHER MEMVAR
				IF COMPROB='18'
					REPLACE COMPROB WITH '18'
				ELSE
					REPLACE COMPROB WITH '69'
				ENDIF	
				REPLACE  Cuenta WITH LEFT(m.Cuenta,4)+'000000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
	SELE Diar
	SKIP-1
ENDSCAN
SELE Diar
SET FILTER TO
RETURN



*- Este procedimiento genera un asiento aparte, de las H/C que utilizan
*- la partida 6.5... por las obras.	
PROCEDURE AsiObra1
*------------------
SELE Diar
SET FILTER TO
SET FILTER TO LEFT(CodPart,2)='65' AND Cuenta<>'9' AND MONTH(FECEMI)=XMES
GO TOP
IF !EOF()
	COPY STRU TO &vindO
	USE &vindO IN 21 ALIAS Obra EXCLUSIVE
	SELE Diar
	GO TOP
	SCAN	
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)
		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------
		SELE Obra
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '29'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '29'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE Diar
	skip-1	
	ENDSCAN
ENDIF
SELE DIAR
SET FILTER TO
		

*- Este procedimiento realiza los asientos aparte de las H/C
*- de planillas.
PROCEDURE AsiPla
*-----------------
SELE Diar
SET FILTER TO TIPDOC='PLA' AND CUENTA<>'9' AND MONTH(FECEMI)=XMES
GO TOP
IF !EOF()
	COPY STRU TO &vindP
	USE &vindP IN 22 ALIAS Planilla EXCLUSIVE
	SELE DIAR
	GO TOP
	SCAN
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)

		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------

		SELE Planilla
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '30'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and  F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '30'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE DIAR
	SKIP-1
	ENDSCAN
ENDIF
SET FILTER TO

PROCEDURE AsiCts
*-----------------
SELE Diar
SET FILTER TO COMPROB='03' AND TIPDOC='RD' AND CUENTA<>'9';
AND MONTH(FECEMI)=XMES and codanal='01.15'
GO TOP

IF !EOF()
	COPY STRU TO &vindc
	USE &vindc IN 0 ALIAS Cts EXCLUSIVE
	SELE DIAR
	GO TOP
	SCAN
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)

		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------

		SELE cts
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '22'
				REPLACE Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE tipo   WITH 'D'
				REPLACE Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and  F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '22'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE DIAR
	SKIP-1
	ENDSCAN
ENDIF
SET FILTER TO

PROCEDURE AsiObra2
*------------------
SELE Diar
SET FILTER TO Comprob='03' AND (CodPart='651130' OR CodPart='651137') AND Cuenta<>'9';
AND MONTH(FECEMI)=XMES AND TIPDOC='O/S'
GO TOP
IF !EOF()
	COPY STRU TO &vindQ
	USE &vindQ IN 21 ALIAS Mano EXCLUSIVE
	SELE Diar
	GO TOP
	SCAN
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)
		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------
		SELE Mano
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '31'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '31'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE Diar
	SKIP-1
	ENDSCAN
ENDIF
SET FILTER TO



* ESTE PROCEDIMIENTO YA NO SE DEBE REALIZAR A PARTIR DE AGOSTO
PROCEDURE CajaChica
*-------------------
SELE Diar
SET FILTER TO

COPY TO &vindS FOR  (COMPROB='98' AND CajChi='S' AND CUENTA<>'101' AND CUENTA<>'9');
AND MONTH(FECEMI)=XMES

USE &vindS IN 24 ALIAS Caja

SELE Caja
GO TOP
SCAN
	REPLACE COMPROB WITH '77'
ENDSCAN
RETURN


PROCEDURE CERTIFICADO
*---------------------
SELE DIAR
COPY STRU TO (VINDJ)
USE (VINDJ) IN 23 ALIAS CERTF
SET FILTER TO (COMPROB='14' AND TIPOBR='N')
GO TOP
SCAN
	SCATTER MEMVAR
	SELE CERTF
	append blank
	GATHER MEMVAR
	REPLACE COMPROB WITH '11'
	SELE DIAR
ENDSCAN
RETURN

FUNCTION leefolio
*----------------
PARAMETERS xTD,xM,xcta
IF LEFT(COMPROB,2)='GG'
*	WAIT WIND COMPROB
ENDIF
vali=ALIAS()
SELE Folios
DO CASE
	CASE LEN(ALLT(XCTA))=2
		SEEK ('LIBMAY'+zmes+zano+xtd+xcta+'00000000')
	CASE LEN(ALLT(XCTA))=4
		SEEK ('LIBMAY'+zmes+zano+xtd+xcta+'000000')
	OTHERWISE
		SEEK ('LIBMAY'+zmes+zano+xtd+xcta+'00000')
ENDCASE
IF FOUND() 
   SELE (VALI)
   RETURN PADL(ALLTRIM(STR(Folios.DFolio,4)),4,'0')
ENDIF
SELE (VALI)
RETURN ' '

*---------------------------------------------------------------------
* Funci¢n que guarda el folio al momento de imprimir el Diario Resumen
*---------------------------------------------------------------------
FUNCTION Guardafol
*-----------------
 PARAMETER xtipdoc,xtipo,xcuenta,xpagina
 SELE Folios
 DO CASE
	CASE LEN(ALLT(XCuenta))=2
		SEEK ('LIBDIR'+zmes+zano+xtipdoc+xcuenta+'00000000')
		xcuenta = xcuenta +'00000000'
	CASE LEN(ALLT(XCuenta))=4
		SEEK ('LIBDIR'+zmes+zano+xtipdoc+xcuenta+'000000')		
		xcuenta = xcuenta +'000000'
	OTHERWISE
		SEEK ('LIBDIR'+zmes+zano+xtipdoc+xcuenta+'00000')
		xcuenta = xcuenta +'00000'
ENDCASE
 
 IF found()
     Bloquea = f_lock(1)
 ELSE
     Bloquea = f_appd()
 ENDIF
 IF Bloquea
    REPLACE Reporte WITH 'LIBDIR',;
            Mes     WITH ZMes,;
		    Periodo WITH ZAno,;
		    Tipdoc  WITH xTipdoc,;
		    Cuenta  WITH xCuenta,;
		    Dfolio  WITH IIF(xtipo='D',xpagina,Dfolio),;
		    Hfolio  WITH IIF(xtipo='H',xpagina,Hfolio)
 ENDIF
SELECT Diar
RETURN ' '

*----MARCO



FUNCTION DMov_Ant
*----------------
SELECT Saln
IF (ALLTRIM(VAjustado)="Ajustado")
   SUM dsumAJU TO Vdmov_ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
   
ELSE
   SUM dsumana TO Vdmov_ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
ENDIF   
RETURN Vdmov_ant


FUNCTION hMov_Ant
*----------------
SELECT Saln
IF (ALLTRIM(vAjustado)="Ajustado")
    SUM hsumAJU TO VhMov_Ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
ELSE
	SUM hsumana TO VhMov_Ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
ENDIF	

RETURN VhMov_Ant




FUNCTION sub_total
*-----------------
IF Temp.Tipo='D'
	vDVan =  vDVan + DSUMA()
ELSE	
	vHVan =  vHVan + HSUMA()
ENDIF
RETURN ' '


FUNCTION sub_total1
*-----------------
PARAMETER vTipo
IF vTipo='D'
	vDVan =  vDVan + vDebe101
ELSE	
	vHVan =  vHVan + vHaber101
ENDIF

RETURN ' '

