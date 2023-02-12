 ** ---------------------------------------------------
** LIBDir.PRG
** Reporte Resumen del Libro Diario (Mensual o Anual)
** AUTOR: MERA
** Ultima Modif. : 18/10/2014 por JFG
** ---------------------------------------------------

* Abrir Archivos de trabajo

CLOS DATA
USE Parmae  IN 1  ORDER TAG Parmae1		ALIAS parma
USE Diario  IN 2  ORDER TAG Diario11	ALIAS Diar
USE SalDoNa IN 3  ORDER TAG SaldoNa1	ALIAS SalN
USE Cuentas IN 4  ORDER TAG Cuentas1	ALIAS Cuen
USE Compro  IN 5  ORDER TAG Compro7		ALIAS Comp
USE Glosas  IN 6  ORDER TAG Glosas1		ALIAS Glos
USE Folios  IN 10 ORDER TAG Folios1		ALIAS Folios
USE Tipos   IN 11 ORDER TAG Tipos1		ALIAS Tipos

PUBLIC vQui,vDVan,vHVan,lsalta

STORE 0 TO vQui,vDVan,vHVan
lsalta = .F.

zMes = cMes
zAno = cAno

IF zmes='12'
	vajuste  = 'Si'
ELSE
	vajuste  = 'No'
ENDIF

SELECT Diar
SET RELATION TO Comprob+ComAdi+codfte INTO Comp

DO RepDiar
CLOSE DATA
RETURN




PROCEDURE Salida
*---------------
RELEASE    WINDOW LibDir
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


PROCEDURE RepDiar
*----------------
SELE PARMA
SET ORDER TO
vind3= SYS(3) +'.idx'
INDEX ON codigoaux to (vInd3) FOR Tipo='TIPDOC'
SELE Diar
vIndi1= SYS(3)+".IDX"
vDBF  = SYS(3)+".DBF"

WAIT WINDOW NOWAIT 'Reporte del Libro Diario Res£men en proceso...'

*COPY TO (vDbf) FOR ORDEN='57' OR ORDEN='58'

COPY TO (vDbf) FOR IIF(ALLTRIM(vajuste)='Si',IIF(LEFT(Comprob,2)#'93',MONTH(Fecemi)=VAL(zMes),MONTH(Fecemi)<=VAL(zMes)),MONTH(Fecemi)=VAL(zMes) AND LEFT(Comprob,2)#'93')

USE (vdbf) ALIAS Diar
SELE Diar
INDEX ON LEFT(COMPROB,2)+ORDEN+TIPO+CUENTA TO (vIndi1)
SET RELATION TO zAno+LEFT(Comprob,2) INTO Tipos
REPLACE comprob WITH Tipos.TipDoc2+SUBSTR(Comprob,3) FOR LEFT(Comprob,2)=Tipos.tipdoc1
SET RELATION TO

IF ALLTRIM(vAjuste)='Si'
	xDia = LastDay(VAL(zMes),VAL(zAno))
	REPLACE FecEmi WITH xDia FOR Comprob='93'
ENDIF

SELE Diar
GO TOP

SET RELATION TO LEFT(Comprob,2) INTO PARMA ADDITIVE
*DO ESPERA WITH 2
IF EOF()
	WAIT WINDOW "No hay registros para procesar"
*	DO STANDBY WITH "No hay registros para procesar"
ELSE
	GO BOTT
	vFin=RECNO()
	GO TOP
	REPORT FORM LibDir3.frx TO PRINTER PROMPT PREVIEW
	
*	DO REPORTE WITH 2, "LibDir3", 'Resumen del Libro Diario Mensual',1,.F.,.T.
ENDIF
CLOSE DATA
ERASE (vIndi1)
ERASE (vDbf)
ERASE (vInd3)
RETURN


FUNCTION leeglo
*--------------

PARAMETER wano,wTipo,wOrd

*SELE glos
IF SEEK(wano+wTipo+wOrd,"glos")
	STORE Glos.Glosa TO wglosa 
 ELSE
	STORE "Glosa no Existe. por Favor Revise" TO wglosa 
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


PROCEDURE xDSUMA
*--------------
vRec=RECNO()
vKey = SUBSTR(Diar.Comprob,1,2)+substr(Cuenta,1,2)
STORE 0 TO vDMonto,vHMonto
SCAN WHILE SUBSTR(Diar.Comprob,1,2)+substr(Cuenta,1,2) = vKey
     IF Diar.Tipo='D'
        vDMonto = vDMonto + Diar.Montos
        vDTot   =  vDTot  + Diar.Montos
     ELSE
        vHMonto = vHMonto + Diar.Montos
        vHTot   =  vHTot  + Diar.Montos
        =HSUMA()
     ENDIF
ENDSCAN
GO vRec
RETURN vDMonto

PROCEDURE DSUMA
*--------------
vRec=RECNO()
vKey = SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2)
STORE 0 TO vDMonto,vHMonto
SCAN WHILE SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2) = vKey
        vDMonto = vDMonto + Diar.Montos
        vDTot   =  vDTot  + Diar.Montos
ENDSCAN
GO vRec
RETURN vDMonto

PROCEDURE HSUMA
*--------------
vRec=RECNO()
vKey = SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2)
STORE 0 TO vDMonto,vHMonto
SCAN WHILE SUBSTR(Diar.Comprob,1,2)+Diar.tipo+substr(Cuenta,1,2) = vKey
        vhMonto = vhMonto + Diar.Montos
        vhTot   =  vhTot  + Diar.Montos
ENDSCAN
GO vRec
RETURN vhMonto


FUNCTION XHSUMA
*-------------
RETURN vHMonto


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
vQUI = vQUI + 1
RETURN vQUI   


FUNCTION DESCRI
*--------------
PARAMETER mCta,xNivel
PRIVATE xCuenta, mAli
xCta=mCta
mAli = ALIAS()
SELE Cuen
SEEK xCta

DO CASE
	CASE SUBSTR(Cuenta,5,11)='00000000000' AND xNivel=Cuen.Nivel
		mCta = SUBSTR(Cuenta,1,4)
		_z=Des_Cuenta(xCta)
*		_z=IIF(FOUND(),Val_Fun('Cuen','Cuenta','Descri',xCta),'')
	CASE SUBSTR(Cuenta,7,9) ='000000000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,6)
		_z=Des_Cuenta(xCta)
*		_z=IIF(FOUND(),Val_Fun('Cuen','Cuenta','Descri',xCta),'')
	CASE SUBSTR(Cuenta,9,7) ='0000000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,8)
		_z=Des_Cuenta(xCta)
*		_z=IIF(FOUND(),Val_Fun('Cuen','Cuenta','Descri',xCta),'')
	CASE SUBSTR(Cuenta,11,5)='00000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,10)
		_z=Des_Cuenta(xCta)
*		_z=IIF(FOUND(),Val_Fun('Cuen','Cuenta','Descri',xCta),'')
	CASE SUBSTR(Cuenta,13,3)='000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,12)
		_z=Des_Cuenta(xCta)
*		_z=IIF(FOUND(),Val_Fun('Cuen','Cuenta','Descri',xCta),'')
 	OTHERWISE
	   mCta = ''
		_z= ''
*	   REPLACE Cuenta WITH SUBSTR(Cuenta,1,10)
ENDCASE

*_z=IIF(FOUND(),Val_Fun('Cuen','Cuenta','Descri',xCuenta),'')
SELE (mAli)
RETURN _z


FUNCTION ZZ
*----------
RETURN xCuenta


FUNCTION DESCRI1
*---------------
vAli=ALIAS()
xCuenta=LEFT(Diar.Cuenta,3)
vCuenta=LEFT(Diar.Cuenta,3)+'0000000'
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

*---------------------------------------------------------------------
* Funci¢n que guarda el folio al momento de imprimir el Diario Resumen
* para la Sra. Genny Ramirez => DDJ
*---------------------------------------------------------------------
FUNCTION Guardafol
*-----------------
PARAMETER xComp,xOrden,xtipo,xcuenta,xpagina
IF LEFT(xCuenta,2)='  '
	WAIT WINDOW "Error en Cuenta. revise N/C "+Diar.comprob+'-'+Diar.ComAdi
*	DO STANDBY WITH "Error en Cuenta. revise N/C "+Diar.comprob+'-'+Diar.ComAdi
	RETURN " "
ENDIF
xCuenta=LEFT(xCuenta,4)+'00000000000'
SELE Folios
IF SEEK ('LIBDIR'+zmes+zano+xComp+xOrden+xcuenta)
	Bloquea = f_lock(1)
 ELSE
	Bloquea = f_appd()
ENDIF
IF Bloquea
	REPLACE Reporte WITH 'LIBDIR',;
			Mes     WITH ZMes,;
			Periodo WITH ZAno,;
			Orden   WITH xOrden,;
			Tipdoc  WITH xComp,;
			Cuenta  WITH xCuenta,;
			Dfolio  WITH IIF(xtipo='D',xpagina,Dfolio),;
			Hfolio  WITH IIF(xtipo='H',xpagina,Hfolio)
ENDIF
SELECT Diar
RETURN ' '


FUNCTION vTipoDoc
*----------------
PARAMETERS xNum
vAlias = SELECT()
SELE PARMA
SEEK xNum
vret = ''
IF FOUND()
   vret = parma.descri
ENDIF   
SELECT(vAlias)
RETURN vret


FUNCTION leefolio
*----------------
PARAMETERS xComp,xOr,xM,xcta
vali=ALIAS()
SELE Folios

SEEK ('LIBMAY'+xM+zano+xComp+xOr+xcta+'00000000')
IF FOUND() 
   SELE (VALI)
   RETURN PADL(ALLTRIM(STR(Folios.DFolio,4)),4,'0')
ENDIF
SELE (VALI)
RETURN ' '


FUNCTION fin
*-----------
RETURN vfin=recno()


FUNCTION DMov_Ant
*----------------
SELECT Saln
IF (ALLTRIM(vAjuste)="Ajustado")
   SUM dsumAJU TO Vdmov_ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
 ELSE
   SUM dsumana TO Vdmov_ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
ENDIF   
RETURN Vdmov_ant


FUNCTION hMov_Ant
*----------------
SELECT Saln
IF (ALLTRIM(vAjuste)="Ajustado")
    SUM hsumAJU TO VhMov_Ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
ELSE
	SUM hsumana TO VhMov_Ant FOR Saln.mes<xmes AND saln.ano=xano AND SUBS(saln.cuenta,3,8)=REPL('0',8) 
ENDIF	

RETURN VhMov_Ant


FUNCTION sub_total
*-----------------
IF Diar.Tipo='D'
	vDVan =  vDVan + IIF(Tipo='D',SumMon(SUBSTR(Cuenta,1,4),2),0)
*	vDVan =  vDVan + DSUMA()
ELSE	
	vHVan =  vHVan + IIF(Tipo='H',SumMon(SUBSTR(Cuenta,1,4),2),0)
*	vHVan =  vHVan + HSUMA()
ENDIF
RETURN ' '


FUNCTION Cuenta
*--------------
PARAMETERS xCta,xNivel
PRIVATE mAli
mAli = ALIAS()
mCta = PADR(xCta,15,'0')
SELE Cuen
SEEK mCta

*wait wind mcta
*wait wind iif(found(),"encontre","no encontre")

DO CASE
	CASE SUBSTR(Cuenta,5,11)='00000000000' AND xNivel=Cuen.Nivel
		mCta = SUBSTR(Cuenta,1,4)
	CASE SUBSTR(Cuenta,7,9)='000000000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,6)
	CASE SUBSTR(Cuenta,9,7)='0000000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,8)
	CASE SUBSTR(Cuenta,11,5)='00000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,10)
	CASE SUBSTR(Cuenta,13,3)='000' AND xNivel=Cuen.Nivel
	   mCta = SUBSTR(Cuenta,1,12)
	OTHERWISE
		* Solo Coje cuenta tal como esta
	   mCta = ''
ENDCASE

SELE (mAli)
RETURN mCta

*FUNCTION Cuenta
*--------------
*PARAMETERS xCta,xNivel
*PRIVATE mAli
*mAli = ALIAS()
*mCta = PADR(xCta,10,'0')
*SELE Cuen
*SEEK mCta
*IF SUBSTR(Cuenta,1,1)='0' OR SUBSTR(Cuenta,1,2)='9'
*	DO CASE
*		CASE SUBSTR(Cuenta,3,8)='00000000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,2)
*		CASE SUBSTR(Cuenta,5,6)='000000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,4)
*		CASE SUBSTR(Cuenta,7,4)='0000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,6)
*		CASE SUBSTR(Cuenta,9,2)='00' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,8)
*	 	OTHERWISE
*			* Solo Coje cuenta tal como esta
*		   mCta = ''
*	ENDCASE
* ELSE
*	DO CASE
*		CASE SUBSTR(Cuenta,3,8)='00000000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,2)
*		CASE SUBSTR(Cuenta,4,7)='0000000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,3)
*		CASE SUBSTR(Cuenta,6,5)='00000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,5)
*		CASE SUBSTR(Cuenta,8,3)='000' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,7)
*		CASE SUBSTR(Cuenta,10,1)='0' AND xNivel=Cuen.Nivel
*		   mCta = SUBSTR(Cuenta,1,9)
*	 	OTHERWISE
*			* Solo Coje cuenta tal como esta
*		   mCta = ''
*	ENDCASE
*ENDIF
*SELE (mAli)
*RETURN mCta

FUNCTION SUMMON
*--------------
PARAMETERS xCuenta,xNivel
PRIVATE xRec,xOrd,xComp,mAlias
STORE 0 TO vMtoD,vMtoH
mAlias = ALIAS()
xOrd  = diar.orden
xComp = LEFT(diar.Comprob,2)
SELE Cuen
SEEK xCuenta
IF FOUND()
	IF Nivel=xNivel
		SELE (mAlias)
		xRec = RECNO()
		xCta = Cuenta(xCuenta,xNivel)
		
*		wait wind xcta
*		wait wind str(xnivel)
		
		SCAN WHILE Cuenta = xCta AND xComp = LEFT(diar.Comprob,2) AND xOrd = diar.orden
			IF STR(YEAR(FecEmi),4)#zAno and !lsalta
				if yesno("Error No coincide el Periodo de Trabajo. de N/C "+Diar.comprob+'-'+Diar.ComAdi +". Desea pasar por alto este tipo de errores?")
					lsalta = .T.
				ENDIF
			ENDIF
			
			IF Tipo = 'D'
				vMtoD = vMtoD + Montos
			 ELSE
				vMtoH = vMtoH + Montos
			ENDIF
		ENDSCAN
		GO xRec
	 ELSE
		xTer = 0
	ENDIF
ENDIF

SELECT (mAlias)
RETURN IIF(Tipo='D',vMtoD,VmtoH)

PROCEDURE NotConRet
*-------------------

SELE vDBF
USE &vindz   IN 15 ALIAS Tempo  EXCLUSIVE
USE &vindW   IN 16 ALIAS Tempo1 EXCLUSIVE
USE &vindM   IN 17 ALIAS Tempo5 EXCLUSIVE
SELE vDBF

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
	vCuen    = tempo1.Cuenta
	vComprob = ALLTR(tempo1.Comprob)
		
	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE Comprob=vComprob AND Cuenta = vCUEN
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
			REPLACE  Cuenta WITH m.Cuenta
			REPLACE  tipo   WITH 'D'
			REPLACE  Montos WITH vMontoD
		ENDIF
		IF  vMontoH > 0 
			APPEND BLANK
			GATHER MEMVAR
			REPLACE  Cuenta WITH m.Cuenta
			REPLACE  tipo   WITH 'H'
			REPLACE  Montos WITH vMontoH
		ENDIF
	SELE tempo1
	SKIP-1
ENDSCAN
RETURN
