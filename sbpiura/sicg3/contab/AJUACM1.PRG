** --------------------------------------------
** AJUACM1.PRG
** Proceso de C lculo de  Ajuste Por Correccion 
** Financiera y Elaboraci¢n Autom tica de Nota 
** de Contabilidad para el Balance Construcctivo 
** AUTOR   : UDEP - Ing. Virna AVel squez
** Fecha   : 04 de Diciembre de 1995
** Soporte : OOC y ACC 
** --------------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens05,Vmens06,Vmens07
PUBLIC m.Comprob,vTip
SET DECIMAL TO 4

USE Parmae   IN 6  ORDER TAG Parmae1    ALIAS parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloqueado, Consulte al Area de Sistemas"
   CLOSE DATA
   RETURN 
ENDIF   

* Abriendo Archivos
USE Cuentas  IN 3 ORDER TAG Cuentas1     ALIAS Cuen
USE SaldoNa  IN 5 ORDER TAG SaldoNa1     ALIAS SalN
USE Compro   IN 7 ORDER TAG Compro7      ALIAS Comp
USE Diario   IN 8 ORDER TAG Diario11     ALIAS Diar
USE Factores IN 10 ORDER TAG Factores2   ALIAS Factores
USE HojTra   IN 11 ORDER TAG HojTra1     ALIAS HojTra EXCLUSIVE

SELECT Parma
SEEK 'MESANOACTUAL'
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))

* Para indicar que ya se realizo el proceso de ajuste

SEEK 'OPERATAJSCC'
IF FOUND()
	REPLACE Descri WITH 'SI'
ENDIF
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

SELE HojTra
SET RELATION TO 'M'+Ano+NumMes INTO Factores

DO Inicia                    && Define ventanas, t¡tulos
DO Pregunta                  && Pregunta y proceso
DO Fin_opcion


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

DEFINE WINDOW wAjuAcm  FROM 09,20 TO 16,60 SHADOW DOUBLE ;
	TITLE " Proceso de Ajuste ACM " COLOR SCHEME 5

RETURN


*- Proceso
PROCEDURE Pregunta
*-----------------
PRIVATE cCampo
ACTIVATE WINDOW wAjuACM
vFecha = LastDay(MONT(vmesact),YEAR(vmesact))

vMes=PADL(MONTH(vFecha),2,'0')
vAno=STR(YEAR(vFecha),4)
vFactor=1
@ 1,1  SAY '        Mes y A¤o :' GET vMes    PICTURE '!!' 
*       VALID val_para(Vmes,'FECMES','C')
@ 1,25 GET vAno  PICTURE '!!!!'  VALID factor('M',vmes,vano)
@ 2,1  SAY '    Fecha Contable:' GET vFecha  VALID vfecha<=DATE()
@ 3,1  SAY ' Factor de Ajuste :' GET vFactor PICTURE '9999.99999'		&& VALID (vfactor >1)
@ 04,05 GET OKCANCEL FUNCTION '*TH \!\<Procesa;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

DEACTIVATE WINDOW wAjuACM
RELEASE    WINDOW wAjuACM

IF !((MONTH(vFecha)>MesCerrado AND YEAR(vFecha)=AnoCerrado) OR YEAR(vFecha)>AnoCerrado)
	DO StandBy WITH "­­­ Dicho mes ya est  cerrado !!!" 
	CLOS DATABASE
	RETURN .T.
ENDIF

IF okcancel # 2 AND LASTKEY()#27
	fverdad=yesno("Esta Seguro de Ejecutar el Ajuste del Mes?")
	IF LASTKEY()#27 .AND. fverdad
		vTip = '93'
		m.Comprob = vTip+'09999'+vMes+'     '
		IF vMes#'00'
			cCampo1 = 'Fac'+LEFT(cmes(VAL(vMes)),3)
			cCampo2 = 'Aju'+LEFT(cmes(VAL(vMes)),3)
		ENDIF
		IF VAL(vMes)>1
			cCampo3 = 'Aju'+LEFT(cmes(VAL(vMes)-1),3)
		 ELSE
			cCampo3 = ''
		ENDIF
		
		*---------------------------------------------------------
		* Borramos la Hoja de Trabajo para Volverla a Generar...!
		*---------------------------------------------------------
		DO Espera WITH 1,"Preparando Informaci¢n..."
		SELE SalN
		SET FILTER TO mes = VAL(vMes) AND ano = VAL(vAno)
		***
		*SALDONA EN ALGUN MOMENTO SE PUEDE UTILIZAR PARA AGREGAR AJUSTES EN AJUMES Y AJUANO
		****
		*REPLACE ALL Dajumes WITH 0
		*REPLACE ALL Hajumes WITH 0
		*REPLACE ALL Dajuano WITH 0
		*REPLACE ALL Hajuano WITH 0
		
		SELE HojTra
		DELE FOR NumMes=vMes AND Ano=vAno
		
		IF vMes#'00'
			REPLACE &cCampo1 WITH 0 FOR Ano=vAno
			REPLACE &cCampo2 WITH 0 FOR Ano=vAno
		ENDIF
		
		PACK
		
		DO BorraNC
		
		*---------------------------------------------------------
		SELE SalN
		SET FILTER TO mes = VAL(vMes) AND ano = VAL(vAno)
		****

		DO Progreso WITH 1,"Preparando Informacion..."

		REPLACE ALL Dajumes WITH 0
		REPLACE ALL Hajumes WITH 0
		REPLACE ALL Dajuano WITH 0
		REPLACE ALL Hajuano WITH 0
		
		SELE Cuen
		SET FILTER TO Cuen.Ajuste=.T.
		COUNT TO Registros
		Actual = 0
		GO TOP
		DO Progreso WITH 1,"Calculando Saldos..."
		SCAN
			Actual = Actual + 1
			DO Progreso WITH 2,'',registros,actual
			@ 1,0 SAY Cuen.Cuenta
			SELE SalN
			SEEK Cuen.Cuenta
			IF FOUND() AND mes = VAL(vMes) AND ano = VAL(vAno)
				SELE SalN
   				REPLACE SalN.DAjuMes WITH SalN.DSUMANA* vfactor - SalN.DSUMANA
				REPLACE SalN.HAjuMes WITH SalN.HSUMANA* vfactor - SalN.HSUMANA
				REPLACE SalN.DAjuAno WITH SalN.DSALDONA*vfactor-SalN.DSALDONA
				REPLACE SalN.HAjuAno WITH SalN.HSALDONA*vfactor-SalN.HSALDONA
				
				nSaldoMov = SalN.Dsumana - SalN.Hsumana
				IF Cuen.Nivel>=2 AND nSaldoMov#0.00
					m.Cuenta     = Cuen.Cuenta
					m.NumMes     = vmes
					m.ano        = vano
					m.CuenAju    = PADR(SUBSTR(Cuen.Cuenta,1,2)+'8',10,'0')
					m.SaldoMo    = nSaldoMov
					
					SELE HojTra  
					SEEK Cuen.Cuenta+vmes+vano
					IF !FOUND()
						IF F_Appd()
							GATHER MEMVAR
						ENDIF
					 ELSE
						IF F_Lock()
							GATHER MEMVAR
						ENDIF
					ENDIF
				ENDIF
			ENDIF
			SELE Cuen
		ENDSCAN
		DO Progreso WITH 3
		SELE Hojtra
		DO Espera WITH 1, "Calculando Ajustes"
		
		IF vMes#'00'
			REPLACE &cCampo1 WITH Factores.&cCampo1;
					FOR	BETW(VAL(NumMes),0,VAL(vMes)-1) AND Ano=vAno
			REPLACE &cCampo2 WITH (SaldoMo*Factores.&cCampo1)-saldomo;
					FOR BETW(VAL(NumMes),0,VAL(vMes)-1) AND Ano=vAno
		ENDIF
		
		DO Espera WITH 1, "Creando Notas de Contabilidad"
		
		GO TOP
		SET RELATION TO Cuenta INTO Cuen ADDITIVE
		
		FOR i = 0 TO VAL(vMes)-1
			IF vMes='00'
				LOOP
			ENDIF
			SET FILTER TO ANO=vAno AND Cuen.Detalle='S' AND nummes=PADL(ALLTRIM(STR(i)),2,'0')
			GO TOP
			
			m.TipDoc   = vTip
*			m.Comprob  = vTip+'09999'+vMes
			m.Comadi     = PADL(ALLTRIM(STR(i)),2,'0')
			m.Fecha      = vfecha
			m.Glosa1     = 'NOTA DE CONTABILIDAD QUE CORRESPONDE AL MES DE '+cmes(VAL(vMes))
			m.Glosa2     = 'POR ACTUALIZACION DE AJUSTES POR CORRECCION MONETARIA CON FACTOR: '+STR(&cCampo1,8,4)
			m.CodFte     = '09'
			m.nummesfte  = vMes
			m.periodofte = vAno
			m.numdocfte  ='9999'
			m.Nummesref  = vmes
			m.Docref     = '9999'
			m.Tipdcr     = '93'
			m.EstCon     = '55'
			
			SELE Comp
			IF F_Appd()
				GATHER MEMVAR
			ENDIF
			
			m.Item  = 0
			xTotDeb = 0
			xTotHab = 0
			ccDebe  = 0
			ccHaber = 0
			
			SELE HojTra
			SCAN
*				IF CUENAJU='208'
*					SET STEP ON
*				ENDIF
				m.Orden  = "01" 
				m.Fecemi = m.Fecha
				m.NumMov = 1
				m.Cuenta = CuenAju
				m.TipAux = '10'
				m.CodFte = '09'
				m.Docref = '9999'
				m.Tipdcr = '93'
				m.EstCon = '55'
				
				nRec = RECNO()
				nMontos = 0
				SCAN WHILE CuenAju = m.Cuenta
					IF TYPE(cCampo3) = 'U'
						nDiferencia = 0
					 ELSE
						nDiferencia = &cCampo3
					ENDIF
					nMontos = nMontos + (&cCampo2-nDiferencia)
				ENDSCAN
				
				SKIP -1
				
				m.Montos = ROUND(ABS(nMontos),2)
				m.Tipo   = IIF(nMontos>0,'D','H')
				
				xTotDeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
				xTotHab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
				
				ccHaber = ccHaber + IIF(m.Tipo = 'D',Montos,0)
				ccDebe  = ccDebe  + IIF(m.Tipo = 'H',Montos,0)
				
				SELE Diar
				cOrd = ORDER()
				SET ORDER TO Diario9
				IF !SEEK(m.Comprob+m.ComAdi+m.Cuenta)
					IF F_Appd()
						m.Item   = m.Item + 1
						GATHER MEMVAR
					ENDIF
				 ELSE
					IF F_Lock(1)
					 	REPLACE Montos WITH Montos+m.Montos
					ENDIF
				ENDIF
				SET ORDER TO (cOrd)
				SELE HojTra
			ENDSCAN
			FOR J = 1 TO 2
				m.Item   = m.Item + 1
				m.Orden  = "01" 
				m.Fecemi = m.Fecha
				m.NumMov = 1
				m.Cuenta = '8980000000'
				m.TipAux = '10'
				m.CodFte = '09'
				m.Docref = '9999'
				m.Tipdcr = '93'
				m.EstCon = '55'
				m.Tipo   = IIF(J=1,'D','H')
				m.Montos = IIF(m.Tipo = 'D',ccDebe,ccHaber)
				
				xTotDeb = xTotDeb + IIF(m.Tipo='D',m.Montos,0)
				xTotHab = xTotHab + IIF(m.Tipo='H',m.Montos,0)
				
				SELE Diar
				IF F_Appd()
					GATHER MEMVAR
				ENDIF
			ENDFOR
			
			SELE Comp
			SEEK m.Comprob+m.ComAdi
			IF FOUND()
				REPLACE TotDeb WITH xTotDeb
				REPLACE TotHab WITH xTotHab
			ENDIF
			SELE Hojtra
		ENDFOR
		
		DO Espera WITH 2
		
		*falta pasar las notas a saldona
		
		SET CONSOLE ON
		SET BELL TO 800,5
	   _WRAP = .F.
		? CHR(7)+CHR(7)+CHR(7)
	   _WRAP = .T.
		SET BELL TO 512,2
	   SET CONSOLE OFF
	
	ENDIF
	
ENDIF

RETURN

PROCEDURE Fin_opcion
*-------------------
SET DECIMAL TO 2
CLOSE DATABASE
RETURN


FUNCTION FACTOR
*---------------
PARAMETER  tip,zmes,zano
PRIVATE cCampo
SELE Factores
IF zMes # '00'
	SEEK tip+zANO+zMES
	cCampo = "Fac"+LEFT(cmes(VAL(zMes)),3)
	IF FOUND()
	   Vfactor=&cCampo
	 ELSE
	   DO STANDBY WITH ' Factor del mes no Registrado....!, verifique su Tabla de Factores'
	   Vfactor=1
	ENDIF
 ELSE
	Vfactor=1
ENDIF
RETURN vFactor


FUNCTION SALDOd
*---------------
PARAMETER vCuenta
SELE SalN
SEEK vCuenta+xmes-1+xano
IF FOUND()
	vDSaldo = Dsaldona
ENDIF
RETURN VDSALDO


FUNCTION SALDOH
*---------------
PARAMETER vCuenta
SELE SalN
SEEK vCuenta+xmes-1+xano
IF FOUND()
	vHSaldo = Hsaldona	
ENDIF
RETURN vHsaldo

go top
scan
	vcuenta1 = cuenta 
	vcuenta2 = cuenta
	vdsumana = 0
	vHsumana = 0
	
	DO WHILE Vcuenta1 = VCUENTA2 AND !EOF()
		vdsumana = vdsumana + dsumana
		vhsumana = vHsumana + hsumana
		SKIP
		vcuenta2 = cuenta
	ENDDO

	sele 2
	seek vcuenta1
	if found()
		REPLACE sALDOMO WITH vDsumana-vHsumana
	endif
	sele 1
	SKIP -1

ENDSCAN

PROCEDURE ActDia
*---------------
SELE Diar2
INDEX ON Comprob+Cuenta TO Ind001
COPY STRU TO vtempo
use vtempo in 0
SET FILTER TO COMPROB='93' AND IIF(VMES#'00',MONTH(FECEMI)=VAL(VMES),SUBSTR(COMPROB,8,2)='00')
*SET FILTER TO COMPROB='64' AND MONTH(FECEMI)=VAL(VMES)
store 0 to vmotto
GO TOP
SCAN
	scatter memvar
	vcuenta1=M.cuenta
	vcuenta2=M.cuenta
	store 0 to debe,haber
	DO WHILE vcuenta1=vcuenta2
		debe  = debe  + iif(tipo='D',montos,0)
		haber = haber + iif(tipo='H',montos,0)
		SKIP
		vcuenta2=cuenta
	ENDDO
	skip -1
	vmonto = debe - haber
	vmotto = vmotto + vmonto
	SELE VTEMPO
	APPEND BLANK
	GATHER MEMVAR
	REPLACE TIPO   WITH IIF(vmonto>0,'D','H')
	REPLACE MONTOS WITH IIF(vmonto>0,ROUND(vmonto,2),ROUND(vmonto*-1,2))
	SELECT Diar2
ENDSCAN
SELE VTEMPO
GO TOP
SCATTER MEMVAR
SUM MONTOS TO DEBE  FOR TIPO='D'
SUM MONTOS TO HABER FOR TIPO='H'
* ADICIONA LOS ASIENTOS 89
FOR I=1 TO 2
	APPEND BLANK
	GATHER MEMVAR
	VITEM = VITEM+1
	REPLACE ITEM   WITH VITEM
	REPLACE CUENTA WITH '8980000000'
	IF I=1
		REPLACE TIPO   WITH 'H'
		REPLACE MONTOS WITH DEBE
	ELSE
		REPLACE TIPO   WITH 'D'
		REPLACE MONTOS WITH HABER
	ENDIF	
	vmonto = haber - debe
	vmotto = vmotto + vmonto
ENDFOR
SELE Diar
IF VAL(VMES)<12
   APPEND FROM vtempo
ENDIF   

sele comp
seek '9309999'+vMes+'     '
if found()
	replace totdeb with vmotto
	replace tothab with vmotto
endif
return

FUNCTION Cuenta
*--------------
DO CASE
	CASE SUBSTR(Cuen.Cuenta,3,8)='00000000'
	   RETURN LEFT(Cuen.Cuenta,2)
	CASE SUBSTR(Cuen.Cuenta,4,7)='0000000'
	   RETURN LEFT(Cuen.Cuenta,3)
	CASE SUBSTR(Cuen.Cuenta,6,5)='00000'
	   RETURN LEFT(Cuen.Cuenta,5)
	CASE SUBSTR(Cuen.Cuenta,8,3)='000'
	   RETURN LEFT(Cuen.Cuenta,7)
 	OTHERWISE
	   RETURN Cuen.Cuenta
ENDCASE


FUNCTION BorraNC
*---------------
SELE Comp
cOrden = ORDER()
SET ORDER TO tag COMPRO10
* Se Verifica la existencia de Documentos del Mismo Tipo
SEEK vtip+Vmes+Vano

DO ESPERA WITH 1,PADC('Borrando Notas de Contabilidad de los ajuste ...',50,' ')
SCAN WHILE SUBSTR(Comp.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Comp.Fecha),2)),2,'0')=Vmes AND STR(YEAR(Comp.Fecha),4)=vano
	IF SUBSTR(Comprob,4,4)='9999'
		IF !f_lock(1)
			UNLOCK ALL
			RETURN
		ENDIF
		SELECT Comp
		IF RLOCK()
			DELETE NEXT 1
		ENDIF
	ENDIF
	SELECT Comp
ENDSCAN
SET ORDER TO (cOrden)
SELE Diar
SET ORDER  TO tag Diario18
SEEK vtip+Vmes+Vano
cOrden = ORDER()
SCAN WHILE SUBSTR(Diar.Comprob,1,2)=vtip AND PADL(ALLTRIM(STR(MONTH(Diar.fecemi),2)),2,'0')=Vmes AND STR(YEAR(Diar.fecemi),4)=vano 
	IF SUBSTR(Comprob,4,4)='9999'
		IF !f_lock(1)
			UNLOCK ALL
			RETURN
		ENDIF
		SELECT Diar
		IF RLOCK()
			DELETE NEXT 1
		ENDIF
	ENDIF
	SELECT Diar
ENDSCAN
UNLOCK ALL
DO ESPERA WITH 2

SET ORDER TO (cOrden)
RETURN 