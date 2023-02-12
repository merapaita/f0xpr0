** --------------------------------------------
** AJUSTE.PRG
** Proceso de C lculo de  Ajuste Por Correccion 
** Financiera y Elaboraci¢n Autom tica de Nota 
** de Contabilidad para el Balance Construcctivo
** CON LOS SALDOS RE4ALES A DICIEMBRE
** AUTOR   : UDEP - Ing. Virna AVel squez
** Fecha   : 04 de Diciembre de 1995
** Soporte : OOC y ACC 
** --------------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08
PUBLIC VCOMPROB
Vmens01 ='C lculo de Ajustes Para E.E.F.F.'
Vmens02 = '  Procesa saldos  '
Vmens05 = '  Procesando  '
Vmens06 = 'No es mes actual.'
Vmens07 = 'Debe mayorizar Cuenta corriente'
vHajuste=0
vDajuste=0


SET DECIMAL TO 4

USE Parmae   IN 6  ORDER TAG Parmae1    ALIAS parma
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloqueado, Consulte al Area de Sistemas"
   CLOSE DATA
   RETURN 
ENDIF   


PUBLIC fmes,vprimera,VITEM,GenNCO
Xtotdeb=0
xtothab=0
vItem  =1
vprimera=.t.	

* Abriendo Archivos
* Es el Archivo donde se guarda el C lculo despues del ajuste 
* lo cual permite volver a recalcular el ajuste en caso de haberse cometido 
* alg£n error en lo concerniente a los  ajustes del constructivo
* CREPFIN ** FAVOR OSWALDO DOCUMENTAR
USE CRepFin  IN 1 ORDER TAG CRepFin3     ALIAS CRepFin 	EXCLUSIVE
USE RepFin   IN 2 ORDER TAG RepFin3      ALIAS RepFin
USE Cuentas  IN 3 ORDER TAG Cuentas1     ALIAS Cuen 
USE SaldoNa  IN 5 ORDER TAG SaldoNa1     ALIAS SalN     EXCLUSIVE
USE Compro   IN 7 ORDER TAG Compro7      ALIAS Comp
USE Diario   IN 8 ORDER TAG Diario11     ALIAS Diar
*USE SalObr  IN 9 ORDER TAG Salobr1      ALIAS SalO
USE FacADic   IN 10 ORDER TAG FacADic2     ALIAS FacADic
*USE Factores IN 10 ORDER TAG Factores2   ALIAS Factores
USE HTADIC   IN 11 ORDER TAG HTaDIC1     ALIAS HTaDIC   EXCLUSIVE
*USE HojTra   IN 11 ORDER TAG HojTra1     ALIAS HojTra EXCLUSIVE

SELECT Parma
SEEK 'MESANOACTUAL'
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))

* Para indicar que ya se realizo el proceso de ajuste

SEEK 'OPERATAJSCC'
IF FOUND()
	REPLACE Descri WITH 'SI'
ENDIF
fverdad=.f.
DO Inicia                    && Define ventanas, t¡tulos
DO Pregunta                  && Pregunta y proceso
DO Fin_opcion


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
TITLE Vmens01 COLOR SCHEME 10

DEFINE WINDOW Wind_1 FROM 12,23 TO 18,57 DOUBLE ;
TITLE Vmens02 COLOR SCHEME 10

DEFINE WINDOW Wind_2 FROM 06,10 TO 09,70 ;
TITLE Vmens05 COLOR SCHEME 10
 
ACTIVATE SCREEN
RETURN


*- Proceso
PROCEDURE Pregunta
*-----------------
ACTIVATE WINDOW Wind_0
ACTIVATE WINDOW Wind_1
xmens  = ''
vFECHA = DATE()
vGeNC  = 'Si'

vMes=STRTRAN(STR(MONTH(vFecha),2),' ','0')
vAno=SUBSTR(ALLTRIM(STR(YEAR(vFecha),4)),3,2)
vFactor=1
*@ 1,1  SAY '        Mes y A¤o :' GET vMes    PICTURE '!!' 
*       VALID val_para(Vmes,'FECMES','C')
@ 1,25 GET vAno  PICTURE '!!'  			&&VALID factor('M',vmes,vano)
@ 2,1  SAY '    Fecha Contable:' GET vFecha  VALID vfecha<=DATE()
*@ 3,1  SAY ' Factor de Ajuste :' GET vFactor PICTURE '9999.99999' VALID (vfactor >=1) ERROR "EL FACTOR DEBE SER MAYOR QUE 1"
@ 4,1  SAY '  Genera N/C ACM? :' GET vGeNC   PICTURE '@M No,Si'
READ



*VICTOR
vComprob = '6409999'+vMes+'     '
*VICTOR
fverdad=yesno("Esta Seguro de Ejecutar el Ajuste del Mes?")

IF LASTKEY()#27 .AND. fverdad
	*---------------------------------------------------------
	* Borramos la Hoja de Trabajo para Volverla a Generar...!
	*---------------------------------------------------------
	***
	*MARCO
	SELE Diar
	COPY TO xDiaTmp for comprob='00' OR COMPROB='01' OR Comprob='02'
	COPY TO xDia
	USE xdia   IN 14     ALIAS Diar1 EXCLU
	SELE Diar1
	ZAP
	index on Comprob+ComAdi+CodFte to xx 
	USE xDiaTmp IN 13
	***
	
	SELE HTadic
	DELE FOR ANO=VANO
	PACK
	vMes='01'
ENDIF

IF LASTKEY()#27 .AND. fverdad
	ACTIVATE WINDOW Wind_2
	xcolor = IIF(escolor, '', '*'+SET('COLOR'))
   
	SELE Cuen
	SET FILTER TO Cuen.Ajuste=.T.
	GO TOP

	* Se recorre todo el archivo del Plan de Cuentas Para Aquellas Cuentas que se Ajustan

	SELE FAcADic
	SEEK 'M'+vAno+vMes
	SCAN WHILE Tipo='M' AND Periodo=vAno
		SELE Cuen
		SET FILTER TO Cuen.Ajuste=.T.
		GO TOP
		SELE FacaDic
		vfactor = FacaDic.Factor
*		vfactor = FACTOR('M',vmes,vano)
		IF Periodo#vAno
			EXIT
		ENDIF
		vMes=FacAdic.Mes
		vAno=FacaDic.Periodo
		XMes=VAL(FacAdic.Mes)
		xAno=VAL(FacaDic.Periodo)
		SELE SalN
		SET FILTER TO (mes = Xmes ) AND ano = xano
		****
		SELE Cuen
		GO TOP
		SCAN
			xCta=cuenta()
			SELE SalN
			SEEK Cuen.Cuenta
			IF FOUND() AND mes=Xmes  AND ano=xano
				GenNCO = .T.
	    		IF SalN.Mes=XMES 
					STORE 0 TO vHajuste,vDajuste
					xMs=PADL(cMES(SALN.MES),9,' ')
    				@ 0,10 SAY "Saldos de ...."+xMs
    				@ 1,10 SAY "Actualizando Saldos Acumulados ...."+Cuen.Cuenta
					*para quitar el asiento de apertura
					SELE xDiaTmp
					SUM MONTOS TO mdebe  FOR tipo='D' AND cuenta=xcta
					SUM MONTOS TO mhaber FOR tipo='H' AND cuenta=xcta 
					SELE SalN
					IF xMes=1
			   			xxDAjuMes = (SalN.DSUMANA -mdebe) *vfactor-(SalN.DSUMANA-mdebe)
						xxHAjuMes = (SalN.HSUMANA -mHaber)*vfactor-(SalN.HSUMANA-mhaber)
						xxDAjuAno = (SalN.DSALDONA-mdebe) *vfactor-(SalN.DSALDONA-mdebe)
						xxHAjuAno = (SalN.HSALDONA-mhaber)*vfactor-(SalN.HSALDONA-mhaber)
					 ELSE
				   		xxDAjuMes = ((SalN.DSUMANA *vfactor)-SalN.DSUMANA)
						xxHAjuMes = ((SalN.HSUMANA *vfactor)-SalN.HSUMANA)
						xxDAjuAno = (SalN.DSALDONA - mDebe) *vfactor -(SalN.DSALDONA-mDebe)
        	 			xxHAjuAno = (SalN.HSALDONA - mHaber)*vfactor -(SalN.HSALDONA-mHaber)
					ENDIF
	  				vHajuste = vHajuste + xxHAjuMes
	  				vDajuste = vDajuste + xxDAjuMes
				ENDIF
			 ELSE
				GenNCO  = .F.
			ENDIF
			
			IF Cuen.Nivel>=2 AND GenNCO
				SELE HTaDic  
				SEEK Cuen.Cuenta+vmes+vano
				IF FOUND()
					IF xMes=1
						SaldoMov = (SalN.Dsumana-mDebe) - (SalN.Hsumana-mHaber)
					 ELSE
						SaldoMov = (SalN.Dsumana) - (SalN.Hsumana)
					ENDIF
*					SaldoMov = SalN.Dsumana - SalN.Hsumana
					*--------------------------------------
					
					REPLACE Cuenta     WITH Cuen.Cuenta
					REPLACE NumMes     WITH vmes
					REPLACE ano        WITH vano
					REPLACE factor     WITH vfactor
					REPLACE CuenAju    WITH SUBSTR(Cuen.Cuenta,1,2)+'8'+'0000000'
					REPLACE SaldoMo    WITH SaldoMov
					REPLACE Sumaju     WITH SaldoMov*vfactor
					REPLACE Ajumes     WITH (SaldoMov*vfactor) - (SaldoMov)
					REPLACE CuenAux    WITH '8980000000'
					REPLACE TipCuenA   WITH IIF(Saldomov>0,'H','D')
				 ELSE
					IF xMes=1
						SaldoMov = (SalN.Dsumana-mDebe) - (SalN.Hsumana-mHaber)
					 ELSE
						SaldoMov = (SalN.Dsumana) - (SalN.Hsumana)
					ENDIF
*					SaldoMov = SalN.Dsumana - SalN.Hsumana
					*-----------------------------------------
				
					IF F_APPD()
						REPLACE Cuenta     WITH Cuen.Cuenta
						REPLACE NumMes     WITH vmes
						REPLACE ano        WITH vano
						REPLACE factor     WITH vfactor
						REPLACE CuenAju    WITH SUBSTR(Cuen.Cuenta,1,2)+'8'+'0000000'
						REPLACE SaldoMo    WITH SaldoMov
						REPLACE Sumaju     WITH SaldoMov*vfactor
						REPLACE Ajumes     WITH (SaldoMov*vfactor) - (SaldoMov)
						REPLACE CuenAux    WITH '8980000000'				
						REPLACE TipCuenA   WITH IIF(Saldomov>0,'H','D')
					ENDIF
				ENDIF
			ENDIF
	     	@ 1,10 SAY "Leyendo ..........................."+Cuen.Cuenta
			DO NCDIC
			SELECT Cuen
			vprimera=.f.	
		ENDSCAN
		SELE FACADIC
	ENDSCAN
	
	* Aqui se graban los datos para hacer el reporte de la Hoja de Trabajo
	SET FILTER TO 
		
	SET CONSOLE ON
	SET BELL TO 800,5
	_WRAP = .F.
	? CHR(7)+CHR(7)+CHR(7)
	_WRAP = .T.
	SET BELL TO 512,2
	SET CONSOLE OFF
*	SET STEP ON
ENDIF   

IF LASTKEY()#27 and fverdad 
	DO ActDia
ENDIF	
CLOSE DATA
*ERASE xDia
*ERASE xDiatmp
ERASE VTEMPO
RETURN



PROCEDURE Fin_opcion
*-------------------
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_2
RELEASE    MENU   mMenu
SET DECIMAL TO 2
CLOSE DATABASE
*ERASE xDiaTmp.dbf
RETURN


FUNCTION FACTOR
*---------------
PARAMETER  tip,zmes,zano
SELE FacaDic
SEEK tip+zANO+zMES
IF FOUND()
   Vfactor=FacAdic.Factor
ELSE
   DO STANDBY WITH ' Factor del mes no Registrado....!, verifique su Tabla de Factores'
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
SELE DIAR1
INDEX ON COMPROB+CUENTA TO XX
*SET ORDE TO DIARIO2
REPLACE FecEmi WITH vFecha ALL
REPLACE ComAdi WITH STR(MONT(vFecha),2) ALL
COPY STRU TO vtempo
use vtempo in 0
SET FILTER TO COMPROB='64' AND MONTH(FECEMI)=VAL(VMES)
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
	SELECT DIAR1
ENDSCAN
SELE VTEMPO
GO TOP
SCATTER MEMVAR
DELETE FOR CUENTA='898'
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
ENDFOR
SELE Diar1
DELETE FOR ALLTRIM(COMPROB)='6409999'+vMes AND MONTH(FECEMI)=VAL(VMES)
SELE DIAR
APPEND FROM xDia
*IF VAL(VMES)<12
   APPEND FROM vtempo
*ENDIF   

sele comp
seek '6409999'+'12'+'     '
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


PROCEDURE NCDIC
*notas de contabilidad
*---------------------------

XM=VMES
*vMes='12'
*GenNCO=.T.
IF GenNCO					&& Genera la nota de contabilidad en DIARIO TEMPORAL
	IF vprimera
		SELECT Comp
		vComprob = '640999912'+'     '
*		vComprob = '6409999'+vMes+'     '
		SEEK vComprob+'12'
		IF FOUND()
			DELE NEXT 1
			SELE Diar
   			SEEK vcomprob+'12'+'09'
       		SCAN WHILE ALLTRIM(Diar.Comprob)+Diar.Comadi+Diar.codfte = ALLTRIM(vcomprob)+'12'+'09'
         		IF !f_lock(1)
   	       			UNLOCK ALL
       	   			RETURN
       			ENDIF
       			SELECT Diar
       			IF RLOCK()
     				DELETE NEXT 1
       			ENDIF
       			SELECT Diar
       			@ 1,10 SAY "Actualizando Diario y Compro .........."+STR(Diar.Item,4)
   			ENDSCAN
		ENDIF
		SELECT Comp
		IF F_APPD()
   			REPLACE TipDoc     WITH '64',;
      			Comprob    WITH vComprob,;
           		Comadi     WITH '12',; 
          		Fecha      WITH vfecha,;
   	      		Glosa1     WITH 'NOTA DE CONTABILIDAD QUE CORRESPONDE AL MES DE DICIEMBRE',;
       	  		Glosa3     WITH 'POR SALDOS DE AJUSTE POR CORRECCION MONETARIA DE ENERO A DICIEMBRE' ,;
       			CodFte     WITH '09',;
       			nummesfte  WITH '12',;
          		periodofte WITH vAno,;
          		numdocfte  WITH '9999',;
   	      		Nummesref  WITH '12',;
       	  		Docref     WITH '9999',;
       			Tipdcr     WITH '64',;
       			EstCon     WITH '55'
	
**      	       			CodCal     WITH vAno+vMes+'RDR'
		ENDIF
		vPrimera=.F.
	ENDIF  && DE vprimera
	SELECT Diar1
	IF Cuen.Nivel = 2 and vDAjuste#vHajuste
		IF vDAjuste#vHajuste
*		IF vDajuste > 0
			IF F_APPD()
				REPLACE Comprob    WITH vcomprob,;
                       Comadi     WITH vMes,; 
                       Fecemi     WITH vfecha,;
                       NumMov     WITH 1,;
                       Item       WITH vItem,;
                       Cuenta     WITH SUBSTR(Cuen.Cuenta,1,2)+'8'+'0000000',;
                       TipAux     WITH '10',;
                       Registro   WITH '001025',;
                     	CodFte     WITH '09',;
                       Docref     WITH '9999',;
                       Tipdcr     WITH '64',;
                       EstCon     WITH '55'
	            REPLACE Diar1.Tipo WITH IIF(vDAjuste>vHAjuste,'D','H')
    	        REPLACE Diar1.Montos    WITH IIF(vDAjuste>vHAjuste,vDajuste-vHajuste,vHajuste-vDajuste)
                xtotdeb=xtotdeb+vDajuste
         		vitem=vitem+1
			ENDIF
		ENDIF
		IF vDAjuste#vHajuste
*		IF vHDajuste > 0
			IF F_APPD()
	           	REPLACE Comprob   WITH vcomprob,;
                       Comadi     WITH vMes,; 
                       Fecemi     WITH vfecha,;
                       NumMov     WITH 1,;
                       Item       WITH vItem,;
                       Cuenta     WITH '8980000000',;
	                   TipAux     WITH '10',;
                       Registro   WITH '001025',;
                       CodFte     WITH '09',;
                       Docref     WITH '9999',;
                       Tipdcr     WITH '64',;
                       EstCon     WITH '55'
	            REPLACE Diar1.Tipo      WITH IIF(vDAjuste>vHAjuste,'H','D')
                REPLACE Diar1.Montos    WITH ABS(vDajuste-vHajuste)
                xtotHab=xtotHab+vHajuste
				vitem=vitem+1
			ENDIF
		ENDIF
	ENDIF
ENDIF
*vMes=xm
RETURN

*		GenNCO = .T.
*		IF GenNCO
*			Sele Comp
*			SEEK vcomprob+vMes
*			IF RLOCK()
*				wtotdeb=xtotdeb
*				wtothab=xtothab
*				REPLACE Comp.totdeb     WITH IIF(wtotdeb>wtothab,xtotdeb,xtotdeb+ABS(wtotdeb-wtothab))
*				REPLACE Comp.tothab     WITH IIF(wtothab>wtotdeb,xtothab,xtothab+ABS(wtotdeb-wtothab))
*				vItem  =1
*			ENDIF
*			Sele Diar
*			IF wtotdeb <> wtothab
*				IF f_appd()
*    		    	REPLACE Comprob       WITH vcomprob,;
*        	   		Comadi        WITH vMes,; 
*            	  	Fecemi        WITH vfecha,;
*		            NumMov        WITH 1,;
*    	            Item          WITH vItem,;
*     		        Cuenta        WITH "8980000000",;
*        		    TipAux        WITH '10',;
*           	    	Registro      WITH '001025',;
*	              	CodFte        WITH '09',;
*		            Docref        WITH '9999',;
*        	        Tipdcr        WITH '64',;
*     	    	    EstCon        WITH '55',;
*        	    	Diar.Tipo     WITH IIF(wtotdeb>wtothab,'H','D'),;
*	           	    Diar.Montos   WITH ABS(wtotdeb-wtothab)
*		     	ENDIF
*	   		ENDIF
*		ENDIF
*		SELE Comp
