** --------------------------------------------
** MAYORIZA.PRG
** Proceso de Mayorizaci¢n Del Balance Constructivo
** para efectos de elaboraci¢n BCns.
** AUTOR : UDEP - Virna ALicia Vel squez Encalada
** ----------------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens05,Vmens06
PUBLIC  vajuste,vfactor,vdajumes,vhajumes,vdajuano,vhajuano
STORE 0 TO vdajumes,vhajumes,vdajuano,vhajuano
Vmens01 = 'Mayorizaci¢n del Constructivo '
Vmens02 = '  Valores de Mayorizaci¢n  '
Vmens05 = '  Procesando  '
Vmens06 = '  Fecha no Valida  '

USE Compro   IN 1  ORDER TAG Compro1                       ALIAS Comp EXCLUSIVE
USE Diario   IN 2  ORDER TAG Diario1                       ALIAS Diar EXCLUSIVE
USE SalMesN  IN 3  ORDER TAG SalMesN1                      ALIAS MesN EXCLUSIVE
USE Cuentas  IN 4  ORDER TAG cuentas1                      ALIAS cuen
USE SaldoNa  IN 5  ORDER TAG SaldoNa1                      ALIAS SalN EXCLUSIVE
USE Parmae   IN 6  ORDER TAG Parmae1                       ALIAS parma
*USE SalCos   IN 7  ORDER TAG SalCos1                       ALIAS SalC EXCLUSIVE
USE Factores IN 12 ORDER TAG Factores2                     ALIAS Factores

*- Inicia proceso
SELECT parma

SEEK 'OPERATGANANC'
vganan  = SUBSTR(Descri,1,10)
SEEK 'OPERATPERDID'
vperdi  = SUBSTR(Descri,1,10)
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
vactmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,1,4) )
SEEK 'OPERATACTIVO'
vtipact = LEFT(Descri,1)
SEEK 'OPERATPASIVO'
vtippas = LEFT(Descri,1)
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(SUBSTR(Descri,5,2))
AnoCerrado = VAL(SUBSTR(Descri,1,4))

vccostog= SPACE(LEN(Diar.CCosto))
vccostop= SPACE(LEN(Diar.CCosto))
USE

USE Parmae   IN 10  ORDER TAG Parmae1                       ALIAS parma

PUBLIC vProg
vfecha = LastDay(MONT(vactmes),YEAR(vactmes))
vtcact = 0
vtcpas = 0
vprog  = SPACE(5)

vmayor = 'No'
vind   = SYS(3) + '.idx'
Bloquea= .T.

DO Inicia                    && Define ventanas, t¡tulos
DO Pregunta
DO Proceso
DO Fin_opcion
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 14,23 TO 20,57 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 06,10 TO 09,70 ;
 TITLE Vmens05 COLOR SCHEME 10

 ACTIVATE SCREEN
 RETURN


PROCEDURE Pregunta
*-----------------
*ACTIVATE WINDOW Wind_0

*ACTIVATE WINDOW Wind_1
DEFINE WINDOW vmay  FROM 09,20 TO 16,60 SHADOW DOUBLE TITLE " Mayorizaci¢n B.Constructivo " COLOR SCHEME 5
ACTIVATE WINDOW vmay

xmens = ''
@ 1,1  SAY '         Fecha :' GET vfecha   VALID ConsFecha() ERROR xmens
*@ 3,1  SAY 'Mayoriza (S/N) :' GET vmayor   PICTURE '@M No,Si'
@ 04,05 GET OKCANCEL FUNCTION '*TH \!\<Procesa;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
IF okcancel=1
	vMayor = 'Si'
 ELSE
	VmAYOR = 'No'
ENDIF
vactmes=vfecha
RELEASE WINDOW vmay

RETURN


PROCEDURE Proceso
*----------------
PRIVATE vAl
vAl=SELECT()
SELE Comp
IF EMPTY(Comp.fecha) &&=MONTH(vactmes)
   SET CONSOLE ON
   SET BELL TO 800,5
   _WRAP = .F.
   ? CHR(7)+CHR(7)+CHR(7)
   _WRAP = .T.
   SET BELL TO 512,2
   SET CONSOLE OFF
   DEACTIVATE WINDOW Wind_1
   @12,30 SAY '!!! BDD Esta Vac¡a !!!'
   WAIT
   RETURN
ENDIF

SELECT (vAl)
IF vmayor = 'Si'
	SELECT parma
	IF !SEEK('OPERATMAYOR')
		DO Standby WITH 'No existe par metro de mayorizaci¢n'
		RETURN
	ENDIF
	IF !f_lock(1)
		RETURN
	ENDIF
	
	USE Diario IN 2 ORDER TAG Diario1               ALIAS Diar EXCLUSIVE
	SELECT Diar
	* PACK
	xcolor = IIF(escolor, '', '*'+SET('COLOR'))
	
*  @ 1,20 SAY 'Acumula Saldos ............'  COLOR &xcolor
	DO Acum

*  @ 1,20 SAY 'Saldos por C.Costo ........'  COLOR &xcolor
*  DO Costo

  
  * Actualiza par metros
  SELECT parma
  REPLACE Descri WITH 'Si'
  SEEK 'LISCONDIARI'
  IF f_lock(1)
     REPLACE Descri WITH 'No'
  ENDIF
  SEEK 'LISCONMAYOR'
  IF f_lock(1)
     REPLACE Descri WITH 'No'
  ENDIF
  SEEK 'LISCONCOMPR'
  IF F_LOCK(1)
     REPLACE Descri WITH 'No'
  ENDIF
  SEEK 'LISCONBALI'
  IF F_LOCK(1)
     REPLACE Descri WITH 'No'
  ENDIF
  SEEK 'LISCONBALII'
  IF F_LOCK(1)
     REPLACE Descri WITH 'No'
  ENDIF
  SEEK 'LISCONESTAD'
  IF F_LOCK(1)
     REPLACE Descri WITH 'No'
  ENDIF

  SET CONSOLE ON
  SET BELL TO 800,5
  _WRAP = .F.
  ? CHR(7)+CHR(7)+CHR(7)
  _WRAP = .T.
  SET BELL TO 512,2
  SET CONSOLE OFF
ENDIF
DEACTIVATE WINDOW Wind_2
RETURN

PROCEDURE Acum
*-------------
* Acumula los movimientos del mes de cada cuenta de detalle
* y calcula sus saldos.
* Tambi‚n, calcula los movimientos del mes de cada cuenta superior
* y calcula sus saldos.
* Este proceso lo realiza tanto para los saldos de las cuentas
* en moneda nacional como en d¢lares.
* Las cuentas de resultado no se acumulan para el a¤o siguiente.
PRIVATE Cu,vXCu,vYQu
vOrd=ORDER()
vIdx1=SYS(3)+'.IDX'
STORE 1 TO Cu
STORE SPACE(10) TO vXCu,vYQu
xErdad=.F.
vErdad=.F.
nosuma=.t.
vXCu = SPACE(15)
vYQu = SPACE(15)

SELE Factores

zano = STR(YEAR(vfecha),4)
zmes = PADL(MONTH(vfecha),2,'0')

*SEEK 'M'+zANO+zMES
*IF FOUND()
*   Vfactor=Factores.Factor
*ELSE
*   Vfactor=1
*ENDIF


IF MONT(vFecha)=1
	DO APERTURA
ENDIF

  *----------------------
*   @ 1,20 SAY 'Preparando Informaci¢n ............'  COLOR &xcolor
SELE MesN
ZAP
SELE SalN
DO Espera WITH 1,'Preparando Informaci¢n de Ajustes de '+cMes(MONTH(vfecha))
SET FILTER TO SalN.Mes=MONTH(vfecha) AND SALN.ano= YEAR(VFECHA)
GO TOP 
SCAN 
	SCATTER MEMVAR
	SELE MesN
	APPEND BLANK
	GATHER MEMVAR
	SELE SalN
ENDSCAN  
SET FILTER TO 

SELE MesN
SET RELATION TO Cuenta INTO Cuen
REPLACE Dsumaju   WITH 0 ALL 
REPLACE Hsumaju   WITH 0 ALL
REPLACE Dsaldoaju WITH 0 ALL
REPLACE Hsaldoaju WITH 0 ALL
*REPLACE Dsumaju   WITH IIF(Cuen.ajuste,DSumana,0) ALL 
*REPLACE Hsumaju   WITH IIF(Cuen.ajuste,HSumana,0) ALL
*REPLACE Dsaldoaju WITH IIF(Cuen.ajuste,DSaldona,0) ALL
*REPLACE Hsaldoaju WITH IIF(Cuen.ajuste,HSaldona,0) ALL
SET RELATION TO
*-------------------------

SELECT Diar
****** TENER EN CUENTA PARA PROX. AUCTUALIZAC.
INDEX ON Cuenta to ind001 FOR MONTH(fecemi)=MONTH(vfecha) AND EstCon='55' AND SUBSTR(Comprob,8,2)#'00'
******
*INDEX ON Cuenta to ind001 FOR MONTH(fecemi)=MONTH(vfecha) AND ALLT(TIPDCR)#'ZZ' AND ALLTRIM(Comprob)='93' AND SUBSTR(Comprob,8,2)#'00'

DO Espera WITH 2
Registros = _TALLY
aCTUAL = 0
DO Progreso WITH 1,'Procesando ...'
*SET ORDER TO Diario3
*SET FILTER TO MONTH(fecemi)=MONTH(vfecha) AND ALLT(TIPDCR)#'ZZ' AND LEFT(COMPROB,2)#'WW' AND LEFT(COMPROB,2)#'00'
GO TOP
* grabar solo movimientos, para esto ya se debe haber realizado
* el ajuste de las cuentas , o sea ya deben de existir en el diario
* las Not/Cont. '93'
SCAN
	Actual = actual + 1
	DO Progreso WITH 2,' ',Registros,Actual
	
  vcuenta=cuenta
  IF VCUENTA='20'
*  	SUSP
  ENDIF
  SELECT Cuen

  SEEK vcuenta
  vnivel  = nivel
  SELECT Diar
  SCATTER MEMVAR
  FOR I=2 TO VNIVEL
	 Vcuenta1=PADR(left(vcuenta,i*2),15,'0')
*    IF I>=2 AND I<5
*		Vcuenta1=PADR(left(vcuenta,(2*I)-1),10,'0')
*	 ELSE
*		Vcuenta1=PADR(left(vcuenta,i*2),10,'0')
*	ENDIF
	SELECT MesN
	SEEK vcuenta1
	
	if (SUBS(vcuenta1,1,3)='337' OR SUBS(vcuenta1,1,3)='424') and tipdoc='O/C'
	 else
		IF FOUND()
			IF LEFT(DIAR.COMPROB,2)='91'
				REPLACE DSaldore WITH DSaldore + IIF(m.tipo='D',montos,0)
  	 			REPLACE HSaldore WITH HSaldore + IIF(m.tipo='H',montos,0)
*				REPLACE DSumAju WITH DSaldore + IIF(m.tipo='D',montos,0)
*				REPLACE HSumAju WITH HSaldore + IIF(m.tipo='H',montos,0)
  	 			IF DIAR.TIPOBR='N'
  	 				REPLACE DSumaux WITH DSumaux + IIF(m.tipo='D',montos,0)
  	 				REPLACE HSumaux WITH HSumaux + IIF(m.tipo='H',montos,0)
  	 			ENDIF
  			ELSE
				REPLACE DSumAju WITH DSumAju + IIF(m.tipo='D',montos,0)
 				REPLACE HSumAju WITH HSumAju + IIF(m.tipo='H',montos,0)
  	 		ENDIF
		ELSE
  			IF f_appd()
				REPLACE cuenta  WITH vcuenta1
  				IF LEFT(DIAR.COMPROB,2)='QQ'
  					REPLACE DSaldore WITH IIF(m.tipo='D',montos,0)
  	 				REPLACE HSaldore WITH IIF(m.tipo='H',montos,0)
*					IF DIAR.TIPOBR='N'
*						REPLACE DSumaux WITH IIF(m.tipo='D',montos,0)
*						REPLACE HSumaux WITH IIF(m.tipo='H',montos,0)
*					ENDIF	
  				ELSE
  	   				REPLACE DSumAju WITH IIF(m.tipo='D',montos,0)
  	 				REPLACE HSumAju WITH IIF(m.tipo='H',montos,0)
  	 			ENDIF
			ENDIF
  		ENDIF	
  	endif
  ENDFOR			
  SELECT diar
ENDSCAN
DO progreso WITH 3
* realiza saldos acumulados.
SELE MESN

GO TOP
SCAN
 	IF DSumAju>HSumAju
 	 	REPLACE DSaldoAju WITH DSumAju-HSumAju
 	ELSE
 		REPLACE HSaldoAju WITH HSumAju-DSumAju
	ENDIF
ENDSCAN
* Realizar el cruce de MesN a Saldona
vmes=STR(MONTH(vfecha),2)
VANO=STR(year(vfecha),4)

* Busca el saldo del Mes Anterior
IF VAL(vMes)>0
  	zMes=STR(VAL(vMes)-1,2)
	zANO=vANO
ENDIF
DO Espera WITH 1, 'Procesando... '

SELECT MesN
GO TOP
SCAN
	SCATTER MEMVAR
	SELECT SalN
	
* Ya no se busca el saldo anterior si el mes es '01' porque ya estan incluidos en el saldo
* de apertura en Diario.
	
*	IF VAL(zmes)=0
*		SEEK m.cuenta+zmes+STR(VAL(Vano),2)  && Buscamos si es que ya se ha mayorizado
*		IF FOUND() && Si la encuentra se reemplazan
*			vdajumes = SalN.Dajumes
*			vHajumes = SalN.Hajumes
*			vdajuano = SalN.Dajuano
*			vHajuano = SalN.Hajuano
*			GATHER MEMVAR
*			REPLACE Mes       WITH VAL(zmes)
*			REPLACE Ano       WITH VAL(vano)
*			REPLACE DAjumes   WITH vDAjumes
*  			REPLACE HAjumes   WITH vHAjumes
*			REPLACE DAjuano   WITH vDAjuano
*  			REPLACE HAjuano   WITH vHAjuano
*			REPLACE DSumAju   WITH m.DSumAju
*  			REPLACE HSumAju   WITH m.HSumAju
*  			REPLACE DSaldoAju WITH m.DSaldoAju
*			REPLACE HSaldoAju WITH m.HSaldoAju
*  			REPLACE DSaldore  WITH m.DSaldore
*			REPLACE HSaldore  WITH m.HSaldore
* 			REPLACE DSumaux WITH m.DSumaux
* 			REPLACE HSumaux WITH m.HSumaux
*		ELSE
*			IF F_APPD()
*				GATHER MEMVAR
*				REPLACE Mes       WITH VAL(zmes)
*				REPLACE Ano       WITH VAL(vano)
*				REPLACE DSumAju   WITH m.DSumAju
*	   			REPLACE HSumAju   WITH m.HSumAju
*  				REPLACE DSaldoAju WITH m.DSaldoAju
*				REPLACE HSaldoAju WITH m.HSaldoAju
*	   			REPLACE DSaldore  WITH m.DSaldore
*				REPLACE HSaldore  WITH m.HSaldore
* 				REPLACE DSumaux WITH m.DSumaux
* 				REPLACE HSumaux WITH m.HSumaux
*			ENDIF
*		ENDIF
*	 ELSE    && guardar el saldo anterior si no es enero
		xDSaldoAju = 0
		xHSaldoAju = 0
		
*		IF cuenta='208'
*			susp
*		endif

		SEEK M.CUENTA+zmes+STR(VAL(zano),4)
		IF FOUND()
			* si lo encuentra guarda saldos del mes anterior..el saldo acumulado del mes
			* anterior
			xDSaldoAju = DSaldoAju
			xHSaldoAju = HSaldoAju
		 ELSE
			xDSaldoAju = 0
			xHSaldoAju = 0
		ENDIF
		
		* buscamos si encuentra con la fecha de mayorizacion
		* para chancar valores
		
		SEEK M.CUENTA+Vmes+STR(VAL(Vano),4)
		
		IF FOUND()
			vdajumes = SalN.Dajumes
			vHajumes = SalN.Hajumes
			vdajuano = SalN.Dajuano
			vHajuano = SalN.Hajuano
		
			REPLACE DAjumes   WITH vDAjumes
   			REPLACE HAjumes   WITH vHAjumes
			REPLACE DAjuano   WITH vDAjuano
   			REPLACE HAjuano   WITH vHAjuano
			REPLACE DSumAju   WITH m.DSumAju
   			REPLACE HSumAju   WITH m.HSumAju
   			REPLACE DSaldore  WITH m.DSaldore
			REPLACE HSaldore  WITH m.HSaldore
			REPLACE DSumaux   WITH m.DSumaux
			REPLACE HSumaux   WITH m.HSumaux
			
			*****
			REPLACE DSaldoAju WITH IIF(xDSaldoAju+DSumAju-xHSaldoAju-HSumAju>0, xDSaldoAju+DSumAju-xHSaldoAju-HSumAju, 0)
			REPLACE HSaldoAju WITH IIF(xHSaldoAju+HSumAju-xDSaldoAju-DSumAju>0, xHSaldoAju+HSumAju-xDSaldoAju-DSumAju, 0)
			*****
			
*			IF DSumAju>HSumAju
*				IF xDSaldoAju > 0 && Si el saldo anterior acumulado es deudor.
*					REPLACE DSaldoAju WITH (DSumAju-HSumAju) + xDSaldoAju
*					REPLACE HSaldoAju WITH 0
*				 ELSE
*					var  = xHSaldoAju - (DSumAju-HSumAju) && se resta el saldo acreedor con el mov. deudor.
*					IF xHSaldoAju > (DSumAju-HSumAju)
*						REPLACE HSaldoAju WITH var
*						REPLACE DSaldoAju WITH 0
*					 ELSE
*						REPLACE DSaldoAju WITH var*-1
*						REPLACE HSaldoAju WITH 0
*					ENDIF
*				ENDIF
*			 ELSE && si el saldo del movimiento es acreedor
*				IF xHSaldoAju > 0
*					REPLACE HSaldoAju WITH (HSumAju-DSumAju)+xHSaldoAju
*					REPLACE DSaldoAju WITH 0
*				 ELSE
*					var = xDSaldoAju - (HSumAju - DSumAju)
*					IF xDSaldoAju > (HSumAju - DSumAju)
*						REPLACE DSaldoAju WITH var
*						REPLACE HSaldoAju WITH 0
*					 ELSE
*					 	REPLACE HSaldoAju WITH var*-1
*					 	REPLACE DSaldoAju WITH 0
*					 ENDIF
*				ENDIF
*			ENDIF
		 ELSE && Si no la encuentra en Saln
			IF F_APPD()
				GATHER MEMVAR
				REPLACE MES       WITH VAL(vmes)
				REPLACE ANO       WITH VAL(vano)
				REPLACE DSumAju   WITH m.DSumAju
				REPLACE HSumAju   WITH m.HSumAju
				*****
				REPLACE DSaldoAju WITH IIF(xDSaldoAju+DSumAju-xHSaldoAju-HSumAju>0, xDSaldoAju+DSumAju-xHSaldoAju-HSumAju, 0)
				REPLACE HSaldoAju WITH IIF(xHSaldoAju+HSumAju-xDSaldoAju-DSumAju>0, xHSaldoAju+HSumAju-xDSaldoAju-DSumAju, 0)
				*****
			
*				IF DSumAju>HSumAju
*					IF xDSaldoAju > 0 && Si el saldo anterior acumulado es deudor.
*						REPLACE DSaldoAju WITH (DSumAju-HSumAju) + xDSaldoAju
*						REPLACE HSaldoAju WITH 0
*					 ELSE
*						var  = xHSaldoAju - (DSumAju-HSumAju) && se resta el saldo acreedor con el mov. deudor.
*						IF xHSaldoAju > (DSumAju-HSumAju)
*							REPLACE HSaldoAju WITH var
*							REPLACE DSaldoAju WITH 0
*						 ELSE
*							REPLACE DSaldoAju WITH var*-1
*							REPLACE HSaldoAju WITH 0
*						ENDIF
*					ENDIF
*				 ELSE && si el saldo es acreedor
*					IF xHSaldoAju > 0
*						REPLACE HSaldoAju WITH (HSumAju-DSumAju)+xHSaldoAju
*						REPLACE DSaldoAju WITH 0
*					 ELSE
*						var = xDSaldoAju - (HSumAju - DSumAju)
*						IF xDSaldoAju > (HSumAju - DSumAju)
*							REPLACE DSaldoAju WITH var
*							REPLACE HSaldoAju WITH 0
*						 ELSE
*							REPLACE HSaldoAju WITH var*-1
*							REPLACE DSaldoAju WITH 0
*						ENDIF
*					ENDIF
*				ENDIF
			ENDIF
		ENDIF
*	ENDIF
	SELECT MESN
ENDSCAN	
* Ahora se actualiza lo de SalN a MesN
*SELECT SalN
*SET FILTER TO Mes= VAL(vMes) AND Ano=VAL(vAno)
*GO TOP
*SCAN 
*	SCATTER MEMVAR
*	SELECT MesN
*	SEEK m.Cuenta 
*	IF !FOUND()
*		APPEND BLANK
*	ENDIF
*	GATHER MEMVAR
*	SELECT SalN
*ENDSCAN
*SELE SalN
*SET FILTER TO
SELE diar
*SET FILTER TO
DO Espera WITH 2
SET INDEX TO
ERASE ind001.idx
RETURN



PROCEDURE Costo
*--------------
* Se acumula por centro de costo, cuenta.

vidx = SYS(3) + '.idx'
USE Compro IN 1  ORDER TAG Compro1   ALIAS Comp
USE Diario IN 2                      ALIAS Diar
SELECT Diar
SET RELATION TO Comprob+ComAdi INTO Comp
vComprob='0100001'
SET ORDER TO Diario1
* SEEK vComprob
* IF FOUND()
*   INDEX ON CCosto + Cuenta TO (vidx) FOR !EMPTY(CCosto);
*         .AND. vFecha>=Diar.FecEmi
* ELSE
   INDEX ON CCosto + Cuenta TO (vidx) FOR !EMPTY(CCosto);
         .AND. MONTH(Diar.FecEmi) = MONTH(vactmes) .AND. YEAR(Diar.FecEmi) = YEAR(vactmes) 
*ENDIF         
SELECT Diar
GO TOP

DO WHILE !EOF()
  @ 1,47 SAY Diar.CCosto
  xCosto    = Diar.CCosto
  xCuenta   = Diar.Cuenta           
  STORE 0 TO m.Dsumaju, m.Hsumaju
  SCAN WHILE xCosto = Diar.CCosto .AND. xCuenta = Diar.Cuenta
       m.Dsumaju = m.Dsumaju + IIF(Diar.Tipo = 'D',Diar.Montos,0)
       m.Hsumaju = m.Hsumaju + IIF(Diar.Tipo = 'H',Diar.Montos,0)
  ENDSCAN
  vmes = STR(MONTH(vfecha),2)
  IF VAL(vmes) = 1
     vmes = '12'
  ELSE
     vmes = STR(VAL(vmes)-1,2)
  ENDIF
  vano = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))
  STORE 0 TO vDSaldoaju, vhSaldoaju
  SELECT SalC
  IF SEEK(xCosto + xCuenta + vmes + vano) .AND. vmes # '12'
     vDSaldoaju = SalC.DSaldoaju
     vhSaldoaju = SalC.hSaldoaju
  ENDIF
  vmes = STR(MONTH(vfecha),2)
  vano = SUBSTR(STR(YEAR(vfecha),4),3,2)
  SELECT SalC
  IF SEEK(xCosto + xCuenta + vmes + vano)
     Bloquea = f_lock(1)
  ELSE
     Bloquea = f_appd()
  ENDIF
  m.Mes = VAL(vmes)
  m.Ano = VAL(vano)
  IF Bloquea
     m.CtoCto   = xCosto
     m.Cuenta   = xCuenta
     m.DSaldoaju = IIF(vDSaldoaju + m.Dsumaju - vhSaldoaju - m.Hsumaju>0,;
                  vDSaldoaju + m.Dsumaju - m.Hsumaju - vhSaldoaju,0)
     m.hSaldoaju = IIF(vhSaldoaju + m.Hsumaju - vDSaldoaju - m.Dsumaju>0,;
                  vhSaldoaju + m.Hsumaju - vDSaldoaju - m.Dsumaju,0)
     GATHER MEMVAR
  ENDIF
  SELECT Diar
ENDDO
SELECT Diar
USE
ERASE (vidx)

vmes = STR(MONTH(vfecha),2)
IF VAL(vmes) = 1
   vmes = '12'
ELSE
   vmes = STR(VAL(vmes)-1,2)
ENDIF
vano = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))

IF vmes # '12'
   vmes1 = STR(MONTH(vfecha),2)
   vano1 = SUBSTR(STR(YEAR(vfecha),4),3,2)

   SELECT SalC
   INDEX ON CToCTo + Cuenta + STR(Mes,2) + STR(Ano,2) FOR (STR(Mes,2) = vmes .AND. STR(Ano,2) = vano);
         .OR. (STR(Mes,2) = vmes1 .AND. STR(Ano,2) = vano1) TO (vidx)
   GO TOP
   SCAN
     IF vmes1 # STR(Mes,2) .OR. vano1 # STR(Ano,2)
        @ 1,47 SAY SalC.CtoCto
        vCtoCto   = SalC.CtoCto
        vCuenta   = SalC.Cuenta
        IF SalC.DSaldoaju # 0 .OR. SalC.hSaldoaju # 0
           vReg = RECNO()
           IF !SEEK(vCtoCto + vCuenta + vmes1 + vano1)
              GO vReg
              SCATTER MEMVAR
              m.Mes = vmes1
              m.Ano = vano1
              STORE 0 TO m.Dsumaju, m.Hsumaju
              IF f_appd()
                 GATHER MEMVAR
              ENDIF
           ENDIF
           GO vReg
        ENDIF
     ENDIF
  ENDSCAN
  SET INDEX TO
  SET ORDER TO TAG SalCos1
  ERASE (vidx)
ENDIF
RETURN



FUNCTION ConsFecha
*-----------------
*IF MONTH(vfecha) # MONTH(vactmes) .OR. YEAR(vfecha) # YEAR(vactmes)
*	xmens = Vmens06
*	RETURN .F.
*ENDIF

IF MONTH(vfecha) < MONTH(vactmes) .OR. YEAR(vfecha) < YEAR(vactmes)
	xmens = Vmens06
	RETURN .F.
ENDIF

IF !((MONTH(vFecha)>MesCerrado AND YEAR(vFecha)=AnoCerrado) OR YEAR(vFecha)>AnoCerrado)
	xmens = "­­­ Dicho mes ya est  cerrado !!!" 
	RETURN .F.
ENDIF

RETURN .T.


PROCEDURE Fin_opcion
*-------------------
  SELECT parma
  SEEK 'OPERATCONSTR'
  REPLACE Parma.Descri WITH 'Si'
  
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_2
  RELEASE    MENU   mMenu
  CLOSE DATABASE
  ERASE (vind)
  RETURN

PROCEDURE Apertura
*-------------
* Acumula los movimientos del Asiento de Apertura de cada cuenta de detalle
* y calcula sus saldos.
* Tambi‚n, calcula los movimientos del mes de cada cuenta superior
* y calcula sus saldos.
* Este proceso lo realiza tanto para los saldos de las cuentas
* en moneda nacional como en d¢lares.
* Las cuentas de resultado no se acumulan para el a¤o siguiente.
PRIVATE Cu,vXCu,vYQu
vOrd=ORDER()
vIdx1=SYS(3)+'.IDX'
STORE 1 TO Cu
STORE SPACE(10) TO vXCu,vYQu
xErdad=.F.
vErdad=.F.
nosuma=.t.
vXCu = SPACE(15)
vYQu = SPACE(15)

  *----------------------
   DO Espera WITH 1,'Preparando Informaci¢n DE APERTURA..'
*   @ 1,20 SAY 'Preparando Informaci¢n DE APERTURA..'  COLOR &xcolor
   SELE MesN
   ZAP
   SELE SalN 
   SET FILTER TO SalN.Mes=0 AND SALN.ano= YEAR(VFECHA)
   GO TOP 
   SCAN 
     SCATTER MEMVAR
     SELE MesN
     APPEND BLANK
     GATHER MEMVAR
     SELE SalN
    ENDSCAN  
   SET FILTER TO 
   SELE MesN
   SET RELATION TO Cuenta INTO Cuen
   REPLACE Dsumaju   WITH 0 ALL 
   REPLACE Hsumaju   WITH 0 ALL
   REPLACE Dsaldoaju WITH 0 ALL
   REPLACE Hsaldoaju WITH 0 ALL
*   REPLACE Dsumaju   WITH IIF(Cuen.ajuste,DSumana,0) ALL 
*   REPLACE Hsumaju   WITH IIF(Cuen.ajuste,HSumana,0) ALL
*   REPLACE Dsaldoaju WITH IIF(Cuen.ajuste,DSaldona,0) ALL
*   REPLACE Hsaldoaju WITH IIF(Cuen.ajuste,HSaldona,0) ALL
   SET RELATION TO 
  *-------------------------


SELE Factores

zano = STR(YEAR(vfecha),4)
zmes = PADL(MONTH(vfecha)-1,2,'0')

*SEEK 'M'+zANO+zMES
*IF FOUND()
*   Vfactor=Factores.Factor
*ELSE
*   Vfactor=1
*ENDIF

SELECT Diar
INDEX ON cuenta to ind001 FOR MONTH(fecemi)=MONTH(vfecha) AND (SUBSTR(COMPROB,1,2)='93' AND SUBSTR(COMPROB,8,2)='00')

DO Espera WITH 2
DO Progreso WITH 1,'Procesando Apertura'
Registros = _TALLY
Actual = 0
*SET ORDER TO Diario3
*SET FILTER TO MONTH(fecemi)=MONTH(vfecha) AND LEFT(COMPROB,2)#'00'
GO TOP
* grabar solo movimientos, para esto ya se debe haber realizado
* el ajuste de las cuentas , o sea ya deben de existir en el diario
* las Not/Cont. '93'
SCAN
  actual = actual + 1
  DO Progreso WITH 2,'',Registros,Actual
  vcuenta=cuenta
  SELECT Cuen
  
  SEEK vcuenta
  vnivel  = nivel
  SELECT Diar
  SCATTER MEMVAR
  FOR I=2 TO VNIVEL
*    IF I>=2 AND I<5
*       	Vcuenta1=PADR(left(vcuenta,(2*I)-1),10,'0')
*    ELSE
    	Vcuenta1=PADR(left(vcuenta,i*2),15,'0')
*    ENDIF
  	SELECT MesN
	SEEK vcuenta1
	
  	if (SUBS(vcuenta1,1,3)='337' OR SUBS(vcuenta1,1,3)='424') and tipdoc='O/C'
  	else
  		IF FOUND()
  			IF LEFT(DIAR.COMPROB,2)='QQ'
  				REPLACE DSaldore WITH DSaldore + IIF(m.tipo='D',montos,0)
  	 			REPLACE HSaldore WITH HSaldore + IIF(m.tipo='H',montos,0)
  	 			IF DIAR.TIPOBR='N'
  	 				REPLACE DSumaux WITH DSumaux + IIF(m.tipo='D',montos,0)
  	 				REPLACE HSumaux WITH HSumaux + IIF(m.tipo='H',montos,0)
  	 			ENDIF
  			ELSE
				REPLACE DSumAju WITH DSumAju + IIF(m.tipo='D',montos,0)
 				REPLACE HSumAju WITH HSumAju + IIF(m.tipo='H',montos,0)
  	 		ENDIF
		ELSE
  			IF f_appd()
				REPLACE cuenta  WITH vcuenta1
  				IF LEFT(DIAR.COMPROB,2)='QQ'
  					REPLACE DSaldore WITH IIF(m.tipo='D',montos,0)
  	 				REPLACE HSaldore WITH IIF(m.tipo='H',montos,0)
  	 				IF DIAR.TIPOBR='N'
 	 					REPLACE DSumaux WITH IIF(m.tipo='D',montos,0)
  	 					REPLACE HSumaux WITH IIF(m.tipo='H',montos,0)
  	 				ENDIF	
  				ELSE
  	   				REPLACE DSumAju WITH IIF(m.tipo='D',montos,0)
  	 				REPLACE HSumAju WITH IIF(m.tipo='H',montos,0)
  	 			ENDIF
			ENDIF
  		ENDIF	
  	endif
  ENDFOR			
  SELECT diar
ENDSCAN
DO Progreso WITH 3
* realiza saldos acumulados.
SELE MESN

GO TOP
SCAN
 	IF DSumAju>HSumAju
 	 	REPLACE DSaldoAju WITH DSumAju-HSumAju
 	ELSE
 		REPLACE HSaldoAju WITH HSumAju-DSumAju
	ENDIF
ENDSCAN
* Realizar el cruce de MesN a Saldona
vmes=STR(MONTH(vfecha),2)
VANO=STR(year(vfecha),4)

* Busca el saldo del Mes Anterior
IF VAL(vMes)>0
  	zMes=STR(VAL(vMes)-1,2)
	zANO=vANO
ENDIF
DO ESPERA WITH 1, 'Procesando...'

SELECT MesN
GO TOP
SCAN
	SCATTER MEMVAR
	SELECT SalN
	
* Ya no se busca el saldo anterior si el mes es '01' porque ya estan incluidos en el saldo
* de apertura en Diario.

	IF VAL(zmes)=0
		SEEK m.cuenta+zmes+STR(VAL(Vano),4)  && Buscamos si es que ya se ha mayorizado
		IF FOUND() && Si la encuentra se reemplazan
			vdajumes = SalN.Dajumes
			vHajumes = SalN.Hajumes
			vdajuano = SalN.Dajuano
			vHajuano = SalN.Hajuano
			GATHER MEMVAR
			REPLACE Mes       WITH VAL(zmes)
			REPLACE Ano       WITH VAL(vano)
			REPLACE DAjumes   WITH vDAjumes
  			REPLACE HAjumes   WITH vHAjumes
			REPLACE DAjuano   WITH vDAjuano
  			REPLACE HAjuano   WITH vHAjuano
			REPLACE DSumAju   WITH m.DSumAju
  			REPLACE HSumAju   WITH m.HSumAju
  			REPLACE DSaldoAju WITH m.DSaldoAju
			REPLACE HSaldoAju WITH m.HSaldoAju
*  			REPLACE DSaldore  WITH m.DSaldore
*			REPLACE HSaldore  WITH m.HSaldore
 			REPLACE DSumaux WITH m.DSumaux
 			REPLACE HSumaux WITH m.HSumaux
		ELSE
			IF F_APPD()
				GATHER MEMVAR
				REPLACE Mes       WITH VAL(zmes)
				REPLACE Ano       WITH VAL(vano)
				REPLACE DSumAju   WITH m.DSumAju
	   			REPLACE HSumAju   WITH m.HSumAju
  				REPLACE DSaldoAju WITH m.DSaldoAju
				REPLACE HSaldoAju WITH m.HSaldoAju
*	   			REPLACE DSaldore  WITH m.DSaldore
*				REPLACE HSaldore  WITH m.HSaldore
				REPLACE DSumaux WITH m.DSumaux
 				REPLACE HSumaux WITH m.HSumaux
			ENDIF
		ENDIF
	 ELSE    && guardar el saldo anterior si no es enero
*		xDSaldoAju = 0
*		xHSaldoAju = 0
*		
*		SEEK M.CUENTA+zmes+zano
*		IF FOUND()
*			* si lo encuentra guarda saldos del mes anterior..el saldo acumulado del mes
*			* anterior
*			xDSaldoAju = DSaldoAju
*			xHSaldoAju = HSaldoAju
*		ENDIF
*		
*		* buscamos si encuentra con la fecha de mayorizacion
*		* para chancar valores
*		
*		SEEK M.CUENTA+Vmes+STR(VAL(Vano),2)
*		
*		IF FOUND()
*			vdajumes = SalN.Dajumes
*			vHajumes = SalN.Hajumes
*			vdajuano = SalN.Dajuano
*			vHajuano = SalN.Hajuano
*			
*			REPLACE DAjumes   WITH vDAjumes
*  			REPLACE HAjumes   WITH vHAjumes
*			REPLACE DAjuano   WITH vDAjuano
*  			REPLACE HAjuano   WITH vHAjuano
*			REPLACE DSumAju   WITH m.DSumAju
*  			REPLACE HSumAju   WITH m.HSumAju
*  			REPLACE DSaldore  WITH m.DSaldore
*			REPLACE HSaldore  WITH m.HSaldore
*			REPLACE DSumaux   WITH m.DSumaux
*			REPLACE HSumaux   WITH m.HSumaux
*			*****
*			REPLACE DSaldoAju WITH IIF(xDSaldoAju+DSumAju-xHSaldoAju-HSumAju>0, xDSaldoAju+DSumAju-xHSaldoAju-HSumAju, 0)
*			REPLACE HSaldoAju WITH IIF(xHSaldoAju+HSumAju-xDSaldoAju-DSumAju>0, xHSaldoAju+HSumAju-xDSaldoAju-DSumAju, 0)
*			*****
*			
*			IF DSumAju>HSumAju
*				IF xDSaldoAju > 0 && Si el saldo anterior acumulado es deudor.
*					REPLACE DSaldoAju WITH (DSumAju-HSumAju) + xDSaldoAju
*					REPLACE HSaldoAju WITH 0
*				 ELSE
*					var  = xHSaldoAju - (DSumAju-HSumAju) && se resta el saldo acreedor con el mov. deudor.
*					IF xHSaldoAju > (DSumAju-HSumAju)
*						REPLACE HSaldoAju WITH var
*						REPLACE DSaldoAju WITH 0
*					 ELSE
*						REPLACE DSaldoAju WITH var*-1
*						REPLACE HSaldoAju WITH 0
*					ENDIF
*				ENDIF
*			 ELSE && si el saldo del movimiento es acreedor
*				IF xHSaldoAju > 0
*					REPLACE HSaldoAju WITH (HSumAju-DSumAju)+xHSaldoAju
*					REPLACE DSaldoAju WITH 0
*				 ELSE
*					var = xDSaldoAju - (HSumAju - DSumAju)
*					IF xDSaldoAju > (HSumAju - DSumAju)
*						REPLACE DSaldoAju WITH var
*						REPLACE HSaldoAju WITH 0
*					 ELSE
*						REPLACE HSaldoAju WITH var*-1
*						REPLACE DSaldoAju WITH 0
*					ENDIF
*				ENDIF
*			ENDIF
*		 ELSE && Si no la encuentra en Saln
*			IF F_APPD()
*				GATHER MEMVAR
*				REPLACE MES       WITH VAL(vmes)
*				REPLACE ANO       WITH VAL(vano)
*				REPLACE DSumAju   WITH m.DSumAju
*  				REPLACE HSumAju   WITH m.HSumAju
*			
*				*****
*				REPLACE DSaldoAju WITH IIF(xDSaldoAju+DSumAju-xHSaldoAju-HSumAju>0, xDSaldoAju+DSumAju-xHSaldoAju-HSumAju, 0)
*				REPLACE HSaldoAju WITH IIF(xHSaldoAju+HSumAju-xDSaldoAju-DSumAju>0, xHSaldoAju+HSumAju-xDSaldoAju-DSumAju, 0)
*				*****
*  				
*				IF DSumAju>HSumAju
*  					IF xDSaldoAju > 0 && Si el saldo anterior acumulado es deudor.
*  						REPLACE DSaldoAju WITH (DSumAju-HSumAju) + xDSaldoAju
*  						REPLACE HSaldoAju WITH 0
*  					 ELSE
*  						var  = xHSaldoAju - (DSumAju-HSumAju) && se resta el saldo acreedor con el mov. deudor.
*  						IF xHSaldoAju > (DSumAju-HSumAju)
*  							REPLACE HSaldoAju WITH var
*  							REPLACE DSaldoAju WITH 0
*  						 ELSE
*  							REPLACE DSaldoAju WITH var*-1
*  							REPLACE HSaldoAju WITH 0
*  						ENDIF
*  					ENDIF
*  				 ELSE && si el saldo es acreedor
*  					IF xHSaldoAju > 0
*  						REPLACE HSaldoAju WITH (HSumAju-DSumAju)+xHSaldoAju
*  						REPLACE DSaldoAju WITH 0
*  					 ELSE
* 						var = xDSaldoAju - (HSumAju - DSumAju)
*  						IF xDSaldoAju > (HSumAju - DSumAju)
*  							REPLACE DSaldoAju WITH var
*  							REPLACE HSaldoAju WITH 0
*  						 ELSE
*  							REPLACE HSaldoAju WITH var*-1
*  							REPLACE DSaldoAju WITH 0
*  						ENDIF
*  					ENDIF
*  				ENDIF	
*  			ENDIF	
*		ENDIF
	ENDIF
	SELECT MESN
ENDSCAN	

DO Espera WITH 2
* Ahora se actualiza lo de SalN a MesN
*SELECT SalN
*SET FILTER TO Mes= VAL(vMes) AND Ano=VAL(vAno)
*GO TOP
*SCAN
*	SCATTER MEMVAR
*	SELECT MesN
*	SEEK m.Cuenta
*	IF !FOUND()
*		APPEND BLANK
*	ENDIF
*	GATHER MEMVAR
*	SELECT SalN
*ENDSCAN
*SELE SalN
*SET FILTER TO
SELE diar
SET INDEX TO
ERASE Ind001.IDX
*SET FILTER TO
RETURN

