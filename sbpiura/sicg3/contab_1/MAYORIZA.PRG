** ----------------------------------
** MAYORIZA.PRG
** Proceso de Mayorizaci¢n
** AUTOR : UDEP - 1995 FMC
** ----------------------------------
* Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08
Vmens01 = 'Proceso de Mayorizaci¢n'
Vmens02 = '  Valores de Mayorizaci¢n  '
Vmens05 = '  Procesando  '
Vmens06 = ' Fecha no valida; dato errado '
Vmens07 = 'Debe Mayorizar Cuenta corriente'
public vsedpli

=PopError('Aseg£rese que NADIE m s use el sistema para empezar el proceso de Mayorizaci¢n sin ning£n problema.',.F. )

USE Parmae   IN 6  ORDER TAG Parmae1    ALIAS parma
USE Compro   IN 1  ORDER TAG Compro1    ALIAS Comp EXCLUSIVE
USE Diario   IN 2  ORDER TAG Diario1    ALIAS Diar EXCLUSIVE
USE SalMesN  IN 3  ORDER TAG SalMesN1   ALIAS MesN EXCLUSIVE
USE Cuentas  IN 4  ORDER TAG cuentas1   ALIAS cuen
USE SaldoNa  IN 5  ORDER TAG SaldoNa1   ALIAS SalN EXCLUSIVE
USE SalCos   IN 7  ORDER TAG SalCos1    ALIAS SalC EXCLUSIVE

*- Inicia proceso
SELECT parma

SEEK 'OPERATGANANC'
vganan  = SUBSTR(Descri,1,10)
SEEK 'OPERATPERDID'
vperdi  = SUBSTR(Descri,1,10)
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
vactmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,3,2) )
SEEK 'OPERATACTIVO'
vtipact = LEFT(Descri,1)
SEEK 'OPERATPASIVO'
vtippas = LEFT(Descri,1)
vccostog= SPACE(LEN(Diar.CCosto))
vccostop= SPACE(LEN(Diar.CCosto))

USE
*USE CtoCto IN 8 ORDER TAG CtoCto1        ALIAS Ctoc
*SELECT Ctoc
*SCAN
*  FOR I = 1 TO 10
*      vccosto = ALLTRIM(SUBSTR(Ctoc.Cuentas,I*10-9,10))
*      IF EMPTY(vccosto)
*         LOOP
*      ENDIF
*      IF vganan = vccosto AND EMPTY(vccostog)
*         vccostog = Ctoc.CtoCto
*      ENDIF
*      IF vperdi = vccosto AND EMPTY(vccostop)
*         vccostop = Ctoc.CtoCto
*      ENDIF
*  ENDFOR
*  IF !EMPTY(vccostog) AND !EMPTY(vccostop)
*     EXIT
*  ENDIF
*ENDSCAN
*USE

USE Parmae   IN 10  ORDER TAG Parmae1                       ALIAS parma

PUBLIC vProg
vfecha = vactmes + DAY(DATE()) - 1
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
ACTIVATE WINDOW Wind_0

ACTIVATE WINDOW Wind_1
vsedpli='Sede   '
xmens = ''
@ 1,1  SAY '         Fecha :' GET vfecha   VALID ConsFecha() ERROR xmens
@ 3,1  SAY 'Mayoriza (S/N) :' GET vmayor   PICTURE '@M No,Si'
READ

vactmes=vfecha
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
   ACTIVATE WINDOW Wind_2
   
*bloquea el sistema en el ingreso de datos

=blo_sis(2)   

* Limpia los archivos de saldos mensuales
  SELECT MesN
  ZAP

*  Borra del archivo de saldos por centro de costos, los saldos del mes
*  SELECT SalC
   vmes = VAL(IIF(LEN(ALLTRIM(STR(MONTH(vfecha),2)))=1,'0','')+ALLTRIM(STR(MONTH(vfecha))))
   vano = VAL(SUBSTR(STR(YEAR(vfecha),4),3,2))
*  DELETE FOR Mes = vmes .AND. Ano = vano
*  PACK

  USE SalAux IN 11 ORDER TAG SalAux1              ALIAS SalA EXCLUSIVE
  SELECT SalA
  DELETE FOR Mes = vmes .AND. Ano = vano
  PACK
  USE

  USE Diario IN 2 ORDER TAG Diario1               ALIAS Diar EXCLUSIVE
  SELECT Diar
  * PACK
  xcolor = IIF(escolor, '', '*'+SET('COLOR'))
  

  @ 1,20 SAY 'Acumula Saldos ............'  COLOR &xcolor
  DO Acum

  * @ 1,20 SAY 'Saldos por C.Costo ........'  COLOR &xcolor
  * DO Costo

  @ 1,20 SAY 'Saldos por Auxiliar ...... '  COLOR &xcolor
  DO SalAux
  
  * @ 1,20 SAY 'Saldos por Obra     ...... '  COLOR &xcolor
  * DO SalObr
  
  @ 1,20 SAY 'Traspaso de saldos ........' COLOR &xcolor
  DO Transal
  
  * Actualiza par metros
  USE Parmae   IN 6  ORDER TAG Parmae1                       ALIAS parma
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
  SET BELL TO 512,2
  SET CONSOLE OFF
ENDIF
DEACTIVATE WINDOW Wind_2
RETURN


PROCEDURE TRANSAL
*-----------------

 m.mes = MONTH(vactmes)
 m.ano = VAL(SUBSTR(STR(YEAR(vactmes),4),3,2))
 
 SELECT SalN
 GO TOP
 
 DELETE FOR SalN.Mes = m.Mes .AND. SalN.Ano = m.Ano
 GO TOP
 
 SELECT MesN
 GO TOP
 SCAN 
   SCATTER MEMVAR
   SELECT SalN
   SEEK m.Cuenta + STR(m.Mes,2) + STR(m.Ano,2)
   IF !FOUND()
      APPEND BLANK
   ENDIF
   GATHER MEMVAR
   @ 1,47 SAY Mesn.Cuenta
   SELECT MesN
ENDSCAN
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
*--------------------------------------------------------------
PRIVATE Cu,vXCu,vYQu
vOrd=ORDER()
vIdx1=SYS(3)+'.IDX'
STORE 1 TO Cu
STORE SPACE(10) TO vXCu,vYQu
xErdad=.F.
vErdad=.F.
nosuma=.t.
vXCu = SPACE(10)
vYQu = SPACE(10)

USE Compro IN 1  ORDER TAG Compro1               ALIAS Comp
USE Diario IN 2  ORDER TAG Diario3               ALIAS Diar
SELECT Diar
SET ORDER TO Diario1
INDEX ON CUENTA+DTOC(FECEMI) TO (vIdx1) FOR MONTH(Diar.FecEmi) = MONTH(vactmes);
         .AND. YEAR(Diar.FecEmi) = YEAR(vactmes) .AND. Estcon<>'55' 
GO TOP

DO WHILE !EOF()
  	SELECT Diar
  	STORE 0 TO vDSumaNa, vHSumaNa
  	m.Cuenta = Diar.Cuenta
  	@ 1,47 SAY m.Cuenta
  	SEEK m.Cuenta
  	
  	*******
  	IF SEEK(m.Cuenta,'Cuen')
  		IF Cuen.Detalle='N'
  			DO StandBy WITH "Cuenta no es de detalle. Corregir N/Cont. N§ Diar.Comprob"
  			RETURN
  		ENDIF
  	ELSE
  		DO StandBy WITH "Cuenta No registrada. Corrija N/Cont. N§" + Diar.Comprob
  		RETURN
  	ENDIF
  	*******
  	
  	SCAN WHILE m.Cuenta = Diar.Cuenta
       IF xErdad
          IF vFecha >= Diar.FecEmi
             vDSumaNa = vDSumaNa + IIF(Diar.Tipo = 'D',Diar.Montos,0)
             vHSumaNa = vHSumaNa + IIF(Diar.Tipo = 'H',Diar.Montos,0)
          ENDIF
       ELSE
          IF MONTH(vactmes) = MONTH(Diar.FecEmi) .AND. YEAR(vactmes) = YEAR(Diar.FecEmi)
             vDSumaNa = vDSumaNa + IIF(Diar.Tipo = 'D',Diar.Montos,0)
             vHSumaNa = vHSumaNa + IIF(Diar.Tipo = 'H',Diar.Montos,0)
          ENDIF
       ENDIF   
  	ENDSCAN
  	vmes = STR(MONTH(vfecha),2)
  	nohay=.f.
	IF VAL(vmes) = 1
	      nohay=.t.
	ELSE
    	nohay=.f.
	    vmes = STR(VAL(vmes)-1,2)
	ENDIF
	IF nohay=.t.
	     STORE 0 TO vDSaldoNa, vHSaldoNa
	ELSE
    	 vano = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))
	     SELECT SalN
         SEEK  m.Cuenta + vmes + vano
    	 vDSaldoNa = IIF(FOUND(),SalN.DSaldoNa,0)
	     vHSaldoNa = IIF(FOUND(),SalN.HSaldoNa,0)
 	ENDIF
  
    SELECT MesN
     
          
    IF SEEK(m.Cuenta)
        Bloquea = f_lock(1)
    ELSE
        Bloquea = f_appd()
    ENDIF
    IF Bloquea
        REPLACE Cuenta WITH m.Cuenta, DSumaNa WITH vDSumaNa,;
                HSumaNa WITH vHSumaNa,;
                DSaldoNa WITH IIF(vDSaldoNa + vDSumaNa > vHSaldoNa + vHSumaNa,;
                vDSaldoNa + vDSumaNa - VHSaldoNa - VHsumaNa,0),;
                HSaldoNa WITH IIF(vHSaldoNa + vHSumaNa > vDSaldoNa + vDSumaNa,;
                vHSaldoNa + vHSumaNa - VDSaldoNa - VDsumaNa,0)
    ENDIF
  
	SELECT MesN
  	vcuenta   = Mesn.Cuenta
  	vXCu      = Mesn.Cuenta
  	vDSumaNa  = Mesn.DSumaNa
  	vHSumaNa  = Mesn.HSumaNa
  	nosuma=.F.
    xnumero   = 10
    DO WHILE xnumero > 1
    	xnumero = xnumero - 1
        SELECT Cuen
        SEEK vcuenta
        DO CASE 
           CASE xnumero = 2 AND Cuen.Nivel>=2
             vXCu = SUBSTR(vCuenta,1,xnumero) + SPACE(10 - xnumero)
             vYQu = STRTRAN(vXCu,' ','0')
             vXCu = vYQu
             nosuma=.t.
           CASE xnumero = 3  AND Cuen.Nivel>=3
             vXCu = SUBSTR(vCuenta,1,xnumero) + SPACE(10 - xnumero)
             vYQu = STRTRAN(vXCu,' ','0')
             vXCu = vYQu
             nosuma=.t.             
           CASE xnumero = 5 AND Cuen.Nivel>=4
             vXCu = SUBSTR(vCuenta,1,xnumero) + SPACE(10 - xnumero)
             vYQu = STRTRAN(vXCu,' ','0')
             vXCu = vYQu
             nosuma=.t.             
           CASE  xnumero = 7 AND Cuen.Nivel=5
             vXCu = SUBSTR(vCuenta,1,xnumero) + SPACE(10 - xnumero)
             vYQu = STRTRAN(vXCu,' ','0')
             vXCu = vYQu
             nosuma=.t.            
	 	ENDCASE             
	  
     	IF nosuma
         	SELECT MesN
         	IF SEEK(vXCu)
            	Bloquea = F_lock(1)
         	ELSE
            	Bloquea = F_appd()
         	ENDIF
         	IF Bloquea
	         	IF VAL(vmes) = 1
		        	nohay=.t.
			 	ELSE
		   	    	nohay=.f.
			    	vmes = STR(VAL(vmes)-1,2)
			 	ENDIF
			  	IF nohay=.t.
			     	STORE 0 TO vDSaldoNa, vHSaldoNa
			  	ELSE
			     	vano = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))
			     	SELECT SalN
			     	SEEK vXCu + vmes + vano
			     	vDSaldoNa = IIF(FOUND(),SalN.DSaldoNa,0)
			     	vHSaldoNa = IIF(FOUND(),SalN.HSaldoNa,0)
			  	ENDIF
	          	SELECT MesN
	          	m.Cuenta   = vXCu
	    	    m.DSumaNa  = vDSumaNa + Mesn.DSumaNa
              	m.HSumaNa  = vHSumaNa + Mesn.HSumaNa
	          	m.DSaldoNa = IIF(vDSaldoNa + m.DSumaNa - vHSaldoNa - m.HSumaNa>0,;
    	                    vDSaldoNa + m.DSumaNa - m.HSumaNa - vHSaldoNa,0)
        	   	m.HSaldoNa = IIF(vHSaldoNa + m.HSumaNa - vDSaldoNa - m.DSumaNa>0,;
	                        vHSaldoNa + m.HSumaNA - vDSaldoNa - m.DSumaNa,0)
    	       	GATHER MEMVAR
         	ENDIF
     	ENDIF
     	nosuma=.f.
	ENDDO 
  	SELECT Diar
ENDDO  

vidx = SYS(3) + '.idx'
SELECT SalN
vmes = MONTH(vfecha)
IF vmes = 1
   vmes = 12
ELSE
   vmes = vmes-1
ENDIF
vano = IIF(vmes = 12, VAL(SUBSTR(STR(YEAR(vfecha)-1,4),3,2)),VAL(SUBSTR(STR(YEAR(vfecha),4),3,2)))
INDEX ON Cuenta FOR Mes = vmes .AND. Ano = vano  TO (vidx)
GO TOP
SCAN
  @ 1,47 SAY SalN.Cuenta
  SCATTER MEMVAR
  SELECT MesN
  IF !SEEK(SalN.Cuenta)
     SELECT Cuen
     SEEK SalN.Cuenta
     IF vmes # 12 .OR. (!Cuen.Tipo $ '5' .AND. vmes = 12)
        SELECT MesN
        IF f_appd()
           m.DSumaNa  = 0
           m.HSumaNa  = 0
           GATHER MEMVAR
        ENDIF
     ENDIF
  ENDIF
  SELECT SalN
ENDSCAN

SET INDEX TO
SET ORDER TO TAG SaldoNa1
ERASE (vidx)
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
SEEK vComprob
IF FOUND()
   INDEX ON CCosto + Cuenta TO (vidx) FOR !EMPTY(CCosto);
         .AND. vFecha>=Diar.FecEmi
ELSE
   INDEX ON CCosto + Cuenta TO (vidx) FOR !EMPTY(CCosto);
         .AND. MONTH(Diar.FecEmi) = MONTH(vactmes) .AND. YEAR(Diar.FecEmi) = YEAR(vactmes)  .AND. Estcon<>'55'  
         
         *AND IIF(alltrim(vsedpli)='Sede',SUBSTR(Diar.Cuenta,4,2)$'0102',IIF(alltrim(vsedpli)='Adm.Cen',SUBSTR(Diar.Cuenta,4,4)$'0101'  ,.T.))
ENDIF         
SELECT Diar
GO TOP

DO WHILE !EOF()
  @ 1,47 SAY Diar.CCosto
  xCosto    = Diar.CCosto
  xCuenta   = Diar.Cuenta           
  STORE 0 TO m.DSumaNA, m.HSumaNa
  SCAN WHILE xCosto = Diar.CCosto .AND. xCuenta = Diar.Cuenta
       m.DSumaNa = m.DSumaNa + IIF(Diar.Tipo = 'D',Diar.Montos,0)
       m.HSumaNa = m.HSumaNa + IIF(Diar.Tipo = 'H',Diar.Montos,0)
  ENDSCAN
  vmes = STR(MONTH(vfecha),2)
  IF VAL(vmes) = 1
     vmes = '12'
  ELSE
     vmes = STR(VAL(vmes)-1,2)
  ENDIF
  vano = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))
  STORE 0 TO vDSaldoNa, vHSaldoNa
  SELECT SalC
  IF SEEK(xCosto + xCuenta + vmes + vano) .AND. vmes # '12'
     vDSaldoNa = SalC.DSaldoNa
     vHSaldoNa = SalC.HSaldoNa
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
     m.DSaldoNa = IIF(vDSaldoNa + m.DSumaNa - vHSaldoNa - m.HSumaNa>0,;
                  vDSaldoNa + m.DSumaNa - m.HSumaNa - vHSaldoNa,0)
     m.HSaldoNa = IIF(vHSaldoNa + m.HSumaNa - vDSaldoNa - m.DSumaNa>0,;
                  vHSaldoNa + m.HSumaNA - vDSaldoNa - m.DSumaNa,0)
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
        IF SalC.DSaldoNa # 0 .OR. SalC.HSaldoNa # 0
           vReg = RECNO()
           IF !SEEK(vCtoCto + vCuenta + vmes1 + vano1)
              GO vReg
              SCATTER MEMVAR
              m.Mes = vmes1
              m.Ano = vano1
              STORE 0 TO m.DSumaNa, m.HSumaNa
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



PROCEDURE SalAux
*---------------
vTipAux   = SPACE(02)
vRegistro = SPACE(06)
vCuenta   = SPACE(10)
STORE 0 TO vDSaldona, vHSaldona      
VRec=0
vind   = SYS(3) + '.idx'
SELECT SalC
USE
USE Compro IN 1  ORDER TAG Compro7   ALIAS Comp 
USE Diario IN 2  ORDER TAG Diario4   ALIAS Diar
USE SalAux IN 10 ORDER TAG SalAux2   ALIAS SalA
USE Auxil  IN 11 ORDER TAG Auxil1    ALIAS Auxi


vmesAnt = STR(MONTH(vfecha),2)
IF VAL(vmesAnt) # 1
   vMesAnt = STR(VAL(vMesAnt) - 1,2)
ELSE
   vMesAnt = '12'
ENDIF
vanoAnt = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))

vmes = STR(MONTH(vfecha),2)
vano = SUBSTR(STR(YEAR(vfecha),4),3,2)

SELE SalA
SCATTER MEMVAR BLANK

SELECT Diar
INDEX ON Tipaux+Registro+Cuenta TO (vind) FOR MONTH(Diar.Fecemi) = MONTH(vfecha) AND YEAR(Diar.Fecemi)=YEAR(vfecha) .AND. Diar.Estcon<>'55'

* and IIF(alltrim(vsedpli)='Sede',SUBSTR(Diar.Cuenta,4,2)$'0102',IIF(alltrim(vsedpli)='Adm.Cen',SUBSTR(Diar.Cuenta,4,4)$'0101',.T.)) .AND. Diar.Estcon<>'55'
SET RELATION TO Comprob+ComAdi+Codfte INTO Comp
GO TOP

vprimera=.T.  
SCAN

   IF EMPTY(Diar.TipAux) .OR. EMPTY(Diar.Registro)
      SKIP
      LOOP
   ENDIF
   
   @ 1,47 SAY Diar.Registro
   xTipAux   = Diar.TipAux
   xRegistro = Diar.Registro
   * A Nivel de Divisionaria
   xCuenta   = SUBSTR(Diar.Cuenta,1,3)+'0000000'
   
   IF (vregistro=xRegistro AND vTipaux=xTipAux AND vCuenta = xCuenta ) .OR. vprimera=.T.  
		 SELE Diar
	      m.DSumana=m.DSumana+IIF(Diar.Tipo='D',Diar.Montos,0)
    	  m.HSumana=m.HSumana+IIF(Diar.Tipo='H',Diar.Montos,0)
   ELSE   
   * Se saca el Saldo del Mes Anterior
   * para acumular al saldo correspondiente 
   * del mes 
   
   SELECT SalA
   IF SEEK(vTipAux + vRegistro + vMesAnt + vAnoAnt+ vCuenta)
      vDSaldoNa = SalA.DSaldoNa
      vHSaldoNa = SalA.HSaldoNa
   ENDIF
   
   SELECT SalA
   IF SEEK(vTipAux + vRegistro + vAno + vMes+ vCuenta)
      Bloquea = f_lock(1)
   ELSE
      Bloquea = f_appd()
   ENDIF
   m.Mes = VAL(vmes)
   m.Ano = VAL(vano)
   IF Bloquea
      m.Tipo     = vTipAux
      m.Codigo   = vRegistro
      m.Cuenta   = vCuenta
      m.DSaldoNa = IIF(vDSaldoNa + m.DSumaNa - vHSaldoNa - m.HSumaNa>0,;
                  vDSaldoNa + m.DSumaNa - m.HSumaNa - vHSaldoNa,0)
      m.HSaldoNa = IIF(vHSaldoNa + m.HSumaNa - vDSaldoNa - m.DSumaNa>0,;
                  vHSaldoNa + m.HSumaNA - vDSaldoNa - m.DSumaNa,0)
      GATHER MEMVAR
   ENDIF
      STORE 0 TO m.DSumana, m.HSumana
      STORE 0 TO vDSaldona, vHSaldona      
      * En el Cambio de Tipo de Auxiliar
      m.DSumana=m.DSumana+IIF(Diar.Tipo='D',Diar.Montos,0)
      m.HSumana=m.HSumana+IIF(Diar.Tipo='H',Diar.Montos,0)      
   ENDIF
   vTipAux   = Diar.TipAux
   vRegistro = Diar.Registro
   vCuenta   = SUBSTR(Diar.Cuenta,1,3)+'0000000'
   vprimera=.F.  
   SELECT Diar
ENDSCAN

*---------------------------------------------------------
* Agregamos los del salA sin 
* movimiento DEL MES; DEBE PASAR EL SALDO DEL MES ANTERIOR
*-----------------------------------------------------------
SET INDEX TO 
ERASE (vind)

vind   = SYS(3) + '.idx'
SELECT Diar
INDEX ON Tipaux+Registro TO (vind) FOR MONTH(Diar.Fecemi) = MONTH(vfecha) AND YEAR(Diar.Fecemi)=YEAR(vfecha) UNIQUE

SELE SalA
DELE ALL FOR (SalA.MES = MONTH(vfecha).AND.SalA.ANO=YEAR(vfecha)-1900) AND (SalA.Dsumana=0.AND.SalA.Hsumana=0)
GO TOP

SELE SalA
SET FILTER TO  SalA.mes=VAL(vMesAnt) AND SalA.Ano=VAL(vAnoAnt)
GO TOP
SCAN WHILE SalA.mes=VAL(vMesAnt) AND SalA.Ano=VAL(vAnoAnt)
   SELE Diar
   IF !SEEK (SalA.tipo+SalA.Codigo)
      @ 1,47 SAY SalA.tipo+'-'+SalA.Codigo
      SELE SalA
      SCATTER MEMVAR 
      vrec=recno()
      * pasan al nuevo mes sin movimiento
      m.mes=MONTH(vfecha)
      m.ano=YEAR(vfecha)-1900
      m.Dsumana=0
      m.Hsumana=0
      APPEND BLANK
      GATHER MEMVAR
      GO (vrec)
   ENDIF
   SELE SalA
ENDSCAN
SELE DIAR
SET INDEX TO 
ERASE (vind)
SELE SalA
SET ORDER TO TAG SalAux1
RETURN

PROCEDURE SalObr
*---------------
vTipObr   = SPACE(01)
vCodobra  = SPACE(04)
vCuenta   = SPACE(10)
xTipObr   = SPACE(01)
xCodobra  = SPACE(04)
xCuenta   = SPACE(10)
vind = SYS(3) + '.idx'
STORE 0 TO vDSaldona, vHSaldona      
USE
USE Compro IN 1  ORDER TAG Compro7   ALIAS Comp 
USE Diario IN 2  ORDER TAG Diario11  ALIAS Diar
USE SalObr IN 10 ORDER TAG SalObr1   ALIAS SalO

SELECT Comp
IF EOF()
   USE
   RETURN
ENDIF

vmesAnt = STR(MONTH(vfecha),2)

IF VAL(vmesAnt) # 1
   vMesAnt = STR(VAL(vMesAnt) - 1,2)
ELSE
   vMesAnt = '12'
ENDIF
vanoAnt = IIF(MONTH(vfecha) = 1, SUBSTR(STR(YEAR(vfecha)-1,4),3,2),SUBSTR(STR(YEAR(vfecha),4),3,2))

vmes = STR(MONTH(vfecha),2)
vano = SUBSTR(STR(YEAR(vfecha),4),3,2)

SELE SalO
SCATTER MEMVAR BLANK

SELECT Diar
INDEX ON Tipobr+Codobra+Cuenta TO (vind) FOR MONTH(Diar.Fecemi) = MONTH(vfecha) AND YEAR(Diar.Fecemi)=YEAR(vfecha) ;
AND (SUBSTR(Cuenta,1,2)='34' OR SUBSTR(Cuenta,1,2)='37' OR  SUBSTR(Cuenta,1,3)='337' OR SUBSTR(Cuenta,1,3)='332');
AND TipObr<>" " AND CodObra<>"    "

*INDEX ON Tipobr+Codobra+Cuenta TO (vind) FOR MONTH(Diar.Fecemi) = MONTH(vfecha) AND YEAR(Diar.Fecemi)=YEAR(vfecha) ;
*AND cta_obra(Diar.cuenta) AND !EMPTY(TipObr) AND !EMPTY(CodObra)


GO TOP
vprimera=.T.  
SCAN

   @ 1,47 SAY Diar.Codobra
   xTipobr  = Diar.Tipobr
   xCodobra = Diar.Codobra
   xCuenta  = Diar.Cuenta
   
   *------------------------------------
   * Se guarda a Nivel de Divisionaria 
   *------------------------------------
   
   IF (vCodobra = xCodobra AND vTipobr = xTipObr AND LEFT(vCuenta,3)=LEFT(xCuenta,3)) .OR. vprimera=.T.  
       SELE Diar
	   m.DSumana=m.DSumana+IIF(Diar.Tipo='D',Diar.Montos,0)
       m.HSumana=m.HSumana+IIF(Diar.Tipo='H',Diar.Montos,0)
   ELSE   
   
	   * Se saca el Saldo del Mes Anterior
	   * para acumular al saldo correspondiente 
	   * del mes 
   
	   SELECT SalO
	   IF SEEK(vTipObr + vCodobra + vMesAnt + vAnoAnt+ LEFT(vCuenta,3)+'0000000')
	      vDSaldoNa = SalO.DSaldoNa
	      vHSaldoNa = SalO.HSaldoNa
	   ENDIF
   
	   SELECT SalO
	   IF SEEK(vTipObr + vCodobra + vAno + vMes + LEFT(vCuenta,3)+'0000000')
	      Bloquea = f_lock(1)
	   ELSE
    	  Bloquea = f_appd()
	   ENDIF
	   
	   m.Mes = VAL(vmes)
	   m.Ano = VAL(vano)
   
	   IF Bloquea
    	  m.Tipobr   = vTipobr
	      m.Codobra  = vCodobra
    	  m.Cuenta   = LEFT(vCuenta,3)+'0000000'
	      m.DSaldoNa = IIF(vDSaldoNa + m.DSumaNa - vHSaldoNa - m.HSumaNa>0,;
    	              vDSaldoNa + m.DSumaNa - m.HSumaNa - vHSaldoNa,0)
	      m.HSaldoNa = IIF(vHSaldoNa + m.HSumaNa - vDSaldoNa - m.DSumaNa>0,;
    	              vHSaldoNa + m.HSumaNA - vDSaldoNa - m.DSumaNa,0)
	      m.Codcal   = Comp.Codcal            
    	  GATHER MEMVAR
	   ENDIF
   
      STORE 0 TO m.DSumana, m.HSumana
      STORE 0 TO vDSaldona, vHSaldona      
      * En el Cambio obra 
      m.DSumana=m.DSumana+IIF(Diar.Tipo='D',Diar.Montos,0)
      m.HSumana=m.HSumana+IIF(Diar.Tipo='H',Diar.Montos,0)      
   ENDIF
   
   vTipobr   = Diar.TipObr
   vCodobra  = Diar.Codobra
   vCuenta   = Diar.Cuenta
   vprimera=.F.  
   SELECT Diar
ENDSCAN
SELE DIAR
SET INDEX TO
ERASE(vind)

*------------------------------------------------
* AHORA SE AGREGAN todas las obras 
* que no tubieron movimiento durante el mes 
*------------------------------------------------

vind = SYS(3) + '.idx'
SELECT Diar
INDEX ON Tipobr+Codobra TO (vind) FOR MONTH(Diar.Fecemi) = MONTH(vfecha) AND YEAR(Diar.Fecemi)=YEAR(vfecha);
AND (SUBSTR(Cuenta,1,2)='34' OR SUBSTR(Cuenta,1,2)='37' OR  SUBSTR(Cuenta,1,3)='337' OR SUBSTR(Cuenta,1,3)='332');
AND TipObr<>" " AND CodObra<>" "

SELE SalO
DELE ALL FOR (SalO.MES = MONTH(vfecha).AND.SalO.ANO=YEAR(vfecha)-1900) AND (SalO.Dsumana=0.AND.SalO.Hsumana=0)
GO TOP

SELE SalO
SET FILTER TO  SalO.mes=VAL(vMesAnt) AND SalO.Ano=VAL(vAnoAnt)
GO TOP

SCAN WHILE SalO.mes=VAL(vMesAnt) AND SalO.Ano=VAL(vAnoAnt)
   SELE Diar
   IF !SEEK (SalO.tipobr+Salo.Codobra)
      @ 1,47 SAY SalO.tipobr+'-'+SalO.CodObra 
      SELE SalO
      SCATTER MEMVAR 
      vrec=recno()
      * pasan al nuevo mes sin movimiento
      m.mes=MONTH(vfecha)
      m.ano=YEAR(vfecha)-1900
      
      m.Dsumana=0
      m.Hsumana=0
      APPEND BLANK
      GATHER MEMVAR
      GO (vrec)
   ENDIF
   SELE SalO
ENDSCAN

SELE DIAR
SET INDEX TO
ERASE(vind)
RETURN



FUNCTION XConsFecha
*-----------------
 IF MONTH(vfecha) # MONTH(vactmes) .OR. YEAR(vfecha) # YEAR(vactmes)
   xmens = Vmens06
   RETURN .F.
ENDIF
RETURN .T.

FUNCTION ConsFecha
*-----------------
 IF MONTH(vfecha) < MONTH(vactmes) .OR. YEAR(vfecha) < YEAR(vactmes)
   xmens = Vmens06
   RETURN .F.
ENDIF
RETURN .T.



PROCEDURE Fin_opcion
*-------------------
  =blo_sis(3)
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_2
  RELEASE    MENU   mMenu
  CLOSE DATABASE
  ERASE (vind)
RETURN

FUNCTION Rambito  && Para que en el reporte aparezca de que se trata.
*---------------
PARAMETER vTipo
RETURN IIF(alltrim(vTipo)='Sede','PROGRAMA : SEDE CENTRAL',IIF(alltrim(vTipo)='Adm.Cen','PROGRAMA : 01 ADMINISTRACION CENTRAL','PLIEGO 01: REGION GRAU'))


FUNCTION cta_obra
*----------------
PARAMETER x_cuenta
RETURN (SUBSTR(x_Cuenta,1,2)='34' OR SUBSTR(x_Cuenta,1,2)='37' OR  SUBSTR(x_Cuenta,1,3)='337' OR SUBSTR(x_Cuenta,1,3)='332')
