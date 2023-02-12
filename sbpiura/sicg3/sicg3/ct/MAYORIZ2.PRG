** ----------------------------------
** MAYORIZ1.PRG
** Proceso de Mayorizaci¢n
** AUTOR : UDEP - 1995 FMC
** ----------------------------------
* Mensajes de aviso al usuario
PRIVATE Vmens06
Vmens06 = ' Fecha no valida; dato errado '

=PopError('Aseg£rese que NADIE m s use el sistema para empezar el proceso de Mayorizaci¢n sin ning£n problema.',.F. )

USE Parmae   IN 6  ORDER TAG Parmae1    ALIAS parma
USE Compro   IN 1  ORDER TAG Compro1    ALIAS Comp EXCLUSIVE
USE Diario   IN 2  ORDER TAG Diario3    ALIAS Diar EXCLUSIVE
USE SalMesN  IN 3  ORDER TAG SalMesN1   ALIAS MesN EXCLUSIVE
USE Cuentas  IN 4  ORDER TAG cuentas1   ALIAS cuen
USE SaldoNa  IN 5  ORDER TAG SaldoNa1   ALIAS SalN EXCLUSIVE

*- Inicia proceso
SELECT parma

SEEK 'OPERATGANANC'
vganan  = SUBSTR(Descri,1,10)
SEEK 'OPERATPERDID'
vperdi  = SUBSTR(Descri,1,10)
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
vactmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,1,4))
SEEK 'OPERATACTIVO'
vtipact = LEFT(Descri,1)
SEEK 'OPERATPASIVO'
vtippas = LEFT(Descri,1)
*vccostog= SPACE(LEN(Diar.CCosto))
*vccostop= SPACE(LEN(Diar.CCosto))

vfecha = LastDay(MONT(vactmes),YEAR(vactmes))
*vtcact = 0
*vtcpas = 0

*vmayor = 'No'
*vind   = SYS(3) + '.idx'
*Bloquea= .T.
*AP=.F.
DO Pregunta
DO Proceso
DO Fin_opcion

RETURN


PROCEDURE Pregunta
*-----------------

DEFINE WINDOW vmay  FROM 09,20 TO 16,60 SHADOW DOUBLE TITLE " Proceso de Mayorizaci¢n* " COLOR SCHEME 5
ACTIVATE WINDOW vmay

xmens = ''
@ 1,1  SAY '         Fecha :' GET vfecha   VALID ConsFecha() ERROR xmens
@ 04,05 GET OKCANCEL FUNCTION '*TH \!\<Procesa;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW vmay
vactmes=vfecha

IF okcancel=1 OR LASTKEY()#27
	DO Proceso
 ELSE
	RETURN
ENDIF

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
	@12,30 SAY '!!! BDD Esta Vac¡a !!!'
	WAIT
	RETURN
ENDIF

SELECT parma
IF !SEEK('OPERATMAYOR')
	DO Standby WITH 'No existe par metro de mayorizaci¢n'
	RETURN
ENDIF

IF !f_lock(1)
	RETURN
ENDIF

*bloquea el sistema en el ingreso de datos

=blo_sis(2)

* Limpia los archivos de saldos mensuales
SELECT MesN
ZAP

*  Borra del archivo de saldos por centro de costos, los saldos del mes
vmes = MONTH(vfecha)
vano = YEAR(vfecha)

*USE SalAux IN 11 ORDER TAG SalAux1              ALIAS SalA EXCLUSIVE
*SELECT SalA
*DELETE FOR Mes = vmes .AND. Ano = vano
*USE

*USE Diario IN 2 ORDER TAG Diario1               ALIAS Diar EXCLUSIVE  
*SELECT Diar
* PACK
*  xcolor = IIF(escolor, '', '*'+SET('COLOR'))


*  @ 1,20 SAY 'Acumula Saldos ............'  COLOR &xcolor
DO Acum

* @ 1,20 SAY 'Saldos por C.Costo ........'  COLOR &xcolor
* DO Costo

*@ 1,20 SAY 'Saldos por Auxiliar ...... '  COLOR &xcolor
*DO SalAux

* @ 1,20 SAY 'Saldos por Obra     ...... '  COLOR &xcolor
* DO SalObr

*  @ 1,20 SAY 'Traspaso de saldos ........' COLOR &xcolor
*DO Transal

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
SET BELL TO 512,2
SET CONSOLE OFF
RETURN


PROCEDURE TRANSAL
*-----------------
IF AP
	m.mes = 0
  ELSE
	m.mes = MONTH(vactmes)
ENDIF
 m.ano = YEAR(vactmes)
 
 SELECT SalN
 GO TOP
 
 DO Espera WITH 1,'Preparando Saldos'
 DELETE FOR SalN.Mes = m.Mes .AND. SalN.Ano = m.Ano
 DO Espera WITH 2
 GO TOP
 
 SELECT MesN
 DO progreso WITH 1,'Trasladando Saldos...'+'Cuenta:'+MesN.cuenta +'-'+ mFecha(MONT(vFecha),YEAR(vFecha))
 COUNT TO Registros
 Actual = 0
 
 GO TOP
 SCAN 
	actual=actual+1
	DO Progreso WITH 2,'',registros,actual
	@ 1,0 SAY ALLTRIM(STR(ACTUAL))+" DE: "+ALLTRIM(STR(REGISTROS))
	SCATTER MEMVAR
	SELECT SalN
	SEEK m.Cuenta + STR(m.Mes,2) + STR(m.Ano,4)
	IF !FOUND()
		APPEND BLANK
	ENDIF
	GATHER MEMVAR
	@ 1,47 SAY Mesn.Cuenta
	SELECT MesN
ENDSCAN
DO PROGRESO WITH 3

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
*vOrd=ORDER()
STORE 1 TO Cu
STORE SPACE(10) TO vXCu,vYQu
m.mes = STR(MONTH(vfecha),2)
m.ano = STR(YEAR(vFecha),4)
vmes  = STR(MONTH(vfecha)-1,2)
*xErdad=.F.
*vErdad=.F.
nosuma=.t.
vXCu = SPACE(10)
vYQu = SPACE(10)

*USE Compro IN 1  ORDER TAG Compro1               ALIAS Comp
*USE Diario IN 2  ORDER TAG Diario3               ALIAS Diar

IF MONT(vFecha)=1
	DO APERTURA
ENDIF

SELECT SalN
GO TOP

DO Espera WITH 1,'Preparando Saldos'
m.mes = STR(MONTH(vfecha),2)
m.ano = STR(YEAR(vFecha),4)
vmes  = STR(MONTH(vfecha)-1,2)
DELETE FOR SalN.Mes = VAL(m.Mes) .AND. SalN.Ano = VAL(m.Ano)
DO Espera WITH 2

DO Espera WITH 1, 'Ordenando Diario...'

vIdx1=SYS(3)+'.IDX'
SELECT Diar
INDEX ON CUENTA+DTOC(FECEMI) TO (vIdx1) FOR MONTH(Diar.FecEmi) = MONTH(vactmes);
         .AND. YEAR(Diar.FecEmi) = YEAR(vactmes) .AND. Estcon<>'55' AND COMPROB # '00'				&& AND ALLT(TIPDCR)<>'ZZ'

DO Espera WITH 2

Registros = _TALLY

DO progreso WITH 1,'Proceso de Mayorizaci¢n de '+mFecha(MONT(vFecha),YEAR(vFecha))
Actual = 0
GO TOP
DO WHILE !EOF()
	SELECT Diar
	STORE 0 TO m.DSumaNa, m.HSumaNa , vDSaldore, vHSaldore			&& vHSumaNa01 , vDSumaNa01 , vHSumaNa02 , vDSumaNa02 ,
	m.Cuenta = Diar.Cuenta
	SEEK m.Cuenta
	SCAN WHILE m.Cuenta = Diar.Cuenta
		actual=actual+1
		DO Progreso WITH 2,'',registros,actual
		@ 1,0 SAY 'Documento:['+allt(diar.comprob)+'.'+diar.comadi+'] Cuenta:'+'.'+diar.cuenta
		@ 2,0 SAY ALLTRIM(STR(ACTUAL))+" DE: "+ALLTRIM(STR(REGISTROS))
		IF MONTH(vactmes) = MONTH(Diar.FecEmi) .AND. YEAR(vactmes) = YEAR(Diar.FecEmi)
			m.DSumaNa = m.DSumaNa + IIF(Diar.Tipo = 'D',Diar.Montos,0)
			m.HSumaNa = m.HSumaNa + IIF(Diar.Tipo = 'H',Diar.Montos,0)
		ENDIF
	ENDSCAN
	nohay=.f.
	
	SELECT SalN
	SEEK  m.Cuenta + vMes + m.ano
	m.DSaldoNa = IIF(FOUND(),SalN.DSaldoNa,0)
	m.HSaldoNa = IIF(FOUND(),SalN.HSaldoNa,0)
	
	SELECT MesN
	
	m.DSaldoNa = IIF(m.DSaldoNa + m.DSumaNa > m.HSaldoNa + m.HSumaNa,;
				 m.DSaldoNa + m.DSumaNa - m.HSaldoNa - m.HsumaNa,0)
	m.HSaldoNa = IIF(m.HSaldoNa + m.HSumaNa > m.DSaldoNa + m.DSumaNa,;
				 m.HSaldoNa + m.HSumaNa - m.DSaldoNa - m.DsumaNa,0)
	
	IF SEEK(m.Cuenta)
		IF f_lock(1)
			REPLACE Cuenta WITH m.Cuenta, DSumaNa WITH m.DSumaNa,;
					HSumaNa WITH m.HSumaNa, DSaldoNa WITH m.DSaldoNa ,;
					HSaldoNa WITH m.HSaldoNa
		ENDIF
	 ELSE
		IF f_appd()
			GATHER MEMVAR
		ENDIF
	ENDIF
	
	SELECT MesN
	vXCu      = m.Cuenta
	vCuenta   = m.Cuenta
	vDSumaNa  = m.DSumaNa
	vHSumaNa  = m.HSumaNa
	xnumero   = 10
	nosuma    = .F.
	DO WHILE xnumero > 1
		xnumero = xnumero - 1
		SELECT Cuen
		SEEK vCuenta
		DO CASE 
			CASE xnumero = 2 AND Cuen.Nivel>=2 AND !(SUBSTR(CUENTA,1,1)="9" OR SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE xnumero = 3  AND Cuen.Nivel>=3 AND !(SUBSTR(CUENTA,1,1)="9" OR SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE xnumero = 5 AND Cuen.Nivel>=4 AND !(SUBSTR(CUENTA,1,1)="9" OR SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE  xnumero = 7 AND Cuen.Nivel=5 AND !(SUBSTR(CUENTA,1,1)="9" OR SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE xnumero = 2 AND Cuen.Nivel>=2 AND (SUBSTR(CUENTA,1,1)="9" OR SUBSTR(CUENTA,1,1)="0") 
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE xnumero = 4  AND Cuen.Nivel>=3 AND (SUBSTR(CUENTA,1,1)="9" OR SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE xnumero = 6 AND Cuen.Nivel>=3 AND (SUBSTR(CUENTA,1,1)="9")		&&CUEN.NIVEL=4
*				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.f.
			CASE xnumero = 6 AND Cuen.Nivel>=4 AND (SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE  xnumero = 8 AND Cuen.Nivel=4 AND (SUBSTR(CUENTA,1,1)="9")		&&CUEN.NIVEL=5
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
			CASE  xnumero = 8 AND Cuen.Nivel=5 AND (SUBSTR(CUENTA,1,1)="0")
				vXCu = PADR(SUBSTR(vCuenta,1,xnumero),10,"0")
				nosuma=.t.
		ENDCASE
		
		IF nosuma
			SELECT SalN
			SEEK vXCu + vmes + m.ano
			m.DSaldoNa = IIF(FOUND(),SalN.DSaldoNa,0)
			m.HSaldoNa = IIF(FOUND(),SalN.HSaldoNa,0)
			
			SELECT MesN
			Bloquea = .F.
			IF !SEEK(vXCu)
				Bloquea = f_Appd()
			 ELSE
				Bloquea = f_Lock(1)
			ENDIF
			IF Bloquea
*				STORE 0 TO m.DSumaNa, m.HSumaNa, m.DSaldoNa, m.HSaldoNa			&& ,vDSaldoNa01, vHSaldoNa01 ,vDSaldoNa02, vHSaldoNa02
				SELECT MesN
				m.Cuenta   = vXCu
				m.DSumaNa  = MesN.DSumaNa + vDSumaNa
				m.HSumaNa  = MesN.HSumaNa + vHSumaNa
				m.DSaldoNa = IIF(m.DSaldoNa + m.DSumaNa - m.HSaldoNa - m.HSumaNa>0,;
							 m.DSaldoNa + m.DSumaNa - m.HSumaNa - m.HSaldoNa,0)
				m.HSaldoNa = IIF(m.HSaldoNa + m.HSumaNa - m.DSaldoNa - m.DSumaNa>0,;
							 m.HSaldoNa + m.HSumaNA - m.DSaldoNa - m.DSumaNa,0)
				
				GATHER MEMVAR
			ENDIF
		ENDIF
		nosuma=.f.
	ENDDO 
	SELECT Diar
ENDDO  
DO PROGRESO WITH 3

SELE Diar
SET INDEX TO
ERASE (vIdx1)

DO progreso WITH 1,'Acumulando Cuentas '+mFecha(MONT(vFecha),YEAR(vFecha))
Actual = 0

vidx = SYS(3) + '.idx'
SELECT SalN

*vmes = MONTH(vfecha)
*IF vmes = 1
*   vmes = 0
*ELSE
*   vmes = vmes-1
*ENDIF
*vano = VAL(STR(YEAR(vfecha),4))
INDEX ON Cuenta FOR Mes = VAL(vmes) .AND. Ano = VAL(m.ano)  TO (vidx)
Registros = _TALLY

GO TOP
SCAN
	actual=actual+1
	DO Progreso WITH 2,'',registros,actual
	@ 1,0 SAY ' Cuenta:'+SalN.Cuenta
	@ 2,0 SAY ALLTRIM(STR(Actual))+" DE: "+ALLTRIM(STR(Registros))
	SELECT MesN
	IF !SEEK(SalN.Cuenta)
		IF f_Appd()
			SCATTER MEMVAR BLANK
			m.Cuenta   = SalN.Cuenta
			m.DSaldoNa = Saln.DSaldona
			m.HSaldoNa = Saln.HSaldona
			GATHER MEMVAR
		ENDIF
	ENDIF
	SELECT SalN
ENDSCAN

DO PROGRESO WITH 3

SET INDEX TO
ERASE (vIdx)
SET ORDER TO TAG SaldoNa1

SELECT SalN
GO TOP

SELECT MesN
DO progreso WITH 1,'Trasladando Saldos...'+'Cuenta:'+MesN.cuenta +'-'+ mFecha(MONT(vFecha),YEAR(vFecha))
COUNT TO Registros
Actual = 0

GO TOP
SCAN 
	actual=actual+1
	DO Progreso WITH 2,'',registros,actual
	@ 1,0 SAY ALLTRIM(STR(ACTUAL))+" DE: "+ALLTRIM(STR(REGISTROS))
	SCATTER MEMVAR
	SELECT SalN
	SEEK m.Cuenta + m.Mes + m.Ano
	IF !FOUND()
		APPEND BLANK
	ENDIF
	GATHER MEMVAR
	@ 1,47 SAY Mesn.Cuenta
	SELECT MesN
ENDSCAN
DO PROGRESO WITH 3

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
     *Bloquea = f_lock(1)
     Bloquea = .T.
  ELSE
  	 APPEND BLANK
     *Bloquea = f_appd()
     Bloquea = .T.
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
      *Bloquea = f_lock(1)
      Bloquea = .T.
   ELSE
      *Bloquea = f_appd()
      APPEND BLANK
      Bloquea = .T.
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
	      *Bloquea = f_lock(1)
	      Bloquea = .T.
	   ELSE
    	  *Bloquea = f_appd()
	   	  APPEND BLANK
    	  Bloquea = .T.
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
  RELEASE    MENU   mMenu
  CLOSE DATABASE
RETURN

FUNCTION Rambito  && Para que en el reporte aparezca de que se trata.
*---------------
PARAMETER vTipo
RETURN IIF(alltrim(vTipo)='Sede','PROGRAMA : SEDE CENTRAL',IIF(alltrim(vTipo)='Adm.Cen','PROGRAMA : 01 ADMINISTRACION CENTRAL','PLIEGO 01: REGION GRAU'))


FUNCTION cta_obra
*----------------
PARAMETER x_cuenta
RETURN (SUBSTR(x_Cuenta,1,2)='34' OR SUBSTR(x_Cuenta,1,2)='37' OR  SUBSTR(x_Cuenta,1,3)='337' OR SUBSTR(x_Cuenta,1,3)='332')


PROCEDURE Apertura
*-------------
* Calcula Los saldos de Apertura y los pone en saldona
* Tambi‚n, calcula los movimientos del mes de cada cuenta superior
* y calcula sus saldos.
* Las cuentas de resultado no se acumulan para el a¤o siguiente.
*--------------------------------------------------------------
PRIVATE Cu,vXCu,vYQu
vIdx1=SYS(3)+'.IDX'
STORE 1 TO Cu
STORE SPACE(10) TO vXCu,vYQu
AP=.T.
nosuma=.t.
vXCu = SPACE(10)
vYQu = SPACE(10)

SELECT SalN
GO TOP

DO Espera WITH 1,'Preparando Saldos'
DELETE FOR SalN.Mes = 0 .AND. SalN.Ano = vAno
DO Espera WITH 2
GO TOP

DO Espera WITH 1, 'Ordenando Diario...(Asiento de Apertura)'

SELECT Diar
INDEX ON CUENTA TO (vIdx1) FOR MONTH(Diar.FecEmi) = 1 .AND. YEAR(Diar.FecEmi) = YEAR(vactmes) ;
		.AND. Comprob='00'

DO Espera WITH 2

DO progreso WITH 1,'Proc. de Mayoriz. del Asto. de Apertura '+mFecha(MONT(vFecha),YEAR(vFecha))
Registros = _TALLY
Actual = 0

GO TOP
DO WHILE !EOF()
	SELECT Diar
	STORE 0 TO m.DSumaNa, m.HSumaNa
	m.Cuenta = Diar.Cuenta
	SEEK m.Cuenta
	SCAN WHILE m.Cuenta = Diar.Cuenta
		actual=actual+1
		DO Progreso WITH 2,'',registros,actual
		@ 1,0 SAY 'Documento:['+allt(diar.comprob)+'.'+diar.comadi+'] Cuenta:'+'.'+diar.cuenta
		@ 2,0 SAY ALLTRIM(STR(ACTUAL))+" DE: "+ALLTRIM(STR(REGISTROS))
		
		IF MONTH(vactmes) = MONTH(Diar.FecEmi) .AND. YEAR(vactmes) = YEAR(Diar.FecEmi)
				m.DSumaNa = m.DSumaNa + IIF(Diar.Tipo = 'D',Diar.Montos,0)
				m.HSumaNa = m.HSumaNa + IIF(Diar.Tipo = 'H',Diar.Montos,0)
		ENDIF
	ENDSCAN
	
	STORE 0 TO m.DSaldoNa, m.HSaldoNa			&& ,vDSaldoNa01, vHSaldoNa01 ,vDSaldoNa02, vHSaldoNa02
	
	m.DSaldoNa = IIF(m.DSaldoNa + m.DSumaNa > m.HSaldoNa + m.HSumaNa,;
					m.DSaldoNa + m.DSumaNa - m.HSaldoNa - m.HsumaNa,0)
	m.HSaldoNa = IIF(m.HSaldoNa + m.HSumaNa > m.DSaldoNa + m.DSumaNa,;
					m.HSaldoNa + m.HSumaNa - m.DSaldoNa - m.DsumaNa,0)
	
	SELECT MesN
	
	IF SEEK(m.Cuenta)
		IF f_lock(1)
			REPLACE Cuenta WITH m.Cuenta, DSumaNa WITH m.DSumaNa,;
					HSumaNa WITH m.HSumaNa, DSaldoNa WITH m.DSaldoNa ,;
					HSaldoNa WITH m.HSaldoNa
		ENDIF
	 ELSE
		IF f_appd()
			GATHER MEMVAR
		ENDIF
	ENDIF
	
	SELECT MesN
  	vcuenta   = Mesn.Cuenta
 	vXCu      = Mesn.Cuenta
  	vDSumaNa  = Mesn.DSumaNa
  	vHSumaNa  = Mesn.HSumaNa
    xnumero   = 10
    nosuma    = .F.
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
			Bloquea = .F.
			IF !SEEK(vXCu)
				Bloquea = f_Appd()
			 ELSE
				Bloquea = f_Lock(1)
			ENDIF
			IF Bloquea
				STORE 0 TO m.DSumaNa, m.HSumaNa, m.DSaldoNa, m.HSaldoNa			&& ,vDSaldoNa01, vHSaldoNa01 ,vDSaldoNa02, vHSaldoNa02
				SELECT MesN
				m.Cuenta   = vXCu
				m.DSumaNa  = MesN.DSumaNa + vDSumaNa
				m.HSumaNa  = MesN.HSumaNa + vHSumaNa
				m.DSaldoNa = IIF(m.DSaldoNa + m.DSumaNa - m.HSaldoNa - m.HSumaNa>0,;
							 m.DSaldoNa + m.DSumaNa - m.HSumaNa - m.HSaldoNa,0)
				m.HSaldoNa = IIF(m.HSaldoNa + m.HSumaNa - m.DSaldoNa - m.DSumaNa>0,;
							 m.HSaldoNa + m.HSumaNA - m.DSaldoNa - m.DSumaNa,0)
				
				GATHER MEMVAR
			ENDIF
		ENDIF
		nosuma=.f.
	ENDDO
	SELECT Diar
ENDDO
DO Progreso WITH 3

SELE Diar
SET INDEX TO
ERASE (vIdx1)

SELECT SalN
GO TOP
m.Mes = 0
m.Ano = vAno
SELECT MesN
DO progreso WITH 1,'Trasladando Saldos...'+'Cuenta:'+MesN.cuenta +'-'+ mFecha(MONT(vFecha),YEAR(vFecha))
COUNT TO Registros
Actual = 0
GO TOP
SCAN 
	actual=actual+1
	DO Progreso WITH 2,'',registros,actual
	@ 1,0 SAY ALLTRIM(STR(Actual))+" DE: "+ALLTRIM(STR(Registros))
	SCATTER MEMVAR
	SELECT SalN
	SEEK m.Cuenta + STR(m.Mes,2) + STR(m.Ano,4)
	IF !FOUND()
		APPEND BLANK
	ENDIF
	GATHER MEMVAR
	SELECT MesN
ENDSCAN
DO PROGRESO WITH 3
SELE MESN
ZAP
AP=.F.
RETURN

* 1226