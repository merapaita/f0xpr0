PROCEDURE val_aux
*----------------
PARAMETERS cCodigo, cTipo

IF SEEK(cTipo+cCodigo,'Auxi')
	mRet = ALLTRIM(auxi.descri)
ELSE
	mRet = "Sin Detalle"
ENDIF

RETURN mRet


PROCEDURE val_codcad
*-------------------
PARAMETERS cCadena,cPeriodo
IF SEEK(cPeriodo+"01001"+cCadena, "maepre")
	mRet = ALLTRIM(maepre.descri)
ELSE
	mRet = "Sin Detalle"
ENDIF

RETURN mRet

FUNCTION desri
*-------------
PRIVATE calias, mret
calias = ALIAS()
IF SEEK('R/I' + recing.tipri, 'AsiAut')
     mret = asiaut.descri
ENDIF
RETURN mret

FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,4)

*************************************************************************
*  PROCEDURE fox_lock.prg    FUE REEMPLAZADO POR F_LOCK()
*
*  Parametros: Tipo de bloqueo, Valor logico de bloqueo
*  Tipos
*        1 : Intenta bloquear registro hasta que lo logra o se cancele
*        2 : Intenta bloquear registro solo una vez
*        3 : Intenta bloquear archivo hasta que lo logra o se cancele
*        4 : Intenta bloquear archivo solo una vez
*
*************************************************************************

PROCEDURE fox_lock
*-----------------
PARAMETERS BTipo,Bloquea
   Beep = CHR(7)

   DO CASE

   CASE BTipo = 1    && Bloquea registro hasta conseguirlo o escape
      Bloquea   = .F.
      IF RLOCK()
         Bloquea   = .T.
      ELSE
         WAIT WINDOW "Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar."
*         DO STANDBY WITH "Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar."
         ?? Beep
         KTecla    = 0

         DO WHILE KTecla <> Escape .AND. (.NOT. RLOCK())
            KTecla    = INKEY()
         ENDDO

         IF KTecla <> Escape
            Bloquea = .T.
         ENDIF

      ENDIF

   CASE BTipo = 2     && Intenta bloquear registro solo una vez

      Bloquea   = .F.
      IF RLOCK()
         Bloquea   = .T.
      ELSE
         WAIT WINDOW "El registro est� siendo utilizado. Se cancela la operaci�n. "
*         DO STANDBY WITH "El registro est� siendo utilizado. Se cancela la operaci�n. "
         ?? Beep
      ENDIF

   CASE BTipo = 3        && Bloquea archivo hasta conseguirlo a cancela
      Bloquea   = .F.

      IF FLOCK()
         Bloquea   = .T.
         Insiste   = .F.
      ELSE
         WAIT WINDOW "Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar."
*         DO STANDBY WITH "Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar."
         ?? Beep
         KTecla    = 0

         DO WHILE KTecla <> Escape .AND. (.NOT. FLOCK())
            KTecla    = INKEY()
         ENDDO

         IF KTecla <> Escape
            Bloquea = .T.
         ENDIF

      ENDIF

   CASE BTipo = 4   && Intenta bloquear archivo solo una vez

      Bloquea   = .F.
      IF FLOCK()
         Bloquea   = .T.
      ELSE
         WAIT WINDOW "Archivo ocupado.  El proceso se cancela."
*         DO STANDBY WITH "Archivo ocupado.  El proceso se cancela."
         ?? Beep
      ENDIF

   ENDCASE

RETURN


FUNCTION f_lock
*--------------
PARAMETERS BTipo

   Beep = CHR(7)

   DO CASE

   CASE BTipo = 1    && Bloquea registro hasta conseguirlo o escape
      v_fun   = .F.

      IF RLOCK()
         v_fun   = .T.
      ELSE
         WAIT WINDOW "Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar."
*         DO STANDBY WITH "Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar."
         ?? Beep
         KTecla    = 0
         
         DO WHILE KTecla <> Escape .AND. (.NOT. RLOCK())
            KTecla    = INKEY()
         ENDDO
         
         IF KTecla <> Escape
            v_fun = .T.
         ENDIF
         
      ENDIF

   CASE BTipo = 2     && Intenta bloquear registro solo una vez

      v_fun   = .F.
      IF RLOCK()
         v_fun   = .T.
      ELSE
         WAIT WINDOW  "El registro est� siendo utilizado. Se cancela la operaci�n. "
*         DO STANDBY WITH "El registro est� siendo utilizado. Se cancela la operaci�n. "
         ?? Beep
      ENDIF

   CASE BTipo = 3        && Bloquea archivo hasta conseguirlo o cancela
      v_fun   = .F.
      
      IF FLOCK()
         v_fun   = .T.
      ELSE
         WAIT WINDOW "Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar."
*         DO STANDBY WITH "Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar."
         ?? Beep
         KTecla    = 0
         
         DO WHILE KTecla <> Escape .AND. (.NOT. FLOCK())
            KTecla    = INKEY()
         ENDDO
         
         IF KTecla <> Escape
            v_fun = .T.
         ENDIF
         
      ENDIF

   CASE BTipo = 4   && Intenta bloquear archivo solo una vez

      v_fun   = .F.
      IF FLOCK()
         v_fun   = .T.
      ELSE
         WAIT WINDOW "Archivo ocupado.  El proceso se cancela."
*         DO STANDBY WITH "Archivo ocupado.  El proceso se cancela."
         ?? Beep
      ENDIF

   ENDCASE

RETURN v_fun

*************************************************************************
* PROCEDURE fox_appd    FUE REEMPLAZADO POR F_APPD()
* Objeto  :   Biblioteca de funciones para trabajo multi-usuario
*             Previamente, es necesario estar en el archivo a agregar
*             y que este estuviese abierto en modo multiusario.
*             Parametros: LAppd. Variable l"gica
* LAppd
*       .T.: Agrego y bloqueo nuevo registro
*       .F.: No pudo Agregar.
*
*  Fecha : 01/12/89
*
*************************************************************************

PROCEDURE fox_appd
*-----------------
PARAMETERS LAppd
   LAppd     = .F.
   Bloquea   = .F.
   GAgrega   = .T.

   APPEND BLANK

   DO FOX_Lock WITH 1,Bloquea

   IF Bloquea
      LAppd   = .T.
   ENDIF

   GAgrega = .F.

RETURN


FUNCTION f_appd
*--------------
   APPEND BLANK
   v_fun = F_Lock(1)

RETURN v_fun

FUNCTION LastDay
*---------------
PARAMETERS Zmes,Zano
*- Devuelve una Fecha que corresponde al �ltimo dia del mes y a�o
*- por M.R.A.
DO CASE
	CASE PARAMETERS() = 0
		zMes = MONT(DATE())
		Zano = YEAR(DATE())
	CASE PARAMETERS() = 1
		Zano = YEAR(DATE())
ENDCASE
DO CASE
	CASE zMes = 1 OR zMes=3 OR zMes = 5 OR zMes = 7 OR zMes = 8 OR zMes = 10 OR zMes = 12
		vDia = '31'
	CASE zMes = 4 OR zMes=6 OR zMes = 9 OR zMes = 11
		vDia = '30'
	CASE zMes = 2
		IF MOD(zAno,4)=0
			vDia = '29'
		 ELSE
			vDia = '28'
		ENDIF
ENDCASE

IF BETWEEN(zMes,1,12)
	vMes = PADL(ALLTRIM(STR(Zmes)),2,'0')
 ELSE
	vMes = "12"
ENDIF

vAno = ALLTRIM(STR(Zano))

vRet = CTOD(vDia+'/'+vMes+'/'+vAno)
RETURN vRet

FUNCTION datext
*--------------
PARAMETERS vFecha
*- Devuelve una cadena conteniendo la descripci�n de la fecha
*- especificada en letras
*- por J.F.G.

vCentury = SET("CENTURY")
SET CENTURY ON
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
vRet  = PADL(ALLTRIM(STR(DAY(vFecha))),2,'0') + ;
        ' DE ' + ;
        ALLTRIM(SUBSTR(Meses,MONTH(vFecha)*9-8,9)) + ;
        ' DE ' + ;
        TRANSFORM(YEAR(vFecha),'9,999')
        
*        TRANSFORM( VAL(SUBS(DTOC(vFecha),7,4)),'9,999')
SET CENTURY &vCentury        
RETURN ALLTRIM(vret)

*------
* balance
*-------
procedure fin
*-----------
RETURN .F.
*RETURN Rfin=RECNO()

FUNCTION DMESANT
*---------------
PUBLIC vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo,vKey
STORE 0 TO vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo
IF nobusca=.t.
	SELECT SalN
	SET ORDER TO TAG Saldona4
	*---------------------------------------------------------------
	*- SIEMPRE DEBE IR AL MES DE ENERO
	SEEK MesN.Cuenta+STR(fano,4)
	*---------------------------------------------------------------
	SCAN WHILE  SalN.Cuenta=MesN.Cuenta AND SalN.Mes<=fMes AND SalN.Ano=fano

*	   vDAcuA = vDAcuA + SalN.DSaldona
*	   vHAcuA = vHAcuA + SalN.HSaldona
*	   vDAcuF = vDAcuF + SalN.DSaldona
*	   vHAcuF = vHAcuF + SalN.HSaldona

		vDAcuA = vDAcuA + SalN.DSumana
		vHAcuA = vHAcuA + SalN.HSumana
	    vDAcuF = vDAcuF + SalN.DSumana
	    vHAcuF = vHAcuF + SalN.HSumana

	ENDSCAN
	=HMESANT()
ENDIF

*IF xMes=1
*	vDAcuA = salapr(cuenta,'D')
*	vHAcuA = salapr(cuenta,'H')
*ENDIF
vDAcuF = vDAcuF + MesN.DSumaNa
=DACUFIN()
vHAcuF = vHAcuF + MesN.HSumaNa
=HACUFIN()
SELE SALN
SET ORDER TO TAG Saldona1

SELECT MesN
RETURN vDAcuA


FUNCTION HMESANT
*---------------
RETURN vHAcuA

FUNCTION DACUFIN
*---------------
RETURN vDAcuF


FUNCTION HACUFIN
*---------------
RETURN vHAcuF

FUNCTION DSalAct
*---------------
IF vDAcuF > vHAcuF
   DSaldo  = vDAcuF - vHAcuF
ELSE
   HSaldo  = vHAcuF - vDAcuF
   =HSalAct()
ENDIF
RETURN DSaldo

FUNCTION HSalAct
*---------------
RETURN HSaldo

FUNCTION Cuenta
*--------------
DO CASE
	CASE SUBSTR(MesN.Cuenta,5,11)='00000000000'
	   RETURN LEFT(MesN.Cuenta,4)
	CASE SUBSTR(MesN.Cuenta,7,9)='000000000'
	   RETURN LEFT(MesN.Cuenta,6)
	CASE SUBSTR(MesN.Cuenta,9,7)='0000000'
	   RETURN LEFT(MesN.Cuenta,8)
	CASE SUBSTR(MesN.Cuenta,11,5)='00000'
	   RETURN LEFT(MesN.Cuenta,10)
	CASE SUBSTR(MesN.Cuenta,13,3)='000'
	   RETURN LEFT(MesN.Cuenta,12)
 	OTHERWISE
	   RETURN MesN.Cuenta
ENDCASE

FUNCTION suma_pag
*----------------
*- Sumatoria de Todo el Reporte:
TDAcuMeAn  =  TDAcuMeAn  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",DMesAnt(),0)
THAcuMeAn  =  THAcuMeAn  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",HMesAnt(),0)
TDDelMes   =  TDDelMes   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",MesN.dsumana,0)
THDelMes   =  THDelMes   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",MesN.HSumana,0)
TDAcuFin   =  TDAcuFin   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",DACUFIN(),0)
THAcuFin   =  THAcuFin   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",HACUFIN(),0)
TDSalact   =  TDSalact   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",DSalAct(),0)
THSalact   =  THSalAct   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000",HSalAct(),0)


*- Sumatoria de las Cuentas Patrimoniales:
TDAcuMeAn1  =  TDAcuMeAn1  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',DMesAnt(),0)
THAcuMeAn1  =  THAcuMeAn1  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',HMesAnt(),0)
TDDelMes1   =  TDDelMes1   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',MesN.dsumana,0)
THDelMes1   =  THDelMes1   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',MesN.hsumana,0)
TDAcuFin1   =  TDAcuFin1   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',DACUFIN(),0)
THAcuFin1   =  THAcuFin1   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',HACUFIN(),0)
TDSalact1   =  TDSalact1   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',DSalAct(),0)
THSalact1   =  THSalAct1   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='A',HSalAct(),0)

*- Sumatoria de las Cuentas Presupuestales:
TDAcuMeAn2  =  TDAcuMeAn2  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',DMesAnt(),0)
THAcuMeAn2  =  THAcuMeAn2  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',HMesAnt(),0)
TDDelMes2   =  TDDelMes2   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',MesN.Dsumana,0)
THDelMes2   =  THDelMes2   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',MesN.Hsumana,0)
TDAcuFin2   =  TDAcuFin2   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',DACUFIN(),0)
THAcuFin2   =  THAcuFin2   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',HACUFIN(),0)
TDSalact2   =  TDSalact2   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',DSalAct(),0)
THSalact2   =  THSalAct2   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='E',HSalAct(),0)
*TDSalact2   =  TDSalact2   + IIF(SUBS(Mesn.Cuenta,4,7)="00000000000" AND Mesn.Tipo='E',DSalAct(),0)
*THSalact2   =  THSalAct2   + IIF(SUBS(Mesn.Cuenta,4,7)="00000000000" AND Mesn.Tipo='E',HSalAct(),0)


*- Sumatoria de las Cuentas De Orden (no existen):
TDAcuMeAn3  =  TDAcuMeAn3  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',DMesAnt(),0)
THAcuMeAn3  =  THAcuMeAn3  + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',HMesAnt(),0)
TDDelMes3   =  TDDelMes3   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',MesN.dsumana,0)
THDelMes3   =  THDelMes3   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',MesN.HSumaNa,0)
TDAcuFin3   =  TDAcuFin3   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',DACUFIN(),0)
THAcuFin3   =  THAcuFin3   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',HACUFIN(),0)
TDSalact3   =  TDSalact3   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',DSalAct(),0)
THSalact3   =  THSalAct3   + IIF(SUBS(Mesn.Cuenta,5,11)="00000000000" AND Mesn.Tipo='O',HSalAct(),0)

*- Recalculo Para que DDJ
TDSalact   =  TDSalact1+TDSalact2+TDSalact3
THSalact   =  THSalAct1+THSalact2+THSalact3
*--------------------------------------------

RETURN " "

PROCEDURE Des_Cuenta
*----------------
PARAMETERS cCta

IF SEEK(cCta,'Cuen')
	mRet = ALLTRIM(cuen.descri)
ELSE
	mRet = "Sin Detalle"
ENDIF

RETURN mRet

* aUXILIAR sTANDARD
FUNCTION GLOSA
*-------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=LEFT(Diar.Cuenta,2)+'00000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '

FUNCTION GLOSA1
*--------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=LEFT(Diar.Cuenta,3)+'0000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '

FUNCTION GLOSA2
*--------------
vAli=ALIAS()
vOrd=ORDER()
vCuenta=Diar.Cuenta
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   RETURN Cuen.Descri
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
RETURN '  '

FUNCTION Glosas
*--------------
PARAMETER conObra
IF PARAMETER()=0
   conObra=.T.
ENDIF   
r  = ""
gl = ALLTRIM( UPPER ( Comp.Glosa1 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+gl
ENDIF
gl = ALLTRIM( UPPER ( Comp.Glosa2 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( Comp.Glosa3 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( Comp.Glosa4 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF

*-HOJA DE CONTROL
*r=r+' '+IIF(!Empty(Diar.NumHC),'Ref. H/C :'+ Diar.NummesHC+'-'+Diar.NumHC,'')
*--

IF conObra
*--OBRA
IF !EMPTY(diar.codobra)
   vAli=SELECT()
   SELECT obra
   SEEK diar.codobra
   IF FOUND()
      r=r+' ( OBRA: '+diar.codobra+' - '+ALLTRIM(obra.descri)+' )'
   ENDIF
   SELECT(vAli)
ENDIF      
*--
ENDIF

RETURN IIF(!Empty(r),ALLTRIM(r),'No se encontr� detalle/glosa para este Documento.')

FUNCTION des_proy
*----------------
vperiodo  = LEFT(comp.codcal,2)
vcodprg   = SUBSTR(comp.codcal,8,2)
vcodsubpr = SUBSTR(comp.codcal,10,3)
vcodproy  = SUBSTR(comp.codcal,13,3)
vret      = SPACE(0)
vcodprg   = vperiodo+vcodprg+vcodsubpr+vcodproy
IF LEN(alltrim(vcodprg))=0
   RETURN 'No existe C�digo del Proyecto'
ENDIF   
Alias = SELECT()
SELECT maepre
SEEK vcodprg
IF FOUND() 
   vret = maepre.descri
ENDIF
SELECT(alias)   
RETURN vret



FUNCTION desc_prov
*-----------------
PARAMETERS xtipo,xreg
area=SELECT()
vRet=""
SELECT auxi
SEEK xtipo+xreg
IF FOUND()
   vRet=LEFT(auxi.descri,26)
ENDIF
SELECT(area)
RETURN vret   


FUNCTION RTipoDoc
*---------------
PARAMETER numD
Area  = SELECT()
xOrd = ORDER()
SELECT Parma
SET INDEX TO (vIndPar)
SEEK numD
ret = parma.codigo
SELECT(Area)
SET ORDER TO (xOrd)
RETURN ret

FUNCTION DAcuAnt
*---------------
PARAMETERS xcuen
vAli=SELECT()
SELECT SalD
vret=0
FOR J=0 TO xMes
    SEEK PADR(xcuen,15,'0')+ STR(j,2) + str(xAno,4)
    vRet = vret + IIF(FOUND(),salD.DSumana,0)
ENDFOR    
SELECT(vAli)
RETURN vRet


FUNCTION HAcuAnt
*---------------
PARAMETERS xcuen
vAli=SELECT()
SELECT SalD
vret = 0
FOR J=0 TO xMes
    SEEK PADR(xcuen,15,'0')+ STR(j,2) + str(xAno,4)
    vRet = vret + IIF(FOUND(),salD.HSumana,0)
ENDFOR
SELECT(vAli)
RETURN vRet

FUNCTION Acumula
*---------------
VanDebe  = VanDebe+IIF(Diar.Tipo='D',Diar.Montos,0)
VanHaber = VanHaber+IIF(Diar.Tipo='H',Diar.Montos,0)
RETURN ''

FUNCTION Resetv
*---------------
VanDebe  = 0
VanHaber = 0
RETURN ''
