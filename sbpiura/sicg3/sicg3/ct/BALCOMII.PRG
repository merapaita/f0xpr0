**---------------------------------
** BALCOM .PRG
** Reporte Plan de Cuentas - BALANCE DE COMPROBACION
** AUTOR : UDEP - 1994. 
**---------------------------------
* Abrir Archivos de trabajo
PUBLIC fmes,fano,fAmbito,mesntmp,mesnidx,RFin,vidx2
MesnTmp = SYS(3) + ".DBF"
Mesnidx = SYS(3) + ".IDX"
CLOSE DATA
USE Parmae  IN 1  ORDER TAG Parmae1     ALIAS parma
USE Cuentas IN 2  ORDER TAG Cuentas1    ALIAS Cuen
USE SalDoNA IN 3  ORDER TAG SalDoNa1    ALIAS SalN

SELECT 4
CREATE TABLE (MesnTmp) ;
(Cuenta      C(15),;
 Mes         N(2),;        
 Ano         N(4),;
 Tipo        C(1),;
 File        N(2),;
 Dsumana     N(18,4),;
 Hsumana     N(18,4),;
 Dsaldona    N(18,4),;
 Hsaldona    N(18,4),;
 Dsumaju     N(18,4),;
 Hsumaju     N(18,4),;
 Dsaldoaju   N(18,4),;
 Hsaldoaju   N(18,4),;
 Dajumes     N(18,4),;
 Hajumes     N(18,4),;
 Dajuano     N(18,4),;
 Hajuano     N(18,4),;
 Valmerc     N(18,4),;
 Vallimi     N(18,4))


USE 
SELECT 4
USE (MesnTmp) IN 4 ALIAS MesN               EXCLUSIVE
INDEX ON Cuenta TO (MesnIdx)



PUBLIC TDAcuMeAn, THAcuMeAn, TDDelMEs, THDelMes, TDAcuFin, THAcuFin, TDSalAct, THSalAct
PUBLIC TDAcuMeAn1, THAcuMeAn1, TDDelMEs1, THDelMes1, TDAcuFin1, THAcuFin1, TDSalAct1, THSalAct1
PUBLIC TDAcuMeAn2, THAcuMeAn2, TDDelMEs2, THDelMes2, TDAcuFin2, THAcuFin2, TDSalAct2, THSalAct2
PUBLIC TDAcuMeAn3, THAcuMeAn3, TDDelMEs3, THDelMes3, TDAcuFin3, THAcuFin3, TDSalAct3, THSalAct3

STORE 0 TO TDAcuMeAn, THAcuMeAn, TDDelMEs, THDelMes, TDAcuFin, THAcuFin, TDSalAct, THSalAct
STORE 0 TO TDAcuMeAn1, THAcuMeAn1, TDDelMEs1, THDelMes1, TDAcuFin1, THAcuFin1, TDSalAct1, THSalAct1
STORE 0 TO TDAcuMeAn2, THAcuMeAn2, TDDelMEs2, THDelMes2, TDAcuFin2, THAcuFin2, TDSalAct2, THSalAct2
STORE 0 TO TDAcuMeAn3, THAcuMeAn3, TDDelMEs3, THDelMes3, TDAcuFin3, THAcuFin3, TDSalAct3, THSalAct3

vind1= SYS(3) + '.idx'
xruta=SET('PATH')

DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  
  SELE PARMA

  vTitulo =  ' Balance Comprobaci¢n '
  DEFINE WINDOW RePlanCu FROM 5,15 TO 19,65 FLOAT DOUBLE TITLE vTitulo COLOR SCHEME 5
  ACTIVATE WINDOW RePlanCu
  
  xbalance = 'Mensual'
  xdigitos = 2
  SELECT Parma
  
  SEEK 'MESANOACTUA'
  vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
  xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
  xAno     = IIF(FOUND(), YEAR(vmesact),   YEAR(DATE()))
  xCuenta1 = '1000000000'
  xCuenta2 = '9900000000'
  vDigito  = 3
  
  @ 01,01 SAY '   Genera Balance : ' GET xBalance PICTURE '@M Mensual,Anual  ' DISABLE
  @ 03,01 SAY '   Cuenta Inicial : '+xCuenta1 
  @ 05,01 SAY '     Cuenta Final : '+xCuenta2 
  @ 07,01 SAY '        Mes y A¤o : ' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
             VALID xMes > 0 .AND. xMes <= 12
  @ 07,28 GET xAno                           PICTURE '9,999'
  @ 08,01 SAY '        # D¡gitos : ' GET vdigito  FUNCTION '^ Dos D¡gitos;Tres Digitos;Detallado'
  @ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
  READ CYCLE
  
  DEACTIVATE WINDOW RePlanCu
  
  
  nobusca=.t.
  IF xMes=1
     nobusca=.T.
     fMes=0
     fAno=xAno
  ELSE
     fMes=xMes-1   
     fAno=xAno
  ENDIF
  
  IF LASTKEY() # 27 AND OKCANCEL = 1
     DO CASE
        CASE xbalance = 'Mensual'
             IF Act_Mes(xMes,xAno)
                SELECT MesN
                DO ESPERA WITH 1,"Balance de Comprobaci¢n en proceso ..."
                INDEX ON Cuenta TO (vind1) 
                GO BOTT
                rFin = RECNO()
                GO TOP
                DO ESPERA WITH 2
				DO CASE
					CASE vDigito=1
						SET FILTER TO RIGHT(CUENTA,11)='00000000000'
					CASE vDigito=2
						SET FILTER TO RIGHT(CUENTA,9)='0000000'
*						SET FILTER TO IIF(Cuenta='9' OR Cuenta='0' ,RIGHT(CUENTA,6)='000000',RIGHT(CUENTA,7)='0000000')
                ENDCASE
				Temp01 = SYS(3) + ".IDX"
				INDEX ON Tipo+Cuenta TO &Temp01
				
                DO REPORTE WITH 2, "RePlanC1", 'Plan de Cuentas Mensual',1,.F.,.T.
                SET INDEX TO
                ERASE (Temp01)
                SELECT Parma
                SEEK 'LISCONPLANCU'
                IF F_Lock(1) .AND. FOUND()
                   REPLACE Parma.Descri WITH 'Si'
                ENDIF
                CLOSE DATA
                ERASE (vind1)
             ENDIF
             
          CLOSE DATA

        CASE xbalance = 'Anual  '
*          vind  = SYS(3) + '.idx'
*          vind1 = SYS(3) + '.idx'
*          vfile = SYS(3) + '.dbf'
*          vfile1= SYS(3) + '.dbf'
*          SELECT SalN
*          COPY FOR SalN.Ano = xAno;
*               .AND. Cuenta >= xCuenta1 .AND. Cuenta <= xCuenta2 TO (vfile)
*          SELECT 4
*          USE (vfile)
*          IF xAno = VAL(SUBSTR(STR(YEAR(vmesact),4),3,2))
*             SELECT MesN
*             USE
*             SELECT 4
*             DELETE FOR Ano = xAno .AND. Mes = MONTH(vmesact)
*             APPEND FROM SalMesN FOR Cuenta >= xCuenta1 .AND. Cuenta <= xCuenta2
*          ENDIF
*          INDEX ON Cuenta TO (vind)
*          TOTAL ON Cuenta TO (vfile1) FIELDS DSumaNa, HSumaNa
*          USE (vfile1)
*          INDEX ON Cuenta TO (vind1)
*          SET RELATION TO CUENTA INTO Cuen
*          DO REPORTE WITH 2, "ReplanCu", ' Plan de Cuentas Anual ',1,.F.,.T.
*          SELECT 4
*          USE
*          ERASE (vind)  
*          ERASE (vind1)
*          ERASE (vfile)
*          ERASE (vfile1)
     ENDCASE
     CLOSE DATABASE
  ENDIF
*  DEACTIVATE WINDOW RePlanCu

  IF !escolor
     RESTORE SCREEN FROM principal
  ENDIF
* SHOW POPUP menu, pop_03
RETURN

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


*FUNCTION Cuenta
*--------------
	IF SUBSTR(MesN.Cuenta,1,1)='0' OR SUBSTR(MesN.Cuenta,1,2)='9'
		DO CASE
			CASE SUBSTR(MesN.Cuenta,3,8)='00000000'
			   RETURN LEFT(MesN.Cuenta,2)
			CASE SUBSTR(MesN.Cuenta,5,6)='000000'
			   RETURN LEFT(MesN.Cuenta,4)
			CASE SUBSTR(MesN.Cuenta,7,4)='0000'
			   RETURN LEFT(MesN.Cuenta,6)
			CASE SUBSTR(MesN.Cuenta,9,2)='00'
			   RETURN LEFT(MesN.Cuenta,8)
		 	OTHERWISE
			   RETURN MesN.Cuenta
		ENDCASE
	 ELSE
		DO CASE
			CASE SUBSTR(MesN.Cuenta,3,8)='00000000' OR SUBSTR(MesN.Cuenta,3,1)='O0000000'
			   RETURN LEFT(MesN.Cuenta,2)
			CASE SUBSTR(MesN.Cuenta,4,7)='0000000' OR SUBSTR(MesN.Cuenta,4,7)='O000000'
			   RETURN LEFT(MesN.Cuenta,3)
			CASE SUBSTR(MesN.Cuenta,6,5)='00000' OR SUBSTR(MesN.Cuenta,6,5)='O0000'
			   RETURN LEFT(MesN.Cuenta,5)
			CASE SUBSTR(MesN.Cuenta,8,3)='000' OR SUBSTR(MesN.Cuenta,8,3)='O00'
			   RETURN LEFT(MesN.Cuenta,7)
			CASE SUBSTR(MesN.Cuenta,10,1)='0' OR SUBSTR(MesN.Cuenta,8,3)='O'
			   RETURN LEFT(MesN.Cuenta,9)
		 	OTHERWISE
			   RETURN MesN.Cuenta
		ENDCASE
	ENDIF

FUNCTION XDMESANT
*---------------
PUBLIC vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo,vKey
STORE 0 TO vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo
SELECT SalN
SCAN WHILE SalN.Cuenta=MesN.Cuenta
     vDAcuA = vDAcuA + SalN.DSumaNa
     vHAcuA = vHAcuA + SalN.HSumaNa
     vDAcuF = vDAcuF + SalN.DSumaNa
     vHAcuF = vHAcuF + SalN.HSumaNa
ENDSCAN
=HMESANT()
vDAcuF = vDAcuF + MesN.DSumaNa
=DACUFIN()
vHAcuF = vHAcuF + MesN.HSumaNa
=HACUFIN()
SELECT MesN
RETURN vDAcuA



FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
IF PARAMETERS()=2
   RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,4)
ENDIF
IF PARAMETERS()=1
   RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) 
ENDIF
RETURN SPACE(0)


PROCEDURE Salida
*---------------
CLOSE DATABASE
ON KEY LABEL F1
RELEASE TDAcuMeAn, THAcuMeAn, TDDelMEs, THDelMes, TDAcuFin, THAcuFin, TDSalAct, THSalAct,;
        TDAcuMeAn1, THAcuMeAn1, TDDelMEs1, THDelMes1, TDAcuFin1, THAcuFin1, TDSalAct1, THSalAct1,;
        TDAcuMeAn2, THAcuMeAn2, TDDelMEs2, THDelMes2, TDAcuFin2, THAcuFin2, TDSalAct2, THSalAct2,;
        TDAcuMeAn3, THAcuMeAn3, TDDelMEs3, THDelMes3, TDAcuFin3, THAcuFin3, TDSalAct3, THSalAct3

RETURN


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


FUNCTION Act_Mes
*----------------
*- Actualiza el archivo SALMESN con informaci¢n correspondiente
*- al mes que se quiere sacar balance.
*- El campo MesN.Tipo se reemplaza por :
*- 'O' Para cuentas de Orden       (01 a 09)
*- 'A' Para cuentas Patrimoniales  (10 a 89)
*- 'E' Para cuentas Presupuestales (90 a final)
*- Con la finalidad que pueda hacer el quiebre en reporte de Balance.

PARAMETERS  _Mes , _Ano
lMayori = .F.

DO ESPERA WITH 1, "Balance de Comprobaci¢n a " + mfecha(_mes,_ano) + " en proceso .... "

SELECT SalN
vIdx2 = SYS(3)+'.idx'
INDEX ON saln.cuenta FOR Saln.ano=_Ano TO (vIdx2)  UNIQUE
GO TOP
SCAN
       SELECT Mesn
       IF F_appd()
          REPLACE mesn.cuenta WITH saln.cuenta
          REPLACE mesn.mes    WITH _mes
          REPLACE mesn.ano    WITH saln.ano
          REPLACE MesN.Tipo WITH IIF(LEFT(mesn.cuenta,1)='9','O', IIF(LEFT(mesn.cuenta,1)='8','E','A'))
*          REPLACE MesN.Tipo WITH IIF(BETWEEN(LEFT(mesn.cuenta,2),'00','09'),'O', IIF(BETWEEN(LEFT(mesn.cuenta,2),'10','89'),'A','E'))
       ENDIF
       SELECT Saln
ENDSCAN
SELECT saln
SET INDEX TO
ERASE(vIdx2)
SET ORDER TO TAG saldona4

SELECT mesn
GO TOP
SCAN
     SELECT saln
     SEEK mesn.cuenta + STR(mesn.ano,4) + STR(mesn.mes,2)
     IF FOUND()
        IF f_lock(1)
           lmayori=.T.
           REPLACE mesn.dsumana WITH saln.dsumana
           REPLACE mesn.hsumana WITH saln.hsumana
           UNLOCK
        ENDIF
     ENDIF
     SELECT mesn
ENDSCAN
DO ESPERA WITH 2

IF !lMayori
   DO STANDBY WITH "No se ha ejecutado el proceso de Mayorizaci¢n del mes de "+mfecha(_mes)
ENDIF   

RETURN lMayori


FUNCTION fin
*-----------
RETURN Rfin=RECNO()


*FUNCTION DMESANT
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
	SCAN WHILE  SalN.Cuenta=MesN.Cuenta AND SalN.Mes<=fMes AND SalN.Ano=fAno
*   vDAcuA = vDAcuA + SalN.DSaldona
*   vHAcuA = vHAcuA + SalN.HSaldona
*   vDAcuF = vDAcuF + SalN.DSaldona
*   vHAcuF = vHAcuF + SalN.HSaldona

	vDAcuA = vDAcuA + SalN.DSumana
	vHAcuA = vHAcuA + SalN.HSumana
    vDAcuF = vDAcuF + SalN.DSumana
    vHAcuF = vHAcuF + SalN.HSumana

	ENDSCAN
	=HMESANT()
ENDIF

IF xMes=1
	vDAcuA = salapr(cuenta,'D')
	vHAcuA = salapr(cuenta,'H')
ENDIF
vDAcuF = vDAcuF + MesN.DSumaNa
=DACUFIN()
vHAcuF = vHAcuF + MesN.HSumaNa
=HACUFIN()
SELE SALN
SET ORDER TO TAG Saldona1

SELECT MesN
RETURN vDAcuA



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


FUNCTION SalApr     && Calcula Saldos de Apertura por Cuenta
*--------------
PARAMETER mCta,mTipo
mTabla=ALIAS()
mCta1=Cuenta()
SELE DIARIO
SUM Montos TO mSalApr FOR Tipo=mTipo AND Cuenta=mCta1
SELE (mTabla)
RETURN mSalApr

