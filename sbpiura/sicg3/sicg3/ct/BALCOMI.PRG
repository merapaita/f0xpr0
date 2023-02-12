**---------------------------------
** REPLANCU .PRG
** Reporte Plan de Cuentas
** AUTOR : UDEP - 1994. JECHS
**---------------------------------
PUBLIC MesnTmp,Mesnidx,FechaFin

MesnTmp = SYS(3) + ".DBF"
Mesnidx = SYS(3) + ".IDX"

* Abrir Archivos de trabajo

CLOSE DATA
USE Parmae  IN 1  ORDER TAG Parmae1     ALIAS parma
USE Cuentas IN 2  ORDER TAG Cuentas1    ALIAS Cuen
USE SalDoNA IN 3  ORDER TAG SalDoNa1    ALIAS SalN



SELECT 4
CREATE TABLE (MesnTmp) ;
(Cuenta      C(10),;
 Tipo        C(1),;
 Mes         N(2),;        
 Ano         N(2),;
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


PUBLIC fmes,fano,fAmbito,XMES

vind1= SYS(3) + '.idx'
xruta=SET('PATH')
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
   vTitulo = ' Balance Comprobaci¢n ' 
  
  DEFINE WINDOW RePlanCu FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE  vTitulo COLOR SCHEME 5
  ACTIVATE WINDOW RePlanCu
  
  xbalance = 'Mensual'
  xdigitos = 2
  SELECT Parma
  
  
  SEEK 'MESANOACTUA'
  vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
  xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
  xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
  xCuenta1 = '1000000000'
  xCuenta2 = '9900000000'  
  

  
  @ 03,01 SAY '        Mes y A¤o : ' GET xMes     PICTURE '99' ;
             VALID xMes > 0 .AND. xMes <= 12
  @ 03,28 GET xAno                           PICTURE '99'

  @ 09,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
  READ
  DEACTIVATE WINDOW RePlanCu
  nobusca=.t.
  NOSUMA=.T.  
  FechaFin=LastDay(xmes,xano)
  IF xMes=1
     NOSUMA=.F. 
     NoBusca=.F.
     fMes=12
     fAno=xAno-1
  ELSE
     fMes=xMes-1   
     fAno=xAno
  ENDIF
  
  IF LASTKEY() # 27 AND OKCANCEL = 1
     DO CASE
        CASE xbalance = 'Mensual'
             IF Act_Mes(xMes,xAno)
                SELECT MesN
                DO ESPERA WITH 1,"Balance de Comprobaci¢n en Proceso ..."
                INDEX ON Tipo+Cuenta TO (vind1) FOR SUBSTR(Cuenta,3,8)='00000000' 
                DO ESPERA WITH 2      
                DO REPORTE WITH 2, "RePlanCU", 'Plan de Cuentas Mensual',1,.F.,.T.
                SELECT Parma
                SEEK 'LISCONPLANCU'
                IF F_Lock(1) .AND. FOUND()
                   REPLACE Parma.Descri WITH 'Si'
                ENDIF
                CLOSE DATA
                ERASE (vind1)
             ENDIF

*             vpath   = '..\'+ALLTRIM(STR(xmes,2))+STR(xano,2)
*             vdir    = '..\'+ALLTRIM(STR(xmes,2)) + STR(xano,2) + '\'
*             vbackup = ALLTRIM(STR(xmes,2)) + STR(xano,2)+'.zip'
*             IF !FILE(vdir + vbackup)
*                DO Standby WITH 'Archivo : de BackUp en Disco del mes ' ;
*                   + ALLTRIM(STR(xmes)) + ' '+ALLTRIM(STR(xano,2)) + ' no existe'
*                RETURN
*             ENDIF
* 
*             !CD &vpath >NUL
* 
*             !FOXSWAP PKUNZIP -o &vbackup Parmae.dbf Cuentas.dbf >NUL
*             !FOXSWAP PKUNZIP -o &vbackup SalMesN.dbf >NUL
*             !FOXSWAP PKUNZIP -o &vbackup Parmae.cdx Cuentas.cdx >NUL
*             !FOXSWAP PKUNZIP -o &vbackup SalMesN.cdx >NUL
*             !CD ..\FUENTES
*             CLOSE DATA
*             SET PATH TO &vpath
*             USE Parmae  IN 1 ORDER TAG Parmae1     ALIAS parma
*             USE SalMesN IN 2 ORDER TAG SalMesN1    ALIAS MesN
*             USE Cuentas IN 3 ORDER TAG Cuentas1    ALIAS Cuen
*             SELECT MesN
*             INDEX ON Cuenta TO (vind1);
*                   FOR Cuenta >= xCuenta1 .AND. Cuenta <= xCuenta2
*             DO REPORTE WITH 2, "RePlanCu", 'Plan de Cuentas Mensual',1,.F.,.T.
*             CLOSE DATA
*             ERASE (vind1)
*             vdbf = vdir + '*.dbf'
*             vcdx = vdir + '*.cdx'
*             !ERASE &vdbf >NUL
*             !ERASE &vcdx >NUL
*             SET PATH TO &xruta
*          ENDIF
          CLOSE DATA

        CASE xbalance = 'Anual  '
          vind  = SYS(3) + '.idx'
          vind1 = SYS(3) + '.idx'
          vfile = SYS(3) + '.dbf'
          vfile1= SYS(3) + '.dbf'
          SELECT SalN
          COPY FOR SalN.Ano = xAno;
               .AND. Cuenta >= xCuenta1 .AND. Cuenta <= xCuenta2 TO (vfile)
          SELECT 4
          USE (vfile)
          IF xAno = VAL(SUBSTR(STR(YEAR(vmesact),4),3,2))
             SELECT MesN
             USE
             SELECT 4
             DELETE FOR Ano = xAno .AND. Mes = MONTH(vmesact)
             APPEND FROM SalMesN FOR Cuenta >= xCuenta1 .AND. Cuenta <= xCuenta2
          ENDIF
          INDEX ON Cuenta TO (vind)
          TOTAL ON Cuenta TO (vfile1) FIELDS DSumaNa, HSumaNa
          USE (vfile1)
          INDEX ON Cuenta TO (vind1)
          SET RELATION TO CUENTA INTO Cuen
          DO REPORTE WITH 2, "ReplanCu", ' Plan de Cuentas Anual ',1,.F.,.T.
          SELECT 4
          USE
          ERASE (vind)  
          ERASE (vind1)
          ERASE (vfile)
          ERASE (vfile1)
     ENDCASE
     CLOSE DATABASE
  ENDIF
  DEACTIVATE WINDOW RePlanCu

  IF !escolor
     RESTORE SCREEN FROM principal
  ENDIF
* SHOW POPUP menu, pop_03
RETURN


FUNCTION Cuenta
*--------------
IF SUBSTR(MesN.Cuenta,3,10)='00000000'
   RETURN LEFT(MesN.Cuenta,2)
ENDIF
IF SUBSTR(MesN.Cuenta,4,10)='0000000'
   RETURN LEFT(MesN.Cuenta,3)
ENDIF
IF SUBSTR(MesN.Cuenta,6,10)='00000'
   RETURN LEFT(MesN.Cuenta,5)
ENDIF
IF SUBSTR(MesN.Cuenta,8,10)='000'
   RETURN LEFT(MesN.Cuenta,7)
ENDIF
IF SUBSTR(MesN.Cuenta,10,10)='0'
   RETURN LEFT(MesN.Cuenta,9)
ENDIF
RETURN

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




FUNCTION DMESANT
*---------------
PUBLIC vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo,vKey
STORE 0 TO vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo


IF nobusca=.t.
	SELECT SalN
	SET ORDER TO TAG Saldona4
	*---------------------------------------------------------------
	*- SIEMPRE DEBE IR AL MES DE ENERO 
	SEEK MesN.Cuenta+STR(fano,2)
	*---------------------------------------------------------------
	SCAN WHILE SalN.Cuenta=MesN.Cuenta AND SalN.Mes<=fMes AND SalN.Ano=fAno
		vDAcuA = vDAcuA + SalN.DSumana
		vHAcuA = vHAcuA + SalN.HSumana
	    vDAcuF = vDAcuF + SalN.DSumana
    	vHAcuF = vHAcuF + SalN.HSumana
	ENDSCAN
	=HMESANT()
ENDIF
vDAcuF = vDAcuF + MesN.DSumaNa
=DACUFIN()
vHAcuF = vHAcuF + MesN.HSumaNa
=HACUFIN()
SELE SALN
SET ORDER TO TAG Saldona1
SELECT MesN
RETURN vDAcuA




FUNCTION _DMESANT_
*---------------
PUBLIC vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo,vKey
STORE 0 TO vDAcuA,vHAcuA,vDAcuF,vHAcuF,DSaldo,HSaldo
IF NOSUMA=.T.  
SELECT SalN 
*SEEK MesN.Cuenta+STR(fmes,2)+STR(fano,2)
SEEK MesN.Cuenta + STR(fmes,2) + STR(fano,2)
SCAN WHILE SalN.Cuenta=MesN.Cuenta AND SalN.Mes=fMes AND SalN.Ano=fAno
     vDAcuA = vDAcuA + SalN.DSaldona
     vHAcuA = vHAcuA + SalN.HSaldona
     vDAcuF = vDAcuF + SalN.DSaldona
     vHAcuF = vHAcuF + SalN.HSaldona
ENDSCAN
=HMESANT()
ENDIF
vDAcuF = vDAcuF + MesN.DSumaNa
=DACUFIN()
vHAcuF = vHAcuF + MesN.HSumaNa
=HACUFIN()
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


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
IF PARAMETERS()=2
   RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)
ENDIF
IF PARAMETERS()=1
   RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) 
ENDIF
RETURN SPACE(0)
   


PROCEDURE Salida
*---------------
CLOSE DATABASE
CLOS INDEX
ERASE(MesnTmp)
ERASE(Mesnidx)
RETURN

FUNCTION XAct_Mes
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
GO TOP
SCAN
    IF salN.mes = _Mes AND salN.ano = _Ano
       lMayori = .T.
       SCATTER MEMVAR
       SELECT Mesn
       IF F_appd()
          GATHER MEMVAR
          REPLACE MesN.Tipo WITH IIF(BETWEEN(LEFT(mesn.cuenta,2),'01','08'),'O', IIF(BETWEEN(LEFT(mesn.cuenta,2),'10','89'),'A','E'))
       ENDIF
    ENDIF
    SELECT Saln
ENDSCAN
DO ESPERA WITH 2
IF !lMayori
   DO STANDBY WITH "No se ha ejecutado el proceso de Mayorizaci¢n del mes de "+mfecha(_mes)
ENDIF
RETURN lMayori


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
INDEX ON saln.cuenta FOR Saln.ano=_ano TO (vIdx2)  UNIQUE
GO TOP
SCAN
       SELECT Mesn
       IF F_appd()
          REPLACE mesn.cuenta WITH saln.cuenta
          REPLACE mesn.mes    WITH _mes
          REPLACE mesn.ano    WITH saln.ano
          REPLACE MesN.Tipo WITH IIF(BETWEEN(LEFT(mesn.cuenta,2),'01','08'),'O', IIF(BETWEEN(LEFT(mesn.cuenta,2),'10','89'),'A','E'))
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
     SEEK mesn.cuenta + STR(mesn.ano,2) + STR(mesn.mes,2)
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



