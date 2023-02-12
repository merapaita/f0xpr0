**----------------------------------------
** LibMayor.PRG
** Reporte de Libro Mayor (Ultima versi¢n)
** Autor : Javier Fr¡as Gomez / Fernando Mu¤oz Cho
** Fecha : Agosto 1996.
**----------------------------------------

*- Definici¢n de Variables

*filaux  = sys(3) + '.dbf'
*filaux2 = sys(3) + '.dbf'
indaux  = sys(3) + '.idx'
vind   = sys(3) + '.idx'
vind3   = sys(3) + '.idx'
vind5   = sys(3) + '.idx'
vind6   = sys(3) + '.idx'

*- Abrir Archivos de Trabajo:
CLOS DATA
USE Diario  IN 1  ORDER TAG Diario16  ALIAS Diar
USE Parmae  IN 2  ORDER TAG Parmae1   ALIAS parma
USE Auxil   IN 3  ORDER TAG Auxil1    ALIAS Auxi
USE Cuentas IN 4  ORDER TAG Cuentas1  ALIAS Cuen
USE Compro  IN 5  ORDER TAG Compro1   ALIAS Comp
USE Saldona IN 6  ORDER TAG Saldona1  ALIAS SalD
USE Folios  IN 7  ORDER TAG Folios1   ALIAS Folios
USE Tipos   IN 11 ORDER TAG Tipos1    ALIAS Tipos

*- Genera un archivo Res£men para generar libro mayor

filaux  = sys(3) + '.dbf'
SELECT 8
CREATE TABLE &filaux;
  (TipDoc  C(02),;
   Mes     C(02),;
   Orden   C(02),;
   Cuenta  C(04),;
   DFolio  C(03),;
   HFolio  C(03),;
   Grupo   C(03),;
   DDeta   C(04),;
   HDeta   C(04),;
   Debe    N(18,3),;
   Haber   N(18,3))
USE IN 8
USE &FilAux IN 8 ALIAS Tempo
SELE Tempo
INDEX ON Mes+TipDoc+Orden+Cuenta To (vind3)

filaux2 = sys(3) + '.dbf'
SELECT 9
CREATE TABLE &filaux2;
   (Grupo   C(03), Mes C(02) )
USE IN 9
USE &filaux2 IN 9 ALIAS Grupos EXCLUSIVE
SELECT grupos
INDEX ON Grupo TO (vind6)
*-

DO Inicia
DO Termina
RETURN

*- Fin de Proceso


PROCEDURE Inicia
*---------------
DEFINE WINDOW MesAno FROM 5,15 TO 18,65 FLOAT DOUBLE TITLE ' Libro Mayor ' COLOR SCHEME 5
ACTIVATE WINDOW MesAno
SELECT Parma
SEEK "MESANO"+"ACTUAL"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,1,4))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xAno     = IIF(FOUND(), YEAR(vmesact), YEAR(DATE()))
xgendet  = 2
vAjustado="Ajustado"

@ 01,03 SAY '           Per¡odo  : ' GET xano     PICTURE '9,999'
@ 03,03 SAY '             Mes   : ' GET xMes    PICTURE '99' ;
      VALID val_para(PADL(ALLT(STR(xmes)),2,'0'),'FECMES',' ',26,20)
*@ 04,03 SAY '           Ajustado : ' GET vAjustado  PICTURE '@M Ajustado,No Ajustado'  
@ 07,03 SAY '  Detallado/General : '
@ 06,26 GET xgendet FUNCTION '^ Detallado ;General ' 
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE


IF xMes=12
	vAjustado="Ajustado"
  ELSE
	vAjustado="No Ajustado"
ENDIF

DEACTIVATE WINDOW MesAno

xmes = PADL(ALLTRIM(STR(xmes,2)),2,'0')
xano = STR(xano,4)
IF OKCANCEL = 1 .AND. LASTKEY() # 27
	IF xgendet=1
		DO GeneraMay2  WITH xMes,xAno
	 ELSE
		DO GeneraMayor WITH xMes,xAno
	ENDIF
ENDIF
DEACTIVATE WINDOW Consis
RETURN


PROCEDURE GeneraMayor
*--------------------
PARAMETERS pMes,pAno
PRIVATE xMes
SELECT Diar
DO ESPERA WITH 1, 'Espere un momento; procesando informaci¢n'

vDbf  = SYS(3) + ".Dbf"
vind4 = sys(3) + '.Idx'
COPY TO (vDbf) FOR IIF(ALLTRIM(vajustado)='Ajustado',IIF(LEFT(Comprob,2)#'93',MONTH(Fecemi)=VAL(pMes),MONTH(Fecemi)<=VAL(pMes)),MONTH(Fecemi)=VAL(pMes) AND LEFT(Comprob,2)#'93') .AND. STR(YEAR(FecEmi),4) = pAno

USE (vDbf) ALIAS Diar
INDEX ON DTOC(FecEmi,1)+Comprob+Orden+Cuenta TO (vInd4)
SET RELATION TO pAno+LEFT(Comprob,2) INTO Tipos
REPLACE comprob WITH Tipos.TipDoc2+SUBSTR(Comprob,3) FOR LEFT(Comprob,2)=Tipos.tipdoc1
SET RELATION TO

IF vAjustado="Ajustado"
	xDia = LastDay(VAL(pMes),VAL(pAno))
	REPLACE FecEmi WITH xDia FOR Comprob='93'
ENDIF

GO TOP
vCont  = 0
vOldG  = '00'
vOldG1 = '00'
SCAN FOR MONTH(FecEmi)=VAL(pMes) and STR(YEAR(FECEMI),4) = pAno
	xTipDoc = LEFT(ALLTRIM(Diar.Comprob),2)
	xMes    = PADL(ALLTRIM(STR(MONTH(Diar.FecEmi))),2,'0')
	
	IF EMPTY(xMes)
		DO STANDBY WITH "Revise el Documento "+Diar.Comprob+" : No tiene fecha!"
	ENDIF
	
	xOrd    = Diar.Orden
	IF EMPTY(xOrd)
		DO STANDBY WITH "Revise el Documento "+Diar.Comprob+" : No tiene Orden!"
	ENDIF
	xCuenta = LEFT(Diar.Cuenta,4)
	@ 0,0 SAY Diar.comprob
	IF EMPTY(xCuenta)
		DO STANDBY WITH "Revise el Documento "+Diar.Comprob+'-'+Diar.ComAdi+" : No tiene n£mero de cuenta!"
	ENDIF
	xMonto  = Diar.Montos
	SELE Tempo
	
	vProc = IIF( SEEK(xMes+xTipDoc+xOrd+xCuenta) , f_lock(1) , f_appd() )
*	vProc = IIF( SEEK(xMes+xOrd+xTipDoc+xCuenta) , f_lock(1) , f_appd() )
	
	IF vOldG <> LEFT(diar.comprob,2) OR vOldG1 <> diar.Orden
		vCont  = vCont+1
		vOldG  = LEFT(diar.comprob,2)
		vOldG1 = diar.Orden
	ENDIF
*	IF vOldG<>LEFT(diar.comprob,2)
*		vCont = vCont+1
*		vOldG = LEFT(diar.comprob,2)
*	ENDIF
	IF vProc
		xtipo=Diar.Tipo
		REPLACE Tempo.TipDoc WITH xTipDoc ,;
				Tempo.Mes    WITH xMes    ,;
				Tempo.Orden  WITH xOrd    ,;
				Tempo.Cuenta WITH xCuenta ,;
				Tempo.DFolio WITH DRetFolio(xTipDoc,xOrd,xMes,xcuenta,xTipo),;
				Tempo.HFolio WITH HRetFolio(xTipDoc,xOrd,xMes,xcuenta,xTipo),;
				Tempo.Grupo  WITH PADL(ALLTRIM(STR(vCont)),3,'0')
				
*				Tempo.DFolio WITH DRetFolio(xTipDoc,xMes,xcuenta,xTipo),;
*				Tempo.HFolio WITH HRetFolio(xTipDoc,xMes,xcuenta,xTipo)
				
				IF Diar.Tipo='D'
					REPLACE Tempo.Debe  WITH Tempo.Debe+xMonto
				 ELSE
					REPLACE Tempo.Haber WITH Tempo.Haber+xMonto
				ENDIF
	ENDIF
	SELE Diar
ENDSCAN
DO ESPERA WITH 2
SELECT Tempo
GO TOP
IF EOF()
	DO standby WITH 'No hay informaci¢n para procesar.'
 ELSE
	DO prep_des
	DO GenRep WITH pMes,pAno
ENDIF
USE IN Diar
ERASE (vDbf)
ERASE (vInd4)
RETURN


PROCEDURE GenRep
*---------------
PARAMETERS pMes,pAno

*- Crea indice en parmae para buscar el folio:
SELECT Parma
SET INDEX TO
indaux2 = sys(3) + '.idx'
INDEX ON codigoaux TO (IndAux2) FOR Tipo='TIPDOC'

SELECT Tempo
SET RELATION TO PADR(Tempo.Cuenta,15,'0') INTO Cuen
SET RELATION TO Tempo.TipDoc              INTO Parma ADDITIVE
SET FILTER TO
INDEX ON CUEN.TIPO+Cuenta+Mes+TipDoc+Orden TO (indAux)
GO TOP

DO REPORTE WITH 2,'MAYOR1','  Libro Mayor de '+ MFecha(VAL(pMes),VAL(pAno))+'  ',1,.F.,.T.

SELECT Parma
SET INDEX TO
ERASE (indaux2)
RETURN


PROCEDURE Termina
*----------------
CLOSE DATABASES
ERASE(FilAux)
ERASE(FilAux2)
ERASE(IndAux)
ERASE(vind3)
ERASE(vind5)
ERASE(vind6)
RETURN


FUNCTION DRetFolio
*----------------
PARAMETERS xTD,xOrd,xM,xcta,xTp
vali=ALIAS()
SELE Folios
SEEK ('LIBDIR'+xM+pAno+xtd+xOrd+xcta+'00000000000')
IF FOUND() 
	SELE (VALI)
	RETURN PADL(ALLTRIM(STR(Folios.DFolio,3)),3,'0')
ENDIF

SELE (VALI)
RETURN ' '


FUNCTION HRetFolio
*----------------
PARAMETERS xTD,xOrd,xM,xcta,xTp
vali=ALIAS()
SELE Folios
SEEK ('LIBDIR'+xM+pAno+xtd+xOrd+xcta+'00000000000')
IF FOUND()
   SELE (VALI)
   RETURN PADL(ALLTRIM(STR(Folios.HFolio,3)),3,'0')
ENDIF

SELE (VALI)
RETURN ' '



FUNCTION xRetFolio
*----------------
PARAMETERS xTD,xM,xTp
IF val(xM)<> val(OldMes)
   HNumFolio = 0
   OldMes    = xM
   OldTDoc   = '00'
ENDIF   

IF val(OldTDoc) <> val(xTd)
   OldTDoc=xTd
   HNumFolio=HNumFolio+1
ENDIF   
RETURN PADL(ALLTRIM(STR(HNumFolio)),3,'0')

PROCEDURE  GeneraMay2
*--------------------
PARAMETER wmes,wano

SELE Comp
SET ORDER TO  TAG Compro7

SELE Diar 
SET ORDER TO  TAG Diario11
SET RELATION TO Comprob+Comadi+Codfte INTO Comp  

Sele Diar
DO ESPERA WITH 1, "Reporte del Mayor Detallado en Proceso...!"
INDEX ON Diar.Cuenta+Diar.Comprob+Diar.Comadi+Diar.Codfte to (VIND) FOR MONTH(Diar.Fecemi)=VAL(wmes) AND YEAR(Diar.fecemi)=VAL(wano) AND;
	IIF(ALLTRIM(vAjustado)="Ajustado",.T.,LEFT(Diar.Comprob,2)<>"93") 


DO ESPERA WITH 2,' '

DO REPORTE WITH 2,'Maydet','Libro Mayor Mensual  Detallado',1,.F.,.T.

SELE Diar
SET INDEX TO
ERASE (Vind)
RETURN

FUNCTION CUENTA1
*----------------
SELE Cuen
SEEK (SUBSTR(Diar.Cuenta,1,2)+'00000000')
SELE Diar
RETURN Cuen.Descri

FUNCTION CUENTA2
*----------------
SELE Cuen
SEEK (SUBSTR(Diar.Cuenta,1,3)+'0000000')
SELE Diar
RETURN Cuen.Descri

FUNCTION CUENTA3
*----------------
SELE Cuen
SEEK (Diar.Cuenta)
SELE Diar
RETURN Cuen.Descri

FUNCTION SALANT1
*------------------
PARAMETER wtipo

SELE SALD
IF VAL(WMES)=1
   v1DSaldona=0
   SELE Diar
   RETURN (v1DSaldona)
ELSE   

   * SEEK (SUBSTR(Diar.Cuenta,1,3)+'0000000')+STR(WMES-1,2)+STR(WANO,2)
   SEEK (SUBSTR(Diar.Cuenta,1,3)+'0000000')+STR(VAL(WMES)-1,2)+WANO
   IF FOUND()
	   IF wtipo='D'
	      v1DSaldona=SALD.DSaldona
	      SELE Diar
	      RETURN SALD.DSaldona
	   ELSE
	      v1HSaldona=SALD.HSaldona
	      SELE Diar
          RETURN SALD.HSaldona
	   ENDIF
   ELSE
         v1HSaldona=0
         v1DSaldona=0
         SELE Diar
	    RETURN (v1DSaldona)
   ENDIF 
ENDIF
SELE Diar
RETURN


FUNCTION SALANT2
*------------------
PARAMETER wtipo
SELE SALD
IF VAL(wMes)=1
         v1DSaldona=0
        SELE Diar
	    RETURN (v1DSaldona)
ELSE   
   * SEEK (SUBSTR(Diar.Cuenta,1,2)+'000000')+STR(WMES-1,2)+STR(WANO,2)
   SEEK (SUBSTR(Diar.Cuenta,1,2)+'000000')+STR(VAL(WMES)-1,2)+WANO
   IF FOUND()
	   IF wtipo='D'
	      v2DSaldona=SALD.DSaldona
	      SELE Diar
	      RETURN SALD.DSaldona
	   ELSE
	      v2HSaldona=SALD.HSaldona
	      SELE Diar
          RETURN SALD.HSaldona
	   ENDIF
   ELSE
        v2HSaldona=0
        v2DSaldona=0
        SELE Diar
	    RETURN   (v2DSaldona)
   ENDIF 
ENDIF
SELE Diar
RETURN

FUNCTION SALANT3
*------------------
PARAMETER wtipo
SELE SALD
IF VAL(WMES)=1
   v1DSaldona=0
   SELE Diar
   RETURN (v1DSaldona)
ELSE   
   * SEEK (Diar.Cuenta)+STR(WMES-1,2)+STR(WANO,2)
   SEEK (Diar.Cuenta)+STR(VAL(WMES)-1,2)+WANO
   IF FOUND()
	   IF wtipo='D'
	      v3DSaldona=SALD.DSaldona
	      SELE Diar
	      RETURN SALD.DSaldona
	   ELSE
	      v3HSaldona=SALD.HSaldona
	      SELE Diar
          RETURN SALD.HSaldona
	   ENDIF
   ELSE
        v3HSaldona=0
        v3DSaldona=0
        SELE Diar
	    RETURN (v3DSaldona)
   ENDIF 
ENDIF
SELE Diar
RETURN

PROCEDURE prep_des
*-----------------
DO ESPERA WITH 1, 'Creando el Detalle del Mayor'

*- Por cada mes tiene que blanquear la base temporal
SELECT grupos
ZAP

*- Indexa el archivo temporal del mayor para el mes correspondiente a 'pMes'
SELECT Tempo
INDEX ON GRUPO TO (vind3) UNIQUE for tempo.mes=pMes		&&PADL(ALLTRIM(STR(j,2)),2,'0')
GO TOP
*- Actualiza el archivo grupos 
SCAN
	vgrupo = tempo.grupo
	SELECT grupos
	APPEND BLANK
	REPLACE Grupos.grupo WITH vgrupo
	SELECT tempo
ENDSCAN

*- Vuelve a reindexar por que se uso la cl usula UNIQUE
SELECT Tempo
INDEX ON GRUPO TO (vind3) FOR tempo.mes = pMes		&&PADL(ALLTRIM(STR(j,2)),2,'0')

*- Una vez que se tiene un archivo temporal con todos los grupos
*- del mes actual (j),
*- se procede a actualizar el campo detalle

SELECT grupos
SCAN
	@ 0,0 SAY grupos.grupo+'( Mes:'+pMes+')'
	SELECT tempo
	INDEX ON tempo.grupo TO (vind5) FOR tempo.grupo=grupos.grupo AND tempo.debe>0 and tempo.mes=pMes		&&PADL(ALLTRIM(STR(j,2)),2,'0')
	IF !EOF()
		COUNT TO vNum
		IF vNum=1 
			GO TOP
			xcuenta = tempo.cuenta
			INDEX ON tempo.grupo TO (vind5) FOR tempo.grupo=grupos.grupo AND tempo.haber>0 and tempo.mes=pMes		&&PADL(ALLTRIM(STR(j,2)),2,'0')
			SCAN
				REPLACE tempo.hDeta WITH xcuenta
			ENDSCAN
		ENDIF
	ENDIF
	
	INDEX ON tempo.grupo TO (vind5) FOR tempo.grupo=grupos.grupo AND tempo.haber>0 and tempo.mes=pMes		&&PADL(ALLTRIM(STR(j,2)),2,'0')
	IF !EOF()
		COUNT TO vNum
		IF vNum=1 
			GO TOP
			xcuenta = tempo.cuenta
			INDEX ON tempo.grupo TO (vind5) FOR tempo.grupo=grupos.grupo AND tempo.debe>0  and tempo.mes=pMes		&&PADL(ALLTRIM(STR(j,2)),2,'0')
			SCAN
				REPLACE tempo.dDeta WITH xcuenta
			ENDSCAN
		ENDIF
	ENDIF
	
	SELECT grupos
ENDSCAN
DO ESPERA WITH 2    
RETURN
     

FUNCTION Guardafol
*-----------------
 PARAMETER xTipDoc,xOrden,xcuenta,vMes,xpagina
 xCuenta=LEFT(xCuenta,4)+'00000000000'
 SELE Folios
 IF SEEK ('LIBMAY'+pmes+pano+xTipDoc+xOrden+xcuenta)
     Bloquea = f_lock(1)
 ELSE
     Bloquea = f_appd()
 ENDIF
 IF Bloquea
    REPLACE Reporte WITH 'LIBMAY',;
            Mes     WITH pMes,;
		    Periodo WITH pAno,;
		    Orden   WITH xOrden,;
		    Tipdoc  WITH xTipdoc,;
		    Cuenta  WITH xCuenta,;
		    Dfolio  WITH xpagina,;
		    Hfolio  WITH xpagina
ENDIF
SELECT Tempo
RETURN ' '


FUNCTION XDAcuAnt
*---------------
PARAMETERS xcuenta,xmes
vAli=SELECT()
SELECT SalD
vMes = VAL(xMes)-1
vAno = VAL(ZAno)
IF VMes=12
   vAno = VAno - 1
ENDIF   
SEEK LEFT(tempo.cuenta,2)+'00000000'+ STR(vMes,2) + STR(vAno,2)
vRet = salD.DSaldona
SELECT(vAli)
RETURN vRet


FUNCTION XHAcuAnt
*---------------
PARAMETERS xcuenta,xmes
vAli=SELECT()
SELECT SalD
vMes = VAL(xMes)-1
vAno = VAL(ZAno)
IF VMes=12
   vAno = VAno - 1
ENDIF   
SEEK LEFT(tempo.cuenta,2)+'00000000' + STR(vMes,2) + STR(vAno,2)
vRet = salD.HSaldona
SELECT(vAli)
RETURN vRet


FUNCTION DAcuAnt
*---------------
PARAMETERS xcuenta,xmes
vAli=SELECT()
SELECT SalD
vMes = VAL(pMes)-1
vAno = VAL(pAno)
vret=0
IF vMes=0
	vMes = -1
ENDIF
IF ALLTRIM(vAjuStado)="Ajustado"
	FOR J=0 TO vMes
	    SEEK LEFT(tempo.cuenta,4)+'00000000000'+ STR(j,2) + STR(vAno,4)
	    vRet = vret + IIF(FOUND(),salD.DSumana,0)
*	    vRet = vret + IIF(FOUND(),salD.DSumAJU,0)
	ENDFOR    
ELSE
	FOR J=0 TO vMes
	    SEEK LEFT(tempo.cuenta,4)+'00000000000'+ STR(j,2) + STR(vAno,4)
	    vRet = vret + IIF(FOUND(),salD.DSumana,0)
	ENDFOR    
ENDIF

SELECT(vAli)
RETURN vRet


FUNCTION HAcuAnt
*---------------
PARAMETERS xcuenta,xmes
vAli=SELECT()
SELECT SalD
vMes = VAL(xMes)-1
vAno = VAL(pAno)
vret = 0

IF vMes=0
	vMes = -1
ENDIF

IF ALLTRIM(vAjuStado)="Ajustado"
	FOR J=0 TO vMes
	    SEEK LEFT(tempo.cuenta,4)+'00000000000' + STR(j,2) + STR(vAno,4)
	   	vRet = vret + IIF(FOUND(),salD.hSumana,0)
*	   	vRet = vret + IIF(FOUND(),salD.hSumAJU,0)
	ENDFOR
ELSE
	FOR J=0 TO vMes
	    SEEK LEFT(tempo.cuenta,4)+'00000000000' + STR(j,2) + STR(vAno,4)
	   	vRet = vret + IIF(FOUND(),salD.hSumana,0)
	ENDFOR
ENDIF

SELECT(vAli)
RETURN vRet


FUNCTION GLOSA
*-------------
PARAMETER vCu
vAli=ALIAS()
vret=''
vCuenta=vcu+'00000000'
SELECT Cuen
SET ORDER TO TAG Cuentas1
SEEK vCuenta
IF FOUND()
   vret= LEFT(Cuen.Descri,38)
ENDIF
SELECT(vAli)
RETURN ALLTRIM(vRet)

