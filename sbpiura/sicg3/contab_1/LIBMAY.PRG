**----------------------------------------
** LibMayor.PRG
** Reporte de Libro Mayor (Ultima versi¢n)
** Fecha : Agosto 1996.
**----------------------------------------

*- Definici¢n de Variables
PUBLIC XRET
PUBLIC filaux,indaux,repomes,repoano,indaux2,vind,vind2,lastq,vind3,vind41,vind51,vind61,XANO
PUBLIC oldmes,dnumfolio,hnumfolio,oldtdoc, xano,wtipo, fmes, fano,filaux2
PUBLIC  vDeb101,vHab101
DECLARE VDeb101(12),VHab101(12)
STORE 0 TO VDeb101,vHab101
zmes       = '  '
zano       = '  '
XRET = SPACE(15)
oldmes     = '00'
oldtdoc    = '00'
wtipo      = ' '


dnumfolio  = 0
hnumfolio  = 0
v1dsaldona = 0
v1hsaldona = 0
v2dsaldona = 0
v2hsaldona = 0
v3dsaldona = 0
v3hsaldona = 0
fmes       = 0
fano       = 0

Xano       = 0

filaux  = sys(3) + '.dbf'
filaux2 = sys(3) + '.dbf'
indaux  = sys(3) + '.idx'
indaux2 = sys(3) + '.idx'
vind    = sys(3) + '.idx'
vind2   = sys(3) + '.idx'
vind3   = sys(3) + '.idx'
vind41   = sys(3) + '.idx'
vind51   = sys(3) + '.idx'
vind61   = sys(3) + '.idx'

*- Abrir Archivos de Trabajo:
USE Diario  IN 1 ORDER TAG Diario16        ALIAS Diar
USE Parmae  IN 2 ORDER TAG Parmae1         ALIAS parma
USE Auxil   IN 3 ORDER TAG Auxil1          ALIAS Auxi
USE Cuentas IN 4 ORDER TAG Cuentas1        ALIAS Cuen
USE Compro  IN 5 ORDER TAG Compro1         ALIAS Comp
USE Saldona IN 6 ORDER TAG Saldona1        ALIAS SalD
USE Folios  IN 7 ORDER TAG Folios2        ALIAS Folios

*- Genera un archivo Res£men para generar libro mayor
SELECT 8
CREATE TABLE (filaux);
  (TipDoc  C(02),;
   Orden   C(02),;
   Mes     C(02),;
   Cuenta  C(02),;
   DFolio  C(03),;
   HFolio  C(03),;
   Grupo   C(03),;
   DDeta   C(02),;
   HDeta   C(02),;
   Debe    N(18,3),;
   Haber   N(18,3))
USE IN 8  
USE (FilAux) IN 8 ALIAS Tempo
SELE Tempo
INDEX ON Mes+Orden+TipDoc+Cuenta To (vind3)


SELECT 9
CREATE TABLE (filaux2);
   (Grupo   C(03), Mes C(02) )
USE IN 9  
USE (FilAux2) IN 9 ALIAS Grupos EXCLUSIVE
SELECT grupos
INDEX ON Grupo TO (vind61)
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
SEEK "MESANO"+"ACTUA"
vmesact  = CTOD('01/'+SUBSTR(parma.Descri,5,2)+'/'+SUBSTR(parma.Descri,3,2))
xMes     = IIF(FOUND(), MONTH(vmesact), MONTH(DATE()))
xMesF    = PADL(ALLTRIM(STR(xmes)),2,'0')
xAno     = IIF(FOUND(), VAL(SUBSTR(STR(YEAR(vmesact),4),3,2)), VAL(SUBSTR(STR(YEAR(DATE()),4),3,2)))
xgendet  = 2
vAjustado="Ajustado"

@ 01,03 SAY '           Per¡odo  : ' GET xano     PICTURE '99'
@ 02,03 SAY '                Mes : ' GET XMES    PICTURE '99' ;
						     VALID val_para(PADL(XMES,2,'0'),'FECMES',' ',26,20)
						     
@ 07,03 SAY '  Detallado/General : '
@ 06,26 GET xgendet FUNCTION '^ Detallado ;General '
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ
xmesF=PADL(XMES,2,'0')
IF xMes=12
	vAjustado="Ajustado"
  ELSE
	vAjustado="No Ajustado"
ENDIF
DEACTIVATE WINDOW MesAno
  
zmes = PADL(ALLTRIM(STR(xmes,2)),2,'0')
zano = PADL(ALLTRIM(STR(xano,2)),2,'0')
Fmes = xMes
Fano = xAno
xmes = XMESF

IF OKCANCEL = 1 .AND. LASTKEY() # 27
   IF xgendet=1 
      DO GeneraMay2  WITH xMes,xAno       
   ELSE
      DO Gentemp88   WITH xMes,xAno
      * DO GeneraMayor WITH xMes,xAno
   ENDIF   
ENDIF
DEACTIVATE WINDOW Consis
RETURN

PROCEDURE Gentemp88
*------------------
* Se ha incluido el Procedimiento  del Libro Diario para efectos del Folio
* se esta viendo la funcionalidad 
*-------------------------------
PARAMETERS QMes,QAno
DO ESPERA WITH 1,"Espere un momento por Favor; Estamos procesando...! "
	vind4  = SYS(3) +'.idx'
    vind1  = SYS(3) +'.idx'
	vind5  = SYS(3) +'.idx'
    vfil1  = SYS(3) +'.dbf'
    vfil2  = SYS(3) +'.dbf'
    vind   = SYS(3) +'.idx'
	vindX  = SYS(3) + '.DBF'
	vIndi2 = SYS(3) + ".IDX"
    vindY  = SYS(3) + ".DBF"
    vindZ  = SYS(3) + ".DBF"
    vindV  = SYS(3) + ".DBF"
	vindW  = SYS(3) + '.DBF'
	vindT  = SYS(3) + '.DBF'
	vindU  = SYS(3) + '.DBF'
	vindO  = SYS(3) + '.DBF'
	vindP  = SYS(3) + '.DBF'
	vindC  = SYS(3) + '.DBF'
	vindQ  = SYS(3) + '.DBF'
	vVqui  = SYS(3) + '.DBF'
	vindF  = SYS(3) + '.DBF'
	vindG  = SYS(3) + '.DBF'
	vindM  = SYS(3) + '.DBF'
	vindS  = SYS(3) + '.DBF'
	VINDJ = SYS(3) + '.DBF'	
	
	SELECT 25
	CREATE TABLE (vVqui);
    	(comprob   c(010),;
	 	 Asiento   c(03))
	USE

	USE (vVqui) IN 25 ALIAS Asiento EXCLUSIVE

	vIndi1=SYS(3)+".IDX"
	
	SELE Diar
	INDEX ON LEFT(COMPROB,2)+CUENTA+CODFTE TO (vInd1) FOR IIF(ALLTRIM(vajustado)='Ajustado',IIF(LEFT(Comprob,2)#'64',MONTH(Fecemi)=VAL(xMesF),MONTH(Fecemi)<=VAL(xMesF)),MONTH(Fecemi)=VAL(xMesF));
	 .AND. (YEAR(Fecemi)-1900) = QAno  AND Comprob<>'05'

	COPY STRU TO temp88
	COPY STRU TO Tempo3
	COPY STRU TO (vindz)
	COPY STRU TO (vindW)
	COPY STRU TO (vindM)

	USE AUXDIAR IN 19 ALIAS temp88 EXCLUSIVE
	SELE temp88
	ZAP
	
	DO AsiPre 
	SELE temp88
	IF FILE((VINDV))
		APPEND FROM (vindv)
	ENDIF
	IF FILE((VINDV))
		SELE Presu
		USE 
		ERASE (VINDV)
	ENDIF

	DO AsiObra1
	DO AsiPla
*	DO AsiCts 
	DO Cruce  && Se graba a la base temporal las cuentas con sus saldos
	DO AsieCent
	DO NotConRet
	DO CajaChica


	IF FILE((VINDM))
		SELE TempO5
		DELE FOR Cuenta='101'
		PACK
	ENDIF
	
    *USE AUXDIAR IN 19 ALIAS temp88 EXCLUSIVE
	SELE temp88
	IF FILE(('Temp1.DBF'))
		APPEND FROM Temp1
	ENDIF

	IF FILE((VINDM))
		APPEND FROM (vindM)
		SELE TEMPO5
		USE
		ERASE (vindM)
	ENDIF

	SELE temp88
	IF FILE((VINDS))
		APPEND FROM (vindS)
		SELE Caja
		USE
		ERASE (vindS)
	ENDIF

	SELE temp88
	IF FILE((VINDO))
		APPEND FROM (vindO)
		IF USED("obra")
			SELE Obra
			USE
			ERASE (vindO)
		ENDIF	
	ENDIF

	SELE temp88
	IF FILE((VINDJ))
		APPEND FROM (vindJ)
		* SELE CERTF
		* USE
		* ERASE (vindJ)
	ENDIF

	SELE temp88
	IF FILE((VINDP))
		APPEND FROM (vindP)
		* SELE Planilla
		* USE
		* ERASE (vindp)
	ENDIF

	SELE temp88
	IF FILE((VINDc))
		APPEND FROM (vindc)
		* SELE Cts
		* USE
		* ERASE (vindc)
	ENDIF

	SELE temp88
	IF FILE((VINDQ))
		APPEND FROM (vindQ)
		* SELE Mano
		* USE
		* ERASE (vindQ)
	ENDIF
	
	*USE Folios  IN 10 ORDER TAG Folios1         ALIAS Folios
	
	SELE temp88
	REPLACE ALL COMPROB WITH 'SS' FOR Comprob='64'
	xDia = CTOD('31/12/'+ALLTRIM(ZANO))
	REPLACE FecEmi WITH xDia FOR Comprob='SS'
	
	*----
*    INDEX ON LEFT(comprob,2)+DTOC(FECEMI) TO (vInd41)  FOR IIF(ALLTRIM(vAjustado)="Ajustado",.T.,LEFT(Comprob,2)<>'SS')
    *----
  
	GO TOP
	vCont = 0
	vOldG = '00'
*SUSP	
INDEX ON ORDEN+TIPO+CUENTA TO ORDEN FOR IIF(ALLTRIM(vAjustado)="Ajustado",.T.,LEFT(Comprob,2)<>'SS')
SCAN FOR MONTH(FecEmi)=val(xMesF) and SUBS(ALLTRIM(STR(YEAR(FecEmi))),3,2) = STR(FAno,2)
      xTipDoc = LEFT(ALLTRIM(Temp88.Comprob),2)
      xOrden  = Temp88.Orden
      xMes    = PADL(ALLTRIM(STR(MONTH(Temp88.FecEmi))),2,'0')
      xCuenta = LEFT(Temp88.Cuenta,2)
      @ 0,0 say Temp88.comprob
      xMonto  = Temp88.Montos
      SELE Tempo
      vProc = IIF( SEEK(xMes+xOrden+xTipDoc+xCuenta) , f_lock(1) , f_appd() )
      
      IF vOldG<>LEFT(Temp88.comprob,2)
         vCont = vCont+1
         vOldG = LEFT(Temp88.comprob,2)
      ENDIF

IF XCUENTA='10'
*	SUSP
ENDIF
      
      IF vProc
         xtipo=Temp88.Tipo
         REPLACE Tempo.TipDoc WITH xTipDoc ,;
                 Tempo.Mes    WITH xMes    ,;
                 Tempo.ORDEN  WITH xOrden  ,;
                 Tempo.Cuenta WITH xCuenta ,;
                 Tempo.DFolio WITH DRetFolio(xOrden,xMes,xcuenta,xTipo),;
                 Tempo.HFolio WITH HRetFolio(xOrden,xMes,xcuenta,xTipo),;
                 Tempo.Grupo  WITH PADL(ALLTRIM(STR(vCont)),3,'0')
*                 Tempo.DFolio WITH DRetFolio(xTipDoc,xMes,xcuenta,xTipo)
*                 Tempo.HFolio WITH HRetFolio(xTipDoc,xMes,xcuenta,xTipo)

         IF Temp88.Tipo='D'
            REPLACE Tempo.Debe  WITH Tempo.Debe+xMonto
         ELSE
            REPLACE Tempo.Haber WITH Tempo.Haber+xMonto
         ENDIF
      ENDIF
      SELE Temp88
ENDSCAN
DO ESPERA WITH 2
SELECT TempO
GO TOP

IF EOF()
   DO standby WITH 'No hay informaci¢n para procesar.'
ELSE
   DO prep_des
   DO GenRep WITH FMes,FAno
ENDIF
RETURN




PROCEDURE GenRep
*---------------
PARAMETERS pMes,pAno
RepoMes = pMes
RepoAno = pAno

*- Crea indice en parmae para buscar el folio:

SELECT Parma
SET INDEX TO
INDEX ON codigoaux TO (IndAux2) FOR Tipo='TIPDOC'

SELECT Tempo
INDEX ON Cuenta+Mes+TipDoc TO (indAux)
SET RELATION TO PADR(Tempo.Cuenta,10,'0') INTO Cuen
SET RELATION TO Tempo.TipDoc INTO Parma ADDITIVE
SELE TEMPO
SET FILTER TO
INDEX ON Cuenta+Mes+TipDoc TO (indAux)
*--------
DO Acu101
*--------
GO TOP
INDEX ON CUEN.TIPO+Cuenta+Mes+TipDoc TO (indAux)
DO REPORTE WITH 2,'MAYOR1','  Libro Mayor de '+alltrim(mfecha(XMESf))+' a '+alltrim(mfecha(str(pmes)))+'  '
RETURN


PROCEDURE Termina
*----------------
CLOSE DATABASES
ERASE(FilAux)
ERASE(IndAux)
ERASE(IndAux2)
ERASE(vind)
ERASE(vind2)
ERASE(vind3)
ERASE(vind41)
ERASE(vind51)
ERASE(vind61)
RETURN


FUNCTION DRetFolio
*----------------
PARAMETERS xTD,xM,xcta,xTp
vali=ALIAS()
SELE Folios
SEEK ('LIBDIR'+xM+zano+xtd+xcta+'00000000')
IF FOUND() 
   SELE (VALI)
   RETURN PADL(ALLTRIM(STR(Folios.DFolio,3)),3,'0')
ENDIF

SELE (VALI)
RETURN ' '


FUNCTION HRetFolio
*----------------
PARAMETERS xTD,xM,xcta,xTp
vali=ALIAS()
SELE Folios
SEEK ('LIBDIR'+xM+zano+xtd+xcta+'00000000')
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




FUNCTION MFecha
*--------------
PARAMETER XMES1,XANO
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
 IF PARAMETERS()=1
   XRET = ALLTRIM(SUBSTR(Meses,VAL(XMes1)*9-8,9)) 
 ELSE
   XRET = ALLTRIM(SUBSTR(Meses,VAL(XMes1)*9-8,9)) + ' ' + ALLTRIM(STR(XAno,2))
ENDIF
RETURN XRET


PROCEDURE  GeneraMay2 
*-------------------------
PARAMETER wmes,wano

SELE Comp
SET ORDER TO  TAG Compro7

SELE Diar 
SET ORDER TO  TAG Diario11
SET RELATION TO Comprob+Comadi+Codfte INTO Comp  
*VIC
Sele Diar
DO ESPERA WITH 1, "Reporte del Mayor Detallado en Proceso...!"
INDEX ON Diar.Cuenta+Diar.Comprob+Diar.Comadi+Diar.Codfte to (VIND) FOR ;
MONTH(Diar.Fecemi)=wmes AND YEAR(Diar.fecemi)=(1900+wano) AND;
IIF(ALLTRIM(vAjustado)="Ajustado",.T.,LEFT(Diar.Comprob,2)<>"64") 

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
IF WMES=1
   v1DSaldona=0
   SELE Diar
   RETURN (v1DSaldona)
ELSE   

   * SEEK (SUBSTR(Diar.Cuenta,1,3)+'0000000')+STR(WMES-1,2)+STR(WANO,2)
   SEEK (SUBSTR(Diar.Cuenta,1,3)+'0000000')+STR(WMES-1,2)+STR(WANO,2)
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
IF WMES=1
         v1DSaldona=0
        SELE Diar
	    RETURN (v1DSaldona)
ELSE   
   * SEEK (SUBSTR(Diar.Cuenta,1,2)+'000000')+STR(WMES-1,2)+STR(WANO,2)
   SEEK (SUBSTR(Diar.Cuenta,1,2)+'00000000')+STR(WMES-1,2)+STR(WANO,2)
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
IF WMES=1
   v1DSaldona=0
   SELE Diar
   RETURN (v1DSaldona)
ELSE   
   * SEEK (Diar.Cuenta)+STR(WMES-1,2)+STR(WANO,2)
   SEEK (Diar.Cuenta)+STR(WMES-1,2)+STR(WANO,2)
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

FUNCTION Rambito  && Para que en el reporte aparezca de que se trata.
*---------------
PARAMETER am
RETURN IIF(alltrim(am)='Sede','PROGRAMA : SEDE CENTRAL',IIF(alltrim(am)='Adm.Cen','PROGRAMA : ADMINISTRACION CENTRAL','PLIEGO 01: REGION GRAU'))


PROCEDURE prep_des
*-----------------
DO ESPERA WITH 1, 'Creando el Detalle del Mayor'
*SUSP
FOR j=1 to val(zmes)
    *- Por cada mes tiene que blanquear la base temporal
    SELECT grupos
    ZAP
    *- Indexa el archivo temporal del mayor para el mes correspondiente a 'j'
    SELECT Tempo
    INDEX ON GRUPO TO (vind3) UNIQUE for tempo.mes=PADL(ALLTRIM(STR(j,2)),2,'0')
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
    INDEX ON GRUPO TO (vind3) FOR tempo.mes = PADL(ALLTRIM(STR(j,2)),2,'0')


    *- Una vez que se tiene un archivo temporal con todos los grupos
    *- del mes actual (j),
    *- se procede a actualizar el campo detalle

    SELECT grupos
    SCAN
          @ 0,0 SAY grupos.grupo+'( Mes:'+PADL(ALLTRIM(STR(j,2)),2,'0')+')'
          SELECT tempo
          INDEX ON tempo.grupo TO (vind51) FOR tempo.grupo=grupos.grupo AND tempo.debe>0 and tempo.mes=PADL(ALLTRIM(STR(j,2)),2,'0')
          IF !EOF()
             COUNT TO vNum
             IF vNum=1 
                GO TOP
                xcuenta = tempo.cuenta
          		INDEX ON tempo.grupo TO (vind51) FOR tempo.grupo=grupos.grupo AND tempo.haber>0 and tempo.mes=PADL(ALLTRIM(STR(j,2)),2,'0')
                SCAN
                   REPLACE tempo.hDeta WITH xcuenta
                ENDSCAN
            ENDIF
          ENDIF

         INDEX ON tempo.grupo TO (vind51) FOR tempo.grupo=grupos.grupo AND tempo.haber>0 and tempo.mes=PADL(ALLTRIM(STR(j,2)),2,'0')
         IF !EOF()
            COUNT TO vNum
            IF vNum=1 
                 GO TOP
                 xcuenta = tempo.cuenta
                INDEX ON tempo.grupo TO (vind51) FOR tempo.grupo=grupos.grupo AND tempo.debe>0  and tempo.mes=PADL(ALLTRIM(STR(j,2)),2,'0')
                 SCAN
                     REPLACE tempo.dDeta WITH xcuenta
                ENDSCAN
            ENDIF
        ENDIF

        SELECT grupos
     
   ENDSCAN
ENDFOR          
DO ESPERA WITH 2    
RETURN
     

FUNCTION Guardafol
*-----------------
 PARAMETER xOrden,xcuenta,vmes,xpagina
 xCuenta=LEFT(xCuenta,2)+'00000000'
 SELE Folios
 IF SEEK ('LIBMAY'+vmes+zano+xOrden+xcuenta)
     Bloquea = f_lock(1)
 ELSE
     Bloquea = f_appd()
 ENDIF
 IF Bloquea
    REPLACE Reporte WITH 'LIBMAY',;
            Mes     WITH vMes,;
		    Periodo WITH ZAno,;
		    Orden   WITH xOrden,;
		    Tipdoc  WITH Tempo.TipDoc,;
		    Cuenta  WITH xCuenta,;
		    Dfolio  WITH xpagina,;
		    Hfolio  WITH xpagina
ENDIF
SELECT Tempo
RETURN ' '

FUNCTION DAcuAnt
*---------------
PARAMETERS xcuenta,xmes1
vAli=SELECT()
SELECT SalD
vMes = VAL(xMes1)-1
vAno = VAL(ZAno)
vret=0
IF ALLTRIM(vAjuStado)="Ajustado"
	FOR J=0 TO vMes
		IF VMES<>0
	    	SEEK LEFT(tempo.cuenta,2)+'00000000'+ STR(j,2) + STR(vAno,4)
	    	vRet = vret + IIF(FOUND(),salD.DSumana,0)
	    ENDIF
	ENDFOR    
ELSE
	FOR J=0 TO vMes
		IF VMES<>0
	    	SEEK LEFT(tempo.cuenta,2)+'00000000'+ STR(j,2) + STR(vAno,4)
	    	vRet = vret + IIF(FOUND(),salD.DSumana,0)
	    ENDIF
	ENDFOR    
ENDIF
SELECT(vAli)
RETURN vRet


FUNCTION HAcuAnt
*---------------
PARAMETERS xcuenta,xmes1
vAli=SELECT()
SELECT SalD
vMes = VAL(xMes1)-1
vAno = VAL(ZAno)
vret = 0

IF ALLTRIM(vAjuStado)="Ajustado"
	FOR J=0 TO vMes
		IF VMES<>0
	    	SEEK LEFT(tempo.cuenta,2)+'00000000' + STR(j,2) + STR(vAno,4)
	   		vRet = vret + IIF(FOUND(),salD.hSumana,0)
	   	ENDIF
	ENDFOR
ELSE
	FOR J=0 TO vMes
		IF VMES<>0
	    	SEEK LEFT(tempo.cuenta,2)+'00000000' + STR(j,2) + STR(vAno,4)
	   		vRet = vret + IIF(FOUND(),salD.hSumana,0)
	   	ENDIF
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
   vret= Cuen.Descri
ENDIF
SELECT(vAli)
RETURN ALLTRIM(vRet)

*- Este procedimiento genera un asiento aparte, de las H/C que utilizan
*- la partida 6.5... por las obras.	
PROCEDURE AsiObra1
*------------------
SELE Diar
SET FILTER TO
SET FILTER TO LEFT(CodPart,2)='65' AND Cuenta<>'9' AND MONTH(FECEMI)=VAL(xMesF)
GO TOP
IF !EOF()
	COPY STRU TO &vindO
	USE &vindO IN 21 ALIAS Obra EXCLUSIVE
	SELE Diar
	GO TOP
	SCAN	
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)
		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------
		SELE Obra
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '29'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '29'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE Diar
	skip-1	
	ENDSCAN
ENDIF
SELE DIAR
SET FILTER TO

*- Este procedimiento realiza los asientos aparte de las H/C
*- de planillas.

PROCEDURE AsiPla
*-----------------
SELE Diar
SET FILTER TO TIPDOC='PLA' AND CUENTA<>'9' AND MONTH(FECEMI)=VAL(XMESF)
GO TOP
IF !EOF()
	COPY STRU TO &vindP
	USE &vindP IN 22 ALIAS Planilla EXCLUSIVE
	SELE DIAR
	GO TOP
	SCAN
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)

		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------

		SELE Planilla
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '30'		
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and  F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '30'		
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE DIAR
	SKIP-1
	ENDSCAN
ENDIF
SET FILTER TO
RETURN

PROCEDURE AsiCts
*-----------------
SELE Diar
SET FILTER TO COMPROB='03' AND TIPDOC='RD' AND CUENTA<>'9';
AND MONTH(FECEMI)=VAL(xMesF) and codanal='01.15'

GO TOP

IF !EOF()
	COPY STRU TO &vindc
	USE &vindc IN 0 ALIAS Cts EXCLUSIVE
	SELE DIAR
	GO TOP
	SCAN
		SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,5)
		vComprob = LEFT(Diar.Comprob,2)

		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF Tipo = 'D'
	    	   	vDMonto = vDMonto + Montos
			ELSE
			   	vHMonto = vHMonto + Montos
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto
		*--------------------------------------

		SELE cts
			IF vmontoD>0 and F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '22'
				REPLACE Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE tipo   WITH 'D'
				REPLACE Montos WITH vMontoD
			ENDIF
			IF vmontoH>0 and  F_APPD()
				GATHER MEMVAR
				REPLACE Comprob WITH '22'
				REPLACE Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		SELE DIAR
	SKIP-1
	ENDSCAN
ENDIF
SET FILTER TO
RETURN


PROCEDURE CRUCE
*--------------
Graba = .T.
SELE Diar
SET FILTER TO Cuenta<>'9' AND tipdoc<>'PLA' AND LEFT(CodPart,2)<>'65' ;
AND TIPOBR<>'N' AND CAJCHI<>'S' AND IIF(ALLTRIM(vajustado)='Si',IIF(LEFT(Comprob,2)#'64',MONTH(Fecemi)=VAL(xMesF),MONTH(Fecemi)<=VAL(xMesF)),MONTH(Fecemi)=VAL(xMesF))

GO TOP
SCAN
	SCATTER MEMVAR
	DO CASE
		CASE LEFT(Diar.Cuenta,1)='0'
			vCuen    = LEFT(Diar.Cuenta,4)
			vComprob = LEFT(Diar.Comprob,2)
		OTHERWISE
			vCuen    = LEFT(Diar.Cuenta,5)
			vComprob = LEFT(Diar.Comprob,2)
	ENDCASE

	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(LEFT(Cuenta,5) = vCUEN),(LEFT(Cuenta,4) = vCUEN))
	  IF LEFT(Diar.Comprob,2)$'18 16 73' and Diar.CodDep="113000"
	     * No debe de pasar por ser  del Asicent
	  ELSE
	  
	    IF LEFT(Diar.Comprob,2)$'18 16 73'
	       m.Coddep=Diar.Coddep
	    ENDIF
	       
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
		   	vHMonto = vHMonto + Montos
		ENDIF
	ENDIF	
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------
	
	SELE temp88
		IF m.Coddep='113000' AND m.Cuenta<>'9' AND m.Comprob<>'99' AND m.Comprob<>'06' 
		&& No los graba, porque Aparecen en el Asiento de Centralizacion.
    		Graba = .F.
		ELSE
			if m.tipdoc='RD' and m.codanal='01.15'
				Graba = .F.
			else
				Graba = .T.
			endif	
		ENDIF           
		
		IF Graba 
			IF  vMontoD # 0 AND F_APPD() 
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF  vMontoH # 0 AND F_APPD() 
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		ENDIF
	SELE DIAR
	SKIP-1
ENDSCAN
SELE Diar
SET FILTER TO
RETURN


PROCEDURE AsieCent && Genera el asiento de Centralizacion
*-----------------
Graba = .T.
SELE Diar

*-Esta base temporal guarda solo el asiento de centralizacion
COPY STRU TO Temp1 
USE Temp1 IN 17 ALIAS Temp1 EXCLUSIVE
*-
SELE Diar

COPY TO &vindx FOR  (Coddep='113000') AND (Cuenta<>'9') AND Comprob<>'99' AND Comprob<>'76' AND LEFT(Comprob,1)<>'N' AND LEFT(Comprob,1)<>'Z' AND Comprob<>'06' AND Comprob<>'11' AND Comprob<>'68' AND Comprob<>'00';
AND Comprob<>'98' AND Cuenta<>'0' AND TIPOBR<>'N' AND CAJCHI<>'S'  AND MONTH(FECEMI)=VAL(xMesF)

* No se considera los c/p porque se les hace un proceso aparte sin retenciones.

USE &vindx IN 16 ALIAS Temp3 EXCLUSIVE

SELE Temp3

*-------------
DO QuitaReten
*-------------

SELE Temp3
APPEND FROM (vindg)

* Ahora se graban con diferente Comprob  para diferenciar los ingresos y los egresos de caja.
* Se crea esta base para no modificar diario.

GO TOP
SCAN
	IF Temp3.tipo='H'
		IF Temp3.Cuenta='101' 
			REPLACE COMPROB WITH 'RR'+LEFT(Comprob,2)
		ELSE
		 	REPLACE COMPROB WITH 'GG'+LEFT(Comprob,2)
		ENDIF
	ELSE
 		 IF Temp3.Cuenta='101' 
		 	REPLACE COMPROB WITH 'GG'+LEFT(Comprob,2)
		 ELSE
			REPLACE COMPROB WITH 'RR'+LEFT(Comprob,2)
		 ENDIF
	ENDIF				 				 				 
ENDSCAN

* Ahora se graba a temp1 
SELE Temp3

INDEX ON LEFT(COMPROB,2)+CUENTA+CODFTE TO (vIndi2) FOR MONTH(Fecemi)=VAL(xMesF);
 .AND. (YEAR(Fecemi)-1900) = FAno   AND Reten<>'S'
GO TOP
SCAN
	SCATTER MEMVAR
	vCuen    = LEFT(Temp3.Cuenta,5)
	vComprob = LEFT(Temp3.Comprob,2)
	*vMonto   = Saldo1(vCuen,Vcomprob)

	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE (LEFT(Comprob,2)=vComprob) AND IIF(LEN(vCuen)=5,(SUBSTR(Cuenta,1,5) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
		IF Tipo = 'D'
	       	vDMonto = vDMonto + Montos
		ELSE
		   	vHMonto = vHMonto + Montos
		ENDIF
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------

	SELE Temp1
		IF Graba  
			IF  vMontoD >0 AND F_APPD()
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF  vMontoH >0 AND F_APPD()
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
		ENDIF
	SELE Temp3
	SKIP-1
ENDSCAN
*SELE Temp3
SELE TEMP1
*---------------------
DELE FOR CUENTA='101'
PACK
*--------------------
GO TOP
RETURN


*QUITA EL MONTO DE LA RETENCION A LOS C/P QUE TIENEN RETENCION

PROCEDURE QuitaReten
*-------------------
* Se quita al monto mayor del c/p la suma  de las retenciones
SELE Diar
COPY STRU TO (vindF)
COPY STRU TO (vindG)
USE &vindf IN 22 ALIAS CP  EXCLUSIVE
USE &vindg IN 23 ALIAS CP1 EXCLUSIVE

* Se graban los datos a la temporal para no modificar diario
SELE Diar
SET FILTER TO Comprob='98' AND CUENTA<>'101' AND CUENTA<>'9' AND CajChi<>'S';
AND MONTH(FECEMI)=VAL(xMesF)

GO TOP
SCAN
	SCATTER MEMVAR
	SELE Cp
	APPEND BLANK
	GATHER MEMVAR
	SELE DIAR
ENDSCAN

SELE CP
vind3= SYS(3) + '.idx'
vind4= SYS(3) + '.idx'
INDEX ON ALLTRIM(Comprob)+Tipo  TO (vind3)
INDEX ON ALLTRIM(Comprob)+cuenta+Tipo TO (vind4)

* Para acumular el monto de todas las retenciones del c/p.
SET INDEX TO (vind3)
GO TOP
SCAN
    Tiene   = .F.
	vComp1  = ALLT(CP.Comprob)
	vComp2  = ALLT(CP.Comprob)
    vRecno  = RECNO()	
	STORE 0 TO SumaRet

	SCAN WHILE vComp1=vComp2
		IF Reten='S'
			Tiene   = .T.
			SumaRet = SumaRet + CP.Montos
		ENDIF
		vComp2=ALLT(CP.Comprob)
	ENDSCAN
	SKIP -1
	vRec1  = RECNO()
	
    *----------------------- 
	* Si tiene retenciones
	IF Tiene
		SELE CP
		GO (vRecno)
		SEEK vcomp1+'D'
		* Para encontrar la cuentas del Debe con mayor monto
		vComp11  = ALLT(CP.Comprob)
		vComp22  = ALLT(CP.ComproB)
		mayor   = 0
		SCAN WHILE vComp11=vComp22 AND Tipo='D'
			IF CP.Montos > Mayor
				Mayor = CP.Montos
				vComp = CP.Comprob
				vCuen = CP.Cuenta
			ENDIF
			vComp22=ALLT(CP.Comprob)
		ENDSCAN

		* Ahora se reemplaza el monto mayor del debe menos las retenciones. 
		SELE CP
		SET INDEX TO (vind4)
		SEEK vComp11+vCuen+'D'
		IF FOUND()
			REPLACE CP.Montos WITH CP.Montos-SumaRet
		ENDIF
		SET INDEX TO (vind3)
	ENDIF
	*---------------------------
	GO (VREC1)		
ENDSCAN

SELE CP
SET FILTER TO
SET INDEX TO (vind4)

* Se acumula los saldos a nivel de divisionaria
SET FILTER TO Comprob='98' AND CUENTA<>'9' AND CUENTA<>'101' AND RETEN<>'S'

GO TOP
SCAN
	SCATTER MEMVAR
	vCuen    = LEFT(CP.Cuenta,3)
	vComprob = CP.Comprob
	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE Comprob=vComprob AND SUBSTR(Cuenta,1,3) = vCUEN
		IF Tipo = 'D'
	       	vDMonto = vDMonto + CP.Montos
		ELSE
		   	vHMonto = vHMonto + CP.Montos
		ENDIF
	ENDSCAN
	vMontoD = vDmonto
	vMontoH = vHmonto
	*--------------------------------------
	SELE CP1
		IF  vMontoD >0 AND F_APPD() 
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vMontoD
			ENDIF
			IF  vMontoH > 0 AND F_APPD() 
				GATHER MEMVAR
				REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vMontoH
			ENDIF
	SELE CP
	SKIP-1
ENDSCAN
RETURN



PROCEDURE NotConRet
*-------------------
SELE Diar
SET FILTER TO
USE &vindz   IN 25 ALIAS Tempo77 EXCLUSIVE
USE &vindW   IN 18 ALIAS Tempo1 EXCLUSIVE
USE &vindM   IN 15 ALIAS Tempo5 EXCLUSIVE
*FEFE
SELE Diar
SET FILTER TO (Comprob='98') AND (Cuenta<>'101'  AND Cuenta<>'9' AND MONTH(FECEMI)=VAL(xMesF))
GO TOP
SCAN
	SCATTER MEMVAR
	SELE Tempo77
	APPEND BLANK
	GATHER MEMVAR
	SELE Diar
ENDSCAN

SELE Tempo77
vind9 = SYS(3) + '.idx'
vind10= SYS(3) + '.idx'
INDEX ON ALLTRIM(Comprob)+Tipo  TO (vind9)
SET INDEX TO (vind9)

* Se verifica cada c/p

SCAN
	vComp1  = ALLT(tempo77.Comprob)
	vComp2  = ALLT(tempo77.ComproB)
	mayor   = 0
	* Para encontrar la cuentas del Debe con mayor monto
	SCAN WHILE vComp1=vComp2 AND Tipo='D'
		IF tempo77.Montos > Mayor
			Mayor = tempo77.Montos
			vComp = tempo77.Comprob
			vCuen = tempo77.Cuenta
		ENDIF
		vComp2=ALLT(tempo77.Comprob)
	ENDSCAN

	* La contracuenta de las retenciones es la cuenta del debe, con
	* mayor monto

	SELE Tempo77
	vComp11  = VCOMP1
	vComp22  = VCOMP1
	VRECNO = RECNO()

	SEEK allt(vcomp1) +'H'
	IF FOUND()
		DO WHILE allt(vComp11) = allt(vComp22)

			IF RETEN='S' 
				RCuenta = Tempo77.Cuenta && Cuenta de Retencion
				RMontos = Tempo77.Montos
				SELE Tempo1
				FOR I=1 TO 2
					IF I=1
						IF F_APPD()
							REPLACE Comprob WITH 'EE'++LEFT(Tempo77.Comprob,2)     && EE=Retenciones
							REPLACE orden with '34'
							REPLACE FecEmi  WITH Tempo77.FecEmi
							REPLACE Tipo    WITH 'D'		
							REPLACE Cuenta  WITH vCuen	
							REPLACE Montos  WITH Rmontos
						ENDIF
					ELSE
						IF F_APPD()
							REPLACE Comprob WITH 'EE'+LEFT(Tempo77.Comprob,2)     && EE=Retenciones
							REPLACE orden with '34'
							REPLACE FecEmi  WITH Tempo77.FecEmi
							REPLACE Tipo    WITH 'H'		
							REPLACE Cuenta  WITH RCuenta		
							REPLACE Montos  WITH Rmontos
						ENDIF
					ENDIF
				ENDFOR
			ENDIF
			SELE Tempo77
			IF !EOF()
				SKIP
			ELSE
				EXIT
			ENDIF
			vComp22 = allt(Tempo77.Comprob)

		ENDDO
		SELE Tempo77
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
	vCuen    = LEFT(tempo1.Cuenta,5)
	vComprob = ALLTR(tempo1.Comprob)
		
	*-------------------------------------
	STORE 0 TO vdmonto,vhmonto
	SCAN WHILE Comprob=vComprob AND SUBSTR(Cuenta,1,5) = vCUEN
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
		IF  vMontoD >0 AND F_APPD() 
			GATHER MEMVAR
			REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
			REPLACE  tipo   WITH 'D'
			REPLACE  Montos WITH vMontoD
		ENDIF
		IF  vMontoH > 0 AND F_APPD() 
			GATHER MEMVAR
			REPLACE  Cuenta WITH LEFT(m.Cuenta,5)+'00000'
			REPLACE  tipo   WITH 'H'
			REPLACE  Montos WITH vMontoH
		ENDIF
	SELE tempo1
	SKIP-1
ENDSCAN
RETURN

* ESTE PROCEDIMIENTO YA NO SE DEBE REALIZAR A PARTIR DE AGOSTO
PROCEDURE CajaChica
*-------------------
SELE Diar
SET FILTER TO

COPY TO &vindS FOR  (COMPROB='98' AND CajChi='S' AND CUENTA<>'101' AND CUENTA<>'9');
AND MONTH(FECEMI)=VAL(xMesF)

USE &vindS IN 24 ALIAS Caja

SELE Caja
GO TOP
SCAN
	REPLACE COMPROB WITH '77'
ENDSCAN
RETURN



PROCEDURE AsiPre
*---------------
* PARAMETER QMES
SELE Diar
SET FILTER TO CUENTA='9' MONTH(Fecemi)=VAL(xMesF)
COPY STRU TO (vindv)
USE (vindv) IN 20  ALIAS Presu EXCLUSIVE
SELE Diar
GO TOP
SCAN
	SCATTER MEMVAR
		vCuen    = LEFT(Diar.Cuenta,4)
		vComprob = LEFT(Diar.Comprob,2)
		vOrd     = Diar.Orden
		*-------------------------------------
		STORE 0 TO vdmonto,vhmonto,vdanoant,vhanoant
		SCAN WHILE (LEFT(Comprob,2)=vComprob) AND Orden=vOrd AND IIF(LEN(vCuen)=4,(SUBSTR(Cuenta,1,4) = vCUEN),(SUBSTR(Cuenta,1,2) = vCUEN))
			IF LEFT(Comprob,2)='14' AND ORDEN='08'
				IF Tipo = 'D'
	    	   		vdanoant = vdanoant + Montos
				ELSE
			   		vhanoant = vhanoant + Montos
				ENDIF
			ELSE	
				IF Tipo = 'D'
	    	   		vDMonto = vDMonto + Montos
				ELSE
			   		vHMonto = vHMonto + Montos
				ENDIF
			ENDIF
		ENDSCAN
		vMontoD = vDmonto
		vMontoH = vHmonto

		SELE Presu
		
		IF vdanoant>0 or vhanoant>0
	    	IF vdanoant>0 and F_APPD()
				GATHER MEMVAR
				REPLACE COMPROB WITH '69'
				REPLACE orden   WITH '08'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,4)+'000000'
				REPLACE  tipo   WITH 'D'
				REPLACE  Montos WITH vdanoant
			ENDIF
			IF vhanoant>0 and F_APPD()
				GATHER MEMVAR
				REPLACE COMPROB WITH '69'
				REPLACE orden   WITH '08'
				REPLACE  Cuenta WITH LEFT(m.Cuenta,4)+'000000'
				REPLACE  tipo   WITH 'H'
				REPLACE  Montos WITH vhanoant
			ENDIF
		ENDIF	
		IF vmontoD>0 and F_APPD()
			GATHER MEMVAR
			IF COMPROB='18'
				REPLACE COMPROB WITH '18'
			ELSE
				REPLACE COMPROB WITH '69'
			ENDIF	
			REPLACE  Cuenta WITH LEFT(m.Cuenta,4)+'000000'
			REPLACE  tipo   WITH 'D'
			REPLACE  Montos WITH vMontoD
		ENDIF
		IF vmontoH>0 and F_APPD()
			GATHER MEMVAR
			IF COMPROB='18'
				REPLACE COMPROB WITH '18'
			ELSE
				REPLACE COMPROB WITH '69'
			ENDIF	
			REPLACE  Cuenta WITH LEFT(m.Cuenta,4)+'000000'
			REPLACE  tipo   WITH 'H'
			REPLACE  Montos WITH vMontoH
		ENDIF
	SELE Diar
	SKIP-1
ENDSCAN
SELE Diar
SET FILTER TO
RETURN

PROCEDURE AntAcu101
*------------------
PRIVATE vAlias
vAlias=ALIAS()
SELECT Diar
FOR i=VAL(XMESf) TO VAL(xmesF)
	SUM Montos TO vDeb101(i) FOR LEFT(Cuenta,3)="101"  AND Tipo='D' AND MONTH(Fecemi)=i AND LEFT(Comprob,2)<>"00"
	SUM Montos TO vHab101(i) FOR LEFT(Cuenta,3)="101"  AND Tipo='H' AND MONTH(Fecemi)=i AND LEFT(Comprob,2)<>"00"
ENDFOR

SELECT Tempo
*Se agregan montos de la 101
FOR i=VAL(XMESf) TO VAL(xmesF)
	*-Para el Debe
	SEEK "10"+ PADL(ALLTRIM(STR(I,2)),2,"0") + "GG"
	IF FOUND()
	    REPLACE DFolio WITH DRetFolio("35",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","D")
*	    REPLACE DFolio WITH DRetFolio("GG",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","D")
	ENDIF
	
   	*-Para el Haber
   	SEEK "10"+ PADL(ALLTRIM(STR(I,2)),2,"0") + "RR"
	IF FOUND()
	    REPLACE HFolio WITH HRetFolio("36",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","H")
*	    REPLACE HFolio WITH HRetFolio("RR",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","H")
	ENDIF
ENDFOR
SELECT (Valias)
RETURN



PROCEDURE Acu101
*-----------------
PRIVATE vAlias
vAlias=ALIAS()
SELECT Diar
FOR i=VAL(XMESf) TO VAL(xmesF)
	SUM Montos TO vDeb101(i) FOR LEFT(Cuenta,3)="101"  AND Tipo='D' AND MONTH(Fecemi)=i AND LEFT(Comprob,2)<>"00"
	SUM Montos TO vHab101(i) FOR LEFT(Cuenta,3)="101"  AND Tipo='H' AND MONTH(Fecemi)=i AND LEFT(Comprob,2)<>"00"
ENDFOR

SELECT Tempo
*Se agregan montos de la 101
	FOR i=VAL(XMESf) TO VAL(xmesF)
		*-Para el Debe
		SEEK "10"+ PADL(ALLTRIM(STR(I,2)),2,"0") + "GG"
		IF FOUND()
			guagru=Tempo.Grupo
		ENDIF
		APPEND BLANK
		REPLACE TipDoc WITH "GG",;
				Orden  WITH '35',;
				Mes	   WITH PADL(ALLTRIM(STR(I,2)),2,"0"),;
				Cuenta WITH "10",;
		    	DFolio WITH DRetFolio("35",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","D"),;
			    Grupo  WITH guagru;
				Debe   WITH vDeb101(i)

*		    	DFolio WITH DRetFolio("GG",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","D")
    	
	   	*-Para el Haber
   		SEEK "10"+ PADL(ALLTRIM(STR(I,2)),2,"0") + "RR"
		IF FOUND()
			guagru2=Tempo.Grupo
		ENDIF
   		APPEND BLANK
		REPLACE TipDoc 	WITH "RR",;
			Orden 		WITH '36',;
			Mes 		WITH PADL(ALLTRIM(STR(I,2)),2,"0"),;
			Cuenta		WITH "10",;	
		    HFolio		WITH HRetFolio("36",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","H"),;
		    Grupo	 	WITH guagru2,;
		   	Haber		WITH vHab101(i)

*		    HFolio		WITH HRetFolio("RR",PADL(ALLTRIM(STR(I,2)),2,"0"),"10","H")

	ENDFOR
SELECT (Valias)
RETURN



*Funciones del Libro Mayor
*GuardaFol(Tempo.Tipdoc,Tempo.Cuenta,Tempo.mes,_pageno)