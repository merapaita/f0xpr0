* ---------------------------------------------------
** SALPRE.PRG
** Reporte Ejecucion ESTADOS
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------

* Abrir Archivos de trabajo

USE Parmae  IN 1 ORDER TAG Parmae1         ALIAS parma
USE Calen   IN 2 ORDER TAG Calen1          ALIAS Calen
USE Maepre  IN 3 order tag Maepre1         ALIAS Maepre
USE MaePar  IN 4 ORDER TAG MaePar1         ALIAS MaePar
USE ItePar  IN 5 ORDER TAG ItePar1         ALIAS ItePar
USE Clase   IN 6 ORDER TAG Clase1          ALIAS Clase
USE HojCon  IN 7 ORDER TAG Hojcon1         ALIAS Hoja
USE IteHc   IN 8 ORDER TAG Itehc1          ALIAS Itehc
USE Repoe5  IN 9                           ALIAS repo   
USE Repopre IN 10                          ALIAS repoP  

*----
PUBLIC TOT,VACUM,TOT1,TOTCOM
TOT = 0
TOT1 = 0
SELECT itehc
set rela to nummes+numhc into hoja
DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------
IF EOF()
   DO standby WITH 'No Existe registros a Procesar'
   RETURN
ENDIF
vtemp    = RECNO()
PUBLIC vFecini,vFecFin
as=order()
vPeriodo  = '  '
vTipFun   = ' '
vCodPrg   = '  '
vCodFte   = '   '
vCodSubPr = '   '
vCodAct   = '  '
vCodPry   = '   '
vCodSpy   = '  '
STORE date() to vFecFin
*STORE CTOD('01-01-95') TO vFecini
STORE DATE() - 30  TO vFecini
 DEFINE WINDOW Lis FROM 6,10 TO 17,70 DOUBLE ;
 TITLE ' °° SALDOS PARTIDAS °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '         Fechas : '
 @  3,2 say '       Programa : '
 @  4,2 say '         Fuente : '
 @  5,2 say '           Tipo : '
 @  6,2 say '    SubPrograma : '
 @  7,2 say '      Actividad : '
 @  8,2 say '       Proyecto : '

 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vFecIni    PICTURE '@D'  &&disable
 @  2,31  GET vFecFin    PICTURE '@D'  VALID vFecFin>=vFecIni
 @  3,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30) AND !EMPTY(vCodPrg)
 @  4,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30) AND !EMPTY(vCodFte)
 @  5,21  GET vtipfun    PICTURE '!'   VALID VAL_PARA(vTiPFun  ,'TIPFUN',' ',21,30)
 @  6,21  GET vCodSubPr  PICTURE '!!!' &&VALID VAL_CALE(vCodSubPr,'CODSUB'+VCodPrg,' ',21,30) 
 @  7,21  GET vCodAct    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'
 @  8,21  GET vCodPry    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I'

 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF LASTKEY()=27
    RETURN
 ELSE
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   
    SELECT REPO
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 9  ALIAS REPO

    SELECT REPOP
    vdbf = sys(3)+'.dbf'
    copy stru to (vdbf)
    USE (VDBF) IN 10  ALIAS REPOP

   
    if alltrim(vtipfun)='I'    
       sele hojA
         SET FILTER TO BETWEEN(hoja.FECHC,VFECINI,VFECFIN) .AND. substr(Codcal,8,2)=ALLTRIM(vCodPrg) .AND. ;
                  substr(CodCal,5,3)=ALLTRIM(vCodFte) .AND. IIF(!EMPTY(vCodSubPr),substr(CodCal,10,3)=ALLTRIM(vCodSubPr),.t.) .AND. ;
                  iif(!empty(vCodPry),substr(CodCal,13,3)=ALLTRIM(vCodPry),.t.) AND tipfun=alltrim(vtipfun) AND ESTADO#'99'
    else
    	SET FILTER TO BETWEEN(hoja.FECHC,VFECINI,VFECFIN) .AND. Codprg=ALLTRIM(vCodPrg) .AND. ;
                      CodFte=ALLTRIM(vCodFte) .AND. IIF(!EMPTY(vCodSubPr),CodSubPr=ALLTRIM(vCodSubPr),.t.) .AND. ;
                      iif(!empty(vCodAct),CodAct=ALLTRIM(vCodAct),.t.) .AND. ;
                      tipfun=alltrim(vtipfun) AND ESTADO#'99'
	endif                  
    GO TOP
    vInd = SYS(3) + '.IDX'
    xInd = SYS(3) + '.IDX'

    do acumula

    IF EOF()
       DEACTIVATE WINDOW STANDBY
       DO STANDBY WITH 'No se tiene registros a Procesar'
       SELE ITEHC
	   SET FILTER TO
    ELSE
	   SELECT REPO
	   INDEX ON IIF(ALLTRIM(VTIPFUN)='F',CODANAL,CODPART) TO (vInd) 
       SET INDEX TO (VIND)
       vInd = SYS(3) + '.IDX'
       SELECT REPOP
       IF ALLTRIM(VTIPFUN)='F'
	      DO PASAF
		ELSE	      
	      DO PASAI
	   ENDIF   
       DEACTIVATE WINDOW STANDBY
       SELE REPO
       SUM VALPART TO TOTCOM
       SELECT REPOP
       GO TOP
QQ
       DO REPORTE WITH 2,"salpre",' Listado Saldo Presupuestal ',1,.F.,.T.
       SELE ITEHC
       SET FILTER TO
       SET INDEX TO
       CLOSE DATA
	   ERASE (VIND)
	   ERASE (XIND)
    ENDIF
 ENDIF
RETURN

PROCEDURE PasaF
*-------------
SELE ITEHC
SCAN
   vCanreq=0
   vCa1 = Codsubpr+codact+CodAnal
   vCa2 = Codsubpr+codact+CodAnal
   vAcum = 0
   vMesx=0
   DO WHILE vCa1 = vCa2
      vCanreq = vCanreq+Valpart
      SKIP
      vCa1 = Codsubpr+codact+CodAnal
   ENDD
   SKIP -1
   SCATTER MEMVAR
   DO AGREGAR
   SELE ITEHC
ENDSCAN

RETURN

PROCEDURE Pasai
*-------------
SELE hoja
SCAN
   vCanreq=0
   vCa1 = CodSubpr+Codproy+Codsupry+CodPart
   vCa2 = CodSubpr+Codproy+Codsupry+CodPart
   vAcum = 0
   vMesx = 0
   DO WHILE vCa1 = vCa2
      vCanreq = vCanreq+ImpTot
      SKIP
      vCa1 = CodSubpr+Codproy+Codsupry+CodPart
   ENDD
   SKIP -1
   SCATTER MEMVAR
   DO AGREGAR
   SELE Hoja
ENDSCAN

RETURN

PROCEDURE AGREGAR
*-----------------
SELE repo   
APPEND BLANK
  IF ALLTRIM(VTIPFUN)='I'
	 m.codFte = substr(hoja.CodCal,5,3)
	 m.codprg = substr(hoja.Codcal,8,2)
	 m.codsubpr=substr(hoja.CodCal,10,3)
     m.Codproy =substr(hoja.CodCal,13,3)
  endif   
  m.ValPart   = vCanReq
 *m.AcuMesAnt = vAcum
  m.DelMes    = vMesx
  m.totpres = vAcum
  GATHER MEMVAR
  vPartid = iif(repo.tipfun='I',repo.codpart,repo.codanal)
  scatter memvar
  SELECT REPOP
  seek alltrim(vPartid)
  if !found() 
     if repo.tipfun='F'
        m.codpart = m.codanal
     endif
     m.valpart = 0
     IF F_APPD() 
	  	 gather memvar
	 ENDIF
  endif	 
sele repo
RETURN .T.

PROCEDURE ACUMULA
*----------------
vTemp=Recno()
QQ=ALIAS()
vAcum = 0
vMesx = 0
vKey = alltrim(vCODFTE)+alltrim(vCodPrg)+alltrim(vCodSubPr)+IIF(ITEHC.TIPFUN='I',alltrim(vCODPRY),alltrim(vCODACT))

SELECT REPOP
INDEX ON CODPART TO (XInd) 
SET INDEX TO (XIND)
SELECT ITEPAR
SET ORDER TO IIF(alltrim(Itehc.Tipfun)='F',10,11)
SET FILT TO CODFTE+CodPrg+CodSubPr+IIF(ITEHC.TIPFUN='I',CODPROY,CODACT)=VKEY AND TIPFUN=ALLTRIM(ITEHC.TIPFUN)
GO TOP

SCAN
    SCATTER MEMVAR  
    SELE REPOP
    IF F_APPD()
       GATHER MEMVAR
    ENDIF
    UNLOCK
    SELE ITEPAR
ENDSCAN

 IF alltrim(Itehc.Tipfun)='X'       
    SELE REPOP
	GO TOp
	SCAN
	    scatter memvar
		v1=LEFT(REPOP.CODPART,2) 
		v2=LEFT(REPOP.CODPART,2) 	
	    vTow = 0
	    ACTIVATE SCREEN
	    SCAN while v1=v2 AND RIGHT(REPOP.CODPART,2)#'00'
	    	 vtow = vtow + valpart
    	 	 V2 = LEFT(REPOP.CODPART,2) 	
		ENDSCAN
	 	BROW
		m.Codpart = V1+'.00'
	    m.valpart = vtow
	    IF F_APPD()
      		 GATHER MEMVAR
	    ENDIF
    	SELE REPOP
	
	endscan
 ENDIF   
SET FILT TO
SELE (QQ)
RETURN

PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(VCodSubPr)+ALLTRIM(VCodPry)
SELE MAEPRE
SET ORDE TO 2
seek vkey
vProyecto = maepre.Descri
select (zx)
set order to (qw)
return (vProyecto)

PROCEDURE Salida
*---------------
RELEASE    WINDOW LibDir
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)


PROCEDURE RepDiar
*----------------
SELE Diar
vIndi1=SYS(3)+".IDX"
INDEX ON LEFT(COMPROB,2)+CUENTA+TIPO TO (vInd1)

DO REPORTE WITH 2, "REPOE5", 'Reporte de Ejecuci¢n E-5',1,.F.,.T.

CLOSE DATA
ERASE (vIndi1)
RETURN


PROCEDURE SUMAFE
*---------------
SELECT REPO   && REPOE5
vTotal = 0
IF RIGHT(REPOP.CODPART,2) = '00'
	SEEK LEFT(REPOP.CODPART,2)
    SCAN WHILE LEFT(REPOP.CODPART,2) = IIF(REPOP.TIPFUN='I',LEFT(REPO.CODPART,2),LEFT(REPO.CODANAL,2)) 
	   	 vTotal = vtotal + valpart
	ENDSCAN
ELSE
	*-----   
	SEEK REPOP.CODPART
	IF FOUND()
     	SCAN WHILE REPOP.CODPART = IIF(REPOP.TIPFUN='I',REPO.CODPART,REPO.CODANAL)
	     	vTotal = vtotal + valpart
	    ENDSCAN
	ENDIF
ENDIF   
GO TOP
SELECT REPOP
RETURN VTOTAL	

PROCEDURE buscact
*----------------
PARAMETER vact
**BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(repoP.periodo)+ALLTRIM(repoP.codprg)+ALLTRIM(repoP.codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO 3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE REPOP
RETURN (vactividad)

