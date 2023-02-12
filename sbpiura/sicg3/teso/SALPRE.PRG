* ---------------------------------------------------
** SALPRE.PRG
** Reporte Ejecucion ESTADOS
** UDEP
** Autor : Ing. Luis Castillo Dur n
** ---------------------------------------------------
PARAMETER VOP
* Abrir Archivos de trabajo
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE maepar   IN 2   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 3   ORDER TAG itepar1  ALIAS itepar
USE clase    IN 4   ORDER TAG clase1   ALIAS clase
USE maepre   IN 5   ORDER TAG maepre1  ALIAS maepre
USE repopre  IN 6                      ALIAS REPO 
USE Calen    IN 7 ORDER TAG Calen1     ALIAS Calen
USE HojCon   IN 8 ORDER TAG Hojcon1    ALIAS Hoja
USE IteHc    IN 9 ORDER TAG Itehc1     ALIAS Itehc
USE Itetra   IN 10 ORDER TAG Itetra1   ALIAS Itetra
USE Itecre   IN 11 ORDER TAG Itecre1   ALIAS Itecre
USE HOJMOD   IN 12 ORDER TAG hojmod1   ALIAS hojmod

*----
PUBLIC TOT,VACUM,TOT1,TOTCOM
TOT = 0
TOT1 = 0
DO Inicia
DO salida
CLOSE DATA
RETURN


PROCEDURE Inicia
*--------------

*PUBLIC vPeriodo,VTipFun,VCodPrg,vCodFte,vCodSub,vCodAct,vProyec,vSubpry,vCalend 

vPeriodo  = '  '
vTipFun   = ' '
vCodPrg   = '  '
vCodFte   = '   '
vCodSub   = '   '
vCodAct   = '  '
vProyec   = '   '
vSubpry   = '  '
vCalend   = '  '

 DEFINE WINDOW Lis FROM 6,10 TO 17,70 DOUBLE ;
 TITLE ' °° SALDOS PARTIDAS °° ' FLOAT COLOR SCHEME 5

 ACTIVATE WINDOW lis
 @  1,2 say '        Periodo : '
 @  2,2 say '     Calendario : '
 @  3,2 say '         Fuente : '
 @  4,2 say '           Tipo : '
*@  5,2 say '       Programa : ' 
*@  6,2 say '    SubPrograma : '
*@  7,2 say '       Proyecto : ' 
*@  8,2 say '      Actividad : '


 @  1,21  GET vPeriodo   PICTURE '!!'  VALID !EMPTY(vPeriodo)
 @  2,21  GET vCalend    PICTURE '!!'  valid VAL_PARA(vCalend  ,'FECMES',' ',21,30)
 @  3,21  GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte  ,'CODFTE',' ',21,30) AND !EMPTY(vCodFte)
 @  4,21  GET vtipfun    PICTURE '!'   VALID VAL_PARA(vTiPFun  ,'TIPFUN',' ',21,30)
*@  5,21  GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg  ,'CODPRG',' ',21,30) AND !EMPTY(vCodPrg) 
*@  6,21  GET vCodsub    PICTURE '!!!' &&VALID VAL_CALE(vCodsub,'CODSUB'+VCodPrg,' ',21,30)
*@  7,21  GET vProyec    PICTURE '!!!' WHEN ALLTRIM(VTIPFUN)='I' 
*@  8,21  GET vCodAct    PICTURE '!!'  WHEN ALLTRIM(VTIPFUN)='F'


 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF LASTKEY()=27
    RETURN
 ELSE
    m.tipfun=vtipfun
    m.periodo = vperiodo
    m.nummes = alltrim(vCalend)
	m.codfte = alltrim(vcodfte)
    ZDX = SYS(3)+'.IDX'
    cd = analpre(0,3,.t.,VOP)
    IF CD
		IF YESNO('¨ Imprime consulta ?')
	    select presu1    
		    IF EOF()
		       DEACTIVATE WINDOW STANDBY
		       DO STANDBY WITH 'No se tiene registros a Procesar'
		    ELSE
		       GO TOP
		       IF ALLTRIM(VTIPFUN)='F'
		           INDEX ON CODPART TO (ZDX)
		           DO REPORTE WITH 2,"salpreT",' Listado Saldo Presupuestal ',1,.F.,.T.
			   ELSE
			       DO REPORTE WITH 2,"salpreS",' Listado Saldo Presupuestal ',1,.F.,.T.
			   ENDIF			       
		    ENDIF
		ENDIF    
    ENDIF
 ENDIF
*CLOSE DATA 
RETURN

PROCEDURE BUSCPRG
*----------------
Private vKey
zx = ALIAS()
qw = order()
vKey=ALLTRIM(VPeriodo)+ALLTRIM(Vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec)
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
*CLOSE DATABASE
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



PROCEDURE buscact
*----------------
PARAMETER vact
private as
as=alias()
**BUSCAR ACTIVIDAD
PRIVATE vkey
vkey=ALLTRIM(periodo)+ALLTRIM(codprg)+ALLTRIM(codsubpr)+ALLTRIM(vact)
SELE maepre
SET ORDE TO maepre3
SEEK vkey
vactividad = IIF(FOUND(),maepre.descri,'?')
SELE (as)
RETURN (vactividad)
*--------------------------------------------------------------------------------------
