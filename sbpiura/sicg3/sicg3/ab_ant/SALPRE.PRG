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
PUBLIC vFecini,vFecFin
m.periodo = SPACE(2)
m.calend  = SPACE(2)
m.codcad  = SPACE(4)
m.codfte  = SPACE(2)
=val_CODCAD(ALLT(m.codcad),m.periodo,'C')
 
DEFINE WINDOW Lis FROM 6,6 TO 17,74 DOUBLE ;
TITLE ' °° SALDOS PARTIDAS °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis
@  1,2 say '          Periodo : '
@  2,2 say '       Calendario : '
@  3,2 say '      Cadena Fun. : '
@  4,2 say '   Fte. Financia. : '
@  5,2 say '          Funci¢n : ' 
@  6,2 say '         Programa : '
@  7,2 say '      SubPrograma : '
@  8,2 say '   Activ./Proyec. : '

 @  1,22  GET m.Periodo   PICTURE '!!'     VALID !EMPTY(m.Periodo)
 @  2,22  GET m.Calend    PICTURE '!!'     VALID VAL_PARA(m.Calend,'FECMES',' ',22,30) 

 @  3,22 GET m.CodCad  PICTURE '!!!!' 	VALID VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
 @  4,22 GET m.Codfte  PICTURE '!!'  	VALID VAL_PARA(m.Codfte,'CODFTE',' ',22,30)
 READ VALID Val_Read()

 IF LASTKEY()=27 
 	GO REC
    DO vista
	RETURN
 ENDIF
 @  5,22  SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
 @  6,22  SAY VAL_PARA1(maepre.codprg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  7,22  SAY VAL_PARA1(maepre.codspr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @  8,22  SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
 READ VALID VAL_READ()

 DEACTIVATE WINDOW lis

 IF LASTKEY()=27
    RETURN
 ELSE
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

