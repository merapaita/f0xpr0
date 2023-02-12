* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Ftot5.PRG  30/09/96                                 L: 0354  ณ	
* ณ Emisiขn del T-5                                              ณ
* ณ AUTOR   : CWL - UDEP                                         ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

* Abrir Archivos de trabajo
USE MovBco   IN 1 ORDER TAG MovBco11        ALIAS MovB
USE IteRi    IN 2 ORDER TAG Iteri1          ALIAS iteRi
USE Parmae   IN 3 ORDER TAG Parmae1         ALIAS Parma
USE IngreSO  IN 4 ORDER TAG ingreso1        ALIAS ingre

filaux = SYS(3) + '.DBF'
zInd   = SYS(3) + '.Idx'

SELECT 8
CREATE TABLE (filaux);
  (codpart   C(011),;
   NumMes    C(002),;
   periodo   C(002),;
   importe   N(18,2))
USE

USE (filaux) IN 8  EXCLUSIVE   ALIAS Temp

** Declaraciขn de variables

PUBLIC vtip,vCODPART,xano,wmes,VSALCU,okcancel
STORE 0 TO xano, vMontoD, VSALDO,VSALDEB,VSALDEBA,VDEBACU,vsalant,vsalacu,fmes
STORE SPACE(11)  TO vCODPART
STORE SPACE(2)  TO WMES,ames
STORE ' ' TO vMONTOD, vIMPORTE, vNumMes
STORE 1 TO okcancel

VCODPRG = '02'
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
    DEFINE WINDOW FTOT5  FROM 13,52 TO 18,78  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW FTOT5  FROM 13,52 TO 18,78  COLOR SCHEME C_POPUP
  ENDIF
  ACTIVATE WINDOW FTOT5 
  VMes     = MONTH(DATE())
  xMes     = SPACE(2)
  IF VMES<10
	 XMES  ='0'+ALLTRIM(STR(MONTH(DATE())))
  ELSE
     xMES=STR(VMES,2)   
  ENDIF   
  xAno     = RIGHT(DTOC(DATE()),2)
  STORE 1 TO vliscta,vlisfte,vlistip,vlissec
  STORE SPACE(1) TO vtip
  STORE SPACE(3) TO vcodfte,vsec
  @ 0, 0 SAY '          T-5  '
  @ 2, 1 SAY '   Mes y Aคo : ' GET xMes PICTURE '!!' VALID Val_Para(xMes,'FECMES','C',15) 
  @ 2,20 GET xAno PICTURE '!!'
  READ 
  IF LASTKEY()=27 
	RETURN .T.
  ENDIF
  WMES=PADL(ALLTRIM(XMES),2,'0')
  DEACTIVATE WINDOW FTOT5

  IF LASTKEY()#27 AND okcancel = 1
	DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	ACTIVATE WINDOW Xwait
	@0,0 SAY " Espere un momento...." COLOR W+/N*

  	  	SELECT M.* ;
  		FROM Movbco M ;
   		WHERE tipcla='S'  ;
  		INTO CURSOR movbco1
			
  	  	SELECT M.* ;
  		FROM iteri M ;
  		WHERE tipo='P' ;
  		INTO CURSOR iteri1

    DO Valida

    SELECT temp
	PP = SYS(3) + '.Idx'
	yy = SYS(3) + '.Idx'
	QQ = SYS(3) + '.Dbf'
	deactivate window Xwait
  	IF EOF()
	   DO STANDBY WITH 'No hay datos..'
   	ELSE
  	   INDEX ON codpart+periodo+Nummes TO (PP)
	   *=acumula()
	   DO REPORTE WITH 2, "RepT5A", ' Reporte T5-A ',2,.F.,.T.			
   	ENDIF
   	USE IN 8
    ERASE (filaux)
	DO FTOT5B WITH XMES,XANO
	DO FTOT5C WITH XMES,XANO
  ENDIF
  RETURN


FUNCTION Valida
*--------------
SELECT Movbco1
vInd = SYS(3) + '.Idx'
INDEX ON NUMMES+NUMERO TO (vInd) FOR 	NUMMESC=WMes AND ;
				TIPCLA<>"S"  AND ESTADO<>'99'
GO TOP
vcodpart = Movbco1.parting
vNumMes = Movbco1.NumMes
vPeridod= Movbco1.Periodo
SCAN 
     vcodpart = Movbco1.parting
     STORE 0 TO vMontoD
     vMontoD = Movbco1.Monto
     SELECT Temp
     APPEND BLANK
     REPLACE Temp.codpart  WITH vcodpart;
             Temp.NumMes   WITH vNumMes;
     	     Temp.periodo  WITH Vperiodo;
             Temp.importe WITH vMontoD
ENDSCAN
DO Valida1
RETURN


FUNCTION Valida1
*--------------
SELECT iteri1
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes TO (vInd1) FOR NUMMES=WMes and tipo='P' and estado<>'99'
SET SKIP TO
GO TOP
vcodpart = iteri1.codpart
vNumMes = iteri1.NumMes
vperiodo= iteri1.periodo
vMontoD = iteri1.impparc
SCAN 
	vcodpart = iteri1.codpart
	vNumMes = iteri1.NumMes
	vperiodo= iteri1.periodo
	vMontoD = iteri1.impparc
    SELECT Temp
    APPEND BLANK
    REPLACE Temp.codpart  WITH vcodpart;
            Temp.NumMes   WITH vNumMes;
            Temp.periodo  WITH Vperiodo;
            Temp.importe WITH vMontoD
ENDSCAN
RETURN

PROCEDURE Salida
*---------------
RELEASE  WINDOW FTOT5
ACTIVATE SCREEN
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,VAL(xMes)*9-8,9)) + ' ' + xAno


FUNCTION VDES
*-------------
PARAMETERS vpar
PRIVATE vdesc
AL=ALIAS()
STORE SPACE(50) TO vdesc
SELEC INGRE
SEEK vpar
IF FOUND()
   vdesc = left(Ingre.desing,50)
ENDIF   
SELEC (AL)
RETURN vdesc