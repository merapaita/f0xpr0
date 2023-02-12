** ----------------------------------------------
** FTOE6.PRG
** Reporte E6
** UDEP - 1996. CWL
** ----------------------------------------------

* Abrir Archivos de trabajo

USE MovBco   IN 1 ORDER TAG MovBco11        ALIAS MovB
USE IteRi    IN 2 ORDER TAG Iteri1          ALIAS iteRi
USE Parmae   IN 3 ORDER TAG Parmae1         ALIAS Parma
USE IngreSO  IN 4 ORDER TAG ingreso1        ALIAS ingre
USE Salde6   IN 5 ORDER TAG salde61 		ALIAS salde6

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

** Declaraci¢n de variables

PUBLIC vtip,vCODPART,xano,wmes,VSALCU
PUBLIC elabora,aprova,telef,nota,observ1,observ2,observ3
STORE SPACE(35)  TO elabora
STORE SPACE(35)  TO aprova
STORE SPACE(08)  TO telef
STORE SPACE(45)  TO nota
STORE SPACE(30)  TO observ1
STORE SPACE(30)  TO observ2
STORE SPACE(30)  TO observ3
STORE 0 TO xano, vMontoD, VSALDO,VSALDEB,VSALDEBA,VDEBACU,vsalant,vsalacu,fmes
STORE SPACE(11)  TO vCODPART
STORE SPACE(2)  TO WMES,ames
STORE ' ' TO vMONTOD, vIMPORTE, vNumMes
ELABORA='LETICIA GARCIA BACA'+SPACE(16)
APROVA= 'PILAR RUESTA ORDINOLA'+SPACE(14)
TELEF='324613'+SPACE(5)

VCODPRG = '02'
DO Inicia
DO salida
RETURN


PROCEDURE Inicia
*---------------
  IF Escolor
     DEFINE WINDOW FtoE6 FROM 13,19 TO 21,79 DOUBLE SHADOW COLOR SCHEME 10
  ELSE
     DEFINE WINDOW FtoE6 FROM 13,19 TO 21,79  DOUBLE COLOR SCHEME c_popup
  ENDIF
  ACTIVATE WINDOW FtoE6
  xMes     = SPACE(2)
  xMes     ='0'+ALLTRIM(STR(MONTH(DATE())))
  xAno     = RIGHT(DTOC(DATE()),2)
  STORE 1 TO vliscta,vlisfte,vlistip,vlissec
  STORE SPACE(1) TO vtip
  STORE SPACE(3) TO vcodfte,vsec
  @ 2, 1 SAY '   Mes y A¤o : ' GET xMes PICTURE '!!' VALID Val_Para(xMes,'FECMES',' ',17,9) 
  @ 2,34 GET xAno PICTURE '!!'
  @ 5,12 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
  READ CYCLE VALID exi()

  WMES=PADL(ALLTRIM(XMES),2,'0')
  DEACTIVATE WINDOW FtoE6

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
  	INDEX ON codpart+periodo+nummes TO (zInd) 
    go top
    VCODPART=codpart
    SCAN
	    VCODPART=codpart
*   	seek vcodpart+XANO+ALLT(XMES)
	    store 0 to vsaldeb
        SCAN WHILE temp.codpart=vcodpart AND temp.periodo=xano AND temp.nummes=ALLT(xmes) 
                vrec=RECNO()
 				vsaldeb=vsaldeb+importe     
	    ENDSCAN
		SELE SaldE6
	  	SEEK Vcodpart+xano+ALLT(Xmes)
		IF FOUND()
	  		    IF RLOCK()
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codpart WITH vcodpart
		  	      REPLACE saldo WITH vsaldeb, caping WITH vsaldeb
	  		      UNLOCK
	  	    	ENDIF
		ELSE
				If f_appd()
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codpart WITH vcodpart
		  	      REPLACE saldo WITH vsaldeb, caping WITH vsaldeb
	  		    ENDIF
		ENDIF
        IF XMES#'01'
	  	  fmes=VAL(ALLTRIM(xmes))-1
	  	  FMES=PADL(ALLT(STR(FMES)),2,'0')
		  SKIP -1
	  	  IF ALLTRIM(SaldE6.codpart)=ALLTRIM(vcodpart)
	   			vsaldeba=SaldE6.saldo
   				vdebacu=vsaldeb+vsaldeba
		   ELSE
   				vdebacu=vsaldeb
		   ENDIF
		   SKIP 
    		IF RLOCK()
	  	    	  REPLACE periodo WITH xano, nummes WITH wmes, codpart WITH vcodpart
	  		      REPLACE saldo WITH vdebacu
		  	   UNLOCK
	  		ENDIF
 	  	endif
	   	SELE TEMP
	   	GO vrec
      ENDSCAN
	    GO TOP
	    PP = SYS(3) + '.Idx'
	    yy = SYS(3) + '.Idx'
	    QQ = SYS(3) + '.Dbf'
	    deactivate window Xwait
  	IF EOF()
		SELE SaldE6
  		FMES=VAL(XMES)-1
	  	AMES=IIF(FMES=0,'12',STR(FMES,2))
  		SEEK ALLTRIM(vcodpart+xano+wmes)
  		SKIP -1
	  	IF ALLTRIM(SaldE6.codpart)=ALLTRIM(Vcodpart)
	       COPY NEXT 1 TO (QQ) 
           USE (QQ) IN 0 ALIAS QQ
		   SELE QQ
		   SET RELATION TO CODPART+PERIODO+NUMMES INTO SALDE6 ADDITIVE
		   DO REPORTE WITH 2, "REPE6", ' Reporte Libro Banco ',2,.F.,.T.				
           USE
        ENDIF
   	ELSE
  	INDEX ON codpart+periodo+Nummes TO (PP)
	SET RELATION TO CODPART+PERIODO+NUMMES INTO SALDE6 ADDITIVE
	=acumula()
	DO REPORTE WITH 2, "Repe6", ' Reporte E6 ',2,.F.,.T.			
   	ENDIF
    CLOSE DATA
    ERASE (filaux)
  ENDIF
  RETURN


FUNCTION Valida
*--------------
SELECT Movbco1
vInd = SYS(3) + '.Idx'
INDEX ON NUMMES+NUMERO TO (vInd) FOR 	NUMMESC=WMes AND ;
				TIPCLA="S"  AND ESTADO<>'99'
GO TOP
vcodpart = Movbco1.parting
vNumMes = Movbco1.NumMes
vPeriodo= Movbco1.Periodo
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
RELEASE  WINDOW FtoE6
ACTIVATE SCREEN
CLOSE    DATABASES
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,VAL(xMes)*9-8,9)) + ' ' + xAno

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.

FUNCTION Val_IngE
*---------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngRE
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = INGRE.CodIng 
 mDescr = SUBSTR( INGRE.DesIng, 1, mLong )
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
      @ ROW(),mCol+24  SAY mValor
      @ ROW(),mCol+37 SAY mDescr
      vpar     =mvalor
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    CASE mVaRiable='T'
      &mVaRiable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
    OTHERWISE   
      vpar = mvalor
      RETURN .T.
  ENDCASE


FUNCTION ACUMULA
*-------------
AL=ALIAS()
VSALACU=0
SELEC SALDE6
GO TOP
SCAN 
     IF SALDE6.PERIODO=XANO AND SALDE6.NUMMES=ALLT(XMES)
		 VSALACU=VSALACU+SALDE6.SALDO
	 ENDIF	 
ENDSCAN	 
SELEC (AL)
RETURN 

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