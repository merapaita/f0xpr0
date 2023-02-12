* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ RECIFTE.Prg                                                  ณ	
* ณ Reporte Hoja de Trabajo de Captaciขn de Ingresos             ณ
* ณ AUTOR  :  Pilar Pacherre Navarro                             ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ
USE IteRi		IN 1 ORDER TAG IteRi1		ALIAS IteR
USE Ingreso		IN 2 ORDER TAG Ingreso1		ALIAS Ingre
USE Cajas       IN 5  ORDER TAG Cajas1       ALIAS Caja
USE Parmae 		IN 3 ORDER TAG Parmae1  ALIAS Parma
USE RecIng 		IN 4 ORDER TAG RecIng1  ALIAS RecI
*************************
filaux = SYS(3) + '.DBF'
zInd   = SYS(3) + '.Idx'
SELECT 6
CREATE TABLE (filaux);
  (CodCtc    C(014),;
   PERIODO   C(002),;
   CODFTE    C(002),;
   fecri     D(008),;
   CODPART   C(011),;
   NumMes    C(002),;
   NUMRI     C(004),;
   TIPRI     C(003),;
   DESCRI	 C(050),;
   IMPPARC   N(18,2))
USE

USE (filaux) IN 6  EXCLUSIVE   ALIAS Temp
*********************
vIdx=sys(3)+'.idx'
*ELE IteR
*ET RELATION TO PERIODO+NUMMES+NUMRI INTO RECI

DEFINE WINDOW RECFTE FROM 06,20 TO 12,70 TITLE ' Captaciขn de Ingresos ' DOUBLE SHADOW COLOR SCHEME 5 
Vmens08 = 'No hay registros para procesar'
  ACTIVATE WINDOW RECFTE
  STORE DATE()   TO vfec1,vfec2
  @ 01,10 SAY " Fec.Inic.: " GET vfec1  PICTURE '@D'
  @ 02,10 say " Fec.Fin. : " GET vfec2  PICTURE '@D'
  @ 03,10 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
  READ CYCLE VALID exi()
  DEACTIVATE WINDOW RECFTE
  IF LASTKEY()=27 OR okcancel = 2
    CLOSE DATA
  	RETURN
  ENDIF
  *
  *
  DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
  ACTIVATE WINDOW Xwait
  @0,0 SAY " Espere un momento ......... " COLOR W+/BR*
* @1,0 SAY FILAUX
  SELE RECI
    INDEX ON PERIODO+NUMMES+NUMRI TO (vIdx) for BETWEEN(FecRI,vFec1,vFec2) AND ESTADO<>'99'
	GO TOP
	vperiodo=periodo
	Vnummes=nummes  
	vnumri=numri
	vcodctc=codctc
	vcodte=codfte
	VFECRI=FECRI
	VTIPRI=TIPRI
	scan
		vperiodo=periodo
		Vnummes=nummes  
		vnumri=numri
		vcodctc=codctc
		vcodfte=codfte
		vfecri=fecri
		VTIPRI=TIPRI
		sele iter
		seek vperiodo+vnummes+vnumri
	    do while periodo=vperiodo and nummes=vnummes and numri=vnumri and not eof()
	    	vcodpart=codpart
	    	vimpparc=impparc
	    	sele ingre
	    	seek vcodpart
	    	IF FOUND()
			   desparw = desing
			ELSE
				DESPARW=SPACE(50)
			ENDIF				
	        SELECT 6
    		appen blank
        	REPLACE temp.CodCtc  WITH vCodCtc;
                temp.Numri   WITH vNumri;
                temp.NumMes  WITH vNumMes;
                temp.periodo WITH vperiodo;
                temp.codfte  WITH vcodfte;
                temp.fecri   WITH vfecri;
                TEMP.TIPRI   WITH VTIPRI;
	    		temp.codpart with vcodpart;
	    		temp.descri with desparw;
	    		temp.impparc with vimpparc
	    	sele iter	
	    	skip
	    enddo
	    SELE RECI
	endscan
	sele temp
	index on codfte+codctc+codpart to zind FOR CODPART<>SPACE(11)
	*
	go top
    DEACTIVATE WINDOW xwait
	DO REPORTE WITH 2, "INGFTE", ' Captaciขn de Ingresos ',1,.F.,.T.			
    DEACTIVATE WINDOW xwait
  *
RELEASE  WINDOW RECFTE
ACTIVATE SCREEN
CLOSE    DATABASES
erase(vIdx)
RETURN
&&&&&&&&&&&&&&&&&&&
***********
FUNCTION VDES1
*-------------
PARAMETERS vtipo
PRIVATE vdesc
AL=ALIAS()
STORE SPACE(24) TO vdesc
SELE PARMA
SEEK 'TIPRI '+vtipo
IF FOUND()
   vdesc = left(Parma.descri,50)
ENDIF   
SELEC (AL)
RETURN vdesc


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.
