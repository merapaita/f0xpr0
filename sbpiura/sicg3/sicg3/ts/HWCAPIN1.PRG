* 旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴커
* ? HwCapIng.Prg  23/09/98                              L:   98  ?	
* ? Reporte Hoja de Trabajo de Captaci줻 de Ingresos             ?
* ? AUTOR  :  Anal. de Sistemas     Miriam Pilar Tuㄲn Reyes     ?
* 突袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴袴槁
PUBLIC vfecini,vfecfin
USE IteRi		IN 1 ORDER TAG IteRi3		ALIAS IteR
USE Ingreso		IN 2 ORDER TAG Ingreso1		ALIAS Ingre
USE Parmae 		IN 3 ORDER TAG Parmae1  ALIAS Parma
USE RecIng 		IN 4 ORDER TAG RecIng1  ALIAS RecI
SELE IteR
SET RELATION TO PERIODO+NUMMES+NUMRI INTO RECI
vIdx=sys(3)+'.idx'
ON KEY LABEL F10 KEYBOARD CHR(23)
PRIVATE vmes,vperiodo
STORE {''} TO vfecini,vfecfin
Okcancel=1
HIDE POPUP Pop_08
IF LASTKEY()#27 
	DO WHILE .T.
		DEFINE WINDOW Wind_13 FROM 06,24 TO 14,79 ;
		     TITLE ' Captaci줻 de Ingresos ' COLOR SCHEME 05
			 ACTIVATE WINDOW Wind_13
  XMes     = MONTH(DATE())
  VMes     = SPACE(2)
  IF XMES<10
	 VMES  ='0'+ALLTRIM(STR(MONTH(DATE())))
  ELSE
     VMES=STR(XMES,2)   
  ENDIF   
		     vperiodo = RIGHT(STR(YEAR(DATE())),2)
		     @  0,1 SAY "      Fecha desde:" GET vfecini 
		     @  1,1 SAY "      Fecha hasta:" GET vfecfin 
			READ
*		xano=VAL(vperiodo)
			IF LASTKEY()=27
			   EXIT
			ENDIF
			DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
			SELE IteR
			GO TOP
*		    INDEX ON RECI.TIPRI+CODPART TO (vIdx) for nummes=ALLT(vmes) .AND. periodo=Vperiodo .AND. TIPO='P'.AND. ESTADO<>'99' 
		    INDEX ON RECI.CODFTE+RECI.CODCTC+CODPART TO (vidx) for betw(fecdep,vfecini,vfecfin) .AND. TIPO='P'.AND. ESTADO<>'99'
		    IF EOF()
		       DO standby WITH 'No hay datos'
		       SET FILT TO
		       RETURN
		    ENDIF   
		    DEACTIVATE WINDOW xwait
			ACTIVATE WINDOW wind_13
			@ 05,01 GET okcancel FUNCTION '*TH \!\<Continuar;\!\<Imprimir\;\!\<Salir' DEFAULT 1 SIZE 1,11,8 
			READ  CYCLE
			IF okcancel = 3 OR LASTKEY() = 27
			   EXIT
			ENDIF
			IF okcancel = 2
	       		DO REPORTE WITH 2, "CAPING1", ' Captaci줻 de Ingresos ',1,.F.,.T.			
		    ENDIF   
		    SET FILT TO
		    DEACTIVATE WINDOW wind_13
	ENDDO	    
ENDIF
RELEASE  WINDOW wind_13
SHOW popup pop_08
ACTIVATE SCREEN
CLOSE    DATABASES
erase(vIdx)
RETURN

FUNCTION VDES1
*-------------
PARAMETERS vtipo
PRIVATE vdesc
AL=ALIAS()
STORE SPACE(24) TO vdesc
*SELEC INGRE
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



