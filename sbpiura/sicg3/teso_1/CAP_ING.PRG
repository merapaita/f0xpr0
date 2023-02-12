* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Sal_Bco.Prg  02/09/96                               L:   96  ณ	
* ณ Consulta de Captaciขn de Ingresos al mes y acumulados        ณ
* ณ AUTOR  :   Ing. Federico Montero Valdiviezo REGION GRAU      ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Salde6  IN 1 ORDER TAG SaldE61  ALIAS E6
USE Parmae  IN 2 ORDER TAG Parmae1  ALIAS Parma
USE Ingreso IN 3 ORDER TAG Ingreso1 ALIAS Ingre

ON KEY LABEL F10 KEYBOARD CHR(23)
PRIVATE vmes,vperiodo
STORE SPACE(2) TO vmes,vcodctc
Okcancel=1
HIDE POPUP Pop_08
IF LASTKEY()#27 
	DO WHILE .T.
		DEFINE WINDOW Wind_13 FROM 06,24 TO 14,79 ;
		     TITLE ' Captaciขn de Ingresos ' COLOR SCHEME 05
			 ACTIVATE WINDOW Wind_13
  XMes     = MONTH(DATE())
  VMes     = SPACE(2)
  IF XMES<10
	 VMES  ='0'+ALLTRIM(STR(MONTH(DATE())))
  ELSE
     VMES=STR(XMES,2)   
  ENDIF   
		     vperiodo = RIGHT(STR(YEAR(DATE())),2)
		     @  0,1 SAY "         Periodo :" GET vperiodo PICTURE '!!'
		     @  1,1 SAY "             Mes :" GET vmes PICTURE '!!' VALID Val_Para(vmes,'FECMES',' ',20,9)
			READ
	*	xmes=VAL(vmes)
			xano=VAL(vperiodo)
			IF LASTKEY()=27
			   EXIT
			ENDIF
			DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
			SELE E6
			GO TOP
		    SET FILT TO nummes=ALLT(vmes) AND periodo=Vperiodo
		    IF EOF()
		       DO standby WITH 'No hay datos'
		       SET FILT TO
		       RETURN
		    ENDIF   
		    DEACTIVATE WINDOW xwait
			ON KEY LABEL F10 KEYBOARD CHR(23)
			DEFINE WINDOW caping FROM 1,1 TO 22,79  TITLE ' Captaciขn de Ingresos ' 
			BROW NOED WINDOW CAPING COLOR SCHEME 10 FIELDS ;
			codpart ,;
			x1=vdes(E6.codpart) :H='Partida',;
			caping :H="Ing. del Mes",;
			saldo  :H="Total Acumulado"
			DEACTIVATE WINDOW caping
			ACTIVATE WINDOW wind_13
			@ 05,01 GET okcancel FUNCTION '*TH \!\<Continuar;\!\<Imprimir\;\!\<Salir' DEFAULT 1 SIZE 1,11,8 
			READ  CYCLE
			IF okcancel = 3 OR LASTKEY() = 27
			   EXIT
			ENDIF
			IF okcancel = 2
	       		DO REPORTE WITH 2, "Ftoe6", ' Captaciขn de Ingesos ',1,.F.,.T.			
		    ENDIF   
		    SET FILT TO
		    DEACTIVATE WINDOW wind_13
	ENDDO	    
ENDIF
RELEASE  WINDOW wind_13
SHOW popup pop_08
ACTIVATE SCREEN
CLOSE    DATABASES
RETURN

FUNCTION VDES
*-------------
PARAMETERS vpar
PRIVATE vdesc
AL=ALIAS()
STORE SPACE(24) TO vdesc
SELEC INGRE
SEEK vpar
IF FOUND()
   vdesc = left(Ingre.desing,24)
ENDIF   
SELEC (AL)
RETURN vdesc



FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)