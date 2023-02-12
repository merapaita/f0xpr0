* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Act_CpHc.Prg  19/08/96                              L:   98  ณ	
* ณ Actualiza Nuevas Hojas de Control en Comprob. de Pago        ณ
* ณ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Compag IN 1 ORDER Compag1 ALIAS Compag
USE Hojcon IN 2 ORDER Hojcon1 ALIAS Hoja
USE Reten  IN 3 ORDER Reten1  ALIAS Reten
USE Cajas  IN 4 ORDER Cajas1  ALIAS Caja 

SELE compag
DEFINE WINDOW actua1 FROM 03,24 TO 11,79  TITLE "Comp. de Pago"         DOUBLE SHADOW COLOR SCHEME 10
DEFINE WINDOW actua2 FROM 12,24 TO 15,79  TITLE "Nueva Hoja de Control" DOUBLE SHADOW COLOR SCHEME 10

PUBLIC vnummes,vnumcp,vcodctc,vnummeshc,vnumhc,anummeshc,anumhc
HIDE POPUP pop_01
DO WHILE LASTKEY()#27
	ACTIVATE WINDOW actua1
	STORE SPACE(2)  TO vnummes,vnummeshc,anummeshc
	STORE SPACE(4)  TO vnumcp,vnumhc,anumhc
	STORE SPACE(14) TO vcodctc
	IF MONTH(DATE())<10
	   vnummes  ='0'+ALLTRIM(STR(MONTH(DATE())))
   	   vnummeshc='0'+ALLTRIM(STR(MONTH(DATE())))
   	ELSE
	   vnummes  =STR(MONTH(DATE()),2)
   	   vnummeshc=STR(MONTH(DATE()),2)
   	ENDIF
  @ 0,01 SAY '          Ingrese Mes : ' GET vnummes PICTURE '!!' 
  @ 1,01 SAY 'Ingrese Nฃmero ComPag : ' GET vnumcp  PICTURE '@!' 
  @ 2,01 SAY '       Ingrese CtaCte : ' GET vcodctc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+LEFT(Descri,10)",vcodctc,1,2,26) 
  READ 
  IF LASTKEY()#27 
	SELE compag
	SEEK ALLT(vnummes)+vnumcp+ALLT(vcodctc)
	IF FOUND()
	   anummeshc=compag.nummeshc
	   anumhc   =compag.numhc
	   @ 4,01 SAY 'Actual Hoja de Control:  ' +anummeshc+'.'+anumhc
	   @ 5,01 SAY '               Importe: ' +STR(compag.import,15,2)
	   @ 6,01 SAY IIF(compag.reten#0,'               Liquido: ' +STR(compag.import-compag.reten,15,2),' ')
	   ACTIVATE WINDOW actua2	
	   @ 0,01 SAY 'Ingrese mes Nueva H/C: ' GET vnummeshc PICTURE '!!' 
	   @ 1,01 SAY ' Ingrese Nuevo Nง H/C: ' GET vnumhc    PICTURE '@!' 
	   READ  VALID val_read()

	   IF RLOCK()
	       REPLACE nummeshc WITH vnummeshc,;
                   numhc    WITH vnumhc,;
                   numref   WITH vnumhc+'.'+vnummeshc
       ENDIF

       IF Compag.Reten#0
	      SELE Reten
		   IF RLOCK()
		       REPLACE nummeshc WITH vnummeshc,;
                       numhc    WITH vnumhc
           ENDIF    
       ENDIF

       SELE Hoja
       SEEK ALLT(anummeshc)+anumhc
       IF FOUND()
          areten  = hoja.reten
          apartret= hoja.partret
          IF RLOCK()
	         REPLACE estado   WITH '00',;
                     codctc   WITH SPACE(14),;
                     nummescp WITH SPACE(2),;
                     numcp    WITH SPACE(4)
          ENDIF
       ENDIF

       SEEK ALLT(Vnummeshc)+Vnumhc
       IF FOUND()
          IF RLOCK()
	         REPLACE estado   WITH '50',;
                     codctc   WITH Vcodctc,;
                     nummescp WITH vnummes,;
                     numcp    WITH vnumcp,;
                     reten    WITH areten,;
                     partret  WITH apartret
          ENDIF
       ENDIF
       DEACTIVATE WINDOW actua2
    ELSE
       DO standby WITH 'Documento no Encontrado'
	ENDIF   
  ELSE
    EXIT	
  ENDIF
ENDDO
RELEASE  WINDOW Actua1
RELEASE  WINDOW Actua2
ACTIVATE SCREEN
CLOSE    DATABASES
RETURN