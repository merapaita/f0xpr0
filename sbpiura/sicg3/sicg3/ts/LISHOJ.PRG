* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ LisHoj.PRG        10/04/96                         L: 0090   ณ	
* ณ Listado de Hojas de Trabajo                                  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

PARAMETERS sede
USE Cajas    IN 8 ORDER TAG Cajas1          ALIAS Caja
USE Parmae   IN 1 ORDER TAG Parmae1         ALIAS Parma
SET EXACT ON
PUBLIC vtipo,velec
  IF Escolor
     DEFINE WINDOW LibBan FROM 03,09 TO 16,70  DOUBLE SHADOW COLOR SCHEME 5 
  ELSE
     DEFINE WINDOW LibBan FROM 03,09 TO 16,70  DOUBLE COLOR SCHEME c_popup
  ENDIF
  ACTIVATE WINDOW LibBan
  vctacte  = SPACE(14)
  VMes     = MONTH(DATE())
  xMes     = SPACE(2)
  IF VMES<10
	 XMES  ='0'+ALLTRIM(STR(MONTH(DATE())))
  ELSE
     xMES=STR(VMES,2)   
  ENDIF   
  xAno     = RIGHT(DTOC(DATE()),2)
  VCODPRG  = '02'
  STORE 1 TO vliscta,vlisfte,vlistip,vlissec
  STORE SPACE(1) TO vtip
  STORE SPACE(3) TO vcodfte,vsec,vref
  STORE DATE()   TO vfec1,vfec2
  SELECT Caja
  @ 0, 1 SAY ' Cuenta Corriente : ' GET vCtaCte PICTURE '!!!!!!!!!!!!!!' ; 
  			VALID  Val_fun('Caja','CodCtc','codctc+descri',vCtaCte,1,1,22,'LEFT(DESCRI,38)')  &&AND  Val_Tipo()
  @ 2, 1 SAY '        Mes y Aคo : ' GET xMes PICTURE '!!'  VALID Val_Para(xMes,'FECMES',' ',22,9)   &&WHEN velec # 1
  @ 2,25 GET xAno PICTURE '!!'   &&WHEN velec # 1 
  @10,12 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
  
  READ CYCLE VALID exi()
  DEACTIVATE WINDOW LibBan
  IF LASTKEY()=27 OR okcancel = 2
    CLOSE DATA
  	RETURN
  ENDIF

 DO lishprP WITH vctacte,xmes,xano,sede 
		 
SET EXACT OFF  
RETURN  

FUNCTION Val_tipo
*------------------
SELECT Caja
SEEK vctacte
IF FOUND()
	IF Sector = 'N'
	   vtipo = .F.
	   IF Codfte = 'TRN'
		  velec = 2 
	   ELSE
		  velec = 1
	   ENDIF
	ELSE
	   vtipo = .T.
	   velec = 3
	ENDIF
	SHOW GETS
	RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR okcancel=1 or okcancel=2
	RETURN .T.
ENDIF
RETURN .F.
