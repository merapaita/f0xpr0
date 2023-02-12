* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ ResHoj.PRG        04/04/96                         L: 0086   ³	
* ³ Resumen de Hojas de Trabajo                                  ³
* ³ AUTOR : Julio C‚sar Cruz Ortiz           REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*---------------------------
*  vele = 1  --> PRP
*  vele = 2  --> TRN
*  vele = 3  --> SECTORIAL
*----------------------------
USE Cajas    IN 2 ORDER TAG Cajas1          ALIAS Caja
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
  			VALID  Val_fun('Caja','CodCtc','codctc+descri',vCtaCte,1,1,22,'LEFT(DESCRI,38)') AND Val_Tipo()
  @ 2, 1 SAY '        Mes y A¤o : ' GET xMes PICTURE '!!' WHEN velec # 1 VALID Val_Para(xMes,'FECMES',' ',22,9) 
  @ 2,25 GET xAno PICTURE '!!' WHEN velec # 1 
  @ 4,12 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8 COLOR SCHEME 5 
  READ CYCLE VALID exi()
  DEACTIVATE WINDOW LibBan
  IF LASTKEY()=27 OR okcancel = 2
  	CLOSE DATA
  	RETURN
  ENDIF
    DO ResHSed  WITH vctacte,VAL(xano),ALLTRIM(xmes),velec
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