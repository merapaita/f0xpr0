* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ RebCon.PRG  15/02/96                               L:  502   ณ	
* ณ Rebaja manual de Conciliaciขn                                ณ
* ณ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU         ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Cajas    IN 1  ORDER Cajas1   ALIAS Caja 
USE Conban   IN 2  ORDER Conban1  ALIAS ConBan
USE Hchqpen  IN 3  ORDER hChqpen1  ALIAS hChqpen
USE Chqpen  IN 4  ORDER Chqpen1  ALIAS Chqpen
DEFINE WINDOW Wind_8 FROM 04,26 TO 17,76  TITLE " Rebaja de Conciliaciขn " FLOAT DOUBLE COLOR SCHEME 05
DEFINE WINDOW VerSal FROM 04,16 TO 20,79 ;
COLOR SCHEME 10 TITLE "ฎF2ฏ Rebaja Total ฎF5ฏ Rebaja Parcial ฎF10ฏ para salir"
DEFINE WINDOW Vermto FROM 20,40 TO 23,79 COLOR SCHEME 05
STORE 0 TO vmtochq,vreb
STORE SPACE(14) TO vcuenta
PRIVATE vchoice
OK=VE_PASSW('MENCB')
IF OK AND LASTKEY()#27 
	 ACTIVATE WINDOW Wind_8
	 vbus     = 1
	 vest     = 1
	 okcancel = 1
 	
	 @ 01,01  SAY " Tipo de Documento: " GET vBus PICTURE "@^ Cargo Pend. Afectac.;Depขsito no contab.;Cargo pendiente X el Banco;Depขsito pendiente X el Banco;Pagos de menos;Pagos de m s;Cargo Indebido"
	 @ 05,01  SAY " Cuenta Corriente : " 
	 @ 05,22  GET vCuenta PICTURE '!!!!!!!!!!!!!!';
	          VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCuenta,1,5,22,'descri',27)
	 @ 10,10  GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
	 READ CYCLE VALID exi()
	 DEACTIVATE WINDOW Wind_8
	 vChoice = vBus
	 IF LASTKEY()=27 OR okcancel = 2
	    CLOSE DATA
	    RETURN 
	 ENDIF
	 SELE Caja
	 IF SEEK(ALLT(Vcuenta))
	    Vmes=ALLT(Caja.Mescon)
	    Vind1=SYS(3)+'.IDX'
		DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
	    SELE ConBan
	    INDEX ON mescon TO (vind1) FOR mescon=ALLT(vmes) AND codctc=ALLTRIM(vcuenta) AND ALLT(conban)=IIF(vbus=1,'+',IIF(vbus=2,'|',IIF(vbus=3,'',IIF(vbus=4,'',IIF(vbus=5,'<',IIF(vbus=6,'>','I'))))))
   	    GO TOP
	    IF !EOF()
	       DO rebaja_cb
	    ELSE
	       DO standby WITH 'No existen registros'
	    ENDIF
	    SET INDEX TO
	 ENDIF  
ENDIF	 
RELEASE WINDOW Wind_8
RELEASE WINDOW Versal
CLOSE DATA
RETURN


PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.


PROCEDURE rebaja_cb
*-------------------
  ON KEY LABEL F2  DO rebT
  ON KEY LABEL F8  DO Busca1
  ON KEY LABEL F5  DO rebP
  ON KEY LABEL F10 KEYBOARD CHR(23)
  DEACTIVATE WINDOW xwait  
  ACTIVATE WINDOW versal
  BROWSE NOEDIT NOAPPEND FIELDS ;
       rebman :R,;
       ValChq :H='Importe' :P='999,9999,999.99' :R,;
       Fecha  :R;
       WINDOW VerSal
  ON KEY LABEL F2  
  ON KEY LABEL F5  
  ON KEY LABEL F10 
  SELE ConBan
RETURN

PROCEDURE rebT
*---------------
vfecha=Conban.fecha
vmtochq=Conban.Valchq
vtipo  = Conban.Flag
SELECT Conban
IF yesno("จEsta Seguro?")
	IF RLOCK()
	   REPLACE mesreb  WITH vmes,;
    	       Conban  WITH '-',;
    	       Rebman  WITH '๛'
	   UNLOCK
	ENDIF
	IF ALLT(vtipo)='A'
	    Vind3=SYS(3)+'.IDX'
		SELE Hchqpen
		INDEX ON STR(valchq)+DTOC(fecha) TO (vind3)
		IF SEEK (STR(vmtochq)+DTOC(vfecha))
		   IF RLOCK()
			   REPLACE mesreb  WITH vmes,;
			   		       Conban  WITH '-'
		   ENDIF   
		   UNLOCK
		ENDIF
	    Vind2=SYS(3)+'.IDX'
		SELE chqpen
		INDEX ON STR(valchq)+DTOC(fecha) TO (vind2)
		IF SEEK (STR(vmtochq)+DTOC(vfecha))
		   IF RLOCK()
			   REPLACE mesreb  WITH vmes,;
			   		       Conban  WITH '-'
		   ENDIF   
		   UNLOCK
		ENDIF

		SET INDEX TO
	ENDIF	
ENDIF	
SELE ConBan
RETURN

PROCEDURE rebP
*---------------
ACTIVATE  WINDOW vermto
@1,1 SAY "Monto a Rebajar: " GET vreb PICTURE "999,999,999.99" VALID(vreb<ConBan.Valchq)
READ
vfecha=Conban.fecha
vtipo  = Conban.Flag
SELECT Conban
IF yesno("จEsta Seguro?")
	IF RLOCK()
	   REPLACE valchq  WITH valchq-vreb,;
       Rebman  WITH '๛'
	   UNLOCK
	ENDIF
	IF ALLT(vtipo)='A'
	    Vind3=SYS(3)+'.IDX'
	    SELE Hchqpen
		INDEX ON STR(valchq)+DTOC(fecha) TO (vind3)
		IF SEEK (STR(vmtochq)+DTOC(vfecha))
		   IF RLOCK()
		      REPLACE valchq  WITH valchq-vreb
	    	  UNLOCK
		   ENDIF   
		ENDIF
		SET INDEX TO
	ENDIF	
ENDIF	
DEACTIVATE WINDOW vermto
SELE ConBan
RETURN



