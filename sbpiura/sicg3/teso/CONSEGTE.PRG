* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Consegte.PRG  20/09/96                             L:  180   ณ	
* ณ Consulta quien registrข los documentos?                      ณ
* ณ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Parmae   IN 1  ORDER Parmae1  ALIAS Parma
USE USUARIO  IN 2  ORDER USUARIO1 ALIAS USUARIO
USE Cajas    IN 6  ORDER Cajas1   ALIAS Caja

DEFINE WINDOW Wind_2 FROM 15,26 TO 21,76  FLOAT DOUBLE COLOR SCHEME 5
DEFINE WINDOW Wind_8 FROM 03,26 TO 13,76  TITLE " Seguimiento " FLOAT DOUBLE COLOR SCHEME 5
PRIVATE vchoice,vlla,vbus,vnummes,vnumero,vcodctc,Varc,vrev
HIDE POPUP POP_08
DO WHILE .T.
 ACTIVATE WINDOW Wind_8
 vfec     = SPACE(08)
 vbus     = SPACE(02)
 varc     = SPACE(08)
 vlla     = SPACE(03)
 vcodctc  = SPACE(14)
 vnumero  = SPACE(04)
 vperiodo = SPACE(02)
	    VMes     = MONTH(DATE())
        Vnummes  = SPACE(2)
        IF VMES<10
      	   vnummes  ='0'+ALLTRIM(STR(MONTH(DATE())))
        ELSE
           vnummes=STR(VMES,2)   
        ENDIF   
 vperiodo = RIGHT(STR(YEAR(DATE())),2)
 okcancel = 1
 @ 01,01  SAY " Tipo de Documento " GET vBus VALID val_paraD(vbus,'PRGTES',' ',21,20,7) 
 READ
 =vindice() 

 DO CASE
 
    CASE ALLT(vlla)='MDC'  
		 @ 03,01  SAY "             Mes : "  GET   vnummes  PICTURE '!!' VALID Val_Para(vnummes,'FECMES',' ',21,9)
		 @ 04,01  SAY "   Nฃmero de Doc.: "  GET   vnumero  PICTURE '@!' DEFAULT SPACE(4)
		 @ 05,01  SAY " Cuenta Corriente: "  GET   vcodctc  PICTURE '!!!!!!!!!!!!!!';
													        VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,6,16,'descri',27)
													        
    CASE ALLT(vlla)='MD'
		 @ 03,01  SAY "             Mes : "  GET   vnummes PICTURE '!!' VALID Val_Para(vnummes,'FECMES',' ',21,9)
		 @ 04,01  SAY "   Nฃmero de Doc.: "  GET   vnumero PICTURE '@!' DEFAULT SPACE(4)
		 @ 05,01  SAY  SPACE(40) 
		 @ 06,01  SAY  SPACE(40) 
		 
    CASE ALLT(vlla)='PMD'  
		 @ 03,01  SAY "         Periodo : "  GET   vperiodo PICTURE '!!'
		 @ 04,01  SAY "             Mes : "  GET   vnummes  PICTURE '!!' VALID Val_Para(vnummes,'FECMES',' ',21,9)
		 @ 05,01  SAY "   Nฃmero de Doc.: "  GET   vnumero  PICTURE '@!' DEFAULT SPACE(4)
		 @ 06,01  SAY  SPACE(40) 
		 
    CASE ALLT(vlla)='D'  
		 @ 03,01  SAY "   Nฃmero de Doc.: "  GET   vnumero  PICTURE '@!' DEFAULT SPACE(4)
		 @ 04,01  SAY  SPACE(40) 
		 @ 05,01  SAY  SPACE(40) 
		 @ 06,01  SAY  SPACE(40) 
		 
    CASE ALLT(vlla)='MC'  
		 @ 03,01  SAY "             Mes : "  GET   vnummes PICTURE '!!' VALID Val_Para(vnummes,'FECMES',' ',21,9)
		 @ 04,01  SAY " Cuenta Corriente: "  GET   vcodctc  PICTURE '!!!!!!!!!!!!!!';
													        VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,5,16,'descri',27)
		 @ 06,01  SAY  SPACE(40) 
		 
    CASE ALLT(vlla)='PMC'  
		 @ 03,01  SAY "         Periodo : "  GET   vperiodo PICTURE '!!'
		 @ 04,01  SAY "             Mes : "  GET   vnummes PICTURE '!!' VALID Val_Para(vnummes,'FECMES',' ',21,9)
		 @ 05,01  SAY " Cuenta Corriente: "  GET   vcodctc  PICTURE '!!!!!!!!!!!!!!';
													        VALID  Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,6,16,'descri',27)
													        
ENDCASE
 @ 08,10 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
 READ CYCLE VALID exi()
 IF LASTKEY()=27 OR okcancel = 2
    CLOSE DATA
    EXIT
 ENDIF
 DO seg_open WITH vlla,varc,vfec
ENDDO
RELEASE WINDOW Wind_2
RELEASE WINDOW wind_8
CLOSE DATA
SHOW POPUP pop_08
RETURN

PROCEDURE Seg_Open
*-----------------
PARAMETERS mlla,marc,mfec
IF LASTKEY()=27 OR Okcancel = 2
   DEACTIVATE WINDOW Wind_2
   EXIT
ENDIF   
Vrev='rever1'
USE (marc) IN 0 ORDER IIF(ALLT((marc))='REVERSIO',(vRev),(marc)+'1')  ALIAS (marc)
SELE (marc)
DO CASE
   CASE mlla='MDC'  
         SEEK ALLT(vnummes)+vnumero+ALLT(vcodctc)
   CASE mlla='MD'  
         SEEK ALLT(vnummes)+vnumero
   CASE mlla='PMD'  
         SEEK vperiodo+ALLT(vnummes)+vnumero
   CASE mlla='D'  
         SEEK vnumero
   CASE mlla='MC'  
         SEEK ALLT(vnummes)+ALLT(vcodctc)
   CASE mlla='PMC'  
         SEEK vperiodo+ALLT(vnummes)+ALLT(vcodctc)
ENDCASE         
IF FOUND()
	ACTIVATE WINDOW Wind_2
	@ 1,1  SAY " Fecha de Registro : "
	@ 1,24 SAY &mfec
	@ 2,1  SAY " Registrado por    : " 
	@ 2,24 SAY disp_usu(usuario)
    @4,15 GET okcancel FUNCTION '*TH \!\<OK' DEFAULT 1 SIZE 1,11,8
	READ
ELSE
	DO STANDBY WITH " Documento no existe "
ENDIF
DEACTIVATE WINDOW Wind_2
USE IN 0
RETURN



PROCEDURE Salir
*---------------
PARAMETERS vSal
IF LASTKEY()=-9
	vsal = .T.
ENDIF
RETURN .T.


PROCEDURE Exi
*---------------
IF LASTKEY()=27 OR Okcancel=1 OR Okcancel=2
	RETURN .T.
ENDIF
RETURN .F.



FUNCTION vindice
*----------------
vali=ALIAS()
vord=ORDER()
SELE Parma
SEEK 'PRGTES'+ALLT(vbus)
IF FOUND()
   vlla=ALLT(Parma.codigoaux)
   varc=ALLT(Parma.descriaux)
   vfec=ALLT(Parma.descriau2)
ENDIF   
SELE (vali)
SET ORDER TO (vord)
RETURN

FUNCTION Disp_usu
*----------------
PARAMETERS musua 
PRIVATE xuser_id,xdesu
vali=ALIAS()
vord=ORDER()
xuser_id=SPACE(08)
xdesu=SPACE(30)
xuser_ID = CHRTRAN(musua,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?','ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
SELE USUARIO
SEEK ALLT(xuser_id)
IF FOUND()
   xdesu  = ALLT(usuario.nombre)
ENDIF
SELE (Vali)
SET ORDER  TO (vord)
RETURN xdesu     