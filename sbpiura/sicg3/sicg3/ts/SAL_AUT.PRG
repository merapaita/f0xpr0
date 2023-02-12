* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Sal_Aut.Prg  17/08/96                               L:  152  ณ	
* ณ Consulta de Saldos de Autorizaciones de Giro                 ณ
* ณ AUTOR  :   Ing. Federico Montero Valdiviezo REGION GRAU      ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Compag  IN 1 ORDER TAG Compag1  ALIAS Compag
USE Cajas   IN 2 ORDER TAG Cajas1   ALIAS Caja
USE Hojanu  IN 3 ORDER TAG Hojanu1  ALIAS Hojanu
USE ITEAUT  IN 4 ORDER TAG ITEAUT4  ALIAS ITEA
USE Parmae  IN 5 ORDER TAG Parmae1  ALIAS Parma

ON KEY LABEL F10 KEYBOARD CHR(23)
PRIVATE x_cta,vnummes,vperiodo
STORE SPACE(14) TO x_cta
HIDE POPUP Pop_08
IF LASTKEY()#27 
	DO WHILE .T.
		DEFINE WINDOW Wind_13 FROM 06,18 TO 18,79 ;
		     TITLE ' Saldos de Autorizaciขn de Giro -Tesoro ' COLOR SCHEME 05
			 ACTIVATE WINDOW Wind_13
  		x_sw   = .F.
		x_sw1  = .F.
	    VMes     = MONTH(DATE())
        Vnummes  = SPACE(2)
        IF VMES<10
      	   vnummes  ='0'+ALLTRIM(STR(MONTH(DATE())))
        ELSE
           vnummes=STR(VMES,2)   
        ENDIF   
		vperiodo = RIGHT(STR(YEAR(DATE())),2)
		@  0,1 SAY "         Periodo :" GET vperiodo PICTURE '!!'
		@  1,1 SAY "             Mes :" GET vnummes PICTURE '!!' VALID Val_Para(vnummes,'FECMES',' ',20,9)
		@  2,1 SAY "Cuenta Corriente :" GET x_cta VALID val_fun('Caja', 'Codctc', "CodCtc+' '+LEFT(Descri,20)",x_cta,1,2,22)
		READ
		IF LASTKEY()=27
		   EXIT
		ENDIF
		DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
		SELE Caja
		SEEK x_cta
		STORE 0 TO xsdo,xaut,xgir,xhan
		
		SELE ITEA
		Vind1=SYS(3)+'.IDX'
		INDEX ON periodo+nummes+codctc+codpart TO (vind1) FOR periodo = vperiodo AND nummes = ALLT(vnummes) AND codctc = ALLT(x_cta)
		DO WHILE !EOF()
		    xaut=xaut+ITEA.totasig
		    SKIP
		ENDDO   
		SET INDEX TO

		SELE Compag
		Vind2=SYS(3)+'.IDX'
		INDEX ON periodo+nummes+codctc TO (vind2) FOR nummes = ALLT(vnummes) AND periodo = vperiodo AND codctc = ALLT(x_cta) AND Estado<>'99' 
		DO WHILE !EOF()
	       xgir=xgir+compag.import
	       SKIP
		ENDDO   
		SET INDEX TO
		
		SELE Hojanu
		Vind3=SYS(3)+'.IDX'
		INDEX ON periodo+nummes+codctc TO (vind3)  FOR nummes = ALLT(vnummes) AND periodo = vperiodo AND codctc = ALLT(x_cta)  AND Estado<>'99' 
		DO WHILE !EOF()
		    xhan=xhan+hojanu.import
		    SKIP
		ENDDO   
		SET INDEX TO
		
   		*-----------------------------------------		
		xsdo = ROUND(xaut - xgir +xhan,2)
		*-----------------------------------------
        DEACTIVATE WINDOW xwait
			@ 4,01 SAY 'Total Autorizado       '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :' 
			@ 4,38 SAY xaut PICTURE '999,999,999,999.99' 
			@ 4,58 SAY '-'
			@ 5,01 SAY 'Total Girado           '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :'
			@ 5,38 SAY xgir  PICTURE '999,999,999,999.99' 
			@ 5,58 SAY '+'
			@ 6,01 SAY 'Total Hojas de Anulac. '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :'
			@ 6,38 SAY xhan  PICTURE '999,999,999,999.99' 
			@ 7,38 SAY REPLICATE('-',18) 
			@ 8,01 SAY 'Saldo Autorizado ===>  '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :'
			@ 8,38 SAY xsdo  PICTURE '999,999,999,999.99' 
			@ 10,05 GET okcancel FUNCTION '*TH \!\<Continuar;\!\<Imprimir\;\!\<Salir' DEFAULT 1 SIZE 1,11,8 
			READ  CYCLE
			IF okcancel = 3 OR LASTKEY() = 27
			   EXIT
			ENDIF
			IF okcancel = 2
	       	   DO repprg WITH "Sdo_Aut"," Saldo de Cuenta Corriente ",1
		    ENDIF   
		    DEACTIVATE WINDOW wind_13
		    x_cta= SPACE(14) 
	ENDDO	    
ENDIF
RELEASE  WINDOW wind_13
SHOW popup pop_08
ACTIVATE SCREEN
CLOSE    DATABASES
RETURN



FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


PROCEDURE Sdo_aut &&Programa Reporte usa repprg F.M.V.
*--------------
PARAMETERS Xcop
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
SELECT Caja
@ 0,00  SAY CHR(18)
@ 1,03   SAY ALLTRIM(CIA)
@ 1,68  SAY "PAG:"
@ 1,76  SAY ALLTRIM(STR(_PAGENO,8))
@ 2,03   SAY "Sdo_Aut"
@ 2,68 SAY "FECHA:"
@ 2,76 SAY DATE()           
@ 4,20 SAY CHR(14)
@ 4,22 SAY "<< SALDO   AUTORIZADO  >>"
@ 5,00 SAY CHR(27)+CHR(18)
@ 7,11 SAY "     Cta cte :"  
@ 7,30 SAY x_cta
@ 7,50 SAY val_fun('Caja','codctc','Descri',x_cta)
@ 9,11 SAY 'Total Autorizado       '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :' 
@ 9,48 SAY xaut PICTURE '999,999,999,999.99' 
@ 9,68 SAY '-'
@10,11 SAY 'Total Girado           '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :'
@10,48 SAY xgir  PICTURE '999,999,999,999.99' 
@10,68 SAY '+'
@11,11 SAY 'Total Hojas de Anulac. '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :'
@11,48 SAY xhan  PICTURE '999,999,999,999.99' 
@12,48 SAY REPLICATE('-',18) 
@13,11 SAY 'Saldo Autorizado ===>  '+mFecha(VAL(vnummes),VAL(vperiodo)) +'     :'
@13,48 SAY xsdo  PICTURE '999,999,999,999.99' 
SET DEVICE TO SCREEN 
RETURN