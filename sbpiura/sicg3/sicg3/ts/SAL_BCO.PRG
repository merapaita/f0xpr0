* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Sal_Bco.Prg  17/08/96                               L:  203  ณ	
* ณ Consulta de Saldos de Cuentas Corrientes y Ahorros           ณ
* ณ AUTOR  :   Ing. Federico Montero Valdiviezo REGION GRAU      ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

USE Compag  IN 1 ORDER TAG Compag1  ALIAS Compag
USE Cajas   IN 2 ORDER TAG Cajas1   ALIAS Caja
USE Salbcos IN 3 ORDER TAG Salbcos1 ALIAS Salbcos
USE MovBco  IN 4 ORDER TAG MovBco1  ALIAS MovB
USE Cheque  IN 5 ORDER TAG Cheque1  ALIAS Cheque
USE Hojanu  IN 6 ORDER TAG Hojanu1  ALIAS Hojanu
USE Reten   IN 7 ORDER TAG Reten1   ALIAS Reten

ON KEY LABEL F10 KEYBOARD CHR(23)
PRIVATE x_cta
STORE SPACE(14) TO x_cta
HIDE POPUP Pop_08
IF LASTKEY()#27 
	DO WHILE .T.
		DEFINE WINDOW Wind_13 FROM 06,18 TO 18,79 ;
		     TITLE ' Saldo de Cuenta ' COLOR SCHEME 05
			 ACTIVATE WINDOW Wind_13
  		x_sw   = .F.
		x_sw1  = .F.
		xfec   = DATE()
		@  0,1 SAY "Cuenta Corriente :" GET x_cta VALID val_fun('Caja', 'Codctc', "CodCtc+' '+LEFT(Descri,20)",x_cta,1,0,22)
		@  1,1 SAY "           Fecha :" GET xfec
		READ
		IF LASTKEY()=27
		   EXIT
		ENDIF
		DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
		ACTIVATE WINDOW Xwait
		@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
		SELE Caja
		SEEK x_cta
		STORE 0 TO xsdo1, xsdo2, xtot1,xtot2,xabo,xcar,xsdo3,xhan,Xchq
		x_sw     = ALLTRIM(Caja.moneda)
		y_cta    = LEFT(Caja.docref,14)
		x_mesant = IIF(MONTH(xfec)=1,'12',PADL(MONTH(xfec)-1,2,'0'))
		x_mes    = PADL(MONTH(xfec),2,'0')
		x_perant = IIF(MONTH(xfec)=1,'95',RIGHT(PADL(YEAR(xfec),4,'0'),2))
		x_per    = RIGHT(PADL(YEAR(xfec),4,'0'),2)
	
		SELE Salbcos
		IF x_sw='Y'
		   SEEK y_cta+x_perant+ALLT(x_mesant)
		ELSE   
		   SEEK x_cta+x_perant+ALLT(x_mesant)
		ENDIF
		IF FOUND()
		   *-------------------------------
		   xsdo1 = ROUND(saldeb - salhab,2)
		   *-------------------------------
		ENDIF   	
		SELE Movb
		Vind1=SYS(3)+'.IDX'
		DO CASE
			CASE x_sw='Y'
				INDEX ON periodo+nummes+codctc TO (vind1) FOR periodo = x_per  AND nummes = ALLT(x_mes) AND codctc = ALLT(y_cta) AND fecha<=xfec AND ALLTRIM(transa)='ABO'	
				DO WHILE !EOF()
				    xabo=xabo+movb.monto
				    SKIP
				ENDDO   
				SET INDEX TO
			CASE x_sw='X'
				INDEX ON periodo+nummes+codctc TO (vind1) FOR  periodo = x_per AND nummes = ALLT(x_mes)  AND codctc = ALLT(x_cta) AND fecha<=xfec AND ALLTRIM(transa)='ABO'	
				DO WHILE !EOF()
				    xabo=xabo+movb.monto
				    SKIP
				ENDDO   
				SET INDEX TO
		    OTHER
				INDEX ON periodo+nummes+codctc TO (vind1) FOR  periodo = x_per AND nummes = ALLT(x_mes)  AND codctc = ALLT(x_cta) AND fecha<=xfec 
				DO WHILE !EOF()
				    DO CASE
				       CASE ALLTRIM(transa)='ABO'	
						    xabo=xabo+movb.monto
				       CASE ALLTRIM(transa)='CAR'	
						    xcar=xcar+movb.monto
					ENDCASE	    
				    SKIP
				ENDDO   
				SET INDEX TO
		    ENDCASE	

			SELE Cheque
			Vind2=SYS(3)+'.IDX'
			IF x_sw='X'
				INDEX ON periodo+nummes+codctc TO (vind2) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(y_cta) AND Estado<>'99' AND fecchq<=xfec
				DO WHILE !EOF()
				    xchq=xchq+cheque.valchq
				    SKIP
				ENDDO   
				SET INDEX TO
			ELSE
				INDEX ON periodo+nummes+codctc TO (vind2) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(x_cta) AND Estado<>'99' AND fecchq<=xfec
				DO WHILE !EOF()
				    xchq=xchq+cheque.valchq
				    SKIP
				ENDDO   
				SET INDEX TO
			ENDIF		
	
			SELE Hojanu
			Vind3=SYS(3)+'.IDX'
			IF x_sw='X'
				INDEX ON periodo+nummes+codctc TO (vind3) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(y_cta) AND Estado<>'99' AND fechan<=xfec
				DO WHILE !EOF()
				    xhan=xhan+hojanu.import
				    SKIP
				ENDDO   
				SET INDEX TO
			ELSE	
				INDEX ON periodo+nummes+codctc TO (vind3) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(x_cta) AND Estado<>'99' AND fechan<=xfec
				DO WHILE !EOF()
				    xhan=xhan+hojanu.import
				    SKIP
				ENDDO   
				SET INDEX TO
			ENDIF	
	   
	   		*-----------------------------------------		
			xsdo2 = ROUND(xabo - xcar - xchq +xhan,2)
			*-----------------------------------------
			
			*-----------------------------
			xtot1 = ROUND(xsdo1 + xsdo2,2)
			*-----------------------------
	        DEACTIVATE WINDOW xwait
			@ 3,01 SAY 'Saldo Libro Bancos    '+mFecha(VAL(x_mesant),VAL(x_perant)) +'     :' 
			@ 3,38 SAY xsdo1 PICTURE '999,999,999,999.99' 
			@ 3,58 SAY '+'
			@ 4,01 SAY 'Mov. de Bancos al '+STR(DAY(xfec),2)+' de '+mFecha(VAL(x_mes),VAL(x_per)) +'  :'
			@ 4,38 SAY xsdo2 PICTURE '999,999,999,999.99' 
			@ 4,58 SAY '+'
			@ 5,38 SAY REPLICATE('-',18) 
			@ 6,01 SAY 'Saldo Disp. al '+STR(DAY(xfec),2)+' de '+mFecha(VAL(x_mes),VAL(x_per)) +'     :'
			@ 6,38 SAY xtot1 PICTURE '999,999,999,999.99' 
			@ 09,05 GET okcancel FUNCTION '*TH \!\<Continuar;\!\<Imprimir\;\!\<Salir' DEFAULT 1 SIZE 1,11,8 
			READ  CYCLE
			IF okcancel = 3 OR LASTKEY() = 27
			   EXIT
			ENDIF
			IF okcancel = 2
	       	   DO repprg WITH "Sdo_Ctc"," Saldo de Cuenta Corriente ",1
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


PROCEDURE Sdo_ctc &&Programa Reporte usa repprg F.M.V.
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
@ 2,03   SAY "SdoProg"
@ 2,68 SAY "FECHA:"
@ 2,76 SAY DATE()           
@ 4,20 SAY CHR(14)
@ 4,22 SAY "<< SALDO   DE  BANCOS  >>"
@ 5,00 SAY CHR(27)+CHR(18)
@ 7,11 SAY "     Cta cte :"  
@ 7,30 SAY x_cta
@ 7,50 SAY val_fun('Caja','codctc','Descri',x_cta)
@ 9,11 SAY 'Saldo Libro Bancos '+mFecha(VAL(x_mesant),VAL(x_perant)) +'  :' 
@ 9,48 SAY xsdo1 PICTURE '999,999,999,999.99' 
@ 9,68 SAY '+'
@10,11 SAY 'Movimiento de Bancos '+mFecha(VAL(x_mes),VAL(x_per)) +'  :'
@10,48 SAY xsdo2 PICTURE '999,999,999,999.99' 
@10,68 SAY '+'
@11,48 SAY REPLICATE('-',18) 
@12,11 SAY 'Saldo Disponible      '+mFecha(VAL(x_mes),VAL(x_per)) +' :'
@12,48 SAY xtot2 PICTURE '999,999,999,999.99' 
SET DEVICE TO SCREEN 
RETURN