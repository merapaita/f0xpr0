* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ Sal_cta.PRG  22/08/96                              L:  228   ณ	
* ณ Consulta de Saldos de Cuenta Por Banco                       ณ
* ณ         : Ing. Federico Montero Valdiviezo REGION GRAU       ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

*-Abriendo archivos
USE Cajas   IN 1 ORDER Cajas1   ALIAS Caja
USE Parmae  IN 2 ORDER Parmae1  ALIAS Parma
USE Salbcos IN 3 ORDER Salbcos1 ALIAS Salbcos
USE MovBco  IN 4 ORDER MovBco1  ALIAS MovB
USE Cheque  IN 5 ORDER Cheque1  ALIAS Cheque
USE Hojanu  IN 6 ORDER Hojanu1  ALIAS Hojanu
STORE SPACE(03) TO vbanco
STORE SPACE(02) TO vnummes
ACTIVATE SCREEN
  vcol ="w+/w,n/w,W/W,w/W,,w+/b"
  IF Escolor
    DEFINE WINDOW CONBA  FROM 13,55 TO 18,79  SHADOW COLOR &vcol
  ELSE
    DEFINE WINDOW CONBA  FROM 13,55 TO 18,79  COLOR SCHEME C_POPUP
  ENDIF
  ACTIVATE WINDOW CONBA 
	    VMes     = MONTH(DATE())
        Vnummes  = SPACE(2)
        IF VMES<10
      	   vnummes  ='0'+ALLTRIM(STR(MONTH(DATE())))
        ELSE
           vnummes=STR(VMES,2)   
        ENDIF   
  vperiodo    = RIGHT(DTOC(DATE()),2)
  STORE 1 TO vliscta,vlisfte,vlistip,vlissec
  STORE SPACE(1) TO vtip
  STORE SPACE(3) TO vcodfte,vsec
  @ 0, 0 SAY '     Saldos x Banco '
  @ 2, 1 SAY '   Mes y Aคo : ' GET vnummes PICTURE '!!' VALID Val_Para(vnummes,'FECMES','C',15) 
  @ 2,20 GET vperiodo PICTURE '!!'
  @ 3, 1 SAY "       Banco : "  GET vbanco   PICTURE '!!!' VALID Val_Para(vbanco,'BANCOS','C',15)
READ 
IF LASTKEY()=27
	   CLOSE DATA
	   RETURN
ENDIF	
SELE caja
DEFINE WINDOW VerSal FROM 1,0 TO 23,79 ;
COLOR SCHEME 10 TITLE "ฎF2ฏ Busca Cuenta Corriente  ฎF4ฏ Listar ฒ Saldo Actual  ฑ"+val_para(Vbanco,'BANCOS','D',6,15)
DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
ACTIVATE WINDOW Xwait
@0,0 SAY " Espere un momento ......... " COLOR W+/BR*
filaux = SYS(3) + '.DBF'
Vind   = SYS(3) + '.IDX'
SELECT 8
CREATE TABLE (filaux);
  (CodCtc    C(014),;
   NumMes    C(002),;
   descri    C(025),;
   mescie    C(002),;
   percie    C(002),;
   saldo     N(18,2))
USE

USE (filaux) IN 8  EXCLUSIVE   ALIAS Temp
SELE Caja
SET FILT TO Tipo$'CCRE' AND CLASE#'T' AND ALLT(DOCREF)#'95' AND banco=ALLT(vbanco)
GO TOP
vCodCtc = caja.CodCtc
vdescri = caja.descri
vmescie = caja.mescie
vpercie = caja.percie
SCAN 
	 vCodCtc = caja.CodCtc
	 vdescri = caja.descri
	 vmescie = caja.mescie
	 vpercie = caja.percie
     STORE 0 TO vsaldo
     =salbcos(caja.codctc,vnummes,vperiodo)
     SELECT Temp
     APPEND BLANK
     REPLACE Temp.CodCtc  WITH vCodCtc;
             Temp.nummes  WITH vnummes;
             Temp.saldo   WITH vsaldo;
             Temp.mescie  WITH vmescie;
             Temp.percie  WITH vpercie;
             Temp.descri  WITH vdescri
     SELE caja        
ENDSCAN

SELE TEMP
INDEX ON ALLT(codctc) TO (vind)
ON KEY LABEL F2  DO Busca1
ON KEY LABEL F4  DO imprime
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOEDIT NOAPPEND FIELDS ;
Codctc :H='Cuenta.' :R,;
Descri :H='Descripciขn' :R :45,;
saldo  :H='Saldo Actual' ,;
x1=IIF(EMPTY(mescie) AND EMPTY(percie),'SIN CONCILIAR',mFecha(VAL(mescie),VAL(percie))) :H='Ultima Conciliaciขn' :R :15 ;
WINDOW VerSal
ON KEY LABEL F2  
ON KEY LABEL F4  
ON KEY LABEL F10 
SET INDEX TO
CLOSE DATA  
RETURN


PROCEDURE Busca1
*----------------

  DEFINE WINDOW VerSal FROM 10,20 TO 14,60 ;
  COLOR SCHEME 5 
  ACTIVATE WINDOW versal
  vreg = RECNO()
  vcodctc = SPACE(14)
  @ 1,1 SAY "    Cuenta : "  GET vcodctc
  READ
  SEEK ALLT(vcodctc)
  IF !FOUND()
	GO vreg	
  ENDIF
DEACTIVATE WINDOW versal
RETURN 

FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

FUNCTION salbcos
*---------------
PARAMETERS x_cta,x_mes,x_per
Vali=ALIAS()
Vord=ORDER()
x_mesant = '0'+ALLTRIM(STR(VAL(x_mes)-1))
x_perant = IIF(x_mes='01','95',x_per)
SELE Caja
SEEK x_cta
STORE 0 TO xsdoA,xabo,xcar,xret,xhan,Xchq,vsaldo
x_sw     = ALLTRIM(Caja.moneda)
y_cta    = LEFT(Caja.docref,14)

SELE Salbcos
IF x_sw='Y'
   SEEK y_cta+x_perant+ALLT(x_mesant)
ELSE   
   SEEK x_cta+x_perant+ALLT(x_mesant)
ENDIF
IF FOUND()
	   *-------------------------------
	   xsdoA = ROUND(saldeb - salhab,2)
	   *-------------------------------
ENDIF   	
SELE Movb
	Vind1=SYS(3)+'.IDX'
	DO CASE
		CASE x_sw='Y'
			INDEX ON periodo+nummes+codctc TO (vind1) FOR periodo = x_per  AND nummes = ALLT(x_mes) AND codctc = ALLT(y_cta)  AND ALLTRIM(transa)='ABO'	
				DO WHILE !EOF()
				    xabo=xabo+movb.monto
				    SKIP
				ENDDO   
				SET INDEX TO
			CASE x_sw='X'
				INDEX ON periodo+nummes+codctc TO (vind1) FOR  periodo = x_per AND nummes = ALLT(x_mes)  AND codctc = ALLT(x_cta) AND ALLTRIM(transa)='ABO'	
				DO WHILE !EOF()
				    xabo=xabo+movb.monto
				    SKIP
				ENDDO   
				SET INDEX TO
		    OTHER
				INDEX ON periodo+nummes+codctc TO (vind1) FOR  periodo = x_per AND nummes = ALLT(x_mes)  AND codctc = ALLT(x_cta) 
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
				INDEX ON periodo+nummes+codctc TO (vind2) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(y_cta) AND Estado<>'99' 
				DO WHILE !EOF()
				    xchq=xchq+cheque.valchq
				    SKIP
				ENDDO   
				SET INDEX TO
			ELSE
				INDEX ON periodo+nummes+codctc TO (vind2) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(x_cta) AND Estado<>'99' 
				DO WHILE !EOF()
				    xchq=xchq+cheque.valchq
				    SKIP
				ENDDO   
				SET INDEX TO
			ENDIF		
	
			SELE Hojanu
			Vind3=SYS(3)+'.IDX'
			IF x_sw='X'
				INDEX ON periodo+nummes+codctc TO (vind3) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(y_cta) AND Estado<>'99' 
				DO WHILE !EOF()
				    xhan=xhan+hojanu.import
				    SKIP
				ENDDO   
				SET INDEX TO
			ELSE	
				INDEX ON periodo+nummes+codctc TO (vind3) FOR nummes = ALLT(x_mes) AND periodo = x_per AND codctc = ALLT(x_cta) AND Estado<>'99' 
				DO WHILE !EOF()
				    xhan=xhan+hojanu.import
				    SKIP
				ENDDO   
				SET INDEX TO
			ENDIF	
			vsaldo=xsdoa+xabo+xhan-xchq-xcar
SELE (vali)
SET ORDER TO (vord)			
RETURN vsaldo

PROCEDURE imprimE
*-----------------
DO REPORTE WITH 2,'SalxBco','Saldos por Banco'
RETURN