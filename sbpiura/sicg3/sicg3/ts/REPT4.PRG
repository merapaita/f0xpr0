* ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
* ณ RepT4.PRG         10/09/96                         L: 1314   ณ	
* ณ Emisiขn del T-4                                              ณ
* ณ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ณ
* ิออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออพ

*- Abre archivos

USE AuGiRe  IN  1 ORDER TAG AuGiRe1                 ALIAS AutG
USE DetCta  IN  2 ORDER TAG Detcta1                 ALIAS extr
USE Cajas   IN  3 ORDER TAG Cajas1                  ALIAS Caja       
USE cheque  IN  4 ORDER TAG cheque1                 ALIAS chqe
USE parmae  IN  5 ORDER TAG parmae1				    ALIAS parma
USE movbco  IN  6 ORDER TAG movbco1				    ALIAS movb
USE ConBan  IN  7 					                ALIAS CONBAN     
USE Hojanu  IN  8 ORDER TAG hojanu1				    ALIAS hojanu

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,Vfecha,xMes,xano,vcuenta,vsubcta,vsect
PUBLIC vTotal1,vTotal2,vTotal3,vTotal4,VTOTAL5,vtotal6,vtotal7,Vtotal8,vtotal9,VTotalc,vtotalu
DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 

STORE .T. TO Agrega
vFecha=DATE()
STORE 0 TO vtotal1,vtotal2,vtotal3,vtotal4,vtotal5,vtotal6,vtotal7,vtotal8,vtotal9,Vtotalc,vtotalu
STORE SPACE(3) TO vfun
DO Inicia
ACTIVATE POPUP RepT
DO salida
RETURN


PROCEDURE Inicia
*---------------
  IF Escolor
     DEFINE POPUP RepT FROM 14,55  to 17,76  SHADOW COLOR &l_col
  ELSE
     DEFINE POPUP RepT FROM 14,55  to 17,76  COLOR SCHEME c_popup
  ENDIF
  DEFINE BAR  1 OF RepT  PROMPT 'T - 4 \<Regional  '
  DEFINE BAR  2 OF RepT  PROMPT 'T - 4 \<Sede Central'

  ON SELECTION POPUP RepT do Selpop
RETURN


PROCEDURE Selpop
*---------------
 vCuenta=SPACE(14)
 xMes     = padl(allt(str(MONTH(VFECHA))),2,'0')
 xano     = SUBST(ALLT(STR(YEAR(VFECHA))),3,2)
 STORE BAR() TO choice
 DO CASE
    CASE choice = 1
         DEFINE WINDOW _fe FROM 10,10 TO 16,70 COLOR SCHEME 10 TITLE " Cuenta "
         ACTIVATE WINDOW _fe
         SELE Caja 
         SET ORDER TO Cajas9
         CLEAR
         @ 1,4 SAY "  Fecha: " GET vfecha
         @ 2,4 SAY " Cuenta: " GET vCuenta VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",vCuenta,1,2,22)
         READ
		 SELEC CAJA
		 SEEK ALLT(VCUENTA)
		 vsubcta=ALLT(caja.codscta)
	     VFUN=ALLT(CAJA.codfte)
         RELEASE WINDOW _fe
         IF LASTKEY()#27
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Procesando T4....." COLOR W+/BR*
		    xMes     = padl(allt(str(MONTH(VFECHA))),2,'0')
 		    xano     = SUBST(ALLT(STR(YEAR(VFECHA))),3,2)
			= Sum11R()
			= sum121()
			= sum122()
			= sum123()
			= sum15()
			= Sum16()
			= Sum21R()
			DEACTIVATE WINDOW xwait
            DO REPORTE WITH 2, "Rep_T4", ' Reporte T-4 Regional',1,.F.,.T.
            SET ORDER TO Cajas1
         ENDIF
   CASE choice = 2
         DEFINE WINDOW _fe FROM 10,10 TO 16,70 COLOR SCHEME 10 TITLE " Cuenta "
         ACTIVATE WINDOW _fe
         CLEAR
         @ 1,4 SAY "  Fecha: " GET vfecha
         @ 2,4 SAY " Cuenta: " GET vCuenta VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",vCuenta,1,2,22)
         READ
		 SELEC CAJA
		 SEEK ALLT(VCUENTA)
		 vsubcta=caja.codscta
		 vsect= caja.sector
         VFUN=CAJA.codfte
         RELEASE WINDOW _fe
         IF LASTKEY()#27
			ACTIVATE WINDOW Xwait
			@0,0 SAY " Procesando T4....." COLOR W+/BR*
		    xMes     = padl(allt(str(MONTH(VFECHA))),2,'0')
 		    xano     = SUBST(ALLT(STR(YEAR(VFECHA))),3,2)
			= sum11S()
			= sum121()
			= sum122()
			= sum123()
			= sum15()
			= sum16()
			= sum18S()
			= sum21()
			DEACTIVATE WINDOW xwait
            DO REPORTE WITH 2, "Rep_T4", ' Reporte T-4 ',1,.F.,.T.
         ENDIF
 ENDCASE

 IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
*SHOW POPUP menu, pop_05, RepT
RETURN


PROCEDURE Salida
*---------------
  RELEASE    POPUP  RepT
  ACTIVATE   SCREEN
  CLOSE DATA
RETURN


FUNCTION SUM11R         && (11)Suma el total asignado de Augire por cuenta,mes,periodo
*---------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal1
SELECT AutG
vind = SYS(3) + '.idx'
INDEX ON CODCTCEN+PERIODO+NUMMES TO (vInd) FOR 	CODCTCEN=VCUENTA and PERIODO=XANO  AND VAL(NUMMES)<=VAL(XMES)
SCAN 
     vtotal1 = vtotal1 + Autg.totasig
ENDSCAN
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN VTOTAL1


FUNCTION SUM11S       && (11)Suma el total cheques girados acumulados al mes por cuenta 
*---------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal1
SELECT chqe
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERIODO+NUMMES TO (vInd) FOR CODCTC=VCUENTA AND PERIODO=XANO  AND VAL(NUMMES)<= VAL(XMES) AND (ESTADO#'99' OR nummesha>=xmes)
SCAN 
     vtotal1 = vtotal1 + chqe.VALCHQ
ENDSCAN
SET INDEX TO
vind1 = SYS(3) + '.idx'
SELECT movb
INDEX ON CODCTC+PERIODO+NUMMES TO (vInd1) FOR  CODCTC=VCUENTA AND PERIODO=xano AND NUMMES<=ALLT(XMES) AND tipdoc='N/C' AND ESTADO#'99'
SCAN 
        vtotal1 = vtotal1 + movb.monto
ENDSCAN
vind2 = SYS(3) + '.idx'
SELECT hojanu
INDEX ON CODCTC+PERIODO+NUMMES TO (vInd2) FOR  CODCTC=VCUENTA AND PERIODO=xano AND NUMMES<=ALLT(XMES) AND ESTADO#'99'
SCAN 
        vtotal1 = vtotal1 - hojanu.import
ENDSCAN
SET INDEX TO
SELE movb
SET INDEX TO
SELE chqe
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN VTOTAL1



FUNCTION SUM121       && (12)Suma el total de cargos de Extbco acumulados por cuenta
*--------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal2
SELECT Extr
vind = SYS(3) + '.idx'
INDEX ON CODCTC+MES TO (vInd) FOR  VAL(MES)<=VAL(XMES) AND CODCTC=VCUENTA AND tipdoc<>'OTR' AND tipdoc<>'NOC'
SCAN
     vtotal2 = vtotal2+extr.cancar
ENDSCAN     
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN VTOTAL2

FUNCTION SUM122      && (18)Suma el total de cheques de Valchq por cuenta,mes,periodo y conban="+"
*---------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal3
SELECT conban
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERCON+MESCON TO (vInd) FOR CODCTC=VCUENTA AND PERCON=xano AND MESCON=ALLT(XMES) AND conban='+' AND ESTADO#'99'
SCAN 	 
    vtotal3 = vtotal3 + conban.VALCHQ
ENDSCAN
vtotal3=vtotal3*-1
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN  VTOTAL3


FUNCTION SUM123      && (18)Suma el total de cheques de Valchq por cuenta,mes,periodo y conban="C"
*---------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotalc
SELECT conban
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERCON+MESCON TO (vInd) FOR CODCTC=VCUENTA AND PERCON=xano AND MESCON=ALLT(XMES) AND conban='C' AND ESTADO#'99'
SCAN 	 
    vtotalc = vtotalc + conban.VALCHQ
ENDSCAN
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN  VTOTALc

FUNCTION SUM15      && (15)Suma el total de cheques de Valchq por cuenta y conban='>'
*-------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal4
SELECT conban
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERCON+MESCON TO (vInd) FOR  CODCTC=VCUENTA AND PERCON=xano AND MESCON=ALLT(XMES) AND conban=">" AND ESTADO#'99'
SCAN 
        vtotal4 = vtotal4 + conban.VALCHQ
ENDSCAN
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN Vtotal4

FUNCTION SUM16      && (16)Suma el total de cheques de Valchq por cuenta y conban=I
*-------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal5
SELECT conban
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERCON+MESCON TO (vInd) FOR  CODCTC=VCUENTA AND PERCON=xano AND MESCON=ALLT(XMES) AND conban="I" AND ESTADO#'99'
SCAN 
        vtotal5 = vtotal5 + conban.VALCHQ
ENDSCAN
vtotal5=vtotal5*-1
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN Vtotal5

FUNCTION SUM18S     && (18)Suma el total de Reversiones de Saldos no utilizados de Movbco por cuenta 
*-------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal6
SELECT movb
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERIODO+NUMMES TO (vInd) FOR  CODCTC=VCUENTA AND PERIODO=xano AND VAL(NUMMES)<=VAL(ALLT(XMES)) AND ALLT(motivo)="23" AND ESTADO#'99'
SCAN 
        vtotal6 = vtotal6 + movb.monto
ENDSCAN
vtotal6=vtotal6*-1
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN Vtotal6

FUNCTION SUM21R     && (21)Suma el total de cheques de Valchq por cuenta,mes,periodo y conban=" "
*---------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal7
SELECT conban
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERCON+MESCON TO (vInd) FOR CODCTC=VCUENTA AND PERCON=xano AND MESCON=ALLT(XMES) AND conban='' AND ESTADO#'99'
SCAN 	
    vtotal7 = vtotal7 + conban.VALCHQ
ENDSCAN
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN  VTOTAL7


FUNCTION SUM21      && (21)Suma el total de cheques de Valchq por cuenta,mes,periodo y conban=" "
*---------------
vAli=ALIAS()
vOrd=ORDER()
STORE 0 TO vtotal7
SELECT conban
vind = SYS(3) + '.idx'
INDEX ON CODCTC+PERCON+MESCON TO (vInd) FOR CODCTC=VCUENTA AND PERCON=xano AND MESCON=ALLT(XMES) AND conban=' ' AND ESTADO#'99'
SCAN 	
    vtotal7 = vtotal7 + conban.VALCHQ
ENDSCAN
SET INDEX TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN  VTOTAL7


FUNCTION MFecha
*--------------
PARAMETERS xmes &&, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) &&+ ' ' + STR(xAno,2)
