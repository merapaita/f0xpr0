* ............................................................... 
*
*  Program				:	AsiCal.PRG  
*	
*  Descrip				:	Asignaci¢n de Calendarios									 
*  Upgrade				:	Diciembre 1999
*
* .................................................................
* 
USE Itepar  IN 1  ORDER TAG Itepar1               ALIAS Itepar
USE Calen   IN 2  ORDER TAG Calen2                ALIAS Calen
USE Parmae  IN 3  ORDER TAG Parmae1               ALIAS Parma
PUBLIC WMES,VUNIGES,VUNIEJE
STORE SPACE(4) TO WMES
DO Inicia
DO salida
RETURN

PROCEDURE Inicia
*---------------
vcol ="w+/w,n/w,W/W,w/W,,w+/b"
IF Escolor
	DEFINE WINDOW AsiCenTe FROM 12,52 TO 17,78  SHADOW COLOR &vcol
 ELSE
	DEFINE WINDOW AsiCenTe FROM 12,52 TO 17,78  COLOR SCHEME c_popup
ENDIF
ACTIVATE WINDOW AsiCenTe
xbalance = 'Mensual'
xdigitos = 4
xMes     = MONTH(DATE())
xAno     = VAL(SUBSTR(STR(YEAR(DATE()),4),3,2))
vuniges  = '  '
vunieje  = '   '
@  0,2 SAY '       UNI.GES. :' GET vUniGes	PICTURE '!!'	VALID val_para(vUniGes,'UNIGES','C')
@  1,2 SAY '       UNI.EJE. :' GET vUniEje 	PICTURE '!!!'	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,'C')
@  2,0 SAY 'Gen. Calendario :' + xBalance
@  3,0 SAY '      Mes y A¤o :' GET xMes     PICTURE '99' WHEN xBalance = 'Mensual';
				VALID xMes > 0 .AND. xMes <= 12
@  3,20 GET xAno                           PICTURE '9999'
READ

wmes = PADL(xmes,2,'0')
IF LASTKEY() # 27
	SELECT parma
	SEEK 'ASICAL'+wmes
	ok=.t.
	IF FOUND()
		IF YESNO('Ya se asigno anteriormente ...¨Deseas Reasignar?')
			SELECT calen
			GO TOP
			delete for nummes = wmes AND IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) 
		 ELSE
			ok=.f.
		ENDIF
	 ELSE
		IF f_appd()
			REPLACE tipo   WITH 'ASICAL'
			REPLACE CODIGO WITH wmes
		ENDIF
	ENDIF
	IF ok
		DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05
		ACTIVATE WINDOW Xwait
		vind1= SYS(3) +'.idx'
		@ 0,10 SAY " Espere un Momento...Asignaci¢n de Calendario en Proceso!" COLOR W+/BR*
		DO CASE
			CASE xbalance = 'Mensual'
				DO asigna   &&proceso que genera los calendarios
		ENDCASE
		RELEASE WINDOW Xwait
	ENDIF
 ENDIF
 DEACTIVATE WINDOW AsiCenTe
 IF !escolor
    RESTORE SCREEN FROM principal
 ENDIF
 SHOW POPUP menu, pop_09
RETURN


FUNCTION Asigna  
*--------------
SELECT Itepar
set filter to UNIGES+UNIEJE=ALLT(VUNIGES)+ALLT(VUNIEJE)
go top
vmonto = 'M_'+wmes
wMES = ALLT(wMES)
SCAN
	SCATTER MEMVAR
	vkey = m.PERIODO+m.uniges+m.unieje+m.CODCAD+m.CODFTE+m.CODPART+WMES
	vkey1= m.PERIODO+m.uniges+m.unieje+m.CODCAD+m.CODFTE+m.CODPART
	SELE CALEN
	SEEK vkey
	IF FOUND() 
		REPLACE valpart WITH valpart+&vmonto
	ELSE
		APPEND BLANK
	    GATHER MEMVAR
		REPLACE valpart WITH &vmonto
	    REPLACE nummes  WITH wmes
	ENDIF
	GO TOP
	SEEK vkey1
	vacu=0
	SCAN WHILE PERIODO+uniges+unieje+CODCAD+CODFTE+CODPART=m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+m.codpart
		REPLACE itepar.TOTCAL WITH vacu+CALEN.VALPART
		IF NUMMES<'04'
			REPLACE CALEN.VALPRE WITH itepar.TOTCAL
		ELSE
			IF NUMMES<'07'
				REPLACE CALEN.VALPRE WITH itepar.TOTCAL-itepar.TRI_01
			ELSE
				IF NUMMES<'10'
					REPLACE CALEN.VALPRE WITH itepar.TOTCAL-(itepar.TRI_01+itepar.TRI_02)
				ELSE
					REPLACE CALEN.VALPRE WITH itepar.TOTCAL-(itepar.TRI_01+itepar.TRI_02+itepar.TRI_03)
				ENDIF
			ENDIF
		ENDIF	
		vacu = vacu+CALEN.VALPART
	ENDSCAN
	SELECT itepar
ENDSCAN
SET FILTER TO
RETURN

PROCEDURE Salida
*---------------
RELEASE    WINDOW AsiCenTe
ACTIVATE   SCREEN
CLOSE DATABASE
RETURN
