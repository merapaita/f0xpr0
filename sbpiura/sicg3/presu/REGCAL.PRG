*-------------------------------------------------------------------------
* Program		:	RegCal.Prg
* Descrip		:	Registra Calendarios
* Autor			:	Ing. Luis Castillo Dur n
* Modificado	:	Javier Frias
* Upgrade		:	Enero 2000
*
*--------------------------------------------------------------------------
*
*- Abriendo Archivos
CLOS DATA
SET EXCLU OFF
USE parmae   IN 1   ORDER TAG parmae1  ALIAS parma
USE calen    IN 2   ORDER TAG calen2   ALIAS calen
USE maepre   IN 3   ORDER TAG maepre1  ALIAS maepre
USE maepar   IN 4   ORDER TAG maepar1  ALIAS presu
USE itepar   IN 5   ORDER TAG itepar1  ALIAS itepar
USE IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehc
USE Itetra   IN 10  ORDER TAG Itetra1  ALIAS Itetra
USE Itecre   IN 11  ORDER TAG Itecre1  ALIAS Itecre
USE HOJMOD   IN 12  ORDER TAG hojmod1  ALIAS hojmod
USE cresup   IN 13  ORDER TAG cresup1  ALIAS cresup
USE Trapar   IN 14  ORDER TAG trapar1  ALIAS trapar
USE CatAsi   IN 15 ORDER TAG CatAsi4  ALIAS CatAsi

*- Mensajes de aviso al usuario
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
vmens11 = 'El Calendario ha sido devuelto'

ON KEY LABEL F9 DO VISTA_DET
SELECT CALEN
SET RELATION TO PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART INTO itepar
SET SKIP TO itepar
SELECT presu
GO TOP

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mmenu
ENDDO

DO fin_opcion

RETURN


PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente           Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 11,79  DOUBLE ;
	TITLE vmens02 COLOR SCHEME 10 FOOTER '[F9] Detalle'   && padre

DEFINE WINDOW wind_2 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Calendarios' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_2a FROM 01,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Calendarios' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 02,02 TO 22,77 DOUBLE ;
	TITLE '² ®F10¯ Escoge ²' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_4 FROM 02,02 TO 22,77 DOUBLE ;
	TITLE '² ®F10¯ Escoge ²' ;
	COLOR SCHEME 10

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
*DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
DEFINE PAD anula   OF mmenu PROMPT 'a\<Nula   '   AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
*ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
ON SELECTION PAD anula  OF mmenu DO anula
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi

RETURN


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR

@  0, 2 SAY "          Periodo :"
@  0,40 SAY "Corr. cadena Fun. :"
@  1, 2 SAY "   Unidad Gestora :"
@  2, 2 SAY " Unidad Ejecutora :"
@  3, 2 SAY "          Funci¢n :"
@  4, 2 SAY "         Programa :"
@  5, 2 SAY "      SubPrograma :"
@  6, 2 SAY "  Activ./Proyecto :"
@  7, 2 SAY "       Componente :"
@  8, 2 SAY "             Meta :"
@  8,40 SAY "        Finalidad :"
@  9, 2 SAY "      Descripci¢n :"
@  9,40 SAY "         Fte.Fto. :"

RETURN

PROCEDURE vista     && Coloca valores de BD en variables y pinta datos
*--------------
SELECT PRESU
SET FILT TO
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
*@  0,60 SAY IIF( m.estado= '00','Aprobado   ',IIF( m.estado = '10','Solicitado ',IIF(m.estado='20','No Aprobad.',IIF(m.estado='50','Atendido   ','Anulado  '))))
=val_codcad(ALLT(M.codcad),M.periodo+M.UNIGES+M.UNIEJE,'C')
@  0,22 SAY m.periodo
@  0,60 SAY m.CODCAD
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA1(maepre.unieje,'UNIEJE'+maepre.uniges,'V',22,40)
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA1(maepre.CodPrg,'CODPRG'+maepre.CodFun,'V',22,40)
@  5,22 SAY VAL_PARA1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,40)
@  6,22 SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
@  7,22 SAY VAL_PARA(maepre.codcom,'CODCOM','V',22,40)
@  8,22 SAY maepre.codmet
@  8,60 SAY maepre.codfin
@  9,22 SAY SUBSTR(maepre.Descri,1,23)
@  9,60 SAY VAL_PARA(m.codfte,'CODFTE','V',60,18)

DO vista_hijo

PROCEDURE vista_hijo
*-------------------
SELECT calen
SET ORDER TO calen2
HIDE POPUP ALL
GO TOP
SEEK m.periodo+m.UNIGES+m.UNIEJE+ALLT(m.codcad)+ALLTRIM(m.codfte)
IF FOUND()
	BROWSE ;
	NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.UNIGES+m.UNIEJE+ALLT(m.codcad)+ALLTRIM(m.codfte) TIMEOUT 0.0001 ;
	WINDOW wind_2 ;
	FIELDS;
	nummes  : H='Mes',;
	codpart : H='P.E.',;
	xx=iif(!EMPTY(ALLTRIM(CodPart)),valasi1('Calen',CodPart,'6','Descri','R'),""):H='Descripci¢n':30,;
	YY=IIF(NUMMES<'04',itepar.TRI_01,IIF(NUMMES<'07',itepar.TRI_02,IIF(NUMMES<'10',itepar.TRI_03,itepar.TRI_04))): H= 'Asig.Trim.' :p='99,999,999':R,; 
	valpart         : H='Cal.Mes' :p='99,999,999',;
	ampliar         : H='Ampliar' :p='99,999,999',;
	SALDO  = (valpart+ampliar): H= 'Tot. Cal.' :p='99,999,999':f,;
	saldo1 = val_saldo(codpart,NUMMES): H= 'Util. Trim.' :p='99,999,999':f

*	xx=IIF(!EMPTY(ALLTRIM(CodPart)),iif(LEN(ALLTRIM(CodPart))>6,VAL_PARA1(RIGHT(CodPart,2),'SUBESP'+substr(codpart,5,2),'D'),val_para(SUBSTR(codpart,5,2),'ESPGAS','D')),' '):H='Descripci¢n':30

Else
	ACTIVATE WINDOW wind_2
	CLEAR
	@ 4,25 say 'No hay asignaci¢n de calendario'
endif
*HIDE WINDOW wind_2
SELE presu
RETURN

PROCEDURE vista_det
*------------------
SELECT calen
SET ORDER TO calen2
HIDE POPUP ALL
vtempo = '°°°°°°[F2]->Ord. x Mes°°°°°°°°°[F3]->Ord x Part °°°°°°°°°°°°[ESC]Terminar°°°°°°'
ON KEY LABEL F9
ON KEY LABEL F2 Do Ord_mes
ON KEY LABEL F3 Do Ord_par
GO TOP
SEEK m.periodo+m.UNIGES+m.UNIEJE+ALLT(m.codcad)+ALLTRIM(m.codfte)

IF FOUND()
	BROWSE ;
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.UNIGES+m.UNIEJE+ALLT(m.codcad)+ALLTRIM(m.codfte) ;
	WINDOW wind_2a TITLE vtempo ;
	FIELDS;
	nummes  : H='Mes',;
	codpart : H='Partida',;
    xx=iif(!empty(ITEPAR.codpart),IIF(!EMPTY(ItePar.SubEsp),VAL_PARA1(ItePar.SubEsp,'SUBESP'+ItePar.EspGas,'D'),val_para(ITEPAR.EspGas,'ESPGAS','D',28,50)),' '):H='Descripci¢n':30,;
	YY=IIF(NUMMES<'04',itepar.TRI_01,IIF(NUMMES<'07',itepar.TRI_02,IIF(NUMMES<'10',itepar.TRI_03,itepar.TRI_04))): H= 'Asig.Trim.' :p='99,999,999':R,; 
	valpart         : H='Cal.Mes' :p='99,999,999',;
	ampliar         : H='Ampliar' :p='99,999,999',;
	SALDO  = (valpart+ampliar): H= 'Tot. Cal.' :p='99,999,999':f,;
	saldo1 = val_saldo(codpart,NUMMES): H= 'Util. Trim.' :p='99,999,999':f
Else
	ACTIVATE WINDOW wind_2A
	CLEAR
	@ 12,25 say 'No hay asignaci¢n de calendario'
endif
*HIDE WINDOW wind_2a
SET ORDER TO CALEN2
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F2
ON KEY LABEL F3 
SHOW MENU mmenu
SELE presu
DO VISTA
RETURN

FUNCTION Ord_mes
*----------------
V=RECNO()
SET ORDER TO CALEN1
GO TOP
GO V
RETUR .T.

FUNCTION Ord_PAR
*----------------
V=RECNO()
SET ORDER TO CALEN2
GO TOP
GO V
RETUR .T.

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE presu
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f10 KEYBOARD CHR(23)
SET RELATION TO periodo+uniges+unieje+codCAD+codfte INTO calen
SET SKIP TO calen
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	presu.periodo   :H='Per',;
	presu.codcad    :H='CodCad',;
	presu.codfte    :H='Fte',;
	calen.nummes	:H= 'Mes',;	
	calen.codpart	:H= 'Partida',;	
	xx=iif(!empty(CALEN.codpart),IIF(LEN(ALLTRIM(Calen.CodPart))>6,VAL_PARA1(RIGHT(Calen.CodPart,2),'SUBESP'+substr(calen.codpart,5,2),'D'),val_para(SUBSTR(CALEN.codpart,5,2),'ESPGAS','D',28,50)),' '):H='Descripci¢n':30,;
	calen.valpart  : H= 'Asignaci¢n' :p='99,999,999'

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SET RELA TO
SELE presu
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vperiodo = RIGHT(DTOC(DATE()),2)
vcodfte  = '  '
vcodcad  = '    '
vuniges  = '  '
vunieje  = '   '
DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA

@  1,2 SAY ' Periodo : ' GET vperiodo   PICTURE '!!'
@  2,2 SAY 'UNI.GES. : ' GET vUniGes	PICTURE '!!'	VALID VAL_PARA(Vuniges,'UNIGES',' ',14,30)
@  3,2 SAY 'UNI.EJE. : ' GET vUniEje 	PICTURE '!!!'	VALID VAL_PARA1(vunieje,'UNIEJE'+vuniges,' ',14,30)
@  4,2 SAY '  Cadena : ' GET vcodcad    PICTURE '!!!!'  VALID val_codcad(vcodcad,vperiodo+ALLT(vuniges)+ALLT(vunieje),' ',14,30)
@  5,2 SAY '  Fuente : ' GET vcodfte    PICTURE '!!'    VALID val_para(vcodfte,'CODFTE',' ',14,30)

READ VALID val_read()

DEACTIVATE WINDOW lisTA

IF EMPTY(vperiodo) .OR. LASTKEY()=27
	RETURN
ELSE
	SET ORDER TO 1
	SEEK vperiodo+ALLTRIM(vuniges)+ALLTRIM(vunieje)+ALLTRIM(vcodcad)+ALLTRIM(vcodfte)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ELSE
		DO vista
	ENDIF
ENDIF
RETURN

PROCEDURE anter
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH vmens05
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE proxi
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH vmens06
	GO BOTTOM
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE corri
*--------------
PRIVATE VCK
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF estado = '99'
	* Anulada
	DO standby WITH vmens09
	RETURN
ENDIF
IF estado = '70'
	* El Calendario ha sido devuelto
	DO standby WITH vmens11
	RETURN
ENDIF
IF estado = '50'
	DO standby WITH vmens12
	RETURN
ENDIF
*****
SELECT presu
VCK = RECNO()
SCATTER MEMVAR
DO pantalla
=val_cODCAD(ALLT(m.codcad),m.periodo,'C')
@  0,22 GET m.periodo   PICTURE '!!'  	DISABLE
@  0,60 GET m.CODCAD	PICTURE '!!!!'  DISABLE
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA1(maepre.unieje,'UNIEJE'+maepre.uniges,'V',22,40)
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA1(maepre.CodPrg,'CODPRG'+maepre.CodFun,'V',22,40)
@  5,22 SAY VAL_PARA1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,40)
@  6,22 SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
@  7,22 SAY VAL_PARA(maepre.codcom,'CODCOM','V',22,40)
@  8,22 SAY maepre.codmet
@  8,60 SAY maepre.codfin
@  9,22 SAY SUBSTR(maepre.Descri,1,23)
@  9,60 SAY VAL_PARA(m.codfte,'CODFTE','V',60,18,2)

*----
IF LASTKEY() # 27
	ok=trabaja_hijo()
	IF ok .AND. LASTKEY() # 27
		SELECT presu
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT presu
DO vista
RETURN

PROCEDURE trabaja_hijo
*---------------------
vsun=.T.
PUBLIC ak,vcanant
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°F4->Corrige°°°F5->Agregar°°°F6->Ampliaci¢n°°° F8->Elimina °°° F10->Terminar°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f4  DO corri_item
ON KEY LABEL f5  DO agreg_item
ON KEY LABEL f6  DO ampli_item
ON KEY LABEL f8  DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELECT calen
SET ORDER TO calen2
SEEK m.periodo+m.uniges+m.unieje+ALLTRIM(m.codcad)+ALLTRIM(m.codfte)
IF !FOUND()
	DO agreg_item
ENDIF

BROWSE ;
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.uniges+m.unieje+ALLTRIM(m.codcad)+ALLTRIM(m.codfte) ;
	WINDOW wind_2 ;
	FIELDS;
	nummes  : H = 'Mes':R,;
	codpart	: H = 'Partida':R,;
	xx=IIF(!EMPTY(ALLTRIM(CodPart)),iif(LEN(ALLTRIM(CodPart))>6,VAL_PARA1(RIGHT(CodPart,2),'SUBESP'+substr(codpart,5,2),'D'),val_para(SUBSTR(codpart,5,2),'ESPGAS','D')),' '):H='Descripci¢n':30,;
	YY=IIF(NUMMES<'04',itepar.TRI_01,IIF(NUMMES<'07',itepar.TRI_02,IIF(NUMMES<'10',itepar.TRI_03,itepar.TRI_04))): H= 'Asig.Trim.' :p='99,999,999':R,; 
	valpart     : H='Cal.Mes' :p='99,999,999':R,;
	ampliar     : H='Ampliar' :p='99,999,999':R,;
	SALDO  = (valpart+ampliar): H= 'Tot. Cal.' :p='99,999,999':f,;
	saldo1 = val_saldo(codpart,NUMMES): H= 'Util. Trim.' :p='99,999,999':f

*	xx=iif(!empty(        CODPART),val_para(RIGHT(CODPART,2),'ESPGAS','D',28,50),' '):H='Descripci¢n':10:R,;
	

IF LASTKEY()=27
	vsun=.F.
ENDIF
SELECT calen
SEEK m.periodo+m.uniges+m.unieje+ALLTRIM(m.codcad)+ALLTRIM(m.codfte)
SCAN WHILE m.periodo = periodo AND m.uniges+m.unieje+ALLTRIM(m.codcad) = uniges+unieje+codcad AND ALLTRIM(m.codfte)=codfte
	IF EMPTY(codcad) OR EMPTY(codfte) OR empty(codpart) OR len(allt(codpart))<6 OR valpart+ampliar=0
   	  iF RLOCK()
		DELETE NEXT 1
	  ENDIF
	  UNLOCK
   else
   	  REPLACE ESTFUN WITH m.estfun
   endif
ENDSCAN

m.estado = '00'

UNLOCK ALL
SET ORDER TO calen2
ON KEY LABEL f4
ON KEY LABEL f5
ON KEY LABEL f6
ON KEY LABEL f10

ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SELECT presu
RETURN vsun


PROCEDURE corri_item
*-------------------
SELECT calen
DEFINE WINDOW lis FROM 10,12 TO 15,68 DOUBLE ;
	TITLE ' °° Corrige el Calendario °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis
vcodpart = calen.codpart
vmes 	 = calen.nummes
vcanant  = calen.valpart
vmonto   = calen.valpart

@  1,2 SAY '       Mes : ' GET vmes		PICTURE '!!'  	  VALID VAL_PARA(vMES,'FECMES',' ',16,30)
@  2,2 SAY '   Partida : ' GET vcodpart	PICTURE '!!!!!!'  VALID VAL_PARt(vcodpart,m.periodo+m.uniges+m.unieje+allt(codcad)+allt(codfte),' ',16,30)
READ VALID val_Lee()

SEEK m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)+ALLT(VMES)
IF !FOUND()
	DO standby WITH 'No ha sido ingresado...'
	return .f.
ENDIF
VRECNO = RECNO()
@  3,2 SAY 'Calendario : ' GET vmonto	PICTURE '99,999,999'
READ
RELEASE WINDOWS lis
REPLACE periodo WITH m.periodo ,;
		UNIGES  WITH m.uniges,;
		UNIeje  WITH m.unieje,;
		nummes  WITH vmes,;
		codCAD  WITH ALLTRIM(m.codCAD) ,;
		codfte  WITH ALLTRIM(m.codfte) ,;
		codpart WITH vcodpart ,;
		valpart WITH vmonto
		
	GO TOP
	SEEK m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)
	vacu=0
	SCAN WHILE PERIODO+uniges+unieje+CODCAD+CODFTE+CODPART=m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)
		REPLACE itepar.TOTCAL WITH vacu+CALEN.VALPART+CALEN.AMPLIAR
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
	*SET FILTER TO	
	GO VRECNO
	ok2 = CHEmon(vcodpart,vmonto+CALEN.AMPLIAR)
	RETURN .T.
* ENDIF
* RETURN .F.



PROCEDURE ampli_item
*-----------------
SELECT calen
DEFINE WINDOW lis FROM 10,12 TO 15,68 DOUBLE ;
	TITLE ' °° Ampliaci¢n de Calendario °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis
VRECNO = RECNO()
vcodpart = calen.codpart
vmes 	 = calen.nummes
vcanant  = calen.valpart
vmonto   = calen.AMPLIAR
@  1,2 SAY '       Mes : ' GET vmes		PICTURE '!!'  	  VALID VAL_PARA(vMES,'FECMES',' ',16,30)
@  2,2 SAY '   Partida : ' GET vcodpart	PICTURE '!!!!!!'  VALID VAL_PARt(vcodpart,m.periodo+m.uniges+m.unieje+allt(codcad)+allt(codfte),' ',16,30)
READ VALID val_Lee()
SEEK m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)+ALLT(VMES)
IF !FOUND()
	DO standby WITH 'No ha sido ingresado...'
	return .f.
ENDIF
VRECNO = RECNO()
@  3,2 SAY 'Ampliaci¢n : ' GET vmonto	PICTURE '99,999,999'
READ
RELEASE WINDOWS lis
*IF ok2 
	REPLACE periodo WITH m.periodo ,;
			UNIGES WITH m.uniges,;
			UNIeje WITH m.unieje,;
		codCAD  WITH ALLTRIM(m.codCAD) ,;
		codfte  WITH ALLTRIM(m.codfte) ,;
		codpart WITH vcodpart ,;
		AMPLIAR WITH vmonto
		
	GO TOP
	SEEK m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)
	vacu=0
	SCAN WHILE PERIODO+uniges+unieje+CODCAD+CODFTE+CODPART=m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)
		REPLACE itepar.TOTCAL WITH vacu+CALEN.VALPART+CALEN.AMPLIAR
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
	*SET FILTER TO	
	GO VRECNO
	ok2 = CHEmon(vcodpart,vmonto+CALEN.VALPART)
	RETURN .T.
*ENDIF
*RETURN .F.

PROCEDURE agreg_item
*-----------------
SELECT calen
DEFINE WINDOW lis FROM 10,12 TO 15,68 DOUBLE ;
	TITLE ' °° Ingreso de Calendario °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis
vmes 	 = space(2)
vcodpart = space(6)
vmonto   = 0 
@  1,2 SAY '       Mes : ' GET vmes		PICTURE '!!'  	  VALID VAL_PARA(vMES,'FECMES',' ',16,30)
@  2,2 SAY '   Partida : ' GET vcodpart	PICTURE '!!!!!!'  VALID VAL_PARt(vcodpart,m.periodo+m.UNIGES+m.UNIEJE+m.codcad+allt(m.codfte),' ',16,25)
@  3,2 SAY 'Calendario : ' GET vmonto	PICTURE '99,999,999'
READ VALID val_Lee()
RELEASE WINDOWS lis
ok1 = CHEQUEO(allt(vcodpart)+allt(vmes))
if ok1
	IF f_appd() && AND ok2
		REPLACE periodo WITH m.periodo ,;
			nummes  WITH ALLT(vmes),;
			UNIGES WITH m.uniges,;
			UNIeje WITH m.unieje,;
			codCAD  WITH ALLTRIM(m.codCAD) ,;
			codfte  WITH ALLTRIM(m.codfte) ,;
			codpart WITH vcodpart ,;
			valpart WITH vmonto
		VRECNO = RECNO()	
		SEEK m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)
		vacu=0
		
		SCAN WHILE PERIODO+uniges+unieje+CODCAD+CODFTE+CODPART=m.periodo+m.uniges+m.unieje+m.codcad+ALLT(m.codfte)+allt(vcodpart)
			REPLACE itepar.TOTCAL WITH vacu+CALEN.VALPART+calen.ampliar
			IF vMES<'04'
				REPLACE CALEN.VALPRE WITH itepar.TOTCAL
			ELSE
				IF vMES<'07'
					REPLACE CALEN.VALPRE WITH itepar.TOTCAL-itepar.TRI_01
				ELSE
					IF vMES<'10'
						REPLACE CALEN.VALPRE WITH itepar.TOTCAL-(itepar.TRI_01+itepar.TRI_02)
					ELSE
						REPLACE CALEN.VALPRE WITH itepar.TOTCAL-(itepar.TRI_01+itepar.TRI_02+itepar.TRI_03)
					ENDIF
				ENDIF
			ENDIF	
			vacu = vacu+CALEN.VALPART
		ENDSCAN
		GO VRECNO
		RETURN
	ENDIF
	ok2 = CHEmon(vcodpart,vmonto)
ENDIF	
RETURN .F.

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PRIVATE VCK
SELECT presu
VCK = RECNO()
DO pantalla
SCATTER MEMVAR
DO pantalla
=val_cODCAD(ALLT(m.codcad),m.periodo+m.uniges+m.unieje,'C')
@  0,22 GET m.periodo   PICTURE '!!'  	DISABLE
@  0,60 GET m.CODCAD	PICTURE '!!!!'  DISABLE
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA1(maepre.unieje,'UNIEJE'+maepre.uniges,'V',22,40)
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA1(maepre.CodPrg,'CODPRG'+maepre.codfun,'V',22,40)
@  5,22 SAY VAL_PARA1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,40)
@  6,22 SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
@  7,22 SAY VAL_PARA(maepre.codcom,'CODCOM','V',22,40)
@  8,22 SAY maepre.codmet
@  8,60 SAY maepre.codfin
@  9,22 SAY SUBSTR(maepre.Descri,1,23)
@  9,60 SAY VAL_PARA(m.codfte,'CODFTE','V',60,18,2)

IF LASTKEY() # 27
	m.estfun = maepre.uniges+maepre.unieje+maepre.codfun+maepre.codprg+maepre.codspr+maepre.actpry+maepre.codcom+maepre.codmet+maepre.codfin
	ok=trabaja_hijo()
	IF ok .AND. LASTKEY() # 27
		SELECT presu
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF
UNLOCK ALL
SELECT presu
DO vista
RETURN


PROCEDURE anula
*---------------
private vtemp
SELECT calen
VTEMP =RECNO()
as =order()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF estado = '20'
   do standby with 'Ya est  anulado..'
   return
endif   
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF

IF yesno('¨ Desea anular ‚ste Calendario ?')
   vcanant = valpart
   m.valpart = 0
   sele itepar
   IF  (VALPART+CRESUP+TRA001+tra003+tra004+tra005 - ITEPAR.TOTCAL - (VALPART- vcanant)) < 0 
		DO standby WITH 'No existe marco presupuestal para asignar.... Se exede en '+str(VALPART+CRESUP+TRA001+tra003+tra004+tra005 - ITEPAR.TOTCAL - M.VALPART + vcanant,8)
		SELECT calen
		set order to (as)
		GO vtemp
		DO vista
		RETURN
    ENDIF
	SELECT calen
    GO vtemp			
	m.Valpre = m.valpre + vcanant
	m.estado = '20'
	GATHER MEMVAR
	DO vista
ENDIF
UNLOCK ALL
RETURN

PROCEDURE elimi_item
*-------------------
IF RLOCK()
	DELETE NEXT 1
	SKIP 1
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
UNLOCK
RETURN

PROCEDURE lista
*--------------
USE repocal  IN 18                   &&  ALIAS REPO   
USE repcal1  IN 19                   &&  ALIAS rep1   

SELE 18
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 18 alias REPO EXCLUSIVE

SELE 19
vInd = SYS(3) + '.DBF'
COPY STRU TO (vInd)
use (vind) in 19 alias REP1 EXCLUSIVE

SELECT calen
vRECNO = RECNO()
vOrder = ORDER()
store 0 to vTotal,vtipo
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUnieje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 

DEFINE WINDOW lis_1 FROM 05,26 TO 15,55 DOUBLE ;
	TITLE ' Opciones ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
opcion=0
set message to 23
@  1,4 PROMPT  '  Marco Calendario  ' messa padc('Imprime el Marco del Calendario',79,' ')
@  3,4 PROMPT  'Saldos de Calendario' messa padc('Lista el  de Calendario por partida especifica',79,' ')
@  5,4 PROMPT  '   Marco Ejecuci¢n  ' messa padc('Lista el Marco de Ejecuci¢n ',79,' ')
@  7,4 PROMPT  '   Ampliaciones     ' messa padc('Lista de Ampliaciones',79,' ')

MENU TO opcion
RELEASE WINDOW lis_1
IF LASTKEY()= 27
	DO vista
	RETURN
ENDIF
DO CASE 
	CASE OPCION=1
		*------Listado Marco Presupuestal----
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °° Calendario Marco Presupuestal °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1

		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)

		@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1

		@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
		@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
		@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
		@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
		@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
		@ 13,2 SAY '   Al mes de : ' GET vcalend    PICTURE '!!'  		VALID val_para(vcalend,'FECMES',' ',18,30)
		@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
	    ACTIVATE WINDOW Standby
		@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
		SELECT rep1
		ZAP
		vind = SYS(3) + '.IDX'                                                                   
		Zind = SYS(3) + '.IDX'                                                                   
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (Vind)
		
		IF vtotal = 1
	    	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODCAD,itepar.cresup,itepar.tra001 ,;
		    	itepar.tra003,itepar.tra004,itepar.tra005,ITEPAR.Codfte,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.UBICAC,ITEPAR.ESTFUN ;
				FROM ITEPAR ;
				WHERE periodo=ALLTRIM(Vperiodo) AND	IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) ;
				INTO CURSOR PRESUX

		ELSE
		
			SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODCAD,itepar.cresup,itepar.tra001 ,;
		    	itepar.tra003,itepar.tra004,itepar.tra005,ITEPAR.Codfte,ITEPAR.TOTAFE,ITEPAR.TOTCAL,ITEPAR.UBICAC,ITEPAR.ESTFUN ;
				FROM ITEPAR ;
				WHERE periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.) ;
				INTO CURSOR PRESUX		
		ENDIF	
		GO TOP
		vInd = SYS(3) + '.DBF'
		COPY TO (vInd)
		use (vind) in 17 alias PRESU1 EXCLU

		SELE PRESU1
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (Zind)
		
		IF vtotal = 1

        	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    		CALEN.Codfte, CALEN.TOTAFE, CALEN.ampliar  ,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			  	FROM CALEN ;
			  	WHERE periodo=ALLTRIM(Vperiodo) AND	IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND NUMMES <= ALLTRIM(vcalend) ;
			  	INTO CURSOR CalenX
		ELSE
		    SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    		CALEN.Codfte, CALEN.TOTAFE , CALEN.ampliar,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			  	FROM CALEN ;
			  	WHERE periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.) ;
					and NUMMES <= ALLTRIM(vcalend) ;
				INTO CURSOR CalenX		
		ENDIF	  	
	
		SELE PRESU1
		GO TOP

		SCAN
			SCATTER MEMVAR		
			m.valpres = m.valpart
			m.cresup  = getcre()
			m.transf  = gettra()
     		SELECT rep1
			IF F_APPD()
               GATHER MEMVAR
			ENDIF
			SELE PRESU1
	    ENDSCAN
		SELE CALENX
		GO TOP	
		SCAN
			SCATTER MEMVAR
     		vrep2 = LEFT(M.ESTFUN,5)+m.codcad+m.codfte+m.codpart
		    STORE 0 TO 	m.C_01,m.C_02,m.C_03,m.C_04,m.C_05,m.C_06,m.C_07,m.C_08,m.C_09,m.C_10,m.C_11,m.C_12
			SELECT rep1
			SEEK ALLTRIM(vrep2)
			IF !FOUND()
				vcod='C_'+ALLTRIM(m.nummes)
				m.&vcod=m.valpart + m.Ampliar
				APPEND BLANK
				GATHER MEMVAR
				vkey = LEFT(M.ESTFUN,5)+m.codCad+m.codfte+m.Codpart
				SELE PRESU1
				GO TOP
				SEEK vkey
				* CUANDO NO SE INGRESA EL MES
								
				IF FOUND()
					Vtransf = PRESU1.TRA001 +PRESU1.TRA003 +PRESU1.TRA004+PRESU1.TRA005
					SELE rep1
					REPLACE valpres WITH PRESU1.valpart,CRESUP WITH PRESU1.CRESUP ,transf with Vtransf
				ENDIF
				m.valpart=0
			ELSE
				vcod='C_'+ALLTRIM(m.nummes)
				REPLACE &vcod WITH &vcod + CALENX.VALPART + CALENx.ampliar
				m.valpart = 0
            endif
			SELECT calenX
		ENDSCAN
		USE IN 17
		SELE rep1
	    DEACTIVATE WINDOW Standby
        GO TOP
        IF vtotal = 1
			if vtipo = 1
				DO reporte WITH 2,"MarCal1",' A nivel de Marco Presupuestal'
			ELSE
				DO reporte WITH 2,"MarCal11",' A nivel de Marco Presupuestal'
			ENDIF
		ELSE
			if vtipo = 1
				DO reporte WITH 2,"MarCaG1",' A nivel de Marco Presupuestal'
			ELSE
				DO reporte WITH 2,"MarCaG11",' A nivel de Marco Presupuestal'
			ENDIF
		ENDIF	
		CLOSE INDEX
		set filt to
		SELE 13
		SET RELA TO 
		SELE 14
		SET RELA TO 
		
		SELECT calen
		SET ORDE TO CALEN2
 
 CASE OPCION=2
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °° SALDO DE CALENDARIOS °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1

		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)

		@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1

		@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
		@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
		@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
		@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
		@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
		@ 13,2 SAY '   Al mes de : ' GET vcalend    PICTURE '!!'  		VALID val_para(vcalend,'FECMES',' ',18,30)
		@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)

		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
	    ACTIVATE WINDOW Standby
		@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
 		SELECT calen
 		GO TOP
		IF EOF()
			DO standby WITH vmens08
			RETURN
		ELSE
			IF vtotal = 1
        		SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    			CALEN.Codfte, CALEN.TOTAFE, CALEN.ampliar ,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			  		FROM CALEN ;
			  		WHERE periodo=ALLTRIM(Vperiodo) AND	IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
						IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND NUMMES = ALLTRIM(vcalend) ;
			  		INTO CURSOR CalenX
			  		
			ELSE
		    	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,CALEN.CODCAD,;
	    			CALEN.Codfte, CALEN.TOTAFE , CALEN.ampliar,CALEN.TOTOC ,CALEN.TOTOS, CALEN.ESTFUN ;
			  		FROM CALEN ;
			  		WHERE periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.) ;
						and NUMMES = ALLTRIM(vcalend) ;
					INTO CURSOR CalenX		
			ENDIF
		ENDIF	
		
		vInd = SYS(3) + '.DBF'
		COPY TO (vInd)
		use (vind) in 0 alias CALEN1 EXCLU
		ZInd = SYS(3) + '.IDX'
	    SELECT CALEN1
	    INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)
	    
		** HOJAS DE AFECTACION
		SELECT IteHc
		SET FILTER TO NUMMES=ALLT(VCALEND) and IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
		GO TOP
		SCAN
			sele maepre
			seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
			vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE+ITEHC.CODPART
			vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE
			SELE CALEN1
			SEEK vkey
			IF FOUND()
				REPLACE TOTAFE WITH TOTAFE+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
			ELSE
				GO TOP	
				LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
				IF FOUND()
					append blank
					REPLACE CODPART WITH ITEHC.CODPART,;
							PERIODO WITH VPERIODO,;
							CODCAD  WITH ITEHC.CODCAD,;
							CODFTE  WITH ITEHC.CODFTE,;
							ESTFUN  WITH LEFT(VKEY,30),;
							TOTAFE  WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
				ENDIF			
			ENDIF
			SELECT ITEHC
		ENDSCAN		
		IF vSistema = '01'
*		IF SISTEM = '1'
			** HOJAS DE AFECTACION EMERGENCIA
			SELECT 8
			USE 
			USE \EMER97\DATA\IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehce
			SELE 3
			USE
			USE \EMER97\DATA\maepre   IN 3  ORDER TAG maepre1  ALIAS maepree
	
			SELECT IteHce
			SET FILTER TO NUMMES=ALLT(VCALEND) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.);
				AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
			GO TOP
			SCAN
				sele maepree
				seek Vperiodo+IteHce.uniges+IteHce.unieje+IteHce.codcad
				vkey = uniges+unieje+codfun+codprg+codspr+actpry+IteHce.codcom+'00001'+IteHce.CODFTE+IteHce.CODPART
				vkey1= uniges+unieje+codfun+codprg+codspr+actpry+IteHce.codcom+'00001'+IteHce.CODFTE
				SELE CALEN1
				SEEK vkey
				IF FOUND()
					REPLACE TOTAFE WITH TOTAFE+IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
				ELSE
					GO TOP	
					LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
					IF FOUND()
						append blank
						REPLACE CODPART WITH IteHce.CODPART,;
							PERIODO WITH VPERIODO,;
							CODCAD  WITH IteHce.CODCAD,;
							CODFTE  WITH IteHce.CODFTE,;
							ESTFUN  WITH LEFT(VKEY,30),;
							TOTAFE  WITH IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
					ENDIF			
				ENDIF
				SELECT IteHce
			ENDSCAN		
			SELECT 8
			USE 
			USE IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehc
			SELE 3
			USE
			USE maepre   IN 3  ORDER TAG maepre1  ALIAS maepre
		ENDIF	
		SELE CALEN1
		GO TOP
		* RESTRUCTURACION DE CADENAS
		SCAN
			VKEY = PERIODO+LEFT(ESTFUN,30)+CODFTE
			SELE ITEPAR
			SET ORDER TO ITEPAR4
			SEEK VKEY
			IF FOUND()
				REPLACE CALEN1.CODCAD WITH ITEPAR.CODCAD
			ENDIF
			SELE CALEN1
		ENDSCAN
		GO TOP
	    DEACTIVATE WINDOW Standby
        IF vtotal = 1
			if vtipo = 1
				DO reporte WITH 2,"SalCal1",' A nivel de Marco Presupuestal'
			ELSE
				DO reporte WITH 2,"SalCal11",' A nivel de Marco Presupuestal'
			ENDIF
		ELSE
			if vtipo = 1
				DO reporte WITH 2,"SalCaG1",' A nivel de Marco Presupuestal'
			ELSE
				DO reporte WITH 2,"SalCaG11",' A nivel de Marco Presupuestal'
			ENDIF
		ENDIF
		USE 
		ERASE (vind)
      	SELECT CALEN
      	SET ORDE TO CALEN2
 CASE OPCION=3
	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Marco de Ejecuci¢n °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
    @  1,2 say '   Al Mes de : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
	@  3,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  4,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
	@  5,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
	@  6,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
	@  7,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+ALLT(vuniges)+ALLT(vunieje),' ',18,30),.T.) when vTotal=1

	@  9,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
	@ 10,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
	@ 11,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
	@ 12,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
	@ 13,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
	@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
	READ VALID val_read()

	DEACTIVATE WINDOW lis_1

	IF LASTKEY()= 27
		RETURN
	ENDIF
	SELE itepar
	IF EOF()
		DO standby WITH vmens08
	ELSE
	   	DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
		ACTIVATE WINDOW Xwait
   		@ 0,10 SAY " Espere un Momento...Procesando Marco de Ejecuci¢n !" COLOR W+/BR*

		*- Abriendo Archivos
		SELE REP1
		zap
		vind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
		SET INDEX TO (vind)
		IF vtotal = 1
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
		ELSE
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) and IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.)
		ENDIF	
		GO TOP
		SCAN
			SCATTER MEMVAR
			m.cresup  = getcre()
			m.transf  = gettra()
			SELECT REP1
			SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
			*m.transf  = m.tra001+m.tra003+m.tra004+m.tra005
			STORE 0 TO m.M_01,m.M_02,m.M_03,m.M_04,m.M_05,m.M_06
			STORE 0 TO m.M_07,m.M_08,m.M_09,m.M_10,m.M_11,m.M_12
			IF !FOUND()
				m.ValPres = m.valpart
				APPEND BLANK
				GATHER MEMVAR
			ELSE
				IF RLOCK()
					REPLACE ValPres WITH m.valpart
					REPLACE cresup  WITH m.cresup
					REPLACE transf  WITH m.transf
				ENDIF
				UNLOCK
			ENDIF
			SELECT ItePar
		ENDSCAN
		SELE REP1
		zind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)

		** HOJAS DE AFECTACION
		SELECT IteHc
		SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
			IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99' AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
		GO TOP
		SCAN
			sele maepre
			seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
			vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE+ITEHC.CODPART
			vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE
			vmes = 'M_'+ALLTRIM(itehc.nummes)
			SELE REP1
			SEEK vkey
			IF FOUND()
				REPLACE &vmes WITH &vmes+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
			ELSE
				GO TOP	
				LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
				IF FOUND()
					append blank
					REPLACE CODPART WITH ITEHC.CODPART,;
							PERIODO WITH VPERIODO,;
							CODCAD  WITH ITEHC.CODCAD,;
							CODFTE  WITH ITEHC.CODFTE,;
							ESTFUN  WITH LEFT(VKEY,30),;
							&vmes   WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
				ENDIF			
			ENDIF
			SELECT ITEHC
		ENDSCAN	
		IF SISTEM = '1'
			** HOJAS DE AFECTACION  emergencia
			SELECT 8
			USE 
			USE \EMER97\DATA\IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehce
			SELE 3
			USE
			USE \EMER97\DATA\maepre   IN 3  ORDER TAG maepre1  ALIAS maepree
			SELECT IteHce
			SET FILTER TO NUMMES<=allt(vcalend) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) and estado # '99' ;
				AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
			GO TOP
			SCAN
				sele maepree
				seek Vperiodo+IteHce.uniges+IteHce.unieje+IteHce.codcad
				vkey = uniges+unieje+codfun+codprg+codspr+actpry+IteHce.codcom+'00001'+IteHce.CODFTE+IteHce.CODPART
				vkey1= uniges+unieje+codfun+codprg+codspr+actpry+IteHce.codcom+'00001'+IteHce.CODFTE
				vmes = 'M_'+ALLTRIM(IteHce.nummes)
				SELE REP1
				SEEK vkey
				IF FOUND()
					REPLACE &vmes WITH &vmes+IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
				ELSE
					GO TOP	
					LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
					IF FOUND()
						append blank
						REPLACE CODPART WITH IteHce.CODPART,;
							PERIODO WITH VPERIODO,;
							CODCAD  WITH IteHce.CODCAD,;
							CODFTE  WITH IteHce.CODFTE,;
							ESTFUN  WITH LEFT(VKEY,30),;
							&vmes   WITH IIF(IteHce.TIPOPE='-',IteHce.VALPART*-1,IteHce.VALPART)
					ENDIF			
				ENDIF
				SELECT IteHce
			ENDSCAN		
			SELECT 8
			USE 
			USE IteHc    IN 8   ORDER TAG Itehc1   ALIAS Itehc
			SELE 3
			USE
			USE maepre   IN 3  ORDER TAG maepre1  ALIAS maepre
		ENDIF	
		SELE ITEPAR
		SET ORDER TO ITEPAR4
		SELE REP1
		GO TOP	
		* RESTRUCTURACION DE CADENAS
		SCAN
			VKEY = PERIODO+LEFT(ESTFUN,30)+CODFTE
			SELE ITEPAR
			SEEK VKEY
			IF FOUND()
				REPLACE REP1.CODCAD WITH ITEPAR.CODCAD
			ENDIF
			SELE REP1
		ENDSCAN		
		SELE REP1
		vind = SYS(3) + '.IDX'
		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
		SET INDEX TO (vind)
	   	RELEASE WINDOW Xwait
		IF EOF()
			DO standby WITH 'No existe Registros para procesar'
		ELSE
			IF vtotal = 1
				if vtipo = 1
					DO reporte WITH 2,"SalPrem1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ELSE
					DO reporte WITH 2,"SalPrem2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ENDIF
			ELSE
				if vtipo = 1
					DO reporte WITH 2,"SalPrmG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ELSE
					DO reporte WITH 2,"SalPrmG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
				ENDIF	
			ENDIF	
		ENDIF
	ENDIF
	
 CASE OPCION=4
	DEFINE WINDOW lis_1 FROM 6,10 TO 18,70 DOUBLE ;
		TITLE ' °°  Marco de Ampliaci¢n °° ' FLOAT COLOR SCHEME 5
	ACTIVATE WINDOW lis_1
	@  1,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
    @  3,2 say '   Al Mes de : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
	@  5,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    VALID val_para(vUniGes,'UNIGES',' ',18,30)
	@  7,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
	@  9,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
	READ VALID val_read()

	DEACTIVATE WINDOW lis_1

	IF LASTKEY()= 27
		RETURN
	ENDIF
   	DEFINE WINDOW Xwait FROM 20,06 TO 22,78 COLOR SCHEME 05 
	ACTIVATE WINDOW Xwait
	@ 0,10 SAY " Espere un Momento...Procesando Ampliaciones del mes !" COLOR W+/BR*
	vDBF = SYS(3) + '.DBF'
	vind = SYS(3) + '.IDX'
	SELE calen
	COPY TO (vDBF) FOR NUMMES = ALLT(vCalend) AND periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfte)),codfte=allt(vcodfte),.T.) AND AMPLIAR>0
	RELEASE WINDOW Xwait
	USE (vDBF) IN 0 ALIAS AMPLIA
	SELECT AMPLIA
	IF EOF()
	   	RELEASE WINDOW Xwait
		DO standby WITH vmens08
	ELSE
		*- Abriendo Archivos
		INDEX ON CODFTE+CODCAD+CODPART TO (VIND)
		GO TOP
		DO reporte WITH 2,"Ampliar",' Ampliaciones ',1,.F.,.T.
		USE
	ENDIF
	ERASE (vDBF)
	ERASE (vind)
	SELECT CALEN
	SET FILTER TO	
   	SET ORDE TO CALEN2
ENDCASE
USE repocal  IN 18                   &&  ALIAS REPO   
USE repcal1  IN 19                   &&  ALIAS rep1   
SELE calen
SET ORDE TO CALEN2
SET RELATION TO PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART INTO itepar
SET SKIP TO itepar
GO vRECNO
DO vista
RETURN


PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_2
RELEASE    WINDOW wind_2A
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
ON KEY LABEL F9
RESTORE SCREEN FROM principal
RETURN

PROCEDURE PENDIENTE
*------------------
DO CASE
   CASE ESTADO = '00' &&aprobado
       REPLACE ESTADO WITH '10'
   CASE ESTADO = '10' &&solicitado
       REPLACE ESTADO WITH '20'
   CASE ESTADO = '20' &&no aprobado
       REPLACE ESTADO WITH '00'       
ENDCAS
DO VISTA       
RETURN


PROCEDURE SUM00
*---------------
PARAMETER vkey,VLLAVE 
vrec = RECNO()
GO TOP
SUM FTE00 TO suma FOR &vLLave = vkey 
GO vrec
RETURN suma

PROCEDURE SUM01
*---------------
PARAMETER vkey,VLLAVE 
vrec = RECNO()
GO TOP
SUM FTE01 TO suma FOR &vLLave = vkey 
GO vrec
RETURN suma

PROCEDURE SUM09
*---------------
PARAMETER vkey,VLLAVE 
vrec = RECNO()
GO TOP
SUM FTE09 TO suma FOR &vLLave = vkey 
GO vrec
RETURN suma

PROCEDURE SUMCAL
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM valpart+ampliar TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumCAL1
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM valpart+ampliar TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM valpart+ampliar TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

PROCEDURE SUMCOM
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM TOTAFE TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumCOM1
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM TOTAFE TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM TOTAFE TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

**************** TOTALES MARCO PRESUPUESTALES *****************

PROCEDURE SUMAR
*--------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
ENDCASE	
SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumMES
*-----------------
PARAMETER vCalen,PART,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE	
SUM &part TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION TOTprg
*---------------
parameter tota
vkey = codcad
vrec = RECNO()
GO TOP
SUM valpRES+iif(tota=1,0,cresup+TRANSF) TO suma FOR coDcad = vkey 
GO vrec
RETURN suma

FUNCTION salprg
*---------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+left(codpart,2)'
	
ENDCASE	
SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO sumA FOR &vFiltro = vCalen
GO vrec
RETURN sumA


FUNCTION SUMPRE
*--------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM valpRES+cresup+TRANSF TO suma FOR &vFiltro = vCalen
GO vrec
RETURN suma

FUNCTION salspr
*---------------
vkey = codcad+codfte
vrec = RECNO()
GO TOP
SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12) TO sumA FOR codcad+codfte = vkey 
GO vrec
RETURN sumA

FUNCTION chequeo
*----------------
PARAMETER vcodpart
ord = ORDER()
vc  = RECNO()
vpartida=m.periodo+m.uniges+m.unieje+m.codcad+m.codfte+vcodpart
SEEK vpartida
IF FOUND()
	DO standby WITH 'Ya est  registrada esta partida '
	GO VC
	RETURN .F.
ENDIF
RETURN .t.

FUNCTION cheMON
*----------------
PARAMETER vcodpart,vcantidad
valias = alias()
ord = ORDER()
SELECT ITEPAR
SEEK m.periodo+m.uniges+m.unieje+m.codcaD+ALLT(m.codfte)+vcodpart
vresto = (ITEPAR.TOTCAL-(calen.valpart+calen.ampliar))+vcantidad
IF (VALPART+CRESUP+TRA001+TRA003+TRA004+TRA005 - vresto) < 0 
	DO standby WITH 'No existe marco presupuestal para asignar.... Se excede en '+str(VALPART+CRESUP+TRA001+TRA003+TRA004+TRA005 - (ITEPAR.TOTCAL+ CALEN.VALPART + calen.ampliar),8)
	SELECT (valias)
	RETURN .F.
ENDIF		
SELECT (valias)
RETURN .T.

FUNCTION summef
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM valpRES+CRESUP+TRANSF TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM valpRES+CRESUP+TRANSF TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summeS1
*--------------
PARAMETER vFiltro,PART
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
IF VTIPO <= 25
	SUM &part TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM &part TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrec
RETURN suma


FUNCTION suMAR1
*--------------
PARAMETER vFiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
DO CASE
	CASE VTIPO <= 25
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro
ENDCASE
GO vrec
RETURN suma

FUNCTION salprg1
*---------------
PARAMETER vFiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
DO CASE
	CASE VTIPO <= 25
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM valpRES+cresup+TRANSF-(C_01+C_02+C_03+C_04+C_05+C_06+C_07+C_08+C_09+C_10+C_11+C_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro
ENDCASE
GO vrec
RETURN suma

FUNCTION sumpre1
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE	
SUM valpres+cresup+transf TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

function case2
*-------------

 		DEFINE WINDOW lis_1 FROM 08,13 TO 15,67 DOUBLE ;
		TITLE ' °° Listado Calendario °° ' FLOAT COLOR SCHEME 5
		
		ACTIVATE WINDOW lis_1
		@  1,2 SAY '   Periodo : ' GET vperiodo   PICTURE '!!' 	 VALID !EMPTY(vperiodo)
    	@  2,2 say '       Mes : ' GET vcalend    PICTURE '!!'	 VALID VAL_PARA(vcalend  ,'FECMES',' ',16,25)
		@  3,2 SAY 'Cad. FunC. : ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo,' ',16,25),.T.)	
		@  4,2 SAY ' Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	 VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',16,30),.T.)
		READ VALID VAL_READ()
		DEACTIVATE WINDOW lis_1

		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
 		SELECT calen
 		GO TOP
		IF EOF()
			DO standby WITH vmens08
			RETURN
		ELSE
			*SET ORDER TO calen7
			*IF vsino = 'N'
     		*   SET FILTER TO codfte=ALLTRIM(vcodfte) AND nummes=ALLTRIM(vcalend) AND periodo=ALLTRIM(periodo) AND tipfun=ALLTRIM(vtipfun)
 			*   DO reporte WITH 2,"LisCalIN",' A nivel de Proyectos de Inversi¢n '
			*else
			SELE REPO
			Yind = SYS(3) + '.IDX'
			INDEX ON periodo+nummes+codcad+codpart TO (Yind)
			SET INDEX TO (YIND)
			SELE CALEN
			SET FILTER TO nummes=ALLTRIM(vcalend) AND periodo=ALLTRIM(periodo) AND ;
			    IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) and ;
		        IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)
		           
			GO TOP
			SCAN
				SCATTER MEMVAR
				vrep2 = m.periodo+m.nummes+m.codcad+m.codpart
				SELECT REPO
				SEEK ALLTRIM(vrep2)
				IF !FOUND()
					vcod='FTE'+m.codFTE	
					APPEND BLANK
					m.&vcod=m.valpart
					GATHER MEMVAR
					m.&vcod=0
				ELSE
					vcod='FTE'+m.codFTE
					IF RLOCK()
			 			REPLACE &vcod WITH &vcod+m.valpart
					ENDIF
					unlock
				ENDIF
				SELECT calen
			ENDSCAN
			SELE REPO
			gO  TOP
			DO reporte WITH 2,"LisCalIN",'Listado de Calendarios '
 		ENDIF	

FUNCTION VAL_SALDO
*-----------------
PARAMETER vcodpart,vmes
vrecno = recno()
*go top
vtotal = 0
SEEK m.periodo+m.UNIGES+m.UNIEJE+ALLT(m.codcad)+ALLTRIM(m.codfte)+vcodpart+vmes
vtipo1 = m.periodo+m.UNIGES+m.UNIEJE+ALLT(m.codcad)+ALLTRIM(m.codfte)+vcodpart
DO CASE
	CASE vmes<"04"
		DO WHILE nummes<=vmes and periodo+UNIGES+UNIEJE+codcad+codfte+codpart = vtipo1 AND !BOF()
			vtotal = vtotal + valpart
			SKIP -1
		ENDDO		
	CASE vmes>"03" and vmes<"07"
		DO WHILE nummes>'03' AND nummes<=vmes and periodo+UNIGES+UNIEJE+codcad+codfte+codpart = vtipo1 AND !BOF()
			vtotal = vtotal + valpart
			SKIP -1
		ENDDO		
	CASE vmes>"06" and vmes<"10"
		DO WHILE nummes>'06' AND nummes<=vmes and periodo+UNIGES+UNIEJE+codcad+codfte+codpart = vtipo1 AND !BOF()
			vtotal = vtotal + valpart
			SKIP -1
		ENDDO		
	CASE vmes>"09" 
		DO WHILE nummes>'09' AND nummes<=vmes and periodo+UNIGES+UNIEJE+codcad+codfte+codpart = vtipo1 AND !BOF()
			vtotal = vtotal + valpart
			SKIP -1
		ENDDO		
ENDCASE
GO vrecno
RETURN vtotal


