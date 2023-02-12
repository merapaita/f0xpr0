*------------------------------------------------------------------
* RegPre.Prg
* Registra Presupuestos
*--------------------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae  IN 1  ORDER TAG parmae1  ALIAS parma
USE maeparI IN 2  ORDER TAG maeparI1  ALIAS presu
USE iteparI IN 3  ORDER TAG iteparI1  ALIAS itepar
USE maepre  IN 5  ORDER TAG maepre1  ALIAS maepre
USE Calen   IN 7  ORDER TAG Calen2   ALIAS Calen
USE recing  IN 8 	ORDER TAG recing1  ALIAS recing
USE Iteri   IN 9 	ORDER TAG Iteri1   ALIAS Iteri
USE traPAR  IN 13 ORDER TAG traPAR1  ALIAS TRAPAR
USE Itetra  IN 10 ORDER TAG Itetra1  ALIAS Itetra
USE CRESUP  IN 4  ORDER TAG CRESUP1  ALIAS CRESUP
USE Itecre  IN 11 ORDER TAG Itecre1  ALIAS Itecre

USE HOJMOD  IN 12 ORDER TAG hojmod1  ALIAS hojmod
USE INGRESO IN 13 ORDER TAG Ingreso1 ALIAS IngR

PUBLIC vCodSub,vCodAct,vProyec,vSubpry,vCalend,vNewIng,mdescr

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'
SELECT presu
GO TOP
ON KEY LABEL F9 DO VISTA_DET
ON KEY LABEL F7 DO asign_tri

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
vNewIng =.F.
*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
*DO busca
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 11,79  DOUBLE ;
	FOOTER '[F7] Asig. Trimestral' TITLE vmens02 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Presupuesto         ®F9¯ Detalle : Item ' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_2a FROM 01,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Presupuesto         ®F10¯ Salir ' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 09,55 TO 23,78 PANEL ;
	TITLE '° Totales °'

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
DEFINE PAD elimi   OF mmenu PROMPT '\<Eliminar'   AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
ON SELECTION PAD elimi  OF mmenu DO elimi
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

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT presu

SET FILT TO
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
=val_codcad(ALLT(M.codcad),M.periodo+m.UNIGES+M.UNIEJE,'C')
*@  0,60 SAY IIF( m.Estado= '00','Emitido  ','Observado')
@  0,22 SAY m.periodo
@  0,60 SAY m.CODCAD
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA1(maepre.unieje,'UNIEJE'+MAEPRE.UNIGES,'V',22,40)
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
SELECT itepar
SET ORDER TO ITEPARI1
SET FILT TO
HIDE POPUP ALL
GO TOP
SEEK m.periodo+m.uniges+m.unieje+m.codcad+ALLTRIM(m.codfte)
IF FOUND()
	DO CASE
		CASE LEFT(maepre.actpry,1)='1'
			BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.uniges+m.unieje+m.codCAD+ALLTRIM(m.codfte) TIMEOUT 0.0001 ;
			WINDOW wind_2 ;
			FIELDS;
        	itepar.codpart	:H= 'Partida',;
     		xx=val_INGE(itepar.CODPART,'D',28,50):H='Descripci¢n',;
        	valpart     : H= 'Asignaci¢n' :p='99,999,999',;
			MESEJE      : H= 'Mes',;
			NUMCRE      : H= 'Cr‚dito',;
			NUMTRA       :H= 'Transf.'

*aa = IIF(!EMPTY(ITEPAR.CODPART),mdescr,' ') :H='DescRipci¢n' :40 :W=!EMPTY(ITEPAR.CodPart),;
*        	itepar.codpart	:H= 'Partida':V=VAL_ING2(SUBSTR(ITEPAR.CodPart,11,2),LEFT(ITEPAR.CodPart,9),'CODPART'),;
*            aa = IIF(!EMPTY(CODPART),mdescr,' ') :H='DescRipci¢n' :40 :W=!EMPTY(CodPart),;


*			ss=VAL_SALDO():H='Saldo':p='99,999,999':F,;
*			CRESUP      : H= 'Cr‚ditos  ' :p='99,999,999',;
*			TRA001		: H= 'Tra. Pliego' :p='99,999,999',;
*			TRA003		: H= 'Cr. Pre-Anu' :p='99,999,999',;
*			TRA004		: H= 'Cr. P/A UE' :p='99,999,999',;
*			TRA005		: H= 'Cr. P/A UG' :p='99,999,999'

*			PP=VAL_PRESU():H='Marco+/-':p='99,999,999':F,;	

*			catgas		: H= 'CG',;
*           grugen		: H= 'GG',;	
*			modapl		: H= 'MA',;	
*			espgas      : H= 'EG',;

		CASE LEFT(maepre.actpry,1)='2'
			BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.periodo+m.uniges+m.unieje+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) TIMEOUT 0.0001 ;
			WINDOW wind_2 ;
			FIELDS;
        	itepar.codpart	:H= 'Partida',;
     		xx=val_INGE(itepar.CODPART,'D',28,50) :H='Descripci¢n',;
			valpart     : H= 'Asignaci¢n' :p='99,999,999',;
			MESEJE      : H= 'Mes',;
			NUMCRE      : H= 'Cr‚dito',;
			NUMTRA       :H= 'Transf.',;
			modeje      : H= 'MEj' :W=valpart#0,;
			coddep      : H= 'UniEje':v=val_para(coddep,'CODDEP','coddep'):F :W=(ALLTRIM(m.tipfun)='I') AND valpart#0,;
			metas       : H= 'Metas F¡sicas' :W=valpart#0


*			PP=VAL_PRESU():H='Marco+/-':p='99,999,999':F,;	
*			valpart     : H= 'Asignaci¢n' :p='99,999,999',;
*			ss=VAL_SALDO():H='Saldo':p='99,999,999':F,;
*			CRESUP      : H= 'Cr‚ditos  ' :p='99,999,999',;
*			TRA001		: H= 'Tra. Pliego' :p='99,999,999',;
*			TRA003		: H= 'Cr. Pre-Anu' :p='99,999,999',;
*			TRA004		: H= 'Cr. P/A UE' :p='99,999,999',;
*			TRA005		: H= 'Cr. P/A UG' :p='99,999,999',;
*			ubicac      : H= 'Ubicaci¢n' :W=itepar.valpart#0,;
*			modeje      : H= 'MEj' :W=valpart#0,;
*			coddep      : H= 'UniEje':v=val_para(coddep,'CODDEP','coddep'):F :W=(ALLTRIM(m.tipfun)='I') AND valpart#0,;
*			metas       : H= 'Metas F¡sicas' :W=valpart#0
			
			
*catgas		: H= 'CG',;			
*grugen		: H= 'GG',;	
*modapl		: H= 'MA',;	
*espgas     : H= 'EG',;
			
	ENDCASE
Else
    ACTIVATE WINDOW wind_2
    CLEA
	@ 4,25 say 'No hay asignaci¢n de presupuesto'
endif
*HIDE WINDOW wind_2

SELE presu
RETURN

PROCEDURE vista_det
*------------------
SELECT itepar
SET ORDER TO ITEPARI1
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GO TOP
SEEK m.periodo+m.uniges+m.unieje+ALLT(m.codcad)+ALLTRIM(m.codfte)
IF FOUND()
	BROWSE ;
	LOCK 1 NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.uniges+m.unieje+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) ;
	WINDOW wind_2a TITLE vtempo ;
	FIELDS;
      	itepar.codpart	:H= 'Partida',;
   		xx=val_INGE(itepar.CODPART,'D',28,50) :H='Descripci¢n',;
		valpart     : H= 'Asignaci¢n' :p='99,999,999',;
		MESEJE      : H= 'Mes',;
    	NUMCRE      : H= 'Cr‚dito',;
		NUMTRA       :H= 'Transf.'

*		xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;
*		PP=VAL_PRESU():H='Marco+/-':p='99,999,999':F,;	
*		valpart     : H= 'Asignaci¢n' :p='99,999,999',;
*		CRESUP      : H= 'Cr‚ditos  ' :p='99,999,999',;
*		TRA001		: H= 'Tra. Pliego' :p='99,999,999',;
*		TRA003		: H= 'Cr. Pre-Anu' :p='99,999,999',;
*		TRA004		: H= 'Cr. P/A UE' :p='99,999,999',;
*		TRA005		: H= 'Cr. P/A UG' :p='99,999,999',;
*		M_01		: H= 'Ene.':p='99,999,999',;
*		M_02		: H= 'Feb.':p='99,999,999',;
*		M_03		: H= 'Mar.':p='99,999,999',;
*		M_04		: H= 'Abr.':p='99,999,999',;
*		M_05		: H= 'May.':p='99,999,999',;
*		M_06		: H= 'Jun.':p='99,999,999',;
*		M_07		: H= 'Jul.':p='99,999,999',;
*		M_08		: H= 'Ago.':p='99,999,999',;
*		M_09		: H= 'Set.':p='99,999,999',;
*		M_10		: H= 'Oct.':p='99,999,999',;
*		M_11		: H= 'Nov.':p='99,999,999',;
*		M_12		: H= 'Dic.':p='99,999,999',;
*		ss=val_saldo():H='Saldo':p='99,999,999':F
		
		*xx=tra001+tra003+tra004+tra005 :h='Transf.':p='99,999,999':R,;

Else
	ACTIVATE WINDOW wind_2A
	CLEAR
	@ 12,25 say 'No hay asignaci¢n de presupuesto'
endif
*HIDE WINDOW wind_2A
ON KEY LABEL F9 DO VISTA_DET
SHOW MENU mmenu
SELE presu
DO VISTA
RETURN


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
SET RELATION TO periodo+uniges+unieje+codCAD+codfte INTO itepar
SET SKIP TO itepar
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	presu.periodo   :H='Per',;
	presu.UNIGES    :H='UNIGES',;
	presu.UNIEJE    :H='UNIEJE',;	
	presu.codcad    :H='CodCad',;
	presu.codfte    :H='Fte',;
	itepar.codpart	:H= 'Partida',;
  	itepar.codpart	:H= 'Partida',;
	xx=val_INGE(itepar.CODPART,'D',28,50) :H='Descripci¢n'
*	itepar.catgas	:H= 'CG',;	
*	itepar.grugen	:H= 'GG',;	
*	itepar.modapl	:H= 'MA',;	
*	itepar.espgas   :H= 'EG',;
*	xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SET RELA TO
SELE presu
SET ORDE TO 1
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
SELE presu
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
SELE presu
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
	* El Presupuesto ha sido devuelto
	DO standby WITH vmens11
	RETURN
ENDIF
IF estado = '50'
	DO standby WITH vmens12
	RETURN
ENDIF
*****
SELECT presu
SCATTER MEMVAR
DO pantalla
=val_cODCAD(ALLT(m.codcad),m.periodo+m.uniges+m.unieje,'C')
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

IF LASTKEY() # 27
	m.estfun = maepre.uniges+maepre.unieje+maepre.codfun+maepre.codprg+maepre.codspr+maepre.actpry+maepre.codcom+maepre.codmet+maepre.codfin
	ok=trabaja_hijo()
	IF ok AND LASTKEY()#27
		SELECT presu
		GATHER MEMVAR
	ENDIF
ELSE
	DO standby WITH 'Proceso cancelado'
ENDIF

UNLOCK ALL
SELECT presu
DO vista
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT presu
op=ORDER()
vtemp = RECNO()
DO pantalla
STORE SPACE(2) TO vcodfte,vuniges
STORE SPACE(3) TO vunieje
STORE SPACE(4) TO vcodcad
vperiodo = RIGHT(DTOC(DATE()),2)
vtemp = RECNO()
@  0,22 GET vperiodo   	PICTURE '!!'  
@  1,22 GET vUniGes		PICTURE '!!'	VALID VAL_PARA(Vuniges,'UNIGES',' ',22,40)
@  2,22 GET vUniEje 	PICTURE '!!!'	VALID VAL_PARA1(vunieje,'UNIEJE'+vuniges,' ',22,40)
@  0,60 GET vCodCad		PICTURE '!!!!'	VALID VAL_codcad(vcodcad,vperiodo+ALLT(vuniges)+ALLT(vunieje),'C',60)
READ VALID val_read()
IF LASTKEY()=27
	go vtemp
	DO vista
	RETURN
ENDIF
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA1(maepre.CodPrg,'CODPRG'+maepre.CodFun,'V',22,40)
@  5,22 SAY VAL_PARA1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,40)
@  6,22 SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
@  7,22 SAY VAL_PARA(maepre.codcom,'CODCOM','V',22,40)
@  8,22 SAY maepre.codmet
@  8,60 SAY maepre.codfin
@  9,22 SAY SUBSTR(maepre.Descri,1,23)
@  9,60 GET vcodfte		PICTURE '!!'	VALID VAL_PARA(vcodFTE,'CODFTE',' ',60,18,3)
READ VALID val_read()
IF LASTKEY()=27
	DO vista
	RETURN
ENDIF
vkey = ALLTRIM(vperiodo)+maepre.uniges+maepre.unieje+ALLTRIM(vcodcad)+ALLTRIM(vcodfte)
SEEK vkey
IF !FOUND()
	SCATTER MEMVAR BLANK
ELSE
	DO standby WITH 'Ya esta Registrado el Programa'
	DO vista
	RETURN
ENDIF
IF LASTKEY() # 27
    m.estado = '00'
	m.fecemi = DATE()
	m.codfte = ALLT(vcodfte)
	m.uniges = maepre.uniges 
	m.unieje = maepre.unieje
	m.codcad = ALLT(vcodcad)
	m.periodo = vperiodo
	m.estfun = maepre.uniges+maepre.unieje+maepre.codfun+maepre.codprg+maepre.codspr+maepre.actpry+maepre.codcom+maepre.codmet+maepre.codfin
	DO WHILE .T.
		ok=trabaja_hijo()
		IF LASTKEY() # 27 AND OK
			IF yesno('¨ Confirme el ingreso ?')
				ok=.T.
				EXIT
			ENDIF
		ELSE
			IF yesno('¨ Cancela el ingreso ?')
				DO standby WITH ' Cancelado el Ingreso ..'
				ok = .F.
				EXIT
			ELSE
				LOOP
			ENDIF
		ENDIF
	ENDDO
	IF ok AND LASTKEY()#27
		SELECT presu
		IF f_appd()
			GATHER MEMVAR
		ENDIF
	ELSE
		SELE itepar
		SEEK m.periodo+ALLTRIM(m.codcad)+ALLTRIM(m.codfte)
		SCAN WHILE periodo=m.periodo AND codcad = ALLTRIM(m.codcad) AND codfte=ALLTRIM(m.codfte)
			IF RLOCK()
				DELETE NEXT 1
			ENDIF
		ENDSCAN
		GO VTEMP
	ENDIF
ENDIF

UNLOCK ALL
SELECT presu
SET ORDE TO (op)

DO vista
RETURN


PROCEDURE trabaja_hijo
*---------------------
vsun=.T.
PUBLIC ak
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f5  DO agreg_item
ON KEY LABEL f8  DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELECT itepar
SET ORDER TO iteparI1
SEEK m.periodo+m.uniges+m.unieje+ALLTRIM(m.codcad)+ALLTRIM(m.codfte)
IF !FOUND()
	DO agreg_item
ENDIF

BROWSE ;
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.uniges+m.unieje+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) ;
	WINDOW wind_2 ;
	FIELDS;
		codpart     : H= 'Partida' :v=val_parti(SUBSTR(codpart,11,2),LEFT(codpart,10),'codpart') :F ,;
		aa = IIF(!EMPTY(codpart),val_parti(SUBSTR(codpart,11,2),LEFT(codpart,10),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codpart) ,;
    	valpart     : H= 'Asignaci¢n' :p='99,999,999' :V=chequeo():F,;
		MESEJE      : H= 'Mes',;
		NUMCRE      : H= 'Cr‚dito',;
		NUMTRA       :H= 'Transf.'

*        	codpart :V= val_INGE(itepar.CODPART,'Z',28,50) :H= 'Partida',;
*   		xx = val_INGE(itepar.CODPART,'D',28,50) :H='Descripci¢n',;

*

*			CRESUP      : H= 'Cr‚ditos  ' :p='99,999,999':R,;
*			xx=tra001+tra003+tra004+tra005 :h='Transf.':p='99,999,999':R,;
*			M_01		: H= 'Ene.':p='99,999,999':V=chequeoa('M_01'):F,;
*			M_02		: H= 'Feb.':p='99,999,999':V=chequeoa('M_02'):F,;
*			M_03		: H= 'Mar.':p='99,999,999':V=chequeoa('M_03'):F,;
*			M_04		: H= 'Abr.':p='99,999,999':V=chequeoa('M_04'):F,;
*			M_05		: H= 'May.':p='99,999,999':V=chequeoa('M_05'):F,;
*			M_06		: H= 'Jun.':p='99,999,999':V=chequeoa('M_06'):F,;
*			M_07		: H= 'Jul.':p='99,999,999':V=chequeoa('M_07'):F,;
*			M_08		: H= 'Ago.':p='99,999,999':V=chequeoa('M_08'):F,;
*			M_09		: H= 'Set.':p='99,999,999':V=chequeoa('M_09'):F,;
*			M_10		: H= 'Oct.':p='99,999,999':V=chequeoa('M_10'):F,;
*			M_11		: H= 'Nov.':p='99,999,999':V=chequeoa('M_11'):F,;
*			M_12		: H= 'Dic.':p='99,999,999':V=chequeoa('M_12'):F,;
*			ss=VAL_SALDO():H='Saldo':p='99,999,999':F

*			catgas		: H= 'CG':V=VAL_PARA(catgas,'CATGAS','catgas'):F,;
*			grugen		: H= 'GG':V=VAL_PARA(grugen,'GRUGEN','grugen'):F,;
*			modapl		: H= 'MA':V=VAL_PARA(modapl,'MODAPL','modapl'):F,;
*			espgas      : H= 'EG':V=VAL_PARA(espgas,'ESPGAS','espgas'):F,;

*CASE LEFT(maepre.actpry,1)='2'
*		BROWSE ;
*			LOCK 4 NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.periodo+m.uniges+m.unieje+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) ;
*			WINDOW wind_2 ;
*			FIELDS;
 *       	itepar.codpart	:H= 'Partida',;
  *   		xx=val_INGE(itepar.CODPART,'D',28,50) :H='Descripci¢n',;
   *  		valpart     : H= 'Asignaci¢n' :p='99,999,999',;
	*		MESEJE      : H= 'Mes',;
	*		NUMCRE      : H= 'Cr‚dito',;
	*		NUMTRA       :H= 'Transf.',;
	*		modeje      : H= 'MEj' :W=valpart#0,;
	*		coddep      : H= 'UniEje':v=val_para(coddep,'CODDEP','coddep'):F :W=(ALLTRIM(m.tipfun)='I') AND valpart#0,;
	*		metas       : H= 'Metas F¡sicas' :W=valpart#0

*			grugen		: H= 'GG':V=VAL_PARA(grugen,'GRUGEN','grugen'):F,;
*			modapl		: H= 'MA':V=VAL_PARA(modapl,'MODAPL','modapl'):F,;
*			espgas      : H= 'EG':V=VAL_PARA(espgas,'ESPGAS','espgas'):F,;
*			valpart     : H= 'Asignaci¢n' :p='99,999,999' :V=chequeo():F ,;
*			CRESUP      : H= 'Cr‚ditos  ' :p='99,999,999':R,;
*			xx=tra001+tra003+tra004+tra005 :h='Transf.':p='99,999,999':R,;
*			M_01		: H= 'Ene.':p='99,999,999':V=chequeoa('M_01'):F,;
*			M_02		: H= 'Feb.':p='99,999,999':V=chequeoa('M_02'):F,;
*			M_03		: H= 'Mar.':p='99,999,999':V=chequeoa('M_03'):F,;
*			M_04		: H= 'Abr.':p='99,999,999':V=chequeoa('M_04'):F,;
*			M_05		: H= 'May.':p='99,999,999':V=chequeoa('M_05'):F,;
*			M_06		: H= 'Jun.':p='99,999,999':V=chequeoa('M_06'):F,;
*			M_07		: H= 'Jul.':p='99,999,999':V=chequeoa('M_07'):F,;
*			M_08		: H= 'Ago.':p='99,999,999':V=chequeoa('M_08'):F,;
*			M_09		: H= 'Set.':p='99,999,999':V=chequeoa('M_09'):F,;
*			M_10		: H= 'Oct.':p='99,999,999':V=chequeoa('M_10'):F,;
*			M_11		: H= 'Nov.':p='99,999,999':V=chequeoa('M_11'):F,;
*			M_12		: H= 'Dic.':p='99,999,999':V=chequeoa('M_12'):F,;
*			ss=VAL_SALDO():H='Saldo':p='99,999,999':F,;
*			ubicac      : H= 'Ubicaci¢n' :W=itepar.valpart#0
			
			*modeje      : H= 'MEj' :W=valpart#0,;
			*coddep      : H= 'UniEje':v=val_para(coddep,'CODDEP','coddep'):F :W=valpart#0,;
			*metas       : H= 'Metas F¡sicas' :W=valpart#0
			
*			catgas		: H= 'CG':V=VAL_PARA(catgas,'CATGAS','catgas'):F,;
			
*ENDCASE

IF LASTKEY()=27
	vsun=.F.
ENDIF
STORE 0 TO vm_01,vm_02,vm_03,vm_04,vm_05,vm_06,vm_07,vm_08,vm_09,vm_10,vm_11,vm_12
SELECT itepar
SEEK m.periodo+m.uniges+m.unieje+ALLTRIM(m.codcad)+ALLTRIM(m.codfte)
SCAN WHILE m.periodo = periodo AND m.uniges+m.unieje+ALLTRIM(m.codcad) = uniges+unieje+codcad AND ALLTRIM(m.codfte)=codfte
	IF EMPTY(codcad) OR EMPTY(codfte) OR empty(codpart) OR len(allt(codpart))<6 
		iF RLOCK()
			DELETE NEXT 1
		ENDIF
		UNLOCK
	ELSE
		REPLACE estfun with m.estfun
	ENDIF
	vm_01 = vm_01 + m_01
	vm_02 = vm_02 + m_02
	vm_03 = vm_03 + m_03
	vm_04 = vm_04 + m_04
	vm_05 = vm_05 + m_05
	vm_06 = vm_06 + m_06
	vm_07 = vm_07 + m_07
	vm_08 = vm_08 + m_08
	vm_09 = vm_09 + m_09
	vm_10 = vm_10 + m_10
	vm_11 = vm_11 + m_11
	vm_12 = vm_12 + m_12
ENDSCAN

m.estado = '00'

*ACTIVATE WINDOWS wind_3 

*@ 1,1 SAY 'ENE : '
*@ 2,1 SAY 'FEB : '
*@ 3,1 SAY 'MAR : '
*@ 4,1 SAY 'ABR : '
*@ 5,1 SAY 'MAY : '
*@ 6,1 SAY 'JUN : '
*@ 7,1 SAY 'JUL : '
*@ 8,1 SAY 'AGO : '
*@ 9,1 SAY 'SET : '
*@10,1 SAY 'OCT : '
*@11,1 SAY 'NOV : '
*@12,1 SAY 'DIC : '

*@ 1,8 SAY vm_01 PICTURE '99,999,999.99'
*@ 2,8 SAY vm_02 PICTURE '99,999,999.99'
*@ 3,8 SAY vm_03 PICTURE '99,999,999.99'
*@ 4,8 SAY vm_04 PICTURE '99,999,999.99'
*@ 5,8 SAY vm_05 PICTURE '99,999,999.99'
*@ 6,8 SAY vm_06 PICTURE '99,999,999.99'
*@ 7,8 SAY vm_07 PICTURE '99,999,999.99'
*@ 8,8 SAY vm_08 PICTURE '99,999,999.99'
*@ 9,8 SAY vm_09 PICTURE '99,999,999.99'
*@10,8 SAY vm_10 PICTURE '99,999,999.99'
*@11,8 SAY vm_11 PICTURE '99,999,999.99'
*@12,8 SAY vm_12 PICTURE '99,999,999.99'

DO standby with 'Presione Tecla para Continuar',19,2
*DEACTIVate WINDOWS wind_3 
UNLOCK ALL
SET FILTER TO
SET ORDER TO 1
ON KEY LABEL f5
ON KEY LABEL f8
ON KEY LABEL f10

ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SELECT presu
*RETURN vsun

PROCEDURE elimi
*---------------
SELECT presu
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Presupuesto ?')
IF velimina .AND.  f_lock(1)
	DELE NEXT 1
	SELECT itepar
	SEEK m.periodo+m.uniges+m.unieje+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte)
	IF FOUND()
		SCAN WHILE m.periodo=itepar.periodo .AND. m.uniges+m.unieje+ALLTRIM(m.codCAD)=itepar.uniges+itepar.unieje+itepar.codCAD .AND. ;
				ALLTRIM(m.codfte)=itepar.codfte
			IF f_lock(1)
				DELETE NEXT 1
			ENDIF
		ENDSCAN
	ENDIF
	SELECT presu
	IF !BOF()
		SKIP -1
	ELSE
		IF !EOF()
			SKIP
		ENDIF
	ENDIF
ENDIF
UNLOCK ALL

DO vista
RETURN

PROCEDURE chequeo
*----------------
ord = ORDER()
vc  = RECNO()
*vcodpart=catgas+grugen+modapl+espgas
vcodpart=ITEPAR.CODPART
vacpya=itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+ITEPAR.codpart
SET ORDER TO iteparI1
SEEK vacpya
IF FOUND() 	and vNewIng 
		vmodeje   = itepar.modeje
		vcoddep   = itepar.coddep
		vmeta     = itepar.metas
*		DO standby WITH 'Ya est  registrada esta partida '
		SET ORDE TO (ord)
		GO vc
		IF RLOCK()
			REPLACE		modeje   WITH vmodeje ,;
				coddep   WITH vcoddep ,;
				metas    WITH vmeta ,;
				codpart with vcodpart

				* ubicac   WITH vubicac 
		ENDIF
		UNLOCK
		vNewIng =.F.
		RETURN .T.
Else
	SET ORDE TO (ord)
	GO vc
	REPLACE ITEPAR.codpart with vcodpart
	vNewIng = .F.
endif
SET ORDE TO (ord)
GO vc
RETURN .T.

PROCEDURE agreg_item
*-----------------
SELECT itepar
IF f_appd()
	REPLACE periodo WITH m.periodo ,;
		uniges 	WITH m.uniges ,;
		unieje	WITH m.unieje ,;
		codCAD  WITH ALLTRIM(m.codCAD) ,;
		codfte  WITH ALLTRIM(m.codfte)
	vNewIng = .T.	
	RETURN .T.
ENDIF
RETURN .F.

PROCEDURE elimi_item
*-------------------
SELECT itepar
IF RLOCK()
	DELETE NEXT 1
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
UNLOCK
RETURN


PROCEDURE lista
*--------------
*------ CONSOLIDADO PRESUPUESTO ANUAL -----------
DEFINE WINDOW lis_1 FROM 06,26 TO 18,55 DOUBLE ;
	TITLE ' Opciones ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis_1
opcion=0
@  1,4 PROMPT  '  Consolidado  Anual  '  messa 'Presupuesto Autorizado Consolidado'
@  2,4 PROMPT  '      Anal¡tico       '  messa 'Presupuesto Anal¡tico'
@  3,4 PROMPT  '  Saldo Presupuestal  '  messa 'Lista Saldos Presupuestales a nivel anal¡tico '
@  4,4 PROMPT  '      Gen‚rico        '  messa 'Lista Saldos Presupuestales a nivel gen‚rico'

@  6,4 PROMPT  'Consolidado   Mensual '  messa 'Consolidado Mensualizado'
@  7,4 PROMPT  'Consolidado Trimestral'  messa 'Consolidado Trimestral'
@  8,4 PROMPT  ' Ejecuci¢n del Gasto  '  messa 'Ejecuci¢n'

MENU TO opcion
RELEASE WINDOW lis_1
IF LASTKEY()= 27
	DO vista
	RETURN
ENDIF
USE repopreI  IN 6  ALIAS REPO 
SELE REPO
vdbf = sys(3)+'.dbf'
Copy stru to (vdbf)
USE (VDBF) IN 6  ALIAS REPO EXCLU
SELE REPO
ZAP
PRIVATE VTEXP
SELECT presu
ord   = ORDER()
vteXp = RECNO()
store 0 to vTotal,vtipo
store space(2) to vperiodo,vcodfte,vcalend,VCODFUN,vUniges
store space(3) to vcodprg,vUnieje
store space(4) to vcodcad,vcodSpr
store space(5) to vcodcom,vcodmet
store space(6) to vcodpart,vactpry 
vperiodo = RIGHT(DTOC(DATE()),2)
vforma ='Partida'
DO CASE 
	CASE OPCION=1
		*------ CONSOLIDADO PRESUPUESTO ANUAL -----------
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Consolidado Presupuesto Anual  °° ' FLOAT COLOR SCHEME 5
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
		@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		 ELSE
		    ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			SELE REPO
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
				SELECT REPO
				SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
				
				vcod = 'FTE'+ALLTRIM(m.codfte)
				m.transf = m.tra001+m.tra003+m.tra004+m.tra005
				m.totcal = 0
				IF !FOUND()
					m.&vcod = m.valpart+m.cresup+m.transf
					APPEND BLANK
					GATHER MEMVAR
					m.&vcod=0
				ELSE
					IF RLOCK()
						REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
					ENDIF
					UNLOCK
					&vcod = 0
				ENDIF
				SELECT itepar
			ENDSCAN
		    DEACTIVATE WINDOW Standby
			SELE REPO
			GO TOP
			IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				IF vtotal = 1
					if vtipo = 1
						DO reporte WITH 2,"LisPreA1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ELSE
						DO reporte WITH 2,"LisPreA2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ENDIF
				ELSE
					if vtipo = 1
						DO reporte WITH 2,"LISPRAG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ELSE
						DO reporte WITH 2,"LisPrAG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ENDIF	
				ENDIF	
			ENDIF
		ENDIF
	
	CASE OPCION=2
		*------ CONSOLIDADO PRESUPUESTO ANUAL -----------
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
			TITLE ' °°   x Partidas Presupuestales     °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1
		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
		@  7,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1
		@  9,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
		@ 10,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
		@ 11,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
		@ 12,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
		@ 13,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		ELSE
		    ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			SELE REPO
			vind = SYS(3) + '.IDX'
			INDEX ON  LEFT(ESTFUN,5)+codcad+codpart TO (vind)
			SET INDEX TO (vind)
			
			IF vtotal = 1
				SELE itepar
				SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
			ELSE
				SELE itepar
				SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)
			ENDIF	
			GO TOP
			SCAN
				SCATTER MEMVAR
				SELECT REPO
				SEEK LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codpart
				vcod = 'FTE'+ALLTRIM(m.codfte)
				m.transf = m.tra001+m.tra003+m.tra004+m.tra005
				m.totcal = 0
				IF !FOUND()
					m.&vcod = m.valpart+m.cresup+m.transf
					APPEND BLANK
					GATHER MEMVAR
					m.&vcod=0
				ELSE
					IF RLOCK()
						REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
					ENDIF
					UNLOCK
					&vcod = 0
				ENDIF
				SELECT itepar
			ENDSCAN
	    	DEACTIVATE WINDOW Standby
			SELE REPO
			GO TOP
			IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				IF vtotal = 1
					if vtipo = 1
						DO reporte WITH 2,"LisPrCA1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ELSE
						DO reporte WITH 2,"LisPrCA2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ENDIF
				ELSE
					if vtipo = 1
						DO reporte WITH 2,"LISPRCG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ELSE
						DO reporte WITH 2,"LisPrCG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ENDIF	
				ENDIF	
			ENDIF
		ENDIF
	CASE OPCION=3
	SUSP
		*------ SALDO PRESUPUESTAL -----------
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
			TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1
		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
		@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
		@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!'	VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.)
		@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
		
		READ VALID val_read()
	
		DEACTIVATE WINDOW lis_1
		
		xCodFte = vCodFte
		xCodCad = vCodCad
		
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		ELSE
			ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			
			SELE REPO
			vind = SYS(3) + '.IDX'
			
			INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
			SET INDEX TO (vind)
			
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND  IIF(!EMPTY(ALLT(vCodCad)),CodCad=allt(vCodCad),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.) AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.) AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.) AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND IIF(EMPTY(ALLT(meseje)),.t.,meseje<=allt(vCalend))
			GO TOP
			
			SCAN
				SCATTER MEMVAR
				SELECT REPO
				SEEK LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
				vcod = 'FTE'+ALLTRIM(m.codfte)
				m.totcal = 0
				IF !FOUND()
				    m.&vcod = m.valparT
					APPEND BLANK
					GATHER MEMVAR
					SELE MaePre
					SEEK m.Periodo+m.UniGes+m.UniEje+m.CodCad
					cMes=''
					IF FOUND()
						cMet = MaePre.Descri
					ENDIF
					SELE Repo
					REPLACE Metas WITH cMet
					m.&vcod=0
				ELSE
					IF RLOCK()
						REPLACE ValPart WITH ItePar.Valpart+ valpart
*						REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.transf
					ENDIF
					UNLOCK
					&vcod = 0
				ENDIF
				SELECT ItePar
			ENDSCAN
			SELE REPO
			zind = SYS(3) + '.IDX'
			INDEX ON codcad+CODFTE+CODPART TO (zind)
			** recibos de ingresos
			IF !USED("Rever")
				USE RevSNu IN 0 ORDER TAG RevSNu1 ALIAS Rever
				Borra = .T.
				SELE IteRi
				SET RELATION TO NumMes+NumRev INTO Rever ADDITIVE
			ENDIF
			SELECT Iteri
			SET RELATION TO Periodo+NumMes+NumRI INTO RecIng ADDITIVE
			SET FILTER TO NUMMES<=allt(vCalend) and tipo='P' AND ITERI.ESTADO#'99' AND IIF(!EMPTY(NumRI) AND EMPTY(NumRev),(Recing.codfte=allt(xcodfte) AND Recing.CodCad=allt(xCodCad)), (Rever.codfte=allt(xcodfte) AND Rever.CodCad=allt(xCodCad)) )
			GO TOP
			SCAN
				IF EMPTY(NumRI) AND !EMPTY(NumRev)
					cCadena = Rever.CodCad
					cFuente = Rever.CodFte
					nImporte = ImpParc*-1
				 ELSE
					cCadena = RecIng.CodCad
					cFuente = RecIng.CodFte
					nImporte = ImpParc
				ENDIF
				sele maepre
				IF !EMPTY(ITERI.NUMRI)
					seek Vperiodo+'01001'+RECING.codcad
				ELSE
					seek Vperiodo+'01001'+cCadena
				ENDIF
				VESTFUN=UNIGES+UNIEJE+CODFUN+CODPRG+CODSPR+ACTPRY+CODCOM+CODMET
				IF !EMPTY(ITERI.NUMRI)
					vkey = RECING.codcad+RECING.CODFTE+ITEri.CODPART
					VCODCAD = RECING.codcad
					VCODFTE = RecIng.codFte
				ELSE
					vkey = cCadena+cFuente+ITEri.CODPART
					VCODCAD = cCadena
					VCODFTE = cFuente
				ENDIF
				vmes = 'MES_'+ALLTRIM(iteri.nummes)
				SELE REPO
				SEEK vkey
				IF FOUND()
					REPLACE totafe WITH totafe+nImporte
					REPLACE  &vmes WITH &vmes+nImporte
				ELSE
					append blank
					REPLACE CODPART WITH ITEri.CODPART,;
							UNIGES  WITH MAEPRE.UNIGES,;
							UNIEJE  WITH MAEPRE.UNIEJE,;
							Metas    WITH MAEPRE.Descri,;
							PERIODO WITH VPERIODO,;
							CODCAD  WITH VCODCAD,;
							CODFTE  WITH VCODFTE,;
							&vmes   WITH nImporte,;
							ESTFUN  WITH VESTFUN,;
							TOTAFE  WITH nImporte
				ENDIF
				IF ITERI.NUMMES = ALLTRIM(vcalend)
					REPLACE totCAL WITH totCAL+nImporte
				ENDIF
				SELECT ITEri
			ENDSCAN
		    DEACTIVATE WINDOW Standby
			SELECT REPO
			GO TOP
			IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				DO reporte WITH 2,"SalPrei1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
			ENDIF
			if yesno('Imprime Consolidado')
				xdbf = SYS(3) + '.DBF'
				xind = SYS(3) + '.IDX'
				COPY STRUC TO (xdbf)
				INDEX ON CODFTE+codpart TO (xind)
				use (xdbf) in 0 alias consol
				select repo
				go top
				SCAN
					vcodpart1 = CODFTE+codpart
					vcodpart2 = CODFTE+codpart
					VTOTAFE = 0	
					VTOTCAL = 0
					VPRESU = 0
					SCATTER MEMVAR
					do while vcodpart1=vcodpart2 AND !EOF()
						VPREsu  = VPREsu  + valpart
						VTOTCAL = VTOTCAL + TOTCAL	&& DEL MES
						VTOTAFE = VTOTAFE + TOTAFE	&& ACUMULADO
						SKIP
						vcodpart2 = CODFTE+codpart
					ENDDO
					m.valpart= vpresu
					m.totCAL = vtotCAL
					m.totafe = vtotafe
					SELE CONSOL
					APPEND BLANK
					GATHER MEMVAR
					select repo
					skip -1
				endscan	
				SELECT CONSOL
				REPLACE ALL CODCAD WITH '0001'
				GO TOP
				DO reporte WITH 2,"LisE5ci",' Consolidado de la Ejecucion ',1,.F.,.T.
				USE
				ERASE (XDBF)
			endif
			SELE IteRi
			SET RELATION OFF INTO RecIng
		ENDIF
	CASE OPCION=4
		*------ CONSOLIDADO PRESUPUESTO ANUAL -----------
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
			TITLE ' °°  Presupuesto Gen‚rico  °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1
		@  2,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30) 
		@  6,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		
		@  7,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 	VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) 
		@  8,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 	VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	
		@  9,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!'VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			
		@ 10,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			
		@ 12,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		ELSE
		    ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			SELE REPO
			vind = SYS(3) + '.IDX'
			INDEX ON PERIODO+CODCAD+CODFTE+codpart TO (vind)
			SET INDEX TO (vind)
			SELE itepar
			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
			GO TOP
			SCAN
				SCATTER MEMVAR
				SELECT REPO
				SEEK itepar.periodo+itepar.CODCAD+itepar.CODFTE+itepar.codpart
				vcod = 'FTE'+ALLTRIM(m.codfte)
				m.transf = m.tra001+m.tra003+m.tra004+m.tra005
				m.totcal = 0
				IF !FOUND()
					m.&vcod = m.valpart+m.cresup+m.traNSF
					APPEND BLANK
					GATHER MEMVAR
					m.&vcod=0
				ELSE
					IF RLOCK()
						REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
					ENDIF
					UNLOCK
					&vcod = 0
				ENDIF
				SELECT itepar
			ENDSCAN
		    DEACTIVATE WINDOW Standby
			SELE REPO
			GO TOP
			IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				REPLACE ALL VALPART WITH FTE00+FTE01+FTE09
				vind = SYS(3) + '.IDX'
				INDEX ON PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART TO (vind)
				GO TOP
				DO reporte WITH 2,"LisPreg1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
			ENDIF
		ENDIF
		
	*************************************************************
	**********REPORTES DEL PRESUPUESTO MENSUALIZADO**************
	*************************************************************
	CASE OPCION=5 
		*------ CONSOLIDADO PRESUPUESTO ANUAL -----------
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
			TITLE ' °°  Consolidado Presupuesto Anual  °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1
		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'		VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!'		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
		@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!'		VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1
		@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'		VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
		@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!'		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
		@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!'		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
		@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!'	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
		@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!'		VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
		@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!'		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		ELSE
		    ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			SELE REPO
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
				SELECT REPO
				SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
	
				vcod = 'FTE'+ALLTRIM(m.codfte)
				m.transf = m.tra001+m.tra003+m.tra004+m.tra005
				m.totcal = 0
				IF !FOUND()
					m.&vcod = m.valpart+m.cresup+m.transf
					APPEND BLANK
					GATHER MEMVAR
					m.&vcod=0
				ELSE
					IF RLOCK()
						REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.traNSF
					ENDIF
					UNLOCK
					&vcod = 0
				ENDIF
				SELECT itepar
			ENDSCAN
		    DEACTIVATE WINDOW Standby
			SELE REPO
			GO TOP
			IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				IF vtotal = 1
					if vtipo = 1
						DO reporte WITH 2,"LisPrem1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ELSE
						DO reporte WITH 2,"LisPrem2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ENDIF
				ELSE
					if vtipo = 1
						DO reporte WITH 2,"LISPRMG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ELSE
						DO reporte WITH 2,"LisPrMG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
					ENDIF	
				ENDIF	
			ENDIF
		ENDIF
		
	CASE OPCION=6
		vtrimes=' '
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
			TITLE ' °°  Consolidado Presupuesto Anual  °° ' FLOAT COLOR SCHEME 5
		ACTIVATE WINDOW lis_1
		@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
		@  1,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  2,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
		@  3,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',18,30)
		@  4,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30)
		@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1
		@  8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
		@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
		@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
		@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
		@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
		@ 13,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
		@ 14,2 SAY ' N§Trimestre : ' GET vtrimes    PICTURE '@M 1,2,3,4,T'   
		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		ELSE
		    ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			USE IN 6
			*- Abriendo Archivos
			USE repcal1  IN 6              &&  ALIAS rep1   
			SELE 6
			vInd = SYS(3) + '.DBF'
			COPY STRU TO (vInd)
			use (vind) in 6 alias REPO EXCLUSIVE
			SELE REPO
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
				SELECT REPO
				SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
				IF !FOUND()
					m.ValPres = m.valpart+m.cresup+m.tra001+m.tra003+m.tra004+m.tra005
					APPEND BLANK
					GATHER MEMVAR
				ELSE
					IF RLOCK()
						REPLACE ValPres WITH ValPres + m.valpart+m.cresup+m.tra001+m.tra003+m.tra004+m.tra005
					ENDIF
					UNLOCK
				ENDIF
				SELECT Calen
				VKEY  = itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+itepar.codpart
				VKEY0 = itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+itepar.codpart
				SEEK VKEY
				IF FOUND()
					DO WHILE VKEY = VKEY0 AND !EOF()
						vTotCal = 0
						vkey1 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
						vKey2 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
						vcod1 = 'C_'+ALLTRIM(nummes)
						DO WHILE vKey1 = vKey2
							vTotCal = vTotCal + ValPart + Ampliar
							SKIP
							vKey2 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
						ENDDO
						SELECT REPO
						REPLACE &vcod1 WITH &vcod1 + vTotCal
						SELECT Calen
						VKEY0 = periodo+uniges+unieje+codcad+codfte+codpart
					ENDDO	
				ENDIF	
				SELECT ItePar
			ENDSCAN
		    DEACTIVATE WINDOW Standby
			SELE REPO
			GO TOP
			IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				IF vtotal = 1
					if vtipo = 1
						IF vtrimes = 'T'
							DO reporte WITH 2,"LisPrT1",' Presupuesto a nivel Trimestral(01) '
      						DO reporte WITH 2,"LisPrT2",' Presupuesto a nivel Trimestral(02) '
      						DO reporte WITH 2,"LisPrT3",' Presupuesto a nivel Trimestral(03) '
	      					DO reporte WITH 2,"LisPrT4",' Presupuesto a nivel Trimestral(04) '
    	  				ELSE
      						
      						VREPO = "LisPrt"+vtrimes
      		    			DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      					ENDIF
	    			ELSE
    					IF vtrimes = 'T'
      						DO reporte WITH 2,"LisPrT11",' Presupuesto a nivel Trimestral(01) '
      						DO reporte WITH 2,"LisPrT22",' Presupuesto a nivel Trimestral(02) '
      						DO reporte WITH 2,"LisPrT33",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"LisPrT44",' Presupuesto a nivel Trimestral(04) '
	      				ELSE
    	  					VREPO = "LisPrT"+vtrimes+vtrimes
      			    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      					ENDIF
    				ENDIF
				ELSE
					if vtipo = 1
						IF vtrimes = 'T'
      						DO reporte WITH 2,"LisPGT1",' Presupuesto a nivel Trimestral(01) '
      						DO reporte WITH 2,"LisPGT2",' Presupuesto a nivel Trimestral(02) '
      						DO reporte WITH 2,"LisPGT3",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"LisPGT4",' Presupuesto a nivel Trimestral(04) '
	      				ELSE
    	  					VREPO = "LisPGt"+vtrimes
      			    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
	      				ENDIF
    				ELSE
    					IF vtrimes = 'T'
      						DO reporte WITH 2,"LisPGT11",' Presupuesto a nivel Trimestral(01) '
      						DO reporte WITH 2,"LisPGT22",' Presupuesto a nivel Trimestral(02) '
      						DO reporte WITH 2,"LisPGT33",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"LisPGT44",' Presupuesto a nivel Trimestral(04) '
	      				ELSE
    	  					VREPO = "LisPGT"+vtrimes+vtrimes
      			    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      					ENDIF
    				ENDIF
		  		ENDIF
    	    ENDIF
		ENDIF
		
	CASE OPCION=7
		vtrimes=' '
		DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
		TITLE ' °°  Consolidado Presupuesto Anual  °° ' FLOAT COLOR SCHEME 5
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
		@ 13,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 		VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
		@ 14,2 SAY ' N§Trimestre : ' GET vtrimes    PICTURE '@M 1,2,3,4,T'   
		READ VALID val_read()
		DEACTIVATE WINDOW lis_1
		IF LASTKEY()= 27
			DO vista
			RETURN
		ENDIF
		SELE itepar
		IF EOF()
			DO standby WITH vmens08
		ELSE
		    ACTIVATE WINDOW Standby
			@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
			USE IN 6
			*- Abriendo Archivos
			USE repcal1  IN 6              &&  ALIAS rep1   
			SELE 6
			vInd = SYS(3) + '.DBF'
			COPY STRU TO (vInd)
			use (vind) in 6 alias REPO EXCLUSIVE
			SELE REPO
			zap
			vind = SYS(3) + '.IDX'
			INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
			SET INDEX TO (vind)
			
			GO TOP
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
				SELECT REPO
				SEEK  LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
				m.transf  = m.tra001+m.tra003+m.tra004+m.tra005
				STORE 0 TO m.M_01,m.M_02,m.M_03,m.M_04,m.M_05,m.M_06
				STORE 0 TO m.M_07,m.M_08,m.M_09,m.M_10,m.M_11,m.M_12
				
				IF !FOUND()
					m.ValPres = m.valpart
					APPEND BLANK
					GATHER MEMVAR
				ELSE
					IF RLOCK()
						REPLACE ValPres WITH ValPres + m.valpart
						REPLACE cresup  WITH cresup  + m.cresup
						REPLACE transf  WITH cresup  + m.transf
					ENDIF
					UNLOCK
				ENDIF
				SELECT Calen
				VKEY  = itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+itepar.codpart
				VKEY0 = itepar.periodo+itepar.uniges+itepar.unieje+itepar.codcad+itepar.codfte+itepar.codpart
				SEEK VKEY
				IF FOUND()
					DO WHILE VKEY = VKEY0 AND !EOF()
						vTotCal = 0
						vtotafe = 0
						vkey1 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
						vKey2 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
						vcod1 = 'C_'+ALLTRIM(nummes)
						DO WHILE vKey1 = vKey2
							vTotCal = vTotCal + ValPart + Ampliar
							SKIP
							vKey2 = periodo+uniges+unieje+codcad+codfte+codpart+nummes
						ENDDO
						SELECT REPO
						REPLACE &vcod1 WITH &vcod1 + vTotCal
						SELECT Calen
						VKEY0 = periodo+uniges+unieje+codcad+codfte+codpart
					ENDDO
				ENDIF
				SELECT ItePar
			ENDSCAN
			SELE REPO
			zind = SYS(3) + '.IDX'
			INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)
			** HOJAS DE AFECTACION
			SELECT IteHc
			DO CASE
				CASE vtrimes = '1'
					SET FILTER TO NUMMES<='03' AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
				CASE vtrimes = '2'
					SET FILTER TO NUMMES<='06' AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
				CASE vtrimes = '3'
					SET FILTER TO NUMMES<='09' AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
				OTHER
					SET FILTER TO IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
					IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)	AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
			ENDCASE		
			GO TOP
			SCAN
				sele maepre
				seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
				vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE+ITEHC.CODPART
				vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE
				vmes = 'M_'+ALLTRIM(itehc.nummes)
				SELE REPO
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
				* afectaciones de emergancia
				SELE 9
				USE
				USE IteHc    IN 9  ORDER TAG Itehc1   ALIAS Itehc
				SELE 5
				USE
				USE maepre   IN 5  ORDER TAG maepre1  ALIAS maepre
			ENDIF
			DEACTIVATE WINDOW Standby
			SELE REPO
			GO TOP	
			* RESTRUCTURACION DE CADENAS
			SCAN
				VKEY = PERIODO+LEFT(ESTFUN,30)+CODFTE
				SELE ITEPAR
				SET ORDER TO ITEPARI4
				SEEK VKEY
				IF FOUND()
					REPLACE REPO.CODCAD WITH ITEPAR.CODCAD
				ENDIF
				SELE REPO
			ENDSCAN
			GO TOP
	      	IF EOF()
				DO standby WITH 'No existe Registros para procesar'
			ELSE
				IF vtotal = 1
					if vtipo = 1
						IF vtrimes = 'T'
      						DO reporte WITH 2,"Liscal1",' Presupuesto a nivel Trimestral(01) '
	      					DO reporte WITH 2,"Liscal2",' Presupuesto a nivel Trimestral(02) '
    	  					DO reporte WITH 2,"Liscal3",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"Liscal4",' Presupuesto a nivel Trimestral(04) '
      		
      					ELSE
      						VREPO = "Liscal"+vtrimes
      		    			DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
	      				ENDIF
    				ELSE
    					IF vtrimes = 'T'
      						DO reporte WITH 2,"LisCal11",' Presupuesto a nivel Trimestral(01) '
      						DO reporte WITH 2,"LisCal22",' Presupuesto a nivel Trimestral(02) '
      						DO reporte WITH 2,"LisCal33",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"LisCal44",' Presupuesto a nivel Trimestral(04) '
	      				ELSE
    	  					VREPO = "LisCal"+vtrimes+vtrimes
      			    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      					ENDIF
    				ENDIF
				ELSE
					if vtipo = 1
						IF vtrimes = 'T'
      						DO reporte WITH 2,"LisCaG1",' Presupuesto a nivel Trimestral(01) '
      						DO reporte WITH 2,"LisCaG2",' Presupuesto a nivel Trimestral(02) '
      						DO reporte WITH 2,"LisCaG3",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"LisCaG4",' Presupuesto a nivel Trimestral(04) '
	      				ELSE
    	  					VREPO = "LisCaG"+vtrimes
      			    		DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
      					ENDIF
    				ELSE
    					IF vtrimes = 'T'
      						DO reporte WITH 2,"LisCaG11",' Presupuesto a nivel Trimestral(01) '
	      					DO reporte WITH 2,"LisCaG22",' Presupuesto a nivel Trimestral(02) '
    	  					DO reporte WITH 2,"LisCaG33",' Presupuesto a nivel Trimestral(03) '
      						DO reporte WITH 2,"LisCaG44",' Presupuesto a nivel Trimestral(04) '
      					ELSE
      						VREPO = "LisCaG"+vtrimes+vtrimes
      		    			DO reporte WITH 2,vrepo,' Presupuesto a nivel Trimestral'+'('+vtrimes+')'
	      				ENDIF
    				ENDIF
	  			ENDIF
	        ENDIF
		ENDIF    
ENDCASE
SELECT REPO
USE
ERASE (VIND)
ERASE (VDBF)
USE REPOPRE IN 6  ALIAS REPO EXCLU
SELECT presu
SET ORDE TO 1
GO vteXp
DO vista
RETURN

FUNCTION Asign_tri
*-----------------
valias = ALIAS()
vorder = ORDER()
vrecno = RECNO()
SET EXCLU OFF
IF YESNO('¨ Seguro de realizar la asignaci¢n trimestral ?')
	ACTIVATE WINDOWS standby
	@ 1,2 say ' Realizando Proceso de Asignaci¢n trimestral ' COLOR W+/BR*
	SELECT ItePar
	GO TOP
	SCAN
		SCATTER MEMVAR
		REPLACE Tri_01 WITH m.M_01+m.M_02+m.M_03 ,;
				Tri_02 WITH m.M_04+m.M_05+m.M_06 ,;
				Tri_03 WITH m.M_07+m.M_08+m.M_09 ,;
				Tri_04 WITH m.M_10+m.M_11+m.M_12
	ENDSCAN
	RELEASE WINDOWS standby
ENDIF
SELECT (valias)
SET ORDER TO (vorder)
GO vrecno
SET EXCLU ON
do vista
RETURN

FUNCTION XAsign_tri
*-----------------
valias = ALIAS()
vorder = ORDER()
vrecno = RECNO()
ACTIVATE WINDOWS standby
@ 1,2 say ' Realizando Proceso de Asignaci¢n trimestral ' COLOR W+/BR*
SELECT ItePar
GO TOP
SCAN
	SCATTER MEMVAR
	m.Tri_01 = round(m.valpart*.22,0)
	m.Tri_02 = round(m.valpart*.24,0)
	m.Tri_03 = round(m.valpart*.25,0)
	m.Tri_04 = m.valpart-(m.Tri_01+m.Tri_02+m.Tri_03)
	GATHER MEMVAR
ENDSCAN
RELEASE WINDOWS standby
SELECT (valias)
SET ORDER TO (vorder)
GO vrecno
RETURN .t.



PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE fin_opcion
*-------------------
CLOSE DATA
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_2A
RELEASE WINDOW wind_3
RELEASE MENU   mmenu
RESTORE SCREEN FROM principal
RETURN


*---------------------
*FUNCIONES DE REPORTES
*---------------------
FUNCTION sumpre
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
SUM VALPRES TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

*FUNCTION sumpre2
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
SUM FTE00+FTE01+FTE09 TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumpre2
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM VALPART TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma


FUNCTION sumspg
*-----------------
PARAMETER PART
prg =repO.codcad+repO.codfte
vrec = RECNO()
GO TOP
SUM &part TO sumsg FOR repO.codcad+repO.codfte=prg
GO vrec
RETURN sumsg

FUNCTION sumar_prg
*-----------------
PARAMETER vCalen, PART,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = 1
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = 2
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
SUM &part TO sumsg FOR &vFiltro= vCalen
GO vrec
RETURN sumsg

PROCEDURE TOTALPRG1
*------------------
PARAMETER vCalen,vnivel,vtipo,vtrim
TOTALT = 0
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE	
IF VTIPO = "1"
	do case
 		case vtrim ='1'
	 		SUM  M_01+M_02+M_03 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='2'
 			SUM  M_04+M_05+M_06 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='3' 	
 			SUM  M_07+M_08+M_09 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='4'
 		    SUM  M_10+M_11+M_12 TO totalm FOR &vFiltro= vCalen
 	ENDCASE	    
 ELSE
 	do case
 		case vtrim ='1'
 			SUM C_01+C_02+C_03 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='2'	
 			SUM C_04+C_05+C_06 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='3'	
 			SUM C_07+C_08+C_09 TO totalm FOR &vFiltro= vCalen
 		case vtrim ='4'	
 			SUM C_10+C_11+C_12 TO totalm FOR &vFiltro= vCalen
 	ENDCASE		
 ENDIF	
 TOTALT=TOTALT+TOTALM
GO TOP
GO vrec
RETURN TOTALT

FUNCTION sumpre1
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '0'
		vFiltro = 'LEFT(ESTFUN,5)'
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE	
SUM FTE00+FTE01+FTE09+FTE10 TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumcre
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM CRESUP TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumtra
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM TRANSF TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION summef
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE00+FTE01+FTE09 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE00+FTE01+FTE09 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summef1
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM VALPRES TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM VALPRES TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summef00
*---------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE00 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE00 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summef01
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE01 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE01 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

FUNCTION summef09
*--------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrecno = RECNO()
go top
IF VTIPO <= 25
	SUM FTE09 TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM FTE09 TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrecno
RETURN suma

*FUNCTION sumcal
*-----------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
go top
IF VTIPO <= 25
	SUM totcal TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM totcal TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrec
RETURN suma

FUNCTION sumcal
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM totcal TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma


FUNCTION sumcal1
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
SUM totcal TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

*FUNCTION SUMafe
*-----------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
go top
IF VTIPO <= 25
	SUM totafe TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM totafe TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrec
RETURN suma

FUNCTION SUMafe
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM totafe TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma



FUNCTION SUMafe1
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
SUM totafe TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION sumsal1
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

SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

*FUNCTION sumsal
*-----------------
PARAMETER vfiltro
vfiltro = ALLT(vfiltro)
vtipo   = LEN(vfiltro)
vrec = RECNO()
go top
IF VTIPO <= 25
	SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
ELSE
	SUM (FTE00+FTE01+FTE09)-TOTCAL TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
ENDIF	
GO vrec
RETURN suma

FUNCTION sumsal
*-----------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '1'
		vFiltro = 'CODCAD'
	CASE vNivel = '2'
		vFiltro = 'CODCAD+codfte'
ENDCASE	
SUM VALPART-TOTAFE TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma



PROCEDURE chequeoa
*----------------
parameter vmes1
vtotmes = M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12
vValpart = valpart+cresup+tra001+tra003+tra004+tra005
IF vValpart<vtotmes
	vsaldo = vtotmes-vValpart
	*do Standby with 'Monto supera al Presupuesto en : '+str(vsaldo,15,2)
	*replace &vmes1 with 0
	*RETURN .F.
endif
REPLACE TRI_01 WITH M_01+M_02+M_03
REPLACE TRI_02 WITH M_04+M_05+M_06
REPLACE TRI_03 WITH M_07+M_08+M_09
REPLACE TRI_04 WITH M_10+M_11+M_12
RETURN .T.

FUNCTION VAL_SALDO
*-----------------
VSALDO1 = valpart+CRESUP+TRA001+TRA003+TRA004+TRA005
VSALDO2 = M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12
VSALDO  = VSALDO1 - VSALDO2
RETURN VSALDO 

FUNCTION VAL_presu
*-----------------
vmarco = valpart+CRESUP+TRA001+TRA003+TRA004+TRA005
RETURN vMarco


FUNCTION sumMES
*-----------------
PARAMETER vCalen,PART,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '0'
		vFiltro = 'LEFT(ESTFUN,5)'
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
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro
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
		SUM FTE00+FTE01+FTE09-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,vtipo) = vFiltro
	CASE VTIPO > 25 AND VTIPO < 28
		SUM FTE00+FTE01+FTE09-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,25)+codfte = vFiltro
	CASE VTIPO > 28 AND VTIPO < 30
		SUM FTE00+FTE01+FTE09-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+LEFT(codpart,2) = vFiltro
	CASE VTIPO > 30
		SUM FTE00+FTE01+FTE09-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)  TO suma FOR LEFT(ESTFUN,25)+codfte+codpart = vFiltro

ENDCASE
GO vrec
RETURN suma

PROCEDURE SUMAR
*--------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '0'
		vFiltro = 'LEFT(ESTFUN,5)'
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE	
SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12  TO suma FOR &vFiltro= vCalen
GO vrec
RETURN suma

FUNCTION salprg
*---------------
PARAMETER vCalen,vnivel
vrec = RECNO()
GO TOP
DO CASE
	CASE vNivel = '0'
		vFiltro = 'LEFT(ESTFUN,5)'
	CASE vNivel = '1'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD'
	CASE vNivel = '2'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
	CASE vNivel = '3'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
	CASE vNivel = '4'
		vFiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE	
SUM FTE00+FTE01+FTE09-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12) TO sumA FOR &vFiltro = vCalen
GO vrec
RETURN sumA


 FUNCTION Val_Ing2
*-----------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )

 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
*  DO CASE
*    CASE mVaRiable=' '   && En edici¢n
*      @ ROW(),mCol   SAY mValor
*      @ ROW(),mCol+7 SAY mDescr
*      RETURN .T.
*    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
*      @ ROW(),mCol SAY mDescr
*      RETURN
*    CASE mVaRiable='V'   && En vista
*      @ ROW(),COL()  SAY mValor
*      RETURN mDescr
*    CASE mVaRiable='D'   && En vista
*      RETURN mDescr
*    CASE mVaRiable='Z'   && En vista SIN PINTAR
*      RETURN mDescr
*    CASE mVaRiable='C'   && Solo codigo
*      RETURN .T.
*    CASE mVaRiable='T'
*      &mVaRiable = mValor
*      @ ROW(),mCol+7 SAY mDescr
 *     RETURN  mValor
 *   OTHERWISE            && En browse de edici¢n
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SELECT (mAlias)
 REPLACE Iteri.codpart  WITH mValor
 REPLACE Iteri.concep  WITH mdescr

 *     REPLACE codpart WITH mValor
*      replace concep with mdescr
*      CODPART=MVALOR
 *     wCodPart=MVALOR
      RETURN .t. 
*  ENDCASE
RETURN

FUNCTION Val_IngE
*---------------
*  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PARAMETERS  Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
*      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SET ORDER TO INGRESO1
  SEEK Filtro
  *+mValor

 IF FOUND()
	 mValor = IngR.CodIng 
	 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 ENDIF

  IF !FOUND() AND !mVaRiable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF

* IF FOUND()
	 mValor = IngR.CodIng 
	 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
* ENDIF

 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
      @ ROW(),mCol   SAY mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mValor
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    CASE mVaRiable='T'
      &mVaRiable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
   * OTHERWISE            && En browse de edici¢n
    CASE MVARIABLE='X'
      REPLACE &mVaRiable WITH mValor
      wCodPart=MVALOR
      RETURN .T.
  ENDCASE

FUNCTION val_parti    && VALIDA PARTIDAS de ingreso
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
ENDCASE
malias  = ALIAS()
SELECT ingr
SEEK filtro+mvalor

IF !FOUND() AND !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(filtro)
		SET FILTER TO codING = filtro
	ENDIF
	GO TOP
	IF EOF()
		DO standby WITH 'No existen partidas definidas'
		SET FILTER TO
		SELE (malias)
		RETURN .f.
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD codING+' '+SUBSTR(desING,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = Ingr.coding
mdescr = SUBSTR( Ingr.desing, 1, mlong )
m.codpart = mvalor
SET ORDE TO 1
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL   SAY mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	CASE mvariable=='T'
		&mvariable = mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN  mvalor
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
ENDCASE

RETURN


* CASE OPCION=3
*	*------ SALDO PRESUPUESTAL -----------
*	DEFINE WINDOW lis_1 FROM 4,10 TO 20,70 DOUBLE ;
*		TITLE ' °°  Saldo Presupuestal °° ' FLOAT COLOR SCHEME 5
*	ACTIVATE WINDOW lis_1
*	@  0,2 SAY '     Periodo : ' GET vperiodo   PICTURE '!!' 	VALID !EMPTY(vperiodo)
*	@  1,2 say '  Calendario : ' GET vCalend    PICTURE '!!'	VALID VAL_PARA(vCalend  ,'FECMES',' ',18,25)
*	@  2,2 SAY '  Por Cadena : ' GET vTotal  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
*	@  3,2 SAY '  Espec¡fico : ' GET vTipo   FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6
*	@  4,2 SAY '  U. Gestora : ' GET vUniges 	PICTURE '!!'    	VALID IIF(!EMPTY(vUniGes),val_para(vUniGes,'UNIGES',' ',18,30),.T.)
*	@  5,2 SAY 'U. Ejecutora : ' GET vUnieje    PICTURE '!!!' 		VALID IIF(!EMPTY(vUniEje),val_para1(vUniEje,'UNIEJE'+vUniGes,' ',18,30),.T.)
*	@  6,2 SAY 'Cad. Funcion.: ' GET vcodcad    PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+allT(vuniges)+allt(vunieje),' ',18,30),.T.) when vTotal=1
*	@ 8,2 SAY '     Funci¢n : ' GET vcodfun    PICTURE '!!'    	VALID IIF(!EMPTY(VCODfun),val_para(vcodfun,'CODFUN',' ',18,30),.T.) 		when vTotal=2
*	@  9,2 SAY '    Programa : ' GET vcodprg    PICTURE '!!!' 		VALID IIF(!EMPTY(VCODPRG),val_para1(vcodprg,'CODPRG'+vCodFun,' ',18,30),.T.) when vTotal=2
*	@ 10,2 SAY ' SubPrograma : ' GET vcodspr    PICTURE '!!!!' 		VALID IIF(!EMPTY(VCODSPR),val_para1(vcodspr,'CODSPR'+vCodPrg,' ',18,30),.T.)	when vTotal=2
*	@ 11,2 SAY 'Activ/Proyec : ' GET vactpry    PICTURE '!!!!!!' 	VALID IIF(!EMPTY(VACTPRY),val_para(vactpry,'ACTPRY',' ',18,30),.T.)			when vTotal=2
*	@ 12,2 SAY '  Componente : ' GET vcodcom    PICTURE '!!!!!' 	VALID IIF(!EMPTY(VCODCOM),val_para(vcodcom,'CODCOM',' ',18,30),.T.)			when vTotal=2
*	@ 14,2 SAY '   Fte. Fto. : ' GET vcodfte    PICTURE '!!' 	VALID IIF(!EMPTY(VCODFTE),val_para(vcodfte,'CODFTE',' ',18,30),.T.)
*	READ VALID val_read()
*	DEACTIVATE WINDOW lis_1
*
*	IF LASTKEY()= 27
*		DO vista
*		RETURN
*	ENDIF
*	SELE itepar
*	IF EOF()
*		DO standby WITH vmens08
*	ELSE
*	    ACTIVATE WINDOW Standby
*		@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
*		SELE REPO
*		vind = SYS(3) + '.IDX'
*		INDEX ON LEFT(ESTFUN,5)+codcad+codfte+codpart TO (vind)
*		SET INDEX TO (vind)
*		IF vtotal = 1
*			SELE Calen
*			SET FILTER TO periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
*				AND NUMMES<=ALLTRIM(vcalend) AND IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
*			SELE itepar
*			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
*				AND	IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.)
*		ELSE
*			SELE Calen
*			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) ;
*				AND NUMMES<=ALLTRIM(vcalend) 
*			SELE itepar
*			SET FILTER TO  periodo=ALLTRIM(Vperiodo) AND IIF(!EMPTY(ALLT(vUniGes)),subs(estfun,1,2)=allt(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),subs(estfun,3,3)=allt(vUnieje),.T.) AND IIF(!EMPTY(ALLT(vcodfun)),subs(estfun,6,2)=allt(vcodfun),.T.)AND IIF(!EMPTY(ALLT(vcodprg)),subs(estfun,8,3)=allt(vcodprg),.T.)	AND IIF(!EMPTY(ALLT(vcodspr)),subs(estfun,11,4)=allt(vcodspr),.T.)AND IIF(!EMPTY(ALLT(vactpry)),subs(estfun,15,6)=allt(vactpry),.T.)AND IIF(!EMPTY(ALLT(vcodcom)),subs(estfun,21,5)=allt(vcodcom),.T.)and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.)
*		ENDIF	
*		GO TOP
*		SCAN
*			SCATTER MEMVAR
*			m.cresup = GETCRE()
**			m.transf = GETTRA()
*			SELECT REPO
*			SEEK LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
*			vcod = 'FTE'+ALLTRIM(m.codfte)
*			m.totcal = 0
*			IF !FOUND()
*			    m.&vcod = m.valpart+m.cresup			&&+m.transf
*				APPEND BLANK
*				GATHER MEMVAR
*				m.&vcod=0
*			ELSE
*				IF RLOCK()
*					REPLACE &vcod WITH &vcod + m.valpart+m.cresup+m.transf
*				ENDIF
*				UNLOCK
*				&vcod = 0
*			ENDIF
*
*			SELECT Calen
*			vFilt = itepar.periodo+LEFT(ITEPAR.ESTFUN,5)+itepar.codcad+itepar.codfte+itepar.codpart
*			SEEK vfilt
*			
*			IF found()
*				vTotCal = 0
*				vTotafe = 0
*				vkey1 = periodo+LEFT(ESTFUN,5)+codcad+codfte+codpart
*				vKey2 = periodo+LEFT(ESTFUN,5)+codcad+codfte+codpart
*				DO WHILE vkey1 = vkey2 and !eof()
*					vTotCal = vTotCal + ValPart + AMPLIAR
*					SKIP
*					vKey2 = periodo+LEFT(ESTFUN,5)+codcad+codfte+codpart
*				ENDDO
*				SELECT REPO
*				REPLACE TotCal WITH TotCal + vTotCal
*				REPLACE Totafe WITH Totafe + vTotafe
*			ENDIF
*			SELECT ItePar
*		ENDSCAN
*		SELE REPO
*		zind = SYS(3) + '.IDX'
*		INDEX ON LEFT(ESTFUN,30)+CODFTE+CODPART TO (zind)
*		
*		** HOJAS DE AFECTACION
*		SELECT IteHc
*		SET FILTER TO NUMMES<=allt(vCalend) and  IIF(!EMPTY(ALLT(VCODcad)),codcad=allt(vcodcad),.T.) and IIF(!EMPTY(ALLT(VCODFTE)),codfte=allt(vcodfte),.T.) AND ;
*				IIF(!EMPTY(ALLT(vUniGes)),UniGes=ALLT(vUniGes),.T.) and IIF(!EMPTY(ALLT(vUniEje)),UNIeje=ALLT(vUniEje),.T.) AND IIF(!EMPTY(NUMPA),MESPR#NUMMES,.T.) AND IIF(!EMPTY(NUMPR),MESPR=NUMMES,.T.)
*		GO TOP
*		SCAN
*			sele maepre
*			seek Vperiodo+itehc.uniges+itehc.unieje+itehc.codcad
*			vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE+ITEHC.CODPART
*			vkey1= uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODFTE
*			vmes = 'M_'+ALLTRIM(itehc.nummes)
*			SELE REPO
*			SEEK vkey
*			IF FOUND()
*				REPLACE totafe WITH totafe+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
*			ELSE
*				GO TOP	
*				LOCATE FOR LEFT(ESTFUN,30)+CODFTE = vkey1
*				IF FOUND()
*					append blank
*					REPLACE CODPART WITH ITEHC.CODPART,;
*							PERIODO WITH VPERIODO,;
*							CODCAD  WITH ITEHC.CODCAD,;
*							CODFTE  WITH ITEHC.CODFTE,;
*							ESTFUN  WITH LEFT(VKEY,30),;
*							&vmes   WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
*				ENDIF			
*			ENDIF
*			SELECT ITEHC
*		ENDSCAN
*		IF SISTEM = '1'
*			SELE 9
*			USE
*			USE IteHc    IN 9  ORDER TAG Itehc1   ALIAS Itehc
*			SELE 5
*			USE
*			USE maepre   IN 5  ORDER TAG maepre1  ALIAS maepre
*		ENDIF	
*	    DEACTIVATE WINDOW Standby
*		SELECT REPO
*		GO TOP
*		* RESTRUCTURACION DE CADENAS
*		SCAN
*			VKEY = PERIODO+LEFT(ESTFUN,30)+CODFTE
*			SELE ITEPAR
*			SET ORDER TO ITEPARI4
*			SEEK VKEY
*			IF FOUND()
*				REPLACE REPO.CODCAD WITH ITEPAR.CODCAD
*			ENDIF
*			SELE REPO
*		ENDSCAN
*		GO TOP
*		IF EOF()
*			DO standby WITH 'No existe Registros para procesar'
*		ELSE
*			IF vtotal = 1
*				if vtipo = 1
*					DO reporte WITH 2,"SalPreI1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
*					DO reporte WITH 2,"SalPre1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
*				ELSE
*					DO reporte WITH 2,"SalPre2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
*				ENDIF
*			ELSE
*				if vtipo = 1
*					DO reporte WITH 2,"SalPreG1",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
*				ELSE
*					DO reporte WITH 2,"SalPreG2",' Consolidado Presupuesto Anual(Funccionamiento) ',1,.F.,.T.
*				ENDIF	
*			ENDIF	
*		ENDIF
*	ENDIF
*  CASE OPCION=4
*