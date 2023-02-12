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
SET RELATION TO periodo+codCAD+codfte INTO itepar
SET SKIP TO itepar
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	presu.periodo   :H='Per',;
	presu.codcad    :H='CodCad',;
	presu.codfte    :H='Fte',;
	itepar.catgas	:H= 'CG',;	
	itepar.grugen	:H= 'GG',;	
	itepar.modapl	:H= 'MA',;	
	itepar.espgas   :H= 'EG',;
	xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;
	itepar.valpart  : H= 'Asignaci¢n' :p='99,999,999.99'

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
=val_cODCAD(ALLT(m.codcad),m.periodo,'C')
@  0,22 GET m.periodo   PICTURE '!!'  	DISABLE
@  0,60 GET m.CODCAD	PICTURE '!!!!'  DISABLE
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA(maepre.unieje,'UNIEJE','V',22,40)
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA(maepre.CodPrg,'CODPRG','V',22,40)
@  5,22 SAY VAL_PARA(maepre.CodSPr,'CODSPR','V',22,40)
@  6,22 SAY VAL_PARA(maepre.actpry,'ACTPRY','V',22,40)
@  7,22 SAY VAL_PARA(maepre.codcom,'CODCOM','V',22,40)
@  8,22 SAY maepre.codmet
@  8,60 SAY maepre.codfin
@  9,22 SAY SUBSTR(maepre.Descri,1,23)
@  9,60 SAY VAL_PARA(m.codfte,'CODFTE','V',60,18,2)

READ
IF LASTKEY() # 27
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
vperiodo = STR(YEAR(DATE())-1900,2)
STORE SPACE(2) TO vcodfte
STORE SPACE(4) TO vcodcad
vtemp = RECNO()

@  0,22 GET vperiodo   	PICTURE '!!'  
@  0,60 GET vCODCAD		PICTURE '!!!!'	VALID VAL_codcad(vcodcad,vperiodo,'C',60)
READ VALID val_read()
IF LASTKEY()=27
	DO vista
	RETURN
ENDIF
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA(maepre.unieje,'UNIEJE','V',22,40)
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA(maepre.CodPrg,'CODPRG','V',22,40)
@  5,22 SAY VAL_PARA(maepre.CodSPr,'CODSPR','V',22,40)
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
vkey = ALLTRIM(vperiodo)+ALLTRIM(vcodcad)+ALLTRIM(vcodfte)
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
	m.codcad = ALLT(vcodcad)
	m.periodo = vperiodo
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

FUNCTION Val_Codcad
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT maepre
  SEEK Filtro+mValor
                                                   *IF .NOT. FOUND() .AND. mVariable<>'V'
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO PERIODO = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    ON KEY LABEL f2  DO busCodCad
    ON KEY LABEL f3  DO busDesCad
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 02,01 TO 23,78  DOUBLE ;
		TITLE '[F2]Busca x Codigo    [F3]Busca x Descripci¢n   [F10] seleccionar' COLOR SCHEME 15
		ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCAD	: H='CodCad',;
    	CODFUN	: H='Fn',;
    	CODPRG	: H='Prg',;
    	CODSPR	: H='SPrg',;
    	ACTPRY	: H='Act/Pry',;
    	CODCOM	: H='CodComp',;
    	CODMET	: H='Meta',;
    	Descri	: H='Descripci¢n':40
	
    RELEASE WINDOWS wind_cad
    ON KEY LABEL f2  
    ON KEY LABEL f3  
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    SET FILTER TO
  ENDIF
  mValor = maepre.codcad
  mDescr = SUBSTR( maepre.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
*     @ ROW(),mCol SAY mDescr
*     RETURN ' '
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
*-------

Procedure busCodCad
*------------------
vcursor = recno()
DEFINE WINDOW lis FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda X C¢digo °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis
vcodcad  = space(4)
@  3,2 SAY '  Cadena : ' GET vcodcad    PICTURE '!!!!' 
READ
SEEK filtro+vcodcad
IF !FOUND()
	Do standby WITH 'No existe Codigo de Cadena...'
	go vcursor
ENDIF
RELEASE WINDOWS lis
RETURN

Procedure busDesCad
*------------------
vcursor = RECNO()
vorder  = ORDER()
DEFINE WINDOW lis FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis
vdescad  = space(60)
@  3,2 SAY 'Descripci¢n : ' GET vdescad    PICTURE '@S40!'
READ
SET EXACT OFF
SET ORDER TO maepre2
SEEK ALLTRIM(vdescad)
IF !FOUND()
	Do standby WITH 'No existe Codigo de Cadena...'
	go vcursor
ENDIF
RELEASE WINDOWS lis
SET ORDER TO maepre1
SET EXACT ON
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
=val_codcad(ALLT(M.codcad),M.periodo,'C')
*@  0,60 SAY IIF( m.Estado= '00','Emitido  ','Observado')
@  0,22 SAY m.periodo
@  0,60 SAY m.CODCAD
@  1,22 SAY VAL_PARA(maepre.uniges,'UNIGES','V',22,40)
@  2,22 SAY VAL_PARA(maepre.unieje,'UNIEJE','V',22,40)
@  3,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
@  4,22 SAY VAL_PARA(maepre.CodPrg,'CODPRG','V',22,40)
@  5,22 SAY VAL_PARA(maepre.CodSPr,'CODSPR','V',22,40)
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
SET ORDER TO ITEPAR1
SET FILT TO
HIDE POPUP ALL
GO TOP
SEEK m.periodo+m.codcad+ALLTRIM(m.codfte)
IF FOUND()
	DO CASE
		CASE LEFT(maepre.actpry,1)='1'
			BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+m.codCAD+ALLTRIM(m.codfte) TIMEOUT 0.0001 ;
			WINDOW wind_2 ;
			FIELDS;
			catgas		: H= 'CG',;
			grugen		: H= 'GG',;	
			modapl		: H= 'MA',;	
			espgas      : H= 'EG',;
			xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;
			valpart     : H= 'Asignaci¢n' :p='99,999,999.99' 

		CASE LEFT(maepre.actpry,1)='2'
			BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.periodo+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) TIMEOUT 0.0001 ;
			WINDOW wind_2 ;
			FIELDS;
			catgas		: H= 'CG',;
			grugen		: H= 'GG',;	
			modapl		: H= 'MA',;	
			espgas      : H= 'EG',;
			xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;
			valpart     : H= 'Asignaci¢n' :p='99,999,999.99',;
			ubicac      : H= 'Ubicaci¢n' :W=itepar.valpart#0,;
			modeje      : H= 'MEj' :W=valpart#0,;
			coddep      : H= 'UniEje':v=val_para(coddep,'CODDEP','coddep'):F :W=(ALLTRIM(m.tipfun)='I') AND valpart#0,;
			metas       : H= 'Metas F¡sicas' :W=(ALLTRIM(m.tipfun)='I') AND valpart#0
			
	ENDCASE
Else
	@ 4,30 say 'No hay asignaci¢n de presupuesto'
endif
HIDE WINDOW wind_2
SELE presu
RETURN

PROCEDURE vista_det
*------------------
SELECT itepar
SET ORDER TO 1
vtempo = '[ESC] Terminar'
ON KEY LABEL F9
HIDE POPUP ALL
GO TOP
SEEK m.periodo+ALLT(m.codcad)+ALLTRIM(m.codfte)
IF FOUND()
	DO CASE
		CASE LEFT(maepre.actpry,1)='1'
			BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) ;
			WINDOW wind_2a TITLE vtempo ;
			FIELDS;
			catgas		: H= 'CG',;
			grugen		: H= 'GG',;	
			modapl		: H= 'MA',;	
			espgas      : H= 'EG',;
			xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;
			valpart     : H= 'Asignaci¢n' :p='99,999,999.99' 

		CASE LEFT(maepre.actpry,1)='2'
			BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.periodo+ALLTRIM(m.codCAD)+ALLTRIM(m.codfte) ;
			WINDOW wind_2 TITLE vtempo;
			FIELDS;
			catgas		: H= 'CG',;
			grugen		: H= 'GG',;	
			modapl		: H= 'MA',;	
			espgas      : H= 'EG',;
			xx=val_para(itepar.espgas,'ESPGAS','D',28,50) :H='Descripci¢n',;
			valpart     : H= 'Asignaci¢n' :p='99,999,999.99',;
			ubicac      : H= 'Ubicaci¢n' :W=itepar.valpart#0,;
			modeje      : H= 'MEj' :W=valpart#0,;
			coddep      : H= 'UniEje':v=val_para(coddep,'CODDEP','coddep'):F :W=(ALLTRIM(m.tipfun)='I') AND valpart#0,;
			metas       : H= 'Metas F¡sicas' :W=(ALLTRIM(m.tipfun)='I') AND valpart#0
			
	ENDCASE
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
