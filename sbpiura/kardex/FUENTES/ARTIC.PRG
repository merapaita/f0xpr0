*--------------------------------
* artic.prg
* catalogo de articulos
*--------------------------------
*PUBLIC _vent
USE IN 1 IteArt ALIAS iteart ORDER IteArt2
USE IN 2 ArtMAeG ALIAS ArtMaeG ORDER ArtMaeG1
USE IN 3 ArtMaeC ALIAS ArtMaeC ORDER ArtMaeC1
USE IN 4 ArtMaeF ALIAS ArtMaeF ORDER ArtMaeF1

PRIVATE vmens01

vmens01 = 'Cat logo de Articulos '
vmens04 = 'Dicho Art¡culo no fue encontrado.'
vmens08 = 'No hay registros para procesar'

SELECT iteart
GOTO TOP
DO inicia
DO pantalla
DO Revis
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     DO Revis
*     ACTIVATE MENU nmenu
ENDDO

DO fin_opcion
RETURN


PROCEDURE inicia
*---------------
ACTIVATE SCREEN
vtempo = ''
vtempo = PADC(vtempo, 79, ' ')
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 09, 00 TO 23, 79 TITLE vMens01 DOUBLE COLOR SCHEME 10;
		FOOTER '[F5] Agrega   [F2] Corrige    [F3] Busca    [F8] Elimina    [F10] Salir' 
DEFINE WINDOW wind_1 FROM 00, 00 TO 08, 79 TITLE 'Articulo' DOUBLE COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 05, 00 TO 17, 79 TITLE 'Registro' DOUBLE COLOR SCHEME 5

*DEFINE MENU nmenu COLOR SCHEME 3
*DEFINE PAD revis OF nmenu PROMPT '\<Revisa' AT 24, 00
*DEFINE PAD busca OF nmenu PROMPT '\<Busca' AT 24, 08
*DEFINE PAD anter OF nmenu PROMPT '\<Anterior' AT 24, 15
*DEFINE PAD proxi OF nmenu PROMPT '\<Siguiente' AT 24, 25
*DEFINE PAD corri OF nmenu PROMPT '\<Corrige' AT 24, 36
*DEFINE PAD ingre OF nmenu PROMPT '\<Ingresa' AT 24, 45
*DEFINE PAD elimi OF nmenu PROMPT '\<Elimina' AT 24, 54
*DEFINE PAD lista OF nmenu PROMPT '\<Lista ' AT 24, 63
*DEFINE PAD termi OF nmenu PROMPT '\<Termina' AT 24, 71
*ON SELECTION PAD revis OF nmenu DO revis
*ON SELECTION PAD busca OF nmenu DO busca
*ON SELECTION PAD anter OF nmenu DO anter
*ON SELECTION PAD proxi OF nmenu DO proxi
*ON SELECTION PAD corri OF nmenu DO corri
*ON SELECTION PAD ingre OF nmenu DO ingre
*ON SELECTION PAD elimi OF nmenu DO elimi
*ON SELECTION PAD lista OF nmenu DO lista
*ON SELECTION PAD termi OF nmenu DO termi
RETURN

PROCEDURE pantalla
*-----------------
ACTIVATE WINDOW wind_1
CLEAR
@ 0,45 SAY 'Fecha : '
@ 1, 1 SAY '       Tipo: '
@ 2, 1 SAY '      Grupo:'
@ 3, 1 SAY '      Clase:'
@ 4, 1 SAY '    Familia:'
@ 5, 1 SAY '       Item:'
@ 6, 1 SAY 'Descripci¢n:'
RETURN

PROCEDURE pantalla2
*------------------
ACTIVATE WINDOW wind_2
CLEAR
@ 0,45 SAY 'Fecha : '
@ 1, 1 SAY '       Tipo:'
@ 2, 1 SAY '      Grupo:'
@ 3, 1 SAY '      Clase:'
@ 4, 1 SAY '    Familia:'
@ 5, 1 SAY '       Item:'
@ 6, 1 SAY '     Codigo:'
@ 7, 1 SAY 'Descripci¢n:'
@ 8, 1 SAY '     Unidad:'
@ 9, 1 SAY '    Menudeo:'
@10, 1 SAY '   Fraccion:'
RETURN

PROCEDURE vista
*--------------
PRIVATE _vent
_vent = WOUTPUT()
SELECT iteart
DO pantalla
IF EOF()
     RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@ 0, 65 SAY DTOC(m.fecreg)
@ 1, 14 SAY m.tipbie
@ 2, 14 SAY LEFT(vGrupo(),60)
@ 3, 14 SAY LEFT(vClase(),60)
@ 4, 14 SAY LEFT(vFamilia(),60)
@ 5, 14 SAY m.item
@ 6, 14 SAY LEFT(m.descri, 60)
ACTIVATE WINDOW (_vent)
RETURN

PROCEDURE Revis
*--------------
ON KEY LABEL F2  DO Corrige
ON KEY LABEL F3  DO Busca
ON KEY LABEL F5  DO Agrega
ON KEY LABEL F8  DO elimi
ON KEY LABEL F10 DO Termi

ACTIVATE WINDOW wind_0
SELECT iteart
IF !EOF()
     BROWSE NOOPTIMIZE NOMENU NOAPPEND NOEDIT NODELETE NOCLEAR ;
     WINDOW wind_0 TITLE '[F5] Agrega   [F2] Corrige    [F3] Busca    [F8] Elimina    [F10] Salir' ;
     NOREFRESH VALID :F vista();
	 FIELDS;
     	codart  :H='Codigo'   :15,;
     	descri  :H='Articulos':40,;
     	coduni  :H='Unidad'   :10,;
     	menudeo :H='Menud'       ,;
     	fraccion:H='Fraccion'
 ELSE
	CLEAR
	@ 4,25 SAY 'No hay Detalles de este Catalogo'
ENDIF
*SELECT iteart
*DO vista
ON KEY LABEL F2
ON KEY LABEL F3
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
KEYBOARD CHR(23)
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
*RESTORE SCREEN FROM principal
RETURN

FUNCTION vGrupo
*--------------
PRIVATE mRet
sele ArtMaeG
IF SEEK(m.tipbie+m.grubie)
   mRet = descri
 ELSE
   mRet = "Sin Detalle"
ENDIF
SELE IteArt
RETURN mRet

FUNCTION vClase
*--------------
PRIVATE mRet
sele ArtMaeC
IF SEEK(m.tipbie+m.grubie+m.ClaBie)
   mRet = descri
 ELSE
   mRet = "Sin Detalle"
ENDIF
SELE IteArt
RETURN mRet

FUNCTION vFamilia
*----------------
PRIVATE mRet
sele ArtMaeF
IF SEEK(m.tipbie+m.grubie+m.ClaBie+m.FamBie)
   mRet = descri
 ELSE
   mRet = "Sin Detalle"
ENDIF
SELE IteArt
RETURN mRet

FUNCTION Agrega
*--------------
SELECT iteart
*SCATTER MEMVAR

m.tipbie = tipbie
m.FecReg = m.fecsis
m.grubie = grubie
m.clabie = clabie
m.fambie = fambie
m.item   = SPACE(4)
m.codart = SPACE(15)
m.Descri = SPACE(135)
m.Menudeo = 'N'
m.coduni = SPACE(15)
m.fraccion = 1

DO pantalla2

@ 0,45 SAY m.fecreg
@ 1,14 SAY m.tipbie
@ 2,14 GET m.grubie VALID val_Grupo(m.grubie,m.TipBie,.t.,2,17)
@ 3,14 GET m.clabie VALID val_Clase(m.clabie,m.grubie,.t.,3,17)
@ 4,14 GET m.fambie VALID Val_Familia(m.fambie,m.grubie+m.clabie,.t.,4,19) AND val_ItArt()
@ 5,14 GET m.item   DISABLE
@ 6,14 GET m.CodArt DISABLE
@ 7,14 GET m.descri FUNCTION 'S50' VALID cUpper(m.descri)
@ 8,14 GET m.CodUni VALID cUpper(m.coduni)
@ 9,14 GET m.Menudeo PICTURE '@M S,N' VALID vMenudeo()
@10,14 GET m.Fraccion VALID m.fraccion >= 1 WHEN m.Menudeo = 'S'

READ VALID val_read()
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
          UNLOCK
     ENDIF
ELSE
     DO standby WITH 'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_2
SELECT iteart
FLUSH
DO vista
RETURN

PROCEDURE busca
*--------------
IF EOF()
    DO standby WITH vmens08
    RETURN
ENDIF
vtemp = RECNO()

DEFINE POPUP plista FROM 0, 5 COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT '\<a. Por Codigo      '
DEFINE BAR 2 OF plista PROMPT '\<b. Por Nombre      '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
cord1 = ORDER()
DO CASE
    CASE BAR() = 1
        vbusca = SPACE(15)
        vnombre = 'Codigo :'
        SET ORDER TO IteArt1
    CASE BAR() = 2
        vbusca = SPACE(30)
        vnombre = 'Nombre : '
        SET ORDER TO IteArt2
    OTHERWISE
        vbusca = ''
        vnombre = ''
        SET ORDER TO
ENDCASE
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 09, 12 TO 16, 68 FLOAT  ;
            TITLE ' °° B£squeda °° ' DOUBLE COLOR SCHEME 5
     ACTIVATE WINDOW lista
     @ 3, 2 SAY vnombre GET vbusca
     READ VALID val_read()
     DEACTIVATE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() = 27
ELSE
	IF ORDER()='iteArt1'
	    SEEK "B"+UPPER(ALLTRIM(vbusca))
	ELSE
	    SEEK UPPER(ALLTRIM(vbusca))
	ENDIF
    IF !FOUND()
        DO standby WITH vmens04
        GOTO vtemp
    ENDIF
ENDIF
SET ORDER TO &cOrd1
DO vista
RETURN

FUNCTION Corrige
*---------------
SELECT iteart
SCATTER MEMVAR

DO pantalla2

@ 0,45 SAY m.fecreg
@ 1,14 SAY m.tipbie
@ 2,14 SAY m.grubie 
@ 2,17 SAY val_Grupo(m.grubie,m.TipBie,.t.,2,17)
@ 3,14 SAY m.clabie
@ 3,17 SAY val_Clase(m.clabie,m.grubie,.t.,3,17)
@ 4,14 SAY m.fambie
@ 4,17 SAY Val_Familia(m.fambie,m.grubie+m.clabie,.t.,4,19)
@ 5,14 SAY m.item   
@ 6,14 SAY m.CodArt 
@ 7,14 GET m.descri FUNCTION 'S50' VALID cUpper(m.descri)
@ 8,14 SAY m.CodUni
@ 9,14 SAY m.Menudeo
@10,14 SAY m.Fraccion

READ VALID val_read()
IF LASTKEY() <> 27
    IF f_lock(1)
        GATHER MEMVAR
        UNLOCK
    ENDIF
 ELSE
    DO standby WITH 'Cancela Informacion'
ENDIF
DEACTIVATE WINDOW wind_2
SELECT iteart
FLUSH
DO vista
RETURN

PROCEDURE elimi
*--------------
SELECT iteart
IF EOF()
    DO standby WITH vmens08
    RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Art¡culo ?')
velimina = .T.
cart = codart
IF velimina
   USE IN 0 IteII ALIAS iteii ORDER IteII3
   SELECT iteii
   IF SEEK(cart)
     DO standby WITH 'Existe un Item con este articulo en Inv. Inicial. No se puede Eliminar este Articulo'
         velimina = .F.
      ELSE
         velimina = .T.
     ENDIF
     USE IN iteii
     IF velimina
         USE IN 0 IteGI ALIAS itegi ORDER IteGI2
         SELECT itegi
         IF SEEK(cart)
            DO standby WITH 'Existe un Item con este articulo en G/I. No se puede Eliminar este Articulo'
            velimina = .F.
          ELSE
            velimina = .T.
         ENDIF
         USE IN itegi
     ENDIF
     IF velimina
         USE IN 0 ItePeco ALIAS itepeco ORDER ItePeco2
         SELECT itepeco
         IF SEEK(cart)
            DO standby WITH 'Existe un Item con este articulo en Pecosas. No se puede Eliminar este Articulo'
            velimina = .F.
          ELSE
            velimina = .T.
         ENDIF
         USE IN itepeco
     ENDIF
     IF velimina
          USE IN 0 IteNea ALIAS itenea ORDER IteNea2
          SELECT itenea
          IF SEEK(cart)
             DO standby WITH 'Existe un Item con este articulo en G/I. No se puede Eliminar este Articulo'
             velimina = .F.
          ELSE
             velimina = .T.
          ENDIF
          USE IN itenea
     ENDIF
     
     IF velimina
         SELECT iteart
         SET ORDER TO IteArt1
         IF SEEK('B' + cart)
             DELETE NEXT 1
             SKIP -1
             IF BOF()
                 GOTO TOP
             ENDIF
         ENDIF
         SET ORDER TO ITEART2
      ELSE
         DO standby WITH 'No Puede Borrar este Articulo por encontrarse registrado en otro documento'
     ENDIF
ENDIF
SELECT iteart
UNLOCK ALL
DO vista
RETURN

FUNCTION cUpper
*--------------
PARAMETER ctexto
ctexto = UPPER(ctexto)
RETURN .T.

PROCEDURE vMenudeo
*-----------------
IF m.Menudeo = 'N'
	m.fraccion = 1
ENDIF
SHOW GET m.fraccion
RETURN

*PROCEDURE val_Grupo
**------------------
*PARAMETER _y
*PRIVATE mret
*mret = ''
*IF EMPTY(m.grubie)
*	mRet = val_fun('ArtMaeG','grubie','Descri',m.grubie)	&& ,1,2,11
*    mret = val_para(m.codgen, 'CODGEB','A',_y,30)
* ELSE
*    IF !SEEK('B' + m.grubie, 'ArtMaeG')
*        mret = val_fun('ArtMaeG','grubie','Descri',m.grubie)	&& ,1,2,11
*     ELSE
*        mret = val_fun('ArtMaeG','grubie','Descri',m.grubie)	&& ,1,2,11
*    ENDIF
*ENDIF
**wait wind mRet
**m.codgen = SUBSTR(parma.codigo,2,2)
*RETURN


*FUNCTION val_Grupo
*-----------------
PARAMETER xcod, filtro, _tipo, _x, _y
PRIVATE medita, mmsg, malias, v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 5) AND _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT ArtMaeG
_oldwnd = WOUTPUT()
*SET STEP ON
IF !medita
    SEEK xcod
    v_fun = IIF(FOUND(), descri, '')
 ELSE
    IF EMPTY(xcod)
        SET ORDER TO ArtMaeG1
        SET FILTER TO grubie=filtro
        ON KEY LABEL enter KEYBOARD CHR(23)
*        ON KEY LABEL f2 DO prvbus
        DEFINE WINDOW _xx FROM 3, 3 TO 22, 77
        BROWSE FIELDS tipbie :H='Tip',;
        			  grubie :H='Grpo',;
        			  descri :H='Descripci¢n':60;
        	   NOMENU NOAPPEND NOEDIT NODELETE ;
        	   WINDOW _xx;
        	   TITLE ' ®Enter¯ Selecciona  ®F3¯ Agregar  ®F2¯ Busca   ';
               NOLGRID
        ON KEY LABEL enter
        ON KEY LABEL f2
        SET FILTER TO
        RELEASE WINDOW _xx
        IF !EMPTY(_oldwnd)
           ACTIVATE WINDOW &_oldwnd
        ENDIF
        IF LASTKEY() = 27
            v_fun = .F.
         ELSE
            xcod = grubie
            IF mmsg
                @ _x, _y SAY LEFT(Descri,50)
            ENDIF
            SELECT (malias)
            IF  .NOT. _tipo
                REPLACE &_campo WITH xcod
            ENDIF
            v_fun = .T.
        ENDIF
     ELSE
        SEEK filtro+xcod
        IF mmsg .AND. FOUND()
            @ _x, _y SAY LEFT(descri,50)
        ENDIF
        v_fun = FOUND()
    ENDIF
ENDIF
SELECT (malias)
RETURN v_fun

*FUNCTION val_Clase
*-----------------
PARAMETER xcod,filtro, _tipo, _x, _y
PRIVATE medita, mmsg, malias, v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 5) AND _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT ArtMaeC
_oldwnd = WOUTPUT()
*SET STEP ON
IF !medita
    SEEK xcod
    v_fun = IIF(FOUND(), descri, '')
 ELSE
    IF EMPTY(xcod)
        SET ORDER TO ArtMaeC1
        SET FILTER TO grubie=filtro
        ON KEY LABEL enter KEYBOARD CHR(23)
*        ON KEY LABEL f2 DO prvbus
        DEFINE WINDOW _xx FROM 3, 3 TO 22, 77
        BROWSE FIELDS tipbie :H='Tip',;
        			  grubie :H='Grpo',;
        			  clabie :H='Clse',;
        			  descri :H='Descripci¢n':50;
        	   NOMENU NOAPPEND NOEDIT NODELETE ;
        	   WINDOW _xx;
        	   TITLE ' ®Enter¯ Selecciona  ®F3¯ Agregar  ®F2¯ Busca   ';
               NOLGRID
        ON KEY LABEL enter
        ON KEY LABEL f2
        SET FILTER TO
        RELEASE WINDOW _xx
        IF !EMPTY(_oldwnd)
           ACTIVATE WINDOW &_oldwnd
        ENDIF
        IF LASTKEY() = 27
            v_fun = .F.
         ELSE
            xcod = clabie
            IF mmsg
                @ _x, _y SAY LEFT(Descri,50)
            ENDIF
            SELECT (malias)
            IF  .NOT. _tipo
                REPLACE &_campo WITH xcod
            ENDIF
            v_fun = .T.
        ENDIF
     ELSE
        SEEK "B"+filtro+xcod
        IF mmsg .AND. FOUND()
            @ _x, _y SAY LEFT(descri,50)
        ENDIF
        v_fun = FOUND()
    ENDIF
ENDIF
SELECT (malias)
RETURN v_fun

*FUNCTION val_Familia
*-------------------
PARAMETER xcod,filtro, _tipo, _x, _y
PRIVATE medita, mmsg, malias, v_fun, _oldwind, _campo
medita = (PARAMETERS() >= 2)
mmsg = (PARAMETERS() = 5) AND _tipo
_campo = VARREAD()
malias = ALIAS()
SELECT ArtMaeF
_oldwnd = WOUTPUT()
*SET STEP ON
IF !medita
    SEEK xcod
    v_fun = IIF(FOUND(), descri, '')
 ELSE
    IF EMPTY(xcod)
        SET ORDER TO ArtMaeF1
        SET FILTER TO grubie+clabie=filtro
        ON KEY LABEL enter KEYBOARD CHR(23)
*        ON KEY LABEL f2 DO prvbus
        DEFINE WINDOW _xx FROM 3, 3 TO 22, 77
        BROWSE FIELDS tipbie :H='Tip',;
        			  grubie :H='Grpo',;
        			  clabie :H='Clse',;
        			  fambie :H='Fam.',;
        			  descri :H='Descripci¢n':50;
        	   NOMENU NOAPPEND NOEDIT NODELETE ;
        	   WINDOW _xx;
        	   TITLE ' ®Enter¯ Selecciona  ®F3¯ Agregar  ®F2¯ Busca   ';
               NOLGRID
        ON KEY LABEL enter
        ON KEY LABEL f2
        SET FILTER TO
        RELEASE WINDOW _xx
        IF !EMPTY(_oldwnd)
           ACTIVATE WINDOW &_oldwnd
        ENDIF
        IF LASTKEY() = 27
            v_fun = .F.
         ELSE
            xcod = fambie
            IF mmsg
                @ _x, _y SAY LEFT(Descri,50)
            ENDIF
            SELECT (malias)
            IF  .NOT. _tipo
                REPLACE &_campo WITH xcod
            ENDIF
            v_fun = .T.
        ENDIF
     ELSE
        SEEK "B"+filtro+xcod
        IF mmsg .AND. FOUND()
            @ _x, _y SAY LEFT(descri,50)
        ENDIF
        v_fun = FOUND()
    ENDIF
ENDIF
SELECT (malias)
RETURN v_fun

*FUNCTION val_Item
*----------------
PRIVATE malias
malias = ALIAS()
SELECT iteart
xord = ORDER()
SET ORDER TO IteArt1
SEEK m.tipbie+m.grubie+'.'+m.clabie+'.'+m.fambie
lagr = .F.
IF FOUND()
    xItem = 0
    SCAN WHILE iteart.tipbie+iteart.grubie+'.'+iteart.clabie=;
               m.tipbie + m.grubie + '.' + m.clabie
    	xItem= xItem + 1
    	IF xItem = VAL(item)
    	 ELSE
    	   m.Item = PADL(ALLTRIM(STR(xItem)),4,'0')
    	   lagr = .T.
    	   EXIT
    	ENDIF
    ENDSCAN
    IF !lagr
        m.Item = PADL(ALLTRIM(STR(xItem+1)),4,'0')
    ENDIF
 ELSE
    m.Item = '0001'
ENDIF
SET ORDER TO (xord)
m.tipbie = m.tipbie
m.codart = m.grubie + '.' + m.clabie + '.' + m.fambie + '.' + m.item
SHOW GET m.item
SHOW GET m.CodArt
RETURN .T.

