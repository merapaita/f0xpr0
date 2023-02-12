**---------------------------------------------------------------------
* Programa					:	stock.prg
* Descrip					:	Permite consultar los stocks de almacen
** 20-07-2000
**---------------------------------------------------------------------
* ABRIENDO ARCHIVOS
USE IteArt IN 1  ORDER TAG IteArt2          ALIAS Iteart
USE STOCK  IN 2  ORDER TAG STOCKX           ALIAS STOCK
select iteart
mmaa='0000'
vfte='  '
on key label F8 do INVENTAR
on key label F2 do Busca
vCampo = mmaa+vfte
DEFINE WINDOW _INVEN    FROM 10,27 TO 15,77 DOUBLE FLOAT SHADOW
DEFINE WINDOW _INVFEC   FROM 02,27 TO 05,77 DOUBLE FLOAT SHADOW
deactivate window standby 
IF LASTKEY()=27
   CLOSE DATA
   RETU
ENDIF   
go top

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08

Vmens02 = ' Cat logo de Existencias '
Vmens01 = 'Actualizaci¢n de Stock de Art¡culos - A¤o: '+SUBSTR(dtoc(date()),7,4)
Vmens03 = 'C¢digo del Art¡culo: '
Vmens04 = 'Dicho Art¡culo no fue encontrado.'
Vmens05 = 'No existe Art¡culo anterior.'
Vmens06 = 'No existe Art¡culo siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Art¡culo?'
Vmens08 = 'No hay registros para procesar'

*- Variables de trabajo (registro a trabajar)
HIDE POPUP ALL
*- Inicia proceso
DO INVFINAL
IF LASTKEY()=27
	RETURN
ENDIF
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Vista
*- Activa men£ mientras vEn_accion es .T.
DEACTIVATE WINDOW Wind_0        
ACTIVATE SCREEN
DO Fin_opcion
RETURN

PROCEDURE INVFINAL
*--------------------
PUBLIC FECINI
fecini = SPACE(10)
ACTIVATE WINDOWS _INVFEC
@01,02 SAY 'Fecha Inventario: ' GET fecini PICTURE '@D' VALID !EMPTY(FECINI)
READ
DEACTIVA WINDOWS _INVFEC


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10
 
 *DEFINE WINDOW Wind_1 FROM 05,02 TO 19,77 DOUBLE ;
 *TITLE ' Listado Pecosas ' COLOR SCHEME 10
 
 ACTIVATE WINDOW Wind_0
 RETURN

PROCEDURE Vista
*---------------------
ACTIVATE SCREEN
vTempo = '°°°°°° ®F2¯ Busca      ®F8¯ Agregar Stock inicial  ®Esc¯ para salir   °°°°°°°'
DO Logos WITH Rotulo1,vTempo
sele iteart

BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
        WINDOW Wind_0 FIELDS ;
        codart   : H= 'Codigo' :P='!!!!!!!!!!' :W=.F.,;
        Descri   : H= 'Descripci¢n' :44 :W=.F.,;
        Coduni   : H= 'Unidad ':W=.F.,;
        Cantini  : H= 'Cantidad' :W=.F.,;
        PreUni   : H= 'Precio' :W=.F.
                
        *INV&vCampo. : H= 'total' ,;
        CodUni      : H= 'Unidad'   :10,;
        PreUni      : H= 'Precio'   :P='99,999.99' ,;
        Codpart     : H= 'Partida' :W=.F.
        *codgen      : H= 'GG' :P='!!' :W=.F. ,;
        codcla	    : H= 'Cls' :P='!!!' :W=.F.,;   
        coddet	    : H= 'Det' :P='!!!':W=.F. 

        
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vTemp = RECNO()
 DO FUNBUX
 IF EOF()
    DO standby WITH Vmens04
    GOTO Vtemp
 ELSE
*   DO Vista
 ENDIF

 RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F5
  ON KEY LABEL F2  
  ON KEY LABEL F8
  DEACTIVATE MENU
  RETURN

PROCEDURE Fin_opcion
*-------------------
  ON KEY LABEL F5
  ON KEY LABEL F2  
  DEACTIVATE MENU
*  RETURN

  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    MENU   nMenu
  RETURN

 FUNCTION BuscArt
 *----------------
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEART.CODART,10)
 *eek ALLTRIM(VCOD)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
 return vfun

 FUNCTION SiCto
 *-------------
 vReturn = .F.
 vAlias  = ALIAS()
 SELECT Parma
 SEEK "TIPEXI" + m.TipExi
 IF TRIM(UPPER(Parma.DescriAux)) = "P.TERMINADO"
   vReturn = .T.
 ENDIF
 SELECT (vAlias)
 RETURN vReturn

FUNCTION VALARTA
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_Art(_Cod,.T.,1,18)
IF XX
  vCod = Produ.Codart
  vFun = .T.
ENDIF

RETURN vFun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,YY,ZZ,vFun
vFun = .t.
AS=ALIAS()
if empty(_Cod)
   ZZ=VAL_PARA(_Cod,'CODGEB','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art1(COD,.F.)
      IF XX
         _COD=PRODU.CODART
         vFun = .T.
      ENDIF
   ENDIF
ELSE
   vFun = .t.
ENDIF
SELECT (AS)
unlock all
RETURN vFun


FUNCTION Val_Art1   && Articulos
*---------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo

_campo = VARREAD()

mAlias = ALIAS()
SELECT Produ
_OldWnd = WOUTPUT()

 SET ORDER TO 1
ACTIVATE WINDOW STANDBY
@ 1,14 SAY 'Espere un Momento ....' color W*
GO TOP
IF EOF()
   DEACTIVATE WINDOW STANDBY
   ACTIVATE SCREEN
   SET FILTER TO
   v_fun = .f.
ELSE
   SET ORDER TO 1
   SET FILTER TO SUBSTR(CODART,2,2)=substr(alltrim(parma.codigo),2,2)
   ACTIVATE WINDOW STANDBY
   @ 1,14 SAY 'Espere un Momento ....' color W*
   GO TOP
   DEACTIVATE WINDOW STANDBY
   ACTIVATE SCREEN
   ON KEY LABEL F10 KEYBOARD CHR(23)
   ON KEY LABEL F2 DO FunBus
   DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
   BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
          CodArt   :H='C¢digo'    :w=EMPTY(SUBSTR(CODART,5,3)),;
          Descri   :H='Nombre':60 :w=empty(descri)
   ON KEY LABEL F10
   ON KEY LABEL F2
   RELEASE WINDOW _BusArt
   SET FILTER TO

   IF !EMPTY(_OldWnd)
      ACTIVATE WINDOW &_OldWnd
   ENDIF

   IF Lastkey()=27
      V_FUN = .f.
   ELSE
      xcod = CodArt
      IF mMsg
         @ _x,_y SAY Descri
      ENDIF
      SELECT (mAlias)
      IF !_tipo
        *REPLACE &_campo WITH  xcod
        &_CAMPO = ALLTRIM(XCOD)
      ENDIF
      v_fun = .T.
  ENDIF
ENDIF
SET ORDER TO 1
SELECT (mAlias)
RETURN v_fun

FUNCTION funbuX
*--------------
ON KEY LABEL F2
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
PRIVATE ORDEN
orden = ORDER()
_cod  = space(11)
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		*_cod = '000.000.000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!.!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			*SET ORDER TO 1
			SET ORDER TO ITEART3
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO ITEART2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
on key label F2 do Busca
RETURN

PROCEDURE INVENTAR
*-----------------
ACTIVATE WINDOW _INVEN
*fecini = SPACE(10)
STORE 14 TO caI
STORE 14 TO PrI
CaI=iteart.cantini
PrI=iteart.PreUni
*@01,02 SAY 'Fecha Inventario: ' GET fecini PICTURE '@D'
@02,02 SAY "Cantidad : " get caI PICTURE '99999.999'
@03,02 SAY "Precio   : " get PrI PICTURE '99999.999'
READ
IF YESNO('¨ Esta Correcto el ingreso ?')
        
SELEC ITEART
codiI=iteart.codart
REPLA ITEART.PREUNI  WITH PrI
REPLA ITEART.CANTINI WITH caI
selec stock
delet for stock.codart=codiI and stock.tipdoc="INV"
appen blank
repla periodo  with substr(FECINI,9,2)
repla tipdoc   with "INV"
repla numdoc   with "INIC"
repla codart   with codiI
repla fuente   with "09"
repla tipomov  with "I"
repla fechamov with ctod(fecini)
repla cantidad with caI
repla cosmed   with PrI
ELSE
	RETURN
ENDIF
selec iteart
DEACTIVATE WINDOW _INVEN
