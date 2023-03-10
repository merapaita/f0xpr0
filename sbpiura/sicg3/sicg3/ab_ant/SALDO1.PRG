**---------------------------------------------------------------------
* Programa					:	
* Descrip					:	Permite consultar los stocks de almacen
** 19-AGO-94
**---------------------------------------------------------------------
* ABRIENDO ARCHIVOS
USE IteArt IN 3  ORDER TAG IteArt2          ALIAS Iteart
USE stock  IN 6  ORDER TAG StockX          ALIAS stock
select iteart
mmaa='0000'
vfte='  '
on key label F2 do Busca
**
vCampo = mmaa+vfte
deactivate window standby 
IF LASTKEY()=27
   CLOSE DATA
   RETU
ENDIF   
go top

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08

Vmens02 = ' Cat?logo de Existencias '
Vmens01 = 'Actualizaci?n del Stock Inicial - A?o: '+ substr(dtoc(date()),7,4)
Vmens03 = 'C?digo del Art?culo: '
Vmens04 = 'Dicho Art?culo no fue encontrado.'
Vmens05 = 'No existe Art?culo anterior.'
Vmens06 = 'No existe Art?culo siguiente.'
Vmens07 = '?Est? seguro que desea ELIMINAR ?ste Art?culo?'
Vmens08 = 'No hay registros para procesar'

*- Variables de trabajo (registro a trabajar)
HIDE POPUP ALL
*- Inicia proceso
DO Inicia                    && Define ventanas, men?s, t?tulos
DO Vista
*- Activa men? mientras vEn_accion es .T.
DEACTIVATE WINDOW Wind_0        

ACTIVATE SCREEN
DO Fin_opcion
RETURN

PROCEDURE Inicia             && Crea ventanas, men?s y t?tulos
*---------------

 DEFINE WINDOW Wind_3 FROM 20,63 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10
 
 DEFINE WINDOW Wind_1 FROM 05,02 TO 19,77 DOUBLE ;
 TITLE ' Listado Movimiento ' COLOR SCHEME 10
 
 ACTIVATE WINDOW Wind_0
 RETURN

PROCEDURE Vista
*---------------------
SET RELA TO
on key label F11 do MOVIMI
SELEC ITEART
SET ORDER TO ITEART3
select STOCK
SET RELATION TO CODART INTO ITEART
SELEC ITEART
ACTIVATE SCREEN
vTempo = '??????????? ?F2? Busca      ?F11? Momiento     ?Esc? para salir   ???????????'
DO Logos WITH Rotulo1,vTempo
*sele iteart
*SET ORDER TO ITEART3
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
        WINDOW Wind_0 FIELDS ;
        iteart.codart      : H= 'Codigo' :P='!!!!!!!!!!' :W=.F.,;
        iteart.Descri      : H= 'Descripci?n' :44 :W=.F.,;
        iteart.Cantini     : H= 'Cantidad'   :P='99,999.99',;
        iteart.PreUni      : H= 'Precio'   :P='99,999.99' 
        
vTempo = '????????????????????????????????????????????????????????????????????????????????'
DO Logos WITH Rotulo1,vTempo
RETURN

PROCEDURE MOVIMI
*----------------
Private malias
malias=alias()
vcod = alltrim(iteart.codart)
SELEC STOCK
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT ;
        WINDOW Wind_1 key vcod FIELDS ;
		STOCK.Periodo  : H = 'PERIODO' ,; 
		STOCK.Tipdoc   : H = 'TIPO',;
		STOCK.Numdoc   : H = 'N? DCTO.' ,;
		STOCK.Fechamov : H = 'FECHA' ,;
		STOCK.Cantidad : H = 'CANTIDAD' :p='99,999.99' ,;
		STOCK.Cosmed   : H = 'PRECIO' :p='99,999.99',;
		STOCK.Codart   : H = 'CODIGO'
SET RELA TO
SELEC ITEART
GO TOP
ON KEY LABEL F11

return

PROCEDURE Busca              && Realiza b?squeda directa
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
   *DO Vista
 ENDIF
 RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F5
  ON KEY LABEL F2  
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
   BROWSE WINDOW _BusArt TITLE '???? [F10] Selecciona   [F2] Buscar ????' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
          CodArt   :H='C?digo'    :w=EMPTY(SUBSTR(CODART,5,3)),;
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

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C?digo '
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
		@01,02 SAY 'C?digo: ' GET _cod PICTURE '!!!.!!!.!!!'
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
			SET ORDER TO 2
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

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
IF !EMPTY(STOCK.CANTIDAD) 
   @ 0,0 SAY STOCK.CANTIDAD picture '9,999,999.99'
else   
   @ 0,0 SAY STOCK.CANTIDAD picture '9,999,999.99'
endif   
return
DEACTIVATE WIND WIND_3
