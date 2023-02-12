**---------------------------------------------------------------------
** Sistema Region Grau                 Saiki Consultores
** Luis Castillo D
** 19-AGO-94
**---------------------------------------------------------------------
* ABRIENDO ARCHIVOS
USE ParMae IN 1  ORDER TAG ParMae1          ALIAS Parma
USE ArtMae IN 2  ORDER TAG ArtMae1          ALIAS Produ
USE IteArt IN 3  ORDER TAG IteArt2          ALIAS Iteart
USE Pecosa IN 4  ORDER TAG Pecosa1          ALIAS Pecosa
* USE stock  IN 5  ORDER TAG Stock2           ALIAS stock
USE OrdCom IN 6  order tag OrdCom1          ALIAS Orden
USE IteOc  IN 7  order tag IteOc1           ALIAS IteOc
USE Itepec IN 8  order tag ItePec10         ALIAS Itepec
USE Promae IN 9  order tag Promae1          ALIAS Promae


select iteart
on key label F11 do PECOSAS
on key label F8 do borrar
on key label F2 do Busca
IF LASTKEY()=27
   CLOSE DATA
   RETU
ENDIF   
go top

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08

Vmens02 = ' Cat logo de Existencias '
Vmens01 = 'Revisi¢n de Art¡culos '
Vmens03 = 'C¢digo del Art¡culo: '
Vmens04 = 'Dicho Art¡culo no fue encontrado.'
Vmens05 = 'No existe Art¡culo anterior.'
Vmens06 = 'No existe Art¡culo siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Art¡culo?'
Vmens08 = 'No hay registros para procesar'


*- Variables de trabajo (registro a trabajar)
HIDE POPUP ALL
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Vista
*- Activa men£ mientras vEn_accion es .T.
DEACTIVATE WINDOW Wind_0        
ACTIVATE SCREEN
DO Fin_opcion
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10
 
 DEFINE WINDOW Wind_1 FROM 05,02 TO 19,77 DOUBLE ;
 TITLE ' Detalle Precios ' COLOR SCHEME 10
 
 
 ACTIVATE WINDOW Wind_0
 RETURN



PROCEDURE Vista
*---------------------
ACTIVATE SCREEN
vTempo = '°°°°°°° ®F2¯ Busca °° ®F8¯ Elimina °° ®F11¯ Detalle °° ®Esc¯ para salir °°°°°°'
DO Logos WITH Rotulo1,vTempo
sele iteart

BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
        WINDOW Wind_0 FIELDS ;
        codart      : H= 'Codigo' :P='!!!!!!!!!!' :W=.F.,;
        Descri      : H= 'Descripci¢n' :55 :W=.F.,;
        CodUni      : H= 'Unidad' :W=.F.     :10,;
        PreUni      : H= 'Precio'   :P='99,999.99':W=.F. ,;
        Codpart     : H= 'Partida' :W=.F.

*       codgen      : H= 'GG' :P='!!' 
*       codcla	    : H= 'Cls' :P='!!!'
        
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
RETURN

PROCEDURE PECOSAS
*----------------
Private malias
malias=alias()
vcod = alltrim(iteart.codart)
as= recno()
ON KEY LABEL F11
select itepec
SET RELATION TO PERIODO+NUMoc+newFTE INTO orden
SET RELATION TO PERIODO+NUMOC+NEWFTE INTO ITEOC ADDI
*ET RELATION TO PERIODO+NUMPEC+CODFTE INTO ITEPEC ADDI
SEEK vcod 
IF !EOF() AND !EMPTY(ITEART.CODART)
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT ;
        WINDOW Wind_1 key vcod FIELDS ;
		codfte : H = 'FTE' ,;
		numpec : H = 'N§PC',;
		canreq : H = 'Pedido' :p='99,999.99' ,;
		coduni : H = 'Unid' :4 ,;
		preuni : H = 'PreUni' :p='99,999.99',;
		XX = PREUNI*CANREQ : H = 'Total' :p='99,999.99',;
		numoc  : H = 'NumOc' ,;
		ITEOC.FECOC : H = 'FecOc' ,;
		dd=val_prv(orden.Codprv) : H = 'Proveedor' :60
ELSE
   DO STANDBY WITH 'No se registra Compra'
ENDIF		
sele (malias)
SET RELA TO
on key label F11 do PECOSAS
return

PROCEDURE borrar
*----------------
Private malias
malias=alias()
vcod = alltrim(iteart.codart)
ON KEY LABEL F8
ON KEY LABEL F11
select itepec
SET RELATION TO PERIODO+NUMoc+newFTE INTO orden
SET RELATION TO PERIODO+NUMOC+NEWFTE INTO ITEOC ADDI
SEEK vcod 
IF !EOF() AND !EMPTY(ITEART.CODART)
    DO STANDBY WITH 'Ya gener¢ Pe.Co.Sa.'
	sele (malias)
	SET RELA TO
ELSE
   select (malias)
   if yesno('¨Desea Eliminar este Item?')
      SCATTER MEMVAR
      SELE BAK     
      IF F_APPD() AND !EMPTY(ITEART.CODART)
         m.fechadel = date()
  		 m.user = sys(0)
         GATHER MEMVAR
      ENDIF
      select (malias)   
      if rlock()
         delete next 1
      endif
   endif      
ENDIF		
On key label F11 do PECOSAS
on key label F8 do borrar
return

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
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  ON KEY LABEL F5
  ON KEY LABEL F2  
  on key label F11 
  on key label F8
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
 seek 'B'+LEFT(ITEART.CODART,6)
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
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '000.000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 1
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
