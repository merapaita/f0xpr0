**---------------------------------------------------------------------
* Programa					:	
* Descrip					:	Permite consultar los stocks de almacen
** 19-AGO-94
**---------------------------------------------------------------------
* ABRIENDO ARCHIVOS
close data
USE IteArt IN 3  ORDER TAG IteArt2         ALIAS Iteart
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

Vmens02 = ' Cat logo de Existencias '
Vmens01 = 'Actualizaci¢n del Stock Inicial - A¤o: '+ substr(dtoc(date()),7,4)
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
on key label F11 do MOVIMI
SELEC ITEART
SET ORDER TO ITEART3
select STOCK
SET RELATION TO CODART INTO ITEART
SELEC ITEART
GO TOP
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°° ®F2¯ Busca      ®F11¯ Movimiento     ®Esc¯ para salir   °°°°°°°°°°°'
*DO Logos WITH Rotulo1,vTempo
BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
        WINDOW Wind_0 FIELDS ;
        iteart.codart      : H= 'Codigo' :P='!!!!!!!!!!' :W=.F.,;
        iteart.Descri      : H= 'Descripci¢n' :44 :W=.F.,;
        iteart.Cantini     : H= 'Cantidad'   :P='99,999.99',;
        iteart.PreUni      : H= 'Precio'   :P='99,999.99' 
        
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
*DO Logos WITH Rotulo1,vTempo
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
		STOCK.Numdoc   : H = 'N§ DCTO.' ,;
		STOCK.Fechamov : H = 'FECHA' ,;
		STOCK.Cantidad : H = 'CANTIDAD' :p='99,999.99' ,;
		STOCK.Cosmed   : H = 'PRECIO' :p='99,999.99',;
		STOCK.Codart   : H = 'CODIGO'
SET RELATION OFF INTO ITEART
ON KEY LABEL F11
*SELEC ITEART
*return

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
		SET ORDER TO (orden)
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
		SELEC ITEART
		SET ORDER TO ITEART2
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
*SET ORDER TO (orden)
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