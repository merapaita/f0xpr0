*-------------------------------------------------------------------*
*-Ambiente
SET DATE DMY
SET TALK OFF
SET ECHO OFF
SET SAFETY OFF
SET DELETED ON
SET ESCAPE OFF
SET CLOCK ON
CLOSE DATA
SET CURSOR ON
*- Abre los archivos correspondientes
*- Abre los archivos correspondientes
close index
selec 1
USE IteArt SHARED
set order to IteArt3
selec 2
USE stock SHARED
set order to stockx
PUNTERO=0
selec 1
SET ORDER TO ITEART3
SET RELATION TO CodArt INTO stock ADDITIVE
SELEC 1
SET ORDER TO ITEART3
ON KEY LABEL F12 DO BUSCAW
*- Mensajes que utiliza el programa
#REGION 1
#DEFINE C_DBFEMPTY		'La base de datos est? vac?a. Desea agregar alg?n registro?'
#DEFINE C_EDITS			'Por favor, finalice su edici?n.'
#DEFINE C_TOPFILE		'Principio de archivo.'
#DEFINE C_ENDFILE		'Fin de archivo.'
#DEFINE C_BRTITLE		'Encontrar registro'
#DEFINE C_NOLOCK		'En este momento no se puede bloquear el registro, int?ntelo m?s tarde.'
#DEFINE C_ECANCEL		'Edici?n cancelada.'
#DEFINE C_DELREC		'Eliminar registros seleccionados ?'
#DEFINE C_DELNOTE 		'Eliminando registros...'
#DEFINE C_READONLY 		'La tabla es de s?lo lectura: no se permite su edici?n.'
#DEFINE C_NOTABLE 		'No hay ninguna tabla seleccionada. Abra una tabla o ejecute una consulta.'
#DEFINE C_BADEXPR		'Expresi?n no v?lida.'
#DEFINE C_PIA2000		'La fecha est? mal ingresada'
#DEFINE C_PRESIDENTE    'El Primer pase no puede ser modificado(PDTE)'
#DEFINE C_PRIFECHA		'La fechas no coinc?den, vuelva a digitarla' 
*- Declaraci?n de variables
PRIVATE reg_editado,nuevo_reg,dep_ant
m.reg_editado = .F.		&& Permite la edici?n en los campos de la tabla actual
m.nuevo_reg   = .F.		&& Permite saber si un registro se ha agregado
m.dep_Ant   = space(5)  && Guarda la dependencia anterior, comienza con TD (Tr?mite Doc.)
Public ART
ART=SPACE(11)
*- Define la ventana de registro de StockAli (ventana hijo)
DEFINE WINDOW W_StockAli FROM 14,0 TO 23,79 DOUBLE COLOR SCHEME 10
DEFINE WINDOW Wind_0  FROM 05,05 TO 18,70 DOUBLE  TITLE "AcTualizaci?n de Stock" COLOR SCHEME 16

GOTO BOTTOM
SCATTER MEMVAR 			&& Crea variables de memoria 

DO Ver_StockAli			&& Visualiza la pantalla de StockAli
DO inicia				&& Visualiza la pantalla padre (Reg. de Articulos)

RETURN
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
*			 Definici?n de Procedimientos y Funciones 					*
*-----------------------------------------------------------------------*

PROCEDURE Inicia
*-----------------------------------------------------------------------*
*	Define la ventana principal, todos los campos de la tabla 			*
*	padre (IteArt.dbf) y botones de control							*
*-----------------------------------------------------------------------*  	

DEFINE WINDOW W_IteArt FROM 0,0 TO 13,79 	DOUBLE COLOR SCHEME 14 ;
	TITLE "Registro de Articulos" 

#REGION 1
IF WVISIBLE("W_IteArt")
	ACTIVATE WINDOW W_IteArt SAME
ELSE
	ACTIVATE WINDOW W_IteArt NOSHOW
ENDIF

*- Visualiza todos los campos de la tabla "IteArt.dbf" 
DO pantalla 

*- Botones de control
@ 11,0 GET m.prim ;
	PICTURE "@*VN \<Primero" ;
	SIZE 1,9,1 ;
	DEFAULT 1;
	VALID valor_Btn('TOP') ;
	MESSAGE 'Primer registro'
@ 11,9 GET m.sigue ;
	PICTURE "@*VN \<Siguiente" ;
	SIZE 1,10,1 ;
	DEFAULT 1;
	VALID valor_Btn('NEXT') ;
	MESSAGE 'Siguiente Art?culo'
@ 11,20 GET m.anter ;
	PICTURE "@*VN \<Anterior" ;
	SIZE 1,10,1 ;
	DEFAULT 1;
	VALID valor_Btn('PREV') ;
	MESSAGE 'Art?culo anterior'
@ 11,30 GET m.ulti ;
	PICTURE "@*VN \<Ultimo" ;
	SIZE 1,8,1 ;
	DEFAULT 1;
	VALID valor_Btn('END') ;
	MESSAGE 'Ultimo Art?culo'
@ 11,38 GET m.encon ;
	PICTURE "@*HN B\<uscar" ;
	SIZE 1,11,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('LOCATE') ;
	MESSAGE 'Busca un Art?culo.'
@ 11,49 GET m.saldo ;
	PICTURE "@*HN S\<aldo" ;
	SIZE 1,10,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('SALDO') ;
	MESSAGE 'Presenta el Saldo de un Art?culo'
@ 11,59 GET m.editar ;
	PICTURE "@*HN T<ipo" ;
	SIZE 1,9,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('EDIT') ;
	MESSAGE 'Presenta el Art?culo por Tipo.'
@ 11,68 GET m.cerrar ;
	PICTURE "@*HN \<Cerrar" ;
	SIZE 1,10,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('EXIT') ;
	MESSAGE 'Cierra la pantalla.'
    SET CLOCK ON
IF NOT WVISIBLE("W_IteArt")
	ACTIVATE WINDOW W_IteArt
ENDIF

*- Lee la entrada del usuario 

READ CYCLE ;	
	ACTIVATE   ReadAct() ;
	DEACTIVATE ReadDeac() ;
	VALID .F. ;
	MODAL ;
	NOLOCK

RETURN

PROCEDURE readDeac
*-----------------
IF reg_editado
	ACTIVATE WINDOW 'W_IteArt'
    WAIT WINDOW C_EDITS NOWAIT
ENDIF
IF !WVISIBLE(WOUTPUT())
    CLEAR READ
    RETURN .T.
ENDIF
RETURN .F.

PROCEDURE readAct
*----------------
  IF !reg_editado
  	SELECT IteArt
  	SHOW GETS
*	_CUROBJ = OBJNUM(agregar)
  ENDIF

RETURN


PROCEDURE valor_Btn
*-------------------
PARAMETER m.nombre_btn

DO CASE
	CASE  m.nombre_btn='TOP'
		GO TOP
		WAIT WINDOW C_TOPFILE NOWAIT
	CASE  m.nombre_btn='PREV'
		IF !BOF()
			SKIP -1
			DO PANTALLA
	 	ENDIF
	 	IF BOF()
			WAIT WINDOW C_TOPFILE NOWAIT
			GO TOP
			DO PANTALLA
		ENDIF
	CASE  m.nombre_btn='NEXT'
		IF !EOF()
			SKIP 1
			DO PANTALLA
		ENDIF
		IF EOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			GO BOTTOM
			DO PANTALLA
		ENDIF
	CASE  m.nombre_btn='END'
		GO BOTTOM
		DO PANTALLA
		WAIT WINDOW C_ENDFILE NOWAIT
	CASE  m.nombre_btn='LOCATE'
		DO BUSCAW
	    DO PANTALLA2
	CASE  m.nombre_btn='SALDO'  				  			&& Muestra todos los Registros
		do Elegir	
	CASE m.nombre_btn='EXIT'  AND reg_editado 			&& Registro cancelado
		IF nuevo_reg
			=edithand('CANCEL')
		ENDIF
		reg_editado=.F.
		nuevo_reg=.F.
		UNLOCK
		WAIT WINDOW C_ECANCEL NOWAIT
		DO refresh
		_CUROBJ = OBJNUM(agregar)						
	CASE m.nombre_btn='EXIT'
		CLEAR READ
		RELEASE WINDOW W_IteArt
		RELEASE WINDOW W_StockAli
		CLOSE DATA
		SET CLOCK OFF
		CLEAR
		RETURN
ENDCASE
SCATTER MEMVAR 
SHOW GETS

RETURN

PROCEDURE pantalla
*-----------------
SET ORDER TO ITEART3

 @  0,2  SAY "Tipo Articulo   : " GET m.TipArt DISABLE
 @  0,40 SAY "Cod. Articulo   : "  GET  m.codArt color "G+/r" DISABLE
 @  1,2  SAY "Fecha Registro  : " GET m.Fecreg DISABLE
 @  1,40 SAY "Cantidad Inicial: " GET m.CantIni DISABLE
 @  2, 2 SAY "Cod. Generico   : " GET m.Codgen DISABLE
 @  3, 2 SAY "Descripci?n     : " GET m.Descri DISABLE
 @  4, 2 SAY "Cod. Clave      : " GET m.Codcla DISABLE
 @  5, 2 SAY "Cod. Detenc.    : " GET m.Coddet DISABLE
 @  6, 2 SAY "Cod. Unico      : " GET m.CodUni DISABLE
 @  7, 2 SAY "Precio Unitario : " GET m.PreUni DISABLE
 @  8, 2 SAY "Marca           : " GET m.Marca DISABLE
 @  9, 2 SAY "Cod. Contable   : " GET m.cod_Cont DISABLE
 @  10,2 SAY "Cod. Partida    : " GET m.CodPart DISABLE
 DO SUMA
RETURN

PROCEDURE pantalla2
*-----------------
SET ORDER TO ITEART3
 @  0,2  SAY "Tipo Articulo   : " GET m.TipArt DISABLE
 @  0,40 SAY "Cod. Articulo   : "  GET  m.codArt color "G+/r" DISABLE
 @  1,2  SAY "Fecha Registro  : " GET m.Fecreg DISABLE
 @  1,40 SAY "Cantidad Inicial: " GET m.CantIni DISABLE
 @  2, 2 SAY "Cod. Generico   : " GET m.Codgen DISABLE
 @  3, 2 SAY "Descripci?n     : " GET m.Descri DISABLE
 @  4, 2 SAY "Cod. Clave      : " GET m.Codcla DISABLE
 @  5, 2 SAY "Cod. Detenc.    : " GET m.Coddet DISABLE
 @  6, 2 SAY "Cod. Unico      : " GET m.CodUni DISABLE
 @  7, 2 SAY "Precio Unitario : " GET m.PreUni DISABLE
 @  8, 2 SAY "Marca           : " GET m.Marca DISABLE
 @  9, 2 SAY "Cod. Contable   : " GET m.cod_Cont DISABLE
 @  10,2 SAY "Cod. Partida    : " GET m.CodPart DISABLE

  
PROCEDURE edithand
*-----------------
PARAMETER accion
	* procedure handles edits
	DO CASE
	CASE m.accion = 'ADD'
		SCATTER MEMVAR BLANK
	CASE m.accion = 'SAVE'
		INSERT INTO IteArt FROM MEMVAR
	CASE m.accion = 'CANCEL'
		* nothing here
	ENDCASE
RETURN


PROCEDURE Ver_StockAli
*------------------
SELECT 2
seek Art
if !found()
*KEY m.CodArt
BROWSE WINDOW W_StockAli  NOEDIT NOAPPEND NOMENU NOWAIT;
	TITLE " REGISTRO DE STOCK ";
	FIELDs;
	CodArt      : H= 'C?digo',;
	Cantidad    : H= 'Cantidad' :P='99,999.99',;
   	Cosmed      : H= 'Uni'      :W=.F. :3,;
   	OBservA=substr(stock.OBservA,1,20) : H= 'Observaci?n',;
   	NumDoc      : H= 'N? Dcto.',;
   	Tipdoc      : H= 'Tipo'
else
	*ACTIVATE WINDOW W_StockAli
	*CLEAR
	*@ 05,25 SAY "No tiene detalle, Revise ..."
endif
SELECT IteArt
RETURN


PROCEDURE Elegir
*--------------
SELEC 1
SET ORDER TO ITEART3
ACTIVATE SCREEN
seek m.CodArt
vTempo = '??????????? ?F2? Busca      ?F11? Movimiento     ?Esc? para salir   ???????????'

BROWSE  NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
        WINDOW Wind_0 COLOR SCHEME 05 FIELDS ;
        iteart.codart      : H= 'Codigo' :P='!!!!!!!!!!' :W=.F.,;
        iteart.Descri      : H= 'Descripci?n' :44 :W=.F.,;
        iteart.Cantini     : H= 'Cantidad'   :P='99,999.99',;
        iteart.PreUni      : H= 'Precio'   :P='99,999.99' 
        
vTempo = '????????????????????????????????????????????????????????????????????????????????'
RETURN

PROCEDURE BUSCAW
*------------------
ON KEY LABEL F12
escolor = iscolor()
if escolor
   set color to ,n/w,b
   c_panta = 8
   c_borde = 10
   c_popup = 4  &&15
   c_texto = "N/W"
   c_fondo = 1
else
   c_panta = 8
   c_borde = 7
   c_popup = 3
   c_texto = "N+/W"
   c_fondo = 1
endif

*IF escolor
	DEFINE POPUP _mm FROM 16,54 
*ELSE
	DEFINE POPUP _mm FROM 16,54 
*ENDIF

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C?digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
DEFINE BAR 3 OF _mm PROMPT ' T\<odos los Art?culos '
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
		@01,02 SAY 'C?digo: ' GET _cod PICTURE '!!.!!!.!!!'
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
	CASE BAR()=3
			set order to IteArt3
			DEFINE WINDOW W_buscar COLOR SCHEME 05 FROM 0,0 TO 20,60 SYSTEM GROW CLOSE ZOOM FLOAT 
			MOVE WINDOW W_buscar CENTER
			BROWSE WINDOW W_buscar NOEDIT NODELETE 	NOMENU TITLE C_BRTITLE
			RELEASE WINDOW W_buscar

	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
on key label F2 do BuscaW
do suma2
RETURN

PROCEDURE SUMA
*---------------
Inicial=m.Cantini
SELEC 2
ENTRA=0
SALE=0
DO WHILE !EOF() 
	IF CODART=m.CodArt AND TIPDOC="O/C"
		ENTRA=ENTRA+cantidad
	ELSE
		IF CODART=m.CodArt AND TIPDOC="DESP"
			SALE= SALE+cantidad
		ENDIF
	ENDIF
	SKIP
ENDDO
ACTUAL=(Inicial+ENTRA)-SALE
@  7,50 SAY "ENTRADA :"+STR(ENTRA)
@  8,50 SAY "SALIDAS :"+STR(SALE)
@  9,50 SAY "ACTUAL  :"+STR(Actual)
SELEC 1
RETURN

PROCEDURE SUMA2
*--------------
Inicial=m.Cantini
SELEC 2
ENTRA=0
SALE=0
DO WHILE !EOF() 
	IF CODART=_cod AND TIPDOC="O/C"
		ENTRA=ENTRA+cantidad
	ELSE
		IF CODART=_Cod AND TIPDOC="DESP"
			SALE= SALE+cantidad
		ENDIF
	ENDIF
	SKIP
ENDDO
ACTUAL=(Inicial+ENTRA)-SALE
@  7,50 SAY "ENTRADA :"+STR(ENTRA)
@  8,50 SAY "SALIDAS :"+STR(SALE)
@  9,50 SAY "ACTUAL  :"+STR(Actual)

SELEC 1
RETURN


PROCEDURE vacio
*--------------
RETURN

