*-------------------------------------------------------------------*
DEACTIVA WINDOW ALL
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
USE Iteart   IN 1   order tag IteArt3  ALIAS IteArt
USE stock    IN 2   order tag Stockx   ALIAS stock

PUNTERO=0
selec IteArt
SET ORDER TO ITEART3
SET RELATION TO CodArt INTO stock ADDITIVE
SELEC IteArt
SET ORDER TO ITEART3
*- Mensajes que utiliza el programa
#REGION 1
#DEFINE C_DBFEMPTY		'La base de datos est  vac¡a. Desea agregar alg£n registro?'
#DEFINE C_EDITS			'Por favor, finalice su edici¢n.'
#DEFINE C_TOPFILE		'Principio de archivo.'
#DEFINE C_ENDFILE		'Fin de archivo.'
#DEFINE C_BRTITLE		'Encontrar registro'
#DEFINE C_NOLOCK		'En este momento no se puede bloquear el registro, int‚ntelo m s tarde.'
#DEFINE C_ECANCEL		'Edici¢n cancelada.'
#DEFINE C_DELREC		'Eliminar registros seleccionados ?'
#DEFINE C_DELNOTE 		'Eliminando registros...'
#DEFINE C_READONLY 		'La tabla es de s¢lo lectura: no se permite su edici¢n.'
#DEFINE C_NOTABLE 		'No hay ninguna tabla seleccionada. Abra una tabla o ejecute una consulta.'
#DEFINE C_BADEXPR		'Expresi¢n no v lida.'
#DEFINE C_PIA2000		'La fecha est  mal ingresada'
#DEFINE C_PRESIDENTE    'El Primer pase no puede ser modificado(PDTE)'
#DEFINE C_PRIFECHA		'La fechas no coinc¡den, vuelva a digitarla' 
*- Declaraci¢n de variables
PRIVATE reg_editado,nuevo_reg,dep_ant
m.reg_editado = .F.		&& Permite la edici¢n en los campos de la tabla actual
m.nuevo_reg   = .F.		&& Permite saber si un registro se ha agregado
m.dep_Ant   = space(5)  && Guarda la dependencia anterior, comienza con TD (Tr mite Doc.)
Public ART
ART=SPACE(11)
*- Define la ventana de registro de StockAli (ventana hijo)
DEFINE WINDOW W_StockAli FROM 14,0 TO 23,79 DOUBLE COLOR SCHEME 10
DEFINE WINDOW Wind_0  FROM 05,05 TO 18,70 DOUBLE  TITLE "AcTualizaci¢n de Stock" COLOR SCHEME 16

GO TOP
*GOTO BOTTOM
SCATTER MEMVAR 			&& Crea variables de memoria 
ANDA1=CODART
SELEC stock
	SEEK ANDA1
	IF FOUND()
		DO Ver_StockAli
	ELSE
		DO NOHAY
	ENDIF
SELEC Iteart		
*DO NOHAY
*DO Ver_StockAli			&& Visualiza la pantalla de StockAli
DO inicia				&& Visualiza la pantalla padre (Reg. de Articulos)

RETURN
*-----------------------------------------------------------------------*
*-----------------------------------------------------------------------*
*			 Definici¢n de Procedimientos y Funciones 					*
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
	PICTURE "@*VN \<Primero " ;
	SIZE 1,9,1 ;
	DEFAULT 1;
	VALID valor_Btn('TOP') ;
	MESSAGE 'Primer registro'
@ 11,9 GET m.sigue ;
	PICTURE "@*VN \<Siguiente" ;
	SIZE 1,10,1 ;
	DEFAULT 1;
	VALID valor_Btn('NEXT') ;
	MESSAGE 'Siguiente Art¡culo'
@ 11,20 GET m.anter ;
	PICTURE "@*VN \<Anterior" ;
	SIZE 1,10,1 ;
	DEFAULT 1;
	VALID valor_Btn('PREV') ;
	MESSAGE 'Art¡culo anterior'
@ 11,30 GET m.ulti ;
	PICTURE "@*VN \<Ultimo" ;
	SIZE 1,8,1 ;
	DEFAULT 1;
	VALID valor_Btn('END') ;
	MESSAGE 'Ultimo Art¡culo'
@ 11,38 GET m.encon ;
	PICTURE "@*HN \<Buscar" ;
	SIZE 1,11,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('LOCATE') ;
	MESSAGE 'Busca un Art¡culo.'
@ 11,59 GET m.editar ;
	PICTURE "@*HN Imprimir" ;
	SIZE 1,9,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('IMPRIMIR') ;
	MESSAGE 'Presenta el Art¡culo por Tipo.'
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
ON KEY LABEL F9 DO ENTREMOV
DO CASE
	CASE  m.nombre_btn='TOP'
		GO TOP
		WAIT WINDOW C_TOPFILE NOWAIT
		UBICA=CodArt
		do pantalla
		DO DETALLE
	CASE  m.nombre_btn='PREV'
		IF !BOF()
			SKIP -1
			UBICA=CodArt
			DO PANTALLA
			DO DETALLE
		ENDIF
	 	IF BOF()
			WAIT WINDOW C_TOPFILE NOWAIT
			GO TOP
			DO PANTALLA
		ENDIF
	CASE  m.nombre_btn='NEXT'
		IF !EOF()
			SKIP 1
			UBICA=CodArt
			DO PANTALLA
			DO DETALLE
		ENDIF
		IF EOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			GO BOTTOM
			DO PANTALLA
		ENDIF
	CASE  m.nombre_btn='END'
		GO BOTTOM
		UBICA=CodArt
		DO PANTALLA
		WAIT WINDOW C_ENDFILE NOWAIT
		DO DETALLE
	CASE  m.nombre_btn='LOCATE'
		DO BUSCAW
		UBICA=CodArt
		DO PANTALLA2
		DO DETALLE
	CASE  m.nombre_btn='SALDO'  				  			&& Muestra todos los Registros
		UBICA=CodArt
		do Elegir
		DO DETALLE
	CASE  m.nombre_btn='IMPRIMIR'  				  			&& Muestra todos los Registros
		*UBICA=CodArt
		do impriX
		
	CASE m.nombre_btn='EXIT'  AND reg_editado 			&& Registro cancelado
		IF nuevo_reg
			=edithand('CANCEL')
		ENDIF
		reg_editado=.F.
		nuevo_reg=.F.
		UNLOCK
		WAIT WINDOW C_ECANCEL NOWAIT
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
ArtB=CodArt
Inicial=Cantini
activate WINDOW W_IteArt
 @  0,2  SAY "Tipo Articulo   : " GET TipArt DISABLE
 @  0,40 SAY "Cod. Articulo   : "  GET  codArt color "G+/r" DISABLE
 @  1,2  SAY "Fecha Registro  : " GET Fecreg DISABLE
 @  1,40 SAY "Cantidad Inicial: " GET CantIni DISABLE
 @  2, 2 SAY "Cod. Generico   : " GET Codgen DISABLE
 @  3, 2 SAY "Descripci¢n     : " GET descri DISABLE
 @  4, 2 SAY "Cod. Clave      : " GET Codcla DISABLE
 @  5, 2 SAY "Cod. Detenc.    : " GET Coddet DISABLE
 @  6, 2 SAY "Cod. Unico      : " GET CodUni DISABLE
 @  7, 2 SAY "Precio Unitario : " GET PreUni DISABLE
 @  8, 2 SAY "Marca           : " GET Marca DISABLE
 @  9, 2 SAY "Cod. Contable   : " GET cod_Cont DISABLE
 @  10,2 SAY "Cod. Partida    : " GET CodPart DISABLE
*--------------------------------------------------------
CLEAR
 @  0,2  SAY "Tipo Articulo   : "  + TipArt 
 @  0,40 SAY "Cod. Articulo   : " + codArt color "G+/r" 
 @  1,2  SAY "Fecha Registro  : " + DTOC(Fecreg )
 @  1,40 SAY "Cantidad Inicial: " + STR(CantIni)
 @  2, 2 SAY "Cod. Generico   : " + Codgen 
 @  3, 2 SAY "Descripci¢n     : " + SUBSTR(descri,1,50) 
 @  4, 2 SAY "Cod. Clave      : " + Codcla 
 @  5, 2 SAY "Cod. Detenc.    : " + Coddet 
 @  6, 2 SAY "Cod. Unico      : " + CodUni 
 @  7, 2 SAY "Precio Unitario : " + STR(PreUni)
 @  8, 2 SAY "Marca           : " + Marca 
 @  9, 2 SAY "Cod. Contable   : " + cod_Cont
 @  10,2 SAY "Cod. Partida    : " + CodPart 


SELEC stock
ENTRA=0
SALE=0
DO WHILE !EOF() 
	IF CODART=ArtB AND TIPDOC="O/C"
		ENTRA=ENTRA+cantidad
	ELSE
		IF CODART=ArtB AND TIPDOC="DESP"
			SALE= SALE+cantidad
		ENDIF
	ENDIF
	SKIP
ENDDO
@09,50 clear to 09,77
@  7,50 SAY "Entradas(E) :"+STR(ENTRA)
@  8,50 SAY "Salidas (S) :"+STR(SALE)
@  9,50 SAY "Stock Actual:"+STR((Inicial+ENTRA)-SALE)
SELEC IteArt
RETURN


PROCEDURE pantalla2
*-----------------
SET ORDER TO ITEART3
 @  0,2  SAY "Tipo Articulo   : " GET TipArt DISABLE
 @  0,40 SAY "Cod. Articulo   : "  GET  codArt color "G+/r" DISABLE
 @  1,2  SAY "Fecha Registro  : " GET Fecreg DISABLE
 @  1,40 SAY "Cantidad Inicial: " GET CantIni DISABLE
 @  2, 2 SAY "Cod. Generico   : " GET Codgen DISABLE
 @  3, 2 SAY "Descripci¢n     : " GET descri DISABLE
 @  4, 2 SAY "Cod. Clave      : " GET Codcla DISABLE
 @  5, 2 SAY "Cod. Detenc.    : " GET Coddet DISABLE
 @  6, 2 SAY "Cod. Unico      : " GET CodUni DISABLE
 @  7, 2 SAY "Precio Unitario : " GET PreUni DISABLE
 @  8, 2 SAY "Marca           : " GET Marca DISABLE
 @  9, 2 SAY "Cod. Contable   : " GET cod_Cont DISABLE
 @  10,2 SAY "Cod. Partida    : " GET CodPart DISABLE
 *----------------------------------------------------
 

PROCEDURE panta3
*------------------
*SET ORDER TO ITEART3
Artb=CodArt
Inicial=Cantini
 @  0,2  SAY "Tipo Articulo   : " GET TipArt DISABLE
 @  0,40 SAY "Cod. Articulo   : "  GET  codArt color "G+/r" DISABLE
 @  1,2  SAY "Fecha Registro  : " GET Fecreg DISABLE
 @  1,40 SAY "Cantidad Inicial: " GET CantIni DISABLE
 @  2, 2 SAY "Cod. Generico   : " GET Codgen DISABLE
 @  3, 2 SAY "Descripci¢n     : " GET descri DISABLE
 @  4, 2 SAY "Cod. Clave      : " GET Codcla DISABLE
 @  5, 2 SAY "Cod. Detenc.    : " GET Coddet DISABLE
 @  6, 2 SAY "Cod. Unico      : " GET CodUni DISABLE
 @  7, 2 SAY "Precio Unitario : " GET PreUni DISABLE
 @  8, 2 SAY "Marca           : " GET Marca DISABLE
 @  9, 2 SAY "Cod. Contable   : " GET cod_Cont DISABLE
 @  10,2 SAY "Cod. Partida    : " GET CodPart DISABLE

SELEC Stock
ENTRA=0
SALE=0
DO WHILE !EOF() 
	IF CODART=ArtB AND TIPDOC="O/C"
		ENTRA=ENTRA+cantidad
	ELSE
		IF CODART=ArtB AND TIPDOC="DESP"
			SALE= SALE+cantidad
		ENDIF
	ENDIF
	SKIP
ENDDO
@09,50 clear to 09,77
@  7,50 SAY "Entradas(E) :"+STR(ENTRA)
@  8,50 SAY "Salidas (S) :"+STR(SALE)
@  9,50 SAY "Stock Actual:"+STR((Inicial+ENTRA)-SALE)
SELEC IteArt
RETURN


  
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
SELECT Stock
seek Art
BROWSE NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT NOAPPEND NOMENU NOWAIT TIMEOUT 0.005 WINDOW W_StockAli ;
	TITLE "®F9¯ Ver movimientos    REGISTRO DE STOCK    ®Esc¯ Salir";
	FIELDs;
	CodArt      : H= 'C¢digo',;
	TipDoc 		: H= 'Dcto.',;
   	FechaMov    : H='Fecha Mov.',;
	Cosmed      : H= 'Precio'      :P='99,999.99',;
	Cant1=iif(TipoMov="E",cantidad,""): H= 'Entrada  ',;
	Cant2=iif(TipoMov="S",cantidad,""): H= 'Salida   ',;
   	NumDoc      : H= 'N§ Dcto.',;   	
    OBservA=substr(stock.OBservA,1,20) : H= 'Observaci¢n'
ON KEY LABEL F10
SELECT IteArt
RETURN

PROCEDURE NOHAY
*--------------
ACTIVATE WINDOW W_StockAli
CLEAR
@ 05,25 SAY "Art¡culo No Tiene Movimientos ..."
RETURN

PROCEDURE Elegir
*--------------
SELEC IteArt
SET ORDER TO ITEART3
ACTIVATE SCREEN
ENTRA=0
SALE=0
ArtP=CodArt
Ini=Cantini
seek CodArt

BROWSE  NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
        WINDOW Wind_0 COLOR SCHEME 05 FIELDS ;
        Codart      : H= 'Codigo' :P='!!!!!!!!!!' :W=.F.,;
        Descri      : H= 'Descripci¢n' :44 :W=.F.,;
        Cantini     : H= 'Cantidad'   :P='99,999.99',;
        PreUni      : H= 'Precio'   :P='99,999.99' 
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
RETURN


PROCEDURE BUSCAW
*------------------

DEFINE POPUP _mm FROM 16,54 

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
DEFINE BAR 3 OF _mm PROMPT ' T\<odos los Art¡culos '
ON SELECTION POPUP _mm DEACTIVATE POPUP
PRIVATE ORDEN
orden = ORDER()
_cod  = space(11)
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!.!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		DO VER_STOCKALI
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
*on key label F2 do BuscaW
DO PANTALLA
do suma2
RETURN

PROCEDURE SUMA2
*--------------
Inicial=Cantini
SELEC Stock
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
@09,50 clear to 09,77
@  7,50 SAY "Entradas(E) :"+STR(ENTRA)
@  8,50 SAY "Salidas (S) :"+STR(SALE)
@  9,50 SAY "Stock Actual:"+STR((Inicial+ENTRA)-SALE)
SELEC IteArt
RETURN



PROCEDURE SUMA
*---------------
Inicial=Cantini
SELEC Stock
ENTRA=0
SALE=0
DO WHILE !EOF() 
	IF CODART=CodArt AND TIPDOC="O/C"
		ENTRA=ENTRA+cantidad
	ELSE
		IF CODART=CodArt AND TIPDOC="DESP"
			SALE= SALE+cantidad
		ENDIF
	ENDIF
	SKIP
ENDDO
@09,50 clear to 09,77
@  7,50 SAY "Entradas(E) :"+STR(ENTRA)
@  8,50 SAY "Salidas (S) :"+STR(SALE)
@  9,50 SAY "Stock Actual:"+STR((Inicial+ENTRA)-SALE)
SELEC IteArt
RETURN


Inicial=Cantini
SELEC Stock
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
@09,50 clear to 09,77
@  7,50 SAY "Entradas(E) :"+STR(ENTRA)
@  8,50 SAY "Salidas (S) :"+STR(SALE)
@  9,50 SAY "Stock Actual:"+STR((Inicial+ENTRA)-SALE)

SELEC IteArt
RETURN

PROCEDURE SUMA3
*---------------
Inicial=Cantini
Cod1=CodArt
SELEC Stock
ENTRA=0
SALE=0
DO WHILE !EOF() 
	IF CODART=Cod1 AND TIPDOC="O/C"
		ENTRA=ENTRA+cantidad
	ELSE
		IF CODART=Cod1 AND TIPDOC="DESP"
			SALE= SALE+cantidad
		ENDIF
	ENDIF
	SKIP
ENDDO

@  7,50 SAY "Entradas(E) :"+STR(ENTRA)
@  8,50 SAY "Salidas (S) :"+STR(SALE)
@  9,50 SAY "Stock Actual:"+STR((Inicial+ENTRA)-SALE)

SELEC 1
RETURN


PROCEDURE ENTREMOV
*-----------------
BusMov=CodArt
UBICA=CodArt
SELECT 2
seek BusMov
if Found()
BROWSE key BusMov NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT NOAPPEND NOMENU NOWAIT TIMEOUT 0.005 WINDOW W_StockAli ;
	TITLE "®F9¯ Ver movimientos    REGISTRO DE STOCK    ®Esc¯ Salir";
	FIELDs;
	CodArt      : H= 'C¢digo',;
	TipDoc 		: H= 'Dcto.',;
   	FechaMov    : H='Fecha Mov.',;
	Cosmed      : H= 'Precio'      :P='99,999.99',;
	Cant1=iif(TipoMov="E",cantidad,""): H= 'Entrada  ',;
	Cant2=iif(TipoMov="S",cantidad,""): H= 'Salida   ',;
   	NumDoc      : H= 'N§ Dcto.',;   	
    OBservA=substr(stock.OBservA,1,20) : H= 'Observaci¢n'
	*DO Ver_StockAli
ElSE
	wait windows " No Se Han Registrado Movimientos " NOWAIT
EndIf
*do ver_stockali
SELECT IteArt
RETURN

PROCEDURE DETALLE
*----------------
selec 2
seek UBICA
	if Found()
		Activate windows w_stockali
		do ver_stockAli
		Else
			Activate windows w_stockali
			CLEAR
			@ 05,25 SAY "Art¡culo No Tiene Movimientos ..." 
			SET COLOR TO
	Endif
SELEC 1

PROCEDURE ImpriX
*-----------------
*DEACTIVATE WINDOW WIND_1
      DEFINE POPUP Lismenu FROM 17,54 
 

  DEFINE BAR 1 OF Lismenu PROMPT ' \<Art¡culo a la vista'
  DEFINE BAR 2 OF Lismenu PROMPT ' \<Total de Articulos '

  ON SELECTION POPUP Lismenu DEACTIVATE POPUP

  ACTIVATE POPUP Lismenu
  DO CASE
     CASE BAR()=1
          SELECT iteart
          set relation to codart into stock
          selec stock 
          set filter to  codart=m.codart
          IF !EOF()
             DO REPORTE WITH 2, "Imp2", " Lista de un Producto " ,1,.F.,.T.
          ELSE
             DO STANDBY WITH "Este Art¡culo No Tiene Movimientos (E/S)..."
          ENDIF
          SET FILTER TO
          *SET ORDER TO 1
     
     CASE BAR()=2
     	  SELECT iteart
     	  go top
          set relation to codart into stock
          Selec IteArt
          go top
		 If !eof()
		 	 DO REPORTE WITH 2, "Imp2", " Lista de un Producto " ,1,.F.,.T.
         EndIf
         SET FILTER TO
    
     OTHERWISE
     ENDCASE
  RELEASE POPUP LisMenu
  RETURN



PROCEDURE vacio
*--------------
RETURN

