set clock off
*-------------------------------------------------------------------*
* 
*-------------------------------------------------------------------*
*-Ambiente
SET DATE DMY
SET TALK OFF
SET ECHO OFF
SET SAFETY OFF
SET DELETED ON
SET ESCAPE OFF
CLOSE DATA
SET CURSOR ON
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

*- Define la ventana de registro de pases (ventana hijo)
DEFINE WINDOW W_stock FROM 14,0 TO 23,79 DOUBLE COLOR SCHEME 10
DEFINE WINDOW W_stock2 FROM 14,0 TO 23,79 DOUBLE COLOR SCHEME 10
DEFINE WINDOW PANTA FROM 00,00 TO 13,79  DOUBLE ;
TITLE "HOLA" COLOR SCHEME 15

GOTO BOTTOM
SCATTER MEMVAR 			&& Crea variables de memoria 

DO Verstock  			&& Visualiza la pantalla de pases
DO inicia				&& Visualiza la pantalla padre (Reg. de Expedientes)

RETURN


*-----------------------------------------------------------------------*
*			 Definici¢n de Procedimientos y Funciones 					*
*-----------------------------------------------------------------------*

PROCEDURE Inicia
*-----------------------------------------------------------------------*
*	Define la ventana principal, todos los campos de la tabla 			*
*	padre (expedien.dbf) y botones de control							*
*-----------------------------------------------------------------------*  	

DEFINE WINDOW W_Iteartw FROM 0,0 TO 13,79 	DOUBLE COLOR SCHEME 01 ;
	TITLE "Registro de Articulos" 

#REGION 1
IF WVISIBLE("W_Iteartw")
	ACTIVATE WINDOW W_Iteartw SAME
ELSE
	ACTIVATE WINDOW W_Iteartw NOSHOW
ENDIF

*- Visualiza todos los campos de la tabla "Expedien.dbf" 


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
	MESSAGE 'Siguiente registro'
@ 11,20 GET m.anter ;
	PICTURE "@*VN \<Anterior" ;
	SIZE 1,10,1 ;
	DEFAULT 1;
	VALID valor_Btn('PREV') ;
	MESSAGE 'Registro anterior'
@ 11,30 GET m.ulti ;
	PICTURE "@*VN \<Ultimo" ;
	SIZE 1,8,1 ;
	DEFAULT 1;
	VALID valor_Btn('END') ;
	MESSAGE 'Ultimo registro'
@ 11,38 GET m.encon ;
	PICTURE "@*HN E\<ncontrar" ;
	SIZE 1,11,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('LOCATE') ;
	MESSAGE 'Busca un registro.'
@ 11,49 GET m.agregar ;
	PICTURE "@*HN S\<aldo" ;
	SIZE 1,10,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('ADD') ;
	MESSAGE 'Agrega un expediente nuevo.'
@ 11,59 GET m.editar ;
	PICTURE "@*HN T<ipo" ;
	SIZE 1,9,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('EDIT') ;
	MESSAGE 'Edita el registro actual.'
@ 11,68 GET m.cerrar ;
	PICTURE "@*HN \<Cerrar" ;
	SIZE 1,10,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('EXIT') ;
	MESSAGE 'Cierra la pantalla.'
IF NOT WVISIBLE("W_IteArtw")
	ACTIVATE WINDOW W_IteArtw
ENDIF

*- Lee la entrada del usuario 
READ CYCLE ;	
	VALID .F. ;
	MODAL ;
	NOLOCK

RETURN

PROCEDURE valor_Btn
*-------------------
PARAMETER m.nombre_btn
ON KEY LABEL F2 DO MOVIMIENTO
DO CASE
	CASE  m.nombre_btn='TOP'
		GO TOP
		do pantalla
		WAIT WINDOW C_TOPFILE NOWAIT
	CASE  m.nombre_btn='PREV'
		IF !BOF()
			SKIP -1
			do pantalla
	 	ENDIF
	 	IF BOF()
			WAIT WINDOW C_TOPFILE NOWAIT
			GO TOP
		ENDIF
	CASE  m.nombre_btn='NEXT'
		IF !EOF()
			SKIP 1
			do pantalla
		ENDIF
		IF EOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			GO BOTTOM
			do pantalla
		ENDIF
	CASE  m.nombre_btn='END'
		GO BOTTOM
		do pantalla
		WAIT WINDOW C_ENDFILE NOWAIT
	CASE  m.nombre_btn='LOCATE'
		DO Encontrar										&& Muestra una ventana browse
		
	CASE m.nombre_btn='EDIT'  AND reg_editado				&&  Guarda el registro editado
		IF nuevo_reg
			=edithand('SAVE')								
		ELSE
			GATHER MEMVAR 
		ENDIF
		UNLOCK
		reg_editado=.F.
		nuevo_reg=.F.
		DO Add_Pases    								&& Activa la ventana hijo para el registro de los pases
		DO verstock									&& Visualizamos los pases ingresados
		DO Act_Mov										&& Actualiza el los movimientos(pases)registrados de la tabla padre
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
		RELEASE WINDOW W_Iteartw
		RELEASE WINDOW W_stock
		CLOSE DATA
		
		CLEAR
		RETURN
ENDCASE
SCATTER MEMVAR 
SHOW GETS

RETURN


PROCEDURE pantalla
*-----------------
SET ORDER TO ITEART3
 @  0,2  SAY "Tipo Articulo   : " + m.TipArt
 @  0,40 SAY "Cod. Articulo : "  + m.codArt color "G+/r"
 @  1,2  SAY "Fecha Registro  : " + dtoc(m.Fecreg) 
 @  1,40 SAY "Cantidad Inicial: " + str(m.CantIni) 
 @  2, 2 SAY "Cod. Generico   : " + m.Codgen 
 @  3, 2 SAY "Descripci¢n     : " + m.Descri 
 @  4, 2 SAY "Cod. Clave      : " + m.Codcla 
 @  5, 2 SAY "Cod. Detenc.    : " + m.Coddet 
 @  6, 2 SAY "Cod. Unico      : " + m.CodUni
 @  7, 2 SAY "Precio Unitario : " + str(m.PreUni)
 @  8, 2 SAY "Marca           : " + m.Marca 
 @  9, 2 SAY "Cod. Contable   : " + m.cod_Cont 
 @  10,2 SAY "Cod. Partida    : " + m.CodPart 

PROCEDURE edithand
*-----------------
PARAMETER accion
	* procedure handles edits
	DO CASE
	CASE m.accion = 'ADD'
		SCATTER MEMVAR BLANK
	CASE m.accion = 'SAVE'
		INSERT INTO exp FROM MEMVAR
	CASE m.accion = 'CANCEL'
		* nothing here
	ENDCASE
RETURN


PROCEDURE Encontrar
*------------------
set order to Iteart3
ART=SPACE(11)
@09,21 TO 11,62 COLOR "GR+/R"
@10,22 SAY " Cod. Articulo :" get ART PICTURE "99.999.999"
READ
SEEK ART
	*PUNTERO=RECNO()
	clear
	*GO PUNTERO
	CLEAR
	DO PANTALLA
	DEFINE WINDOW W_buscar FROM 0,0 TO 20,60 SYSTEM GROW CLOSE ZOOM FLOAT 
	MOVE WINDOW W_buscar CENTER

	BROWSE WINDOW W_buscar NOEDIT NODELETE 	NOMENU TITLE C_BRTITLE
	RELEASE WINDOW W_buscar
return

PROCEDURE verstock
*------------------
hide popup all
SELECT 2
SET ORDER TO STOCKX
BROWSE WINDOW W_stock KEY m.CodArt  NOEDIT NOAPPEND NOMENU NOWAIT;
	TITLE "®F2¯ Ver Movientos del Articulo  ®Esc¯ Salir  ";
	FIELDs;
	stock.CodArt      : H= 'C¢digo',;
	stock.Cantidad    : H= 'Cantidad' :P='99,999.99',;
   	stock.Cosmed      : H= 'Uni'      :W=.F. :3,;
   	stock.OBservA=substr(stock.OBservA,1,20) : H= 'Observaci¢n',;
   	stock.NumDoc      : H= 'N§ Dcto.',;
   	stock.Tipdoc      : H= 'Tipo'
SELECT 1
*set order to ITEART3
RETURN


PROCEDURE Elegir
*--------------
IF LASTKEY() = 13
	DEACTIVATE POPUP P_descrip
ENDIF
RETURN

PROCEDURE readDeac
*-----------------
IF reg_editado
	ACTIVATE WINDOW 'W_IteArtw'
    WAIT WINDOW C_EDITS NOWAIT
ENDIF
IF !WVISIBLE(WOUTPUT())
    CLEAR READ
    RETURN .T.
ENDIF
RETURN .F.

PROCEDURE MOVIMIENTO
*-------------------
SELECT 2
SET ORDER TO STOCKX
BROWSE WINDOW W_stock2 KEY m.CodArt NOEDIT NOAPPEND NOMENU NOWAIT;
	TITLE "®F2¯ Ver Movientos del Articulo  ®Esc¯ Salir  ";
	FIELDs;
	stock.CodArt      : H= 'C¢digo',;
	stock.Cantidad    : H= 'Cantidad' :P='99,999.99',;
   	stock.Cosmed      : H= 'Uni'      :W=.F. :3,;
   	stock.OBservA=substr(stock.OBservA,1,20) : H= 'Observaci¢n',;
   	stock.NumDoc      : H= 'N§ Dcto.',;
   	stock.Tipdoc      : H= 'Tipo'
SELECT 1
set order to ITEART3
RETURN

PROCEDURE PANTALLA2
*------------------
 @  0,2  SAY "*Tipo Articulo   : " + TipArt
 @  0,40 SAY "Cod. Articulo : "  + codArt color "G+/r"
 @  1,2  SAY "Fecha Registro  : " + dtoc(Fecreg) 
 @  1,40 SAY "Cantidad Inicial: " + str(CantIni) 
 @  2, 2 SAY "Cod. Generico   : " + Codgen 
 @  3, 2 SAY "Descripci¢n     : " + Descri 
 @  4, 2 SAY "Cod. Clave      : " + Codcla 
 @  5, 2 SAY "Cod. Detenc.    : " + Coddet 
 @  6, 2 SAY "Cod. Unico      : " + CodUni
 @  7, 2 SAY "Precio Unitario : " + STr(PreUni)
 @  8, 2 SAY "Marca           : " + Marca 
 @  9, 2 SAY "Cod. Contable   : " + cod_Cont 
 @  10,2 SAY "Cod. Partida    : " + CodPart 

PROCEDURE vacio
*--------------
RETURN
