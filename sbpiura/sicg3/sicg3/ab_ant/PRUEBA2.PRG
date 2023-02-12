set clock on
*-------------------------------------------------------------------*
* Programa cuya funci¢n es registrar los expedientes                *
* PRUEBA.PRG                                                        *                                            
* Registro de expedientes                                           *                                            
* ELABORADO POR: JUAN MANUEL GIRON ALAMO                            *
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
close index

USE IteArt   IN 1   order tag Iteart3  ALIAS Iteartw  && item de articulos
USE STOCK    IN 0   order tag stockx   ALIAS StockAli
selec IteArtw
SET RELATION TO CodArt INTO StockAli ADDITIVE
*SELEC Iteartw
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
*- Declaraci¢n de variables
PRIVATE reg_editado,nuevo_reg,dep_ant
m.reg_editado = .F.		&& Permite la edici¢n en los campos de la tabla actual
m.nuevo_reg   = .F.		&& Permite saber si un registro se ha agregado
m.dep_Ant   = space(5)  && Guarda la dependencia anterior, comienza con TD (Tr mite Doc.)

*- Define la ventana de registro de pases (ventana hijo)
DEFINE WINDOW W_Stock FROM 14,0 TO 23,79 DOUBLE COLOR SCHEME 10

A=RECCOUNT()
GO A
*GOTO BOTTOM
SCATTER MEMVAR 			&& Crea variables de memoria 

DO Ver_Pases			&& Visualiza la pantalla de pases
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

DEFINE WINDOW W_Iteartw FROM 0,0 TO 13,79 	DOUBLE COLOR SCHEME 14 ;
	TITLE "Registro de Expedientes" 

#REGION 1
IF WVISIBLE("W_Iteartw")
	ACTIVATE WINDOW W_Iteartw SAME
ELSE
	ACTIVATE WINDOW W_Iteartw NOSHOW
ENDIF

*- Visualiza todos los campos de la tabla "Iteart.wdbf" 
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
	PICTURE "@*HN A\<gregar" ;
	SIZE 1,10,1 ;
	DEFAULT 1 ;
	VALID valor_Btn('ADD') ;
	MESSAGE 'Agrega un expediente nuevo.'
@ 11,59 GET m.editar ;
	PICTURE "@*HN E\<ditar" ;
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

IF NOT WVISIBLE("W_exp")
	ACTIVATE WINDOW W_iteartw
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
	ACTIVATE WINDOW 'W_Iteartw'
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
  	SELECT IteArtw
  	SHOW GETS
	_CUROBJ = OBJNUM(agregar)
  ENDIF
  DO REFRESH
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
	 	ENDIF
	 	IF BOF()
			WAIT WINDOW C_TOPFILE NOWAIT
			GO TOP
		ENDIF
	CASE  m.nombre_btn='NEXT'
		IF !EOF()
			SKIP 1
		ENDIF
		IF EOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			GO BOTTOM
		ENDIF
	CASE  m.nombre_btn='END'
		GO BOTTOM
		WAIT WINDOW C_ENDFILE NOWAIT
	CASE  m.nombre_btn='LOCATE'
		DO Encontrar										&& Muestra una ventana browse
	*CASE  m.nombre_btn='ADD'  AND !reg_editado  			&& Agrega un nuevo registro 
	*	reg_editado=.T.										
	*	nuevo_reg=.T.
	*	A=RECCOUNT()
	*	GO A
	*	*GO BOTTOM
	*	=edithand('ADD')   									&& Crea variables de memoria 
	*	m.numexp = PADL(ALLTRIM(STR(VAL(numexp)+1)),5,"0") 	&& Actualiza Correlativo
	*	_CUROBJ= OBJNUM(m.NumDoc)							&& Campo NumDoc (3)
	*	DO refresh											&& Actualizaci¢n de botones
	*	SHOW GETS
	*	RETURN
	CASE  m.nombre_btn='EDIT'  AND !reg_editado  			&& Edita un nuevo registro 
		IF EOF() OR BOF()
			WAIT WINDOW C_ENDFILE NOWAIT
			RETURN
		ENDIF
		*IF RLOCK()
		*	reg_editado=.T.
		*	_CUROBJ = OBJNUM(m.NumDoc)
		*	DO refresh
		*	RETURN
		*ELSE
		*			WAIT WINDOW C_NOLOCK
		*ENDIF
	CASE m.nombre_btn='EDIT'  AND reg_editado				&&  Guarda el registro editado
		IF nuevo_reg
			=edithand('SAVE')								
		ELSE
			GATHER MEMVAR 
		ENDIF
		UNLOCK
		reg_editado=.F.
		nuevo_reg=.F.
		DO refresh
		DO Add_Pases    								&& Activa la ventana hijo para el registro de los pases
		DO Ver_Pases									&& Visualizamos los pases ingresados
		*DO Act_Mov										&& Actualiza el los movimientos(pases)registrados de la tabla padre
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
		SET CLOCK OFF
		CLEAR
		RETURN
ENDCASE
SCATTER MEMVAR 
SHOW GETS

RETURN

PROCEDURE pantalla
*-----------------
selec STOCKALI
 @  1, 2 SAY "       Tipo Art. :"
 @  1,40 SAY "   Fecha Registro:"
 @  2, 2 SAY "   Cod. Articulo :"
 @  3, 2 SAY "   Cod. Generico :"
 @  4, 2 SAY "   Cod. Clave    :"
 @  5, 2 SAY "   Cod. Detenci¢n:"
 @  6, 2 SAY "   Descripci¢n   :"
 @  7, 2 SAY "   Cod. Unico    :"
 @  8, 2 SAY " Precio Unitario :"
 @  9, 2 SAY " Cantidad Inicial:"
 @  9,40 SAY "             Ntn :"
 @ 10, 2 SAY "   Cod. Contable :"
 @ 11, 2 SAY "    Cod. Partida :"
RETURN

PROCEDURE REFRESH
*----------------
  DO CASE
   CASE (RECCOUNT()=0 OR EOF()) AND !m.reg_editado
  	SHOW GETS DISABLE
	SHOW GET agregar  ENABLE
	SHOW GET cerrar   ENABLE
  CASE m.reg_editado
    SHOW GET prim     DISABLE
	SHOW GET sigue    DISABLE
	SHOW GET anter    DISABLE
	SHOW GET ulti     DISABLE
	SHOW GET encon    DISABLE
	SHOW GET agregar  DISABLE
	SHOW GET cerrar   DISABLE
	*SHOW GET m.p DISABLE
	*SHOW GET m.numMov DISABLE
	SHOW GET editar,1 PROMPT  "\<Guardar"
	SHOW GET cerrar,1  PROMPT  "\<Cancelar"
	ON KEY LABEL ESCAPE DO valor_Btn WITH 'EXIT'
	RETURN
  OTHERWISE
	SHOW GET editar,1 PROMPT "E\<ditar"
	SHOW GET cerrar,1  PROMPT "\<Cerrar"
	SHOW GETS ENABLE
  ENDCASE
  ON KEY LABEL ESCAPE
RETURN


PROCEDURE edithand
*-----------------
PARAMETER accion
	* procedure handles edits
	DO CASE
	CASE m.accion = 'ADD'
		SCATTER MEMVAR BLANK
	CASE m.accion = 'SAVE'
		INSERT INTO IteArtw FROM MEMVAR
	CASE m.accion = 'CANCEL'
		* nothing here
	ENDCASE
RETURN


PROCEDURE Encontrar
*------------------
index on codart to nn
codart=SPACE(11)
 	
@09,21 TO 11,62 COLOR "GR+/R"
@10,22 SAY " Ingrese el Codigo :" get CodArt
READ
SEEK CodArt
@09,21 clear 11,62
DEFINE WINDOW W_buscar FROM 0,0 TO 20,60 SYSTEM GROW CLOSE ZOOM FLOAT 
MOVE WINDOW W_buscar CENTER

BROWSE WINDOW W_buscar NOEDIT NODELETE 	NOMENU TITLE C_BRTITLE
RELEASE WINDOW W_buscar

RETURN

PROCEDURE Ver_pases
*------------------
SELECT stockAli
BROWSE WINDOW W_stock NOEDIT NOAPPEND NOMENU NOWAIT;
	TITLE " REGISTRO DE PASES ";
	FIELDs;
	stockAli.CodArt      : H= 'C¢digo',;
	stockAli.Cantidad    : H= 'Cantidad' :P='99,999.99',;
   	stockAli.Cosmed      : H= 'Uni'      :W=.F. :3,;
   	stockAli.OBservA     : H= 'Observaci¢n',;
   	stockAli.NumDoc      : H= 'N§ Dcto.',;
   	stockAli.Tipdoc      : H= 'Tipo'

SELECT IteArtw
RETURN

PROCEDURE Elegir
*--------------
IF LASTKEY() = 13
	DEACTIVATE POPUP P_descrip
ENDIF
RETURN

PROCEDURE PANTALLA
*------------------
selec IteArtw
 @  1, 2 SAY "       Tipo Art. :"+ IteArtw.TipArt
 @  1,40 SAY "   Fecha Registro:" +IteArtw.codArt
 @  2, 2 SAY "   Cod. Articulo :" +IteArtw.CodGen
 @  3, 2 SAY "   Cod. Generico :" +IteArtw.Codcla
 @  4, 2 SAY "   Cod. Clave    :" +IteArtw.Coddet
 @  5, 2 SAY "   Cod. Detenci¢n:" +IteArtw.Descri
 @  6, 2 SAY "   Descripci¢n   :" +IteArtw.Coduni
 @  7, 2 SAY "   Cod. Unico    :" +str(IteArtw.PreUni)
 @  8, 2 SAY " Precio Unitario :" +str(IteArtw.Cantini)
 @  9, 2 SAY " Cantidad Inicial:" +IteArtw.Marca
 @  9,40 SAY "             Ntn :" +IteArtw.Ntn
 @ 10, 2 SAY "   Cod. Contable :" +IteArtw.cod_Cont
 @ 11, 2 SAY "    Cod. Partida :" +IteArtw.CodPart

RETURN


PROCEDURE vacio
*--------------
RETURN
