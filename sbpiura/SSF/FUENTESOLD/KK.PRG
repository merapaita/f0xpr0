*--------------------------------------------------------------
* KKKK.Prg
* Corregir Opciones Del Kardex
* Autor: 
*-------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE itecob   IN 1  ORDER TAG itecob1  ALIAS itecob
USE KarSSF   IN 2  ORDER TAG KarSSF1  ALIAS KarSSF
*-
*- Mensajes de aviso al usuario
SELECT IteCob
SET RELATION TO CodCon INTO KarSSF
GO TOP
*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
*DO pantalla                  && Muestra pantalla inicial
DO vista
*DO fin_opcion

PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
*vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
*DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_1 FROM 01,00 TO 12,79  DOUBLE ;
	TITLE "Cobranzas" COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 13,00 TO 24,79 DOUBLE ;
	TITLE 'Kardexs ' ;
	COLOR SCHEME 10

RETURN


PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
ON KEY LABEL ESCAPE DO Nada
ON KEY LABEL F2  DO Bus_Con
ON KEY LABEL F10 DO fin_opcion
*ACTIVATE WINDOWS WIND_1
*ACTIVATE WINDOWS WIND_2
CLEAR

SELECT IteCob

BROWSE ;
		NOEDIT NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOWAIT;
		WINDOW wind_1 ;
		FIELDS;
		CodCon	: H= 'Contr.',;
		NumCob	: H= 'Corr.',;
		reccob	: H= 'Recibo',;
		Feccob	: H= 'Cuota',;
		FecPag	: H= 'Pag¢',;
		TIPCOB  : H= 'P/C',;
		codpro	: H= 'Promotor',;
		codcob	: H= 'Cobrador',;
		cancob  : H= 'Monto':P='99,999.99',;
		OBSERV	: H= 'Observaci¢n':40

SELECT KarSSF

BROWSE ;
		NOEDIT NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOWAIT;
		WINDOW wind_2 ;
		FIELDS;
		CodCon	: H= 'Contr.',;	
		Correl	: H= 'Correl',;	
		reccob	: H= 'Recibo',;	
		Feccob	: H= 'Cuota',;
		Fecha	: H= 'Pag¢',;
		TIPO    : H= 'C/S',;
		CanCob	: H= 'Cuota',;
		SdoCob	: H= 'Sd.Cob',;
		CtaCap	: H= 'C. C.',;
		SdoCC	: H= 'Sd. CC',;
		GtoAdm  : H= 'G. A.',;
		SdoGA	: H= 'SD. GA',;
		MtoSer	: H= 'Servicio'

SET PROCEDURE TO KK
*		:P='99,999.99',;
*KEY m.codcon
*DO vista_hijo
*do total
RETURN

PROCEDURE Nada
*-------------
RETURN

PROCEDURE fin_opcion
*-------------------
ON KEY LABEL ESCAPE
ON KEY LABEL F2
ON KEY LABEL F10
CLOSE DATA
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RESTORE SCREEN
*RESTORE SCREEN FROM principal
SET PROCEDURE TO Sai_lib
RETURN

PROCEDURE Bus_Con
*----------------
DEFINE WINDOW w_Bus FROM 9,10 TO 12,69  DOUBLE ;
	TITLE "Busca" COLOR SCHEME 10
ACTIVATE WIND w_Bus

*CLEAR
cCod = SPACE(6)
cRec = IIF(EOF(),-1,RECNO())
SELE IteCob
@ 1,0 SAY "Ingrese C¢digo : " GET cCod
READ

IF !SEEK(cCod)
	WAIT WIND "CODIGO NO ENCONTRADO....."
	IF cRec = -1
		GO BOOT
	ELSE
		GO vRec
	ENDIF
ENDIF

RELEASE WINDOW w_Bus

RETURN