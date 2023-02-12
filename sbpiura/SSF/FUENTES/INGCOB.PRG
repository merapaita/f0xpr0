* corregir la opcion ingresa para las cobranzas
*--------------------------------------------------------------
* InsSoc.Prg
* Registra Socios
* Autor: Analista Segundo Correa Sandoval
* Corregido : Analista Marco Rodriguez Abad
*-------------------------------------------------------------
*- Abriendo Archivos
CLOS DATA
USE parmae   IN 1  ORDER TAG parmae1  ALIAS parma
USE maesoc   IN 2  ORDER TAG maesoc1  ALIAS socio
USE itecob   IN 3  ORDER TAG itecob1  ALIAS itecob
USE maepro   IN 4  ORDER TAG maepro1  ALIAS promo
USE maecob   IN 5  ORDER TAG maecob1  ALIAS cobra
USE KarSSF   IN 6  ORDER TAG KarSSF1  ALIAS KarSSF

*-
*- Mensajes de aviso al usuario
vmens01 = 'Registro de Socios'
vmens02 = ' Socios : COBRANZA '
vmens04 = 'Dicho Socio no fue encontrado'
vmens05 = 'No existe Socio anterior'
vmens06 = 'No existe Socio siguiente'
vmens07 = '¨ Desea Anular ‚ste Socio ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Socio ha sido anulado'
SELECT socio
GO BOTT
ON KEY LABEL F7 DO VISTA_DET
*- Variables de trabajo (registro a trabajar)
*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mmenu
ENDDO

DO fin_opcion

RETURN


PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 12,79  DOUBLE ;
	TITLE vmens02 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Socios        ®F7¯ Detalle : Item ' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_2a FROM 01,00 TO 23,79 DOUBLE ;
	TITLE 'Detalle: Socios         ®F10¯ Salir ' ;
	COLOR SCHEME 10

DEFINE WINDOW Wind_3 FROM 20,64 TO 22,78 ;
 TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
*DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
*DEFINE PAD elimi   OF mmenu PROMPT '\<Eliminar'   AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
*ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
*ON SELECTION PAD elimi  OF mmenu DO elimi
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi
RETURN


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR

 @  1, 2 SAY "  N§ del Contrato :"
 @  2, 2 SAY "          N§ L.E. :"
 @  2,40 SAY "      Instituci¢n :"
 @  3, 2 SAY "        Apellidos :"
 @  4, 2 SAY "          Nombres :"
 @  5, 2 SAY "        Direcci¢n :"
 @  6, 2 SAY "         Tel‚fono :"
 @  7, 2 SAY "             Edad :"
 @  8, 2 SAY "  Codigo Promotor :"
 @  9, 2 SAY "Fecha Inscripci¢n :"
 @  9,40 SAY "      Inscripci¢n :"
 @ 10, 2 SAY "         Comisi¢n :"
 @ 10,40 SAY "    Cuota Mensual :"

RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT socio
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
* @  0, 60 SAY Verest(m.estado)
 @  0, 60 SAY Verest(m.estado) COLOR SCHEME 5
 @  1, 22 SAY m.codcon
 @  2, 22 SAY m.libele
 @  2, 60 SAY val_para(m.codins,'INSTIT','V',60,18)
 @  3, 22 SAY m.apelli
 @  4, 22 SAY m.Nombre
 @  5, 22 SAY m.Direcc
 @  6, 22 SAY m.Telefo
 @  7, 22 SAY m.Edad 
 @  8, 22 SAY Val_fun('promo','codigo',"codigo+' '+apelli",m.codpro,3,8,22)
 @  9, 22 SAY m.fecins
 @  9, 60 SAY m.canins	PICTURE '99,999.99'
 @ 10, 22 SAY m.cancom	PICTURE '99,999.99'
 @ 10, 60 SAY m.cuotas	PICTURE '99,999.99'
DO vista_hijo
do total
RETURN

PROCEDURE vista_hijo
*-------------------
SELECT iteCOB

SEEK m.CODCON
IF FOUND()
	BROWSE ;
		NOEDIT NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.codcon TIMEOUT 0.0001 ;
		WINDOW wind_2 ;
		FIELDS;
		reccob	: H= 'N§ Recibo',;	
		Feccob	: H= 'Cuota',;
		FecPag	: H= 'Pag¢',;
		TIPCOB  : H= 'P/C',;
		codpro	: H= 'Promotor',;
		codcob	: H= 'Cobrador',;
		cancob  : H= 'Monto':P='99,999.99',;
		OBSERV	: H= 'Observaci¢n':40
Else
	ACTIVATE WINDOWS WIND_2
	CLEAR
	@ 4,30 say 'No hay Pagos'
endif
SELE socio
RETURN

PROCEDURE vista_det
*------------------
SELECT itecob
SEEK SOCIO.CODCON
vtempo = '[ESC] Terminar'
ON KEY LABEL F7
HIDE POPUP ALL
IF FOUND()
	BROWSE ;
		NOEDIT NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY SOCIO.codcon;
		WINDOW wind_2a TITLE vtempo ;
		FIELDS;
		reccob	: H= 'N§ Recibo',;	
		Feccob	: H= 'Cuota',;
		FecPag	: H= 'Pag¢',;
		TIPCOB  : H= 'P/C',;
		codpro	: H= 'Promotor',;
		codcob	: H= 'Cobrador',;
		cancob  : H= 'Monto':P='99,999.99',;
		OBSERV	: H= 'Observaci¢n':40
Else
	ACTIVATE WINDOWS WIND_2a
	CLEAR
	@ 4,30 say 'No hay Afiliados'
endif
ON KEY LABEL F7 DO VISTA_DET
SHOW MENU mmenu
SELE socio
DO VISTA
RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,0 SAY m.TOTCOB picture '99,999,999.99'
return


PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE socio
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SET RELATION TO codcon INTO iteCOB
SET SKIP TO itecob
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	socio.codcon    :H='Per',;
	socio.apelli    :H='Apellido',;
	socio.nombre    :H='Nombre',;	
	itecob.reccob	: H= 'N§ Recibo',;	
	itecob.Feccob	: H= 'Fecha',;	
	itecob.FecPag	: H= 'Pag¢',;
	itecob.codcob	: H= 'Cobrador',;
	itecob.cancob  : H= 'Monto':P='99,999.99'

vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SET RELA TO
SELE socio
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
SELE socio
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°®F09¯C¢digo °°°®F10¯Selecciona°°°®F11¯Apellido°°° ®F12¯L.E. °°®Esc¯Cancela °°°'
DO logos WITH rotulo1,vtempo
*SET RELATION TO codcon INTO itesoc
*SET SKIP TO itesoc
HIDE MENU mmenu
ACTIVATE SCREEN
ON KEY LABEL F9 DO Buscodsoc
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F11 DO Busnomsoc
ON KEY LABEL F12 DO Buslesoc
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	socio.codcon    :H='N§ Contrato',;
	socio.apelli    :H='Apellido',;
	socio.nombre    :H='Nombre',;
	socio.libele    :H='L.E.'
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SELE socio
SET ORDER TO maesoc1
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F9
ON KEY LABEL f10
ON KEY LABEL F11 
ON KEY LABEL F12 
DO vista
RETURN

PROCEDURE anter
*--------------
SELE socio
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH vmens05
ELSE
	DO vista
ENDIF
RETURN


PROCEDURE proxi
*--------------
SELE socio
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH vmens06
	GO BOTTOM
ELSE
	DO vista
ENDIF
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
SELECT SOCIO
op=ORDER()
vtemp = RECNO()
DO pantalla
SELECT SOCIO
SCATTER MEMVAR
 @  0, 60 SAY Verest(m.estado) COLOR SCHEME 5
 @  1, 22 SAY m.codcon
 @  2, 22 say m.libele	PICTURE '99999999'	
 @  2, 60 SAY val_para(m.codins,'INSTIT','V',60,18)
 @  3, 22 say m.apelli	PICTURE '@!'		
 @  4, 22 say m.Nombre	PICTURE '@!'		
 @  5, 22 say m.Direcc 	PICTURE '@S50!'
 @  6, 22 say m.Telefo
 @  7, 22 say m.Edad 
 @  8, 22 say Val_fun('promo','codigo',"codigo+' '+apelli",m.codpro,3,8,22)
 @  9, 22 say m.fecins
 @  9, 60 SAY m.canins	PICTURE '99,999.99'
 @ 10, 22 say m.canins	PICTURE '99,999.99'
 @ 10, 60 SAY m.cuotas	PICTURE '99,999.99'
IF LASTKEY()=27
	IF !EOF() AND !BOF()
		go vtemp
		DO vista
	ENDIF	
	RETURN
 Else
	DO WHILE .T.
		ok = trabaja_hijo()
		IF LASTKEY() # 27 AND OK
			IF yesno('¨ Confirme el ingreso ?')
				ok=.T.
				EXIT
			ENDIF
		ELSE
			IF yesno('¨ Cancela el ingreso ?')
				DO standby WITH ' Cancelado el Ingreso ..'
				ok = .F.
				EXIT
			ELSE
				LOOP
			ENDIF
		ENDIF
	ENDDO
	IF ok AND LASTKEY()#27
		SELECT socio
*		IF M.TOTCOB>0
*			REPLACE ESTADO WITH '10'
*		ENDIF	
		REPLACE TOTCOB WITH M.TOTCOB
	ELSE
		SELE itecob
		SEEK m.codcon
		SCAN WHILE codcon=m.codcon
			IF RLOCK()
*				DELETE NEXT 1
			ENDIF
		ENDSCAN
		GO VTEMP
	ENDIF
ENDIF
UNLOCK ALL
SELECT socio
do pantalla
DO vista
RETURN

PROCEDURE trabaja_hijo
*---------------------
vsun = .T.
PUBLIC ak
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°F2->Corrige°°°°°°°F5->Agregar°°°°°°°°F8->Eliminar°°°°°°°°F10->Terminar°°°°°°'

DO logos WITH rotulo1,vtempo
ON KEY LABEL F2  DO edita_item
ON KEY LABEL F5  DO agrega_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itecob
SEEK m.codcon
IF !FOUND()
	DO agrega_item
ENDIF
BROWSE ;
	NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOMODIFY KEY m.codcon ;
	WINDOW wind_2 ;
	FIELDS;
		NumCob	: H= 'Ite',;
		reccob	: H= 'N§ Recibo',;
		Feccob	: H= 'Cuota',;
		FecPag	: H= 'Pag¢',;
		TIPCOB  : H= 'P/C':P="@M C,P",;
		codpro	: H= 'Promotor',;
		codcob	: H= 'Cobrador',;
		cancob  : H= 'Monto':P='99,999.99' ,;
		OBSERV	: H= 'Observaci¢n':40

SELECT ITECOB
SEEK m.codcon
m.Totcob = 0
SCAN WHILE codcon = m.codcon
	m.Totcob = m.Totcob + CANCOB
    IF CANCOB=0
       if rlock()
          delete next 1
       endif
       unlock
    endif
ENDSCA

ON KEY LABEL f2
ON KEY LABEL f5
ON KEY LABEL f8
ON KEY LABEL f10
ACTIVATE SCREEN
SHOW MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SELECT socio
RETURN vsun

PROCEDURE Agrega_Item
*--------------------
DEFINE WINDOW Wind_4 FROM 01,01 TO 14,48 DOUBLE ;
TITLE "Adici¢n de Pagos" COLOR SCHEME 10

ACTIVATE WINDOW Wind_4
**- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

SELE IteCob

SET ORDER TO IteCob1

IF SEEK(m.Codcon)
	lProc = .T.
	SCAN WHILE Codcon = m.Codcon
		m.NumCob = NumCob
		IF !isdigit(m.numcob)
			lProc = .F.
		ENDIF
	ENDSCAN
	
	IF lProc
		m.NumCob = PADL(ALLTRIM(STR(VAL(m.NumCob)+1)),4,'0')
	ELSE
		DO StandBy WITH "Error en correlativo de cobranzas"
		DEACTIVATE WINDOW Wind_4
		RELEASE    WINDOW Wind_4
		
		ON KEY LABEL F2  DO Edita_item
		ON KEY LABEL F5  DO agrega_item
		ON KEY LABEL F8  DO elimi_item
		ON KEY LABEL F10 KEYBOARD CHR(23)
		RETURN
	ENDIF
 ELSE
	m.NumCob = '0001'
ENDIF

SET ORDER TO IteCob4

m.FecCob = DATE()
m.FecPag = DATE()
m.RecCob = SPACE(6)
m.TipCob = SPACE(1)
m.CodPro = SPACE(6)
m.CodCob = SPACE(6)
m.CanCob = m.Cuotas
m.Observ = SPACE(80)
m.GtoAdm = 0
m.CtaCap = 0 

@  0,1  SAY '        Contrato: ' + m.Codcon
@  1,1  SAY '  Corr. Cobranza: ' + m.NumCob
@  2,1  SAY ' Recibo Cobranza:' GET m.RecCob
@  3,1  SAY '  Cuota Cobranza:' GET m.FecCob
@  4,1  SAY '      Fecha Pago:' GET m.FecPag
@  5,1  SAY '    Cob. / Prom.:' GET m.TipCob	PICTURE "@M C,P"
@  6,1  SAY '        Promotor:' GET m.CodPro	VALID Val_fun('promo','codigo',"codigo+' '+apelli",m.codpro,1 ) AND vcodigo() WHEN m.TipCob = 'P'
@  7,1  SAY '        Cobrador:' GET m.CodCob	VALID Val_fun('cobra','codigo',"codigo+' '+apelli",m.codcob,1 ) AND vcodigo() WHEN m.TipCob = "C"
@  8,1  SAY '           Monto:' GET m.CanCob	PICTURE '99,999.99'		WHEN Val_CC()
@  9,1  SAY ' Gasto Administ.:' GET m.GtoAdm	PICTURE '99,999.99'
@ 10,1  SAY '   Cuota Capital:' GET m.CtaCap	PICTURE '99,999.99'
@ 11,1  SAY '   Observaciones:' GET m.Observ	PICTURE '@S20!'

READ VALID val_read() AND Val_Can()

IF LASTKEY()#27
	ak = ActKar('C',m.NumCob,m.FecPag)
	IF ak
		IF f_appd()
			GATHER MEMVAR
		ENDIF
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

DEACTIVATE WINDOW Wind_4
RELEASE    WINDOW Wind_4

ON KEY LABEL F2  DO Edita_item
ON KEY LABEL F5  DO agrega_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN

PROCEDURE Edita_Item
*-------------------
DEFINE WINDOW Wind_4 FROM 01,01 TO 14,48 DOUBLE ;
TITLE "Adici¢n de Pagos" COLOR SCHEME 10

ACTIVATE WINDOW Wind_4
**- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

SELE IteCob
SET ORDER TO IteCob4

SCATTER MEMVAR

@  0,1  SAY '        Contrato: ' + m.Codcon
@  1,1  SAY '  Corr. Cobranza: ' + m.NumCob
@  2,1  SAY ' Recibo Cobranza:' GET m.RecCob
@  3,1  SAY '  Cuota Cobranza:' GET m.FecCob	WHEN .F.
@  4,1  SAY '      Fecha Pago:' GET m.FecPag	WHEN .F.
@  5,1  SAY '    Cob. / Prom.:' GET m.TipCob	PICTURE "@M C,P"
@  6,1  SAY '        Promotor:' GET m.CodPro	VALID Val_fun('promo','codigo',"codigo+' '+apelli",m.codpro,1 ) AND vcodigo() WHEN m.TipCob = 'P'
@  7,1  SAY '        Cobrador:' GET m.CodCob	VALID Val_fun('cobra','codigo',"codigo+' '+apelli",m.codcob,1 ) AND vcodigo() WHEN m.TipCob = "C"
@  8,1  SAY '           Monto:' GET m.CanCob	PICTURE '99,999.99'
@  9,1  SAY ' Gasto Administ.:' GET m.GtoAdm	PICTURE '99,999.99'
@ 10,1  SAY '   Cuota Capital:' GET m.CtaCap	PICTURE '99,999.99'
@ 11,1  SAY '   Observaciones:' GET m.Observ	PICTURE '@S20!'

READ VALID val_read() AND Val_Can()

IF LASTKEY()#27
	ak = ActKar('C',m.NumCob,m.FecPag)
	IF ak
		GATHER MEMVAR
	ENDIF
 ELSE
	DO StandBy WITH "Cancela Informacion"
ENDIF

DEACTIVATE WINDOW Wind_4
RELEASE    WINDOW Wind_4

ON KEY LABEL F2  DO Edita_item
ON KEY LABEL F5  DO agrega_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN

PROCEDURE elimi
*---------------
SELECT socio
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Socio?')
IF velimina .AND.  f_lock(1)
	DELE NEXT 1
	SELECT iteCob
	SEEK m.codcon
	IF FOUND()
		SCAN WHILE m.codcon=iteCob.codcon
			IF f_lock(1)
				DELETE NEXT 1
			ENDIF
		ENDSCAN
	ENDIF
	SELECT socio
	IF !BOF()
		SKIP -1
	 ELSE
		IF !EOF()
			SKIP
		ENDIF
	ENDIF
ENDIF
UNLOCK ALL

DO vista
RETURN

*PROCEDURE elimi
*---------------
SELECT socio
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !estado # '  00'
	* ya pas¢
	DO standby WITH vmens10
	RETURN
ENDIF
velimina = yesno('¨ Desea ELIMINAR ‚ste Socio?')
IF velimina .AND.  f_lock(1)
	DELE NEXT 1
	SELECT itesoc
	SEEK m.codcon
	IF FOUND()
		SCAN WHILE m.codcon=itesoc.codcon
			IF f_lock(1)
				DELETE NEXT 1
			ENDIF
		ENDSCAN
	ENDIF
	SELECT socio
	IF !BOF()
		SKIP -1
	ELSE
		IF !EOF()
			SKIP
		ENDIF
	ENDIF
ENDIF
UNLOCK ALL

DO vista
RETURN

PROCEDURE elimi_item
*-------------------
SELECT IteCob
IF YesNo("Estas seguro de Eliminar este Item")
	SELE KarSSF
	SET ORDER TO KarSSF1
	IF SEEK(IteCob.CodCon+"C"+IteCob.NumCob)
		IF RLOCK()
			cCodcon = IteCob.Codcon
			cCorrel  = IteCob.NumCob
			DELETE NEXT 1
			DO Recalcula WITH cCodCon
		ENDIF
	ENDIF
	SELE IteCob
	IF RLOCK()
		DELETE NEXT 1
	 ELSE
		DO standby WITH 'No puede eliminar este Item.'
	ENDIF
ENDIF
UNLOCK
RETURN

PROCEDURE lista
*--------------
vAli = Alias()
vord = Order()
vrec = Recno()
STORE 1  TO vToCli,vTOFEC,vLista,vTofue,vTomes
STORE date() to vfecini,vfecfin,vfecins
vCodcon = SPACE(6)
vCodins = SPACE(3)

DEFINE WINDOW LIS FROM 2,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado Cobranzas' COLOR SCHEME 5
ACTIVATE WINDOW LIS

@ 01,01 SAY "       Todos : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,1,22)

@ 03,01 SAY "      C¢digo : " 
@ 03,22 GET vCodcon    PICTURE '!!!!!!' VALID Val_socio() when vTOCLI=2

@ 05,01 SAY " Instituci¢n : " GET vcodins  PICTURE '999' valid IIF(!EMPTY(vcodins),val_para(vcodins,'INSTIT',' ',17,18),.T.)   COLOR SCHEME 7 WHEN vtocli=1

*@ 07,01 SAY "Fec. Inscrip.: " GET vTOfec  FUNCTION '*RNH \<Si;\<No' SIZE 5,10,6 WHEN vtocli=1
*@ 09,01 SAY "       Fecha : "
*@ 09,22 GET vfecins  PICTURE '@D'   COLOR SCHEME 7 WHEN vTOfec=1  and vtocli=1

@ 11,01 SAY "   Cobranzas : " GET vToFue  FUNCTION '*RNH \<Todas;\<Por fechas' SIZE 1,10,6  VALID VALTOD(VTOfue,5,15) 
@ 13,01 SAY "      Fechas : "
@ 13,20 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN vtofue=2 
@ 13,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini)  WHEN vtofue=2 

@ 15,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	IF vtocli=2
		SELECT ITECOB
		SET RELATION TO CODCON INTO SOCIO
		SET SKIP TO SOCIO
		SET FILTER TO codcon=vcodcon and iif(vtofue=2,BETWEEN(feccob,vfecini,vfecfin),.t.) and socio.estado<>'99'
		do reporte with 2,"Cobranza",'Listado de Cobranza',2
	Else
		SELE IteCob
		SET RELATION TO CODCON INTO Socio ADDITIVE
		SET RELATION TO CodCon+'C'+ NumCob INTO KarSSF ADDITIVE
		INDEX ON RecCob TO vIdx FOR iif(vtofue=2,BETWEEN(ITECOB.fecpag,vfecini,vfecfin),.t.) and IIF(!EMPTY(vcodins),SOCIO.codins=allt(vcodins),.T.) and socio.estado<>'99'
susp
		do reporte with 2,"Cobra1",'Listado de Cobranza',2
		SET RELATION TO
		SELECT ITECOB
		SET ORDE TO ITECOB1


*		SELECT ITECOB
*		SET ORDE TO ITECOB4
*		SELECT SOCIO
*		SET ORDE TO MAESOC5
*		SET RELATION TO CODCON INTO ITECOB
*		SET SKIP TO ITECOB
*		set step on
*		SET FILTER TO iif(vtofue=2,BETWEEN(ITECOB.fecpag,vfecini,vfecfin),.t.) and IIF(!EMPTY(vcodins),SOCIO.codins=allt(vcodins),.T.) and socio.estado<>'99'
**		SET FILTER TO iif(vtofue=2,BETWEEN(ITECOB.fecpag,vfecini,vfecfin),.t.) and iif(vtofeC=1,socio.fecins=vfecins,.t.) AND IIF(!EMPTY(vcodins),SOCIO.codins=allt(vcodins),.T.) and socio.estado<>'99'
*		do reporte with 2,"Cobra1",'Listado de Cobranza',2
*		SET FILTER TO
*		SET RELATION TO
*		SELECT SOCIO
*		SET ORDE TO MAESOC1
*		SELECT ITECOB
*		SET ORDE TO ITECOB1
	endif
ENDIF
SET FILTER TO
SET RELATION TO
SELECT (vAli)
SET ORDER TO (vorD)
GO vrec
DO PANTALLA
DO VISTA
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
ON KEY LABEL F7
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_2
RELEASE WINDOW wind_2A
RELEASE WINDOW wind_3
RELEASE MENU   mmenu
RESTORE SCREEN FROM principal
RETURN

FUNCTION FunCob
*--------------
* Retorna Nombre del Cobrador
mRet = SPACE(30)
IF SEEK(IteCob.CodCob,'Cobra')
	mRet = ALLTRIM(Cobra.Apelli) + ' ' + ALLTRIM(Cobra.Nombre)
ENDIF
RETURN mRet

FUNCTION Val_CC
*--------------
m.CtaCap = m.CanCob/2
m.GtoAdm = m.CanCob/2

SHOW GET m.CtaCap
SHOW GET m.GtoAdm

RETURN

FUNCTION Val_Can
*---------------
IF (m.CtaCap+m.GtoAdm) # m.CanCob
	DO StandBy WITH "Cantidad Cobrada debe ser igual a la suma de Gto Ad. + CtaCap"
	RETURN .F.
ENDIF