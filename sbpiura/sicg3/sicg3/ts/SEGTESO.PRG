**------------------------------------------------------------------------
** SEGTESO.PRG
** Registro de Accesos al Sistema de Tesorer¡a
** AUTOR: UDEP - 1996. FMV
**------------------------------------------------------------------------
PARAMETERS v_Us
*- Mensajes de aviso al usuario
Vmens01 = 'ÍÍ Seguridad ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'+Cia+'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
Vmens02 = 'Revisi¢n de Seguridad de '+Cia
Vmens03 = 'Digite C¢digo de Usuario que desea :'
Vmens04 = 'Dicho Usuario no fue encontrado'
Vmens05 = 'No existe Usuario anterior'
Vmens06 = 'No existe Usuario siguiente'
Vmens07 = '¨ Est  seguro que desea ANULAR este Usuario ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'M¢dulos'
Vmens10 = '¨ Est  seguro que desea ANULAR este m¢dulo ?'

*- Abre archivos y se posiciona en £ltimo registro
USE Usuario  IN 1  ORDER TAG Usuario1                  ALIAS Usua
USE IteUsu   IN 2  ORDER TAG IteUsu1                   ALIAS IteU
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
HIDE POPUP all
SELE iteu
SET FILT TO SISTEMA='3'
SELECT Usua
IF v_Us#"ADMIN"
	SET FILT TO Usuario=v_Us
ENDIF
*SET FILT TO CODDEP='113000' OR FLAG='*'
SET EXACT OFF
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
DO Inicia                    && Define ventanas, men£s, t¡tulos

*- Inicia proceso
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
	ACTIVATE SCREEN
	ACTIVATE MENU MMENU
ENDDO

DO Fin_opcion


PROCEDURE Termi
*--------------
vEn_accion = .F.
DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
CLOSE DATA
RELEASE    WINDOW Wind_F0
RELEASE    WINDOW Wind_F1
RELEASE    WINDOW Wind_F2
RELEASE    MENU   MMENU
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO Logos WITH Rotulo1,vtempo

DEFINE WINDOW Wind_F0 FROM 00,00 TO 08,79 DOUBLE ;
TITLE Vmens01 COLOR SCHEME 10

DEFINE WINDOW Wind_F1 FROM 00,00 TO 23,79 DOUBLE ;
TITLE Vmens02 COLOR SCHEME 10

DEFINE WINDOW Wind_F2 FROM 08,00 TO 23,79 DOUBLE ;
TITLE Vmens09 COLOR SCHEME 10

DEFINE MENU MMENU COLOR SCHEME 3
DEFINE PAD revis   OF MMENU PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF MMENU PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF MMENU PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF MMENU PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF MMENU PROMPT '\<Corrige'    AT 24,36		SKIP FOR v_Us#'ADMIN'
DEFINE PAD ingre   OF MMENU PROMPT '\<Ingresa'    AT 24,45		SKIP FOR v_Us#'ADMIN'
DEFINE PAD elimi   OF MMENU PROMPT '\<Elimina'    AT 24,54
DEFINE PAD lista   OF MMENU PROMPT '\<Lista '     AT 24,63
DEFINE PAD termi   OF MMENU PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF MMENU DO revis
ON SELECTION PAD busca  OF MMENU DO busca
ON SELECTION PAD anter  OF MMENU DO anter
ON SELECTION PAD proxi  OF MMENU DO proxi
ON SELECTION PAD corri  OF MMENU DO corri
ON SELECTION PAD ingre  OF MMENU DO ingre
ON SELECTION PAD elimi  OF MMENU DO elimi
ON SELECTION PAD lista  OF MMENU DO lista
ON SELECTION PAD termi  OF MMENU DO termi

 DEFINE POPUP Modulos FROM 1,50 TO 22,80 COLOR SCHEME 3

 DEFINE BAR 1  OF Modulos PROMPT  '\<A. Asigna Cta.Cte a H/C'
 DEFINE BAR 2  OF Modulos PROMPT  '\<B. Comprobante de Pago'
 DEFINE BAR 3  OF Modulos PROMPT  '\<C. Hoja de Anulaci¢n de Cheque'
 DEFINE BAR 4  OF Modulos PROMPT  '\<D. Cuadro de Retenciones'
 
 DEFINE BAR 5  OF Modulos PROMPT  '\<E. Recibos de Ingresos'
 DEFINE BAR 6  OF Modulos PROMPT  '\<F. Reversiones T - 6'
 DEFINE BAR 7  OF Modulos PROMPT  '\<G. Rebajas de Ingresos '
 DEFINE BAR 8  OF Modulos PROMPT  '\<H. Facturas'
 
 DEFINE BAR 9  OF Modulos PROMPT  '\<I. Movimientos de Banco'
 DEFINE BAR 10 OF Modulos PROMPT  '\<J. Entrega de Cheques'
 DEFINE BAR 11 OF Modulos PROMPT  '\<K. Extractos Bancarios'
 DEFINE BAR 12 OF Modulos PROMPT  '\<L. Libro Banco'
 DEFINE BAR 13 OF Modulos PROMPT  '\<M. Conciliaci¢n Bancaria'
 DEFINE BAR 14 OF Modulos PROMPT  '\<N. Listado de Cheques'
 
 DEFINE BAR 15 OF Modulos PROMPT  '\<O. Prog.:Hojas de Trabajo'
 
 DEFINE BAR 16 OF Modulos PROMPT  '\<P. Bancos'
 DEFINE BAR 17 OF Modulos PROMPT  '\<Q. Cuentas Corrientes y Ahorros'
 DEFINE BAR 18 OF Modulos PROMPT  '\<R. Tipos de Recibos de Ingreso'
 DEFINE BAR 19 OF Modulos PROMPT  '\<S. Rubros de Retenciones'
 DEFINE BAR 20 OF Modulos PROMPT  '\<T. Clasificador de Ingresos'
 DEFINE BAR 21 OF Modulos PROMPT  '\<U. Auxiliar OTros'

 DEFINE BAR 22 OF Modulos PROMPT  '\<V. Caja Chica'

 DEFINE BAR 23 OF Modulos PROMPT  '\<W. Indexar archivos'
 DEFINE BAR 24 OF Modulos PROMPT  '\<X. Backup de los archivos'
 DEFINE BAR 25 OF Modulos PROMPT  '\<Y. Depuraci¢n de backups'
 DEFINE BAR 26 OF Modulos PROMPT  '\<Z. ManTenimiento Correlativos'
 DEFINE BAR 27 OF Modulos PROMPT  '\<a. Mantenimiento PaR metros'
 DEFINE BAR 28 OF Modulos PROMPT  '\<b. impresi¢n de Archivos'
 DEFINE BAR 29 OF Modulos PROMPT  '\<c. Seguridad'
 DEFINE BAR 30 OF Modulos PROMPT  '\<d. Control de Mensajes y Usuarios'
 DEFINE BAR 31 OF Modulos PROMPT  '\<e. Ir al DOS'
 
ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_F0
CLEAR
@  1, 2 SAY "          Usuario :"
@  3, 2 SAY "           Nombre :"
@  4, 2 SAY "      Dependencia :"
@  6, 2 SAY "    Observaciones :"
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta dato
*--------------
ON KEY LABEL F3 DO PerCla
ACTIVATE WINDOW Wind_F0
SELECT Usua
SCATTER MEMVAR
@  1,22 SAY m.Usuario
@  3,22 SAY m.Nombre
@  4,22 SAY val_para(m.CodDep,'CODDEP','A',22,50,7)
@  6,22 SAY m.Observ

DO Vista_Hijo
RETURN

PROCEDURE PERCLA
*-------------------
* Personaliza claves
*-------------------
SELE Usua
cclave1 = "A"
cClave2 = SPACE(10)
cClave3 = SPACE(10)
DEFINE WINDOW Pass FROM 05,10 TO 15,50 DOUBLE ;
	TITLE "Clave de Opciones del Sistema" COLOR SCHEME 10
	
ACTIVATE WINDOWS Pass 

@ 01,03 SAY " Usuario : "
@ 01,20 SAY m.Usuario


IF LASTKEY()#27
	
	DO WHILE !(ALLTRIM(m.Clave) == ALLTRIM(cClave1))
		@ 03,03 SAY IIF(!EMPTY(m.Clave),"Ingrese su clave anterior :",SPACE(10))
		@ 03,30 SAY SPACE(10)
		cClave1 = IIF(!EMPTY(m.Clave),Aster(3,30),"")
		IF LASTKEY()=27
			DEACTIVATE WINDOWS Pass
			RELEASE    WINDOWS Pass
			RETURN
		ENDIF
		
		cClave1 = chrtran(cClave1,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
		
		IF !(ALLTRIM(m.Clave) == ALLTRIM(cClave1))
			DO StandBy WITH "Error al digitar su clave. Intente de nuevo"
			LOOP
		ENDIF
	ENDDO
	
	cClave2 = "A"
	cClave3 = "B"
	DO WHILE !(cClave2 == cClave3)
		@ 05,03 SAY "Ingrese su Nueva Clave :"
		@ 05,30 SAY SPACE(10)
		@ 07,30 SAY SPACE(10)
		cClave2 = Aster(5,30)
		@ 07,03 SAY "Repita su Nueva Clave: "
		@ 07,30 SAY SPACE(10)
		cClave3 = Aster(7,30)
		IF !(cClave2 == cClave3)
			DO StandBy WITH "Clave Incorrecta Intente de nuevo"
		ENDIF
	ENDDO
	
	cClave3 = chrtran(cClave3,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
	IF LASTKEY()#27
		REPLACE Usua.Clave WITH cClave3
	ENDIF
 ELSE
	DO StandBy WITH "Proceso Cancelado"
ENDIF

DEACTIVATE WINDOWS Pass
RELEASE    WINDOWS Pass

RETURN

PROCEDURE Aster
*--------------
PARAMETERS xFil,xCol
m.Key = 0
xPass = ""
IF LASTKEY()#27
	@ xFil,xCol SAY ''
	DO WHILE m.key # 13 .AND. m.key#27             &&  accept until car_return
		m.key = INKEY(0)
		DO CASE
			CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122) .OR. BETWEEN(m.key,48,57) .OR. BETWEEN(m.key,164,165)
				xPass = xPass + CHR(m.key)
			CASE m.key = 19 .OR. m.key = 127 .OR. m.key = 7
				@ ROW(),COL()-1 SAY ' '
				@ ROW(),COL()-1 SAY ''
				xPass = SUBSTR(xPass,1,LEN(xPass)-1)
			OTHERWISE
		ENDCASE
		
		IF LASTKEY()#27
			@ xFil,xCol SAY REPLICATE('',LEN(xPass))
		ENDIF
	ENDDO
ENDIF
RETURN xPass


PROCEDURE Vista_Hijo
*-------------------
SELECT IteU
vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
			                   'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
SEEK ALLTRIM(vUsuCla)
BROWSE;
		NOAPPEND NODELETE NOMENU NOCLEAR NOEDIT TIMEOUT .001;
		WINDOW Wind_F2 KEY ALLTRIM(vUsuCla)    FIELDS;
		Modulo   :H='M¢dulo',;
		Descri   = xDescri() :40 :H='      Descripci¢n       '
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F3
SELECT Usua
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
Vtemp = RECNO()
HIDE MENU MMENU
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW Wind_F1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	Usuario :H='Usuario',;
	Nombre  :H='Nombre',;
	Observ  :H='Observaciones'

vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
	GOTO Vtemp
 ELSE
	SCATTER MEMVAR
ENDIF
SHOW MENU MMENU
ON KEY LABEL F10
DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 ON KEY LABEL F3
 SELECT Usua
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW standby
 STORE SPACE(LEN(m.Usuario))  TO Vbusca
 @ 1,3 SAY Vmens03 GET Vbusca COLOR SCHEME 1
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(Vbusca) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   IF !SEEK(ALLTRIM(Vbusca))
     DO standby WITH Vmens04
     GOTO Vtemp
   ELSE
     SCATTER MEMVAR
     DO Vista
   ENDIF
 ENDIF
 RETURN


PROCEDURE Anter
*--------------
 SELECT Usua
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH Vmens05
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
 SELECT Usua
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH Vmens06
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
ON KEY LABEL F3
SELECT Usua
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF

SELECT Usua
IF f_lock(1)
	ACTIVATE WINDOW Wind_F0
	@  4,22 CLEAR TO 4,79
	@  1,22 SAY m.Usuario
	@  3,22 GET m.Nombre PICTURE '@!'
	@  4,22 GET m.CodDep  PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,8)
	@  6,22 GET m.Observ PICTURE '@!'
	READ
	
	IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
		SELE ITEU
		OK=Trabaja_Hijo()
		IF OK
			SELECT Usua
			GATHER MEMVAR
		ENDIF
	 ELSE
		SELECT Usua
		SCATTER MEMVAR
	ENDIF
	DO Vista                    && Muestra nuevos datos
ENDIF
UNLOCK ALL
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
ON KEY LABEL F3
ACTIVATE WINDOW Wind_F0
SELECT Usua
 vLastkey= 0
 SCATTER MEMVAR BLANK
 @  4,22 CLEAR TO 4,79
 @  1,22 GET m.Usuario     PICTURE 'NNNNNNNNNN'
 READ
 m.Usuario = UPPER(m.Usuario)
 IF LASTKEY() = 27 .OR. EMPTY(m.Usuario)
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   GO BOTTOM
 ELSE

      SELECT Usua
      IF SEEK(ALLTRIM(m.Usuario))
        DO standby WITH "El C¢digo del Usuario ya est  registrado. Proceda a corregir datos."
        DO Vista
        DO Corri
      ELSE
        @  1,22 SAY m.Usuario
        @  3,22 GET m.Nombre PICTURE '@!'
        @  4,22 GET m.CodDep  PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40,8)

        @  6,22 GET m.Observ PICTURE '@!'
	
        READ
        IF LASTKEY() <> 27
           SELECT IteU
           ok=Trabaja_hijo()
           select Usua
           if ok
              if f_appd()
                 GATHER MEMVAR
           	  endif
           endif
        ELSE
          GO BOTTOM
        ENDIF
      ENDIF
   ENDIF
 UNLOCK ALL
 DO Vista
 RETURN


PROCEDURE Trabaja_Hijo
*---------------------
private vfun
vfun = .t.
HIDE MENU MMENU
ACTIVATE SCREEN
 DO Logos WITH Rotulo1,'°°°°F5->Agregar°°°°°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°°°°°F10->Terminar°°°°'
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteU
vMens   = ''
vUsuCla = CHRTRAN(m.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                            'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
SEEK ALLTRIM(vUsuCla)

if !found()
    do agreg_item
endif

BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_F2 KEY ALLTRIM(vUsuCla) FIELDS;
	Modulo     :H='M¢dulo' :P='N':V= Modulos():F :E=vMens,;
	Descri   = xDescri() :40 :H='      Descripci¢n        '

* BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_F2 KEY ALLTRIM(vUsuCla) FIELDS;
*	 Modulo     : H= 'M¢dulo':v=val_paraD(modulo,'MODTES','modulo'):F ,;
*    Descri   = xDescri() :40 :H='      Descripci¢n        '

 SELECT IteU
 SEEK ALLTRIM(vUsuCla)
 FLUSH
 ACTIVATE SCREEN
 DO Logos WITH Rotulo1, ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 SHOW MENU MMENU
 ON KEY
 RETURN vfun

FUNCTION Modulos
*---------------
vMens    = ''
vRetorno = .F.
vModulo  = IteU.Modulo
vLastkey  = LASTKEY()
IF vLastkey = -7  && F8
	vRetorno  = .T.
 ELSE
	IF !vModulo $ 'ABCDEFGHIJKLMN¥OPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890*/-~';
		.OR. EMPTY(vModulo)
		
		ON SELECTION POPUP Modulos DEACTIVATE POPUP Modulos
		ACTIVATE POPUP Modulos
		IF !EMPTY(PROMPT())
			vModulo  = SUBSTR(PROMPT(),1,1)
			vRetorno = .T.
		 ELSE
			vMens    = 'Seleccione un m¢dulo'
			vModulo  = SPACE(1)
		ENDIF
	 ELSE
		vRetorno = .T.
	ENDIF
	vReg    = RECNO()
	IF SEEK(vUsuCla + vModulo)
		IF vReg # RECNO()
			vRetorno = .F.
			vMens    = 'M¢dulo ya seleccionado'
			GO vReg
			IF !EMPTY(IteU.Modulo)
				IF f_lock(1)
					REPLACE Modulo WITH SPACE(1)
				ENDIF
			ENDIF
		 ELSE
			IF EMPTY(IteU.Modulo)
				IF f_lock(1)
					REPLACE Modulo WITH vModulo
				ENDIF
			ENDIF
		ENDIF
	 ELSE
		GO vReg
		IF EMPTY(IteU.Modulo)
			IF f_lock(1)
				REPLACE Modulo WITH vModulo
			ENDIF
		ENDIF
	ENDIF
ENDIF
RETURN vRetorno


PROCEDURE Agreg_item
*-------------------
SELECT IteU
IF f_appd()
   REPLACE UsuCla   WITH vUsuCla
   REPLACE Sistema  WITH '3'
ENDIF
RETURN


PROCEDURE Elimi_item
*-------------------
 IF f_lock(1)
   STORE YESNO(Vmens10) TO vElimina
   IF vElimina
     SELECT IteU
     DELETE NEXT 1
   ENDIF
 ENDIF
 RETURN


PROCEDURE Elimi
*--------------
 ON KEY LABEL F3
 STORE YESNO(Vmens07) TO vElimina
 IF !vElimina
   RETURN
 ENDIF
 IF f_lock(1)
 vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                                'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
 SELECT IteU
 SEEK ALLTRIM(vUsuCla)
   SCAN WHILE vUsuCla = IteU.UsuCla
        IF f_lock(1)
           DELETE
         ENDIF
   ENDSCAN
   SELECT Usua
   DELETE
   UNLOCK ALL
 ENDIF
 SELECT Usua
 IF !BOF()
    SKIP -1
 ELSE
    GO BOTTOM
 ENDIF
 DO Vista
 RETURN


PROCEDURE Lista
*--------------
 ON KEY LABEL F3
 ACTIVATE WINDOW standby
 SELECT UsuA
 vReg = RECNO()
 GO TOP
 numini = Usua.Usuario
 GO BOTTOM
 numfin = Usua.Usuario
 @ 0, 1 SAY 'Usuario Inicial:' GET numini VALID numini<=numfin;
        ERROR 'Usuario inicial mayor que el final'
 @ 1, 1 SAY '          Final:' GET numfin VALID numini<=numfin;
        ERROR 'Usuario inicial mayor que el final'
 READ
 DEACTIVATE WINDOW standby
 DO RepPrg WITH 'ImpUsu', ' Relaci¢n de Usuarios '
 SELECT Usua
 GO vReg
 DO Vista
 RETURN


PROCEDURE ImpUsu
*---------------
PARAMETER _desti
IF _desti=2
  SET PRINTER TO (p_fil)
ENDIF

SET DEVICE TO PRINT
STORE 0 TO lineas

 SELECT Usua
 SEEK numini
 SCAN WHILE numini <= Usua.Usuario .AND. numfin >= Usua.Usuario
    SCATTER MEMVAR
    IF lineas > 50 .OR. lineas = 0
      lineas = 0
      DO ImpTit WITH lineas, .T.
    ELSE
      DO ImpTit WITH lineas, .F.
    ENDIF
    vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
                                   'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~')
    SELECT IteU
    SEEK ALLTRIM(vUsuCla)
    SCAN WHILE vUsuCla = IteU.UsuCla
       IF lineas > 50 .OR. lineas = 0
         lineas = 0
         DO ImpTit WITH lineas, .T.
       ENDIF
       SELECT IteU
       @ lineas, 0 SAY IteU.Modulo
       @ lineas,10 SAY xDescri()
       lineas = lineas + 1
    ENDSCAN

    SELECT Usua
 ENDSCAN
 SET DEVICE TO SCREEN
 SET PRINTER TO
 RETURN


FUNCTION xDescri
*---------------
DO CASE
	CASE IteU.Modulo == 'A'
		RETURN   ' Asigna Cta.Cte a H/C '
	CASE IteU.Modulo == 'B'
		RETURN   ' Comprobante de Pago'
	CASE IteU.Modulo == 'C'
		RETURN   ' Hoja de Anulaci¢n de Cheque'
	CASE IteU.Modulo == 'D'
		RETURN   ' Cuadro de Retenciones '
		
	CASE IteU.Modulo == 'E'
		RETURN   ' Recibos de Ingresos'
	CASE IteU.Modulo == 'F'
		RETURN   ' Reversiones T - 6'
	CASE IteU.Modulo == 'G'
		RETURN   ' Rebajas de Ingresos'
	CASE IteU.Modulo == 'H'
		RETURN   ' Facturas'
	
	CASE IteU.Modulo == 'I'
		RETURN   ' Movimientos de \<Banco'
	CASE IteU.Modulo == 'J'
		RETURN   ' Entrega de Cheques'
	CASE IteU.Modulo == 'K'
		RETURN   ' Extractos Bancarios'
	CASE IteU.Modulo == 'L'
		RETURN   ' Libro Banco'
	CASE IteU.Modulo == 'M'
		RETURN   ' Conciliaci¢n Bancaria'
	CaSE IteU.Modulo == 'N'
		RETURN   ' Listado de Cheques'
		
	CaSE IteU.Modulo == 'O'
		RETURN   ' Prog.:Hojas de Trabajo'
	
	CaSE IteU.Modulo == 'P'
		RETURN   " Bancos"
	CASE IteU.Modulo == 'Q'
		RETURN   " Cuentas Corrientes y Ahorros"
	CASE IteU.Modulo == 'R'
		RETURN   ' Tipos de Recibos de Ingreso'
	CASE IteU.Modulo == 'S'
		RETURN   ' Rubros de Retenciones'
	CASE IteU.Modulo == 'T'
		RETURN   ' Clasificador de Ingresos'
	CASE IteU.Modulo == 'U'
		RETURN   ' Auxiliar OTros'
		
	CASE IteU.Modulo == 'V'
		RETURN   ' Caja Chica'
	
	CASE IteU.Modulo == 'W'
		RETURN   ' Indexar Archivos '
	CaSE IteU.Modulo == 'X'
		RETURN   ' BackUp de Archivos '
	CASE IteU.Modulo == 'Y'
		RETURN   " Depuracion de BackUp's "
	CASE IteU.Modulo == 'Z'
		RETURN   ' Mantenimiento de Correlativos'
	CASE IteU.Modulo == 'a'
		RETURN   ' Matenimiento de Parametros'
	CASE IteU.Modulo == 'b'
		RETURN   ' Impresi¢n de Archivos'
	CASE IteU.Modulo == 'c'
		RETURN   ' Seguridad'
	CASE IteU.Modulo == 'd'
		RETURN   ' Control de Mensajes y Usuarios'
	CASE IteU.Modulo == 'e'
		RETURN   ' Ir Al D.O.S.'
ENDCASE
RETURN ''


FUNCTION XXDescri
*--------------
PRIVATE vdes,Vmod

vali=ALIAS()
vord=ORDER()
Vdes=SPACE(40)
Vmod=ALLT(iteu.modulo)
Vdes=Val_Para(vmod,'MODTES','D')
SELE (Vali)
SET ORDER TO (vord)
RETURN vdes   





PROCEDURE ImpTit
*---------------
PARAMETERS vlineas, Cabecera
IF vlineas = 0
   vtitulo = 'Relaci¢n de Usuarios'
   @  1,0  SAY SUBSTR(Cia,1,20)
   @  1,INT((80 - LEN(vtitulo))/2) SAY vTitulo
   @  1,73 SAY DATE()
   vlineas = 2
ENDIF
vlineas = vlineas + 1
@  vlineas,0  SAY "   Usuario: " + m.Usuario
vlineas = vlineas + 1
@  vlineas,0  SAY "    Nombre: " + m.Nombre
vlineas = vlineas + 1
xClave  = CHRTRAN(m.Clave,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~',;
                          'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789')
@  vlineas,0  SAY "     Clave: " + xClave
vlineas = vlineas + 1
IF Cabecera
   @ vlineas,00 SAY REPLICATE('-',80)
   vlineas = vlineas + 1
   @ vlineas,00 SAY 'M¢dulo'
   @ vlineas,10 SAY '    Descripci¢n     '
   vlineas = vlineas + 1
   @ vlineas,00 SAY REPLICATE('-',80)
   vlineas = vlineas + 1
   SELECT IteU
ELSE
  vlineas = vlineas + 1
ENDIF
RETURN


FUNCTION Valida
*--------------
PARAMETERS Valor, Compara
Valor   = UPPER(Valor)
Compara = UPPER(Compara)
IF Valor # Compara
  Valor = SPACE(LEN(Valor))
  @  4,22 SAY Valor  PICTURE '!!!!!!!!!!'
  RETURN -1
ENDIF
RETURN 1