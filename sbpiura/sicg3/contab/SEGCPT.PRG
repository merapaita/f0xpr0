**------------------------------------------------------------------------
** SEGTESO.PRG
** Registro de Accesos al Sistema de Contabilidad Patrimonial
** AUTOR: UDEP - 1996. JFG
**------------------------------------------------------------------------
PARAMETERS v_Us
*- Mensajes de aviso al usuario
Vmens01 = 'ÕÕ Seguridad ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ'+Cia+'ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ'
Vmens02 = 'Revisi¢n de Seguridad de '+Cia
Vmens03 = 'Digite C¢digo de Usuario que desea :'
Vmens04 = 'Dicho Usuario no fue encontrado'
Vmens05 = 'No existe Usuario anterior'
Vmens06 = 'No existe Usuario siguiente'
Vmens07 = '® Est† seguro que desea ANULAR este Usuario ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'M¢dulos'
Vmens10 = '® Est† seguro que desea ANULAR este m¢dulo ?'

*- Abre archivos y se posiciona en £ltimo registro
CLOS DATA
USE Usuario  IN 1  ORDER TAG Usuario1                  ALIAS Usua
USE IteUsu   IN 2  ORDER TAG IteUsu1                   ALIAS IteU
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
HIDE POPUP all
SELE iteu
SET FILT TO SISTEMA='5'
SELECT Usua
IF v_Us#"ADMIN"
	SET FILT TO Usuario=v_Us
ENDIF
*SET FILT TO CODDEP='070100' 
SET EXACT OFF
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
DO Inicia                    && Define ventanas, men£s, t°tulos

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
  RESTORE SCREEN FROM Pantalla
  RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t°tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vtempo

 DEFINE WINDOW Wind_F0 FROM 00,00 TO 08,79 DOUBLE ;
 TITLE Vmens01 FOOTER "[F3] CLAVE DE ACCESO" COLOR SCHEME 10

 DEFINE WINDOW Wind_F1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_F2 FROM 08,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE MENU MMENU COLOR SCHEME 3
 DEFINE PAD revis   OF MMENU PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF MMENU PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF MMENU PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF MMENU PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF MMENU PROMPT '\<Corrige'    AT 24,36
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


 DEFINE BAR 1  OF Modulos PROMPT  '\<A. Documentos Varios'
 DEFINE BAR 2  OF Modulos PROMPT  '\<B. Notas de Contabilidad'
 DEFINE BAR 3  OF Modulos PROMPT  '\<C. Notas del Constructivo'
 
 DEFINE BAR 4  OF Modulos PROMPT  '\<E. Consistencia General'
 DEFINE BAR 5  OF Modulos PROMPT  '\<F. Consistencia detallada'
 DEFINE BAR 6  OF Modulos PROMPT  '\<G. Listado de Errores'
 DEFINE BAR 7  OF Modulos PROMPT  '\<H. Errores por Tipo de auxiliar'
 
 DEFINE BAR 8  OF Modulos PROMPT  '\<J. Mayorizaci¢n'
 DEFINE BAR 9  OF Modulos PROMPT  '\<K. Mayorizaci¢n Constructivo'
 DEFINE BAR 10 OF Modulos PROMPT  '\<L. Proceso de Ajuste A.C.M.'
 DEFINE BAR 11 OF Modulos PROMPT  '\<M. Cierre del Mes'
 DEFINE BAR 12 OF Modulos PROMPT  '\<N. Reprocesos'
 DEFINE BAR 13 OF Modulos PROMPT  '\<O. Generacion de EE.FF.'
 DEFINE BAR 14 OF Modulos PROMPT  '\<P. Analisis de Cuenta'

 DEFINE BAR 15 OF Modulos PROMPT  '\<Q. Integraci¢n Hojas de Control'
 DEFINE BAR 16  OF Modulos PROMPT  '\<R. Integraci¢n Recivos de Ingreso'
 DEFINE BAR 17 OF Modulos PROMPT  '\<S. Integraci¢n Comprobantes de Pago'
 DEFINE BAR 18 OF Modulos PROMPT  '\<T. Integraci¢n Boletas de Deposito'
 DEFINE BAR 19 OF Modulos PROMPT  '\<U. Integraci¢n Notas de Cargo'
 DEFINE BAR 20 OF Modulos PROMPT  '\<V. Integraci¢n Notas de Abono'
 DEFINE BAR 21 OF Modulos PROMPT  '\<W. Integraci¢n Polizas E/S'
 DEFINE BAR 22 OF Modulos PROMPT  '\<X. Integraci¢n Facturas'
 DEFINE BAR 23 OF Modulos PROMPT  '\<Y. Integraci¢n Hojas de Modificaci¢n'
 DEFINE BAR 24 OF Modulos PROMPT  '\<Z. Integraci¢n Ordenes de Compra'
 DEFINE BAR 25 OF Modulos PROMPT  '\<a. Integraci¢n Cheques'
 DEFINE BAR 26 OF Modulos PROMPT  '\<b. Integraci¢n Ordenes de Servicio'
 DEFINE BAR 27 OF Modulos PROMPT  '\<c. Integraci¢n O/S con C/P'
 DEFINE BAR 28 OF Modulos PROMPT  '\<d. Integraci¢n Reversiones T-6'
 DEFINE BAR 29 OF Modulos PROMPT  '\<e. Integraci¢n Rebaja de Ingresos'
 DEFINE BAR 30 OF Modulos PROMPT  '\<f. Integraci¢n Parte de Reb. y Anul. O/C, O/S'

 DEFINE BAR 31 OF Modulos PROMPT  '\<g. Reporte Libro Caja'
 DEFINE BAR 32 OF Modulos PROMPT  '\<h. Reporte Libro Diario(Detallado)'
 DEFINE BAR 33 OF Modulos PROMPT  '\<i. Reporte Libro Diario(Resumen)'
 DEFINE BAR 34 OF Modulos PROMPT  '\<j. Reporte Libro Mayor'
 DEFINE BAR 35 OF Modulos PROMPT  '\<k. Reporte Lib. Inv. y Balances'
 DEFINE BAR 36 OF Modulos PROMPT  '\<m. Reporte Auxiliar Estandard'
 DEFINE BAR 37 OF Modulos PROMPT  '\<n. Reporte Libros Bancos'
 DEFINE BAR 38 OF Modulos PROMPT  '\<o. Reporte Asiento Centralizaci¢n'
 DEFINE BAR 39 OF Modulos PROMPT  '\<p. Reporte Analisis de Cuentas'
 DEFINE BAR 40 OF Modulos PROMPT  '\<q. Reporte Cedulas de Ajuste'
 DEFINE BAR 41 OF Modulos PROMPT  '\<r. Reporte Not/Cont. Ajustes'

 DEFINE BAR 42 OF Modulos PROMPT  '\<u. Reporte Lib. Balance Comprobaci¢n'
 DEFINE BAR 43 OF Modulos PROMPT  '\<v. Reporte Lib. Balance Constructivos'
 DEFINE BAR 44 OF Modulos PROMPT  '\<w. Reporte Lib. Historico'
 DEFINE BAR 45 OF Modulos PROMPT  '\<x. Reporte Estados de Gesti¢n'

 DEFINE BAR 46 OF Modulos PROMPT  '\<y. Plan de Cuentas'
 
 DEFINE BAR 47 OF Modulos PROMPT  '\<3. Auxiliares: Tipo de Auxiliares'
 DEFINE BAR 48 OF Modulos PROMPT  '\<4. Auxiliares: Parametros de Control'
 DEFINE BAR 49 OF Modulos PROMPT  '\<5. Auxiliares: Tipos de Documentos'
 DEFINE BAR 50 OF Modulos PROMPT  '\<6. Auxiliares: Cambio de Tipos'
 DEFINE BAR 51 OF Modulos PROMPT  '\<7. Auxiliares: Seguridad'
 DEFINE BAR 52 OF Modulos PROMPT  '\<8. Auxiliares: Tipos Financieros'
 DEFINE BAR 53 OF Modulos PROMPT  '\<9. Auxiliares Estructura EE.FF.'
 DEFINE BAR 54 OF Modulos PROMPT  '\<0. Auxiliares Tipos Finacierso Automaticos'
 DEFINE BAR 55 OF Modulos PROMPT  '\<{. Auxiliares Factores ACM - IPM'
 DEFINE BAR 56 OF Modulos PROMPT  '\<|. Auxiliares Fecha del Sistema'
 DEFINE BAR 57 OF Modulos PROMPT  '\<}. Auxiliares Mantenimiento Correlativos'
 
 DEFINE BAR 58 OF Modulos PROMPT  '\<~. Tablas: Conceptos Mov. Bncarios'
 DEFINE BAR 59 OF Modulos PROMPT  '\<Å. Tablas: Glosas'
 DEFINE BAR 60 OF Modulos PROMPT  '\<Ç. Tablas: Clases de Cuentas'
 
 DEFINE BAR 61 OF Modulos PROMPT  '\<É. Estructura F-1'
 DEFINE BAR 62 OF Modulos PROMPT  '\<Ñ. Estructura F-2'
 DEFINE BAR 63 OF Modulos PROMPT  '\<Ö. Estructura F-3'
 
 DEFINE BAR 64 OF Modulos PROMPT  '\<Ü. Indexar Archivos'
 DEFINE BAR 65 OF Modulos PROMPT  "\<à. BackUp's de Archivos"
 DEFINE BAR 66 OF Modulos PROMPT  "\<â. Depuracion de BackUp's"
 DEFINE BAR 67 OF  Modulos PROMPT '\<ä. Impresion de Archivos'
 DEFINE BAR 68 OF  Modulos PROMPT '\<ã. Usuarios'
 DEFINE BAR 69 OF  Modulos PROMPT '\<å. Seguridad'
 
 DEFINE BAR 70 OF  Modulos PROMPT '\<ç. Datos Iniciales: Saldos y Sumas'
 DEFINE BAR 71 OF  Modulos PROMPT '\<é. Dat. Inic. Saldos Apertura EE.FF'
 DEFINE BAR 72 OF  Modulos PROMPT '\<è. Saldos de Auxiliar'
 
ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m†scara de datos
*-----------------
 ACTIVATE WINDOW Wind_F0
 CLEAR
 @  1, 2 SAY "          Usuario :"
 @  3, 2 SAY "           Nombre :"
 @  4, 2 SAY "      Dependencia :"
 @  5, 2 SAY "    Observaciones :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta dato
*--------------
 ACTIVATE WINDOW Wind_F0
 ON KEY LABEL F3 DO PerCla
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
		
		cClave1 = chrtran(cClave1,'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#')
		
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
	
	cClave3 = chrtran(cClave3,'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789',;
							  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#')
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

 vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789',;
                                'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#')
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
 vTempo = '∞∞∞∞∞∞∞∞∞∞∞Presione ÆF10Ø para seleccionar  o  ÆEscØ para cancelar∞∞∞∞∞∞∞∞∞∞∞∞'
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
        DO standby WITH "El C¢digo del Usuario ya est† registrado. Proceda a corregir datos."
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
 DO Logos WITH Rotulo1,'∞∞∞∞F5->Agregar∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞F8->Eliminar∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞∞F10->Terminar∞∞∞∞'
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteU
vMens   = ''

vUsuCla = CHRTRAN(m.Usuario,'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789',;
                            'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#')

SEEK ALLTRIM(vUsuCla)

if !found()
    do agreg_item
endif
 BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_F2 KEY ALLTRIM(vUsuCla) FIELDS;
 Modulo     : H= 'M¢dulo':v=Modulo():F ,;
 Descri   = xDescri() :40 :H='      Descripci¢n        '

 SELECT IteU
 SEEK ALLTRIM(vUsuCla)
 FLUSH
 ACTIVATE SCREEN
 DO Logos WITH Rotulo1, ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 SHOW MENU MMENU
 ON KEY
 RETURN vfun


PROCEDURE Agreg_item
*-------------------
SELECT IteU
IF f_appd()
   REPLACE UsuCla   WITH vUsuCla
   REPLACE Sistema  WITH '5'
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
 STORE YESNO(Vmens07) TO vElimina
 IF !vElimina
   RETURN
 ENDIF
 IF f_lock(1)
   vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789',;
                                  'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#')
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
	vUsuCla = CHRTRAN(Usua.Usuario,'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789',;
                                   'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#')

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

xClave  = CHRTRAN(m.Clave,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~´¨@õ_#',;
                          'ABCDEFGHIJKLMN•OPQRSTUVWXYZ0123456789')

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

FUNCTION xDescri
*---------------
 DO CASE
   CASE IteU.Modulo == 'A'
        RETURN  ' Documentos Varios '
   CASE IteU.Modulo == 'B'
		RETURN  ' Notas de Contabilidad'
   CASE IteU.Modulo == 'C'
		RETURN  ' Notas del Constructivo'
		
   CASE IteU.Modulo == 'E'
		RETURN  ' Consistencia General'
   CASE IteU.Modulo == 'F'
		RETURN  ' Consistencia Detallada'
   CASE IteU.Modulo == 'G'
		RETURN  ' Listado de Errores'
   CASE IteU.Modulo == 'H'
        RETURN  ' Errores por Tipo de Auxiliar'

   CASE IteU.Modulo == 'J'
		RETURN  ' Mayorizaci¢n'
   CASE IteU.Modulo == 'K'
		RETURN  ' Mayorizaci¢n del contructivo'
   CASE IteU.Modulo == 'L'
		RETURN  ' Proceso de Ajuste ACM'
   CASE IteU.Modulo == 'M'
		RETURN  ' Cierre del Mes'
   CASE IteU.Modulo == 'N'
	    RETURN  ' Reprocesos'
   CASE IteU.Modulo == 'O'
		RETURN  ' Generaci¢n EE.FF.'
   CASE IteU.Modulo == 'P'
		RETURN  ' Analisis de Cienta'

   CASE IteU.Modulo == 'Q'
		RETURN  ' Integracion: Hojas de Control'
   CASE IteU.Modulo == 'R'
		RETURN  ' Integracion: Recibos de Ingreso'
   CASE IteU.Modulo == 'S'
		RETURN  ' Integraci¢n: Comprobantes de Pago'
   CASE IteU.Modulo == 'T'
		RETURN  ' Integraci¢n: Boletas de Deposito'
   CASE IteU.Modulo == 'U'
		RETURN  ' Integraci¢n: Nota de Cargo'
   CASE IteU.Modulo == 'V'
		RETURN  ' Integraci¢n: Nota de Abono'
   CASE IteU.Modulo == 'W'
		RETURN  ' Integraci¢n: Polizas E/S'
   CASE IteU.Modulo == 'X'
		RETURN  ' Integraci¢n: Facturas'
   CASE IteU.Modulo == 'Y'
		RETURN  ' Integraci¢n: Hojas de Modificaci¢n'
   CASE IteU.Modulo == 'Z'
		RETURN  ' Integraci¢n: Ordenes de Compra'
   CASE IteU.Modulo == 'a'
		RETURN  ' Integraci¢n: Cheques'
   CASE IteU.Modulo == 'b'
		RETURN  ' Integraci¢n: Ordenes de Sevicio'
   CASE IteU.Modulo == 'c'
		RETURN  ' Integraci¢n: O/S con pagadas'
   CASE IteU.Modulo == 'd'
		RETURN  ' Integraci¢n: Reversi¢n T-6'
   CASE IteU.Modulo == 'e'
		RETURN  ' Integraci¢n: Rebaja de Ingresos'
   CASE IteU.Modulo == 'f'
		RETURN  ' Integraci¢n: Parte Rebaja y Anulacion; O/C,O/S'
		
		
   CASE IteU.Modulo == 'g'
		RETURN  ' Reportes: Libro Caja'
   CASE IteU.Modulo == 'h'
		RETURN  ' Reportes: Libro Diario(Detallado)'
   CASE IteU.Modulo == 'i'
		RETURN  ' Reportes: Libro Diario(Resumen)'
   CASE IteU.Modulo == 'j'
		RETURN  ' Reportes: Libro Mayor'
   CASE IteU.Modulo == 'k'
        RETURN  ' Reportes: Libro Inventarios y Balances'
   CASE IteU.Modulo == 'l'
        *RETURN  ' Reportes: Balances'
   CASE IteU.Modulo == 'm'
	    RETURN  ' Reportes: Auxiliar'
   CASE IteU.Modulo == 'n'
		RETURN   ' Reportes: Libro Bancos'
   CASE IteU.Modulo == 'o'
		RETURN   ' Reportes: Asiento Centralizaci¢n'
   CASE IteU.Modulo == 'p'
		RETURN   ' Reportes: Analisis de Cuentas'
   CASE IteU.Modulo == 'q'
		RETURN   ' Reportes: Cedulas de Ajuste'
   CASE IteU.Modulo == 'r'
		RETURN   ' Reportes: N/C Ajustes'

   CASE IteU.Modulo == 'u'
	    RETURN  ' Reportes: Balance Comprobaci¢n'
   CASE IteU.Modulo == 'v'
	    RETURN  ' Reportes: Balance Constructivo'
   CASE IteU.Modulo == 'w'
	    RETURN  ' Reportes: Historico'
   CASE IteU.Modulo == 'x'
	    RETURN  ' Reportes: Estado de Gestion'
	    
   CASE IteU.Modulo == 'y'
      		RETURN   ' Plan Contable'
      		
   CASE IteU.Modulo == '3'
		RETURN   ' Auxiliares: Tipos Auxiliares'
   CASE IteU.Modulo == '4'
		RETURN   ' Auxiliares: Parametros Control'
   CASE IteU.Modulo == '5'
		RETURN   ' Auxiliares: Tipod Documento'
   CASE IteU.Modulo == '6'
		RETURN   ' Auxiliares: Cambio de Tipos'
   CASE IteU.Modulo == '7'
		RETURN   ' Auxiliares: Seguridad'
   CASE IteU.Modulo == '8'
		RETURN   ' Auxiliares: Tipos Financieros'
   CASE IteU.Modulo == '9'
		RETURN   ' Auxiliares: Estructura EE.FF.'
   CASE IteU.Modulo == '0'
		RETURN   ' Tipos Financieros Automaticos'
   CASE IteU.Modulo == '{'
		RETURN   ' Factores A.C.M. - I.P.M.'
   CASE IteU.Modulo == '|'
		RETURN   ' Auxiliares: Fecha del Sistema'
   CASE IteU.Modulo == '}'
	    RETURN  ' Auxiliares: Manten. Correl.'

   CASE IteU.Modulo == '~'
        RETURN   ' Tablas: Conc. Mov. Vancarios'
   CASE IteU.Modulo == 'Å'
		RETURN   ' Tablas: Glosas'
   CASE IteU.Modulo == 'Ç'
		RETURN   ' Tablas: Clases de Cuentas'
		
   CASE IteU.Modulo == 'É'
		RETURN   ' Estructuras: F-1'
   CASE IteU.Modulo == 'Ñ'
		RETURN   ' Estructuras: F-2'
   CASE IteU.Modulo == 'Ö'
		RETURN   ' Estructuras: F-3'
		
   CASE IteU.Modulo == 'Ü'
		RETURN   ' Indexar Archivos'
   CASE IteU.Modulo == 'à'
		RETURN   " BackUp's Archivos"
   CASE IteU.Modulo == 'â'
		RETURN   " Depuracion BackUp's"
   CASE IteU.Modulo == 'ä'
		RETURN   ' Impresion de Archivos'
   CASE IteU.Modulo == 'ã'
		RETURN   ' Usuarios'
   CASE IteU.Modulo == 'å'
		RETURN   ' Seguridad'

   CASE IteU.Modulo == 'ç'
		RETURN   ' Dat. Iniciales: Saldos y Sumas'
   CASE IteU.Modulo == 'é'
		RETURN   ' Dat. Iniciales: Saldos de Apertura EE.FF.'
   CASE IteU.Modulo == 'è'
		RETURN   ' Dat. Iniciales: Saldos de Auxiliar'
ENDCASE
RETURN ''

FUNCTION Modulo
*--------------
vMens    = ''
vRetorno = .F.
vModulo  = IteU.Modulo
vLastkey  = LASTKEY()
IF vLastkey = -7  && F8
	vRetorno  = .T.
 ELSE
	IF !vModulo $ 'ABC<\ßDEFGHIJKLMN•OPQRSTUVWXYZabcdefghijklmn§opqrstuvwxyz1234567890*/-!@#$%^&*()+=/´¨@õ_{|}~ÅÇÉÑÖÜàâäãåçéè';
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


FUNCTION XXDescri
*----------------
vali=ALIAS()
vord=ORDER()
PRIVATE vdes,Vmod
Vdes=SPACE(40)
Vmod=ALLT(iteu.modulo)
Vdes=Val_Para(vmod,'MODCPT','D')
SELE (Vali)
SET ORDER TO (vord)
RETURN vdes   

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
