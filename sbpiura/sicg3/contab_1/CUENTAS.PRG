** --------------------------------------
** CUENTAS.PRG
** Plan de Cuentas
** AUTOR : UDEP - 1994
**
** Tipos de Cuentas:
** 'O'=Orden,     'A'=Activo,      'P'=Pasivo,  'T'=Patrimonio,  'G'=Gesti?n,
** 'R'=Resultado, 'E'=Presupuesto.
** --------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Plan Contable '
Vmens02 = 'Revisi?n del Plan Contable'
Vmens03 = 'Digite c?digo de Cuenta que desea :'
Vmens04 = 'Dicha Cuenta no fue encontrada'
Vmens05 = 'No existe Cuenta anterior.'
Vmens06 = 'No existe Cuenta siguiente.'
Vmens07 = '?Est? seguro que desea ELIMINAR ?sta Cuenta?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Cuenta existe en los Comprobantes de Movimientos'

Agrega = .T.
Bloquea= .T.
USE Cuentas  IN 1  ORDER TAG Cuentas1          ALIAS Cuen
USE Parmae   IN 2  ORDER TAG Parmae1           ALIAS parma
USE Diario   IN 3  ORDER TAG Diario3           ALIAS Diar
SELECT Cuen
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men?s, t?tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men? mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men?s y t?tulos
*---------------

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi

 ACTIVATE SCREEN
 RETURN


PROCEDURE Pantalla           && Pinta m?scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  1, 2 SAY "            Cuenta:"
 @  3, 2 SAY "Descripci?n Cuenta:"
 @  5, 2 SAY "             Nivel:"
 @  7, 2 SAY "           Detalle:"
 @  9, 2 SAY "              Tipo:"
 @ 11, 2 SAY "Contra cuenta Debe:"
 @ 13, 2 SAY "Contr.cuenta Haber:"
 @ 15, 2 SAY "        Debe/Haber:"
 @ 17, 2 SAY "Descripci?n Ingl?s:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT Cuen
 SCATTER MEMVAR
 @  1,22 SAY m.Cuenta    PICTURE '!!!!!!!!!!!!!!!'
 @  3,22 SAY m.Descri
 @  5,22 SAY m.Nivel    PICTURE '99'
 @  7,22 SAY IIF(m.Detalle = 'S','Si','No')
 @  9,22 SAY TipoCuenta(m.Tipo)
 @ 11,22 SAY m.DContra  PICTURE '!!!!!!!!!!!!!!!'
 vreg = IIF(!EOF(),RECNO(),0)
 @ 11,33 SAY IIF(EMPTY(VAL_cuen(m.DContra)),SPACE(40),Val_cuen(m.DContra))
 @ 13,22 SAY m.HContra  PICTURE '!!!!!!!!!!!!!!!'
 @ 13,33 SAY IIF(EMPTY(VAL_cuen(m.HContra)),SPACE(40),Val_cuen(m.HContra))
 IF vreg # 0
    GO vreg
 ENDIF
 @ 15,22 SAY IIF(DbHb='D','Deudora  ','Acreedora')
 @ 17,22 SAY m.DesIng
 @ 17,60 SAY "Se Ajusta: " + IIF(m.Ajuste=.T.,'Si','No')
 @ 19,60 SAY "Ref.Oblig: " + IIF(m.RefObl,'Si',IIF(m.Detalle='S','No','  '))
 RETURN


PROCEDURE Revis              && Revisi?n de BD en browse
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Cuenta   :H=' Cuenta',;
     Descri   :H='      Descripci?n',;
     Nivel    :H='Nivel',;
     Detalle  =IIF(Detalle='S','Si','No') :H='Detalle',;
     Tipo     = TipoCuenta(Tipo) :H='  Tipo'

 vTempo = '????????????????????????????????????????????????????????????????????????????????'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b?squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW standby
 STORE SPACE(10) TO Vbusca
 @ 1,2 SAY Vmens03 GET Vbusca PICT '!!!!!!!!!!!!!!!'
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca = SPACE(10) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK PADR( ALLTRIM(Vbusca),10,"0" )
   IF EOF()
     DO standby WITH Vmens04
     GOTO Vtemp
   ELSE
     DO Vista
   ENDIF
 ENDIF
 RETURN


PROCEDURE Anter
*--------------
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
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 DO Fox_Lock WITH 1, Bloquea
 IF Bloquea
   vreg = IIF(!EOF(),RECNO(),0)
   xDetalle  = IIF(m.Detalle='S','Si','No')
   x1Detalle = IIF(m.Detalle='S','@M Si,No','@M No,Si')
   XDbHb     = IIF(m.DbHb   ='D','Deudora  ','Acreedora')
   X1DbHb    = IIF(m.DbHb   ='D','@M Deudora  ,Acreedora','@M Acreedora, Deudora  ')
   xAjuste   = IIF(m.Ajuste=.T.,'Si','No')
   xRefObl   = IIF(m.RefObl,'Si',IIF(m.Detalle='S','No','  '))
   vmens     = ''
   
   @  1,22 SAY m.Cuenta   PICTURE '!!!!!!!!!!!!!!!'
   @ 17,60 SAY "Se Ajusta:"
   @ 19,60 SAY "Ref.Oblig:"
   @  3,22 GET m.Descri
   @  5,22 GET m.Nivel    PICTURE '99'			WHEN .F.
   @  7,22 GET xDetalle   PICTURE x1Detalle		WHEN IngDeta() AND .F.
   @  9,22 GET m.tipo PICTURE '!!' valid VAL_PARAX(m.tipo,'TIPCLA',' ',22,25)
   @ 11,22 GET m.DContra  PICTURE '!!!!!!!!!!!!!!!' VALID EMPTY(m.DContra) .OR. Contra(m.DContra,.T.,11,22);
                          ERROR vmens
   @ 13,22 GET m.HContra  PICTURE '!!!!!!!!!!!!!!!' VALID EMPTY(m.HContra) .OR. Contra(m.HContra,.T.,13,22);
                          ERROR vmens
   @ 15,22 GET XDbHb      PICTURE x1DbHb
   @ 17,22 GET m.DesIng
   @ 17,71 GET xAjuste    PICTURE '@M Si,No'   && WHEN SUBSTR(xTipo,1,1) $ 'A P '
   @ 19,71 GET xRefObl    PICTURE '@M Si,No'   WHEN xDetalle='Si'
   READ
   
   m.DContra = IIF(EMPTY(m.HContra) .AND. !EMPTY(m.DContra),SPACE(10),m.DContra)
   m.HContra = IIF(EMPTY(m.DContra) .AND. !EMPTY(m.HContra),SPACE(10),m.HContra)
   m.Detalle = IIF(xDetalle $ 'Si','S','N')
   *m.Tipo    = CodigoCuenta(xTipo)
   m.DbHb    = IIF(xDbHb    $ 'Deudora  ','D','H')
   m.Ajuste  = IIF(xAjuste  = "Si",.T.,.F.)
   m.RefObl  = IIF(xRefObl  = "Si".AND. m.Detalle = "S",.T.,.F.)
   IF vreg # 0
      GO vreg
   ENDIF
   IF LASTKEY()<>27 .AND. yesno("Confirme la correcci?n")        && Si se presion? Esc, no graba
      GATHER MEMVAR
      SEEK m.Cuenta
   ENDIF
   DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu? coloca valores por omisi?n (default)
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR BLANK
 @  1,22 GET m.Cuenta   PICTURE '!!!!!!!!!!!!!!!'	VALID vLlena() AND ValCCta()
 READ
 SELECT Cuen
 IF LASTKEY() = 27 .OR. m.Cuenta=SPACE(15)
    DO standby WITH "Proceso cancelado. No se graba ning?n cambio."
    GO BOTTOM
 ELSE
   SEEK  m.Cuenta
   IF FOUND()
      DO standby WITH "Cuenta ya est? registrada. Proceda a corregir datos."
      DO Vista
      DO Corri
   ELSE
     IF !VerDeta()
        RETURN
     ENDIF
     @  3,22 CLEAR TO 23,78
     vreg = IIF(!EOF(),RECNO(),0)
     xDetalle = IIF(m.Detalle='S','Si','No')
     XDbHb    = IIF(m.DbHb   ='D','Deudora  ','Acreedora')
     xAjuste   = IIF(m.Ajuste=.T.,'Si','No')
     xRefObl   = IIF(m.RefObl,'Si',IIF(m.Detalle='S','No','  '))
     vmens    = ''

     @  1,22 SAY m.Cuenta   PICTURE '!!!!!!!!!!!!!!!'
     @ 17,60 SAY "Se Ajusta:"
     @ 19,60 SAY "Ref.Oblig:"
     @  3,22 GET m.Descri
     @  5,22 GET m.Nivel    PICTURE '99'	WHEN .F.
     @  7,22 GET xDetalle   PICTURE "@M Si,No" WHEN IngDeta() AND .F.
     @  9,22 GET m.tipo PICTURE '!!' valid VAL_PARAX(m.tipo,'TIPCLA',' ',22,25)
     @ 11,22 GET m.DContra  PICTURE '!!!!!!!!!!!!!!!' VALID EMPTY(m.DContra) .OR. Contra(m.DContra,.T.,11,22);
                            ERROR vmens
     @ 13,22 GET m.HContra  PICTURE '!!!!!!!!!!!!!!!' VALID EMPTY(m.HContra) .OR. Contra(m.HContra,.T.,13,22);
                            ERROR vmens
                            
     @ 15,22 GET XDbHb      PICTURE '@M Deudora  ,Acreedora'
     @ 17,22 GET m.DesIng
     @ 17,71 GET xAjuste    PICTURE '@M Si,No'      && WHEN SUBSTR(xTipo,1,1) $ "A P "
     @ 19,71 GET xRefObl    PICTURE '@M Si,No'   WHEN xDetalle='Si'
     READ

     m.DContra = IIF(EMPTY(m.HContra) .AND. !EMPTY(m.DContra),SPACE(10),m.DContra)
     m.HContra = IIF(EMPTY(m.DContra) .AND. !EMPTY(m.HContra),SPACE(10),m.HContra)
     m.Detalle = IIF(xDetalle $ 'Si','S','N')
     m.DbHb    = IIF(xDbHb    $ 'Deudora  ','D','H')
     m.Ajuste  = IIF(xAjuste  = "Si",.T.,.F.)
     m.RefObl  = IIF(xRefObl  = "Si".AND. m.Detalle = "S",.T.,.F.)
     IF vreg # 0
       GO vreg
     ENDIF

     IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
       DO Fox_appd WITH Agrega
       IF Agrega
         GATHER MEMVAR
         DO Veri_niv
         SEEK m.Cuenta
       ENDIF
       UNLOCK
     ELSE
       GO BOTTOM
     ENDIF
     IF vreg # 0
       GO vreg
     ENDIF

   ENDIF
 ENDIF
 DO Vista
 RETURN

FUNCTION Veri_niv
*----------------
PRIVATE xCta,xCta2,xNiv
xCta = quitacero(m.Cuenta)
xLen = LEN(xCta)
xCta = LEFT(xcta,IIF(xLen=4,xLen-3,xLen-2))
xNiv = Nivel
*SCAN WHILE (Cuenta = xCta)
*	xNiv = Nivel
*ENDSCAN
IF SEEK(xCta)
	SCAN WHILE (Cuenta = xCta) AND Nivel < xNiv
*	SCAN WHILE (LEFT(Cuenta,1) = xCta) AND Nivel < xNiv
		REPLACE Detalle WITH 'N'
	ENDSCAN
ENDIF
RETURN

*FUNCTION Veri_niv
*----------------
PRIVATE xCta,xNiv
xCta = LEFT(m.Cuenta,1)
SCAN WHILE (LEFT(Cuenta,1) = xCta)
	xNiv = Nivel
ENDSCAN
IF SEEK(xCta)
	SCAN WHILE (LEFT(Cuenta,1) = xCta) AND Nivel < xNiv
		REPLACE Detalle WITH 'N'
	ENDSCAN
ENDIF
RETURN

PROCEDURE QuitaCero
*------------------
PARAMETER vcuen1
PRIVATE vCuen
VALI = ALIAS()
REC  = RECNO()
SELE Cuen
SEEK vCuen1
IF FOUND()
	vniv = nivel
ELSE
	vniv = 0
ENDIF
DO CASE
	CASE vniv = 1
		vCuen = SUBSTR(vCuen1,1,1)
	CASE vniv = 2
		vCuen = SUBSTR(vCuen1,1,4)
	CASE vniv = 3
		vCuen = SUBSTR(vCuen1,1,6)
	CASE vniv = 4
		vCuen = SUBSTR(vCuen1,1,8)
	CASE vniv = 5
		vCuen = SUBSTR(vCuen1,1,10)
	CASE vniv = 6
		vCuen = SUBSTR(vCuen1,1,12)
	CASE vniv = 7
		vCuen = SUBSTR(vCuen1,1,14)
	OTHERWISE
		vCuen = vCuen1
ENDCASE
SELE (VALI)
GO REC
RETURN vCuen

FUNCTION vLlena
*--------------
m.Cuenta = PADR(ALLTRIM(m.Cuenta),15,'0')
SHOW GET m.Cuenta
RETURN .T.

FUNCTION ValCCta
*---------------
PRIVATE mRet
mRet = .T.
xNiv1 = LEFT(m.Cuenta,1)
DO CASE
	CASE BETW(VAL(xNiv1),1,9)
		xNiv2 = SUBSTR(m.Cuenta,2,3)
		xNiv3 = SUBSTR(m.Cuenta,5,2)
		xNiv4 = SUBSTR(m.Cuenta,7,2)
		xNiv5 = SUBSTR(m.Cuenta,9,2)
		xNiv6 = SUBSTR(m.Cuenta,11,2)
		xNiv7 = SUBSTR(m.Cuenta,13,2)
ENDCASE

FOR i = 7 TO 1 STEP -1
	xCmp = "xNiv"+STR(i,1)
	xVal = &xCmp
	IF VAL(xVal) > 0
		m.Nivel = i
		EXIT
	ENDIF
ENDFOR

xCta = ""
FOR i = 1 TO 7
	xCmp = "xNiv"+STR(i,1)
	xVal = &xCmp
	IF VAL(xVal) = 0 AND i < m.Nivel
		DO StandBy WITH "Error al definir la Cuenta"
		mRet = .F.
	ENDIF
	IF VAL(xVal) # 0
		xCta = xCta + xVal
	ELSE
		EXIT
	ENDIF
ENDFOR

IF mRet
	nReg = RECNO()
	IF SEEK(xCta)
		m.Detalle = 'N'
	ELSE
		m.Detalle = 'S'
	ENDIF
	
	xCta = ''
	FOR i = 1 to m.Nivel - 1
		xCmp = "xNiv"+STR(i,1)
		xVal = &xCmp
		IF VAL(xVal) # 0
			xCta = xCta + xVal
		ELSE
			EXIT
		ENDIF
	ENDFOR
	
	IF !SEEK(xCta)
		DO StandBy WITH "No han Ingresado el nivel anterior."
		mRet = .F.
	ENDIF
	nReg = RECNO()
ENDIF

RETURN mRet

FUNCTION VerDeta
*---------------
vnum = LEN(ALLTRIM(m.Cuenta))
DO WHILE vnum > 0
  vnum = vnum - 1
  SEEK SUBSTR(m.Cuenta,1,vnum) + SPACE(15 - vnum)
  IF FOUND()
    IF Cuen.Detalle = 'S'
      SELECT Diar
      SEEK Cuen.Cuenta
      IF FOUND()
         DO Standby WITH 'Cuenta ' + ALLTRIM(Cuenta) + ' existe en los comprobantes'
         SELECT Cuen
         RETURN .F.
      ENDIF
      SELECT Cuen
      cambia = Yesno(ALLTRIM(Cuen.Cuenta) + " es de Detalle. Modifica")
      IF cambia
        DO Fox_Lock WITH 1, Bloquea
        IF Bloquea
          REPLACE Cuen.Detalle WITH 'No'
          EXIT
        ELSE
          RETURN .F.
        ENDIF
      ENDIF
    ELSE
      EXIT
    ENDIF
  ENDIF
ENDDO
RETURN .T.

FUNCTION IngDeta
*---------------
SEEK ALLTRIM(m.Cuenta)
IF FOUND()
   IF m.Cuenta = Cuen.Cuenta
      SKIP
   ENDIF
   IF ALLTRIM(m.Cuenta) = SUBSTR(Cuen.Cuenta,1,LEN(ALLTRIM(m.Cuenta)))
      xDetalle = 'No'
      @ 7,22 SAY xDetalle
      SEEK m.Cuenta
      RETURN .F.
   ENDIF
ENDIF
SEEK m.Cuenta
RETURN .T.

FUNCTION Contra
*--------------
PARAMETERS vContra, vLogica, Fila, Columna
vmens = ''
SELECT Cuen
vReg     = IIF(!EOF(), RECNO(), 0)
vDetalle = SUBSTR(xDetalle,1,1)
IF Val_Cuen(vContra,vLogica,Fila,Columna)
   SELECT Cuen
   SEEK vContra
   IF UPPER(Cuen.Detalle) # vDetalle
      IF vReg # 0
         GO vReg
      ELSE
         GO BOTTOM
      ENDIF
      vmens = 'Contra cuenta ' + IIF(vDetalle = 'S','no es','es') + ' detalle'
      RETURN .F.
   ENDIF
ELSE
  SELECT Cuen
  IF vReg # 0
     GO vreg
  ELSE
     GO BOTTOM
  ENDIF
  vmens = 'Cuenta no existe'
  RETURN .F.
ENDIF
SELECT Cuen
IF vReg # 0
   GO vReg
ELSE
   GO BOTTOM
ENDIF
RETURN .T.

PROCEDURE Elimi
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SELECT Diar
 SEEK m.Cuenta
 IF FOUND()
    DO standby WITH Vmens09
    SELECT Cuen
    RETURN
 ENDIF
 SELECT Cuen
 STORE YESNO(Vmens07) TO vElimina
 IF vElimina
    DO Fox_Lock WITH 1, Bloquea
    IF Bloquea
       DELETE NEXT 1
       IF !BOF()
          SKIP -1
       ENDIF
    ENDIF
    UNLOCK
 ENDIF
 DO Vista
 RETURN


PROCEDURE Lista
*--------------
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE ' Listado del Plan Contable ' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE  1  TO vLista
STORE  1  TO vtipo 
STORE SPACE(02) TO vPrograma,vSubPrograma
IF LASTKEY()=27
   RETURN
ENDIF
@ 01,01 SAY "      # de D?gitos : " 
@ 01,22 GET vLista FUNCTION '^ Resumen (04 D?gitos);Divisionaria (06 D?gitos);Detallado (15 D?gitos)' 
@ 04,01 SAY "              Tipo : "
@ 04,22 GET vtipo  FUNCTION '^ General;Por Programa' WHEN vlista=3
@ 07,01 SAY "          Programa : "
@ 07,22 GET vPrograma WHEN vTipo=2 PICTURE "!!" VALID VAL_PARA(vPrograma,'CODPRG',' ',22,21)
@ 09,01 SAY "      Sub-Programa : "
@ 09,22 GET vSubPrograma WHEN vTipo=2 PICTURE "!!"
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
vPrograma = ALLTRIM(vPrograma)
RELEASE WINDOW LIS
IF OKCANCEL = 1
	SELECT Cuen
	SET ORDER TO CuentaS7
	DO CASE
		CASE vLista=1			&& AND vtipo<>3
			SET FILTER TO RIGHT(Cuen.Cuenta,11)= '00000000000'
			GO TOP
		CASE vLista=2			&& AND vtipo<>3
			SET FILTER TO RIGHT(Cuen.Cuenta,9)= '000000000' 			&&OR RIGHT(Cuen.Cuenta,8)= '00000000'
			GO TOP
		CASE vLista=3 AND vtipo=1
			GO TOP
		CASE  vLista=3 AND vtipo=2
			IF vSubPrograma=SPACE(02)
				SET FILTER TO ( ALLTRIM(SUBSTR(Cuen.Cuenta,4,2)) = ALLTRIM(vPrograma) ) .OR. SUBSTR(Cuen.Cuenta,3,8)="00000000" .OR. SUBSTR(Cuen.Cuenta,4,7)="0000000"
			 ELSE
				SET FILTER TO ( ALLTRIM(SUBSTR(Cuen.Cuenta,4,2)) = ALLTRIM(vPrograma).AND.ALLTRIM(SUBSTR(Cuen.Cuenta,6,2)) = ALLTRIM(vSubPrograma) ) .OR. SUBSTR(Cuen.Cuenta,3,8)="00000000" .OR. ( SUBSTR(Cuen.Cuenta,4,7)="0000000" )
			ENDIF
			GO TOP
	ENDCASE
	IF !EOF()
		DO REPORTE WITH 2,"Cuentas"," Plan Contable ",1,.F.,.T.
	 ELSE
		DO StandBy WITH 'Dicho Programa no fue encontrado.'
	ENDIF
	SET FILTER TO
	ENDIF
SELECT Cuen
SET ORDER TO CuentaS1
GO BOTTOM
DO Vista
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    MENU   mMenu
  CLOSE DATABASE
  DO Logos WITH Rotulo1, Rotulo2
 RETURN


PROCEDURE TipoCuenta
*-------------------
PARAMETER xTipo
VALIAS = ALIAS()
DesTipo = ''
SELECT PARMA
SEEK 'TIPCLA'+xtipo
IF FOUND()
	DesTipo = descri
ENDIF
SELECT (valias)
RETURN (DesTipo)


PROCEDURE CodigoCuenta
*---------------------
PARAMETER DesTipo
xTipo = ''
DO CASE
   CASE DesTipo =  'Orden      '
        xTipo   =  'O'
   CASE DesTipo =  'Activo     '
        xTipo   =  'A'
   CASE DesTipo =  'Pasivo     '
        xTipo   =  'P'
   CASE DesTipo =  'Patrimonio '
        xTipo   =  'T'
   CASE DesTipo =  'Gesti?n    '
        xTipo   =  'G'
   CASE DesTipo =  'Resultado  '
        xTipo   =  'R'
   CASE DesTipo =  'Presupuesto'
        xTipo   =  'E'
   OTHERWISE
        xTipo   =  ' '
ENDCASE
RETURN( xTipo )



FUNCTION SubTitulo
*-----------------
PARAMETERS vLista,vTipo,vPrograma
RetVal=""
DO CASE 
   CASE  vLista=1 AND vtipo<>3
         RetVal = "RESUMEN"
   CASE  vLista=2 AND vtipo<>3
         RetVal = "POR DIVISIONARIA"
   CASE  vLista=3 AND vtipo=1
         RetVal = "DETALLADO"
   CASE  vLista=3 AND vtipo=2
         RetVal = "PROGRAMA: " + ALLTRIM(vPrograma) + SPACE(02) + ALLTRIM(VAL_PARA(vPrograma,'CODPRG','D'))
ENDCASE
IF LEN(RetVal)<>0
   RETURN "( " + ALLTRIM( RetVal ) + " )"
ENDIF   
RETURN RetVal


FUNCTION vCuenta
*---------------
STORE 0 to vDigi,vSpac
DO CASE
   CASE cuen.nivel = 1 
        vDigi = 1
		vdevolu = LEFT(Cuen.Cuenta,vDigi)
*		vdevolu = SPACE(vSpac) + LEFT(Cuen.Cuenta,vDigi)
   CASE cuen.nivel = 2 
		vDigi = 4
		vdevolu = LEFT(Cuen.Cuenta,vDigi)	
        
   CASE cuen.nivel = 3   
       	vDigi = 4
       	vdevolu = LEFT(Cuen.Cuenta,vDigi)+'.'+subst(Cuen.Cuenta,5,2)
   CASE cuen.nivel = 4
        vDigi = 4
		vdevolu = LEFT(Cuen.Cuenta,vDigi)+'.'+subst(Cuen.Cuenta,5,2)+'.'+subst(Cuen.Cuenta,7,2)
   CASE cuen.nivel = 5
        vDigi = 4
        vdevolu = LEFT(Cuen.Cuenta,vDigi)+'.'+subst(Cuen.Cuenta,5,2)+'.'+subst(Cuen.Cuenta,7,2)+'.'+subst(Cuen.Cuenta,9,2)
   CASE cuen.nivel = 6
        vDigi = 4
        vdevolu = LEFT(Cuen.Cuenta,vDigi)+'.'+subst(Cuen.Cuenta,5,2)+'.'+subst(Cuen.Cuenta,7,2)+'.'+subst(Cuen.Cuenta,9,2)+'.'+subst(Cuen.Cuenta,11,2)
   CASE cuen.nivel = 7
        vDigi = 4
        vdevolu = LEFT(Cuen.Cuenta,vDigi)+'.'+subst(Cuen.Cuenta,5,2)+'.'+subst(Cuen.Cuenta,7,2)+'.'+subst(Cuen.Cuenta,9,2)+'.'+subst(Cuen.Cuenta,11,2)+'.'+subst(Cuen.Cuenta,13,2)
ENDCASE
RETURN vdevolu
