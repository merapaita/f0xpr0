** --------------------------------------
** CUENTAS.PRG
** Plan de Cuentas
** AUTOR : UDEP - 1994
** --------------------------------------

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Plan Contable '
Vmens02 = 'Revisi¢n del Plan Contable'
Vmens03 = 'Digite c¢digo de Cuenta que desea :'
Vmens04 = 'Dicha Cuenta no fue encontrada'
Vmens05 = 'No existe Cuenta anterior.'
Vmens06 = 'No existe Cuenta siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚sta Cuenta?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Cuenta existe en los comprobantes del mes'

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
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
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


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  1, 2 SAY "            Cuenta:"
 @  3, 2 SAY "Descripci¢n Cuenta:"
 @  5, 2 SAY "             Nivel:"
 @  7, 2 SAY "           Detalle:"
 @  9, 2 SAY "              Tipo:"
 @ 11, 2 SAY "Contra cuenta Debe:"
 @ 13, 2 SAY "Contr.cuenta Haber:"
 @ 15, 2 SAY "        Debe/Haber:"
 @ 17, 2 SAY "Descripci¢n Ingl‚s:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT Cuen
 SCATTER MEMVAR
 @  1,22 SAY m.Cuenta   PICTURE '9999999999'
 @  3,22 SAY m.Descri
 @  5,22 SAY m.Nivel    PICTURE '99'
 @  7,22 SAY IIF(m.Detalle = 'S','Si','No')
 @  9,22 SAY IIF(m.Tipo    = 'A','Activo   ',IIF(m.Tipo ='P','Pasivo   ','Resultado'))
 @ 11,22 SAY m.DContra  PICTURE '9999999999'
 vreg = IIF(!EOF(),RECNO(),0)
 @ 11,33 SAY IIF(EMPTY(VAL_cuen(m.DContra)),SPACE(40),Val_cuen(m.DContra))
 @ 13,22 SAY m.HContra  PICTURE '9999999999'
 @ 13,33 SAY IIF(EMPTY(VAL_cuen(m.HContra)),SPACE(40),Val_cuen(m.HContra))
 IF vreg # 0
    GO vreg
 ENDIF
 @ 15,22 SAY IIF(DbHb='D','Deudora  ','Acreedora')
 @ 17,22 SAY m.DesIng
 @ 17,60 SAY "Se Ajusta: " + IIF(m.Ajuste,'Si',IIF(m.Tipo$"AP",'No','  '))
 @ 19,60 SAY "Ref.Oblig: " + IIF(m.RefObl,'Si',IIF(m.Detalle='S','No','  '))
 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Cuenta   :H=' Cuenta',;
     Descri   :H='      Descripci¢n',;
     Nivel    :H='Nivel',;
     Detalle  =IIF(Detalle='S','Si','No') :H='Detalle',;
     Tipo     = IIF(Tipo    = 'A','Activo   ',IIF(Tipo ='P','Pasivo   ','Resultado')) :H='  Tipo'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW standby
 STORE SPACE(10) TO Vbusca
 @ 1,2 SAY Vmens03 GET Vbusca PICT '9999999999'
 READ
 DEACTIVATE WINDOW standby
 IF Vbusca = SPACE(10) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK Vbusca
   IF .NOT. FOUND()
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
   XTipo     = IIF(m.Tipo    = 'A','Activo   ',IIF(m.Tipo ='P','Pasivo   ','Resultado'))
   XDbHb     = IIF(m.DbHb   ='D','Deudora  ','Acreedora')
   X1DbHb    = IIF(m.DbHb   ='D','@M Deudora  ,Acreedora','@M Acreedora, Deudora  ')
   xAjuste   = IIF(m.Tipo    $ "A P", IIF(m.Ajuste,'Si','No'),'  ')
   xRefObl   = IIF(m.RefObl,'Si',IIF(m.Detalle='S','No','  '))
   vmens     = ''
   
   @  1,22 SAY m.Cuenta   PICTURE '9999999999'
   @ 17,60 SAY "Se Ajusta:"
   @ 19,60 SAY "Ref.Oblig:"
   @  3,22 GET m.Descri
   @  5,22 GET m.Nivel    PICTURE '99'
   @  7,22 GET xDetalle   PICTURE x1Detalle    WHEN IngDeta()
   @  9,22 GET xTipo      PICTURE '@M Activo   ,Pasivo   ,Resultado'
   @ 11,22 GET m.DContra  PICTURE '9999999999' VALID EMPTY(m.DContra) .OR. Contra(m.DContra,.T.,11,22);
                          ERROR vmens
   @ 13,22 GET m.HContra  PICTURE '9999999999' VALID EMPTY(m.HContra) .OR. Contra(m.HContra,.T.,13,22);
                          ERROR vmens
   @ 15,22 GET XDbHb      PICTURE x1DbHb
   @ 17,22 GET m.DesIng
   @ 17,71 GET xAjuste    PICTURE '@M Si,No'   WHEN SUBSTR(xTipo,1,1) $ 'A P '
   @ 19,71 GET xRefObl    PICTURE '@M Si,No'   WHEN xDetalle='Si'
   READ
   
   m.DContra = IIF(EMPTY(m.HContra) .AND. !EMPTY(m.DContra),SPACE(10),m.DContra)
   m.HContra = IIF(EMPTY(m.DContra) .AND. !EMPTY(m.HContra),SPACE(10),m.HContra)
   m.Detalle = IIF(xDetalle $ 'Si','S','N')
   m.Tipo    = IIF(xTipo    $ 'Activo   ','A',IIF(xTipo $ 'Pasivo   ','P','R'))
   m.DbHb    = IIF(xDbHb    $ 'Deudora  ','D','H')
   m.Ajuste  = IIF(xAjuste  = "Si".AND. m.Tipo $ "A P ",.T.,.F.)
   m.RefObl  = IIF(xRefObl  = "Si".AND. m.Detalle = "S",.T.,.F.)
   IF vreg # 0
      GO vreg
   ENDIF
   IF LASTKEY()<>27 .AND. yesno("Confirme la correcci¢n")        && Si se presion¢ Esc, no graba
      GATHER MEMVAR
   ENDIF
   DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR BLANK
 @  1,22 GET m.Cuenta   PICTURE '9999999999'
 READ
 SELECT Cuen
 IF LASTKEY() = 27 .OR. m.Cuenta=SPACE(10)
    DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
    GO BOTTOM
 ELSE
   SEEK  m.Cuenta
   IF FOUND()
      DO standby WITH "Cuenta ya est  registrada. Proceda a corregir datos."
      DO Vista
      DO Corri
   ELSE
     IF !VerDeta()
        RETURN
     ENDIF
     @  3,22 CLEAR TO 23,78
     vreg = IIF(!EOF(),RECNO(),0)
     xDetalle = IIF(m.Detalle='S','Si','No')
     XTipo    = IIF(m.Tipo    = 'A','Activo   ',IIF(m.Tipo ='P','Pasivo   ','Resultado'))
     XDbHb    = IIF(m.DbHb   ='D','Deudora  ','Acreedora')
     xAjuste   = IIF(m.Tipo    $ "A P", IIF(m.Ajuste,'Si','No'),'  ')
     xRefObl   = IIF(m.RefObl,'Si',IIF(m.Detalle='S','No','  '))
     vmens    = ''

     @  1,22 SAY m.Cuenta   PICTURE '9999999999'
     @ 17,60 SAY "Se Ajusta:"
     @ 19,60 SAY "Ref.Oblig:"
     @  3,22 GET m.Descri
     @  5,22 GET m.Nivel    PICTURE '99'
     @  7,22 GET xDetalle   PICTURE "@M Si,No" WHEN IngDeta()
     @  9,22 GET xTipo      PICTURE "@M Activo   ,Pasivo   ,Resultado"
     @ 11,22 GET m.DContra  PICTURE '9999999999' VALID EMPTY(m.DContra) .OR. Contra(m.DContra,.T.,11,22);
                            ERROR vmens
     @ 13,22 GET m.HContra  PICTURE '9999999999' VALID EMPTY(m.HContra) .OR. Contra(m.HContra,.T.,13,22);
                            ERROR vmens
                            
     @ 15,22 GET XDbHb      PICTURE '@M Deudora  ,Acreedora'
     @ 17,22 GET m.DesIng
     @ 17,71 GET xAjuste    PICTURE '@M Si,No'   WHEN SUBSTR(xTipo,1,1) $ "A P "
     @ 19,71 GET xRefObl    PICTURE '@M Si,No'   WHEN xDetalle='Si'
     READ

     m.DContra = IIF(EMPTY(m.HContra) .AND. !EMPTY(m.DContra),SPACE(10),m.DContra)
     m.HContra = IIF(EMPTY(m.DContra) .AND. !EMPTY(m.HContra),SPACE(10),m.HContra)
     m.Detalle = IIF(xDetalle $ 'Si','S','N')
     m.Tipo    = IIF(xTipo    $ 'Activo   ','A',IIF(xTipo $ 'Pasivo   ','P','R'))
     m.DbHb    = IIF(xDbHb    $ 'Deudora  ','D','H')
     m.Ajuste  = IIF(xAjuste  = "Si".AND. m.Tipo $ "A P ",.T.,.F.)
     m.RefObl  = IIF(xRefObl  = "Si".AND. m.Detalle = "S",.T.,.F.)
     IF vreg # 0
       GO vreg
     ENDIF

     IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
       DO Fox_appd WITH Agrega
       IF Agrega
         GATHER MEMVAR
       ENDIF
       UNLOCK
     ELSE
       GO BOTTOM
     ENDIF
   ENDIF
 ENDIF
 DO Vista
 RETURN

FUNCTION VerDeta
*---------------
vnum = LEN(ALLTRIM(m.Cuenta))
DO WHILE vnum > 0
  vnum = vnum - 1
  SEEK SUBSTR(m.Cuenta,1,vnum) + SPACE(10 - vnum)
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
 SELECT Cuen
 DEFINE WINDOW Temp FROM 15,20 TO 20,60 DOUBLE ;
 TITLE 'Rango de Cuentas' COLOR SCHEME 10
 STORE SPACE(10) TO vcuenta1, vcuenta2
 ACTIVATE WINDOW Temp
 @ 1,1 SAY 'Cuenta Inicial:' GET vcuenta1 PICTURE '9999999999'
 @ 2,1 SAY '  Cuenta Final:' GET vcuenta2 PICTURE '9999999999'
 READ
 DEACTIVATE WINDOW Temp
 RELEASE WINDOW Temp
 IF LASTKEY() = 27
   DO Vista
   RETURN
 ENDIF
 SELECT Cuen
 SET FILTER TO Cuen.Cuenta >= vcuenta1 .AND. Cuen.Cuenta <= vcuenta2
 DO REPORTE WITH 2,"Cuentas"," Plan Contable "
 SET FILTER TO
 SELECT Cuen
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
