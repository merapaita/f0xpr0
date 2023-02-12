**----------------------------------------------------------------------------------
** PROMAE.PRG  Administra la lista de Proveedores
**
** 11- 08 -94
** SISTEMA DE LA REGION GRAU
** PIURA
** LUIS CASTILLO DURAN
**---------------------------------------------------------------------------------------------
USE  parmae   IN 1   ORDER TAG parmae1        ALIAS Parma
USE  promae   IN 2   ORDER TAG promae1        ALIAS Promae
USE  prvaux   IN 3   ORDER TAG prvaux1        ALIAS aux
USE  artmae   IN 4   ORDER TAG artmae1        ALIAS produ


SELECT PROMAE
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK
ON KEY LABEL F2 DO VISTA_PRO
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

DO Termina

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW wind_cte0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' CATALOGO DE PROVEEDORES ' COLOR SCHEME 10 ;
 FOOTER '± ®F2¯ Muestra Clasificaci¢n ±'

 DEFINE WINDOW wind_cte1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' Revisi¢n de Proveedores ' COLOR SCHEME 10

 DEFINE WINDOW wind_2P   FROM 12,10 TO 22,69 DOUBLE ;
 TITLE ' Detalle : Clasificaci¢n ' COLOR SCHEME 10 ;
 FOOTER '± ®Esc¯ Sale ±'

 DEFINE WINDOW wind_2Pa  FROM 14,04 TO 22,75 DOUBLE ;
 TITLE ' Clasificaci¢n del Proveedor ' COLOR SCHEME 10 ;

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
*DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
*DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
*DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
*ON SELECTION PAD corri  OF mMenu DO corri
*ON SELECTION PAD ingre  OF mMenu DO ingre
*ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi

 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW wind_cte0

 @  1, 2 SAY "          C¢digo : "
 @  1,23 SAY "/"
 @  1,40 SAY "   Fecha Ingreso : "

 @  3, 2 SAY "          Nombre : "
 @  4, 2 SAY "       Direcci¢n : "
 @  5, 2 SAY "        Tel‚fono : "
 @  5,40 SAY "             Fax : "
 @  6, 2 SAY "           R.U.C.: "
 @  6,40 SAY "           Sigla : "

 @  8, 2 SAY "   Lic.Municipal : "
 @  8,40 SAY "   Reg.Comercial : "
 @  9, 2 SAY "   Reg.Unificado : "
 @  9,40 SAY "  Reg.Industrial : "
 @ 10, 2 SAY "      Testimonio : "
 @ 10,40 SAY "      C.Apertura : "

 @ 12, 2 SAY "  Nombre R.Legal : "
 @ 13, 2 SAY "  Direcc.R.Legal : "
 @ 14, 2 SAY "     RUC R.Legal : "
 @ 15, 2 SAY "    L.E. R.Legal : "
 @ 16, 2 SAY "  Telef. R.Legal : "

 @ 18, 2 SAY "  Tipo Operaci¢n : "
 @ 19, 2 SAY "   Act.Econ¢mica : "

 @ 20, 2 SAY "          Ambito : "
 @ 21, 2 SAY "          M‚rito : "

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW wind_cte0
 SELECT Promae
 IF EOF()
    DO Pantalla
    RETURN
 ENDIF

 SCATTER MEMVAR
 @  0,60 SAY IIF(m.Estado='VG','Vigente','Anulado')

 @  1,22 SAY m.TipPro
 @  1,24 SAY m.Codprv
 @  1,60 SAY m.Fecing

 @  3,22 SAY m.Nompro
 @  4,22 SAY m.Dirpro
 @  5,22 SAY m.Telpro
 @  5,60 SAY m.Faxpro
 @  6,22 SAY m.Numruc
 @  6,60 SAY m.Sigla

 @  8,22 SAY m.LicMun
 @  8,60 SAY m.RegCom
 @  9,22 SAY m.RegUnf
 @  9,60 SAY m.RegInd
 @ 10,22 SAY m.Test
 @ 10,60 SAY m.Apert

 @ 12,22 SAY m.Rl_nom
 @ 13,22 SAY m.Rl_dir
 @ 14,22 SAY m.Rl_ruc
 @ 15,22 SAY m.Rl_le
 @ 16,22 SAY m.Rl_tel

 @ 18,22 SAY Val_para (m.TipOpe,'TIPOPE','V')
 @ 19,22 SAY Val_para (m.Acteco,'ACTECO','V')
 @ 20,22 SAY Val_para (m.Ambito,'AMBITO','V')
 @ 21,22 SAY m.Clasif

 RETURN

PROCEDURE Revis
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF
 vTemp = RECNO()
 IF YESNO( '¨ En orden alfab‚tico [ NO = N£merico] ?')
   SET ORDER TO 2
 ENDIF

 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 ON KEY LABEL F10    KEYBOARD CHR(23)

 BROWSE WINDOW wind_cte1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 CodPrv :H='C¢digo' ,;
 NomPro :H='Nombre',;
 Dirpro  :H='Direcci¢n',;
 NumRuc  :H='RUC' ,;
 xx=iif(m.tipPRo='S','Serv','Bien') :H='Tipo'

 ON KEY LABEL F10

 SET ORDER TO 1
 vTempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 IF LASTKEY()=27
   GOTO vtemp
 ENDIF
 SHOW MENU mMenu
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 IF escolor
    DEFINE POPUP Busmenu FROM 17,54 TO 21,78 SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Busmenu FROM 17,54 TO 21,78 COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR 1 OF Busmenu PROMPT '  \<C¢digo       '
  DEFINE BAR 2 OF Busmenu PROMPT '  \<Nombre       '

  ON SELECTION POPUP Busmenu DEACTIVATE POPUP

  ACTIVATE POPUP Busmenu
  SELECT PROMAE
  Vtemp = RECNO()
  DO CASE
     CASE BAR()=1
       ACTIVATE WINDOW standby
       STORE '0000' TO Vbusca
       @ 1,2 SAY 'C¢digo: ' GET Vbusca PICT '!!!!' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby
       IF LASTKEY() # 27
          vBusca = UPPER(ALLTRIM(vbusca))
       ENDIF
     CASE BAR() = 2
       ACTIVATE WINDOW standby
       STORE SPACE(40) TO Vbusca
       @ 1,1 SAY ' Nombre: ' GET Vbusca PICTURE '@S30' && COLOR SCHEME 7
       READ
       DEACTIVATE WINDOW standby


       IF LASTKEY() # 27
          vBusca = TRIM(UPPER(vBusca))
          SET ORDER TO 2
       ENDIF
     OTHERWISE
       RETURN
    ENDCASE

 IF EMPTY( Vbusca ) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicho Proveedor no fue encontrado"
     GOTO Vtemp
   ELSE
     DO Vista
   ENDIF
 ENDIF
 SET ORDER TO 1
 RETURN


PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT Promae
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH "Inicio de archivo: no existe Contratante anterior"
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Proxi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT Promae
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe Contratante siguiente"
   GO BOTTOM
 ELSE
   DO Vista
 ENDIF
 RETURN


PROCEDURE Corri
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 IF Estado = 'AN'
   * El proveedor esta anulado
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF

 SCATTER MEMVAR
 ACTIVATE WINDOW wind_cte0
 DO PANTALLA
 IF RLOCK() OR F_LOCK(1)
    @  1,22 GET m.TipPro PICTURE '!' Valid Val_para (m.TipPro ,'TIPPRO','C',22,20)
    @  1,24 GET m.Codprv DISABLE
    @  1,60 GET m.Fecing

    @  3,22 GET m.Nompro
    @  4,22 GET m.Dirpro
    @  5,22 GET m.Telpro
    @  5,60 GET m.Faxpro
    @  6,22 GET m.Numruc
    @  6,60 GET m.Sigla

    @  8,22 GET m.LicMun
    @  8,60 GET m.RegCom
    @  9,22 GET m.RegUnf
    @  9,60 GET m.RegInd
    @ 10,22 GET m.Test
    @ 10,60 GET m.Apert

    @ 12,22 GET m.Rl_nom
    @ 13,22 GET m.Rl_dir
    @ 14,22 GET m.Rl_ruc
    @ 15,22 GET m.Rl_le
    @ 16,22 GET m.Rl_tel

    @ 18,22 GET m.TipOpe PICTURE '!!!!!'   VALID Val_para (m.TipOpe,'TIPOPE',' ',22,20)
    @ 19,22 GET m.Acteco PICTURE '!!!!!!' VALID Val_para (m.Acteco,'ACTECO',' ',22,20)
    @ 20,22 GET m.Ambito PICTURE '!!!' VALID Val_para (m.Ambito,'AMBITO',' ',22,25)
    @ 21,22 GET m.Clasif PICTURE '!'

   READ VALID Val_Read()
   IF LASTKEY() <> 27
    SELECT AUX
    DO WHILE .T.
       Ok = Trabaja_prov()
       IF LASTKEY() # 27
          IF YESNO('¨ Conforme la modificaci¢n?')
             EXIT
          ENDIF
       ELSE
          IF YESNO('¨ Cancela la modificaci¢n ?')
             ok = .F.
             EXIT
          ENDIF
       ENDIF
   ENDDO
   IF Ok .AND. LASTKEY() # 27
      m.Giro = IIF(LEFT(m.Acteco,2)='PJ','JURIDICA','NATURAL')
      SELECT PROMAE
      GATHER MEMVAR
   ENDIF
 ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
 UNLOCK
 SELECT PROMAE
 DO Vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Promae
 SCATTER MEMVAR BLANK

 SELECT Parma
 SEEK 'CORRELPROVEE'
 = valpro ( Parma.NumEnt + 1 )

 SELECT Promae
 DO Pantalla
 STORE 'VG'  TO m.Estado
 m.fecing=date()
 @  1,22 GET m.TipPro PICTURE '!' Valid Val_para (m.TipPro ,'TIPPRO','C',22,20)
 @  1,24 GET m.Codprv DISABLE
 @  1,60 GET m.Fecing

 @  3,22 GET m.Nompro
 @  4,22 GET m.Dirpro
 @  5,22 GET m.Telpro
 @  5,60 GET m.Faxpro
 @  6,22 GET m.Numruc
 @  6,60 GET m.Sigla

 @  8,22 GET m.LicMun
 @  8,60 GET m.RegCom
 @  9,22 GET m.RegUnf
 @  9,60 GET m.RegInd
 @ 10,22 GET m.Test
 @ 10,60 GET m.Apert

 @ 12,22 GET m.Rl_nom
 @ 13,22 GET m.Rl_dir
 @ 14,22 GET m.Rl_ruc
 @ 15,22 GET m.Rl_le
 @ 16,22 GET m.Rl_tel

 @ 18,22 GET m.TipOpe PICTURE '!!!!!'   VALID Val_para (m.TipOpe,'TIPOPE',' ',22,20)
 @ 19,22 GET m.Acteco PICTURE '!!!!!!'  VALID Val_para (m.Acteco,'ACTECO',' ',22,20)
 @ 20,22 GET m.Ambito PICTURE '!!!'     VALID Val_para (m.Ambito,'AMBITO',' ',22,25)
 @ 21,22 GET m.Clasif PICTURE '!'

 READ VALID Val_Read()
 IF LASTKEY() <> 27
    SELECT PROMAE
    IF F_appd()
       SELECT AUX
       DO WHILE .T.
          Ok = Trabaja_prov()
          IF LASTKEY() # 27
             IF YESNO('¨ Confirme el ingreso ?')
                EXIT
             ENDIF
          ELSE
             IF YESNO('¨ Cancela el ingreso ?')
                ok = .F.
                EXIT
             ENDIF
          ENDIF
       ENDDO
       IF Ok .AND. LASTKEY() # 27
          m.Giro = IIF(LEFT(m.Acteco,2)='PJ','JURIDICA','NATURAL')
          SELECT PROMAE
          GATHER MEMVAR
          SELECT Parma
          SEEK 'CORRELPROVEE'
          REPLACE NumEnt WITH NumEnt + 1
       ENDIF
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    GO BOTTOM
 ENDIF
SELECT PROMAE
DO VISTA
RETURN

PROCEDURE Trabaja_Prov
*---------------------
 ACTIVATE SCREEN
 hide menu mmenu
 ACTIVATE WINDOW WIND_2Pa
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_pro
 ON KEY LABEL F8  DO Elimi_pro
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE aux
 SET ORDE TO 1
 SEEK m.codprv

 if !found()
    DO Agreg_Pro
 endif

 BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2PA KEY m.codprv FIELD ;
    CodCla : H='Clase' :V=VALART(CODCLA):F:W=EMPTY(CodCla),;
    xDescri  = Val_Fun('Produ','CodArt','Descri',CodCla): H= 'Descripci¢n' :60 :W=.F.

 DEACTIVATE WINDOW WIND_2Pa
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SCAN WHILE m.codprv=codprv
    IF F_LOCK(1)
      IF EMPTY(CodCla)
        DELETE NEXT 1
      ENDIF
    ENDIF
 ENDSCAN

 UNLOCK ALL

SHOW MENU MMENU
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10

 SELECT PROMAE
 RETURN


PROCEDURE Vista_Pro
*---------------------
 SELE AUX
 SET ORDE TO 1
 SEEK m.codprv

 if !found()
    DO STANDBY WITH 'Este proveedor no tiene Clasificaci¢n'
 else
   ACTIVATE WINDOW WIND_2P
   HIDE MENU mMenu
   BROWSE NOEDIT NOAPPEND NODELETE NOCLEAR KEY m.codprv FIELD ;
       CodCla : H='Clase' ,;
       xDescri  = Val_Fun('Produ','CodArt','Descri',CodCla): H= 'Descripci¢n' :44 :W=.F.  ,;
       xTipprv=iif(left(codcla,1)='B','Bien','Serv')  :H='Tipo' :W=.F.
 endif
 DEACTIVATE WINDOW WIND_2P
 SHOW MENU mMenu
 SELECT PROMAE
 RETURN



PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar"
   RETURN
 ENDIF

 IF ESTADO = 'AN'
   DO standby WITH "El contratante ya est  anulado"
 ENDIF

 IF escolor
   DEFINE POPUP Elimenu FROM 17,54 TO 21,78 SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP Elimenu FROM 17,54 TO 21,78 COLOR SCHEME C_POPUP
 ENDIF
 DEFINE BAR 1 OF Elimenu PROMPT ' \<Anula al Proveedor  '
 DEFINE BAR 2 OF Elimenu PROMPT ' \<Elimina f¡sicamente '

 ON SELECTION POPUP Elimenu DEACTIVATE POPUP
 ACTIVATE POPUP Elimenu
 DO CASE
   CASE BAR() = 1
     vElimina = .F.
   CASE BAR() = 2
     vElimina = .T.
   OTHERWISE
    RELEASE POPUP Elimenu
    RETURN
 ENDCASE
 RELEASE POPUP Elimenu

 IF RLOCK() .OR. F_lock(1)
   IF vElimina
     IF YESNO('¨Continua?')
       DELETE NEXT 1
     ENDIF
   ELSE
     REPLACE ESTADO WITH 'AN'
   ENDIF
 ENDIF
 UNLOCK
 GO BOTT
 DO Vista
 RETURN

PROCEDURE Agreg_Pro
*-----------------
IF F_appd()
  REPLACE codprv WITH m.codprv

  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Pro
*-------------------
SELECT Aux
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif

return

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN

PROCEDURE Termina
*----------------
  CLOSE DATA
  ON KEY LABEL F2
  RELEASE    WINDOW wind_cte0
  RELEASE    WINDOW wind_cte1
  RELEASE    MENU   mMenu
  ACTIVATE   SCREEN
  RESTORE    SCREEN FROM Principal
  RETURN

PROCEDURE Lista
*--------------
SELECT PROMAE
 IF EOF()
    RETURN
 ENDIF
 VTEMP = RECNO()
 go top
 DO LisPro
 SELECT promae
 GO VTEMP
ON KEY LABEL F2 DO VISTA_PRO
RETURN

FUNCTION VALDIR
*--------------
m.Dir_cob = m.Dir_cte
RETURN .T.

function valprv
*--------------
parameter vcli
private xx, vfun
vfun = .f.
vcli= iif( empty(vcli),vcli,padl(alltrim(vcli),4,'0'))
xx = val_prv( vcli,.t.,3,10)
if xx
   return .t.
endif
return vfun

FUNCTION VALPRO
*--------------
parameter vcodprv
private vfun
vfun = .t.
m.codprv=padl(alltrim(str(vcodprv,4)),4,'0')
if m.codprv = '0000' or empty(m.codprv)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,YY,ZZ,vFun
vFun = .t.
AS=ALIAS()
if empty(_Cod)
   ZZ=VAL_PARA(_Cod,'CODGE','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art(CodCla,.F.)
      IF XX
         SELECT Aux
         REPLACE CodCla WITH Produ.Codart
         vFun = .T.
      ENDIF
   ENDIF
ELSE
   vFun = .t.
ENDIF
SELECT (AS)
unlock all
ON KEY LABEL F2  DO VISTA_PRO
ON KEY LABEL F5  DO Agreg_pro
ON KEY LABEL F8  DO Elimi_pro
ON KEY LABEL F10 KEYBOARD CHR(23)
RETURN vFun


FUNCTION VALLIS
*--------------
PARAMETERS _Cod
PRIVATE XX,YY,ZZ,vFun
vFun = .t.
AS=ALIAS()
ZZ=VAL_PARA(_Cod,'CODGEB','C')
IF ZZ
   XX = Val_Art(CodCla,.T.)
   IF XX
      vFun = .T.
   ENDIF
ENDIF
SELECT (AS)
RETURN vFun


PROCEDURE LisPro1
*---------------
IF EOF()
   DO STANDBY WITH vMens08
   RETURN
ENDIF
vTemo = RECNO()

DEFINE WINDOW LIS FROM 2,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Proveedores' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1        TO vToCli,vOrden,vTipPro,vEstado,vTipLis
vCli = SPAC(4)

@ 01,01 SAY "  Total Proveedor : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "        Proveedor : "
@ 03,22 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!' VALID Valprv(vcli)

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Nombre;C¢digo;Ingreso;RUC'

@ 08,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Todos;Aptos;No aptos'

@ 11,01 SAY "   Tipo Proveedor : " GET vTipPro  FUNCTION '^ Todos;Bienes;Servicios'

@ 14,01 SAY "     Tipo Listado : " GET vTipLis  FUNCTION '^ General;Resumido;INAP'

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   DO CASE
      CASE vTipLis=1 OR vTipLis=3
         DO CASE
            CASE vEstado=1
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(FECing))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
            CASE vEstado=2
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(FECing))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
                 SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(!EMPTY(TEST) AND !EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)), (!EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)) ) AND !EMPTY(GIRO)
            CASE vEstado=3
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(FECing))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
                 SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(EMPTY(TEST) or EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)), (EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)) ) or EMPTY(GIRO)
         ENDCASE        
      CASE vTipLis=2
         DO CASE
            CASE vEstado=1
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(FECing))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
            CASE vEstado=2
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(FECing))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
                 SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(!EMPTY(TEST) AND !EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)), (!EMPTY(RL_LE) AND !EMPTY(REGUNF) AND !EMPTY(NUMRUC)) ) AND !EMPTY(GIRO)
            CASE vEstado=3
                 INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(FECing))) TO (vInd) ;
                 FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
                 SET FILT TO IIF(SUBSTR(ACTECO,1,2)='PJ',(EMPTY(TEST) or EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)), (EMPTY(RL_LE) or EMPTY(REGUNF) or EMPTY(NUMRUC)) ) or EMPTY(GIRO)
         ENDCASE
   ENDCASE
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   IF !EOF()
      IF vTipLis=1
         vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' por Bienes ',' por Servicio '))
         do reporte  with 2, 'LISPRO', ' Lista de Proveedores '
      ELSE
         IF vTipLis=2
            vTitulo=IIF(vEstado=1,' Proveedores Aptos ',' Proveedores no Aptos ')
            do reporte  with 2, 'LISPRO2', ' Lista de Proveedores '
         ELSE
            vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' por Bienes ',' por Servicio '))
            vTitu  =IIF(vEstado=1,' Todos  ',IIF(vEstado=2,'  Aptos  ','No Aptos'))
            ACTIVATE WINDOW STANDBY
            AN=STR(YEAR(DATE()),4)
            MES = '00'
            @ 1,10 SAY 'Ingrese Per¡odo -> ' GET AN PICTURE '!!!!'
            @ 1,35 GET MES PICTURE '!!' 
            read
            deactivate window standby
            do reporte  with 2, 'LISPROX', ' Lista de Proveedores '
         ENDIF
      ENDIF
   ELSE
      DO STANDBY WITH 'Archivo Vac¡o ...'
   ENDIF
   SET FILTER TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT PROMAE
SET ORDE TO 1
GO VTEMP
RETURN


PROCEDURE LisPro
*---------------
  IF escolor
    DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
  ENDIF
  public ZZ
  DEFINE BAR 1 OF Lismenu PROMPT ' \<Gen‚rico     '
  DEFINE BAR 2 OF Lismenu PROMPT ' por \<Cat logo '
  DEFINE BAR 3 OF Lismenu PROMPT ' por \<Proveedor'

  ON SELECTION POPUP Lismenu DEACTIVATE POPUP

  ACTIVATE POPUP Lismenu
  vTemp=recn()
  DO CASE
     CASE BAR()=1
          DO LISPRO1
     CASE BAR()=2
          IF escolor
             DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
          ELSE
             DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
          ENDIF
          DEFINE BAR 1 OF Lismenu PROMPT '\<Grupo gen‚rico'
          DEFINE BAR 2 OF Lismenu PROMPT '\<Clase         '
          ON SELECTION POPUP Lismenu DEACTIVATE POPUP
          ACTIVATE POPUP Lismenu
          DO CASE
             CASE BAR()=1
                  SELECT Aux
                  vTem=recno()
                  SET UNIQ ON
                  vInd = SYS(3) + '.IDX'
                  INDEX ON LEFT(CODCLA,3)+CODPRV TO (vInd)
                  SET INDEX TO (VIND)
                  IF !YESNO(' Todos los Grupos Gen‚ricos ?')
                     ACTIVATE WINDOW STANDBY
                     _Cod='   '
                     @ 1,1 say 'Ingrese C¢digo :' get _Cod PICTURE '!!!' valid VAL_PARA(_Cod,'CODGE','C')
                     read
                     DEACTIVATE WINDOW STANDBY
                     Cod=Parma.Codigo
                     SET FILT TO CODCLA=ALLTRIM(COD)
                  ENDIF
                  GO TOP
                  IF !EOF()
                     DO REPORTE WITH 2, "GenPrv1", " Lista por Grupo Gen‚rico "
                  ELSE
                     DO STANDBY WITH "No se tiene existencias de esta l¡nea"
                  ENDIF
                  SET UNIQ OFF
                  SET FILTER TO
                  CLOSE INDEX
                  SELE AUX
                  SET ORDER TO 1
                  go Bott
                  ERASE (VIND)
             CASE BAR()=2
                  SELECT Aux
                  SET ORDER TO 2
                   IF !YESNO(' Toda la Clasificaci¢n ?')
                      activate window standby
                      _Cod='       '
                      @ 1,1 say 'Ingrese C¢digo :' get _Cod PICTURE '!!!.!!!' valid Val_ART1(_Cod,.T.)
                      read
                      deactivate window standby
                      SET FILTER TO CODCLA=ALLTRIM(_COD)
                  ENDIF
                  GO TOP
                  IF !EOF()
                     DO REPORTE WITH 2, "GenPrv", " Lista por Catalogos "
                  ELSE
                     DO STANDBY WITH "No se tiene existencias de esta l¡nea"
                  ENDIF
                  SET FILTER TO
                  SET ORDER TO 1
          ENDCASE
     CASE BAR()=3
          SELECT Aux
          SET ORDER TO 1
          SET RELA TO CODPRV INTO PROMAE
          SET SKIP TO PROMAE
          GO TOP
          DEFINE WINDOW LIS FROM 3,15 TO 21,65 FLOAT DOUBLE TITLE 'Listado Proveedores' COLOR SCHEME 5
          ACTIVATE WINDOW LIS
          STORE 1        TO vToCli,vOrden,vTipPro,vEstado,VtipPro
          vCli = SPAC(4)

          @ 01,01 SAY "  Total Proveedor : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
          @ 03,01 SAY "        Proveedor : "
          @ 03,22 GET vCli   WHEN VTOCLI=2  PICTURE '!!!!' VALID Valprv(vcli)

          @ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Nombre;C¢digo;Ingreso;RUC'

          @ 08,01 SAY "           Estado : " GET vEstado  FUNCTION '^ Aptos;No aptos'

          @ 11,01 SAY "   Tipo Proveedor : " GET vTipPro  FUNCTION '^ Todos;Bienes;Servicios'

          @ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
          READ CYCLE
          RELEASE WINDOW LIS
          IF OKCANCEL = 1
             ACTIVATE WINDOW STANDBY
             @ 01,04 SAY 'Espere un momento........'
             vInd = SYS(3) + '.IDX'
             IF vEstado=1
                INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(Promae.FECing))) TO (vInd) ;
                      FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
                      SET FILT TO IIF(SUBSTR(PROMAE.ACTECO,1,2)='PJ',(!EMPTY(PROMAE.TEST) AND !EMPTY(PROMAE.RL_LE) AND !EMPTY(PROMAE.REGUNF) AND !EMPTY(PROMAE.NUMRUC)), (!EMPTY(PROMAE.RL_LE) AND !EMPTY(PROMAE.REGUNF) AND !EMPTY(PROMAE.NUMRUC)) );
                      AND !EMPTY(PROMAE.GIRO)
             ELSE
                INDEX ON IIF(vOrden=1,Promae.NomPro,IIF(vOrden=2,CodPrv,DTOS(Promae.FECing))) TO (vInd) ;
                      FOR IIF(vToCli=1,.T.,CodPrv = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,TipPro = 'B',TipPro = 'S' ))
                      SET FILT TO IIF(SUBSTR(PROMAE.ACTECO,1,2)='PJ',(EMPTY(PROMAE.TEST) or EMPTY(PROMAE.RL_LE) or EMPTY(PROMAE.REGUNF) or EMPTY(PROMAE.NUMRUC)), (EMPTY(PROMAE.RL_LE) or EMPTY(PROMAE.REGUNF) or EMPTY(PROMAE.NUMRUC)) );
                      OR EMPTY(PROMAE.GIRO)
             ENDIF

             SET INDEX TO (vInd)
             GO TOP
             DEACTIVATE WINDOW STANDBY

             IF !EOF()
                DO REPORTE WITH 2, "GenPrv2A", " Lista por Proveedor "
             ELSE
                DO STANDBY WITH "No se tiene existencias de esta l¡nea"
             ENDIF
             SET RELATION TO
             SET FILTER TO
             CLOSE INDEX
             ERASE (VIND)
           ENDIF
           SELECT PROMAE
           SET ORDE TO 1
     OTHERWISE
  ENDCASE
  RELEASE POPUP LisMenu
  RETURN


FUNCTION Val_Art1   && Articulos
*---------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo

_campo = VARREAD()

mAlias = ALIAS()
SELECT Produ
_OldWnd = WOUTPUT()

IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 1
      ACTIVATE WINDOW STANDBY
      @ 1,14 SAY 'Espere un Momento ....' color W*
      GO TOP
      IF EOF()
         DEACTIVATE WINDOW STANDBY
         ACTIVATE SCREEN
         SET FILTER TO
         v_fun = .f.
      ELSE
         DEACTIVATE WINDOW STANDBY
         ACTIVATE SCREEN
         ON KEY LABEL F10 KEYBOARD CHR(23)
         ON KEY LABEL F2 DO FunBus
         DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
         BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
                CodArt   :H='C¢digo'    :w=EMPTY(SUBSTR(CODART,5,3)),;
                Descri   :H='Nombre':60 :w=empty(descri)
         ON KEY LABEL F10
         ON KEY LABEL F2
         RELEASE WINDOW _BusArt
         SET FILTER TO

         IF !EMPTY(_OldWnd)
            ACTIVATE WINDOW &_OldWnd
         ENDIF

         IF Lastkey()=27
            V_FUN = .f.
         ELSE
            xcod = CodArt
            IF mMsg
               @ _x,_y SAY Descri
            ENDIF
            SELECT (mAlias)
            IF !_tipo
               REPLACE &_campo WITH  xcod
            ENDIF
            v_fun = .T.
        ENDIF
      ENDIF
   ELSE
      SEEK xcod
      IF mMsg .AND. FOUND()
         @ _x,_y SAY Descri
      ENDIF
      v_fun = FOUND()
   ENDIF
ENDIF
SET ORDER TO 1
SELECT (mAlias)
RETURN v_fun

 FUNCTION BuscArt
 *----------------
 PARAMETER VCODIGO
 as=alias()
 sele produ
 set orde to 1
 Seek ALLTRIM(VCODIGO)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=produ.descri
 endif
 sele (as)
 return vfun
