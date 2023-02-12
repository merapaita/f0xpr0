*--------------------------------------------------------------------------
* RegCot.Prg
* Atiende las Solicitudes de Cotizaciones
* Estado de la cotizacion:
*   '00' Emitida   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
*- Abriendo Archivos
PARAMETER vOpcion
SET EXCL OFF
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
IF vOpcion = 1
	USE Solcot   IN 2   order tag Solcot1  ALIAS Solcot
	USE AuxCot   IN 7   order tag Auxcot1  ALIAS Auxcot
 ELSE
	USE Solcot   IN 2   order tag Solcot2  ALIAS Solcot
	USE AuxCot   IN 7   order tag Auxcot2  ALIAS Auxcot
ENDIF
USE Itesc    IN 3   order tag Itesc1   ALIAS Itesc
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Itepec   IN 5   order tag ItePec2  ALIAS Itepec
USE Promae   IN 6   order tag Promae1  ALIAS Promae
USE Iteart   IN 8   order tag Iteart1  ALIAS Iteart

*- Mensajes de aviso al usuario
Vmens01 = ' Solicitud de Cotizaci¢n - Proveedor : REVISION '
Vmens02 = ' Recepci¢n de Cotizaci¢n '
Vmens04 = 'Dicho Solicitud de Cotizaci¢n no fue encontrada'
Vmens05 = 'No existe Solicitud de Cotizaci¢n anterior'
Vmens06 = 'No existe Solicitud de Cotizaci¢n siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚sta Solicitud de Cotizaci¢n ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Solicitud de Cotizaci¢n ha sido anulada'
Vmens10 = 'La Solicitud de Cotizaci¢n ya fue atendida'
Vmens11 = 'La Solicitud de Cotizaci¢n ha sido devuelta'

SELECT Solcot
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
ON KEY LABEL F5 DO VISTA_PROV

*- Inicia proceso
HIDE POPUP ALL
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Cotizaci¢n ' ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_9 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' ®Cuadro Comparativo de Cotizaciones¯ ' ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
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
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAR
 @  1, 2 SAY " N£mero Solicitud :"
 @  1,40 SAY "            Fecha :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Solcot
 DO Pantalla
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR

 @ 0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','Con Cotiz',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='50','Atendido ','         '))))
 @ 1,22 SAY m.Periodo
 @ 1,24 SAY '-'
 @ 1,25 SAY m.NumSc
 @ 1,60 SAY m.FecSc
 @  2,40 SAY iif(empty(m.numccc),'                   ','Cuadro Comparativo:')
 @  2,60 SAY iif(empty(m.numccc),'    ',m.numccc)

if !empty(m.Codprv1)
 @  3, 2 SAY "   Proveedor N§ 1 :  "
 @ 3,22 SAY m.codprv1
 @ 3,27 SAY val_prv( m.codprv1)
else
 @ 3,2 say SPac(77)
endif

if !empty(m.Codprv2)
 @  4, 2 SAY "   Proveedor N§ 2 :  "
 @ 4,22 SAY m.codprv2
 @ 4,27 SAY val_prv( m.codprv2)
else
 @ 4,2 say spac(77)
endif

if !empty(m.Codprv3)
 @  5, 2 SAY "   Proveedor N§ 3 :  "
 @ 5,22 SAY m.codprv3
 @ 5,27 SAY val_prv( m.codprv3)
else
 @ 5,2 say spac(77)
endif

if !empty(m.Codprv4)
 @  6, 2 SAY "   Proveedor N§ 4 :  "
 @ 6,22 SAY m.codprv4
 @ 6,27 SAY val_prv( m.codprv4)
else
 @ 6,2 say spac(77)
endif
if !empty(m.Codprv5)
 @  7, 2 SAY "   Proveedor N§ 5 :  "
 @ 7,22 SAY m.codprv5
 @ 7,27 SAY val_prv( m.codprv5)
else
 @ 7,2 say spac(77)
endif
if !empty(m.Codprv6)
 @  8, 2 SAY "   Proveedor N§ 6 :  "
 @ 8,22 SAY m.codprv6
 @ 8,27 SAY val_prv( m.codprv6)
else
 @ 8,2 say spac(77)
endif
 DO VISTA_HIJO
 RETURN

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT IteSc
if vOpcion=1
   BROWSE ;
     NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numsc TIMEOUT 0.001 ;
     WINDOW Wind_2 ;
     FIELDS;
     XX='-',;
     Desped      : H= 'Descripci¢n' :57 :W=.F. ,;
     Cansol      : H= 'Cantid' :P='9,999.999',;
     unimed      : H= 'Uni'      :W=.F. :6
else
   BROWSE ;
     NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numsc TIMEOUT 0.001 ;
     WINDOW Wind_2 ;
     FIELDS;
     XX='-',;
     AA=MLINE(DeTALLE,1)  : H= 'Descripci¢n' :79 :W=.F.
endif
SELE SOLCOT
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT SOLCOT
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NumSc INTO Auxcot
 SET SKIP TO AUXCOT
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumSc  :H=' N§ ' ,;
 FecSc  :H='Fecha' ,;
 TipDoc :H='DOC' ,;
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
 auxcot.codprv :H='CodPro',;
 auxcot.nompro :H='Proveedor'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 sele solcot
 DO Vista
 RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_sc  = 0
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero Solicitud : ' GET vPeriodo PICTURE '!!'
 @ 1,32 SAY '-' GET vNum_sc PICTURE '9999' VALID vBusca()
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_sc) .or. LASTKEY()=27
    RETURN
 ELSE
   SEEK  vPeriodo + vNum_sc
   IF !FOUND()
     DO standby WITH Vmens04
     GOTO Vtemp
   ELSE
     DO Vista
   ENDIF
 ENDIF
 RETURN

PROCEDURE vBusca
*---------------
vNum_sc=Padl(alltrim(str(vNum_sc,4)),4,'0')
retur .t.

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
    Do Pantalla
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
 SCATTER MEMVAR
 do case
    case m.estado = '00'
         do standby with ' La Solicitud NO tiene Cotizaci¢n registrada '
         return
    case m.estado = '70'
         do standby with ' La Solicitud ha sido Devuelta '
         return
    case m.estado = '99'
         do standby with ' La Solicitud est  Anulada '
         return
 endcase
 ACTIVATE WINDOW Wind_1
 DO PANTALLA
 @ 1,22 GET m.Periodo  DISABLE
 @ 1,24 SAY '-'
 @ 1,25 GET m.NumSc    DISABLE
 @ 1,60 GET m.FecSc    DISABLE
 sele auxCOT
 set filter to periodo=m.periodo and numsc=m.numsc
 GO TOP
 IF EOF()
    DO STANDBY WITH 'No se tiene registrado a ningun Proveedor '
 endif
 vCod=1
 SCAN
    vN=STR(vCod,1)
     m.Codprv&vN = auxcot.codprv
     @ vCod+2, 2 SAY "   Proveedor N§ &vN :  "
     @ vCod+2,22 GET m.Codprv&vN VALID valprv()
     Read
     IF LASTKEY() # 27
        DO WHILE .T.
              Ok = Trabaja_Hijo()
              IF LASTKEY() # 27
                 IF YESNO('¨ Conforme ?')
                    ok=.t.
                    EXIT
                 ENDIF
              ELSE
                 IF YESNO('¨ Cancela ?')
                    ok = .F.
                    EXIT
                ENDIF
              ENDIF
           ENDDO

           IF Ok .AND. LASTKEY() # 27
                 SELECT Solcot
                 GATHER MEMVAR
           ELSE
                 SELECT Solcot
                 EXIT
           ENDIF
     ELSE
       DO STANDBY WITH ' Siguiente Proveedor '
     ENDIF
 sele auxCOT
 vCod=vCod+1
 ENDSCAN
 set filter to
 UNLOCK ALL
 SELECT solcot
 DO Vista
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Parma
 SEEK 'CORRELSOLCOT'

 SELECT Solcot
 SCATTER MEMVAR
 do case
    case m.estado = '20'
         do standby with ' La Solicitud ya tiene Cotizaci¢n registrada '
         return
    case m.estado = '70'
         do standby with ' La Solicitud ha sido Devuelta '
         return
    case m.estado = '99'
         do standby with ' La Solicitud est  Anulada '
         return
 endcase

 DO Pantalla
 @ 1,22 GET m.Periodo  DISABLE
 @ 1,24 SAY '-'
 @ 1,25 GET m.NumSc    DISABLE
 @ 1,60 GET m.FecSc    DISABLE
 sele auxCOT
 set filter to periodo=m.periodo and numsc=m.numsc
 GO TOP
 IF EOF()
    DO STANDBY WITH 'No se tiene registrado a ningun Proveedor '
 endif
 vCod=1
 SCAN
     vN=STR(vCod,1)
     m.Codprv&vN = auxcot.codprv
     @ vCod+2, 2 SAY "   Proveedor N§ &vN :  "
     @ vCod+2,22 GET m.Codprv&vN VALID valprv()
     Read
     IF LASTKEY() # 27
        DO WHILE .T.
              Ok = Trabaja_Hijo()
              IF LASTKEY() # 27
                 IF YESNO('¨ Conforme los precios ?')
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
                 m.estado='20'
                 SELECT Solcot
                 GATHER MEMVAR
           ELSE
                 SELECT Solcot
                 EXIT
           ENDIF
     ELSE
       DO STANDBY WITH 'Proveedor cancelado'
     ENDIF
 sele auxCOT
 vCod=vCod+1
 ENDSCAN
 set filter to
 UNLOCK ALL
 SELECT solcot
 DO Vista
 RETURN

PROCEDURE Trabaja_Hijo
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°° [*] Selecciona°°°°° [Space Bar] Desselecciona °°°°°° F10->Terminar°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F9 DO VISOBS
 ON KEY LABEL F10 KEYBOARD CHR(23)
 sele itesc
 if vOpcion = 1
    BROWSE ;
        NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo + m.numsc ;
        WINDOW Wind_9 ;
        FIELDS;
        Desped      : H= 'Descripci¢n' :39 :W=.F. ,;
        Cansol      : H= 'Cantid' :P='9,999.999' :W=.f.,;
        unimed      : H= 'Uni'      :W=.F. :6 ,;
        preuni&vN   : H= 'Pre_&vN'  :P='9,999,999.999' :V=COSTO():F,;
        valtot&vN   : H= 'Tot_&vN'  :P='9,999,999.99' :W=.F.
 else
    BROWSE ;
        NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo + m.numsc ;
        WINDOW Wind_9 ;
        FIELDS;
        XX=MLINE(DETALLE,1)     : H= 'Descripci¢n' :64 :W=.F. ,;
        valtot&vN   : H= 'Tot_&vN' :V=ACT():F :P='9,999,999.99'
 endif
 ON KEY LABEL F10
 ON KEY LABEL F9
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Solcot
 RETURN

PROCEDURE ACT
*------------
if rlock()
   replace PREUNI&vN with valtot&vN,cansol with 1
endif
return .t.

PROCEDURE costo
*--------------
if rlock()
   replace valtot&vN with cansol*preuni&vN
endif
return .t.

PROCEDURE Elimi
*--------------
SELE SOLCOT
 do case
    case m.estado = '70'
         do standby with ' La Solicitud ha sido Devuelta '
         return
    case m.estado = '99'
         do standby with ' La Solicitud est  Anulada '
         return
 endcase
 velimina = YESNO('¨ Desea ELIMINAR los proveedores de esta solicitud ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
    SELE ITESC
    SET FILTER TO ItesC.Numsc=SolCot.Numsc
    GO TOP
    SCAN
        FOR vcod =1 to 6
          vN=STR(vCod,1)
          replace Preuni&vN with 0 ,;
                  Valtot&vN with 0
        ENDfor
    ENDSCAN
    SET FILTER TO

    SELE SOLCOT
    vCod=1
    for vcod = 1 to 6
      vN=STR(vCod,1)
      replace Codprv&vN with space(4)
    ENDfor
    REPLACE ESTADO WITH '00'
 ENDIF
 DO Vista
 UNLOCK
 RETURN


PROCEDURE Agreg_Item
*-----------------
sele itesc
IF F_appd()
  REPLACE NumSc  WITH m.NumSc ,;
          CodArt WITH Itepec.CodArt ,;
          CodCad WITH Itepec.CodCad ,;
          CanSol WITH vCanreq ,;
          UniMed WITH Itepec.Coduni ,;
          DesPed WITH Itepec.Descri
  RETURN .T.
ENDIF
RETURN .F.


PROCEDURE Marca
*-----------------
DO CASE
   CASE llave='*'
        if rlock()
           REPLACE llave WITH '*', estado with '20'
        ENDIF
        RETURN .T.
   CASE llave=' '
        if rlock()
           REPLACE llave WITH ' ', estado with '  '
        ENDIF
        RETURN .T.
ENDCASE
RETURN .F.

PROCEDURE Lista
*--------------
select SolCot
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   vNum_sc  = SOLCOT.NUMSC
   vPeriodo = RIGHT(DTOC(DATE()),2)
   ACTIVATE WINDOW standby
   @ 1,01 SAY 'Ingrese N£mero Solicitud : ' GET vPeriodo PICTURE '!!'
   @ 1,32 SAY '-' GET vNum_sc PICTURE '!!!!' 
   READ
   DEACTIVATE WINDOW standby
   IF LASTKEY()=27
      DO VISTA
      RETURN
   ENDIF
   SEEK VPERIODO+VNUM_SC
   IF !FOUND()
      GO VTEMP
      DO REVIS
   ENDIF
   aV=Numsc
   bV=Periodo
   SET RELATION TO PERIODO+NumSc INTO Itesc
   SET RELATION TO PERIODO+NumSc INTO Auxcot Addi
   SET SKIP TO ITESc
  *SET FILT TO SOLCOT.ESTADO='20' AND SOLCOT.Numsc=aV and SOLCOT.Periodo = bV
   SET FILT TO SOLCOT.Numsc=aV and SOLCOT.Periodo = bV
   SET MEMO TO 112
   IF vOpcion=1
	   do reporte with 2,"cUcOCO",' Cuadro Comparativo de Cotizaciones '
   ELSE
       do reporte with 2,"cUcOCOS",' Cuadro Comparativo de Cotizaciones '
   ENDIF
  *do reporte with 2,"LisCot",' Cuadro Comparativo de Cotizaciones '
   SET FILTER TO
   SET RELATION TO
   SET MEMO TO 56
endif
select SolCot
go vtemp
DO VISTA
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_c1
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv&vN = iif( empty(m.codprv&vN),m.codprv&vN,padl(alltrim(m.codprv&vN),4,'0'))
xx = val_prv( m.codprv&vN,.t.,vCod+2,27)
if xx
   return .t.
endif
*@ 6,198 say vfun
*@ 6,19 say vfun
return vfun

function valsC
*-----------------
parameter vnumSc
private vfun
vfun = .t.
m.numSc =padl(alltrim(str(vnumSc,4)),4,'0')
if m.numSc  = '0000' or empty(m.numSc)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_Art(CodArt,.F.)
IF XX
  SELECT Itepec
  REPLACE ;
          coduni  WITH Produ.coduni,;
          preuni  WITH Produ.preuni
  vFun = .T.
ENDIF
RETURN vFun
*-----------*-----------*
FUNCTION OBSERVA
*---------------
vAlias = ALIAS()
SELE ITESC
SET MEMOWIDTH TO 56
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("OBSERVA")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA SAME
ELSE
   ACTIVATE WINDOW OBSERVA NOSHOW
ENDIF

MODIFY MEMO DETALLE WINDOW OBSERVA

IF !WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA
ENDIF

RELEASE WINDOW OBSERVA
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba el OBSERVA '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
SELE ITESC
IF !WEXIST("OBSERVA")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA SAME
ELSE
   ACTIVATE WINDOW OBSERVA NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW OBSERVA

IF !WVISIBLE("OBSERVA")
   ACTIVATE WINDOW OBSERVA
ENDIF
RELEASE WINDOW OBSERVA
RETURN .T.
