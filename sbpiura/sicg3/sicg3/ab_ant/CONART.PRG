**---------------------------------------------------------------------
** Sistema de Abastecimientos
**---------------------------------------------------------------------
* ABRIENDO ARCHIVOS
USE ParMae IN 1 ORDER TAG ParMae1          ALIAS Parma
USE ArtMae IN 2 ORDER TAG ArtMae1          ALIAS Produ
USE IteArt IN 3 ORDER TAG IteArt1          ALIAS Iteart

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08
Vmens01 = ' Cat?logo de Existencias '
Vmens02 = 'Revisi?n de Art?culos'
Vmens03 = 'C?digo del Art?culo: '
Vmens04 = 'Dicho Art?culo no fue encontrado.'
Vmens05 = 'No existe Art?culo anterior.'
Vmens06 = 'No existe Art?culo siguiente.'
Vmens07 = '?Est? seguro que desea ELIMINAR ?ste Art?culo?'
Vmens08 = 'No hay registros para procesar'

SELECT Produ
GO TOP

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
HIDE POPUP ALL
*- Inicia proceso
DO Inicia                    && Define ventanas, men?s, t?tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men? mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU nMenu
ENDDO

DO Fin_opcion
RETURN


PROCEDURE Inicia             && Crea ventanas, men?s y t?tulos
*---------------

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente                             Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 08,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 09,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10 ;
 FOOTER '?F5? Avanza'

 DEFINE WINDOW Wind_2 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 09,00 TO 23,79 DOUBLE ;
 TITLE Vmens02+'       ?Esc? Escoge'     COLOR SCHEME 10
* FOOTER '?Esc? Escoge'

 DEFINE MENU nMenu COLOR SCHEME 3
 DEFINE PAD revis   OF nMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF nMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF nMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF nMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD lista   OF nMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF nMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF nMenu DO revis
 ON SELECTION PAD busca  OF nMenu DO busca
 ON SELECTION PAD anter  OF nMenu DO anter
 ON SELECTION PAD proxi  OF nMenu DO proxi
 ON SELECTION PAD lista  OF nMenu DO lista
 ON SELECTION PAD termi  OF nMenu DO termi

 ACTIVATE SCREEN
 RETURN


PROCEDURE Pantalla           && Pinta m?scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR

 @  2, 2 SAY "         Cat?logo :"
 @  4, 2 SAY "      Descripci?n :"

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT Produ
 SCATTER MEMVAR

 @  2,22 SAY m.CodArt

 @  4,22 SAY SUBSTR(m.Descri,1,54)
 @  5,22 SAY SUBSTR(m.Descri,55,10)
* ACTIVATE WINDOW Wind_1
 DO VISTA_HIJO

 RETURN

PROCEDURE Vista_Hijo
*---------------------
 ACTIVATE WINDOW Wind_1
 SELE IteArt
   GO TOP
   seek LEFT(m.CodArt,7)
   IF FOUND()
      ON KEY LABEL F5 DO VISTA_PRO
      BROWSE ;
      NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY LEFT(m.CodArt,7) TIMEOUT 0.001;
      WINDOW Wind_1 FIELDS ;
        XX=SUBSTR(CODART,8,3) :H='Cod' ,;
        Descri      : H= 'Descripci?n' :64 ,;
        CodUni      : H= 'Unidad'      :8
*       PreUni      : H= 'Precio'   :P='99,999.99'
*       Codpart     : H= 'Partida'
   ELSE
        ACTIVATE WINDOW Wind_1
        CLEAR
        @ 5,33 SAY 'Sin Detalle'
   ENDIF
SELECT Produ
RETURN

PROCEDURE Vista_PRO
*---------------------
 SELE IteArt
   seek LEFT(m.CodArt,7)
   if found()
      BROWSE ;
      NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY LEFT(m.CodArt,7) ;
      WINDOW Wind_1 FIELDS ;
        XX=SUBSTR(CODART,8,3) :H='Cod' ,;
        Descri      : H= 'Descripci?n' :64 ,;
        CodUni      : H= 'Uni'      :8
*       PreUni      : H= 'Precio'   :P='99,999.99'
*       Codpart     : H= 'Partida'
   endif
SELECT Produ
RETURN

PROCEDURE Revis              && Revisi?n de BD en browse
*--------------
 SELE PRODU
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 IF YESNO("? En Orden Alfab?tico ? [ NO = Por C?digo]")
    SET ORDER TO 2
 ENDIF
 HIDE MENU nMenu
 ACTIVATE SCREEN
 vTempo = '???????????Presione ?F10? para seleccionar  o  ?Esc? para cancelar????????????'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SET RELATION TO LEFT(CODART,7) INTO ITEART
 SET SKIP TO ITEART
 BROWSE WINDOW Wind_2 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     XQ=SUBSTR(CodArt,1,7)   :H='Articulo' ,;
     Descri   :H='Descripci?n' :30,;
     XX=SUBSTR(IteArt.CodArt,8,3) :H='Det',;
     iteart.Descri   :H='Detalle' :60 ,;
     iteart.PreUni   :H='Precio',;
     iteart.Codpart  :H='Partida'

 vTempo = '????????????????????????????????????????????????????????????????????????????????'
 DO Logos WITH Rotulo1,vTempo
 SET ORDER TO 1
 IF LASTKEY()=27
   SET RELA TO
   GOTO Vtemp
 ENDIF
 SHOW MENU nMenu
 ON KEY LABEL F10
 SET RELA TO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b?squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
  vTemp = RECNO()
  IF !YESNO('?Por Art?culo?')
     DO FUNBUS
  ELSE
     SELE ITEART
     SET RELATION TO 'B'+LEFT(CODART,6) INTO PRODU
     DO FUNBUSDET
     SET ORDER TO 2
     BROWSE WINDOW Wind_3 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Iteart.CodArt  :H='Articulo':10 ,;
     iteart.Descri  :H='Descripci?n' :35,;
     Produ.CodArt  :H='Det':7,;
     Produ.Descri  :H='Detalle'
     SET ORDER TO 1
     SET RELA TO
     SELE PRODU
  ENDIF
  IF EOF()
    DO standby WITH Vmens04
    GOTO Vtemp
  ELSE
    DO Vista
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


PROCEDURE Lista
*--------------
 SELECT Produ
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vReg = RECNO()
 DO LisExi
 GO vReg
 RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F5
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    MENU   nMenu
  RETURN


PROCEDURE LisExi
*---------------
DEACTIVATE WINDOW WIND_1
  IF escolor
    DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
  ELSE
    DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
  ENDIF

  DEFINE BAR 1 OF Lismenu PROMPT ' \<Grupo gen?rico   '
  DEFINE BAR 2 OF Lismenu PROMPT ' g\<Rupo espec?fico '
  DEFINE BAR 3 OF Lismenu PROMPT ' por \<Producto     '

  ON SELECTION POPUP Lismenu DEACTIVATE POPUP

  ACTIVATE POPUP Lismenu
  DO CASE
     CASE BAR()=1
          SELECT Parma
          set filt to tipo='CODGEB'
          go top
          DO REPORTE WITH 2,"Artprv3"," Grupos Gen?ricos " ,1,.F.,.T.
          SET FILTER TO
     CASE BAR()=2
          IF !YESNO('? Reporte por 1 Grupo Gen?rico ?')
             SET FILTER TO LEFT(CODART,1) = 'B'
             DO REPORTE WITH 2,"Artprv1"," Art?culos " ,1,.F.,.T.
          ELSE
             ACTIVATE WINDOW STANDBY
             vCod = '   '
             @01,02 SAY 'C?digo: ' GET vCod PICTURE '!!!' valid Val_para(vCod,'CODGEB',' ',10,10)
             READ
             DEACTIVATE WINDOW STANDBY
             IF LASTKEY() # 27
                SELECT Produ
                SET FILTER TO LEFT(CODART,3) = alltrim(vCod) AND LEFT(CODART,1) = 'B'
                DO REPORTE WITH 2, "ArtPrv1", " Existencias por L?neas " ,1,.F.,.T.
             ELSE
                DO STANDBY WITH " No se tiene proveedores en este cat?logo "
             ENDIF
             SET FILTER TO
          ENDIF
     CASE BAR()=3
          SELECT iteart
          IF YESNO('? Reporte por 1 Grupo Espec?fico ?')
             ACTIVATE WINDOW STANDBY
             Cod='       '
             @ 1,1 say 'Ingrese C?digo :' get Cod valid VaLArt(Cod)
             read
             deactivate window standby
             SET FILTER TO 'B'+SUBSTR(CodArt,1,6)=ALLTRIM(COD)
          ENDIF
          GO TOP

          IF !EOF()
             DO REPORTE WITH 2, "Artprv2", " Lista por Productos " ,1,.F.,.T.
          ELSE
             DO STANDBY WITH "No se tiene existencias de esta l?nea"
          ENDIF
          SET FILTER TO
          SET ORDER TO 1
     OTHERWISE
  ENDCASE
  SELE PRODU
  RELEASE POPUP LisMenu
  DO VISTA
  RETURN

 FUNCTION BuscArt
 *----------------
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEART.CODART,6)
 *eek ALLTRIM(VCOD)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
 return vfun

 FUNCTION SiCto
 *-------------
 vReturn = .F.
 vAlias  = ALIAS()
 SELECT Parma
 SEEK "TIPEXI" + m.TipExi
 IF TRIM(UPPER(Parma.DescriAux)) = "P.TERMINADO"
   vReturn = .T.
 ENDIF
 SELECT (vAlias)
 RETURN vReturn

FUNCTION VALARTA
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_Art(_Cod,.T.,1,18)
IF XX
  vCod = Produ.Codart
  vFun = .T.
ENDIF

RETURN vFun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,YY,ZZ,vFun
vFun = .t.
AS=ALIAS()
if empty(_Cod)
   ZZ=VAL_PARA(_Cod,'CODGEB','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art1(COD,.F.)
      IF XX
         _COD=PRODU.CODART
         vFun = .T.
      ENDIF
   ENDIF
ELSE
   vFun = .t.
ENDIF
SELECT (AS)
unlock all
RETURN vFun


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
   SET ORDER TO 1
   SET FILTER TO SUBSTR(CODART,2,2)=substr(alltrim(parma.codigo),2,2)
   ACTIVATE WINDOW STANDBY
   @ 1,14 SAY 'Espere un Momento ....' color W*
   GO TOP
   DEACTIVATE WINDOW STANDBY
   ACTIVATE SCREEN
   ON KEY LABEL F10 KEYBOARD CHR(23)
   ON KEY LABEL F2 DO FunBus
   DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
   BROWSE WINDOW _BusArt TITLE '???? [F10] Selecciona   [F2] Buscar ????' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
          CodArt   :H='C?digo'    :w=EMPTY(SUBSTR(CODART,5,3)),;
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
        *REPLACE &_campo WITH  xcod
        &_CAMPO = ALLTRIM(XCOD)
      ENDIF
      v_fun = .T.
  ENDIF
ENDIF
SET ORDER TO 1
SELECT (mAlias)
RETURN v_fun
