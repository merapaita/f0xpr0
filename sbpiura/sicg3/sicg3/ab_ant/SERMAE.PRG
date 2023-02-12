**---------------------------------------------------------------------
** Sistema Region Grau                 Saiki Consultores
** Luis Castillo D
** 19-AGO-94
**---------------------------------------------------------------------
* ABRIENDO ARCHIVOS
USE ParMae IN 1 ORDER TAG ParMae1          ALIAS Parma
USE ArtMae IN 2 ORDER TAG ArtMae1          ALIAS Produ
USE IteArt IN 3 ORDER TAG IteArt1          ALIAS Iteart

*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08
Vmens01 = ' Cat logo de Existencias '
Vmens02 = 'Revisi¢n de servicios'
Vmens03 = 'C¢digo del servicio: '
Vmens04 = 'Dicho servicio no fue encontrado.'
Vmens05 = 'No existe servicio anterior.'
Vmens06 = 'No existe servicio siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste servicio?'
Vmens08 = 'No hay registros para procesar'

SELECT Produ
set filter to left(codart,1)='S'
GO TOP

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
HIDE POPUP ALL
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU nMenu
ENDDO

DO Fin_opcion
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 08,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 09,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10 ;
 FOOTER '®F5¯ Avanza'

 DEFINE WINDOW Wind_2 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 09,00 TO 23,79 DOUBLE ;
 TITLE Vmens02+'       ®Esc¯ Escoge'     COLOR SCHEME 10
* FOOTER '®Esc¯ Escoge'

 DEFINE MENU nMenu COLOR SCHEME 3
 DEFINE PAD revis   OF nMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF nMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF nMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF nMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF nMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF nMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF nMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF nMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF nMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF nMenu DO revis
 ON SELECTION PAD busca  OF nMenu DO busca
 ON SELECTION PAD anter  OF nMenu DO anter
 ON SELECTION PAD proxi  OF nMenu DO proxi
 ON SELECTION PAD corri  OF nMenu DO corri
 ON SELECTION PAD ingre  OF nMenu DO ingre
 ON SELECTION PAD elimi  OF nMenu DO elimi
 ON SELECTION PAD lista  OF nMenu DO lista
 ON SELECTION PAD termi  OF nMenu DO termi

 ACTIVATE SCREEN
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_2
 CLEAR

 @  4, 2 SAY "           CODIGO :"
 @  8, 2 SAY "      DESCRIPCION :"

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_2
 SELECT Produ
 SCATTER MEMVAR

 @  4,22 SAY m.CodArt

 @  8,22 SAY SUBSTR(m.Descri,1,54)
 @  9,22 SAY SUBSTR(m.Descri,55,10)

 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELE PRODU
 SET ORDE TO ArtMae1
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 IF YESNO("¨ En Orden Alfab‚tico ? [ NO = Por C¢digo]")
    SET ORDER TO ArtMae2
 ENDIF
 HIDE MENU nMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_2 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     XQ=SUBSTR(CodArt,1,7)   :H='Servicio' ,;
     Descri   :H='Descripci¢n' :73

     
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SET ORDER TO ArtMae1
 IF LASTKEY()=27
   SET RELA TO
   GOTO Vtemp
 ENDIF
 SHOW MENU nMenu
 ON KEY LABEL F10
 SET RELA TO
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
  vTemp = RECNO()
  DO FUNBUS
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


PROCEDURE Corri
*--------------
SELE PRODU
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
 ACTIVATE WINDOW Wind_2
 vIngreso='C'
 IF RLOCK() OR F_LOCK(1)
    @ 4,22 GET m.CodArt PICTURE '!!!.!!!'   DISABLE
    @ 8,22 GET m.Descri PICTURE '@S54'
    READ VALID Val_Read()
    IF LASTKEY() # 27
       SELECT Produ
       GATHER MEMVAR
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
ENDIF
UNLOCK
SELECT Produ
DO Vista
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 SELECT Produ
 vtemp = Recno()
 SCATTER MEMVAR BLANK
 vDescri=spac(20)
 DO Pantalla
 vIngreso ='I'
 vTipart='S'
 vCodArt='      '
 @ 4,22 GET vTipArt DISABLE 
 @ 4,23 GET vCodArt PICTURE '!!.!!!'  VALID LEN(alltrim(vCODART))=6
 READ
 m.CodArt = vTipArt + vCodArt
 IF LASTKEY() # 27
    SEEK m.CodArt
    IF FOUND()
       DO standby WITH "El Servicio ya est  registrado"
       go vtemp
       DO Vista
       RETURN
    ELSE
       @ 8,22 GET m.Descri PICTURE '@S54' VALID LEN(alltrim(vCODART))=6
   ENDIF
   READ VALID Val_Read()

   IF LASTKEY() # 27
       SELECT Produ
          IF F_Appd()
             GATHER MEMVAR
          ELSE
             *- No pudo agregar
             SELECT Produ
             GO BOTTOM
          ENDIF
    ELSE
      DO STANDBY WITH 'Proceso cancelado'
      go vtemp
   ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   go vtemp
 ENDIF
 UNLOCK
 SELECT Produ
 DO Vista
 RETURN

PROCEDURE Validar
*----------------
private vfun
vRec = Recno()
vKey = 'B'+iteart.CodArt
go top
seek vkey
vFun = .t.
if found() and !empty(Iteart.Descri)
   do standby with 'El C¢digo ya Existe..'
   vFun = .f.
endif
go vRec
return vFun

PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 STORE YESNO(Vmens07) TO vElimina
 IF vElimina
   IF RLOCK() OR F_LOCK(1)
      SELE ITEART
          SCAN FOR TIPART+SUBSTR(CODART,1,6)=SUBSTR(M.CODART,1,7)
          DO standby WITH "Espere un Momento,Por Favor..."
          DO Elimi_Item
      ENDS
      SELE PRODU
      DELETE NEXT 1
   ENDIF
   UNLOCK
 SKIP
 ENDIF
 DO Vista
 RETURN


PROCEDURE Lista
*--------------
 SELECT Produ
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vReg = RECNO()
 DO LISSER
 GO vReg
 RETURN


PROCEDURE LISSER
*----------------
IF escolor
	DEFINE POPUP Lismenu FROM 17,54 MARGIN SHADOW COLOR &L_COL
 ELSE
	DEFINE POPUP Lismenu FROM 17,54 MARGIN COLOR SCHEME C_POPUP
ENDIF

DEFINE BAR 1 OF Lismenu PROMPT ' \<Grupo gen‚rico   '
DEFINE BAR 2 OF Lismenu PROMPT ' \<Servicios       '

ON SELECTION POPUP Lismenu DEACTIVATE POPUP

ACTIVATE POPUP Lismenu
DO CASE
	CASE BAR()=1
		SELECT Parma
		set filt to tipo='CODGES'
		go top
		DO REPORTE WITH 2,"SerPrv3"," Servicios " ,1,.F.,.T.
	CASE BAR()=2
		IF !YESNO('¨ Reporte por 1 Grupo Gen‚rico ?')
			SET FILTER TO LEFT(CODART,1) = 'S'
			DO REPORTE WITH 2,"SerPrv"," Servicios " ,1,.F.,.T.
		 ELSE
			ACTIVATE WINDOW STANDBY
			vCod = '   '
			@01,02 SAY 'C¢digo: ' GET vCod PICTURE '!!!' valid Val_para(vCod,'CODGES',' ',10,10)
			READ
			DEACTIVATE WINDOW STANDBY
			IF LASTKEY() # 27
				SELECT Produ
				cFil = FILTER()
				SET FILTER TO LEFT(CODART,3) = alltrim(vCod) AND LEFT(CODART,1) = 'S'
				DO REPORTE WITH 2,"SerPrv"," Servicios " ,1,.F.,.T.
				SET FILTER TO &cFil
			 ELSE
				DO STANDBY WITH " No se tiene proveedores en este cat logo "
			ENDIF
		ENDIF
ENDCASE
SELE PRODu
RETURN   

PROCEDURE Agreg_Item
*-----------------
IF F_appd()
   REPLACE Codart with SUBSTR(m.CodArt,2,6) ,;
           TipArt with SUBSTR(m.CodArt,1,1)
  RETURN .T.
ENDIF
RETURN .F.


PROCEDURE Elimi_item
*-------------------
select ITEART
if rlock()
   delete next 1
else
   do standby with 'No puede eliminar este Item.'
endif
return


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F5
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_2
  RELEASE    WINDOW wind_1
  RELEASE    MENU   nMenu
  RETURN


 FUNCTION BuscArt
 *----------------
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEART.CODART,6)
 *eek ALLTRIM(VCOD)
 if !found()
    vFun = ' *** ESTE SERVICIO NO ESTA REGISTRADO **** '
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


FUNCTION Val_Art1   && servicios
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