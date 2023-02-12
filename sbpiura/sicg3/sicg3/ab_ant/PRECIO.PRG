*--------------------------------------------------------------------------
* Precio.Prg
* Visualiza Precios
* Estado :
*   '00' Emitida   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '20' Afectada
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE OrdCom   IN 1   order tag OrdCom1  ALIAS Orden
USE IteOc    IN 2   order tag IteOc5   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE IteArt   IN 4   order tag Iteart2  ALIAS Iteart
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE ArtMae   IN 8   order tag ArtMae1  ALIAS Produ

PUBLIC vMes,vPart
ON KEY LABEL F2 DO BUSCA
*- Mensajes de aviso al usuario
Vmens01 = ' Listado Precios : REVISION '
Vmens02 = ' Listado Precios '
Vmens04 = 'Dicha Orden de Compra no fue encontrado'
Vmens05 = 'No existe Orden de Compra anterior'
Vmens06 = 'No existe Orden de Compra siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Orden de Compra ha sido anulado'
Vmens10 = 'Este Orden de Compra ya fue atendido'
Vmens11 = 'Este Orden de Compra ha sido devuelto'

SELECT IteOc

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
HIDE POPUP ALL
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Revis

*- Activa men£ mientras vEn_accion es .T.
CLOSE DATA
RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
 vTempo = '                                                                 Lista  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 06,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 07,00 TO 23,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10
 
 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO revis

RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_0
CLEAR
 @ 1, 2 SAY "     C¢digo :"
 @ 2, 2 SAY "Descripci¢n :"
 @ 3, 2 SAY "     Unidad :" 
 
RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
IF EOF()
   DO Pantalla
   RETURN
ENDIF
ACTIVATE WINDOW WIND_0
SCATTER MEMVAR
 
@  1,17 SAY m.Codart
@  2,17 SAY m.Descri PICTURE '@60'
@  3,17 SAY m.Coduni
 
DO VISTA_HIJO
RETURN


PROCEDURE VISTA_HIJO
*-------------------
hide popup all
sele iteoc
SET ORDE TO 4
*SET FILT TO ITEOC.CODART=M.CODART
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY M.CODART TIMEOUT 0.001 ;
   WINDOW Wind_1 ;
   FIELDS;
   AA=IIF(empty(IteOc.Numpec),' ----',' '+IteOc.Numpec) : H= 'Pecosa' ,;   
   BB=IIF(empty(IteOc.NumOc), ' ----',' '+IteOc.NumOc)  : H= '  O/C ' ,;
   IteOc.FecOc      : H= 'Fech O/C',;  
   IteOc.PreUni     : H= '    Precio' :P='99,999.999' 
SET FILTER TO    
SET ORDE TO 5   
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo

RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT iteoc
HIDE MENU MMENU
SET ORDE TO ITEOC5
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
ACTIVATE SCREEN
vTempo = '°°°°°°°Presione ®F10¯ para seleccionar   ®F2¯ Busca    ®Esc¯ Cancelar°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW Wind_3 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 iteoc.codart :h='C¢digo' ,;
	 iteoc.descri :H='Descripci¢n' ,;
	 iteoc.coduni :H='Unidad'
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
   DEACTIVATE MENU MMENU
   ACTIVATE SCREE
   RETUR
ENDIF
ON KEY LABEL F10
DO PANTALLA
DO VISTA
ACTIVATE MENU mMenu
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vTemp = RECNO()
 vDesc = spac(60)
 activate window standby
 @ 1,1 say 'Ingrese Art¡culo : ' get vdesc picture '@S20'
 read
 deactivate window standby
 SELE iteoc
 seek alltrim(upper(vDesc))
 IF !found()
    DO standby WITH 'Art¡culo no hallado'
    GOTO Vtemp
 ELSE
    DO Revis
 ENDIF

RETURN
 
PROCEDURE vBusca
*---------------
vNum_Oc=Padl(alltrim(str(vNum_Oc,4)),4,'0')
retur .t.


PROCEDURE BUSPEC
*---------------
ACTIVATE WINDOW WIND_4
VNUMPEC='    '
@ 0,1 GET VNUMPEC PICTURE '!!!!'
READ
DEACTIVATE WINDOW WIND_4
SEEK M.PERIODO+ALLTRIM(VNUMPEC)
RETURN


PROCEDURE Lista
*--------------
SELECT ORDEN
SET RELATION TO PERIODO+NUMOC INTO ITEOC
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LisOrd
endif
SELECT ORDEN
SET RELATION TO
SET ORDE TO 1
go vtemp
DO VISTA
RETURN


PROCEDURE lisOrd
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 1,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Ordenes de Compra' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue
vCli = SPAC(4)
vAno = '95'
VFTE = '   '
vcodfte = '   '
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ Documento;Resumido' 

@ 05,01 SAY "        Total O/C : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) when vlistado =1
@ 07,01 SAY "              O/C : "
@ 07,22 GET vFte    PICTURE '!!!' VALID VAL_PARA(VFTE,'CODFTE','C') WHEN VTOCLI=2 AND vlistado=1
@ 07,26 GET vAno    picture '!!'   WHEN VTOCLI=2 and vlistado=1
@ 07,28 SAY '-'
@ 07,29 GET vCli    WHEN VTOCLI=2 and vlistado=1 PICTURE '!!!!' VALID VO() AND ValOrd()

@ 09,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,11,22)  WHEN VLISTADO=2
@ 11,01 SAY "           Fuente : "
@ 11,22 GET vCodFte  PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 AND VLISTADO=2

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' WHEN vtocli=1 and vlistado=2

@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos' WHEN vtocli=1 and vlistado=2

@ 20,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,NumOc,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOc))) TO (vInd) ;
      FOR IIF(vToCli=1,.T.,NumOc = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' )) ;
      and iif(vtofue=1,.t.,Codfte=alltrim(vCodfte))
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   SET MEMOWIDTH TO 43
   IF !EOF()
     SET SKIP TO ITEOC
     IF VLISTADO=1
        DO REPORTE WITH 2,"LisOc1",' Ordenes de Compra',1,.F.,.T.
     ELSE
     	DO REPORTE WITH 2,"LisOrdc",' Ordenes de Compra ',1,.F.,.T.
     ENDIF   
     SELECT ORDEN
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF
RETURN

PROCEDURE Valord
*--------------
SELECT ORDEN
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
SEEK vano+VCLI+ALLTRIM(vfte)
if !found()
    SET SKIP TO ITEOC
    GO TOP
	HIDE MENU mMenu
	ACTIVATE SCREEN
	vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO Logos WITH Rotulo1,vTempo
	ON KEY LABEL F10 KEYBOARD CHR(23)
	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	NumOc  :H=' N§ ' ,;
	FecOc  :H='Fecha' ,;
	ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	iteOc.descri :H='Articulo ' :36 ,;
	iteOc.coduni :H='Unid' ,;
	iteOc.Canreq :H='Cantid'
	vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO Logos WITH Rotulo1,vTempo
	IF LASTKEY()=27
	   SELE ORDEN
	   SET RELATION TO
	ENDIF
	SHOW MENU mMenu
	ON KEY LABEL F10
	SELE ORDEN
 endif
 vAno = Orden.Periodo	 
 VCli = Orden.NumOC
 vFte = Orden.Codfte
 RETURN 

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F9 
  HIDE WINDOW WIND_1
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_3
  RELEASE WINDOW wind_4
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv= iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.,2,26)
if xx
   return .t.
endif
return vfun

function valOc
*-----------------
parameter vnumOc
private vfun
vfun = .t.
m.numOc =padl(alltrim(str(vnumOc,4)),4,'0')
if m.numOc  = '0000' or empty(m.numOc)
   vfun = .f.
endif
return vfun

FUNCTION Observa
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle O/C ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF

RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF

RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,18 TO 20,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle O/C ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

procedure itep
*-------------
as=alias()
sele pecosa
seek iteoc.periodo+iteoc.numpec+iteoc.codfte
*OK1 = val_para(PECOSA.CodDep,'CODDEP','D',22,60)
OK1 = PECOSA.DESTINO
sele (as)
retu (ok1)

*--------------*-------------------
PROCEDURE ALAN
*-------------
RETU
AX=ALIAS()
SELE ALAN
SEEK ALLTRIM(M.CODFTE)+ALLTRIM(M.NUMOC)
IF FOUND()
   M.NUMMES = ALAN.NUMMES
ELSE
   M.NUMMES = '00'
ENDIF      
SELE (AX)
RETURN

FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo
_campo = VARREAD()
ORD=ORDER()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun=.f.
IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 2
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F8 DO BorrDet
      ON KEY LABEL F5 DO Agr2Det
      ON KEY LABEL F2 DO FunBusDet
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo' :W=.F. ,;
        Descri   :H='Nombre':70  ,;
        CodUni   :H='Unidad':7   
      vORD = RECNO()  
      GO TOP
      SCAN WHILE EMPTY(DESCRI)
           IF RLOCK()
              DELETE NEXT 1
           ENDIF    
      ENDSCAN  
      GO TOP
      GO vord
      ON KEY LABEL F10
      ON KEY LABEL F8
      ON KEY LABEL F5
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1
      
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
ENDIf
IF v_Fun
   SELECT iteoc
   IF RLOCK()
      REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
   ENDIF
endif
SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_iC
ON KEY LABEL F8  DO Elimi_iC
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun

