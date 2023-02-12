*--------------------------------------------------------------------------
* CONOC.Prg
* CONSULTA  Orden de Compra
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
USE IteOc    IN 2   order tag IteOc1   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE IteArt   IN 4   order tag Iteart3  ALIAS Iteart
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE maepre   IN 8   order tag maepre1  ALIAS maepre
USE itepar   in 9   order tag itepar1  ALIAS ITEPAR
USE HojCon   IN 10  ORDER TAG HojCon1  ALIAS Hoja
USE Itehc    IN 11  ORDER TAG Itehc1   ALIAS Itehc
USE HOJMOD   IN 12  ORDER TAG HOJMOD1  ALIAS HOJMOD



PUBLIC vMes,vPart,vNumOc,OQ
*- Mensajes de aviso al usuario
PRIVATE vTotOC

xtotoc = 0

Vmens01 = ' Orden de Compra : REVISION '
Vmens02 = ' Registro de Ordenes de Compra '
Vmens04 = 'Dicha Orden de Compra no fue encontrado'
Vmens05 = 'No existe Orden de Compra anterior'
Vmens06 = 'No existe Orden de Compra siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Orden de Compra ha sido anulado'
Vmens10 = 'Este Orden de Compra ya fue atendido'
Vmens11 = 'Este Orden de Compra ha sido devuelto'

SELECT Orden
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE '®F4¯ Imprime  ° ®F7¯ Seguimiento  ° ®F9¯ Detalle :Item  ° ®F12¯ Visa' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,63 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_4 FROM 20,48 TO 22,78 ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
 @  1, 2 SAY "       N£mero O/C :"
 @  1,40 SAY "            Fecha :"
 @  2, 2 SAY "        Proveedor :"

 @  4, 2 SAY " F.Financiamiento :"
 @  4,40 SAY "             Tipo :"
 @  5, 2 SAY "         Programa :"
 @  6, 2 SAY "      Subprograma :"
 @  7, 2 SAY "Actividad/Proyect :"
 @  8, 2 SAY "       Calendario :"
 @  8,40 SAY "              H/C :"
 @  9, 2 SAY "    Observaciones :"

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ON KEY LABEL F5
 ON KEY LABEL F8
 SELECT Orden
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WIND_1
 ON KEY LABEL F9 DO VISTA_DET
 SCATTER MEMVAR
 @  0,02 SAY IIF(m.Tipdoc='OK' AND m.estado#'5','O/C Visado','             ')
 @  0,60 SAY vEstOc(m.Estado)
 @  1,22 SAY m.Periodo
 @  1,25 SAY m.NumOc
 @  1,60 SAY m.FecOc
 @  1,68 SAY IIF(!EMPTY(m.FecDesp),'<'+DTOC(m.FecDesp)+'>',SPAC(10))
 @ 2, 2 SAY "        Proveedor :"
 @ 2,22 SAY IIF(EMPTY(m.codprv),'Sin Codigo'+space(40),val_prv(m.Codprv))
 if m.Tipo = 'M'		
    @ 3, 2 SAY "        Documento :"
    @ 3,22 say m.Memoran
 else   
	 @ 3, 2 say spac(79)
 endif		
 @  4,22 SAY val_para(m.CodFte,'CODFTE','V',22,20)
 @  4,60 SAY val_para(m.Tipfun,'TIPFUN','A',60,15,2)
 @  5,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,40)
 @  6,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @  7,22 SAY IIF(!EMPTY(substr(m.codcal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,56,6),SPAC(56))
 IF alltrim(m.Tipfun)='I' 
    @  7,25 SAY '.'
	@  7,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
 endif	
 @  8,22 SAY val_para(m.NumMes,'FECMES','V',22,10)
 @  8,60 SAY m.Perhc+' '+m.Numhc
 @  9,22 SAY m.Observa PICTURE "@S56"

 DO VISTA_HIJO
 DO TOTAL
 RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
IF !EMPTY(m.NUMANU) OR !EMPTY(M.NUMREB)
   @ 0,0 SAY m.Valtot-m.Anultot picture '9,999,999.99'
else   
   @ 0,0 SAY m.Valtot picture '9,999,999.99'
endif   
return

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT IteOc
GO TOP   &&+alltrim(m.codfte)
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOc  TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo',;
   CanReq      : H= 'Cantidad' :P='99,999.99',;
   CodUni      : H= 'Uni'      :W=.F. :3,;
   Descri      : H= 'Descripci¢n' :26 :W=.F. ,;
   Numpec      : H= 'Pecs' :w=.f. ,;
   PreUni      : H= 'PreUni' :P='99,999.999' :W=.F. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.

SELE Orden

RETURN

PROCEDURE VISTA_DET
*------------------
hide popup all
SELECT IteOc
ON KEY LABEL F9 DO OBSERVA
ON KEY LABEL F7 
&&+alltrim(m.codfte)
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOc  ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo',;
   CanReq      : H= 'Cantidad' :P='99,999.99',;
   CodUni      : H= 'Uni'      :W=.F. :3,;
   Descri      : H= 'Descripci¢n' :26 :W=.F. ,;
   Numpec      : H= 'Pecs' :w=.f. ,;
   PreUni      : H= 'PreUni' :P='99,999.999' :W=.F. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.

SELE Orden
DO VISTA
ON KEY LABEL F9 DO VISTA_DET
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT ORDEN
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMOC+CODFTE INTO ITEOC
 SET SKIP TO ITEOC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumOc  :H=' N§ ' ,;
 Codfte :H='Fte' ,;
 FecOc  :H='Fecha' ,;
 CC=vEstOc(ORDEN.Estado) :H='Estd' :4,;
 iteOc.descri :H='Articulo ' :36 ,;
 iteOc.coduni :H='Unid' ,;
 iteOc.Canreq :H='Cantid'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
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
 vNum_oC = '    '
 *vNum_Oc = 0
 VFTE = '   '
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/C : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNum_OC PICTURE '!!!!' VALID vBusca()
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_OC) .or. LASTKEY()=27
    RETURN
 ELSE
	SEEK  vPeriodo + vNum_OC &&+ ALLTRIM(VFTE)
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
vNum_Oc=Padl(alltrim(vNum_Oc),4,'0')
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


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F4
  ON KEY LABEL F9 
  ON KEY LABEL F7  
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
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun

