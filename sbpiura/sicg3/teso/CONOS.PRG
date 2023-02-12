*-------------------------------------------------------------------------
* CONOS.Prg
* CONSULTA  Orden de Servicio
* Estado :
*   '00' Emitido   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '20' afectado
*   '50' Atendido
*   '70' Devuelto
*   '99' Anulado
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE OrdSer   IN 1  order tag OrdSer1  ALIAS OrdSe
USE Solser   IN 2  order tag Solser1  ALIAS Solser
USE Parmae   IN 3  order tag Parmae1  ALIAS Parma
USE Promae   IN 4  order tag Promae1  ALIAS Promae
USE Calen    IN 5  order tag calen1   ALIAS calen
USE Clase    IN 6  order tag clase1   ALIAS clase
USE itepar   in 9  ORDER TAG itepar1  ALIAS ITEPAR           
USE maepre   IN 10 ORDER TAG maepre1  ALIAS maepre
USE HojCon   IN 11  ORDER TAG HojCon1  ALIAS Hoja
USE Cheque   IN 12  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag
USE ITEHC    IN 14  ORDER TAG ITEHC1  ALIAS ITEHC

PUBLIC vMes,vPart,CON,CONY,CONX
SET MEMOWIDTH TO 56
*- Mensajes de aviso al usuario
Vmens01 = ' Orden de Servicio : REVISION '
Vmens02 = ' Registro de Ordenes de Servicio '
Vmens04 = 'Dicho Orden de Servicio no fue encontrado'
Vmens05 = 'No existe Orden de Servicio anterior'
Vmens06 = 'No existe Orden de Servicio siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Orden de Servicio ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Orden de Servicio ha sido anulado'
Vmens10 = 'Este Orden de Servicio ya fue atendido'
Vmens11 = 'Este Orden de Servicio ha sido devuelto'
on key label f2 do VisObs
on key label F4 do imprimir
SELECT OrdSe
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

 DEFINE WINDOW WINDO_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 14,01 TO 16,79 ;
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
ACTIVATE WINDOW WINDO_0
CLEAR

 @  1, 2 SAY "       Numero O/S :"
 @  2, 2 SAY "        Fecha O/S :"
 @  3, 2 SAY "              H/C :"
 @  4, 2 SAY "        Proveedor :"
 @  6, 2 SAY " F.Financiamiento :"
 @  6,40 SAY "             Tipo :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      Subprograma :"
 @  9, 2 SAY " Activid/Proyecto :"
 @ 10, 2 SAY "       Calendario :"
 @ 11, 2 SAY "      Dependencia :"
 @ 12, 2 SAY "          Destino :"
 @ 13, 2 SAY "      Descripci¢n :"
 @ 18, 2 SAY "          Importe :"
 @ 18,40 say "          Partida :"
 @ 20, 2 SAY "         Contrato :"
 
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT OrdSe
 ON KEY LABEL F7 DO Estado WITH 'OS','m.perhc+m.numhc'
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WINDO_0
 SCATTER MEMVAR
 @  0,60 SAY vEstOc(m.estado)
 @  1,40 SAY SPACE(40) 
 @  1,22 SAY m.periodo
 @  1,24 SAY '/'
 @  1,25 SAY m.NumOS
 @  1,40 SAY IIF(EMPTY(m.Memoran),"       N£mero S/S :","       Memorandum :")
 @  1,60 SAY IIF(EMPTY(m.Memoran),m.Periodo+'.'+m.Numss,subst(m.Memoran,1,18))
 
 @  2,22 SAY m.FecOS
 @  2,60 SAY IIF(m.estado='50',fecliq,'        ')
 @  3,22 SAY m.Perhc+' '+m.Numhc
 @  4,22 say space(56)
 @  4,22 SAY m.CodPrv
 @  4,27 SAY val_prv(m.CodPrv) 
 @  6,22 SAY val_para(substr(m.CodCal, 5,3),'CODFTE','V',26,20)
 @  6,60 SAY val_para(m.Tipfun,'TIPFUN','A',60,15,2)
 @  7,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,40)
 @  8,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @  9,22 SAY IIF(!EMPTY(substr(m.codcal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,56,6),SPAC(56))
 IF alltrim(m.Tipfun)='I' 
    @  9,25 SAY '.'
	@  9,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
 else
*   @  9,22 SAY Spac(60)	
 endif	
 @ 10,22 SAY val_para(m.NumMes,'FECMES','V',22,40)
 @ 11,22 SAY val_para(m.CodDep,'CODDEP','A',22,56)
 @ 12,22 SAY m.destino picture "@s56"
 @ 13,22 clear to 17,79
 @ 13,22 SAY MLINE(Detalle,1) 
 @ 14,22 SAY MLINE(Detalle,2) 
 @ 15,22 SAY MLINE(Detalle,3) 
 @ 16,22 SAY MLINE(Detalle,4) 
 @ 17,22 SAY MLINE(Detalle,5) 
 @ 18,22 SAY m.ValTot   PICTURE '999,999.99'
 @ 18,60 SAY IIF(alltrim(m.tipfun)="I",m.CodPart,m.codanal)
 @ 20,22 SAY SPACE(40)
 @ 20,22 SAY IIF(EMPTY(m.ConCon),'       ',m.NumCon)
 @ 21,00 SAY PADC('° ®F2¯ Detalle O/S °  ®F4¯ Imprime ° ®F7¯ Seguimiento °',79,' ') COLOR W+/B
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT OrdSe
*set orde to 2
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
 BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumOs  :H=' N§ ' ,;
 CodFte :h='Fte' ,;
 AA=vEstOc(estado) :H='Estd':4 ,;
 FecOs  :H='Fecha' ,;
 CodDep :H='DEP',;
 valtot :H='Total':p='999,999.99' ,;
 ZZ=MLINE(DETALLE,1)  :H='Descripci¢n'

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
 vNum_OS  = 0
 vNumos = '0000'
 VFTE = '   '
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/S : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNumOS PICTURE '!!!!' 
 vNum_os = ALLTRIM(vNumos)
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_OS) .or. LASTKEY()=27
    RETURN
 ELSE
*  SEEK  vPeriodo + vNum_OS + ALLTRIM(VFTE)
   SEEK  vPeriodo + vNumOS &&+ ALLTRIM(VFTE)
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
vNum_OS=Padl(alltrim(str(vNum_OS,4)),4,'0')
retur .t.

PROCEDURE vBusca1
*---------------
vNumOS=Padl(alltrim(vNumOS),4,'0')
retur .t.

PROCEDURE Anter
*--------------
SELE ORDSE
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
SELE ORDSE
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


PROCEDURE MUESPT
*---------------
IF alltrim(m.tipfun)="I"
   @ 18,60 SAY m.CodPart PICTURE '!!.!!' 
ELSE   
   @ 18,60 SAY m.codanal PICTURE '!!.!!' 
ENDIF   




PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F6
  ON KEY LABEL F7
  ON KEY LABEL F4
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW WINDO_0
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


FUNCTION DETALLE
*---------------
vAlias = ALIAS()

ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Detalle")
   DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Descripci¢n del O/S ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle SAME
ELSE
   ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE WINDOW Detalle

IF !WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle
ENDIF

RELEASE WINDOW Detalle
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba el Detalle '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Detalle")
   DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Descripci¢n del O/S ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle SAME
ELSE
   ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW Detalle

IF !WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle
ENDIF
RELEASE WINDOW Detalle
SELECT (vALIAS)
RETURN .T.


PROCEDURE VALCCC
*---------------
PRIVATE AS,vnumccc
USE SOLCOT  IN 13  ORDER TAG Solcot2  ALIAS solcot
USE IteSC   IN 14  ORDER TAG Itesc1   ALIAS Itesc
USE IN 15

AS=ALIAS()
VCC=ORDSE.NUMSS
SELEct ITESC
SET RELA TO PERIODO+NUMSC INTO SOLCOT
SET ORDE TO ITESC5
SEEK VCC
VNUMCCC=IIf(FOUND()," CC/C :"+SOLCOT.NUMCCC,'   ')
set rela to
SELEct (as)

USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag
USE ITEHC    IN 14  ORDER TAG ITEHC1  ALIAS ITEHC
USE HOJMOD   IN 15  ORDER TAG HOJMOD1  ALIAS HOJMOD

return vnumccc


