*--------------------------------------------------------------------------
* .Prg
* Anula Orden de Compra
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
parameter vOpcion
*- Abriendo Archivos
if vOpcion = 1
   USE PteAnu   IN 1   order tag PteAnu3  ALIAS AnuPA
   USE OrdCom   IN 2   order tag OrdCom1  ALIAS Orden
   USE IteOc    IN 3   order tag IteOc1   ALIAS IteOc
else
   USE PteAnu   IN 1   order tag PteAnu4  ALIAS AnuPA
   USE OrdSer   IN 2   order tag OrdSer1  ALIAS Ordse
endif
USE Parmae   IN 4   order tag Parmae1  ALIAS Parma
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE Itepec   IN 6   order tag ItePec4  ALIAS Itepec
USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE Calen    IN 9   order tag calen1   ALIAS calen
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE HOJCON   in 12  order tag hojcon1  ALIAS hoja
USE COMPAG   in 13  order tag compag1  ALIAS Compag
USE Cheque   IN 14  ORDER TAG Cheque1  ALIAS Cheque

*- Mensajes de aviso al usuario

if vOpcion = 1
 Vmens01 = ' Parte Rebaja Orden de Compra : REVISION '
 Vmens02 = ' Registro de Parte Rebaja Ordenes de Compra '
 Vmens04 = 'Dicho Orden de Compra no fue encontrado'
 Vmens05 = 'No existe Orden de Compra anterior'
 Vmens06 = 'No existe Orden de Compra siguiente'
 Vmens07 = '¨ Desea ModificaR ‚ste Orden de Compra ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Este Orden de Compra ha sido Modificado'
 Vmens10 = 'Este Orden de Compra ya fue atendido'
 Vmens11 = 'Este Orden de Compra ha sido devuelto'
else
 Vmens01 = ' Parte Rebaja Orden de Servicio: REVISION '
 Vmens02 = ' Registro de Parte Rebaja Ordenes de Servicio '
 Vmens04 = 'Dicho Orden de Servicio no fue encontrado'
 Vmens05 = 'No existe Orden de Servicio anterior'
 Vmens06 = 'No existe Orden de Servicio siguiente'
 Vmens07 = '¨ Desea Modificar ‚ste Orden de Servicio ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Este Orden de Servicio ha sido Modificado'
 Vmens10 = 'Este Orden de Servicio ya fue atendido'
 Vmens11 = 'Este Orden de Servicio ha sido devuelto'
endif


SELECT AnuPA
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
 TITLE ' Detalle O/C:            ®F12¯ Habilita Item' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_0
CLEAR
 @  1, 2 SAY "       N£mero P/R :"
 @  2, 2 SAY "        Fecha P/R :"
 IF vOpcion =1
    @  1,40 SAY "       N£mero O/C :"
    @  2,40 SAY "        Fecha O/C :"
 else
    @  1,40 SAY "       N£mero O/S :"
    @  2,40 SAY "        Fecha O/S :"
 endif
 @  4, 2 SAY "        Proveedor :"

 @  6, 2 SAY "   Importe Rebaja :"

 @  8, 2 SAY " F.Financiamiento :"
 @  9, 2 SAY "         Programa :"
 @ 10, 2 SAY "      Subprograma :"
 @ 11, 2 SAY "         Proyecto :"

 @ 13, 2 SAY "           Motivo :"

 @ 15, 2 SAY "       Numero H/C :"
 @ 16, 2 SAY "       Numero C/P :"
 @ 17, 2 SAY "    Numero Cheque :"

 @ 19, 2 SAY "    Observaciones :"

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
ACTIVATE WINDOW Wind_0
SELECT AnuPA
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 SCATTER MEMVAR
 @  0,60 SAY IIF(m.Estado= '00','Pendiente    ',IIF( m.Estado = '20','En Cont.Pres',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='50','Atendido ','         '))))

 @  1,22 SAY m.NumPa
 @  1,60 SAY m.Periodo 
 @  1,63 SAY m.Codfte  
 @  1,67 SAY m.NumRef  

 @  2,22 SAY m.FecPa
 @  2,60 SAY m.FecRef

 @  4,22 SAY val_prv(m.Codprv)

 @  6,22 SAY m.ValTot PICTURE '999,999.99'

 @  8,22 SAY val_para(substr(m.CodCal, 5,3),'CODFTE','V',26,20)
 @  9,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,20)
 @ 10,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @ 11,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
 IF alltrim(m.Tipfun)='I' 
    @ 11,25 SAY '.'
	@ 11,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
 else
    @ 11,22 SAY Spac(60)	
 endif	
 @ 07,22 SAY SPAC(50)
 @ 13,22 SAY m.Motivo PICTURE '@S56'

 @ 15,22 SAY m.NumHC 
 @ 15,26 SAY '.'
 @ 15,27 SAY m.PerHC  
 
 @ 16,22 SAY m.NumCP  
 @ 16,26 SAY '.'
 @ 16,27 SAY m.PerCp  
 @ 17,22 SAY m.NumChq 
 @ 19,22 SAY m.Observa

RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT ANUPA
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
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 numpa  :H='N§Dc' ,;
 fecpa  :H='Fecha' ,;
 TipDoc :H='Doc' ,;
 numref :H='N§' ,;
 Fecref :H='Fecha' ,;
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Afec',IIF(Estado='9','Anul',IIF(Estado='40','Aten','Liqu')))) :H='Estd' ,;
 codprv :H='Prv' ,;
 codcal :H='Calendario',;
 observa :H='Observaci¢n'

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
 SELECT ANUPA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_Oc  = 0
 ACTIVATE WINDOW standby
 if vopcion=1
   @ 1,01 SAY 'Ingrese N£mero O/C : ' &&GET vPeriodo PICTURE '!!'
 else
   @ 1,01 SAY 'Ingrese N£mero O/S : ' &&GET vPeriodo PICTURE '!!'
 endif
 @ 1,23 GET vNum_Oc PICTURE '9999' VALID vBusca()
 READ

 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_Oc) .or. LASTKEY()=27
    RETURN
 ELSE
   SEEK vNum_Oc
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
vNum_Oc=Padl(alltrim(str(vNum_Oc,4)),4,'0')
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


function valprv
*--------------
private xx, vfun
vfun = .f.
codprv = iif( empty(codprv),codprv,padl(alltrim(codprv),4,'0'))
xx = val_prv( codprv,.t.)
if xx
   replace codprv with promae.codprv ,  nompro with promae.nompro
   return .t.
endif
return vfun


PROCEDURE Lista
*--------------
SELECT AnuPA
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
  vmensL= Iif( Vopcion=1,' PARTE ANULACION O/C',' PARTE ANULACION O/S')
  IF Vopcion=1
    SET ORDER TO 3
  ELSE
    SET ORDER TO 4
  ENDIF
  ACTIVATE WINDOW STANDBY
  vnumpa = m.numpa
  vperio = m.periodo
  @ 1,1 say 'Ingrese N§ Pte Rebaja : ' get vperio
  @ 1,29 get vnumpa
  read
  DEACTIVATE WINDOW STANDBY
  IF LASTKEY() = 27
  	RETURN
  ENDIF
  SET FILTER TO PERIODO = VPERIO AND NUMPA=VNUMPA
  if vOpcion = 1
       VNUMREF = BUSC_OC()
       do reporte with 2,"LisPRC1",vmensL,2
  else   
       VNUMREF = BUSC_OS()
       do reporte with 2,"LisPRs1",vmensL,2
  endif    
  SET FILT TO
endif
select AnuPA
go vtemp
DO VISTA
RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
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
m.codpr = iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.)
if xx
   return .t.
endif
return vfun

function valPA
*-----------------
parameter vnumPA
private vfun
vfun = .t.
m.numPA =padl(alltrim(str(vnumPA,4)),4,'0')
if m.numPA  = '0000' or empty(m.numPA)
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

PROCEDURE lisSER
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 1,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Solicitud de Servicios' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vTofue
vCli = SPAC(4)
vAno = '95'
VFTE = '   '
vcodfte = '   '

@ 02,01 SAY "        Total O/C : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) 
@ 04,01 SAY "              O/C : "
@ 04,22 GET vFte    PICTURE '!!!' VALID VAL_PARA(VFTE,'CODFTE','C') WHEN VTOCLI=2 
@ 04,26 GET vAno    picture '!!'  WHEN VTOCLI=2 
@ 04,28 SAY '-'
@ 04,29 GET vCli    WHEN VTOCLI=2 PICTURE '!!!!' VALID VO() AND ValOrd()

@ 06,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,11,22) 
@ 08,01 SAY "           Fuente : "
@ 08,22 GET vCodFte  PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2

*@ 10,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' WHEN vtocli=1 AND (VLISTADO=2 OR VLISTADO=3)

*@ 13,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados' WHEN vtocli=1 and (vlistado=2 OR  vlistado=3)

@ 17,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
*   INDEX ON  IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
*         FOR iif(vtofue=1,.t.,Codfte=alltrim(vCodfte)) .and. ;
*	         IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado='00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado='20',iif(vTipPro=5,'99','50'))))) 
   SET FILTER TO IIF(vToCli =1,.T.,Periodo+NumOC+Codfte = vAno+vCli+ALLTRIM(vFte)) 
   SET INDEX TO (VIND)
   COUNT ALL TO vTotoS
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Orden Compra',IIF(vTipPro=2,'Listado Orden de Servicio Pendientes',IIF(vTipPro=3,'Listado Orden de Servicios Afectados',IIF(vTipPro=4,'Listado Orden de Servicios Anulados','Listado Orden de Compra Liquidados'))))   
   IF !EOF()
IF .F.
     do case 
        case VLISTADO=1
	        DO REPORTE WITH 2,"LisOs1",' Ordenes de Compra ',2
        case VLISTADO=2
    	 	DO REPORTE WITH 2,"LisOrds",' Ordenes de Compra ',1,.F.,.T.
        case VLISTADO=3     	
    	 	DO REPORTE WITH 2,"LisOrsX",' Ordenes de Compra ',1,.F.,.T.        
     endcase   
ENDIF
	  do reporte with 2,"LisPRC1",vmensL  
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILTER TO
   CLOSE INDEX
   ERASE (VIND)
   
ENDIF
SELE ORDSE
SET ORDE TO 1

RETURN

PROCEDURE VO
*-----------
vCli=Padl(alltrim(vCli),4,'0')
RETURN .T.

PROCEDURE Valord
*---------------
 SELECT ORDEN
 DEFINE WINDOW WINDO_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 SEEK VANO+VCLI+ALLTRIM(VFTE)
 
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumOc  :H=' N§ ' ,;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
 FecOC  :H='Fecha' ,;
 CodFTE :H='FTE ' ,;
 OBSERVA  :H='Descripci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 ENDIF
 VCLI=NUMOC
 GOTO Vtemp
 RETURN .t.

*-----------*-----------*

PROCEDURE busc_oc
*---------------
private qw
qw = alias()
vck = anupa.periodo+anupa.numref+anupa.codfte
select orden
tot = 0
SEEK VCK
if found()
   tot = orden.valtot
endif
SELECT (QW)
return tot     
      
PROCEDURE busc_os
*---------------
private qw
qw = alias()
vck = anupa.periodo+anupa.numref+anupa.codfte
select ORDSE
tot = 0
SEEK VCK
if found()
   tot = ORDSE.valtot
endif
SELECT (QW)
return tot     
      
