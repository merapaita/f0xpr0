*-------------------------------------------------------------------------
* OrdSer.Prg
* Registra el Orden de Servicio
* Estado :
*   '00' Emitido   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '20' afectado
*   '50' Atendido
*   '70' Devuelto
*   '99' Anulado
*--------------------------------------------------------------------------
*- Abriendo Archivos
PARAMETERS  key_docfte,SISTEMA

IF PARAMETERS()=0
   key_docfte = ''
ENDIF

USE OrdSer   IN  1  ORDER TAG OrdSer1  ALIAS OrdSe
USE Solser   IN  2  ORDER TAG Solser1  ALIAS Solser
USE Parmae   IN  3  ORDER TAG Parmae1  ALIAS Parma
USE Promae   IN  4  ORDER TAG Promae1  ALIAS Promae
USE Calen    IN  5  ORDER TAG calen4   ALIAS calen
USE maepre   IN 10  ORDER TAG maepre1  ALIAS maepre
USE HojCon   IN 11  ORDER TAG HojCon1  ALIAS Hoja
USE Cheque   IN 12  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag
USE ITEHC    IN 14  ORDER TAG ITEHC1   ALIAS ITEHC
USE ITEOS1   IN 16  ORDER TAG ITEOS11  ALIAS ITEOS1
USE USUARIO  IN 20  ORDER TAG USUARIO1 ALIAS USU

PUBLIC vMes,CON,CONY,CONX,VMS,v_reg,vcadena,GH
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW WINDO_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

 DEFINE WINDOW Wind_2 FROM 12,04 TO 18,75 DOUBLE ;
 TITLE '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT '\aNula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW WINDO_0
CLEAR

 @  1, 2 SAY "       Numero O/S :"
 @  1,40 SAY "       N£mero S/S :"
 @  2, 2 SAY "        Fecha O/S :"
 @  3, 2 SAY "              H/C :"
 @  4, 2 SAY "        Proveedor :"

 @  5, 2 SAY "        Cad.Func. :"
 @  6, 2 SAY " Fte. Financiami. :"
 
 @  7, 2 SAY "          Funci¢n :"
 @  8, 2 SAY "         Programa :"
 @  9, 2 SAY "      Subprograma :"
 @ 10, 2 SAY "   Activ./Proyec. :"

 @ 11, 2 SAY "       Calendario :"
 @ 12, 2 SAY "      Dependencia :"
 @ 13, 2 SAY "          Destino :"
 @ 14, 2 SAY "      Descripci¢n :"
 @ 19, 2 SAY "          Importe :"
 @ 20, 2 SAY "	            IGV :"
 @ 20,40 SAY "         Contrato :" 
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ON KEY LABEL F7 DO ESTADOS
 SELECT ORDSE
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WINDO_0

 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C') 

 @  0,02 SAY IIF(EMPTY(m.User),space(50),'     Elaborado por: ')+user COLOR SCHEME 02
 @  0,60 SAY vEstOc(m.estado) COLOR SCHEME 02
 @  1,40 SAY SPACE(40) 
 @  1,22 SAY m.periodo
 @  1,24 SAY '-'
 @  1,25 SAY m.NumOS
 @  1,40 SAY IIF(EMPTY(m.Memoran),"       N£mero S/S :","       Memorandum :")
 @  1,60 SAY IIF(EMPTY(m.Memoran),m.Periodo+'.'+m.Numss,subst(m.Memoran,1,18))
 @  2,22 SAY m.FecOS
 @  2,60 SAY IIF(m.estado='50',fecliq,'        ')
 @  3,22 SAY m.Perhc+' '+m.Numhc
 @  4,22 say space(56)
 @  4,22 SAY m.CodPrv
 @  4,27 SAY val_prv(m.CodPrv) 
 @  5,22 SAY val_codcad(m.codcad,m.periodo,'D',22,30)
 @  6,22 SAY val_para(m.CodFte,'CODFTE','D',22,30)
 @  7,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  8,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  9,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @ 10,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)
 @ 11,22 SAY val_para(m.NumMes,'FECMES','D',22,40)
 @ 12,22 SAY val_para(m.CodDep,'CODDEP','A',22,56)
 @ 13,22 SAY m.destino picture "@s56"
 @ 14,22 clear to 18,79
 @ 14,22 SAY MLINE(Detalle,1) 
 @ 15,22 SAY MLINE(Detalle,2) 
 @ 16,22 SAY MLINE(Detalle,3) 
 @ 17,22 SAY MLINE(Detalle,4) 
 @ 18,22 SAY MLINE(Detalle,5) 
 @ 19, 2 SAY "          Importe :"
 @ 19,22 SAY m.ValTot   PICTURE '999,999.99'
 @ 20, 2 SAY "              IGV :"
 @ 20,22 SAY m.IGV      pict '9,999.99'
 @ 20,40 SAY "         Contrato :" 
* @ 20,22 SAY SPACE(40)
 @ 20,60 SAY IIF(EMPTY(m.ConCon),'       ',m.NumCon)

 @ 21,00 SAY PADC('° ®F2¯ Detalle O/S °  ®F4¯ Imprime ° ®F7¯ Seguimiento °',79,' ') COLOR W+/B
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT OrdSe
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
 VFTE = '  '
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/S : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNumOS PICTURE '!!!!' 
 vNum_os = ALLTRIM(vNumos)
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_OS) .or. LASTKEY()=27
    RETURN
 ELSE
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


function valprv
*--------------
private xx, vfun
vfun = .f.
codprv = iif( empty(codprv),codprv,padl(alltrim(codprv),4,'0'))
xx = val_prv( codprv,.t.)
if xx
   replace codprv with promae.codprv , nompro with promae.nompro
   return .t.
endif
return vfun


PROCEDURE Lista
*--------------
SELECT ORDSE
SCATTER MEMVAR
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LISSER
endif
select Ordse
go vtemp
DO VISTA
RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT ordse
VCON = RECNO()
SCATTER MEMVAR
vNumOs = m.Nummes+m.Numos
SET relation to PERIODO+m.Numos into iteos1
set filt to Nummes+NumOs=vNumos
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"LisOsX",' Orden de Servicios ',2
endif
set filter to
SELECT ordse
GO VCON
DO VISTA
RETURN


PROCEDURE lisSER
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 1,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Solicitud de Servicios' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue,vtoprv
vCli = SPAC(4)
vAno = '97'
VFTE = '  '
vcodfte = '  '
VPRV='    '
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ por Documento;en Detalle;Resumido;Control' 

@ 04,01 SAY "        Total O/S : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,5,22) when vlistado =1 
@ 05,01 SAY "              O/S : "
@ 05,22 GET vFte    PICTURE '!!' VALID VAL_PARA(VFTE,'CODFTE','C') WHEN VTOCLI=2 AND VLISTADO=1
@ 05,26 GET vAno    picture '!!'  WHEN VTOCLI=2 and vlistado=1
@ 05,28 SAY '-'
@ 05,29 GET vCli    WHEN VTOCLI=2 and vlistado=1 PICTURE '!!!!' VALID VO() AND ValOrd()
@ 06,01 SAY "Todos Proveedores : " GET vTOprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) when (vlistado #1 AND VLISTADO#4)
@ 07,01 SAY "    Proveedor O/S : "
@ 07,22 GET vPrv    WHEN VTOprv=2 PICTURE '!!!!' VALID val_prv(vprv,.t.)

@ 09,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,11,22)  WHEN (VLISTADO=2 OR VLISTADO=3 OR VLISTADO=4)
@ 11,01 SAY "           Fuente : "
@ 11,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 AND (VLISTADO=2 OR VLISTADO=3 OR VLISTADO=4)

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' WHEN vtocli=1 AND (VLISTADO=2 OR VLISTADO=3 OR VLISTADO=4)

@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados' WHEN vtocli=1 and (vlistado=2 OR  vlistado=3 OR VLISTADO=4)

@ 20,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   IF vListado # 4
      INDEX ON  IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
                FOR iif(vtofue=1,.t.,Codfte=alltrim(vCodfte)) .and. ;
	            IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado='00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado='20',iif(vTipPro=5,Estado='99',Estado='50'))))) 
      SET FILTER TO IIF(vToCli =1,.T.,Periodo+NumOs+Codfte = vAno+vCli+ALLTRIM(vFte)) AND iif(vToPrv=2,Codprv=vPrv,.t.)
   ELSE
      INDEX ON  IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
                FOR iif(vtofue=1,.t.,Codfte=alltrim(vCodfte)) .and. busprv()
   ENDIF   
   SET INDEX TO (VIND)
   COUNT ALL TO vTotoS
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Orden Servicio',IIF(vTipPro=2,'Listado Orden de Servicio Pendientes',IIF(vTipPro=3,'Listado Orden de Servicios Afectados',IIF(vTipPro=4,'Listado Orden de Servicios Anulados','Listado Orden de Servicios Liquidados'))))   
   IF !EOF()
     do case 
        case VLISTADO=1
	        DO REPORTE WITH 2,"LisOsX",' Ordenes de Servicios ',2
        case VLISTADO=2
    	 	DO REPORTE WITH 2,"LisOrds",' Ordenes de Servicios ',1,.F.,.T.
        case VLISTADO=3     	
    	 	DO REPORTE WITH 2,"LisOrsX",' Ordenes de Servicios ',1,.F.,.T.        
        case VLISTADO=4
    	 	DO REPORTE WITH 2,"Sercont",' Ordenes de Servicios ',1,.F.,.T.        
     endcase   
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

procedure busprv
*---------------
private ali,vkey
ali=alias()
vkey = codprv
select promae
SEEK vkey
if found()
   if estado='VG'
      vfun = .F.
   else
      vfun = .T.
   endif     
else   
   vfun = .T.
endif
select (ali)
return vfun


PROCEDURE VO
*-----------
vCli=Padl(alltrim(vCli),4,'0')
RETURN .T.

PROCEDURE Valord
*---------------
 SELECT ORDSE
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
 NumOs  :H=' N§ ' ,;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
 FecOs  :H='Fecha' ,;
 CodDep :H='DEP.',;
 CodFTE :H='FTE.' ,;
 DESOS  :H='Descripci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 ENDIF
 VCLI=NUMOs
 GOTO Vtemp
 RETURN .t.

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F6
  ON KEY LABEL F7
  ON KEY LABEL F4
  DEACTIVATE MENU
  RETURN

PROCEDURE ESTADOS
*-----------------
USE IN 2  
USE IN 4  
USE IN 5  
USE IN 6  
USE Cheque   IN 12  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag

DO Estado WITH 'OS','m.perhc+m.numhc'

USE Solser   IN 2  order tag Solser1  ALIAS Solser
USE Promae   IN 4  order tag Promae1  ALIAS Promae
USE Calen    IN 5  order tag calen1   ALIAS calen
USE Clase    IN 6  order tag clase1   ALIAS clase
RETURN

PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW WINDO_0
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

function valOS
*-----------------
parameter vnumOS
private vfun
vfun = .t.
m.numOS =padl(alltrim(str(vnumOS,4)),4,'0')
if m.numOS  = '0000' or empty(m.numOS)
   vfun = .f.
endif
return vfun

function valCt
*-----------------
parameter vnumCt
private vfun
vfun = .t.
m.numcon =padl(alltrim(str(vnumct,4)),4,'0')
if m.numcon  = '0000' or empty(m.numcon)
   vfun = .f.
endif
return vfun


procedure cont
*-------------
if yesno('Continua descripci¢n 2?')
   con = .t.
else
   con = .f.
endif 
retu 

procedure conx
*-------------
if yesno('Continua descripci¢n 3?')
   conx = .t.
else
   conx = .f.
endif 
retu 

procedure cony
*-------------
if yesno('Continua descripci¢n 4?')
   cony = .t.
else
   cony = .f.
endif 
retu 


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
SET ORDE TO ITESC1
SEEK VCC
VNUMCCC=IIf(FOUND()," CC/C :"+SOLCOT.NUMCCC,'   ')
set rela to
SELEct (as)

USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag

return vnumccc


PROCEDURE VUSUA
***************
PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

