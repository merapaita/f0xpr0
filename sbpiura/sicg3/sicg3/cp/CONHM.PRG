*--------------------------------------------------------------------------
* comH.Prg
* consulta HOJAS DE MODIFICACION
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
* FECHA: 12/10/94
*--------------------------------------------------------------------------
USE hojmod   IN 1   order tag hojmod1   ALIAS Hojmod
USE HojCon   IN 3   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 4   order tag Itehc1    ALIAS Itehc
USE PARMAE   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Clase    IN 9   order tag Clase1    ALIAS Clase

USE Promae   IN 10  order tag Promae1   ALIAS Promae
USE Personal IN 11  order tag Personal2 ALIAS Personal

USE Cuentas  IN 12  order tag Cuentas6 ALIAS Cuenta
USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre



PUBLIC VALCS, vCodPrg ,vCodSub , vProyec ,vCodact , vSubpry ,vTotahc, vResto

VALCS = .T.
*- Mensajes de aviso al usuario

 Vmens01 = ' Hoja de Modificaci¢n : REVISION '
 Vmens02 = ' Registro de Hoja de Modificaci¢n '
 Vmens04 = 'Dicho Hoja de Modificaci¢n no fue encontrado'
 Vmens05 = 'No existe Hoja de Modificaci¢n anterior'
 Vmens06 = 'No existe Hoja de Modificaci¢n siguiente'
 Vmens07 = '¨ Desea ANULAR ‚ste Hoja de Modificaci¢n ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Esta Hoja de Modificaci¢n ha sido anulada'
 Vmens10 = 'Este Hoja de Modificaci¢n ya fue atendida'
 Vmens11 = 'Este Hoja de Modificaci¢n ha sido devuelto'
ON KEY LABEL F2 DO VISOBS
SELECT Hojmod
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_1H FROM 00,00 TO 13,79  DOUBLE ;
 TITLE 'Registro Hoja Modificacion' COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE '>Estad¡stica Diaria por Objeto del Gasto      ®F9¯ Item   ' COLOR SCHEME 10

 DEFINE WINDOW Wind_2A FROM 13,41 TO 23,79 DOUBLE ;
 TITLE 'Est. Diaria por Objeto del Gasto' COLOR SCHEME 10

 DEFINE WINDOW Wind_2B FROM 14,00 TO 23,40 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,64 TO 22,78 ;
 TITLE ' TOTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_4 FROM 20,63 TO 22,77 ;
 TITLE ' PARTIDA ' COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 07,08 TO 17,72 double;
 TITLE ' COMPROMISO PRESUPUESTAL    [®F5¯Agrega  ®F8¯Borra]'  COLOR SCHEME 10

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
 @  1, 2 SAY "       N£mero H/M :"
 @  1,40 SAY "        Fecha H/M :"
 @  2, 2 SAY "       Incidencia :"
 @  2,40 SAY "        Operaci¢n :"
 @  4, 2 SAY "       N£mero H/C :"
 @  4,26 say '.'
 @  4,40 SAY "        Fecha H/C :"
 @  5, 2 SAY "        Proveedor :"
 @  6, 2 SAY " F.Financiamiento :"
 @  6,40 SAY "            Nivel :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      Subprograma :"
 @  9, 2 SAY " Activid/Proyecto :"
 @ 10, 2 SAY " Partida Gen‚rica :"
* @  9,40 SAY " Comprobante Pago :"

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Hojmod
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO vista_det
 SCATTER MEMVAR
 @  0,48 say veresthm(m.estado)
 @  1,22 SAY m.NumMes
 @  1,24 SAY '.'
 @  1,25 SAY m.NumHm
 @  1,60 SAY m.FecHm
*@  2,22 SAY m.TipHm
 @  2,22 SAY val_para(m.TipHm,'HOJMOD','D',22,18)
 @  2,60 SAY val_para(m.Operac,'OPERAC','D',22,18)
 @  4,22 SAY m.NummesHc
 @  4,24 SAY '.'
 @  4,25 SAY m.NumHc
 @  4,60 SAY m.FecHc
 @  5,22 CLEAR TO 09,79
 @  5,22 SAY IIF(m.tipprv='O',m.Nombre,IIF(m.TipPrv='P',val_prv(m.Codprv),val_pro(m.Codemp)))

 @  6,22 SAY val_para(m.CodFte,'CODFTE','V',26,20)
 @  6,60 SAY val_para(m.TipFun,'TIPFUN','D',60,20,2)

 @  7,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',22,40)
 @  8,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @  9,02 SAY IIF(alltrim(m.Tipfun)='I',"         Proyecto :",IIF(!empty(substr(m.codcal,13,3)),"        Actividad :" ,"                   " ))
 @  9,22 SAY IIF(!empty(substr(m.codcal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,50),' ')
 IF alltrim(m.Tipfun)='I'
    @  9,25 SAY IIF(!EMPTY(substr(m.codcal,16,2)),'.',' ')
    @  9,26 SAY IIF(!EMPTY(substr(m.codcal,16,2)),VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5),' ')
    @ 10,02 SAY " Partida Gen‚rica :"
    @ 10,22 say m.CodPart
 else
    @ 10,02 SAY Spac(25)	
 endif	
*@  9,60 SAY iif(!empty(numcp),nummescp + '/'+numcp+' ','Sin Pago')
@ 11,00 SAY PADC(' ° ®F2¯  Justificaciones ° ',79,' ') COLOR W+/B

DO VISTA_HIJO

RETURN


PROCEDURE VISTA_HIJO
*-------------------
hide popup all
ACTIVATE WINDOW WIND_2
SELE ITEHC
GO TOP
SEEK m.NumMesHc + m.NumHc
IF FOUND()
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMesHc + m.NumHC TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   TIPOPE  : H='ä'  ,;
   tipfun  : H='T'  ,;
   CodFte  : H='Fte'  ,;
   CodPart : H='Part.' ,;
   CodAnal : H='Anlt.' ,;
   AA=VAL_PART(SUBSTR(itehC.CodAnal,4,2),LEFT(itehC.CodAnal,2),'D',22,50) :H='Descripci¢n' :30,;
   ItehC.Valpart     : H='  Parcial' :P='99,999,999.99' ,;
   XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM+'.'+NUMMESHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :11             
ELSE
   CLEAR
   @ 2,25 SAY 'No existe detalle, Revise..'
ENDIF
SELE Hojmod
RETURN

PROCEDURE VISTA_DET
*------------------
hide popup all
ACTIVATE WINDOW WIND_2
ON KEY LABEL F9
SELE ITEHC
GO TOP
SEEK m.NumMesHc + m.NumHc
IF FOUND()
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMesHc + m.NumHC ;
   WINDOW Wind_2 ;
   FIELDS;
   TIPOPE  : H='ä'  ,;
   tipfun  : H='T'  ,;
   CodFte  : H='Fte'  ,;
   CodPart : H='Part.' ,;
   CodAnal : H='Anlt.' ,;
   AA=VAL_PART(SUBSTR(itehC.CodAnal,4,2),LEFT(itehC.CodAnal,2),'D',22,50) :H='Descripci¢n' :37,;
   ItehC.Valpart     : H='  Parcial' :P='99,999,999.99'
ELSE
   CLEAR
   @ 2,25 SAY 'No existe detalle, Revise..'
ENDIF
SELE Hojmod
ON KEY LABEL F9 DO vista_det
RETURN


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,0 SAY m.ImpTot picture '99,999,999.99'
return

PROCEDURE PARTIDA
*----------------
IF ALLTRIM(m.TipFun)='I'
   show window WIND_4
   @ 0,4 SAY m.CodPart
else
   hide window WIND_4
endif
return

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
PRIVATE VTEMP
 SELECT HOJMOD
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO NUMMESHC + NUMHC INTO ITEHC
 SET SKIP TO ITEHC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)

 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 numMes :H='Mes' ,;
 numhm  :H='H/M ' ,;
 nummesHC :H='MHc' ,;
 numHC  :H='H/C' ,;
 codprv :H='Prv' ,;
 codcal :H='Calendario',;
 itehc.CodParT:H='Partida' ,;
 itehc.CodAnal :H='Analit.' ,;
 itehc.ValPart :H='Parcial' :P='9,999,999.99' ,;
 ImpTot :H='TOTAL' :P='9,999,999.99'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 SELE HOJMOD
 DO Vista

RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
PRIVATE VTEMP
SELECT HOJMOD
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_MES = '00'
 vNum_HC  = '0000'
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero H/M : '
 @ 1,23 GET vNum_Mes PICTURE '!!'
 @ 1,25 SAY '.'
 @ 1,26 GET vNum_Hc  PICTURE '!!!!'
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_HC) .or. LASTKEY()=27
    RETURN
 ELSE
   SEEK alltrim(vNum_Mes) + vNum_hc
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
SELE HOJMOD
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
SELE HOJMOD
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


PROCEDURE TERMI
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
  RELEASE WINDOW wind_2
  RELEASE WINDOW wind_2A
  RELEASE WINDOW wind_2B
  RELEASE WINDOW wind_3
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

FUNCTION valHC
*-------------
parameter vnumHC
private vfun
vfun = .t.
vnumHC =padl(alltrim(str(vnumHC,4)),4,'0')
return vNumhc

FUNCTION valRF
*-------------
m.Numref=padl(alltrim(m.Numref),4,'0')
return .t.

FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,02 TO 12,78 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Justificaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVA NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

function valprv
*--------------
private xx, vfun
vfun = .f.
malias = alias()
m.codprv= iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.,4,30)
if xx
   m.codemp='     '
   select (malias)
   return .t.
endif
select (malias)
return vfun

FUNCTION VAL_PRO
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
private medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (parameters()>=2)
mmsg   = (parameters()=4) .and.  _tipo

_campo = varread()

malias = alias()
select PERSONAL
_oldwnd = woutput()

If !medita
   SET ORDE TO 1
   seek xcod
   v_fun = iif(found(),Descri,"")
else
   if empty(xcod)
      set orde to 2
      on key label ENTER keyboard chr(23)
      define window _xx from 3,22 to 22,77
      browse window _xx title ' ®Enter¯  Selecciona   ' nolgrid noedit noappend nodelete nomenu fields;
         codigo   :h='C¢digo'     ,;
         descri   :h='Nombre'
       * dirpro   :h='Direccci¢n' :25
      on key label ENTER
      release window _xx
      set order to 2
      if !empty(_oldwnd)
         activate window &_oldwnd
      endif
      if lastkey()=27
         v_fun = .f.
      else
         xcod = codigo
         if mmsg
            @ _x,_y say descri
         endif
         select (malias)
         if !_tipo
            replace &_campo with  xcod
         endif
         v_fun = .t.
      endif
   else
      SET ORDE TO 1
      seek xcod
      if mmsg .and. found()
         @ _x,_y say descri
      endif
      v_fun = found()
   endif
endif
m.Codprv='    '
select (malias)
return v_fun

PROCEDURE VALE_MES
*-----------------
xx=padl(alltrim(str((val(m.NumMes)+1),2)),2,'0')
m.FechM=IIF(val(m.Nummes)<month(DATE()),ctod('01/'+xx+'/95') - 1,date())
return .t.

PROCEDURE VAL_MES
*----------------
SELECT PARMA
SEEK 'HOJCON'+ALLTRIM(M.NumMes)
if !found()
   do standby with 'El Correlativo del Mes no est  Inicializado'
   SELE hoja
   return .F.
 else
   IF CODIGOAUX='00'
      do standby with 'El Calendario del Mes '+alltrim(m.nummes)+' ya est  cerrado'
	  SELE hoja
   	  return .F.
   else
	  m.NumHc=valHC(Parma.NumEnt + 1)
	  xx=padl(alltrim(str((val(m.NumMes)+1),2)),2,'0')
	  m.Fechc=IIF(val(m.Nummes)<month(DATE()),ctod('01/'+xx+'/95') - 1,date())
	  sele hoja
      return .t.
   endif
endif


PROCEDURE ABRE
*-------------
USE hojmod   IN 1   order tag hojmod1   ALIAS Hojmod
USE HojCon   IN 3   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 4   order tag Itehc1    ALIAS Itehc
USE PARMAE   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Clase    IN 9   order tag Clase1    ALIAS Clase

USE Promae   IN 10  order tag Promae1   ALIAS Promae
USE Personal IN 11  order tag Personal2 ALIAS Personal

USE Cuentas  IN 12  order tag Cuentas6 ALIAS Cuenta
USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre
ON KEY LABEL F2 DO VISOBS
return

PROCEDURE Listar
*---------------
CLOSE DATA

USE hojmod   IN 1   order tag hojmod1   ALIAS Hojmod
USE HojCon   IN 3   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 4   order tag Itehc1    ALIAS Itehc
USE PARMAE   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Clase    IN 9   order tag Clase1    ALIAS Clase

USE Promae   IN 10  order tag Promae1   ALIAS Promae
USE Personal IN 11  order tag Personal2 ALIAS Personal

USE Cuentas  IN 12  order tag Cuentas6 ALIAS Cuenta
USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre

SELECT Hoja
SET RELATION TO NUMMESHC + NUMHC INTO ITEHC
SET SKIP TO ITEHC
SET MEMOWIDTH TO 34
vInd = SYS(3) + '.IDX'
INDEX ON NumMes+NumHc TO (vInd) ;
      FOR Periodo+NumMes+NumHc =vImpr
SET INDEX TO (VIND)
GO TOP
SCATTER MEMVAR
do reporte with 2,"LisHc1",' Hojas de Control '
close index
CLOSE DATA
*ERASE (VIND)
DO ABRE
RETURN

PROCEDURE VALER
*--------------
SELE HOJmod
m.NumHm = padL(alltrim(m.NumHm),4,'0')
SEEK ALLTRIM(m.NumMes)+m.NumHm

IF FOUND()
    DO STANDBY WITH 'La H/M ya ha sido generada'
    BROW
    RETURN .F.
ELSE
	RETURN .T.
ENDIF	
*--------------

PROCEDURE ASIact
*------------
REPLACE codact with vcodact
return .t.

PROCEDURE ASIpry
*------------
REPLACE codproy with vproyec
return .t.

FUNCTION valpyac  && VALIDA  PROYECTOS Y/O ACTIVIDADES
*----------------
PARAMETERS nvalor, nfiltro, nvariable, ncol, nlong ,nancho
PRIVATE nalias

DO CASE
	CASE PARAMETERS() = 2
		ncol = 0
		nvariable = ' '
		nlong = 40
		nancho = 6
	CASE PARAMETERS() = 3
		ncol = 0
		nlong = 40
		nancho = 6
	CASE PARAMETERS() = 4
		nlong = 40               && Longitud campo DESCRI
		nancho = 6
	CASE PARAMETERS() = 5		
		nancho = 6
ENDCASE
nalias  = ALIAS()


SELECT maepre						&& 2,3
SET ORDER TO IIF(ALLTRIM(m.tipfun)='I',6,7)

SEEK ALLTRIM(Itehc.TipFun)+nfiltro+nvalor

IF !FOUND()	OR !nvariable $'V'
	SET FILTER TO periodo+codprg+codsubpr = nfiltro
	GO TOP
	IF !EOF()
		IF !EMPTY(nvalor)
			SEEK alLTRIM(Itehc.TipFun)+nfiltro+nvalor
			IF !FOUND()
				DO yrolea
			ENDIF
		ELSE
			DO yrolea
		ENDIF
	ELSE
		DO standby WITH 'Error en Codificaci¢n program tica'
		SET FILTER TO
		IF !EMPTY( nalias )
			SELECT (nalias)
		ENDIF
		return .f.
	ENDIF	
ENDIF	
nvalor = IIF(ALLTRIM(itehc.tipfun)='I',maepre.codproy ,maepre.codact)
ndescr = SUBSTR( maepre.descri, 1, nlong )
*-Variables
SET FILTER TO
IF !EMPTY( nalias )
	SELECT (nalias)
ENDIF
DO CASE
	CASE nvariable=' '   && En edici¢n
		@ ROW(),ncol   SAY PADL(nvalor,nancho,' ')
		@ ROW(),ncol   SAY ndescr
		RETURN .T.
	CASE nvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),ncol SAY ndescr
		RETURN
	CASE nvariable='V'   && En vista
		@ ROW(),COL()  SAY PADR(nvalor,nancho,' ')
		RETURN ndescr
	CASE nvariable='D'   && En vista
		RETURN ndescr
	CASE nvariable='Z'   && En vista SIN PINTAR
		RETURN ndescr
	CASE nvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		&nvariable = nvalor
		RETURN .T.
ENDCASE

PROCEDURE Yrolea
*---------------
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06,10 TO 17,69 DOUBLE FLOAT SHADOW COLOR SCHEME 10
GO TOP
DO CASE
	CASE ALLTRIM(itehc.tipfun) = 'I' AND !EOF() AND !EMPTY(Codproy)
		BROWSE WINDOW _xx TITLE ' PROYECTOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codproy  :H='Pry' ,;
			descri   :H='Detalle'
	CASE ALLTRIM(itehc.tipfun) = 'F' AND !EOF() AND !EMPTY(Codact)
		BROWSE WINDOW _xx TITLE ' ACTIVIDAD :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codact   :H='Act',;
			descri   :H='Detalle'
	OTHER
		IF !EMPTY(nvalor)
			DO standby WITH 'No se tiene '+IIF(ALLTRIM(HOJA.tipfun)='F','Actividad','Proyecto')+' en referencia ---> '+M.TIPFUN
		ENDIF
eNDCASE
ON KEY LABEL ENTER
RELEASE WINDOW _xx
IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN


FUNCTION SIGUE
*-------------
SELE HOJMOD
IF EOF()
   RETURN .T.
ELSE
	SKIP
ENDIF
RETUR .T.
	   
	