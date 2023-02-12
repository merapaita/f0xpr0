*--------------------------------------------------------------------------
* HOJMOD1.Prg
* REGISTRA HOJAS DE MODIFICACION
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
USE hojmod   IN 1   order tag hojmod1   ALIAS Hojmod
USE HojCon   IN 3   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 4   order tag Itehc1    ALIAS Itehc
USE Parmae   IN 5   order tag Parmae1   ALIAS Parma
USE maepre   IN 6   order tag maepre3   ALIAS maepre
USE itepar   IN 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Auxil    IN 10  order tag Auxil1    ALIAS Auxi
USE Cuentas  IN 12  order tag Cuentas6  ALIAS Cuenta

PUBLIC VALCS, vCodPrg ,vCodSub , vProyec ,vCodact , vSubpry ,vTotahc, vResto, vcadena

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
 Vmens12 = '°   ®F2¯ Justificaci¢n   ° '

SELECT itehc
SET RELATION TO hoja.periodo+itehc.codcad+itehc.codcom+itehc.codmet INTO maepre

SELECT Hojmod
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
ON KEY LABEL F2 DO VISOBS
ON KEY LABEL F7 DO vis_ap
ON KEY LABEL F4 DO imp_hm
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente                             Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 14,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_1H FROM 00,00 TO 14,79  DOUBLE ;
 TITLE 'Registro Hoja Modificacion' COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 15,00 TO 23,79 DOUBLE ;
 TITLE 'Estad¡stica Diaria por Objeto del Gasto' COLOR SCHEME 10

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
 @  6, 2 SAY "Corr. cadena Fun. :"
 @  7, 2 SAY "          Funci¢n :"
 @  8, 2 SAY "         Programa :"
 @  9, 2 SAY "      SubPrograma :"
 @ 10, 2 SAY "  Activ./Proyecto :"
 @ 11, 2 SAY "         Fte.Fto. :"
 @ 12, 0 SAY PADC("²±  ®F2¯ Observaciones                  ®F7¯ Asiento   ®F9¯ Item  ±²",77,' ') color '7+/1'
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
 =val_cadHM(ALLT(m.codcad),m.periodo,'C')
 @  0,60 say veresthm(m.estado) COLOR 7+/1
 @  1,22 SAY m.NumMes
 @  1,24 SAY '.'
 @  1,25 SAY m.NumHm
 @  1,60 SAY m.FecHm
 @  2,22 SAY val_para(m.TipHm,'HOJMOD','D',22,18)
 @  2,60 SAY val_para(m.Operac,'OPERAC','D',22,18)
 @  4,22 SAY m.NummesHc
 @  4,24 SAY '.'
 @  4,25 SAY m.NumHc
 @  4,60 SAY m.FecHc
 @  5,22 CLEAR TO 09,79
*@  5,22 SAY IIF(m.tipprv='O',IIF(EMPTY(m.Codotr),m.nombre,Val_aux(m.Codotr,'09','D',24)),IIF(m.TipPrv='P',Val_aux(m.Codprv,'20','D',24),Val_aux(m.CodEmp,'30','D',24)))
 @  5,22 SAY IIF(m.tipprv='O',m.nombre,IIF(m.TipPrv='P',Val_aux(m.Codprv,'20','D',24),Val_aux(m.CodEmp,'30','D',24)))
 @  6,22 SAY m.CODCAD
 =VE_CAD()
 @ 11,22 SAY VAL_PARA(m.codfte,'CODFTE','V',43)
 DO VISTA_HIJO
RETURN


PROCEDURE VISTA_HIJO
*-------------------
hide popup all
ACTIVATE WINDOW WIND_2
SELE MAEPRE
SET ORDER TO TAG MAEPRE3
SELE ITEHC
SET RELATION TO Hoja.periodo+itehc.codcad+itehc.codcom+itehc.codmet INTO maepre
GO TOP
SEEK m.NumMesHc + m.NumHc
IF FOUND()
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMeshc + m.NumHc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
             TIPOPE   : H='ä'       :V=TIPOPE$'-+' :W=!ITEHC.TIPOPE $ 'è*',;
    		 CodFte   : H='Fte'     :P='!!' :W=!ITEHC.TIPOPE $ 'è*',;
    		 Codcad   : H='Cadena'  ,;
    		 maepre.codfun :H='Fn'  :W=.F. ,;
    		 maepre.codprg :H='Prg' :W=.F. ,;
    		 maepre.codspr :H='Spr' :W=.F. ,;
    		 maepre.actpry :H='ActPry' :W=.F. ,;
             CODCOM	  : H= 'Comp.'  :W=!ITEHC.TIPOPE $ 'è*' ,;
             CODMET   : H= 'Meta'   :W=!ITEHC.TIPOPE $ 'è*' ,;
             CodPart  : H= 'Partid' :W=!ITEHC.TIPOPE $ 'è*',;
             ValPart  : H='Total'   :P='99,999,999.99' :W=!ITEHC.TIPOPE $ 'è*',;
             XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :W=.F.       
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

SELE MAEPRE
SET ORDER TO TAG MAEPRE3
SELE ITEHC
SET RELATION TO Hoja.periodo+itehc.codcad+itehc.codcom+itehc.codmet INTO maepre

SELE ITEHC
GO TOP
SEEK m.NumMesHc + m.NumHc
IF FOUND()
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMesHC + m.NumHc ;
   WINDOW Wind_2 ;
   FIELDS;
             TIPOPE   : H='ä'          :V=TIPOPE$'-+' :W=!ITEHC.TIPOPE $ 'è*',;
    		 CodFte   : H='Fte'        :V=VAL_PARA(CodFte,'CODFTE','codfte'):F  :P='!!' :W=!ITEHC.TIPOPE $ 'è*',;
    		 Codcad   : H='Cadena'     :F,;
    		 maepre.codfun :H='Fn'     :W=.F. ,;
    		 maepre.codprg :H='Prg'    :W=.F. ,;
    		 maepre.codspr :H='SPrg'   :W=.F. ,;
    		 maepre.actpry :H='ActPry' :W=.F. ,;
             CODCOM	  : H= 'Comp.'     :F :W=!ITEHC.TIPOPE $ 'è*' ,;
             CODMET   : H= 'Meta'      :F :W=!ITEHC.TIPOPE $ 'è*' ,;
             CodPart  : H= 'Partid'    :W=!ITEHC.TIPOPE $ 'è*',;
             ValPart  : H='Total'      :P='99,999,999.99' :W=!ITEHC.TIPOPE $ 'è*',;
             XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :W=.F.       
ELSE
   CLEAR
   @ 2,25 SAY 'No existe detalle, Revise..'
ENDIF
SELE Hojmod
ON KEY LABEL F9 DO vista_det
DO VISTA
RETURN


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,0 SAY m.ImpTot picture '99,999,999.99'
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
 codcad :H='Cod. Cadena',;
 itehc.CodParT	:H='Partida' ,;
 itehc.ValPart 	:H='Parcial' :P='9,999,999.99' ,;
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



PROCEDURE ASIG_FEC
*-----------------
m.Periodo= ALLTRIM(STR(YEAR(m.fechM)-1900,4))
return .t.


PROCEDURE Trab_hijo            && Revisi¢n de BD en browse
*------------------
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO agreg_HM
ON KEY LABEL F8  DO elimi_HM
ON KEY LABEL F10 KEYBOARD CHR(23)

SELE MAEPRE
SET ORDER TO TAG MAEPRE3
SELE ITEHC
SET RELATION TO Hoja.periodo+itehc.codcad+itehc.codcom+itehc.codmet INTO maepre

SELECT itehc
seek alltrim(m.NummesHC)+m.NumhC

if !found()
  DO STANDBY WITH 'No tiene registros'
endif
  BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummeshc)+m.NumhC FIELDS ;
             TIPOPE   : H='ä'          :V=TIPOPE$'-+' :W=!ITEHC.TIPOPE $ 'è*',;
    		 CodFte   : H='Fte'        :V=VAL_PARA(CodFte,'CODFTE','codfte'):F  :P='!!' :W=!ITEHC.TIPOPE $ 'è*',;
    		 Codcad   : H='Cadena'     :V=VAL_cadhm(itehc.codcad,hoja.periodo,'R',22,40) AND SEL3() :F,;
    		 maepre.codfun :H='Fn'     :W=.F. ,;
    		 maepre.codprg :H='Prg'    :W=.F. ,;
    		 maepre.codspr :H='SPrg'   :W=.F. ,;
    		 maepre.actpry :H='ActPry' :W=.F. ,;
             CODCOM	  : H= 'Comp.'     :V=Val_comHM(m.periodo+'01001'+itehc.codcad,itehc.codcom,'codcom')  AND SEL3() :F :W=!ITEHC.TIPOPE $ 'è*' ,;
             CODMET   : H= 'Meta'      :V=Val_metHM(m.periodo+'01001'+itehc.codcad,itehc.codcom+itehc.codmet,'codmet')  AND SEL3() :F :W=!ITEHC.TIPOPE $ 'è*' ,;
             CodPart  : H= 'Partid'    :W=!ITEHC.TIPOPE $ 'è*',;
             ValPart  : H='Total'      :P='99,999,999.99' :W=!ITEHC.TIPOPE $ 'è*',;
             XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :W=.F.       
            
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SET FILTER TO
SELE HOJMOD
if lastkey()=27
   return .f.
endif
RETURN .t.



PROCEDURE Agreg_hm
*-----------------
sele iteHc
vp = codpart
IF F_appd()
      REPLACE NumHc  WITH m.NumHc ,;
              NumMes WITH m.NumMeshc ,;
      		  NumHm  WITH m.NumHm ,;
           NumMesHm  WITH m.Nummes,;
             codpart with vp ,;
              Tipdoc WITH 'H/M' ,;
              Tipope WITH '-',;              
              Estado WITH '92' ,;
              CodFte WITH m.Codfte ,;
              OPERAC WITH m.operac,;
              tipcom with m.tipHM,;
              codcad with maepre.codcad,;
              UNIGES WITH '01',;
              UNIEJE WITH '001'
ENDIF
UNLOCK
RETURN .T.


PROCEDURE Elimi_hm
*-----------------
if ITEHC.TIPOPE $ 'è*'
   RETURN .T.
ENDIF   
if rlock()
   delete next 1
endif
UNLOCK
return .T.


PROCEDURE V_hC
*--------------
PRIVATE VTEMP2
SELECT HOJA
IF EOF()
    DO standby WITH Vmens08
    RETURN
ENDIF
m.NumHC = padL(alltrim(m.NumHC),4,'0')
seek alltrim(m.nummeshc)+m.numhc
if !found()
	vtemp2 = RECNO()
	HIDE MENU mMenu
	ACTIVATE SCREEN
	vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO Logos WITH Rotulo1,vTempo
	ON KEY LABEL F10 KEYBOARD CHR(23)
	BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 numMes :H='Mes' ,;
	 numhC  :H='H/C ' ,;
	 TipDoc :H='Doc' ,;
	 numref :H='N§' ,;
	 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
	 ImpTot :H='TOTAL' :P='9,999,999.99'
*	 codprv :H='Prv' ,;
*	 codcal :H='Calendario',;
	 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 IF LASTKEY()=27
	   SELEC HOJA 
	   GOTO BOTT
	 ENDIF
	 SHOW MENU mMenu
	 ON KEY LABEL F10
endif
IF HOJA.ESTADO = '99'
   DO STANDBY WITH 'La Hoja ya esta anulada..'
   return .f.
endif   
m.FecHc   = hoja.fecHc
m.TipPrv  = hoja.TipPrv
m.Codprv  = hoja.Codprv
m.CodEmp  = hoja.CodEmp
*m.Codotr  = hoja.Codotr
m.Nombre  = hoja.Nombre

m.Codfte  = hoja.Codfte
*m.tipfun  = hoja.tipfun
m.Codpart = hoja.CodPart
m.NumHc   = hoja.Numhc
m.NumMeshc = hoja.Nummes
m.codcad   = hoja.codcad

*vCodPrg   = substr(hoja.codcal,08,2)
*vCodSub   = substr(hoja.codcal,10,3)
*vProyec   = substr(hoja.codcal,13,3)
*vCodact   = substr(hoja.codcal,13,2)
*vSubpry   = substr(hoja.codcal,16,4)

SELE HOJA
RETURN

PROCEDURE T_h      && Revisi¢n de BD en browse
*------------
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
seek alltrim(m.Nummes)+m.Numhc
SET FILT TO ESTADO#'92'
if !found()
   SET FILT TO
   RETUR .F.
endif
DO CASE
   CASE alltrim(m.tipfun)='I'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhc FIELDS ;
             TIPOPE   : H='ä' :V=TIPOPE$'-+':F :W=.F.,;
             CodPart  : H= 'Partida' :V=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'codpart') AND PONE():F :W=.F. ,;
             CodAnal  : H= 'Analitc' :V=VAL_PART(SUBSTR(CodAnal,4,2),LEFT(CodAnal,2),'codanal'):F :W=.F. ,;
             aa = IIF(!EMPTY(CODanal),VAL_PART(SUBSTR(Codanal,4,2),LEFT(Codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=.F. ,;
             ValPart :H='Total' :P='99,999,999.99' :V=CALE():F :W=.F.
   CASE alltrim(m.tipfun)='F'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhc FIELDS ;
             TIPOPE   : H='ä' :V=TIPOPE$'-+':F ,;
             CodAnal  : H= 'Analitc' :V=VAL_PART(SUBSTR(CodAnal,4,2),LEFT(CodAnal,2),'codanal'):F :W=.F. ,;
             aa = IIF(!EMPTY(CODanal),VAL_PART(SUBSTR(Codanal,4,2),LEFT(Codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=.F. ,;
             ValPart :H='Total' :P='99,999,999.99' :V=CALE():F :W=.F.
   OTHER
ENDCASE
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
ON KEY LABEL F10
SET FILTER TO
if lastkey()=27
   return .f.
endif
RETuRN .t.


PROCEDURE CALE
*------------
REPLA CODFTE WITH SUBSTR(M.CODCAL,5,3),CODPRG WITH  SUBSTR(M.CODCAL,8,2) ,CODSUBPR WITH SUBSTR(M.CODCAL,10,3)
RETURN .T.

PROCEDURE PONE
*-------------
IF ALLTRIM(m.tipfun)='I'
   m.codpart = itehc.codpart
endif
return .t.

PROCEDURE Borra_Oc            && Revisi¢n de BD en browse
*----------------
AX=ALIAS()
SELECT ITEHC
SEEK ALLTRIM(m.NumMes) + m.NumHc
if rlock()
   SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc
        delete next 1
   ENDSCAN
endif
sele (AX)
retur

PROCEDURE Anula
*--------------
 private vfun
 vfun = .F.
 OR =.F.
 SELECT hojMOD
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF LEFT(Estado,1) = '9'
   * ya pas¢
   DO STANDBY WITH 'La H/C ya esta anulada o con H/M'
   RETURN
 ENDIF
 IF YESNO('¨ Desea ANULAR esta Hoja de Modificaci¢n ?')
    ACTIVATE WINDOW STANDBY
    @ 1,14 SAY 'Espere un Momento ....' color W*
    
    SELE ITEHC
    SET FILT TO ESTADO='92'
    SEEK ALLTRIM(m.NumMesHc) + m.NumHc
    SCAN FOR NumMes = ALLTRIM(m.NumMeshC) and NumHc = m.NumHc AND NUMMESHM=M.NUMMES AND NUMHM=M.NUMHM &&and estado = '92'
         if rlock()
            REPLACE ESTADO WITH '99'
         endif
         unlock
    ENDSCAN
    SET FILT TO
    SELECT HOJMOD
    if rlock()
       REPLACE ESTADO WITH '99'
    endif
    unlock
    DEACTIVATE WINDOW STANDBY    
 ELSE
    do standby with 'Proceso cancelado'
 ENDIF
 selec hojMOD
 DO Vista
 UNLOCK ALL
 RETURN


PROCEDURE Lista
*--------------
SELECT Hojmod
vtemp =recno()
*CLOSE DATA

*USE hojmod   IN 1   order tag hojmod1   ALIAS Hojmod
*USE HojCon   IN 3   order tag HojCon1   ALIAS Hoja
*USE Itehc    IN 4   order tag Itehc1    ALIAS Itehc
*USE Parmae   IN 5   order tag Parmae1   ALIAS Parma

*USE maepre   IN 6   order tag maepre1   ALIAS maepre
*USE itepar   in 7   order tag itepar1   ALIAS Itepar
*USE Calen    IN 8   order tag calen1    ALIAS calen
*USE Clase    IN 9   order tag Clase1    ALIAS Clase
*USE Auxil    IN 10  order tag Auxil1    ALIAS Auxi

*USE Cuentas  IN 12  order tag Cuentas6 ALIAS Cuenta
*USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre

SELECT Hojmod
SET RELATION TO NUMMESHC + NUMHc INTO ITEHc
SET SKIP TO ITEHC
if eof()
   do standby with vmens08
   return
else
  do lishjm
endif
SELECT HOJmod
SET RELATION TO
*CLOSE DATA
*DO ABRE
SELECT HOJmod
GO VTEMP
DO VISTA
RETURN

PROCEDURE LisHjm
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 21,65 FLOAT DOUBLE TITLE 'Listado Hojas de Modificaci¢n' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToCli,vOrden,vTipPro,vLista
vCli = m.numhm
vMes = m.nummes
VpERI= sPACE(2)
@ 01,01 SAY "      Listado por : " GET vLista   FUNCTION '^ Documento;Resumen' WHEN vTocli=1
@ 04,01 SAY "              H/M : "
@ 04,22 GET vMes    PICTURE '!!'
@ 04,25 GET vCli    WHEN Vlista=1  PICTURE '!!!!' VALID Val_Hc()
@ 04,31 SAY "    Periodo: " get vperi when vlista=2
@ 06,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emisi¢n'  WHEN vLista=2
@ 10,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos' WHEN vLista=2

@ 14,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   SELE ITEHC
*  SET FILT TO IIF(TIPOPE$'+-',NUMMESHM=HOJMOD.NUMMES AND NUMHM=HOJMOD.NUMHM,.T.)
   SELE HOJMOD
   IF VLISTA=1
   	 INDEX ON IIF(vOrden=1,NumMes+NumHM,IIF(vOrden=2,CodPrv,DTOS(FECHC)))  TO (vInd) ;
        FOR (NumMes+NumHM=vMes+vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))     
   ELSE
   	 INDEX ON IIF(vOrden=1,NumMes+NumHM,IIF(vOrden=2,CodPrv,DTOS(FECHC))) TO (vInd) ;
   	   FOR (NumMes=vMes) AND (PERIODO=VPERI) AND IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))
   ENDIF
   SET INDEX TO (VIND)
   GO TOP
   SET RELATION TO NUMMESHC + NUMHc INTO ITEHc
   SET SKIP TO ITEHC
   DEACTIVATE WINDOW STANDBY
   SCATTER MEMVAR
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
     IF vLista = 1
       do  reporte with 2,"LisHm1",' Hojas de Modificaci¢n '
     ELSE
       * SELE HOJMOD
     	do reporte with 2,"LisHm2",' Hojas de Modificaci¢n '
     	
     ENDIF
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELE ITEHC
SET FILT TO 
SELE HOJMOD
RETUR

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

FUNCTION Observa
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 75
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,02 TO 12,78 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Justificaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVA WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF

RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF
SELECT (vAlias)
RETURN .T.


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

PROCEDURE COMPRE1
*----------------
private as,vtemp
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO agreg_ap
ON KEY LABEL F8  DO elimi_ap
ON KEY LABEL F10 KEYBOARD CHR(23)
USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre
SELECT ASTPRE
SET ORDE TO Astpre5
seek alltrim(m.Nummes)+m.Numhm+'H/M'
if !found()
  DO AGREG_AP
endif
SET ORDE TO ASTPRE5
BROWSE WINDOW Wind_5  NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhm+'H/M' FIELDS ;
       TIPO   : H='' :V=TIPO$'DH' and valtipo():F ,;
       CtaDeb : H='Cta.Debe' :V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,11)+' '+Descri",ctadeb,2):F :W=iif(tipo='D',.t.,.f.),;
       CtaHab : H='Cta.Haber':V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,11)+' '+Descri",ctahab,2):F :W=iif(tipo='H',.t.,.f.) ,;
       valdeb : H=' al Debe ':W=iif(tipo='D',.t.,.f.) ,;
       valHab : H=' al Haber':W=iif(tipo='H',.t.,.f.) 

seek alltrim(m.Nummes)+m.Numhm+'H/M'
SCAN WHILE NUMMES=alltrim(m.Nummes) AND NUMREF=m.Numhm AND TIPDOC='H/M'
     DO CASE
        CASE TIPO ='*' OR TIPO= ' '
             IF RLOCK()
                DELETE NEXT 1
             ENDIF
             UNLOCK
        OTHER
             IF RLOCK()
                REPLACE CUENTA WITH IIF(TIPO='D',CTADEB,CTAHAB)
			 ENDIF
             UNLOCK
     ENDCASE
ENDSCAN

vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
ON KEY LABEL F10
if lastkey()=27
   return .f.
endif
RETuRN .t.

PROCEDURE VIS_AP
*---------------
PRIVATE AD
AD =ALIAS()
USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre
SELECT ASTPRE
set orde to astpre5
seek alltrim(m.Nummes)+m.Numhm+'H/M'
ON KEY LABEL F7
BROWSE WINDOW Wind_5 NOEDIT NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhm+'H/M' FIELDS ;
       TIPO   : H='' :V=TIPO$'DH' and valtipo():F ,;
       CtaDeb : H='Cta.Debe' :V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctadeb,2):F :W=iif(tipo='D',.t.,.f.),;
       CtaHab : H='Cta.Haber':V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctahab,2):F :W=iif(tipo='H',.t.,.f.) ,;
       valdeb : H=' al Debe ':W=iif(tipo='D',.t.,.f.) ,;
       valHab : H=' al Haber':W=iif(tipo='H',.t.,.f.) 
USE IN 13
SELECT (AD)
ON KEY LABEL F7 do vis_ap       
RETURN


PROCEDURE AGREG_AP
*-----------------
if f_appd()
    replace Nummes with m.Nummes , TipDoc with 'H/M' , NumRef with m.NumHm , Fecref with m.fecref, CODPART WITH M.CODPART
    REPLACE PERIODO with m.PERIODO
else
    DO STANDBY WITH 'No se pudo Agregar'
endif
unlock
return 

PROCEDURE ELIMI_AP
*----------------
SELECT ASTPRE
VFUN = .T.   
IF RLOCK()
   DELE NEXT 1
ELSE
   VFUN = .F.   
ENDIF   
RETURN .T.

procedure valtipo
*----------------
do case
   CASE tipo = 'D'
        replace CtaHab with spac(10),valhab with 0
   CASE tipo = 'H'
        replace CtaDeb with spac(10),valdeb with 0
endcase  
RETURN .T.

PROCEDURE XCOMPRE1
*---------------
PARAMETER SIG
AX=ALIAS()
m.Valdeb = 0 && ABS(vtotsig) &&vtotaLH
m.Valhab = 0 && ABS(vtotsig) &&vtotaLH
IF SIG='-'
  * m.CtaDeb = iif(alltrim(m.CODFTE)='TRN','9240100000','9220100000')
  * m.CtaHab = iif(alltrim(m.CODFTE)='TRN','9040100000','9020100000')
ELSE
  * m.CtaDeb = iif(alltrim(m.CODFTE)='TRN','9040100000','9020100000')
  * m.CtaHab = iif(alltrim(m.CODFTE)='TRN','9240100000','9220100000')
ENDIF   
SELECT CUENTA
ACTIVATE WINDOW WIND_5
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  GET m.CtaDeb PICTURE '!!!!!!!!!!' Valid val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",m.CtaDeb,1)
@ 01,18  GET m.ValDeb PICTURE '99,999,999.99' valid valdb1()
READ
@ 02,12  GET m.CtaHab PICTURE '!!!!!!!!!!' Valid val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",m.CtaHab,1)
@ 02,34  GET m.ValHab PICTURE '99,999,999.99' valid valHb1()
READ
DEACTIVATE WINDOW WIND_5
RETURN


procedure valdb1
*-----------------
AS=ALIAS()
SELECT ASTPRE
SET ORDE TO 3
SEEK 'D'+alltrim(m.NumMes)+alltrim(m.NumHm) &&+alltrim(m.CtaDeb)
if !found()
   if f_appd()
      replace Nummes with m.Nummes,TipDoc with 'H/M',NumRef with m.NumHm , cuenta with m.CtaDeb, tipo with 'D',Fecref with m.fecref,CODPART WITH M.CODPART,CODCAL WITH m.periodo+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
   endif
   unlock
ENDIF
   if rlock()
      replace Ctadeb with m.CtaDeb ,Ctahab with space(10), Valdeb with m.ValDeb ,Valhab with 0 ,Codcal with m.Codcal,CODCAL WITH m.periodo+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
   endif
sele (as)
return

procedure valhb1
*-----------------
AS=ALIAS()
SELECT ASTPRE
SET ORDE TO 3
SEEK 'H'+alltrim(m.NumMes)+alltrim(m.NumHm) &&+alltrim(m.CtaHab)
if !found()
   if f_appd()
      replace Nummes with m.Nummes,TipDoc with 'H/M',NumRef with m.NumHm , cuenta with m.Ctahab, tipo with 'H',Fecref with m.fecref,CODPART WITH M.CODPART,              CODCAL WITH m.periodo+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
   endif
   unlock
endif
   if rlock()
      replace Ctadeb with space(10),Ctahab with m.CtaHab , Valdeb with 0 ,Valhab with m.ValHab ,Codcal with m.Codcal,              CODCAL WITH m.periodo+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
   endif
sele (as)
return


PROCEDURE VAL_HC             && Revisi¢n de BD en browse
*---------------
 SELECT HOJMOD
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK vMes+vCli
 IF !FOUND()
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
    numhC  :H='H/C ' ,;
    TipDoc :H='Doc' ,;
    numref :H='N§' ,;
    ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
    codprv :H='Prv' ,;
    codcal :H='Calendario',;
    itehc.CodParT:H='Partida' ,;
    itehc.ValPart :H='Parcial' ,;
    ImpTot :H='TOTAL' :P='99,999.99'
    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    IF LASTKEY()=27
       GO BOTT
    ENDIF
 ENDIF
 vMes = NumMes
 vcli = NumhM
 vResto=0
 SELECT ITEHC
 SEEK HOJMOD.NUMMESHC+HOJMOD.NUMHC
 IF FOUND()
 	DO WHILE NUMMES=HOJMOD.NUMMESHC AND NUMHC=HOJMOD.NUMHC
 		IF TIPOPE='-' AND HOJMOD.OPERAC='R'
 			VRESTO=VRESTO+itehc.VALPART
 		ENDIF
 		SKIP
 		IF EOF()
 			EXIT
 		ENDIF	
 	ENDDO	
 ENDIF
 SELECT HOJA
 SEEK HOJMOD.NumMeshc+HOJMOD.Numhc
 vtotahc=Imptot
 ON KEY LABEL F10
 SELECT HOJMOD
 SET RELATION TO
RETURN

FUNCTION vtotales
*----------------
 vresto=0
 vtotahc=0
 SELECT ITEHC
 SEEK HOJMOD.NUMMESHC+HOJMOD.NUMHC 
 IF FOUND()
    SCAN FOR ITEHC.NUMMES=HOJMOD.NUMMESHC AND ITEHC.NUMHC=HOJMOD.NUMHC  
 		IF TIPOPE='-' AND HOJMOD.OPERAC='R'
 			VRESTO=VRESTO+itehc.VALPART
 		ENDIF
 		SELE ITEHC
 	ENDSCAN
 ENDIF
 SELECT HOJA
 SEEK HOJMOD.NumMeshc+HOJMOD.Numhc
 vtotahc=Imptot-vresto
 SELECT HOJMOD
RETUR vtotahc



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
m.FechM=IIF(val(m.Nummes)<month(DATE()),ctod('01/'+xx+'/'+SUBS(DTOC(DATE()),7,2)) - 1,date())
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
USE Parmae   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre3   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Clase    IN 9   order tag Clase1    ALIAS Clase
USE Auxil    IN 10  order tag Auxil1    ALIAS Auxi

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
USE Parmae   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Clase    IN 9   order tag Clase1    ALIAS Clase
USE Auxil    IN 10  order tag Auxil1    ALIAS Auxi

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



	   
FUNCTION ve_cad
*--------------
 IF LASTKEY()#27
 @  7,22 SAY VAL_PARA(maepre.codfun,'CODFUN','V',22,40)
 @  8,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  9,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @ 10,22 SAY maepre.actpry
 @ 10,29 SAY VAL_PARA(maepre.actpry,'ACTPRY','D',23,40)
 ENDIF    
RETURN .T.


FUNCTION Val_cadHM
*-----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT maepre
  SET ORDER TO TAG maepre1
* IF sistema = '1'
  		SET FILTER TO hoja.periodo+'01001' = Filtro+"01001"
  		SEEK Filtro + "01001" + mValor
 *ELSE
 *		SET FILTER TO LEFT(CODCAD,1)='C'
 *		SEEK filtro + mvalor
 *ENDIF		
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd1 = WOUTPUT()
    ACTIVATE SCREEN
    *SET FILTER TO PERIODO = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
       SET ORDER TO TAG maepre3
	   return	
    ENDIF
    ON KEY LABEL f2  DO busCodCad
    ON KEY LABEL f3  DO busDesCad
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 02,01 TO 23,78  DOUBLE ;
		TITLE '[F2]Cadena   [F3]Componente   [F10] seleccionar' COLOR SCHEME 15
		ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCAD	: H='CodCad',;
    	UNIGES	: H='GEST.',;
    	UNIEJE	: H='EJEC.',;
    	CODFUN	: H='Fn',;
    	CODPRG	: H='Prg',;
    	CODSPR	: H='SPrg',;
    	ACTPRY	: H='Act/Pry',;
    	CODCOM	: H='CodComp',;
    	XX = VAL_PARA(CODCOM,'CODCOM','D'):H='Descripci¢n':25,;
    	CODMET	: H='Meta',;
    	Descri	: H='Descripci¢n':20
    RELEASE WINDOWS wind_cad
    ON KEY LABEL f2  
    ON KEY LABEL f3  
    IF !EMPTY(_OldWnd1)
       ACTIVATE WINDOW &_OldWnd1
    ENDIF
    set order to tag maepre3
  ENDIF
  SET FILTER TO
  SELECT maepre
  SET ORDER TO TAG maepre3

  mValor = maepre.codcad
  mDescr = SUBSTR( maepre.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    CASE mvariable='R'   && Para reemplazar
      REPLACE itehc.codcad with maepre.codcad
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
RETURN


FUNCTION Val_comHM
*-----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO TAG MAEPRE1
vrecno = Recno()
vorder = order()
IF SISTEMA ='2'
	SET ORDER TO MAEPRE1
ENDIF	
SEEK vfiltro
vBusca1 = PERIODO+UNIGES+UNIEJE+CODFUN+CODPRG+CODSPR+ACTPRY
SET ORDE TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	SEEK vBusca1
	vkey=periodo+uniges+unieje+codfun+codprg+codspr+actpry
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry=vkey
	go top
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Componentes ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCOM	: H='CodComp',;
		xx=val_para(codcom,'CODCOM','D'):h='Descripci¢n':25
    RELEASE WINDOWS wind_cad
    SET FILTER TO
endif
mValor = maepre.codcom
mDescr = val_para(maepre.codcom,'CODCOM','D')

select maepre
IF SISTEMA='2'
	set orde to maepre3
ELSE
	set orde to maepre1
ENDIF
SELECT MAEPRE
SET ORDER TO TAG MAEPRE3
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
     CASE mVariable='C'   && Solo codigo
       RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
RETURN


FUNCTION Val_METHM
*-----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO TAG MAEPRE1
vrecno = Recno()
SEEK vfiltro
vBusca1 = PERIODO+UNIGES+UNIEJE+CODFUN+CODPRG+CODSPR+ACTPRY
SET ORDE TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	SEEK vBUSCA1
	vORD1=periodo+uniges+unieje+codfun+codprg+codspr+actpry+CODCOM
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry+CODCOM=vORD1
	go top
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD1 FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Metas ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad1
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad1  FIELD ;
		CODMET	: H='META',;
		DESCRI  : h='Descripci¢n':30
    RELEASE WINDOWS wind_cad1
    SET FILTER TO
endif
mValor = maepre.codMET
mDescr = maepre.DESCRI
SELECT maepre
SET ORDER TO TAG MAEPRE3
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
RETURN

FUNCTION Sel3
*-----------
vX=SELECT()
SELE MAEPRE
SET ORDER TO TAG MAEPRE3
SELECT(vX)
RETURN .T.


PROCEDURE imp_hm
*---------------
SELECT Hojmod
vtemp =recno()
SELECT Hojmod
SET RELATION TO NUMMESHC + NUMHC INTO ITEHC
SET SKIP TO ITEHC
if eof()
   do standby with vmens08
   return
else
  do lishjm1
endif
SELECT HOJmod
SET RELATION TO
SELECT HOJmod
GO VTEMP
DO VISTA
RETURN

PROCEDURE LisHjm1
*----------------
STORE 1  TO vToCli,vOrden,vTipPro,vLista

vCli    = m.numhm
vMes    = m.nummes
VpERI   = sPACE(2)
vLista  = 1
vOrden  = 1
vTipPro = 1

RELEASE WINDOW LIS
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   SELE ITEHC
   SELE HOJMOD
   IF VLISTA=1
   	 INDEX ON IIF(vOrden=1,NumMes+NumHM,IIF(vOrden=2,CodPrv,DTOS(FECHC)))  TO (vInd) ;
        FOR (NumMes+NumHM=vMes+vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))     
   ELSE
   	 INDEX ON IIF(vOrden=1,NumMes+NumHM,IIF(vOrden=2,CodPrv,DTOS(FECHC))) TO (vInd) ;
   	   FOR (NumMes=vMes) AND (PERIODO=VPERI) AND IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))
   ENDIF
   SET INDEX TO (VIND)
   GO TOP
   SET RELATION TO NUMMESHC + NUMHc INTO ITEHc
   SET SKIP TO ITEHC
   DEACTIVATE WINDOW STANDBY
   SCATTER MEMVAR
   do  reporte with 2,"LisHm1",' Hojas de Modificaci¢n '
   CLOSE INDEX
   ERASE (VIND)

SELE ITEHC
SET FILT TO 
SELE HOJMOD
RETUR
