*--------------------------------------------------------------------------
* HOJMOD.Prg
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

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_1H FROM 00,00 TO 13,79  DOUBLE ;
 TITLE 'Registro Hoja Modificacion' COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE '>Estad¡stica Diaria por Objeto del Gasto      ®F9¯ Item   ®F7¯ Asiento ' COLOR SCHEME 10

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
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula   '    AT 24,54
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
 ON KEY LABEL F7 do vis_ap       
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


PROCEDURE Corri              && Crea nuevo registro en BD
*--------------
PRIVATE VTEMP1
vTemp1 = recno()
Public vCodPrg,vCodSub,vProyec,vCodact,vSubpry,vCodPrg,vCodSub,vProyec,vCodact,vSubpry,vTotalh,vTotSig
DO PANTALLA
SELECT Hojmod
SCATTER MEMVAR BLANK
m.FecHm = DATE()
@ 2,22 GET m.TipHm    PICTURE '!!!'  VALID val_para(m.TipHM,'HOJMOD',' ',22,10,4) AND ASIG_FEC()
@ 2,60 GET m.Operac   PICTURE '!'    VALID val_para(m.Operac,'OPERAC',' ',60,10,3)
READ VALID VAL_READ()

IF LASTKEY() # 27
    oK = Trab_hijo()
    IF OK .AND. LASTKEY()#27
       *- calcula total de parciales
       SELECT ITEHC
       SET FILT TO ESTADO = '92'
       GO TOP
       SEEK ALLTRIM(m.NumMesHc) + m.NumHc
       vTotal = 0

       SCAN WHILE NumMes = ALLTRIM(m.NumMeshC) and NumHc = m.NumHc
          vtotal = vtotal + valpart
          IF valpart=0
             if rlock()
                delete next 1
             endif
             unlock
           else
             if rlock()
                if nummeshm=alltrim(m.nummes) and numhm=alltrim(m.numhm)
                   replace OPERAC WITH m.operac,tipcom with m.tiphm
                endif
             endif
             Unlock   
          endif
       ENDSCAN
       SET FILTER TO 
       GO TOP
       SELECT HOJMOD
       m.Imptot  = vtotal
       m.codpart = vantpart
       vImpr = m.Periodo+ALLTRIM(m.Nummes)+m.Numhm
       vHm = alltrim(m.Nummeshc)+m.numhc
       USE IN 8
       USE IN 9
	   USE IN 10    	
   	   USE IN 11
       DO COMPRE1
  	   SELE HOJmod
       GATHER MEMVAR
       =OBSERVA()
       SELE HOJA
       SEEK vhm
       if found()
          if rlock()
             replace numhm with hojmod.numhm,fechm with hojmod.fechm &&,nummeshm with hojmod.nummes
          endi
          unlock
       else
          do standby with 'error!'   
       endif      
        
    ENDIF
    SELECT HOJmod
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    SELECT HOJmod
 ENDIF
 CLOSE DATA
 DO ABRE
 SELE HOJmod
 go vtem
 DO Vista
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
PRIVATE VTEMP1
vTemp1 = recno()
Public vCodPrg,vCodSub,vProyec,vCodact,vSubpry,vCodPrg,vCodSub,vProyec,vCodact,vSubpry,vTotalh,vTotSig
DO PANTALLA
SELECT Hojmod
SCATTER MEMVAR BLANK
m.FecHm = DATE()
@ 4,22 GET m.NummesHc 
@ 4,24 SAY '.'
@ 4,25 GET m.NumHc    VALID V_HC()

@ 4,22 GET m.NummesHc
@ 4,24 SAY '.'
@ 4,25 GET m.NumHc  

@ 1,22 GET m.NumMes   PICTURE '!!' VALID VAL_PARA(m.NumMes,'FECMES','C',22,30) AND VALE_MES()
@ 1,24 SAY '.'
@ 1,25 GET m.NumHm    PICTURE '!!!!' VALID VALER() AND !EMPTY(m.NumHm)
@ 1,60 GET m.FecHm
@ 2,22 GET m.TipHm    PICTURE '!!!'  VALID val_para(m.TipHM,'HOJMOD',' ',22,10,4) AND ASIG_FEC()
@ 2,60 GET m.Operac   PICTURE '!'    VALID val_para(m.Operac,'OPERAC',' ',60,10,3)

@ 4,60 GET m.FecHc

@ 5,22 GET m.TipPrv    PICTURE '@M P,E,O'   
 
@ 5,24 GET m.Codprv    PICTURE '!!!!'   VALID valprv() WHEN m.tipprv='P'
@ 5,24 GET m.CodEmp    PICTURE '!!!!!'  VALID val_pro(m.Codemp,.T.,4,30) WHEN m.TipPrv='E'
@ 5,24 GET m.Nombre    PICTURE '@S40'   when m.TipPrv='O'

@ 6,22 GET m.CodCad	PICTURE '!!!!'	VALID VAL_codcad(m.codcad,m.periodo,' ',22,40)
 READ VALID Val_Read()
 IF LASTKEY()=27
 	DO STANDBY WITH 'Proceso cancelado'
 	UNLOCK ALL
 	CLOSE DATA
 	DO ABRE
 	SELECT HOJMOD
 	GO TOP
 	GO VTEP
	DO Vista
	ON KEY LABEL F2 DO VISOBS
	RETURN
 ENDIF

READ VALID Val_Read()

IF LASTKEY() # 27
    vAntpart = iif(ALLTRIM(m.tipfun)='I',m.codpart,'     ')
    oK = Trab_hijo()
    IF OK .AND. LASTKEY()#27
       *- calcula total de parciales
       USE COMPAG IN 0 ORDER TAG COMPAG4 ALIAS COMPAG
       SELECT Compag
       SEEK ALLTRIM(m.NumMesHc) + m.numhc
       IF FOUND()
       	m.numcp    = Compag.Numcp
       	m.Nummescp = Compag.Nummes
       ENDIF 
       USE 
       SELECT ITEHC
       SET FILT TO ESTADO='92'
       SEEK ALLTRIM(m.NumMesHc) + m.NumHc
       vTotal  = 0
       vTotalh = 0       
       vTotSig = 0      
       SCAN FOR NumMes = ALLTRIM(m.NumMeshC) and NumHc = m.NumHc &&and estado = '92'
               vTotal  = vTotal + valpart
			   vtotalh = vtotalh + valpart
               vtotsig = vtotsig + iif(tipope='-',-1*valpart,valpart)
               IF valpart = 0
                  if rlock()
                     delete next 1
                  endif
                  unlock
               else
                  if rlock()
                  	*REPLACE CodFte  WITH m.Codfte,CodPrg  WITH vCodprg,CodSubPR  WITH vcodsub
                  	 if alltrim(m.tipfun)='F'
                        REPLACE CodProy WITH '   ',CodSuPry WITH '  ',CodObra WITH '   '
                     else
                        REPLACE CodAct WITH '   '
                     endif
                   endif
                   unlock   
               endif
       ENDSCAN
       SET FILT TO
       GO TOP
       SELECT HOJMOD
       m.Imptot  = vtotal
       m.Codcal  = ALLTRIM(str(year(m.fechc)-1900))+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
       m.codpart = vantpart
       vImpr = m.Periodo+ALLTRIM(m.Nummes)+m.Numhm
       vHm = alltrim(m.Nummeshc)+m.numhc
       USE IN 8
       USE IN 9
	   USE IN 10    	
   	   USE IN 11
       DO COMPRE1
  	   SELE HOJmod
  	   m.estado = '00'
       IF F_Appd()
          GATHER MEMVAR
       ENDIF
       =OBSERVA()
       SELE HOJA
       SEEK vhm
       if found()
          if rlock()
             replace numhm with hojmod.numhm,fechm with hojmod.fechm &&,nummeshm with hojmod.nummes
          endi
          unlock
       else
          do standby with 'error!'   
       endif      
    ENDIF
    SELECT HOJmod
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    SELECT HOJmod
 ENDIF
 CLOSE DATA
 DO ABRE
 SELE HOJmod
 GO BOTT
 DO Vista
 ON KEY LABEL F2 DO VISOBS
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
SELECT itehC
seek alltrim(m.NummesHC)+m.NumhC
if !found()
  DO STANDBY WITH 'No tiene registros'
endif
  BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummeshc)+m.NumhC FIELDS ;
             TIPOPE   : H='ä'       :V=TIPOPE$'-+' :W=!ITEHC.TIPOPE $ 'è*',;
			 CodCad   : H='Cod.Cad' :V=val_codcad(codcad,M.periodo,'CodCad'):F :W=!EMPTY(CODFTE) AND !ITEHC.TIPOPE $ 'è*',;
			 CodFte   : H='Fte'     :V=VAL_PARA(CodFte,'CODFTE','codfte'):F  :P='!!!' :W=!ITEHC.TIPOPE $ 'è*',;
             CodPart  : H='Part.'   :V=VAL_cale(codpart,m.periodo+allt(codcad)+allt(codfte)+allt(m.nummes),'codpart'):F:W=!ITEHC.TIPOPE $ 'è*',; 
             ValPart  : H='Total'   :P='99,999,999.99' :W=!ITEHC.TIPOPE $ 'è*',;
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
              CodCad WITH m.CodCad,;
              Tipdoc WITH 'H/M' ,;
              Tipope WITH '-',;              
              Estado WITH '92' ,;
              CodFte WITH m.Codfte ,;
            OPERAC WITH m.operac,;
            tipcom with m.tipHM,;
            uniges with '01',;
            unieje with '001'
QQ
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
	 codprv :H='Prv' ,;
	 codcal :H='Calendario',;
	 ImpTot :H='TOTAL' :P='9,999,999.99'
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
m.Nombre  = hoja.Nombre
m.Codfte  = hoja.Codfte
m.tipfun  = hoja.tipfun
m.Codpart = hoja.CodPart
m.NumHc   = hoja.Numhc
m.NumMeshc = hoja.Nummes
vCodPrg   = substr(hoja.codcal,08,2)
vCodSub   = substr(hoja.codcal,10,3)
vProyec   = substr(hoja.codcal,13,3)
vCodact   = substr(hoja.codcal,13,2)
vSubpry   = substr(hoja.codcal,16,4)
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
CLOSE DATA

USE hojmod   IN 1   order tag hojmod1   ALIAS Hojmod
USE HojCon   IN 3   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 4   order tag Itehc1    ALIAS Itehc
USE Parmae   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen
USE Clase    IN 9   order tag Clase1    ALIAS Clase

USE Promae   IN 10  order tag Promae1   ALIAS Promae
USE Personal IN 11  order tag Personal2 ALIAS Personal

USE Cuentas  IN 12  order tag Cuentas6 ALIAS Cuenta
USE AstPre   IN 13  order tag Astpre5  ALIAS AstPre
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
CLOSE DATA
DO ABRE
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
   	 INDEX ON IIF(vOrden=1,NumMes+NumHM,IIF(vOrden=2,CodPrv,DTOS(FECHC)))+ITEHC.CODANAL  TO (vInd) ;
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
SELECT ASTPRE
SET ORDE TO Astpre5
seek alltrim(m.Nummes)+m.Numhm+'H/M'
if !found()
  DO AGREG_AP
endif
SET ORDE TO ASTPRE5
BROWSE WINDOW Wind_5  NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhm+'H/M' FIELDS ;
       TIPO   : H='' :V=TIPO$'DH' and valtipo():F ,;
       CtaDeb : H='Cta.Debe' :V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctadeb,2):F :W=iif(tipo='D',.t.,.f.),;
       CtaHab : H='Cta.Haber':V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctahab,2):F :W=iif(tipo='H',.t.,.f.) ,;
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
SELECT (AD)
ON KEY LABEL F7 do vis_ap       
       
RETURN


PROCEDURE AGREG_AP
*-----------------
if f_appd()
    replace Nummes with m.Nummes , TipDoc with 'H/M' , NumRef with m.NumHm , Fecref with m.fecref, CODPART WITH M.CODPART,CODCAD WITH M.CODCAD
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
    codcaD :H='Cod. Cad.',;
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
USE Parmae   IN 5   order tag Parmae1   ALIAS Parma

USE maepre   IN 6   order tag maepre1   ALIAS maepre
USE itepar   in 7   order tag itepar1   ALIAS Itepar
USE Calen    IN 8   order tag calen1    ALIAS calen

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
USE Parmae   IN 5   order tag Parmae1   ALIAS Parma

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

*DO CASE
 *  CASE alltrim(m.tipfun)='F'
 *       BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummeshc)+m.NumhC FIELDS ;
 *            TIPOPE   : H='ä' :V=TIPOPE$'-+' :W=!ITEHC.TIPOPE $ 'è*',;
 *            tipfun   : H='T' :V=val_para(tipfun,'TIPFUN','tipfun') :W=!ITEHC.TIPOPE $ 'è*',;
 *            CodFte   : H='Fte'     :V=VAL_PARA(CodFte,'CODFTE','codfte'):F:P='!!!':W=!ITEHC.TIPOPE $ 'è*',;
 *            CodPrg   : H='Prog'    :P='!!'   :V=VAL_PARA(CodPrg  ,'CODPRG','codprg'):F :W=!EMPTY(CODFTE) AND !ITEHC.TIPOPE $ 'è*',;
 *            CodSubPr : H='Sprg'    :P='!!!'  :V=VAL_SUBP(CodSubPr,'CODSUB'+CodPrg+'    ','codsubpr') :F :W=!EMPTY(CodPrg) AND !ITEHC.TIPOPE $ 'è*' ,;    
 *            CodAct   : H='Act'     :V=VAL_PYAC(codact  ,m.Periodo+codprg+Codsubpr,'vcodact') AND ASIact():F :W=!EMPTY(CodSubPr) AND !ITEHC.TIPOPE $ 'è*' ,;      
 *            CodAnal  : H='Anlt.'   :V=VAL_PART(SUBSTR(CodAnal,4,2),LEFT(CodAnal,2),'codanal') :W=!ITEHC.TIPOPE $ 'è*' ,;
 *            aa = IIF(!EMPTY(CODanal),VAL_PART(SUBSTR(Codanal,4,2),LEFT(Codanal,2),'D'),' ') :H='Descripci¢n' :30 :W=!EMPTY(CodAnal) and !ITEHC.TIPOPE $ 'è*' ,;
 *            ValPart :H='Total' :P='99,999,999.99':W=!ITEHC.TIPOPE $ 'è*'  
 *  CASE alltrim(m.tipfun)='I'

procedure frias
*--------------
DEFINE WINDOW MIWIN FROM 1,1 TO 09,35 DOUBLE FLOAT SHADOW COLOR SCHEME 15
ACTIVATE WINDOW MIWIN
@ 00,01 SAY PADC('Bienvenidos',30,' ')
@ 01,01 SAY PADC('al',30,' ')
@ 02,01 SAY PADC('Centro de C¢mputo'    ,30,' ')
@ 03,01 SAY PADC('Regi¢n Grau',30,' ')
@ 04,01 SAY PADC('-------',30,' ')
*@ 05,01 SAY PADC(HOY(),30,' ')
*@ 06,01 SAY PADC(ANIO(),30,' ')

ON KEY LABEL F3 DO TERMINA


   FOR J = 1 TO 45
      MOVE WINDOW MIWIN TO 1,J
      =INKEY(0.01)
   ENDFOR
   FOR J = 1 TO 15
      MOVE WINDOW MIWIN TO J,45
      =INKEY(0.01)
   ENDFOR
   FOR J = 45 TO 1 STEP -1
      MOVE WINDOW MIWIN TO 15,J
      =INKEY(0.01)
   ENDFOR
   FOR J = 15 TO 1 STEP -1
      MOVE WINDOW MIWIN TO J,1
      =INKEY(0.01)
   ENDFOR

*vNumMes    = '  '
*vNumHm     = '    '
*vFecHm     = date()
*vTipHm     = '   '
*vNumMesHc  = '    '
*vNumhc     = '    '
*vFecHc     = date()
* SUM itehc.valpart for (hojmod.Operac='R' and itehc.tipope='-') to vResto

FUNCTION SIGUE
*-------------
SELE HOJMOD
IF EOF()
   RETURN .T.
ELSE
	SKIP
ENDIF
RETUR .T.
	   
	