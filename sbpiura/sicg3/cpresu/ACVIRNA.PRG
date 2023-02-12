*--------------------------------------------------------------------------
* REGHC.Prg
* REGISTRA HOJAS DE AFECTACION
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
* FECHA: 12/10/94
*--------------------------------------------------------------------------
USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja   &&SHARE
USE Itehc    IN 2   order tag Itehc1    ALIAS Itehc  &&SHARE
USE Parmae   IN 3   order tag Parmae1   ALIAS Parma  &&SHARE

USE maepre   IN 4   order tag maepre1   ALIAS maepre &&SHARE
USE itepar   in 5   order tag itepar1   ALIAS Itepar &&SHARE
USE Calen    IN 6   order tag calen1    ALIAS calen  &&SHARE
USE Clase    IN 7   order tag Clase1    ALIAS Clase  &&SHARE

USE OrdCom   IN 8   order tag OrdCom1   ALIAS Orden
USE IteOc    IN 9   order tag IteOc1    ALIAS IteOc
USE Iteart   IN 10  order tag Iteart3   ALIAS iteart
USE Ordser   IN 11  order tag OrdSer1   ALIAS OrdSer
USE Promae   IN 12  order tag Promae1   ALIAS Promae
USE Personal IN 13  order tag Personal2 ALIAS Personal


ON KEY LABEL F4 DO IMPRIMIR

PUBLIC  VALCS, vCodPrg ,vCodSub , vProyec ,vCodact , vSubpry , vgun,VPART

VALCS = .T.
*- Mensajes de aviso al usuario

 Vmens01 = ' Hoja de Afectaci¢n : REVISION '
 Vmens02 = ' Registro de Hoja de Afectaci¢n '
 Vmens04 = 'Dicho Hoja de Afectaci¢n no fue encontrado'
 Vmens05 = 'No existe Hoja de afectaci¢n anterior'
 Vmens06 = 'No existe Hoja de afectaci¢n siguiente'
 Vmens07 = '¨ Desea ANULAR ‚ste Hoja de afectaci¢n ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Esta Hoja de Afectaci¢n ha sido anulada'
 Vmens10 = 'Este Hoja de afectaci¢n ya fue atendida'
 Vmens11 = 'Este Hoja de Afectaci¢n ha sido devuelto'

SELECT Hoja

GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista
ON KEY LABEL F2 DO VISOBS
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
 TITLE ' Estad¡stica Diaria por Objeto del Gasto ' COLOR SCHEME 10

 DEFINE WINDOW Wind_2A FROM 14,41 TO 23,79 DOUBLE ;
 TITLE 'Est. Diaria por Objeto del Gasto' COLOR SCHEME 10

 DEFINE WINDOW Wind_2B FROM 14,00 TO 23,40 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,64 TO 22,78 ;
 TITLE ' TOTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_4 FROM 20,63 TO 22,77 ;
 TITLE ' PARTIDA ' COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 13,10 TO 17,70 ;
 TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_6 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

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
 @  0, 2 SAY "          Periodo :"
 @  1, 2 SAY "       N£mero H/A :"
 @  1,40 SAY "        Fecha H/A :"
 @  2, 2 SAY "   Tipo Documento :"
 @  3, 2 SAY " N£mero Documento :"
 @  3,26 say '.'
 @  3,40 SAY "  Fecha Documento :"
 @  4, 2 SAY "        Proveedor :"
 @  5, 2 SAY " F.Financiamiento :"
 @  5,40 SAY "            Nivel :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      Subprograma :"
 @  9, 2 SAY " Activid/Proyecto :"
 @ 10, 2 SAY " Partida Gen‚rica :"
 @ 11, 2 SAY "Partida Retenci¢n :"

 @ 9, 40 SAY " Comprobante Pago :"
* @ 11,55 SAY " Comprobante Pago :"

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Hoja
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR
 @  0,22 say m.Periodo
 @  0,60 say veresthc(m.estado)
 @  1,22 SAY m.NumMes
 @  1,24 SAY '.'
 @  1,25 SAY m.NumHc
 @  1,60 SAY m.FecHc
 @  2,22 SAY m.Tipdoc
 @  2,28 SAY VAl_para(m.TipDoc,'TIPDOC','D',28,30)
 @  3,22 say space(55)

 DO CASE
    case m.TipDoc='O/'
	    @  3,22 SAY m.NumRef
	case m.TipDoc#'O/'
	    @  3,22 SAY m.DesRef
 ENDCASE

 @  3,60 SAY m.FecRef
**
 @  4,22 CLEAR TO 11,79
 @  4,22 SAY IIF(m.tipprv='O',m.Nombre,IIF(m.TipPrv='P',val_prv(m.Codprv),val_pro(m.Codemp)))

 @  6,22 SAY val_para(m.CodFte,'CODFTE','V',26,20)
 @  6,60 SAY val_para(m.TipFun,'TIPFUN','D',60,20,2)

 @  7,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',22,40)
 @  8,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @  9,02 SAY IIF(alltrim(m.Tipfun)='I',"         Proyecto :",IIF(!empty(substr(m.codcal,13,3)),"        Actividad :" ,"                   " ))
 @  9,22 SAY IIF(!empty(substr(m.codcal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,50),' ')
 IF alltrim(m.Tipfun)='I'
*   @  6,22 SAY m.codobra
    @  9,25 SAY IIF(!EMPTY(substr(m.codcal,16,2)),'.',' ')
    @  9,26 SAY IIF(!EMPTY(substr(m.codcal,16,2)),VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5),' ')
	
    @ 10,02 SAY " Partida Gen‚rica :"
    @ 10,22 say m.CodPart
 else
    @ 10,02 SAY Spac(25)	
 endif	
@ 9,60 SAY iif(!empty(numcp),nummescp + '/'+numcp+' ','Sin Pago')
*@ 10,60 SAY iif(!empty(numcp),nummescp + '/'+numcp+' ','Sin Pago')
@ 11,00 SAY PADC(' ° ®F2¯  Observaciones °                      ° ®F4¯ Imprime H/C ° ',79,' ') COLOR W+/B

DO VISTA_HIJO
DO TOTAL
RETURN


PROCEDURE VISTA_HIJO
*-------------------
hide popup all
ACTIVATE WINDOW WIND_2
SELE ITEHC
SEEK m.NumMes + m.NumHc
IF FOUND()
IF alltrim(m.TipFun)='I'
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMes + m.NumHc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   TIPOPE :H='ä' :W=.F. ,;
   HOJA.CodPart :H='Part.' ,;
   Itehc.CodAnal     : H='Anlt.' ,;
   AA=VAL_PART(SUBSTR(itehc.CodAnal,4,2),LEFT(itehc.CodAnal,2),'D',22,50) :H='Descripci¢n' :37,;
   Itehc.Valpart     : H='  Parcial' :P='99,999,999.99',;
   XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :15
ELSE
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMes + m.NumHc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   TIPOPE :H='ä' :W=.F. ,;
   itehc.Codanal     : H='Partida' ,;
   AA=VAL_PART(SUBSTR(itehc.Codanal,4,2),LEFT(itehc.Codanal,2),'D',22,50) :H='Descripci¢n' :41,;
   itehc.ValparT     : H='  Parcial' :P='99,999,999.99',;
   XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :15
ENDIF
ELSE
   CLEAR
   @ 2,25 SAY 'No existe detalle, Revise..'
ENDIF
SELE Hoja
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
 SELECT HOJA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO NUMMES + NUMHC INTO ITEHC
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
 DO Vista

RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
PRIVATE VTEMP
 SELECT HOJA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_MES = '00'
 vNum_HC  = '0000'
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero H/C : '
 @ 1,23 GET vNum_Mes PICTURE '!!'
 @ 1,25 SAY '.'
 @ 1,26 GET vNum_Hc  PICTURE '!!!!'
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_Hc) .or. LASTKEY()=27
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
SELE HOJA
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
SELE HOJA
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
PRIVATE VTEP
 VALCS = .T.
 SELECT hoja
 VTEP = RECNO()
 DO CASE
     CASE ESTADO='9'
	     DO STANDBY WITH 'La H/C ya est  anulada'
	     do vista
	     return
    CASE ESTADO='50'
	     DO STANDBY WITH 'La H/C ya est  Pagada'
	     do vista
	    * return
    CASE ESTADO='51'
	     DO STANDBY WITH 'La H/C ya est  contabilizada'
	     do vista
	     return
 ENDCASE
 DO PANTALLA
 SCATTER MEMVAR
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyec  = substr(m.codcal,13,3)
 vCodact  = substr(m.codcal,13,2)
 vSubpry  = substr(m.codcal,16,4)
 *
 m.PartRet= space(5)
 
 @  0,22 GET m.periodo picture '!!' DISABLE
 @  1,22 GET m.NumMes DISABLE
 @  1,24 SAY '.'
 @  1,25 GET m.NumhC  PICTURE '!!!!' VALID !EMPTY(m.NumHc) disable
 @  1,60 GET m.FechC  VALID !EMPTY(M.FECHC)
 DO CASE
    case m.TipDoc='O/'
        @  2,22 SAY VAl_para(m.TipDoc,'TIPDOC','V',22,30)
	    @  3,22 GET m.NumRef
	    @  3,26 SAY '.'
	    @  3,27 GET m.PerRef
	case m.TipDoc#'O/'
        @  2,22 GET m.tipdoc valid VAl_para(m.TipDoc,'TIPDOC',' ',22,30)
	    @  3,22 GET m.DesRef
 ENDCASE

 @  3,60 SAY m.FecRef

 @  4,22 GET m.TipPrv    PICTURE '@M P,E,O' VALID LIMPIA()
 @  4,24 GET m.Codprv    PICTURE '!!!!'  VALID valprv()  WHEN m.TipPrv='P'
 @  4,24 GET m.CodEmp    PICTURE '!!!!!' VALID val_pro(m.Codemp,.T.,4,30) WHEN m.TipPrv='E'
 @  4,24 GET m.Nombre    PICTURE '@S40'  when m.TipPrv='O'
 *
 @  5,22 GET m.Codfte     PICTURE '!!!'  VALID VAL_PARA(m.Codfte ,'CODFTE',' ',22,20)
 @ 5,60 GET m.tipfun     PICTURE '!'     VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) AND LIMPIA2()  AND V_OBRA(m.tipdoc,m.TipFun)

 @  7,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg ,'CODPRG',' ',22,40)
 @  8,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
 @  9,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  9,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F'
 @  9,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @ 10,22 GET m.Codpart   picture '!!.!!' valid VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),'C',22,60)  WHEN ALLTRIM(m.tipfun)='I'

 @ 11,22 GET m.PartRet  PICTURE '!!.!!' valid VAL_PART(SUBSTR(m.PartRet,4,2),LEFT(m.PartRet,2),' ',22,60)  WHEN YESNO('Tiene Retenci¢n')

 READ VALID Val_Read() AND VAL_MS()

 IF LASTKEY() # 27 and vgun
    ON KEY LABEL F7 DO VAL_cal
    vAntpart = iif(ALLTRIM(m.tipfun)='I',m.codpart,'     ')
    cd = IIF(ALLTRIM(m.tipfun)='I',analcal(0,3,.T.),analcal(0,IIF(m.Tipdoc='O/C',1,IIF(m.Tipdoc='O/S',2,3)),.f.))
    oK = iif(m.TipDoc='O/' AND VALCS,Escoge_o(),Trab_hijo())
    IF OK .AND. LASTKEY()#27
       SELECT ITEHC
       SET ORDE TO ITEHC1
       SEEK ALLTRIM(m.NumMes) + m.NumHc
       vTotal = 0
       SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc AND ESTADO#'92'
           vtotal = vtotal + valpart
           REPLACE CodFte  WITH  m.codfte ,;
 				   CodPrg  WITH substr(m.codcal,08,2),;
                   CodSubPR  WITH substr(m.codcal,10,3),;
                   codpart with iif(alltrim(m.tipfun)='I',vAntpart,'     '),;
                   tipfun with iif(ALLTRIM(M.TIPFUN)='I','I','F')
		   IF ALLTRIM(M.TIPFUN)='I'
                      REPLACE CodProy WITH substr(m.codcal,13,3),;
                     		  CodSupry With substr(m.codcal,16,2),;
					  		  CodObra with substr(m.codcal,18,2)
		   ELSE
			  REPLACE Codact with substr(m.codcal,13,2)
		   ENDIF	  	
       ENDSCAN
       GO TOP
       SELE HOJA
       m.Codcal = ALLTRIM(str(year(m.fechc)-1900))+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
       m.Imptot = vtotal
       m.codpart = vantpart
       vseek = m.Periodo+m.Numref+ALLTRIM(m.Codfte)
       IF LEFT(m.tipdoc,2)='O/'
          DO CASE
   	         CASE alltrim(m.tipdoc)='O/C'
 	      	        SELECT Orden
 	      	        SET ORDER TO ORDCOM1
           	 CASE alltrim(m.tipdoc)='O/S'
					SELECT Ordser
 	      	        SET ORDER TO ORDSER1					
		  ENDCASE 	      	
	 	  seek ALLTRIM(vseek)

	 	  if found()
		 	  IF RLOCK()
		 	     REPLA NUMHC WITH M.NUMHC,PERHC WITH M.NUMMES,ESTADO WITH IIF(ESTADO='5',ESTADO,'20')
		 	  ENDIF
		 	  UNLOCK
              DO v_dest		  		 	  
		  else
		      do standby with 'La '+iif(alltrim(m.tipdoc)='O/S','O/S','O/C')+' no est  conectada a Abastecimentos'
		  endif	
      ELSE		
         ACTIVATE WINDOW wind_6
         @ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
         READ
         DEACTIVATE WINDOW wind_6
	  ENDIF		
      USE IN 8
      USE IN 9
      USE IN 10
      USE IN 11
      DO COMPRE
      SELECT HOJA
	   m.user = sys(0)
	   m.user_fc = date()
       m.user_TP = 'C'
      
      =observa()
      
      SELECT HOJA
      GATHER MEMVAR
    ELSE
       DO STANDBY WITH 'Proceso cancelado'
    ENDIF
 ENDIF
 UNLOCK ALL
 CLOSE DATA
 DO ABRE
 SELECT HOJA
 GO TOP
 GO VTEP
DO Vista
ON KEY LABEL F2 DO VISOBS
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
PRIVATE VTEMPW
VALCS = .T.
SELECT Hoja

XS=ORDER()
DO PANTALLA
SCATTER MEMVAR BLANK
 vTempW = recno()
 m.Estado = '00'
 m.NumRef = space(4)
 m.PartRet= space(5)

 vCodPrg  = space(2)
 vCodSub  = space(3)
 vProyec  = space(3)
 vSubpry  = space(4)
 vCodAct  = space(3)

 @ 0,22 GET m.periodo picture '!!' valid !empty(m.periodo)
 @ 1,22 GET m.NumMes PICTURE '!!' VALID VAL_PARA(m.NumMes,'FECMES','C',22,30) AND VAL_MES()
 @ 1,24 SAY '.'
 @ 1,25 GET m.NumHC PICTURE '!!!!' VALID VALER() AND !EMPTY(m.NumHc) 
 @ 1,60 GET m.FechC 
 @ 2,22 GET m.TipDoc PICTURE '!!!'  VALID val_para(m.TipDoc,'TIPDOC',' ',22,30) 
 @ 5,22 GET m.Codfte PICTURE '!!!'  VALID LIMPIA1() AND VAL_PARA(m.Codfte ,'CODFTE','C') when m.tipdoc = 'O/'
 @ 3,22 GET m.NumRef PICTURE '!!!!' VALID ValRf() and BUSCAR() when m.tipdoc = 'O/'   &&  cuando es O/C
 @ 3,27 GET m.PerRef PICTURE '!!'   when m.tipdoc = 'O/'   &&     o O/S
 @ 3,22 GET m.DesRef PICTURE '@S15' when m.tipdoc # 'O/'
 @ 3,60 GET m.FecRef

 @ 4,22 GET m.TipPrv    PICTURE '@M P,E,O' VALID LIMPIA()
 @ 4,24 GET m.Codprv    PICTURE '!!!!'  VALID valprv()  WHEN m.TipPrv='P'
 @ 4,24 GET m.CodEmp    PICTURE '!!!!!' VALID val_pro(m.Codemp,.T.,4,30) WHEN m.TipPrv='E'
 @ 4,24 GET m.Nombre    PICTURE '@S40'  when m.TipPrv='O'

 @ 5,22 GET m.Codfte    PICTURE '!!!'   VALID VAL_PARA(m.Codfte ,'CODFTE',' ',22,20) when m.tipdoc # 'O/'
 *@ 6,60 GET m.tipfun    PICTURE '!'     VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) AND LIMPIA2()
 @ 5,60 GET m.tipfun     PICTURE '!'     VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) AND LIMPIA2()  AND V_OBRA(m.tipdoc,m.TipFun)
 
 @ 7,22 GET vCodPrg     PICTURE '!!'    VALID VAL_PARA(vCodPrg ,'CODPRG',' ',22,40)
 @ 8,22 GET vCodSub     PICTURE '!!!'   VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
 @ 9,22 GET vProyec     PICTURE '!!!'   VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @ 9,22 GET vCodAct     PICTURE '!!!'   VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F'
 @ 9,26 GET vSubPry     PICTURE '!!!!'  VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40) when !EMPTY(vProyec) and m.tipfun='I'
 @ 10,22 GET m.Codpart  picture '!!.!!' valid VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),' ',22,60)  WHEN ALLTRIM(m.tipfun)='I'
 
 @ 11,22 GET m.PartRet  PICTURE '!!.!!' valid VAL_PART(SUBSTR(m.PartRet,4,2),LEFT(m.PartRet,2),' ',22,60)  WHEN YESNO('Tiene Retenci¢n')
 
 READ VALID Val_Read() AND VAL_MS()
  
 IF LASTKEY() # 27 and vgun
    vtipfun = m.tipfun
    USE IN 13
    DO CASE
       CASE m.TipDoc='O/S'
            USE IN 8
            USE IN 9 
            USE IN 10
       CASE m.TipDoc='O/C'
            USE IN 11
    ENDCASE       
    ON KEY LABEL F7 DO VAL_cal
    vAntpart = iif(ALLTRIM(m.tipfun)='I',m.codpart,'     ')
    cd = IIF(ALLTRIM(m.tipfun)='I',analcal(0,3,.T.),analcal(0,IIF(m.Tipdoc='O/C',1,IIF(m.Tipdoc='O/S',2,3)),.f.))
    oK = iif(m.TipDoc='O/' AND VALCS,Escoge_o(),Trab_hijo())
    IF OK .AND. LASTKEY()#27
       *-calcula total de parciales
       SELECT ITEHC
       m.tipfun = vtipfun
       SEEK ALLTRIM(m.NumMes) + m.NumHc
       vTotal = 0
       SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc
               vtotal = vtotal + valpart
               IF valpart=0
                  if rlock()
                     delete next 1
                  endif
                  unlock
               else
                  if rlock()
			         REPLACE CodFte  WITH m.Codfte,CodPrg  WITH vCodprg,CodSubPR  WITH vcodsub , codpart with iif(alltrim(m.tipfun)='I',vAntpart ,'     ')
  					 if alltrim(m.tipfun)='I'
    	  		        REPLACE CodProy WITH vProyec,CodSuPry WITH substr(vSubPry,1,2),CodObra WITH substr(vSubPry,3,4)
  		             else
        		        REPLACE  CodAct WITH vCODACT
        		     endif
        		     REPLACE TIPFUN WITH m.tipfun
        		  endif
        		  unlock
		       endif
          ENDSCAN
          GO TOP
          SELECT HOJA
          m.Imptot  = vtotal
          m.Codcal  = ALLTRIM(str(year(m.fechc)-1900))+alltrim(m.Nummes)+alltrim(m.Codfte)+alltrim(vCodPrg)+alltrim(vCodSub)+IIF(ALLTRIM(M.TIPFUN)='I',alltrim(vProyec)+ALLTRIM(VSubPry),ALLTRIM(VCODACT))
          m.codpart = vantpart
          vImpr = m.Periodo+ALLTRIM(m.Nummes)+m.Numhc
          vseek = m.Periodo+m.Numref+ALLTRIM(m.Codfte)
          *- Aumento el correlativo
          SELECT Parma
          SEEK 'HOJCON'+ALLTRIM(m.NumMes)
          REPLACE NumEnt WITH NumEnt + 1
          IF LEFT(m.tipdoc,2)='O/'
	          DO CASE
    	         CASE alltrim(m.tipdoc)='O/C'
        	        SELECT Orden
 	      	        SET ORDER TO ordCOM1
            	 CASE alltrim(m.tipdoc)='O/S'
					SELECT Ordser
 	      	        SET ORDER TO ordser1					
		 	  ENDCASE
		 	  seek ALLTRIM(vseek)
		 	  if found()
			 	  IF RLOCK()
			 	     REPLA NUMHC WITH M.NUMHC,PERHC WITH M.NUMMES,ESTADO WITH IIF(ESTADO='5',ESTADO,'20')
			 	     *if alltrim(m.tipdoc)='O/S'
			 	     *   replace codpart with IIF(alltrim(m.tipfun)="I",m.codpart,"     "),codanal with IIF(alltrim(m.tipfun)="F",itehc.codanal,"     ")
			 	     *endif 
			 	  ENDIF
			 	  unlock
			 	  do v_dest
			  else
			      do standby with 'La '+iif(alltrim(m.tipdoc)='O/S','O/S','O/C')+' no est  conectada a Abastecimentos'
			  endif	
		  ELSE
	         ACTIVATE WINDOW wind_6
    	     @ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
	         READ
	         DEACTIVATE WINDOW wind_6
		  ENDIF		 	
  	      USE IN 8
    	  USE IN 9
	      USE IN 10    	
   	   	  USE IN 11
       	  DO COMPRE
  	      SELE HOJA
  		   m.user = sys(0)
  		   m.user_fc = date()
	       m.user_TP = 'I'

          IF F_Appd()
             GATHER MEMVAR
          ENDIF
          vTempW = recno()
          =observa()
        ENDIF

 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 CLOSE DATA
 DO ABRE
 SELE HOJA
 GO TOP
 go vtempW
 DO VISTA
 ON KEY LABEL F2 DO VISOBS
RETURN


PROCEDURE V_OBRA 
*----------------
PARAMETER  m.TipDoc,m.TipFun
IF m.tipdoc <> 'O/'
	IF ALLTRIM(M.TipFun)='I'
		USE Obra  IN 14  ORDER TAG Obra2  ALIAS Obra
		SELECT Obra
		IF YESNO("Ingresa Obra")
			@ 6,2 SAY "C¢digo de la Obra :"
			*val_para(m.CodObra,'m.CodObra','D',52,40,2)
			@ 6,22 GET m.Tipobr  PICTURE "@!" VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40)    
			@ 6,24 GET m.Codobra PICTURE "@!" VALID Val_Obra(m.CodObra,'m.CodObra',' ',52,40) 
			READ
			IF SEEK (m.CodObra)
				@ 6,30 SAY RTRIM(Obra.Descri)
			ENDIF
		ELSE
		    m.Tipobr  = space(1)
    	    m.CodObra = space(3)
		ENDIF		
		USE IN 14
	ENDIF
ENDIF
RETURN


PROCEDURE LIMPIA
*---------------
@ 4,22 SAY SPAC(56)
RETURN


PROCEDURE LIMPIA1
*---------------
@ 3,22 SAY SPAC(20)
RETURN

PROCEDURE LIMPIA2
*----------------
IF ALLTRIM(M.TIPFUN)='F'
	@ 10,02 SAY SPAC(38)
ENDIF
RETURN

PROCEDURE Escoge_O            && Revisi¢n de BD en browse
*-----------------
PRIVATE vTemp
aS=ALIAS()
private  vfun
vfun = .f.
vInd = SYS(3) + '.IDX'
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
        *SE IN 11
        SELE ITEOC
        SET ORDER TO ITEOC1
   CASE alltrim(m.tipdoc)='O/S'
        *SE IN 8
        *SE IN 9
        SELE ORDSER
        SET FILTER TO NUMOS = m.NumRef and PERIODO = m.Periodo and Codfte=alltrim(m.Codfte) &&AND estado='00'
        GO BOTT
   OTHER
        SELE &AS
        RETURN vfun
ENDCASE
IF EOF()
   DO standby WITH ' Documento no en Linea '
   SET FILTER TO
   SET RELATION TO
   SELE HOJA
   VFUN=Trab_hijo()
   RETURN vfun
ENDIF

vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°° ®F10¯ para confirmar     ®F7¯ CALENDARIO      ®Esc¯ Cancela °°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
   		if alltrim(m.tipfun)='I'
	        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key m.periodo+m.numref+alltrim(m.codfte) FIELDS ;
	        Codanal :H='Partid':v=VAL_PART(SUBSTR(Iteart.CodPart,4,2),LEFT(iteart.CodPart,2),'iteart.codpart') and asigpar() :f,;	        
	        CodArt  :H='C¢digo' :W=.f. ,;
	        descri  :H='Art¡culo ' :32 :W=.f.,;
	        coduni  :H='Unid' :3 :W=.f.,;
	        Canreq  :H='Cantid':W=.f.,;
	        ValTot  :H='Total' :P='999,999.99' :W=.F. :V=val_afe(codpart,VALTOT,vpart):f
	   else        &&and val_afe(codanal,VALTOT)
	        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key m.periodo+m.numref+alltrim(m.codfte) FIELDS ;
	        Codanal :H='Partid':v=VAL_PART(SUBSTR(Iteart.CodPart,4,2),LEFT(iteart.CodPart,2),'iteart.codpart') and asigpar():f,;
	        CodArt  :H='C¢digo' :W=.f. ,;
	        descri  :H='Art¡culo ' :32 :W=.f.,;
	        coduni  :H='Unid':3 :W=.f.,;
	        Canreq  :H='Cantid':W=.f.,;
	        ValTot  :H='Total' :P='999,999.99' :W=.F. :V=val_afe(codanal,VALTOT,vpart)
		endif
        vInd = SYS(3) + '.IDX'
        USE IN 12
        USE IN 13
        sele iteoc
        set order to iteoc8
        do pasa_it 
    CASE alltrim(m.tipdoc)='O/S'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU FIELDS ;
    	    Codanal :H='Partida' :v=asignp():f ,;    	    
	        Est = IIF(Estado= '00','Pend',IIF(Estado = '20','afec',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' :W=.F. ,;
	        FecOs  :H='Fecha' :W=.F. ,;
	        CodDep :H='DEP' :W=.F. ,;
	        DESOS  :H='Descripci¢n' :30 :W=.F.,;
	        ValTot :H='Total' :P='999,999.99' :W=.F. :V = val_afe(codanal,VALTOT,vpart):f
ENDCASE
vORDE = RECNO()
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
IF LASTKEY()#27
   DO AGREGA_ITEM
   vFun = .T.
ELSE
   vFun = .F.
ENDIF
ON KEY LABEL F10
SET FILTER TO
CLOSE INDEX
*ERASE (VIND)
SET RELATION TO
SELE HOJA

RETURN vFun


PROCEDURE Trab_hijo            && Revisi¢n de BD en browse CUANDO # O/?
*------------------
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°F7->CALENDARIO°°°°°F5->Agregar°°°°°F8->Eliminar°°°°°°F10->Terminar°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itehc
SET ORDE TO ITEHC1
seek alltrim(m.Nummes)+m.Numhc
SET FILT TO ESTADO#'92'
if !found()
   DO agreg_item
endif
DO CASE
   CASE alltrim(m.tipfun)='I'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhc FIELDS ;
             TIPOPE   : H='ä' :V=TIPOPE$'-+':F :W=.F.,;
             CodPart  : H= 'Partida' :V=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'codpart')  AND PONE():F ,;
             CodAnal  : H= 'Analitc' :V=VAL_PART(SUBSTR(CodAnal,4,2),LEFT(CodAnal,2),'codanal'):F ,;
             aa = IIF(!EMPTY(CODanal),VAL_PART(SUBSTR(Codanal,4,2),LEFT(Codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(CodAnal) ,;
             ValPart :H='Total' :P='99,999,999.99' :V=CALE() AND val_afe(codPART,ITEHC.VALPART,VPART):F
   CASE alltrim(m.tipfun)='F'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key alltrim(m.Nummes)+m.Numhc FIELDS ;
             TIPOPE   : H='ä' :V=TIPOPE$'-+':F :W=.F.,;
             CodAnal  : H= 'Analitc' :V=VAL_PART(SUBSTR(CodAnal,4,2),LEFT(CodAnal,2),'codanal') and DOND() :F ,;
             aa = IIF(!EMPTY(CODanal),VAL_PART(SUBSTR(Codanal,4,2),LEFT(Codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(CodAnal) ,;
             ValPart :H='Total' :P='99,999,999.99' :V=CALE() AND val_afe(codanal,ITEHC.VALPART,VPART):F
   OTHER
ENDCASE
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SET FILTER TO
SELE HOJA
if lastkey()=27
   return .f.
endif
RETuRN .t.

procedure asignp
*---------------
replace codpart with iif(alltrim(m.tipfun)="I",m.codpart,"     ")
vpart = ordser.valtot  && O/S
retur .t.

procedure asigpar
*----------------
replace iteoc.codpart with iif(alltrim(m.tipfun)='I',HOJA.codpart,"     ")
vpart = iteoc.valtot   &&  O/C
return .t.	

PROCEDURE DOND
*------------
vpart = itehc.Valpart   && OTRO DOC
RETURN .T.

PROCEDURE PONE
*-------------
IF ALLTRIM(m.tipfun)='I'
   m.codpart = itehc.codpart
endif
vpart = itehc.Valpart
return .t.


PROCEDURE CALE
*------------
PRIVATE DF
DF = ALIAS()
REPLA CODFTE WITH SUBSTR(M.CODCAL,5,3),CODPRG WITH  SUBSTR(M.CODCAL,8,2) ,CODSUBPR WITH SUBSTR(M.CODCAL,10,3)
*IF  alltrim(m.tipfun)='I'
*	sele calen
*	set orde to calen6
*	seek m.Periodo+itehc.NumMes+itehc.CodPrg+itehc.CodFte+itehc.CodPart+itehc.CodSubpr+itehc.CodProy+itehc.CodSuPry+itehc.CodObra
*ELSE				  
*	Sele calen
*	set orde to calen5		          
*	seek m.Periodo+itehc.Nummes+itehc.CodPrg+itehc.CodFte+itehc.CodAnal+itehc.CodSubpr+itehc.CodAct
*ENDIF
SELE CALEN1
IF  alltrim(m.tipfun)='I'
    LOCATE FOR CODPART=ITEHC.CODPART
ELSE
    LOCATE FOR CODPART=ITEHC.CODANAL
ENDIF    

IF FOUND()
   IF RLOCK()
*  REPLACE TOTAFE WITH TOTAFE-(VPART-ITEHC.VALPART)
   ENDIF
   UNLOCK
ENDIF
SELE (DF)
RETURN .T.   


RETURN .T.


PROCEDURE Agreg_Item
*-------------------
sele iteHc
vp = codpart
IF F_appd()
      REPLACE NumHc  WITH m.NumHc ,;
              NumMes WITH m.NumMes,;
              codpart with vp ,;
              TipFun WITH m.TipFun ,;
              Estado WITH '00' ,;
              CodFte WITH m.Codfte ,;
              CodPrg WITH vCodprg ,;
            CodSubpR WITH vCodSub ,;
              tipdoc WITH m.tipdoc ,;
              tipope with '*'
              if alltrim(m.tipfun)='I'
                 replace CodProy WITH vProyec ,;
                        CodSuPry WITH vSubPry
              else
                 replace CodAct WITH VCODACT
              endif
ENDIF

RETURN .T.


PROCEDURE Elimi_Item
*-----------------
sele iteHc
if rlock()
   REPLACE VALPART WITH 0
   delete next 1
endif
return

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
 SELECT hoja
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF LEFT(Estado,1) = '9'
   * ya pas¢
   DO STANDBY WITH 'La H/C ya esta anulada o con H/M'
   RETURN
 ENDIF
 vElimina = YESNO('¨ Desea ANULAR esta Hoja de Control ?')
 IF vElimina 
    DO CASE
       CASE ALLTRIM(m.TipDoc)='O/C'
            SELE ORDEN
            SEEK m.Periodo+m.Numref+ALLTRIM(m.codfte)
            if found()
            IF RLOCK()
                REPLACE ORDEN.ESTADO WITH '00',ORDEN.PERHC WITH '  ',ORDEN.NUMHC WITH '    '
            ENDIF
            else
              DO STANDBY WITH 'No se pudo anular ...'            
            endif
       CASE ALLTRIM(m.TipDoc)='O/S'
            SELE ORDSER
            SEEK m.Periodo+m.Numref+ALLTRIM(m.codfte)
            if found()
            IF RLOCK()
                REPLACE ORDSER.ESTADO WITH '00',ORDSER.PERHC WITH '  ',ORDSER.NUMHC WITH '    '
            ENDIF
            ELSE
              DO STANDBY WITH 'No se pudo anular ...'
            endif
    ENDCASE
    UNLOCK ALL
    SELE hoja
    IF RLOCK()
      * REPLACE hoja.ESTADO WITH IIF(vfun,'90','99')
       REPLACE hoja.ESTADO WITH '99'
    ENDIF
    SELE CALEN
    REPLACE ALL TOTAFE WITH 0
    SELECT ITEHC
    SEEK ALLTRIM(m.NumMes) + m.NumHc

    SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc
         IF RLOCK() &&AND ESTADO#'92'
            *REPLACE itehc.ESTADO WITH IIF(vfun,'90','99')
            REPLACE itehc.ESTADO WITH '99'
         ENDIF
    ENDSCAN
    GO TOP
 ELSE
    do standby with 'Proceso cancelado'
 ENDIF
 selec hoja
 DO Vista
 UNLOCK ALL
 RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT Hoja
VCON = RECNO()
SCATTER MEMVAR
vNumOc = m.Nummes+m.NumHc
SET RELATION TO NUMMES + NUMHC INTO ITEHC
set filt to Nummes+NumHc=vNumoc
SET SKIP TO ITEHC
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"LisHc1",' Hojas de Control '
endif
set skip to
set filter to
SET RELATION TO 
SELECT HOJA
GO VCON
DO VISTA
RETURN




PROCEDURE Lista
*--------------
SELECT Hoja
PRIVATE VTEMP1
vtemp1 =recno()
CLOSE DATA
USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1    ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1   ALIAS Parma

USE maepre   IN 4   order tag maepre1   ALIAS maepre
USE itepar   in 5   order tag itepar1   ALIAS Itepar
USE Calen    IN 6   order tag calen1    ALIAS calen
USE Clase    IN 7   order tag Clase1    ALIAS Clase

USE Iteart   IN 10  order tag Iteart3   ALIAS iteart

USE Promae   IN 12  order tag Promae1   ALIAS Promae
USE Personal IN 13  order tag Personal2 ALIAS Personal

SELECT Hoja
SET RELATION TO NUMMES + NUMHC INTO ITEHC
SET SKIP TO ITEHC
if eof()
   do standby with vmens08
   return
else
  do lishjc
endif
SELECT HOJA
SET RELATION TO
CLOSE DATA
DO ABRE
SELECT HOJA
GO VTEMP1
DO VISTA
RETURN


PROCEDURE LisHjc
*---------------
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Hojas de Control' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToCli,vOrden,vTipPro,vLista,vTofue,vTomes
vCli = SPACE(4)
vfTE = Space(3)
vMes = Space(2)
vMesx = Space(2)
@ 01,01 SAY "        Total H/C : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "              H/C : "
@ 03,22 GET vMes    WHEN VTOCLI=2  PICTURE '!!'
@ 03,25 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!' VALID Val_Hc() 

@ 05,01 SAY "Todas las Fuentes : " GET vToFue  WHEN vToCli=1 FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) 
@ 07,01 SAY "           Fuente : "
@ 07,22 GET vFTe    WHEN vToFue=2 AND VTOCLI=1  PICTURE '!!!' valid val_para(vfte,'CODFTE','C') 

@ 09,01 SAY "  Todos los Meses : " GET vToMes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,11,22) WHEN VTOCLI=1
@ 11,01 SAY "              Mes : "
@ 11,22 GET vMesx   WHEN vToMes=2 AND VTOCLI=1 PICTURE '!!' valid val_para(vMesx,'FECMES','C') 

@ 13,01 SAY "      Listado por : " GET vLista   FUNCTION '^ Documento;Resumen;Res.det' WHEN vTocli=1
@ 16,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emisi¢n' WHEN vTocli=1

@ 19,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos' WHEN vTocli=1

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,NumMes+NumHc,IIF(vOrden=2,CodPrv,DTOS(FECHC)))+ITEHC.CODANAL  TO (vInd) ;
      FOR IIF(vToCli=1,.T.,NumMes+NumHc =ALLTRIM(vMes)+vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' )) ;
      AND IIF(vToFue=1,.T.,codfte=ALLTRIM(vfte)) AND IIF(vToMes=1,.T.,NumMes = ALLTRIM(vMesx))
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   SCATTER MEMVAR
   SET RELA TO NUMMES+NUMHC INTO ITEHC
   SET SKIP TO ITEHC
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
	   SET MEMOWIDTH TO 34
		IF !EMPTY(vMes) AND !EMPTY(Vcli)
			m.NumMes = Vmes
			m.NumHc  = Vcli
			DO Imprimir
		ELSE
   		    DO CASE
    		    case vLista = 1
        	    	 do reporte with 2,"LisHc1",' Hojas de Control '
	        	case vLista = 2
    	        	do reporte with 2,"LisHc3",' Hojas de Control '
        		case vLista = 3
            		do reporte with 2,"LisHc2",' Hojas de Control '            
	     	ENDCASE
   		ENDIF
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF
RETURN


PROCEDURE Imp
*------------
PARAMETER vMes,vCli
IF !EMPTY(vMes) AND !EMPTY(Vcli)
	m.NumMes = Vmes
	m.NumHc  = Vcli
	DO Imprimir
ENDIF
RETURN


PROCEDURE TERMI
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F7
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F2
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
SET MEMOWIDTH TO 34
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,22 TO 20,57 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERV WINDOW OBSERVA

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
   DEFINE WINDOW Observa FROM 03,22 TO 20,57 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERV NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

PROCEDURE COMPRE
*---------------
AX=ALIAS()
m.Valdeb = m.Imptot
m.Valhab = m.Imptot
m.CtaDeb = iif(alltrim(m.CODFTE)='TRN','9040100000','9020100000')
m.CtaHab = iif(alltrim(m.CODFTE)='TRN','9240100000','9220100000')
USE Cuentas  IN 14  order tag Cuentas6 ALIAS Cuenta
USE AstPre   IN 15  order tag Astpre1  ALIAS AstPre
SELECT CUENTA
ACTIVATE WINDOW WIND_5
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  GET m.CtaDeb PICTURE '!!!!!!!!!!' Valid val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",m.CtaDeb,1)
@ 01,18  GET m.ValDeb PICTURE '99,999,999.99' valid valedeb()
READ
@ 02,12  GET m.CtaHab PICTURE '!!!!!!!!!!' Valid val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",m.CtaHab,1)
@ 02,34  GET m.ValHab PICTURE '99,999,999.99' valid valeHab()
READ
DEACTIVATE WINDOW WIND_5
RETURN

procedure valedeb
*-----------------
AS=ALIAS()
SELECT ASTPRE
SEEK 'D'+alltrim(m.NumMes)+alltrim(m.NumHc) &&+alltrim(m.CtaDeb)
if !found()
   if f_appd()
      replace Nummes with m.Nummes,TipDoc with 'H/C',NumRef with m.NumHc , cuenta with m.CtaDeb, tipo with 'D',Fecref with m.fecref,CODPART WITH M.CODPART,periodo with m.periodo
   endif
   unlock
ENDIF
   if rlock()
      replace Ctadeb with m.CtaDeb ,Ctahab with space(10), Valdeb with m.ValDeb ,Valhab with 0 ,Codcal with m.Codcal,periodo with m.periodo
   endif
sele (as)
return

procedure valehab
*-----------------
AS=ALIAS()
SELECT ASTPRE
SEEK 'H'+alltrim(m.NumMes)+alltrim(m.NumHc) &&+alltrim(m.CtaHab)
if !found()
   if f_appd()
      replace Nummes with m.Nummes,TipDoc with 'H/C',NumRef with m.NumHc , cuenta with m.Ctahab, tipo with 'H',Fecref with m.fecref,CODPART WITH M.CODPART,periodo with m.periodo
   endif
   unlock
endif
   if rlock()
      replace Ctadeb with space(10),Ctahab with m.CtaHab , Valdeb with 0 ,Valhab with m.ValHab ,Codcal with m.Codcal,periodo with m.periodo
   endif
sele (as)
return


PROCEDURE VAL_HC             && Revisi¢n de BD en browse
*---------------
 SELECT HOJA
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK vMes+vCli
 IF !FOUND()
    SET RELATION TO NUMMES + NUMHC INTO ITEHC
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
 vcli = Numhc
 ON KEY LABEL F10
 SELECT HOJA
 SET RELATION TO
RETURN



function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv= iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.,4,30)
if xx
   m.codemp='     '
   return .t.
endif
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


PROCEDURE VAL_MES
*----------------
SELECT PARMA
SET ORDER TO PARMAE1
M.FecHc  = date()
SEEK 'HOJCON'+ALLTRIM(M.NumMes)
if !found()
   do standby with 'El Correlativo del Mes no est  Inicializado'
   SELE hoja
   return .F.
 else
   *vano = right(str(year(date()),4),2)
   IF CODIGOAUX='00'  &&#vano
      do standby with 'El Calendario del Mes '+alltrim(m.nummes)+' ya est  cerrado'
	  SELE hoja
   	  return .F.
   else
	  m.NumHc=valHC(Parma.NumEnt + 1)
	  xx = IIF(m.Nummes='12','01',padl(alltrim(str((val(m.NumMes)+1),2)),2,'0'))
	  m.Fechc = IIF( val(m.Nummes) < iif( xx='01',month(DATE())+12,month(DATE())),ctod('01/'+xx+'/'+ALLTRIM(STR(YEAR(m.fechc)-1900,4))) - 1,date())
	  sele hoja
      return .t.
   endif
endif

PROCEDURE VAL_MS
*----------------
SELECT PARMA
SEEK 'HOJCON'+ALLTRIM(M.NumMes)
if !found()
   do standby with 'El Correlativo del Mes no est  Inicializado'
   SELE hoja
   vgun = .f.
   return 
 else
   IF CODIGOAUX='00'
      do standby with 'El Calendario del Mes '+alltrim(m.nummes)+' ya est  cerrado'
	  SELE hoja
      vgun = .f.
   	  return 
   else
	  sele hoja
      vgun = .t.
   endif
endif


PROCEDURE BUSCAR
*---------------
***QUITAR
*IF worker
*  IF m.tipdoc='O/' and !YESNO('Valida O/C o O/S')  && and m.numref='B'
*     VALCS = .F.
*     return .t.
* endif
*ENDIF
IF m.tipdoc='O/' AND m.numref#'B' AND m.numref#'A'   &&  OR (m.tipdoc='O/' AND m.numref#'A')
	if alltrim(m.tipdoc)='O/C'
		select Orden
	else
		select Ordser
	endif
	set filt to codfte=alltrim(m.codfte) AND estado='00'
	go top
	if !eof()
	    
	    Seek m.periodo+m.Numref+alltrim(m.codfte)
	    if !found()
		    GO TOP
			do case
			   case alltrim(m.tipdoc)='O/C'
				 HIDE MENU mMenu
				 ACTIVATE SCREEN
				 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
				 DO Logos WITH Rotulo1,vTempo
				 ON KEY LABEL F10 KEYBOARD CHR(23)
				 SET RELATION TO PERIODO+NUMOC INTO ITEOC
				 SET SKIP TO ITEOC
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
					RETURN .F.
				 ENDIF
				 m.Numref = NUMOC
				 m.Fecref = FecOc
				 SHOW MENU mMenu
				 ON KEY LABEL F10
				 SET RELATION TO
		   	   case alltrim(m.tipdoc)='O/S'
				 HIDE MENU mMenu
				 ACTIVATE SCREEN
				 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
				 DO Logos WITH Rotulo1,vTempo
				 ON KEY LABEL F10 KEYBOARD CHR(23)
				 BROWSE WINDOW WIND_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
					 NumOs  :H=' N§ ' ,;
					 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
					 FecOs  :H='Fecha' ,;
					 CodDep :H='DEP',;
					 CodFTE :H='FTE ' ,;
					 DESOS  :H='Descripci¢n'
				 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
				 DO Logos WITH Rotulo1,vTempo
				 IF LASTKEY()=27
			    	RETURN .F.
				 ENDIF
				 SHOW MENU mMenu
				 ON KEY LABEL F10
				 m.Numref = NUMOS
				 m.Fecref = FecOs
		   	ENDCASE
		ELSE
			m.Numref = iif(alltrim(m.tipdoc)='O/C',NUMOC,NUMOS)
			m.Fecref = iif(alltrim(m.tipdoc)='O/C',FecOc,FecOs)
	    ENDIF
		m.Perref  = Periodo
		m.Codfte  = Codfte
		m.TipFun  = TipFun
		m.Codcal  = Codcal
		m.Codpart = Codpart
		m.CodPrv  = Codprv
        m.Destino = Destino				 		
        m.CodObra = CodObra
        m.TipObr  = TipObr
		vNummes  = Nummes
		vCodPrg  = substr(m.codcal,08,2)
		vCodSub  = substr(m.codcal,10,3)
		vProyec  = substr(m.codcal,13,3)
		vCodact  = substr(m.codcal,13,2)
		vSubpry  = substr(m.codcal,16,4)
		IF ALLTRIM(m.NumMes)#vNummes
			if yesno('No coincide con Mes '+vNumMes+' Inicial,Reemplazar?.. ')
		        m.Nummes = iif(alltrim(m.tipdoc)='O/C',Orden.Nummes,Ordser.Nummes)	 	
 				@ 1,22 GET m.NumMes PICTURE '!!' VALID VAL_PARA(m.NumMes,'FECMES','C',22,30) AND VAL_MES()		 	
				@ 1,24 SAY '.'
				@ 1,25 GET m.NumHC PICTURE '!!!!' VALID !EMPTY(m.NumHc)
				read
			endif	
	 	ENDIF
	 	SET FILT TO
	ELSE
    	DO standby WITH Vmens08
	    RETURN .F.
	ENDIF
ELSE	
    VALCS = .F.
    return .t.
ENDIF
RETURN

PROCEDURE AGREGA_ITEM
*-------------------
do borra_oc
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
      * PASAR DE ITEOC A ITEHC
        SEEK m.Periodo+m.NumRef+alltrim(m.Codfte) 
        if found()
        SCAN WHILE NUMOC = m.NumRef and PERIODO = m.Periodo and Codfte=alltrim(m.Codfte) 
          vPart1 =iteoc.Numoc+iteoc.codfte+IteOC.Codanal
          vPart2 =iteoc.numoc+iteoc.codfte+IteOC.Codanal
          vTot = 0
          * SCAN WHILE vPart1 = vPart2
          DO WHILE  vPart1 = vPart2
              vTot = vTot + Valtot
              SKIP
              vPart1 =iteoc.Numoc+iteoc.codfte+IteOC.Codanal
          ENDDO
          * ENDSCAN
          SKIP -1
          DO AGREG_HC
          SELE ITEOC
        ENDSCAN
        else
      	    do standby with 'Error...'
        endif
   CASE alltrim(m.tipdoc)='O/S'
        * PASAR DE ORDSER A ITEHC
        IF ALLTRIM(M.TIPFUN)="I"
        SCAN
          vPart1 = numos+codfte+CodPart
          vPart2 = numos+codfte+CodPart
          vTot = 0
          DO WHILE  vPart1 = vPart2
              vTot = vTot + Valtot
              SKIP
              vPart1 = numos+codfte+CodPart
          ENDD
          SKIP -1
          DO AGREG_HC
          SELE ORDSER
        ENDSCAN
        ELSE
        SCAN
          vPart1 = numos+codfte+CodAnal
          vPart2 = numos+codfte+Codanal
          vTot = 0
          DO WHILE  vPart1 = vPart2
              vTot = vTot + Valtot
              SKIP
              vPart1 = numos+codfte+Codanal
          ENDD
          SKIP -1
          DO AGREG_HC
          SELE ORDSER
        ENDSCAN

        ENDIF
ENDCASE
set filt to
return


PROCEDURE Agreg_HC
*-----------------
AS=ALIAS()
sele iteHc
IF F_appd()
   DO CASE
      CASE alltrim(m.tipdoc)='O/C'
           REPLACE  NumHc WITH m.NumHc ,;
		  		   NumMes WITH m.NumMes,;
                  ValParT WITH vTot    ,;
                   TipFun WITH m.TipFun ,;
                   Estado WITH '00' ,;
                   CodFte WITH m.Codfte ,;
                   CodPrg WITH vCodprg ,;
                   tipdoc WITH m.tipdoc ,;
                   tipope with 'è',;
                 CodSubpR WITH vCodSub,;
                  CodPart WITH IIF(ALLTRIM(m.Tipfun)='I',HOJA.Codpart,'     ') ,;
                  CodAnal WITH IteOC.CodANAL
          	 if alltrim(m.tipfun)='I'
                 replace CodProy WITH vProyec ,;
                         CodSuPry WITH vSubPry
	         else
    	         replace CodAct WITH vCodAct
        	 endif
      CASE alltrim(m.tipdoc)='O/S'
           REPLACE NumHc  WITH m.NumHc ,;
              TipFun WITH m.TipFun ,;
              tipdoc WITH m.tipdoc ,;
              tipope with 'è',;              
              NumMes WITH m.NumMes,;
              Descri WITH Ordser.Desos ,;
              ValParT WITH vTot    ,;
              CodPart WITH IIF(ALLTRIM(m.TipFun)='I',ordser.Codpart,'     ') ,;
               Estado WITH '00' ,;
              CodAnal WITH Ordser.Codanal   && codpart
   ENDCASE
   SELE &AS
   RETURN .T.
ENDIF
SELE &AS
RETURN .F.

PROCEDURE ABRE
*-------------
USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1    ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1   ALIAS Parma

USE maepre   IN 4   order tag maepre1   ALIAS maepre
USE itepar   in 5   order tag itepar1   ALIAS Itepar
USE Calen    IN 6   order tag calen1    ALIAS calen
USE Clase    IN 7   order tag Clase1    ALIAS Clase

USE OrdCom   IN 8   order tag OrdCom1   ALIAS Orden
USE IteOc    IN 9   order tag IteOc1    ALIAS IteOc
USE Iteart   IN 10  order tag Iteart3   ALIAS iteart
USE Ordser   IN 11  order tag OrdSer1   ALIAS OrdSer
USE Promae   IN 12  order tag Promae1   ALIAS Promae
USE Personal IN 13  order tag Personal2 ALIAS Personal
return


PROCEDURE Listar
*---------------
CLOSE DATA

USE HojCon   IN 1   order tag HojCon1   ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1    ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1   ALIAS Parma

USE maepre   IN 4   order tag maepre1   ALIAS maepre
USE itepar   in 5   order tag itepar1   ALIAS Itepar
USE Calen    IN 6   order tag calen1    ALIAS calen
USE Clase    IN 7   order tag Clase1    ALIAS Clase

USE Iteart   IN 10  order tag Iteart3   ALIAS iteart

USE Promae   IN 12  order tag Promae1   ALIAS Promae
USE Personal IN 13  order tag Personal2 ALIAS Personal

SELECT Hoja
SET RELATION TO NUMMES + NUMHC INTO ITEHC
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
SELE HOJA
SEEK ALLTRIM(m.NumMes)+m.NumHc

IF FOUND()
    DO STANDBY WITH 'La H/C ya ha sido generada'
    RETURN .F.
ELSE
	RETURN .T.
ENDIF	

procedure v_dest
*---------------
PRIVATE AZ
m.destino = IIF(EMPTY(m.destino),iif(alltrim(m.tipdoc)='O/C',ORDEN.DESTINO,ORDser.DESTINO),m.destino)
ACTIVATE WINDOW wind_6
@ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
READ
DEACTIVATE WINDOW wind_6
retuR

procedure pasa_it
*----------------
sele iteoc
seek m.periodo+m.numref+alltrim(m.codfte)
scan while periodo=m.periodo and numoc=m.numref and codfte=alltrim(m.codfte)
	vkf = iteoc.codart
	sele iteart
	seek vkf
	if found()
		if rlock()
		   replace codpart with iif(alltrim(m.tipfun)='I',iteoc.codpart,iteoc.codanal)
		endif
		unlock
	else
	    do standby with 'Error...'
	endif
	sele iteoc
endScan
return



