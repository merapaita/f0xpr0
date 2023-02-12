*--------------------------------------------------------------------------
* VERHJC.Prg
* VERIFICA HOJAS DE AFECTACION
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: Luis Castillo Dur n
* FECHA: 12/10/94
*--------------------------------------------------------------------------
USE HojCon   IN 1   order tag HojCon1  ALIAS Hoja
USE Itehc    IN 2   order tag Itehc1   ALIAS Itehc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE maepre   IN 4   order tag maepre1  ALIAS maepre
USE OrdCom   IN 5   order tag OrdCom1  ALIAS Orden
USE IteOc    IN 6   order tag IteOc1   ALIAS IteOc
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE Calen    IN 8   order tag calen1   ALIAS calen
USE Iteart   IN 9   order tag Iteart1  ALIAS iteart
USE Clase    IN 10  order tag Clase1   ALIAS Clase
USE Ordser   IN 11  order tag OrdSer1  ALIAS OrdSer
USE Itepar   IN 12  order tag Itepar1  ALIAS Itepar
USE astpre   IN 13  order tag Astpre1  ALIAS AstPre
USE Cuentas  IN 14  order tag Cuentas1 ALIAS Cuenta
USE personal IN 15  order tag personal1 ALIAS Personal
*USE hojmod   IN 16  order tag hojmod1  ALIAS HOJMOD
*- Mensajes de aviso al usuario

 Vmens01 = ' Hoja de Afectaci¢n : REVISION '
 Vmens02 = ' Verificaci¢n de Hoja de Afectaci¢n '
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  ®Verifica¯                Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE ' Estad¡stica Diaria por Objeto del Gasto ' COLOR SCHEME 10

 DEFINE WINDOW Wind_2A FROM 13,41 TO 23,79 DOUBLE ;
 TITLE 'Est. Diaria por Objeto del Gasto' COLOR SCHEME 10

 DEFINE WINDOW Wind_2B FROM 13,00 TO 23,40 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,63 TO 22,77 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 13,10 TO 17,70 ;
 TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD verif   OF mMenu PROMPT '®\<Verifica¯' AT 24,45
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD verif  OF mMenu DO verif
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
 @  1, 2 SAY "       N£mero H/A :"
 @  1,40 SAY "        Fecha H/A :"
 @  2, 2 SAY "   Tipo Documento :"
 @  3, 2 SAY " N£mero Documento :"
 @  3,40 SAY "  Fecha Documento:"
 @  4, 2 SAY "        Proveedor :"
 @  6, 2 SAY " F.Financiamiento :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      Subprograma :"
 @  9, 2 SAY " Activid/Proyecto :"
 @ 10, 2 SAY " Partida Gen‚rica :"


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
 @  0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','Con Cotiz',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='50','Atendido ','         '))))
 @  1,22 SAY m.NumMes
 @  1,24 SAY '.'
 @  1,25 SAY m.NumHc
 @  1,60 SAY m.FecHc

 @  2,22 SAY VAl_para(m.TipDoc,'TIPDOC',' ',22,30)
 @  3,22 say space(55)

 DO CASE
    case m.TipDoc='O/' 
	    @  3,22 SAY m.NumRef
 		*  @  3,26 SAY '.'
		*  @  3,27 SAY m.PerRef
	case m.TipDoc#'O/'
	    @  3,22 SAY m.DesRef
 ENDCASE

 @  3,60 SAY m.FecRef

 @  4,22 CLEAR TO 10,79
 @  4,22 SAY IIF(m.tipprv='O',m.Nombre,IIF(m.TipPrv='P',val_prv(m.Codprv),val_pro(m.Codemp)))

 @  6,22 SAY val_para(m.CodFte,'CODFTE','V',26,20)
 @  6,60 SAY val_para(m.TipFun,'TIPFUN','D',60,20,2)
 
 @  7,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',22,40)
 @  8,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @  9,02 SAY IIF(alltrim(m.Tipfun)='I',"         Proyecto :","        Actividad :"  )
 @  9,22 SAY IIF(!empty(substr(m.codcal,13,3)),VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,50),' ') 
 IF alltrim(m.Tipfun)='I' 
    @  9,25 SAY IIF(!EMPTY(substr(m.codcal,16,2)),'.',' ')
	@  9,26 SAY IIF(!EMPTY(substr(m.codcal,16,2)),VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5),' ')
    @ 10,02 SAY " Partida Gen‚rica :"	
    @ 10,22 say m.CodPart
 else
  * @  9,22 SAY Spac(60)	
    @ 10,02 SAY Spac(78)	
 endif	

@ 11,00 SAY PADC(' ° ®F2¯  Observaciones ° ',79,' ') COLOR W+/B

DO VISTA_HIJO
DO TOTAL
RETURN


PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SET RELA TO NUMMES+NUMHC INTO ITEHC
SET SKIP TO Itehc
GO TOP
IF alltrim(m.TipFun)='I'
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMes + m.NumHc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   hoja.CodPart  : H='Part.' ,;
   Itehc.CodAnal  : H='Anlt.' ,;
   IteHc.Estado   : H='Et'    ,;
   AA=VAL_PART(SUBSTR(itehc.Codanal,4,2),LEFT(itehc.Codanal,2),'D',22,50) :H='Descripci¢n' :35 ,;
   Itehc.ValparT     : H='  Parcial',;
   XX= '       '   : H='          ' :15
ELSE
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.NumMes + m.NumHc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   itehc.Codanal     : H='Partida' ,;
   IteHc.Estado   : H='Et'    ,;
   AA=VAL_PART(SUBSTR(itehc.CodAnal,4,2),LEFT(itehc.CodAnal,2),'D',22,50) :H='Descripci¢n' :39,;
   itehc.ValparT     : H='  Parcial',;
   XX= '       '   : H='          ' :15
ENDIF
SET RELA TO
SELE Hoja
RETURN


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
@ 0,4 SAY m.ImpTot picture '99,999.99'
return

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
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
 itehc.ValParT :H='Parcial' ,;
 ImpTot :H='TOTAL' :P='99,999.99'
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
 DO PANTALLA
 SELECT hoja
 SCATTER MEMVAR
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyec  = substr(m.codcal,13,3) 
 vCodact  = substr(m.codcal,13,2) 
 vSubpry  = substr(m.codcal,16,4)
 
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

 @  6,22 GET m.Codfte     PICTURE '!!!'  VALID VAL_PARA(m.Codfte ,'CODFTE',' ',22,20)
 @  6,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) AND LIMPIA2()
 
 @  7,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg ,'CODPRG',' ',22,40)  
 @  8,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
 @  9,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  9,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
 @  9,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @ 10,22 GET m.Codpart   picture '!!.!!' valid VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),' ',22,60)  WHEN ALLTRIM(m.tipfun)='I'
 
 READ VALID Val_Read()
 vImpr = m.Periodo+ALLTRIM(m.Nummes)+m.Numhc
 IF LASTKEY() # 27
    vAntpart = m.codpart
    vImpr = m.Periodo+ALLTRIM(m.Nummes)+m.Numhc
    oK = iif(m.TipDoc='O/' AND VALCS,Escoge_o(),Trab_hijo())    
    *oK = iif(m.TipDoc='O/',Escoge_o(),Trab_hijo())
    IF OK .AND. LASTKEY()#27
       SELECT ITEHC
       SEEK ALLTRIM(m.NumMes) + m.NumHc
       vTotal = 0
       SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc
           vtotal = vtotal + valpart
           REPLACE CodFte  WITH  substr(m.codcal,05,3),;
 				   CodPrg  WITH substr(m.codcal,08,2),;
                   CodSubPR  WITH substr(m.codcal,10,3)
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
       DO COMPRE
       SELECT HOJA
       GATHER MEMVAR
        =observa()
    ELSE
       DO STANDBY WITH 'Proceso cancelado'
    ENDIF
 ENDIF
 SELECT HOJA
 UNLOCK ALL
 DO Vista
 RETURN


PROCEDURE Verif              && Crea nuevo registro en BD
*--------------
 DO PANTALLA
 SELECT Hoja
 
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyec  = substr(m.codcal,13,3) 
 vCodact  = substr(m.codcal,13,2) 
 vSubpry  = substr(m.codcal,16,4)
 
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

 @  6,22 GET m.Codfte     PICTURE '!!!'  VALID VAL_PARA(m.Codfte ,'CODFTE',' ',22,20)
 @  6,60 GET m.tipfun    PICTURE '!'    VALID VAL_PARA(m.tipfun,'TIPFUN',' ',60,15,2) AND LIMPIA2()
 
 @  7,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg ,'CODPRG',' ',22,40)  
 @  8,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
 @  9,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  9,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
 @  9,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @ 10,22 GET m.Codpart   picture '!!.!!' valid VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),' ',22,60)  WHEN ALLTRIM(m.tipfun)='I'
 
 READ VALID Val_Read()
 =observa()
 DO COMPRE
 IF LASTKEY() # 27 AND YESNO('¨Est  seguro de Aprobar esta Hoja de Control?')
    *- Calcula total de parciales
    SELECT ITEHC
    SEEK ALLTRIM(m.NumMes) + m.NumHc
    xAcum = 0
    SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc AND ESTADO='00'
         AD=AFECTA()
         IF !AD
            xAcum = xAcum + 1
         ENDIF
         IF RLOCK() AND AD
            REPLACE Itehc.Estado WITH '50'
         ENDIF
    ENDSCAN
    GO TOP
    SELECT HOJA
    if xAcum =0
       m.Estado='50'
    else
      do standby with 'Existen '+str(xacum,2)+' no Verificadas'
    endif
    SELECT HOJA
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    SELECT HOJA
 ENDIF
 DO Vista
 RETURN

PROCEDURE LIMPIA
*---------------
@ 4,22 SAY SPAC(56)
RETUR

PROCEDURE LIMPIA1
*---------------
@ 3,22 SAY SPAC(20)
RETUR

PROCEDURE LIMPIA2
*---------------
IF ALLTRIM(M.TIPFUN)='F'
	@ 10,02 SAY SPAC(78)
ENDIF
RETUR


PROCEDURE AFECTA
*---------------
** Se hace el proceso de afectacion en ITEPAR
vPeriodo = left(Hoja.CodCal,2)
vMes     = substr(Hoja.CodCal,3,2)
vObra    = substr(Hoja.Codcal,18,2)
vKey     = vPeriodo+IteHC.CodPrg+IteHC.CODFTE+IteHC.TIPFUN+iif(m.TipFun='I',IteHC.CODPART,ITEHC.CODANAL)+IteHC.CODSUBPR+IIF(ITEHC.TIPFUN='I',ITEHC.CODPROY+ITEHC.CODSUPRY,ITEHC.CODACT)
vCal     = vPeriodo+vMes+IteHC.CODPRG+IteHC.CODFTE+iif(m.TipFun='I',IteHC.CodPart,left(iteHc.Codanal,2)+'.00')+IteHC.CODSUBPR+IIF(ITEHC.TIPFUN='I',itehc.CODPROY+itehc.CODSUPRY+vOBRA,itehc.CODACT)
zx       = Alias()
xz       = Order()
SELECT ITEPAR
SET ORDER TO IIF(alltrim(m.Tipfun)='F',3,4)
SEEK ALLTRIM(vKey)
vDun = .t.
IF FOUND()
   vCodFte   = substr(m.codcal,05,3)
   vCodPrg   = substr(m.codcal,08,2)
   vCodSub   = substr(m.codcal,10,3)
   vCodAct   = substr(m.codcal,13,2)
   vCodProy  = substr(m.codcal,13,3)
   vCodSuPy  = substr(m.codcal,16,2)
   
   ** AFECTA AL CALENDARIO
   SELECT CALEN
   SET ORDER TO IIF(alltrim(m.Tipfun)='F',5,6)
   SEEK ALLTRIM(vCal)
   
   IF FOUND()
      IF RLOCK()
         REPLACE Calen.TotAfe WITH Calen.TotAfe + Itehc.Valpart
      ENDIF
      UNLOCK
   ELSE
      DO STANDBY WITH 'No est  registrado en el Calendario,Consulte a Presupuesto'
      vdun = .f.
      *DO AGREG_CAL
   ENDIF

   ** AFECTA AL PRESUPUESTO
   
   IF RLOCK() AND vDun
      REPLACE ITEPAR.MES_&vMes WITH ITEPAR.MES_&vMes + itehc.valparT ,;
              ITEPAR.CANTEJE   WITH ITEPAR.CANTEJE + itehc.valparT
   ENDIF
   UNLOCK
   ** VALIDAR SI VALAPART-TOTAFE
ELSE
   DO STANDBY WITH 'No esta Registrado en el Marco,Consulte a Presupuesto'
   vDun = .f.
   *DO AGREG_PRE
   *DO AGREG_CAL
ENDIF
SELECT (zx)
SET ORDER TO (xz)
RETURN vDun


PROCEDURE Agreg_pRE
*-----------------
AS=ALIAS()
SELE ITEPAR
IF F_appd()
  if alltrim(itehc.tipfun)='F'
  REPLACE Periodo WITH vPeriodo ,;
          CodFte  WITH itehc.codfte ,;
          CodPrg  WITH itehc.codprg ,;
          CodSubPR  WITH itehc.codsubpr ,;
          CodAct  WITH itehc.codact ,;
          TipFun  WITH alltrim(itehc.TipFun),;
          Codpart with itehc.codanal ,;
          CANTEJE WITH CANTEJE + ITEHC.VALPART
  else
  REPLACE Periodo WITH vPeriodo ,;
          TipFun  WITH alltrim(itehc.TipFun) ,;
          CodFte  WITH itehc.codfte ,;
          CodPrg  WITH itehc.codprg ,;
          CodSubPR WITH itehc.codsubpr ,;
          CodProy WITH itehc.codproy ,;
          CodSuPRy WITH itehc.codsupry ,;
          Codpart with itehc.codanal ,;
          CANTEJE WITH CANTEJE + ITEHC.VALPART
  endif
SELE (AS)
  RETURN .T.
ENDIF
SELE (AS)
RETURN .F.


PROCEDURE Agreg_Cal
*-----------------
AS = ALIAS()
SELE CALEN
IF F_appd()
  if alltrim(itehc.tipfun)='F'
  REPLACE Periodo WITH vPeriodo ,;
          NumMes  WITH vMes ,;
          CodFte  WITH itehc.codfte ,;
          CodPrg  WITH itehc.codprg ,;
          CodSubPR  WITH itehc.codsubpr ,;
          CodAct  WITH itehc.codact ,;
          TipFun  WITH alltrim(itehc.TipFun),;
          Codpart with iif(itehc.TipFun='I',IteHC.CodaNAL,left(iteHc.CodANAL,2)+'.00'),;
          TOTAFE WITH TOTAFE+ITEHC.VALPART
  else
  REPLACE Periodo WITH vPeriodo ,;
          NumMes  WITH vMes ,;
          TipFun  WITH alltrim(itehc.TipFun) ,;
          CodFte  WITH itehc.codfte ,;
          CodPrg  WITH itehc.codprg ,;
          CodSubPR  WITH itehc.codsubpr ,;
          CodProy WITH itehc.codproy ,;
          CodSuPRy WITH itehc.codsupry ,;
          Codpart with iif(itehc.TipFun='I',IteHC.CodPart,left(iteHc.CodPart,2)+'.00'),;
          TOTAFE WITH TOTAFE+ITEHC.VALPART
   endif
  SELE (AS)
  RETURN .T.
ENDIF
SELE (AS)
RETURN .F.


PROCEDURE Escoge_O            && Revisi¢n de BD en browse
*-----------------
aS=ALIAS()
private  vfun
vfun = .f.
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
        SELE ITEOC
        SET RELATION TO 'B'+codart INTO Iteart
        SET FILTER TO NUMOC = m.NumRef and PERIODO = m.Periodo and Codfte=alltrim(m.Codfte)
        GO TOP
   CASE alltrim(m.tipdoc)='O/S'
        SELE ORDSER
        SET FILTER TO NUMOS = m.NumRef and PERIODO = m.Periodo and Codfte=alltrim(m.Codfte)
        GO TOP
   OTHER
        SELE &AS
        RETURN vfun
ENDCASE
IF EOF()
   DO standby WITH Vmens08
   SET FILTER TO
   SET RELATION TO
   SELE HOJA
   RETURN vfun
ENDIF
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°Presione ®F10¯ para confirmar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU FIELDS ;
        Iteart.CodPart :H='Partid':v=VAL_PART(SUBSTR(Iteart.CodPart,4,2),LEFT(iteart.CodPart,2),'iteart.codpart') and asigpar():f,;
        CodArt :H='C¢digo' :W=.f. ,;
        descri :H='Art¡culo ' :34 :W=.f.,;
        coduni :H='Unid' :W=.f.,;
        Canreq :H='Cantid':W=.f.,;
        ValTot :H='Total' :P='999,999.99' :W=.F.
   CASE alltrim(m.tipdoc)='O/S'
        BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU FIELDS ;
        Codpart :H='Partida' ,;
        Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' :W=.F. ,;
        FecOs  :H='Fecha' :W=.F. ,;
        CodDep :H='DEP' :W=.F. ,;
        DESOS  :H='Descripci¢n' :30 :W=.F.,;
        ValTot :H='Total' :P='999,999.99' :W=.F.
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
SET RELATION TO
SELE HOJA
RETURN vFun


PROCEDURE AGREGA_ITEM
*---------------------
do borra_oc
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
      * PASAR DE ITEOC A ITEHC
        SCAN
          vPart1 = Iteart.CodPart
          vPart2 = Iteart.CodPart
          vTot = 0
          DO WHILE  vPart1 = vPart2
              vTot = vTot + Valtot
              SKIP
              vPart1 = Iteart.CodPart
          ENDD
          SKIP -1
          DO AGREG_HC
          SELE ITEOC
        ENDSCAN
   CASE alltrim(m.tipdoc)='O/S'
        * PASAR DE ORDSER A ITEHC
        SCAN
          vPart1 = CodPart
          vPart2 = CodPart
          vTot = 0
          DO WHILE  vPart1 = vPart2
              vTot = vTot + Valtot
              SKIP
              vPart1 = CodPart
          ENDD
          SKIP -1
          DO AGREG_HC
          SELE ORDSER
        ENDSCAN
ENDCASE
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
                  CodPart WITH Iteart.Codpart,;
                   TipFun WITH m.TipFun ,;
                   Estado WITH '00' ,;
                   CodFte WITH m.Codfte ,;
                   CodPrg WITH vCodprg ,;
                   tipdoc WITH m.tipdoc ,;                  
                 CodSubpR WITH vCodSub,;
                  CodPart WITH Orden.Codpart,;        
                  CodAnal WITH Iteart.Codpart
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
              NumMes WITH m.NumMes,;
              Descri WITH Ordser.Desos ,;
              ValParT WITH vTot    ,;
              CodPart WITH Ordser.Codpart ,;
               Estado WITH '00' ,;
              CodAnal WITH Ordser.Codpart 
   ENDCASE
   SELE &AS
   RETURN .T.
ENDIF
SELE &AS
RETURN .F.


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
 SELECT hoja
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 vElimina = YESNO('¨ Desea ANULAR esta Hoja de Control ?')
 IF vElimina
    SELE hoja
    IF RLOCK()
       REPLACE hoja.ESTADO WITH '99'
    ENDIF
    SELECT ITEHC
    SEEK ALLTRIM(m.NumMes) + m.NumHc
    SCAN WHILE NumMes = ALLTRIM(m.NumMes) and NumHc = m.NumHc
         IF RLOCK()
            REPLACE itehc.ESTADO WITH '99'
         ENDIF
    ENDSCAN
    GO TOP
 ENDIF
 selec hoja
 DO Vista
 UNLOCK
 RETURN


PROCEDURE Lista
*--------------
SELECT Hoja
vtemp =recno()
SET RELATION TO NUMMES + NUMHC INTO ITEHC
SET SKIP TO ITEHC
if eof()
   do standby with vmens08
   return
else
  do lishjc
endif
SET SKIP TO
SET RELATION TO
select hoja
go vtemp
DO VISTA
RETURN

PROCEDURE LisHjc
*---------------
vTemp = RECNO()
DEFINE WINDOW LIS FROM 5,15 TO 19,65 FLOAT DOUBLE TITLE 'Listado Hojas de Control' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToCli,vOrden,vTipPro
vCli = SPACE(4)
@ 01,01 SAY "        Total H/C : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "              H/C : "
@ 03,22 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!' VALID Val_Hc()

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emisi¢n'

@ 08,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos'

@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,NumMes+NumHc,IIF(vOrden=2,CodPrv,DTOS(FECHC))) TO (vInd) ;
      FOR IIF(vToCli=1,.T.,NumMes+NumHc = vCli) .AND. IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',Estado = '50' ))
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ',IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
     do reporte with 2,"LisHc",' Hojas de Control '
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF

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
  RELEASE WINDOW wind_2
  RELEASE WINDOW wind_2A
  RELEASE WINDOW wind_2B
  RELEASE WINDOW wind_3
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN



function valHC
*-----------------
parameter vnumHC
private vfun
vfun = .t.
vnumHC =padl(alltrim(str(vnumHC,4)),4,'0')
return vNumhc
*--------------------------------------------------------------
FUNCTION Observa
*---------------
vAlias = ALIAS()
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
*   DEFINE WINDOW Observa FROM 03-2, 29-5 TO 20,60-5 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± OBSERVACIONES ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
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

RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03-2, 29-5 TO 20,60-1 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± OBSERVACIONES ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
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
m.Valdeb = m.Imptot
m.Valhab = m.Imptot
SELECT CUENTA
SET FILT TO CUENTA='9'
ACTIVATE WINDOW WIND_5
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  GET m.CtaDeb PICTURE '!!!!!!!!!!' Valid val_fun('Cuenta','Cuenta',"LEFT(Cuenta,4)+' '+Descri",m.CtaDeb,1)
@ 01,18  GET m.ValDeb PICTURE '99,999,999.99'  valid valedeb()
READ
@ 02,12  GET m.CtaHab PICTURE '!!!!!!!!!!' Valid val_fun('Cuenta','Cuenta',"LEFT(Cuenta,4)+' '+Descri",m.CtaHab,1)
@ 02,34  GET m.ValHab PICTURE '99,999,999.99'  valid valehab()
READ
DEACTIVATE WINDOW WIND_5
SET FILT TO
RETURN

procedure valedeb
*-----------------
AS=ALIAS()
SELECT ASTPRE
SEEK 'D'+alltrim(m.NumMes)+alltrim(m.NumHc) &&+alltrim(m.CtaDeb)
if !found()
   if f_appd()
      replace Nummes with m.Nummes,TipDoc with 'H/C',NumRef with m.NumHc , cuenta with m.CtaDeb, tipo with 'D',Fecref with m.fecref,CodPart with m.codpart
   endif
   unlock
ENDIF
   if rlock()
      replace Ctadeb with m.CtaDeb ,Ctahab with space(10), Valdeb with m.ValDeb ,Valhab with 0 ,Codcal with m.Codcal
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
      replace Nummes with m.Nummes,TipDoc with 'H/C',NumRef with m.NumHc , cuenta with m.Ctahab, tipo with 'H',Fecref with m.fecref,codpart with m.codpart
   endif
   unlock
endif
   if rlock()
      replace Ctadeb with space(10),Ctahab with m.CtaHab , Valdeb with 0 ,Valhab with m.ValHab ,Codcal with m.Codcal
   endif
sele (as)
return

function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv= iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.,4,28)
if xx
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
   SET ORDE TO 2
   seek xcod
   v_fun = iif(found(),Descri,"")
*   if mmsg
*      @ _x,_y say descri
*   endif
else
   if empty(xcod)
      set orde to 3
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
