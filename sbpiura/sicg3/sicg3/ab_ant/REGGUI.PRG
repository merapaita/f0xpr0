*-------------------------------------------------------------------------
* RegGui
* Registra Guias de Internamiento
* Estado :
*   '00' Emitida   Este es el que se registra en la guia de internamiento
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
*--------------------------------------------------------------------------
*- Abriendo Archivos
PARAMETER sistema
SET EXCL OFF
USE OrdCom   IN 1   order tag OrdCom1  ALIAS Orden
USE Iteoc    IN 2   order tag IteOc1   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE Artmae   IN 4   order tag Artmae1  ALIAS Produ
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec4  ALIAS Itepec
USE KARDEX   IN 7   order tag KARDEX2  ALIAS KARDEX
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE calen    IN 9   order tag calen1   ALIAS cale
USE IteArt   IN 10  order tag Iteart3  ALIAS Iteart


IF sistema='1'
	USE maepre   IN 11  order tag maepre1  ALIAS maepre
ELSE
	USE maepre   IN 11  order tag maepre3  ALIAS maepre
ENDIF	
USE itepar   in 12  order tag itepar1  ALIAS ITEPAR          
USE IteOC1   IN 13  order tag IteOC11  ALIAS IteOC1
PUBLIC SUMATOT

*- Mensajes de aviso al usuario
Vmens01 = ' Gu¡as de Internamiento : REVISION '
Vmens02 = ' Registro de Gu¡as de Internamiento '
Vmens04 = 'Dicho Gu¡as de Internamiento no fue encontrado'
Vmens05 = 'No existe Gu¡as de Internamiento anterior'
Vmens06 = 'No existe Gu¡as de Internamiento siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Gu¡as de Internamiento ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Gu¡as de Internamiento ha sido anulado'
Vmens10 = 'Este Gu¡as de Internamiento ya fue Contabilizado'
Vmens11 = 'Este Gu¡as de Internamiento ha sido devuelto'

SELECT Orden
GO BOTTOM

PUBLIC VTOPRV
*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

ON KEY LABEL F12 DO CORRI
ON KEY LABEL F6 DO CORRI1
ON KEY LABEL F2 DO FECREP

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  liQuida  Ingresa           Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: Gu¡as de Internamiento                   ®F9¯ Detalle ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_3 FROM 20,63 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10

 
 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT 'li\<Quida'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
*DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO liquida
 ON SELECTION PAD ingre  OF mMenu DO ingre
*ON SELECTION PAD anula  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
 @  1, 2 SAY "       N£mero O/C :"
 @  1,40 SAY "Fecha Liquidaci¢n :"
 @  2, 2 SAY "        Fecha O/C :"
 @  2,40 SAY "              H/C :" 
 @  3, 2 SAY "        Proveedor :"

 @  4, 2 SAY "      Cod. Cadena :"
 @  5, 2 SAY "         Fte.Fto. :"

 @  6, 2 SAY "          Funci¢n :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      Subprograma :"
 @  9, 2 SAY "  Activ./Proyect. :"

 @ 10, 2 SAY "          Destino :" 
 @ 11, 2 SAY "    Observaciones :"

RETURN



PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Orden
 ON KEY LABEL F9 DO VISTA_DET

 IF EOF()
   DO Pantalla
   RETURN
 ENDIF

 ACTIVATE WINDOW WIND_1
 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C') 

 @  0,60 SAY vEstOc(m.estado)
 @  1,22 SAY m.Periodo
 @  1,25 SAY m.NumOc 
 @  1,60 SAY m.FecDesp
 @  2,22 SAY m.FecOC
 @  2,60 SAY m.Perhc+' '+m.Numhc 
 @  3,22 SAY val_prv(m.Codprv)

 @  4,22 SAY val_codcad(m.codcad,m.periodo,'D',22,30)
 @  5,22 SAY val_para(m.codfte,'CODFTE','D',22,30)
 
 @  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

 @ 10,22 SAY m.destino picture '@S56'
 @ 11,22 SAY m.Observa

 DO VISTA_HIJO
 DO TOTAL
 RETURN
 

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT IteOc
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   Numpec      : H= 'Pecs' :w=.f. ,;   
   CodUni      : H= 'Uni'      :W=.F. :2,;   
   Descri      : H= 'Descripci¢n' :20 :W=.F. ,;
   PreUni      : H= 'PreUni' :P='99,999.99' :W=.F. ,;
   X=ROUND(CanReq*PreUni,5)  :H='Total'  :P='9,999,999.99' :W=.F. ,;
   CanReq      : H= 'Cantd' :P='99,999.99',;
   CanDesp     : H= 'Despd' :P='99,999.99',;
   XX=IIF(EMPTY(FecDesp),'Sin Ingr',FecDesp) :H= 'Atendido'    
SELE Orden

RETURN
&& + ALLTRIM(m.codfte)



PROCEDURE VISTA_DET
*------------------
hide popup all
SELECT IteOc
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOc + ALLTRIM(m.codfte) ;
   WINDOW Wind_2 ;
   FIELDS;
   Numpec      : H= 'Pecs' :w=.f. ,;      
   CodUni      : H= 'Uni'      :W=.F. :2,;   
   Descri      : H= 'Descripci¢n' :20 ,;
   PreUni      : H= 'PreUni' :P='99,999.99' :W=.F. ,;
   X=ROUND(CanReq*PreUni,5)  :H='Total'  :P='9.999,999.99' :W=.F. ,;
   CanReq      : H= 'Cantd' :P='99,999.99',;
   CanDesp     : H= 'Despd' :P='99,999.99' ,;
   FecDesp     : H= 'Atendido'   
SELE Orden
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
IF ESTADO='99'
   DO STANDBY WITH 'La O/C ya est  anulada'
   do vista
   return
ENDIF
IF ESTADO='00'
   DO STANDBY WITH 'La O/C no esta afectada'
   do vista
   return
ENDIF
IF ESTADO='50'
   DO STANDBY WITH 'La O/C ya est  Liquidada'
   do vista
   return       
ENDIF   
 DEACTIVATE WINDOW WIND_3
 SELECT Orden
 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C') 
 vFecDesp= date()

 IF LASTKEY() # 27
    DO WHILE .T.
       Ok =Ingresa_Hijo()
       IF LASTKEY() # 27 AND Ok
          IF YESNO('¨ Confirme el ingreso ?')
             EXIT
          ENDIF
       ELSE
          IF YESNO('¨ Cancela el ingreso ?')
             ok = .F.
             EXIT
          ENDIF
       ENDIF
    ENDDO

    IF Ok .AND. LASTKEY() # 27
       SELE ITEOC
       SET ORDE TO ITEOC2
       SEEK m.Periodo + m.NumOc &&+ ALLTRIM(m.CodFte)
       vIg = 0
       vEs = 0
       vfec=iteoc.fecdesp
       entro= .t.
       SCAN WHILE PERIODO+NUMOC=m.Periodo + m.NumOc
          IF Iteoc.CanDesp1=0
             vEs = vEs + 1
          ELSE               
            SELE KARDEX
            seek iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart
            if found()
               IF RLOCK()
   		          REPLACE TIPDOC WITH 'O/C', NUMDOC WITH m.NumOc ,FECDOC WITH m.FecOc, CODART with ITEOC.CodArt,FECENT WITH ITEOC.Fecdesp , CODDEP WITH ITEOC.Coddep ,CODCAD with ITEOC.CodCad,ENTCAN with ITEOC.Candesp1 ,;
                          PREUNIE WITH ITEOC.PreUni, ENTIMP WITH ITEOC.PreUni*KARDEX.EntCan ,HORENT WITH TIME(),PERIODO WITH m.Periodo,codfte with m.codfte,codcad with m.codcad,ESTADO WITH '  '
               ENDIF 
            ELSE
               IF F_APPD()
		          REPLACE TIPDOC WITH 'O/C', NUMDOC WITH m.NumOc ,FECDOC WITH m.FecOc, CODART with ITEOC.CodArt,FECENT WITH ITEOC.Fecdesp , CODDEP WITH ITEOC.Coddep ,CODCAD with ITEOC.CodCad,ENTCAN with ITEOC.Candesp1 ,;
                          PREUNIE WITH ITEOC.PreUni, ENTIMP WITH ITEOC.PreUni*KARDEX.EntCan ,HORENT WITH TIME(),PERIODO WITH m.Periodo,codfte with m.codfte,codcad with m.codcad
               ENDIF
               UNLOCK
            ENDIF
            SELE ITEOC
            IF RLOCK()
               REPLACE CANDESP WITH CANDESP+CANDESP1,ESTADO WITH IIF(CANDESP=CANREQ,'50','00'),CANDESP1 WITH 0
               * REPLACE CANDESP WITH CANDESP+CANDESP1,ESTADO WITH IIF(TIPORD='O',IIF(CANDESP=CANREQ,'50','00'),'60'),CANDESP1 WITH 0
            ENDIF   
            IF (candesp < canreq) 
               vIg = vIg + 1
            endif                
            if iteoc.fecdesp >= vfec
               * m.fecdesp = iteoc.fecdesp
               * vfec = iteoc.fecdesp
            endif    
          ENDIF   
          entro = .t.
       ENDSCAN
       SELECT ORDEN 
       if entro
          if vIg = 0 AND vEs = 0
	         m.ESTADO = iif(m.estado='51','51','40')
          endif       
          gather memvar
       else
          do standby with ' La O/C no se pudo atender '
       endif  
	   IF sistema='1'
		  USE maepre   IN 11  order tag maepre1  ALIAS maepre
	   ELSE
		  USE maepre   IN 11  order tag maepre3  ALIAS maepre
	   ENDIF	
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT ITEOC
 SET ORDE TO 1
 SELECT Orden
 DO PANTALLA
 DO Vista
 RETURN


PROCEDURE INGRESA_HIJO
*---------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°°°Presione ®F10¯ para salir grabando o  ®Esc¯ para cancelar°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
hide popup all
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteOc
SET ORDE TO 1
SET FILTER TO ESTADO='00' OR ESTADO='95'
*SET FILTER TO ESTADO='00' and ESTADO='95'

IF EOF()
   DO STANDBY WITH 'La O/C ya est  atendida'
   SET ORDER TO 1
   do vista
   return
ENDIF
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH KEY m.Periodo + m.NumOc ;
   WINDOW Wind_2 ;
   FIELDS;
   CodUni      : H= 'Uni'      :W=.F. :3,;
   Descri      : H= 'Descripci¢n' :22 :W=.F. ,;
   PreUni      : H= 'PreUni' :P='99,999.99' :W=.F. ,;
   X=ROUND(CanReq*PreUni,5)  :H='Total'  :P='9,999,999.99' :W=.F. ,;
   CanReq      : H= 'Cantd' :P='99,999.99' :W=.F.,;
   CanDesp1    : H= 'Despd' :P='99,999.99' :V=val_can():F,;
   FecDesp     : H= 'Atendido' :W=CANDESP1>0 
 ACTIVATE SCREEN
 &&+ alltrim(m.codfte)
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
*SET ORDE TO 1
SET FILT TO
SELE Orden
ON KEY LABEL F10
RETURN


PROCEDURE val_can
*----------------
private vfun
vfun = .t.
vResta = Canreq-Candesp
if Candesp1 > vResta
   do standby with 'Se esta exediendo en '+alltrim(str(candesp1-vResta,10))
   vfun = .f.
endif
return vfun


PROCEDURE LIQUIDA
*----------------
 SELECT Orden
 vRe=recno()
 SCATTER MEMVAR
 DO CASE
     CASE M.ESTADO='99'
	     DO STANDBY WITH 'La O/C ya est  anulada'
	     do vista
	     return
    CASE M.ESTADO='50'
	     DO STANDBY WITH 'La O/C ya est  liquidada'
	     do vista
	     return
    CASE M.ESTADO='51'
	     DO STANDBY WITH 'La O/C ya est  contabilizada'
	     do vista
	     return
    CASE M.ESTADO='00' 
	     DO STANDBY WITH 'Todav¡a no esta atendida'
	     do vista
	     return	     
    CASE M.ESTADO='20' 
	     DO STANDBY WITH 'Todav¡a no esta despachada'
	     do vista
	     return	     

 endCASE 
 DEACTIVATE WINDOW WIND_3 
 @ 1,60 get m.fecdesp
 read VALID VAL_READ()
 IF LASTKEY()#27
    m.ESTADO = iif(m.estado='51','51','50')
    m.Tipdoc = 'OK'
    gather memvar
 ENDIF   
 DO Vista
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
 ess=vEstOc(iteoc.estado) :5 :H='Estd' ,;
 iteOc.descri :H='Articulo ' :30 ,;
 iteOc.coduni :H='Unid' ,;
 iteOc.Canreq :H='Cantid',;
 iteOc.Candesp :H='Despac'
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


procedure val_fec
*----------------
if empty(fecdesp)
if rlock()
   replace fecdesp with date()
else
  retur .f.   
endif   
endif
retur .t.


PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
IF !EMPTY(m.NUMANU) OR !EMPTY(M.NUMREB)
   @ 0,0 SAY m.Anultot picture '9,999,999.99'
else   
   @ 0,0 SAY m.Valtot picture '9,999,999.99'
endif   
return



PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_oC = SPACE(4)
 *vNum_Oc = 0
 VFTE = SPACE(2)
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/C : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNum_OC PICTURE '!!!!' VALID vBusca()
*@ 1,34 SAY 'FTE' GET VFTE VALID VAL_PARA(VFTE,'CODFTE','C')
*vNum_oC = ALLTRIM(vNum_oC)
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


PROCEDURE Corri
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SELECT Orden
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR
 @ 1,60 get m.fecdesp
 read VALID VAL_READ()
 
 IF LASTKEY() # 27
    SELE ORDEN 
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT Orden
 DO PANTALLA
 DO Vista
 RETURN


function Avalprv
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


PROCEDURE Agreg_Item
*-------------------
vOrd=ORDER()
Sele iteOc
IF F_appd()
  REPLACE NumOc  WITH m.NumOc ,;
          Periodo WITH m.Periodo ,;
          CodArt WITH Itepec.CodArt ,;
          CodCad WITH Itepec.CodCad ,;
          CanReq WITH Itepec.Canreq ,;
          CodUni WITH Itepec.Coduni ,;
          DesCri WITH Itepec.Descri ,;
          Preuni WITH Itepec.Preuni
  RETURN .T.
ENDIF
SET ORDER TO vOrd
RETURN .F.


PROCEDURE Lista
*--------------
SELECT ORDEN
SET RELATION TO PERIODO+NUMOC+CODFTE INTO ITEOC
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
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Ordenes de Compra' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue,vToPrv
vCli = SPAC(4)
vAno = '99'
VFTE = '  '
vcodfte = '  '
vcodPRV = '    '
vfecini = date()
vfecfin = date()

@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ Detallado;Resumido' 

@ 05,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,07,22)  
@ 07,01 SAY "           Fuente : "
@ 07,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 

@ 09,01 SAY "Todos los Proveed : " GET vToprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOprv,11,22) 
@ 11,01 SAY "      Proveedores : "
@ 11,22 GET vCodPrv  PICTURE '!!!!' VALID VALPrv() WHEN vToPRV=2 

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' 

@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados'

@ 20,01 SAY "           Fechas : " GET vFecIni picture '@D'  when vtippro = 6
@ 20,32                            GET vFecFin picture '@D'  when vtippro = 6

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,NumOc,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOc))) TO (vInd) ;
      FOR  IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado = '20',iif(vTipPro=5,Estado = '99',Estado = '50'))))) AND ;
           iif(vtofue =1,.t.,Codfte=alltrim(vCODfte)) 
      SET FILT TO iif(vtoprv =1,.t.,Codprv=alltrim(vCODprv)) AND IIF(vTipPro=6,BETWEEN(FECDESP,VFECINI,VFECFIN),.T.) &&AND orden.tipord#'B'
   SET INDEX TO (VIND)
   COUNT ALL TO vTotoc     
   SUM valtot - anultot TO SUMATOT
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Orden Compra',IIF(vTipPro=2,'Listado Orden de Compra Pendientes',IIF(vTipPro=3,'Listado de Ordenes de Compra Atendidas',IIF(vTipPro=4,'Listado Orden de Compra Afectadas',IIF(vTipPro=4,'Listado Orden de Compra Anuladas','Listado Orden de Compra Liquidadas')))))
   SET MEMOWIDTH TO 43
   IF !EOF()
     SET SKIP TO ITEOC
     	IF VTIPPRO#6
     		do case 
        		case VLISTADO=1
    	 			DO REPORTE WITH 2,"LisOrdc1",' Ordenes de Compra ',1,.F.,.T.
        		case VLISTADO=2    	
    	 			DO REPORTE WITH 2,"LisOrdX1",' Ordenes de Compra ',1,.F.,.T.        
     		ENDcase   
     	ELSE
     		do case 
        	case VLISTADO=1
    			DO REPORTE WITH 2,"LisOrdc",' Ordenes de Compra ',1,.F.,.T.
        	case VLISTADO=2    	
    			DO REPORTE WITH 2,"LisOrdX",' Ordenes de Compra ',1,.F.,.T.        
     		ENDcase   
     	ENDIF
     SELECT ORDEN
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
RETURN


PROCEDURE aLista
*--------------
SELECT ORDEN
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   SET RELATION TO PERIODO+NUMOC INTO ITEOC
   SET SKIP TO ITEOC
            do reporte with 2,"LisOc",' Gu¡as de Internamiento '
   SET RELATION TO
endif
select Orden
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
  ON KEY LABEL F9
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_2
  RELEASE WINDOW wind_3
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


function valprv
*--------------
private xx
vfun = .f.
VCODPRV = iif( empty(VCODPRV),Vcodprv,padl(alltrim(Vcodprv),4,'0'))
xx = val_prv( Vcodprv,.t.)
if xx
   return .t.
endif
return .F.

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
*-----------*--------

PROCEDURE FECREP
*---------------
SET FUNCTION  F3 TO DTOC(FECDESP)
RETURN FECDESP



PROCEDURE Corri1
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SELECT Orden
*SELECT ITEOC
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR
* @ 2,55 get m.CODPART
* @ 2,55 get m.PERHC
* @ 2,60 get m.NUMHC
 @ 5,22 GET M.CODFTE
 read VALID VAL_READ()
 
 IF LASTKEY() # 27
    SELE ORDEN 
*    SELE ITEOC
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 SELE ITEOC
* SELE ITEOC1
 SEEK m.Periodo + m.NumOc 
 SET FILT TO Periodo = m.Periodo and NumOc = m.NumOc    
 GO TOP
 SCAN WHILE Periodo+NumOc = m.Periodo + m.NumOc 
      REPLA ALL codfte WITH m.codfte  
*      REPLA ALL codPART WITH m.codPART
 ENDSCAN


 
 UNLOCK ALL
 SELECT Orden
 
 
 DO PANTALLA
 DO Vista
 RETURN

