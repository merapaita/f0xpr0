*-------------------------------------------------------------------------
* OrdSer.Prg
* Registra el Orden de Servicio
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

PUBLIC vMes,vPart,CON
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
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
 @ 10, 2 SAY "   Calendario O/S :"
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
 on key label F2 do liquidar
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
 @ 13,22 SAY SUBSTR(m.Destino,1,56) 
 @ 13,22 SAY SUBSTR(m.DesOs,1,56)
 @ 14,22 SAY SUBSTR(m.DesOs,57,56)
 @ 15,22 SAY SUBSTR(m.DesOs,113,56)
 @ 16,22 SAY SUBSTR(m.DesOs,169,56)
 @ 17,22 SAY SUBSTR(m.DesOs,225,56)
 @ 18,22 SAY m.ValTot   PICTURE '999,999.99'
 @ 18,60 SAY IIF(alltrim(m.tipfun)="I",m.CodPart,m.codanal)
 @ 20,22 SAY SPACE(40)
 @ 20,22 SAY IIF(EMPTY(m.ConCon),'       ',m.NumCon)
 @ 21,00 SAY PADC(' °  ®F7¯  Seguimiento  °',79,' ') COLOR W+/B
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT OrdSe
set orde to 2
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
 DESOS  :H='Descripci¢n'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
 set orde to 1
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
*@ 1,26 SAY '-' GET vNum_OS PICTURE '9999' VALID vBusca()
 @ 1,26 SAY '-' GET vNumOS PICTURE '!!!!' 
*@ 1,34 SAY 'FTE' GET VFTE VALID VAL_PARA(VFTE,'CODFTE','C')
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


PROCEDURE Corri
*--------------
PRIVATE VNUM
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
  DO CASE  
    CASE Estado = '99'
 		 * Anulada
         DO STANDBY WITH Vmens09
         RETURN
	CASE Estado = '20'
         * El Orden de Compra ha sido devuelto
         DO STANDBY WITH 'El O/S ya esta Afectado'
         RETURN
	CASE Estado = '50'
         * El Orden de S ya Liquida ?
         DO STANDBY WITH 'El O/S ya est  liquidado'
         RETURN
	CASE Estado = '51'
         * El Orden de Compra ya tiene ?
         DO STANDBY WITH 'El O/S ya est  Contabilizado'
         RETURN
 ENDCASE

 ************
 SELECT OrdSe
 SCATTER MEMVAR
 VNUM = RECNO()
 ACTIVATE WINDOW WINDO_0
 DO PANTALLA
 vSino = 'N'
 
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyec  = substr(m.codcal,13,3) 
 vCodact  = substr(m.codcal,13,2) 
 vSubpry  = substr(m.codcal,16,4)
 vCodpart = m.codpart
 vcodanal = m.codanal

 vDesos1  = substr(desos,1,56)
 vDesos2  = substr(desos,57,56)
 vDesos3  = substr(desos,113,56)
 vDesos4  = substr(desos,169,56)
 vDesos5  = substr(desos,225,56)

 xDesos1  = substr(desos1,1,56)
 xDesos2  = substr(desos1,57,56)
 xDesos3  = substr(desos1,113,56)
 xDesos4  = substr(desos1,169,56)
 xDesos5  = substr(desos1,225,56)

 yDesos1  = substr(desos2,1,56)
 yDesos2  = substr(desos2,57,56)
 yDesos3  = substr(desos2,113,56)
 yDesos4  = substr(desos2,169,56)
 yDesos5  = substr(desos2,225,56)

 zDesos1  = substr(desos3,1,56)
 zDesos2  = substr(desos3,57,56)
 zDesos3  = substr(desos3,113,56)
 zDesos4  = substr(desos3,169,56)
 zDesos5  = substr(desos3,225,56)

*vDesos6  = substr(desaux,1,56)
*vDesos7  = substr(desaux,57,56)
*vDesos8  = substr(desaux,113,56)
 
 SELECT ORDSE
 IF RLOCK() OR F_LOCK(1)
    @ 1,22 GET m.Periodo  PICTURE '!!' DISABLE
    @ 1,24 SAY '/'
    @ 1,25 GET m.NumOs   PICTURE '!!!!' &&DISABLE
    @ 1,63 GET m.Numss   PICTURE '!!!!' DISABLE &&VALID VAL_FUN("SOLSER","NUMSS","NUMSS+' '+DESSS",m.NUMSS,1)  &&,2,28)
    @ 2,22 GET m.FecOs   PICTURE '@D'

    @ 4,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)
    @ 6,22 GET m.CodFte  PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,20)
    @ 6,60 GET m.TipFun  PICTURE '!'   VALID val_para(m.Tipfun,'TIPFUN',' ',60,15,2)
    @ 7,22 GET vCodPrg   PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,20)
 	@ 8,22 GET vCodSub   PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
	@ 9,22 GET vProyec   PICTURE '!!!' VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
	@ 9,22 GET vCodAct   PICTURE '!!!' VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
 	@ 9,26 GET vSubPry   PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
    @ 10,22 GET m.Nummes PICTURE '!!' VALID !EMPTY(m.Nummes) and analcal(0,2) AND val_para(m.NumMes,'FECMES',' ',22,15)  &&VALMES()
    @ 11,22 GET m.CodDep PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,30,7)
   *@ 13,22 get m.Desos PICTURE '@S56'
    @ 13,22 get vDesos1 PICTURE '@S56'   
    @ 14,22 get vDesos2 PICTURE '@S56'   
    @ 15,22 get vDesos3 PICTURE '@S56'   
    @ 16,22 get vDesos4 PICTURE '@S56'   
    @ 17,22 get vDesos5 PICTURE '@S56' valid cont()
    
    @ 13,22 get xDesos1 PICTURE '@S56' when con
    @ 14,22 get xDesos2 PICTURE '@S56' when con  
    @ 15,22 get xDesos3 PICTURE '@S56' when con  
    @ 16,22 get xDesos4 PICTURE '@S56' when con  
    @ 17,22 get xDesos5 PICTURE '@S56' when con  valid conx()
 
    @ 13,22 get yDesos1 PICTURE '@S56' when con
    @ 14,22 get yDesos2 PICTURE '@S56' when con  
    @ 15,22 get yDesos3 PICTURE '@S56' when con  
    @ 16,22 get yDesos4 PICTURE '@S56' when con  
    @ 17,22 get yDesos5 PICTURE '@S56' when con  valid cony()

    @ 13,22 get zDesos1 PICTURE '@S56' when con
    @ 14,22 get zDesos2 PICTURE '@S56' when con  
    @ 15,22 get zDesos3 PICTURE '@S56' when con  
    @ 16,22 get zDesos4 PICTURE '@S56' when con  
    @ 17,22 get zDesos5 PICTURE '@S56' when con  
 
  
    @ 18,22 GET m.ValTot  PICTURE '999,999.99'
    IF alltrim(m.tipfun)="I"
       @ 18,60 GET m.CodPart PICTURE '!!.!!' VALID IIF(!EMPTY(m.Codpart),VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),'L'),.t.)
    ELSE   
       @ 18,60 GET m.codanal PICTURE '!!.!!' VALID IIF(!EMPTY(m.codanal),VAL_PART(SUBSTR(m.codanal,4,2),LEFT(m.codanal,2),'L'),.t.)
    ENDIF   
    @ 20,22 get vSino PICTURE '!' VALID  vSino $'NS'
    @ 20,24 get m.NumCon  WHEN vSino='S'
    
    READ VALID VAL_READ()  
    SELE ORDSE
    vImpr = m.Periodo+m.NumOs+ALLTRIM(m.Codfte)
    IF LASTKEY() # 27
       m.codcal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
       m.DesOs = vDesos1+vDesos2+vDesos3+vDesos4+vDesos5
       m.DesOs1 = xDesos1+xDesos2+xDesos3+xDesos4+xDesos5       
       m.DesOs2 = yDesos1+yDesos2+yDesos3+yDesos4+yDesos5       
       m.DesOs3 = zDesos1+zDesos2+zDesos3+zDesos4+zDesos5       
       IF vSino='N'
          m.NumCon = space(10)
       ELSE
          SELECT PARMA
          SEEK 'CORREL'+'NUMCON'
          REPLACE NumEnt WITH NumEnt + 1
       ENDIF
       DO ITESER
       SELECT Ordse
       GO VNUM
       GATHER MEMVAR
    ELSE
      DO STANDBY WITH 'Proceso cancelado'
      SELE ORDSE
      GO TOP 
      GO VNUM
    ENDIF
  ENDIF
  UNLOCK ALL
  SELECT Ordse
  DO Vista  
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 sincot=.t.
 IF escolor
    DEFINE POPUP xcot  FROM 17,55 SHADOW COLOR &L_COL
 ELSE
    DEFINE POPUP xcot  FROM 17,55 COLOR SCHEME c_popup
 ENDIF
 DEFINE BAR 1 OF xcot PROMPT ' \<Sin Cotizaci¢n '
 DEFINE BAR 2 OF xcot PROMPT ' \<Con Cotizaci¢n '
 DEFINE BAR 3 OF xcot PROMPT ' c\<On otro docmto'
 ON SELECTION POPUP xcot  DEACTIVATE POPUP
 ACTIVATE POPUP XCOT
 DO CASE
    CASE BAR() = 1
      sincot=.t.
      conmem=.f.
    CASE BAR() = 2
      sincot=.f.
      conmem=.f.
    CASE BAR() = 3
      sincot=.f.
      conmem=.t.
    OTHERWISE
 ENDCASE
 RELEASE POPUP Xcot
IF LASTKEY()=27
   DO VISTA
   RETURN
ENDIF
   
 DO PANTALLA
 SELECT OrdSe
 SCATTER MEMVAR BLANK
 ASD = RECNO()
 m.FecOs  = date()
 m.periodo = str(year(DATE())-1900,2)
 vNumss = '    '
 vAno = '  '
 vSino = 'N'
 
 vNumCon = '     '
 vCodPrg  = space(2)  
 vCodSub  = space(3)    
 vProyec  = space(3) 
 vSubpry  = space(4)
 vCodAct  = space(3)
 vDesOs1  = substr(m.desos,  1,56)
 vDesOs2  = substr(m.desos, 57,56)
 vDesOs3  = substr(m.desos,113,56)
 vDesOs4  = substr(m.desos,169,56)
 vDesOs5  = substr(m.desos,225,56)

 xDesOs1  = substr(m.desos1,  1,56)
 xDesOs2  = substr(m.desos1, 57,56)
 xDesOs3  = substr(m.desos1,113,56)
 xDesOs4  = substr(m.desos1,169,56)
 xDesOs5  = substr(m.desos1,225,56)

 yDesOs1  = substr(m.desos2,  1,56)
 yDesOs2  = substr(m.desos2, 57,56)
 yDesOs3  = substr(m.desos2,113,56)
 yDesOs4  = substr(m.desos2,169,56)
 yDesOs5  = substr(m.desos2,225,56)

 zDesOs1  = substr(m.desos3,  1,56)
 zDesOs2  = substr(m.desos3, 57,56)
 zDesOs3  = substr(m.desos3,113,56)
 zDesOs4  = substr(m.desos3,169,56)
 zDesOs5  = substr(m.desos3,225,56)
 
 SET CONFIRM OFF
 @  1,22 GET m.Periodo   PICTURE '!!' valid v_numos()
 @  1,24 SAY '-'
 @  1,25 GET m.NumOs     PICTURE '!!!!'  valid validar()
 IF CONMEM
   @ 1,40 SAY " Documento Fuente :" get m.Memoran PICTURE '@S18'
 ELSE	
   @ 1,40 SAY "       N£mero S/S :" get vano
   @ 1,62 SAY '.'
   @ 1,63 GET vNumss PICTURE '!!!!' VALID V_SER()
 ENDIF 
 @  2,22 GET m.FecOs     PICTURE '@D'
 
 @  4,22 GET m.CodPrv    PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)
 
 @  6,22 GET m.CodFte    PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,20)
 @  6,60 GET m.TipFun    PICTURE '!'   VALID val_para(m.Tipfun,'TIPFUN',' ',60,15,2) AND asig_py()
 
 @  7,22 GET vCodPrg     PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40)
 @  8,22 GET vCodSub     PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
 
 @  9,22 GET vProyec     PICTURE '!!!' VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  9,22 GET vCodAct     PICTURE '!!!' VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
 
 @  9,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @ 10,22 GET m.Nummes    PICTURE '!!' VALID !EMPTY(m.Nummes) and analcal(0,2) AND val_para(m.NumMes,'FECMES',' ',22,15)   &&VALMES()
 
 @ 11,22 GET m.CodDep  PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,30,7)
*@ 13,22 GET m.DesOs   PICTURE '@S56'
 @ 13,22 get vDesos1   PICTURE '@S56'
 @ 14,22 get vDesos2   PICTURE '@S56'
 @ 15,22 get vDesos3   PICTURE '@S56'
 @ 16,22 get vDesos4   PICTURE '@S56'
 @ 17,22 get vDesos5   PICTURE '@S56' VALID CONT()
 
 @ 13,22 get xDesos1 PICTURE '@S56' when con
 @ 14,22 get xDesos2 PICTURE '@S56' when con  
 @ 15,22 get xDesos3 PICTURE '@S56' when con  
 @ 16,22 get xDesos4 PICTURE '@S56' when con  
 @ 17,22 get xDesos5 PICTURE '@S56' when con  valid conx()
 
 @ 13,22 get yDesos1 PICTURE '@S56' when con
 @ 14,22 get yDesos2 PICTURE '@S56' when con  
 @ 15,22 get yDesos3 PICTURE '@S56' when con  
 @ 16,22 get yDesos4 PICTURE '@S56' when con  
 @ 17,22 get yDesos5 PICTURE '@S56' when con  valid cony()

 @ 13,22 get zDesos1 PICTURE '@S56' when con
 @ 14,22 get zDesos2 PICTURE '@S56' when con  
 @ 15,22 get zDesos3 PICTURE '@S56' when con  
 @ 16,22 get zDesos4 PICTURE '@S56' when con  
 @ 17,22 get zDesos5 PICTURE '@S56' when con  
 
 
 @ 18,22 GET m.ValTot   PICTURE '999,999.99'
 IF alltrim(m.tipfun)="I"
    @ 18,60 GET m.CodPart PICTURE '!!.!!' &&VALID IIF(!EMPTY(m.Codpart),VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),'L'),.t.)
 ELSE   
    @ 18,60 GET m.codanal PICTURE '!!.!!' &&VALID IIF(!EMPTY(m.codanal),VAL_PART(SUBSTR(m.codanal,4,2),LEFT(m.codanal,2),'L'),.t.)
 ENDIF   
 @ 20,22 get vSino PICTURE '!' VALID vSino $'NS'
 @ 20,24 get vNumCon WHEN vSino='S'
 
 READ VALID VAL_READ()  
 SET CONFIRM ON
 IF LASTKEY() # 27
       m.fecemi=date()
       m.codcal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
       m.Concon = vsino
       m.NumCon = iif(vsino='N','     ',vNumCon)
       m.Estado = '00'
       vImpr = m.Periodo+m.NumOs+ALLTRIM(m.Codfte)
       m.DesOs = vDesos1+vDesos2+vDesos3+vDesos4+vDesos5
       m.DesOs1 = xDesos1+xDesos2+xDesos3+xDesos4+xDesos5       
       m.DesOs2 = yDesos1+yDesos2+yDesos3+yDesos4+yDesos5       
       m.DesOs3 = zDesos1+zDesos2+zDesos3+zDesos4+zDesos5       
       DO ITESER
       SELECT ORDSE
       vSS = m.Periodo+m.Numss
       IF F_Appd()
	       GATHER MEMVAR
	   endif    
       *- Aumento el correlativo
       SELECT Parma
       SEEK 'CORRELORDENS'
       REPLACE NumEnt WITH NumEnt + 1
       IF vSino='S'
          SEEK 'CORREL'+'NUMCON'
          REPLACE NumEnt WITH NumEnt + 1
       ENDIF
       SELECT SOLSER
       seek vSS
       if found()
	       IF RLOCK()
    	      REPLACE ESTADO WITH '30',NUMOS WITH ORDSE.NUMOS,PEROS WITH ORDSE.PERIODO
	       ENDIF
    	   unlock
       endif
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
   SELE ORDSE
   GO ASD
ENDIF
UNLOCK ALL
SELECT OrdSe
DO Vista
RETURN

procedure v_numos
*----------------
 SELECT Parma
 SEEK 'CORRELORDENS'
 = valOs ( Parma.NumEnt + 1 )
 SELECT ORDSE
 return


PROCEDURE xIngre              && Crea nuevo registro en BD
*--------------
 sincot=.t.
 IF escolor
    DEFINE POPUP xcot  FROM 17,55 SHADOW COLOR &L_COL
 ELSE
    DEFINE POPUP xcot  FROM 17,55 COLOR SCHEME c_popup
 ENDIF
 DEFINE BAR 1 OF xcot PROMPT ' \<Sin Cotizaci¢n '
 DEFINE BAR 2 OF xcot PROMPT ' \<Con Cotizaci¢n '
 DEFINE BAR 3 OF xcot PROMPT ' c\<On otro docmto'
 ON SELECTION POPUP xcot  DEACTIVATE POPUP
 ACTIVATE POPUP XCOT
 DO CASE
    CASE BAR() = 1
      sincot=.t.
      conmem=.f.
    CASE BAR() = 2
      sincot=.f.
      conmem=.f.
    CASE BAR() = 3
      sincot=.f.
      conmem=.t.
    OTHERWISE
 ENDCASE
 RELEASE POPUP Xcot
IF LASTKEY()=27
   DO VISTA
   RETURN
ENDIF
   
 DO PANTALLA
 SELECT OrdSe
 SCATTER MEMVAR BLANK

 m.FecOs  = date()
 m.periodo = str(year(DATE())-1900,2)
 vNumss = '    '
 vAno = '95'
 vSino = 'N'
 @  1,22 GET m.Periodo   PICTURE '!!' DISABLE
 @  1,24 SAY '-'
 @  1,25 GET m.NumOs     PICTURE '!!!!'  DISABLE
 IF CONMEM
   @ 1,40 SAY " Documento Fuente :" get m.Memoran PICTURE '@S18'
 ELSE	
   @ 1,40 SAY "       N£mero S/S :" get vano
   @ 1,62 SAY '.'
   @ 1,63 GET vNumss PICTURE '!!!!' VALID V_SER()
 ENDIF 
 @ 6,22 GET m.CodFte    PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,20)
 READ
 IF LASTKEY()=27
    DO VISTA
    RETURN
 ENDIF
 IF !CONMEM
 	IF SOLSER.NUMOS#'    ' &&SOLSER.ESTADO='30'
       DO STANDBY WITH 'La Solicitud de Servicio ya tiene ORDEN '
       DO VISTA
       return
    endif
 ENDIF
 vNumCon = '     '
 vCodPrg  = space(2)  
 vCodSub  = space(3)    
 vProyec  = space(3) 
 vSubpry  = space(4)
 vCodAct  = space(3)
 
 vDesOs1  = substr(m.desos,  1,56)
 vDesOs2  = substr(m.desos, 57,56)
 vDesOs3  = substr(m.desos,113,56)
 vDesOs4  = substr(m.desos,169,56)
 vDesOs5  = substr(m.desos,225,56)

 xDesOs1  = substr(m.desos1,  1,56)
 xDesOs2  = substr(m.desos1, 57,56)
 xDesOs3  = substr(m.desos1,113,56)
 xDesOs4  = substr(m.desos1,169,56)
 xDesOs5  = substr(m.desos1,225,56)

 yDesOs1  = substr(m.desos2,  1,56)
 yDesOs2  = substr(m.desos2, 57,56)
 yDesOs3  = substr(m.desos2,113,56)
 yDesOs4  = substr(m.desos2,169,56)
 yDesOs5  = substr(m.desos2,225,56)

 zDesOs1  = substr(m.desos3,  1,56)
 zDesOs2  = substr(m.desos3, 57,56)
 zDesOs3  = substr(m.desos3,113,56)
 zDesOs4  = substr(m.desos3,169,56)
 zDesOs5  = substr(m.desos3,225,56)


 SELECT Parma
 SEEK 'CORREL'+iif(alltrim(m.CodFte)='TRN','SERTRN','SERPRP')
 = valOs ( Parma.NumEnt + 1 )
 SELECT ORDSE
 SET CONFIRM OFF
 @  1,22 GET m.Periodo   PICTURE '!!-'
 @  1,25 GET m.NumOs     PICTURE '!!!!' VALID VALIDAR()
 IF CONMEM
    @  1,60 GET m.Memoran picture '@S18' disable
 ELSE
	@  1,60 GET vAno disable
 	@  1,63 GET m.Numss     PICTURE '!!!!' DISABLE
 ENDIF
 @  2,22 GET m.FecOs     PICTURE '@D'
 @  4,22 GET m.CodPrv    PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)
 @  6,60 GET m.TipFun    PICTURE '!'   VALID val_para(m.Tipfun,'TIPFUN',' ',60,15,2) AND asig_py()
 
 @  7,22 GET vCodPrg     PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40)
 @  8,22 GET vCodSub     PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
 
 @  9,22 GET vProyec     PICTURE '!!!' VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
 @  9,22 GET vCodAct     PICTURE '!!!' VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
 
 @  9,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
 @ 10,22 GET m.Nummes    PICTURE '!!' VALID !EMPTY(m.Nummes) and analcal(0,2) AND val_para(m.NumMes,'FECMES',' ',22,15)   &&VALMES()
 
 @ 11,22 GET m.CodDep  PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,30,7)
*@ 13,22 GET m.DesOs   PICTURE '@S56'
 @ 13,22 get vDesos1   PICTURE '@S56'
 @ 14,22 get vDesos2   PICTURE '@S56'
 @ 15,22 get vDesos3   PICTURE '@S56'
 @ 16,22 get vDesos4   PICTURE '@S56'
 @ 17,22 get vDesos5   PICTURE '@S56' VALID CONT()
 
 @ 13,22 get xDesos1 PICTURE '@S56' when con
 @ 14,22 get xDesos2 PICTURE '@S56' when con  
 @ 15,22 get xDesos3 PICTURE '@S56' when con  
 @ 16,22 get xDesos4 PICTURE '@S56' when con  
 @ 17,22 get xDesos5 PICTURE '@S56' when con  valid conx()
 
 @ 13,22 get yDesos1 PICTURE '@S56' when con
 @ 14,22 get yDesos2 PICTURE '@S56' when con  
 @ 15,22 get yDesos3 PICTURE '@S56' when con  
 @ 16,22 get yDesos4 PICTURE '@S56' when con  
 @ 17,22 get yDesos5 PICTURE '@S56' when con  valid cony()

 @ 13,22 get zDesos1 PICTURE '@S56' when con
 @ 14,22 get zDesos2 PICTURE '@S56' when con  
 @ 15,22 get zDesos3 PICTURE '@S56' when con  
 @ 16,22 get zDesos4 PICTURE '@S56' when con  
 @ 17,22 get zDesos5 PICTURE '@S56' when con  
 
 
 @ 18,22 GET m.ValTot   PICTURE '999,999.99'
 @ 20,22 get vSino PICTURE '!' VALID vSino $'NS'
 @ 20,24 get vNumCon WHEN vSino='S'
 
 READ VALID VAL_READ()  
 SET CONFIRM ON
 IF LASTKEY() # 27
       m.fecemi=date()
       m.codcal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
       m.Concon = vsino
       m.NumCon = iif(vsino='N','     ',vNumCon)
       m.Estado = '00'
       vImpr = m.Periodo+m.NumOs+ALLTRIM(m.Codfte)
       m.DesOs = vDesos1+vDesos2+vDesos3+vDesos4+vDesos5
       m.DesOs1 = xDesos1+xDesos2+xDesos3+xDesos4+xDesos5       
       m.DesOs2 = yDesos1+yDesos2+yDesos3+yDesos4+yDesos5       
       m.DesOs3 = zDesos1+zDesos2+zDesos3+zDesos4+zDesos5       
       DO ITESER
       SELECT ORDSE
       vSS = m.Periodo+m.Numss
       IF F_Appd()
	       GATHER MEMVAR
	   endif    
       *- Aumento el correlativo
       SELECT Parma
       SEEK 'CORREL'+iif(alltrim(m.CodFte)='TRN','SERTRN','SERPRP')
       REPLACE NumEnt WITH NumEnt + 1
       IF vSino='S'
          SEEK 'CORREL'+'NUMCON'
          REPLACE NumEnt WITH NumEnt + 1
       ENDIF
       SELECT SOLSER
       seek vSS
       if found()
	       IF RLOCK()
    	      REPLACE ESTADO WITH '30',NUMOS WITH ORDSE.NUMOS,PEROS WITH ORDSE.PERIODO
	       ENDIF
    	   unlock
       endif
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT OrdSe
 DO Vista
 IF YESNO('¨Imprime el Documento?')
    SEEK vImpr  
    SET FILTER TO Periodo+NumOs+Codfte = ALLTRIM(vImpr)
    IF !EOF()
       DO REPORTE WITH 2,"LisOs1",' Ordenes de Servicio',2
    ENDIF   
    SET FILTER TO 
    GO TOP
    SEEK vImpr
    do vista
 ENDIF   

RETURN


*@  2,22 GET m.Numss     PICTURE '!!!!' VALID VAL_FUN("SOLSER","NUMSS","NUMSS+' '+DESSS",m.NUMSS,1)  &&,2,28)

PROCEDURE Xstado
*-----------------
ON KEY LABEL F7
DEFINE WINDOW Estado FROM 5,3 TO 9,77 TITLE '®ESC¯ Salir    °°° Estado de O/S °°°               ' COLOR SCHEME 10
IF WVISIBLE("Estado")
 *  ACTIVATE WINDOW Estado SAME
   RETURN
*ELSE
 *  ACTIVATE WINDOW Estado NOSHOW
ENDIF
USE HOJCON   in 0  order tag hojcon1  ALIAS hoja
Select Hoja
seek m.PerHc+m.numhc
m.NumCp  = Hoja.NumCp
m.MesCp  = Hoja.NumMesCp    
m.Cta    = Hoja.CodCtc
USE
USE COMPAG   in 0  order tag compag1  ALIAS Compag
Select compag
seek m.MesCp+m.NumCp+m.Cta
m.NumChq = ComPag.NumChq    
m.Import = Compag.Import
m.reten  = Compag.reten
USE
SELECT OrdSE
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOS+alltrim(m.codfte);
   WINDOW Estado ;
   FIELDS;
   p   = IIF(EMPTY(Numhc),'No afectado',Numhc+'/'+perhc)	 :H='   H/C ':11,; 
   cp  = IIF(EMPTY(m.NumCp),'Sin pago',m.NumCp+'/'+m.MESCp)  :H='    C/P ':8,;
   cheq= IIF(EMPTY(m.NumChq),'No girado',m.NumChq)		     :H='  Cheq.':9,;   
   imp = IIF(EMPTY(m.NumCp),'    ---.--',m.import)			 :H='  Total':12,;
   ret = IIF(EMPTY(m.NumCp),'    ---.--',m.reten)		     :H=' Retenido':12,;
   tp  = IIF(EMPTY(m.NumCp),'    ---.--',m.import-m.reten)    :H='Total Pagado':12

DEACTIVATE WINDOW Estado
ON KEY LABEL F7 DO Estado WITH 'OS','m.perhc+m.numhc'
RETURN

PROCEDURE VALMES
*---------------
IF alltrim(m.Nummes)#vmes
   do standby WITH 'No Coincide con Mes del Calendario'
endif   
return

PROCEDURE ASIG_PY
*----------------
vProyec  = space(3) 
vSubpry  = space(4)
*CodObr  = space(2)
vCodAct  = space(3)
RETURN .T.


PROCEDURE V_SER
*--------------
 AS=ALIAS()
 SELECT SOLSER
 Vtemp = RECNO()
 vNumSs=Padl(alltrim(vNumSs),4,'0')
 seek vAno+vNumss
 if !found()
 HIDE MENU mMenu
 ACTIVATE SCREEN
  vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
  DO Logos WITH Rotulo1,vTempo
  ON KEY LABEL F10 KEYBOARD CHR(23)
  IF SINCOT
  * SET FILTER TO ESTADO='00'AND ESTADO#'99'
    SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99'
  ELSE
    SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99'
  ENDIF
  GO TOP
  IF !EOF()
     BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
     NumSs  :H=' N§ ',;
     Periodo :H='A¤',;
     Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
     FecSs  :H='Fecha' ,;
     CodDep :H='DEP',;
     CodFte :H='FTE',;
     DESSS  :H='Descripci¢n'
*     Codcal :H='Calendario'
     vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO Logos WITH Rotulo1,vTempo
     IF LASTKEY()=27
        GOTO Vtemp
     ENDIF
  ELSE
     DO STANDBY WITH 'No existe Art¡culos a Procesar '
     RETURN .F.
  ENDIF
  SHOW MENU mMenu
  SET FILTER TO
  ON KEY LABEL F10
 endif
 
 IF !CONMEM
 	IF SOLSER.NUMOS#'    ' &&SOLSER.ESTADO='30'
       DO STANDBY WITH 'La Solicitud de Servicio ya tiene ORDEN '
       DO VISTA
       return
    endif
 ENDIF

 vAno = solser.Periodo
 vNumss = solser.Numss
 *----
 m.TipFun = solser.TipFun
 m.Numss  = solser.Numss
 m.CodPrv = solser.codprv
 m.Codcal = solser.Codcal
 m.CodFte = solser.Codfte
 m.CodDep = solser.CodDep
 m.DesOs  = solser.Desss
 m.DesOs1 = solser.Desss1
 m.DesOs2 = solser.Desss2
 m.DesOs3 = solser.Desss3
 m.ValTot = solser.ValTot
 m.Destino = solser.destino
 m.CodObra = solser.codobra
 m.tipObr  = solser.tipObr
 *-----------
  vDesOs1  = substr(m.desos,  1,56)
 vDesOs2  = substr(m.desos, 57,56)
 vDesOs3  = substr(m.desos,113,56)
 vDesOs4  = substr(m.desos,169,56)
 vDesOs5  = substr(m.desos,225,56)

 xDesOs1  = substr(m.desos1,  1,56)
 xDesOs2  = substr(m.desos1, 57,56)
 xDesOs3  = substr(m.desos1,113,56)
 xDesOs4  = substr(m.desos1,169,56)
 xDesOs5  = substr(m.desos1,225,56)

 yDesOs1  = substr(m.desos2,  1,56)
 yDesOs2  = substr(m.desos2, 57,56)
 yDesOs3  = substr(m.desos2,113,56)
 yDesOs4  = substr(m.desos2,169,56)
 yDesOs5  = substr(m.desos2,225,56)

 zDesOs1  = substr(m.desos3,  1,56)
 zDesOs2  = substr(m.desos3, 57,56)
 zDesOs3  = substr(m.desos3,113,56)
 zDesOs4  = substr(m.desos3,169,56)
 zDesOs5  = substr(m.desos3,225,56)

*-------------
 SELE (AS)
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

PROCEDURE Anula
*--------------
 SELE ordSE
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚sta Orden de Servicio ?')
 IF vElimina
    SELECT SOLSER
    SEEK m.periodo+m.numss
    IF Found() and RLOCK() 
       REPLACE SOLSER.ESTADO WITH '00',NUmOS with '    ',PEROS WITH '  '
    ENDIF
    SELE CALEN 
    REPLA ALL TOTOS WITH 0
    SELE ORDSE
    IF  RLOCK()
       REPLACE ORDSE.ESTADO WITH '99' ,FECVER WITH DATE()
    ENDIF
    DO Vista
 ENDIF
 UNLOCK ALL
 RETURN


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
  *do reporte with 2,"LisOs",' Ordenes de Servicios '
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
set filt to Nummes+NumOs=vNumos
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"LisOs1",' Orden de Servicios ',2
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
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue
vCli = SPAC(4)
vAno = '95'
VFTE = '   '
vcodfte = '   '
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ por Documento;en Detalle;Resumido' 

@ 05,01 SAY "        Total O/S : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) when vlistado =1
@ 07,01 SAY "              O/S : "
@ 07,22 GET vFte    PICTURE '!!!' VALID VAL_PARA(VFTE,'CODFTE','C') WHEN VTOCLI=2 AND VLISTADO=1
@ 07,26 GET vAno    picture '!!'  WHEN VTOCLI=2 and vlistado=1
@ 07,28 SAY '-'
@ 07,29 GET vCli    WHEN VTOCLI=2 and vlistado=1 PICTURE '!!!!' VALID VO() AND ValOrd()

@ 09,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,11,22)  WHEN (VLISTADO=2 OR VLISTADO=3)
@ 11,01 SAY "           Fuente : "
@ 11,22 GET vCodFte  PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 AND (VLISTADO=2 OR VLISTADO=3)

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' WHEN vtocli=1 AND (VLISTADO=2 OR VLISTADO=3)

@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados' WHEN vtocli=1 and (vlistado=2 OR  vlistado=3)

@ 20,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON  IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
         FOR iif(vtofue=1,.t.,Codfte=alltrim(vCodfte)) .and. ;
	         IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado='00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado='20',iif(vTipPro=5,Estado='99',Estado='50'))))) 
   SET FILTER TO IIF(vToCli =1,.T.,Periodo+NumOs+Codfte = vAno+vCli+ALLTRIM(vFte)) 
   SET INDEX TO (VIND)
   COUNT ALL TO vTotoS
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Orden Servicio',IIF(vTipPro=2,'Listado Orden de Servicio Pendientes',IIF(vTipPro=3,'Listado Orden de Servicios Afectados',IIF(vTipPro=4,'Listado Orden de Servicios Anulados','Listado Orden de Servicios Liquidados'))))   
   IF !EOF()
     do case 
        case VLISTADO=1
	        DO REPORTE WITH 2,"LisOs1",' Ordenes de Servicios ',2
        case VLISTADO=2
    	 	DO REPORTE WITH 2,"LisOrds",' Ordenes de Servicios ',1,.F.,.T.
        case VLISTADO=3     	
    	 	DO REPORTE WITH 2,"LisOrsX",' Ordenes de Servicios ',1,.F.,.T.        
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
 CodDep :H='DEP',;
 CodFTE :H='FTE ' ,;
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

PROCEDURE Validar
*----------------
AS=RECNO()
seek m.Periodo+m.NumOs
if found()
   do standby with 'El O/S ya esta registrado'
   GOTO AS
   return .f.
endif
IF !EOF()
   GOTO AS
ENDIF   
return .t.

PROCEDURE LIQUIDAR
*-----------------
PRIVATE OK
OK=VE_PASSW('LABEL')
IF OK
DO CASE
   CASE ESTADO = '00'
        DO STANDBY WITH 'El Documento a£n no est  afectado'
        do vista
        return
   CASE ESTADO = '50'
        DO STANDBY WITH 'El Documento ya est  liquidado'
        do vista
        return
   CASE ESTADO = '99'
        DO STANDBY WITH 'El Documento ya est  Anulado'
        do vista
        return
   CASE ESTADO = '20'
        DO STANDBY WITH 'El Documento ya est  Afectado'
ENDCA            
IF YESNO('Est  seguro de Liquidar este O/S')
   vfecliq=date()
   activate window standby
   @ 1,1 say 'Fecha Liquidaci¢n : ' get vfecliq 
   read
   DEactivate window standby
   IF RLOCK()
      REPLACE ESTADO WITH '50',fecliq with vfecliq,TIPDOC WITH 'OK'
   ENDIF 
else 
   DO STANDBY WITH 'Proceso Cancelado'
endif
do vista
ENDIF
retuRN

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


procedure iteSER
*---------------
PRIVATE AZ
aZ=alias()
select SOLSER
seek ORDSE.periodo+ORDSE.numSS
m.destino = IIF(EMPTY(m.destino),SOLSER.DESTINO,m.destino)
m.codobra = solser.codobra
m.tipobr  = solser.tipobr
sele (aZ)
ACTIVATE WINDOW wind_1
@ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
READ
DEACTIVATE WINDOW wind_1
retuR



