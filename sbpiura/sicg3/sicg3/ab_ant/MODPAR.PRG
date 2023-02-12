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
   USE PteAnu   IN 1   order tag PteAnu1  ALIAS AnuPA
   USE OrdCom   IN 2   order tag OrdCom1  ALIAS Orden
   USE IteOc    IN 3   order tag IteOc1   ALIAS IteOc
else
   USE PteAnu   IN 1   order tag PteAnu2  ALIAS AnuPA
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

*- Mensajes de aviso al usuario

if vOpcion = 1
 Vmens01 = ' Parte Modificaci¢n Orden de Compra : REVISION '
 Vmens02 = ' Registro de Parte Modificaci¢n Ordenes de Compra '
 Vmens04 = 'Dicho Orden de Compra no fue encontrado'
 Vmens05 = 'No existe Orden de Compra anterior'
 Vmens06 = 'No existe Orden de Compra siguiente'
 Vmens07 = '¨ Desea ModificaR ‚ste Orden de Compra ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Este Orden de Compra ha sido Modificado'
 Vmens10 = 'Este Orden de Compra ya fue atendido'
 Vmens11 = 'Este Orden de Compra ha sido devuelto'
else
 Vmens01 = ' Parte Modificaci¢n Orden de Servicio: REVISION '
 Vmens02 = ' Registro de Parte Modificaci¢n Ordenes de Servicio '
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
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
ACTIVATE WINDOW Wind_0
CLEAR
 @  1, 2 SAY "       N£mero P/A :"
 @  2, 2 SAY "        Fecha P/A :"
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
 SELECT AnuPA
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 SCATTER MEMVAR
 @  0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','Con Cotiz',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='50','Atendido ','         '))))

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
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
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


PROCEDURE Corri
*--------------
 DO PANTALLA
 SELECT AnuPA
 SCATTER MEMVAR
 vCodFte  = substr(m.codcal,05,3)
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyecto= substr(m.codcal,13,7)
 
 @  1,22 GET m.NumPa   DISABLE
 @  2,22 GET m.FecPa   

 @  1,60 GET m.Periodo DISABLE
 @  1,63 GET m.Codfte  DISABLE
 @  1,67 GET m.NumRef  DISABLE

 @  2,60 GET m.FecRef  DISABLE

 @  4,22 SAY val_prv(m.Codprv)
 @  6,22 SAY m.ValTot  PICTURE '999,999.99' && valid verfi_s()
 @  8,22 SAY VAL_PARA(vCodFte,'CODFTE','V',22,20)
 @  8,60 SAY VAL_PARA(M.tipfun,'TIPFUN','D',60,15,2)

 @  9,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,40)
 @ 10,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @ 11,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
 
 IF alltrim(m.Tipfun)='I' 
    @ 11,25 SAY '.'
	@ 11,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
 else
    @ 11,22 SAY Spac(60)	
 endif	
 
 @ 13,22 GET m.Motivo PICTURE '@S56'
 
 @ 15,22 SAY m.NumHC 
 @ 15,26 SAY '.'
 @ 15,27 SAY m.PerHC  
 
 @ 16,22 SAY m.NumCP  
 @ 16,26 SAY '.'
 @ 16,27 SAY m.PerCp  
 
 @ 17,22 SAY m.NumChq 

 @ 19,22 GET m.Observa
*------

 READ VALID Val_Read()

 IF LASTKEY() # 27
    SELECT AnuPA
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 UNLOCK ALL
 DO PANTALLA
 DO Vista
 RETURN

procedure verfi_s
*----------------
if m.Valtot > vMax
   do standby with 'Se est  exediendo en '+str(m.Valtot - vMax,5)
   return .f.
else
  return .t.
endif     

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 DO PANTALLA
 SELECT AnuPA
 SCATTER MEMVAR BLANK
 m.Estado  = '00'
 m.NumRef  = '    '
 m.periodo = '  '
 m.FecPa   = date()
 m.periodo = str(year(date()) - 1900,2)
 @  1,60 GET m.Periodo picture '!!' valid !empty(m.Periodo)
 @  1,63 GET m.Codfte  valid   val_para(m.Codfte,'CODFTE','C')
 @  1,67 GET m.NumRef  PICTURE  '!!!!' VALID VAL_REV()
 READ
 IF LASTKEY()=27
    SELECT AnuPA
    DO VISTA
    RETURN
 ENDIF

 SELECT Parma
 if vOpcion = 1
    SEEK 'CORREL'+'ANULOC'
 else
    SEEK 'CORREL'+'ANULOS'
 endif
 = valPA ( Parma.NumEnt + 1 )
 SELECT AnuPA

 IF vOpcion = 1
    m.TipFun = orden.TipFun
    m.TipDoc =  'O/C'
    m.NumRef = orden.NumOc
    m.Mespa  = orden.NumMes
    m.FecRef = orden.fecOc
    m.CodPrv = orden.CodPrv
    m.Codcal = orden.codcal
    m.NumHc  = orden.NumHc
    m.PerHc  = orden.PerHc    
    m.NumCp  = orden.NumCp
    m.PerCp  = orden.PerCp    
    m.valtot = 0
    vMax = orden.Valtot
 ELSE
    m.TipFun = ordse.TipFun
    m.TipDoc =  'O/S'
    m.NumRef = ordse.NumOs
    m.Mespa  = ordse.NumMes    
    m.FecRef = ordse.fecOs
    m.CodPrv = ordse.CodPrv
    m.Codcal = ordse.codcal
    m.NumHc  = ordse.NumHc
    m.PerHc  = orden.PerHc    
    m.NumCp  = ordse.NumCp
    m.PerCp  = ordse.PerCp        
    m.valtot = 0
    vMax = ordse.Valtot    
 ENDIF
 m.motivo  = space(254)
 m.observa = space(40)
 vCodFte  = substr(m.codcal,05,3)
 vCodPrg  = substr(m.codcal,08,2)
 vCodSub  = substr(m.codcal,10,3)
 vProyec  = substr(m.codcal,13,3) 
 vCodact  = substr(m.codcal,13,2) 
 vSubpry  = substr(m.codcal,16,4)

 @  1,22 GET m.NumPa   &&DISABLE
 @  2,22 GET m.FecPa

 @  1,60 GET m.Periodo DISABLE
 @  1,63 GET m.Codfte  DISABLE
 @  1,67 GET m.NumRef  DISABLE

 @  2,60 GET m.FecREf  DISABLE

 @  4,22 SAY val_prv(m.Codprv)
 @  6,22 SAY m.ValTot  PICTURE '999,999.99' && valid verfi_s()
 @  8,22 SAY VAL_PARA(vCodFte,'CODFTE','V',22,20)
 @  8,60 SAY VAL_PARA(M.tipfun,'TIPFUN','D',60,15,2)

 @  9,22 SAY val_para(substr(m.CodCal, 8,2),'CODPRG','V',26,40)
 @ 10,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @ 11,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
 
 IF alltrim(m.Tipfun)='I' 
    @ 11,25 SAY '.'
	@ 11,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
 else
    @ 11,22 SAY Spac(60)	
 endif	
 
 @ 13,22 GET m.Motivo PICTURE '@S56'
 
 @ 15,22 SAY m.NumHC 
 @ 15,26 SAY '.'
 @ 15,27 SAY m.PerHC  
 
 @ 16,22 SAY m.NumCP  
 @ 16,26 SAY '.'
 @ 16,27 SAY m.PerCp  
 
 @ 17,22 SAY m.NumChq 

 @ 19,22 GET m.Observa
 
 READ VALID Val_Read()

 IF LASTKEY() # 27
    vcod = m.numpa
    IF F_Appd()
       *- Aumento el correlativo
       SELECT Parma
       if vOpcion = 1
          SEEK 'CORREL'+'ANULOC'
       else
          SEEK 'CORREL'+'ANULOS'
       endif
       = valPA ( Parma.NumEnt + 1 )
       REPLACE NumEnt WITH NumEnt + 1
       IF vOPCION=1
          SELE ORDEN
          IF RLOCK()
             REPLACE ORDEN.ESTADO WITH '90',ANULTOT WITH m.ValTot
          ENDIF
          sele iteoc
          SCAN WHILE iteoc.numoc+iteoc.codfte = orden.numoc + orden.codfte
               vArt = iteoc.codart
               vCal = iteoc.codcal
               vPrv = orden.codprv
               vkey = iteoc.periodo+iteoc.numoc+iteoc.codfte
		 	   vfun = .t.
          	   *SELE ITEPEC
 		 	   *SET ORDE TO 7
		       *vTemp=Recno()
		       *vkey = iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart
     	       *SEEK VKEY
		       *If FOUND()
		 	   *   IF RLOCK()
	      	   *     REPLACE orden with ' ',estado with '95',;
   	       	   *     NUMOC WITH SPACE(4)
   	       	   *   ELSE 	
     	       *     do standby with 'Advertencia:La Pecosa no se habilit¢'
     	       *     vFun=.f.     	       	
			   *    ENDIF
			   *   UNLOCK
		       *else
			   *  do standby with 'Advertencia:No es ubicado la Pecosa,Revise'
			   *  vFun=.f.
		       *ENDif
		       SELECT ITEoc
               IF RLOCK() and vfun
		          REPLACE ESTADO WITH '95'
		       ENDIF
		       UNLOCK
          ENDSCAN
       ELSE
          SELE ORDSE
          IF RLOCK()
             REPLACE ORDSE.ESTADO WITH '95',ANULTOT WITH m.ValTot
          ENDIF
       ENDIF
       UNLOCK ALL
       m.codcal = m.periodo+ALLTRIM(m.Mespa)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
       m.numpa = vcod
       m.tipo = 'R'
       SELECT AnuPA
       GATHER MEMVAR
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    SELECT AnuPA
 ENDIF
 DO Vista
 RETURN

PROCEDURE VAL_REV              && Revisi¢n de BD en browse
*----------------
IF vOpcion = 1
   AS=ALIAS()
   SELECT ORDEN
   IF EOF()
      DO standby WITH Vmens08
      RETURN
   ENDIF
   SET RELATION TO PERIODO+NUMOC+CODFTE INTO ITEOC
   SET SKIP TO ITEOC
   SET FILTER TO ESTADO#'90'AND ESTADO#'99'
   GO TOP
   SEEK m.Periodo+m.Numref+alltrim(m.Codfte)
   if !found()
      vTemp = RECNO()
      HIDE MENU mMenu
      ACTIVATE SCREEN
      vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
      DO Logos WITH Rotulo1,vTempo
      ON KEY LABEL F10 KEYBOARD CHR(23)
      BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
      NumOc  :H=' N§ ' ,;
      FecOc  :H='Fecha' ,;
      ess=IIF( Estado= '00','Pend',IIF( Estado = '20','C/c ',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
      iteOc.descri :H='Articulo ' :36 ,;
      iteOc.coduni :H='Unid' ,;
      iteOc.Canreq :H='Cantid'
      vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
      DO Logos WITH Rotulo1,vTempo
      SHOW MENU mMenu
      ON KEY LABEL F10
   endif
   set filter to
   SET RELATION TO
   SELE (AS)
   DO Vista
ELSE
   AS=ALIAS()
   SELECT OrdSe
   IF EOF()
       DO standby WITH Vmens08
       RETURN
   ENDIF
   SET FILTER TO ESTADO#'90'AND ESTADO#'99'
   GO  TOP
   SEEK m.Periodo+m.Numref+alltrim(m.Codfte)
   if !found()
    Vtemp = RECNO()
    HIDE MENU mMenu
    ACTIVATE SCREEN
    vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    ON KEY LABEL F10 KEYBOARD CHR(23)
    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
       NumOs  :H=' N§ ' ,;
       Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
       FecOs  :H='Fecha' ,;
       CodDep :H='DEP',;
       Codcal :H='Calendario' ,;
       DESOS  :H='Descripci¢n'

       vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    IF LASTKEY()=27
       GOTO Vtemp
    ENDIF
    SHOW MENU mMenu
    ON KEY LABEL F10
   ENDIF
   SELE (AS)
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


PROCEDURE Anula
*--------------
 SELECT AnuPA
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR esta Parte ?')
 IF vElimina
    IF VOPCION=1
       SELE ORDEN
       IF RLOCK()
          REPLACE ORDEN.ESTADO WITH '00'
       ENDIF
    ELSE
       SELE ORDSE
       IF RLOCK()
          REPLACE ORDSE.ESTADO WITH '00'
       ENDIF
    ENDIF
    SELECT AnuPA
    IF  RLOCK()
       REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
    ENDIF
    DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Agreg_Item
*-------------------
vOrd=ORDER()
Sele iteOc
IF F_appd()
  REPLACE NumOc  WITH m.NumOc ,;
          Periodo WITH m.Periodo ,;
          CodArt WITH Itepec.CodArt ,;
          CodCal WITH Itepec.CodCal ,;
          CanReq WITH Itepec.Canreq ,;
          CodUni WITH Itepec.Coduni ,;
          DesCri WITH Itepec.Descri ,;
          Preuni WITH Itepec.Preuni
  RETURN .T.
ENDIF
SET ORDER TO vOrd
RETURN .F.


PROCEDURE Marca
*-----------------
vTemp = Recno()
vOrde = Itepec.Orden
vCodCal=ItePec.CodCal+ItePec.CodPrv
SET FILTER TO ItePec.CodCal+ItePec.CodPrv=vCodCal
GO TOP
SCAN
   DO CASE
      CASE vOrde='*'
           if rlock()
              REPLACE orden WITH '*' , estado with '30'
           ENDIF
      CASE vOrde=' '
           if rlock()
              REPLACE orden WITH ' ' , estado with '20'
           ENDIF
   ENDCASE
ENDSCAN
SET FILTER TO
go vTemp
KEYBOARD CHR(23)
RETURN .T.

PROCEDURE Lista
*--------------
SELECT AnuPA
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
  vmensL= Iif( Vopcion=1,' PARTE ANULACION O/C',' PARTE ANULACION O/S')
  do reporte with 2,"LisPA",vmensL
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
*-----------*-----------*
