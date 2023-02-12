*--------------------------------------------------------------------------
* AnuPar.Prg
* Anula Orden de Compra
* Estado :
*   '00' Emitida   Este es el que se registra en la Orden de Compra
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: 
*--------------------------------------------------------------------------
parameter vOpcion
*- Abriendo Archivos
if vOpcion = 1
   USE PteAnu   IN 1   order tag PteAnu1  ALIAS AnuPA
   USE OrdCom   IN 2   order tag OrdCom1  ALIAS Orden
   USE IteOc    IN 3   order tag IteOc1   ALIAS IteOc
   USE IteOc1   IN 15  order tag IteOc11  ALIAS IteOc1
else
   USE PteAnu   IN 1   order tag PteAnu2  ALIAS AnuPA
   USE OrdSer   IN 2   order tag OrdSer1  ALIAS Ordse
   USE Solser   IN 3   order tag Solser1  ALIAS Solser
   USE IteOs1   IN 15  order tag IteOs11  ALIAS IteOs1   
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
USE Cheque   IN 13  ORDER TAG Cheque1  ALIAS Cheque

*- Mensajes de aviso al usuario

PUBLIC VMENSL
vmensL= Iif( Vopcion=1,' PARTE ANULACION O/C',' PARTE ANULACION O/S')
if vOpcion = 1
 Vmens01 = ' Parte Anulaci¢n Orden de Compra : REVISION '
 Vmens02 = ' Registro de Parte Anulaci¢n Ordenes de Compra '
 Vmens04 = 'Dicho Orden de Compra no fue encontrado'
 Vmens05 = 'No existe Orden de Compra anterior'
 Vmens06 = 'No existe Orden de Compra siguiente'
 Vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Este Orden de Compra ha sido anulado'
 Vmens10 = 'Este Orden de Compra ya fue atendido'
 Vmens11 = 'Este Orden de Compra ha sido devuelto'
else
 Vmens01 = ' Parte Anulaci¢n Orden de Servicio: REVISION '
 Vmens02 = ' Registro de Parte Anulaci¢n Ordenes de Servicio '
 Vmens04 = 'Dicho Orden de Servicio no fue encontrado'
 Vmens05 = 'No existe Orden de Servicio anterior'
 Vmens06 = 'No existe Orden de Servicio siguiente'
 Vmens07 = '¨ Desea ANULAR ‚ste Orden de Servicio ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Este Orden de Servicio ha sido anulado'
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
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
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
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
 @  5, 2 SAY "         Importe  :"

 @  6, 2 SAY "      Cadena Fun. :"
 @  7, 2 SAY " Fte. Financiami. :"
 
 @  8, 2 SAY "          Funci¢n :"
 @  9, 2 SAY "         Programa :"
 @ 10, 2 SAY "      Subprograma :"
 @ 11, 2 SAY "   Activ./Proyec. :"

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
 =val_codcad(allt(m.codcad),m.periodo,'C')

 @  0,60 SAY IIF( m.Estado= '00','Pendiente   ',IIF( m.Estado = '20','En Cont.Pres.',IIF(m.Estado='99','Anulada      ',IIF(m.Estado='50','Atendido    ','             '))))

 @  1,22 SAY m.NumPa

 @  1,60 SAY m.Periodo 
 @  1,63 SAY m.Codfte  
 @  1,67 SAY m.NumRef  

 @  2,22 SAY m.FecPa
 @  2,60 SAY m.FecRef

 @  4,22 SAY val_prv(m.Codprv)

 @  5,22 SAY m.ValTot PICTURE '999,999.99'

 @  6,22 SAY val_codcad(m.codcad,m.periodo,'D',22,30)
 @  7,22 SAY val_para(m.codfte,'CODFTE','D',22,30)

 @  8,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  9,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @ 10,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @ 11,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

 @ 13,22 SAY m.Motivo PICTURE '@S56'

 @ 15,22 SAY m.NumHC 
 @ 15,26 SAY '.'
 @ 15,27 SAY m.PerHC  
 
 @ 16,22 SAY m.NumCP  
 @ 16,26 SAY '.'
 @ 16,27 SAY m.PerCp  
 @ 17,22 SAY m.numchq
 
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
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Aten',IIF(Estado='99','Anul',IIF(Estado='50','Liqu','Aten')))) :H='Estd',;
 codprv :H='Prv' ,;
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


PROCEDURE VAL_REV              && Revisi¢n de BD en browse
*----------------
PRIVATE VTEMP

IF vOpcion = 1  && ************ PARTE DE ANULACION O/C *******************
   AS=ALIAS()
   SELECT ORDEN
   SET FILT TO ESTADO#'5'
   IF EOF()
      DO standby WITH Vmens08
      RETURN
   ENDIF
   SET RELATION TO PERIODO+NUMOC+CODFTE INTO ITEOC
   SET SKIP TO ITEOC
   SET FILTER TO ESTADO#'90'AND ESTADO#'99'
   GO TOP
   SEEK m.Periodo+m.Numref+alltrim(m.Codfte)
   IF !FOUND()
      vTemp = RECNO()
      HIDE MENU mMenu
      ACTIVATE SCREEN
      vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
      DO Logos WITH Rotulo1,vTempo
      ON KEY LABEL F10 KEYBOARD CHR(23)
      BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
      NumOc  :H=' N§ ' ,;
      FecOc  :H='Fecha' ,;
      ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Afec.',IIF(Estado='99','Anul',IIF(Estado='50','Aten','    ')))) :H='Estd' ,;
      iteOc.descri :H='Articulo ' :36 ,;
      iteOc.coduni :H='Unid' ,;
      iteOc.Canreq :H='Cantid'
      vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
      DO Logos WITH Rotulo1,vTempo
      SHOW MENU mMenu
      ON KEY LABEL F10
      m.NumHc  = orden.NumHc
      m.PerHc  = orden.PerHc

	  SELECT Hoja
	  vOrd = ORDER()
	  SET ORDER TO hojcon1
	  SEEK m.PerHc+m.numhc

	  IF FOUND() 
		IF !EMPTY(HOJA.NumCp)
			m.NumCp  = Hoja.NumCp
			m.percp  = Hoja.NumMesCp    
			vCta    = Hoja.CodCtc
	    else		
			m.Numcp  =''
			m.percp  =''
			vCta    =''
	    endif 	
	  ELSE
		m.Numcp  =''
		m.Percp  =''
		vCta    =''
	  ENDIF
	  SET ORDER TO (vOrd)

	  SELECT Cheque
	  vord = ORDER()
	  SET ORDER TO Cheque1
	  SEEK m.percp+m.NumCp+vCta
	  IF FOUND() 
	  	m.NumChq = Cheque.NumChq
	  ELSE
		m.NumChq =''
	  ENDIF
	  SET ORDER TO (vOrd)
      m.codfte = iteoc.codfte
   ELSE
      m.NumHc  = orden.NumHc
      m.PerHc  = orden.PerHc
	
	  SELECT Hoja
	  vOrd = ORDER()
	  SET ORDER TO hojcon1
	  SEEK m.PerHc+m.numhc

	  IF FOUND() 
		IF !EMPTY(HOJA.NumCp)
			m.NumCp  = Hoja.NumCp
			m.percp  = Hoja.NumMesCp    
			vCta    = Hoja.CodCtc
	    else		
			m.Numcp  =''
			m.percp  =''
			vCta    =''
	    endif 	
	  ELSE
		m.Numcp  =''
		m.Percp  =''
		vCta    =''
	  ENDIF
	  SET ORDER TO (vOrd)

	  SELECT Cheque
	  vord = ORDER()
	  SET ORDER TO Cheque1
	  SEEK m.percp+m.NumCp+vCta
	  IF FOUND() 
	  	m.NumChq = Cheque.NumChq
	  ELSE
		m.NumChq =''
	  ENDIF
	  SET ORDER TO (vOrd)
      m.codfte = iteoc.codfte
   ENDIF
*   set filter to
*   SET RELATION TO
   SELE (AS)
*   DO Vista
ELSE  && ******************* PARTE DE ANULACION DE O/S ********************
   AS=ALIAS()
   SELECT OrdSe
   SET FILT TO ESTADO#'5'   
   IF EOF()
       DO standby WITH Vmens08
       RETURN
   ENDIF
   SET FILTER TO ESTADO#'90'AND ESTADO#'99'
   GO  TOP
   SEEK m.Periodo+m.Numref+alltrim(m.Codfte)
   IF !found()
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
       DESOS  :H='Descripci¢n'

       * Codcal :H='Calendario' ,;

       vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    m.codfte = ordse.codfte
    IF LASTKEY()=27
       GO TOP
*      GOTO Vtemp
    ENDIF
    SHOW MENU mMenu
    ON KEY LABEL F10
      m.NumHc  = ordSe.NumHc
      m.PerHc  = ordSe.PerHc
	
	  SELECT Hoja
	  vOrd = ORDER()
	  SET ORDER TO hojcon1
	  SEEK m.PerHc+m.numhc
	  IF FOUND() 
		IF !EMPTY(HOJA.NumCp)
			m.NumCp  = Hoja.NumCp
			m.percp  = Hoja.NumMesCp    
			vCta    = Hoja.CodCtc
	    else		
			m.Numcp  =''
			m.percp  =''
			vCta    =''
	    endif 	
	  ELSE
		m.Numcp  =''
		m.Percp  =''
		vCta    =''
	  ENDIF
	  SET ORDER TO (vOrd)

	  SELECT Cheque
	  vord = ORDER()
	  SET ORDER TO Cheque1
	  SEEK m.percp+m.NumCp+vCta
	  IF FOUND() 
	  	m.NumChq = Cheque.NumChq
	  ELSE
		m.NumChq =''
	  ENDIF
	  SET ORDER TO (vOrd)
      m.codfte = Ordse.codfte

   ELSE
      m.NumHc  = ordse.NumHc
      m.PerHc  = ordse.PerHc
	
	  SELECT Hoja
	  vOrd = ORDER()
	  SET ORDER TO hojcon1
	  SEEK m.PerHc+m.numhc

	  IF FOUND() 
		IF !EMPTY(HOJA.NumCp)
			m.NumCp  = Hoja.NumCp
			m.percp  = Hoja.NumMesCp    
			vCta    = Hoja.CodCtc
	    else		
			m.Numcp  =''
			m.percp  =''
			vCta    =''
	    endif 	
	  ELSE
		m.Numcp  =''
		m.Percp  =''
		vCta    =''
	  ENDIF
	  SET ORDER TO (vOrd)

	  SELECT Cheque
	  vord = ORDER()
	  SET ORDER TO Cheque1
	  SEEK m.percp+m.NumCp+vCta
	  IF FOUND() 
	  	m.NumChq = Cheque.NumChq
	  ELSE
		m.NumChq =''
	  ENDIF
	  SET ORDER TO (vOrd)
      m.codfte = ordse.codfte
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



PROCEDURE Marca
*-----------------
vTemp = Recno()
vOrde = Itepec.Orden
vCodCal=ItePec.CodCad+ItePec.CodPrv
SET FILTER TO ItePec.CodCad+ItePec.CodPrv=vCodCad
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
  ACTIVATE WINDOW STANDBY
  vnumpa = m.numpa
  vperio = m.periodo
  @ 1,1 say 'Ingrese N§ Pte Anulaci¢n : ' get vperio
  @ 1,32 get vnumpa
  read
  DEACTIVATE WINDOW STANDBY
  IF VOPCION = 1 
     SET ORDER TO PTEANU1
  ELSE
     SET ORDER TO PTEANU2
  ENDIF      
  SET FILTER TO PERIODO = VPERIO AND NUMPA=VNUMPA
  if vOpcion = 1
     do reporte with 2,"LisPAC1",vmensL,2,.F.,.F.
  else   
     do reporte with 2,"LisPAs1",vmensL,2,.F.,.F.
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
