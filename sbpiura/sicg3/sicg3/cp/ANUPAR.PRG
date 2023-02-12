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
parameter vOpcion,sistema
*- Abriendo Archivos
if vOpcion = 1
   USE PteAnu   IN 1   order tag PteAnu1  ALIAS AnuPA
   vanureb='O/C'
else
   vanureb='O/S'
   USE PteAnu   IN 1   order tag PteAnu2  ALIAS AnuPA
endif
USE PARMAE    IN 4   order tag Parmae1  ALIAS Parma
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE Itepec   IN 6   order tag ItePec4  ALIAS Itepec
USE AuxCot   IN 7   order tag AuxCot1  ALIAS Auxcot
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE Calen    IN 9   order tag calen1   ALIAS calen
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE HOJCON   in 12  order tag hojcon1  ALIAS hoja
USE Itehc    IN 13  order tag Itehc1    ALIAS Itehc  
USE AstPre   IN 14  order tag astpre21  ALIAS AstPre
USE Cuentas  IN 15  order tag Cuentas1  ALIAS Cuenta


*- Mensajes de aviso al usuario

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
ON KEY LABEL F7 do vis_ap       

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa                   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10


 DEFINE WINDOW Wind_5 FROM 07,08 TO 17,72 double;
 TITLE ' COMPROMISO PRESUPUESTAL    [®F5¯Agrega  ®F8¯Borra]'  COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
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
 @  6, 2 SAY "       Cadena Fun :"
 @  7, 2 SAY " Fte. Financiami. :"
 @  8, 2 SAY "          Funci¢n :"
 @  9, 2 SAY "         Programa :"
 @ 10, 2 SAY "      Subprograma :"
 @ 11, 2 SAY "   Activ./Proyec. :"
 @ 13, 2 SAY "           Motivo :"

 @ 15, 2 SAY "       Numero H/C :"
 @ 16, 2 SAY "       Numero C/P :"
 @ 17, 2 SAY "    Numero Cheque :"
 @ 18, 2 SAY "    Observaciones :"
 @ 20, 2 SAY "   Mes a trabajar :"
 @ 21, 2 SAY "       Incidencia :"
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT AnuPA
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 =val_codcad(allt(m.codcad),m.periodo,'C')
 @  0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','Atendido ',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='5','Contabilz','         '))))
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
 @ 17,22 SAY m.NumChq 
 @ 18,22 SAY m.Observa
 @ 20,22 SAY val_para(m.Nummestr,'FECMES','V',22,15,3)
 @ 21,22 SAY val_para(m.tipcom  ,'HOJMOD','V',22,30,3)
 
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
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Aten',IIF(Estado='99','Anul',IIF(Estado='50','Liqu','Aten')))) :H='Estd' ,;
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
SELE ANUPA
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
SELE ANUPA
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
 =val_codcad(allt(m.codcad),m.periodo,'C') 
 
 @  1,22 SAY m.NumPa
 @  1,60 SAY m.Periodo 
 @  1,63 SAY m.Codfte  
 @  1,67 SAY m.NumRef  

 @  2,22 SAY m.FecPa
 @  2,60 SAY m.FecRef

 @  4,22 SAY val_prv(m.Codprv)
 @  5,22 SAY m.ValTot PICTURE '999,999.99'
 @  6,22 SAY VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
 @  7,22 SAY VAL_PARA(m.CodFte,'CODFTE',' ',22,30)
 @  8,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  9,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @ 10,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @ 11,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

 @ 13,22 SAY m.Motivo  PICTURE "@S56"

 @ 15,22 SAY m.NumHC 
 @ 15,26 SAY '.'
 @ 15,27 SAY m.PerHC  
 @ 16,22 SAY m.NumCP  
 @ 16,26 SAY '.'
 @ 16,27 SAY m.PerCp  
 @ 17,22 SAY m.NumChq 
 @ 18,22 SAY m.Observa
 
 @ 20,22 GET m.Nummestr picture '!!' valid val_para(m.Nummestr,'FECMES',' ',22,25,3)
 @ 21,22 GET m.tipcom   picture '!!!' valid val_para(m.tipcom  ,'HOJMOD',' ',22,25,3)

*------

 READ VALID Val_Read()

 IF LASTKEY() # 27
    if !empty(m.numhc)
       select Hoja
       seek m.perhc+m.numhc
       if found()
          if rlock()
             replace numanu with m.NumPa,valanu with m.valtot,ESTADO WITH '70'
          endif   
       endif
       select itehc
       seek m.perhc+m.numhc
       if found()
          SCAN WHILE m.perhc=nummes and m.numhc=numhc
	          if rlock()
    	         replace numPA with m.NumPa,MESPR with m.NumMesTr,tipdoc with 'P/A',ESTADO WITH '70'
        	  endif   
          endscan 	  
	   endif
	       
       UNLOCK
    endif       
    DO COMPRE1     
    SELECT AnuPA
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 UNLOCK ALL
 DO PANTALLA
 DO Vista
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 DO PANTALLA
 SELECT AnuPA
 SCATTER MEMVAR 
 @  1,22 SAY m.NumPa
 @  1,60 SAY m.Periodo 
 @  1,63 SAY m.Codfte  
 @  1,67 SAY m.NumRef  

 @  2,22 SAY m.FecPa
 @  2,60 SAY m.FecRef

 @  4,22 SAY val_prv(m.Codprv)
 @  5,22 SAY m.ValTot PICTURE '999,999.99'
 @  6,22 SAY VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
 @  7,22 SAY VAL_PARA(m.CodFte,'CODFTE',' ',22,30)
 @  8,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  9,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @ 10,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @ 11,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

 @ 13,22 SAY m.Motivo PICTURE "@S56"

 @ 15,22 SAY m.NumHC 
 @ 15,26 SAY '.'
 @ 15,27 SAY m.PerHC  
 @ 16,22 SAY m.NumCP  
 @ 16,26 SAY '.'
 @ 16,27 SAY m.PerCp  
 @ 17,22 SAY m.NumChq 

 @ 18,22 SAY m.Observa PICTURE "@S56"

 @ 20,22 GET m.Nummestr picture '!!'  valid val_para(m.Nummestr,'FECMES',' ',22,25,3)
 @ 21,22 GET m.tipcom   picture '!!!' valid val_para(m.tipcom  ,'HOJMOD',' ',22,25,3)
 
 READ VALID Val_Read()

 IF LASTKEY() # 27
    if !empty(m.numhc)
       select Hoja
       seek m.perhc+m.numhc
       if found()
          if rlock()
             replace valanu with m.valtot,numanu with m.NumPa, ESTADO WITH '70'
          endif   
       endif
       select itehc
       seek m.perhc+m.numhc
       if found()
          SCAN WHILE m.perhc=nummes and m.numhc=numhc
          if rlock()
             replace numPA with m.NumPa,MESPR with m.NumMesTr,tipdoc with 'P/A',ESTADO WITH '70'
          endif   
          endscan
       endif
       UNLOCK
    endif      
    DO COMPRE1   
    SELECT AnuPA
    m.Estado = '20'
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
    SELECT AnuPA
 ENDIF
 DO Vista
 RETURN


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
 IF YESNO('¨ Desea ANULAR esta Parte ?')
    SELECT AnuPA
    IF  RLOCK()
       REPLACE ESTADO WITH '00'
    ENDIF
    DO Vista
 ENDIF
 UNLOCK
 RETURN

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
SET ORDE TO Astpre21
seek alltrim(m.MESPR)+m.Numpa+'P/A'+vanureb
if !found()
   =asi_pres()
endif
SET ORDE TO ASTPRE21
BROWSE WINDOW Wind_5  NOAPPEND NODELETE NOMENU key alltrim(m.MESPR)+m.Numpa+'P/A'+vanureb FIELDS ;
       TIPO   : H='' :V=TIPO$'DH' and valtipo():F ,;
       CtaDeb : H='Cta.Debe' :V=iif(tipo='D',val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctadeb,2),.t.):F,; 
       CtaHab : H='Cta.Haber':V=iif(tipo='H',val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctaHAB,2),.t.):F,; 
       valdeb : H=' al Debe ':W=iif(tipo='D',.t.,.f.) ,;
       valHab : H=' al Haber':W=iif(tipo='H',.t.,.f.) 

seek alltrim(m.MESPR)+m.NumPA+'P/A'+VANUREB
SCAN WHILE NUMMES=alltrim(m.MESPR) AND NUMREF=m.NumPA AND TIPDOC='P/A' AND ANUREB=VANUREB
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
SELEC (AS)
if lastkey()=27
   return .f.
endif
RETuRN .t.

PROCEDURE VIS_AP
*---------------
PRIVATE AD
AD =ALIAS()
SELECT ASTPRE
set orde to astpre21
seek alltrim(m.MESPR)+m.Numpa+'P/A'+vanureb
ON KEY LABEL F7
BROWSE WINDOW Wind_5 NOEDIT NOAPPEND NODELETE NOMENU key alltrim(m.MESPR)+m.Numpa+'P/A'+vanureb FIELDS ;
       TIPO   : H='' :V=TIPO$'DH' and valtipo():F ,;
       CtaDeb : H='Cta.Debe' :V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctadeb,2):F :W=iif(tipo='D',.t.,.f.),;
       CtaHab : H='Cta.Haber':V=val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctahab,2):F :W=iif(tipo='H',.t.,.f.) ,;
       valdeb : H=' al Debe ':W=iif(tipo='D',.t.,.f.) ,;
       valHab : H=' al Haber':W=iif(tipo='H',.t.,.f.) 
SELECT (AD)
ON KEY LABEL F7 do vis_ap       
       
RETURN


function asi_pres
*----------------
SELECT astpre
do case
	case allT(m.tipCoM) ='C'
		SET ORDE TO astpre1
		seek 'D'+m.PerHC+m.NumHC
		IF FOUND()
			vcuentaH=cuenta
			vValorH =valdeb
			APPEND BLANK
			replace Periodo with m.periodo,Nummes with m.MESPR , TipDoc with 'P/A' , NumRef with m.NumPA, Fecref with m.fecPA
			REPLACE ANUREB WITH vanureb
			REPLACE TIPO WITH 'H',CUENTA WITH vcuentaH,ctahab with vcuentaH
			replace codcad with m.codcad,valhab with vValorH
		ENDIF	
		seek 'H'+m.PerHC+m.NumHC
		IF FOUND()
			vcuentaD=cuenta
			vValorD =valHAB
			APPEND BLANK
			replace Periodo with m.periodo,Nummes with m.MESPR , TipDoc with 'P/A' , NumRef with m.NumPA, Fecref with m.fecPA
			REPLACE ANUREB WITH vanureb
			REPLACE TIPO WITH 'D',CUENTA WITH vcuentaD,ctaDEB with vcuentaD
			replace codcad with m.codcad,valDEB with vValorD
		ENDIF	
	case allT(m.tipCoM) ='CYP'
	
	case allT(m.tipCoM) ='P'
endcase
RETURN

PROCEDURE AGREG_AP
*-----------------
if f_appd()
   replace Periodo with m.periodo,Nummes with m.MESPR , TipDoc with 'P/A' , NumRef with m.NumPA, Fecref with m.fecPA
   REPLACE ANUREB WITH vanureb
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
        replace CtaHab with spac(11),valhab with 0
   CASE tipo = 'H'
        replace CtaDeb with spac(11),valdeb with 0
endcase  
RETURN .T.
