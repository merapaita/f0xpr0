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
   USE PteAnu   IN 1   order tag PteAnu3  ALIAS AnuPA
   vanureb='O/C'
else
   USE PteAnu   IN 1   order tag PteAnu4  ALIAS AnuPA
   vanureb='O/S'
endif
USE PARMAE    IN 4   order tag Parmae1  ALIAS Parma
USE Promae   IN 8   order tag Promae1  ALIAS Promae
USE maepre   IN 10  order tag maepre1  ALIAS maepre
USE itepar   in 11  order tag itepar1  ALIAS ITEPAR          
USE HOJCON   in 12  order tag hojcon1  ALIAS hoja
USE Itehc    IN 13  order tag Itehc1    ALIAS Itehc  
USE Clase    IN 14  order tag Clase1    ALIAS Clase  
USE AstPre   IN 15  order tag Astpre21  ALIAS AstPre
USE Cuentas  IN 16  order tag Cuentas1  ALIAS Cuenta


*- Mensajes de aviso al usuario
ON KEY LABEL F7 do vis_ap       
PUBLIC vrebaja
if vOpcion = 1
 Vmens01 = ' Parte Rebaja Orden de Compra : REVISION '
 Vmens02 = ' Registro de Parte Rebaja Ordenes de Compra '
 Vmens04 = 'Dicho Orden de Compra no fue encontrado'
 Vmens05 = 'No existe Orden de Compra anterior'
 Vmens06 = 'No existe Orden de Compra siguiente'
 Vmens07 = '¨ Desea ModificaR ‚ste Orden de Compra ?'
 Vmens08 = 'No hay registros para procesar'
 Vmens09 = 'Este Orden de Compra ha sido Modificado'
 Vmens10 = 'Este Orden de Compra ya fue atendido'
 Vmens11 = 'Este Orden de Compra ha sido devuelto'
else
 Vmens01 = ' Parte Rebaja Orden de Servicio: REVISION '
 Vmens02 = ' Registro de Parte Rebaja Ordenes de Servicio '
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente           Ingresa  aNula            Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 15,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 16,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle: ' COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 07,08 TO 17,72 double;
 TITLE ' COMPROMISO PRESUPUESTAL    [®F5¯Agrega  ®F8¯Borra]'  COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54

 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD anula  OF mMenu DO anula

 ON SELECTION PAD termi  OF mMenu DO termi
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
@  1, 2 SAY "       N£mero P/R :"
@  2, 2 SAY "        Fecha P/R :"
IF vOpcion =1							&& Para las O/C
   @  1,40 SAY "       N£mero O/C :"
   @  2,40 SAY "        Fecha O/C :"
else									&& Para las O/S
   @  1,40 SAY "       N£mero O/S :"
   @  2,40 SAY "        Fecha O/S :"
endif
@  3, 2 SAY "        Proveedor :"
@  4, 2 SAY "       Cadena Fun :"
@  4,40 SAY " Fte. Financiami. :"
@  5, 2 SAY "          Funci¢n :"
@  6, 2 SAY "         Programa :"
@  7, 2 SAY "      Subprograma :"
@  8, 2 SAY "   Activ./Proyec. :"
@  9, 2 SAY "           Motivo :"
@ 10, 2 SAY "       Numero H/C :"
@ 10,40 SAY "       Numero C/P :"
@ 11, 2 SAY "    Numero Cheque :"
@ 12, 2 SAY "    Observaciones :"
@ 13, 2 SAY "   Mes a Trabajar :"
@ 13,40 SAY "       Incidencia :"
 
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT AnuPA
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 SCATTER MEMVAR
 =val_codcad(allt(m.codcad),m.periodo,'C')
 ACTIVATE WINDOW Wind_1
 @  0,60 SAY IIF( m.Estado= '00','Pendiente',IIF( m.Estado = '20','Atendido ',IIF(m.Estado='99','Anulada  ',IIF(m.Estado='5','Contabilz','         '))))

 @  1,22 SAY m.NumPa
 @  1,60 SAY m.Periodo 
 @  1,63 SAY m.Codfte  
 @  1,67 SAY m.NumRef  

 @  2,22 SAY m.FecPa
 @  2,60 SAY m.FecRef

 @  3,22 SAY val_prv(m.Codprv)
 @  4,22 SAY val_codcad(m.codcad,m.periodo,'D',22,17)
 @  4,60 SAY val_para(m.codfte,'CODFTE','D',22,17)
 @  5,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  6,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  7,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @  8,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)
 @  9,22 SAY m.Motivo PICTURE '@S56'
 @ 10,22 SAY m.NumHC 
 @ 10,26 SAY '.'
 @ 10,27 SAY m.PerHC  
 @ 10,60 SAY m.NumCP  
 @ 10,64 SAY '.'
 @ 10,65 SAY m.PerCp  
 @ 11,22 SAY m.NumChq 
 @ 12,22 SAY m.Observa
 @ 13,22 SAY val_para(m.Nummestr,'FECMES','V',22,15,3)
 @ 13,60 SAY IIF(EMPTY(m.tipcom),space(18),val_para(m.tipcom  ,'HOJMOD','D',60,18,3))

 DO VISTA_HIJO

*@ 21,22 say m.partida
 
RETURN

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
ACTIVATE WINDOW WIND_2
SELE ITEHC
SEEK m.Perhc + m.NumHc
IF FOUND()
   BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Perhc + m.NumHc TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   TIPOPE 	: H='ä':W=.F. ,;
   CODCOM   : H='Comp.',;
   CODMET   : H='Meta',;
   CodPart  : H='Partid',;
   AA=val_para(right(CodPart,2),'ESPGAS','D',22,40) :H='Descripci¢n':40,;
   Valpart	: H='  Parcial' :P='99,999,999.99',;
   XX=IIF(!ITEHC.ESTADO='92','       ','H/M:'+NUMHM)  : H=IIF(!ESTADO='90','          ','Hoja Modificac') :15
ELSE
   CLEAR
   @ 2,25 SAY 'No existe detalle, Revise..'
ENDIF
SELE ANUPA
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
 ess=IIF( Estado= '00','Pend',IIF( Estado = '20','Afec',IIF(Estado='9','Anul',IIF(Estado='40','Aten','Liqu')))) :H='Estd' ,;
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
SELECT ANUPA
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
SELECT ANUPA

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

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT AnuPA
 SCATTER MEMVAR 
 ACTIVATE WINDOW Wind_1
 @ 13,22 GET m.NumMestr picture '!!' valid val_para(m.Nummestr,'FECMES',' ',22,15,3)
 @ 13,60 GET m.tipcoM picture '!!!' valid val_para(m.tipcom  ,'HOJMOD',' ',60,18,3)
 read VALID val_read()
 if lastkey()#27
 	ok  = Escoge_O()
    ok1 = traba_hijo()
	SELECT AnuPA
	IF OK AND OK1
        DO COMPRE1   
        SELECT HOJA
		SEEK m.Perhc + m.NumHc
		REPLACE ESTADO WITH '80'
        SELECT Anupa
    	m.Estado = '20'
    	GATHER MEMVAR
    	
 	ELSE
    	DO STANDBY WITH 'Proceso cancelado'
	    SELECT AnuPA
	endif
 ELSE
   	DO STANDBY WITH 'Proceso cancelado'
    SELECT AnuPA
 endif

 DO Vista
 RETURN

PROCEDURE TRABA_HIJO
*-------------------
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO agreg_pa
ON KEY LABEL F8  DO elimi_pa
ON KEY LABEL F10 KEYBOARD CHR(23)
vfun=.f.
SELECT itehC
SEEK m.Perhc + m.NumHc
if !found()
  DO STANDBY WITH 'No tiene registros'
  return vfun
else
	BROWSE WINDOW Wind_2  NOAPPEND NODELETE NOMENU key m.Perhc + m.NumHc FIELDS ;
       TIPOPE   : H='ä'       :V=TIPOPE$'-+' :W=!ITEHC.TIPOPE $ 'è*',;
       CODCOM	: H= 'Comp.':V=Val_comp(m.periodo+uniges+unieje+m.codcad,codcom,'codcom'):F ,;
       CODMET   : H= 'Meta' :V=Val_meta(m.periodo+uniges+unieje+m.codcad,codcom+codmet,'codmet'):F ,;
       CodPart  : H= 'Partid' :F :W=!ITEHC.TIPOPE $ 'è*',;
       aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,40)) :H='Descripci¢n':40 :W=.F.,;
       ValPart :H='Total' :P='99,999,999.99' :W=!ITEHC.TIPOPE $ 'è*'
endif

SELECT itehC
SEEK m.Perhc + m.NumHc
vrebaja = 0
DO while  Perhc + NumHc = m.Perhc + m.NumHc AND !EOF()
	if tipope='-'
		vrebaja = vrebaja + valpart
	endif
	SKIP
enddo
SEEK m.Perhc + m.NumHc
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'

DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F10
SELE anupa
if lastkey()=27
   return .f.
endif
RETURN .t.

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
seek alltrim(m.MESPA)+m.Numpa+'P/A'+vanureb
if !found()
   =asi_pres()
endif
SET ORDE TO ASTPRE21
BROWSE WINDOW Wind_5  NOAPPEND NODELETE NOMENU key alltrim(m.MESPA)+m.Numpa+'P/A'+vanureb FIELDS ;
       TIPO   : H='' :V=TIPO$'DH' and valtipo():F ,;
       CtaDeb : H='Cta.Debe' :V=iif(tipo='D',val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctadeb,2),.t.):F,; 
       CtaHab : H='Cta.Haber':V=iif(tipo='H',val_fun('Cuenta','Cuenta',"LEFT(Cuenta,10)+' '+Descri",ctaHAB,2),.t.):F,; 
       valdeb : H=' al Debe ':W=iif(tipo='D',.t.,.f.) ,;
       valHab : H=' al Haber':W=iif(tipo='H',.t.,.f.) 

seek alltrim(m.MESPA)+m.NumPA+'P/A'+VANUREB
SCAN WHILE NUMMES=alltrim(m.MESPA) AND NUMREF=m.NumPA AND TIPDOC='P/A' AND ANUREB=VANUREB
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

function asi_pres
*----------------
SELECT astpre
do case
	case allT(m.tipCoM) ='C'
		SET ORDE TO astpre1
		seek 'D'+m.PerHC+m.NumHC
		IF FOUND()
			vcuentaH=cuenta
			vValorH =vrebaja
			APPEND BLANK
			replace Periodo with m.periodo,Nummes with m.MESPA , TipDoc with 'P/A' , NumRef with m.NumPA, Fecref with m.fecPA
			REPLACE ANUREB WITH vanureb
			REPLACE TIPO WITH 'H',CUENTA WITH vcuentaH,ctahab with vcuentaH
			replace codcad with m.codcad,valhab with vValorH
		ENDIF	
		seek 'H'+m.PerHC+m.NumHC
		IF FOUND()
			vcuentaD=cuenta
			vValorD =vrebaja
			APPEND BLANK
			replace Periodo with m.periodo,Nummes with m.MESPA , TipDoc with 'P/A' , NumRef with m.NumPA, Fecref with m.fecPA
			REPLACE ANUREB WITH vanureb
			REPLACE TIPO WITH 'D',CUENTA WITH vcuentaD,ctaDEB with vcuentaD
			replace codcad with m.codcad,valDEB with vValorD
		ENDIF	
	case allT(m.tipCoM) ='CYP'
	
	case allT(m.tipCoM) ='P'
endcase
RETURN

PROCEDURE VIS_AP
*---------------
PRIVATE AD
AD =ALIAS()
SELECT ASTPRE
set orde to astpre21
seek alltrim(m.MESPA)+m.Numpa+'P/R'+vanureb
ON KEY LABEL F7
BROWSE WINDOW Wind_5 NOEDIT NOAPPEND NODELETE NOMENU key alltrim(m.MESPA)+m.Numpa+'P/R'+vanureb FIELDS ;
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
   replace Periodo with m.periodo,Nummes with m.MESPA , TipDoc with 'P/R' , NumRef with m.NumPA, Fecref with m.fecPA
   REPLACE ANUREB with vanureb
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

PROCEDURE Agreg_pa
*-----------------
sele iteHc
=val_codcad(allt(m.codcad),m.periodo,'C')
Vcodcom = MAEPRE.CODCOM
Vcodmet = MAEPRE.codmet
IF F_appd()
    REPLACE Itehc.NumHc   WITH m.NumHc ,;
 		    Itehc.NumMes  WITH m.PerHC  ,;
            Itehc.CodCad  WITH m.CodCad,;
            Itehc.Estado  WITH '00' ,;
            Itehc.CodFte  WITH m.Codfte ,;
			Itehc.Tipdoc  WITH 'P/R',;
            Itehc.tipope  with '-',;
            Itehc.codcom  with Vcodcom,;
            Itehc.codmet  with Vcodmet,;
            Itehc.uniges  WITH '01',;
            Itehc.unieje  WITH '001',;
            Itehc.numpr WITH m.numpa,;
            Itehc.mespr with m.nummestr
ENDIF
UNLOCK
RETURN .T.


PROCEDURE Elimi_pa
*-----------------
if ITEHC.TIPOPE $ 'è*'
   RETURN .T.
ENDIF   
if rlock()
   delete next 1
endif
UNLOCK
return .T.

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
  DEACTIVATE WINDOW WIND_1
  DEACTIVATE WINDOW WIND_2
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
  
***************
****NUEVAS FUNCIONES
***************

PROCEDURE Escoge_O            && Revisi¢n de BD en browse
*-----------------
PRIVATE vTemp
aS=ALIAS()
private  vfun
vfun = .f.
vInd = SYS(3) + '.IDX'
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
        USE IteOc1   IN 9   order tag IteOc11   ALIAS IteOc1
        SELE ITEOC1
   CASE alltrim(m.tipdoc)='O/S'
        USE ITEOS1    IN 9 order tag Iteos11 ALIAS iteos1
        SELECT iteos1
ENDCASE
IF EOF()
   DO standby WITH ' Documento no en Linea '
   SELE HOJA
   VFUN=TRABA_HIJO()
   RETURN vfun
ENDIF
IF LASTKEY()#27
   DO AGREGA_ITEM
   vFun = .T.
ELSE
   vFun = .F.
ENDIF
ON KEY LABEL F10
SELE (aS)
RETURN vFun

PROCEDURE AGREGA_ITEM
*-------------------
DO CASE
   CASE alltrim(m.tipdoc)='O/C'
      * PASAR DE ITEOC A ITEHC
      	SET ORDER TO ITEOC13
        SEEK m.Periodo+m.NumRef
        if found()
        	SCAN WHILE NUMOC = m.NumRef and PERIODO = m.Periodo
          		vPart1 =Numoc+Codcad+codcom+codmet+codpart
          		vPart2 =Numoc+Codcad+codcom+codmet+codpart
          		vTot = 0
          	    DO WHILE  vPart1 = vPart2
          	    	IF VALREB>0
              			vTot = vTot + (ValPart-VALREB)
              		ENDIF	
             		vcodcom = codcom
              		vcodmet = codmet
              		vcodpar = codpart
              		SKIP
              		vPart1  = Numoc+Codcad+codcom+codmet+codpart
          		ENDDO
          		SKIP -1
          		IF vTot>0
          			DO AGREG_HC
          		ENDIF	
          		SELE ITEOC1
        	ENDSCAN
        else
      	    do standby with 'Error.'
        endif
   CASE alltrim(m.tipdoc)='O/S'
      * PASAR DE ITEOS A ITEHC
      	SET ORDER TO ITEOS13
        SEEK m.Periodo+m.NumRef
        if found()
        	SCAN WHILE NUMOS = m.NumRef and PERIODO = m.Periodo
          		vPart1 =NumoS+Codcad+codcom+codmet+codpart
          		vPart2 =NumoS+Codcad+codcom+codmet+codpart
          		vTot = 0
          	    DO WHILE  vPart1 = vPart2
          	    	IF VALREB>0
              			vTot = vTot + (ValPart-VALREB)
              		ENDIF	
             		vcodcom = codcom
              		vcodmet = codmet
              		vcodpar = codpart
              		SKIP
              		vPart1 =NumoS+Codcad+codcom+codmet+codpart
          		ENDDO
          		SKIP -1
          		IF vTot>0
          			DO AGREG_HC
          		ENDIF	
          		SELE ITEOS1
        	ENDSCAN
        else
      	    do standby with 'Error.'
        endif
        
ENDCASE
return

PROCEDURE Agreg_HC
*-----------------
AS=ALIAS()
sele iteHc
IF F_appd()
    REPLACE Itehc.NumHc   WITH m.NumHc ,;
 		    Itehc.NumMes  WITH m.PerHC  ,;
            Itehc.ValParT WITH vTot    ,;
            Itehc.CodCad  WITH m.CodCad,;
            Itehc.Estado  WITH '00' ,;
            Itehc.CodFte  WITH m.Codfte ,;
			Itehc.Tipdoc  WITH 'P/R',;
            Itehc.tipope  with '-',;
            Itehc.codcom  with Vcodcom,;
            Itehc.codmet  with Vcodmet,;
            Itehc.CodPart WITH VCodpar,;
            Itehc.uniges  WITH '01',;
            Itehc.unieje  WITH '001',;
            Itehc.numpr WITH m.numpa,;
            Itehc.mespr with m.nummestr

   SELE &AS
   RETURN .T.
ENDIF
SELE &AS
RETURN .F.

procedure valtipo
*----------------
do case
   CASE tipo = 'D'
        replace CtaHab with spac(11),valhab with 0
   CASE tipo = 'H'
        replace CtaDeb with spac(11),valdeb with 0
endcase  
RETURN .T.
