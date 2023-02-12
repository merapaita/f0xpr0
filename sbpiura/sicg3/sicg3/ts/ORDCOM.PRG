*--------------------------------------------------------------------------
* OrdCom.Prg
* Registra las Orden de Compra
* Estado :
*   '00' Emitida   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '20' Afectada
*   '50' Atendido
*   '70' Devuelta
*   '99' Anulada
* Autor: NRR.
*--------------------------------------------------------------------------
*- Abriendo Archivos
SET EXCL OFF
USE OrdCom   IN 1   order tag OrdCom1  ALIAS Orden
USE IteOc    IN 2   order tag IteOc1   ALIAS IteOc
USE Parmae   IN 3   order tag Parmae1  ALIAS Parma
USE IteArt   IN 4   order tag Iteart3  ALIAS Iteart
USE Pecosa   IN 5   order tag Pecosa1  ALIAS Pecosa
USE Itepec   IN 6   order tag ItePec1  ALIAS Itepec
USE Promae   IN 7   order tag Promae1  ALIAS Promae
USE maepre   IN 8   order tag maepre1  ALIAS maepre
USE Calen    in 9   order tag calen4   ALIAS CALEN
USE HojCon   IN 10  ORDER TAG HojCon1  ALIAS Hoja
USE Itehc    IN 11  ORDER TAG Itehc1   ALIAS Itehc
USE Hojmod   IN 12  ORDER TAG HOJMOD1  ALIAS HOJMOD
USE IteOc1   IN 13  ORDER TAG Iteoc11  ALIAS IteOc1
USE USUARIO  IN 20  ORDER TAG USUARIO1 ALIAS USU

*ON KEY LABEL F12 DO LIQUIDAR
*on key label F4  do imprimir
*on key label F11 do ACTUA

PUBLIC vMes,vPart,vNumOc,OQ,VMS,vfecvis,v_reg ,vTothoc , vcadena
*- Mensajes de aviso al usuario
PRIVATE vTotOC
xtotoc = 0
Vmens01 = ' Orden de Compra : REVISION '
Vmens02 = ' Registro de Ordenes de Compra '
Vmens04 = 'Dicha Orden de Compra no fue encontrado'
Vmens05 = 'No existe Orden de Compra anterior'
Vmens06 = 'No existe Orden de Compra siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Orden de Compra ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Orden de Compra ha sido anulado'
Vmens10 = 'Este Orden de Compra ya fue atendido'
Vmens11 = 'Este Orden de Compra ha sido devuelto'

PUBLIC GH
SELECT Orden
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

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE '®F4¯ Imprime  ° ®F7¯ Seguimiento  ° ®F9¯ Detalle :Item  ° ®F12¯ Visa' COLOR SCHEME 10

 DEFINE WINDOW Wind_2A FROM 10,04 TO 16,75 DOUBLE ;
 TITLE '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 20,63 TO 22,78 ;
 TITLE 'TOTAL ' COLOR SCHEME 10
 
 DEFINE WINDOW Wind_4 FROM 20,48 TO 22,78 ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_5 FROM 16,01 TO 18,79 ;
 TITLE ' Destino '

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
* DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
* DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
* DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
* DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
* ON SELECTION PAD corri  OF mMenu DO corri
* ON SELECTION PAD ingre  OF mMenu DO ingre
* ON SELECTION PAD anula  OF mMenu DO anula
* ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW Wind_1
CLEAR
 @  1, 2 SAY "       N£mero O/C:"
 @  1,40 SAY "            Fecha:"
 @  2, 2 SAY "        Proveedor:"

 @  4, 2 SAY "      Cadena Fun.:"
 @  5, 2 SAY "  Fte. Financiam.:"

 @  6, 2 SAY "          Funci¢n:"
 @  7, 2 SAY "         Programa:"
 @  8, 2 SAY "      Subprograma:"
 @  9, 2 SAY " Activid./Proyec.:"

 @ 10, 2 SAY "       Calendario:"
 @ 10,40 SAY "              H/C:"
 @ 11, 2 SAY "    Observaciones:"
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ON KEY LABEL F5
 ON KEY LABEL F8
 SELECT Orden
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WIND_1

 ON KEY LABEL F9 DO VISTA_DET
* ON KEY LABEL F7 DO PREstado WITH 'OC','m.perhc+m.numhc'

 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo+'01001','C')

 @  0,02 SAY IIF(m.Tipdoc='OK' AND m.estado#'5','O/C Visado','             ') 
 @  0,60 SAY vEstOc(m.Estado) COLOR SCHEME 02
 @  1,22 SAY m.Periodo
 @  1,25 SAY m.NumOc
 @  1,60 SAY m.FecOc
 @  1,68 SAY IIF(!EMPTY(m.FecDesp),'<'+DTOC(m.FecDesp)+'>',SPAC(10))
 @  2, 2 SAY "        Proveedor:"
 @  2,22 SAY IIF(EMPTY(m.codprv),'Sin Codigo'+space(40),val_prv(m.Codprv))

 if m.Tipo = 'M'		
    @ 3, 2 SAY "      Documento :"
    @ 3,22 say m.Memoran
 else   
	@ 3, 2 say spac(79)
 endif
 		
 @  4,22 Say val_codcad(m.codcad,m.periodo+'01001','V',22,30)
 @  5,22 SAY val_para(m.CodFte,'CODFTE','V',22,30)
 
 @  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

 @ 10,22 SAY val_para(m.NumMes,'FECMES','V',20,10)
 @ 10,60 SAY m.Perhc+' '+m.Numhc
 @ 11,22 SAY m.Observa PICTURE "@S54"

 DO VISTA_HIJO
 DO TOTAL
 RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW WIND_3
IF !EMPTY(m.NUMANU) OR !EMPTY(M.NUMREB)
   @ 0,0 SAY m.Valtot-m.Anultot picture '9,999,999.99'
else   
   @ 0,0 SAY m.Valtot picture '9,999,999.99'
endif   
return

PROCEDURE VISTA_HIJO
*-------------------
hide popup all
SELECT IteOc
SEEK m.Periodo + m.NumOc
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOc  TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo',;
   CanReq      : H= 'Cantidad' :P='99,999.99',;
   CodUni      : H= 'Uni'      :W=.F. :3,;
   Descri      : H= 'Descripci¢n' :26 :W=.F. ,;
   Numpec      : H= 'Pecs' :w=.f. ,;
   PreUni      : H= 'PreUni' :P='99,999.999' :W=.F. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.
SELE Orden
RETURN

PROCEDURE VISTA_DET
*------------------
hide popup all
SELECT IteOc
seek m.Periodo + m.NumOc
ON KEY LABEL F9 DO OBSERVA
ON KEY LABEL F7 
GO TOP
BROWSE ;
   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY m.Periodo + m.NumOc  ;
   WINDOW Wind_2 ;
   FIELDS;
   CodArt      : H= 'C¢digo',;
   CanReq      : H= 'Cantidad' :P='99,999.99',;
   CodUni      : H= 'Uni'      :W=.F. :3,;
   Descri      : H= 'Descripci¢n' :26 :W=.F. ,;
   Numpec      : H= 'Pecs' :w=.f. ,;
   PreUni      : H= 'PreUni' :P='99,999.999' :W=.F. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.
SELE Orden
DO VISTA
*ON KEY LABEL F7 DO PREstado WITH 'OC','m.perhc+m.numhc'
ON KEY LABEL F9 DO VISTA_DET
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT ORDEN
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NUMOC INTO ITEOC &&+CODFTE
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
 CC=vEstOc(ORDEN.Estado) :H='Estd' :4,;
 iteOc.descri :H='Articulo ' :36 ,;
 iteOc.coduni :H='Unid' ,;
 iteOc.Canreq :H='Cantid'
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
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 vtemp    = RECNO()
 vPeriodo = RIGHT(DTOC(DATE()),2)
 vNum_oC = '    '
 VFTE = space(2)

 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/C : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNum_OC PICTURE '!!!!' VALID vBusca()
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

PROCEDURE Actua
*--------------
ON KEY LABEL F11
if VE_PASSW('PRICE')
PRIVATE AW
AW = ALIAS()
sele iteoc
seek m.Periodo + m.NumOc  
SCAN WHILE M.PERIODO+M.NUMOC=PERIODO+NUMOC
     SELE ITEPEC
     SET ORDE TO ITEPEC7
     SEEK iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart+alltrim(iteoc.numord)
     if found()
        if rlock()
           if preuni#iteoc.preuni and tippec='O'
              replace preuni with iteoc.preuni
           endif   
        endif
        UNLOCK
     else
        do standby with 'Pecosa no hallado ...'
     endif
     sele iteoc         
ENDSCAN
sele itepec
set orde to itepec1
sele (aw)
endif
*ON KEY LABEL F11 DO ACTUA
RETURN


PROCEDURE Corri
*--------------
PRIVATE AD
OQ=.T.
AD = RECNO()
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
         DO STANDBY WITH 'La O/C ya esta Afectada'
         RETURN
	CASE Estado = '40'
         * El Orden de Compra ya tiene ?
         DO STANDBY WITH 'La O/C ya est  atendida'
         RETURN
	CASE Estado = '5'
         * El Orden de Compra ya tiene ?
         DO STANDBY WITH 'La O/C ya est  liquidada/Contabilizada'
         RETURN
 ENDCASE

 SELECT Orden
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_1
 DO PANTALLA
 vtipo = m.tipo
 =val_CODCAD(ALLT(m.codcad),m.periodo+'01001','C')
 @  1,22 GET m.Periodo  PICTURE '!!'  DISABLE
 @  1,24 SAY '-'
 @  1,25 GET m.NumOc   PICTURE '!!!!' DISABLE
 @  1,60 GET m.FecOc   PICTURE '@D'  &&VALID ALAN()

DO CASE 
   CASE vTipo = 'C'
        @ 2,22 SAY m.CodPrv  
        @ 2,28 SAY Val_prv( m.CodPrv,.T.,02,27)
   CASE vTipo = 'S' OR vtipo='M'
 		@ 2,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,02,27)
ENDCASE

 if m.Tipo = 'M'		
    @ 3, 2 SAY "        Documento :"
    @ 3,22 GET m.Memoran
 endif
 
@ 4,22 GET m.CodCad  PICTURE '!!!!' VALID val_codcad(m.codcad,m.periodo+'01001',' ',22,30) 
@ 5,22 GET m.CodFte  PICTURE '!!'   VALID val_para(m.Codfte,'CODFTE',' ',22,30) 
READ VALID val_read()
IF LASTKEY()=27 
	DO vista
	RETURN
ENDIF
@  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 10,22 GET m.Nummes	PICTURE '!!'	VALID val_para(m.NumMes,'FECMES',' ',22,15) && AND VAalcal(XTOTOC,1)
@ 11,22 GET m.Observa PICTURE "@S54"
READ VALID VAL_READ()

IF LASTKEY() # 27
   vcadena=maepre.uniges+maepre.unieje+maepre.Codfun+maepre.CodPrg+maepre.CodSpr+maepre.ActPry
   DO WHILE .T.
      Or = IIF(m.tipo='M',CORRI_HJ(),CORRIJE_HJ())
      IF LASTKEY() # 27
         IF YESNO('¨ Conforme la modificaci¢n ?')
            or=.t.
            EXIT
         ENDIF
      ELSE
         IF YESNO('¨ Cancela la modificaci¢n ?')
            or = .F.
            EXIT
         ENDIF
      ENDIF
   ENDDO
   IF Or .AND. LASTKEY() # 27 AND OQ
      codcad = m.codcad
      vTotal = 0
      SELECT ITEOC1
      SEEK m.Periodo + m.NumOc 
      SET FILT TO Periodo = m.Periodo and NumOc = m.NumOc
      GO TOP
      SCAN WHILE Periodo+NumOc = m.Periodo + m.NumOc
      	   vtotal = vtotal + valpart
		   IF RLOCK()
        	  *SELEC CALEN
              *SEEK m.periodo+vcadena+iteoc1.codcom+iteoc1.codmet+allt(m.codfte)+allt(m.nummes)+iteoc1.codpart
              *if !found()
              *	  do standby with 'No existe partida en calendarios...'
              *else	
              *	  REPLACE totoc WITH totoc+ITEOC1.valPart
              *endif	
              SELECT iteoc1
            ENDIF
            UNLOCK
      ENDSCAN     
      SET FILTER TO
      SELE ITEOC
      SEEK m.Periodo + m.NumOc 
      SET FILT TO Periodo = m.Periodo and NumOc = m.NumOc
      GO TOP
      SCAN WHILE Periodo+NumOc = m.Periodo + m.NumOc
           REPLA ALL codcad WITH m.codcad, codfte WITH m.codfte
      ENDSCAN
      m.Valtot = vtotal
      DO DESTINO
      SELECT Orden
      GO AD
      m.user = sys(0)
      m.user_fc = date()
      m.user_TP = 'C'
      M.ESTADO='00'
     GATHER MEMVAR
   ELSE
      SELECT Orden
   ENDIF
   UNLOCK
ENDIF  
SELECT Orden
GO AD
DO VISTA
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
DEACTIVATE WINDOW WIND_3
PUBLIC VTIPO,AST,REC,GH
sincot=.t.

IF escolor
   DEFINE POPUP xcot  FROM 17,55 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP xcot  FROM 17,55 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF xcot PROMPT ' \<Sin Cotizaci¢n '
DEFINE BAR 2 OF xcot PROMPT ' \<Con Cotizaci¢n '
DEFINE BAR 3 OF xcot PROMPT ' c\<On otro documento '

ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP XCOT

DO CASE
   CASE BAR() = 1
        sincot=.t.
        vTipo = 'S'
   CASE BAR() = 2
        sincot=.f.
        vTipo = 'C'
   CASE BAR() = 3
        sincot=.f.
        vTipo = 'M'
   OTHERWISE
ENDCASE
RELEASE POPUP Xcot
 
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado'
   DO VISTA
   RETURN
ENDIF

DO PANTALLA
SELECT Orden
REC=RECNO()
AST=ORDER()

SCATTER MEMVAR BLANK
m.Periodo = STR(YEAR(DATE()) - 1900,2)
= REPASA() 
m.FecEmi = date()
m.FecOc  = date()
m.Periodo = STR(YEAR(DATE()) - 1900,2)

@  1,22 GET m.periodo DISABLE
@  1,25 GET m.NumOc   PICTURE '!!!!' DISABLE && valid SIHAY() 
@  1,60 GET m.FecOc
@  2,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,2,27)  and siprv()

if vTipo = 'M'		
   @ 3, 2 SAY "      Documento :"
   @ 3,22 GET m.Memoran PICTURE '@S54'
endif

@  4,22 GET m.CodCad  PICTURE '!!!!' 	VALID VAL_CODCAD(m.codcad,m.periodo+'01001',' ',22,30)
@  5,22 GET m.Codfte  PICTURE '!!'  	VALID VAL_PARA(m.Codfte,'CODFTE',' ',22,30)

READ VALID Val_Read()
IF LASTKEY()=27
   SELE ORDEN
   GO GH
   if numoc=alltrim(m.numoc)
      dele next 1
   endif
   GO REC
   DO vista
   RETURN
ENDIF

@  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 10,22 GET m.Nummes   PICTURE '!!' VALID VAL_PARA(m.Nummes,'FECMES',' ',22,15)
@ 11,22 GET m.Observa  PICTURE "@S54"

READ VALID Val_Read()
SET CONFIRM ON

IF LASTKEY() # 27
   	vcadena=maepre.uniges+maepre.unieje+maepre.Codfun+maepre.CodPrg+maepre.CodSpr+maepre.ActPry
   	DO WHILE .T.
      Ok = TRABAJA_HJ()
      IF LASTKEY() # 27 AND Ok
          IF YESNO('¨ Confirme el ingreso ?')
              EXIT
           ELSE
              OK = .F.
              EXIT
          ENDIF
       ELSE
          DO STANDBY WITH ' Cancelado el Ingreso '
          ok = .F.
          EXIT
       ENDIF
    ENDDO

	IF Ok .AND. LASTKEY() # 27
    	*- Aumento el correlativo
		DO ACTUALIZA with 1
        SELECT ITEOC
        SEEK m.Periodo+m.NumOc 
        If vtipo = 'S' or vtipo = 'C'
          	 ok1 = corri_hj()
        endif
        SELECT ITEOC1
        SEEK m.Periodo + m.NumOc 
        vTotal = 0
        SCAN WHILE Periodo = m.Periodo and NumOc = m.NumOc  
               vtotal = vtotal + valPart
        ENDSCAN
        DO DESTINO
        SELECT ORDEN
        GO GH
        IF !empty(m.Numoc)
             m.Valtot = vtotal
             m.tipo   = vtipo
             vImpr = m.Periodo+m.NumOc+ALLTRIM(m.Codfte)
      	     M.user = sys(0)
             m.user_fc = date()
             m.user_TP = 'I'
             m.Estado = '00'
             GATHER MEMVAR
        ENDIF
        if !found()
              do standby with 'El N£mero de O/C ya ha sido generado por '+alltrim(user)+' Revise..'
        endif
    ELSE    
       SELECT Iteoc
       SEEK m.periodo+m.numoc
       IF FOUND()
       		SCAN WHILE m.Periodo = Periodo  .and.  m.Numoc = Numoc 
            	IF F_LOCK(1)
                	DELETE NEXT 1
            	ENDIF
          	ENDSCAN
          	UNLOCK
       ENDIF   	
       SELE ORDEN
       GO GH
       IF numoc=alltrim(m.numoc)
       		dele next 1
       ENDIF   
       GO REC
    ENDIF
ELSE
	DO STANDBY WITH 'Proceso cancelado'
    SELECT ORDEN
    GO GH
    if numoc=alltrim(m.numoc)
    	dele next 1
    endif   
    GO REC
ENDIF
SELECT ORDEN
DO VISTA
RETURN

PROCEDURE siprv
*--------------
parameter vfun
vfun = .t.
if !v_reg='VG'
   do standby with 'El Proveedor no esta Regularizado...Observaci¢n'
endif
return vfun   

PROCEDURE TRABAJA_HJ
*-------------------

PRIVATE vfun
vFun=.t.
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°° ®F2¯ Busca Pec °°° ®F11¯ Marca û °°° ®F12¯ Desmarca û °°° ®F10¯ Sale °°°°°'
DO Logos WITH Rotulo1,vTempo

ON KEY LABEL F2  DO BUSPEC
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F11 DO MARCA
ON KEY LABEL F12 DO DESMARCA

SELECT ITEPEC
SET RELATION TO codart into iteart
SET RELATION TO Periodo+NumPec INTO Pecosa additive
GO TOP
IF !EOF()
	DO CASE
       CASE vTipo = 'S'
            SET ORDER TO ITEPEC5
            ORANT=ORDER()
            VESTA='00'
            SET FILT TO ESTADO='00' AND CODFTE=ALLTRIM(CODFTE) AND TIPPEC$'OT'
            ON KEY LABEL F9 DO VISOBS
            GO TOP
            BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_2 FIELD ;
                orden         : H= ''          :W=.F. ,;
                numord        : H= 'Od'         :W=.F. ,;
   				xx=' '+NumPec : H= 'Pecosa'     :W=.F. ,;
   				codfte        : H= 'Fte'        :W=.F. ,;
   				Descri        : H= 'Descripci¢n':W=.F. :25 ,;
    			Coduni        : H= 'Uni'        :W=.F. :3  ,;
				Canreq        : H= 'Cantid'     :W=.F. :P='99,999.999' ,;
				PreUni        : H= 'Costo'      :P='99,999.999':V=Preuni>0 ,;
    			X=ROUND(CanReq*PreUni,2)        :W=.F. :H='Total' :P='99,999.99' 
    		ON KEY LABEL F9     			
       CASE vTipo = 'C'    
            SET ORDER TO ITEPEC6
            ORANT=ORDER()            
            VESTA='20'            
            SET FILTER TO codprv=alltrim(m.codprv) &&A¤adir Filtro
            ON KEY LABEL F9 DO VISOBS
            GO TOP      
            IF !EOF()
            BROWSE NOAPPEND NODELETE NOMENU WINDOW Wind_2 FIELD ;
                orden         : H= ''         :W=.F. ,;
                numord        : H= 'Od'        :W=.F. ,;
   				xx=' '+NumPec : H= 'Pecosa'    :W=.F. ,;
				CodPrv        : H= 'Prv'       :W=.F. ,;
				Descri        : H= 'Descripci¢n' :24 :W=.F.  ,;
				Coduni        : H= 'Uni'       :W=.F. :3,;
				Canreq        : H= 'Cantidad'  :P='99,999.999' :W=.F. ,;
				PreUni        : H= 'Costo'     :P='99,999.999' :V=Preuni>0 ,;
				X=ROUND(CanReq*PreUni,2) :H='Total'     :P='99,999.99' :W=.F.
			ELSE
			  DO STANDBY WITH 'No existe Art¡culos a procesar ...'
			ENDIF	
            ON KEY LABEL F9 
       CASE vTipo = 'M'				
			DO CORRI_HJ
	ENDCASE
	set filter to 
    IF LASTKEY()=27
       vFun=.f.
    ENDIF   
 ELSE
    DO STANDBY WITH 'No existe Art¡culos a Procesar '
    vFun=.f.
 ENDIF
 set rela to
 SET ORDE TO ITEPEC1
 SET FILTER TO
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Orden
 ON KEY LABEL F10
 ON KEY LABEL F11 
 ON KEY LABEL F12 DO LIQUIDAR
 ON KEY LABEL F2 
RETURN vFun

PROCEDURE ACTUALIZA
*------------------
PARAMETER VOPCION
PRIVATE AW,VORD,VOPCION,ad
AW = ALIAS()
SELE itepec
VORD = ORDER()
GO TOP
IF !EOF()
   ACTIVATE WINDOW STANDBY
   @ 01,10 SAY 'Espere un momento........' 
   SET ORDER TO ITEPEC15
   seek m.periodo+m.numoc
   SET FILT TO Periodo = m.Periodo and Numoc = m.Numoc          
   SCAN while periodo+numoc=m.periodo+m.numoc 
        if ESTADO=iif(vtipo= 'C','20','00')
           vantfte = itepec.codfte
           REPLACE estado with '30', Antfte with vantfte, NewFte with m.codfte,codfte with m.codfte
           ad=recno()
		   select pecosa
		   set orde to pecosa1
		   seek itepec.periodo+itepec.numpec &&+itepec.codfte+itepec.codcad
		   if found()
		      replace codfte with m.codfte
		   else
		      do standby WITH "Error:1001 No grabo en Pecosa"
		   endif   
		   IF vOpcion = 1
	          DO Agreg_item  && agrega  en iteoc
	       ENDIF   	
       	   sele itepec
       	   go ad
       	endif   
   endscan
   GO TOP
   IF RLOCK()
   	  REPLACE ALL codfte with m.codfte
   ENDIF
   UNLOCK
   DEACTIVATE WINDOW STANDBY
   SELE ITEPEC
   SET FILTER TO
endif
SET ORDE TO (VORD)
SELE (AW)
RETURN

PROCEDURE ACTUALIZ_C
*------------------
PARAMETER VOPCION
PRIVATE AW,VORD,VOPCION,ad
AW = ALIAS()
SELE ITEPEC
VORD = ORDER()
GO TOP
IF !EOF()
   ACTIVATE WINDOW STANDBY
   @ 01,10 SAY 'Espere un momento........' 
   SET ORDER TO ITEPEC15
   SET FILT TO Periodo = m.Periodo and Numoc = m.Numoc 
   GO TOP
   SCAN while periodo+numoc=m.periodo+m.numoc 
        if ESTADO=iif(vtipo= 'C','20','00')
           vantfte = itepec.codfte
       	   REPLACE estado with '30',codfte with m.codfte
		   ad=recno()
		   select pecosa
		   set orde to pecosa1
		   seek itepec.periodo+itepec.numpec
		   if found()
		      replace codfte with m.codfte
		   else
		      do standby WITH "Error:1001 No grabo en Pecosa"
		   endif   
      	   DO AGREG_ITEM  && agrega  en iteoc
       	   sele itepec
       	   go ad
       	endif   
    endscan
    GO TOP
    REPLACE ALL codfte with m.codfte
    DEACTIVATE WINDOW STANDBY
endif
SET ORDE TO (VORD)
SELE (AW)
RETURN

PROCEDURE ACTUALIZA_X
*------------------
PARAMETER VOPCION
PRIVATE AW,VORD,VOPCION,ad
AW = ALIAS()
SELE ITEPEC
VORD = ORDER()
GO TOP
IF !EOF()
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........' 
   SET ORDER TO ITEPEC1
   SET FILT TO orden + periodo + numoc = 'û' + m.periodo + m.numoc AND ESTADO=iif(vtipo= 'C','20','00')
   GO TOP
   SCAN 
        vantfte = itepec.codfte
		ad=recno()
		select pecosa
		set orde to pecosa1
		seek itepec.periodo+itepec.numpec
		if found()
		   replace codfte with m.codfte
		else
		  do standby WITH "Error:1001 No grabo en Pecosa"
		endif   
      	DO AGREG_ITEM  && agrega  en iteoc
       	sele itepec
       	go ad
   endscan
   GO TOP
   REPLACE ALL codfte with m.codfte
   DEACTIVATE WINDOW STANDBY
endif
SET FILTER TO 
SET ORDE TO (VORD)
SELE (AW)
RETURN

PROCEDURE AAA_Hj
*-----------------

ON KEY LABEL F7
ON KEY LABEL F9
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo

ON KEY LABEL F5  DO Agreg_Ic
ON KEY LABEL F8  DO Elimi_Ic
ON KEY LABEL F10 KEYBOARD CHR(23)

SELE ITEOC
set orde to iteoc1
seek m.Periodo + m.NumOc  &&+m.Codfte
if !found()
   do agreg_ic
endif   
if vtipo='M'
	BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumOc FIELD ;
   		CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt,.F.):F :W=EMPTY(CodArt) ,;
   		Descri      : H= 'Descripci¢n' :29 :W=.F. ,;
   		CanReq      : H= 'Cantidad' :P='99,999.999' ,;
   		CodUni      : H= 'Uni'      :W=.F. :3,;
   		PreUni      : H= 'PreUni' :P='99,999.999' :V=ING_VAL():F,;
   		X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='999,999.99' :W=.F.
else
	BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumOc FIELD ;
   		CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt,.F.):F :W=EMPTY(CodArt) ,;
   		Descri      : H= 'Descripci¢n' :29 :W=.F. ,;
   		CanReq      : H= 'Cantidad' :P='99,999.999' :R,;
   		CodUni      : H= 'Uni'      :W=.F. :3,;
   		PreUni      : H= 'PreUni' :P='99,999.999':R,;
   		X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='999,999.99' :W=.F.
endif

 SELE ITEOC
 seek m.Periodo + m.NumOc
 vTothoc = 0
 SCAN WHILE Periodo +NumOc=m.Periodo + m.NumOc
	vTothoc = vTothoc + valtot
 ENDSCAN 
 seek m.Periodo + m.NumOc
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F9  DO VISTA_DET
* ON KEY LABEL F7  DO PREstado WITH 'OC','m.perhc+m.numhc' 
 ON KEY LABEL F10
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Orden
 if lastkey()=27
    return .f.
 endif
 set relation to
 do trab_hijo && se adicionan los componentes, metas y partidas
 RETURN
 
PROCEDURE CORRIJE_HJ
*---------------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg_oc
ON KEY LABEL F8  DO Elimi_oc
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE ITEOC
SEEK m.Periodo + m.NumOc
IF !FOUND()
	DO Agreg_oc
ENDIF
BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumOc  FIELD ;
   CodArt      : H= 'C¢digo'   :W=.F.,;
   Descri      : H= 'Descripci¢n' :29 :W=.F. ,;
   CanReq      : H= 'Cantidad' :P='99,999.999' :W=.F.,;
   CodUni      : H= 'Uni'      :W=.F. :3,;
   PreUni      : H= 'PreUni' :P='99,999.999' :w=.f. ,;
   X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='99,999.99' :W=.F.
     
ON KEY LABEL F5  DO Agreg_ITEM
ON KEY LABEL F8  DO Elimi_ITEM
ON KEY LABEL F10
sele iteoc
SEEK m.Periodo + m.NumOc  
vTothoc = 0
SCAN WHILE M.PERIODO+M.NUMOC=PERIODO+NUMOC
	 vTothoc = vTothoc + valtot
     SELE ITEPEC
     SET ORDE TO ITEPEC7
     SEEK iteoc.periodo+iteoc.numoc &&+iteoc.codfte+iteoc.codart+alltrim(iteoc.numord)
     IF found()
        IF rlock()
           replace preuni with iteoc.preuni
        ENDIF
        UNLOCK
     ELSE
        do standby with 'Pecosa no hallado ...'
     ENDIF
     sele iteoc         
ENDSCAN

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Orden
 if lastkey()=27
    OQ =.F.
    return .f.
 endif
 do trab_hijo && se adicionan los componentes, metas y partidas
 RETURN

PROCEDURE agreg_OC
*-----------------
VALIAS = ALIAS()
OQ=TRABAJA_HJ()
IF OQ
  DO ACTUALIZ_C WITH 1
ENDIF   
ON KEY LABEL F5  DO Agreg_oc
ON KEY LABEL F8  DO Elimi_oc
ON KEY LABEL F10 KEYBOARD CHR(23)
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SELECT (VALIAS)
return OQ
 
PROCEDURE ELIMI_OC
*-----------------
PRIVATE vFun,vKey
vfun = .t.
SELE ITEPEC
SET FILT TO
SET ORDE TO ITEPEC7
GO TOP

seek iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart+iteoc.numord

if found()
  IF RLOCK()
   	REPLACE orden with ' ',estado with iif(vtipo= 'C','20','00'),;
   	        NUMOC WITH SPACE(4)
  ENDIF
  UNLOCK  
else
  do standby with 'Advertencia:No es ubicado la Pecosa,Revise'
  vFun=.f.
ENDif

SELECT ITEoc
IF RLOCK() and vfun
   DELE NEXT 1
ENDIF
RETURN

PROCEDURE ELIMI_C
*----------------
SELE ITEPEC
vTemp=Recno()
SET ORDE TO ITEPEC3
SET FILTER TO ITEPEC.CODART=ITESC.CODART AND ITEPEC.NUMSC=M.NUMSC
GO TOP
SCAN
  IF RLOCK()
     REPLACE LLAVE WITH '.' ESTADO WITH '00',NUMSC WITH '    '
  ENDIF
  UNLOCK
ENDSCAN
SET FILTER TO
SET ORDE TO ITEPEC2

SELECT ITESC
IF RLOCK()
  DELE NEXT 1
ENDIF
RETURN

PROCEDURE Anula
*--------------
 SELE orden
 PRIVATE vFun,vKey
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚sta Orden de Compra ?')
 IF vElimina
    sele iteoc
    SCAN WHILE periodo+numoc+codfte = orden.periodo + orden.numoc + orden.codfte
         vArt = iteoc.codart
         vPrv = orden.codprv
		 vkey = iteoc.periodo+iteoc.numoc+iteoc.codfte
		 vfun = .t.
		 SELE ITEPEC
		 SET ORDE TO 7
		 vTemp=Recno()
		 vkey = iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart
     	 SEEK VKEY

		 If FOUND()
		 	IF RLOCK()
		   		REPLACE orden with ' ',estado with '00',;
   	       		NUMOC WITH SPACE(4)
   	       	ELSE 	
     	       	do standby with 'Advertencia:La Pecosa no se habilit¢'
     			vFun=.f.     	       	
			ENDIF
			UNLOCK
		 else
			do standby with 'Advertencia:No es ubicado la Pecosa,Revise'
			vFun=.f.
		 ENDif
         SELECT ITEoc
		 IF RLOCK() and vfun
		    REPLA ESTADO WITH '99'
		 ENDIF
		 UNLOCK
    ENDSCAN
    SELE ORDEN
    if vfun
    IF  RLOCK()
       REPLACE ESTADO WITH '99' ,FECVER WITH DATE(),user WITH sys(0),user_fc WITH date(),user_TP WITH 'A'
    ENDIF
    endif
    UNLOCK
    DO Vista
 ENDIF
 RETURN

PROCEDURE Agreg_Ic
*-----------------
PRIVATE VFUN
VFUN=.F.
SELE ITEOC
seek m.periodo+m.numoc
SET FILT TO Periodo = m.Periodo and Numoc = m.Numoc       
if !found()
   IF F_appd()
      REPLACE NumOc   WITH m.NumOc ,;
              Periodo WITH m.Periodo ,;
              CodCad  WITH m.CodCad ,;
              CodFte  WITH m.Codfte ,;
              Codart  WITH '     ',;
              descri  WITH spac(60) ,;
              canreQ  WITH 0 ,;
              ESTADO  WITH '00',;
              fecoc   WITH m.fecoc
      VFUN=.T.
   ENDIF
   UNLOCK 
else 
   do standby with 'El N£mero de O/C ya ha sido generado por '+alltrim(user)+' Revise..'
   vfun=.f.
endif
RETURN VFUN

PROCEDURE ELIMI_ic
*-----------------
PRIVATE VFUN
VFUN = .F.
SELE ITEPEC
SET ORDE TO ITEPEC7
SEEK iteoc.periodo+iteoc.numoc+iteoc.codfte+iteoc.codart+iteoc.numord
IF FOUND()
   IF RLOCK()
     REPLACE orden WITH ' ' ESTADO WITH '20',NUMoc WITH '    '
     VFUN = .T.
   ENDIF
   UNLOCK
ELSE
   IF M.TIPO='M'
     VFUN = .T.
   ENDIF
ENDIF  
SELECT ITEoc
IF VFUN
   IF RLOCK()
      DELE NEXT 1
   ENDIF
   UNLOCK
ELSE
   DO STANDBY WITH 'No se ubic¢ el Pecosa ...'    
ENDIF

RETURN

PROCEDURE Agreg_Item
*-------------------
PRIVATE VFUN
VFUN=.F.
SELEC ITEOC
IF F_appd()
  REPLACE  NumOc  WITH m.NumOc ,;
         Periodo  WITH m.Periodo ,;
          CodCad  WITH m.CodCad ,;
          CodArt  WITH Itepec.CodArt ,;
		  CodPart WITH Iteoc1.CodPart ,;
          CodFte  WITH m.Codfte ,;
          CanReq  WITH Itepec.Canreq ,;
          CodUni  WITH Itepec.Coduni ,;
          DesCri  WITH Itepec.Descri ,;
          Preuni  WITH Itepec.Preuni ,;
          Perpec  WITH PADL(MONTH(Pecosa.FECpec),2,"0"),;
          numpec  WITH Itepec.numpec ,;
          CodDep  WITH Pecosa.Coddep ,;
          Valtot  WITH Preuni*Canreq ,;
          ESTADO  WITH '00',;
          fecoc   WITH m.fecoc,;
   	      observa WITH itepec.observa,;
   	      numord  WITH itepec.numord
  VFUN=.T.
ENDIF
UNLOCK 
RETURN VFUN

PROCEDURE VALER
*--------------
SELECT Parma
SEEK 'CORREL'+iif(alltrim(m.CodFte)='TRN','ORDTRN','ORDPRP')
 m.numOc =padl(alltrim(str(Parma.NumEnt + 1)),4,'0')
RETURN .T.

PROCEDURE VALERX
*---------------
SELECT Parma
SEEK 'CORRELORDENC'
vNumOc = Parma.NumEnt + 1
DO WHILE .T.
	m.numOc =padl(alltrim(str(vNumoc)),4,'0')
	@  1,25 GET m.NumOc   DISABLE    
	SELE ORDEN
	SEEK m.periodo+m.NumOc &&+alltrim(m.codfte)
	IF FOUND()
	    DO STANDBY WITH 'La O/C ya ha sido generada'
        vnumoc = vnumoc + 1
        LOOP
    ELSE
        EXIT    
    ENDIF        
ENDDO
RETURN .T.

PROCEDURE VALMES
*---------------
IF alltrim(m.Nummes)#vmes
   do standby WITH 'No Coincide con Mes del Calendario'
endif   
return

PROCEDURE NUMERA
*--------------
SELE ORDEN
SEEK m.periodo+m.NumOc &&+alltrim(m.codfte)
IF FOUND()
    DO STANDBY WITH 'La O/C ya ha sido generada'
    RETURN .F.
ELSE
	RETURN .T.
ENDIF	

PROCEDURE AS_PRV
*---------------
vAntprv = codprv
return .t.

PROCEDURE ORD_1
*--------------
SELE ITEPEC
SET FILT TO
SET ORDE TO (ORANT)
RETURN

PROCEDURE ORD_2
*--------------
SELE ITEPEC
SET ORDE TO 8
SET FILT TO ESTADO=VESTA
RETURN

PROCEDURE BUSPEC
*---------------
ACTIVATE WINDOW WIND_4
VNUMPEC='0000'
@ 0,1 say 'Ingrese N§ Pecosa => ' GET VNUMPEC PICTURE '!!!!' 
READ
DEACTIVATE WINDOW WIND_4
SEEK m.PERIODO+ALLTRIM(VNUMPEC)
RETURN
 
PROCEDURE Marca
*--------------
REPLACE orden with 'û',numoc with m.numoc
return

PROCEDURE DesMarca
*-----------------
REPLACE orden with ' ',numoc with spac(4)
return

PROCEDURE ING_VAL
*----------------
IF RLOCK()
	REPLACE  Valtot with Preuni*Canreq 
ELSE
    RETURN .F.	
ENDIF
RETUR .T. 


PROCEDURE ASSIG
*--------------
vcli    = orden.NUMoc
vAno    = orden.PERIODO
vfte    = orden.codfte
RETURN  .T.

PROCEDURE VO
*-----------
vCli=Padl(alltrim(vCli),4,'0')
RETURN .T.

PROCEDURE Valord
*--------------
SELECT ORDEN
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
SEEK vano+VCLI &&+ALLTRIM(vfte)
if !found()
    SET SKIP TO ITEOC
    GO TOP
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
	IF LASTKEY()=27
	   SELE ORDEN
	   SET RELATION TO
	ENDIF
	SHOW MENU mMenu
	ON KEY LABEL F10
	SELE ORDEN
 endif
 vAno = Orden.Periodo	 
 VCli = Orden.NumOC
 vFte = Orden.Codfte
 RETURN 

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F4
  ON KEY LABEL F9 
  ON KEY LABEL F7  
  HIDE WINDOW WIND_1
  DEACTIVATE MENU
  RETURN

PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW wind_0
  RELEASE WINDOW wind_1
  RELEASE WINDOW wind_3
  RELEASE WINDOW wind_4
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

function valprv
*--------------
private xx, vfun
vfun = .f.
m.codprv= iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.,2,26)
if xx
   return .t.
endif
return vfun

function valOc
*-------------
parameter vnumOc
private vfun
vfun = .t.
m.numOc =padl(alltrim(str(vnumOc,4)),4,'0')
if m.numOc  = '0000' or empty(m.numOc)
   vfun = .f.
endif
return vfun

FUNCTION Observa
*---------------
vAlias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle O/C ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa WINDOW OBSERVA

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
   DEFINE WINDOW Observa FROM 03,18 TO 20,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle O/C ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERVa NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.

procedure itep
*-------------
as=alias()
sele pecosa
seek iteoc.periodo+iteoc.numpec+iteoc.codfte
OK1 = PECOSA.DESTINO
sele (as)
retu (ok1)

procedure DESTINO
*---------------
PRIVATE AZ
aZ=alias()
SELE ITEOC
SEEK m.Periodo + m.NumOc &&+ ALLTRIM(m.CodFte)
select pecosa
seek iteoc.periodo+iteoc.numpec+iteoc.codfte
m.destino = IIF(EMPTY(m.destino),PECOSA.DESTINO,m.destino)
sele (aZ)
ACTIVATE WINDOW wind_5
@ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
READ
DEACTIVATE WINDOW wind_5
retuR

PROCEDURE ALAN
*-------------
RETU
AX=ALIAS()
SELE ALAN
SEEK ALLTRIM(M.CODFTE)+ALLTRIM(M.NUMOC)
IF FOUND()
   M.NUMMES = ALAN.NUMMES
ELSE
   M.NUMMES = '00'
ENDIF      
SELE (AX)
RETURN

FUNCTION Val_ArtC  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo
_campo = VARREAD()
ORD=ORDER()
mAlias = ALIAS()
SELECT IteArt
GO TOP
_OldWnd = WOUTPUT()
v_Fun=.f.
IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 2
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F8 DO BorrDet
*      ON KEY LABEL F5 DO Agr2Det
      ON KEY LABEL F2 DO FunBusDet
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo' :W=.F. ,;
        Descri   :H='Nombre':70  ,;
        CodUni   :H='Unidad':7   
      vORD = RECNO()  
      GO TOP
      SCAN WHILE EMPTY(DESCRI)
           IF RLOCK()
              DELETE NEXT 1
           ENDIF    
      ENDSCAN  
      GO TOP
      GO vord
      ON KEY LABEL F10
      ON KEY LABEL F8
      ON KEY LABEL F5
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1
      
      IF Lastkey()=27
         V_FUN = .f.
      ELSE
         xcod = CodArt
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod
         ENDIF
         v_fun = .T.
      ENDIF
   ENDIF
ENDIf
IF v_Fun
   SELECT iteoc
   IF RLOCK()
      REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
   ENDIF
endif
SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_iC
ON KEY LABEL F8  DO Elimi_iC
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
RETURN v_fun

PROCEDURE Titulo
*---------------
pagina = pagina  + 1
vTitulo = ' ORDEN DE COMPRA '
@ 0,1   SAY CHR(18)+CHR(14)
@ 0,15 SAY VTITULO+CHR(18)

@ 1,2   SAY cia
@ 1,70  SAY 'P g.'
@ 1,74  SAY pagina   PICTURE '##,###'

@ 2,2   SAY SUBC
@ 2,72  SAY DATE()
@ 3,04  SAY '                  ÚÄÄÄÄÄÄÄÄÄ¿'
@ 4,04  SAY 'ORDEN DE COMPRA : ³ '+Orden.Numoc+'.'+Orden.Nummes+' ³'
@ 4,62  SAY 'Estado : '+IIF(Orden.Estado='00','Emitido  ',IIF(Orden.Estado='20','Afectado ',IIF(Orden.Estado='99','Anulado',IIF(Orden.Estado='50','Liquidado',' Atendido'))))

@ 5,04  SAY '                  ÀÄÄÄÄÄÄÄÄÄÙ'
@ 5,59  say 'Fecha O/C : '+DTOC(Orden.fecoC)

@ 6,66  say IIF(!EMPTY(Orden.NUMHC),'H/C :','  ')+IIF(!EMPTY(Orden.NUMHC),Orden.PERHC+"."+Orden.NUMHC,' ')

@ 7,02  SAY ' SE¥OR(ES) : '+CHR(27)+'G'+val_prv(Orden.Codprv)+CHR(27)+'H'
@ 8,02  SAY ' DIRECCION : '+Val_Fun('ProMae','CodPrv','Dirpro',Orden.Codprv)
@ 9,02  SAY ' -Le agradecemos enviar a nuestro Almac‚n : Av Chirichigno S/N Urb. San Eduardo'
@ 10,1 say chr(15)
@ 10,06 say IIF(SUBST(Orden.OBSERVA,1,4)='REF:','-'+CHR(15)+Orden.OBSERVA+CHR(18),' ')+CHR(18)
@ 11,02 say ' -Facturar a nombre de : REGION GRAU - R.U.C. N§ 15447745'+CHR(15)
@ 12,02 SAY IIF(ORDEN.TIPO='M',"REFERENCIA : "+MEMORAN,CHR(18)+VALCCC()+CHR(15))
@ 13,5 SAY 'ÚÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 14,5 SAY '³Pecosa ³  C¢digo   ³Unidad³                             Art¡culo                                    ³Cantidad³       PreUni³       Total  ³'
@ 15,5 SAY 'ÀÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
linea = 16
RETURN


PROCEDURE sumario
*----------------
@ 46,1   SAY CHR(18)

@ 47,07 SAY 'ÚÄ¿                                       ÚÄ¿'
@ 48,07 SAY '³1³                                       ³3³'
@ 49,07 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-        ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-'
@ 50,07 say '     Director de Abastecimiento                   Jefe de Almacen '

@ 53,07 SAY 'ÚÄ¿                                                RECIBI CONFORME:  '
@ 54,07 SAY '³2³                                              ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿ '
@ 55,07 SAY 'ÀÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ-               ³ Fecha:   /  /   ³ '
@ 56,07 say '       Jefe de Adquisiciones                     ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ '

@ 57,56 SAY chr(15)+'Elaborado por: '+Vusua(user)
@ 59, 1 say chr(15)+'Nota: Esta Orden es Nula sin la firma mancomunada del Director de Abastecimientos y el Jefe de Adquisiciones .Nos reservamos el derecho de'
@ 60, 1 say chr(15)+'      devolver la mercader¡a que no est‚ de acuerdo con nuestras especificaciones.'+chr(18)+chr(12)
linea = 1
RETURN

PROCEDURE LIQUIDAR
*-----------------
PRIVATE OK
OK=VE_PASSW('OBTC')
IF OK
DO CASE
   CASE ESTADO = '00'
        DO STANDBY WITH 'El Documento a£n no est  afectado'
        do vista
   CASE ESTADO = '50'
        DO STANDBY WITH 'El Documento ya est  liquidado'
        do vista
        return
   CASE ESTADO = '99'
        DO STANDBY WITH 'El Documento ya est  Anulado'
        do vista
        return
ENDCA            
IF YESNO('Est  seguro de Visar esta O/C')
   activate WINDOW STANDBY
   vfecvis=date() 
   @ 1,1 say 'Ingrese Fecha-> ' get vfecvis
   read
   deactivate WINDOW STANDBY
   IF RLOCK()
      REPLACE tipdoc WITH 'OK',FECVIS WITH vfecvis
   ENDIF 
else 
   DO STANDBY WITH 'Proceso Cancelado'
endif
do vista
ENDIF
retu

FUNCTION Val_cal
*----------------
 PARAMETERS xtotoc
 vTotal = 0
 SELECT Itepec
 VORD = ORDER()
 IF !EOF()
   SET ORDER TO ITEPEC1
   GO TOP
 	SCAN FOR orden+periodo+numoc = 'û'+m.periodo+m.numoc AND ESTADO='00' &&estado#'30'
 		vtotal = vtotal + Preuni*Canreq
 	ENDSCAN
 	IF xTotOc < vtotal
		SET ORDER TO (vord)
		xtotoc = 0
		RETURN .F.
 	ENDIF
 	SET ORDER TO (vord)
 ENDIF
 RETURN .T.


Function repasa
*--------------
vfun = .t.
vrec = recno()
vali = alias()
select ORDEN
set orde to ORDCOM1
SET FILTER TO 
SEEK '970800'
AS = RECNO()
IF FOUND()
   go as
ENDIF  
numr = 0800
do while .t.
   if val(numOC)=numr
      numr = numr + 1
      skip
      loop
   else 
      exit   
   endif   
enddo

m.numoc=padl(alltrim(str(numr,4)),4,'0')
if m.numoc = '0000' or empty(m.numoc)
   vfun = .f.
else
	SELE ORDEN
	IF F_APPD() 
    	 replace periodo with m.periodo,numoc with m.numoc, estado with '00', user_tp with 'E',user with sys(0)
     	gh = recno()
	ENDIF 
	UNLOCK
endif

SELECT Parma
SEEK 'CORRELORDENC'
REPLACE NumEnt WITH NumR 
sele (vali)
return vfun


procedure PRestado
*-----------------
use in 11
use in 12
USE Cheque   IN 13  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 14  ORDER TAG Compag1  ALIAS compag
DO Estado WITH 'OC','m.perhc+m.numhc'
use in 13
use in 14
USE Itehc    IN 11  ORDER TAG Itehc1   ALIAS Itehc
USE HOJMOD   IN 12  ORDER TAG HOJMOD1  ALIAS HOJMOD
RETURN


PROCEDURE VALCCC
*---------------
PRIVATE AS,vnumccc
USE Solcot  IN 10  ORDER TAG Solcot1  ALIAS solcot
USE IteSC   IN 11  ORDER TAG Itesc1   ALIAS Itesc
USE IN 12 

AS=ALIAS()
VCC=ITEOC.NUMPEC+ITEOC.CODART+ALLTRIM(ITEOC.NUMORD)
SELEct ITESC
SET RELA TO PERIODO+NUMSC INTO SOLCOT
SET ORDE TO ITESC4
SEEK VCC
VNUMCCC=IIf(FOUND(),"   CUADRO COMPARATIVO : "+SOLCOT.NUMCCC,'   ')
set rela to
SELEct (as)

USE HojCon   IN 10  ORDER TAG HojCon1  ALIAS Hoja
USE Itehc    IN 11  ORDER TAG Itehc1   ALIAS Itehc
USE HOJMOD   IN 12  ORDER TAG HOJMOD1  ALIAS HOJMOD

return vnumccc

procedure busprv
*---------------
private ali,vkey
ali=alias()
vkey = codprv
select promae
SEEK vkey
if found()
   if estado='VG'
      vfun = .F.
   else
      vfun = .T.
   endif     
else   
   vfun = .T.
endif
select (ali)
return vfun

PROCEDURE Vusua
*----------------
PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

PROCEDURE Trab_hijo            && Revisi¢n de BD en browse CUANDO # O/?
*------------------
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°F5->Agregar°°°°°°°°°°F8->Eliminar°°°°°°°°°°°F10->Terminar°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agre_ItOC
ON KEY LABEL F8  DO elim_itoc
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteoc1
SET ORDE TO ITEOC11
seek m.periodo +m.Numoc
if !found()
	DO Agre_ItOC
else
	DO Borra_Oc	
endif
BROWSE WINDOW Wind_2A NOAPPEND NODELETE NOMENU key m.periodo +m.Numoc FIELDS ;
       CODCOM	: H= 'Componente.':V=Val_comp(m.periodo+maEPRE.uniges+maepre.unieje+m.codcad,codcom,'codcom'):F ,;
       CODMET   : H= 'Meta' :V=Val_meta(m.periodo+maEPRE.uniges+maepre.unieje+m.codcad,CODCOM+codmet,'codmet'):F ,;
       CodPart  : H= 'Partida' :F ,;
       aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,40)) :H='Descripci¢n':40,;
       ValPart  : H='Monto' :P='99,999,999.99'
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo

* CodPart  : H= 'Partida' :V=VAL_cale(codpart,m.periodo+vcadena+codcom+codmet+allt(codfte)+allt(m.nummes),'codpart'):F ,;

SELECT iteoc1
seek m.periodo +m.Numoc
vtotcom = 0
SCAN WHILE PERIODO+NUMOC=m.periodo +m.Numoc
	vtotcom = vtotcom + valpart
	IF VALPART=0
		DELETE NEXT 1
	ENDIF
ENDSCAN	
IF vtotcom <> vTothoc
	DO STANDBY WITH ('Total Compon. es diferente al total de  O/C...Revise...')
ENDIF	
SHOW MENU mMenu
ON KEY LABEL F5
ON KEY LABEL F7
ON KEY LABEL F8
ON KEY LABEL F10
SET FILTER TO
SELE (AS)
if lastkey()=27
   return .f.
endif
RETURN .t.

PROCEDURE Agre_ItOC
*-------------------
sele iteoc1
vp = codpart
vuniges = MAEPRE.uniges
vunieje = MAEPRE.unieje
vcodcom = MAEPRE.codcom
vcodmet = MAEPRE.codmet
IF F_appd()
   REPLACE Periodo WITH m.Periodo ,;
           Numoc   WITH m.Numoc ,;
           CodCad  WITH m.CodCad,;
           uniges  WITH vuniges,;
           unieje  WITH vunieje,;
           Codcom  WITH vCodcom,;
           Codmet  WITH vCodmet,;
           Codpart WITH Vp ,;
           valpart WITH ITEOC1.valpart
ENDIF
RETURN .T.

PROCEDURE elim_itoc
*-------------------
sele iteoc1
if rlock()
   delete next 1
endif
return

PROCEDURE Borra_oc            && Borra los items de itehc
*----------------
AX=ALIAS()
SELECT IteOc1
SEEK m.Periodo + m.NumOc
if rlock()
   SCAN WHILE Periodo+NumOc=m.Periodo+m.NumOc
   		* Elimina las asignaciones efectuadas en el calendario
   		SELECT CALEN
        SEEK m.periodo+vcadena+iteoc1.codcom+iteoc1.codmet+allt(m.codfte)+allt(m.nummes)+iteoc1.codpart
   		REPLACE TotOC WITH TotOC-iteoc1.Valpart
		SELECT ITEoc1
   ENDSCAN
endif
sele (AX)
retur

PROCEDURE SIHAY
*--------------
PRIVATE AS
AS=RECNO()
SEEK m.periodo+m.NumOc 
if found() AND RECNO()#AS
   do standby with 'La O/C ya esta registrado'
   return .f.
endif   
return

PROCEDURE Corri_Hj
*-----------------
ON KEY LABEL F7
ON KEY LABEL F9
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agreg_Ic
ON KEY LABEL F8  DO Elimi_Ic
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE ITEOC
set orde to iteoc1
seek m.Periodo + m.NumOc  &&+m.Codfte
if !found()
   do agreg_ic
endif   
if vtipo='M'
	BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumOc FIELD ;
   		CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt,.F.):F :W=EMPTY(CodArt) ,;
   		Descri      : H= 'Descripci¢n' :29 :W=.F. ,;
   		CanReq      : H= 'Cantidad' :P='99,999.999' ,;
   		CodUni      : H= 'Uni'      :W=.F. :3,;
   		PreUni      : H= 'PreUni' :P='99,999.999' :V=ING_VAL():F,;
   		X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='999,999.99' :W=.F.
else
	BROWSE NOAPPEND NODELETE NOCLEAR NOMENU WINDOW Wind_2 key m.Periodo + m.NumOc FIELD ;
   		CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt,.F.):F :W=EMPTY(CodArt) ,;
   		Descri      : H= 'Descripci¢n' :29 :W=.F. ,;
   		CanReq      : H= 'Cantidad' :P='99,999.999' :R,;
   		CodUni      : H= 'Uni'      :W=.F. :3,;
   		PreUni      : H= 'PreUni' :P='99,999.999':R,;
   		X=ROUND(CanReq*PreUni,2)  :H='Total'  :P='999,999.99' :W=.F.
endif

 SELE ITEOC
 seek m.Periodo + m.NumOc
 vTothoc = 0
 SCAN WHILE Periodo +NumOc=m.Periodo + m.NumOc
	vTothoc = vTothoc + valtot
 ENDSCAN 
 seek m.Periodo + m.NumOc
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F9  DO VISTA_DET
* ON KEY LABEL F7  DO PREstado &&WITH 'OC','m.perhc+m.numhc' 
 ON KEY LABEL F10
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Orden
 if lastkey()=27
    return .f.
 endif
 set relation to
 do trab_hijo && se adicionan los componentes, metas y partidas
 RETURN
