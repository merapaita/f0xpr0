*--------------------------------------------------------------------------
* RegPec.Prg
* Registra los Pecosas que se emiten en cada dependencia
* Estado del Pecosa:
*   '00' Emitida     ** 	Este es el que se registra en el pecosa
*   '20' Con Correlativo
*   '50' Con O/C
*   '70' Devuelta
*   '99' Anulada
* Autor: LCD
*--------------------------------------------------------------------------
parameter vopcion,vtipfun
*- Abriendo Archivos
SET EXCL OFF
USE Parmae   IN 1   ORDER TAG Parmae1  ALIAS Parma
USE calen    IN 2   ORDER TAG calen1   ALIAS calen
USE Pecosa   IN 3   ORDER TAG Pecosa1  ALIAS Pecosa
USE ItePec   IN 4   ORDER TAG ItePec1  ALIAS Itepec
USE Artmae   IN 5   ORDER TAG Artmae1  ALIAS Produ
USE IteArt   IN 6   ORDER TAG IteArt1  ALIAS Iteart
USE cdrnec   IN 7   ORDER TAG Cdrnec1  ALIAS cuadro
USE itecn    IN 8   ORDER TAG itecn1   ALIAS itecn
USE maepre   IN 10  ORDER TAG maepre1  ALIAS maepre
USE itepar   IN 11  ORDER TAG itepar1  ALIAS ITEPAR          
USE OrdCom   IN 13  ORDER TAG Ordcom1  ALIAS Orden
USE USUARIO  IN 20  ORDER TAG USUARIO1 ALIAS USU

*- Mensajes de aviso al usuario
Vmens01 = ' Pecosas : REVISION '
Vmens02 = 'Registro de Pecosa'
Vmens04 = 'Dicho Pecosa no fue encontrado'
Vmens05 = 'No existe Pecosa anterior'
Vmens06 = 'No existe Pecosa siguiente'
Vmens07 = '¨ Desea Anular ‚ste Pecosa ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Pecosa ha sido anulado'
Vmens10 = 'El Pecosa ya est  Atendido'
Vmens11 = 'El Pecosa ha sido devuelto'
Vmens12 = 'El Pecosa ya tiene O/C'

PUBLIC VANT,gh,vnp
SELECT Pecosa
GO BOTTOM

if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif   

on key label F4 do imprimir
on key label F5 do lprecioS
on key label F6 do lprecioX

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO Vista
HIDE POPUP ALL

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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 13,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE ' Detalle :Pecosa   ®F9¯ Detalle :Item     ®F4¯Imprime  ®F5¯Art.Reg ' COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 14,00 TO 23,79 DOUBLE ;
 TITLE ' ®F7¯Seguimiento   ®F9¯Detalle Item   ®Esc¯ Sale   ®F3¯Cdro Nec.' COLOR SCHEME 10

 DEFINE WINDOW Wind_4 FROM 08,00 TO 10,79 DOUBLE ;
 TITLE ' Detalle: Cuadro Necesidades ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nular '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
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
 @  1, 2 SAY "            Fecha :"
 @  1,38 SAY "    N£mero Pecosa :"
 @  2, 2 SAY "      Dependencia :"
 @  3, 2 SAY "             Atte :"

 @  4, 2 SAY "      Cadena Fun. :" 
 @  5, 2 SAY " Fte. Funcionami. :"

 @  6, 2 SAY "          Funci¢n :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      SubPrograma :"
 @  9, 2 SAY "   Activ./Proyec. :"

 @ 10, 2 SAY "          Destino :"
 @ 11, 2 SAY "    Observaciones :"
 RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Pecosa
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 
 ACTIVATE WINDOW Wind_1
 ON KEY LABEL F9 DO vista_det
 
 SCATTER MEMVAR
  =val_CODCAD(ALLT(m.codcad),m.periodo+'01001','C')
  IF USer_tp$'E' 
     @  1,22 SAY m.FecPec
     @  1,60 SAY m.periodo
     @  1,63 SAY m.NumPec
     DO STANDBY with 'Pecosa Elaborando por '+user
     IF !BOF()
        SKIP -1
        DO VISTA
     ENDIF   
  ELSE
  @  0,00 SAY IIF(m.user_tp='C',PADC("®Corregido por "+ IIF(!EMPTY(m.User_CR),ALLTRIM(m.User_CR),ALLTRIM(m.User))+'¯',79,' '),PADC("®Elaborado por "+ ALLTRIM(m.User)+'¯',79,' '))
  @  0,02 SAY IIF(m.TipPec='S','Pecosa Stock     ',IIF(m.TipPec='O','Pecosa Compra    ',IIF(m.TipPec='R','Pecosa Referencia',IIF(m.TipPec='T','Pecosa Transporte','Pecosa Caja Chica')))) COLOR SCHEME 02
  @  0,60 SAY vEstPec(m.Estado) COLOR SCHEME 02
  @  1,22 SAY m.FecPec
  @  1,63 SAY m.NumPec
  
  @  2,22 SAY val_para(m.CodDep,'CODDEP','A',22,50)
  @  3,22 SAY m.atte

  @  4,22 Say val_codcad(m.codcad,m.periodo+'01001','V',22,40)
  @  5,22 SAY Val_Para(m.CodFte,'CODFTE','V',22,40)

  @  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
  @  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
  @  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
  @  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

  @ 10,22 SAY m.destino picture '@S56'
  @ 11,22 SAY m.Observa             && PRUEBA
  DO VISTA_HIJO
  ENDIF
  RETURN

PROCEDURE VISTA_HIJO
*-------------------
private vest
hide popup all
SELECT ItePec
SEEK m.periodo+m.numpec &&+alltrim(m.codfte) 
vEst00 = 0
vEst20 = 0
vEst30 = 0
vEst50 = 0
vEst99 = 0
vFte   = 0
SCAN while itepec.periodo+itepec.numpec=m.periodo+m.numpec 
      if itepec.CODfte # alltrim(m.codfte)
  		 vfte = vfte + 1	    
      endif 
      do case 
         case itepec.estado='00' &&Pendientes
         vEst00 = vEst00 + 1
         case itepec.estado='20' &&
         vEst20 = vEst20 + 1
         case itepec.estado='30' &&
         vEst30 = vEst30 + 1
         case itepec.estado='50' &&
         vEst50 = vEst50 + 1
         case itepec.estado='99' &&
         vEst99 = vEst99 + 1
      endcase
ENDSCAN

if vfte#0
    do standby with "Cambio de Fte. de Financto ... Revise"
endif

IF M.ESTADO#'50'
DO CASE
   CASE vEst00 # 0 
        SELECT PECOSA              
	    if rlock()
           repla pecosa.estado with '00'
        endif   
        UNLOCK
    CASE vEst20 # 0 and vest30=0
         SELECT PECOSA              
	     if rlock()
            repla pecosa.estado with '20'
         endif   
         UNLOCK
    CASE vEst30 # 0 and vest50=0
         SELECT PECOSA              
	     if rlock()
            repla pecosa.estado with '30'
         endif   
         UNLOCK
ENDCASE
ENDIF
SELE ITEPEC
SEEK m.periodo+m.numpec
BROWSE ; 
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+m.numpec TIMEOUT 0.001 ;
   WINDOW Wind_2 ;
   FIELDS;
   XX = IIF( Estado= '50','û','') :H = 'œ' :W=.F.,;
   numord      : H= 'Od' :W=.F.  ,;
   CodArt      : H= 'C¢digo' :V=VALART():F:W=EMPTY(CodArt),;
   Descri      : H= 'Descripci¢n' :35 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :3 ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   Codfte      : H= 'Fte' :P='!!' :w=.f. ,;
   Ess=IIF( Estado= '00','Pendte',IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50' AND m.tippec='S',"Atendido","OC"+NumOc)))) :H='Estado' :W=.F.,;
   Preuni      : H= 'Precio ' :P='99,999.99'  :W=.F.
   SELE PECOSA
RETURN

PROCEDURE CONSULT
*----------------
USE IN 5
USE IN 6
USE IN 7
USE IN 8

USE HojCon   IN 14  ORDER TAG HojCon1  ALIAS Hoja
USE Cheque   IN 15  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 16  ORDER TAG Compag1  ALIAS compag

DO Estado WITH 'PE','ItePec.Periodo+Itepec.Numoc+Itepec.Codfte'

USE IN 14
USE IN 15
USE IN 16

USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE cdrnec   IN 7   order tag Cdrnec1  ALIAS cuadro
USE itecn    IN 8   order tag itecn3   ALIAS itecn
USE HojCon   IN 14  ORDER TAG HojCon1  ALIAS Hoja
RETURN

PROCEDURE VISTA_det
*------------------
hide popup all
ON KEY LABEL F9 DO OBSERVA
ON KEY LABEL F7 DO CONSULT 
ON KEY LABEL F3 DO MUSCDR

SELECT ItePec
SEEK m.periodo+m.numpec
BROWSE ;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.periodo+m.numpec ;
   WINDOW Wind_3 ;
   FIELDS;
   CodArt      : H= 'C¢digo' :V=VALART():F:W=EMPTY(CodArt),;
   Artcod      : H= 'Cd' ,;   
   Descri      : H= 'Descripci¢n' :38 :W=.F.  ,;
   Coduni      : H= 'Uni'      :W=.F. :3 ,;
   Canreq      : H= 'Cantd' :P='99,999.99' :W=!EMPTY(CodArt) ,;
   Codfte      : H= 'Fte' :P='!!' :W=.F. ,;   
   Ess=IIF( Estado= '00','Pendte',IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50','Atendi',"OC"+NumOc)))) :H='Estado' :W=.F.,;
   Preuni      : h= 'Precio ' :P='99,999.99' :W=.F.
   
ON KEY LABEL F9 DO vista_det
ON KEY LABEL F7 
ON KEY LABEL F3

SELE PECOSA
DO VISTA
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9

SELE PECOSA
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF

SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
SET SKIP TO ITEPEC
Vtemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
NumPec :H=' N§ ' ,;
Est = IIF(Itepec.Estado= '00','Pend',IIF(Itepec.Estado = '30',itepec.NumOc,IIF(Estado='99','Anul',IIF(Itepec.Estado='50','Aten','S/Ct')))) :H='Estd' ,;
Codfte :H='Fte ' ,;
FecPec :H='Fecha' ,;
Coddep :H='DEP',;
Itepec.CanReq : H='Cantid':P='99,999.99',;
itepec.Descri :H='Detalle '
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
IF LASTKEY()=27
  GOTO Vtemp
ENDIF
SHOW MENU mMenu
ON KEY LABEL F10
SET RELA TO
SELE PECOSA
DO Vista
RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF

ON KEY LABEL F7 
ON KEY LABEL F9

vtemp    = RECNO()
vPeriodo = RIGHT(DTOC(DATE()),2)
vNum_pec = '    '
*vCod_fte = '  '
ACTIVATE WINDOW standby
@ 1,01 SAY 'Ingrese N£mero Pecosa: ' GET M.Periodo PICTURE '!!'
@ 1,27 SAY '-' GET vNum_Pec PICTURE '!!!!' VALID vBusca()
*@ 1,33 SAY '-' GET vcod_fte PICTURE '!!' VALID VAL_PARA(vCod_Fte ,'CODFTE','C',33,20)
READ

DEACTIVATE WINDOW standby
IF EMPTY(vNum_pec) .or. LASTKEY()=27
   ON KEY LABEL F9 DO vista_det
   RETURN
ELSE
   SEEK vPeriodo + vNum_pec &&+ alltrim(vcod_fte)
   IF !FOUND()
      DO standby WITH Vmens04
      GOTO Vtemp
   ELSE
      DO Vista
   ENDIF
ENDIF
ON KEY LABEL F9 DO vista_det
RETURN


PROCEDURE vBusca
*---------------
vnum_pec=padl(alltrim(vnum_pec),4,'0')
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
ON KEY LABEL F7 
ON KEY LABEL F9

PUBLIC vMes,vPart
 
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
IF Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
ENDIF
IF Estado = '70'
   * La pecosa ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
ENDIF
IF Estado = '50'
   * La pecosa ya tiene O/C
   DO STANDBY WITH Vmens12
   RETURN
ENDIF
IF ESTADO = '30'
    DO STANDBY WITH 'La pecosa ya tiene O/C' 
    return
ENDIF	
IF ESTADO = '40'
    DO STANDBY WITH 'La pecosa ya est  despachado '
    return
ENDIF    

SELECT Pecosa
IF escolor
    DEFINE POPUP xcot  FROM 16,65 SHADOW COLOR &L_COL
ELSE
    DEFINE POPUP xcot  FROM 16,65 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF xcot PROMPT ' \<Stock     '
DEFINE BAR 2 OF xcot PROMPT ' \<Compra    '
DEFINE BAR 3 OF xcot PROMPT ' \<Transporte'
DEFINE BAR 4 OF xcot PROMPT ' \<Referencia'
 
ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP XCOT
DO CASE
    CASE BAR() = 1
      vTipo = 'S'
    CASE BAR() = 2
      vTipo = 'O'
    CASE BAR() = 3
      vTipo = 'T'
    CASE BAR() = 4
      vTipo = 'R'
    OTHERWISE
ENDCASE
RELEASE POPUP Xcot

IF LASTKEY()=27
    DO STANDBY WITH 'Proceso cancelado'
    DO VISTA
    RETURN
ENDIF

SCATTER MEMVAR
m.TipPec = vTipo
ACTIVATE WINDOW Wind_1
DO PANTALLA
=val_CODCAD(ALLT(m.codcad),m.periodo+'01001','C')

 @  0,02 SAY IIF(m.TipPec='S','Pecosa Stock     ',IIF(m.TipPec='O','Pecosa Compra    ',IIF(m.TipPec='R','Pecosa Referencia',IIF(m.TipPec='T','Pecosa Transporte','Pecosa Caja Chica')))) COLOR SCHEME 02
 @  1,22 GET m.FecPec    PICTURE '@D'
 @  1,60 GET m.Periodo   PICTURE '!!' DISABLE
 @  1,62 SAY '-'
 @  1,63 GET m.NumPec	 PICTURE '!!!!' DISABLE

 @  2,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_DEP( m.CodDep,'CODDEP',' ',22,40) AND VALATTE()
 @  3,22 GET m.atte      VALID !EMPTY(m.CodDep)

 @  4,22 GET m.CodCad    PICTURE '!!!!' valid val_codcad(m.codcad,m.periodo+'01001',' ',22,40)
 @  5,22 GET m.CodFte    PICTURE '!!'   VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,40) 
 READ VALID val_read()
 IF LASTKEY()=27 
	DO vista
	RETURN
 ENDIF
 @  6,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  7,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  8,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @  9,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

 @ 10,22 GET m.destino   picture '@S49' 
 @ 11,22 GET m.Observa   picture "@S56"
 READ VALID VAL_READ()

 IF LASTKEY() # 27 && Si se presion¢ Esc, no graba
    DO WHILE .T.
       Ok = Trabaja_Hj()
       IF LASTKEY() # 27
          IF YESNO('¨ Conforme la correcci¢n ?')
             EXIT
          ENDIF
       ELSE
          IF YESNO('¨ Cancela la correcci¢n ?')
             ok = .F.
             EXIT
          ENDIF
       ENDIF
    ENDDO

    IF Ok .AND. LASTKEY() # 27
    && verifica si estan correctos
       SELECT Itepec
       SEEK m.periodo+m.numpec &&+alltrim(m.Codfte)
       vorer=1
	   SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec  
			IF RLOCK()
		       IF EMPTY(CODART) OR EMPTY(DESCRI) or  canreq=0
	  			  DELETE NEXT 1
    		   ELSE
			      REPLACE CodCad with m.codcad ,TipPec with m.tippec , Codfte with m.CodFte, CODDEP WITH M.CODDEP
			      if empty(numord)
				     replace numord with padl(vorer,2,'0')
			      endif   
			      vorer= vorer + 1
			   ENDIF   
		    ENDIF
       ENDSCAN

       IF Ok .AND. LASTKEY() # 27
          SELECT Pecosa
		  m.user_cr = sys(0)
          m.user_fc = date()
          m.user_TP = 'C'
          GATHER MEMVAR
       ELSE
          SELECT Itepec
       ENDIF
    ELSE
       *- No pudo agregar la liquidaci¢n
       SELECT Pecosa
    ENDIF
ENDIF
DO Vista                    && Muestra nuevos datos
UNLOCK
RETURN


PROCEDURE Ingre             && Crea nuevo registro en BD
*--------------
PUBLIC AST,REC,VMES,VPART,VTIPO
ON KEY LABEL F7 
ON KEY LABEL F9

IF escolor
    DEFINE POPUP xcot  FROM 16,65 SHADOW COLOR &L_COL
ELSE
    DEFINE POPUP xcot  FROM 16,65 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF xcot PROMPT ' \<Stock     '
DEFINE BAR 2 OF xcot PROMPT ' \<Compra    '
DEFINE BAR 3 OF xcot PROMPT ' \<Transporte'
DEFINE BAR 4 OF xcot PROMPT ' \<Referencia' 
DEFINE BAR 5 OF xcot PROMPT ' c\<Aja Chica'
 
ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP XCOT

DO CASE
    CASE BAR() = 1
      vTipo = 'S'
    CASE BAR() = 2
      vTipo = 'O'
    CASE BAR() = 3
      vTipo = 'T'
    CASE BAR() = 4
      vTipo = 'R'
    CASE BAR() = 5
      vTipo = 'C'
    OTHERWISE
ENDCASE
RELEASE POPUP Xcot

IF LASTKEY()=27 OR !vtipo$'SOTR'
   DO STANDBY WITH 'Proceso cancelado'
   DO VISTA
   RETURN
ENDIF

DO PANTALLA
SELECT Pecosa
REC=RECNO()
AST=ORDER()

SCATTER MEMVAR BLANK
m.periodo = STR(YEAR(DATE())-1900,2)
= REPASA()
m.Fecpec  = DATE()
m.TipPec = vTipo

SET CONFIRM OFF
@  0,02 SAY IIF(m.TipPec='S','Pecosa Stock     ',IIF(m.TipPec='O','Pecosa Compra    ',IIF(m.TipPec='R','Pecosa Referencia',IIF(m.TipPec='T','Pecosa Transporte','Pecosa Caja Chica')))) COLOR SCHEME 02
@  1,22 GET m.FecPec    PICTURE '@D'
@  1,60 GET m.Periodo   DISABLE
@  1,62 SAY '-'
@  1,63 GET m.NumPec	PICTURE '!!!!' DISABLE &&valid SIHAY() 
@  2,22 GET m.CodDep	PICTURE '!!!!!!' VALID Val_DEP( m.CodDep,'CODDEP',' ',22,40,7) AND VALATTE()
@  3,22 GET m.atte		VALID !EMPTY(m.CodDep)

@  4,22 GET m.CodCad    PICTURE '!!!!' valid val_codcad(m.codcad,m.periodo+'01001',' ',22,30)
@  5,22 GET m.CodFte    PICTURE '!!'   VALID VAL_PARA(m.CodFte ,'CODFTE',' ',22,30)

READ VALID val_read()
IF LASTKEY()=27
	SELE PECOSA
    GO GH
    if numpec=alltrim(m.numpec)
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

@ 10,22 GET m.destino   picture '@S49' 
@ 11,22 GET m.Observa   picture '@S56' 

READ VALID VAL_READ()
SET CONFIRM ON

IF LASTKEY() # 27 
   DO WHILE .T.
      Ok = Trabaja_Hj()
      IF LASTKEY() # 27
         IF YESNO('¨ Confirme el ingreso ?')
            EXIT
         ENDIF
      ELSE
         DO STANDBY WITH ' Cancelado el Ingreso ...'
         ok = .F.
         EXIT
      ENDIF
   ENDDO

   IF Ok .AND. LASTKEY() # 27     && verifica si estan correctos
      SELECT Itepec
	  SEEK m.periodo+m.numpec
	  vNumOrd = 0
	  SCAN WHILE m.Periodo = Periodo and m.Numpec = Numpec 
		   IF F_LOCK(1)
		      IF EMPTY(CODART) OR EMPTY(DESCRI) or  canreq = 0
		  	     DELETE NEXT 1
	    	  ELSE
	    		 vnumord = vnumord + 1 
			     REPLACE CodCad with m.codcad , TipPec with m.tippec , numord with padl(alltrim(str(vnumord,2)),2,'0'),CODDEP WITH M.CODDEP
			  ENDIF
		   ENDIF
	  ENDSCAN
	  SELECT Pecosa
	  GO GH
      IF !empty(m.Numpec)
		 m.user = sys(0)
  		 m.user_fc = date()
	     m.user_TP = 'I'
	     m.estado  = '00'
   		 GATHER MEMVAR
       ENDIF   
   ELSE
       SELECT Itepec
       SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
       SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec &&and alltrim(codfte)= codfte
            IF F_LOCK(1)
               DELETE NEXT 1
            ENDIF
       ENDSCAN
       UNLOCK
       SELE PECOSA
       GO GH
       if numpec=alltrim(m.numpec)
          dele next 1
       endif   
       GO REC
  ENDIF
ELSE
   DO STANDBY WITH 'Proceso cancelado'
   SELE PECOSA
   GO GH
   if numpec=alltrim(m.numpec)
      dele next 1
   endif   
   GO REC
ENDIF
SELECT Pecosa
if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif   
DO Vista
RETURN


procedure lprecios
*-----------------
private as
AS=ALIAS()
select itepec
od=order()
set order to itepec10
SET FILTER TO ESTADO#'9'
go top
DO REPORTE WITH 2, "lprecio", " Lista por Productos " ,1,.F.,.T.
SET FILTER TO
set orde to (od)
if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif 
SELE (AS)  
return

procedure lprecioX
*-----------------
private as
AS=ALIAS()
select itepec
od=order()
set order to itepec10
SET FILTER TO ESTADO#'9'
go top
DO REPORTE WITH 2, "lprecioX", " Lista por Productos " ,1,.F.,.T.
SET FILTER TO
set orde to (od)
if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif 
SELE (AS)  
return

PROCEDURE VAL_PE
*---------------
m.NumPec = Padl(alltrim(vNumPec),4,'0')
return


PROCEDURE SIHAY
*--------------
PRIVATE AS
AS=RECNO()
SEEK m.periodo+m.NumPec 
if found() AND RECNO()#AS
   do standby with 'El Pecosa ya esta registrado'
   return .f.
endif   
return


PROCEDURE VALATTE
*----------------
SELECT Cuadro
SEEK  m.Periodo+m.Coddep

if found()
   m.atte = IIF(EMPTY(m.atte) or m.atte='No se Registra Solicitante..,revise',Cuadro.Atte,m.atte)
else
   m.atte = 'No se Registra Solicitante..,revise'
endif

SELECT Pecosa
return

PROCEDURE Trabaja_Hj
*-------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)

 SELE ITEPEC
 SEEK m.periodo+m.numpec &&+alltrim(m.codfte)  &&quitar luego 

 IF !FOUND()
     DO Agreg_item
 ENDIF

 BROWSE NOREFRESH NOAPPEND NODELETE NOMENU WINDOW Wind_2 KEY m.periodo+m.numpec FIELD ;
     CodArt      : H= 'C¢digo' :V=VAL_ARTC(codArt) AND antr() :F ,;
     Descri      : H= 'Descripci¢n' :43 :W=.F.,;
     Coduni      : H= 'Uni' :W=.F. :5 ,;
     Canreq      : H= 'Cantd' :P='99,999.99' :V=iif(itepec.tipcdr='S',VALOR(),.t.):F :W=!EMPTY(CodArt) ,;
     Ess=IIF( Estado= '00','Pendte',IIF( Estado = '20',"SC"+NumSc,IIF(Estado='99','Anulad',IIF(Estado='50','Atendi',"OC"+NumOc)))) :H='Estado' :W=.F.
	&&and iif(itepec.codart='00.',.t.,canreq>0)
UNLOCK ALL

 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 SELECT Pecosa
 RETURN .T.

procedure antr
*-------------
vAnt = itepec.Canreq
return .t.

PROCEDURE Valor
*--------------
sele itecn
seek m.Periodo+alltrim(m.Coddep)+ALLTRIM(ITEPEC.CodArt)
vPer = Trimestre(m.Fecpec)
vDispon = Itecn.Nec_&vPer - Itecn.Ped_&vPer

If Itepec.Canreq > vDispon + vant
   do standby with IIF(vdispon+vant=0,'Ya no Existe Art¡culos para este Trimestre','Ud. s¢lo puede pedir m ximo '+ALLTRIM(str(vdispon+VANT,5))+' para este Trimestre')
endif
  if rlock()
     replace Ped_&vper with Ped_&vper + ItePec.Canreq - vant
  endif
sele itepec
return .t.

PROCEDURE Anula
*---------------
 SELECT Pecosa
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚ste Pecosa ?')
 vEstado = .t.
 SELECT Itepec
 SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
 SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
      IF ESTADO='30' or estado='20' 
         DO STANDBY WITH 'La Pecosa ya tiene generada O/C o S/C,no se puede anular'
         vEstado = .f.            
         EXIT
 	 ENDIF
 ENDSCAN
 if vestado
    IF velimina 
	    SELECT Itepec
	    SEEK m.periodo+m.numpec &&+ alltrim(m.codfte)
	    SCAN WHILE m.Periodo = Periodo  .and.  m.Numpec = Numpec and alltrim(codfte)= codfte
		 if rlock()
		    replace estado with '99' 
         endif
	    endscan
   		sele pecosa
	    REPLACE ESTADO WITH '99' ,FECVER WITH DATE()
    endif
 endif   
 sele pecosa    
 DO Vista
 UNLOCK
 RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT Pecosa
VCON = RECNO()
SCATTER MEMVAR
fdbf = sys(3)+'*.DBF'
fidx = sys(3)+'*.IDX'
USE IN 5
USE IN 6
USE IN 7
copy stru to (fdbf)
use (fdbf) in 5  alias def exclu
select def
index on periodo+numpec to (fidx)
append blank
gather memvar
SET RELATION TO periodo + numpec INTO itepec
SET SKIP TO ITEPEC
if eof()
   do standby with vmens08
   return
else
  do reporte with 2,"Pecosa",' Pe.co.sa ',2
endif
SET RELATION TO 
USE IN 5
USE Artmae   IN 5   order tag Artmae1  ALIAS Produ
USE IteArt   IN 6   order tag IteArt1  ALIAS Iteart
USE cdrnec   IN 7   order tag Cdrnec1  ALIAS cuadro
USE itecn    IN 8   order tag itecn3   ALIAS itecn
SELECT PECOSA
if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif   
GO VCON
DO VISTA
RETURN

PROCEDURE Agreg_Item
*-----------------
IF F_appd()
  REPLACE Periodo  WITH m.Periodo ,;
          NumPec   WITH m.NumPec ,;
          Estado   WITH '00' ,;
          CodCad   WITH m.CodCad ,;
		  CodFte   with m.Codfte	
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
SELECT ItePec
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Corri_Item
*-----------------

REPLACE CodCad  WITH vCodCad
RETURN .T.

PROCEDURE LISTA
*--------------
ON KEY LABEL F7 
ON KEY LABEL F9
select PECOSA
vtemp =recno()
SET RELATION TO PERIODO+NUMPEC+CODFTE INTO ITEPEC
SET SKIP TO ITEPEC
if eof()
   do standby with vmens08
   return
else
   DO LISPEC
endif
select PECOSA
SET RELATION TO
if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif   
DO VISTA
RETURN

PROCEDURE LisPec
*---------------
vOrde = ORDER()
vrec  = RECNO()
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Pecosas' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vToPec,vToMes,vToFue,vTodep,vOrden,vtiplis,vTipRep
vNp 	= SPAC(4)
vFte    = SPAC(2)
vCodmes = SPAC(2)
vPeriod = SPAC(2)
vCoddep = SPAC(6)
vCodFte = SPAC(2)

@ 01,01 SAY "Todas las Pecosas : " GET vToPec  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vToPec,2,22)  and assig()
@ 02,01 SAY "           Pecosa : "
@ 02,22 GET vNp		 PICTURE '!!!!' WHEN vToPec = 2  &&VALID Padl(alltrim(vNp),4,'0')
@ 02,27 get vPeriod  Picture '!!'   WHEN vToPec = 2 
@ 02,30 SAY '-'
@ 02,31 GET vFte     PICTURE '!!' VALID VAL_PARA(vfte,'CODFTE','C') AND ValPecO() WHEN vToPec = 2

@ 04,01 SAY "  Todos las Meses : " GET vTomes  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTomes,5,22)  WHEN vToPec = 1
@ 05,01 SAY "              Mes : "
@ 05,22 GET vCodmes  PICTURE '!!'  VALID VAL_PARA(vCodMes,'FECMES','C') WHEN vToPec = 1 AND vTomes = 2

@ 07,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTofue,8,22)  WHEN vToPec = 1
@ 08,01 SAY "           Fuente : "
@ 08,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C')  WHEN vToPec = 1 and vTofue =2

@ 10,01 SAY "Todas las Dependc : " GET vToDep  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(vTodep,11,22)  WHEN vToPec = 1
@ 11,01 SAY "      Dependencia : "
@ 11,22 GET vCodDep  PICTURE '!!!!!!' VALID VAL_PARA(vCodDep,'CODDEP','C')  WHEN vToPec = 1 and vTodep =2

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Dependencia;Emision'  WHEN vToPec = 1

@ 16,01 SAY "           Estado : " GET vTipLis  FUNCTION '^ Todos;Pendientes;Atendidos;Liquidados'  WHEN vToPec = 1

@ 19,01 SAY "           Estado : " GET vTipRep  FUNCTION '^ Resumido;Detallado'  WHEN vToPec = 1

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1 AND LASTKEY()#27
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento ... Gracias ...'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Periodo+NumPec,IIF(vOrden=2,CodDep,DTOS(FECemi))) TO (vInd) ;
      FOR IIF(vToPec=1,.T.,Periodo+NumPec+CodFte = vPeriod+vNp+vFte) .AND. IIF(vTipLis=1,.T.,iif(vTipLis=2,Estado = '00',iif(vTipLis=3,Estado = '40',Estado='50')))
   SET FILTER TO iif(vToMes=1,.t.,IIF(vTipLis=4,month(fecdesp),month(fecpec))=val(vCodMes)) and iif(vToMes=1,.t.,month(fecpec)=val(vCodMes)) AND iif(vToFue=1,.t.,Codfte=alltrim(vCodfte)) and ;
		 		 iif(vToDep=1,.t.,CodDep=alltrim(vCodDep)) 
   SET INDEX TO (VIND)
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipLis=1,' en General ',IIF(vTipLis=2,' Pendientes ',' Atendidos '))
   IF !EOF()
      if vtopec = 1
         if vTiprep = 1
	         do reporte with 2,"LisPecX",' Pe.co.sa ',1,.F.,.T.
	     else
	         do reporte with 2,"LisPec",' Pe.co.sa ',1,.F.,.T.
	     endif    
	  else
	     do reporte with 2,"LisPec1",' Pe.co.sa '
	  endif   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILT TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELECT PECOSA
SET ORDE TO (VORDE)
if vopcion=2
   set filter to IIF(VFLAG='*',.T.,coddep=substr(vcoddep,1,vnumdep))
endif   
GO TOP
GO VREC
RETURN

PROCEDURE ASSIG
*--------------
vNp = PECOSA.NUMPEC
vPeriod = pecosa.PERIODO
vfte    = pecosa.codfte
RETURN  .T.

PROCEDURE VALPECO              && Revisi¢n de BD en browse
*--------------
SELE PECOSA
IF EOF()
    DO standby WITH Vmens08
    RETURN
ENDIF
vNp=Padl(alltrim(vNp),4,'0')
SEEK '97'+vNP &&+alltrim(vFte)
IF !FOUND()
     SET FILTER TO Codfte = alltrim(vFte)	
	 SET RELATION TO PERIODO+NUMPEC INTO ITEPEC
	 SET SKIP TO ITEPEC
	 vtemp = RECNO()
	 HIDE MENU mMenu
	 ACTIVATE SCREEN
	 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 ON KEY LABEL F10 KEYBOARD CHR(23)
	 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	 NumPec :H=' N§ ' ,;
	 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
	 Codcad :H='Cadena Fun.' ,;
	 FecPec :H='Fecha' ,;
	 Coddep :H='DEP',;
	 Itepec.CanReq : H='Cantidad':P='99,999',;
	 itepec.Descri :H='Detalle '
	 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	 DO Logos WITH Rotulo1,vTempo
	 SELE PECOSA
	 SET RELATION TO
	 set filter to
ENDIF
vnumpec	= Numpec
vFte    = Codfte
SHOW MENU mMenu
ON KEY LABEL F10
SELE PECOSA
RETURN

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  ON KEY LABEL F7 
  ON KEY LABEL F9
  ON KEY LABEL F4  
  RETURN

PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  ON KEY LABEL F7 
  ON KEY LABEL F9
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

function valpec
*--------------
parameter vnumpec,alis
alis = alias()
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpecp,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
SELECT Parma
SEEK 'CORREL' + 'PECOSA'
REPLACE NumEnt WITH NumEnt + 1
select (alis)
return vfun

function valult
*--------------
parameter vrec
vrec = recno()
select pecosa
set orde to pecosa1
go bott
vnp = val(numpec)+1
m.numpec=padl(alltrim(str(vnp,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif

Function repasa
*--------------
vfun = .t.
vrec = recno()
vali = alias()
select PECOSA
set orde to PECOSA1
SET FILTER TO 
SEEK '971179'
AS = RECNO()
IF FOUND()
  go as
ENDIF  
numr = 1179
do while .t.
   if val(numpec)=numr
      numr = numr + 1
      skip
      loop
   else 
      exit   
   endif   
enddo

m.numpec=padl(alltrim(str(numr,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
else
	SELE PECOSA
	IF F_APPD() 
    	 replace periodo with m.periodo,numpec with m.numpec, estado with '00', user_tp with 'E',user with sys(0)
     	gh = recno()
	ENDIF 
	UNLOCK
endif

SELECT Parma
SEEK 'CORRELPECOSA'
REPLACE NumEnt WITH NumR 
sele (vali)
return vfun


FUNCTION XVALART
*--------------
PARAMETERS _Cod
DC=ALIAS()
PRIVATE XX,YY,ZZ,vFun
vFun = .F.
SELEC ITEART
vTemp = RECNO()
SEEK 'B'+ALLTRIM(ITEPEC.CODART)
IF FOUND() AND !EMPTY(ITEPEC.CODART)
   SELECT Itecn
   seek m.Periodo+alltrim(m.Coddep)+itepec.Codart
   if found()
      SELECT itepec
         IF RLOCK()
          REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri,;
          tipcdr  WITH 'S'
          ENDIF
      vFun = .T.
   else
      do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'
      SELE ITEPEC
      IF RLOCK()
        *REPLACE ITEPEC.CodArt  WITH spac(11) && CAMBIAR PARA VALIDAR LOS PROD. QUE NO ESTAN EN CN
         REPLACE TIPCDR WITH 'N'
      ENDIF
      vFun = .T.
  endif
ELSE
   SELE itepec
   ZZ=VAL_PARA(CodArt,'CODGEB','C')
   IF LASTKEY()=27
      RETURN .T.
   ENDIF
   IF ZZ
      XX = Val_Art(_Cod,.F.)
      IF XX
         YY = Val_ArtDet(SUBSTR(Alltrim(PRODU.CODART),2,6),.F.)
         IF YY and m.Tipdoc='S'
            SELECT Itecn
            seek m.Periodo+alltrim(m.Coddep)+itepec.Codart
            if !found()
               do standby with 'Este producto no est  registrado en el Cuadro de Necesidades'
               SELE ITEPEC
               if Rlock()
                   REPLACE ;
                   coduni  WITH Iteart.coduni,;
                   preuni  WITH Iteart.preuni,;
                   descri  WITH Iteart.descri,;
                   TIPCDR WITH 'N'
               endif
               vFun = .T.
            else
               SELECT Itepec
               if rlock()
                  REPLACE ;
                   coduni  WITH Iteart.coduni,;
                   preuni  WITH Iteart.preuni,;
                   descri  WITH Iteart.descri,;
                   TIPCDR WITH 'S'
               endif
               vFun = .T.
            endif
         ELSE
           if f_lock(1)
               REPLACE ITEPEC.codart  WITH spac(11)
           endif
           vFun = .F.
        ENDIF
      ELSE
        if f_lock(1)
            REPLACE ITEPEC.codart  WITH spac(11)
        endif
        vFun = .F.
      ENDIF
   ELSE
      if f_lock(1)
        REPLACE ITEPEC.codart  WITH spac(11)
      endif
      vFun = .F.
   ENDIF
ENDIF

ON KEY
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELEC itepec

RETURN vFun

PROCEDURE Trimestre
*------------------

parameter vFecha

DO CASE
   CASE MONTH(vFecha) = 1  OR MONTH(vFecha) = 2   OR  MONTH(vFecha) = 3
        vTrim = '1'
   CASE MONTH(vFecha) = 4  OR MONTH(vFecha) = 5   OR  MONTH(vFecha) = 6
        vTrim = '2'
   CASE MONTH(vFecha) = 7  OR MONTH(vFecha) = 8   OR  MONTH(vFecha) = 9
        vTrim = '3'
   CASE MONTH(vFecha) = 10 OR MONTH(vFecha) = 11  OR  MONTH(vFecha) = 12
        vTrim = '4'
ENDCASE
return vtrim

FUNCTION Observa
*---------------

vAlias = ALIAS()
SET MEMOWIDTH TO 43
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 05,18 TO 18,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
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
   DEFINE WINDOW Observa FROM 03,18 TO 20,61 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Detalle Pecosa ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
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

PROCEDURE VUSUA
***************

PARAMETER CSYS
PRIVATE ALI
ALI = ALIAS()
VKEY = ALLTRIM(CSYS)
SELE USU
SEEK VKEY
VFUN = NOMBRE
SELE (ALI)
RETURN VFUN

FUNCTION BuscArt
*----------------
 as=alias()
 sele produ
 set orde to 1
 seek 'B'+LEFT(ITEPEC.CODART,6)
 if !found()
    vFun = ' *** ESTE GRUPO ESPECIFICO NO ESTA REGISTRADO **** '
 else
    vfun=UPPER(produ.descri)
 endif
 sele (as)
return vfun

FUNCTION val_DEP
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
SEEK filtro+mvalor

IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro AND CODIGO = substr(vcoddep,1,vnumdep)
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = parma.codigo
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE

FUNCTION Val_ArtC  && Articulos
*------------------

PARAMETER xcod,_tipo,_x,_y

ON KEY LABEL F5  
ON KEY LABEL F8  

** _tipo = .F. ---> Campo
**         .T. ---> Variable.
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
v_ent =.f.

IF !mEdita
   SET ORDER TO ITEART3
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
   v_ent = found()
ENDIF
   IF EMPTY(xcod) OR !V_ENT
      SET ORDER TO ITEART2
      GO TOP
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F2 DO FunBusDet
      ON KEY LABEL F5
      ON KEY LABEL F8
      DEFINE WINDOW _BusArt FROM 2,01 TO 22,78
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        CodArt   :H='C¢digo'     ,;
        Descri   :H='Nombre':60   ,;
        CodUni   :H='Unidad':7   
      ON KEY LABEL F10
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO ITEART1

      IF Lastkey()=27
         V_FUN = .f.
         v_ent = .f.
      ELSE
         xcod = CodArt
         xDes = Descri
         xuni = coduni
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod &&,itepec.descri with xdes ,itepec.coduni with xuni
         ENDIF
         v_fun = .T.
         v_ent = .t.
    ENDIF
ENDIF
vTipCdr='N'
IF v_ent and m.tippec='S' and itepec.canreq=0
   SELECT Itecn
   seek m.Periodo+alltrim(m.Coddep)+ALLTRIM(xCod)
   IF !FOUND()
      vTipCdr='N'
   else
      do muestra WITH 'ITEART.DESCRI'
      vTipCdr='S'
   ENDIF
ENDIF   
SELECT itepec
IF RLOCK()
   REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri,;
          TIPCDR  WITH vTipCdr
ENDIF

SELECT (mAlias)
SET ORDE TO (ORD)
ON KEY LABEL F5  DO Agreg_item
ON KEY LABEL F8  DO Elimi_item
ON KEY LABEL F10 KEYBOARD CHR(23)
UNLOCK ALL
IF !V_ENT
   RETURN v_FUN
ELSE
   RETURN v_ENT
ENDIF   

PROCEDURE MUSCDR
*---------------

private ed
ed=alias()
IF m.tippec='S' 
   SELECT Itecn
   seek m.Periodo+alltrim(m.Coddep)+ALLTRIM(ITEPEC.CODART)
   IF !FOUND()
      DO STANDBY WITH 'No tiene Cuadro de Necesidades'
   else
      do muestra WITH 'ITEPEC.DESCRI'
   ENDIF
ENDIF   
select (ed)
return


PROCEDURE MUESTRA
*----------------

PARAMETER UY
PRIVATE UY,VTRI
VTRI=TRIMESTRE(m.FECPEC)
UY = ITEART.DESCRI 
UA = 'SCHEME 10'
UB = 'W+/BG, W+/BG, GR+/W*, W+/B, R+/B, W+/GR, GR+/RB, N+/N, GR+/B, R+/B'

DEFINE WINDOW Wind_T FROM 12,00 TO 24,79 DOUBLE ;
 TITLE "Art¡culo: "+ALLTRIM(&UY) COLOR SCHEME 10

IF VTRI="1"
   DEFINE WINDOW Wind_1T FROM 14,21 TO 16,59 DOUBLE ;
   TITLE ' Movimiento :1§ Trimestre ' COLOR &UB
ELSE
   DEFINE WINDOW Wind_1T FROM 14,21 TO 16,59 DOUBLE ;
   TITLE ' Movimiento :1§ Trimestre ' COLOR &UA
ENDIF
   
IF VTRI="2"
   DEFINE WINDOW Wind_2T FROM 17,01 TO 19,39 DOUBLE ;
   TITLE ' Movimiento :2§ Trimestre ' COLOR &UB
ELSE
   DEFINE WINDOW Wind_2T FROM 17,01 TO 19,39 DOUBLE ;
   TITLE ' Movimiento :2§ Trimestre ' COLOR &UA
ENDIF

IF VTRI="3"
   DEFINE WINDOW Wind_3T FROM 17,41 TO 19,78 DOUBLE ;
   TITLE ' Movimiento :3§ Trimestre ' COLOR &UB
ELSE
   DEFINE WINDOW Wind_3T FROM 17,41 TO 19,78 DOUBLE ;
   TITLE ' Movimiento :3§ Trimestre ' COLOR &UA
ENDIF

IF VTRI="4"
   DEFINE WINDOW Wind_4T FROM 20,21 TO 22,59 DOUBLE ;
   TITLE ' Movimiento :4§ Trimestre ' COLOR &UB
ELSE
   DEFINE WINDOW Wind_4T FROM 20,21 TO 22,59 DOUBLE ;
   TITLE ' Movimiento :4§ Trimestre ' COLOR &UA
ENDIF

DO CALCDR WITH UY
   
activate window wind_t

activate window wind_1t
@ 0,0 say 'Pedido :'
@ 0,8 SAY Nec_1 picture '9,999' 
@ 0,20 say 'Atendido :'
@ 0,30 SAY Ped_1 picture '9,999'

activate window wind_2t
@ 0,0 say 'Pedido :'
@ 0,8 SAY Nec_2 picture '9,999' 
@ 0,20 say 'Atendido :'
@ 0,30 SAY Ped_2 picture '9,999'

activate window wind_3t
@ 0,0 say 'Pedido :'
@ 0,8 SAY Nec_3 picture '9,999' 
@ 0,20 say 'Atendido :'
@ 0,30 SAY Ped_3 picture '9,999'

activate window wind_4t
@ 0,0 say 'Pedido :'
@ 0,8 SAY Nec_4 picture '9,999' 
@ 0,20 say 'Atendido :'
@ 0,30 SAY Ped_4 picture '9,999'
SS=INKEY(0)
deactivate window wind_1t,wind_2t,wind_3t,wind_4t,wind_t
return

PROCEDURE CALCDR
*----------------

PARAMETER UYA
PRIVATE ALIAS,VKEY
VKEY = m.PERIODO+alltrim(m.CODDEP)+alltrim(uya)
ALIAS=ALIAS()
SELECT ITEPEC
ORDEN = ORDER()
SET ORDER TO ITEPEC13
SET FILT TO ESTADO#'99'
seek vkey
if found()
   scan while vkey = Periodo+coddep+codart
   endscan
endif

