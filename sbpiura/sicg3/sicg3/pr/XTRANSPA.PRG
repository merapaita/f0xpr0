* --------------------------------------
*
* Program	:	TRANSpa.PRG
* Descrip	:	Transferencias de Partidas
* AUTHOR	:	UDEP - 1994
* Upgrade	:	Diciembre 2000
*
** --------------------------------------

*- Guarda el menu y la pantalla anterior
SAVE SCREEN TO pta_comp
HIDE POPUP pop_01,menu

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens01 = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
Vmens02 = 'Documentos'
Vmens04 = 'Dicho Trancferencias no fue encontrado'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
xPos    = INT((80 - (LEN(ALLTRIM(cia)) + 2))/2)
Vmens01 = SUBSTR(Vmens01, 1, xPos+1) + ' ' + 'Transferencias' + ' ' + SUBSTR(Vmens01,xPos + LEN(ALLTRIM(cia)) + 3,80)

*- Abre archivos

USE TraPar  IN  1 ORDER TAG TraPar1                 ALIAS TraPar
USE IteTra  IN  2 ORDER TAG IteTra1                 ALIAS IteTra
USE Parmae  IN  4 ORDER TAG Parmae1                 ALIAS Parma
USE ItePar  IN  5 ORDER TAG ItePar1                 ALIAS ItePar
USE MaePre  IN  6 ORDER TAG MaePre1                 ALIAS MaePre
USE MaePar  IN  7 ORDER TAG MaePar1                 ALIAS Presu

*- Variables de trabajo (registro a trabajar)
PUBLIC TipIng,vMonAcu

ON KEY LABEL F7 DO vis_det
ON KEY LABEL F9 DO PENDIENTE

STORE .T. TO Agrega
STORE 0 TO vMonTraI,vMonTraS,m.MonTra
m.fecha=DATE()

*- Mes y a¤o actual
SELECT parma
SEEK "MESANO"+"ACTUAL"
_actmes = LEFT(descri,6)
actmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,3,2) )

*- Inicia proceso
SELECT TraPar
*SET FILTER TO CODOPE='001'
GO TOP
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

RESTORE SCREEN FROM pta_comp
SHOW POPUP Menu

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
*vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa           Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 12,79 DOUBLE ;
 FOOTER '° ®F7¯Detalle:Item       ®F9¯ [Aprobado/Solicitado] °' TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 13,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 10,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 03,40 TO 15,79 DOUBLE ;
 TITLE '° ®F10¯ Sale °'


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
*DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
*ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO impri
 ON SELECTION PAD termi  OF mMenu DO termi

 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 00,02 SAY "          Periodo :"
 @ 01,02 SAY "   Tipo Documento :"
 @ 02,02 SAY "     N§ Documento :"
 @ 03,02 SAY "            Fecha :"
 
 @ 02,40 SAY "   Unidad Gestora :"
 @ 03,40 SAY " Unidad Ejecutora :"
 @ 04,02 SAY "    Modalidad Doc :"
 ***
* @ 06,02 SAY "   Fuente Fto.	:"
 * se debe de ubicar la modalidad del documento (ModDoc)
 @ 07,02 SAY "      Descripci¢n :"
 @ 08,02 SAY "         Asignado :"
 @ 09,02 SAY "    Monto Ingreso :"
 @ 10,02 SAY "     Monto Salida :"
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
*DIMENSION dbftags(2)
*dbftags(1) = 'Habilita'
*dbftags(2) = 'Habilitado'
 ACTIVATE WINDOW Wind_C0
 SELECT TraPar
 SCATTER MEMVAR
 @  0,60 SAY IIF( m.estado= '00','Pendiente','Aprobado ')
 @  0,22 SAY m.Periodo
 @  1,22 SAY VAL_PARA(m.TipDoc,'TIPDOC','V',22,40)
 @  2,22 SAY m.NumDoc
 @  3,22 SAY m.Fecha

 @  2,60 SAY m.uniges
 @  3,60 SAY m.unieje

** @  4,22 GET m.moddoc PICTURE '@^' FROM dbftags SIZE 2, 14 DISABLE
 @  4,22 SAY VAL_PARA(m.moddoc,'MODDOC','V',22,40)
*****
* @  6,22 say m.codfte
 @  7,22 SAY SUBSTR(m.Descri,1,60)
 @  8,22 SAY m.TotAsig  PICTURE "999,999,999,999.99"
 @  9,22 SAY m.MonTraI  PICTURE "999,999,999,999.99"
 @ 10,22 SAY m.MonTraS  PICTURE "999,999,999,999.99"
 DO Vista_Hijo
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 HIDE POPUP ALL
 SET EXAC OFF
 SELECT IteTra
 SET ORDE TO ITETRA1
 SEEK m.periodo+m.codope+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc) &&+ALLTRIM(m.UniGes)+ALLTRIM(m.UniEje)
 BROWSE;
    NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY M.PERIODO+m.codope+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc) TIMEOUT 0.0001;
    WINDOW Wind_C2 ;
    FIELDS;
    TipOpe   :H='T':P='!',;
	codcad   :H='Cad. Fun.',;
	codfte   :h='Cod. Fte.',;
	CodPart  :H='Partida',;
    valpart  :H='Presup.Inicial':P='999,999,999,999.99',;
    MonTra   :H='Transferencia':P='999,999,999,999.99'

 SELECT TraPar
HIDE WINDOW wind_C2
RETURN

PROCEDURE Vis_DET         && Muestra items del documento
*-------------------
 on key label f7
 SELECT IteTra
 SET ORDE TO ITETRA1
 SEEK m.periodo+m.codope+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc)
 BROWSE;
    NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT ;
    WINDOW Wind_C2 KEY M.PERIODO+m.codope+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc);
    FIELDS;
    TipOpe   :H='T':P='!',;
	codcad   :H='Cad. Fun.',;
	codfte   :h='Cod. Fte.',;
	CodPart  :H='Partida' ,;
    valpart  :H='Presup.Inicial':P='999,999,999,999.99',;
    MonTra   :H='Transferencia':P='999,999,999,999.99'
 ON KEY LABEL F7 DO vis_det
 SELECT TraPar
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT TraPar
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 ACTIVATE SCREEN

 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   TipDoc   :H= 'Tipo doc.',;
   NumDoc   :H= 'Documento',;
   Fecha    :H= 'CompAdi',;
   Descri   :H= 'Descrip.',;
   MonTraI  :H='Ingreso Trans.':P='999,999,999,999.99',;
   MonTraS  :H='Salida Trans. ':P='999,999,999,999.99'

 ON KEY LABEL F10
 DEACTIVATE WINDOW Wind_C4

 DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vper = RIGHT(DTOC(DATE()),2)
AS=ORDER()
vnum  = '00000'
vdoc  = '   '
vtip  = ' '

DEFINE WINDOW lis FROM 08,12 TO 15,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis
@  1,2 SAY ' Periodo : ' GET vper   PICTURE '!!'
@  2,2 SAY 'Tip.Doc. : ' GET vdoc   PICTURE '!!!' VALID val_para(vdoc,'TIPDOC','C',14,30) 
@  3,2 SAY 'Num.Doc. : ' GET vnum   PICTURE '!!!!!' 
@  3,19 say '-'
@  3,20 GET vper   PICTURE '!!' 
READ

DEACTIVATE WINDOW lis

IF EMPTY(vnum) .OR. LASTKEY()=27
	RETURN
ELSE
   vkey = vPer+'001'+alltrim(vDoc)+alltrim(vnum)+'-'+alltrim(vPer)
   seek ALLTRIM(vkey)
   IF !FOUND()
		DO standby WITH 'Transferencia no encontrado'
		GOTO vtemp
   ELSE
		DO vista
   ENDIF
ENDIF
RETURN

PROCEDURE Anter              && Documento anterior
*--------------
 SELECT TraPar
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF !BOF()
    SKIP -1
 ENDIF

 IF BOF()
    GO TOP
    DO standby WITH "No existe Documento anterior."
 ELSE
    DO Vista
 ENDIF

RETURN


PROCEDURE Proxi              && Documento siguiente
*--------------
 SELECT TraPar
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF !EOF()
    SKIP
 ENDIF

 IF EOF()
    DO standby WITH "No existe Documento siguiente."
    GO BOTTOM
 ELSE
    DO Vista
 ENDIF
RETURN


PROCEDURE Corri              && Corrige el documento actual
*--------------
 tiping = 'C'
  IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF
 IF m.fecha < m.actmes
*    DO standby WITH "Documento no es del mes actual"
*    RETURN
 ENDIF
 IF !f_lock(1)
    RETURN
 ENDIF
 SELECT TraPar
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_C0
 
* DIMENSION dbftags(2)
* dbftags(1) = 'Habilita'
* dbftags(2) = 'Habilitado'
* m.moddoc = 'Habilita'
 m.codope = '003'
 @ 00,22 GET m.Periodo
 @ 01,22 SAY Val_Para(m.TipDoc,'TIPDOC',' ',22,40)
 @ 02,22 SAY m.NumDoc
 @ 03,22 GET m.Fecha       
 @ 02,60 GET m.uniges   PICTURE '!!'  VALID VAL_PARA(m.uniges,'UNIGES',' ',60,15,3)
 @ 03,60 GET m.unieje   PICTURE '!!!' VALID VAL_PARA1(m.unieje,'UNIEJE'+ALLt(m.uniges),' ',60,12)

**@ 04,22 GET m.moddoc PICTURE '@^' FROM dbftags SIZE 2, 14 DISABLE
 @ 04,22 GET m.moddoc      PICTURE '!!!' VALID Val_para(m.modDOC,'MODDOC',' ',22,20) DISA

 ***
* @ 06,22 GET m.codfte      PICTURE '!!' VALID Val_Para (m.CodFte, 'CODFTE', ' ',22)
 @ 07,22 GET m.Descri      PICTURE "@S56"
 @ 08,22 say m.TotAsig     PICTURE "999,999,999,999.99"
 @ 09,22 SAY m.MonTraI     PICTURE "999,999,999,999.99"
 @ 10,22 SAY m.MonTraS     PICTURE "999,999,999,999.99"

 READ VALID Val_Read()
 IF LASTKEY() # 27
    DO Tra_Hijo
    =Valida()     
    SELEC TRAPAR
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 UNLOCK ALL
 SELECT TraPar
 DO Vista
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 tiping = 'I'
 ACTIVATE WINDOW Wind_C0
 SELECT TraPar
 SCATTER MEMVAR BLANK
 *DIMENSION dbftags(2)
 *dbftags(1) = 'Habilita'
 *dbftags(2) = 'Habilitado'
 *m.moddoc = 'Habilita'
 m.codope = '003'
 @ 00,22 GET m.Periodo
 @ 01,22 GET m.TipDoc   PICTURE '!!!' VALID Val_para(m.TipDoc,'TIPDOC',' ',22,20)
 
 @ 02,22 GET m.NumDoc   PICTURE '!!!!!-!!'
 @ 03,22 GET m.Fecha       
 @ 02,60 GET m.uniges   PICTURE '!!'  VALID VAL_PARA(m.uniges,'UNIGES',' ',60,15,3)
 @ 03,60 GET m.unieje   PICTURE '!!!' VALID VAL_PARA1(m.unieje,'UNIEJE'+ALLt(m.uniges),' ',60,12) AND V_NUM()
 
 *@ 04,22 GET m.moddoc      PICTURE '!!!' VALID Val_para(m.modDOC,'MODDOC',' ',22,20)
 **CODIGO DE LA FUENTE SE AGREGO
* @ 06,22 GET m.codfte   PICTURE '!!'  VALID Val_Para (m.CodFte, 'CODFTE', ' ',22)
 @ 07,22 GET m.Descri      PICTURE "@S56"
 @ 08,22 SAY m.TotAsig     PICTURE "999,999,999,999.99"
 @ 09,22 SAY m.MonTraI     PICTURE "999,999,999,999.99"
 @ 10,22 SAY m.MonTraS     PICTURE "999,999,999,999.99"
 
 READ VALID Val_Read()
 IF LASTKEY()#27
    vAnula = .F.
    SELECT IteTra
    DO WHILE .T.
       OK=Tra_Hijo()
       IF LASTKEY() = 27 AND OK
          IF YESNO( '¨ Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
       ELSE
          IF YESNO( '¨ Est n correctos los datos ?')
              EXIT
           ENDIF
       ENDIF
    ENDDO
    IF !vAnula
	    =valida()
	    SELECT TraPar  
	    m.estado = '00'
	    IF F_Appd()
	       GATHER MEMVAR
	    ENDIF
    ELSE
       DO Anula
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 STORE 0 TO vMonTraI,vMonTraS
 UNLOCK ALL
 SELECT TraPar
 DO Vista
 RETURN

*@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados' WHEN vtocli=1 and (vlistado=2 OR  vlistado=3 OR VLISTADO=4)
* @ 04,22 GET m.moddoc PICTURE '^Habilta,habilitado' FROM dbftags SIZE 2, 14


PROCEDURE Tra_Hijo
*----------------
 HIDE MENU mMenu
 ACTIVATE SCREEN
 VTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO agreg_item
 ON KEY LABEL F8  DO elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELECT IteTra
 SEEK alltrim(m.periodo)+m.codope+alltrim(m.TipDoc)+alltrim(m.NumDoc)
 IF !FOUND()
    DO Agreg_Item
 ENDIF
 STORE 0 TO m.MonTra,m.MonTraI,m.MonTraS,vMontoS,vMontoI
 BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
    KEY alltrim(m.periodo)+m.codope+alltrim(m.TipDoc)+alltrim(m.NumDoc) FIELDS;
      TipOpe   : H='T'         ,;
   	  codcad   : H='Cad. Fun.' :V=val_codcad(codcad,M.periodo+ALLT(m.uniges)+ALLT(m.unieje),'codcad'):f,;
   	  CodFte   : H='Fte'       :V=Val_ParaBr(CODFTE,'CODFTE','codfte'):F,;
   	  CodPart  : H='Part'      :V=VAL_PARt1(codpart,periodo+ALLT(m.uniges)+ALLT(m.unieje)+codcad+codfte,'codpart') AND POSI():F,;
      ValPart  : H='Asignado'  :P='999,999,999,999.99':R,;
      MonTra   : H='Transfiere':P='999,999,999,999.99'
      
*** aqui me quedo *****

* and ver_pres(periodo+ALLT(m.uniges)+ALLT(m.unieje)+codcad+codfte)
      
 vMonAcu = 0
 vMonIng = 0
 vMonSal = 0
	  
 SELECT IteTra
 SEEK alltrim(m.periodo)+m.CodOpe+alltrim(m.TipDoc)+alltrim(m.NumDoc)
 
 *SCAN WHILE PERIODO+CodOpe+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)+uniges+unieje = alltrim(m.periodo)+m.CodOpe+alltrim(m.TipDoc)+alltrim(m.NumDoc)+ALLTRIM(m.UniGes)+ALLTRIM(m.UniEje)
 SCAN WHILE PERIODO+CodOpe+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC)= alltrim(m.periodo)+m.CodOpe+alltrim(m.TipDoc)+alltrim(m.NumDoc)
     IF ITETRA.MONTRA = 0
        IF RLOCK()
           DELETE NEXT 1
        ENDIF
     ELSE
        REPLACE FECHA WITH M.FECHA   
	    vMonAcu = vMonAcu + montra*iif(tipope='-',-1,1)
        vmonsal = vmonsal + iif(tipope='-',montra,0)
        vmoning = vmoning + iif(tipope='+',montra,0)
     ENDIF
     UNLOCK
 ENDSCAN
 m.TotAsig = vMonAcu
 m.Montrai = vmoning
 m.Montras = vmonsal
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5

 UNLOCK ALL

ACTIVATE SCREEN
vTempo = '                                                                           '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
SELECT TraPar
RETURN .T.

PROCEDURE Agreg_item
*-------------------
SELECT IteTra
IF F_Appd()
   REPLACE Periodo  WITH m.Periodo,;
   		   CodOpe   WITH m.codope ,;
   		   TipDoc   WITH m.TipDoc ,;
           NumDoc   WITH m.NumDoc ,;
           uniges   WITH ALLTRIM(m.UniGes),;
           UniEje   WITH ALLTRIM(m.UniEje),;
           Fecha    WITH m.Fecha 
	IF allt(m.moddoc)='HTA'
		REPLACE TipOpe  WITH '-'
   	ELSE
   		REPLACE TipOpe  WITH '+'
   	ENDIF
ENDIF
UNLOCK
RETURN


PROCEDURE Elimi_item
*-------------------
SELECT IteTra
vOrd=ORDER()
IF RLOCK()
   SELECT ItePar
   SEEK IteTra.periodo+allt(M.UNIGES)+ALLT(M.UNIEJE)+IteTra.codcad+IteTra.codfte+IteTra.CodPart
   IF FOUND() .AND. RLOCK()
      IF IteTra.TipOpe='-'
         REPLACE ItePar.Tra001  WITH ItePar.Tra001  + IteTra.MonTra
         REPLACE TraPar.MonTraS WITH TraPar.MonTraS - IteTra.MonTra
         SELECT IteTra
         DELETE NEXT 1
      ENDIF
      IF IteTra.TipOpe='+'
         REPLACE ItePar.Tra001  WITH ItePar.Tra001  - IteTra.MonTra
         REPLACE TraPar.MonTrai WITH TraPar.MonTrai - IteTra.MonTra
         SELECT IteTra
         DELETE NEXT 1
      ENDIF
   ENDIF
ELSE
   DO StandBy WITH 'No puede eliminar este Item.'
ENDIF
SELECT IteTra
SET ORDER TO vOrd
RETURN

PROCEDURE Anula
*--------------
* Anula el documento actual.
IF EOF()
   DO StandBy WITH Vmens08
   RETURN
ENDIF
IF yesno("¨ANULA Documento?")
   vAli=ALIAS()
   vOrd=ORDER()
   SELECT TraPar
   vPeriodo=ALLTRIM(TraPar.Periodo)
   vTipDoc =ALLTRIM(TraPar.TipDoc)
   vcodope =ALLTRIM(TraPar.codope)
   vNumDoc =ALLTRIM(TraPar.NumDoc)
   vgeseje = trapar.uniges+trapar.unieje
   SELECT IteTra
   GO TOP
   SCAN WHILE vPeriodo=Periodo and vcodope=codope and vTipDoc = ALLTRIM(TipDoc) .AND. vNumDoc = ALLTRIM(NumDoc) and vgeseje=uniges+unieje
        SELECT ItePar
        SEEK IteTra.Periodo+allt(M.UNIGES)+ALLT(M.UNIEJE)+IteTra.Codcad+IteTra.Codfte+IteTra.CodPart
        IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
           *REPLACE ItePar.Transf  WITH ItePar.Transf  - ABS(IteTra.MonTra)
           IF IteTra.TipOpe='-'
              REPLACE ItePar.ValPart WITH ItePar.tra001 + IteTra.MonTra
           ELSE
              REPLACE ItePar.ValPart WITH ItePar.tra001 - IteTra.MonTra
           ENDIF
        ENDIF
        SELECT IteTra
   ENDSCAN
   SELECT (vAli)
   SET ORDER TO vOrd
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT IteTra
      SEEK vPeriodo+vTipDoc+vcodope+vNumDoc+vgeseje
      IF FOUND()
         SCAN WHILE vPeriodo=Periodo and vcodope=codope and vTipDoc = ALLTRIM(TipDoc) .AND. vNumDoc = ALLTRIM(NumDoc) and vgeseje = uniges+unieje
              IF f_lock(1)
                 DELETE NEXT 1
              ENDIF
         ENDSCAN
      ENDIF
      SELECT TraPar
      IF !BOF()
         SKIP -1
      ELSE
         IF !EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
ENDIF
UNLOCK ALL
DO Vista
RETURN

FUNCTION Valida  && Suma el ingreso de todos los Items ingresados.
*--------------
parameter vAli,vtp
vAli=ALIAS()

SELECT IteTRA
SEEK m.Periodo+m.CodOpe+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc)+ALLTRIM(m.UniGes)+ALLTRIM(m.UniEje)
SCAN WHILE Periodo=m.Periodo and codope=m.codope and alltrim(TipDoc)=ALLTRIM(m.TipDoc) .AND. alltrim(NumDoc)=ALLTRIM(m.NumDoc) 
     SELECT Itepar
     SEEK IteTra.periodo+allt(M.UNIGES)+ALLT(M.UNIEJE)+IteTra.codcad+IteTra.codfte+IteTra.CodPart
     IF FOUND()
    	   vAntcan = iif(tiping='C',itepar.tra001,0)
    	   IF RLOCK() 
              REPLACE ItePar.Tra001  WITH roltra(),ITEPAR.NUMTRA WITH IteTra.NUMDOC
           ENDIF  
     ENDIF
     UNLOCK ALL
     SELECT IteTra
ENDSCAN
SELECT (vali)
RETURN

PROCEDURE ROLTRA
*---------------
private alis,vkeyf,vkeyi,vtp,vtotal
alis=alias()
sele itetra
vtp = recno()
vKeyi = periodo+codcad+codfte+CodPart+codope+numdoc+UniGes+UniEje
vKeyf = periodo+codcad+codfte+CodPart+codope+numdoc+UniGes+UniEje
go top
vtotal = 0
scan for periodo+codcad+codfte+CodPart+codope+numdoc+UniGes+UniEje = vkeyi
	vtotal = vTotal + montra*iif(tipope='-',-1,1)
endscan	
go vtp
select (alis)
return vtotal	



PROCEDURE Termi
*--------------
 vEn_accion = .F.
 DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
 DO Logos WITH Rotulo1,Rotulo2
 RELEASE    WINDOW Wind_C0
 RELEASE    WINDOW Wind_C2
 RELEASE    WINDOW WIND_3
 RELEASE    MENU   mMenu
 CLOSE DATA
RETURN

* Imprime documento

PROCEDURE impri
*--------------
SELE TRAPAR
SCATTER MEMVAR
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF LASTKEY()=27
	RETURN
ENDIF

vInd = SYS(3) + '.idx'
Rec = RECNO()
VKEY = M.PERIODO+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc)
SELECT ITETRA

SET RELATION TO M.PERIODO+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc) INTO TRAPAR
SET FILTER TO PERIODO+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)=VKEY


*if alltrim(m.tipfun)='I'
	DO REPORTE WITH 2,"Transf_I"," Reporte de Transferencias "
*else
    DO REPORTE WITH 2,"Transf_F"," Reporte de Transferencias "
*endif

SET SKIP TO
SET FILTER TO
SET RELATION TO
SELECT TRAPAR
GO Rec
DO vista
RETURN

FUNCTION Posi  &&Asigna el valor del campo.
*------------
vOrd=ORDER()
SELECT ItePar
SEEK IteTra.periodo+allt(m.uniges)+allt(m.unieje)+IteTra.Codcad+IteTra.Codfte+IteTra.CodPart
IF FOUND() AND ( RLOCK() OR F_LOCK(1) )
      IF EMPTY(IteTra.ValPart)
         REPLACE IteTra.ValPart WITH ItePar.ValPart
      ENDIF
ELSE
      DO STANDBY WITH 'No se tiene registrado esta partida'
ENDIF
SELECT IteTra
RETURN

FUNCTION v_Num
*------------
vOrd=ORDER()

SEEK m.periodo+m.codope+ALLTRIM(m.TipDoc)+ALLTRIM(m.NumDoc)+ALLT(m.UniGes)+ALLT(m.Unieje)

IF FOUND()
   DO StandBy WITH "Documento Existe"
ENDIF
SET ORDER TO vOrd
RETURN

PROCEDURE PENDIENTE
*------------------
DO CASE
   CASE ESTADO = '00' &&PENDIENTE
       REPLACE ESTADO WITH '10'
   CASE ESTADO = '10' &&APROBADO
       REPLACE ESTADO WITH '00'
ENDCAS
DO VISTA       
RETURN
