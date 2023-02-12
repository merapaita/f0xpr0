**--------------------------------------
** cons_nc.PRG
** Programa de Consulta
** de Notas de Contabilidad y otros Doc
** AUTOR : JAVIER FRIAS GOMEZ
** Estados : 00 Pendiente de Cancelaci¢n
**           50 Cancelado
**--------------------------------------

*- Guarda el menu y la pantalla anteri

SAVE SCREEN TO pta_comp
HIDE POPUP pop_01,menu
*- Mensajes de aviso al usuario
mverifd='    '
PRIVATE Vmens01,Vmens02,Vmens09,vMens20
Vmens01 = 'Í  Notas de Contabilidad ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '° Detalle °'
Vmens20 = 'Documento Ya Procesado en el Balance.'

xPos    = INT((80 - (LEN(ALLTRIM(cia)) + 2))/2)
Vmens01 = SUBSTR(Vmens01, 1, xPos+1) + ' ' + cia + ' ' + SUBSTR(Vmens01,xPos + LEN(ALLTRIM(cia)) + 3,80)


*- Abre archivos
USE Compro    IN  1 ORDER TAG Compro7                 ALIAS Comp
USE Diario    IN  2 ORDER TAG Diario11                ALIAS Diar
USE Cuentas   IN  3 ORDER TAG Cuentas1                ALIAS Cuen
USE Auxil     IN  6 ORDER TAG Auxil1                  ALIAS Auxi
USE CptObra   IN  7 ORDER TAG CptObra2                ALIAS Obra
USE Cajas     IN 10 ORDER TAG Cajas1                  ALIAS Caja
USE AutFin    IN 09 ORDER TAG AutFin1                 ALIAS AutFin
USE EstFin    IN 11 ORDER TAG EstFin1                 ALIAS EstFin
*USE HojCon    IN 14 ORDER TAG HojCon4                 ALIAS Hoja
USE Clase     IN 14 ORDER TAG Clase1                  ALIAS Clase

Idxauxi  = SYS(3) + '.idx'
SELE EstFin
INDEX ON cuenta FOR !EMPTY(Tipo) TO (idxauxi) UNIQUE

*- Copia estructura del diario a temporal
filaux  = SYS(3) + '.DBF'
SELECT 8
CREATE TABLE (filaux);
  (Comprob    C(014),;
   ComAdi     C(002),;
   Item       N(004),;
   NumMov     N(003),;
   FecEmi     D(008),;
   Cuenta     C(010),;
   Tipo       C(001),;
   Montos     N(18,3),;
   TipAux     C(002),;
   Registro   C(006),;
   DocRef     C(007),;
   DocAdi     C(002),;
   CodCtc     C(014),;
   NumChq     C(014),;
   CCosto     C(005),;
   CodDep     C(006),;
   TipDcr     C(002),;
   Estado     C(002),;
   FecVen     D(008),;
   SaldPen    N(18,3),;
   CodFte     C(003),;
   Nummesref  C(002),;   
   Periodoref C(002),;
   Tipfun     C(001),;
   CodArt     C(011),;
   Descri     C(135),;
   Codpart    C(005),;
   Codanal    C(005),;
   TipFin     C(004),;
   TFin95     C(004),;
   TipObr     C(001),;
   CodObra    C(004),;
   Reten      N(15,3),;
   PerHC      C(002),;
   Nummeshc   C(002),;
   NumHC      C(004),;
   PerHm      C(002),;
   NummesHm   C(002),;
   NumHm      C(004),;
   Liquida    C(001),;
   Liqusu     C(001))
USE

USE (filaux) IN 8 EXCLUSIVE                  ALIAS diax

*- Variables de trabajo (registro a trabajar)
PUBLIC m.comprob, m.comadi, m.TipDoc, m.tipdcr, m.codcal, m.montos, m.fecemi
PUBLIC m.docref, m.docadi, m.fecref,m.NumMov ,m.codpart
PUBLIC m.numchq , m.tipfun , m.codart, m.descri, m.codobra, m.tipobr
PUBLIC vNumCP, vNumHC, vMes, vNumOC, vNumPec, vItem, vCodPart, vCodSubp,vcoddep,vCodFte,vfecha
PUBLIC m.fecha, m.actmes, vLleno
PUBLIC m.OldDebe, m.OldHaber 
PUBLIC v_sumas, v_cuadre, m.estado, m.glosa1, m.glosa2, vCon, vCon1, vMto, vcodcal
PUBLIC m.totdeb, m.tothab, m.codctc, m.codprv, m.codcal, m.glosa3, m.glosa4,m.estcon
PUBLIC xmov, _msg1,_msg2,;
       xfecemi, xcuenta, xTipAux, xRegistro, xmontos, m.CodCtc, xcodfte, xTipfun,xCodart,xDescri,xcodpart,xcodanal,xtipObr,xCodObra
PUBLIC vErdad,auxerr,vUsuario
PUBLIC tiP,mFila
PUBLIC wnummov,wfecemi,wmontos,wTipAux,wregistro,wDocRef,wdocadi,wnumchq,wCCosto,wCodDep,wtipdcr,wcodctc,kcuenta,;
       wmonto,wspacio,wdestino,vIndPar,FMES,MesCerrado
       
PUBLIC vtipfin,vtipfin95,vAli,vDHCuentas,VDESTINO,VCODARTI,Vindpar,vasignafecha
PUBLIC filaux2,idxaux2,filaux3,idxaux3,DtotalDoc,HtotalDoc,vistadoc

wcodaux=space(02)   
vtipfin   = SPACE(8)
xTipAux   = SPACE(2)
xRegistro = SPACE(6)
wTipAux   = SPACE(2)
wRegistro = SPACE(6)
vIndPar   = SYS(3)+".IDX"
tcambia   =.F. 
auxerr    = .F.
Agrega    = .T.
vistadoc  = SPACE(3)
vasignafecha=.f.

STORE 0 TO m.item,vLleno,DtotalDoc,HtotalDoc

filaux2 = SYS(3) + '.DBF'
idxaux2 = SYS(3) + '.idx'
filaux3 = SYS(3) + '.DBF'
idxaux3 = SYS(3) + '.idx'

USE Parmae  IN  4 ORDER TAG Parmae1                 ALIAS Parma
DO indexa_par

SELECT parma

vUsuario = ALLTRIM(SUBSTR(SYS(0),1,6))

*- Mes y a¤o actual
SELECT parma
   
SEEK "MESANO"+"ACTUAL"
 _actmes = LEFT(descri,6)
 actmes = CTOD("01/"+SUBSTR(_actmes,5,2)+"/"+SUBSTR(_actmes,3,2) )
 fmes = SUBSTR(_actmes,5,2)

*- Si obliga a cuadre y al total de suma de cuentas
*- en el documento
SEEK "OPERAT"+"SUMAS"
v_sumas = (LEFT(descri,2)="SI")
SEEK "OPERAT"+"CUADRE"
v_cuadre = (LEFT(descri,2)="SI")
SEEK "CONTAB"+"CIERRE"
MesCerrado = VAL(codigoaux)


*- Inicia proceso
SELECT Comp
GO BOTTOM
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
ERASE (filaux)
ERASE(idxauxi)

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 12,79 DOUBLE ;
 TITLE " ² Notas de Contabilidad (Consulta) ² " COLOR SCHEME 10

 DEFINE WINDOW Wind_C2 FROM 13,00 TO 23,79 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_LQ FROM 10,00 TO 23,79 DOUBLE ;
 TITLE "±° Liquida Documentos °±   <F11>liquidar/anular liq.   <F10>Termina" COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 04,00 TO 23,58 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW wVerif FROM 12,20 TO 21,60 COLOR SCHEME 10 TITLE '° Consistencia ° ®ESC Continuar¯'


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO LIS_NOT && liscomp
 ON SELECTION PAD termi  OF mMenu DO termi
 ACTIVATE SCREEN
 
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @  0,02 SAY "   Documento:"
 @  0,41 SAY "Fecha:"
 @ 01,02 SAY "       Glosa:" 
 @ 02,02 SAY "       Glosa:" 
 @ 03,02 SAY "       Glosa:" 
 @ 04,02 SAY "       Glosa:"    
 @ 05,02 SAY "Cod.Proyecto:"
 @ 05,37 SAY "T. Gasto:"
 @ 06,02 SAY "C. Corriente:"
 @ 07,02 SAY "        Obra:"
 @ 08,02 SAY " Mod.Ejecuc.:"
 @ 09,02 SAY "  Total Debe:"
 @ 09,41 SAY "Total Haber:"
RETURN

PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 @  0,30 SAY "Fecha:"
 ON KEY LABEL F9  DO Vista_Det
 ON KEY LABEL F4  DO impri_nota
 ON KEY LABEL F11 DO Liqui_usu
 ON KEY LABEL F12 DO Cor_Obr 

 SELECT Comp
 SCATTER MEMVAR
 xCodfte = SUBSTR(m.CodCal,5,3)
 wtipo   = m.Tipdoc
 wMens   = PADC(IIF(m.estado='20',"Procesado",IIF(m.estado="10","        ","")),09," ")
 wColor  = IIF(m.estado='20',"7+/1",IIF(m.estado="10",SCHEME(10),SCHEME(10)))
 wcolor2 = IIF(!empty(m.usuario),'7+/5','7+/3')
 @  00,36 SAY REPLICATE(" ",43)
 @  00,16 SAY PADR(vTipoDoc(m.Comprob,m.ComAdi),14,' ')
 @  00,38 SAY m.Fecha
 @  00,69 SAY wMens COLOR &wColor
 @  00,47 SAY IIF(!empty(m.usuario),"Digit. por ","           ") + ;
              IIF(!empty(m.usuario),m.usuario,space(10)) COLOR &wcolor2
 @  01,16 SAY SUBSTR(m.Glosa1,1,60)
 @  02,16 SAY SUBSTR(m.Glosa2,1,60)
 @  03,16 SAY SUBSTR(m.Glosa3,1,60)
 @  04,16 SAY SUBSTR(m.Glosa4,1,60) 
 @  05,16 SAY SPACE(20)
 @  05,16 SAY m.CodCal
 @  05,47 SAY m.tipfun
 @  05,50 SAY IIF(!EMPTY(m.tipfun),VAL_PARA(m.tipfun,'TIPFUN','D',50,15),space(15))
 @  06,16 SAY m.CodCtc
 @  06,28 SAY DESCTC(m.CodCtc)
 @ 08,16  SAY m.Codpart  
 @  07,16 SAY m.Tipobr
 @  07,18 SAY m.Codobra
 
 
 IF ALLTRIM(m.Tipfun)='I' 
    IF !empty(m.codobra)
       =Val_Obra(m.CodObra,'m.CodObra',' ',27,40) 
       @ 07,23 SAY LEFT(wdestino,53)
       IF !empty(m.codpart)
           @ 08,16 SAY m.codpart+' '+VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),'D',16,50)
       ENDIF
   ELSE
       DO STANDBY WITH 'Error en C¢dificaci¢n de la Obra'
   ENDIF
 ELSE
    @ 07,16 SAY REPLICATE(" ",60)
    @ 08,16 SAY REPLICATE(" ",60)
 ENDIF
 
 @  9,16 SAY m.TotDeb  PICTURE "999,999,999,999.99"
 @  9,54 SAY m.TotHab  PICTURE "999,999,999,999.99"
 @ 10,00 SAY PADC("±° ®F4¯Imprime N/C   °   ®F9¯Detalle N/C   °±",78,' ') COLOR '7+/1'
 
 AuxErr=.F.
 DO Vista_Hijo
 IF AuxErr
    DO STANDBY WITH "Error en Codificaci¢n del Proveedor."
    AuxErr=.F.
ENDIF
RETURN


PROCEDURE Vista_Hijo         && Muestra items del documento
*-------------------
 SCATTER MEMVAR
 SELECT Diar
 vOrd=ORDER()
 SET ORDER TO TAG Diario11
 BROWSE;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_C2 KEY m.Comprob + m.ComAdi + xCodfte;   
   FIELDS;
   fecemi   :H='Emisi¢n',;
   coddep   :H='Depen',;
   cuenta   :H='  Cuenta',;
   Liqusu   :H='LQ',;
   vDebe    = IIF(tipo="D",TRANSFORM(montos,"99,999,999.99"),SPACE(13)) :H=" Debe M.Nac.",;
   vHaber   = IIF(tipo="H",TRANSFORM(montos,"99,999,999.99"),SPACE(13)) :H="Haber M.Nac.",;
   vProvee  = rAuxil( diar.tipaux,diar.registro ) :H="Proveedor" :30,;
   registro :H='C¢d.Aux.',;
   tipfin   :H='Tipo Fin.',;
   tfin95   :H='TipFin 95'
   
  *   nivel    = Val_Fun('Cuen','Cuenta','nivel',Diar.Cuenta):P="99":H="Nv",
 SET ORDER TO TAG vOrd
 SELECT Comp
 

RETURN

PROCEDURE  Vista_Det
*-------------------
 HIDE POPUP ALL
 ON KEY LABEL F9
 ON KEY LABEL F4
 SCATTER MEMVAR
 SELECT Diar
 vOrd=ORDER()
 SET ORDER TO TAG Diario11
 BROWSE;
   NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT ;
   WINDOW Wind_C2 KEY m.Comprob + m.ComAdi + xCodfte;   
   FIELDS;
  item     :H='Item'       :R,;  
  nummov   :H='Mov.'       :R,;
  fecemi   :H='Emisi¢n'    :R,;
  cuenta   :H=' Cuenta'    :R,;
  tipo     :H='D/H'        :R,;
  montos   :H=' Importe S/. ':P='99,999,999.99':R,;
  vProvee  = rAuxil( diaR.tipaux,diaR.registro ) :H="Proveedor" :30,;
  diaR.tipaux   :H='TipAux.'    :R,;
  diaR.registro :H='C¢d.Aux. '  :R,;
  ccosto   :H='C.Cto':R,;
  Tipfun   :H='Tipfun' :R,;
  CodArt   :H='Art¡culo' :R,;
  Descri   :H='Descripci¢n' :R,;
  Codpart  :H='Partida' :R,;
  Codanal  :H='Analirica' :R,;
  TipFin   :H='TipFin' :R,;
  TFin95   :H='TipFin 95':R


 SET ORDER TO TAG vOrd
 SELECT Comp
 DO VISTA
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT Comp
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 ACTIVATE SCREEN
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo

 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   TipDoc   :H= 'Tipo doc.',;
   Comprob  :H= 'Documento',;
   ComAdi   :H= 'CompAdi',;
   Glosa1   :H= '              Glosa',;
   Glosa2   :H= '              Glosa',;
   Totdeb   :H= '  Debe M.Nac.',;
   Tothab   :H= ' Haber M.Nac.'
   ON KEY LABEL F10

 DEACTIVATE WINDOW Wind_C4

 DO Vista
RETURN



PROCEDURE Busca              && Realiza b£squeda directa
*---------------
PARAMETER vis
IF PARAMETERS()=0
   	vis = .T.
ENDIF 
 ON KEY LABEL F4  	
 SELECT Comp
 SET ORDER TO TAG COMPRO7
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF
 IF LASTKEY()=27
    RETURN
 ENDIF
 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 10,12 TO 16,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE FMes      TO vM
 STORE SPACE(02) TO vT
 STORE SPACE(5)  TO Vbusca
 STORE SPACE(02) TO Vbusca1
 STORE SPACE(03) TO Vbusca2
 @ 0, 0 SAY '                     Mes  : ' GET vM      PICTURE '!!' VALID val_para(vm,'FECMES',' ',29,25)
 @ 1, 0 SAY '       Tipo de Documento  : ' GET vT      PICTURE '!!' VALID Val_para7(vT,'TIPDOC',' ',29,20)
 @ 2, 0 SAY '    N£mero  de documento  : ' GET Vbusca  PICTURE REPLICATE('!',5)
 @ 3, 0 SAY '        N£mero adicional  : ' GET Vbusca1 PICTURE '!!'  
 @ 4, 0 SAY 'Fuente de Financiamiento  : ' GET Vbusca2 PICTURE '!!!' 
 READ 
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT comp
    RETURN
 ENDIF
 xComprob = PADR(ALLTRIM(vT) + ALLTRIM(Vbusca) + ALLTRIM(vM),14,' ')
 xComAdi  = PADR(ALLTRIM(vBusca1),2,' ')
 xCodFte  = ALLTRIM(vBusca2)
 DocB     = xComprob+xComAdi+xCodFte
 IF EMPTY(vBusca) AND EMPTY(vBusca1) AND EMPTY(vBusca2)
    DocB = ALLTRIM(vT)
 ENDIF    
 IF !SEEK(DocB)
    DO standby WITH "Dicho Documento No Fue Encontrado."
    GOTO Vtemp
 ELSE
    IF vis
       DO Vista
    ENDIF
 ENDIF
 RETURN



PROCEDURE Anter              && Documento anterior
*--------------
 SELECT Comp
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
 SELECT Comp
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
 RELEASE    MENU   mMenu
 ON KEY LABEL F3
 ON KEY LABEL F4
 ON KEY LABEL F8
 ON KEY LABEL F9
 ON KEY LABEL F10
 CLOSE DATA
 ERASE (filaux)
RETURN


PROCEDURE sal_hijo
*-----------------

* Valida los totales para salir:
* la suma de cuentas
* los cargos con los abonos
* Verifica que no existan descuadres por redondeos; si lo hubieran
* permite ajustar estos redondeos regresando a la rutina Tra_Hijo.

valido    = .T.      && utilizada por tra_hijo.
redonerr  = .F.      && indica si existe error por redondeo.
rec       = IIF(EOF(),-1,RECNO())
STORE 0 TO m.totdeb, m.tothab
STORE 0 TO xtotdeb,xtothab

SELECT Diax
idxaux = SYS(3) + '.IDX'
INDEX ON STR(nummov,3)+STR(item,4) TO (idxaux)
DEFINE WINDOW _yy FROM 10,15 TO 16,65 COLOR SCHEME 10 TITLE " Valida montos por movimientos "
   xnummov = nummov
   STORE 0 TO xtotdeb, xtothab
   vReg    = RECNO()
   SUM diax.montos FOR diax.tipo ="H"  TO xtothab
   GO TOP

   SUM diax.montos FOR diax.tipo ="D"  TO xtotdeb
   GO TOP 
 
   IF xtotdeb # xtothab
      ACTIVATE WINDOW _yy
      @ 1,1  SAY "Mov. "+STR(xnummov,3) + " no cuadra "
      WAIT ''
   ENDIF
RELEA  WINDOW _yy
SET INDEX TO
ERASE (idxaux)
IF redonerr
   RETURN
ENDIF
  m.totdeb = xtotdeb
  m.tothab = xtothab
IF rec<0
   GO BOTTOM
ELSE
   GO rec
ENDIF
vali = .T.
IF m.totdeb # m.tothab
   IF !v_cuadre
      SET CONSOLE ON
      SET BELL TO 130,5
      _WRAP = .F.
      ? CHR(7)
      _WRAP = .T.
      SET BELL TO 512,2
      SET CONSOLE OFF
      DO standby WITH "No CUADRAN los cargos con los abonos"
      valido = .T.
   ELSE
      SET CONSOLE ON
      SET BELL TO 130,5
      _WRAP = .F.
      ? CHR(7)
      _WRAP = .T.
      SET BELL TO 512,2
      SET CONSOLE OFF
      DO standby WITH "No CUADRAN los cargos con los abonos"
      valido = .F.
  ENDIF

  IF !vali .AND. !valido
     IF !yesno("¨Cancela ingreso? <NO> = Corrige")
        RETURN
     ENDIF
  ENDIF
ENDIF
valido = valido .AND. vali

KEYBOARD CHR(23)
RETURN

PROCEDURE Voucher2     &&Programa reporteador de notas de Contabilidad
*-----------------
PARAMETERS XCOP
_PAGENO=1
vrec = RECNO()
DO CASE
	CASE _DEST='Impresora'
	    SET DEVICE TO PRINTER
	CASE _DEST='Pantalla '
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST='Archivo  '
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
mFila=0
clave = Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.CodCal,5,3)
SELECT Diar
SET ORDER TO TAG Diario17
SEEK Clave
STORE 0 TO TotalDebe , TotalHaber
DO cabe_notcon
mFila=8
DO WHILE !EOF() .AND. Clave = Comprob+ComAdi+CodFte 
   @ mFila,2   SAY SUBSTR(Diar.Cuenta,1,2)
   @ mFila,6   SAY busca_cta("SUBSTR(DIAR->CUENTA,1,2)+'00000000'")
   @ mFila,88  SAY IIF(Diar.Tipo='D',TRANSFORM(Diar.Montos,'999,999,999,999.99'),' ')
   @ mFila,109 SAY IIF(Diar.Tipo='H',TRANSFORM(Diar.Montos,'999,999,999,999.99'),' ')
   IF Diar.Tipo='D'
      TotalDebe = TotalDebe + Diar.Montos
   ELSE
      TotalHaber = TotalHaber + Diar.Montos
   ENDIF
   mFila = mFila + 1
   @ mFila,6   SAY Diar.Cuenta
   @ mFila,18  SAY busca_cta("DIAR.CUENTA")
   @ mFila,60  SAY Diar.Montos PICTURE "999,999,999,999.99"
   mFila = mFila + 1
   IF mFila>=50
      mFila = mFila + 1
      @  mFila,60 SAY "VAN ...    "
      @  mFila,88  SAY TotalDebe  PICTURE "999,999,999,999.99"
      @  mFila,109 SAY TotalHaber PICTURE "999,999,999,999.99"
      @ mFila+1,1  SAY CHR(12)  && Ejecta Hoja
      _PAGENO = _PAGENO + 1 
      DO cabe_notcon
      mFila = 8
      @  mFila,60 SAY "VIENEN ... "
      @  mFila,88  SAY TotalDebe  PICTURE "999,999,999,999.99"
      @  mFila,109 SAY TotalHaber PICTURE "999,999,999,999.99"
      mFila = mFila + 2
   ENDIF      
   SKIP
ENDDO
@  mFila,88  SAY "=================="
@  mFila,109 SAY "=================="
mFila = mFila + 1
@  mFila,60 SAY "TOTAL  ===>"
@  mFila,88  SAY TotalDebe  PICTURE "999,999,999,999.99"
@  mFila,109 SAY TotalHaber PICTURE "999,999,999,999.99"
mFila = mFila+2
@ mFila,2 SAY SUBSTR(COMP->GLOSA1,1,100) 
mFila = mFila+1
@ mFila,2 SAY SUBSTR(COMP->GLOSA2,1,100) 
mFila = mFila+1
@ mFila,2 SAY SUBSTR(COMP->GLOSA3,1,100) 
mFila = mFila+1
@ mFila,2 SAY SUBSTR(COMP->GLOSA3,101,100) 
mFila = mFila+1
@ mFila,2 SAY SUBSTR(COMP->GLOSA4,101,100) 
mFila = mFila+1
@ mFila,2 SAY 'Obra : '+COMP->TIPOBR+'-'+COMP->CODOBRA
@ mFila,17 SAY VER_OBRA()

mfila = mfila + 3

@ mFila,20 SAY "----------------------------" 
@ mFila,63 SAY "----------------------------" 
mFila = mFila + 1
@ mFila,27 SAY "PREPARADO POR" 
@ mFila,74 SAY "CONTADOR" 
* EJECT
@ mFila+1,1  SAY CHR(12)  && Ejecta Hoja
SET DEVICE TO SCREEN 
SELECT Diar
SET ORDER TO TAG Diario11
SELECT Comp
GOTO vrec
SET RELATION TO Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3) INTO Diar
SET SKIP TO Diar
RETURN


PROCEDURE Cabe_NotCon
*--------------------   && Cabecera nota de contabilidad
@ 0,0 SAY CHR(15)
@ 0,1 SAY ALLTRIM(Cia)
@ 0,122 SAY "Pag. " + STR(_pageno,2)
@ 1,0 SAY "Voucher2" 
@ 2,52 SAY CHR(14)+"NOTA DE CONTABILIDAD"
@ 4,0 SAY "DOCUMENTO :" 
@ 4,16 SAY Val_para5(Comp.tipdoc,'TIPDOC','D')
@ 4,70 SAY 'N§'
@ 4,73 SAY Comp.Comprob + " - " + Comp.ComAdi
@ 4,95 SAY "Fecha  :" + DTOC(Comp.Fecha)
@ 5,0 SAY "----------------------------------------------------------------------------------------------------------------------------------" 
@ 6,4 SAY "CUENTA" 
@ 6,33 SAY "DENOMINACION" 
@ 6,99 SAY "DEBE" 
@ 6,120 SAY "HABER" 
@ 7,0 SAY "----------------------------------------------------------------------------------------------------------------------------------" 
RETURN


PROCEDURE LisComp
*---------------
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado de Notas de Contabilidad' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista
vtipo = SPACE(2)
STORE DATE() TO xf1, xf2
SELECT Comp
rec = RECNO()
IF LASTKEY()=27
   RETURN
ENDIF

@ 01,01 SAY "         Documentos: " 
@ 01,22 GET vLista FUNCTION '^ Este Documento;Por Tipo de Doc.;Todos;ComPag' 
@ 04,01 SAY "  Tipo de Documento: "
@ 04,22 GET vtipo  WHEN vlista=2 PICTURE '!!' VALID Val_para7(vTipo,'TIPDOC',' ',22,20)
@ 06,01 SAY "        Fecha Desde: "
@ 06,22 GET xf1 WHEN vlista<>1
@ 08,01 SAY "        Fecha Hasta: "
@ 08,22 GET xf2 WHEN vlista<>1 VALID(xf2>=xf1) ERROR "Debe ser >= al 1ro"
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
IF OKCANCEL = 1
   idxaux = SYS(3) + '.IDX'
 *  Sele Diar
 *  SET ORDER TO TAG Diario17
 *  INDEX ON Diar.Comprob+Diar.ComAdi+Diar.CodFte+Diar.Tipo+SUBSTR(Diar.Cuenta,1,2)  TO (idxaux) 
   SELECT Comp
   rec = RECNO()
   SET RELATION TO Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3) INTO Diar
   SET SKIP TO Diar
   
   idxaux = SYS(3) + '.IDX'
   DO CASE 
   CASE vlista=1                        && Este documento
        DO repPRG WITH "Voucher2"," Listado Notas de Contabilidad "
   CASE vlista=2                        && Rango x Fechas y Tipo
     DO ESPERA WITH 1,' Espere un momento; Procesando Informaci¢n  '   
     INDEX ON Comp.Comprob+Comp.ComAdi+substr(Comp.Codcal,5,3) TO (idxaux) FOR SUBSTR(Comp.Comprob,1,2)=ALLTRIM(vtipo) AND BETWEEN(Comp.Fecha,xf1,xf2) 
     DO ESPERA WITH 2,' '
     SELECT Diar                     &&
     SET ORDER TO TAG Diario17       &&
     SELECT Comp                     &&
     IF EOF()
      	DO STANDBY WITH "No hay registros para procesar"
	 ELSE
        DO REPORTE WITH 2,"NOTA"," Reporte de Vouchers Digitados "
	 ENDIF
   CASE vlista=3
     DO ESPERA WITH 1,' Espere un momento; Procesando Informaci¢n  '   
     INDEX ON Comp.Comprob+Comp.ComAdi+substr(Comp.Codcal,5,3) TO (idxaux) FOR BETWEEN(Comp.Fecha,xf1,xf2) &&Comp.fecha>=xf1 .AND. Comp.fecha<=xf2
     DO ESPERA WITH 2,' '
     SELECT Diar                     &&
     SET ORDER TO TAG Diario17       &&
     SELECT Comp                     &&
     IF EOF()
       	DO STANDBY WITH "No hay registros para procesar"
	 ELSE
        DO REPORTE WITH 2,"NOTA"," Reporte de Vouchers Digitados " 
	 ENDIF
   CASE vlista=4
        DO repPRG WITH "Compro_con"," Comprobante de Pago "
 ENDCASE
 SET INDEX TO
 ERASE (idxaux)
* SET INDEX TO
* ERASE (idxaux1)
 
 ENDIF
 
 SET RELATION OFF INTO DIAR
 SELE Comp  
 SET ORDER TO tag Compro7                 
 SELE Diar 
 SET ORDER TO tag Diario11                 
 SELECT COMP
* IF LASTKEY()<>27
*    IF rec > 0
  GO rec
*    ENDIF
* ENDIF    
 DO vista
RETURN



FUNCTION ver_obra
*------------------
* Funcion lee obra para el reporte de notas de contabilidad
*----------------------------------------------------------
*fefe
IF !EMPTY(Comp.Codobra)
    =Val_Obra(Comp.CodObra,'Comp.Codobra','D') 
    RETURN wdestino   
ELSE
    RETURN ' '
ENDIF    
RETURN ' '

FUNCTION rAuxil
*--------------
PARAMETERS vtipaux,vregistro
* Retorna la descripci¢n del proveedor,
* y a la vez actualiza una variable l¢gica AuxErr
* seg£n haya o no error en codificaci¢n del proveedor.

*STORE .F. TO auxerr
IF EMPTY(vtipaux) OR EMPTY(vRegistro)
   STORE .T. TO auxerr
ENDIF   
AreaActual  = SELECT()
Orden       = ORDER()
vret = SPACE(0)
SELECT Auxi
SEEK vTipAux+vRegistro
Existe      = FOUND()
auxerr      = IIF(FOUND(),.F.,.T.)
IF Existe
   vRet   = Auxi.Descri
ENDIF
SELECT(AreaActual)
SET ORDER TO (Orden)
*IF !AuxErr
*   Auxerr = !Existe
*ENDIF   
RETURN vRet
   

PROCEDURE Indexa_Par
*-------------------
SELECT parma
SET ORDER TO
INDEX ON CodigoAux TO (vIndPar) FOR parma.tipo='TIPDOC' AND !EMPTY(parma.codigoaux)
SET INDEX TO
SET ORDER TO 1
RETURN .T.


FUNCTION vTipoDoc
*----------------
PARAMETERS xCom,xAdi
areactual= SELECT()
ordenactu= ORDER()
vTipoDoc = LEFT(xCom,2)
vNumero  = SUBSTR(xCom,3,5)
vAdicio  = IIF(EMPTY(xAdi),"   ","-"+xAdi)

SELE parma
SET INDEX TO (vindpar)
SEEK vTipoDoc
vret     = LEFT(parma.codigo,3)+' '+vNumero+vAdicio
vistadoc = LEFT(parma.codigo,3)
SET INDEX TO
SET ORDER TO 1

SELECT(areactual)
SET ORDER TO (ordenactu)

RETURN vret


PROCEDURE Cabe_comproCon
*--------------------   && Cabecera Comprobante de Pago
@ 0,0 SAY CHR(27)
@ 0,2 SAY ALLTRIM(Cia)
@ 0,68 SAY "Pag. " + STR(_pageno,2)
@ 1,01  SAY 'Compro'
@ 1,58 SAY "N§:" +  SUBSTR(Comp.Comprob,4,4) + " - " + Comp.ComAdi
@ 2,58 SAY "Fecha  :" + DTOC(Comp.Fecha)
@ 3,20 SAY CHR(14)+"COMPROBANTE DE PAGO"
@ 5,0 SAY "NOMBRE : " + rAuxil(diar.tipaux,diar.registro)
*  wmonso=m.montos*m.tipocam
@ 6,0 SAY "SON    : " + uPPER(xletras(Diar.montos))+' Nuevos Soles.'
*@ 7,0 say "("+UPPER(xletras(m.monto))+")" + "Dolares Americanos"
@ 8,0 SAY REPLICA("-",78)
@ 9,0 SAY "                                  CONCEPTO"
@10,0 say REPLICA("-",78)
@12,20 say "CUENTA CORRIENTE:"+ m.Codctc
@14,2 say glosa1
@15,2 SAY GLOSA2
@16,2 SAY GLOSA3
*@18,2 SAY "T/C:"+tran(tipocam,'99.99')+'       '+'US $:'+ TRAN(MONTO,'999,999.99')+'       '+"N§ CHEQUE:"+ comp.numchq
@19,0 say REPLICA("-",78)
@21,0 SAY "                             CONTABILIDAD PATRIMONIAL"
@23,0 SAY "           CUENTA                    DEBE                HABER "
RETURN


Procedure Compro_con && programa reporteador de Comprobantes de Pago
*-----------------
PARAMETERS XCOP
_PAGENO=1
vrec = RECNO()
DO CASE
	CASE _DEST='Impresora'
	    SET DEVICE TO PRINTER
	CASE _DEST='Pantalla '
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST='Archivo  '
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
mFila=0
clave = Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.CodCal,5,3)
SELECT Diar
SET ORDER TO TAG Diario17
SEEK Clave
STORE 0 TO TotalDebe , TotalHaber
DO cabe_comprocon
mFila=24
DO WHILE !EOF() .AND. Clave = Comprob+ComAdi+CodFte 
   @ mfila,6   say diar.cuenta
   @ mFila,25  SAY IIF(Diar.Tipo='D',TRANSFORM(Diar.Montos,'999,999,999,999.99'),' ')
   @ mFila,45 SAY IIF(Diar.Tipo='H',TRANSFORM(Diar.Montos,'999,999,999,999.99'),' ')
   IF Diar.Tipo='D'
      TotalDebe = TotalDebe + Diar.Montos
   ELSE
      TotalHaber = TotalHaber + Diar.Montos
   ENDIF
   mFila = mFila + 1
   *@ mFila,6   SAY Diar.Cuenta
   *@ mFila,18  SAY busca_cta("DIAR.CUENTA")
   *@ mFila,60  SAY Diar.Montos PICTURE "999,999,999,999.99"
   *mFila = mFila + 1
   IF mFila>=50
      mFila = mFila + 1
      @  mFila,60 SAY "VAN ...    "
      @  mFila,25  SAY TotalDebe  PICTURE "999,999,999,999.99"
      @  mFila,45 SAY TotalHaber PICTURE "999,999,999,999.99"
      @ mFila+1,1  SAY CHR(12)  && Ejecta Hoja
      _PAGENO = _PAGENO + 1 
      DO cabe_notcon
      mFila = 8
      @  mFila,60 SAY "VIENEN ... "
      @  mFila,25  SAY TotalDebe  PICTURE "999,999,999,999.99"
      @  mFila,45 SAY TotalHaber PICTURE "999,999,999,999.99"
      mFila = mFila + 2
   ENDIF      
   SKIP
ENDDO
@  mFila,24  SAY "=================="
@  mFila,44 SAY "=================="
mFila = mFila + 1
@  mFila,10 SAY "TOTAL  ===>"
@  mFila,24  SAY TotalDebe  PICTURE "999,999,999,999.99"
@  mFila,44 SAY TotalHaber PICTURE "999,999,999,999.99"
mFila = mFila+10
@ mFila,10 SAY "------------------"
@ mFila,30 SAY "------------------"
@ mFila,50 say "------------------"

mFila = mFila + 1
@ mFila,10 SAY "  RECIBI CONFORME"
@ mFila,35 SAY "  TESORERIA"
@ mfila,55 say "  CONTADOR "
@ mfila+1,10 say "L.E. # ............"
@ mFila+1,1  SAY CHR(12)  && Ejecta Hoja
SET DEVICE TO SCREEN 
SELECT Diar
SET ORDER TO TAG Diario11
SELECT Comp
GOTO vrec
SET RELATION TO Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3) INTO Diar
SET SKIP TO Diar
RETURN



FUNCTION Glosas
*--------------
r  = ""
gl = ALLTRIM( UPPER ( T_Com.Glosa1 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+gl
ENDIF
gl = ALLTRIM( UPPER ( T_Com.Glosa2 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( T_Com.Glosa3 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
gl = ALLTRIM( UPPER ( T_Com.Glosa4 ) )
IF !EMPTY(gl) AND gl<>REPLICATE('-',LEN(gl)) AND gl<>REPLICATE('=',LEN(gl))
   r = r+' '+gl
ENDIF
RETURN IIF(!Empty(r),ALLTRIM(r),'No se encontr¢ detalle/glosa para este Documento.')




PROCEDURE Lis_Not
*----------------
*- Listado de Notas de Contabilidad
DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado de Notas de Contabilidad' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1  TO vLista
vtipo = SPACE(2)
STORE DATE() TO xf1, xf2
SELECT Comp
rec = RECNO()
IF LASTKEY()=27
   RETURN
ENDIF


@ 01,01 SAY "         Documentos: " 
@ 01,22 GET vLista FUNCTION '^ Este Documento;Por Tipo de Doc.;Todos;ComPag' 
@ 04,01 SAY "  Tipo de Documento: "
@ 04,22 GET vtipo  WHEN vlista=2 PICTURE '!!' VALID Val_para7(vTipo,'TIPDOC',' ',22,20)
@ 06,01 SAY "        Fecha Desde: "
@ 06,22 GET xf1 WHEN vlista<>1
@ 08,01 SAY "        Fecha Hasta: "
@ 08,22 GET xf2 WHEN vlista<>1 VALID(xf2>=xf1) ERROR "Debe ser >= al 1ro"
@ 11,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
IF OKCANCEL = 1
   = db_temp()
   idxaux = SYS(3) + '.IDX'
   SELECT Comp
   rec = RECNO()
   SET RELATION TO Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3) INTO Diar
   SET SKIP TO Diar
   idxaux = SYS(3) + '.IDX'
   
   DO CASE 

      CASE vlista=1                        && Este documento
           SELECT comp
           SCATTER MEMVAR
           SELECT t_com
           =f_appd()
           GATHER MEMVAR
           SELECT diar
           vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
           SCAN WHILE vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
                SCATTER MEMVAR
                SELECT T_dia
                SEEK LEFT(diar.cuenta,3)+diar.tipo
                IF !FOUND()
                    =f_appd()
                     REPLACE t_dia.Comprob   WITH diar.comprob
                     REPLACE t_dia.ComAdi    WITH diar.comadi
                     REPLACE t_dia.FecEmi    WITH diar.fecemi
                     REPLACE t_dia.Cuenta    WITH diar.cuenta
                     REPLACE t_dia.Tipo      WITH diar.tipo
                     REPLACE t_dia.Montos    WITH diar.montos
                     REPLACE t_dia.TipAux    WITH diar.tipaux
                     REPLACE t_dia.Registro  WITH diar.registro
                     REPLACE t_dia.CodFte    WITH diar.codfte
                     REPLACE t_dia.Tipfun    WITH diar.tipfun
                     REPLACE t_dia.TipObr    WITH diar.tipobr
                     REPLACE t_dia.CodObra   WITH diar.codobra
               ELSE
                     REPLACE t_dia.Montos    WITH t_dia.montos + diar.montos
               ENDIF
               SELECT diar
               vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
           ENDSCAN
           
           SELECT t_dia
           SET INDEX TO
           INDEX ON COMPROB+COMADI+CODFTE+t_dia.tipo+t_DIA.CUENTA TO (idxAux2)
           GO TOP
           SELECT T_Com
           SET RELATION TO T_Com.Comprob+T_Com.ComAdi+SUBSTR(T_Com.Codcal,5,3) INTO T_DIA
           GO TOP
           SELECT t_dia
           IF !EOF()
              DO REPORTE WITH 2,"NOTA"," Listado Nota de Contabilidad "
           ELSE
              DO STANDBY WITH 'No hay registros para procesar.'
           ENDIF
           
      CASE vlista=2                        && Rango x Fechas y Tipo

           DO ESPERA WITH 1,'Notas de Contabilidad en Proceso ... '   
           SELECT comp
           INDEX ON Comp.Comprob+Comp.ComAdi+substr(Comp.Codcal,5,3) TO (idxaux) FOR SUBSTR(Comp.Comprob,1,2)=ALLTRIM(vtipo) AND BETWEEN(Comp.Fecha,xf1,xf2) .AND. ESTADO#"5599"
           
           *- Se trasladan estos documentos a los archivos temporales:
           SELECT comp
           GO TOP
           nr = 0
           DO WHILE !EOF('comp')
               SCATTER MEMVAR
               nr=nr+1
               @ 0,0 SAY alltrim(str(nr))
               SELECT t_com
               =f_appd()
               GATHER MEMVAR
               
               SELECT diar 
               vDoc = Comp.COMPROB+Comp.COMADI+SUBSTR(comp.CodCal,5,3)
               SCAN FOR vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
                    SCATTER MEMVAR
                    SELECT T_dia
                    SEEK Diar.Comprob + Diar.Comadi + Diar.CodFte + LEFT(diar.cuenta,3)+diar.tipo
                    IF !FOUND()
                       =f_appd()
                       REPLACE t_dia.Comprob   WITH diar.comprob
                       REPLACE t_dia.ComAdi    WITH diar.comadi
                       REPLACE t_dia.FecEmi    WITH diar.fecemi
                       REPLACE t_dia.Cuenta    WITH diar.cuenta
                       REPLACE t_dia.Tipo      WITH diar.tipo
                       REPLACE t_dia.Montos    WITH diar.montos
                       REPLACE t_dia.TipAux    WITH diar.tipaux
                       REPLACE t_dia.Registro  WITH diar.registro
                       REPLACE t_dia.CodFte    WITH diar.codfte
                       REPLACE t_dia.Tipfun    WITH diar.tipfun
                       REPLACE t_dia.TipObr    WITH diar.tipobr
                       REPLACE t_dia.CodObra   WITH diar.codobra
                    ELSE
                       REPLACE t_dia.Montos    WITH t_dia.montos + diar.montos
                    ENDIF
                    SELECT diar
                    vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
              ENDSCAN
           
              SELECT comp
              SKIP
           ENDDO
           DO ESPERA WITH 2
           
           SELECT t_dia
           SET INDEX TO
           INDEX ON COMPROB+COMADI+CODFTE+t_dia.tipo+t_DIA.CUENTA TO (idxAux2)
           
           SELECT T_Com
           INDEX ON COMPROB+COMADI+subs(codcal,5,3) TO (idxAux3)
           
           SELECT t_dia
           SET RELATION TO T_dia.Comprob+T_dia.ComAdi+T_dia.Codfte INTO T_com
           set skip to t_com
           GO TOP
           IF !EOF()
              DO REPORTE WITH 2,"NOTA","Notas de Contabilidad por Tipo de Documento"
           ELSE
              DO STANDBY WITH 'No hay registros para procesar.'
           ENDIF
       	   
      CASE vlista=3
           DO ESPERA WITH 1,' Espere un momento; Procesando Informaci¢n  '   
           INDEX ON Comp.Comprob+Comp.ComAdi+substr(Comp.Codcal,5,3) TO (idxaux) FOR BETWEEN(Comp.Fecha,xf1,xf2)

           *- Se trasladan estos documentos a los archivos temporales:
           SELECT comp
           GO TOP
           nr = 0
           DO WHILE !EOF('comp')
               SCATTER MEMVAR
               nr=nr+1
               @ 0,0 SAY alltrim(str(nr))
               SELECT t_com
               =f_appd()
               GATHER MEMVAR
               
               SELECT diar 
               vDoc = Comp.COMPROB+Comp.COMADI+SUBSTR(comp.CodCal,5,3)
               SCAN FOR vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
                    SCATTER MEMVAR
                    SELECT T_dia
                    SEEK Diar.Comprob + Diar.Comadi + Diar.CodFte + LEFT(diar.cuenta,3)+diar.tipo
                    IF !FOUND()
                       =f_appd()
                       REPLACE t_dia.Comprob   WITH diar.comprob
                       REPLACE t_dia.ComAdi    WITH diar.comadi
                       REPLACE t_dia.FecEmi    WITH diar.fecemi
                       REPLACE t_dia.Cuenta    WITH diar.cuenta
                       REPLACE t_dia.Tipo      WITH diar.tipo
                       REPLACE t_dia.Montos    WITH diar.montos
                       REPLACE t_dia.TipAux    WITH diar.tipaux
                       REPLACE t_dia.Registro  WITH diar.registro
                       REPLACE t_dia.CodFte    WITH diar.codfte
                       REPLACE t_dia.Tipfun    WITH diar.tipfun
                       REPLACE t_dia.TipObr    WITH diar.tipobr
                       REPLACE t_dia.CodObra   WITH diar.codobra
                    ELSE
                       REPLACE t_dia.Montos    WITH t_dia.montos + diar.montos
                    ENDIF
                    SELECT diar
                    vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
              ENDSCAN
           
              SELECT comp
              SKIP
           ENDDO
           DO ESPERA WITH 2
           
           SELECT t_dia
           SET INDEX TO
           INDEX ON COMPROB+COMADI+CODFTE+t_dia.tipo+t_DIA.CUENTA TO (idxAux2)
           
           SELECT T_Com
           INDEX ON COMPROB+COMADI+subs(codcal,5,3) TO (idxAux3)
           
           SELECT t_dia
           SET RELATION TO T_dia.Comprob+T_dia.ComAdi+T_dia.Codfte INTO T_com
           set skip to t_com
           GO TOP
           IF !EOF()
              DO REPORTE WITH 2,"NOTA","Notas de Contabilidad"
           ELSE
              DO STANDBY WITH 'No hay registros para procesar.'
           ENDIF
	       
      CASE vlista=4
           DO repPRG WITH "Compro_con"," Comprobante de Pago "
           
    ENDCASE
    SELECT comp
    SET INDEX TO
    USE IN 12
    USE IN 13
    ERASE (idxaux)
    ERASE (filaux2)
    ERASE (idxaux2)
    ERASE (filaux3)
    ERASE (idxaux3)

 ENDIF
 
 SELECT COMP
 SET RELATION OFF INTO DIAR
 SELE Comp  
 SET ORDER TO tag Compro7                 
 SELE Diar 
 SET ORDER TO tag Diario11                 
 SELECT COMP
 GO rec
 DO vista
RETURN


FUNCTION db_temp
*---------------
*- Copia estructura del compro a temporal
SELECT comp
COPY STRUCTURE TO (filaux3)
SELECT 12
USE (filaux3) ALIAS t_com EXCL


*- Copia estructura del diario a temporal
SELECT diar
filaux2 = SYS(3) + '.DBF'
idxaux2 = SYS(3) + '.IDX'
SELECT 13
CREATE TABLE (filaux2);
  (Comprob   C(014),;
   ComAdi    C(002),;
   FecEmi    D(008),;
   Cuenta    C(10),;
   Tipo      C(1),;
   Montos    N(18,3),;
   TipAux    C(2),;
   Registro  C(6),;
   CodFte    C(3),;
   Tipfun    C(1),;
   TipObr    C(1),;
   CodObra   C(4))
USE
USE (filaux2) IN 13 EXCLUSIVE ALIAS t_dia
SELECT 13
INDEX ON t_dia.Comprob + t_dia.comadi + t_dia.codfte + t_dia.cuenta + t_dia.tipo TO (idxAux2)
RETURN

FUNCTION t_deb
*-------------
area = SELECT()
SELECT T_Dia
vDoc  = t_dia.comprob + t_dia.comadi + t_dia.CodFte
vCuen = LEFT(t_dia.cuenta,2)
vTipo = 'D'
vRegi = RECNO()
SUM TO vtoD FOR  (vDoc  = t_dia.comprob + t_dia.comadi + t_dia.codfte) AND (t_dia.tipo=vTipo) AND (LEFT(t_dia.cuenta,2)=vCuen)
SELECT(area)
GOTO vRegi
RETURN vtoD


FUNCTION t_Hab
*-------------
area = SELECT()
SELECT T_Dia
vDoc  = t_dia.comprob + t_dia.comadi + t_dia.CodFte
vCuen = LEFT(t_dia.cuenta,2)
vTipo = 'H'
vRegi = RECNO()
SUM TO vtoH FOR  (vDoc  = t_dia.comprob + t_dia.comadi + t_dia.codfte) AND (t_dia.tipo=vTipo) AND (LEFT(t_dia.cuenta,2)=vCuen)
SELECT(area)
GOTO vRegi
RETURN vtoH


FUNCTION Inic_tot
*-----------------
*- Inicializa las variables para los totales por documento
STORE 0 TO DtotalDoc,HtotalDoc
RETURN ' '


FUNCTION suma_doc
*----------------
*- Suma el total del debe y haber por documento
DtotalDoc = DtotalDoc+IIF(t_Dia.tipo='D',t_dia.montos,0)
HtotalDoc = HtotalDoc+IIF(t_Dia.tipo='H',t_dia.montos,0)
RETURN ' '


FUNCTION DESCTC
*--------------
PARAMETER vcodctc
IF EMPTY(vcodctc)
   RETURN REPL(' ',40)
ENDIF   
AREA = SELECT()
SELECT Caja
SEEK vcodctc
vRet = IIF(FOUND(),descri,repl('?',40))
SELECT(area)
RETURN vret


PROCEDURE impri_nota
*--------------------
*- Imprime la Nota de Contabilidad actual al presionar F4
ON KEY LABEL F4   && Desactiva la tecla F4
DO cierra
= db_temp()       && Crea archivos temporales para el compro y diario
USE Usuario IN 15 ORDER TAG usuario1 ALIAS Usu

idxaux = SYS(3) + '.IDX'

SELECT Comp
rec = RECNO()

SET RELATION TO Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3) INTO Diar
SET SKIP TO Diar
idxaux = SYS(3) + '.IDX'

*- Copia el registro actual del compro en el archivo t_com
SELECT comp
SCATTER MEMVAR
SELECT t_com
=f_appd()
GATHER MEMVAR

*- Copia todos los registros del diario en el archivo temporal t_dia
SELECT diar
vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
SCAN WHILE vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
     SCATTER MEMVAR
     SELECT T_dia
     SEEK diar.cuenta+diar.tipo
     IF !FOUND()
         =f_appd()
          REPLACE t_dia.Comprob   WITH diar.comprob
          REPLACE t_dia.ComAdi    WITH diar.comadi
          REPLACE t_dia.FecEmi    WITH diar.fecemi
          REPLACE t_dia.Cuenta    WITH diar.cuenta
          REPLACE t_dia.Tipo      WITH diar.tipo
          REPLACE t_dia.Montos    WITH diar.montos
          REPLACE t_dia.TipAux    WITH diar.tipaux
          REPLACE t_dia.Registro  WITH diar.registro
          REPLACE t_dia.CodFte    WITH diar.codfte
          REPLACE t_dia.Tipfun    WITH diar.tipfun
          REPLACE t_dia.TipObr    WITH diar.tipobr
          REPLACE t_dia.CodObra   WITH diar.codobra
    ELSE
          REPLACE t_dia.Montos    WITH t_dia.montos + diar.montos
    ENDIF
    SELECT diar
    vDoc = diar.COMPROB+diar.COMADI+diar.CODFTE
ENDSCAN

*- Relaciona los archivos temporales e imprime la nota de contabilidad
SELECT t_dia
SET INDEX TO
INDEX ON COMPROB+COMADI+CODFTE+t_dia.tipo+t_DIA.CUENTA TO (idxAux2)
GO TOP
SELECT T_Com
SET RELATION TO T_Com.Comprob+T_Com.ComAdi+SUBSTR(T_Com.Codcal,5,3) INTO T_DIA
SET RELATION TO t_com.usuario+SPACE(5) INTO usu

GO TOP
SELECT t_dia
IF !EOF()
   DO REPORTE WITH 2,"NOTA"," Listado Nota de Contabilidad "
ELSE
   DO STANDBY WITH 'No hay registros para procesar.'
ENDIF
SELECT comp
SET INDEX TO

*- Cierra y elimina los archivos temporales
USE IN 12
USE IN 13
USE IN 15
ERASE (idxaux)
ERASE (filaux2)
ERASE (idxaux2)
ERASE (filaux3)
ERASE (idxaux3)
DO abre

*- Selecciona nuevamente los archivos principales
SELECT COMP
SET RELATION OFF INTO DIAR
SELE Comp  
SET ORDER TO tag Compro7                 
SELE Diar 
SET ORDER TO tag Diario11                 
SELECT COMP
GO rec
DO vista
RETURN


FUNCTION partida
*---------------
* Visualiza la partida gen‚rica en el caso que el tipo de gasto sea
* por inversi¢n y tenga hoja de control.

vAlias=SELECT()
SELECT hoja
vPerRef = SUBS(DTOC(comp.fecha),7,2)
vMes    = SUBS(DTOC(comp.fecha),4,2)
vTipDoc = vistadoc
vNumRef = SUBS(comp.comprob,4,4)
vret    = SPACE(70)
SEEK vPerRef + vMes + vTipDoc + vNumRef

IF FOUND()
   vret = hoja.codpart
ENDIF
SELECT(vAlias)
RETURN vret


FUNCTION MFecha
*--------------
PARAMETER vmes
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM( ALLTRIM(SUBSTR(Meses,VAL(VMes)*9-8,9)) + ' ' )


FUNCTION busca_cta
*----------------
PARAMETERS vcuenta 
vAlias = SELECT()
SELECT Cuen
SEEK &vcuenta 
IF FOUND()
   SELE(VAlias)
   RETURN  ALLTRIM(Cuen.descri) 
ELSE
   SELE(VAlias)
   RETURN  'Cuenta no Existe'    
ENDIF
RETURN 


FUNCTION SUMDEB
*----------------
PRIVATE VREC
wmonto=0
wspacio=SPACE(20)
Kcuenta=SUBSTR(diar.Cuenta,1,2)
SELE Diar
vrec=recno()
SUM Diar.Montos to wmonto FOR SUBSTR(diar.Cuenta,1,2)=Kcuenta AND Diar.tipo='D' AND Diar.Comprob+Diar.ComAdi+Diar.Codfte = Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3)
GO vrec
RETURN wmonto
   

FUNCTION SUMHAB
*----------------
PRIVATE VREC
wmonto=0
wspacio=SPACE(20)
Kcuenta=SUBSTR(diar.Cuenta,1,2)
Sele Diar
vrec=recno()
SUM Diar.Montos TO wmonto FOR SUBSTR(diar.Cuenta,1,2)=Kcuenta and Diar.tipo='H' AND Diar.Comprob+Diar.ComAdi+Diar.Codfte = Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.Codcal,5,3)
GO vrec
RETURN wmonto


PROCEDURE cierra
*---------------
* Cierra algunas areas para imprimir la nota de contabilidad

USE    IN 6  && Auxi
USE    IN 9  && AutFin
USE    IN 11 && EstFin

RETURN

PROCEDURE abre
*-------------
* Vuelve a abrir las areas despu‚s de imprimir la nota de contabilidad

USE Auxil     IN 6  ORDER TAG Auxil1                  ALIAS Auxi
USE AutFin    IN 9  ORDER TAG AutFin1                 ALIAS AutFin
USE EstFin    IN 11 ORDER TAG EstFin1                 ALIAS EstFin

RETURN
