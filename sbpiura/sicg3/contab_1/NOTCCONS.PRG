**--------------------------------------
** NOTCON1.PRG
** Programa de Ingreso
** de Notas de Contabilidad para Constructivo
** Reingenieria de Procesos a JC
** AUTOR : UDEP - FMC - 1995
** Estados : 00 Pendiente de Cancelaci¢n
**           50 Cancelado
**--------------------------------------

*- Guarda el menu y la pantalla anterior

SAVE SCREEN TO pta_comp
HIDE POPUP pop_01,menu
*- Mensajes de aviso al usuario
mverifd='    '
PRIVATE Vmens01,Vmens02,Vmens09,vMens20
Vmens01 = ' Notas Contab. Constructivo '
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = ' °Detalle° '
Vmens20 = 'Documento Ya Procesado en el Balance.'

xPos    = INT((80 - (LEN(ALLTRIM(cia)) + 2))/2)
**Vmens01 = SUBSTR(Vmens01, 1, xPos+1) + ' ' + cia + ' ' + SUBSTR(Vmens01,xPos + LEN(ALLTRIM(cia)) + 3,80)


*- Abre archivos
USE Compro    IN  1 ORDER TAG Compro7                 ALIAS Comp
USE Diario    IN  2 ORDER TAG Diario11                ALIAS Diar
USE Cuentas   IN  3 ORDER TAG Cuentas1                ALIAS Cuen
USE CtoCto    IN  5 ORDER TAG CtoCto1                 ALIAS CtoC
USE Auxil     IN  6 ORDER TAG Auxil1                  ALIAS Auxi
*USE CptObra   IN  7 ORDER TAG CptObra2               ALIAS Obra
*USE Obra   IN  7 ORDER TAG Obra2                      ALIAS Obra
USE Cajas     IN 10 ORDER TAG Cajas1                  ALIAS Caja
USE AutFin    IN 09 ORDER TAG AutFin1                 ALIAS AutFin
USE EstFin    IN 11 ORDER TAG EstFin1                 ALIAS EstFin
USE Clase     IN 14 ORDER TAG Clase1                  ALIAS Clase

Idxauxi  = SYS(3) + '.idx'
SELE EstFin
INDEX ON cuenta FOR !EMPTY(Tipo) TO (idxauxi) UNIQUE

*- Copia estructura del diario a temporal
filaux  = SYS(3) + '.DBF'

*-M s apropiado
SELECT DIAR
COPY STRUCTURE TO (filaux)
SELECT 8


*---Mucho C¢digo, 
*CREATE TABLE (filaux);
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
   SaldPen   N(18,3),;
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
   Reten      C(001),;
   TipDoc     C(003),;
   CajChi     C(001),;
   Liquida    C(001),;
   Liqusu     C(001))
*-----------
 
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
PUBLIC m.totdeb, m.tothab, m.codctc, m.codprv, m.codcal, m.glosa3, m.glosa4
PUBLIC xmov, _msg1,_msg2,;
       xfecemi, xcuenta, xTipAux, xRegistro, xmontos, m.CodCtc, xcodfte, xTipfun,xCodart,xDescri,xcodpart,xcodanal,xtipObr,xCodObra
PUBLIC vErdad,auxerr,vUsuario
PUBLIC tiP,mFila
PUBLIC wnummov,wfecemi,wmontos,wTipAux,wregistro,wDocRef,wdocadi,wnumchq,wCCosto,wCodDep,wtipdcr,wcodctc,kcuenta,;
       wmonto,wspacio,wdestino,vIndPar,FMES,MesCerrado
       
PUBLIC vtipfin,vtipfin95,vAli,vDHCuentas,VDESTINO,VCODARTI,Vindpar,vasignafecha
PUBLIC filaux2,idxaux2,filaux3,idxaux3,DtotalDoc,HtotalDoc,vistadoc
PUBLIC xcodctc, xTipAux, xRegistro,xTipDcr,xDocRef,fmes,ActMes, xPeriodoRef, xNummesRef, xNumChq

wcodaux	    = SPACE(02)   
vtipfin     = SPACE(8)
*----
xTipAux     = SPACE(2)
xDocAdi		= SPACE(2)
xPeriodoRef = SPACE(2)
xNummesRef  = SPACE(2)
xNumChq		= SPACE(14)
xRegistro = SPACE(6)
xtipdcr   = SPACE(2) 
xdocref   = SPACE(7)
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
IF blo_sis(1)
   DO STANDBY WITH "Sistema Bloquedo, Consulte al Area de Sistemas"
   CLOS DATA
   RETURN 
ENDIF   

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
 TITLE Vmens01 FOOTER PADC(" ®F4¯ Imprime ÍÍÍÍÍÍÍ  ®F11¯ Liquida ÍÍÍÍÍÍÍ  ®F9¯ Detalle ",78,'Í') COLOR SCHEME 10

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
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO LIS_NOT  &&liscomp 
 ON SELECTION PAD termi  OF mMenu DO termi
 ACTIVATE SCREEN
 
RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @  0,02 SAY "   Documento:"
 @  0,34 SAY "-"
 @  0,41 SAY "Fecha:"
 @ 01,02 SAY "       Glosa:" 
 @ 02,02 SAY "       Glosa:" 
 @ 03,02 SAY "       Glosa:" 
 @ 04,02 SAY "       Glosa:"    
 @ 05,02 SAY "Cod.Proyecto:"
 @ 05,37 SAY "T. Gasto:"
 @ 06,02 SAY "C. Corriente:"
 
******
*** No se tienen Obras
*** @ 07,02 SAY "        Obra:"
******

 @ 08,02 SAY "  Total Debe:"

 @ 08,41 SAY "Total Haber:"
RETURN

PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 ON KEY LABEL F9 DO Vista_Det
 ON KEY LABEL F11 DO Liqui_usu
 ON KEY LABEL F4 DO LIS_NOT
 SELECT Comp
 SCATTER MEMVAR
 xCodfte=SUBSTR(m.CodCal,5,3)
 wtipo=m.Tipdoc
 @  0,16 SAY m.TipDoc
 @  0,19 SAY m.Comprob
 @  0,35 SAY m.ComAdi
 @  0,48 SAY m.Fecha
 @  1,16 SAY SUBSTR(m.Glosa1,1,60)
 @  2,16 SAY SUBSTR(m.Glosa2,1,60)
 @  3,16 SAY SUBSTR(m.Glosa3,1,60)
 @  4,16 SAY SUBSTR(m.Glosa4,1,60) 
 @  5,16 SAY SPACE(20)
 @  5,16 SAY m.CodCal
 @  5,47 SAY m.tipfun
 @  5,52 SAY val_para(m.Tipfun,'TIPFUN','Z',52,15)
 @  6,16 SAY m.CodCtc
 
*** No se tienen obras en Beneficencia
*** @  7,16 SAY m.Tipobr
*** @  7,16 SAY m.Codobra
*** End.

IF ALLTRIM(M.TIPFUN)='I'  AND !EMPTY(M.CODOBRA)
***  =Val_Obra(m.CodObra,'C',21,40) 
ELSE
    wdestino=SPACE(55)   
ENDIF
*@  7,21 SAY wdestino
 @  8,16 SAY m.TotDeb  PICTURE "999,999,999,999.99"
 @  8,54 SAY m.TotHab  PICTURE "999,999,999,999.99"
 DO Vista_Hijo
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
   vDebe    = IIF(tipo="D",TRANSFORM(montos,"99,999,999.99"),SPACE(13)) :H=" Debe M.Nac.",;
   vHaber   = IIF(tipo="H",TRANSFORM(montos,"99,999,999.99"),SPACE(13)) :H="Haber M.Nac.",;
   vProvee  = rAuxil( diar.tipaux,diar.registro ) :H="Proveedor" :30,;
   registro :H='C¢d.Aux.',;
   tipfin   :H='Tipo Fin.',;
   tfin95   :H='TipFin 95'
   
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


PROCEDURE Corri              && Corrige el documento actual
*--------------
 SELECT Comp
 IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF
 
 IF m.estado = '20' 
       IF !yesno(vUsuario+', el Documento ya ha sido procesado. ¨ Est  Seguro de CORREGIR ?')
          UNLOCK ALL
          RETURN
       ENDIF
 ENDIF
 ON KEY LABEL F4
 IF !f_lock(1)
    RETURN
 ENDIF
 ACTIVATE WINDOW Wind_C0
 DO WHILE .T.
    tcambia=.f. 
    ON KEY LABEL F3 DO CorCom
    =v_tdoc()
   ***Limpiar Area
    @ 06,30 SAY REPLICATE(' ',47)
    @ 07,21 SAY REPLICATE(' ',54)
    @ 09,23 SAY REPLICATE(' ',54)

    
    @  0,38 GET m.Fecha       && VALID !m.fecha < m.actmes;
                              && ERROR "El a¤o/mes no es el correcto"
    @ 01,16 GET m.Glosa1      PICTURE "@S60"
    @ 02,16 GET m.Glosa2      PICTURE "@S60"
    @ 03,16 GET m.Glosa3      PICTURE "@S60"
    @ 04,16 GET m.Glosa4      PICTURE "@S60"
    @ 05,16 GET m.CodCal      
    @ 05,47 GET m.Tipfun      PICTURE "!"  VALID VAL_PARA(m.tipfun,'TIPFUN',' ',47,15)   
    READ
    
    IF YESNO("Tiene Cta.Cte")
	    @ 06,16 GET m.CodCtc  PICTURE "@!" VALID Val_Fun("Caja","CodCTc","CodCtc+' '+Descri",m.CodCTc,1,6,16)
	    READ
	ELSE
		m.codctc=SPACE(14)
		@ 06,16 SAY m.codctc
	ENDIF
   IF ALLTRIM(M.TIPFUN)='F'
      wdestino=SPACE(55)   
   ENDIF
  xTipObr =m.TipObr
  xCodObra=m.CodObra
  xTipfun =m.Tipfun
  xCodCtc = m.codctc
  
   ON KEY LABEL F3
   @  0,63 SAY "               "
   ON KEY
   
   IF LASTKEY()=27
      DO vista
      RETURN
   ENDIF
   * N/C normales
   m.estado="  "   
   IF yesno("Confirme datos")
      EXIT
   ENDIF
 ENDDO
 IF tcambia=.F.
 SS= Tra_Hijo(1)     && Corrige items
 lAppd = .F.
 IF SS .AND. yesno("Confirme Datos Ingresados")
    SELECT parma
    IF !SEEK('OPERATMAYOR')
       DO Standby WITH 'Par metro de Mayor no existe'
       RETURN
    ENDIF
    IF !f_lock(1)
       RETURN
    ENDIF

    SELECT Parma          && Mayorizaci¢n no efectuada
    SEEK 'OPERATMAYOR'
    REPLACE Descri WITH IIF(MONTH(m.fecha) = MONTH(actmes);
            .AND. YEAR(m.fecha) = YEAR(actmes),'No',parma.Descri)

    *- Graba items del documento.
    SELECT Diax
    
    GO TOP
    
    xComprob = Diax.Comprob
    xComAdi  = Diax.ComAdi
    xCodfte  = Diax.CodFte
    xCodCtc  = Diax.CodCtc
    SELECT Diar
    vOrd=ORDER()
    SET ORDER TO TAG Diario11
    SEEK Diax.Comprob + Diax.ComAdi + Diax.Codfte
    SCAN WHILE xComprob + xComadi + xcodfte = Diar.Comprob + Diar.ComAdi + Diax.Codfte
         IF !f_lock(1)
            UNLOCK ALL
            RETURN
         ENDIF
            SELECT Diar
            IF RLOCK()
               DELETE NEXT 1
            ENDIF   
         SELECT Diar
    ENDSCAN
    
* Se actualiza    
    SET ORDER TO TAG vOrd
    SELECT diax
    DO WHILE !EOF()
       SCATTER TO datos
       SELECT Diar
       IF f_appd()
          GATHER FROM datos
          lAppd = .T.
       ENDIF
       SELECT Diax
       SKIP
    ENDDO
    
    IF lAppd            
       =Verif_Ctas()    
    ENDIF               

    
    SELECT Comp
*    m.codpart = vcodpart
    GATHER MEMVAR
*   REPLACE comp.usuario WITH login()
    
*----------------    
 ELSE
    DO standby WITH "Error en digitaci¢n"
 ENDIF

 SELECT Diax
 ZAP
 *----------------
 SCATTER MEMVAR BLANK
 *-------------------
 ENDIF
 
 SELECT Comp
 UNLOCK ALL
 DO Vista
RETURN
   
    
FUNCTION Tra_Hijo
*----------------
PARAMETER cor_ing       && cor_ing = 1 correccion carga data antigua
PUBLIC valido
HIDE MENU mMenu
ACTIVATE SCREEN

 redonerr  = .F.
 xmov      = 0
 xfecemi   = CTOD('  /  /  ')
 _msg1     = " "
 _msg2     = " "
 
Tit = "²² F2:Edita  F5:Agrega Item  F7:TipFin  F8:Borra               F10:Sale ²²"

ON KEY LABEL F2  DO Edicion
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F7  DO TipFin
ON KEY LABEL F10 DO sal_hijo

IF cor_ing = 1    && Correccion, carga data antigua
   m.Item = 0
   SELECT Diar
   vOrd=ORDER()
   SET ORDER TO TAG Diario11
   SEEK m.Comprob + m.ComAdi + xCodfte
   SCAN WHILE Diar.Comprob + Diar.ComAdi + Diar.Codfte = m.Comprob + m.ComAdi + xCodfte
        SCATTER TO datos
        SELECT Diax
        APPEND BLANK
        GATHER FROM datos        
        m.Item   = Diax.Item
        wtipaux  = Diax.tipaux
        wregistro= Diax.registro              
        Sele Diar
   ENDSCAN
   
   SELECT Diax
   REPLACE ALL TipObr  WITH xTipObr
   REPLACE ALL CodObra WITH xCodObra
   REPLACE ALL Tipfun  WITH xTipfun
   REPLACE ALL CodCtc  WITH xCodCtc
   
   SELE Diar
   SET ORDER TO TAG vOrd
ENDIF
SELECT diax
BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_C2;
  TITLE Tit FIELDS;
  item     :H='Item'       :R,;  
  fecemi   :H='Emisi¢n'    :R,;
  cuenta   :H=' Cuenta'    :R,;
  tipo     :H='D/H'        :R,;
  montos   :H=' Importe S/. ':P='99,999,999.99':R,;
  vProvee  = rAuxil( diaX.tipaux,diaX.registro ) :H="Proveedor" :30,;
  diax.tipaux   :H='TipAux.'    :R,;
  diax.registro :H='C¢d.Aux. '  :R,;
  ccosto   :H='C.Cto':R,;
  Tipfun   :H='Tipfun' :R,;
  CodArt   :H='Art¡culo' :R,;
  Descri   :H='Descripci¢n' :R,;
  Codpart  :H='Partida' :R,;
  Codanal  :H='Analirica' :R,;
  TipFin   :H='TipFin' :R,;
  TFin95   :H='TipFin 95':R
  
  ON KEY LABEL F10
  ON KEY LABEL F5
  ON KEY LABEL F8
  ON KEY LABEL F2

IF LASTKEY()=27
   DO standby WITH " Cancela Ingreso "
   RETURN .F.
ENDIF
SELECT comp
ACTIVATE SCREEN
vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO Logos WITH Rotulo1,vtempo
SHOW MENU mMenu
RETURN valido





PROCEDURE Agreg_item
*-------------------
 ON KEY LABEL F4
 SELECT Diax
 APPEND BLANK
 m.Item = m.Item + 1
 
 *-----Para Constructivo
 REPLACE EstCon   WITH "55"
 *----------------------
 
 REPLACE CodCtc   WITH m.CodCtc
 REPLACE Comprob  WITH m.Comprob
 REPLACE ComAdi   WITH m.ComAdi
 REPLACE Item     WITH m.Item
 REPLACE Tipo     WITH 'D'
 REPLACE CodCtc   WITH m.CodCtc
 REPLACE CodFte   WITH SUBSTR(m.CodCal,5,3)
 REPLACE Tipfun   WITH xTipfun 
 REPLACE TipObr   WITH xTipObr
 REPLACE CodObra  WITH xCodObra
 
 REPLACE Fecemi     WITH  m.fecha
 REPLACE CodDep     WITH '120000'    
 REPLACE TipAux     WITH xTipAux
 REPLACE PeriodoRef WITH xPeriodoRef
 REPLACE NumMesRef  WITH xNumMesRef
 REPLACE NumChq     WITH xNumChq
 REPLACE Registro   WITH xRegistro
 REPLACE TipDcr     WITH xTipDcr
 REPLACE DocRef     WITH xDocRef
 REPLACE DocAdi     WITH xDocAdi

 DO Edicion
RETURN


PROCEDURE Elimi_item
*-------------------
* Elimina el registro
* En caso que el registro tenga una provisi¢n verifica que ‚sta
* no tenga cancelaciones (sea en el mismo o en otro documento).
* Si se borra el registro debe recalcular el correlativo de registro
* para el documento.

IF yesno("Confirme ELIMINACION de Item")

  * Recalcula totales del documento
   SELECT Diax
   IF diax.tipo='D'
      m.TotDeb = m.TotDeb - Diax.Montos
      REPLACE Comp.TotDeb WITH Comp.TotDeb - Diax.Montos
   ELSE
      m.TotHab = m.TotHab - Diax.Montos
      REPLACE Comp.TotHab WITH Comp.TotHab - Diax.Montos      
   ENDIF

  * Borra el registro y recalcula el correlativo.
   SELECT Diax
   m.Item = Diax.Item - 1
   DELETE
   SKIP
   vreg = IIF(!EOF(),RECNO(),0)
   DO WHILE !EOF()
      m.Item = m.Item + 1
      REPLACE Item WITH m.Item
      SKIP
   ENDDO
   IF vreg # 0
      GO vreg
   ELSE
      GO BOTTOM
   ENDIF
ENDIF
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ON KEY LABEL F4
 ACTIVATE WINDOW Wind_C0
 vreg = RECNO()  
 SELECT Comp
 SCATTER MEMVAR BLANK
 m.fecha = ActMes
 m.estcon= "55"
 DO WHILE .T.
    _msg2  = ""
    @ 08,16 SAY REPLICATE(' ',20)
    @ 08,54 SAY REPLICATE(' ',20)

    @  0,16 SAY REPLICATE(" ",22)
    @  0,16 GET m.TipDoc      VALID v_Tdoc() .AND. Asigna()
    @  0,19 GET m.Comprob     VALID v_comp();
                              MESSAGE 'El N£mero del Comprobante  debe Termiar en el N£mero de Mes a Procesar' ;
                              ERROR _msg2
                                                            
    @  0,35 GET m.ComAdi      VALID No_Dupli(m.comprob,m.comadi) ERROR _Msg2 && vericom()
    @  0,38 GET m.Fecha       VALID vfecAct() .AND. v_comp1() .AND. (!m.fecha < m.actmes) ERROR _msg2
    @ 01,16 GET m.Glosa1      PICTURE "@S60"
    @ 02,16 GET m.Glosa2      PICTURE "@S60"
    @ 03,16 GET m.Glosa3      PICTURE "@S60"
    @ 04,16 GET m.Glosa4      PICTURE "@S60"        
    @ 05,16 GET m.CodCal      VALID !EMPTY(m.CodCal)
    @ 05,47 GET m.Tipfun      PICTURE "!"  VALID VAL_PARA(m.tipfun,'TIPFUN',' ',47,15)   
    @ 06,16 GET m.CodCtc      PICTURE "@!" VALID Val_Fun("Caja","CodCTc","CodCtc+' '+Descri",m.CodCTc,1,6,16) WHEN YESNO("Tiene Cta.Cte")
    
    @ 00,37 SAY REPLICATE(" ",43)
    @ 06,22 SAY REPLICATE(' ',54)
    @ 07,22 SAY REPLICATE(' ',54)
    @ 08,16 SAY REPLICATE(' ',10)
    @ 08,54 SAY REPLICATE(' ',10)
    @ 09,22 SAY REPLICATE(' ',54)
    READ

    IF ALLTRIM(m.tipfun)='F'
       wdestino=SPACE(55)   
    ENDIF
    
    xTipObr =m.TipObr
    xCodObra=m.CodObra
    xTipfun =m.Tipfun
*    vcodpart=m.codpart
    
    IF LASTKEY()=27
       DO STANDBY WITH "Proceso Cancelado. No se graba el Documento"
       GOTO vreg
       DO Vista
       RETURN
    ENDIF
    * Notas/Contabilidad Normales  
    m.estado="  "   
    IF yesno("Confirme datos del documento")
       EXIT
    ENDIF
 ENDDO
 
 SELECT diax
 m.Item = 0
 redonerr  = .F.
 xmov      = 0
 xfecemi   = CTOD('  /  /  ')
 _msg1     = " "
 _msg2     = " "
 xTipAux   = SPACE(2)
 xRegistro = SPACE(6)
 xEstado   = m.Estado
* xcodpart  = vcodpart
 
 DO agreg_item
 SS= Tra_Hijo(2)     && 2 indica que es documento nuevo.
 lAppd = .F. 

 IF SS .AND. yesno("¨Graba la Informaci¢n?")
    SELECT Parma
    IF !SEEK('OPERATMAYOR')
       DO Standby WITH 'No existe Par metro de Mayor'
       RETURN
    ENDIF
    IF !f_lock(1)
       RETURN
    ENDIF
    SELECT Comp
    IF f_appd()
       SELECT Parma
       SEEK 'OPERATMAYOR' &&=Si &&AJUSTE POR DIFERENCIA
       REPLACE Descri WITH IIF(MONTH(m.fecha) = MONTH(actmes);
               .AND. YEAR(m.fecha) = YEAR(actmes),'No',parma.Descri)
       SELECT Comp
       IF m.tipdoc>'20'
          =relee()
       ENDIF   
*       m.codpart=vcodpart
       GATHER MEMVAR
       REPLACE FecOpe  WITH DATE()
*      REPLACE Usuario WITH LOGIN()
       
       SELECT Diax
       GO TOP
       
       IF m.tipdoc>'20'
	       SCAN
    	     REPLACE Diax.comprob WITH m.comprob
	         SELECT Diax
    	   ENDSCAN
	       GO TOP
       ENDIF
       SCAN
          SCATTER TO datos
          SELECT Diar
          IF f_appd()
             GATHER FROM datos
*             REPLACE diar.codpart WITH vcodpart
             lAppd=.T. 
          ENDIF
          SELECT Diax
       ENDSCAN
    ENDIF
    
    SELECT Diar
    SEEK Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.CodCal,5,3)
    *- ACTUALIZA SECUENCIA
    IF m.tipdoc>'20'
       SELECT Parma
       SEEK 'CORREL' + 'NOTCON'
       REPLACE NumEnt WITH NumEnt + 1
     ENDIF  
    
    IF lAppd            
       =Verif_Ctas()    
    ENDIF               
 ELSE
    DO standby WITH "Error en digitaci¢n"
 ENDIF

 SELECT Diax
 ZAP
 *------------
 SCATTER MEMVAR BLANK
 
 *-----------
 SELECT Diar
 UNLOCK ALL
 
 SELE Diar
 SET ORDER TO Diario11
 
 SELE COMP
 SET ORDER TO Compro7
 
 DO vista
RETURN

       
PROCEDURE Anula
*--------------
* Anula el documento actual.
* Solo se puede anular si no tiene provisiones que se cancelen
* en otros documento o si el documento no se ha cerrado.

IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF

IF m.fecha < m.actmes
   DO standby WITH "Documento no es del mes actual"
   RETURN
ENDIF

IF m.estado = '20'
     DO standby WITH vMens20
     UNLOCK ALL
     RETURN
ENDIF

IF yesno("¨ANULA Documento?")
   vErdad=.T.
   vOrd=ORDER()
   SELECT Comp
   IF f_lock(1)
      REPLACE Estado WITH '99'
      DELETE
      *-Busca y elimina los Items
      SELECT Diar
      SET ORDER TO TAG Diario11
      SEEK Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.CodCal,5,3)
      IF FOUND()
         SCAN WHILE Comp.Comprob+Comp.ComAdi+SUBSTR(Comp.CodCal,5,3)+Comp.CodCtc= Diar.Comprob+Diar.ComAdi+Diar.CodFte
              IF vErdad
                 =Rebaja()
              ENDIF
              IF f_lock(1)
                 REPLACE Estado WITH '99'
                 DELETE
              ENDIF
         ENDSCAN
      ENDIF
      SET ORDER TO TAG Diario1
      SELECT Comp
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


FUNCTION Edicion
*---------------
ACTIVATE WINDOW Wind_C3
PRIVATE vTipDcr,vCodDep,vTipDoc
*- Al presionar nuevamente F2, F5, F8, F10 no ejecuta.
ON KEY LABEL F2  DO nada
ON KEY LABEL F3
ON KEY LABEL F5  DO nada
ON KEY LABEL F7  DO nada
ON KEY LABEL F8  DO nada
ON KEY LABEL F9  DO nada
ON KEY LABEL F10 DO nada
vtipdcr=TipDcr
vcoddep=CodDep
vtipDoc=TipDoc
xTipfun =Tipfun
XCodart =Codart
xDescri =Descri
xCodpart=Codpart
xCodanal=Codanal
xtipObr =TipObr
xCodObra=CodObra

**Guarda Datos 
xTipAux=TipAux
xregistro=Registro
xPeriodoRef=PeriodoRef
xNumMesRef=NumMesRef
xNumChq=NumChq
xTipDcr=TipDcr
xDocREf=DocRef
xDocAdi=DocAdi

REPLACE NumMov   WITH 1

@  0,1  SAY '    Item:' + STR(Item,4)
@  1,1  SAY ' Fec.Emi:' GET FecEmi    WHEN .F.
tip=.F.
@  2,1  SAY '  Cuenta:' GET Cuenta    VALID   v_cta() ERROR   _msg1 PICTURE '!!!!!!!!!!'
@  3,1  SAY '     D/H:' GET Tipo      PICTURE '@M D,H' 
@  4,1  SAY ' Imp.S/.:' GET Montos    PICTURE '999,999,999,999.99'
@  5,1  SAY 'Tip.Aux.:' GET TipAux    VALID   v_aux1()  ERROR   "Dato errado!!!"
@  6,1  SAY 'C¢d.Aux.:' GET Registro  VALID   V_AUX()
@  8,1  SAY '  DocRef:'
@  8,21 SAY '-'
@  8,11 GET vTipdcr  PICTURE "!!" VALID vv_Tdoc()  
@  8,14 GET DocRef    PICTURE "!!!!!!!"
@  8,22 GET DocAdi    VALID Valida() PICTURE "!!"
@  9,1  SAY 'Mes-Per. DocRef: ' GET  Nummesref    PICTURE "!!"
@  9,21 SAY '-'
@  9,22 GET  Periodoref   PICTURE "!!"
@ 10,1  SAY '  CtaCte:' GET CodCtc   WHEN .F.
@ 11,1  SAY 'N.Cheque:' GET NumChq    PICTURE "!!!!!!!!!!!!!!"
@ 12,1  SAY 'Cod.Dep.:'
@ 12,11 GET vCodDep PICTURE '!!!!!!' VALID Val_parA(VCodDep,'CODDEP','W',11,30,7)
READ VALID VAL_READ()  
tip=.F.
SELE EstFin
IF SEEK(LEFT(Diax.Cuenta,3))
	tip=.T.
ENDIF

IF tip
    =Aut_Fin(SUBSTR(Comp.Comprob,1,2),Diax.Cuenta)    
    IF !EMPTY(vTipfin)
    	REPLACE TipFin WITH SUBSTR(vTipfin,1,4)
	ENDIF	
    SELECT diax
	@ 16,1   SAY 'Tip Fin.:' GET TipFin  VALID Val_Tipo(TipFin,'TIPFIN','TipFin')
	READ 
ENDIF

SELECT Diax
IF LASTKEY() = 27
   DO standby WITH " Cancela Ingreso "
ELSE
   Tipfun       = xTipfun
   Codart       = xCodart
   Descri       = xDescri
   Codpart      = xcodpart
   Codanal      = xcodanal
   tipObr       = xTipObr
   CodObra      = xCodObra   
   m.numchq     = Diax.NUMCHQ
   m.docref     = Diax.Docref
   m.tipdcr     = Diax.Tipdcr
   m.docadi     = Diax.Docadi
   m.nummesref  = Diax.nummesref
   m.periodoref = Diax.Periodoref
   m.Fecemi     = Diax.fecemi
   m.montos     = Diax.montos
   m.nummov     = Diax.nummov
   **
   m.coddep		= Diax.CodDep
   ******
   GATHER MEMVAR FIELDS EXCEPT ITEM,TIPDOC
   ******
ENDIF
xTipAux=TipAux
xregistro=Registro
xPeriodoRef=PeriodoRef
xNumMesRef=NumMesRef
xNumChq=NumChq
xTipDcr=TipDcr
xDocREf=DocRef
xDocAdi=DocAdi


DO regreg

DEACTIVATE WINDOW WIND_C3

ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F7  DO TipFin
ON KEY LABEL F10 DO sal_hijo
RETURN .F.


*- Funci¢n para seleccionar Tipo Financiero
FUNCTION TipFin
*------------------
ON KEY LABEL F2  DO nada
ON KEY LABEL F3
ON KEY LABEL F5  DO nada
ON KEY LABEL F7  DO nada
ON KEY LABEL F8  DO nada
*ON KEY LABEL F9  DO nada
ON KEY LABEL F10 DO nada

tip=.F.
SELE EstFin
IF SEEK(LEFT(Diax.Cuenta,3))
	tip=.T.
ENDIF
SELECT Diax

IF tip
	DEFINE WINDOW TFin FROM 10,15 TO 13,60 FOOTER '[ESC]:Cancela  [F10]:Confirma'
	ACTIVATE WINDOW TFin
	tf=Diax.TipFin
	* ON KEY LABEL F10 CLEAR READ 
	@0,7 SAY 'Tipo Financiero : ' GET TipFin VALID Val_Tipo(TipFin,'TIPFIN','TipFin')   
	READ  
	IF LASTKEY()=27 
		REPLACE TipFin WITH tf
	ENDIF
	DEACTIVATE WINDOW TFin
ELSE
	WAIT WINDOW 'Cuenta no se encuentra en la Estructura Financiera' NOWAIT
ENDIF
* ON KEY LABEL F10
ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F7  DO TipFin
*ON KEY LABEL F9  DO TFin95
ON KEY LABEL F10 DO sal_hijo
IF LASTKEY() = 27
   DO standby WITH " Cancela Ingreso "
ENDIF
RETURN .F.


FUNCTION xxTipFin
*------------------
ON KEY LABEL F2  DO nada
ON KEY LABEL F3
ON KEY LABEL F5  DO nada
ON KEY LABEL F7  DO nada
ON KEY LABEL F8  DO nada
ON KEY LABEL F9  DO nada
ON KEY LABEL F10 DO nada

tip=.F.
SELE EstFin
IF SEEK(LEFT(Diax.Cuenta,3))
	tip=.T.
ENDIF
SELECT Diax

IF tip=.T.
	DEFINE WINDOW TFin FROM 10,15 TO 13,60 FOOTER '[ESC]:Cancela  [F10]:Confirma'
	ACTIVATE WINDOW TFin
	tf=Diax.TipFin
	* ON KEY LABEL F10 CLEAR READ 
	@0,7 SAY 'Tipo Financiero : ' GET TipFin VALID Val_Tipo(TipFin,'TIPFIN','TipFin')   
	READ  
	IF LASTKEY()=27 
		REPLACE TipFin WITH tf
	ENDIF
	DEACTIVATE WINDOW TFin
ELSE
	WAIT WINDOW 'Cuenta no se encuentra en la Estructura Financiera' NOWAIT
ENDIF
* ON KEY LABEL F10
ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F7  DO TipFin
* ON KEY LABEL F9  DO TFin95
ON KEY LABEL F10 DO sal_hijo
IF LASTKEY() = 27
   DO standby WITH " Cancela Ingreso "
ENDIF
RETURN .F.

*- Funci¢n para seleccionar Tipo Financiero 95
FUNCTION TFin95
*------------------
ON KEY LABEL F2  DO nada
ON KEY LABEL F3
ON KEY LABEL F5  DO nada
ON KEY LABEL F7  DO nada
ON KEY LABEL F8  DO nada
ON KEY LABEL F9  DO nada
ON KEY LABEL F10 DO nada

tip=.F.
SELE EstFin
IF SEEK(LEFT(Diax.Cuenta,3))
	tip=.T.
ENDIF
SELECT Diax

IF tip=.T.
	DEFINE WINDOW TFin FROM 10,15 TO 13,60 FOOTER '[ESC]:Cancela  [F10]:Confirma'
	ACTIVATE WINDOW TFin
	tf=Diax.TFin95
	* ON KEY LABEL F10 CLEAR READ
	@0,7 SAY 'Tipo Financiero 95 : ' GET TFin95 VALID Val_Tipo(TFin95,'TFIN95','TFin95')
	READ 
	IF LASTKEY()=27 
		REPLACE TFin95 WITH tf
	ENDIF
	DEACTIVATE WINDOW TFin
ELSE
	WAIT WINDOW 'Cuenta no se encuentra en la Estructura Financiera' NOWAIT
ENDIF
* ON KEY LABEL F10
ON KEY LABEL F2  DO Edicion
ON KEY LABEL F5  DO agreg_item
ON KEY LABEL F8  DO elimi_item
ON KEY LABEL F7  DO TipFin
* ON KEY LABEL F9  DO TFin95
ON KEY LABEL F10 DO sal_hijo
IF LASTKEY() = 27
   DO standby WITH " Cancela Ingreso "
ENDIF
RETURN .F.


FUNCTION Asigna
*--------------
vlen      = LEN(m.Comprob)
xcomprob=SUBSTR(m.Comprob,3,14)
SELECT Parma
SEEK 'CORREL'+'NOTCON'
xcomprob=PADL(ALLTRIM(STR(Parma.NumEnt+1,5)),5,'0')
m.Comprob = m.tipdoc + xcomprob+fmes
SELECT  Comp 
*WAIT WINDOW 'El N£mero del Comprobante  debe Terminar en el N£mero de Mes a Procesar' 
RETURN .T.


FUNCTION relee
*-------------
vlen      = LEN(m.Comprob)
xfinmes   =SPACE(02)
xcomprob=SUBSTR(m.Comprob,3,14)
xfinmes =RIGHT(ALLTRIM(m.Comprob),2)
SELECT Parma
SEEK 'CORREL'+'NOTCON'
xcomprob=PADL(ALLTRIM(STR(Parma.NumEnt+1,5)),5,'0')
m.Comprob = m.tipdoc + xcomprob + xfinmes
SELECT  Comp 
RETURN .T.

FUNCTION Asigna1
*---------------
 IF m.TipDoc # vTipDoc
    vlen      = LEN(m.Comprob)
    xcomprob=SUBSTR(m.Comprob,3,14)
    m.Comprob = alltrim(m.tipdoc)+ xcomprob
 ENDIF
 RETURN .T.


FUNCTION Asigna2
*---------------
 vlen        = LEN(m.DocRef)
 m.DocRef = m.tipdcr + SPACE(vlen - LEN(m.tipdcr))
 RETURN .T.

FUNCTION v_comp
*--------------
 IF LEFT(m.comprob,2)<>ALLTRIM(m.tipdoc)
    _MSG2 = "Digitos iniciales INCORRECTOS"
    RETURN .f.
 ENDIF

 IF LEN(ALLTRIM(m.comprob)) <= 8
    _MSG2 = "N£mero de Documento errado"
    RETURN .f.
 ENDIF

  _rec = IIF(EOF(),-1,RECNO())

 IF _rec>0
    GO _rec
 ELSE
    GO BOTTOM
 ENDIF

RETURN

FUNCTION vericom
*---------------
SELE Comp
SET ORDER TO TAG Compro7
SEEK  m.Comprob+m.Comadi+'PRP'
IF FOUND()
   DO STANDBY WITH 'Documento ya esta registrado en PRP,Verifique su Secuencia'
   RETURN .F.
ELSE
   SEEK  m.Comprob+m.Comadi+'TRN'
   IF FOUND()
      DO STANDBY WITH 'Documento ya esta registrado en TRN,Verifique su Secuencia'
      RETURN .F.
   ENDIF   
ENDIF   
RETURN .T.




FUNCTION v_comp1
*---------------
PRIVATE __fun,__fun1
SET ORDER TO TAG Compro7

  _rec = IIF(EOF(),-1,RECNO())

  __fun = !SEEK(m.comprob+m.comadi+ALLTRIM(STR(MONTH(m.fecha)))+ALLTRIM(m.CodCal))
 
 _msg2 = "Documento YA EXISTE"

 __fun1= !m.fecha < m.actmes
 IF !__fun1
    _msg2 = "El a¤o/mes no es el correcto"
 ENDIF
 
 If RIGHT(ALLTRIM(m.comprob),2) <> PADL(ALLTRIM(STR(MONTH(m.fecha),2)),2,'0')
    _msg2 = "El mes de la fecha no coincide con la numeraci¢n final del comprobante"
    RETURN .F.
 ENDIF
 
  
 IF _rec>0
    GO _rec
 ELSE
    GO BOTTOM
 ENDIF
 
SET ORDER TO
m.codcal=SUBSTR(STR(YEAR(m.fecha),4),3,2)+PADL(ALLTRIM(STR(MONTH(m.fecha),2)),2,'0')+'PRP'+SPACE(12)
RETURN __fun


PROCEDURE CorCom
*---------------
* Permite corregir el n£mero del documento.

ON KEY LABEL F3
_msg2    = ""
vTipDoc  = m.TipDoc
vComprob = m.Comprob
vComAdi  = m.ComAdi
vCodCal = m.CodCal 
vReg     = RECNO()

 DEFINE WINDOW WCambia FROM 10,12 TO 15,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WCambia
@  0,2 SAY 'Tipo de Documento :' GET m.TipDoc      VALID Val_para7(m.Tipdoc,'TIPDOC',' ',22,15);
                                .AND. Asigna1()
@  1,2 SAY '        Documento :' GET m.Comprob     VALID v_comp();
                          ERROR _msg2
@  2,2 SAY '        Adicional :' GET m.ComAdi
@  3,2 SAY ' Codigo  Proyecto :' GET m.CodCal

READ
RELEASE WINDOW WCambia

IF LASTKEY() # 27
   SELECT Comp
   GO vReg
   REPLACE TipDoc   WITH m.TipDoc
   REPLACE Comprob  WITH m.Comprob
   REPLACE ComAdi   WITH m.ComAdi
   REPLACE Codcal  WITH m.CodCal
   SELECT Diar
   DO WHILE .T.
      IF SEEK(vComprob+vComAdi+SUBSTR(vCodCal,5,3))
         REPLACE Diar.Comprob WITH m.Comprob
         REPLACE Diar.ComAdi  WITH m.ComAdi
         REPLACE Diar.Codfte  WITH SUBSTR(m.CodCal,5,3)
      ELSE
         EXIT
      ENDIF
   ENDDO
   tcambia=.T. 
   SELECT Diar
   SEEK m.Comprob + m.ComAdi + SUBSTR(m.CodCal,5,3)
   SELECT Comp
ELSE
   * m.TipDoc  = vTipDoc
   * m.Comprob = vComprob
   * m.ComAdi  = vComAdi
   * m.CodCal = vcodpry
   * @  0,16 SAY m.TipDoc
   * @  1,16 SAY m.Comprob 
   * @  1,27 SAY m.ComAdi
   SELECT Comp
   GO vReg
ENDIF
RETURN


*- 2. Valida Fecha del item
FUNCTION Val_Fec
*---------------
IF MONTH(fecemi) = MONTH(m.fecha) .AND. YEAR(fecemi) = YEAR(m.fecha)
   RETURN .T.
ENDIF
IF PADL(YEAR(fecemi),4,'0') + PADL(MONTH(fecemi),2,'0') < ;
   PADL(YEAR(m.Fecha),4,'0') + PADL(MONTH(m.Fecha),2,'0')
   DO STANDBY WITH "Mes distinto al del DOCUMENTO."
   *IF YesNo("Mes distinto al del DOCUMENTO. ¨Continua?")
      RETURN .F.
   *ENDIF
ENDIF
RETURN .F.


*- 4 Tipo de auxiliar
FUNCTION v_aux1
*--------------
vAlias  = ALIAS()
vreturn = .F.
IF EMPTY(diax.Tipaux)
   vreturn = Val_para(Diax.Tipaux,'AUXIL ','Tipaux',' ',6,18)
ENDIF   

SELECT Parma
SEEK 'AUXIL ' + Diax.TipAux
IF FOUND()
   REPLACE Diax.Tipaux WITH Parma.Codigo
   @ 5,15 SAY SUBSTR(Parma.Descri,1,18)
   vreturn=.t.
ELSE 
   vreturn=.f.
ENDIF   

SELECT (vAlias)
RETURN (vreturn)



*- 5 Codigo auxiliar

FUNCTION v_aux
*---------
resx    = .F.
wtipaux=diax.tipaux
wregistro=diax.registro      
=VAL_AUX(wregistro,wtipaux,'  ',11,30) 
IF !EMPTY(WREGISTRO)
    IF LASTKEY()<>27
       REPLACE  diax.registro WITH wregistro
	   wtipaux=diax.tipaux
	   wregistro=diax.registro      
	ENDIF   
     resx = .T.  
ELSE   
    resx = .F.  
ENDIF
RETURN (resx)




FUNCTION Xv_aux
*-------------
 _ALI_   = ALIAS()
 resx    = .F.
 vReturn = .F.

 IF EMPTY(Diax.registro)
       xaux = SPACE(30)  && Temporalmente guarda la descripci¢n
       SELECT Auxi
       vidx = SYS(3) + '.idx'
       INDEX ON Descri TO &vidx FOR Diax.Tipaux = Tipo
       resx = val_fun('Auxi','Descri','descri',xaux,1)
       SELECT Auxi
       SEEK xAux
       xaux = Auxi.Codigo
       SET INDEX TO
       SET ORDER TO TAG AUXIL1
       ERASE (vidx)
       SELECT Diax
       IF resx
          REPLACE Diax.registro WITH xaux
       ENDIF
       SELECT Auxi
       IF !SEEK(Diax.tipaux + Diax.registro)
          resx = .F.
       ENDIF       
ELSE
 SELECT auxi
 SET ORDER TO TAG AUXIL1
 SEEK diax.tipaux + SUBSTR(diax.registro,1,6)
  IF FOUND()
     @ 7,18 SAY SUBSTR(auxi.descri,1,30)
     wtipaux=diax.tipaux
     wregistro=diax.registro      
     resx = .T.  
  ELSE   
    resx = .F.  
  ENDIF
ENDIF

SELECT auxi
 SET ORDER TO TAG AUXIL1
SEEK diax.tipaux + SUBSTR(diax.registro,1,6)
IF FOUND()
   @ 7,18 SAY SUBSTR(auxi.descri,1,30)
     wtipaux=diax.tipaux
     wregistro=diax.registro      
     resx = .T.  
 ELSE
     resx = .F.  
ENDIF  
SELECT (_ali_)
RETURN resx

PROCEDURE regreg
*------------------
IF yesno("Desea Actualizar Todos los Items con el mismo Auxiliar ?")  
    * Se Actualiza el auxiliar    
    SELE Diax
    REPLACE ALL diax.tipaux   WITH wtipaux
    REPLACE ALL diax.registro WITH wregistro
*    REPLACE ALL diax.docref   WITH wdocref
*    REPLACE ALL diax.docadi   WITH wdocadi
*    REPLACE ALL diax.nummesref  WITH wnummesref
*    REPLACE ALL diax.Periodoref WITH wperiodoref
  ENDIF  
RETURN


*- 7. La cuenta
FUNCTION v_cta
*-------------
* Muestra el tipo de auxiliar posible.
* Verifica que la cuenta sea de detalle y no sea autom tica.
* Si la cuenta no existe permite agregarla en el plan de cuentas.
*------------------
* VERIFICAMOS EL DETALLE CON EL TIPO DE DOCUMENTO
   IF  !EMPTY(mVerifd) AND SUBSTR(Cuenta,4,4)=mverifd
    	     _f_=.T.
	ELSE
	 IF EMPTY(mVerifd)
	    _f_=.T.
	 ELSE   
         _msg1 = " Detalle de la Cuenta no corresponde al Documento --> "+mverifd
         SELECT Diax
         RETURN .F.
      ENDIF   
   ENDIF 
*----------------------------------------------
 SELECT Cuen
 _f_ = SEEK(Diax.Cuenta) .AND. !EMPTY(Diax.Cuenta)
 SELECT Diax
  @ 3,22 SAY SUBSTR(Cuen.Descri,1,35)
  
 IF _f_ .OR. EMPTY(Diax.Cuenta)
    IF EMPTY(Diax.Cuenta)
       vcuenta = SPACE(LEN(Diax.Cuenta))
       SELECT Cuen
       SET ORDER TO TAG Cuentas4
       _f_ = Val_Fun1('Cuen','Cuenta',"TRIM(Cuenta) +' '+ Descri",VCuenta,1,3,22,'SUBSTR(Descri,1,35)','Cuentas4','Cuentas5')
       SET ORDER TO TAG Cuentas1
       SELECT Diax
       IF !_f_
          _msg1 = "Indique el n£mero de cuenta!!!"
          RETURN .F.
       ELSE
          REPLACE Diax.Cuenta  WITH vCuenta
          @ 2, 11 SAY Diax.Cuenta
       ENDIF
    ENDIF
   
    SELECT Cuen
    IF UPPER(Cuen.Detalle)  # 'S'
       _msg1 = "Cuenta no es de Detalle"
       SELECT Diax
       RETURN .F.
    ENDIF
    
    
   SELE EstFin
   IF SEEK(LEFT(Diax.Cuenta,3))
   *IF SEEK(LEFT(Diax.Cuenta,3)+SUBSTR(Diax.Cuenta,9,2))
   		tip=.T.
   ENDIF
   SELECT Diax
   
 ENDIF
 
RETURN _f_


* 8 . Numero de Movimiento

FUNCTION v_mov
*-------------
* Guarda todos los valores que un mismo movimiento debe tener.
* Tambi‚n, guarda los anteriores valores de los documentos antes
* de ser modificados.

 xTipAux    = TipAux
 xRegistro  = Registro
 xmov       = nummov
 xfecemi    = fecemi
 xcuenta    = cuenta
 xCodCtc    = CodCtc
 IF EMPTY(xfecemi)
    vreg = RECNO()
    SELECT Diax
    GO TOP
    SCAN
       IF xmov = nummov .AND. vreg # RECNO()
          xfecemi    = fecemi
          EXIT
       ENDIF
    ENDSCAN
    GO vreg
    REPLACE fecemi WITH xfecemi
    @  2,11 SAY  fecemi
    @  3,11 SAY  cuenta     PICTURE '9999999999'
 ENDIF
RETURN .T.


 
*  9. Desactiva las teclas F2, F5, F8, F10
PROCEDURE Nada
*-------------
RETURN


* 10. Agrega una nueva cuenta.
PROCEDURE ap_cta
*---------------
PARAMETER xxcta
*- apertura de ctas. en linea
 DEFINE WINDOW _ctas FROM 3,5 TO 20,75 TITLE " APERTURA DE CUENTAS " COLOR SCHEME 10
 ACTIVATE WINDOW _ctas
 CLEAR

 PRIVATE M.CUENTA, M.DESCRI,  M.NIVEL,   M.DETALLE
 PRIVATE M.TIPO,   M.DCONTRA, M.HCONTRA
 PRIVATE M.DBHB,   xxalias

 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW _ctas

 SELECT Cuen
 SCATTER MEMVAR BLANK
 m.Cuenta = xxcta
 xxalias = ALIAS()

 vreg = IIF(!EOF(),RECNO(),0)
 xDetalle = IIF(m.Detalle='S','Si','No')
 XTipo    = IIF(m.Tipo   ='A','Activo   ','Pasivo   ')
 XDbHb    = IIF(m.DbHb   ='D','Deudora  ','Acreedora')
 m.Nivel  = LEN(ALLTRIM(m.Cuenta))

 @  3,2 SAY "            Cuenta:" GET m.Cuenta   WHEN (.F.) PICTURE '9999999999'
 @  4,2 SAY "       Descripci¢n:" GET m.Descri   PICTURE "@!"
 @  5,2 SAY "             NIVEL:" GET m.Nivel    WHEN (.F.) PICTURE '99'
 @  6,2 SAY "           Detalle:" GET xDetalle   PICTURE "@M Si,No"
 @  7,2 SAY "              Tipo:" GET xTipo      PICTURE "@M Activo   ,Pasivo   ,Resultado"
 @  8,2 SAY "Contra cuenta Debe:" GET m.DContra  PICTURE '9999999999' VALID EMPTY(m.DContra) .OR. Val_Cuen(m.DContra,.T.,13,23)
 @  9,2 SAY "Contr.cuenta Haber:" GET m.HContra  PICTURE '9999999999' VALID EMPTY(m.HContra) .OR. Val_Cuen(m.HContra,.T.,15,23)
 @ 10,2 SAY "        Debe/Haber:" GET XDbHb      PICTURE '@M Deudora  ,Acreedora'
 READ
 m.Detalle = IIF(xDetalle $ 'Si','S','N')
 m.Tipo    = IIF(xTipo    $ 'Activo   ','A',IIF(xTipo $ 'Pasivo   ','P','R'))
 m.DbHb    = IIF(xDbHb    $ 'Deudora  ','D','H')
 IF vreg # 0 .AND. vreg <= RECCOUNT()
    GO vreg
 ENDIF

 IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
    IF f_appd()
       GATHER MEMVAR
    ENDIF
    UNLOCK
 ELSE
    RELEASE WINDOW _ctas
    SELECT diax
    RETURN .F.
 ENDIF

 RELEASE WINDOW _ctas
 SELECT diax

RETURN .T.


* 11. Centro de Costo
FUNCTION v_cto
*-------------
_F = .T.
IF !EMPTY(ccosto)
   xcto   = Diax.ccosto
   vAlias = ALIAS()
   SELECT Ctoc
   IF !SEEK(xcto)
      xcto = SPACE(LEN(Diax.ccosto))
      _F   = Val_Fun('Ctoc','ctocto','descri',xcto,1)
      IF LASTKEY()=27 .OR. !_F
         IF yesno("Centro de Costo no existe. ¨Crea uno?")
            _f = ap_cto(2)
            IF _f
               @ 10, 17 SAY SUBSTR(Ctoc.Descri,1,19)
            ENDIF
         ELSE
            SELECT Diax
            REPLACE CCosto WITH SPACE(LEN(Diax.CCosto))
            @ 10,17 SAY SPACE(19)
            RETURN .T.
         ENDIF
      ELSE
        SELECT Diax
        REPLACE CCosto   WITH xcto
        @ 10, 17 SAY SUBSTR(Ctoc.Descri,1,19)
      ENDIF
   ENDIF
   SELECT (vAlias)
ELSE
   IF SUBSTR(Diax.Cuenta,1,1) = '9' .AND.;
      !YesNo("Falta indicar Centro de Costo. ¨Continua?")
      _f = .F.
   ENDIF
ENDIF

RETURN _f


* 12. Crea nuevo centro de costo.
FUNCTION ap_cto
*--------------
PARAMETER _tt

 DEFINE WINDOW _ctos FROM 3,5 TO 23,75 TITLE " APERTURA DE CTOS. DE CTOS. " COLOR SCHEME 10
 ACTIVATE WINDOW _ctos
 CLEAR

 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW _ctos

 SELECT Ctoc

 IF _tt = 1
    SCATTER MEMVAR
    XCuenta1 = SUBSTR(m.Cuentas,1,10)
    XCuenta2 = SUBSTR(m.Cuentas,11,10)
    XCuenta3 = SUBSTR(m.Cuentas,21,10)
    XCuenta4 = SUBSTR(m.Cuentas,31,10)
    XCuenta5 = SUBSTR(m.Cuentas,41,10)
    XCuenta6 = SUBSTR(m.Cuentas,51,10)
    XCuenta7 = SUBSTR(m.Cuentas,61,10)
    XCuenta8 = SUBSTR(m.Cuentas,71,10)
    XCuenta9 = SUBSTR(m.Cuentas,81,10)
    XCuent10 = SUBSTR(m.Cuentas,91,10)
 ELSE
    SCATTER MEMVAR BLANK
    m.ctocto = diax.ccosto
    STORE SPACE(10) TO XCuenta1, XCuenta2, XCuenta3, XCuenta4, XCuenta5, XCuenta6,;
                   XCuenta7, XCuenta8, XCuenta9, XCuent10
 ENDIF

 @  3, 2 SAY "   Centro de Costo: " GET m.ctocto WHEN (.F.)
 @  5, 2 SAY "       Descripci¢n: "  GET m.Descri PICTURE "@!"
 @  7, 2 SAY "           Cuentas: "
 @  7,22 GET XCuenta1   PICTURE '9999999999' VALID EMPTY(XCuenta1)  .OR. Val_Cuen(XCuenta1,.T.,7,22)
 @  8,22 GET XCuenta2   PICTURE '9999999999' VALID EMPTY(XCuenta2)  .OR. Val_Cuen(XCuenta2,.T.,8,22)
 @  9,22 GET XCuenta3   PICTURE '9999999999' VALID EMPTY(XCuenta3)  .OR. Val_Cuen(XCuenta3,.T.,9,22)
 @ 10,22 GET XCuenta4   PICTURE '9999999999' VALID EMPTY(XCuenta4)  .OR. Val_Cuen(XCuenta4,.T.,10,22)
 @ 11,22 GET XCuenta5   PICTURE '9999999999' VALID EMPTY(XCuenta5)  .OR. Val_Cuen(XCuenta5,.T.,11,22)
 @ 12,22 GET XCuenta6   PICTURE '9999999999' VALID EMPTY(XCuenta6)  .OR. Val_Cuen(XCuenta6,.T.,12,22)
 @ 13,22 GET XCuenta7   PICTURE '9999999999' VALID EMPTY(XCuenta7)  .OR. Val_Cuen(XCuenta7,.T.,13,22)
 @ 14,22 GET XCuenta8   PICTURE '9999999999' VALID EMPTY(XCuenta8)  .OR. Val_Cuen(XCuenta8,.T.,14,22)
 @ 15,22 GET XCuenta9   PICTURE '9999999999' VALID EMPTY(XCuenta9)  .OR. Val_Cuen(XCuenta9,.T.,15,22)
 @ 16,22 GET XCuent10   PICTURE '9999999999' VALID EMPTY(XCuent10)  .OR. Val_Cuen(XCuent10,.T.,16,22)
 READ
 RELEASE WINDOW _ctos

 IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
    m.Cuentas = XCuenta1 + XCuenta2 + XCuenta3 + XCuenta4 + XCuenta5 + XCuenta6 +;
                XCuenta7 + XCuenta8 + XCuenta9 + XCuent10
    IF _tt = 2
       Agrega = F_appd()
    ELSE
       Agrega = F_Lock(1)
    ENDIF

    IF Agrega
       GATHER MEMVAR
    ENDIF

    UNLOCK
 ELSE
    GO BOTTOM
    SELECT diax

    RETURN .f.
 ENDIF
 SELECT diax
RETURN .T.


* 13.- Apertura de Auxiliares
FUNCTION ap_aux
*--------------
PARAMETER xxcta
*- apertura de auxiliares en l¡nea
 DEFINE WINDOW ap_aux FROM 3,5 TO 20,75 TITLE " APERTURA DE AUXILIARES " COLOR SCHEME 10
 ACTIVATE WINDOW ap_aux
 CLEAR

 PRIVATE m.Tipo,     m.Descri,  m.Direccion, m.Codigo
 PRIVATE m.Telefono, m.Ruc      m.Observ

 *- Aqu¡ coloca valores por omisi¢n (default)

 SELECT Auxi
 SCATTER MEMVAR BLANK
 m.Tipo   = Diax.TipAux
 m.Codigo = Diax.Registro
 xxalias = ALIAS()

 vreg = IIF(!EOF(),RECNO(),0)

 @  3, 2 SAY "            C¢digo:" + m.Codigo
 @  5, 2 SAY "       Descripci¢n:" GET m.Descri
 @  7, 2 SAY "         Direcci¢n:" GET m.Direccion
 @  9, 2 SAY "          Tel‚fono:" GET m.Telefono
 @ 11, 2 SAY "             R.U.C:" GET m.Ruc
 @ 13, 2 SAY "     Observaciones:" GET m.Observ

 READ

 IF vreg # 0 .AND. vreg <= RECCOUNT()
    GO vreg
 ENDIF

 IF LASTKEY() <> 27 .AND. yesno("Confirme el ingreso")
    IF f_appd()
       GATHER MEMVAR
    ENDIF
    UNLOCK
 ELSE
    RELEASE WINDOW ap_aux
    SELECT diax
    RETURN .F.
 ENDIF

 RELEASE WINDOW ap_aux
 SELECT diax

RETURN .T.


* 14.- Tipo de Documento de Referencia
FUNCTION V_TipDoc
*----------------
IF LASTKEY() = -2    && F3
   REPLACE TipDcr    WITH TipDoc
   REPLACE DocRef    WITH Comprob
   REPLACE DocAdi    WITH ComAdi
   RETURN .T.
ENDIF
IF EMPTY(TipDcr)
   vAlias = ALIAS()
   SELECT Cuen
   SEEK Diax.Cuenta
   SELECT (vAlias)
   IF Cuen.RefObl .AND. !YesNo("Falta indicar el T.Documento. ¨Continua?")
      RETURN .F.
   ENDIF
   RETURN .T.
ENDIF
IF Val_Para(TipDcr,'TIPDOC','TipDcr')
   RETURN .T.
ENDIF
RETURN .F.


* 15 Pinta la Cuenta Corriente
FUNCTION CtaCte
*--------------
vAli=ALIAS()
vOrd=ORDER()

IF EMPTY(Diax.CodCtc)
   SELECT Caja
*   DEFINE POPUP parametro FROM 0,40 TO 12,75 PROMPT FIELD CodCtc+' '+Descri TITLE ' Cuentas Corrientes '
*   ON SELECTION POPUP parametro DEACTIVATE POPUP
*   ACTIVATE POPUP parametro

    vcta = SPACE(LEN(Diax.CodCtc))
    SET ORDER TO TAG Cajas1
    _f_ = Val_Fun('Caja','CodCtc',"CodCtc +' '+ Descri",VCta,1,3,22,'SUBSTR(Descri,1,14)')

   REPLACE Diax.CodCtc WITH VCta

   @ 9,22 SAY LEFT(Caja.Descri,10)

   RELEASE POPUP parametro

ENDIF
SELECT Diax

RETURN


* 13. Utilizado x T.Auxiliar y Auxiliar.
*FUNCTION NoCanc
*---------------
* Ver si el documento de referencia tiene cancelaciones.
* En caso tiene no se puede modificar el Auxiliar.
IF EMPTY(Diax.Registro)
  RETURN .T.
ENDIF
RETURN .F.


* 13. Utilizado x T.Auxiliar y Auxiliar.
*FUNCTION NoCanc
*---------------
* Ver si el documento de referencia tiene cancelaciones.
* En caso tiene no se puede modificar el Auxiliar.

IF EMPTY(Diax.DocRef) .AND. EMPTY(Diax.TipDocRef)
   RETURN .T.
ENDIF
vReturn = .T.
IF Diax.DocRef = Diax.NumDoc .AND. Diax.TipDcr = Diax.TipDocNum
   vAlias = ALIAS()
   SELECT diar
   SET ORDER TO TAG Diario6
   SEEK Diax.TipDocRef + Diax.DocRef + Diax.TipAux + Diax.Registro
   SCAN WHILE Diax.TipDocRef = Diar.TipDocRef .AND. Diax.DocRef = Diar.Docref;
              .AND. Diax.TipAux = Diar.TipAux .AND. Diax.Registro = Diar.Registro
              IF (Diax.Comprob # Diar.Comprob .OR. Diax.Item # Diar.Item);
                 .AND. Diar.Docref # Diar.Numdoc
                 IF SUBSTR(Diar.Comprob,1,2) = "35"
                    SELECT Comp
                    SEEK Diar.Comprob
                    IF MONTH(Comp.Fecha) # MONTH(actmes) .OR. YEAR(Comp.Fecha) # YEAR(actmes)
                       vReturn = .F.
                    ENDIF
                    SEEK Diax.Comprob
                 ELSE
                    vReturn = .F.
                 ENDIF
              ENDIF
  ENDSCAN
  SELECT Diar
  SET ORDER TO TAG Diario1
  SELECT (vAlias)
ENDIF
RETURN vReturn


*- 3. Documento de referencia

*FUNCTION v_docref
*----------------
* Verifica las provisiones  y cancelaciones.
* Toma en cuenta lo siguiente:
       * No se puede provisionar documento ya provisionado.
       * Si la provisi¢n fue al DEBE la cancelaci¢n es al HABER
       * Si la provisi¢n fue al HABER la cancelaci¢n es al DEBE
       * No se puede cancelar mas del monto que queda por cancelar.
       * El documento debe ser cancelado con la misma cuenta que;
        se provision¢.

 PARAMETERS vGraba

 _fun_     = .T.
 _al_      = ALIAS()
 _rec      = RECNO()
 _doc      = Diax.docref
 _tip      = Diax.TipDcr
 _Aux      = Diax.TipAux
 _Reg      = Diax.Registro
 _Cuen     = Diax.Cuenta
 vSaldo    = 0
 vTipo     = 'D'

 IF EMPTY(_doc)
    vAlias = ALIAS()
    SELECT Cuen
    SEEK Diax.Cuenta
    SELECT (vAlias)
    IF Cuen.RefObl .AND. !YesNo("Falta indicar el T.Documento. ¨Continua?");
       .AND. vGraba
       RETURN .F.
    ENDIF

    REPLACE Diax.TipDcr WITH SPACE(2)
    RETURN .T.
 ENDIF
 IF EMPTY(Diax.Numdoc)
    REPLACE Diax.TipDocNum WITH SPACE(2)
 ENDIF

 SELECT Diax

 *- Ve si es provisi¢n o no
 IF Diax.Docref = Diax.Numdoc .AND. Diax.TipDocRef = Diax.TipDocNum
    _lider = .T.
    vSaldo    = IIF(Diax.Moneda # 'D',Diax.Montos,Diax.Montod)
    vmoneda   = Diax.Moneda
    vTipo     = Diax.Tipo
 ELSE
    _lider = .F.
 ENDIF

 *- Ver si el anterior documento(xDocref) tenia movimientos;
   siempre y cuando sea una provisi¢n.
 IF (xDocRef # _doc .OR. xTipDocRef # _tip) .AND. xNumDoc = xDocRef;
    .AND. xTipDocref = xTipDocNum
    xItem = Diax.Item
    SELECT diar
    SET ORDER TO TAG Diario6
    SEEK xTipDocRef + xDocRef + xTipAux + xRegistro
    SCAN WHILE xTipDocRef = Diar.TipDocRef .AND. xDocRef = Diar.Docref;
         .AND. xTipAux = Diar.TipAux .AND. xRegistro = Diar.Registro
         IF (Diax.Comprob # Diar.Comprob .OR. Diax.Item # Diar.Item);
            .AND. Diar.Docref # Diar.Numdoc
            SELECT Diar
            SET ORDER TO TAG Diario1
            _fun_ = .F.
            _msg2 = 'Documento anterior tiene movimientos'
            SELECT Diax
            REPLACE Docref   WITH xDocref, TipDocref WITH xTipDocref
            REPLACE NumDoc   WITH xNumDoc, TipDocnum WITH xTipDocnum
            REPLACE TipAux   WITH xTipAux, Registro  WITH xRegistro
            @ 13, 11 SAY xTipDocNum
            @ 13, 15 SAY xNumDoc
            @ 14, 11 SAY xTipDocRef
            @ 14, 15 SAY xDocRef
            SELECT (_al_)
            GO _rec
            RETURN _fun_
         ENDIF
         SELECT Diar
    ENDSCAN
    SELECT Diar
    SET ORDER TO TAG Diario1
 ENDIF

 SELECT diar
 SET ORDER TO TAG Diario6

 *- Ver si el documento (en caso de ser cancelaci¢n) existe o no.
 IF !_lider
    Busco = .F.
    vRegistros = ''
    SEEK _Tip + _Doc
    SCAN WHILE _Tip + _Doc = Diar.TipDocRef + Diar.DocRef
         IF Diar.TipDocRef + Diar.DocRef = Diar.TipDocNum + Diar.NumDoc;
            .AND. Diar.Comprob # Diax.Comprob
            IF _Aux + _Reg # Diar.TipAux + Diar.Registro
               vRegistros = vRegistros + IIF(!EMPTY(vRegistros), ', ','') +;
               ALLTRIM(Diar.TipAux) + '-' + ALLTRIM(Diar.Registro)
            ELSE
               vSaldo    = IIF(Diar.Moneda # 'D',Diar.Montos,Diar.Montod)
               vmoneda   = Diar.Moneda
               vTipo     = Diar.Tipo
               Busco     = .T.
               IF _Cuen # Diar.Cuenta
                  _fun_ = .F.
                  _msg2 = 'Provisi¢n con Cuenta :' + Diar.Cuenta
               ENDIF
               EXIT
            ENDIF
         ENDIF
   ENDSCAN
   IF !Busco
      SELECT Diax
      SCAN
         IF _Tip + _Doc = Diax.TipDocRef + Diax.DocRef .AND.;
            Diax.TipDocRef + Diax.DocRef = Diax.TipDocNum + Diax.NumDoc
            IF _Aux + _Reg  # Diax.TipAux + Diax.Registro
               vRegistros = vRegistros + IIF(!EMPTY(vRegistros), ', ','') +;
               ALLTRIM(Diax.TipAux) + '-' + ALLTRIM(Diax.Registro)
            ELSE
               vSaldo    = IIF(Diax.Moneda # 'D',Diax.Montos,Diax.Montod)
               vmoneda   = Diax.Moneda
               vTipo     = Diax.Tipo
               Busco     = .T.
               IF _Cuen # Diax.Cuenta
                  _fun_ = .F.
                  _msg2 = 'Provisi¢n con Cuenta :' + Diax.Cuenta
               ENDIF
               EXIT
            ENDIF
         ENDIF
     ENDSCAN
     GO _rec
     IF !Busco
        SELECT Diax
        _fun_ = .F.
        _msg2 = 'Documento no provisionado'
        IF !EMPTY(vRegistros)
           _fun_ = .F.
           DO Standby WITH  'Doc. con Auxiliares ' + vRegistros
        ENDIF
     ENDIF
   ENDIF
 ENDIF


 * En caso es provisi¢n, verificar que no exista.
 IF _lider .AND. _fun_
    vRegistros = ''
    SELECT Diar
    SEEK _Tip + _Doc
    SCAN WHILE _Tip = Diar.TipDocref .AND. _doc = Diar.Docref;
         .AND. _fun_
         IF Diar.Estado $ 'Au Aj'
            LOOP
         ENDIF
         IF Diax.Comprob # Diar.Comprob .AND. Diar.Docref = Diar.Numdoc .AND. Diar.TipDocref = Diar.TipDocNum
            IF _Aux = Diar.TipAux .AND. _Reg = Diar.Registro
               _fun_ = .F.
               _msg2 = 'Documento ya provisionado'
               EXIT
            ELSE
               vRegistros= vRegistros + IIF(!EMPTY(vRegistros),', ','');
               + ALLTRIM(Diar.TipAux) + '-' + ALLTRIM(Diar.Registro)
            ENDIF
         ENDIF
   ENDSCAN


   SELECT Diax
   GO TOP
   SCAN WHILE _fun_
        IF Diax.Estado $ 'Au Aj'
           LOOP
        ENDIF
        IF Diax.TipDocRef + Diax.Docref = _Tip + _doc .AND.;
           Diax.TipDocref + Diax.DocRef = Diax.TipDocNum + Diax.NumDoc
           IF _rec # RECNO()
              IF _Aux = Diax.TipAux .AND. _Reg = Diax.Registro
                 _fun_ = .F.
                 _msg2= 'Documento ya provisionado'
                 EXIT
              ELSE
                 vRegistros= vRegistros + IIF(!EMPTY(vRegistros),', ','');
                 + ALLTRIM(Diax.TipAux) + '-' + ALLTRIM(Diax.Registro)
              ENDIF
            ENDIF
        ENDIF
   ENDSCAN
   SELECT Diax
   GO _rec
   IF !EMPTY(vRegistros) .AND. vGraba
      IF !YesNo('Documento provisionado con el(los) AUXILIAR(ES) ' +;
         vRegistros +'. ¨Continua?')
         _fun_ = .F.
         _msg2 = 'Documento ya provisionado'
      ENDIF
   ENDIF
 ENDIF


 *- Ubica el saldo de la provisi¢n
 SELECT Diar
 SEEK _Tip + _doc + _Aux + _Reg
 SCAN WHILE _Tip = Diar.TipDocref .AND. _doc = Diar.Docref .AND.;
      _Aux = Diar.TipAux .AND. _Reg = Diar.Registro .AND. _fun_
      IF Diar.Estado $ 'Au Aj '.OR. SUBSTR(Diar.Comprob,1,2) = "35"
         LOOP
      ENDIF
      IF Diax.Comprob # Diar.Comprob .AND. Diar.Docref # Diar.Numdoc;
         .AND. Diax.Item # Diar.Item
         IF vtipo = 'D'
            IF Diar.Tipo = 'D'
               _fun_ = .F.
               _msg2= 'Documento lider al Debe'
            ELSE
               vSaldo = vSaldo - IIF(vmoneda # 'D', Diar.Montos, Diar.Montod)
            ENDIF
         ELSE
            IF Diar.Tipo = 'D'
               vSaldo = vSaldo - IIF(vmoneda # 'D', Diar.Montos, Diar.Montod)
            ELSE
               _fun_ = .F.
               _msg2= 'Documento lider al Haber'
            ENDIF
         ENDIF
      ENDIF
 ENDSCAN
 IF vsaldo < 0
    _fun_ = .F.
    _msg2 = 'Documento cancelado'
 ENDIF
 SET ORDER TO TAG Diario1
 SELECT diax
 GO TOP
 IF vsaldo >= 0 .AND. _fun_
    SCAN WHILE _fun_
         IF Diax.Estado $ 'Au Aj ' .OR. SUBSTR(Diax.Comprob,1,2) = "35"
            LOOP
         ENDIF
         IF Diax.Docref # Diax.Numdoc .AND. _doc = Diax.Docref;
            .AND. _Aux = Diax.TipAux .AND. _Reg = Diax.Registro;
            .AND. _Tip = Diax.TipDocref
            IF vtipo = 'D'
               IF Diax.Tipo = 'D'
                  _fun_ = .F.
                  _msg2= 'Documento lider al Debe'
               ELSE
                  vSaldo = vSaldo - IIF(vmoneda # 'D', Diax.Montos, Diax.Montod)
               ENDIF
            ELSE
               IF Diax.Tipo = 'D'
                  vSaldo = vSaldo - IIF(vmoneda # 'D', Diax.Montos, Diax.Montod)
               ELSE
                  _fun_ = .F.
                  _msg2= 'Documento lider al Haber'
               ENDIF
            ENDIF
         ENDIF
   ENDSCAN
   IF vsaldo < 0
      _fun_ = .F.
      _msg2 = 'Importe excede al saldo'
   ENDIF
 ENDIF
 SELECT Diax
 IF _rec # 0
    GO _rec
    IF vsaldo < 0 .AND. !_lider
     *  REPLACE Diax.Montos WITH 0
    ENDIF
    IF vsaldo < 0 .AND. _lider
     *  REPLACE Diax.Montos WITH xMontos    && JFG 
    ENDIF
 ENDIF
 IF _fun_ .AND. _lider .AND. EMPTY(Diax.Vencim)
    REPLACE Diax.Vencim WITH FecEmi
 ENDIF
 IF !_fun_ .AND. vsaldo >= 0 .AND. vGraba
    REPLACE Diax.DocRef    WITH SPACE(LEN(Diax.DocRef))
    REPLACE Diax.TipDocRef WITH SPACE(LEN(Diax.TipDocRef))
    @ 14, 11 SAY Diax.TipDocRef
    @ 14, 15 SAY Diax.DocRef
    _fun_ = -1
    DO Standby WITH _msg2
 ENDIF
 SELECT (_al_)
 GO _rec

RETURN _fun_


FUNCTION Tdoc
*------------
vAli=ALIAS()
vOrd=ORDER()
SELECT Parma
DEFINE POPUP parametro FROM 0,40 TO 15,75 PROMPT FIELD ALLTRIM(Codigo)+' '+Descri TITLE ' Tipo de Documentos '
ON SELECTION POPUP parametro DEACTIVATE POPUP
ACTIVATE POPUP parametro

m.TipDoc=ALLTRIM(Parma.Codigo)
@ 0,20 SAY ALLTRIM(Parma.Descri)

RELEASE POPUP parametro
SET FILTER TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN


FUNCTION Rebaja
*--------------
 vComprob =Diar.DocRef
 vComAdi  =Diar.DocAdi
 vCuenta  =Diar.Cuenta
 vTipAux  =Diar.TipAux
 vRegistro=Diar.Registro
 vMontos  =Diar.Montos
 vRec=RECNO()
 vOrd=ORDER()
 SET ORDER TO TAG Diario9
 SEEK vComprob+vComAdi+vCuenta+vTipAux+vRegistro
 
 IF FOUND()
    REPLACE Diar.Estado  WITH SPACE(2)
    REPLACE Diar.SalDPen WITH 0
 ENDIF
 
 SET ORDER TO TAG vOrd
 GO vRec
 RETURN     

* Valida la existencia de la cuenta.
FUNCTION Valida
*--------------
 PUBLIC vMonD,vMonH
 STORE 0 TO vMonD,vMonH
 vRec=RECNO()
 vOrd=ORDER()
 vAli=ALIAS()
 SELECT Diar
 vRec11=RECNO()
 vOrd11=ORDER()
 vAli11=ALIAS()
 vComprob =Diax.DocRef
 vComAdi  =Diax.DocAdi
 vCuenta  =Diax.Cuenta
 vTipAux  =Diax.TipAux
 vRegistro=Diax.Registro
 SET ORDER TO TAG Diario9
 SEEK vComprob+vComAdi+vCuenta+vTipAux+vRegistro
 IF FOUND() .AND. Diar.Tipo#Diax.Tipo
    IF Diar.Estado = '50'
       DO StandBy WITH "EL documento ya fue !! Cancelado !!"
    ENDIF     
    IF Diar.SalDPen > 0 .OR. Diar.SalDPen = 0
       IF Diar.SalDPen > 0
          IF Diar.Montos > 0 .OR. Diar.Montos < 0
             REPLACE Diar.SalDPen WITH Diar.Montos - (Diax.Montos + Diar.SalDPen)
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ENDIF
          IF Diar.Montos = 0
             REPLACE Diar.SalDPen WITH Diar.SalDPen - Diax.Montos
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ENDIF
       ELSE
          IF Diar.SalDPen = 0
             IF Diar.Montos = 0
                REPLACE Diar.SalDPen WITH 0
             ELSE
                REPLACE Diar.SalDPen WITH Diar.Montos - Diax.Montos
                IF Diar.SalDPen=0
                   REPLACE Diar.Estado WITH '50'
                ENDIF
             ENDIF
          ENDIF
       ENDIF
    ELSE
       IF Diar.SalDPen < 0
          IF Diar.Montos=0
             REPLACE Diar.SalDPen WITH Diar.Montos - Diax.Montos
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ELSE
             REPLACE Diar.SalDPen WITH Diar.Montos - Diax.Montos  &&Saldo Debe
             IF Diar.SalDPen=0
                REPLACE Diar.Estado WITH '50'
             ENDIF
          ENDIF
       ENDIF
       IF Diar.Montos=Diax.Montos
          REPLACE Diar.Estado WITH '50'   &&Asiento Cancelado
          REPLACE Diar.SalDPen WITH 0
       ENDIF
    ENDIF
    IF Diar.SalDPen > 0 .OR. Diar.SalDPen < 0
       REPLACE Diar.Estado WITH '00'   &&Doc.Pendiente de cancelaci¢n
    ELSE
       REPLACE Diar.Estado WITH '50'   &&Asiento Cancelado
    ENDIF
 ENDIF
 SELECT (vAli11)
 SET ORDER TO TAG vOrd11
 SELECT Diax
 SET ORDER TO TAG vOrd
 GO vRec
 RETURN

FUNCTION v_Tdoc
*--------------
IF EMPTY(m.TipDoc)
 vAli=ALIAS()
 vOrd=ORDER()
 SELECT Parma
 SET FILTER TO Tipo='TIPDOC' and !EMPTY(Codigoaux)
 SET INDEX TO (vIndPar) 
 DEFINE POPUP parametro FROM 0,40 PROMPT FIELD ALLTRIM(CodigoAUX)+' '+Descri TITLE ' Tipos de Documento '
 ON SELECTION POPUP parametro DEACTIVATE POPUP
 ACTIVATE POPUP parametro
 SET ORDER TO PARMAE1
 m.TipDoc=ALLTRIM(Parma.CodigoAux) &&SEGUNDO CODIGO DE PARMAE
* @ 0,20 SAY ALLTRIM(Parma.Descri)
 RELEASE POPUP parametro
 SET FILTER TO
 SELECT (vAli)
 SET ORDER TO vOrd
ELSE
  vAli=ALIAS()
  vOrd=ORDER()
  SELECT Parma
  SET FILTER TO Tipo='TIPDOC' 
  LOCATE FOR TIPO='TIPDOC' AND ALLTRIM(CODIGOAUX)=ALLTRIM(m.Tipdoc) 
  IF !FOUND()
  	SET FILTER TO
	SELECT (vAli)
	SET ORDER TO vOrd
	RETURN .F.
  ENDIF
*     @ 0,20 SAY ALLTRIM(Parma.Descri)
  SET FILTER TO
  SELECT (vAli)
  SET ORDER TO vOrd
ENDIF
mverifd=ALLTRIM(Parma.descriau2)
RETURN

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
**@ mFila,2 SAY 'Obra : '+COMP->TIPOBR+'-'+COMP->CODOBRA
**@ mFila,17 SAY VER_OBRA()

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


FUNCTION ver_obra
*------------------
* Funcion lee obra para el reporte de notas de contabilidad
*----------------------------------------------------------
*fefe
IF !EMPTY(Comp.Codobra)
    =Val_Obra(ALLT(Comp.CodObra),'Comp.Codobra','D') 
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

FUNCTION vFecAct
*---------------
IF !EMPTY(m.fecha)
   vret = ( MONTH(m.fecha) > MesCerrado )
   IF !vret
      DO standby WITH "­­­ Dicho mes ya est  cerrado !!!" 
   ENDIF
   RETURN vret   
ENDIF
RETURN .T.   


PROCEDURE Con_Ctas
*-----------------
* Este procedure genera una tabla para verificaci¢n de doc. extensos
Area = SELECT()
vReg = RECNO()
num_com = diar.comprob
num_adi = diar.comadi
cod_fte = diar.codfte
ON KEY LABEL F12
CuentasTemp = sys(3) + '.dbf'
kk1         = sys(3) + '.idx'
kk2         = sys(3) + '.idx'
Pic         = '9,999,999,999.99'
ERASE (CuentasTemp)
CREATE TABLE (CuentasTemp) ( Cuenta C(2), deBe N(18,3), haber N(18,3) )
USE

USE (CuentasTemp) IN 20 Alias Temp
SELE Temp
INDEX ON cuenta TO (kk1)

SELECT diar
SET ORDER TO TAG DIARIO11
DO ESPERA WITH 1,"Consistencia en proceso, espere ..."
SEEK num_com+num_adi+cod_fte
SCAN WHILE Num_Com + num_adi + cod_fte = Diar.Comprob + Diar.ComAdi + Diar.Codfte
    xcu = LEFT(diar.Cuenta,2)
    SELE Temp
    SEEK xcu
    IF FOUND()
       IF diar.Tipo='D'
          REPLACE Temp.debe WITH Temp.debe + Diar.Montos
       ELSE
          REPLACE Temp.haber WITH Temp.haber + Diar.Montos
       ENDIF
    ELSE
       Append Blank
       REPLACE Temp.Cuenta WITH xcu
       IF Diar.Tipo='D'
          REPLACE Temp.debe WITH Diar.Montos
       ELSE
          REPLACE Temp.haber WITH Diar.Montos
       ENDIF
    ENDIF
    
    Select Diar
ENDSCAN
DO ESPERA WITH 2
SELE Temp           
SUM DEBE  TO vDeb
SUM HABER TO vHab
GO TOP
BROW WINDOW wVerif ;
     FIELDS CUENTA :H='Cta', DEBE :H="    Debe " :P=pic ,  HABER :H="   Haber " :P=pic NOMODIFY

IF vDeb<>vHab
   WAIT WINDOW "Diferencia: "+ALLTRIM(tran(vdeb-vhab,pic)) NOWAIT
ENDIF   

IF yesno("¨ Imprime la Consulta ?")
   DO REPORTE WITH 2,"consis"," Consistencia de Documentos "
ENDIF   

USE IN 20
ERASE(CuentasTemp)
ERASE(kk1)
ERASE(kk2)
SELE(Area)
GOTO vreg
ON KEY LABEL F12 DO cor_obr 
RETURN


FUNCTION Verif_Ctas
*------------------
*- Verifica que las cuentas tengan el mismo c¢digo de sector
RETURN .T.



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


FUNCTION no_dupli
*----------------
*- Esta funci¢n no permite la duplicidad en el n£mero de comprobante
PARAMETERS v_Com, vAdi
vMens = "N£mero de Documento YA EXISTENTE."
vAli  = ALIAS()
vExpr = PADR(ALLTRIM(v_com),14,' ') + PADR(ALLTRIM(vAdi),2,' ')
SELECT comp
SET ORDER TO COMPRO7
SEEK vexpr
enc   = FOUND()
SELECT(vAli)
IF FOUND()
   * DO STANDBY WITH vMens
   _msg2 = vMens
ENDIF   
RETURN !FOUND()



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
   CodObra   C(4),;
   CodPart   C(5))
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

FUNCTION Val_Tipo
*----------------
  PARAMETERS mValor, Filtro, mVariable    
  PRIVATE mAlias, mLong
  mLong = 40               && Longitud campo DESCRI
  mAlias  = ALIAS()

  SELECT Parma
  SEEK Filtro+mValor

  IF !FOUND()   
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   RETURN	
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELDS;
           Codigo+' '+CodigoAux+' '+SUBSTR(Descri,1,40)
           ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = Parma.Codigo
  *mCuenta= Parma.DescriAu2
  mcuenta =LEFT(Parma.DescriAu2,4)
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = SUBSTR( Parma.DescriAux, 1, mLong)
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
IF LASTKEY()#27
      REPLACE &mVariable WITH mValor
      RETURN .T.
ENDIF
FUNCTION Aut_Fin
*-----------------------------------------------------
* Funci¢n que Permite validar los tipos financieros en
* Forma autom tica
* _vtipdoc : Variable que recibe el tipo de documento
* _vCuenta : Variable que recibe el tipo de documento
*-----------------------------------------------------
PARAMETER  _vtipdoc,_vCuenta 
vAli=ALIAS()
SELE AutFin
SEEK _vtipdoc+_vCuenta 
IF FOUND()
   vTipfin  =Autfin.Tipfin+Autfin.Tipfin95
ENDIF
SELE (vAli)
RETURN vTipfin


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


PROCEDURE cor_obr
*----------------
* Corrige Datos de la Obra
  SELECT comp
  SCATTER MEMVAR
  ACTIVATE WINDOW Wind_C0
  @ 01,16 GET m.Glosa1      PICTURE "@S60"
  @ 02,16 GET m.Glosa2      PICTURE "@S60"
  @ 03,16 GET m.Glosa3      PICTURE "@S60"
  @ 04,16 GET m.Glosa4      PICTURE "@S60"
  @ 05,16 GET m.CodCal      
  @ 05,47 GET m.Tipfun      PICTURE "!"  VALID VAL_PARA(m.tipfun,'TIPFUN',' ',47,15,2)   
  ** No se tienen obras en benef.
  ** @ 07,16 GET m.TipObr      PICTURE '!'  VALID VAL_para(m.tipobr ,'TIPOBR','C',26,40)    WHEN ALLTRIM(M.TIPFUN)='I' 
  ** End.
  @ 07,18 GET m.Codobra     PICTURE "@!" VALID Val_Obra(m.CodObra,'m.CodObra',' ',27,40) WHEN ALLTRIM(M.TIPFUN)='I'
**  @ 08,16 GET m.Codpart     PICTURE '!!.!!' valid VAL_PART1(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),' ',16,50)  AND partObr(m.codpart)   WHEN ALLTRIM(m.tipfun)='I'
  READ 
  IF LASTKEY()=27
     DO STANDBY WITH 'Proceso Cancelado'
  ELSE
     IF f_lock(1)
        REPLACE comp.glosa1  WITH m.glosa1
        REPLACE comp.glosa2  WITH m.glosa2
        REPLACE comp.glosa3  WITH m.glosa3
        REPLACE comp.glosa4  WITH m.glosa4
        REPLACE comp.codcal  WITH m.codcal
        REPLACE comp.tipfun  WITH m.tipfun
        REPLACE comp.tipobr  WITH m.tipobr
        REPLACE comp.codobra WITH m.codobra
        REPLACE comp.codpart WITH m.codpart
        UNLOCK
    ENDIF
  ENDIF
DO VISTA
RETURN
  


FUNCTION MFecha
*--------------
PARAMETER vmes
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM( ALLTRIM(SUBSTR(Meses,VAL(VMes)*9-8,9)) + ' ' )


FUNCTION partObr
*---------------
PARAMETERS wcodpart
IF SUBS(wcodpart,4,2)='00'
   DO STANDBY WITH 'Debe ser una partida espec¡fica'
   RETURN .F.
ENDIF   
IF !(LEFT(wcodpart,2)$'07 08')
   DO STANDBY WITH 'Debe especificar la partida 07 ¢ 08'
   RETURN .F.
ENDIF
RETURN .T.
   
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
SELECT EstFin
SET INDEX TO (idxauxi) 
RETURN



FUNCTION DESCRI
*--------------
PARAMETER vcuenta 
vAli=ALIAS()
*xCuenta=LEFT(Cuenta,2)
*vCuenta=LEFT(Cuenta,2)+'00000000'
SELECT Cuen
SEEK vCuenta
IF FOUND()
 *  =ZZ()
   _z=Val_Fun('Cuen','Cuenta','Descri',vCuenta)
   RETURN _z
ENDIF
SELECT (vAli)
RETURN

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


FUNCTION vv_Tdoc
*----------------
IF EMPTY(vTipDcr)
	 vAli=ALIAS()
	 vOrd=ORDER()
	 SELECT Parma
	 SET FILTER TO Tipo='TIPDOC' and !EMPTY(Codigoaux)
	 SET INDEX TO (vIndPar) 
	 DEFINE POPUP parametro FROM 0,40 PROMPT FIELD ALLTRIM(CodigoAUX)+' '+Descri TITLE ' Tipos de Documento '
	 ON SELECTION POPUP parametro DEACTIVATE POPUP
	 ACTIVATE POPUP parametro
	 SET ORDER TO PARMAE1
	 vTipDcr=ALLTRIM(Parma.CodigoAux) &&SEGUNDO CODIGO DE PARMAE
	 vTipDoc=ALLTRIM(Parma.Codigo)    &&PRIMER  CODIGO DE PARMAE
	 RELEASE POPUP parametro
	 SET FILTER TO
	 SELECT (vAli)
	 SET ORDER TO vOrd
	 REPLACE TipDoc WITH vTipDoc
	 REPLACE TipDcr WITH vTipDcr
ELSE
	 vAli=ALIAS()
	 vOrd=ORDER()
	 SELECT Parma
	 SET FILTER TO Tipo='TIPDOC' 
	 LOCATE FOR TIPO='TIPDOC' AND ALLTRIM(CODIGOAUX)=ALLTRIM(vTipdcr) 
	 IF !FOUND()
	  	SET FILTER TO
		SELECT (vAli)
		SET ORDER TO vOrd
		RETURN .F.
	ELSE
		vTipDoc=ALLTRIM(Parma.Codigo)		 
	ENDIF
	SET FILTER TO
	SELECT (vAli)
	SET ORDER TO vOrd
	
	REPLACE TipDoc WITH vTipDoc
	
ENDIF
mverifd=ALLTRIM(Parma.descriau2)
RETURN


PROCEDURE LIQUIDOC
*------------------

OK=VE_PASSW('NANCY')
IF !OK
	ON KEY LABEL F11
	SELECT Comp
	DO VISTA
    RETURN
ENDIF

SELECT Diar
SET ORDER TO TAG Diario11
SEEK m.Comprob + m.ComAdi + xCodfte
IF liquIDA='û' and liquSU='û'
   DO STANDBY WITH 'Documento ya liquidado por el Sistema ...!, no es posible anular liquidaci¢n...!'
ELSE
 IF liquSU='û' AND Yesno(" Anula Liquidaci¢n ?")
 	SCAN WHILE Diar.Comprob + Diar.ComAdi + Diar.Codfte = m.Comprob + m.ComAdi + xCodfte
    	    IF F_LOCK(1)
        	   REPLACE Diar.LiquSU  WITH ' ' 
	        ENDIF
    	    UNLOCK    
        	Sele Diar
	ENDSCAN
 ELSE
 	SCAN WHILE Diar.Comprob + Diar.ComAdi + Diar.Codfte = m.Comprob + m.ComAdi + xCodfte
    	    IF F_LOCK(1)
        	   REPLACE Diar.LiquSU WITH 'û' 
	        ENDIF
    	    UNLOCK    
        	Sele Diar
	ENDSCAN
 ENDIF	
ENDIF
	
ON KEY LABEL F11
SELECT Comp
DO VISTA
RETURN

PROCEDURE  Liqui_Usu
*-------------------
* Procedimiento para liquidar una sola cuenta
* nota: Felipe observ¢ que solo deber¡a eliminar una cuenta 
*       y no todas las cuentas del documento.
*------------------------------------------------------------------

* HIDE POPUP ALL
 ON KEY LABEL F9
 ON KEY LABEL F4
 ON KEY LABEL F10 KEYBOARD CHR(23)
 ON KEY LABEL F11 DO Marcar
 SCATTER MEMVAR
 SELECT Diar
 vOrd=ORDER()
 SET ORDER TO TAG Diario11
 BROWSE;
  NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT ;
  WINDOW Wind_LQ KEY m.Comprob + m.ComAdi + xCodfte;   
  FIELDS;
  LiqUsu   :H='û'         :R,;  
  fecemi   :H='Emisi¢n'    :R,;
  cuenta   :H=' Cuenta'    :R,;
  tipo     :H='D/H'        :R,;
  montos   :H=' Importe S/. ':P='99,999,999.99':R,;
  SaldPen  :H='A cuenta S/. ':P='99,999,999.99':R,;
  vProvee  = rAuxil( diaR.tipaux,diaR.registro ) :H="Proveedor" :30

 SET ORDER TO TAG vOrd
 SELECT Comp
 ON KEY LABEL F11 DO Liqui_usu
 ON KEY LABEL F10 KEYBOARD CHR(23)
 DO VISTA
RETURN


PROCEDURE marcar
*---------------
vSaldPen=0
SELECT Diar
SET ORDER TO TAG Diario11
IF liquIDA='û' and liquSU='û'
   DO STANDBY WITH 'Documento ya liquidado por el Sistema ...!, no es posible anular liquidaci¢n...!'
ELSE
 IF (liquSU='û' OR SaldPen>0) AND Yesno(" Anula Liquidaci¢n O Rebaja ?")
    	    IF F_LOCK(1)
        	   REPLACE Diar.LiquSU   WITH ' ' 
        	   REPLACE Diar.SalDpen  WITH 0 
	        ENDIF
    	    UNLOCK    
        	Sele Diar
 ELSE
      IF Yesno("Anula el Documento en Forma Total ?")
    	    IF F_LOCK(1)
        	   REPLACE Diar.LiquSU WITH 'û' 
	        ENDIF
    	    UNLOCK    
        	Sele Diar
      ELSE
      
      *----------------------------
      DO WHILE .T.
	      ACTIVATE WINDOW STANDBY
	      @ 1,3 SAY "Ingrese el Monto a Rebajar : " GET vSaldPen PICTURE "999,999,999,999.99"
	      READ
	      IF Diar.Montos < vSaldPen
	      	WAIT WINDOW "Monto no Permitido"
	      ELSE
	      	EXIT
	      ENDIF
	  ENDDO
      
      DEACTIVATE WINDOW STANDBY
      *----------------------------
    	    IF F_LOCK(1)
        	   REPLACE Diar.SalDpen  WITH vSaldPen
	        ENDIF
    	    UNLOCK    
    	    vSaldPen=0
        	Sele Diar
      ENDIF
 ENDIF	
ENDIF
RETURN