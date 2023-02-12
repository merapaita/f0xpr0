*--------------------------------------------------------------------------
* RegPre.Prg
* Registra Presupuestos
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE Maepar   IN 2   order tag Maepar1  ALIAS Presu
USE Itepar   IN 3   order tag Itepar1  ALIAS Itepar
USE Clase    IN 4   order tag clase1   ALIAS clase
USE Maepre   IN 5   order tag Maepre1  ALIAS Maepre
USE RepoPre  IN 6                      ALIAS Repo   EXCLUSIVE
*-
*- Mensajes de aviso al usuario
Vmens01 = 'Registro de Presupuesto'
Vmens02 = ' Presupuesto : REVISION '
Vmens04 = 'Dicho Presupuesto no fue encontrado'
Vmens05 = 'No existe Presupuesto anterior'
Vmens06 = 'No existe Presupuesto siguiente'
Vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Presupuesto ha sido anulado'
Vmens10 = 'El Presupuesto ya est  Atendido'
Vmens11 = 'El Presupuesto ha sido devuelto'
SELECT Presu
GO TOP

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10


 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Presupuesto' ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 03,40 TO 15,79 PANEL ;
 TITLE '° ®F10¯ Sale °'

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Eliminar'   AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO elimi
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_1
 CLEAR

 @  1, 2 SAY "          Periodo :"
 @  3, 2 SAY "          Volumen :"
 @  4, 2 SAY "           Sector :"
 @  5, 2 SAY "           Pliego :"
 @  6, 2 SAY " Fte.Financiamnto :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "            Nivel :"

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Presu
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_1
 SCATTER MEMVAR
*@  0,60 SAY IIF( m.Estado= '00','Emitido  ',IIF( m.Estado = '20','         ',IIF(m.Estado='99','Anulado  ',IIF(m.Estado='50','Atendido ',' ------- '))))

 @  1,22 SAY m.Periodo
 @  3,22 SAY '02    GOBIERNOS REGIONALES'
 @  4,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
 @  5,22 SAY '001   REGION GRAU'
 @  6,22 SAY VAL_PARA(m.CodFte,'CODFTE','V',22,45)
 @  7,22 SAY VAL_PARA(m.CodPrg,'CODPRG','V',22,40)
 @  8,22 SAY VAL_PARA(m.TipFun,'TIPFUN','V',22,40)

 DO VISTA_HIJO

PROCEDURE VISTA_HIJO
*-------------------

SELECT ItePar
SET ORDER TO 1
hide popup all
GO TOP
seek m.periodo+alltrim(m.codprg)+alltrim(m.codfte)

DO CASE
   CASE alltrim(m.TipFun)='F'
        BROWSE ;
          NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+alltrim(m.codprg)+alltrim(m.codfte)+alltrim(m.tipfun) TIMEOUT 0.001 ;
          WINDOW Wind_2 ;
          FIELDS;
          CodPart     : H= 'Partida',;
          CodSubPr    : H= 'SubPrg' ,;
          CodAct      : H= 'CodAct' ,;
          AA=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D') :H='Descripci¢n' :40,;
          Valpart     : H= 'Asignaci¢n' :P='9,999,999.99'
   CASE alltrim(m.TipFun)='I'
        BROWSE ;
          NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY m.periodo+alltrim(m.codprg)+alltrim(m.codfte)+alltrim(m.tipfun) TIMEOUT 0.001 ;
          WINDOW Wind_2 ;
          FIELDS;
          CodPart     : H= 'Partida',;
          CodSuBPr    : H= 'SubPrg' ,;
          CodProy     : H= 'Proy' ,;
          CodSupry    : H= 'SubPy' ,;
          AA=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D') :H='Descripci¢n' :36,;
          Valpart     : H= 'Asignaci¢n' :P='9,999,999.99'
ENDCASE
HIDE WINDOW WIND_2
SELE presu
RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE PRESU
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SET RELATION TO PERIODO+CODPRG+CODFTE+TIPFUN INTO ITEPAR
 SET SKIP TO ITEPAR
 vTemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 PRESU.Periodo   :H='Per',;
 PRESU.codfte    :H='Fte',;
 PRESU.codprg    :H='Prg',;
 ITEPAR.codsubpr :H='Spg',;
 ITEPAR.codact   :H='Act',;
 ITEPAR.codproy  :H='Pry',;
 ITEPAR.CODSUPRY :H='Spy',;
 ITEPAR.tipfun   :H='T',;
 ITEPAR.CODPART  :H='Part',;
 ITEPAR.VALPART  :H='Ejecutado':P='9,999,999.99',;
 xx=VAL_PART(SUBSTR(itepar.CodPart,4,2),LEFT(itepar.CodPart,2),'D',28,50) :H='Descripci¢n'
*PRESU.DESCRI:H='Descripci¢n'
*Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD'

 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE PRESU
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
vCodFte  = '   '
vCodPrg  = '  '
vCodSubPr= '   '
vTipFun  = ' '
 DEFINE WINDOW Lis FROM 09,12 TO 16,68 DOUBLE ;
 TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis


 @  1,2 say ' Periodo : ' GET vPeriodo   PICTURE '!!'
 @  2,2 say '  Fuente : ' GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',14,30)
 @  3,2 say 'Programa : ' GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',14,30)
 @  4,2 SAY '   Nivel : ' GET vTipFun    PICTURE '!'   VALID VAL_PARA(vTipFun,'TIPFUN',' ',14,30)
 READ

DEACTIVATE WINDOW lis

 IF EMPTY(vPeriodo) .or. LASTKEY()=27
    RETURN
 ELSE
    set order to 1
    SEEK vPeriodo+ALLTRIM(vCodPrg)+alltrim(vCodFte)+alltrim(vTipFun)
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
vnum_pec=padl(alltrim(str(vnum_pec,4)),4,'0')
retur .t.

PROCEDURE Anter
*--------------
 SELE PRESU
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
 SELE PRESU
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
   * El Presupuesto ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 IF Estado = '50'
   DO STANDBY WITH Vmens12
   RETURN
 ENDIF
 *****
 SELECT Presu
 SCATTER MEMVAR

 DO PANTALLA

 @  1,22 GET m.Periodo   PICTURE '!!'  DISABLE
 @  3,22 SAY '02    GOBIERNOS REGIONALES'
 @  4,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
 @  5,22 SAY '001   REGION GRAU'
 @  6,22 GET m.CodFte    PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,45)  DISABLE
 @  7,22 GET m.CodPrg    PICTURE '!!'  VALID VAL_PARA(m.CodPrg,'CODPRG',' ',22,40)  DISABLE
 @  8,22 GET m.TipFun    PICTURE '!'   VALID VAL_PARA(m.TipFun,'TIPFUN',' ',22,30)  DISABLE
 READ
 IF LASTKEY() # 27
    ok=Trabaja_hijo()
    IF OK and LASTKEY()#27
          SELECT PRESU
        *  GATHER MEMVAR
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

 UNLOCK ALL
 SELECT Presu
 DO Vista
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
SELECT Presu
OP=ORDER()
vTemp = Recno()
 DO Pantalla
 vPeriodo = str(year(DATE())-1900+1,2)
 store space(3) to vCodFte
 store space(2) to vCodPrg
 store space(1) to vTipFun
 vTemp = Recno()
 @ 1,22 GET vPeriodo   PICTURE '!!'
 @ 3,22 SAY '02    GOBIERNOS REGIONALES'
 @ 4,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
 @ 5,22 SAY '001   REGION GRAU'
 @ 6,22 GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',22,45)
 @ 7,22 GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40)
 @ 8,22 GET vTipFun    PICTURE '!'   VALID VAL_PARA(vTipFun,'TIPFUN',' ',22,40)

 READ valid Val_read()

vKey = alltrim(vPeriodo)+alltrim(vCodPrg)+alltrim(vCodFte)+alltrim(vTipFun)
 SEEK vKey

 IF !FOUND()
    SCATTER MEMVAR BLANK
 ELSE
    DO STANDBY WITH 'Ya esta Registrado el Programa'
   *GO vTemp
    DO VISTA
    RETURN
 ENDIF
 IF LASTKEY() # 27
     m.Estado = '00'
     m.fecemi = date()
     m.CodFte = vCodFte
     m.CodPrg = vCodPrg
     m.TipFun = vTipFun
    m.Periodo = vPeriodo
     DO WHILE .T.
        ok=Trabaja_hijo()
        IF LASTKEY() # 27
           IF YESNO('¨ Confirme el ingreso ?')
              OK=.T.
              EXIT
           ENDIF
        ELSE
           IF YESNO('¨ Cancela el ingreso ?')
              DO STANDBY WITH ' Cancelado el Ingreso ..'
              ok = .F.
              EXIT
           ELSE
              LOOP
           ENDIF
        ENDIF
     ENDDO
     IF ok and lastkey()#27
        SELECT PRESU
        IF F_Appd()
           GATHER MEMVAR
        ENDIF
     ELSE
        SELE ItePar
        seek m.periodo+alltrim(m.codprg)+alltrim(m.codfte)
        SCAN WHILE Periodo=m.Periodo and CodPrg = alltrim(m.CodPrg) and codfte=alltrim(m.codfte)
             if rlock()
                delete next 1
             endif
        endscan
     ENDIF
 ENDIF

 UNLOCK ALL
 SELECT Presu
 SET ORDE TO (OP)
 GO VTEMP
 DO Vista
 RETURN


PROCEDURE Trabaja_Hijo
*---------------------
 vsun=.t.
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELECT ItePar
 SET ORDER TO IIF(alltrim(m.TipFun)='F',3,4)
 SEEK m.periodo+alltrim(m.codprg)+alltrim(m.codfte)

 IF !FOUND()
    DO agreg_item
 ENDIF

 DO CASE
    CASE alltrim(m.TipFun)='F'
         BROWSE ;
           NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+alltrim(m.codprg)+alltrim(m.codfte) ;
           WINDOW Wind_2 ;
           FIELDS;
           CodPart     : H= 'Partida' :V=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'codpart'):F ,;
           CodSuBPr    : H= 'SubPrg'  :V=VAL_CALE1(CodSubPr,'CODSUB'+alltrim(m.CodPrg)+SPAC(4),'codsubpr'):F :W=!EMPTY(CodPart),;
           CodAct      : H= 'CodAct' :W=VALIDA():V=VALIDA():F,;
           aa = IIF(!EMPTY(CODPART),VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(CodPart) ,;
           Valpart     : H= 'Asignaci¢n' :P='9,999,999.99' :W=!EMPTY(Codpart) AND Chequeo()

    CASE alltrim(m.TipFun)='I'
         BROWSE ;
           NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH  KEY m.periodo+alltrim(m.codprg)+alltrim(m.codfte) ;
           WINDOW Wind_2 ;
           FIELDS;
           CodPart     : H= 'Partida':V=VAL_PART(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'codpart'):F ,;
           CodSuBPr    : H= 'SubPrg' :V=VAL_CALE1(CodSubPr,'CODSUB'+alltrim(m.CodPrg)+SPAC(4),'codsubpr'):F :W=!EMPTY(Codpart),;
           CodProy     : H= 'Proy' :W=VALIDA() :V=VALIDA():F,;
           CodSupry    : H= 'SubPy' :W=!EMPTY(Codproy),;
           DesSupry    : H= 'Descripci¢n Sub_Proyecto':25 :W=!EMPTY(Codproy),;
           Valpart     : H= 'Asignaci¢n' :P='99,999,999.99' :W=!EMPTY(Codpart) AND CHEQUEO() ,;
           UbiCac      : H= 'Ubicaci¢n' :W=ITEPAR.Valpart#0,;
           ModEje      : H= 'MEj' :W=Valpart#0,;
           Coddep      : H= 'UniEje':V=Val_parA(CodDep,'CODDEP','coddep'):f :W=(alltrim(m.TipFun)='I') and valpart#0,;
           Metas       : H= 'Metas F¡sicas' :W=(alltrim(m.TipFun)='I') and valpart#0
*:W=EMPTY(Codpart)
 ENDCASE

 if lastkey()=27
    vSun=.f.
 endif

 SELECT Itepar
 SEEK m.periodo+alltrim(m.codprg)+alltrim(m.codfte)
 SCAN WHILE m.Periodo = Periodo and alltrim(m.CodPrg) = CodPrg and alltrim(m.CodFte)=Codfte
    IF F_LOCK(1)
      IF EMPTY(CodPrg) OR empty(CodFte) OR Valpart=0
         DELETE NEXT 1
      ENDIF
    ENDIF
 ENDSCAN

 UNLOCK ALL
 SET FILTER TO
 SET ORDER TO 1
 ON KEY LABEL F5
 ON KEY LABEL F8
 ON KEY LABEL F10

 ACTIVATE SCREEN
 SHOW MENU mMenu
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SELECT Presu
 RETURN vSun


PROCEDURE Elimi
*---------------
 SELECT Presu
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF !Estado # '  00'
    * ya pas¢
    DO STANDBY WITH Vmens10
    RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR ‚ste Presupuesto ?')
 IF vElimina .AND.  F_Lock(1)
    dele next 1
    SELECT ItePar
    seek m.periodo+alltrim(m.codprg)+alltrim(m.codfte)
    IF FOUND()
       SCAN WHILE m.Periodo=ItePar.Periodo .AND. ALLTRIM(m.codprg)=Itepar.CodPrg .AND.;
            alltrim(m.codfte)=ItePar.CodFte
            IF f_lock(1)
               DELETE NEXT 1
            ENDIF
       ENDSCAN
    ENDIF
    SELECT Presu
    IF !BOF()
       SKIP -1
    ELSE
       IF !EOF()
          SKIP
       ENDIF
    ENDIF
 ENDIF
 UNLOCK all

 DO Vista
 RETURN

PROCEDURE CHEQUEO
*----------------
IF valpart=0
   IF ALLTRIM(m.tipfun)='I'
      vAcPyA=itepar.Periodo+itepar.CodPrg+itepar.Tipfun+itepar.Codpart+itepar.CodSubPr+Itepar.CodProy+Itepar.CodSuPRy
      ord=order()
      VC  = RECNO()
      set orde to 9
      SEEK vAcPyA
      IF FOUND() AND VALPART#0 AND ALLTRIM(M.CODFTE)#ALLTRIM(ITEPAR.CODFTE)
         vDessupry = itepar.Dessupry
         vUbicac   = itepar.Ubicac
         vModeje   = itepar.Modeje
         vCoddep   = itepar.Coddep
         vMeta     = itepar.Metas
         DO STANDBY WITH 'Ya est  registrado este SubPrograma con la Fte: '+ VAL_PARA(itepar.codfte,'CODFTE','D')
         set orde to (ord)
         GO VC
         IF RLOCK()
            REPLACE Dessupry with vDessupry ,;
                    Ubicac   with vUbicac   ,;
                    Modeje   with vModeje   ,;
                    Coddep   with vCoddep   ,;
                    Metas    with vmeta
         endif
         return .t.
      ENDIF
     set orde to (ord)
     GO VC
  endif
*o top
DO CASE
   CASE IIF(ALLTRIM(m.tipfun)='F',empty(Codact),empty(Codproy))
        vAcPy=itepar.Periodo+itepar.CodPrg+itepar.CodFte+itepar.Tipfun+itepar.Codpart+itepar.CodSubPr
        SEEK vacpy
        IF FOUND() AND VALPART#0
           DO STANDBY WITH 'Ya est  registrado este SubPrograma'
           return .f.
        ENDIF
   CASE IIF(ALLTRIM(m.tipfun)='F',!Empty(Codact),!empty(Codproy)) AND IIF(ALLTRIM(m.tipfun)='F',.T.,EMPTY(CODSUPRY))
        vAcPy=itepar.Periodo+itepar.CodPrg+itepar.CodFte+itepar.Tipfun+itepar.Codpart+itepar.CodSubPr+IIF(alltrim(m.TipFun)='I',Itepar.CodProy,Itepar.CodAct)
        seek vAcPy
        IF FOUND() AND VALPART#0
           DO STANDBY WITH 'Ya est  registrado est'+IIF(alltrim(m.TipFun)='F','a Actividad ','e Proyecto ')
           return .f.
        ENDIF
   CASE IIF(ALLTRIM(m.tipfun)='I',!empty(CodSuPRy),.t.)
        vAcPy=itepar.Periodo+itepar.CodPrg+itepar.CodFte+itepar.Tipfun+itepar.Codpart+itepar.CodSubPr+Itepar.CodProy+Itepar.CodSuPRy
        seek vAcPy
        IF FOUND() AND VALPART#0
           DO STANDBY WITH 'Ya est  registrado este Subproyecto'
           return .f.
        ENDIF
ENDCASE
endif
RETURN .T.

PROCEDURE Agreg_Item
*-----------------
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          CodPrg  WITH alltrim(m.CodPrg) ,;
          CodFte  WITH alltrim(m.CodFte) ,;
          TipFun  WITH alltrim(m.TipFun)
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
if rlock()
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif
return

PROCEDURE Corri_Item
*-----------------
REPLACE CodCal  WITH m.CodCal
RETURN .T.

PROCEDURE Lista
*--------------
SELE REPO
ZAP
SELECT PRESU
ord   = order()
vtemp = recno()
vPeriodo = '  '
vCodPrg  = '  '
vTipFun  = ' '
vCodFte  = '   '
vCodSubPr='   '
VCodAct ='  '
VCODPROY ='   '
DEFINE WINDOW LIS_1 FROM  8,13 TO 17,67 DOUBLE ;
TITLE ' °° Reporte Presupuesto Anual °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW LIS_1
@  1,2 SAY '  Periodo : ' GET vPeriodo   PICTURE '!!'
@  3,2 Say ' Programa : ' GET vCodPrg    PICTURE '!!'  valid Val_Para(vCodPrg,'CODPRG',' ',15,30)
@  5,2 Say '    Nivel : ' GET vTipFun    PICTURE '!'   VALID VAL_PARA(vTipFun,'TIPFUN',' ',15,30)
READ

*IF vTipFun = 'F'
*   @  7,2 Say '   Fuente : ' GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',15,30)
*   READ
*ENDIF
DEACTIVATE WINDOW LIS_1

IF LASTKEY() = 27
   DO VISTA
   RETURN
ENDIF

SELE ITEPAR

IF EOF()
   do standby with vmens08
ELSE
   IF vTipFun = 'F'

      SET FILTER TO  PERIODO=ALLTRIM(PERIODO) AND TIPFUN=ALLTRIM(VTIPFUN) and CodPrg=ALLTRIM(vCodPrg) &&AND CodSubpr=ALLTRIM(vCodSubpr) and CodAct=ALLTRIM(vCodAct)
      SCAN
         SCATTER MEMVAR
         SELE REPO
         vCod='FTE'+m.CodFte
         m.&vCod=m.ValPart
         APPEND BLANK
         GATHER MEMVAR
         M.&vCod=0
         SELE ITEPAR
      ENDSCAN
      SELE REPO

      DO CASE
         CASE alltrim(codprg)='01'
        
               do reporte with 2,"LisPre01",' Presupuesto1 '
         CASE alltrim(codprg)='02'
        
               do reporte with 2,"LisPre02",' Presupuesto2 '
         CASE alltrim(codprg)='03'
        
               do reporte with 2,"LisPre03",' Presupuesto3 '
         CASE alltrim(codprg)='04'
               do reporte with 2,"LisPre04",' Presupuesto4 '
         CASE alltrim(codprg)='05'
               do reporte with 2,"LisPre05",' Presupuesto5 '
         CASE alltrim(codprg)='06'
         
               do reporte with 2,"LisPre06",' Presupuesto6 '
         CASE alltrim(codprg)='07'
               do reporte with 2,"LisPre07",' Presupuesto7 '
         CASE alltrim(codprg)='08'
               do reporte with 2,"LisPre08",' Presupuesto8 '
      ENDCASE
   ELSE
      SELE REPO
      vInd = SYS(3) + '.IDX'
      INDEX ON Periodo+CodPrg+CodSubPr+CodProy+CodSupry+Codpart TO (vInd)
      SELE ITEPAR
      SET FILTER TO PERIODO=ALLTRIM(vPeriodo) AND TIPFUN=ALLTRIM(VTIPFUN) and CodPrg=ALLTRIM(vCodPrg) AND CodSubpr=ALLTRIM(vCodSubpr) and CodProy=ALLTRIM(vCodProy)
      
      GO TOP
      SCAN
        SCATTER MEMVAR
        vRep2 = m.Periodo+m.CodPrg+m.CodSubPr+m.CodProy+m.CodSupry+m.Codpart
        SELECT REPO
        SEEK vRep2
        if !found()
           append blank
           vCod='FTE'+m.CodFte
           m.&vCod=m.ValPart
           Gather Memvar
        else
           vCod='FTE'+m.CodFte
           if rlock()
              replace &vCod with m.ValPart
           endif
        endif
        SELECT itepar
      ENDSCAN
      SELE REPO
      SET INDEX TO (VIND)
      
      DO REPORTE WITH 2,"LisPre2",' Presupuesto2 '
   ENDIF
ENDIF
close data


USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE Maepar   IN 2   order tag Maepar1  ALIAS Presu
USE Itepar   IN 3   order tag Itepar1  ALIAS Itepar
USE Clase    IN 4   order tag clase1   ALIAS clase
USE Maepre   IN 5   order tag Maepre1  ALIAS Maepre
USE RepoPre  IN 6                      ALIAS Repo   EXCLUSIVE
SELECT Presu
SET ORDE TO 1
GO vTemp
DO VISTA

RETURN



PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
 CLOSE DATA
 RELEASE WINDOW wind_0
 RELEASE WINDOW wind_1
 RELEASE WINDOW wind_2
 RELEASE WINDOW wind_3
 RELEASE MENU   mMenu
 RESTORE SCREEN FROM PRINCIPAL
 RETURN


function valpec
*-----------------
parameter vnumpec
private vfun
vfun = .t.
m.numpec=padl(alltrim(str(vnumpec,4)),4,'0')
if m.numpec = '0000' or empty(m.numpec)
   vfun = .f.
endif
return vfun

FUNCTION VALART
*--------------
PARAMETERS _Cod
PRIVATE XX,vFun
vFun = .F.

XX = Val_ArtDet(_Cod,.F.)
IF XX
  SELECT Presu
  REPLACE ;
          coduni  WITH Iteart.coduni,;
          preuni  WITH Iteart.preuni,;
          descri  WITH Iteart.descri
  vFun = .T.
ENDIF
RETURN vFun
*****

FUNCTION Val_Cale1
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
  ENDCASE
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor

  IF (!FOUND() OR EMPTY(mValor)) AND !EMPTY(Itepar.CodPart)
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      SET FILTER TO Tipo+Codigo = Filtro
      GO TOP
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CODIGOAUX)+' '+SUBSTR(Descri,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = Parma.CodigoAux
 mDescr = SUBSTR( Parma.Descri, 1, mLong )
 SET ORDE TO 1
 IF !EMPTY( mAlias )
   SELECT (mAlias)
 ENDIF
 REPLACE &mVariable WITH mValor
 RETURN .T.
*-----------

FUNCTION VALIDA
*--------------
AS=ALIAS()
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT MAEPRE
SET ORDE TO IIF(alltrim(m.TipFun)='I',2,3)
SET FILTER TO Periodo=alltrim(m.periodo) and Codprg=alltrim(m.codprg) and CodsubPr=alltrim(Itepar.CodSubPr)
GO TOP
IF EOF()
  *DO STANDBY WITH 'No se registra ningun'+IIF(alltrim(m.TipFun)='F','a Actividad ',' Proyecto ')
   SELECT (as)
   RETURN .F.
ENDIF
xKey = alltrim(m.periodo)+alltrim(m.codprg)+alltrim(Itepar.CodSubPr)+IIF(alltrim(m.TipFun)='F',ALLTRIM(Itepar.CodAct),ALLTRIM(Itepar.CodProy))
SEEK xkey

IF !FOUND() OR (IIF(alltrim(m.TipFun)='F',EMPTY(Itepar.CodAct),EMPTY(Itepar.CodProy)))
   GO TOP
   DO CASE
      CASE alltrim(m.TipFun)='F'
           BROWSE ;
              NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+alltrim(m.codprg)+Itepar.CodSubPr ;
              WINDOW Wind_3 ;
                     FIELDS;
                     CodAct    : H= 'Act',;
                     Descri    : H= 'Descripci¢n'
      CASE alltrim(m.TipFun)='I'
           BROWSE ;
              NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH KEY m.periodo+alltrim(m.codprg)+Itepar.CodSubPr ;
              WINDOW Wind_3 ;
                     FIELDS;
                     CodProy   : H= 'Pry',;
                     Descri    : H= 'Descripci¢n'
   ENDCASE
   SELECT ItePar
   IF RLOCK()
      DO CASE
         CASE alltrim(m.TipFun)='F'
              REPLACE ItePar.CODACT  WITH MAEPRE.CODACT
         CASE alltrim(m.TipFun)='I'
              REPLACE ItePar.CODPROY WITH MAEPRE.CODPROY
      ENDCASE
      REPLACE ItePar.Valpart WITH 0
   ENDIF
ENDIF
UNLOCK ALL
SET FILT TO
SELECT (AS)
RETURN .T.

PROCEDURE BUSCPRG
*----------------
**BUSCAR ACTIVIDAD
Private vKey
vKey=ALLTRIM(REPO.Periodo)+ALLTRIM(REPO.Codprg)+ALLTRIM(REPO.CodSubPr)+ALLTRIM(REPO.CodProy)
SELE MAEPRE
SET ORDE TO 2
seek vkey
vProyecto = maepre.Descri
SET ORDE TO 1
sele REPO
return (vProyecto)

FUNCTION SUMPRG_C
*-------------
vKey = CodPrg
vRec = RECNO()
go top
sum FTECAN to suma1 for CodPrg = vKey
go vRec
RETURN SUMA1

FUNCTION SUMPRG_T
*-------------
vKey = CodPrg
vRec = RECNO()
go top
sum FTETES to suma2 for CodPrg = vKey
go vRec
RETURN SUMA2


*------------------------
FUNCTION SUMPRY_T
*---------------
vKey = CodPrg + CodSubPr + CodProy
vRec = RECNO()
go top
sum FTETES to suma2 for CodPrg+CodSubPr+CodProy  = vKey
go vRec
RETURN SUMA2

FUNCTION SUMPRY_C
*---------------
vKey = CodPrg + CodSubPr + CodProy
vRec = RECNO()
go top
sum FTECAN to suma1 for CodPrg+CodSubPr+CodProy  = vKey
go vRec
RETURN SUMA1


FUNCTION SUMSPRG_C
*---------------
vKey = CodPrg + CodSubPr
vRec = RECNO()
go top
sum FTECAN to suma1 for CodPrg+CodSubPr = vKey
go vRec
RETURN SUMA1

FUNCTION SUMSPRG_T
*---------------
vKey = CodPrg + CodSubPr
vRec = RECNO()
go top
sum FTETES to suma2 for CodPrg+CodSubPr = vKey
go vRec
RETURN SUMA2



FUNCTION TOTALIZ
*---------------
vKey = CodPrg + CodSubPr + CodProy
vRec = RECNO()
go top
sum FTECAN to suma1 for CodPrg+CodSubPr+CodProy  = vKey
GO TOP
sum FTETES to suma2 for CodPrg+CodSubPr+CodProy  = vKey
go vRec
RETURN SUMA1+SUMA2

FUNCTION TOTALSUB
*---------------
vKey = CodPrg + CodSubPr
vRec = RECNO()
go top
sum FTECAN to suma1 for CodPrg+CodSubPr  = vKey
GO TOP
sum FTETES to suma2 for CodPrg+CodSubPr  = vKey
go vRec
RETURN SUMA1+SUMA2

FUNCTION TOTALPRG
*---------------
vKey = CodPrg
vRec = RECNO()
go top
sum FTECAN to suma1 for CodPrg  = vKey
GO TOP
sum FTETES to suma2 for CodPrg  = vKey
go vRec
RETURN SUMA1+SUMA2

