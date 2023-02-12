*-------------------------------------------------------------------------
* RegCal.Prg
* Registra Calendarios
*--------------------------------------------------------------------------
*- Abriendo Archivos
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE calen    IN 2   order tag calen3   ALIAS calen
USE Maepre   IN 3   order tag Maepre1  ALIAS Maepre
USE Maepar   IN 4   order tag Maepar1  ALIAS Presu
USE Itepar   IN 5   order tag Itepar2  ALIAS Itepar
USE Clase    IN 6   order tag clase1   ALIAS clase
USE RepoCal  IN 7                      aLIAS Repo   EXCLUSIVE
USE RepCal1  IN 8                      aLIAS Rep1   EXCLUSIVE

SELE REPO
ZAP
SELE REP1
ZAP
*- Mensajes de aviso al usuario
Vmens01 = 'Registro de Calendario'
Vmens02 = ' Calendario : REVISION '
Vmens04 = 'Dicho Calendario no fue encontrado'
Vmens05 = 'No existe Calendario anterior'
Vmens06 = 'No existe Calendario siguiente'
Vmens07 = '¨ Desea Anular ‚ste Calendario ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Calendario ha sido anulado'
Vmens10 = 'El Calendario ya est  Atendido'
Vmens11 = 'El Calendario ha sido devuelto'

SELECT Calen
GO BOTTOM

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
 TITLE 'Detalle: Calendario' ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 05,25 TO 15,78 DOUBLE ;
 TITLE '² ®F10¯ Escoge ²' ;
 COLOR SCHEME 10

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
 ACTIVATE WINDOW Wind_0
 CLEAR

 @  1, 2 SAY "              Mes :"
 @  1,40 SAY "          Periodo :"
 @  2, 2 SAY "          Volumen :"
 @  3, 2 SAY "           Sector :"
 @  4, 2 SAY "           Pliego :"

 @  6, 2 SAY " F.Financiamiento :"
 @  7, 2 SAY "         Programa :"
 @  8, 2 SAY "      SubPrograma :"
 @  9, 2 SAY "     Tipo Funci¢n :"

 @ 19, 2 SAY "          Partida :"
 @ 20, 2 SAY "       Asignaci¢n :"

RETURN

PROCEDURE Vista     && Coloca valores de BD en variables y pinta datos
*--------------
SELECT Calen
IF EOF()
   DO Pantalla
   RETURN
ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 @  0,60 SAY IIF( m.Estado= '00','Emitido  ',IIF( m.Estado = '20','         ',IIF(m.Estado='99','Anulado  ',IIF(m.Estado='50','Atendido ',' ------- '))))
 @  1,22 SAY Val_Para(m.NUmMes  ,'FECMES','V',26,20)
 @  1,60 SAY m.Periodo
 @  2,22 SAY '02    GOBIERNOS REGIONALES'
 @  3,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
 @  4,22 SAY '001   REGION GRAU'
 @  6,22 SAY Val_Para(m.CodFte  ,'CODFTE','V',26,40)
 @  7,22 SAY Val_Para(m.CodPrg  ,'CODPRG','V',26,40)
 @  8,22 SAY Val_Cale(m.CodSubPr,'CODSUB'+m.CodPrg,'V',22,40)
 @  9,22 SAY Val_Para(m.TipFun  ,'TIPFUN','V',22,15)
 @ 11,01 CLEAR TO 13,78
 IF alltrim(m.TipFun)='F'
    if !empty(m.codact)
       @ 11, 2 SAY "        Actividad : "
       @ 11,22 SAY m.CodAct
       @ 12, 2 SAY "                           "
       @ 13, 2 SAY "                           "
    endif
 ELSE
  IF !EMPTY(M.CodProy)
     public vProyecto
     =BuscPrg()
     @ 11, 2 SAY "         Proyecto : "
     @ 12, 2 SAY "      Subproyecto : "
     @ 11,22 SAY m.CodProy
     @ 11,28 SAY LEFT(vProyecto,50)
     @ 12,22 SAY m.CodsuPry
     @ 12,28 SAY m.DessuPry
     IF !EMPTY(m.CodObra)
        @ 13, 2 SAY "             Obra : "
        @ 13,22 SAY m.CodObra
     ENDIF
  ENDIF
 ENDIF

 @ 15,2  clear to 18,60

 IF !EMPTY(m.Descri)
    @ 15, 2 SAY "      Descripci¢n :"
    @ 15,22 SAY SUBSTR(m.Descri,1,55)
    @ 16,22 SAY SUBSTR(m.Descri,56,55)
 ENDIF
 if alltrim(m.TipFun)='I'
    @ 18, 2 SAY "        Ubicaci¢n :"
    @ 18,22 SAY m.Ubicac
 endi
 @ 19,22 SAY m.CodPart
 @ 19,28 SAY VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),'D',28,50)
 @ 20,22 SAY m.ValPart picture '999,999,999.99'

 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE CALEN
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
 Codpart :H='Part.' ,;
 XX = VAL_PARA( NumMes ,'FECMES','D',2,3) :H='MES' ,;
 tipfun  :H='Tp' ,;
 codfte  :H='F Fto',;
 codprg  :H='Prg',;
 codsubpr :H='SubPrg',;
 codact  :H='Act',;
 codproy :H='Proy',;
 codsupry:H='Spry',;
 codobra :H='Obra',;
 descri :H='Obs'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
*Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE CALEN
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
as=order()
vNum_MES = 0
vNumMes  = '  '
vCodFte  = '   '
vTipFun  = ' '
vCodPrg  = '  '
vCodSubPr= '   '

 DEFINE WINDOW Lis FROM 11,12 TO 20,68 DOUBLE ;
 TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lis


 @  1,2 say ' Periodo : ' GET vPeriodo   PICTURE '!!'
 @  2,2 say '     Mes : ' GET vNumMes    PICTURE '!!'  VALID Val_Para(VNUmMes  ,'FECMES',' ',14,30)
 @  3,2 say '  Fuente : ' GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',14,30)
 @  4,2 say 'Programa : ' GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',14,30)
 @  5,2 say 'SubProg. : ' GET vCodSubPr  PICTURE '!!!' VALID VAL_CALE(vCodSubPr,'CODSUB'+VCodPrg,' ',14,30)
 @  6,2 Say '   Nivel : ' GET vTipFun    PICTURE '!'   VALID VAL_PARA(vTipFun,'TIPFUN',' ',14,30)

 READ

DEACTIVATE WINDOW lis

 IF EMPTY(vNumMES) .or. LASTKEY()=27
    RETURN
 ELSE
    set order to calen4
    SEEK vPeriodo+ALLTRIM(vNumMES)+ALLTRIM(vCodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSubpr)+ALLTRIM(vTipFun)
    IF !FOUND()
       DO standby WITH Vmens04
       set order to (as)
       GOTO Vtemp
     ELSE
       set order to (as)
       DO Vista
     ENDIF
 ENDIF


PROCEDURE vBusca
*---------------
vnum_pec=padl(alltrim(str(vnum_pec,4)),4,'0')
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
   * El Calendario ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 IF Estado = '50'

   DO STANDBY WITH Vmens12
   RETURN
 ENDIF
 *****
 SELECT Calen
 SCATTER MEMVAR
 vDescri1 = SUBSTR(m.Descri,1,55)
 vDescri2 = SUBSTR(m.Descri,56,55)
 ACTIVATE WINDOW Wind_0
 vTemp=recno()
 @  1,22 GET m.NumMes    PICTURE '!!'  valid Val_Para(m.NUmMes  ,'FECMES',' ',22,20)
 @  1,60 GET m.Periodo   PICTURE '!!'
 @  2,22 SAY '02    GOBIERNOS REGIONALES'
 @  3,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
 @  4,22 SAY '001   REGION GRAU'
 @  6,22 GET m.CodFte    PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,40)   DISABLE
 @  7,22 GET m.CodPrg    PICTURE '!!'  VALID VAL_PARA(m.CodPrg,'CODPRG',' ',22,40)   DISABLE
 @  8,22 GET m.CodSubPr  PICTURE '!!!' VALID VAL_CALE(m.CodSubPr,'CODSUB'+m.CodPrg,' ',22,40)  DISABLE
 @  9,22 GET m.tipfun    PICTURE '!'   VALID VAL_PARA(m.tipfun,'TIPFUN',' ',22,15)  DISABLE
 READ
 vRem=Recno()
 IF m.TipFun='F'
    @ 11, 2 SAY "        Actividad :"
    @ 11,22 GET m.CodAct   PICTURE '!!'  DISABLE
    @ 12, 2 SAY "                   "
    @ 13, 2 SAY "                   "
 ELSE
    @ 11, 2 SAY "         Proyecto :"
    @ 12, 2 SAY "      Subproyecto :"
    @ 13, 2 SAY "             Obra :"
    @ 11,22 GET m.CodProy  PICTURE '!!!' VALID VALSUB()
    @ 12,22 GET m.CodSuPry PICTURE '!!'  WHEN  vOkey
    @ 13,22 GET m.CodObra  PICTURE '!!'  DISABLE
 ENDIF
 go vRem
 @ 15,22 GET vDescri1 WHEN !EMPTY(m.CodObra)
 @ 16,22 GET vDescri2 WHEN !EMPTY(m.CodObra)
 @ 18, 2 clear to 18,79

 if alltrim(m.TipFun)='I'
    @ 18, 2 SAY "        Ubicaci¢n :"
    @ 18,22 GET m.Ubicac
 endif
 @ 19,22 GET m.codpart  picture '!!.!!' VALID VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),' ',22,50)
 @ 20,22 GET m.valpart  picture '99,999,999.99'
 READ VALID VAL_READ()

 IF LASTKEY() # 27
    m.DesCri = vDescri1+vDescri2
    DO WHILE .T.
       Ok = .F.
       IF LASTKEY() # 27
          ok = .T.
          EXIT
       ELSE
          DO STANDBY WITH ' Cancelado la modificaci¢n..'
          ok = .F.
          EXIT
       ENDIF
    ENDDO

    IF Ok .AND. LASTKEY() # 27
       IF m.TipFun='F'
          m.Codproy ='   '
          m.CodSuPry='  '
          m.CodObra ='  '
       ELSE
          m.CodAct  ='   '
       ENDIF
       go vTemp
       SELECT Calen
       GATHER MEMVAR
    ELSE
       *-??
    ENDIF
 ELSE
    *- No pudo agregar la liquidaci¢n
    SELECT Calen
    GO BOTTOM
 ENDIF
 UNLOCK ALL
 SELECT Calen
 DO PANTALLA
 DO Vista
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT parma
 SEEK 'CORRELCODOBR'
 SELECT Calen
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.periodo = str(year(DATE())-1900,2)
 m.codproy='   '
 @  1,22 GET m.NumMes    PICTURE '!!'  VALID Val_Para(m.NUmMes  ,'FECMES',' ',22,20)
 @  1,60 GET m.Periodo   PICTURE '!!'
 @  2,22 SAY '02    GOBIERNOS REGIONALES'
 @  3,22 SAY '01    PRESIDENCIA DEL CONSEJO DE MINISTROS'
 @  4,22 SAY '001   REGION GRAU'
 @  6,22 GET m.CodFte    PICTURE '!!!' VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,45)
 @  7,22 GET m.CodPrg    PICTURE '!!'  VALID VAL_PARA(m.CodPrg,'CODPRG',' ',22,40)
 @  8,22 GET m.CodSubPr  PICTURE '!!!' VALID VAL_CALE(m.CodSubPr,'CODSUB'+m.CodPrg,' ',22,40)
 @  9,22 GET m.tipfun    PICTURE '!'   VALID VAL_PARA(m.tipfun,'TIPFUN',' ',22,15)
 READ

 IF LASTKEY()=27
    SELECT CALEN
    DO VISTA
    RETURN
 ENDIF

 *---VALIDA SI TIENE ACTIVIDAD / PROYECTO
 AS=VALIDA()
 *----------
 DO CASE
    CASE !AS  &&  NO TIENE ACTIVIDAD
         AD=alias()
         SELECT ITEPAR
         SET ORDE TO ITEPAR7
         SET FILT TO Periodo=ALLTRIM(m.Periodo) and CodPrg=ALLTRIM(m.Codprg) and CodFte=ALLTRIM(m.CodFte) AND TipFun=ALLTRIM(m.TipFun) ;
                     and SUBSTR(CodPart,4,2)='00' and CodSubPr=ALLTRIM(m.CodSubPr)
         GO TOP
         vkey = ALLTRIM(m.Periodo)+ALLTRIM(m.Codprg)+ALLTRIM(m.CodFte)+ALLTRIM(m.TipFun)+ALLTRIM(m.CodSubPr)
         SEEK vkey

         IF FOUND()
            BROWSE ;
            NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH ;
            WINDOW Wind_3 ;
                   FIELDS;
                   CodPart   : H= 'Partida',;
                   Valpart   : H= 'Asignaci¢n' :P='99,999,999.99'
            m.Codpart   = Itepar.Codpart
            m.Valpart   = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
            m.Valpre    = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
         ELSE
            DO STANDBY WITH 'La Actividad NO tiene Presupuesto..Revise'
            vOkey=.f.
      *     SELECT (AD)     CUANDO NO ESTA REGISTRADO EN EL PRESUPUESTO
      *     DO VISTA
      *     RETURN
         ENDIF
         SET ORDE TO ITEPAR2
         SELECT (AD)
    CASE ALLTRIM(m.TipFun)='F' AND AS
         @ 11, 2 SAY "        Actividad :"
         @ 11,22 GET m.CodAct   PICTURE '!!' VALID VALACTV()
    CASE ALLTRIM(m.TipFun)='I' AND AS
         @ 11, 2 SAY "         Proyecto :"
         @ 12, 2 SAY "      Subproyecto :"
         @ 11,22 GET m.CodProy  PICTURE '!!!' VALID VALSUB()
         @ 12,22 GET m.CodSuPry PICTURE '!!'  WHEN vOkey
         READ
         IF LASTKEY()=27
            SELE CALEN
            DO VISTA
            RETURN
         ENDIF
         IF vOkey
            IF YESNO("Desea registrar una Obra?")
               SINO='S'
               SELECT PARMA
               SEEK 'CORRELCODOBR'
               =VALCAL ( Parma.NumEnt + 1,SINO )
               @ 13, 2 SAY "             Obra :"
               @ 13,22 GET m.CodObra  PICTURE '!!'
               m.DesCri = spac(60)
               @ 15,22 GET m.Descri
            ENDIF
         ENDIF
 ENDCASE
 @ 15,02 CLEAR TO 18,79
 IF alltrim(m.TipFun)='I'
    @ 18, 2 SAY "        Ubicaci¢n :"
    @ 18,22 GET m.Ubicac
 ENDIF
 @ 19,22 GET m.CodPart  picture '!!.!!' VALID VAL_PART(SUBSTR(m.CodPart,4,2),LEFT(m.CodPart,2),' ',22,50)
 @ 20,22 GET m.ValPart  picture '999,999,999.99'
 READ

 LLAVE = ALLTRIM(m.Periodo)+alltrim(m.nummes)+alltrim(m.CodPrg)+alltrim(m.CodFte)+alltrim(m.CodPart)+alltrim(m.CodSubpr)
 DO CASE
    CASE ALLTRIM(m.Tipfun)='I'
         IF !EMPTY(m.CodObra)
            LLAVE = ALLTRIM(m.Periodo)+alltrim(m.nummes)+alltrim(m.CodPrg)+alltrim(m.CodFte)+alltrim(m.CodPart)+alltrim(m.CodSubpr)+alltrim(m.CodProy)+ALLTRIM(CODSUPRY)+ALLTRIM(CODOBRA)
         ELSE
            IF !EMPTY(m.CodSuPry)
               LLAVE = ALLTRIM(m.Periodo)+alltrim(m.nummes)+alltrim(m.CodPrg)+alltrim(m.CodFte)+alltrim(m.CodPart)+alltrim(m.CodSubpr)+alltrim(m.CodProy)+ALLTRIM(CODSUPRY)
            ELSE
               IF !EMPTY(m.CodProy)
                  LLAVE = ALLTRIM(m.Periodo)+alltrim(m.nummes)+alltrim(m.CodPrg)+alltrim(m.CodFte)+alltrim(m.CodPart)+alltrim(m.CodSubpr)+alltrim(m.CodProy)
               ENDIF
            ENDIF
         ENDIF
    CASE ALLTRIM(m.Tipfun)='F'
         IF !EMPTY(m.CodAct)
            LLAVE = ALLTRIM(m.Periodo)+alltrim(m.nummes)+alltrim(m.CodPrg)+alltrim(m.CodFte)+alltrim(m.CodPart)+alltrim(m.CodSubpr)+alltrim(m.CodAct)
         ENDIF
 ENDCASE
 SELECT CALEN
 vX=Order()
 vTemp = Recno()
 SET ORDER TO IIF(alltrim(m.TipFun)='I',6,5)
 SEEK llave

 IF FOUND()
    DO STANDBY WITH 'Ya est  registrado '
    SET ORDER TO (vX)
    DO VISTA
    RETURN
 ENDIF
 SET ORDE TO (VX)
 go vTemp
 IF LASTKEY() # 27 AND YESNO('¨ Confirme el ingreso ?')
   m.Estado = '00'
   m.fecemi = date()
   IF !EMPTY(m.CodObra)
      SELECT Parma
      SEEK 'CORREL' + 'CODOBR'
      REPLACE NumEnt WITH NumEnt + 1
   ENDIF
   SELECT Calen
   IF F_Appd()
      GATHER MEMVAR
   ENDIF
ELSE
   *- No pudo agregar la liquidaci¢n
   DO STANDBY WITH ' Cancelado el Ingreso ..'
ENDIF
UNLOCK ALL
DO Vista
RETURN

FUNCTION VALIDA
*--------------
PUBLIC vOKEY,vPKEY
am=alias()
os=Order()
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT MAEPRE
SET FILTER TO IIF(alltrim(m.TipFun)='F',!EMPTY(CodAct),!EMPTY(Codproy))
vLla = ALLTRIM(m.Periodo)+alltrim(m.CodPrg)+alltrim(m.CodSubpr)
SET ORDER TO IIF(alltrim(m.TipFun)='I',2,3)
SEEK vLla
vfun=.t.
IF FOUND()
   DO CASE
      CASE alltrim(m.TipFun)='F'
           BROWSE ;
           NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH KEY vLla ;
           WINDOW Wind_3 ;
                     FIELDS;
                     CodAct    : H= 'Act',;
                     Descri    : H= 'Descripci¢n'
           M.CODACT =  MAEPRE.CODACT
           M.DESCRI =  MAEPRE.DESCRI

      CASE alltrim(m.TipFun)='I'
           BROWSE ;
           NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH KEY vLla ;
           WINDOW Wind_3 ;
                     FIELDS;
                     CodProy   : H= 'Pry',;
                     Descri    : H= 'Descripci¢n'
           M.CODPROY = MAEPRE.CODPROY
   ENDCASE
   vOkey=.t.
ELSE
   M.DESCRI =  VAL_CALE(m.CodSubPr,'CODSUB'+m.CodPrg,'D',22,40)
   vOkey=.f.
   vFun=.f.
endif
UNLOCK ALL
SET FILTER TO
sele (aM)
SET ORDER TO (os)
RETURN VFUN

PROCEDURE VALSUB  && Valida SubProgramas
*---------------
Private vKey
as=alias()
SELECT ITEPAR
SET ORDE TO ITEPAR2
ON KEY LABEL F10 KEYBOARD CHR(23)
SET FILT TO Periodo=ALLTRIM(m.Periodo) and CodFte=ALLTRIM(m.CodFte) and CodPrg=ALLTRIM(m.Codprg) and CodSubPr=ALLTRIM(m.CodSubPr) and CodProy=ALLTRIM(m.CodProy) ;
            AND !EMPTY(CodSuPry)
GO TOP
vOkey = .t.
IF EOF()
   m.CodSuPry  = '   '
   DO STANDBY WITH 'No se tiene registrado SubProyectos para este Proyecto '
   vOkey=.f.
   vKey=ALLTRIM(m.Periodo)+ALLTRIM(m.CodFte)+ALLTRIM(m.Codprg)+ALLTRIM(m.CodSubPr)+ALLTRIM(m.CodProy)
   SET FILTER TO EMPTY(CODSUPRY)
   SEEK vKey
   IF FOUND()
      m.Ubicac    = Itepar.Ubicac
      m.Codpart   = Itepar.Codpart
      m.Valpart   = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
      m.Valpre    = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
      m.DESCRI    = BUSCPRG()
   ELSE
      DO STANDBY WITH 'No se tiene Presupuesto para este Proyecto '
   ENDIF
   SET FILTER TO
   SELECT (As)

   return
ENDIF

vKey=ALLTRIM(m.Periodo)+ALLTRIM(m.CodFte)+ALLTRIM(m.Codprg)+ALLTRIM(m.CodSubPr)+ALLTRIM(m.CodProy)+ALLTRIM(m.CodSuPry)
SEEK vKey

IF !EOF() OR !FOUND()
   BROWSE ;
   NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH  ;
   WINDOW Wind_3 ;
          FIELDS;
          CodSuPry : H= 'Pry',;
          DesSuPry : H= 'Descripci¢n'
   m.CodSuPry  = Itepar.CodsuPry
   m.DesSuPry  = Itepar.DesSuPry
   m.Descri    = Itepar.DesSuPry
   m.Ubicac    = Itepar.Ubicac
   m.Codpart   = Itepar.Codpart
   m.Valpart   = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
   m.Valpre    = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
   vOkey=.t.
ENDIF
SET FILTER TO
SELECT (As)
return


PROCEDURE VALACTV  && Valida Actividades
*----------------
Private vKey
aD=alias()
SELECT ITEPAR
SET ORDE TO ITEPAR7
SET FILT TO Periodo=ALLTRIM(m.Periodo) and CodPrg=ALLTRIM(m.Codprg) and CodFte=ALLTRIM(m.CodFte) AND TipFun=ALLTRIM(m.TipFun) ;
            and SUBSTR(CodPart,4,2)='00' and CodSubPr=ALLTRIM(m.CodSubPr) ;
            and CodAct=ALLTRIM(m.CodAct) && and EMPTY(m.Codact)
GO TOP
vkey = ALLTRIM(m.Periodo)+ALLTRIM(m.Codprg)+ALLTRIM(m.CodFte)+ALLTRIM(m.TipFun)+ALLTRIM(m.CodSubPr)+ALLTRIM(m.CodAct)
SEEK vkey
IF FOUND()
   IF EMPTY(m.Codact)
      BROWSE ;
      NOAPPEND NODELETE NOMENU NOEDIT NOCLEAR NOOPTIMIZE NOREFRESH  ;
      WINDOW Wind_3 ;
             FIELDS;
             CodPart  : H= 'Part.' ,;
             ValPart  : H= 'Ejecutado' :P='99,999,999.99'
   ENDIF
   m.Codpart   = Itepar.Codpart
   m.Valpart   = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
   m.Valpre    = Itepar.Valpart +Itepar.Cresup - Itepar.CanTeje
   vOkey=.t.
ELSE
   m.Codact  = '  '
   DO STANDBY WITH 'La Actividad NO tiene Presupuesto..Revise'
   vOkey=.f.
ENDIF
SET ORDE TO ITEPAR2
SELECT (AD)
RETURN

PROCEDURE Elimi
*---------------
 SELECT Calen
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF !Estado # '  00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR ‚ste Calendario ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   dele next 1
   SKIP
   DO Vista
 ENDIF
 UNLOCK all
 RETURN

PROCEDURE Agreg_Item
*-----------------
IF F_appd()
  REPLACE Periodo WITH m.Periodo ,;
          NumPec  WITH m.NumPec ,;
          Estado  WITH m.Estado ,;
          CodCal  WITH m.CodCal ,;
          Abopar  WITH 'N'
 *KEYBOARD CHR(15)
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Elimi_Item
*-------------------
SELECT Itepar
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
SELE REP1
ZAP
select CALEN
vtemp =recno()
if YESNO (" Marco Presupuestal ?")
 *------Listado Marco Presupuestal----

   vPeriodo = RIGHT(STR(YEAR(DATE()),4),2)
   vNumMes  = '  '
   vCodFte  = '   '
   vTipFun  = ' '
   vCodPrg  = '  '
   vCodPart = '  .  '

   DEFINE WINDOW LIS_1 FROM 08,13 TO 15,67 DOUBLE ;
   TITLE ' °° Calendario Marco Presupuestal °° ' FLOAT COLOR SCHEME 5

   ACTIVATE WINDOW LIS_1

   @  1,2 SAY '  Periodo : ' GET vPeriodo   PICTURE '!!'
   @  2,2 Say '  Fte Fin : ' GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',15,30)
   @  3,2 Say '  Programa: ' GET vCodPrg    PICTURE '!!'  VALID VAL_PARA(m.CodPrg,'CODPRG',' ',22,40)

   READ

   DEACTIVATE WINDOW LIS_1
   if lastkey()= 27
       do vista
       return
   endif

   if eof()
       do standby with vmens08
       return
   Else
		
QQ
          SELE REP1
          vInd = SYS(3) + '.IDX'
          INDEX ON Periodo+CodFte+CodPrg+CodSubPr+CodProy+CodSuPry TO (vInd)
          SELE CALEN
          SET FILTER TO CODFTE=ALLTRIM(vCODFTE)  AND PERIODO=ALLTRIM(vPERIODO) AND CODPRG=ALLTRIM(vCODPRG)
          GO TOP
          SCAN
            SCATTER MEMVAR
            vRep2 = m.Periodo+m.CodFte+m.CodPrg+m.CodSubPr+m.CodProy+m.CodSupry
            SELECT REP1
            SEEK alltrim(vRep2)
            if !found()
               append blank
               vCod='M_'+ALLTRIM(m.NumMes)
               m.&vCod=m.ValPart
               Gather Memvar
               vKey=Alltrim(m.Periodo)+Alltrim(m.CodFte)+Alltrim(m.CodPrg)+Alltrim(m.CodSubpr)+Alltrim(m.CodProy)+Alltrim(m.CodSupry)
               SELE ITEPAR
               go top
               SEEK vKey
               if Found()
                   SELE REP1
                   replace Valpres with Itepar.Valpart
                 * RETURN
               ENDIF
           else
 			
              vCod='M_'+ALLTRIM(m.NumMes)
              if rlock()
              replace &vCod with m.ValPart
           endif

        endif
        SELECT CALEN
        ENDSCAN


        SELE REP1
         SET INDEX TO (VIND)
        do reporte with 2,"LisCal3",' A nivel de Marco Presupuestal '
        close data
        erase (vInd)
        USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
        USE calen    IN 2   order tag calen3   ALIAS calen
        USE Maepre   IN 3   order tag Maepre1  ALIAS Maepre
        USE Maepar   IN 4   order tag Maepar1  ALIAS Presu
        USE Itepar   IN 5   order tag Itepar2  ALIAS Itepar
        USE Clase    IN 6   order tag clase1   ALIAS clase
        USE RepoCal  IN 7                      aLIAS Repo   EXCLUSIVE
        USE RepCal1  IN 8                      aLIAS Rep1   EXCLUSIVE


   ENDIF
   RETURN
ELSE

   vPeriodo = RIGHT(STR(YEAR(DATE()),4),2)
   vNumMes  = '  '
   vCodFte  = '   '
   vTipFun  = ' '

   DEFINE WINDOW LIS_1 FROM 08,13 TO 15,67 DOUBLE ;
   TITLE ' °° Listado Calendario °° ' FLOAT COLOR SCHEME 5

   ACTIVATE WINDOW LIS_1

   @  1,2 SAY '  Periodo : ' GET vPeriodo   PICTURE '!!'
   @  2,2 Say '  Fte Fin : ' GET vCodFte    PICTURE '!!!' VALID VAL_PARA(vCodFte,'CODFTE',' ',15,30)
   @  3,2 Say '      Mes : ' GET vNumMes    PICTURE '!!'  valid Val_Para(vNUmMes,'FECMES',' ',15,30)
   @  4,2 Say '    Nivel : ' GET vTipFun    PICTURE '!'   VALID VAL_PARA(vTipFun,'TIPFUN',' ',15,30)
   READ

   DEACTIVATE WINDOW LIS_1

   if lastkey()= 27
       do vista
       return
   endif

   if eof()
       do standby with vmens08
       return
   Else
      IF vTipFun = 'F'
          SELE REPO
          vInd = SYS(3) + '.IDX'
          INDEX ON Periodo+Nummes+TipFun+CodFte+CodPrg+CodSubPr+CodAct TO (vInd)
          SELE CALEN
          SET FILTER TO CODFTE=ALLTRIM(vCODFTE) AND NUMMES=ALLTRIM(vNUMMES) AND PERIODO=ALLTRIM(PERIODO) AND TIPFUN=ALLTRIM(VTIPFUN)
          GO TOP
          SCAN
            SCATTER MEMVAR
            vRep2 = m.Periodo+m.Nummes+m.TipFun+m.CodFte+m.CodPrg+m.CodSubPr+m.CodAct
            SELECT REPO
            SEEK alltrim(vRep2)
            if !found()
               append blank
               vCod='C_'+LEFT(m.CodPart,2)
               m.&vCod=m.ValPart
               Gather Memvar
           else
              vCod='C_'+LEFT(m.CodPart,2)
              if rlock()
              replace &vCod with m.ValPart
           endif
        endif
        SELECT CALEN
        ENDSCAN
        SELE REPO
         SET INDEX TO (VIND)
        QQ
        do reporte with 2,"LisCal1A",' A nivel Funcionamiento '
        close data
        erase (vInd)
        USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
        USE calen    IN 2   order tag calen3   ALIAS calen
        USE Maepre   IN 3   order tag Maepre1  ALIAS Maepre
        USE Maepar   IN 4   order tag Maepar1  ALIAS Presu
        USE Itepar   IN 5   order tag Itepar2  ALIAS Itepar
        USE Clase    IN 6   order tag clase1   ALIAS clase
        USE RepoCal  IN 7                      aLIAS Repo   EXCLUSIVE
   ELSE
      SET ORDER TO CALEN7
      SET FILTER TO CODFTE=ALLTRIM(vCODFTE) AND NUMMES=ALLTRIM(vNUMMES) AND PERIODO=ALLTRIM(PERIODO) AND TIPFUN=ALLTRIM(VTIPFUN)
      do reporte with 2,"LisCal2",' A nivel de Proyectos de Inversi¢n '
   ENDIF
endif
ENDIF
SELE Calen
GO vtemp
do Vista

RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN


PROCEDURE Valcal
*-----------------
parameter vNumCal,vSino
private vfun
vfun = .t.
IF ALLTRIM(vSINO)='S'
   m.CodObra=padl(alltrim(str(vnumCAL,2)),2,'0')
   if m.CodObra = '00' or empty(m.CodObra)
      vfun = .f.
   endif
endif
return vfun

PROCEDURE BUSCPRG
*----------------
*BUSCAR ACTIVIDAD
 Private vKey
 vKey=ALLTRIM(m.Periodo)+ALLTRIM(m.Codprg)+ALLTRIM(m.CodSubPr)+ALLTRIM(m.CodProy)
 SELECT MAEPRE
 SET ORDER TO 2
 seek vkey
 vProyecto = maepre.Descri
 select calen
 return (vProyecto)


PROCEDURE BUSCPRO
*----------------
*BUSCAR ACTIVIDAD
 Private vKey
 vKey=ALLTRIM(Periodo)+ALLTRIM(Codprg)+ALLTRIM(CodSubPr)+ALLTRIM(CodProy)
 SELECT MAEPRE
 SET ORDER TO 2
 seek vkey
 vProyecto = maepre.Descri
 select calen
 return (vProyecto)

FUNCTION SUMAPRG
*---------------
 PARAMETER PART
 TOTACT = 0
 Prg = Repo.CodPRG
 vRec = RECNO()
 GO TOP
 SUM &PART TO TOTAL FOR Codprg = Prg
 GO vRec
 RETURN Total

FUNCTION SUMACT
*--------------
PARAMETER PART
Prg  = Repo.CodSubPr
vRec = RECNO()
GO TOP
SUM &PART TO TOTAL1 FOR CodSubPr = Prg and !empty(CodAct)
GO vRec
RETURN Total1


FUNCTION TOTACT
*--------------
total2 = c_01+c_02+c_03+c_04+c_05+c_06+c_07+c_08+c_09+c_10+c_11+c_12+c_14
RETURN Total2

FUNCTION TOTSUB
*--------------

SubPrg = Repo.CodSubPr
vRec = RECNO()
GO TOP
TOTACT0 = 0
FOR Q= 1 TO 14
    ZX = padl(alltrim(str(q,2)),2,'0')
    TOTACT0 = TOTACT0 + C_&ZX
    go top
endfor
GO vRec
RETURN Totact0


FUNCTION TOTACT1
*--------------
SubPrg = Repo.CodSubPr
vRec = RECNO()
GO TOP
TOTACT1= 0
FOR Q= 1 TO 14
    ZX = padl(alltrim(str(q,2)),2,'0')
    TOTACT1 = TOTACT1 + SUMACT('C_&ZX')
    go top
endfor
GO vRec
RETURN Totact1

FUNCTION SUMPRG
*-------------
vKey = CodPrg
vRec = RECNO()
go top
sum valpart to suma for CodPrg = vKey
go vRec
BROW
RETURN SUMA

FUNCTION SUMSPRG
*---------------
vKey = CodPrg + CodSubPr
vRec = RECNO()
go top
sum valpart to sumaQ for CodPrg+CodSubPr = vKey
go vRec
RETURN SUMAQ

FUNCTION TOTSUPRY
*--------------
totalM = M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12
RETURN TotalM

FUNCTION TOTSALSPRY
*--------------
PUBLIC Totals
totalS = VALPRES-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12)
RETURN TotalS

FUNCTION SUMSUPRY
*-----------------
PARAMETER PART
PRG =REP1.Codprg+REP1.CodSubpr+Rep1.CodProy+REP1.CodSupry
vRec = RECNO()
go top
sum &PART to SUMAM for REP1.Codprg+REP1.CodSubpr+Rep1.CodProy+REP1.CodSupry=PRG AND !EMPTY(CODPART)
go vRec
RETURN SUMAM

FUNCTION SUMSPYTOT
*---------------
vKey = CodPrg + CodSubPr+CodProy+CodSupry
vRec = RECNO()
go top
sum valpart to sumT for CodPrg+CodSubPr+CodProy+CodSupry = vKey
go vRec
RETURN SUMT

FUNCTION SUMSALSPRY
*---------------
vKey = CodPrg + CodSubPr+CodProy+CodSupry
vRec = RECNO()
go top
sum Totals to SALSPRY for CodPrg+CodSubPr+CodProy+CodSupry = vKey
go vRec
RETURN SALSPRY
***-------fin-------****
