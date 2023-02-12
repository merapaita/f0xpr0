* RegRI1.Prg
* Registra los Recibos de Ingreso
*--------------------------------------------------------------------------
*- Abriendo Archivos

USE Parmae   IN  1  ORDER TAG Parmae1  ALIAS Parma
USE Cajas    IN  2  ORDER TAG Cajas1   ALIAS Caja
USE RecIng   IN  3  ORDER TAG RecIng1  ALIAS RI
USE IteRI    IN  4  ORDER TAG IteRI1   ALIAS IteRI
USE Ingreso  IN  5  ORDER TAG Ingreso1 ALIAS IngR
USE AstPat   IN  6  ORDER TAG AstPat14  ALIAS astpat
USE cuentas  IN  7  ORDER TAG cuentas1 ALIAS cuenta
USE Auxil    IN  8  ORDER TAG Auxil1   ALIAS Auxi
USE maepre   IN 16   order tag maepre1      ALIAS maepre
USE itepar   in 25  order tag itepar1      ALIAS ITEPAR          

*- Mensajes de aviso al usuario
Vmens01 = ' Recibos de Ingreso : REVISION '
Vmens02 = ' °    Registro de Recibos de Ingreso   ° '
Vmens04 = 'Dicho R/I no fue encontrada'
Vmens05 = 'No existe R/I anterior'
Vmens06 = 'No existe R/I siguiente'
Vmens07 = '¨ Desea Anular este R/I ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este R/I ha sido anulada'
Vmens10 = 'El R/I ya est  Atendido'
Vmens11 = 'El R/I ha sido devuelto'
Vmens12 = 'El R/I ya tiene Cheque'
SELECT RI
GO BOTTOM
*- Variables de trabajo (registro a trabajar)
PUBLIC aa,vOrden,vOrd,vRec,vAli,vCli,vMes,vTipPro,vToCli,vCta,vAlo,vMesT,vKey,vTot1,M.TIPAUX,WCODPART,MDESCR,W_TIPCTC
PUBLIC vcodprg,vcodsub,vproyec,vsubpry,vcodact
STORE SPACE(1)  TO VORDEN
STORE SPACE(40) TO aa,vKey
STORE SPACE(2)  TO vMes,vMesT
STORE SPACE(11) TO WCODPART
vCodPrg  = space(2)  
vCodSub  = space(3)    
vProyec  = space(3) 
vSubpry  = space(4)
vCodAct  = space(3)
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista
ON KEY LABEL F2 DO VISOBS
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

 DEFINE WINDOW Wind_1 FROM 00,00 TO 14,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 15,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Rec.Ing.' ;
 COLOR SCHEME 10

 DEFINE WINDOW Wind_3 FROM 15,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Bol.dep.' ;
 COLOR SCHEME 10

 DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
 TITLE ' ASIENTOS PRESUPUESTALES' COLOR SCHEME 10

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
 @  1, 2 SAY "       N£mero R/I :"
 @  1,43 SAY "Fec.R/I :"
 @  1,62 SAY "Periodo :"
 @  2, 2 SAY " Deposito Cta.Cte :"
 @  2,62 SAY "Per.Ref :"
 @  3, 2 SAY "         Tipo R/I :"
 @  4, 2 SAY "    Tipo Auxiliar :" 
 @  5, 2 SAY "  Cod.  Auxiliar. :" 
 @  6, 2 SAY "  Doc. Referencia :"
 @  6,40 SAY " Fecha Doc.Refer. :"
 @ 07,02 SAY "Fte.Financiamiento:"
 @ 07,40 SAY "             Tipo :"
 @ 08,02 SAY "         Programa :"
 @ 09,02 SAY "      SubPrograma :"
 @ 10,02 SAY " Proyecto/Activid :"
 @ 11, 2 SAY " Monto Depositado :"
 @ 12,0 SAY PADC(' ° ®F2¯  Observaciones ° ',79,' ') COLOR W+/B
 RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_1
 SELECT RI
 SCATTER MEMVAR
 @  1,22 SAY m.NumRI
 @  1,27 SAY m.numMes
 @  1,52 SAY m.FecRI
 @  1,72 SAY m.periodo
 @  2,22 SAY m.CodCtc 
 @  2,72 SAY M.PERREF PICTURE "!!" 
 @  3,22 SAY Val_Para(m.TipRI,"TIPRI ","V",22,30)
 @  3,63 SAY IIF(m.estado='99','ANULADO','ATENDIDO')
 @  4,22 SAY Val_Para(m.TipAux,"AUXIL ","V",22,30)
 @  5,22 say m.codprv  PICTURE '@S10' 
 @  6,22 SAY m.DocRef   PICTURE "@S18"
 @  6,60 SAY m.FecDig 
 @  7,22 SAY Val_para(m.CodFte,'CODFTE','V',22)
 @  7,60 SAY val_para(m.tipfun,'TIPFUN','D',22,15)
 @  8,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
 @  9,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
 @ 10,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,3)
   IF alltrim(m.Tipfun)='I' 
	  @ 10,25 SAY '.'
	  @ 10,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
   ELSE
      @ 10,22 SAY Spac(60)	
   ENDIF
 @ 11,22 SAY m.CanRI    PICTURE "99,999,999.99"
 DO Vista_Hijo
 RETURN

PROCEDURE Vista_Hijo
*-------------------
SCATTER MEMVAR
SELECT IteRI
SEEK M.PERIODO+ALLTRIM(m.NumMes)+ALLTRIM(m.NumRi)
BROWSE ;
   NOCLEAR NOREFRESH NOOPTIMIZE NOEDIT TIMEOUT 0.005;
   WINDOW Wind_2 KEY M.PERIODO+ALLTRIM(m.NumMes)+ALLTRIM(m.NumRi);
   FIELDS;
   BolDep  :H= 'Boleta',;
   FecDep  :H= 'FecDep',;
   ImpParc :H= 'Nombre Part.',;
   CodPart :H= 'C¢d.Par' ,;
   aa = IIF(!EMPTY(CODPART),mdescr,' ') :H='Descripci¢n' :40 :W=!EMPTY(CodPart)
   SELECT RI
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELE RI
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET RELATION TO PERIODO+NumMes+NumRi INTO ITERI
 SET SKIP TO ITERI
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumRI         :H=' N§ RI',;
 NumMes        :H='Mes',;
 FecRI         :H='FecRI',;
 CodCtc        :H='CtaCte',;
 IteRI.BolDep  :H='Boleta',;
 IteRI.FecDep  :H='Fec.B/D',;
 IteRI.Impparc :H='Monto B/D',;
 CanRI         :H='Total R/I',;
 DocRef        :H='Referencia'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION OFF INTO RI
 SELE RI
 SET RELATION OFF INTO ITERI
 DO Vista
 RETURN

PROCEDURE Busca              && Realiza b£squeda directa
*--------------
VTEMP=RECNO()
IF EOF()
   DO standby WITH Vmens08
   RETURN
ENDIF
vano=space(2)
vano=right(str(year(date())),2)
ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero RI: ' GET vRI PICTURE '@!' DEFAULT SPACE(4)
 @ 1,26 SAY 'Mes:' GET vMes
 @ 1,36 SAY 'A¤O:' GET vano
READ
DEACTIVATE WINDOW standby
 IF EMPTY(vRI) .or. LASTKEY()=27
    RETURN
 ELSE
    vRI = PADL(ALLTRIM(vRI),4,'0')
    SEEK vano+ALLTRIM(vMes)+ALLTRIM(vRI)
    IF !FOUND()
       DO standby WITH Vmens04
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF
RETURN

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
 IF Estado = '99'  && Anulada
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 SELECT RI
 SCATTER MEMVAR
 ACTIVATE WINDOW Wind_1
 DO PANTALLA
 IF RLOCK() OR F_LOCK(1)
 VCODPRG=SUBSTR(M.CODCAL,8,2)
 VCODSUB=SUBSTR(M.CODCAL,10,3)
    @  1,22 SAY m.NumRI
    @  1,27 GET m.NumMes   PICTURE '!!'  VALID Val_Para(m.NUmMes  ,'FECMES',' ',27,9) DISABLE
    @  1,52 GET m.FecRI 
    @  1,72 get m.periodo  PICTURE "!!" VALID valf(m.fecri)  DISABLE
    @  2,22 GET m.CodCtc   PICTURE "@!" VALID Val_Fun("Caja","CodCtc","CodCtc+' '+Descri",m.CodCtc,1,2,22) DISABLE
    @  2,72 GET M.PERREF   PICTURE "!!"
    READ
    @  3,22 GET m.TipRI    PICTURE "@!" VALID Val_Para(m.TipRI,"TIPRI "," ",22)
    @  4,22 GET m.TipAux   PICTURE "!!" VALID val_Para(m.TipAux,"AUXIL "," ",22) 
    @  5,22 GET m.CodPrv   PICTURE "@!" VALID val_Auxi(m.CodPrv,m.TipAux,'A',30)
    @  6,22 GET m.DocRef   PICTURE "@S18"  &&VALID Val_Para(m.DocRef,'TIPDOC','C',22,30)
    @  6,60 GET m.FecDig
	@  7,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	@  7,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)  
    @ 8,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) 
    @ 9,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
    @ 10,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Perref+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
    @ 10,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Perref+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
    @ 10,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Perref+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and ALLTRIM(m.tipfun)='I'
    @ 11,22 GET m.CanRI    PICTURE "99,999,999.99"
    READ VALID Val_Read()
    m.codcal  = m.perref+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))	  
 	  SELE CAJA
	  SEEK M.CODCTC
	  W_TIPCTC=CAJA.TIPO
	  SELE RI
      IF LASTKEY() # 27
         =Observa()    
         SELECT RI
         IF RLOCK()
            GATHER MEMVAR
         ENDIF
       SELECT IteRI
       DO IngAP
       DO Trabaja_HijO
       =Valida()
       do compre
    ELSE
       DO STANDBY WITH 'Proceso cancelado'
    ENDIF
 ELSE
    DO STANDBY WITH 'Registro bloqueado'
 ENDIF
 UNLOCK ALL
 ON KEY LABEL F2 DO VISOBS
 DO Vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT RI
 DO Pantalla
 SCATTER MEMVAR BLANK
 SELECT Parma
 SEEK 'CORREL'+'RECING'
 m.NumRI = PADL(NumEnt+1,4,'0')
 m.FecRI = DATE()
 m.FecDEP= DATE()
 m.fecdig=date()
 m.Estado= '10'
   @  1,22 GET m.NumRI  PICTURE '!!!!'
   @  1,26 SAY '.'
   @  1,27 GET m.NumMes  PICTURE '!!'   VALID Val_Para(m.NUmMes  ,'FECMES',' ',27,9) .AND. IGUAL()
   @  1,52 GET m.FecRI VALID VALF(M.FECRI)
   @  1,72 get m.periodo PICTURE '!!'
   @  2,22 GET m.CodCtc PICTURE "@!"   VALID Val_Fun("Caja","CodCTc","CodCtc+' '+Descri",m.CodCTc,1,2,22)
   @  2,72 GET M.PERREF  PICTURE "!!"
   READ
   @  3,22 GET m.TipRI  PICTURE "@!"   VALID Val_Para(m.TipRI,"TIPRI "," ",22,30)
   @  4,22 GET m.TipAux   PICTURE "!!" VALID Val_Para(m.TipAux,"AUXIL "," ",22,30) 
   @  5,22 GET m.CodPrv   PICTURE "@!" VALID val_Auxi(m.CodPrv,m.TipAux,"A",30)
   @  6,22 GET m.DocRef PICTURE "@S18"
   @  6,60 GET m.FecDig
   @  7,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
   @  7,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2) 
   @  8,22 GET vCodPrg     PICTURE '!!'  VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) 
   @ 9,22  GET vCodSub     PICTURE '!!!' VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
   @ 10,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Perref+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
   @ 10,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Perref+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
   @ 10,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Perref+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
   @ 11,22 GET m.CanRI  PICTURE "99,999,999.99"
   READ VALID Val_Read()
   m.codcal  = m.perref+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))	  
  SELE CAJA
  SEEK ALLTRIM(M.CODCTC)
  IF FOUND()
  W_TIPCTC=CAJA.TIPO
  ENDIF
  SELE RI
 IF LASTKEY()#27
    vAnula = .F.
    SELECT RI
    IF F_Appd()
       GATHER MEMVAR
       UNLOCK ALL
       SELECT Parma
       SEEK 'CORREL'+'RECING'
       REPLACE NumEnt WITH NumEnt + 1       
       SELECT RI
       DO Agreg_item
       SELECT IteRI
       =Observa()
       SCATTER MEMVAR
       DO WHILE .T.
          SELECT IteRI
          DO IngAp
   		  DO Trabaja_Hijo
          IF LASTKEY() = 27
             IF YESNO( '¨ Cancela el Ingreso ?' )
                vAnula = .T.
                EXIT
             ENDIF
          ELSE
             IF YESNO( '¨ Est n correctos los datos ?')
                EXIT
                =Valida()
             ENDIF
          ENDIF
       ENDDO
       IF vAnula
          DO Anula
       ENDIF
    ELSE
       GO BOTTOM
    ENDIF
	DO compre    
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT RI
 GO BOTT
 ON KEY LABEL F2 DO VISOBS
 DO Vista
 RETURN


FUNCTION VALF
*-----------------
PARAMETERS XFEC
m.periodo=RIGHT(STR(YEAR(XFEC)),2)
M.PERREF=M.PERIODO
RETURN 


PROCEDURE Trabaja_Hijo
*---------------------
 ACTIVATE SCREEN
 HIDE MENU mMenu
 vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F5  DO Agreg_item
 ON KEY LABEL F8  DO Elimi_item
 ON KEY LABEL F10 KEYBOARD CHR(23)
 SELE ITERI
 SEEK ALLTRIM(M.PERIODO)+ALLTRIM(m.NumMes)+ALLTRIM(m.NumRI)
 IF !FOUND()
    DO Agreg_Item
 ENDIF
   BROWSE NOAPPEND NODELETE NOMENU NOREFRESH NOOPTIMIZE WINDOW Wind_2;
   KEY M.PERIODO+ALLTRIM(m.NumMes) + ALLTRIM(m.NumRI) FIELD ;
   Tipo       :H= 'T':P='@M O,P',;
   CodPart    :H= 'Partida':V=VAL_INGE(SUBSTR(CodPart,10,2),LEFT(CodPart,8),'codpart'):F:W=Tipo='P',;
   aa = IIF(!EMPTY(CODPART),mdescr,' ') :H='Descripci¢n' :40 :W=!EMPTY(CodPart),;
   BolDep     :H= 'N§ Bol./Dep.': P='@!',;
   FecDep     :H= 'FechaDep',;
   Glosa      :H= 'Ingreso en': P='@!',;
   ImpParc    :H='Monto' : P='999,999,999,999.99'
 SELECT ITERI
 ON KEY LABEL F10
 ON KEY LABEL F8
 ON KEY LABEL F5
 UNLOCK ALL
 SELECT RI
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 SHOW MENU mMenu
 RETURN


PROCEDURE Anula
*--------------
 SELECT RI
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 IF Estado = '99' && ya pas¢
    DO STANDBY WITH Vmens09
    RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR este R/I ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   REPLACE ESTADO WITH '99'
 ENDIF
 UNLOCK
 sele astpat

 SEEK M.PERIODO+ALLTRIM(m.nummes)+m.numri+alltrim(m.codctc)
 IF FOUND()
   	scan while PERIODO=M.PERIODO AND nummes=alltrim(m.nummes) and numref=m.numri and codctc=m.codctc and tipdoc="R/I" 
	    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
		ENDIF       
	    UNLOCK
    ENDSCAN
 ENDIF	    
    USE astpre   IN 9  ORDER TAG astpre20      ALIAS astpre  
    sele astpRE
    SEEK M.PERIODO+ALLTRIM(m.nummes)+m.numRI+alltrim(m.codctc)
    IF FOUND()
    	SCAN while PERIODO=M.PERIODO AND nummes=alltrim(m.nummes) and numref=m.numRI and codctc=m.codctc and tipdoc="R/I" 
		    IF RLOCK() .OR. F_LOCK(1)
		       DELETE NEXT 1
		    ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
    USE IN 9 
    SELE RI
   DO Vista
 RETURN

PROCEDURE Agreg_Item
*-------------------
SELECT IteRI
IF F_Appd()
   REPLACE PERIODO WITH M.PERIODO,NumRI WITH m.NumRI ,NumMes with m.NumMes, ImpParc WITH 0, Estado WITH m.Estado
ENDIF
RETURN


PROCEDURE Elimi_Item
*-------------------
SELECT IteRI
if rlock()
   SELECT IteRI
   DELETE NEXT 1
else
   do standby with 'No puede eliminar este Item.'
endif

return


PROCEDURE Lista  &&Reportes varios
*--------------
vAli=ALIAS()
vOrd=ORDER()
IF EOF()
   DO standby WITH Vmens10
   RETURN
ENDIF
IF LASTKEY()=27
   RETURN
ENDIF
vTemp=RECNO()
PRIVATE VFTE
DEFINE WINDOW LIS FROM 4,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Recibo de Ingresos' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1         TO vToCli,vOrden,vTipPro,vToCta,vLista
STORE SPACE(4)  TO vCli
STORE SPACE(2)  TO vMes,vMesT,vTipRI,vtipaux,VPER
store space(10) to vauxiliar
STORE SPACE(14) TO vcta
STORE SPACE(3)  TO vFTE
vMesT='0'+ALLTRIM(STR(MONTH(DATE())))
vMest = PADL(ALLTRIM(vMest),2,'0')
store date() to vfecini,vfecfin
VPER=M.PERIODO
VCLI=M.NUMRI
VMES=M.NUMMES
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;CtaCte;FteFto;Operaciones;Concepto;Auxiliar'          
@ 05,01 SAY "           N§ R/I : "
@ 05,22 GET vCli    WHEN VLISTA=1  PICTURE '!!!!'
@ 05,27 GET vMesT    WHEN VLISTA=1  PICTURE '!!'  
@ 05,30 GET VPER   WHEN VLISTA=1  PICTURE '!!'   VALID ValRes()
@ 06,01 SAY "   Cta. Corriente : "
@ 06,22 GET vcta  VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,7,22) WHEN  vlista=2 OR VLISTA=5
@ 07,01 SAY "      Fte. Financ.: " GET vfte  VALID Val_Para(Vfte,"CODFTE"," ",22,30) WHEN VLISTA=3 
@ 09,01 SAY "         Concepto : " GET vTIPRI  VALID Val_Para(VTipRI,"TIPRI "," ",22,30) WHEN VLISTA=5 
@ 10,01 SAY "     Tipo Auxiliar: " GET vTIPaux VALID Val_Para(vtipaux,"AUXIL "," ",22,30) WHEN VLISTA=6
@ 11,01 SAY "   C¢digo Auxiliar: " GET vAUXILIAR VALID Val_AUXI(Vauxiliar,VTIPAUX," ",30) WHEN VLISTA=6 
@ 12,01 SAY "              Mes : "
@ 12,22 GET vMesT  wHEN vLista=4  PICTURE '!!'
@ 12,27 GET VPER WHEN VLISTA=4 PICTURE "!!"
@ 13,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Registrados;Anulados' WHEN vlista=2
@ 15,01 SAY " Fecha de Emisi¢n : "
@ 16,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista<>1 and vlista<>4
@ 16,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista<>1 and vlista<>4
@ 18,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   SELECT RI
   SET MEMOWIDTH TO 50
   vInd = SYS(3) + '.IDX'
   DEACTIVATE WINDOW STANDBY
      DO CASE
	   CASE vLista = 1
		    VKEY=PERIODO+ALLTRIM(VMES)+VCLI
    		SELE RI
    		SEEK VKEY
    		DO repPRG WITH "LISRI"," Listado Comprobante de Pago ",2
         CASE vLista=2
		      vTitulo=IIF(vTipPro=1,'Registrados ',' Anulados ')
	          DO CASE
	           CASE Vtippro=1
		     	   SELECT I.*, R.TIPRI, R.CODCTC  ;
		     	   FROM ITERI I, RECING R;
		     	   WHERE !EMPTY(R.PERIODO+R.NumMes+R.NumRi) AND ;
		     	    (R.PERIODO+R.NumMes+R.NumRi=I.PERIODO+I.NumMes+I.NumRi) ;
		     	   	AND (R.codctc=vcta AND BETWEEN(R.fecRI,vfecini,vfecfin);
		     	   	AND R.ESTADO#"99");
		     	   ORDER BY I.numri ;
		     	   INTO CURSOR ITRI

		     	   SELECT ITRI
		     	   INDEX ON PERIODO+NumMes+NumRi TO PP
				   SELECT RI
		     	   SET FILTER TO codctc=vcta AND BETWEEN(fecRI,vfecini,vfecfin) AND ESTADO#"99"
				   SELECT Itri
	  		       SET RELATION TO PERIODO+NumMes+NumRi INTO RI
				   SELECT ITRI
		     	   IF EOF()
		     	      DO STANDBY WITH "Registros no existen"
		     	      SET FILT TO
		     	      SELE RI
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
               CASE Vtippro=2
		     	   SELECT I.*, R.TIPRI, R.CODCTC  ;
		     	   FROM ITERI I, RECING R;
		     	   WHERE !EMPTY(R.PERIODO+R.NumMes+R.NumRi) AND ;
		     	    (R.PERIODO+R.NumMes+R.NumRi=I.PERIODO+I.NumMes+I.NumRi) ;
		     	   	AND (R.codctc=vcta AND BETWEEN(R.fecRI,vfecini,vfecfin);
		     	   	AND R.ESTADO="99");
		     	   ORDER BY I.numri ;
		     	   INTO CURSOR ITRI

		     	   SELECT ITRI
		     	   INDEX ON PERIODO+NumMes+NumRi TO PP
				   SELECT RI
		     	   SET FILTER TO codctc=vcta AND BETWEEN(fecRI,vfecini,vfecfin) AND ESTADO="99"
				   SELECT Itri
	  		       SET RELATION TO PERIODO+NumMes+NumRi INTO RI
				   SELECT ITRI
		     	   IF EOF()
		     	      DO STANDBY WITH "Registros no existen"
		     	      SET FILT TO
		     	      SELE RI
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
		     ENDCASE	   
*             DO REPORTE WITH 2,"LisRICta",' Reporte de R/I por Cuentas Corrientes ',1,.F.,.T.
             DO REPORTE WITH 2,"LisRICt",' Reporte de R/I por Cuentas Corrientes ',1,.F.,.T.
             set filter to
         CASE vLista=3
              INDEX ON RI.CodFTE TO (vInd) FOR BETWEEN(RI.fecRI,vfecini,vfecfin) AND RI.ESTADO='10'
		     	   IF EOF()
		     	      DO STANDBY WITH "Registros no existen"
		     	      SET INDEX TO
		     	      SET ORDER TO RECING1
		     	      SELE RI
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
              DO REPORTE WITH 2,"LisRIFte",' Reporte de R/I por Fuente de Financiamiento ',1,.F.,.T.
         CASE vLista=4
              SELECT AstPat
              SET FILTER TO ASTPAT.NUMMES =ALLTRIM(VMEST)  AND ASTPAT.PERIODO=ALLTRIM(VPER)
		      SET RELATION TO PERIODO+nummes+numref  INTO RI ADDI
		     	   IF EOF()
		     	      DO STANDBY WITH "Registros no existen"
		     	      SET FILT TO
		     	      SELE RI
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
              DO REPORTE WITH 2,"LisRIOPE",' Listado de Operaciones ',1,.F.,.T.
              SET FILT TO
              SET ORDER TO ASTPAT14
              SELE RI
         CASE vLista=5
              INDEX ON RI.TipRI TO (vInd)  FOR  RI.TIPRI = ALLTRIM(VTIPRI) AND BETWEEN(RI.fecRI,vfecini,vfecfin) AND RI.ESTADO<>'99' AND RI.CODCTC=ALLTRIM(VCTA)
		     	   IF EOF()
		     	      DO STANDBY WITH "Registros no existen"
		     	      SET INDEX TO
		     	      SET ORDER TO RECING1
		     	      SELE RI
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
              DO REPORTE WITH 2,"LisRICON",' Reporte por Tipo de Recibo de Ingreso ',1,.F.,.T.
         CASE vLista=6
              INDEX ON RI.Tipaux+RI.codprv TO (vInd) FOR BETWEEN(RI.fecRI,vfecini,vfecfin) AND RI.ESTADO='10' .AND. RI.TIPAUX = ALLTRIM(VTIPAUX) AND RI.CODPRV = ALLTRIM(VAUXILIAR) 
         	   IF EOF()
		     	      DO STANDBY WITH "Registros no existen"
		     	      SET INDEX TO
		     	      SET ORDER TO RECING1
		     	      SELE RI
		     	      GO BOTT
		     	      RETURN
 	     	   ENDIF   
               DO REPORTE WITH 2,"LisRIAux",' Reporte por Tipo de AUXILIAR ',1,.F.,.T.
      ENDCASE
      sele ri
      SET MEMOWIDTH TO 50
ENDIF
IF vTemp>0
   GO vTemp
ENDIF
DO vista
RETURN


PROCEDURE ValRes
*--------------
 SELECT RI
 vTemp=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SEEK VPER+vMes+vCli
 IF !FOUND()
    SET ORDER TO RecIng1
    HIDE MENU mMenu
    ACTIVATE SCREEN
    vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo    
    ON KEY LABEL F10 KEYBOARD CHR(23)    
    BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
    PERIODO,;
    NumMes :H='Mes',;
    NumRI  :H=' N§ RI' ,;
    FecRI  :H='Fecha' ,;
    CanRI  :H='Total R/I',;
    Observ :H='Observaci¢n',;
    CodCtc :H='CtaCte'

    vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
    DO Logos WITH Rotulo1,vTempo
    IF LASTKEY()=27
       GO Vtemp
    ENDIF

    SHOW MENU mMenu
    ON KEY LABEL F10
    SET RELATION TO
    GO Vtemp
 ENDIF
 vMes=NumMes
 vCli=NumRI
 RETURN .t.


PROCEDURE LISRI &&Programa Reporte usa repprg F.M.V.
*--------------
PARAMETERS XCOP
PRIVATE FILA,VDEBPRE,VHABPRE
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
FOR I=1 TO XCOP
STORE 0 TO XVAL,TVAL
VDEBPRE=SPACE(11)
HABPRE=SPACE(11)
SELE RI
vMES=alltrim(nummes)
vcli=numRI
VCTA=ALLTRIM(CODCTC)
Vkey= RI.PERIODO+ALLTRIM(RI.NUMMES)+RI.NUMRI
@0,0 SAY CHR(15)
@1,32 SAY "<< RECIBO DE INGRESOS N§ "+NUMRI+'.'+NUMMES+" >>"
@1,88  say "PAG:"
@1,96  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY ALLTRIM(CIA)
@2,70 SAY IIF(Estado='99','A N U L A D O',' A T E N D I D O')
@3,3   SAY "LisRi"
@3,35 SAY "Presupuestado A¤o 199"+RIGHT(DTOC(DATE()),1)
@3,88 SAY "FECHA:"
@3,96 SAY fecri           
fila=4
@FILA,1 SAY "Ú"
@FILA,2 SAY REPLICATE("Ä",103)
@FILA,105 SAY "¿" 
FILA=FILA+1
@FILA,1 SAY "³"
@FILA,4 SAY "CODIGO"
@FILA,14 SAY "³"
@FILA,36 SAY "C O N C E P T O"
@FILA,65 SAY "³"
@FILA,78 SAY "I M P O R T E"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,65 SAY "³"
@FILA,71 SAY "PARCIAL"
@FILA,91 SAY "TOTAL"
@FILA,105 SAY "³"
fila=fila+1
@FILA,1 SAY "À"
@FILA,2 SAY REPLICATE("Ä",103) 
@FILA,105 SAY "Ù"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,1)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,2)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,3)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,4)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,5)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,6)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,7)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,8)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,9)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,10)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,11)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,12)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,13)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,14)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,15)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,16)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,17)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,18)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,19)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,20)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,16 SAY MLINE(RI.Observ,21)
@FILA,66 SAY "³"
@FILA,85 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
SELE ITERI
SEEK VKEY 
IF FOUND()
   VDEBPRE=IIF(ITERI.CODPART=" "," ",IIF(SUBSTR(RI.CODCAL,5,3)="PRP","9120100000","9140100000"))
   VHABPRE=IIF(ITERI.CODPART=" "," ",IIF(SUBSTR(RI.CODCAL,5,3)="PRP","9020100000","9040100000"))
	@FILA,03 SAY ITERI.CODPART
	@FILA,14 SAY "³"
	@FILA,16 SAY IIF(!EMPTY(Iteri.concep),LEFT(IteRI.ConCep,48),LEFT(IteRI.Glosa,48))
ELSE
	@FILA,14 SAY "³"
ENDIF
SELE RI
@FILA,66 SAY "³"
@fila,67 say RI.canri picture '@z 999,999,999.99'
@FILA,85 SAY  "³"
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY  "³"
@FILA,14 SAY  "³"
@FILA,66 SAY  "Ú"
@FILA,67 SAY REPLICATE("Ä",38)
@FILA,105 SAY "¿" 
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,66 SAY "³"
@FILA,72 SAY "TOTAL  --->"
@FILA,87 SAY RI.CANRI picture '@z 999,999,999,999.99'
@FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
@FILA,14 SAY "³"
@FILA,66 SAY "À"
@FILA,67 SAY REPLICATE("Ä",38) 
@FILA,105 SAY "Ù"
FILA=FILA+1
@FILA,1 SAY "<<<<<< CODIGO DE LA CONTABILIDAD PRESUPUESTAL Y CLASIFICACION PROGRAMATICA DEL GASTO PUBLICO  >>>>>>>>>"
FILA=FILA+4
@FILA,1 SAY "Ú"
@FILA,2 SAY REPLICATE("Ä",103)
@FILA,105 SAY "¿" 
FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,9 SAY "Cuenta Mayor"
 @FILA,32 SAY "³"
 @FILA,40 SAY "³"
 @FILA,49 SAY "³"
 @FILA,54 SAY "³"
 @FILA,59 SAY "³"
 @FILA,64 SAY "³"
 @FILA,70 SAY "³"
 @FILA,76 SAY "³"
 @FILA,86 SAY "³"
 @FILA,96 SAY "³"
 @FILA,105 SAY "³"
 FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,6 SAY "Debe"
 @FILA,17 SAY "³"
 @FILA,22 SAY "Haber"
 @FILA,32 SAY "³"
 @FILA,34 SAY "Sector"
  @FILA,40 SAY "³"
 @FILA,42 SAY "Pliego"
 @FILA,49 SAY "³"
 @FILA,51 SAY "Prg"
 @FILA,54 SAY "³"
 @FILA,56 SAY "Spg"
 @FILA,59 SAY "³"
 @FILA,61 SAY "Pry"
 @FILA,64 SAY "³"
 @FILA,66 SAY "Obra"
 @FILA,70 SAY "³"
 @FILA,72 SAY "Act"
 @FILA,76 SAY "³"
 @FILA,79 SAY "Tarea"
 @FILA,86 SAY "³"
 @FILA,87 SAY "Funcional"
 @FILA,96 SAY "³"
  @FILA,98 SAY "FteFto"
 @FILA,105 SAY "³"
 fila=fila+1
 @FILA,1 SAY "³"
 @FILA,2 say replicate("-",29)
 @FILA,32 SAY "³"
 @FILA,33 say replicate("-",6)
 @FILA,40 SAY "³"
 @FILA,41 say replicate("-",7)
 @FILA,49 SAY "³"
 @FILA,50 say replicate("-",3)
 @FILA,54 SAY "³"
 @FILA,55 say replicate("-",3)
 @FILA,59 SAY "³"
 @FILA,60 say replicate("-",3)
 @FILA,64 SAY "³"
 @FILA,65 say replicate("-",4)
 @FILA,70 SAY "³"
 @FILA,71 say replicate("-",4)
 @FILA,76 SAY "³"
 @FILA,77 say replicate("-",8)
 @FILA,86 SAY "³"
 @FILA,87 say replicate("-",8)
 @FILA,96 SAY "³"
 @FILA,97 say replicate("-",8)
 @FILA,105 SAY "³"
FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,3 SAY VDEBPRE
 @FILA,17 SAY "³"
 @FILA,19 SAY VHABPRE
 @FILA,32 SAY "³"
 @FILA,35 SAY "25"
 @FILA,40 SAY "³"
 @FILA,43 SAY "001"
 @FILA,49 SAY "³"
 @FILA,51 SAY SUBSTR(RI.CodCal,8,2)
 @FILA,54 SAY "³"
 @FILA,56 SAY SUBSTR(RI.CodCal,10,3)
 @FILA,59 SAY "³"
 @FILA,61 SAY SUBSTR(RI.CodCal,13,3)
 @FILA,64 SAY "³"
 @FILA,66 SAY SUBSTR(RI.CodCal,16,2)
 @FILA,70 SAY "³"
 @FILA,76 SAY "³"
 @FILA,86 SAY "³"
 @FILA,96 SAY "³"
 @FILA,99 SAY SUBSTR(RI.CodCal,5,3)
 @FILA,105 SAY "³"
 FILA=FILA+1
@FILA,1 SAY "À"
@FILA,2 SAY REPLICATE("Ä",103) 
@FILA,105 SAY "Ù"
fila=fila+1
@FILA,1 SAY "Ú"
@FILA,2 SAY REPLICATE("Ä",64)
@FILA,66 SAY "¿" 
@FILA,68 SAY "Ú"
@FILA,69 SAY REPLICATE("Ä",36)
@FILA,105 SAY "¿" 
fila=fila+1
 @FILA,1 SAY "³"
 @FILA,10   SAY "C O N T A B I L I D A D   P A T R I M O N I A L"
 @FILA,66 SAY "³"
 @FILA,68 SAY "³"
 @FILA,105 SAY "³"
 FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,08 SAY "C O D I G O                           I M P O R T E"
 @FILA,66 SAY "³"
 @FILA,68 SAY "³"
 @FILA,105 SAY "³"
 FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,2 SAY REPLICATE('-',64)
 @FILA,66 SAY "³"
 @FILA,68 SAY "³"
 @FILA,105 SAY "³"
 FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,2 SAY "CUENTA"
 @FILA,8 SAY "³"
 @FILA,13 SAY "SUB-CTAS"
 @FILA,26 SAY "³"
 @FILA,36 SAY "DEBE"
 @FILA,46 SAY "³"
 @FILA,56 SAY "HABER"
 @FILA,66 SAY "³"
 @FILA,68 SAY "³"
 @FILA,69 SAY "1.----------------------------------"
 @FILA,105 SAY "³"
 FILA=FILA+1
 @FILA,1 SAY "³"
 @FILA,2 SAY "MAYOR"
 @FILA,8 SAY "³"
 @FILA,26 SAY "³"
 @FILA,46 SAY "³"
 @FILA,66 SAY "³"
 @FILA,68 SAY "³"
 @FILA,69 SAY "       Director de tesorer¡a        "
 @FILA,105 SAY "³"
 fila=fila+1
@FILA,1 SAY "À"
@FILA,2 SAY REPLICATE("Ä",64) 
@FILA,66 SAY "Ù"
@FILA,68 SAY "À"
@FILA,69 SAY REPLICATE("Ä",36) 
@FILA,105 SAY "Ù"
 fila=fila+1
@FILA,1 SAY "Ú"
@FILA,2 SAY REPLICATE("Ä",64)
@FILA,66 SAY "¿" 
@FILA,68 SAY "Ú"
@FILA,69 SAY REPLICATE("Ä",36)
@FILA,105 SAY "¿" 
XKEY=RI.PERIODO+ALLTRIM(RI.NUMMES)+RI.NUMRI+ALLTRIM(RI.CODCTC)
SELE ASTPAT
SEEK XKEY
FILA=FILA+1
SCAN WHILE PERIODO=VPER AND NUMMES=ALLTRIM(VMES) AND NUMREF=VCLI AND CODCTC=ALLTRIM(VCTA)
     @FILA,1 SAY "³"
     IF TIPCTA="D"
     	  @FILA,02 SAY SUBSTR(ASTPAT.CODCTA,1,2)
		  @FILA,8 SAY "³"
     	  @FILA,09 SAY SUBSTR(ASTPAT.CODCTA,1,3)
		  @FILA,17 SAY "³"
		  @FILA,26 SAY "³"
          @FILA,27 SAY ASTPAT.MTODEB  PICTURE "@z 999,999,999,999.99"             
     ELSE     
     	  @FILA,05 SAY SUBSTR(ASTPAT.CODCTA,1,2)
		  @FILA,8 SAY "³"
		  @FILA,17 SAY "³"
     	  @FILA,18 SAY SUBSTR(ASTPAT.CODCTA,1,3)
		  @FILA,26 SAY "³"
          @FILA,47 SAY ASTPAT.MTOHAB  PICTURE "@z 999,999,999,999.99"             
     ENDIF     
     @FILA,66 SAY "³"
     @FILA,68 SAY "³"
     @FILA,105 SAY "³"
     FILA=FILA+1
ENDSCAN
SELE RI
@FILA,1 SAY "À"
@FILA,2 SAY REPLICATE("Ä",64) 
@FILA,66 SAY "Ù"
@FILA,68 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
@FILA,1 SAY "Ú"
@FILA,2 SAY REPLICATE("Ä",44)
@FILA,46 SAY "¿" 
@FILA,68 SAY "³"
@FILA,105 SAY "³"
FILA=FILA+1
@FILA,1 SAY "³"
@FILA,3 SAY "VoBo"
@FILA,46 SAY "³"
@FILA,68 SAY "³"
@FILA,69 SAY "2.----------------------------------"
@FILA,105 SAY "³"
FILA=FILA+1
@FILA,1 SAY "À"
@FILA,2 SAY REPLICATE("Ä",44) 
@FILA,46 SAY "Ù"
@FILA,68 SAY "³"
@FILA,69 SAY "    Recepcionista del Dep¢sito      "
@FILA,105 SAY "³"
FILA=FILA+1
@FILA,68 SAY "À"
@FILA,69 SAY REPLICATE("Ä",36) 
@FILA,105 SAY "Ù"
ENDFOR
SET DEVICE TO SCREEN 
RETURN


FUNCTION Observa
*---------------
vAlias = ALIAS()
SELECT RI
SET MEMOWIDTH TO 50
ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF
IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF
MODIFY MEMO OBSERV WINDOW OBSERVA
IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba la Observaci¢n '
ENDIF
SELECT (vAlias)
RETURN .T.


FUNCTION VisObs
*--------------
vAlias = ALIAS()
SELECT RI
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,14 TO 20,66 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Observa")
   ACTIVATE WINDOW Observa SAME
ELSE
   ACTIVATE WINDOW Observa NOSHOW
ENDIF

MODIFY MEMO OBSERV NOEDIT WINDOW OBSERVA

IF !WVISIBLE("Observa")
   ACTIVATE WINDOW Observa
ENDIF
RELEASE WINDOW Observa
RETURN .T.


FUNCTION VALFECHA
*----------------
an=right(str(year(date()),4),2)
vMe = val(m.Nummes)+1
me = padl(alltrim(str(vMe,2)),2,'0')
vFec = '01/&ME/&AN'
m.FecRI = ctod(vFec) - 1
return .t.


FUNCTION Valida && Suma el ingreso de todos los Items ingresados.
*--------------
PRIVATE VACUM
vAcum=0
SELE ITERI
SEEK M.PERIODO+ALLTRIM(M.NUMMES)+M.NUMRI
SCAN WHILE PERIODO=M.PERIODO AND ALLTRIM(M.NUMMES)=ITERI.NUMMES AND M.NUMRI=ITERI.NUMRI
     vAcum = vAcum + ITERI.impparc
ENDSCAN
SELE RI
IF vAcum > RI.CANRI
   DO StandBy WITH 'ERROR! El importe excede el monto depositado. CORREGIR'
ENDIF
IF vAcum < RI.CANRI
   DO StandBy WITH 'ERROR! El importe es menor al monto depositado. CORREGIR'
ENDIF
RETURN


PROCEDURE ingap   && Ingresa asientos patrimoniales
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
SEEK M.PERIODO+ALLTRIM(m.nummes)+m.numri+m.codctc
IF !FOUND()
	DO agrite
ENDIF
BROWSE FIELDS ;
    periodo :R,;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2):F ,;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999.99';
	WINDOW wind_2 KEY M.PERIODO+ALLTRIM(m.NumMes)+m.NumRI+m.CodCtc
STORE 0 TO vdebe, vhaber ,vRet
SEEK M.PERIODO+ALLTRIM(m.nummes)+m.numri+m.codctc
SCAN WHILE PERIODO=M.PERIODO AND ALLTRIM(m.nummes)=nummes AND m.numri=numri AND m.codctc=codctc 
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	dele next 1
    else
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF		
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SELECT RI
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
RETURN


PROCEDURE ingapbd   && Ingresa asientos patrimoniales Boletas de Dep¢sito
*--------------
ACTIVATE SCREEN
HIDE MENU mMenu
vTempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT astpat
_bdord=ORDER()
SET ORDER TO ASTPAT6
SEEK M.PERIODO+ALLTRIM(m.nummes)+m.numri+m.codctc
IF !FOUND()
	DO agrite
ENDIF
BROWSE FIELDS ;
    periodo :R,;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2):F ,;
	tipcta :H='D/H' :P='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :P='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :P='999,999,999.99';
	WINDOW wind_2 KEY M.PERIODO+ALLTRIM(m.NumMes)+m.NumRI+m.CodCtc
STORE 0 TO vdebe, vhaber ,vRet
SEEK M.PERIODO+ALLTRIM(m.nummes)+m.numri+m.codctc
SCAN WHILE PERIODO=M.PERIODO AND ALLTRIM(m.nummes)=nummes AND m.numri=numri AND m.codctc=codctc 
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	dele next 1
    else
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF		
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
ON KEY
SET ORDER TO (_bdord)
SELECT RI
ON KEY LABEL F10
ON KEY LABEL F8
ON KEY LABEL F5
UNLOCK
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
SHOW MENU mMenu
RETURN

PROCEDURE agrite
*---------------
SELECT astpat
IF f_appd()
	REPLACE periodo WITH m.periodo,;
	     	nummes WITH m.nummes,;
	        numref WITH m.numri,;
	        tipdoc WITH 'R/I',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.FecRI,;
	        TIPCTC WITH W_TIPCTC
ENDIF
RETURN

PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN

FUNCTION SEPARA  &&Para el reporte
*--------------
AS=ALIAS()
VKEY = PERIODO+NUMMES+NUMRI
SELECT ASTPAT
vInd = SYS(3) + '.Idx'
INDEX ON PERIODO+NumMes+NumREF+CodCtc+TipCta to (vInd) for PERIODO+NUMMES+NUMREF = VKEY .AND. TipDoc = 'R/I'
SELECT (AS)
RETURN ' '


FUNCTION CUENTA  &&Para el reporte
*--------------
PARAMETER TIPO
AS=ALIAS()
VKEY = PERIODO+NUMMES+NUMRI
SELECT ASTPAT
VCTA=IIF(TIPO='D',ASTPAT.CODCTA,ASTPAT.CODCTA)
VALO=IIF(TIPO='D',ASTPAT.MTODEB,ASTPAT.MTOHAB)
IF !EOF()
   SKIP
ENDIF
SELECT (AS)
RETURN ' '


FUNCTION TotalRI && Suma todos los Items ingresados.
*---------------
PUBLIC vTot1
vAli=ALIAS()
vOrd=ORDER()
SELECT ITERI
vRec=RECNO()
vNumRI=RI.NumRI
vNumMes=RI.NumMes
VPER=RI.PERIODO
vTot1=0
SEEK VPER+ALLTRIM(VNUMRI+VNUMMES)
*SET FILTER TO ALLTRIM(RI.NumMes)=vNumMes .AND. ALLTRIM(RI.NumRI)=vNumRI
*GO TOP
SUM ITERI.IMPPARC TO VTOT1 FOR vNumMes=ALLTRIM(ITERI.NumMes) .AND. vnumRI=ALLTRIM(ITERI.NumRI)
*SET FILTER TO
SELECT (vAli)
SET ORDER TO vOrd
RETURN vTot1


*- 5 Codigo auxiliar
FUNCTION v_aux
*-------------
 _ALI_ = ALIAS()
 resx  = .F.
 IF m.TipAux='05' OR m.TipAux='25' OR m.TipAux='20'
    vAli=ALIAS()
    vOrd=ORDER()
    SELECT AUXIL
    SET ORDE TO AUXIL1
    vReturn = Val_fun('AUXIL','Codigo',"Codigo+' '+Descri",CodPrv,1,7,20,'SUBSTR(descri,1,22)')
    SELECT (vAli)
    SET ORDE TO vOrd
    RETURN vReturn
 ELSE
    IF EMPTY(m.CodPrv)
       xaux = SPACE(30)  && Temporalmente guarda la descripci¢n
       SELECT Auxi
       vidx = SYS(3) + '.idx'
       INDEX ON Descri TO &vidx FOR m.Tipaux = Tipo
       resx = val_fun('Auxi','Descri','descri',xaux,1)       
        SELECT Auxi
       SEEK xAux
       xaux = Auxi.Codigo
       SET INDEX TO
       SET ORDER TO TAG AUXIL1
       ERASE (vidx)
       SELECT RI
       IF resx
          REPLACE CodPrv WITH xaux
       ENDIF
    ELSE
       SELECT Auxi
       IF !SEEK(m.tipaux + RI.CodPrv)
          IF yesno("Auxiliar no existe. ¨Crea nuevo?")
             IF ap_aux(m.Registro)
                resx = .T.
             ENDIF
          ENDIF
       ELSE
          resx = .T.
       ENDIF
    ENDIF
 ENDIF

 SELECT auxi
 SEEK m.tipaux + SUBSTR(RI.CodPrv,1,6)
 @ 4,68 SAY SUBSTR(auxi.descri,1,18)

 SELECT (_ali_)

RETURN resx


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
 m.Tipo   = RI.TipAux
 m.Codigo = RI.CodPrv
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
    SELECT RI
    RETURN .F.
 ENDIF

 RELEASE WINDOW ap_aux
 SELECT RI

RETURN .T.


FUNCTION IGUAL
*-------------
vAli=ALIAS()
vOrd=ORDER()
SELE RI
SEEK M.PERIODO+ALLTRIM(m.NumMes)+ALLTRIM(m.NumRI)
IF FOUND()
   DO StandBy WITH "El Documento YA EXISTE"
   SELECT (vAli)
   SET ORDER TO vOrd
   RETURN .F.
ENDIF
SELECT (vAli)
SET ORDER TO vOrd
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
  ON KEY LABEL F2    
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    WINDOW wind_c1
  RELEASE    MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

FUNCTION Val_AUXI
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol
  PRIVATE mAlias
   DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 4
      mLong = 25               && Longitud campo DESCRI
      mTIP  = 24
      mDist = 10
    CASE PARAMETERS() = 5
      mDist = 10
  ENDCASE
  mAlias  = ALIAS()
  velig=.f.
  SELECT AUXI
  SEEK ALLTRIM(Filtro)+mValor
  IF !FOUND() .AND. !mVariable $'VZ'
     SET FILTER TO TIPO = ALLTRIM(Filtro)
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    vidx = SYS(3) + '.idx'
*   INDEX ON Descri TO &vidx FOR TIPO = ALLTRIM(Filtro)
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    mTipo  = AUXI.Tipo
  	mValor = AUXI.Codigo
  	mDescr = SUBSTR(AUXI.Descri, 1, mLong)
  	velig=.t.
    SET INDEX TO
    SET ORDER TO TAG AUXIL1
    ERASE (vidx)
  ENDIF
  if velig=.f.
   	mTipo  = AUXI.Tipo
  	mValor = AUXI.Codigo
  	mDescr = SUBSTR(AUXI.Descri, 1, mLong)
  endif	
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
    * @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
      m.CodPrv=mvalor
      m.TipAux=mtipo
*     @ ROW(),mCol-mtip  SAY mvalor  &&mtipo
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE

Function val_resp
*----------------
parameter vcampo
IF empty(vcampo)
   DO StandBy WITH "Ingrese 05 ¢ 20 ¢ 25 para ver Prove/Cont. y 03 Trabajador"
   _Curobj=objnum(vcampo)
else
 IF VCAMPO#'05'
 	 IF VCAMPO#'20'
  	 	IF VCAMPO#'25'
 			IF VCAMPO#'03' 	 	
  	 			DO StandBy WITH "Ingrese 05 ¢ 20 ¢ 25 para ver Prove/Cont. y 03 Trabajador"
  	 			_Curobj=objnum(vcampo)
  	 		endif	
  	 	endif
  	 endif
  endif 	
endif  
RETURN

FUNCTION Val_IngE
*---------------
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
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO CodIng >= Filtro
      ENDIF
      GO TOP
      IF EOF() 
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CODING)+'.'+ALLTRIM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng +'.'+ IngR.SubIng
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol   SAY mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    CASE mVariable='T'
      &mVariable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      WCODPART=MVALOR
      RETURN .T.
  ENDCASE

PROCEDURE COMPRE
*---------------
AS=ALIAS()
m.valdeb = m.CanRI
m.valhab = m.CanRI
IF wcodpart=" "
   return
else   
do case
case subs(m.CODCAL,5,3)='PRP'
	m.CtaDeb = '9120100000'
	m.CtaHab = '9020100000'
case subs(m.CODCAL,5,3)='TRN'
	m.CtaDeb = '9140100000'
	m.CtaHab = '9040100000'
 OTHERWISE
    WAIT "Corrija, el documento ingresando la Cod.Program tica..." window 
   RETURN	
ENDCASE
AS=ALIAS()
USE astpre   IN  9  ORDER TAG astpre6  ALIAS astpre  
SELECT astpre
SEEK 'D'+M.PERIODO+ALLTRIM(m.nummes)+ALLTRIM(m.numri)+ALLTRIM(M.CTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE PERIODO WITH M.PERIODO,nummes WITH m.nummes,tipdoc WITH 'R/I',numref WITH m.numri , cuenta WITH m.ctadeb, tipo WITH 'D',fecref WITH m.fecri,CodCtc WITH ALLTRIM(m.CodCtc),CodCal WITH m.CodCal,codpart WITH Wcodpart
	ENDIF
	UNLOCK
ENDIF
IF RLOCK()
	REPLACE ctadeb WITH m.ctadeb ,ctahab WITH SPACE(10), valdeb WITH m.valdeb ,valhab WITH 0,codcal WITH m.codcal, FecRef WITH m.FecRI,CodCtc WITH ALLTRIM(m.CodCtc)
ENDIF
USE IN 9
SELE (AS)
USE astpre   IN  9  ORDER TAG astpre6  ALIAS astpre  
SELECT astpre
SEEK 'H'+M.PERIODO+ALLTRIM(m.nummes)+ALLTRIM(m.numri)+ALLTRIM(M.CTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE PERIODO WITH M.PERIODO,nummes WITH m.nummes,tipdoc WITH 'R/I',numref WITH m.numri , cuenta WITH m.ctahab, tipo WITH 'H',fecRef WITH m.fecri,CodCtc WITH ALLTRIM(m.CodCtc),CodCal WITH m.CodCal,codpart WITH Wcodpart
	ENDIF
	UNLOCK
ENDIF
IF RLOCK()
	REPLACE ctadeb WITH SPACE(10),ctahab WITH m.ctahab , valdeb WITH 0 ,valhab WITH m.valhab,codcal WITH m.codcal, FecRef WITH m.FecRI,CodCtc WITH ALLTRIM(m.CodCtc)  
ENDIF
USE IN 9
SELE (AS)
ACTIVATE WINDOW wind_6
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY m.ctadeb PICTURE '!!!!!!!!!!' 
@ 01,18  SAY m.valdeb PICTURE '999,999,999.99' 
@ 02,12  SAY m.ctahab PICTURE '!!!!!!!!!!' 
@ 02,34  SAY m.valhab PICTURE '999,999,999.99' 
WAIT ' '
DEACTIVATE WINDOW wind_6
vCtadeb = m.ctadeb
vValdeb = m.Valdeb
vCtahab = m.ctahab
vValhab = m.valhab
ENDIF
RETURN

function dismem
SET MEMOWIDTH TO 100
return

FUNCTION GLOSA
PUBLIC _q
SET MEMOWIDTH TO 60
_Q=UPPER(ALLTRIM(MLINE(RI.Observ,3)))
RETURN _q
