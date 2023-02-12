*-------------------------------------------------------------------------
* OrdSer.Prg
* Registra el Orden de Servicio
* Estado :
*   '00' Emitido   Este es el que se registra en el Solicitud de Cotizaci¢n
*   '20' afectado
*   '50' Atendido
*   '70' Devuelto
*   '99' Anulado
* Autor: Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
SET EXCLUSIVE OFF
USE OrdSer   IN  1  ORDER TAG OrdSer1  ALIAS OrdSe
USE Solser   IN  2  ORDER TAG Solser1  ALIAS Solser
USE Parmae   IN  3  ORDER TAG Parmae1  ALIAS Parma
USE Promae   IN  4  ORDER TAG Promae1  ALIAS Promae
USE Calen    IN  5  ORDER TAG calen4   ALIAS calen
USE maepre   IN 10  ORDER TAG maepre1  ALIAS maepre
USE HojCon   IN 11  ORDER TAG HojCon1  ALIAS Hoja
USE Cheque   IN 12  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag
USE ITEHC    IN 14  ORDER TAG ITEHC1   ALIAS ITEHC
USE ITEOS1   IN 16  ORDER TAG ITEOS11  ALIAS ITEOS1
USE USUARIO  IN 20  ORDER TAG USUARIO1 ALIAS USU

PUBLIC vMes,CON,CONY,CONX,VMS,v_reg,vcadena,GH
SET MEMOWIDTH TO 56
*- Mensajes de aviso al usuario
Vmens01 = ' Orden de Servicio : REVISION '
Vmens02 = ' Registro de Ordenes de Servicio '
Vmens04 = 'Dicho Orden de Servicio no fue encontrado'
Vmens05 = 'No existe Orden de Servicio anterior'
Vmens06 = 'No existe Orden de Servicio siguiente'
Vmens07 = '¨ Desea ANULAR ‚ste Orden de Servicio ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Orden de Servicio ha sido anulado'
Vmens10 = 'Este Orden de Servicio ya fue atendido'
Vmens11 = 'Este Orden de Servicio ha sido devuelto'

on key label F2 do VisObs
*on key label F4 do imprimir
*on key label F6 do liquidar
SELECT OrdSe
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

 DEFINE WINDOW WINDO_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

 DEFINE WINDOW Wind_2 FROM 12,04 TO 18,75 DOUBLE ;
 TITLE '®F5¯ Agrega  ° ®F8¯ Eliminar  ° ®F10¯ Terminar ' COLOR SCHEME 10

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
ACTIVATE WINDOW WINDO_0
CLEAR

 @  1, 2 SAY "       Numero O/S :"
 @  1,40 SAY "       N£mero S/S :"
 @  2, 2 SAY "        Fecha O/S :"
 @  3, 2 SAY "              H/C :"
 @  4, 2 SAY "        Proveedor :"

 @  5, 2 SAY "        Cad.Func. :"
 @  6, 2 SAY " Fte. Financiami. :"
 
 @  7, 2 SAY "          Funci¢n :"
 @  8, 2 SAY "         Programa :"
 @  9, 2 SAY "      Subprograma :"
 @ 10, 2 SAY "   Activ./Proyec. :"

 @ 11, 2 SAY "       Calendario :"
 @ 12, 2 SAY "      Dependencia :"
 
 @ 13, 2 SAY "          Destino :"
 @ 14, 2 SAY "      Descripci¢n :"

 @ 20, 2 SAY "          Importe :"
 @ 20,40 SAY "         Contrato :" 
RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
* ON KEY LABEL F6 DO LIQUIDAR
* ON KEY LABEL F7 DO ESTADOS
 SELECT ORDSE
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WINDO_0

 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo+'01001','C') 

 @  0,02 SAY IIF(EMPTY(m.User),space(50),'     Elaborado por: ')+user COLOR SCHEME 02
 @  0,60 SAY vEstOc(m.estado) COLOR SCHEME 02
 @  1,40 SAY SPACE(40) 
 @  1,22 SAY m.periodo
 @  1,24 SAY '-'
 @  1,25 SAY m.NumOS
 @  1,40 SAY IIF(EMPTY(m.Memoran),"       N£mero S/S :","       Memorandum :")
 @  1,60 SAY IIF(EMPTY(m.Memoran),m.Periodo+'.'+m.Numss,subst(m.Memoran,1,18))
 
 @  2,22 SAY m.FecOS
 @  2,60 SAY IIF(m.estado='50',fecliq,'        ')
 @  3,22 SAY m.Perhc+' '+m.Numhc
 @  4,22 say space(56)
 @  4,22 SAY m.CodPrv
 @  4,27 SAY val_prv(m.CodPrv) 
 
 @  5,22 SAY val_codcad(m.codcad,m.periodo+'01001','D',22,30)
 @  6,22 SAY val_para(m.CodFte,'CODFTE','D',22,30)

 @  7,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
 @  8,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
 @  9,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
 @ 10,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)
 
 @ 11,22 SAY val_para(m.NumMes,'FECMES','D',22,40)
 @ 12,22 SAY val_para(m.CodDep,'CODDEP','A',22,56)

 @ 13,22 SAY m.destino picture "@s56"
 @ 14,22 clear to 18,79
 
 @ 14,22 SAY MLINE(Detalle,1) 
 @ 15,22 SAY MLINE(Detalle,2) 
 @ 16,22 SAY MLINE(Detalle,3) 
 @ 17,22 SAY MLINE(Detalle,4) 
 @ 18,22 SAY MLINE(Detalle,5) 

 @ 20, 2 SAY "          Importe :"
 @ 20,40 SAY "         Contrato :" 
 
 @ 20,22 SAY m.ValTot   PICTURE '999,999.99'
* @ 20,22 SAY SPACE(40)
 @ 20,60 SAY IIF(EMPTY(m.ConCon),'       ',m.NumCon)

 @ 21,00 SAY PADC('° ®F2¯ Detalle O/S °  ®F4¯ Imprime ° ®F6¯  Liquida °  ®F7¯ Seguimiento °',79,' ') COLOR W+/B
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELECT OrdSe
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
 BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumOs  :H=' N§ ' ,;
 CodFte :h='Fte' ,;
 AA=vEstOc(estado) :H='Estd':4 ,;
 FecOs  :H='Fecha' ,;
 CodDep :H='DEP',;
 valtot :H='Total':p='999,999.99' ,;
 ZZ=MLINE(DETALLE,1)  :H='Descripci¢n'

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
 vNum_OS  = 0
 vNumos = '0000'
 VFTE = '  '
 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/S : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNumOS PICTURE '!!!!' 
 vNum_os = ALLTRIM(vNumos)
 READ
 DEACTIVATE WINDOW standby
 IF EMPTY(vNum_OS) .or. LASTKEY()=27
    RETURN
 ELSE
*  SEEK  vPeriodo + vNum_OS + ALLTRIM(VFTE)
   SEEK  vPeriodo + vNumOS &&+ ALLTRIM(VFTE)
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
vNum_OS=Padl(alltrim(str(vNum_OS,4)),4,'0')
retur .t.

PROCEDURE vBusca1
*---------------
vNumOS=Padl(alltrim(vNumOS),4,'0')
retur .t.

PROCEDURE Anter
*--------------
SELE ORDSE
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
SELE ORDSE
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
PRIVATE VNUM
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
         DO STANDBY WITH 'El O/S ya esta Afectado'
         RETURN
	CASE Estado = '50'
         * El Orden de S ya Liquida ?
         DO STANDBY WITH 'El O/S ya est  liquidado'
         RETURN
	CASE Estado = '51'
         * El Orden de Compra ya tiene ?
         DO STANDBY WITH 'El O/S ya est  Contabilizado'
         RETURN
 ENDCASE

 SELECT OrdSe
 SCATTER MEMVAR
 VNUM = RECNO()
 ACTIVATE WINDOW WINDO_0
 DO PANTALLA
 vSino = iif(!empty(m.numcon),'S','N')
 =val_CODCAD(ALLT(m.codcad),m.periodo+'01001','C') 
 vcodanal = m.codanal

 SELECT ORDSE
 IF RLOCK() OR F_LOCK(1)
    @ 1,22 GET m.Periodo  PICTURE '!!' DISABLE
    @ 1,24 SAY '-'
    @ 1,25 GET m.NumOs   PICTURE '!!!!' &&DISABLE
	@ 1,60 SAY IIF(EMPTY(m.Memoran),m.Periodo+'.'+m.Numss,subst(m.Memoran,1,18))
    @ 2,22 GET m.FecOs   PICTURE '@D'

    @ 4,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27) AND SIPRV()

    @ 5,22 GET m.codcad  PICTURE '!!!!' VALID VAL_CODCAD(m.codcad,m.periodo+'01001',' ',22,30) 
    @ 6,22 GET m.CodFte  PICTURE '!!'   VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,30)
	READ VALID val_read()

	IF LASTKEY()=27 
		DO vista
		RETURN
	ENDIF

	@  7,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
	@  8,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
	@  9,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
	@ 10,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

    @ 11,22 GET m.Nummes PICTURE '!!'     VALID VAL_PARA(m.Nummes,'FECMES',' ',22,20)
    @ 12,22 GET m.CodDep PICTURE '!!!!!!' VALID VAL_PARA(m.CodDep,'CODDEP',' ',22,30,7)
  
    @ 20,22 GET m.ValTot  PICTURE '999,999.99'
       
    @ 20,60 GET vSino PICTURE '!' VALID  vSino $'NS'
    @ 20,62 GET m.NumCon  WHEN vSino='S'
    
    READ VALID VAL_READ()  
    
    SELE Ordse
    vImpr = m.Periodo+m.NumOs+ALLTRIM(m.Codfte)
    IF LASTKEY() # 27
       vcadena=maepre.uniges+maepre.unieje+maepre.Codfun+maepre.CodPrg+maepre.CodSpr+maepre.ActPry
       IF vSino='N'
          m.NumCon = space(10)
       ELSE
          SELECT PARMA
          SEEK 'CORREL'+'NUMCON'
          REPLACE NumEnt WITH NumEnt + 1
       ENDIF
       DO ITESER
       SELECT Ordse
       GO VNUM
	   m.user = sys(0)
       m.user_fc = date()
       m.user_TP = 'C'
       M.ESTADO='00'
       GATHER MEMVAR
       OK = DETALLE()
       *SELEC CALEN
       *SEEK m.periodo+vcadena+iteos1.codcom+iteos1.codmet+allt(m.codfte)+allt(m.nummes)+iteos1.codpart
       *if !found()
       *		do standby with 'No existe partida en calendarios...'
       *else	
       *     REPLACE totos WITH totos+ITEOs1.valPart
       *endif	
    ELSE
      DO STANDBY WITH 'Proceso cancelado'
      SELE ORDSE
      GO TOP 
      GO VNUM
    ENDIF
  ENDIF
  UNLOCK ALL
  SELECT Ordse
  DO Vista  
  RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
sincot=.t.
 
IF escolor
   DEFINE POPUP xcot  FROM 17,55 SHADOW COLOR &L_COL
ELSE
   DEFINE POPUP xcot  FROM 17,55 COLOR SCHEME c_popup
ENDIF
 
DEFINE BAR 1 OF xcot PROMPT ' \<Sin Cotizaci¢n '
DEFINE BAR 2 OF xcot PROMPT ' \<Con Cotizaci¢n '
DEFINE BAR 3 OF xcot PROMPT ' c\<On otro docmto'
 
ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP XCOT
 
DO CASE
   CASE BAR() = 1
		sincot=.t.
		conmem=.f.
   CASE BAR() = 2
		sincot=.f.
		conmem=.f.
   CASE BAR() = 3
		sincot=.f.
		conmem=.t.
   OTHERWISE
ENDCASE
RELEASE POPUP Xcot

IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado'
   DO VISTA
   RETURN
ENDIF
   
DO PANTALLA
SELECT OrdSE
SCATTER MEMVAR BLANK
ASD = RECNO()
m.FecOs  = date()
m.periodo = str(year(DATE())-1900,2)
vNumss = '    '
vAno = str(year(DATE())-1900,2)
vSino = 'N'
vnumcon = space(6)

@ 13,22 clear to 17,79 

@  1,22 GET m.Periodo   PICTURE '!!' valid v_numos()
@  1,24 SAY '-'
@  1,25 GET m.NumOs     PICTURE '!!!!' valid validar()

IF CONMEM
	@ 1,40 SAY " Documento Fuente :" get m.Memoran PICTURE '@S18'
ELSE	
   @ 1,40 SAY "       N£mero S/S :" get vano
   @ 1,62 SAY '.'
   @ 1,63 GET vNumss PICTURE '!!!!' VALID V_SER()
ENDIF 
@  2,22 GET m.FecOs     PICTURE '@D'
@  4,22 GET m.CodPrv    PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,28) AND SIPRV()

@  5,22 GET m.CodCad    PICTURE '!!!!' VALID VAL_CODCAD(m.codcad,m.periodo+'01001',' ',22,30)
@  6,22 GET m.CodFte    PICTURE '!!'   VALID VAL_PARA(m.CodFte,'CODFTE',' ',22,30)
READ VALID Val_Read()

IF LASTKEY()=27 
	DO vista
	RETURN
ENDIF

@  7,22 SAY VAL_PARA(MAEPRE.codfun,'CODFUN','V',22,40)
@  8,22 SAY VAL_PARA1(MAEPRE.CodPrg,'CODPRG'+MAEPRE.codfun,'V',22,40)
@  9,22 SAY VAL_PARA1(MAEPRE.CodSPr,'CODSPR'+MAEPRE.codprg,'V',22,40)
@ 10,22 SAY VAL_PARA(MAEPRE.actpry,'ACTPRY','V',22,40)

@ 11,22 GET m.Nummes    PICTURE '!!'     VALID VAL_PARA(m.Nummes,'FECMES',' ',22,20)
@ 12,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40)
 
@ 20,22 GET m.ValTot   PICTURE '999,999.99'

@ 20,60 GET vSino PICTURE '!' VALID vSino $'NS' &&Del Contrato
@ 20,62 GET vNumCon WHEN vSino='S'
 
READ VALID VAL_READ()  

SELE Ordse
IF LASTKEY() # 27
   vcadena=maepre.uniges+maepre.unieje+maepre.Codfun+maepre.CodPrg+maepre.CodSpr+maepre.ActPry
   m.fecemi=date()
   m.Concon = vsino
   m.NumCon = iif(vsino='N','     ',vNumCon)
   m.Estado = '00'
   vImpr = m.Periodo+m.NumOs+ALLTRIM(m.Codfte)

   DO ITESER
   SELECT Ordse
   vSS = m.Periodo+m.Numss
   m.user = sys(0)
   m.user_fc = date()
   m.user_TP = 'I'

   IF F_Appd()
	  GATHER MEMVAR
      IF !CONMEM	      
		 SELECT Solser
         seek vAno+vNumss
         SELECT Ordse           
         REPLACE detalle with solser.detalle
      endif   
   endif    
   
   OK = DETALLE()
   *- Aumento el correlativo
   SELECT Parma
   SEEK 'CORREL'+'ORDENS'
   REPLACE NumEnt WITH NumEnt + 1
   IF vSino='S'
      SEEK 'CORREL'+'NUMCON'
      REPLACE NumEnt WITH NumEnt + 1
   ENDIF
   
   SELECT SOLSER
   seek vSS
   if found()
	  IF RLOCK()
         REPLACE ESTADO WITH '30',NUMOS WITH ORDSE.NUMOS,PEROS WITH ORDSE.PERIODO
	  ENDIF
      unlock
   endif
   SELECT iteos1
   SEEK m.periodo + m.numos
   *SELEC CALEN
   *SEEK m.periodo+vcadena+iteos1.codcom+iteos1.codmet+allt(m.codfte)+allt(m.nummes)+iteos1.codpart
   *if !found()
   *		do standby with 'No existe partida en calendarios...'
   *else	
   *     REPLACE totos WITH totos+ITEOs1.valPart
   *endif	
ELSE
   DO STANDBY WITH 'Proceso cancelado'
   SELE ORDSE
   GO ASD

*   GO GH
*   if numss=alltrim(m.numss)
*      dele next 1
*   endif   
*   GO REC
ENDIF
UNLOCK ALL
SELECT OrdSe
DO Vista
RETURN

procedure v_numos
*----------------
 SELECT Parma
 SEEK 'CORRELORDENS'
 = valOs ( Parma.NumEnt + 1 )
 SELECT ORDSE
 return

PROCEDURE VALMES
*---------------
IF alltrim(m.Nummes)#vmes
   do standby WITH 'No Coincide con Mes del Calendario'
endif   
return

PROCEDURE V_SER
*--------------
PRIVATE AS,VFUN
VFUN = .T.
AS=ALIAS()
SELECT SOLSER
vtemp = RECNO()
vNumSs=Padl(alltrim(vNumSs),4,'0')
seek vAno+vNumss
if !found()
   HIDE MENU mMenu
   ACTIVATE SCREEN
   vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   ON KEY LABEL F10 KEYBOARD CHR(23)
   IF SINCOT
     SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99'
   ELSE
     SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99'
   ENDIF
   GO TOP
   IF !EOF()
     BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
     NumSs  :H=' N§ ',;
     Periodo :H='A¤',;
     Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
     FecSs  :H='Fecha' ,;
     CodDep :H='DEP.',;
     CodFte :H='FTE.',;
     XX=MLINE(DETALLE,1)  :H='Descripci¢n'
     vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
     DO Logos WITH Rotulo1,vTempo
     IF LASTKEY()=27
        GOTO Vtemp
     ENDIF
   ELSE
     DO STANDBY WITH 'No existe Solicitud de Servicio a Procesar '
     RETURN .F.
   ENDIF
   SHOW MENU mMenu
   SET FILTER TO
   ON KEY LABEL F10
 endif
 
 IF !CONMEM
 	IF SOLSER.NUMOS#'    ' &&SOLSER.ESTADO='30'
       DO STANDBY WITH 'La Solicitud de Servicio ya tiene ORDEN '
       return .f.
    endif
    m.detalle = solser.detalle
 ENDIF

 vAno   = solser.Periodo
 vNumss = solser.Numss
 *----
 
 m.codcad  = solser.codcad
 m.Numss   = solser.Numss
 m.CodPrv  = solser.codprv
 m.CodFte  = solser.Codfte
 m.CodDep  = solser.CodDep
 m.DesOs   = solser.Desss
 m.DesOs1  = solser.Desss1
 m.DesOs2  = solser.Desss2
 m.DesOs3  = solser.Desss3
 m.ValTot  = solser.ValTot
 m.Destino = solser.destino
 
 *-----------
 
SELE (AS)
RETURN

function valprv
*--------------
private xx, vfun
vfun = .f.
codprv = iif( empty(codprv),codprv,padl(alltrim(codprv),4,'0'))
xx = val_prv( codprv,.t.)
if xx
   replace codprv with promae.codprv , nompro with promae.nompro
   return .t.
endif
return vfun

PROCEDURE Anula
*--------------
 SELE ordSE
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 IF Estado # '00'
   * ya pas¢
   DO STANDBY WITH Vmens10
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ANULAR ‚sta Orden de Servicio ?')
 IF vElimina
    SELECT SOLSER
    SEEK m.periodo+m.numss
    IF Found() and RLOCK() 
       REPLACE SOLSER.ESTADO WITH '00',NUmOS with '    ',PEROS WITH '  '
    ENDIF
    SELE CALEN 
    REPLA ALL TOTOS WITH 0
    SELE ORDSE
    IF  RLOCK()
       REPLACE ORDSE.ESTADO WITH '99' ,FECVER WITH DATE(),user WITH sys(0),user_fc WITH date(),user_TP WITH 'A'
    ENDIF
    DO Vista
 ENDIF
 UNLOCK ALL
 RETURN

PROCEDURE Lista
*--------------
SELECT ORDSE
SCATTER MEMVAR
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO LISSER
endif
select Ordse
go vtemp
DO VISTA
RETURN

PROCEDURE IMPRIMIR
*-----------------
PRIVATE VCON
SELECT ordse
VCON = RECNO()
SCATTER MEMVAR
vNumOs = m.Nummes+m.Numos
SET relation to PERIODO+m.Numos into iteos1
set filt to Nummes+NumOs=vNumos
if eof()
   do standby with vmens08
   return
else
   do reporte with 2,"LisOsX",' Orden de Servicios ',2
endif
set filter to
SELECT ordse
GO VCON
DO VISTA
RETURN


PROCEDURE lisSER
*---------------
vOrde = ORDER()
DEFINE WINDOW LIS FROM 1,15 TO 23,65 FLOAT DOUBLE TITLE 'Listado Solicitud de Servicios' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue,vtoprv
vCli = SPAC(4)
vAno = '97'
VFTE = '  '
vcodfte = '  '
VPRV='    '
@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ por Documento;en Detalle;Resumido;Control' 

@ 04,01 SAY "        Total O/S : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,5,22) when vlistado =1 
@ 05,01 SAY "              O/S : "
@ 05,22 GET vFte    PICTURE '!!' VALID VAL_PARA(VFTE,'CODFTE','C') WHEN VTOCLI=2 AND VLISTADO=1
@ 05,26 GET vAno    picture '!!'  WHEN VTOCLI=2 and vlistado=1
@ 05,28 SAY '-'
@ 05,29 GET vCli    WHEN VTOCLI=2 and vlistado=1 PICTURE '!!!!' VALID VO() AND ValOrd()
@ 06,01 SAY "Todos Proveedores : " GET vTOprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,7,22) when (vlistado #1 AND VLISTADO#4)
@ 07,01 SAY "    Proveedor O/S : "
@ 07,22 GET vPrv    WHEN VTOprv=2 PICTURE '!!!!' VALID val_prv(vprv,.t.)

@ 09,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,11,22)  WHEN (VLISTADO=2 OR VLISTADO=3 OR VLISTADO=4)
@ 11,01 SAY "           Fuente : "
@ 11,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 AND (VLISTADO=2 OR VLISTADO=3 OR VLISTADO=4)

@ 13,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' WHEN vtocli=1 AND (VLISTADO=2 OR VLISTADO=3 OR VLISTADO=4)

@ 16,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Anulados;Liquidados' WHEN vtocli=1 and (vlistado=2 OR  vlistado=3 OR VLISTADO=4)

@ 20,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8

READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   IF vListado # 4
      INDEX ON  IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
                FOR iif(vtofue=1,.t.,Codfte=alltrim(vCodfte)) .and. ;
	            IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado='00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado='20',iif(vTipPro=5,Estado='99',Estado='50'))))) 
      SET FILTER TO IIF(vToCli =1,.T.,Periodo+NumOs+Codfte = vAno+vCli+ALLTRIM(vFte)) AND iif(vToPrv=2,Codprv=vPrv,.t.)
   ELSE
      INDEX ON  IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
                FOR iif(vtofue=1,.t.,Codfte=alltrim(vCodfte)) .and. busprv()
   ENDIF   
   SET INDEX TO (VIND)
   COUNT ALL TO vTotoS
   GO TOP
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,'Listado Orden Servicio',IIF(vTipPro=2,'Listado Orden de Servicio Pendientes',IIF(vTipPro=3,'Listado Orden de Servicios Afectados',IIF(vTipPro=4,'Listado Orden de Servicios Anulados','Listado Orden de Servicios Liquidados'))))   
   IF !EOF()
     do case 
        case VLISTADO=1
	        DO REPORTE WITH 2,"LisOsX",' Ordenes de Servicios ',2
        case VLISTADO=2
    	 	DO REPORTE WITH 2,"LisOrds",' Ordenes de Servicios ',1,.F.,.T.
        case VLISTADO=3     	
    	 	DO REPORTE WITH 2,"LisOrsX",' Ordenes de Servicios ',1,.F.,.T.        
        case VLISTADO=4
    	 	DO REPORTE WITH 2,"Sercont",' Ordenes de Servicios ',1,.F.,.T.        
     endcase   
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILTER TO
   CLOSE INDEX
   ERASE (VIND)
ENDIF
SELE ORDSE
SET ORDE TO 1
RETURN

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


PROCEDURE VO
*-----------
vCli=Padl(alltrim(vCli),4,'0')
RETURN .T.

PROCEDURE Valord
*---------------
 SELECT ORDSE
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 SEEK VANO+VCLI+ALLTRIM(VFTE)
 
 IF !FOUND()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 NumOs  :H=' N§ ' ,;
 Est = IIF(Estado= '00','Pend',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  ')))) :H='ESTD' ,;
 FecOs  :H='Fecha' ,;
 CodDep :H='DEP.',;
 CodFTE :H='FTE.' ,;
 DESOS  :H='Descripci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 ENDIF
 VCLI=NUMOs
 GOTO Vtemp
 RETURN .t.

PROCEDURE Termi
*--------------
  vEn_accion = .F.
  ON KEY LABEL F2
  ON KEY LABEL F6
  ON KEY LABEL F7
  ON KEY LABEL F4
  DEACTIVATE MENU
  RETURN

PROCEDURE ESTADOS
*-----------------
USE IN 2  
USE IN 4  
USE IN 5  
USE IN 6  
USE Cheque   IN 12  ORDER TAG Cheque1  ALIAS Cheque
USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag

DO Estado WITH 'OS','m.perhc+m.numhc'

*USE IN 12  
*USE IN 13 
USE Solser   IN 2  order tag Solser1  ALIAS Solser
USE Promae   IN 4  order tag Promae1  ALIAS Promae
USE Calen    IN 5  order tag calen1   ALIAS calen
USE Clase    IN 6  order tag clase1   ALIAS clase
RETURN

PROCEDURE Fin_opcion
*-------------------
  CLOSE DATA
  RELEASE WINDOW WINDO_0
  RELEASE MENU   mMenu
  RESTORE SCREEN FROM PRINCIPAL
  RETURN

function valprv
*--------------
private xx, vfun
vfun = .f.
m.codpr = iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.)
if xx
   return .t.
endif
return vfun

function valOS
*-----------------
parameter vnumOS
private vfun
vfun = .t.
m.numOS =padl(alltrim(str(vnumOS,4)),4,'0')
if m.numOS  = '0000' or empty(m.numOS)
   vfun = .f.
endif
return vfun

function valCt
*-----------------
parameter vnumCt
private vfun
vfun = .t.
m.numcon =padl(alltrim(str(vnumct,4)),4,'0')
if m.numcon  = '0000' or empty(m.numcon)
   vfun = .f.
endif
return vfun

PROCEDURE Validar
*----------------
AS=RECNO()
seek m.Periodo+m.NumOs
if found()
   do standby with 'El O/S ya esta registrado'
   GOTO AS
   return .f.
endif
IF !EOF()
   GOTO AS
ENDIF   
return .t.

PROCEDURE LIQUIDAR
*-----------------
PRIVATE OK
OK=VE_PASSW('CHANA')
IF OK
DO CASE
   CASE ESTADO = '00'
        DO STANDBY WITH 'El Documento a£n no est  afectado'
        do vista
        return
   CASE ESTADO = '50'
        DO STANDBY WITH 'El Documento ya est  liquidado'
        do vista
        return
   CASE ESTADO = '99'
        DO STANDBY WITH 'El Documento ya est  Anulado'
        do vista
        return
   CASE ESTADO = '20'
        DO STANDBY WITH 'El Documento ya est  Afectado'
ENDCA            
IF YESNO('Est  seguro de Liquidar este O/S')
   vfecliq=date()
   activate window standby
   @ 1,1 say 'Fecha Liquidaci¢n : ' get vfecliq 
   read
   DEactivate window standby
   IF RLOCK()
      REPLACE ESTADO WITH '50',fecliq with vfecliq,TIPDOC WITH 'OK'
   ENDIF 
else 
   DO STANDBY WITH 'Proceso Cancelado'
endif
do vista
ENDIF
retuRN

procedure cont
*-------------
if yesno('Continua descripci¢n 2?')
   con = .t.
else
   con = .f.
endif 
retu 

procedure conx
*-------------
if yesno('Continua descripci¢n 3?')
   conx = .t.
else
   conx = .f.
endif 
retu 

procedure cony
*-------------
if yesno('Continua descripci¢n 4?')
   cony = .t.
else
   cony = .f.
endif 
retu 

procedure iteSER
*---------------
PRIVATE AZ
aZ=alias()
select SOLSER
seek ORDSE.periodo+ORDSE.numSS
m.destino = IIF(EMPTY(m.destino),SOLSER.DESTINO,m.destino)
sele (aZ)
ACTIVATE WINDOW wind_1
@ 0,0 SAY 'Destino: ' get m.destino picture '@S73'
READ
DEACTIVATE WINDOW wind_1
do trab_hijo
RETURN

FUNCTION DETALLE
*---------------
vAlias = ALIAS()

ON KEY LABEL F10 KEYBOARD CHR(23)
IF !WEXIST("Detalle")
   DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Descripci¢n del O/S ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle SAME
ELSE
   ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE WINDOW Detalle

IF !WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle
ENDIF

RELEASE WINDOW Detalle
IF LASTKEY()=27
   DO STANDBY WITH 'Proceso cancelado. No graba el Detalle '
ENDIF
SELECT (vAlias)
RETURN .T.

FUNCTION VisObs
*--------------
vAlias = ALIAS()
IF !WEXIST("Detalle")
   DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Descripci¢n del O/S ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle SAME
ELSE
   ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW Detalle

IF !WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle
ENDIF
RELEASE WINDOW Detalle
SELECT (vALIAS)
RETURN .T.

PROCEDURE VALCCC
*---------------
PRIVATE AS,vnumccc
USE SOLCOT  IN 13  ORDER TAG Solcot2  ALIAS solcot
USE IteSC   IN 14  ORDER TAG Itesc1   ALIAS Itesc
USE IN 15

AS=ALIAS()
VCC=ORDSE.NUMSS
SELEct ITESC
SET RELA TO PERIODO+NUMSC INTO SOLCOT
SET ORDE TO ITESC1
SEEK VCC
VNUMCCC=IIf(FOUND()," CC/C :"+SOLCOT.NUMCCC,'   ')
set rela to
SELEct (as)

USE Compag   IN 13  ORDER TAG Compag1  ALIAS compag

return vnumccc


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

PROCEDURE siprv
*--------------
parameter vfun
vfun = .t.
if !v_reg='VG'
   do standby with 'El Proveedor no esta Regularizado...Observaci¢n'
   * vfun .f.
endif
return vfun   

PROCEDURE Trab_hijo            && Revisi¢n de BD en browse CUANDO # O/?
*------------------
as    = ALIAS()
vTemp = RECNO()
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°F5->Agregar°°°°°°°°°°F8->Eliminar°°°°°°°°°°°F10->Terminar°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
ON KEY LABEL F5  DO Agre_itos
ON KEY LABEL F8  DO elim_itos
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteos1
SET ORDE TO ITEOS11
seek m.periodo +m.Numos
if !found()
   DO Agre_ItOS
else
   DO Borra_Os
endif

BROWSE WINDOW Wind_2 NOAPPEND NODELETE NOMENU key m.periodo +m.Numos FIELDS ;
       CODCOM	: H= 'Componente.':V=Val_comp(m.periodo+maEPRE.uniges+maepre.unieje+m.codcad,codcom,'codcom'):F ,;
       CODMET   : H= 'Meta' :V=Val_meta(m.periodo+maEPRE.uniges+maepre.unieje+m.codcad,codcom+codmet,'codmet'):F ,;
       codPart  : H= 'Partida' :F ,;
       aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,40)) :H='Descripci¢n':40,;
       ValPart  :H='Monto' :P='99,999,999.99'

vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo

* codPart  : H= 'Partida' :V=VAL_cale(codpart,m.periodo+vcadena+codcom+codmet+allt(codfte)+allt(m.nummes),'codpart'):F ,;

SELECT iteos1
seek m.periodo +m.Numos
vtotcom = 0
SCAN WHILE PERIODO+NUMOS=m.periodo +m.Numos
	vtotcom = vtotcom + valpart
	IF VALPART=0
		DELETE NEXT 1
	ENDIF
ENDSCAN	
IF vtotcom <> m.ValTot
	DO STANDBY WITH ('Total Compon. es diferente al total de  O/S...Revise...')
ENDIF	
SHOW MENU mMenu
ON KEY LABEL F5
ON KEY LABEL F7
ON KEY LABEL F10
SET FILTER TO
SELE (AS)
if lastkey()=27
   return .f.
endif
RETURN .t.

PROCEDURE Agre_ItOS
*-------------------
sele iteos1
vp = codpart
vuniges = MAEPRE.uniges
vunieje = MAEPRE.unieje
vcodcom = MAEPRE.codcom
vcodmet = MAEPRE.codmet
IF F_appd()
   REPLACE Periodo WITH m.Periodo ,;
   		  Numos   WITH m.Numos ,;
          CodCad  WITH m.CodCad,;
          Uniges  WITH vuniges,;
          UniEje  WITH vunieje,;
          Codcom  WITH vCodcom,;
          Codmet  WITH vCodmet,;
          Codpart WITH Vp ,;
          valpart WITH Iteos1.valpart
ENDIF
RETURN .T.

PROCEDURE elim_itos
*-------------------
sele iteos1
if rlock()
   delete next 1
endif
return

PROCEDURE Borra_os            && Borra los items de o/s
*----------------
AX=ALIAS()
SELECT IteOs1
SEEK m.Periodo + m.NumOs
if rlock()
   SCAN WHILE Periodo+NumOs=m.Periodo+m.NumOs
   		* Elimina las asignaciones efectuadas en el calendario
   		SELECT CALEN
        SEEK m.periodo+vcadena+iteos1.codcom+iteos1.codmet+allt(m.codfte)+allt(m.nummes)+iteos1.codpart
   		REPLACE TotOs WITH TotOs-iteos1.Valpart
		SELECT ITEos1
   ENDSCAN
endif
sele (AX)
retur
