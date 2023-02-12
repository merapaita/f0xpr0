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
* Soporte y Actualizaci¢n : GUSWALEN
*--------------------------------------------------------------------------
*- Abriendo Archivos
Parameter sistema,vopcion
USE OrdSer   IN  1  ORDER TAG OrdSer1  ALIAS OrdSe
USE Solser   IN  2  ORDER TAG Solser1  ALIAS Solser
USE Parmae   IN  3  ORDER TAG Parmae1  ALIAS Parma
USE Promae   IN  4  ORDER TAG Promae1  ALIAS Promae
USE Calen    IN  5  ORDER TAG calen4   ALIAS calen
USE AsiAut   IN  0  ORDER TAG AsiAut1

IF sistema='1'
	USE maepre   IN 10  order tag maepre1  ALIAS maepre
ELSE
	USE maepre   IN 10  order tag maepre3  ALIAS maepre
ENDIF	
USE HojCon   IN 11  ORDER TAG HojCon1   ALIAS Hoja
USE Cheque   IN 12  ORDER TAG Cheque1   ALIAS Cheque
USE Compag   IN 13  ORDER TAG Compag1   ALIAS compag
USE ITEHC    IN 14  ORDER TAG ITEHC1    ALIAS ITEHC
USE ITEOS1   IN 16  ORDER TAG ITEOS11   ALIAS ITEOS1
USE AstOrd	 IN 15	ORDER TAG AstOrd1   ALIAS AstOrd
USE USUARIO  IN 20  ORDER TAG USUARIO1  ALIAS USU
USE IteUsuOp IN 0   ORDER TAG IteUsuOp1 ALIAS SubOp
USE CatAsi   IN 0   ORDER TAG CatAsi4   ALIAS CatAsi

PUBLIC vMes,CON,CONY,CONX,VMS,v_reg,vcadena,GH,vtipord
SET MEMOWIDTH TO 56
*- Mensajes de aviso al usuario
vtipord = ' '
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
on key label F4 do imprimir
on key label F6 do liquidar
ON KEY LABEL F12 DO CORRI1
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
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
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD anula   OF mMenu PROMPT 'a\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
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

 @ 19, 2 SAY "          Importe :"
 @ 19,40 SAY "         Contrato :" 
 @ 20, 2 SAY "             IGV? :   " 
RETURN



PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ON KEY LABEL F6 DO LIQUIDAR
 ON KEY LABEL F7 DO ESTADOS
 SELECT ORDSE
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW WINDO_0

 SCATTER MEMVAR
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C') 

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
* @  4,27 SAY Val_prv( m.CodPrv,.T.,4,27)  and siprv() and promae.estado="VG"
 @  4,27 SAY Val_prv( m.CodPrv,.T.,4,27)  
 *and siprv()  and promae.estado="RG"
 
 @  5,22 SAY val_codcad(m.codcad,m.periodo,'D',22,30)
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

 @ 19, 2 SAY "          Importe :"
 @ 19,40 SAY "         Contrato :" 
 @ 20,2  SAY "             IGV? :  "
 
 @ 19,22 SAY m.ValTot   PICTURE '999,999.99'
* @ 20,22 SAY SPACE(40)
 @ 19,60 SAY IIF(EMPTY(m.ConCon),'       ',m.NumCon)
 @ 20,24 SAY m.IGV PICTURE "9999.99"
 @ 21,00 SAY PADC('° ®F2¯ Detalle O/S °  ®F4¯ Imprime ° ®F6¯  Liquida °  ®F7¯ Seguimiento °',79,' ') COLOR W+/B
 IF !vFlag$'J*'
	DO SubOpc
 ENDIF
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
 vNum_OS  = '    '
 VFTE = space(2)

 ACTIVATE WINDOW standby
 @ 1,01 SAY 'Ingrese N£mero O/S : ' GET vPeriodo PICTURE '!!'
 @ 1,26 SAY '-' GET vNum_OS PICTURE '!!!!' valid vBusca()
 READ
 DEACTIVATE WINDOW standby

 IF EMPTY(vNum_OS) .or. LASTKEY()=27
    RETURN
 ELSE
   SEEK  vPeriodo + vNum_OS
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
vNum_OS=Padl(alltrim(vNum_OS),4,'0')
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
PRIVATE VNUM,nIgv
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
 vIgv="S"
 SELECT OrdSe
 SCATTER MEMVAR
 IF EMPTY(m.igv)
 	vIGV = "N"
  ELSE
 	vIGV = "S"
 ENDIF
 VNUM = RECNO()
 ACTIVATE WINDOW WINDO_0
 DO PANTALLA
 vSino = iif(!empty(m.numcon),'S','N')
 =val_CODCAD(ALLT(m.codcad),m.periodo,'C') 
 vcodanal = m.codanal

 SELECT ORDSE
 IF RLOCK() OR F_LOCK(1)
    @ 1,22 GET m.Periodo  PICTURE '!!' DISABLE
    @ 1,24 SAY '-'
    @ 1,25 GET m.NumOs   PICTURE '!!!!' DISABLE
	@ 1,60 SAY IIF(EMPTY(m.Memoran),m.Periodo+'.'+m.Numss,subst(m.Memoran,1,18))
    @ 2,22 GET m.FecOs   PICTURE '@D'	VALID Val_Ano()

*    @ 4,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)  and siprv() and promae.estado="VG"
    @ 4,22 GET m.CodPrv  PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)  
    *and siprv() and promae.estado="RG"

    @ 5,22 GET m.codcad  PICTURE '!!!!' VALID VAL_CODCAD(m.codcad,m.periodo,' ',22,30) 
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
  
    @ 19,22 GET m.ValTot  PICTURE '999,999.99'
       
    @ 19,60 GET vSino PICTURE '!' VALID  vSino $'NS'
    @ 19,62 GET m.NumCon  WHEN vSino='S'
	@ 20,22 GET vIgv PICTURE "!" valid vIgv$"SN"
	READ
	IF vIgv="S"
		nIgv = IIF(DTOC(m.fecos,1)>'20110228',1.18,1.19)
		m.igv= ROUND(m.Valtot-(m.Valtot/nIgv),2)
	ELSE
		m.igv=0
	ENDIF
	@ 20,24 GET m.igv PICTURE "9999.99"  WHEN vIgv="S"
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
*	   m.user = vUser_Id
	   m.user_Cr = vUser_Id
*   m.user = sys(0)
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
vtipss= " " 
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
m.FecOs  = m.fecsis
m.periodo = SUBS(DTOC(m.fecsis),9,2)
vNumss = '    '
vAno = SUBS(DTOC(m.fecsis),9,2)
vSino = 'N'
vnumcon = space(6)
vIgv = 'N'
@ 13,22 clear to 17,79

= v_numos() AND validar()

@  1,22 GET m.Periodo   PICTURE '!!'	DiSABLE
@  1,24 SAY '-'
@  1,25 GET m.NumOs     PICTURE '!!!!'	DISABLE
IF CONMEM
	@ 1,40 SAY " Documento Fuente :" get m.Memoran PICTURE '@S18'
 ELSE
	@ 1,40 SAY "       N£mero S/S :" get vano
	@ 1,62 SAY '.'
	@ 1,63 GET vNumss PICTURE '!!!!' VALID V_SER() &&DISABLE
ENDIF
@  2,22 GET m.FecOs     PICTURE '@D' VALID Val_Ano()
*@  4,22 GET m.CodPrv    PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)  and siprv() and promae.estado="VG"
@  4,22 GET m.CodPrv    PICTURE '!!!!' VALID Val_prv( m.CodPrv,.T.,4,27)
*and siprv() and promae.estado="RG"

@  5,22 GET m.CodCad    PICTURE '!!!!' VALID VAL_CODCAD(m.codcad,m.periodo,' ',22,30)
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

@ 11,22 GET m.Nummes    PICTURE '!!'     VALID VAL_PARA(m.Nummes,'FECMES',' ',22,20) AND VAL_MES()
@ 12,22 GET m.CodDep    PICTURE '!!!!!!' VALID Val_parA( m.CodDep,'CODDEP',' ',22,40)
 
@ 19,22 GET m.ValTot   PICTURE '999,999.99'

@ 19,60 GET vSino PICTURE '!' VALID vSino $'NS' &&Del Contrato
@ 19,62 GET vNumCon WHEN vSino='S'
@ 20,22 GET vIgv PICTURE "!" valid vIgv$"SN"
READ
IF vIgv="S"
	nIgv  = IIF(DTOC(m.fecos,1)>'20110228',1.18,1.19)
	m.igv = ROUND(m.Valtot-(m.Valtot/nIgv),2)
ENDIF
@ 20,24 GET m.igv PICTURE "9999.99"  WHEN vIgv="S"
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
   m.user = vUser_Id
*   m.user = sys(0)
   m.user_fc = date()
   m.user_TP = 'I'
   
   IF F_Appd()
	  GATHER MEMVAR
      IF !CONMEM	      
		 SELECT Solser
         SEEK vAno+vNumss
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
   SEEK vSS
	IF FOUND()
		IF F_Lock(1)
			REPLACE ESTADO WITH '30',NUMOS WITH ORDSE.NUMOS,PEROS WITH ORDSE.PERIODO
		ENDIF
		UNLOCK
	ENDIF
	SELECT iteos1
	SEEK m.periodo + m.numos
 ELSE
	DO STANDBY WITH 'Proceso cancelado'
	SELE ORDSE
	GO ASD
ENDIF
UNLOCK ALL
SELECT OrdSe
DO Vista
RETURN

FUNCTION Val_Ano
*---------------
IF YEAR(m.FecOS) = YEAR(m.FecSis)
	mRet = .T.
ELSE
	DO StandBy WITH "Error en la Fecha... Revise."
	mRet = .F.
ENDIF
RETURN mRet


PROCEDURE v_numos
*----------------
SELECT Parma
SEEK 'CORRELORDENS'
m.NumOS = PADL(ALLTRIM(STR(Parma.NumEnt + 1)),4,"0")
SELECT ORDSE
RETURN

PROCEDURE VALMES
*---------------
IF alltrim(m.Nummes)#vmesPRO
	do standby WITH 'No Coincide con Mes del Calendario'
endif   
return

PROCEDURE V_SER
*--------------
PRIVATE AS,VFUN
VFUN = .T.
AS=ALIAS()
SELECT SOLSER   &&190899
vtemp = RECNO()
vNumSs=Padl(alltrim(vNumSs),4,'0')
seek vAno+vNumss
IF FOUND()
   IF !CONMEM
* 	   IF SOLSER.NUMOS#'    ' &&SOLSER.ESTADO='30'
	   IF SOLSER.ESTADO='30'
          DO STANDBY WITH 'La Solicitud de Servicio ya tiene ORDEN '
          return .f.
       ENDIF
          m.detalle = solser.detalle
    ENDIF
ELSE
*SET FILTER TO EMPTY(NUMSS) AND ESTADO='10' 
SET FILTER TO EMPTY(NUMSS) AND ESTADO='00' 
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
*     SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99' AND ESTADO='10'
      SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99' AND ESTADO='00'
   ELSE
     SET FILTER TO EMPTY(NUMOS) AND ESTADO#'99' AND ESTADO='20'
   ENDIF
   GO TOP
   IF !EOF()
     BROWSE WINDOW WINDO_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
     NumSs  :H=' N§ ',;
     Periodo :H='A¤',;
     Est = IIF(Estado= '00','Pend',IIF(ESTADO='10','Progra',IIF(Estado = '20','S/Ct',IIF(Estado='99','Anul',IIF(Estado='50','Aten',' -  '))))) :H='ESTD' ,;
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

* IF !CONMEM
** 	IF SOLSER.NUMOS#'    ' &&SOLSER.ESTADO='30'
*	IF SOLSER.ESTADO='30'
*       DO STANDBY WITH 'La Solicitud de Servicio ya tiene ORDEN '
*       return .f.
*    ENDIF
*       m.detalle = solser.detalle
* ENDIF
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
SET SKIP TO ITEOS1

set filt to Nummes+NumOs=vNumos
if eof()
   do standby with vmens08
   return
else
   * do reporte with 2,"LisOsE",' Orden de Servicios ',2 &&Para Emergencia
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
DEFINE WINDOW LIS FROM 0,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Ordenes de Servicio' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro,vListado,vTofue,vToPrv, VTRI
vCli = SPAC(4)
vAno = '99'
VFTE = '  '
VTRI1='  '
VTRI2='  '
vcodfte = '  '
vcodPRV = '    '
vfecini = date()
vfecfin = date()

@ 01,01 SAY "     Tipo Listado : " GET vListado FUNCTION '^ Detallado;Resumido' 

@ 04,01 SAY "Todas las Fuentes : " GET vTofue  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,07,22)
@ 06,01 SAY "           Fuente : "
@ 06,22 GET vCodFte  PICTURE '!!' VALID VAL_PARA(vCodFte,'CODFTE','C') WHEN VTOFUE=2 

@ 08,01 SAY "Todos los Proveed : " GET vToprv  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOprv,11,22)
@ 10,01 SAY "      Proveedores : "
@ 10,22 GET vCodPrv  PICTURE '!!!!' VALID VALPrv() WHEN vToPRV=2 

@ 12,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ Numero;Proveedor;Emision;Fuente' 

@ 15,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todos;Pendientes;Atendidos;Afectados;Menor Cuant¡a;Liquidados'

@ 18,01 SAY "           Fechas : " GET vtRI1 picture '!!' 		&& when vtippro = 5
@ 18,32                            GET vtri2 picture '!!'  		&& when vtippro = 5

@ 20,01 SAY "           Fechas : " GET vFecIni picture '@D'  when vtippro = 6
@ 20,32                            GET vFecFin picture '@D'  when vtippro = 6

@ 22,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS

IF OKCANCEL = 1
	ACTIVATE WINDOW STANDBY
	@ 01,04 SAY 'Espere un momento........'
	vInd = SYS(3) + '.IDX'
	INDEX ON IIF(vOrden=1,NumOs,IIF(vOrden=2,CodPrv,iif(vOrden=3,DTOS(FECemi),Codfte+NumOs))) TO (vInd) ;
		FOR  IIF(vTipPro=1,.T.,iif(vTipPro=2,Estado = '00',iif(vTipPro=3,Estado = '40',iif(vTipPro=4,Estado = '20',iif(vTipPro=5,Estado = '50',Estado = '50'))))) AND ;
		iif(vtofue =1,.t.,Codfte=alltrim(vCODfte)) 
	SET FILT TO iif(vtoprv =1,.t.,Codprv=alltrim(vCODprv)) AND IIF(vTipPro=6,BETWEEN(FECLIQ,VFECINI,VFECFIN),IIF(EMPTY(vTRI1) OR EMPTY(vTRI2),.T.,BETWEEN(MONT(FECOS),VAL(vTRI1),VAL(vTRI2)))) &&AND orden.tipord#'B'
*		SET FILT TO IIF(vTipPro=5,IIF(VTRI='01',BETWEEN(month(fecLIQ),01,03),IIF(VTRI='02',BETWEEN(MONTH(FECLIQ),04,06),IIF(VTRI='03',BETWEEN(MONTH(FECLIQ),07,09),BETWEEN(MONTH(FECLIQ),10,12)))),.T.) AND VALTOT < 35000.00
	SET INDEX TO (VIND)
	COUNT ALL TO vTotos
*	SUM valtot - anultot TO SUMATOT
	GO TOP
	DEACTIVATE WINDOW STANDBY
	vTitulo=IIF(vTipPro=1,'Listado de Ordenes de Servicio',IIF(vTipPro=2,'Listado de Ordenenes de Servicio Pendientes',IIF(vTipPro=3,'Listado de Ordenes de Servicio Atendidas',IIF(vTipPro=4,'Listado de Ordenes de Servicio Afectadas',IIF(vTipPro=5,'Listado de Ordenes de Servicio Anuladas','Listado de Ordenes de Servicio Liquidadas')))))
	SET MEMOWIDTH TO 43
	IF !EOF()
		SET relation to PERIODO+m.Numos into iteos1
		SET SKIP TO ITEOS1
		*do case
		if vtippro # 6
			do case
				case VLISTADO=1
					SET MEMOWIDTH TO 100
					DO REPORTE WITH 2,"LisOrdS",' Ordenes de Servicio ',1,.F.,.T.
*					DO REPORTE WITH 2,"LisOrdS1",' Ordenes de Servicio ',1,.F.,.T.
				case VLISTADO=2
					DO REPORTE WITH 2,"LisOrsX",' Ordenes de Servicio ',1,.F.,.T.
*					DO REPORTE WITH 2,"LisOrsX1",' Ordenes de Servicio ',1,.F.,.T.
*					DO REPORTE WITH 2,"LisMCUS",' Ordenes de Servicio ',1,.F.,.T.
			endcase
		 else
			do case
				case VLISTADO=1 
					DO REPORTE WITH 2,"LisOrdS",' Ordenes de Servicio ',1,.F.,.T.
				case VLISTADO=2
					DO REPORTE WITH 2,"LisOrsX",' Ordenes de Servicio ',1,.F.,.T.
			ENDcase
		endif
		SELECT ORDSE
	 ELSE
		DO STANDBY WITH VMENS08
	ENDIF
	SET FILT TO
	CLOSE INDEX
	ERASE (VIND)
ENDIF

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

* USE IN 12  
* USE IN 13 
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
vcodprv = iif( empty(vcodprv),vcodprv,padl(alltrim(vcodprv),4,'0'))
xx = val_prv( vcodprv,.t.)
if xx
   return .t.
endif
return vfun

*function valprv
*--------------
private xx, vfun
vfun = .f.
m.codpr = iif( empty(m.codprv),m.codprv,padl(alltrim(m.codprv),4,'0'))
xx = val_prv( m.codprv,.t.)
if xx
   return .t.
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
SEEK m.Periodo+m.NumOs
IF FOUND()
	do standby with 'El O/S ya esta registrado'
	GOTO AS
	RETURN .F.
ENDIF
IF !EOF()
	GOTO AS
ENDIF   
RETURN .T.



PROCEDURE LIQUIDAR
*-----------------
PRIVATE OK
*OK=VE_PASSW('RCL1')
*IF OK
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
	vprv=m.codprv
	activate window standby
	@ 1,1 say 'Fecha Liquidaci¢n : ' get vfecliq 
	read
	DEactivate window standby
	IF RLOCK() &&AND TIPORD='S'
		REPLACE ESTADO WITH '50',fecliq with vfecliq,TIPDOC WITH 'OK'
	else 
   		DO STANDBY WITH 'Proceso Cancelado'
	endif
endif
do vista
*ENDIF
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
SELECT Solser
SEEK ORDSE.periodo+ORDSE.numSS
m.destino = IIF(EMPTY(m.destino),SolSer.Destino,m.destino)
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
ON KEY LABEL F10 KEYBOARD CHR(23)  &&NO VA
IF !WEXIST("Detalle")
   DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Descripci¢n del O/S ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
*  DEFINE WINDOW Detalle FROM 03,12 TO 20,67 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Descripci¢n del O/S ±' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 1
ENDIF

IF WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle SAME
ELSE
   ACTIVATE WINDOW Detalle NOSHOW
ENDIF

MODIFY MEMO DETALLE NOEDIT WINDOW Detalle
*MODIFY MEMO DETALLE WINDOW Detalle
IF !WVISIBLE("Detalle")
   ACTIVATE WINDOW Detalle
ENDIF
RELEASE WINDOW Detalle

*IF LASTKEY()=27 &&NO VA
*   DO STANDBY WITH 'Proceso cancelado. No graba el Detalle ' &&NOVA
*ENDIF  &&NOVA

SELECT (vALIAS)
RETURN .T.

PROCEDURE VALCCC
*---------------
PRIVATE AS,vnumccc
USE SOLCOT  IN 13  ORDER TAG Solcot2  ALIAS solcot
USE IteSC   IN 14  ORDER TAG Itesc1   ALIAS Itesc

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
*--------------
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
*if !v_reg='VG'
if v_reg='RG'
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
SEEK m.periodo +m.Numos

SET RELATION TO Periodo+uniges+unieje+codcad INTO MaePre ADDITIVE

IF !FOUND()
	DO Agre_ItOS
 ELSE
	DO Borra_Os
ENDIF

BROWSE WINDOW Wind_2 NOAPPEND NODELETE NOMENU key m.periodo +m.Numos FIELDS ;
		CODCad	: H= 'Cadena'		:V=val_CODCAD(ALLT(codcad),periodo,'CodCad'):F ,;
		CODCOM	: H= 'Componente.'	:V=Val_comp(periodo+maEPRE.uniges+maepre.unieje+codcad,codcom,'codcom'):F ,;
		CODMET	: H= 'Meta'			:V=Val_meta(periodo+maEPRE.uniges+maepre.unieje+codcad,codcom+codmet,'codmet'):F ,;
		igv		: H='IGV'			:P='@M S,N',;
		codPart	: H= 'Partida'		:V=VAL_cale(codpart,periodo+maepre.uniges+maepre.unieje+maepre.Codfun+maepre.CodPrg+maepre.CodSpr+maepre.ActPry+codcom+codmet+allt(codfte)+allt(m.nummes),'codpart'):F ,;
		ValPart	:H='Monto' :P='99,999,999.99'

SET RELATION TO

*	aa		= IIF(EMPTY(CodPart),' ',IIF(LEN(ALLTRIM(CodPart))=6,val_para(SUBSTR(codpart,5,2),'ESPGAS','D',28,40),VAL_PARA1(SUBSTR(CodPart,7,2),'SUBESP'+SUBSTR(CodPart,5,2),'D'))) :H='Descripci¢n':40

vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo

*  aa = IIF(EMPTY(CodPart),' ',val_partida(right(codpart,2),'ESPGAS','D',28,40)) :H='Descripci¢n':40,;
*  codPart  : H= 'Partida' :F ,;

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
DO AsiOrd
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
*SUSP

vuniges = vUGes
vunieje = vUEje

IF F_appd()
   REPLACE Periodo WITH m.Periodo ,;
   		  Numos   WITH m.Numos ,;
          CodCad  WITH m.CodCad,;
          Uniges  WITH vuniges,;
          UniEje  WITH vunieje
   UNLOCK
ENDIF

vcodcom = MAEPRE.codcom
vcodmet = MAEPRE.codmet

IF f_lock(1)
	REPLACE Codcom  WITH vCodcom,;
			Codmet  WITH vCodmet,;
			Codpart WITH Vp ,;
			IGV     WITH vIGV ,;
			valpart WITH m.ValTot
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
IF rlock()
	SCAN WHILE Periodo+NumOs=m.Periodo+m.NumOs
		* Elimina las asignaciones efectuadas en el calendario
		SELECT CALEN
		SEEK m.periodo+vcadena+iteos1.codcom+iteos1.codmet+allt(m.codfte)+allt(m.nummes)+iteos1.codpart
		REPLACE TotOs WITH TotOs-iteos1.Valpart
		SELECT ITEos1
	ENDSCAN
ENDIF
SELE (AX)
RETURN



PROCEDURE VAL_MES
*----------------
IF VOPCION = 1
	SELECT PARMA
	SET ORDER TO PARMAE1
	M.FecOc  = date()
	*SEEK 'ORDENC'+ALLTRIM(M.NumMes)
	SEEK 'CORRELORDENC'
	if !found()
	   do standby with 'El Correlativo del Mes no est  Inicializado'
	   SELE orden
	   return .F.
	else
	  IF CODIGOAUX='00'
	     do standby with 'El Calendario del Mes '+alltrim(m.nummes)+' ya est  cerrado'
		 SELE orden
   		 return .F.
	  else
		  * m.NumOc=valHC(Parma.NumEnt + 1)
		  xx = IIF(m.Nummes='12','01',padl(alltrim(str((val(m.NumMes)+1),2)),2,'0'))
 		  if !val(m.nummes)=month(date())
		     m.FecOS = IIF( val(m.Nummes) < iif( xx='01',month(DATE())+12,month(DATE())),ctod('01/'+xx+'/'+ALLTRIM(STR(YEAR(m.fecOc)-2000,4))) - 1,date())
  		  endif
		  sele OrdSE 
	      return .t.
	   endif
	endif
ELSE
   RETURN .T.
ENDIF

PROCEDURE siprv
*--------------
parameter vfun
vfun = .t.
if !v_reg='VG'
   do standby with 'El Proveedor no esta Regularizado...Observaci¢n'
endif
return vfun   


PROCEDURE Corri1
*--------------
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
* SELECT Orden
SELECT ORDSE
ACTIVATE WINDOW WindO_0
 SCATTER MEMVAR
 @ 2,55 get m.CODPART
* @ 2,55 get m.PERHC
* @ 2,60 get m.NUMHC
* @ 5,22 GET M.CODFTE
 read VALID VAL_READ()
 
 IF LASTKEY() # 27
    *SELE ORDEN 
    SELE ORDSE
    GATHER MEMVAR
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF

* SELE ITEOC
 SELE ITEOS1
 SEEK m.Periodo + m.NumOS 
 SET FILT TO Periodo = m.Periodo and NumOS = m.NumOS    
 GO TOP
 SCAN WHILE Periodo+NumOS = m.Periodo + m.NumOS 
*      REPLA ALL codfte WITH m.codfte  
      REPLA ALL codPART WITH m.codPART
 ENDSCAN


 
 UNLOCK ALL
 SELECT OrdSE
 
 
 DO PANTALLA
 DO Vista
 RETURN



PROCEDURE AsiOrd
*---------------
*SELE Parma
*SEEK 'TIPDOCO/S'
*IF !FOUND()
*	DO StandBy WITH "PARAMETRO NO INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
*	RETURN
*ENDIF
SELE AsiAut
SEEK "O/S"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD = DCuenta
	cCtaH = HCuenta
ENDIF
SELE AstOrd
SEEK m.Periodo+m.NumMes+m.NumOS+"O/S"
IF FOUND()
	FOR i = 1 TO 2
		IF f_Lock(1) OR RLOCK()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.NumMes  ,;
					NUMREF	WITH m.NumOS   ,;
					TIPDOC	WITH "O/S"     ,;
					FECHA	WITH m.FecEmi,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
			SKIP
		ENDIF
	ENDFOR
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.NumMes  ,;
					NUMREF	WITH m.NumOS   ,;
					TIPDOC	WITH "O/S"     ,;
					FECHA	WITH m.FecEmi,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValTot,0),;
					MTOHAB	WITH IIF(i=2,m.ValTot,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
DEFINE WINDOW wAstOrd FROM 10,10 TO 15,70 ;
 TITLE ' ASIENTOS DE ORDEN' COLOR SCHEME 02
ACTIVATE WINDOW wAstOrd
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY '        Debe '
@ 00,34  SAY '        Haber '
@ 01,04  SAY cCtaD PICTURE '!!!!!!!!!!!'
@ 01,18  SAY m.ValTot PICTURE '99,999,999.99'
@ 02,12  SAY cCtaH PICTURE '!!!!!!!!!!!'
@ 02,34  SAY m.ValTot PICTURE '99,999,999.99'
WAIT " "
DEACTIVATE WINDOW wAstOrd
RELEASE WIND wAstOrd
RETURN

FUNCTION val_partida
*--------------------
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

*IF FILTRO='CODFTE'
*	WAIT WIND MVALOR
*ENDIF

SEEK filtro+mvalor
*IF .NOT. FOUND() .AND. mVariable<>'V'
IF !FOUND() .AND. !mvariable $'VZ'

	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	SET CONFIRM ON
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = ALLTRIM(parma.codigo)
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF

DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE


*SET SKIP TO ITEOS1

PROCEDURE SubOpc
*---------------
PRIVATE cAlias,cMod
cAlias = ALIAS()
SELE SubOp
cCtrlOp = ''
cMod = "02"

SEEK vUsuCla+PADL(SistCtrl,2,'0')+cMod

IF FOUND()
	SCAN WHILE vUsuCla+PADL(SistCtrl,2,'0')+cMod = ALLTRIM(SubOp.User)+SubOp.Sistema+SubOp.Modulo
		cCtrlOp = cCtrlOp + SubOp.Opcion
	ENDSCAN
ENDIF

set skip of PAD Revis of mMenu !'A' $cCtrlOp
set skip of PAD Busca of mMenu !'B' $cCtrlOp
set skip of PAD Anter of mMenu !'C' $cCtrlOp
set skip of PAD Proxi of mMenu !'D' $cCtrlOp
set skip of PAD Corri of mMenu !'E' $cCtrlOp
set skip of PAD Ingre of mMenu !'F' $cCtrlOp
set skip of PAD Anula of mMenu !'G' $cCtrlOp
set skip of PAD Lista of mMenu !'H' $cCtrlOp

SELE (cAlias)

RETURN

