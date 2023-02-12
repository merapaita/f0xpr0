* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegHa95.PRG  15/05/96                               L: 1631  ³	
* ³ Registro de Hojas de Anulaci¢n de Cheques 95                 ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*- Abriendo Archivos
*- Estados
*- 00 Emitido
*- 99 Anulado
*- Cuando en C/P el campo Pres=* ,ya tiene H/A
*- Cuando en C/P el campo Pres= ,el cheque ya fue entregado
CLOSE DATA
USE parmae     IN 20  ORDER TAG parmae1      ALIAS par96 &&Maestro de Par metros
USE parmae95   IN  1  ORDER TAG parmae1      ALIAS parma &&Maestro de Par metros
USE HOJANU     IN  6  ORDER TAG hojanu1	   ALIAS hojanu	
USE itehA      IN  7  ORDER TAG itehA1       ALIAS itehA 
USE compag95   IN  3  ORDER TAG compag1      ALIAS compag &&C/P
USE clase95    IN  8  ORDER TAG clase1       ALIAS clase   
USE cajas      IN 10  ORDER TAG cajas1       ALIAS caja
USE cheque95   IN 11  ORDER TAG cheque1      ALIAS cheque
USE Auxil      IN 15  ORDER TAG Auxil2       ALIAS Auxil
USE maepre95   IN 16   order tag maepre1      ALIAS maepre
USE itepar95   IN 25  order tag itepar1      ALIAS ITEPAR          
SELECT HOJANU
GO BOTTOM
*- Mensajes de aviso al usuario
vmens01 = ' Hojas de Anulaci¢n de Cheque: REVISION '
vmens02 = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
vmens04 = 'Dicha H/A no fue encontrada'
vmens05 = 'No existe H/A anterior'
vmens06 = 'No existe H/A siguiente'
vmens07 = '¨ Desea Anular esta H/A ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta H/A ha sido anulada'
vmens10 = 'La H/A ya est  Atendido'
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Variables de trabajo (registro a trabajar)
PUBLIC vDescri,vNumMes,vNumRef,vCodCtc,vAlo,vCta,vTITULO,vCon1,vDeb,vFlag,vdes,W_TIPCTC
PUBLIC vFecha,vKey,X,MMONTO,VTOT,VPAR,VPTO
store 0 to MMONTO,VTOT,vmondeb,x
vFlag=.T.
vFecha = DATE()
STORE '  ' TO vDescri, vnummes,vKey
*- Inicia proceso
DO inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO pantalla                  && Muestra pantalla inicial
DO vista
*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO ven_accion
DO WHILE ven_accion
	ACTIVATE SCREEN
	ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RETURN

PROCEDURE inicia             && Crea ventanas, men£s y t¡tulos
*---------------
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anular   Listar  Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10 

DEFINE WINDOW wind_1 FROM 00,00 TO 14,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 15,00 TO 23,79 DOUBLE ;
	TITLE '         Detalle de Hoja de Anulacion de Cheque' COLOR SCHEME 10

DEFINE WINDOW WIND_3 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
	TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 10

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD corri   OF mmenu PROMPT '\<Corrige'    AT 24,36
DEFINE PAD ingre   OF mmenu PROMPT '\<Ingresa'    AT 24,45
DEFINE PAD anula   OF mmenu PROMPT 'a\<Nular '    AT 24,54
DEFINE PAD lista   OF mmenu PROMPT '\<Listar '    AT 24,63
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD corri  OF mmenu DO corri
ON SELECTION PAD ingre  OF mmenu DO ingre
ON SELECTION PAD anula  OF mmenu DO anula
ON SELECTION PAD lista  OF mmenu DO lista
ON SELECTION PAD termi  OF mmenu DO termi
RETURN


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR
@ 01,02 SAY "           N§ H/A :"
@ 01,40 SAY "       Fecha  H/A :"
@ 02,02 SAY " Cuenta Corriente :"
@ 03,02 SAY "        Proveedor :"
@ 04,02 SAY "           N§ C/P :"
@ 04,40 SAY " Fecha Digitaci¢n :"
@ 05,02 SAY "Fte.Financiamient :"
@ 05,40 SAY "             Tipo :"
@ 06,02 SAY "         Programa :"
@ 07,02 SAY "      SubPrograma :"
@ 08,02 SAY " Proyecto/Activid :"
@ 09,02 SAY "          Destino :"
@ 10,02 SAY "    Justificaci¢n :"
@ 12,00 SAY PADC(ALLTRIM(vmens02),79,' ') COLOR W+/B
RETURN

PROCEDURE vista              && Displaya datos de HOJANU
*--------------
SELECT HOJANU
SET RELATION TO NumMescp+Numcp INTO compag
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@  0,60 SAY verestha(m.estado)  &&displaya estado de H/A
@  1,22 SAY m.numha
@  1,26 SAY '.'
@  1,27 SAY m.nummes
@  1,60 SAY m.fechan
@  2,22 SAY m.codctc
DO CASE
	CASE m.TipDoc='RE'
    	@ 0,1 SAY 'C/P.Retenci¢n'
	    @ 3,22 SAY VAL_PARA(m.codret,'CODRET','V',22,40)
	CASE m.TipDoc='ME'
	    @ 0,1 say 'C/P.Prestamo '
	    @ 3,22 SAY SPACE(60)
		@ 3,22 SAY m.nompre
	CASE m.TipDoc='HC'
	    @ 0,1 say 'C/P.Hoja Cont'
	    @ 3,22 SAY SPACE(60)
	    DO CASE
		    CASE compag.TIPPRV="O"
		        @ 3,22 SAY compag.nomPre
			    case compag.tipprv="E"
			    @ 3,22 say m.codemp
		        @ 3,22 SAY valemp2(m.codemp)
		    CASE compag.tipprv="P"
		       @ 3,22 say m.codprv
	   		   @ 3,22 SAY valprv2(m.codprv)
         ENDCASE
         M.TIPPRV=compag.TIPPRV
ENDCASE
IF !EMPTY(m.numcp)
	@  4,22 SAY m.numcp+'.'+m.nummescp+' C/P'
ELSE
	@  4,22 SAY m.docref
	@  4,26 SAY m.numref
ENDIF
M.PERIODO=SUBSTR(M.CODCAL,1,2)
@  4,60 SAY m.fecref
@  5,22 SAY val_para(SUBSTR(m.codcal, 5,3),'CODFTE','V',26,50)
@  5,60 SAY val_para(m.tipfun,'TIPFUN','D',22,15)
@  6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
IF m.INCPRES='S'
   @  7,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
   @  8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,3)
   IF alltrim(m.Tipfun)='I' 
	  @  8,25 SAY '.'
	  @  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
   ELSE
      @  8,22 SAY Spac(60)	
   ENDIF	
ENDIF	
@ 09,22 SAY m.destino PICTURE '@S56'
@ 10,22 SAY m.justif PICTURE '@S56'
DO vista_hijo
DO TOTAL
RETURN

FUNCTION VerEstHA  &&Displaya estados de la H/A
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00'
      vFun ='Registrado'
   CASE vEst='99'
      vFun =' Anulado  '
   ENDCASE
RETURN vFun

FUNCTION valprv2 &&Usado para displayar descripc. del prov. en Vista  
*--------------
parameter prov,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF prov<>'0000' .OR. !EMPTY(prov)
   sele auxil
   SET FILT TO TIPO="20"
   seek prov
   v_fun = iif(found(),Descri,"")
   @3,30 SAY DESCRI
   m.Codemp=space(5)
   m.codprv=prov
   vfun = .t.
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
RETURN vfun


FUNCTION valemP2 &&Usado para displayar descripc. del empleado. en Vista  
*--------------
parameter emp,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF emp<>'0000' .OR. !EMPTY(emp)
   sele auxil
   SET FILT TO TIPO="30"
   seek emp
   v_fun = iif(found(),Descri,"")
   @3,30 SAY DESCRI
   m.Codprv=space(4)
   m.codemp=emp
   vfun=.t.
ENDIF
SET FILT TO 
SELE (MALIAS) 
RETURN vfun

PROCEDURE vista_hijo  &&displaya datos ITEHA
*-------------------
HIDE POPUP ALL
SELECT iteHA
SEEK ALLTRIM(m.nummes)+m.numHA
IF FOUND()
		BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numHA  TIMEOUT 0.001 ;
			WINDOW wind_2 ;
			FIELDS;
    anula      :H= 'Anula S/N',;
	codpart    :H= 'Part.',;
	numchq     :H= 'N§ Cheque' ,;
	fecchq     :H= 'Fecha' ,;
	nomgir     :H= 'Girado a' :p='@S20' ,;
	valchq     :H= 'Monto' :p='99,999,999.99'
ELSE
	DO standby WITH 'No tiene detalle'
ENDIF
SELE HOJANU
SET RELATION OFF INTO COMPAG
RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW wind_3
@ 0,1 SAY m.import PICTURE '9,999,999.99'
RETURN

PROCEDURE revis              && Revisa H/A en browse
*--------------
SELE hojanu
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	x1=numha+'.'+nummes  :H=' N§ H/A' ,;
	fechan,;
	codctc :H='CtaCte',;
	tipprv,;
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
	codfte :H='Fte.Fin.',;
	IMPORT :H='Total H/A',;
	x3=NumCP+'.'+NumMesCP :H=' N§ C/P',;
	codcal :H='Calendario',;
	Estado :H='Estado'
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE HOJANU
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
sele hojanu
vtemp=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
ACTIVATE WINDOW standby
STORE SPACE(14) TO vcodctc
@ 0,01 SAY '          Ingrese Mes: ' GET vnummes PICTURE '!!' DEFAULT '  '
@ 1,01 SAY '  Ingrese N£mero H/A : ' GET vcompag PICTURE '!!!!' DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vcompag) .OR. LASTKEY()=27
	RETURN
ELSE
	vnummes = PADL(ALLTRIM(vnummes),2,'0')
	vcompag = PADL(ALLTRIM(vcompag),4,'0')
	SEEK vnummes+vcompag
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ELSE
		DO vista
	ENDIF
ENDIF
RETURN

PROCEDURE anter
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !BOF()
	SKIP -1
ENDIF
IF BOF()
	GO TOP
	DO standby WITH vmens05
ELSE
	DO vista
ENDIF
RETURN

PROCEDURE proxi
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF !EOF()
	SKIP
ENDIF
IF EOF()
	DO standby WITH vmens06
	GO BOTTOM
ELSE
	DO vista
ENDIF
RETURN

PROCEDURE corri
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF estado = '99'
	* Anulada
	DO standby WITH vmens09
	RETURN
ENDIF
IF estado = '50'
	DO standby WITH vmens10
	RETURN
ENDIF
vFlag=.F.
SELECT hojanu
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
DO CASE
	CASE m.TipDoc ='HC' OR M.TIPDOC='RG'
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) DISABLE
		@ 1,22 GET m.numha  WHEN .F.
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() DISABLE
		@ 1,60 GET m.fechan DISABLE
		IF M.TIPDOC='HC'
		DO CASE
		    CASE M.TIPPRV="P"
			   @ 3,22 SAY m.codprv
			   @ 3,30 SAY VALPRV(M.CODPRV) 
		    CASE M.TIPPRV="E"
		   	   @ 3,22 SAY m.codemp 
			   @ 3,30 SAY VALEMP(M.CODEMP) 
		    OTHER
	    	   @ 3,30 SAY m.nompre function "!"
	    ENDCASE	
	    ELSE
	    	   @ 3,30 SAY m.nompre function "!"
	    ENDIF  	   
	    @ 4,22 SAY m.numhc+'.'+m.nummesCP + ' C/P'
		@ 4,60 GET m.fecref
		@ 5,22 SAY val_para(SUBSTR(m.codcal, 5,3),'CODFTE','V',26,50)
		@ 6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
	    @ 7,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
		@ 8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
		IF alltrim(m.Tipfun)='I' 
		    @  8,25 SAY '.'
			@  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
		ELSE
		   @  8,22 SAY Spac(60)	
		ENDIF	
	    @  09,22 say m.destino  PICTURE '@S56' 
		READ &&VALID val_read()
	CASE m.TipDoc='ME'
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	    READ
		@ 1,22 GET m.numha when .f.
	    @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	    @ 1,60 GET m.fechan     VALID ingfec()
	    @ 3,22 SAY m.nompre FUNCTION "!"   
		@ 4,22 SAY m.docref    
	    @ 4,26 SAY m.numref    
	    @ 4,60 GET m.fecref
 	    @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	    @ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	IF M.INCPRES='S'
    @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) &&AND ALAN()
    @ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
    @ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
    @ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
    @ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and ALLTRIM(m.tipfun)='I'
    ENDIF
	READ VALID val_read()
	IF M.INCPRES='S'
    m.codcal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
    ENDIF
	CASE m.TipDoc='RE'
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) DISABLE
	    @ 1,22 GET m.numha when .f.
	    @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	    @ 1,60 GET m.fechan     VALID INGFEC()
        @ 03,02 SAY "Fuente de Retencion"
        @ 3,22 say VAL_PARA(m.codret,'CODRET','V',22,40)
	    @ 4,22 say m.docref
	    @ 4,26 say m.numref    
	    @ 4,60 GET m.fecref
 	    @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	    @ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	    @ 6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
	    @ 7,22 SAY val_subp(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'    ','V',22,40)
        @ 8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
    	IF alltrim(m.Tipfun)='I' 
	    	@  8,25 SAY '.'
			@  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
    	ELSE
		    @  8,22 SAY Spac(60)	
	    ENDIF	
		READ VALID val_read()
CASE m.TipDoc='SE'
	@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) DISABLE
	@ 1,22 GET m.numHA WHEN .F.
	@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() DISABLE
	@ 1,60 GET m.fecHAN VALID INGFEC()
    @ 3,22 SAY m.nompre FUNCTION "!"   
	    @ 4,22 say m.docref
	    @ 4,26 say m.numref    
	    @ 4,60 GET m.fecref
 	    @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	    @ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	    @ 6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
	    @ 7,22 SAY val_subp(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'    ','V',22,40)
        @ 8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
    	IF alltrim(m.Tipfun)='I' 
	    	@  8,25 SAY '.'
			@  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
    	ELSE
		    @  8,22 SAY Spac(60)	
	    ENDIF	
	READ VALID val_read()
ENDCASE
@  10,22 GET m.justif FUNCTION "!" PICTURE '@S56' WHEN !EMPTY(m.Numha)
READ
SELECT caja
SEEK m.codctc
W_TIPCTC=CAJA.TIPO
SELE HOJANU
IF LASTKEY() = 27
	DO vista
	RETURN
ELSE
    INGRESO=.F.
    DO trabaja_hijo
    DO ingap
    DO COMPRE
	SELECT hojanu
	IF RLOCK()
		GATHER MEMVAR
	ENDIF	
ENDIF
UNLOCK ALL
DO vista                    && Muestra nuevos datos
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PUBLIC VCTADEB,VCTAHAB,VVALDEB,VVALHAB
DEACTIVATE WINDOW WIND_3
SELECT hojanu
DO pantalla
SCATTER MEMVAR BLANK
m.fecref = DATE()
vFlag=.T.
OI=carcp()
DO CASE
   CASE M.TIPDOC="HC" OR M.TIPDOC='RG'
	  IF LASTKEY()=27 OR !OI
		 SELECT hojanu
		 DO vista
		 RETURN
	  ENDIF
	  SELE hojanu
      @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  @ 1,22 GET m.numha
	  @ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.fechan 
	  IF M.TIPDOC='HC'
	  DO CASE 
	      CASE  M.TIPPRV="P"
			  @ 3,22 SAY m.codprv
			  @ 3,30 SAY valprV(m.codprv) 
	      CASE  M.TIPPRV="E"
			  @ 3,22 SAY m.codemp 
			  @ 3,30 SAY valemp(m.codemp) 
	      OTHER
			  @ 3,22 SAY m.nompre  function "!" 
	  ENDCASE
	  ELSE
			  @ 3,22 SAY m.nompre  function "!" 
      ENDIF			  
      @ 4,22 SAY m.numCP+'.'+m.nummesCP + ' C/P'
 	  @ 4,60 GET m.fecref
 	  @ 5,22 SAY val_para(SUBSTR(m.codcal, 5,3),'CODFTE','V',26,50)
	  @ 6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
	  @ 7,22 SAY val_subp(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'    ','V',22,40)
      @ 8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
   	  IF alltrim(m.Tipfun)='I' 
	     @  8,25 SAY '.'
		 @  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
      ELSE
	     @  8,22 SAY Spac(60)	
	  ENDIF	
      @  09,22 say m.destino  PICTURE '@S56' 
	  READ VALID val_READ()
  CASE M.TIPDOC="ME"
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  IF LASTKEY()=27
		 SELECT hojanu
		 DO vista
		 RETURN
	  ENDIF
      SELECT hojanu
	  @ 1,22 GET m.numha 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.fechan     VALID ingfec()
	  @ 3,22 SAY m.nompre    
	  @ 4,22 SAY m.docref    
	  @ 4,26 SAY m.numref    
	  @ 4,60 GET m.fecref
 	  @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	  @ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	  IF M.INCPRES='S'
		  @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) &&AND ALAN()
	      @ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
	      @ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
	      @ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
	      @ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
      ENDIF
	  READ VALID VAL_READ() 
 	  m.tipdoc = 'ME'
 	  IF M.INCPRES='S'
      m.codcal  = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))	  
      ENDIF
  CASE M.TIPDOC='RE'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
  	  IF LASTKEY()=27
		 SELECT HOJANU
		 DO vista
		 RETURN
	  ENDIF
	  SELECT hojanu
	  @ 1,22 GET m.numha
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.fechan     VALID INGFEC()
      @ 03,02 SAY "Fuente de Retencion"
      @ 3,22 say VAL_PARA(m.codret,'CODRET','V',22,40)
	  @ 4,22 say m.docref
	  @ 4,26 say m.numref    
	  @ 4,60 GET m.fecref
 	  @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	  @ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	  @ 6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
	  @ 7,22 SAY val_subp(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'    ','V',22,40)
      @ 8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
      IF alltrim(m.Tipfun)='I' 
	     @  8,25 SAY '.'
		 @  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
      ELSE
	     @  8,22 SAY Spac(60)	
	  ENDIF	
	  READ VALID val_READ() 
	  m.estado = '00'
  CASE M.TIPDOC='SE'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO buscaja
	  IF LASTKEY()=27
		 SELECT HOJANU
		 DO vista
		 RETURN
	  ENDIF
      SELECT HOJANU
	  @ 1,22 GET m.numHA 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.fecHAN     VALID ingfec()
	  @ 3,22 SAY m.nompre    
	  M.DOCREF='H/C'
	  @ 4,22 say m.docref
	  @ 4,26 say m.numref    
	  @ 4,60 GET m.fecref
 	  @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	  @ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	  @ 6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
	  @ 7,22 SAY val_subp(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'    ','V',22,40)
      @ 8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
      IF alltrim(m.Tipfun)='I' 
	     @  8,25 SAY '.'
		 @  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
      ELSE
	     @  8,22 SAY Spac(60)	
	  ENDIF	
	  READ VALID VAL_READ() 
	  m.estado = '00'
ENDCASE
@  10,22 GET m.justif FUNCTION "!" PICTURE '@S56' WHEN !EMPTY(m.Numha)
READ VALID val_read()
SELECT caja
SEEK m.codctc
W_TIPCTC=CAJA.TIPO
SELE HOJANU
IF LASTKEY() # 27
    m.estado = '00'
	SELECT compag
	SEEK m.nummescp+m.numcp
	IF RLOCK()
	   REPLACE pres WITH '*'  &&Marca C/P trabajado
	ENDIF
	UNLOCK 
	SELECT par96
	SEEK 'CORREL' + 'HOJANU'
	REPLACE nument WITH nument + 1
	UNLOCK 
	SELECT hojanu
	INGRESO = .T.
	DO trabaja_hijo
    DO INGAP
    DO COMPRE
    SELECT HOJANU
	m.ctadeb = vctadeb
    m.ctahab = vctahab
	m.valdeb = vvaldeb 
	m.valhab = vvalhab 	
	IF f_appd()
       gather memvar
    ENDIF		
ENDIF
DO PANTALLA
DO vista
RETURN

PROCEDURE carcp  &&Captura C/P para el ingreso de H/A
*--------------
vfun = .T.
OK = FOUND()
SELECT COMPAG
SET FILTER TO (ESTADO="10" OR ESTADO="20") AND PRES<>"*" AND PRES<>""
 &&Solo C/P con Cheques Girados Y * los que ya fueron registrados en H/A,  ya fueron entregados 
GO TOP
IF EOF()
	DO standby WITH 'No existe Comprobantes de Pago'
	SET FILT TO
	RETURN .F.
ENDIF
vMes ='00'
vcP  =SPACE(4)
VCC  = SPACE(14)
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ C/P: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 say '.'
@ 1,17 GET vCP DEFAULT SPACE(4)
@ 2, 4 SAY "Cta.Cte.:" GET vcc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcc,1,2,24)
READ
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILTER TO
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vCP = PADL(ALLTRIM(vCP),4,'0')
IF !SEEK(alltrim(vMes)+vCP+alltrim(vcc))
  GO BOTTOM
  ON KEY LABEL F10 KEYBOARD CHR(23)
  DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija el C/P con F10 "
  BROWSE NOED WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numcp :H="C/P",;
     nummes :H="Mes",;
     tipdoc,;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF((TIPPRV="O" .OR. TIPPRV=" "),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
     x2=TRANSF(import,'99,999,999.99'):H="Monto",;
     fecCP :H="fecha",;
     x3=codcal+' '+codctc :H="Cal.CTC"
ENDIF
IF LASTKEY()#27
    SELE compag
	m.tipfun   = tipfun
	m.nummeshc = nummeshc
	m.nummescp = nummes
	m.numcp    = numcp
	m.numhc    = numhc
	m.fecref   = DATE()
	m.codprv   = codprv
	m.codemp   = codemp
	m.codcal   = codcal
	m.codctc   = codctc
	m.codpart  = codpart
	m.tipdoc   = tipdoc
	m.nomPre   = nompre
	m.tipprv   = tipprv
	m.periodo  = periodo
	M.CODFTE   = iif(!empty(CODFTE),codfte,substr(codcal,5,3))
	M.NUMREF   = NUMCP+"."+NUMMES
	M.TIPDOC   = TIPDOC
	m.docref   = "C/P"
	M.CODRET   = CODRET
	M.INCPRES  = INCPRES
	m.destino  = destino
	m.codobra  = codobra
	SHOW GETS
	*- El correlativo
    SELECT Par96
    SEEK 'CORREL'+'HOJANU'
    m.Numha = PADL(NumEnt+1,4,'0')
	SELECT COMPAG
	SET FILT TO 
	SELECT HOJANU
ENDIF
RETURN .T.


PROCEDURE INGFEC  &&Valida Periodo
*---------------
m.periodo = '95'
RETURN .T.

PROCEDURE valfecha
*-----------------
an=RIGHT(STR(YEAR(DATE()),4),2)
vme = VAL(m.nummes)+1
me = PADL(ALLTRIM(STR(vme,2)),2,'0')
vfec = '01/&ME/&AN'
m.fechan = CTOD(vfec) - 1
RETURN .T.

FUNCTION VALPRV  &&Usado para displayar descripc. del prov. en Corrige e Ingresa  
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
private  malias, v_fun, _oldwind,_campo
_campo = varread()
malias = alias()
select AUXIL
_oldwind = woutput()
   seek xcod
   IF FOUND()
      @3,30 SAY DESCRI
      m.codprv=xcod
      V_FUN=.T.
   ELSE   
   SET FILT TO TIPO='20'
   on key label ENTER keyboard chr(23)
   define window _xx from 3,20 to 22,79
   browse window _xx title ' ®Enter¯  Selecciona ' nolgrid noedit noappend nodelete nomenu fields;
   codigo   :h='C¢digo'     ,;
   descri   :h='Nombre'
   on key label ENTER
   release window _xx
   if !empty(_oldwind)
      activate window &_oldwInd
   endif
   if lastkey()=27
        v_fun = .f.
   else
       xcod = codigo
       @3,30 SAY DESCRI
       select (malias)
       if !_tipo
          replace &_campo with  xcod
       endif
       m.Codprv=XCOD
       V_FUN=.T.
    endif
ENDIF    
SET FILT TO
select (malias)
return 

FUNCTION VALEMP &&Usado para displayar descripc. del empleado. en Corrige e Ingresa  
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
private  malias, v_fun, _oldwind,_campo
_campo = varread()
malias = alias()
select AUXIL
_oldwind = woutput()
seek xcod
IF FOUND()
      @3,30 SAY DESCRI
      m.codemp=xcod
      VFUN=.T.
ELSE
SET FILT TO TIPO='30'
   on key label ENTER keyboard chr(23)
   define window _xx from 3,20 to 22,79
   browse window _xx title ' ®Enter¯  Selecciona ' nolgrid noedit noappend nodelete nomenu fields;
   codigo   :h='C¢digo'     ,;
   descri   :h='Nombre'
   on key label ENTER
   release window _xx
   if !empty(_oldwind)
      activate window &_oldwInd
   endif
   if lastkey()=27
        v_fun = .f.
   else
       xcod = codigo
       @3,30 SAY DESCRI
       select (malias)
       if !_tipo
          replace &_campo with  xcod
       endif
       m.Codemp=XCOD
       V_FUN=.T.
    endif
ENDIF    
SET FILT TO
select (malias)
return 

PROCEDURE TRABAJA_HIJO  &&trabaja en ITEHA, capturando DATA de CHEQUE
*---------------------
PRIVATE XTOT,xkey
XTOT=0
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT CHEQUE
SEEK  m.NumMesCP+m.NumCp+m.CodCtc
IF ingreso
	SCAN WHILE nummes=ALLTRIM(M.nummescp) AND numcp=M.numcp and codctc=alltrim(m.codctc)
		SELECT iteha
		IF f_appd()
			REPLACE periodo with m.periodo, nummes WITH ALLTRIM(m.nummes), numha with m.numha,codcal with m.codcal,;
  					codpart WITH CHEQUE.codpart, numchq with cheque.numchq, valchq WITH cheque.valchq,;
       				codfte with m.codfte,tipdoc with m.tipdoc, nomgir with cheque.nomgir,FECCHQ WITH CHEQUE.FECCHQ, ANULA WITH 'N' 
		ENDIF
		XTOT=XTOT+VALCHQ
		UNLOCK
		SELECT cheque
	ENDSCAN
ELSE
	SCAN WHILE nummes=ALLTRIM(M.nummescp) AND numcp=M.numcp and codctc=alltrim(m.codctc)
		SELECT iteha
	 	LOCATE FOR numchq=cheque.numchq and valchq=cheque.valchq
		IF !found()
		   IF f_appd()
			   REPLACE periodo with m.periodo, nummes WITH ALLTRIM(m.nummes), numha with m.numha,codcal with m.codcal,;
  					codpart WITH CHEQUE.codpart, numchq with cheque.numchq, valchq WITH cheque.valchq,;
       				codfte with m.codfte,tipdoc with m.tipdoc, nomgir with cheque.nomgir,FECCHQ WITH CHEQUE.FECCHQ, ANULA WITH 'N' 
		   ENDIF
		   XTOT=XTOT+VALCHQ
		   UNLOCK
	    ELSE
	    	&&Revierte el Proceso
	        USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
			SELECT salpar
			SEEK iteha.codpart+iteha.nummes
			REPLACE mtoeje WITH mtoeje + iteha.valchq 
			UNLOCK
			USE IN 5
	    ENDIF
	    SELECT cheque
	ENDSCAN
ENDIF
SELECT iteha
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numha FIELD ;
    anula      :H= 'Anula S/N' :p='!' :v=(anula$'SN'),; 
	codpart    :H= 'Part.':R,;
	numchq     :H= 'N§ Cheque' :R,;
	fecchq     :H= 'Fecha' :R,;
	nomgir     :H= 'Girado a' :p='@S20' :R,;
	valchq     :H= 'Monto' :p='99,999,999.99'
XKEY=ALLTRIM(M.NUMMES)+M.NUMHA	
SEEK XKEY
SCAN WHILE nummes=alltrim(m.nummes) and numha=m.numha
	IF iteha.anula="N"
        USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
		SELECT salpar
		SEEK iteha.codpart+iteha.nummes
		REPLACE mtoeje WITH mtoeje - iteha.valchq 
		UNLOCK
		USE IN 5
		sele iteha
		XTOT=XTOT-VALCHQ
		DELETE NEXT 1
	ELSE
		SELECT CHEQUE
		LOCATE FOR numchq=ITEHA.numchq 
		IF VALCHQ=ITEHA.VALCHQ 
			REPLACE ESTADO WITH "99"
			REPLACE FECANU WITH M.FECHAN
			REPLACE NUMMESHA WITH M.NUMMES
			REPLACE NUMHA WITH M.NUMHA
		ELSE &&Rebaja de Cheques
			REPLACE ESTADO WITH "25"
			REPLACE FECANU WITH M.FECHAN
			REPLACE NUMMESHA WITH M.NUMMES
			REPLACE NUMHA WITH M.NUMHA
		    REPLACE VALREB WITH ITEHA.VALCHQ
		    WAIT 'REBAJA' WINDOW
		ENDIF    	
		SELE ITEHA
	ENDIF
	UNLOCK
ENDSCAN
M.IMPORT=M.IMPORT+XTOT
SET FILTER TO
SELECT hojanu
UNLOCK ALL
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT hojanu
RETURN


PROCEDURE ingap   &&Muestra asiento patrimonial automatico INVERSO al C/P
*--------------
PRIVATE XDEB,XHAB,XCTA,XTIP,XRET
STORE 0 TO XDEB,XHAB
STORE SPACE(1) TO XTIP,XRET
STORE SPACE(14) TO XCTA
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
USE astpat95   IN 24  ORDER TAG astpat953      ALIAS astpat95 &&Egresos
SELECT astpat95
SEEK ALLTRIM(m.nummesCP)+m.numcp+alltrim(m.codctc)
IF !FOUND()
	DO STANDBY WITH "No existe asiento de C/P"
	return
ENDIF
ON KEY LABEL f10 KEYBOARD CHR(23)
IF ingreso
	SCAN WHILE ALLTRIM(m.nummescp)=nummes AND m.numcp=numref AND m.codctc=alltrim(codctc)
			xdeb=mtohab
			xhab=mtodeb
	    	xcta=codcta
		    xtip=iif(tipcta="D","H","D")
		    XRET=RET
	    	XREC=RECNO()
	    	SELE ASTPAT
			IF f_appd()
				REPLACE nummes WITH m.nummes, numref WITH m.numha,tipdoc WITH 'H/A',ret WITH xret,codctc WITH m.codctc,fecha WITH m.fechan, FecHC WITH m.FecRef,TIPCTC WITH W_TIPCTC
				REPLACE codcta WITH xcta,Mtodeb WITH xDEB, mtohab with xHAB, Tipcta WITH xtip,TIPCTC WITH W_TIPCTC
		    ENDIF
		    SELE ASTPAT95
			store 0 to xdeb,xhab
			store space(1) to xtip
			go xrec	
	ENDSCAN   
ENDIF  	    
USE IN 09 
USE IN 24 
USE astpat   IN 09  ORDER TAG astpat18     ALIAS astpat18 &&Egresos
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
SELE ASTPAT18
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numHA+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta' ,;
	tipcta :H='Tp' :p='@M D,H' ,;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99' ,;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99' ,;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F 
USE IN 09 
USE IN 14
ON KEY
SELECT hojanu
RETURN


PROCEDURE agrite     &&
*---------------
SELECT astpat18
IF f_appd()
	REPLACE nummes WITH m.nummes, numref WITH m.numha,tipdoc WITH 'H/A',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.Fechan,TIPCTC WITH W_TIPCTC,periodo WITH m.periodo
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat18
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN

FUNCTION Asig
*------------
vMonDeb=mtodeb
RETURN

PROCEDURE GRABA  &&Utilizado en ingap
*--------------
REPLACE CODCTC WITH m.CODCTC
RETURN .T.

PROCEDURE COMPRE  &&Asiento Presupuestal automatico inverso al C/P
*---------------
private as
AS=ALIAS()
m.valdeb = M.IMPORT
m.valhab = M.IMPORT
m.CtaDeb = '9320100000'
m.CtaHab = '9220100000'
USE astpre   IN 13  ORDER TAG astpre18      ALIAS astpre18  
SELECT astpre18
SEEK 'D'+ALLTRIM(m.nummes)+ALLTRIM(m.numHA)+ALLTRIM(M.CTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes WITH m.nummes,tipdoc WITH 'H/A',numref WITH m.numHA , cuenta WITH m.ctadeb, tipo WITH 'D',fecref WITH m.fechan,codpart WITH m.codpart,CodCtc WITH ALLTRIM(m.CodCtc),CodCal WITH m.CodCal
	ENDIF
	UNLOCK
ENDIF
IF RLOCK()
	REPLACE ctadeb WITH m.ctadeb ,ctahab WITH SPACE(10), valdeb WITH m.valdeb ,valhab WITH 0 ,codcal WITH m.codcal, FecRef WITH m.fechan,CodCtc WITH ALLTRIM(m.CodCtc)
ENDIF
unlock
USE IN 13
USE astpre   IN 13  ORDER TAG astpre18      ALIAS astpre18  
SELECT astpre18
SEEK 'H'+ALLTRIM(m.nummes)+ALLTRIM(m.numHA)+ALLTRIM(M.CTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes WITH m.nummes,tipdoc WITH 'H/A',numref WITH m.numHA , cuenta WITH m.ctahab, tipo WITH 'H',fecRef WITH m.fechan,codpart WITH m.codpart,CodCtc WITH ALLTRIM(m.CodCtc), CodCal WITH m.CodCal
	ENDIF
	UNLOCK
ENDIF
IF RLOCK()
	REPLACE ctadeb WITH SPACE(10),ctahab WITH m.ctahab , valdeb WITH 0 ,valhab WITH m.valhab ,codcal WITH m.codcal, FecRef WITH m.fechan,CodCtc WITH ALLTRIM(m.CodCtc)
ENDIF
unlock
USE IN 13
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
sele (as)
RETURN


PROCEDURE anula
*---------------
SELECT hojanu
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF ESTADO="99" 
	DO standby WITH vmens09
	RETURN
ENDIF
velimina = yesno('¨ Desea ANULAR esta H/A ?') 
IF velimina .AND. ( RLOCK() .OR. f_lock(1) )
	REPLACE estado WITH '99'
	SELECT compag
	IF SEEK(m.NumMescp+m.Numcp)
	 IF RLOCK() .OR. F_LOCK(1)
	   REPLACE compag.pres with ' ' &&Libera C/P
	 ENDIF  
	UNLOCK
	ENDIF
    USE astpat   IN 09  ORDER TAG astpat18  ALIAS astpat18
    sele astpat18
    SEEK ALLTRIM(m.nummes)+m.numHA+alltrim(m.codctc)
    IF FOUND()
	    SCAN WHILE nummes=alltrim(m.nummes) and numref=m.numHA and codctc=m.codctc and tipdoc="H/A" 
    		IF RLOCK() .OR. F_LOCK(1)
		       DELETE NEXT 1
		    ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
    USE IN 09
    USE astpre   IN 13  ORDER TAG astpre19      ALIAS astpre19  
    sele astpre19
    GO TOP
    SEEK ALLTRIM(m.nummes)+m.numHA+alltrim(m.codctc)
    IF FOUND()
    	SCAN WHILE nummes=alltrim(m.nummes) and numref=m.numHA and codctc=m.codctc and tipdoc="H/A" 
		    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
	    	ENDIF       
	    	UNLOCK
    	ENDSCAN
    ENDIF
    USE IN 13
    USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
    SELECT iteHA
    SEEK ALLTRIM(m.NumMes)+m.NumHA
    IF FOUND()
	    SCAN WHILE nummes=alltrim(m.nummes) and numHA=m.numHA and codctc=m.codctc 
	    	IF RLOCK()
				SELECT salpar
				SEEK iteHA.codpart+iteHA.nummes
				REPLACE mtoeje WITH mtoeje - iteHA.VALCHQ
				SELECT CHEQUE
				LOCATE FOR NUMCHQ=ITEHA.NUMCHQ
				IF VALCHQ<>ITEHA.VALCHQ &&REBAJA DE CHEQUE
					REPLACE VALREB WITH 0
				ENDIF  
				REPLACE ESTADO WITH "00"
	    	ENDIF       
		    UNLOCK
		    SELE ITEHA
    	ENDSCAN
    ENDIF
	SELECT HOJANU
	DO vista
ENDIF
UNLOCK
RETURN

PROCEDURE lista
*--------------
USE astpat   IN 09  ORDER TAG astpat18     ALIAS astpat &&Egresos
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELECT HOJANU
VRECNO=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
SET RELATION TO alltrim(NUMmes) + numHA  INTO iteHA ADDI
SET SKIP TO
DEFINE WINDOW lis FROM 3,10 TO 23,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vestado,vlista
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli
VCTA=M.CODCTC
VANO=M.NUMMES
VCLI=M.NUMHA
STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte;Operaciones'
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_ha()
@ 07,01 SAY "              Mes : " GET vMes    PICTURE '!!'  VALID val_para(vMes  ,'FECMES',' ',27,9) WHEN vLista=3
@ 09,01 SAY "   Cta. Corriente : "
@ 09,22 GET vcta  WHEN  vlista=2 VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,10,07)
@ 12,01 SAY "           Estado : " GET vestado  FUNCTION '^ Registradas;Anuladas' WHEN vlista=2
@ 15,01 SAY " Fecha de Emisi¢n : "
@ 16,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista=2
@ 16,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista=2
@ 18,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
IF LASTKEY()=27
   RELEASE WINDOW lis
   RETURN
ENDIF   
RELEASE WINDOW lis
IF okcancel = 1
   	ACTIVATE WINDOW standby
	xcolor = '*' + SET('COLOR')
	@ 01,06 SAY 'Espere un momento...Reporte en proceso' COLOR &xcolor
	DO CASE 
	   CASE vLista = 1  &&Usa el Repo del Fox
	       VKEY=ALLTRIM(VANO)+VCLI+ALLTRIM(VCTA)
	       wKEY=ALLTRIM(VANO)+VCLI
	       SELE HOJANU
	       SET ORDER TO HOJANU2
	  	   SEEK VKEY
		    DEACTIVATE WINDOW standby
	  	   DO repPRG WITH "LISHA1"," Listado Hoja Anulaci¢n Cheque ",2
	  	   SET ORDER TO HOJANU1
	   CASE vLista = 2  &&Usa programa reporteador
	        DO CASE
	           CASE VESTADO=1
		     	   SET FILTER TO codctc=vcta AND BETWEEN(fecref,vfecini,vfecfin) AND ESTADO="00"
         	       VTITULO = 'REGISTRADAS'
                   DO repPRG WITH "RESHA"," Resumen de Hojas de Anulacion Registradas por Cta.Cte. "                                  	       
               CASE VESTADO=2
			      	SET FILTER TO codctc=vcta AND BETWEEN(fecref,vfecini,vfecfin) AND ESTADO="99"
       	           VTITULO = 'ANULADOS'
				    DEACTIVATE WINDOW standby
                   DO repPRG WITH "RESHA"," Resumen de Hojas de Anulacion Anuladas por Cta.Cte. "                                  	       
		     ENDCASE
	   CASE vLista = 3 &&Usa el Repo del Fox
                SELECT AstPat
                SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") .AND. TipDoc = 'H/A'
                SET RELATION TO nummes+numref  INTO HOJANU ADDI
			    DEACTIVATE WINDOW standby
                DO REPORTE WITH 2,"RESHA"," Listado de Operaciones de Hojas de Anulacion de Cheques "
                SET FILTER TO
	ENDCASE
	SELE HOJANU
    SET RELATION OFF INTO ITEHA
ENDIF
USE IN 9
USE IN 14
SELECT HOJANU
GO VRECNO
DO VISTA
RETURN

PROCEDURE val_ha  && Revisi¢n de Hojas de Anulacion  en browse en Pantalla de Impresion
*---------------
SELE HOJANU
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
SEEK vano+vcli
IF !FOUND()
	vtemp = RECNO()
	HIDE MENU mmenu
	ACTIVATE SCREEN
	vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	ON KEY LABEL f10 KEYBOARD CHR(23)
	BROWSE WINDOW wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	x1=numha+'.'+nummes  :H=' N§ H/A' ,;
	fechan,;
	codctc :H='CtaCte',;
	tipprv,;
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
	codfte :H='Fte.Fin.',;
	IMPORT :H='Total H/A',;
	x3=NumCP+'.'+NumMesCP :H=' N§ C/P',;
	codcal :H='Calendario',;
	Estado :H='Estado'
	vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
	DO logos WITH rotulo1,vtempo
	SHOW MENU mmenu
	IF LASTKEY()=27
		GOTO BOTT
	ENDIF
ENDIF
vano = nummes
vcli = numHA
vcta = codctc
ON KEY LABEL f10
SELE hojanu
RETURN

PROCEDURE LISHA1 &&Programa Reporte usa repprg F.M.V.
*--------------
PARAMETERS XCOP
PRIVATE FILA,XVAL,TVAL,VPAR,TOTALQ
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
FOR I=1 TO XCOP
STORE 0 TO XVAL,TVAL,FILA
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat18     ALIAS astpat &&Egresos
SELE HOJANU
SET ORDER TO HOJANU2
vano=alltrim(nummes)
vcli=numHA
vcta=alltrim(codctc)
Vkey= ALLTRIM(HOJANU.NUMMES)+HOJANU.NUMHA+ALLTRIM(HOJANU.CODCTC)
wkey= ALLTRIM(HOJANU.NUMMES)+HOJANU.NUMHA
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,68  say "PAG:"
@1,76  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisHa1"
@2,68 SAY "FECHA:"
@2,76 SAY DATE()           
@4,20 SAY CHR(14)
@4,22 SAY "<< HOJA DE ANULACION DE CHEQUE >>"
@5,0 SAY CHR(27)+CHR(18)
@06,03 SAY "      N§ H/A :"  
@06,20 SAY NUMHA+'.'+NUMMES
@06,68 SAY "Estado :"  
@06,76 SAY IIF(Estado='00','Emitido',IIF(Estado='99','Anulado',' -  '))
@07,03 SAY "   Fecha H/A :"  
@07,20 SAY FECHAN picture "@D"
@08,03 SAY "         Son :"  
@08,20 SAY LETRAS(IMPORT,'SOLES')
@09,03 SAY "Razon Social :"  
@09,20 SAY IIF(TIPDOC="RE",codret,IIF((TIPPRV='O' OR EMPTY(TIPPRV) OR TIPDOC='RG')," ",IIF((TIPPRV='P'and tipdoc<>'RG'),CODPRV,IIF(TIPDOC<>'RG',CODEMP,' '))))
@09,27 SAY IIF(TIPDOC="RE",VAL_PARA(Codret,'CODRET','D',28,40),IIF((TIPPRV='O' OR EMPTY(TIPPRV) OR TIPDOC='RG'),compag.NOMPRE,IIF((TIPPRV='P' AND TIPDOC<>'RG'),VALPRV3(CODPRV),IIF(TIPDOC<>'RG',VALEMP3(CODEMP),' '))))
*@09,27 SAY IIF(hojanu.TIPDOC="RE",VAL_PARA(hojanu.codret,'CODRET','V',22,40),IIF((hojanu.TIPPRV='O' OR EMPTY(hojanu.TIPPRV)),hojanu.NOMPRE,IIF(hojanu.TIPPRV='P',VALPRV3(hojanu.CODPRV),VALEMP3(hojanu.CODEMP))))
@10,03 SAY "  Referencia :"  
@10,20 SAY 'C/P'+' '+hojanu.nummescp+'.'+hojanu.numcp
*@10,20 SAY docref
*@10,24 SAY numref
@10,35 SAY IIF(!EMPTY(DOCREF),fecref,' ')
@11,03 SAY "     Cta cte :"  
@11,20 SAY codctc
@11,35 SAY val_fun('Caja','codctc','Descri',codctc)
@12,03 SAY "     Fte Fto :"
@12,20 SAY codfte
@12,25 SAY alltrim(val_para(CODFTE,'CODFTE','D',26,50))+' - '+alltrim(val_para(tipfun,'TIPFUN','D',26,50))
	IF INCPRES='S'
		@13,07 SAY "Programa :"
		@13,20 SAY substr(CodCal, 8,2)
		@13,25 SAY val_para(substr(CodCal, 8,2),'CODPRG','D',26,40)
		@14,04 SAY "Subprograma :"
		@14,20 SAY substr(codcal,10,3)
		@14,25 SAY Val_SUBP(substr(codcal,10,3),'CODSUB'+substr(codcal,8,2)+'    ','D',22,50)
		@15,04 SAY IIF(empty(substr(codcal,13,3)),' ',IIF(TIPFUN='F','  Actividad :','   Proyecto :'))
		@15,20 SAY IIF(empty(substr(codcal,13,3)),' ',substr(codcal,13,3))
		@15,25 SAY IIF(empty(substr(codcal,13,3)),' ',VAL_PYAC(alltrim(substr(codcal,13,3)),Periodo+substr(CodCal,8,5),'D',22,50))
		@16,04 SAY IIF(EMPTY(substr(codcal,16,2)),' ',IIF(TIPFUN='F','             ','SubProyecto :'))
		@16,20 SAY IIF(EMPTY(substr(codcal,16,2)),' ',substr(codcal,16,2))
		@16,25 SAY IIF(EMPTY(substr(codcal,16,2)),' ',VAL_SUPY(alltrim(substr(codcal,16,2)),Periodo+substr(codcAl,8,8),'D',22,50))
	ENDIF

@17,08 SAY IIF(EMPTY(DESTINO),' ','Destino :')
@17,19 SAY CHR(15)
@17,22 SAY DESTINO
@18,31 SAY CHR(18)
@19,40 SAY "J U S T I F I C A C I O N "
@20,01 SAY CHR(15)
@20,03 SAY SUBSTR(JUSTIF,1,130)
@21,03 SAY SUBSTR(JUSTIF,131,70)
@22,24 SAY CHR(18)
@23,25 SAY "<< CONTABILIDAD PRESUPUESTAL >>"
@24,10 SAY "Cuentas"
@24,52 SAY "Debe"
@24,72 SAY "Haber"
@25,03 SAY SUBSTR(CTADEB,1,2)
@25,10 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,25)',substr(CtaDeb,1,2))
@25,40 SAY VALDEB PICTURE '@Z 999,999,999,999.99'
@26,03 SAY SUBSTR(CTAHAB,1,2)
@26,10 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,25)',substr(CtaHAB,1,2))
@26,60 SAY VALHAB PICTURE '@Z 999,999,999,999.99'
@28,20 SAY "<<  D E T A L L E     D E    C H E Q U E S>>"
@29,03 SAY "N§ de Cheque"
@29,18 SAY "Fecha"
@29,30 SAY "Girado a"
@29,60 SAY "Monto"
SELE ITEHA
SEEK  wKEY
FILA=30
SCAN WHILE NUMMES=ALLTRIM(VANO) AND NUMha=VCLI 
     @FILA,04 SAY Iteha.numchq
     @FILA,18 SAY IIF(EMPTY(ITEHA.FECCHQ)," ",iteha.fecchq)
     @FILA,30 SAY LEFT(Iteha.nomgir,25)
     @FILA,60 SAY Iteha.valchq PICTURE '@Z 999,999,999,999.99'
     xval=xval+ITEha.valchq
     tval=tval+ITEha.valchq
     FILA=FILA+1
ENDSCAN     
SELE hojanu
FILA=FILA+1
@FILA,40 SAY "      TOTAL :"    
@fila,60 say Tval PICTURE '@Z 999,999,999,999.99'
FILA=FILA+2
@FILA,25 SAY "<< CONTABILIDAD PATRIMONIAL >>"
FILA=FILA+1
@FILA,08 SAY "Cuenta D"
@FILA,32 SAY "Importe"
@FILA,46 SAY "Cuenta H"
@FILA,72 SAY "Importe"
SELE ASTPAT
SEEK VKEY
FILA=FILA+1
SCAN WHILE NUMMES=ALLTRIM(VANO) AND NUMREF=VCLI AND CODCTC=ALLTRIM(VCTA)
     IF TIPCTA="D"
     	  @FILA,06 SAY ASTPAT.CODCTA
          @FILA,20 SAY ASTPAT.MTODEB  PICTURE "@z 999,999,999,999.99"             
     ELSE     
     	  @FILA,44 SAY ASTPAT.CODCTA
          @FILA,60 SAY ASTPAT.MTOHAB  PICTURE "@z 999,999,999,999.99"             
     ENDIF     
     FILA=FILA+1
ENDSCAN
FILA=FILA+3
@FILA,1 SAY REPLICATE("Ä",20)
@FILA,42 SAY REPLICATE("Ä",26)
FILA=FILA+1
@FILA,1 SAY "   CONTROL INTERNO"
@FILA,42 SAY "TESORERO CAJERO GENERAL"
ENDFOR
IF _DEST1=2
	EJECT
ENDIF	
SELE HOJANU
SET ORDER TO HOJANU1
SET DEVICE TO SCREEN 
RETURN


FUNCTION SEPARA  &&Utilizada en REPO Lisha1, para el asiento patrimonial
*--------------
AS=ALIAS()
VKEY = HOJANU.NUMMES+HOJANU.NUMha
VIND4 = SYS(3)+'.IDX'
SELECT ASTPAT
INDEX ON NumMes+NumREF to (vind4) for NUMMES+NUMREF = VKEY AND TIPDOC="H/A"
SET INDEX TO (vInd4)
SELECT (AS)
RETURN ' '

FUNCTION CUENTA  &&Utilizada en REPO Lisha1, para ITEHA, para el asiento patrimonial
*--------------
PARAMETER TIPO
AS=ALIAS()
VKEY =HOJANU.NUMMES+HOJANU.NUMha
SELECT ASTPAT
VCTA=IIF(TIPO='H',ASTPAT.CODCTA,ASTPAT.CODCTA)
VALO=IIF(TIPO='H',ASTPAT.MTOHAB,ASTPAT.MTODEB)
IF !EOF()
   SKIP
ENDIF
SELECT (AS)
RETURN ' '
FUNCTION ORDEN1  &&Usado en REPO lisha1 para ITEHA
*-------------
private vAli
vAli=ALIAS()
VKey1=HOJANU.NumMes+HOJANU.NumHA
SELECT IteHA
vInd1 = SYS(3) + '.Idx'
INDEX ON NumMes+NumHA TO (vInd) FOR NumMes+NumHA=vKey1
SELECT (vAli)
RETURN ' '

FUNCTION SALTA1 &&Usado en REPO lisha1 para ITEHA
*--------------
private vAli
vAli=ALIAS()
SELECT IteHA
IF !EOF() 
   SKIP
else
   set orde to ITEha1
endif	
SELECT (vAli)
RETURN ' '


FUNCTION valprv3  &&Usado para displayar descripc. del prov. en el REPO lisha1  
*--------------
parameter prov,MALIAS
malias = alias()
PRIVATE xx, vfun
SET ORDE TO 2
IF prov<>'0000' .OR. !EMPTY(prov)
   sele auxil
   SET FILT TO TIPO="20"
   seek prov
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
RETURN v_fun


FUNCTIO VALEMP3 &&Usado para displayar descripc. del empleado en el REPO lisha1  
*--------------
parameter emp,MALIAS
malias = alias()
PRIVATE xx, vfun
SET ORDE TO 2
IF emp<>'0000' .OR. !EMPTY(emp)
   sele auxil
   SET FILT TO TIPO="30"
   seek emp
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS) 
RETURN v_fun

PROCEDURE RESHA &&Programa Reporte usa repprg .FMV.
*--------------
PARAMETERS XCOP
PRIVATE FILA,XVAL,TVAL,XKEY
FILA=7
STORE 0 TO XVAL,TVAL
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
SELE HOJANU	
GO TOP
fila=7
DO imp_header  
SCAN WHILE !EOF()
     @FILA,03 SAY HOJANU.NUMHA+"."+HOJANU.NUMMES
     @FILA,12 SAY HOJANU.FECHAN
     XKEY=ALLTRIM(HOJANU.NUMMES)+HOJANU.NUMHA
     SELE ITEHA
     SEEK XKEY
     SCAN WHILE NUMHA=HOJANU.NUMHA AND NUMMES=HOJANU.NUMMES
       	  @FILA,22 SAY ITEHA.NUMCHQ
          @FILA,38 SAY SUBSTR(ITEHA.NOMGIR,1,40)
          @FILA,80 SAY ITEHA.VALCHQ  PICTURE "@z 999,999,999,999.99"             
          XVAL=XVAL+VALCHQ
          FILA=FILA+1
     ENDSCAN
     sele hojanu
     @FILA,22 SAY REPLICATE("- ",40)
     FILA=FILA+1
     @FILA,22 SAY "Subtotal --->"
     @fila,80 say xval picture "@z 999,999,999,999.99"             
     FILA=FILA+1
     @FILA,02 SAY REPLICATE("-",100)
     tval=tval+xval
     xval=0
	 IF fila=54
		IF _DEST1=2
           EJECT
		ENDIF
   	    DO imp_header
        fila=7
     ENDIF
ENDSCAN
@FILA,02 SAY REPLICATE("=",100)
fila=fila+1
@FILA,02 SAY "TOTAL CTA.CTE.--->"+VCTA
@fila,80 say Tval picture "@z 999,999,999,999.99"             
fila=fila+1
@FILA,02 SAY REPLICATE("=",100)
IF _DEST1=2
   EJECT
ENDIF
SET DEVICE TO SCREEN 
RETURN
 
PROCEDURE imp_header  &&Cabecera de Reporte 
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,90 say "PAG:"
@1,95  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "ResHa"
@2,15  SAY "<< RESUMEN DE HOJAS DE ANULACION POR CTA.CTE.>>"+"   "+PROPER(VTITULO)
@2,88 SAY "FECHA:"
@2,95 SAY DATE()           
@3,13 SAY "CTA.CTE: --->"+ALLTRIM(Val_Fun('Caja','CodCtc',"ALLTRIM(Codctc)+' '+SUBSTR(Descri,1,26)",VCTA))  
@4,1 SAY "Ú"
@4,2 SAY REPLICATE("Ä",100)
@4,102 SAY "¿" 
@5,1 SAY "³"
@5,3 SAY "   H/A    FECHA    CHEQUE No        RAZON SOCIAL                                         MONTO"
@5,102 SAY "³"
@6,1 SAY "À"
@6,2 SAY REPLICATE("Ä",100) 
@6,102 SAY "Ù"
RETURN

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_2
RELEASE    WINDOW wind_3
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
RESTORE SCREEN FROM principal
RETURN