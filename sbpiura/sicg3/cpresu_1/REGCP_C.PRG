**--------------------------------------------------------------------------
* RegCP3.Prg  Ultimo. 30-11-95 con Regularizaciones y O/C O/S LIQUIDADAS
* Registra los Comprobantes de Pago
*--------------------------------------------------------------------------
*- Abriendo Archivos
*- Estados
*- 00 Emitido
*- 10 Girado con Retencion
*- 20 Girado con H/C
*- 99 Anulado
CLOSE DATA
USE parmae   IN  1  ORDER TAG parmae1      ALIAS parma &&Maestro de Par metros
USE compag   IN  3  ORDER TAG compag1      ALIAS compag &&C/P
USE itecp    IN  4  ORDER TAG itecp1       ALIAS itecp  
USE hojcon   IN  6  ORDER TAG hojcon1      ALIAS hoja   &&H/C
USE itehc    IN  7  ORDER TAG itehc1       ALIAS itehc 
USE clase    IN  8  ORDER TAG clase1       ALIAS clase   
USE cajas    IN 10  ORDER TAG cajas1       ALIAS caja
USE cheque   IN 11  ORDER TAG cheque1      ALIAS cheque
USE Auxil    IN 15  ORDER TAG Auxil2       ALIAS Auxil
USE maepre   IN 16   order tag maepre1      ALIAS maepre
USE itepar   in 25  order tag itepar1      ALIAS ITEPAR          
SELECT compag
GO BOTTOM
*- Mensajes de aviso al usuario
vmens01 = ' Comprobantes de Pago : REVISION '
vmens02 = '° ®F2¯ Gira cheque    ®F3¯ Retenciones     ®F4¯ Imprime C/P °'
vmens04 = 'Dicho C/P no fue encontrada'
vmens05 = 'No existe C/P anterior'
vmens06 = 'No existe C/P siguiente'
vmens07 = '¨ Desea Anular este C/P ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este C/P ha sido anulada'
vmens10 = 'El C/P ya est  Atendido'
vmens11 = 'El C/P ha sido devuelto'
vmens12 = 'El C/P ya tiene Cheque'
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Variables de trabajo (registro a trabajar)
PUBLIC lcd,vDescri,vNumMes,vNumRef,vCodCtc,vAlo,vCta,vTITULO,vCon1,vDeb,vFlag,vdes,W_TIPCTC
PUBLIC vFecha,vKey,X,MMONTO,VTOT,VPAR,VPTO,xflag,XPRES,VCOMPAG,ZZ,YY,LSEEK
STORE 0 TO MMONTO,VTOT,vmondeb,x
YY=SPACE(5)
ZZ=SPACE(20)
vFlag=.T.
xflag=space(1)
XPRES=SPACE(1)
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
vtempo = ' Revisa  Busca  Anterior  Siguiente                                     Termina '
DO logos WITH rotulo1,vtempo

DEFINE WINDOW wind_0 FROM 00,00 TO 23,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10 

DEFINE WINDOW wind_1 FROM 00,00 TO 14,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 15,00 TO 23,79 DOUBLE ;
	TITLE 'F2: Observaciones      Comprobante de Pago : Detalle' COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_4 FROM 08,01 TO 15,78 DOUBLE ;
	TITLE '** GIRO DE CHEQUE->  ®F5¯ Agregar   ®F8¯ Eliminar   ®F10¯ Terminar **' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_5 FROM 08,02 TO 15,77 DOUBLE ;
	TITLE '** RETENCIONES-> ®F5¯ Agregar   ®F8¯ Eliminar   ®F10¯ Terminar **' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
	TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_7 FROM 13,10 TO 17,70 ;
	TITLE ' CENTRALIZACION DE CAJA ' COLOR SCHEME 10

 DEFINE WINDOW Wind_8 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis   OF mmenu PROMPT '\<Revisa'     AT 24,00
DEFINE PAD busca   OF mmenu PROMPT '\<Busca'      AT 24,08
DEFINE PAD anter   OF mmenu PROMPT '\<Anterior'   AT 24,15
DEFINE PAD proxi   OF mmenu PROMPT '\<Siguiente'  AT 24,25
DEFINE PAD termi   OF mmenu PROMPT '\<Termina'    AT 24,71
ON SELECTION PAD revis  OF mmenu DO revis
ON SELECTION PAD busca  OF mmenu DO busca
ON SELECTION PAD anter  OF mmenu DO anter
ON SELECTION PAD proxi  OF mmenu DO proxi
ON SELECTION PAD termi  OF mmenu DO termi
*ON KEY LABEL f2 DO gircheq
*ON KEY LABEL f3 DO regret
*ON KEY LABEL f4 DO imp_cp
RETURN

PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR
@ 01,02 SAY "           N§ C/P :"
@ 01,40 SAY "       Fecha  C/P :"
@ 02,02 SAY " Cuenta Corriente :"
@ 03,02 SAY "        Proveedor :"
@ 04,02 SAY "   Doc.Referencia :"
@ 04,48 SAY " Fecha Digitaci¢n :"
@ 05,02 SAY "Fte.Financiamient :"
@ 05,40 SAY "             Tipo :"
@ 06,02 SAY "         Programa :"
@ 07,02 SAY "      SubPrograma :"
@ 08,02 SAY " Proyecto/Activid :"
@ 09,02 SAY "            Glosa :"
@ 10,02 SAY "    Observaciones :"
*@ 12,00 SAY PADC(ALLTRIM(vmens02),79,' ') COLOR W+/B
RETURN

PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT compag
SET RELATION TO NumMesHC+NumHC INTO Hoja
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
@  0,60 SAY verestcp(m.estado)
@  1,22 SAY m.numcp
@  1,26 SAY '.'
@  1,27 SAY m.nummes
@  1,60 SAY m.feccp
@  2,22 SAY m.codctc
DO CASE
	CASE m.TipDoc='RE'
    	@ 0,1 SAY 'Retenci¢n'
	    @ 3,22 SAY VAL_PARA(m.codret,'CODRET','V',22,40)
	CASE m.TipDoc='ME'
	    @ 0,1 say 'Memorandum'
	    @ 3,22 SAY SPACE(60)
		@ 3,22 SAY m.nompre
	CASE m.TipDoc='SE'
	    @ 0,1 say 'Sectorial'
	    @ 3,22 SAY SPACE(60)
		@ 3,22 SAY m.nompre
	CASE m.TipDoc='HC' OR M.TIPDOC='RG'
	    IF M.TIPDOC='HC'
		    @ 0,1 say 'Hoja Cont'
		ELSE 
		    @0,1 SAY 'Regularizac. H/C'    
		ENDIF    
	    @ 3,22 SAY SPACE(60)
	    IF M.TIPDOC='HC'
		    DO CASE
			    CASE hoja.tipprv="E"
			       @ 3,22 say m.codemp
		    	   @ 3,22 SAY valemp2(m.codemp)
			    CASE hoja.tipprv="P"
			       @ 3,22 say m.codprv
			   	   @ 3,22 SAY valprv2(m.codprv)
				OTHER  	   
			        @ 3,22 SAY hoja.nombre
        	ENDCASE
	        M.TIPPRV=HOJA.TIPPRV
	    ELSE
	    	@3,22 SAY M.NOMPRE     
	    ENDIF 	
ENDCASE
IF !EMPTY(m.numhc)
	@  4,22 SAY m.numhc+'.'+m.nummeshc+' H/C'
ELSE
	@  4,22 SAY m.docref
	@  4,26 SAY m.numref
	@  4,34 SAY m.REFER
ENDIF
M.PERIODO=SUBSTR(M.CODCAL,1,2)
@  4,68 SAY m.fecref
@  5,22 SAY val_para(m.codfte,'CODFTE','V',26,50) &&val_para(SUBSTR(m.codcal, 5,3),'CODFTE','V',26,50)
@  5,60 SAY val_para(m.tipfun,'TIPFUN','D',22,15)
IF M.INCPRES='S'
@  6,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
@  7,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
@  8,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,3)
   IF alltrim(m.Tipfun)='I' 
	  @  8,25 SAY '.'
	  @  8,26 SAY VAL_SUPY(alltrim(substr(m.codcal,16,2)),m.Periodo+substr(m.codcal, 8,8),'V',22,40,5) 
   ELSE
      @  8,22 SAY Spac(60)	
   ENDIF
ENDIF   
@  9,22 SAY m.glosa PICTURE '@S56' 
@ 10,22 SAY SUBSTR(m.observ,1,56)
IF m.Reten>0
   @11,02 say '        Retenci¢n=>'
   @11,22 say m.Reten
ELSE
   @11,00 say spac(79)
ENDIF      
DO vista_hijo
DO TOTAL
RETURN

FUNCTION valprv2
*--------------
parameter prov,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
*SET ORDE TO 2
IF prov='0000' .OR. EMPTY(prov)
ELSE   
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


FUNCTION valemP2
*--------------
parameter emp,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
*SET ORDE TO 2
IF emp='0000' .OR. EMPTY(emp)
ELSE   
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

PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT itecp
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
IF FOUND()
	IF ALLTRIM(m.tipfun)='I'
		BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)  TIMEOUT 0.001 ;
			WINDOW wind_2 ;
			FIELDS;
			codpart    :H= 'C¢d.Par' ,;
			codanal    :H= 'C¢d.Anl' ,;
			aa=val_part(RIGHT(codanal,2),LEFT(codanal,2),'D') :H='Descripci¢n' :40,;
			impparc    :H= 'Monto' :p='9,999,999.99'
	ELSE
		BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)  TIMEOUT 0.001 ;
			WINDOW wind_2 ;
			FIELDS;
			codanal    :H= 'C¢d.Par' ,;
			aa=val_part(RIGHT(codanal,2),LEFT(codanal,2),'D') :H='Descripci¢n' :40,;
			impparc    :H= 'Monto' :p='9,999,999.99'
	ENDIF
ELSE
	DO standby WITH 'No tiene detalle'
ENDIF
SELE compag
SET RELATION OFF INTO Hoja
RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW wind_3
@ 0,1 SAY m.import PICTURE '9,999,999.99'
RETURN

PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE COMPAG
SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
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
	x1=numcp+'.'+nummes  :H=' N§ CP' ,;
	feccp,;
	codctc :H='CtaCte',;
	tipprv,;
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
	codfte :H='Fte.Fin.',;
	IMPORT :H='Total CP',;
	x3=NumHC+'.'+NumMesHC :H=' N§ H/C',;
	codcal :H='Calendario',;
	Estado :H='Estado'
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE compag
SET RELATION OFF INTO Cheque
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
vtemp=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
STORE SPACE(14) TO VCODCTC
STORE SPACE(4) TO VCOMPAG
ACTIVATE WINDOW standby
@ 0,01 SAY '          Ingrese Mes: ' GET vnummes PICTURE '!!' DEFAULT space(2)
@ 1,01 SAY 'Ingrese N£mero ComPag: ' GET vcompag PICTURE '@!' DEFAULT SPACE(4)
@ 2,01 SAY '       Ingrese CtaCte: ' GET vcodctc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcodctc,1,2,24)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vcompag) .OR. LASTKEY()=27
	RETURN
ELSE
	vnummes = PADL(ALLTRIM(vnummes),2,'0')
	vcompag = PADL(ALLTRIM(vcompag),4,'0')
	SEEK vnummes+vcompag+ALLTRIM(vcodctc)
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
SELECT compag
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
vperiodo = SUBSTR(m.codcal,1,2)
vCodPrg  = substr(m.codcal,08,2)
vCodSub  = substr(m.codcal,10,3)
vProyec  = substr(m.codcal,13,3) 
vCodact  = substr(m.codcal,13,2) 
vSubpry  = substr(m.codcal,16,4)
DO CASE
CASE m.TipDoc ='HC' or m.tipdoc="RG"
	@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) DISABLE
	@ 1,22 GET m.numcp WHEN .F.
	@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() DISABLE
	@ 1,60 GET m.feccp DISABLE
	if m.tipdoc="HC"
		DO CASE
		    CASE M.TIPPRV="P"
			   @ 3,22 SAY m.codprv
			   @ 3,30 SAY VALPRV2(M.CODPRV) 
		    CASE M.TIPPRV="E"
		       @ 3,22 SAY m.codemp 
			   @ 3,30 SAY VALEMP2(M.CODEMP) 
			OTHER   
	    	@ 3,30 SAY m.nompre 
	    ENDCASE	
	ELSE
	  @3,30 SAY M.NOMPRE    
	ENDIF  
	@ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
	@ 4,68 GET m.fecref
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
	READ &&VALID val_read()
    ACTIVATE WINDOW wind_8
    	     @ 0,0 SAY 'Destino: ' get m.destino picture '@S73' FUNCTION "!"
	         READ
    DEACTIVATE WINDOW wind_8
CASE m.TipDoc='ME'
    @ 00,02 say "         Prestamo :" get xflag function "M S,N"
    @ 00,40 say "Incidencia Presup.:" get m.INCPRES  FUNCTION "M S,N"
	@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) DISABLE
    READ
	@ 1,22 GET m.numcp WHEN .F.
	@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() DISABLE
	@ 1,60 GET m.feccp  VALID INGFEC()
	@ 3,22 GET m.nompre FUNCTION "!"
	@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
	@ 4,26 GET m.numref PICTURE '!!!!.!!'
	@ 4,34 GET M.REFER
	@ 4,68 GET m.fecref
	@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
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
	@ 1,22 GET m.numcp WHEN .F.
	@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() DISABLE
	@ 1,60 GET m.feccp VALID INGFEC()
    @ 03,02 SAY "Fuente de Retencion"
	@ 3,22 GET m.codret VALID VAL_Para(m.codret,'CODRET',' ',22,40)
	@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
	@ 4,26 GET m.numref PICTURE '!!!!.!!'
	@ 4,34 GET M.REFER
	@ 4,68 GET m.fecref
	@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
    @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) &&AND ALAN()
    @ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
    @ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
    @ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
    @ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and ALLTRIM(m.tipfun)='I'
	READ VALID val_read()
    m.codcal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
CASE m.TipDoc='SE'
	@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) DISABLE
	@ 1,22 GET m.numcp WHEN .F.
	@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() DISABLE
	@ 1,60 GET m.feccp VALID INGFEC()
	@ 3,22 GET m.nompre FUNCTION "!"
	@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
	@ 4,26 GET m.numref PICTURE '!!!!.!!'
	@ 4,34 GET M.REFER
	@ 4,68 GET m.fecref
	@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
    @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) &&AND ALAN()
    @ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
    @ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
    @ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
    @ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and ALLTRIM(m.tipfun)='I'
    DESE=.F.
	READ VALID val_read()
    m.codcal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))
ENDCASE
    M.USUARIO = _WCOD
@  9,22 GET m.glosa  PICTURE '@S56' FUNCTION "!"
@ 10,22 GET m.observ PICTURE '@S56' FUNCTION "!"
READ
	  SELE CAJA
	  SEEK M.CODCTC
	  W_TIPCTC=CAJA.TIPO
	  SELE COMPAG
IF LASTKEY() = 27
	DO vista
	RETURN
ELSE
    INGRESO=.F.
    do ingap
    IF m.docref='H/C' OR M.DOCREF="RG"
       do trabaja_hijo
    ELSE
       do traba_hijo
    ENDIF
    VTOTAL=0
	DO Total_padre
	m.import = vtotal
	DO COMPRE
	SELE COMPAG
	IF RLOCK()
		SELECT compag
		GATHER MEMVAR
	ENDIF	
ENDIF
UNLOCK ALL
ON KEY LABEL f2 DO gircheq
ON KEY LABEL f3 DO regret
ON KEY LABEL f4 DO imp_cp
DO vista                    && Muestra nuevos datos
RETURN

PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PUBLIC VCTADEB,VCTAHAB,VVALDEB,VVALHAB
DEACTIVATE WINDOW WIND_3
IF escolor
	DEFINE POPUP xcot  FROM 17,55 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP xcot  FROM 17,55 COLOR SCHEME c_popup
ENDIF
DEFINE BAR 1 OF xcot PROMPT ' de \<Hoja de control '
DEFINE BAR 2 OF xcot PROMPT ' de \<Memorandum      '
DEFINE BAR 3 OF xcot PROMPT ' de \<Retenciones     '
DEFINE BAR 4 OF xcot PROMPT ' de re\<Gularizaciones'
DEFINE BAR 5 OF xcot PROMPT ' de \<Sectoriales     '
ON SELECTION POPUP xcot  DEACTIVATE POPUP
ACTIVATE POPUP xcot
DO CASE
   CASE BAR() = 1
		dehc = .T.
		deme = .F.
		dere = .F.
		dese = .F.
		dreg = .F.
   CASE BAR() = 2
		dehc = .F.
		deme = .T.
		dere = .F.
		dese = .F.
		dreg = .F.
   CASE BAR() = 3
		dehc = .F.
		deme = .F.
		dere = .T.
		dese = .F.
		dreg = .F.
   CASE BAR() = 4
		dehc = .T.
		deme = .F.
		dere = .F.
		dreg = .T.
		dese = .F.
   CASE BAR() = 5
		dehc = .F.
		deme = .F.
		dere = .F.
		dese = .T.
		dreg = .F.
   OTHERWISE
	   	dehc = .F.
		deme = .F.
		dere = .F.
		dese = .F.
		dreg = .F.
ENDCASE
RELEASE POPUP xcot
SELECT compag
DO pantalla
SCATTER MEMVAR BLANK
m.fecref = DATE()
M.INCPRES ='S'
vCodPrg  = space(2)  
vCodSub  = space(3)    
vProyec  = space(3) 
vSubpry  = space(4)
vCodAct  = space(3)
vFlag=.T.
DO CASE
   CASE dehc
      OI=carhc()  &&Captura H/C
      IF LASTKEY()=27 OR !OI
		 SELECT compag
		 DO vista
		 RETURN
	  ENDIF
	  SELE COMPAG
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  @ 1,22 GET m.numcp
	  @ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp 
	  IF DREG=.F.
		  @ 3,22 GET m.codprv  PICTURE '!!!!'   VALID valprV(m.codprv) WHEN HOJA.TIPPRV="P"  
		  @ 3,22 GET m.codemp  PICTURE '!!!!!!' VALID valemp(m.codemp) WHEN  HOJA.TIPPRV="E" 
		  @ 3,22 GET m.nompre  function "!" WHEN  HOJA.TIPPRV="O" 
	  ELSE
          @ 03,02 SAY "Cta. de Devoluci¢n "
		  @ 3,22 GET m.nompre  function "!"  
 	  ENDIF  
      @ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
 	  @ 4,68 GET m.fecref
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
	  READ VALID val_READ()
      ACTIVATE WINDOW wind_8
      @ 0,0 SAY 'Destino: ' get m.destino picture '@S73' FUNCTION "!"
      READ
      DEACTIVATE WINDOW wind_8
	  m.docref = 'H/C'
	  if dreg
	     m.tipdoc="RG"
	  else   
	     m.tipdoc = 'HC'
	  endif
  CASE deme
      @ 00,02 say "         Prestamo :" get xflag function "M S,N"
      @ 00,40 say "Incidencia Presup.:" get m.INCPRES  FUNCTION "M S,N"
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO buscaja
	  IF LASTKEY()=27
		 SELECT compag
		 DO vista
		 RETURN
	  ENDIF
      SELECT compag
      M.DOCREF='MEM'
	  @ 1,22 GET m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID ingfec()
	  @ 3,22 GET m.nompre    function "!"
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET M.REFER
	  @ 4,68 GET m.fecref
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  @ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
	  IF M.INCPRES='S'
		  @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) 
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
   CASE dere
      M.DOCREF='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO buscaja
  	  IF LASTKEY()=27
		 SELECT compag
		 DO vista
		 RETURN
	  ENDIF
	  SELECT compag
	  @ 1,22 GET m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID INGFEC()
      @ 03,02 SAY "Fuente de Retencion"
      @ 3,22 GET m.codret    VALID VAL_PARA(m.codret,'CODRET',' ',22,40)
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40) && AND m.docref#'H/C'
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET M.REFER
	  @ 4,68 GET m.fecref
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  @ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
      @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) &&AND ALAN()
      @ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
      @ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
      @ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
      @ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and ALLTRIM(m.tipfun)='I'
	  READ VALID val_READ() 
      m.codcal  = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))	  
	  m.estado = '00'
   	  m.tipdoc = 'RE'
  CASE dese
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO buscaja
	  IF LASTKEY()=27
		 SELECT compag
		 DO vista
		 RETURN
	  ENDIF
      SELECT compag
	  @ 1,22 GET m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID ingfec()
	  @ 3,22 GET m.nompre    function "!"
	  M.DOCREF='H/C'
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40)
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET M.REFER
	  @ 4,68 GET m.fecref
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
      M.TIPFUN='F'
	  @ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
      VCODPRG='02'
	  @ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) &&AND ALAN()
     @ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
     @ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vProyec),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
     @ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
     @ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(alltrim(vSubPry),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and m.tipfun='I'
	  READ VALID VAL_READ() 
      m.codcal  = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',alltrim(vProyec)+alltrim(vSubPry),alltrim(vCodAct))	  
	  m.estado = '00'
     m.tipdoc = 'SE'
ENDCASE
M.USUARIO=_WCOD
@  9,22 GET m.glosa  PICTURE '@S56' WHEN !EMPTY(m.NumCp) FUNCTION "!"
@ 10,22 GET m.observ PICTURE '@S56' WHEN !EMPTY(m.NumCp) FUNCTION "!"
READ VALID val_read()
IF LASTKEY() # 27
    m.estado = '00'  
    SELECT caja
	SEEK m.codctc
    W_TIPCTC=CAJA.TIPO
	IF RLOCK()
		REPLACE corcp WITH corcp + 1
	ENDIF
	unlock
	IF !EMPTY(m.numhc)
		SELECT hoja
		SEEK m.nummeshc+m.numhc
		IF RLOCK()
			REPLACE nummescp WITH m.nummes, numcp WITH m.numcp, ESTADO WITH '50'
		ENDIF
		SELECT parma
		SEEK 'CORREL' + 'COMPAG'
		REPLACE nument WITH nument + 1
	ENDIF
	UNLOCK 
	SELECT compag
	INGRESO = .T.
	DO CASE
       	CASE dehc
       	    DO INGAP
			DO trabaja_hijo
       	CASE dreg
       	    DO INGAP
			DO trabaja_hijo
		CASE deme
			DO ingap
			DO traba_hijo
		CASE dere
			DO ingap
			DO traba_hijo
		CASE deSE
			DO ingap
			DO traba_hijo
	ENDCASE
	vtotal = 0
    DO Total_padre
	DO COMPRE
 	SELECT compag		
    m.import = vtotal
    m.ctadeb = vctadeb
    m.ctahab = vctahab
    m.valdeb = vvaldeb 
    m.valhab = vvalhab 	
	if f_appd()
       gather memvar
    else
       do standby with "Atenci¢n: No grabo el Registro"   
    endif	
    UNLOCK
ENDIF
ON KEY LABEL F2 DO gircheq
ON KEY LABEL F3 DO regret
ON KEY LABEL f4 DO imp_cp
DO PANTALLA
DO vista
RETURN

PROCEDURE carhc
*--------------
* Carga valores de HC
vfun = .T.
OK = FOUND()
SELECT hoja
SET FILTER TO ESTADO='20'
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos H/C '
	SET FILT TO
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHc  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/C: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 say '.'
@ 1,17 GET vHC DEFAULT SPACE(4)
READ
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILT TO
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHC = PADL(ALLTRIM(vHC),4,'0')
IF !SEEK(alltrim(vMes)+vHC)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HC con F10 "
   BROWSE NOED WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhc :H="HC",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O",NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechc :H="fecha",;
     x3=codcal+' '+codctc :H="Cal.CTC"
ENDIF
IF LASTKEY()#27
    SELE HOJA
	m.tipfun   = tipfun
	m.nummeshc = nummes
	m.nummes   = nummes
	m.numhc    = numhc
	m.fecref   = fechc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codcal   = codcal
	m.codctc   = codctc
	m.codpart  = codpart
	m.tipdoc   = tipdoc
	m.nombre   = nompre
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = DESTINO
	m.codobra  = codobra
	M.CODFTE   = CODFTE
	M.NUMREF   = NUMHC+"."+NUMMES
    Lseek = Periodo+Numref+ALLTRIM(Codfte)
	*- El correlativo
	DO CASE
	   CASE alltrim(TIPDOC)='O/C'
			USE OrdCom   IN 23   order tag OrdCom1   ALIAS Orden
			SELE ORDEN
			SEEK ALLTRIM(LSEEK)
		    IF FOUND() AND ALLTRIM(ORDEN.TIPDOC)#'OK'
			   do standby with 'La O/C no est  liquidada llamar a Abastecimentos'
	           USE IN 23
		       sele compag
		       RETURN .F.
		    ENDIF   
		    USE IN 23
	   CASE alltrim(TIPDOC)='O/S'
			USE Ordser   IN 24  order tag OrdSer1   ALIAS OrdSer
			SELE ORDSER
			SEEK ALLTRIM(LSEEK)
		    IF FOUND() AND ALLTRIM(ORDSER.TIPDOC)#'OK'
			   do standby with 'La O/S no est  liquidada llamar a Abastecimentos'
	           USE IN 24
		       sele compag
		       RETURN .F.
		    ENDIF   
		    USE IN 24
	ENDCASE   
	sele hoja
	SHOW GETS
	SELECT caja
	SEEK ALLTRIM(m.codctc)
	IF !FOUND()
		DO standby WITH 'No se tiene inscrito la cuenta ' + m.codctc
	ELSE
		m.numcp =  PADL(corcp,4,'0')
		*        REPLACE  CorCP WITH CorCP + 1
	ENDIF
	SELECT hoja
	SET FILT TO
	SELECT compag
ENDIF
RETURN .T.


PROCEDURE INGFEC
*---------------
m.periodo = RIGHT(DTOC(m.feccp),2)
RETURN .T.

PROCEDURE buscaja
*----------------
SELECT caja
SEEK m.codctc
IF !FOUND()
	DO standby WITH 'No se tiene inscrito la cuenta ' + m.codctc
ELSE
	m.numcp =  PADL(corcp,4,'0')
ENDIF
RETURN

PROCEDURE valfecha
*-----------------
an=RIGHT(STR(YEAR(DATE()),4),2)
vme = VAL(m.nummes)+1
me = PADL(ALLTRIM(STR(vme,2)),2,'0')
vfec = '01/&ME/&AN'
m.feccp = CTOD(vfec) - 1
RETURN .T.

PROCEDURE ingap   && F5
*--------------
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
IF !FOUND()
	DO agrite
ENDIF
ON KEY LABEL f5 DO agrite
ON KEY LABEL f8 DO eliite
ON KEY LABEL f10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F
SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
STORE 0 TO vdebe, vhaber ,VRET
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=alltrim(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
		IF RET='S'
		   VRET=VRET+MTOHAB
        ENDIF		   
        if subs(codcta,1,3)='104' OR subs(codcta,1,3)='108'
         	mmonto=Mtohab
        endif	
ENDSCAN
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
if ingreso
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef,TIPCTC WITH W_TIPCTC
		REPLACE codcta WITH '1010201000',Mtodeb WITH mmonto, Tipcta WITH 'D', TIPCTC WITH W_TIPCTC
	ENDIF
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef,TIPCTC WITH W_TIPCTC
		REPLACE codcta WITH '1010201000',Mtohab WITH mmonto, Tipcta WITH 'H',TIPCTC WITH W_TIPCTC
	ENDIF
	DO vis_cen
else
	* si es corrije solo modifica
	SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
	IF FOUND()
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=alltrim(codctc)
		    if codcta='1010201000' and tipcta='D'
	        	repla Mtodeb WITH mmonto
	        endif
	        if codcta='1010201000' and tipcta='H'
	        	repla Mtohab WITH mmonto
	       endif
		ENDSCAN
    ELSE		
		IF f_appd()
			REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef, TIPCTC WITH W_TIPCTC
			REPLACE codcta WITH '1010201000',Mtodeb WITH mmonto, Tipcta WITH 'D', TIPCTC WITH W_TIPCTC
		ENDIF
		IF f_appd()
			REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef, TIPCTC WITH W_TIPCTC
			REPLACE codcta WITH '1010201000',Mtohab WITH mmonto, Tipcta WITH 'H',TIPCTC WITH W_TIPCTC
		ENDIF
	ENDIF	
	DO vis_cen
endif	
USE IN 09 
USE IN 14
ON KEY
SELECT compag
IF RLOCK() AND VRET#0 AND INGRESO
   REPLA RETEN WITH VRET
ELSE
   m.Reten = vret
ENDIF
UNLOCK
RETURN

procedure vis_cen &&Visualizacentralizaci¢n de caja
ACTIVATE WINDOW wind_7
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY '1010201000'
@ 01,18  SAY mmonto PICTURE '999,999,999.99' 
@ 02,12  SAY '1010201000'
@ 02,34  SAY mmonto PICTURE '999,999,999.99' 
DO standby WITH 'Visualizando los Movimientos'
DEACTIVATE WINDOW wind_7
return

FUNCTION Asig
*------------
vMonDeb=mtodeb
RETURN


PROCEDURE agrite     &&
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecReF ,TIPCTC WITH W_TIPCTC
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN


PROCEDURE verap   && F5
*--------------
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+m.codctc
IF !FOUND()
	DO standby WITH 'No tiene detalle'
ELSE
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numcp   TIMEOUT 0.001 ;
		WINDOW wind_2 ;
		FIELDS;
		codcta :H='CtaCte',;
		tipcta :H='Tp',;
		mtodeb :H='Monto Debe' :p='999,999,999.99',;
		mtohab :H='Monto Haber'  :p='999,999,999.99',;
		ret    :H='Ret?' :p='!'
ENDIF
USE IN 09
SELECT compag
RETURN

PROCEDURE COMPRE
*---------------
private as
AS=ALIAS()
m.valdeb = vtotal
m.valhab = vtotal
m.CtaDeb = '9220100000'
m.CtaHab = '9320100000'
USE astpre   IN 13  ORDER TAG astpre2      ALIAS astpre  
SELECT astpre
SEEK 'D'+ALLTRIM(m.nummes)+ALLTRIM(m.numcp)+ALLTRIM(M.CTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes WITH m.nummes,tipdoc WITH 'C/P',numref WITH m.numcp , cuenta WITH m.ctadeb, tipo WITH 'D',fecref WITH m.fecCP,codpart WITH m.codpart,CodCtc WITH ALLTRIM(m.CodCtc),CodCal WITH m.CodCal
	ENDIF
	UNLOCK
ENDIF
IF RLOCK()
	REPLACE ctadeb WITH m.ctadeb ,ctahab WITH SPACE(10), valdeb WITH m.valdeb ,valhab WITH 0 ,codcal WITH m.codcal, FecRef WITH m.FecCP,CodCtc WITH ALLTRIM(m.CodCtc)
ENDIF
unlock
USE IN 13
USE astpre   IN 13  ORDER TAG astpre2      ALIAS astpre  
SELECT astpre
SEEK 'H'+ALLTRIM(m.nummes)+ALLTRIM(m.numcp)+ALLTRIM(M.CTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes WITH m.nummes,tipdoc WITH 'C/P',numref WITH m.numcp , cuenta WITH m.ctahab, tipo WITH 'H',fecRef WITH m.fecCP,codpart WITH m.codpart,CodCtc WITH ALLTRIM(m.CodCtc), CodCal WITH m.CodCal
	ENDIF
	UNLOCK
ENDIF
IF RLOCK()
	REPLACE ctadeb WITH SPACE(10),ctahab WITH m.ctahab , valdeb WITH 0 ,valhab WITH m.valhab ,codcal WITH m.codcal, FecRef WITH m.FecCP,CodCtc WITH ALLTRIM(m.CodCtc)
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

PROCEDURE TRABAJA_HIJO
*---------------------
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F2  DO edit_item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT itecp
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
*- Mira si tiene HC
IF !FOUND() AND !EMPTY(M.numhc) AND dehc
	SELECT itehc
	SEEK ALLTRIM(M.nummeshc)+M.numhc
	SCAN WHILE nummes=ALLTRIM(M.nummeshc) AND numhc=M.numhc AND ESTADO<>'92' AND ESTADO<>'99'
		SELECT itecp
		IF f_appd()
			REPLACE nummes WITH ALLTRIM(m.nummes), numcp WITH m.numcp, ;
    				codpart WITH itehc.codpart, impparc WITH ITEHC.valpart,codctc WITH m.codctc ,codanal WITH itehc.codanal,;
    				periodo with m.periodo ,tipfun with m.tipfun,codfte with m.codfte,tipdoc with m.tipdoc,ESTADO WITH '00'
		ENDIF
		UNLOCK
        USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
		SELECT salpar
		SEEK itecp.codpart+itecp.nummes
		REPLACE mtoeje WITH mtoeje + itecp.impparc
		UNLOCK
		USE IN 5
		SELECT itehc
	ENDSCAN
	SELECT itecp
ENDIF
IF ALLTRIM(m.tipfun)='F'
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
		codanal    :H= 'Partida' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal) :R,;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) :R,;
		impparc    :H= 'Monto'   :v=actual():F :p='9,999,999.99' :R
ELSE
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
		codpart    :H= 'Partida' :v=val_part(SUBSTR(codpart,4,2),LEFT(codpart,2),'codpart'):F :W=EMPTY(codpart) :R,;
		codanal    :H= 'Analitc' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal) :R,;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) :R,;
		impparc    :H= 'Monto'   :v=actual():F :p='9,999,999.99' :R
ENDIF
GO TOP
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
SCAN WHILE nummes=alltrim(m.nummes) and numcp = m.numcp and codctc= alltrim(m.codctc)
	IF EMPTY(itecp.codanal) OR itecp.impparc = 0.00
		DELETE NEXT 1
	ELSE
		IF RLOCK()
		   REPLACE codcal WITH m.codcal
		ENDIF
		UNLOCK
	ENDIF
ENDSCAN
SELECT compag
UNLOCK ALL
ON KEY
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT compag
RETURN


PROCEDURE traba_hijo
*-------------------
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f2  DO edit_item
ON KEY LABEL f5  DO agreg_item
ON KEY LABEL f8  DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELE itecp
SEEK ALLTRIM(m.nummes)+m.numcp + ALLTRIM(m.codctc)
*- Mira
IF !FOUND()
	DO agreg_item
ENDIF
IF ALLTRIM(m.tipfun)='F'
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
		codanal    :H= 'Partida' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal),;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) ,;
		impparc    :H= 'Monto'   :v=actual():F :p='9,999,999.99'
ELSE
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
		codpart    :H= 'Partida' :v=val_part(SUBSTR(codpart,4,2),LEFT(codpart,2),'codpart'):F :W=EMPTY(codpart),;
		codanal    :H= 'Analitc' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal),;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) ,;
		impparc    :H= 'Monto'   :v=actual():F :p='9,999,999.99'
ENDIF
GO TOP
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
SCAN WHILE nummes=alltrim(m.nummes) and numcp = m.numcp and codctc= alltrim(m.codctc)
		IF RLOCK()
			REPLACE codcal WITH m.codcal
			UNLOCK
		ENDIF
ENDSCAN
SELECT compag
UNLOCK ALL
ON KEY
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT compag
RETURN


PROCEDURE agreg_item
*-------------------
SELECT itecp
IF f_appd()
	REPLACE  nummes WITH m.nummes, numcp WITH m.numcp,impparc WITH 0,codctc WITH m.codctc,periodo with m.periodo ,tipfun with m.tipfun,codfte with m.codfte,tipdoc with m.tipdoc, ESTADO WITH '00'
	DO CASE
	   CASE TIPDOC='ME'
	        IF XFLAG="S"
	           REPLACE CODPART WITH '00.00',CODANAL WITH '18.01'
            ENDIF
   	   CASE TIPDOC='RE'
            REPLACE CODPART WITH '00.00',CODANAL WITH '16.01'
    ENDCASE 	
ENDIF
IF RLOCK()
	USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
	SELECT salpar
	SEEK itecp.codpart+itecp.nummes
	REPLACE mtoeje WITH mtoeje + itecp.impparc
	UNLOCK
	USE IN 5
ENDIF	
SELECT itecp
RETURN .T.


FUNCTION actual
*--------------
vAli=ALIAS()
vOrd=ORDER()
IF RLOCK()
	USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
	SELECT salpar
	SEEK itecp.codpart+itecp.nummes
	REPLACE mtoeje WITH mtoeje + itecp.impparc
	UNLOCK
	USE IN 5
ENDIF	
SELECT (vAli)
SET ORDER TO vOrd
RETURN .T.

PROCEDURE elimi_item
*-------------------
SELECT itecp
IF RLOCK()
    USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
	SELECT salpar
	SEEK itecp.codpart+itecp.nummes
	REPLACE mtoeje WITH mtoeje - itecp.impparc
	USE IN 5
	SELECT itecp
	DELETE NEXT 1
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
RETURN


PROCEDURE edit_item
*------------------
SELECT itecp
SCATTER MEMVAR
DEFINE WINDOW editacp  FROM 12,20 TO 16,60 COLOR SCHEME 10
ACTIVATE WINDOW editacp
USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
@ 1,2 SAY "Partida:"  GET m.codpart PICTURE "@!" VALID verpar()
@ 2,2 SAY "  Monto:"  GET m.impparc PICTURE "99,999,999.99" &&VALID &&BajSal()
READ
IF LASTKEY()#27
    IF RLOCK()
		USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
		SELECT salpar
		SEEK itecp.codpart+itecp.nummes
		REPLACE mtoeje WITH mtoeje - itecp.impparc
		SEEK m.codpart+m.nummes
		REPLACE mtoeje WITH mtoeje + itecp.impparc
		USE IN 5
	ENDIF	
	SELECT itecp
	GATHER MEMVAR
ENDIF
RELEASE WINDOW editacp
RETURN

PROCEDURE TOTAL_PADRE
*--------------------
SELECT itecp
SEEK ALLTRIM(m.nummes) + m.numcp + ALLTRIM(m.codctc)
SCAN WHILE nummes = ALLTRIM(m.nummes) .AND. numcp = m.numcp .AND. codctc = alltrim(M.codctc)
	vtotal = vtotal + impparc
ENDSCAN
RETURN VTOTAL 


PROCEDURE anula
*---------------
SELECT compag
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
IF ESTADO="99" 
	DO standby WITH vmens09
	RETURN
ENDIF
velimina = yesno('¨ Desea ANULAR este C/P ?') 
IF velimina .AND. ( RLOCK() .OR. f_lock(1) )
	REPLACE estado WITH '99'
	SELECT Hoja
	IF SEEK(m.NumMesHC+m.NumHc)
	 IF RLOCK() .OR. F_LOCK(1)
	   REPLACE Hoja.Nummescp with '  ',Hoja.NumCP WITH '    ',HOJA.ESTADO WITH '20' 
	   WAIT "H/C Liberado" window
	 ENDIF  
	UNLOCK
	ENDIF
	SELE ITECP
    SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
    IF FOUND()
		REPLACE estado WITH '99'
    ENDIF
    USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
    sele astpat
    SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
    IF FOUND()
    	scan while nummes=alltrim(m.nummes) and numref=m.numcp and codctc=m.codctc and tipdoc="C/P" 
		    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
	   		ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF	    
	SELECT CHEQUE
	SEEK m.nummes+m.numcp+m.codctc    
    IF FOUND()
		scan while nummes=alltrim(m.nummes) and numcp=m.numcp and codctc=m.codctc 
		    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
	   		ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
   	USE reten  IN 12  ORDER TAG reten1  ALIAS reten
	SELECT reten
	SEEK m.nummes+m.numcp+m.codctc    
    IF FOUND()
		scan while nummes=alltrim(m.nummes) and numcp=m.numcp and codctc=m.codctc 
		    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
	   		ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
    USE IN 09
    USE IN 12
    USE astpre   IN 13  ORDER TAG astpre17      ALIAS astpre  
    sele astpRE
    SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
    IF FOUND()
    	SCAN while nummes=alltrim(m.nummes) and numref=m.numcp and codctc=m.codctc and tipdoc="C/P" 
		    IF RLOCK() .OR. F_LOCK(1)
		       DELETE NEXT 1
		    ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
    USE IN 13
    USE salpar   IN  5  ORDER TAG salpar1      ALIAS salpar &&Saldo de  Partidas
    SELECT itecp
    SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
    IF FOUND()
    	scan while nummes=alltrim(m.nummes) and numCP=m.numcp and codctc=m.codctc 
    		IF RLOCK()
				SELECT salpar
				SEEK itecp.codpart+itecp.nummes
				REPLACE mtoeje WITH mtoeje - itecp.impparc
	    	ENDIF       
    		UNLOCK
	    ENDSCAN
    ENDIF
	USE IN 5
	SELECT Compag
	DO vista
ENDIF
UNLOCK
RETURN


PROCEDURE lista
*--------------
USE IN  6 
USE IN  7
USE IN  8
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELECT compag
VRECNO=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW lis FROM 3,10 TO 23,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vestado,vlista
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli
VCTA=M.CODCTC
VANO=M.NUMMES
VCLI=M.NUMCP
STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte;Operaciones;C/P-Cheque;C/P-Detalle Gasto;Pr‚stamos'
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_CP()
@ 07,01 SAY "              Mes : " GET vMes    PICTURE '!!'  VALID val_para(vMes  ,'FECMES',' ',27,9) WHEN vLista=3 OR VLISTA=4 or vlista=5 
@ 09,01 SAY "   Cta. Corriente : "
@ 09,22 GET vcta  WHEN  vlista=1 OR VLISTA=2 OR VLISTA=6 VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,10,07)
@ 12,01 SAY "           Estado : " GET vestado  FUNCTION '^ General;Anuladas' WHEN vlista=2
@ 15,01 SAY " Fecha de Emisi¢n : "
@ 16,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista=2 OR VLISTA=6
@ 16,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista=2  OR VLISTA=6
@ 18,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
   XIND=SYS(3)+".IDX"
	DO CASE 
	   CASE vLista = 1
		    VKEY=ALLTRIM(VANO)+VCLI+ALLTRIM(VCTA)
    		SELE COMPAG
    		SEEK VKEY
    		DO repPRG WITH "LISSE1"," Listado Comprobante de Pago ",2
	   CASE vLista = 2
	        DO CASE
	           CASE VESTADO=1
				   sele compag
				   SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
	        	   SET FILTER TO codctc=vcta AND BETWEEN(fecCP,vfecini,vfecfin) AND ESTADO<>"99"
         	       VTITULO = 'REGISTRADOS'
                   DO reporte WITH 2,"lisCP"," Resumen de Comprobantes de Pago Registradas por Cta.Cte. "                                  	       
               CASE VESTADO=2
				   sele compag
				   SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
			      	SET FILTER TO codctc=vcta AND BETWEEN(fecCP,vfecini,vfecfin) AND ESTADO="99"
       	           VTITULO = 'ANULADOS'
                   DO reporte WITH 2,"lisCP"," Resumen de Comprobantes de Pago Anuladas por Cta.Cte. "                                  	       
		     ENDCASE
		     sele compag
	         set rela off into cheque
      CASE vLista = 3
                SELECT AstPat
                SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") .AND. TipDoc = 'C/P'
                SET RELATION TO nummes+numref+codctc  INTO Compag ADDI
                SET RELATION TO nummes+numref+codctc  INTO Cheque ADDI
                DO REPORTE WITH 2,"LisCPOpe"," Listado de Operaciones de Comprobantes de Pago "
                SET FILTER TO
			    SET RELATION OFF INTO cheque	
	  CASE vLista = 4		    
		   sele compag
		   SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
      	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") 
           DO reporte WITH 2,"lisgeCP"," Listado General de Comprobantes de Pago "                                  	       
	       set filter to
		   sele compag
	       set rela off into cheque
	  CASE vLista = 5
		   sele itecp
		   SET   ORDER TO  ITECP1
		   SET RELATION TO nummes+numcp+codctc  INTO compag  ADDI
      	   SET FILTER TO NumMes =PADL(ALLTRIM(vMes),2,"0") 
           DO reporte WITH 2,"estadCP"," Listado General de Comprobantes de Pago "                                  	       
	       set filter to
		   sele itecp
	       set rela off into compag
	  CASE vLista = 6
			USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
			SELECT astpat
			INDEX ON NUMMES+NUMREF+CODCTC TO (XIND) FOR codctc=vcta AND BETWEEN(fecHA,vfecini,vfecfin)	AND LEFT(CODCTA,3)='389' AND TIPDOC='C/P'
            SET RELATION TO nummes+numref+codctc  INTO Compag ADDI
            SET RELATION TO nummes+numref+codctc  INTO Cheque ADDI
			      	IF EOF()
		              DO STANDBY WITH "Registros no existen"
		     	      SET INDEX TO
		     	      USE IN 9
		     	      SELE compag
		     	      GO BOTT
		     	      RETURN
		     	   ENDIF   
      	           VTITULO = 'PRESTAMOS'
                   DO reporte WITH 2,"lisCP3"," Resumen de Comprobantes de Pago de Prestamos por Cta.Cte. "                                  	       
                   SET INDEX TO
                   USE IN 9
    ENDCASE
	SELE COMPAG
ENDIF
USE IN 9
USE IN 14
USE hojcon   IN  6  ORDER TAG hojcon1      ALIAS hoja   &&H/C
USE itehc    IN  7  ORDER TAG itehc1       ALIAS itehc 
USE clase    IN  8  ORDER TAG clase1       ALIAS clase   
SELECT compag
GO VRECNO
DO VISTA
RETURN

PROCEDURE val_cp             && Revisi¢n de BD en browse
*---------------
SELE compag
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
	x1=numcp+'.'+nummes  :H=' N§ CP' ,;
	feccp,;
	codctc :H='CtaCte',;
	tipprv,;
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20))) :H="Nombre",;
	codfte :H='Fte.Fin.',;
	IMPORT :H='Total CP',;
	x3=NumHC+'.'+NumMesHC :H=' N§ H/C',;
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
vcli = numcp
vcta = codctc

ON KEY LABEL f10
SELE compag
RETURN

PROCEDURE LISSE1 &&Programa Reporte usa repprg F.M.V.
*--------------
PARAMETERS XCOP
PRIVATE FILA,XVAL,TVAL,VPAR,TOTALQ,LVAL
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
*FOR I=1 TO XCOP
	STORE 0 TO XVAL,TVAL,FILA
	USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
	USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
	SELE COMPAG
	vano=alltrim(nummes)
	vcli=numcp
	vcta=codctc
	Vkey=COMPAG.NUMMES+COMPAG.NUMCP+COMPAG.CODCTC
	LVAL=0
	@0,0 SAY CHR(18)
	@1,3   SAY ALLTRIM(CIA)
	@1,68  say "PAG:"
	@1,76  SAY ALLTRIM(STR(_PAGENO,8))
	@2,3   SAY "Lisse1"
	@2,68 SAY "FECHA:"
	@2,76 SAY DATE()           
	@4,20 SAY CHR(14)
	@4,22 SAY "<< COMPROBANTE DE PAGO >>"
	@5,0 SAY CHR(27)+CHR(18)
	@06,03 SAY "      N§ C/P :"  
	@06,20 SAY NUMCP+'.'+NUMMES
	@06,68 SAY "Estado :"  
	@06,76 SAY IIF(Estado='00','Emitido',IIF(Estado='20','Girado',IIF(Estado='99','Anulado',IIF(Estado='50','Pagado',' -  '))))
	@07,03 SAY "   Fecha C/P :"  
	@07,20 SAY FECCP picture "@D"
	@08,03 SAY "         Son :"  
	totalq = 0
	
	USE reten IN 12  ORDER TAG reten1  ALIAS reten
	SELECT RETEN
	SEEK VKEY
	IF FOUND()
		SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
   			totalq = totalq + valret
	    ENDSCAN
	ENDIF    
	USE IN 12
	
	SELE COMPAG
	@08,20 SAY LETRAS(IMPORT-TOTALQ,'SOLES')
	@09,03 SAY "Razon Social :"  
	@09,20 SAY IIF(TIPDOC="RE",codret,IIF((TIPPRV='O' OR EMPTY(TIPPRV))," ",IIF(TIPPRV='P',CODPRV,CODEMP)))
	@09,27 SAY IIF(TIPDOC="RE",VAL_PARA(Codret,'CODRET','D',28,40),IIF((TIPPRV='O' OR EMPTY(TIPPRV)),compag.NOMPRE,IIF(TIPPRV='P',VALPRV3(CODPRV)+' '+IIF(!EMPTY(NOMPRE),NOMPRE,' '),VALEMP3(CODEMP)+' '+IIF(!EMPTY(NOMPRE),NOMPRE,' '))))
	@10,03 SAY "  Referencia :"  
	@10,20 SAY docref
	@10,24 SAY numref
	*@10,35 SAY IIF(!EMPTY(DOCREF),fecref,' ')
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
	@18,40 SAY "C O N C E P T O" 
	@19,01 SAY CHR(15)
	@19,03 SAY SUBSTR(GLOSA,1,130)
	@20,03 SAY SUBSTR(GLOSA,131,70)
	@21,32 SAY CHR(18)
	@21,40 SAY "OBSERVACIONES" 
	@22,01 SAY CHR(15)
	@22,03 SAY SUBSTR(OBSERV,1,100)
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
	@28,20 SAY "<< ESTADISTICA DIARIA OBJETO DEL GASTO >>"
	@29,03 SAY "Partida"
	@29,30 SAY "Analitico"
	@29,50 SAY "Parcial"
	@29,72 SAY "Total"
	SELE ITECP
	SEEK  VKEY
	@30,03 SAY ITECP.CODPART
	FILA=30
	SCAN WHILE NUMMES=ALLTRIM(VANO) AND NUMCP=VCLI AND CODCTC=ALLTRIM(VCTA)
    	 IF fila>=31
        	IF SUBSTR(CODANAL,1,2) <> vPar
           		FILA=FILA-1 
		       @fila,60 say xval PICTURE '@Z 999,999,999,999.99'
		       FILA=FILA+1
	    	   xval=0
	        ENDIF
			*        FILA=FILA+1
	     ENDIF           
    	 @FILA,11 SAY IIf(LEFT(IteCP.CodAnal,2)='18' AND RIGHT(IteCP.CodAnal,2)='01','ASIGNACION',IIF(LEFT(IteCP.CodAnal,2)='16' AND RIGHT(IteCP.CodAnal,2)='01','RETENCIONES',' '))
	     @FILA,30 SAY ITECP.CODANAL
	     @FILA,40 SAY ITECP.IMPPARC PICTURE '@Z 999,999,999,999.99'	
	     vpar=substr(codanal,1,2)
	     xval=xval+ITECP.impparc
	     tval=tval+ITECP.impparc
	     FILA=FILA+1
	ENDSCAN     
	SELE COMPAG
	FILA=FILA-1
	@fila,60 say xval PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+2
	@FILA,40 SAY "      TOTAL :"    
	@fila,60 say Tval PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+1	
	@FILA,40 SAY "DEDUCCIONES :"    
	@FILA,60 SAY TOTALQ PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+1
	@FILA,40 SAY "    LIQUIDO :"    
	@FILA,60 SAY IMPORT-TOTALQ PICTURE '@Z 999,999,999,999.99'
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
	FILA=FILA+1
	@FILA,3 SAY "Ú"
	@FILA,4 SAY REPLICATE("Ä",20)
	@FILA,24 SAY "¿" 
	FILA=FILA+1
	@FILA,3 SAY "³"
	@FILA,4 SAY " RECIBI CONFORME"
	@FILA,24 SAY "³"
	FILA=FILA+1
	@FILA,3 SAY "À"
	@FILA,4 SAY REPLICATE("Ä",20) 
	@FILA,24 SAY "Ù"
	FILA=FILA+1
	@FILA,1 SAY "FIRMA :"
	@FILA,27 SAY "FORMA DE PAGO"
	FILA=FILA+1
	@FILA,1 SAY " L.E. :"
	@FILA,26 SAY REPLICATE("-",16)
	@FILA,58 SAY REPLICATE("-",14)
	FILA=FILA+1
	@FILA,1 SAY "FECHA :"
	@FILA,10  SAY "  /  /"         
	@FILA,22 SAY "N§ CHEQUE :"     
	SELE CHEQUE
	SEEK VKEY
	@FILA,38 SAY iif(empty(CHEQUE.numchq),' Sin Cheque',CHEQUE.numchq)
	SELE COMPAG
	@FILA,58 SAY "AUTORIZACION"    
	FILA=FILA+1
	@FILA,1 SAY "LOGIN:"
	@FILA,20 SAY IIF(M.USUARIO='1001','MILAGROS PERALTA',IIF(M.USUARIO='1002','MARTINA LACHAPELLE',' '))
	USE reten  IN 12  ORDER TAG reten1  ALIAS reten
	SELECT RETEN	
	SEEK VKEY
	IF FOUND()	
		IF _DEST1=2
	    	EJECT
		ENDIF
		@0,0 SAY CHR(18)
		@1,3   SAY ALLTRIM(CIA)
		@1,68  say "PAG:"
		@1,76  SAY ALLTRIM(STR(_PAGENO,8))
		@2,3   SAY "LisRetCP"
		@2,68 SAY "FECHA:"
		@2,76 SAY DATE()           
		@4,20 SAY CHR(14)
		@4,13 SAY "DEDUCCIONES Y/O RETENCIONES"
		@5,0 SAY CHR(27)+CHR(18)
		@5,34 SAY 'C/P: '+VCLI+'.'+ALLTRIM(VANO)
		@6,1 SAY "Ú"
		@6,2 SAY REPLICATE("Ä",70)
		@6,72 SAY "¿" 
		@7,1 SAY "³"
		@7,3 SAY " RUBRO            DESCRIPCION                              MONTO"
		@7,72 SAY "³"
		@8,1 SAY "À"
		@8,2 SAY REPLICATE("Ä",70) 
		@8,72 SAY "Ù"
	    totalq = 0
	    FILA=10
		SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
		     @FILA,03 SAY RETEN.CODRET PICTURE '@J'
	    	 @FILA,15 SAY RETEN.NOMRET
		     @FILA,50 SAY RETEN.VALRET PICTURE '@Z 999,999,999,999.99'
		     FILA=FILA+1
	    	 LVAL=LVAL+RETEN.VALRET
		ENDSCAN     
		FILA=FILA+1
		@FILA,01 SAY REPLICATE("-",71)
		FILA=FILA+1
		@FILA,20 SAY 'T O T A L  ---->'
	    @fila,50 say Lval PICTURE '@Z 999,999,999,999.99'
		FILA=FILA+1
		@FILA,01 SAY REPLICATE("-",71)
	ENDIF
	USE IN 12
	IF _DEST1=2
    	EJECT
	ENDIF
*ENDFOR
SET DEVICE TO SCREEN 
RETURN

PROCEDURE RESCP &&Programa Reporte usa repprg .FMV., v lido cuando se acepten n cheques por C/p
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
SELE COMPAG
GO TOP
fila=7
DO imp_header  
SCAN WHILE !EOF()
     @FILA,03 SAY COMPAG.NUMCP+"."+COMPAG.NUMMES
     @FILA,12 SAY COMPAG.FECCP
     XKEY=ALLTRIM(COMPAG.NUMMES)+COMPAG.NUMCP+ALLTRIM(COMPAG.CODCTC)
     SELE CHEQUE
     SEEK XKEY
     IF FOUND()
*     SCAN WHILE NUMCP=COMPAG.NUMCP AND NUMMES=COMPAG.NUMMES AND CODCTC=COMPAG.CODCTC
    	   	  @FILA,22 SAY CHEQUE.NUMCHQ
	          @FILA,38 SAY SUBSTR(CHEQUE.NOMGIR,1,40)
	          @FILA,80 SAY CHEQUE.VALCHQ  PICTURE "@z 999,999,999,999.99"             
	          XVAL=XVAL+VALCHQ
*          FILA=FILA+1
*   	 ENDSCAN
     ELSE
        @FILA,22 SAY "Sin Cheque"
*       fila=fila+1    	 
     ENDIF   
     sele compag
*     @FILA,22 SAY REPLICATE("- ",40)
 *    FILA=FILA+1
 *   @FILA,22 SAY "Subtotal --->"
 *   @fila,70 say xval picture "@z 999,999,999,999.99"             
 *   FILA=FILA+1
 *   @FILA,02 SAY REPLICATE("-",90)
 *    tval=tval+xval
 *   xval=0
	 IF fila=54
		IF _DEST1=2
           EJECT
		ENDIF
   	    DO imp_header
        fila=7
     ENDIF
     FILA=FILA+1
ENDSCAN
@FILA,02 SAY REPLICATE("=",100)
fila=fila+1
@FILA,02 SAY "TOTAL CTA.CTE.--->"+VCTA
@fila,80 say Tval picture "@z 999,999,999,999.99"             
fila=fila+1
@FILA,02 SAY REPLICATE("=",100)
SET DEVICE TO SCREEN 
RETURN
 
PROCEDURE imp_header  &&Cabecera de Reporte 
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,90 say "PAG:"
@1,95  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "ResCP"
@2,15  SAY "<< RESUMEN DE COMPROBANTES DE PAGO POR CTA.CTE.>>"+"   "+PROPER(VTITULO)
@2,88 SAY "FECHA:"
@2,95 SAY DATE()           
@3,13 SAY "CTA.CTE: --->"+ALLTRIM(Val_Fun('Caja','CodCtc',"ALLTRIM(Codctc)+' '+SUBSTR(Descri,1,26)",VCTA))  
@4,1 SAY "Ú"
@4,2 SAY REPLICATE("Ä",100)
@4,102 SAY "¿" 
@5,1 SAY "³"
@5,3 SAY "   C/P    FECHA    CHEQUE No        RAZON SOCIAL                                         MONTO"
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
RELEASE    WINDOW wind_4
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
RESTORE SCREEN FROM principal
RETURN

FUNCTION VisObs
*--------------
vAlias = ALIAS()
SELECT Hoja
IF !WEXIST("Observa")
   DEFINE WINDOW Observa FROM 03,15 TO 20,65 FLOAT NOCLOSE SHADOW DOUBLE TITLE '± Observaciones ±' FOOTER ' ° ®Esc¯ Sale ° ' COLOR SCHEME 1
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
SELECT (vAlias)
RETURN .T.

PROCEDURE gircheq
*----------------
ON KEY LABEL f5  DO agregchq
ON KEY LABEL f8  DO elimichq
ON KEY LABEL f10 KEYBOARD CHR(23)
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
STORE 0 TO vdebe, vhaber
vaviso=.F.
SCAN WHILE nummes=ALLTRIM(m.nummes) and numref=m.numcp and codctc=alltrim(m.codctc)
	IF RET='S' && suma solo valores del haber para comparar con valor de retencion
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)  
		vhaber= vhaber + mtohab
		vaviso=.T.
	ENDIF	
ENDSCAN
IF  vaviso
    USE reten  IN 12  ORDER TAG reten1  ALIAS reten
   	SELEC RETEN
   	GO TOP
   	set orde to 1
   	seek m.nummes+m.numcp+m.codctc
   	IF EOF()
   		do standby with 'Error, en C/P, entre a corregir el documento'
   		sele compag
   		do vista
   		return
   	ENDIF	
   	totalq = 0
   	SCAN WHILE nummes=ALLTRIM(m.nummes) and numcp=m.numcp and codctc=ALLTRIM(m.codctc)
   		totalq = totalq + valret
   	ENDSCAN
   	IF VHABER-TOTALQ#0
    	DO STANDBY WITH 'La Retenci¢n es diferente a lo registrado en el asiento Patrimonial'
      	selec compag
      	do vista
      	return
   	endif
ENDIF
SELE cheque
SEEK m.nummes+m.numcp+m.codctc
IF !FOUND()
	DO agregchq
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_4 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
	estado     :H= 'ES' :W=.F.,;
	codpart    :H= 'Part.':v=val_part(SUBSTR(codpart,4,2),LEFT(codpart,2),'codpart'),;
	numchq     :H= 'N§ Cheque' ,;
	fecchq     :H= 'Fecha' ,;
	nomgir     :H= 'Girado a' :p='@S26' :v = valeto() :F ,;
	valchq     :H= 'Monto' :p='99,999,999.99'
GO TOP
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
SCAN WHILE NUMMES=ALLTRIM(m.nummes) AND NUMCP=m.numcp AND CODCTC=ALLTRIM(m.codctc)
	IF EMPTY(numchq)
		DELE NEXT 1
	ENDIF
ENDSCAN
SELE compag
IF RLOCK()
   DO CASE
	  CASE tipdoc='HC' 
   		   repla estado with '20'
	  CASE tipdoc='RE'
   			repla estado with '10'
   	  CASE tipdoc='ME'
   		   IF yesno("Pertenece a HC o RE (Si='HC' No='RE')")
   			  repla estado with '20'
   		   ELSE
   			  repla estado with '10'
   		   ENDIF
   	ENDCASE				
ENDIF
USE IN 12
SELE COMPAG
DO vista
RETURN


PROCEDURE regret
*---------------
ON KEY LABEL f5  DO agregret
ON KEY LABEL f8  DO elimiret
ON KEY LABEL f10 KEYBOARD CHR(23)
USE reten    IN 12  ORDER TAG reten1       ALIAS reten
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+alltrim(m.codctc)
STORE 0 TO vdebe, vhaber
SCAN WHILE nummes=ALLTRIM(m.nummes) and numref=m.numcp and codctc=alltrim(m.codctc)
	IF RET='S' && suma solo valores del haber para comparar con valor de retencion
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
	ENDIF
ENDSCAN
IF VHABER = 0
   DO STANDBY WITH 'No existe retenciones registradas'
   selec compag
   do vista
   return
endif
SELE reten
SEEK m.nummes+m.numcp+m.codctc
vale=0
IF !FOUND()
	DO agregret
ENDIF

BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_5 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
	estado     :H= 'ES' :W=.F. :R ,;
	fecret     :H= 'Fecha':V=Fecha() ,;
	codret     :H= 'Cod'  :V=VALNOM()  ,;
	nomret     :H= 'Girado a':p='@S39':R,;
	valret     :H= 'Monto' :p='99,999,999.99' 
GO TOP
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
totalq = 0
SCAN WHILE nummes=ALLTRIM(m.nummes) AND numcp=m.numcp AND codctc = m.codctc
	IF valret = 0.00 OR EMPTY(codret)
	   DELE NEXT 1
	ELSE
       totalq = totalq + valret
	ENDIF	
ENDSCAN
DO CASE
CASE vhaber > totalq
	DO standby WITH 'Lo registrado en Retenciones es menor a C.Patr.'
	SELE compag
	IF RLOCK()
	   REPLACE estado WITH '00'
	ENDIF
CASE vhaber < totalq
	SELE compag
	DO standby WITH 'La C.Patr. es menor a la Retenciones registradas'
	IF RLOCK()
		REPLACE estado WITH '00'
	ENDIF
CASE vhaber = totalq
	SELE compag
	DO standby WITH 'Retenciones O.K.'
	IF RLOCK()
		REPLACE estado WITH '10'
	ENDIF
ENDCASE
SELE compag
USE IN 09  
USE IN 12
DO vista
RETURN


FUNCTION Fecha
*-------------
vFecha = FecRet
RETURN


FUNCTION valNOM
*---------------
YY=val_para(codret,'CODRET',' ')
IF RLOCK()
    YY=ALLTRIM(PARMA.CODIGO) 
    ZZ=PARMA.DESCRI
	REPLACE RETEN.codret WITH YY, RETEN.nomret WITH ZZ
ENDIF
UNLOCK
RETURN 

PROCEDURE agregchq
*-----------------
PUBLIC vdes
vdes=IIF(empty(m.codprv) and empty(m.codemp),m.nompre,iif(!empty(m.codemp),valemp1(m.codemp),valprv1(m.codprv)))
SELECT cheque
IF f_appd()
	REPLACE nummes  WITH m.nummes,;
    		numcp   WITH m.numcp,;
	   	    codctc  WITH m.codctc,;
		    fecchq  WITH DATE(),;
		    nomgir  WITH VDES,;
            estado  WITH '00',;
		    codcal  WITH m.codcal,;
		    codpart WITH ITECP.codpart
ENDIF
RETURN


PROCEDURE elimichq
*-------------------
SELECT cheque
IF RLOCK()
	DELE NEXT 1
ENDIF
RETURN


PROCEDURE agregret
*-----------------*/
SELECT reten
IF f_appd()
	REPLACE nummes WITH m.nummes ,;
	    fecret with vfecha,;
		numcp  WITH m.numcp  ,;
		codctc WITH m.codctc ,;
		fecret WITH m.feccp  ,;
		estado WITH '00'     ,;
		codcal WITH m.codcal
ENDIF
RETURN


PROCEDURE elimiret
*-----------------
SELECT reten
IF RLOCK()
	DELE NEXT 1
ENDIF
RETURN

FUNCTION verpar
*--------------
PRIVATE vfun, vpart
vpart=m.codpart+compag.nummes
vfun = val_fun('SalPar','CodPart+NumMes',;
	"CodPart+' '+NumMes+' AG: '+TRANSF(MtoAG,'99,999,999.99')+;
	' Saldo '+TRANSF(mtoag-mtoeje,'99,999,999.99')",vPart,1)
m.codpart = LEFT(vpart,5)
USE IN 5
RETURN vfun


PROCEDURE GRABA
*--------------
REPLACE CODCTC WITH m.CODCTC
RETURN .T.

PROCEDURE VALE
*-------------
IF EMPTY(M.CODCTC) 
   DO STANDBY with 'No esta asignada la Cuenta Corriente'
*  DELE NEXT 1
   return .F.
else
   return .t.
endif

FUNCTION VALPRV
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
private  malias, v_fun, _oldwind,_campo
_campo = varread()
malias = alias()
select AUXIL
_oldwind = woutput()
   SET FILT TO TIPO='20'  && POR EL MOMENTO PUES COJE EL PRIMER CODIGO
   seek xcod   
   IF FOUND()
      @3,30 SAY DESCRI
      m.codprv=xcod
      V_FUN=.T.
   ELSE   
   *SET FILT TO TIPO='20'
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


FUNCTION VALEMP
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
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


FUNCTION valprv1
*--------------
parameter prov,MALIAS
malias = alias()
PRIVATE VDES
IF prov='0000' .OR. EMPTY(prov)
ELSE   
   sele auxil
   SET FILT TO TIPO='20'
   seek prov
   vdes = iif(found(),Descri,"")
ENDIF
SET FILT TO
SELE (MALIAS)
RETURN vdes


FUNCTION valemP1
*--------------
parameter emp
PRIVATE VDES,MALIAS
MALIAS=ALIAS()
IF emp='0000' .OR. EMPTY(emp)
ELSE   
   sele auxil
   SET FILT TO TIPO="30"
   seek emp
   vdes = iif(found(),Descri,"")
ENDIF
SET FILT TO
SELE (MALIAS)
RETURN vdes


FUNCTION valprv3
*--------------
parameter prov
PRIVATE xx, vfun , MALIAS
malias = alias()
*SET ORDE TO 2
IF prov='0000' .OR. EMPTY(prov)
ELSE   
   sele auxil
   SET FILT TO TIPO="20"
   seek prov
   v_fun = iif(found(),Descri,"")
   SET FILT TO 
ENDIF
SELE (MALIAS)
SELE COMPAG
RETURN v_fun


FUNCTION valemP3
*--------------
parameter emp
PRIVATE xx, vfun , MALIAS
MALIAS=ALIAS()
*SET ORDE TO 2
IF emp='0000' .OR. EMPTY(emp)
ELSE   
   sele auxil
   SET FILT TO TIPO="30"
   seek emp
   v_fun = iif(found(),Descri,"")
   SET FILT TO
ENDIF
SELE (MALIAS) 
RETURN v_fun

PROCEDURE valeto
*---------------
vtemp = RECNO()
cx = ALIAS()
STORE 0 TO vale,vale1
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
SCAN WHILE nummes = ALLTRIM(m.nummes) AND numcp = m.numcp AND !EMPTY(numchq) AND codctc = m.codctc
	vale = vale + valchq
ENDSCAN
USE reten IN 12 ORDER TAG reten1 ALIAS reten
SELE reten
SEEK m.nummes+m.numcp+m.codctc
SCAN WHILE nummes = ALLTRIM(m.nummes) AND numcp = m.numcp  AND codctc = m.codctc
	vale1 = vale1 + valret
ENDSCAN
USE IN 12
SELE (cx)
GO vtemp
IF m.import - (vale+vale1) <= 0
*	DO standby WITH 'El monto Total ya est  girado'
*    RETURN .F.
*ELSE
	IF RLOCK()
		REPLACE valchq WITH m.import-(vale+vale1)
	ENDIF
	UNLOCK
	SELE compag
	IF EMPTY(numchq) AND RLOCK()
		REPLACE numchq WITH cheque.numchq
	ENDIF
	UNLOCK ALL
	SELE (cx)
	RETURN .T.
ENDIF
RETURN

PROCEDURE imp_cp
*---------------
PUBLIC NUM_C
NUM_C=1
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
_DEST = 'Impresora'
P_FIL = SPACE(8)
_Dest1 = 2
P_FIL = sys(3)+".LST"
IMPRE = (_DEST='Impresora')
PRINTING = .T.
DEFINE WINDOW Numcop FROM 20,50 TO 22,79 DOUBLE
ACTIVATE WINDOW Numcop
@ 0,3 SAY "N§ de Copias : " GET NUM_C PICTURE "99" VALID NUM_C>0 AND NUM_C<99
READ
RELEASE WINDOW Numcop
IF LASTKEY()=27
   RETUR
ENDIF 
IF IMPRE
   IF !EMPTY(LEFT(SYS(0),15))
     IF !YESNO("¨Imprime en impresora local?  <NO = IMPRESORA DE RED>")
       **-- Impresora de red.
       SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
       SET PRINTER TO \\SPOOLER\NB
     ENDIF
   ENDIF
   IF !READY2PR()
       PRINTING = .F.
   ENDIF
ENDIF   
IF PRINTING
  IF IMPRE
   	 SET DEVICE TO PRINT
     SET PRINTER TO &p_fil
     DO LISSE1 WITH NUM_C
     SET PRINTER TO
     IF READY2PR() 
     	FOR I=1 TO NUM_C
        	!TYPE &P_FIL >PRN
        ENDFOR	
     ENDIF
     ERASE &p_fil
   ENDIF
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF
 SET DEVICE TO SCREEN
 SELE COMPAG
RETURN

PROCEDURE xrepo_cp
*---------------
PARAMETER _tipo, _form, _tit
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING
_DEST = 'Impresora'
P_FIL = SPACE(8)
_Dest1 = 2
P_FIL = sys(3)+".LST"
IMPRE = (_DEST='Impresora')
PRINTING = .T.
IF IMPRE
   IF !EMPTY(LEFT(SYS(0),15))
     IF !YESNO("¨Imprime en impresora local?  <NO = IMPRESORA DE RED>")
       **-- Impresora de red.
       SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
       SET PRINTER TO \\SPOOLER\NB
     ENDIF
   ENDIF
   IF !READY2PR()
       PRINTING = .F.
   ENDIF
ENDIF   
IF PRINTING
   IF IMPRE
     SET DEVICE TO PRINT
   	 REPORT FORM &_FORM  NOEJECT TO priNT
     SET PRINTER TO &p_fil
     SET PRINTER TO
     IF READY2PR() 
        !TYPE &P_FIL >PRN
     ENDIF
     IF LASTKEY()=27
        EXIT
     ENDIF
     ERASE &p_fil
   ENDIF
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF
 SET DEVICE TO SCREEN
 RETURN
