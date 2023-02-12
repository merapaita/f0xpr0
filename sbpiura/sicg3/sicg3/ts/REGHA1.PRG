PARAMETERS newsistem

* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegHa.PRG  14/06/96                              L: 1822     ³	
* ³ Registro de Hojas de Anulaci¢n de Cheques                    ³
* ³ Modificado por A.S. Marco Rodr¡guez Abad                     ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾
*- Abriendo Archivos
*- Estados
*- 00 Emitido
*- 99 Anulado
*- Cuando en C/P el campo Pres=* ,ya tiene H/A
*- Cuando en C/P el campo Pres= ,el cheque ya fue entregado
CLOSE DATA
USE parmae   IN  1  ORDER TAG parmae1   ALIAS parma
USE hojanu   IN  6  ORDER TAG hojanu1	ALIAS hojanu
USE itehA    IN  7  ORDER TAG itehA1    ALIAS itehA
USE compag   IN  3  ORDER TAG compag1   ALIAS compag
USE clase    IN  8  ORDER TAG clase1    ALIAS clase
USE astpat   IN  9  ORDER TAG astpat18  ALIAS astpat
USE cajas    IN 10  ORDER TAG cajas1    ALIAS caja
USE cheque   IN 11  ORDER TAG cheque1   ALIAS cheque
USE Hchqpen  IN 12  ORDER TAG hchqpen2  ALIAS hchqpen
USE astpre   IN 13  ORDER TAG astpre19  ALIAS astpre
USE AstOrd	 IN 17	ORDER TAG AstOrd1   ALIAS AstOrd
USE AsiAut   IN 18  ORDER TAG AsiAut1
USE Auxil    IN 15  ORDER TAG Auxil1    ALIAS Auxi
USE maepre   IN 16  ORDER TAG maepre1   ALIAS maepre
USE itepar   IN 25  ORDER TAG itepar1   ALIAS Itepar
SELECT HojAnu
GO BOTTOM
*- Mensajes de aviso al usuario
vmens01 = ' Hojas de Anulaci¢n de Cheque: REVISION '
vmens02 = ' ®F4¯ Imprime H/A       ®F9¯  Corrige Asientos Patrimoniales  '
vmens04 = 'Dicha H/A no fue encontrada'
vmens05 = 'No existe H/A anterior'
vmens06 = 'No existe H/A siguiente'
vmens07 = '¨ Desea Anular esta H/A ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta H/A ha sido anulada'
vmens10 = 'La H/A ya est  Atendida'
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Variables de trabajo (registro a trabajar)
PUBLIC vDescri,vNumMes,vNumRef,vCodCtc,vAlo,vCta,vTitulo,vCon1,vDeb,vFlag,vdes,w_TipCtc,VCODCAD
PUBLIC vFecha,vKey,X,mMonto,vTot,vPar,vPto,w_ctaD,w_ctaH,sw,VUSER_ID,wuser_id,DESTIW
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
store 0 TO mMonto,vTot,vmondeb,x,sw
vFlag=.T.
vFecha = DATE()
STORE '  ' TO vDescri, vnummes,vKey
STORE SPACE(12) TO w_ctaD,w_ctaH
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

DEFINE WINDOW wind_1 FROM 00,00 TO 13,79  DOUBLE ;
	TITLE vmens01 COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 14,00 TO 23,79 DOUBLE ;
	FOOTER "F5] AGREGA    [F10] GRABAR " TITLE '         Detalle de Hoja de Anulacion de Cheque' COLOR SCHEME 10

DEFINE WINDOW WIND_3 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
	TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 02

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
ON KEY LABEL F9 DO cor_as
ON KEY LABEL f4 DO imp_ha
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
@ 04,40 SAY "    Fecha Emisi¢n :"
@ 05,02 SAY "Fte.Financiamient :"
@ 06,02 SAY " Cadena Funcional :"
*@ 06,02 SAY "         Programa :"
*@ 07,02 SAY "      SubPrograma :"
*@ 08,02 SAY " Proyecto/Activid :"
@ 09,02 SAY "          Destino :"
@ 10,02 SAY "    Justificaci¢n :"
@ 11,00 SAY PADC(ALLTRIM(vmens02),79,' ') COLOR W+/B
RETURN

PROCEDURE vista              && Displaya datos de HojAnu
*--------------
SELECT HojAnu
SET RELATION TO NumMescp+Numcp INTO compag
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
ON KEY LABEL F9 DO cor_as
IF newsistem='1'
	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
ELSE
	=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
ENDIF
@  0,60 SAY verestha(m.estado)  COLOR SCHEME 2
@  1,22 SAY m.numha
@  1,26 SAY '.'
@  1,27 SAY m.nummes
@  1,60 SAY m.fechan
@  2,22 SAY m.codctc

DO CASE

	CASE m.TipDoc$'RESRRURSAR'
    	@ 0,01 SAY IIF(m.tipdoc='RE','Retenci¢n',IIF(m.tipdoc='SR','Ret.Sector',IIF(m.tipdoc='AR','Ret.Activid',IIF(m.tipdoc='RU','Ret.Subsid','Ret.Sec.Sub')))) COLOR SCHEME 05
	    @ 3,22 SAY VAL_PARA(m.codret,'CODRET','V',22,40)

	CASE m.TipDoc$'MESUHCSS'
	    @ 0,01 SAY IIF(m.tipdoc='ME','Memorandum',IIF(m.tipdoc='HC','Hoja Control',IIF(m.tipdoc='SU','Subsidios','Sect.Sub'))) COLOR SCHEME 05
	    @ 3,22 SAY SPACE(60)
	    =pres_pro()
       	
	CASE m.TipDoc$'SESHAH'  
	    @ 0,01 SAY IIF(m.tipdoc='SH','Sector.H/C','Activid.H/C') COLOR SCHEME 05
	    @ 3,22 SAY SPACE(60)
	    =pres_pro()
		
	CASE m.TipDoc$'HMRGIN'
	    @ 0,01 SAY IIF(m.tipdoc='HM','Hoja Mod.',IIF(m.tipdoc='RG','Regularizac. H/C','Inutilizado')) COLOR SCHEME 05
	    @ 3,22 SAY SPACE(60)
    	@ 3,22 SAY m.nompre     

ENDCASE

IF !EMPTY(m.numcp)
	@  4,22 SAY m.numcp+'.'+m.nummescp+' C/P'
ELSE
	@  4,22 SAY m.docref
	@  4,26 SAY m.numref
ENDIF
*m.Periodo = SUBSTR(m.CodCal,1,2)
@  4,60 SAY m.fecref
@  5,22 SAY val_para(m.Codfte,'CODFTE','V',26,50)
DO Vis_Calen
*@  5,60 SAY val_para(m.tipfun,'TIPFUN','D',22,15)
*@  6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
*IF m.incpres='S'
 *  @  7,22 SAY VAL_SUBP(substr(m.CodCal,10,3),'CODSUB'+substr(m.CodCal,8,2)+'    ','V',22,40)
 *  @  8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,3)
 *  IF alltrim(m.Tipfun)='I' 
*	  @  8,25 SAY '.'
*	  @  8,26 SAY VAL_SUPY(alltrim(substr(m.CodCal,16,2)),m.Periodo+substr(m.CodCal, 8,8),'V',22,40,5) 
 *  ELSE
 *     @  8,22 SAY Spac(60)	
 *  ENDIF	
*ENDIF	
@ 09,22 SAY m.destino PICTURE '@S56'
@ 10,22 SAY m.justif PICTURE '@S56'
DO vista_hijo
DO Total
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

FUNCTION valOTR2 &&Usado para displayar descripc. del prov. en Vista  
*--------------
parameter OTRO,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF OTRO<>'0000' .OR. !EMPTY(OTRO)
   sele auxi
   SET FILT TO TIPO$"09"
   seek otro
   v_fun = iif(found(),Descri,"")
   @3,30 SAY DESCRI
   m.Codemp=space(5)
   m.Codprv=space(5)
   m.CodPre=space(8)
  * m.codotr=otro
   vfun = .t.
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
RETURN vfun


FUNCTION valprv2 &&Usado para displayar descripc. del prov. en Vista  
*--------------
parameter prov,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF prov<>'0000' .OR. !EMPTY(prov)
   sele auxi
   SET FILT TO TIPO="20"
   seek prov
   v_fun = iif(found(),Descri,"")
   @3,30 SAY DESCRI
   m.Codemp=space(5)
   m.codotr=space(6)
   m.codpre=space(8)
   m.codprv=prov
   vfun = .t.
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
RETURN vfun

FUNCTION valpre3 &&Usado para displayar descripc. del prov. en Vista  
*--------------
parameter pre,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF pre<>'0000' .OR. !EMPTY(pre)
   sele auxi
   SET FILT TO TIPO="80"
   seek pre
   v_fun = iif(found(),Descri,"")
   @3,30 SAY DESCRI
   m.Codemp=space(5)
   m.codprv=space(6)
   m.codotr=space(6)
   m.codpre=pre
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
   sele auxi
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
*	codpart    :H= 'Part.',;

		BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numHA  TIMEOUT 0.001 ;
			WINDOW wind_2 ;
			FIELDS;
	numchq     :H= 'N§ Cheque' ,;
	fecchq     :H= 'Fecha' ,;
	nomgir     :H= 'Girado a' :p='@S20' ,;
	valchq     :H= 'Monto' :p='99,999,999.99'
ELSE
	DO standby WITH 'No tiene detalle'
ENDIF
SELE HojAnu
SET RELATION OFF INTO COMPAG
RETURN

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW wind_3
@ 0,1 SAY m.import PICTURE '9,999,999.99'
RETURN

PROCEDURE revis              && Revisa H/A en browse
*--------------
SELE HojAnu
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
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxi', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxi', 'descri', 'descri', codemp), 20))) :H="Nombre",;
	codfte :H='Fte.Fin.',;
	IMPORT :H='Total H/A',;
	x3=NumCP+'.'+NumMesCP :H=' N§ C/P',;
	Estado :H='Estado'
*codcal :H='Calendario',;
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
	GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE HojAnu
DO vista
RETURN

PROCEDURE busca              && Realiza b£squeda directa
*--------------
sele HojAnu
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
SELECT HojAnu
SCATTER MEMVAR
ACTIVATE WINDOW wind_1
DO pantalla
DO CASE
	CASE m.TipDoc ='HC' OR M.TIPDOC='RG' OR m.tipdoc='SH'
*		@ 2,22 SAY Val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numha  WHEN .F.
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) 		&&&AND DISABLE
		@ 1,60 GET m.fechan 
		*DISABLE
		IF m.tipdoc="HC"
		    =pres_pro()
		ELSE
		    @ 3,30 GET m.nompre
		ENDIF  

*		READ
*		 valfecha()
	    @ 4,22 SAY m.numcp+'.'+m.nummesCP + ' C/P'
		@ 4,60 GET m.fecref
		@ 5,22 SAY val_para(m.CodFte,'CODFTE','V',26,50)
		*@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
	    *@ 7,22 SAY VAL_SUBP(substr(m.CodCal,10,3),'CODSUB'+substr(m.CodCal,8,2)+'    ','V',22,40)
		*@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
  		@ 09,22 say m.destino  PICTURE '@S56' 
*READ
	
	CASE m.TipDoc ='HM'
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 GET m.numha  WHEN .F.
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND DISABLE
		@ 1,60 GET m.fechan DISABLE
   	    @ 3,30 SAY m.nompre function "!"
	    @ 4,22 SAY m.numcp+'.'+m.nummescp + ' C/P'
		@ 4,60 SAY m.fecref
		@ 5,22 SAY val_para(m.CodFte,'CODFTE','V',26,50)
		*@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
	    *@ 7,22 SAY VAL_SUBP(substr(m.CodCal,10,3),'CODSUB'+substr(m.CodCal,8,2)+'    ','V',22,40)
		*@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
	    @  09,22 say m.destino  PICTURE '@S56' 
		READ 
		
	CASE m.TipDoc='ME' OR  m.tipdoc='SU'
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 GET m.numha when .f.
	    @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) 
	    @ 1,60 GET m.fechan    VALID(MONTH(m.fechan)=VAL(m.nummes)) AND ingfec()
	    	DO CASE
		    CASE compag.TIPPRV="O"
		       @ 3,22 say m.codOTR
		       @ 3,22 SAY valotr(m.coDOTR)
		    *    @ 3,22 SAY compag.nomPre
		    CASE compag.tipprv="E"
			    @ 3,22 say m.codemp
		        @ 3,22 SAY valemp2(m.codemp)
		    CASE compag.tipprv="P"
		       @ 3,22 say m.codprv
	   		   @ 3,22 SAY valprv2(m.codprv)
	   	    CASE compag.tipprv="I"
		       @ 3,22 say m.codpre
	   		   @ 3,22 SAY valpre(m.codpre)
      
         ENDCASE

*	    @ 3,22 SAY m.nompre FUNCTION "!"   
		@ 4,22 SAY m.docref    
	    @ 4,26 SAY m.numref    
	    @ 4,60 SAY m.fecref
 	    @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	    *@ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
		*IF m.incpres='S'
		*	@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
		 *   @ 7,22 SAY VAL_SUBP(substr(m.CodCal,10,3),'CODSUB'+substr(m.CodCal,8,2)+'    ','V',22,40)
		*	@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4) 
*	    ENDIF
		READ VALID val_read()
		
	CASE m.TipDoc='RE' .OR. M.TIPDOC='SR'
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
	    @ 1,22 GET m.numha when .f.
	    @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9)
	    @ 1,60 GET m.fechan    VALID(MONTH(m.fechan)=VAL(m.nummes)) AND INGFEC()
        @ 03,02 SAY "Fuente de Retencion"
        @ 3,22 say VAL_PARA(m.codret,'CODRET','V',22,40)
	    @ 4,22 say m.docref
	    @ 4,26 say m.numref    
	    @ 4,60 SAY m.fecref
 	    @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	    *@ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	    *@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
	    *@ 7,22 SAY val_subp(SUBSTR(m.CodCal,10,3),'CODSUB'+SUBSTR(m.CodCal,8,2)+'    ','V',22,40)
        *@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		READ VALID val_read()
		
	CASE m.TipDoc='SE'
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 GET m.numHA WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND DISABLE
		@ 1,60 GET m.fecHAN VALID(MONTH(m.fechan)=VAL(m.nummes))  AND INGFEC()
	    @ 3,22 SAY m.nompre FUNCTION "!"   
	    @ 4,22 say m.docref
	    @ 4,26 say m.numref    
	    @ 4,60 SAY m.fecref
 	    @ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
	    *@ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
	    *@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
	    *@ 7,22 SAY val_subp(SUBSTR(m.CodCal,10,3),'CODSUB'+SUBSTR(m.CodCal,8,2)+'    ','V',22,40)
        *@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		READ VALID val_read()
ENDCASE
@  10,22 GET m.justif FUNCTION "!" PICTURE '@S56' WHEN !EMPTY(m.Numha)
READ

SELECT caja
SEEK m.codctc
w_TipCtc=Caja.Tipo
w_ctaH  =Caja.CuentaH
m.usuario=wuser_id
SELE HojAnu
IF LASTKEY() = 27
	DO vista
	RETURN
ELSE
    INGRESO=.F.
    DO trabaja_hijo
    IF sw#2
	    DO ingap
	    DO AsiOrd
	    IF m.tipdoc<>'ME' AND m.tipdoc<>'SU' 
		    DO compre
		ENDIF    
		SELECT HojAnu
		IF RLOCK()
			GATHER MEMVAR
		ENDIF	
	ENDIF
ENDIF	
UNLOCK ALL
ON KEY LABEL f4 DO imp_HA
DO vista                    && Muestra nuevos datos
RETURN


PROCEDURE ingre              && Crea nuevo registro en BD
*--------------
PUBLIC vctadeb,vctahab,vvaldeb,vvalhab
DEACTIVATE WINDOW WIND_3
SELECT HojAnu
DO pantalla
SCATTER MEMVAR BLANK
m.fecref = DATE()
m.justif = 'SE ANULA EL PRESENTE COMPROMISO POR'+SPACE(167)
vFlag=.T.

OI=carcp()
DO CASE
	CASE m.tipdoc="HC" OR m.tipdoc='RG' OR m.tipdoc='SH'
		IF LASTKEY()=27 OR !OI
			SELECT HojAnu
			DO vista
			RETURN
		ENDIF
		SELE HojAnu
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 GET m.numha
		
*		VALID VEXISTE()
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
		@ 1,60 GET m.fechan  VALID(MONTH(m.fechan)=VAL(m.nummes))
		IF m.tipdoc='HC'
			DO CASE
				CASE  m.tipprv="P"
					@ 3,22 SAY m.codprv
					@ 3,30 SAY valprV(m.codprv) 
				CASE  m.tipprv="E"
					@ 3,22 SAY m.codemp
					@ 3,30 SAY valemp(m.codemp)
				CASE  m.tipprv="O"
					@ 3,22 SAY m.codotr
					@ 3,30 SAY valotr(m.codotr)
				OTHER
					@ 3,22 SAY m.nompre  function "!"
			ENDCASE
		 ELSE
			@ 3,22 SAY m.nompre  function "!"
		ENDIF
		@ 4,22 SAY m.numCP+'.'+m.nummesCP + ' C/P'
		@ 4,60 SAY m.fecref
		@ 5,22 SAY val_para(m.Codfte,'CODFTE','V',26,50)
		IF newsistem='1'
			=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
		 ELSE
			=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
		ENDIF
		DO Vis_Calen
		*@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
		*@ 7,22 SAY val_subp(SUBSTR(m.CodCal,10,3),'CODSUB'+SUBSTR(m.CodCal,8,2)+'    ','V',22,40)
		*@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		@  09,22 say m.destino  PICTURE '@S56' 
		READ VALID val_READ()
	CASE m.tipdoc="HM"
		IF LASTKEY()=27 OR !OI
			SELECT HojAnu
			DO vista
			RETURN
		ENDIF
		SELE HojAnu
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 SAY m.numha
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
		@ 1,60 GET m.fechan  VALID(MONTH(m.fechan)=VAL(m.nummes))
		@ 3,22 SAY m.nompre  function "!"
		@ 4,22 SAY m.numCP+'.'+m.nummesCP + ' C/P'
		@ 4,60 SAY m.fecref
		*@ 5,22 SAY val_para(SUBSTR(m.CodCal, 5,3),'CODFTE','V',26,50)
		*@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
		*@ 7,22 SAY val_subp(SUBSTR(m.CodCal,10,3),'CODSUB'+SUBSTR(m.CodCal,8,2)+'    ','V',22,40)
		*@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		@  09,22 say m.destino  PICTURE '@S56' 
		READ VALID val_READ()

	CASE m.tipdoc="ME" OR m.tipdoc='SU'
		IF LASTKEY()=27
			SELECT HojAnu
			DO vista
			RETURN
		ENDIF
		SELECT HojAnu
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 SAY m.numha
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
		@ 1,60 GET m.fechan    VALID(MONTH(m.fechan)=VAL(m.nummes)) AND ingfec()
		DO CASE
			CASE compag.TIPPRV="O"
				@ 3,22 say m.codOTR
				@ 3,22 SAY valotr(m.coDOTR)
			CASE compag.tipprv="E"
				@ 3,22 say m.codemp
				@ 3,22 SAY valemp2(m.codemp)
			CASE compag.tipprv="P"
				@ 3,22 say m.codprv
				@ 3,22 SAY valprv2(m.codprv)
			CASE compag.tipprv="I"
				@ 3,22 say m.codpre
				@ 3,22 SAY valpre(m.codpre)
		ENDCASE
*		M.TIPPRV=compag.TIPPRV
*		@ 3,22 SAY m.nompre
		@ 4,22 SAY m.docref    
		@ 4,26 SAY m.numref
		@ 4,60 SAY m.fecref
		@ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
		*@ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
		IF m.incpres='S'
			*	@ 6,22 SAY VAL_PARA(SUBSTR(m.codcal,8,2),'CODPRG','V',22,40) 
			*	@ 7,22 SAY VAL_SUBP(SUBSTR(m.codcal,10,3),'CODSUB'+SUBSTR(m.codcal,8,2)+'   ','V',22,40)
			*	@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		ENDIF
		READ VALID val_Read() 
	CASE m.Tipdoc='RE' .OR. M.TIPDOC='SR'
		IF LASTKEY()=27
			SELECT HojAnu
			DO vista
			RETURN
		ENDIF
		SELECT HojAnu
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 SAY m.numha
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
		@ 1,60 GET m.fechan    VALID(MONTH(m.fechan)=VAL(m.nummes)) AND INGFEC()
		@ 3,02 SAY "Fuente de Retencion"
		@ 3,22 SAY Val_Para(m.codret,'CODRET','V',22,40)
		@ 4,22 SAY m.docref
		@ 4,26 SAY m.numref    
		@ 4,60 SAY m.fecref
		@ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
		*@ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
		*@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
		*@ 7,22 SAY val_subp(SUBSTR(m.CodCal,10,3),'CODSUB'+SUBSTR(m.CodCal,8,2)+'    ','V',22,40)
		*@ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		READ VALID val_READ()
	CASE m.tipdoc='SE'
		IF LASTKEY()=27
			SELECT HojAnu
			DO vista
			RETURN
		ENDIF
		SELECT HojAnu
		@ 2,22 SAY val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
		@ 1,22 SAY m.numHA
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
		@ 1,60 GET m.fecHAN    VALID(MONTH(m.fechan)=VAL(m.nummes)) AND ingfec()
		@ 3,22 SAY m.nompre
		m.docref='H/C'
		@ 4,22 say m.docref
		@ 4,26 say m.numref
		@ 4,60 SAY m.fecref
		@ 5,22 SAY val_para(m.codfte,'CODFTE','V',26,50)
		*	@ 5,58 say val_para(m.tipfun,'TIPFUN','V',58,15,2)
		*	@ 6,22 SAY val_para(SUBSTR(m.CodCal, 8,2),'CODPRG','V',26,40)
		*	@ 7,22 SAY val_subp(SUBSTR(m.CodCal,10,3),'CODSUB'+SUBSTR(m.CodCal,8,2)+'    ','V',22,40)
		*   @ 8,22 SAY VAL_PYAC(alltrim(substr(m.CodCal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,4)
		READ VALID VAL_READ() 
ENDCASE
@  10,22 GET m.justif FUNCTION "!" PICTURE '@S56' WHEN !EMPTY(m.Numha)
READ VALID val_read()
SELECT caja
SEEK m.codctc
w_TipCtc=Caja.tipo
w_ctaH  =Caja.CuentaH
m.usuario=wuser_id
USE IteCp IN 25 ORDER IteCp1 ALIAS itecp
SELE Itecp
SEEK ALLTRIM(m.NumMesCp)+m.NumCp + ALLTRIM(m.CodCtc)
w_part=IteCp.CodPart
SELE 25
USE 
SELE HojAnu
IF LASTKEY() # 27
	m.estado = '00'
	SELECT compag
	SEEK m.nummescp+m.numcp+m.codctc
	IF RLOCK()
		REPLACE pres WITH '*'  &&Marca C/P trabajado
	ENDIF
	UNLOCK 
	SELECT HojAnu
	ingreso = .T.
	DO trabaja_hijo
	IF sw#2
		DO ingap
		DO AsiOrd
		IF m.tipdoc<>'ME' AND m.tipdoc<>'SU'
			DO compre
		ENDIF
		SELECT HojAnu
		m.ctadeb = vctadeb
    	m.ctahab = vctahab
		m.valdeb = vvaldeb 
		m.valhab = vvalhab 	
		IF f_appd()
			GATHER MEMVAR
		ENDIF
		SELECT parma
		SEEK 'CORREL' + 'HOJANU'
		REPLACE nument WITH nument + 1
		UNLOCK 
	ENDIF
ENDIF	
ON KEY LABEL f4 DO imp_HA
DO Pantalla
DO vista
RETURN


PROCEDURE ingfec  &&Valida Periodo
*---------------
m.Periodo = RIGHT(DTOC(m.fechan),2)
RETURN .T.

PROCEDURE valfecha
*-----------------
m.fechan = DATE()
RETURN .T.

FUNCTION VALPRV  &&Usado para displayar descripc. del prov. en Corrige e Ingresa  
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
private  malias, v_fun, _oldwind,_campo
_campo = varread()
malias = alias()
select AUXI
_oldwind = woutput()
   seek "20"+xcod
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
select AUXI
_oldwind = woutput()
seek "30"+xcod
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

FUNCTION VALOTR &&Usado para displayar descripc. de otro. en Corrige e Ingresa  
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
private  malias, v_fun, _oldwind,_campo
_campo = varread()
malias = alias()
select AUXI
_oldwind = woutput()
seek "09"+xcod
IF FOUND()
      @3,30 SAY DESCRI
      m.codotr=xcod
      VFUN=.T.
ELSE
SET FILT TO TIPO='09'
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
       m.Codotr=XCOD
       V_FUN=.T.
    endif
ENDIF    
SET FILT TO
select (malias)
return 

FUNCTION VALPRE &&Usado para displayar descripc. de otro. en Corrige e Ingresa  
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
private  malias, v_fun, _oldwind,_campo
_campo = varread()
malias = alias()
select AUXI
_oldwind = woutput()
seek xcod
IF FOUND()
      @3,30 SAY DESCRI
      m.codPRE=xcod
      VFUN=.T.
ELSE
SET FILT TO TIPO='80'
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
       m.CodPRE=XCOD
       V_FUN=.T.
    endif
ENDIF    
SET FILT TO
select (malias)
return 

PROCEDURE TRABAJA_HIJO  &&trabaja en ITEHA, capturando DATA de CHEQUE
*---------------------
PRIVATE XTOT,xkey
PUBLIC xTot1
XTOT=0
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F5  DO Agr_Item
ON KEY LABEL F8  DO Eli_Item
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT iteha

IF !SEEK(ALLTRIM(m.nummes)+m.numha)
	DO Agr_Item
ENDIF

BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numha FIELD ;
	numchq     :H= 'N§ Cheque' :R,;
	fecchq     :H= 'Fecha' :R,;
	nomgir     :H= 'Girado a' :p='@S20' :R,;
	valchq     :H= 'Monto' :p='99,999,999.99'

SELE IteHA
xkey = ALLTRIM(m.nummes)+m.numha

xtot1 = 0

IF SEEK(xKey)
	SCAN WHILE NumMes+NumHA = xKey
		xTot1 = xTot1 + ValCHQ
	ENDSCAN
ENDIF


SELECT Cheque
SET ORDER TO Cheque4

SELE IteHA
SEEK xkey

SCAN WHILE NumMes+NumHA=xkey
	SELECT Cheque
	SEEK ALLTRIM(ITeha.numchq)
	IF fLock(1) AND FOUND()
		REPLACE estado   WITH "99",;
				fecanu   WITH m.fechan,;
				nummesha WITH m.nummes,;
				numha    WITH m.numha
		UNLOCK
	ENDIF
	SELE IteHA
ENDSCAN

SELE IteHA
SEEK xkey
SELE Cheque
SET ORDER TO Cheque1
SELECT HChqpen
SEEK  m.codctc+ALLTRIM(STR(VAL(left(ITeha.numchq,at("-",ITeha.numchq)-1))))
*SEEK  m.codctc+ALLTRIM(ITeha.numchq)
IF FOUND()
	IF RLOCK()
		REPLACE Hchqpen.estado WITH "99"
	ENDIF
	UNLOCK
ENDIF
SELE ITeha
m.import=xtot
SET FILTER TO
SELECT HojAnu
UNLOCK ALL
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT HojAnu
RETURN

*****

PROCEDURE Marca
*--------------
REPLACE AnuChq with 'û'
return


PROCEDURE DesMarca
*-----------------
REPLACE AnuChq with ' '
return

FUNCTION Agr_Item
*----------------
PRIVATE cFil

mF5 = ON('KEY', 'F4')
mF8 = ON('KEY', 'F4')

ON KEY LABEL F5
ON KEY LABEL F8
ON KEY LABEL F11 DO Marca
ON KEY LABEL F12 DO Desmarca

SELECT CHEQUE

cFil = FILTER()

SET FILTER TO ANUCHQ#'û'

IF SEEK(ALLT(m.NumMesCP)+m.NumCp+ALLT(m.CodCtc))
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLT(m.NumMesCP)+m.NumCp+ALLT(m.CodCtc) ;
	TITLE "[F11] Anula    [F12] Habilita   [F10] Salir " FIELD ;
		AnuChq     :H= '*' :R,;
		numCP      :H= 'C/P' :R,;
		numchq     :H= 'N§ Cheque' :R,;
		fecchq     :H= 'Fecha' :R,;
		nomgir     :H= 'Girado a' :p='@S20' :R,;
		valchq     :H= 'Monto' :p='99,999,999.99'
 ELSE
	DO standby WITH "No tiene registrado el Cheque para anular"
ENDIF

SET FILTER TO

SEEK ALLT(m.NumMesCP)+m.NumCp+ALLT(m.CodCtc)

Xtot = 0

SCAN WHILE NumMes+NumCP+CodCtc=ALLT(m.NumMesCP)+m.NumCp+ALLT(m.CodCtc)
	IF AnuChq = 'û' AND EMPTY(nummesha)
		SELE IteHA
		IF !SEEK(m.NumMes+NumHA+ALLTRIM(Cheque.NumChq))
			IF f_appd()
				REPLACE periodo WITH m.Periodo,;
						nummes  WITH ALLTRIM(m.nummes),;
						numha   WITH m.numha,;
						codcad  WITH m.CodCad,;
						codpart WITH CHEQUE.codpart,;
						numchq  WITH cheque.numchq,;
						valchq  WITH cheque.valchq,;
						codfte  WITH m.codfte,;
						tipdoc  WITH m.tipdoc,;
						nomgir  WITH cheque.nomgir,;
						Fecchq  WITH Cheque.fecchq,;
						anula   WITH 'S'
			ENDIF
			Xtot = xTot + Valchq
			UNLOCK
		 ELSE
			IF f_Lock(1)
				REPLACE periodo WITH m.Periodo,;
						nummes  WITH ALLTRIM(m.nummes),;
						numha   WITH m.numha,;
						codcad  WITH m.CodCad,;
						codpart WITH CHEQUE.codpart,;
						numchq  WITH cheque.numchq,;
						valchq  WITH cheque.valchq,;
						codfte  WITH m.codfte,;
						tipdoc  WITH m.tipdoc,;
						nomgir  WITH cheque.nomgir,;
						Fecchq  WITH Cheque.fecchq,;
						anula   WITH 'S'
			ENDIF
			Xtot = xTot + Valchq
			UNLOCK
		ENDIF
	ENDIF
	SELE Cheque
ENDSCAN 

*SET FILTER TO

SELE IteHA

ON KEY LABEL F5 &mF5
ON KEY LABEL F8 &mF8
ON KEY LABEL F11
ON KEY LABEL F12

RETURN

*****


*PROCEDURE TRABAJA_HIJO  &&trabaja en ITEHA, capturando DATA de CHEQUE
*---------------------
PRIVATE XTOT,xkey
XTOT=0
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT CHEQUE
IF SEEK(ALLT(m.NumMesCP)+m.NumCp+ALLT(m.CodCtc))
	IF ingreso
		SELECT iteha
		IF f_appd()
			REPLACE periodo WITH m.Periodo,;
				nummes  WITH ALLTRIM(m.nummes),;
				numha   WITH m.numha,;
				codcad  WITH m.CodCad,;
				codpart WITH CHEQUE.codpart,;
				numchq  WITH cheque.numchq,;
				valchq  WITH cheque.valchq,;
				codfte  WITH m.codfte,;
				tipdoc  WITH m.tipdoc,;
				nomgir  WITH cheque.nomgir,;
				Fecchq  WITH Cheque.fecchq,;
				anula   WITH 'S'
		ENDIF
		Xtot=Valchq
		UNLOCK
	 ELSE
		IF !found()
			SELE Iteha
			IF f_appd()
				REPLACE periodo WITH m.Periodo,;
						nummes  WITH ALLTRIM(m.nummes),;
						numha   WITH m.numha,;
						codcad  WITH m.CodCad,;
						codpart WITH CHEQUE.codpart,;
						numchq  WITH cheque.numchq,;
						valchq  WITH cheque.valchq,;
						codfte  WITH m.codfte,;
						tipdoc  WITH m.tipdoc,;
						nomgir  WITH cheque.nomgir,;
						Fecchq  WITH Cheque.fecchq,;
						anula   WITH 'S'
			ENDIF
			Xtot=Valchq
			UNLOCK
		 ELSE
			SELE iteha
			IF RLOCK()
				REPLACE periodo WITH m.Periodo,;
						nummes  WITH ALLTRIM(m.nummes),;
						numha   WITH m.numha,;
						codcad  WITH m.CodCad,;
						codpart WITH CHEQUE.codpart,;
						numchq  WITH cheque.numchq,;
						valchq  WITH cheque.valchq,;
						codfte  WITH m.codfte,;
						tipdoc  WITH m.tipdoc,;
						nomgir  WITH cheque.nomgir,;
						Fecchq  WITH Cheque.fecchq,;
						anula   WITH 'S'
	   		ENDIF
	   		xtot=valchq
   			UNLOCK
	   	ENDIF			
	ENDIF
 ELSE
	DO standby WITH "No tiene registrado el Cheque"
	sw=2
	RETURN
ENDIF    
SELECT iteha

BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numha FIELD ;
	numchq     :H= 'N§ Cheque' :R,;
	fecchq     :H= 'Fecha' :R,;
	nomgir     :H= 'Girado a' :p='@S20' :R,;
	valchq     :H= 'Monto' :p='99,999,999.99'

xkey=ALLTRIM(m.nummes)+m.numha	
SEEK xkey
SELECT Cheque
SET ORDER TO Cheque4
SEEK ALLTRIM(ITeha.numchq)
IF RLOCK() AND FOUND()
	REPLACE estado   WITH "99",;
			fecanu   WITH m.fechan,;
			nummesha WITH m.nummes,;
			numha    WITH m.numha
	UNLOCK
ENDIF
SET ORDER TO Cheque1
SELECT HChqpen
SEEK  m.codctc+ALLTRIM(ITeha.numchq)
IF FOUND()
	IF RLOCK()
		REPLACE Hchqpen.estado WITH "99"
	ENDIF
	UNLOCK
ENDIF
SELE ITeha
m.import=xtot
SET FILTER TO
SELECT HojAnu
UNLOCK ALL
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT HojAnu
RETURN


PROCEDURE ingap   && F5
*--------------
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numha+alltrim(m.codctc)
IF !FOUND()
      DO agriaut
ENDIF
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numha+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F
SEEK ALLTRIM(m.nummes)+m.numha+alltrim(m.codctc)
STORE 0 TO vdebe, vhaber ,vret
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numha=numref AND m.codctc=alltrim(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
		IF RET='S'
		   VRET=VRET+MTOHAB
        ENDIF		   
ENDSCAN
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
USE IN 14
ON KEY
m.Reten = vret
UNLOCK
RETURN

PROCEDURE agrite     &&
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH m.nummes,;
	        numref WITH m.numha,;
	        tipdoc WITH 'H/A',;
	        ret    WITH 'N',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.Fechan,;
	        Tipctc WITH w_TipCtc,;
	        periodo WITH m.Periodo
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
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
REPLACE codctc WITH m.codctc
RETURN .T.

PROCEDURE COMPRE  &&Asiento Presupuestal automatico inverso al C/P
*---------------
PRIVATE as,cOrd
AS1=ALIAS()
SELE AsiAut
SEEK "HAC"+"   "+"ASTPRE"

IF !FOUND()
	DO StandBy WITH "NO EXISTE PARAMETRO DE CTAS. PRESUPUESTARIAS NO INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	RETURN
 ELSE
	cCtaD  = ALLTRIM(DCuenta)
	cCtaH  = ALLTRIM(HCuenta)
ENDIF

SELE AstPre
cOrd = ORDER()
SET ORDER TO AstPre5

m.valdeb = xTot1
m.valhab = xTot1
*m.valdeb = m.import
*m.valhab = m.import

m.CtaDeb = PADR("8401"+IIF(m.CodFte="09","0201","0402")+RIGH(m.CodCad,2),15,"0")
m.CtaHab = PADR("8601"+IIF(m.CodFte="09","0201","0402")+RIGH(m.CodCad,2),15,"0")
*m.CtaDeb = PADR(cCtaD+IIF(m.Codfte='09','07','10')+m.CodCad,10,"0")
*m.CtaHab = PADR(cCtaH+IIF(m.Codfte='09','07','10')+m.CodCad,10,"0")

SELECT astpre
SEEK m.NumMes+m.NumHA+"H/A"

IF FOUND()
	FOR i = 1 TO 2
		IF f_Lock(1) OR RLOCK()
			REPLACE periodo WITH m.periodo,;
				nummes  WITH m.nummes,;
				numref  WITH m.numHA ,;
				tipdoc  WITH 'H/A',;
				fecref  WITH m.fechan,;
				codpart WITH m.codpart,;
				CodCtc  WITH ALLTRIM(m.CodCtc),;
				CodCad  WITH m.CodCad,;
				cuenta  WITH IIF(i=1,m.ctadeb,m.ctadeb),;
				tipo    WITH IIF(i=1,"D","H"),;
				ctadeb  WITH IIF(i=1,m.ctadeb,SPACE(10)),;
				ctahab  WITH IIF(i=2,m.ctaHab,SPACE(10)),;
				valdeb  WITH IIF(i=1,m.valdeb,0),;
				valhab  WITH IIF(i=2,m.valHab,0)
			UNLOCK
			SKIP
		ENDIF
	ENDFOR
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE periodo WITH m.periodo,;
				nummes  WITH m.nummes,;
				numref  WITH m.numHA ,;
				tipdoc  WITH 'H/A',;
				fecref  WITH m.fechan,;
				codpart WITH m.codpart,;
				CodCtc  WITH ALLTRIM(m.CodCtc),;
				CodCad  WITH m.CodCad,;
				cuenta  WITH IIF(i=1,m.ctadeb,m.ctaHab),;
				tipo    WITH IIF(i=1,"D","H"),;
				ctadeb  WITH IIF(i=1,m.ctadeb,SPACE(10)),;
				ctahab  WITH IIF(i=2,m.ctaHab,SPACE(10)),;
				valdeb  WITH IIF(i=1,m.valdeb,0),;
				valhab  WITH IIF(i=2,m.valHab,0)
			UNLOCK
		ENDIF
	ENDFOR
ENDIF
SET ORDER TO (cOrd)
ACTIVATE WINDOW wind_6
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY m.ctadeb PICTURE '!!!!!!!!!!!!'
@ 01,18  SAY m.valdeb PICTURE '999,999,999.99'
@ 02,12  SAY m.ctahab PICTURE '!!!!!!!!!!!!'
@ 02,34  SAY m.valhab PICTURE '999,999,999.99'
WAIT ' '
DEACTIVATE WINDOW wind_6
vCtadeb = m.ctadeb
vValdeb = m.Valdeb
vCtahab = m.ctahab
vValhab = m.valhab
SELE hojanu
RETURN


PROCEDURE anula
*---------------
SELECT HojAnu
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
	REPLACE usuario WITH wuser_id
	SELECT compag
	IF SEEK(m.NumMescp+m.Numcp)
	 IF RLOCK() .OR. F_LOCK(1)
	   REPLACE compag.pres with ' ' &&Libera C/P
	 ENDIF  
	UNLOCK
	ENDIF
    SELE astpat
    SEEK ALLTRIM(m.nummes)+m.numHA+alltrim(m.codctc)
    IF FOUND()
	    SCAN WHILE nummes=alltrim(m.nummes) and numref=m.numHA and codctc=m.codctc and tipdoc="H/A" 
    		IF RLOCK() .OR. F_LOCK(1)
		       DELETE NEXT 1
		    ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
    sele astpre
	set order to astpre19
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
	SELECT HojAnu
	DO vista
ENDIF
UNLOCK
RETURN

PROCEDURE lista
*--------------
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELECT HojAnu
VRECNO=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
Xind=SYS(3)+'.IDX'
SET RELATION TO alltrim(NUMmes) + numHA  INTO iteHA ADDI
SET SKIP TO
DEFINE WINDOW lis FROM 4,10 TO 17,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vestado,vlista
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli,VCODCAD
STORE SPACE(50) TO DESTIW
VCTA=M.CODCTC
VANO=M.NUMMES
VCLI=M.NUMHA
STORE DATE() TO vfecini, vfecfin
@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte'
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_ha()
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta  WHEN  vlista=2 VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,6,07)
@ 09,01 SAY " Fecha de Emisi¢n : "
@ 09,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7   WHEN vlista=2
@ 09,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7 WHEN vlista=2
@ 11,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
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
	   CASE vLista = 1
	       vkey=ALLTRIM(vano)+Vcli+ALLTRIM(vcta)
	       wKEY=ALLTRIM(Vano)+Vcli
	       SELE HojAnu
	       SET ORDER TO HojAnu2
	  	   SEEK Vkey
	       DEACTIVATE WINDOW standby
	  	   DO repprg WITH "LisHa"," Listado Hoja Anulaci¢n Cheque ",2
	  	   SET ORDER TO HojAnu1
	   CASE vLista = 2
	        INDEX ON numha TO (xind) FOR codctc=vcta AND BETWEEN(fechan,vfecini,vfecfin) AND estado='00'
		    DEACTIVATE WINDOW standby
            DO reporte WITH 2,"LisHaCta"," Resumen de Hojas de Anulacion Anuladas por Cta.Cte. "                                  	       
            SET INDEX TO
            ERASE (xind)
	ENDCASE
	SELE HojAnu
    SET RELATION OFF INTO ITEHA
ENDIF
USE IN 14
SELECT HojAnu
GO VRECNO
DO VISTA
RETURN

PROCEDURE val_ha  && Revisi¢n de Hojas de Anulaci¢n  en browse en Pantalla de Impresion
*---------------
SELE HojAnu
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
    x4=IIF((TIPPRV="O" .OR. EMPTY(TIPPRV)),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxi', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxi', 'descri', 'descri', codemp), 20))) :H="Nombre",;
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
SELE HojAnu
RETURN

PROCEDURE LISHA &&Programa Reporte usa repprg F.M.V.
*--------------
PARAMETERS XCOP
PRIVATE FILA,XVAL,TVAL,vPar,TOTALQ
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
STORE 0 TO XVAL,TVAL,FILA
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELE HojAnu
SET ORDER TO HojAnu2
vano=alltrim(nummes)
vcli=numHA
vcta=alltrim(codctc)
DESTIW=DESTINO
VCODCAD=CODCAD
M.VALDEB=VALDEB
M.VALHAB=VALHAB
Vkey= ALLTRIM(HojAnu.NUMMES)+HojAnu.NUMHA+ALLTRIM(HojAnu.CODCTC)
wkey= ALLTRIM(HojAnu.NUMMES)+HojAnu.NUMHA
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,68  say "PAG:"
@1,76  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisHa"
*@2,68 SAY "FECHA:"
*@2,76 SAY DATE()           
@4,14 SAY CHR(14)
@4,16 SAY "<< HOJA DE ANULACION DE CHEQUE >>"
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
	@09,20 SAY IIF(tipdoc$"RESRRURSAR",codret,IIF((TIPPRV='O' OR EMPTY(TIPPRV) OR TIPDOC='RG' OR tipdoc='HM')," ",IIF((TIPPRV='P'and tipdoc<>'RG' AND tipdoc<>'HM'),Codprv,IIF(TIPDOC<>'RG' AND tipdoc<>'HM',Codemp,' '))))
	@09,29 SAY IIF(tipdoc$"RESRRURSAR",VAL_PARA(Codret,'CODRET','D',28,40),;
	       IIF((tipprv='O' AND EMPTY(codotr) OR EMPTY(tipprv) OR tipdoc='RG' OR tipdoc='HM'),ComPag.nompre,;
	       IIF((tipprv='P' AND Tipdoc<>'RG' AND tipdoc<>'HM'),val_auxi(ALLT(codprv),'20','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='E', VAL_AUXI(allt(Codemp),'30','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='I', VAL_AUXI(allt(Codpre),'80','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='O', VAL_AUXI(allt(Codotr),'09','D',40),;
   	       IIF(tipprv='R' AND !EMPTY(codotr) AND tipdoc='ME',VAL_PARA(ALLTRIM(Codotr),'CODRET','D',28,40),' ')))))))

*@09,20 SAY IIF(TIPDOC="RE" OR TIPDOC="SR",codret,IIF(TIPPRV='O' AND TIPDOC='RG'," ",IIF(TIPPRV='P'and tipdoc<>'RG',CODPRV,IIF(TIPDOC<>'RG'and tipprv='O',CODotr,IIF(TIPDOC<>'RG' OR TIPPRV='I',CODPRE,' ')))))
*@09,29 SAY IIF(TIPDOC<>"RG" and TIPPRV="O",VALOTR3(CODOTR),IIF(TIPDOC="RE" OR TIPDOC="SR",VAL_PARA(Codret,'CODRET','D',28,40),IIF((TIPPRV='I' AND TIPDOC<>'RG'),VALPRE4(CODPRE),IIF((TIPPRV='P' AND TIPDOC<>'RG'),VALPRV3(CODPRV),IIF(TIPDOC<>'RG',VALEMP3(CODEMP),compag.NOMPRE)))))
*&&@09,27 SAY IIF(TIPDOC="ME" OR TIPPRV="O",VALOTR3(CODOTR),compag.NOMPRE)
*&&@09,27 SAY IIF(HojAnu.TIPDOC="RE" OR TIPDOC="SR",VAL_PARA(HojAnu.codret,'CODRET','V',22,40),IIF((HojAnu.TIPPRV='O' OR EMPTY(HojAnu.TIPPRV)),HojAnu.NOMPRE,IIF(HojAnu.TIPPRV='P',VALPRV3(HojAnu.CODPRV),VALEMP3(HojAnu.CODEMP))))
@10,03 SAY "  Referencia :"  
@10,20 SAY 'C/P'+' '+HojAnu.nummescp+'.'+HojAnu.numcp
@10,35 SAY IIF(!EMPTY(DOCREF),DTOC(HojAnu.fecref)+'     H/C '+HojAnu.nummeshc+'.'+HojAnu.numhc,' ')
@11,03 SAY "     Cta cte :"  
@11,20 SAY VCTA
@11,35 SAY val_fun('Caja','VCTA','Descri',VCTA)
@12,03 SAY "     Fte Fto :"
@12,20 SAY codfte
@12,25 SAY alltrim(val_para(CODFTE,'CODFTE','D',26,50))

*JFG
*IF newsistem='1'
*	=val_codcad(ALLTRIM(codcad),periodo+'01001','C')
*ELSE
*	=val_codca1(ALLTRIM(codcad),periodo,'C')
*ENDIF

*=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
=val_codcad(ALLTRIM(M.Codcad),M.periodo+'01001','C')
@14,04 SAY "Cadena Func.:   UG  UE  FN PRG SBPRG ACTPRY"
@15,20 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry

*IF INCPRES='S'
*@13,07 SAY "Programa :"
*@13,20 SAY substr(CodCal, 8,2)
*@13,25 SAY val_para(substr(CodCal, 8,2),'CODPRG','D',26,40)
*@14,04 SAY "Subprograma :"
*@14,20 SAY substr(codcal,10,3)
*@14,25 SAY Val_SUBP(substr(codcal,10,3),'CODSUB'+substr(codcal,8,2)+'    ','D',22,50)
*@15,04 SAY IIF(empty(substr(codcal,13,3)),' ',IIF(TIPFUN='F','  Actividad :','   Proyecto :'))
*@15,20 SAY IIF(empty(substr(codcal,13,3)),' ',substr(codcal,13,3))
*@15,25 SAY IIF(empty(substr(codcal,13,3)),' ',VAL_PYAC(alltrim(substr(codcal,13,3)),Periodo+substr(CodCal,8,5),'D',22,50))
*@16,04 SAY IIF(EMPTY(substr(codcal,16,2)),' ',IIF(TIPFUN='F','             ','SubProyecto :'))
*@16,20 SAY IIF(EMPTY(substr(codcal,16,2)),' ',substr(codcal,16,2))
*@16,25 SAY IIF(EMPTY(substr(codcal,16,2)),' ',VAL_SUPY(alltrim(substr(codcal,16,2)),Periodo+substr(codcAl,8,8),'D',22,50))
*ENDIF
@17,08 SAY IIF(EMPTY(DESTINO),' ','Destino :')
@17,19 SAY CHR(15)
@17,22 SAY DESTIW
@18,31 SAY CHR(18)
@19,40 SAY "J U S T I F I C A C I O N "
@20,01 SAY CHR(15)
@20,06 SAY SUBSTR(JUSTIF,1,127)
@21,06 SAY SUBSTR(JUSTIF,128,67)
@22,24 SAY CHR(18)
@23,25 SAY "<< CONTABILIDAD PRESUPUESTAL >>"
@24,10 SAY "Cuentas"
@24,52 SAY "Debe"
@24,72 SAY "Haber"
@25,03 SAY SUBSTR(CTADEB,1,3)
@25,10 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,25)',substr(CtaDeb,1,2))
@25,40 SAY M.VALDEB PICTURE '@Z 999,999,999,999.99'
@26,03 SAY SUBSTR(CTAHAB,1,3)
@26,10 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,25)',substr(CtaHAB,1,2))
@26,60 SAY M.VALHAB PICTURE '@Z 999,999,999,999.99'
@28,20 SAY "<<  D E T A L L E     D E    C H E Q U E S>>"
@29,03 SAY "N§ de Cheque"
@29,18 SAY "Fecha"
@29,30 SAY "Girado a"
@29,60 SAY "Monto"
SELE ITEHA
SEEK  wKEY
FILA = 30

SCAN WHILE NumMes+NumHa = m.NumMes+m.NumHa
	@FILA,04 SAY Iteha.numchq
	@FILA,18 SAY IIF(EMPTY(ITEHA.FECCHQ)," ",iteha.fecchq)
	@FILA,30 SAY LEFT(Iteha.nomgir,25)
	@FILA,60 SAY Iteha.valchq PICTURE '@Z 999,999,999,999.99'
	tval = tVal + ITEha.valchq
	FILA=FILA + 1
ENDSCAN
*xval=ITEha.valchq
*tval=ITEha.valchq
FILA=FILA+1
SELE HojAnu
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
@FILA,10 SAY REPLICATE("Ä",20)
@FILA,50 SAY REPLICATE("Ä",26)
FILA=FILA+1
*@FILA,10 SAY "   CONTROL INTERNO"
@FILA,52 SAY "TESORERO CAJERO GENERAL"

FILA=FILA+3
@FILA,50 SAY "LOGIN:"
@FILA,60 SAY disp_usu()
@fila+1,50 SAY "Fecha:"
@fila+1,60 SAY HojAnu.FecDig

IF _DEST1=2
	EJECT
ENDIF	
SELE HojAnu
SET ORDER TO HojAnu1
SET DEVICE TO SCREEN 
RETURN

FUNCTION valprv3  &&Usado para displayar descripc. del prov. en el REPO lisha1  
*--------------
parameter prov,MALIAS
malias = alias()
PRIVATE xx, vfun
SET ORDE TO 2
IF prov<>'0000' .OR. !EMPTY(prov)
   sele auxi
   SET FILT TO TIPO="20"
   seek prov
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
RETURN v_fun

FUNCTION valOTR3 &&Usado para displayar descripc. en el REPO lisha1  
*--------------
parameter OTRO,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF OTRO<>'0000' .OR. !EMPTY(OTRO)
   sele auxi
   SET FILT TO TIPO$"09"
   seek otro
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS)
SELE COMPAG
RETURN v_fun

FUNCTION valPRE4 &&Usado para displayar descripc. en el REPO lisha1  
*--------------
parameter PRE,MALIAS
malias = alias()
PRIVATE xx, vfun
vfun = .F.
IF PRE<>'00000000' .OR. !EMPTY(PRE)
   sele auxi
   SET FILT TO TIPO$"80"
   seek pre
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
   sele auxi
   SET FILT TO TIPO="30"
   seek emp
   v_fun = iif(found(),Descri,"")
ENDIF
SET FILT TO 
SELE (MALIAS) 
RETURN v_fun



PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE fin_opcion
*-------------------
CLOSE DATA
ON KEY
RELEASE    WINDOW wind_0
RELEASE    WINDOW wind_1
RELEASE    WINDOW wind_2
RELEASE    WINDOW wind_3
RELEASE    WINDOW wind_c1
RELEASE    MENU   mmenu
RESTORE SCREEN FROM principal
RETURN


PROCEDURE cor_as  && F9
*--------------
SELE Caja
ingreso=.F.
SEEK m.codctc
w_TipCtc=Caja.Tipo
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numha+alltrim(m.codctc)
IF !FOUND()
	DO agrite
ENDIF
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numha+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F
SEEK ALLTRIM(m.nummes)+m.numha+alltrim(m.codctc)
STORE 0 TO vdebe, vhaber ,vret
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numha=numref AND m.codctc=alltrim(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
		IF astpat.tipcta='D' AND LEFT(ALLT(asTpat.codcta),2)#'10'
		   ztotal=astpat.mtodeb
		ENDIF   
		IF RET='S'
		   VRET=VRET+MTOHAB
        ENDIF		   
ENDSCAN
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
USE IN 14
ON KEY
ON KEY LABEL f4 DO imp_ha
DEACTIVATE WINDOW wind_12
UNLOCK
SELE HojAnu
DO vista
RETURN

PROCEDURE imp_ha
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
     DO LISha WITH NUM_C
     SET PRINTER TO
     IF READY2PR() 
     	FOR I=1 TO NUM_C
*        	!TYPE &P_FIL > "\\ABASTECIMIENTO\FX1180-ABAS"
	       	!TYPE &P_FIL >PRN
        ENDFOR	
     ENDIF
     ERASE &p_fil
   ENDIF
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF
 SET DEVICE TO SCREEN
 SELE hojanu
RETURN

PROCEDURE agriaut    &&
*---------------

DO CASE
   CASE m.tipdoc='SE' 
	   private a1,a2,a3,a4,b1,b4
	   STORE SPACE(3) TO a1,a4,b1,b4
	   STORE SPACE(2) TO a2,a3
	   SELE CLASE
	   SEEK LEFT(ALLT(w_part),2)+RIGHT(ALLT(w_part),2)
	   w_ctad=Clase.CuentaD
	   a1=LEFT(w_ctad,3)
	   a4=RIGHT(w_ctad,3)
	   a2=LEFT(ALLT(SUBST(m.codcal,8,2)),2)
	   a3=SUBST(ALLT(SUBST(m.codcal,10,3)),2,2)
	   b1=LEFT(w_ctah,3)
	   b4=RIGHT(w_ctah,3)
	   IF a3#'06' AND a3#'10'
		   w_ctad=a1+a2+a3+a4
	       w_ctah=b1+a2+a3+b4
	   ENDIF	
   CASE m.tipdoc$'SUSS'   
 		w_ctad='4110101000'
   CASE m.tipdoc$'HCRGMEHMSH'      
	    x1=LEFT(ALLTRIM(w_part),1)
	    x2=RIGHT(ALLTRIM(w_part),2)
	    IF x1='6'
	    	w_ctad='4240000000'
	    ELSE
	    	SELE PARMA
	    	SEEK 'ESPGAS'+x2
	    	w_ctad=LEFT(descriaux,10)
	    	IF x2='SU'
	    		w_ctad='4190000000'
	    	ENDIF
	    ENDIF
   CASE m.tipdoc$'RERU'	     	    
		SELE ParMa
		SEEK 'CODRET'+ALLT(m.codret)
		w_ctaD=ALLT(Parma.descriaux)
ENDCASE		

SELECT astpat
IF f_appd()
   REPLACE nummes WITH m.nummes,;
           numref WITH m.numha,;
	       tipdoc WITH 'H/A',;
	       ret    WITH 'N',;
	       codctc WITH m.codctc,;
	       fecha  WITH m.Fechan,;
	       FecHC  WITH m.FecReF ,;
	       tipctc WITH w_tipctc,;
	       periodo WITH m.periodo,;
	       tipcta WITH 'D',;
	        Codcta WITH w_ctaH,;
	        mtodeb WITH m.import
ENDIF

IF f_appd()
   REPLACE nummes WITH m.nummes,;
           numref WITH m.numha,;
	       tipdoc WITH 'H/A',;
	       ret    WITH 'N',;
	       codctc WITH m.codctc,;
	       fecha  WITH m.Fechan,;
	       FecHC  WITH m.FecReF ,;
	       tipctc WITH w_tipctc,;
	       periodo WITH m.periodo,;
	       tipcta WITH 'H',;
	        Codcta WITH w_ctad,;
	        mtoHab WITH m.import
ENDIF

IF f_appd()
   REPLACE nummes WITH m.nummes,;
           numref WITH m.numha,;
	       tipdoc WITH 'H/A',;
	       ret    WITH 'N',;
	       codctc WITH m.codctc,;
	       fecha  WITH m.Fechan,;
	       FecHC  WITH m.FecReF ,;
	       tipctc WITH w_tipctc,;
	       periodo WITH m.periodo,;
	       tipcta WITH 'D',;
	       Codcta WITH '110101011900000',;
	        mtodeb WITH m.import
ENDIF

IF f_appd()
   REPLACE nummes WITH m.nummes,;
           numref WITH m.numha,;
	       tipdoc WITH 'H/A',;
	       ret    WITH 'N',;
	       codctc WITH m.codctc,;
	       fecha  WITH m.Fechan,;
	       FecHC  WITH m.FecReF ,;
	       tipctc WITH w_tipctc,;
	       periodo WITH m.periodo,;
	       tipcta WITH 'H',;
	       Codcta WITH '110101011900000',;
	        mtohab WITH m.import
ENDIF

RETURN

PROCEDURE VALE
*-------------
IF EMPTY(M.CODCTC) 
   DO STANDBY with 'No esta asignada la Cuenta Corriente'
*  DELE NEXT 1
   return .F.
ELSE
   return .t.
ENDIF

PROCEDURE carcp  &&Captura C/P para el ingreso de H/A
*--------------
vfun = .T.
OK = FOUND()
SELECT COMPAG
SET FILTER TO estado<>'99' AND pres<>"*" AND pres<>""
 &&Solo C/P con Cheques Girados Y * los que ya fueron registrados en H/A,  ya fueron entregados 
GO TOP
IF EOF()
	DO standby WITH 'No existe Comprobantes de Pago'
	SET FILT TO
	RETURN .F.
ENDIF
vMes ='00'
vcP  =SPACE(4)
VCC  = SPACE(17)
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ C/P: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 say '.'
@ 1,17 GET vCP DEFAULT SPACE(4)
@ 2, 4 SAY "Cta.Cte.:" GET vcc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcc,1,2,24)
READ
DEACTIVATE WINDOW standby
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
		x1=IIF((TIPPRV="O" .OR. TIPPRV=" "),NOMPRE,IIF(tipprv="P",LEFT(val_fun('Auxi', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxi', 'descri', 'descri', codemp), 20))) :H="Nombre",;
		x2=TRANSF(import,'99,999,999.99'):H="Monto",;
		fecCP :H="fecha"
ENDIF

IF LASTKEY()#27
	SELE compag
	m.nummeshc = nummeshc
	m.nummescp = nummes
	m.numcp    = numcp
	m.numhc    = numhc
	m.fecdig   = DATE()
	m.fecref   = feccp	
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
*	m.codpre   = codpre
	m.CodCad   = codcad
	m.codctc   = codctc
	m.codpart  = codpart
	m.tipdoc   = tipdoc
	m.nomPre   = nompre
	m.tipprv   = tipprv
	m.Periodo  = periodo
	m.codfte   = codfte
	m.numref   = numcp+"."+nummes
	m.tipdoc   = tipdoc
	m.docref   = "C/P"
	m.codret   = codret
	m.incpres  = incpres
	m.destino  = destino
	m.codobra  = codobra
	SHOW GETS
	*- El correlativo
	SELECT Parma
	SEEK 'CORREL'+'HOJANU'
	m.Numha = PADL(NumEnt+1,4,'0')
	SELECT ComPag
	SET FILT TO 
	SELECT HojAnu
ENDIF
RETURN .T.




PROCEDURE vis_calen   &&Vista Codificaci¢n Funcional Program tica
*-------------------
@ 06,22 SAY m.codcad
@ 06,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 07,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
RETURN


FUNCTION Disp_usu
*----------------
PRIVATE xuser_id,xdesu
vali=ALIAS()
vord=ORDER()
xuser_id=SPACE(08)
xdesu=SPACE(30)
xuser_ID = CHRTRAN(HojAnu.Usuario,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?','ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
USE USUARIO IN 0 ORDER USUARIO1 ALIAS USUARIO
SELE USUARIO
SEEK ALLT(xuser_id)
IF FOUND()
   xdesu  = ALLT(usuario.nombre)
ENDIF
USE 
SELE (Vali)
SET ORDER  TO (vord)
RETURN xdesu     

FUNCTION Vexiste
*---------------
SELE HojAnu
SET ORDER TO HojAnu4
SEEK numha
IF FOUND()
    DO STANDBY WITH "La Hoja de Anulaci¢n ya existe"
	RETURN .F.
ENDIF
RETURN .T.	

FUNCTION pres_pro        &&Displaya descripci¢n del proveedor
*----------------
DO CASE
   CASE m.tipprv="E"
        @ 3,22 SAY val_Auxi(ALLT(m.Codemp),'30','V')
   CASE m.tipprv="P"
        @ 3,22 SAY val_Auxi(ALLT(m.Codprv),'20','V')
   CASE m.tipprv="O" AND !EMPTY(m.codotr)
        @ 3,22 SAY val_Auxi(ALLT(m.Codotr),'09','V')
   CASE m.tipprv="I"
        @ 3,22 SAY val_Auxi(ALLT(m.Codpre),'80','V')
   CASE m.tipprv="R" AND !EMPTY(m.codotr)
   		@ 3,22 SAY VAL_PARA(m.codotr,'CODRET','V',22,40)
   OTHERWISE
        @ 3,22 SAY m.nompre
ENDCASE
RETURN


PROCEDURE AsiOrd
*---------------
*SELE Parma
*SEEK 'TIPDOCHAC'
*IF !FOUND()
*	DO StandBy WITH "PARAMETRO NO INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
*	RETURN
*ENDIF
SELE AsiAut
SEEK "HAC"+"   "+"ASTORD"

IF !FOUND()
	DO StandBy WITH "PARAMETRO DE CTAS. DE ORDEN INICIALIZADO, CONSULTE AL AREA DE SISTEMAS"
	USE IN AsiAut
	RETURN
 ELSE
	cCtaD  = DCuenta
	cCtaH  = HCuenta
ENDIF
SELE AstOrd
SEEK m.Periodo+m.NumMes+m.NumHA+"H/A"

m.valdeb = xTot1
m.valhab = xtot1

IF FOUND()
	FOR i = 1 TO 2
		IF f_Lock(1) OR RLOCK()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.NumMes  ,;
					NUMREF	WITH m.NumHA   ,;
					TIPDOC	WITH "H/A"     ,;
					FECHA	WITH m.FecHAn,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValDeb,0),;
					MTOHAB	WITH IIF(i=2,m.ValHab,0)
			UNLOCK
			SKIP
		ENDIF
	ENDFOR
 ELSE
	FOR i = 1 TO 2
		IF f_Appd()
			REPLACE Periodo WITH m.Periodo ,;
					NUMMES	WITH m.NumMes  ,;
					NUMREF	WITH m.NumHA   ,;
					TIPDOC	WITH "H/A"     ,;
					FECHA	WITH m.FecHAn,;
					CODCTA	WITH IIF(i=1,cCtaD,cCtaH),;
					TIPCTA	WITH IIF(i=1,"D","H"),;
					MTODEB	WITH IIF(i=1,m.ValDeb,0),;
					MTOHAB	WITH IIF(i=2,m.ValHab,0)
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
@ 01,18  SAY m.ValDeb PICTURE '99,999,999.99'
@ 02,12  SAY cCtaH PICTURE '!!!!!!!!!!!'
@ 02,34  SAY m.ValHab PICTURE '99,999,999.99'
WAIT " "
DEACTIVATE WINDOW wAstOrd
RELEASE WIND wAstOrd
RETURN
