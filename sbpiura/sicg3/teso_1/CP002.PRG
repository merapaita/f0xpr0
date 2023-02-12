PARAMETERS newsistem
*REGISTRO C/P ANTERIOR
* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegCp.Prg  19/11/96                              L: 3935     ³	
* ³ Registro de Comprobantes de Pago                             ³
* ³ AUTORES : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³                Julio Cruz Ortiz  (Pago de Retenciones)       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.     05/1997 ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

* ÕÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¸
* ³ Estados                           ³ 
* ³  00 C/P Emitido                   ³
* ³  Pres='*' Tiene Hoja de Anulaci¢n ³
* ³  99 C/P Anulado                   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

CLOSE DATA
*USE ParMae2   IN  1  ORDER TAG ParMae1      ALIAS ParMa 
USE ParMae   IN  1  ORDER TAG ParMae1      ALIAS ParMa 
USE ComPag   IN  3  ORDER TAG ComPag1      ALIAS ComPag 
USE IteCp    IN  4  ORDER TAG IteCp1       ALIAS IteCp  
USE HojCon   IN  6  ORDER TAG HojCon1      ALIAS Hoja   
USE IteHc    IN  7  ORDER TAG IteHc1       ALIAS IteHc 
USE Cajas    IN 10  ORDER TAG Cajas2       ALIAS Caja
USE Cheque   IN 11  ORDER TAG Cheque1      ALIAS Cheque
USE Auxil    IN 15  ORDER TAG Auxil1       ALIAS Auxil
USE MaePre   IN 16  ORDER tag MaePre1      ALIAS MaePre
USE ItePar   IN 25  ORDER tag ItePar1      ALIAS ItePar          
*USE Reten    IN 26  ORDER tag Reten1       ALIAS Reten          
*USE CTASEC IN 26 ORDER TAG CTASEC1 ALIAS CTASEC
SELECT ComPag
GO BOTTOM

*- Mensajes de aviso al usuario

vmens01 = ' Comprobantes de Pago : REVISION '
vmens02 = '°  ®F2¯Gira Cheque     ®F3¯Retenciones    ®F4¯Imprime C/P °'
vmens04 = 'Dicho C/P no fue encontrado'
vmens05 = 'No existe C/P anterior'
vmens06 = 'No existe C/P siguiente'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este C/P ha sido anulado'
vmens10 = 'No Existe Aut.de Giro'
vmens11 = 'Saldo es Menor'
vmens13 = ' °  Retenciones  ° '

SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Variables de trabajo (registro a trabajar)

PUBLIC vFecha,vKey,mmonto,vpar,vComPag,zz,yy,lseek,x_partret,Vrec,Wmtoret,wchq,ztotal,vref,vref1,VUSER_ID,wuser_id,Vglosa2,vsector

PUBLIC vNumMes,vCodCtc,vCta,vdes,w_tipctc,vret,w_ctad,w_part,w_ctah,vtitulo,vncheque,vimport,vvpartret,vcadena,cccomp,ccmet,FTEW,WDES,VPART
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID = CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE SPACE(14)  TO vcodctc,vncheque
STORE SPACE(18)  TO wchq
STORE SPACE(5)   TO w_part,yy
STORE SPACE(11)  TO w_ctad,w_ctah
STORE 0          TO mmonto,vmondeb,vret,VMONTO
STORE SPACE(20)  TO zz,Vref,vref1
STORE .T.        TO vflag
STORE SPACE(1)   TO m.prestamo
STORE DATE()     TO vfecha
STORE '  '       TO vnummes,vKey
STORE SPACE(180)  TO vglosa2

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
	TITLE '®F9¯: Corrige Asientos    Comprobante de Pago : Detalle   ' COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_10 FROM 20,64 TO 22,78 ;
	TITLE ' TOTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_4 FROM 08,01 TO 15,78 DOUBLE ;
	TITLE '** GIRO DE Cheque->  ®F5¯ Agregar   ®F8¯ Eliminar   ®F10¯ Terminar **' ;
	COLOR SCHEME 10

DEFINE WINDOW WIND_5 FROM 08,02 TO 15,77 DOUBLE ;
	TITLE '** RETENCIONES-> ®F5¯ Agregar   ®F8¯ Eliminar   ®F10¯ Terminar **' ;
	COLOR SCHEME 10

DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
	TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME  15

DEFINE WINDOW wind_7 FROM 13,10 TO 17,70 ;
	TITLE ' CENTRALIZACION DE Caja ' COLOR SCHEME 10

DEFINE WINDOW Wind_8 FROM 14,01 TO 16,79 ;
 TITLE ' Destino ' 

DEFINE WINDOW wind_9 FROM 05,00 TO 23,79 DOUBLE ;
	TITLE vmens13 COLOR SCHEME 2

DEFINE WINDOW Wind_11 FROM 16,01 TO 18,79 ;
	 TITLE ' Pr‚stamos ' COLOR SCHEME 5
	 
DEFINE WINDOW Wind_12 FROM 14,11 TO 16,69 ;
TITLE ' Retenci¢n ' 

DEFINE WINDOW Wind_13 FROM 13,10 TO 17,70  ;
    TITLE 'Sectores'  COLOR SCHEME 10

DEFINE WINDOW Wind_14 FROM 13,10 TO 17,70  ;
    TITLE 'Actividades'  COLOR SCHEME 10


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
ON KEY LABEL F2 DO gircheq
ON KEY LABEL F3 DO regret
ON KEY LABEL F4 DO imp_cp
ON KEY LABEL F9 DO cor_as
RETURN


PROCEDURE pantalla           && Pinta m scara de datos
*-----------------
ACTIVATE WINDOW wind_1
CLEAR
@ 00,02 SAY "         Pr‚stamo :" 
@ 00,40 SAY "Incidencia Presup.:" 
@ 01,02 SAY "           N§ C/P :"
@ 01,40 SAY "       Fecha  C/P :"
@ 02,02 SAY " Cuenta Corriente :"
@ 03,02 SAY "        Proveedor :"
@ 04,02 SAY "  Doc. Referencia :"
@ 04,48 SAY " Fecha Digitaci¢n :"
@ 05,02 SAY "         Fte.Fto. :"
@ 06,02 SAY " Cadena Funcional :"
@ 06,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 08,02 SAY "            Glosa :"
@ 09,02 SAY "    Observaciones :"
@ 11,00 SAY PADC(ALLTRIM(vmens02),77,' ') COLOR W+/B
RETURN


PROCEDURE vista              && Coloca valores de BD en variables y pinta datos
*--------------
SELECT ComPag
SET RELATION TO NumMesHC+NumHC 		INTO Hoja
SET RELATION TO NumMes+NumCP+Codctc INTO Cheque  ADDI
IF EOF()
	DO pantalla
	RETURN
ENDIF
ACTIVATE WINDOW wind_1
SCATTER MEMVAR
IF newsistem='1'
	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
ELSE
	=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
ENDIF
ON KEY LABEL F9 DO cor_as
@  0,0  SAY SPACE(80)
@  0,45 SAY 'Estado     :'
@  0,60 SAY IIF(m.pres='*','  Con H/A  ',IIF(M.tipdoc='IN','Inutilizado',IIF(Estado='99','  Anulado  ','     OK û   '))) COLOR SCHEME 2
@  1,22 SAY m.numcp
@  1,26 SAY '.'
@  1,27 SAY m.nummes
@  1,60 SAY m.feccp
@  2,22 SAY m.codctc
@  2,60 SAY SPACE(18)
@  2,60 SAY Verestch(Cheque.estado) COLOR SCHEME 2
SET RELA TO

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

DO CASE

   CASE m.tipdoc$'HCRG'
		@  4,22 SAY m.numhc+'.'+m.nummeshc+' H/C'
		
   CASE m.tipdoc='HM'
		@  4,22 SAY m.numhm+'.'+m.nummeshm+' H/M'
		
   OTHER	
		@  4,22 SAY m.docref
		@  4,26 SAY m.numref
		@  4,34 SAY m.refer
		
ENDCASE

IF  m.tipdoc#'IN'
    @  4,68 SAY m.fecref 
	@  5,22 SAY val_para(m.codfte,'CODFTE','V',26,50) 
 	@  6,22 SAY m.codcad

	   DO vis_calen

	@ 08,22 SAY m.glosa PICTURE '@S56' 
	@ 09,22 SAY SUBSTR(m.observ,1,130)
	IF m.Reten>0
	   @10,02 SAY '        Retenci¢n=>' COLOR SCHEME 05
	   @10,22 SAY m.Reten
	ELSE
	   @10,00 SAY SPACE(79)
	ENDIF      
	DO vista_hijo
	DO Total
ENDIF	
RETURN


PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT IteCp
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
IF FOUND()
  IF m.codcad<>'0000'
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT  KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)  TIMEOUT 0.001 ; 
	WINDOW wind_2 ;
	FIELDS ;
        CodCom     :H= 'Componente',;
        CodMet     :H= 'Meta',;
		CodPart    :H= 'Partida' ,;
		aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':38,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' 
  ELSE
	BROWSE NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT  KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)  TIMEOUT 0.001 ; 
	WINDOW wind_2 ;
	FIELDS ;
		CodPart    :H= 'Partida'  ,;
		aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':38,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' 
  ENDIF
ELSE
	DO standby WITH 'No tiene detalle'
ENDIF
SELE ComPag
SET RELATION OFF INTO Hoja
RETURN


PROCEDURE Total
*--------------
ACTIVATE WINDOW wind_3
@ 0,1 SAY m.import PICTURE '9,999,999.99'
RETURN


PROCEDURE revis              && Revisi¢n de BD en browse
*--------------
SELE ComPag
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
	X4=muespro(compag.tipprv,compag.codotr,compag.tipdoc) :H="Proveedor" ,;
	codfte :H='Fte.Fin.',;
	import :H='Total CP',;
	x3=NumHC+'.'+NumMesHC :H=' N§ H/C',;
	x2=NumHM+'.'+NumMesHM :H=' N§ H/M',;
	codcad :H='Cad. Func.',;
	Estado :H='Estado'
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
IF LASTKEY()=27
   GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL f10
SELE ComPag
SET RELATION OFF INTO Cheque
DO vista
RETURN


PROCEDURE busca              
*--------------
vtemp=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
STORE SPACE(4) TO VComPag
ACTIVATE WINDOW standby
@ 0,01 SAY '          Ingrese Mes: ' GET vnummes PICTURE '!!' DEFAULT SPACE(2)
@ 1,01 SAY 'Ingrese N£mero ComPag: ' GET vComPag PICTURE '@!' DEFAULT SPACE(4)
@ 2,01 SAY '       Ingrese CtaCte: ' GET vcodctc FUNCTION '!' VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",vcodctc,1,2,24)
READ
DEACTIVATE WINDOW standby
IF EMPTY(vComPag) .OR. LASTKEY()=27
	RETURN
ELSE
	vnummes = PADL(ALLTRIM(vnummes),2,'0')
	vComPag = PADL(ALLTRIM(vComPag),4,'0')
	SEEK vnummes+vComPag+ALLTRIM(vcodctc)
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
	DO standby WITH vmens09
	RETURN
ENDIF
vFlag=.F.
SELECT ComPag
ACTIVATE WINDOW wind_1
DO pantalla

SCATTER MEMVAR

DO CASE

	CASE m.TipDoc $'HCRG'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp WHEN .F.
		IF m.tipdoc="HC"
		    =pres_pro()
		ELSE
		    @ 3,30 GET m.nompre
		ENDIF  
		@ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
	    @ 4,35 GET m.refer 	  
	    @ 4,68 GET m.fecref
 	    @ 5,22 SAY Val_Para(m.codfte ,'CODFTE','D',22,47)
 	    
 	    IF newsistem='1'
			=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
		ELSE		
			=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
		ENDIF
 	    DO vis_calen
    	IF !EMPTY(m.destino)	
		    ACTIVATE WINDOW wind_8
		    @ 0,0  SAY 'Destino: ' 
		    @ 0,20 SAY m.destino picture '@S73' FUNCTION "!"
		ENDIF    
		READ 
	    DEACTIVATE WINDOW wind_8
        IF m.tipdoc='RG'
	        ACTIVATE WINDOW wind_11
			@ 0, 2 SAY " N§ C/P Pr‚stamo: " GET m.mesaaff DEFAULT PADL(MONTH(DATE()),2,'0')
			@ 0,23 SAY '.'
			@ 0,24 GET m.numcpAf DEFAULT SPACE(4)
			@ 0,32 SAY "Cta.Cte:" GET m.codCtcAf DEFAULT SPACE(14)
    	    READ VALID val_READ()
    	    vpoint=RECNO()
    	    SEEK ALLT(m.mesaaff)+numcpaf+ALLT(m.codctcaf)
    	    x_reten=0
    	    IF FOUND() AND RLOCK()
    	       x_reten  =ComPag.reten
    	       REPLACE conaaff WITH '#'
    	    ENDIF
		    IF X_reten>0
			   ACTIVATE WINDOW wind_12
			   @0,1 SAY  "Partida" GET m.partret PICTURE '!!.!!' VALID VAL_PRET(SUBSTR(M.partret,4,2),LEFT(M.partret,2),'C',22) 
			   @0,30 SAY "Reten. " +STR(x_reten,10,2) 
			   READ
      		   SELE ComPag
    	       REPLACE partret WITH m.partret
  		   	   USE reten  IN 12  ORDER TAG reten1  ALIAS reten
			   SELECT RETEN
		       SEEK ALLTRIM(m.mesaaff)+m.numcpaf+ALLTRIM(m.codctcaf)
	  	       IF FOUND() AND RLOCK()
			      SCAN WHILE  ALLTRIM(m.mesaaff)+m.numcpaf+ALLTRIM(m.codctcaf) = ALLT(RETEN.nummes)+RETEN.Numcp+ALLT(Reten.Codctc)
			          REPLACE partret WITH m.partret, nummeshc WITH m.nummeshc, numhc WITH m.numhc
			     ENDSCAN            
		      ENDIF   
		      USE IN 12
			  DEACTIVATE WINDOW wind_12
		    ENDIF	  
			SELE ComPag
    	    GO vpoint
  	        DEACTIVATE WINDOW wind_11
        ENDIF
        
	CASE m.TipDoc='ME'  
	
	    @ 00,22 GET m.Prestamo VALID(m.Prestamo$'SN')
	    @ 00,61 GET m.IncPres  VALID(m.IncPres$'S,N')
		@ 02,22 GET m.codctc   PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
	    READ
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp  VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	    @ 3,22 GET m.TipPrv    VALID(m.tipprv$'PEOIR')
	    @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
	    @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
	    @ 3,24 GET m.Codpre    PICTURE "!!!!!!!!"  VALID val_AuxiD(m.Codpre,'80',"A",30) WHEN m.TipPrv='I'
	    @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
	    @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID VAL_ParaD(m.Codotr,'CODRET',' ',24,40) WHEN m.TipPrv='R'
		@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.refer
		@ 4,68 GET m.fecref
	  	@ 5,22 GET m.codfte    PICTURE '!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  	IF m.IncPres='S'
	  		IF newsistem='1'
     		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
     		ELSE
     		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 	  
			ENDIF
      	ELSE 
      	  m.codcad='0000'
      	ENDIF
      	
	  	READ VALID VAL_READ() 
      	IF !EMPTY(m.CodCad) 
      		IF newsistem='1'
      			=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      		ELSE
      			=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
      		ENDIF
      		DO vis_calen
      	ENDIF
	CASE m.TipDoc='RE'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
		@ 03,02 SAY "Rubro de Retencion "
		@ 3,22 GET m.codret VALID VAL_ParaD(m.codret,'CODRET',' ',22,40)  AND Pagret('2')
		@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.refer
		@ 4,68 GET m.fecref

	  	@ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
  		IF newsistem='1'
     		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
   		ELSE
    		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 	  
		ENDIF

*	  	@ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	 
	  	READ VALID val_READ() 
      	IF !EMPTY(m.CodCad) 
      		DO vis_calen
      	ENDIF

	CASE m.TipDoc='RU' &&Retenciones de Subsidios
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	    @ 03,02 SAY "Rubro de Retencion "
		@ 3,22 GET m.codret VALID VAL_ParaD(m.codret,'CODRET',' ',22,40)  AND Pagret('2')
		@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.refer
		@ 4,68 GET m.fecref
		@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
		@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
		DO map_calen
		READ VALID val_read()
	    m.CodCal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',ALLTRIM(vProyec)+ALLTRIM(vSubPry),ALLTRIM(vCodAct))

	CASE m.TipDoc='SR' or M.TIPDOC='AR'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	    @ 3,02 SAY "Rubro de Retenci¢n "
		@ 3,22 GET m.codret VALID VAL_ParaD(m.codret,'CODRET',' ',22,40)  AND Pagret('2')
		@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.refer
		@ 4,68 GET m.fecref
		@ 5,22 GET m.codfte     PICTURE '!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
        @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	 
	    READ VALID val_READ() 
        IF !EMPTY(m.CodCad) 
      	  DO vis_calen
        ENDIF

	CASE m.TipDoc ='HM' 

		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp WHEN .F.
	    @ 3,30 GET m.nompre   
		@ 4,22 SAY m.numhm+'.'+m.nummeshm + ' H/M'
	    @ 4,35 GET m.refer 	  
	    @ 4,68 GET m.fecref
	    DO vis_calen
    	IF !EMPTY(m.destino)	
	    	ACTIVATE WINDOW wind_8
		    @ 0,0  SAY 'Destino: ' 
		    @ 0,20 SAY m.destino picture '@S73' FUNCTION "!"
		ENDIF    
		READ
	    DEACTIVATE WINDOW wind_8

	CASE m.TipDoc='SE'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
		@ 3,22 GET m.nompre FUNCTION "!"
		@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) 
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.Refer
		@ 4,68 GET m.fecref
		@ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
		@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
		DO map_calen
	    Dese=.F.
		READ VALID val_read()
	    m.CodCal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',ALLTRIM(vProyec)+ALLTRIM(vSubPry),ALLTRIM(vCodAct))

	CASE m.TipDoc ='SH' or m.TipDoc='AH'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp WHEN .F.
	    =pres_pro()
		@ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
	    @ 4,35 GET m.refer 	  

      	@ 5,22 SAY Val_Para(m.codfte ,'CODFTE','D',22,47)
      	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      	DO vis_calen
  	  	IF !EMPTY(m.destino)
	     	ACTIVATE WINDOW wind_8
      	 	@ 0,0  SAY 'Destino: '
	     	@ 0,20 SAY m.destino PICTURE '@S73' FUNCTION "!"
	  	ENDIF    
      	READ VALID val_Read()
      	DEACTIVATE WINDOW wind_8


	CASE m.TipDoc='SU'

	    @ 00,61 get m.IncPres  VALID(m.IncPres$'S,N')
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
	    READ
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp  VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	    @ 3,22 GET m.TipPrv    VALID(m.tipprv$'PEO')
	    @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
	    @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
	    @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.Refer
		@ 4,68 GET m.fecref
		@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)

	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  IF m.IncPres='S'
     	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
      ELSE 
      	  m.codcad='0000'
      ENDIF
	  READ VALID VAL_READ() 
      IF !EMPTY(m.CodCad) 
      	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      	DO vis_calen
      ENDIF


		@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
*		IF m.IncPres='S'
*		   DO map_calen
*	    ENDIF
		READ VALID val_read()
		IF m.IncPres='S'
		    m.CodCal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',ALLTRIM(vProyec)+ALLTRIM(vSubPry),ALLTRIM(vCodAct))
	    ENDIF

	CASE m.TipDoc='SS'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	    @ 3,22 GET m.TipPrv    VALID(m.tipprv$'PEO')
	    @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
	    @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
	    @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.Refer
		@ 4,68 GET m.fecref
		@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
		@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
*	    DO map_calen
		READ VALID val_read()
*    m.CodCal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',ALLTRIM(vProyec)+ALLTRIM(vSubPry),ALLTRIM(vCodAct))

	CASE m.TipDoc='RS'
	
		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	    @ 3,02 SAY "Rubro de Retencion "
		@ 3,22 GET m.codret VALID VAL_ParaD(m.codret,'CODRET',' ',22,40)  AND Pagret('2')
		@ 4,22 GET m.docref VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
		@ 4,26 GET m.numref PICTURE '!!!!.!!'
		@ 4,34 GET m.refer
		@ 4,68 GET m.fecref
		@ 5,22 GET m.codfte     PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
		@ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
*	DO map_calen
		READ VALID val_read()
*    m.CodCal = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',ALLTRIM(vProyec)+ALLTRIM(vSubPry),ALLTRIM(vCodAct))
	    
	CASE m.TipDoc='IN'

		@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) WHEN .F.
		@ 1,22 GET m.numcp WHEN .F.
		@ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() WHEN .F.
		@ 1,60 GET m.feccp  VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
		@ 3,22 GET m.nompre FUNCTION "!"
		m.estado='92'
		READ VALID val_read()
	
ENDCASE
    DO CASE
       CASE m.tipprv='P'
            STORE SPACE(5) TO m.codemp
            STORE SPACE(6) TO m.codotr
            store space(8) to m.codpre
       CASE m.tipprv='E'
            STORE SPACE(4) TO m.codprv
            STORE SPACE(6) TO m.codotr
            store space(8) to m.codpre
       CASE m.tipprv='O'
            STORE SPACE(4) TO m.codprv
            STORE SPACE(5) TO m.codemp
            store space(8) to m.codpre
       CASE m.tipprv='I'
            STORE SPACE(4) to m.codprv
            STORE SPACE(5) TO m.codemp
            store space(6) to m.codotr
    ENDCASE 

m.usuario = wUser_ID

IF m.tipdoc#'IN'
	@  9,22 GET m.glosa  PICTURE '@S56' FUNCTION "!"
	@ 10,22 GET m.observ PICTURE '@S56' FUNCTION "!"
	READ
ENDIF
SELE Caja
SEEK m.codctc
w_tipctc=Caja.Tipo
SELE ComPag
IF LASTKEY() = 27
	DO vista
	RETURN
ELSE
    IF m.tipdoc#'IN'
	   ingreso=.F.
	   DO CASE
       	  CASE m.tipdoc='HC' or m.tipdoc='AH'
		       DO trabaja_hijo
	      CASE m.tipdoc='HM'
	           DO tra_hijo
	      OTHER
		       DO traba_hijo
	   ENDCASE
       Vtotal=0
	   DO Total_padre
	   m.import = vtotal
       DO ingap
       SELE ComPag
	   IF !m.tipdoc$'SUMERUSSRS'
     	  DO Compre
	   ENDIF	
	ELSE
	   m.import=0
	ENDIF   

	DO CASE
		CASE m.tipdoc='HC'
			SELECT hoja
			SEEK ALLTRIM(m.nummeshc)+m.numhc
			IF RLOCK()  AND FOUND()
				REPLACE nummescp WITH m.nummes,;
				        numcp    WITH m.numcp,;
				        estado   WITH '50'	
				        
				        *reten    WITH m.reten,;
				        *partret  WITH m.partret
			ENDIF
		CASE m.tipdoc='RG'
			SELECT hoja
			SEEK ALLTRIM(m.nummeshc)+m.numhc
			IF RLOCK()  AND FOUND()
				REPLACE nummescp WITH m.nummes,;
				        numcp    WITH m.numcp,;
				        estado   WITH '50',;
				        reten    WITH x_reten,;
				        partret WITH m.partret
			ENDIF
		CASE m.tipdoc='HM'
			USE hojmod   IN 20  ORDER TAG hojmod1      ALIAS hojm   &&H/M
			SELECT hojm
			SEEK ALLTRIM(m.nummeshc)+m.numhc
			IF RLOCK()  AND FOUND()
				REPLACE nummescp WITH m.nummes,;
				        numcp    WITH m.numcp,;
				        estado   WITH '50'
			ENDIF
			USE IN 20
	ENDCASE
	UNLOCK 

	SELE ComPag
	IF RLOCK()
		SELECT ComPag
		GATHER MEMVAR
	ENDIF	
ENDIF
UNLOCK ALL
IF m.tipdoc#'IN'
	ON KEY LABEL F2 DO gircheq
	ON KEY LABEL F3 DO regret
ENDIF
ON KEY LABEL F4 DO imp_cp
DO gircheq
DO vista                    
RETURN


PROCEDURE ingre              
*--------------
PUBLIC vctadeb,vctahab,vvaldeb,vvalhab
DEACTIVATE WINDOW wind_3

IF escolor
	DEFINE POPUP xcomp FROM 00,15 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP xcomp FROM 00,15 COLOR SCHEME c_popup
ENDIF

IF newsistem='1'
	DEFINE BAR 1  OF xcomp PROMPT '\<a. Por Hoja de control Sede  '
	DEFINE BAR 2  OF xcomp PROMPT '\<b. Por Memor ndum Sede       '
	DEFINE BAR 3  OF xcomp PROMPT '\<c. Por Retenciones Sede      '
	DEFINE BAR 4  OF xcomp PROMPT '\<d. Por Subsidios Sede        '
	DEFINE BAR 5  OF xcomp PROMPT '\<e. Por Retenci¢n de Subsidios Sede '
	DEFINE BAR 6  OF xcomp PROMPT '\<f. Por Regularizaci¢n # Cta Sede '
	DEFINE BAR 7  OF xcomp PROMPT '\<g. Por Hoja de Control Actividades'
	DEFINE BAR 8  OF xcomp PROMPT '\<h. Por Retenciones Actividades'
	DEFINE BAR 9  OF xcomp PROMPT '\<i. Por Memo - Recurs.Direct.Recaud.'
	DEFINE BAR 10 OF xcomp PROMPT '\<j. Por Hoja de Control Sectores'
	DEFINE BAR 11 OF xcomp PROMPT '\<k. Por Retencioes Sectores'
	DEFINE BAR 12 OF xcomp PROMPT '\<l. Por Subsidios Sectores  '
	DEFINE BAR 13 OF xcomp PROMPT '\<m. Por Retenciones de Subsidios Sectores'
ELSE
	DEFINE BAR 1  OF xcomp PROMPT '\<a. Por Hoja de control Sede  '
	DEFINE BAR 2  OF xcomp PROMPT '\<b. Por Memor ndum Sede       '
 	DEFINE BAR 3  OF xcomp PROMPT '\<c. Por Retenciones Sede      '
*   DEFINE BAR 4  OF xcomp PROMPT '\<d. Por Meta                  '
   DEFINE BAR 5  OF xcomp PROMPT '\<e. Por Retenciones x Meta    '

ENDIF

ON SELECTION POPUP xcomp  DEACTIVATE POPUP
ACTIVATE POPUP xcomp

DO CASE

   CASE BAR() =  1
		dehc  = .T.
		STORE   .F. TO deme,dere,desh,dreg,desu,dinu,dehm,desr,deru,dess,ders,ddhc,derm,demt,DHCA,DREA,DMDR
		
   CASE BAR() =  2
		deme  = .T.
		STORE   .F. TO dere,desh,dreg,dinu,desu,dehc,dehm,desr,deru,dess,ders,ddhc,derm,demt,DHCA,DREA,DMDR		

   CASE BAR() =  3
		dere  = .T.
		STORE   .F. TO desh,dreg,dinu,desu,dehc,deme,dehm,desr,deru,dess,ders,ddhc,derm,demt,DHCA,DREA,DMDR

   CASE BAR() =  4
* 		IF newsistem ='1'
		desu  = .T.
		STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,dehm,desr,deru,dess,ders,ddhc,demt,derm,DHCA,DREA,DMDR
*		ELSE
*		demt  = .T.
*		STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,dehm,desr,deru,dess,ders,ddhc,desu,derm
*		ENDIF
   CASE BAR() =  5
		IF newsistem = '1'
		STORE   .T. TO deru
		STORE   .F. TO deme,dere,desh,dinu,desu,dehm,desr,dehc,dreg,dess,ders,ddhc,demt,derm,DHCA,DREA,DMDR
		ELSE
		STORE   .T. TO derm
		STORE   .F. TO deme,dere,desh,dinu,desu,dehm,desr,dehc,dreg,dess,ders,ddhc,deru,demt,DHCA,DREA,DMDR
		ENDIF

*		STORE   .T. TO deru
*		STORE   .F. TO deme,dere,desh,dinu,desu,dehm,desr,dehc,dreg,dess,ders,ddhc

   CASE BAR() =  6
		STORE   .T. TO dehc,dreg
		STORE   .F. TO deme,dere,desh,dinu,desu,dehm,desr,deru,dess,ders,ddhc,derm,demt,DHCA,DREA,DMDR
		
		
   CASE BAR() =  7
		dhca  = .T.
		STORE   .F. TO dere,desh,dreg,dinu,desu,dehc,dehm,desr,deru,dess,ders,ddhc,derm,demt,DEME,DREA,DMDR

   CASE BAR() =  8
		drea  = .T.
		STORE   .F. TO dehc,deme,dere,dreg,dinu,desu,dehm,desr,deru,dess,ders,ddhc,derm,demt,DESH,DHCA,DMDR

   CASE BAR() =  9
		dmdr  = .T.
		STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,dehm,desu,deru,dess,ders,ddhc,derm,demt,DESR,DHCA,DREA

*   CASE BAR() =  7
*		dehm  = .T.
*		STORE   .F. TO dehc,deme,dere,dreg,dinu,desu,desh ,desr,deru,dess,ders,ddhc,derm,demt
		
   CASE BAR() =  10
		desH  = .T.
		STORE   .F. TO dehc,deme,dere,dreg,dinu,desu,dehm,desr ,deru,dess,ders,ddhc,derm,demt,DHCA,DREA,DMDR

		
   CASE BAR() =  11
		desr  = .T.
		STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,dehm,desu,deru,dess,ders,ddhc,derm,demt,DHCA,DREA,DMDR
		

   CASE BAR() =  12
		dess  = .T.
		STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,dehm,desu,deru,desr,ders,ddhc,derm,demt,DHCA,DREA,DMDR

   CASE BAR() =  13
		ders  = .T.
		STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,dehm,desu,deru,desr,dess,ddhc,derm,demt,DHCA,DREA,DMDR
		
*   CASE BAR() =  12
*		dinu  = .T.
*		STORE   .F. TO dehc,deme,dere,desh,dreg,desu,dehm,desr,deru,dess,ders,ddhc,derm,demt
		
   OTHER
   		ddhc  = .T.
	   	STORE   .F. TO dehc,deme,dere,desh,dreg,dinu,desu,dehm,desr,deru,dess,ders,derm,demt,DHCA,DREA,DMDR
	   	
ENDCASE

RELEASE POPUP xcomp
SELECT ComPag
DO pantalla
SCATTER MEMVAR BLANK
m.fecref = DATE()
m.IncPres ='S'
m.glosa  = 'IMPORTE QUE SE GIRA POR '+SPACE(226)
vFlag=.T.

DO CASE

   CASE dehc && H/C de la Sede
   
   		OI=carhc()  &&Captura H/C
      	IF LASTKEY()=27 OR !OI
		   SELECT ComPag
		   DO vista
		   RETURN
		ENDIF
	    SELE ComPag
	    @ 2,22 SAY  val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
	    @ 1,22 SAY m.numcp
	    @ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	    @ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes))
	    IF dreg
	       STORE SPACE(40) TO m.nompre
           @ 03,02  SAY "Cta. de Devoluci¢n "
           m.nompre='DEVOLUCION A CTA MATRIZ'+SPACE(15)
		   @ 03,22  GET m.nompre  FUNCTION "!"  
	    ELSE	  
	       @ 3,22 SAY IIF(hoja.tipprv='P',m.codprv,iif(hoja.tipprv='E',m.codemp,iif(hoja.tipprv='O',m.codotr,' ')))
	       @ 3,30 SAY IIF(hoja.tipprv='P',val_auxi(ALLT(m.codprv),'20','D',30),iif(hoja.tipprv='E',val_auxi(ALLT(m.codemp),'30','D',30),iif(hoja.tipprv='O',val_auxi(ALLT(m.codotr),'09','D',30),m.nompre)))
 	    ENDIF  
        @ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
        @ 4,35 GET m.Refer     PICTURE '@S10' 	  
 	    @ 4,68 GET m.fecref VALID val_ff()
 	    @ 5,22 SAY Val_Para(m.codfte ,'CODFTE','D',22,47)
 	    
       	IF newsistem='1'
			=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
		ELSE
			=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
		ENDIF
 	    DO vis_calen
	    IF !EMPTY(m.destino)
	         ACTIVATE WINDOW wind_8
    	  	 @ 0,0  SAY 'Destino: '
	         @ 0,20 SAY m.destino PICTURE '@S73' FUNCTION "!"
		ENDIF    
	    READ VALID val_Read()
        DEACTIVATE WINDOW wind_8
        IF dreg
	        ACTIVATE WINDOW wind_11
			@ 0,02 SAY " N§ C/P Pr‚stamo: " GET m.mesaaff DEFAULT PADL(MONTH(DATE()),2,'0')
			@ 0,23 SAY '.'
			@ 0,24 GET m.numcpAf DEFAULT SPACE(4)
			@ 0,32 SAY "Cta.Cte:" GET m.codCtcAf DEFAULT SPACE(14)
    	    READ VALID val_READ()
    	    vpoint=RECNO()
    	    SEEK ALLT(m.mesaaff)+numcpaf+ALLT(m.codctcaf)
    	    IF FOUND()
    	       x_reten  =ComPag.reten
    	       REPLACE conaaff WITH '#'
			   IF !EMPTY(X_reten)
				  ACTIVATE WINDOW wind_12
				  @0,01 SAY "Partida" GET m.partret PICTURE '!!.!!' VALID VAL_PRET(SUBSTR(m.partret,4,2),LEFT(m.partret,2),'C',22) 
			      @0,30 SAY "Reten. "+STR(x_reten,10,2)
				  READ
	    	      REPLACE partret WITH m.partret
 				  USE reten  IN 12  ORDER TAG reten1  ALIAS reten
				  SELECT RETEN
			      SEEK ALLTRIM(m.mesaaff)+m.numcpaf+ALLTRIM(m.codctcaf)
			  	  IF FOUND() AND RLOCK()
				       SCAN WHILE  ALLTRIM(m.mesaaff)+m.numcpaf+ALLTRIM(m.codctcaf) = ALLT(RETEN.nummes)+RETEN.Numcp+ALLT(Reten.Codctc)
				            REPLACE partret WITH m.partret, nummeshc WITH m.nummeshc, numhc WITH m.numhc
				       ENDSCAN            
			      ENDIF   
			      USE IN 12
			      DEACTIVATE WINDOW wind_12
		      ENDIF	
		    ENDIF  
 			SELE ComPag   	    
    	    GO vpoint
  	        DEACTIVATE WINDOW wind_11
      ENDIF
	  
	  m.docref = 'H/C'
	  m.tipdoc = IIF(dreg=.T.,'RG','HC')

&&&&Retenciones x Meta
*======================
CASE derm
	  STORE SPACE(5) TO cccomp,ccmet
      SELE Caja
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
	  SELECT ComPag
	  @ 1,22 SAY m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 3,02 SAY "      C¢digo Meta"
      @ 3,22 GET m.codcad PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) AND PGRET('1') 	  
*      @ 3,VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,02 SAY '   Rubro Retenci¢n.'
      @ 4,22 get m.codret VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) 
*      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
		
*      IF newsistem='1'
*     	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
*      ELSE
*       	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 	          DISABLE
*	  ENDIF

	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'RE'


&&&&Fin Retenciones x Meta

  CASE deme OR ddhc

      @ 00,22 GET m.Prestamo FUNCTION "M N,S"
      @ 00,61 GET m.IncPres  FUNCTION "M S,N"
	  @ 2,22  GET m.codctc   PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja
	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
      SELECT ComPag
      m.docref='MEM'

	  @ 1,22 SAY m.numcp 
	  IF ddhc
		  @ 1,22 GET m.numcp 
	  ENDIF
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	  @ 3,22 GET m.TipPrv    FUNCTION "M P,E,O,R,I"   VALID Limpia() 

      @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
      @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
      @ 3,24 GET m.Codpre    PICTURE "!!!!!!!!"  VALID val_AuxiD(m.Codpre,'80',"A",30) WHEN m.TipPrv='I'
      @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
      @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID VAL_ParaD(m.Codotr,'CODRET',' ',24,40) WHEN m.TipPrv='R'
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40) 
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  	IF m.IncPres='S'
	  		IF newsistem='1'
     		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
     		ELSE
     		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 	  
			ENDIF
      	ELSE 
      	  m.codcad='0000'
      	ENDIF
	  READ VALID VAL_READ() 
      IF !EMPTY(m.CodCad) 
       	IF newsistem='1'
			=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
		ELSE
			=val_codca1(ALLTRIM(m.codcad),m.periodo,'C')
		ENDIF
      	DO vis_calen
      ENDIF

      ACTIVATE WINDOW wind_8
   	  @ 0,0  SAY 'Destino: '
	  @ 0,20 GET m.destino PICTURE '@S73' FUNCTION "!"
      READ VALID val_Read()
      DEACTIVATE WINDOW wind_8

 	  m.tipdoc = 'ME'

   CASE dere &&Retenci¢n Sede

	  STORE SPACE(5) TO cccomp,ccmet
      SELE Caja
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
	  SELECT ComPag
	  @ 1,22 SAY m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 3,02 SAY "Fuente de Retenci¢n"
      @ 3,22 GET m.codret    VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)

      IF newsistem='1'
     	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
      ELSE
       	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCa1(m.codcad,m.periodo,' ',22,47) 	          DISABLE
	  ENDIF

	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'RE'

   CASE deru

      STORE SPACE(5) TO cccomp,ccmet
      SELE Caja
      SET ORDER TO Cajas6
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
	  SELECT ComPag
	  @ 1,22 SAY m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 03,02 SAY "Fuente de Retencion"
      @ 3,22 GET m.codret    VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'RU'
      SELE Caja
      SET ORDER TO Cajas2


   CASE dehm

	  USE hojmod   IN 20  ORDER TAG hojmod1      ALIAS hojm   &&H/M
      OJ=carhm()  &&Captura H/M
      IF LASTKEY()=27 OR !OJ
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
	  USE IN 20
	  SELE ComPag
	  @ 2,22 SAY  val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc)
	  @ 1,22 SAY m.numcp
	  @ 1,27 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes))
      STORE SPACE(40) TO m.nompre
      @ 03,02  SAY "Cta. de Devoluci¢n "
      m.nompre='DEVOLUCION A CTA MATRIZ'+SPACE(15)
	  @ 03,22  GET m.nompre  FUNCTION "!"  
      @ 4,22 SAY m.numhm+'.'+m.nummeshm + ' H/M'
      @ 4,35 GET m.Refer     PICTURE '@S10' 	  
 	  @ 4,68 GET m.fecref VALID val_ff()
 	  DO vis_calen
	  IF !EMPTY(m.destino)
	      ACTIVATE WINDOW wind_8
    	  @ 0,0  SAY 'Destino: '
	      @ 0,20 SAY m.destino PICTURE '@S73' FUNCTION "!"
	  ENDIF    
	  READ VALID val_Read()
      DEACTIVATE WINDOW wind_8
	  m.docref = 'H/M'
	  m.tipdoc = 'HM'

  CASE desH  &&Sectores por Hoja de Control

      SELE Caja
      SET ORDER TO Cajas5
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF	
      DO BusCaja1

 	  vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_13
	  @ 1, 05 SAY "Sector        :" GET vsector  VALID VAL_PARA(vSector,'SECT97',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_13

   	  OR=carSHC(ALLTRIM(vsector))  &&Captura H/C
      IF LASTKEY()=27 OR !OR
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
      SELE ComPag
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()  AND valsec() 	  
	  @ 1,22 GET m.numcp     VALID vexiste() .AND. ((LEFT(m.numcp,1)=LEFT(m.numhc,1)) .OR. LEFT(m.numcp,1)='A') PICTURE '!999'
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes))   AND IngFec() 
      @ 3,22 SAY IIF(hoja.tipprv='P',m.codprv,iif(hoja.tipprv='E',m.codemp,iif(hoja.tipprv='O',m.codotr,' ')))
      @ 3,30 SAY IIF(hoja.tipprv='P',val_auxi(ALLT(m.codprv),'20','D',30),iif(hoja.tipprv='E',val_auxi(ALLT(m.codemp),'30','D',30),iif(hoja.tipprv='O',val_auxi(ALLT(m.codotr),'09','D',30),m.nompre)))
      @ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
      @ 4,68 GET m.fecref VALID val_ff()
      @ 5,22 SAY Val_Para(m.codfte ,'CODFTE','D',22,47)
      =val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      DO vis_calen
  	  IF !EMPTY(m.destino)
	     ACTIVATE WINDOW wind_8
      	 @ 0,0  SAY 'Destino: '
	     @ 0,20 SAY m.destino PICTURE '@S73' FUNCTION "!"
	  ENDIF    
      READ VALID val_Read()
      DEACTIVATE WINDOW wind_8
  	  m.docref = 'H/C'
	  m.tipdoc = 'SH'
	  m.estado = '00'
      SELE Caja
      SET ORDER TO Cajas2

   CASE desr    && Retenciones Sectores
   
      STORE SPACE(5) TO cccomp,ccmet
      SELE Caja
      SET ORDER TO Cajas5
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja1
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF

 	  vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_13
	  @ 1, 05 SAY "Sector        :" GET vsector  VALID VAL_PARA(vSector,'SECT97',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_13
  	  

	  SELECT ComPag
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() AND valse1(vsector)
	  @ 1,22 GET m.numcp     VALID vexiste() .AND. (LEFT(m.numcp,1)=ALLT(VSECTOR)) PICTURE '!999'
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 3,02 SAY "Fuente de Retenci¢n"
      @ 3,22 GET m.codret    VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'SR'
      SELE Caja
      SET ORDER TO Cajas2
     
  CASE desu  &&Subsidios Sede

      SELE Caja
      SET ORDER TO Cajas6
      @ 00,61 get m.IncPres  FUNCTION "M S,N"
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja
	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
      SELECT ComPag
      m.docref='MEM'
	  @ 1,22 SAY m.numcp 
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	  @ 3,22 GET m.TipPrv    FUNCTION "M P,E,O"   VALID Limpia() 
      @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
      @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
      @ 3,24 GET m.CodOtr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  IF m.IncPres='S'
     	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
      ELSE 
      	  m.codcad='0000'
      ENDIF
	  READ VALID VAL_READ() 
      IF !EMPTY(m.CodCad) 
      	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      	DO vis_calen
      ENDIF
 	  m.tipdoc = 'SU'
      SELE Caja
      SET ORDER TO Cajas2


   CASE dess &&Subsidios Sectores

      SELE Caja
      SET ORDER TO Cajas6
      @ 00,61 get m.IncPres  FUNCTION "M N,S"
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
      m.docref='MEM'
	  *@ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  *READ
	  DO busCaja1
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
***	 
 	  vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_13
	  @ 1, 05 SAY "Sector        :" GET vsector  VALID VAL_PARA(vSector,'SECT97',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_13
*** 
	  
	  SELECT ComPag
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() AND valse1(VSECTOR) 	  
	  @ 1,22 GET m.numcp     VALID vexiste() .AND. (LEFT(m.numcp,1)=ALLT(VSECTOR)) PICTURE '!999'
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
*	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	  @ 3,22 GET m.TipPrv    FUNCTION "M P,E,O"   VALID Limpia() 
      @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
      @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
      @ 3,24 GET m.CodOtr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40) AND m.docref#'H/C'
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  IF m.IncPres='S'
     	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
      ELSE 
      	  m.codcad='0000'
      ENDIF
	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'SS'
      SELE Caja
      SET ORDER TO Cajas2
      
	 CASE deRS &&Ret. Sub. Sctores
	 
	  STORE SPACE(5) TO cccomp,ccmet	 
      SELE Caja
      SET ORDER TO Cajas6
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
     *vcodprg='02'
     *@ 6,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) 
     *@ 7,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)  
	  READ
	    
	  DO busCaja1
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
******	  
	   vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_13
	  @ 1, 05 SAY "Sector        :" GET vsector  VALID VAL_PARA(vSector,'SECT97',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_13
******	  
	  
	  
	  SELECT ComPag
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() AND valsec() 	  
	  @ 1,22 GET m.numcp     VALID vexiste() .AND. (LEFT(m.numcp,1)=ALLT(VSECTOR)) PICTURE '!999'
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 03,02 SAY "Fuente de Retencion"
      @ 3,22 GET m.codret    VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  @ 5,60 GET m.tipfun    PICTURE '!'   VALID val_para(m.tipfun,'TIPFUN',' ',60,15,2)
   	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
      *@ 8,22 GET vProyec     PICTURE '!!!'  VALID VAL_PYAC(ALLTRIM(vProyec),m.periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vproyec',22,40)  when m.tipfun='I'
      *@ 8,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(ALLTRIM(vCodact),m.periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  when m.tipfun='F' 
      *@ 8,26 GET vSubPry     PICTURE '!!!!' VALID VAL_SUPY(ALLTRIM(vSubPry),m.periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub)+ALLTRIM(vProyec),'vSubPry',22,40)  when !EMPTY(vProyec) and ALLTRIM(m.tipfun)='I'
	  READ VALID val_READ() 
      *m.CodCal  = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)
      *+ALLTRIM(vCodSub)+IIF(m.Tipfun='I',ALLTRIM(vProyec)+ALLTRIM(vSubPry),ALLTRIM(vCodAct))	  
	  m.estado = '00'
   	  m.tipdoc = 'RS'
      SELE Caja
      SET ORDER TO Cajas2


   CASE dinu

	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22) 
	  m.nompre='I N U T I L I Z A D O'
	  READ
	  DO busCaja
	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
      SELECT ComPag
	  @ 1,22 GET m.numcp  
	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() AND vexiste()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	  @ 3,22 GET m.nompre    function "!"
	  READ VALID VAL_READ() 
	  m.estado = '92'
      m.tipdoc = 'IN'

** ppn
  CASE dmdr

      @ 00,22 GET m.Prestamo FUNCTION "M N,S"
      @ 00,61 GET m.IncPres  FUNCTION "M S,N"
	  @ 2,22  GET m.codctc   PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja1
	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF

      m.docref='MEM'
*
 	  vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_14
	  @ 1,05 SAY "Actividad     :" GET vsector  VALID VAL_PARA(vSector,'ACTT98',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_14

      SELECT ComPag
	  
	  @ 1,27 GET m.nummes PICTURE '!!' VALID val_para(m.nummes  ,'FECMES',' ',27,9) .AND. valfecha() .AND. valse1(vsector)
	  @ 1,22 GET m.numcp VALID vexiste() .AND. (LEFT(m.numcp,1)=ALLT(VSECTOR)) PICTURE '!999'
*	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha()
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
	  @ 3,22 GET m.TipPrv    FUNCTION "M P,E,O,R"   VALID Limpia() 

      @ 3,24 GET m.CodPrv    PICTURE "!!!!"    VALID val_AuxiD(m.CodPrv,'20',"A",30) WHEN m.TipPrv='P'
      @ 3,24 GET m.Codemp    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codemp,'30',"A",30) WHEN m.TipPrv='E'
      @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID val_AuxiD(m.Codotr,'09',"A",30) WHEN m.TipPrv='O'
      @ 3,24 GET m.Codotr    PICTURE "!!!!!!"  VALID VAL_ParaD(m.Codotr,'CODRET',' ',24,40) WHEN m.TipPrv='R'
	  @ 4,22 GET m.docref    VALID val_para(m.docref,'TIPDOC','C',22,40) 
	  @ 4,26 GET m.numref    PICTURE '!!!!.!!'
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  	IF m.IncPres='S'
     		  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  
      	ELSE 
      	  m.codcad='0000'
      	ENDIF
	  READ VALID VAL_READ() 
      IF !EMPTY(m.CodCad) 
			=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      	DO vis_calen
      ENDIF

      ACTIVATE WINDOW wind_8
   	  @ 0,0  SAY 'Destino: '
	  @ 0,20 GET m.destino PICTURE '@S73' FUNCTION "!"
      READ VALID val_Read()
      DEACTIVATE WINDOW wind_8

 	  m.tipdoc = 'ME'

  CASE dHca  &&ACTIVIDAD por Hoja de Control

      SELE Caja
      SET ORDER TO Cajas5
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF	
      DO BusCaja1

 	  vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_14
	  @ 1,05 SAY "Actividad     :" GET vsector  VALID VAL_PARA(vSector,'ACTT98',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_14

  	  OR=carSHC(ALLTRIM(vsector))  &&Captura H/C
*   	  OR=carHCA()  &&Captura H/C
      IF LASTKEY()=27 OR !OR
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF
      SELE ComPag
*    	@ 1,22 GET m.numcp     VALID vexiste() .AND. ((LEFT(m.numcp,1)=LEFT(m.numhc,1)) .OR. LEFT(m.numcp,1)='A')  AND valse1(vsector) PICTURE '!999'
*		@ 1,22 get m.numcp   
  	  @ 1,27 GET m.nummes    PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() .AND. VALSE1(VSECTOR) 	  	  
	  @ 1,22 GET m.numcp     VALID vexiste()  .AND. VALSE1(VSECTOR) PICTURE '!999'
	  @ 1,60 GET m.feccp     VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec() 
      @ 3,22 SAY IIF(hoja.tipprv='P',m.codprv,iif(hoja.tipprv='E',m.codemp,iif(hoja.tipprv='O',m.codotr,' ')))
      @ 3,30 SAY IIF(hoja.tipprv='P',val_auxi(ALLT(m.codprv),'20','D',30),iif(hoja.tipprv='E',val_auxi(ALLT(m.codemp),'30','D',30),iif(hoja.tipprv='O',val_auxi(ALLT(m.codotr),'09','D',30),m.nompre)))
      @ 4,22 SAY m.numhc+'.'+m.nummeshc + ' H/C'
      @ 4,68 GET m.fecref VALID val_ff()
      @ 5,22 SAY Val_Para(m.codfte ,'CODFTE','D',22,47)
      =val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
      DO vis_calen
  	  IF !EMPTY(m.destino)
	     ACTIVATE WINDOW wind_8
      	 @ 0,0  SAY 'Destino: '
	     @ 0,20 SAY m.destino PICTURE '@S73' FUNCTION "!"
	  ENDIF    
      READ VALID val_Read()
      DEACTIVATE WINDOW wind_8
  	  m.docref = 'H/C'
	  m.tipdoc = 'AH'
	  m.estado = '00'
      SELE Caja
      SET ORDER TO Cajas2



   CASE drea    && Retenciones Actividad
   
      STORE SPACE(5) TO cccomp,ccmet
      SELE Caja
      SET ORDER TO Cajas5
      m.docref='C/P'
	  @ 2,22 GET m.codctc PICTURE "@!" VALID val_fun('Caja', 'Codctc', "CodCtc+' '+Descri",m.codctc,1,2,22)
	  READ
	  DO busCaja1
  	  IF LASTKEY()=27
		 SELECT ComPag
		 DO vista
		 RETURN
	  ENDIF

 	  vsector = SPACE(01)
 	  ACTIVATE WINDOW Wind_14
	  @ 1,05 SAY "Actividad     :" GET vsector  VALID VAL_PARA(vSector,'ACTT98',' ',21,40) 
	  READ VALID Val_read()
	  DEACTIVATE WINDOW Wind_14
*  	  
	  SELECT ComPag
	  @ 1,27 GET m.nummes PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',27,9) AND valfecha() AND valse1(vsector)
	  @ 1,22 GET m.numcp VALID vexiste() .AND. (LEFT(m.numcp,1)=ALLT(VSECTOR)) PICTURE '!999'
	  @ 1,60 GET m.feccp VALID(MONTH(m.feccp)=VAL(m.nummes)) AND IngFec()
      @ 3,02 SAY "Fuente de Retenci¢n"
      @ 3,22 GET m.codret    VALID VAL_PARAD(m.codret,'CODRET',' ',22,40) AND Pagret('1')
      @ 4,34 GET m.Refer     PICTURE '@S10'
	  @ 4,68 GET m.fecref VALID val_ff()
	  @ 5,22 GET m.codfte    PICTURE '!!!' VALID val_para(m.codfte ,'CODFTE',' ',22,20)
	  @ 6,22 GET m.CodCad    PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 	  DISABLE
	  READ VALID val_READ() 
      IF !EMPTY(m.CodCad) 
      	DO vis_calen
      ENDIF
	  m.estado = '00'
   	  m.tipdoc = 'AR'
      SELE Caja
      SET ORDER TO Cajas2
      
ENDCASE
***
m.usuario=wUser_ID
IF m.tipdoc#'IN'
	@ 08,22 GET m.glosa  PICTURE '@S56' WHEN !EMPTY(m.NumCp) FUNCTION "!"
	@ 09,22 GET m.observ PICTURE '@S56' WHEN !EMPTY(m.NumCp) FUNCTION "!"
	READ VALID val_read()
ENDIF
IF LASTKEY() # 27
    m.estado = '00'  
    DO CASE
       CASE m.tipprv='P'
            STORE SPACE(5) TO m.codemp
            STORE SPACE(6) TO m.codotr
            store space(8) to m.codpre
       CASE m.tipprv='E'
            STORE SPACE(4) TO m.codprv
            store space(8) to m.codpre
            STORE SPACE(6) TO m.codotr
       CASE m.tipprv='O'
            STORE SPACE(4) TO m.codprv
            store space(8) to m.codpre
            STORE SPACE(5) TO m.codemp
       CASE m.tipprv='I'
            STORE SPACE(4) TO m.codprv
            store space(6) to m.codotr
            STORE SPACE(5) TO m.codemp
    ENDCASE 
    SELECT Caja
	SEEK m.codctc
    w_tipctc=Caja.Tipo
	SELECT ComPag
	ingreso = .T.
	giro = .F.
	DO CASE
       	CASE dehc OR dreg OR desh OR DHCA
			DO trabaja_hijo
		CASE dehm
		    DO tra_hijo	
		OTHER    
			DO traba_hijo
	ENDCASE
	if giro 
		SELE ComPag
	   	RETURN .F.
	endif		    	
	vtotal = 0
	IF 	m.tipdoc#'IN'
		DO CASE
	
			CASE dehc OR desh OR DHCA
				SELECT hoja
				SEEK ALLTRIM(m.nummeshc)+m.numhc
				IF RLOCK() AND FOUND()
					REPLACE nummescp WITH m.nummes,;
					        numcp    WITH m.numcp,;
					        codctc   WITH m.codctc,;
					        estado   WITH '50'
				ENDIF
				IF !EMPTY(m.nummeshm) AND !EMPTY(m.numhm)
					USE hojmod   IN 20  ORDER TAG hojmod1      ALIAS hojm   &&H/M
					SELECT hojM
					SEEK ALLTRIM(m.nummeshm)+m.numhm
					IF RLOCK()  AND FOUND()
						REPLACE nummescp WITH m.nummes,;
						        numcp WITH m.numcp,;
						        codctc WITH m.codctc,;
						        estado WITH '52' 
					ENDIF
					USE IN 20
				ENDIF	
				
			CASE dreg	
				SELECT hoja
				SEEK ALLTRIM(m.nummeshc)+m.numhc
				IF RLOCK()  AND FOUND()
					REPLACE nummescp WITH m.nummes,;
					        numcp    WITH m.numcp,;
					        codctc   WITH m.codctc,;
					        estado   WITH '50',;
					        reten WITH x_reten,;
					        partret WITH m.partret
				ENDIF
			    
			CASE dehm
			
				USE hojmod   IN 20  ORDER TAG hojmod1      ALIAS hojm   &&H/M
				SELECT hojM
				SEEK ALLTRIM(m.nummeshm)+m.numhm
				IF RLOCK()  AND FOUND()
					REPLACE nummescp WITH m.nummes,;
					        codctc   WITH m.codctc,;
					        numcp WITH m.numcp,;
					        estado WITH '50' 
				ENDIF
				USE IN 20
		ENDCASE
		UNLOCK 
	    DO Total_Padre
	 	SELECT ComPag		
	    m.import = vtotal
	    DO ingap
	 	SELECT ComPag		
	    
		IF !desu AND !deme AND !deru AND !dess AND 	!ders and !DMDR
			DO Compre
		 	SELECT ComPag		
		    m.ctadeb = vctadeb
		    m.ctahab = vctahab
		    m.valdeb = vvaldeb 
		    m.valhab = vvalhab 	
		ENDIF	
	    SELECT Caja
		SEEK m.codctc
    IF derm OR deme OR dehc OR dehm OR desu OR dreg OR dinu OR dere OR deru or ddhc
			IF RLOCK()
				REPLACE corcp WITH corcp + 1
			ENDIF
			unlock
		ELSE
		IF (desh .AND. m.codfte<>'00') OR (dhca .and. m.codfte<>'09')
				IF RLOCK()
					REPLACE corcp WITH corcp + 1
				ENDIF
				unlock
			ELSE
			if (desh .and. m.codfte ='00') or (dhca .and. m.codfte='09')
				USE CtaSec IN 27 ORDER CtaSec1 ALIAS CtaSec
				SELE CtaSeC
				IF LEFT(m.numhc,1) $ '0123456789'
					v='A'
				ELSE
					v=LEFT(m.numhc,1)
				ENDIF 
				IF DHCA  .AND. m.codfte='09'
					v=vsector
				endif
*				v=LEFT(m.numhc,1)
*			ENDIF 
				SEEK allt(m.codctc)+ALLT(V)
				IF RLOCK()
					REPLACE CtaSec.corsec WITH CtaSec.corsec + 1
				ENDIF
				unlock
				USE
            else
	 			if desr OR DESS OR DERS or drea OR DMDR
					USE CtaSec IN 27 ORDER CtaSec1 ALIAS CtaSec
					SELE CtaSeC
					SEEK allt(m.codctc)+allt(Vsector)
					IF RLOCK()
						REPLACE CtaSec.corsec WITH CtaSec.corsec + 1
					ENDIF
					unlock
					USE

			  endif
			endif
		ENDIF
	endif
	SELECT ComPag
    ELSE
	    m.import = 0
	ENDIF
	IF f_appd()
       GATHER MEMVAR
    ELSE
       DO STANDBY WITH "Atenci¢n: No grabo el Registro"   
    ENDIF	
    UNLOCK
ENDIF
IF m.tipdoc#'IN'
	ON KEY LABEL F2 DO gircheq
	ON KEY LABEL F3 DO regret
ENDIF
ON KEY LABEL F4 DO imp_cp
DO PANTALLA
DO vista
RETURN
**
PROCEDURE carhcA
*--------------
* Carga valores de HC ACTIVIDADES
vfun = .T.
OK   = FOUND()
SELECT hoja
vvord=ORDER()
SET FILT TO
SET ORDER TO hojcon4
GO TOP
IF EOF()
	DO standby WITH 'No existen documentos H/C '
	*SET FILT TO
	SET ORDER TO (vvord)
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHc  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/C: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 SAY '.'
@ 1,17 GET vHC DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
   SET ORDER TO (vvord)
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHC  = PADL(ALLTRIM(vHC),4,'0')
IF !SEEK(ALLTRIM(vMes)+vHC)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HC con F10 "
   BROWSE NOEDIT WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhc :H="HC",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O" AND EMPTY(codotr),NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),IIF(tipprv="E",LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codotr), 20)))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechc :H="Fecha",;
     x3=codcad+' '+codctc :H="Cad.ctc"
ENDIF
IF LASTKEY()#27
    SELE HOJA
	m.nummes   = nummes
	m.nummeshc = nummes
	m.numhc    = numhc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
	m.CodCad   = codcad
	m.codctc   = codctc
	m.CodPart  = CodPart
	m.tipdoc   = tipdoc
	m.nombre   = nompre
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = Destino
	m.codobra  = codobra
	m.codfte   = codfte
	m.numref   = Numhc+"."+Nummes
	m.partret  = partret
	Vref       = SPACE(20)
    Lseek = Periodo+Numref+ALLTRIM(Codfte)
	DO CASE
	   CASE ALLTRIM(tipdoc)='O/C'
	   		USE OrdCom   IN 23   ORDER TAG OrdCom1    ALIAS Orden
			USE ItePec   IN 30   ORDER TAG ItePec12   ALIAS Itepec
			USE Pecosa   IN 31   ORDER TAG Pecosa1    ALIAS Pecosa
			SELE ItePec
			SET RELATION TO Periodo+NumPec+CodFte+CodCad INTO Pecosa
			SELE ORDEN
			SEEK ALLTRIM(Lseek)
		    IF FOUND()
		      IF ALLTRIM(orden.tipdoc)#'OK'
				   DO STANDBY WITH 'La O/C no est  liquidada llamar a Abastecimentos'
		           USE IN 23
		           USE IN 30
			       SELE ComPag
		    	   RETURN .F.
		      ELSE
		           vper =periodo
		           vordc=numoc
		           vmesc=nummes
		           vref='O/C ('+orden.numoc+'.'+orden.nummes+')'
		           SELE itepec
		           SEEK vper+vordc+vmesc
		           IF FOUND()
			           vref1='PECOSA ('+itepec.numpec+'.'+PADL(MONTH(fecpec),2,'0')+')'
		           ENDIF
		       ENDIF 
		    ENDIF   
		    USE IN 30
		    USE IN 23
		    
	   CASE ALLTRIM(tipdoc)='O/S'
			USE SolSer   IN 32  ORDER TAG SolSer4   ALIAS Solser
			USE Ordser   IN 24  ORDER TAG OrdSer1   ALIAS OrdSer
			SELE OrdSer
			SEEK ALLTRIM(Lseek)
		    IF FOUND() 
		       IF ALLTRIM(OrdSer.tipdoc)#'OK'
				   DO STANDBY WITH 'La O/S no est  liquidada llamar a Abastecimentos'
		           USE IN 24
		           USE IN 32
			       SELE ComPag
		    	   RETURN .F.
		       ELSE
		           vper =periodo
		           vordc=numos
		           vref='O/S ('+ordser.numos+'.'+nummesos+')'
		           SET MEMOWIDTH TO 60
		           Vglosa2=MLINE(ordser.detalle,1)+MLINE(ordser.detalle,2)+MLINE(ordser.detalle,3)+MLINE(ordser.detalle,4)
                   m.glosa=ALLT(m.glosa)+SPACE(1)+vglosa2
		           SELE Solser
		           SEEK vper+vordc
		           IF FOUND()
			           vref1='S/S ('+solser.numss+'.'+PADL(MONTH(FecSS),2,"0")+')'
		           ENDIF
		       ENDIF 
		    ENDIF   
		    USE IN 32
		    USE IN 24
	ENDCASE   
	SELE Hoja
	m.observ='H/C('+m.numref+')'+','+vref+','+vref1+SPACE(80)
	m.glosa =LEFT(m.glosa,250)
	SHOW GETS
	*SELECT Caja
	*SEEK ALLTRIM(m.codctc)
	*IF !FOUND()
	*	DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
	*ELSE
		&&&MENSAJE 
	*	IF RLOCK()
	*		m.numcp =  PADL(corcp+1,4,'0')
	*		w_ctah  =  Caja.CuentaH
			*
	*	ELSE
	*		DO STANDBY WITH "Archivo utilizado...Presione ESC y Espere un momento"
			*RETURN .F.
	*	ENDIF
	*ENDIF
	SELECT hoja
	SET ORDER TO (vvord)
	SELECT ComPag
ENDIF
RETURN .T.


PROCEDURE carhc
*--------------
* Carga valores de HC
vfun = .T.
OK   = FOUND()
SELECT hoja
vvord=ORDER()
SET FILT TO
SET ORDER TO hojcon4
GO TOP
IF EOF()
	DO standby WITH 'No existen documentos H/C '
	*SET FILT TO
	SET ORDER TO (vvord)
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHc  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/C: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 SAY '.'
@ 1,17 GET vHC DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
   SET ORDER TO (vvord)
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHC  = PADL(ALLTRIM(vHC),4,'0')
IF !SEEK(ALLTRIM(vMes)+vHC)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HC con F10 "
   BROWSE NOEDIT WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhc :H="HC",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O" AND EMPTY(codotr),NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),IIF(tipprv="E",LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codotr), 20)))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechc :H="Fecha",;
     x3=codcad+' '+codctc :H="Cad.ctc"
ENDIF
IF LASTKEY()#27
    SELE HOJA
	m.nummes   = nummes
	m.nummeshc = nummes
	m.numhc    = numhc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
	m.CodCad   = codcad
	m.codctc   = codctc
	m.CodPart  = CodPart
	m.tipdoc   = tipdoc
	m.nombre   = nompre
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = Destino
	m.codobra  = codobra
	m.codfte   = codfte
	m.numref   = Numhc+"."+Nummes
	m.partret  = partret
	Vref       = SPACE(20)
    Lseek = Periodo+Numref+ALLTRIM(Codfte)
	DO CASE
	   CASE ALLTRIM(tipdoc)='O/C'
	   		USE OrdCom   IN 23   ORDER TAG OrdCom1    ALIAS Orden
			USE ItePec   IN 30   ORDER TAG ItePec12   ALIAS Itepec
			USE Pecosa   IN 31   ORDER TAG Pecosa1    ALIAS Pecosa
			SELE ItePec
			SET RELATION TO Periodo+NumPec+CodFte+CodCad INTO Pecosa
			SELE ORDEN
			SEEK ALLTRIM(Lseek)
		    IF FOUND()
		      IF ALLTRIM(orden.tipdoc)#'OK'
				   DO STANDBY WITH 'La O/C no est  liquidada llamar a Abastecimentos'
		           USE IN 23
		           USE IN 30
			       SELE ComPag
		    	   RETURN .F.
		      ELSE
		           vper =periodo
		           vordc=numoc
		           vmesc=nummes
		           vref='O/C ('+orden.numoc+'.'+orden.nummes+')'
		           SELE itepec
		           SEEK vper+vordc+vmesc
		           IF FOUND()
			           vref1='PECOSA ('+itepec.numpec+'.'+PADL(MONTH(fecpec),2,'0')+')'
		           ENDIF
		       ENDIF 
		    ENDIF   
		    USE IN 30
		    USE IN 23
		    
	   CASE ALLTRIM(tipdoc)='O/S'
			USE SolSer   IN 32  ORDER TAG SolSer4   ALIAS Solser
			USE Ordser   IN 24  ORDER TAG OrdSer1   ALIAS OrdSer
			SELE OrdSer
			SEEK ALLTRIM(Lseek)
		    IF FOUND() 
		       IF ALLTRIM(OrdSer.tipdoc)#'OK'
				   DO STANDBY WITH 'La O/S no est  liquidada llamar a Abastecimentos'
		           USE IN 24
		           USE IN 32
			       SELE ComPag
		    	   RETURN .F.
		       ELSE
		           vper =periodo
		           vordc=numos
		           vref='O/S ('+ordser.numos+'.'+nummesos+')'
		           SET MEMOWIDTH TO 60
		           Vglosa2=MLINE(ordser.detalle,1)+MLINE(ordser.detalle,2)+MLINE(ordser.detalle,3)+MLINE(ordser.detalle,4)
                   m.glosa=ALLT(m.glosa)+SPACE(1)+vglosa2
		           SELE Solser
		           SEEK vper+vordc
		           IF FOUND()
			           vref1='S/S ('+solser.numss+'.'+PADL(MONTH(FecSS),2,"0")+')'
		           ENDIF
		       ENDIF 
		    ENDIF   
		    USE IN 32
		    USE IN 24
	ENDCASE   
	SELE Hoja
	m.observ='H/C('+m.numref+')'+','+vref+','+vref1+SPACE(80)
	m.glosa =LEFT(m.glosa,250)
	SHOW GETS
	SELECT Caja
	SEEK ALLTRIM(m.codctc)
	IF !FOUND()
		DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
	ELSE
		&&&MENSAJE 
	*	IF RLOCK()
			m.numcp =  PADL(corcp+1,4,'0')
			w_ctah  =  Caja.CuentaH
			*
	*	ELSE
	*		DO STANDBY WITH "Archivo utilizado...Presione ESC y Espere un momento"
			*RETURN .F.
	*	ENDIF
	ENDIF
	SELECT hoja
	SET ORDER TO (vvord)
	SELECT ComPag
ENDIF
RETURN .T.


PROCEDURE carhm
*--------------
* Carga valores de HM
vfun = .T.
OK   = FOUND()
SELECT hojm
SET FILTER TO Operac$'TC' AND Estado='20'
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos H/M '
	SET FILT TO
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHm  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/M: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 SAY '.'
@ 1,17 GET vHm DEFAULT SPACE(4)
READ
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILT TO
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHm  = PADL(ALLTRIM(vHm),4,'0')
IF !SEEK(ALLTRIM(vMes)+vHm)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HM con F10 "
   BROWSE NOED WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhm :H="HM",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O" AND EMPTY(codotr),NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),IIF(tipprv="E",LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codotr), 20)))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechm :H="fecha",;
     x3=codcal+' '+codctc :H="Cal.ctc"
ENDIF
IF LASTKEY()#27
    SELE Hojm
	m.tipfun   = tipfun
	m.nummeshm = nummes
	m.numhm    = numhm
	m.nummes   = nummes
	m.nummeshc = nummeshc
	m.numhc    = numhc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
	m.codctc   = codctc
	m.CodPart  = CodPart
	m.tipdoc   = tipdoc
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = Destino
	m.codfte   = IIF(codfte='TRN','PRP','TRN')
    m.CodCal   = STUFF(codcal, 5, 3, m.codfte)	
	m.numref   = Numhm+"."+Nummes
	SELE Hojm
	SHOW GETS
	SELECT Caja
	SEEK ALLTRIM(m.codctc)
	IF !FOUND()
		DO standby WITH 'No se tiene inscrito la cuenta ' + m.codctc
	ELSE
		*IF RLOCK()
			m.numcp =  PADL(corcp+1,4,'0')
			w_ctah  =  Caja.CuentaH
		*ELSE
		*	DO STANDBY WITH "Archivo utilizado...Presione ESC y Espere un momento" 
		*	RETURN .F.
		*ENDIF
	ENDIF
	SELECT hojm
	SET FILT TO
	SELECT ComPag
ENDIF
RETURN .T.


PROCEDURE carSHC
*---------------
PARAMETERS vsec
* Carga valores de HC Sectorial
vfun = .T.
OK   = FOUND()

SELECT hoja
vvord= ORDER()

DO CASE
   CASE vsec='A' .and. ftew='00' 	
   		SET ORDER TO Hojcon6
   CASE vsec='E' .and. ftew='00' 	
   		SET ORDER TO Hojcon7
   CASE vsec='I' 	
   		SET ORDER TO Hojcon8
   CASE vsec='T'
   		SET ORDER TO Hojcon9
   CASE vsec='P' .AND. ftew='00'
   		SET ORDER TO Hojcon10
*   CASE vsec='M'
*   		SET ORDER TO Hojcon12
*   CASE vsec='V'
*   		SET ORDER TO Hojcon13
*   CASE vsec='G' 	
*   		SET ORDER TO Hojcon14
*   CASE vsec='S'
*   		SET ORDER TO Hojcon15
*   CASE vsec='F' 	
*  		SET ORDER TO Hojcon16
*   CASE vsec='B' 	
	OTHERWISE
  		SET ORDER TO Hojcon4
ENDCASE
   		
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos H/C para sectores'
	SET ORDER TO (vvord)
	SELE compag
	RETURN .F.
ENDIF
* Trata de ingresar directamente
vMes ='00'
vHc  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/C: " GET vMes DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 SAY '.'
@ 1,17 GET vHC DEFAULT SPACE(4)
READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
   SET ORDER TO (vvord)
   SELE compag
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHC  = PADL(ALLTRIM(vHC),4,'0')
IF !SEEK(ALLTRIM(vMes)+vHC)
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW EliHC FROM 1,1 TO 18,79 TITLE " Elija la HC con F10 "
   BROWSE NOEDIT WINDOW EliHC COLOR SCHEME 10 FIELDS ;
     numhc :H="HC",;
     nummes:H="Mes",;
     tipprv,;
     codprv :H="CodPro",;
     codemp :H="CodEmp",;
     x1=IIF(TIPPRV="O" AND EMPTY(codotr),NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxil', 'descri', 'descri', codprv), 20),IIF(tipprv="E",LEFT(val_fun('Auxil', 'descri', 'descri', codemp), 20),LEFT(val_fun('Auxil', 'descri', 'descri', codotr), 20)))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechc :H="Fecha",;
     x3=codcad+' '+codctc :H="Cad.ctc"

ENDIF
IF LASTKEY()#27
    SELE HOJA
	m.nummes   = nummes
	m.nummeshc = nummes
	m.numhc    = numhc
	m.import   = imptot
	m.codprv   = codprv
	m.codemp   = codemp
	m.codotr   = codotr
	m.CodCad   = codcad
	m.CodPart  = CodPart
	m.tipdoc   = tipdoc
	m.nombre   = nompre
	m.tipprv   = tipprv
	m.periodo  = periodo
	m.nompre   = nombre
	m.destino  = Destino
	m.codobra  = codobra
	m.codfte   = codfte
	m.numref   = Numhc+"."+Nummes
	m.partret  = partret

	SHOW GETS
	SET ORDER TO (vvord)
	SELECT ComPag
ENDIF
RETURN .T.

PROCEDURE IngFec
*---------------
m.periodo = RIGHT(DTOC(m.feccp),2)
RETURN .T.


PROCEDURE busCaja
*----------------
SELECT Caja
SEEK m.codctc
IF !FOUND()
	DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
ELSE
	*IF RLOCK()
		m.numcp  =  PADL(corcp+1,4,'0')
		w_ctah   =  Caja.CuentaH
	*ELSE
	*	DO STANDBY WITH "Archivo utilizado...Presione ESC y Espere un momento"
	*	RETURN .F.
	*ENDIF
ENDIF
RETURN

PROCEDURE busCaja1
*-----------------
SELECT Caja
SEEK m.codctc
IF !FOUND()
	DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
ELSE
	w_ctah   =  Caja.CuentaH
	ftew=Caja.codfte
ENDIF

RETURN


PROCEDURE busCaja2
*-----------------
PARAMETERS VSECTOR
USE CtaSec IN 0 ORDER CtaSec1 ALIAS CtaSec
SELE CtaSeC
SEEK ALLT(m.codctc)+ALLT(VSECTOR)
IF !FOUND()
	DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
ELSE
	IF RLOCK()
		m.numcp  =  PADL(corsec+1,4,'0')
	ELSE
		DO STANDBY WITH "Archivo utilizado...Presione ESC y Espere un momento"
		RETURN .F.
	ENDIF

ENDIF
RETURN


PROCEDURE valfecha
*-----------------
m.feccp = DATE()
RETURN .T.


PROCEDURE ingap   && F5
*--------------
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
IF !FOUND()
      DO agriaut
ENDIF

ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)

BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber ,vret
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
		IF RET='S'
		   vret=vret+MTOHAB
        ENDIF		   
        DO CASE
           CASE SUBS(codcta,1,3)='104' OR SUBS(codcta,1,3)='108' .OR. SUBS(CODCTA,1,2)='44'
         		mmonto=Mtohab
           CASE SUBS(codcta,1,3)='102'
           		m.flagcon='F'
           CASE SUBS(codcta,1,3)='384'
           		m.flagcon='A'
           CASE SUBS(codcta,1,3)='385'
           		m.flagcon='E'
		ENDCASE
ENDSCAN
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
IF ingreso
	IF f_appd()

		REPLACE nummes WITH m.nummes,;
		        numref WITH m.numcp,;
		        tipdoc WITH 'C/P',;
		        ret    WITH 'N',;
		        codctc WITH m.codctc,;
		        fecha  WITH m.FecCP,;
		        FecHC  WITH m.FecRef,;
		        tipctc WITH w_Tipctc,;
				codcta WITH '10101010100',;
				Mtodeb WITH mmonto,;
				Tipcta WITH 'D', periodo WITH m.periodo
	ENDIF
	IF f_appd()
	
		REPLACE nummes WITH m.nummes,;
		        numref WITH m.numcp,;
		        tipdoc WITH 'C/P',;
		        ret    WITH 'N',;
		        codctc WITH m.codctc,;
		        fecha  WITH m.FecCP,;
		        FecHC  WITH m.FecRef,;
		        tipctc WITH w_Tipctc,;
		        codcta WITH '10101010100',;
		        Mtohab WITH mmonto,;
		        Tipcta WITH 'H',;
		        periodo WITH m.periodo
	ENDIF
	DO vis_cen
ELSE
	* si es corrije solo modifica
	SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND()
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
		    IF codcta='10101010100'
	        	REPLA Mtodeb WITH mmonto, Fecha WITH m.feccp
	        	REPLA Mtohab WITH mmonto, Fecha WITH m.feccp
	        ENDIF
		ENDSCAN
    ELSE		
		IF f_appd()
		
			REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '10101010100',;
			        Mtodeb WITH mmonto,;
			        Tipcta WITH 'D',;
			        periodo WITH m.periodo
		ENDIF
		IF f_appd()
			REPLACE nummes WITH m.nummes,;
			        numref WITH m.numcp,;
			        tipdoc WITH 'C/P',;
			        ret    WITH 'N',;
			        codctc WITH m.codctc,;
			        fecha  WITH m.FecCP,;
			        FecHC  WITH m.FecRef,;
			        tipctc WITH w_Tipctc,;
			        codcta WITH '10101010100',;
			        Mtohab WITH mmonto,;
			        Tipcta WITH 'H',;
			        periodo WITH m.periodo
		ENDIF
	ENDIF	
	DO vis_cen
ENDIF	
USE IN 09 
USE IN 14
ON KEY
m.Reten = vret
m.PartRet = vvpartret
IF !EMPTY(m.reten)
	ACTIVATE WINDOW wind_12
	@0,1 SAY "Partida" GET m.partret PICTURE '!!!!!!'
    @0,30 SAY "Reten. "+STR(m.reten,10,2)
	READ
ENDIF	
DEACTIVATE WINDOW wind_12
UNLOCK
RETURN


PROCEDURE vis_cen &&Visualiza centralizaci¢n de Caja
*-----------------
ACTIVATE WINDOW wind_7
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY '10101010100'
@ 01,18  SAY mmonto PICTURE '999,999,999.99' 
@ 02,12  SAY '10101010100'
@ 02,34  SAY mmonto PICTURE '999,999,999.99' 
DO standby WITH 'Visualizando los Movimientos'
DEACTIVATE WINDOW wind_7
RETURN


FUNCTION Asig
*------------
vMonDeb=mtodeb
RETURN


PROCEDURE agrite     &&
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH m.nummes,;
	        numref WITH m.numcp,;
	        tipdoc WITH 'C/P',;
	        ret    WITH 'N',;
	        codctc WITH m.codctc,;
	        fecha  WITH m.FecCP,;
	        FecHC  WITH m.FecReF ,;
	        tipctc WITH w_tipctc,;
	        periodo WITH m.periodo
ENDIF
RETURN

PROCEDURE agriaut    &&
*----------------
DO CASE
   CASE m.tipdoc$'SE'    &&'SHSESS'
	   private a1,a2,a3,a4,b1,b4
	   STORE SPACE(3) TO a1,a4,b1,b4
	   STORE SPACE(2) TO a2,a3
	   SELE Clase
	   SEEK LEFT(ALLT(w_part),2)+RIGHT(ALLT(w_part),2)
	   *
	   w_ctad=Clase.CuentaD
	   a1=LEFT(w_ctad,3)
	   a4=RIGHT(w_ctad,3)
	   a2=LEFT(ALLT(vcodprg),2)
	   a3=SUBST(ALLT(vcodsub),2,2)
	   b1=LEFT(w_ctah,3)
	   b4=RIGHT(w_ctah,3)
	   IF a3#'06' AND a3#'10'
		   w_ctad='302'+a2+a3+a4
	       w_ctah=b1+a2+a3+b4
	   ENDIF	

   CASE m.tipdoc$'SUSS'       &&'HCRGMESUHM'
   
   		** AGREGAR ASIENTO PARA SUBSIDIOS ...............
 		w_ctad='41101010100'  		

   CASE m.tipdoc$'HCRGMEHMSHAH'       &&'HCRGMESUHM'
   
	    x1=LEFT(ALLTRIM(w_part),1)
	    x2=RIGHT(ALLTRIM(w_part),2)
	    IF x1='6'
	    	w_ctad='42401010100'   
	    ELSE
	    	SELE PARMA
	    	SEEK 'ESPGAS'+x2
	    	w_ctad=LEFT(descriaux,11)
	    	IF x2='SU'
	    		w_ctad='41901010100'
	    	ENDIF
	    ENDIF
   CASE m.tipdoc$'RERU'	     	    
		SELE ParMa
		SEEK 'CODRET'+ALLT(m.codret)
		w_ctaD=ALLT(Parma.descriaux)
   CASE m.tipdoc='SRRSAR'
	   private a1,a2,a3,a4,b1,b4
	   STORE SPACE(3) TO a1,a4,b1,b4
	   STORE SPACE(2) TO a2,a3
  	   SELE ParMa
	   SEEK 'CODRET'+ALLT(m.codret)
	   w_ctaD=allt(ParMa.descriaux)
	   a1=LEFT(w_ctad,3)
	   a4=RIGHT(w_ctad,3)
	   a2=LEFT(ALLT(vcodprg),2)
	   a3=SUBST(ALLT(vcodsub),2,2)
	   b1=LEFT(w_ctah,3)
	   b4=RIGHT(w_ctah,3)
	   IF a3#'06' AND a3#'10'
		   w_ctad='302'+a2+a3+a4
	       w_ctah=b1+a2+a3+b4
	   ENDIF	
ENDCASE		
SELECT astpat
IF f_appd()
   REPLACE nummes  WITH m.nummes,;
	       numref  WITH m.numcp,;
	       tipdoc  WITH 'C/P',;
	       ret     WITH 'N',;
	       codctc  WITH m.codctc,;
	       fecha   WITH m.FecCP,;
	       FecHC   WITH m.FecReF ,;
	       tipctc  WITH w_tipctc,;
	       periodo WITH m.periodo,;
	       tipcta  WITH 'D',;
	       Codcta  WITH w_ctad,;
	       mtodeb  WITH m.import
ENDIF
IF f_appd()
	REPLACE nummes  WITH m.nummes,;
	        numref  WITH m.numcp,;
	        tipdoc  WITH 'C/P',;
	        ret     WITH 'N',;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FecCP,;
	        FecHC   WITH m.FecReF ,;
	        Tipctc  WITH w_tipctc,;
	        periodo WITH m.periodo,;
	        tipcta  WITH 'H',;
	        Codcta  WITH w_ctaH,;
	        mtohab  WITH m.import
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
SELECT ComPag
RETURN

PROCEDURE Compre
*---------------
private as
AS=ALIAS()
m.valdeb = vtotal-m.reten
m.valhab = vtotal-m.reten

m.CtaDeb = IIF(m.Codfte='00','90301010101','90301010102')
m.CtaHab = IIF(m.Codfte='00','90401010101','90401010102')

IF m.tipdoc$'SE'
   private a1,a2,a3,a4,b1,b4
   STORE SPACE(3) TO a1,a4,b1,b4
   STORE SPACE(2) TO a2,a3
   a1=LEFT(m.ctadeb,3)
   a4=RIGHT(m.ctadeb,3)
   a2=LEFT(ALLT(vcodprg),2)
   a3=SUBST(ALLT(vcodsub),2,2)
   b1=LEFT(m.ctahab,3)
   b4=RIGHT(m.ctahab,3)
   IF a3#'06' AND a3#'10'
	   m.ctadeb=a1+a2+a3+a4
       m.ctahab=b1+a2+a3+b4
   ENDIF	
ENDIF   
USE astpre   IN 13  ORDER TAG astpre2      ALIAS astpre  
SELECT astpre
SEEK 'D'+ALLTRIM(m.nummes)+ALLTRIM(m.numcp)+ALLTRIM(M.CTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctadeb,;
		        ctadeb  WITH m.ctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
	    	    valdeb WITH m.valdeb ,;
		        ctadeb WITH m.ctadeb ,;
		        ctahab WITH SPACE(11),;
	        	valhab WITH 0 
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctadeb,;
		        ctadeb  WITH m.ctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
	    	    valdeb WITH m.valdeb ,;
		        ctahab WITH SPACE(11),;
	        	valhab WITH 0 
	ENDIF
	UNLOCK
ENDIF
	
SEEK 'H'+ALLTRIM(m.nummes)+ALLTRIM(m.numcp)+ALLTRIM(M.CTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctahab,;
		        ctahab  WITH m.ctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc,;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH m.valhab,;
		        ctadeb WITH SPACE(11)
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH m.ctahab,;
		        ctahab  WITH m.ctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecCP,;
		        CodPart WITH m.CodPart,;
		        CodCtc  WITH m.CodCtc,;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH m.valhab,;
		        ctadeb WITH SPACE(11)
	ENDIF
	UNLOCK
ENDIF	
USE IN 13
ACTIVATE WINDOW wind_6
@ 01,04  SAY m.ctadeb PICTURE '!!!!!!!!!!!' 
@ 01,18  SAY m.valdeb PICTURE '999,999,999.99' 
@ 02,12  SAY m.ctahab PICTURE '!!!!!!!!!!!' 
@ 02,34  SAY m.valhab PICTURE '999,999,999.99' 
WAIT ' '
DEACTIVATE WINDOW wind_6
vCtadeb = m.ctadeb
vValdeb = m.Valdeb
vCtahab = m.ctahab
vValhab = m.valhab
SELE (as)
RETURN


PROCEDURE Trabaja_Hijo
*---------------------
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteCp
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
*- Mira si tiene HC
IF !FOUND() AND !EMPTY(m.numhc) 
	SELECT IteHc
	SEEK ALLTRIM(m.nummeshc)+m.numhc
	SCAN WHILE nummes=ALLTRIM(m.nummeshc) AND numhc=m.numhc AND estado<>'99'
		if codfte='00'
			VPART=SUBSTR(ITEHC.CODPART,2,1)
			USE TOTAUT   IN 2  ORDER tag TOTAUT1      ALIAS TOTAUT
			sele totaut
			seek m.periodo+m.nummeshc+m.codctc+VPART
			if !found()
				DO standby WITH vmens10
				* mensaje no existe aut.giro
				USE IN 2
				giro=.T.
				SELE COMPAG
				RETURN .F.
			else
				if saldo < itehc.valpart and itehc.tipope<>'-'
					DO standby WITH vmens11
					*mensaje saldo es menor
					USE IN 2
					GIRO=.T.
					sele compag
					return .f.
				endif
				IF RLOCK()
					REPLACE ejecut with iif(itehc.tipope='-',ejecut+itehc.valpart*-1,ejecut+itehc.valpart)
					replace saldo with asigna-ejecut
				ENDIF
				UNLOCK
			endif
			GIRO=.F.
			USE IN 2
		endif			
		SELECT IteCp
		DO CASE
		   CASE  IteHc.estado='92' AND !EMPTY(IteHc.NumHm) AND IteHc.NummesHm=ALLTRIM(m.nummes)
				IF f_appd()
				   REPLACE nummes   WITH ALLTRIM(m.nummes),;
			           numcp    WITH m.numcp, ;
	    		   	   CodPart  WITH IteHc.CodPart,;
	    		   	   codctc   WITH m.codctc ,;
    				   periodo  WITH m.periodo ,;
    				   impparc  WITH IIF(IteHc.Tipope='-',IteHc.valpart*-1,IteHc.valpart),;
    				   codcom   WITH IteHc.codcom ,;
    				   codmet   WITH IteHc.codmet ,;    				       				   
	    			   codfte   WITH m.codfte,;
	    			   tipdoc   WITH m.tipdoc,;
    				   estado   WITH IIF(IteHc.Tipope='-','90','00'),;
					   codcad   WITH m.codcad
					    	
				ENDIF
				UNLOCK
				m.nummeshm=Itehc.nummeshm
				m.numhm   =itehc.numhm
		   CASE  IteHc.estado='94' 
				IF f_appd()
				   REPLACE nummes   WITH ALLTRIM(m.nummes),;
			           numcp    WITH m.numcp, ;
	    		   	   CodPart  WITH IteHc.CodPart,;
	    		   	   codctc   WITH m.codctc ,;
    			   	   periodo  WITH m.periodo ,;
    				   impparc  WITH IteHc.valpart*-1,;
	    			   codfte   WITH m.codfte,;
    				   codcom   WITH IteHc.codcom ,;
    				   codmet   WITH IteHc.codmet ,;    				       				   
	    			   tipdoc   WITH m.tipdoc,;
    				   estado   WITH '90',;
    				   codcad   WITH m.codcad
    				   
				ENDIF
				UNLOCK
				&&Grabar Parte de Rebaja
		   CASE  IteHc.estado='00'
				IF f_appd()
				   REPLACE nummes   WITH ALLTRIM(m.nummes),;
			           numcp    WITH m.numcp, ;
	    		   	   CodPart  WITH IteHc.CodPart,;
	    		   	   codctc   WITH m.codctc ,;
    				   periodo  WITH m.periodo ,;
    				   impparc  WITH IIF(!EMPTY(IteHc.NumPa) AND IteHc.mesPa=ALLT(m.nummes),IteHc.valpart*-1,IteHc.valpart),;
    				   codcom   WITH IteHc.codcom ,;
    				   codmet   WITH IteHc.codmet ,;    				       				   
	    			   codfte   WITH m.codfte,;
	    			   tipdoc   WITH m.tipdoc,;
    				   estado   WITH IIF(!EMPTY(IteHc.NumPa) AND IteHc.mesPa=ALLT(m.nummes),'90','00'),;
    				   codcad   WITH m.codcad    				   

					ENDIF
				UNLOCK
		ENDCASE		
	ENDSCAN
	SELECT IteCp
	vvpartret = codpart
ENDIF
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
        CodCom     :H= 'Componente':R,;
        CodMet     :H= 'Meta' :R,;
		CodPart    :H= 'Partida'  :R,;
		aa = IIF(EMPTY(CodPart),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':40 :R,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' :R		

GO TOP
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)

w_part=IteCp.CodPart

SCAN WHILE nummes=ALLTRIM(m.nummes) and numcp = m.numcp and codctc= ALLTRIM(m.codctc)
	IF EMPTY(IteCp.codpart) OR IteCp.impparc = 0.00
		DELETE NEXT 1
	ELSE
		IF RLOCK()
		   REPLACE codfte with m.codfte, codcad WITH m.CodCad
		ENDIF
		UNLOCK
	ENDIF
ENDSCAN
SELECT ComPag
UNLOCK ALL
ON KEY LABEL F2  
ON KEY LABEL F10 
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT ComPag
RETURN


PROCEDURE traba_hijo
*-------------------
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL f5  DO agreg_item
ON KEY LABEL f8  DO elimi_item
ON KEY LABEL f10 KEYBOARD CHR(23)
SELE IteCp
SEEK ALLTRIM(m.nummes)+m.numcp + ALLTRIM(m.codctc)
*- Mira
IF !FOUND()
	DO agreg_item
	IF m.TipDoc='RE' .OR. m.TipDoc='RU' .OR. m.TipDoc='SR' .OR. m.TipDoc='RS' .OR. m.TipDoc='AR'
		REPLACE codcom WITH cccomp,codmet WITH ccmet
	ENDIF
ENDIF

DO CASE
	CASE newsistem='1'
		IF m.IncPres='S' .AND. !EMPTY(m.codcad)
			BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
	        CodCom     :H= 'Componente':V=Val_comp(m.periodo+'01001'+m.codcad,codcom,'codcom'):F ,;
	        CodMet     :H= 'Meta' :V=Val_meta(m.periodo+'01001'+m.codcad,codcom+codmet,'codmet'):F ,;
			CodPart    :H= 'Partida' :V=val_part() :F :P='9999!!' ,;
			aa = IIF(EMPTY(CodPart).AND. !val_part(),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':40 :R,;
			impparc    :H= 'Monto'   :F :p='9,999,999.99' 		
		ELSE
			BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
			CodPart    :H= 'Partida' :V=val_part() :F :P='9999!!' ,;
			aa = IIF(EMPTY(CodPart).AND. !val_part(),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':40 :R,;
			impparc    :H= 'Monto'   :F :p='9,999,999.99' 		
		ENDIF

	CASE newsistem='2'
		IF m.IncPres='S' .AND. !EMPTY(m.codcad)
			BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
	        CodCom     :H= 'Componente':V=Val_com1(m.periodo+m.codcad,codcom,'codcom'):F ,;
	        CodMet     :H= 'Meta' :V=Val_met1(m.periodo+m.codcad,codcom+codmet,'codmet'):F ,;
			CodPart    :H= 'Partida' :V=val_part() :F :P='9999!!' ,;
			aa = IIF(EMPTY(CodPart).AND. !val_part(),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':40 :R,;
			impparc    :H= 'Monto'   :F :p='9,999,999.99' 		
		ELSE
			BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
			CodPart    :H= 'Partida' :V=val_part() :F :P='9999!!' ,;
			aa = IIF(EMPTY(CodPart).AND. !val_part(),' ',val_para(right(codpart,2),'ESPGAS','D',28,30)) :H='Descripci¢n':40 :R,;
			impparc    :H= 'Monto'   :F :p='9,999,999.99' 		
		ENDIF
	ENDCASE	
		

GO TOP
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)

w_part=IteCp.CodPart

SCAN WHILE nummes=ALLTRIM(m.nummes) and numcp = m.numcp and codctc= ALLTRIM(m.codctc)
		IF RLOCK()
		    REPLACE codfte WITH m.codfte, codcad WITH m.CodCad
			UNLOCK
		ENDIF
ENDSCAN
SELECT ComPag
UNLOCK ALL
ON KEY LABEL f2  
ON KEY LABEL f5  
ON KEY LABEL f8  
ON KEY LABEL f10 
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT ComPag
RETURN


PROCEDURE Tra_Hijo  &&H/M
*---------------------
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteCp
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
*- Mira si tiene HC
IF !FOUND() AND !EMPTY(M.numhc) AND dehm
	SELECT IteHc
	SET ORDER TO IteHc4
	SEEK ALLTRIM(m.nummeshc)+m.numhc
	SCAN WHILE nummes=ALLTRIM(m.nummeshc) AND numhc=m.numhc 
	    IF estado<>'99' AND tipope='+'
			SELECT IteCp
			IF f_appd()
			   REPLACE nummes   WITH ALLTRIM(m.nummes),;
		           numcp    WITH m.numcp, ;
	    	   	   CodPart  WITH IteHc.CodPart,;
	    	   	   codctc   WITH m.codctc ,;
    		   	   codanal  WITH IteHc.codanal,;
    			   periodo  WITH m.periodo ,;
    			   impparc  WITH IteHc.valpart,;
    			   tipfun   WITH m.tipfun,;
	    		   codfte   WITH m.codfte,;
	    		   tipdoc   WITH m.tipdoc,;
    			   estado   WITH IIF(IteHc.Tipope='-','90','00')
			ENDIF
			UNLOCK
		ENDIF	
	ENDSCAN
	SELECT IteCp
ENDIF
SELE IteHc
SET ORDER TO IteHc1
SELE iteCp
IF ALLTRIM(m.tipfun)='F'
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
		codanal    :H= 'Partida' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal) :R,;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) :R,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' :R
ELSE
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
		CodPart    :H= 'Partida' :v=val_part(SUBSTR(CodPart,4,2),LEFT(CodPart,2),'CodPart'):F :W=EMPTY(CodPart) :R,;
		codanal    :H= 'Analitc' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal) :R,;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) :R,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' :R
ENDIF
GO TOP
SEEK ALLTRIM(m.NumMes)+m.NumCp + ALLTRIM(m.CodCtc)
w_part=IIF(m.tipfun='I',IteCp.CodPart,IteCp.Codanal)
SCAN WHILE nummes=ALLTRIM(m.nummes) and numcp = m.numcp and codctc= ALLTRIM(m.codctc)
	IF EMPTY(IteCp.codanal) OR IteCp.impparc = 0.00
		DELETE NEXT 1
	ELSE
		IF RLOCK()
		   REPLACE tipfun with m.tipfun,codfte with m.codfte, codcal WITH m.CodCal
		ENDIF
		UNLOCK
	ENDIF
ENDSCAN
SELECT ComPag
UNLOCK ALL
ON KEY LABEL F2  
ON KEY LABEL F10 
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1,vtempo
SHOW MENU mmenu
SELECT ComPag
RETURN



PROCEDURE agreg_item
*-------------------
SELECT IteCp
IF f_appd()
	REPLACE nummes WITH m.nummes, numcp WITH m.numcp,impparc WITH 0,codctc WITH m.codctc,codcad WITH m.codcad;
			periodo with m.periodo ,codfte with m.codfte,tipdoc with m.tipdoc, ESTADO WITH '00'
	DO CASE						&& 'MESUSS'
	  CASE tipdoc$'SUSS'     
            REPLACE CodPart WITH '0000SU'
   	  CASE TIPDOC$'RESRRURSAR'
            REPLACE CodPart WITH '0000RE',impparc WITH m.import
    ENDCASE 	
ENDIF
SELECT IteCp
RETURN .T.


PROCEDURE elimi_item
*-------------------
SELECT IteCp
IF RLOCK()
	DELETE NEXT 1
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
RETURN


PROCEDURE TOTAL_PADRE
*--------------------
SELECT IteCp
SEEK ALLTRIM(m.nummes) + m.numcp + ALLTRIM(m.codctc)
SCAN WHILE nummes = ALLTRIM(m.nummes) .AND. numcp = m.numcp .AND. codctc = ALLTRIM(M.codctc)
     vtotal = vtotal + impparc
ENDSCAN
RETURN VTOTAL 


PROCEDURE anula
*---------------
SELECT ComPag
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
	REPLACE usuario WITH wuser_id
	DO CASE
		CASE m.tipdoc$'RGHCSHAH'
			SELECT Hoja
			SET ORDER TO Hojcon1
			IF SEEK(m.NumMesHC+m.NumHc)
				 IF RLOCK() .OR. F_LOCK(1)
				    REPLACE Hoja.Nummescp with '  ',Hoja.NumCP WITH '    ',HOJA.ESTADO WITH IIF(m.tipdoc='SH' OR m.tipdoc='AH','00','20' )
				    WAIT "H/C Liberada" window
				 ENDIF  
				UNLOCK
			ENDIF
			*
		CASE m.tipdoc='HM'
			USE hojmod   IN 20  ORDER TAG hojmod1      ALIAS hojm   &&H/M
			SELECT Hojm
			IF SEEK(m.NumMesHm+m.NumHm)
				 IF RLOCK() .OR. F_LOCK(1)
				    REPLACE Hojm.Nummescp with '  ',HojM.NumCP WITH '    ',HOJM.ESTADO WITH '20' 
				    WAIT "H/M Liberada" window
				 ENDIF  
				UNLOCK
			ENDIF
		CASE m.tipdoc='RE'	
		  	USE reten  IN 12  ORDER TAG reten1  ALIAS reten
			SELECT reten
			SEEK m.nummes+m.numcp+m.codctc    
   			IF RLOCK() .OR. F_LOCK(1)
				REPLACE conpago  WITH ' '
				REPLACE fecpag   WITH {  /  /  }
				REPLACE numcppg  WITH ' '
				REPLACE mescppg  WITH ' '
   	     	    WAIT "Retenciones Liberadas" window
			ENDIF
			
			USE IN 20
	ENDCASE
	SELE IteCp
    SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
    IF FOUND()
    	SCAN while nummes=ALLTRIM(m.nummes) and numCP=m.numcp and codctc=m.codctc 
		    IF RLOCK() .OR. F_LOCK(1)
				REPLACE estado WITH '99'
			ENDIF
 			if codfte='00'
 				VMONTO=ITECP.IMPPARC
				VPART=SUBSTR(ITEHC.CODPART,2,1)
				USE TOTAUT   IN 2  ORDER tag TOTAUT1      ALIAS TOTAUT
				sele totaut
				seek m.periodo+m.nummeshc+m.codctc+VPART
				if found()
					IF RLOCK()
						REPLACE ejecut with EJECUT-VMONTO
						replace saldo with asigna-ejecut
					ENDIF
					UNLOCK
				endif
				USE IN 2
				SELE ITECP			
			ENDIF    
		ENDSCAN
	ENDIF					
	*
    USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
    sele astpat
    SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
    IF FOUND()
    	SCAN WHILE nummes=ALLTRIM(m.nummes) AND numref=m.numcp AND codctc=m.codctc AND tipdoc="C/P" 
		    IF RLOCK() .OR. F_LOCK(1)
    		   DELETE NEXT 1
	   		ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF	    
	SELECT Cheque
	SEEK m.nummes+m.numcp+m.codctc    
    IF FOUND()
        vncheque=cheque.numchq
		SCAN WHILE nummes=ALLTRIM(m.nummes) AND numcp=m.numcp AND codctc=m.codctc 
		    IF RLOCK() .OR. F_LOCK(1)
				REPLACE estado WITH '99'
	   		ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
  	USE reten  IN 12  ORDER TAG reten1  ALIAS reten
	SELECT reten
	SEEK m.nummes+m.numcp+m.codctc    
   	IF FOUND()
		SCAN WHILE nummes=ALLTRIM(m.nummes) AND numcp=m.numcp AND codctc=m.codctc 
    	    IF !m.Tipdoc$'RESRRURSAR'
		    	IF RLOCK() .OR. F_LOCK(1)
    			   DELETE NEXT 1
	   			ENDIF       
		    	UNLOCK
		    ELSE 
				REPLACE conpago  WITH ' '
				REPLACE fecpag   WITH {  /  /  }
				REPLACE numcppg  WITH ' '
				REPLACE mescppg  WITH ' '
		    ENDIF
		ENDSCAN
   	ENDIF
    USE IN 09
    USE IN 12
    
   	USE Hchqpen  IN 0  ORDER TAG hchqpen2  ALIAS hchqpen
	SELECT hchqpen
	SEEK m.codctc+ALLT(vncheque)    
   	IF FOUND()
		SCAN WHILE nummes=ALLTRIM(m.nummes) AND numcp=m.numcp AND codctc=m.codctc 
	    	IF RLOCK() .OR. F_LOCK(1)
   			   REPLACE estado WITH '99'
   			ENDIF       
	    	UNLOCK
		ENDSCAN
   	ENDIF
    USE IN 0 
    
    USE astpre   IN 13  ORDER TAG astpre7      ALIAS astpre  
    sele astpRE
    SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
    IF FOUND()
    	SCAN while nummes=ALLTRIM(m.nummes) and numref=m.numcp and codctc=m.codctc and tipdoc="C/P" 
		    IF RLOCK() .OR. F_LOCK(1)
		       DELETE NEXT 1
		    ENDIF       
		    UNLOCK
	    ENDSCAN
    ENDIF
    USE IN 13
	SELECT ComPag
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
USE IN 12
USE Reten    IN 12  ORDER tag Reten1      ALIAS Reten          
SELECT ComPag
*SELECT Reten
VRECNO=RECNO()
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW lis FROM 3,10 TO 23,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vlista,vesta,Vsino
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli,Vinicp,vfincp
STORE SPACE(212) TO vobs
vcta=m.codctc
Vano=m.nummes
Vcli=m.numcp
STORE DATE() TO vfecini, vfecfin
IF newsistem='1'
	@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte'
ELSE
	@ 01,01 SAY "     Tipo Listado : " GET vlista  FUNCTION '^ Documento;Resumen x Cta.Cte;Resumen x Meta;Resumen Retenc. x Meta'
ENDIF
@ 05,01 SAY "     N§ Documento : "
@ 05,22 GET vano    WHEN vlista=1  PICTURE '!!'
@ 05,25 GET vcli    WHEN vlista=1  PICTURE '!!!!' VALID val_CP()
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta    VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,08,07)
@ 11,01 SAY "          Destino :  " GET vsino   FUNCTION '*RNH Presidencia;Cajero/Pagador' WHEN vlista=2 .OR. vlista=3 OR vlista=4
@ 13,01 SAY " Fecha / Correlat.: "
@ 13,22 GET vinicp   PICTURE '!!!!' COLOR SCHEME 10 WHEN vsino=2
@ 13,32 GET vfincp   PICTURE '!!!!' COLOR SCHEME 10 WHEN vsino=2
@ 13,22 GET vfecini  PICTURE '@D'   COLOR SCHEME 7 WHEN (vlista=2 AND vsino=1) OR (vlista=3 AND vsino=1) OR (vlista=4 AND vsino=1)
@ 13,32 GET vfecfin  PICTURE '@D'   COLOR SCHEME 7 VALID (vfecfin >= vfecini)  WHEN (vlista=2 AND vsino=1) OR (vlista=3 AND vsino=1) OR (vlista=4 AND vsino=1)
@ 15,01 SAY "    Observaciones : "  GET vobs     PICTURE '@S28' WHEN vlista=2 .OR. vlista=3  OR vlista=4 FUNCTION "!"
@ 17,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
xind=SYS(3)+".IDX"

DEFINE WINDOW Xwait FROM 21,35 TO 23,75 COLOR SCHEME 5 
	DO CASE 
	   CASE vLista = 1
		    vkey=ALLTRIM(VANO)+VCLI+ALLTRIM(VCTA)
    		SELE ComPag
    		SEEK vkey
    		DO repPRG WITH "LisCP"," Listado Comprobante de Pago ",2
	   CASE vLista = 2
 		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
		    SELE ComPag
			SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
			IF vsino=1
				INDEX ON numcp TO (xind) FOR codctc=vcta AND BETWEEN(ComPag.fecCP,vfecini,vfecfin) AND (BETWEEN(LEFT(COMPAG.NUMCP,1),'0','9') or SUBS(compag.numcp,1,1)="B")
			ELSE	
				INDEX ON numcp TO (xind) FOR codctc=vcta AND BETWEEN(ComPag.numcp,vinicp,vfincp)  AND (BETWEEN(LEFT(COMPAG.NUMCP,1),'0','9') or SUBS(compag.numcp,1,1)="B")
			ENDIF	
			DEACTIVATE WINDOW xwait
            DO reporte WITH 2,"LisCpCta"," Resumen de Comprobantes de Pago Registradas por Cta.Cte. ",2,.F.,.T.                                  	       
	        SELE ComPag
	        SET RELA OFF INTO Cheque
	        SET INDEX TO
   	        SELE ComPag
   	        SET ORDER TO ComPag1
	   CASE vLista = 3
 		    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
		    SELE ComPag
		    SET RELATION TO nummes+numcp+codctc  INTO Cheque ADDI
			IF vsino=1
				INDEX ON codcad+numcp TO (xind) FOR codctc=vcta AND BETWEEN(ComPag.fecref,vfecini,vfecfin) 
			ELSE	
				INDEX ON codcad+numcp TO (xind) FOR codctc=vcta AND BETWEEN(ComPag.numcp,vinicp,vfincp) 
			ENDIF	
			DEACTIVATE WINDOW xwait
            DO reporte WITH 2,"LisCpMe2"," Resumen de Comprobantes de Pago Registradas por meta ",2,.F.,.T. 
	        SELE ComPag
	        SET RELA OFF INTO Cheque
	        SET INDEX TO
   	        SELE ComPag
   	        SET ORDER TO ComPag1
	  CASE vlista = 4
	  	    SELECT Reten
			VRECNO=RECNO()
			IF EOF()
				DO standby WITH vmens08
			RETURN
			ENDIF
			vtemp = RECNO()

	  	    ACTIVATE WINDOW Xwait
		    @0,0 SAY " Procesando reporte...." COLOR W+/BR*
		    SELE Reten
			*SET RELATION TO nummes+numcp+codctc  INTO RETEN ADDI
			IF vsino=1
				INDEX ON codcad+codret TO (xind) ASCENDING FOR codctc=vcta AND BETWEEN(reten.fecret,vfecini,vfecfin) 
			ELSE	
				INDEX ON codcad+codret TO (xind) ASCENDING FOR codctc=vcta AND BETWEEN(reten.numcp,vinicp,vfincp) 
			ENDIF	
			DEACTIVATE WINDOW xwait
            DO reporte WITH 2,"LisCpMe4"," Resumen de Retenciones por meta ",2,.F.,.T. 
  	        SELE Reten
*           SET RELA OFF INTO Cheque
	        SET INDEX TO
   	        SELE ComPag
   	        SET ORDER TO ComPag1
        
    ENDCASE
	SELE ComPag
ENDIF
USE IN 9
USE IN 14
USE HojCon   IN  6  ORDER TAG HojCon1      ALIAS hoja   &&H/C
USE IteHc    IN  7  ORDER TAG IteHc1       ALIAS IteHc 
USE Clase    IN  8  ORDER TAG Clase1       ALIAS Clase   
SELECT ComPag
GO VRECNO
DO VISTA
RETURN


PROCEDURE SUMA
*=============
PARAMETERS cad,cod
SUMA =0
sele reten
VREG=RECNO()
*vorde = ORDER()
*SET ORDER TO TAG CADRET
set filter TO codcad=cad and codret=cod
GO TOP
DO WHILE !EOF()
	SUMA=SUMA+RETEN.VALRET
	SKIP
ENDDO
SET FILTER TO
SELE RETEN
GO VREG
*SET ORDER TO TAG vorde
RETURN SUMA	

PROCEDURE val_cp             && Revisi¢n de BD en browse
*---------------
SELE ComPag
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
SELE ComPag
RETURN


PROCEDURE LisCP  &&Programa Reporte usa repprg 
*--------------
PARAMETERS xcop
PRIVATE fila,xval,tval,vpar,lval,x_sw1,vref
use in 8
STORE 0 TO X_sw1,xval,tval,fila
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos

USE reten    IN 12  ORDER TAG reten5  ALIAS reten
SELE ComPag
vcad=codcad
vper=periodo
vano=ALLTRIM(nummes)
vcli=numcp
vcta=codctc
Vkey=ComPag.Nummes+ComPag.Numcp+ComPag.Codctc
Vref=ComPag.refer
LVAL=0
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,68  SAY "PAG:"
@1,76  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisCp"
@2,68 SAY "FECHA:"
@2,76 SAY DATE()           
IF TIPDOC#'IN'
	@5,14 SAY CHR(14)
	@5,15 SAY "<< COMPROBANTE DE PAGO >>"
	@5,44 SAY CHR(27)+CHR(18)
	@07,03 SAY "      N§ C/P :"  
	@07,20 SAY NUMCP+'.'+NUMMES
	@07,68 SAY "Estado :"  
	@07,76 SAY IIF(Estado='00','Emitido',IIF(Estado='20','Girado',IIF(Estado='99','Anulado',IIF(Estado='50','Pagado',IIF(Estado='92','Inutilizado',IIF(Estado="10","Girado",' -  '))))))
	@08,03 SAY "   Fecha C/P :"  
	@08,20 SAY FECCP picture "@D"
	@09,03 SAY "         Son :"  
	@09,20 SAY LETRAS(IMPORT-RETEN,'SOLES')
	@10,03 SAY "Raz¢n Social :"  
	@10,20 SAY IIF(tipdoc$"RESRRURSAR",codret,IIF((TIPPRV='O' OR EMPTY(TIPPRV) OR TIPDOC='RG' OR tipdoc='HM')," ",IIF((TIPPRV='P'and tipdoc<>'RG' AND tipdoc<>'HM'),Codprv,IIF(TIPDOC<>'RG' AND tipdoc<>'HM',Codemp,' '))))
	@10,27 SAY IIF(tipdoc$"RESRRURSAR",VAL_PARA(Codret,'CODRET','D',28,40),;
	       IIF((tipprv='O' AND EMPTY(codotr) OR EMPTY(tipprv) OR tipdoc='RG' OR tipdoc='HM'),ComPag.nompre,;
	       IIF((tipprv='P' AND Tipdoc<>'RG' AND tipdoc<>'HM'),val_auxi(ALLT(codprv),'20','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='E', VAL_AUXI(allt(Codemp),'30','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='I', VAL_AUXI(allt(Codpre),'80','D',40),;
	       IIF(tipdoc<>'RG' AND tipdoc<>'HM' AND tipprv='O', VAL_AUXI(allt(Codotr),'09','D',40),;
   	       IIF(tipprv='R' AND !EMPTY(codotr) AND tipdoc='ME',VAL_PARA(ALLTRIM(Codotr),'CODRET','D',28,40),' ')))))))
	@11,03 SAY "  Referencia :"  
	@11,20 SAY docref
	@11,24 SAY numref+' '+refer
	@12,03 SAY "     Cta.Cte.:"  
	@12,20 SAY codctc
	@12,35 SAY val_fun('Caja','codctc','Descri',codctc)
	@13,03 SAY "     Fte.Fto.:"
	@13,20 SAY codfte
	*@13,25 SAY ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))+' - '+ALLTRIM(val_para(tipfun,'TIPFUN','D',26,50))
	@13,25 SAY ALLTRIM(val_para(CODFTE,'CODFTE','D',26,50))
	IF incpres='S'
		IF newsistem='1'
			=val_codcad(ALLTRIM(vcad),vper+'01001','C')
		ELSE
			=val_codca1(ALLTRIM(m.codcad),vper,'C')
		ENDIF
		@15,04 SAY "Cadena Func.:   UG  UE  FN PRG SBPRG ACTPRY"
		@16,20 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
		IF newsistem='2'
			@17,04 SAY "        Meta: "
			SELE IteCp
			SEEK  vkey
			@17,20 SAY IteCp.CodMet +' '+ Val_met1(vper+m.codcad,IteCp.codcom+IteCp.codmet,'D')
			SELE ComPag
		ENDIF
		
	ENDIF
	@18,08 SAY IIF(EMPTY(DESTINO),' ','Destino :')
	@18,19 SAY CHR(15)
	@18,22 SAY DESTINO
	@19,31 SAY CHR(18)
	@19,40 SAY "C O N C E P T O" 
	@20,01 SAY CHR(15)
	@20,03 SAY SUBSTR(GLOSA,1,130)
	@21,03 SAY SUBSTR(GLOSA,131,250)
	@22,32 SAY CHR(18)
	@22,40 SAY "OBSERVACIONES" 
	@23,01 SAY CHR(15)
	@23,03 SAY SUBSTR(OBSERV,1,130)
	@23,24 SAY CHR(18)
	@24,25 SAY "<< CONTABILIDAD PRESUPUESTAL >>"
	@25,10 SAY "Cuentas"
	@25,52 SAY "Debe"
	@25,72 SAY "Haber"
	@26,01 SAY CTADEB
	@26,13 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,26)',substr(CtaDeb,1,3))
	@26,40 SAY VALDEB PICTURE '999,999,999,999.99'
	@27,01 SAY CTAHAB
	@27,13 SAY Val_Fun('Cuenta','Cuenta','SUBSTR(Descri,1,26)',substr(CtaHAB,1,3))
	@27,60 SAY VALHAB PICTURE '999,999,999,999.99'
	@29,20 SAY "<< ESTADISTICA DIARIA OBJETO DEL GASTO >>"
	@30,03 SAY "     Componente      Meta      Partida            Parcial               Total"
	SELE IteCp
	SEEK  vkey
	@31,06 SAY '    '+IteCp.CodCom+'         '+IteCp.CodMet   &&+'        '+ALLTRIM(IteCp.CodPart)
	@31,34 SAY ALLTRIM(IteCp.CodPart)
	FILA=31
		SCAN WHILE NUMMES=ALLTRIM(VANO) AND NUMCP=VCLI AND CODCTC=ALLTRIM(VCTA)
    
    
    	 IF fila>=32
        	IF IteCp.codpart <> vPar
           	   FILA=FILA-1 
		       @fila,62 SAY xval PICTURE '@Z 999,999,999,999.99'
		       FILA=FILA+1
	    	   xval=0
	        ENDIF
	     ENDIF           
	    IF fila>=32 
			@fila,06 SAY '    '+IteCp.CodCom+'         '+IteCp.CodMet   &&+'        '+ALLTRIM(IteCp.CodPart)
			@fila,34 SAY ALLTRIM(IteCp.CodPart)
		ENDIF
	     @FILA,40 SAY IteCp.IMPPARC PICTURE '@Z 999,999,999,999.99'	
	     vpar=IteCp.codpart
	     xval=xval+IteCp.impparc
	     tval=tval+IteCp.impparc
	     FILA=FILA+1
	ENDSCAN     
	SELE ComPag
	FILA=FILA-1
	@fila,62 SAY xval PICTURE '@Z 999,999,999,999.99'
	FILA=FILA+2
	@FILA,40 SAY "      TOTAL :"    
	@fila,62 SAY Tval PICTURE '999,999,999,999.99'
	FILA=FILA+1	
	@FILA,40 SAY "DEDUCCIONES :"    
	@FILA,62 SAY Reten PICTURE '999,999,999,999.99'
	FILA=FILA+1
	@FILA,40 SAY "    LIQUIDO :"    
	@FILA,62 SAY IMPORT-reten PICTURE '999,999,999,999.99'
	FILA=FILA+2
	&&&
	IF FILA >=52
		FILA =3
	ENDIF
	@FILA,25 SAY "<< CONTABILIDAD PATRIMONIAL >>"
	FILA=FILA+1
	@FILA,08 SAY "Cuenta D"
	@FILA,32 SAY "Importe"
	@FILA,46 SAY "Cuenta H"
	@FILA,72 SAY "Importe"
	FILA=FILA+1
	SELE ASTPAT
	SEEK vkey
	SCAN WHILE ASTPAT.NUMMES=ALLTRIM(VANO) AND ASTPAT.NUMREF=VCLI AND ASTPAT.CODCTC=ALLTRIM(VCTA)  
	IF ASTPAT.RET='N'	
		IF ASTPAT.TIPCTA="D" 
		   @FILA,06 SAY ASTPAT.CODCTA
	       @FILA,20 SAY IIF(LEFT(ALLT(ASTPAT.CODCTA),2)='10',ASTPAT.MTODEB,ASTPAT.MTODEB-ComPag.RETEN) PICTURE '999,999,999,999.99'
	   	ELSE
	   	   @FILA,44 SAY ASTPAT.CODCTA
	   	   @FILA,60 SAY ASTPAT.MTOHAB PICTURE '999,999,999,999.99'
	   ENDIF
	   FILA=FILA+1
   ENDIF	   
   ENDSCAN

	FILA=FILA+1
&&&
	IF FILA >=52
		FILA =3
	ENDIF

	@FILA,3 SAY "Ú"
	@FILA,4 SAY REPLICATE("Ä",30)
	@FILA,34 SAY "¿" 
	@FILA,37 SAY "Ú"
	@FILA,38 SAY REPLICATE("Ä",40)
	@FILA,78 SAY "¿" 

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,10 SAY " RECIBI CONFORME"
	@FILA,34 SAY "³"
	@FILA,37 SAY "³"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,34 SAY "³"
	
	@FILA,37 SAY "³"
	@FILA,38 SAY " -----------------"
	@FILA,59 SAY " -----------------"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,34 SAY "³"

	@FILA,37 SAY "³"
	@FILA,39 SAY "CONTROL INTERNO "
	@FILA,61 SAY "CAJERO GENERAL"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,6 SAY " ------------------------"
	@FILA,34 SAY "³"

	@FILA,37 SAY "³"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,34 SAY "³"
	
	@FILA,37 SAY "³"
	@FILA,78 SAY "³"

	FILA=FILA+1
    
	@FILA,3 SAY "³"
	@FILA,4 SAY " L.E. :"
	@FILA,34 SAY "³"
	
	@FILA,37 SAY "³"
	@FILA,38 SAY " -----------------"
	@FILA,59 SAY " -----------------"
	@FILA,78 SAY "³"


	FILA=FILA+1

	@FILA,3 SAY "³"
	@FILA,4 SAY " FECHA :"
	@FILA,12  SAY "  /  /"         
	@FILA,34 SAY "³"

	@FILA,37 SAY "³"
	@FILA,39 SAY "VISACION CONTADOR"
	@FILA,61 SAY "AUTORIZACION"
	@FILA,78 SAY "³"

	FILA=FILA+1

	@FILA,3 SAY "À"
	@FILA,4 SAY REPLICATE("Ä",30) 
	@FILA,34 SAY "Ù"

	@FILA,37 SAY "À"
	@FILA,38 SAY REPLICATE("Ä",40)
	@FILA,78 SAY "Ù" 

	FILA=FILA+1
		
	@FILA,12 SAY "N§ Cheque :"     
	SELE Cheque
	SEEK vkey
	@FILA,24 SAY iif(empty(Cheque.numchq),' Sin Cheque',Cheque.numchq)
	SELE ComPag
	@FILA,40 SAY "LOGIN:"
	@FILA,50 SAY disp_usu()
    @fila+1,03 say "N O T A:"
    @fila+1,12 say "SR.USUARIO SU CHEQUE"
    @fila+1,40 SAY "Fecha:"
    @fila+1,50 SAY Compag.FecDig
    @fila+2,03 say "VENCE A LOS 30 DIAS DE EMITIDO"
    @fila+2,40 SAY "Hora :"
    @fila+2,50 SAY Compag.HorDig
	IF TIPDOC$'RESRRURSAR' AND ESTADO#'99'
*	SELE reten
*	SET ORDER TO TAG reten5
	USE reten  IN 12  ORDER TAG reten5  ALIAS reten
		X_SW1=1
	ELSE	
*	SELE reten
*	SET ORDER TO TAG reten1
	USE reten  IN 12  ORDER TAG reten1  ALIAS reten
	ENDIF	
	SELE Maepre
	vord1=ORDER()
	SET ORDER TO maepre1

	SELE Reten
	SET RELATION TO periodo+'01001'+codcad INTO Maepre
	SELECT ComPag
	vrec=RECNO()

	
	SELECT RETEN	
	SEEK vkey
	
	IF FOUND()	
		IF _DEST1=2
	    	EJECT
		ENDIF

		DO cab_ret
		
		DO CASE
		   CASE X_SW1=1 
		       YZ=1
				SCAN WHILE MESCPPG=VANO AND NUMCPPG=VCLI AND CODCTC=VCTA AND CONPAGO='û'
				     
				     @FILA,01 SAY RETEN.NUMMES PICTURE '@J'
			    	 @FILA,03 SAY '.'
			    	 @FILA,04 SAY RETEN.NUMCP  PICTURE '@J'
		IF newsistem='1'
			=val_codcad(ALLTRIM(vcad),vper+'01001','C')
		ELSE
			=val_codca1(ALLTRIM(m.codcad),vper,'C')
		ENDIF
		IF NEWSISTEM='1'
    	 		   	 @FILA,10 SAY maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry+' '+reten.codcom
		ELSE
    	 		   	 @FILA,10 SAY RETEN.CODCAD+' '+maepre.codspr+'  '+maepre.actpry+' '+reten.codcom
		ENDIF
				   	 @FILA,37 SAY dispglo(yZ) 
				     @FILA,72 SAY RETEN.VALRET PICTURE '@Z 9,999,999.99'
			IF FILA>=56
					FILA =2
			 ENDIF
				     FILA=FILA+1
				     YZ=YZ+1
			    	 @FILA,37 SAY dispglo(YZ) 
				     FILA=FILA+1
				     YZ=YZ+1
			    	 @FILA,37 SAY dispglo(YZ) 
			    	 FILA=FILA+1
			    	 YZ=1
			    	 LVAL=LVAL+RETEN.VALRET

				ENDSCAN     
		   CASE X_SW1=0	
				SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
		    		 @FILA,03 SAY RETEN.CODRET PICTURE '@J'
			    	 @FILA,15 SAY RETEN.NOMRET
				     @FILA,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
				     FILA=FILA+1
			    	 
			    	 LVAL=LVAL+RETEN.VALRET
				ENDSCAN     
		ENDCASE		
		FILA=FILA+1
		@FILA,01 SAY REPLICATE("-",81)
		FILA=FILA+1
		@FILA,30 SAY 'T O T A L  ---->'
	    @fila,70 SAY Lval PICTURE '@Z 9,999,999.99'
		FILA=FILA+1
		@FILA,01 SAY REPLICATE("-",81)
		IF vref='**********'
		   DO cab_ret1
		   LVAL=0
		   SELECT RETEN	
		   SET ORDER TO RETEN1
		   SEEK vkey
	   	   SCAN WHILE NUMMES=VANO AND NUMCP=VCLI AND CODCTC=VCTA
		    	 @FILA,03 SAY RETEN.CODRET PICTURE '@J'
		    	 @FILA,15 SAY RETEN.NOMRET
			     @FILA,53 SAY RETEN.VALRET PICTURE '@Z 999,999,999.99'
			     FILA=FILA+1
		    	 LVAL=LVAL+RETEN.VALRET
			ENDSCAN     
		FILA=FILA+1
			@FILA,01 SAY REPLICATE("-",71)
			FILA=FILA+1
			@FILA,20 SAY 'T O T A L  ---->'
		    @fila,53 SAY Lval PICTURE '@Z 999,999,999.99'
			FILA=FILA+1
			@FILA,01 SAY REPLICATE("-",71)
		ENDIF	
	ENDIF
	SELE Maepre
	SET ORDER TO (vord1)

	USE IN 12
	SELE ComPag
	GO vrec
ELSE
	@5,14 SAY CHR(14)
	@5,15 SAY "<< COMPROBANTE DE PAGO >>"
	@5,44 SAY CHR(27)+CHR(18)
	@07,03 SAY "      N§ C/P :"  
	@07,20 SAY NUMCP+'.'+NUMMES
	@08,03 SAY "   Fecha C/P :"  
	@08,20 SAY FECCP picture "@D"
	@09,03 SAY "     Cta cte :"  
	@09,20 SAY codctc
	@09,35 SAY val_fun('Caja','codctc','Descri',codctc)
	@14,14 SAY CHR(14)
	@14,18 SAY "I N U T I L I Z A D O"
	@15,01 SAY CHR(27)+CHR(18)
ENDIF

IF _DEST1=2
   	EJECT
ENDIF

SET DEVICE TO SCREEN 
USE Clase    IN  8  ORDER TAG Clase1       ALIAS Clase   
*SELE reten
*SET ORDER TO TAG reten1

RETURN


FUNCTION dispglo  &&Displaya Glosa de C/P.Retenci¢n
*--------------
parameters ZY
PRIVATE valias
valias=ALIAS()
SELE ComPag
SEEK ALLTRIM(reten.nummes)+reten.numcp+allt(reten.codctc)
IF FOUND()
   vglosa=IIF(ZY=1,SUBST(ComPag.glosa,21,36),IIF(ZY=2,SUBST(ComPag.glosa,58,36),IIF(ZY=3,SUBST(ComPag.glosa,95,36),' ')))
ELSE
   STORE SPACE(33) TO vglosa
ENDIF
SELE (valias)
RETURN vglosa

PROCEDURE termi
*--------------
ven_accion = .F.
DEACTIVATE MENU
RETURN

PROCEDURE fin_opcion
*-------------------
ON KEY
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

PROCEDURE gircheq
*----------------
ON KEY LABEL f5  DO agregchq
ON KEY LABEL f8  DO elimichq
ON KEY LABEL f10 KEYBOARD CHR(23)
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber
vaviso=.F.
SCAN WHILE nummes=ALLTRIM(m.nummes) and numref=m.numcp and codctc=ALLTRIM(m.codctc)
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
   	SET ORDE TO RETEN1
   	SEEK m.nummes+m.numcp+m.codctc
   	IF EOF()
   		DO STANDBY WITH 'Error, en C/P, entre a corregir el documento'
   		SELE ComPag
   		DO Vista
   		RETURN
   	ENDIF	
   	totalq = 0
   	SCAN WHILE nummes=ALLTRIM(m.nummes) and numcp=m.numcp and codctc=ALLTRIM(m.codctc)
   		totalq = totalq + valret
   	ENDSCAN
   	IF VHABER-TOTALQ#0
    	DO STANDBY WITH 'La Retenci¢n es diferente a lo registrado en el asiento Patrimonial'
      	SELE ComPag
      	DO vista
      	RETURN
   	ENDIF
ENDIF
SELE Cheque
SEEK m.nummes+m.numcp+m.codctc
IF !FOUND()
	DO agregchq
ENDIF
IF m.estado#'99'
***
SELE COMPAG
SEEK m.nummes+m.numcp+m.codctc
VALOR=ComPag.import-ComPag.reten
SELE CHEQUE
SEEK m.nummes+m.numcp+m.codctc
REPLA  VALCHQ WITH VALOR
***
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_4 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
	estado     :H= 'ES' :W=.F.,;
	numchq     :H= 'N§ Cheque' ,;
	fecchq     :H= 'Fecha' ,;
	nomgir     :H= 'Girado a' :p='@S26' :F ,;
	valchq     :H= 'Monto' :p='99,999,999.99' :v=(VALCHQ=VALOR)
	*VALID VALCHQ=VALOR

GO TOP
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
SCAN WHILE NUMMES=ALLTRIM(m.nummes) AND NUMCP=m.numcp AND CODCTC=ALLTRIM(m.codctc)
	IF EMPTY(numchq)
		DELE NEXT 1
	ENDIF
ENDSCAN
SELE ComPag
IF RLOCK()
   	 REPLA estado with '10'
ENDIF
ELSE
REPLACE estado with '99'
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_4 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc) FIELD ;
	estado     :H= 'ES' :W=.F. :R,;
	CodPart    :H= 'Part.':W=.F. :R,;
	numchq     :H= 'N§ Cheque' ,;
	fecchq     :H= 'Fecha' :R,;
	nomgir     :H= 'Girado a' :p='@S26' :F :R,;
	valchq     :H= 'Monto' :p='99,999,999.99' :R
GO TOP
ENDIF
USE IN 12
SELE ComPag
DO vista
RETURN


PROCEDURE regret
*---------------
ON KEY LABEL F5  DO agregret
ON KEY LABEL F8  DO elimiret
ON KEY LABEL f10 KEYBOARD CHR(23)
USE reten    IN 12  ORDER TAG reten1       ALIAS reten
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT itecp
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
vvcompon=Itecp.codcom
vvmeta  =Itecp.codmet

SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber
SCAN WHILE nummes=ALLTRIM(m.nummes) and numref=m.numcp and codctc=ALLTRIM(m.codctc)
	 IF RET='S' && suma solo valores del haber para comparar con valor de retencion
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
	 ENDIF
ENDSCAN
IF vhaber = 0
   DO STANDBY WITH 'No existen retenciones registradas'
   SELEC ComPag
   DO vista
   RETURN
ENDIF


SELECT reten
SEEK ALLT(m.nummes)+m.numcp+ALLT(m.codctc)
vale=0
IF !FOUND()
	DO agregret
ENDIF
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_5 KEY ALLT(m.nummes)+m.numcp+ALLT(m.codctc) FIELDS;
	estado     :H= 'Es' :W=.F. :R ,;
	fecret     :H= 'Fecha':V=Fecha() ,;
	codret     :v=val_rete(Codret,"CODRET","Codret")  ,;
    nomret     :H= 'Retenido a':p='@S39':R ,;
	Valret     :H= 'Monto' :p='99,999,999.99' 
SELE reten	
GO TOP
SEEK ALLT(m.nummes)+m.numcp+ALLT(m.codctc)
totalq = 0

SCAN WHILE nummes=ALLT(m.nummes) AND numcp=m.numcp AND codctc = ALLT(m.codctc)
    REPLACE partret WITH m.partret,;
     		nummeshc WITH m.nummeshc,;
	    	numhc WITH m.numhc
	IF valret = 0.00 OR EMPTY(codret)
	   DELE NEXT 1
	ELSE
       totalq = totalq + valret
	ENDIF	
ENDSCAN
FLUSH

SELECT Reten
DO CASE
	CASE vhaber > totalq
		DO standby WITH 'Lo registrado en Retenciones es menor a C.Patr.'
		SELE ComPag
		IF RLOCK()
		   REPLACE estado WITH '00'
		ENDIF
	CASE vhaber < totalq
		SELE ComPag
		DO standby WITH 'La C.Patr. es menor a las Retenciones registradas'
		IF RLOCK()
			REPLACE estado WITH '00'
		ENDIF
	CASE vhaber = totalq
		SELE ComPag
		DO standby WITH 'Retenciones O.K.'
		IF RLOCK()
			REPLACE estado WITH '10'
		ENDIF
ENDCASE
SELE ComPag
USE IN 09  
USE IN 12
DO vista
RETURN


FUNCTION Fecha
*-------------
vFecha = FecRet
RETURN

PROCEDURE agregchq
*-----------------
PUBLIC vdes,xpart
XPART=space(5)
=dispdes()
SELECT Cheque
IF f_appd()
	REPLACE nummes  WITH ComPag.nummes,;
    		numcp   WITH ComPag.numcp,;
	   	    codctc  WITH ComPag.codctc,;
		    fecchq  WITH ComPag.feccp,;
		    nomgir  WITH vdes,;
            estado  WITH '00',;
		    codcad  WITH ComPag.codcad,;
		    tipdoc  WITH ComPag.tipdoc,;
		    periodo WITH ComPag.periodo,;
		    valchq  WITH ComPag.import-ComPag.reten
ENDIF
RETURN


PROCEDURE elimichq
*-------------------
SELECT Cheque
IF RLOCK()
	DELE NEXT 1
ENDIF
RETURN


PROCEDURE agregret
*-----------------
SELECT reten
IF f_appd()
REPLACE nummes   WITH m.nummes ,;
	    fecret   with vfecha,;
		numcp    WITH m.numcp  ,;
		codctc   WITH m.codctc ,;
		fecret   WITH m.feccp  ,;
		estado   WITH '00'     ,;
		codcad   WITH m.CodCad ,;
		codcom   WITH vvcompon,;
		codmet   WITH vvmeta,;
		periodo  WITH m.periodo,;
		partret  WITH m.partret,;
		nummeshc WITH m.nummeshc,;
		numhc    WITH m.numhc
ENDIF
RETURN


PROCEDURE elimiret
*-----------------
SELECT reten
IF RLOCK()
	DELE NEXT 1
ENDIF
RETURN


PROCEDURE GRABA
*--------------
REPLACE codctc WITH m.codctc
RETURN .T.


PROCEDURE VALE
*-------------
IF EMPTY(m.codctc) 
   DO STANDBY with 'No esta asignada la Cuenta Corriente'
   return .F.
else
   return .t.
endif


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
	IF RLOCK()
		REPLACE valchq WITH m.import-(vale+vale1)
	ENDIF
	UNLOCK
	SELE ComPag
	IF EMPTY(numchq) AND RLOCK()
		REPLACE numchq WITH Cheque.numchq
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
     DO LISCP WITH NUM_C
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
 SELE ComPag
RETURN


FUNCTION valsec  &&Displaya Nro C/P Sectorial
*--------------
PUBLIC vinicp
IF vsector#'A'
	vinicp=LEFT(m.numhc,1)
ELSE
	vinicp='A'
ENDIF 
IF m.codfte<>'00' .AND. m.codfte<>'09'
	SELECT Caja
	SEEK ALLTRIM(m.codctc)
	IF !FOUND()
		DO standby WITH 'No se tiene inscrita la cuenta ' + m.codctc
	ELSE
		m.numcp =  vinicp+PADL(corcp+1,3,'0')
		w_ctah  =  Caja.CuentaH
	ENDIF
ELSE

	USE CtaSec IN 27  ORDER CtaSec1 ALIAS CtaSec
	SELE CtaSec
	SEEK allt(m.codctc)+allt(vinicp)
	m.numcp =  vinicp+PADL(CtaSec.corsec+1,3,'0')
	USE
ENDIF

RETURN

FUNCTION valse1  &&Displaya Nro C/P Sectorial
*---------------
parameters VSECTOR
*PUBLIC vinicp
DO CASE
	CASE Vsector='A'
		vinicp='A'
	CASE Vsector='E'
		vinicp='E'
	CASE Vsector='I'
		vinicp='I'
	CASE Vsector='P'
		vinicp='P'
	CASE Vsector='T'
		vinicp='T'
*	CASE Vsector='M'
*		vinicp='M'
*	CASE Vsector='V'
*		vinicp='V'
*	CASE Vsector='G'
*		vinicp='G'
	CASE Vsector='S'
		vinicp='S'
	CASE Vsector='F'
		vinicp='F'
	CASE Vsector='B'
		vinicp='B'
ENDCASE

	USE CtaSec IN 27 ORDER CtaSec1 ALIAS CtaSec
	SELE CtaSec
	SEEK allt(m.codctc)+allt(vinicp)
**	
	m.numcp = vinicp+PADL(CTASEC.corsec+1,3,'0')
	USE

RETURN

&&&& Reten. xMetas

PROCEDURE Pgret
*---------------
PARAMETER mtipo
vimport=0
vsel = SELECT()
SELECT ComPag
vreg = RECNO()
vOrd = ORDER()
USE IN 26
USE RETEN IN 12 ORDER TAG RETEN6  ALIAS Reten
SELECT Reten
set order to tag reten6
IF EOF()
	USE IN 12
	RETURN .F.
ENDIF
vcond = 'EMPTY(Conpago) OR fecpag = m.feccp '
vcond1= '(numcppg = m.numcp AND mescppg = m.nummes) OR EMPTY(Conpago) OR fecpag = m.feccp )'
SET FILT TO  IIF(mtipo='1',&vcond,&vcond1)
vord = ORDER()
*SET FILT TO CODCTC=m.codctc AND CODCAD=m.codcad
SET ORDER TO Reten7
GO TOP
SEEK m.codctc+m.codcad
*IF EOF()
IF !FOUND()
   SET ORDER TO Vord
   USE IN 12
   DO Standby WITH 'Rubro sin retenci¢n'
   RETURN .F.
ENDIF   	   
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F7 DO Maruno
ON KEY LABEL F8 DO DesMartodo
ON KEY LABEL F9 DO DesMaruno
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°®F7¯ û °  ®F9¯ xû  °    ®F6¯ û Todo      °       F8¯ xû Todo ° ®F10¯ SALE°°°'
DO Logos WITH Rotulo1,vTempo
SET REFRESH TO 0.01
BROWSE WINDOW Wind_9 KEY m.codctc+m.codcad NODELETE NOMENU ;
			NOAPPEND FIELD ;
			Nummes :r:H="Mes":W=.f.,;
			Numcp  :r,;
			conpago:r:H="Pago":w=.F.,;
			nomret :r:W=.f.:30,;
			valret :r:W=.f.,;
			CODRET :R:W=.F.,;
			codcad :r:w=.f.
SET REFRESH TO 0

SELE RETEN
GO TOP
cont=0
SEEK m.codctc+m.codcad
SCAN WHILE RETEN.codctc=ALLT(m.codctc) AND reten.codcad=ALLT(m.codcad) 
	 cont=cont+1
     IF ALLTRIM(conpago)='û' AND fecpag=m.feccp
     	m.import=m.import+reten.valret
     	IF cont=1
     		m.codcad=reten.codcad
     		cccomp  =reten.codcom
     		ccmet   =reten.codmet
        ENDIF
     ENDIF
ENDSCAN
ON KEY LABEL F10 
ON KEY LABEL F6 
ON KEY LABEL F7 
ON KEY LABEL F8 
ON KEY LABEL F9 
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
@ 8,60 SAY M.import
SELECT ComPag
SELECT Reten
USE	
SELECT (vsel)		

RETURN .T.

&&&&Fin Ret.x Meta

PROCEDURE Pagret
*---------------
PARAMETER mtipo
vimport=0
vsel = SELECT()
SELECT ComPag
vreg = RECNO()
vOrd = ORDER()
USE IN 26
USE RETEN IN 12 ORDER TAG RETEN4  ALIAS Reten
SELECT Reten
set order to tag reten4
IF EOF()
	USE IN 12
	RETURN .F.
ENDIF
vcond = 'EMPTY(Conpago) OR fecpag = m.feccp '
vcond1= '(numcppg = m.numcp AND mescppg = m.nummes) OR EMPTY(Conpago) OR fecpag = m.feccp )'
SET FILT TO  IIF(mtipo='1',&vcond,&vcond1)
vord = ORDER()
SET ORDER TO Reten2
SEEK m.codctc+m.codret
IF !FOUND()
   SET ORDER TO Vord
   USE IN 12
   DO Standby WITH 'Rubro sin retenci¢n'
   RETURN .F.
ENDIF   	   
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F7 DO Maruno
ON KEY LABEL F8 DO DesMartodo
ON KEY LABEL F9 DO DesMaruno
HIDE MENU mMenu
ACTIVATE SCREEN
vTempo = '°°°°®F7¯ û °  ®F9¯ xû  °    ®F6¯ û Todo      °       F8¯ xû Todo ° ®F10¯ SALE°°°'
DO Logos WITH Rotulo1,vTempo
SET REFRESH TO 0.01
BROWSE WINDOW Wind_9 KEY m.codctc+m.codret NODELETE NOMENU ;
			NOAPPEND FIELD ;
			Nummes :r:H="Mes":W=.f.,;
			Numcp  :r,;
			conpago:r:H="Pago":w=.F.,;
			nomret :r:W=.f.:30,;
			valret :r:W=.f.
SET REFRESH TO 0
SELE RETEN
GO TOP
cont=0
SEEK m.codctc+m.codret
SCAN WHILE RETEN.codctc=ALLT(m.codctc) AND reten.codret=ALLT(m.codret) 
	 cont=cont+1
     IF ALLTRIM(conpago)='û' AND fecpag=m.feccp
     	m.import=m.import+reten.valret
     	IF cont=1
     		m.codcad=reten.codcad
     		cccomp  =reten.codcom
     		ccmet   =reten.codmet
        ENDIF
     ENDIF
ENDSCAN
ON KEY LABEL F10 
ON KEY LABEL F6 
ON KEY LABEL F7 
ON KEY LABEL F8 
ON KEY LABEL F9 
ACTIVATE SCREEN
vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO Logos WITH Rotulo1,vTempo
@ 8,60 SAY M.import
SELECT ComPag
SELECT Reten
USE	
SELECT (vsel)		

RETURN .T.

PROCEDURE MARtodo
*--------------------
PRIVATE vctaret
RECS=RECNO()
vctaret = m.Codctc+m.codret
SEEK vctaret
SCAN WHILE vctaret = codctc+codret 
	SELECT Reten
  	REPLACE conpago with 'û'
	REPLACE fecpag  WITH m.feccp
  	REPLACE numcppg WITH m.numcp
  	REPLACE mescppg WITH m.nummes
  	IF AT(ALLT(m.observ) + ' ' + numcp + ' ('+ALLT(STR(valret,18,2))+'), ',m.observ)=0
  		m.observ = ALLT(m.observ) + ' ' + numcp + ' ('+ALLT(STR(valret,18,2))+'), '
  	ENDIF
ENDSCAN 
m.observ=LEFT(m.observ,130)
GO recs
RETURN

PROCEDURE Maruno
*--------------
SELECT Reten
REPLACE conpago with 'û'
REPLACE fecpag  WITH m.feccp
REPLACE numcppg WITH m.numcp
REPLACE mescppg WITH m.nummes
IF AT(ALLT(m.observ) + ' ' + numcp + ' ('+ALLT(STR(valret,18,2))+'), ',m.observ)=0
	m.observ = ALLT(m.observ) + ' '+numcp + ' ('+ALLT(STR(valret,18,2))+'), '
ENDIF
IF !EOF()
   SKIP
ENDIF   
m.observ=LEFT(m.observ,130)
SELE compag
return

PROCEDURE DesMaruno
*-----------------
SELECT Reten
REPLACE conpago with ' '
REPLACE fecpag  WITH {  /  /  }
REPLACE numcppg WITH ' '
REPLACE mescppg WITH ' '
m.observ = STRTRAN(m.observ,numcp + ' ('+ALLT(STR(valret,18,2))+'), ','')
IF !EOF()
   SKIP
ENDIF   
return

PROCEDURE Desmartodo
*-----------------
RECS=RECNO()
PRIVATE vctaret
vctaret = m.Codctc+m.codret
SEEK vctaret
SCAN WHILE vctaret = codctc+codret 
  	REPLACE conpago with ' '
  	REPLACE fecpag  WITH {  /  /  }
	REPLACE numcppg WITH ' '
	REPLACE mescppg WITH ' '
	m.observ = STRTRAN(m.observ,numcp + ' ('+ALLT(STR(valret,18,2))+'), ','')
ENDSCAN
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


FUNCTION Vexiste
*---------------
SELE ComPag
SEEK ALLT(m.nummes)+m.numcp+ALLT(m.codctc)
IF FOUND()
    DO STANDBY WITH "El Comprobante de Pago ya existe"
	RETURN .F.
ENDIF
RETURN .T.	

FUNCTION DISPDES  &&Displaya nombre del Acreedor
*---------------
DO CASE
   CASE ComPag.tipdoc$'HCMESUSHSSAH'
        vdes=IIF(ComPag.tipprv='I',val_auxi(ALLT(ComPag.codpre),'80','D'),IIF(ComPag.tipprv='P',val_auxi(ALLT(ComPag.codprv),'20','D'),IIF(ComPag.tipprv='E',val_Auxi(ALLT(ComPag.codemp),'30','D'),IIF(ComPag.tipprv='O' AND !EMPTY(m.codotr),val_Auxi(ALLT(ComPag.codotr),'09','D'),ComPag.nompre))))
   CASE ComPag.tipdoc$'RESRRURSAR' 
       vdes =VAL_PARA(ComPag.codret,'CODRET','D',22,40)
   OTHER     
	   vdes	=ComPag.nompre         		
ENDCASE       
RETURN vdes


FUNCTION Disppart  &&Displaya partida
*---------------
private ypart
ypart=0
IF ComPag.tipdoc$'HCRGSHSRSEHMAH'
   SELE IteCp
   SET ORDER TO Itecp1
   SEEK ALLT(ComPag.nummes)+ComPag.numcp+allt(ComPag.codctc)
   IF FOUND()
   	  IF Compag.tipfun='I'
   	     xpart=IteCp.CodPart
   	  ELSE
   	     ypart=val(IteCp.codanal)
   	     SCAN WHILE ALLT(IteCp.nummes)+IteCp.NumCP+ALLT(IteCp.codctc)=ALLT(ComPag.nummes)+ComPag.numcp+allt(ComPag.codctc) 
			 IF val(IteCp.codanal)>=ypart
		  	    ypart=val(IteCp.codanal)
			 ENDIF
		ENDSCAN
		Xpart=IIF(ypart<10,'0'+ALLT(STR(ypart,5,2)),ALLT(STR(ypart,5,2)))
	 ENDIF
	ELSE
	   DO STANDBY WITH "Sin partida"  			       	     
	ENDIF
ENDIF
RETURN Xpart	   

PROCEDURE cab_ret  &&Cabecera de Retenciones
*----------------
		@0,0 SAY CHR(18)
		@1,3   SAY ALLTRIM(CIA)
		@1,68  SAY "PAG:"
		@1,76  SAY ALLTRIM(STR(_PAGENO,8))
		@2,3   SAY "LisRetCP"
		@2,68 SAY "FECHA:"
		@2,76 SAY DATE()           
		@4,20 SAY CHR(14)
		@4,13 SAY IIF(X_SW1=1,"RETENCIONES CANCELADAS","DEDUCCIONES Y/O RETENCIONES")
		@5,0 SAY CHR(27)+CHR(18)
		@5,34 SAY 'C/P: '+VCLI+'.'+ALLTRIM(VANO)
		@6,1 SAY "Ú"
		@6,2 SAY REPLICATE("Ä",82)
		@6,84 SAY "¿" 
		@7,1 SAY "³"
		IF NEWSISTEM='2'
			@7,3 SAY IIF(X_SW1=1," C/P  META    CADENA FUNCIONAL            DESCRIPCION                     MONTO"," RUBRO            DESCRIPCION                              MONTO")
		ELSE
			@7,3 SAY IIF(X_SW1=1," C/P     CADENA FUNCIONAL            DESCRIPCION                     MONTO"," RUBRO            DESCRIPCION                              MONTO")
		ENDIF
		@7,84 SAY "³"
		@8,1 SAY "À"
		@8,2 SAY REPLICATE("Ä",82) 
		@8,84 SAY "Ù"
	    FILA=10
RETURN

PROCEDURE cab_ret1  &&Cabecera de Retenciones
*----------------
		@0,0 SAY CHR(18)
		@1,3   SAY ALLTRIM(CIA)
		@1,68  SAY "PAG:"
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
		@7,3 SAY IIF(X_SW1=1," C/P              DESCRIPCION                              MONTO"," RUBRO            DESCRIPCION                              MONTO")
		@7,72 SAY "³"
		@8,1 SAY "À"
		@8,2 SAY REPLICATE("Ä",70) 
		@8,72 SAY "Ù"
	    FILA=10
RETURN

FUNCTION wdbco  &&Descripci¢n de Banco para el Repo
*-------------
PRIVATE wdbc,wcbc
SELE Caja
SEEK ComPag.codctc
IF FOUND()
   wcbc=Caja.banco
   wdbc=Val_Para(wcbc,"BANCOS","D",22,25)   
ELSE
   wdbc=SPACE(10)
ENDIF
RETURN wdbc      


PROCEDURE cor_as  && F9
*--------------
SELE Caja
ingreso=.F.
SEEK m.codctc
w_Tipctc=Caja.Tipo
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat3      ALIAS astpat &&Egresos
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
IF !FOUND()
	DO agrite
ENDIF
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numcp+ALLTRIM(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	ret    :H='Ret?' :p='!' :v=(ret$'SN') AND GRABA():F
SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
STORE 0 TO vdebe, vhaber ,vret
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
		IF astpat.tipcta='D' AND LEFT(ALLT(asTpat.codcta),2)#'10'
		   ztotal=astpat.mtodeb
		ENDIF   
		IF RET='S'
		   vret=vret+MTOHAB
        ENDIF		   
        IF subs(codcta,1,3)='104' OR subs(codcta,1,3)='108' .OR. SUBS(CODCTA,1,2)='44'
         	mmonto=Mtohab
        ENDIF	
ENDSCAN
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
if ingreso
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef,tipctc WITH w_Tipctc
		REPLACE codcta WITH IIF(!m.tipdoc$'SESHSRAHAR','1010101000',IIF(SUBSTR(m.CodCal,11,2)#'06' AND  SUBSTR(m.CodCal,11,2)#'10','10102'+SUBSTR(m.CodCal,11,2)+'000','1010101000')),Mtodeb WITH mmonto, Tipcta WITH 'D', periodo WITH m.periodo
	ENDIF
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef,tipctc WITH w_Tipctc
		REPLACE codcta WITH IIF(!m.tipdoc$'SESHSRAHAR','1010101000',IIF(SUBSTR(m.CodCal,11,2)#'06' AND  SUBSTR(m.CodCal,11,2)#'10','10102'+SUBSTR(m.CodCal,11,2)+'000','1010101000')),Mtohab WITH mmonto, Tipcta WITH 'H', periodo WITH m.periodo
	ENDIF
else
	* si es corrije solo modifica
	SEEK ALLTRIM(m.nummes)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND()
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numcp=numref AND m.codctc=ALLTRIM(codctc)
		    if codcta=IIF(!m.tipdoc$'SESHSRAHAR','1010101000',IIF(SUBSTR(m.CodCal,11,2)#'06' AND  SUBSTR(m.CodCal,11,2)#'10','10102'+SUBSTR(m.CodCal,11,2)+'000','1010101000')) and tipcta='D'
	        	repla Mtodeb WITH mmonto, Fecha WITH m.feccp
	        endif
	        if codcta=IIF(!m.tipdoc$'SESHSRAHAR','1010101000',IIF(SUBSTR(m.CodCal,11,2)#'06' AND  SUBSTR(m.CodCal,11,2)#'10','10102'+SUBSTR(m.CodCal,11,2)+'000','1010101000')) and tipcta='H'
	        	repla Mtohab WITH mmonto, Fecha WITH m.feccp
	       endif
		ENDSCAN
    ELSE		
		IF f_appd()
			REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef,tipctc WITH w_Tipctc
			REPLACE codcta WITH IIF(!m.tipdoc$'SESHSRAHAR','1010101000',IIF(SUBSTR(m.CodCal,11,2)#'06' AND  SUBSTR(m.CodCal,11,2)#'10','10102'+SUBSTR(m.CodCal,11,2)+'000','1010101000')),Mtodeb WITH mmonto, Tipcta WITH 'D', TIPCTC WITH w_tipctc,periodo WITH m.periodo
		ENDIF
		IF f_appd()
			REPLACE nummes WITH m.nummes, numref WITH m.numcp,tipdoc WITH 'C/P',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.FecCP, FecHC WITH m.FecRef,tipctc WITH w_Tipctc
			REPLACE codcta WITH IIF(!m.tipdoc$'SESHSRAHAR','1010101000',IIF(SUBSTR(m.CodCal,11,2)#'06' AND  SUBSTR(m.CodCal,11,2)#'10','10102'+SUBSTR(m.CodCal,11,2)+'000','1010101000')),Mtohab WITH mmonto, Tipcta WITH 'H',TIPCTC WITH w_tipctc,periodo WITH m.periodo
		ENDIF
	ENDIF	
ENDIF	
USE IN 09 
USE IN 14
ON KEY
ON KEY LABEL f2 DO gircheq
ON KEY LABEL f3 DO regret
ON KEY LABEL f4 DO imp_cp
DEACTIVATE WINDOW wind_12
UNLOCK
SELE ComPag
DO vista
RETURN


PROCEDURE LIMPIA
*---------------
@ 3,22 SAY SPAC(56)
RETURN

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

FUNCTION muespro
*---------------
PARAMETERS xtipprv,xcodotr,xtipdoc
PRIVATE xnompre
DO CASE
    CASE xtipprv="E"
         xnompre=val_Auxi(ALLT(Compag.Codemp),'30','V')
    CASE xtipprv="P"
         xnompre=val_Auxi(ALLT(Compag.Codprv),'20','V')
    CASE xtipprv="O" AND !EMPTY(xcodotr)
         xnompre=val_Auxi(ALLT(xCodotr),'09','V')
    CASE xtipprv="I" 
         xnompre=val_Auxi(ALLT(compag.codpre),'80','V')
    CASE xtipdoc$"RESRRUAR"
		 xnompre=VAL_PARA(Compag.codret,'CODRET','V',22,40)	
	OTHER  	   
	     xnompre=compag.nompre
ENDCASE
RETURN xnompre


FUNCTION Val_rete
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  mCol = 0
  mLong = 40
  mDist = 6
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    SET ORDER TO PARMAE2
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       SELECT (mAlias)
	   RETURN 
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    SELE PARMA
    SET FILTER TO
    SET ORDER TO PARMAE1
  ENDIF
  mValor = Parma.Codigo
  mCUENTA= Parma.DescriAu2
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = LEFT(Parma.descriau2, 1)
  SELECT (mAlias)
  REPLACE Reten.codret  WITH mValor
  REPLACE Reten.nomret  WITH mdescr
  REPLACE reten.tributo WITH mdescriaux
RETURN .T.


PROCEDURE vis_calen   &&Vista Codificaci¢n Funcional Program tica
*-------------------
@ 06,22 SAY m.codcad
@ 06,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 07,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
RETURN


FUNCTION val_ff
*--------------
m.fecdig=DATE()
m.hordig=TIME()
RETURN



FUNCTION Disp_usu
*----------------
PRIVATE xuser_id,xdesu
vali=ALIAS()
vord=ORDER()
xuser_id=SPACE(08)
xdesu=SPACE(30)
xuser_ID = CHRTRAN(Compag.Usuario,'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?','ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789')
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
