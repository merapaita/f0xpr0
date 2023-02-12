* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegMb.PRG  13/07/96                                 L: 1385  ³	
* ³ Movimiento de Bancos  Asientos Autom ticos                   ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.   06/1997   ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*USE PARMAE2   IN  1 	ORDER TAG Parmae1      ALIAS Parma
USE PARMAE   IN  1 	ORDER TAG Parmae1      ALIAS Parma
USE Cajas    IN  2 	ORDER TAG cajas1       ALIAS caja
USE Cheque   IN  3 	ORDER TAG Cheque1      ALIAS Cheq
USE MovBco   IN  4 	ORDER TAG Movbco11     ALIAS Movb
USE cuentas  IN  7 	ORDER TAG cuentas1     ALIAS cuenta
USE IteRi    IN  5 	ORDER TAG IteRi1       ALIAS IteRi
*USE clase    IN  8  ORDER TAG clase1       ALIAS clase
USE maepre   IN 16  ORDER TAG maepre1      ALIAS maepre
USE itepar   IN 25  ORDER TAG itepar1      ALIAS ITEPAR          
USE hojcon   IN  9  ORDER TAG hojcon1      ALIAS hoja   &&H/C
USE Ingreso  IN 10  ORDER TAG Ingreso1 		ALIAS IngR
USE IteUsuOp IN 26  ORDER TAG IteUsuOp1 		ALIAS SubOp

Vmens09 = 'Este Movimiento Bancario ha sido anulado'
	
SELECT MovB
GO BOTTOM
*- Variables de trabajo (registro a trabajar)
PUBLIC m.CodCtc,w_tipctc,w_monto,xmri,xnri,vcodprg,vcodsub,vcodact,vcodanal,mmonto,sw_z,w_ctaD,mcta10,mcond,mctaf,mctai
PUBLIC vctadeb,vctahab,vvaldeb,vvalhab,vuser_id,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE SPACE(1)  TO mcta10,mcond
STORE SPACE(5)  TO vcodanal
STORE SPACE(2)  TO vcodprg,xmri
STORE SPACE(3)  TO vcodsub,vcodact
STORE SPACE(4)  TO xnri
STORE SPACE(10) TO mctai,vctadeb,vctahab,w_ctaD,w_CtaH
STORE SPACE(12) TO mctaf
STORE 0 	    TO w_monto,mmonto,sw_z,vvaldeb,vvalhab
SCATTER MEMVAR
*- Inicia proceso
DO Inicia                    && Define ventanas, men£s, t¡tulos
DO Pantalla                  && Muestra pantalla inicial
DO vista WITH .F.
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

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE ' ®F9 Corrige Asientos ¯   Movimiento de Bancos  ' COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 24,79 DOUBLE ;
 TITLE 'Revisi¢n de Movimientos     Presione F10 para concluir          ' COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 15,10 TO 23,70 DOUBLE ;
 TITLE 'Movimiento Bancario : Detalle - F10 Graba' COLOR SCHEME 10

 DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
 TITLE ' ASIENTOS PRESUPUESTALES' COLOR SCHEME 02

 DEFINE WINDOW wind_7 FROM 13,10 TO 17,70 ;
 TITLE ' Centralizaci¢n de Caja ' COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT 'A\<Nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO lista
 ON SELECTION PAD termi  OF mMenu DO termi
 ON KEY LABEL f9 DO cor_as
 ACTIVATE SCREEN
 RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  1, 02 SAY "          Nro. Doc :"
 @  2, 02 SAY "             Fecha :"
 @  2, 60 SAY            "Periodo :"
 @  3, 02 SAY "           Cta.Cte :"
 @  4, 02 SAY "             Banco :"
 @  5, 02 SAY "           Tip.Doc :"
 @  6, 02 SAY "       Transacci¢n :"
 @  7, 02 SAY "           Fte.Fto :"
 @  8, 02 SAY "          Concepto :"
 @  9, 02 SAY "Hoja Control [S/N] :"   
 @ 10, 02 SAY " Cadena Funcional :"
 @ 10, 27 SAY " UG  UE  FN PRG SBPRG ACTPRY"


* @ 10, 02 SAY "          Programa :"
* @ 11, 02 SAY "       SubPrograma :"
* @ 12, 02 SAY "         Actividad :"
 @ 13, 02 SAY "        Referencia :"
 @ 14, 02 SAY "             Monto :" 
 @ 15, 02 SAY "             Forma :"   
 @ 16, 02 SAY "            Nombre :"   
 @ 17, 02 SAY "     Observaciones :"   
 @ 18, 02 SAY "Part. Presupuestal :"   
 @ 19, 02 SAY "Clasificador [S/N] :"   
 @ 20, 02 SAY "      Mes Contable :"   
 @ 20, 40 SAY "     Fec. Contable :"   
 RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 PARAMETER Vacio
 ACTIVATE WINDOW Wind_0
 SELECT MovB
 IF !Vacio
    SCATTER MEMVAR
	 ON KEY LABEL f9 DO cor_as
 ENDIF
 IF !EMPTY(m.codcad) 
	 =val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
 ENDIF

 @  0,45 SAY 'Estado     :'
 @  0,60 SAY IIF(Estado='99','  Anulado  ','    OK û   ') COLOR SCHEME 2
 @  1,22 SAY m.numero PICTURE '9999'
 @  1,29 SAY m.Nummes 
 @  2,22 SAY m.Fecha
 @  2,72 say m.periodo
 @  3,22 SAY m.CodCtc
 @  3,38 SAY Val_Fun("Caja","Codctc","Descri",ALLTRIM(m.Codctc)) 
 @  4,22 SAY Val_para(m.codban,'BANCOS','V',26,26)
 @  5,22 SAY Val_para(m.TipDoc,'MOVBCO','V',22)
 @  6,22 SAY Val_para(ALLTRIM(m.Transa),'TRANSA','V',22)
 @  7,22 SAY Val_para(m.CodFte,'CODFTE','V',22)
 @  8,22 SAY Val_para(m.Motivo,'MOTIVO','V',22)
 @  9,22 SAY IIF(m.hojcon='S',m.numhc+'.'+m.nummeshc,SPACE(7))
 IF !EMPTY(m.codcad) 
 	DO vis_calen
 ENDIF


* @ 10,22 SAY val_para(SUBSTR(m.codcal, 8,2),'CODPRG','V',26,40)
* @ 11,22 SAY VAL_SUBP(substr(m.codcal,10,3),'CODSUB'+substr(m.codcal,8,2)+'    ','V',22,40)
* @ 12,22 SAY VAL_PYAC(alltrim(substr(m.codcal,13,3)),m.Periodo+substr(m.CodCal, 8,5),'V',22,40,3)
 @ 13,22 SAY m.numref
 @ 14,22 SAY m.Monto    PICTURE '999,999,999,999.99'
 @ 15,22 SAY IIF(m.transa='ABO',Val_para(ALLTRIM(m.Forma),'FORMAS','V',22),' ')
 @ 16,22 SAY m.Nombre SIZE 1,40
 @ 17,22 SAY m.Concept
 *IF ALLTRIM(m.tipdoc)='N/C' AND (ALLTRIM(m.motivo)='15'  OR ALLTRIM(m.motivo)='14' )
 *    @ 18,22 SAY m.codpart
*	 @ 18,28 SAY VAL_PART(SUBSTR(m.codpart,4,2),LEFT(m.codpart,2),'D',22) 
 *ENDIF	 
* @ 19,22 SAY IIF(m.tipcla='S',m.parting,' ')
* @ 19,34 SAY IIF(m.tipcla='S',vdpart(m.parting),' ') 
 @ 20,22 SAY m.NummesC            PICTURE '99'
 @ 20,62 SAY m.FechaC             PICTURE '@E'

  IF !vFlag2$'J*'
	DO SubOpc
  ENDIF

 RETURN


PROCEDURE Revis
*--------------
SELECT Movb
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 ACTIVATE SCREEN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE  WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
 	NummesC  :H='Nummes',;
 	Numero,;
 	Tipdoc,;
 	Monto,;
 	fecha,;
	codctc :H='CtaCte',;
    x1=Val_para (Motivo, 'MOTIVO', 'D') :H='Motivo'
 ON KEY LABEL F10
 DO vista
 RETURN


PROCEDURE Busca
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 18,42 TO 20,60 COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(4) TO Vbusca
 STORE SPACE(2) TO VANOB
 @ 0,13 say "."
 @ 0,01 SAY 'N£mero:' GET Vbusca PICT '@!' 
 @ 0,14 GET VANOB PICT '@!' 
 READ
 RELEASE WINDOW WBusca
 IF EMPTY(Vbusca) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK ALLT(VANOB)+Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicho movimiento de banco no fue encontrado."
     GOTO Vtemp
   ELSE
     DO vista
   ENDIF
 ENDIF
 RETURN

PROCEDURE Anter
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT Movb
 IF !BOF()
   SKIP -1
 ENDIF
 IF BOF()
   GO TOP
   DO standby WITH "Inicio de archivo: no existe Caja anterior."
 ELSE
   DO vista
 ENDIF
 RETURN

PROCEDURE Proxi
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 SELECT Movb
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe Caja siguiente."
   GO BOTTOM
 ELSE
   DO vista
 ENDIF
 RETURN

PROCEDURE Corri
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 
 IF Estado = '99'  && Anulada
	DO STANDBY WITH Vmens09
	RETURN
 ENDIF

 ACTIVATE WINDOW Wind_0
 DO pantalla
 SCATTER MEMVAR
 INGRESO=.F.
* vcodprg=SUBSTR(m.codcal,8,2)
* Vcodsub=SUBSTR(m.codcal,10,3)
* Vcodact=SUBSTR(m.codcal,13,3)
 @  1,22 GET m.numero  PICTURE "9999" WHEN .F.
 @  1,29 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',29,9) 
 *DISABLE
 @  2,22 GET m.Fecha VALID(MONTH(m.fecha)=VAL(m.nummes)) 
*@  2,22 GET m.Fecha VALID(MONTH(m.fecha)=VAL(m.nummes)) AND Valf(m.fecha,m.tipfun) AND vctc() 
 @  2,72 GET m.periodo 
*DISABLE
*@  3,22 GET m.CodCtc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.CodCtc,1,03,22) DISABLE
 @  3,22 GET m.CodCtc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.CodCtc,1,03,22) AND vctc()
*@  4,22 SAY Val_paraD(m.codban,'BANCOS','V',26,26)
 @  5,22 GET m.TipDoc  PICTURE '!!!' VALID Val_paraD (m.TipDoc ,'MOVBCO', ' ',22) AND vtransa()
 @  6,22 GET m.Transa   PICTURE '!!!' VALID Val_para(ALLTRIM(m.Transa),'TRANSA',' ',22)
 @  7,22 GET m.CodFte  PICTURE '!!!' VALID Val_Para (m.CodFte, 'CODFTE', ' ',22)
 *Se di¢ acceso a modificar la cadena funcional.
 @ 10,22 GET m.CodCad   PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 
 *
 @  8,22 GET m.Motivo  PICTURE '!!!!!' VALID Val_para(m.Motivo, 'MOTIVO', ' ',22)
 READ
 SELE parma
 SEEK 'MOTIVO'+m.motivo
 m.tipcla=SUBST(ALLT(parma.descriaux),11,1)
 m.parting=SUBST(ALLT(parma.descriaux),14,9)
 SELE movb
 @ 09,22 SAY IIF(m.hojcon='S',m.numhc+'.'+m.nummeshc,' ')
* @ 10,22 GET vCodPrg     PICTURE '!!'   VALID VAL_PARA(vCodPrg,'CODPRG',' ',22,40) 
* @ 11,22 GET vCodSub     PICTURE '!!!'  VALID VAL_SUBP(vCodSub,'CODSUB'+vCodPrg,' ',22,40)
* @ 12,22 GET vCodAct     PICTURE '!!!'  VALID VAL_PYAC(aLLTRIM(vCodact),m.Periodo+ALLTRIM(vcodprg)+ALLTRIM(vCodsub),'vcodact',22,40)  
 @ 13,22 GET m.numref
 @ 14,22 GET m.Monto   PICTURE '999,999,999,999.99'
 @ 15,22 GET m.Forma   PICTURE '!!'  VALID Val_para (ALLTRIM(m.Forma) , 'FORMAS', ' ',22) WHEN m.transa='ABO'
 @ 16,22 GET m.Nombre SIZE 1,40
 @ 17,22 GET m.Concept
* @ 18,22 GET m.codpart PICTURE '!!.!!' VALID VAL_PART(SUBSTR(M.CODPART,4,2),LEFT(M.CODPART,2),'C',22) WHEN ALLTRIM(m.tipdoc)='N/C' AND (ALLTRIM(m.motivo)='15' OR ALLTRIM(m.motivo)='14')
 m.estado='00'
 m.usuario=wuser_id
 *@ 19,22 SAY m.tipcla PICTURE "!"
 *@ 19,24 GET m.parting VALID VAL_INGE(SUBSTR(m.parting,11,2),LEFT(m.parting,9),' ') WHEN m.tipcla='S'
 @ 20,22 GET m.nummesC  PICTURE '!!'  VALID val_para(m.nummesC ,'FECMES',' ',29,9) 
 *DISABLE
 @ 20,62 GET m.fechaC  VALID(MONTH(m.fechaC)=VAL(m.nummesC))
 READ VALID VALREAD() 
* m.codcal  = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vCodAct)
 SELE MOVB
 IF LASTKEY() = 27
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   DO Vista
   RETURN
 ENDIF
*IF LASTKEY() <> 27 .AND. yesno("Confirme Correcciones")         && Si se presion¢ Esc, no graba

* IF m.asiento#0
DO IngAP
* ENDIF

 IF m.tipcla='S' OR m.hojcon='S'  &&Asientos Presupuestales para ingresos y egresos
*	   DO compre
 ENDIF    
* IF f_appd()
 IF LASTKEY() # 27
     SELECT Movb
    GATHER MEMVAR
	IF substr(movb.NOMBRE,1,30)=space(30)
	 	repla movb.NOMBRE with Val_para(mOVB.Motivo,'MOTIVO','D',26,22)
	endif		    
 ENDIF
 DO vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SELECT Movb
 DO pantalla
 SCATTER MEMVAR BLANK
 SELECT Parma
 * SEEK "CORREL"+ALLTRIM("MOVBCO")
 SEEK "CORREL"+"MOVBCO"
 m.numero=nument+1
 m.numero=PADL(m.numero,4,'0')
 ingreso=.T.
 m.fecha=DATE()
 M.PERIODO=right(str(year(m.fecha)),2)

 m.asiento=0
 @  1,22 SAY m.numero  PICTURE "9999"
 @  1,29 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',29,9) AND  VAL_NUM(m.nummes,m.numero)
 @  2,22 GET m.Fecha VALID(MONTH(m.fecha)=VAL(m.nummes))
 @  2,72 GET m.periodo VALID(right(str(year(m.fecha)),2)=M.PERIODO)
 @  3,22 GET m.CodCtc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.CodCtc,1,03,22) AND vctc()
 @  5,22 GET m.TipDoc  PICTURE '!!!' VALID Val_paraD (m.TipDoc ,'MOVBCO', ' ',22) AND vtransa()
 @  6,22 GET m.Transa   PICTURE '!!!' VALID Val_para(m.Transa,'TRANSA',' ',22)
 @  7,22 GET m.CodFte  PICTURE '!!' VALID Val_Para (m.CodFte, 'CODFTE', ' ',22)
 @  8,22 GET m.Motivo  PICTURE '!!!!!' VALID Val_para (m.Motivo, 'MOTIVO', ' ',22)
 READ
 SELE parma
 SEEK 'MOTIVO'+m.motivo
 m.tipcla=SUBST(ALLT(parma.descriaux),11,1)
 m.parting=SUBST(ALLT(parma.descriaux),14,9)
 SELE movb
 IF ALLTRIM(m.Tipdoc)='N/C' AND ALLTRIM(m.motivo)#'ABO'
* IF ALLTRIM(m.Tipdoc)='N/C' AND (ALLTRIM(m.motivo)='15' OR ALLTRIM(m.motivo)='14' )
    @ 09,22 GET m.hojcon FUNCTION 'M N,S'
    READ
    IF m.hojcon='S'
	    =carhc()
    ENDIF
	 @ 09,22 SAY IIF(m.hojcon='S',m.numhc+'.'+m.nummeshc,' ')
  	 @ 10,22 GET m.CodCad   PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 
  	 DO vis_calen
 ELSE
  	 @ 10,22 GET m.CodCad   PICTURE '9999' VALID Val_CodCad(m.codcad,m.periodo+'01001',' ',22,47) 
  	 DO vis_calen
 ENDIF	 
  IF m.HojCon ='S'
		m.NumRef = 'H/C N§ '+ m.numhc+m.nummeshc
  ENDIF
 @ 13,22 GET m.numref
 @ 14,22 GET m.Monto   PICTURE '999,999,999,999.99' 
 @ 15,22 GET m.Forma   PICTURE '!!'     VALID Val_para (ALLTRIM(m.Forma) , 'FORMAS', ' ',22) WHEN m.transa='ABO'
 @ 16,22 GET m.Nombre SIZE 1,40
 @ 17,22 GET m.Concept VALID vmesc()
 *@ 18,22 GET m.codpart PICTURE '!!.!!' VALID VAL_PART(SUBSTR(M.CODPART,4,2),LEFT(M.CODPART,2),'C',22) WHEN ALLTRIM(m.tipdoc)='N/C' AND (ALLTRIM(m.motivo)='15' OR ALLTRIM(m.motivo)='14')
 m.estado='00'
 m.usuario=wuser_id
 *@ 19,22 SAY m.tipcla PICTURE "!"
 *@ 19,24 GET m.parting VALID VAL_INGE(SUBSTR(m.parting,11,2),LEFT(m.parting,9),' ') WHEN m.tipcla='S'
 @ 20,22 GET m.nummesC PICTURE '!!'  VALID val_para(m.nummesC ,'FECMES',' ',29,9) 
 @ 20,62 GET m.fechaC VALID(MONTH(m.fechaC)=VAL(m.nummesC))
 READ VALID Val_read()
 *m.codcal  = m.periodo+ALLTRIM(m.NumMes)+ALLTRIM(m.CodFte)+ALLTRIM(vCodPrg)+ALLTRIM(vCodSub)+alltrim(vCodAct)
 SELE MOVB
 IF LASTKEY() = 27
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   DO Vista
   RETURN
 ENDIF
* IF m.asiento#0
DO IngAP
* ENDIF	  
 IF m.tipcla='S' OR m.hojcon='S'  &&Asientos Presupuestales para ingresos y egresos
*     DO compre
 ENDIF    
 IF m.hojcon='S'
	 SELE hoja
	 SEEK m.nummeshc+m.numhc
	 IF FOUND()
	    IF RLOCK()
		    REPLACE Hoja.Estado   WITH '54',;
	    	        Hoja.Nummescp WITH m.nummes,;
	        	    Hoja.Numcp    WITH m.numero,;
	        	    Hoja.codctc   WITH m.codctc
	    ENDIF
	    UNLOCK
	 ENDIF
 ENDIF	 
 SELE MovB       	    
 IF f_appd()
    GATHER MEMVAR
	IF substr(movb.NOMBRE,1,30)=space(30)
	 	repla movb.NOMBRE with Val_para(mOVB.Motivo,'MOTIVO','D',26,22)
	endif		    
 ELSE
    DO standby WITH "Atenci¢n: No grabo el Registro"   
 ENDIF	
 UNLOCK
 SELECT Parma
 * SEEK "CORREL"+ alltrim("MOVBCO")
 SEEK "CORREL"+ "MOVBCO"
 IF RLOCK()
	 REPLACE NumEnt WITH NumEnt + 1
     UNLOCK
 ENDIF	  
 DO vista
RETURN


FUNCTION Valf
*-----------------
PARAMETERS XFEC,XTIP
m.periodo=RIGHT(STR(YEAR(XFEC)),2)
xtip='F'
RETURN .T. 


function val_num
*-----------------
PARAMETERS nmes,nnum
SELE MOVB
SET ORDER TO MovBco11
SEEK ALLTRIM(NMES)+nNUM
IF FOUND()
   WAIT "Error, N£mero de Documento ya existe" WINDOW
   SET ORDER TO MovBco11
   RETURN .F.
ELSE
   SET ORDER TO MovBco11
   RETURN .T.	    	   
ENDIF    
      	

PROCEDURE valfecha
*-----------------
an=RIGHT(STR(YEAR(DATE()),4),2)
vme = VAL(m.nummes)+1
me = PADL(ALLTRIM(STR(vme,2)),2,'0')
vfec = '01/&ME/&AN'
m.fecha = CTOD(vfec) - 1
RETURN .T.

PROCEDURE anula
*--------------
IF EOF()
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF
 
IF Estado = '99'
	DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
	RETURN
ENDIF

SELECT MovB
STORE YESNO('¨Est  seguro que desea Anular esta Transaccion?') TO vElimina
IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	DO CASE
		CASE m.TipDoc='N/C'
			USE AstPat   IN  6 ORDER TAG AstPat5   ALIAS AstPat
		CASE m.TipDoc='N/A'
			USE AstPat   IN  6 ORDER TAG AstPat6   ALIAS AstPat
		CASE m.TipDoc='NOC'
			USE AstPat   IN  6 ORDER TAG AstPat9  ALIAS AstPat
		CASE m.TipDoc='B/D'
			USE AstPat   IN  6 ORDER TAG AstPat7   ALIAS AstPat
	ENDCASE
	SELE ASTPAT
	SEEK ALLTRIM(m.NUMMESc)+ALLTRIM(m.numero)		&&+ALLTRIM(m.CodCtc)
	IF FOUND()
		SCAN WHILE nummes=ALLTRIM(m.nummes) AND numref=m.numero		&& and codctc=m.codctc 
			IF RLOCK() .OR. F_LOCK(1)
				DELETE NEXT 1
			ENDIF
			UNLOCK
		ENDSCAN
	ENDIF
	USE IN 6
	
	DO CASE
		CASE m.TipDoc='N/A'
			USE AstPre   IN 11 ORDER TAG AstPre13  ALIAS AstPre
		CASE m.TipDoc='N/C'
			USE AstPre   IN 11 ORDER TAG AstPre14  ALIAS AstPre
		CASE m.TipDoc='B/D'
			USE AstPre   IN 11 ORDER TAG AstPre15  ALIAS AstPre
		CASE m.TipDoc='NOC'
			USE AstPre   IN 11 ORDER TAG AstPre16 ALIAS AstPre
	ENDCASE
	
	SELE astpre
	SEEK ALLTRIM(m.NUMMESc)+ALLTRIM(m.numero)+ALLTRIM(m.CodCtc)
	IF FOUND()
		SCAN WHILE nummes=ALLTRIM(m.nummes) AND numref=m.numero and codctc=m.codctc 
			IF RLOCK() .OR. F_LOCK(1)
				DELETE NEXT 1
			ENDIF
			UNLOCK
		ENDSCAN
	ENDIF
	USE IN 11
	UNLOCK all
	
	SELE MOVB
	REPLACE ESTADO WITH '99'
*	GO BOTT
ENDIF
DO vista	
RETURN

PROCEDURE Lista
*--------------
 SELECT MovB
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 vTemp = RECNO()
DEFINE WINDOW lis FROM 4,10 TO 22,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vestado,vlista,VREPO
STORE SPACE(50) TO vTitulo
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes,vcon
STORE SPACE(4)  TO vnum
STORE SPACE(3)  TO Vdoc
STORE DATE() TO vfecini, vfecfin
vmes=m.nummesc
Vnum=m.numero
Vcta=m.codctc
@ 01,01 SAY "     Tipo Listado : " GET vREPO  FUNCTION '^ Documento;Por Cta.Cte;Para Extracto Banc.;Por Concepto;Por Tip.Doc'
@ 05,01 SAY "     N§ Documento : "
@ 05,26 SAY "."
@ 05,22 GET vnum WHEN vrepo=1
@ 05,27 GET vmes WHEN vrepo=1
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta  VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,7,22,'descri',35)
@ 09,01 SAY "         Concepto : "
@ 09,22 GET vcon  PICTURE '!!' VALID Val_paraD (vcon, 'MOTIVO', ' ',22) WHEN vrepo=4
@ 11,01 SAY "Tipo de Documento : "
@ 11,22 GET vdoc  PICTURE '!!!' VALID Val_paraD (vdoc, 'MOVBCO', ' ',22) WHEN vrepo=5
@ 13,01 SAY " Fecha de Emisi¢n : " 
@ 13,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7    WHEN vrepo # 1
@ 13,34 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7  WHEN vrepo#1
@ 16,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW lis
IF okcancel = 1
 DEACTIVATE WINDOW lis
 SELECT MovB
 xind=sys(3)+".DBF"
 DO CASE

 	CASE Vrepo = 1  		&& por documento.
	 	DO repPRG WITH "LisMovB"," Listado Movimientos de Banco ",2
	CASE VREPO=2  			&& por cta. cte.
		INDEX ON numero+ALLTRIM(nummes) TO (xind) FOR BETWEEN(Fecha,vFecIni,vFecFin) ;
				AND Codctc=ALLTRIM(Vcta) 
		 IF EOF()
	    	 DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
			 CLOSE INDEX
			 ERASE (XIND)
			 SELE MOVB
		     GOTO vTemp
		     RETURN
		 ENDIF   
		 DO REPORTE WITH 2,"MovBco","Movimientos Bancarios",1
		 CLOSE INDEX
		 ERASE (XIND)
	CASE vrepo = 3 		&& para extracto bancario
		INDEX ON Fecha TO (xind) FOR BETWEEN(Fecha,vFecIni,vFecFin) ;
				AND Codctc=ALLTRIM(Vcta) AND Tipdoc # 'NOC'
		 IF EOF()
	    	 DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
			 CLOSE INDEX
			 ERASE (XIND)
			 SELE MOVB
		     GOTO vTemp
		     RETURN
		 ENDIF   
		 DO REPORTE WITH 2,"MovBco","Movimientos Bancarios",1
		 CLOSE INDEX
		 ERASE (XIND)
	CASE vrepo = 4 		&& por Concepto
		INDEX ON numero+ALLTRIM(nummes) TO (xind) FOR BETWEEN(Fecha,vFecIni,vFecFin) ;
				AND Codctc=ALLTRIM(Vcta) AND motivo=ALLT(vcon)
		 IF EOF()
	    	 DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
			 CLOSE INDEX
			 ERASE (XIND)
			 SELE MOVB
		     GOTO vTemp
		     RETURN
		 ENDIF   
		 Vtitulo='CONCEPTO ==>  '+Val_para(Motivo,'MOTIVO','V',22)
		 DO REPORTE WITH 2,"MovBco","Movimientos Bancarios",1
		 CLOSE INDEX
		 ERASE (XIND)
	CASE vrepo = 5 		&& por Tipo de Documento
		INDEX ON numero+ALLTRIM(nummes) TO (xind) FOR BETWEEN(Fecha,vFecIni,vFecFin) ;
				AND Codctc=ALLTRIM(Vcta)  AND tipdoc=ALLT(vdoc)
		 IF EOF()
	    	 DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
			 CLOSE INDEX
			 ERASE (XIND)
			 SELE MOVB
		     GOTO vTemp
		     RETURN
		 ENDIF   
		 Vtitulo='DOCUMENTO ==>  '+Val_para(Tipdoc,'MOVBCO','V',22) 
		 DO REPORTE WITH 2,"MovBco","Movimientos Bancarios",1
		 CLOSE INDEX
		 ERASE (XIND)
ENDCASE
ENDIF
SELE MOVB
SET ORDER TO movbco11
GOTO vTemp
RETURN


PROCEDURE ingap   && F5
*--------------
Asi_Cen = .F.
DO CASE
   CASE  m.TipDoc='N/C'
		 USE AstPat   IN  6 ORDER TAG AstPat5   ALIAS AstPat
		 w_CtaH = w_CtaD
		 w_CtaD = "210301010000000"
		 Asi_Cen = .T.
   CASE m.TipDoc='N/A'
		 USE AstPat   IN  6 ORDER TAG AstPat6   ALIAS AstPat
   CASE m.TipDoc='NOC'
		 USE AstPat   IN  6 ORDER TAG AstPat9  ALIAS AstPat
   CASE m.TipDoc='B/D'
         USE AstPat   IN  6 ORDER TAG AstPat7   ALIAS AstPat
ENDCASE

SELE ASTPAT
SEEK ALLTRIM(m.NUMMESc)+ALLTRIM(m.numero)			&&+ALLTRIM(m.CodCtc)
IF !FOUND()
	DO agrautbd
*    DO ASIAUT
ENDIF
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummesc)+ALLTRIM(m.numero) FIELDS;
	codcta :H='Cuenta' :v=VALE() .AND. Val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2) AND ctacte(codcta):F,;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	codctc :H='Cta. Cte.':R

STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.NUMMESc)+m.numero		&&+ALLTRIM(m.CodCtc)
mmonto=Mtohab+mtodeb
SCAN WHILE ALLTRIM(m.NUMMESc)=NumMes AND m.numero=NumREF		&& AND m.CodCtc=ALLTRIM(CodCtc)
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	DELE NEXT 1
    	MMONTO = 0
    ELSE
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
USE IN 6
SELECT Movb
sw_z=0
UNLOCK
RETURN



PROCEDURE agrautbd   &&
*-----------------
SELECT astpat
w_tipctc = "CC"
*IF mcta10='S'

IF f_appd()
	REPLACE nummes  WITH m.nummesC,;
	        numref  WITH m.numero,;
	        Periodo WITH m.periodo,;
    	    tipdoc  WITH m.TipDoc,;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FechaC,;
			Tipctc  WITH w_tipctc,;
	        tipcta  WITH 'D',;
	        Codcta  WITH w_ctad,;
	        mtodeb  WITH m.monto
ENDIF
UNLOCK

IF f_appd()
	REPLACE nummes  WITH m.nummesC,;
	        numref  WITH m.numero,;
	        Periodo WITH m.periodo,;
	        tipdoc  WITH m.TipDoc,;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FechaC,;
			Tipctc  WITH w_tipctc,;
	        tipcta  WITH 'H',;
	        Codcta  WITH w_ctaH,;
	        mtoHab  WITH m.monto
ENDIF
UNLOCK

IF Asi_Cen
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
	    	    numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
    		    tipdoc  WITH m.TipDoc,;
	    	    codctc  WITH m.codctc,;
	        	fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'D',;
	    	    Codcta  WITH '110101011900000',;
	        	mtodeb  WITH m.monto
	ENDIF
	UNLOCK
	
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
	    	    Periodo WITH m.periodo,;
	        	tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
	        	tipcta  WITH 'H',;
	    	    Codcta  WITH '110101011900000',;
		        mtoHab  WITH m.monto
	ENDIF
	UNLOCK
ENDIF

*ELSE

*IF  m.asiento=4 AND mcond='U'
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*	    	    Periodo WITH m.periodo,;
*	    	    tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*	    	    fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*		        tipcta  WITH 'D',;
*		        Codcta  WITH IIF(m.transa='ABO',w_ctad,mctaf),;
*		        mtodeb  WITH m.monto
*	ENDIF
*
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*	    	    Periodo WITH m.periodo,;
*	    	    tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*		        fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*	   		    tipcta  WITH 'H',;
*	        	Codcta  WITH IIF(m.transa='ABO',mctaf,w_ctad),;
*		        mtoHab  WITH m.monto
*	ENDIF
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*	   		    Periodo WITH m.periodo,;
*	   	    	tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*		        fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*		        tipcta  WITH 'D',;
*	    	    Codcta  WITH '101010101000',;
*		        mtodeb  WITH m.monto
*	ENDIF
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*		        Periodo WITH m.periodo,;
*	    	    tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*	    	    fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*	    	    tipcta  WITH 'H',;
*	        	Codcta  WITH '101010101000',;
*		        mtoHab  WITH m.monto
*	ENDIF
* ELSE	
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*	    	    Periodo WITH m.periodo,;
*	    	    tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*	    	    fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*		        tipcta  WITH 'D',;
*		        Codcta  WITH Mctaf,;
*		        mtodeb  WITH m.monto
*	ENDIF
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*	    	    Periodo WITH m.periodo,;
*	    	    tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*		        fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*   		    tipcta  WITH 'H',;
*	        	Codcta  WITH mctaI,;
*		        mtoHab  WITH m.monto
*	ENDIF
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*		        numref  WITH m.numero,;
*			    Periodo WITH m.periodo,;
*			   	tipdoc  WITH m.TipDoc,;
*			    codctc  WITH m.codctc,;
*			    fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*			    tipcta  WITH 'D',;
*			    Codcta  WITH '101010101000',;
*			    mtodeb  WITH m.monto
*	ENDIF
*	IF f_appd()
*		REPLACE nummes  WITH m.nummesC,;
*			    numref  WITH m.numero,;
*			    Periodo WITH m.periodo,;
*			    tipdoc  WITH m.TipDoc,;
*		        codctc  WITH m.codctc,;
*	    	    fecha   WITH m.FechaC,;
*				Tipctc  WITH w_tipctc,;
*	    	    tipcta  WITH 'H',;
*	        	Codcta  WITH '101010101000',;
*		        mtoHab  WITH m.monto
*	ENDIF
*ENDIF	
*ENDIF	
RETURN

*PROCEDURE agrautbd   &&
*-----------------
SELECT astpat
IF mcta10='S'
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
	    	    tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'D',;
		        Codcta  WITH IIF(m.transa='ABO',w_ctad,'101010101000'),;
		        mtodeb  WITH m.monto
	ENDIF
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
		        tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'H',;
		        Codcta  WITH IIF(m.transa='ABO','101010101000',w_ctad),;
		        mtoHab  WITH m.monto
	ENDIF
ELSE
  	IF  m.asiento=4 AND mcond='U'
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
			        Codcta  WITH IIF(m.transa='ABO',w_ctad,mctaf),;
			        mtodeb  WITH m.monto
			        	
		ENDIF

		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		   		    tipcta  WITH 'H',;
		        	Codcta  WITH IIF(m.transa='ABO',mctaf,w_ctad),;
			        mtoHab  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		   		    Periodo WITH m.periodo,;
		   	    	tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
		    	    Codcta  WITH '101010101000',;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
			        Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		    	    tipcta  WITH 'H',;
		        	Codcta  WITH '101010101000',;
			        mtoHab  WITH m.monto
		ENDIF
	ELSE	
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
			        Codcta  WITH Mctaf,;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
	    		    tipcta  WITH 'H',;
		        	Codcta  WITH mctaI,;
			        mtoHab  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
	    		    Periodo WITH m.periodo,;
	    	    	tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
		    	    Codcta  WITH '101010101000',;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
			        Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		    	    tipcta  WITH 'H',;
		        	Codcta  WITH '101010101000',;
			        mtoHab  WITH m.monto
		ENDIF
	ENDIF	
ENDIF	
RETURN

PROCEDURE agrite     &&
*---------------
SELECT AstPat
w_tipctc = "CC"

IF f_appd()
	REPLACE nummes  WITH m.nummesC,;
	        numref  WITH m.numero,;
	        Periodo WITH m.periodo,;
	        tipdoc  WITH m.TipDoc,;
	        codctc  WITH m.codctc,;
	        fecha   WITH m.FechaC,;
			Tipctc  WITH w_tipctc
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT AstPat
IF RLOCK()
	DELETE NEXT 1
ENDIF
RETURN

PROCEDURE Compre
*---------------
private as
AS=ALIAS()
m.valdeb = m.Monto
m.valhab = m.Monto

m.CtaDeb = PADR("8401"+IIF(m.CodFte="09","0201","0402")+RIGH(m.Cadena,2),15,"0")
m.CtaHab = PADR("8601"+IIF(m.CodFte="09","0201","0402")+RIGH(m.Cadena,2),15,"0")
*m.CtaDeb = PADR(IIF(m.Codfte='13','9510','9507')+m.CodCad,10,"0")
*m.CtaHab = PADR(IIF(m.Codfte='13','9710','9707')+m.CodCad,10,"0")

as = ALIAS()
DO CASE
   CASE  m.TipDoc='N/A'
		 USE astpre   IN  11  ORDER TAG astpre9  ALIAS astpre  
   CASE m.TipDoc='N/C'
		 USE astpre   IN  11  ORDER TAG astpre10 ALIAS astpre  
   CASE m.TipDoc='B/D'
		 USE astpre   IN  11  ORDER TAG astpre11 ALIAS astpre  
   CASE m.TipDoc='NOC'
		 USE astpre   IN  11  ORDER TAG astpre12 ALIAS astpre  
ENDCASE         
SELECT astpre

SELECT astpre
SEEK 'D'+ALLTRIM(m.nummes)+ALLTRIM(m.Numero)+ALLTRIM(M.CTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummes,;
				tipdoc  WITH m.TipDoc,;
				numref  WITH m.Numero ,;
				cuenta  WITH m.ctadeb,;
				ctadeb  WITH m.ctadeb,;
				tipo    WITH 'D',;
				fecref  WITH m.fecha,;
				CodCtc  WITH m.CodCtc;
				CodCad  WITH m.CodCad,;
				periodo WITH m.periodo,;
				valdeb WITH m.valdeb ,;
				ctadeb WITH m.ctadeb ,;
				ctahab WITH SPACE(15),;
				valhab WITH 0
				
*				CodPart WITH m.CodPart
	ENDIF
	UNLOCK
 ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH m.TipDoc,;
		        numref  WITH m.numero ,;
		        cuenta  WITH m.ctadeb,;
		        ctadeb  WITH m.ctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecha,;
		        CodCtc  WITH m.CodCtc;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
	    	    valdeb WITH m.valdeb ,;
		        ctahab WITH SPACE(15),;
	        	valhab WITH 0 
*		        CodPart WITH m.CodPart
	ENDIF
	UNLOCK
ENDIF
	
SEEK 'H'+ALLTRIM(m.nummes)+ALLTRIM(m.numero)+ALLTRIM(M.CTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH m.TipDoc,;
		        numref  WITH m.numero ,;
		        cuenta  WITH m.ctahab,;
		        ctahab  WITH m.ctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecha,;
		        CodCtc  WITH m.CodCtc,;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH m.valhab,;
		        ctadeb WITH SPACE(15)
*		        CodPart WITH m.CodPart
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummes,;
		        tipdoc  WITH m.TipDoc,;
		        numref  WITH m.numero ,;
		        cuenta  WITH m.ctahab,;
		        ctahab  WITH m.ctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecha,;
		        CodCtc  WITH m.CodCtc,;
		        CodCad  WITH m.CodCad,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH m.valhab,;
		        ctadeb WITH SPACE(15)
*		        CodPart WITH m.CodPart
	ENDIF
	UNLOCK
ENDIF	
USE IN 13
ACTIVATE WINDOW wind_6
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
SELE (as)
RETURN


*PROCEDURE Compre
*---------------
IF VAL(m.nummes)<=3
   DO CASE
      CASE m.hojcon='S'
		   vCtaDeb = '9220100000'
		   vCtaHab = '9320100000'
      CASE m.tipcla='S'
		   IF m.Codfte='PRP'
		      vCtaDeb = '9120101000'
		      vCtaHab = '9020101000'
		   ELSE   
			  vCtaDeb = '9140101000'
		      vCtaHab = '9040101000'
		   ENDIF
	ENDCASE	   
ELSE   
   DO CASE
      CASE m.hojcon='S'
		   vCtaDeb = IIF(m.Codfte='PRP','9030101020','9030101040')
		   vCtaHab = IIF(m.Codfte='PRP','9040101020','9040101040')
      CASE m.tipcla='S'
		   vCtaDeb = IIF(m.Codfte='PRP','9020101020','9020101040')
		   vCtaHab = IIF(m.Codfte='PRP','9010101020','9010101040')
   ENDCASE	   
ENDIF   

as=ALIAS()
DO CASE
   CASE  m.TipDoc='N/C'
		 USE astpre   IN  11  ORDER TAG astpre9  ALIAS astpre  
   CASE m.TipDoc='N/A'
		 USE astpre   IN  11  ORDER TAG astpre10 ALIAS astpre  
   CASE m.TipDoc='NOC'
		 USE astpre   IN  11  ORDER TAG astpre12 ALIAS astpre  
   CASE m.TipDoc='B/D'
		 USE astpre   IN  11  ORDER TAG astpre11 ALIAS astpre  
ENDCASE         
SELECT astpre

SEEK 'D'+ALLTRiM(m.nummesC)+ALLTRiM(m.numero)+ALLTRiM(vctadeb)+ALLTRiM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE Periodo WITH m.Periodo,;
		        nummes  WITH m.nummesc,;
				tipdoc  WITH m.tipdoc,;
				numref  WITH m.numero ,;
				cuenta  WITH vctadeb,;
				tipo    WITH 'D',;
				fecref  WITH m.fechac,;
				CodCtc  WITH ALLTRiM(m.CodCtc),;
				CodCal  WITH m.CodCal,;
				codpart WITH IIF(m.hojcon='S',m.codpart,m.parting),;
                ctadeb  WITH vctadeb ,;
				ctahab  WITH SPACE(10),;
				valdeb  WITH m.monto ,;
				valhab  WITH 0
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE Periodo WITH m.Periodo,;
		        nummes  WITH m.nummesc,;
				tipdoc  WITH m.tipdoc,;
				numref  WITH m.numero ,;
				cuenta  WITH vctadeb,;
				tipo    WITH 'D',;
				fecref  WITH m.fechac,;
				CodCtc  WITH ALLTRiM(m.CodCtc),;
				CodCal  WITH m.CodCal,;
				codpart WITH IIF(m.hojcon='S',m.codpart,m.parting),;
                ctadeb  WITH vctadeb ,;
				ctahab  WITH SPACE(10),;
				valdeb  WITH m.monto ,;
				valhab  WITH 0
	ENDIF
	UNLOCK
ENDIF	


SEEK 'H'+ALLTRiM(m.nummes)+ALLTRiM(m.numero)+ALLTRiM(vctahab)+ALLTRiM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE Periodo WITH m.Periodo,;
				nummes  WITH m.nummesc,;
				tipdoc  WITH m.tipdoc,;
				numref  WITH m.numero ,;
				cuenta  WITH vctahab,;
				tipo    WITH 'H',;
				fecRef  WITH m.fechac,;
				CodCtc  WITH ALLTRiM(m.CodCtc),;
				CodCal  WITH m.CodCal,;
			    codpart WITH IIF(m.hojcon='S',m.codpart,m.parting),;
				ctadeb  WITH SPACE(10),;
				ctahab  WITH vctahab ,;
				valdeb  WITH 0 ,;
				valhab  WITH m.monto
	ENDIF
	UNLOCK
ELSE		
	IF RLOCK()
		REPLACE Periodo WITH m.Periodo,;
				nummes  WITH m.nummesc,;
				tipdoc  WITH m.tipdoc,;
				numref  WITH m.numero ,;
				cuenta  WITH vctahab,;
				tipo    WITH 'H',;
				fecRef  WITH m.fechac,;
				CodCtc  WITH ALLTRiM(m.CodCtc),;
				CodCal  WITH m.CodCal,;
			    codpart WITH IIF(m.hojcon='S',m.codpart,m.parting),;
				ctadeb  WITH SPACE(10),;
				ctahab  WITH vctahab ,;
				valdeb  WITH 0 ,;
				valhab  WITH m.monto
	ENDIF
	UNLOCK
ENDIF	
USE IN 11
SELE (as)
ACTIVATE WINDOW wind_6
@ 00,08  SAY 'Cuentas '
@ 00,18  SAY 'Debe '
@ 00,34  SAY 'Haber '
@ 01,04  SAY vctadeb PICTURE '!!!!!!!!!!' 
@ 01,18  SAY m.monto  PICTURE '999,999,999.99' 
@ 02,12  SAY vctahab PICTURE '!!!!!!!!!!' 
@ 02,34  SAY m.monto  PICTURE '999,999,999.99' 
WAIT ' '
DEACTIVATE WINDOW wind_6
vValdeb = m.monto  
vValhab = m.monto 
RETURN

PROCEDURE lismovB &&Programa Reporte usa repprg F.M.V.
*--------------
PARAMETERS XCOP
PRIVATE FILA,LFORMA
LFORMA=SPACE(20)
DO CASE
	CASE _DEST = 'PANTALLA'
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST = 'IMPRESORA'
		SET DEVICE TO PRINTER
	CASE _DEST = 'ARCHIVO'
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
FOR I=1 TO XCOP
	STORE 0 TO FILA
	IF m.TipDoc='N/C'
		USE AstPat   IN  6 ORDER TAG AstPat5   ALIAS AstPat
		SELE ASTPAT
	ENDIF
	IF m.TipDoc='N/A'
		USE AstPat   IN  6 ORDER TAG AstPat6   ALIAS AstPat
		SELE ASTPAT
	ENDIF
	IF m.TipDoc='NOC'
		USE AstPat   IN  6 ORDER TAG AstPat9  ALIAS AstPat
		SELE ASTPAT
	ENDIF
	IF m.TipDoc='B/D'
		USE AstPat   IN  6 ORDER TAG AstPat7   ALIAS AstPat
		SELE ASTPAT
	ENDIF
	SELE MOVB
	vMES=alltrim(nummesC)
	vnum=numERO
	vcta=alltrim(codctc)
	Vkey= ALLTRIM(MOVB.NUMMESC)+MOVB.NUMERO+ALLTRIM(MOVB.CODCTC)
	@0,0 SAY CHR(18)
	@1,3   SAY ALLTRIM(CIA)
	@1,68  say "PAG:"
	@1,76  SAY ALLTRIM(STR(_PAGENO,8))
	@2,3   SAY "LisMovB"
	@2,68 SAY "FECHA:"
	@2,76 SAY DATE()           
	@4,20 SAY CHR(14)
	@4,22 SAY "<< MOVIMIENTO DE BANCOS>>"
	@4,63 SAY CHR(27)+CHR(18)
	@5,3 SAY "Ú"
	@5,4 SAY REPLICATE("Ä",61)
	@5,65 SAY "¿" 
	FILA=FILA+1
	@6,3 SAY "³"
	@6,4 SAY CHR(14)
	@6,10 SAY IIF(M.TIPDOC='B/D',"<< BOLETA DE DEPOSITO>>",IIF(M.TIPDOC='N/A',"<<NOTA DE ABONO>>",IIF(M.TIPDOC='N/C',"<<NOTA DE CARGO>>","<<NOTA DE CONTABILIDAD>>")))
	@6,35 SAY CHR(27)+CHR(18)
	@06,38 SAY "     N§ "+M.TIPDOC+":"+vnum+"."+VMES  
	@6,65 SAY "³"
	@7,3 SAY "À"
	@7,4 SAY REPLICATE("Ä",61) 
	@7,65 SAY "Ù"
	@08,03 SAY "   Fecha Doc :"  
	@08,20 SAY FECHA picture "@D"
	@09,03 SAY "         Son :"  
	@09,20 SAY LETRAS(M.MONTO,'SOLES')
	@10,03 SAY "      Nombre :"  
	@10,20 SAY m.nombre SIZE 1,40
	@11,03 SAY "      Motivo :"  
	@11,20 SAY Val_para(m.Motivo,'MOTIVO','D',26,22)
	@12,03 SAY "       Banco :"  
	@12,20 SAY Val_para(m.CodBan,'BANCOS','D',26,22)
	@13,03 SAY "     Cta cte :"  
	@13,20 SAY codctc
	@13,35 SAY val_fun('Caja','codctc','Descri',codctc)
	@14,03 SAY "     Fte Fto :"
	@14,20 SAY codfte
	@14,25 SAY alltrim(val_para(CODFTE,'CODFTE','D',26,50))
	@15,03 SAY "Cad.Funcional:"
	@15,20 SAY  codcad
	IF !EMPTY(CODCAD) 
		@15,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
		@16,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
	ENDIF
	*@15,20 SAY substr(CodCal, 8,2)
	*@15,25 SAY val_para(substr(CodCal, 8,2),'CODPRG','D',26,40)
	*@16,03 SAY "Subprograma  :"
	*@16,20 SAY substr(codcal,10,3)
	*@16,25 SAY Val_SUBP(substr(codcal,10,3),'CODSUB'+substr(codcal,8,2)+'    ','D',22,50)
	*@17,03 SAY IIF(empty(substr(codcal,13,3)),' ','  Actividad  :')
	*@17,20 SAY IIF(empty(substr(codcal,13,3)),' ',substr(codcal,13,3))
	*@17,25 SAY IIF(empty(substr(codcal,13,3)),' ',VAL_PYAC(alltrim(substr(codcal,13,3)),Periodo+substr(CodCal,8,5),'D',22,50))
	@18,03 SAY "    Concepto :"
	@18,20 SAY m.concept
	@18,72 SAY "Monto" 
	LFORMA=IIF(M.FORMA='EF',"EFECTIVO",IIF(M.FORMA='CHQ',"CHEQUE","CON BOLETA DE DEP."))
	@19,02 SAY IIF(M.TIPDOC='B/D',"Forma de Pago :",' ')
	@19,20 SAY IIF(M.TIPDOC='B/D',LFORMA," ") 
	@19,60 SAY m.monto PICTURE "@z 999,999,999,999.99"             
	fila=22
	@FILA,25 SAY "<< CONTABILIDAD PATRIMONIAL >>"
	FILA=FILA+1
	@FILA,08 SAY "Cuenta D"
	@FILA,32 SAY "Importe"
	@FILA,46 SAY "Cuenta H"
	@FILA,72 SAY "Importe"
	SELE ASTPAT
	SEEK VKEY
	FILA=FILA+1
	SCAN WHILE NUMMES=ALLTRIM(Vmes) AND NUMREF=vnum 		&& AND CODCTC=ALLTRIM(VCTA)
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
	@FILA,1 SAY "USUARIO:"
	@FILA,20 SAY desuse
	USE IN 6
ENDFOR

IF _DEST = 'IMPRESORA'
	EJECT
ENDIF
SET DEVICE TO SCREEN

RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU
  RETURN


PROCEDURE Fin_opcion
*-------------------
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    MENU   mMenu
  CLOSE DATA
  ON KEY
  RETURN


FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)

   
FUNCTION vmesc  &&Fecha Contable
*-------------
 m.nummesc=m.nummes
 m.fechac =m.fecha
RETURN

PROCEDURE cor_as  && F9
*--------------
SELE caja
SEEK m.codctc
w_tipctc=caja.tipo
ingreso = .F.
SELE movb
IF m.TipDoc='N/C'
   USE AstPat   IN  6 ORDER TAG AstPat5   ALIAS AstPat
   SELE ASTPAT
   SEEK ALLTRIM(m.NUMMES)+ALLTRIM(m.numero)		&&+ALLTRIM(m.CodCtc)
ENDIF
IF m.TipDoc='N/A'
   USE AstPat   IN  6 ORDER TAG AstPat6   ALIAS AstPat
   SELE ASTPAT
   SEEK ALLTRIM(m.NUMMES)+ALLTRIM(m.numero)		&&+ALLTRIM(m.CodCtc)
ENDIF
IF m.TipDoc='NOC'
   USE AstPat   IN  6 ORDER TAG AstPat9  ALIAS AstPat
   SELE ASTPAT
   SEEK ALLTRIM(m.NUMMES)+ALLTRIM(m.numero)		&&+ALLTRIM(m.CodCtc)
ENDIF
IF m.TipDoc='B/D'
   USE AstPat   IN  6 ORDER TAG AstPat7   ALIAS AstPat
   SELE ASTPAT
   SEEK ALLTRIM(m.NUMMES)+ALLTRIM(m.numero)		&&+ALLTRIM(m.CodCtc)
ENDIF
IF !FOUND()
	DO agrite
ENDIF
ON KEY LABEL F5 DO AgrIte
ON KEY LABEL F8 DO EliIte
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numero+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=VALE() .AND. val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2) AND ctacte(codcta) :F,;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99',;
	Codctc :H='Cta.Cte':R
	
STORE 0 TO vdebe, vhaber ,vRet
SEEK ALLTRIM(m.NUMMES)+m.numero+ALLTRIM(m.CodCtc)
mmonto=Mtohab+mtodeb
SCAN WHILE ALLTRIM(m.NUMMES)=NumMes AND m.numero=NumREF AND m.CodCtc=ALLTRIM(CodCtc)
    IF IIF(TipCta='D',MtoDeb,MtoHab)=0
    	dele next 1
    	MMONTO = 0
    ELSE
		vdebe = vdebe  + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
    ENDIF
    IF LEFT(Astpat.Codcta,3)='101'
    ENDIF
ENDSCAN
IF vdebe#vhaber
   DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
if m.tipdoc<>'B/D'
   if ingreso 
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numero,tipdoc WITH M.TIPDOC,codctc WITH m.codctc,fecha WITH m.FecHA,TIPCTC WITH w_tipctc
		REPLACE codcta WITH '110101010100000',Mtodeb WITH mmonto, Tipcta WITH 'D'
	ENDIF
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numero,tipdoc WITH M.TIPDOC,codctc WITH m.codctc,fecha WITH m.FecHA,TIPCTC WITH w_tipctc
		REPLACE codcta WITH '110101010100000',Mtohab WITH mmonto, Tipcta WITH 'H'
	ENDIF
   else 
	* si es corrije solo modifica
	SEEK ALLTRIM(m.nummes)+m.numero+alltrim(m.codctc)
	IF FOUND() 
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numero=numref AND m.codctc=alltrim(codctc)
		    if codcta='110101010100000' and tipcta='D'
		        IF RLOCK()
	        		repla Mtodeb WITH mmonto
	        	ENDIF	
	        endif
	        if codcta='110101010100000' and tipcta='H'
	        	IF RLOCK()
		        	repla Mtohab WITH mmonto
		        ENDIF	
	       endif
		ENDSCAN
	ENDIF	
endif	
endif	
ON KEY
UNLOCK
USE IN 6
SELECT Movb
DO vista
RETURN


FUNCTION vctc
*------------
Vali=ALIAS()
SELE Caja
SEEK ALLT(m.codctc)
IF FOUND()
   w_tipctc =  Caja.Tipo
   m.codfte =  Caja.Codfte
   m.codban =  Caja.Banco
   w_ctaD   =  LEFT(Caja.DocRef,15)
   w_ctaH   =  RIGHT(Caja.DocRef,15)
   @ 01,62 SAY IIF(w_tipctc='CC','SEDE CENTRAL',IIF(w_tipctc='RE','INT.REGIONAL','OTROS PROGRAMAS')) COLOR SCHEME 2
   @  4,22 SAY Val_paraD(m.codban,'BANCOS','V',26,26)
ENDIF
SELE (vali)
RETURN   

PROCEDURE carhc
*--------------
* Carga valores de HC
Cierra =.F.
IF !USED("Auxi")
	USE Auxil IN 0 ORDER TAG Auxil1 ALIAS Auxi
	Cierra = .T.
ENDIF
vfun = .T.
OK   = FOUND()
SELECT hoja
SET FILTER TO (Tipdoc='N/C' OR Tipdoc='O/S') 			&&AND Estado='00'
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
@ 1,16 SAY '.'
@ 1,17 GET vHC DEFAULT SPACE(4)
READ
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILT TO
   RETURN .F.
ENDIF
vMes = PADL(ALLTRIM(vMes),2,'0')
vHC  = PADL(ALLTRIM(vHC),4,'0')
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
     x1=IIF(TIPPRV="O",NOMBRE,IIF(tipprv="P",LEFT(val_fun('Auxi', 'descri', 'descri', codprv), 20),LEFT(val_fun('Auxi', 'descri', 'descri', codemp), 20))) :H="Nombre",;
     x2=TRANSF(imptot,'99,999,999.99'):H="Monto",;
     fechc :H="fecha"

*     x3=codcal+' '+codctc :H="Cal.ctc"
ENDIF
IF LASTKEY()#27
	SELE HOJA
	m.nummeshc = nummes
	m.numhc    = numhc
	m.monto    = imptot
	m.codcad   = codcad
*	m.codcal   = codcal
	m.CodPart  = CodPart
	m.codfte   = codfte
	SELE Hoja
	SHOW GETS
	SET FILT TO
	SELECT MoVB
ENDIF
IF Cierra
	USE IN Auxi
ENDIF

RETURN .T.

FUNCTION vtransa
*---------------
Vali=ALIAS()
SELE Parma
LOCATE FOR tipo='MOVBCO' and ALLT(Codigo)=ALLT(m.tipdoc) 
IF FOUND()
   m.transa=IIF(parma.descriaux='A','ABO',IIF(parma.descriaux='C','CAR','ORD'))	
   @  6,22 SAY Val_para(m.Transa,'TRANSA','V',22)
ENDIF
RETURN   

FUNCTION Val_IngE
*---------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol, mLong
  PRiVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRi
  ENDCASE
  mAlias  = ALIAS()
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVaRiable $'VZ'
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
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRiM(CODING)+'.'+ALLTRiM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng 
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
      @ ROW(),mCol+24  SAY mValor
      @ ROW(),mCol+37 SAY mDescr
      m.parting=mvalor
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    CASE mVaRiable='T'
      &mVaRiable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
    OTHERWISE   
      m.parting= mvalor
      RETURN .T.
  ENDCASE

FUNCTION vdpart
*--------------
PARAMETERS vparting
nali=ALIAS()
SELE ingr
SEEK LEFT(vParting,9)+SUBSTR(vParting,11,2)
IF FOUND()
   vdpart=Ingr.Desing
ENDIF
RETURN vdpart   


PROCEDURE vis_calen   &&Vista Codificaci¢n Funcional Program tica
*-------------------
@ 10,22 SAY m.codcad
@ 10,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 11,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
RETURN

PROCEDURE calen   &&Vista Codificaci¢n Funcional Program tica
*--------------
@ 15,27 SAY " UG  UE  FN PRG SBPRG ACTPRY"
@ 16,28 SAY maepre.uniges+'  '+maepre.unieje+' '+maepre.codfun+' '+maepre.codprg+' '+maepre.codspr+'  '+maepre.actpry
RETURN

PROCEDURE ASIAUT   &&
*-----------------
SELECT astpat
*IF mcta10='S'
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
	    	    tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'D',;
		        Codcta  WITH IIF(m.TIPDOC='N/A',w_ctad,'110101010100000'),;
		        mtodeb  WITH m.monto
		        
*	    Codcta  WITH IIF(m.transa='ABO',w_ctad,'101010101000'),;		        
	ENDIF
	IF f_appd()
		REPLACE nummes  WITH m.nummesC,;
		        numref  WITH m.numero,;
		        Periodo WITH m.periodo,;
		        tipdoc  WITH m.TipDoc,;
		        codctc  WITH m.codctc,;
		        fecha   WITH m.FechaC,;
				Tipctc  WITH w_tipctc,;
		        tipcta  WITH 'H',;
		        Codcta  WITH IIF(m.TIPDOC='N/A','110101010100000',w_ctad),;
		        mtoHab  WITH m.monto
	ENDIF
*ELSE
*  	IF  m.asiento=4 AND mcond='U'
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
			        Codcta  WITH IIF(m.transa='ABO',w_ctad,mctaf),;
			        mtodeb  WITH m.monto
			        	
		ENDIF

		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		   		    tipcta  WITH 'H',;
		        	Codcta  WITH IIF(m.transa='ABO',mctaf,w_ctad),;
			        mtoHab  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		   		    Periodo WITH m.periodo,;
		   	    	tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
		    	    Codcta  WITH '110101010100000',;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
			        Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		    	    tipcta  WITH 'H',;
		        	Codcta  WITH '110101010100000',;
			        mtoHab  WITH m.monto
		ENDIF
	ELSE	
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
			        Codcta  WITH Mctaf,;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
		    	    Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
	    		    tipcta  WITH 'H',;
		        	Codcta  WITH mctaI,;
			        mtoHab  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
	    		    Periodo WITH m.periodo,;
	    	    	tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
			        fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
			        tipcta  WITH 'D',;
		    	    Codcta  WITH '110101010100000',;
			        mtodeb  WITH m.monto
		ENDIF
		IF f_appd()
			REPLACE nummes  WITH m.nummesC,;
			        numref  WITH m.numero,;
			        Periodo WITH m.periodo,;
		    	    tipdoc  WITH m.TipDoc,;
			        codctc  WITH m.codctc,;
		    	    fecha   WITH m.FechaC,;
					Tipctc  WITH w_tipctc,;
		    	    tipcta  WITH 'H',;
		        	Codcta  WITH '110101010100000',;
			        mtoHab  WITH m.monto
		ENDIF
	ENDIF	
*ENDIF	
RETURN

FUNCTION ctacte
*--------------
PARAMETER vcta
valias=alias()
if empty(vcta)
	DO standby WITH 'No tiene Cta. Cte.'
	SELE (valias)
	return .f.
endif

if LEFT(vctA,3)<>'110104'
	SELE (valias)
	return .T.
Else
	SELE caja
	set orde to cajas4
	SEEK vcta
	IF !found()
		DO standby WITH 'Cuenta No existe'
		SELE (valias)	
		return .f.
	ENDIF	
	vcta=ALLTRIM(CODCTC)
	set orde to cajas1
	SELE (valias)
	replace codctc with vcta
	return .T.
ENDIF	
SELE (valias)
replace codctc with M.CODCTC
return .T.

PROCEDURE VALE
*-------------
IF EMPTY(m.codctc) 
   DO STANDBY with 'No esta asignada la Cuenta Corriente'
   return .F.
else
   return .t.
endif

PROCEDURE SubOpc
*---------------
PRIVATE cAlias,cMod
cAlias = ALIAS()
SELE SubOp
cCtrlOp = ''
cMod = "03"

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
set skip of PAD Elimi of mMenu !'G' $cCtrlOp
set skip of PAD Lista of mMenu !'H' $cCtrlOp

SELE (cAlias)

RETURN
