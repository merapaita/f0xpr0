* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ RegCD.PRG  15/05/96                                 L: 1298  ³	
* ³ Registro de Hoja de Cargas Diversas                          ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU         ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae   IN  1  ORDER TAG Parmae1       ALIAS Parma
USE Cajas    IN  2  ORDER TAG cajas1        ALIAS caja
*USE Maereg   IN  5  ORDER TAG Maereg1       ALIAS maereg
USE compag   IN  3  ORDER TAG compag1       ALIAS compag &&C/P
USE IteReg   IN  4  ORDER TAG IteReg1       ALIAS IteReg 
USE hojcon   IN  6  ORDER TAG hojcon1       ALIAS hoja   &&H/C
USE itehc    IN  7  ORDER TAG itehc1        ALIAS itehc 
USE Auxil    IN 15  ORDER TAG Auxil2        ALIAS Auxil
USE clase    IN  8  ORDER TAG clase1        ALIAS clase   
USE MovBco   IN 10  ORDER TAG Movbco11      ALIAS Movb
USE cuentas  IN 14  ORDER TAG cuentas1     ALIAS cuenta
USE astpat   IN 09  ORDER TAG astpat9      ALIAS  astpat &&Egresos
USE reten    IN 12  ORDER TAG reten1  ALIAS reten
	
SELECT MAEREG
GO BOTTOM
*- Variables de trabajo (registro a trabajar)
PUBLIC m.CodCtc,VTOTAL,Vfte,vcal,w_banco,xcodpart,Xctadeb,xctahab,xvaldeb,xvalhab,VUSER_ID,wuser_id
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
Xcodpart=SPACE(5)
Vfte=SPACE(3)
STORE SPACE(22) TO  vcal
VTOTAL=0
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
 DEFINE WINDOW Wind_0 FROM 00,00 TO 11,79 DOUBLE ;
 TITLE 'Regularizaciones -- <Misma Cuenta>' COLOR SCHEME 10

DEFINE WINDOW wind_1 FROM 00,00 TO 24,79  DOUBLE ;
 TITLE 'Revisi¢n de Movimientos     Presione F10 para concluir          ' COLOR SCHEME 10

DEFINE WINDOW wind_2 FROM 12,00 TO 23,79 DOUBLE ;
	TITLE 'F2: Observaciones      Regularizaciones    : Detalle' COLOR SCHEME 10

DEFINE WINDOW wind_3 FROM 20,64 TO 22,78 ;
	TITLE ' LIQUIDO ' COLOR SCHEME 10

DEFINE WINDOW wind_6 FROM 13,10 TO 17,70 ;
	TITLE ' COMPROMISO PRESUPUESTAL ' COLOR SCHEME 10

DEFINE WINDOW wind_7 FROM 13,10 TO 17,70 ;
	TITLE ' CENTRALIZACION DE CAJA ' COLOR SCHEME 10

DEFINE WINDOW Wind_12 FROM 14,11 TO 16,69 ;
TITLE ' Retenci¢n ' 

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
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

 ACTIVATE SCREEN
 RETURN

PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @ 01, 8 SAY "  Secuencia :"
 @ 02, 8 SAY "      Fecha :"
 @ 03, 8 SAY "    Cta.Cte :"
 @ 04, 8 SAY "     N§ H/C :"   
 @ 05, 8 SAY "      Monto :"
 @ 06, 8 SAY "  Retenci¢n :"
 @ 07, 8 SAY "     N§ C/P.:"
 @ 08, 8 SAY "   Concepto :" 
 RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 PARAMETER Vacio
 ACTIVATE WINDOW Wind_0
 SELECT Maereg
 IF !Vacio
    SCATTER MEMVAR
 ENDIF
 @  1,22 SAY m.Numero             PICTURE '9999'
 @  1,29 SAY m.NumMES             PICTURE '99'
 @  2,22 SAY m.FECREG
 @  2,40 say m.periodo
 @ 03,22 SAY m.CodCtc
 @ 04,22 SAY m.numMESHC+'.'+M.NUMHC+'--'+PERHC
 @ 05,22 SAY M.IMPORTE              PICTURE '999,999,999,999.99'
 @ 06,22 SAY M.RETEN              PICTURE '999,999,999,999.99'
 @ 07,22 SAY m.numMESCP+'.'+M.NUMCP+'--'+PERCP
 @ 08,22 SAY m.glosa PICTURE '@S56' 
 DO vista_hijo
 DO TOTAL
 RETURN

PROCEDURE vista_hijo
*-------------------
HIDE POPUP ALL
SELECT IteReg
SEEK ALLTRIM(m.nummes)+m.numero
IF FOUND()
	IF ALLTRIM(m.tipfun)='I'
		BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numero  TIMEOUT 0.001 ;
			WINDOW wind_2 ;
			FIELDS;
			codpart    :H= 'C¢d.Par' ,;
			codanal    :H= 'C¢d.Anl' ,;
			aa=val_part(RIGHT(codanal,2),LEFT(codanal,2),'D') :H='Descripci¢n' :40,;
			impparc    :H= 'Monto' :p='9,999,999.99'
	ELSE
		BROWSE ;
			NOAPPEND NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH NOEDIT KEY ALLTRIM(m.nummes)+m.numero  TIMEOUT 0.001 ;
			WINDOW wind_2 ;
			FIELDS;
			codanal    :H= 'C¢d.Par' ,;
			aa=val_part(RIGHT(codanal,2),LEFT(codanal,2),'D') :H='Descripci¢n' :40,;
			impparc    :H= 'Monto' :p='9,999,999.99'
	ENDIF
ELSE
	DO standby WITH 'No tiene detalle'
ENDIF
SELE maereg
SET RELATION OFF INTO Hoja
RETURN


PROCEDURE Revis
*--------------
SELECT maereg
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 ACTIVATE SCREEN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE  WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU
 ON KEY LABEL F10
 DO vista
 DO total
 RETURN


PROCEDURE Busca
*--------------
 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF
 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 09,12 TO 13,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(4) TO Vbusca
 STORE SPACE(2) TO VANOB
 @ 1,43 say "."
 @ 1,3 SAY 'Digite N£mero de Movim. que desea :' GET Vbusca PICT '@!' 
 @ 1,44 GET VANOB PICT '@!' 
 READ
 RELEASE WINDOW WBusca
 IF EMPTY(Vbusca) .OR. LASTKEY()=27
   GOTO Vtemp
 ELSE
   SEEK VANOB+Vbusca
   IF .NOT. FOUND()
     DO standby WITH "Dicha Regularizaci¢n no fue encontrada"
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
 SELECT maereg
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
 SELECT maereg
 IF !EOF()
   SKIP
 ENDIF
 IF EOF()
   DO standby WITH "Fin de archivo: no existe regularizaci¢n siguiente."
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
 ACTIVATE WINDOW Wind_0
 INGRESO=.F.
 @  1,22 GET m.Numero  PICTURE "9999" WHEN .F.
 @  1,29 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',29,9) AND valfecha() DISABLE
 @  2,22 GET m.FECREG  VALID(MONTH(m.fecReg)=VAL(m.nummes)) AND VALF(M.FECREG)
 @  2,40 GET m.periodo DISABLE
 @ 03,22 GET m.CodCtc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.CodCtc,1,11,22) DISABLE
 @ 04,29 GET m.nummeshc
 @ 04,31 SAY '.'
 @ 04,32 GET m.numhc  VALID DISP()
 @ 04,36 SAY '--'
 @ 04,38 GET M.perhc
 @ 05,22 GET M.IMPORTE   PICTURE '999,999,999,999.99' 
 @ 06,22 GET M.reten   PICTURE '999,999,999,999.99' 
 @ 07,29 GET m.nummesCP
 @ 07,31 SAY '.'
 @ 07,32 SAY m.numCP
 @ 07,36 SAY '--'
 @ 07,38 SAY m.perCP
 @ 08,22 GET m.GLOSA PICTURE '@S56' 
 M.ESTADO='00'
 READ 
 m.usuario=vuser_id
 SELECT caja
 SEEK ALLTRIM(m.codctc)
 w_tipctc=CAJA.TIPO
 w_banco =CAJA.banco
 IF !EMPTY(m.reten)
  	ACTIVATE window wind_12
   	@0,1 SAY "Partida" GET m.partret picture '!!.!!' VALID VAL_PRET(SUBSTR(M.partret,4,2),LEFT(M.partret,2),'C',22) 
    @0,30 SAY "Reten. " +str(m.reten,10,2) 
   	READ
 ENDIF	
 DEACTIVATE WINDOW wind_12
 IF !EMPTY(m.numhc)
	SELECT hoja
	SEEK m.nummeshc+m.numhc
	IF FOUND() AND RLOCK()
		REPLACE nummescp WITH m.nummes, numcp WITH m.numERO,;
		CODCTC WITH M.CODCTC, ESTADO WITH '52',;
		reten WITH m.reten, partret WITH m.partret
	ENDIF
	SELECT compag
	SEEK ALLTRIM(m.nummescp)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND() AND RLOCK()
		REPLACE conaaff WITH '='
		REPLACE partret WITH m.partret
	ENDIF
	SELECT RETEN
	SEEK ALLTRIM(m.nummescp)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND() AND RLOCK()
		SCAN WHILE ALLT(Reten.Nummes)+Reten.NumcP+ALLT(Reten.Codctc)=ALLTRIM(m.nummescp)+m.numcp+ALLTRIM(m.codctc)
		   REPLACE partret WITH m.partret, nummeshc WITH m.nummeshc, numhc WITH m.numhc,CODCAL WITH VCAL
		ENDSCAN   
	ENDIF   
	 DO ingap
	 DO trabaja_hijo
	 total = 0
	 DO Total_padre
	 do compre
	 SELE maereg
	 IF LASTKEY() <> 27 .AND. yesno("Confirme Correcciones")         && Si se presion¢ Esc, no graba
	    SELECT maereg
	    GATHER MEMVAR
	 ENDIF
 ENDIF	 
 DO vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SELECT maereg
 DO pantalla
 SCATTER MEMVAR BLANK
 SELECT Parma
 SEEK "CORREL"+"MAEREG"
 m.numero=PADL(nument+1,4,'0')
 SEEK "CORREL"+"MOVBCO"
 m.numnoc=PADL(nument+1,4,'0')
 STORE  RIGHT(STR(YEAR(DATE()),4),2) TO m.periodo,m.perhc,m.percp
 ingreso=.T.
 @  1,22 SAY m.Numero  PICTURE "9999" 
 @  1,29 GET m.nummes  PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',29,9) AND valfecha() AND  VAL_NUM(M.NUMMES,M.NUMERO)
 @  2,22 GET m.fecreg VALID(MONTH(m.fecReg)=VAL(m.nummes)) AND Valf(m.fecreg)
 @ 2,40  GET m.periodo
 @ 03,22 GET m.CodCtc  VALID Val_fun('Caja','CodCtc','codctc+descri',m.CodCtc,1,11,22)
 OI=carhc()  &&Captura H/C
 @ 04,22 SAY m.nummeshc+'.'+m.numhc+'--'+m.perhc
 @ 05,22 GET m.importe   PICTURE '999,999,999,999.99' 
 @ 06,22 GET m.reten   PICTURE '999,999,999,999.99' 
 OJ=carCP()  &&Captura C/P
 @ 07,22 SAY m.nummescp+'.'+m.numcp+'--'+m.percp
 @ 08,22 GET m.glosa PICTURE '@S56' 
 m.estado='00'
 READ VALID Val_read()
 m.usuario=vuser_id
 SELECT caja
 SEEK ALLTRIM(m.codctc)
 w_tipctc=caja.tipo
 w_banco =Caja.Banco
 IF LASTKEY() = 27
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   DO Vista
   RETURN
 ENDIF
 IF !EMPTY(m.reten)
  	ACTIVATE window wind_12
   	@0,1 SAY "Partida" GET m.partret picture '!!.!!' VALID VAL_PRET(SUBSTR(M.partret,4,2),LEFT(M.partret,2),'C',22) 
    @0,30 SAY "Reten. " +str(m.reten,10,2) 
   	READ
 ENDIF	
 DEACTIVATE WINDOW wind_12
 IF !EMPTY(m.numhc)
	SELECT hoja
	SEEK m.nummeshc+m.numhc
	IF FOUND() AND RLOCK()
		REPLACE nummescp WITH m.nummes, numcp WITH m.numERO,;
		 codctc WITH m.codctc, estado WITH '52',;
		reten WITH m.reten, partret WITH m.partret
		ENDIF
	ENDIF
	SELECT compag
	SEEK ALLTRIM(m.nummescp)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND() AND RLOCK()
		REPLACE conaaff WITH '='
		REPLACE partret WITH m.partret
	ENDIF
	SELECT RETEN
	SEEK ALLTRIM(m.nummescp)+m.numcp+ALLTRIM(m.codctc)
	IF FOUND() AND RLOCK()
		SCAN WHILE ALLT(Reten.Nummes)+Reten.NumcP+ALLT(Reten.Codctc)=ALLTRIM(m.nummescp)+m.numcp+ALLTRIM(m.codctc)
		   REPLACE partret WITH m.partret, nummeshc WITH m.nummeshc, numhc WITH m.numhc, CODCAL with VCAL
		ENDSCAN   
	ENDIF   
    SELECT maereg
    INGRESO = .T.
    DO ingap
    DO trabaja_hijo
	vtotal = 0
    DO Total_padre
    DO compre
    SELE MAEREG
	 If f_appd()
	    gather memvar
	 else
	    do standby with "Atenci¢n: No grab¢ el Registro"   
	 endif	
	 UNLOCK
	 SELECT Parma
	 SEEK "CORREL"+"MAEREG"
	 IF RLOCK()
		 REPLACE NumEnt WITH NumEnt + 1
		  UNLOCK
	 ENDIF	  
DO vista
RETURN


FUNCTION VALF
*-----------------
PARAMETERS XFEC
m.periodo=RIGHT(STR(YEAR(XFEC)),2)
RETURN 


function val_NUM
*-----------------
PARAMETERS nmes,nnum
SELE maereg
SET ORDER TO maereg1
SEEK ALLTRIM(NMES)+nNUM
IF FOUND()
   WAIT "Error, N£mero de Documento ya existe" WINDOW
   SET ORDER TO maeregCO1
   RETURN .F.
ELSE
   SET ORDER TO maereg1
   RETURN .T.	    	   
ENDIF    
      	

PROCEDURE valfecha
*-----------------
an=RIGHT(STR(YEAR(DATE()),4),2)
vme = VAL(m.nummes)+1
me = PADL(ALLTRIM(STR(vme,2)),2,'0')
vfec = '01/&ME/&AN'
M.FECREG = CTOD(vfec) - 1
RETURN .T.

PROCEDURE Elimi
*--------------
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 SELECT maereg
 STORE YESNO('¨Est  seguro que desea ELIMINAR esta Transaccion?') TO vElimina
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
    dele next 1
    SKIP
    DO Vista
 ENDIF
 SELE maereg
 RETURN
 
 

PROCEDURE Lista
*--------------
 SELECT maereg
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 vTemp = RECNO()
DEFINE WINDOW lis FROM 4,10 TO 18,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1  TO vtocta,vestado,vlista,VREPO
STORE SPACE(14) TO vCta,vCuenta
STORE SPACE(2)  TO vAno,vMes
STORE SPACE(4)  TO vCli
STORE DATE() TO vfecini, vfecfin
vMes=m.nummes
vCli=m.numero
vCta=m.codctc
@ 01,01 SAY "     Tipo Listado : " GET vREPO  FUNCTION '^ Documento;Por Cta.Cte;Cargas Diversas'
@ 05,01 SAY "     N§ Documento : "
@ 05,26 SAY "."
@ 05,22 GET vcli WHEN vrepo=1
@ 05,27 GET vmes WHEN vrepo=1
@ 07,01 SAY "   Cta. Corriente : "
@ 07,22 GET vcta  VALID val_fun('Caja', 'CodCtc', "CodCtC+' '+Descri",vcta,1,06,07) WHEN VREPO=2 or vrepo=3
@ 09,01 SAY " Fecha de Emisi¢n : " 
@ 09,22 GET vfecini  PICTURE '@D' COLOR SCHEME 7    WHEN VREPO=2 or vrepo=3
@ 09,32 GET vfecfin  PICTURE '@D' VALID (vfecfin >= vfecini) COLOR SCHEME 7  WHEN VREPO=2 or vrepo=3
@ 12,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis
IF okcancel = 1
	 DEACTIVATE WINDOW lis
	 SELECT maereg
	 xind=sys(3)+".IDX"
	 yind=sys(3)+".DBF"
	 zind=sys(3)+".IDX"
	 wind=sys(3)+".IDX"
	 DO CASE
	   CASE VREPO=1
		   DO repPRG WITH "LisCarDi"," Lista de Cargas Diversas ",2
		CASE VREPO=2
		   INDEX ON NUMERO+ALLTRIM(NUMMES) TO (xind) FOR BETWEEN(FecREG,vFecIni,vFecFin) AND CODCTC=ALLTRIM(VCTA) 
		   IF EOF()
		      DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
		      CLOSE INDEX
			  ERASE (XIND)
			  SELE maereg
		      GOTO vTemp
		      RETURN
		   ENDIF   
	       DO REPORTE WITH 2,"Lisregd","Listado de Regularizaciones",1
		   CLOSE INDEX
		   ERASE (XIND)
	   CASE VREPO=3
	       SELE COMPAG
	       INDEX ON ALLTRIM(mesaaff)+numcpaf+ALLTRIM(codctcaf) TO (zind) FOR tipdoc='RG'
	       COPY TO (yind)
	       SET INDEX TO
	       USE (yind) IN 0 ALIAS reg
	       SELE reg
	       INDEX ON ALLTRIM(mesaaff)+numcpaf+ALLTRIM(codctcaf) TO (wind)
	       SELE COMPAG
	       INDEX ON ALLTRIM(nummes)+numcp+ALLTRIM(codctc) TO (xind) FOR prestamo='S' AND BETWEEN(FecCp,vFecIni,vFecFin) AND CODCTC=ALLTRIM(VCTA) 
	       IF EOF()
	          DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
		      CLOSE INDEX
			  ERASE (XIND)
			  SELE maereg
			  GOTO vtemp
			  RETURN
	        ENDIF	
	        SELE Maereg
	        _xord=ORDER()
	        SET ORDER TO Maereg2
		    DO repPRG WITH "Lisaaff"," Conciliaci¢n Asignaci¢n Financiera ",2
		    SELE Maereg
		    SET ORDER TO _xord
	ENDCASE        
 ENDIF
 SELE maereg
 GOTO vTemp
 RETURN

PROCEDURE LISAAFF	 &&Conciliaci¢n de Asignaciones Financieras
*-----------------
PARAMETERS xcop
PRIVATE fila,xtot,vtot,tsdo,taf,tre,tsd,x_reten,x_partret
STORE 0 TO xtot,vtot,tsdo,taf,tre,tsd,x_reten
STORE SPACE(5) TO X_PARTRET
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
		SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	

DO imp_header
	STORE 0 TO Fila
	fila = 7
	SELE compag
	GO TOP
	nmes=ALLTRIM(Compag.nummes)
	ncp=Compag.numCp
	ncta=ALLTRIM(Compag.codctc)
	SCAN 
		nmes=ALLTRIM(Compag.nummes)
		ncp=Compag.numCp
		ncta=ALLTRIM(Compag.codctc)
		xtot=Compag.import
	    x_reten=compag.reten
		SELE Maereg  &&=
        @ Fila,02 SAY nmes+"."+ncp
        @ fila,10 SAY compag.feccp PICTURE "@D"
        @ Fila,19 SAY LEFT(compag.glosa,40)
        @ Fila,60 SAY xtot PICTURE "999,999,999,999.99"
	    Fila=fila+1
        @ Fila,19 SAY SUBSTR(compag.glosa,41,40)
	    Fila=fila+1
		SEEK nmes+ncp+ncta
	    vtot = 0
	    tsdo = 0
		IF FOUND()
			SCAN WHILE nummescp=nmes AND numcp=ncp AND codctc=ncta
			    @ fila,79   SAY "="
				@ fila,80   SAY  nummes+'.'+numero
				@ fila,89   SAY  nummeshc+'.'+numhc
		        @ fila,97   SAY LEFT(Maereg.glosa,40)
				@ fila,139  SAY  importe PICTURE "999,999,999,999.99"
			    Fila=fila+1
		        @ Fila,97   SAY SUBSTR(Maereg.glosa,41,39)
		        vtot=vtot + importe
			ENDSCAN
			SELE COMPAG
	    ENDIF
		SELE reg &&#
		SEEK nmes+ncp+ncta
		IF FOUND()
			SCAN WHILE reg.mesaaff=nmes AND reg.numcpaf=ncp AND reg.codctcaf=ncta
			    @ fila,79   SAY "#"
				@ fila,80   SAY  mesaaff+'.'+numcpaf
				@ fila,89   SAY  nummeshc+'.'+numhc
		        @ fila,97   SAY  LEFT(reg.glosa,40)
				@ fila,139  SAY  import PICTURE "999,999,999,999.99"
			    Fila=fila+1
		        @ Fila,97   SAY SUBSTR(reg.glosa,41,39)
		        vtot=vtot + import
			ENDSCAN
			SELE COMPAG
	    ENDIF
		fila=fila+1
	    @fila,2 SAY REPLICATE("Ä",180)
		fila=fila+1
		@ fila,22 SAY "TOTAL PRESTAMO =>"
        @ Fila,60 SAY xtot PICTURE "999,999,999,999.99"
		@ fila,92 SAY "TOTAL REGULARIZACION =>"
	    @ fila,139 SAY vtot PICTURE "999,999,999,999.99"
	    tsdo = xtot - vtot
	    @ fila,159 SAY tsdo PICTURE "@Z 999,999,999,999.99"
		fila=fila+1
	    @fila,2 SAY REPLICATE("Ä",180)
		fila=fila+1
	    SELE Compag
		taf = taf + xtot   
		tre = tre + vtot
		tsd = tsd + tsdo
		IF fila>=60
			fila=7
			DO IMP_HEADER
		ENDIF
	ENDSCAN    
	fila=fila+2
    @fila,2 SAY REPLICATE("Ä",180)
	fila=fila+1
	@ fila,12  SAY "TOTAL PRESTAMO CTACTE=>"
    @ Fila,60  SAY taf PICTURE "999,999,999,999.99"
	@ fila,82  SAY "TOTAL REGULARIZACION CTACTE=>"
    @ fila,139 SAY tre PICTURE "999,999,999,999.99"
    @ fila,159 SAY tsd PICTURE "@Z 999,999,999,999.99"
	fila=fila+1
    @fila,2 SAY REPLICATE("Ä",180)
    SELE REG
    USE 
IF _DEST1=2
    EJECT
ENDIF
SET DEVICE TO SCREEN 
RETURN

PROCEDURE Liscardi &&Programa Reporte usa repprg F.M.V.
*-------------------
PARAMETERS XCOP
PRIVATE FILA,LFORMA,XVAL,TVAL,VPAR,TOTALQ,LVAL
STORE 0 TO xval,tval,lval
LFORMA=SPACE(20)
DO CASE
	CASE _DEST1=1
		SET DEVICE TO FILE (P_FIL)
	CASE _DEST1=2
SET DEVICE TO PRINTER
	CASE _DEST1=3
		SET DEVICE TO FILE (P_FIL)
ENDCASE	
FOR I=1 TO XCOP
STORE 0 TO FILA
SELE maereg
vMES=alltrim(nummes)
vcli=numERO
vcta=alltrim(codctc)
Vkey= ALLTRIM(maereg.NUMMES)+maereg.NUMERO
@0,0 SAY CHR(18)
@1,3   SAY ALLTRIM(CIA)
@1,68  say "PAG:"
@1,76  SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "Liscardi"
@2,68 SAY "FECHA:"
@2,76 SAY DATE()           
@4,3 SAY "Ú"
@4,4 SAY REPLICATE("Ä",66)
@4,70 SAY "¿" 
@5,3 SAY "³"
@5,06 SAY "<<      H O J A    D E    C A R G A S     D I V E R S A S    >>"
@5,70 SAY "³"
@6,3 SAY "À"
@6,4 SAY REPLICATE("Ä",66) 
@6,70 SAY "Ù"
@08,06 SAY "      N§ Doc :"  
@08,25 SAY VCLI+"."+VMES  
@09,06 SAY "   Fecha Doc :"  
@09,25 SAY FECREG picture "@D"
@10,06 SAY "     Importe :"  
@10,25 SAY M.IMPORTE PICTURE "@Z 999,999,999,999.99"
@11,06 SAY "   Retenci¢n :"  
@11,25 SAY M.RETEN PICTURE "@Z 999,999,999,999.99"
@12,06 SAY "     L¡quido :"  
@12,25 SAY LETRAS(M.IMPORTE-M.RETEN,'SOLES')
@13,06 SAY "     Cta cte :"  
@13,25 SAY codctc
@13,40 SAY val_fun('Caja','codctc','Descri',codctc)
@14,06 SAY "         H/C :"
@14,25 SAY m.numMESHC+'.'+M.NUMHC+'--'+PERHC
@15,06 SAY "         C/P :"
@15,25 SAY m.numMESCP+'.'+M.NUMCP+'--'+PERCP
@16,06 SAY "    Concepto :"
@16,25 SAY left(m.glosa,50) 
@17,25 SAY substr(m.glosa,51,49) 
@18,25 SAY substr(m.glosa,101,49) 
@19,25 SAY substr(m.glosa,151,49) 
@21,20 SAY "<< ESTADISTICA DIARIA OBJETO DEL GASTO >>"
@22,03 SAY "Partida"
@22,30 SAY "Analitico"
@22,50 SAY "Parcial"
@22,72 SAY "Total"
SELE IteReg
SEEK  VKEY
@23,03 SAY IteReg.CODPART
FILA=23
SCAN WHILE NUMMES=ALLTRIM(VANO) AND NUMERO=VCLI 
   	 IF fila>=31
        	IF SUBSTR(CODANAL,1,2) <> vPar
           		FILA=FILA-1 
		       @fila,60 say xval PICTURE '@Z 999,999,999,999.99'
		       FILA=FILA+1
	    	   xval=0
	        ENDIF
			*        FILA=FILA+1
	     ENDIF           
    	 @FILA,11 SAY IIf(LEFT(IteReg.CodAnal,2)='18' AND RIGHT(IteReg.CodAnal,2)='01','ASIGNACION',IIF(LEFT(IteReg.CodAnal,2)='16' AND RIGHT(IteReg.CodAnal,2)='01','RETENCIONES',' '))
	     @FILA,30 SAY IteReg.CODANAL
	     @FILA,40 SAY IteReg.IMPPARC PICTURE '@Z 999,999,999,999.99'	
	     vpar=substr(codanal,1,2)
	     xval=xval+IteReg.impparc
	     tval=tval+IteReg.impparc
	     FILA=FILA+1
ENDSCAN     
SELE MAEREG
FILA=FILA-1
@fila,60 say xval PICTURE '@Z 999,999,999,999.99'
FILA=FILA+2
@FILA,40 SAY "      TOTAL :"    
@fila,60 say Tval PICTURE '@Z 999,999,999,999.99'
fila=fila+3
@fila,6 SAY REPLICATE("Ä",20)
@fila,46 SAY REPLICATE("Ä",26)
fila=fila+1
@fila,6 SAY "   CONTROL INTERNO"
@fila,48 SAY "TESORERO CAJERO GENERAL"
ENDFOR
IF _DEST1=2
    EJECT
ENDIF
SET DEVICE TO SCREEN 
RETURN

PROCEDURE ingap   && F5
*--------------
SELE movb
IF ingreso 
	IF F_Appd()
	REPLACE Periodo  WITH m.periodo ,;
			nummes   WITH m.nummes  ,;
			nummesC  WITH m.nummes  ,;
			numero   WITH m.numnoc  ,;
			numref   WITH m.numero  ,;
			Transa   WITH 'ORD'     ,;
			Fecha    WITH m.fecreg  ,;
			FechaC   WITH m.fecreg  ,;
			Motivo   WITH '20'     ,;
			Tipdoc   WITH 'NOC'     ,;
			codctc   WITH m.codctc  ,;
			forma    WITH 'EF'      ,;
			Monto    WITH m.importe  ,;
			estado   WITH '00'       ,; 
			codcal   WITH vcal       ,;
			codfte   WITH vfte       ,;
			tipfun   WITH m.tipfun   ,;
			CodBan   WITH w_banco    ,;
			Nombre   WITH m.glosa,;
			usuario  WITH vuser_id
	ENDIF
	UNLOCK ALL
ELSE	
    SEEK ALLTRIM(m.nummes+m.numnoc)
    IF FOUND()
	REPLACE Periodo  WITH m.periodo ,;
			nummes   WITH m.nummes  ,;
			nummesC  WITH m.nummes  ,;
			numero   WITH m.numnoc  ,;
			numref   WITH m.numero  ,;
			Transa   WITH 'ORD'     ,;
			Fecha    WITH m.fecreg  ,;
			FechaC   WITH m.fecreg  ,;
			Motivo   WITH '20'     ,;
			Tipdoc   WITH 'NOC'     ,;
			codctc   WITH m.codctc  ,;
			forma    WITH 'EF'      ,;
			Monto    WITH m.importe  ,;
			estado   WITH '00'       ,; 
			codcal   WITH vcal       ,;
			codfte   WITH vfte       ,;
			tipfun   WITH m.tipfun   ,;
			CodBan   WITH w_banco    ,;
			Nombre   WITH m.glosa,;
			usuario  WITH vuser_id
			
	ELSE		
		IF F_Appd()
			REPLACE Periodo  WITH m.periodo ,;
			nummes   WITH m.nummes  ,;
			numero   WITH m.numnoc  ,;
			numref   WITH m.numero  ,;
			Transa   WITH 'ORD'     ,;
			Fecha    WITH m.fecreg  ,;
			Motivo   WITH '20'     ,;
			Tipdoc   WITH 'NOC'     ,;
			codctc   WITH m.codctc  ,;
			forma    WITH 'EF'      ,;
			Monto    WITH m.importe  ,;
			estado   WITH '00'       ,; 
			codcal   WITH vcal       ,;
			codfte   WITH vfte       ,;
			tipfun   WITH m.tipfun   ,;
			CodBan   WITH w_banco    ,;
			Nombre   WITH m.glosa
		ENDIF	
	ENDIF
ENDIF	
SELECT astpat
SEEK ALLTRIM(m.nummes)+m.numnoc+alltrim(m.codctc)
IF !FOUND()
	DO agrite
ENDIF
ON KEY LABEL F5 DO agrite
ON KEY LABEL F8 DO eliite
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes)+m.numnoc+alltrim(M.CODCTC) FIELDS;
	codcta :H='Cuenta':v=val_fun('Cuenta','Cuenta',"Cuenta+' '+Descri",codcta,2),;
	tipcta :H='Tp' :p='@M D,H',;
	mtodeb :H='Monto Debe'  :W=tipcta='D' :V=Asig():p='999,999,999.99',;
	mtohab :H='Monto Haber' :W=tipcta='H' :p='999,999,999.99'
SEEK ALLTRIM(m.nummes)+m.numnoc+alltrim(m.codctc)
STORE 0 TO vdebe, vhaber ,vret
mmonto=mtohab+mtodeb  &&Para la 101
SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numnoc=numref AND m.codctc=alltrim(codctc)
		vdebe = vdebe + IIF(tipcta='D',mtodeb,0)
		vhaber= vhaber + IIF(tipcta='H',mtohab,0)
ENDSCAN
IF vdebe#vhaber
	DO standby WITH 'Ojo: No cuadra debe con haber'
ENDIF
if ingreso
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numnoc  ,;
				tipdoc WITH 'NOC',ret WITH 'N',;
				codctc WITH m.codctc,fecha WITH m.Fecreg,;
				tipctc WITH w_Tipctc
		REPLACE codcta WITH '1010201000',Mtodeb WITH mmonto, Tipcta WITH 'D', periodo WITH m.periodo
	ENDIF
	IF f_appd()
		REPLACE nummes WITH m.nummes, numref WITH m.numnoc,tipdoc WITH 'NOC',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.Fecreg,tipctc WITH w_Tipctc
		REPLACE codcta WITH '1010201000',Mtohab WITH mmonto, Tipcta WITH 'H', periodo WITH m.periodo
	ENDIF
	DO vis_cen
else
	* si es corrije solo modifica
	SEEK ALLTRIM(m.nummes)+m.numnoc+alltrim(m.codctc)
	IF FOUND()
		SCAN WHILE ALLTRIM(m.nummes)=nummes AND m.numnoc=numref AND m.codctc=alltrim(codctc)
		    if codcta='1010201000' and tipcta='D'
	        	repla Mtodeb WITH mmonto
	        endif
	        if codcta='1010201000' and tipcta='H'
	        	repla Mtohab WITH mmonto
	       endif
		ENDSCAN
    ELSE		
		IF f_appd()
			REPLACE nummes WITH m.nummes, numref WITH m.numnoc,tipdoc WITH 'NOC',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.Fecreg,tipctc WITH w_Tipctc
			REPLACE codcta WITH '1010201000',Mtodeb WITH mmonto, Tipcta WITH 'D', TIPCTC WITH W_TIPCTC,periodo WITH m.periodo
		ENDIF
		IF f_appd()
			REPLACE nummes WITH m.nummes, numref WITH m.numnoc,tipdoc WITH 'NOC',ret WITH 'N',codctc WITH m.codctc,fecha WITH m.Fecreg,tipctc WITH w_Tipctc
			REPLACE codcta WITH '1010201000',Mtohab WITH mmonto, Tipcta WITH 'H',TIPCTC WITH W_TIPCTC,periodo WITH m.periodo
		ENDIF
	ENDIF	
	DO vis_cen
ENDIF	
ON KEY
UNLOCK
RETURN



PROCEDURE vis_cen &&Visualiza centralizaci¢n de caja
*-----------------
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
RETURN


PROCEDURE Compre
*---------------
private as
AS=ALIAS()
xvaldeb = m.importe-m.reten
xvalhab = m.importe-m.reten
IF VAL(m.nummes)<=3
   xCtaDeb = '9220100000'
   xCtaHab = '9320100000'
ELSE   
    SELE compag
    IF SEEK(ALLT(m.nummesCP)+m.numcp+ALLT(m.codctc)) 
       Vcodfte=ALLT(compag.codfte)
	   xCtaDeb = IIF(VCodfte='PRP','9030101020','9030101040')
	   xCtaHab = IIF(VCodfte='PRP','9040101020','9040101040')
	ENDIF   
ENDIF   
USE astpre   IN 13  ORDER TAG astpre2      ALIAS astpre  
SELECT astpre
SEEK 'D'+ALLTRIM(m.nummescp)+ALLTRIM(m.numcp)+ALLTRIM(xCTADEB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummescp,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH xctadeb,;
		        ctadeb  WITH xctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecreg,;
		        CodCtc  WITH m.CodCtc;
		        periodo WITH m.periodo,;
	    	    valdeb WITH xvaldeb ,;
		        ctahab WITH SPACE(10),;
	        	valhab WITH 0 
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummescp,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH xctadeb,;
		        ctadeb  WITH xctadeb,;
		        tipo    WITH 'D',;
		        fecref  WITH m.fecreg,;
		        CodCtc  WITH m.CodCtc;
		        periodo WITH m.periodo,;
	    	    valdeb WITH xvaldeb ,;
		        ctahab WITH SPACE(10),;
	        	valhab WITH 0 
	ENDIF
	UNLOCK
ENDIF
	
SEEK 'H'+ALLTRIM(m.nummescp)+ALLTRIM(m.numcp)+ALLTRIM(xCTAHAB)+ALLTRIM(m.CodCtc)
IF !FOUND()
	IF f_appd()
		REPLACE nummes  WITH m.nummescp,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH xctahab,;
		        ctahab  WITH xctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecreg,;
		        CodCtc  WITH m.CodCtc,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH xvalhab,;
		        ctadeb WITH SPACE(10)
	ENDIF
	UNLOCK
ELSE
	IF RLOCK()
		REPLACE nummes  WITH m.nummescp,;
		        tipdoc  WITH 'C/P',;
		        numref  WITH m.numcp ,;
		        cuenta  WITH xctahab,;
		        ctahab  WITH xctahab,;
		        tipo    WITH 'H',;
		        fecRef  WITH m.fecreg,;
		        CodCtc  WITH m.CodCtc,;
		        periodo WITH m.periodo,;
			    valdeb WITH 0 ,;
			    valhab WITH xvalhab,;
		        ctadeb WITH SPACE(10)
	ENDIF
	UNLOCK
ENDIF	
USE IN 13
ACTIVATE WINDOW wind_6
@ 01,04  SAY xctadeb PICTURE '!!!!!!!!!!' 
@ 01,18  SAY xvaldeb PICTURE '999,999,999.99' 
@ 02,12  SAY xctahab PICTURE '!!!!!!!!!!' 
@ 02,34  SAY xvalhab PICTURE '999,999,999.99' 
WAIT ' '
DEACTIVATE WINDOW wind_6
vCtadeb = xctadeb
vValdeb = xValdeb
vCtahab = xctahab
vValhab = xvalhab
SELE (as)
RETURN

FUNCTION Asig
*------------
vMonDeb=mtodeb
RETURN


PROCEDURE agrite     &&
*---------------
SELECT astpat
IF f_appd()
	REPLACE nummes WITH m.nummes, numref WITH m.numnoc,;
			tipdoc WITH 'NOC',ret WITH 'N',;
			codctc WITH m.codctc ,;
			fecha  WITH m.FecReg ,;
			TIPCTC WITH W_TIPCTC  ,;
			periodo WITH m.periodo
ENDIF
RETURN


PROCEDURE eliite
*---------------
SELECT astpat
IF RLOCK()
	DELETE NEXT 1
ENDIF
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
  RELEASE    WINDOW wind_2
  RELEASE    WINDOW wind_3
  RELEASE    MENU   mMenu
  CLOSE DATA
  RETURN

FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,2)

FUNCTION RIG     &&Usado en el Repo
*--------------
PUBLIC XDEP
XMES=maereg.NUMMES
XDEP=maereg.NUMref
PRIVATE XMES
SELE ITERI
SEEK ALLTRIM(XMES)+ALLTRIM(XDEP)
IF FOUND()
   XMRI=ITERI.NUMMES
   XNRI=ITERI.NUMRI
ENDIF
sele maereg
return
   
PROCEDURE carhc
*--------------
* Carga valores de HC
vfun = .T.
OK = FOUND()
SELECT hoja
SET FILTER TO ESTADO='20' OR ESTADO='00'
GO TOP
IF EOF()
	DO standby WITH 'No existe documentos H/C '
	SET FILT TO
	RETURN .F.
ENDIF
* Trata de ingresar directamente
M.NUMMESHC ='00'
M.NUMHC  ='    '
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ H/C: " GET M.NUMMESHC DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 say '.'
@ 1,17 GET M.NUMHC DEFAULT SPACE(4)
READ
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILT TO
   RETURN .F.
ENDIF
SELE HOJA
M.NUMMesHC= PADL(ALLTRIM(M.NUMMESHC),2,'0')
M.NUMHC = PADL(ALLTRIM(M.NUMHC),4,'0')
IF !SEEK(alltrim(M.NUMMESHC)+M.NUMHC)
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
     fechc :H="fecha"
ENDIF     
IF LASTKEY()#27
    SELE HOJA
	m.nummeshc = nummes
	m.numhc    = numhc
	m.importE  = imptot
	m.perHC    = periodo
	vcal       = codcal
	m.tipfun   = Tipfun
	Vfte       = Codfte
	sele hoja
	SHOW GETS
	SET FILT TO
	SELECT MAEREG
ENDIF
RETURN .T.
   
   
PROCEDURE carcp  &&Captura C/P
*--------------
vfun = .T.
OK = FOUND()
SELECT COMPAG
SET FILTER TO PRESTAMO='S' AND ESTADO<>'99' AND EMPTY(CONAAFF)
GO TOP
IF EOF()
	DO standby WITH 'No existe Comprobantes de Pago'
	SET FILT TO
	RETURN .F.
ENDIF
M.NUMMESCP ='00'
M.NUMCP =SPACE(4)
ACTIVATE WINDOW Standby
@ 1, 4 SAY " N§ C/P: " GET M.NUMMESCP DEFAULT PADL(MONTH(DATE()),2,'0')
@ 1,16 say '.'
@ 1,17 GET M.NUMCP DEFAULT SPACE(4)
READ
SELE COMPAG
deactivate WINDOW standby
IF LASTKEY()=27
   SET FILTER TO
   RETURN .F.
ENDIF
M.NUMMESCP = PADL(ALLTRIM(M.NUMMESCP),2,'0')
M.NUMCP = PADL(ALLTRIM(M.NUMCP),4,'0')
IF !SEEK(alltrim(M.NUMMESCP)+M.NUMCP+alltrim(M.CODCTC))
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
	m.nummescp = nummes
	m.numcp    = numcp
	m.percp    = periodo
	m.reten    = reten
	xcodpart   = codpart
	SHOW GETS
	SELECT COMPAG
	SET FILT TO 
	SELECT maereg
ENDIF
RETURN .T.

PROCEDURE TOTAL
*--------------
ACTIVATE WINDOW wind_3
@ 0,1 SAY m.importE-M.RETEN PICTURE '9,999,999.99'
RETURN
   
PROCEDURE TRABAJA_HIJO
*---------------------
ACTIVATE SCREEN
HIDE MENU mmenu
vtempo = '°°°°°°°°F5->Agregar°°°°°°°°°°°°°°F8->Eliminar°°°°°°°°°°°°°°F10->Terminar°°°°°°°°'
DO logos WITH rotulo1,vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
SELECT IteReg
SEEK ALLTRIM(m.NumMes+m.NumERO)
*- Mira si tiene HC
IF !FOUND() AND !EMPTY(M.numhc) 
	SELECT itehc
	SEEK ALLTRIM(M.nummeshc)+M.numhc
	SCAN WHILE nummes=ALLTRIM(M.nummeshc) AND numhc=M.numhc AND ESTADO<>'92' AND ESTADO<>'99'
		SELECT IteReg
		IF f_appd()
		   IF ESTADO='94'
		      REPLACE impparc WITH itehc.valpart*-1
		   ELSE   
		      REPLACE impparc WITH itehc.valpart
		   ENDIF   
			REPLACE nummes WITH ALLTRIM(m.nummes), numERO WITH m.numero, ;
    				codpart WITH itehc.codpart, codctc WITH m.codctc ,codanal WITH itehc.codanal,;
    				periodo with m.periodo ,tipfun with m.tipfun,codfte with vfte,ESTADO WITH '00',;
    				CODCAL WITH vcal
		ENDIF
		UNLOCK
		SELECT itehc
	ENDSCAN
	SELECT IteReg
ENDIF
IF ALLTRIM(m.tipfun)='F'
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes+m.numERO) FIELD ;
		codanal    :H= 'Partida' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal) :R,;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) :R,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' :R
ELSE
	BROWSE NOAPPEND NODELETE NOMENU WINDOW wind_2 KEY ALLTRIM(m.nummes+m.numERO) FIELD ;
		codpart    :H= 'Partida' :v=val_part(SUBSTR(codpart,4,2),LEFT(codpart,2),'codpart'):F :W=EMPTY(codpart) :R,;
		codanal    :H= 'Analitc' :v=val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'codanal'):F :W=EMPTY(codanal) :R,;
		aa = IIF(!EMPTY(codanal),val_part(SUBSTR(codanal,4,2),LEFT(codanal,2),'D'),' ') :H='Descripci¢n' :40 :W=!EMPTY(codanal) :R,;
		impparc    :H= 'Monto'   :F :p='9,999,999.99' :R
ENDIF
GO TOP
SEEK ALLTRIM(m.NumMes)+m.NumERO
SCAN WHILE nummes=alltrim(m.nummes) and numERO = m.numERO and codctc= alltrim(m.codctc)
	IF EMPTY(IteReg.codanal) OR IteReg.impparc = 0.00
		DELETE NEXT 1
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
   
PROCEDURE TOTAL_PADRE
*--------------------
SELECT IteReg
SEEK ALLTRIM(m.nummes) + m.numERO
SCAN WHILE nummes = ALLTRIM(m.nummes) .AND. numERO = m.numERO .AND. codctc = alltrim(M.codctc)
	vtotal = vtotal + impparc
ENDSCAN
RETURN VTOTAL 
   
   
FUNCTION DISP
*------------
SELE HOJA   
IF SEEK(alltrim(M.NUMMESHC)+M.NUMHC)
	m.perHC    = Hoja.periodo
	vcal       = Hoja.codcal
	m.tipfun   = Hoja.Tipfun
	Vfte       = Hoja.Codfte
ENDIF
RETURN   

PROCEDURE imp_headER
*--------------------
@1,3   SAY ALLTRIM(CIA)
@1,164 say "  PAG:"
@1,172 SAY ALLTRIM(STR(_PAGENO,8))
@2,3   SAY "LisAAFF"
@2,50  say 'CONCILIACION DE ASIGNACIONES FINANCIERAS DEL '+DTOC(VFECINI)+'  AL  '+DTOC(VFECFIN)
@2,164 SAY "FECHA:"
@2,172 SAY DATE()           
@3,53  say "CUENTA CORRIENTE : "+ALLTRIM(VCTA)
@3,90  say val_fun('Caja','codctc','Descri',ALLTRIM(VCTA))
@4,2 SAY REPLICATE("Ä",180)
@5,3 SAY "  C/P    FECHA                  DETALLE                             MONTO      REG.     H/C              DETALLE                                    MONTO              SALDO"
@6,2 SAY REPLICATE("Ä",180) 
RETURN