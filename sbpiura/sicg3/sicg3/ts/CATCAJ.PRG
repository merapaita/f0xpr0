* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CatCaj.PRG  12/06/96                                L: 0559  ³	
* ³ Cat logo de Cuentas de Bancos                                ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.             ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae   IN 1   ORDER TAG Parmae1  ALIAS Parma
USE Auxil    IN 3   ORDER TAG Auxil1
USE cajas    IN 2   ORDER TAG Cajas1   ALIAS Caja
*USE ctasec   IN 4   ORDER TAG Ctasec2  ALIAS Ctasec
USE SubCtas  IN 5   ORDER TAG Subctas1 ALIAS Subcta
USE Cuentas  IN 7   ORDER TAG Cuentas1 ALIAS cuenta
USE MovBco   IN 8   ORDER TAG MovBco11 ALIAS MovB
USE Cheque   IN 6   ORDER TAG Cheque1  ALIAS Cheque
*- Variables de trabajo (registro a trabajar)
SELECT Caja
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco
PUBLIC wtipoau,wctacte,vflag,Veli
STORE SPACE (1)  TO vflag
STORE SPACE (14) TO wctacte
STORE SPACE (2)  TO wtipoau
wtipoau="11"

*- Mensajes de aviso al usuario
Vmens01 = ' Registro de Cuentas    ± F4 Correlativos ± '
Vmens02 = 'Revisi¢n de Cajas'
Vmens04 = 'Dicha Cuenta no fue encontrada'
Vmens05 = 'No existe Cuenta anterior'
Vmens06 = 'No existe Cuenta siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚sta Cuenta ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Caja ha sido anulada'
Vmens10 = 'La Caja ya fue atendida'


*- Inicia proceso
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79  ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_c1 FROM 00,00 TO 23,79 ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar'     AT 24,63
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
 @ 01,02 SAY "   Cuenta Corriente :"
 @ 03,02 SAY "        Descripci¢n :"
 @ 05,02 SAY "               Tipo :"
 @ 06,02 SAY "              Banco :"
 @ 07,02 SAY "       Pertenece a? :"
 @ 08,02 SAY "             Fuente :"
 @ 09,02 SAY "       Mancomunada? :"
 *@ 10,02 SAY "  Cuenta Sectorial? :"
 *@ 11,02 SAY " Trabaja Subsidios? :"
 *@ 12,02 SAY " Maneja Sub-Cuentas :"
 *@ 13,02 SAY "         Sub-Cuenta :"
 *@ 14,02 SAY "         Condici¢n? :"
 *@ 15,02 SAY "   Oficina Giradora :"
 @ 16,02 SAY "Incidencia Contable :"
 @ 17,02 SAY " Asiento Autom tico :"
 @ 18,02 SAY "     Fecha Apertura :"
 @ 19,02 SAY "               Obra :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Caja
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 ON KEY LABEL F4 DO CorCorre
 @ 01,24 SAY m.CodCtc
 @ 03,24 SAY m.Descri
 @ 05,24 SAY m.clase
 @ 05,30 SAY val_para(m.clase,'TIPCTC','D',5,30)
 @ 06,24 SAY m.banco
 @ 06,30 SAY val_para(m.banco,'BANCOS','D',6,30)
 @ 07,24 SAY m.tipo
 @ 07,30 SAY val_para(m.tipo,'TIPCAJ','D',7,30)
 @ 08,24 SAY m.codfte
 @ 08,30 SAY val_para(m.codfte,'CODFTE','D',8,30)
 @ 09,24 SAY m.ctaman
 **@ 09,30 SAY val_para(m.tipfun,'TIPGAS','D',9,30)
 *@ 10,24 SAY m.sector
 *@ 11,24 SAY m.subsidio
 *@ 12,24 SAY IIF(!EMPTY(m.codscta),'S','N')
 *@ 13,24 SAY IIF(!EMPTY(m.codscta),m.CodScta,SPACE(8))
 *@ 13,36 SAY IIF(!EMPTY(m.codscta),IIF(m.codfte='00',Val_Fun("SubCta","Subcta","Descri",ALLTRIM(m.CodScta)),"OFICINA GIRADORA DE INGRESOS PROPIOS"),SPACE(36))
 *@ 14,24 SAY IIF(!EMPTY(m.codscta),m.ofigir,' ')
 *@ 14,30 SAY IIF(!EMPTY(m.codscta),val_para(m.ofigir,'CONOFG','D',14,30),SPACE(30))
 *@ 15,24 SAY IIF(m.ofigir='S',m.codofg,'  ')
 *@ 15,30 SAY IIF(m.ofigir='S',IIF(m.codfte='00',val_para(m.codofg,'OFIGIR','D',15,30),val_para(m.codofg,'OFGIIP','D',15,30)),SPACE(25))
 @ 16,24 SAY m.inccon
 @ 17,24 SAY IIF(m.inccon='S',m.cuentaH,SPACE(11))
 @ 17,36 SAY IIF(m.inccon='S',val_fun('Cuenta','Cuenta',"DescRi",m.cuentaH),SPACE(50))
 @ 18,24 SAY m.FecIni
 @ 19,24 SAY m.obra
 RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE caja
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
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 codctc  :H='Cta Cte' ,;
 descri  :H='Detalle' ,;
 tipo    :H='Tipo'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 DO Vista
 RETURN

PROCEDURE Busca
*--------------
 SELECT caja

 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF

 Vtemp = RECNO()

 STORE SPACE(14) TO vGen

 ACTIVATE WINDOW standby
 @ 1,03 SAY "   Cuenta Corriente :" GET vGen
 READ
 DEACTIVATE WINDOW standby

 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK vGen
    IF !FOUND()
       DO standby WITH "Dicha Cuenta no est  registrada"
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

 ACTIVATE WINDOW Wind_0
 SELECT Caja
 SCATTER MEMVAR
 IF RLOCK() OR F_LOCK(1)
	 vflag=IIF(!EMPTY(m.codscta),'S','N')
	 @ 01,24 GET m.CodCtc DISABLE
	 @ 03,24 GET m.Descri
	 @ 05,24 GET m.clase    PICTURE '!'    VALID val_para(m.clase,'TIPCTC',' ',24,20)
	 @ 06,24 GET m.Banco    PICTURE '!!!'  VALID val_paraD(m.banco,'BANCOS',' ',24,20)
	 @ 07,24 GET m.tipo     PICTURE '!!'   VALID val_para(m.tipo,'TIPCAJ',' ',24,20)
	 @ 08,24 GET m.codfte   PICTURE "!!"  VALID val_para(m.codfte,'CODFTE',' ',24,20)
	 @ 09,24 GET m.ctaman   PICTURE "!"    FUNCTION "@M N,S"
*	 @ 10,24 GET m.sector   FUNCTION "@M N,S"
*	 @ 11,24 GET m.subsidio FUNCTION "@M N,S"
*	 @ 12,24 GET vflag      FUNCTION "@M N,S"
	 READ
*	 IF  vflag='S'
*	     IF ALLT(m.codfte)='00'
*		     @ 13,24  GET m.CodScta  PICTURE "@!"  VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",m.CodScta,1,3,36,'Descri') 
*		 ELSE  
*		     m.codscta='ING.PRP.'  
*		     @ 13,24  SAY m.CodScta
*		 ENDIF    
*	     @ 14,24  GET m.ofigir   PICTURE "!"   VALID val_para(m.ofigir,'CONOFG',' ',24,20)
*	     READ
*	     IF ALLT(m.ofigir)='S'
*		     IF ALLT(m.codfte)='00'
*		        @ 15,24 GET m.CodOfg      VALID Val_Para(m.CodOfG,'OFIGIR',' ',24,20) 
*		     ELSE   
*		        @ 15,24 GET m.CodOfg      VALID Val_Para(m.CodOfG,'OFGIIP',' ',24,20) 
*		     ENDIF   
*	     ENDIF
*	 ELSE
*	     m.ofigir='N'
*	 ENDIF
	 @ 16,24 GET m.inccon FUNCTION "@M S,N"
	 READ
	 IF m.inccon='S'
	    @ 17,24 GET m.cuentaH VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",m.cuentaH,1)
	 ENDIF
*	 PICTURE "99999999999"
	 @ 18,24 GET m.FecIni DISABLE
	 IF m.ctaman='S'
	    @ 19,24 GET m.obra PICTURE '@!' FUNCTION '@S45'
	 ENDIF
	 READ VALID Val_Read()
	 IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
	    SELECT Caja
	    GATHER MEMVAR
	    DO corrauxi
	 ENDIF
	 DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 DO standby WITH 'Debe comunicar a Contabilidad para la apertura contable' 
 SELECT Caja
 DO Pantalla
 SCATTER MEMVAR BLANK
 @ 01,24 GET m.CodCtc VALID vctc()
 @ 03,24 GET m.Descri
 @ 05,24 GET m.clase    PICTURE '!'    VALID val_para(m.clase,'TIPCTC',' ',24,20)
 @ 06,24 GET m.Banco    PICTURE '!!!'  VALID val_paraD(m.banco,'BANCOS',' ',24,20)
 @ 07,24 GET m.tipo     PICTURE '!!'   VALID val_para(m.tipo,'TIPCAJ',' ',24,20)
 @ 08,24 GET m.codfte   PICTURE "!!"  VALID val_para(m.codfte,'CODFTE',' ',24,20)
 @ 09,24 GET m.ctaman   PICTURE "!"    FUNCTION "@M N,S"
* @ 10,24 GET m.sector   FUNCTION "M N,S"
* @ 11,24 GET m.subsidio FUNCTION "M N,S"
* @ 12,24 GET vflag      FUNCTION "M N,S"
 READ
*	 IF  vflag='S'
*	     IF ALLT(m.codfte)='00'
*		     @ 13,24  GET m.CodScta  PICTURE "@!"  VALID Val_Fun("SubCta","Subcta","Subcta+' '+Descri",m.CodScta,1,3,36,'Descri') 
*		 ELSE  
*		     m.codscta='ING.PRP.'  
*		     @ 13,24  SAY m.CodScta
*		 ENDIF    
*	     @ 14,24  GET m.ofigir   PICTURE "!"   VALID val_para(m.ofigir,'CONOFG',' ',24,20)
*	     READ
*	     IF ALLT(m.ofigir)='S'
*		     IF ALLT(m.codfte)='00'
*		        @ 15,24 GET m.CodOfg      VALID Val_Para(m.CodOfG,'OFIGIR',' ',24,20) 
*		     ELSE   
*		        @ 15,24 GET m.CodOfg      VALID Val_Para(m.CodOfG,'OFGIIP',' ',24,20) 
*		     ENDIF   
*	     ENDIF
*	 ELSE
*	     m.ofigir='N'
*	 ENDIF
 @ 16,24 GET m.inccon FUNCTION "M S,N"
 READ
 IF m.inccon='S'
    @ 17,24 GET m.cuentaH VALID val_fun('Cuenta','Cuenta',"Cuenta+' '+DescRi",m.cuentaH,1)
*PICTURE "99999999999" 
 ENDIF
 @ 18,24 GET m.FecIni 
 IF m.ctaman='S'
    @ 19,24 GET m.obra PICTURE '@!' FUNCTION '@S45'
 ENDIF

 READ VALID Val_Read()
 IF LASTKEY() # 27
    IF F_Appd()
       GATHER MEMVAR
       DO graauxi
    ELSE
       GO BOTTOM
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado, no se graba nada'
   GO BOTTOM
 ENDIF
 UNLOCK ALL
 SELECT Caja
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 SELECT Caja
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 veli=.F.
 SELE movb
 GO TOP
 LOCATE FOR codctc=m.codctc
 IF !FOUND()
    veli=.T.
 ENDIF 
 SELE cheque
 GO TOP
	LOCATE FOR codctc=m.codctc
    IF !FOUND()
    veli=.T.
    ENDIF 
 SELE caja
 IF !Veli	
    DO standby WITH 'No se puede eliminar, tiene movimientos'
 ELSE   
	 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚sta Cuenta ?')
	 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	   WCTACTE=CODCTC
	   DELETE NEXT 1
	   GO BOTTOM
	   do eliauxi
	   DO Vista
	 ENDIF
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
SELECT Caja 
Vind=SYS(3)+'.idx'
vtemp =RECNO()
IF EOF()
   DO standby WITH vmens08
   RETURN
ELSE
	IF escolor
		DEFINE POPUP xcot  FROM 15,55 SHADOW COLOR &l_col
	ELSE
		DEFINE POPUP xcot  FROM 15,55 COLOR SCHEME c_popup
	ENDIF
	DEFINE BAR 1 OF xcot PROMPT 'Por \<C¢digo     '
	DEFINE BAR 2 OF xcot PROMPT 'Por \<Banco      '
	DEFINE BAR 3 OF xcot PROMPT 'Por \<Tipo       '
	DEFINE BAR 4 OF xcot PROMPT 'Por \<Pertenencia'
	DEFINE BAR 5 OF xcot PROMPT 'Por \<Fuente     '
	ON SELECTION POPUP xcot  DEACTIVATE POPUP
	ACTIVATE POPUP xcot
	DO CASE
	   CASE BAR() = 1
		    do reporte with 2,"LisCaj",' Listado de Cuentas Corrientes '
	   CASE BAR() = 2
	        INDEX ON banco TO (Vind) 
		    DO reporte WITH 2,"LisCajB",' Listado de Cuentas Corrientes por Banco'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	   CASE BAR() = 3
	        INDEX ON clase TO (Vind) 
		    DO reporte WITH 2,"LiscajT",' Listado de Cuentas Corrientes por Tipo'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	   CASE BAR() = 4
	        INDEX ON tipo TO (Vind) 
		    DO reporte WITH 2,"LiscajP",' Listado de Cuentas Corrientes por Pertenencia'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	   CASE BAR() = 5
	        INDEX ON Codfte TO (Vind) 
		    DO reporte WITH 2,"LiscajF",' Listado de Cuentas Corrientes por Fuente de Fianciamiento'
		    SET INDEX TO
		    CLOSE INDEX
		    ERASE (vind)
			SET ORDER  TO Cajas1
	ENDCASE
ENDIF
GO vtemp
DO Vista
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


PROCEDURE Corrauxi
*-------------------
SELE auxil
SEEK  wTipoau + m.Codctc
IF FOUND()
   IF RLOCK() OR F_LOCK(1)
	   REPLACE Tipo   WITH wtipoau,;
	           Codigo WITH m.codctc,;
	           descri WITH m.descri
   ENDIF
   UNLOCK
ENDIF
RETURN   
      
PROCEDURE Graauxi
*-------------------
SELECT Parma
SEEK 'CORREL'+'CTACTE'
wcorr=nument+1
wcorr = SUBSTR(STR(wcorr,4),3,2)
SELE auxil
IF F_Appd()
   REPLACE Tipo      WITH wtipoau,;
           Codigo    WITH m.codctc,;
           descri    WITH m.descri,;
           direccion WITH wcorr
ENDIF
UNLOCK ALL
SELECT Parma
SEEK 'CORREL'+'CTACTE'
REPLACE NumEnt WITH NumEnt + 1       
UNLOCK
RETURN

PROCEDURE Eliauxi
*-------------------
SELE auxil
SEEK  wTipoau + wctacte
IF FOUND()
   IF RLOCK() OR F_LOCK(1)
	   DELETE NEXT 1
	   GO BOTTOM
   ENDIF
   UNLOCK
ENDIF
RETURN   

FUNCTION Corcorre
*----------------

IF m.sector = 'N'
	DEFINE WINDOW Wind_3 FROM 18,14 TO 22,60 DOUBLE ;
 	TITLE "Correlativo" COLOR SCHEME 10
	ACTIVATE WINDOW Wind_3
	@ 1,1 SAY " Correlativo C/P : " GET m.corcp 
	READ VALID Val_read()
	REPLACE corcp WITH m.corcp
	DEACTIVATE WINDOW Wind_3
ELSE
	DEFINE WINDOW Wind_3 FROM 18,14 TO 22,60 DOUBLE ;
 	TITLE "Correlativo" COLOR SCHEME 10
	ACTIVATE WINDOW Wind_3
	@ 1,1 SAY " Correlativo C/P : " GET m.corcp 
	READ VALID Val_read()
	REPLACE corcp WITH m.corcp
	DEACTIVATE WINDOW Wind_3


*	IF m.codfte='00'
	DEFINE WINDOW Wind_3 FROM 05,02 TO 18,77 DOUBLE ;
 	COLOR SCHEME 10
		SELECT Ctasec
		vTempo = '°°°°°°°°°°°Presione ®F10¯ para Grabar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
*		DO Logos WITH Rotulo1,vTempo
		ON KEY LABEL F10 KEYBOARD CHR(23)

		BROWSE WINDOW WIND_3 TITLE vtempo KEY m.codctc NOMENU NODELETE NOAPPEND FIELD ;
			Sector :H='Sector':R:W=.F.,;
			corsec
			
*	ENDIF
	SELECT Caja
ENDIF
ON KEY LABEL F10
RETURN

FUNCTION vctc
*------------
SELE Caja
SEEK m.codctc
IF FOUND()
   DO STANDBY WITH 'Cuenta Corriente ya existe' 
   RETURN  .F.
ENDIF
RETURN .T.