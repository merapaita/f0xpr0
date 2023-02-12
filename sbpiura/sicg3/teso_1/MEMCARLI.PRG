* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ Memcarli.PRG  14/12/98                                       ³	
* ³ Cargo de Liberaci¢n                                          ³
* ³ Autor      :  PILAR PACHERRE NAVARRO                         ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario

PRIVATE Vmens01,Vmens02,Vmens09
Vmens01 = 'Revisi¢n de Cargos de Liberaci¢n de Fianzas'
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '

*- Abre archivos

USE carfia  IN  1 ORDER TAG carlib				ALIAS carfia
USE Parmae   IN  4 ORDER TAG Parmae1			ALIAS Parma
USE CptObra IN 3 ORDER Cptobra2					ALIAS obra 
USE Auxil   IN 4 ORDER TAG Auxil1				ALIAS Auxil
*- Variables de trabajo (registro a trabajar)
PUBLIC vTipCF,oriw,vtocli,vnuevo,sw,VUSER_ID,wuser_id,VORDEN
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
STORE 1  TO vTipCF,oriw,vtocli,VORDEN
STORE 0  TO sw
STORE SPACE(4)  TO vnuevo
SCATTER MEMVAR

*- Inicia proceso
SELECT CARFIA
GO BOTTOM
DO Inicia                    && Define ventanas, men£s, t¡tulos
HIDE POPUP ALL
DO Pantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
   ACTIVATE SCREEN
   ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion

SHOW POPUP Menu

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Anula    Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_C3 FROM 12,00 TO 23,38 DOUBLE ;
 TITLE Vmens09 COLOR SCHEME 10

 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10

* DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
* TITLE 'Detalle: Memorandum de Bases de Licitaci¢n' ;

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT 'A\<nula  '    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO Lista
 ON SELECTION PAD termi  OF mMenu DO termi
 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 01,02 SAY "     N§. Cargo:"  
 @ 03,02 SAY "   Fecha Cargo:" 
 @ 04,02 SAY "N§Carta Fianza:" 
 @ 06,02 SAY "         Banco:" 
 @ 07,02 SAY "       Importe:" 
 @ 08,02 SAY "      Garant¡a:" 
 @ 09,02 SAY "       O b r a:" 
 @ 10,02 SAY " Fecha Vencim.:" 
 @ 11,02 SAY "    Referencia:" 
 @ 12,02 SAY "   N§ Registro:" 
 @ 13,02 SAY "   Contratista:" 
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT carfia
 SCATTER MEMVAR
 @  1,18 SAY m.carlib
 @  3,18 SAY m.Feclib
 @  4,18 SAY m.numdoc 
 @  6,18 SAY Val_Para(m.CodBan,"BANCOS",'V',22,40)
 @  7,18 SAY m.importe picture '999,999,999,999.99'
 @  8,18 SAY VAL_PARA(m.garantia,'GARANT','V', 22,20)
 @  9,18 SAY IIF(m.origen='G',Val_Fun("obra","Codobra","LEFT(Descri,50)",ALLTRIM(m.Codobr)),m.refer)
 @ 10,18 SAY m.fecven
 @ 11,18 SAY m.memcarli
 @ 12,18 SAY m.numero
 @ 13,18 SAY IIF(m.tipo='C',val_Auxi(ALLT(m.CodPrv),'50','V'),IIF(m.tipo='P',val_Auxi(ALLT(m.CodPrv),'20','V'),val_Auxi(ALLT(m.CodPrv),'09','V')))
RETURN

***********
 @  1,22 SAY m.numero
 @  2,22 SAY m.Fecrec
 @  4,22 SAY Val_Para(m.CodBan,"BANCOS",'V',22,40)
 @  5,22 SAY m.NumDoc
 @  7,22 SAY m.FecEmi
 @  8,22 SAY m.FecVen
 @ 09,22 SAY m.importe PICTURE '999,999,999,999.99'
 @ 11,22 SAY m.origen PICTURE "!"
 @ 13,22 SAY m.tipo PICTURE "!"
 @ 14,22 SAY IIF(m.tipo='C',val_Auxi(ALLT(m.CodPrv),'50','V'),IIF(m.tipo='P',val_Auxi(ALLT(m.CodPrv),'20','V'),val_Auxi(ALLT(m.CodPrv),'09','V')))
 @ 15,22 SAY m.codobr  PICTURE "@!"
* @ 15,22 GET m.refer   PICTURE "@S50"   FUNCTION "!" when m.origen='A'
 @ 15,27 SAY IIF(m.origen='G',Val_Fun("obra","Codobra","LEFT(Descri,50)",ALLTRIM(m.Codobr)),m.refer)
 @ 17,22 SAY VAL_PARA(m.garantia,'GARANT','V', 22,20)
 @ 18,22 SAY m.Observ    PICTURE '@S50'
**************


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT  Carfia
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 vTemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_C4;
   NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
   numero   :H= 'Cargo' ,;
   Fecha    :H= 'Fecha',;
   numdoc  ,;
   importe  :H='Importe'
   vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
   DO Logos WITH Rotulo1,vTempo
   IF LASTKEY()=27
      GOTO Vtemp
   ENDIF
   SHOW MENU mMenu
   ON KEY LABEL F10
   DEACTIVATE WINDOW Wind_C4
   DO Vista
RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 SELECT CarFia
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(04) TO Vbusca
 @ 0, 2 SAY 'N£mero de Cargo que desea buscar:'
 @ 0,40 GET Vbusca PICTURE '9999'
 READ
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT CarFia
    RETURN
 ENDIF
IF !SEEK(Vbusca)
   DO standby WITH "Documento no encontrado."
   GOTO Vtemp
ELSE
   DO Vista
ENDIF
RETURN


PROCEDURE Anter              && Documento anterior
*--------------
 SELECT CarFia
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF !BOF()
    SKIP -1
 ENDIF

 IF BOF()
    GO TOP
    DO standby WITH "No existe Documento anterior."
 ELSE
    DO Vista
 ENDIF

RETURN


PROCEDURE Proxi              && Documento siguiente
*--------------
 SELECT CarFia
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF

 IF !EOF()
    SKIP
 ENDIF

 IF EOF()
    DO standby WITH "No existe Documento siguiente."
    GO BOTTOM
 ELSE
    DO Vista
 ENDIF
RETURN


PROCEDURE Corri              && Corrige el documento actual
*--------------
 SELECT CarFia
 SCATTER MEMVAR
 IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF
*vasu1=LEFT(m.asunto,45)
*vasu2=SUBSTR(m.asunto,46,45)
*vasu3=RIGHT(m.asunto,10)
 ingreso=.F.
 IF !f_lock(1)
    RETURN
 ENDIF
 ACTIVATE WINDOW Wind_C0

 @  1,18 GET m.carlib   WHEN .F.
 @  3,18 GET m.Feclib 	PICTURE '@E' 
 @  4,18 GET m.numdoc   picture "@!"
 @  6,18 GET m.codban   picture "999"
 @  7,18 GET m.importe  picture "999,999,999.99"
 @  8,18 GET m.garantia picture "@!"
 @  6,18 GET m.codban   FUNCTION "999"
 @ 10,16 GET vasu1      FUNCTION "!" PICTURE '@S45'
 @ 11,16 GET vasu2      FUNCTION "!" PICTURE '@S45'
 @ 12,16 GET vasu3      FUNCTION "!"  
 @ 13,16 GET m.refer    FUNCTION "!"
 @ 15,16 GET m.conlic   PICTURE "9999" VALID val_fun('ConLic','ConLic',"ConLic+' '+Deslic",m.conlic,1)
 READ VALID Val_Read()
 SELE MemBas
 IF LASTKEY() # 27
    SELECT MemBas
    m.asunto=vasu1+vasu2+vasu3
    GATHER MEMVAR
    SCATTER MEMVAR
    UNLOCK ALL
    SELECT MemBas
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 DO Vista
RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT MemBas
 DO pantalla
 SCATTER MEMVAR BLANK
 m.tipo=velige
 m.Fecha=DATE()
 SELECT Parma
 SEEK 'CORREL'+'MEMBAS'
 m.numero  = PADL(NumEnt+1,4,'0')
 STORE SPACE(45) TO vasu1,vasu2
 STORE SPACE(10) TO vasu3

 @ 01,16 GET m.numero      PICTURE "9999"  VALID vexiste()
 @ 03,16 GET m.Fecha       PICTURE '@E'  
 @ 05,16 GET m.destino  FUNCTION "!" 
 @ 07,16 GET m.cargo    FUNCTION "!" 
 @ 10,16 GET vasu1      FUNCTION "!"    PICTURE '@S45'
 @ 11,16 GET vasu2      FUNCTION "!"    PICTURE '@S45'
 @ 12,16 GET vasu3      FUNCTION "!"   
 @ 13,16 GET m.refer    FUNCTION "!"
 @ 15,16 GET m.conlic   PICTURE "9999" VALID val_fun('ConLic','ConLic',"ConLic+' '+Deslic",m.conlic,1)
 READ VALID Val_Read()
 SELE MemBas
  IF LASTKEY()#27 
     vAnula = .F.
     SELECT MemBas
     IF F_Appd()
        m.asunto=vasu1+vasu2+vasu3
        GATHER MEMVAR
        UNLOCK ALL
        SELECT Parma
        SEEK 'CORREL'+'MEMBAS'
        REPLACE NumEnt WITH NumEnt + 1       
        SELECT MemBas
        SCATTER MEMVAR
    ELSE
       GO BOTTOM
    ENDIF
    UNLOCK ALL
    SELECT MemBas
ELSE
    DO STANDBY WITH 'Proceso cancelado'
ENDIF
DO Vista
RETURN

PROCEDURE Anula
*--------------
* Anula el documento actual.
IF yesno("¨ANULA Documento?")
   vErdad=.T.
   SELECT MemBas
   vnumero=ALLTRIM(MemBas.numero)
   IF f_lock(1)
      DELETE
      *-Busca y elimina los Items
      SELECT MemBas
      IF !BOF()
         SKIP -1
      ELSE
         IF !EOF()
            SKIP
         ENDIF
      ENDIF
  ENDIF
ENDIF
UNLOCK ALL
DO Vista
RETURN


PROCEDURE Termi
*--------------
 vEn_accion = .F.
 DEACTIVATE MENU
RETURN


PROCEDURE Fin_opcion
*-------------------
 DO Logos WITH Rotulo1,Rotulo2
 RELEASE    WINDOW Wind_C0
 RELEASE    WINDOW Wind_C2
 RELEASE    MENU   mMenu
 CLOSE DATA
 RESTORE SCREEN FROM PRINCIPAL
RETURN


PROCEDURE Lista
*--------------
 PRIVATE costo
 SELECT MemBas
 IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF

 SET RELATION TO conlic INTO conlic
 costo=Conlic.Vtabase
 SCATTER MEMVAR
 rec=RECNO()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
 SET FILT TO numero=m.numero
 IF velige='C'
	 DO REPORTE WITH 2,"FtoMemB"," Memorandum de Resultados de Bases ",1,.F.,.T.
 ELSE
 	 DO REPORTE WITH 2,"FtoMemO"," Memorandum de Resultados de Inscripciones ",1,.F.,.T.
 ENDIF
 SET FILT TO
 SELE Fact
 SET FILT TO conlic=m.conlic .AND. tipo=velige
 IF velige='C'
	 DO REPORTE WITH 2,"Resbas"," Resultados de Bases ",1,.F.,.T.
 ELSE
 	 DO REPORTE WITH 2,"ResOtr"," Resultados de Inscripciones ",1,.F.,.T.
 ENDIF
 SET FILT TO
 SELECT MemBas
 GO rec
 DO vista
RETURN


FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)


PROCEDURE VALFECHA
*-----------------
an=right(str(year(date()),4),2)
vMe = val(m.Nummes)+1
me = padl(alltrim(str(vMe,2)),2,'0')
vFec = '01/&ME/&AN'
m.Fecha = ctod(vFec) - 1
RETURN .T.


FUNCTION Vexiste
*---------------
SELE MemBas
SEEK m.numero
IF FOUND()
    DO STANDBY WITH "El N§ de Memo ya existe"
	RETURN .F.
ENDIF
RETURN .T.	