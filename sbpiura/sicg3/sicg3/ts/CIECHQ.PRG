* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ CieChq.PRG  10/10/96                                L: 0451  ³	
* ³ Cierre de Chequeras                                          ³
* ³ AUTOR : Ing. Federico Montero Valdiviezo REGION GRAU - 1996  ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

*- Mensajes de aviso al usuario
PUBLIC Ingreso
PRIVATE Vmens01,Vmens02,Vmens09
Vmens02 = 'Documentos'
Vmens08 = 'No hay registros para procesar'
Vmens09 = '  Detalle  '
Vmens01 = 'Cierre de Chequeras    '

*- Abre archivos

USE Contchq  IN  1 ORDER TAG Contchq4                ALIAS ContC
USE Parmae   IN  3 ORDER TAG Parmae1                 ALIAS Parma
USE Cajas    IN  4 ORDER TAG Cajas2                  ALIAS Caja

*- Variables de trabajo (registro a trabajar)
PUBLIC vFun,m.fecha,xMes,xAno,sw,Vnummes,VUSER_ID,wuser_id,wbanco,wdesbco
vUser_ID = ALLTRIM(LEFT(SYS(0),15))
wUser_ID= CHRTRAN(vUser_ID,'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789',;
          'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?')
SELECT ContC
SCATTER MEMVAR BLANK

STORE .T. TO Agrega,vFun
m.feccie=DATE()
store 0 to sw
*- Mes y a¤o actual

*- Inicia proceso

SELECT ContC
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
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_C0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10 
 
 DEFINE WINDOW Wind_C4 FROM 00,00 TO 23,79 DOUBLE;
 TITLE Vmens02 COLOR SCHEME 10


 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Lista '     AT 24,63
 DEFINE PAD termi   OF mMenu PROMPT '\<Termina'    AT 24,71
 ON SELECTION PAD revis  OF mMenu DO revis
 ON SELECTION PAD busca  OF mMenu DO busca
 ON SELECTION PAD anter  OF mMenu DO anter
 ON SELECTION PAD proxi  OF mMenu DO proxi
 ON SELECTION PAD corri  OF mMenu DO corri
 ON SELECTION PAD ingre  OF mMenu DO ingre
 ON SELECTION PAD elimi  OF mMenu DO anula
 ON SELECTION PAD lista  OF mMenu DO LISTA
 ON SELECTION PAD termi  OF mMenu DO termi
 ACTIVATE SCREEN
RETURN


PROCEDURE Pantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_C0
 CLEAR
 @ 0, 1 SAY "      N£mero :"
 @ 2, 1 SAY "     Periodo :"
 @ 4, 1 SAY "       Fecha :"
 @ 6, 1 SAY "         Mes :"
 @ 8, 1 SAY "   N§ Oficio :"
 @10, 1 SAY "      Codctc :"
 @12, 1 SAY "    Desde N§ :"
 @12,40 SAY "    Hasta N§ :"
 @14, 1 SAY "     Nro.Chq :"
 @14,40 SAY "       Serie :" 
 @16, 1 SAY "       Fecha :"
 @18, 1 SAY "Chq. Anulados:"
RETURN


PROCEDURE Vista              && Muestra cabecera e items del documento
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT ContC
 ON KEY LABEL F2 DO e_chequera
 SCATTER MEMVAR
 @  0,16 SAY m.numero
 @  2,16 SAY m.Periodo
 @  4,16 SAY m.Fecha
 @  6,16 SAY m.NumMes
 @  6,23 SAY IIF(!EMPTY(m.nummes),Val_Para(m.NumMes,'FECMES','D',1,23),' ')
 @  8,16 SAY m.nroofi+'.'+m.mesofi
 @ 10,16 SAY m.Codctc
 @ 10,32 SAY IIF(!EMPTY(m.codctc),Val_Fun("Caja","CodCtc","Descri",m.Codctc),' ')
 @ 12,16 SAY m.inichq
 @ 12,56 SAY m.finchq
 @ 14,16 SAY m.nrochq PICTURE "999"
 @ 14,56 SAY m.serie PICTURE '999' 
 @ 16,16 SAY m.feccie
 @ 18,16 SAY m.totinu
RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 SELECT ContC
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
   Nummes   :H= 'Mes',;
   Numero   ,;
   CodCtc   :H= 'Cta_Cte',;
   inichq   :H= 'Desde N§',;
   finchq   :H='Hasta N§',;
   feccie   ,;
   totinu   :H='Chq. anulados'
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
 SELECT ContC
 IF EOF()
    DO standby WITH VMens08
    RETURN
 ENDIF
 Vtemp = RECNO()
 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 5
 ACTIVATE WINDOW WBusca
 STORE SPACE(02) TO vmesb
 STORE SPACE(04) TO Vbusca
 @ 0,43 say "."
 @ 0, 2 SAY 'N£mero de documento que desea buscar:'
 @ 0,41 GET Vmesb PICTURE '!!'
 @ 0,44 GET Vbusca PICTURE '!!!!'
 READ
 RELEASE WINDOW WBusca
 IF LASTKEY()=27
    SELECT ContC
    RETURN
 ENDIF
IF !SEEK(ALLTRIM(Vmesb)+ALLTRIM(Vbusca))
   DO standby WITH "Documento no encontrado."
   GOTO Vtemp
ELSE
   DO Vista
ENDIF
RETURN


PROCEDURE Anter              && Documento anterior
*--------------
 SELECT ContC
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
 SELECT ContC
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
 SELECT ContC
 SCATTER MEMVAR
 IF EOF()
    DO standby WITH Vmens08
    UNLOCK ALL
    RETURN
 ENDIF
 IF !f_lock(1)
    RETURN
 ENDIF
 ACTIVATE WINDOW Wind_C0
 ingreso=.F.
 @  0,16 SAY m.numero
 @  2,16 SAY m.Periodo
 @  4,16 SAY m.Fecha
 @  6,16 SAY m.NumMes
 @  6,23 SAY IIF(!EMPTY(m.nummes),Val_Para(m.NumMes,'FECMES','D',1,23),' ')
 @  8,16 SAY m.nroofi+'.'+m.mesofi
 @ 10,16 SAY m.Codctc
 @ 10,32 SAY IIF(!EMPTY(m.codctc),Val_Fun("Caja","CodCtc","Descri",m.Codctc),' ')
 @ 12,16 SAY m.inichq
 @ 12,56 SAY m.finchq
 @ 14,16 SAY m.nrochq PICTURE "999"
 @ 14,56 SAY m.serie PICTURE '999' 
 @ 16,16 GET m.Feccie      PICTURE '@E' VALID m.feccie>=m.fecrec
 @ 18,16 GET m.totinu      PICTURE "999"
 m.usuarioe=wuser_id
 READ VALID Val_Read()
 IF LASTKEY() # 27
    SELECT ContC
    IF  RLOCK()
        REPLACE feccie   WITH m.feccie
        REPLACE totinu   WITH m.totinu
        REPLACE estado   WITH m.estado
        REPLACE usuarioe WITH m.usuarioe
    ENDIF
    UNLOCK        
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT ContC
 DO Vista
RETURN



PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 ACTIVATE WINDOW Wind_C0
 SELECT ContC
 DO pantalla
 SCATTER MEMVAR 
 oi=carrec()
 IF LASTKEY()=27 OR !OI
   SELECT ContC
   DO vista
   RETURN
ENDIF
m.Feccie=DATE()
ingreso = .T.
SELECT ContC
 @ 16,16 GET m.Feccie      PICTURE '@E' VALID m.feccie>=m.fecrec
 @ 18,16 GET m.totinu      PICTURE "999"
 m.usuarioe=wuser_id
 m.estado='20'
 READ VALID Val_Read()
 IF LASTKEY()#27
    vAnula = .F.
    SELECT ContC
    IF RLOCK()
        REPLACE feccie   WITH m.feccie
        REPLACE totinu   WITH m.totinu
        REPLACE estado   WITH m.estado
        REPLACE usuarioe WITH m.usuarioe
        UNLOCK        
       SELECT ContC
    ELSE
       GO BOTTOM
    ENDIF
 ELSE
    DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT ContC
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
 RELEASE    MENU   mMenu
 CLOSE DATA
 ON KEY
 RESTORE SCREEN FROM PRINCIPAL
RETURN

PROCEDURE Lista
*--------------
 SELECT ContC
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
vtiplis = 1
xAno = VAL(m.Periodo)
mmes  = m.nummes
vfecini = DATE()
vfecfin = DATE()
vcodctc = SPACE(14)
vcodctc = m.codctc
SET UDFPARMS TO REFERENCE 
DEFINE WINDOW lis FROM 4,10 TO 18,70 FLOAT DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 01,01 SAY "  Tipo Listado : "  GET vtiplis FUNCTION '^ Res£men por Cuenta Corriente'
@ 05,01 SAY "      Cta.Cte. : "  GET vcodctc   VALID ;
		Val_Fun('Caja','CodCtc','codctc+descri',vCodctc,1,5,19,'descri',27)
@ 07,01 SAY "  Rango Fechas : "  GET vfecini  WHEN vtiplis = 1 PICTURE "@D"
@ 07,27 SAY "  Al  " GET vfecfin  WHEN vtiplis = 1 PICTURE "@D" VALID vfecfin>=vfecini
@ 11,15 GET okcancel FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW lis
 IF LASTKEY()=27
    RETURN
 ENDIF

IF okcancel = 1
 DO CASE
     CASE vtiplis = 1
         SELECT ContC
         vord = ORDER()
         vrec = RECNO()
         vind = SYS(3)+'.idx'
         INDEX ON codctc+inichq TO (vind)  FOR BETWEEN(Feccie,vfecini,vfecfin) AND  ;
         								codctc = vcodctc AND estado='20'
         GO TOP
         DO REPORTE WITH 2,"Ciechq"," Resumen de Control de Chequeras por Cuenta Corriente ",1,.F.,.T.
		 SELECT ContC
         SET INDEX TO
		 SET ORDER TO (vord)
 ENDCASE 
ENDIF
SELECT ContC
GO vrec
DO vista
RETURN



FUNCTION MFecha
*--------------
PARAMETERS xmes, xano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,xMes*9-8,9)) + ' ' + STR(xAno,2)

   
PROCEDURE carrec
*---------------
* Carga valores de Transferencia de Fondos
vfun = .T.
SELE Contc
OK   = FOUND()
vord=ORDER()
SET ORDER TO contchq3
GO TOP
IF EOF()
	DO standby WITH 'No existe Recepci¢n de chequeras'
	SET FILT TO
	RETURN .F.
ELSE
  GO BOTTOM
** Cambio de POPUP con BROWSE
   ON KEY LABEL F10 KEYBOARD CHR(23)
   DEFINE WINDOW Elicg FROM 1,1 TO 18,79 TITLE " Elija la solicitud de chequeras con F10 "
   BROWSE NOED WINDOW Elicg COLOR SCHEME 10 FIELDS ;
   Nummes   :H= 'Mes',;
   Numero   ,;
   CodCtc   :H= 'Cta_Cte',;
   inichq   :H= 'Desde N§',;
   finchq   :H='Hasta N§',;
   fecrec
ENDIF
 @  0,16 SAY numero
 @  2,16 SAY Periodo
 @  4,16 SAY Fecha
 @  6,16 SAY NumMes
 @  6,23 SAY IIF(!EMPTY(nummes),Val_Para(NumMes,'FECMES','D',1,23),' ')
 @  8,16 SAY nroofi+'.'+mesofi
 @ 10,16 SAY Codctc
 @ 10,32 SAY IIF(!EMPTY(codctc),Val_Fun("Caja","CodCtc","Descri",Codctc),' ')
 @ 12,16 SAY inichq
 @ 12,56 SAY finchq
 @ 14,16 SAY nrochq PICTURE "999"
 @ 14,56 SAY serie PICTURE '999' 
SET ORDER TO (vord)
RETURN .T.