*--------------------------------------------------------------------------
* RegPre.Prg
* Registra Presupuestos
* Autor: Ing. Luis Castillo Dur n
*--------------------------------------------------------------------------
*- Abriendo Archivos
PARAMETER vsistema
CLOS DATA
USE Parmae   IN 1   order tag Parmae1  ALIAS Parma
USE Maepre   IN 2   order tag Maepre1  ALIAS MAEPRE
*USE calen    IN 3   order tag calen3   ALIAS calen

*- Mensajes de aviso al usuario
Vmens01 = 'Registro de Presupuesto'
Vmens02 = ' Presupuesto : REVISION '
Vmens04 = 'Dicho Presupuesto no fue encontrado'
Vmens05 = 'No existe Presupuesto anterior'
Vmens06 = 'No existe Presupuesto siguiente'
Vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Este Presupuesto ha sido anulado'
Vmens10 = 'El Presupuesto ya est  Atendido'
Vmens11 = 'El Presupuesto ha sido devuelto'

SELECT MAEPRE
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco

*- Inicia proceso
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

RETURN


PROCEDURE Inicia             && Crea ventanas, men£s y t¡tulos
*---------------
 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79  DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10


 DEFINE WINDOW Wind_1 FROM 00,00 TO 11,79  DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

 DEFINE WINDOW Wind_2 FROM 12,00 TO 23,79 DOUBLE ;
 TITLE 'Detalle: Presupuesto' ;
 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\<Eliminar'   AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\<Listar '    AT 24,63
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

 @  1, 2 SAY "          Periodo :"
 @  1,40 SAY "Corr. cadena Fun. :"
 @  3, 2 SAY "   Unidad Gestora :"
 @  4, 2 SAY " Unidad Ejecutora :"
 @  5, 2 SAY "          Funci¢n :"
 @  6, 2 SAY "         Programa :"
 @  7, 2 SAY "      SubPrograma :"
 @  8, 2 SAY "  Activ./Proyecto :"
 @  9, 2 SAY "       Componente :"
 @ 11, 2 SAY "             Meta :"
 @ 12, 2 SAY "        Finalidad :"
 @ 13, 2 SAY "      Descripci¢n :"
 IF vsistema = "2"
 	@ 16, 2 SAY " C¢digo Actividad :"
 ENDIF	

RETURN

PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT MAEPRE
 STORE 0 TO vAsigna
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0

 SCATTER MEMVAR
 *@  0,60 SAY IIF( m.Estado= '00','Emitido  ',IIF( m.Estado = '20','         ',IIF(m.Estado='99','Anulado  ',IIF(m.Estado='50','Atendido ',' ------- '))))
 @  1,22 SAY m.Periodo
 @  1,60 SAY m.CODCAD
 @  3,22 SAY VAL_PARA(m.uniges,'UNIGES','V',22,40)
 @  4,22 SAY VAL_PARA1(m.unieje,'UNIEJE'+ALLT(m.uniges),'V',22,40)
 @  5,22 SAY VAL_PARA(m.codfun,'CODFUN','V',22,40)
 @  6,22 SAY VAL_PARA1(m.CodPrg,'CODPRG'+ALLT(m.codfun),'V',22,40)
 @  7,22 SAY VAL_PARA1(m.CodSPr,'CODSPR'+ALLT(m.codPrg),'V',22,40)
 @  8,22 SAY VAL_PARA(m.actpry,'ACTPRY','V',22,40)
 @  9,22 SAY VAL_PARA(m.codcom,'CODCOM','V',22,40)
 @ 11,22 SAY m.codmet
 @ 12,22 SAY m.codfin
 @ 13,22 SAY SUBSTR(m.Descri,1,55)
 @ 14,22 SAY SUBSTR(m.Descri,56,55)
 IF vsistema = "2"
 	 @ 16,22 SAY m.cod_act
 ENDIF	

 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE MAEPRE
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
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 CODCAD  :H='Cad.Fun.' ,;
 uniges  :H='UG',;
 unieje  :H='UE',;
 codfun  :H='Fn',;
 codprg  :H='Prg',;
 codspr:H='SubPrg',;
 actpry  :H='Act/Pry',;
 codcom  :H='Compon.',;
 codmet  :H='Meta',;
 codfin  :H='Finalid.',;
 xx=IIF(VSISTEMA="2",cod_act," "):H='Actividad',;
 descri:H='Descripci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 SELE MAEPRE
 DO Vista
 RETURN


PROCEDURE busca              && Realiza b£squeda directa
*--------------
IF EOF()
	DO standby WITH vmens08
	RETURN
ENDIF
vtemp    = RECNO()
vperiodo = RIGHT(DTOC(DATE()),2)
vcodfte  = '  '
vcodcad  = '    '
vuniges  = '  '
vunieje  = '   '
DEFINE WINDOW lisTA FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5

ACTIVATE WINDOW lisTA

@  1,2 SAY ' Periodo : ' GET vperiodo   PICTURE '!!'
@  2,2 SAY 'UNI.GES. : ' GET vUniGes	PICTURE '!!'	VALID VAL_PARA(Vuniges,'UNIGES',' ',14,30)
@  3,2 SAY 'UNI.EJE. : ' GET vUniEje 	PICTURE '!!!'	VALID VAL_PARA1(vunieje,'UNIEJE'+vuniges,' ',14,30)
@  4,2 SAY '  Cadena : ' GET vcodcad    PICTURE '!!!!'  VALID val_codcad(vcodcad,vperiodo+ALLT(vuniges)+ALLT(vunieje),' ',14,30)

READ VALID val_read()

DEACTIVATE WINDOW lisTA

IF EMPTY(vperiodo) .OR. LASTKEY()=27
	RETURN
ELSE
	SET ORDER TO 1
	SEEK vperiodo+ALLTRIM(vuniges)+ALLTRIM(vunieje)+ALLTRIM(vcodcad)
	IF !FOUND()
		DO standby WITH vmens04
		GOTO vtemp
	ELSE
		DO vista
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
 IF Estado = '99'
   * Anulada
   DO STANDBY WITH Vmens09
   RETURN
 ENDIF
 IF Estado = '70'
   * El Presupuesto ha sido devuelto
   DO STANDBY WITH Vmens11
   RETURN
 ENDIF
 IF Estado = '50'
   DO STANDBY WITH Vmens12
   RETURN
 ENDIF
 *****
 SELECT MAEPRE
 SCATTER MEMVAR
 vDescri1 = SUBSTR(m.Descri,1,55)
 vDescri2 = SUBSTR(m.Descri,56,55)
 ACTIVATE WINDOW Wind_0

 @  1,22 SAY m.Periodo
 @  1,60 SAY m.CODCAD
 @  3,22 GET m.uniges PICTURE '!!' 		VALID 	VAL_PARA(m.uniges,'UNIGES',' ',22,40)
 @  4,22 GET m.unieje PICTURE '!!!'		VALID 	VAL_PARA1(m.unieje,'UNIEJE'+ALLT(m.uniges),' ',22,40)
 @  5,22 GET m.codfun PICTURE '!!' 		VALID 	VAL_PARA(m.codfun,'CODFUN',' ',22,40)
 @  6,22 GET m.codprg PICTURE '!!!'		VALID 	VAL_PARA1(m.CodPrg,'CODPRG'+ALLT(m.codfun),' ',22,40)
 @  7,22 GET m.codspr PICTURE '!!!!'	VALID 	VAL_PARA1(m.CodSPr,'CODSPR'+ALLT(m.codPrg),' ',22,40)
 @  8,22 GET m.actpry PICTURE '!!!!!!'	VALID 	VAL_PARA(m.actpry,'ACTPRY',' ',22,40)
 @  9,22 GET m.codcom PICTURE '!!!!!'	VALID 	VAL_PARA(m.codcom,'CODCOM',' ',22,40)
 @ 11,22 GET m.codmet
 @ 12,22 GET m.codfin
 @ 13,22 GET vDescri1
 @ 14,22 GET vDescri2
 IF vsistema = "2"
 	  @ 16,22 GET m.cod_act PICTURE '!!!!!!!!!'
 ENDIF	

 READ VALID Val_Read()

 IF LASTKEY() # 27
    m.Estado = '00'
    m.fecemi = date()
    m.DesCri = vDescri1+vDescri2
    IF LASTKEY() # 27
       SELECT MAEPRE
       GATHER MEMVAR
    ELSE
      *-??
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado'
 ENDIF
 UNLOCK ALL
 SELECT MAEPRE
 DO PANTALLA
 DO Vista
 RETURN

PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT MAEPRE
 V=RECNO()
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.periodo = PADL(ALLTRIM(str(year(DATE())-2000)),2,"0")
 store space(55) to vDescri1 , vdescri2
 *= actcodcad()
 @  1,22 GET m.Periodo   PICTURE '!!'
 @  1,60 GET m.CODCAD PICTURE '!!!!'
 @  3,22 GET m.uniges PICTURE '!!' 		VALID 	VAL_PARA(m.uniges,'UNIGES',' ',22,40)
 @  4,22 GET m.unieje PICTURE '!!!'		VALID 	VAL_PARA1(m.unieje,'UNIEJE'+ALLT(m.uniges),' ',22,40)
 @  5,22 GET m.codfun PICTURE '!!' 		VALID 	VAL_PARA(m.codfun,'CODFUN',' ',22,40)
 @  6,22 GET m.codprg PICTURE '!!!'		VALID 	VAL_PARA1(m.CodPrg,'CODPRG'+ALLT(m.codfun),' ',22,40)
 @  7,22 GET m.codspr PICTURE '!!!!'	VALID 	VAL_PARA1(m.CodSPr,'CODSPR'+ALLT(m.codPrg),' ',22,40)
 @  8,22 GET m.actpry PICTURE '!!!!!!'	VALID 	VAL_PARA(m.actpry,'ACTPRY',' ',22,40)
 @  9,22 GET m.codcom PICTURE '!!!!!'	VALID 	VAL_PARA(m.codcom,'CODCOM',' ',22,40)
 @ 11,22 GET m.codmet
 @ 12,22 GET m.codfin
 @ 13,22 GET vDescri1
 @ 14,22 GET vDescri2
 IF vsistema = "2"
 	  @ 16,22 GET m.cod_act PICTURE '!!!!!!!!!'	VALID 	VAL_CODACT(m.cod_act)
 ENDIF	
 READ Valid Val_read()
 IF LASTKEY() = 27
   	DO STANDBY WITH 'Proceso cancelado'
    GO V
    DO VISTA
    RETUR
 ELSE
 	SEEK m.periodo+m.codcad+m.uniges+m.unieje
   	IF Found()
    	DO STANDBY WITH 'Ya est  registrado.'
    	GO V
    	DO PANTALLA
    	DO Vista
    	RETURN
   	endif
    IF F_Appd()
       m.Estado = '00'
       m.fecemi = date()
       m.DesCri = vDescri1+vDescri2
       IF LASTKEY() # 27
          SELECT MAEPRE
          GATHER MEMVAR
          *SELECT parma
          *SEEK 'CORRELCODCAD'
          *REPLACE nument WITH nument+1
          *SELECT Presu
       ELSE
         *-??
       ENDIF
    ELSE
       *- No pudo agregar la liquidaci¢n
       SELECT MAEPRE
       GO BOTTOM
    ENDIF
 ENDIF
 UNLOCK ALL
 SELECT MAEPRE
 DO Vista
 RETURN

FUNCTION actcodcad
*------------------
valias =  ALIAS()
SELECT parma
SEEK 'CORRELCODCAD'
IF FOUND()
	m.codcad = PADL(nument+1,4,'0')
ELSE
	DO standby WITH 'No est  inicializado el correlativo de cadena'
	m.codcad = '    '
ENDIF	
SELECT (valias)
RETURN

PROCEDURE Elimi
*---------------
SELECT MAEPRE
IF EOF()
	DO standby WITH Vmens08
	RETURN
ENDIF
IF !Estado # '  00'
	* ya pas¢
	DO STANDBY WITH Vmens10
	RETURN
ENDIF
velimina = YESNO('¨ Desea ELIMINAR ‚ste Presupuesto ?')
IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	dele next 1
	SKIP
	IF EOF()
		GO BOTT
	ENDIF
	DO Vista
ENDIF
UNLOCK all
RETURN

PROCEDURE Lista
*--------------
select MAEPRE
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
   DO cadfun
Endif
SELE MAEPRE
GO vtemp
do Vista
RETURN


PROCEDURE cadfun
*---------------
vTemo = RECNO()
DEFINE WINDOW LIS FROM 4,15 TO 20,65 FLOAT DOUBLE TITLE 'Listado Actividad / Proyecto' COLOR SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro
vCODCAD = SPAC(4)
vperiodo = '  '
store space(2) to vUniges
store space(3) to vUniEje

@ 00,01 SAY "          Per¡odo : " GET vPeriodo 

@ 02,01 SAY '   Unidad Gestora : ' GET vUniges 	PICTURE '!!'    	VALID val_para(vUniGes,'UNIGES',' ',22,20)
@ 04,01 SAY ' Unidad Ejecutora : ' GET vUnieje    PICTURE '!!!' 	VALID val_para1(vUniEje,'UNIEJE'+vUniGes,' ',22,20)

@ 06,01 SAY " Cadena Funcional : "
@ 06,22 GET vCODCAD  PICTURE '!!!!' VALID IIF(!EMPTY(VCODCAD),VAL_codcad(vcodcad,vperiodo+ALLT(vUniges)+ALLT(vUnieje),' ',22,20),.T.) 

@ 08,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   vTitulo=' En General '
   IF !EMPTY(VCODCAD)
   		SET FILTER TO PERIODO+UNIGES+UNIEJE+CODCAD = VPERIODO+ALLT(VUNIGES)+ALLT(VUNIEJE)+ALLT(vcodcad)
   ELSE
		SET FILTER TO PERIODO+UNIGES+UNIEJE = VPERIODO+ALLT(VUNIGES)+ALLT(VUNIEJE)
   ENDIF
   IF !EOF()
     DO REPORTE WITH 2,"CADFUN",'Cadenas Funcionales'
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   SET FILTER TO 
ENDIF
GO vTemo
DO VISTA
RETURN

PROCEDURE ValCad
*--------------
SELECT PRESU
 vTem=recno()
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
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 CODCAD  :H='Cad.Fun.' ,;
 uniges  :H='UG',;
 unieje  :H='UE',;
 codfun  :H='Fn',;
 codprg  :H='Prg',;
 codspr:H='SubPrg',;
 actpry  :H='Act/Pry',;
 codcom  :H='Compon.',;
 codmet  :H='Meta',;
 codfin  :H='Finalid.',;
 descri  :H='Describci¢n'
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 VCLI=CodCad
 SHOW MENU mMenu
 ON KEY LABEL F10
 GOTO Vtemp
 RETURN .t.


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

PROCEDURE Trimestre
*------------------
parameter vFecha

DO CASE
   CASE MONTH(vFecha) = 1  OR MONTH(vFecha) = 2   OR  MONTH(vFecha) = 3
        vTrim = '1'
   CASE MONTH(vFecha) = 4  OR MONTH(vFecha) = 5   OR  MONTH(vFecha) = 6
        vTrim = '2'
   CASE MONTH(vFecha) = 7  OR MONTH(vFecha) = 8   OR  MONTH(vFecha) = 9
        vTrim = '3'
   CASE MONTH(vFecha) = 10 OR MONTH(vFecha) = 11  OR  MONTH(vFecha) = 12
        vTrim = '4'
ENDCASE

return .t.

FUNCTION VAL_CODACT
*------------------
PARAMETER vcodigo
vorder = ORDER()
valias = ALIAS()
vrecno = RECNO()
SELECT maepre
SET ORDER TO maepre5
SEEK vcodigo
IF FOUND()
	DO STANDBY WITH 'Ya existe esta actividad'
	RETURN .F.
ENDIF
RETURN .T.	
