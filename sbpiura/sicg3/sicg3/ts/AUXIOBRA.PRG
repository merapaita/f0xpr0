* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ AuxIOBRA.PRG  12/08/96                                       ³	
* ³ Registro de Obras otras                                      ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

USE Parmae  IN 1   ORDER TAG Parmae1  ALIAS parma
USE Obra    IN 2   ORDER TAG Obra2    ALIAS Obra
USE MaePre  IN 3  ORDER tag MaePre1   ALIAS MaePre
DO Inicia
DO Salida
RETURN


PROCEDURE Inicia
*---------------
*- Mensajes de aviso al usuario
PRIVATE Vmens01,Vmens02,Vmens03,Vmens04,Vmens05,Vmens06,Vmens07,Vmens08,Vmens09,Vmens10,Vmens11
Vmens01 = ' Cat logo de Auxiliar OBRAS '
Vmens02 = 'Revisi¢n de Auxiliar OBRAS'
Vmens03 = 'Digite c¢digo de Obra que desea :'
Vmens04 = 'Dicha Obra no fue encontrada.'
Vmens05 = 'No existe Registro anterior.'
Vmens06 = 'No existe Registro siguiente.'
Vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Registro?'
Vmens08 = 'No hay registros para procesar'
SELECT obra
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
*- Inicia proceso
DO reInicia                    && Define ventanas, men£s, t¡tulos
DO rePantalla                  && Muestra pantalla inicial
DO Vista

*- Activa men£ mientras vEn_accion es .T.
STORE .T. TO vEn_accion
DO WHILE vEn_accion
  ACTIVATE SCREEN
  ACTIVATE MENU mMenu
ENDDO

DO Fin_opcion
RETURN


PROCEDURE Salida
*---------------
  ACTIVATE   SCREEN
  CLOSE DATABASE
RETURN


PROCEDURE reInicia             && Crea ventanas, men£s y t¡tulos
*---------------

 ACTIVATE SCREEN
 vTempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
 DO Logos WITH Rotulo1,vTempo

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens01 COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE Vmens02 COLOR SCHEME 10

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


PROCEDURE rePantalla           && Pinta m scara de datos
*-----------------
 ACTIVATE WINDOW Wind_0
 CLEAR
 @  4, 2 SAY "            C¢digo:"
 @  4,50 SAY "Periodo:"
 @  6, 2 SAY "       Descripci¢n:"
 @  9, 2 SAY "         Ubicaci¢n:"
 @ 11, 2 SAY "       Observaci¢n:"
*@ 14, 2 SAY "            Codcal:"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT obra
 SCATTER MEMVAR
 @  4,22 SAY m.Codobra  PICTURE '!!!!'
 @  4,60 SAY m.periodo  picture '!!'
 @  6,22 SAY m.Descri
 @ 09,22 SAY m.ubicac
 @ 11,22 SAY m.Observa
*@ 14,22 SAY m.codcaL
*@ 14,28 SAY val_codcad(ALLTRIM(m.codcaL),m.periodo+'01001','D',28,47)
 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 ACTIVATE WINDOW STANDBY
 vopcion = 'C¢digo'
 @ 1,4 SAY 'Revisi¢n Ordenado por : ' GET vopcion PICTURE '@M Descripci¢n,C¢digo     '
 READ
 DEACTIVATE WINDOW STANDBY
 IF LASTKEY()=27
    DO vista
    RETURN
 ENDIF
 IF vOpcion='Descri'
    SET ORDER TO TAG obra4
 ENDIF
 GOTO vTemp        
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_1 NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
     Codobra   :H='C¢digo',;
     Descri   :H='Descripci¢n',;
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 SHOW MENU mMenu
 ON KEY LABEL F10
 IF vOpcion='Descripci¢n'
    SET ORDER TO TAG obra2
 ENDIF
 DO Vista
 RETURN


PROCEDURE Busca              && Realiza b£squeda directa
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 Vtemp = RECNO()
 STORE SPACE(04) TO vCodigo
 STORE SPACE(100) TO vDescri
 tb=yesno("Desea b£squeda por C¢digo (NO = por Descripci¢n)")
 IF LASTKEY()=27
    DO VISTA
    RETURN
 ENDIF
 ACTIVATE WINDOW standby
 @ 1,1 SAY IIF(tb,"     C¢digo : ","Descripci¢n : ")
 IF tb
    @ 1,15 GET vCodigo PICTURE "!!!!"
 ELSE
    @ 1,15 GET vDescri PICTURE REPLICATE("!",30)
 ENDIF   
 READ
 DEACTIVATE WINDOW standby
 IF tb
   SET ORDER TO TAG obra2
   SEEK  ALLTRIM(vCodigo)
 ELSE
   SET ORDER TO TAG obra4
   SEEK ALLTRIM(vDescri)
 ENDIF
 SET ORDER TO TAG obra2
 IF EOF()
     DO standby WITH "Dich" + IIF(tb, "o C¢digo ","a Descripci¢n ") + "no fu‚ encontrada."
     GOTO Vtemp
 ELSE
     DO Vista
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
 @  4,22 SAY m.Codobra       PICTURE '!!!!'
 @  4,60 SAY m.periodo       PICTURE '!!'
 @  6,22 GET m.Descri
 @  9,22 GET m.ubicac
 @ 11,22 GET m.observa
*@ 14,22 GET m.CodCal  PICTURE '9999' VALID Val_CodCad(m.codcal,m.periodo+'01001',' ',22,47)
 READ
 IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
 	if rlock()
	 	sele obra
	    GATHER MEMVAR
	endif	    
 ENDIF
 UNLOCK ALL
 DO Vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR BLANK
 @  4,22 GET m.Codobra  PICTURE '999999' VALID val_cod(m.Codobra)
 READ
 SELECT obra
 IF LASTKEY() = 27 .OR. m.Codobra=SPACE(4)
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   GO BOTTOM
 ELSE
   SEEK  m.Codobra
   IF FOUND()
     DO standby WITH "Auxiliar ya est  registrado. Proceda a corregir datos."
     DO Vista
     DO Corri
   ELSE
     @  4,60 GET m.periodo
     @  6,22 GET m.Descri
     @  9,22 GET m.ubicac
     @ 11,22 GET m.Observa
     READ VALID VAL_READ()
     IF LASTKEY() <> 27
     	if F_APPD()
     		sele obra
	        GATHER MEMVAR
		ELSE
			GO BOTTOM
		ENDIF
	 ELSE
	 	DO STANDBY  WITH 'Proceso Cancelado, no se  graba'
	 	go bottom
	 ENDIF
   ENDIF
   UNLOCK ALL
 ENDIF
 DO Vista
 RETURN


FUNCTION val_cod
*---------------
PARAMETERS vcod
vcod= PADL(ALLTRIM(vcod),4,'0')
RETURN

PROCEDURE Elimi
*--------------
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 STORE YESNO(Vmens07) TO vElimina
 IF vElimina
     DELETE NEXT 1
     IF !BOF()
       SKIP -1
     ENDIF
 ENDIF
 DO Vista
 RETURN



PROCEDURE Lista
*--------------
DEFINE WINDOW Lis FROM 4,15 TO 24,65 FLOAT DOUBLE TITLE 'Listado Recibo de Ingresos' COLOR SCHEME 5
ACTIVATE WINDOW LIS
 vopcion = 'C¢digo'
 STORE 1         TO vLista
 Vper=m.Periodo
 @ 1,04 SAY 'Listado Ordenado por : ' GET vopcion PICTURE '@M Descri,C¢digo     '
 @ 3,04 SAY "        Tipo Listado : " GET vlista  FUNCTION '^ General;Periodo'
 READ
 sele obra
 nReg = RECNO()
 do case
 	case vopcion='Descri' and vlista=1
 		set order to obra4
	case vopcion='Descri' and vlista=2
		@ 06,04 SAY "             Periodo: " GET vper pict '!!'
		read
		set order to obra3
		seek vper
		if eof()
		   DO standby WITH Vmens08
		endif
		SET FILTER TO PERIODO=VPER
	case vopcion='C¢digo' and vlista=1
		set order to obra2
	case vopcion='C¢digo' and vlista=2
		@ 06,04 SAY "             Periodo: " GET vper pict '!!'
		read
		set order to obra1
		seek vper
		if eof()
		   DO standby WITH Vmens08
		endif
		SET FILTER TO PERIODO=VPER
endcase			
DO REPORTE WITH 2,"Auxobr"," Auxiliares "
SELECT obra
SET ORDER TO TAG obra2
GO nReg
DO Vista
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
  DO Logos WITH Rotulo1, Rotulo2
RETURN


