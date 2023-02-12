* ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
* ³ ConLic.PRG  18/11/96                               L: 0372   ³	
* ³ Cat logo de Concursos de Licitaci¢n                          ³
* ³ AUTOR   : Ing. Federico Montero Valdiviezo REGION GRAU       ³
* ³ Adecuaci¢n :   A.S. Oswaldo Arturo Oliva Carl¡n.             ³
* ÔÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¾

PUBLIC velige

DEFINE WINDOW Welige FROM 13,10 TO 17,70  COLOR SCHEME 10
ACTIVATE WINDOW Welige
velige='C'
@ 1, 05 SAY "Concurso / Otro :" GET velige  PICTURE '@M C,O' 
READ VALID val_read()
DEACTIVATE WINDOW Welige

USE Parmae   IN 1   ORDER TAG Parmae1  ALIAS Parma
IF velige='C'
	USE ConLic   IN 2   ORDER TAG ConLic1  ALIAS ConLic
ELSE
	USE ConLic   IN 2   ORDER TAG ConLic2  ALIAS ConLic
ENDIF
USE MaePre   IN 16  ORDER TAG MaePre1  ALIAS MaePre
USE ItePar   IN 25  ORDER TAG ItePar1  ALIAS ItePar          


*- Variables de trabajo (registro a trabajar)
SELECT ConLic
GO BOTTOM
SCATTER MEMVAR BLANK         && Crea variables en blanco
PUBLIC vcodprg,vcodsub,vproyec,vsubpry,vcodact,xmes,xano
vCodPrg  = space(2)  
vCodSub  = space(3)    
vProyec  = space(3) 
vSubpry  = space(4)
vCodAct  = space(3)
*- Mensajes de aviso al usuario
Vmens01 = ' Registro de Concursos de Licitaci¢n '
Vmens02 = 'Revisi¢n de Concursos de Licitaci¢n'
Vmens04 = 'Dicho Concurso no fue encontrado'
Vmens05 = 'No existe Concurso anterior'
Vmens06 = 'No existe Concurso siguiente'
Vmens07 = '¨ Desea ELIMINAR este Concurso ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Concurso ha sido anulado'
Vmens10 = 'El Concurso ya fue atendido'


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
 IF velige='C'
 	@ 01,02 SAY "Concurso Licitaci¢n :"
 ELSE
 	@ 01,02 SAY "Descripc. Actividad :"
 ENDIF
 @ 02,02 SAY " Periodo Calendario :"
 @ 03,02 SAY "     Mes Calendario :"
 @ 04,02 SAY "        Descripci¢n :"
 
 @ 06,02 SAY "   Cadena Funcional :" 
 @ 07,02 SAY "          Fte. Fto. :"
 @ 08,02 SAY "            Funci¢n :"
 @ 09,02 SAY "           Programa :"
 @ 10,02 SAY "        SubPrograma :"
 @ 11,02 SAY "    Activ./Proyecto :"
 @ 12,02 SAY "         Componente :"

 @ 15,02 SAY "              Fecha :"
 @ 16,02 SAY "    Precio de Bases :"  
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT ConLic
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 IF !EMPTY(m.codcad)
 	=val_codcad(ALLTRIM(m.codcad),m.periodo+'01001','C')
 ENDIF
 @ 01,24 SAY m.Conlic
 @ 02,24 SAY m.periodo
 @ 03,24 SAY m.nummes
 @ 03,30 SAY val_para(m.nummes,'FECMES','D',3,30)
 @ 04,24 SAY m.deslic FUNCTION "!" PICTURE '@S52' 
 IF !EMPTY(m.codcad)
 	@ 06,24 SAY m.codcad
 	@ 06,30 SAY Val_codcad(m.codcad,m.periodo+'01001','D',28,47) 
 ENDIF
 @ 07,24 SAY Val_para(m.CodFte,'CODFTE','V',22) 
 IF !EMPTY(m.codcad) .AND. m.conlic<='0027'
 	@  8,24 SAY Val_para(maepre.codfun,'CODFUN','V',22,47)
 	@  9,24 SAY Val_para1(maepre.CodPrg,'CODPRG'+maepre.CodFun,'V',22,47)
 	@ 10,24 SAY Val_para1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,47)
 	@ 11,24 SAY Val_para(maepre.actpry,'ACTPRY','V',22,47)
 	@ 12,24 SAY Val_para(maepre.codcom,'CODCOM','V',22,47)
 ENDIF
 @ 15,24 SAY m.Fecha
 @ 16,24 SAY m.vtabase PICTURE "999,999,999.99"
 RETURN


PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE ConLic
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
 conlic  :H='N§ Concurso' ,;
 deslic  :H='Descripci¢n' ,;
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
 SELECT ConLic

 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF

 Vtemp = RECNO()

 STORE SPACE(4) TO vGen

 ACTIVATE WINDOW standby
 @ 1,03 SAY "   N§ Concurso Licitaci¢n :" GET vGen
 READ
 DEACTIVATE WINDOW standby

 IF EMPTY(vGen) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK vGen
    IF !FOUND()
       DO standby WITH "El concurso no est  registrado"
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
 SELECT ConLic
 SCATTER MEMVAR
 =val_cODCAD(ALLT(m.codcad),m.periodo+'01001','C')
 IF RLOCK() OR F_LOCK(1)
   	  @ 01,24  SAY m.Conlic
	  @ 02,24  GET m.periodo PICTURE "!!"
	  @ 03,24  GET m.nummes PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',24,9) 
	  @ 04,24  GET m.deslic PICTURE '@S52' 

      @ 06,24 GET m.CodCad    PICTURE '!!!!' valid val_codcad(m.codcad,m.periodo+'01001',' ',24,45)
 	  @ 07,24 GET m.CodFte    PICTURE '!!'   VALID VAL_PARA(m.CodFte ,'CODFTE',' ',24,45) 

      READ VALID val_read()
	  IF LASTKEY()=27 
		DO vista
		RETURN
	  ENDIF

 	@  8,24 SAY Val_para(maepre.codfun,'CODFUN','V',22,47)
 	@  9,24 SAY Val_para1(maepre.CodPrg,'CODPRG'+maepre.CodFun,'V',22,47)
 	@ 10,24 SAY Val_para1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,47)
 	@ 11,24 SAY Val_para(maepre.actpry,'ACTPRY','V',22,47)
 	@ 12,24 SAY Val_para(maepre.codcom,'CODCOM','V',22,47)

	  @ 15,24  GET m.Fecha
	  @ 16,24  GET m.vtabase PICTURE "999,999,999.99"
	  READ VALID Val_Read()
  
  IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
	 SELECT ConLic
	 GATHER MEMVAR
  ENDIF
ENDIF	
DO Vista                    && Muestra nuevos datos
UNLOCK
RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT ConLic
 DO Pantalla
 SCATTER MEMVAR BLANK
 m.tipo=velige
 m.fecha =DATE()
 VMes = MONTH(m.fecha)
 IF Vmes<10
    m.nummes  ='0'+ALLTRIM(STR(vmes))
 ELSE
    m.nummes  =STR(VMES,2)   
 ENDIF   
 m.periodo      = RIGHT(DTOC(m.fecha),2) 
 SELECT Parma
 IF velige='C'
 	SEEK "CORREL"+"CONLIC"
 ELSE
 	SEEK "CORREL"+"OTRACT"
 ENDIF
 m.conlic=nument+1
 m.conlic=PADL(m.conlic,4,'0')
 SELE ConLic
  @ 01,24  SAY m.Conlic
  @ 02,24  GET m.periodo PICTURE "!!"
  @ 03,24  GET m.nummes PICTURE '!!'  VALID val_para(m.nummes  ,'FECMES',' ',24,9) 
  @ 04,24  GET m.deslic PICTURE '@S52' 
  
  @ 06,24 GET m.CodCad    PICTURE '!!!!' valid val_codcad(m.codcad,m.periodo+'01001',' ',24,45)
  @ 07,24 GET m.CodFte    PICTURE '!!'   VALID VAL_PARA(m.CodFte ,'CODFTE',' ',24,45) 

  READ VALID val_read()
  IF LASTKEY()=27 
	DO vista
	RETURN
  ENDIF

 	@  8,24 SAY Val_para(maepre.codfun,'CODFUN','V',22,47)
 	@  9,24 SAY Val_para1(maepre.CodPrg,'CODPRG'+maepre.CodFun,'V',22,47)
 	@ 10,24 SAY Val_para1(maepre.CodSPr,'CODSPR'+maepre.CodPrg,'V',22,47)
 	@ 11,24 SAY Val_para(maepre.actpry,'ACTPRY','V',22,47)
 	@ 12,24 SAY Val_para(maepre.codcom,'CODCOM','V',22,47)

  @ 15,24  GET m.Fecha
  @ 16,24  GET m.vtabase PICTURE "999,999,999.99"

  READ VALID Val_Read()
  
 IF LASTKEY() # 27
    IF F_Appd()
       GATHER MEMVAR
    ELSE
       GO BOTTOM
    ENDIF
 ELSE
   DO STANDBY WITH 'Proceso cancelado, no se graba nada'
   GO BOTTOM
 ENDIF
 UNLOCK ALL
 SELECT ConLic
 SELECT Parma
 IF velige='C'
 	SEEK "CORREL"+"CONLIC"
 ELSE
 	SEEK "CORREL"+"OTRACT"
 ENDIF 
 IF RLOCK()
   REPLACE NumEnt WITH NumEnt + 1
   UNLOCK
 ENDIF	  
 DO Vista
RETURN


PROCEDURE Elimi
*--------------
 SELECT ConLic
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE este Concurso ?')
	 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
	   DELETE NEXT 1
	   GO BOTTOM
	   DO Vista
	 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
SELE ConLic
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
vrec = RECNO()
GO TOP
DO REPORTE with 2,"ConLic",' Listado de Concursos de Licitaci¢n '
GO vrec
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