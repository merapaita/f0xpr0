*- Abriendo Archivos
SET PROC TO SAI_LIB
*- Mensajes de aviso al usuario
Vmens01 = ' Libros : RECEPCION '
Vmens02 = 'Revisi¢n de Libros'
Vmens04 = 'Dicha Libro no fue encontrada'
Vmens05 = 'No existe Libro anterior'
Vmens06 = 'No existe Libro siguiente'
Vmens07 = '¨ Desea ELIMINAR ‚sta Libro ?'
Vmens08 = 'No hay registros para procesar'
Vmens09 = 'Esta Libro ha sido anulada'
Vmens10 = 'La Libro ya fue atendida'

USE LIBROS  IN 1   order tag LIBROS2  ALIAS LIBRO
USE parmae  IN 2   ORDER TAG parmae1  ALIAS parma
GO BOTTOM

*- Variables de trabajo (registro a trabajar)
SCATTER MEMVAR BLANK         && Crea variables en blanco
on key label f9 do estado
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
 @  1,40 SAY "           C¢digo :"
 @  2, 2 SAY "          Periodo :"
 @  4, 2 SAY "   Tipo Documento :"
 @  6, 2 SAY "       Periocidad :"
 @  8, 2 SAY "  Tipo Periocidad :"  
 @ 10, 2 SAY "           Fuente :"  
 @ 12, 2 SAY "      Descripci¢n :"
 @ 16, 2 SAY "      Observaci¢n :"
 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 SELECT Libro
 IF EOF()
   DO Pantalla
   RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 SCATTER MEMVAR
 @  0,60 SAY IIF(m.Estado='00','Disponible','No Disponi.')
 @  1,60 say M.CODIGO
 @  2,22 SAY m.Periodo    PICTURE 'XX'
 @  4,22 SAY VAL_PARA(m.TIPDOC,'PREDOC','D',22,40) 
 @  6,22 SAY VAL_PARA(m.Periocidad,'PREPER','D',22,40)
 @  8,22 SAY m.Numper     picture '@!'
 @ 10,22 SAY VAL_PARA(m.Prefte,'PREFTE','D',22,40)
 @ 12,22 SAY m.Descri     picture '@!'
 @ 16,22 say m.Observa    picture '@!'
 @ 21,00 SAY PADC(' ° ®F9¯ Cambia de Estado ° ',79,' ') COLOR W+/B
 RETURN

PROCEDURE Revis              && Revisi¢n de BD en browse
*--------------
SELE Libro
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
 Periodo   :H='Per' ,;
 TipDoc    :H='Doc' ,;
 Periocidad:H='Tp',;
 NumPer    :H='N§',;
 Prefte    :H='Fte',;
 Descri    :H='Descripcion'
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
 SELECT Libro
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF

 Vtemp = RECNO()
 STORE SPACE(2) TO vPer
 STORE SPACE(2) TO vDoc
 STORE SPACE(1) TO vPrc
 
 activate window standby
 @ 0,03 SAY "        Periodo :" GET vPer PICT '!!' valid !empty(vper)
 @ 1,03 say " Tipo documento :" GET vDoc PICT '!!!' VALID VAL_PARA(vdoc,'PREDOC','C',22,40) 
 @ 2,03 say "     Periocidad :" GET vPrc PICT '!' VALID VAL_PARA(vPrc,'PREPER','C',22,40) 
 READ
 deactivate window standby

 IF LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK vPer+vDoc+vPrc
    IF !FOUND()
       DO standby WITH "Dicho Libro no est  registrada"
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
 SELECT Libro
 SCATTER MEMVAR
 IF RLOCK() OR F_LOCK(1)

  @  2,22 GET m.Periodo    PICTURE 'XX' VALID !EMPTY(M.PERIODO) DISA
  @  4,22 GET m.Tipdoc     picture 'XXX' VALID VAL_PARA(m.Tipdoc,'PREDOC',' ',22,40) DISA
  @  6,22 GET m.Periocidad PICTURE 'X'  VALID VAL_PARA(m.Periocidad,'PREPER',' ',22,40) 
  @  8,22 GET m.Numper     picture '@!'  
  @ 10,22 GET m.PreFte     picture '!!!' valid VAL_PARA(m.Prefte,'PREFTE',' ',22,40)  
  @ 12,22 GET m.Descri     picture '@S56'
  @ 16,22 GET m.Observa    picture '@S56'
  READ VALID Val_Read()
  IF LASTKEY() # 27          && Si se presion¢ Esc, no graba
     SELECT Libro
     M.CODIGO = ALLTRIM(M.PERIODO)+ALLTRIM(M.TIPDOC)+ALLTRIM(M.PERIOCIDAD)+ALLTRIM(M.NUMPER)+ALLTRIM(M.PREFTE)
     GATHER MEMVAR
  ENDIF
  DO Vista                    && Muestra nuevos datos
 ENDIF
 UNLOCK
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
 SELECT Libro
 DO Pantalla
 SCATTER MEMVAR BLANK
  @  1,60 say space(10)
  @  2,22 GET m.Periodo    PICTURE 'XX' VALID !EMPTY(M.PERIODO)
  @  4,22 GET m.Tipdoc     picture 'XXX' VALID VAL_PARA(m.Tipdoc,'PREDOC',' ',22,40)
  @  6,22 GET m.Periocidad PICTURE 'X' VALID VAL_PARA(m.Periocidad,'PREPER',' ',22,40)
  @  8,22 GET m.Numper     picture '@!'
  @ 10,22 GET m.PreFte     picture '!!!' valid VAL_PARA(m.Prefte,'PREFTE',' ',22,40)    
  @ 12,22 GET m.Descri     picture '@S56'
  @ 16,22 GET m.Observa    picture '@S56'    
 READ VALID Val_Read()
 IF LASTKEY() # 27
    SELEC LIBRO
    M.CODIGO = ALLTRIM(M.PERIODO)+ALLTRIM(M.TIPDOC)+ALLTRIM(M.PERIOCIDAD)+ALLTRIM(M.NUMPER)+ALLTRIM(M.PREFTE)
    SEEK M.CODIGO
    IF !FOUND()
       IF F_Appd()
          m.estado='00'
          GATHER MEMVAR
       ELSE
         GO BOTTOM
       ENDIF
    ELSE
       DO STANDBY WITH 'Proceso cancelado, ya est  registrado' 
    ENDIF   
 ELSE
   DO STANDBY WITH 'Proceso cancelado, no se graba nada'
   GO BOTTOM
 ENDIF
 UNLOCK ALL
 SELECT Libro
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
 SELECT Libro
 IF EOF()
   DO standby WITH Vmens08
   RETURN
 ENDIF
 velimina = YESNO('¨ Desea ELIMINAR FISICAMENTE ‚sta libro ?')
 IF vElimina .AND. ( RLOCK() .OR. F_Lock(1) )
   DELETE NEXT 1
   GO BOTTOM
   DO Vista
 ENDIF
 UNLOCK
 RETURN

PROCEDURE Lista
*--------------
select Libro
vtemp =recno()
if eof()
   do standby with vmens08
   return
else
    SET FILT TO ESTADO='00'
    do reporte with 2,"libros",' Listado de Clasificador por Objeto del Gasto '
    SET FILT TO
Endif

SELE Libro
GO vtemp
do Vista
RETURN

PROCEDURE CLAS
*---------------
vTemo = RECNO()
DEFINE WINDOW LIS FROM 5,15 TO 19,65 FLOAT DOUBLE TITLE 'Listado de las Libros' color SCHEME 5
ACTIVATE WINDOW LIS
STORE 1        TO vToCLI,vORDEN,vTipPro
vCli = SPAC(4)
@ 01,01 SAY "Todas las Libros : " GET vTOCLI  FUNCTION '*RNH \<Si;\<No' SIZE 1,10,6   VALID VALTOD(VTOCLI,3,22)
@ 03,01 SAY "           Libro : "
@ 03,22 GET vCli    WHEN VTOCLI=2  PICTURE '!!!!' VALID ValClas()

@ 05,01 SAY "     Ordenado por : " GET vOrden   FUNCTION '^ PartGen'

@ 08,01 SAY "           Estado : " GET vTipPro  FUNCTION '^ Todas'

@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE

RELEASE WINDOW LIS
IF OKCANCEL = 1
   ACTIVATE WINDOW STANDBY
   @ 01,04 SAY 'Espere un momento........'
   vInd = SYS(3) + '.IDX'
   INDEX ON IIF(vOrden=1,Codpart,CodSubp) TO (vInd) ;
      FOR IIF(vToCli=1,.T.,CodPart = vCli) .AND. IIF(vTipPro=1,.T.,'  ') &&iif(vTipPro=2,Estado = '00',Estado = '50' ))
   SET INDEX TO (VIND)
   GO TOP
*   SET RELATION TO PERIODO+NUMOC INTO ITEOC
 *  SET SKIP TO ITEOC
   DEACTIVATE WINDOW STANDBY
   vTitulo=IIF(vTipPro=1,' en General ','  ')  &&IIF(vTipPro=2,' Pendientes ',' Atendidos '))
   IF !EOF()
     DO REPORTE WITH 2,"CLASIF",' Listado de Clasificador por Objeto del Gasto '
   ELSE
     DO STANDBY WITH VMENS08
   ENDIF
   CLOSE INDEX
   ERASE (VIND)
ENDIF

RETURN

PROCEDURE ValClas
*--------------
SELECT Libro
 vTem=recno()
 IF EOF()
    DO standby WITH Vmens08
    RETURN
 ENDIF
* SET RELATION TO PERIODO+NUMOC INTO ITEOC
* SET SKIP TO ITEOC
 Vtemp = RECNO()
 HIDE MENU mMenu
 ACTIVATE SCREEN
 vTempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE WINDOW Wind_0 NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
 codpart  :H='Generica' ,;
 codsubp  :H='Espec¡fica' ,;
 despar   :H='Descripci¢n' 
 vTempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
 DO Logos WITH Rotulo1,vTempo
 IF LASTKEY()=27
   GOTO Vtemp
 ENDIF
 VCLI=CodPart
 SHOW MENU mMenu
 ON KEY LABEL F10
 SET RELATION TO
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

PROCEDURE Estado
*------------------
DO CASE
   CASE ESTADO = '00' &&aprobado
       REPLACE ESTADO WITH '10'
   CASE ESTADO = '10' &&solicitado
       REPLACE ESTADO WITH '00'
ENDCAS
DO VISTA       
RETURN
