*- ManPar.prg
*- Mantenimiento general de par metros del sistema (del PARMAE.DBF)
*- Luis Castillo Dur n
*- Julio de 1994
*--------------------------------------------------------------------
PARAMETER mTipo, Desc, DescS, TipMan, DesIt1, DesIt2, DesIt3
PRIVATE mtipo, Ti_Par, Desc, DescS, TipMan, DesIt1, DesIt2, DesIt3, pantalla

* mTipo     : Filtro de Parmae : TIPO
* Desc      : Descripci¢n en Singular para los mensajes
* DescS     : Descripci¢n en Plural para los mensajes
* TipMan    : Tipo de mantenimiento:
*                 1 = Tambi‚n en blanco: mantenimiento de Codigo y Descri
*                 2 = Mantenimiento de Codigo, Descri y Descriaux
*                 3 = Mantenimiento de Codigo, Descri y Fecha
*                 4 = Mantenimiento de Codigo, Descri y Nument
*                 5 = Mantenimiento de Codigo, Descri y NumDec
*                 6 = Mantenimiento de Codigo, Descri, Nument y Fecha
*                 7 = Mantenimiento de Codigo, CodigoAux,Descri
*                 8 = Mantenimiento de Codigo, Descri y Nument + TIPO CORREL
* DesIt1    : Label para Tercera variable a mantener
* DesIt2    : Label para Cuarta variable a mantener
* DesIt3    : Label para Quinta variable a mantener
*


SAVE SCREEN TO pantalla

TipMan= IIF(PARAMETER()<4,1,TipMan)
mTipo = IIF(PARAMETER()=0,"????",mTipo)
Desc  = IIF(PARAMETER()<2,mTipo,Desc)
DescS = IIF(PARAMETER()<3,mTipo,DescS)
DesIt1= IIF(PARAMETER()<5,"Tercera Variable",DesIt1)
DesIt2= IIF(PARAMETER()<6,"Cuarta Variable",DesIt2)
DesIt3= IIF(PARAMETER()<7,"Quinta Variable",DesIt3)

*Desc  = PADL(Desc, 20,' ')
*DescS = PADL(DescS,20,' ')
*DesIt1= PADL(DesIt1,20,' ')
*DesIt2= PADL(DesIt2,20,' ')
*DesIt3= PADL(DesIt3,20,' ')

USE Parmae   IN  1 ORDER Parmae1 ALIAS parma
IF TIPMAN=7
   USE Parmaux  IN  2  ALIAS parmax EXCLU
   sele parmax
   zap
ENDIF

SELECT parma
IF TIPMAN=8
  SET FILTER TO TIPO + CODIGO = mTipo
ELSE
  SET FILTER TO TIPO = mTipo
ENDIF
GO TOP

*- Variables de trabajo (registro a trabajar)
PUBLIC mCodigo, mDescri, mAux1, mAux2

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
 DO Logos WITH Rotulo1,VTEMPO

 vTitulo = 'Tabla de '+DescS
 vTitul2 = 'Revisi¢n de '+DescS+'    Presione F10 para concluir          '

 DEFINE WINDOW Wind_0 FROM 00,00 TO 23,79 DOUBLE ;
 TITLE vTitulo COLOR SCHEME 10

 DEFINE WINDOW Wind_1 FROM 00,00 TO 24,79 DOUBLE ;
 TITLE vTitul2 COLOR SCHEME 10

 DEFINE MENU mMenu COLOR SCHEME 3
 DEFINE PAD revis   OF mMenu PROMPT '\<Revisa'     AT 24,00
 DEFINE PAD busca   OF mMenu PROMPT '\<Busca'      AT 24,08
 DEFINE PAD anter   OF mMenu PROMPT '\<Anterior'   AT 24,15
 DEFINE PAD proxi   OF mMenu PROMPT '\<Siguiente'  AT 24,25
 DEFINE PAD corri   OF mMenu PROMPT '\<Corrige'    AT 24,36
 DEFINE PAD ingre   OF mMenu PROMPT '\<Ingresa'    AT 24,45
 DEFINE PAD elimi   OF mMenu PROMPT '\Elimina'    AT 24,54
 DEFINE PAD lista   OF mMenu PROMPT '\Lista '     AT 24,63
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
 @  3,20 SAY "Codigo:"
 IF TipMan=7
    @ 4,18 SAY "Programa:"
 ENDIF
 @  6, 7 SAY PADL(Desc+":",20," ")

 DO CASE
 CASE TipMan =8
    @ 9, 7 SAY PADL(DesIt1 + ":",20," ")
 CASE TipMan < 6 .AND. TipMan>1
    @ 9, 7 SAY PADL(DesIt1 + ":",20," ")
 CASE TipMan = 6
    @ 9, 7 SAY PADL(DesIt1 + ":",20," ")
    @12, 7 SAY PADL(DesIt2 + ":",20," ")
 ENDCASE

 RETURN


PROCEDURE Vista              && Coloca valores de BD en variables y pinta datos
*--------------
 ACTIVATE WINDOW Wind_0
 SELECT parma
 mCodigo     = Codigo
 mDescri     = Descri
 mDescri1    = SUBSTR(Descri,01,50)
 mDescri2    = SUBSTR(Descri,51,50)
 mAux1       = Asg_Val(TipMan,1)
 mAux2       = Asg_Val(TipMan,2)

 *@ 20,10 say MEMO() PICTURE "9999"
 @  3,28 SAY mCodigo      PICTURE '@S30'
 @  6,28 SAY mDescri1
 @  7,28 SAY mDescri2

 DO CASE
 CASE TipMan=2
    @ 9,28 SAY mAux1
 CASE TipMan=3
    @ 9,28 SAY mAux1
 CASE TipMan=4 or TipMan=8
    @ 9,28 SAY mAux1  PICTURE "9,999,999,999"
 CASE TipMan=5
    @ 9,28 SAY mAux1  PICTURE "999,999,999.99"
 CASE TipMan = 6
    @ 9,28 SAY mAux1  PICTURE "9,999,999,999"
    @12,28 SAY mAux2
 CASE TipMan = 7
    @ 4,28 SAY mAux1  PICTURE '!!!!!!'
 ENDCASE

 RETURN


PROCEDURE Revis
*--------------
IF TIPMAN = 8
   DO STANDBY WITH 'Opci¢n no disponible'
   return
 endif
 SELECT parma
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 ACTIVATE SCREEN
 ON KEY LABEL F10 KEYBOARD CHR(23)
 BROWSE KEY mTipo WINDOW Wind_1;
    NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
    Codigo    :H='C¢digo',;
    CodigoAUX :H='Subcdgo',;
    Descri    :H='Descripci¢n':50 ,;
    Nument    :H='Correl' :P='99,999'
 ON KEY LABEL F10
 DO Vista
 RETURN


PROCEDURE Busca
*--------------
 SELECT parma
IF TIPMAN = 8
   DO STANDBY WITH 'Opci¢n no disponible'
   return
 endif

 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF

 Vtemp = RECNO()

 DEFINE WINDOW WBusca FROM 10,12 TO 14,68 DOUBLE COLOR SCHEME 6
 ACTIVATE WINDOW WBusca
 STORE SPACE(6) TO Vbusca

 @ 1,3 SAY "Digite c¢digo de "+Desc+" que desea :" GET Vbusca PICT '@!' COLOR SCHEME 7
 READ
 RELEASE WINDOW WBusca

 IF EMPTY(Vbusca) .OR. LASTKEY()=27
    GOTO Vtemp
 ELSE
    SEEK mTipo+Vbusca
    IF !FOUND()
       DO standby WITH "Dicho "+Desc+" no fue encontrado."
       GOTO Vtemp
    ELSE
       DO Vista
    ENDIF
 ENDIF

 RETURN


PROCEDURE Anter
*--------------
 SELECT parma
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 IF !BOF()
    SKIP -1
 ENDIF
 IF BOF()
    GO TOP
    DO standby WITH "Inicio de archivo: no existe &mTipo anterior."
 ELSE
    DO Vista
 ENDIF
RETURN
 

PROCEDURE Proxi
*--------------
 SELECT parma
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF

 IF !EOF()
    SKIP
 ENDIF

 IF EOF()
    DO standby WITH "Fin de archivo: no existe "+Desc+" siguiente."
    GO BOTTOM
 ELSE
    DO Vista
 ENDIF

 RETURN


PROCEDURE Corri
*--------------
 SELECT parma
 IF EOF()
    DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
    RETURN
 ENDIF
 ACTIVATE WINDOW Wind_0
 @  3,28 SAY mCodigo         && Campo que forma llave primaria
 IF TipMan = 7
    @ 4,28 GET mAux1  PICTURE '!!!!!!'
 ENDIF
 if tipman#8
    @ 6,28 GET mDescri   PICTURE '@S50'
 endif
 DO CASE
 CASE TipMan=2
    @ 9,28 GET mAux1
 CASE TipMan=3
    @ 9,28 GET mAux1
 CASE TipMan=4 OR Tipman=8
    @ 9,28 GET mAux1  PICTURE "9,999,999,999"
 CASE TipMan=5
    @ 9,28 GET mAux1  PICTURE "99,999,999.99"
 CASE TipMan = 6
    @ 9,28 GET mAux1  PICTURE "9,999,999,999"
    @12,28 GET mAux2
 ENDCASE

 READ
 IF LASTKEY() <> 27          && Si se presion¢ Esc, no graba
   IF f_lock(1)
      REPLACE Descri     WITH mDescri

      DO CASE
      CASE TipMan=2
         REPLACE DescriAux WITH mAux1
      CASE TipMan=3
         REPLACE Fecha WITH mAux1
      CASE TipMan=4 OR Tipman=8
         REPLACE NumEnt WITH mAux1
      CASE TipMan=5
         REPLACE NumDec WITH mAux1
      CASE TipMan=6
         REPLACE NumEnt WITH mAux1, Fecha WITH mAux2
      CASE TipMan=7
         REPLACE Codigoaux WITH mAux1
      ENDCASE

      UNLOCK

   ENDIF
 ENDIF
 DO Vista                    && Muestra nuevos datos
 RETURN


PROCEDURE Ingre              && Crea nuevo registro en BD
*--------------
IF TIPMAN = 8
   DO STANDBY WITH 'Opci¢n no disponible'
   return
 endif
 *- Aqu¡ coloca valores por omisi¢n (default)
 ACTIVATE WINDOW Wind_0
 STORE SPACE(6)  TO mCodigo
 STORE SPACE(60) TO mDescri

 SELECT parma
 @  3,28 GET mCodigo   PICTURE '@!'
 IF TipMan = 7
    @ 4,28 GET mAux1  PICTURE '!!!!!!'
 ENDIF
 READ
 IF LASTKEY() = 27 .OR. EMPTY(mCodigo)
   DO standby WITH "Proceso cancelado. No se graba ning£n cambio."
   GO BOTTOM
 ELSE
 IF TipMan = 7
   SEEK mTipo+mCodigo+mAux1
 ELSE
   SEEK mTipo+mCodigo
 ENDIF
   IF FOUND()
     DO standby WITH Desc+" ya est  registrado. Proceda a corregir datos."
     DO Vista
     DO Corri
   ELSE

     @  6,28 GET mDescri PICTURE '@S50'
     DO CASE
     CASE TipMan=2
        @ 9,28 GET mAux1
      CASE TipMan=3
         @ 9,28 GET mAux1
      CASE TipMan=4 OR Tipman=8
         @ 9,28 GET mAux1  PICTURE "9,999,999,999"
      CASE TipMan=5
         @ 9,28 GET mAux1  PICTURE "999,999,999.99"
      CASE TipMan = 6
         @ 9,28 GET mAux1  PICTURE "9,999,999,999"
         @12,28 GET mAux2
      ENDCASE
      READ

     IF LASTKEY() <> 27
       IF f_appd()
          REPLACE Tipo   WITH mTipo, Codigo WITH mCodigo;
                  Descri WITH mDescri
          DO CASE
          CASE TipMan=2
             REPLACE DescriAux With mAux1
          CASE TipMan=3
             REPLACE Fecha With mAux1
          CASE TipMan=4  OR Tipman=8
             REPLACE NumEnt With mAux1
          CASE TipMan=5
             REPLACE NumDec With mAux1
          CASE TipMan=6
             REPLACE NumEnt With mAux1, Fecha With mAux2
          CASE TipMan=7
             REPLACE Codigoaux WITH mAux1
          ENDCASE

          UNLOCK

       ENDIF

     ELSE
       GO BOTTOM
     ENDIF

   ENDIF
 ENDIF
 DO Vista
 RETURN


PROCEDURE Elimi
*--------------
IF TIPMAN = 8
   DO STANDBY WITH 'Opci¢n no disponible'
   return
 endif
 SELECT Parma

 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 STORE YESNO("¨Est  seguro que desea ELIMINAR este "+Desc+" ?") TO vElimina

 IF vElimina
    IF f_lock(1)
       DELETE NEXT 1
       IF !BOF()
          SKIP -1
       ENDIF
    ENDIF
 ENDIF

 DO Vista

 RETURN


PROCEDURE Lista
*--------------
IF TIPMAN = 8
   DO STANDBY WITH 'Opci¢n no disponible'
   return
 endif
 SELECT parma

 IF EOF()
   DO standby WITH "Archivo: vac¡o. No hay registros para procesar."
   RETURN
 ENDIF

 vTemp = RECNO()

 DO CASE
 CASE TipMan = 2
    campo3 = "Descriaux"
    DO REPORTE WITH 2,"ManPar2","CATALOGO DE "+UPPER(DescS)
 CASE TipMan = 3
    campo3 = "Fecha"
    DO REPORTE WITH 2,"ManPar3","CATALOGO DE "+UPPER(DescS)
 CASE TipMan = 4 OR Tipman=8
    campo3 = "NumEnt"
    DO REPORTE WITH 2,"ManPar4","CATALOGO DE "+UPPER(DescS)
 CASE TipMan = 5
    campo3 = "NumDec"
    DO REPORTE WITH 2,"ManPar5","CATALOGO DE "+UPPER(DescS)
 CASE TipMan = 6
    campo3 = "NumEnt"
    campo4 = "Fecha"
    DO REPORTE WITH 2,"ManPar6","CATALOGO DE "+UPPER(DescS)
 casE TipMan = 7
    SET FILTER TO TIPO='CODSUB'
    GO TOP
    SCAN
        SCATTER MEMVAR
        SELE PARMAX
        append blank
        GATHER MEMVAR
        SELE PARMA
    ENDSCAN
    SET FILTER TO
    SELE PARMAX
    GO TOP
    do reporte with 2,"ManPar7","CATALOGO DE "+UPPER(DescS)
    sele parma
 OTHERWISE
    DO REPORTE WITH 2,"ManPar","CATALOGO DE "+UPPER(DescS)
  ENDCASE


 GOTO vTemp

 RETURN


PROCEDURE Termi
*--------------
  vEn_accion = .F.
  DEACTIVATE MENU

  RETURN


PROCEDURE Fin_opcion
*-------------------
  SELECT     parma
  SET FILTER TO
  RELEASE    WINDOW wind_0
  RELEASE    WINDOW wind_1
  RELEASE    MENU   mMenu
  RESTORE    SCREEN FROM Pantalla
  CLOSE DATA

  RETURN


FUNCTION Asg_Val
*---------------
PARAMETER Tipman, Otro

   PRIVATE v_fun
   DO CASE
   CASE TipMan = 1
      v_fun = Descri
   CASE TipMan = 2
      v_fun = DescriAux
   CASE TipMan = 3
      v_fun = Fecha
   CASE TipMan = 4 OR Tipman=8
      v_fun = NumEnt
   CASE TipMan = 5
      v_fun = NumDec
   CASE TipMan = 6 .AND. Otro=1
      v_fun = NumEnt
   CASE TipMan = 6 .AND. Otro=2
      v_fun = Fecha
   CASE TipMan = 7
      v_fun = CodigoAux
   ENDCASE

RETURN v_fun

PROCEDURE BUSCPRG
*----------------
PARAMETER VCODIGO
CV=ALLTRIM(VCODIGO)
AX=PRG_&CV
RETURN AX
