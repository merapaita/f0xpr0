
************************************************************************
*
*                        LIBRERIA FOX
*  Autores :   Carola Arrese - Carlos Atoche
*  Objeto  :   Biblioteca de funciones FoxPro para m£ltiples usos
*  Fechas  :   Mayo 1991 - Junio 1992
***********************************************************************

*  Rutina : Fox_Ambi                                                  *
*  Objeto : Crea el ambiente de trabajo en FoxPRO                     *
***********************************************************************

PROCEDURE fox_ambi
*-----------------
* Ambiente de FoxPRO/LAN

   DEACTIVATE WINDOW ALL
   DEACTIVATE POPUP
   DEACTIVATE MENU
   ACTIVATE SCREEN

   CLEAR WINDOW
   RELEASE ALL
   CLEAR ALL

   PUBLIC worker
   worker = IIF(TRIM(UPPER(GETENV('WORKER')))='ON',.T.,.F. )

   ACTIVATE SCREEN
   SET ALTERNATE        OFF  && Default
   SET ALTERNATE TO          && Default
   SET AUTOSAVE         OFF
   SET BELL             ON   && Default
   SET BLOCKSIZE TO     33   && Default
   SET BLINK            ON  && Allows use of bright backgrounds
   SET BORDER TO        SINGLE
   SET CENTURY          ON  && Default
   SET CLEAR            ON
   SET CLOCK            ON   && Default
   SET CLOCK TO         00,68
   SET COMPATIBLE       OFF  && Default
   SET CONFIRM          ON
   SET CONSOLE          OFF   && Default
   SET CURRENCY         LEFT && Default
   SET DATE             BRITISH   && Per£

   IF .NOT. worker
      SET DEBUG          OFF  && Default
   ELSE
      SET DEBUG          ON
   ENDIF

   SET DECIMALS TO      2   && Default

   SET DELETED          ON

   IF worker
      SET DEVELOPMENT    ON
   ELSE
      SET DEVELOPMENT    OFF && Default
   ENDIF

   SET DEVICE TO        SCREEN && Default
   SET DOHISTORY        OFF  && Default
   SET ECHO             OFF  && Default

   IF worker            && TURN OFF FOR LIVE USE, ON FOR PROGRAMMING & TESTING
      SET ESCAPE         ON
   ELSE
      SET ESCAPE         OFF  && Default
   ENDIF

   SET EXACT            OFF
   SET EXCLUSIVE        OFF
   SET FIELDS           OFF
   SET FIXED            ON
   SET FORMAT           TO
   SET FULLPATH         ON
   SET HEADING          OFF
   SET HELP             ON
   SET HOURS TO         12
   SET INSTRUCT         ON
   SET INTENSITY        ON

   IF worker
      SET LOGERROR       ON
   ELSE
      SET LOGERROR       OFF
   ENDIF

   SET MARGIN TO        0
   SET MARK TO          "/"  && THIS IS THE DATE FIELD SEPARATOR
   SET MEMOWIDTH TO     255
   SET MENU             ON
   SET MESSAGE TO       23
   SET NOTIFY           ON
   SET NEAR             OFF
   SET ODOMETER TO      1  && SET UP TO 10 OR 100 IN LARGER APPLICATIONS
   SET PATH TO          && ACCEPT DEFAULT PATHS
   SET POINT TO         "."
   SET PRECISION TO     3
   SET PRINT            OFF
   SET PRINTER TO       LPT1
   SET REFRESH TO 5
   SET SAFETY           OFF
   SET SCOREBOARD       OFF
   SET SEPARATOR TO     ","
   SET SPACE            OFF
   SET STATUS           OFF
   SET SYSMENU          OFF

   IF .NOT. worker
      SET STEP           OFF
   ENDIF

   SET STICKY           ON
   SET TALK             OFF
   SET TOPIC     TO
   SET TYPEAHEAD TO     0    && CLEAR AWAY ANY OVERANXIOUS INPUT!
   SET TYPEAHEAD TO     25
   SET UDFPARMS  TO     REFERENCE
   SET UNIQUE           OFF


   * SET THE INSERT / CAPS / NUMLOCK KEYS
   * NOTE: LOOKUP POPUPS WORK BETTER WITH INSMODE OFF, AS BELOW
	DEFINE WINDOW _funbus    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW
   

   = INSMODE(.T.)
   = CAPSLOCK(.T.)
   = NUMLOCK(.T.)

   * SYSTEM VARIABLE SETTINGS
   _alignment =  "LEFT"
   _box =        .t.
   _indent =     0
   _lmargin =    0
   _padvance =   "FORMFEED"
   _pageno =     1
   _pbpage =     1
   * _PCOLNO     SYSTEM MAINTAINED VARIABLE
   _pcopies =    1
   * _PDRIVER =    GENERIC.PR2 &&EE
   _pecode =     ""
   _peject =     "AFTER"
   _pepage =     32767
   _pform =      ""
   _plength =    60          && A4 = 66
   * _PLINENO =  SYSTEM MAINTAINED VARIABLE
   _ploffset =   0
   _ppitch =     "PICA"
   _pquality =   .t.
   _pscode =     ""
   _pspacing =   1
   _pwait =      .f.
   _rmargin =    85
   _tabs =       "10, 20, 30, 40, 50, 60, 70"
   _wrap =       .t.

   ON KEY LABEL RIGHTMOUSE KEYBOARD CHR(13)
   ON KEY LABEL F10        KEYBOARD CHR(23)

   DEFINE WINDOW Err_input  FROM 01,60 TO 03,77
	
   DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5
   
   * Ventana de Proceso
   DEFINE WINDOW Espera     FROM 12,06 TO 14,78 COLOR SCHEME 05 
   

   * Typical system messages:
   DEFINE WINDOW msg2user   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW GROW COLOR SCHEME 5

   * Press any key or click mouse to continue messages:
   DEFINE WINDOW standby    FROM 19,23 TO 23,77 DOUBLE FLOAT SHADOW GROW COLOR SCHEME 5

   IF worker
      ON ERROR
   ELSE
      ON ERROR    &&& DO fox_errs WITH PROGRAM()
   ENDIF

RETURN

*************************************************************************
*  PROCEDURE fox_lock.prg    FUE REEMPLAZADO POR F_LOCK()
*
*  Parametros: Tipo de bloqueo, Valor logico de bloqueo
*  Tipos
*        1 : Intenta bloquear registro hasta que lo logra o se cancele
*        2 : Intenta bloquear registro solo una vez
*        3 : Intenta bloquear archivo hasta que lo logra o se cancele
*        4 : Intenta bloquear archivo solo una vez
*
*************************************************************************

PROCEDURE fox_lock
*-----------------
PARAMETERS BTipo,Bloquea
   Beep = CHR(7)

   DO CASE

   CASE BTipo = 1    && Bloquea registro hasta conseguirlo o escape
      Bloquea   = .F.
      IF RLOCK()
         Bloquea   = .T.
      ELSE
         DO STANDBY WITH "Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar."
         ?? Beep
         KTecla    = 0

         DO WHILE KTecla <> Escape .AND. (.NOT. RLOCK())
            KTecla    = INKEY()
         ENDDO

         IF KTecla <> Escape
            Bloquea = .T.
         ENDIF

      ENDIF

   CASE BTipo = 2     && Intenta bloquear registro solo una vez

      Bloquea   = .F.
      IF RLOCK()
         Bloquea   = .T.
      ELSE
         DO STANDBY WITH "El registro est  siendo utilizado. Se cancela la operaci¢n. "
         ?? Beep
      ENDIF

   CASE BTipo = 3        && Bloquea archivo hasta conseguirlo a cancela
      Bloquea   = .F.

      IF FLOCK()
         Bloquea   = .T.
         Insiste   = .F.
      ELSE
         DO STANDBY WITH "Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar."
         ?? Beep
         KTecla    = 0

         DO WHILE KTecla <> Escape .AND. (.NOT. FLOCK())
            KTecla    = INKEY()
         ENDDO

         IF KTecla <> Escape
            Bloquea = .T.
         ENDIF

      ENDIF

   CASE BTipo = 4   && Intenta bloquear archivo solo una vez

      Bloquea   = .F.
      IF FLOCK()
         Bloquea   = .T.
      ELSE
         DO STANDBY WITH "Archivo ocupado.  El proceso se cancela."
         ?? Beep
      ENDIF

   ENDCASE

RETURN


FUNCTION f_lock
*--------------
PARAMETERS BTipo

   Beep = CHR(7)

   DO CASE

   CASE BTipo = 1    && Bloquea registro hasta conseguirlo o escape
      v_fun   = .F.

      IF RLOCK()
         v_fun   = .T.
      ELSE
         DO STANDBY WITH "Registro ocupado. Espere un momento por favor o presione <Esc> para cancelar."
         ?? Beep
         KTecla    = 0
         
         DO WHILE KTecla <> Escape .AND. (.NOT. RLOCK())
            KTecla    = INKEY()
         ENDDO
         
         IF KTecla <> Escape
            v_fun = .T.
         ENDIF
         
      ENDIF

   CASE BTipo = 2     && Intenta bloquear registro solo una vez

      v_fun   = .F.
      IF RLOCK()
         v_fun   = .T.
      ELSE
         DO STANDBY WITH "El registro est  siendo utilizado. Se cancela la operaci¢n. "
         ?? Beep
      ENDIF

   CASE BTipo = 3        && Bloquea archivo hasta conseguirlo o cancela
      v_fun   = .F.
      
      IF FLOCK()
         v_fun   = .T.
      ELSE
         DO STANDBY WITH "Archivo ocupado.  Espere un instante por favor o presiona <Esc> para cancelar."
         ?? Beep
         KTecla    = 0
         
         DO WHILE KTecla <> Escape .AND. (.NOT. FLOCK())
            KTecla    = INKEY()
         ENDDO
         
         IF KTecla <> Escape
            v_fun = .T.
         ENDIF
         
      ENDIF

   CASE BTipo = 4   && Intenta bloquear archivo solo una vez

      v_fun   = .F.
      IF FLOCK()
         v_fun   = .T.
      ELSE
         DO STANDBY WITH "Archivo ocupado.  El proceso se cancela."
         ?? Beep
      ENDIF

   ENDCASE

RETURN v_fun

*************************************************************************
* PROCEDURE fox_appd    FUE REEMPLAZADO POR F_APPD()
* Objeto  :   Biblioteca de funciones para trabajo multi-usuario
*             Previamente, es necesario estar en el archivo a agregar
*             y que este estuviese abierto en modo multiusario.
*             Parametros: LAppd. Variable l"gica
* LAppd
*       .T.: Agrego y bloqueo nuevo registro
*       .F.: No pudo Agregar.
*
*  Fecha : 01/12/89
*
*************************************************************************

PROCEDURE fox_appd
*-----------------
PARAMETERS LAppd
   LAppd     = .F.
   Bloquea   = .F.
   GAgrega   = .T.

   APPEND BLANK

   DO FOX_Lock WITH 1,Bloquea

   IF Bloquea
      LAppd   = .T.
   ENDIF

   GAgrega = .F.

RETURN


FUNCTION f_appd
*--------------
   APPEND BLANK
   v_fun = F_Lock(1)

RETURN v_fun


******************************************************************
* PROCEDURE fox_errs
* Rutina que controla el error al agregar un nuevo registro
* Para los demas casos genera un archivo core.fox que contiene
* el estado de la memoria al momento de ocurrir el error
*
* Parametros : Enviar a esta rutina la funcion SYS(16)
* Fecha : 04/12/89
****************************************************************** ;

PROCEDURE fox_errs
*-----------------
PARAMETERS Prg_error

   DO CASE
   CASE ERROR() = 108 .AND. Gagrega
      DO standby WITH "Un momento por favor para agregar registro"
      RETRY
   OTHERWISE
      Nom_usr   = SYS(30)
      Num_error = ERROR()
      Des_error = MESSAGE()
      Lin_error = MESSAGE(1)
      Dbf_actua = DBF()
      IF SYS(21) <> '0'
         Idx_actua = NDX(VAL(SYS(21)))
      ENDIF
      SAVE SCREEN TO Pnt_error
      CREATE VIEW core.viw
*      SAVE TO "core.fox"

      DO standby WITH Des_error+" ERROR no recuperable presione ENTER para terminar"

      WAIT ""
      IF worker
         ON ERROR
         CANCEL
      ENDIF
      QUIT
   ENDCASE
RETURN


PROCEDURE standby
*----------------
PARAMETERS msgwords,posi,posc

   PRIVATE _ln, _colW, msg1, msg2

   IF !WEXIST('standby')
      DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5
   ENDIF

   IF PARAMETERS()>1
      posi = IIF(posi>19,19,posi)
      colp = 27
      IF PARAMETERS()>2
         colp = IIF(posc>28,29,posc)
      ENDIF
      MOVE WINDOW standby    TO posi,colp
   ENDIF

   ACTIVATE WINDOW standby

   msgwords = ALLTRIM(msgwords)
   _ln      = LEN(msgwords)
   _colW    = WCOLS()
   msgwords = iif(_ln>2*_colW,PADR(msgwords,2*_colW,' '),msgwords)
   _ln      = LEN(msgwords)

   msg1     = IIF(_ln>_colW,LEFT(msgwords,_colW),msgwords)
   msg2     = IIF(_ln>_colW,SUBSTR(msgwords,_colW+1),"")

   IF _ln>_colW
      FOR I=0 TO _colW/3-1
         IF SUBSTR(msg1,_colW-i,1) == " "
            msg1 = LEFT(msg1,_colW-i)
            msg2 = SUBSTR(msgwords,_colW-i+1)
            msg2 = IIF(LEN(msg2)>_colW,LEFT(msg2,_colW),msg2)
            EXIT
         ENDIF
      ENDFOR
   ENDIF


   @ 00,wcols()/2-LEN(msg1)/2  SAY msg1
   IF !EMPTY(msg2)
      @ 01,wcols()/2-LEN(msg2)/2  SAY msg2
   ENDIF

   @ 02,_colW/2-16 SAY '<Pres. una tecla para continuar>'
   _ss = INKEY(0)

   DEACTIVATE WINDOW standby

   MOVE WINDOW standby TO 19,27

RETURN             && standby

FUNCTION yesno
*-------------
  PARAMETERS msgwords,posi,posc

  IF !WEXIST('yesno')
     DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5
  ENDIF

  IF PARAMETERS()>1
     posi = IIF(posi > 19, 19, posi)
     colp = 27
     IF PARAMETERS()>2
        colp = IIF(posc>28,29,posc)
     ENDIF
     MOVE WINDOW yesno TO posi,colp
  ENDIF

  ACTIVATE WINDOW yesno

  msgwords = ALLTRIM(msgwords)
  _ln      = LEN(msgwords)
  _colW    = WCOLS()
  msgwords = IIF(_ln>2*_colW,PADR(msgwords,2*_colW,' '),msgwords)
  _ln      = LEN(msgwords)

  msg1     = IIF(_ln>_colW,LEFT(msgwords,_colW),msgwords)
  msg2     = IIF(_ln>_colW,SUBSTR(msgwords,_colW+1),"")

  IF _ln>_colW
     FOR I=0 TO _colW/3-1
        IF SUBSTR(msg1,_colW-i,1) == " "
           msg1 = LEFT(msg1,_colW-i)
           msg2 = SUBSTR(msgwords,_colW-i+1)
           msg2 = IIF(LEN(msg2)>_colW,LEFT(msg2,_colW),msg2)
           EXIT
        ENDIF
     ENDFOR
  ENDIF

  @ 00,wcols()/2-LEN(msg1)/2  SAY msg1
  IF !EMPTY(msg2)
     @ 01,wcols()/2-LEN(msg2)/2  SAY msg2
  ENDIF
  v_Fun = .F.
  @ 02,10 GET _NoSi FUNCTION '*TH \!\<No;\?\<Si' DEFAULT 1;
	SIZE 1,10,8
  READ CYCLE
  IF LASTKEY() # 27
    v_Fun = IIF(_NoSi = 1,.F.,.T.)
  ENDIF
DEACTIVATE WINDOW yesnO

RETURN v_Fun

FUNCTION XXyesno
*-------------
PARAMETERS msgwords,posi,posc
PRIVATE YesNo, V_Fun, _ln, _colW, msg1, msg2

   STORE 'No' TO yesno
   IF !WEXIST('yesno')
      DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5
   ENDIF

   IF PARAMETERS()>1
      posi = IIF(posi>19,19,posi)
      colp = 27
      IF PARAMETERS()>2
         colp = IIF(posc>28,29,posc)
      ENDIF
      MOVE WINDOW yesno TO posi,colp
   ENDIF

   ACTIVATE WINDOW yesno

   msgwords = ALLTRIM(msgwords)
   _ln      = LEN(msgwords)
   _colW    = WCOLS()
   msgwords = IIF(_ln>2*_colW,PADR(msgwords,2*_colW,' '),msgwords)
   _ln      = LEN(msgwords)

   msg1     = IIF(_ln>_colW,LEFT(msgwords,_colW),msgwords)
   msg2     = IIF(_ln>_colW,SUBSTR(msgwords,_colW+1),"")

   IF _ln>_colW
      FOR I=0 TO _colW/3-1
         IF SUBSTR(msg1,_colW-i,1) == " "
            msg1 = LEFT(msg1,_colW-i)
            msg2 = SUBSTR(msgwords,_colW-i+1)
            msg2 = IIF(LEN(msg2)>_colW,LEFT(msg2,_colW),msg2)
            EXIT
         ENDIF
      ENDFOR
   ENDIF

   @ 00,wcols()/2-LEN(msg1)/2  SAY msg1
   IF !EMPTY(msg2)
      @ 01,wcols()/2-LEN(msg2)/2  SAY msg2
   ENDIF

   @ 02,05 SAY 'Si/No:' GET yesno PICTURE '@M Si,No '
   READ

   DEACTIVATE WINDOW yesno
   IF LASTKEY()=27    && ESCAPE
      v_fun = .F.
   ELSE
      STORE IIF(yesno='Si',.t.,.f.) TO v_fun
   ENDIF

   MOVE WINDOW yesno TO 19,27

RETURN v_fun


FUNCTION ready2pr
*----------------
   DO WHILE .NOT. PRINTSTATUS()
     IF !yesno('La Impresora no esta lista! Se reintentar  la impresi¢n...')
       RETURN .f.
     ENDIF
   ENDDO
RETURN .t.


****************************************************************************
* Programa   : popdate.prg                                                 *
* Autor      : Andrew Coupe, Carlos Atoche                                 *
* Uso        : <varDate> = POPDATE(<fila>,<columna>,[<valor en defecto>])  *
* Descripci¢n: UDF que permite elegir la fecha usando la facilidad del     *
*              calendario.                                                 *
****************************************************************************
FUNCTION POPDATE
*---------------
PARAMETER row,col,default

   thismsg = SET("MESSAGE",1)
   thisdate= _diarydate

   **-- Si enviaron el tercer par metro, asigna a DEFAULT el tercer par m.

   DEFAULT = IIF( PARAMETERS()=3, default, _diarydate)

   _diarydate = default

   DEFINE WINDOW CAL FROM row,col TO row+16,col+22;
   DOUBLE TITLE "Calendario" FLOAT GROW COLOR SCHEME 10

   **-- Necesita el SET STATUS ON para visualizar el mensaje:

   SET MESSAGE TO ;
   "Cambie la fecha con las flechas. [H]oy d¡a, Mes:[PgUp,PgDn], A¤o:[^PgUp,^PgDn]"

   ACTIVATE WINDOW cal
   ACTIVATE WINDOW calendar IN cal
   MOVE WINDOW calendar to -1,-1

   DO WHILE LASTKEY() # 27
      i = INKEY(0,"H")
      DO CASE
       CASE i = 13 .OR. i = 27
          EXIT
       CASE i = 72 .OR. i = 104       && 'H' : Hoy d¡a
          _diarydate = DATE()
       CASE i = 24                    && Flecha para abajo
          _diarydate = _diarydate + 7
       CASE i = 05                    && Flecha para arriba
          _diarydate = _diarydate - 7
       CASE i = 19                    && Flecha para izquierda
          _diarydate = _diarydate - 1
       CASE i = 04                    && Flecha para derecha
          _diarydate = _diarydate + 1
       CASE i = 03                    && P gina abajo
          _diarydate = gomonth(_diarydate,1)
       CASE i = 18                    && P gina arriba
          _diarydate = gomonth(_diarydate,-1)
       CASE i = 30                    && Ctrl. P gina arriba
          _diarydate = gomonth(_diarydate,12)
       CASE i = 31                    && Ctrl. P gina abajo
          _diarydate = gomonth(_diarydate,-12)
      ENDCASE
   ENDDO

   SET MESSAGE TO (thismsg)
   RELEASE WINDOWS cal

   newdate =;
   IIF(LASTKEY()=27, default, _diarydate)

   _diarydate = thisdate

RETURN newdate

PROCEDURE XREPORTE
*----------------
** Programa   : REPORTE.PRG
** Descripcion: Impresion de reportes utilizando archivos de reporte.
** Par metros :
**               _Tipo : tipo de reporte (1=con ambiente, 2=sin ambiente)
**               _Form : archivo de reporte
**               _Tit  : t¡tulo de la ventana
**               Num_C : N£mero de Copias.
**                 _wp : Si se envia a Word Perfect
**               Ran_pg: Si se pregunta un rango de paginas.
** -----------------------------------------------------------------------

PARAMETER _tipo, _Form, _Tit, Num_C, _wp, Ran_Pg

 **-- Environment
 PRIVATE _WndOld

 _WndOld = WOUTPUT()

 SET ESCAPE ON
 ON ESCAPE STORE .F. TO PRINTING
 _CONSO = SET("CONSOLE")
 SET CONSOLE ON

 *- Si no va a WP
 _wp = .F.
 IF PARAMETERS()<5
   _wp = .f.
 ENDIF

 *- Numero de copias
 IF PARAMETER()<4
   Num_C = 1
 ENDIF
 IF Num_C # 1
   DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " # DE COPIAS "
   ACTIVATE WINDOW _xyx
   CLEAR
   @ 1,5 SAY "COPIAS : " GET Num_C VALID(Num_C>0) PICTURE "99"
   READ
   RELEASE WINDOW _xyx

   IF LASTKEY()=27
     IF !EMPTY(_WndOld)
       ACTIVATE WINDOW &_WndOld
     ELSE
       ACTIVATE SCREEN
     ENDIF
     SET CONSOLE &_conso
     RETURN
   ENDIF
   _NCOPIES = Num_C
 ENDIF

 *- Rango de Paginas
 IF PARAMETER() >= 6 .AND. Ran_Pg
   DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " RANGO DE PAGINAS "
   ACTIVATE WINDOW _xyx
   CLEAR
   vPBPAGE = 1
   vPEPAGE = 32767
   @ 0,5 SAY "INICIO : " GET vPBPAGE PICTURE "99,999" VALID vPBPAGE <= vPEPAGE
   @ 1,5 SAY "   FIN : " GET vPEPAGE PICTURE "99,999" VALID vPBPAGE <= vPEPAGE;
     .AND. vPEPAGE <= 32767
   READ
   RELEASE WINDOW _xyx

   IF LASTKEY()=27
     IF !EMPTY(_WndOld)
       ACTIVATE WINDOW &_WndOld
     ELSE
       ACTIVATE SCREEN
     ENDIF
     SET CONSOLE &_conso
     RETURN
   ENDIF
 ENDIF


 **-- PREGUNTA SI SE IMPRIME O VISUALIZA

 _DEST = 'Pantalla '
 IF !_wp
   DEFINE WINDOW _VeDest FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW GROW COLOR SCHEME 10;
     TITLE ' DESTINO DE IMPRESION '
   ACTIVATE WINDOW _VeDest

   P_FIL = SPACE(8)
   _Dest1 = 1
   @ 00,2  GET _DEST1 FUNCTION "^ \<Pantalla ;\<Impresora;\<Archivo  " SIZE 2,10 COLOR SCHEME 6
   @ 01,20 SAY 'ARCHIVO:' GET P_FIL PICTURE "NNNNNNNN" VALID !EMPTY(P_FIL) WHEN _DEST1 = 3
     READ
   P_FIL = ALLTRIM(P_FIL) + ".LST"
   DO CASE
     CASE _Dest1 = 1
       _DEST = "Pantalla "
     CASE _Dest1 = 2
       _DEST = "Impresora"
     CASE _Dest1 = 3
       _DEST = "Archivo  "
   ENDCASE
   RELEASE WINDOW _VeDest
 ENDIF

 IF LASTKEY() = 27
   IF !EMPTY(_WndOld)
     ACTIVATE WINDOW &_WndOld
   ELSE
     ACTIVATE SCREEN
   ENDIF
   SET CONSOLE &_conso
   RETURN
 ENDIF
 IMPRE = (_DEST='Impresora')

 IF !IMPRE .AND. _DEST = "Pantalla "
   P_FIL = SYS(3)+".LST"
 ENDIF


 PRINTING = .T.
 IF IMPRE
   IF !EMPTY(LEFT(SYS(0),15))
     IF !YESNO("¨Imprime en impresora local?")
       **-- Impresora de red.
       SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
       SET PRINTER TO \\SPOOLER\NB
     ENDIF
   ENDIF
   IF !READY2PR()
     PRINTING = .F.
   ENDIF
 ENDIF


 IF PRINTING
   **-- VENTANA DE GENERACION DE REPORTE
   DEFINE WINDOW _REPO FROM 4,0 TO 22,79 COLOR SCHEME 10 TITLE _Tit
   ACTIVATE WINDOW _REPO
   CLEAR

   **-- SI SE IMPRIME O VISUALIZA
   IF IMPRE
     p_fil = SYS(3) + ".LST"
     SET PRINTER TO &p_fil

     IF _TIPO = 1
       IF PARAMETER() >= 6 .AND. Ran_Pg
         PRINTJOB
           _PBPAGE = vPBPAGE
           _PEPAGE = vPEPAGE
           REPORT FORM &_Form ENVIRONMENT NOEJECT TO PRINT
         ENDPRINTJOB
       ELSE
         REPORT FORM &_Form ENVIRONMENT NOEJECT TO PRINT
       ENDIF

     ELSE
       IF PARAMETER() >= 6 .AND. Ran_Pg
         PRINTJOB
           _PBPAGE = vPBPAGE
           _PEPAGE = vPEPAGE
           REPORT FORM &_Form NOEJECT TO PRINT
         ENDPRINTJOB
       ELSE
         REPORT FORM &_Form NOEJECT TO PRINT
       ENDIF
     ENDIF
     SET PRINTER TO           && Si era la impresora server, libera el spool

     IF !_wp
       FOR i=1 TO Num_C
         IF READY2PR() .AND. IIF(Num_C>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
           !TYPE &P_FIL >PRN
         ENDIF
         IF LASTKEY()=27
           EXIT
         ENDIF
       ENDFOR
     ELSE
       !FOXSWAP WP &p_fil
     ENDIF

     ERASE &p_fil

   ELSE

     IF _TIPO = 1
       IF PARAMETER() >= 6 .AND. Ran_Pg
         PRINTJOB
           _PBPAGE = vPBPAGE
           _PEPAGE = vPEPAGE
           REPORT FORM &_Form ENVIRONMENT TO FILE (P_FIL) NOEJECT
         ENDPRINTJOB
       ELSE
         REPORT FORM &_Form ENVIRONMENT TO FILE (P_FIL) NOEJECT
       ENDIF
     ELSE
       IF PARAMETER() >= 6 .AND. Ran_Pg
         PRINTJOB
           _PBPAGE = vPBPAGE
           _PEPAGE = vPEPAGE
           REPORT FORM &_Form TO FILE (P_FIL) NOEJECT
         ENDPRINTJOB
       ELSE
         REPORT FORM &_Form TO FILE (P_FIL) NOEJECT
       ENDIF
     ENDIF

     IF !_wp
       IF _Dest = "Pantalla "
         MODIFY COMMAND (P_FIL) NOEDIT WINDOW _REPO

         IF YESNO("¨Imprime el reporte ? ")
           FOR i=1 TO Num_C
             IF READY2PR() .AND. IIF(Num_C>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
               !TYPE &P_FIL >PRN
             ENDIF
             IF LASTKEY()=27
               EXIT
             ENDIF
           ENDFOR
         ENDIF
       ENDIF
     ELSE
       !FOXSWAP WP &p_fil
     ENDIF

     IF _DEST # "Archivo  "
       ERASE (P_FIL)
     ENDIF
   ENDIF

   SS = INKEY(10)
   DEACTIVATE WINDOW _REPO
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF

 ON ESCAPE
 SET ESCAPE OFF

 IF _TIPO = 1
   CLOSE DATA
 ENDIF

 IF !EMPTY(_WndOld)
   ACTIVATE WINDOW &_WndOld
 ELSE
   ACTIVATE SCREEN
 ENDIF

 SET CONSOLE &_CONSO

RETURN

PROCEDURE reporte
*----------------
** Programa   : REPORTE.PRG
** Descripcion: Impresion de reportes utilizando archivos de reporte.
** Par metros :
**               _Tipo : tipo de reporte (1=con ambiente, 2=sin ambiente)
**               _Form : archivo de reporte
**               _Tit  : t¡tulo de la ventana
**               Num_C : N£mero de Copias.
**                 _wp : Si se envia a Word Perfect
**               Ran_pg: Si se pregunta un rango de paginas.
** -----------------------------------------------------------------------

PARAMETER _tipo, _form, _tit, num_c, _wp, ran_pg

**-- Environment
PRIVATE _wndold

_wndold = WOUTPUT()

SET ESCAPE ON
ON ESCAPE STORE .F. TO printing
_conso = SET("CONSOLE")
SET CONSOLE ON

**-- Verifica Existencia de windows

DEFINE WINDOW msg2use   FROM 07,02 TO 11,77 DOUBLE FLOAT SHADOW

*- Si no va a WP
_wp = .F.
IF PARAMETERS()<5
	_wp = .F.
ENDIF

*- Numero de copias
IF PARAMETER()<4
	num_c = 1
ENDIF
IF num_c # 1
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " # DE COPIAS "
	ACTIVATE WINDOW _xyx
	CLEAR
	@ 1,5 SAY "Copias : " GET num_c VALID(num_c>0) PICTURE "99"
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
	_ncopies = num_c
ENDIF

*- Rango de Paginas
IF PARAMETER() >= 6 .AND. ran_pg
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " Rango de P ginas "
	ACTIVATE WINDOW _xyx
	CLEAR
	vpbpage = 1
	vpepage = 32767
	@ 0,5 SAY "Inicio : " GET vpbpage PICTURE "99,999" VALID vpbpage <= vpepage
	@ 1,5 SAY "   Fin : " GET vpepage PICTURE "99,999" VALID vpbpage <= vpepage;
		.AND. vpepage <= 32767
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
ENDIF


**-- PREGUNTA SI SE IMPRIME O VISUALIZA

_dest = 'Pantalla '
IF !_wp


	ACTIVATE WINDOW msg2use
	_dest = 'Impresora'
	TITLE = ' DESTINO DE IMPRESION '
	p_fil = SPACE(8)
	_dest1 = 1
	@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _dest PICTURE "@M Impresora,Pantalla,Archivo"
	READ
	IF _dest = 'Archivo  '
		CLEAR
		@ 01,20 SAY 'Nombre del Archivo :' GET p_fil PICTURE "NNNNNNNN" VALID !EMPTY(p_fil)
		READ
	ENDIF
	p_fil = ALLTRIM(p_fil) + ".LST"
	RELEASE WINDOW msg2use
ENDIF

IF LASTKEY() = 27
	IF !EMPTY(_wndold)
		ACTIVATE WINDOW &_wndold
	ELSE
		ACTIVATE SCREEN
	ENDIF
	SET CONSOLE &_conso
	RETURN
ENDIF
impre = (_dest='Impresora')

IF !impre .AND. _dest = "Pantalla "
	p_fil = SYS(3)+".LST"
ENDIF


printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),15))
		IF !yesno("¨Imprime en impresora local?")
			**-- Impresora de red.
			SET PRINTER TO \\ibm_pc\printq_0=lpt1
			SET PRINTER TO \\spooler\nb
		ENDIF
	ENDIF
	IF !ready2pr()
		printing = .F.
	ENDIF
ENDIF


IF printing
	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		p_fil = SYS(3) + ".LST"
		SET PRINTER TO &p_fil

		IF _tipo = 1
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form ENVIRONMENT NOEJECT TO PRINT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form ENVIRONMENT NOEJECT TO pri
			ENDIF
		ELSE
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form NOEJECT TO PRINT
				ENDPRINTJOB
			ELSE

				REPORT FORM &_form NOEJECT TO PRINT
			ENDIF
		ENDIF
		SET PRINTER TO           && Si era la impresora server, libera el spool

		IF !_wp
			FOR i=1 TO num_c
				IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
					!TYPE &p_fil >prn
				ENDIF
				IF LASTKEY()=27
					EXIT
				ENDIF
			ENDFOR
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF

		ERASE &p_fil

	ELSE

		IF _tipo = 1
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
			ENDIF
		ELSE
			IF PARAMETER() >= 6 .AND. ran_pg
				PRINTJOB
				_PBPAGE = vpbpage
				_PEPAGE = vpepage
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
				ENDPRINTJOB
			ELSE
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
			ENDIF
		ENDIF

		IF !_wp
			IF _dest = "Pantalla "
				MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo

				IF yesno("¨Imprime el reporte ? ")
					FOR i=1 TO num_c
						IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
							!TYPE &p_fil >prn
						ENDIF
						IF LASTKEY()=27
							EXIT
						ENDIF
					ENDFOR
				ENDIF
			ENDIF
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF

		IF _dest # "Archivo  "
			ERASE (p_fil)
		ENDIF
	ENDIF

	*   SS = INKEY(10)
	DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'EL REPORTE HA SIDO CANCELADO.'
ENDIF
RELEASE WINDOW msg2use
ON ESCAPE
SET ESCAPE OFF

IF _tipo = 1
	CLOSE DATA
ENDIF

IF !EMPTY(_wndold)
	ACTIVATE WINDOW &_wndold
ELSE
	ACTIVATE SCREEN
ENDIF

SET CONSOLE &_conso

RETURN

PROCEDURE Xrepprg
*---------------
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**               _PrgRpt : Programa reporteador.
PARAMETER  _prgrpt, _tit, _copia, _wp
PRIVATE  resul
resul = 'OK'
IF PARAMETERS()=2
	_copia = 1
ENDIF

IF PARAMETERS()=3
	_wp = .F.
ENDIF

**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USER")
	DEFINE WINDOW msg2user   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW COLOR SCHEME 7
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
ACTIVATE WINDOW msg2user
_dest = 'IMPRESORA'
@ 01,5 SAY 'DESTINO DE IMPRESION : PANTALLA/IMPRESORA' GET _dest PICTURE "@M IMPRESORA,PANTALLA"
READ
DEACTIVATE WINDOW msg2user

IF MOD(READKEY(),256)=12
	ON KEY
	SET ESCAPE OFF
	ACTIVATE SCREEN
	RETURN
ENDIF
impre = (_dest='IMPRESORA')

IF !impre
	p_fil = SYS(3)+".LST"
ENDIF

printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),18))
		IF !yesno("IMPRIME EN IMPRESORA LOCAL ?  <NO = IMPRESORA DE RED>")
			**-- Impresora de red.
			SET PRINTER TO \\ibm_pc\printq_0=lpt1
			SET PRINTER TO \\spooler\nb
		ENDIF
		IF !ready2pr()
			printing = .F.
		ENDIF
	ENDIF
ENDIF

IF printing

	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit ;
		FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		DO &_prgrpt WITH 1
	ELSE
		@ 02,20  SAY  '** Reporte en ejecuci¢n **'

		DO &_prgrpt WITH 2

		IF _wp
			ACTIVATE SCREEN
			RESTORE SCREEN FROM pantalla
			RUN FOXSWAP wp &p_fil
		ELSE
			MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
		ENDIF
		IF resul = 'OK'
			IF yesno("¨ Imprime el Reporte ?")
				FOR v = 1 TO _copia
					IF ready2pr()
						!TYPE &p_fil>prn
					ENDIF
				ENDFOR
			ENDIF
		ELSE
			resul = 'OK'
		ENDIF
		ERASE (p_fil)
	ENDIF
	DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN

RETURN


PROCEDURE XXXREPPRG    &&C.A.K.
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**               _PrgRpt : Programa reporteador.
**               _Tit    : T¡tulo de la ventana
**               Num_C   : N£mero de Copias
**               _WP     : .T. si envia a Word Perfect.
** -------------------------------------------------------------------
PARAMETER _PrgRpt, _Tit, Num_C, _WP

 **-- Environment
 PRIVATE _WndOld

 _WndOld = WOUTPUT()
 SET ESCAPE ON
 ON ESCAPE STORE .F. TO PRINTING

 *- Si no va a WP
 _wp = .F.
 IF PARAMETERS()<4
   _wp = .f.
 ENDIF

 *- Numero de copias
 IF PARAMETER()<3
   Num_C = 1
 ENDIF
 IF Num_C # 1
   DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " # DE COPIAS "
   ACTIVATE WINDOW _xyx
   CLEAR
   @ 1,5 SAY "COPIAS : " GET Num_C VALID(Num_C>0) PICTURE "99"
   READ
   RELEASE WINDOW _xyx

   IF LASTKEY()=27
     ON KEY
     SET ESCAPE OFF
     IF !EMPTY(_WndOld)
       ACTIVATE WINDOW &_WndOld
     ELSE
       ACTIVATE SCREEN
     ENDIF
     RETURN
   ENDIF
 ENDIF


 **-- PREGUNTA SI SE IMPRIME O VISUALIZA
 _DEST = 'Pantalla '
 IF !_wp
   DEFINE WINDOW _VeDest FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW GROW COLOR SCHEME 10;
     TITLE ' DESTINO DE IMPRESION '
   ACTIVATE WINDOW _VeDest

   P_FIL = SPACE(8)
   _Dest1 = 1
   @ 00,2  GET _DEST1 FUNCTION "^ \<Pantalla ;\<Impresora;\<Archivo  " SIZE 2,10 COLOR SCHEME 6
   @ 01,20 SAY 'ARCHIVO:' GET P_FIL PICTURE "NNNNNNNN" VALID !EMPTY(P_FIL) WHEN _DEST1 = 3
   READ
   P_FIL = P_FIL + ".LST"
   DO CASE
     CASE _Dest1 = 1
       _DEST = "Pantalla "
     CASE _Dest1 = 2
       _DEST = "Impresora"
     CASE _Dest1 = 3
       _DEST = "Archivo  "
   ENDCASE
   RELEASE WINDOW _VeDest
 ENDIF

 IF MOD(READKEY(),256)=12
   ON KEY
   SET ESCAPE OFF
   IF !EMPTY(_WndOld)
     ACTIVATE WINDOW &_WndOld
   ELSE
     ACTIVATE SCREEN
   ENDIF
   RETURN
 ENDIF
 IMPRE = (_DEST='Impresora')

 IF !IMPRE .AND. _DEST = "Pantalla "
   P_FIL = SYS(3)+".LST"
 ENDIF

 PRINTING = .T.

 IF IMPRE
   IF !EMPTY(LEFT(SYS(0),15))
     IF !YESNO("¨Imprime en impresora local?  <NO = IMPRESORA DE RED>")
       **-- Impresora de red.
       SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
       SET PRINTER TO \\SPOOLER\NB
     ENDIF
   ENDIF

   IF !READY2PR()
     PRINTING = .F.
   ENDIF

 ENDIF

 IF PRINTING

   **-- VENTANA DE GENERACION DE REPORTE
   DEFINE WINDOW _REPO FROM 4,0 TO 22,79 COLOR SCHEME 10 TITLE _Tit
   ACTIVATE WINDOW _REPO
   CLEAR

   **-- SI SE IMPRIME O VISUALIZA
   IF IMPRE
     p_fil = SYS(3)+".LST"
     SET PRINTER TO &p_fil
     DO &_PrgRpt WITH 1
     SET PRINTER TO

     IF !_WP
       FOR i=1 TO Num_C
         IF READY2PR() .AND. IIF(Num_C>1,yesno("Copia "+STR(i,2)+" .Prepare el papel. Listo?"),.T.)
           !TYPE &P_FIL >PRN
         ENDIF
         IF LASTKEY()=27
           EXIT
         ENDIF
       ENDFOR
     ELSE
       !FOXSWAP WP &p_fil
     ENDIF
     ERASE &p_fil
   ELSE
     @ 04,15  SAY  '       Reporte en ejecuci¢n'
     DO &_PrgRpt WITH 2

     IF !_wp
       IF _Dest = "Pantalla "
         MODIFY COMMAND (P_FIL) NOEDIT WINDOW _REPO
         IF YESNO("¨Imprime el reporte ? ")
           FOR i=1 TO Num_C
             IF READY2PR() .AND. IIF(Num_C>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
               !TYPE &P_FIL >PRN
             ENDIF
             IF LASTKEY()=27
               EXIT
             ENDIF
           ENDFOR
         ENDIF
       ENDIF
     ELSE
       !FOXSWAP WP &p_fil
     ENDIF
     IF _DEST # "Archivo  "
       ERASE (P_FIL)
     ENDIF
   ENDIF
   DEACTIVATE WINDOW _REPO
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF

 ON ESCAPE
 SET ESCAPE OFF

 IF !EMPTY(_WndOld)
   ACTIVATE WINDOW &_WndOld
 ELSE
   ACTIVATE SCREEN
 ENDIF

RETURN


PROCEDURE repprg
*---------------
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**    _PrgRpt : Programa reporteador.
PARAMETER  _prgrpt, _tit, _copia, _wp, ran_pg
PRIVATE  resul
resul = 'OK'
IF PARAMETERS()<3
	_copia = 1
ENDIF
IF _copia # 1
	DEFINE WINDOW _xyx FROM 15,45 TO 19,70 COLOR SCHEME 10 TITLE " # DE COPIAS "
	ACTIVATE WINDOW _xyx
	CLEAR
	@ 1,5 SAY " N§ Copias : " GET _copia VALID(_copia>0) PICTURE "99"
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
	_ncopies = _copia
ENDIF

IF PARAMETERS()=3
	_wp = .F.
ENDIF

*- Rango de Paginas
IF PARAMETER() >= 5 .AND. ran_pg
	DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " Rango de P ginas "
	ACTIVATE WINDOW _xyx
	CLEAR
	vpbpage = 1
	vpepage = 32767
	@ 0,5 SAY "Inicio : " GET vpbpage PICTURE "99,999" VALID vpbpage <= vpepage
	@ 1,5 SAY "   Fin : " GET vpepage PICTURE "99,999" VALID vpbpage <= vpepage;
		.AND. vpepage <= 32767
	READ
	RELEASE WINDOW _xyx

	IF LASTKEY()=27
		IF !EMPTY(_wndold)
			ACTIVATE WINDOW &_wndold
		ELSE
			ACTIVATE SCREEN
		ENDIF
		SET CONSOLE &_conso
		RETURN
	ENDIF
ENDIF

**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USE")
	DEFINE WINDOW msg2use   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW 
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
_dest = 'Pantalla '
IF !_wp

ACTIVATE WINDOW msg2use
_dest = 'Impresora'
TITLE = ' DESTINO DE IMPRESION '
p_fil = SPACE(8)
_dest1 = 1
@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _dest PICTURE "@M Impresora,Pantalla,Archivo"
READ
IF _dest = 'Archivo  '
	CLEAR
	@ 01,20 SAY 'Nombre del Archivo :' GET p_fil PICTURE "NNNNNNNN" VALID !EMPTY(p_fil)
	READ
ENDIF
p_fil = ALLTRIM(p_fil) + ".LST"
RELEASE WINDOW msg2use
endif

IF MOD(READKEY(),256)=12
	ON KEY
	SET ESCAPE OFF
	ACTIVATE SCREEN
	RETURN
ENDIF
impre = (_dest='Impresora')

IF !impre .AND. _dest = 'Pantalla '
	p_fil = SYS(3)+".LST"
ENDIF

printing = .T.
IF impre
	IF !EMPTY(LEFT(SYS(0),18))
		IF !yesno("IMPRIME EN IMPRESORA LOCAL ?  <NO = IMPRESORA DE RED>")
			**-- Impresora de red.
			SET PRINTER TO \\ibm_pc\printq_0=lpt1
			SET PRINTER TO \\spooler\nb
		ENDIF
		IF !ready2pr()
			printing = .F.
		ENDIF
	ENDIF
ENDIF

IF printing

	**-- VENTANA DE GENERACION DE REPORTE
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit ;
		FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		DO &_prgrpt WITH 1
	ELSE
		@ 02,20  SAY  '** Reporte en ejecuci¢n **'

		DO &_prgrpt WITH 2

		IF _wp
			ACTIVATE SCREEN
			RESTORE SCREEN FROM pantalla
			RUN FOXSWAP wp &p_fil
		ELSE
			MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
		ENDIF
		IF resul = 'OK'
			IF yesno("¨ Imprime el Reporte ?")
				FOR v = 1 TO _copia
					IF ready2pr() .AND. IIF(_copia>1,yesno("Copia "+STR(v,2)+" .¨Prepare el papel. Listo?"),.T.)
						!TYPE &p_fil>prn
					ENDIF
				ENDFOR
			ENDIF
		ELSE
			resul = 'OK'
		ENDIF
		IF _dest = 'Impresora' .OR. _dest = 'Pantalla '
			ERASE (p_fil)
		ENDIF	
		
	ENDIF
	DEACTIVATE WINDOW _repo
ELSE
	DO standby WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN

RETURN



PROCEDURE ELI_FECHA
*------------------
PARAMETER _campo
PRIVATE averigua, _al, _var, _fec, _campo

   averigua = IIF(PARAMETER()>0,.F.,.T.)

   _al  = ALIAS()
   _var = VARREAD()
   _fec = &_var

   IF TYPE((_var)) # "D"
      RETURN
   ENDIF

   IF averigua

      _campo = .F.
      IF !EMPTY(DBF())
         FOR i=1 TO FCOUNT()
            IF FIELD(i)=UPPER(_var)
               _campo=.T.
               EXIT
            ENDIF
         ENDFOR
      ENDIF
   ENDIF

   IF _campo
      _var = _al + _var
      REPLACE &_var WITH popdate(8,40,_fec)
      FLUSH
   ELSE
      &_var = popdate(8,40,_fec)
   ENDIF

RETURN


FUNCTION ENHAN      && Activa/desactiva enhanced printing
*-------------
PARAMETER ACTIVO
   IF ACTIVO
      RETURN CHR(27)+"E"
   ELSE
      RETURN CHR(27)+"F"
   ENDIF
RETURN ' '


FUNCTION Val_paraX
*-----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
       mCol = 0
       mLong = 40
       mVariable = 'D'
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
  ENDCASE
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor

  IF .NOT. FOUND() .AND. mVariable<>'V'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro AND CODIGOAUX<>'0'
    GO TOP
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLT(CODIGO)+' '+ALLT(CODIGOAUX)+' '+Descri
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = ALLTR(Codigo)+ALLT(CodigoAUX)
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
         @ ROW(),mCol SAY mDescr
         RETURN .T.
    CASE mVariable='V'   && En vista
         @ ROW(),COL()  SAY mValor
         RETURN mDescr
    CASE mVariable='A'   && En vista
         @ ROW(),COL()  SAY mDescr
         RETURN
    CASE mVariable='D'   && En vista
         RETURN mDescr
    CASE mVariable='C'   && Solo codigo
         RETURN .T.
    OTHERWISE            && En browse de edici¢n
         REPLACE &mVariable WITH mValor
         RETURN .T.
  ENDCASE


FUNCTION val_para
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
SEEK filtro+mvalor
*IF .NOT. FOUND() .AND. mVariable<>'V'
IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = ALLTRIM(parma.codigo)
mcuenta= ALLTRIM(parma.descriau2)
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=='W'   && En edici¢n
		REPLACE CODDEP WITH mvalor
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.

	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mDESCR
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE


FUNCTION Val_para1
*-----------------
PARAMETERS mValor, Filtro, mVariable, mCol, mLon

PRIVATE mAlias, Filtro, mVariable, mCol, mLon, _OldWnd

DO CASE
	CASE PARAMETERS() = 2
		mCol = 0
		mVariable = 'D'
	CASE PARAMETERS() = 3
		mCol = 0
ENDCASE

IF PARAMETERS()<5
	mLon = 40
ENDIF

mAlias  = ALIAS()
SELECT parma
SEEK Filtro+mValor
IF !FOUND() .AND. mVariable<>'D'
	SET FILTER TO Tipo = Filtro
	GO TOP
	_OldWnd = WOUTPUT()
	ACTIVATE SCREEN
	
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD Descri
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	DO WHILE .NOT. FOUND()
		ACTIVATE POPUP parametro
		SEEK Tipo+Codigo
	ENDDO
	
	RELEASE POPUP parametro
	
	IF !EMPTY(_OldWnd)
		ACTIVATE WINDOW &_OldWnd
	ENDIF
ENDIF

mValor = parma.Codigo
mDescr = parma.Descri
SET FILTER TO

IF SUBSTR(VERSION(),12)="2.0"
	SELECT IIF(EMPTY(mAlias),SELECT(0),(mAlias))
 ELSE
	IF !EMPTY(mAlias)
		SELECT (mAlias)
	ENDIF
ENDIF

DO CASE
	CASE mVariable=' '   && En edici¢n
		@ ROW(),mCol SAY PADR(mDescr,mLon," ")
		RETURN .T.
	CASE mVariable='A'   && Solo Descripci¢n
		@ ROW(),mCol SAY   mDescr
		RETURN '  '
	CASE mVariable='D'   && Solo descripci¢n
		RETURN PADR(mDescr,mLon," ")
	OTHERWISE            && En browse de edici¢n
		REPLACE &mVariable WITH mValor
		RETURN .T.
ENDCASE
RETURN


PROCEDURE Logos
*--------------
PARAMETERS Rotulo1,Rotulo2,Tiempo

   ACTIVATE SCREEN

   DO CASE
     CASE PARAMETERS() = 0
       STORE SPACE(80) TO Rotulo1,Rotulo2
       Tiempo = 0
     CASE PARAMETERS() = 1
       STORE SPACE(80) TO Rotulo2
       Tiempo = 0
     CASE PARAMETERS() = 2
       Tiempo = 0
   ENDCASE

   FOR I=0 TO 39
     @ 00,I    SAY  C                     COLOR SCHEME  c_borde
     @ 00,79-I SAY  LEFT( Rotulo1,I+1 )   COLOR SCHEME  c_fondo

     @ 24,79-I SAY  C                     COLOR SCHEME  c_borde
     @ 24,00   SAY  RIGHT( Rotulo2,I+1 )  COLOR SCHEME  c_fondo

     FOR Y = 0 TO Tiempo
     ENDFOR

   ENDFOR

   FOR I=40 TO 0 STEP -1
     @ 00,79-I SAY  SUBSTR( Rotulo1,2*(40-I),I+1)   COLOR SCHEME  c_fondo
     @ 00,I    SAY  LEFT( Rotulo1,2*(40-I))         COLOR SCHEME  c_borde
     @ 24,00   SAY  SUBSTR( Rotulo2,41-I,I)         COLOR SCHEME  c_fondo
     @ 24,I    SAY  RIGHT( Rotulo2,2*(40-I))        COLOR SCHEME  c_borde

     FOR Y = 0 TO Tiempo
     ENDFOR

   ENDFOR

*   SET HOUR TO 24
   SET CLOCK ON

RETURN

PROCEDURE err_input
*------------------
* window definido en objects

ACTIVATE WINDOW err_input
@ 00,01 SAY 'DATO NO VALIDO'
aa = INKEY(1,'H')
DEACTIVATE WINDOW err_input
RETURN

FUNCTION Val_Fun
*----------------
PARAMETER v__al, v__dev, v__bus, v__cod, v__tipo, v__x, v__y, v__ver, v__Long
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWnd,_campo, mVali, mRec, v__x, v__y, v__lon, mParam
* Alias :     v__al  " "
* Var.Devol.: v__dev " "
* Var.Mostr.: v__bus ( en el popup ) " expr"
* Var.Modif.: v__cod   variable
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y   para el v__bus
* A ver     : v__Ver     Usado para visualizar, si no se pone, usa v__bus
* Long.A ver: v__Long    Usado para mostrar el tama¤o de v__Ver

mEdita = (parameters()>=5) .and.  (v__tipo # 3)
mMsg   = (parameters()>=7) .and.  (v__tipo # 3)
mVali  = (parameters()=5)  .and.  (v__tipo = 3)
v__Ver = IIF(PARAMETERS()>=8,v__ver,v__bus)
mRec   = IIF(EOF(),-1,RECNO())
mAlias = ALIAS()
mParam = PARAMETERS()
_campo = TRIM(mAlias)+'.'+VARREAD()
SELECT &v__al
_OldWnd = WOUTPUT()

*SET STEP ON

IF !mEdita
	SEEK v__cod
	v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__ver,""))
 ELSE
	IF !EMPTY(IIF(v__tipo#2,v__cod,&_campo))
		GO TOP
		IF EOF()
			DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
			v_fun = .F.
		 ELSE
			_ln = LEN(&v__bus)
			_ln = IIF(_ln>65,65,_ln)
			
			ACTIVATE SCREEN
			DEFINE POPUP v__xx FROM 1,80-_ln TO 12,79 PROMPT FIELD &v__bus
			ON SELECTION POPUP v__xx DEACTIVATE POPUP
			ACTIVATE POPUP v__xx
			RELEASE POPUP v__xx
			
			IF !EMPTY(_OldWnd)
*				ACTIVATE WINDOW &_OldWnd
			ENDIF
			IF Lastkey()=27
				v_fun = .f.
			 ELSE
				v__cod = &v__dev
				IF mMsg
					*@ v__x,v__y SAY IIF(PARAMETERS()#9, TRIM(&v__ver),
					@ v__x,v__y SAY IIF(mParam#9, TRIM(&v__ver),;
					SUBSTR(&v__ver,1,v__Long))
				ENDIF
				IF !EMPTY(mAlias)
					SELECT (mAlias)
					IF (v__tipo  =2)
						IF mRec>0
							GO mRec
							REPLACE &_campo WITH v__cod
							v_fun = .T.
						 ELSE
							DO standby WITH " El archivo est  vac¡o "
							v_fun = .F.
						ENDIF
					 ELSE
						v_fun = .T.
					ENDIF
				 ELSE
					DO standby WITH " NO HAY ARCHIVO ABIERTO "
				ENDIF
			ENDIF
		ENDIF
	 ELSE
	 	SEEK IIF(v__tipo#2,v__cod,&_campo)
	 	IF mMsg .AND. FOUND()
	 		@ v__x,v__y SAY IIF(PARAMETERS()#9, TRIM(&v__ver),;
	 		SUBSTR(&v__ver,1,v__Long))
	 	ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF

IF EMPTY(mAlias)
	SELECT 0
 ELSE
	SELECT (mAlias)
ENDIF

IF !EMPTY(ALIAS())
	IF mRec>0
		GO mRec
	 ELSE
		GO BOTTOM
	ENDIF
ENDIF

RETURN v_fun

*FUNCTION Val_Fun
*----------------
PARAMETER v__al, v__dev, v__bus, v__cod, v__tipo, v__x, v__y, v__ver, v__Long
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWnd,_campo, mVali, mRec, v__x, v__y, v__lon
* Alias :     v__al  " "
* Var.Devol.: v__dev " "
* Var.Mostr.: v__bus ( en el popup ) " expr"
* Var.Modif.: v__cod   variable
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y   para el v__bus
* A ver     : v__Ver     Usado para visualizar, si no se pone, usa v__bus
* Long.A ver: v__Long    Usado para mostrar el tama¤o de v__Ver
   mEdita = (parameters()>=5) .and.  (v__tipo # 3)
   mMsg   = (parameters()>=7) .and.  (v__tipo # 3)
   mVali  = (parameters()=5)  .and.  (v__tipo = 3)
   v__Ver = IIF(PARAMETERS()=8,v__ver,v__bus)
   mRec   = IIF(EOF(),-1,RECNO())
   mAlias = ALIAS()
   _campo = TRIM(mAlias)+'.'+VARREAD()
   
   SELECT &v__al
   _OldWnd = WOUTPUT()
   IF !mEdita
      SEEK v__cod
      v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__ver,""))
   ELSE
      IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
         GO TOP
         IF EOF()
            DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
            v_fun = .F.
         ELSE
            _ln = LEN(&v__bus)
            _ln = IIF(_ln>65,65,_ln)

			ACTIVATE SCREEN
            DEFINE POPUP v__xx FROM 1,80-_ln TO 12,79 PROMPT FIELD &v__bus
            ON SELECTION POPUP v__xx DEACTIVATE POPUP
            ACTIVATE POPUP v__xx
            RELEASE POPUP v__xx

            IF !EMPTY(_OldWnd)
               ACTIVATE WINDOW &_OldWnd
            ENDIF
            IF Lastkey()=27
               v_fun = .f.
            ELSE
               v__cod = &v__dev
               IF mMsg
                  @ v__x,v__y SAY IIF(PARAMETERS()#9, TRIM(&v__ver),;
                    SUBSTR(&v__ver,1,v__Long))
               ENDIF
               IF !EMPTY(mAlias)
                  SELECT (mAlias)
                  IF (v__tipo  =2)
                     IF mRec>0
                        GO mRec
                        REPLACE &_campo WITH v__cod
                        v_fun = .T.
                     ELSE
                        DO standby WITH " El archivo est  vac¡o "
                        v_fun = .F.
                     ENDIF
                  ELSE
                     v_fun = .T.
                  ENDIF
               ELSE
                  DO standby WITH " NO HAY ARCHIVO ABIERTO "
               ENDIF
            ENDIF
         ENDIF
      ELSE
         SEEK IIF(v__tipo#2,v__cod,&_campo)
         IF mMsg .AND. FOUND()
            @ v__x,v__y SAY ALLT(&v__ver)
         ENDIF
         v_fun = FOUND()
      ENDIF
   ENDIF

   IF EMPTY(mAlias)
      SELECT 0
   ELSE
      SELECT (mAlias)
   ENDIF

   IF !EMPTY(ALIAS())
      IF mRec>0
         GO mRec
      ELSE
         GO BOTTOM
      ENDIF
   ENDIF
RETURN v_fun


FUNCTION ve_passw
*----------------
** Pide el password de acceso a cada opci¢n del sistema
PARAMETER _psw
PRIVATE _ln, xxx

   _ln = LEN(_psw)
   xxx = SPACE(_ln)

   DEFINE WINDOW _pasw FROM 12,15 TO 17,65  TITLE " Password de seguridad " COLOR SCHEME 5
   ACTIVATE WINDOW _pasw
   CLEAR
   @ 1, 4 SAY "Opci¢n restringida, ingrese password: "
   @ 2,20 SAY "["+SPACE(_ln)+"]"
   @ 2,21 GET xxx PICTURE "@!" COLOR ,X
   READ
   RELEASE WINDOW _pasw

   IF LASTKEY()#27 .AND. xxx=_psw
      RETURN .T.
   ELSE
      DO STANDBY WITH "Password incorrecto. Acceso denegado !"
   ENDIF

RETURN .F.


FUNCTION MAC
*-----------
PARAMETER _n_mes
   PRIVATE _n_mes, v_fun

   IF !BETWEEN(n_mes,1,12)
      RETURN "Inv lido"
   ENDIF
   v_fun ="Enero    Febrero  Marzo    Abril    Mayo     Junio    ;
Julio    Agosto   SetiembreOctubre  NoviembreDiciembre"

RETURN TRIM(SUBSTR(v_fun,9*(n_mes-1)+1,9))


FUNCTION letras
*--------------
parameter num
private cad1

cad1 = ''
IF Num = 0
   RETURN ( cad1 )
ENDIF

deci = INT(MOD(num,1)*100)

cmm = INT ( NUM / 1000000 )
cmu = MOD ( NUM , 1000000 )
cmu = INT(cmu)

DO CASE
   CASE  ( cmm = 1 )
      cad1 = ' un mill¢n'
   CASE  ( cmm = 0 )
      cad1 = ''
   CASE  ( cmm > 1 )
      cad1 = NUM6( cmm ) + ' millones'
   OTHERWISE
      cad1 = '*****'
ENDCASE

cad1 = cad1 + NUM6( cmu )
cad1 = cad1 + ' y '+PADL(ALLTRIM(STR(deci,2)),2,'0')+'/100 '

RETURN ( cad1 )


FUNCTION letras1
*--------------
PARAMETERS wmonto,simbolo
  wletras = ""
  DIMENSION letras(4,9)
  letras(1,1) = 'UN '
  letras(1,2) = 'DOS '
  letras(1,3) = 'TRES '
  letras(1,4) = 'CUATRO '
  letras(1,5) = 'CINCO '
  letras(1,6) = 'SEIS '
  letras(1,7) = 'SIETE '
  letras(1,8) = 'OCHO '
  letras(1,9) = 'NUEVE '
  letras(2,1) = 'ONCE '
  letras(2,2) = 'DOCE '
  letras(2,3) = 'TRECE '
  letras(2,4) = 'CATORCE '
  letras(2,5) = 'QUINCE '
  letras(2,6) = 'DIECISEIS '
  letras(2,7) = 'DIECISIETE '
  letras(2,8) = 'DIECIOCHO '
  letras(2,9) = 'DIECINUEVE '
  letras(3,1) = 'DIEZ'
  letras(3,2) = 'VEINTI'
  letras(3,3) = 'TREINTA'
  letras(3,4) = 'CUARENTA'
  letras(3,5) = 'CINCUENTA'
  letras(3,6) = 'SESENTA'
  letras(3,7) = 'SETENTA'
  letras(3,8) = 'OCHENTA'
  letras(3,9) = 'NOVENTA'
  letras(4,1) = 'CIENTO '
  letras(4,2) = 'DOSCIENTOS '
  letras(4,3) = 'TRESCIENTOS '
  letras(4,4) = 'CUATROCIENTOS '
  letras(4,5) = 'QUINIENTOS '
  letras(4,6) = 'SEISCIENTOS '
  letras(4,7) = 'SETECIENTOS '
  letras(4,8) = 'OCHOCIENTOS '
  letras(4,9) = 'NOVECIENTOS '

  centavos = wmonto - INT(wmonto)
  w2  = STR( INT(wmonto ),18)      && La variable 'wmonto' es el monto a fox_repl a letras
  i = 1
  DO WHILE i <= 18
     w1  = 0
     n   = SUBSTR( w2, i, 3 )
     IF VAL( n ) > 0
        x  = VAL( n )
        y = 1
        DO WHILE y <= 3        && Repite para cada 3 digitos.
           x1 = SUBSTR( n, y, 1 )
           IF VAL(x1) # 0
              DO CASE
                 CASE y = 1
                    IF VAL(n) = 100
                       wletras = wletras + 'CIEN '
                    ELSE
                       wletras = wletras + letras(4,VAL(x1))
                    ENDIF
                 CASE y = 2
                     IF VAL(SUBSTR(n,y,2)) > 10 .AND. VAL(SUBSTR(n,y,2)) < 20
                        wletras = wletras + letras(2,VAL(SUBSTR( n, 3, 1 ) ))
                     ELSE
                        wletras = wletras + letras(3,VAL(x1))
                     ENDIF
                 CASE y = 3
                     IF !SUBSTR(n,2,1) $ '1'
                        IF !SUBSTR(n,2,1) $ '0' .AND. !EMPTY(wletras)
                           wletras = wletras + ' Y '
                        ENDIF
                        wletras = wletras + letras(1,VAL(x1))
                     ENDIF
              ENDCASE
           ENDIF
           y = y + 1
        ENDDO

        DO CASE
           CASE i = 1 .OR. i = 7 .OR. i = 13
               wletras = wletras + 'MIL '
           CASE i = 4
               wletras = wletras + 'BILLON '
           CASE i = 10
               IF x  = 1
                  wletras = wletras + 'MILLON '
               ELSE
                  wletras = wletras + 'MILLONES '
               ENDIF
        ENDCASE
     ENDIF
     i = i + 3
  ENDDO
**wletras = ALLTRIM(wletras) + ' Y  ' + STR(centavos*100,2) + '/100 ' + simbolo
 wletras = ALLTRIM(wletras) + ' Y  ' + PADL(ALLTRIM(STR(centavos*100,2)),2,'0') + '/100 ' + simbolo

  ll  = SUBSTR( wletras, 51, 15)
  l   = 0
  t   = 16
  r   = LEN( wletras )
  IF r > 65
    DO WHILE t <> 20
      t = t - 1
      p = SUBSTR(ll,t,1)
      IF p = ' '
        numlet1 = SUBSTR( wletras, 1, 50 ) + SUBSTR( ll, 1, T )
        numlet2 = SUBSTR( wletras, ( 51 + t ), ( r - ( 50 + t ) ) )
        t    = 20
      ENDIF
    ENDDO
  ELSE
    numlet1 = wletras
  ENDIF
  RETURN numlet1



FUNCTION NUM6
*-------------
PARAMETER  cm
PRIVATE cad2
cmx = INT( cm / 1000 )
cuu = MOD ( cm , 1000 )
cad2 = ''
DO CASE
   CASE ( cmx = 1 )
      cad2 = ' mil'
   CASE ( cmx = 0 )
      cad2 = ''
   CASE ( cmx > 1 )
      cad2 = NUM3( cmx ) + ' mil'
   OTHERWISE
      cad2 = '****'
ENDCASE

cad2 = cad2 + NUM3( cuu )

RETURN ( cad2 )


FUNCTION NUM3
*-------------
parameters  cu

DECLARE Centena( 9 )
private cad3

Centena( 1 ) = ' ciento'
Centena( 2 ) = ' doscientos'
Centena( 3 ) = ' trecientos'
Centena( 4 ) = ' cuatrocientos'
Centena( 5 ) = ' quinietos'
Centena( 6 ) = ' seiscientos'
Centena( 7 ) = ' setecientos'
Centena( 8 ) = ' ochocientos'
Centena( 9 ) = ' novecientos'

cad3 = ''
ce = INT( cu / 100 )
de = MOD( cu,100 )

IF ( ce = 1  .AND. de = 0 )
   cad3 = ' cien'
ELSE
   IF ce <> 0
      cad3 = Centena( ce )
   ENDIF
   IF de <> 0
      cad3 = cad3 + NUM2( de )
   ENDIF
ENDIF
RETURN ( cad3 )


FUNCTION  NUM2
*--------------
PARAMETER  du

PRIVATE cad2

DECLARE dece  ( 7 )
DECLARE Unidad( 9 )
DECLARE Unid  ( 6 )

dece( 1  ) = ' treinta'
dece( 2  ) = ' cuarenta'
dece( 3  ) = ' cincuenta'
dece( 4  ) = ' sesenta'
dece( 5  ) = ' setenta'
dece( 6  ) = ' ochenta'
dece( 7  ) = ' noventa'

Unidad( 1 ) = ' un'
Unidad( 2 ) = ' dos'
Unidad( 3 ) = ' tres'
Unidad( 4 ) = ' cuatro'
Unidad( 5 ) = ' cinco'
Unidad( 6 ) = ' seis'
Unidad( 7 ) = ' siete'
Unidad( 8 ) = ' ocho'
Unidad( 9 ) = ' nueve'

Unid( 1 ) = ' diez'
Unid( 2 ) = ' once'
Unid( 3 ) = ' doce'
Unid( 4 ) = ' trece'
Unid( 5 ) = ' catorce'
Unid( 6 ) = ' qince'

cad2 = ''

d = INT( du /10 )
u = MOD ( du,10 )

DO CASE
   CASE ( d >= 3 .and. d <= 9 )
      IF ( u = 0 )
         cad2 = Dece( d-2 )
      ELSE
         cad2 = Dece ( d- 2 ) + 'i' + LTRIM(Unidad( u ))
*         cad2 = Dece ( d- 2 ) + ' y' + Unidad( u )
      ENDIF
   CASE ( d = 2 )
      IF ( u = 0 )
         cad2 = ' veinte'
      ELSE
         cad2 = ' veinti' + LTRIM(Unidad( u ))
      ENDIF
   CASE ( d = 1 )
      DO CASE
         CASE ( u <= 5 )
            cad2 = LTRIM(unid( u + 1 ))
         CASE ( u >= 6 )
            cad2 = ' dieci' + LTRIM(Unidad( u ))
         OTHERWISE
            cad2 = '****'
      ENDCASE
   CASE ( d = 0 )
      IF ( u <> 0 )
         cad2 = ' ' + LTRIM(Unidad( u ))
      ENDIF
   OTHERWISE
      cad2 = '****'
   ENDCASE
RETURN ( cad2 )


FUNCTION IsDisket
*----------------
PARAMETER _Drive
   PRIVATE m.Drive, vRet

   vRet = .T.
   m.Drive = _Drive
   LOAD IsDiskIn.BIN
   CALL isdiskin WITH m.drive
   DO WHILE m.Drive='0:'
      ??chr(7) + chr(7)
      DO standby WITH '! El drive '+_Drive+ ' no est  listo !'
      IF LASTKEY()=27
         vRet = .F.
         EXIT
      ENDIF
      m.Drive = _Drive
      CALL isdiskin WITH m.drive
   ENDDO
   _Drive = m.Drive
   RELEASE MODULE IsDiskIn.BIN

RETURN vRet


FUNCTION VAL_READ
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
PRIVATE _Fun
_Fun = .T.
IF LASTKEY() # 27
  IF .NOT. YESNO('¨ Est n correctos los datos ?')
    _Fun = .F.
  ENDIF
ENDIF
RETURN (_Fun)


PROCEDURE CLRSCR
*---------------
* Borra la pantalla desde arriba hacia abajo
PRIVATE fil,col
fil = 0
DO WHILE (fil < 24)
  @ fil,0 CLEAR TO fil,80
  fil = fil + 1
  = INKEY(0.001,'H')
ENDDO
CLEAR
RETURN



PROCEDURE BOXOPEN
*-----------------
* Pinta la pantalla desde el centro
* Autor: C‚sar Ch varry Arias
PRIVATE fil,col
fil = 11
col = 39
DO WHILE (col > 0) .AND. (fil > 0)
  @ fil,col,23-fil,79-col BOX '±±±±±±±±±±'
  fil = fil - 1
  col = col - 3
  = INKEY(.001,'H')
ENDDO
RETURN


FUNCTION poperror
*----------------
PARAMETERS err_mess, _xtime
PRIVATE cur_color, cur_curs, bord_str, err_mess, say_mess
PRIVATE num_lines, start_line, cur_width, i, rvalue
PRIVATE cur_win, _mens

************************************************************
*  poperror(<expC>)                                        *
*  Version # 2.0  FP          Date: 07-20-91               *
*  Programmed by: Malcolm C. Rubel                         *
*                                                          *
*  Copyright (c) 1991                                      *
*  Performance Dynamics Associates                         *
*  All Rights Reserved                                     *
*                                                          *
*  Note: Procedure pops up an error in lower right corner  *
*  of screen and waits for a keypress to go back to screen.*
*  Modified 7-20-91 to include PUSH/POP key.               *
* Modificado por Cesar Chavarry A.: Pintado de letras      *
************************************************************

PUSH KEY CLEAR
**= beep(1)

cur_win = WOUTPUT()
cur_width = SET('memowidth')  &&  save the old setting
SET MEMOWIDTH TO 48

num_lines = MEMLINES(err_mess) &&  how long is message?
Altura    = num_lines + 3 + 2  &&  Altura de la ventana a mostrar.
start_line= (25 - Altura)/2    &&  starting row for box

DEFINE WINDOW poperr FROM start_line,13 TO start_line+Altura,66 DOUBLE ;
              SHADOW COLOR SCHEME 5
ACTIVATE WINDOW poperr

IF num_lines = 1
  _mens = PADC(err_mess,48)
  FOR j = 1 TO LEN(_mens)
    @ 1,1+j SAY SUBSTR(_mens,j,1)
    IF (PARAMETERS() >= 2 .AND. _xtime) .OR. PARAMETERS < 2
      =INKEY(0.001,' ')
    ENDIF
  ENDFOR
  i = 2
ELSE
  FOR i = 1 TO num_lines
    say_mess = LTRIM( MLINE(err_mess,i) )
    * @ i,02 SAY LTRIM(say_mess)
    FOR j = 1 TO LEN(say_mess)
       @ i,1+j SAY SUBSTR( say_mess,j,1)
       IF (PARAMETERS() >= 2 .AND. _xtime) .OR. PARAMETERS() < 2
         =INKEY(0.001,' ')
       ENDIF
    ENDFOR

  ENDFOR
ENDIF

@ i+1,01 SAY replicate(chr(196),49)      &&  draw line
_mens = 'Presione una tecla para continuar ....'
FOR j = 1 TO LEN(_mens)
   @ i+2,1+j SAY SUBSTR(_mens,j,1)
   =INKEY(0.001,' ')
ENDFOR

rvalue = inkey(0,'hm')        &&  hide the cursor

IF wexist('poperr')
  RELEASE WINDOW poperr
ENDIF

IF empty(cur_win)
  ACTIVATE SCREEN
ENDIF

SET MEMOWIDTH TO cur_width
POP KEY

RETURN(rvalue)


FUNCTION Val_cuen
*----------------
PARAMETER xcod,xtipo,_x,_y    && codb : codigo ;   tipo : 1=valida, nada:descripci¢n
** TIPO = .F. ---> Campo
**        .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind

mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and. xTIPO
xcod   = SUBSTR(xcod,1,10)
v_fun  = .F.

_campo = VARREAD()

mAlias = ALIAS()
SELECT Cuen
_OldWnd = WOUTPUT()

SEEK xcod
IF !mEdita
   v_fun = IIF(FOUND(),Descri,"")
ELSE
  IF EMPTY(xcod) .OR. !FOUND()
     ACTIVATE SCREEN
     DEFINE POPUP _xx FROM 5,40 PROMPT FIELD Descri
     ON SELECTION POPUP _xx DEACTIVATE POPUP
     ACTIVATE POPUP _xx
     IF !EMPTY(_OldWnd)
        ACTIVATE WINDOW &_OldWnd
     ENDIF
     IF Lastkey()=27
        V_FUN = .f.
        xcod  = SPACE(10)
     ELSE
        xcod = cuenta
     ENDIF
  ENDIF
  IF !EMPTY(xcod)
    V_FUN = .T.
    IF mMsg
      @ _x,_y SAY TRIM(Cuenta)
      @ _x,_y+11 SAY Descri
    ENDIF
    SELECT (mAlias)
    IF !xTipo
      REPLACE &_campo WITH  Cuen.Cuenta
    ENDIF
    v_fun = .T.
  ENDIF
ENDIF

SELECT (mAlias)
RETURN v_fun



FUNCTION Val_Fun1
*----------------
PARAMETER v__al, v__dev, v__bus, v__cod, v__tipo, v__x, v__y, v__ver, vTag1, vTag2
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWnd,_campo, mVali, mRec, v__x, v__y, v__lon
* Alias :     v__al  " "
* Var.Devol.: v__dev " "
* Var.Mostr.: v__bus ( en el popup ) " expr"
* Var.Modif.: v__cod   variable
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y   para el v__bus
* A ver     : v__Ver     Usado para visualizar, si no se pone, usa v__bus
* vTag1     : Tag indexado por c¢digo
* vTag2     : Tag indexado por Nombre

   mEdita = (parameters()>=5) .and.  (v__tipo # 3)
   mMsg   = (parameters()>=7) .and.  (v__tipo # 3)
   mVali  = (parameters()=5)  .and.  (v__tipo = 3)
   v__Ver = IIF(PARAMETERS()>=8,v__ver,v__bus)
   mRec   = IIF(EOF(),-1,RECNO())
   mAlias = ALIAS()
   _campo = TRIM(mAlias)+'.'+VARREAD()

   SELECT &v__al
   _OldWnd = WOUTPUT()
   IF !mEdita
      SEEK v__cod
      v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__ver,""))
   ELSE
     IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
       GO TOP
       IF EOF()
         DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
         v_fun = .F.
       ELSE
         _ln    = LEN(&v__bus)
         _ln    = IIF(_ln>65,65,_ln)
         v__bus = "' '+" + v__bus

	     ACTIVATE SCREEN
         DEFINE WINDOW Ayuda FROM 1,79 - _ln TO 22,79 COLOR SCHEME 10 DOUBLE
         DEFINE POPUP v__xx FROM 0,0 PROMPT FIELD &v__bus COLOR SCHEME 9;
           TITLE ' ® F10 ¯ Elegir '


         ACTIVATE WINDOW Ayuda
         _Key10 = ON('KEY','F10')
         ON KEY LABEL F10 KEYBOARD CHR(23)
         vChoice = 1
         vCod    = SPACE(LEN(v__Cod))
         vNombre = SPACE(40)
         vPict   = '@S' + ALLTRIM(STR(_ln-15)) + '!'
         GO TOP
         vUbica  = &v__bus

         DO WHILE LASTKEY() # 23 .AND. LASTKEY() # 27
           @  0,0              GET vUbica  POPUP v__xx SIZE 12, _ln - 1;
                                           VALID FunBus1(0,"",vUbica)
           @ 13,9              GET vChoice PICTURE  '@^ CODIGO;NOMBRE';
                                           SIZE 1,10 COLOR SCHEME 8,9
           @ 17,1 SAY 'C¢digo:'GET vCod    PICTURE  '@!'  WHEN vChoice = 1;
                                           VALID FunBus1(vChoice,vCod,vUbica);
                                           COLOR SCHEME 8
           @ 18,1 SAY 'Nombre:'GET vNombre PICTURE  vPict WHEN vChoice = 2;
                                           VALID FunBus1(vChoice,vNombre,vUbica);
                                           COLOR SCHEME 8
         READ
         ENDDO
         ON KEY LABEL F10 &_Key10
         DEACTIVATE WINDOW Ayuda


         IF !EMPTY(_OldWnd)
           ACTIVATE WINDOW &_OldWnd
         ENDIF
         IF LASTKEY()=27
           v_fun = .f.
         ELSE
           v__cod = &v__dev
           IF mMsg
             @ v__x,v__y SAY TRIM(&v__ver)
           ENDIF
           IF !EMPTY(mAlias)
             SELECT (mAlias)
             IF (v__tipo  =2)
               IF mRec>0
                 GO mRec
                 REPLACE &_campo WITH v__cod
                 v_fun = .T.
               ELSE
                 DO standby WITH " El archivo est  vac¡o "
                  v_fun = .F.
               ENDIF
             ELSE
               v_fun = .T.
             ENDIF
           ELSE
             DO standby WITH " NO HAY ARCHIVO ABIERTO "
           ENDIF
         ENDIF
       ENDIF
     ELSE
       SEEK IIF(v__tipo#2,v__cod,&_campo)
       IF mMsg .AND. FOUND()
          @ v__x,v__y SAY TRIM(&v__ver)
       ENDIF
       v_fun = FOUND()
     ENDIF
   ENDIF

   IF EMPTY(mAlias)
      SELECT 0
   ELSE
      SELECT (mAlias)
   ENDIF

   IF !EMPTY(ALIAS())
      IF mRec>0
         GO mRec
      ELSE
         GO BOTTOM
      ENDIF
   ENDIF
RETURN v_fun

*--------------------------
* Funci¢n Val_aux : Ubica el c¢digo Auxiliar por medio
* de un popup que esta ordenado en forma alfab‚tica
* 
*----------------
FUNCTION val_aux
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias
filtro=alltrim(filtro)
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()
morden  = ORDER()

SELECT Auxi
SEEK filtro+mvalor
*IF .NOT. FOUND() .AND. mVariable<>'V'
IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(filtro)
	    SET FILTER TO tipo = filtro
	ENDIF    
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
			SET ORDER TO (morden)
		ENDIF
		RETURN
	ENDIF
	SET ORDER TO Auxil10
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	SELE AUXI
	SET ORDER TO Auxil1
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = Auxi.codigo
mdescr = SUBSTR( Auxi.descri, 1, mlong )
IF !EMPTY( malias )
	SELECT (malias)
    SET ORDER TO (morden)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE


FUNCTION funbus
*--------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '000.000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 1
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
RETURN

FUNCTION funbus1
*--------------
*PARAMETER 0,"",vUbica)
PARAMETER V1,V2,V3
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '000.000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 1
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
RETURN

FUNCTION funbusdet
*--------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '00.000.000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!.!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 1
			SEEK 'B'+ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
RETURN


FUNCTION Val_para1
*-----------------
PARAMETERS mValor, Filtro, mVariable, mCol, mLon

   PRIVATE mAlias, Filtro, mVariable, mCol, mLon, _OldWnd

   DO CASE
     CASE PARAMETERS() = 2
       mCol = 0
       mVariable = 'D'
     CASE PARAMETERS() = 3
       mCol = 0
   ENDCASE
   IF PARAMETERS()<5
      mLon = 40
   ENDIF

   mAlias  = ALIAS()
   SELECT auxi
   SEEK Filtro+mValor
   IF !FOUND() .AND. mVariable<>'D'
     SET FILTER TO Tipo = Filtro
     GO TOP
     _OldWnd = WOUTPUT()
     ACTIVATE SCREEN

     DEFINE POPUP parametro FROM 03,40 PROMPT FIELD Descri
     ON SELECTION POPUP parametro DEACTIVATE POPUP
     DO WHILE .NOT. FOUND()
       ACTIVATE POPUP parametro
       SEEK Tipo+Codigo
     ENDDO
     RELEASE POPUP parametro

     IF !EMPTY(_OldWnd)
        ACTIVATE WINDOW &_OldWnd
     ENDIF
   ENDIF

   mValor = auxi.Codigo
   mDescr = auxi.Descri
   SET FILTER TO

   IF SUBSTR(VERSION(),12)="2.0"
      SELECT IIF(EMPTY(mAlias),SELECT(0),(mAlias))
   ELSE
      IF !EMPTY(mAlias)
         SELECT (mAlias)
      ENDIF
   ENDIF

   DO CASE
     CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol SAY PADR(mDescr,mLon," ")
       RETURN .T.
     CASE mVariable='D'   && Solo descripci¢n
       RETURN PADR(mDescr,mLon," ")
     OTHERWISE            && En browse de edici¢n
       REPLACE &mVariable WITH mValor
       RETURN .T.
   ENDCASE
RETURN


FUNCTION Val_para2
*-----------------
PARAMETERS mValor, Filtro, mVariable, mCol, mLon

   PRIVATE mAlias, Filtro, mVariable, mCol, mLon, _OldWnd

   DO CASE
     CASE PARAMETERS() = 2
       mCol = 0
       mVariable = 'D'
     CASE PARAMETERS() = 3
       mCol = 0
   ENDCASE
   IF PARAMETERS()<5
      mLon = 40
   ENDIF

   mAlias  = ALIAS()
   SELECT parma
   SEEK Filtro+mValor
   IF !FOUND() .AND. mVariable<>'D'
     SET FILTER TO Tipo = Filtro
     GO TOP
     _OldWnd = WOUTPUT()
     ACTIVATE SCREEN

     DEFINE POPUP parametro FROM 03,40 PROMPT FIELD Descri
     ON SELECTION POPUP parametro DEACTIVATE POPUP
     DO WHILE .NOT. FOUND()
       ACTIVATE POPUP parametro
       SEEK Tipo+CodigoAux
     ENDDO
     RELEASE POPUP parametro

     IF !EMPTY(_OldWnd)
        ACTIVATE WINDOW &_OldWnd
     ENDIF
   ENDIF

   mValor = parma.CodigoAux
   mDescr = parma.Descri
   SET FILTER TO

   IF SUBSTR(VERSION(),12)="2.0"
      SELECT IIF(EMPTY(mAlias),SELECT(0),(mAlias))
   ELSE
      IF !EMPTY(mAlias)
         SELECT (mAlias)
      ENDIF
   ENDIF

   DO CASE
     CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol SAY PADR(mDescr,mLon," ")
       RETURN .T.
     CASE mVariable='A'   && Solo Descripci¢n
       @ ROW(),mCol SAY   mDescr
       RETURN '  '
     CASE mVariable='D'   && Solo descripci¢n
       RETURN PADR(mDescr,mLon," ")
     OTHERWISE            && En browse de edici¢n
       REPLACE &mVariable WITH mValor
       RETURN .T.
   ENDCASE
RETURN


FUNCTION val_subp && VALIDA SUBPROGRAMAS
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong
PRIVATE malias,mvalor,filtro
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
ENDCASE
malias  = ALIAS()
SELECT parma
SEEK filtro+mvalor

IF !FOUND() AND !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo+codigo = filtro
	GO TOP
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(codigoaux)+' '+SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = parma.codigoaux
mdescr = SUBSTR( parma.descri, 1, mlong )
SET ORDE TO 1
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL   SAY mvalor
		@ ROW(),MCOL+6 SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE

FUNCTION val_pyac  && VALIDA  PROYECTOS Y/O ACTIVIDADES
*----------------
PARAMETERS nvalor, nfiltro, nvariable, ncol, nlong ,nancho
PRIVATE nalias
DO CASE
	CASE PARAMETERS() = 2
		ncol = 0
		nvariable = ' '
		nlong = 40
		nancho = 6
	CASE PARAMETERS() = 3
		ncol = 0
		nlong = 40
		nancho = 6
	CASE PARAMETERS() = 4
		nlong = 40               && Longitud campo DESCRI
		nancho = 6
	CASE PARAMETERS() = 5		
		nancho = 6
ENDCASE
nalias  = ALIAS()
IF ALLTRIM(m.tipfun)='F' 
   U=IIF(ALIAS()="ORDSE",9,7)
*   @ U,22 say '    '
endif   
SELECT maepre						
IF ALLTRIM(m.tipfun)='I'
	SET ORDER TO MAEPRE6
ELSE
	SET ORDER TO MAEPRE7
ENDIF
SEEK ALLTRIM(m.TipFun)+nfiltro+nvalor
**SEEK nfiltro+nvalor

IF !FOUND()	OR !nvariable $'V'
	SET FILTER TO periodo+codfun+codprg+codsubpr = nfiltro AND TIPFUN = ALLT(M.TIPFUN)
	GO TOP
	IF !EOF()
		IF !EMPTY(nvalor) 
		SEEK ALLTRIM(m.TipFun)+nfiltro+nvalor
			IF !FOUND()
				DO rolea
			ENDIF
		ELSE
			DO rolea
		ENDIF
	ELSE
		DO standby WITH 'Error en Codificaci¢n program tica'
		SET FILTER TO
		IF !EMPTY( nalias )
			SELECT (nalias)
		ENDIF
		return .f.
	ENDIF	
ENDIF	
nvalor = IIF(ALLTRIM(m.tipfun)='I',maepre.codproy ,maepre.codact)
ndescr = SUBSTR( maepre.descri, 1, nlong )
*-Variables
SET FILTER TO
IF !EMPTY( nalias )
	SELECT (nalias)
ENDIF
DO CASE
	CASE nvariable==' '   && En edici¢n
		@ ROW(),ncol     SAY ndescr
		RETURN .T.
	CASE nvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),ncol SAY ndescr
		RETURN
	CASE nvariable=='V'   && En vista
		@ ROW(),COL()  SAY PADR(nvalor,nancho,' ')
		RETURN ndescr
	CASE nvariable=='D'   && En vista
		RETURN ndescr
	CASE nvariable=='Z'   && En vista SIN PINTAR
		RETURN ndescr
	CASE nvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		&nvariable = nvalor
		@ ROW(),ncol   SAY ndescr		
		RETURN .T.
ENDCASE


FUNCTION Xval_pyac  && VALIDA  PROYECTOS Y/O ACTIVIDADES
*-----------------
PARAMETERS nvalor, nfiltro, nvariable, ncol, nlong ,nancho
PRIVATE nalias
DO CASE
	CASE PARAMETERS() = 2
		ncol = 0
		nvariable = ' '
		nlong = 40
		nancho = 6
	CASE PARAMETERS() = 3
		ncol = 0
		nlong = 40
		nancho = 6
	CASE PARAMETERS() = 4
		nlong = 40               && Longitud campo DESCRI
		nancho = 6
	CASE PARAMETERS() = 5		
		nancho = 6
ENDCASE
nalias  = ALIAS()
IF SUBSTR(NFILTRO,1)='F' 
   U=IIF(ALIAS()="ORDSE",9,7)
*   @ U,22 say '    '
endif   
SELECT maepre						
IF SUBSTR(NFILTRO,1)='I'
	SET ORDER TO MAEPRE6
ELSE
	SET ORDER TO MAEPRE7
ENDIF
SEEK nfiltro+nvalor
IF !FOUND()	OR !nvariable $'V'
	SET FILTER TO ALLT(TIPFUN)+periodo+codfun+codprg+codsubpr = nfiltro  AND TIPFUN = SUBSTR(NFILTRO,1)

	GO TOP
	IF !EOF()
		IF !EMPTY(nvalor) 
		SEEK nfiltro+nvalor                                  
		   IF !FOUND()
			**	DO rolea
			ENDIF
		ELSE
			** DO rolea
		ENDIF
	ELSE
		DO standby WITH 'Error en Codificaci¢n program tica'
		SET FILTER TO
		IF !EMPTY( nalias )
			SELECT (nalias)
		ENDIF
		return .f.
	ENDIF	
ENDIF	
nvalor = IIF(ALLTRIM(m.tipfun)='I',maepre.codproy ,maepre.codact)
ndescr = SUBSTR( maepre.descri, 1, nlong )
*-Variables
SET FILTER TO
IF !EMPTY( nalias )
	SELECT (nalias)
ENDIF
DO CASE
	CASE nvariable==' '   && En edici¢n
		@ ROW(),ncol     SAY ndescr
		RETURN .T.
	CASE nvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),ncol SAY ndescr
		RETURN
	CASE nvariable=='V'   && En vista
		@ ROW(),NCOL  SAY PADR(nvalor,nancho,' ')
		RETURN ndescr
	CASE nvariable=='D'   && En vista
		RETURN ndescr
	CASE nvariable=='Z'   && En vista SIN PINTAR
		RETURN ndescr
	CASE nvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		&nvariable = nvalor
		@ ROW(),ncol   SAY ndescr		
		RETURN .T.
ENDCASE

PROCEDURE rolea
*---------------
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06,10 TO 17,69 DOUBLE FLOAT SHADOW COLOR SCHEME 10
GO TOP
DO CASE
	CASE ALLTRIM(m.tipfun) = 'I' AND !EOF() 
		BROWSE WINDOW _xx TITLE ' PROYECTOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codproy  :H='Pry' ,;
			descri   :H='Detalle'
	CASE ALLTRIM(m.tipfun) = 'F' AND !EOF() 
		BROWSE WINDOW _xx TITLE ' ACTIVIDAD :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codact   :H='Act',;
			descri   :H='Detalle'
	OTHER
		IF !EMPTY(nvalor)
			DO standby WITH 'No se tiene '+IIF(ALLTRIM(m.tipfun)='F','Actividad','Proyecto')+' en referencia'
		ENDIF
eNDCASE
ON KEY LABEL ENTER 
RELEASE WINDOW _xx
IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN

FUNCTION val_supy && VALIDA SUBPROYECTOS    (DEL PRESUPUESTO)
*----------------
PARAMETERS cvalor, cfiltro, cvariable, ccol, clong , cAncho
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		ccol = 0
		cvariable = ' '
		clong = 40
		cAncho = 6
	CASE PARAMETERS() = 3
		ccol = 0
		clong = 40
		cAncho = 6		
	CASE PARAMETERS() = 4
		clong = 40               && Longitud campo DESCRI
		cAncho = 6
	CASE PARAMETERS() = 5		
		cAncho = 6
ENDCASE
calias  = ALIAS()
** TIENE QUE VALIDAR CON CALENDARIOS
SELECT itepar
SET ORDER TO ItePar8
SEEK cfiltro+cvalor
if !found() OR !cvariable $'V'
	SET FILTER TO codfte+periodo+codfun+codprg+codsubpr+codproy+codsupry = alltrim(m.codfte)+cfiltro
	GO TOP
	IF !EMPTY(cvalor) AND !EOF()
		SEEK cfiltro+cvalor
		IF !FOUND()
			DO rolea_1
		ENDIF
	ELSE
		DO rolea_1
	ENDIF
ENDIF
cvalor = itepar.codsupry &&+Itepar.CodObra
cdescr = SUBSTR(itepar.dessupry,1, clong )
*-Variables
SET FILTER TO
IF !EMPTY( calias )
	SELECT (calias)
ENDIF
DO CASE
	CASE cvariable==' '   && En edici¢n
		@ ROW(),ccol     SAY PADR(cvalor,cancho,' ')
		@ ROW(),ccol     SAY cdescr
		RETURN .T.
	CASE cvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),ccol     SAY cdescr
		RETURN
	CASE cvariable=='V'   && En vista
		@ ROW(),COL()    SAY PADR(cvalor,cancho,' ')
		RETURN cdescr
	CASE cvariable=='D'   && En vista
		RETURN cdescr
	CASE cvariable=='Z'   && En vista SIN PINTAR
		RETURN cdescr
	CASE cvariable=='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		&cvariable = cvalor
		@ ROW(),ccol+8   SAY cdescr
		RETURN .T.
ENDCASE

PROCEDURE rolea_1
*---------------
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 03,10 TO 20,69 DOUBLE FLOAT SHADOW COLOR SCHEME 10
GO TOP
DO CASE
	CASE !EOF()
		BROWSE WINDOW _xx TITLE ' SUBPROYECTOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codsupry  :H='SubPry' ,;
			codpart   :H='Partida',;
			dessupry  :H='Detalle'
			*codObra   :H='Obra' 
	OTHER
		IF !EMPTY(cvalor)
			DO standby WITH 'No se tiene Suproyecto en referencia'
		ENDIF
ENDCASE
ON KEY LABEL enter
RELEASE WINDOW _xx
IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN



PROCEDURE ANALCALP  && PRESUPUESTO
*----------------
PARAMETERS vCantOC,vtipO,vmues
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=PXrolea_calP(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub), alltrim(vcodact),vcantoc,VTIPO,vmues)
ELSE	
	WDF=PXrolea_calP(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO,vmues)
ENDIF
*USE IN 24
RETURN WDF


PROCEDURE PXrolea_CALP
*-----------------------
parameter xvalor,proact,vtotoc,vtipo,vmues
AS=ALIAS()
Private vdf 
vdf = .t.
ACTIVATE SCREEN
on key label f5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10

RDX = SYS(3)+'.IDX'
SDX = SYS(3)+'.IDX'
TDX = SYS(3)+'.IDX'

********* PRESUPUESTO ****************

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	      ITEHC.CODSUPRY, ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
		  FROM ITEHC ;
		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND ESTADO#'99' AND TIPFUN='I'  AND EMPTY(NUMPA)  and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.); 
				INTO CURSOR Hojax
ELSE
 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR, ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
           FROM ITEHC ;
	 	   WHERE nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,left(CODanal,3)='5.3',.T.) AND ESTADO#'99' AND TIPFUN='F'  AND EMPTY(NUMPA)  and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.); 
					INTO CURSOR HOJAX
ENDIF
vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 19 alias hoja1

SELE HOJA1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (RDX)
Else
   INDEX ON CODSUBPR+CODACT+CODANAL TO (RDX)
endif

******* CALENDARIOS ************

IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ;
		  FROM CALEN ;
		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='I';
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS ;
               FROM CALEN ;
		 	   WHERE nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='F';
					INTO CURSOR CalenX
ENDIF
vInd = SYS(3) + '.DBF'
	SELE CALEN1
	USE
COPY TO (vInd)
use (vind) in 18 alias calen1

SELE CALEN1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (SDX)
Else   
   INDEX ON CODSUBPR+CODACT+CODPART TO (SDX)
endif   

GO TOP
DEACTIVATE WINDOW Standby

**** PROCESO 1:ACUMULA AFECTACIONES ****

IF !EOF()
vTotAfe = 0
SELECT HOJA1
GO TOP
IF ALLTRIM(m.tipfun) = 'I'
   SCAN 
     vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
     vPart2 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
     vTotAfe = 0
     DO WHILE  vPart1 = vPart2
        do case
           case HOJA1.estado='92'
                Sele hojmod
                seek hoja1.nummeshm+hoja1.numhm
                if found()
                   vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                else   
                   vPor = iif(hoja1.tipope='-',-1,1) 
                endif  
                selec hoja1 
*                vPor = iif(HOJA1.tipope='-',-1,1) 
           case HOJA1.estado='94'
                vPor = -1
           case HOJA1.estado='99'
                vPor = 0 
           OTHER
                vPor = 1
        endcase   		                 		                 		                     
        vTotafe = vTotafe + vpor*Valpart
        SKIP
        vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
     ENDD
     SKIP -1
	 sele calen1
	 seek hoja1.CODSUBPR+hoja1.CODPROY+hoja1.CODSUPRY+hoja1.CODPART 
	 if found()
	    if rlock()
           replace totafe with vtotafe
        endif
        unlock
	 endif   
     SELE HOJA1
    ENDSCAN
ELSE  
    SCAN 
	  vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	  vPart2 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	  vTotAfe = 0
	  DO WHILE  vPart1 = vPart2 
	     do case
	        case HOJA1.estado='92'
                 Sele hojmod
                 seek hoja1.nummeshm+hoja1.numhm
                 if found()
                    vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                 else   
                    vPor = iif(hoja1.tipope='-',-1,1) 
                 endif  
                 selec hoja1 
*	             vPor = iif(HOJA1.tipope='-',-1,1) 
	        case HOJA1.estado='94'
	             vPor = -1
	        case HOJA1.estado='99'
	             vPor = 0 
	        OTHER
	             vPor = 1
	     endcase   		                 		                 		                     
	     vTotafe = vTotafe + vpor*Valpart
	     SKIP
	     vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	  ENDD
	  SKIP -1
	  sele calen1
	  SEEK HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.CODANAL
	  if found()
	     repla totafe with vtotafe	
	  endif
	  Unlock   
	  SELE HOJA1
	ENDSCAN
ENDIF   

*SELECT HOJA1
go top
	
SCAN 
    SELE CALEN1
    IF alltrim(m.tipfun) = 'I'
	    SEEK HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART
    ELSE
	    SEEK HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal
    ENDIF
    IF !FOUND()
        sele hoja1
		vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
		vPart2 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
		vTotAfe = 0
		DO WHILE  vPart1 = vPart2 
			do case
			   case hoja1.estado='92'
                    Sele hojmod
                    seek hoja1.nummeshm+hoja1.numhm
                    if found()
                       vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                    else   
                       vPor = iif(hoja1.tipope='-',-1,1) 
                    endif  
                    selec hoja1 
*			         vPor = iif(itehc.tipope='-',-1,1) 
			   case hoja1.estado='94'
	                 vPor = -1
	    	   OTHER
	                 vPor = 1
	        endcase   		                 		                 		                     
		    vTotafe = vTotafe + vpor*hoja1.Valpart
	        SKIP	
			vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
		ENDD
		SKIP -1
	    vtipfun = m.tipfun
		scatter memvar
        sele calen1
        IF F_APPD()
            gather memvar
	        REPLACE totafe with vtotafe        
            if alltrim(m.tipfun)='I'
               REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
            ELSE   
               REPLACE CODPART WITH HOJA1.CODANAL ,valpart with 0
            ENDIF   
	    ENDIF    

	    UNLOCK
	    m.tipfun = vtipfun
        sele hoja1
	 endIF
     sele hoja1 
ENDSCAN

SELECT CALEN1
GO TOP
SCAN
   IF RIGHT(CODPART,2)='00'
      if alltrim(m.tipfun)='F'
     	VPAR = codfun+codprg+CODSUBPR+CODACT+LEFT(CALEN1.CODPART,2)
     	SELE HOJA1
	    SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR codfun+codprg+CODSUBPR+CODACT+LEFT(CODANAL,2) = VPAR				    
	  ELSE
		VPAR = codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) 
        SELE HOJA1					
		SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) = VPAR
	  ENDIF    
	  SELE CALEN1
	  IF RLOCK()
		 REPLACE TOTAFE WITH VSUMA
	  ENDIF
	  UNLOCK
   ENDIF
ENDSCAN	
GO TOP
if vmues
   DO VAL_CAL
endif	
ELSE
    ******  CUANDO NO EXISTE CALENDARIO
	DO standby WITH 'No se tiene Calendario para este Mes'
	vdf =.f.
ENDIF
SELECT Itehc
set order to itehc1
ON KEY LABEL enter
RELEASE WINDOW _yy

RETURN vdf




PROCEDURE ANALCAL
*----------------
IF ALLTRIM(m.tipfun)='F'
	DO rolea_cal WITH maepre.periodo+maepre.codfun+maepre.codprg+maepre.codsubpr               ,maepre.codact
ELSE	
	DO rolea_cal WITH itepar.periodo+itepar.codfun+itepar.codprg+itepar.codsubpr+itepar.codproy,itepar.codsupry
ENDIF
  
RETURN

PROCEDURE ANALCAL1  && ABAS
*----------------
PARAMETERS vCantOC,vtipO,VMUES
SET SAFETY OFF
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=Xrolea_cal(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub)                , alltrim(vcodact),vcantoc,VTIPO)
ELSE	
	WDF=Xrolea_cal(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO)
ENDIF

RETURN WDF

PROCEDURE ANALCAL2   &&CPRESU
*----------------
PARAMETERS vCantOC,vtipO,VMUES
SET SAFETY OFF
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=MXrolea_cal2(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub), alltrim(vcodact),vcantoc,VTIPO)
ELSE
	WDF=MXrolea_cal2(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO)
ENDIF
RETURN WDF

PROCEDURE Xrolea_CAL  && ABAS
*-----------------------
parameter xvalor,proact,vtotoc,vtipo
AS=ALIAS()
Private vdf 
vdf = .t.

ACTIVATE SCREEN
on key label f5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
DEFINE WINDOW _yy FROM 04,01 TO 20,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10

rDX = SYS(3)+'.IDX'
SDX = SYS(3)+'.IDX'
TDX = SYS(3)+'.IDX'

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	      ITEHC.CODSUPRY, ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
		  FROM ITEHC ;
		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND ESTADO#'99' AND TIPFUN='I';
				INTO CURSOR Hojax
ELSE
 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR, ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
           FROM ITEHC ;
	 	   WHERE nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,left(CODanal,3)='5.3',.T.) AND ESTADO#'99' AND TIPFUN='F';
					INTO CURSOR HOJAX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 24 alias hoja1 EXCLU

IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ;
		  FROM CALEN ;
		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1 AND ALLTRIM(M.TIPFUN)='F',CODPART='5.3',IIF(VTIPO=2 AND ALLTRIM(M.TIPFUN)='F',CODPART='5.3',.T.)) AND TIPFUN='I';
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS ;
               FROM CALEN ;
		 	   WHERE nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='F';
					INTO CURSOR CalenX
ENDIF


vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 25 alias calen1 EXCLU


SELE HOJA1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (RDX)
Else
   INDEX ON CODSUBPR+CODACT+CODANAL TO (RDX)
endif

SELE CALEN1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (SDX)
Else   
   INDEX ON CODSUBPR+CODACT+CODPART TO (SDX)
endif

GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	SELECT HOJA1
	GO TOP
	IF ALLTRIM(m.tipfun) = 'I'
          SCAN
	          vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	          vPart2 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	          vTotAfe = 0
	          DO WHILE  vPart1 = vPart2
	              do case
	                 case HOJA1.estado='92'
	                      vPor = iif(HOJA1.tipope='-',-1,1) 
	                 case HOJA1.estado='94'
	                      vPor = -1
	                 case HOJA1.estado='99'
	                      vPor = 0 
	                 OTHER
	                      vPor = 1
	              endcase
	              vTotafe = vTotafe + vpor*Valpart
	              SKIP
	              vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	          ENDD
	          SKIP -1
		  	  sele calen1
		      seek hoja1.CODSUBPR+hoja1.CODPROY+hoja1.CODSUPRY+hoja1.CODPART 
		      if found()
		        if rlock()
                   replace totafe with vtotafe
                endif
                unlock
	          endif   
	          SELE HOJA1
	       ENDSCAN
	ELSE  
 	      SCAN 
		       vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		       vPart2 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		       vTotAfe = 0
		       DO WHILE  vPart1 = vPart2 
		          do case
		             case HOJA1.estado='92'
		                  vPor = iif(HOJA1.tipope='-',-1,1) 
		             case HOJA1.estado='94'
		                  vPor = -1
		             case HOJA1.estado='99'
		                  vPor = 0 
		             OTHER
		                  vPor = 1
		          endcase   		                 		                 		                     
		          vTotafe = vTotafe + vpor*Valpart
		          SKIP
		          vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		       ENDD
		       SKIP -1
			   sele calen1
		       SEEK HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.CODANAL
	           if found()
			      repla totafe with vtotafe
			   endif
			   unlock
		       SELE HOJA1
		    ENDSCAN
	ENDIF   
	go top
	SCAN 
	    SELE CALEN1
	    IF alltrim(m.tipfun) = 'I'
		    SEEK HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART
	    ELSE
		    SEEK HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal
	    ENDIF
	    IF !FOUND()
	        sele hoja1
			vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
			vPart2 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
			vTotAfe = 0
			DO WHILE  vPart1 = vPart2
				do case
				   case hoja1.estado='92'
			         vPor = iif(itehc.tipope='-',-1,1)
				   case hoja1.estado='94'
	                 vPor = -1
	    	       OTHER
	                 vPor = 1
		        endcase
   			    vTotafe = vTotafe + vpor*hoja1.Valpart
		        SKIP	
				vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
			ENDD
			SKIP -1
		    vtipfun = m.tipfun
			scatter memvar
	        sele calen1
	        IF F_APPD()
	            gather memvar
		        REPLACE totafe with vtotafe
	            if alltrim(m.tipfun)='I'
	               REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
	            ELSE   
	               REPLACE CODPART WITH HOJA1.CODANAL ,valpart with 0
	            ENDIF   
		    ENDIF    

		    UNLOCK
		    m.tipfun = vtipfun
	        sele hoja1
	 endif
     sele hoja1 
	endscan
	SELECT CALEN1
    GO TOP
        SCAN
          IF RIGHT(CALEN1.CODPART,2)='00'
             if alltrim(m.tipfun)='F'
           	    	VPAR = codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,3)
           	    	SELE HOJA1
				    SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR codfun+codprg+CODSUBPR+CODACT+LEFT(CODANAL,3) = VPAR
	    		ELSE
					VPAR = codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) 
           	    	SELE HOJA1
					SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) = VPAR
				ENDIF
				SELE CALEN1
				IF RLOCK()
					REPLACE TOTAFE WITH VSUMA
				ENDIF
				UNLOCK
			ENDIF
		ENDSCAN
		GO TOP
		IF VTIPO = 1
			vtotoc = 0
			SELECT Iteoc
			ASW = ORDER()
			VFR = RECNO()
			set relation to periodo+numoc+codfte into orden
		    vOrd = ORDER()
			IF ALLTRIM(m.tipfun) = 'I'
			   vKeyI = ALLTRIM(Calen1.periodo+ Calen1.NumMes+ Calen1.CodFte+ Calen1.codfun+ Calen1.Codprg+ Calen1.Codsubpr+ Calen1.CodProy+ Calen1.CodSuPry) &&+ Calen1.CodPart
			   set orde to iteoc6
			   SET FILT TO ESTADO#'99'
			   seek vkeyI

			   IF FOUND()
	   	          SCAN
			          vPart1 = ALLTRIM(ITEOC.CODCAL)+IteOC.CodPart
			          vPart2 = ALLTRIM(ITEOC.CODCAL)+IteOC.CodPart
			          vTotAfe = 0
			          vtotoc = 0
			          DO WHILE  vPart1 = vPart2
			              vTotOC = vTotOC + ValTOT
			              SKIP
			              vPart1 = ALLTRIM(ITEOC.CODCAL)+IteOC.CodPart
		    	      ENDD
		        	  SKIP -1
			          sele calen1
					  seek substr(orden.codcal,13,4)+substr(orden.codcal,17,3)+substr(orden.codcal,20,2)+iteoc.codpart
					  if found()
					     if rlock()
			                replace totOC with vtotOC
			             endif
		    	         unlock
			          endif
			          vtotoc=0   
			          SELE ITEOc
			        ENDSCAN
		    	ENDIF
			ELSE   
			   vKeyF = ALLTRIM(Calen1.periodo+ Calen1.NumMes+ Calen1.CodFte+ Calen1.Codfun+ Calen1.Codprg+ Calen1.Codsubpr+Calen1.CodAct) &&+Calen1.CodPart
			   set orde to iteOc7
		       seek vkeyF
 			   IF FOUND()
 					SCAN
			          vPart1 = ALLTRIM(ITEOC.CODCAL)+IteoC.Codanal
			          vPart2 = ALLTRIM(ITEOC.CODCAL)+IteoC.Codanal
			          vTotOc = 0
			          DO WHILE  vPart1 = vPart2
			              vTotOC = vTotOC + Valtot
			              SKIP
			              vPart1 = ALLTRIM(ITEOC.CODCAL)+Iteoc.Codanal
			          ENDD
			          SKIP -1
			          sele calen1
					  seek substr(orden.codcal,13,4)+substr(orden.codcal,17,2)+iteoc.CodAnal
					  if found()
					     repla totoc with vTotOc
					  endif
					  unlock   
					  vtotoc = 0
			          SELE IteOc
			        ENDSCAN
 			   ENDIF
 			ENDIF
			SET ORDER TO (vord)

			SELECT CALEN1
			SET FILTER TO
		    GO TOP
	        SCAN
    	      IF RIGHT(CALEN1.CODPART,2)='00'
        	     if alltrim(m.tipfun)='F'
           	    	VPAR = codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,3)
           	    	vger = recno()
				    SUM TOTOC TO VSUMA FOR codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,3) = VPAR 
	    		 ELSE
					VPAR = codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3)
					vger = recno()
					SUM TOTOC TO VSUMA FOR codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) = VPAR
				 ENDIF
				 SELE CALEN1
				 go vger
				 IF RLOCK()
					REPLACE TOTOC WITH VSUMA
				 ENDIF
				 UNLOCK
			 ENDIF
		  ENDSCAN
		  GO TOP
			SELECT Calen1
			DEACTIVATE WINDOW Standby
			BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®Enter¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		        XX=VAL_PARA(nummes,'FECMES','D') :H='Mes':3 ,;
       	        FRO=IIF(ALLTRIM(M.TIPFUN)='I',CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' '),CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' ')) : H=IIF(ALLTRIM(M.TIPFUN)='I','SubPry','Actv')  ,;
				vv=Codpart  :H='Partd' ,;
				Valpart  :H='     Asignado':P='99,999,999.99',;
				Totafe             :H='     Afectado':P='99,999,999.99',;
				TotOc              :H='     Con O/C ':P='99,999,999.99',;
				zz=valpart- TotAFE :H='      Saldo' :P='9,999,999.99'
		        *XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':3 
	   			m.Nummes = Calen1.Nummes
				m.CodPart= Calen1.Codpart
		        vPart = Calen1.CodPart
		  SELE ITEOC
		  SET RELA TO
		  SET ORDE TO ASW
		  GO TOP
		  IF !BOF()
		  	GO VFR
		  ENDIF
**************
	    else
			vtotoS = 0
			SELECT ORDSE
		    vOrd = ORDER()
			IF alltrim(m.tipfun) = 'I'
			   vKeyI = ALLTRIM(Calen1.periodo+ Calen1.NumMes+ Calen1.CodFte+Calen1.Codfun+ Calen1.Codprg+ Calen1.Codsubpr+Calen1.CodProy+ Calen1.CodSuPry) &&+ Calen1.CodPart
			   SET FILT TO !EMPTY(ORDSE.codPART) AND ESTADO#'99'
			   set orde to ordser4
			   GO TOP
			   seek vkeyI	
			   IF FOUND()
     			  SCAN while ESTADO#'99' &&ALLTRIM(ORDSE.CODCAL) = vKeyI 
			          vPart1 = ALLTRIM(ORDSE.CodCal)+ORDSE.CodPart
			          vPart2 = ALLTRIM(ORDSE.CodCal)+ORDSE.CodPart
			          vTotAfe = 0
			          vtotos = 0	
			          DO WHILE  vPart1 = vPart2
			              vTotOS = vTotOS + ValTOT
			              IF !EOF()
			                 SKIP
			              ELSE
			                 EXIT   
			              ENDIF   
			              vPart1 = ALLTRIM(ORDSE.CodCal)+ORDSE.CodPart
		    	      ENDD
		        	  SKIP -1
			          sele calen1
					  seek substr(ordse.codcal,13,4)+substr(ordse.codcal,17,3)+substr(ordse.codcal,20,2)+ordse.codpart
					  if found()
					     if rlock()
			                replace totOs with vtotOs
			             endif
		    	         unlock
			          endif
			          vtotos = 0
			          SELE ORDSE
			        ENDSCAN
		    	ENDIF
			ELSE
			   vKeyF = ALLTRIM(Calen1.periodo+ Calen1.NumMes+ Calen1.CodFte+ Calen1.Codfun+ Calen1.Codprg+ Calen1.Codsubpr+ Calen1.CodAct)  &&+ORDSE.CodPart
               SET FILT TO !EMPTY(ORDSE.codanal) AND ESTADO#'99'
			   set orde to ordser5
			   GO TOP
		       seek vkeyF
			   IF FOUND()
  			    	scan while ESTADO#'99'  &&ALLTRIM(ORDSE.Codcal) = vKeyF
			          vPart1 = ALLTRIM(ORDSE.CodCal)+ORDSE.Codanal
			          vPart2 = ALLTRIM(ORDSE.CodCal)+ORDSE.Codanal
			          vTotAfe = 0
			          vtotos = 0	
			          DO WHILE  vPart1 = vPart2  
			              vTotOS = vTotOS + VALTOT
			              IF !EOF()
			                 SKIP
			              ELSE
			                 EXIT   
			              ENDIF   
			              vPart1 = ALLTRIM(ORDSE.CodCal)+ORDSE.Codanal
			          ENDD
			          SKIP -1
   			          sele calen1
					  seek substr(ORDSE.codcal,13,4)+substr(ORDSE.codcal,17,2)+ORDSE.CodAnal
					  if found()
					     repla totos with vTotOs
					  endif
					  unlock   
 			          vtotos = 0
			          SELE ORDSE
			        ENDSCAN
			   ENDIF
			ENDIF
			SET FILT TO
			SET ORDER TO (vord)

			SELECT CALEN1
		    GO TOP
	        SCAN
    	      IF RIGHT(CALEN1.CODPART,2)='00'
        	     if alltrim(m.tipfun)='F'
           	    	VPAR = codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,3)
           	    	vger = recno()
				    SUM TOTOS TO VSUMA FOR codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,3) = VPAR
	    		 ELSE
					VPAR = codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) 
					vger = recno()
					SUM TOTOS TO VSUMA FOR codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) = VPAR
				 ENDIF
				 SELE CALEN1
				 go vger
				 IF RLOCK()
					REPLACE TOTOS WITH VSUMA
				 ENDIF
				 UNLOCK
			 ENDIF
		  ENDSCAN
		  GO TOP

			SELECT Calen1
			DEACTIVATE WINDOW Standby
            VNUMES =VAL_PARA(nummes,'FECMES','D')+'>'
			BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®Enter¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		        DD=VNUMES :H='Mes':3 ,;
       	        FRO=IIF(ALLTRIM(M.TIPFUN)='I',CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' '),CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' ')) : H=IIF(ALLTRIM(M.TIPFUN)='I','SubPry','Actv')  ,;
				vv=Codpart  :H='Partd' ,;
				Valpart  :H='     Asignado':P='99,999,999.99',;
				Totafe             :H='     Afectado':P='99,999,999.99',;
				TotOs              :H='     Con O/S ':P='99,999,999.99',;
				zz=valpart- Totafe :H='      Saldo' :P='9,999,999.99'
	   			m.Nummes = Calen1.Nummes
	   			if alltrim(m.tipfun)='I'
					m.CodPart = Calen1.Codpart
					m.codAnal = '      '
				else	
     				m.codanal = Calen1.Codpart
     				m.codpart = '      '
     			endif	
		        vPart = Calen1.CodPart
	    endif
***		
*       if vmues
*			DO VAL_CAL
*	endif	
ELSE
	    ******  CUANDO NO EXISTE CALENDARIO
		DO standby WITH 'No se tiene Calendario para este Mes'
		vdf =.f.
ENDIF
SELECT Itehc
set order to itehc1
ON KEY LABEL enter
RELEASE WINDOW _yy

RETURN vdf

PROCEDURE MXrolea_CAL2
*-----------------------
parameter xvalor,proact,vtotoc,vtipo,vmues
AS=ALIAS()
Private vdf 
vdf = .t.

RDX = SYS(3)+'.IDX'
SDX = SYS(3)+'.IDX'
TDX = SYS(3)+'.IDX'

ACTIVATE SCREEN
on key label f5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10

SELE ITEHC

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado, itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	      ITEHC.CODSUPRY, ITEHC.CODFUN, ITEHC.CODPRG, ITEHC.CODSUBPR, ITEHC.Codfte, ITEHC.Tipfun, ITEHC.TOTCAL ;
		  FROM ITEHC ;
		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',iif(VTIPO=4,left(CODPART,3)='5.1',.T.)) AND ESTADO#'99' AND TIPFUN='I' AND EMPTY(NUMPA); 
				INTO CURSOR Hojax
ELSE
 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR,ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
           FROM ITEHC ;
	 	   WHERE nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,left(CODanal,3)='5.3',iif(VTIPO=4,left(CODPART,3)='5.1',.T.)) AND ESTADO#'99' AND TIPFUN='F' AND EMPTY(NUMPA);
					INTO CURSOR HOJAX
ENDIF

*AND iif(VTIPO=1,left(CODanal,3)='5.3',.T.)
vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 24 alias hoja1 exclu
IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ;
		  FROM CALEN ;
		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(empty(vProyec),codproy,'')  ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor            ;
				INTO CURSOR CalenX

		  SET FILTER TO IIF(!empty(proact),codsupry,'')=iif(!empty(proact),proact,'') AND TIPFUN='I' AND periodo+codfun+codprg+codsubpr+codproy=xvalor
		  
*		  WHERE  nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
* 				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',iif(VTIPO=4,left(CODPART,3)='5.1',.T.)) AND TIPFUN='I' ;
*				INTO CURSOR CalenX


ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS ;
               FROM CALEN ;
		 	   WHERE nummes+tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),CoDact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',iif(VTIPO=4,left(CODPART,3)='5.1',.T.)) AND TIPFUN='F';
					INTO CURSOR CalenX
ENDIF

* WHERE ..AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='F'

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 25 alias calen1 exclu

SELE HOJA1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (RDX)
Else
   INDEX ON CODSUBPR+CODACT+CODANAL TO (RDX)
endif

SELE CALEN1
REPLA ALL TOTAFE WITH 0
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (SDX)
Else   
   INDEX ON CODSUBPR+CODACT+CODPART TO (SDX)
endif

GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	SELECT HOJA1
	IF ALLTRIM(m.tipfun) = 'I'
       GO TOP
       SCAN 
	     vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	     vPart2 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
         vTotAfe = 0
         DO WHILE  vPart1 = vPart2
            do case
               case HOJA1.estado='92'
                    vPor = iif(HOJA1.tipope='-',-1,1) 
               case HOJA1.estado='94'
                    vPor = -1
               case HOJA1.estado='99'
                    vPor = 0 
               OTHER
                    vPor = 1
            endcase   		                 		                 		                     
	        vTotafe = vTotafe + vpor*Valpart
	        SKIP
	        vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      ENDD
	      SKIP -1
		  sele calen1
		  seek hoja1.CODSUBPR+hoja1.CODPROY+hoja1.CODSUPRY+hoja1.CODPART 
		  if found()
		     if rlock()
                replace totafe with vtotafe
             endif
             unlock
	      endif   
	      SELE HOJA1
       ENDSCAN
	ELSE  
	   GO TOP
       SCAN 
	        vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		    vPart2 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		    vTotAfe = 0
		    DO WHILE  vPart1 = vPart2 
		      do case
		         case HOJA1.estado='92'
		              vPor = iif(HOJA1.tipope='-',-1,1) 
		         case HOJA1.estado='94'
		              vPor = -1
		         case HOJA1.estado='99'
		              vPor = 0 
		         OTHER
		             vPor = 1
		      endcase   		                 		                 		                     
		      vTotafe = vTotafe + vpor*Valpart
		      SKIP
		      vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		    ENDD
		    SKIP -1
		    sele calen1
		    SEEK HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.CODANAL
	        if found()
			   repla totafe with vtotafe
			endif
			unlock   
		    SELE HOJA1
		 ENDSCAN
	ENDIF   
    SELECT Hoja1
*	if alltrim(m.tipfun)='I'
*	   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (TDX)
*	Else   
*	   INDEX ON CODSUBPR+CODACT+CODanal TO (TDX)
*	endif   
	go top
	
	SCAN 
	    SELE CALEN1
	    IF alltrim(m.tipfun) = 'I'
		    SEEK HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART
	    ELSE
		    SEEK HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal
	    ENDIF
	    IF !FOUND()
	        sele hoja1
			vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
			vPart2 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
			vTotAfe = 0
			DO WHILE  vPart1 = vPart2 
				do case
				   case hoja1.estado='92'
			         vPor = iif(HOJA1.tipope='-',-1,1) 
				   case hoja1.estado='94'
	                 vPor = -1
	    	       OTHER
	                 vPor = 1
		        endcase   		                 		                 		                     
   			    vTotafe = vTotafe + vpor*hoja1.Valpart
		        SKIP
				vPart1 = iif(alltrim(m.tipfun)='I',HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+Hoja1.CODPART,HOJA1.CODSUBPR+HOJA1.CODACT+Hoja1.Codanal)
			ENDD
			SKIP -1
		    vtipfun = m.tipfun
			scatter memvar
	        sele calen1
	        IF F_APPD()
	            gather memvar
		        REPLACE totafe with vtotafe
	            if alltrim(m.tipfun)='I'
	               REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
	            ELSE
	               REPLACE CODPART WITH HOJA1.CODANAL ,valpart with 0
	            ENDIF   
		    ENDIF    

		    UNLOCK
		    m.tipfun = vtipfun
	        sele hoja1
	 endif
     sele hoja1 
	endscan
	SELECT CALEN1
    GO TOP
        SCAN
          IF RIGHT(CALEN1.CODPART,2)='00'
             if alltrim(m.tipfun)='F'
           	    	VPAR = codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,3)
           	    	SELE HOJA1
				    SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR codfun+codprg+CODSUBPR+CODACT+LEFT(CODANAL,3) = VPAR
	    		ELSE
					VPAR = codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) 
           	    	SELE HOJA1
					SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,3) = VPAR
				ENDIF    
				SELE CALEN1
				IF RLOCK()
					REPLACE TOTAFE WITH VSUMA
				ENDIF
				UNLOCK
			ENDIF
		ENDSCAN	
		GO TOP
        if vmues
			DO VAL_CAL
		endif	
ELSE
	    ******  CUANDO NO EXISTE CALENDARIO
		DO standby WITH 'No se tiene Calendario para este Mes'
		vdf =.f.
ENDIF
SELECT Itehc
set order to itehc1
ON KEY LABEL enter
RELEASE WINDOW _yy

RETURN vdf





PROCEDURE rolea_CAL
*------------------
parameter xvalor,proact
IF YESNO(' ¨ Valida Calendario? ')
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _yy FROM 04,09 TO 20,71 DOUBLE FLOAT SHADOW COLOR SCHEME 10
SELE CALEN
SET ORDER TO IIF(ALLTRIM(m.tipfun)='I',6,5)
IF ALLTRIM(m.tipfun)='I'
   SET FILTER TO nummes+tipfun+codfte+periodo+codfun+codprg+codsubpr + codproy + iif(!empty(proact),codsupry,' ')  = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')  
ELSE  
   SET FILTER TO nummes+tipfun+codfte+periodo+codfun+codprg+codsubpr + iif(!empty(proact),codact,' ') = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')  AND LEFT(codpart,2)=iif(alias()="ORDSE",'03','02')
ENDIF   
GO TOP
IF !EOF()
	BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS; 
        XX=VAL_PARA(nummes,'FECMES','D')  :H='Mes':10 ,;
		vv=' '+Codpart  :H='Partida' ,;
		Valpart  :H='     Asignado':P='99,999,999.99',;
		Totafe   :H='     Afectado':P='99,999,999.99',;
		zz=valpart-Totafe :H='       Saldo':P='99,999,999.99'
   		m.Nummes = Calen.Nummes
		m.CodPart= Calen.Codpart
        vPart = Calen.CodPart
ELSE
	DO standby WITH 'No se tiene Calendario para este Mes'
	IF ALLTRIM(m.tipfun)='I'
	   SET FILTER TO tipfun+codfte+periodo+codfun+codprg+codsubpr + codproy + iif(!empty(proact),codsupry,' ')  = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')  
	ELSE  
	   SET FILTER TO tipfun+codfte+periodo+codfun+codprg+codsubpr + iif(!empty(proact),codact,' ') =  alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')  AND LEFT(codpart,2)=iif(alias()="ORDSE",'03','02')
	ENDIF   
    GO TOP
	BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
        XX=VAL_PARA(nummes,'FECMES','D')  :H='Mes':10 ,;
		vv=' '+Codpart  :H='Partida' ,;
		Valpart  :H='     Asignado':P='99,999,999.99',;
		Totafe   :H='     Afectado':P='99,999,999.99',;
		zz=valpart-Totafe :H='       Saldo':P='99,999,999.99'
		m.Nummes = Calen.Nummes
		m.CodPart= Calen.Codpart
        vPart = Calen.CodPart
ENDIF
ON KEY LABEL enter
RELEASE WINDOW _yy
IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF
endif
RETURN

FUNCTION val_part    && VALIDA PARTIDAS
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
ENDCASE
malias  = ALIAS()
SELECT clase
SEEK filtro+mvalor
IF !FOUND() AND !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(filtro)
		SET FILTER TO catego+codpart+modali = filtro
	ENDIF
	GO TOP
	IF EOF()
		DO standby WITH 'No existen partidas definidas'
		SET FILTER TO
		SELE (malias)
		**RETURN &&.f.
		RETURN .F.
		
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD CATEGO+'.'+ALLTRIM(codpart)+'.'+MODALI+'.'+ALLTRIM(codsubp)+' '+SUBSTR(despar,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = clase.catego +'.'+clase.codpart +'.'+clase.modali +'.'+ clase.codsubp
mdescr = SUBSTR( clase.despar, 1, mlong )
***Usado em OrdSer
IF m.tipfun="I"
	m.codpart = mvalor
ELSE
	m.codanal = mvalor
ENDIF
*SET ORDE TO 1
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL   SAY mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	CASE mvariable=='X'   && Solo valor
		RETURN mvalor
	CASE mvariable=='T'
		&mvariable = mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN  mvalor
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
ENDCASE
RETURN

FUNCTION val_part1    && VALIDA PARTIDAS
*----------------
PARAMETERS mvalor, mvalor1, filtro, mvariable, MCOL, mlong

PRIVATE malias
DO CASE
	CASE PARAMETERS() = 3
		MCOL = 0
		mvariable = ' '
		mlong = 40
	CASE PARAMETERS() = 4
		MCOL = 0
		mlong = 40
	CASE PARAMETERS() = 5
		mlong = 40               && Longitud campo DESCRI
ENDCASE
malias  = ALIAS()
SELECT clase
GO TOP
SEEK filtro+mvalor+mvalor1

IF !FOUND() AND !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(filtro)
		SET FILTER TO CATEGO = filtro
	ENDIF
	GO TOP
	IF EOF()
		DO standby WITH 'No existen partidas definidas'
		SET FILTER TO
		SELE (malias)
		**RETURN &&.f.
		RETURN .F.
		
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CATEGO)+'.'+ALLTRIM(codpart)+'.'+ALLTRIM(MODALI)+'.'+ALLTRIM(codsubp)+' '+SUBSTR(despar,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
SET FILTER TO
mvalor = clase.catego +'.'+ clase.codpart +'.'+ clase.modali +'.'+ clase.codsubp
mdescr = SUBSTR( clase.despar, 1, mlong )
***Usado em OrdSer
IF m.tipfun="I"
	m.codpart = mvalor
ELSE
	m.codanal = mvalor
ENDIF
*SET ORDE TO 1
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL   SAY mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	CASE mvariable=='X'   && Solo valor
		RETURN mvalor
	CASE mvariable=='T'
		&mvariable = mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN  mvalor
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
ENDCASE
RETURN


FUNCTION val_part2    && VALIDA PARTIDAS
*----------------
PARAMETERS mvalor, mvariable, MCOL, mlong
PRIVATE malias,mOrdClas
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mlong = 40
	CASE PARAMETERS() = 3
		mlong = 40
ENDCASE
malias  = ALIAS()
SELECT clase
mOrdClas = ORDER()
SET ORDER TO CLASE3
SEEK mvalor
IF !FOUND() AND !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	GO TOP
	IF EOF()
		DO standby WITH 'No existen partidas definidas'
		SET FILTER TO
		SET ORDER TO &mOrdClas
		SELE (malias)
		RETURN .F.
	ENDIF
	
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD CodPart1+' '+SUBSTR(despar,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = Clase.CodPart1



mdescr = ALLTRIM(clase.despar)
*mdescr = SUBSTR( clase.despar, 1, mlong )
***Usado em OrdSer
IF m.tipfun="I"
	m.codpart = mvalor
ELSE
	m.codanal = mvalor
ENDIF
SET ORDE TO &mOrdClas
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL   SAY mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	CASE mvariable=='X'   && Solo valor
		RETURN mvalor
	CASE mvariable=='T'
		&mvariable = mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN  mvalor
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
ENDCASE
RETURN



FUNCTION val_prv   && VALIDA PROVEEDORES
*---------------
PARAMETER xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo
medita = (PARAMETERS()>=2)
mmsg   = (PARAMETERS()=4) .AND.  _tipo
xcod   =ALLTRIM(xcod)
_campo = VARREAD()

malias = ALIAS()
SELECT promae
_oldwnd = WOUTPUT()

IF !medita
	SEEK xcod
	v_fun = IIF(FOUND(),nompro,"")
ELSE
	IF EMPTY(xcod)
		SET ORDE TO 2
		ON KEY LABEL enter KEYBOARD CHR(23)
		ON KEY LABEL f2  DO prvbus
		*ON KEY LABEL f3  DO promaex
		DEFINE WINDOW _xx FROM 3,3 TO 22,77
		BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona    ®F2¯ Busca  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codprv   :H='C¢digo'     ,;
			nompro   :H='Nombre':45  ,;
			dirpro   :H='Direccci¢n' :25
		ON KEY LABEL enter
		ON KEY LABEL f2
		*ON KEY LABEL f3
		RELEASE WINDOW _xx
		SET ORDER TO 1
		IF !EMPTY(_oldwnd)
			ACTIVATE WINDOW &_oldwnd
		ENDIF

		IF LASTKEY()=27
			v_fun = .F.
		ELSE
			xcod = codprv
			IF mmsg
				@ _x,_y SAY nompro
			ENDIF
			SELECT (malias)
			IF !_tipo
				REPLACE &_campo WITH  xcod
			ENDIF
			v_fun = .T.
		ENDIF
	ELSE
		SEEK xcod
		IF mmsg .AND. FOUND()
			@ _x,_y SAY nompro
		ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF
SELECT (malias)
RETURN v_fun

FUNCTION val_art    && Valida Articulos
*---------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo
medita = (PARAMETERS()>=2)
mmsg   = (PARAMETERS()=4) .AND.  _tipo

_campo = VARREAD()

malias = ALIAS()
SELECT PRODU
_oldwnd = WOUTPUT()
IF !medita
	SEEK xcod
	v_fun = IIF(FOUND(),descri,"")
ELSE
	IF EMPTY(xcod)
		SET ORDER TO ARTMAE1
		SET FILTER TO SUBSTR(codart,2,2)=SUBSTR(ALLTRIM(parma.codigo),2,2)
		GO TOP
		IF EOF()
			SET FILTER TO
			v_fun = .F.
		ELSE
			ON KEY LABEL F10 KEYBOARD CHR(23)
			ON KEY LABEL f2 DO funbus
			DEFINE WINDOW _busart FROM 2,02 TO 22,77 color scheme 10
			SELECT PRODU
			SET ORDER TO ARTMAE1
			GO TOP
			*QQ
			BROWSE WINDOW _busart TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
				codart   :H='C¢digo'    :r,;
				descri   :H='Nombre':60 :r
			RELEASE WINDOW _busart
			ON KEY LABEL F10
			ON KEY LABEL f2
			SELECT produ
			SET FILTER TO
			SET ORDE TO ARTMAE1
			IF !EMPTY(_oldwnd)
				*ACTIVATE WINDOW &_OldWnd
			ENDIF

			IF LASTKEY()=27
				v_fun = .F.
			ELSE
				xcod = codart
				IF mmsg
					@ _x,_y SAY descri
				ENDIF
				SELECT (malias)
				IF !_tipo
					REPLACE &_campo WITH  xcod
				ENDIF
				v_fun = .T.
			ENDIF
		ENDIF
	ELSE
		SEEK xcod
		IF mmsg .AND. FOUND()
			@ _x,_y SAY descri
		ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF
SELECT (malias)
RETURN v_fun

PROCEDURE AGRART
*---------------
vCodart = LEFT(Produ.Codart,4)
IF F_appd()
  REPLACE Codart WITH vCodArt ,fecreg with date()
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE BORART
*---------------
if rlock()
   IF YESNO('Desea borrar esta Clasificaci¢n?')
      DELETE NEXT 1
   endif
else
   do standby with 'No puede eliminar este Item.'
endif
return .t.



FUNCTION val_artdet  && valida detalle Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo
medita = (PARAMETERS()>=2)
mmsg   = (PARAMETERS()=4) .AND.  _tipo

_campo = VARREAD()

malias = ALIAS()
SELECT iteart
_oldwnd = WOUTPUT()

IF !medita
	SEEK xcod
	v_fun = IIF(FOUND(),descri,"")
ELSE
	IF !EMPTY(xcod)
		SET ORDER TO 1
		SEEK 'B'+xcod
		IF !FOUND()
			IF !yesno('Desea registrar un producto?')
				UNLOCK ALL
				SELECT (malias)
				v_fun = .F.
				RETURN v_fun
			ELSE
				DO agredet
			ENDIF
		ENDIF
		ACTIVATE SCREEN
		ON KEY LABEL f10 KEYBOARD CHR(23)
		ON KEY LABEL f8 DO borrdet
		ON KEY LABEL f5 DO agredet
		ON KEY LABEL f2 DO funbusdet
		DEFINE WINDOW _busart FROM 2,02 TO 22,77
		BROWSE WINDOW _busart TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU KEY 'B'+xcod FIELDS;
			codart   :H='C¢digo'     :W=EMPTY(SUBSTR(codart,8,3)),;
			descri   :H='Nombre':70  :W=SUBSTR(CODART,7,1)#'.' &&EMPTY(descri) ,;
			coduni   :H='Unidad':7   :W=SUBSTR(CODART,7,1)#'.' &&EMPTY(coduni)
		*       CodPart  :H='Partd':5  :w=empty(CodPart)
		ON KEY LABEL f10
		ON KEY LABEL f8
		ON KEY LABEL f5
		ON KEY LABEL f2
		RELEASE WINDOW _busart
		SET ORDER TO 1
		IF !EMPTY(_oldwnd)
			*        ACTIVATE WINDOW &_OldWnd
		ENDIF
		IF LASTKEY()=27
			v_fun = .F.
		ELSE
			xcod = codart
			IF mmsg
				@ _x,_y SAY descri
			ENDIF
			SELECT (malias)
			IF !_tipo
				REPLACE &_campo WITH  xcod
			ENDIF
			v_fun = .T.
		ENDIF
	ENDIF
ENDIF
SELECT (malias)
UNLOCK ALL
RETURN v_fun


PROCEDURE AGRedet
*---------------
vCodart = SUBSTR(Produ.Codart,2,6)
vTipart = SUBSTR(Produ.Codart,1,1)
IF F_appd()
  REPLACE Codart  WITH vCodArt+'.',TipArt with vTipArt ,Fecreg with date()
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE BORrdet
*---------------
if rlock()
   IF YESNO('Desea borrar esta Clasificaci¢n?')
      DELETE NEXT 1
   endif
else
   do standby with 'No puede eliminar este Item.'
endif
return .t.


FUNCTION Val_paraC
*-----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLon
  PRIVATE AuxLg

  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = 'V'
      mLon  = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLon = 40
    CASE PARAMETERS() = 4
      mLon = 40
  ENDCASE

  AuxLg = LEN(mValor)
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor
  IF .NOT. FOUND() .AND. mVariable<>'V' .AND. mVariable<>'A'
    SET FILTER TO Tipo = Filtro
    GO TOP
    IF EOF()
       DO standby WITH "NO HAY OPCIONES A ELEGIR"
       SET FILTER TO
       SELECT (mAlias)
       RETURN .F.
    ENDIF

    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro

    IF !EMPTY(_OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
  ENDIF
  mValor = LEFT(Parma.Codigo,AuxLg)
  mDescr = PADR( Parma.Descri, mlon, ' ' )
  SET FILTER TO
  SELECT (mAlias)
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol SAY mDescr
      RETURN .T.
    CASE mVariable='V'   && En vista
      RETURN mDescr
    CASE mVariable='A'   && En asignaci¢n
      RETURN mDescr
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE

FUNCTION Val_Cale
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
  ENDCASE
  mAlias  = ALIAS()
  SELECT Parma
  SEEK Filtro+mValor

  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      SET FILTER TO Tipo+Codigo = Filtro
      GO TOP
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CODIGOAUX)+' '+SUBSTR(Descri,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = Parma.CodigoAux
 mDescr = SUBSTR( Parma.Descri, 1, mLong )
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol   SAY mValor
      @ ROW(),mCol+6 SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE

FUNCTION Val_Proy
*----------------
PARAMETERS mValor, Filtro, mVariable, mCol, mLong
PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
  ENDCASE
  mAlias  = ALIAS()
  SELECT Calen
  vTipFun=m.tipfun
  vTempo =left(m.codcal,4)
  if alltrim(vtipfun)='I'
     set orde to 1
  else
     set orde to 2
  endif

  Public vSale
  vSale=.t.
  GO TOP
  SEEK vtempo+filtro+ALLTRIM(mValor)

 IF (!FOUND() AND !mVariable $'VZ') &&OR empty(mvalor)
     SET FILTER TO codfte+codfun+codprg+codsubpr+Tipfun = Filtro + ALLTRIM(vtipfun)
     GO TOP
     IF EOF()
        DO STANDBY WITH 'No se tiene calendario para este Programa'
        SET FILTER TO
        IF !EMPTY( mAlias )
           SELECT (mAlias)
        ENDIF
        vSale=.f.
        RETURN
     ELSE
       _OldWnd = WOUTPUT()
       ACTIVATE SCREEN
       ON KEY LABEL ENTER KEYBOARD CHR(23)
       DEFINE WINDOW _xx FROM 2,01 TO 22,78 DOUBLE FLOAT SHADOW COLOR SCHEME 1
       DO CASE
          CASE alltrim(vtipfun) = 'I'
             BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
                 XX=VAL_PARA(nummes,'FECMES','D')  :H='Mes':3 ,;
                 CodProy  :H='Pry' ,;
                 Codsupry :H='Sub',;
                 Codobra  :H='Ob' ,;
                 CodPart  :H='Partida',;
                 Totafe   :H='Total Afectado' :P='99,999,999.99' ,;
                 ZZ=Valpart - Totafe :H='Saldo Calendario' :P='99,999,999.99' ,;
                 Descri   :H='Nombre Proyecto':50
         CASE alltrim(vtipfun) = 'F' AND !empty(CodAct)
             BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
                 XX=VAL_PARA(nummes,'FECMES','D')  :H='Mes':3 ,;
                 Codact   :H='Act',;
                 CodPart  :H='Partida',;
                 Totafe   :H='Total Afectado' :P='99,999,999.99' ,;
                 ZZ=valpart - Totafe :H='Saldo Calendario' :P='99,999,999.99' ,;
                 Descri   :H='Nombre Actividad':50
       ENDCASE
       ON KEY LABEL ENTER
       RELEASE WINDOW _xx
       IF !EMPTY(_OldWnd)
          ACTIVATE WINDOW &_OldWnd
       ENDIF
     ENDIF
     SET FILTER TO
  ENDIF
  mValor = IIf(alltrim(vtipfun)='I',calen.codproy + calen.codsupry + calen.codobra,calen.codact)
  mas = IIf(alltrim(vtipfun)='I',8,3)
 *mas = mdist
  mDescr = SUBSTR( calen.Descri, 1, mLong )
  vMes = calen.Nummes
  vPart = calen.Codpart
  m.Codpart = calen.Codpart
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
       CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+mas SAY mDescr
       RETURN .T.
       CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
       @ ROW(),mCol SAY mDescr
      *RETURN .T.
       RETURN mDescr
       CASE mVariable='V'   && En vista
       @ ROW(),COL() SAY mValor
       RETURN ' '+mDescr
       CASE mVariable='D'   && En vista
       RETURN mDescr
       CASE mVariable='Z'   && En vista SIN PINTAR
       RETURN mDescr
       CASE mVariable='C'   && Solo codigo
       RETURN .T.
       OTHERWISE            && En browse de edici¢n
       REPLACE &mVariable WITH mValor
       RETURN .T.
  ENDCASE


  
FUNCTION val_para5
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias

DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
LOCATE FOR TIPO=filtro AND ALLTRIM(CODIGOAUX)=ALLTRIM(mvalor)
IF !FOUND() .AND. !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF

mvalor = parma.codigo
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE


FUNCTION val_para7
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong , mdist
PRIVATE malias

DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
		mdist = 6
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
		mdist = 6
	CASE PARAMETERS() = 5
		mdist = 6
ENDCASE
malias  = ALIAS()

SELECT parma
LOCATE FOR TIPO=filtro AND ALLTRIM(CODIGOAUX)=ALLTRIM(mvalor)
IF !FOUND() .AND. !mvariable $'VZ' .OR. EMPTY(mvalor)
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	SET FILTER TO tipo = filtro
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		SET FILTER TO
		IF !EMPTY( malias )
			SELECT (malias)
		ENDIF
		RETURN
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(descri,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF

mvalor = parma.codigoaux
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable=' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable='D'   && En vista
		RETURN mdescr
	CASE mvariable='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
		RETURN .T.
ENDCASE



FUNCTION verest
*--------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
	CASE vest='VG'
		vfun ='Vigente     '
	CASE vest='RG'
		vfun ='Regularizar '
	CASE vest='PD'
		vfun ='Pendiente   '
	CASE vest='AN'
		vfun ='Anulado     '
ENDCASE
RETURN vfun

FUNCTION vEstOc
*--------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
	CASE vest='00'
		vfun ='Emitido      '
	CASE vest='20'
		vfun ='Afectado     '
	CASE vest='40'
		vfun ='Atendido     '
	CASE vest='50'
		vfun ='Liquidado    '
	CASE vest='51'
		vfun ='Contabilizado'		
	CASE vest='90'
		vfun ='Con Pte A/M  '		
	CASE vest='99'
		vfun ='Anulado      '
ENDCASE
RETURN vfun

FUNCTION vEstPec
*---------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
	CASE vest='00'
		vfun ='Emitido      '
	CASE vest='30'
		vfun ='Con O/C      '
	CASE vest='40'
		vfun ='Atendido     '
	CASE vest='50'
		vfun ='Liquidado    '
	CASE vest='51'
		vfun ='Contabilizado'		
	CASE vest='99'
		vfun ='Anulado      '
ENDCASE
RETURN vfun
FUNCTION VerEsthc
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00' or vEst='  '
      vFun ='Emitido     '
   CASE vEst='5'
      vFun ='Pagado      '
   CASE vEst='90'
      vFun ='Con H/Modifc'
   CASE vEst='99'
      vFun ='Anulado     '
   ENDCASE
RETURN vFun

FUNCTION VerEsthm
*----------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00' or vEst='  '
      vFun ='Emitido     '
   CASE vEst='51'
      vFun ='Contabilizado'
   CASE vEst='99'
      vFun ='Anulado     '
   ENDCASE
RETURN vFun
FUNCTION VerEstOB
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00' or vEst='  '
      vFun ='Emitido     '
   CASE vEst='5'
      vFun ='Liquidado   '
   CASE vEst='99'
      vFun ='Anulado     '
   ENDCASE
RETURN vFun


FUNCTION val_Obra   && VALIDA OBRAS  (DEL PRESUPUESTO)
*----------------
PARAMETERS cvalor, cvariable, ccol, clong , cAncho
PRIVATE malias
cvalor=ALLT(cvalor)
DO CASE
	CASE PARAMETERS() = 2
		ccol = 0
		cvariable = ' '
		clong = 40
		cAncho = 6
	CASE PARAMETERS() = 3
		ccol = 0
		clong = 40
		cAncho = 6		
	CASE PARAMETERS() = 4
		clong = 40               && Longitud campo DESCRI
		cAncho = 6
	CASE PARAMETERS() = 5		
		cAncho = 6
ENDCASE
calias  = ALIAS()
** TIENE QUE VALIDAR CON CALENDARIOS
SELECT OBRA
SEEK cvalor
if !found() OR !cvariable $'V' 
*	SET FILTER TO periodo+codfun+codprg+codsubpr+codproy+codsupry = ALLTRIM(cfiltro)
*	GO TOP
	IF !EMPTY(cvalor) AND !EOF()
		SEEK cvalor
		IF !FOUND()
			DO rolea_ob
		ENDIF
	ELSE
		DO rolea_ob
	ENDIF
ENDIF
cvalor = Obra.CodObra
cdescr = SUBSTR(Obra.descri,1, clong )
wdestino = ALLTRIM(SUBSTR(Obra.descri,1,100))
*-Variables
SET FILTER TO
IF !EMPTY( calias )
	SELECT (calias)
ENDIF
DO CASE
	CASE cvariable=' '   && En edici¢n
		@ ROW(),ccol     SAY PADR(cvalor,cancho,' ')
		@ ROW(),ccol     SAY cdescr
		RETURN .T.
	CASE cvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),ccol     SAY cdescr
		RETURN
	CASE cvariable='V'   && En vista
		@ ROW(),COL()    SAY PADR(cvalor,cancho,' ')
		RETURN cdescr
	CASE cvariable='D'   && En vista
		RETURN cdescr
	CASE cvariable='Z'   && En vista SIN PINTAR
		RETURN cdescr
	CASE cvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		&cvariable = cvalor
		RETURN .T.
ENDCASE

PROCEDURE rolea_ob
*-----------------
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 03,10 TO 20,69 DOUBLE FLOAT SHADOW COLOR SCHEME 10
GO TOP
DO CASE
	CASE !EOF()
		BROWSE WINDOW _xx TITLE ' OBRAS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			periodo   :H='Periodo' ,;
			codObra   :H='Obra',;		
			descri    :H='Detalle'
			
			* codproy   :H='Pry' ,;

	OTHER
		IF !EMPTY(cvalor)
			DO standby WITH 'No se tiene Obra en referencia'
		ENDIF
ENDCASE
ON KEY LABEL enter
RELEASE WINDOW _xx
IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN

FUNCTION ve_passw
*----------------
** Pide el password de acceso a cada opci¢n del sistema
PARAMETER _psw
PRIVATE xxx
   xxx = SPACE(5)
   DEFINE WINDOW _pasw FROM 12,15 TO 17,65  TITLE " Password de seguridad " COLOR SCHEME 5
   ACTIVATE WINDOW _pasw
   CLEAR
   @ 1, 4 SAY "Opci¢n restringida, ingrese password: "
   @ 2,20 SAY "[     ]"
   @ 2,21 GET xxx PICTURE "!!!!!" COLOR ,X
   READ
   RELEASE WINDOW _pasw
   IF LASTKEY()#27 .AND. xxx=_psw
      RETURN .T.
   ELSE
      DO STANDBY WITH "Password incorrecto. ! Acceso denegado !"
   ENDIF

RETURN .F.

FUNCTION blo_sis
*----------------
* Funci¢n de Bloqueo del sistema para Realizar operaci¢nes de
* actualizaci¢n 
* Parametros 1 Verificaci¢n de Bloqueo => Regresa Falso si esta desbloqueado
*            2 Bloquea el Sistema     
*            3 Libera el Sistema
* Autor : FMC / Saiki  
*------------------------------------------------
PARAMETERS mbloqueo
SELECT parma
SEEK 'OPERATBLOQUE'
DO CASE 
   CASE mbloqueo=1
        IF ALLTRIM(Parma.Descri)='NO'
           RETURN .F.
        ENDIF   
   CASE mbloqueo=2     
        IF RLOCK()
           REPLACE  Parma.Descri WITH 'SI'
        ENDIF
        UNLOCK   
        RETURN .T.
   CASE mbloqueo=3     
        IF RLOCK()
           REPLACE Parma.Descri WITH 'NO'
        ENDIF
        UNLOCK   
        RETURN .T.
ENDCASE        
RETURN

PROCEDURE ESPERA 
*-----------------------------------------------------------
* Esta funcion activa una pantalla de Espera 
* en caso de que se procesada a indexar o demorar el proceso
* Desarrollado por : FMC Saiki. Sucursal Piura
* Fecha : 11/01/96
*-----------------------------------------------------------
PARAMETER festado,fmensaje   
 DEFINE WINDOW Espera  FROM 21,05 TO 23,75 COLOR SCHEME 1
*DEFINE WINDOW Espera  FROM 11,05 TO 13,75 COLOR SCHEME 5
   DO CASE 
      CASE festado=1
      	   ACTIVATE WINDOW Espera
	  	   @ 0,0 SAY PADC( ALLTRIM(fmensaje) , WCOLS() )COLOR W+/BR*
	  	   RETURN
	  CASE festado=2
		   DEACTIVATE WINDOW Espera
		   RETURN 
	ENDCASE
RETURN

FUNCTION vEstPol
*---------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
	CASE vest='00'
		vfun ='Emitido      '
	CASE vest='50'
		vfun ='Atendifo     '
	CASE vest='51'
		vfun ='Contabilizado'		
	CASE vest='99'
		vfun ='Anulado      '
ENDCASE
RETURN vfun

PROCEDURE Estado
*-----------------
PARAMETERS vDoc,vllave1
PRIVATE vAlias,vPerhc,vNumhc,vFun
vAlias = ALIAS()
ON KEY LABEL F7
DEFINE WINDOW Estado FROM 5,3 TO 9,77 TITLE '®ESC¯ Salir    °°° Seguimiento de ';
					+IIF(vDoc='PE','PECOSA',IIF(vDoc='SS','Solicitud de Servicio',;
					IIF(vDoc = 'OC','O/C','O/S')))+' °°°               ' &&COLOR SCHEME 7
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
vfun = .t.
DO CASE
	CASE UPPER(vDoc) = 'PE'
		SELECT Orden
		vOrd = ORDER()
		SET ORDER TO OrdCom1
		SEEK &vLlave1 
		IF FOUND() AND !ESTADO$'0099'
			vPerhc = Orden.Perhc
			vNumhc = Orden.Numhc
		ELSE
			vPerhc = ''
			vNumhc = ''
			vfun = .f.
		ENDIF
		SET ORDER TO (vOrd)
	
	CASE UPPER(vDoc) = 'SS'
		SELECT 	OrdSe
		vOrd = ORDER()
		SET ORDER TO OrdSer1
		SEEK &vLlave1
		IF FOUND() AND !ESTADO$'0099'
			vPerhc = OrdSe.Perhc
			vNumhc = OrdSe.Numhc
		ELSE
			vPerhc = ''
			vNumhc = ''
			vfun = .t.			
		ENDIF
		SET ORDER TO (vOrd)

ENDCASE

SELECT Hoja
vOrd = ORDER()
SET ORDER TO hojcon1
SEEK IIF(vdoc='OC' OR vdoc='OS',&vllave1,vPerHc+vnumhc)
IF FOUND() AND !ESTADO$'0099' and vfun
    IF !EMPTY(HOJA.NumCp)
		vNumCp  = Hoja.NumCp
		vMesCp  = Hoja.NumMesCp    
		vCta    = Hoja.CodCtc
    else		
		vNumcp  =''
		vMescp  =''
		vCta    =''
    	vfun = .f.    
    endif 	
ELSE
	vNumcp  =''
	vMescp  =''
	vCta    =''
	vfun = .f.
ENDIF
SET ORDER TO (vOrd)

SELECT Compag
vord = ORDER()
SET ORDER TO Compag1
SEEK ALLTRIM(vMescp)+vNumcp+ALLTRIM(vcta)
IF FOUND() and vfun
	vPercp = Periodo
	vImport = Compag.Import
	vreten  = Compag.reten
ELSE
	vPercp = ''
	vfun = .f.
	vImport = 0
	vreten  = 0
	
ENDIF
SET ORDER TO (vOrd)

SELECT Cheque
vord = ORDER()
SET ORDER TO Cheque1
SEEK vMesCp+vNumCp+vCta
IF FOUND() and vfun
	vNumCh = Cheque.NumChq
ELSE
	vNumCh =''
ENDIF
SET ORDER TO (vOrd)

DO CASE
	CASE vDoc='OC' OR vdoc='OS'
		IF FOUND() and vfun
			DEACTIVATE WINDOW Standby
			BROWSE ;
			   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY vMesCp+vNumCp+vCta ;
			   WINDOW Estado ;
			   FIELDS;
			   p   = IIF(EMPTY(m.Numhc),'No afect',m.Numhc+'/'+m.perhc)	 :H=' H/C ':8,;
			   cp  = IIF(EMPTY(vNumCp),'Sin pago',vNumCp+'/'+vmesCp)  :H='   C/P ':8,;
			   cheq= IIF(EMPTY(vNumCh),'No girado',vNumCh)		     :H='  Cheq.':12,;   
			   imp = IIF(EMPTY(vNumCp),'    ---.--',vimport)			 :H='  Total':12,;
			   ret = IIF(EMPTY(vNumCp),'    ---.--',vreten)		     :H=' Retenido':12,;
			   tp  = IIF(EMPTY(vNumCp),'    ---.--',vimport-vreten)    :H='Total Pagado':12
		ELSE
			SELECT Numchq WHERE RECNO()=1 FROM Cheque INTO CURSOR query
			SELECT query
			DEACTIVATE WINDOW Standby
			BROWSE ;
			   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT ;
			   WINDOW Estado ;
			   FIELDS;
			   p   = IIF(EMPTY(m.Numhc),'No afect',m.Numhc+'/'+m.perhc)	 :H=' H/C ':8,;
			   cp  = IIF(EMPTY(vNumCp),'Sin pago',vNumCp+'/'+vmesCp)  :H='   C/P ':8,;
			   cheq= IIF(EMPTY(vNumCh),'No girado',vNumCh)		     :H='  Cheq.':12,;   
			   imp = IIF(EMPTY(vNumCp),'    ---.--',vimport)			 :H='  Total':12,;
			   ret = IIF(EMPTY(vNumCp),'    ---.--',vreten)		     :H=' Retenido':12,;
			   tp  = IIF(EMPTY(vNumCp),'    ---.--',vimport-vreten)    :H='Total Pagado':12
		ENDIF
	OTHER
		IF FOUND() and vfun
			DEACTIVATE WINDOW Standby
			BROWSE ;
			   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT KEY vMesCp+vNumCp+vCta ;
			   WINDOW Estado ;
			   FIELDS;
			   oc  = IIF(EMPTY(IIF(vdoc='SS',solser.numos,Itepec.Numoc)) ,'Pendiente'     ,IIF(UPPER(vDoc)='SS',SolSer.NumOs,Itepec.Numoc)):H = IIF(UPPER(vDoc)='SS','  O/S  ','   O/C  '):14,;			   
			   hc  = IIF(EMPTY(vNumhc),'Sin afectaci¢n',vNumhc+'/'+vperhc)   :H='   H/C ':14,; 
			   cp  = IIF(EMPTY(vNumcp),'Sin pago'      ,vNumcp+'/'+vMescp)   :H='   C/P ':14,;
			   cheq= IIF(EMPTY(vNumCh),'No girado'     ,vNumCh)		         :H='  Cheq.':14   
		ELSE
			SELECT Numchq WHERE RECNO()=1 FROM Cheque INTO CURSOR query
			SELECT query
			DEACTIVATE WINDOW Standby
			BROWSE ;
			   NOAPPEND NODELETE NOCLEAR NOMENU NOOPTIMIZE NOREFRESH NOEDIT ;
			   WINDOW Estado ;
			   FIELDS;
			   oc  = IIF(EMPTY(IIF(vdoc='SS',solser.numos,Itepec.Numoc)) ,'Pendiente'     ,IIF(UPPER(vDoc)='SS',SolSer.NumOs,Itepec.Numoc)):H = IIF(UPPER(vDoc)='SS','  O/S  ','   O/C  '):14,;
			   hc  = IIF(EMPTY(vNumhc),'Sin afectaci¢n',vNumhc+'/'+vperhc)   :H='   H/C ':14,; 			   
			   cp  = IIF(EMPTY(vNumcp),'Sin pago'      ,vNumcp+'/'+vMescp)   :H='   C/P ':14,;
			   cheq= IIF(EMPTY(vNumCh),'No girado'     ,vNumCh)		         :H='  Cheq.':14   
		ENDIF
ENDCASE

SELECT &vAlias
DO CASE
	CASE UPPER(vDoc) = 'PE'
		ON KEY LABEL F7 DO Estado WITH 'PE','ItePec.Periodo+Itepec.Numoc+Itepec.Codfte'
	CASE UPPER(vdoc) = 'SS'
		ON KEY LABEL F7 DO Estado WITH 'SS','Solser.Periodo+solser.Numos+solser.Codfte'
	CASE UPPER(vDoc) = 'OC'
		ON KEY LABEL F7 DO Estado WITH 'OC','m.perhc+m.numhc'
	CASE UPPER(vDoc) = 'OS'
		ON KEY LABEL F7 DO Estado WITH 'OS','m.perhc+m.numhc'

ENDCASE
RETURN

FUNCTION VerEstCP
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00'
      vFun ='Pendiente   '
   CASE vEst='10'
      vFun ='Retencion OK'
   CASE vEst='20'
      vFun ='Chq.Girado  '
   CASE vEst='50'
      vFun ='C/P Aprobado'
   CASE vEst='99'
      vFun ='Anulado     '
   ENDCASE
RETURN vFun


**********************

FUNCTION prvbus
*--------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

DEFINE WINDOW _FUNBUS    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW
DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
DEFINE BAR 3 OF _mm PROMPT ' Busqueda por \<RUC    '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '0000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 1
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	CASE BAR()=3
		ACTIVATE WINDOW _funbus
		_cod = SPACE(8)
		@01,02 SAY 'RUC: ' GET _cod PICTURE '@S8'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO 3
			SEEK ALLTRIM(_cod)
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
IF !FOUND()
	DO standby WITH 'Proveedor no encontrado'
ENDIF
RETURN

function valtod
*--------------
parameters _toda, _x,_y
private _toda, _x, _y
if _toda = 1
   @ _x,_y say space(40)
endif
return .t.


FUNCTION VAL_LEE
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
* Autor: C‚sar Ch varry Arias   Fecha: 14-09-93
PRIVATE _Fun
_Fun = .T.
IF LASTKEY() # 27
  IF .NOT. YESNO('¨ Est n correctos los datos ?',11,15)
    _Fun = .F.
  ENDIF
ENDIF
RETURN (_Fun)

PROCEDURE ANALSAL
*----------------
private as,ort
as=alias()
ort = order()
IF ALLTRIM(m.tipfun)='F'
	Xvalor = m.periodo+alltrim(m.codfun)+alltrim(m.codprg)+alltrim(m.codsubpr)            
	proact = alltrim(m.codact)
ELSE	
	Xvalor = m.periodo+alltrim(m.codfun)+alltrim(m.codprg)+alltrim(m.codsubpr)+alltrim(m.codproy) 
	proact = alltrim(m.codsupry)
ENDIF
AS=ALIAS()
Private vdf
vdf = .t.
ACTIVATE SCREEN
on key label f5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10
USE IN 18
USE IN 19
USE IN 20


IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ,CALEN.TOTPRE , CALEN.ESTADO;
		  FROM CALEN ;
		  WHERE  tipfun+codfte+periodo+codfun+codprg+iif(!empty(m.Codsubpr),codsubpr,'') + iif(!empty(m.codproy),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND TIPFUN='I'  AND val(nummes)<=val(m.nummes) ;
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS,CALEN.TOTPRE,CALEN.ESTADO ;
               FROM CALEN ;
		 	   WHERE tipfun+codfte+periodo+codfun+codprg+iif(!empty(m.Codsubpr),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'')  AND TIPFUN='F'  AND val(nummes)<=val(m.nummes) ;
					INTO CURSOR CalenX
ENDIF

vInd = SYS(3) + '.DBF'
YDX = SYS(3)+'.IDX'
COPY TO (vInd)
use (vind) in 20 alias calen1 EXCLU

SELE CALEN1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (YDX)
Else
   INDEX ON CODSUBPR+CODACT+CODPART TO (YDX)
endif

GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotcal = 0
	IF ALLTRIM(m.tipfun) = 'I'
	    ***---- ACUMULA CALENDARIOS
        SELE CALEN1
        GO TOP
        SCAN 
              vPart1 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              vPart2 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              vTotAfe = 0
              DO WHILE  vPart1 = vPart2
                  do case
                     case Calen1.estado='99'
                          vPor = 0
                     OTHER
                          vPor = 1
                  endcase   		                 		                 		
                  vTotcal = vTotcal + vpor*Valpart
                  SKIP
                  vPart1 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              ENDD
              SKIP -1
              sele ITEPAR
              VANTER = RECNO()
              set orde to itepar5
			  seek calen1.codfun+calen1.CodPrg+calen1.CodFte+calen1.CodSubpr+calen1.CodProy+calen1.CodSuPry+calen1.CodPart
		      if found()
		        if rlock()
	              replace cantcal with vtotcal
	            endif
	            unlock
                vTotcal = 0	
	          endif
	          SELE Calen1
     	ENDSCAN
     	
	ELSE
		***---- ACUMULA CALENDARIOS 
        SELE CALEN1
        GO TOP 
        SCAN 
              vPart1 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              vPart2 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              vTotCal = 0
              DO WHILE  vPart1 = vPart2
                  do case
                     case Calen1.estado='99'
                          vPor = 0
                     OTHER
                          vPor = 1
                  endcase   		                 		                 		
                  vTotcal = vTotcal + vpor*Valpart
                  SKIP
                  vPart1 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              ENDD
              SKIP -1
              sele ITEPAR
              VANTER = RECNO()
              set orde to itepar6
			  seek calen1.CodFun+calen1.CodPrg+calen1.CodFte+calen1.CodSubpr+calen1.CodAct+calen1.CodPart
			  if found()
		        if rlock()
	              replace cantcal with vtotcal
	            endif
	            unlock
	            VTOTCAL = 0
	         endif
	         SELE Calen1
     	ENDSCAN
	ENDIF
ELSE
	    ******  CUANDO NO EXISTE CALENDARIO
		DO standby WITH 'No se tiene Calendario para este Mes'
		vdf =.f.
ENDIF
USE IN 20
SELE ITEPAR
GO  VANTER 
select (as)
ON KEY LABEL ENTER
RETURN vdf

PROCEDURE borrdet
*---------------
IF RLOCK()
	IF yesno('Desea borrar esta Clasificaci¢n?')
		DELETE NEXT 1
	ENDIF
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
RETURN .T.

PROCEDURE agr2det
*---------------
DO STANdBY WITH 'Consulte con almacen,para a¤adir nuevo C¢digo'
return .t.

PRIVATE vfun
vfun=.F.
vcodart = '00.'   && provisional
SELECT Parma
SEEK 'CORREL'+'CODART'
vCart   = padl(alltrim(str(Parma.NumEnt+1,4)),7,'0') && provisional
vtipart = SUBSTR(produ.codart,1,1)
SELE ITEART
IF f_appd()
	REPLACE codart  WITH vcodart+vcart,tipart WITH vtipart ,fecreg WITH DATE()	&&provisional		
	vfun=.T.
ENDIF

SELE STOCK
IF f_appd()
	REPLACE codart  WITH vcodart+vcart,tipart WITH vtipart ,fecreg WITH DATE() ,codinv WITH '0695'	&&provisional		
	vfun=.T.
ENDIF

SELECT Parma
SEEK 'CORREL'+'CODART'
REPLA NUMENT WITH NumEnt+1

SELE ITEART
SET ORDER TO 2
ON KEY LABEL f10 KEYBOARD CHR(23)
ON KEY LABEL f8 DO borrdet
ON KEY LABEL f5 DO agr2det
ON KEY LABEL f2 DO funbusdet
RETURN vfun

FUNCTION Val_Tipo
*----------------
  PARAMETERS mValor, Filtro, mVariable    
  PRIVATE mAlias, mLong
  mLong = 40               && Longitud campo DESCRI
  mAlias  = ALIAS()

  SELECT Parma
  SEEK Filtro+mValor

  IF !FOUND()   
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   RETURN	
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELDS;
           Codigo+' '+CodigoAux+' '+SUBSTR(Descri,1,40)
           ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = Parma.Codigo
  mCuenta= Parma.DescriAu2
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = SUBSTR( Parma.DescriAux, 1, mLong)
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
IF LASTKEY()#27
      REPLACE &mVariable WITH mValor
      RETURN .T.
ENDIF
PROCEDURE VAL_cal
*----------------
PRIVATE ALS
ALS=ALIAS()
ON KEY LABEL F7
IF USED('CALEN1')
	SELE CALEN1
	USE
ENDIF

SELEC CALEN
GO TOP
vdbf = sys(3)+'.dbf'
copy to (vdbf)
USE (VDBF) IN 0 ALIAS CALEN1 exclu
DEACTIVATE WINDOW Standby

VDX = SYS(3)+'.IDX'
ZDX = SYS(3)+'.IDX'

SELE CALEN1
*SET FILTER TO CODACT=vCodAct AND NUMMES=M.NUMMES
IF ALLTRIM(m.tipfun)='F'
*M  	SET FILTER TO CODACT=vCodAct AND NUMMES=M.NUMMES
    INDEX ON CODSUBPR+CODACT+CODPART TO (VDX) FOR CODACT=vCodAct AND NUMMES=ALLTRIM(M.NUMMES)
    GO TOP
    DEFINE WINDOW _yy FROM 04,03 TO 19,75 DOUBLE FLOAT SHADOW TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' COLOR SCHEME 10
    ACTIVATE WIND _yy
	BROWSE NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	        XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':11 ,;
	        FRO=IIF(RIGHT(CODPART,2)='00' OR VALPART=0,CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' '),'') :H='Act':7  ,;
			vv=Codpart  :V=ValPart2(CodPart,CodPart):H='Partida' ,;
			Valpart  :H='     Asignado':P='99,999,999.99',;
			Totafe             :H='     Afectado':P='99,999,999.99',;
			zz=valpart- TotAfe :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _YY
ELSE
	SET FILTER TO CODPROY=vProyec AND NUMMES=M.NUMMES
    INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (VDX)
    GO TOP
    DEFINE WINDOW _ww FROM 04,08 TO 15,71 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _ww TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	        FRO = CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' ') : H='SubPry'  ,;
			vv=' '+Codpart  :H='Partida' ,;
			Valpart  :H='     Asignado':P='99,999,999.99',;
			Totafe             :H='     Afectado':P='99,999,999.99',;
			zz=valpart- TotAfe :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _ww
ENDIF
*SELECT (ALS)
ON KEY LABEL F7 DO VAL_CAL
return


PROCEDURE ANALPRE
*----------------
PARAMETERS vCantOC,vtipO,vmues
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=Xrolea_PRE(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub)                , alltrim(vcodact),vcantoc,VTIPO,vmues)
ELSE	
	WDF=Xrolea_PRE(m.periodo+alltrim(vcodfun)+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO,vmues)
ENDIF
*USE IN 24
RETURN WDF


PROCEDURE Xrolea_PRE
*-----------------------
parameter xvalor,proact,vtotoc,vtipo,vmues
AS=ALIAS()
Private vdf
vdf = .t.
ACTIVATE SCREEN
on key label f5 on key
ON KEY LABEL enter KEYBOARD CHR(23)
ACTIVATE WINDOW Standby
@ 1,14 SAY "Espere un momento ..." COLOR W+/RB*
DEFINE WINDOW _yy FROM 04,07 TO 19,72 DOUBLE FLOAT SHADOW COLOR SCHEME 10

XDX = SYS(3)+'.IDX'
YDX = SYS(3)+'.IDX'
ZDX = SYS(3)+'.IDX'

IF ALLTRIM(m.tipfun)='I'

   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	      ITEHC.CODSUPRY, ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
		  FROM ITEHC ;
		  WHERE  tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND ESTADO#'99' AND TIPFUN='I' AND val(nummes)<=val(vcalend) ;
				INTO CURSOR Hojax
ELSE

 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR, ITEHC.CODFUN,ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL ;
           FROM ITEHC ;
	 	   WHERE tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,left(CODanal,3)='5.3',.T.) AND ESTADO#'99' AND TIPFUN='F'  AND val(nummes)<=val(vcalend) ;
					INTO CURSOR HOJAX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 18 alias hoja1 EXCLU

IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ,CALEN.TOTPRE , CALEN.ESTADO;
		  FROM CALEN ;
		  WHERE  tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='I'  AND val(nummes)<=val(vcalend) ;
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODFUN,CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS,CALEN.TOTPRE,CALEN.ESTADO ;
               FROM CALEN ;
		 	   WHERE tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='F'  AND val(nummes)<=val(vcalend) ;
					INTO CURSOR CalenX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 19 alias calen1 EXCLU

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODPROY, ITEPAR.DESSUPRY ,itepar.cresup,itepar.transf ,;
	      ITEPAR.CODSUPRY, itepar.CODFUN,ITEPAR.CODPRG,ITEPAR.CODSUBPR,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
		  FROM ITEPAR ;
		  WHERE  tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,left(CODPART,3)='5.3',.T.) AND TIPFUN='I';
				INTO CURSOR PRESUX
ELSE
	 	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO,itepar.cresup,itepar.transf, ;
	     	   ITEPAR.CODSUBPR, itepar.CODFUN,ITEPAR.CODPRG,ITEPAR.CODACT,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
               FROM ITEPAR ;
		 	   WHERE tipfun+codfte+periodo+codfun+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,ITEPAR.CODPART='02',IIF(VTIPO=2,ITEPAR.CODPART='03',.T.)) AND TIPFUN='F';
					INTO CURSOR PRESUX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 20 alias PRESU1 EXCLU

SELE HOJA1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (XDX)
Else
   INDEX ON CODSUBPR+CODACT+CODANAL TO (XDX)
endif

SELE CALEN1
If alltrim(m.tipfun)='I'
   INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (YDX)
Else
   INDEX ON CODSUBPR+CODACT+CODPART TO (YDX)
endif

SELE PRESU1
If alltrim(m.tipfun)='I'
	index on Periodo+codfun+codprg+CodFte+CodPart+CodSubpr+CodProy+CodSuPry to (ZDX)
ELSE
	index on Periodo+codfun+codprg+CodFte+Codpart+CodSubpr+CodAct to (ZDX)
ENDIF

GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	vTotcal = 0
	IF ALLTRIM(m.tipfun) = 'I'
	   vKeyI = Calen1.NumMes+ Calen1.TipFun+ Calen1.CodFte+ calen1.CODFun+Calen1.Codprg  &&+Calen1.Codsubpr+ Calen1.CodProy+ Calen1.CodSuPry
	   vOrd = ORDER()
       ***------- ACUMULA AFECTACIONES	
       SELECT HOJA1
       SCAN 
	      vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      vPart2 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      vTotAfe = 0
	      DO WHILE  vPart1 = vPart2
	         do case
	           case HOJA1.estado='92'
	               vPor = iif(HOJA1.tipope='-',-1,1)
	           case HOJA1.estado='94'
	               vPor = -1
	           case HOJA1.estado='99'
	               vPor = 0
	           OTHER
	               vPor = 1
	         endcase   		                 		                 		
	         vTotafe = vTotafe + vpor*Valpart
	         SKIP
	         vPart1 = HOJA1.CODSUBPR+HOJA1.CODPROY+HOJA1.CODSUPRY+HOJA1.CodPart
	      ENDD
	      SKIP -1
	      sele PRESU1
	      seek m.Periodo+HOJA1.CodFun+HOJA1.CodPrg+HOJA1.CodFte+HOJA1.CodPart+HOJA1.CodSubpr+HOJA1.CodProy+HOJA1.CodSuPry
		  if found()
			 if rlock()
	            replace totAfe with vtotafe
	         endif
	         unlock
	      endif
	      SELE HOJA1
	    ENDSCAN
	
	    
	    ***---- ACUMULA CALENDARIOS
	    
        SELE CALEN1

        GO TOP
        SCAN WHILE ESTADO#'99'
              vPart1 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              vPart2 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              vTotAfe = 0
              DO WHILE  vPart1 = vPart2
                  do case
                     case Calen1.estado='99'
                          vPor = 0
                     OTHER
                          vPor = 1
                  endcase   		                 		                 		
                  vTotcal = vTotcal + vpor*Valpart
                  SKIP
                  vPart1 = CALEN1.CODSUBPR+CALEN1.CODPROY+CALEN1.CODSUPRY+CALEN1.CodPart
              ENDD
              SKIP -1
              sele PRESU1
			  seek m.Periodo+calen1.CODFUN+calen1.CODPRG+calen1.CodFte+calen1.CodPart+calen1.CodSubpr+calen1.CodProy+calen1.CodSuPry
		      if found()
		        if rlock()
	              replace totcal with vtotcal
	            endif
	            unlock
                vTotcal = 0	
	         endif
	         SELE Calen1
     	ENDSCAN
	    
	ELSE
	   	vKeyF = Calen1.NumMes+ Calen1.TipFun+ Calen1.CodFte+ calen1.CODFUN+calen1.CODPRG+Calen1.Codsubpr+ Calen1.CodAct
		vOrd = ORDER()
		SELECT HOJA1
		GO TOP
      	SCAN while Estado # '99'
	       vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	       vPart2 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
	       vTotAfe = 0
	       DO WHILE  vPart1 = vPart2
	          do case
	             case HOJA1.estado='92'
	                  vPor = iif(HOJA1.tipope='-',-1,1)
	             case HOJA1.estado='94'
	                  vPor = -1
	             case HOJA1.estado='99'
	                  vPor = 0
	             OTHER
	                  vPor = 1
	          endcase   		                 		                 		
	          vTotafe = vTotafe + vpor*Valpart
              SKIP
		      vPart1 = HOJA1.CODSUBPR+HOJA1.CODACT+HOJA1.Codanal
		    ENDD
		    SKIP -1
 		    sele presu1 	
			seek m.Periodo+HOJA1.Codfun+HOJA1.CodPrg+HOJA1.CodFte+HOJA1.CodAnal+HOJA1.CodSubpr+HOJA1.CodAct
			if found()
			   replace totAfe with vtotafe
			endif
			unlock
		    SELE HOJA1
		  ENDSCAN
		***---- ACUMULA CALENDARIOS 
		
        SELE CALEN1
        GO TOP 
        SCAN WHILE CALEN1.ESTADO#'99'
              vPart1 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              vPart2 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              vTotCal = 0
              DO WHILE  vPart1 = vPart2
                  do case
                     case Calen1.estado='99'
                          vPor = 0
                     OTHER
                          vPor = 1
                  endcase   		                 		                 		
                  vTotcal = vTotcal + vpor*Valpart
                  SKIP
                  vPart1 = CALEN1.CODSUBPR+CALEN1.CODACT+CALEN1.CodPart
              ENDD
              SKIP -1
              sele PRESU1
			  seek m.Periodo+calen1.CODFUN+calen1.CODPRG+calen1.CodFte+calen1.CodPart+calen1.CodSubpr+calen1.CodAct
			  if found()
		        if rlock()
	              replace totcal with vtotcal
	            endif
	            unlock
	         endif
	         SELE Calen1
     	ENDSCAN
	ENDIF
	
	SELECT PRESU1
    GO TOP
    SCAN
      IF RIGHT(CODPART,2)='00'
         CV=RECNO()
         if alltrim(m.tipfun)='F'
  	    	VPAR = codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,2)
   	    	SELE PRESU1
			SUM TOTAFE TO VSUMA FOR codfun+codprg+CODSUBPR+CODACT+LEFT(CODPART,2) = VPAR
         ELSE
			VPAR = codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2)
           	SELE PRESU1				
		    SUM TOTAFE TO VSUMA FOR codfun+codprg+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) = VPAR
     	 ENDIF
		 GO CV
		 IF RLOCK()
			REPLACE TOTAFE WITH VSUMA
		 ENDIF
		 UNLOCK
	  ENDIF
	ENDSCAN	
	GO TOP
    if vmues
		DO VAL_PRE
	endif	
ELSE
	    ******  CUANDO NO EXISTE CALENDARIO
		DO standby WITH 'No se tiene Calendario para este Mes'
		vdf =.f.
ENDIF
SELECT Itehc
set order to itehc1
ON KEY LABEL enter
RELEASE WINDOW _yy

RETURN vdf

FUNCTION traprgau
*---------------
vkey = codfun+codprg + codsubpr
vrec = RECNO()
GO TOP
SUM transf TO sumsg FOR codfun+codprg+codsubpr = vkey AND RIGHT(codpart,2)#'00'
GO vrec
RETURN sumsg


PROCEDURE VAL_PRE
*----------------
PRIVATE ALS
ALS=ALIAS()
ON KEY LABEL F7
SELEC PRESU1
GO TOP
DEACTIVATE WINDOW Standby
vCodSub   = '   '
vCodAct   = '  '
ADX = SYS(3)+'.IDX'

IF ALLTRIM(m.tipfun)='F'

    INDEX ON CODSUBPR+codact+CODPART TO  (ADX)
    GO TOP
    DEFINE WINDOW _yy FROM 04,06 TO 19,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	        XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':11 ,;
	        FRO=IIF(RIGHT(CODPART,2)='00' OR VALPART=0,CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' '),'') :H='Act':6  ,;	
			vv=' '+Codpart  :H='Partida' ,;
			cc=Valpart+cresup+transf   :H='Marco +/- Mod':P='99,999,999.99',;
			TotCal             :H='Con Calendar.':P='99,999,999.99',;
			Totafe             :H='Afectado     ':P='99,999,999.99',;			
			zz=valpart+cresup+transf-Totcal :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _YY			
ELSE
    INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (ADX)
    GO TOP
    DEFINE WINDOW _ww FROM 04,04 TO 15,67 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _ww TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	        FRO = CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' ') : H='SubPry' :10 ,;
			vv=' '+Codpart  :H='Partida' ,;
			cc=Valpart+cresup+transf   :H='Marco +/- Mod':P='99,999,999.99',;
			TotCal             :H='Con Calendar.':P='99,999,999.99',;
			Totafe             :H='Afectado     ':P='99,999,999.99',;			
			zz=valpart+cresup+transf-Totcal :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _ww			
ENDIF		
SELECT (ALS)			
*ON KEY LABEL F10
ON KEY LABEL F7 DO VAL_PRE
return

PROCEDURE VAL_AFE
*----------------
PARAMETER VCPT,VTOT,VPART
PRIVATE OSW,VRET
VRET = .T.
ON KEY LABE F7
OSW = ALIAS()
SELECT CALEN1
LOCATE FOR CODPART=VCPT
IF FOUN()
	IF VALPART < TOTAFE+(VTOT-VPART)
	   DO STANDBY WITH 'No existe calendario para Asignar..'
	   DO VAL_CAL
	   sele (osw)
	   REPLACE VALPART WITH 0	   
	   Vret=.f.
	else
	   replace totafe with  TOTAFE+(VTOT-VPART)
	endif
ELSE
   	DO STANDBY WITH 'No existe calendario Asignado..'
   	DO VAL_CAL
   	sele (osw)
    REPLACE VALPART WITH 0   	
   	Vret=.f.
ENDIF
sele (osw)
ON KEY LABEL F7 DO VAL_CAL
return &&VRET


FUNCTION Val_PRET
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
  ENDCASE
  mAlias  = ALIAS()  
  SELECT Clase
  SEEK Filtro+mValor
  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
	 SET FILTER TO CodPart = Filtro
      ENDIF
      GO TOP
      IF EOF()
	 DO STANDBY WITH 'No existen partidas definidas'
	 SET FILTER TO
	 sele (malias)
	 return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CODPART)+'.'+ALLTRIM(CODSUBP)+' '+SUBSTR(DesPar,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
	 ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = ALLTRIM(Clase.Codpart) +'.'+ Clase.CodSubp
 mDescr = SUBSTR( Clase.Despar, 1, mLong )
 m.partret = mValor
 SET ORDE TO 1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol   SAY mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    CASE mVariable='T'
      &mVariable = mValor
      @ ROW(),mCol+7 SAY mDescr
      RETURN  mValor
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
  ENDCASE


FUNCTION Val_paraD
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  &&  PARA BUSCAR POR CODIGO Y SI NO LO ENCUENTRA 
  &&  EL POPUP SALE ORDENADO POR DESCRIPCION
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT Parma
  SEEK Filtro+mValor
	      *IF .NOT. FOUND() .AND. mVariable<>'V'
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    SET ORDER TO PARMAE2
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
	  SELECT (mAlias)
       ENDIF
	   return       
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
    SET ORDER TO PARMAE1
  ENDIF
  mValor = Parma.Codigo
  mCuenta= Parma.DescriAu2
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = SUBSTR( Parma.DescriAux, 1, mLong)
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN ' '
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE


FUNCTION val_parti    && VALIDA PARTIDAS de ingreso
*----------------
PARAMETERS mvalor, filtro, mvariable, MCOL, mlong
PRIVATE malias
DO CASE
	CASE PARAMETERS() = 2
		MCOL = 0
		mvariable = ' '
		mlong = 40
	CASE PARAMETERS() = 3
		MCOL = 0
		mlong = 40
	CASE PARAMETERS() = 4
		mlong = 40               && Longitud campo DESCRI
ENDCASE
malias  = ALIAS()
SELECT ingreso
SEEK filtro+mvalor

IF !FOUND() AND !mvariable $'VZ'
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	IF !EMPTY(filtro)
		SET FILTER TO codING = filtro
	ENDIF
	GO TOP
	IF EOF()
		DO standby WITH 'No existen partidas definidas'
		SET FILTER TO
		SELE (malias)
		RETURN &&.f.
	ENDIF
	DEFINE POPUP parametro FROM 03,40 PROMPT FIELD codING+' '+SUBSTR(desING,1,40)
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF

	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = ingreso.coding
mdescr = SUBSTR( ingreso.desing, 1, mlong )
m.codpart = mvalor
SET ORDE TO 1
IF !EMPTY( malias )
	SELECT (malias)
ENDIF
DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL   SAY mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
	CASE mvariable=='D'   && En vista
		RETURN mdescr
	CASE mvariable=='Z'   && En vista SIN PINTAR
		RETURN mdescr
	CASE mvariable=='C'   && Solo codigo
		RETURN .T.
	CASE mvariable=='T'
		&mvariable = mvalor
		@ ROW(),MCOL+7 SAY mdescr
		RETURN  mvalor
	OTHERWISE            && En browse de edici¢n
		REPLACE &mvariable WITH mvalor
ENDCASE

FUNCTION LastDay
*---------------
PARAMETERS Zmes,Zano
*- Devuelve una Fecha que corresponde al £ltimo dia del mes y a¤o
*- por M.R.A.
DO CASE
	CASE PARAMETERS() = 0
		zMes = MONT(DATE())
		Zano = YEAR(DATE())
	CASE PARAMETERS() = 1
		Zano = YEAR(DATE())
ENDCASE
DO CASE
	CASE zMes = 1 OR zMes=3 OR zMes = 5 OR zMes = 7 OR zMes = 8 OR zMes = 10 OR zMes = 12
		vDia = '31'
	CASE zMes = 4 OR zMes=6 OR zMes = 9 OR zMes = 11
		vDia = '30'
	CASE zMes = 2
		IF MOD(zAno,4)=0
			vDia = '29'
		 ELSE
			vDia = '28'
		ENDIF
ENDCASE

IF BETWEEN(zMes,1,12)
	vMes = PADL(ALLTRIM(STR(Zmes)),2,'0')
 ELSE
	vMes = "12"
ENDIF

vAno = ALLTRIM(STR(Zano))

vRet = CTOD(vDia+'/'+vMes+'/'+vAno)
RETURN vRet

*FUNCTION LastDay
*---------------
PARAMETERS Zmes,Zano
*- Devuelve una Fecha que corresponde al £ltimo dia del mes y a¤o
*- por J.F.G.
IF PARAMETERS()=1
   Zano = year(date())
ENDIF   
vDia = '31'
vMes = PADL(ALLTRIM(STR(Zmes)),2,'0')
IF !BETWEEN(vMes,'01','12')
   RETURN CTOD('')
ENDIF   
vAno = ALLTRIM(STR(Zano))
vCentury = SET("CENTURY")
IF !BETWEEN(VAL(vano),0,99)
   SET CENTURY ON
ENDIF   
vRet = CTOD(vDia+'/'+vMes+'/'+vAno)
DO WHILE EMPTY(vRet)
         vDia = STR(VAL(vDia)-1,2)
         vRet = CTOD(vDia+'/'+vMes+'/'+vAno)
ENDDO
SET CENTURY &vCentury        
RETURN vRet


FUNCTION datext
*--------------
PARAMETERS vFecha
*- Devuelve una cadena conteniendo la descripci¢n de la fecha
*- especificada en letras
*- por J.F.G.

vCentury = SET("CENTURY")
SET CENTURY ON
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
vRet  = PADL(ALLTRIM(STR(DAY(vFecha))),2,'0') + ;
        ' DE ' + ;
        ALLTRIM(SUBSTR(Meses,MONTH(vFecha)*9-8,9)) + ;
        ' DE ' + ;
        TRANSFORM(YEAR(vFecha),'9,999')
        
*        TRANSFORM( VAL(SUBS(DTOC(vFecha),7,4)),'9,999')
SET CENTURY &vCentury        
RETURN ALLTRIM(vret)

FUNCTION Xletras
*--------------
parameter num
private cad1

cad1 = ''
IF Num = 0
   RETURN ( cad1 )
ENDIF

deci = INT(MOD(num,1)*100)

cmm = INT ( NUM / 1000000 )
cmu = MOD ( NUM , 1000000 )
cmu = INT(cmu)

DO CASE
   CASE  ( cmm = 1 )
      cad1 = ' un mill¢n'
   CASE  ( cmm = 0 )
      cad1 = ''
   CASE  ( cmm > 1 )
      cad1 = NUM6( cmm ) + ' millones'
   OTHERWISE
      cad1 = '*****'
ENDCASE

cad1 = cad1 + NUM6( cmu )
cad1 = cad1 + ' y '+PADL(ALLTRIM(STR(deci,2)),2,'0')+'/100 '

RETURN ( cad1 )

FUNCTION Val_Codcad
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVariable = ' '
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 4
      mLong = 40               && Longitud campo DESCRI
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT maepre
  SEEK Filtro+mValor

  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd1 = WOUTPUT()
    ACTIVATE SCREEN
   	SET FILTER TO PERIODO+UNIGES+UNIEJE = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    ON KEY LABEL f2  DO busCodCad
    ON KEY LABEL f3  DO busDesCad
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 02,01 TO 23,78  DOUBLE ;
		TITLE '[F2]Cadena   [F3]Componente   [F10] seleccionar' COLOR SCHEME 15
		ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCAD	: H='CodCad',;
    	CODFUN	: H='Fn',;
    	CODPRG	: H='Prg',;
    	CODSPR	: H='SPrg',;
    	ACTPRY	: H='Act/Pry',;
    	CODCOM	: H='CodComp',;
    	CODMET	: H='Meta',;
    	Descri	: H='Descripci¢n':40
	
    RELEASE WINDOWS wind_cad
    ON KEY LABEL f2  
    ON KEY LABEL f3  
    *IF !EMPTY(_OldWnd1)
    *   ACTIVATE WINDOW &_OldWnd1
    *ENDIF

    SET FILTER TO
  ENDIF
  mValor = maepre.codcad
  mDescr = SUBSTR( maepre.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
*     @ ROW(),mCol SAY mDescr
*     RETURN ' '
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
*-------

Procedure busCodCad
*------------------
vcursor = recno()
DEFINE WINDOW lis FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda X C¢digo °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis
vcodcad  = space(4)
@  3,2 SAY '  Cadena : ' GET vcodcad    PICTURE '!!!!' 
READ
SEEK filtro+vcodcad
IF !FOUND()
	Do standby WITH 'No existe Codigo de Cadena...'
	go vcursor
ENDIF
RELEASE WINDOWS lis
RETURN

Procedure busDesCad
*------------------
vcursor = RECNO()
vorder  = ORDER()
DEFINE WINDOW lis FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis
vCODCOM = space(60)
@  3,2 SAY ' Componente : ' GET vCodcom   PICTURE '!!!!!' VALID VAL_PARA(vCODCOM,'CODCOM',' ',18,30)
READ
locate for codcom=ALLT(vcodcom)
IF !FOUND()
	Do standby WITH 'No existe Codigo de Componente...'
	go vcursor
ENDIF
RELEASE WINDOWS lis
RETURN

FUNCTION MFecha
*--------------
PARAMETERS vmes, vano
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9)) + ' ' + STR(vAno,4)

FUNCTION cMes
*--------------
PARAMETERS vmes
Meses = "ENERO    FEBRERO  MARZO    ABRIL    MAYO     JUNIO    JULIO    AGOSTO   SETIEMBREOCTUBRE  NOVIEMBREDICIEMBRE"
RETURN ALLTRIM(SUBSTR(Meses,vMes*9-8,9))

FUNCTION Val_Fun2
*----------------
PARAMETER v__al, v__dev, v__bus, v__cod, v__tipo, v__x, v__y, v__ver, v__Long
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWnd,_campo, mVali, mRec, v__x, v__y, v__lon, mParam
* Alias :     v__al  " "
* Var.Devol.: v__dev " "
* Var.Mostr.: v__bus ( en el popup ) " expr"
* Var.Modif.: v__cod   variable
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y   para el v__bus
* A ver     : v__Ver     Usado para visualizar, si no se pone, usa v__bus
* Long.A ver: v__Long    Usado para mostrar el tama¤o de v__Ver
   mEdita = (parameters()>=5) .and.  (v__tipo # 3)
   mMsg   = (parameters()>=7) .and.  (v__tipo # 3)
   mVali  = (parameters()=5)  .and.  (v__tipo = 3)
   v__Ver = IIF(PARAMETERS()>=8,v__ver,v__bus)
   mRec   = IIF(EOF(),-1,RECNO())
   mAlias = ALIAS()
   mParam = PARAMETERS()
   _campo = TRIM(mAlias)+'.'+VARREAD()
   SELECT &v__al
   SET FILTER TO ctaman=iif(vmancom=1,'S','N')
   _OldWnd = WOUTPUT()
   IF !mEdita
      SEEK v__cod
      v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__ver,""))
   ELSE
      IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
	 GO TOP
	 IF EOF()
	    DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
	    v_fun = .F.
	 ELSE
	    _ln = LEN(&v__bus)
	    _ln = IIF(_ln>65,65,_ln)

			ACTIVATE SCREEN
	    DEFINE POPUP v__xx FROM 1,80-_ln TO 12,79 PROMPT FIELD &v__bus
	    ON SELECTION POPUP v__xx DEACTIVATE POPUP
	    ACTIVATE POPUP v__xx
	    RELEASE POPUP v__xx

	    IF !EMPTY(_OldWnd)
	       ACTIVATE WINDOW &_OldWnd
	    ENDIF
	    IF Lastkey()=27
	       v_fun = .f.
	    ELSE
	       v__cod = &v__dev
	       IF mMsg
		  		*@ v__x,v__y SAY IIF(PARAMETERS()#9, TRIM(&v__ver),
		  
		   		@ v__x,v__y SAY IIF(mParam#9, TRIM(&v__ver),;
		    		SUBSTR(&v__ver,1,v__Long))
	       ENDIF
	       IF !EMPTY(mAlias)
		  SELECT (mAlias)
		  IF (v__tipo  =2)
		     IF mRec>0
			GO mRec
			REPLACE &_campo WITH v__cod
			v_fun = .T.
		     ELSE
			DO standby WITH " El archivo est  vac¡o "
			v_fun = .F.
		     ENDIF
		  ELSE
		     v_fun = .T.
		  ENDIF
	       ELSE
		  DO standby WITH " NO HAY ARCHIVO ABIERTO "
	       ENDIF
	    ENDIF
	 ENDIF
      ELSE
	 SEEK IIF(v__tipo#2,v__cod,&_campo)
	 IF mMsg .AND. FOUND()
	    @ v__x,v__y SAY IIF(PARAMETERS()#9, TRIM(&v__ver),;
		    SUBSTR(&v__ver,1,v__Long))
	 ENDIF
	 v_fun = FOUND()
      ENDIF
   ENDIF

   IF EMPTY(mAlias)
      SELECT 0
   ELSE
      SELECT (mAlias)
   ENDIF

   IF !EMPTY(ALIAS())
      IF mRec>0
	 GO mRec
      ELSE
	 GO BOTTOM
      ENDIF
   ENDIF
RETURN v_fun

PROCEDURE progreso
*-----------------
PARAMETER control,Cmens,total,actual
DO CASE
   CASE control=1 && pinta el cuadro
        DEFINE WINDOW Vview FROM 08,11 to 14,68 DOUBLE COLOR SCHEME(5) 
        ACTIVATE WINDOW Vview
        @ 02,00 TO 04,57
        @ 00,00 SAY Cmens
   CASE control=2 && pinta barra de avance
        vtotC=54
        porcA=ROUND((actual*100)/total,2)
        NumC=ROUND((porcA*54)/100,2)
        @ 03,01 SAY REPL('Û',Numc)
        @ 01,52 SAY porcA PICTURE '999%'
   CASE control=3 && borra el cuadro
        RELEASE WINDOW vview
ENDCASE
RETURN

