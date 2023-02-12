* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  SAI_LIB.PRG                                                        º
* º                                                                     º
* º                 LIBRERIA DEL SISTEMA DE LA REGION GRAU              º
* º                                                                     º
* º                                                                     º
* º  SAIKI Consultores   Av. Arequipa 1130 Dpto 102 - Lima Per£         º
* º                      Tel-Fax 709612                                 º
* º                                                                     º
* º  Observaci¢n: para Fox Pro 2.x                                      º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

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
      DO STANDBY WITH "Registro del archivo " + ALIAS() + " ocupado. Espere un momento por favor o presione <Esc> para cancelar"
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
      DO STANDBY WITH "El registro del file" + ALIAS() + " est  siendo utilizado. Se cancela la operaci¢n"
      ?? Beep
   ENDIF

CASE BTipo = 3        && Bloquea archivo hasta conseguirlo a cancela
   v_fun   = .F.

   IF FLOCK()
      v_fun   = .T.
   ELSE
      DO STANDBY WITH ALIAS() + "Archivo ocupado.  Espere un instante por favor o presione <Esc> para cancelar"
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
      DO STANDBY WITH ALIAS() + " Archivo ocupado. El proceso se cancela"
      ?? Beep
   ENDIF

ENDCASE

RETURN v_fun

function sigue
*------------
parameter xx
skip +1
return .t.

FUNCTION f_appd
*--------------
 APPEND BLANK
 v_fun = RLOCK() .OR. F_Lock(1)

RETURN v_fun


*************************************************************************
*  PROCEDURE fox_lock.prg
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
   Insiste   = .T.
   Bloquea   = .F.
   DO WHILE Insiste
      IF RLOCK()
         Bloquea   = .T.
         Insiste   = .F.
      ELSE
         DO STANDBY WITH "Registro ocupado. Espere un momento;
 por favor o presione <Esc> para cancelar."
         ?? Beep
         KTecla    = 0
         DO WHILE KTecla <> 27 .AND. (.NOT. RLOCK())
            KTecla    = INKEY()
         ENDDO
         IF KTecla <> 27
            Bloquea = .T.
         ENDIF
         Insiste = .F.
      ENDIF
   ENDDO

CASE BTipo = 2     && Intenta bloquear registro solo una vez

   Bloquea   = .F.
   IF RLOCK()
      Bloquea   = .T. 
   ELSE
      DO STANDBY WITH "El registro esta en uso. Presione <Esc> para cancelar. "
      ?? Beep
      KTecla    = 0
      DO WHILE KTecla <> 27
         KTecla    = INKEY()
      ENDDO
   ENDIF


CASE BTipo = 3        && Bloquea archivo hasta conseguirlo a cancela
   Insiste   = .T.
   Bloquea   = .F.
   DO WHILE Insiste
      IF FLOCK()
         Bloquea   = .T.
         Insiste   = .F.
      ELSE
         DO STANDBY WITH "Archivo ocupado.  Espere un instante por favor o"+;
                     " presiona <Esc> para cancelar."
         ?? Beep
         KTecla    = 0
         DO WHILE KTecla <> 27 .AND. (.NOT. FLOCK())
            KTecla    = INKEY()
         ENDDO
         IF KTecla <> 27
            Bloquea = .T.
         ENDIF
         Insiste = .F.
      ENDIF
   ENDDO

CASE BTipo = 4   && Intenta bloquear archivo solo una vez

   Bloquea   = .F.
   IF FLOCK()
      Bloquea   = .T.
   ELSE
      DO STANDBY WITH "Archivo ocupado.  Presione <Esc> para cancelar."
      ?? Beep
      KTecla    = 0
      DO WHILE KTecla <> 27
         KTecla    = INKEY()
      ENDDO
   ENDIF

ENDCASE
RETURN

*************************************************************************
* PROCEDURE fox_appd
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


******************************************************************
* PROCEDURE fox_errs
* Rutina que controla el error al agregar un nuevo registro
* Para los demas casos genera un archivo core.fox que contiene
* el estado de la memoria al momento de ocurrir el error
*
* Parametros : Enviar a esta rutina la funcion SYS(16)
******************************************************************

PROCEDURE fox_errs
*-----------------
PARAMETERS Prg_error
DO CASE
CASE ERROR() = 108
   DO STANDBY WITH "Un momento por favor el archivo est  en uso"
   RETRY
CASE ERROR() = 109
   *- Record used by another
   WAIT WINDOW 'Registro est  siendo modificado por otro usuario.' NOWAIT
   RETURN
CASE ERROR() = 130
   *- Record is not locked
   KEYBOARD CHR(13)
   RETURN
CASE ERROR() = 125
   IF yesno('La Impresora no est  lista. ¨Se reintenta impresi¢n?')
     SET DEVICE TO PRINT
     RETRY
   ELSE
     SET DEVICE TO SCREEN
     SET PRINTER TO
     RETURN
   ENDIF

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
   CREATE VIEW CORE.VIW
   SAVE TO "core.fox"

   DO PopError WITH "El sistema acaba de detectar un error interno. Avise al  rea de Sistemas." + CHR(13) + CHR(13) + ;
      "Tome nota de la siguiente descripci¢n:" + CHR(13) + ;
      "Programa: " + ALLTRIM( Prg_Error ) + CHR(13) + ;
      "L¡nea   : " + ALLTRIM( Lin_Error ) + CHR(13) + ;
      "Error   : " + ALLTRIM( Des_Error )

   IF WORKER
      ON ERROR
      CANCEL
   ENDIF
*  QUIT
ENDCASE
RETURN

FUNCTION letras
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
  wletras = ALLTRIM(wletras) + ' Y  ' + STR(centavos*100,2) + '/100 ' + simbolo
 *wletras = ALLTRIM(wletras) + ' Y  ' + PADL(ALLTRIM(STR(centavos*100,2)),'0',2) + '/100 ' + simbolo

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



FUNCTION pticker
*---------------
  * Prints a status bar on the screen
  STORE SET("CONSOLE") TO setcon
  STORE SET("PRINTER") TO setprn
  SET CONSOLE ON
  SET PRINTER OFF
  @ 00,00 SAY SUBSTR(ticker_str,seg,77)
  IF setcon = 'OFF'
    SET CONSOLE OFF
  ENDIF
  IF setprn = 'ON'
    SET PRINTER ON
  ENDIF
  STORE IIF(seg=11,1,seg + 1) TO seg
  RETURN ''


FUNCTION ready2pr
*----------------
  DO WHILE .NOT. PRINTSTATUS()
    IF .NOT. YESNO('La Impresora est  apagada.¨ Continua ?')
      RETURN .f.
    ENDIF
  ENDDO
  RETURN .t.


PROCEDURE STANDBY
*----------------
  PARAMETERS msgwords,posi,posc

  IF !WEXIST('standby')
     DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5
  ENDIF

  IF PARAMETERS()>1
     posi = IIF(posi > 19, 19, posi)
     colp = 27
     IF PARAMETERS()>2
        colp = IIF(posc>28,29,posc)
     ENDIF
     MOVE WINDOW standby    TO posi,colp
  ENDIF

  ACTIVATE WINDOW standby in screen

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



FUNCTION wintitle
*----------------
  PARAMETERS ctitle
  STORE WCOLS() TO clen
  STORE INT((clen - LEN(ctitle))/2) TO padding
  STORE SPACE(padding) + ctitle + SPACE((clen-LEN(ctitle))-padding) TO padtitle
  RETURN padtitle


PROCEDURE msg2user
*-----------------
  PARAMETERS message1, message2, message3
  IF message1 = 'CLOSE'
    DEACTIVATE WINDOW msg2user
  ELSE
    ACTIVATE WINDOW msg2user
    CLEAR
    @ 00,00 SAY SPACE(1) + LEFT(message1 + SPACE(71),71) + SPACE(1)
    @ 01,00 SAY SPACE(1) ;
    + LEFT(IIF(PARAMETERS() < 2, SPACE(71), message2) + SPACE(71),71) + SPACE(1)
    @ 02,00 SAY SPACE(1) ;
    + LEFT(IIF(PARAMETERS() < 3, SPACE(71), message3) + SPACE(71),71) + SPACE(1)
  ENDIF
  RETURN


FUNCTION Parametro
*-----------------
  PARAMETERS vTipo,vKey
  private mAlias
  mAlias = ALIAS()
  SELECT Parma
  SEEK vTipo+vKey
  IF .NOT. FOUND()
    vNombre = ' '
  ELSE
    vNombre = Descri
  ENDIF
  SELECT (mAlias)
  RETURN (vNombre)

function valtod
*--------------
parameters _toda, _x,_y
private _toda, _x, _y
if _toda = 1
   @ _x,_y say space(40)
endif
return .t.

FUNCTION Fecha
*-------------
  PARAMETERS vFecha
IF EMPTY(vfecha)
	vfecha = DATE()
ENDIF
vnumero = MONTH(VFECHA)
  DO CASE
    CASE vNumero = 1
      vMes = 'Enero'
    CASE vNumero = 2
      vMes = 'Febrero'
    CASE vNumero = 3
      vMes = 'Marzo'
    CASE vNumero = 4
      vMes = 'Abril'
    CASE vNumero = 5
      vMes = 'Mayo'
    CASE vNumero = 6
      vMes = 'Junio'
    CASE vNumero = 7
      vMes = 'Julio'
    CASE vNumero = 8
      vMes = 'Agosto'
    CASE vNumero = 9
      vMes = 'Setiembre'
    CASE vNumero = 10
      vMes = 'Octubre'
    CASE vNumero = 11
      vMes = 'Noviembre'
    CASE vNumero = 12
      vMes = 'Diciembre'
ENDCASE
vDia = STR(DAY(vFecha),2)
vAno = TRANSFORM(YEAR(vFecha),'9,999')
vFFecha = 'Piura, '+vDia+' de '+vMes+' de '+vAno
RETURN (vFFecha)


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

 FOR I=0 TO 39 step -1
   @ 00,I    SAY  C                     COLOR SCHEME  c_borde
   @ 00,79-I SAY  LEFT( Rotulo1,I+1 )   COLOR SCHEME  c_fondo

   @ 24,79-I SAY  C                     COLOR SCHEME  c_borde
   @ 24,00   SAY  RIGHT( Rotulo2,I+1 )  COLOR SCHEME  c_fondo

   FOR Y = 0 TO Tiempo
   ENDFOR

 ENDFOR

 FOR I=40 TO 0 STEP -1    &&  -8
   @ 00,79-I SAY  SUBSTR( Rotulo1,2*(40-I),I+1)   COLOR SCHEME  c_fondo
   @ 00,I    SAY  LEFT( Rotulo1,2*(40-I))         COLOR SCHEME  c_borde
   @ 24,00   SAY  SUBSTR( Rotulo2,I,I)            COLOR SCHEME  c_fondo
   @ 24,I    SAY  RIGHT( Rotulo2,2*(40-I))        COLOR SCHEME  c_borde

   FOR Y = 0 TO Tiempo
   ENDFOR

 ENDFOR

** SET HOUR TO 24
** SET CLOCK ON

RETURN

PROCEDURE Err_input
*------------------
* window definido en objects

  ACTIVATE WINDOW Err_input
  @ 00,01 SAY 'DATO NO VALIDO'
  aa = INKEY(1,'H')
  DEACTIVATE WINDOW Err_input
  RETURN


PROCEDURE REPPRG
*---------------
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**               _PrgRpt : Programa reporteador.
PARAMETER  _PrgRpt, _Tit, _copia, _Wp
private  resul
resul = 'OK'
IF PARAMETERS()=2
  _Copia = 1
ENDIF

IF PARAMETERS()=3
  _Wp = .F.
ENDIF
_CONSO = SET("CONSOLE")
**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USER")
   DEFINE WINDOW msg2user   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW COLOR SCHEME 7
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
ACTIVATE WINDOW MSG2USER
_DEST = 'IMPRESORA'
P_FIL = SPACE(8)
_Dest1 = 1
@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _DEST PICTURE "@M Impresora,Pantalla,Archivo"
READ
IF _DEST = 'Archivo  '
	CLEAR
    @ 01,20 SAY 'Nombre del Archivo :' GET P_FIL PICTURE "NNNNNNNN" VALID !EMPTY(P_FIL)
    READ
ENDIF
P_FIL = ALLTRIM(P_FIL) + ".LST"
DEACTIVATE WINDOW MSG2USER
IF MOD(READKEY(),256)=12
   ON KEY
   SET ESCAPE OFF
   ACTIVATE SCREEN
   RETURN
ENDIF
IMPRE = (_DEST='IMPRESORA')

IF !IMPRE
   P_FIL = SYS(3)+".LST"
ENDIF

PRINTING = .T.
IF IMPRE
  IF !EMPTY(LEFT(SYS(0),18))
      IF !YESNO("IMPRIME EN IMPRESORA LOCAL ?  <NO = IMPRESORA DE RED>")
      **-- Impresora de red.
         SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
         SET PRINTER TO \\SPOOLER\NB
      ENDIF
      IF !READY2PR()
         PRINTING = .F.
      ENDIF
   ENDIF
ENDIF

IF PRINTING
   **-- VENTANA DE GENERACION DE REPORTE
   DEFINE WINDOW _REPO FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _Tit ;
   FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
   ACTIVATE WINDOW _REPO
   CLEAR

   **-- SI SE IMPRIME O VISUALIZA
   IF IMPRE
      DO &_PrgRpt WITH 1
   ELSE
      @ 02,20  SAY  '** Reporte en ejecuci¢n **'

      DO &_PrgRpt WITH 2

      IF _Wp
        ACTIVATE SCREEN
        RESTORE SCREEN FROM Pantalla
        RUN FOXSWAP WP &P_FIL
      ELSE
        MODIFY COMMAND (P_FIL) NOEDIT WINDOW _REPO
      ENDIF
      IF resul = 'OK'
         IF yesno("¨ Imprime el Reporte ?")
            FOR v = 1 TO _copia
               IF READY2PR()
                  !TYPE &P_FIL>PRN
               ENDIF
            ENDFOR
         ENDIF
      ELSE
         resul = 'OK'
      ENDIF
      ERASE (P_FIL)
   ENDIF
   DEACTIVATE WINDOW _REPO
ELSE
   DO STANDBY WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF
ACTIVATE SCREEN

RETURN

PROCEDURE REPORTE
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

**-- Verifica Existencia de windows

DEFINE WINDOW msg2use   FROM 07,02 TO 11,77 DOUBLE FLOAT SHADOW

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
   @ 1,5 SAY "Copias : " GET Num_C VALID(Num_C>0) PICTURE "99"
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
   DEFINE WINDOW _xyx FROM 15,40 TO 18,75 COLOR SCHEME 10 TITLE " Rango de P ginas "
   ACTIVATE WINDOW _xyx
   CLEAR
   vPBPAGE = 1
   vPEPAGE = 32767
   @ 0,5 SAY "Inicio : " GET vPBPAGE PICTURE "99,999" VALID vPBPAGE <= vPEPAGE
   @ 1,5 SAY "   Fin : " GET vPEPAGE PICTURE "99,999" VALID vPBPAGE <= vPEPAGE;
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


   ACTIVATE WINDOW MSG2USE
   _DEST = 'Impresora'
   TITLE = ' DESTINO DE IMPRESION '
   P_FIL = SPACE(8)
   _Dest1 = 1
   @ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora/Archivo' GET _DEST PICTURE "@M Impresora,Pantalla,Archivo"
   READ
   IF _DEST = 'Archivo  '
      CLEAR
      @ 01,20 SAY 'Nombre del Archivo :' GET P_FIL PICTURE "NNNNNNNN" VALID !EMPTY(P_FIL)
      READ
   ENDIF
   P_FIL = ALLTRIM(P_FIL) + ".LST"
   RELEASE WINDOW MSG2USE
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
   DEFINE WINDOW _REPO FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _Tit
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
           REPORT FORM &_Form ENVIRONMENT NOEJECT TO PRI
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

*   SS = INKEY(10)
   DEACTIVATE WINDOW _REPO
 ELSE
   DO STANDBY WITH 'EL REPORTE HA SIDO CANCELADO.'
 ENDIF
 RELEASE WINDOW MSG2USE
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


PROCEDURE XREPORTE
*----------------
** Programa   : REPORTE.PRG
** Descripcion: Impresion de reportes utilizando archivos de reporte.
** Par metros :
**               _Tipo      : tipo de rpeorte (1=con ambiente, 2=sin ambiente,3= Con Condici¢n)
**               _Form      : archivo de reporte
**               _Tit       : t¡tulo de la ventana
**               _Condicion : condici¢n de los registros
PARAMETER _tipo,_Form,_Tit,_Condicion, _wp

**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO PRINTING

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USER")
   DEFINE WINDOW msg2user   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW COLOR SCHEME 7
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
ACTIVATE WINDOW MSG2USER
_DEST = 'Impresora'
@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora' GET _DEST PICTURE "@M Impresora,Pantalla"
READ
DEACTIVATE WINDOW MSG2USER

IF MOD(READKEY(),256)=12
   ACTIVATE SCREEN
   RETURN
ENDIF
IMPRE = (_DEST='Impresora')
IF !IMPRE
   P_FIL = SYS(3)+".LST"
ENDIF

PRINTING = .T.

IF IMPRE
   IF !EMPTY(LEFT(SYS(0),18))
      IF YESNO("¨ Imprime en RED ?")
      **-- Impresora de red.
         SET PRINTER TO \\IBM_PC\PRINTQ_0=LPT1
         SET PRINTER TO \\SPOOLER\NB
      ENDIF
      IF !READY2PR()
         PRINTING = .F.
      ENDIF
   ENDIF
ENDIF

IF PRINTING

   **-- VENTANA DE GENERACION DE REPORTE
   DEFINE WINDOW _REPO FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _Tit;
   FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
   ACTIVATE WINDOW _REPO
   CLEAR

   **-- SI SE IMPRIME O VISUALIZA
   IF IMPRE
      DO CASE
        CASE _Tipo = 1
          REPORT FORM &_Form ENVIRONMENT NOEJECT TO PRINT
        CASE _Tipo = 2
          REPORT FORM &_Form NOEJECT TO PRINT
        CASE _Tipo = 3
          REPORT FORM &_Form WHILE &_Condicion NOEJECT TO PRINT
          ** REPORT FORM &_Form FOR &_Condicion NOEJECT TO PRINT
      ENDCASE
      SET PRINTER TO           && Si era la impresora server, libera el spool
   ELSE
      DO CASE
        CASE _Tipo = 1
          REPORT FORM &_Form ENVIRONMENT TO FILE (P_FIL) NOEJECT
        CASE _Tipo = 2
          REPORT FORM &_Form TO FILE (P_FIL) NOEJECT
        CASE _Tipo = 3
          REPORT FORM &_Form WHILE &_Condicion TO FILE (P_FIL) NOEJECT
          ** REPORT FORM &_Form FOR &_Condicion TO FILE (P_FIL) NOEJECT
      ENDCASE
      MODIFY COMMAND (P_FIL) NOEDIT WINDOW _REPO
      IF !_WP
         IF yesno("¨ Imprime el Reporte ?")
            IF READY2PR()
              !TYPE &P_FIL>PRN
            ENDIF
         ENDIF
      ELSE
          !FOXSWAP WP &p_fil
      ENDIF
      ERASE (P_FIL)
   ENDIF
   DEACTIVATE WINDOW _REPO
   RELEASE WINDOW _REPO
ELSE
   DO STANDBY WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF

IF _TIPO = 1
   CLOSE DATA
ENDIF

ACTIVATE SCREEN

RETURN


FUNCTION Val_para
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
      mLong = 40
      mDist = 6
    CASE PARAMETERS() = 5
      mDist = 6
  ENDCASE
  mAlias  = ALIAS()

  SELECT Parma
  SEEK Filtro+mValor
  IF !FOUND() .AND. !mVariable $'VZ'
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
	   return	
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD LEFT(CODIGO,4)+' '+SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = allt(Parma.Codigo)
  mCuenta= ALLTRIM(Parma.DescriAu2)
  mCodAux= ALLTRIM(Parma.Codigoaux)
  mDescr = SUBSTR( Parma.Descri, 1, mLong )
  mDescriAux = SUBSTR( Parma.DescriAux, 1, mLong)
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable==' '   && En edici¢n
      @ ROW(),mCol       SAY LEFT(mValor,3)
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable=='S'   && En edici¢n
      @ ROW(),mCol       SAY mCodAux
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable=='A'   && En edici¢n S
     @ ROW(),mCol SAY mDescr
      RETURN mCodAux
*     RETURN ' '
    CASE mVariable=='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable=='D'   && En vista
      RETURN LEFT(mvalor,3)+SPACE(5)+mDescr
    CASE mVariable=='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable=='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE

FUNCTION Val_Para1
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
 SET ORDE TO PARMAE1
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

FUNCTION Val_Part
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
  SELECT itepar
  SEEK Filtro+mValor
	
  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO periodo+UNIGES+UNIEJE+codcad+codfte = Filtro
      ENDIF
      GO TOP
      IF EOF()
      	set filter to
      ENDIF

      IF EOF()
         DO STANDBY WITH 'No existen partidas definidas'
         SET FILTER TO
         sele (malias)
         return &&.f.
      endif
      DEFINE POPUP parametro FROM 11,40 PROMPT FIELD CODPART+' '+str(valpart,10,2) TITLE 'Partida Asignado' 
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = ITEPAR.Codpart
 mDescr = VAL_PARA(RIGH(mvalor,2),'ESPGAS','D')
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
      RETURN .T.
  ENDCASE

FUNCTION Val_Part1
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
  SELECT itepar
  SEEK Filtro+mValor
  IF !FOUND() AND !mVariable $'VZ'
  	  
  	  IF len(allT(mValor))=6
  	  	
		 V1=VAL_PARA(left(mValor,1),'CATGAS','C')
	 	 V2=VAL_PARA(SUBS(mValor,2,1),'GRUGEN','C')
		 V3=VAL_PARA(SUBS(mValor,3,2),'MODAPL','C')
		 V4=VAL_PARA(RIGHT(mValor,2),'ESPGAS','C')
		
		 IF V1 AND V2 AND V3 AND V4
		 	IF YESNO('Partida no Presupuestada .. ¨ Deseas Ingresarla ?')
			ELSE
				SELECT (mAlias)
				RETURN		 	
			ENDIF 	
		 ELSE
		 	DO standby 	WITH 'Existe un error en la partida a asignar...verifique'
			SELECT (mAlias)
			RETURN		 	
		 ENDIF	
		 SELECT presu
		 SEEK FILTRO
		 IF !FOUND()
 		 	IF f_appd()
  	  	 		REPLACE periodo WITH LEFT(filtro,2) ,;
  	  	 				uniges  WITH SUBS(filtro,3,2),;
  	  	 				unieje  WITH SUBS(filtro,5,3),;
  	  	 		        codcad  WITH SUBS(filtro,8,4),;
  	  	 		        codfte  WITH RIGHT(filtro,2),;
  	  	 		        estado  WITH '00',;
  	  	 		        fecemi  WITH m.fecha
  	  	 	ENDIF	
  	  	 ENDIF
  	  	 SELECT itepar
  	  	 IF f_appd()
  	  	 	vestfun = maepre.uniges+maepre.unieje+maepre.codfun+maepre.codprg+maepre.codspr+maepre.actpry+maepre.codcom+maepre.codmet+maepre.codfin
  	  	 	REPLACE periodo WITH LEFT(filtro,2) ,;
  	  	 			uniges  WITH SUBS(filtro,3,2),;
  	  	 			unieje  WITH SUBS(filtro,5,3),;
  	  	 		    codcad  WITH SUBS(filtro,8,4),;
  	  	 		    codfte  WITH RIGHT(filtro,2),;
  	  	 			catgas  WITH left(mValor,1) ,;
	 	 			grugen  WITH SUBS(mValor,2,1),;
		 			modapl  WITH SUBS(mValor,3,2),;
		 			espgas  WITH RIGHT(mValor,2),;
		 			codpart WITH mValor,;
		 			estfun  WITH vestfun
  	  	 ENDIF

  	  ELSE
      	IF !EMPTY(FILTRO)
         	SET FILTER TO periodo+uniges+unieje+codcad+codfte = Filtro
      	ENDIF
      	GO TOP
      	IF EOF()
      		DEACTIVATE WINDOWS standby
         	DO STANDBY WITH 'No existen partidas definidas'
         	SET FILTER TO
         	sele (malias)
         	return &&.f.
      	endif
      	DEFINE POPUP parametro FROM 11,40 PROMPT FIELD CODPART+' '+str((valpart+cresup+tra001+tra003+tra004+tra005),10,2) TITLE 'Partida Asignado' 
      	ON SELECTION POPUP parametro DEACTIVATE POPUP
      	ACTIVATE POPUP parametro
      	RELEASE POPUP parametro
      	SET FILTER TO
      ENDIF	
 ENDIF
 mValor = ITEPAR.Codpart
 mDescr = VAL_PARA(RIGH(mvalor,2),'ESPGAS','D')
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
      RETURN .T.
  ENDCASE

FUNCTION VAL_PRV
*---------------
parameter xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
private medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (parameters()>=2)
mmsg   = (parameters()=4) .and.  _tipo

_campo = varread()

malias = alias()
select promae
_oldwnd = woutput()

if !medita
   seek xcod
   v_fun = iif(found(),nompro,"")
else
   if empty(xcod)
      set orde to 2
      on key label ENTER keyboard chr(23)
      on key label F2  do funbus
      define window _xx from 3,22 to 22,77
      browse window _xx title ' ®Enter¯  Selecciona     ®F2¯ Busca ' nolgrid noedit noappend nodelete nomenu fields;
         codprv   :h='C¢digo'     ,;
         nompro   :h='Nombre':25  ,;
         dirpro   :h='Direccci¢n' :25
      on key label ENTER
      on key label f2
      release window _xx
      set order to 1
      if !empty(_oldwnd)
         activate window &_oldwnd
      endif

      if lastkey()=27
         v_fun = .f.
      else
         xcod = codprv
         if mmsg
            @ _x,_y say nompro
         endif
         select (malias)
         if !_tipo
            replace &_campo with  xcod
         endif
         v_fun = .t.
      endif
   else
      seek xcod
      if mmsg .and. found()
         @ _x,_y say nompro
      endif
      v_fun = found()
   endif
endif
select (malias)
return v_fun

FUNCTION Val_Art    && Articulos
*---------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo

_campo = VARREAD()

mAlias = ALIAS()
SELECT Produ
_OldWnd = WOUTPUT()

IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF EMPTY(xcod)
      SET ORDER TO 1
      SET FILTER TO SUBSTR(CODART,2,2)=substr(alltrim(parma.codigo),2,2)  &&AND LEFT(CODART,1)='B'
      ACTIVATE WINDOW STANDBY
      @ 1,14 SAY 'Espere un Momento ....' color W*
      GO TOP

      IF EOF()
         DEACTIVATE WINDOW STANDBY
         ACTIVATE SCREEN
         SET FILTER TO
         v_fun = .f.
      ELSE

         DEACTIVATE WINDOW STANDBY
         ACTIVATE SCREEN
         ON KEY LABEL F10 KEYBOARD CHR(23)
         ON KEY LABEL F8 DO BorArt
         ON KEY LABEL F5 DO AgrArt
         ON KEY LABEL F2 DO FunBus
         DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
         BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
                CodArt   :H='C¢digo'    :w=EMPTY(SUBSTR(CODART,5,3)),;
                Descri   :H='Nombre':60 :w=empty(descri)
         ON KEY LABEL F10
         ON KEY LABEL F8
         ON KEY LABEL F5
         ON KEY LABEL F2
         RELEASE WINDOW _BusArt
         SET ORDE TO 1
         SET FILTER TO
         IF !EMPTY(_OldWnd)
            ACTIVATE WINDOW &_OldWnd
         ENDIF

         IF Lastkey()=27
            V_FUN = .f.
         ELSE
            xcod = CodArt
            IF mMsg
               @ _x,_y SAY Descri
            ENDIF
            SELECT (mAlias)
            IF !_tipo
               REPLACE &_campo WITH  xcod
            ENDIF
            v_fun = .T.
        ENDIF
      ENDIF
   ELSE
      SEEK xcod
      IF mMsg .AND. FOUND()
         @ _x,_y SAY Descri
      ENDIF
      v_fun = FOUND()
   ENDIF
ENDIF
SELECT (mAlias)
RETURN v_fun

FUNCTION Val_ArtDet  && Articulos
*------------------
PARAMETER xcod,_tipo,_x,_y
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo
mEdita = (parameters()>=2)
mMsg   = (parameters()=4) .and.  _tipo

_campo = VARREAD()

mAlias = ALIAS()
SELECT IteArt
_OldWnd = WOUTPUT()

IF !mEdita
   SEEK xcod
   v_fun = IIF(FOUND(),Descri,"")
ELSE
   IF !EMPTY(xcod)
      SET ORDER TO 1
      SEEK 'B'+XCOD
      IF !FOUND()
          if !yesno('Desea registrar un producto?')
             UNLOCK ALL
             SELECT (mAlias)
             v_fun = .f.
             return v_fun
          else
             do agredet
          endif
      ENDIF
      ACTIVATE SCREEN
      ON KEY LABEL F10 KEYBOARD CHR(23)
      ON KEY LABEL F8 DO BorrDet
      ON KEY LABEL F5 DO AgreDet
      ON KEY LABEL F2 DO FunBusDet
      DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
      BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU key 'B'+xcod FIELDS;
        CodArt   :H='C¢digo'     :w=EMPTY(SUBSTR(CODART,8,3)),;
        Descri   :H='Nombre':50  :w=empty(descri) ,;
        CodUni   :H='Unidad':7   :w=empty(CodUni)
*       CodPart  :H='Partd':5  :w=empty(CodPart)
      ON KEY LABEL F10
      ON KEY LABEL F8
      ON KEY LABEL F5
      ON KEY LABEL F2
      RELEASE WINDOW _BusArt
      SET ORDER TO 1
      IF !EMPTY(_OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF
      IF Lastkey()=27
         V_FUN = .f.
      ELSE
         xcod = CodArt
         IF mMsg
            @ _x,_y SAY Descri
         ENDIF
         SELECT (mAlias)
         IF !_tipo
            REPLACE &_campo WITH  xcod
         ENDIF
         v_fun = .T.
      ENDIF
   ENDIF
ENDIF
SELECT (mAlias)
UNLOCK ALL
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


FUNCTION Val_Fun
*---------------
PARAMETER v__al,v__dev,v__bus,v__cod,v__tipo,v__x,v__y
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo, mVali, mRec
* Alias :     v__al
* Var.Devol.: v__dev
* Var.Mostr.: v__bus
* Var.Modif.: v__cod
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y
mEdita = (parameters()>=5) .and.  (v__tipo # 3)
mMsg   = (parameters()=7)  .and.  (v__tipo # 3)
mVali  = (parameters()=5)  .and.  (v__tipo = 3)
mRec   = IIF(EOF(),-1,RECNO())
mAlias = ALIAS()
IF mEdita
   _campo = TRIM(mAlias)+'.'+IIF(v__tipo >1, VARREAD(), v__dev)
ENDIF

SELECT &v__al
_OldWnd = WOUTPUT()

IF !mEdita
   SEEK v__cod
   v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__bus,""))
ELSE
   IF EMPTY(IIF(v__tipo#2,v__cod,&_campo))
      GO TOP
      IF EOF()
         DO standby WITH "²²²²²²²² NO HAY VALORES PARA ELEGIR ²²²²²²²²"
         v_fun = .F.
      ELSE
         IF (v__tipo#2)
            ACTIVATE SCREEN
         ENDIF
         DEFINE POPUP v__xx FROM 2,40 TO 17,79 PROMPT FIELD &v__bus
         ON SELECTION POPUP v__xx DEACTIVATE POPUP
         ACTIVATE POPUP v__xx
         RELEASE POPUP v__xx

         IF !EMPTY(_OldWnd) .AND. v__tipo#2
            ACTIVATE WINDOW &_OldWnd
         ENDIF
         IF Lastkey()=27
            v_fun = .f.
         ELSE
            v__cod = &v__dev
            IF mMsg
               @ v__x,v__y SAY &v__bus
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
               DO standby WITH "²²²²²²²²²² NO HAY ARCHIVO ABIERTO ²²²²²²²²²²"
            ENDIF
         ENDIF
      ENDIF
   ELSE
      SEEK IIF(v__tipo#2,v__cod,&_campo)
      IF mMsg .AND. FOUND()
         @ v__x,v__y SAY &v__bus
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


FUNCTION FUNBUS
*--------------
 IF escolor
   DEFINE POPUP _MM FROM 16,54 SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP _MM FROM 16,54 COLOR SCHEME C_POPUP
 ENDIF

 DEFINE BAR 1 OF _MM PROMPT ' Busqueda por \<C¢digo '
 DEFINE BAR 2 OF _MM PROMPT ' Busqueda por \<Nombre '
 ON SELECTION POPUP _MM DEACTIVATE POPUP
 ORDEN = ORDER()
  ACTIVATE SCREEN
  ACTIVATE POPUP _MM
  DO CASE
    CASE BAR()=1
      ACTIVATE WINDOW _FUNBUS
      _Cod = '000.000'
      @01,02 SAY 'C¢digo: ' GET _Cod PICTURE '!!!.!!!'
      READ
      DEACTIVATE WINDOW _FUNBUS
      IF LASTKEY() # 27
        SET ORDER TO 1
        SEEK ALLTRIM(_Cod)
      ENDIF
    CASE BAR()=2
      ACTIVATE WINDOW _FUNBUS
      _Cod = SPACE(40)
      @01,02 SAY 'Nombre: ' GET _Cod PICTURE '@S30'
      READ
      DEACTIVATE WINDOW _FUNBUS
      IF LASTKEY() # 27
        SET ORDER TO 2
        SEEK UPPER( ALLTRIM(_Cod) )
      ENDIF
    OTHERWISE
      RELEASE POPUP _MM
      SET ORDER TO (ORDEN)
      RETURN
 ENDCASE
 RELEASE POPUP _MM
 SET ORDER TO (ORDEN)
 RETURN

FUNCTION FUNBUSDET
*-----------------
 IF escolor
   DEFINE POPUP _MM FROM 16,54 SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP _MM FROM 16,54 COLOR SCHEME C_POPUP
 ENDIF

 DEFINE BAR 1 OF _MM PROMPT ' Busqueda por \<C¢digo '
 DEFINE BAR 2 OF _MM PROMPT ' Busqueda por \<Nombre '
 ON SELECTION POPUP _MM DEACTIVATE POPUP
 ORDEN = ORDER()
  ACTIVATE SCREEN
  ACTIVATE POPUP _MM
  DO CASE
    CASE BAR()=1
      ACTIVATE WINDOW _FUNBUS
      _Cod = '00.000.000'
      @01,02 SAY 'C¢digo: ' GET _Cod PICTURE '!!.!!!.!!!'
      READ
      DEACTIVATE WINDOW _FUNBUS
      IF LASTKEY() # 27
        SET ORDER TO 1
        SEEK ALLTRIM(_Cod)
      ENDIF
    CASE BAR()=2
      ACTIVATE WINDOW _FUNBUS
      _Cod = SPACE(40)
      @01,02 SAY 'Nombre: ' GET _Cod PICTURE '@S30'
      READ
      DEACTIVATE WINDOW _FUNBUS
      IF LASTKEY() # 27
        SET ORDER TO 2
        SEEK UPPER( ALLTRIM(_Cod) )
      ENDIF
    OTHERWISE
      RELEASE POPUP _MM
      SET ORDER TO (ORDEN)
      RETURN
 ENDCASE
 RELEASE POPUP _MM
 SET ORDER TO (ORDEN)
 RETURN


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

PROCEDURE CLRSCR
*---------------
* Autor: C‚sar Ch varry Arias
* Borra la pantalla desde el centro
*
PRIVATE fil,col
fil = 11
col = 39
DO WHILE (col > 0) .AND. (fil > 0)
  @ fil,col CLEAR TO 23-fil,79-col
  fil = fil - 1
  col = col - 3
  = INKEY(.001,'H')
ENDDO
CLEAR
RETURN

PROCEDURE CLRSCR1
*---------------
* Autor: Luis Castillo Duran
* Borra la pantalla desde el centro
*
PRIVATE fil,col
fil = 0
col = 39
col1= 40
DO WHILE (col > 0)
  @ fil,col  CLEAR TO 24,col
  @ fil,col1 CLEAR TO 24,col1
  col = col - 1
  col1= col1+ 1
  FOR Y=1 TO 150
  NEXT Y
ENDDO
CLEAR
RETURN

FUNCTION poperror
*----------------
PARAMETERS err_mess
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

***  paint the message on the screen line by line

IF num_lines = 1
  _mens = PADC(err_mess,48)
  FOR j = 1 TO LEN(_mens)
    @ 1,1+j SAY SUBSTR(_mens,j,1)
   *=INKEY(0.001,' ')
  ENDFOR
  i = 2
ELSE
  FOR i = 1 TO num_lines
    say_mess = LTRIM( MLINE(err_mess,i) )
    * @ i,02 SAY LTRIM(say_mess)
    FOR j = 1 TO LEN(say_mess)
       @ i,1+j SAY SUBSTR( say_mess,j,1)
      *=INKEY(0.001,' ')
    ENDFOR

  ENDFOR
ENDIF

***  draw a line across the bottom of the message and
***  paint "press any key..."

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

FUNCTION password
*----------------
PARAMETERS pw_len
PRIVATE pw_len, pass_str, key, cur_curs

*PUSH KEY CLEAR

IF parameters() = 0           &&  if no parameters passed
  pw_len = 6                  &&  use password length of 6
ENDIF
IF pw_len <= 0                &&  Valida la longitud del password
   pw_len = 6
ELSE
  IF pw_len > 30
    pw_len =   30
  ENDIF
ENDIF
pass_str = ''
key = 0

cur_curs = set('cursor') = 'ON'   &&  current cursor state
SET CURSOR on
cur_win = woutput()

DEFINE WINDOW pass_win FROM 08,20 TO 15,60 DOUBLE ;
              COLOR SCHEME 21
ACTIVATE WINDOW pass_win

@ 02,09 SAY 'Ingrese su password'
@ 03,12 SAY '(' + ltrim(str(pw_len)) + ' caracteres)'
@ 05,15 SAY ''

DO WHILE m.key # 13 .AND. m.Key#27             &&  accept until car_return
  SET COLOR OF NORMAL TO x
  m.key = inkey(0)

  DO CASE
    CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122)
      pass_str = pass_str + chr(m.key)
    CASE m.key = 19 .or. m.key = 127 .or. m.key = 7
      @ row(),col()-1 SAY ' '
      @ row(),col()-1 SAY ''
      pass_str = substr(pass_str,1,len(pass_str)-1)
    OTHERWISE
   ENDCASE

  IF len(pass_str) >= pw_len
    m.key = 13
  ENDIF

  SET COLOR OF NORMAL TO
  @ 05,15 SAY space(pw_len)
  @ 05,15 SAY replicate('',len(pass_str))
ENDDO

RELEASE WINDOW pass_win

IF empty(cur_win)
  ACTIVATE SCREEN
ENDIF

IF .not. cur_curs             &&  if cursor was off
  SET CURSOR OFF              &&  set it back on
ENDIF

RETURN(pass_str)

FUNCTION Val_TC
*--------------
PARAMETER vFecha,vRetVal

mAlias  = ALIAS()
SELECT tipcam
GO TOP
IF EOF()
   DO standby WITH 'No existen tipos de cambio registrados'
   SELECT IIF(EMPTY(mAlias),0,(mAlias))
   RETURN
ENDIF
SEEK DTOC(vFecha,1)

_OldWnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL ENTER  DO Sel_TC
ON KEY LABEL F10    DO Sel_TC
**ON KEY LABEL LEFTARROW   KEYBOARD CHR(15)   && SALTA CAMPO PARA ATRAS
ON KEY LABEL RIGHTARROW  KEYBOARD CHR(9)    && TAB:

DEFINE WINDOW _winTC FROM 05,32 TO 13,78  COLOR SCHEME 8;
  TITLE ' Para seleccionar presione   Enter  ¢  F10 '
BROWSE NOEDIT NOAPPEND NODELETE   WINDOW  _winTC FIELDS ;
     fecha   :W=.F.,;
     compar  :H='Compra ML'  :P='9,999.99',;
     venpar  :H='Venta ML'  :P='9,999.99',;
     combco  :H='Compra Bco' :P='9,999.99',;
     venbco  :H='Venta Bco' :P='9,999.99'
**     moneda  :W=.F.  :H='',;

ON KEY


IF !EMPTY(_OldWnd)
   ACTIVATE WINDOW &_OldWnd
ENDIF

IF SUBSTR(VERSION(),12)="2.0"
   SELECT IIF(EMPTY(mAlias),SELECT(0),(mAlias))
ELSE
   IF !EMPTY(mAlias)
      SELECT (mAlias)
   ENDIF
ENDIF
RETURN


PROCEDURE Sel_TC
*---------------
  ON KEY
  vcampo  = VARREAD()
  vRetVal = &vcampo
  KEYBOARD CHR(23)
RETURN


PROCEDURE Fox_ambi
*-----------------
* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º                                                                     º
* º         AMBIENTE DE TRABAJO DEL SISTEMA DE ANSHIN                   º
* º                                                                     º
* º                                                                     º
* º  SAIKI Consultores   Av. Arequipa 1130 Dpto 02  Telf.  709612       º
* º                                                                     º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

* COMPLETE LIST OF SET COMMANDS

* Procedimineto general para los errores de entrada.
ON READERROR         DO   Err_input
*

SET ALTERNATE        OFF  && Default
SET ALTERNATE TO          && Default
SET AUTOSAVE         OFF
SET BELL             ON   && Default
SET BLOCKSIZE TO     33   && Default
SET BLINK            ON   && Allows use of bright backgrounds
SET BORDER TO        SINGLE
* SET CARRY               && NOT RELEVANT TO SYSTEM ENVIRONMENT
* SET CARRY TO            && NOT RELEVANT TO SYSTEM ENVIRONMENT
SET CENTURY          ON  && Default
SET CLEAR            ON
SET CLOCK            ON   && Default
SET CLOCK TO         00,69
* SET COLOR               && ALL COLOR SETTINGS HANDLED IN COLORSET.PRG
SET COMPATIBLE       OFF  && Default
SET CONFIRM          ON
SET CONSOLE          ON   && Default
SET CURRENCY         LEFT && Default
SET DATE             BRITISH  && Default
IF !worker
  SET DEBUG          OFF  && Default
ELSE
  * THE WORKER VARIABLE CONTROLS HOW VARIOUS SETTINGS ARE HANDLED THAT ARE
  * HELPFUL DURING PROGRAM DEVELOPMENT. EXAMPLES: ESCAPE, DEVELOPMENT, ETC.
  * Here's some examples of how you can use the WORKER environmental and
  * FoxPro variables for your convenience.
  * SET DISPLAY TO     EGA43
  * SET DEBUG          ON
  * SET STEP ON
ENDIF
SET DECIMALS TO      2   && Default
SET DEFAULT TO           && LEAVE SET TO CURRENT DEFAULT DRIVE
SET DELETED          ON
IF worker
  SET DEVELOPMENT    ON
ELSE
  SET DEVELOPMENT    OFF && Default
ENDIF
SET DEVICE TO        SCREEN && Default
*SET DISPLAY         && ACCEPT DEFAULT, WHICH MEANS 25 LINE MODE COLOR OR MONO
SET DOHISTORY        OFF  && Default
SET ECHO             OFF  && Default
IF worker            && TURN OFF FOR LIVE USE, ON FOR PROGRAMMING & TESTING
  SET ESCAPE         ON
ELSE
  SET ESCAPE         OFF  && Default
ENDIF
SET EXACT            OFF
SET EXCLUSIVE        OFF
SET MULTILOCKS       ON
SET FIELDS           OFF
* SET FIELDS         NOT RELEVANT TO SYSTEM ENVIRONMENT
* SET FILTER         NOT RELEVANT TO SYSTEM ENVIRONMENT
SET FIXED            ON
SET FORMAT           TO
SET FULLPATH         ON
* SET FUNCTION       && LEAVE DEFAULTS
SET FUNCTION  1 TO
SET FUNCTION  2 TO
SET FUNCTION  3 TO
SET FUNCTION  4 TO
SET FUNCTION  5 TO
SET FUNCTION  6 TO
SET FUNCTION  7 TO
SET FUNCTION  8 TO
SET FUNCTION  9 TO
SET FUNCTION 10 TO
SET HEADING          OFF
SET HELP             OFF
SET HOURS TO         24
* SET INDEX          NOT RELEVANT TO SYSTEM ENVIRONMENT
SET INSTRUCT         ON
SET INTENSITY        ON
IF worker
  SET logerror       ON
ELSE
  SET logerror       OFF
ENDIF
SET MARGIN TO        0
SET MARK TO          "-"  && THIS IS THE DATE FIELD SEPARATOR
*  THE DEFAULT IS "/"
*  WE'RE USING SOMETHING DIFFERENT HERE
SET MEMOWIDTH TO     80
SET MENU             ON
SET MESSAGE TO       23
SET notify           ON
SET NEAR             OFF
SET ODOMETER TO      1  && SET UP TO 10 OR 100 IN LARGER APPLICATIONS
* SET ORDER TO       NOT RELEVANT TO SYSTEM ENVIRONMENT
SET PATH TO          && ACCEPT DEFAULT PATHS
SET POINT TO         "."
SET PRECISION TO     3
SET PRINT            OFF
SET PRINTER TO       lpt1
* SET procedure TO   && THIS COULD BE HANDLED HERE, BUT WE PREFER NOT TO
*                       "BURY" IMPORTANT INFO LIKE THIS IN A SUBROUTINE.
* SET RELATION TO    NOT RELEVANT TO SYSTEM ENVIRONMENT
SET SAFETY           OFF
SET SCOREBOARD       OFF
SET SYSMENU          OFF
SET SEPARATOR TO     ","
SET SPACE            OFF
SET STATUS           OFF
IF .NOT. worker
  SET STEP           OFF
ENDIF
SET sticky           ON
SET TALK             OFF
SET TOPIC TO
SET TYPEAHEAD TO     0    && CLEAR AWAY ANY OVERANXIOUS INPUT!
SET TYPEAHEAD TO     25
SET UDFPARMS  TO REFERENCE
SET UNIQUE           OFF

* SET VIEW             ON &&EE
* SET WINDOW OF MEMO TO &&EE


* SET THE INSERT / CAPS / NUMLOCK KEYS
* NOTE: LOOKUP POPUPS WORK BETTER WITH INSMODE OFF, AS BELOW

= INSMODE(.F.)
= CAPSLOCK(.T.)
= NUMLOCK(.T.)


* Define working windows
DEFINE WINDOW Err_input  FROM 01,60 TO 03,77

* Define standard system communication windows
DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5
* Typical system messages:
DEFINE WINDOW msg2user   FROM 04,02 TO 08,77 DOUBLE FLOAT SHADOW

* Press any key or click mouse to continue messages:
DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5

DEFINE WINDOW _FUNBUS    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW
* VARIABLE PARA EL BLOQUEO
PUBLIC Escape
ESCAPE = 27
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
*_PDRIVER =    &&GENERIC.PR2
_pecode =     ""
_peject =     "AFTER"
_pepage =     32767
_pform =      ""
_plength =    60
* _PLINENO =  SYSTEM MAINTAINED VARIABLE
_ploffset =   0
_ppitch =     "PICA"
_pquality =   .t.
_pscode =     ""
_pspacing =   1
_pwait =      .f.
_rmargin =    80
_tabs =       "10, 20, 30, 40, 50, 60, 70"
_wrap =       .t.
RETURN



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


FUNCTION PideFec
*---------------
PARAMETERS _titu, vFecIni, vFecFin

IF PARAMETERS()=1
   RETURN .F.
ENDIF

PRIVATE _retval
_retval=.T.

ff = IIF(PARAMETERS()=2,20,21)

ACTIVATE WINDOW STANDBY

@ 00,00 SAY  PADC(_titu, WCOLS(),' ')
IF PARAMETERS()=2
   @ 01,04 SAY 'Fecha  : ' GET vFecIni  ;
                    ERROR 'Fecha Inv lida'
ELSE
   @ 01,04 SAY 'Inicio : ' GET vFecIni  ;
                    ERROR 'Fecha Inv lida'
   @ 02,04 SAY 'Final  : ' GET vFecFin  VALID vFecIni<=vFecFin ;
                    ERROR 'Fecha Inv lida'
ENDIF
READ
DEACTIVATE WINDOW STANDBY
IF LASTKEY()=27
   _retval=.F.
ENDIF
RETURN _retval


FUNCTION msgpro  && Envia mensaje de realizac. de proceso.
*--------------
PARAMETER _sw,_lin,_msg

PRIVATE _sw,_msg, vNumPara     && _sw= boolean  ; _msg= mensaje

**vBlink   = SET('BLINK')
vNumPara = PARAMETER()
_lin = IIF(vNumPara=1, 23, _lin)
IF vNumPara=1 or vNumPara=2
   IF _sw
      _msg = 'Procesando, por favor espere un momento..'
   ENDIF
ENDIF

IF _sw
   PUBLIC _oldmsg, _oldWnd
   _msg    = PADC(_msg,2+LEN(_msg),' ')
   _oldmsg = _msg
   _oldWnd = WOUTPUT()
   _pos    = (80-LEN(_oldmsg))/2
   SET BLINK ON
   IF !WEXIST('_xyz')
      DEFINE WINDOW _xyz FROM  _lin,_pos TO _lin,LEN(_oldmsg)+_pos  NONE IN SCREEN
   ENDIF
   ACTIVATE WINDOW _xyz SAME
   @ 00,00 SAY _msg  COLOR  n*/w
ELSE
   DEACTIVATE WINDOW _xyz
   RELEASE WINDOW _xyz
   SET BLINK OFF
ENDIF

IF TYPE('_OldWnd') # 'U'
   IF !EMPTY(_OldWnd)
      ACTIVATE WINDOW &_OldWnd
   ELSE
      ACTIVATE SCREEN
   ENDIF
ENDIF

RETURN


FUNCTION VAL_READ
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
* Autor: C‚sar Ch varry Arias   Fecha: 14-09-93
PRIVATE _Fun
_Fun = .T.
IF LASTKEY() # 27
  IF .NOT. YESNO('¨ Est n correctos los datos ?')
    _Fun = .F.
  ENDIF
ENDIF
RETURN (_Fun)

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

*----------------------VERSION NORMAL--------------------

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

*------------
IF VTIPO = 1

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEOC.PERIODO, ITEOC.TIPFUN, ITEOC.ESTADO, ITEOC.CODFTE, ITEOC.CODCAL, ;
	      ITEOC.VALTOT, ITEOC.CODPART,ITEOC.CODANAL ;
		  FROM ITEOC ;
		  WHERE nummes+tipfun+codfte+periodo+SUBSTR(ITEOC.codCAL,8,2)+iif(!empty(vCodsub),SUBSTR(ITEOC.CODCAL,10,3),'') + iif(!empty(vProyec),SUBSTR(ITEOC.CODCAL,13,3),'') + iif(!empty(proact),SUBSTR(ITEOC.CODCAL,16,2),'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1 AND ALLTRIM(M.TIPFUN)='F',CODPART='02',IIF(VTIPO=2 AND ALLTRIM(M.TIPFUN)='F',CODPART='03',.T.)) AND TIPFUN='I';
				INTO CURSOR ITEOCX
ELSE
   SELECT ITEOC.PERIODO, ITEOC.TIPFUN, ITEOC.ESTADO, ITEOC.CODFTE, ITEOC.CODCAL, ;
	      ITEOC.VALTOT, ITEOC.CODPART,ITEOC.CODANAL ;
		  FROM ITEOC ;
	 	  WHERE nummes+tipfun+codfte+periodo+SUBSTR(ITEOC.codCAL,8,2)+iif(!empty(vCodsub),SUBSTR(ITEOC.CODCAL,10,3),'') + iif(!empty(proact),SUBSTR(ITEOC.codCAL,13,2),'') ;
   					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='F';
					INTO CURSOR ITEOCX
ENDIF


vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 27 alias ORDEN1 EXCLU

ELSE

IF ALLTRIM(m.tipfun)='I'
   SELECT ORDSE.PERIODO, ORDSE.TIPFUN, ORDSE.ESTADO, ORDSE.CODFTE, ORDSE.CODCAL, ;
	      ORDSE.VALTOT, ORDSE.CODPART,ORDSE.CODANAL ;
		  FROM ORDSE ;
		  WHERE nummes+tipfun+codfte+periodo+SUBSTR(codCAL,8,2)+iif(!empty(vCodsub),SUBSTR(CODCAL,10,3),'') + iif(!empty(vProyec),SUBSTR(CODCAL,13,3),'') + iif(!empty(proact),SUBSTR(CODCAL,16,2),'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1 AND ALLTRIM(M.TIPFUN)='F',CODPART='02',IIF(VTIPO=2 AND ALLTRIM(M.TIPFUN)='F',CODPART='03',.T.)) AND TIPFUN='I';
				INTO CURSOR ORDSEX
ELSE
   SELECT ORDSE.PERIODO, ORDSE.TIPFUN, ORDSE.ESTADO, ORDSE.CODFTE, ORDSE.CODCAL, ;
	      ORDSE.VALTOT, ORDSE.CODPART,ORDSE.CODANAL ;
		  FROM ORDSE ;
	 	  WHERE nummes+tipfun+codfte+periodo+SUBSTR(codCAL,8,2)+iif(!empty(vCodsub),SUBSTR(CODCAL,10,3),'') + iif(!empty(proact),SUBSTR(codCAL,13,2),'') ;
   					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='F';
					INTO CURSOR ORDSEX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 27 alias ORDSE1 EXCLU

ENDIF


FUNCTION vEstPec
*---------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
	CASE vest='00'
		vfun ='Emitido      '
	CASE vest='20'
		vfun ='Con S/C      '
	CASE vest='30'
		vfun ='Con O/C      '
	CASE vest='40'
		vfun ='Despachado   '
	CASE vest='50'
		vfun ='Liquidado    '
	CASE vest='51'
		vfun ='Contabilizado'		
	CASE vest='99'
		vfun ='Anulado      '
ENDCASE
RETURN vfun

FUNCTION SavCon
*---------------
USE UsuGRAU IN 0 ALIAS Usu
PUBLIC _Hora
SELECT Usu
APPEND BLANK
_hora  = TIME()
REPLACE CodUsu  WITH vconex
REPLACE machine WITH vmaq
replace tipmaq with substr(sys(17),3)
REPLACE Prog    WITH SYS(16,1)
REPLACE Hora    WITH _hora
REPLACE dia     WITH DAY(DATE())
REPLACE LLAVE   WITH VLLAV
vUsurec = recno()
USE
RETURN

FUNCTION BorCon
*-----------------
USE UsuGRAU IN 0 ALIAS Usu
SELECT Usu
GO TOP
vPrg   = SYS(16,1)
SET EXACT ON
DO WHILE !EOF()
	LOCATE FOR  ALLT(Codusu)=vconex AND ALLT(machine) = vmaq AND LLAVE=VLLAV
		IF FOUND()
			DELETE NEXT 1
		ENDIF
	CONTINUE	
ENDDO	
SET EXACT OFF
USE
RETURN

FUNCTION EliCon
*-----------------
USE UsuGRAU IN 0 ALIAS Usu
SELECT Usu
GO vUsurec
DELETE NEXT 1
USE
RETURN

FUNCTION val_BOY
*---------------
PARAMETERS mvalor, filtro, mvariable
PRIVATE malias
malias  = ALIAS()

SELECT parma
SEEK filtro+mvalor

mvalor = parma.codigo
mdescr = parma.descri
mdescriaux = parma.descriaux

IF !EMPTY( malias )
	SELECT (malias)
ENDIF
RETURN mdescr+mdescriaux

FUNCTION GETCRE
*---------------
private vkey,valias
vAlias   = ALIAS()
vCodpart = codpart
vKey= codcad+CODFTE
************************
* CREDITOS SUPLEMENTARIOS
*************************

SELECT itecre
SET RELATION TO PERIODO+CODOPE+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC) INTO CRESUP
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
SUM monasig TO vcresup FOR Codpart=vcodpart	AND RIGHT(NUMDOC,2)<=ALLT(vCALEND) AND codcad+codfte=vkey
vtotalc=vcresup
SELECT (vAlias)
RETURN vtotalc


FUNCTION GETTRA
*---------------
private vkey,valias
vAlias   = ALIAS()
vCodpart =codpart
vKey= codcad+CODFTE
****************
* TRANSFERENCIAS
****************

SELECT itetra
SET RELATION TO PERIODO+CODOPE+ALLTRIM(TIPDOC)+ALLTRIM(NUMDOC) INTO traPAR
GO TOP
* se filtra de acuerdo a la fecha del reporte vNumMes
vtransf=0
SCAN 
	IF MONTH(fecha)<=VAL(vCalend) AND CODCAD+CODFTE=vkey AND Codpart=vCodpart
	   vtransf=vtransf+iif(tipope='-',-1,1)*montra
	ENDIF
ENDSCAN
vtotalt=vtransf
SELECT (vAlias)
RETURN vtotalt


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
    SET FILTER TO
  ENDIF
  mValor = maepre.codcad
  mDescr = SUBSTR( maepre.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable==' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable=='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable=='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable=='D'   && En vista
      RETURN mDescr
    CASE mVariable=='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable=='C'   && Solo codigo
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

FUNCTION ver_pres
*----------------
PARAMETER vkey
valias = ALIAS()
SELECT presu
SEEK vKey
IF !FOUND()
	IF !YESNO( 'Cadena y fte. no est n presupuestadas...¨ Continua ?')
		SELECT (valias)
		RETURN .F.
	ENDIF	
ENDIF
SELECT (valias)
RETURN .T.

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


function Val_busper
*------------------
PARAMETER vtipo,vorden
vfun=.T.
SET ORDER TO vorden
SEEK vtipo
IF FOUND()
	DO STANDBY WITH 'Ya existe esta '+IIF(vordeN=1,'L.E.','Cta. de Ahor.')
	vfun=.f.
ENDIF	
set order to 1	
return vfun

PROCEDURE ESPERA 
*-----------------------------------------------------------
* Esta funcion activa una pantalla de Espera 
* en caso de que se procesada a indexar o demorar el proceso
* Desarrollado por : FMC Saiki. Sucursal Piura
* Fecha : 11/01/96
*-----------------------------------------------------------
PARAMETER festado,fmensaje   
  IF !WEXIST('Espera')     
     DEFINE WINDOW Espera    FROM 12,06 TO 14,78 COLOR SCHEME 05 
  ENDIF
   DO CASE 
      CASE festado=1
	   ACTIVATE WINDOW Espera
		   @0,10 SAY fmensaje COLOR W+/BR*
		   RETURN
	  CASE festado=2
		   DEACTIVATE WINDOW Espera
		   RETURN 
	ENDCASE
RETURN


FUNCTION cMes
*------------
PARAMETER _n_mes
   PRIVATE v_fun

   IF !BETWEEN(_n_mes,1,12)
      RETURN "Inv lido"
   ENDIF
   v_fun ="Enero    Febrero  Marzo    Abril    Mayo     Junio    ;
Julio    Agosto   SetiembreOctubre  NoviembreDiciembre"

RETURN TRIM(SUBSTR(v_fun,9*(_n_mes-1)+1,9))
