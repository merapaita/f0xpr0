***ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  TES_LIB.PRG                                                        º
* º                                                                     º
* º                 LIBRERIA DEL SISTEMA SBP_P
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
**wletras = ALLTRIM(wletras) + ' Y  ' + STR(centavos*100,2) + '/100 ' + simbolo
  wletras = ALLTRIM(wletras) + ' Y  ' + PADL(ALLTRIM(STR(centavos*100,2)),2,'0') + '/100 ' + simbolo

  ll  = SUBSTR( wletras, 85, 15)
  l   = 0
  t   = 16
  r   = LEN( wletras )
  IF r > 100
    DO WHILE t <> 20
      t = t - 1
      p = SUBSTR(ll,t,1)
      IF p = ' '
	numlet1 = SUBSTR( wletras, 1, 85 ) + SUBSTR( ll, 1, T )
	numlet2 = SUBSTR( wletras, ( 86 + t ), ( r - ( 85 + t ) ) )
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
  mAlis = ALIAS()
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
	DEFINE WINDOW _xyx FROM 15,45 TO 19,70 COLOR SCHEME 10 TITLE " # DE COPIAS "
	ACTIVATE WINDOW _xyx
	CLEAR
	@ 1,5 SAY " N§ Copias : " GET num_c VALID(num_c>0) PICTURE "99"
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
				REPORT FORM &_form ENVIRONMENT NOEJECT TO print
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


PROCEDURE REPPRG    &&C.A.K.
**********************************************************************
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
   DEFINE WINDOW _VeDest FROM 07,02 TO 11,77 DOUBLE FLOAT SHADOW 
   ACTIVATE WINDOW _VeDest
   P_FIL = SPACE(8)
    @ 01,02  SAY 'Destino de Impresi¢n : Pantalla/Impresora/Archivo' GET _DEST PICTURE "@M Impresora,Pantalla,Archivo"
    READ
	IF _dest = 'Archivo  '
		CLEAR
		@ 01,20 SAY 'Nombre del Archivo :' GET p_fil PICTURE "NNNNNNNN" VALID !EMPTY(p_fil)
		READ
	ENDIF
	p_fil = ALLTRIM(p_fil) + ".LST"
	DO CASE
  		 CASE _dest= 'Pantalla '
  		      _dest1=1
  		 CASE _dest= 'Impresora'
  		      _dest1=2
  		 CASE _dest= 'Archivo  '
  		      _dest1=3
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
      mLong = 40               && Longitud campo DESCRI
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
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
*       ACTIVATE WINDOW &_OldWnd
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
  DO CASE
    CASE mVariable=' '   && En edici¢n
      *@ ROW(),mCol       SAY mValor
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
  SEEK ALLT(Filtro)+mValor
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


FUNCTION Val_paraB
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  &&  PARA BUSCAR POR CODIGO Y SI NO LO ENCUENTRA 
  &&  EL POPUP SALE ORDENADO POR DESCRIPCION Y CONDICIONAL
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
    SET FILT TO tipo=Filtro;
    	 AND IIF(codigoaux<>'X',ALLTRIM(codigoaux)=ALLTRIM(m.codfte),.T.);
   	     AND (LEFT(descriaux,1)=LEFT(m.transa,1) OR;
   	     IIF(substr(descriaux,2,1)#' ',SUBSTR(descriaux,2,1)=LEFT(m.transa,1),.F.))
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILT TO
       IF !EMPTY( mAlias )
		  SELECT (mAlias)
       ENDIF
	   RETURN       
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(Descri,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILT TO
    SET ORDER TO PARMAE1
  ENDIF
  mValor = Parma.Codigo
  mcta10 = SUBSTR(Parma.Descriaux,5,1)
  mcond  = SUBSTR(Parma.Descriaux,8,1)
  mctaf  = LEFT(Parma.descriau2,10)
  mctai  = SUBSTR(Parma.Descriau2,12,10)
  m.asiento = Parma.Nument
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



FUNCTION Val_para9 && MEF - REGION
*-----------------
  PARAMETERS mCuenta, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
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
  SEEK Filtro+mCuenta

  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    GO TOP
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(ALLTRIM(DescriAu2),1,40)+' '+ALLTRIM(DescriAux)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mCuenta= Parma.DescriAu2
  mDescr = Parma.DescriAux
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable='J'   && Solo codigo
      @ ROW(),mCol       SAY ALLTRIM(mCuenta)
      RETURN .T.
    CASE mVariable='D'   && En vista
      @ ROW(),mCol       SAY ALLTRIM(mDescr)
      RETURN .T.
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY ALLTRIM(mCuenta)
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE


FUNCTION Val_para0 && REG.GRAU - OFC.GIRADORAS
*-----------------
  PARAMETERS mCuenta, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias
  DO CASE
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
  SEEK Filtro+mCuenta
						   *IF .NOT. FOUND() .AND. mVariable<>'V'
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO Tipo = Filtro
    GO TOP
    DEFINE POPUP parametro FROM 03,30 PROMPT FIELD SUBSTR(ALLTRIM(Codigo)+' '+ALLTRIM(Descri),1,40)+' '+ALLTRIM(DescriAux)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mCuenta= Parma.Descri
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable='J'   && Solo codigo
      @ ROW(),mCol       SAY ALLTRIM(mCuenta)
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE


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
SELECT maepre                                           
SET ORDER TO IIF(ALLTRIM(m.tipfun)='I',6,7)
SEEK ALLTRIM(m.TipFun)+nfiltro+nvalor
IF !FOUND()     OR !nvariable $'V'
	SET FILTER TO periodo+codprg+codsubpr = nfiltro
	GO TOP
	IF !EOF()
		IF !EMPTY(nvalor) 
			SEEK alLTRIM(m.TipFun)+nfiltro+nvalor
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
	CASE nvariable=' '   && En edici¢n
		@ ROW(),ncol   SAY PADR(nvalor,nancho,' ')
		@ ROW(),ncol   SAY ndescr
		RETURN .T.
	CASE nvariable='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),ncol SAY ndescr
		RETURN
	CASE nvariable='V'   && En vista
		@ ROW(),COL()  SAY PADR(nvalor,nancho,' ')
		RETURN ndescr
	CASE nvariable='D'   && En vista
		RETURN ndescr
	CASE nvariable='Z'   && En vista SIN PINTAR
		RETURN ndescr
	CASE nvariable='C'   && Solo codigo
		RETURN .T.
	OTHERWISE            && En browse de edici¢n
		&nvariable = nvalor
		RETURN .T.
ENDCASE

PROCEDURE rolea
*---------------
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06,10 TO 17,69 DOUBLE FLOAT SHADOW COLOR SCHEME 10
GO TOP
SCATTER MEMVAR
DO CASE
	CASE ALLTRIM(m.tipfun) = 'I' AND !EOF() AND !EMPTY(Codproy)
		BROWSE WINDOW _xx TITLE ' PROYECTOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codproy  :H='Pry' ,;
			descri   :H='Detalle'
	CASE ALLTRIM(m.tipfun) = 'F' AND !EOF() AND !EMPTY(Codact)
		BROWSE WINDOW _xx TITLE ' ACTIVIDAD :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codact   :H='Act',;
			descri   :H='Detalle'
	OTHER
		IF !EMPTY(nvalor)
			DO standby WITH 'No se tiene '+IIF(ALLTRIM(m.tipfun)='F','Actividad','Proyecto')+' en referencia'
		ENDIF
ENDCASE
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
SET ORDER TO 8
SEEK cfiltro+cvalor
if !found() OR !cvariable $'V'
	SET FILTER TO codfte+periodo+codprg+codsubpr+codproy+codsupry = alltrim(m.codfte)+cfiltro
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

PROCEDURE ANALCAL
*----------------
PARAMETERS vCantOC,vtipO,vmues
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=Xrolea_cal(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)                , alltrim(vcodact),vcantoc,VTIPO,vmues)
ELSE	
	WDF=Xrolea_cal(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO,vmues)
ENDIF
USE IN 24
RETURN WDF


PROCEDURE Xrolea_CAL
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
   SELECT ITEHC.NUMMES, ITEHC.CODPART, itehc.estado,itehc.tipope, ITEHC.VALPART, ITEHC.CODPROY,;
	      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm;
		  FROM ITEHC ;
		  WHERE  iif(estado='92',nummeshm,nummes)+tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND ESTADO#'99' AND TIPFUN='I' AND EMPTY(NUMPA) and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.);
				INTO CURSOR Hojax
ELSE
 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
           FROM ITEHC ;
	 	   WHERE iif(estado='92',nummeshm,nummes)+tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,CODANAL='02',IIF(VTIPO=2,CODANAL='03',.T.)) AND ESTADO#'99' AND TIPFUN='F' AND EMPTY(NUMPA) and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.);
					INTO CURSOR HOJAX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 24 alias hoja1 exclu

IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ;
		  FROM CALEN ;
		  WHERE  nummes+tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I' AND ESTADO#'20';
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS ;
               FROM CALEN ;
		 	   WHERE nummes+tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='F' AND ESTADO#'20';
					INTO CURSOR CalenX
ENDIF
 
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
                    Sele hojmod
                    seek hoja1.nummeshm+hoja1.numhm
                    if found()
                       vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                    else   
                       vPor = iif(hoja1.tipope='-',-1,1) 
                    endif  
                    selec hoja1 
                    *vPor = iif(HOJA1.tipope='-',-1,1) 
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
	                 Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
     	             *vPor = iif(HOJA1.tipope='-',-1,1) 
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
                     Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
			         *vPor = iif(HOJA1.tipope='-',-1,1) 
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
           	    	VPAR = CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2)
           	    	SELE HOJA1
				    SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR CODPRG+CODSUBPR+CODACT+LEFT(CODANAL,2) = VPAR
	    		ELSE
					VPAR = CODPRG+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) 
           	    	SELE HOJA1					
					SUM IIF(TIPOPE='-',-1*VALPART,VALPART) TO VSUMA FOR CODPRG+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) = VPAR
				ENDIF    
				SELE CALEN1
				IF RLOCK()
					REPLACE TOTAFE WITH VSUMA
				ENDIF
				UNLOCK
			ENDIF
		ENDSCAN	
		GO TOP
*       if vmues
			DO VAL_CAL
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

PROCEDURE VAL_cal
*----------------
PRIVATE ALS
ALS=ALIAS()
ON KEY LABEL F7
SELEC CALEN1
GO TOP
vdbf = sys(3)+'.dbf'
copy to (vdbf)
USE (VDBF) IN 25 ALIAS CALEN1 exclu
DEACTIVATE WINDOW Standby

VDX = SYS(3)+'.IDX'
ZDX = SYS(3)+'.IDX'


IF ALLTRIM(m.tipfun)='F'
    INDEX ON CODSUBPR+CODACT+CODPART TO (VDX)
    GO TOP
    DEFINE WINDOW _yy FROM 04,03 TO 19,75 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	        XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':11 ,;
	        FRO=IIF(RIGHT(CODPART,2)='00' OR VALPART=0,CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' '),'') :H='Act':6  ,;	        
			vv=' '+Codpart  :H='Partida' ,;
			Valpart  :H='     Asignado':P='99,999,999.99',;
			Totafe             :H='     Afectado':P='99,999,999.99',;
			zz=valpart- TotAfe :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _YY			
ELSE
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


PROCEDURE XANALCAL
*----------------

IF ALLTRIM(m.tipfun)='F'
	DO rolea_cal WITH maepre.periodo+maepre.codprg+maepre.codsubpr                , maepre.codact
ELSE	
	DO rolea_cal WITH itepar.periodo+itepar.codprg+itepar.codsubpr+itepar.codproy , itepar.codsupry
ENDIF

RETURN

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
   SET FILTER TO nummes+tipfun+codfte+periodo+codprg+codsubpr + codproy + iif(!empty(proact),codsupry,' ')  = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')
ELSE
   SET FILTER TO nummes+tipfun+codfte+periodo+codprg+codsubpr + iif(!empty(proact),codact,' ') = alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')  AND LEFT(codpart,2)=iif(alias()="ORDSE",'03','02')
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
	   SET FILTER TO tipfun+codfte+periodo+codprg+codsubpr + codproy + iif(!empty(proact),codsupry,' ')  = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')
	ELSE
	   SET FILTER TO tipfun+codfte+periodo+codprg+codsubpr + iif(!empty(proact),codact,' ') =  alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,' ')  AND LEFT(codpart,2)=iif(alias()="ORDSE",'03','02')
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
     SET FILTER TO codfte+codprg+codsubpr+Tipfun = Filtro + ALLTRIM(vtipfun)
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

FUNCTION Val_Prycto
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
  vTempo =ALLTRIM(m.periodo)+ALLTRIM(m.nummes)
  if alltrim(vtipfun)='I'
     set orde to 1
  else
     set orde to 2
  endif

  Public vSale
  vSale=.t.
  SEEK vtempo+ALLTRIM(filtro)+ALLTRIM(mValor)

  IF mVariable $' '
     SET FILTER TO codfte+codprg+codsubpr+Tipfun = Filtro + ALLTRIM(vtipfun)  AND EMPTY(CODSUPRY)
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
       DEFINE WINDOW _xx FROM 3,19 TO 22,79 &&COLOR SCHEME 7
       DO CASE
	 CASE alltrim(vtipfun) = 'I'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 CodProy  :H='Proy' ,;
		 Descri   :H='Nombre':50
	 CASE alltrim(vtipfun) = 'F'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 Codact   :H='Act',;
		 Descri   :H='Nombre':50
       ENDCASE
       ON KEY LABEL ENTER
       RELEASE WINDOW _xx
       IF !EMPTY(_OldWnd)
	  ACTIVATE WINDOW &_OldWnd
       ENDIF
     ENDIF
     SET FILTER TO
  ENDIF
  mValor = IIf(alltrim(vtipfun)='I',calen.codproy,calen.codact)
  mDescr = SUBSTR( calen.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
       CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN .T.
       CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
       CASE mVariable='V'   && En vista
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
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


FUNCTION Val_Supry
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
  vTempo =ALLTRIM(m.periodo)+ALLTRIM(m.nummes)
  if alltrim(vtipfun)='I'
     set orde to 1
  else
     set orde to 2
  endif

  Public vSale
  vSale=.t.
  SEEK vtempo+ALLTRIM(filtro)+ALLTRIM(mValor)

  IF mVariable $' '
     SET FILTER TO codfte+codprg+codsubpr+codproy+Tipfun = Filtro + ALLTRIM(vtipfun) AND !EMPTY(CODSUPRY) AND EMPTY(CODOBRA)
     GO TOP
     IF EOF()
	DO STANDBY WITH 'No se tiene calendario para este Programa'
	SET FILTER TO
	SELECT (mAlias)
	vSale=.f.
	RETURN
     ELSE
       _OldWnd = WOUTPUT()
       ACTIVATE SCREEN
       ON KEY LABEL ENTER KEYBOARD CHR(23)
       DEFINE WINDOW _xx FROM 3,19 TO 22,79 &&COLOR SCHEME 7
       DO CASE
	  CASE alltrim(vtipfun) = 'I'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 Codsupry :H='SubPy',;
		 Descri   :H='Nombre':50
	 CASE alltrim(vtipfun) = 'F'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 Codact   :H='Act',;
		 Descri   :H='Nombre':50
       ENDCASE
       ON KEY LABEL ENTER
       RELEASE WINDOW _xx
       IF !EMPTY(_OldWnd)
	  ACTIVATE WINDOW &_OldWnd
       ENDIF
     ENDIF
     SET FILTER TO
  ENDIF
  mValor = IIf(alltrim(vtipfun)='I',calen.Codsupry,calen.codact)
  mDescr = SUBSTR( calen.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
       CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN .T.
       CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN '  '
       CASE mVariable='V'   && En vista
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
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

FUNCTION Val_Prypre
*-----------------
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
  SELECT presu
  vTipFun=m.tipfun
  vTempo =ALLTRIM(m.periodo)
  if alltrim(vtipfun)='I'
     set orde to 1
  else
     set orde to 2
  endif
  Public vSale
  vSale=.t.
  SEEK vtempo+ALLTRIM(filtro)+ALLTRIM(mValor)
  IF mVariable $' ' AND EMPTY(mValor)
     SET FILTER TO codprg+codsubpr+Tipfun = Filtro + ALLTRIM(vtipfun)

     GO TOP
     IF EOF()
	DO STANDBY WITH 'No se tiene presupuesto para este Programa'
	SET FILTER TO
	IF !EMPTY( mAlias )
	   SELECT (mAlias)
	ENDIF
	vSale=.f.
	RETURN .F.
     ELSE
       _OldWnd = WOUTPUT()
       ACTIVATE SCREEN
       ON KEY LABEL ENTER KEYBOARD CHR(23)
       DEFINE WINDOW _xx FROM 3,19 TO 22,79 &&COLOR SCHEME 7
       DO CASE
	 CASE alltrim(vtipfun) = 'I'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 CodProy  :H='Proy' ,;
		 Descri   :H='Nombre':50
	 CASE alltrim(vtipfun) = 'F'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 Codact   :H='Act',;
		 Descri   :H='Nombre':50
       ENDCASE
       ON KEY LABEL ENTER
       RELEASE WINDOW _xx
       IF !EMPTY(_OldWnd)
	  ACTIVATE WINDOW &_OldWnd
       ENDIF
     ENDIF
     SET FILTER TO
  ENDIF
  mValor = IIf(alltrim(vtipfun)='I',presu.codproy,presu.codact)
  mDescr = SUBSTR( presu.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  set orde to 3
  DO CASE
       CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN .T.
       CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
       CASE mVariable='V'   && En vista
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
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


FUNCTION Val_act
*-----------------
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
  SELECT presu
  vTipFun=m.tipfun
  vTempo =ALLTRIM(m.periodo)
  set orde to iif(alltrim(vtipfun)='I',1,2)
  Public vSale
  vSale=.t.
  SEEK vtempo+ALLTRIM(filtro)+ALLTRIM(mValor)

  IF mVariable $' ' AND EMPTY(mValor)
     SET FILTER TO codprg+codsubpr+Tipfun = Filtro + ALLTRIM(vtipfun)
     GO TOP
     IF EOF()
	DO STANDBY WITH 'No se tiene presupuesto para este Programa'
	SET FILTER TO
	IF !EMPTY( mAlias )
	   SELECT (mAlias)
	ENDIF
	vSale=.f.
	IF mValor#'00'
	   RETURN .F.
	ENDIF
     ELSE
       _OldWnd = WOUTPUT()
       ACTIVATE SCREEN
       ON KEY LABEL ENTER KEYBOARD CHR(23)
       DEFINE WINDOW _xx FROM 3,19 TO 22,79 &&COLOR SCHEME 7
       DO CASE
	 CASE alltrim(vtipfun) = 'I'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 CodProy  :H='Proy' ,;
		 Descri   :H='Nombre':50
	 CASE alltrim(vtipfun) = 'F'
	     BROWSE WINDOW _xx TITLE ' ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
		 Codact   :H='Act',;
		 Descri   :H='Nombre':50
       ENDCASE
       ON KEY LABEL ENTER
       RELEASE WINDOW _xx
       IF !EMPTY(_OldWnd)
	  ACTIVATE WINDOW &_OldWnd
       ENDIF
     ENDIF
     SET FILTER TO
  ENDIF
     mValor = IIf(alltrim(vtipfun)='I',presu.codproy,presu.codact)
     mDescr = SUBSTR( presu.Descri, 1, mLong )
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  set orde to 3
  DO CASE
       CASE mVariable=' '   && En edici¢n
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN .T.
       CASE mVariable='A'   && En edici¢n SOLO DESCRIPCION
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
       CASE mVariable='V'   && En vista
       @ ROW(),mCol     SAY mValor
       @ ROW(),mCol+6   SAY mDescr
       RETURN ' '
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


FUNCTION Val_Part1
*-----------------
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
 m.codpart = mValor
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


FUNCTION Val_Part
*----------------
vvsel=SELECT()
IF !EMPTY(codpart)  
	IF (m.tipdoc='ME' .OR. m.tipdoc='RE' .OR. m.tipdoc='RU'.OR. m.tipdoc='SR') .AND. (codpart='0000RE' .OR. codpart='0000EN')     && Retenciones
		RETURN .T.
	ENDIF
	IF (m.tipdoc='SU' .OR. m.tipdoc='ME' .OR. m.tipdoc='SS') .AND. codpart='0000SU'     && Subsidios
		RETURN .T.
	ENDIF
	cc1=LEFT(codpart,1)
	cc2=SUBSTR(codpart,2,1)
	cc3=SUBSTR(codpart,3,2)
	cc4=RIGHT(codpart,2)
	SELE Parma
	GO TOP
	SEEK 'CATGAS'+cc1
	IF !FOUND()
		RETURN .F.
	ELSE
		GO TOP
		SEEK 'GRUGEN'+cc2
		IF !FOUND()
			RETURN .F.
		ELSE
			GO TOP
			SEEK 'MODAPL'+cc3
			IF !FOUND()
				RETURN .F.
			ELSE
				GO TOP
				SEEK 'ESPGAS'+cc4
				IF !FOUND()
					RETURN .F.
				ENDIF
			ENDIF
		ENDIF
	ENDIF
ELSE
	RETURN .F.
ENDIF			

RETURN .T.		


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

*FUNCTION XVal_ArtDet  && Articulos
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
*SET STEP ON
IF !mEdita
	SEEK xcod
	v_fun = IIF(FOUND(),Descri,"")
  ELSE
	IF !EMPTY(xcod)
		SET ORDER TO 1
		SEEK 'B'+XCOD
		IF !FOUND()
			if !yesno('Articulo no Encontrado...')
				UNLOCK ALL
				SELECT (mAlias)
				v_fun = .f.
				return v_fun
			endif
		ENDIF
	  ELSE
		ACTIVATE SCREEN
		ON KEY LABEL F10 KEYBOARD CHR(23)
		ON KEY LABEL F8 DO BorrDet
		ON KEY LABEL F5 DO AgreDet
		ON KEY LABEL F2 DO FunBusDet
		DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
		BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU key 'B' FIELDS;  	
			CodArt   :H='C¢digo'     :w=EMPTY(SUBSTR(CODART,8,3)),;
			Descri   :H='Nombre':50  :w=empty(descri) ,;
			CodUni   :H='Unidad':7   :w=empty(CodUni)
*       CodPart  :H='Partd':5  :w=empty(CodPart)
*&&     +XCOD
			ON KEY LABEL F10
			ON KEY LABEL F8
			ON KEY LABEL F5
			ON KEY LABEL F2
			RELEASE WINDOW _BusArt
			SET ORDER TO IteArt1
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
*SET STEP ON
IF !mEdita
	SEEK xcod
	v_fun = IIF(FOUND(),Descri,"")
  ELSE
	IF !EMPTY(xcod)
		SET ORDER TO IteArt1
		SEEK 'B'+XCOD
		IF !FOUND()
			if !yesno('Articulo no Encontrado...')
				UNLOCK ALL
				SELECT (mAlias)
				v_fun = .f.
				return v_fun
			endif
		ENDIF
	  ELSE
	  	GO TOP
		ACTIVATE SCREEN
		ON KEY LABEL F10 KEYBOARD CHR(23)
		ON KEY LABEL F8 			&& DO BorrDet
		ON KEY LABEL F5 			&& DO AgreDet
		ON KEY LABEL F2 DO FunBusDet
		DEFINE WINDOW _BusArt FROM 2,02 TO 22,77
		BROWSE WINDOW _BusArt TITLE '²²²² [F10] Selecciona  [F5] Agrega  [F8] Elimina  [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS ; 	
			CodArt   :H='C¢digo'     :w=EMPTY(SUBSTR(CODART,8,3)),;
			Descri   :H='Nombre':50  :w=empty(descri) ,;
			CodUni   :H='Unidad':7   :w=empty(CodUni)
*       CodPart  :H='Partd':5  :w=empty(CodPart)
*&&     +XCOD
		ON KEY LABEL F10
		ON KEY LABEL F8
		ON KEY LABEL F5
		ON KEY LABEL F2
		RELEASE WINDOW _BusArt
		SET ORDER TO IteArt1
		IF !EMPTY(_OldWnd)
*			ACTIVATE WINDOW &_OldWnd
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


*FUNCTION Val_Fun
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
		RETURN
ENDCASE
RELEASE POPUP _MM
RETURN

FUNCTION FUNBUS1
*--------------
PARAMETERS mLis1,mEsp1,mInd1,mLis2,mEsp2,mInd2,mLis3,mEsp3,mInd3,mLis4,mEsp4,mInd4
PRIVATE i,mOp,mEs,mIn
mOrd = ORDER()
DO CASE
	CASE PARAMETERS()=3
	CASE PARAMETERS()=6
	CASE PARAMETERS()=9
	CASE PARAMETERS()=12
	OTHERWISE
		DO StandBy WITH 'Error en la Definici¢n de parametros'
		RETURN
ENDCASE
IF escolor
	DEFINE POPUP _MM FROM 16,54 SHADOW COLOR &L_COL
  ELSE
	DEFINE POPUP _MM FROM 16,54 COLOR SCHEME C_POPUP
ENDIF
FOR i = 1 TO (PARAMETERS()/3)
	mLis = 'mLis'+STR(i,1)
	DEFINE BAR i OF _MM PROMPT &mlis
ENDFOR
ON SELECTION POPUP _MM DEACTIVATE POPUP
ACTIVATE SCREEN
ACTIVATE POPUP _MM
ACTIVATE WINDOW _FUNBUS
mOp = BAR()
mLis = 'mLis'+STR(mOp,1)
mEs = 'mEsp'+STR(mOp,1)
_Cod = SPACE(&mEs)
mIn = 'mInd'+STR(mOp,1)
mIn = &mIn
@01,02 SAY &mLis+' :' GET _Cod
READ
DEACTIVATE WINDOW _FUNBUS
IF LASTKEY() # 27
	SET ORDER TO (mIn)
	SEEK ALLTRIM(_Cod)
ENDIF
SET ORDER TO (mOrd)
RETURN

FUNCTION FUNBUSDET
*--------------
 IF escolor
   DEFINE POPUP _MM FROM 16,54 SHADOW COLOR &L_COL
 ELSE
   DEFINE POPUP _MM FROM 16,54 COLOR SCHEME C_POPUP
 ENDIF

 DEFINE BAR 1 OF _MM PROMPT ' Busqueda por \<C¢digo '
 DEFINE BAR 2 OF _MM PROMPT ' Busqueda por \<Nombre '
 ON SELECTION POPUP _MM DEACTIVATE POPUP

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
      RETURN
 ENDCASE
 RELEASE POPUP _MM
 RETURN

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
     * =INKEY(0.001,' ')
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
       *  =INKEY(0.001,' ')
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


FUNCTION ve_passw
*----------------
** Pide el password de acceso a cada opci¢n del sistema
PARAMETER _psw
PRIVATE xxx
   xxx = SPACE(6)
   DEFINE WINDOW _pasw FROM 12,15 TO 17,65  TITLE " Password de seguridad " COLOR SCHEME 5
   ACTIVATE WINDOW _pasw
   CLEAR
   @ 1, 4 SAY "Opci¢n restringida, ingrese password: "
   @ 2,20 SAY "[      ]"
   @ 2,21 GET xxx PICTURE "!!!!!!" COLOR ,X
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

*POP KEY

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
SET BLINK            ON  && Allows use of bright backgrounds
SET BORDER TO        SINGLE
* SET CARRY               && NOT RELEVANT TO SYSTEM ENVIRONMENT
* SET CARRY TO            && NOT RELEVANT TO SYSTEM ENVIRONMENT
SET CENTURY          OFF  && Default
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
   SET DEBUG          ON
  * SET STEP ON
ENDIF
SET DECIMALS TO      2   && Default
*SET DEFAULT TO           && LEAVE SET TO CURRENT DEFAULT DRIVE
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
SET EXACT            Off
*-------------------------------------------------------------OJO
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
SET SAFETY           ON
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

 * Ventana de Proceso
 DEFINE WINDOW Espera     FROM 12,06 TO 14,78 COLOR SCHEME 05 
   

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


FUNCTION VerEstCh
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00'
      vFun ='Cheque Girado'
   CASE vEst='10'
      vFun ='Cheque Autorizado'
   CASE vEst='40'
      vFun ='Cheque Entregado'
   CASE vEst='99'
      vFun ='Anulado'
   OTHERWISE
      vFun ='Sin Cheque'
   ENDCASE
RETURN vFun


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


FUNCTION vEstven
*---------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
DO CASE
	CASE vest='00'
		vfun ='Emitida      '
	CASE vest='99'
		vfun ='Anulada      '
	CASE vest='40'
		vfun ='Despachada   '
	CASE vest='50'
		vfun ='Devuelta     '
	CASE vest='60'
		vfun ='Pendiente     '	
ENDCASE
RETURN vfun


FUNCTION vEstOc
*--------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
IF !EMPTY(M.NUMREB) AND ESTADO#'22'
   vfun ='Con P/R: '+m.NumReb
ELSE
   IF !EMPTY(M.NUMANU) AND ESTADO#'21'
      vfun ='Con P/A: '+m.NumAnu
   ELSE   
	  DO CASE
		CASE vest='00'
			vfun ='Emitido       '
		CASE vest='20'
			vfun ='Afectado      '
		CASE vest='40'
			vfun ='En Almac‚n    '
		CASE vest='50'
			vfun ='Liquidado     '
		CASE vest='51'
			vfun ='Contabilizado '		
		CASE vest='21'
			vfun ='Con P/A: '+m.Numanu
		CASE vest='22'
			vfun ='Con P/R: '+m.NumReb
		CASE vest='99'
			vfun ='Anulado       '
	ENDCASE
	ENDIF
ENDIF
RETURN vfun

FUNCTION vEstNa
*--------------
PARAMETER vest
PRIVATE vfun
vfun = SPACE(12)
 DO CASE
		CASE vest='00'
			vfun ='Emitido       '
		CASE vest='50'
			vfun ='Liquidado     '
		CASE vest='51'
			vfun ='Contabilizado '		
		CASE vest='99'
			vfun ='Anulado       '
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


FUNCTION Val_Ing
*---------------
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
  SELECT IngR
  SEEK Filtro+mValor

  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
	 SET FILTER TO CodIng = Filtro
      ENDIF
      GO TOP
      IF EOF()
	 DO STANDBY WITH 'No existen partidas definidas'
	 SET FILTER TO
	 sele (malias)
	 return &&.f.
      endif
      DEFINE POPUP parametro FROM 03,40 PROMPT FIELD ALLTRIM(CODING)+'.'+ALLTRIM(SUBING)+' '+SUBSTR(DESING,1,40)
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
	 ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
 ENDIF
 mValor = IngR.CodIng +'.'+ IngR.SubIng
 mDescr = SUBSTR( IngR.DesIng, 1, mLong )
 m.codpart = mValor
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
   IF !mEdita
      SEEK v__cod
      v_fun = IIF(mVali,FOUND(),IIF(FOUND(),&v__ver,""))
   ELSE
wait wind str(v__Tipo)
wait wind v__Cod
wait wind _Campo

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

FUNCTION Val_Fun1
*----------------
PARAMETER v__al,v__dev,v__bus,v__cod,v__tipo,v__Ind,v__x,v__y,v__lon
PRIVATE mEdita, mMsg, mAlias, v_fun, _oldWind,_campo, mVali, mRec, vord
* Alias :     v__al
* Var.Devol.: v__dev
* Var.Mostr.: v__bus
* Var.Modif.: v__cod
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Indice    : v__Ind
* Posiciones: v__x, v__y
mEdita = (parameters()>=5) .and.  (v__tipo # 3)
mMsg   = (parameters()=8)  .and.  (v__tipo # 3)
mVali  = (parameters()=5)  .and.  (v__tipo = 3)
mlong  = IIF(PARAMETERS()=9,v__lon,40)
mRec   = IIF(EOF(),-1,RECNO())
mAlias = ALIAS()
IF mEdita
   _campo = TRIM(mAlias)+'.'+IIF(v__tipo >1, VARREAD(), v__dev)
ENDIF

SELECT &v__al
_OldWnd = WOUTPUT()

vOrd   = ORDER()
IF EMPTY(v__cod)
	SET ORDER TO (v__Ind)
ENDIF
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
         DEFINE POPUP v__xx FROM 2,01 TO 21,79 PROMPT FIELD &v__bus
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
               @ v__x,v__y SAY SUBS(&v__bus,1,mlong)
            ENDIF
            IF !EMPTY(mAlias)
               SET ORDER TO (vOrd)
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
         @ v__x,v__y SAY SUBS(&v__bus,1,mlong)
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


PROCEDURE EliAlf
*-------------------
PARAMETERS vVar
store space(14) to a,d
store 0 to b
store space(1) to X
A=vVar
B=LEN(vVar)
DECLARE C[B]
FOR I=1 TO B
    X=SUBSTR(A,I,1)
    IF X$'ABCDEFGHIJKLMNOPQRSTUVWXYZ-./'    
       C[I]=' '
    ELSE
       C[I]=X
    ENDIF
    IF !EMPTY(C[I])
       D=D+C[I]           
    ENDIF   
ENDFOR
RETURN D


FUNCTION SavCon
*---------------
USE h:\SISRG\DATA96\UsuSis IN 0 ALIAS Usu
PUBLIC _Hora
PRIVATE vconexa,vmaqi

SELECT Usu
APPEND BLANK
vConex = ALLTRIM(LEFT(SYS(0),8))
vmaq = ALLTRIM(RIGHT(SYS(0),2))
_hora  = TIME()
REPLACE CodUsu  WITH vconex
REPLACE machine WITH vmaq
REPLACE Prog    WITH SYS(16,1)
REPLACE Hora    WITH _hora
REPLACE dia     WITH DAY(DATE())
USE

USE h:\SISRG\DATA96\UsuGRAU IN 0 ALIAS Usu1
PUBLIC _Hora
SELECT Usu1
APPEND BLANK
_hora  = TIME()
REPLACE CodUsu  WITH vconex
REPLACE machine WITH vmaq
replace tipmaq  WITH substr(sys(17),3)
REPLACE Prog    WITH SYS(16,1)
REPLACE Hora    WITH _hora
REPLACE dia     WITH DAY(DATE())
REPLACE LLAVE   WITH VLLAV
vUsurec = recno()
USE
RETURN

FUNCTION EliCon
*-----------------
USE h:\SISRG\DATA96\UsuGRAU IN 0 ALIAS Usu1
SELECT Usu1
GO vUsurec
DELETE NEXT 1
USE
RETURN

FUNCTION BorCon
*-----------------
USE h:\SISRG\DATA96\UsuSis IN 0 ALIAS Usu
PRIVATE vconex,vPrg,Vmaq
SELECT Usu
GO TOP
vConex = ALLTRIM(LEFT(SYS(0),8))
vPrg   = SYS(16,1)
vmaq = ALLTRIM(RIGHT(SYS(0),2))
SET EXACT ON
DO WHILE !EOF()
	LOCATE FOR  ALLT(Codusu)=vconex AND ALLT(machine) = vmaq
		IF FOUND()
			DELETE NEXT 1
		ENDIF
	CONTINUE	
ENDDO	
SET EXACT OFF
USE
RETURN

 PROCEDURE EliAlf2
*-------------------
PARAMETERS vVar
_sw=.T.
store space(14) to a,d
store 0 to b
store space(1) to X
A=vVar
B=LEN(vVar)
DECLARE C[B]
X=SUBSTR(A,1,1)
IF UPPER(X)$'ABCDEFGHIJKLMNOPQRSTUVWXYZ-./0 '    
   _SW=.F.
ENDIF   
FOR I=1 TO B
    X=SUBSTR(A,I,1)
    IF UPPER(X)$'ABCDEFGHIJKLMNOPQRSTUVWXYZ-./ '    
        C[I]=' '
    ELSE    
		IF X='0' AND _SW=.F.
		   _sw=.F.
	       C[I]=' '
		ELSE
	       C[I]=X
           _SW=.T.
        ENDIF   
    ENDIF
    IF !EMPTY(C[I])
       D=D+C[I]           
    ENDIF   
ENDFOR
RETURN D

FUNCTION val_Obra && VALIDA OBRAS  (DEL PRESUPUESTO)
*----------------
PARAMETERS cvalor, cfiltro, cvariable, ccol, clong , cAncho
PRIVATE malias,VFUN
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
OK = .F.
VFUN = .F.
IF !EMPTY(cvalor)
    SET ORDER TO OBRA1
    SEEK CFILTRO+ALLTRIM(cvalor)
    ok = found()
    vFun = found()
endif
if !ok
   SET ORDER TO 1
   SEEK cfiltro+ALLTRIM(cvalor)
   if !found() OR !cvariable $'V'
	  SET FILTER TO periodo+Codprg+codsubpr+codproy+codsupry = ALLTRIM(cfiltro)
	  GO TOP
	  IF !EOF()
      	IF !EMPTY(cvalor) AND !EOF()
			SEEK cfiltro+cvalor
			IF !FOUND()
				DO rolea_ob
			ENDIF
		  ELSE
			DO rolea_ob
		  ENDIF
		  VFUN = .T.
	  ENDIF	  
  ENDIF
endif

if vfun
	cvalor    = Obra.CodObra
	cdescr    = SUBSTR(Obra.descri,1, clong )
	*m.destino = 'OBRA '+CODOBRA+ALLTRIM(tipobr)+' : '+ALLTRIM(VAL_PARA(m.TipObr,'TIPOBR','D',22,40))+' - '+SUBSTR(Obra.descri,1, 100 )
	vCodPrg   = Obra.Codprg
	vCodSub   = Obra.Codsubpr
	vProyec   = Obra.Codproy
	vSubpry   = Obra.CodSupry
else
    m.codobra = '   '
    cvalor = '   ' 
    cdescr = '**'
endif
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
			numObra   :H='Obr',;
			codObra   :H='Cont',;		
			descri    :H='Detalle'
			valobra = .t.
	OTHER
		IF !EMPTY(cvalor)
			DO standby WITH 'No se tiene Obra en referencia'
		ENDIF
		valobra = .f.
ENDCASE
ON KEY LABEL enter
RELEASE WINDOW _xx
IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF
RETURN

FUNCTION Val_Auxi
*----------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol
  PRiVATE mAlias
   DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 4
      mLong = 30               && Longitud campo DESCRi
      mTIP  = 24
      mDist = 10
    CASE PARAMETERS() = 5
      mDist = 10
  ENDCASE
  mAlias  = ALIAS()
  velig=.f.
  SELECT Auxil
  SEEK ALLTRiM(Filtro)+mValor
  IF !FOUND() .AND. !mVaRiable $'VZ'
     SET FILTER TO TIPO = ALLTRiM(Filtro)
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   RETURN	
    ENDIF
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(DescRi,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    mTipo  = Auxil.Tipo
  	mValor = Auxil.Codigo
  	mDescr = SUBSTR(Auxil.DescRi, 1, mLong)
  	velig=.t.
  ENDIF
  if velig=.f.
   	mTipo  = Auxil.Tipo
  	mValor = Auxil.Codigo
  	mDescr = SUBSTR(Auxil.DescRi, 1, mLong)
  endif	
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
    * @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      m.CodPrv=mvalor
      m.TipAux=mtipo
*     @ ROW(),mCol-mtip  SAY mvalor  &&mtipo
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPACE &mVaRiable WITH mValor
      RETURN .T.
  ENDCASE

FUNCTION Val_AuxiD
*----------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol
  PRiVATE mAlias
   DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 4
      mLong = 30               && Longitud campo DESCRi
      mTIP  = 24
      mDist = 10
    CASE PARAMETERS() = 5
      mDist = 10
  ENDCASE
  mAlias  = ALIAS()
  velig=.f.
  SELECT Auxil
  SEEK ALLTRiM(Filtro)+mValor
  IF !FOUND() .AND. !mVaRiable $'VZ'
	DO CASE 
	   CASE ALLTRIM(filtro)='20'
	   		SET ORDER TO Auxil6
	   CASE ALLTRIM(filtro)='30'
	   		SET ORDER TO Auxil9
	   CASE ALLTRIM(filtro)='09'
	   		SET ORDER TO Auxil14
	   CASE ALLTRIM(filtro)='80'
	   		SET ORDER TO Auxil15
	   CASE ALLTRIM(filtro)='50'
	   		SET ORDER TO Auxil16
	   OTHERWISE
		    SET FILTER TO TIPO = ALLTRiM(Filtro)
    ENDCASE
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       SET ORDER TO Auxil1
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   RETURN	
    ENDIF
    *SET ORDER TO auxil10
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(DescRi,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    mTipo  = Auxil.Tipo
  	mValor = Auxil.Codigo
  	mDescr = SUBSTR(Auxil.DescRi, 1, mLong)
  	velig=.t.
  	SET  FILT TO
  	SET ORDER TO Auxil1
  ENDIF
  if velig=.f.
   	mTipo  = Auxil.Tipo
  	mValor = Auxil.Codigo
  	mDescr = SUBSTR(Auxil.DescRi, 1, mLong)
  endif	
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
    * @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      m.CodPrv=mvalor
      m.TipAux=mtipo
*     @ ROW(),mCol-mtip  SAY mvalor  &&mtipo
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPACE &mVaRiable WITH mValor
      RETURN .T.
  ENDCASE


FUNCTION Val_AuxiD1
*----------------
  PARAMETERS mValor, Filtro, mVaRiable, mCol
  PRiVATE mAlias
   DO CASE
    CASE PARAMETERS() = 2
      mCol = 0
      mVaRiable = ' '
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 3
      mCol = 0
      mLong = 40
      mDist = 10
    CASE PARAMETERS() = 4
      mLong = 30               && Longitud campo DESCRi
      mTIP  = 24
      mDist = 10
    CASE PARAMETERS() = 5
      mDist = 10
  ENDCASE
  mAlias  = ALIAS()
  velig=.f.
  SELECT Auxil
  SEEK ALLTRiM(Filtro)+mValor
  IF !FOUND() .AND. !mVaRiable $'VZ'
     SET FILTER TO TIPO = ALLTRiM(Filtro)
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   RETURN	
    ENDIF
    SET ORDER TO auxil10
    DEFINE POPUP parametro FROM 03,40 PROMPT FIELD SUBSTR(DescRi,1,40)
    ON SELECTION POPUP parametro DEACTIVATE POPUP
    ACTIVATE POPUP parametro
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF
    RELEASE POPUP parametro
    mTipo  = Auxil.Tipo
  	mValor = Auxil.Codigo
  	mDescr = SUBSTR(Auxil.DescRi, 1, mLong)
  	velig=.t.
  	SET  FILT TO
  	SET ORDER TO Auxil1
  ENDIF
  if velig=.f.
   	mTipo  = Auxil.Tipo
  	mValor = Auxil.Codigo
  	mDescr = SUBSTR(Auxil.DescRi, 1, mLong)
  endif	
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVaRiable=' '   && En edici¢n
    * @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVaRiable='A'   && En edici¢n SOLO DESCRiPCION
      m.CodPrv=mvalor
      m.TipAux=mtipo
*     @ ROW(),mCol-mtip  SAY mvalor  &&mtipo
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVaRiable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVaRiable='D'   && En vista
      RETURN mDescr
    CASE mVaRiable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVaRiable='C'   && Solo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPACE &mVaRiable WITH mValor
      RETURN .T.
  ENDCASE

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

PROCEDURE ANALPRE
*----------------
PARAMETERS vCantOC,vtipO,vmues,VPART
PRIVATE WDF
WDF=.T.
IF ALLTRIM(m.tipfun)='F'
	WDF=Xrolea_PRE(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)                , alltrim(vcodact),vcantoc,VTIPO,vmues)
ELSE	
	WDF=Xrolea_PRE(m.periodo+alltrim(vcodprg)+alltrim(vcodsub)+alltrim(vProyec) , alltrim(vSubpry),vcantoc,VTIPO,vmues)
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
	      ITEHC.CODSUPRY, ITEHC.CODPRG,ITEHC.CODSUBPR,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
		  FROM ITEHC ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND ESTADO#'99' AND TIPFUN='I' AND val(nummes)<=val(vcalend)  and iif(!empty(NumPr),MesPr <= alltrim(m.Nummes),.t.); 
				INTO CURSOR Hojax
ELSE

 	SELECT ITEHC.NUMMES, ITEHC.CODANAL , itehc.estado,itehc.tipope , ITEHC.VALPART,  ;
     	   ITEHC.CODSUBPR, ITEHC.CODPRG,ITEHC.CODACT,ITEHC.Codfte, ITEHC.Tipfun ,ITEHC.TOTCAL,ITEHC.NUMMESHM,itehc.numhm ;
           FROM ITEHC ;
	 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,' ') ;
  					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,' ') AND iif(VTIPO=1,CODANAL='02',IIF(VTIPO=2,CODANAL='03',.T.)) AND ESTADO#'99' AND TIPFUN='F'  AND val(nummes)<=val(vcalend)  and iif(!empty(NumPr),MesPr<=alltrim(m.Nummes),.t.) AND LEFT(CODANAL,2)=VPART ; 
					INTO CURSOR HOJAX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 18 alias hoja1 EXCLU

IF ALLTRIM(m.tipfun)='I'
   SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, CALEN.CODPROY, CALEN.DESSUPRY,;
	      CALEN.CODSUPRY, CALEN.CODPRG,CALEN.CODSUBPR,CALEN.Codfte, CALEN.Tipfun ,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ,CALEN.TOTPRE , CALEN.ESTADO;
		  FROM CALEN ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I'  AND val(nummes)<=val(vcalend) ;
				INTO CURSOR CalenX
ELSE
	 	SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO, ;
	     	   CALEN.CODSUBPR, CALEN.CODPRG,CALEN.CODACT,CALEN.Codfte, CALEN.Tipfun,CALEN.TOTAFE,CALEN.TOTOC,CALEN.TOTOS,CALEN.TOTPRE,CALEN.ESTADO ;
               FROM CALEN ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='F'  AND val(nummes)<=val(vcalend) AND LEFT(CODPART,2)=VPART  ;
					INTO CURSOR CalenX
ENDIF

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 19 alias calen1 EXCLU

IF ALLTRIM(m.tipfun)='I'
   SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO, ITEPAR.CODPROY, ITEPAR.DESSUPRY ,itepar.cresup,itepar.transf ,;
	      ITEPAR.CODSUPRY, ITEPAR.CODPRG,ITEPAR.CODSUBPR,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
		  FROM ITEPAR ;
		  WHERE  tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(vProyec),codproy,'') + iif(!empty(proact),codsupry,'') ;
  				 = alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,CODPART='02',IIF(VTIPO=2,CODPART='03',.T.)) AND TIPFUN='I';
				INTO CURSOR PRESUX
ELSE
	 	SELECT ITEPAR.CODPART, ITEPAR.VALPART, ITEPAR.PERIODO,itepar.cresup,itepar.transf, ;
	     	   ITEPAR.CODSUBPR, ITEPAR.CODPRG,ITEPAR.CODACT,ITEPAR.Codfte, ITEPAR.Tipfun ,ITEPAR.TOTAFE,ITEPAR.TOTCAL ;
               FROM ITEPAR ;
		 	   WHERE tipfun+codfte+periodo+codprg+iif(!empty(vCodsub),codsubpr,'') + iif(!empty(proact),codact,'') ;
   					 = alltrim(m.tipfun)+alltrim(m.codfte) + xValor + iif(!empty(proact),proact,'') AND iif(VTIPO=1,ITEPAR.CODPART='02',IIF(VTIPO=2,ITEPAR.CODPART='03',.T.)) AND TIPFUN='F' AND LEFT(CODPART,2)=VPART;
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
	index on Periodo+CodPrg+CodFte+CodPart+CodSubpr+CodProy+CodSuPry to (ZDX)
ELSE
	index on Periodo+CodPrg+CodFte+Codpart+CodSubpr+CodAct to (ZDX)
ENDIF

GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	vTotcal = 0
	IF ALLTRIM(m.tipfun) = 'I'
	   vKeyI = Calen1.NumMes+ Calen1.TipFun+ Calen1.CodFte+ Calen1.Codprg  &&+Calen1.Codsubpr+ Calen1.CodProy+ Calen1.CodSuPry
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
                    Sele hojmod
                    seek hoja1.nummeshm+hoja1.numhm
                    if found()
                       vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                    else   
                       vPor = iif(hoja1.tipope='-',-1,1) 
                    endif  
                    selec hoja1 
	           
*	               vPor = iif(HOJA1.tipope='-',-1,1)
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
	      seek m.Periodo+HOJA1.CodPrg+HOJA1.CodFte+HOJA1.CodPart+HOJA1.CodSubpr+HOJA1.CodProy+HOJA1.CodSuPry
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
			  seek m.Periodo+calen1.CodPrg+calen1.CodFte+calen1.CodPart+calen1.CodSubpr+calen1.CodProy+calen1.CodSuPry
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
	   	vKeyF = Calen1.NumMes+ Calen1.TipFun+ Calen1.CodFte+ Calen1.Codprg+Calen1.Codsubpr+ Calen1.CodAct
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
                    Sele hojmod
                    seek hoja1.nummeshm+hoja1.numhm
                    if found()
                       vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                    else   
                       vPor = iif(hoja1.tipope='-',-1,1) 
                    endif  
                    selec hoja1 
            
*	                  vPor = iif(HOJA1.tipope='-',-1,1)
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
			seek m.Periodo+HOJA1.CodPrg+HOJA1.CodFte+HOJA1.CodAnal+HOJA1.CodSubpr+HOJA1.CodAct
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
			  seek m.Periodo+calen1.CodPrg+calen1.CodFte+calen1.CodPart+calen1.CodSubpr+calen1.CodAct
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
  	    	VPAR = CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2)
   	    	SELE PRESU1
			SUM TOTAFE TO VSUMA FOR CODPRG+CODSUBPR+CODACT+LEFT(CODPART,2) = VPAR
         ELSE
			VPAR = CODPRG+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2)
           	SELE PRESU1				
		    SUM TOTAFE TO VSUMA FOR CODPRG+CODSUBPR+CODPROY+CODSUPRY+LEFT(CODPART,2) = VPAR
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


PROCEDURE VAL_PRE
*----------------
PRIVATE ALS
ALS=ALIAS()
ON KEY LABEL F7
ON KEY LABEL F10
SELEC PRESU1
GO TOP
DEACTIVATE WINDOW Standby
vCodSub   = '   '
vCodAct   = '  '
ADX = SYS(3)+'.IDX'

IF ALLTRIM(m.tipfun)='F'

    INDEX ON CODSUBPR+codact+CODPART TO  (ADX)
    GO TOP
    DEFINE WINDOW _yy FROM 04,02 TO 19,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _yy TITLE 'SALDOS PRESUPUESTALES :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	        XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':3 ,;
	        FRO=IIF(RIGHT(CODPART,2)='00' OR VALPART=0,CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' '),'') :H='Act':6  ,;	
			vv=' '+Codpart  :H='Partida' ,;
			cc=Valpart+cresup+transf   :H='Marco ñ Modf':P='99,999,999.99',;
			TotCal             :H='Calendar.Acum':P='99,999,999.99',;
			Totafe             :H='Afectado Acum':P='99,999,999.99',;			
			zz=valpart+cresup+transf-TotAfe :H='Saldo Pptal' :P='99,999,999.99'
	RELEASE WINDOW _YY			
ELSE
    INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (ADX)
    GO TOP
    DEFINE WINDOW _ww FROM 04,01 TO 19,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _ww TITLE ' SALDOS PRESUPUESTALES :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	        FRO = CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' ') : H='SubPry' :10 ,;
			vv=' '+Codpart  :H='Partida' ,;
			cc=Valpart+cresup+transf   :H='Marco ñ Modf':P='99,999,999.99',;
			TotCal             :H='Calendar.Acum':P='99,999,999.99',;
			Totafe             :H='Afectado Acum':P='99,999,999.99',;			
			zz=valpart+cresup+transf-TotAfe :H='Saldo Pptal' :P='99,999,999.99'
	RELEASE WINDOW _ww			
ENDIF		
SELECT (ALS)			
*ON KEY LABEL F10
ON KEY LABEL F7 DO VAL_PRE
return

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


FUNCTION Val_Codcad
*------------------
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
  SET ORDER TO maepre1
  SEEK Filtro+mValor

  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO periodo+uniges+unieje = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    SET ORDER TO maepre4
    ON KEY LABEL f2  DO Completo
    ON KEY LABEL f3  DO PorCompon

    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 02,01 TO 23,78  DOUBLE ;
		TITLE '[F2] Orden completo [F3] Orden x Act/Pry+Comp.  [F10] seleccionar' COLOR SCHEME 15
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
    	DesCom=Val_para(codcom,'CODCOM','D',22,47) : H='Descripci¢n Componente'
	
    RELEASE WINDOWS wind_cad
    ON KEY LABEL f2  
    ON KEY LABEL f3  
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

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


FUNCTION Val_Codca1
*------------------
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
  SET ORDER TO maepre3
  SEEK Filtro+mValor

  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET FILTER TO periodo+uniges+unieje = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY( mAlias )
          SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    SET ORDER TO maepre4
    ON KEY LABEL f2  DO Completo
    ON KEY LABEL f3  DO PorCompon

    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 02,01 TO 23,78  DOUBLE ;
		TITLE '[F2] Orden completo [F3] Orden x Act/Pry+Comp.  [F10] seleccionar' COLOR SCHEME 15
		ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCAD	: H='CodCad',;
		UNIGES  : H='UniGes',;
		UNIEJE  : H='UniEje',;
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
    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

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
SET ORDER TO maepre1
SEEK filtro+vcodcad
IF !FOUND()
	Do standby WITH 'No existe Codigo de Cadena...'
	go vcursor
ENDIF
RELEASE WINDOWS lis
SET ORDER TO maepre4
RETURN

Procedure busDesCad
*------------------
vcursor = RECNO()
vorder  = ORDER()
DEFINE WINDOW lis FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lis
vdescad  = space(60)
@  3,2 SAY 'Descripci¢n : ' GET vdescad    PICTURE '@S40!'
READ
SET EXACT OFF
SET ORDER TO maepre2
SEEK ALLTRIM(vdescad)
IF !FOUND()
	Do standby WITH 'No existe Codigo de Cadena...'
	go vcursor
ENDIF
RELEASE WINDOWS lis
SET ORDER TO maepre4
SET EXACT ON
RETURN


PROCEDURE Completo
*-----------------
SELE maepre
SET ORDER TO maepre4
RETURN


PROCEDURE PorCompon
*------------------
SELE maepre
SET ORDER TO maepre5
RETURN


FUNCTION Val_Comp
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO maepre1
vrecno = Recno()
SEEK vfiltro
vBusca1 = Periodo+Uniges+Unieje+CodFun+CodPrg+CodSpr+ActPry
SET ORDE TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	vkey=vbusca1
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry=vkey
	GO TOP
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_Cad FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Componentes ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCOM	: H='CodComp',;
		xx=val_para(codcom,'CODCOM','D',20,25):h='Descripci¢n':25 :W=.F.
    RELEASE WINDOWS wind_cad
    SET FILTER TO
endif
mValor = maepre.codcom
mDescr = val_para(maepre.codcom,'CODCOM','D')
set orde to maepre1
go vrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
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
RETURN


FUNCTION Val_Meta
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO maepre1
vrecno = Recno()
SEEK vfiltro
vBusca1 = Periodo+Uniges+Unieje+CodFun+CodPrg+CodSpr+ActPry
SET ORDER TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	vord1=vbusca1+LEFT(vbusca,5)
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry+codcom=vORD1
	GO TOP
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_cad1 FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Metas ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad1
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad1  FIELD ;
		CODMET	: H='META',;
		DESCRI  : h='Descripci¢n':30 :W=.F.
    RELEASE WINDOWS wind_cad1
    SET FILTER TO
endif
mValor = maepre.codmet
mDescr = maepre.descri
SET ORDER to maepre1
go vrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && S¢lo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
RETURN



FUNCTION Val_Com1
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO maepre3
vrecno = Recno()
SEEK vfiltro
vBusca1 = Periodo+Uniges+Unieje+CodFun+CodPrg+CodSpr+ActPry
SET ORDE TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	vkey=vbusca1
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry=vkey
	GO TOP
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_Cad FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Componentes ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCOM	: H='CodComp',;
		xx=val_para(codcom,'CODCOM','D',20,25):h='Descripci¢n':25 :W=.F.
    RELEASE WINDOWS wind_cad
    SET FILTER TO
endif
mValor = maepre.codcom
mDescr = val_para(maepre.codcom,'CODCOM','D')
set orde to maepre1
go vrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
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
RETURN


FUNCTION Val_Met1
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO maepre3
vvrecno = Recno()
SEEK vfiltro
vBusca1 = Periodo+Uniges+Unieje+CodFun+CodPrg+CodSpr+ActPry
SET ORDER TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	vord1=vbusca1+LEFT(vbusca,5)
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry+codcom=vORD1
	GO TOP
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_cad1 FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Metas ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad1
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad1  FIELD ;
		CODMET	: H='META',;
		DESCRI  : h='Descripci¢n':30 :W=.F.
    RELEASE WINDOWS wind_cad1
    SET FILTER TO
endif
mValor = maepre.codmet
mDescr = maepre.descri
SET ORDER to maepre1
go vvrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && S¢lo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
RETURN

FUNCTION Val_Met2
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
SET ORDER TO maepre3
vvrecno = Recno()
SEEK vfiltro
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	vord1=vbusca1+LEFT(vbusca,5)
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry+codcom=vORD1
	GO TOP
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_cad1 FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Metas ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad1
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad1  FIELD ;
		CODMET	: H='META',;
		DESCRI  : h='Descripci¢n':30 :W=.F.
    RELEASE WINDOWS wind_cad1
    SET FILTER TO
endif
mValor = maepre.codmet
mDescr = maepre.descri
SET ORDER to maepre1
go vvrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
ENDIF
DO CASE
    CASE mVariable=' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mdist SAY mDescr
      RETURN .T.
    CASE mVariable='A'   && En edici¢n S
      RETURN mCodAux
    CASE mVariable='V'   && En vista
      @ ROW(),COL()  SAY mValor
      RETURN mDescr
    CASE mVariable='D'   && En vista
      RETURN mDescr
    CASE mVariable='Z'   && En vista SIN PINTAR
      RETURN mDescr
    CASE mVariable='C'   && S¢lo codigo
      RETURN .T.
    OTHERWISE            && En browse de edici¢n
      REPLACE &mVariable WITH mValor
      RETURN .T.
  ENDCASE
RETURN


FUNCTION Val_Cale1
*-----------------
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
  SELECT CALEN
  SEEK Filtro+mValor
  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      SET FILTER TO PERIODO+LEFT(ESTFUN,30)+CODFTE+NUMMES= Filtro
      GO TOP
	  IF EOF()
	  	 DO STANDBY WITH 'No existe Calendario a la Fecha'
	  	 SET FILTER TO
	  	 SET ORDE TO CALEN4
  		 IF !EMPTY( mAlias )
    		SELECT (mAlias)
    		RETURN .T.
  		 ENDIF
      ELSE
        ACTIVATE WINDOW STANDBY
        @ 01,13 SAY 'Espere un momento ...' COLOR '7+/5*'

      	vestfun = left(calen.estfun,10)
      	vcodfte = calen.codfte
      	vnummes = calen.nummes
      	set filter to
      	
      	vdbf = SYS(3) + '.DBF'
      	vind = SYS(3) + '.IDX'

		COPY STRUC TO (Vdbf)		      	
      	* selecciono todas las partidas de la estructura func. escogida
     	
      	SELECT * ;
 		FROM CALEN;
 		WHERE LEFT(CALEN.ESTFUN,10) = vestfun ;
   			  AND CALEN.CODFTE = vcodfte and calen.nummes=vnummes ;
		INTO CURSOR CALEx

      	zdbf = SYS(3) + '.DBF'
		zind = SYS(3) + '.IDX'
		COPY TO (zdbf)
		USE  (zdbf) IN 0 ALIAS CALEN2
		USE  (vdbf) IN 0 ALIAS CALEN1

		* creacion de techos 
				
		SELECT CALEN2
		INDEX ON CODPART TO (zind)
		go top
		SCAN
			SCATTER MEMVAR
			VKEY1 = SUBS(CALEN2.CODPART,1,2)
			VKEY2 = SUBS(CALEN2.CODPART,1,2)
			store 0 to vValpart,vtotafe,vtotoc,vtotos
			DO WHILE VKEY1 = VKEY2 AND !EOF()
				vValpart = vValpart + CALEN2.valpart 
				vtotafe  = vtotafe  + CALEN2.totafe
				vtotoc   = vtotoc   + CALEN2.totoc
				vtotos	 = vtotos   + CALEN2.totos
				skip
				VKEY2 = SUBS(CALEN2.CODPART,1,2)
			ENDDO				
			SKIP -1
			SELECT CALEN1
			APPEND BLANK
			GATHER MEMVAR
			REPLACE CODPART WITH SUBS(M.CODPART,1,2)
			REPLACE VALPART WITH vVALPART
			REPLACE TOTAFE  WITH vTOTAFE
			REPLACE TOTOC   WITH vTOTOC
			REPLACE TOTOS   WITH vTOTOS
			SELEC CALEN2
		ENDSCAN
		USE
		ERASE (zdbf)
		selec calex
		USE
		ERASE caleX
		
    	SELEC CALEN1     && BASE DE TECHOS
    	INDEX ON left(estfun,10)+codfte+LEFT(CODPART,2) TO (VIND)
    	
	 	SELEC CALEN     && PARTIDAS ASIGNADAS 
	  	SET FILTER TO PERIODO+LEFT(ESTFUN,30)+CODFTE+NUMMES= Filtro
      	GO TOP
	  	SET RELATION TO left(estfun,10)+codfte+LEFT(CODPART,2) INTO CALEN1
		SET SKIP TO CALEN1
		deactivate window standby
		
		DEFINE WINDOW wind_tec FROM 05,00 TO 15,79  DOUBLE FLOAT ;
		TITLE ' Techos Asignados '+LEFT(vestfun,2)+'.'+SUBS(vestfun,3,3)+'.'+SUBS(vestfun,6,2)+'.'+SUBS(vestfun,8,3) COLOR SCHEME 15
		SHOW WINDOWS wind_TEC
		SELEC CALEN1
		BROWSE ;
 			NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE  TIMEOUT 0.00001;
 			WIND wind_TEC FIELD ;
    		CodPart	: H='PG',;
    		valpart	: H='Asignado':P='999,999.99',;
    		TotAfe	: H='Afectado':P='999,999.99',;
    		TotOc	: H='Por O/C':P='999,999.99',;
    		TotOs	: H='Por O/S':P='999,999.99',;
    		PorAfe =IIF(totoc+totos#0,(totoc+totos)-Totafe,0):H='Por Afectar':P='999,999.99',;
    		SalPar = IIF(totoc+totos#0,ValPart-(totoc+totos),VALPART-TotAfe):H='Saldos':P='999,999.99'
		
       	ON KEY LABEL F10 KEYBOARD CHR(23)
       	DEFINE WINDOW wind_CAl FROM 16,00 TO 23,79  DOUBLE FLOAT SHADOW;
        FOOTER '[F10] Seleccionar' TITLE ' Calendario Asignado ' COLOR SCHEME 10
 	  	ACTIVATE WINDOWS wind_cal
		SELEC CALEN
	  	BROWSE ;
			NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
			IN WINDOW wind_cal  FIELD ;
    		CodFte	: H='Fte',;
    		CodPart	: H='Partid',;
       		xx=val_para(right(codpart,2),'ESPGAS','D',28,20) :H='Descripci¢n':20,;
    		valpart	: H='Asignado':P='999,999.99',;
    		TotAfe	: H='Afectado':P='999,999.99',;
    		TotOc	: H='Por O/C':P='999,999.99',;
    		TotOs	: H='Por O/S':P='999,999.99',;
    		PorAfe =IIF(totoc+totos#0,(totoc+totos)-Totafe,0):H='Por Afectar':P='999,999.99',;
    		SalPar = IIF(totoc+totos#0,ValPart-(totoc+totos),VALPART-TotAfe):H='Saldos':P='999,999.99'
		
		selec caleN1
		vValpart = valpart 
		vtotafe  = totafe
		vtotoc   = totoc
		vtotos	 = totos
		USE
		ERASE (vdbf)
		
		SELECT CALEN

      RELEASE WINDOW win_cad
	  RELEASE WINDOWS wind_cal
      RELEASE WINDOWS wind_tec
      RELEASE POPUP parametro
      SET FILTER TO
      endif
 ENDIF
 mValor = calen.CodPart
 mDescr = VAL_PARA(RIGH(mvalor,2),'ESPGAS','D')
 SET ORDE TO CALEN4
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  ON KEY LABEL F10 KEYBOARD CHR(23)
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
  SELECT CALEN
  SET ORDE TO CALEN4
  SEEK Filtro+mValor
  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      SEEK Filtro
	  IF !FOUND()
	  	 DO STANDBY WITH 'No existe Calendario a la Fecha'
	  	 SET ORDE TO CALEN4
  		 IF !EMPTY( mAlias )
    		SELECT (mAlias)
    		RETURN .F.
  		 ENDIF
	  
	  ELSE  		 
         IF !EMPTY(FILTRO)
         	SET FILTER TO Periodo+LEFT(estfun,30)+CodFte+Nummes=Filtro
      	 ENDIF
         GO TOP
      ENDIF

      DEFINE POPUP parametro FROM 11,40 PROMPT FIELD periodo+' '+nummes+' ' +codfte+' '+codpart TITLE 'Partidas con calendario' 
      ON SELECTION POPUP parametro DEACTIVATE POPUP
      ACTIVATE POPUP parametro
      IF !EMPTY( _OldWnd)
         ACTIVATE WINDOW &_OldWnd
      ENDIF

      RELEASE POPUP parametro
      SET FILTER TO
      
 ENDIF

  mValor = calen.CodPart
  mDescr = VAL_PARA(RIGH(mvalor,2),'ESPGAS','D')
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF

*  ON KEY LABEL F10 KEYBOARD CHR(23)
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


FUNCTION sector
*---------------
PARAMETER vvsec

DO CASE
	CASE vvsec='P'
		RETURN 'PESQUERIA'
	CASE vvsec='I'
		RETURN 'INDUSTRIA'
	CASE vvsec='T'
		RETURN 'TRABAJO'
	CASE vvsec='E'
		RETURN 'ENERGIA'
	CASE vvsec='A'
		RETURN 'ARCHIVO'
	OTHERWISE
		RETURN 'ADMINISTRACION CENTRAL'
ENDCASE

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
SUM monasig TO vcresup FOR Codpart=vcodpart	AND MONTH(fecha)<=VAL(vCALEND) AND codcad+codfte=vkey
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




FUNCTION busALM
*--------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

vAlias = ALIAS()

SELE StkAlm

DEFINE BAR 1 OF _mm PROMPT ' Busqueda por \<C¢digo '
DEFINE BAR 2 OF _mm PROMPT ' Busqueda por \<Nombre '
ON SELECTION POPUP _mm DEACTIVATE POPUP
orden = ORDER()
ACTIVATE SCREEN
ACTIVATE POPUP _mm
DO CASE
	CASE BAR()=1
		ACTIVATE WINDOW _funbus
		_cod = '000.000.000'
		@01,02 SAY 'C¢digo: ' GET _cod PICTURE '!!!.!!!.!!!'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO CodArt1
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO codart2
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
SELECT &vAlias
RETURN


FUNCTION BusLab
*----------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF

vAlias = Alias()

SELE ArtMae

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
			SET ORDER TO Artmae3
			GO TOP
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO Artmae4
			GO TOP
			SEEK UPPER( ALLTRIM(_cod) )
		ENDIF
	OTHERWISE
		RELEASE POPUP _mm
		SET ORDER TO (orden)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
SELECT &vAlias
RETURN



FUNCTION msgpro  && Envia mensaje de realizac. de proceso.
*--------------
PARAMETER _sw,_lin,_msg

PRIVATE _sw,_msg, vnumpara     && _sw= boolean  ; _msg= mensaje

**vBlink   = SET('BLINK')
vnumpara = PARAMETER()
_lin = IIF(vnumpara=1, 23, _lin)
IF vnumpara=1 OR vnumpara=2
	IF _sw
		_msg = 'Procesando, por favor espere un momento..'
	ENDIF
ENDIF

IF _sw
	PUBLIC _oldmsg, _oldwnd
	_msg    = PADC(_msg,2+LEN(_msg),' ')
	_oldmsg = _msg
	_oldwnd = WOUTPUT()
	_pos    = (80-LEN(_oldmsg))/2
	SET BLINK ON
	IF !WEXIST('_xyz')
		DEFINE WINDOW _xyz FROM  _lin,_pos TO _lin,LEN(_oldmsg)+_pos  NONE IN SCREEN
	ENDIF
	ACTIVATE WINDOW _xyz SAME
	@ 00,00 SAY _msg  COLOR  N*/W
ELSE
	DEACTIVATE WINDOW _xyz
	RELEASE WINDOW _xyz
	SET BLINK OFF
ENDIF

IF TYPE('_OldWnd') # 'U'
	IF !EMPTY(_oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ELSE
		ACTIVATE SCREEN
	ENDIF
ENDIF

RETURN
