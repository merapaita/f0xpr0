* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º  SAI_LIB.PRG                                                        º
* º                                                                     º
* º                 LIBRERIA DEL SISTEMA DE LA SBP                      º
* º                      SISTEMA DE ABASTECIMIENTOS                     º
* º                                                                     º
* º  Observaci¢n: para Fox Pro 2.x                                      º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

FUNCTION f_lock
*--------------
PARAMETERS btipo

beep = CHR(7)

DO CASE
	CASE btipo = 1    && Bloquea registro hasta conseguirlo o escape
		v_fun   = .F.
		IF RLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH "Registro del archivo " + ALIAS() + " ocupado. Espere un momento por favor o presione <Esc> para cancelar"
			?? beep
			ktecla    = 0

			DO WHILE ktecla <> ESCAPE .AND. ( .NOT. RLOCK())
				ktecla    = INKEY()
			ENDDO

			IF ktecla <> ESCAPE
				v_fun = .T.
			ENDIF
		ENDIF
	CASE btipo = 2     && Intenta bloquear registro solo una vez
		v_fun   = .F.
		IF RLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH "El registro del file" + ALIAS() + " est  siendo utilizado. Se cancela la operaci¢n"
			?? beep
		ENDIF
	CASE btipo = 3        && Bloquea archivo hasta conseguirlo a cancela
		v_fun   = .F.
		IF FLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH ALIAS() + "Archivo ocupado.  Espere un instante por favor o presione <Esc> para cancelar"
			?? beep
			ktecla    = 0
			DO WHILE ktecla <> ESCAPE .AND. ( .NOT. FLOCK())
				ktecla    = INKEY()
			ENDDO
			IF ktecla <> ESCAPE
				v_fun = .T.
			ENDIF
		ENDIF
	CASE btipo = 4   && Intenta bloquear archivo solo una vez
		v_fun   = .F.
		IF FLOCK()
			v_fun   = .T.
		ELSE
			DO standby WITH ALIAS() + " Archivo ocupado. El proceso se cancela"
			?? beep
		ENDIF
ENDCASE
RETURN v_fun

FUNCTION f_appd
*--------------
APPEND BLANK
v_fun = RLOCK() .OR. f_lock(1)

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
PARAMETERS btipo,bloquea
beep = CHR(7)

DO CASE

	CASE btipo = 1    && Bloquea registro hasta conseguirlo o escape
		insiste   = .T.
		bloquea   = .F.
		DO WHILE insiste
			IF RLOCK()
				bloquea   = .T.
				insiste   = .F.
			ELSE
				DO standby WITH "Registro ocupado. Espere un momento;
					por favor o presione <esc> PARA cancelar."
				?? beep
				ktecla    = 0
				DO WHILE ktecla <> 27 .AND. ( .NOT. RLOCK())
					ktecla    = INKEY()
				ENDDO
				IF ktecla <> 27
					bloquea = .T.
				ENDIF
				insiste = .F.
			ENDIF
		ENDDO

	CASE btipo = 2     && Intenta bloquear registro solo una vez

		bloquea   = .F.
		IF RLOCK()
			bloquea   = .T.
		ELSE
			DO standby WITH "El registro esta en uso. Presione <Esc> para cancelar. "
			?? beep
			ktecla    = 0
			DO WHILE ktecla <> 27
				ktecla    = INKEY()
			ENDDO
		ENDIF


	CASE btipo = 3        && Bloquea archivo hasta conseguirlo a cancela
		insiste   = .T.
		bloquea   = .F.
		DO WHILE insiste
			IF FLOCK()
				bloquea   = .T.
				insiste   = .F.
			ELSE
				DO standby WITH "Archivo ocupado.  Espere un instante por favor o"+;
					" presiona <Esc> para cancelar."
				?? beep
				ktecla    = 0
				DO WHILE ktecla <> 27 .AND. ( .NOT. FLOCK())
					ktecla    = INKEY()
				ENDDO
				IF ktecla <> 27
					bloquea = .T.
				ENDIF
				insiste = .F.
			ENDIF
		ENDDO

	CASE btipo = 4   && Intenta bloquear archivo solo una vez

		bloquea   = .F.
		IF FLOCK()
			bloquea   = .T.
		ELSE
			DO standby WITH "Archivo ocupado.  Presione <Esc> para cancelar."
			?? beep
			ktecla    = 0
			DO WHILE ktecla <> 27
				ktecla    = INKEY()
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
*  
*ha : 01/12/89
*
*************************************************************************


PROCEDURE fox_appd
*-----------------
PARAMETERS lappd
lappd     = .F.
bloquea   = .F.
gagrega   = .T.
APPEND BLANK
DO fox_lock WITH 1,bloquea
IF bloquea
	lappd   = .T.
ENDIF
gagrega = .F.
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
PARAMETERS prg_error
DO CASE
	CASE ERROR() = 108
		DO standby WITH "Un momento por favor el archivo est  en uso"
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
		nom_usr   = SYS(30)
		num_error = ERROR()
		des_error = MESSAGE()
		lin_error = MESSAGE(1)
		dbf_actua = DBF()
		IF SYS(21) <> '0'
			idx_actua = NDX(VAL(SYS(21)))
		ENDIF
		SAVE SCREEN TO pnt_error
		CREATE VIEW core.viw
		SAVE TO "core.fox"

		DO poperror WITH "El sistema acaba de detectar un error interno. Avise al  rea de Sistemas." + CHR(13) + CHR(13) + ;
			"Tome nota de la siguiente descripci¢n:" + CHR(13) + ;
			"Programa: " + ALLTRIM( prg_error ) + CHR(13) + ;
			"L¡nea   : " + ALLTRIM( lin_error ) + CHR(13) + ;
			"Error   : " + ALLTRIM( des_error )

		IF worker
			ON ERROR
			CANCEL
		ENDIF
		  QUIT
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
letras(3,1) = 'DIEZ '
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
	N   = SUBSTR( w2, i, 3 )
	IF VAL( N ) > 0
		x  = VAL( N )
		y = 1
		DO WHILE y <= 3        && Repite para cada 3 digitos.
			x1 = SUBSTR( N, y, 1 )
			IF VAL(x1) # 0
				DO CASE
					CASE y = 1
						IF VAL(N) = 100
							wletras = wletras + 'CIEN '
						ELSE
							wletras = wletras + letras(4,VAL(x1))
						ENDIF
					CASE y = 2
						IF VAL(SUBSTR(N,y,2)) > 10 .AND. VAL(SUBSTR(N,y,2)) < 20
							wletras = wletras + letras(2,VAL(SUBSTR( N, 3, 1 ) ))
						ELSE
							wletras = wletras + letras(3,VAL(x1))
						ENDIF
					CASE y = 3
						IF !SUBSTR(N,2,1) $ '1'
							IF !SUBSTR(N,2,1) $ '0' .AND. !EMPTY(wletras)
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
*wletras = ALLTRIM(wletras) + ' Y  ' + STR(centavos*100,2) + '/100 ' + simbolo
wletras = ALLTRIM(wletras) + ' Y ' + PADL(ALLTRIM(STR(centavos*100,2)),2,'0') + '/100 ' + simbolo

ll  = SUBSTR( wletras, 51, 15)
l   = 0
t   = 16
R   = LEN( wletras )
IF R > 65
	DO WHILE t <> 20
		t = t - 1
		p = SUBSTR(ll,t,1)
		IF p = ' '
			numlet1 = SUBSTR( wletras, 1, 50 ) + SUBSTR( ll, 1, t )
			numlet2 = SUBSTR( wletras, ( 51 + t ), ( R - ( 50 + t ) ) )
			t    = 20
		ENDIF
	ENDDO
ELSE
	numlet1 = wletras
ENDIF
RETURN numlet1

FUNCTION letras1
*--------------
parameter num,simbolo
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
      cad1 = ' UN MILLON'
   CASE  ( cmm = 0 )
      cad1 = ''
   CASE  ( cmm > 1 )
      cad1 = NUM6( cmm ) + ' MILLONES'
   OTHERWISE
      cad1 = '*****'
ENDCASE

cad1 = cad1 + NUM6( cmu )
cad1 = cad1 + ' Y '+PADL(ALLTRIM(STR(deci,2)),2,'0')+'/100 '+ simbolo

RETURN ( cad1 )


FUNCTION NUM6
*-------------
PARAMETER  cm
PRIVATE cad2
cmx = INT( cm / 1000 )
cuu = MOD ( cm , 1000 )
cad2 = ''
DO CASE
   CASE ( cmx = 1 )
      cad2 = ' MIL'
   CASE ( cmx = 0 )
      cad2 = ''
   CASE ( cmx > 1 )
      cad2 = NUM3( cmx ) + ' MIL'
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

Centena( 1 ) = ' CIENTO'
Centena( 2 ) = ' DOSCIENTOS'
Centena( 3 ) = ' TRECIENTOS'
Centena( 4 ) = ' CUATROCIENTOS'
Centena( 5 ) = ' QUINIENTOS'
Centena( 6 ) = ' SEISCIENTOS'
Centena( 7 ) = ' SETECIENTOS'
Centena( 8 ) = ' OCHOCIENTOS'
Centena( 9 ) = ' NOVECIENTOS'

cad3 = ''
ce = INT( cu / 100 )
de = MOD( cu,100 )

IF ( ce = 1  .AND. de = 0 )
   cad3 = ' CIEN'
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

dece( 1  ) = ' TREINTA'
dece( 2  ) = ' CUARENTA'
dece( 3  ) = ' CINCUENTA'
dece( 4  ) = ' SESENTA'
dece( 5  ) = ' SETENTA'
dece( 6  ) = ' OCHENTA'
dece( 7  ) = ' NOVENTA'

Unidad( 1 ) = ' UN'
Unidad( 2 ) = ' DOS'
Unidad( 3 ) = ' TRES'
Unidad( 4 ) = ' CUATRO'
Unidad( 5 ) = ' CINCO'
Unidad( 6 ) = ' SEIS'
Unidad( 7 ) = ' SIETE'
Unidad( 8 ) = ' OCHO'
Unidad( 9 ) = ' NUEVE'

Unid( 1 ) = ' DIEZ'
Unid( 2 ) = ' ONCE'
Unid( 3 ) = ' DOCE'
Unid( 4 ) = ' TRECE'
Unid( 5 ) = ' CATORCE'
Unid( 6 ) = ' QUINCE'

cad2 = ''

d = INT( du /10 )
u = MOD ( du,10 )

DO CASE
   CASE ( d >= 3 .and. d <= 9 )
      IF ( u = 0 )
         cad2 = Dece( d-2 )
      ELSE
         cad2 = Dece ( d- 2 ) + 'I' + LTRIM(Unidad( u ))
*         cad2 = Dece ( d- 2 ) + ' Y' + Unidad( u )
      ENDIF
   CASE ( d = 2 )
      IF ( u = 0 )
         cad2 = ' VEINTE'
      ELSE
         cad2 = ' VEINTI' + LTRIM(Unidad( u ))
      ENDIF
   CASE ( d = 1 )
      DO CASE
         CASE ( u <= 5 )
            cad2 = LTRIM(unid( u + 1 ))
         CASE ( u >= 6 )
            cad2 = ' DIECI' + LTRIM(Unidad( u ))
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
	IF .NOT. yesno('La Impresora est  apagada.¨ Continua ?')
		RETURN .F.
	ENDIF
ENDDO
RETURN .T.


PROCEDURE standby
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

ACTIVATE WINDOW standby IN SCREEN

msgwords = ALLTRIM(msgwords)
_ln      = LEN(msgwords)
_colw    = WCOLS()
msgwords = IIF(_ln>2*_colw,PADR(msgwords,2*_colw,' '),msgwords)
_ln      = LEN(msgwords)

msg1     = IIF(_ln>_colw,LEFT(msgwords,_colw),msgwords)
msg2     = IIF(_ln>_colw,SUBSTR(msgwords,_colw+1),"")

IF _ln>_colw
	FOR i=0 TO _colw/3-1
		IF SUBSTR(msg1,_colw-i,1) == " "
			msg1 = LEFT(msg1,_colw-i)
			msg2 = SUBSTR(msgwords,_colw-i+1)
			msg2 = IIF(LEN(msg2)>_colw,LEFT(msg2,_colw),msg2)
			EXIT
		ENDIF
	ENDFOR
ENDIF

@ 00,WCOLS()/2-LEN(msg1)/2  SAY msg1
IF !EMPTY(msg2)
	@ 01,WCOLS()/2-LEN(msg2)/2  SAY msg2
ENDIF

@ 02,_colw/2-16 SAY '<Pres. una tecla para continuar>'
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
_colw    = WCOLS()
msgwords = IIF(_ln>2*_colw,PADR(msgwords,2*_colw,' '),msgwords)
_ln      = LEN(msgwords)

msg1     = IIF(_ln>_colw,LEFT(msgwords,_colw),msgwords)
msg2     = IIF(_ln>_colw,SUBSTR(msgwords,_colw+1),"")

IF _ln>_colw
	FOR i=0 TO _colw/3-1
		IF SUBSTR(msg1,_colw-i,1) == " "
			msg1 = LEFT(msg1,_colw-i)
			msg2 = SUBSTR(msgwords,_colw-i+1)
			msg2 = IIF(LEN(msg2)>_colw,LEFT(msg2,_colw),msg2)
			EXIT
		ENDIF
	ENDFOR
ENDIF

@ 00,WCOLS()/2-LEN(msg1)/2  SAY msg1
IF !EMPTY(msg2)
	@ 01,WCOLS()/2-LEN(msg2)/2  SAY msg2
ENDIF
v_fun = .F.
@ 02,10 GET _nosi FUNCTION '*TH \!\<No;\?\<Si' DEFAULT 1;
	SIZE 1,10,8
READ CYCLE
IF LASTKEY() # 27
	v_fun = IIF(_nosi = 1,.F.,.T.)
ENDIF
DEACTIVATE WINDOW yesno
RETURN v_fun

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


FUNCTION parametro
*-----------------
PARAMETERS vtipo,vkey
PRIVATE malias
malias = ALIAS()
SELECT parma
SEEK vtipo+vkey
IF .NOT. FOUND()
	vnombre = ' '
ELSE
	vnombre = descri
ENDIF
SELECT (malias)
RETURN (vnombre)

FUNCTION valtod
*--------------
PARAMETERS _toda, _x,_y
PRIVATE _toda, _x, _y
IF _toda = 1
	@ _x,_y SAY SPACE(40)
ENDIF
RETURN .T.

FUNCTION fecha
*-------------
PARAMETERS vfecha
IF EMPTY(vfecha)
	vfecha = DATE()
ENDIF
vnumero = MONTH(VFECHA)
DO CASE
	CASE vnumero = 1
		vmes = 'Enero'
	CASE vnumero = 2
		vmes = 'Febrero'
	CASE vnumero = 3
		vmes = 'Marzo'
	CASE vnumero = 4
		vmes = 'Abril'
	CASE vnumero = 5
		vmes = 'Mayo'
	CASE vnumero = 6
		vmes = 'Junio'
	CASE vnumero = 7
		vmes = 'Julio'
	CASE vnumero = 8
		vmes = 'Agosto'
	CASE vnumero = 9
		vmes = 'Setiembre'
	CASE vnumero = 10
		vmes = 'Octubre'
	CASE vnumero = 11
		vmes = 'Noviembre'
	CASE vnumero = 12
		vmes = 'Diciembre'
ENDCASE
vdia = PADL(ALLTRIM(STR(DAY(vfecha),2)),2,'0')
vano = TRANSFORM(YEAR(vfecha),'9,999')
vffecha = 'Piura, '+vdia+' de '+vmes+' de '+vano
RETURN (vffecha)


PROCEDURE logos
*--------------
PARAMETERS rotulo1,rotulo2,tiempo

ACTIVATE SCREEN

DO CASE
	CASE PARAMETERS() = 0
		STORE SPACE(80) TO rotulo1,rotulo2
		tiempo = 0
	CASE PARAMETERS() = 1
		STORE SPACE(80) TO rotulo2
		tiempo = 0
	CASE PARAMETERS() = 2
		tiempo = 0
ENDCASE

FOR i=0 TO 39 STEP -1
	@ 00,i    SAY  C                     COLOR SCHEME  c_borde
	@ 00,79-i SAY  LEFT( rotulo1,i+1 )   COLOR SCHEME  c_fondo

	@ 24,79-i SAY  C                     COLOR SCHEME  c_borde
	@ 24,00   SAY  RIGHT( rotulo2,i+1 )  COLOR SCHEME  c_fondo

	FOR y = 0 TO tiempo
	ENDFOR

ENDFOR

FOR i=40 TO 0 STEP -1    &&  -8
	@ 00,79-i SAY  SUBSTR( rotulo1,2*(40-i),i+1)   COLOR SCHEME  c_fondo
	@ 00,i    SAY  LEFT( rotulo1,2*(40-i))         COLOR SCHEME  c_borde
	@ 24,00   SAY  SUBSTR( rotulo2,i,i)            COLOR SCHEME  c_fondo
	@ 24,i    SAY  RIGHT( rotulo2,2*(40-i))        COLOR SCHEME  c_borde

	FOR y = 0 TO tiempo
	ENDFOR

ENDFOR

RETURN

PROCEDURE err_input
*------------------
* window definido en objects

ACTIVATE WINDOW err_input
@ 00,01 SAY 'DATO NO VALIDO'
aa = INKEY(1,'H')
DEACTIVATE WINDOW err_input
RETURN


PROCEDURE repprg
*---------------
** Programa   : REPPRG.PRG
** Descripcion: Impresion de reportes utilizando programa reporteador.
** Par metros :
**               _PrgRpt : Programa reporteador.

PARAMETER  _prgrpt, _tit, _copia, _wp, ran_pg
PRIVATE  resul
_wndold = WOUTPUT()

_conso = SET("CONSOLE")


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
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit ;
		FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
IF impre
			FOR v = 1 TO _copia
				IF ready2pr() .AND. IIF(_copia>1,yesno("Copia "+STR(v,2)+" .¨Prepare el papel. Listo?"),.T.)
				*!TYPE &p_fil>prn
				    DO &_prgrpt WITH 3					
				ENDIF
			ENDFOR


*     DO &_prgrpt WITH 3

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


PROCEDURE reporte
**-----------------------------------------------------------------------
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
			SET PRINTER TO \\Abastecimiento\fx1180-Abas
*			SET PRINTER TO \\ibm_pc\printq_0=lpt1
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
		xDirImp = SET("PRINT",1)
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
		SET PRINTER TO &xDirImp

		IF !_wp
			FOR i=1 TO num_c
				IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
					!TYPE &p_fil > &xDirImp
*					!TYPE &p_fil >prn
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
				
				****
					IF !EMPTY(LEFT(SYS(0),15))
						IF !yesno("¨Imprime en impresora local?")
							**-- Impresora de red.
							SET PRINTER TO \\Abastecimiento\fx1180-Abas
						ENDIF
					ENDIF
					xDirImp = SET("PRINT",1)
				****
					SET PRINTER TO &xDirImp
					
					FOR i=1 TO num_c
						IF ready2pr() .AND. IIF(num_c>1,yesno("Copia "+STR(i,2)+" .¨Prepare el papel. Listo?"),.T.)
							!TYPE &p_fil > &xDirImp
*							!TYPE &p_fil >prn
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

PROCEDURE xreporte
*----------------
** Programa   : REPORTE.PRG
** Descripcion: Impresion de reportes utilizando archivos de reporte.
** Par metros :
**               _Tipo      : tipo de rpeorte (1=con ambiente, 2=sin ambiente,3= Con Condici¢n)
**               _Form      : archivo de reporte
**               _Tit       : t¡tulo de la ventana
**               _Condicion : condici¢n de los registros
PARAMETER _tipo,_form,_tit,_condicion, _wp

**-- Environment
SET ESCAPE ON
ON ESCAPE STORE .F. TO printing

**-- Verifica Existencia de windows
IF !WEXIST("MSG2USER")
	DEFINE WINDOW msg2user   FROM 12,02 TO 16,77 DOUBLE FLOAT SHADOW COLOR SCHEME 7
ENDIF

**-- PREGUNTA SI SE IMPRIME O VISUALIZA
ACTIVATE WINDOW msg2user
_dest = 'Impresora'
@ 01,5 SAY 'Destino de impresi¢n : Pantalla/Impresora' GET _dest PICTURE "@M Impresora,Pantalla"
READ
DEACTIVATE WINDOW msg2user

IF MOD(READKEY(),256)=12
	ACTIVATE SCREEN
	RETURN
ENDIF
impre = (_dest='Impresora')
IF !impre
	p_fil = SYS(3)+".LST"
ENDIF

printing = .T.

IF impre
	IF !EMPTY(LEFT(SYS(0),18))
		IF yesno("¨ Imprime en RED ?")
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
	DEFINE WINDOW _repo FROM 0,0 TO 24,79 COLOR SCHEME 10 TITLE _tit;
		FOOTER ' Pag: [Pg-Up]  Pag: [Pg-Dn]  Inicio: [Ctrl+Home]  Final: [Ctrl+End] '
	ACTIVATE WINDOW _repo
	CLEAR

	**-- SI SE IMPRIME O VISUALIZA
	IF impre
		DO CASE
			CASE _tipo = 1
				REPORT FORM &_form ENVIRONMENT NOEJECT TO PRINT
			CASE _tipo = 2
				REPORT FORM &_form NOEJECT TO PRINT
			CASE _tipo = 3
				REPORT FORM &_form WHILE &_condicion NOEJECT TO PRINT
				** REPORT FORM &_Form FOR &_Condicion NOEJECT TO PRINT
		ENDCASE
		SET PRINTER TO           && Si era la impresora server, libera el spool
	ELSE
		DO CASE
			CASE _tipo = 1
				REPORT FORM &_form ENVIRONMENT TO FILE (p_fil) NOEJECT
			CASE _tipo = 2
				REPORT FORM &_form TO FILE (p_fil) NOEJECT
			CASE _tipo = 3
				REPORT FORM &_form WHILE &_condicion TO FILE (p_fil) NOEJECT
				** REPORT FORM &_Form FOR &_Condicion TO FILE (P_FIL) NOEJECT
		ENDCASE
		MODIFY COMMAND (p_fil) NOEDIT WINDOW _repo
		IF !_wp
			IF yesno("¨ Imprime el Reporte ?")
				IF ready2pr()
					!TYPE &p_fil>prn
				ENDIF
			ENDIF
		ELSE
			!FOXSWAP wp &p_fil
		ENDIF
		ERASE (p_fil)
	ENDIF
	DEACTIVATE WINDOW _repo
	RELEASE WINDOW _repo
ELSE
	DO standby WITH 'El reporte ha sido cancelado'
ENDIF

ON ESCAPE
SET ESCAPE OFF

IF _tipo = 1
	CLOSE DATA
ENDIF

ACTIVATE SCREEN

RETURN

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

*IF FILTRO='CODFTE'
*	WAIT WIND MVALOR
*ENDIF

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
	SET CONFIRM ON
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
mcuenta= parma.descriau2
mdescr = SUBSTR( parma.descri, 1, mlong )
mdescriaux = SUBSTR( parma.descriaux, 1, mlong)
IF !EMPTY( malias )
	SELECT (malias)
ENDIF

DO CASE
	CASE mvariable==' '   && En edici¢n
		@ ROW(),MCOL       SAY mvalor
		@ ROW(),MCOL+mdist SAY mdescr
		RETURN .T.
	CASE mvariable=='A'   && En edici¢n SOLO DESCRIPCION
		@ ROW(),MCOL SAY mdescr
		RETURN ' '
	CASE mvariable=='V'   && En vista
		@ ROW(),COL()  SAY mvalor
		RETURN mdescr
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



PROCEDURE rolea
*---------------
_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter KEYBOARD CHR(23)
DEFINE WINDOW _xx FROM 06,10 TO 17,69 DOUBLE FLOAT SHADOW COLOR SCHEME 10
GO TOP
SCATTER MEMVAR
DO CASE
	CASE ALLTRIM(tipfun) = 'I' AND !EOF() AND !EMPTY(Codproy)
*	CASE ALLTRIM(m.tipfun) = 'I' AND !EOF() AND !EMPTY(Codproy)
		BROWSE WINDOW _xx TITLE ' PROYECTOS :  ®Enter¯  Selecciona  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codproy  :H='Pry' ,;
			descri   :H='Detalle'
	CASE ALLTRIM(tipfun) = 'F' AND !EOF() AND !EMPTY(Codact)			
*	CASE ALLTRIM(m.tipfun) = 'F' AND !EOF() AND !EMPTY(Codact)
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

PROCEDURE ANALCAL
*----------------
PARAMETERS vCantOC,vtipO,VMUES
SET SAFETY OFF
PRIVATE WDF
WDF=.T.
IF WDF=.t.
	WDF=Xrolea_cal(m.periodo+m.codcad)
ENDIF

RETURN WDF

PROCEDURE Xrolea_CAL
*-----------------------
parameter xvalor,proact,vtotoc,vtipo,vmues
AS=ALIAS()
Private vdf 
vdf = .t.
vms = m.nummes
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

SELECT HOJA.PERIODO,ITEHC.NUMMES, ITEHC.CODPART, hoja.estado,itehc.tipope, ITEHC.VALPART,ITEHC.Codfte,ITEHC.TOTCAL,ITEHC.NUMMESHM,ITEHC.MESPR,itehc.numhm,ITEHC.NUMPA,ITEHC.NUMPR;
		  FROM ITEHC,hoja ;
		  WHERE  iif(HOJA.estado='92',nummeshm,ITEHC.nummes)+HOJA.PERIODO+ITEHC.codfte+ITEHC.codcad =;
		   alltrim(m.Nummes)+alltrim(m.codfte) + xvalor AND hoja.ESTADO#'99' AND hoja.numhc=itehc.numhc and iif(!empty(NumPr),MesPr=alltrim(m.Nummes),.t.) ;
				INTO CURSOR Hojax
				
vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 24 alias hoja1 exclu

SELECT CALEN.NUMMES, CALEN.CODPART, CALEN.VALPART, CALEN.PERIODO,;
	      CALEN.Codfte,CALEN.TOTAFE ,CALEN.TOTOC ,CALEN.TOTOS ;
		  FROM CALEN ;
		  WHERE  nummes+codfte+periodo+codcad ;
  				 = alltrim(m.Nummes)+alltrim(m.codfte) + xvalor AND ESTADO#'20';
				INTO CURSOR CalenX

vInd = SYS(3) + '.DBF'
COPY TO (vInd)
use (vind) in 25 alias calen1 exclu
SELE HOJA1
INDEX ON CODPART TO (RDX)
SELE CALEN1
REPLA ALL TOTAFE WITH 0
INDEX ON CODPART TO (SDX)
GO TOP
DEACTIVATE WINDOW Standby

IF !EOF()
	vTotAfe = 0
	SELECT HOJA1
    GO TOP                                     
    SCAN 
    	vPart1 = HOJA1.CodPart
	    vPart2 = HOJA1.CodPart
        vTotAfe = 0
        DO WHILE  vPart1 = vPart2
            do case
               case HOJA1.estado='92'
                    Sele hojmod
                    seek hoja1.nummeshm+hoja1.numhm
                    if found()
                       vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
																                       *vpor = iif(RIGHT(ALLTRIM(tiphm),1)='P',0,iif(hoja1.tipope='-',-1,1))
                    else   
                       vPor = iif(hoja1.tipope='-',-1,1) 
                    endif  
                    selec hoja1 
																	                    *vPor = iif(HOJA1.tipope='-',-1,1) 
               case HOJA1.estado='94'
                    vPor = -1
               case HOJA1.estado='99'
                    vPor = 0 
                case hoja1.estado='00' AND !empty(hoja1.numPA) 
                     if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
                        vTotReb = vTotReb + valpart
                        *
                        * no se usa
                        * DO REBA
                        *
                        vPor = -1	             
                     else   
                        vPor = 0	               
                     endif   
               OTHER
                    vPor = 1
            endcase   		                 		                 		                     
	        vTotafe = vTotafe + vpor*Valpart
	        SKIP
	        vPart1 = HOJA1.CodPart
	    ENDD
	    SKIP -1
		sele calen1
		seek HOJA1.CodPart
		if found()
		   if rlock()
              replace totafe with vtotafe
           endif
           unlock
	    endif   
	    SELE HOJA1
    ENDSCAN
    SELECT Hoja1
	go top
	SCAN 
	    SELE CALEN1
	    SEEK HOJA1.CodPart
	    IF !FOUND()
	        sele hoja1
			vPart1 = HOJA1.CodPart
			vPart2 = HOJA1.CodPart
			vTotAfe = 0
			DO WHILE  vPart1 = vPart2 
				do case
				   case hoja1.estado='92'
                     Sele hojmod
                     seek hoja1.nummeshm+hoja1.numhm
                     if found()
                        vpor = iif(tiphm='P',0,iif(hoja1.tipope='-',-1,1))
                        *vpor = iif(RIGHT(ALLTRIM(tiphm),1)='P',0,iif(hoja1.tipope='-',-1,1))
                     else   
                        vPor = iif(hoja1.tipope='-',-1,1) 
                     endif  
                     selec hoja1 
			         *vPor = iif(HOJA1.tipope='-',-1,1) 
				   case hoja1.estado='94'
	                 vPor = -1
                case hoja1.estado='00' AND !empty(hoja1.numPA) 
                     if mespa=ALLTRIM(Vnummes) and !empty(mespa) AND HOJA1.NUMMES<ALLTRIM(VNUMMES)	                  
                        vTotReb = vTotReb + valpart
                        *
                        * no se usa
  		                * DO REBA
  		                *
                        vPor = -1	             
                     else   
                        vPor = 0	               
                     endif   
	                 
	    	       OTHER
	                 vPor = 1
		        endcase   		                 		                 		                     
   			    vTotafe = vTotafe + vpor*hoja1.Valpart
		        SKIP	
				vPart1 = HOJA1.CodPart
			ENDD
			SKIP -1
		    vtipfun = m.tipfun
			scatter memvar
	        sele calen1
	        IF F_APPD()
	            gather memvar
		        REPLACE totafe with vtotafe        
                REPLACE CODPART WITH HOJA1.CODPART ,valpart with 0
		    ENDIF    

		    UNLOCK
		    m.tipfun = vtipfun
	        sele hoja1
	 endif
     sele hoja1 
	endscan
	SELECT CALEN1
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

PROCEDURE VAL_cal
*----------------
PRIVATE ALS
ALS=ALIAS()
ON KEY LABEL F7
SELEC CALEN1
GO TOP
DEACTIVATE WINDOW Standby
*ON KEY LABEL F10 KEYBOARD CHR(23)
VDX = SYS(3)+'.IDX'
ZDX = SYS(3)+'.IDX'

IF ALLTRIM(m.tipfun)='F'
    INDEX ON CODSUBPR+CODACT+CODPART TO (ZDX)
    GO TOP
    DEFINE WINDOW _yy FROM 04,06 TO 19,78 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _yy TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS ;
	        XX=IIF(RIGHT(CODPART,2)='00',VAL_PARA(nummes,'FECMES','D')+'>','         ') :H='Mes':11 ,;
	        FRO=IIF(RIGHT(CODPART,2)='00' OR VALPART=0,CODSUBPR+IIF(!EMPTY(CODACT),'.'+CODACT,' '),'') :H='Act':6  ,;	        
			vv=' '+Codpart  :H='Partida' ,;
			Valpart  :H='     Asignado':P='99,999,999.99',;
			Totafe             :H='     Afectado':P='99,999,999.99',;
			zz=valpart- TotAfe :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _YY			
ELSE
    INDEX ON CODSUBPR+CODPROY+CODSUPRY+CODPART TO (ZDX)
    GO TOP 
    DEFINE WINDOW _ww FROM 04,04 TO 15,67 DOUBLE FLOAT SHADOW COLOR SCHEME 10
	BROWSE WINDOW _ww TITLE ' CALENDARIOS :  ®ESC¯  Continua  ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
	        FRO = CODSUBPR+'.'+CODPROY+IIF(!EMPTY(CODSUPRY),'.'+CODSUPRY,' ') : H='SubPry'  ,;
			vv=' '+Codpart  :H='Partida' ,;
			Valpart  :H='     Asignado':P='99,999,999.99',;
			Totafe             :H='     Afectado':P='99,999,999.99',;
			zz=valpart- TotAfe :H='       Saldo' :P='99,999,999.99'
	RELEASE WINDOW _ww			
ENDIF		
SELECT (ALS)			
*ON KEY LABEL F10
ON KEY LABEL F7 DO VAL_CAL
return

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
  SET ORDER TO TAG ESTFUN10 
  SEEK Filtro+mValor
  IF !FOUND() AND !mVariable $'VZ'
      _OldWnd = WOUTPUT()
      ACTIVATE SCREEN
      IF !EMPTY(FILTRO)
         SET FILTER TO periodo+codcad+codfte = Filtro
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
*SUSP
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
		vestfun  = left(calen.estfun,10)
		vestfun1 = left(calen.estfun,20)
		vcodfte = calen.codfte
		vnummes = calen.nummes
		
		vdbf = SYS(3) + '.DBF'
		vind = SYS(3) + '.IDX'
      	
		COPY STRUC TO (Vdbf)		      	
      	* selecciono todas las partidas de la estructura func. escogida
		* base para los techos del calendario
		
      	zdbf = SYS(3) + '.DBF'
		zind = SYS(3) + '.IDX'
		Xind = SYS(3) + '.IDX'
		yind = SYS(3) + '.IDX'

		* base de partidas especificas
		
		copy to  (zdbf) for LEFT(CALEN.ESTFUN,10) = vestfun ;
   	    	  AND CALEN.CODFTE = vcodfte and calen.nummes=vnummes 
		
		
		USE   && CIERRA CALENDARIO PARA DAR MAS MEMORIA
		
		USE  (zdbf) IN 0 ALIAS CALEN2	 && PARTIDAS ESPECIFICAS
		USE  (vdbf) IN 0 ALIAS CALEN1    && PARTIDAS GENERICAS
		
		* MONTOS DE LA HOJA DE CONTROL
		SELE ITEHC
		vrecno = IIF(EOF(),-1,recno())
		
		INDEX ON CODPART to (yind) FOR CODFTE = vcodfte and nummes=vnummes 
		GO TOP
		SCAN
			sele maepre
			seek m.periodo+itehc.uniges+itehc.unieje+itehc.codcad
			vkey = uniges+unieje+codfun+codprg+codspr+actpry+itehc.codcom+itehc.codmet+ITEHC.CODPART+ITEHC.CODFTE
			SELECT calen2
			LOCATE FOR LEFT(ESTFUN,30)+CODPART+CODFTE = vkey
			if found()
				REPLACE totafe WITH totafe+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
			ENDIF
			SELECT itehc
		ENDSCAN
		SET ORDER TO ITEHC1
		IF vrecno#-1
			go vrecno	
		ENDIF
		* cracion de techos 
				
		SELECT CALEN2
		INDEX ON CODPART TO (zind)
		go top
		SCAN
			VKEY1 = LEFT(CALEN2.CODPART,2)
			VKEY2 = LEFT(CALEN2.CODPART,2)
			store 0 to vValpart,vtotafe,vtotoc,vtotos
			DO WHILE VKEY1 = VKEY2 AND !EOF()
				vValpart = vValpart + CALEN2.valpart 
				vTotAfe  = vTotAfe  + CALEN2.totafe
				skip
				VKEY2 = LEFT(CALEN2.CODPART,2)
			ENDDO				
			SKIP -1
			SELECT CALEN1
			APPEND BLANK
			REPLACE CODPART WITH LEFT(CALEN2.CODPART,2)
			REPLACE VALPART WITH vVALPART
			REPLACE totafe  WITH vtotafe
			REPLACE ESTFUN  WITH CALEN2.ESTFUN
			SELEC CALEN2
		ENDSCAN
		
    	SELEC CALEN1     && BASE DE TECHOS
    	INDEX ON left(estfun,10)+codfte+LEFT(CODPART,2) TO (VIND)
    	
      	SELEC CALEN2     && PARTIDAS ASIGNADAS 
	  	INDEX ON left(estfun,10)+codfte+LEFT(CODPART,2) TO (XIND)
		SET FILTER TO PERIODO+left(estfun,30) = LEFT(FILTRO,32) INTO CALEN1
		GO TOP
		
		DEFINE WINDOW wind_tec FROM 00,01 TO 10,78  DOUBLE FLOAT SHADOW;
		TITLE 'Techos en : '+vestfun+SPACE(3)+'Fte. Fto. : '+ allt(m.codfte) COLOR SCHEME 10
		SHOW WINDOWS wind_TEC
		SELEC CALEN1
		GO TOP
		BROWSE ;
 			NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE  TIMEOUT 0.00001;
 			WIND wind_TEC FIELD ;
    		CodPart	: H='PG',;
    		valpart	: H='Asignado':P='99,999,999.99',;
    		TotAfe	: H='Afectado':P='99,999,999.99',;
    		SalPar = IIF(totoc+totos#0,ValPart-(totoc+totos),VALPART-TotAfe):H='Saldos':P='99,999,999.99'
    	
       	ON KEY LABEL F10 KEYBOARD CHR(23)
       	
       	DEFINE WINDOW wind_CAl FROM 11,01 TO 24,78  DOUBLE FLOAT SHADOW;
        FOOTER '[F10] Seleccionar' TITLE 'Calendario Asignado  ' COLOR SCHEME 15
 	  	ACTIVATE WINDOWS wind_cal
		SELEC CALEN2
		
	  	
	  	BROWSE ;
			NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH ;
			IN WINDOW wind_cal  FIELD ;
    		CodFte	: H='Fn',;
    		CodPart	: H='Partida',;
       		xx=val_para(right(codpart,2),'ESPGAS','D',28,20) :H='Descripci¢n':20,;
    		valpart	: H='Asignado':P='99,999,999.99',;
    		TotAfe	: H='Afectado':P='99,999,999.99',;
    		SalPar = IIF(totoc+totos#0,ValPart-(totoc+totos),VALPART-TotAfe):H='Saldos':P='99,999,999.99'
		
		vValpart = valpart 
		vtotafe  = totafe
		vtotoc   = totoc
		vtotos	 = totos
		mValor   = CodPart
 		mDescr   = VAL_PARA(RIGH(mvalor,2),'ESPGAS','D')
		
		USE
		ERASE (zdbf)
		
		SELECT CALEN1
		USE
		ERASE (vdbf)
		
	  	RELEASE WINDOWS wind_cal
      	RELEASE WINDOWS wind_tec

      	RELEASE POPUP parametro
      endif
      USE Calen    in 9   order tag calen4   ALIAS CALEN
      SELEC CALEN
 ELSE
 	mValor = calen.CodPart
 	mDescr = VAL_PARA(RIGH(mvalor,2),'ESPGAS','D')
 ENDIF
 SET ORDE TO CALEN4
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  ON KEY LABEL F10 KEYBOARD CHR(23)
  DO CASE
    CASE mVariable==' '   && En edici¢n
      @ ROW(),mCol   SAY mValor
      @ ROW(),mCol+6 SAY mDescr
      RETURN .T.
    CASE mVariable=='A'   && En edici¢n SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN
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


FUNCTION val_prv   && VALIDA PROVEEDORES
*---------------
PARAMETER xcod,_tipo,_x,_y     && codb : codigo ;   _tipo : 1=valida, nada:descripci¢n
** _tipo = .F. ---> Campo
**         .T. ---> Variable.
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo

medita = (PARAMETERS()>=2)
mmsg   = (PARAMETERS()=4) .AND.  _tipo

_campo = VARREAD()

malias = ALIAS()
SELECT promae
_oldwnd = WOUTPUT()

IF !medita
	SEEK xcod
	v_fun = IIF(FOUND(),nompro,"")
	v_reg = estado
ELSE
	IF EMPTY(xcod)
		SET ORDE TO PROMAE2
*		ON KEY LABEL F10
		ON KEY LABEL enter KEYBOARD CHR(23)
		ON KEY LABEL f2  DO prvbus
*       ON KEY LABEL f3  DO promae3
		DEFINE WINDOW _xx FROM 3,3 TO 22,77
		BROWSE WINDOW _xx TITLE ' ®Enter¯ Selecciona  ®F3¯ Agregar  ®F2¯ Busca   ' NOLGRID NOEDIT NOAPPEND NODELETE NOMENU FIELDS;
			codprv   :H='C¢digo'     ,;
			nompro   :H='Nombre':45  ,;
			dirpro   :H='Direccci¢n' :25
		ON KEY LABEL enter
		ON KEY LABEL f2
*    	ON KEY LABEL f3
		RELEASE WINDOW _xx
		SET ORDER TO 1
		IF !EMPTY(_oldwnd)
			ACTIVATE WINDOW &_oldwnd
		ENDIF

		IF LASTKEY()=27
			v_fun = .F.
		ELSE
			xcod = codprv
			v_reg = estado
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
			v_reg = estado
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
		SET ORDER TO 1
		SET FILTER TO SUBSTR(codart,2,2)=SUBSTR(ALLTRIM(parma.codigo),2,2)  &&AND LEFT(CODART,1)='B'
		ACTIVATE WINDOW standby
		@ 1,14 SAY 'Espere un Momento ....' COLOR W*
		GO TOP

		IF EOF()
			DEACTIVATE WINDOW standby
			ACTIVATE SCREEN
			SET FILTER TO
			v_fun = .F.
		ELSE

			DEACTIVATE WINDOW standby
			ACTIVATE SCREEN
			ON KEY LABEL f10 KEYBOARD CHR(23)
*		ON KEY LABEL f8 DO borart
*		ON KEY LABEL f5 DO agrart
			ON KEY LABEL f2 DO funbus
			DEFINE WINDOW _busart FROM 2,02 TO 22,77
			BROWSE WINDOW _busart TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU FIELDS;
				codart   :H='C¢digo'    :W=EMPTY(SUBSTR(codart,5,3)),;
				descri   :H='Nombre':60 :W=EMPTY(descri)
			ON KEY LABEL f10
*		ON KEY LABEL f8
*		ON KEY LABEL f5
			ON KEY LABEL f2
			RELEASE WINDOW _busart
			SET ORDE TO 1
			SET FILTER TO
			IF !EMPTY(_oldwnd)
				*            ACTIVATE WINDOW &_OldWnd
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
*		ON KEY LABEL f8 DO borrdet
*		ON KEY LABEL f5 DO agredet
		ON KEY LABEL f2 DO funbusdet
		DEFINE WINDOW _busart FROM 2,02 TO 22,77
		BROWSE WINDOW _busart TITLE '²²²² [F10] Selecciona   [F2] Buscar ²²²²' NOLGRID  NOAPPEND NODELETE NOMENU KEY 'B'+xcod FIELDS;
			codart   :H='C¢digo'     :W=EMPTY(SUBSTR(codart,8,3)),;
			descri   :H='Nombre':70  :W=SUBSTR(CODART,7,1)#'.' &&EMPTY(descri) ,;
			coduni   :H='Unidad':7   :W=SUBSTR(CODART,7,1)#'.' &&EMPTY(coduni)
*           CodPart  :H='Partd':5  :w=empty(CodPart)
		ON KEY LABEL f10
*		ON KEY LABEL f8
*		ON KEY LABEL f5
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

PROCEDURE agrart
*---------------
vcodart = LEFT(produ.codart,4)
IF f_appd()
	REPLACE codart WITH vcodart ,fecreg WITH DATE()
	RETURN .T.
ENDIF
RETURN .F.

PROCEDURE borart
*---------------
IF RLOCK()
	IF yesno('Desea borrar esta Clasificaci¢n?')
		DELETE NEXT 1
	ENDIF
ELSE
	DO standby WITH 'No puede eliminar este Item.'
ENDIF
RETURN .T.


PROCEDURE agredet
*---------------
vcodart = SUBSTR(produ.codart,2,6)
vtipart = SUBSTR(produ.codart,1,1)
IF f_appd()
	REPLACE codart  WITH vcodart+'.',tipart WITH vtipart ,fecreg WITH DATE()
	RETURN .T.
ENDIF
RETURN .F.

PROCEDURE agr1det
*---------------
PRIVATE vfun
vfun=.F.
zz=val_para(codart,'CODGEB','C')
IF zz
	_cod=ALLTRIM(parma.codigo)
	xx = val_art(' ',.F.)
	IF xx
	    vcodart = SUBSTR(produ.codart,2,6) && original
		vtipart = SUBSTR(produ.codart,1,1)
		SELECT Parma
		SEEK 'CORREL'+'CODART'
		IF f_appd()
			REPLACE codart  WITH vcodart+'.',tipart WITH vtipart ,fecreg WITH DATE()
			vfun=.T.
		ENDIF
	ELSE
		IF f_lock(1)
			REPLACE itepec.codart  WITH SPAC(11)
		ENDIF
	ENDIF
ENDIF
SET ORDER TO 1
ON KEY LABEL f10 KEYBOARD CHR(23)
ON KEY LABEL f8 DO borrdet
ON KEY LABEL f5 DO agr1det
ON KEY LABEL f2 DO funbusdet
RETURN vfun

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


*FUNCTION val_fun
*---------------
PARAMETER v__al,v__dev,v__bus,v__cod,v__tipo,v__x,v__y
PRIVATE medita, mmsg, malias, v_fun, _oldwind,_campo, mvali, mrec
* Alias :     v__al
* Var.Devol.: v__dev
* Var.Mostr.: v__bus
* Var.Modif.: v__cod
* Tipo      : v__tipo : 1=variable    2=campo   3=solo valida
* Posiciones: v__x, v__y
medita = (PARAMETERS()>=5) .AND.  (v__tipo # 3)
mmsg   = (PARAMETERS()=7)  .AND.  (v__tipo # 3)
mvali  = (PARAMETERS()=5)  .AND.  (v__tipo = 3)
mrec   = IIF(EOF(),-1,RECNO())
malias = ALIAS()
IF medita
	_campo = TRIM(malias)+'.'+IIF(v__tipo >1, VARREAD(), v__dev)
ENDIF

SELECT &v__al
_oldwnd = WOUTPUT()

IF !medita
	SEEK v__cod
	v_fun = IIF(mvali,FOUND(),IIF(FOUND(),&v__bus,""))
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

			IF !EMPTY(_oldwnd) .AND. v__tipo#2
				ACTIVATE WINDOW &_oldwnd
			ENDIF
			IF LASTKEY()=27
				v_fun = .F.
			ELSE
				v__cod = &v__dev
				IF mmsg
					@ v__x,v__y SAY &v__bus
				ENDIF
				IF !EMPTY(malias)
					SELECT (malias)
					IF (v__tipo  =2)
						IF mrec>0
							GO mrec
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
		IF mmsg .AND. FOUND()
			@ v__x,v__y SAY &v__bus
		ENDIF
		v_fun = FOUND()
	ENDIF
ENDIF

IF EMPTY(malias)
	SELECT 0
ELSE
	SELECT (malias)
ENDIF

IF !EMPTY(ALIAS())
	IF mrec>0
		GO mrec
	ELSE
		GO BOTTOM
	ENDIF
ENDIF
RETURN v_fun


FUNCTION prvbus
*--------------
IF escolor
	DEFINE POPUP _mm FROM 16,54 SHADOW COLOR &l_col
ELSE
	DEFINE POPUP _mm FROM 16,54 COLOR SCHEME c_popup
ENDIF
ON KEY LABEL ENTER
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
		ON KEY LABEL enter KEYBOARD CHR(23)
		RETURN
ENDCASE
RELEASE POPUP _mm
SET ORDER TO (orden)
IF !FOUND()
	DO standby WITH 'Proveedor no encontrado'
ENDIF
ON KEY LABEL enter KEYBOARD CHR(23)
RETURN



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
			SET ORDER TO ArtMae1
*			SET ORDER TO 1
			SEEK ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO ArtMae2
*			SET ORDER TO 2
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
			SET ORDER TO IteArt1
*			SET ORDER TO 1
			SEEK 'B'+ALLTRIM(_cod)
		ENDIF
	CASE BAR()=2
		ACTIVATE WINDOW _funbus
		_cod = SPACE(40)
		@01,02 SAY 'Nombre: ' GET _cod PICTURE '@S30'
		READ
		DEACTIVATE WINDOW _funbus
		IF LASTKEY() # 27
			SET ORDER TO IteArt2
*			SET ORDER TO 2
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

PROCEDURE clrscr
*---------------
* Autor: CCA
* Borra la pantalla desde el centro
*
PRIVATE fil,COL
fil = 11
COL = 39
DO WHILE (COL > 0) .AND. (fil > 0)
	@ fil,COL CLEAR TO 23-fil,79-COL
	fil = fil - 1
	COL = COL - 3
	= INKEY(.001,'H')
ENDDO
CLEAR
RETURN

PROCEDURE clrscr1
*---------------
* Autor: Luis Castillo Duran
* Borra la pantalla desde el centro
*
PRIVATE fil,COL
fil = 0
COL = 39
col1= 40
DO WHILE (COL > 0)
	@ fil,COL  CLEAR TO 24,COL
	@ fil,col1 CLEAR TO 24,col1
	COL = COL - 1
	col1= col1+ 1
	FOR y=1 TO 150
	NEXT y
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
altura    = num_lines + 3 + 2  &&  Altura de la ventana a mostrar.
start_line= (25 - altura)/2    &&  starting row for box

DEFINE WINDOW poperr FROM start_line,13 TO start_line+altura,66 DOUBLE ;
	SHADOW COLOR SCHEME 5

ACTIVATE WINDOW poperr

***  paint the message on the screen line by line

IF num_lines = 1
	_mens = PADC(err_mess,48)
	FOR j = 1 TO LEN(_mens)
		@ 1,1+j SAY SUBSTR(_mens,j,1)
		* =INKEY(0.0001,' ')
	ENDFOR
	i = 2
ELSE
	FOR i = 1 TO num_lines
		say_mess = LTRIM( MLINE(err_mess,i) )
		* @ i,02 SAY LTRIM(say_mess)
		FOR j = 1 TO LEN(say_mess)
			@ i,1+j SAY SUBSTR( say_mess,j,1)
			*  =INKEY(0.001,' ')
		ENDFOR

	ENDFOR
ENDIF

***  draw a line across the bottom of the message and
***  paint "press any key..."

@ i+1,01 SAY REPLICATE(CHR(196),49)      &&  draw line
_mens = 'Presione una tecla para continuar ....'
FOR j = 1 TO LEN(_mens)
	@ i+2,1+j SAY SUBSTR(_mens,j,1)
	=INKEY(0.001,' ')
ENDFOR

rvalue = INKEY(0,'hm')        &&  hide the cursor

IF WEXIST('poperr')
	RELEASE WINDOW poperr
ENDIF

IF EMPTY(cur_win)
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
@ 2,21 GET xxx PICTURE "!!!!!" COLOR ,x
READ
RELEASE WINDOW _pasw
IF LASTKEY()#27 .AND. xxx=_psw
	RETURN .T.
ELSE
	DO standby WITH "Password incorrecto. ! Acceso denegado !"
ENDIF

RETURN .F.



FUNCTION password
*----------------
PARAMETERS pw_len
PRIVATE pw_len, pass_str, KEY, cur_curs

*PUSH KEY CLEAR

IF PARAMETERS() = 0           &&  if no parameters passed
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
KEY = 0

cur_curs = SET('cursor') = 'ON'   &&  current cursor state
SET CURSOR ON
cur_win = WOUTPUT()

DEFINE WINDOW pass_win FROM 08,20 TO 15,60 DOUBLE ;
	COLOR SCHEME 21
ACTIVATE WINDOW pass_win

@ 02,09 SAY 'Ingrese su password'
@ 03,12 SAY '(' + LTRIM(STR(pw_len)) + ' caracteres)'
@ 05,15 SAY ''

DO WHILE m.key # 13 .AND. m.key#27             &&  accept until car_return
	SET COLOR OF NORMAL TO x
	m.key = INKEY(0)

	DO CASE
		CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122)
			pass_str = pass_str + CHR(m.key)
		CASE m.key = 19 .OR. m.key = 127 .OR. m.key = 7
			@ ROW(),COL()-1 SAY ' '
			@ ROW(),COL()-1 SAY ''
			pass_str = SUBSTR(pass_str,1,LEN(pass_str)-1)
		OTHERWISE
	ENDCASE

	IF LEN(pass_str) >= pw_len
		m.key = 13
	ENDIF

	SET COLOR OF NORMAL TO
	@ 05,15 SAY SPACE(pw_len)
	@ 05,15 SAY REPLICATE('',LEN(pass_str))
ENDDO

RELEASE WINDOW pass_win

IF EMPTY(cur_win)
	ACTIVATE SCREEN
ENDIF

IF .NOT. cur_curs             &&  if cursor was off
	SET CURSOR OFF              &&  set it back on
ENDIF

*POP KEY

RETURN(pass_str)


FUNCTION val_tc
*--------------
PARAMETER vfecha,vretval

malias  = ALIAS()
SELECT tipcam
GO TOP
IF EOF()
	DO standby WITH 'No existen tipos de cambio registrados'
	SELECT IIF(EMPTY(malias),0,(malias))
	RETURN
ENDIF
SEEK DTOC(vfecha,1)

_oldwnd = WOUTPUT()
ACTIVATE SCREEN
ON KEY LABEL enter  DO sel_tc
ON KEY LABEL f10    DO sel_tc
**ON KEY LABEL LEFTARROW   KEYBOARD CHR(15)   && SALTA CAMPO PARA ATRAS
ON KEY LABEL rightarrow  KEYBOARD CHR(9)    && TAB:

DEFINE WINDOW _wintc FROM 05,32 TO 13,78  COLOR SCHEME 8;
	TITLE ' Para seleccionar presione   Enter  ¢  F10 '
BROWSE NOEDIT NOAPPEND NODELETE   WINDOW  _wintc FIELDS ;
	fecha   :W=.F.,;
	compar  :H='Compra ML'  :p='9,999.99',;
	venpar  :H='Venta ML'  :p='9,999.99',;
	combco  :H='Compra Bco' :p='9,999.99',;
	venbco  :H='Venta Bco' :p='9,999.99'
**     moneda  :W=.F.  :H='',;

ON KEY


IF !EMPTY(_oldwnd)
	ACTIVATE WINDOW &_oldwnd
ENDIF

IF SUBSTR(VERSION(),12)="2.0"
	SELECT IIF(EMPTY(malias),SELECT(0),(malias))
ELSE
	IF !EMPTY(malias)
		SELECT (malias)
	ENDIF
ENDIF
RETURN


PROCEDURE sel_tc
*---------------
ON KEY
vcampo  = VARREAD()
vretval = &vcampo
KEYBOARD CHR(23)
RETURN


*PROCEDURE Afox_ambi
*-----------------
* ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
* º                                                                     º
* º         AMBIENTE DE TRABAJO DEL SISTEMA DE ABASTECIMIENTO           º
* º                                                                     º
* º                                                                     º
* º  SAIKI Consultores   Av. Arequipa 1130 Dpto 02  Telf.  709612       º
* º                                                                     º
* ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½

* COMPLETE LIST OF SET COMMANDS

* Procedimineto general para los errores de entrada.
ON READERROR         DO   err_input
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
SET DECIMALS TO      3   && Default
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
SET HOURS TO         12
* SET INDEX          NOT RELEVANT TO SYSTEM ENVIRONMENT
SET INSTRUCT         ON
SET INTENSITY        ON
IF worker
	SET LOGERROR       ON
ELSE
	SET LOGERROR       OFF
ENDIF
SET MARGIN TO        0
SET MARK TO          "-"  && THIS IS THE DATE FIELD SEPARATOR
*  THE DEFAULT IS "/"
*  WE'RE USING SOMETHING DIFFERENT HERE
SET MEMOWIDTH TO     80
SET MENU             ON
SET MESSAGE TO       23
SET NOTIFY           ON
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
SET STICKY           ON
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
DEFINE WINDOW err_input  FROM 01,60 TO 03,77

* Define standard system communication windows
DEFINE WINDOW yesno      FROM 19,27 TO 23,77 DOUBLE FLOAT GROW SHADOW COLOR SCHEME 5

* Typical system messages:
DEFINE WINDOW msg2user   FROM 04,02 TO 08,77 DOUBLE FLOAT SHADOW

* Press any key or click mouse to continue messages:
DEFINE WINDOW standby    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW COLOR SCHEME 5

DEFINE WINDOW _funbus    FROM 19,27 TO 23,77 DOUBLE FLOAT SHADOW

* VARIABLE PARA EL BLOQUEO
PUBLIC ESCAPE
ESCAPE = 27

* SYSTEM VARIABLE SETTINGS
_ALIGNMENT =  "LEFT"
_BOX =        .T.
_INDENT =     0
_LMARGIN =    0
_PADVANCE =   "FORMFEED"
_PAGENO =     1
_PBPAGE =     1
* _PCOLNO     SYSTEM MAINTAINED VARIABLE
_PCOPIES =    1
*_PDRIVER =    &&GENERIC.PR2
_PECODE =     ""
_PEJECT =     "AFTER"
_PEPAGE =     32767
_PFORM =      ""
_PLENGTH =    60
* _PLINENO =  SYSTEM MAINTAINED VARIABLE
_PLOFFSET =   0
_PPITCH =     "PICA"
_PQUALITY =   .T.
_PSCODE =     ""
_PSPACING =   1
_PWAIT =      .F.
_RMARGIN =    80
_TABS =       "10, 20, 30, 40, 50, 60, 70"
_WRAP =       .T.
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
SET HOURS TO         12
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


FUNCTION isdisket
*----------------
PARAMETER _drive
PRIVATE m.drive, vret

vret = .T.
m.drive = _drive
LOAD isdiskin.bin
CALL isdiskin WITH m.drive
DO WHILE m.drive='0:'
	??CHR(7) + CHR(7)
	DO standby WITH '! El drive '+_drive+ ' no est  listo !'
	IF LASTKEY()=27
		vret = .F.
		EXIT
	ENDIF
	m.drive = _drive
	CALL isdiskin WITH m.drive
ENDDO
_drive = m.drive
RELEASE MODULE isdiskin.bin

RETURN vret


FUNCTION pidefec
*---------------
PARAMETERS _titu, vfecini, vfecfin

IF PARAMETERS()=1
	RETURN .F.
ENDIF

PRIVATE _retval
_retval=.T.

ff = IIF(PARAMETERS()=2,20,21)

ACTIVATE WINDOW standby

@ 00,00 SAY  PADC(_titu, WCOLS(),' ')
IF PARAMETERS()=2
	@ 01,04 SAY 'Fecha  : ' GET vfecini  ;
		ERROR 'Fecha Inv lida'
ELSE
	@ 01,04 SAY 'Inicio : ' GET vfecini  ;
		ERROR 'Fecha Inv lida'
	@ 02,04 SAY 'Final  : ' GET vfecfin  VALID vfecini<=vfecfin ;
		ERROR 'Fecha Inv lida'
ENDIF
READ
DEACTIVATE WINDOW standby
IF LASTKEY()=27
	_retval=.F.
ENDIF
RETURN _retval

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


FUNCTION val_read
*----------------
* Valida al terminar un Read
* Ejemplo:  READ VALID Val_Read()
* Autor: C‚sar Ch varry Arias   Fecha: 14-09-93
PRIVATE _fun
_fun = .T.
IF LASTKEY() # 27
	IF .NOT. yesno('¨ Est n correctos los datos ?')
		_fun = .F.
	ENDIF
ENDIF

RETURN (_fun)


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
	CASE vest='10'
		vfun ='Programado   '
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
		vfun ='Liquidado     '
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
			SELECT distinct codpart FROM Cheque INTO CURSOR query
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

	SELECT ITEOC.PERIODO, ITEOC.TIPFUN, ITEOC.ESTADO, ITEOC.CODFTE, ;
		ITEOC.VALTOT, ITEOC.CODPART,ITEOC.CODANAL ;
	  FROM ITEOC ;
	  WHERE nummes+tipfun+codfte+periodo+SUBSTR(ITEOC.codCAL,8,2)+iif(!empty(vCodsub),SUBSTR(ITEOC.CODCAL,10,3),'') + iif(!empty(vProyec),SUBSTR(ITEOC.CODCAL,13,3),'') + iif(!empty(proact),SUBSTR(ITEOC.CODCAL,16,2),'') ;
		= alltrim(m.Nummes)+alltrim(m.tipfun)+alltrim(m.codfte) + xvalor + iif(!empty(proact),proact,'') AND iif(VTIPO=1 AND ALLTRIM(M.TIPFUN)='F',CODPART='02',IIF(VTIPO=2 AND ALLTRIM(M.TIPFUN)='F',CODPART='03',.T.)) AND TIPFUN='I';
		  INTO CURSOR ITEOCX

*  SELECT ITEOC.PERIODO, ITEOC.TIPFUN, ITEOC.ESTADO, ITEOC.CODFTE, ITEOC.CODCAL

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


FUNCTION SavCon
*---------------
USE UsuGRAU IN 0 ALIAS Usu
PUBLIC _Hora
SET RESO ON
SELECT Usu
APPEND BLANK
_hora  = TIME()

REPLACE CodUsu  WITH vconex
REPLACE machine WITH vmaq
replace tipmaq with substr(sys(17),3)
REPLACE Prog    WITH SYS(16,1)
REPLACE Hora    WITH _hora
REPLACE dia     WITH DAY(DATE())
REPLACE LLAVE   WITH vllav
vUsurec = recno()
USE
RETURN

FUNCTION BorCon
*-----------------
USE UsuGRAU IN 0 ALIAS Usu
SELECT Usu
GO TOP
SET EXACT ON
DO WHILE !EOF()
	LOCATE FOR Codusu=vconex AND machine = vmaq AND llave = vllav
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
   CASE vEst='50'
      vFun ='C/P:'+M.numcp + '.'+M.numMEScp+' '+ALLTRIM(M.Codctc)
   ENDCASE
RETURN vFun

FUNCTION VerEsthc
*--------------
PARAMETER vEst
PRIVATE vFun
   vFun = SPACE(10)
   DO CASE
   CASE vEst='00' or vEst='  '
      vFun ='Emitido     '
   CASE vEst='20'
      vFun ='Con Cta.Cte. '
   CASE vEst='52'
      vFun ='Regularizado '
   CASE vEst='54'
      vFun ='Con N/C '
   CASE vEst='50'
      vFun ='C/P:'+M.numcp + '.'+M.numMEScp+'   '
   CASE vEst='90'
      vFun ='Con H/Modifc'
   CASE vEst='99'
      vFun ='Anulado     '
   ENDCASE
RETURN vFun

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

SELECT AuxiL
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
	SELE AUXIL
	SET ORDER TO Auxil1
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
	SET FILTER TO
ENDIF
mvalor = AuxiL.codigo
mdescr = SUBSTR( AuxiL.descri, 1, mlong )
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

FUNCTION val_pla
*----------------
PARAMETERS mvalor,vtipo
PRIVATE malias,vFun
malias  = ALIAS()
morden  = ORDER()
vfun = .f.
SELECT VEHIC
SEEK mvalor
IF !FOUND() OR EMPTY(MVALOR)
	_oldwnd = WOUTPUT()
	ACTIVATE SCREEN
	GO TOP
	IF EOF()
		DO standby WITH 'No existen Registros para Procesar'
		IF !EMPTY( malias )
			SELECT (malias)
			SET ORDER TO (morden)
			VFUN=.F.
		ENDIF
	ENDIF
	DEFINE POPUP parametro FROM 03,25 PROMPT FIELD codpla+'°'+clase+' '+color
	ON SELECTION POPUP parametro DEACTIVATE POPUP
	ACTIVATE POPUP parametro
	IF !EMPTY( _oldwnd)
		ACTIVATE WINDOW &_oldwnd
	ENDIF
	RELEASE POPUP parametro
ELSE
    VFUN = .T.	
ENDIF
mvalor = VEHIC.codpla
IF !EMPTY( malias )
	SELECT (malias)
    SET ORDER TO (morden)
ENDIF
IF LASTKEY()=27
   mvalor = '       ' 
   RETURN .F.
ENDIF   
do case
   case vtipo = 'T'
        RETURN (VEHIC.codpla)
   case vtipo = 'C'
        m.codpla = VEHIC.codpla
        return .T.
   case vtipo = 'V'
        return iif(vfun,.t.,.f.)
   other
        return .f.     
ENDCASE 

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
 SET FILTER TO maepre.periodo+maepre.uniges+maepre.unieje = Filtro+"01001"
 SEEK Filtro + "01001" + mValor

  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd1 = WOUTPUT()
    ACTIVATE SCREEN
    *SET FILTER TO PERIODO = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       IF !EMPTY (mAlias)
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
     	UNIGES	: H='GEST.',;
     	UNIEJE	: H='EJEC.',;
    	CODFUN	: H='Fn',;
    	CODPRG	: H='Prg',;
    	CODSPR	: H='SPrg',;
    	ACTPRY	: H='Act/Pry',;
    	CODCOM	: H='CodComp',;
    	XX = VAL_PARA(CODCOM,'CODCOM','D'):H='Descripci¢n':25,;
    	CODMET	: H='Meta',;
    	Descri	: H='Descripci¢n':20
	
    	
    RELEASE WINDOWS wind_cad
    ON KEY LABEL f2  
    ON KEY LABEL f3  
    IF !EMPTY(_OldWnd1)
       ACTIVATE WINDOW &_OldWnd1
    ENDIF
    *SET FILTER TO
  ENDIF
  SET FILTER TO
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
DEFINE WINDOW lista FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda X C¢digo °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lista
vcodcad  = space(4)
@  3,2 SAY '  Cadena : ' GET vcodcad    PICTURE '!!!!' 
READ
RELEASE WINDOWS lista
SEEK filtro+vcodcad
IF !FOUND()
	Do standby WITH 'No existe Codigo de Cadena...'
	go vcursor
ENDIF
RETURN


Procedure busDesCad
*------------------
vcursor = RECNO()
vorder  = ORDER()
DEFINE WINDOW lista FROM 09,12 TO 16,68 DOUBLE ;
	TITLE ' °° B£squeda °° ' FLOAT COLOR SCHEME 5
ACTIVATE WINDOW lista
vCODCOM = space(5)
@  3,2 SAY ' Componente : ' GET vCodcom   PICTURE '!!!!!' VALID VAL_PARA(vCODCOM,'CODCOM',' ',18,30)
READ
RELEASE WINDOWS lista
locate for codcom=ALLT(vcodcom)
IF !FOUND()
	Do standby WITH 'No existe Codigo de Componente...'
	go vcursor
ENDIF
RETURN

  
FUNCTION Val_comp
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
vrecno = Recno()
IF SISTEMA ='2'
	SET ORDER TO MAEPRE2
ENDIF	
SEEK vfiltro
vBusca1 = PERIODO+UNIGES+UNIEJE+CODFUN+CODPRG+CODSPR+ACTPRY
SET ORDE TO maepre4
SEEK vBusca1+vbusca
IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	SEEK vBusca1
	vkey=periodo+uniges+unieje+codfun+codprg+codspr+actpry
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry=vkey
	go top

    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Componentes ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad  FIELD ;
		CODCOM	: H='CodComp',;
		xx=val_para(codcom,'CODCOM','D'):h='Descripci¢n':25
    RELEASE WINDOWS wind_cad
    SET FILTER TO
endif

mValor = maepre.codcom
mDescr = val_para(maepre.codcom,'CODCOM','D')
IF SISTEMA='2'
	set orde to maepre3
ELSE
	set orde to maepre1
ENDIF
go vrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
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
RETURN


FUNCTION Val_META
*----------------
PARAMETER vfiltro,vBusca,mvariable
valias = alias()
SELE MAEPRE
vrecno = Recno()
IF SISTEMA ='2'
	SET ORDER TO MAEPRE2	
ENDIF
SEEK vfiltro
vBusca1 = PERIODO+UNIGES+UNIEJE+CODFUN+CODPRG+CODSPR+ACTPRY
SET ORDE TO maepre4
SEEK vBusca1+vbusca

IF !FOUND()
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
	SEEK vBUSCA1
	vORD1=periodo+uniges+unieje+codfun+codprg+codspr+actpry+CODCOM
	SET FILTER TO periodo+uniges+unieje+codfun+codprg+codspr+actpry+CODCOM=vORD1
	go top
    ON KEY LABEL f10 KEYBOARD CHR(23)
    DEFINE WINDOW wind_CAD1 FROM 05,20 TO 15,60  DOUBLE ;
		TITLE  ' ± Metas ± ' FOOTER ' ° ®F10¯ Graba ° ' COLOR SCHEME 15
	ACTIVATE WINDOWS wind_cad1
	BROWSE ;
		NOAPPEND NOEDIT NODELETE NOMENU NOCLEAR NOOPTIMIZE NOREFRESH TITLE 'Relaci¢n de Cadenas Funcionales ';
		IN WINDOW wind_cad1  FIELD ;
		CODMET	: H='META',;
		DESCRI  : h='Descripci¢n':30
    RELEASE WINDOWS wind_cad1
    SET FILTER TO
endif
mValor = maepre.codMET
mDescr = maepre.DESCRI
IF SISTEMA='2'
	set orde to maepre3
ELSE
	set orde to maepre1
ENDIF
go vrecno
IF !EMPTY( vAlias )
    SELECT (vAlias)
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
RETURN


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
    *............
    * no se usa
    *
    * ON KEY LABEL f2  DO Completo
    * ON KEY LABEL f3  DO PorCompon
    *................

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

FUNCTION DialBox
*---------------
PARAMETERS TipDial,MsgDial,TitDial
* TipDial  1 Visualiza unicamente el bot¢n Aceptar
*          2 Visualiza 2 botones Aceptar y cancelar
*          3 Visualiza 2 botones Si y No
* MsgDial  Cadena que contiene el mensaje a visualizar
* TitDial  Titulo del cuadro de di logo (opcional)

IF PARAMETERS()=2
   TitDial=""
ENDIF   

DEFINE WINDOW w1 ;
		FROM 7,15 TO 16,64 ;
		NOFLOAT ;
		NOCLOSE ;
		NOMINIMIZE ;
		NONE ;
		COLOR SCHEME 5

ACTIVATE WINDOW w1 

IF !EMPTY(TitDial)
   @ 0,0 SAY PADC(ALLT(TitDial),50,' ') COLOR '0/7'
ENDIF   

   msgDial = ALLTRIM(msgDial)
   _ln      = LEN(msgDial)
   _colW    = WCOLS()
   msgDial  = iif(_ln>2*_colW,PADR(msgDial,2*_colW,' '),msgDial)
   _ln      = LEN(msgDial)

   msg1     = IIF(_ln>_colW,LEFT(msgDial,_colW),msgDial)
   msg2     = IIF(_ln>_colW,SUBSTR(msgDial,_colW+1),"")

   IF _ln>_colW
      FOR I=0 TO _colW/3-1
	 IF SUBSTR(msg1,_colW-i,1) == " "
	    msg1 = LEFT(msg1,_colW-i)
	    msg2 = SUBSTR(msgDial,_colW-i+1)
	    msg2 = IIF(LEN(msg2)>_colW,LEFT(msg2,_colW),msg2)
	    EXIT
	 ENDIF
      ENDFOR
   ENDIF


   @ 03,(wcols()/2-LEN(msg1)/2)+1  SAY msg1
   IF !EMPTY(msg2)
      @ 04,(wcols()/2-LEN(msg2)/2)+1  SAY msg2
   ENDIF

DO CASE 
   CASE TipDial=1
	@ 08,19 GET okcan ;
		PICTURE "@*HT \! Aceptar " ;
		SIZE 1,11,5 ;
		DEFAULT 1 ;
		COLOR SCHEME 5

   CASE TipDial=2
	@ 08,12 GET okcan ;
		PICTURE "@*HT \! Aceptar ;Cancelar" ;
		SIZE 1,11,5 ;
		DEFAULT 1 ;
		COLOR SCHEME 5
   CASE TipDial=3
	@ 08,13 GET okcan ;
		PICTURE "@*HT \! Contin£a ;Cancelar" ;
		SIZE 1,11,5 ;
		DEFAULT 1 ;
		COLOR SCHEME 5

ENDCASE         
@ 7,0 TO 7,49
READ CYCLE
RELEASE WINDOW w1
IF LASTKEY()=27
   OkCan=2
ENDIF   
RETURN OkCan=1


PROCEDURE ESPERA1 
*-----------------------------------------------------------
* Esta funcion activa una pantalla de Espera 
* en caso de que se procesada a indexar o demorar el proceso
* Desarrollado por : FMC Saiki. Sucursal Piura
* Fecha : 11/01/96
*-----------------------------------------------------------
PARAMETER festado,fmensaje
IF PARAMETERS()=1
   fmensaje='Espere Un Momento ...'
ENDIF   
   
   DEFINE WINDOW Espera  FROM 21,79-len(allt(fmensaje))-1 TO 23,79 COLOR SCHEME 5
   DO CASE 
      CASE festado=1
      	   ACTIVATE WINDOW Espera
	  	   @ 0,0 SAY ALLTRIM(fmensaje) COLOR 7+*/5
	  	   RETURN
	  CASE festado=2
		   DEACTIVATE WINDOW Espera
		   RETURN 
	ENDCASE
RETURN

PROCEDURE progreso
*-----------------
PARAMETER control,Cmens,total,actual
DO CASE
   CASE control=1 && pinta el cuadro
	define window vView from 16,10 to 21,70 COLOR SCHEME 1
	ACTIVATE WINDOW Vview
	@ 00,00 SAY Cmens COLOR '3+/1'
	@ 2,0 SAY REPL('°',59) color '3/1'
	@ 3,0 SAY REPL('°',59) color '3/1' 
   CASE control=2 && pinta barra de avance
	vtotC=59
	porcA=ROUND((actual*100)/total,2)
	NumC=ROUND((porcA*vTotC)/100,0)
	@ 01,55 SAY porcA PICTURE '999%'
	@ 2,0 SAY REPL('Û',numc) COLOR 'GR+/1'
	@ 3,0 SAY REPL('Û',numc) COLOR 'GR+/1'
   CASE control=3 && borra el cuadro
	RELEASE WINDOW vview
ENDCASE
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

FUNCTION Val_prv1
*----------------
  PARAMETERS mValor, Filtro, mVariable, mCol, mLong , mDist
  PRIVATE mAlias,mPeriodo
* mPeriodo = m.Periodo
  
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
  SELECT AuxiL
  SET ORDER TO TAG auxil1
  SEEK Filtro+IIF(!EMPTY(mvalor),ALLTRIM(mValor),mvalor)
                             
  IF !FOUND() .AND. !mVariable $'VZ'
    _OldWnd = WOUTPUT()
    ACTIVATE SCREEN
    SET ORDER TO TAG auxil10
    SET FILTER TO Tipo = Filtro
    GO TOP
    IF EOF()
       DO STANDBY WITH 'No existen Registros para Procesar'
       SET FILTER TO
       SET ORDER TO TAG auxil1
       IF !EMPTY( mAlias )
           SELECT (mAlias)
       ENDIF
	   return	
    ENDIF
    
      on key label ENTER keyboard chr(23)
      on key label F2  do funbus1 
      define window _xx from 3,02 to 22,60 COLOR scheme 10
      browse window _xx title ' ±± Cat logo de '+IIF(Filtro='20','Proveedores',IIF(filtro='30','Empleados','Otros'))+' ±±  ®F2¯ Busca ' nolgrid noedit noappend nodelete nomenu fields;
         codigo   :h='C¢digo'     ,;
         descri   :h='Descripci¢n':50  
      on key label ENTER
      on key label f2

    IF !EMPTY( _OldWnd)
       ACTIVATE WINDOW &_OldWnd
    ENDIF

    RELEASE POPUP parametro
    SET FILTER TO
  ENDIF
  mValor = auxil.Codigo
  mDescr = SUBSTR( auxil.Descri, 1, mLong )
  SET ORDER TO TAG auxil1
  IF !EMPTY( mAlias )
    SELECT (mAlias)
  ENDIF
  DO CASE
    CASE mVariable==' '   && En edici¢n
      @ ROW(),mCol       SAY mValor
      @ ROW(),mCol+mDist SAY mDescr
      RETURN &&.T.
    CASE mVariable=='A'   && En edici¢n SOLO DESCRIPCION
      @ ROW(),mCol SAY mDescr
      RETURN ' '
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


