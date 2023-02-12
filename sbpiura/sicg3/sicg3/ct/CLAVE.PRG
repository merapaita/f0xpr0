** --------------------------------
** CLAVE.PRG
** Clave de Acceso
** AUTOR : M.E.R.A. 2,002.  
** --------------------------------

*PARAMETER xUser_ID,xFila1,xColumna1,xFila2,xColumna2
PRIVATE xPassword,m.key,vRetorno,m.cur_curs,m.cur_win,m.Usuario

*-- Regresa las siguientes variables
* xUser_ID = Codigo de usuario

*  El archivo de usuarios se asume que debe estar abierto.

xPassword = ''
vRetorno = .F.
m.KEY = 0

*SUSP
m.cur_curs = SET('cursor') = 'ON'   &&  current cursor state
SET CURSOR ON

m.cur_win = WOUTPUT()

SELECT Usua
*SET FILTER TO CODDEP='700000' OR CODDEP='112000'
DEFINE WINDOW pass_win FROM 08,15 TO 15,65 DOUBLE ;
	TITLE '  Usuario  ' SHADOW COLOR SCHEME 21
ACTIVATE WINDOW pass_win

m.Usuario = SPACE(15)
@ 01,03 SAY 'USUARIO'
@ 01,11 GET m.Usuario VALID Val_Fun("Usua","Usuario","Nombre",@m.Usuario,1,2,11)
READ
IF LASTKEY()#27
	SEEK m.Usuario
	@ 03,09 SAY 'Ingrese su password'
	@ 05,15 SAY ''
	DO WHILE m.key # 13 .AND. m.key#27             &&  accept until car_return
*		SET COLOR OF NORMAL TO X
		m.key = INKEY(0)
		
		DO CASE
			CASE BETWEEN(m.key,65,90) .OR. BETWEEN(m.key,97,122) .OR. BETWEEN(m.key,48,57) .OR. BETWEEN(m.key,164,165)
				xPassword = xPassword + CHR(m.key)
			CASE m.key = 19 .OR. m.key = 127 .OR. m.key = 7
				@ ROW(),COL()-1 SAY ' '
				@ ROW(),COL()-1 SAY ''
				xPassword = SUBSTR(xPassword,1,LEN(xPassword)-1)
			OTHERWISE
		ENDCASE
		
*		SET COLOR OF NORMAL TO
		@ 05,15 SAY REPLICATE('',LEN(xPassword))
	ENDDO
ENDIF

RELEASE WINDOW pass_win

IF EMPTY(cur_win)
	ACTIVATE SCREEN
ENDIF

IF .NOT. cur_curs             &&  if cursor was off
	SET CURSOR OFF            &&  set it back on
ENDIF

IF LASTKEY()#27
	vEncrip = chrtran(UPPER(xPassword),'ABCDEFGHIJKLMN¥OPQRSTUVWXYZ0123456789',;
    	                               'XWAQSD!R$1Z2LH)^CEP&67UIYMTxw%/-+}{?~«¬@›_#')
    	             
	IF vEncrip==ALLTRIM(Usua.Clave)
		@ 22,25 SAY IIF(VAL(SUBSTR(TIME(),1,2)) < 12,'Buenos Dias ',;
			'Buenas Tardes ') + ALLTRIM(Usua.Nombre)
		@ 23,25 SAY 'USUARIO : ' + ALLTRIM(Usua.Nombre)
		WAIT '' TIMEOUT 2
		@  23,25,23,80  BOX "°°°°°°°°°" COLOR SCHEME c_fondo
		vCoddep = Usua.Coddep
		mRet = .T.
	 ELSE
		mRet = .F.
	ENDIF
 ELSE
	mRet = .F.
ENDIF

RETURN(mRet)



****

IF vretorno
	@ 22,25 SAY IIF(VAL(SUBSTR(TIME(),1,2)) < 12,'Buenos Dias ',;
				'Buenas Tardes ') + ALLTRIM(Usua.Nombre)
	@ 23,25 SAY 'USUARIO : ' + ALLTRIM(Usua.Nombre)
	WAIT '' TIMEOUT 2
	@  23,25,23,80  BOX "°°°°°°°°°" COLOR SCHEME c_fondo
	vCoddep = Usua.Coddep
ENDIF
RETURN vretorno

