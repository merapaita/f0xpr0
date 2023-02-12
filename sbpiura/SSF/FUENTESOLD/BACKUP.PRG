**-----------------------------------
** Backup. prg
** Copia los archivos del sistema
**-----------------------------------
=PopError( 'La copia de seguridad, tiene el formato: SFddmmaa.ZIP, y '+;
'se graba en el directorio ..\BACKUP, adem s de grabarlo en el diskette.'+ CHR(13) + ;
'Aseg£rese que NADIE use el sistema para empezar el proceso de Backup.')

IF LASTKEY()=27
	RETURN
ENDIF

DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE '    Utilitario de Backup     ' COLOR SCHEME 5
ACTIVATE WINDOW LIS

STORE 1  TO vLista
IF LASTKEY()=27
	RETURN
ENDIF

IF SET("PATH")#RutaPr
	mRuta = SUBSTR(Ruta,AT(";",Ruta)+1)
	_Dirdat=PADR(mRuta+'\',35,' ')
	_Dirdes=PADR(RutaBk,35,' ')
 ELSE
	_Dirdat=PADR(RutaPr+'\',35,' ')
	_Dirdes=PADR(RutaBk,35,' ')
ENDIF

Copia = "SF" + PADL(DAY(DATE()),2,'0') + PADL(MONT(DATE()),2,'0') + SUBSTR(ALLTRIM(STR(YEAR(DATE()))),3,2) + '.ZIP'

@ 01,03 SAY "Nombre del Archivo  : " + Copia
@ 03,03 SAY "Drive del Back UP   : " 
@ 03,26 GET vLista FUNCTION '^ Drive A:   ;Drive B:   ;Drive C:   ' 
@ 06,03 SAY "Dir. Origen de la Data: " 
@ 07,03 GET _Dirdat
@ 09,03 SAY "Dir. Destino / Back UP: " 
@ 010,03 GET _Dirdes
@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ CYCLE
RELEASE WINDOW LIS

DO CASE
	CASE vlista=1
		_drive = 'A:'
	CASE vlista=2
		_drive = 'B:'
	CASE vlista=3
		_drive = 'C:'
ENDCASE

_Dirdat=ALLTRIM(_Dirdat)
_Dirdes=ALLTRIM(_Dirdes)

IF LASTKEY() # 27 AND OKCANCEL=1
	DO STANDBY WITH 'Inserte el diskette en el drive ' + _drive
	IF LASTKEY() = 27
		RETURN
	ENDIF
	
	IF !IsDisKet(_Drive)
		RETURN
	ENDIF
	
	_datfpt =   _dirdat+'*.DBF '+ _dirdat+'*.FPT'
	DO ESPERA WITH 1, "Espere un momento, se est  sacando Copia..!"
	
	RUN FOXSWAP PKZIP &copia &_datfpt     >NUL
	RUN FOXSWAP COPY  &copia &_drive      >NUL
	RUN FOXSWAP COPY  &copia &_dirdes     >NUL
	RUN FOXSWAP ERASE &copia              >NUL
	DO ESPERA WITH 2
	SHOW POPUP MENU
ENDIF

RETURN
