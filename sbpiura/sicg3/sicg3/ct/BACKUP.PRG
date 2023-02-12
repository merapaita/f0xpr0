**----------------------------------------------------------------------
** Backup. prg          Sistema
** Copia los archivos del sistema
**----------------------------------------------------------------------
=PopError( 'La copia de seguridad, tiene el formato: BKddmmaa.ZIP, y '+;
'se graba en el directorio ..\BACKUP, adem s de grabarlo en el diskette.'+ CHR(13) + ;
'Aseg£rese que NADIE use el sistema para empezar el proceso de Backup.',.F. )

* ACTIVATE WINDOW STANDBY

    DEFINE WINDOW LIS FROM 5,15 TO 20,65 FLOAT DOUBLE TITLE '    Utilitario de Backup     ' COLOR SCHEME 5
	ACTIVATE WINDOW LIS
	STORE 1  TO vLista
	IF LASTKEY()=27
	   RETURN
	ENDIF
	
	_Dirdat="C:\BENE\DATA\                           "  
	_Dirdes="C:\BENE\BACKUP                           "  
	
    @ 01,01 SAY "  Drive del Back UP   : " 
	@ 01,26 GET vLista FUNCTION '^ Drive A:   ;Drive B:   ' 
	@ 04,01 SAY "Dir. Origen de la Data: " 
	@ 05,01 GET _Dirdat
	@ 07,01 SAY "Dir. Destino / Back UP: " 
	@ 08,01 GET _Dirdes
	@ 12,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
	READ CYCLE
	RELEASE WINDOW LIS
	
	DO CASE
       CASE vlista=1
			_drive = 'A:'
       CASE vlista=2
			_drive = 'B:'
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
    
   dia = DTOC(DATE())
   Copia = "BK" + LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) +'.ZIP'
    _datfpt =   _dirdat+'*.DBF '+ _dirdat+'*.FPT'
   DO ESPERA WITH 1, "Espere un momento, se est  sacando Copia..!"

   RUN FOXSWAP PKZIP &copia  &_datfpt >NUL
   RUN FOXSWAP COPY  &copia   &_drive >NUL
   RUN FOXSWAP COPY  &copia   &_dirdes >NUL
   RUN FOXSWAP ERASE &copia             >NUL
   DO ESPERA WITH 2
   SHOW POPUP MENU
ENDIF

RETURN
