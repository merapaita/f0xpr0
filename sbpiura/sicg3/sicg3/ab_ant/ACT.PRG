*PROGRAMA Act.PRG
*---------------
* PROCEDIMIENTO PARA ACTUALIZAR VERSION DE LA APLICACION
********************************************************
PARAMETERS cArch
archerr = -1

IF FILE('..\NUEVO\'+cArch) 	&& ¨Existe el archivo?
	wait wind "hola encntre"
	archerr = FOPEN(cArch,12) && Si si, abrir lectura/escritura
ENDIF
IF archerr < 0
	*
ELSE
	=FCLOSE(archerr)
	cadena = "..\NUEVO\"+cArch
	WAIT WIND "COPIANDO"
	!COPY &cadena	> NULL
	WAIT WIND "BORRANDO"
	ERASE &cadena	> NULL
ENDIF

QUIT
