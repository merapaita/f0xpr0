**----------------------------------------------------------------------
** Backup. prg                                  Sistema Uni
** Copia los archivos del sistema
** Autor: Luis Castillo.
**----------------------------------------------------------------------
ACTIVATE WINDOW STANDBY
_drive = 'A:'
@ 01,01 SAY "En que drive est  sacando el backup:  " GET _drive PICTURE '@M A:,B:,C:,F:'
READ
DEACTIVATE WINDOW STANDBY
IF LASTKEY() # 27
   if _drive = 'C:' OR _drive = 'F'
      do backup1 with 1
   else
      DO STANDBY WITH 'Inserte el diskette en el drive ' + _drive
      IF LASTKEY()=27 OR .NOT. IsDisket(_Drive)
         RETURN
      ENDIF
      dia = DTOC(DATE())
      HIDE POPUP ALL
      RESTORE SCREEN FROM Pantalla
      *Copia = 'DATA.ZIP'
      Copia = 'CP'+LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) +'.ZIP'
      @ 10,10 CLEAR TO 14,60
      @ 10,10,14,60 BOX
      @ 12,12 SAY "Espere un momento, se est  sacando Copia"
      RUN FOXSWAP PKZIP &copia  ..\DATA\*.DBF  ..\DATA\*.FPT ..\data\*.tbk > NUL
      RUN FOXSWAP COPY  &copia   &_drive       > NUL
      RUN FOXSWAP COPY  &copia   ..\zip     > NUL
      RUN FOXSWAP ERASE &copia                 > NUL
      RESTORE SCREEN FROM Principal
      SHOW POPUP MENU
   endif
ENDIF

RETURN
