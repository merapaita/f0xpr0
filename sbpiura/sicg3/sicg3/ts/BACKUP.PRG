**----------------------------------------------------------------------
** Backup. prg          Sistema
** Copia los archivos del sistema
**---------------------------------------------------------------------- 
=PopError( 'La copia de seguridad, tiene el formato: TEddmmaa.ZIP, y '+;
'se graba en el directorio ..\DBFBAK, adem s de grabarlo en el diskette.'+ CHR(13) + ;
'Aseg£rese que NADIE use el sistema para empezar el proceso de Backup.',.F. )

ACTIVATE WINDOW STANDBY
_drive = 'A:'
@ 01,01 SAY "En que drive est  sacando el backup:  " GET _drive PICTURE '@M A:,B:'
READ
DEACTIVATE WINDOW STANDBY
IF LASTKEY() # 27
   DO STANDBY WITH 'Inserte el diskette en el drive ' + _drive
   IF LASTKEY() = 27
      RETURN
   ENDIF

   IF !IsDisKet(_Drive)
      RETURN
   ENDIF

   dia = DTOC(DATE())
   HIDE POPUP ALL
   Copia = "TE" + LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) +'.ZIP'
   @ 10,10 CLEAR TO 14,60
   @ 10,10,14,60 BOX
   @ 12,12 SAY "Espere un momento, se est  sacando Copia"
   RUN FOXSWAP PKZIP &copia  ..\DATA\*.DBF  ..\DATA\*.FPT >NUL
   RUN FOXSWAP BACKUP  &copia   &_drive >NUL
   RUN FOXSWAP COPY  &copia   ..\DBFBAK >NUL
   RUN FOXSWAP ERASE &copia             >NUL
   RESTORE SCREEN FROM Principal
   SHOW POPUP MENU
ENDIF

RETURN
