**----------------------------------------------------------------------
** ResBk. prg                                       Sistema Peruval
** Copia los archivos del sistema
**----------------------------------------------------------------------
PRIVATE dia, copia, _rutbk, _drive, _fecbk

=PopError( "El recuperar el backup elimina la informaci¢n actual." + CHR(13) + ;
           "Se recomienda haber sacado un BACKUP previo, de la informaci¢n actual" + CHR(13)+;
           "Presione ® ESC ¯ para cancelar"  )
IF LASTKEY() = 27
   RETURN
ENDIF

ACTIVATE WINDOW STANDBY
_drive = 'C:'
_rutbk = "..\SALUD\ZIP\"
_fecbk = DATE() - 1

@ 00,01 SAY "Backup del DISCO DURO: lo recibe de ..\ZIP\"
@ 01,01 SAY "Drive de restauraci¢n: " GET _drive PICTURE '@M C:,A:,B:'
@ 02,01 SAY "    Fecha del backup : " GET _fecbk ;
  VALID (ve_fecbk()) ERROR "No hay backup de ese d¡a"
READ
DEACTIVATE WINDOW STANDBY
IF LASTKEY() # 27
   IF _drive#"T:"
      DO STANDBY WITH 'Inserte el diskette BackUp en el drive ' + _drive
      IF LASTKEY() = 27
         RETURN
      ENDIF
   ELSE
     _drive = _rutbk
   ENDIF
   
   HIDE POPUP ALL
   RESTORE SCREEN FROM MainScr
   @ 10,10 CLEAR TO 14,60
   @ 10,10,14,60 BOX 
   @ 12,12 SAY " Espere un momento, se est  restaurando BACKUP "
   fil1 = shr_rut
   fil1 = '..\DATA\'
   dia = DTOC(_fecbk )
   
   IF _drive#"T:"
      Copia = _drive + LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) + '.ZIP'
   ELSE
      Copia = _RutBK + LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) + '.ZIP'
   ENDIF

   RUN FOXSWAP PKUNZIP -O &copia  *.DBF *.FPT &fil1  > NULL
   RESTORE SCREEN FROM Principal
   SHOW POPUP MENU
ENDIF

RETURN


FUNCTION Ve_FecBK
*----------------
   dia = DTOC(_fecbk )
   IF _drive#"C:"
      Copia = _drive + LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) + '.ZIP'
   ELSE
      Copia = _RutBK + LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) + '.ZIP'
   ENDIF
RETURN FILE(Copia)