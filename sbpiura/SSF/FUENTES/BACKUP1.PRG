*:*********************************************************************
* PROGRAMA. Bkp_Dist.prg
* AUTOR   . lcd -- 12 jun 94
* OBJETIVO. Control de las copias de seguridad.
*------------------------------
parameters vtipo

* vTipo: 1  BackUp. Hace el empaquetado.
* vTipo: 2  Restaura. Desempaqueta la copia de seguridad.

*set path to &Ruta
_drive="..\Backup"
do case
case vtipo=1
   * Falta verificar espacio en disco
      HIDE POPUP ALL
      RESTORE SCREEN FROM Pantalla
      *Copia = 'DATA.ZIP'
      dia = DTOC(DATE())
      Copia = 'CP'+LEFT( dia,2) + SUBSTR( dia, 4,2 ) + RIGHT (dia,2 ) +'.ZIP'
      @ 10,10 CLEAR TO 14,60
      @ 10,10,14,60 BOX
      @ 12,12 SAY "Espere un momento, se est  sacando Copia"
      RUN FOXSWAP PKZIP &copia  ..\DATA\*.DBF  ..\DATA\*.FPT ..\data\*.tbk > NUL
      RUN FOXSWAP COPY  &copia   &_drive       > NUL
*      RUN FOXSWAP COPY  &copia   ..\zip     > NUL
      RUN FOXSWAP ERASE &copia                 > NUL
      RESTORE SCREEN FROM Principal
      SHOW POPUP MENU

*   =epk_fil('..\DATA\','*.DBF *.FPT')
case vtipo=2
   =poperror('   Al restaurar los archivos de datos de una copia de '+;
      'seguridad se destruyen los anteriores y no se podr n recuperar.'+chr(13)+;
      '   Se recomienda sacar un "BackUp" antes de hacer este procedimiento.')
   if yesno('¨Est  seguro que desea restaurar?')
      =dpk_fil('..\DATA\')
   endif
endcase
*set path to &Ruta
return
*: EOF: BKP_DISTR.PRG
