*:*********************************************************************
* PROGRAMA. Bkp_Dist.prg
* AUTOR   . lcd -- 12 jun 94
* OBJETIVO. Control de las copias de seguridad.
*------------------------------
parameters vtipo

* vTipo: 1  BackUp. Hace el empaquetado.
* vTipo: 2  Restaura. Desempaqueta la copia de seguridad.

set path to &vpath;..\CPRESU\

do case
case vtipo=1
   * Falta verificar espacio en disco
   =epk_fil('..\DATA\','*.DBF *.FPT')
case vtipo=2
   =poperror('   Al restaurar los archivos de datos de una copia de '+;
      'seguridad se destruyen los anteriores y no se podr n recuperar.'+chr(13)+;
      '   Se recomienda sacar un "BackUp" antes de hacer este procedimiento.')
   if yesno('¨Est  seguro que desea restaurar?')
      =dpk_fil('..\DATA\')
   endif
endcase
set path to &vpath
return
*: EOF: BKP_DISTR.PRG
