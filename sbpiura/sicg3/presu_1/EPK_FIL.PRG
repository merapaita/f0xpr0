*****************************************************************************
* PROGRAMA	 : EPK_Fil.PRG
* AUTOR		 : LCD -- 15 Jun 94
* MODIFICADO :
* OBJETIVO	 : EmPaKetar los archivos con alguna extension.
*****************************************************************************

parameter vdir,vext, vdirdest
* vDir  Donde estan los archivos a empaquetar.
* vExt  La extension de los archivos.
* vDirDest	Es el directorio donde estara el archivo ZIP.
* Debe terminar en '\'

* La funcion retornara el nombre del archivo ZIP o empaquetado

private vnamezip, vdefazip, volddir, vret

if parameter()=2
   vdirdest = '..\ZIP\'
endif
vret = .t.
volddir  = sys(5)+curdir()
vdir     = fullpath(vdir)
set default to (vdirdest)

vnamezip = get_file('ZIP','Grabar el "BackUp" como:',.t.,.f.,.f.,.f.)
if vnamezip='NEW FILE'
   if pidfile(vnamezip, vdirdest ,'ZIP')
      vnamezip = vdirdest+vnamezip
   else
      vnamezip=''
   endif
endif
if empty(vnamezip)
   wait window 'No seleccion¢ nombre del archivo.' nowait
   vret = .f.
else
   set default to (vdir)
   =MsgPro(.T.,23,' ®   Espere un Momento Guardando Archivos  ¯ ')
   run foxswap pkzip -u &vnamezip  &vext >nul
   =MsgPro(.F.)
endif
set default to (volddir)

return vnamezip
*: EOF: EPK_FIL.PRG