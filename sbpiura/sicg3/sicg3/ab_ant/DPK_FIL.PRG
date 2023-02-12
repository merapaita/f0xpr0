*:*********************************************************************
* PROGRAMA. DPK_Fil.PRG
* AUTOR   . lcd -- 15 Jun 94
* OBJETIVO. DesemPaKetar los archivos con alguna extension.
*
*------------------------------
parameter vtodir, vfromdir
private vnamezip, volddir, vret, vdd

if parameters()=0
   wait window 'Tiene que definirse par metros.' nowait
   return .f.
endif
if parameters()=1
   vfromdir = '..\ZIP\'
endif

vret = .t.
volddir  = sys(5)+curdir()
vtodir = fullpath(vtodir)
set default to (vfromdir)
if sys(5)='A:'
   vnamezip = get_file('ZIP','Restaurar archivos desde:',.f.,.f.,.t.,.t.,2)
else
   vnamezip = get_file('ZIP','Restaurar archivos desde:',.f.,.f.,.f.,.f.)
endif
if empty(vnamezip)
   wait window 'No seleccion¢ nombre del archivo.' nowait
   vret = .f.
else
   set default to (vtodir)
   =adir(vdd,vnamezip)
   if type('vDD')#'C'
      wait window 'No se hall¢ fecha del archivo &vNameZip ' nowait
      vret = .f.
   else
      if yesno('La copia &vNameZip se hizo el d¡a '+dtoc(vdd[1,3])+;
            ', a las &vDD[1,4] hrs. ¨Restaurar?')
         *=MsgPro(.T.,23,' <<  Desempacando  >> ')
         !pkunzip -o &vnamezip >nul
         *=MsgPro(.F.)
      else
         vret = .f.
      endif
   endif
endif
set default to (volddir)

return vret
*: EOF: DPK_FIL.PRG
