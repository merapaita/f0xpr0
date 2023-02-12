close data

define window xwait from 21,35 to 23,75 color scheme 5 
activate window xwait
@0,0 say " Procesando Informaci¢n....." COLOR W+/BR*

use cp  order tag cp1  in 1
use ite order tag ite1 in 2
use ret                in 3

sele ret 
go top
scan
	@0,30 say numcp color w+/br*
	vclave=ret.periodo+ret.nummes+ret.numcp+ret.codctc
	sele cp
	seek vclave
	if found()
		replace ret.nummeshc with cp.nummeshc
		replace ret.numhc    with cp.numhc
	endif
	sele ite
	seek vclave
	if found()
		replace ret.codcad   with ite.codcad
		replace ret.codcom   with ite.codcom
		replace ret.codmet   with ite.codmet
	endif
	sele ret
endscan	

deactivate window xwait
  






function partida
*---------------
parameters vpar
if len(vpar)<9
	return vpar
else
	newpar=left(vpar,1)+substr(vpar,3,1)+substr(vpar,5,2)+substr(vpar,8,2)
	return newpar
endif
	