*:*********************************************************************
*:
*: Procedure file: C:\COCA\FUENTES\PIDFILE.PRG
*:
*:         System:
*:         Author:
*:      Copyright (c) 1994,
*:  Last modified: 06/21/93     11:03
*:
*:  Procs & Fncts: PIDFILE()
*:               : VAL_FILE()
*:
*:          Calls: IIF()              (function  in ?)
*:               : PARAMETERS()       (function  in ?)
*:               : WCOLS()            (function  in ?)
*:               : LEN()              (function  in ?)
*:               : VAL_FILE()         (function  in PIDFILE.PRG)
*:               : LASTKEY()          (function  in ?)
*:
*:      Documented 05/17/94 at 17:15               FoxDoc  version 2.10b
*:*********************************************************************
parameter vfilename, vpath, vext
* Para solicitar Nomb_Archivo .(Ahora depende de vExt)
* \\ LCD
* @ 1994
*-------------------------
_msg = 'Nombre Inv lido.'
vpath = iif(parameters()=1,'',vpath)

define   window _pidfil from 19,27 to 23,75 color scheme 5  in screen
activate window _pidfil
vtit = 'Ingrese el nombre del archivo a grabar :'
@ 0,(wcols()-len(vtit))/2 say vtit
@ 2,(wcols()-6 )/2 get vfilename picture  'NNNNNNNNNNNN' ;
   valid val_file() error _msg
read cycle
release window _pidfil
if lastkey()=27
   return .f.
endif

return

*!*********************************************************************
*!
*!       Function: VAL_FILE()
*!
*!      Called by: PIDFILE.PRG
*!
*!          Calls: LASTKEY()          (function  in ?)
*!               : SPACE()            (function  in ?)
*!               : STRTRAN()          (function  in ?)
*!               : AT()               (function  in ?)
*!               : LEFT()             (function  in ?)
*!               : IIF()              (function  in ?)
*!               : ALLTRIM()          (function  in ?)
*!               : LEN()              (function  in ?)
*!               : PADR()             (function  in ?)
*!               : EMPTY()            (function  in ?)
*!               : FILE()             (function  in ?)
*!
*!*********************************************************************
function val_file
*----------------
private _pasa
_pasa = .t.
_msg = 'Nombre Inv lido.'
if lastkey()=27
   vfilename=space(12)
   return .t.
endif
vfilename = strtran(vfilename,' ','')
vpos = at('.',vfilename)
if vpos#0
   vname = left( vfilename, iif(vpos<9,vpos-1, 8 ) )
else
   vfilename = alltrim(vfilename)
   vname = iif( len(vfilename)>8 , left(vfilename,8), vfilename)
endif
vfilename = padr( vname+'.'+vext,12,' ')
do case
case empty(vname)
   _pasa = .f.
case file(vpath+vfilename)
   _pasa= .f.
   _msg = 'Archivo ya existe. Ingrese otro nombre.'
endcase
if _pasa
   wait window 'Ud. ha elegido el archivo &vFileName.' nowait
   clear read
endif
return _pasa
*: EOF: PIDFILE.PRG
