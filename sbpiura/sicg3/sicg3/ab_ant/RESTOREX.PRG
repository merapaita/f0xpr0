* ............................................
*  
*  Program		 RESTOREX.PRG
*  Descrip		 Progrma que epura los archivos del sistema
*
*  Upgrade		 Diciembre 1999
* ............................................
*
DEFINE WINDOW LibDir FROM 6,15 TO 14,65 FLOAT DOUBLE TITLE ' Restaurar Backup ' COLOR SCHEME 1
ACTIVATE WINDOW libdir
vfile='BK'+subs(dtoc(date()),7,2)+subs(dtoc(date()),4,2)+subs(dtoc(date()),1,2)+'.ZIP'
vdest=PADR('\BKMON01\',60,' ')
@ 1,1 SAY 'Nombre del Backup: ' GET vfile 
@ 3,1 SAY ' Origen de Backup: ' GET vDest PICTURE '@!KS27' 
@ 5,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ
deactivate window libdir
vdest=allt(vdest)
vfile=allt(vfile)
if !'.'$vfile
   vfile=vfile+'.ZIP'
endif
IF LASTKEY()=27 OR OKCANCEL=2
   WAIT WINDOW 'Proceso cancelado' NOWAIT
   RETURN
ENDIF

IF !FILE(vdest+vfile)
   =dialbox(1,'No se encontr¢ el archivo '+vdest+vfile,'Restaurar Backup') 
   RETURN
ENDIF
   
vdest=allt(vdest)
bkdsk=.f.
if left(vdest,2)$'A:B:'
   bkdsk=.t. 
endif

if dialbox(2,'Est  realmente seguro que desea restaurar el backup','Confirmar Proceso')=.F.
   RETURN
ENDIF

! CLS
if bkdsk
   cmd = 'PKUNZIP '+vdest+vfile+' *.prg -o'
else
   cmd = 'PKUNZIP '+vdest+vfile+' *.prg -o'
endif
! &cmd
vworker = worker
WORKER='ON'
! DEL *.CDX
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP menu
HIDE WINDOW ALL
DO INDEXABA WITH .F.
WORKER=vworker
=dialbox(1,'El proceso de restauraci¢n de backup ha conclu¡do.','Restaurar Backup') 
RETURN
