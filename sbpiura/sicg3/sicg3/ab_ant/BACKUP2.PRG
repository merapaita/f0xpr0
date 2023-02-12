** зддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд©
** Ё Programa	: Backup2.prg									   Ё
** Ё 			  Utilitario de Backup   						   Ё
** Ё Fecha		: 18/08/98                                         Ё
** цддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддд╢
** Ё Autor	   : Tec. Prog. Norberto Recarte Romero				   Ё
** юддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддддды

DEFINE WINDOW LibDir FROM 6,15 TO 14,65 FLOAT DOUBLE TITLE ' Backup de los Archivos' COLOR SCHEME 1
ACTIVATE WINDOW libdir

vfile='BK'+subs(dtoc(date()),7,2)+subs(dtoc(date()),4,2)+subs(dtoc(date()),1,2)+'.ZIP'
vdest=PADR('C:\BKABAS98\',60,' ')

@ 1,1 SAY 'Nombre del Backup: ' GET vfile DISABLED
@ 3,1 SAY 'Destino de Backup: ' GET vDest PICTURE '@!KS27' 
@ 5,10 GET OKCANCEL FUNCTION '*TH \!\<OK;\?\<Cancela' DEFAULT 1 SIZE 1,11,8
READ
deactivate window libdir
IF LASTKEY()=27 OR OKCANCEL=2
   WAIT WINDOW 'Proceso cancelado' NOWAIT
   RETURN
ENDIF

vdest=allt(vdest)
bkdsk=.f.
if left(vdest,2)$'A:B:'
   bkdsk=.t. 
endif

if !yesno('╗Proceder backup?')
   WAIT WINDOW 'Proceso cancelado' NOWAIT
   RETURN
ENDIF

! CLS
if bkdsk
   cmd = 'PKZIP '+vdest+vfile+' *.prg -&'
else
   cmd = 'PKZIP '+vdest+vfile+' *.prg -S'
endif
! &cmd
RESTORE SCREEN FROM PRINCIPAL
SHOW POPUP MENU
*SHOW POPUP pop_04,menu
HIDE WINDOW ALL
if file(vdest+vfile)
   =dialbox(1,'Se gener╒ correctamente el archivo de backup '+vdest+vfile,'Backup de los Archivos')
else
   =dialbox(1,'No se pudo generar el archivo de backup!.','Backup de los archivos')
endif

RETURN
