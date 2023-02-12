DEFINE WINDOW libdir FROM 6, 15  ;
       TO 14, 65 FLOAT TITLE  ;
       ' Restaurar Backup '  ;
       DOUBLE COLOR SCHEME 1
ACTIVATE WINDOW libdir
vfile = 'BK' +  ;
        SUBSTR(DTOC(DATE()), 7,  ;
        2) + SUBSTR(DTOC(DATE()),  ;
        4, 2) +  ;
        SUBSTR(DTOC(DATE()), 1,  ;
        2) + '.ZIP'
vdest = PADR('\BKMON01\', 60,  ;
        ' ')
@ 1, 1 SAY 'Nombre del Backup: '  ;
  GET vfile
@ 3, 1 SAY ' Origen de Backup: '  ;
  GET vdest PICTURE '@!KS27'
@ 5, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ
DEACTIVATE WINDOW libdir
vdest = ALLTRIM(vdest)
vfile = ALLTRIM(vfile)
IF  .NOT. '.' $ vfile
     vfile = vfile + '.ZIP'
ENDIF
IF LASTKEY() = 27 .OR. okcancel =  ;
   2
     WAIT WINDOW NOWAIT  ;
          'Proceso cancelado'
     RETURN
ENDIF
IF  .NOT. FILE(vdest + vfile)
     = dialbox(1, ;
       'No se encontr¢ el archivo ' +  ;
       vdest + vfile, ;
       'Restaurar Backup')
     RETURN
ENDIF
vdest = ALLTRIM(vdest)
bkdsk = .F.
IF LEFT(vdest, 2) $ 'A:B:'
     bkdsk = .T.
ENDIF
IF dialbox(2, ;
   'Est  realmente seguro que desea restaurar el backup', ;
   'Confirmar Proceso') = .F.
     RETURN
ENDIF
RUN CLS
IF bkdsk
     cmd = 'PKUNZIP ' + vdest +  ;
           vfile + ' *.prg -o'
ELSE
     cmd = 'PKUNZIP ' + vdest +  ;
           vfile + ' *.prg -o'
ENDIF
! &cmd
vworker = worker
worker = 'ON'
RUN DEL *.CDX
RESTORE SCREEN FROM principal
SHOW POPUP menu
HIDE WINDOW ALL
DO indexaba WITH .F.
worker = vworker
= dialbox(1, ;
  'El proceso de restauraci¢n de backup ha conclu¡do.', ;
  'Restaurar Backup')
RETURN
*
