DEFINE WINDOW libdir FROM 6, 15  ;
       TO 14, 65 FLOAT TITLE  ;
       ' Backup de los Archivos'  ;
       DOUBLE COLOR SCHEME 1
ACTIVATE WINDOW libdir
vfile = 'BK' +  ;
        SUBSTR(DTOC(DATE()), 7,  ;
        2) + SUBSTR(DTOC(DATE()),  ;
        4, 2) +  ;
        SUBSTR(DTOC(DATE()), 1,  ;
        2) + '.ZIP'
vdest = PADR('C:\BKMON98\', 60,  ;
        ' ')
@ 1, 1 SAY 'Nombre del Backup: '  ;
  GET vfile DISABLE
@ 3, 1 SAY 'Destino de Backup: '  ;
  GET vdest PICTURE '@!KS27'
@ 5, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ
DEACTIVATE WINDOW libdir
IF LASTKEY() = 27 .OR. okcancel =  ;
   2
     WAIT WINDOW NOWAIT  ;
          'Proceso cancelado'
     RETURN
ENDIF
vdest = ALLTRIM(vdest)
bkdsk = .F.
IF LEFT(vdest, 2) $ 'A:B:'
     bkdsk = .T.
ENDIF
IF  .NOT. yesno( ;
    '¨Proceder backup?')
     WAIT WINDOW NOWAIT  ;
          'Proceso cancelado'
     RETURN
ENDIF
RUN CLS
IF bkdsk
     cmd = 'PKZIP ' + vdest + vfile +;
' *.prg -&'
ELSE
     cmd = 'PKZIP ' + vdest +  ;
           vfile + ' *.prg'
ENDIF
! &cmd
RESTORE SCREEN FROM principal
SHOW POPUP menu
HIDE WINDOW ALL
IF FILE(vdest + vfile)
     = dialbox(1, ;
       'Se gener¢ correctamente el archivo de backup ' +  ;
       vdest + vfile, ;
       'Backup de los Archivos')
ELSE
     = dialbox(1, ;
       'No se pudo generar el archivo de backup!.', ;
       'Backup de los archivos')
ENDIF
RETURN
*
