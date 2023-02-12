= poperror( ;
  'La copia de seguridad, tiene el formato: BKddmmaa.ZIP, y ' +  ;
  'se graba en el directorio ..\BACKUP, adem s de grabarlo en el diskette.' +  ;
  CHR(13) +  ;
  'Aseg£rese que NADIE use el sistema para empezar el proceso de Backup.' ;
  )
DEFINE WINDOW lis FROM 5, 15 TO  ;
       20, 65 FLOAT TITLE  ;
       '    Utilitario de Backup     '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
STORE 1 TO vlista
STORE 2 TO vlista2
IF LASTKEY() = 27
     RETURN
ENDIF
_dirdat = SET('PATH')
_dirdes = '..\BACKUP                     '
@ 01, 01 SAY  ;
  '  Drive del Back UP   : '
@ 01, 26 GET vlista FUNCTION  ;
  '^ Drive A:   ;Drive B:   ;Drive C:   '
@ 04, 01 SAY  ;
  'Dir. Origen de la Data: '
@ 04, 26 GET vlista2 FUNCTION  ;
  '^ ' + _dirdat
@ 08, 01 SAY  ;
  'Dir. Destino / Back UP: '
@ 09, 01 GET _dirdes
@ 12, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW lis
DO CASE
     CASE vlista = 1
          _drive = 'A:'
     CASE vlista = 2
          _drive = 'B:'
     CASE vlista = 3
          _drive = 'C:'
ENDCASE
DO CASE
     CASE vlista2 = 1
          _dirdat = SUBSTR(_dirdat,  ;
                    1, AT(';',  ;
                    _dirdat) - 1) +  ;
                    '\'
     CASE vlista2 = 2
          _dirdat = SUBSTR(_dirdat,  ;
                    AT(';',  ;
                    _dirdat) + 1) +  ;
                    '\'
ENDCASE
_dirdes = ALLTRIM(_dirdes)
IF LASTKEY() <> 27 .AND. okcancel =  ;
   1
     DO standby WITH  ;
        'Inserte el diskette en el drive '+ ;
        _drive
     IF LASTKEY() = 27
          RETURN
     ENDIF
     IF  .NOT. isdisket(_drive)
          RETURN
     ENDIF
     dia = DTOC(DATE())
     copia = 'BK' + LEFT(dia, 2) +  ;
             SUBSTR(dia, 4, 2) +  ;
             RIGHT(dia, 2) +  ;
             '.ZIP'
     _datfpt = _dirdat + '*.DBF ' +  ;
               _dirdat + '*.FPT'
     DO espera WITH 1,  ;
        'Espere un momento, se est  sacando Copia..!'
     RUN FOXSWAP PKZIP &copia  &_datfpt;
>NUL
     RUN FOXSWAP COPY  &copia  &_drive;
>NUL
     RUN FOXSWAP COPY  &copia  &_dirdes;
>NUL
     RUN FOXSWAP ERASE &copia  >NUL
     DO espera WITH 2
     SHOW POPUP menu
ENDIF
RETURN
*
