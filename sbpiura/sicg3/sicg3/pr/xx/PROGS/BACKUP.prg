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
IF LASTKEY() = 27
     RETURN
ENDIF
IF SET('PATH') <> rutapr
     mruta = SUBSTR(ruta, AT(';',  ;
             ruta) + 1)
     _dirdat = PADR(mruta + '\',  ;
               35, ' ')
     _dirdes = PADR(rutabk, 35,  ;
               ' ')
ELSE
     _dirdat = PADR(rutapr + '\',  ;
               35, ' ')
     _dirdes = PADR(rutabk, 35,  ;
               ' ')
ENDIF
copia = 'BK' + PADL(DAY(DATE()),  ;
        2, '0') +  ;
        PADL(MONTH(DATE()), 2,  ;
        '0') +  ;
        SUBSTR(ALLTRIM(STR(YEAR(DATE()))),  ;
        3, 2) + '.ZIP'
@ 01, 03 SAY  ;
  'Nombre del Archivo  : ' +  ;
  copia
@ 03, 03 SAY  ;
  'Drive del Back UP   : '
@ 03, 26 GET vlista FUNCTION  ;
  '^ Drive A:   ;Drive B:   ;Drive C:   '
@ 06, 03 SAY  ;
  'Dir. Origen de la Data: '
@ 07, 03 GET _dirdat
@ 09, 03 SAY  ;
  'Dir. Destino / Back UP: '
@ 010, 03 GET _dirdes
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
_dirdat = ALLTRIM(_dirdat)
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
     _datfpt = _dirdat + '*.DBF ' +  ;
               _dirdat + '*.FPT'
     DO espera WITH 1,  ;
        'Espere un momento, se est  sacando Copia..!'
     SUSPEND
     RUN FOXSWAP;
..\PKZIP &copia &_datfpt >NUL
     RUN FOXSWAP COPY  &copia &_drive;
 >NUL
     RUN FOXSWAP COPY  &copia &_dirdes;
>NUL
     RUN FOXSWAP ERASE &copia ;
        >NUL
     DO espera WITH 2
     SHOW POPUP menu
ENDIF
RETURN
*
