ACTIVATE WINDOW standby
_drive = 'A:'
@ 01, 01 SAY  ;
  'En que drive est  sacando el backup:  '  ;
  GET _drive PICTURE  ;
  '@M A:,B:,C:,F:'
READ
DEACTIVATE WINDOW standby
IF LASTKEY() <> 27
     IF _drive = 'C:' .OR. _drive =  ;
        'F'
          DO backup1 WITH 1
     ELSE
          DO standby WITH  ;
             'Inserte el diskette en el drive '+ ;
             _drive
          IF LASTKEY() = 27 .OR.   ;
             .NOT.  ;
             isdisket(_drive)
               RETURN
          ENDIF
          dia = DTOC(DATE())
          HIDE POPUP ALL
          RESTORE SCREEN FROM  ;
                  pantalla
          copia = 'CP' + LEFT(dia,  ;
                  2) + SUBSTR(dia,  ;
                  4, 2) +  ;
                  RIGHT(dia, 2) +  ;
                  '.ZIP'
          @ 10, 10 CLEAR TO 14,  ;
            60
          @ 10, 10, 14, 60 BOX
          @ 12, 12 SAY  ;
            'Espere un momento, se est  sacando Copia'
          RUN FOXSWAP PKZIP &copia;
 ..\DATA\*.DBF ;
..\DATA\*.FPT;
..\data\*.tbk > NUL
          RUN FOXSWAP COPY  &copia;
  &_drive       > NUL
          RUN FOXSWAP COPY  &copia;
  ..\zip     > NUL
          RUN FOXSWAP ERASE &copia;
                > NUL
          RESTORE SCREEN FROM  ;
                  principal
          SHOW POPUP menu
     ENDIF
ENDIF
RETURN
*
