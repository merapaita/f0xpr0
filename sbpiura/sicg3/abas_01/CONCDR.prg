USE IN 1 CdrNec ALIAS cuadro  ;
    ORDER CdrNec1
USE IN 2 IteCn ALIAS itecn ORDER  ;
    IteCn2
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 4 Artmae ALIAS produ ORDER  ;
    Artmae1
USE IN 5 Iteart ALIAS iteart  ;
    ORDER Iteart1
IF EOF()
     DO standby WITH  ;
        'No Existe registros a Procesar'
     CLOSE DATABASES
     RETURN
ENDIF
SELECT itecn
vtemp = RECNO()
as = ORDER()
vperiodo = RIGHT(STR(YEAR(DATE()),  ;
           4), 2)
DEFINE WINDOW lis FROM 10, 12 TO  ;
       14, 68 FLOAT TITLE  ;
       ' °° Consolidaci¢n °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis
@ 1, 12 SAY '        Periodo : '
@ 1, 31 GET vperiodo PICTURE '!!'  ;
  VALID  .NOT. EMPTY(vperiodo)
READ
DEACTIVATE WINDOW lis
IF EMPTY(vperiodo) .OR. LASTKEY() =  ;
   27
     CLOSE DATABASES
     RETURN
ELSE
     SET FILTER TO periodo = vperiodo
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No se tiene registros a Procesar'
     ELSE
          vtitulo = 'Per¡odo 19' +  ;
                    ALLTRIM(vperiodo)
          DO reporte WITH 2,  ;
             'ConCdr1',  ;
             ' Consolidaci¢n Cuadro de Necesidades '
     ENDIF
ENDIF
SELECT itecn
SET FILTER TO
CLOSE DATABASES
RETURN
*
