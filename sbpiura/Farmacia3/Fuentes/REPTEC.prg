CLOSE DATABASES
USE IN 1 Caja ALIAS caja ORDER  ;
    Caja2
USE IN 2 Parmae ALIAS parma ORDER  ;
    Parmae1
SELECT caja
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
mfecha1 = DATE()
mfecha2 = DATE()
vtocj = 2
m.tipcaj = SPACE(1)
DEFINE WINDOW wlista FROM 8, 15  ;
       TO 17, 70 FLOAT TITLE  ;
       'Reporte de Caja' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 05, 01 SAY  ;
  '      Rango de Fechas: ' GET  ;
  mfecha1
@ 05, 36 GET mfecha2
@ 07, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Caja'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SET FILTER TO BETWEEN(fecreg, mfecha1,;
mfecha2);
.AND. estado <> '99'
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'CajTec',  ;
             'Listado de Ingresos Caja x Tecnicos'
     ELSE
          DO standby WITH  ;
             'No Existe Informaci¢n para Procesar.'
     ENDIF
     SET FILTER TO
ELSE
     DO espera WITH 2
     DO standby WITH  ;
        'Proceso Cancelado.'
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
