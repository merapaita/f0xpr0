CLOSE DATABASES
USE IN 1 Donac ALIAS donac ORDER  ;
    Donac1
USE IN 2 Clientes ALIAS clien  ;
    ORDER Clientes1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
SELECT donac
SET RELATION TO codcli INTO clien
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
mfecha1 = DATE()
mfecha2 = DATE()
vestado = 2
DEFINE WINDOW wlista FROM 8, 15 TO 17, 70 FLOAT TITLE 'Reporte de Donaciones'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY '      Rango de Fechas: ' GET mfecha1
@ 01, 36 GET mfecha2
@ 03, 10 SAY '  Estado : ' GET vestado PICTURE '@^ Todos;Con Saldo'
@ 07, 10 GET okcancel DEFAULT 1 SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Reporte de Donacines'
IF LASTKEY() <> 27 .AND. okcancel <> 2
   SET FILTER TO BETWEEN(fecdon, mfecha1,mfecha2).AND. estado = IIF(vestado = 2, '20','')
*   SET FILTER TO BETWEEN(fecdon, mfecha1,mfecha2).AND. estado = IIF(vestado = 2, '20','')
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'Donac',  ;
             'Listado de Donaciones Realizadas'
     ELSE
          DO standby WITH  ;
             'No Existe Informaci?n para Procesar.'
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
FUNCTION retsb
PARAMETER msb
PRIVATE mret
IF SEEK('ENTSUB' + msb, 'Parma')
     mret = parma.descri
ELSE
     mret = 'Sin Descripci?n'
ENDIF
RETURN mret
*
