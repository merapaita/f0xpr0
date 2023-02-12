CLOSE DATABASES
USE IN 1 KarCre ALIAS kardex  ;
    ORDER KarCre1
USE IN 2 Creditos ALIAS creditos  ;
    ORDER Creditos1
USE IN 3 Clientes ALIAS clien  ;
    ORDER Clientes1
SELECT kardex
vidx = SYS(3) + '.Idx'
SET RELATION TO codcre INTO creditos ADDITIVE
SELECT creditos
SET RELATION TO codcli INTO clien ADDITIVE
SELECT kardex
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vtocr = 1
m.codcre = SPACE(7)
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Reporte de Kardex' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '    Todas los Creditos : ' GET  ;
  vtocr SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 03, 01 SAY '  Credito: ' GET  ;
  m.codcre VALID valcre() WHEN  ;
  vtocr = 2
@ 15, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Kardex'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SET FILTER TO codcre = IIF(vtocr;
= 1, '', m.codcre)
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'Kardex',  ;
             'Reporte de Kardex',  ;
             2, .F., .T.
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
