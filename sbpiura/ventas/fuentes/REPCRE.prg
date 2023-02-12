CLOSE DATABASES
USE IN 1 Creditos ALIAS creditos  ;
    ORDER Creditos1
USE IN 2 Clientes ALIAS clien  ;
    ORDER Clientes1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
SELECT creditos
SET RELATION TO codcli INTO clien
vidx = SYS(3) + '.Idx'
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
mfecha1 = DATE()
mfecha2 = DATE()
vestado = 2
vtocr = 2
m.tipvta = SPACE(2)
DEFINE WINDOW wlista FROM 5, 15  ;
       TO 18, 70 FLOAT TITLE  ;
       'Reporte de Creditos'  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '    Todas las Creditos : ' GET  ;
  vtocr SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 03, 01 SAY '  Tipo: ' GET  ;
  m.tipvta VALID  ;
  val_para(m.tipvta,'TIPVTA',' ', ;
  32,20) WHEN vtocr = 2
@ 05, 01 SAY  ;
  '      Rango de Fechas: ' GET  ;
  mfecha1
@ 05, 36 GET mfecha2
@ 07, 01 SAY '  Estado : ' GET  ;
  vestado PICTURE  ;
  '@^ Todos;Con Saldo'
@ 10, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Reporte de Creditos'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SET FILTER TO tipvta = IIF(vtocr;
= 1, '', m.tipvta);
.AND. BETWEEN(feccre, mfecha1, mfecha2);
.AND. estado = IIF(vestado = 2, '20',;
'')
     INDEX ON DTOC(feccre, 1) TO  ;
           (vidx)
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'Creditos',  ;
             'Listado de Creditos'
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
ERASE (vidx)
RETURN
*
FUNCTION rettv
PARAMETER mtv
PRIVATE mret
IF SEEK('TIPVTA' + mtv, 'Parma')
     mret = parma.descri
ELSE
     mret = 'Sin Descripci¢n'
ENDIF
RETURN mret
*
