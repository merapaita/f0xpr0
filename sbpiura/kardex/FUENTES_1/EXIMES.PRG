CLOSE DATABASES
USE IN 1 KardexV ALIAS kardex  ;
    ORDER KardexV1
USE IN 2 ArtMae ALIAS artmae  ;
    ORDER aRTmAE1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
SET RELATION TO 'B' + LEFT(codart, 6);
INTO artmae
DO inicia
DO salida
*
PROCEDURE inicia
m.periodo = RIGHT(STR(YEAR(m.fecsis),  ;
            4), 2)
m.mes = PADL(MONTH(m.fecsis), 2,  ;
        '0')
m.tipkar = SPACE(1)
m.codgen = SPACE(2)
m.codcla = SPACE(3)
m.coddet = SPACE(4)
mfecha1 = CTOD('01/01/' +  ;
          STR(YEAR(m.fecsis),  ;
          4))
mfecha2 = DATE()
DEFINE WINDOW wlista FROM 2, 15  ;
       TO 21, 70 FLOAT TITLE  ;
       'Reporte de Kardex' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 10 SAY '  A?o : ' +  ;
  STR(YEAR(m.fecsis), 4)
@ 03, 01 SAY  ;
  '  Tipo de Existencias: ' GET  ;
  m.tipkar VALID  ;
  val_para(m.tipkar,'TIPKAR',' ', ;
  22,50)
@ 05, 01 SAY  ;
  '                 Mes : ' GET  ;
  m.mes VALID val_para(m.mes, ;
  'FECMES',' ',32,50)
@ 07, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
ctipkar = val_para(m.tipkar, ;
          'TIPKAR','D',22,50)
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Kardex'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SET FILTER TO tipkar = m.tipkar;
.AND. PADL(MONTH(fecha), 2, '0') = m.mes
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,'ExiMes','Reporte de Existencias Mensuales',2,.F.,.T.
     ELSE
          DO standby WITH 'No Existe Informaci?n para Procesar.'
     ENDIF
     IF yesno('Desea Imprimir Consolidado')
          GOTO TOP
          DO reporte WITH 2, 'ExiMes3', 'Resumen de Existencias Mensuales', 2, .F., .T.
     ENDIF
ELSE
     DO espera WITH 2
     DO standby WITH 'Proceso Cancelado.'
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
