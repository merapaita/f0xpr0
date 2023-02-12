CLOSE DATABASES
USE IN 1 KarCre ALIAS kardex  ;
    ORDER KarCre1
USE IN 2 Creditos ALIAS creditos  ;
    ORDER Creditos1
USE IN 3 Clientes ALIAS clien  ;
    ORDER Clientes1
USE IN 4 Parmae ALIAS parma ORDER  ;
    Parmae1
vdbf = SYS(3) + '.Dbf'
CREATE DBF (vdbf) (codcre C (7),  ;
       feccre D, codvta C (7),  ;
       tipvta C (10), nomcli C  ;
       (30), fultpag D, mtocre N  ;
       (6, 2), totamo N (8, 2),  ;
       totint N (8, 2), saldo N  ;
       (8, 2))
USE
USE IN 5 (vdbf) ALIAS estcre
SELECT estcre
vidx = SYS(3) + '.Idx'
INDEX ON DTOC(feccre, 1) TO  ;
      (vidx)
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vtocr = 1
m.codcre = SPACE(7)
m.tipvta = SPACE(2)
mfecha1 = CTOD('01/01/' +  ;
          STR(YEAR(m.fecsis),  ;
          4))
mfecha2 = DATE()
mfeccor = DATE()
DEFINE WINDOW wlista FROM 5, 15  ;
       TO 18, 70 FLOAT TITLE  ;
       'Reporte de Kardex' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '    Todas los Creditos : ' GET  ;
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
@ 07, 01 SAY '   Fecha Corte : '  ;
  GET mfeccor PICTURE '@D'
@ 09, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Procesando Resumen de Kardex'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SELECT creditos
     SET RELATION TO codcli INTO clien;
ADDITIVE
     SET RELATION TO codcre INTO kardex;
ADDITIVE
     SET SKIP TO kardex
     SET FILTER TO tipvta = IIF(vtocr;
= 1, '', m.tipvta);
.AND. BETWEEN(kardex.fecha, mfecha1, mfecha2)
     SELECT creditos
     GOTO TOP
     SCAN
          vcodcre = codcre
          vfeccre = feccre
          vcodvta = codvta
          vtipvta = tipvta
          vnomcli = clien.nomcli
          vmtocre = mtocre
          vtotamo = 0
          vtotint = 0
          vsaldo = 0
          SCAN WHILE codcre =  ;
               vcodcre
               vfultpag = kardex.fecha
               vtotamo = vtotamo +  ;
                         kardex.mtoamo
               vtotint = vtotint +  ;
                         kardex.mtoint
          ENDSCAN
          vsaldo = vmtocre +  ;
                   vtotint -  ;
                   vtotamo
          SELECT estcre
          APPEND BLANK
          REPLACE codcre WITH  ;
                  vcodcre, feccre  ;
                  WITH vfeccre,  ;
                  codvta WITH  ;
                  vcodvta, tipvta  ;
                  WITH vtipvta,  ;
                  nomcli WITH  ;
                  vnomcli, mtocre  ;
                  WITH vmtocre,  ;
                  totamo WITH  ;
                  vtotamo, totint  ;
                  WITH vtotint,  ;
                  saldo WITH  ;
                  vsaldo, fultpag  ;
                  WITH vfultpag
          SELECT creditos
          SKIP -1
     ENDSCAN
     SELECT estcre
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2,  ;
             'EstCre',  ;
             'Estado de Creditos',  ;
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
ERASE (vdbf)
ERASE (vidx)
RETURN
*
