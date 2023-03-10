CLOSE DATABASES
USE IN 1 KarCre ALIAS kardex      ORDER KarCre1
USE IN 2 Creditos ALIAS creditos  ORDER Creditos1
USE IN 3 Clientes ALIAS clien     ORDER Clientes1
USE IN 4 Parmae ALIAS parma ORDER Parmae1
vdbf = SYS(3) + '.Dbf'
CREATE DBF (vdbf) (codcre C (7), feccre D, codvta C (7),  ;
       tipvta C (10), nomcli C  ;
       (30), fultpag D, mtocre N(6,2), totaju N(6,2), ;
       totamo N (8, 2),  ;
       totint N (8, 2), intact N  ;
       (8, 2), saldo N (8, 2),  ;
       totdeu N (8, 2))
USE
USE IN 5 (vdbf) ALIAS estcre
vidx = SYS(3) + '.Idx'
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
vtocr = 1
m.codcre = SPACE(7)
m.tipvta = SPACE(2)
m.codcli = SPACE(6)
mfecha1 = CTOD('01/01/' + STR(YEAR(m.fecsis), 4))
mfecha2 = DATE()
mfeccor = DATE()
DEFINE WINDOW wlista FROM 4, 15 TO 19, 70 FLOAT TITLE 'Reporte de Kardex' DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  '    Todas los Creditos : ' GET  ;
  vtocr SIZE 1, 10, 2 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 03, 01 SAY '  Tipo: ' GET  ;
  m.tipvta VALID  ;
  val_para(m.tipvta,'TIPVTA',' ', ;
  32,20) WHEN vtocr = 2
@ 05, 01 SAY '   Cliente : ' GET  ;
  m.codcli VALID IIF( .NOT.  ;
  EMPTY(m.codcli),  ;
  val_cli(m.codcli), .T.)
@ 07, 01 SAY  ;
  '      Rango de Fechas: ' GET  ;
  mfecha1
@ 07, 36 GET mfecha2
@ 09, 01 SAY '   Fecha Corte : '  ;
  GET mfeccor PICTURE '@D' VALID  ;
  mfeccor >= mfecha2 ERROR  ;
  'Fecha Incorrecta'
@ 11, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1, 'Procesando Resumen de Kardex'
IF LASTKEY() <> 27 .AND. okcancel <>  2
     SELECT creditos
     SET RELATION TO codcli INTO clien ADDITIVE
     SET RELATION TO codcre INTO kardex ADDITIVE
     SET SKIP TO kardex
     SET FILTER TO tipvta = IIF(vtocr = 1, '', m.tipvta) .AND. BETWEEN(kardex.fecha, mfecha1, mfecha2) .AND. codcli = IIF(.NOT. EMPTY(m.codcli), m.codcli, '')
     SELECT creditos
     GOTO TOP
     SCAN
          vcodcre = codcre
          vfeccre = feccre
          vcodvta = codvta
          vtipvta = tipvta
          vnomcli = clien.nomcli
          vmtocre = mtocre
          vTotAju = 0
          vtotamo = 0
          vtotint = 0
          vsaldo = 0
          SCAN WHILE codcre = vcodcre
               vfultpag = kardex.fecha
               vtotamo = vtotamo + kardex.mtoamo
               vtotaju = vtotaju + kardex.mtoaju
               vtotint = vtotint + kardex.mtoint
          ENDSCAN
          vsaldo = vmtocre + vtotint - vtotamo + vTotAju
          vintact = 0
          vtotdeu = 0
          IF DTOC(vfeccre, 1) > '20101231'
               vintact = ROUND(vsaldo * ((mfeccor - vfultpag) / 30) * (17.0/1000), 2)
          ENDIF
          vtotdeu = vsaldo + vintact
          SELECT estcre
          APPEND BLANK
          REPLACE codcre WITH vcodcre, feccre WITH vfeccre,  ;
                  codvta WITH vcodvta, tipvta WITH vtipvta,  ;
                  nomcli WITH vnomcli, mtocre WITH vmtocre,  ;
                  totamo WITH vtotamo, ;
                  totint WITH vtotint, ;
                  totaju WITH vtotaju, ;
                  saldo  WITH vsaldo, fultpag WITH vfultpag, ;
                  intact WITH vintact, totdeu WITH vtotdeu
          SELECT creditos
          SKIP -1
     ENDSCAN
     SELECT estcre
     SET FILTER TO ;
.NOT. EMPTY(saldo)
     INDEX ON DTOC(feccre, 1) +  ;
           nomcli TO (vidx)
     GOTO TOP
     DO espera WITH 2
     IF  .NOT. EOF()
          DO reporte WITH 2, 'DeuAct', 'Deuda Actualizada', 2, .F., .T.
     ELSE
          DO standby WITH 'No Existe Informaci?n para Procesar.'
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
