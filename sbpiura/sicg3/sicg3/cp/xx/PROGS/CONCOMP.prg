PUBLIC vidx1, temp, vperiodo,  ;
       vcalend
vidx1 = SYS(3) + '.idx'
temp = SYS(3) + '.dbf'
CLOSE DATABASES
USE IN 1 PARMAE ALIAS parma ORDER  ;
    parmae1
USE IN 2 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 3 itehc ALIAS itehc ORDER  ;
    itehc1
USE IN 4 hojcon ALIAS hoja ORDER  ;
    hojcon1
CREATE DBF (temp) (periodo C (2),  ;
       nummes C (2), codfte C (2),  ;
       codcad C (4), uniges C (2),  ;
       unieje C (3), codfun C (2),  ;
       codprg C (3), codspr C (4),  ;
       actpry C (6), codcom C (5),  ;
       codmet C (5), codpart C  ;
       (6), tipope C (1), valpart  ;
       N (18, 2))
USE IN 5
SELECT 5
USE IN 5 (temp) ALIAS temp
INDEX ON periodo + nummes +  ;
      codfte + uniges + unieje +  ;
      codcad + codpart TO  ;
      (vidx1)
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
PUBLIC vfecini, vfecfin
vperiodo = RIGHT(STR(YEAR(DATE()),  ;
           4), 2)
vtotal = 2
vtipo = 2
vuniges = '01'
vunieje = '001'
STORE SPACE(2) TO vcodfte,  ;
      vcalend, vcodfun
STORE SPACE(3) TO vcodprg
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' Consulta de Compromisos '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 1, 2 SAY '         Mes : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,30)
@ 3, 2 SAY '  U. Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vuniges),  ;
  val_para(vuniges,'UNIGES',' ', ;
  18,30), .T.)
@ 4, 2 SAY 'U. Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vunieje),  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',18,30), .T.)
@ 5, 2 SAY '     Funci¢n : ' GET  ;
  vcodfun PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfun),  ;
  val_para(vcodfun,'CODFUN',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 6, 2 SAY '    Programa : ' GET  ;
  vcodprg PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodprg),  ;
  val_para1(vcodprg,'CODPRG' +  ;
  vcodfun,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 7, 2 SAY ' SubPrograma : ' GET  ;
  vcodspr PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodspr),  ;
  val_para1(vcodspr,'CODSPR' +  ;
  vcodprg,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 8, 2 SAY 'Activ/Proyec : ' GET  ;
  vactpry PICTURE '!!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vactpry),  ;
  val_para(vactpry,'ACTPRY',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 9, 2 SAY '  Componente : ' GET  ;
  vcodcom PICTURE '!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcom),  ;
  val_para(vcodcom,'CODCOM',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 10, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1, 14 SAY  ;
  'Espere un momento ...' COLOR W+/ ;
  RB* 
SELECT itehc
SET RELATION TO ALLTRIM(vperiodo) + uniges;
+ unieje + codcad INTO maepre
SET FILTER TO itehc.nummes = ALLTRIM(vcalend);
.AND. itehc.codfte = ALLTRIM(vcodfte);
.AND. itehc.uniges = ALLTRIM(vuniges);
.AND. itehc.unieje = ALLTRIM(vunieje);
.AND. itehc.estado <> '99';
.AND. hoja.estado <> '99'
GOTO TOP
SCAN
     m.periodo = vperiodo
     m.nummes = itehc.nummes
     m.codfte = itehc.codfte
     m.uniges = itehc.uniges
     m.unieje = itehc.unieje
     m.codcad = itehc.codcad
     m.codfun = maepre.codfun
     m.codprg = maepre.codprg
     m.codspr = maepre.codspr
     m.actpry = maepre.actpry
     m.codcom = itehc.codcom
     m.codmet = itehc.codmet
     m.codpart = itehc.codpart
     m.tipope = itehc.tipope
     m.valpart = itehc.valpart
     SELECT temp
     SEEK m.periodo + m.nummes +  ;
          m.codfte + m.uniges +  ;
          m.unieje + m.codcad +  ;
          m.codpart
     IF FOUND()
          REPLACE temp.valpart  ;
                  WITH  ;
                  temp.valpart +  ;
                  IIF(m.tipope =  ;
                  '-', m.valpart * - ;
                  1, m.valpart)
          REPLACE temp.codfun  ;
                  WITH  ;
                  maepre.codfun
          REPLACE temp.codprg  ;
                  WITH  ;
                  maepre.codprg
          REPLACE temp.codspr  ;
                  WITH  ;
                  maepre.codspr
          REPLACE temp.actpry  ;
                  WITH  ;
                  maepre.actpry
     ELSE
          APPEND BLANK
          GATHER MEMVAR
          REPLACE temp.codfun  ;
                  WITH  ;
                  maepre.codfun
          REPLACE temp.codprg  ;
                  WITH  ;
                  maepre.codprg
          REPLACE temp.codspr  ;
                  WITH  ;
                  maepre.codspr
          REPLACE temp.actpry  ;
                  WITH  ;
                  maepre.actpry
     ENDIF
     SELECT itehc
ENDSCAN
SELECT temp
SET FILTER TO IIF(EMPTY(vcodfun),;
.T., ALLTRIM(vcodfun) = codfun);
.AND. IIF(EMPTY(vcodprg),;
.T., ALLTRIM(vcodprg) = codprg);
.AND. IIF(EMPTY(vcodspr),;
.T., ALLTRIM(vcodspr) = codspr);
.AND. IIF(EMPTY(vactpry),;
.T., ALLTRIM(vactpry) = actpry);
.AND. IIF(EMPTY(vcodcom),;
.T., ALLTRIM(vcodcom) = codcom);
.AND. IIF(EMPTY(vcodmet),;
.T., ALLTRIM(vcodmet) = codmet)
SET RELATION TO ALLTRIM(vperiodo) + uniges;
+ unieje + codcad INTO maepre ADDITIVE
DEACTIVATE WINDOW standby
GOTO TOP
IF EOF()
     DO standby WITH  ;
        'No existen registros para procesar'
ELSE
     DO reporte WITH 2, 'concomp',  ;
        ' Reporte de compromisos del mes ',  ;
        1, .F., .T.
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
ERASE (vidx1)
ERASE (temp)
RELEASE vidx1, temp, vperiodo,  ;
        vcalend
RETURN
*
FUNCTION sumagg
vreg = RECNO()
vkey = periodo + nummes + codfte +  ;
       uniges + unieje + codcad +  ;
       LEFT(codpart, 2)
vtot = 0
SCAN WHILE periodo + nummes +  ;
     codfte + uniges + unieje +  ;
     codcad + LEFT(codpart, 2) =  ;
     vkey
     vtot = vtot + valpart
ENDSCAN
GOTO vreg
RETURN vtot
*
