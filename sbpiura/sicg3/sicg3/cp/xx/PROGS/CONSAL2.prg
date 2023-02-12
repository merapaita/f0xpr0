CLOSE DATABASES
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 calen ALIAS calen ORDER  ;
    calen2
USE IN 3 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 4 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 5 itepar ALIAS itepar  ;
    ORDER itepar3
USE IN 6 itehc ALIAS itehc ORDER  ;
    itehc1
USE IN 9 PteAnu ALIAS anupa ORDER  ;
    PteAnu9
vmens01 = 'Registro de Calendario'
vmens02 = ' Calendario : REVISION '
vmens04 = 'Dicho Calendario no fue encontrado'
vmens05 = 'No existe Calendario anterior'
vmens06 = 'No existe Calendario siguiente'
vmens07 = '¨ Desea Anular ‚ste Calendario ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Calendario ha sido anulado'
vmens10 = 'El Calendario ya est  Atendido'
vmens11 = 'El Calendario ha sido devuelto'
PUBLIC tot, vacum, tot1, totcom
tot = 0
tot1 = 0
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
      vcalend, vcodfun, vpargen
STORE SPACE(3) TO vcodprg
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       22, 70 FLOAT TITLE  ;
       ' °° Saldo De Calendario Anal¡tico °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
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
@ 8, 2 SAY '     Funci¢n : ' GET  ;
  vcodfun PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfun),  ;
  val_para(vcodfun,'CODFUN',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 9, 2 SAY '    Programa : ' GET  ;
  vcodprg PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodprg),  ;
  val_para1(vcodprg,'CODPRG' +  ;
  vcodfun,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 10, 2 SAY ' SubPrograma : ' GET  ;
  vcodspr PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodspr),  ;
  val_para1(vcodspr,'CODSPR' +  ;
  vcodprg,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 11, 2 SAY 'Activ/Proyec : ' GET  ;
  vactpry PICTURE '!!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vactpry),  ;
  val_para(vactpry,'ACTPRY',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 12, 2 SAY '  Componente : ' GET  ;
  vcodcom PICTURE '!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcom),  ;
  val_para(vcodcom,'CODCOM',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 13, 2 SAY '   Al mes de : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,30)
@ 14, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30)
@ 15, 2 SAY '  P.Gen‚rica : ' GET  ;
  vpargen PICTURE '!!'
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1, 14 SAY  ;
  'Espere un momento ...' COLOR W+/ ;
  RB* 
SELECT calen
GOTO TOP
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     IF vtotal = 1
          SELECT calen.nummes,  ;
                 calen.codpart,  ;
                 calen.valpart,  ;
                 calen.periodo,  ;
                 calen.codcad,  ;
                 calen.codfte,  ;
                 calen.totafe,  ;
                 calen.ampliar,  ;
                 calen.totoc,  ;
                 calen.totos,  ;
                 calen.estfun  ;
                 FROM CALEN WHERE  ;
                 periodo =  ;
                 ALLTRIM(vperiodo)  ;
                 AND IIF( NOT  ;
                 EMPTY(ALLTRIM(vcodcad)),  ;
                 codcad =  ;
                 ALLTRIM(vcodcad),  ;
                 .T.) AND codfte =  ;
                 ALLTRIM(vcodfte)  ;
                 AND IIF( NOT  ;
                 EMPTY(ALLTRIM(vuniges)),  ;
                 uniges =  ;
                 ALLTRIM(vuniges),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vunieje)),  ;
                 unieje =  ;
                 ALLTRIM(vunieje),  ;
                 .T.) AND nummes =  ;
                 ALLTRIM(vcalend)  ;
                 INTO CURSOR  ;
                 CalenX
     ELSE
          SELECT calen.nummes,  ;
                 calen.codpart,  ;
                 calen.valpart,  ;
                 calen.periodo,  ;
                 calen.codcad,  ;
                 calen.codfte,  ;
                 calen.totafe,  ;
                 calen.ampliar,  ;
                 calen.totoc,  ;
                 calen.totos,  ;
                 calen.estfun  ;
                 FROM CALEN WHERE  ;
                 periodo =  ;
                 ALLTRIM(vperiodo)  ;
                 AND IIF( NOT  ;
                 EMPTY(ALLTRIM(vuniges)),  ;
                 SUBSTR(estfun, 1,  ;
                 2) =  ;
                 ALLTRIM(vuniges),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vunieje)),  ;
                 SUBSTR(estfun, 3,  ;
                 3) =  ;
                 ALLTRIM(vunieje),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vcodfun)),  ;
                 SUBSTR(estfun, 6,  ;
                 2) =  ;
                 ALLTRIM(vcodfun),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vcodprg)),  ;
                 SUBSTR(estfun, 8,  ;
                 3) =  ;
                 ALLTRIM(vcodprg),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vcodspr)),  ;
                 SUBSTR(estfun,  ;
                 11, 4) =  ;
                 ALLTRIM(vcodspr),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vactpry)),  ;
                 SUBSTR(estfun,  ;
                 15, 6) =  ;
                 ALLTRIM(vactpry),  ;
                 .T.) AND IIF(  ;
                 NOT  ;
                 EMPTY(ALLTRIM(vcodcom)),  ;
                 SUBSTR(estfun,  ;
                 21, 5) =  ;
                 ALLTRIM(vcodcom),  ;
                 .T.) AND codfte =  ;
                 ALLTRIM(vcodfte)  ;
                 AND nummes =  ;
                 ALLTRIM(vcalend)  ;
                 INTO CURSOR  ;
                 CalenX
     ENDIF
ENDIF
vind = SYS(3) + '.DBF'
COPY TO (vind)
USE IN 0 EXCLUSIVE (vind) ALIAS  ;
    calen1
vind = SYS(3) + '.IDX'
SELECT calen1
INDEX ON LEFT(estfun, 5) + codcad +  ;
      codfte + codpart TO (vind)
yind = SYS(3) + '.IDX'
SELECT itehc
IF vtotal = 2
     INDEX ON codpart TO (yind)  ;
           FOR codfte =  ;
           ALLTRIM(vcodfte) .AND.  ;
           nummes =  ;
           ALLTRIM(vcalend) .AND.  ;
           estado <> '99' .AND.  ;
           IIF( .NOT.  ;
           EMPTY(numpa), mespr <>  ;
           nummes, .T.) .AND.  ;
           IIF( .NOT.  ;
           EMPTY(numpr), mespr =  ;
           nummes, .T.) .AND.  ;
           IIF(EMPTY(vpargen),  ;
           .T.,  ;
           LEFT(itehc.codpart, 2) =  ;
           vpargen)
ELSE
     INDEX ON codpart TO (yind)  ;
           FOR codfte =  ;
           ALLTRIM(vcodfte) .AND.  ;
           nummes =  ;
           ALLTRIM(vcalend) .AND.  ;
           estado <> '99' .AND.  ;
           IIF( .NOT.  ;
           EMPTY(numpa), mespr <>  ;
           nummes, .T.) .AND.  ;
           IIF( .NOT.  ;
           EMPTY(numpr), mespr =  ;
           nummes, .T.) .AND.  ;
           IIF(EMPTY(vpargen),  ;
           .T.,  ;
           LEFT(itehc.codpart, 2) =  ;
           vpargen)
ENDIF
GOTO TOP
SCAN
     SELECT maepre
     SEEK vperiodo + itehc.uniges +  ;
          itehc.unieje +  ;
          itehc.codcad
     vkey = uniges + unieje +  ;
            codfun + codprg +  ;
            codspr + actpry +  ;
            itehc.codcom +  ;
            itehc.codmet +  ;
            itehc.codpart +  ;
            itehc.codfte
     SELECT calen1
     LOCATE FOR LEFT(estfun, 30) +  ;
            codpart + codfte =  ;
            vkey
     IF FOUND()
          REPLACE totafe WITH  ;
                  totafe +  ;
                  IIF(itehc.tipope =  ;
                  '-',  ;
                  itehc.valpart * - ;
                  1,  ;
                  itehc.valpart)
     ELSE
          APPEND BLANK
          REPLACE nummes WITH  ;
                  ALLTRIM(vcalend),  ;
                  codpart WITH  ;
                  itehc.codpart,  ;
                  periodo WITH  ;
                  vperiodo,  ;
                  codcad WITH  ;
                  itehc.codcad,  ;
                  codfte WITH  ;
                  itehc.codfte,  ;
                  estfun WITH  ;
                  LEFT(vkey, 30),  ;
                  totafe WITH  ;
                  IIF(itehc.tipope =  ;
                  '-',  ;
                  itehc.valpart * - ;
                  1,  ;
                  itehc.valpart)
     ENDIF
     SELECT itehc
ENDSCAN
DEACTIVATE WINDOW standby
SELECT calen1
SET FILTER TO periodo = ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), SUBSTR(estfun,;
1, 2) = ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), SUBSTR(estfun,;
3, 3) = ALLTRIM(vunieje),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfun)), SUBSTR(estfun,;
6, 2) = ALLTRIM(vcodfun),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodprg)), SUBSTR(estfun,;
8, 3) = ALLTRIM(vcodprg),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodspr)), SUBSTR(estfun,;
11, 4) = ALLTRIM(vcodspr),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vactpry)), SUBSTR(estfun,;
15, 6) = ALLTRIM(vactpry),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcom)), SUBSTR(estfun,;
21, 5) = ALLTRIM(vcodcom),;
.T.);
.AND. codfte = ALLTRIM(vcodfte);
.AND. nummes = ALLTRIM(vcalend);
.AND. ;
.NOT. EMPTY(totafe)
GOTO TOP
IF vtotal = 1
     IF vtipo = 1
          DO reporte WITH 2,  ;
             'SalCal1',  ;
             ' A nivel de Marco Presupuestal'
     ELSE
          DO reporte WITH 2,  ;
             'SalCal11',  ;
             ' A nivel de Marco Presupuestal'
     ENDIF
ELSE
     INDEX ON periodo +  ;
           LEFT(estfun, 10) +  ;
           codfte + codpart TO  ;
           (vind)
     GOTO TOP
     IF vtipo = 1
          DO reporte WITH 2,  ;
             'SalCaG1',  ;
             ' A nivel de Marco Presupuestal'
     ELSE
          DO reporte WITH 2,  ;
             'SalCaA1',  ;
             ' A nivel de Marco Presupuestal'
     ENDIF
ENDIF
USE
ERASE (vind)
SELECT calen
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION sumcal
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM valpart+ampliar TO suma FOR &vFiltro=;
vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcal1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro valpart +  ;
         ampliar TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro valpart +  ;
         ampliar TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION sumcom
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM TOTAFE TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcom1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro totafe TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro totafe  ;
         TO suma
ENDIF
GOTO vrecno
RETURN suma
*
