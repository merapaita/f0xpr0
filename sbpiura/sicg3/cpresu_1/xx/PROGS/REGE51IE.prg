CLOSE DATABASES
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 3 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 5 itehc ALIAS itehc ORDER  ;
    itehc1
USE IN 6 Itetra ALIAS itetra  ;
    ORDER Itetra1
USE IN 7 Itecre ALIAS itecre  ;
    ORDER Itecre1
USE IN 8 repopre ALIAS repo
USE IN 9 traPAR ALIAS trapar  ;
    ORDER traPAR1
USE IN 10 CRESUP ALIAS cresup  ;
    ORDER CRESUP1
USE IN 11 iteparI ALIAS itepari  ;
    ORDER iteparI1
USE IN 12 maeparI ALIAS presui  ;
    ORDER maeparI1
PUBLIC valcs, vcodprg, vcodsub,  ;
       vproyec, vcodact, vsubpry,  ;
       vgun, vpart
PUBLIC xxm01, xxm02, xxm03, xxm04,  ;
       xxm05, xxm06, xxm07, xxm08,  ;
       xxm09, xxm10, xxm11,  ;
       xxm12
PUBLIC ffm01, ffm02, ffm03, ffm04,  ;
       ffm05, ffm06, ffm07, ffm08,  ;
       ffm09, ffm10, ffm11,  ;
       ffm12
DO pantalla
RETURN
*
PROCEDURE pantalla
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' °°  Saldo Presupuestal °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
STORE 0 TO vtotal, vtipo
STORE SPACE(2) TO vperiodo,  ;
      vcodfte, vcalend, vcodfun,  ;
      vuniges
STORE SPACE(3) TO vcodprg,  ;
      vunieje
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 1, 2 SAY '  Calendario : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,25)
@ 2, 2 SAY '  Por Cadena : ' GET  ;
  vtotal SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 3, 2 SAY '  Espec¡fico : ' GET  ;
  vtipo SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 4, 2 SAY '  U. Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vuniges),  ;
  val_para(vuniges,'UNIGES',' ', ;
  18,30), .T.)
@ 5, 2 SAY 'U. Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vunieje),  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',18,30), .T.)
@ 6, 2 SAY 'Cad. Funcion.: ' GET  ;
  vcodcad PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcad),  ;
  val_codcad(vcodcad,vperiodo +  ;
  ALLTRIM(vuniges) +  ;
  ALLTRIM(vunieje),' ',18,30),  ;
  .T.) WHEN vtotal = 1
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
@ 14, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfte),  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30), .T.)
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     RETURN
ENDIF
SELECT repo
vdbf = SYS(3) + '.dbf'
COPY TO (vdbf) STRUCTURE
USE IN 8 EXCLUSIVE (vdbf) ALIAS  ;
    repo
SELECT repo
ZAP
SELECT itepari
IF EOF()
     DO standby WITH vmens08
ELSE
     DEFINE WINDOW xwait FROM 20,  ;
            06 TO 22, 78 COLOR  ;
            SCHEME 05
     ACTIVATE WINDOW xwait
     @ 0, 10 SAY  ;
       ' Espere un Momento...Procesando el Listado E-5!'  ;
       COLOR W+/RB* 
     SELECT repo
     vind = SYS(3) + '.IDX'
     yind = SYS(3) + '.IDX'
     INDEX ON codcad + codfte +  ;
           codpart TO (vind)
     SET INDEX TO (vind)
     IF vtotal = 1
          SELECT itepari
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
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
     ELSE
          SELECT itepari
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
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.)
     ENDIF
     GOTO TOP
     SCAN
          SCATTER MEMVAR
          m.cresup = getcre()
          m.transf = gettra()
          SELECT repo
          SEEK itepari.codcad +  ;
               itepari.codfte +  ;
               itepari.codpart
          vcod = 'FTE' +  ;
                 ALLTRIM(m.codfte)
          m.totcal = 0
          IF  .NOT. FOUND()
               m.&vcod = m.valpart+m.cresup+m.transf
               APPEND BLANK
               GATHER MEMVAR
               m.&vcod=0
          ELSE
               IF RLOCK()
                    REPLACE &vcod WITH;
&vcod + m.valpart+m.cresup+m.transf
               ENDIF
               UNLOCK
               &vcod = 0
          ENDIF
          SELECT itepari
     ENDSCAN
     SELECT repo
     zind = SYS(3) + '.IDX'
     INDEX ON LEFT(estfun, 30) +  ;
           codfte + codpart TO  ;
           (zind)
     SELECT itehc
     SET FILTER TO nummes <= ALLTRIM(vcalend);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodfte)), codfte;
= ALLTRIM(vcodfte),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vuniges)), uniges;
= ALLTRIM(vuniges),;
.T.);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vunieje)), unieje;
= ALLTRIM(vunieje),;
.T.);
.AND. estado <> '99';
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
     GOTO TOP
     SCAN
          SELECT maepre
          SEEK vperiodo +  ;
               itehc.uniges +  ;
               itehc.unieje +  ;
               itehc.codcad
          vkey = uniges + unieje +  ;
                 codfun + codprg +  ;
                 codspr + actpry +  ;
                 itehc.codcom +  ;
                 itehc.codmet +  ;
                 itehc.codfte +  ;
                 itehc.codpart
          vkey1 = uniges + unieje +  ;
                  codfun + codprg +  ;
                  codspr + actpry +  ;
                  itehc.codcom +  ;
                  itehc.codmet +  ;
                  itehc.codfte
          SELECT repo
          SEEK vkey
          IF FOUND()
               REPLACE totafe  ;
                       WITH  ;
                       totafe +  ;
                       IIF(itehc.tipope =  ;
                       '-',  ;
                       itehc.valpart * - ;
                       1,  ;
                       itehc.valpart)
          ELSE
               GOTO TOP
               LOCATE FOR  ;
                      LEFT(estfun,  ;
                      30) +  ;
                      codfte =  ;
                      vkey1
               IF FOUND()
                    APPEND BLANK
                    REPLACE codpart  ;
                            WITH  ;
                            itehc.codpart,  ;
                            periodo  ;
                            WITH  ;
                            vperiodo,  ;
                            codcad  ;
                            WITH  ;
                            itehc.codcad,  ;
                            codfte  ;
                            WITH  ;
                            itehc.codfte,  ;
                            estfun  ;
                            WITH  ;
                            LEFT(vkey,  ;
                            30),  ;
                            totafe  ;
                            WITH  ;
                            totafe +  ;
                            IIF(itehc.tipope =  ;
                            '-',  ;
                            itehc.valpart * - ;
                            1,  ;
                            itehc.valpart)
               ENDIF
          ENDIF
          IF itehc.nummes =  ;
             ALLTRIM(vcalend)
               REPLACE totcal  ;
                       WITH  ;
                       totcal +  ;
                       IIF(itehc.tipope =  ;
                       '-',  ;
                       itehc.valpart * - ;
                       1,  ;
                       itehc.valpart)
          ENDIF
          SELECT itehc
     ENDSCAN
     SELECT repo
     GOTO TOP
     SCAN
          vkey = periodo +  ;
                 LEFT(estfun, 30) +  ;
                 codfte
          SELECT itepari
          SET ORDER TO ITEPARI4
          SEEK vkey
          IF FOUND()
               REPLACE repo.codcad  ;
                       WITH  ;
                       itepari.codcad
          ENDIF
          SELECT repo
     ENDSCAN
     SELECT repo
     vind = SYS(3) + '.IDX'
     INDEX ON LEFT(estfun, 5) +  ;
           codcad + codfte +  ;
           codpart TO (vind)
     SET INDEX TO (vind)
     RELEASE WINDOW xwait
     SELECT repo
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existe Registros para procesar'
     ELSE
          IF vtotal = 1
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisE51',  ;
                       ' Consolidado de la Ejecucion ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'LisE52',  ;
                       ' Consolidado de la Ejecucion ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ELSE
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'LisE5G1',  ;
                       ' Consolidado de la Ejecucion ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'LisE5G2',  ;
                       ' Consolidado de la Ejecucion ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ENDIF
     ENDIF
ENDIF
CLOSE DATABASES
RETURN
*
FUNCTION sumpre2
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM FTE00+FTE01+FTE09 TO suma FOR &vFiltro=;
vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcal
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM totcal TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumafe
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM totafe TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumsal
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM (FTE00+FTE01+FTE09)-TOTAFE TO suma;
FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
