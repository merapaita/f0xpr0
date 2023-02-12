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
DO pantalla
CLOSE DATABASES
RETURN
*
PROCEDURE pantalla
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' °°  Actualizaci¢n Presupuestal °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
STORE 0 TO vtotal, vtipo
STORE SPACE(2) TO vperiodo,  ;
      vcodfte, vcalend, vcodfun
STORE SPACE(3) TO vcodprg
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
vuniges = '01'
vunieje = '001'
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
SELECT itepar
IF EOF()
     DO standby WITH vmens08
ELSE
     DEFINE WINDOW xwait FROM 20,  ;
            06 TO 22, 78 COLOR  ;
            SCHEME 05
     ACTIVATE WINDOW xwait
     @ 0, 10 SAY  ;
       ' Espere un Momento...Actualizando Presupuesto!'  ;
       COLOR W+/RB* 
     SELECT repo
     vind = SYS(3) + '.IDX'
     yind = SYS(3) + '.IDX'
     INDEX ON codcad + codfte +  ;
           codpart TO (vind)
     SET INDEX TO (vind)
     IF vtotal = 1
          SELECT itepar
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
          SELECT itepar
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
          SELECT maepre
          SEEK itepar.periodo +  ;
               itepar.uniges +  ;
               itepar.unieje +  ;
               itepar.codcad
          m.metas = ALLTRIM(descri)
          SELECT repo
          SEEK itepar.codcad +  ;
               itepar.codfte +  ;
               itepar.codpart
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
          SELECT itepar
     ENDSCAN
     SELECT repo
     zind = SYS(3) + '.IDX'
     INDEX ON codcad + codfte +  ;
           codpart TO (zind)
     SELECT itehc
     SET FILTER TO IIF(;
.NOT. EMPTY(numhm), nummeshm <= ALLTRIM(vcalend),;
nummes <= ALLTRIM(vcalend));
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
          vkey = itehc.codcad +  ;
                 itehc.codfte +  ;
                 itehc.codpart
          vkey1 = uniges + unieje +  ;
                  codfun + codprg +  ;
                  codspr + actpry +  ;
                  itehc.codcom +  ;
                  itehc.codmet
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
                       WITH vkey1,  ;
                       totafe  ;
                       WITH  ;
                       totafe +  ;
                       IIF(itehc.tipope =  ;
                       '-',  ;
                       itehc.valpart * - ;
                       1,  ;
                       itehc.valpart)
          ENDIF
          IF itehc.nummeshm =  ;
             ALLTRIM(vcalend)
               REPLACE totcal  ;
                       WITH  ;
                       totcal +  ;
                       IIF(itehc.tipope =  ;
                       '-',  ;
                       itehc.valpart * - ;
                       1,  ;
                       itehc.valpart)
          ELSE
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
          ENDIF
          SELECT itehc
     ENDSCAN
     SELECT repo
     vind = SYS(3) + '.IDX'
     INDEX ON LEFT(estfun, 5) +  ;
           codcad + codfte +  ;
           codpart TO (vind)
     SET INDEX TO (vind)
     SELECT repo
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existe Registros para procesar'
     ELSE
          DO actualiza
     ENDIF
     RELEASE WINDOW xwait
ENDIF
CLOSE DATABASES
RETURN
*
PROCEDURE actualiza
SELECT repo
SCAN
     vkey = periodo + uniges +  ;
            unieje + codcad +  ;
            codfte + codpart
     vtotcal = totcal
     vmes = 'M_' + vcalend
     SELECT itepar
     SEEK vkey
     IF FOUND()
          REPLACE &vmes with vtotcal
          DO chequeoa
     ENDIF
     SELECT repo
ENDSCAN
*
FUNCTION chequeoa
vtotmes = m_01 + m_02 + m_03 +  ;
          m_04 + m_05 + m_06 +  ;
          m_07 + m_08 + m_09 +  ;
          m_10 + m_11 + m_12
vvalpart = valpart + cresup +  ;
           tra001 + tra003 +  ;
           tra004 + tra005
IF vvalpart < vtotmes
     vsaldo = vtotmes - vvalpart
ENDIF
REPLACE tri_01 WITH m_01 + m_02 +  ;
        m_03
REPLACE tri_02 WITH m_04 + m_05 +  ;
        m_06
REPLACE tri_03 WITH m_07 + m_08 +  ;
        m_09
REPLACE tri_04 WITH m_10 + m_11 +  ;
        m_12
RETURN .T.
*
