CLOSE DATABASES
USE IN 1 parmae ALIAS parma ORDER  ;
    parmae1
USE IN 2 maepar ALIAS presu ORDER  ;
    maepar1
USE IN 3 itepar ALIAS itepar  ;
    ORDER itepar1
USE IN 4 maepre ALIAS maepre  ;
    ORDER maepre1
USE IN 5 IteHc ALIAS itehc ORDER  ;
    Itehc1
USE IN 10 Itetra ALIAS itetra  ;
    ORDER Itetra1
USE IN 11 Itecre ALIAS itecre  ;
    ORDER Itecre1
USE IN 12 cresup ALIAS cresup  ;
    ORDER cresup1
USE IN 13 Trapar ALIAS trapar  ;
    ORDER trapar1
PUBLIC vcodsub, vcodact, vproyec,  ;
       vsubpry, vcalend
vmens01 = 'Registro de Presupuesto'
vmens02 = ' Presupuesto : REVISION '
vmens04 = 'Dicho Presupuesto no fue encontrado'
vmens05 = 'No existe Presupuesto anterior'
vmens06 = 'No existe Presupuesto siguiente'
vmens07 = '¨ Desea Anular ‚ste Presupuesto ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Presupuesto ha sido anulado'
vmens10 = 'El Presupuesto ya est  Atendido'
vmens11 = 'El Presupuesto ha sido devuelto'
tot = 0
tot1 = 0
DO inicia
DO salida
CLOSE DATABASES
RETURN
*
PROCEDURE inicia
PRIVATE vtexp
SELECT presu
ord = ORDER()
vtexp = RECNO()
STORE 0 TO vtotal, vtipo, vproact
vuniges = '01'
vunieje = '001'
STORE SPACE(2) TO vperiodo,  ;
      vcodfte, vcalend, vcodfun
STORE SPACE(3) TO vcodprg
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' °°  Saldo Presupuestal °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 1, 2 SAY '   Al Mes de : ' GET  ;
  vcalend PICTURE '!!' VALID  ;
  val_para(vcalend,'FECMES',' ', ;
  18,25)
@ 3, 2 SAY '  Por Cadena : ' GET  ;
  vtotal SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 4, 2 SAY '  Espec¡fico : ' GET  ;
  vtipo SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 5, 2 SAY '  U. Gestora : ' GET  ;
  vuniges PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vuniges),  ;
  val_para(vuniges,'UNIGES',' ', ;
  18,30), .T.)
@ 6, 2 SAY 'U. Ejecutora : ' GET  ;
  vunieje PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vunieje),  ;
  val_para1(vunieje,'UNIEJE' +  ;
  vuniges,' ',18,30), .T.)
@ 7, 2 SAY 'Cad. Funcion.: ' GET  ;
  vcodcad PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodcad),  ;
  val_codcad(vcodcad,vperiodo,' ', ;
  18,30), .T.) WHEN vtotal = 1
@ 8, 2 SAY '   Proy/Act. : ' GET  ;
  vproact SIZE 1, 7, 6 FUNCTION  ;
  '*RNH \<Si;\<No;\<Todo'
@ 9, 2 SAY '     Funci¢n : ' GET  ;
  vcodfun PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfun),  ;
  val_para(vcodfun,'CODFUN',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 10, 2 SAY '    Programa : ' GET  ;
  vcodprg PICTURE '!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodprg),  ;
  val_para1(vcodprg,'CODPRG' +  ;
  vcodfun,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 11, 2 SAY ' SubPrograma : ' GET  ;
  vcodspr PICTURE '!!!!' VALID  ;
  IIF( .NOT. EMPTY(vcodspr),  ;
  val_para1(vcodspr,'CODSPR' +  ;
  vcodprg,' ',18,30), .T.) WHEN  ;
  vtotal = 2
@ 12, 2 SAY 'Activ/Proyec : ' GET  ;
  vactpry PICTURE '!!!!!!' VALID  ;
  IIF( .NOT. EMPTY(vactpry),  ;
  val_para(vactpry,'ACTPRY',' ', ;
  18,30), .T.) WHEN vtotal = 2
@ 13, 2 SAY '  Componente : ' GET  ;
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
SELECT itepar
IF EOF()
     DO standby WITH vmens08
ELSE
     DEFINE WINDOW xwait FROM 20,  ;
            06 TO 22, 78 COLOR  ;
            SCHEME 05
     ACTIVATE WINDOW xwait
     @ 0, 10 SAY  ;
       ' Espere un Momento...Procesando Saldos Presupuestales !'  ;
       COLOR W+/RB* 
     USE IN 6 repcal1
     SELECT 6
     vind = SYS(3) + '.DBF'
     COPY TO (vind) STRUCTURE
     USE IN 6 EXCLUSIVE (vind)  ;
         ALIAS repo
     SELECT repo
     ZAP
     vind = SYS(3) + '.IDX'
     INDEX ON LEFT(estfun, 5) +  ;
           codcad + codfte +  ;
           codpart TO (vind)
     SET INDEX TO (vind)
     IF vtotal = 1
          SELECT itepar
          SET FILTER TO periodo = ALLTRIM(vperiodo);
.AND. IIF(;
.NOT. EMPTY(ALLTRIM(vcodcad)), codcad;
= ALLTRIM(vcodcad),;
.T.);
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
          SELECT repo
          SEEK LEFT(itepar.estfun,  ;
               5) + itepar.codcad +  ;
               itepar.codfte +  ;
               itepar.codpart
          m.transf = m.tra001 +  ;
                     m.tra003 +  ;
                     m.tra004 +  ;
                     m.tra005
          STORE 0 TO m.m_01,  ;
                m.m_02, m.m_03,  ;
                m.m_04, m.m_05,  ;
                m.m_06
          STORE 0 TO m.m_07,  ;
                m.m_08, m.m_09,  ;
                m.m_10, m.m_11,  ;
                m.m_12
          m.cresup = getcre()
          m.transf = gettra()
          IF  .NOT. FOUND()
               m.valpres = m.valpart
               APPEND BLANK
               GATHER MEMVAR
          ELSE
               IF RLOCK()
                    REPLACE valpres  ;
                            WITH  ;
                            m.valpart
                    REPLACE cresup  ;
                            WITH  ;
                            m.cresup
                    REPLACE transf  ;
                            WITH  ;
                            m.transf
               ENDIF
               UNLOCK
          ENDIF
          SELECT itepar
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
.AND. estado <> '99'
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
          vmes = 'M_' +  ;
                 ALLTRIM(itehc.nummes)
          SELECT repo
          SEEK vkey
          IF FOUND()
               REPLACE &vmes WITH &vmes+IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
          ELSE
               GOTO TOP
               LOCATE FOR  ;
                      LEFT(estfun,  ;
                      30) +  ;
                      codfte =  ;
                      vkey1
               IF FOUND()
                    APPEND BLANK
                    REPLACE CODPART WITH;
ITEHC.CODPART, PERIODO WITH VPERIODO,;
CODCAD  WITH ITEHC.CODCAD, CODFTE;
 WITH ITEHC.CODFTE, ESTFUN  WITH LEFT(VKEY,30),;
&vmes   WITH IIF(ITEHC.TIPOPE='-',ITEHC.VALPART*-1,ITEHC.VALPART)
               ENDIF
          ENDIF
          SELECT itehc
     ENDSCAN
     SELECT 4
     USE
     USE IN 4 \EMER97\DATA\maepre  ;
         ALIAS maepree ORDER  ;
         maepre1
     SELECT 5
     USE
     USE IN 5 \EMER97\DATA\IteHc  ;
         ALIAS itehce ORDER  ;
         Itehc1
     SELECT itehce
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
.AND. estado <> '99'
     GOTO TOP
     SCAN
          SELECT maepree
          SEEK vperiodo +  ;
               itehce.uniges +  ;
               itehce.unieje +  ;
               itehce.codcad
          vkey = uniges + unieje +  ;
                 codfun + codprg +  ;
                 codspr + actpry +  ;
                 itehce.codcom +  ;
                 '00001' +  ;
                 itehce.codfte +  ;
                 itehce.codpart
          vkey1 = uniges + unieje +  ;
                  codfun + codprg +  ;
                  codspr + actpry +  ;
                  itehce.codcom +  ;
                  '00001' +  ;
                  itehce.codfte
          vmes = 'M_' +  ;
                 ALLTRIM(itehce.nummes)
          SELECT repo
          SEEK vkey
          IF FOUND()
               REPLACE &vmes WITH &vmes+IIF(ITEHCE.TIPOPE='-',ITEHCE.VALPART*-1,ITEHCE.VALPART)
          ELSE
               GOTO TOP
               LOCATE FOR  ;
                      LEFT(estfun,  ;
                      30) +  ;
                      codfte =  ;
                      vkey1
               IF FOUND()
                    APPEND BLANK
                    REPLACE CODPART WITH;
ITEHCE.CODPART, PERIODO WITH VPERIODO,;
CODCAD  WITH ITEHCE.CODCAD, CODFTE;
 WITH ITEHCE.CODFTE, ESTFUN  WITH LEFT(VKEY,30),;
&vmes   WITH IIF(ITEHCE.TIPOPE='-',ITEHCE.VALPART*-1,ITEHCE.VALPART)
               ENDIF
          ENDIF
          SELECT itehce
     ENDSCAN
     SELECT repo
     GOTO TOP
     SCAN
          vkey = periodo +  ;
                 LEFT(estfun, 30) +  ;
                 codfte
          SELECT itepar
          SET ORDER TO ITEPAR4
          SEEK vkey
          IF FOUND()
               REPLACE repo.codcad  ;
                       WITH  ;
                       itepar.codcad
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
     IF EOF()
          DO standby WITH  ;
             'No existe Registros para procesar'
     ELSE
          DO CASE
               CASE vproact = 1
                    SET FILTER TO SUBSTR(estfun,;
21, 1) = '2'
               CASE vproact = 2
                    SET FILTER TO SUBSTR(estfun,;
21, 1) = '1'
          ENDCASE
          IF vtotal = 1
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'SalPrem1',  ;
                       ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'SalPrem2',  ;
                       ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ELSE
               IF vtipo = 1
                    DO reporte  ;
                       WITH 2,  ;
                       'SalPrmG1',  ;
                       ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                       1, .F.,  ;
                       .T.
               ELSE
                    DO reporte  ;
                       WITH 2,  ;
                       'SalPrmG2',  ;
                       ' Consolidado Presupuesto Anual(Funccionamiento) ',  ;
                       1, .F.,  ;
                       .T.
               ENDIF
          ENDIF
          IF vproact < 3
               SET FILTER TO
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION sumpre1
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM valpres+cresup+transf TO suma FOR;
&vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION summes
PARAMETER vcalen, part, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM &part TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumar
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12;
 TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION salprg
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
     CASE vnivel = '3'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+codpart'
     CASE vnivel = '4'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte+LEFT(codpart,2)'
ENDCASE
SUM valpres+cresup+transf-(M_01+M_02+M_03+M_04+M_05+M_06+M_07+M_08+M_09+M_10+M_11+M_12);
TO sumA FOR &vFiltro = vCalen
GOTO vrec
RETURN suma
*
FUNCTION summef
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrecno = RECNO()
GOTO TOP
IF vtipo <= 25
     SUM FOR LEFT(estfun, vtipo) =  ;
         vfiltro valpres + cresup +  ;
         transf TO suma
ELSE
     SUM FOR LEFT(estfun, 25) +  ;
         codfte = vfiltro valpres +  ;
         cresup + transf TO suma
ENDIF
GOTO vrecno
RETURN suma
*
FUNCTION summes1
PARAMETER vfiltro, part
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
IF vtipo <= 25
     SUM &part TO suma FOR LEFT(ESTFUN,vtipo);
= vFiltro
ELSE
     SUM &part TO suma FOR LEFT(ESTFUN,25)+codfte;
= vFiltro
ENDIF
GOTO vrec
RETURN suma
*
FUNCTION sumar1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
DO CASE
     CASE vtipo <= 25
          SUM FOR LEFT(estfun,  ;
              vtipo) = vfiltro  ;
              m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12  ;
              TO suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro m_01 + m_02 +  ;
              m_03 + m_04 + m_05 +  ;
              m_06 + m_07 + m_08 +  ;
              m_09 + m_10 + m_11 +  ;
              m_12 TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro m_01 + m_02 +  ;
              m_03 + m_04 + m_05 +  ;
              m_06 + m_07 + m_08 +  ;
              m_09 + m_10 + m_11 +  ;
              m_12 TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12  ;
              TO suma
ENDCASE
GOTO vrec
RETURN suma
*
FUNCTION salprg1
PARAMETER vfiltro
vfiltro = ALLTRIM(vfiltro)
vtipo = LEN(vfiltro)
vrec = RECNO()
DO CASE
     CASE vtipo <= 25
          SUM FOR LEFT(estfun,  ;
              vtipo) = vfiltro  ;
              fte00 + fte01 +  ;
              fte09 - (m_01 +  ;
              m_02 + m_03 + m_04 +  ;
              m_05 + m_06 + m_07 +  ;
              m_08 + m_09 + m_10 +  ;
              m_11 + m_12) TO  ;
              suma
     CASE vtipo > 25 .AND. vtipo <  ;
          28
          SUM FOR LEFT(estfun,  ;
              25) + codfte =  ;
              vfiltro fte00 +  ;
              fte01 + fte09 -  ;
              (m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12)  ;
              TO suma
     CASE vtipo > 28 .AND. vtipo <  ;
          30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              LEFT(codpart, 2) =  ;
              vfiltro fte00 +  ;
              fte01 + fte09 -  ;
              (m_01 + m_02 + m_03 +  ;
              m_04 + m_05 + m_06 +  ;
              m_07 + m_08 + m_09 +  ;
              m_10 + m_11 + m_12)  ;
              TO suma
     CASE vtipo > 30
          SUM FOR LEFT(estfun,  ;
              25) + codfte +  ;
              codpart = vfiltro  ;
              fte00 + fte01 +  ;
              fte09 - (m_01 +  ;
              m_02 + m_03 + m_04 +  ;
              m_05 + m_06 + m_07 +  ;
              m_08 + m_09 + m_10 +  ;
              m_11 + m_12) TO  ;
              suma
ENDCASE
GOTO vrec
RETURN suma
*
