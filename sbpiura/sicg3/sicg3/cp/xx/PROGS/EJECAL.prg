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
    ORDER itepar1
USE IN 8 IteHc ALIAS itehc ORDER  ;
    Itehc1
USE IN 10 Itetra ALIAS itetra  ;
    ORDER Itetra1
USE IN 11 Itecre ALIAS itecre  ;
    ORDER Itecre1
USE IN 12 HOJMOD ALIAS hojmod  ;
    ORDER hojmod1
USE IN 13 cresup ALIAS cresup  ;
    ORDER cresup1
USE IN 14 Trapar ALIAS trapar  ;
    ORDER trapar1
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
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
SELECT calen
vtemp = RECNO()
vperiodo = RIGHT(STR(YEAR(DATE()),  ;
           4), 2)
STORE 0 TO vtotal, vtipo
STORE SPACE(2) TO vcodfte,  ;
      vcalend, vcodfun, vuniges
STORE SPACE(3) TO vcodprg,  ;
      vunieje
STORE SPACE(4) TO vcodcad,  ;
      vcodspr
STORE SPACE(5) TO vcodcom,  ;
      vcodmet
STORE SPACE(6) TO vcodpart,  ;
      vactpry
vtrimes = ' '
DEFINE WINDOW lis_1 FROM 4, 10 TO  ;
       20, 70 FLOAT TITLE  ;
       ' °°  Consolidado Presupuesto Anual  °° '  ;
       DOUBLE COLOR SCHEME 5
ACTIVATE WINDOW lis_1
@ 0, 2 SAY '     Periodo : ' GET  ;
  vperiodo PICTURE '!!' VALID   ;
  .NOT. EMPTY(vperiodo)
@ 1, 2 SAY '  Por Cadena : ' GET  ;
  vtotal SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
@ 2, 2 SAY '  Espec¡fico : ' GET  ;
  vtipo SIZE 1, 10, 6 FUNCTION  ;
  '*RNH \<Si;\<No'
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
@ 13, 2 SAY '   Fte. Fto. : ' GET  ;
  vcodfte PICTURE '!!' VALID IIF(  ;
  .NOT. EMPTY(vcodfte),  ;
  val_para(vcodfte,'CODFTE',' ', ;
  18,30), .T.)
@ 14, 2 SAY ' N§Trimestre : ' GET  ;
  vtrimes PICTURE '@M 1,2,3,4,T'
READ VALID val_read()
DEACTIVATE WINDOW lis_1
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
SELECT itepar
IF EOF()
     DO standby WITH vmens08
ELSE
     ACTIVATE WINDOW standby
     @ 1, 14 SAY  ;
       'Espere un momento ...'  ;
       COLOR W+/RB* 
     USE IN 6
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
     GOTO TOP
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
          IF  .NOT. FOUND()
               m.valpres = m.valpart
               APPEND BLANK
               GATHER MEMVAR
          ELSE
               IF RLOCK()
                    REPLACE valpres  ;
                            WITH  ;
                            valpres +  ;
                            m.valpart
                    REPLACE cresup  ;
                            WITH  ;
                            cresup +  ;
                            m.cresup
                    REPLACE transf  ;
                            WITH  ;
                            cresup +  ;
                            m.transf
               ENDIF
               UNLOCK
          ENDIF
          SELECT calen
          vkey = itepar.periodo +  ;
                 itepar.uniges +  ;
                 itepar.unieje +  ;
                 itepar.codcad +  ;
                 itepar.codfte +  ;
                 itepar.codpart
          vkey0 = itepar.periodo +  ;
                  itepar.uniges +  ;
                  itepar.unieje +  ;
                  itepar.codcad +  ;
                  itepar.codfte +  ;
                  itepar.codpart
          SEEK vkey
          IF FOUND()
               DO WHILE vkey= ;
                  vkey0 .AND.   ;
                  .NOT. EOF()
                    vtotcal = 0
                    vtotafe = 0
                    vkey1 = periodo +  ;
                            uniges +  ;
                            unieje +  ;
                            codcad +  ;
                            codfte +  ;
                            codpart +  ;
                            nummes
                    vkey2 = periodo +  ;
                            uniges +  ;
                            unieje +  ;
                            codcad +  ;
                            codfte +  ;
                            codpart +  ;
                            nummes
                    vcod1 = 'C_' +  ;
                            ALLTRIM(nummes)
                    DO WHILE  ;
                       vkey1= ;
                       vkey2
                         vtotcal =  ;
                          vtotcal +  ;
                          valpart +  ;
                          ampliar
                         SKIP
                         vkey2 = periodo +  ;
                                 uniges +  ;
                                 unieje +  ;
                                 codcad +  ;
                                 codfte +  ;
                                 codpart +  ;
                                 nummes
                    ENDDO
                    SELECT repo
                    REPLACE &vcod1 WITH;
&vcod1 + vTotCal
                    SELECT calen
                    vkey0 = periodo +  ;
                            uniges +  ;
                            unieje +  ;
                            codcad +  ;
                            codfte +  ;
                            codpart
               ENDDO
          ENDIF
          SELECT itepar
     ENDSCAN
     SELECT repo
     zind = SYS(3) + '.IDX'
     INDEX ON LEFT(estfun, 30) +  ;
           codfte + codpart TO  ;
           (zind)
     SELECT itehc
     DO CASE
          CASE vtrimes = '1'
               SET FILTER TO nummes <=;
'03' .AND. IIF( .NOT. EMPTY(ALLTRIM(vcodcad)), codcad = ALLTRIM(vcodcad), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vcodfte)), codfte = ALLTRIM(vcodfte), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vuniges)), uniges = ALLTRIM(vuniges), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vunieje)), unieje = ALLTRIM(vunieje), .T.) .AND. IIF( .NOT. EMPTY(numpa), mespr <> nummes, .T.) .AND. IIF( .NOT. EMPTY(numpr), mespr = nummes, .T.)
          CASE vtrimes = '2'
               SET FILTER TO nummes <=;
'06' .AND. IIF( .NOT. EMPTY(ALLTRIM(vcodcad)), codcad = ALLTRIM(vcodcad), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vcodfte)), codfte = ALLTRIM(vcodfte), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vuniges)), uniges = ALLTRIM(vuniges), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vunieje)), unieje = ALLTRIM(vunieje), .T.) .AND. IIF( .NOT. EMPTY(numpa), mespr <> nummes, .T.) .AND. IIF( .NOT. EMPTY(numpr), mespr = nummes, .T.)
          CASE vtrimes = '3'
               SET FILTER TO nummes <=;
'09' .AND. IIF( .NOT. EMPTY(ALLTRIM(vcodcad)), codcad = ALLTRIM(vcodcad), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vcodfte)), codfte = ALLTRIM(vcodfte), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vuniges)), uniges = ALLTRIM(vuniges), .T.) .AND. IIF( .NOT. EMPTY(ALLTRIM(vunieje)), unieje = ALLTRIM(vunieje), .T.) .AND. IIF( .NOT. EMPTY(numpa), mespr <> nummes, .T.) .AND. IIF( .NOT. EMPTY(numpr), mespr = nummes, .T.)
          OTHERWISE
               SET FILTER TO IIF(;
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
.T.);
.AND. IIF(;
.NOT. EMPTY(numpa), mespr <> nummes,;
.T.);
.AND. IIF(;
.NOT. EMPTY(numpr), mespr = nummes,;
.T.)
     ENDCASE
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
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existe Registros para procesar'
     ELSE
          DEACTIVATE WINDOW  ;
                     standby
          IF vtotal = 1
               IF vtipo = 1
                    IF vtrimes =  ;
                       'T'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'Liscal1',  ;
                            ' Presupuesto a nivel Trimestral(01) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'Liscal2',  ;
                            ' Presupuesto a nivel Trimestral(02) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'Liscal3',  ;
                            ' Presupuesto a nivel Trimestral(03) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'Liscal4',  ;
                            ' Presupuesto a nivel Trimestral(04) '
                    ELSE
                         vrepo = 'Liscal' +  ;
                                 vtrimes
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            vrepo,  ;
                            ' Presupuesto a nivel Trimestral'+ ;
                            '('+ ;
                            vtrimes+ ;
                            ')'
                    ENDIF
               ELSE
                    IF vtrimes =  ;
                       'T'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCal11',  ;
                            ' Presupuesto a nivel Trimestral(01) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCal22',  ;
                            ' Presupuesto a nivel Trimestral(02) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCal33',  ;
                            ' Presupuesto a nivel Trimestral(03) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCal44',  ;
                            ' Presupuesto a nivel Trimestral(04) '
                    ELSE
                         vrepo = 'LisCal' +  ;
                                 vtrimes +  ;
                                 vtrimes
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            vrepo,  ;
                            ' Presupuesto a nivel Trimestral'+ ;
                            '('+ ;
                            vtrimes+ ;
                            ')'
                    ENDIF
               ENDIF
          ELSE
               IF vtipo = 1
                    IF vtrimes =  ;
                       'T'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG1',  ;
                            ' Presupuesto a nivel Trimestral(01) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG2',  ;
                            ' Presupuesto a nivel Trimestral(02) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG3',  ;
                            ' Presupuesto a nivel Trimestral(03) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG4',  ;
                            ' Presupuesto a nivel Trimestral(04) '
                    ELSE
                         vrepo = 'LisCaG' +  ;
                                 vtrimes
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            vrepo,  ;
                            ' Presupuesto a nivel Trimestral'+ ;
                            '('+ ;
                            vtrimes+ ;
                            ')'
                    ENDIF
               ELSE
                    IF vtrimes =  ;
                       'T'
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG11',  ;
                            ' Presupuesto a nivel Trimestral(01) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG22',  ;
                            ' Presupuesto a nivel Trimestral(02) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG33',  ;
                            ' Presupuesto a nivel Trimestral(03) '
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            'LisCaG44',  ;
                            ' Presupuesto a nivel Trimestral(04) '
                    ELSE
                         vrepo = 'LisCaG' +  ;
                                 vtrimes +  ;
                                 vtrimes
                         DO reporte  ;
                            WITH  ;
                            2,  ;
                            vrepo,  ;
                            ' Presupuesto a nivel Trimestral'+ ;
                            '('+ ;
                            vtrimes+ ;
                            ')'
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
ON KEY LABEL F9
RESTORE SCREEN FROM principal
RETURN
*
PROCEDURE pendiente
DO CASE
     CASE estado = '00'
          REPLACE estado WITH  ;
                  '10'
     CASE estado = '10'
          REPLACE estado WITH  ;
                  '20'
ENDCASE
DO vista
RETURN
*
PROCEDURE salida
RELEASE WINDOW libdir
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION sumpre
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM VALPRES TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumcre
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM CRESUP TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumtra
PARAMETER vcalen, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'CODCAD'
     CASE vnivel = '2'
          vfiltro = 'CODCAD+codfte'
ENDCASE
SUM TRANSF TO suma FOR &vFiltro= vCalen
GOTO vrec
RETURN suma
*
FUNCTION sumar_prg
PARAMETER vcalen, part, vnivel
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = 1
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = 2
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
SUM &part TO sumsg FOR &vFiltro= vCalen
GOTO vrec
RETURN sumsg
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
FUNCTION totalprg1
PARAMETER vcalen, vnivel, vtipo,  ;
          vtrim
totalt = 0
vrec = RECNO()
GOTO TOP
DO CASE
     CASE vnivel = '1'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD'
     CASE vnivel = '2'
          vfiltro = 'LEFT(ESTFUN,5)+CODCAD+codfte'
ENDCASE
IF vtipo = '1'
     DO CASE
          CASE vtrim = '1'
               SUM  M_01+M_02+M_03 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '2'
               SUM  M_04+M_05+M_06 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '3'
               SUM  M_07+M_08+M_09 TO;
totalm FOR &vFiltro= vCalen
          CASE vtrim = '4'
               SUM  M_10+M_11+M_12 TO;
totalm FOR &vFiltro= vCalen
     ENDCASE
ELSE
     DO CASE
          CASE vtrim = '1'
               SUM C_01+C_02+C_03 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '2'
               SUM C_04+C_05+C_06 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '3'
               SUM C_07+C_08+C_09 TO totalm;
FOR &vFiltro= vCalen
          CASE vtrim = '4'
               SUM C_10+C_11+C_12 TO totalm;
FOR &vFiltro= vCalen
     ENDCASE
ENDIF
totalt = totalt + totalm
GOTO TOP
GOTO vrec
RETURN totalt
*
