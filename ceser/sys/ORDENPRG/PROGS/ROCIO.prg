*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DO carsa
RETURN
*
PROCEDURE carsa
CLEAR
SET SYSMENU ON
SET ESCAPE ON
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER 1
SELECT 2
USE SHARED st_sicli ORDER 1
SELECT 3
USE ca_iorep ALIAS iorep ORDER  ;
    ord_fecemi
DIMENSION can( 5)
w_anio = 1996
w_fecha = DATE()
w_codemi = '200'
SELECT 4
CREATE CURSOR b_carsa (mes N (2,  ;
       0), codemi C (4), cantidad  ;
       N (6, 0), repara N (6, 0),  ;
       domici N (6, 0), domrep N  ;
       (6), domins N (6, 0))
SELECT iorep
SEEK STR(MONTH({02/01/1996}), 2) +  ;
     SUBSTR(w_codemi, 1, 1)
BROWSE
IF FOUND()
     DO WHILE YEAR(fecemi)=w_anio  ;
        .AND.  .NOT. EOF()
          w_fecha = fecemi
          w_codemi = codemi
          STORE 0 TO can( 1),  ;
                can( 2), can( 3),  ;
                can( 4), can( 5)
          @ 10, 20 SAY  ;
            iorep.numdoc
          DO WHILE MONTH(fecemi)= ;
             MONTH(w_fecha) .AND.   ;
             .NOT. EOF()
               IF codemi <>  ;
                  w_codemi
                    SELECT b_carsa
                    APPEND BLANK
                    REPLACE mes  ;
                            WITH  ;
                            MONTH(w_fecha)
                    REPLACE codemi  ;
                            WITH  ;
                            w_codemi
                    REPLACE cantidad  ;
                            WITH  ;
                            can(1)
                    REPLACE repara  ;
                            WITH  ;
                            can(2)
                    REPLACE domici  ;
                            WITH  ;
                            can(3)
                    REPLACE domrep  ;
                            WITH  ;
                            can(4)
                    REPLACE domins  ;
                            WITH  ;
                            can(5)
                    SELECT iorep
                    w_codemi = codemi
                    STORE 0 TO  ;
                          can( 1),  ;
                          can( 2),  ;
                          can( 3),  ;
                          can( 4),  ;
                          can(  ;
                          5)
               ENDIF
               can( 1) = can(1) +  ;
                  1
               SELECT st_isrep
               SEEK iorep.numsol
               IF st_isrep.coddes =  ;
                  'R'
                    can( 2) =  ;
                       can(2) +  ;
                       1
               ELSE
                    can( 3) =  ;
                       can(3) +  ;
                       1
                    w_fla = .F.
                    SELECT st_sicli
                    SEEK iorep.numsol
                    IF FOUND()
                         SCAN WHILE  ;
                              numdoc =  ;
                              iorep.numsol  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF SUBSTR(codsin,  ;
                                 2,  ;
                                 3) =  ;
                                 '570'
                                   w_fla = .T.
                              ENDIF
                         ENDSCAN
                    ENDIF
                    IF w_fla
                         can( 5) =  ;
                            can(5) +  ;
                            1
                    ELSE
                         can( 4) =  ;
                            can(4) +  ;
                            1
                    ENDIF
               ENDIF
               SELECT iorep
               SKIP
          ENDDO
          SELECT b_carsa
          APPEND BLANK
          REPLACE mes WITH  ;
                  MONTH(w_fecha)
          REPLACE codemi WITH  ;
                  w_codemi
          REPLACE cantidad WITH  ;
                  can(1)
          REPLACE repara WITH  ;
                  can(2)
          REPLACE domici WITH  ;
                  can(3)
          REPLACE domrep WITH  ;
                  can(4)
          REPLACE domins WITH  ;
                  can(5)
          SELECT iorep
     ENDDO
ENDIF
SELECT b_carsa
BROWSE
RETURN
*
PROCEDURE recupe
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ALIAS  ;
    st_isrep ORDER codigo
RECALL ALL
USE SHARED st_iorep ALIAS  ;
    st_iorep ORDER ord_fecdoc
RECALL ALL
USE SHARED st_ispre ALIAS  ;
    st_ispre ORDER pre_numord
RECALL ALL
USE SHARED st_idpre ALIAS  ;
    st_idpre ORDER codigo
RECALL ALL
USE SHARED st_iprep ALIAS  ;
    st_iprep ORDER rep_numord
RECALL ALL
USE SHARED st_idped ALIAS  ;
    st_idped ORDER codigo
RECALL ALL
USE SHARED st_mvord ALIAS  ;
    st_mvord ORDER estado
RECALL ALL
USE SHARED st_iredo ALIAS  ;
    st_iredo ORDER codigo
RECALL ALL
USE SHARED st_estad ALIAS  ;
    st_estad ORDER est_numord
RECALL ALL
USE SHARED gc_hve00 ALIAS  ;
    gc_hve00 ORDER nrdore
RECALL ALL
USE SHARED gc_dve00 ALIAS  ;
    gc_dve00 ORDER codigo
RECALL ALL
USE SHARED st_iscic ALIAS  ;
    st_iscic ORDER numord
RECALL ALL
USE SHARED st_movca ALIAS  ;
    st_movca ORDER codigo
RECALL ALL
USE SHARED st_movso ALIAS  ;
    st_movso ORDER codigo
RECALL ALL
USE SHARED st_sicli ALIAS  ;
    st_sicli ORDER codigo
RECALL ALL
USE SHARED st_users ALIAS  ;
    st_users ORDER numsol
RECALL ALL
USE SHARED st_iclpr ALIAS  ;
    st_iclpr ORDER codigo
RECALL ALL
USE SHARED gc_cli00
RECALL ALL
rge_monbas = 'DOL '
USE SHARED gc_cmv00
RECALL ALL
USE SHARED gc_dco00
RECALL ALL
USE SHARED gc_dip00
RECALL ALL
USE SHARED gc_dre00
RECALL ALL
USE SHARED gc_dve00
RECALL ALL
USE SHARED gc_dpv00
RECALL ALL
USE SHARED gc_est00
RECALL ALL
USE SHARED gc_gas00
RECALL ALL
USE SHARED gc_hco00
RECALL ALL
USE SHARED gc_hip00
RECALL ALL
USE SHARED gc_hre00
RECALL ALL
USE SHARED gc_hve00
RECALL ALL
USE SHARED gc_hpv00
RECALL ALL
USE SHARED gc_kar00
RECALL ALL
USE SHARED gc_nfa00
RECALL ALL
USE SHARED gc_ord00
RECALL ALL
CLOSE DATABASES
RETURN
*
PROCEDURE verifi
SET SYSMENU ON
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER  ;
    sol_codent
SELECT 2
USE SHARED st_iclpr ORDER codigo
DIMENSION cli( 12)
SEEK 'C'
SCAN WHILE indent = 'C' .AND.   ;
     .NOT. EOF()
     @ 20, 40 SAY indent
     @ 21, 40 SAY codent
     SCATTER TO cli
     SELECT st_isrep
     SEEK cli(2)
     w_eli = .F.
     IF FOUND()
          SCAN WHILE codent =  ;
               cli(2) .AND.   ;
               .NOT. EOF()
               w_fecemi = fecemi
          ENDSCAN
          IF w_fecemi < DATE() -  ;
             730
               w_eli = .T.
          ENDIF
     ELSE
          w_eli = .T.
     ENDIF
     IF w_eli
          SELECT st_iclpr
          DO rbloquea
          DELETE
          UNLOCK
     ELSE
          SELECT st_iclpr
     ENDIF
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE liquida
SET SYSMENU ON
CLEAR
CLOSE DATABASES
SELECT 1
USE SHARED st_isrep ORDER  ;
    sol_fchemi
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 3
USE SHARED st_ispre ORDER  ;
    pre_numord
SELECT 4
USE SHARED st_idpre ORDER codigo
SELECT 5
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT 6
USE SHARED st_idped ORDER codigo
SELECT 7
USE SHARED st_mvord ORDER estado
SELECT 8
USE SHARED st_iredo ORDER codigo
SELECT 9
USE SHARED st_estad ORDER  ;
    est_numord
SELECT st_isrep
GOTO TOP
w_fecha = CTOD('01/01/94')
SET NEAR ON
SEEK dtoc2(w_fecha)
SET NEAR OFF
BROWSE
SCAN WHILE YEAR(fecemi) = 1994  ;
     .AND.  .NOT. EOF()
     w_numsol = numdoc
     w_fecha = fecemi
     @ 10, 20 SAY w_numsol
     @ 10, 40 SAY w_fecha PICTURE  ;
       '99/99/99'
     DO rbloquea
     DELETE
     UNLOCK
     SKIP -1
     SELECT st_iorep
     SEEK w_numsol
     IF FOUND()
          w_numord = numdoc
          DO rbloquea
          DELETE
          UNLOCK
          SELECT st_ispre
          SEEK w_numord
          IF FOUND()
               w_numpre = numdoc
               DO rbloquea
               DELETE
               UNLOCK
               SELECT st_idpre
               SEEK w_numpre
               IF FOUND()
                    SCAN WHILE  ;
                         numdoc =  ;
                         w_numpre  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         DO rbloquea
                         DELETE
                         UNLOCK
                         SKIP -1
                    ENDSCAN
               ENDIF
          ENDIF
          SELECT st_iprep
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    w_numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    w_numped = numdoc
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SKIP -1
                    SELECT st_idped
                    SEEK w_numped
                    IF FOUND()
                         SCAN WHILE  ;
                              numdoc =  ;
                              w_numped  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              DO rbloquea
                              DELETE
                              UNLOCK
                              SKIP - ;
                               1
                         ENDSCAN
                    ENDIF
                    SELECT st_iprep
               ENDSCAN
          ENDIF
          SELECT st_mvord
          SEEK w_numord
          IF FOUND()
               SCAN WHILE orden =  ;
                    w_numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SKIP -1
               ENDSCAN
          ENDIF
          SELECT st_iredo
          SEEK 'ORD ' + w_numord
          IF FOUND()
               SCAN WHILE (indodo =  ;
                    'ORD ' .AND.  ;
                    numodo =  ;
                    w_numsol)  ;
                    .AND.  .NOT.  ;
                    EOF()
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SKIP -1
               ENDSCAN
          ENDIF
          SELECT st_estad
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    VAL(w_numord)  ;
                    .AND.  .NOT.  ;
                    EOF()
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SKIP -1
               ENDSCAN
          ENDIF
     ENDIF
     SELECT st_iredo
     SEEK 'SSE ' + w_numsol
     IF FOUND()
          SCAN WHILE (indodo =  ;
               'SSE ' .AND.  ;
               numodo = w_numsol)  ;
               .AND.  .NOT.  ;
               EOF()
               DO rbloquea
               DELETE
               UNLOCK
               SKIP -1
          ENDSCAN
     ENDIF
     SELECT st_isrep
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE act_ord
CLEAR
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER CODIGO
SELECT 2
USE SHARED IOREP ORDER CODIGO
GOTO TOP
SCAN WHILE  .NOT. EOF()
     @ 10, 20 SAY numdoc
     IF  .NOT. EMPTY(observ)
          SELECT st_iorep
          SEEK iorep.numdoc
          IF FOUND() .AND.  ;
             EMPTY(observ)
               DO rbloquea
               REPLACE observ  ;
                       WITH  ;
                       iorep.observ
               UNLOCK
          ENDIF
          SELECT iorep
     ENDIF
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE act_sol
SET SYSMENU ON
CLEAR
CLOSE DATABASES
SELECT 1
USE SHARED ST_ISREP ORDER CODIGO
SELECT 2
USE SHARED ISREP ORDER CODIGO
GOTO TOP
SCAN WHILE  .NOT. EOF()
     @ 10, 20 SAY numdoc
     IF  .NOT. EMPTY(observ) .OR.   ;
         .NOT. EMPTY(desace)
          BROWSE
          SELECT st_isrep
          SEEK isrep.numdoc
          IF FOUND()
               BROWSE
               IF EMPTY(observ)
                    DO rbloquea
                    UNLOCK
               ENDIF
               IF EMPTY(desace)
                    DO rbloquea
                    UNLOCK
               ENDIF
          ENDIF
          SELECT isrep
     ENDIF
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE acmemo
SET SYSMENU ON
CLEAR
CLOSE DATABASES
SELECT 1
USE SHARED ST_ISPRE ORDER  ;
    PRE_NUMORD
SELECT 2
USE SHARED ST_IOREP ORDER CODIGO
GOTO TOP
SCAN WHILE  .NOT. EOF()
     @ 10, 20 SAY numdoc
     IF EMPTY(observ) .AND.  ;
        auxest = '003 '
          SELECT st_ispre
          SEEK st_iorep.numdoc
          IF FOUND()
               SELECT st_iorep
               DO rbloquea
               REPLACE observ  ;
                       WITH  ;
                       st_ispre.observ
               UNLOCK
          ENDIF
          SELECT st_iorep
     ENDIF
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE xxxxxx
CLEAR
CLOSE DATABASES
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 1
USE SHARED st_iorep ORDER fecha
SELECT 2
USE SHARED st_mvord ORDER estado
SELECT st_iorep
SEEK DTOS(DATE() - 1)
SCAN WHILE date = DATE() - 1  ;
     .AND.  .NOT. EOF()
     @ 10, 20 SAY numdoc
     SELECT st_mvord
     SEEK st_iorep.numdoc +  ;
          st_iorep.auxest
     IF  .NOT. FOUND()
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH  ;
                  st_iorep.fecest
          REPLACE hora WITH  ;
                  st_iorep.time
          REPLACE orden WITH  ;
                  st_iorep.numdoc
          REPLACE tecnico WITH  ;
                  st_iorep.codtec
          REPLACE estado WITH  ;
                  st_iorep.auxest
          REPLACE inftec WITH  ;
                  st_iorep.observ
          REPLACE user WITH  ;
                  st_iorep.user
          REPLACE date WITH  ;
                  st_iorep.date
          REPLACE time WITH  ;
                  st_iorep.time
          UNLOCK
          SELECT ge_tab0
          SEEK 'ESOR' +  ;
               st_mvord.estado
          IF FOUND()
               SELECT st_mvord
               DO rbloquea
               REPLACE destado  ;
                       WITH  ;
                       ge_tab0.tab_destab
               UNLOCK
          ENDIF
          SELECT st_mvord
          BROWSE
     ENDIF
     SELECT st_iorep
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE act_arc
SELECT 1
USE SHARED st_isrep ORDER codigo
SELECT 2
USE SHARED st_iorep ORDER  ;
    ord_numsol
SELECT 3
USE SHARED st_ispre ORDER codigo
SELECT 4
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT st_isrep
GOTO TOP
SCAN WHILE  .NOT. EOF()
     w_numsol = numdoc
     SELECT st_iorep
     SEEK w_numsol
     IF FOUND()
          w_indest = indest
          w_numpre = numpre
          w_numord = numdoc
          SELECT st_ispre
          SEEK w_numpre
          IF FOUND()
               IF w_indest = 'B'  ;
                  .OR. w_indest =  ;
                  'F' .OR.  ;
                  w_indest = 'C'
                    DO rbloquea
                    REPLACE indest  ;
                            WITH  ;
                            'C'
                    UNLOCK
               ENDIF
               IF w_indest = 'N'
                    DO rbloquea
                    REPLACE indest  ;
                            WITH  ;
                            'N'
                    UNLOCK
               ENDIF
          ENDIF
          SELECT st_iprep
          SEEK w_numord
          SCAN WHILE w_numord =  ;
               numord .AND.   ;
               .NOT. EOF()
               DO rbloquea
               indest with  ;
                      w_indest
               UNLOCK
          ENDSCAN
          SELECT st_isrep
          IF w_indest = 'F' .OR.  ;
             w_indest = 'B' .OR.  ;
             w_indest = 'C'
               DO rbloquea
               REPLACE indest  ;
                       WITH  ;
                       w_indest
               UNLOCK
          ENDIF
     ENDIF
     SELECT st_isrep
ENDSCAN
*
PROCEDURE actua
SELECT 1
USE SHARED ST_ISPRE ORDER CODIGO
SELECT 2
USE SHARED ST_IDPRE ORDER 1
INDEX ON numdoc TO num1
SELECT st_ispre
GOTO TOP
v = 1
SCAN WHILE  .NOT. EOF()
     @ 10, 20 SAY v
     SELECT st_idpre
     SEEK st_ispre.numdoc
     IF FOUND()
          SCAN WHILE numdoc =  ;
               st_ispre.numdoc  ;
               .AND.  .NOT.  ;
               EOF()
               DO rbloquea
               REPLACE numord  ;
                       WITH  ;
                       st_ispre.numord
               UNLOCK
          ENDSCAN
     ENDIF
     ? numdoc, numord
     SELECT st_ispre
     v = v + 1
ENDSCAN
CLOSE DATABASES
RETURN
*
PROCEDURE rep_domi
CLEAR
SELECT 1
USE SHARED ge_tab0 ORDER codigo
CREATE CURSOR zonas (zona C (4),  ;
       codigo C (4), distri C  ;
       (30))
SELECT ge_tab0
SEEK 'DIST'
BROWSE
SCAN WHILE tab_codpre = 'DIST'  ;
     .AND.  .NOT. EOF()
     @ 10, 10 SAY tab_destab
     SELECT zonas
     APPEND BLANK
     REPLACE zona WITH  ;
             ge_tab0.tab_parame
     REPLACE codigo WITH  ;
             ge_tab0.tab_codtab
     REPLACE distri WITH  ;
             ge_tab0.tab_destab
     SELECT ge_tab0
ENDSCAN
SELECT zonas
INDEX ON zona TO zona1
BROWSE
MODIFY REPORT ROCIO
REPORT FORMAT ROCIO TO PRINTER
CLOSE DATABASES
ERASE zona1.idx
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
