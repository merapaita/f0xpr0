*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
ind_prg = PROGRAM()
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 2,  ;
   ' CIERRE PERIODICO '
DEFINE WINDOW venpor FROM 18, 21  ;
       TO 20, 59 IN screen NONE
ppas = .T.
STORE 0 TO c
DO WHILE ppas
     STORE 1 TO w_opc1, w_opc2
     STORE DATE() TO w_fecha1,  ;
           w_fecha2, w_fecemi
     STORE SPACE(20) TO w_ruta,  ;
           w_rufi
     STORE SPACE(8) TO w_numsol,  ;
           w_numord, w_numpre,  ;
           w_numped
     w_ruta = 'Q:\BASTEM\          '
     DO esc_modo WITH 'E'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     @ 04, 08 SAY  ;
       'Guardar Movimiento de :'
     @ 09, 08 SAY  ;
       'Selecci¢n de A¤o      :'
     @ 09, 32 SAY  ;
       'Del:            Al:'
     @ 11, 08 SAY  ;
       'Ruta de Destino       :'
     @ 13, 08 SAY  ;
       'Procesar              :'
     @ 04, 32 GET w_opc1 PICTURE  ;
       '@*RVT Sistema de Inventario;Sistema de Ordenes   ;Maestros             ;Todos                '
     @ 09, 36 GET w_fecha1  ;
       PICTURE '99/99/9999'
     @ 09, 51 GET w_fecha2  ;
       PICTURE '99/99/9999'
     @ 11, 32 GET w_ruta PICTURE  ;
       '@!'
     @ 13, 32 GET w_opc2 PICTURE  ;
       '@*RVT Si;No'
     CLEAR GETS
     @ 04, 32 GET w_opc1 PICTURE  ;
       '@*RVT Sistema de Inventario;Sistema de Ordenes   ;Maestros             ;Todos                '
     READ
     IF LASTKEY() == 27
          ppas = .F.
          LOOP
     ENDIF
     IF w_opc1 = 3
          @ 9, 08 SAY SPACE(55)
     ELSE
          @ 09, 36 GET w_fecha1  ;
            PICTURE '99/99/9999'
          @ 09, 51 GET w_fecha2  ;
            PICTURE '99/99/9999'
     ENDIF
     @ 11, 32 GET w_ruta PICTURE  ;
       '@!' VALID  .NOT.  ;
       EMPTY(w_ruta)
     @ 13, 32 GET w_opc2 PICTURE  ;
       '@*RVT Si;No'
     READ
     IF LASTKEY() == 27
          ppas = .F.
          LOOP
     ELSE
          IF LASTKEY() <> 13 .OR.  ;
             w_opc2 = 2
               ppas = .T.
               LOOP
          ENDIF
     ENDIF
     w_ruta = ALLTRIM(w_ruta)
     IF LASTKEY() = 13 .AND.  ;
        w_opc2 = 1
          DEFINE WINDOW bb FROM  ;
                 17, 21 TO 17, 59  ;
                 NONE
          ACTIVATE WINDOW bb
          CLEAR
          STORE .T. TO w_elor,  ;
                w_elin
          IF w_opc1 = 1 .OR.  ;
             w_opc1 = 4
               SELECT 1
               USE SHARED  ;
                   gc_kar00 ORDER  ;
                   fecdoc
               SET NEAR ON
               SEEK DTOS(w_fecha1)
               SET NEAR OFF
               IF (kar_fecdoc <  ;
                  w_fecha1 .OR.  ;
                  kar_fecdoc >  ;
                  w_fecha2)
                    DO error WITH  ;
                       'No existe Informaci¢n de Inventarios '
                    IF w_opc1 = 1
                         DEACTIVATE  ;
                          WINDOW  ;
                          bb
                         RELEASE WINDOW  ;
                                 bb
                         LOOP
                    ENDIF
                    w_elin = .F.
               ENDIF
          ENDIF
          IF w_opc1 = 2 .OR.  ;
             w_opc1 = 4
               SELECT 1
               USE SHARED  ;
                   st_isrep ALIAS  ;
                   st_isrep ORDER  ;
                   sol_fchemi
               SET NEAR ON
               SEEK dtoc2(w_fecha1)
               SET NEAR OFF
               IF (fecemi <  ;
                  w_fecha1 .OR.  ;
                  fecemi >  ;
                  w_fecha2)
                    DO error WITH  ;
                       'No existe Informaci¢n de Reparaciones '
                    IF w_opc1 = 2
                         DEACTIVATE  ;
                          WINDOW  ;
                          bb
                         RELEASE WINDOW  ;
                                 bb
                         LOOP
                    ENDIF
                    w_elor = .F.
               ENDIF
          ENDIF
          DO CASE
               CASE w_opc1 = 1
                    @ 0, 0 SAY  ;
                      '** Limpiando Archivos de Inventarios **'  ;
                      COLOR N/W* 
                    DO abmat2
                    DO inven
               CASE w_opc1 = 2
                    @ 0, 0 SAY  ;
                      '  ** Limpiando Archivos de Ordenes **  '  ;
                      COLOR N/W* 
                    DO abmat1
                    DO ordenes
               CASE w_opc1 = 3
                    @ 0, 0 SAY  ;
                      '    ** Copiando Archivos Maestros **   '  ;
                      COLOR N/W* 
                    DO maestro
               CASE w_opc1 = 4
                    IF w_elor
                         ACTIVATE  ;
                          WINDOW  ;
                          bb
                         @ 0, 0  ;
                           SAY  ;
                           '  ** Limpiando Archivos de Ordenes **  '  ;
                           COLOR  ;
                           N/W* 
                         DO abmat1
                         DO ordenes
                    ENDIF
                    IF w_elin
                         ACTIVATE  ;
                          WINDOW  ;
                          bb
                         @ 0, 0  ;
                           SAY  ;
                           '** Limpiando Archivos de Inventarios **'  ;
                           COLOR  ;
                           N/W* 
                         DO abmat2
                         DO inven
                    ENDIF
                    ACTIVATE WINDOW  ;
                             bb
                    @ 0, 0 SAY  ;
                      '   ** Copiando Archivos Maestros **    '  ;
                      COLOR N/W* 
                    DO maestro
          ENDCASE
          CLOSE DATABASES
          DEACTIVATE WINDOW  ;
                     venpor
          DEACTIVATE WINDOW bb
          RELEASE WINDOW bb
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL f6
ON KEY LABEL f10
RETURN
*
PROCEDURE ordenes
DO abredbf1
SELECT st_isrep
COUNT FOR fecemi >= w_fecha1  ;
      .AND. fecemi <= w_fecha2 TO  ;
      nreg
SET NEAR ON
SEEK dtoc2(w_fecha1)
SET NEAR OFF
c = 0
SCAN WHILE fecemi >= w_fecha1  ;
     .AND. fecemi <= w_fecha2  ;
     .AND.  .NOT. EOF()
     c = c + 1
     DO porcen WITH c
     SCATTER MEMO TO sol
     w_numsol = numdoc
     w_indori = indori
     w_codmar = codmar
     w_codmod = codmod
     w_numser = numser
     w_flag = 0
     w_sig = .F.
     SELECT st_iorep
     SEEK w_numsol
     IF FOUND()
          w_auxest = ALLTRIM(auxest)
          IF ALLTRIM(codtall) <  ;
             '010'
               DO CASE
                    CASE indori =  ;
                         'FGAR'
                         IF w_auxest =  ;
                            '023'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '024'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '025'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '100'
                              w_flag =  ;
                               1
                         ELSE
                              w_flag =  ;
                               0
                         ENDIF
                    CASE indori =  ;
                         'FREC'  ;
                         .OR.  ;
                         indori =  ;
                         'GREC'  ;
                         .OR.  ;
                         indori =  ;
                         'PREC'
                         IF w_auxest =  ;
                            '022'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '023'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '024'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '025'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '100'
                              w_flag =  ;
                               1
                         ELSE
                              w_flag =  ;
                               0
                         ENDIF
                    CASE indori =  ;
                         'GARA'  ;
                         .OR.  ;
                         indori =  ;
                         'PVEN'
                         IF w_auxest =  ;
                            '024'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '025'  ;
                            .OR.  ;
                            auxest =  ;
                            '100'
                              w_flag =  ;
                               1
                         ELSE
                              w_flag =  ;
                               0
                         ENDIF
               ENDCASE
          ELSE
               DO CASE
                    CASE indori =  ;
                         'FGAR'
                         IF w_auxest =  ;
                            '024'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '025'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '028'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '100'
                              w_flag =  ;
                               1
                         ELSE
                              w_flag =  ;
                               0
                         ENDIF
                    CASE indori =  ;
                         'FREC'  ;
                         .OR.  ;
                         indori =  ;
                         'GREC'  ;
                         .OR.  ;
                         indori =  ;
                         'PREC'
                         IF w_auxest =  ;
                            '024'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '025'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '028'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '029'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '100'
                              w_flag =  ;
                               1
                         ELSE
                              w_flag =  ;
                               0
                         ENDIF
                    CASE indori =  ;
                         'GARA'  ;
                         .OR.  ;
                         indori =  ;
                         'PVEN'
                         IF w_auxest =  ;
                            '024'  ;
                            .OR.  ;
                            w_auxest =  ;
                            '025'  ;
                            .OR.  ;
                            auxest =  ;
                            '100'
                              w_flag =  ;
                               1
                         ELSE
                              w_flag =  ;
                               0
                         ENDIF
               ENDCASE
          ENDIF
          IF w_flag = 1
               w_sig = .T.
          ENDIF
     ELSE
          IF sol(8) = 'C'
               w_sig = .T.
          ENDIF
     ENDIF
     IF w_sig
          SELECT isrep
          SEEK w_numsol
          IF  .NOT. FOUND()
               APPEND BLANK
               GATHER FROM sol  ;
                      MEMO
          ENDIF
          SELECT 1
          USE SHARED st_sicli  ;
              ALIAS st_sicli  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_sicli'
          use &w_rufi order codigo shared;
alias sicli
          SELECT st_sicli
          SEEK w_numsol
          IF FOUND()
               SCAN WHILE numdoc =  ;
                    w_numsol  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER TO  ;
                            scli
                    SELECT sicli
                    APPEND BLANK
                    GATHER FROM  ;
                           scli
                    SELECT st_sicli
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED st_users  ;
              ALIAS st_users  ;
              ORDER numsol
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_users'
          use &w_rufi order numsol shared;
alias users
          SELECT st_users
          SEEK w_numsol
          IF FOUND()
               SCAN WHILE numsol =  ;
                    w_numsol  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER TO  ;
                            usr
                    SELECT users
                    APPEND BLANK
                    GATHER FROM  ;
                           usr
                    SELECT st_users
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED st_iredo  ;
              ALIAS st_iredo  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_iredo'
          use &w_rufi order codigo shared;
alias iredo
          SELECT st_iredo
          SEEK 'SSE ' + w_numsol
          IF FOUND()
               SCAN WHILE (indodo =  ;
                    'SSE ' .AND.  ;
                    numodo =  ;
                    w_numsol)  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER MEMO  ;
                            TO  ;
                            redo
                    SELECT iredo
                    APPEND BLANK
                    GATHER FROM  ;
                           redo  ;
                           MEMO
                    SELECT st_iredo
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED gc_hve00  ;
              ALIAS gc_hve00  ;
              ORDER nrdore
          SELECT 3
          w_rufi = w_ruta +  ;
                   'gc_hve00'
          use &w_rufi order codigo shared;
alias hve00
          SELECT gc_hve00
          SEEK w_numsol
          IF FOUND()
               SCAN WHILE  ;
                    VAL(hve_nrdore) =  ;
                    VAL(w_numsol)  ;
                    .AND.  ;
                    hve_tidore =  ;
                    'SOLI' .AND.   ;
                    .NOT. EOF()
                    SCATTER TO  ;
                            hve
                    SELECT hve00
                    APPEND BLANK
                    GATHER FROM  ;
                           hve
                    SELECT gc_hve00
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT st_isrep
          DO rbloquea
          DELETE
          UNLOCK
          IF w_indori = 'GARA'
               SELECT 1
               USE SHARED  ;
                   st_iseri ALIAS  ;
                   st_iseri ORDER  ;
                   ser_codmar
               SELECT 2
               w_rufi = w_ruta +  ;
                        'st_iseri'
               use &w_rufi order ser_codmar;
shared alias iseri
               SELECT st_iseri
               SEEK w_codmar +  ;
                    w_codmod +  ;
                    w_numser
               IF FOUND()
                    IF fecgar <  ;
                       DATE()
                         SCATTER TO  ;
                                 ser
                         SELECT iseri
                         APPEND BLANK
                         GATHER FROM  ;
                                ser
                         SELECT st_iseri
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     IF w_flag = 1
          SELECT st_iorep
          w_numord = numdoc
          w_numpre = numpre
          SELECT 1
          USE SHARED st_ispre  ;
              ALIAS st_ispre  ;
              ORDER pre_numord
          SELECT 2
          USE SHARED st_idpre  ;
              ALIAS st_idpre  ;
              ORDER codigo
          SELECT 3
          w_rufi = w_ruta +  ;
                   'st_ispre'
          use &w_rufi order codigo shared;
alias ispre
          SELECT 4
          w_rufi = w_ruta +  ;
                   'st_idpre'
          use &w_rufi order codigo shared;
alias idpre
          SELECT st_ispre
          SEEK w_numord
          IF FOUND()
               w_numpre = numdoc
               SCATTER MEMO TO  ;
                       pre
               DO rbloquea
               DELETE
               UNLOCK
               SELECT ispre
               APPEND BLANK
               GATHER FROM pre  ;
                      MEMO
               SELECT st_idpre
               SEEK w_numpre
               IF FOUND()
                    SCAN WHILE  ;
                         numdoc =  ;
                         w_numpre  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         SCATTER MEMO  ;
                                 TO  ;
                                 dpre
                         SELECT idpre
                         APPEND BLANK
                         GATHER FROM  ;
                                dpre  ;
                                MEMO
                         SELECT st_idpre
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDSCAN
               ENDIF
          ENDIF
          SELECT 1
          USE SHARED st_iprep  ;
              ALIAS st_iprep  ;
              ORDER rep_numord
          SELECT 2
          USE SHARED st_idped  ;
              ALIAS st_idped  ;
              ORDER codigo
          SELECT 3
          w_rufi = w_ruta +  ;
                   'st_iprep'
          use &w_rufi order codigo shared;
alias iprep
          SELECT 4
          w_rufi = w_ruta +  ;
                   'st_idped'
          use &w_rufi order codigo shared;
alias idped
          SELECT st_iprep
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    w_numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    w_numped = numdoc
                    SCATTER MEMO  ;
                            TO  ;
                            ped
                    SELECT iprep
                    APPEND BLANK
                    GATHER FROM  ;
                           ped  ;
                           MEMO
                    SELECT st_iprep
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SELECT st_idped
                    SEEK w_numped
                    IF FOUND()
                         SCAN WHILE  ;
                              numdoc =  ;
                              w_numped  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              SCATTER  ;
                               MEMO  ;
                               TO  ;
                               dped
                              SELECT  ;
                               idped
                              APPEND  ;
                               BLANK
                              GATHER  ;
                               FROM  ;
                               dped  ;
                               MEMO
                              SELECT  ;
                               st_idped
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ENDSCAN
                    ENDIF
                    SELECT st_iprep
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED st_mvord  ;
              ALIAS st_mvord  ;
              ORDER estado
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_mvord'
          use &w_rufi order estado shared;
alias mvord 
          SELECT st_mvord
          SEEK w_numord
          IF FOUND()
               SCAN WHILE orden =  ;
                    w_numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER MEMO  ;
                            TO  ;
                            movi
                    SELECT mvord
                    APPEND BLANK
                    GATHER FROM  ;
                           movi  ;
                           MEMO
                    SELECT st_mvord
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED st_iredo  ;
              ALIAS st_iredo  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_iredo'
          use &w_rufi order codigo shared;
alias iredo
          SELECT st_iredo
          SEEK 'ORD ' + w_numord
          IF FOUND()
               SCAN WHILE (indodo =  ;
                    'ORD ' .AND.  ;
                    numodo =  ;
                    w_numord)  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER MEMO  ;
                            TO  ;
                            redo
                    SELECT iredo
                    APPEND BLANK
                    GATHER FROM  ;
                           redo  ;
                           MEMO
                    SELECT st_iredo
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED st_estad  ;
              ALIAS st_estad  ;
              ORDER est_numord
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_estad'
          use &w_rufi order est_numord;
shared alias estad
          SELECT st_estad
          SEEK w_numord
          IF FOUND()
               SCAN WHILE numord =  ;
                    VAL(w_numord)  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER MEMO  ;
                            TO  ;
                            stat
                    SELECT estad
                    APPEND BLANK
                    GATHER FROM  ;
                           stat  ;
                           MEMO
                    SELECT st_estad
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT 1
          USE SHARED st_movca  ;
              ALIAS st_movca  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_movca'
          use &w_rufi order codigo shared;
alias movca
          SELECT st_movca
          SEEK w_numord
          SCAN WHILE numord =  ;
               w_numord .AND.   ;
               .NOT. EOF()
               SCATTER TO mca
               SELECT movca
               APPEND BLANK
               GATHER FROM mca
               SELECT st_movca
               DO rbloquea
               DELETE
               UNLOCK
          ENDSCAN
          SELECT 1
          USE SHARED st_movso  ;
              ALIAS st_movso  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_movso'
          use &w_rufi order codigo shared;
alias movso
          SELECT st_movso
          SEEK w_numord
          SCAN WHILE numord =  ;
               w_numord .AND.   ;
               .NOT. EOF()
               SCATTER TO mso
               SELECT movso
               APPEND BLANK
               GATHER FROM mso
               SELECT st_movso
               DO rbloquea
               DELETE
               UNLOCK
          ENDSCAN
          SELECT 1
          USE SHARED st_iscic  ;
              ALIAS st_iscic  ;
              ORDER numord
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_iscic'
          use &w_rufi order numord shared;
alias iscic
          SELECT st_iscic
          SEEK w_numord
          SCAN WHILE numord =  ;
               w_numord .AND.   ;
               .NOT. EOF()
               SCATTER MEMO TO  ;
                       icli
               SELECT iscic
               APPEND BLANK
               GATHER FROM icli  ;
                      MEMO
               SELECT st_iscic
               DO rbloquea
               DELETE
               UNLOCK
          ENDSCAN
          SELECT 1
          USE SHARED gc_hve00  ;
              ALIAS gc_hve00  ;
              ORDER nrdore
          SELECT 2
          USE SHARED gc_dve00  ;
              ALIAS gc_dve00  ;
              ORDER codigo
          SELECT 3
          w_rufi = w_ruta +  ;
                   'gc_hve00'
          use &w_rufi order nrdore shared;
alias hve00
          SELECT 4
          w_rufi = w_ruta +  ;
                   'gc_dve00'
          use &w_rufi order codigo shared;
alias dve00
          SELECT gc_hve00
          SEEK w_numord
          IF FOUND()
               SCATTER TO hve
               SELECT gc_dve00
               SEEK gc_hve00.hve_tipdoc +  ;
                    gc_hve00.hve_nrodoc
               IF FOUND()
                    SCAN WHILE  ;
                         dve_tipdoc =  ;
                         gc_hve00.hve_tipdoc  ;
                         .AND.  ;
                         dve_nrodoc =  ;
                         gc_hve00.hve_nrodoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         SCATTER TO  ;
                                 dve
                         SELECT dve00
                         APPEND BLANK
                         GATHER FROM  ;
                                dve
                         SELECT gc_dve00
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDSCAN
               ENDIF
               SELECT hve00
               APPEND BLANK
               GATHER FROM hve
               SELECT gc_hve00
               DO rbloquea
               DELETE
               UNLOCK
          ENDIF
          SELECT 1
          USE SHARED st_iclpr  ;
              ALIAS st_iclpr  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'st_iclpr'
          use &w_rufi order codigo shared;
alias iclpr
          SELECT st_iclpr
          SEEK 'C' +  ;
               st_isrep.codent
          IF FOUND()
               SCATTER TO clp
               SELECT iclpr
               SEEK st_iclpr.indent +  ;
                    st_iclpr.codent
               IF  .NOT. FOUND()
                    APPEND BLANK
               ENDIF
               GATHER FROM clp
          ENDIF
          SELECT st_iorep
          SCATTER MEMO TO ord
          SELECT iorep
          APPEND BLANK
          GATHER FROM ord MEMO
          SELECT st_iorep
          DO rbloquea
          DELETE
          UNLOCK
     ENDIF
     SELECT st_isrep
ENDSCAN
SELECT 1
USE SHARED st_iclpr ALIAS  ;
    st_iclpr ORDER codigo
SELECT 2
w_rufi = w_ruta + 'st_iclpr'
use &w_rufi order codigo shared alias;
iclpr
SELECT st_isrep
SET ORDER TO sol_codent
SELECT st_iclpr
COUNT FOR indent = 'C' TO nreg
SEEK 'C'
w_fecemi = CTOD(SPACE(8))
IF FOUND()
     c = 0
     SCAN WHILE indent = 'C'  ;
          .AND.  .NOT. EOF()
          c = c + 1
          DO porcen WITH c
          SCATTER TO clp
          SELECT st_isrep
          SEEK st_iclpr.codent
          w_eli = .F.
          IF FOUND()
               SCAN WHILE codent =  ;
                    st_iclpr.codent  ;
                    .AND.  .NOT.  ;
                    EOF()
                    w_fecemi = fecemi
               ENDSCAN
               IF w_fecemi <  ;
                  DATE() -  ;
                  rge_vicodi
                    w_eli = .T.
               ENDIF
          ELSE
               w_eli = .T.
          ENDIF
          IF w_eli
               SELECT iclpr
               SEEK st_iclpr.indent +  ;
                    st_iclpr.codent
               IF  .NOT. FOUND()
                    APPEND BLANK
               ENDIF
               GATHER FROM clp
               SELECT st_iclpr
               DO rbloquea
               DELETE
               UNLOCK
          ELSE
               SELECT st_iclpr
          ENDIF
     ENDSCAN
ENDIF
DEACTIVATE WINDOW venpor
RETURN
*
PROCEDURE inven
DO abredbf2
SELECT 9
USE SHARED gc_kar00 ALIAS  ;
    gc_kar00 ORDER kar_fecing
SELECT 10
w_rufi = w_ruta + 'gc_kar00.dbf'
use &w_rufi order codigo shared alias;
kar00
SELECT gc_kar00
COUNT FOR kar_fecing >= w_fecha1  ;
      .AND. kar_fecing <=  ;
      w_fecha2 TO nreg
SET NEAR ON
SEEK DTOS(w_fecha1)
SET NEAR OFF
c = 0
SCAN WHILE kar_fecing >= w_fecha1  ;
     .AND. kar_fecing <= w_fecha2  ;
     .AND.  .NOT. EOF()
     c = c + 1
     DO porcen WITH c
     SCATTER TO kar
     SELECT 1
     USE SHARED gc_hip00 ALIAS  ;
         gc_hip00 ORDER codigo
     SELECT 2
     USE SHARED gc_dip00 ALIAS  ;
         gc_dip00 ORDER codigo
     SELECT 3
     USE SHARED gc_gas00 ALIAS  ;
         gc_gas00 ORDER codigo
     SELECT 4
     w_rufi = w_ruta +  ;
              'gc_hip00.dbf'
     use &w_rufi order codigo shared alias;
hip00
     SELECT 5
     w_rufi = w_ruta +  ;
              'gc_dip00.dbf'
     use &w_rufi order codigo shared alias;
dip00
     SELECT 6
     w_rufi = w_ruta +  ;
              'gc_gas00.dbf'
     use &w_rufi order codigo shared alias;
gas00
     SELECT gc_hip00
     SEEK gc_kar00.kar_tipdoc +  ;
          gc_kar00.kar_nrodoc
     IF FOUND()
          SCATTER TO hip
          SELECT hip00
          APPEND BLANK
          GATHER FROM hip
          SELECT gc_hip00
          DO rbloquea
          DELETE
          UNLOCK
          SELECT gc_dip00
          SEEK gc_kar00.kar_tipdoc +  ;
               gc_kar00.kar_nrodoc
          IF FOUND()
               SCAN WHILE  ;
                    dip_tipdoc =  ;
                    gc_kar00.kar_tipdoc  ;
                    .AND.  ;
                    dip_nrodoc =  ;
                    gc_kar00.kar_nrodoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER TO  ;
                            dip
                    SELECT dip00
                    APPEND BLANK
                    GATHER FROM  ;
                           dip
                    SELECT gc_dip00
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
          SELECT gc_gas00
          SEEK gc_kar00.kar_tipdoc +  ;
               gc_kar00.kar_nrodoc
          IF FOUND()
               SCAN WHILE  ;
                    gas_tipdoc =  ;
                    gc_kar00.kar_tipdoc  ;
                    .AND.  ;
                    gas_nrodoc =  ;
                    gc_kar00.kar_nrodoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER TO  ;
                            gas
                    SELECT gas00
                    APPEND BLANK
                    GATHER FROM  ;
                           gas
                    SELECT gc_gas00
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
     ELSE
          IF gc_kar00.kar_tidore <>  ;
             SPACE(4) .AND.  ;
             gc_kar00.kar_nrdore <>  ;
             SPACE(10)
               SELECT 1
               USE SHARED  ;
                   gc_hpv00 ALIAS  ;
                   gc_hpv00 ORDER  ;
                   codigo
               SELECT 2
               USE SHARED  ;
                   gc_dpv00 ALIAS  ;
                   gc_dpv00 ORDER  ;
                   codigo
               SELECT 3
               w_rufi = w_ruta +  ;
                        'gc_hpv00.dbf'
               use &w_rufi order codigo;
shared alias hpv00
               SELECT 4
               w_rufi = w_ruta +  ;
                        'gc_dpv00.dbf'
               use &w_rufi order codigo;
shared alias dpv00
               SELECT gc_hpv00
               SEEK gc_kar00.kar_tidore +  ;
                    gc_kar00.kar_nrdore
               IF FOUND()
                    SCATTER TO  ;
                            hpv
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SELECT hpv00
                    APPEND BLANK
                    GATHER FROM  ;
                           hpv
                    SELECT gc_dpv00
                    SEEK gc_kar00.kar_tidore +  ;
                         gc_kar00.kar_nrdore
                    IF FOUND()
                         SCAN WHILE  ;
                              dpv_tipdoc =  ;
                              gc_kar00.kar_tidore  ;
                              .AND.  ;
                              dpv_nrodoc =  ;
                              gc_kar00.kar_nrdore  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              SCATTER  ;
                               TO  ;
                               dpv
                              SELECT  ;
                               dpv00
                              APPEND  ;
                               BLANK
                              GATHER  ;
                               FROM  ;
                               dpv
                              SELECT  ;
                               gc_dpv00
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ENDSCAN
                    ENDIF
               ENDIF
               SELECT 1
               USE SHARED  ;
                   gc_hve00 ALIAS  ;
                   gc_hve00 ORDER  ;
                   codigo
               SELECT 2
               USE SHARED  ;
                   gc_dve00 ALIAS  ;
                   gc_dve00 ORDER  ;
                   codigo
               SELECT 3
               w_rufi = w_ruta +  ;
                        'gc_hve00.dbf'
               use &w_rufi order codigo;
shared alias hve00
               SELECT 4
               w_rufi = w_ruta +  ;
                        'gc_dve00.dbf'
               use &w_rufi order codigo;
shared alias dve00
               SELECT gc_hve00
               SEEK gc_kar00.kar_tidore +  ;
                    gc_kar00.kar_nrdore
               IF FOUND()
                    SCATTER TO  ;
                            hve
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SELECT hve00
                    APPEND BLANK
                    GATHER FROM  ;
                           hve
               ENDIF
          ENDIF
          SELECT 1
          USE SHARED gc_hve00  ;
              ALIAS gc_hve00  ;
              ORDER codigo
          SELECT 2
          USE SHARED gc_dve00  ;
              ALIAS gc_dve00  ;
              ORDER codigo
          SELECT 3
          w_rufi = w_ruta +  ;
                   'gc_hve00.dbf'
          use &w_rufi order codigo shared;
alias hve00
          SELECT 4
          w_rufi = w_ruta +  ;
                   'gc_dve00.dbf'
          use &w_rufi order codigo shared;
alias dve00
          SELECT gc_hve00
          SEEK gc_kar00.kar_tipdoc +  ;
               gc_kar00.kar_nrodoc
          IF FOUND()
               SCATTER TO hve
               DO rbloquea
               DELETE
               UNLOCK
               SELECT hve00
               APPEND BLANK
               GATHER FROM hve
               SELECT gc_dve00
               SEEK gc_kar00.kar_tipdoc +  ;
                    gc_kar00.kar_nrodoc
               IF FOUND()
                    SCAN WHILE  ;
                         dve_tipdoc =  ;
                         gc_kar00.kar_tipdoc  ;
                         .AND.  ;
                         dve_nrodoc =  ;
                         gc_kar00.kar_nrodoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         SCATTER TO  ;
                                 dve
                         SELECT dve00
                         APPEND BLANK
                         GATHER FROM  ;
                                dve
                         SELECT gc_dve00
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDSCAN
               ENDIF
          ENDIF
          SELECT kar00
          APPEND BLANK
          GATHER FROM kar
          SELECT gc_kar00
          DO rbloquea
          DELETE
          UNLOCK
          SELECT 1
          USE SHARED gc_est00  ;
              ALIAS gc_est00  ;
              ORDER codigo
          SELECT 2
          w_rufi = w_ruta +  ;
                   'gc_est00.dbf'
          use &w_rufi order codigo shared;
alias est00
          SELECT gc_est00
          SEEK gc_kar00.kar_tipdoc +  ;
               gc_kar00.kar_nrodoc
          IF FOUND()
               SCAN WHILE  ;
                    est_tipdoc =  ;
                    gc_kar00.kar_tipdoc  ;
                    .AND.  ;
                    est_nrodoc =  ;
                    gc_kar00.kar_nrodoc  ;
                    .AND.  .NOT.  ;
                    EOF()
                    SCATTER TO  ;
                            est
                    SELECT est00
                    APPEND BLANK
                    GATHER FROM  ;
                           est
                    SELECT gc_est00
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDSCAN
          ENDIF
     ENDIF
     SELECT gc_kar00
ENDSCAN
SELECT 9
USE SHARED gc_hco00 ALIAS  ;
    gc_hco00 ORDER hco_fecdoc
SELECT 10
USE SHARED gc_dco00 ALIAS  ;
    gc_dco00 ORDER codigo
SELECT 11
w_rufi = w_ruta + 'gc_hco00.dbf'
use &w_rufi order hco_fecdoc shared alias;
hco00
SELECT 12
w_rufi = w_ruta + 'gc_dco00.dbf'
use &w_rufi order codigo shared alias;
dco00
SELECT gc_hco00
SET NEAR ON
SEEK DTOS(w_fecha1)
SET NEAR OFF
SCAN WHILE hco_fecdoc >= w_fecha1  ;
     .AND. hco_fecdoc <= w_fecha2  ;
     .AND.  .NOT. EOF()
     SCATTER TO hco
     SELECT gc_dco00
     SEEK gc_hco00.hco_nrodoc
     IF FOUND()
          SCAN WHILE dco_nrodoc =  ;
               gc_hco00.hco_nrodoc  ;
               .AND.  .NOT.  ;
               EOF()
               IF (dco_canbor +  ;
                  dco_cancon) >  ;
                  0
                    SCATTER TO  ;
                            dco
                    SELECT 1
                    USE SHARED  ;
                        gc_nfa00  ;
                        ALIAS  ;
                        gc_nfa00  ;
                        ORDER  ;
                        nfa_numfac
                    SELECT 2
                    w_rufi = w_ruta +  ;
                             'gc_nfa00.dbf'
                    use &w_rufi order;
nfa_numfac shared alias nfa00
                    SELECT gc_nfa00
                    SEEK gc_dco00.dco_numfac +  ;
                         gc_dco00.dco_nrodoc +  ;
                         gc_dco00.dco_codpro
                    IF FOUND()
                         SCATTER TO  ;
                                 nfa
                         SELECT nfa00
                         APPEND BLANK
                         GATHER FROM  ;
                                nfa
                         SELECT gc_nfa00
                         DO rbloquea
                         DELETE
                         UNLOCK
                    ENDIF
                    SELECT 1
                    USE SHARED  ;
                        gc_ord00  ;
                        ALIAS  ;
                        gc_ord00  ;
                        ORDER  ;
                        ord_docprp
                    SELECT 2
                    w_rufi = w_ruta +  ;
                             'gc_ord00.dbf'
                    use &w_rufi order;
ord_docprp shared alias ord00
                    SELECT gc_ord00
                    SEEK gc_dco00.dco_nrodoc +  ;
                         gc_dco00.dco_codprp
                    IF FOUND()
                         SCAN WHILE  ;
                              ord_nrodoc =  ;
                              gc_dco00.dco_nrodoc  ;
                              .AND.  ;
                              ord_codprp =  ;
                              gc_dco00.dco_codprp  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              SCATTER  ;
                               TO  ;
                               ord
                              SELECT  ;
                               ord00
                              APPEND  ;
                               BLANK
                              GATHER  ;
                               FROM  ;
                               ord
                              SELECT  ;
                               gc_ord00
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ENDSCAN
                    ENDIF
                    SELECT dco00
                    APPEND BLANK
                    GATHER FROM  ;
                           dco
                    SELECT gc_dco00
                    DO rbloquea
                    DELETE
                    UNLOCK
               ENDIF
          ENDSCAN
          SELECT hco00
          APPEND BLANK
          GATHER FROM hco
          SELECT gc_hco00
          DO rbloquea
          DELETE
          UNLOCK
     ENDIF
ENDSCAN
SELECT 1
USE SHARED gc_hve00 ALIAS  ;
    gc_hve00 ORDER hve_ticofe
SELECT 2
USE SHARED gc_cli00 ALIAS  ;
    gc_cli00 ORDER cli_feccre
SELECT 3
w_rufi = w_ruta + 'gc_cli00.dbf'
use &w_rufi order codigo shared alias;
cli00
SELECT gc_cli00
SET NEAR ON
SEEK DTOS(w_fecha1)
SET NEAR OFF
SCAN WHILE cli_feccre >= w_fecha1  ;
     .AND. cli_feccre <= w_fecha2  ;
     .AND.  .NOT. EOF()
     IF cli_tpper = 'C' .OR.  ;
        cli_tpper = 'P'
          SCATTER TO cli
          SELECT gc_hve00
          SEEK gc_cli00.cli_tpper +  ;
               gc_cli00.cli_codigo
          IF FOUND()
               SCAN WHILE  ;
                    ALLTRIM(hve_tipent) =  ;
                    gc_cli00.cli_tpper  ;
                    .AND.  ;
                    hve_codent =  ;
                    gc_cli00.cli_codigo  ;
                    .AND.  .NOT.  ;
                    EOF()
                    w_fecemi = hve_fecdoc
               ENDSCAN
               IF w_fecemi <  ;
                  DATE() -  ;
                  rge_vicodi
                    w_eli = .T.
               ENDIF
          ELSE
               w_eli = .T.
          ENDIF
          IF w_eli
               SELECT cli00
               SEEK gc_cli00.cli_tpper +  ;
                    gc_cli00.cli_codigo
               IF  .NOT. FOUND()
                    APPEND BLANK
               ENDIF
               GATHER FROM cli
               SELECT gc_cli00
               DO rbloquea
               DELETE
               UNLOCK
          ELSE
               SELECT gc_cli00
          ENDIF
     ENDIF
ENDSCAN
SELECT 1
USE SHARED gc_cmv00 ALIAS  ;
    gc_cmv00 ORDER cmv_feinmo
SELECT 2
w_rufi = w_ruta + 'gc_cmv00.dbf'
use &w_rufi order codigo shared alias;
cmv00
SELECT gc_cmv00
SET NEAR ON
SEEK DTOS(w_fecha1)
SET NEAR OFF
SCAN WHILE cmv_fechac >= w_fecha1  ;
     .AND. cmv_fechac <= w_fecha2  ;
     .AND.  .NOT. EOF()
     SCATTER TO cmv
     SELECT cmv00
     APPEND BLANK
     GATHER FROM cmv
     SELECT gc_cmv00
     DO rbloquea
     DELETE
     UNLOCK
ENDSCAN
DEACTIVATE WINDOW venpor
RETURN
*
PROCEDURE maestro
SELECT 1
USE SHARED st_asiem ORDER asi_cod
w_rufi = w_ruta + 'st_asiem.dbf'
copy to &w_rufi with cdx
USE SHARED st_imode ORDER codigo
w_rufi = w_ruta + 'st_imode.dbf'
copy to &w_rufi with cdx
USE SHARED st_iparg
w_rufi = w_ruta + 'st_iparg.dbf'
copy to &w_rufi with cdx
USE SHARED st_itecn ORDER codigo
w_rufi = w_ruta + 'st_itecn.dbf'
copy to &w_rufi with cdx
USE SHARED st_mobra ORDER codigo
w_rufi = w_ruta + 'st_mobra.dbf'
copy to &w_rufi with cdx
USE SHARED st_sint ORDER codigo
w_rufi = w_ruta + 'st_sint.dbf'
copy to &w_rufi with cdx
USE SHARED st_inv00 ORDER fecha
w_rufi = w_ruta + 'st_inv00.dbf'
copy to &w_rufi with cdx
USE SHARED campofox
w_rufi = w_ruta + 'campofox.dbf'
copy to &w_rufi with cdx
USE SHARED gc_alm00 ORDER codigo
w_rufi = w_ruta + 'gc_alm00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_din00 ORDER codigo
w_rufi = w_ruta + 'gc_din00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_dlp00 ORDER codigo
w_rufi = w_ruta + 'gc_dlp00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_hlp00 ORDER codigo
w_rufi = w_ruta + 'gc_hlp00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_eti00
w_rufi = w_ruta + 'gc_eti00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_imp00
w_rufi = w_ruta + 'gc_imp00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_inv00 ORDER codigo
w_rufi = w_ruta + 'gc_inv00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_nve00 ORDER codigo
w_rufi = w_ruta + 'gc_nve00.dbf'
copy struct to &w_rufi with cdx
USE SHARED gc_modul
w_rufi = w_ruta + 'gc_modul.dbf'
copy to &w_rufi with cdx
USE SHARED gc_mov00 ORDER codigo
w_rufi = w_ruta + 'gc_mov00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_par00
w_rufi = w_ruta + 'gc_par00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_pro00 ORDER codigo
w_rufi = w_ruta + 'gc_pro00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_uni00 ORDER codigo
w_rufi = w_ruta + 'gc_uni00.dbf'
copy to &w_rufi with cdx
USE SHARED gc_vnd00 ORDER codigo
w_rufi = w_ruta + 'gc_vnd00.dbf'
copy to &w_rufi with cdx
USE SHARED ge_tab0 ORDER codigo
w_rufi = w_ruta + 'ge_tab0.dbf'
copy to &w_rufi with cdx
USE SHARED gt_carsa ORDER codigo
w_rufi = w_ruta + 'gt_carsa.dbf'
copy to &w_rufi with cdx
USE SHARED password ORDER codigo
w_rufi = w_ruta + 'password.dbf'
copy to &w_rufi with cdx
RETURN
*
PROCEDURE abredbf1
CLOSE DATABASES
w_rufi = w_ruta + 'st_iorep.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_iorep
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_isrep.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_isrep
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_ispre.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_ispre
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_idpre.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_idpre
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_iprep.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_iprep
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_idped.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_idped
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_mvord.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_mvord
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_sicli.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_sicli
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_movca.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_movca
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_movso.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_movso
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_estad.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_estad
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_iredo.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_iredo
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_iclpr.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_iclpr
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_iscic.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_iscic
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_users.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_users
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_hve00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_hve00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_dve00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_dve00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'st_iseri.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED st_iseri
     copy struct to &w_rufi with cdx
ENDIF
SELECT 5
USE SHARED st_isrep ALIAS  ;
    st_isrep ORDER sol_fchemi
SELECT 6
USE SHARED st_iorep ALIAS  ;
    st_iorep ORDER ord_numsol
SELECT 7
w_rufi = w_ruta + 'st_isrep.dbf'
use &w_rufi order codigo shared;
 alias isrep
SELECT 8
w_rufi = w_ruta + 'st_iorep.dbf'
use &w_rufi order codigo shared alias;
iorep
RETURN
*
PROCEDURE abredbf2
CLOSE DATABASES
w_rufi = w_ruta + 'gc_cli00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_cli00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_cmv00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_cmv00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_dco00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_dco00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_dip00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_dip00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_dre00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_dre00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_dve00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_dve00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_dpv00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_dpv00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_est00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_est00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_gas00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_gas00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_hco00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_hco00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_hip00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_hip00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_hre00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_hre00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_hve00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_hve00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_hpv00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_hpv00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_kar00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_kar00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_nfa00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_nfa00
     copy struct to &w_rufi with cdx
ENDIF
w_rufi = w_ruta + 'gc_ord00.dbf'
IF  .NOT. FILE(w_rufi)
     SELECT 1
     USE SHARED gc_ord00
     copy struct to &w_rufi with cdx
ENDIF
RETURN
*
PROCEDURE porcen
PARAMETER conta
por = conta / nreg
ACTIVATE WINDOW venpor
@ 0, 0 FILL TO 4, 38 COLOR W+/BG 
@ 0, 0 TO 4, 38 COLOR W+/BG 
@ 1, 2 SAY  ;
  'Porcentaje del Proceso : ' +  ;
  TRANSFORM((por * 100),  ;
  '999.99') + ' %' COLOR W+/BG 
RETURN
*
PROCEDURE abmat1
SELECT 1
USE SHARED st_isrep
w_ncam = FCOUNT()
DIMENSION sol( w_ncam)
USE SHARED st_iorep
w_ncam = FCOUNT()
DIMENSION ord( w_ncam)
USE SHARED st_ispre
w_ncam = FCOUNT()
DIMENSION pre( w_ncam)
USE SHARED st_idpre
w_ncam = FCOUNT()
DIMENSION dpre( w_ncam)
USE SHARED st_iprep
w_ncam = FCOUNT()
DIMENSION ped( w_ncam)
USE SHARED st_idped
w_ncam = FCOUNT()
DIMENSION dped( w_ncam)
USE SHARED st_mvord
w_ncam = FCOUNT()
DIMENSION movi( w_ncam)
USE SHARED st_iredo
w_ncam = FCOUNT()
DIMENSION redo( w_ncam)
USE SHARED st_estad
w_ncam = FCOUNT()
DIMENSION stat( w_ncam)
USE SHARED gc_hve00
w_ncam = FCOUNT()
DIMENSION hve( w_ncam)
USE SHARED st_iclpr
w_ncam = FCOUNT()
DIMENSION clp( w_ncam)
USE SHARED st_movca
w_ncam = FCOUNT()
DIMENSION mca( w_ncam)
USE SHARED st_movso
w_ncam = FCOUNT()
DIMENSION mso( w_ncam)
USE SHARED st_iscic
w_ncam = FCOUNT()
DIMENSION icli( w_ncam)
USE SHARED st_sicli
w_ncam = FCOUNT()
DIMENSION scli( w_ncam)
USE SHARED st_users
w_ncam = FCOUNT()
DIMENSION usr( w_ncam)
USE SHARED st_iseri
w_ncam = FCOUNT()
DIMENSION ser( w_ncam)
RETURN
*
PROCEDURE abmat2
SELECT 1
USE SHARED gc_hve00
w_ncam = FCOUNT()
DIMENSION hve( w_ncam)
USE SHARED gc_kar00
w_ncam = FCOUNT()
DIMENSION kar( w_ncam)
USE SHARED gc_dve00
w_ncam = FCOUNT()
DIMENSION dve( w_ncam)
USE SHARED gc_hpv00
w_ncam = FCOUNT()
DIMENSION hpv( w_ncam)
USE SHARED gc_dpv00
w_ncam = FCOUNT()
DIMENSION dpv( w_ncam)
USE SHARED gc_est00
w_ncam = FCOUNT()
DIMENSION est( w_ncam)
USE SHARED gc_hip00
w_ncam = FCOUNT()
DIMENSION hip( w_ncam)
USE SHARED gc_dip00
w_ncam = FCOUNT()
DIMENSION dip( w_ncam)
USE SHARED gc_gas00
w_ncam = FCOUNT()
DIMENSION gas( w_ncam)
USE SHARED gc_hco00
w_ncam = FCOUNT()
DIMENSION hco( w_ncam)
USE SHARED gc_dco00
w_ncam = FCOUNT()
DIMENSION dco( w_ncam)
USE SHARED gc_nfa00
w_ncam = FCOUNT()
DIMENSION nfa( w_ncam)
USE SHARED gc_ord00
w_ncam = FCOUNT()
DIMENSION ord( w_ncam)
USE SHARED gc_tpe00
w_ncam = FCOUNT()
DIMENSION tpe( w_ncam)
USE SHARED gc_cli00
w_ncam = FCOUNT()
DIMENSION cli( w_ncam)
USE SHARED gc_cmv00
w_ncam = FCOUNT()
DIMENSION cmv( w_ncam)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
