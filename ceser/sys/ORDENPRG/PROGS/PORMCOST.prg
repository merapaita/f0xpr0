*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_archiv
DIMENSION fecha( 1), docum( 1),  ;
          valor( 1)
w_tipca2 = w_tipcam
SELECT gc_hve00
STORE 0 TO s_toacta, i, s_cosfle,  ;
      i2
SEEK w_numsol
SCAN WHILE  ;
     ALLTRIM(gc_hve00.hve_nrdore) =  ;
     ALLTRIM(w_numsol) .AND.   ;
     .NOT. EOF()
     IF hve_estdoc <> 'A' .AND.  ;
        hve_codmov = 'PCTA'
          i = i + 1
          DIMENSION fecha( i),  ;
                    docum( i),  ;
                    valor( i)
          fecha( i) =  ;
               gc_hve00.hve_fecdoc
          docum( i) =  ;
               gc_hve00.hve_nrodoc
          valor( i) =  ;
               gc_hve00.hve_solgen
          s_toacta = s_toacta +  ;
                     hve_solgen
          IF hve_codcta = '005 '  ;
             .AND. w_flete = 0
               w_flete = w_flete +  ;
                         hve_totvta
               s_cosfle = s_cosfle +  ;
                          hve_solgen
          ENDIF
     ENDIF
ENDSCAN
IF w_indest <> 'C' .AND. w_indest <>  ;
   'B' .AND. w_indest <> 'F'
     SELECT st_iorep
     SEEK w_numord
     w_cosmob = cosmob
     IF w_cosmob = 0
          SELECT 20
          USE SHARED st_imode  ;
              ORDER codigo
          SEEK w_codmar +  ;
               w_codmod
          w_codart = codcla
          SELECT 20
          USE st_mobra ORDER  ;
              codigo
          SEEK w_codmar +  ;
               w_codart
          IF FOUND()
               IF w_indori =  ;
                  'GARA' .OR.  ;
                  w_indori =  ;
                  'PVEN'
                    w_cosmob = mo_monmog
               ELSE
                    IF w_indori =  ;
                       'FGAR'
                         w_cosmob =  ;
                          mo_monmof
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
     IF w_flete > 0 .AND.  ;
        s_cosfle = 0
          s_fle = ROUND(ROUND(w_flete *  ;
                  w_tipca2, 2) *  ;
                  (1 + w_facigv),  ;
                  2)
          s_cosfle = s_fle
     ENDIF
ELSE
     SEEK w_numord
     IF FOUND()
          STORE 0 TO i2
          DIMENSION arraytc( 1),  ;
                    fechac( 1),  ;
                    documc( 1),  ;
                    valorc( 1)
          SCAN WHILE  ;
               ALLTRIM(gc_hve00.hve_nrdore) =  ;
               ALLTRIM(w_numord)  ;
               .AND.  .NOT.  ;
               EOF()
               IF hve_estdoc <>  ;
                  'A' .AND.  ;
                  hve_codmov <>  ;
                  'PCTA'
                    i2 = i2 + 1
                    DIMENSION fechac(  ;
                              i2),  ;
                              documc(  ;
                              i2),  ;
                              valorc(  ;
                              i2)
                    fechac( i2) =  ;
                          gc_hve00.hve_fecdoc
                    documc( i2) =  ;
                          gc_hve00.hve_nrodoc
                    valorc( i2) =  ;
                          gc_hve00.hve_mtocan
                    s_toacta = s_toacta +  ;
                               hve_mtocan
                    s_descue = gc_hve00.hve_soldes
                    w_tipca2 = gc_hve00.hve_tipcam
                    s_cosfle = gc_hve00.hve_solfle
                    w_flete = gc_hve00.hve_flete
               ENDIF
          ENDSCAN
     ENDIF
     IF w_indest = 'C'
          w_fla = 0
          w_fecha = DATE()
          SELECT 20
          USE SHARED st_mvord  ;
              ORDER estado
          SEEK w_numord + '025'
          IF FOUND()
               w_fla = 1
               w_fecha = dia
          ELSE
               SEEK w_numord +  ;
                    '024'
               IF FOUND()
                    w_fla = 1
                    w_fecha = dia
               ELSE
                    SEEK w_numord +  ;
                         '027'
                    IF FOUND()
                         w_fla = 1
                    ELSE
                         SEEK w_numord +  ;
                              '026'
                         IF FOUND()
                              w_fla =  ;
                               1
                         ELSE
                              SEEK  ;
                               w_numord +  ;
                               '021'
                              IF FOUND()
                                   w_fla = 1
                              ENDIF
                         ENDIF
                    ENDIF
                    IF w_fla = 1  ;
                       .AND.  ;
                       SUBSTR(w_indori,  ;
                       1, 1) =  ;
                       'F'
                         w_fecha =  ;
                          dia
                    ENDIF
               ENDIF
          ENDIF
          IF w_fla = 1 .AND.  ;
             w_fecha <> DATE()
               SELECT 20
               USE SHARED  ;
                   gc_cmv00 ORDER  ;
                   cmv_feinmo
               w_tipca2 = ootc2(w_fecha, ;
                          rge_monbas, ;
                          'DOL ', ;
                          '2')
          ENDIF
     ENDIF
     IF i2 = 0
          s_cosfle = ROUND(ROUND(w_flete *  ;
                     w_tipca2, 2) *  ;
                     (1 +  ;
                     w_facigv),  ;
                     2)
          s_descue = ROUND(ROUND(w_totdes *  ;
                     (1 +  ;
                     w_facigv),  ;
                     2) *  ;
                     w_tipca2,  ;
                     2)
     ENDIF
ENDIF
DO llena_rep
s_cosmob = ROUND(ROUND(w_cosmob *  ;
           w_tipca2, 2) * (1 +  ;
           w_facigv), 2)
IF i2 = 0 .AND. (w_indest <> 'F'  ;
   .AND. w_indest <> 'B')
     s_descue = ROUND((s_cosrep *  ;
                w_desrep) / 100,  ;
                2) +  ;
                ROUND((s_cosmob *  ;
                w_desmob) / 100,  ;
                2)
ENDIF
s_total = s_cosrep + s_cosmob +  ;
          s_cosfle
s_totgen = s_total - s_descue
s_totpag = s_totgen - s_toacta
IF i2 = 1
     s_totpag = 0
ELSE
     IF (w_auxest = '025 ' .OR.  ;
        w_auxest = '024 ')
          s_totpag = 0
     ELSE
          IF (w_auxest = '029 '  ;
             .OR. w_auxest =  ;
             '028 ' .OR. w_auxest =  ;
             '023 ' .OR. w_auxest =  ;
             '022 ') .AND.  ;
             SUBSTR(w_indori, 2,  ;
             1) = 'R' .AND.  ;
             w_cosrep = 0
               s_totpag = 0
          ELSE
               IF ((w_auxest =  ;
                  '028 ' .OR.  ;
                  w_auxest =  ;
                  '023 ') .AND.  ;
                  w_indori =  ;
                  'FGAR' .AND.  ;
                  w_cosrep = 0)
                    s_totpag = 0
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE llena_rep
SELECT 20
USE SHARED st_iprep ORDER  ;
    rep_numord
SEEK w_numord
STORE 0 TO w_cosrep, a, s_cosrep
DIMENSION arrayt( 1)
IF FOUND()
     SELECT 21
     USE st_idped ORDER codigo
     SELECT st_iprep
     SCAN WHILE numord = w_numord  ;
          .AND.  .NOT. EOF()
          IF indest <> 'N'
               SELECT st_idped
               SEEK st_iprep.numdoc
               SCAN WHILE numdoc =  ;
                    st_iprep.numdoc  ;
                    .AND. numord =  ;
                    w_numord  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF canpro > 0
                         a = a +  ;
                             1
                         s_cosrep =  ;
                          s_cosrep +  ;
                          ROUND(ROUND(ROUND(valpro *  ;
                          (1 +  ;
                          w_facigv),  ;
                          2) *  ;
                          w_tipcam,  ;
                          2) *  ;
                          canpro,  ;
                          2)
                         w_cosrep =  ;
                          w_cosrep +  ;
                          ROUND((ROUND((valpro *  ;
                          (1 +  ;
                          w_facigv)),  ;
                          2) *  ;
                          canpro),  ;
                          2)
                    ENDIF
               ENDSCAN
               SELECT st_iprep
          ENDIF
     ENDSCAN
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
