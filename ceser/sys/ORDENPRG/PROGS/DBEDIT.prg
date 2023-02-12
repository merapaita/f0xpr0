*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER l1, c1, l2, c2, arrcam,  ;
          fun, nom
IF TYPE('arrcam[1]') = 'C'
     cam = arrcam(1)
     @ l1, c1 SAY nom(1)
     @ l1 + 1, c1 SAY  ;
       REPLICATE(CHR(196),  ;
       LEN(nom(1)))
     l1 = l1 + 2
     d_lin = l2 - l1 + 1
ELSE
     cam = arrcam
     d_lin = l2 - l1 + 1
ENDIF
d_rec = RECNO()
d_des = 0
d_pag = 1
DO d_limpia
DO d_listar
DO d_negra
d_ppal = .T.
DO WHILE d_ppal
     d_inkey = INKEY(0)
     DO CASE
          CASE d_inkey = 5 .AND.  ;
               RECNO() > 1
               DO d_blanca
               IF d_des > 0
                    SKIP -1
                    d_des = d_des -  ;
                            1
                    DO d_negra
               ELSE
                    SCROLL l1, c1,  ;
                           l2, c2, - ;
                           1
                    SKIP -1
                    d_rec = RECNO()
                    DO d_negra
               ENDIF
          CASE d_inkey = 24 .AND.  ;
               RECNO() <  ;
               RECCOUNT()
               DO d_blanca
               IF d_des < (d_lin -  ;
                  1)
                    SKIP 1
                    d_des = d_des +  ;
                            1
                    DO d_negra
               ELSE
                    SCROLL l1, c1,  ;
                           l2, c2,  ;
                           1
                    SKIP 1
                    d_rec = d_rec +  ;
                            1
                    DO d_negra
               ENDIF
          CASE d_inkey = 18 .AND.  ;
               d_rec > 1
               DO d_limpia
               SKIP -d_lin
               d_rec = RECNO()
               d_des = 0
               DO d_listar
               DO d_negra
          CASE d_inkey = 3 .AND.  ;
               d_rec + d_lin - 1 <  ;
               RECCOUNT()
               DO d_limpia
               SKIP d_lin
               IF EOF()
                    SKIP -1
               ENDIF
               d_rec = RECNO()
               d_des = 0
               DO d_listar
               DO d_negra
          CASE d_inkey = 1 .AND.  ;
               d_rec > 1
               DO d_limpia
               GOTO TOP
               d_rec = RECNO()
               d_des = 0
               DO d_listar
               DO d_negra
          CASE d_inkey = 6 .AND.  ;
               d_rec + d_lin - 1 <  ;
               RECCOUNT()
               DO d_limpia
               GOTO BOTTOM
               d_rec = RECNO()
               d_des = 0
               DO d_listar
               DO d_negra
          CASE d_inkey == 27  ;
               .AND. LEN(fun) =  ;
               0
               d_ppal = .F.
     ENDCASE
     IF LEN(fun) > 0
          d_ret =  &fun
          IF d_ret == 0
               d_ppal = .F.
          ENDIF
          IF d_ret == 2
               GOTO d_rec
               DO d_listar
               SKIP d_des
               DO d_negra
          ENDIF
     ENDIF
ENDDO
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
