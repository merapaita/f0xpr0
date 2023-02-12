*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
@ 00, 000 SAY SPACE(1)
@ 02, 70 - (LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 02, 108 SAY 'ORIGINAL'
@ 03, 108 SAY tit_eminro
@ 04, 108 SAY tit_fecha
@ 05, 015 SAY tit_client
@ 05, 075 SAY tit_codigo
@ 06, 015 SAY tit_direcc
@ 06, 115 SAY tit_feccom
@ 07, 015 SAY w_desdis
@ 07, 065 SAY tit_codgeo
@ 07, 115 SAY tit_tel1
@ 08, 115 SAY tit_tel2
@ 09, 105 SAY ALLTRIM(w_destia)
@ 10, 001 SAY w_nomtit
@ 10, 031 SAY w_codmod
@ 10, 051 SAY w_desmar
@ 10, 076 SAY tit_numser
@ 10, 105 SAY ALLTRIM(w_gara)
@ 11, 001 SAY tit_tit2
@ 11, 055 SAY tit_tit3
IF  .NOT. EMPTY(w_provee)
     @ 11, 083 SAY 'Proveedor: ' +  ;
       w_provee
ENDIF
IF w_ndoref <> SPACE(8)
     IF SUBSTR(w_indori, 1, 1) =  ;
        'G'
          @ 11, 105 SAY 'S/S ==>'
          @ 11, 115 SAY w_ndosol
     ELSE
          @ 11, 105 SAY 'O/R ==>'
          @ 11, 115 SAY w_ndoref
     ENDIF
ENDIF
ls = 0
FOR lin = 1 TO 10
     IF SUBSTR(mat(lin), 1, 4) <>  ;
        SPACE(4)
          @ 11 + lin, 01 SAY  ;
            mat(lin)
     ENDIF
     @ 11 + lin, 055 SAY  ;
       w_acceso(lin)
ENDFOR
IF sol_totgen <> 0
     @ 22, 095 SAY w_docvta
     @ 22, 100 SAY  ;
       f_ceros(VAL(w_numdoc),10, ;
       1)
     @ 22, 111 SAY sol_totgen  ;
       PICTURE '99,999,999.99'
ENDIF
@ 24, 01 SAY tit_tit4 + SPACE(10) +  ;
  'Recep.: ' + w_nomemp
@ 25, 01 SAY SUBSTR(w_obs(1), 1,  ;
  76)
@ 26, 01 SAY SUBSTR(w_obs(1), 77,  ;
  14) + SUBSTR(w_obs(2), 1, 62)
@ 27, 01 SAY SUBSTR(w_obs(2), 63,  ;
  28) + SUBSTR(w_obs(3), 1, 48)
@ 28, 01 SAY SUBSTR(w_obs(3), 49,  ;
  42)
@ 28, 96 SAY w_fecha
IF (SUBSTR(w_emisor, 1, 1) <> '2'  ;
   .OR. w_coddes <> 'D')
     @ 35, 70 - (LEN(tit_tit5) /  ;
       2) SAY tit_tit5
     @ 35, 108 SAY 'COPIA'
     @ 36, 108 SAY tit_eminro
     @ 37, 108 SAY tit_fecha
     @ 38, 015 SAY tit_client
     @ 38, 075 SAY tit_codigo
     @ 39, 015 SAY tit_direcc
     @ 39, 115 SAY tit_feccom
     @ 40, 015 SAY w_desdis
     @ 40, 065 SAY tit_codgeo
     @ 40, 115 SAY tit_tel1
     @ 41, 115 SAY tit_tel2
     @ 42, 105 SAY  ;
       ALLTRIM(w_destia)
     @ 43, 001 SAY w_nomtit
     @ 43, 031 SAY w_codmod
     @ 43, 051 SAY w_desmar
     @ 43, 076 SAY tit_numser
     @ 43, 105 SAY  ;
       ALLTRIM(w_gara)
     @ 44, 001 SAY tit_tit2
     @ 44, 055 SAY tit_tit3
     IF  .NOT. EMPTY(w_provee)
          @ 44, 083 SAY  ;
            'Proveedor: ' +  ;
            w_provee
     ENDIF
     IF w_ndoref <> SPACE(8)
          IF SUBSTR(w_indori, 1,  ;
             1) = 'G'
               @ 44, 105 SAY  ;
                 'S/S ==>'
               @ 44, 115 SAY  ;
                 w_ndosol
          ELSE
               @ 44, 105 SAY  ;
                 'O/R ==>'
               @ 44, 115 SAY  ;
                 w_ndoref
          ENDIF
     ENDIF
     FOR lin = 1 TO 10
          IF SUBSTR(mat(lin), 1,  ;
             4) <> SPACE(4)
               @ 44 + lin, 01 SAY  ;
                 mat(lin)
          ENDIF
          @ 44 + lin, 55 SAY  ;
            w_acceso(lin)
     ENDFOR
     IF sol_totgen <> 0
          @ 55, 095 SAY w_docvta
          @ 55, 100 SAY  ;
            f_ceros(VAL(w_numdoc), ;
            10,1)
          @ 55, 111 SAY  ;
            sol_totgen PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 57, 01 SAY tit_tit4 +  ;
       SPACE(10) + 'Recep.: ' +  ;
       w_nomemp
     @ 58, 01 SAY SUBSTR(w_obs(1),  ;
       1, 76)
     @ 59, 01 SAY SUBSTR(w_obs(1),  ;
       77, 14) + SUBSTR(w_obs(2),  ;
       1, 62)
     @ 60, 01 SAY SUBSTR(w_obs(2),  ;
       63, 28) + SUBSTR(w_obs(3),  ;
       1, 48)
     @ 61, 01 SAY SUBSTR(w_obs(3),  ;
       49, 42)
     @ 61, 96 SAY w_fecha
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
