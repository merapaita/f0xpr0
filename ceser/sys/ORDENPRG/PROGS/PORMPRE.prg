*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
@ 01, (65 - LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 01, 115 SAY tit_tit6 +  ;
  STR(tit_nrop, 8)
@ 02, (65 - LEN(tit_tit5) / 2)  ;
  SAY tit_subr
@ 02, 115 SAY tit_tit3 +  ;
  STR(tit_soli, 8)
@ 03, 115 SAY tit_ord +  ;
  STR(tit_orde, 8)
@ 04, 108 SAY DATE()
@ 04, 119 SAY TIME()
@ 05, 010 SAY tit_nomb
@ 05, 075 SAY tit_codi
@ 06, 010 SAY tit_dire
@ 06, 115 SAY tit_feccom
@ 07, 015 SAY tit_distri
@ 07, 115 SAY tit_tel1
@ 08, 015 SAY w_desciu
@ 08, 115 SAY tit_tel2
@ 09, 105 SAY w_destia
@ 10, 003 SAY SUBSTR(tit_prod, 1,  ;
  20)
@ 10, 025 SAY SUBSTR(tit_cpro, 1,  ;
  20)
@ 10, 050 SAY SUBSTR(w_nommar, 1,  ;
  20)
@ 10, 075 SAY w_numser
IF w_indori = 'GARA' .OR.  ;
   w_indori = 'GREC'
     @ 10, 105 SAY DTOC(w_fecvta) +  ;
       '  ' + ALLTRIM(w_docgar)
ENDIF
@ 11, 002 SAY 'ITEM'
@ 11, 028 SAY 'DESCRIPCION'
@ 11, 070 SAY 'CANT'
@ 11, 085 SAY 'P.UNIT.'
@ 11, 105 SAY 'P.TOTAL.'
con_lin = 12
i = 1
DO WHILE  .NOT. EMPTY(pro(i))
     tit_item = LTRIM(STR(i, 2))
     tit_codg = pro(i)
     tit_desc = dex(i)
     tit_cant = can(i)
     IF w_codmon = 'DOL '
          tit_prec = pre(i)
          tit_tota = tot(i)
     ELSE
          tit_prec = pres(i)
          tit_tota = tots(i)
     ENDIF
     @ con_lin, 004 SAY tit_item
     @ con_lin, 028 SAY  ;
       SUBSTR(tit_desc, 1, 30)
     @ con_lin, 070 SAY tit_cant  ;
       PICTURE '9999'
     @ con_lin, 080 SAY tit_prec  ;
       PICTURE '99,999,999.99'
     @ con_lin, 100 SAY tit_tota  ;
       PICTURE '99,999,999.99'
     con_lin = con_lin + 1
     i = i + 1
ENDDO
@ 22, 80 SAY 'TECNICO.:'
@ 22, 90 SAY tit_tecn
@ 22, 101 SAY SUBSTR(w_nomtec, 1,  ;
  30)
@ 23, 00 SAY 'TOTAL REPUESTO:'
@ 23, 15 SAY IIF(w_codmon =  ;
  'DOL ', w_totnet, s_totnet)  ;
  PICTURE '9,999,999.99'
@ 23, 30 SAY 'M/O :'
@ 23, 37 SAY IIF(w_codmon =  ;
  'DOL ', w_totman, s_totman)  ;
  PICTURE '9,999,999.99'
@ 23, 50 SAY 'SUBTOTAL :'
@ 23, 62 SAY IIF(w_codmon =  ;
  'DOL ', w_totafe, s_totafe)  ;
  PICTURE '9,999,999.99'
@ 24, 00 SAY tit_tit4
@ 24, 15 SAY tit_mone
@ 24, 30 SAY empre9 + ':'
@ 24, 37 SAY IIF(w_codmon =  ;
  'DOL ', w_totigv, s_totigv)  ;
  PICTURE '9,999,999.99'
@ 24, 50 SAY 'TOTAL    :'
@ 24, 61 SAY IIF(w_codmon =  ;
  'DOL ', w_totgrl, s_totgrl)  ;
  PICTURE '99,999,999.99'
FOR i = 1 TO 4
     @ 24 + i, 00 SAY  ;
       imp_obser(i)
ENDFOR
@ 28, 96 SAY tit_fech
IF ncopia = 2
     @ 34, (65 - LEN(tit_tit5) /  ;
       2) SAY tit_tit5
     @ 34, 115 SAY tit_tit6 +  ;
       STR(tit_nrop, 8)
     @ 35, (65 - LEN(tit_tit5) /  ;
       2) SAY tit_subr
     @ 35, 115 SAY tit_tit3 +  ;
       STR(tit_soli, 8)
     @ 36, 115 SAY tit_ord +  ;
       STR(tit_orde, 8)
     @ 37, 108 SAY DATE()
     @ 37, 119 SAY TIME()
     @ 38, 010 SAY tit_nomb
     @ 38, 075 SAY tit_codi
     @ 39, 010 SAY tit_dire
     @ 39, 115 SAY tit_feccom
     @ 40, 15 SAY tit_distri
     @ 40, 115 SAY tit_tel1
     @ 41, 015 SAY w_desciu
     @ 41, 115 SAY tit_tel2
     @ 42, 105 SAY w_destia
     @ 43, 003 SAY  ;
       SUBSTR(tit_prod, 1, 20)
     @ 43, 025 SAY  ;
       SUBSTR(tit_cpro, 1, 20)
     @ 43, 050 SAY  ;
       SUBSTR(w_nommar, 1, 20)
     @ 43, 075 SAY w_numser
     IF w_indori = 'GARA' .OR.  ;
        w_indori = 'GREC'
          @ 43, 105 SAY  ;
            DTOC(w_fecvta) + '  ' +  ;
            ALLTRIM(w_docgar)
     ENDIF
     @ 44, 002 SAY 'ITEM'
     @ 44, 028 SAY 'DESCRIPCION'
     @ 44, 070 SAY 'CANT'
     @ 44, 085 SAY 'P.UNIT.'
     @ 44, 105 SAY 'P.TOTAL.'
     con_lin = 45
     i = 1
     DO WHILE  .NOT.  ;
        EMPTY(pro(i))
          tit_item = LTRIM(STR(i,  ;
                     2))
          tit_codg = pro(i)
          tit_desc = dex(i)
          tit_cant = can(i)
          IF w_codmon = 'DOL '
               tit_prec = pre(i)
               tit_tota = tot(i)
          ELSE
               tit_prec = pres(i)
               tit_tota = tots(i)
          ENDIF
          @ con_lin, 004 SAY  ;
            tit_item
          @ con_lin, 028 SAY  ;
            SUBSTR(tit_desc, 1,  ;
            30)
          @ con_lin, 070 SAY  ;
            tit_cant PICTURE  ;
            '9999'
          @ con_lin, 080 SAY  ;
            tit_prec PICTURE  ;
            '99,999,999.99'
          @ con_lin, 100 SAY  ;
            tit_tota PICTURE  ;
            '99,999,999.99'
          con_lin = con_lin + 1
          i = i + 1
     ENDDO
     @ 55, 80 SAY 'TECNICO.:'
     @ 55, 90 SAY tit_tecn
     @ 55, 101 SAY  ;
       SUBSTR(w_nomtec, 1, 30)
     @ 56, 00 SAY  ;
       'TOTAL REPUESTO:'
     @ 56, 15 SAY IIF(w_codmon =  ;
       'DOL ', w_totnet,  ;
       s_totnet) PICTURE  ;
       '9,999,999.99'
     @ 56, 30 SAY 'M/O :'
     @ 56, 37 SAY IIF(w_codmon =  ;
       'DOL ', w_totman,  ;
       s_totman) PICTURE  ;
       '9,999,999.99'
     @ 56, 50 SAY 'SUBTOTAL :'
     @ 56, 62 SAY IIF(w_codmon =  ;
       'DOL ', w_totafe,  ;
       s_totafe) PICTURE  ;
       '9,999,999.99'
     @ 57, 00 SAY tit_tit4
     @ 57, 15 SAY tit_mone
     @ 57, 30 SAY empre9 + ':'
     @ 57, 37 SAY IIF(w_codmon =  ;
       'DOL ', w_totigv,  ;
       s_totigv) PICTURE  ;
       '9,999,999.99'
     @ 57, 50 SAY 'TOTAL    :'
     @ 57, 61 SAY IIF(w_codmon =  ;
       'DOL ', w_totgrl,  ;
       s_totgrl) PICTURE  ;
       '99,999,999.99'
     FOR i = 1 TO 4
          @ 57 + i, 00 SAY  ;
            imp_obser(i)
     ENDFOR
     @ 61, 96 SAY tit_fech
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
