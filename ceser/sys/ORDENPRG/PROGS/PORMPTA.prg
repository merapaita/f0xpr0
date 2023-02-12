*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
IF w_docvta = 'FACT'
     @ 03, 56 SAY 'R.U.C.  ' +  ;
       rge_nroruc
     @ 11, 00 SAY DATE()
     @ 11, 15 SAY w_emisor
     @ 13, 00 SAY w_noment
     @ 13, 55 SAY w_codcli
     @ 15, 00 SAY w_nomcal
     @ 17, 26 SAY w_desmon
     @ 17, 45 SAY 'CONTADO'
     @ 17, 65 SAY 'S/S' + ' ' +  ;
       ALLTRIM(STR(w_numero))
     @ 22, 10 SAY w_concep
     @ 22, 15 SAY ootab('ACTA', ;
       w_concep)
ELSE
     @ 02, 56 SAY 'R.U.C.  ' +  ;
       rge_nroruc
     @ 11, 00 SAY DATE()
     @ 11, 15 SAY w_emisor
     @ 13, 00 SAY w_noment
     @ 13, 55 SAY w_codcli
     @ 15, 00 SAY w_nomcal
     @ 17, 26 SAY w_desmon
     @ 17, 45 SAY 'CONTADO'
     @ 17, 65 SAY 'S/S' + ' ' +  ;
       ALLTRIM(STR(w_numero))
     @ 22, 10 SAY w_concep
     @ 22, 15 SAY ootab('ACTA', ;
       w_concep)
ENDIF
IF w_docvta = 'FACT'
     IF w_codmon = 'DOL '
          @ 36, 63 SAY empre13
          @ 36, 66 SAY w_valvta  ;
            PICTURE  ;
            '999,999,999.99'
          @ 37, 66 SAY w_valigv  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY w_totgen  ;
            PICTURE  ;
            '999,999,999.99'
     ELSE
          @ 36, 63 SAY empre8
          @ 36, 66 SAY sol_totvta  ;
            PICTURE  ;
            '999,999,999.99'
          @ 37, 66 SAY sol_igv  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY sol_totgen  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
ELSE
     IF w_codmon = 'DOL '
          @ 36, 63 SAY empre13
          @ 36, 66 SAY w_totgen  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY w_totgen  ;
            PICTURE  ;
            '999,999,999.99'
     ELSE
          @ 36, 63 SAY empre8
          @ 36, 66 SAY sol_totgen  ;
            PICTURE  ;
            '999,999,999.99'
          @ 38, 66 SAY sol_totgen  ;
            PICTURE  ;
            '999,999,999.99'
     ENDIF
ENDIF
IF w_codmon = 'DOL '
     @ 40, 01 SAY  ;
       oonumlet(w_totgen, ;
       w_codmon)
ELSE
     @ 40, 01 SAY  ;
       oonumlet(sol_totgen, ;
       w_codmon)
ENDIF
@ 44, 62 SAY  ;
  f_ceros(VAL(w_numdoc),10,1)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
