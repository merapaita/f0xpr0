*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
@ 02, 70 - (LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 02, 115 SAY tit_eminro
@ 03, 115 SAY tit_nrosol
@ 04, 115 SAY tit_fecha
@ 05, 015 SAY tit_client
@ 05, 075 SAY tit_codigo
@ 06, 015 SAY tit_direcc
@ 06, 115 SAY tit_feccom
@ 07, 015 SAY tit_distri
@ 07, 115 SAY tit_tel1
@ 08, 115 SAY tit_tel2
@ 09, 105 SAY ALLTRIM(w_destia)
@ 10, 001 SAY tit_desmod
@ 10, 030 SAY tit_codmod
@ 10, 050 SAY tit_desmar
@ 10, 075 SAY tit_numser
@ 10, 105 SAY ALLTRIM(w_dogtia)
@ 11, 001 SAY tit_tit2
@ 11, 070 SAY tit_tit3
FOR lin = 1 TO 9
     IF w_codsin(lin) <>  ;
        SPACE(35)
          @ 11 + lin, 01 SAY  ;
            w_codsin(lin)
     ENDIF
     @ 11 + lin, 071 SAY  ;
       w_acceso(lin)
ENDFOR
@ 21, 001 SAY tit_tit6
@ 21, 080 SAY 'T‚cnico :'
@ 21, 090 SAY STR(w_otec, 9)
@ 21, 103 SAY SUBSTR(w_destec, 1,  ;
  30)
@ 22, 01 SAY w_obs(1)
@ 23, 01 SAY w_obs(2)
@ 24, 01 SAY w_obs(3)
@ 25, 01 SAY tit_tit4
@ 26, 01 SAY SUBSTR(w_sol(1), 1,  ;
  76)
@ 27, 01 SAY SUBSTR(w_sol(1), 77,  ;
  14) + SUBSTR(w_sol(2), 1, 62)
@ 28, 01 SAY SUBSTR(w_sol(2), 63,  ;
  28) + SUBSTR(w_sol(3), 1, 48)
@ 28, 97 SAY tit_fechho
@ 29, 01 SAY SUBSTR(w_sol(3), 49,  ;
  42)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
