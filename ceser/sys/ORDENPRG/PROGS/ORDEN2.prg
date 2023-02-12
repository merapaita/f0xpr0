*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
@ 02, 70 - (LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 02, 115 SAY tit_eminro
@ 03, 70 - (LEN(tit_subray) / 2)  ;
  SAY tit_subray
@ 03, 115 SAY tit_nrosol
@ 04, 115 SAY tit_fecha
@ 05, 015 SAY tit_client
@ 05, 075 SAY tit_codigo
@ 06, 015 SAY tit_direcc
@ 06, 115 SAY tit_feccom
@ 07, 115 SAY tit_telef1
@ 09, 115 SAY tit_tipo
@ 10, 001 SAY tit_desmod
@ 10, 030 SAY tit_codmod
@ 10, 050 SAY tit_desmar
@ 10, 072 SAY tit_numser
@ 10, 092 SAY ALLTRIM(w_dogtia)
@ 11, 001 SAY tit_tit2
@ 11, 070 SAY tit_tit3
FOR lin = 1 TO 7
     IF wk_codsin(lin) <>  ;
        SPACE(35)
          @ 11 + lin, 01 SAY  ;
            wk_codsin(lin)
     ENDIF
     @ 11 + lin, 071 SAY  ;
       wk_acceso(lin)
ENDFOR
@ 21, 001 SAY tit_tit6
@ 21, 070 SAY 'T‚cnico :'
@ 21, 080 SAY STR(w_otec, 9)
@ 21, 103 SAY SUBSTR(w_destec, 1,  ;
  30)
nota1 = wk_obsord(1) +  ;
        wk_obsord(2)
nota2 = wk_obsord(3) +  ;
        wk_obsord(4)
nota3 = wk_obsord(5) +  ;
        wk_obsord(6)
@ 22, 01 SAY nota1
@ 23, 01 SAY nota2
@ 24, 01 SAY nota3
@ 25, 01 SAY tit_tit4 + SPACE(10) +  ;
  'Recep.: ' + wrk_nomemp
@ 26, 01 SAY SUBSTR(wk_obs(1), 1,  ;
  76)
@ 27, 01 SAY SUBSTR(wk_obs(1), 77,  ;
  14) + SUBSTR(wk_obs(2), 1, 62)
@ 28, 01 SAY SUBSTR(wk_obs(2), 63,  ;
  28) + SUBSTR(wk_obs(3), 1, 48)
@ 28, 97 SAY tit_fechho
??? CHR(15)
EJECT
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
