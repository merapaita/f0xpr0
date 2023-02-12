*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
w_tit2 = 'SINTOMAS :'
w_tit3 = 'ACCESORIOS :'
w_tit4 = 'OBSERVACIONES :'
w_tit5 = 'S A L I D A S'
w_tit6 = 'INF.TEC. / NOTAS :'
SET CONSOLE OFF
SET DEVICE TO PRINTER
SET PRINTER ON
SET PRINTER TO lpt1
@ PROW(), PCOL() SAY CHR(15)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
@ 02, 70 - (LEN(w_tit5) / 2) SAY  ;
  w_tit5
@ 02, 115 SAY w_codemi + 'N§: ' +  ;
  w_numord
@ 03, 064 SAY '-------------'
@ 03, 115 SAY 'S/S N§: ' +  ;
  w_numero
@ 04, 115 SAY DATE()
@ 05, 015 SAY w_nomcli
@ 05, 075 SAY w_codcli
@ 06, 015 SAY w_dir
@ 09, 105 SAY w_coddoc
@ 10, 030 SAY w_modelo
@ 10, 050 SAY w_dmarc
@ 10, 075 SAY w_serie
@ 10, 105 SAY w_numdoc
@ 11, 001 SAY w_tit2
@ 11, 070 SAY w_tit3
@ 21, 001 SAY w_tit6
@ 21, 025 SAY 'Operador: ' +  ;
  users
@ 21, 080 SAY 'T‚cnico :'
@ 21, 090 SAY st_iorep.codtec
@ 22, 001 SAY w_infte1
@ 23, 001 SAY w_infte2
@ 24, 001 SAY w_infte3
@ 25, 001 SAY w_tit4
@ 26, 001 SAY  ;
  SUBSTR(st_isrep.desace, 1, 76)
@ 28, 110 SAY DTOC(DATE())
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
