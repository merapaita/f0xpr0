*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLOSE DATABASES
@ 05, 05 TO 12, 70 DOUBLE
a = 0
a = a + 1
rge_monbas = 'SOL '
USE ge_tab0
DISPLAY STATUS TO FILE bases.doc
DISPLAY STRUCTURE TO FILE  ;
        bases.doc ADDITIVE
@ 08, 10 SAY a
USE gc_alm00
DO graba
USE gc_cli00
DO graba
USE gc_cmv00
DO graba
USE gc_dco00
DO graba
USE gc_din00
DO graba
USE gc_dip00
DO graba
USE gc_dlp00
DO graba
USE gc_dpv00
DO graba
USE gc_dre00
DO graba
USE gc_dtr00
DO graba
USE gc_dve00
DO graba
USE gc_dvt00
DO graba
USE gc_est00
DO graba
USE gc_eti00
DO graba
USE gc_gas00
DO graba
USE gc_hco00
DO graba
USE gc_hip00
DO graba
USE gc_hlp00
DO graba
USE gc_hpres
DO graba
USE gc_hpv00
DO graba
USE gc_hre00
DO graba
USE gc_htr00
DO graba
USE gc_hve00
DO graba
USE gc_hvest
DO graba
USE gc_hvt00
DO graba
USE gc_imp00
DO graba
USE gc_inv00
DO graba
USE gc_kar00
DO graba
USE gc_modul
DO graba
USE gc_mov00
DO graba
USE gc_nfa00
DO graba
USE gc_nve00
DO graba
USE gc_ord00
DO graba
USE gc_pal00
DO graba
USE gc_par00
DO graba
USE gc_pro00
DO graba
USE gc_tpe00
DO graba
USE gc_uni00
DO graba
USE gc_vnd00
DO graba
USE st_asiem
DO graba
USE st_estad
DO graba
USE st_iclpr
DO graba
USE st_idfac
DO graba
USE st_idped
DO graba
USE st_idpre
DO graba
USE st_imode
DO graba
USE st_inv00
DO graba
USE st_iorep
DO graba
USE st_iparg
DO graba
USE st_iprep
DO graba
USE st_iredo
DO graba
USE st_iscic
DO graba
USE st_iseri
DO graba
USE st_isint
DO graba
USE st_isore
DO graba
USE st_ispre
DO graba
USE st_isrep
DO graba
USE st_issre
DO graba
USE st_itecn
DO graba
USE st_mobra
DO graba
USE st_movca
DO graba
USE st_movso
DO graba
USE st_mvord
DO graba
USE st_sicli
DO graba
USE st_sint
DO graba
USE st_users
DO graba
CLOSE DATABASES
*
PROCEDURE graba
DISPLAY STATUS TO FILE bases.doc  ;
        ADDITIVE
DISPLAY STRUCTURE TO FILE  ;
        bases.doc ADDITIVE
a = a + 1
@ 08, 10 SAY a
*
*** 
*** ReFox - retrace your steps ... 
***
