*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
USE SHARED gc_par00
rge_monbas = par_monbas
CLOSE DATABASES
SET DATE briti
DO hora
DO ordenes
CLOSE DATABASES
QUIT
LOGOUT
RETURN
*
PROCEDURE hora
STORE TIME() TO wtime
CLEAR
SET CLOCK ON
@ 10, 10 SAY 'Hora ' GET wtime
READ
IF LASTKEY() = 27
     ?? 'Cancelado'
     RETURN
ENDIF
DO WHILE TIME()<wtime
ENDDO
RETURN
*
PROCEDURE ordenes
USE EXCLUSIVE st_iorep
PACK
USE EXCLUSIVE st_mvord
PACK
RETURN
*
PROCEDURE inventa
USE EXCLUSIVE ge_tab0
PACK
USE EXCLUSIVE gc_pro00
PACK
USE EXCLUSIVE gc_hlp00
PACK
USE EXCLUSIVE gc_dlp00
PACK
USE EXCLUSIVE gc_pal00
PACK
USE EXCLUSIVE gc_cli00
PACK
USE EXCLUSIVE gc_cmv00
PACK
USE EXCLUSIVE gc_alm00
PACK
USE EXCLUSIVE gc_vnd00
PACK
USE EXCLUSIVE gc_hip00
PACK
USE EXCLUSIVE gc_gas00
PACK
USE EXCLUSIVE gc_hco00
PACK
USE EXCLUSIVE gc_dco00
PACK
USE EXCLUSIVE gc_kar00
PACK
USE EXCLUSIVE gc_hve00
PACK
USE EXCLUSIVE gc_dve00
PACK
USE EXCLUSIVE gc_hvt00
PACK
USE EXCLUSIVE gc_dvt00
PACK
USE EXCLUSIVE gc_tpe00
PACK
USE EXCLUSIVE gc_din00
PACK
USE EXCLUSIVE gc_tin00
PACK
USE EXCLUSIVE gc_nve00
PACK
USE EXCLUSIVE gc_ord00
PACK
USE EXCLUSIVE gc_nve00
PACK
USE EXCLUSIVE gc_imp00
PACK
USE EXCLUSIVE gc_inv00
PACK
USE EXCLUSIVE gc_mod00
PACK
USE EXCLUSIVE gc_mov00
PACK
RETURN
*
FUNCTION dtoc2
PARAMETER wk_par
wk_aux = STR(YEAR(wk_par), 4) +  ;
         STR(MONTH(wk_par), 2) +  ;
         STR(DAY(wk_par), 2)
RETURN wk_aux
*
*** 
*** ReFox - retrace your steps ... 
***