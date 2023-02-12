*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
USE EXCLUSIVE ST_IOREP
PACK
DELETE TAG all
INDEX ON numdoc TAG codigo
INDEX ON codent + dtoc2(fecemi)  ;
      TAG ord_codent
INDEX ON dtoc2(fecemi) TAG  ;
      ord_fchemi
INDEX ON codmar + codmod +  ;
      dtoc2(fecemi) TAG  ;
      ord_codmar
INDEX ON numsol TAG ord_numsol
INDEX ON numser TAG ord_numser
INDEX ON codtec + DTOS(fecemi) +  ;
      numdoc TAG ord_tecn
INDEX ON DTOS(fecemi) + numdoc  ;
      TAG ord_fecdoc
INDEX ON auxest + codemi +  ;
      DTOS(fecest) + numdoc TAG  ;
      ord_esem
INDEX ON codfabo + numfabo +  ;
      numdoc TAG ord_numfab
INDEX ON indori + indest +  ;
      codtall + DTOS(fecemi) +  ;
      numdoc TAG ord_inesta
INDEX ON codtec + auxest TAG  ;
      ord_tecest
INDEX ON DTOS(fecfin) + indori  ;
      TAG ord_fecind
INDEX ON codmar + codmod + numser  ;
      TAG ord_mamose
INDEX ON DTOS(fecent) + indori  ;
      TAG ord_entind
INDEX ON DTOS(fecfabo) TAG  ;
      ord_fecfac
INDEX ON codent TAG codent
INDEX ON DTOS(date) + time TAG  ;
      fecha
USE
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
