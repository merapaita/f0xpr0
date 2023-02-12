*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PUBLIC vecto, vecto2, wk_monto
USE SHARED st_iorep ORDER CODIGO
seek '&wk_numaux'
REPLACE fecfin WITH DATE(),  ;
        indest WITH 'C'
REPLACE codcca WITH  ;
        STR(wk_codtec1, 9),  ;
        cosmob WITH wk_monman
REPLACE user WITH users
REPLACE date WITH DATE()
REPLACE time WITH TIME()
STORE 0 TO sum, tot_titore
wk_codmod = SPACE(15)
wk_numser = SPACE(20)
wk_fecemi = CTOD('  /  /  ')
wk_ano = VAL(SUBSTR(STR(YEAR(DATE()),  ;
         4), 3, 2))
wk_mes = VAL(SUBSTR(STR(MONTH(DATE()),  ;
         4), 3, 2))
wk_indori = indori
wk_codmod = codmod
wk_codmar = codmar
wk_numser = numser
wk_fecrep = fecemi
wk_fecdes = DATE()
wk_codtec1 = codtec
wk_cosmob = cosmob
wk_solici = numsol
USE
wk_titore = DATE() - wk_fecrep
USE SHARED st_isrep AGAIN ORDER  ;
    CODIGO
SEEK '&wk_solici'
wk_feccom = feccom
USE
SELECT 5
USE SHARED st_idped ORDER  ;
    DRE_NUMORD
SELECT 6
USE SHARED st_iprep ORDER codigo
SELECT 5
SEEK '&wk_numaux'
IF FOUND()
     DO WHILE  .NOT. EOF()
          IF numord = wk_numaux
               tot_totite = totite
               wrk_numped = numdoc
               SELECT 6
               SEEK wrk_numped
               IF FOUND()
                    IF indest <>  ;
                       'N'
                         tot_titore =  ;
                          tot_titore +  ;
                          tot_totite
                    ENDIF
               ENDIF
               SELECT 5
          ENDIF
          SKIP
     ENDDO
ENDIF
SELECT 6
SET ORDER TO REP_NUMORD
SEEK '&wk_numaux'
IF FOUND()
     IF indest <> 'N'
          REPLACE indest WITH 'P'
     ENDIF
     DO WHILE  .NOT. EOF()
          IF STR(wk_numord, 8) =  ;
             numord
               IF indest <> 'N'
                    REPLACE indest  ;
                            WITH  ;
                            'P'
               ENDIF
          ENDIF
          SKIP
     ENDDO
ENDIF
USE
SELECT 5
USE
subto = tot_titore + wk_monman
totde = subto * (wk_desc / 100)
totneto = subto - totde
totigva = totneto * (wk_igv /  ;
          100)
totbrut = totneto + totigva
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_numaux'
REPLACE subtot WITH subto, totdes  ;
        WITH totde
REPLACE totnet WITH totneto,  ;
        totigv WITH totigva
REPLACE totbru WITH totbrut,  ;
        cosrep WITH tot_titore
USE
USE SHARED st_estfa
FOR t = 1 TO cuenta
     APPEND BLANK
     REPLACE numord WITH  ;
             wk_numaux, codfal  ;
             WITH vecto(t)
     REPLACE monto WITH vecto2(t)
ENDFOR
USE
USE st_estad
APPEND BLANK
REPLACE indori WITH wk_indori,  ;
        anorep WITH wk_ano
REPLACE mesrep WITH wk_mes,  ;
        numord WITH wk_numord
REPLACE codmod WITH wk_codmod,  ;
        codmar WITH wk_codmar
REPLACE numser WITH wk_numser,  ;
        fecrep WITH wk_fecrep
REPLACE fecdes WITH DATE(),  ;
        feccon WITH wk_feccom
REPLACE codtec WITH wk_codtec1,  ;
        valmao WITH wk_cosmob
REPLACE titore WITH wk_titore,  ;
        valrep WITH tot_titore
REPLACE user WITH users
REPLACE date WITH DATE()
REPLACE time WITH TIME()
USE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
