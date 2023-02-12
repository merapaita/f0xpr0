*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_numdoc, w_codpve,  ;
          w_newgar
SELECT st_iprep
SET ORDER TO numord
SEEK w_numdoc
SCAN WHILE numord = w_numdoc  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N'
          w_numped = numdoc
          SELECT st_idped
          SEEK w_numped +  ;
               w_numaux
          IF FOUND()
               SCAN WHILE  ;
                    w_numped =  ;
                    numdoc .AND.  ;
                    numord =  ;
                    w_numaux  ;
                    .AND.  .NOT.  ;
                    EOF()
                    IF canpro > 0
                         w_rep = w_rep +  ;
                                 totite
                    ENDIF
               ENDSCAN
          ENDIF
          SELECT st_iprep
          DO rbloquea
          REPLACE indest WITH 'C'
          REPLACE user WITH users
          REPLACE date WITH  ;
                  DATE()
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
     ENDIF
ENDSCAN
subto = w_rep + w_monman +  ;
        w_flete
totneto = subto
totigva = ROUND(totneto * w_igv,  ;
          2)
totbrut = totneto + totigva
SELECT st_iorep
SEEK w_numaux
DO rbloquea
REPLACE indest WITH 'C   '
REPLACE cosmob WITH w_monman
REPLACE subtot WITH subto
REPLACE totnet WITH totneto,  ;
        totigv WITH totigva
REPLACE totbru WITH totbrut,  ;
        cosrep WITH w_rep
REPLACE observ WITH w_aux
REPLACE user WITH users
REPLACE date WITH DATE()
REPLACE time WITH TIME()
*
*** 
*** ReFox - retrace your steps ... 
***
