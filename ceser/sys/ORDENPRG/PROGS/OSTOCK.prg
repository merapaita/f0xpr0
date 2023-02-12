*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
USE SHARED gc_dco00 ORDER  ;
    dco_codprp
GOTO TOP
STORE 0 TO soli, tran, back
SCAN WHILE  .NOT. EOF()
     IF dco_indest = 'S'
          soli = soli +  ;
                 (dco_cansol *  ;
                 dco_valpre)
     ENDIF
     IF dco_cancon > 0
          tran = tran +  ;
                 (dco_cancon *  ;
                 dco_conpre)
     ENDIF
     IF dco_canbor > 0
          back = back +  ;
                 (dco_canbor *  ;
                 dco_conpre)
     ENDIF
ENDSCAN
CLEAR
?? 'Totales'
? 'Solicitado: '
?? soli PICTURE '99,999,999.99'
? 'Tr nsito  : '
?? tran PICTURE '99,999,999.99'
? 'Backorders: '
?? back PICTURE '99,999,999.99'
*
*** 
*** ReFox - retrace your steps ... 
***
