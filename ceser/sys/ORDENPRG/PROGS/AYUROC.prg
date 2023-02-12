*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
*
PROCEDURE ayudaro
PARAMETER w_clave, w_tit, w_campo
CREATE CURSOR TAB_PAR (codigo C  ;
       (4), descri C (30))
SELECT ge_tab0
SEEK w_clave
IF FOUND()
     SCAN WHILE tab_codpre =  ;
          w_clave .AND.  .NOT.  ;
          EOF()
          SELECT tab_par
          APPEND BLANK
          REPLACE codigo WITH  ;
                  tab_codtab
          REPLACE descri WITH  ;
                  tab_destab
          SELECT ge_tab0
     ENDSCAN
     ACTIVATE WINDOW ayudaro
     ON KEY LABEL enter do carcod
     SELECT tab_par
     BROWSE FIELDS codigo :H =  ;
            'CODIGO', descri :H =  ;
            'DESCRIPCION' FREEZE  ;
            codigo TITLE w_tit IN  ;
            ayudaro
     ON KEY
ENDIF
DEACTIVATE WINDOW ayudaro
RETURN &w_campo
*
PROCEDURE carcod
IF LASTKEY() = 13
     &w_campo=codigo
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
