*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SELECT 1
USE SHARED ST_ISPRE ORDER codigo
SELECT 2
USE SHARED ST_IDPRE ORDER CODIGO
SELECT 3
USE SHARED st_iorep ORDER codigo
STORE 0 TO w_totrep, w_totigv,  ;
      w_totafe, w_totman
STORE SPACE(4) TO w_indest
SELECT st_ispre
GOTO TOP
num = 0
SCAN WHILE  .NOT. EOF()
     num = num + 1
     @ 10, 20 SAY num
     w_totrep = 0
     w_indest = indest
     SELECT st_iorep
     SEEK st_ispre.numord
     IF FOUND()
          DO CASE
               CASE indest =  ;
                    'V   '
                    w_indest = 'V   '
               CASE indest =  ;
                    'C   ' .OR.  ;
                    indest =  ;
                    'F   ' .OR.  ;
                    indest =  ;
                    'B   '
                    w_indest = 'C   '
               CASE indest =  ;
                    'N   '
                    w_indest = 'N   '
          ENDCASE
     ENDIF
     SELECT st_idpre
     SEEK st_ispre.numdoc
     IF FOUND()
          SCAN WHILE  ;
               st_ispre.numdoc =  ;
               numdoc .AND.   ;
               .NOT. EOF()
               DO rbloquea
               REPLACE valpro  ;
                       WITH  ;
                       ROUND(valpro *  ;
                       (59.0/50),  ;
                       2)
               REPLACE totite  ;
                       WITH  ;
                       canpro *  ;
                       valpro
               UNLOCK
               w_totrep = w_totrep +  ;
                          totite
          ENDSCAN
     ENDIF
     SELECT st_ispre
     w_totman = ROUND(monman *  ;
                (59.0/50), 2)
     w_totgrl = w_totrep +  ;
                w_totman
     w_totafe = ROUND(w_totgrl /  ;
                1.18 , 2)
     w_totigv = w_totgrl -  ;
                w_totafe
     DO rbloquea
     REPLACE monman WITH w_totman
     REPLACE monrep WITH w_totrep
     REPLACE totigv WITH w_totigv
     REPLACE indest WITH w_indest
     UNLOCK
ENDSCAN
CLOSE DATABASES
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
