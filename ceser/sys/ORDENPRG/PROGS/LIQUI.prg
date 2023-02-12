*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
CLEAR
SET SYSMENU ON
SET ESCAPE ON
SELECT 1
USE SHARED st_iorep ORDER  ;
    ORD_INESTA
SELECT 2
USE SHARED ST_IPREP ORDER  ;
    REP_NUMORD
SELECT 3
USE SHARED ST_IDPED ORDER CODIGO
SELECT 4
USE SHARED st_mvord ORDER codigo
SELECT 5
USE SHARED GE_TAB0 ORDER CODIGO
STORE SPACE(4) TO varind, varfin
STORE 0 TO tot_titore
cont = 0
SELECT st_iorep
SEEK 'FGAR' + 'C   ' + '002 '
SCAN WHILE indori = 'FGAR' .AND.  ;
     indest = 'C   ' .AND.  ;
     codtall = '002 ' .AND.   ;
     .NOT. EOF()
     @ 10, 10 SAY numdoc
     IF YEAR(fecemi) < 1996 .AND.  ;
        totrep(numdoc) = 0 .AND.  ;
        (auxest = '010 ' .OR.  ;
        auxest = '021 ')
          cont = cont + 1
          @ 12, 14 SAY cont
          @ 10, 20 SAY indori
          @ 10, 30 SAY codtall
          @ 10, 40 SAY  ;
            YEAR(fecemi)
          @ 10, 50 SAY tot_titore
          @ 10, 60 SAY auxest
          IF auxest = '010 '
               varfin = '025 '
          ELSE
               varfin = '028 '
          ENDIF
          DO rbloquea
          REPLACE auxest WITH  ;
                  varfin
          IF fecfin = CTOD( ;
             '  /  /  ')
               REPLACE fecfin  ;
                       WITH  ;
                       CTOD( ;
                       '31/12/95' ;
                       )
          ENDIF
          UNLOCK
          SELECT ge_tab0
          SEEK 'ESOR' + varfin
          wk_esorde = tab_destab
          SELECT st_mvord
          APPEND BLANK
          DO rbloquea
          REPLACE dia WITH  ;
                  CTOD( ;
                  '31/12/95'),  ;
                  hora WITH  ;
                  TIME()
          REPLACE orden WITH  ;
                  st_iorep.numdoc,  ;
                  destado WITH  ;
                  wk_esorde
          REPLACE estado WITH  ;
                  varfin
          REPLACE date WITH  ;
                  CTOD( ;
                  '31/12/95')
          REPLACE time WITH  ;
                  TIME()
          UNLOCK
     ENDIF
     SELECT st_iorep
ENDSCAN
CLOSE DATABASES
RETURN
*
FUNCTION totrep
PARAMETER codigo
w_numped = SPACE(8)
tot_titore = 0
SELECT st_iprep
SEEK codigo
IF FOUND()
     SCAN WHILE numord = codigo  ;
          .AND.  .NOT. EOF()
          IF indest <> 'N'
               w_numped = numdoc
               SELECT st_idped
               SEEK w_numped +  ;
                    codigo
               IF FOUND()
                    SCAN WHILE  ;
                         w_numped =  ;
                         numdoc  ;
                         .AND.  ;
                         numord =  ;
                         codigo  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         tot_titore =  ;
                          tot_titore +  ;
                          totite
                    ENDSCAN
               ENDIF
          ENDIF
     ENDSCAN
ENDIF
SELECT st_iorep
RETURN tot_titore
*
PROCEDURE frygr
cont = 0
FOR i = 1 TO 2
     IF i = 1
          varind = 'FREC'
     ELSE
          varind = 'GREC'
     ENDIF
     SELECT st_iorep
     SEEK varind
     SCAN WHILE indori = varind  ;
          .AND.  .NOT. EOF()
          @ 10, 10 SAY numdoc
          IF codtall = '002 '  ;
             .AND. YEAR(fecemi) <  ;
             1996 .AND.  ;
             totrep(numdoc) = 0  ;
             .AND. (auxest =  ;
             '010 ' .OR. auxest =  ;
             '021 ')
               cont = cont + 1
               @ 12, 14 SAY cont
               @ 10, 20 SAY  ;
                 indori
               @ 10, 30 SAY  ;
                 codtall
               @ 10, 40 SAY  ;
                 YEAR(fecemi)
               @ 10, 50 SAY  ;
                 tot_titore
               @ 10, 60 SAY  ;
                 auxest
               IF auxest = '010 '
                    varfin = '029 '
               ELSE
                    varfin = '028 '
               ENDIF
               DO rbloquea
               REPLACE auxest  ;
                       WITH  ;
                       varfin
               REPLACE indest  ;
                       WITH 'C'
               IF fecfin = CTOD( ;
                  '  /  /  ')
                    REPLACE fecfin  ;
                            WITH  ;
                            CTOD( ;
                            '31/12/95' ;
                            )
               ENDIF
               UNLOCK
               SELECT ge_tab0
               SEEK 'ESOR' +  ;
                    varfin
               wk_esorde = tab_destab
               SELECT st_mvord
               APPEND BLANK
               DO rbloquea
               REPLACE dia WITH  ;
                       CTOD( ;
                       '31/12/95' ;
                       ), hora  ;
                       WITH  ;
                       TIME()
               REPLACE orden WITH  ;
                       st_iorep.numdoc,  ;
                       destado  ;
                       WITH  ;
                       wk_esorde
               REPLACE estado  ;
                       WITH  ;
                       varfin
               REPLACE date WITH  ;
                       CTOD( ;
                       '31/12/95' ;
                       )
               REPLACE time WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          SELECT st_iorep
     ENDSCAN
ENDFOR
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
