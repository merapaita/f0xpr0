*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
SET CENTURY ON
STORE DATE() TO w_fecini,  ;
      w_fecfin
STORE SPACE(4) TO w_garini,  ;
      w_garfin, w_emiini,  ;
      w_emifin
STORE 1 TO w_copia
DEFINE WINDOW winmarco FROM 04,  ;
       01 TO 20, 78 FLOAT CLOSE  ;
       MINIMIZE NONE COLOR SCHEME  ;
       2
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'CONSUMO DE REPUESTOS',  ;
   'REPORTE'
w_program = PROGRAM()
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000001000011', 2
wrk_selpro = SELECT()
STORE 1 TO nop1, nop2
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER codigo
SELECT 3
USE SHARED st_iprep ORDER  ;
    rep_fecnum
SELECT 4
USE SHARED st_idped ORDER codigo
SELECT 5
USE SHARED st_iorep ORDER codigo
SET CURSOR ON
ACTIVATE WINDOW SAME winmarco
= ooscreen()
@ 01, 24 GET w_fecini SIZE 1, 10  ;
  VALID oovalid(VARREAD()) .AND.   ;
  .NOT. EMPTY(w_fecini) COLOR  ;
  SCHEME 6
@ 01, 51 GET w_fecfin SIZE 1, 10  ;
  VALID  .NOT. EMPTY(w_fecfin)  ;
  COLOR SCHEME 6
@ 02, 24 GET w_garini SIZE 1, 4  ;
  PICTURE '@!' VALID  ;
  oovalid(VARREAD()) COLOR SCHEME  ;
  6
@ 02, 51 GET w_garfin SIZE 1, 4  ;
  PICTURE '@!' VALID  ;
  oovalid(VARREAD()) COLOR SCHEME  ;
  6
@ 03, 24 GET w_emiini SIZE 1, 4  ;
  PICTURE '@!' VALID  ;
  oovalid(VARREAD()) COLOR SCHEME  ;
  6
@ 03, 51 GET w_emifin SIZE 1, 4  ;
  PICTURE '@!' VALID  ;
  oovalid(VARREAD()) COLOR SCHEME  ;
  6
@ 05, 18 GET nop1 DEFAULT 1 SIZE  ;
  1, 10, 0 PICTURE  ;
  '@*RTHN Pantalla  ;Impresora '  ;
  VALID oovalid(VARREAD()) COLOR  ;
  SCHEME 12
READ CYCLE COLOR SCHEME 6
RELEASE WINDOW winmarco
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
erase &w_idx
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'NOP1'
          IF nop1 = 2
               @ 05, 50 SAY  ;
                 'Copias :' GET  ;
                 w_copia RANGE 1, ;
                 9 PICTURE '99'  ;
                 VALID  .NOT.  ;
                 EMPTY(w_copia)
               READ
               IF LASTKEY() = 27
                    @ 05, 50  ;
                      CLEAR TO 05,  ;
                      70
                    RETURN .F.
               ENDIF
          ENDIF
          = ooproceso(nop1)
     CASE cvalid = 'NOP2'
          IF LASTKEY() = 13
               = ooproceso(nop1)
          ENDIF
     CASE cvalid = 'W_FECINI'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_GARINI'
          w_busca = 'INGA'
          SELECT ge_tab0
          SEEK w_busca + w_garini
          IF FOUND()
               @ 02, 28 SAY  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 20)
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo no Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_GARFIN'
          w_busca = 'INGA'
          SELECT ge_tab0
          SEEK w_busca + w_garfin
          IF FOUND()
               @ 02, 55 SAY  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 20)
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo no Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_EMIINI'
          w_busca = 'EMIS'
          SELECT ge_tab0
          SEEK w_busca + w_emiini
          IF FOUND()
               @ 03, 28 SAY  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 20)
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo no Existe '
               RETURN .F.
          ENDIF
     CASE cvalid = 'W_EMIFIN'
          w_busca = 'EMIS'
          SELECT ge_tab0
          SEEK w_busca + w_emifin
          IF FOUND()
               @ 03, 55 SAY  ;
                 SUBSTR(ge_tab0.tab_destab,  ;
                 1, 20)
          ELSE
               DO p_mensaje WITH  ;
                  ' C¢digo no Existe '
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ooproceso
PARAMETER nop1
= ooaviso( ;
  ' T R A B A J A N D O . . .')
CREATE CURSOR usos (numdoc C (8),  ;
       codemi C (4), indori C (4),  ;
       fecemi D (8), numord C (8),  ;
       numsol C (8), codpro C  ;
       (14), canpro N (9), valpro  ;
       N (12, 2), subcat C (4))
SELECT st_iprep
SET NEAR ON
SEEK DTOS(w_fecini)
SET NEAR OFF
SCAN WHILE fecemi <= w_fecfin  ;
     .AND.  .NOT. EOF()
     IF indest <> 'N'
          SELECT st_iorep
          SEEK st_iprep.numord
          IF FOUND()
               IF (indori >=  ;
                  w_garini .AND.  ;
                  indori <=  ;
                  w_garfin) .AND.  ;
                  (codemi >=  ;
                  w_emiini .AND.  ;
                  codemi <=  ;
                  w_emifin)
                    SELECT st_idped
                    SEEK st_iprep.numdoc
                    SCAN WHILE  ;
                         numdoc =  ;
                         st_iprep.numdoc  ;
                         .AND.   ;
                         .NOT.  ;
                         EOF()
                         IF canpro >  ;
                            0
                              SELECT  ;
                               gc_pro00
                              SEEK  ;
                               st_idped.codpro
                              w_subcat =  ;
                               pro_subcat
                              SELECT  ;
                               usos
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               codemi  ;
                               WITH  ;
                               st_iorep.codemi,  ;
                               indori  ;
                               WITH  ;
                               st_iorep.indori
                              REPLACE  ;
                               numdoc  ;
                               WITH  ;
                               st_idped.numdoc,  ;
                               fecemi  ;
                               WITH  ;
                               st_iprep.fecemi
                              REPLACE  ;
                               numord  ;
                               WITH  ;
                               st_iprep.numord,  ;
                               numsol  ;
                               WITH  ;
                               st_iorep.numsol
                              REPLACE  ;
                               codpro  ;
                               WITH  ;
                               st_idped.codpro,  ;
                               canpro  ;
                               WITH  ;
                               st_idped.canpro
                              REPLACE  ;
                               valpro  ;
                               WITH  ;
                               st_idped.valpro,  ;
                               subcat  ;
                               WITH  ;
                               w_subcat
                              SELECT  ;
                               st_idped
                         ENDIF
                    ENDSCAN
               ENDIF
          ENDIF
     ENDIF
     SELECT st_iprep
ENDSCAN
SELECT usos
COUNT TO nreg
IF nreg = 0
     DO p_mensaje WITH  ;
        ' NO SE ENCONTRO INFORMACION '
     DEACTIVATE WINDOW winmensaje
     RETURN
ELSE
     SELECT usos
     w_idx = f_indice()
     inde on codemi+dtos(fecemi)+numdoc;
to &w_idx
     = ooimprime(nop1)
ENDIF
RETURN
*
PROCEDURE ooimprime
PARAMETER nop1
IF nop1 = 2
     DO WHILE LASTKEY()<>27
          IF SYS(13) = 'OFFLINE'
               swt = f_yesno( ;
                     'Impresora fuera de Linea. Intentar nuevamente ' ;
                     )
               IF swt
                    LOOP
               ELSE
                    EXIT
               ENDIF
          ENDIF
          = ooaviso( ;
            ' I M P R I M I E N D O . . .' ;
            )
          SET DEVICE TO PRINTER
          @ 00, 00 SAY CHR(27) +  ;
            CHR(15)
          FOR a = 1 TO w_copia
               REPORT FORMAT  ;
                      agcr0210 TO  ;
                      PRINTER  ;
                      NOCONSOLE
          ENDFOR
          @ 00, 0 SAY CHR(27) +  ;
            CHR(18)
          SET DEVICE TO SCREEN
          DO p_mensaje WITH  ;
             ' FIN DE IMPRESION '
          EXIT
     ENDDO
ELSE
     w_txt = f_texto()
     repo form agcr0210 to file &w_txt;
noconsole
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     modi comm &w_txt noedit WINDOW PANTALL;

     SET SYSMENU OFF
ENDIF
DEACTIVATE WINDOW winmensaje
RETURN
*
PROCEDURE ooscreen
@ 00, 00 TO 18, 79
@ 01, 02 SAY 'Per¡odo       :'  ;
  SIZE 1, 15, 0
@ 02, 02 SAY 'Tipo de Gt¡a. :'  ;
  SIZE 1, 15, 0
@ 03, 02 SAY 'Emisor        :'  ;
  SIZE 1, 15, 0
@ 05, 02 SAY 'Destino       :'  ;
  SIZE 1, 15, 0
@ 01, 18 SAY 'Del ' SIZE 1, 05,  ;
  0
@ 01, 46 SAY 'Al ' SIZE 1, 05, 0
@ 02, 18 SAY 'Del ' SIZE 1, 05,  ;
  0
@ 02, 46 SAY 'Al ' SIZE 1, 05, 0
@ 03, 18 SAY 'Del ' SIZE 1, 05,  ;
  0
@ 03, 46 SAY 'Al ' SIZE 1, 05, 0
@ 04, 00 SAY 'Ã' SIZE 1, 1, 0
@ 04, 77 SAY '´' SIZE 1, 1, 0
@ 04, 01 TO 04, 76
@ 06, 00 SAY 'Ã' SIZE 1, 1, 0
@ 06, 77 SAY '´' SIZE 1, 1, 0
@ 06, 01 TO 06, 76
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
