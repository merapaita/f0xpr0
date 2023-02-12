*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
STORE DATE() TO wrk_fecini,  ;
      wrk_fecfin
STORE SPACE(4) TO wrk_garini,  ;
      wrk_garfin, wrk_emiini,  ;
      wrk_emifin
STORE 1 TO wrk_copias
DEFINE WINDOW winmarco FROM 04,  ;
       01 TO 20, 78 FLOAT CLOSE  ;
       MINIMIZE NONE COLOR SCHEME  ;
       2
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'PLANILLA DE VENTAS',  ;
   'REPORTE'
wrk_progra = PROGRAM()
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000001000011', 2
wrk_selpro = SELECT()
nop1 = 1
nop2 = 1
= ooopen('GE_TAB0',1)
= ooopen('GC_PRO00',1)
= ooopen('GC_KAR00',1)
= ooopen('ST_IPREP',1)
= ooopen('ST_IDPED',1)
SET CURSOR ON
ACTIVATE WINDOW SAME winmarco
= ooscreen()
@ 01, 24 GET wrk_fecini SIZE 1, 8  ;
  VALID oovalid(VARREAD()) .AND.   ;
  .NOT. EMPTY(wrk_fecini) COLOR  ;
  SCHEME 6
@ 01, 51 GET wrk_fecfin SIZE 1, 8  ;
  VALID  .NOT. EMPTY(wrk_fecfin)  ;
  COLOR SCHEME 6
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
                 wrk_copias  ;
                 PICTURE '999'  ;
                 VALID  .NOT.  ;
                 EMPTY(wrk_copias)
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
     CASE cvalid = 'WRK_FECINI'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE ooproceso
PARAMETER nop1
= ooaviso( ;
  ' T R A B A J A N D O . . .')
SELECT st_idped.numdoc,  ;
       st_idped.numord,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_iorep.fecemi,  ;
       st_iorep.codemi,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.codmar,  ;
       st_iorep.codmod,  ;
       st_iorep.numser,  ;
       st_iorep.numsol,  ;
       st_idped.date FROM  ;
       ST_IDPED, ST_IOREP WHERE  ;
       st_iorep.numdoc =  ;
       st_idped.numord AND  ;
       (BETWEEN(st_idped.date,  ;
       wrk_fecini, wrk_fecfin))  ;
       AND  ;
       (BETWEEN(st_iorep.indori,  ;
       wrk_garini, wrk_garfin))  ;
       AND  ;
       (BETWEEN(st_iorep.codemi,  ;
       wrk_emiini, wrk_emifin))  ;
       AND st_idped.canpro > 0  ;
       ORDER BY st_iorep.codemi,  ;
       st_idped.date,  ;
       st_idped.numdoc INTO  ;
       CURSOR QUERY
COUNT TO nreg
IF nreg = 0
     DO p_mensaje WITH  ;
        ' NO SE ENCONTRO INFORMACION '
     DEACTIVATE WINDOW winmensaje
     RETURN
ELSE
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
                    DEACTIVATE WINDOW  ;
                               winmensaje
                    RETURN
               ENDIF
          ENDIF
     ENDDO
     = ooaviso( ;
       ' I M P R I M I E N D O . . .' ;
       )
     SET DEVICE TO PRINTER
     @ 0, 0 SAY CHR(27) + CHR(15)
     REPORT FORMAT AGCR2210 TO  ;
            PRINTER NOCONSOLE
     SET DEVICE TO PRINTER
     @ 0, 0 SAY CHR(27) + CHR(18)
     SET DEVICE TO SCREEN
     RELEASE WINDOW winmensaje
     DO p_mensaje WITH  ;
        ' FIN DE IMPRESION '
ELSE
     wrk_txt = f_texto()
     REPO FORM AGCR0210 TO FILE &WRK_TXT;
NOCONSOLE  
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     modi comm &WRK_TXT noedit WINDOW;
PANTALL 
     SET SYSMENU OFF
     RELEASE WINDOW winmensaje
ENDIF
RETURN
*
PROCEDURE ooscreen
@ 00, 00 TO 18, 79
@ 01, 02 SAY 'Peri¢do       :'  ;
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
FUNCTION oorangos
PARAMETER nx1, ny1, ny2, nlon,  ;
          ccodigo, cvarread
IF (LASTKEY() = 13)
     = ootablas(nx1,ny1,ny2,nlon, ;
       ccodigo,cvarread)
ENDIF
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF EMPTY(&cvarread)
     DO p_mensaje WITH  ;
        ' Falta c¢digo de Tabla '
     RETURN .F.
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***
