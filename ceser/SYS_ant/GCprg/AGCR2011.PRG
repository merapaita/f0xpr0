*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER var1
ON KEY
wrk_prg = PROGRAM()
DEFINE POPUP tablas FROM 11, 00  ;
       TO 17, 35 PROMPT FIELDS  ;
       tab_destab IN screen COLOR  ;
       SCHEME 20
ON SELECTION POPUP tablas DO P_CONFIRM
DEFINE POPUP impresora FROM 18,  ;
       10 IN screen COLOR SCHEME  ;
       20
DEFINE BAR 1 OF impresora PROMPT  ;
       '  Por  \<Pantalla   '
DEFINE BAR 2 OF impresora PROMPT  ;
       '  Por  \<Impresora  '
ON SELECTION POPUP impresora DO REPORTS;
WITH BAR()
SELECT 1
USE GE_TAB0 ORDER CODIGO
SEEK 'TABL'
arch1 = f_archivo()
COPY TO &ARCH1 WHILE TAB_CODPRE="TABL"
SELECT 20
USE &ARCH1
ACTIVATE POPUP tablas
CLOSE DATABASES
RELEASE POPUP tablas
ACTIVATE SCREEN
RETURN
*
PROCEDURE p_confirm
ACTIVATE POPUP impresora
SELECT 20
RETURN
*
PROCEDURE reports
PARAMETER bar
rpt = .T.
IF bar = 2
     rpt = f_yesno( ;
           'PREPARE LA IMPRESORA Y CONFIRME ACCION' ;
           )
     IF rpt
          stat = SYS(13)
          DO WHILE stat='OFFLINE'
               swt = f_yesno( ;
                     'LA IMPRESORA NO ESTA LISTA, VERIFIQUE POR FAVOR' ;
                     )
               IF swt
                    stat = SYS(13)
               ELSE
                    DEACTIVATE POPUP  ;
                               impresora
                    SET CURSOR OFF
                    RETURN
               ENDIF
          ENDDO
     ENDIF
ENDIF
IF rpt
     arch2 = f_archivo()
     SELECT 20
     aux_tabl = tab_codtab
     aux_fac = tab_factor
     aux_des = tab_destab
     SELECT 1
     SEEK aux_tabl
     COPY TO &ARCH2 WHILE TAB_CODPRE=AUX_TABL
     SET DEVICE TO PRINTER
     nomb = f_texto()
     IF bar = 1
          SET PRINTER TO &NOMB
     ELSE
          SET PRINTER ON
     ENDIF
     SET CONSOLE OFF
     SELECT 21
     USE &ARCH2
     GOTO TOP
     fil = 61
     STORE 0 TO pagina, acum1s,  ;
           acum2s, acum3s, acum1d,  ;
           acum2d, acum3d
     x = 1
     STORE SPACE(6) TO xsolic,  ;
           xprovs
     DO WHILE  .NOT. EOF()
          IF fil > 55 .OR. fil =  ;
             1
               fil = 1
               DO cabecera WITH  ;
                  bar
          ENDIF
          @ fil, 06 SAY  ;
            tab_codtab
          @ fil, 15 SAY  ;
            tab_destab
          IF aux_fac <> 0
               @ fil, 60 SAY  ;
                 tab_factor  ;
                 PICTURE  ;
                 '9999999.99'
          ENDIF
          fil = fil + 1
          SELECT 21
          IF  .NOT. EOF()
               SKIP
          ENDIF
     ENDDO
     IF bar = 2
          EJECT
     ELSE
          fil = fil + 1
          @ fil, 00 SAY SPACE(1)
     ENDIF
     SET DEVICE TO SCREEN
     SET CONSOLE ON
     IF bar = 1
          SET PRINTER TO
          DEFINE WINDOW report  ;
                 FROM 0, 0 TO 24,  ;
                 79 TITLE  ;
                 'REPORTE' COLOR  ;
                 SCHEME 8
          MODI COMM &NOMB NOEDIT WINDOW;
REPORT
          RELEASE WINDOW report
     ELSE
          SET PRINTER OFF
     ENDIF
ENDIF
SET CURSOR OFF
RETURN
*
PROCEDURE cabecera
PARAMETER destino
pagina = pagina + 1
IF destino = 2
     @ fil, 00 SAY CHR(27) + 'P' +  ;
       CHR(18)
     @ fil, 00 SAY rge_razsoc
     @ fil, 59 SAY 'PAGINA   : ' +  ;
       STR(pagina, 8, 0)
     fil = fil + 1
     @ fil, 00 SAY '       ' +  ;
       rge_abrev
     @ fil, 59 SAY 'FECHA    : ' +  ;
       DTOC(DATE())
     fil = fil + 1
     @ fil, 02 SAY rge_calle
     @ fil, 59 SAY 'PROGRAMA : ' +  ;
       wrk_prg
     fil = fil + 2
ELSE
     @ fil, 01 SAY rge_razsoc
     @ fil, 59 SAY 'PAGINA   : ' +  ;
       STR(pagina, 8, 0)
     fil = fil + 1
     @ fil, 00 SAY '       ' +  ;
       rge_abrev
     @ fil, 59 SAY 'FECHA    : ' +  ;
       DTOC(DATE())
     fil = fil + 1
     @ fil, 02 SAY rge_calle
     @ fil, 59 SAY 'PROGRAMA : ' +  ;
       wrk_prg
     fil = fil + 2
ENDIF
tit = ' RELACION  DE  TABLAS'
IF destino = 2
     @ fil, 00 SAY CHR(14) +  ;
       CHR(15)
ENDIF
@ fil, (80 - LEN(tit)) / 2 SAY  ;
  tit
fil = fil + 2
IF destino = 2
     @ fil, 00 SAY CHR(18)
ENDIF
@ fil, 05 SAY 'TABLA DE  ??  ' +  ;
  aux_des
fil = fil + 2
@ fil, 000 SAY '?' +  ;
  REPLICATE('?', 76) + '?'
fil = fil + 1
@ fil, 000 SAY '?'
@ fil, 005 SAY 'CODIGO'
@ fil, 015 SAY 'DESCRIPCION '
IF aux_fac <> 0
     @ fil, 060 SAY 'FACTOR'
ENDIF
@ fil, 077 SAY '?'
fil = fil + 1
@ fil, 00 SAY '?' + REPLICATE('?',  ;
  76) + '?'
SELECT 21
fil = fil + 2
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
