*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0401>'
tit_2 = 'INFORME GENERAL DE TABLAS'
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2, tit_2
imprime = .T.
STORE ' ' TO paso1, paso2, paso3
DO WHILE imprime
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 4, 10, 15, 66 BOX  ;
       'ÉÍ»º¼ÍÈº '
     @ 5, 12 SAY  ;
       ' C¢digo              Descripci¢n'
     @ 6, 11 SAY REPLICATE('Ä',  ;
       55)
     DO saycenter WITH 10,  ;
        'Buscando Informaci¢n Requerida'
     paso1 = da_nombre()
     Create table &PASO1 (tab_codpre C(4),tab_codtab;
c(4),tab_descri C(30),tab_factor n(6),tab_estado;
c(1))
     USE
     SELECT 2
     use &PASO1
     SELECT 1
     USE SHARED ge_tab0 ORDER  ;
         codigo
     GOTO TOP
     DO WHILE  .NOT. EOF()
          wk_codtab = tab_codtab
          wk_codpre = tab_codpre
          wk_destab = tab_destab
          SELECT 2
          APPEND BLANK
          REPLACE tab_codpre WITH  ;
                  wk_codpre
          REPLACE tab_codtab WITH  ;
                  wk_codtab
          REPLACE tab_descri WITH  ;
                  wk_destab
          SELECT 1
          SKIP
     ENDDO
     SELECT 2
     USE
     SELECT 1
     USE
     SELECT 1
     use &PASO1 EXCLUSIVE
     index on tab_codpre+tab_codtab to;
&PASO1
     USE
     SELECT 1
     USE &PASO1 INDEX &PASO1  EXCLUSIVE
     paso2 = da_nombre()
     COPY TO &PASO2 FOR TAB_CODPRE ==;
'TABL'
     USE &PASO2 EXCLUSIVE
     REPLACE tab_estado WITH ' '  ;
             ALL
     GOTO TOP
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'INI', 'ANT', 'OTR'
     DO esc_indica WITH 2, 'MBV',  ;
        'FIN', 'STE', 'ESC'
     dbaux = 0
     rgaux = 0
     campo = 'TAB_ESTADO + " " + TAB_CODTAB + " " + TAB_DESCRI'
     DO dbedit WITH 7, 12, 13, 64,  ;
        campo, 'MFUNrq()'
     IF LASTKEY() <> 27
          imprime = .T.
     ENDIF
     IF LASTKEY() = 27
          imprime = .F.
     ENDIF
     IF LASTKEY() <> 27 .AND.  ;
        rgaux <> 0
          imprime = .T.
          DO esc_modo WITH 'P'
          DO esc_indica WITH 1,  ;
             'BBB', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'BBB'
          busca = ''
          GOTO TOP
          DO WHILE  .NOT. EOF()
               IF tab_estado ==  ;
                  CHR(251)
                    busca = busca +  ;
                            tab_codtab +  ;
                            ';'
               ENDIF
               SKIP
          ENDDO
          ZAP
          USE
          SELECT 2
          USE &PASO1 INDEX &PASO1 EXCLUSIVE
          paso3 = da_nombre()
          COPY TO &PASO3 FOR TAB_CODPRE$BUSCA
          SELECT 1
          use &PASO3 EXCLUSIVE;
   
          IF  .NOT. EOF()
               DO lista01
               SET PRINTER TO
               SET CONSOLE ON
               SET DEVICE TO SCREEN
               SET PRINTER OFF
               DO esc_modo WITH  ;
                  'S'
          ENDIF
          CLOSE DATABASES
     ENDIF
ENDDO
CLOSE DATABASES
x401 = paso1 + '.DBF'
ERASE &X401
x401 = paso2 + '.DBF'
ERASE &X402
IF  .NOT. EMPTY(paso3)
     x401 = paso3 + '.DBF'
     ERASE &X403
ENDIF
RELEASE paso1, paso2, paso3
DO saca_win
RETURN
*
PROCEDURE lista01
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
?? CHR(18)
lin = 0
pag = 0
cla = SPACE(4)
SELECT 1
DO titu01
DO WHILE  .NOT. EOF()
     IF cla <> tab_codpre
          cla = tab_codpre
          xxx = 'TABL' +  ;
                tab_codpre
          SELECT 2
          FIND '&XXX'
          @ lin, 2 SAY tab_codtab
          @ lin, 15 SAY  ;
            tab_descri
          SELECT 1
          lin = lin + 1
     ENDIF
     @ lin, 6 SAY tab_codtab
     @ lin, 15 SAY tab_descri
     @ lin, 75 SAY tab_estado
     lin = lin + 1
     SKIP
     IF lin > 60 .AND.  .NOT.  ;
        EOF()
          EJECT
          DO titu01
     ENDIF
ENDDO
EJECT
?? CHR(18)
RETURN
*
PROCEDURE titu01
pag = pag + 1
@ 1, 0 SAY empre1
@ 1, 60 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
@ 2, (40 - (LEN(tit_2) / 2)) SAY  ;
  tit_2
@ 2, 60 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 05, 0 SAY REPLICATE('=', 80)
@ 06, 0 SAY  ;
  '  CODIGO                          DESCRIPCION                           '
@ 07, 0 SAY REPLICATE('=', 80)
lin = 08
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
