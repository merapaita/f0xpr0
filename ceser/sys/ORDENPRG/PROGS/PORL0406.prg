*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0406>'
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INFORME DE CATALOGO DE FALLAS'
ppas = .T.
DO WHILE ppas
     @ 7, 1 CLEAR TO 13, 77
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'IMP', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     DEFINE WINDOW cata FROM 8,  ;
            18 TO 12, 62 SHADOW
     ACTIVATE WINDOW cata
     @ 0, 2 SAY  ;
       ' Sr(a).Usuario: '
     @ 1, 2 SAY  ;
       'Al Digitar < F7 > Se Procedera a listar'
     @ 2, 2 SAY  ;
       'El Catalogo de Fallas .'
     = INKEY(0)
     DO WHILE LASTKEY()<>27 .AND.  ;
        LASTKEY()<>-6
          = INKEY(0)
     ENDDO
     DEACTIVATE WINDOW cata
     RELEASE WINDOW cata
     IF LASTKEY() == -6
          DO lis_analis
          LOOP
     ENDIF
     IF LASTKEY() == 27
          ppas = .F.
          LOOP
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL F6
CLOSE DATABASES
RETURN
*
PROCEDURE lis_analis
STORE SPACE(4) TO vie_marca,  ;
      vie_indori
STORE SPACE(15) TO vie_mode
STORE 0 TO wk_contado, acum
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag,  ;
      wk_garanti
STORE 0 TO wk_canti, wk_garmar,  ;
      tope
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
tit1 = '              CODIGO            DESCRIPCION DE FALLA        '
con_lin = 12
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
DO pie WITH pag, tit1,  ;
   'INFORME CATALOGO DE FALLAS'
SELECT 1
USE SHARED GE_TAB0 ORDER CODIGO
GOTO TOP
LOCATE FOR tab_codpre = 'FALL'
wk_tab = tab_codpre
DO WHILE tab_codpre=wk_tab
     @ con_lin, 10 SAY tab_codtab
     @ con_lin, 30 SAY tab_destab
     con_lin = con_lin + 1
     IF con_lin = 50
          EJECT
          DO pie WITH pag, tit1,  ;
             'INFORME CATALOGO DE FALLAS'
          con_lin = 12
     ENDIF
     SKIP
ENDDO
USE
EJECT
? CHR(18)
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, pie1
PRIVATE centro
centro = 0
pag = pag + 1
@ 1, 0 SAY empre1
@ 1, 60 SAY 'PAGINA   : ' +  ;
  STR(pag, 8)
@ 2, 0 SAY empre2
centro = INT((80 - LEN(pie1)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 2, centro SAY pie1
@ 2, 60 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 10, 0 SAY REPLICATE('=', 80)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
