*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0418>'
tit_prg = 'INFORMES'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INFORME DE MODELOS/SINTOMAS'
@ 3, 2 TO 7, 76
@ 4, 5 SAY 'Desde La  Marca   :'
@ 6, 5 SAY 'Hasta La  Marca   :'
SELECT 1
USE st_ifall INDEX st_1fall,  ;
    st_2fall
CREATE DBF pas_arte.dbf  ;
       (pas_marca C (4),  ;
       pas_desmar C (35),  ;
       pas_mode C (15),  ;
       pas_desmod C (30),  ;
       pas_monto N (9))
SELECT 2
USE pas_arte.dbf
ppas = .T.
wk_hay = .F.
DO WHILE ppas
     @ 7, 1 CLEAR TO 13, 76
     @ 4, 30 SAY SPACE(35)
     @ 6, 30 SAY SPACE(35)
     @ 5, 41 SAY SPACE(30)
     @ 3, 2 TO 7, 76
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     STORE SPACE(4) TO wk_codmar,  ;
           wk_codmar2
     wk_codmod = SPACE(15)
     wk_nommod = SPACE(30)
     wk_codcla = SPACE(04)
     wk_mesgar = 0
     wk_codent = 0
     wk_fecori = DATE()
     efecin = 1
     @ 4, 25 GET wk_codmar  ;
       PICTURE '!!!!' VALID  ;
       entra1_1(wk_codmar,'MARC')  ;
       WHEN colocaf6()
     @ 6, 25 GET wk_codmar2  ;
       PICTURE '!!!!' VALID  ;
       entra2_2(wk_codmar2, ;
       wk_codmar,'MARC') WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     DO vali
     IF wk_hay = .F.
          DO error WITH  ;
             '*** No Existe Ning£na Marca ***'
          LOOP
     ENDIF
     key = INKEY()
     DO WHILE key<>27 .AND. key<>- ;
        9 .AND. key<>-6
          IF key == 0
               key = 255
          ENDIF
          DO esc_modo WITH 'S'
          DO esc_indica WITH 1,  ;
             'AYU', 'IMP', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
          key = INKEY(0)
     ENDDO
     IF key == -6
          DO liste
          LOOP
     ENDIF
     IF key == -9
          SELECT 2
          ZAP
          LOOP
     ENDIF
     IF key == 27
          ppas = .F.
          LOOP
     ENDIF
ENDDO
DO saca_win
@ 24, 69 SAY SPACE(10)
ON KEY LABEL F6
ON KEY LABEL F10
SELECT 2
USE
ERASE pas_arte.dbf
ERASE paso.idx
RETURN
*
PROCEDURE ayuda01
ON KEY LABEL F6
IF ROW() == 4 .OR. ROW() == 6
     USE st_itabl INDEX st_1tabl
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_descri'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
ON KEY LABEL F6 do ayuda01
RETURN
*
FUNCTION entra1_1
PARAMETER cod, cc
SELECT 4
USE ST_ITABL INDEX ST_1TABL
IF cod == SPACE(4)
     entaux = cc
     FIND '&ENTAUX'
     aux = tab_codtab
     KEYBOARD '{CTRL+Y}' + aux  ;
              PLAIN
     RETURN .F.
ENDIF
entaux = cc + cod
FIND '&ENTAUX'
IF EOF()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
@ 4, 30 SAY SUBSTR(tab_descri, 1,  ;
  40)
SELECT 4
USE
RETURN .T.
*
FUNCTION entra2_2
PARAMETER cod, codx, cc
IF LASTKEY() == 5
     RETURN .T.
ENDIF
SELECT 4
USE st_itabl INDEX st_1tabl
IF cod == SPACE(4)
     entaux = cc
     FIND '&ENTAUX'
     DO WHILE tab_codpre==cc
          SKIP
     ENDDO
     SKIP -1
     aux = tab_codtab
     KEYBOARD '{CTRL+Y}' + aux  ;
              PLAIN
     RETURN .F.
ENDIF
entaux = cc + cod
FIND '&ENTAUX'
IF EOF()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
IF cod < codx
     DO error WITH  ;
        '** C¢digo debe ser Mayor **'
     RETURN .F.
ENDIF
@ 6, 30 SAY SUBSTR(tab_descri, 1,  ;
  40)
SELECT 4
USE
RETURN .T.
*
PROCEDURE vali
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_solici = SPACE(8)
wk_marca = SPACE(4)
wk_mode = SPACE(15)
wk_numser = SPACE(20)
STORE 0 TO wk_garanti
STORE CTOD('  /  /  ') TO  ;
      wk_fecompr, wk_feingre
SELECT 2
USE pas_arte.dbf
SELECT 3
USE st_itabl INDEX st_1tabl
SELECT 1
GOTO TOP
DO WHILE  .NOT. EOF()
     IF codmar >= wk_codmar .AND.  ;
        codmar <= wk_codmar2
          wk_marca = codmar
          wk_mode = codfal
          wk_nommod = desfal
          wk_monto = monfal
          SELECT 3
          GOTO TOP
          codaux = 'MARC' +  ;
                   wk_marca
          find '&codaux'
          wk_desmarc = tab_descri
          SELECT 2
          APPEND BLANK
          REPLACE pas_marca WITH  ;
                  wk_marca
          REPLACE pas_desmar WITH  ;
                  wk_desmarc
          REPLACE pas_mode WITH  ;
                  wk_mode
          REPLACE pas_desmod WITH  ;
                  wk_nommod
          REPLACE pas_monto WITH  ;
                  wk_monto
          wk_hay = .T.
     ENDIF
     SELECT 1
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 2
     GOTO TOP
     INDEX ON pas_marca +  ;
           pas_mode TO paso.idx
ENDIF
SELECT 3
USE
RETURN
*
PROCEDURE liste
tit1 = ' CODIGO              DESCRIPCION FALLA                                 VALOR $ '
con_lin = 09
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag, wk_garanti,  ;
      wk_garmar
STORE SPACE(4) TO wk_antiguo,  ;
      wk_clasi
STORE SPACE(15) TO wk_anti1
STORE SPACE(40) TO wk_des1,  ;
      wk_des2, wk_desmarc, tipo
wk_fecha = CTOD('  /  /  ')
sw_impre = 0
DO impresora WITH sw_impre
IF sw_impre <> 1
     RETURN
ENDIF
DO esc_modo WITH 'P'
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
SELECT 2
SET INDEX TO paso.idx
GOTO TOP
DO pie WITH pag, tit1, ' ',  ;
   'INFORME DE TARIFAS', ' '
DO WHILE  .NOT. EOF()
     IF con_lin = 50
          EJECT
          DO pie WITH pag, tit1,  ;
             ' ',  ;
             'INFORME DE TARIFAS',  ;
             ' '
          con_lin = 09
     ENDIF
     IF pas_marca <> wk_antiguo
          IF  .NOT.  ;
              EMPTY(wk_antiguo)
               wk_garmar = 0
          ENDIF
          con_lin = con_lin + 2
          @ con_lin, 1 SAY  ;
            'MARCA...:' +  ;
            pas_marca + '  ' +  ;
            pas_desmar
     ENDIF
     con_lin = con_lin + 1
     @ con_lin, 2 SAY pas_mode  ;
       PICTURE '!!!!'
     @ con_lin, 19 SAY pas_desmod  ;
       PICTURE '@!'
     @ con_lin, 69 SAY pas_monto  ;
       PICTURE '99,999,99.99'
     tipo = wk_des1
     wk_garmar = wk_garmar + 1
     wk_antiguo = pas_marca
     wk_desmarc = pas_desmar
     wk_anti1 = pas_mode
     wk_des1 = pas_desmod
     SKIP
ENDDO
EJECT
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 2
USE pas_arte
SET INDEX TO paso
ZAP
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, wk_fech,  ;
          pie1, pie2
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
centro = INT((80 - LEN(pie2)) /  ;
         2)
centro2 = centro - 4
centro3 = centro + 2
@ 3, centro SAY pie2
@ 3, 60 SAY 'PROGRAMA : ' +  ;
  SUBSTR(ind_prg, 2, 8)
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY REPLICATE('=', 80)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
