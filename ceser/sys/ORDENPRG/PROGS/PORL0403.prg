*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORL0403>'
tit_prg = 'INFORMES'
CLOSE DATABASES
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA02
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' INFORME CLIENTE / PROVEEDOR'
@ 3, 2 CLEAR TO 7, 76
@ 3, 2 TO 7, 76
@ 4, 5 SAY 'Tipo De   Entidad :'
@ 5, 5 SAY 'Desde La  Entidad :'
@ 6, 5 SAY 'Hasta La  Entidad :'
pas403 = da_nombre()
Create table &PAS403 (pas_numrut C(9),pas_nombre;
C(30),pas_clasi C(4),pas_desclas C(35))
SELECT 1
use &PAS403 EXCLUSIVE  
ppas = .T.
wk_hay = .F.
DO WHILE ppas
     @ 4, 25 SAY SPACE(35)
     @ 6, 25 SAY SPACE(40)
     @ 5, 25 SAY SPACE(40)
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     STORE SPACE(1) TO wk_enti
     STORE 0 TO wk_codent,  ;
           wk_codent2, cod
     wk_codmod = SPACE(15)
     wk_nommod = SPACE(30)
     wk_codcla = SPACE(04)
     wk_codent = 0
     wk_fecori = DATE()
     efecin = 1
     @ 4, 25 GET wk_enti PICTURE  ;
       '!' VALID valtab('ENTI', ;
       wk_enti,27,34) WHEN  ;
       colocaf6()
     @ 5, 25 GET wk_codent  ;
       PICTURE '99999999999'  ;
       VALID entra1_1(wk_codent)  ;
       WHEN colocaf6()
     @ 6, 25 GET wk_codent2  ;
       PICTURE '99999999999'  ;
       VALID entra2_2(wk_codent2, ;
       wk_codent) WHEN  ;
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
     IF wk_enti = 'P'
          pregunta = 'indent="P".and. Codent>=Str(wk_codent,11).and. Codent<=Str(wk_codent2,11)'
     ELSE
          pregunta = 'indent="C".and. Codent>=Str(wk_codent,11).and. Codent<=Str(wk_codent2,11)'
     ENDIF
     DO vali
     IF wk_hay = .F.
          DO error WITH  ;
             '*** No Ninguna Entidad ***'
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
          SELECT 1
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
SELECT 1
USE
CLOSE DATABASES
x403 = pas403 + '.DBF'
erase &X403  
x403 = pas403 + '.IDX'
erase &X403  
RETURN
*
PROCEDURE ayuda02
ON KEY LABEL F6
IF ROW() == 04 .AND. COL() < 30
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'ENTI'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE ENTIDADES'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF (ROW() == 05 .OR. ROW() == 06)  ;
   .AND. COL() > 10
     SELECT 5
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SET FILTER TO indent == wk_enti
     GOTO TOP
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE ENTIDADES'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
     SELECT 5
     USE
ENDIF
ON KEY LABEL F6 do ayuda02
RETURN
*
FUNCTION entra1_1
PARAMETER cod
ACTIVATE WINDOW trabajo
SELECT 4
USE SHARED st_iclpr ORDER CODIGO
SET ORDER TO 1
IF EMPTY(cod)
     GOTO TOP
     DO WHILE  .NOT. EOF()
          IF wk_enti = indent
               cod = VAL(codent)
               EXIT
          ENDIF
          SKIP
     ENDDO
ENDIF
entaux = wk_enti + STR(cod, 9)
SEEK '&ENTAUX'
IF  .NOT. FOUND()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
@ 5, 36 SAY SUBSTR(noment, 1, 40)
wk_codent = cod
SELECT 4
USE
RETURN .T.
*
FUNCTION entra2_2
PARAMETER cod, cc
IF LASTKEY() == 5
     RETURN .T.
ENDIF
SELECT 4
USE SHARED st_iclpr ORDER CODIGO
SET ORDER TO 1
GOTO TOP
IF EMPTY(cod)
     GOTO BOTTOM
     SKIP -1
     DO WHILE  .NOT. BOF()
          IF wk_enti = indent
               cod = VAL(codent)
               EXIT
          ENDIF
          SKIP -1
     ENDDO
ENDIF
entaux = wk_enti + STR(cod, 9)
SEEK '&ENTAUX'
IF  .NOT. FOUND()
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '** C¢digo No Existente **'
     RETURN .F.
ENDIF
IF cod < cc
     ACTIVATE WINDOW trabajo
     DO error WITH  ;
        '**C¢digo debe ser Mayor **'
     KEYBOARD CHR(5) + '{CTRL+Y}'
     RETURN .F.
ENDIF
wk_codent2 = cod
@ 6, 36 SAY SUBSTR(noment, 1, 40)
SELECT 4
USE
RETURN .T.
*
PROCEDURE vali
PRIVATE wk_marca, wk_fecemi,  ;
        wk_hora
wk_code = SPACE(9)
wk_marca = SPACE(4)
wk_mode = SPACE(15)
wk_numser = SPACE(20)
STORE 0 TO wk_garanti
STORE CTOD('  /  /  ') TO  ;
      wk_fecompr, wk_feingre
SELECT 1
Use &PAS403 EXCLUSIVE      
SELECT 3
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iclpr ORDER CODIGO
GOTO TOP
DO WHILE  .NOT. EOF()
     IF &pregunta
          wk_code = codent
          wk_noment = noment
          wk_clasi = codcla
          SELECT 3
          GOTO TOP
          codaux = 'CATC' +  ;
                   wk_clasi
          SEEK '&codaux'
          wk_desclas = tab_destab
          SELECT 1
          APPEND BLANK
          REPLACE pas_numrut WITH  ;
                  wk_code
          REPLACE pas_nombre WITH  ;
                  wk_noment
          REPLACE pas_clasi WITH  ;
                  wk_clasi
          REPLACE pas_descla WITH  ;
                  wk_desclas
          wk_hay = .T.
     ENDIF
     SELECT 2
     SKIP
ENDDO
acum = 0
IF wk_hay = .T.
     SELECT 1
     GOTO TOP
     Index on pas_numrut to &PAS403;
         
ENDIF
SELECT 3
USE
SELECT 2
USE
RETURN
*
PROCEDURE liste
tit1 = 'CODIGO        NOMBRE/RAZON SOCIAL                    CLASIFICACION ENTIDAD      '
con_lin = 11
STORE 0 TO tot_mode, sum_gen,  ;
      sum_estado, pag, wk_garanti,  ;
      wk_garmar
STORE SPACE(4) TO wk_antiguo,  ;
      wk_clasi
STORE SPACE(15) TO wk_anti1
STORE SPACE(40) TO wk_des1,  ;
      wk_des2, wk_desmarc, tipo
wk_fecha = CTOD('  /  /  ')
SET PRINTER ON
SET DEVICE TO PRINTER
SET CONSOLE OFF
? CHR(18)
SELECT 1
Set index to &PAS403    
GOTO TOP
DO pie WITH pag, tit1, ' ',  ;
   'INFORME DE ENTIDADES',  ;
   'CLIENTE/PROVEEDOR'
DO WHILE  .NOT. EOF()
     IF con_lin = 50
          EJECT
          DO pie WITH pag, tit1,  ;
             ' ',  ;
             'INFORME DE ENTIDADES',  ;
             'CLIENTE/PROVEEDOR'
          con_lin = 11
     ENDIF
     con_lin = con_lin + 1
     @ con_lin, 1 SAY pas_numrut  ;
       PICTURE '@!'
     @ con_lin, 11 SAY pas_nombre  ;
       PICTURE '@!'
     @ con_lin, 45 SAY pas_clasi  ;
       PICTURE '!!!!'
     @ con_lin, 50 SAY  ;
       SUBSTR(pas_descla, 1, 29)
     sum_gen = sum_gen + 1
     SKIP
ENDDO
con_lin = con_lin + 2
@ con_lin, 45 SAY  ;
  'TOTAL ENTIDADES ===>'
@ con_lin, 70 SAY sum_gen PICTURE  ;
  '99,999,999'
EJECT
SET PRINTER TO
SET CONSOLE ON
SET DEVICE TO SCREEN
SET PRINTER OFF
SELECT 1
use &PAS403 EXCLUSIVE     
set index to &PAS403      
ZAP
RETURN
*
PROCEDURE pie
PARAMETER pag, titu1, tit2, pie1,  ;
          pie2
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
@ 4, 20 SAY 'TIPO ENTIDAD : ' +  ;
  wk_enti
@ 07, 0 SAY REPLICATE('=', 80)
@ 08, 0 SAY titu1
@ 09, 0 SAY REPLICATE('=', 80)
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
