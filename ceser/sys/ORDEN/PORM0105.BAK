*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     ind_prg = '<PORM0105>'
     tit_prg = 'MANTENCION'
ELSE
     ind_prg = '<PORM0303>'
     tit_prg = 'CONSULTA'
ENDIF
wrk_progra = PROGRAM()
CLOSE DATABASES
DO crea_win
ON KEY LABEL F6 DO AYUDA02
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' REGISTRO CLIENTE/PROVEEDOR '
@ 3, 2 CLEAR TO 5, 77
@ 3, 2 TO 5, 77
@ 4, 5 SAY  ;
  'Indicador Entidad :    C¢digo Entidad :'
ppas = .T.
DO WHILE ppas
     @ 6, 2 CLEAR TO 13, 77
     @ 4, 57 SAY SPACE(15)
     @ 3, 2 TO 5, 77
     ind_modo = 'I'
     DO esc_modo WITH ind_modo
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
     wk_codent = SPACE(1)
     wk_codcli = 0
     wk_noment = SPACE(50)
     wk_nomcal = SPACE(50)
     wk_nomdis = SPACE(04)
     wk_nomciu = SPACE(04)
     wk_numte1 = 0
     wk_numte2 = 0
     wk_codcla = SPACE(4)
     efecin = 1
     @ 4, 25 GET wk_codent  ;
       PICTURE '!' VALID  ;
       val2_1('ENTI',wk_codent,0, ;
       0) WHEN colocaf6()
     @ 4, 45 GET wk_codcli  ;
       PICTURE '99999999999'  ;
       VALID codnum(wk_codcli)  ;
       WHEN colocaf6()
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
     USE SHARED ST_ICLPR ORDER  ;
         CODIGO
     SEEK wk_codent +  ;
          STR(wk_codcli, 11)
     IF  .NOT. FOUND() .AND.  ;
         config_prg <> 1
          USE
          DO error WITH  ;
             '** C¢digo Entidad NO EXISTE **'
          KEYBOARD '{CTRL+Y}'  ;
                   PLAIN
          LOOP
     ENDIF
     @ 5, 2 TO 13, 77
     @ 5, 2 SAY 'Ã'
     @ 5, 74 SAY '´'
     @ 06, 4 SAY  ;
       'Nombre o Raz¢n Social :'
     @ 07, 4 SAY  ;
       'Nombre de Calle       :'
     @ 08, 4 SAY  ;
       'Nombre Distrito       :'
     @ 09, 4 SAY  ;
       'Nombre Ciudad         :'
     @ 10, 4 SAY  ;
       'N£mero Tel‚fono 1     :'
     @ 11, 4 SAY  ;
       'N£mero Tel‚fono 2     :'
     @ 12, 4 SAY  ;
       'C¢digo Clasificaci¢n  :'
     IF FOUND()
          @ 4, 57 SAY  ;
            SUBSTR(noment, 1,  ;
            15)
          wk_noment = noment
          wk_nomcal = nomcal
          wk_nomdis = nomdis
          wk_nomciu = nomciu
          wk_numte1 = numte1
          wk_numte2 = numte2
          wk_codcla = codcla
     ELSE
          wk_aux = CHRSAW()
          DO WHILE wk_aux
               = INKEY()
               wk_aux = CHRSAW()
          ENDDO
          KEYBOARD '{F3}' PLAIN
     ENDIF
     USE
     @ 06, 28 SAY  ;
       SUBSTR(wk_noment, 1, 40)  ;
       PICTURE '@!'
     @ 07, 28 SAY  ;
       SUBSTR(wk_nomcal, 1, 40)  ;
       PICTURE '@!'
     @ 08, 28 SAY wk_nomdis  ;
       PICTURE '!!!!'
     @ 09, 28 SAY wk_nomciu  ;
       PICTURE '!!!!'
     @ 10, 28 SAY wk_numte1  ;
       PICTURE '99999999'
     @ 11, 28 SAY wk_numte2  ;
       PICTURE '99999999'
     @ 12, 28 SAY wk_codcla  ;
       PICTURE '!!!!'
     wk_clave = 'DIST' +  ;
                wk_nomdis
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_clave'
     @ 08, 33 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     wk_clave = 'PROV' +  ;
                wk_nomciu
     SEEK '&wk_clave'
     @ 09, 33 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     codaux = 'ENTI' + wk_codcla
     SEEK '&CODAUX'
     @ 12, 33 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     USE
     key = INKEY()
     DO WHILE key<>27 .AND. key<>- ;
        9 .AND. (key<>-2 .AND.  ;
        config_prg==1) .AND. (key<>- ;
        3 .AND. config_prg==1)
          IF key == 0
               key = 255
          ENDIF
          DO esc_modo WITH 'C'
          IF config_prg == 1
               DO esc_indica WITH  ;
                  1, 'AYU', 'MOD',  ;
                  'BBB', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'ELI',  ;
                  'IGN', 'ESC'
          ELSE
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'BBB', 'BBB'
               DO esc_indica WITH  ;
                  2, 'BBB', 'BBB',  ;
                  'IGN', 'ESC'
          ENDIF
          key = INKEY(0)
     ENDDO
     IF key == -9
          LOOP
     ENDIF
     IF key == 27
          ppas = .F.
          LOOP
     ENDIF
     DO CASE
          CASE key == -2
               DO esc_modo WITH  ;
                  'M'
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'BBB', 'INT'
               DO esc_indica WITH  ;
                  2, 'RAC', 'BBB',  ;
                  'IGN', 'ESC'
               @ 06, 28 GET  ;
                 wk_noment  ;
                 FUNCTION 'S40'  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 codalf(wk_noment)
               @ 07, 28 GET  ;
                 wk_nomcal  ;
                 FUNCTION 'S40'  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 codalf(wk_nomcal)
               @ 08, 28 GET  ;
                 wk_nomdis  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('DIST', ;
                 wk_nomdis,37,30)  ;
                 WHEN colocaf6()
               @ 09, 28 GET  ;
                 wk_nomciu  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('PROV', ;
                 wk_nomciu,37,30)  ;
                 WHEN colocaf6()
               @ 10, 28 GET  ;
                 wk_numte1  ;
                 PICTURE  ;
                 '99999999'
               @ 11, 28 GET  ;
                 wk_numte2  ;
                 PICTURE  ;
                 '99999999'
               @ 12, 28 GET  ;
                 wk_codcla  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('CATC', ;
                 wk_codcla,37,30)  ;
                 WHEN colocaf6()
               SET CURSOR ON
               READ
               SET CURSOR OFF
          CASE key == -3
               DO esc_modo WITH  ;
                  'E'
               DO error WITH  ;
                  '** Confirma Eliminaci¢n [ ÄÙ ] **'
               IF LASTKEY() = 13
                    procodx = wk_codent +  ;
                              STR(wk_codcli,  ;
                              11)
                    SELECT 8
                    USE SHARED  ;
                        ST_ICLPR  ;
                        ORDER  ;
                        CODIGO
                    SEEK '&PROCODX'
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              DELETE
                              UNLOCK
                              EXIT
                         ELSE
                              sw_sn =  ;
                               f_yesno1( ;
                               'Registro Bloqueado, Reintentar' ;
                               )
                         ENDIF
                    ENDDO
                    SELECT 8
                    USE
               ENDIF
               LOOP
     ENDCASE
     IF config_prg == 2
          DO esc_modo WITH 'C'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'GRA', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     keyx = LASTKEY()
     DO WHILE keyx<>27 .AND. keyx<>- ;
        9 .AND. keyx<>-1
          keyx = INKEY(0)
     ENDDO
     IF keyx == -9
          LOOP
     ENDIF
     IF keyx == 27
          IF efecin = 1
               ppas = .F.
               LOOP
          ELSE
               LOOP
          ENDIF
     ENDIF
     IF keyx == -1
          DO CASE
               CASE key == -2
                    procodx = wk_codent +  ;
                              STR(wk_codcli,  ;
                              11)
                    USE SHARED  ;
                        ST_ICLPR  ;
                        ORDER  ;
                        CODIGO
                    SEEK '&PROCODX'
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   APPEND BLANK
                                   REPLACE indent WITH wk_codent, codent WITH STR(wk_codcli, 11)
                              ENDIF
                              REPLACE  ;
                               noment  ;
                               WITH  ;
                               wk_noment,  ;
                               nomcal  ;
                               WITH  ;
                               wk_nomcal,  ;
                               nomdis  ;
                               WITH  ;
                               wk_nomdis,  ;
                               nomciu  ;
                               WITH  ;
                               wk_nomciu,  ;
                               numte1  ;
                               WITH  ;
                               wk_numte1,  ;
                               numte2  ;
                               WITH  ;
                               wk_numte2,  ;
                               codcla  ;
                               WITH  ;
                               wk_codcla
                              UNLOCK
                              USE
                              EXIT
                         ELSE
                              sw_sn =  ;
                               f_yesno1( ;
                               'Registro Bloqueado, Reintentar' ;
                               )
                         ENDIF
                    ENDDO
          ENDCASE
     ENDIF
ENDDO
DO saca_win
ON KEY LABEL F6
ON KEY LABEL F10
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
IF ROW() == 04 .AND. COL() > 30
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SET FILTER TO indent == wk_codent
     GOTO TOP
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE ENTIDADES'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     USE
ENDIF
IF ROW() == 08
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'DIST'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE DISTRITOS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 09
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'PROV'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE PROVINCIAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 12
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'CATC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE CLASIFICACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
ON KEY LABEL F6 do ayuda02
RETURN
*
FUNCTION val2_1
PARAMETER clave, codig, colu,  ;
          largo
CLEAR TYPEAHEAD
IF codig = 'C' .OR. codig = 'P'
     SELECT 7
     USE SHARED ge_tab0 ORDER  ;
         codigo
     codaux = clave + codig
     SEEK '&codaux'
     IF  .NOT. FOUND()
          USE
          DO error WITH  ;
             '** Codigo NO EXISTE **'
          RETURN .F.
     ENDIF
     IF colu <> 0
          @ ROW(), colu SAY  ;
            SUBSTR(tab_destab, 1,  ;
            largo)
     ENDIF
     USE
     SELECT 1
ELSE
     DO error WITH  ;
        '** Solo Clientes o Proveedores **'
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
