*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     tit_prg = ' MANTENCION '
ELSE
     tit_prg = ' CONSULTA '
ENDIF
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' MAESTRO DE MODELOS '
@ 3, 2 CLEAR TO 6, 77
@ 3, 2 TO 6, 77
@ 4, 5 SAY 'Codigo de Marca   :'
@ 5, 5 SAY 'Codigo de Modelo  :'
ppas = .T.
DO WHILE ppas
     CLOSE DATABASES
     @ 4, 30 SAY SPACE(30)
     @ 5, 41 SAY SPACE(30)
     @ 7, 2 CLEAR TO 13, 77
     @ 6, 2 SAY 'À'
     @ 6, 74 SAY 'Ù'
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
     wk_codmar = SPACE(4)
     wk_codmod = SPACE(15)
     wk_nommod = SPACE(30)
     wk_codcla = SPACE(04)
     wk_codlin = SPACE(04)
     wk_mesgar = 0
     wk_codent = 0
     wk_fecori = DATE()
     efecin = 1
     @ 4, 25 GET wk_codmar  ;
       PICTURE '@!' VALID  ;
       valtab9('MARC',wk_codmar, ;
       30,30) WHEN colocaf6()
     @ 5, 25 GET wk_codmod  ;
       PICTURE '@!' VALID  ;
       codalf(wk_codmod) WHEN  ;
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
     USE SHARED st_imode ORDER  ;
         CODIGO
     SEEK wk_codmar + wk_codmod
     IF  .NOT. FOUND() .AND.  ;
         config_prg <> 1
          USE
          DO error WITH  ;
             '** Codigo Codigo Modelo NO EXISTE **'
          LOOP
     ENDIF
     @ 06, 02 CLEAR TO 13, 77
     @ 06, 02 TO 13, 77
     @ 06, 02 SAY 'Ã'
     @ 06, 74 SAY '´'
     @ 07, 08 SAY  ;
       'Nombre del Modelo     :'
     @ 08, 08 SAY  ;
       'Codigo Clasificacion  :'
     @ 09, 08 SAY  ;
       'Codigo L¡nea          :'
     @ 10, 08 SAY  ;
       'Mes de Garantia       :'
     @ 11, 08 SAY  ;
       'Codigo Proveedor      :'
     IF FOUND()
          @ 05, 41 SAY  ;
            SUBSTR(nommod, 1,  ;
            30)
          wk_nommod = nommod
          wk_codcla = codcla
          wk_mesgar = mesgar
          wk_codent = VAL(codent)
          wk_fecori = fecori
          wk_codlin = linea
     ELSE
          wk_aux = CHRSAW()
          DO WHILE wk_aux
               = INKEY()
               wk_aux = CHRSAW()
          ENDDO
          KEYBOARD '{F3}' PLAIN
     ENDIF
     USE
     @ 07, 32 SAY wk_nommod  ;
       PICTURE '@!'
     @ 08, 32 SAY wk_codcla  ;
       PICTURE '@!'
     @ 09, 32 SAY wk_codlin  ;
       PICTURE '@!'
     @ 10, 32 SAY wk_mesgar  ;
       PICTURE '99'
     @ 11, 32 SAY wk_codent  ;
       PICTURE '99999999999'
     USE SHARED ge_tab0 ORDER  ;
         codigo
     codaux = 'CLAS' + wk_codcla
     SEEK '&CODAUX'
     @ 08, 37 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     SEEK 'LINE' + wk_codlin
     IF FOUND()
          @ 09, 37 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            30)
     ELSE
          @ 09, 37 SAY ' '
     ENDIF
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     codaux = 'P' + STR(wk_codent,  ;
              11)
     SEEK '&codaux'
     @ 11, 44 SAY SUBSTR(noment,  ;
       1, 30)
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
          @ 7, 3 CLEAR TO 11, 75
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
               @ 07, 32 GET  ;
                 wk_nommod  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 codalf(wk_nommod)
               @ 08, 32 GET  ;
                 wk_codcla  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 valtab3('CLAS', ;
                 wk_codcla,37,30)  ;
                 WHEN colocaf6()
               @ 09, 32 GET  ;
                 wk_codlin  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 valtab3('LINE', ;
                 wk_codlin,37,30)  ;
                 WHEN colocaf6()
               @ 10, 32 GET  ;
                 wk_mesgar  ;
                 PICTURE '99'
               @ 11, 32 GET  ;
                 wk_codent  ;
                 PICTURE  ;
                 '99999999999'  ;
                 VALID  ;
                 provee2(wk_codent, ;
                 44) WHEN  ;
                 colocaf6()
               SET CURSOR ON
               READ
               SET CURSOR OFF
          CASE key == -3
               DO esc_modo WITH  ;
                  'E'
               DO error WITH  ;
                  '** Confirma Eliminaci¢n [ ÄÙ ] **'
               IF LASTKEY() = 13
                    procodx = wk_codmar +  ;
                              wk_codmod
                    USE SHARED  ;
                        st_imode  ;
                        ORDER  ;
                        CODIGO
                    SEEK '&procodx'
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              DELETE
                              UNLOCK
                              USE
                              EXIT
                         ELSE
                              sw_sn =  ;
                               f_yesno1( ;
                               'Registro Bloquedo, Reintentar' ;
                               )
                         ENDIF
                    ENDDO
               ENDIF
               @ 7, 3 CLEAR TO 11,  ;
                 75
               LOOP
          CASE key == -3
               DO esc_modo WITH  ;
                  'E'
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
          @ 7, 3 CLEAR TO 11, 75
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
                    procodx = wk_codmar +  ;
                              wk_codmod
                    USE SHARED  ;
                        st_imode  ;
                        ORDER  ;
                        CODIGO
                    SEEK '&procodx'
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              IF   ;
                               .NOT.  ;
                               FOUND()
                                   APPEND BLANK
                                   REPLACE codmar WITH wk_codmar, codmod WITH wk_codmod, linea WITH wk_codlin
                              ENDIF
                              REPLACE  ;
                               nommod  ;
                               WITH  ;
                               wk_nommod,  ;
                               codcla  ;
                               WITH  ;
                               wk_codcla,  ;
                               mesgar  ;
                               WITH  ;
                               wk_mesgar
                              REPLACE  ;
                               fecori  ;
                               WITH  ;
                               wk_fecori,  ;
                               linea  ;
                               WITH  ;
                               wk_codlin,  ;
                               codent  ;
                               WITH  ;
                               STR(wk_codent,  ;
                               11)
                              REPLACE  ;
                               user  ;
                               WITH  ;
                               users,  ;
                               date  ;
                               WITH  ;
                               DATE(),  ;
                               time  ;
                               WITH  ;
                               TIME()
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
               CASE key == -3
                    procodx = wk_codmar +  ;
                              wk_codmod
                    USE SHARED  ;
                        st_imode  ;
                        ORDER  ;
                        CODIGO
                    SEEK '&procodx'
                    sw_sn = .T.
                    DO WHILE  ;
                       sw_sn
                         IF RLOCK()
                              SET DELETED;
ON
                              DELETE
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
     @ 7, 3 CLEAR TO 11, 75
ENDDO
DO saca_win
ON KEY LABEL F6
ON KEY LABEL F10
RETURN
*
PROCEDURE ayuda01
ON KEY LABEL F6
IF ROW() == 4
     USE SHARED ge_tab0 ORDER  ;
         descri
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 5
     wrk_select = SELECT()
     SELECT 11
     USE SHARED st_imode ORDER  ;
         CODIGO
     SET FILTER TO codmar == wk_codmar
     wrk_selpro = SELECT()
     wrk_campo = st_imode.codmar
     DO pro2 WITH wrk_campo,  ;
        wrk_select, wrk_selpro,  ;
        1
     IF LASTKEY() <> 27
          wk_codmod = wrk_campo
          KEYBOARD wk_codmod
     ENDIF
     SELECT 11
     USE
     SELECT (wrk_select)
ENDIF
IF ROW() == 8
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'CLAS'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE CLASIFICACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 9
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'LINE'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE LINEAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 11
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SET FILTER TO indent == 'P'
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE PROVEEDORES'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     USE
ENDIF
ON KEY LABEL F6 do ayuda01
RETURN
*
PROCEDURE pro4
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen
DEFINE WINDOW produ FROM 12, 18  ;
       TO 19, 73 IN screen
DEFINE POPUP prod FROM 16, 31
DEFINE BAR 1 OF prod PROMPT  ;
       '\<Codigo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripcion '
ON SELECTION POPUP prod do buspro4 with;
bar(),wrk_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       19, 73 PROMPT FIELDS  ;
       codmod + '³' + nommod IN  ;
       screen
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with wrk_campo,wrk_selec,wrk_selpro
IF LASTKEY() <> 27
     wrk_campo = st_imode.codmod
ENDIF
SELECT (wrk_selec)
RETURN
*
PROCEDURE buspro4
PARAMETER bar, wrk_selpro
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF bar = 1
     wrk_codpro = SPACE(14)
     SET ORDER TO CODIGO
     @ 00, 00 SAY 'Codigo :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     wrk_codpro = SPACE(40)
     SET ORDER TO MOD_NOMMOD
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK wrk_codpro
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod4
     BROWSE FIELDS codmod :R :H =  ;
            'Cod. Modelo.',  ;
            nommod :R : 35 :H =  ;
            'Descripcion' FREEZE  ;
            codmod IN produ
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
SET NEAR OFF
RETURN
*
PROCEDURE tomacod4
ON KEY
wrk_campo = st_imode.codmod
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
