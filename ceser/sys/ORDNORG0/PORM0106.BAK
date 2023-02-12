*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     ind_prg = '<PORM0106>'
     tit_prg = 'MANTENCION'
ELSE
     ind_prg = '<PORM0106>'
     tit_prg = 'CONSULTA'
ENDIF
wrk_progra = PROGRAM()
DO crea_win
CLOSE DATABASES
SELECT 1
USE SHARED st_itecn ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED st_asiemp ALIAS  ;
    st_asiemp ORDER asi_cod
ON KEY LABEL f10 do fcinco
@ 02, 01 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' FICHA DE TECNICOS '
@ 03, 01 CLEAR TO 5, 77
@ 03, 01 TO 14, 77
@ 04, 03 SAY 'C¢digo T‚cnico :'
ppas = .T.
DO WHILE ppas
     @ 5, 2 CLEAR TO 13, 73
     @ 4, 19 SAY SPACE(55)
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_codent = 0
     wk_noment = SPACE(30)
     wk_nomcal = SPACE(30)
     wk_nomdis = SPACE(04)
     wk_nomciu = SPACE(04)
     wk_codtec = SPACE(04)
     w_cla1 = SPACE(04)
     w_cla2 = SPACE(04)
     wk_codesp = SPACE(04)
     wk_codtal = SPACE(04)
     wk_subtal = SPACE(04)
     wk_observ = SPACE(25)
     wk_meslab = DATE()
     w_mes = SPACE(5)
     STORE 0 TO wk_labdom,  ;
           wk_labtal, w_dlab
     efecin = 1
     @ 04, 19 GET wk_codent  ;
       PICTURE '999999999' VALID  ;
       codnum(wk_codent) WHEN  ;
       colocaf6(1)
     SET CURSOR ON
     READ
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     SELECT st_itecn
     SEEK STR(wk_codent, 9)
     IF  .NOT. FOUND() .AND.  ;
         config_prg <> 1
          DO error WITH  ;
             '** C¢digo T‚cnico No Existe **'
          LOOP
     ENDIF
     @ 03, 1 TO 14, 77
     @ 05, 3 SAY  ;
       'Apell./Nombres :'
     @ 06, 3 SAY  ;
       'Direcci¢n      :'
     @ 07, 3 SAY  ;
       'Distrito       :'
     @ 07, 45 SAY 'Ciudad     :'
     @ 08, 3 SAY  ;
       'Taller         :'
     @ 09, 3 SAY  ;
       'Sub Taller     :'
     @ 10, 3 SAY  ;
       'Clas.Taller    :'
     @ 11, 3 SAY  ;
       'Clas.Domicilio :'
     @ 12, 3 SAY  ;
       'Especializaci¢n:'
     @ 13, 3 SAY  ;
       'Observaciones  :'
     @ 10, 45 SAY 'Fecha Lab. :'
     @ 11, 45 SAY 'D.Lab.Tall.:'
     @ 12, 45 SAY 'D.Lab Domi.:'
     @ 13, 45 SAY 'Cod.T‚cnico:'
     IF FOUND()
          @ 4, 30 SAY  ;
            SUBSTR(noment, 1,  ;
            30)
          wk_noment = noment
          wk_nomcal = nomcal
          wk_nomdis = nomdis
          wk_nomciu = nomciu
          wk_codtec = codtec
          w_cla1 = codcla
          w_cla2 = codcl2
          wk_codesp = codesp
          wk_codtal = codtal
          wk_subtal = subtal
          wk_observ = observ
          DO llelab WITH 0
     ELSE
          wk_aux = CHRSAW()
          DO WHILE wk_aux
               = INKEY()
               wk_aux = CHRSAW()
          ENDDO
          KEYBOARD '{F3}' PLAIN
     ENDIF
     @ 05, 19 SAY wk_noment  ;
       PICTURE '@!'
     @ 06, 19 SAY wk_nomcal  ;
       PICTURE '@!'
     @ 07, 19 SAY wk_nomdis  ;
       PICTURE '!!!!'
     @ 07, 57 SAY wk_nomciu  ;
       PICTURE '!!!!'
     @ 08, 19 SAY wk_codtal  ;
       PICTURE '!!!!'
     @ 09, 19 SAY wk_subtal  ;
       PICTURE '!!!!'
     @ 10, 19 SAY w_cla1 PICTURE  ;
       '!!!!'
     @ 11, 19 SAY w_cla2 PICTURE  ;
       '!!!!'
     @ 12, 19 SAY wk_codesp  ;
       PICTURE '!!!!'
     @ 13, 19 SAY wk_observ
     @ 10, 57 SAY wk_meslab
     @ 11, 57 SAY wk_labtal  ;
       PICTURE '99.99'
     @ 12, 57 SAY wk_labdom  ;
       PICTURE '99.99'
     @ 13, 57 SAY wk_codtec  ;
       PICTURE '!!!!'
     wk_clave = 'DIST' +  ;
                wk_nomdis
     SELECT ge_tab0
     seek '&wk_clave'
     @ 07, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     wk_clave = 'PROV' +  ;
                wk_nomciu
     seek '&wk_clave'
     @ 07, 62 SAY  ;
       SUBSTR(tab_destab, 1, 10)
     SEEK 'TALL' + wk_codtal
     @ 08, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     codaux = 'SUBT' + wk_subtal
     seek '&CODAUX'
     @ 09, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     codaux = 'CLAT' + w_cla1
     seek '&CODAUX'
     @ 10, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     codaux = 'CLAT' + w_cla2
     seek '&CODAUX'
     @ 11, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     codaux = 'CODE' + wk_codesp
     seek '&CODAUX'
     @ 12, 24 SAY  ;
       SUBSTR(tab_destab, 1, 20)
     key = INKEY()
     DO esc_modo WITH 'C'
     IF config_prg == 1
          DO esc_indica WITH 1,  ;
             'AYU', 'MOD', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'ELI', 'IGN',  ;
             'ESC'
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'IGN',  ;
             'ESC'
     ENDIF
     DO WHILE key<>27 .AND. key<>- ;
        9 .AND. (key<>-2 .AND.  ;
        config_prg==1) .AND. (key<>- ;
        3 .AND. config_prg==1)
          IF key == 0
               key = 255
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
               = llelab(0)
               DO esc_modo WITH  ;
                  'M'
               DO esc_indica WITH  ;
                  1, 'AYU', 'BBB',  ;
                  'BBB', 'INT'
               DO esc_indica WITH  ;
                  2, 'RAC', 'BBB',  ;
                  'IGN', 'ESC'
               @ 05, 19 GET  ;
                 wk_noment  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 codalf(wk_noment)  ;
                 WHEN  ;
                 colocaf6(2)
               @ 06, 19 GET  ;
                 wk_nomcal  ;
                 PICTURE '@!'  ;
                 VALID  ;
                 codalf(wk_nomcal)  ;
                 WHEN  ;
                 colocaf6(2)
               @ 07, 19 GET  ;
                 wk_nomdis  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('DIST', ;
                 wk_nomdis,24,20)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 07, 57 GET  ;
                 wk_nomciu  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('PROV', ;
                 wk_nomciu,62,10)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 08, 19 GET  ;
                 wk_codtal  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('TALL', ;
                 wk_codtal,24,20)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 09, 19 GET  ;
                 wk_subtal  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('SUBT', ;
                 wk_subtal,24,20)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 10, 19 GET  ;
                 w_cla1 PICTURE  ;
                 '!!!!' VALID  ;
                 valtab('CLAT', ;
                 w_cla1,24,20)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 11, 19 GET  ;
                 w_cla2 PICTURE  ;
                 '!!!!' VALID  ;
                 valtab('CLAT', ;
                 w_cla2,24,20)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 12, 19 GET  ;
                 wk_codesp  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 valtab('CODE', ;
                 wk_codesp,24,20)  ;
                 WHEN  ;
                 colocaf6(1)
               @ 13, 19 GET  ;
                 wk_observ  ;
                 PICTURE '@!'  ;
                 WHEN  ;
                 colocaf6(2)
               @ 10, 57 GET  ;
                 wk_meslab
               @ 11, 57 GET  ;
                 wk_labtal  ;
                 PICTURE '99.99'  ;
                 VALID wk_labtal >=  ;
                 0 .AND.  ;
                 wk_labtal <= 30
               @ 12, 57 GET  ;
                 wk_labdom  ;
                 PICTURE '99.99'  ;
                 VALID wk_labdom >=  ;
                 0 .AND.  ;
                 wk_labdom <= (30 -  ;
                 wk_labtal)
               @ 13, 57 GET  ;
                 wk_codtec  ;
                 PICTURE '!!!!'  ;
                 VALID  ;
                 codtec(wk_codtec)  ;
                 WHEN  ;
                 colocaf6(2)
               READ
          CASE key == -3
               DO esc_modo WITH  ;
                  'E'
               DO error WITH  ;
                  '** Confirma Eliminaci¢n [ ÄÙ ] **'
               IF LASTKEY() = 13
                    procodx = STR(wk_codent,  ;
                              9)
                    SELECT st_itecn
                    seek '&procodx'
                    DO rbloquea
                    DELETE
                    UNLOCK
                    SELECT st_asiemp
                    seek '&procodx'
                    IF FOUND()
                         SCAN WHILE  ;
                              codemp =  ;
                              procodx  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              DO rbloquea
                              DELETE
                              UNLOCK
                         ENDSCAN
                    ENDIF
                    LOOP
               ENDIF
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
                    DO mensa WITH  ;
                       '** Espere un momento ,Grabando **',  ;
                       'COLO'
                    procodx = STR(wk_codent,  ;
                              9)
                    SELECT st_itecn
                    seek '&procodx'
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                         DO rbloquea
                         REPLACE codent  ;
                                 WITH  ;
                                 STR(wk_codent,  ;
                                 9)
                         UNLOCK
                    ENDIF
                    DO rbloquea
                    REPLACE noment  ;
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
                            codtec  ;
                            WITH  ;
                            wk_codtec,  ;
                            codcla  ;
                            WITH  ;
                            w_cla1
                    REPLACE codcl2  ;
                            WITH  ;
                            w_cla2,  ;
                            codesp  ;
                            WITH  ;
                            wk_codesp,  ;
                            codtal  ;
                            WITH  ;
                            wk_codtal,  ;
                            subtal  ;
                            WITH  ;
                            wk_subtal,  ;
                            observ  ;
                            WITH  ;
                            wk_observ,  ;
                            user  ;
                            WITH  ;
                            users
                    REPLACE date  ;
                            WITH  ;
                            DATE(),  ;
                            time  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    SELECT st_asiemp
                    seek '&procodx'+str(year(wk_meslab),5)
                    IF  .NOT.  ;
                        FOUND()
                         APPEND BLANK
                         DO rbloquea
                         REPLACE codemp  ;
                                 WITH  ;
                                 procodx
                         REPLACE anio  ;
                                 WITH  ;
                                 STR(YEAR(wk_meslab),  ;
                                 5)
                         UNLOCK
                    ENDIF
                    IF MONTH(wk_meslab) <  ;
                       10
                         w_dtal =  ;
                          'MES0' +  ;
                          STR(MONTH(wk_meslab),  ;
                          1)
                         w_ddom =  ;
                          'MED0' +  ;
                          STR(MONTH(wk_meslab),  ;
                          1)
                    ELSE
                         w_dtal =  ;
                          'MES' +  ;
                          STR(MONTH(wk_meslab),  ;
                          2)
                         w_ddom =  ;
                          'MED' +  ;
                          STR(MONTH(wk_meslab),  ;
                          2)
                    ENDIF
                    repl &w_dtal with;
 wk_labtal
                    repl &w_ddom with;
 wk_labdom
                    REPLACE user  ;
                            WITH  ;
                            users,  ;
                            date  ;
                            WITH  ;
                            DATE(),  ;
                            time  ;
                            WITH  ;
                            TIME()
                    UNLOCK
                    w_dlab = dialab()
                    SELECT st_itecn
                    SET ORDER TO fecha
                    SET NEAR ON
                    SEEK DTOS(DATE() -  ;
                         15)
                    SET NEAR OFF
                    SCAN WHILE   ;
                         .NOT.  ;
                         EOF()
                         SELECT st_asiemp
                         SEEK st_itecn.codent +  ;
                              STR(YEAR(wk_meslab),  ;
                              5)
                         IF  .NOT.  ;
                             FOUND()
                              APPEND  ;
                               BLANK
                              DO rbloquea
                              REPLACE  ;
                               codemp  ;
                               WITH  ;
                               st_itecn.codent
                              REPLACE  ;
                               anio  ;
                               WITH  ;
                               STR(YEAR(wk_meslab),  ;
                               5)
                              UNLOCK
                         ENDIF
                         IF ALLTRIM(st_itecn.codtal) >  ;
                            '010'
                              w_mes =  ;
                               'MED'
                         ELSE
                              w_mes =  ;
                               'MES'
                         ENDIF
                         IF MONTH(wk_meslab) <  ;
                            10
                              w_mes =  ;
                               w_mes +  ;
                               '0' +  ;
                               STR(MONTH(wk_meslab),  ;
                               1)
                         ELSE
                              w_mes =  ;
                               w_mes +  ;
                               STR(MONTH(wk_meslab),  ;
                               2)
                         ENDIF
                         if empty(&w_mes)
                              DO rbloquea
                              repl &w_mes;
with w_dlab   
                              REPLACE  ;
                               user  ;
                               WITH  ;
                               users
                              REPLACE  ;
                               date  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               time  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ENDIF
                         SELECT st_itecn
                    ENDSCAN
                    SET ORDER TO codigo
                    DO mensa WITH  ;
                       '** Espere un momento ,Grabando **',  ;
                       'SACA'
          ENDCASE
     ENDIF
ENDDO
ON KEY
DO sacawin
CLOSE DATABASES
RETURN
*
PROCEDURE llelab
PARAMETER posi
STORE 0 TO w_cont
SELECT st_asiemp
procodx = STR(wk_codent, 9)
wk_anio = STR(YEAR(wk_meslab), 5)
seek '&procodx'+ wk_anio
IF FOUND()
     IF MONTH(wk_meslab) < 10
          w_camt = 'MES0' +  ;
                   STR(MONTH(wk_meslab),  ;
                   1)
          w_camd = 'MED0' +  ;
                   STR(MONTH(wk_meslab),  ;
                   1)
     ELSE
          w_camt = 'MES' +  ;
                   STR(MONTH(wk_meslab),  ;
                   2)
          w_camd = 'MED' +  ;
                   STR(MONTH(wk_meslab),  ;
                   2)
     ENDIF
     wk_labtal=&w_camt
     wk_labdom=&w_camd
ELSE
     wk_labtal = 0
     wk_labdom = 0
ENDIF
IF wk_labtal = 0 .AND. wk_labdom =  ;
   0
     IF ALLTRIM(wk_codtal) <  ;
        '010'
          wk_labtal = dialab()
          wk_labdom = 0
     ELSE
          wk_labtal = 0
          wk_labdom = dialab()
     ENDIF
ENDIF
IF posi = 1
     @ 12, 57 SAY wk_labdom  ;
       PICTURE '99.99' COLOR N/W 
ENDIF
RETURN
*
FUNCTION dialab
STORE 0 TO cond
vfec = CTOD('01/' +  ;
       STR(MONTH(wk_meslab), 2) +  ;
       '/' +  ;
       SUBSTR(STR(YEAR(wk_meslab),  ;
       4), 3, 2))
DO WHILE MONTH(vfec)= ;
   MONTH(wk_meslab)
     DO CASE
          CASE DOW(vfec) = 2 .OR.  ;
               DOW(vfec) = 3 .OR.  ;
               DOW(vfec) = 4 .OR.  ;
               DOW(vfec) = 5 .OR.  ;
               DOW(vfec) = 6
               cond = cond + 1
          CASE DOW(vfec) = 7
               cond = cond + 0.5 
     ENDCASE
     vfec = vfec + 1
ENDDO
w_area = ALIAS()
SELECT ge_tab0
IF MONTH(wk_meslab) < 10
     w_meslab = '0' +  ;
                STR(MONTH(wk_meslab),  ;
                1)
ELSE
     w_meslab = STR(MONTH(wk_meslab),  ;
                2)
ENDIF
SEEK 'FERI' + w_meslab
IF FOUND()
     SCAN WHILE 'FERI' +  ;
          SUBSTR(tab_codtab, 1,  ;
          2) = 'FERI' + w_meslab  ;
          .AND.  .NOT. EOF()
          cond = cond -  ;
                 tab_factor
     ENDSCAN
ENDIF
sele &w_area
RETURN cond
*
PROCEDURE ayuda03
ON KEY LABEL F6
IF ROW() == 04
     SELECT st_itecn
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     SET ORDER TO codigo
ENDIF
IF ROW() == 07 .AND. VARREAD() =  ;
   'WK_NOMDIS'
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre == 'DIST'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE DISTRITOS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF ROW() == 07 .AND. VARREAD() =  ;
   'WK_NOMCIU'
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre == 'PROV'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE PROVINCIAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
IF ROW() == 08
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'TALL'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE TALLER'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF ROW() == 09
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'SUBT'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE SUBTALLER'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF ROW() == 10 .OR. ROW() == 11
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'CLAT'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE CLASIFICACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
IF ROW() == 12
     SELECT ge_tab0
     SET ORDER TO descr
     SET FILTER TO tab_codpre == 'CODE'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE ESPECIALIZACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
     SET ORDER TO codigo
ENDIF
ON KEY LABEL F6 do ayuda03
RETURN
*
FUNCTION colocaf6
PARAMETER opc
IF opc = 1
     ON KEY LABEL f6 do ayuda03  
     ACTIVATE WINDOW indicar
     @ 1, 19 SAY ' [F6] '
     @ 1, 26 SAY 'B£squeda'
     ACTIVATE WINDOW trabajo
ELSE
     ON KEY LABEL f6
     ACTIVATE WINDOW indicar
     @ 1, 19 SAY SPACE(06)
     @ 1, 26 SAY SPACE(08)
     ACTIVATE WINDOW trabajo
ENDIF
RETURN .T.
*
FUNCTION codtec
PARAMETER grupo
SELECT st_itecn
SET ORDER TO tec_codtec
seek '&grupo'
IF FOUND() .AND. codent <>  ;
   STR(wk_codent, 9)
     DO error WITH  ;
        '** C¢digo ya EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SET ORDER TO codigo
RETURN .T.
*
FUNCTION valtab
PARAMETER clave, codig, colu,  ;
          largo
IF EMPTY(codig)
     RETURN .F.
ENDIF
SELECT ge_tab0
codaux = clave + codig
seek '&codaux'
IF  .NOT. FOUND()
     DO error WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
DO sacaf6
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
