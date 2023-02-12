*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 do ayuda0a
ON KEY LABEL F10 DO FCINCO
SET SYSMENU ON
@ 2, 1 SAY DATE()
DO saycenter WITH 1,  ;
   ' MANTENCION '
DO saycenter WITH 2,  ;
   ' PARAMETROS GENERALES '
@ 3, 3 CLEAR TO 15, 70
@ 3, 3 TO 15, 70
@ 04, 8 SAY  ;
  'R.U.C. Empresa         :'
@ 05, 8 SAY  ;
  'Nombre o Raz¢n Social  :'
@ 06, 8 SAY  ;
  'Direcci¢n              :'
@ 07, 8 SAY  ;
  'Nombre Distrito        :'
@ 08, 8 SAY  ;
  'Ciudad                 :'
@ 09, 8 SAY  ;
  'Almacen de Repuestos   :'
@ 10, 8 SAY  ;
  'Tiempo Limite Servicio :'
@ 11, 8 SAY  ;
  'Lista Precio Garantia  :'
@ 12, 8 SAY  ;
  'Lista Precio Fuera G.  :'
@ 13, 8 SAY  ;
  'Periodo de Garantia    :'
@ 11, 50 SAY 'Boleta :'
@ 12, 50 SAY 'Factura:'
ppas = .T.
DO WHILE ppas
     DO esc_modo WITH 'C'
     efecin = 1
     USE SHARED st_iparg
     wk_rutemp = VAL(sys_numruc)
     wk_nomemp = sys_razsoc
     wk_direcc = sys_nomcal
     wk_distri = sys_nomdis
     wk_ciudad = sys_nomciu
     wk_taller = sys_codtal
     wk_tiempo = sys_tmpdem
     wk_codlpg = sys_codlpg
     wk_codlpf = sys_codlpf
     wk_pergar = sys_fecgar
     wk_lptbol = sys_nrobol
     wk_lptfac = sys_numfac
     USE
     @ 04, 33 SAY wk_rutemp  ;
       PICTURE '99999999999'
     @ 05, 33 SAY wk_nomemp
     @ 06, 33 SAY wk_direcc
     @ 07, 33 SAY wk_distri
     @ 08, 33 SAY wk_ciudad
     @ 09, 33 SAY wk_taller
     @ 10, 33 SAY wk_tiempo  ;
       PICTURE '9999'
     @ 11, 33 SAY wk_codlpg
     @ 11, 58 SAY wk_lptbol  ;
       PICTURE '9999999999'
     @ 12, 33 SAY wk_codlpf
     @ 12, 58 SAY wk_lptfac  ;
       PICTURE '9999999999'
     @ 13, 33 SAY wk_pergar  ;
       PICTURE '9999'
     wk_clave = 'DIST' +  ;
                wk_distri
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_clave'
     @ 07, 38 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     wk_clave = 'PROV' +  ;
                wk_ciudad
     SEEK '&wk_clave'
     @ 08, 38 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     wk_clave = 'ALMA' +  ;
                wk_taller
     SEEK '&wk_clave'
     @ 09, 38 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     USE SHARED gc_hlp00 ORDER  ;
         codigo
     SEEK '&wk_codlpg'
     @ 11, 38 SAY  ;
       SUBSTR(hlp_deslis, 1, 10)
     SEEK '&wk_codlpf'
     @ 12, 38 SAY  ;
       SUBSTR(hlp_deslis, 1, 10)
     @ 13, 38 SAY 'D¡as'
     USE
     DO esc_indica WITH 1, 'AYU',  ;
        'MOD', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     in_key = 0
     DO WHILE in_key<>27 .AND.  ;
        in_key<>-2
          in_key = INKEY(0)
     ENDDO
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. (efecin == 2  ;
               .OR. efecin ==  ;
               10)
               LOOP
     ENDCASE
     IF in_key = -2
          DO esc_modo WITH 'M'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'INT'
          DO esc_indica WITH 2,  ;
             'RAC', 'BBB', 'IGN',  ;
             'ESC'
          @ 04, 33 GET wk_rutemp  ;
            PICTURE '99999999999'  ;
            VALID  ;
            codnum(wk_rutemp)
          @ 05, 33 GET wk_nomemp  ;
            PICTURE '@!' VALID  ;
            codalf(wk_nomemp)
          @ 06, 33 GET wk_direcc  ;
            PICTURE '@!' VALID  ;
            codalf(wk_direcc)
          @ 07, 33 GET wk_distri  ;
            PICTURE '@!' VALID  ;
            valtab('DIST', ;
            wk_distri,38,30) WHEN  ;
            colocaf6()
          @ 08, 33 GET wk_ciudad  ;
            PICTURE '@!' VALID  ;
            valtab('PROV', ;
            wk_ciudad,38,30) WHEN  ;
            colocaf6()
          @ 09, 33 GET wk_taller  ;
            PICTURE '@!' VALID  ;
            valtab('ALMA', ;
            wk_taller,38,30) WHEN  ;
            colocaf6()
          @ 10, 33 GET wk_tiempo  ;
            PICTURE '9999'
          @ 11, 33 GET wk_codlpg  ;
            PICTURE '@!' VALID  ;
            codpre(wk_codlpg,38)  ;
            WHEN colocaf6()
          @ 11, 58 GET wk_lptbol  ;
            PICTURE '9999999999'
          @ 12, 33 GET wk_codlpf  ;
            PICTURE '@!' VALID  ;
            codpre(wk_codlpf,38)  ;
            WHEN colocaf6()
          @ 12, 58 GET wk_lptfac  ;
            PICTURE '9999999999'
          @ 13, 33 GET wk_pergar  ;
            PICTURE '9999' WHEN  ;
            colocaf6()
          SET CURSOR ON
          READ
          SET CURSOR OFF
          DO CASE
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 1
                    ppas = .F.
                    LOOP
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    (efecin == 2  ;
                    .OR. efecin ==  ;
                    10)
                    LOOP
          ENDCASE
          DO esc_modo WITH 'C'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'GRA', 'BBB', 'IGN',  ;
             'ESC'
          ink_aux = 0
          DO WHILE ink_aux<>27  ;
             .AND. ink_aux<>-1
               ink_aux = INKEY(0)
               IF ink_aux == -9
                    DO fcinco
               ENDIF
          ENDDO
          DO CASE
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    efecin == 1
                    ppas = .F.
                    LOOP
               CASE LASTKEY() ==  ;
                    27 .AND.  ;
                    (efecin == 2  ;
                    .OR. efecin ==  ;
                    10)
                    LOOP
          ENDCASE
          USE SHARED st_iparg
          IF EOF()
               APPEND BLANK
          ENDIF
          sw_sn = .T.
          DO WHILE sw_sn
               IF RLOCK()
                    REPLACE sys_numruc  ;
                            WITH  ;
                            STR(wk_rutemp,  ;
                            11),  ;
                            sys_razsoc  ;
                            WITH  ;
                            wk_nomemp,  ;
                            sys_nomcal  ;
                            WITH  ;
                            wk_direcc,  ;
                            sys_nomdis  ;
                            WITH  ;
                            wk_distri,  ;
                            sys_nomciu  ;
                            WITH  ;
                            wk_ciudad,  ;
                            sys_codtal  ;
                            WITH  ;
                            wk_taller,  ;
                            sys_tmpdem  ;
                            WITH  ;
                            wk_tiempo
                    REPLACE sys_codlpg  ;
                            WITH  ;
                            wk_codlpg,  ;
                            sys_codlpf  ;
                            WITH  ;
                            wk_codlpf,  ;
                            sys_nrobol  ;
                            WITH  ;
                            wk_lptbol,  ;
                            sys_numfac  ;
                            WITH  ;
                            wk_lptfac
                    UNLOCK
                    EXIT
               ELSE
                    sw_sn = f_yesno1( ;
                            'Registro Bloquedo, Reintenta ' ;
                            )
               ENDIF
          ENDDO
          USE
          empre1 = TRIM(wk_nomemp)
          empre2 = TRIM(wk_direcc)
          @ 0, 75 -  ;
            LEN(TRIM(empre1)) SAY  ;
            empre1
          @ 1, 75 -  ;
            LEN(TRIM(empre2)) SAY  ;
            empre2
     ENDIF
ENDDO
ON KEY LABEL F10
ON KEY LABEL F6
DO saca_win
RETURN
*
PROCEDURE ayuda0a
ON KEY LABEL F6
IF ROW() == 07
     USE SHARED ge_tab0 ORDER  ;
         codigo
     IF FLOCK()
          SET FILTER TO tab_codpre ==;
'DIST'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE DISTRITOS'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
     ELSE
          DO error WITH  ;
             '** Esta Tomado por otro usuario **'
          RETRY
     ENDIF
     USE
ENDIF
IF ROW() == 08
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
IF ROW() == 09
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'ALMA'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE TALLER / ALMACEN'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 11 .OR. ROW() == 12
     USE SHARED gc_hlp00 ORDER  ;
         codigo
     campoa = '"  "+hlp_codlis+"  "+hlp_deslis'
     campob = '"  "+hlp_codlis+"  "+hlp_deslis'
     titulo = 'AYUDA LISTAS DE PRECIO'
     DO ii WITH campoa, campob,  ;
        titulo, 'hlp_codlis'
     USE
ENDIF
ON KEY LABEL F6 do ayuda0a
RETURN
*
FUNCTION codpre
PARAMETER grupo, colu
USE SHARED gc_hlp00 ORDER codigo
SEEK '&grupo'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** Codigo Lista Precio NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), colu SAY  ;
  SUBSTR(hlp_deslis, 1, 10)
USE
DO sacaf6
RETURN .T.
*
PROCEDURE ii
PARAMETER campo1, campo2, mensaje,  ;
          clave
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
define popup ayu2 from 0,35 to 8,79 promp;
field &campo1 shadow title mensaje
ON SELECTION POPUP ayu2 do choice2
ACTIVATE POPUP ayu2 NOWAIT
FOR i = 1 TO 5
     MOVE POPUP ayu2 BY 1, 0
ENDFOR
FOR i = 1 TO 09
     MOVE POPUP ayu2 BY 0, -1
ENDFOR
ACTIVATE POPUP ayu2
DEACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu2
RETURN
*
PROCEDURE choice2
IF LASTKEY() == 13
     keyboard &clave
     DEACTIVATE POPUP ayu2
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
