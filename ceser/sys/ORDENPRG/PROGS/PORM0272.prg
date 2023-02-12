*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
wrk_progra = PROGRAM()
DO crea_win
@ 02, 01 SAY DATE()
DO saycenter WITH 1, ' REPORTE'
DO saycenter WITH 2,  ;
   ' DIFERENCIA DE INVENTARIO '
CLOSE DATABASES
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED st_iclpr ORDER codigo
STORE .T. TO a
DO WHILE a
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 00 TO 04, 73
     STORE 1 TO w_estado, w_copia
     STORE SPACE(04) TO w_talini,  ;
           w_talfin, w_emiini,  ;
           w_emifin, w_garini,  ;
           w_garfin
     STORE SPACE(08) TO w_arch
     SET CURSOR ON
     @ 05, 03 SAY  ;
       'Archivo de Inventario: INVEN'
     @ 05, 32 GET w_estado RANGE  ;
       1,999 PICTURE '999' VALID  ;
       despues(1)
     READ
     IF LASTKEY() = 27
          @ 06, 03 CLEAR TO 11,  ;
            70
          a = .F.
     ELSE
          STORE SPACE(05) TO  ;
                w_clave
          @ 06, 03 SAY  ;
            'Ingrese la clave     :'  ;
            COLOR W+/N,W/W  GET  ;
            w_clave PICTURE '@!'  ;
            VALID  .NOT.  ;
            EMPTY(w_clave) .AND.  ;
            LEN(ALLTRIM(w_clave)) >  ;
            1
          READ
          IF LASTKEY() = 27
               f = .F.
          ELSE
               IF w_clave <> pass
                    DO error WITH  ;
                       '*** Clave Incorrecta ***'
                    f = .F.
               ELSE
                    f = .T.
               ENDIF
          ENDIF
          DO WHILE f
               IF inve = 'INTA'  ;
                  .OR. inve =  ;
                  'INVE'
                    @ 07, 03 SAY  ;
                      'Taller'
                    @ 07, 10 SAY  ;
                      'Del:' GET  ;
                      w_talini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(4)  ;
                      WHEN  ;
                      antes(4)
                    @ 08, 10 SAY  ;
                      ' Al:' GET  ;
                      w_talfin  ;
                      RANGE  ;
                      w_talini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(5)  ;
                      WHEN  ;
                      antes(4)
               ENDIF
               IF inve = 'INRE'  ;
                  .OR. inve =  ;
                  'INVE'
                    @ 07, 38 SAY  ;
                      'Emisor'
                    @ 07, 45 SAY  ;
                      'Del:' GET  ;
                      w_emiini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(2)  ;
                      WHEN  ;
                      antes(2)
                    @ 08, 45 SAY  ;
                      ' Al:' GET  ;
                      w_emifin  ;
                      RANGE  ;
                      w_emiini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(3)  ;
                      WHEN  ;
                      antes(2)
                    @ 09, 38 SAY  ;
                      'Atenc.'
                    @ 09, 45 SAY  ;
                      'Del:' GET  ;
                      w_garini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(6)  ;
                      WHEN  ;
                      antes(3)
                    @ 10, 45 SAY  ;
                      ' Al:' GET  ;
                      w_garfin  ;
                      RANGE  ;
                      w_garini  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(7)  ;
                      WHEN  ;
                      antes(3)
               ENDIF
               @ 10, 03 SAY  ;
                 'Orden por :'
               @ 13, 38 SAY  ;
                 'Por :'
               @ 10, 15 GET  ;
                 w_opcion DEFAULT  ;
                 1 SIZE 1, 12, 0  ;
                 PICTURE  ;
                 '@*RVN S/Servicio;O/Reparaci¢n;Taller;Emisor;Marca'  ;
                 WHEN antes(1)
               @ 13, 44 GET  ;
                 output DEFAULT 1  ;
                 PICTURE  ;
                 '@*RHTN Pantalla;Impresora'
               READ
               IF LASTKEY() = 27
                    @ 06, 03  ;
                      CLEAR TO 14,  ;
                      70
                    f = .F.
               ELSE
                    DO imprime
               ENDIF
          ENDDO
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF EMPTY(w_estado)
               RETURN .F.
          ENDIF
          w_arch = 'INVEN' +  ;
                   LTRIM(STR(w_estado)) +  ;
                   '.DBF'
          IF FILE(w_arch)
               SELECT 3
               use &w_arch shared
          ELSE
               do error with "*** Archivo &w_arch no Existe ***"
               RETURN .F.
          ENDIF
ENDCASE
RETURN
*
PROCEDURE antes
PARAMETER opc2
DO CASE
     CASE opc2 = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          ON KEY LABEL F6
     CASE opc2 = 2
          ON KEY LABEL F6 do ayuda with;
2
     CASE opc2 = 3
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          ON KEY LABEL F6 do ayuda with;
3
     CASE opc2 = 4
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL F6 do ayuda with;
4
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_estado)
               RETURN .F.
          ENDIF
          w_arch = 'INVEN' +  ;
                   LTRIM(STR(w_estado)) +  ;
                   '.DBF'
          IF FILE(w_arch)
               SELECT 3
               use &w_arch
          ELSE
               do error with "*** Archivo &w_arch no Existe ***"
               RETURN .F.
          ENDIF
     CASE opc = 2
          IF EMPTY(w_emiini)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_emiini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Estado No Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 3
          IF EMPTY(w_emifin)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'EMIS' + w_emifin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo de Estado No Existe ***'
               RETURN .F.
          ENDIF
          @ 08, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 4
          IF EMPTY(w_talini)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF (w_talini > '010 '  ;
             .AND. w_talini <  ;
             '020 ') .OR.  ;
             (w_talini > '060 '  ;
             .AND. w_talini <  ;
             '070 ')
               DO error WITH  ;
                  '*** No hay inventario de Domicilio ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + w_talini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 20 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 5
          IF EMPTY(w_talfin)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          IF (w_talfin > '010 '  ;
             .AND. w_talfin <  ;
             '020 ') .OR.  ;
             (w_talfin > '060 '  ;
             .AND. w_talfin <  ;
             '070 ')
               DO error WITH  ;
                  '*** No hay inventario de Domicilio ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'TALL' + w_talfin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 08, 20 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 6
          IF EMPTY(w_garini)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_garini
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 09, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
     CASE opc = 7
          IF EMPTY(w_garfin)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT ge_tab0
          SEEK 'INGA' + w_garfin
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** C¢digo no Existe ***'
               RETURN .F.
          ENDIF
          @ 10, 55 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            15)
ENDCASE
RETURN
*
PROCEDURE ayuda
PARAMETER opc1
SELECT ge_tab0
DO CASE
     CASE opc1 = 1
          SET FILTER TO tab_codpre = 'INME'
     CASE opc1 = 2
          SET FILTER TO tab_codpre = 'EMIS'
     CASE opc1 = 3
          SET FILTER TO tab_codpre = 'INGA'
     CASE opc1 = 4
          SET FILTER TO tab_codpre = 'TALL'
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE TABLAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
RETURN
*
PROCEDURE imprime
IF output = 2
     @ 14, 59 SAY 'Copias :' GET  ;
       w_copia RANGE 1,10 PICTURE  ;
       '99'
     READ
     IF LASTKEY() = 27
          @ 14, 59 SAY SPACE(15)
          sele '&w_arch'
          RETURN
     ENDIF
ENDIF
DO mensa WITH  ;
   '** Un momento, Por Favor ... **',  ;
   'COLO'
CREATE CURSOR nuevo (numdoc C (8),  ;
       numsol C (8), fecemi D (8),  ;
       indori C (4), codmar C (4),  ;
       codmod C (15), numser C  ;
       (20), codtec C (9), auxest  ;
       C (4), fecest D (8),  ;
       codtall C (4), codemi C  ;
       (4), pass C (5), codent C  ;
       (9), conteo C (1), inve C  ;
       (4), repo C (1), user C  ;
       (8), fecha D (8), hora C  ;
       (8))
sele '&w_arch'
IF w_opcion = 1
     SET ORDER TO numsol
ELSE
     IF w_opcion = 2
          SET ORDER TO numord
     ELSE
          IF w_opcion = 3
               SET ORDER TO codtal
          ELSE
               IF w_opcion = 4
                    SET ORDER TO codemi
               ELSE
                    SET ORDER TO codmar
               ENDIF
          ENDIF
     ENDIF
ENDIF
GOTO TOP
SCAN WHILE  .NOT. EOF()
     sigue = .F.
     IF conteo = '2'
          sigue = .T.
     ELSE
          IF inve = 'INVE' .OR.  ;
             inve = 'INRE'
               IF (codemi >=  ;
                  w_emiini .AND.  ;
                  codemi <=  ;
                  w_emifin) .AND.  ;
                  (indori >=  ;
                  w_garini .AND.  ;
                  indori <=  ;
                  w_garfin)
                    sigue = .T.
               ENDIF
          ENDIF
          IF inve = 'INVE' .OR.  ;
             inve = 'INTA'
               IF (codtall >=  ;
                  w_talini .AND.  ;
                  codtall <=  ;
                  w_talfin)
                    sigue = .T.
               ENDIF
          ENDIF
     ENDIF
     IF sigue
          SCATTER MEMVAR
          SELECT nuevo
          APPEND BLANK
          DO rbloquea
          GATHER MEMVAR
          UNLOCK
     ENDIF
     sele '&w_arch'
ENDSCAN
SELECT nuevo
COUNT TO w_total
IF w_total = 0
     DO mensa WITH  ;
        '***  Un momento, Por Favor ...  ***',  ;
        'SACA'
     DO error WITH  ;
        '*** No existen Registros a Listar ***'
ELSE
     IF output = 2
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          FOR a = 1 TO w_copia
               SET DEVICE TO PRINTER
               ??? CHR(15)
               REPORT FORMAT  ;
                      porm0272 TO  ;
                      PRINTER  ;
                      NOCONSOLE
               SET PRINTER TO
               SET DEVICE TO SCREEN
          ENDFOR
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ELSE
          w_file = SYS(3) +  ;
                   '.TXT'
          repo form porm0272 to file &w_file;
noconsole
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &w_file noedit window;
pantall
          SET SYSMENU OFF
          dele file &w_file
     ENDIF
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
