*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
tit_prg = 'ACTUALIZACION'
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' REGISTRO DE MANO DE OBRA '
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER codigo
SELECT 2
USE SHARED ST_ISREP ORDER codigo
SELECT 3
USE SHARED ST_ICLPR ORDER codigo
SELECT 4
USE SHARED GE_TAB0 ORDER codigo
valor = .T.
DO WHILE valor
     DO esc_indica WITH 1, 'AYU',  ;
        'BUS', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     STORE 0 TO wrk_numord,  ;
           wrk_codcli
     STORE SPACE(30) TO  ;
           wrk_dircli
     STORE SPACE(4) TO wrk_discli,  ;
           wrk_procli
     @ 05, 10 TO 13, 67
     @ 07, 15 SAY  ;
       'Nro. de Orden .. :'
     @ 09, 15 SAY  ;
       'Cliente ........ :'
     @ 10, 15 SAY  ;
       'Nombre ......... :'
     @ 11, 15 SAY  ;
       'Direcci¢n ...... :'
     @ 12, 15 SAY  ;
       'Distrito ....... :'
     @ 13, 15 SAY  ;
       'Provincia ...... :'
     SET CURSOR ON
     @ 07, 41 GET wrk_numord  ;
       PICTURE '99999999' VALID  ;
       despues(1) WHEN antes(1)
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'GRA', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE .T.
          = INKEY(0, 'H')
          IF LASTKEY() = 27
               EXIT
          ENDIF
          IF LASTKEY() = -1
               DO graba
               EXIT
          ENDIF
     ENDDO
ENDDO
CLOSE DATABASES
DO saca_win
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 3
          ON KEY
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_numord)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT 1
          SEEK wrk_numord
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** N£mero de Orden No Existe ***'
               RETURN .F.
          ENDIF
          @ 07, 46 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
          @ 09, 41 GET wrk_codcli  ;
            PICTURE '999999999'  ;
            VALID despues(1) WHEN  ;
            antes(1)
          @ 10, 41 GET wrk_nomcli  ;
            PICTURE '@!' VALID  ;
            despues(2) WHEN  ;
            antes(2)
          @ 11, 52 GET wrk_dircli  ;
            PICTURE '@!' VALID  ;
            despues(3) WHEN  ;
            antes(3)
          @ 12, 52 GET wrk_discli  ;
            PICTURE '@!' VALID  ;
            despues(4) WHEN  ;
            antes(4)
          @ 13, 52 GET wrk_procli  ;
            PICTURE '@!' VALID  ;
            despues(5) WHEN  ;
            antes(5)
     CASE opc = 2
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_codart)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          SELECT 1
          SEEK 'CLAS' +  ;
               wrk_codart
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Art¡culo No Existe'
               RETURN .F.
          ENDIF
          @ 08, 46 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
          SELECT 2
          SEEK wrk_codmar +  ;
               wrk_codart
          IF FOUND()
               wrk_monmog = mo_monmog
               wrk_monmof = mo_monmof
               wrk_monmax = mo_monmax
               @ 10, 52 SAY  ;
                 wrk_monmof  ;
                 PICTURE  ;
                 '999,999.99'
               @ 11, 52 SAY  ;
                 wrk_monmax  ;
                 PICTURE  ;
                 '999,999.99'
          ELSE
               wrk_estado = 1
          ENDIF
ENDCASE
*
PROCEDURE graba
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'COLO'
IF wrk_estado = 1
     APPEND BLANK
ENDIF
REPLACE mo_codmar WITH wrk_codmar
REPLACE mo_codart WITH wrk_codart
REPLACE mo_monmog WITH wrk_monmog
REPLACE mo_monmof WITH wrk_monmof
REPLACE mo_monmax WITH wrk_monmax
DO mensa WITH  ;
   '*****  G R A B A N D O  *****',  ;
   'SACA'
RETURN
*
PROCEDURE ayuda
SELECT 1
SET FILTER TO tab_codpre == 'MARC'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE MARCAS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
*
PROCEDURE ayuda01
SET FILTER TO tab_codpre == 'CLAS'
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE ARTICULOS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
SET FILTER TO
*
*** 
*** ReFox - retrace your steps ... 
***
