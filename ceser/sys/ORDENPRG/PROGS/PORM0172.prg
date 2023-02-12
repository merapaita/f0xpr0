*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
ind_prg = '<PORM0172>'
ON ERROR DO ERRO WITH ERROR()
PUBLIC camino, arck, dd, ee
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
STORE ' ' TO wk_drive
DO saycenter WITH 1,  ;
   'NUMEROS DE SERIE'
DO saycenter WITH 2,  ;
   ' PROCESO AUTOMATICO '
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
CLOSE DATABASES
LOAD verdis.bin
DO WHILE .T.
     STORE ' ' TO wk_drive,  ;
           wk_copia
     STORE 0 TO val, erre, wk_hay
     STORE SPACE(30) TO camino,  ;
           pah
     STORE SPACE(12) TO polo
     @ 4, 2 CLEAR TO 7, 75
     @ 4, 2 TO 6, 75
     @ 05, 04 SAY  ;
       'DRIVE ORIGEN  [A],[B]....:'
     @ 05, 31 GET wk_drive  ;
       PICTURE '!' VALID  ;
       drive2()
     @ 05, 33 GET camino PICTURE  ;
       '@!'
     SET CURSOR ON
     READ
     SET CURSOR OFF
     CLEAR TYPEAHEAD
     IF LASTKEY() = 27
          EXIT
     ENDIF
     p = AT('\ ', camino)
     IF p = 0
          camino = RTRIM(camino) +  ;
                   '\'
     ENDIF
     pah = "'" + ALLTRIM(camino) +  ;
           "ST_ISERI.DBF'"
     DIMENSION a( 1)
     wk_aux = adir(a,&pah)
     IF  .NOT. EMPTY(a(1))
          wk_hay = 1
          DO traspasa
          EXIT
     ELSE
          DO error WITH  ;
             '** No se encuentra el Archivo ST_ISERI.DBF **'
          LOOP
     ENDIF
ENDDO
CLOSE DATABASES
RELEASE MODULE verdis
DO saca_win
ON ERROR
RETURN
*
PROCEDURE traspasa
pah2 = "'" + ALLTRIM(camino) +  ;
       "ST_ISERI.DBF'"
ee=&pah2
SELECT 5
USE EXCLUSIVE st_iseri.dbf
SELECT 6
use &ee
DO WHILE  .NOT. EOF()
     wk_marca = codmar
     wk_numser = numser
     wk_codent = codent
     wk_fecing = fecing
     wk_modelo = modelo
     wk_fecvta = fecvta
     wk_fecgar = fecgar
     buscar = 'wk_marca+wk_modelo+wk_numser'
     SELECT 5
     SET ORDER TO SER_CODMAR
     seek &buscar
     IF  .NOT. FOUND()
          APPEND BLANK
          REPLACE codmar WITH  ;
                  wk_codmar,  ;
                  fecing WITH  ;
                  wk_fecing
          REPLACE numser WITH  ;
                  wk_numser,  ;
                  modelo WITH  ;
                  wk_modelo
          REPLACE codent WITH  ;
                  wk_codent,  ;
                  fecvta WITH  ;
                  wk_fecvta
     ENDIF
     SELECT 6
     SKIP
ENDDO
SELECT 5
INDEX ON codent + codmar +  ;
      DTOC(fecing) TAG st_1seri
INDEX ON codmar + modelo + numser  ;
      TAG st_2seri
IF erre = 202
     USE
     KEYBOARD '{ESC}'
ELSE
     DO error WITH  ;
        '** El Archivo Fue Actualizado, sin problemas **'
ENDIF
RETURN
*
FUNCTION drive2
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
IF wk_drive = 'A' .OR. wk_drive =  ;
   'B'
     STORE ' ' TO s, a
     s = wk_drive
     CALL verdis WITH s, a
     IF s = '€'
          DO error WITH  ;
             '** Disquetera No preparada **'
          RETURN .F.
     ENDIF
     IF s = CHR(2)
          DO error WITH  ;
             '** Disco No formateado **'
          RETURN .F.
     ENDIF
     camino = wk_drive +  ;
              ':\                           '
ELSE
     DO error WITH  ;
        '** Drive no valido **'
     RETURN .F.
ENDIF
RETURN
*
FUNCTION erro
PARAMETER num
IF num = 202
     erre = num
     DO error WITH  ;
        '** El Path asignado es Invalido **'
     RETURN .F.
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
