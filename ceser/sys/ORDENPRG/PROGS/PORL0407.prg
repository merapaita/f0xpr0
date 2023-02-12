*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'INFORME'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ESTADO DE ARTICULOS (INVENTARIO) '
ppas = .T.
SELECT 1
USE SHARED ST_IOREP ORDER CODIGO
SELECT 2
USE SHARED GE_TAB0 ORDER CODIGO
DO WHILE ppas
     @ 07, 01 CLEAR TO 13, 77
     @ 03, 02 TO 06, 77
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_key = 0
     STORE SPACE(4) TO wrk_estado,  ;
           wrk_emisor
     STORE SPACE(1) TO wrk_destin
     STORE 0 TO wrk_copia
     SET CURSOR ON
     @ 04, 04 SAY  ;
       'C¢digo de Estado :'
     @ 04, 53 SAY  ;
       'Destino (D/R) :'
     @ 05, 53 SAY  ;
       'Copias        :'
     @ 04, 23 GET wrk_estado  ;
       PICTURE '@!' VALID  ;
       valida(1) WHEN antes(1)
     @ 04, 68 GET wrk_destin  ;
       PICTURE '@!' VALID  ;
       wrk_destin $ 'RD'
     @ 05, 68 GET wrk_copia  ;
       PICTURE '99' VALID  ;
       valida(4)
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT st_iorep.numdoc,  ;
            st_iorep.fecemi,  ;
            st_iorep.codemi,  ;
            st_iorep.codent,  ;
            st_iorep.indori,  ;
            st_iorep.indest,  ;
            st_iorep.auxest,  ;
            st_iorep.codmar,  ;
            st_iorep.codmod,  ;
            st_iorep.numser,  ;
            st_iorep.codtec,  ;
            st_iorep.observ,  ;
            st_iorep.numsol,  ;
            st_iorep.codtall,  ;
            st_iorep.fecest,  ;
            st_isrep.coddes,  ;
            st_iclpr.numte1 FROM  ;
            ST_IOREP, ST_ISREP,  ;
            ST_ICLPR WHERE  ;
            st_isrep.numdoc =  ;
            st_iorep.numsol AND  ;
            st_iclpr.codent =  ;
            st_iorep.codent AND  ;
            (st_iorep.indest <>  ;
            'N' AND  ;
            st_iorep.indest <>  ;
            'F' AND  ;
            st_iorep.indest <>  ;
            'B' AND  ;
            st_iorep.auxest =  ;
            wrk_estado AND  ;
            st_isrep.coddes =  ;
            wrk_destin) ORDER BY  ;
            st_iorep.codemi,  ;
            st_iorep.indori INTO  ;
            CURSOR QUERY
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>27
          wk_key = INKEY(0, 'H')
     ENDDO
     IF wk_key = -6
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(15)
               REPORT FORMAT  ;
                      PORL4130 TO  ;
                      PRINTER  ;
                      NOCONSOLE
               SET PRINTER TO
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
     ENDIF
ENDDO
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
SELECT 2
DO CASE
     CASE opc = 1
          SET FILTER TO tab_codpre ==;
'ESOR'
          titulo = 'AYUDA DE ESTADOS'
     CASE opc = 3
          SET FILTER TO tab_codpre ==;
'EMIS'
          titulo = 'AYUDA DE EMISORES'
ENDCASE
GOTO TOP
campo = 'tab_codtab + "  " + tab_destab'
titulo = 'AYUDA DE ESTADOS'
DO ayuda1 WITH campo, titulo,  ;
   'tab_codtab'
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY LABEL F6 DO AYUDA WITH;
1
     CASE opc = 3
          ON KEY LABEL F6 DO AYUDA WITH;
3
ENDCASE
*
FUNCTION valida
PARAMETER opc
DO CASE
     CASE opc = 1
          ON KEY
          SELECT 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_estado)
               DO error WITH  ;
                  'No se Permiten Blancos'
               RETURN .F.
          ENDIF
          SEEK 'ESOR' +  ;
               wrk_estado
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Estado No Existe'
               RETURN .F.
          ENDIF
          @ 04, 29 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 3
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_emisor)
               DO error WITH  ;
                  'No se Permiten Blancos'
               RETURN .F.
          ENDIF
          SEEK 'EMIS' +  ;
               wrk_emisor
          IF  .NOT. FOUND()
               DO error WITH  ;
                  'C¢digo de Emisor No Existe'
               RETURN .F.
          ENDIF
          @ 05, 29 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            20)
     CASE opc = 4
          ON KEY
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_copia)
               DO error WITH  ;
                  'No se Permiten Ceros'
               RETURN .F.
          ENDIF
          CLEAR READ
ENDCASE
*
PROCEDURE sss
DO WHILE ppas
     @ 7, 1 CLEAR TO 13, 77
     @ 4, 30 SAY SPACE(30)
     @ 3, 2 TO 5, 77
     @ 4, 5 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BUS', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_key = 0
     wrk_estado = SPACE(4)
     SET CURSOR ON
     @ 04, 05 SAY  ;
       'C¢digo de Estado :'
     @ 04, 25 GET wrk_estado  ;
       PICTURE '@!'
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          EXIT
     ENDIF
     SELECT 2
     SEEK 'ESOR' + wrk_estado
     IF  .NOT. FOUND()
          DO error WITH  ;
             'C¢digo de Estado No Existe'
          LOOP
     ENDIF
     @ 04, 30 SAY tab_destab
     wrk_desest = tab_destab
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 1
     COPY TO TEMP STRUCTURE
     SELECT 3
     USE EXCLUSIVE TEMP
     APPEND FROM ST_IOREP FOR  ;
            auxest = wrk_estado  ;
            .AND. indest <>  ;
            'F   ' .AND. indest <>  ;
            'B   ' .AND. indest <>  ;
            'N   '
     INDEX ON auxest + codemi +  ;
           indori + DTOC(fecemi)  ;
           TAG codigo
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'IMP', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO WHILE LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>27
          wk_key = INKEY(0, 'H')
     ENDDO
     IF wk_key = -6
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'COLO'
          REPORT FORMAT ESTADO TO  ;
                 PRINTER  ;
                 NOCONSOLE
          SET PRINTER TO
          DO mensa WITH  ;
             '*** I m p r i m i e n d o ... ***',  ;
             'SACA'
     ENDIF
     SELECT 3
     USE
     ERASE FILE
     ERASE FILE
ENDDO
*
*** 
*** ReFox - retrace your steps ... 
***
