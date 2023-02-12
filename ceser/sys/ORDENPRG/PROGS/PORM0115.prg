*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
tit_prg = ' ACTUALIZACION '
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' TIPO DE CAMBIO '
DO esc_modo WITH 'I'
DEFINE WINDOW a1 FROM 05, 18 TO  ;
       07, 63 IN screen NONE
DEFINE WINDOW a2 FROM 05, 04 TO  ;
       07, 25 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a3 FROM 08, 04 TO  ;
       10, 25 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a4 FROM 11, 04 TO  ;
       13, 25 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a5 FROM 14, 04 TO  ;
       16, 25 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a6 FROM 05, 34 TO  ;
       07, 74 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a7 FROM 08, 34 TO  ;
       15, 74 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a8 FROM 14, 34 TO  ;
       18, 74 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a9 FROM 05, 30 TO  ;
       09, 74 NOCLOSE IN screen  ;
       NONE
DEFINE WINDOW a10 FROM 17, 34 TO  ;
       20, 74 IN screen
DEFINE POPUP a9 FROM 08, 30 TO 16,  ;
       74 PROMPT FIELDS tmp_tipo +  ;
       ' Ё ' + DTOC(tmp_fechac) +  ;
       ' Ё ' +  ;
       TRANSFORM(tmp_tipcac,  ;
       '999,999.99') + ' Ё ' +  ;
       TRANSFORM(tmp_tipcav,  ;
       '999,999.99') IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP a9 DEAC POPUP A9
DEFINE POPUP a7 FROM 08, 34 TO 16,  ;
       74 PROMPT FIELDS ' ' +  ;
       DTOC(cmv_fechac) + ' Ё ' +  ;
       TRANSFORM(cmv_tipcac,  ;
       '999,999.99') + ' Ё ' +  ;
       TRANSFORM(cmv_tipcav,  ;
       '999,999.99') IN screen  ;
       COLOR SCHEME 8
ON SELECTION POPUP a7 DEAC POPUP A7
w_program = PROGRAM()
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED GC_CMV00 ORDER  ;
    cmv_feinmo
DIMENSION a1( FCOUNT())
STORE CTOD('  /  /    ') TO a1(  ;
      01)
STORE SPACE(01) TO a1( 02)
STORE SPACE(04) TO a1( 03), a1(  ;
      04)
STORE 0 TO a1( 05)
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'MD4', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DO r_buscar WITH 1
DEACTIVATE WINDOW tablas
RELEASE WINDOW a1, a2, a3, a4, a5,  ;
        a6, a7, a8, a9, a10
RELEASE POPUP a3, a7, a9, tablas
ON KEY
CLOSE DATABASES
DO saca_win
RETURN
*
PROCEDURE r_buscar
PARAMETER cual
DO muestra1
DO carga
DEACTIVATE WINDOW a2, a3, a4, a5
ON KEY
RETURN
*
PROCEDURE muestra1
ACTIVATE WINDOW a2
@ 00, 00 SAY  ;
  'здддддддддддддддддддд©'
@ 01, 00 SAY  ;
  'Ё  Moneda Nacional   Ё'
@ 02, 00 SAY  ;
  'юдддддддддддддддддддды'
a1( 03) = rge_monbas
SELECT ge_tab0
SEEK 'MONE' + a1(03)
ACTIVATE WINDOW a3
@ 00, 00 SAY  ;
  'здддддддддддддддддддд©'
@ 01, 00 SAY  ;
  'Ё                    Ё'
@ 01, 01 SAY SUBSTR(tab_destab, 1,  ;
  20)
@ 02, 00 SAY  ;
  'юдддддддддддддддддддды'
ACTIVATE WINDOW a4
@ 00, 00 SAY  ;
  'здддддддддддддддддддд©'
@ 01, 00 SAY  ;
  'Ё  Moneda De Cambio  Ё'
@ 02, 00 SAY  ;
  'юдддддддддддддддддддды'
SELECT ge_tab0
SEEK 'MONE' + 'DOL'
IF FOUND()
     a1( 02) = ALLTRIM(STR(cual))
     a1( 04) = tab_codtab
ENDIF
ACTIVATE WINDOW a5
@ 00, 00 SAY  ;
  'здддддддддддддддддддд©'
@ 01, 00 SAY 'Ё' +  ;
  SUBSTR(ge_tab0.tab_destab, 1,  ;
  19) + SPACE(1) + 'Ё'
@ 02, 00 SAY  ;
  'юдддддддддддддддддддды'
RETURN
*
PROCEDURE carga
ON KEY
SELECT gc_cmv00
IF cual = 4
     CREATE CURSOR ARCH1  ;
            (tmp_tipo C (03),  ;
            tmp_fechac D (08),  ;
            tmp_tipcac N (9, 2),  ;
            tmp_tipcav N (9, 2))
     SELECT gc_cmv00
     SET FILTER TO
     SET NEAR ON
     SEEK DTOS(DATE() -  ;
          rge_vicodi)
     SET NEAR OFF
     SCAN WHILE  .NOT. EOF()
          IF cmv_monref = a1(04)
               SELECT arch1
               APPEND BLANK
               IF gc_cmv00.cmv_indica =  ;
                  '1'
                    REPLACE tmp_tipo  ;
                            WITH  ;
                            'OFI'
               ENDIF
               IF gc_cmv00.cmv_indica =  ;
                  '2'
                    REPLACE tmp_tipo  ;
                            WITH  ;
                            'PAR'
               ENDIF
               IF gc_cmv00.cmv_indica =  ;
                  '3'
                    REPLACE tmp_tipo  ;
                            WITH  ;
                            'BAN'
               ENDIF
               REPLACE tmp_fechac  ;
                       WITH  ;
                       gc_cmv00.cmv_fechac
               REPLACE tmp_tipcac  ;
                       WITH  ;
                       gc_cmv00.cmv_tipcac
               REPLACE tmp_tipcav  ;
                       WITH  ;
                       gc_cmv00.cmv_tipcav
               SELECT gc_cmv00
          ENDIF
     ENDSCAN
ELSE
     SET FILTER TO
     SET FILTER TO cmv_monref = a1(04);
.AND. cmv_indica = ALLTRIM(STR(cual));
.AND. cmv_fechac >= (DATE() - rge_vicodi)
ENDIF
IF cual <> 4
     COUNT TO w_count
     IF w_count = 0 .AND.  ;
        LASTKEY() = 13
     ELSE
     ENDIF
ENDIF
IF cual = 4
     ACTIVATE WINDOW a9
     @ 00, 00 SAY  ;
       'зддддбддддддддддддбддддддддддддбдддддддддддд©'
     @ 01, 00 SAY  ;
       'ЁTipoЁ   Fecha    ЁValor CompraЁValor Venta Ё'
     @ 02, 00 SAY  ;
       'юддддаддддддддддддаддддддддддддадддддддддддды'
     SELECT arch1
     GOTO BOTTOM
     ACTIVATE POPUP a9 REST
     DEACTIVATE WINDOW a9
ELSE
     ACTIVATE WINDOW a6
     @ 00, 00 SAY  ;
       'зддддддддддддбддддддддддддбддддддддддддд©'
     @ 01, 00 SAY  ;
       'Ё  Fecha     ЁValor CompraЁ Valor Venta Ё'
     @ 02, 00 SAY  ;
       'юддддддддддддаддддддддддддаддддддддддддды'
     GOTO BOTTOM
     DO seteo
     ACTIVATE POPUP a7 REST
     DEACTIVATE WINDOW a6
ENDIF
ON KEY
RETURN
*
FUNCTION elimina
ON KEY
IF RECCOUNT() = 0 .AND. LASTKEY() = - ;
   3
     DO p_mensaje WITH  ;
        'NO HAY REGISTROS'
ELSE
     IF cmv_fechac < DATE() .AND.  ;
        xnnn <> 'A7' .AND. xnnn <>  ;
        'A5'
          DO p_mensaje WITH  ;
             'No se puede Borrar de otro d║a'
          RETURN .F.
     ENDIF
     DO rbloquea
     DELETE
     UNLOCK
     DO seteo
     SELECT gc_cmv00
     COUNT TO w_count
     IF w_count = 0
     ELSE
          DO ponf6
     ENDIF
ENDIF
RETURN
*
PROCEDURE procesa
PARAMETER w_op
ON KEY
SELECT gc_cmv00
IF EOF()
     w_recno = RECNO() - 1
ELSE
     w_recno = RECNO()
ENDIF
COUNT TO w_count
IF w_count = 0 .AND. LASTKEY() =  ;
   13
     DO p_mensaje WITH  ;
        'NO HAY REGISTROS'
ELSE
     IF w_op = 1
          STORE DATE() TO a1( 01)
          STORE 0 TO a1( 05), a1(  ;
                06)
     ELSE
          GOTO w_recno
          SCATTER TO a1
     ENDIF
     SELECT gc_cmv00
     DO WHILE .T.
          ACTIVATE WINDOW a8
          @ 00, 00 SAY  ;
            'зддддддддддддбддддддддддддбддддддддддддд©'
          @ 01, 00 SAY  ;
            'Ё            Ё            Ё             Ё'
          @ 02, 00 SAY  ;
            'юддддддддддддаддддддддддддаддддддддддддды'
          IF w_op = 1
               @ 01, 02 GET a1(  ;
                 1)
               READ
               IF a1(1) = CTOD( ;
                  '  /  /    ')
                    EXIT
               ENDIF
               SEEK DTOS(a1(01)) +  ;
                    a1(02)
               IF FOUND()
                    DO error WITH  ;
                       'Fecha ya fue ingresada...'
                    LOOP
               ENDIF
          ELSE
               @ 01, 02 GET a1(  ;
                 1)
               @ 01, 15 GET a1(  ;
                 5) PICTURE  ;
                 '999,999.99'
               @ 01, 28 GET a1(  ;
                 6) PICTURE  ;
                 '999,999.99'
               CLEAR GETS
               IF a1(1) < DATE()  ;
                  .AND. xnnn <>  ;
                  'A7' .AND. xnnn <>  ;
                  'A5'
                    DO p_mensaje  ;
                       WITH  ;
                       'No se puede Modificar de otro d║a'
                    DEACTIVATE WINDOW  ;
                               a8
                    EXIT
               ENDIF
          ENDIF
          DO WHILE .T. .AND.  ;
             LASTKEY()<>27
               ACTIVATE WINDOW a8
               @ 01, 15 GET a1(  ;
                 5) PICTURE  ;
                 '999,999.99'  ;
                 VALID a1(5) > 0
               READ
               IF LASTKEY() <> 27
                    IF a1(5) = 0
                         DO p_mensaje  ;
                            WITH  ;
                            'Ingrese Valor Compra........'
                         LOOP
                    ENDIF
               ENDIF
               @ 01, 28 GET a1(  ;
                 6) PICTURE  ;
                 '999,999.99'  ;
                 VALID a1(6) > 0
               READ
               IF LASTKEY() <> 27
                    IF a1(6) = 0
                         DO p_mensaje  ;
                            WITH  ;
                            'Ingrese Valor Venta.........'
                         LOOP
                    ENDIF
               ENDIF
               EXIT
          ENDDO
          IF LASTKEY() <> 27
               SELECT gc_cmv00
               IF w_op = 1
                    APPEND BLANK
               ENDIF
               DO rbloquea
               GATHER FROM a1
               REPLACE cmv_usuari  ;
                       WITH  ;
                       users
               REPLACE cmv_fecha  ;
                       WITH  ;
                       DATE()
               REPLACE cmv_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          EXIT
     ENDDO
     DEACTIVATE WINDOW a8
     SELECT gc_cmv00
     DO ponf6
     DO seteo
ENDIF
DEACTIVATE WINDOW a8
RETURN
*
PROCEDURE seteo
ON KEY LABEL ENTER DO PROCESA WITH 2 
ON KEY LABEL F3 DO PROCESA WITH 1 
ON KEY LABEL F4 DO ELIMINA
ON KEY LABEL f7 do imprimi 
RETURN
*
PROCEDURE ponf6
*
PROCEDURE imprimi
ON KEY
ACTIVATE WINDOW a10
STORE DATE() TO w_fecini,  ;
      w_fecfin
@ 00, 01 SAY  ;
  'Imprimir  Fecha del:'
@ 01, 18 SAY 'Al:'
@ 00, 22 GET w_fecini
@ 01, 22 GET w_fecfin RANGE  ;
  w_fecini
READ
IF LASTKEY() <> 27
     SELECT gc_cmv00
     SET FILTER TO
     SET NEAR ON
     SEEK DTOS(w_fecini)
     SET NEAR OFF
     SET DEVICE TO PRINTER
     @ 00, 00 SAY CHR(27) +  ;
       CHR(15)
     REPORT FORMAT agca0207 TO  ;
            PRINTER NOCONSOLE  ;
            WHILE cmv_fechac <=  ;
            w_fecfin
     @ 00, 00 SAY CHR(27) +  ;
       CHR(18)
     SET DEVICE TO SCREEN
     IF cual <> 4
          SET FILTER TO cmv_monref = a1(04);
.AND. cmv_indica = ALLTRIM(STR(cual));
.AND. cmv_fechac >= (DATE() - rge_vicodi)
     ENDIF
ENDIF
DEACTIVATE WINDOW a10
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
