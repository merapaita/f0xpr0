*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
IF config_prg == 1
     ind_prg = '<PORQ0204>'
     tit_prg = 'CREACION'
ELSE
     ind_prg = '<PORQ0302>'
     tit_prg = 'CONSULTA'
ENDIF
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA13
ON KEY LABEL F10 DO FCINCO
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 42, 76
ZOOM WINDOW indicar NORM FROM 45,  ;
     0 TO 48, 76
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONSULTA DE REPUESTOS '
@ 07, 2 CLEAR TO 13, 72
@ 07, 2 TO 13, 72
@ 08, 5 SAY 'Numero del Pedido :'
@ 09, 5 SAY 'Numero de Orden   :'
@ 10, 5 SAY 'Fecha de Pedido   :'
@ 11, 5 SAY 'Codigo Tecnico    :'
@ 12, 5 SAY 'Codigo Almacen    :'
ppas = .T.
DO WHILE ppas
     efecin = 1
     @ 8, 25 CLEAR TO 12, 71
     @ 14, 2 CLEAR TO 32, 72
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
     wk_numped = 0
     wk_numord = 0
     wk_fecing = DATE()
     wk_codtec = 0
     wk_codalm = SPACE(4)
     @ 08, 25 GET wk_numped  ;
       PICTURE '99999999' VALID  ;
       numped2(wk_numped) WHEN  ;
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
     wk_numaux = STR(wk_numped,  ;
                 8)
     USE SHARED st_iprep ORDER  ;
         CODIGO
     SEEK '&wk_numaux'
     @ 09, 25 SAY numord
     @ 10, 25 SAY fecemi
     @ 11, 25 SAY codtec
     @ 12, 25 SAY codalm
     wk_numord = numord
     wk_codtec = codtec
     wk_codalm = 'ALMA' + codalm
     USE
     USE SHARED st_itecn ORDER  ;
         CODIGO
     SEEK '&wk_codtec'
     @ 11, 35 SAY noment
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_codalm'
     @ 12, 35 SAY  ;
       SUBSTR(tab_destab, 1, 35)
     USE
     @ 16, 2 TO 18, 72
     @ 17, 3 SAY  ;
       'Cantidad Codigo               Descripcion                            '
     @ 18, 2 TO 32, 72
     @ 18, 2 SAY 'Ã'
     @ 18, 72 SAY '´'
     DIMENSION pro( 12), can( 12),  ;
               dex( 12), dev(  ;
               12)
     FOR i = 1 TO 12
          pro( i) = SPACE(14)
          can( i) = 0
          dex( i) = SPACE(35)
          dev( i) = 0
     ENDFOR
     SELECT 1
     USE SHARED st_idped ORDER  ;
         CODIGO
     SEEK '&wk_numaux'
     i = 1
     DO WHILE wk_numaux==numdoc
          pro( i) = codpro
          can( i) = canpro
          wk_proaux = codpro
          SELECT 2
          USE SHARED gc_pro00  ;
              ORDER codigo
          SEEK '&wk_proaux'
          dex( i) =  ;
             SUBSTR(pro_descri, 1,  ;
             35)
          USE
          SELECT 1
          SKIP
          i = i + 1
     ENDDO
     USE
     FOR i = 1 TO 12
          IF can(i) <> 0 .AND.  ;
             pro(i) <> SPACE(14)
               @ 18 + i, 3 SAY  ;
                 can(i) PICTURE  ;
                 '99999.99'
               @ 18 + i, 12 SAY  ;
                 pro(i) PICTURE  ;
                 '!!!!!!!!!!!!!!'
               @ 18 + i, 27 SAY  ;
                 dex(i) PICTURE  ;
                 '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          ENDIF
     ENDFOR
     DO esc_modo WITH 'C'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     keyx = 0
     DO WHILE keyx<>27 .AND. keyx<>- ;
        9
          keyx = INKEY(0)
     ENDDO
     IF keyx == -9
          LOOP
     ENDIF
     IF keyx == 27
          ppas = .F.
          LOOP
     ENDIF
     USE
ENDDO
ON KEY LABEL F6
ON KEY LABEL F10
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO saca_win
RETURN
*
PROCEDURE ayuda13
ON KEY LABEL F6
IF VARREAD() == 'WK_NUMPED'
     USE SHARED st_iprep ORDER  ;
         REP_NUMORD
     GOTO TOP
     campo = 'numdoc+" "+numord+" "+dtoc(fecemi)+"   "+codalm+"  "+codtec'
     titulo = 'PEDIDO    ORDEN   FECHA   ALMACEN  TECNICO'
     DO ayuda1 WITH campo, titulo,  ;
        'numdoc'
     USE
ENDIF
ON KEY LABEL F6 do ayuda13
RETURN
*
FUNCTION numped2
PARAMETER num
IF num == 0
     DO error2 WITH  ;
        '** Error N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
USE SHARED st_iprep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error2 WITH  ;
        '** N§ Pedido Repuestos NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N'
     USE
     DO error2 WITH  ;
        '** N§ Pedido Repuestos ANULADO. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
DO sacaf6
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
