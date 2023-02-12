*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
ind_prg = '<PORQ0376>'
tit_prg = 'CONSULTA'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA13
ON KEY LABEL F10 DO FCINCO
ACTIVATE SCREEN
@ 24, 69 SAY SPACE(11)
ACTIVATE WINDOW trabajo
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' PEDIDOS A TRAVES DE ORDEN '
@ 05, 2 CLEAR TO 11, 72
@ 05, 2 TO 11, 72
@ 06, 5 SAY 'Numero de Orden...:'
@ 07, 5 SAY 'Numero del Pedido.:'
@ 08, 5 SAY 'Fecha de Pedido...:'
@ 09, 5 SAY 'Codigo Tecnico....:'
@ 10, 5 SAY 'Codigo Almacen....:'
ppas = .T.
DO WHILE ppas
     f10 = 0
     efecin = 1
     @ 6, 25 CLEAR TO 10, 71
     @ 13, 2 CLEAR TO 29, 72
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'IGN', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     wk_numped = 0
     wk_numord = 0
     wk_fecing = DATE()
     wk_codtec = 0
     wk_codalm = SPACE(4)
     @ 06, 25 GET wk_numord  ;
       PICTURE '99999999' VALID  ;
       numord(wk_numord) WHEN  ;
       colocaf6()
     @ 07, 25 GET wk_numped  ;
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
     @ 08, 25 SAY fecemi
     @ 09, 25 SAY codtec
     @ 10, 25 SAY codalm
     wk_numord = numord
     wk_codtec = codtec
     wk_codalm = 'ALMA' + codalm
     USE
     USE SHARED st_itecn ORDER  ;
         CODIGO
     SEEK '&wk_codtec'
     @ 09, 35 SAY noment
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SEEK '&wk_codalm'
     @ 10, 35 SAY  ;
       SUBSTR(tab_destab, 1, 35)
     USE
     @ 13, 2 TO 15, 72
     @ 14, 3 SAY  ;
       'Cantidad Codigo               Descripcion                       '
     @ 15, 2 TO 29, 72
     @ 15, 2 SAY 'Ã'
     @ 15, 72 SAY '´'
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
          IF pro(i) <> SPACE(14)
               @ 15 + i, 3 SAY  ;
                 can(i) PICTURE  ;
                 '99999.99'
               @ 15 + i, 12 SAY  ;
                 pro(i) PICTURE  ;
                 '!!!!!!!!!!!!!!'
               @ 15 + i, 27 SAY  ;
                 dex(i) PICTURE  ;
                 '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
          ENDIF
     ENDFOR
     SAVE SCREEN TO polo12
     DO mensa2 WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     pide = ' '
     DEFINE POPUP ayu FROM 0, 0  ;
            PROMPT FIELDS  ;
            kar_codpro + '  ' +  ;
            STR(kar_stkant, 6, 2) +  ;
            ' ' + STR(kar_cantid,  ;
            6, 2) + '    ' +  ;
            DTOC(kar_fecha) + ' ' +  ;
            kar_codmov TITLE  ;
            'CODIGOÄÄÄÄÄSTOCK.ANTÄCANTIDADÄÄFECHAÄÄCOD.MVTO.ÄÄÄÄÄÄÄÄÄÄ'  ;
            SHADOW MARK CHR(16)  ;
            MARGIN IN screen  ;
            SCROLL
     USE SHARED GC_KAR00 ORDER  ;
         CODIGO
     SELECT gc_kar00.kar_codpro,  ;
            gc_kar00.kar_codmov,  ;
            gc_kar00.kar_stkant,  ;
            gc_kar00.kar_cantid,  ;
            gc_kar00.kar_nrodoc,  ;
            gc_kar00.kar_tipdoc,  ;
            gc_kar00.kar_fecha  ;
            FROM GC_KAR00 WHERE  ;
            gc_kar00.kar_tipdoc =  ;
            'PED ' AND  ;
            gc_kar00.kar_nrodoc =  ;
            STR(wk_numped, 8)  ;
            INTO CURSOR AYU
     DO mensa2 WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'SACA'
     KEYBOARD '{DNARROW}' PLAIN
     SET DISPLAY TO VGA50
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 42, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 45, 0 TO 48, 76
     @ 31, 08 GET pide SIZE 07,  ;
       60 POPUP ayu
     ON KEY LABEL F10 DO SAL2
     READ
     ON KEY LABEL F10
     DEACTIVATE POPUP ayu
     RELEASE POPUP ayu
     USE
     RESTORE SCREEN FROM polo12
     IF LASTKEY() == 1
          LOOP
     ENDIF
     IF LASTKEY() == 27
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
@ 24, 69 SAY SPACE(10)
RETURN
*
PROCEDURE ayuda13
ON KEY LABEL F6
IF VARREAD() == 'WK_NUMPED'
     USE SHARED st_iprep ORDER  ;
         REP_NUMORD
     SET FILTER TO numord = STR(wk_numord,;
8)
     GOTO TOP
     campo = 'numdoc+" "+numord+" "+dtoc(fecemi)+"   "+codalm+"  "+codtec'
     titulo = 'PEDIDO    ORDEN   FECHA   ALMACEN  TECNICO'
     DO ayuda1 WITH campo, titulo,  ;
        'numdoc'
     SET FILTER TO
     USE
ENDIF
IF VARREAD() == 'WK_NUMORD' .AND.  ;
   config_prg > 1
     USE SHARED st_iorep ORDER  ;
         CODIGO
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
     USE
ENDIF
ON KEY LABEL F6 do ayuda13
RETURN
*
FUNCTION numped2
PARAMETER num
IF LASTKEY() = 5
     RETURN .T.
ENDIF
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
     RETURN .F.
ENDIF
IF numord <> STR(wk_numord, 8)
     USE
     DO error2 WITH  ;
        '** N§ Pedido, No corresponde a la ORDEN. **'
     RETURN .F.
ENDIF
IF indest == 'N'
     USE
     DO error2 WITH  ;
        '** N§ Pedido Repuestos ANULADO. **'
     RETURN .F.
ENDIF
USE
DO sacaf6
RETURN .T.
*
FUNCTION numord
PARAMETER num
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
IF num == 0
     DO error2 WITH  ;
        '** Error N§ debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
USE SHARED st_iorep ORDER CODIGO
SEEK '&wk_clave'
IF  .NOT. FOUND()
     USE
     DO error2 WITH  ;
        '** N§ Orden Reparacion NO EXISTE. **'
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE sal2
f10 = 1
CLEAR READ
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
