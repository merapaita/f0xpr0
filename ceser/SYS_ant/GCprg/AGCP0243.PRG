*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW indicar FROM 20, 00  ;
       TO 23, 01 SHADOW
DEFINE WINDOW trabajo FROM 23, 78  ;
       TO 24, 79 SHADOW
wrk_progra = PROGRAM()
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SET DISPLAY TO VGA50
ZOOM WINDOW trabajo NORM FROM 01,  ;
     00 TO 42, 78
ZOOM WINDOW indicar NORM FROM 45,  ;
     00 TO 48, 78
ACTIVATE SCREEN
ACTIVATE WINDOW trabajo
SELECT 1
USE SHARED GC_PRO00 ORDER CODIGO
SELECT 2
USE SHARED GC_KAR00 ORDER CODIGO
SELECT 3
USE SHARED GC_ALM00 ORDER CODIGO
SELECT 4
USE SHARED ST_IPREP ORDER CODIGO
SELECT 5
USE SHARED ST_IDPED ORDER CODIGO
SELECT 6
USE SHARED ST_IOREP ORDER CODIGO
SELECT 7
USE SHARED ST_ITECN ORDER CODIGO
SELECT 8
USE SHARED GE_TAB0 ORDER CODIGO
DO p_prestab WITH 'MANTENCION',  ;
   ' ANULACION DE PEDIDOS DE REPUESTOS ',  ;
   'MODIFICA'
@ 2, 63 SAY PROGRAM()
@ 07, 2 CLEAR TO 13, 72
@ 07, 2 TO 13, 72
@ 08, 5 SAY 'N?mero del Pedido.:'
@ 09, 5 SAY 'N?mero de Orden...:'
@ 10, 5 SAY 'Fecha de Pedido...:'
@ 11, 5 SAY 'C?digo T?cnico....:'
@ 12, 5 SAY 'C?digo Almac?n....:'
ppas = .T.
DO WHILE ppas
     efecin = 1
     @ 8, 25 CLEAR TO 12, 71
     @ 14, 2 CLEAR TO 32, 72
     wk_numped = 0
     wk_numord = 0
     wk_fecing = DATE()
     wk_codtec = 0
     wk_codalm = SPACE(4)
     wrk_precio = 0
     @ 08, 25 GET wk_numped  ;
       PICTURE '99999999' VALID  ;
       numped2(wk_numped)
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
     SELECT st_iprep
     SEEK '&wk_numaux'
     @ 09, 25 SAY numord
     @ 10, 25 SAY fecemi
     @ 11, 25 SAY codtec
     @ 12, 25 SAY codalm
     wk_numord = numord
     wk_codtec = codtec
     wk_codalm = 'ALMA' + codalm
     wrk_codalm = codalm
     SELECT st_itecn
     SEEK '&wk_codtec'
     @ 11, 35 SAY noment
     SELECT ge_tab0
     SEEK '&wk_codalm'
     @ 12, 35 SAY  ;
       SUBSTR(tab_destab, 1, 35)
     @ 16, 2 TO 18, 72
     @ 17, 3 SAY  ;
       'Cantidad C?digo         Descripci?n                              Uni'
     @ 18, 2 TO 32, 72
     @ 18, 2 SAY '?'
     @ 18, 72 SAY '?'
     @ 35, 10 SAY  ;
       '[F2] = GRABAR                       [F10] = OTRA ORDEN'
     @ 37, 10 SAY  ;
       '                                    [ESC] = SALIR'
     SELECT st_idped
     SEEK '&wk_numaux'
     wrk_ped = 0
     IF FOUND()
          SCAN WHILE wk_numaux ==  ;
               numdoc .AND.   ;
               .NOT. EOF()
               wrk_ped = wrk_ped +  ;
                         1
          ENDSCAN
     ENDIF
     DIMENSION pro( wrk_ped),  ;
               can( wrk_ped),  ;
               dex( wrk_ped),  ;
               uni( wrk_ped),  ;
               cps( wrk_ped),  ;
               cpd( wrk_ped),  ;
               cuc( wrk_ped),  ;
               pre( wrk_ped)
     FOR i = 1 TO wrk_ped
          pro( i) = SPACE(14)
          can( i) = 0
          dex( i) = SPACE(40)
          cps( i) = 0
          cpd( i) = 0
          cuc( i) = 0
          uni( i) = 0
          pre( i) = 0
     ENDFOR
     SELECT st_idped
     SEEK '&wk_numaux'
     i = 1
     IF FOUND()
          DO WHILE wk_numaux== ;
             numdoc
               pro( i) = codpro
               can( i) = canpro
               pre( i) = valpro
               wk_proaux = codpro
               SELECT gc_pro00
               SEEK '&wk_proaux'
               dex( i) =  ;
                  SUBSTR(pro_descri,  ;
                  1, 40)
               uni( i) =  ;
                  SUBSTR(pro_unimed,  ;
                  1, 03)
               cpd( i) =  ;
                  pro_coprmo
               cps( i) =  ;
                  pro_coprmb
               cuc( i) =  ;
                  pro_ulcomb
               SELECT st_idped
               SKIP
               i = i + 1
          ENDDO
          FOR i = 1 TO wrk_ped
               IF can(i) <> 0  ;
                  .AND. pro(i) <>  ;
                  SPACE(14)
                    @ 18 + i, 3  ;
                      SAY can(i)  ;
                      PICTURE  ;
                      '99999.99'
                    @ 18 + i, 12  ;
                      SAY pro(i)
                    @ 18 + i, 27  ;
                      SAY dex(i)
                    @ 18 + i, 68  ;
                      SAY uni(i)
               ENDIF
          ENDFOR
     ENDIF
     keyx = 0
     DO WHILE keyx<>27 .AND. keyx<>- ;
        9 .AND. keyx<>-1
          keyx = INKEY(0)
     ENDDO
     IF keyx == -9
          LOOP
     ENDIF
     IF keyx == 27
          ppas = .F.
          LOOP
     ENDIF
     IF keyx == -1
          wk_sinoes = 'N'
          SET CURSOR ON
          @ 39, 22 SAY  ;
            'Confirma Anular ?  (S/N)'  ;
            COLOR SCHEME 8 GET  ;
            wk_sinoes PICTURE '!'  ;
            VALID wk_sinoes $  ;
            'SN'
          READ
          @ 39, 22 SAY  ;
            '                             '
          IF LASTKEY() == 27
               wk_sinoes = 'N'
          ENDIF
          IF wk_sinoes = 'S'
               wk_numaux = STR(wk_numped,  ;
                           8)
               SELECT st_iprep
               seek '&wk_numaux'
               DO rbloquea
               REPLACE indest  ;
                       WITH 'N'
               REPLACE user WITH  ;
                       clave,  ;
                       date WITH  ;
                       DATE(),  ;
                       time WITH  ;
                       TIME()
               UNLOCK
               FOR i = 1 TO  ;
                   wrk_ped
                    IF pro(i) <>  ;
                       SPACE(14)  ;
                       .AND.  ;
                       can(i) <>  ;
                       0
                         wk_clave =  ;
                          pro(i) +  ;
                          SUBSTR(wk_codalm,  ;
                          5, 4)
                         SELECT gc_alm00
                         SEEK '&wk_clave'
                         IF FOUND()
                              wk_antes9 =  ;
                               alm_stkfis
                              DO rbloquea
                              REPLACE  ;
                               alm_stkfis  ;
                               WITH  ;
                               alm_stkfis +  ;
                               can(i)
                              REPLACE  ;
                               alm_usuari  ;
                               WITH  ;
                               clave,  ;
                               alm_fecha  ;
                               WITH  ;
                               DATE(),  ;
                               alm_hora  ;
                               WITH  ;
                               TIME()
                              UNLOCK
                         ELSE
                              wk_antes9 =  ;
                               0
                         ENDIF
                         SELECT gc_kar00
                         APPEND BLANK
                         DO rbloquea
                         REPLACE kar_codpro  ;
                                 WITH  ;
                                 pro(i),  ;
                                 kar_fecing  ;
                                 WITH  ;
                                 DATE()
                         REPLACE kar_horing  ;
                                 WITH  ;
                                 TIME(),  ;
                                 kar_tipdoc  ;
                                 WITH  ;
                                 'PED '
                         REPLACE kar_nrodoc  ;
                                 WITH  ;
                                 STR(wk_numped,  ;
                                 10),  ;
                                 kar_codmov  ;
                                 WITH  ;
                                 'IFD '
                         REPLACE kar_codmon  ;
                                 WITH  ;
                                 'DOL ',  ;
                                 kar_unimed  ;
                                 WITH  ;
                                 'UNID'
                         REPLACE kar_stkant  ;
                                 WITH  ;
                                 wk_antes9,  ;
                                 kar_cantid  ;
                                 WITH  ;
                                 can(i)
                         REPLACE kar_fecha  ;
                                 WITH  ;
                                 DATE(),  ;
                                 kar_hora  ;
                                 WITH  ;
                                 TIME(),  ;
                                 kar_usuari  ;
                                 WITH  ;
                                 clave
                         REPLACE kar_almrec  ;
                                 WITH  ;
                                 wrk_codalm,  ;
                                 kar_fecdoc  ;
                                 WITH  ;
                                 DATE()
                         REPLACE kar_import  ;
                                 WITH  ;
                                 pre(i),  ;
                                 kar_cosant  ;
                                 WITH  ;
                                 cpd(i)
                         REPLACE kar_cosanb  ;
                                 WITH  ;
                                 cps(i),  ;
                                 kar_cosuni  ;
                                 WITH  ;
                                 cuc(i)
                         UNLOCK
                         SELECT gc_pro00
                         SEEK pro(i)
                         IF FOUND()
                              DO rbloquea
                              REPLACE  ;
                               pro_ultmov  ;
                               WITH  ;
                               DATE()
                              REPLACE  ;
                               pro_usuari  ;
                               WITH  ;
                               clave
                              REPLACE  ;
                               pro_hora  ;
                               WITH  ;
                               TIME()
                              REPLACE  ;
                               pro_fecha  ;
                               WITH  ;
                               DATE()
                              UNLOCK
                         ENDIF
                    ENDIF
               ENDFOR
          ENDIF
     ENDIF
ENDDO
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DEACTIVATE WINDOW indicar,  ;
           trabajo
CLOSE DATABASES
RELEASE WINDOW cabecera, detalle,  ;
        item, indicar, trabajo
DEACTIVATE WINDOW tablas, codigo,  ;
           footer
DO p_footer WITH  ;
   '100000000001001110001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION numped2
PARAMETER num
IF num == 0
     DO p_mensaje WITH  ;
        '*** Error N? debe ser Ingresado. ***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
wk_clave = STR(num, 8)
SELECT st_iprep
SEEK '&wk_clave'
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        '*** N? Pedido Repuestos NO EXISTE. ***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest == 'N'
     DO p_mensaje WITH  ;
        '*** N? Pedido Repuestos ANULADO.***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
wk_numord = numord
SELECT st_iorep
SEEK wk_numord
IF indest = 'C'
     DO p_mensaje WITH  ;
        '*** Orden de Reparaci?n CERRADA ***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest = 'N'
     DO p_mensaje WITH  ;
        '*** Orden de Reparaci?n ANULADA ***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF indest = 'F' .OR. indest = 'B'
     DO p_mensaje WITH  ;
        '*** Orden de Reparaci?n Facturada ***'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
RETURN .T.
*
*** 
*** ReFox - retrace your steps ... 
***
