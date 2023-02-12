*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'CONSULTA'
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONSULTA DE PEDIDOS POR DETALLE '
ON KEY
SET CURSOR ON
DEFINE WINDOW cabecera FROM 05,  ;
       05 TO 08, 70 IN screen  ;
       NONE
DEFINE WINDOW buscod FROM 12, 24  ;
       TO 14, 54 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW busqueda FROM 11,  ;
       13 TO 20, 64 IN screen  ;
       COLOR SCHEME 8
DEFINE WINDOW ven2 FROM 09, 00 TO  ;
       19, 76 IN screen COLOR  ;
       SCHEME 21
DEFINE WINDOW ven3 FROM 16, 13 TO  ;
       22, 69 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW ven4 FROM 18, 01 TO  ;
       20, 23 IN screen COLOR  ;
       SCHEME 20
DEFINE WINDOW ven5 FROM 16, 08 TO  ;
       22, 71 IN screen COLOR  ;
       SCHEME 10
DEFINE WINDOW orden FROM 07, 33  ;
       TO 11, 42 IN screen NONE  ;
       COLOR SCHEME 10
DEFINE POPUP ayu0 FROM 0, 0 TO 04,  ;
       12 COLOR SCHEME 8
STORE 0 TO lm, opc
STORE SPACE(10) TO estades
CLOSE DATABASES
SELECT 1
USE SHARED gc_hco00 ORDER codigo
SELECT 2
USE SHARED gc_dco00 ORDER codigp
SELECT 3
USE SHARED gc_ord00 ORDER  ;
    ord_refprp
SELECT 4
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT 5
USE SHARED st_idped ORDER codigo
SELECT 6
USE SHARED ge_tab0 ORDER codigo
SELECT 7
USE SHARED st_iorep ORDER codigo
SELECT 8
USE SHARED st_itecn ORDER codigo
w_codigo = SPACE(10)
tit = SPACE(77)
DIMENSION choices( 3)
opc = 1
w_tcod = 'DOC.REFER.:'
w_codigo = SPACE(10)
sigue = .T.
DO WHILE sigue
     DO esc_indica WITH 1, 'AYU',  ;
        'INI', 'ANT', 'BBB'
     DO esc_indica WITH 2, 'BUS',  ;
        'FIN', 'STE', 'ESC'
     ON KEY
     ON KEY LABEL f6 do ordena
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 00 TO 03, 70
     @ 01, 01 SAY w_tcod COLOR W/ ;
       N 
     @ 01, 12 SAY SPACE(14)
     SELECT gc_ord00
     @ 01, 12 GET w_codigo  ;
       PICTURE '@!' VALID  ;
       valcod() COLOR N/W 
     READ
     IF LASTKEY() = 27
          sigue = .F.
          DEACTIVATE WINDOW  ;
                     cabecera
     ELSE
          DO CASE
               CASE LASTKEY() = 1
                    GOTO TOP
               CASE LASTKEY() = 6
                    GOTO BOTTOM
               CASE LASTKEY() = 3
                    IF EOF()
                         GOTO BOTTOM
                    ELSE
                         SKIP lm
                    ENDIF
               CASE LASTKEY() =  ;
                    18
                    IF BOF()
                         GOTO TOP
                    ELSE
                         SKIP -1
                    ENDIF
          ENDCASE
          DO CASE
               CASE opc = 1
                    w_codigo = ord_docref
               CASE opc = 2
                    w_codigo = ord_codprp
               CASE opc = 3
                    w_codigo = ord_nrodoc
          ENDCASE
          @ 01, 12 SAY w_codigo  ;
            COLOR N/W 
          ACTIVATE WINDOW ven2
          CLEAR
          DO dato
     ENDIF
ENDDO
RELEASE WINDOW cabecera
DEACTIVATE WINDOW tablas
DEACTIVATE WINDOW ven2
DO sacawin
ON KEY
CLEAR READ
CLEAR GETS
CLOSE DATABASES
ACTIVATE SCREEN
RETURN
*
PROCEDURE dato
SELECT 20
USE SHARED gc_cli00 ORDER codigo
SELECT 21
USE SHARED gc_pro00 ORDER codigo
w_reg = 0
CREATE CURSOR detalle (codigo C  ;
       (14), codprp C (14),  ;
       descri C (30), soli N (6),  ;
       conf N (6), back N (6),  ;
       desp N (6), canc N (6),  ;
       pedido C (10), fecped D,  ;
       feclle D, prove C (30),  ;
       factu C (10), pedrep C (8),  ;
       docref C (10), estado C  ;
       (15), tecnico C (30),  ;
       desino C (15), hfeclle D)
SELECT gc_ord00
DO CASE
     CASE opc = 1
          w_cond = 'ord_docref=w_codigo'
     CASE opc = 2
          w_cond = 'ord_codprp=w_codigo'
     CASE opc = 3
          w_cond = 'ord_nrodoc=w_codigo'
ENDCASE
count to w_reg for &w_cond     
IF w_reg = 0
     @ 00, 00 SAY SPACE(50)
     DO error WITH  ;
        ' *** No hay consulta *** '
     RETURN
ENDIF
lm = 0
SEEK w_codigo
scan  while &w_cond and !eof()
     lm = lm + 1
     SELECT gc_hco00
     SEEK gc_ord00.ord_nrodoc
     IF FOUND()
          SELECT detalle
          APPEND BLANK
          REPLACE fecped WITH  ;
                  gc_hco00.hco_fecdoc
          REPLACE hfeclle WITH  ;
                  gc_hco00.hco_feclle
          REPLACE codigo WITH  ;
                  gc_ord00.ord_codpro
          REPLACE codprp WITH  ;
                  gc_ord00.ord_codprp
          REPLACE feclle WITH  ;
                  gc_ord00.ord_feclle
          REPLACE pedido WITH  ;
                  gc_ord00.ord_nrodoc
          REPLACE factu WITH  ;
                  gc_ord00.ord_numfac
          REPLACE docref WITH  ;
                  gc_ord00.ord_docref
          IF LEN(ALLTRIM(detalle.docref)) <=  ;
             8 .AND.  ;
             gc_ord00.ord_inorig =  ;
             'O'
               w_orden = SPACE(8 -  ;
                         LEN(ALLTRIM(detalle.docref))) +  ;
                         ALLTRIM(detalle.docref)
               SELECT st_iorep
               SEEK w_orden
               IF FOUND()
                    SELECT st_itecn
                    SEEK st_iorep.codtec
                    IF FOUND()
                         SELECT detalle
                         REPLACE tecnico  ;
                                 WITH  ;
                                 st_itecn.noment
                    ENDIF
               ENDIF
          ENDIF
          SELECT ge_tab0
          SEEK 'IMPO' +  ;
               gc_ord00.ord_indest
          IF FOUND()
               SELECT detalle
               REPLACE estado  ;
                       WITH  ;
                       SUBSTR(ge_tab0.tab_destab,  ;
                       1, 15)
          ENDIF
          SELECT ge_tab0
          SEEK 'OPED' +  ;
               gc_ord00.ord_inorig
          IF FOUND()
               SELECT detalle
               REPLACE desino  ;
                       WITH  ;
                       SUBSTR(ge_tab0.tab_destab,  ;
                       1, 15)
          ENDIF
          SELECT gc_pro00
          IF detalle.codigo <>  ;
             SPACE(14) .AND.  ;
             detalle.codigo <>  ;
             detalle.codprp
               SEEK detalle.codigo
          ELSE
               SEEK detalle.codprp
          ENDIF
          IF FOUND()
               SELECT detalle
               REPLACE descri  ;
                       WITH  ;
                       SUBSTR(gc_pro00.pro_descri,  ;
                       1, 20)
          ENDIF
          SELECT gc_cli00
          SEEK 'P' +  ;
               gc_hco00.hco_codent
          IF FOUND()
               SELECT detalle
               REPLACE prove WITH  ;
                       gc_cli00.cli_razsoc
          ENDIF
          SELECT gc_ord00
     ENDIF
ENDSCAN
w_ind = f_indice() + '.idx'
SELECT detalle
DO CASE
     CASE opc = 1
          index on docref+pedido+codprp;
to &w_ind
     CASE opc = 2
          index on codprp+pedido+docref;
to &w_ind
     CASE opc = 3
          index on pedido+codprp+docref;
to &w_ind
ENDCASE
DO esc_indica WITH 1, 'AYU',  ;
   'INI', 'ANT', 'INF'
IF opc <> 1
     DO esc_indica WITH 2, 'BUS',  ;
        'FIN', 'OTR', 'ESC'
ELSE
     DO esc_indica WITH 2, 'BBB',  ;
        'FIN', 'OTR', 'ESC'
ENDIF
ACTIVATE WINDOW cabecera
@ 01, 01 SAY w_tcod COLOR W/N 
@ 01, 12 SAY w_codigo COLOR N/W 
GOTO TOP
IF opc = 2
     @ 01, 28 SAY descri COLOR W/ ;
       N 
ELSE
     IF opc = 3
          @ 01, 40 SAY  ;
            'Fec. Doc.:' COLOR W/ ;
            N 
          @ 01, 52 SAY fecped  ;
            PICTURE '99/99/99'  ;
            COLOR W/N 
          @ 02, 01 SAY  ;
            'Proveedor :' COLOR W/ ;
            N 
          @ 02, 12 SAY prove  ;
            COLOR W/N 
          @ 02, 40 SAY  ;
            'Fec. LLe.:' COLOR W/ ;
            N 
          @ 02, 52 SAY hfeclle  ;
            PICTURE '99/99/99'  ;
            COLOR W/N 
     ENDIF
ENDIF
ACTIVATE WINDOW ven2
ON KEY
ON KEY LABEL home do inicio
ON KEY LABEL end do final
ON KEY LABEL f8 do Info
ON KEY LABEL f9 do rep
ON KEY LABEL f6 do busca  with opc
DO CASE
     CASE opc = 1
          BROWSE FIELDS pedido :  ;
                 10 :H =  ;
                 'N§ Pedido',  ;
                 feclle : 8 :H =  ;
                 'Fec.LLe.',  ;
                 codprp : 14 :H =  ;
                 'C¢digo  ',  ;
                 codigo : 14 :H =  ;
                 ' Reemplazo ',  ;
                 descri : 15 :H =  ;
                 'Descripci¢n',  ;
                 estado : 10 :H =  ;
                 'Estado', fecped  ;
                 : 8 :H =  ;
                 'Fec.Ped.',  ;
                 prove : 15 :H =  ;
                 'Proveedor',  ;
                 factu : 10 :H =  ;
                 'N§ Factura'  ;
                 NOEDIT IN ven2
     CASE opc = 2
          BROWSE FIELDS pedido :  ;
                 10 :H =  ;
                 'N§ Pedido',  ;
                 feclle : 8 :H =  ;
                 'Fec.LLe.',  ;
                 desino : 8 :H =  ;
                 'Tip.Doc.',  ;
                 docref : 10 :H =  ;
                 'Doc.Ref.',  ;
                 codigo : 14 :H =  ;
                 'Reemplazo',  ;
                 estado : 10 :H =  ;
                 'Estado', fecped  ;
                 : 8 :H =  ;
                 'Fec.Ped.',  ;
                 prove : 15 :H =  ;
                 'Proveedor',  ;
                 factu : 10 :H =  ;
                 'N§ Factura'  ;
                 NOEDIT IN ven2
     CASE opc = 3
          BROWSE FIELDS codprp :  ;
                 14 :H =  ;
                 'C¢digo  ',  ;
                 codigo : 14 :H =  ;
                 'Reemplazo',  ;
                 descri : 15 :H =  ;
                 'Descripci¢n',  ;
                 desino : 8 :H =  ;
                 'Tip.Doc.',  ;
                 docref : 10 :H =  ;
                 'Doc.Ref.',  ;
                 estado : 10 :H =  ;
                 'Estado',  ;
                 tecnico : 15 :H =  ;
                 'T‚cnico',  ;
                 feclle : 8 :H =  ;
                 'Fec.LLe.',  ;
                 factu : 10 :H =  ;
                 'N§ Factura'  ;
                 NOEDIT IN ven2
ENDCASE
IF LASTKEY() = 27
     DEACTIVATE WINDOW ven2
ENDIF
SELECT gc_ord00
RETURN
*
PROCEDURE inicio
GOTO TOP
RETURN
*
PROCEDURE final
GOTO BOTTOM
RETURN
*
PROCEDURE rep
IF LEN(ALLTRIM(detalle.docref)) <=  ;
   8 .AND.  ;
   ALLTRIM(detalle.desino) =  ;
   'ORDEN'
     CREATE CURSOR repues (pedrep  ;
            C (8), fecrep D,  ;
            codrep C (14), canrep  ;
            N (6, 0), prerep N  ;
            (10, 2), totrep N (10,  ;
            2))
     w_orden = SPACE(8 -  ;
               LEN(ALLTRIM(detalle.docref))) +  ;
               ALLTRIM(detalle.docref)
     SELECT st_iprep
     SEEK w_orden
     IF  .NOT. FOUND()
          DO error WITH  ;
             '** No se ha generado ningun Pedido de Repuesto **'
     ELSE
          SCAN WHILE w_orden =  ;
               numord .AND.   ;
               .NOT. EOF()
               IF ALLTRIM(indest) <>  ;
                  'N'
                    SELECT st_idped
                    SEEK st_iprep.numdoc +  ;
                         st_iprep.numord +  ;
                         detalle.codprp
                    IF FOUND()  ;
                       .AND.  ;
                       canpro >  ;
                       0
                         SELECT repues
                         APPEND BLANK
                         REPLACE pedrep  ;
                                 WITH  ;
                                 st_idped.numdoc
                         REPLACE fecrep  ;
                                 WITH  ;
                                 st_iprep.fecemi
                         REPLACE codrep  ;
                                 WITH  ;
                                 st_idped.codpro
                         REPLACE canrep  ;
                                 WITH  ;
                                 st_idped.canpro
                         REPLACE prerep  ;
                                 WITH  ;
                                 st_idped.valpro
                         REPLACE totrep  ;
                                 WITH  ;
                                 st_idped.totite
                    ENDIF
                    IF (detalle.codigo <>  ;
                       SPACE(14)  ;
                       .AND.  ;
                       detalle.codigo <>  ;
                       detalle.codprp)
                         SEEK st_iprep.numdoc +  ;
                              st_iprep.numord +  ;
                              detalle.codigo
                         IF FOUND()  ;
                            .AND.  ;
                            canpro >  ;
                            0
                              SELECT  ;
                               repues
                              APPEND  ;
                               BLANK
                              REPLACE  ;
                               pedrep  ;
                               WITH  ;
                               numdoc
                              REPLACE  ;
                               codrep  ;
                               WITH  ;
                               codpro
                              REPLACE  ;
                               canrep  ;
                               WITH  ;
                               canpro
                              REPLACE  ;
                               prerep  ;
                               WITH  ;
                               valpro
                              REPLACE  ;
                               totrep  ;
                               WITH  ;
                               totite
                         ENDIF
                    ENDIF
                    SELECT st_iprep
               ENDIF
          ENDSCAN
          SELECT repues
          GOTO TOP
          COUNT TO n
          IF n = 0
               DO error WITH  ;
                  '** No se ha generado ningun Pedido de Repuesto **'
          ELSE
               ACTIVATE WINDOW  ;
                        ven5
               ON KEY LABEL f8
               ON KEY LABEL f9
               GOTO TOP
               BROWSE FIELDS  ;
                      pedrep : 8  ;
                      :H =  ;
                      'Ped. Rep.',  ;
                      fecrep : 8  ;
                      :H =  ;
                      'Fec.Rep.',  ;
                      codrep : 14  ;
                      :H =  ;
                      'Cod.Rep.',  ;
                      canrep :P =  ;
                      '99,999' :H =  ;
                      'Cant.',  ;
                      prerep :P =  ;
                      '99,999.99'  ;
                      :H =  ;
                      'Precio ' +  ;
                      ALLTRIM(empre13),  ;
                      totrep :P =  ;
                      '99,999.99'  ;
                      :H =  ;
                      'Total ' +  ;
                      ALLTRIM(empre13)  ;
                      FREEZE  ;
                      pedrep  ;
                      NOEDIT IN  ;
                      ven5
               DEACTIVATE WINDOW  ;
                          ven5
               ACTIVATE WINDOW  ;
                        ven2
               SELECT detalle
               ON KEY LABEL f8 do info;

               ON KEY LABEL f9 do rep;

          ENDIF
     ENDIF
ELSE
     DO error WITH  ;
        '** Documento no es Orden de Reparaci¢n **'
ENDIF
SELECT detalle
RETURN
RETURN
*
PROCEDURE busca
PARAMETER w_opc
IF w_opc = 2 .OR. w_opc = 3
     w_docref = SPACE(10)
     ACTIVATE WINDOW ven4
     @ 00, 01 SAY 'Doc.Ref.:'
     @ 00, 10 GET w_docref  ;
       PICTURE '@!' COLOR N/W 
     READ
     IF LASTKEY() <> 27
          LOCATE FOR docref =  ;
                 ALLTRIM(w_docref)
     ENDIF
     DEACTIVATE WINDOW ven4
ENDIF
RETURN
*
PROCEDURE info
CREATE CURSOR deta1 (nrodoc C  ;
       (10), codigo C (14),  ;
       docref C (10), cansol N  ;
       (6), cancof N (6), canbor  ;
       N (6), cancel N (6),  ;
       candes N (6))
SELECT gc_dco00
SEEK detalle.pedido +  ;
     detalle.codprp
IF FOUND()
     SELECT deta1
     APPEND BLANK
     REPLACE nrodoc WITH  ;
             detalle.pedido
     REPLACE codigo WITH  ;
             detalle.codprp
     REPLACE docref WITH  ;
             detalle.docref
     REPLACE cansol WITH  ;
             gc_dco00.dco_cansol
     REPLACE cancof WITH  ;
             gc_dco00.dco_cancon
     REPLACE canbor WITH  ;
             gc_dco00.dco_canbor
     REPLACE candes WITH  ;
             gc_dco00.dco_candes
     REPLACE cancel WITH  ;
             gc_dco00.dco_cancel
     ACTIVATE WINDOW ven3
     ON KEY LABEL f8
     BROWSE FIELDS codigo : 14 :H =  ;
            'C¢digo ', cansol :P =  ;
            '99,999' :H =  ;
            'C.Soli.', cancof :P =  ;
            '99,999' :H =  ;
            'C.Conf.', canbor :P =  ;
            '99,999' :H =  ;
            'C.Back.', candes :P =  ;
            '99,999' :H =  ;
            'C.Desp.', cancel :P =  ;
            '99,999' :H =  ;
            'C.Canc.' FREEZE  ;
            codigo NOEDIT IN  ;
            ven3
     DEACTIVATE WINDOW ven3
     SELECT detalle
     ACTIVATE WINDOW ven2
     ON KEY LABEL f8 do info 
ENDIF
RETURN
*
FUNCTION valcod
SET NEAR ON
SEEK w_codigo
IF EOF()
     DO error WITH  ;
        ' *** CODIGO NO EXISTE *** '
     RETURN .F.
ELSE
     DO CASE
          CASE opc = 1
               w_codigo = ord_docref
          CASE opc = 2
               w_codigo = ord_codprp
          CASE opc = 3
               w_codigo = ord_nrodoc
     ENDCASE
ENDIF
SET NEAR OFF
RETURN .T.
*
PROCEDURE ordena
ON KEY
STORE 'DOC.REFER. ' TO choices(  ;
      1)
STORE 'CODIGO     ' TO choices(  ;
      2)
STORE 'N§ PEDIDO  ' TO choices(  ;
      3)
ACTIVATE WINDOW orden
FOR i = 1 TO 3
     DEFINE BAR i OF ayu0 PROMPT  ;
            choices(i)
ENDFOR
ON SELECTION POPUP ayu0 do choice0 with;
prompt()
ACTIVATE POPUP ayu0 NOWAIT
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
DEACTIVATE WINDOW orden
ACTIVATE WINDOW cabecera
@ 01, 01 SAY w_tcod COLOR W/N 
@ 01, 12 SAY SPACE(14)
@ 01, 12 SAY w_codigo COLOR N/W 
ON KEY LABEL f6 do ordena
RETURN
*
PROCEDURE choice0
PARAMETER clave
IF LASTKEY() == 13
     SELECT gc_ord00
     DO CASE
          CASE SUBSTR(clave, 1,  ;
               1) = 'D'
               opc = 1
               w_tcod = 'DOC.REFER.:'
               w_codigo = SPACE(10)
               SET ORDER TO ord_refprp
          CASE SUBSTR(clave, 1,  ;
               1) = 'C'
               opc = 2
               w_codigo = SPACE(14)
               w_tcod = 'CODIGO    :'
               SET ORDER TO ord_prdore
          CASE SUBSTR(clave, 1,  ;
               1) = 'N'
               opc = 3
               w_tcod = 'N§ PEDIDO :'
               w_codigo = SPACE(10)
               SET ORDER TO ord_docprp
     ENDCASE
     ACTIVATE WINDOW buscod
     @ 00, 02 SAY w_tcod
     @ 00, 13 GET w_codigo  ;
       PICTURE '@!'
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW  ;
                     buscod
     ELSE
          DEACTIVATE WINDOW  ;
                     buscod
          SET NEAR ON
          SEEK w_codigo
          SET NEAR OFF
          DO oobuscar WITH opc
          DEACTIVATE POPUP ayu0
     ENDIF
ENDIF
RETURN
*
PROCEDURE oobuscar
PARAMETER opc
DO CASE
     CASE opc = 1
          campo = 'ord_docref:h="Doc.Ref.",ord_nrodoc:h="N§ Pedido ",ord_codprp:h="C¢digo",ord_indest:h="Estado"'
          w_cla = 'ord_docref'
     CASE opc = 2
          campo = 'ord_codprp:h="C¢digo",ord_nrodoc:h="N§ Pedido ",ord_docref:h="Doc.Ref. ",ord_indest:h="Estado"'
          w_cla = 'ord_codprp'
     CASE opc = 3
          campo = 'ord_nrodoc:h="N§ Pedido",ord_codprp:h="C¢digo",ord_docref:h="Doc.Ref. ",ord_indest:h="Estado"'
          w_cla = 'ord_nrodoc'
ENDCASE
ACTIVATE WINDOW busqueda
ON KEY LABEL enter do llecod
browse field &campo  freeze &w_cla nomodify;
in window busqueda rest
DEACTIVATE WINDOW busqueda
ON KEY
RETURN
*
PROCEDURE llecod
ON KEY
w_codigo = &w_cla
DEACTIVATE WINDOW busqueda
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
