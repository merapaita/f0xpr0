*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
titu1 = 'REPORTE'
titu2 = 'CONSOLIDADO DE O/R FACTURADAS EN GTIA.'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 do ayuda
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
DO esc_modo WITH 'I'
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'INT'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'IGN', 'ESC'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
STORE 0 TO x, wk_numaux, wk_tem,  ;
      wk_con
STORE SPACE(30) TO wrk_desmar,  ;
      wrk_despro, wrk_desdis
CLOSE DATABASES
SELECT 1
USE SHARED ST_IOREP ORDER codigo
SELECT 2
USE SHARED gc_hve00 ORDER codigo
SELECT 3
USE SHARED st_isrep ORDER codigo
SELECT 4
USE SHARED st_imode ORDER codigo
SELECT 5
USE SHARED st_sicli ORDER codigo
SELECT 6
USE SHARED st_sint ORDER  ;
    sin_lincod
SELECT 7
USE SHARED st_itecn ORDER codigo
SELECT 8
USE SHARED st_iclpr ORDER codigo
SELECT 9
USE SHARED st_iseri ORDER  ;
    ser_codmar
SELECT 10
USE SHARED ge_tab0 ORDER codigo
SELECT 11
USE SHARED gc_pro00 ORDER codigo
SELECT 12
USE SHARED st_iprep ORDER  ;
    rep_numord
SELECT 13
USE SHARED st_idped ORDER codigo
SET RELATION TO codpro INTO gc_pro00
DO WHILE .T.
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IGN', 'ESC'
     DIMENSION orden[ 1]
     DIMENSION wk_codsin( 15)
     DIMENSION wk_sin( 8)
     efecin = 1
     STORE 0 TO x, wrk_numfac,  ;
           wk_tem, wk_con
     @ 08, 20 SAY  ;
       'N§ de Factura : ' GET  ;
       wrk_numfac PICTURE  ;
       '9999999999' WHEN  ;
       colocaf6()
     SET CURSOR ON
     READ
     SET CURSOR OFF
     IF LASTKEY() = 27
          EXIT
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'IMP', 'ESC'
     DO WHILE LASTKEY()<>-6 .AND.  ;
        LASTKEY()<>27
          wk_key = INKEY(0, 'H')
     ENDDO
     IF wk_key = -6
          DO imprime
     ENDIF
ENDDO
CLOSE DATABASES
DO sacawin
RETURN
*
FUNCTION numfac
PARAMETER wrk_numfac
wrk_numfac = f_ceros(wrk_numfac, ;
             10,1)
SELECT gc_hve00
SEEK 'LIQU' + wrk_numfac
IF  .NOT. FOUND()
     SEEK 'FACT' + wrk_numfac
ENDIF
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** N£mero de Factura No Existe ***'
     RETURN .F.
ENDIF
DO CASE
     CASE hve_estdoc = 'A' .OR.  ;
          hve_estdoc = 'N'
          DO error WITH  ;
             '*** N£mero de Factura Anulada ***'
          RETURN .F.
     CASE hve_estdoc = 'C'
          DO error WITH  ;
             '*** Factura Corresponde a Venta de Rptos. ***'
          RETURN .F.
     CASE hve_estdoc <> 'O'
          DO error WITH  ;
             '*** Factura no Corresponde a una O/R ***'
          RETURN .F.
ENDCASE
RETURN
*
FUNCTION imprime
wrk_numfac = f_ceros(wrk_numfac, ;
             10,1)
DO mensa WITH  ;
   '*** P r o c e s a n d o ... ***',  ;
   'COLO'
SELECT st_iorep
SET ORDER TO ord_numfab
SEEK 'LIQU' + wrk_numfac
IF  .NOT. FOUND()
     SEEK 'FACT' + wrk_numfac
ENDIF
c = 0
IF FOUND()
     SCAN WHILE  .NOT. EOF()  ;
          .AND. numfabo =  ;
          wrk_numfac
          c = c + 1
          DIMENSION orden[ c]
          orden[ c] = numdoc
     ENDSCAN
ELSE
     DO mensa WITH  ;
        '*** P r o c e s a n d o ... ***',  ;
        'SACA'
     DO error WITH  ;
        '*** No existen Registros a Listar ... ***'
     RETURN .F.
ENDIF
SET ORDER TO CODIGO
DO mensa WITH  ;
   '*** P r o c e s a n d o ... ***',  ;
   'SACA'
DO mensa WITH  ;
   '*** I m p r i m i e n d o ... ***',  ;
   'COLO'
SET PRINTER ON
SET DEVICE TO PRINTER
@ PROW(), PCOL() SAY CHR(15)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
SET CONSOLE OFF
FOR a = 1 TO c
     FOR i = 1 TO 15
          wk_codsin( i) =  ;
                   SPACE(35)
     ENDFOR
     wrk_numord = orden(a)
     SELECT st_iorep
     SEEK wrk_numord
     IF FOUND()
          wrk_fecemi = fecemi
          wk_horemi = horemi
          wrk_numsol = numsol
          wrk_codcli = codent
          wrk_modelo = codmod
          wrk_codmar = codmar
          wrk_numser = numser
          wrk_tipgar = indori
          wrk_codtec = codtec
          wrk_codemi = codemi
          wrk_inftec = observ
          wrk_codtal = codtall
          wrk_auxest = auxest
          SELECT st_isrep
          SEEK wrk_numsol
          wrk_observ = observ
          wrk_desace = desace
          wrk_feccom = feccom
          SELECT st_imode
          SEEK wrk_codmar +  ;
               wrk_modelo
          wrk_desmod = nommod
          wrk_codcla = codcla
          w_linea = linea
          SELECT st_sicli
          SEEK wrk_numsol
          i = 1
          IF FOUND()
               SCAN WHILE  .NOT.  ;
                    EOF() .AND.  ;
                    numdoc ==  ;
                    wrk_numsol
                    wk_aux2 = w_linea +  ;
                              SUBSTR(codsin,  ;
                              2,  ;
                              3)
                    SELECT st_sint
                    seek '&wk_aux2'
                    wk_codsin( i) =  ;
                             SUBSTR(dessin,  ;
                             1,  ;
                             35)
                    i = i + 1
                    SELECT st_sicli
               ENDSCAN
          ENDIF
          SELECT st_itecn
          SEEK wrk_codtec
          wrk_destec = noment
          SELECT st_iclpr
          SEEK 'C' + wrk_codcli
          wrk_descli = noment
          wrk_direcc = nomcal
          wrk_coddis = nomdis
          wrk_codpro = nomciu
          wrk_numtel = numte1
          SELECT st_iseri
          SEEK wrk_codmar +  ;
               wrk_modelo +  ;
               wrk_numser
          wrk_docgar = DTOC(fecvta) +  ;
                       '  ' +  ;
                       ALLTRIM(docgar)
          SELECT st_iprep
          wrk_rep = 0
          i = 1
          SEEK wrk_numord
          SCAN WHILE numord =  ;
               wrk_numord .AND.   ;
               .NOT. EOF()
               IF indest <> 'N'
                    SELECT st_idped
                    SEEK st_iprep.numdoc
                    IF FOUND()
                         SCAN WHILE  ;
                              numdoc =  ;
                              st_iprep.numdoc  ;
                              .AND.   ;
                              .NOT.  ;
                              EOF()
                              IF canpro >  ;
                                 0
                                   PUBLIC dime, repue( i)
                                   repue( i) = gc_pro00.pro_codpro + SPACE(2) + gc_pro00.pro_descri + STR(canpro, 9, 2) + STR(valpro, 9, 2) + SPACE(1) + STR(totite, 9, 2) + SPACE(1) + st_iprep.codalm + SPACE(1) + numdoc + SPACE(1) + gc_pro00.pro_propie
                                   i = i + 1
                                   wrk_rep = wrk_rep + 1
                              ENDIF
                         ENDSCAN
                    ENDIF
               ENDIF
               SELECT st_iprep
          ENDSCAN
          SELECT st_iorep
          tit_tit2 = 'SINTOMAS :'
          tit_tit3 = 'ACCESORIOS :'
          tit_tit4 = 'OBSERVACIONES :'
          tit_tit5 = 'CONSOLIDADO DE ORDEN DE REPARACION'
          tit_subray = 'ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ'
          tit_tit6 = 'INF.TEC. / NOTAS :'
          tit_tit7 = 'CONSUMO DE REPUESTOS'
          DIMENSION wk_acc( 9),  ;
                    wk_obs( 3)
          wk_acc( 1) =  ;
                SUBSTR(wrk_desace,  ;
                01, 30)
          wk_acc( 2) =  ;
                SUBSTR(wrk_desace,  ;
                31, 30)
          wk_acc( 3) =  ;
                SUBSTR(wrk_desace,  ;
                61, 30)
          wk_acc( 4) =  ;
                SUBSTR(wrk_desace,  ;
                91, 30)
          wk_acc( 5) =  ;
                SUBSTR(wrk_desace,  ;
                121, 30)
          wk_tem = 1
          wk_con = wrk_rep / 5
          IF MOD(wrk_rep, 5) > 0
               wk_con = wk_con +  ;
                        1
          ENDIF
          IF wk_con = 0
               wk_con = 1
          ENDIF
          FOR h = 1 TO wk_con
               wk_tem = 1 + (5 *  ;
                        (h - 1))
               DO imprim
          ENDFOR
     ENDIF
ENDFOR
??? CHR(15)
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
DO mensa WITH  ;
   '*** I m p r i m i e n d o ... ***',  ;
   'SACA'
RETURN
*
PROCEDURE imprim
??? CHR(15)
@ 02, 070 - (LEN(tit_tit5) / 2)  ;
  SAY tit_tit5
@ 02, 115 SAY 'O/R  N§: ' +  ;
  wrk_numord
@ 03, 070 - (LEN(tit_subray) / 2)  ;
  SAY tit_subray
@ 03, 109 SAY wrk_codemi +  ;
  '  S/S  N§: ' + wrk_numsol
@ 04, 115 SAY DTOC(wrk_fecemi) +  ;
  ' - ' + wk_horemi
@ 05, 015 SAY wrk_descli
@ 05, 075 SAY wrk_codcli
@ 06, 015 SAY wrk_direcc
@ 06, 115 SAY wrk_feccom
@ 07, 015 SAY ootab('DIST', ;
  wrk_coddis)
@ 07, 075 SAY ootab('PROV', ;
  wrk_codpro)
@ 07, 113 SAY wrk_numtel
@ 09, 105 SAY SUBSTR(ootab('INGA', ;
  wrk_tipgar), 1, 25)
@ 10, 00 SAY ootab('CLAS', ;
  wrk_codcla)
@ 10, 030 SAY wrk_modelo
@ 10, 050 SAY SUBSTR(ootab('MARC', ;
  wrk_codmar), 1, 20)
@ 10, 075 SAY wrk_numser
@ 10, 105 SAY wrk_docgar
@ 11, 002 SAY tit_tit2
@ 11, 070 SAY tit_tit3
wk_sin( 1) = wk_codsin(1) +  ;
      SUBSTR(wk_codsin(2), 1,  ;
      30)
wk_sin( 2) = wk_codsin(3) +  ;
      SUBSTR(wk_codsin(4), 1,  ;
      30)
FOR lin = 1 TO 2
     @ 11 + lin, 02 SAY  ;
       wk_sin(lin)
     @ 11 + lin, 071 SAY  ;
       wk_acc(lin)
ENDFOR
@ 14, 02 SAY tit_tit7
@ 14, 70 SAY 'Repuesto.:'
@ 14, 80 SAY cosrep PICTURE  ;
  '$99,999.99'
@ 14, 92 SAY 'Total....:'
@ 14, 102 SAY totnet PICTURE  ;
  '$99,999.99'
@ 15, 02 SAY 'Estado:'
@ 15, 10 SAY ootab('ESOR', ;
  wrk_auxest)
@ 15, 70 SAY 'Mano Obra:'
@ 15, 80 SAY cosmob PICTURE  ;
  '$99,999.99'
@ 15, 92 SAY 'Igv......:'
@ 15, 102 SAY totigv PICTURE  ;
  '$99,999.99'
@ 16, 70 SAY 'Flete....:'
@ 16, 80 SAY flete PICTURE  ;
  '$99,999.99'
@ 16, 92 SAY 'T.General:'
@ 16, 102 SAY totbru PICTURE  ;
  '$99,999.99'
IF wrk_rep > 0
     @ 17, 08 SAY SPACE(14) +  ;
       'Descripci¢n' + SPACE(34) +  ;
       'Cant.' + SPACE(2) +  ;
       'P.Unit.' + SPACE(2) +  ;
       'P.Total' + SPACE(1) +  ;
       'Almac‚n' + SPACE(1) +  ;
       'NøPed.' + 'Propie'
     c = 1
     FOR i = wk_tem TO wrk_rep
          IF c = 1 .AND. wk_tem >  ;
             5
               @ 18, 01 SAY  ;
                 'VIE...'
          ENDIF
          @ 17 + c, 08 SAY  ;
            repue(i)
          IF c = 5
               @ 22, 124 SAY  ;
                 'VAN...'
               EXIT
          ENDIF
          c = c + 1
     ENDFOR
ENDIF
wk_fecha = DTOC(wrk_fecemi)
tit_fechoy = SUBSTR(wk_fecha, 1,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 4,  ;
             2) + SPACE(3) +  ;
             SUBSTR(wk_fecha, 7,  ;
             2)
@ 24, 02 SAY tit_tit6
@ 24, 25 SAY 'T‚cnico :'
@ 24, 38 SAY wrk_codtec
@ 24, 50 SAY SUBSTR(wrk_destec, 1,  ;
  25)
@ 25, 002 SAY SUBSTR(wrk_inftec,  ;
  001, 76)
@ 26, 002 SAY SUBSTR(wrk_inftec,  ;
  077, 76)
@ 27, 002 SAY SUBSTR(wrk_inftec,  ;
  153, 76)
@ 28, 002 SAY SUBSTR(wrk_inftec,  ;
  229, 76)
@ 28, 097 SAY tit_fechoy
RETURN
*
PROCEDURE ayuda
SELECT gc_hve00
SET FILTER TO hve_tipdoc = 'FACT';
.AND. hve_estdoc = 'O'
campoa = '" "+HVE_nROdoc+"   "+dtos(HVE_fecDOC)+"   "+HVE_NUMORE+"   "+STR(HVE_SOLREP,12)+"     "+HVE_ESTDOC'
doc = hve_tipdoc
DO ayuda8 WITH campoa, doc
ON KEY LABEL F6 do ayuda
RETURN
*
PROCEDURE choice2
IF LASTKEY() == 13
     wk_aux = ALLTRIM(hve_nrodoc)
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
