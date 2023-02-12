*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
wrk_prg = PROGRAM()
pagina = 0
DEFINE POPUP vent01 FROM 07, 05  ;
       IN screen COLOR SCHEME 20
DEFINE BAR 1 OF vent01 PROMPT  ;
       'Ordenado Por \<Codigo   '
DEFINE BAR 2 OF vent01 PROMPT  ;
       'Ordenado Por \<Descripcion '
ON SELECTION POPUP vent01 DO PROCESO1;
WITH BAR()
DEFINE POPUP impresora FROM 18,  ;
       10 IN screen COLOR SCHEME  ;
       20
DEFINE BAR 1 OF impresora PROMPT  ;
       '  Por  \<Pantalla   '
DEFINE BAR 2 OF impresora PROMPT  ;
       '  Por  \<Impresora  '
ON SELECTION POPUP impresora DO REPORTS;
WITH BAR()
ACTIVATE POPUP vent01
CLOSE DATABASES
RELEASE POPUP vent01
ACTIVATE SCREEN
RETURN
*
PROCEDURE proceso1
PARAMETER bar
DO CASE
     CASE bar = 1
          SELECT gc_pal00.pal_codpro,  ;
                 gc_pro00.pro_descri,  ;
                 gc_pro00.pro_unimed,  ;
                 gc_pal00.pal_copral  ;
                 FROM GC_PAL00,  ;
                 GC_PRO00 WHERE  ;
                 gc_pro00.pro_codpro =  ;
                 gc_pal00.pal_codpro  ;
                 ORDER BY  ;
                 gc_pal00.pal_codpro,  ;
                 gc_pal00.pal_copral  ;
                 INTO CURSOR  ;
                 ALTER
     CASE bar = 2
          SELECT gc_pal00.pal_codpro,  ;
                 gc_pro00.pro_descri,  ;
                 gc_pro00.pro_unimed,  ;
                 gc_pal00.pal_copral  ;
                 FROM GC_PAL00,  ;
                 GC_PRO00 WHERE  ;
                 gc_pro00.pro_codpro =  ;
                 gc_pal00.pal_codpro  ;
                 ORDER BY  ;
                 gc_pro00.pro_descri,  ;
                 gc_pal00.pal_copral  ;
                 INTO CURSOR  ;
                 ALTER
ENDCASE
DO imprime
RETURN
*
PROCEDURE imprime
ON KEY
STORE 70 TO aux_lin
ON KEY
rpt = .T.
rpt = f_yesno( ;
      'PREPARE LA IMPRESORA Y CONFIRME ACCION' ;
      )
IF rpt
     stat = SYS(13)
     DO WHILE stat='OFFLINE'
          swt = f_yesno( ;
                'LA IMPRESORA NO ESTA LISTA, VERIFIQUE POR FAVOR' ;
                )
          IF swt
               stat = SYS(13)
          ELSE
               DEACTIVATE POPUP  ;
                          impresora
               SET CURSOR OFF
               RETURN
          ENDIF
     ENDDO
     SET DEVICE TO PRINTER
     DO imprimir
ENDIF
RETURN
*
PROCEDURE imprimir
DO r_titulo
@ aux_lin, 00 SAY CHR(27) + 'F'
wrk_despro = SPACE(14)
wrk_unipro = SPACE(4)
wrk_produc = SPACE(14)
DO WHILE  .NOT. EOF()
     IF aux_lin > 60
          DO r_titulo
     ENDIF
     aux_lin = aux_lin + 1
     IF wrk_produc <> pal_codpro
          aux_lin = aux_lin + 1
          @ aux_lin, 01 SAY  ;
            pal_codpro
          @ aux_lin, 17 SAY  ;
            pro_unimed
          @ aux_lin, 23 SAY  ;
            pro_descri
          wrk_produc = pal_codpro
          aux_lin = aux_lin + 2
     ENDIF
     @ aux_lin, 22 SAY pal_copral
     = oodespro(pal_copral)
     = oounipro(pal_copral)
     @ aux_lin, 38 SAY wrk_unipro
     @ aux_lin, 44 SAY  ;
       SUBSTR(wrk_despro, 1, 34)
     SKIP
ENDDO
@ 61, 00 SAY ''
@ 00, 00 SAY CHR(27) + 'P'
EJECT
SET DEVICE TO SCREEN
SET PRINTER TO
RETURN
*
PROCEDURE r_titulo
pagina = pagina + 1
@ 00, 00 SAY CHR(27) + 'U' + '1'
@ 00, 00 SAY CHR(27) + 'M'
@ 00, 00 SAY CHR(27) + 'P' +  ;
  CHR(18)
@ 00, 00 SAY rge_razsoc
@ 00, 59 SAY 'PAGINA   : ' +  ;
  STR(pagina, 8, 0)
@ 01, 00 SAY '       ' +  ;
  rge_abrev
@ 01, 59 SAY 'FECHA    : ' +  ;
  DTOC(DATE())
@ 02, 02 SAY rge_calle
@ 02, 59 SAY 'PROGRAMA : ' +  ;
  wrk_prg
@ 04, 00 SAY CHR(14)
IF bar = 1
     @ 04, 10 SAY  ;
       'PRODUCTOS ALTERNATIVOS'
     @ 05, 00 SAY CHR(14)
     @ 05, 10 SAY  ;
       'POR ORDEN DE CODIGO'
ELSE
     @ 04, 10 SAY  ;
       'PRODUCTOS ALTERNATIVOS'
     @ 05, 00 SAY CHR(14)
     @ 05, 10 SAY  ;
       'POR ORDEN DE DESCRIPCION'
ENDIF
@ 06, 00 SAY CHR(27) + 'F'
@ 06, 00 SAY CHR(20)
@ 06, 00 SAY  ;
  'ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿'
@ 07, 00 SAY  ;
  '³  PRODUCTO      UND.     DESCRIPCION                                          ³'
@ 08, 00 SAY  ;
  '³                     ALTERNATIVO     UND.        DESCRIPCION                  ³'
@ 09, 00 SAY  ;
  'ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ'
aux_lin = 09
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
