*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
cuenta_lin = 0
IF w_coddoc = 'FACT'
     @ 03, 56 SAY 'R.U.C.  ' +  ;
       rge_nroruc
     @ 11, 00 SAY DTOC(DATE())
     @ 11, 15 SAY w_codemi
     @ 13, 00 SAY w_nomcli
     @ 13, 58 SAY w_codcli
     @ 15, 00 SAY w_dir
     @ 17, 28 SAY w_desmon
     @ 17, 40 SAY SUBSTR(w_despa2,  ;
       1, 10)
     @ 17, 56 SAY 'S/S:' +  ;
       ALLTRIM(SUBSTR(w_numero, 1,  ;
       8))
     @ 17, 68 SAY 'O/R:' +  ;
       ALLTRIM(SUBSTR(w_numord, 1,  ;
       8))
ELSE
     @ 02, 56 SAY 'R.U.C.  ' +  ;
       rge_nroruc
     @ 11, 00 SAY DTOC(DATE())
     @ 11, 15 SAY w_codemi
     @ 13, 00 SAY w_nomcli
     @ 13, 58 SAY w_codcli
     @ 15, 00 SAY SUBSTR(w_dir, 1,  ;
       25)
     @ 15, 26 SAY 'ARTEF:' +  ;
       w_dmarc + ' ' + w_modelo +  ;
       ' ' + w_serie
     @ 17, 28 SAY w_desmon
     @ 17, 40 SAY SUBSTR(w_despa2,  ;
       1, 10)
     @ 17, 56 SAY 'S/S:' +  ;
       ALLTRIM(SUBSTR(w_numero, 1,  ;
       8))
     @ 17, 68 SAY 'O/R:' +  ;
       ALLTRIM(SUBSTR(w_numord, 1,  ;
       8))
ENDIF
w_des = 0
IF w_desmob <> 0 .OR. w_desrep <>  ;
   0
     w_des = 2
ENDIF
SELECT factu
GOTO TOP
DO WHILE  .NOT. EOF()
     cuenta_lin = cuenta_lin + 1
     w_codpro = codigo
     w_canpro = cantid
     w_descri = descri
     @ lin, 00 SAY cuenta_lin  ;
       PICTURE '99'
     @ lin, 18 SAY w_descri  ;
       PICTURE '@!'
     @ lin, 47 SAY w_canpro  ;
       PICTURE '999,999'
     IF w_coddoc = 'FACT'
          w_valpro = precio
     ELSE
          w_valpro = ROUND(precio *  ;
                     (1 +  ;
                     w_facigv),  ;
                     2)
     ENDIF
     IF w_codmon = rge_monbas
          w_x1 = ROUND(w_valpro *  ;
                 w_tipcam, 2)
     ELSE
          w_x1 = w_valpro
     ENDIF
     w_x2 = ROUND((w_x1 *  ;
            w_canpro), 2)
     @ lin, 53 SAY w_x1 PICTURE  ;
       '99,999,999.99'
     @ lin, 67 SAY w_x2 PICTURE  ;
       '99,999,999.99'
     w_totcan = w_totcan + w_x2
     lin = lin + 1
     IF w_coddoc = 'BOLE'
          IF lin = 35 - w_des
               EXIT
          ENDIF
     ELSE
          IF lin = 35 - w_des
               EXIT
          ENDIF
     ENDIF
     SELECT factu
     SKIP
ENDDO
IF w_coddoc = 'FACT'
     IF w_desmob <> 0 .OR.  ;
        w_desrep <> 0
          @ 33, 01 SAY  ;
            'Descuento Especial : ' +  ;
            w_infdes
     ENDIF
     @ 34,01 say 'FLETE &w_moneda'
     @ 34, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_cosfle,  ;
       w_flete) PICTURE  ;
       '99,999,999.99'
     @ 35, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_subtot,  ;
       w_subtot) PICTURE  ;
       '99,999,999.99'
     @ 36, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_cosmob,  ;
       ROUND(w_mano * (1 +  ;
       w_facigv), 2)) PICTURE  ;
       '99,999,999.99'
     @ 36,62 say '&w_moneda'
     @ 36, 65 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totvta,  ;
       w_totvta) PICTURE  ;
       '9999,999,999.99'
     @ 37, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_descue,  ;
       w_totdes) PICTURE  ;
       '99,999,999.99'
     @ 37, 65 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totigv,  ;
       w_totigv) PICTURE  ;
       '9999,999,999.99'
     IF w_toacta > 0
          @ 38, 10 SAY  ;
            IIF(w_codmon =  ;
            rge_monbas, w_toacta,  ;
            w_dolac) PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 38, 65 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totpag,  ;
       w_totpag) PICTURE  ;
       '9999,999,999.99'
     IF w_codmon = rge_monbas
          @ 40, 01 SAY  ;
            oonumlet(sol_totpag, ;
            rge_monbas)
     ELSE
          @ 40, 01 SAY  ;
            oonumlet(w_totpag, ;
            'DOL ')
     ENDIF
     @ 42, 00 SAY w_infte1
     IF w_cont01 > 0
          @ 42, 40 SAY nrodoc(1) +  ;
            ' ' +  ;
            TRANSFORM(montos(1),  ;
            '99,999,999.99')
     ENDIF
     @ 43, 00 SAY w_infte2
     IF w_cont01 > 1
          @ 43, 40 SAY nrodoc(2) +  ;
            ' ' +  ;
            TRANSFORM(montos(2),  ;
            '99,999,999.99')
     ENDIF
     @ 44, 00 SAY w_infte3
     IF w_cont01 > 2
          @ 44, 40 SAY nrodoc(3) +  ;
            ' ' +  ;
            TRANSFORM(montos(3),  ;
            '99,999,999.99')
     ENDIF
     @ 44, 65 SAY numvta
ELSE
     IF w_desmob <> 0 .OR.  ;
        w_desrep <> 0
          @ 33, 01 SAY  ;
            'Descuento Especial : ' +  ;
            w_infdes
     ENDIF
     @ 35,07 say '&w_moneda'
     @ 35, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_cosfle,  ;
       w_flete) PICTURE  ;
       '99,999,999.99'
     @ 36, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_subtot,  ;
       w_subtot) PICTURE  ;
       '99,999,999.99'
     @ 36,62 say '&w_moneda'
     @ 36, 65 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totgen,  ;
       w_totbru) PICTURE  ;
       '9999,999,999.99'
     @ 37, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_cosmob,  ;
       ROUND(w_mano * (1 +  ;
       w_facigv), 2)) PICTURE  ;
       '99,999,999.99'
     IF w_toacta > 0
          @ 37, 65 SAY  ;
            IIF(w_codmon =  ;
            rge_monbas, w_toacta,  ;
            w_dolac) PICTURE  ;
            '9999,999,999.99'
     ENDIF
     @ 38, 10 SAY IIF(w_codmon =  ;
       rge_monbas, sol_descue,  ;
       w_totdes) PICTURE  ;
       '99,999,999.99'
     @ 38, 65 SAY IIF(w_codmon =  ;
       rge_monbas, sol_totpag,  ;
       w_totpag) PICTURE  ;
       '9999,999,999.99'
     IF w_codmon = rge_monbas
          @ 40, 01 SAY  ;
            oonumlet(sol_totpag, ;
            rge_monbas)
     ELSE
          @ 40, 01 SAY  ;
            oonumlet(w_totpag, ;
            'DOL ')
     ENDIF
     @ 41, 00 SAY w_infte1
     @ 42, 00 SAY w_infte2
     IF w_cont01 > 0
          @ 42, 40 SAY nrodoc(1) +  ;
            ' ' +  ;
            TRANSFORM(montos(1),  ;
            '99,999,999.99')
     ENDIF
     @ 43, 00 SAY w_infte3
     IF w_cont01 > 1
          @ 43, 40 SAY nrodoc(2) +  ;
            ' ' +  ;
            TRANSFORM(montos(2),  ;
            '99,999,999.99')
     ENDIF
     @ 44, 00 SAY w_infte4
     IF w_cont01 > 2
          @ 44, 40 SAY nrodoc(3) +  ;
            ' ' +  ;
            TRANSFORM(montos(3),  ;
            '99,999,999.99')
     ENDIF
     @ 44, 65 SAY numvta
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
