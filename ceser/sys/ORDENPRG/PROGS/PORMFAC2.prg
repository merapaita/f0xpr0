*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
IF w_codcli = 106635710
     w_codcli = 37270628
     w_nomcli = 'SONY SUCURSAL DEL PERU'
     w_dir = 'AV. REP. DE PANAMA 3531 - SAN ISIDRO'
ENDIF
@ 03, 56 SAY 'R.U.C.  ' +  ;
  rge_nroruc
@ 11, 00 SAY w_fecven
@ 13, 00 SAY w_nomcli
@ 13, 62 SAY STR(w_codcli, 11)
@ 15, 00 SAY w_dir
@ 17, 28 SAY ALLTRIM(w_desmon)
@ 17, 45 SAY ALLTRIM(w_despag)
@ 23, 16 SAY  ;
  'Por el servicio de reparaci¢n'
@ 24, 16 SAY  ;
  'de artefactos electrodom‚sticos'
@ 25, 16 SAY  ;
  'seg£n Ordenes de Reparaci¢n adjuntas'
IF w_codmon = 'DOL '
     IF w_flete > 0
          @ 34, 01 SAY 'Flete :'
          @ 34, 10 SAY  ;
            ROUND(w_flete * (1 +  ;
            w_facigv), 2) PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 35, 10 SAY ROUND((w_repues *  ;
       (1 + w_facigv)), 2)  ;
       PICTURE '99,999,999.99'
     @ 36, 10 SAY ROUND((w_mano *  ;
       (1 + w_facigv)), 2)  ;
       PICTURE '99,999,999.99'
     @ 36, 65 SAY w_totvta  ;
       PICTURE '99,999,999.99'
     @ 37, 10 SAY w_totdes  ;
       PICTURE '99,999,999.99'
     @ 37, 65 SAY w_totigv  ;
       PICTURE '99,999,999.99'
     IF ctatot > 0
          @ 38, 10 SAY ctatot  ;
            PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 38, 65 SAY w_totbru  ;
       PICTURE '99,999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(w_totbru, ;
       w_codmon)
ELSE
     IF s_flete > 0
          @ 34, 01 SAY 'Flete :'
          @ 34, 10 SAY  ;
            ROUND(s_flete * (1 +  ;
            w_facigv), 2) PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 35, 10 SAY ROUND((s_repues *  ;
       (1 + w_facigv)), 2)  ;
       PICTURE '99,999,999.99'
     @ 36, 10 SAY ROUND((s_mano *  ;
       (1 + w_facigv)), 2)  ;
       PICTURE '99,999,999.99'
     @ 36, 65 SAY s_totvta  ;
       PICTURE '99,999,999.99'
     @ 37, 10 SAY s_descue  ;
       PICTURE '99,999,999.99'
     @ 37, 65 SAY s_totigv  ;
       PICTURE '99,999,999.99'
     IF s_ctatot > 0
          @ 38, 10 SAY s_ctatot  ;
            PICTURE  ;
            '99,999,999.99'
     ENDIF
     @ 38, 65 SAY s_totbru  ;
       PICTURE '99,999,999.99'
     @ 40, 01 SAY  ;
       oonumlet(s_totbru, ;
       w_codmon)
ENDIF
@ 44, 62 SAY w_numdoc
*
*** 
*** ReFox - retrace your steps ... 
***
