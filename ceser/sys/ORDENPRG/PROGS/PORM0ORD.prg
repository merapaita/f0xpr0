*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER w_opcion
IF w_opcion = 'G'
     DO pormgui
ELSE
     IF w_contab > 1
          DO pormgui
     ELSE
          DO pormord
     ENDIF
ENDIF
RETURN
*
PROCEDURE pormord
@ 02, 70 - (LEN(w_tit5) / 2) SAY  ;
  w_tit5
@ 02, 115 SAY w_codemi + 'N§: ' +  ;
  w_numord
@ 03, 115 SAY 'S/S N§: ' +  ;
  w_numsol
@ 04, 115 SAY w_fecha
@ 05, 015 SAY w_noment
@ 05, 075 SAY w_codent
@ 06, 015 SAY w_nomcal
@ 06, 115 SAY w_feccom
@ 07, 015 SAY w_nomdis
@ 07, 115 SAY w_numte1
@ 08, 115 SAY w_numte2
@ 09, 105 SAY w_indori
@ 10, 001 SAY w_desmod
@ 10, 030 SAY w_codmod
@ 10, 050 SAY w_desmar
@ 10, 075 SAY w_numser
@ 10, 105 SAY ALLTRIM(w_doctia)
@ 11, 001 SAY w_tit2
@ 11, 070 SAY w_tit3
FOR lin = 1 TO 9
     IF w_codsin(lin) <>  ;
        SPACE(35)
          @ 11 + lin, 01 SAY  ;
            w_codsin(lin)
     ENDIF
     @ 11 + lin, 071 SAY  ;
       w_acceso(lin)
ENDFOR
@ 21, 001 SAY w_tit6
@ 21, 025 SAY 'Operador: ' +  ;
  SUBSTR(w_nomemp, 1, 25)
@ 21, 080 SAY 'T‚cnico :'
@ 21, 090 SAY w_codtec
@ 21, 103 SAY SUBSTR(w_destec, 1,  ;
  30)
@ 22, 001 SAY w_obsord(1)
@ 23, 001 SAY w_obsord(2)
@ 24, 001 SAY w_obsord(3)
@ 25, 001 SAY w_tit4
@ 26, 001 SAY SUBSTR(w_obssol(1),  ;
  1, 76)
@ 27, 001 SAY SUBSTR(w_obssol(1),  ;
  77, 14) + SUBSTR(w_obssol(2), 1,  ;
  62)
@ 28, 001 SAY SUBSTR(w_obssol(2),  ;
  63, 28) + SUBSTR(w_obssol(3), 1,  ;
  48)
@ 28, 110 SAY DTOC(DATE())
@ 29, 001 SAY SUBSTR(w_obssol(3),  ;
  49, 42)
RETURN
*
PROCEDURE pormgui
SELECT ge_tab0
IF w_opcion = 'G'
     SEEK 'GUIA' + '200 '
     IF FOUND()
          w_factor = tab_factor +  ;
                     1
          DO rbloquea
          REPLACE tab_factor WITH  ;
                  w_factor
          UNLOCK
     ENDIF
     IF SUBSTR(w_codemi, 1, 1) =  ;
        '3'
          @ 08, 075 SAY 'X'
          fil = 10
          @ fil, 020 SAY w_noment
          @ fil, 064 SAY w_codent
          @ fil, 090 SAY  ;
            DTOC(DATE())
          @ fil + 1, 020 SAY  ;
            w_nomcal
          @ fil + 1, 065 SAY  ;
            w_numte1
          @ fil + 1, 090 SAY  ;
            TIME()
     ELSE
          @ 08, 077 SAY 'X'
          fil = 10
          @ fil, 015 SAY w_noment
          @ fil, 080 SAY w_codent
          @ fil, 108 SAY  ;
            DTOC(DATE())
          @ fil, 118 SAY TIME()
          @ fil + 1, 015 SAY  ;
            w_nomcal
          @ fil + 1, 060 SAY  ;
            SUBSTR(w_nomdis, 1,  ;
            22)
          @ fil + 1, 105 SAY  ;
            w_numte1
     ENDIF
ELSE
     @ 02, 70 - (LEN(w_tit5) / 2)  ;
       SAY w_tit5
     fil = 4
     @ fil, 115 SAY w_fecha
     @ fil + 1, 020 SAY w_noment
     @ fil + 1, 062 SAY w_codent
     @ fil + 2, 020 SAY w_nomcal
     @ fil + 3, 015 SAY w_nomdis
     @ fil + 3, 115 SAY w_numte1
     @ fil + 4, 115 SAY w_numte2
ENDIF
IF w_opcion = 'G'
     SELECT st_iclpr
     w_emisor = SPACE(9 -  ;
                LEN(ALLTRIM(w_emisor))) +  ;
                ALLTRIM(w_emisor)
     w_emisor = STR(VAL(w_emisor),  ;
                9)
     SEEK 'C' + w_emisor
     IF FOUND()
          w_nomen2 = noment
          w_nomca2 = nomcal
          IF SUBSTR(w_codemi, 1,  ;
             1) = '3'
               @ 12, 024 SAY  ;
                 w_nomen2
               @ 13, 024 SAY  ;
                 w_nomca2
          ELSE
               @ 12, 015 SAY  ;
                 w_nomen2
               @ 13, 015 SAY  ;
                 w_nomca2
               SELECT ge_tab0
               SEEK 'DIST' +  ;
                    st_iclpr.nomdis
               IF FOUND()
                    @ 13, 060 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 22)
               ENDIF
          ENDIF
     ELSE
          STORE SPACE(10) TO  ;
                w_nomen2,  ;
                w_nomca2
     ENDIF
     fil = 16
ELSE
     @ 10, 04 SAY 'S/S' +  ;
       SPACE(05) + 'MARCA' +  ;
       SPACE(13) + 'MODELO' +  ;
       SPACE(19) + 'SERIE' +  ;
       SPACE(12) + 'ARTICULO' +  ;
       SPACE(20) +  ;
       'TIPO DE ATENCION'
     fil = 11
ENDIF
SELECT soli
GOTO TOP
SCAN WHILE  .NOT. EOF()
     IF w_opcion = 'G'
          @ fil, 000 SAY numdoc
          @ fil, 010 SAY  ;
            SUBSTR(marca, 1, 25)
          @ fil, 028 SAY  ;
            SUBSTR(codmod, 1,  ;
            20)
          @ fil, 050 SAY numser
          @ fil, 074 SAY articu
          @ fil, 100 SAY  ;
            ootab('INGA',indori)
          fil = fil + 1
     ELSE
          @ fil, 002 SAY numdoc +  ;
            SPACE(1)
          @ fil, 012 SAY marca +  ;
            SPACE(1)
          @ fil, 030 SAY codmod +  ;
            SPACE(1)
          @ fil, 055 SAY numser +  ;
            SPACE(1)
          @ fil, 072 SAY articu +  ;
            SPACE(1)
          @ fil, 100 SAY  ;
            ootab('INGA',indori)
          fil = fil + 1
     ENDIF
ENDSCAN
IF w_opcion = 'G'
     @ 24, 010 SAY w_nomen2
     @ 24, 051 SAY w_nomca2
     IF SUBSTR(w_codemi, 1, 1) =  ;
        '3'
          @ 27, 074 SAY  ;
            SUBSTR(w_nomdis, 1,  ;
            22)
     ENDIF
     @ 27, 075 SAY 'Operador: ' +  ;
       SUBSTR(w_nomemp, 1, 25)
     @ 28, 097 SAY w_factor  ;
       PICTURE '9999999999'
ELSE
     @ 23, 001 SAY 'Operador: ' +  ;
       SUBSTR(w_nomemp, 1, 25)
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
