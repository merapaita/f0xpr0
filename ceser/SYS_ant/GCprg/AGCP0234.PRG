*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
SET CURSOR ON
CLOSE DATABASES
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 10, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 11, 02  ;
       TO 21, 77 NONE COLOR  ;
       SCHEME 7
SELECT 1
USE GC_INV00 ORDER CODIGO
SELECT 2
USE GC_PRO00 ORDER CODIGO
SELECT 3
USE GE_TAB0 ORDER CODIGO
ACTIVATE WINDOW tablas
DO p_prestab WITH 'ACTUALIZACION',  ;
   'INGRESO DE DIFERENCIAS',  ;
   'ACTUALIZACION'
@ 2, 63 SAY PROGRAM()
wrk_progra = PROGRAM()
DO WHILE .T.
     ON KEY
     STORE SPACE(4) TO wrk_codalm
     STORE SPACE(14) TO  ;
           wrk_codpro
     STORE 0 TO wrk_conteo
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 01, 02 SAY 'Almac‚n :'
     @ 02, 12 SAY 'C¢digo :'
     @ 03, 12 SAY 'Conteo :'
     @ 03, 50 SAY 'Sistema:'
     @ 04, 50 SAY 'F¡sico :'
     @ 01, 12 GET wrk_codalm  ;
       PICTURE '@!' VALID  ;
       despues(1) WHEN antes(1)  ;
       COLOR SCHEME 8
     READ
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW cabecera,  ;
                  detalle,  ;
                  winmensaje
          DO p_footer WITH  ;
             '100000000001001110001',  ;
             1
          ACTIVATE SCREEN
          RETURN
     ENDIF
     DO WHILE .T.
          STORE SPACE(14) TO  ;
                wrk_codpro
          STORE 0 TO wrk_conteo
          @ 02, 21 GET wrk_codpro  ;
            PICTURE '@!' VALID  ;
            despues(2) WHEN  ;
            antes(2) COLOR SCHEME  ;
            8
          @ 03, 21 GET wrk_conteo  ;
            COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               EXIT
          ENDIF
          SELECT gc_inv00
          SEEK wrk_codalm +  ;
               wrk_codpro
          IF  .NOT. FOUND()
               APPEND BLANK
               DO rbloquea
               REPLACE alm_codpro  ;
                       WITH  ;
                       wrk_codpro
               REPLACE alm_codalm  ;
                       WITH  ;
                       wrk_codalm
               UNLOCK
          ENDIF
          DO rbloquea
          REPLACE alm_stkres WITH  ;
                  wrk_conteo
          REPLACE alm_conteo WITH  ;
                  wrk_conteo
          REPLACE alm_coprdo WITH  ;
                  oocospro(wrk_codpro)
          REPLACE alm_coprso WITH  ;
                  oocosprb(wrk_codpro)
          REPLACE alm_usuari WITH  ;
                  clave
          REPLACE alm_fecha WITH  ;
                  DATE()
          REPLACE alm_hora WITH  ;
                  TIME()
          UNLOCK
     ENDDO
ENDDO
RETURN
*
PROCEDURE antes
PARAMETER wrk_opc1
DO CASE
     CASE wrk_opc1 = 1
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          ON KEY LABEL F6 DO BUSCA;
 WITH WRK_BUSCA,wrk_var
          wrk_busca = 'ALMA'
          wrk_var = 'wrk_codalm'
     CASE wrk_opc1 = 2
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
ENDCASE
*
FUNCTION despues
PARAMETER wrk_opc
DO CASE
     CASE wrk_opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          SELECT 3
          SEEK 'ALMA' +  ;
               wrk_codalm
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Almac‚n No Existe'
               RETURN .F.
          ENDIF
          @ 01, 17 SAY tab_destab
     CASE wrk_opc = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          SELECT 2
          SEEK wrk_codpro
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo de Producto No Existe'
               RETURN .F.
          ENDIF
          @ 02, 37 SAY  ;
            SUBSTR(pro_descri, 1,  ;
            30)
          SELECT gc_inv00
          SEEK wrk_codalm +  ;
               wrk_codpro
          IF FOUND()
               @ 03, 60 SAY  ;
                 gc_inv00.alm_stkfis  ;
                 PICTURE  ;
                 '99,999,999'
               @ 04, 60 SAY  ;
                 gc_inv00.alm_conteo  ;
                 PICTURE  ;
                 '99,999,999'
          ELSE
               @ 03, 60 SAY  ;
                 'SIN INVEN.'
               @ 04, 60 SAY  ;
                 SPACE(10)
          ENDIF
ENDCASE
*
*** 
*** ReFox - retrace your steps ... 
***
