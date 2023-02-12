*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
SET CENTURY ON
DEFINE WINDOW cabecera FROM 08,  ;
       10 TO 18, 68 COLOR SCHEME  ;
       24
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER  ;
    pro_nivpro
SELECT 3
USE SHARED gc_dlp00 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESOS',  ;
   'DESCUENTOS A PRODUCTOS INACTIVOS',  ;
   'PROCESO'
@ 02, 63 SAY PROGRAM()
STORE .T. TO sigue
w_facigv = facigv()
IF w_facigv = 0
     sigue = .F.
ENDIF
STORE SPACE(4) TO w_inaini,  ;
      w_inafin, w_busca, w_titu
STORE 0 TO w_factor
ACTIVATE SCREEN
DO WHILE sigue
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ACTIVATE WINDOW cabecera
     CLEAR
     @ 00, 01 SAY  ;
       'Nivel de Movimiento:'
     @ 01, 17 SAY 'Del:'
     @ 02, 17 SAY ' al:'
     @ 04, 08 SAY  ;
       'Nuevo Factor:    %'
     @ 06, 13 SAY 'Procesa:'
     @ 01, 22 GET w_inaini  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) .AND.   ;
       .NOT. EMPTY(w_inaini) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 24
     @ 02, 22 GET w_inafin RANGE  ;
       w_inaini PICTURE '@!'  ;
       VALID oovalid(VARREAD())  ;
       .AND.  .NOT.  ;
       EMPTY(w_inafin) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 24
     @ 04, 22 GET w_factor RANGE  ;
       1,99 PICTURE '99' VALID   ;
       .NOT. EMPTY(w_factor) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 24
     @ 06, 22 GET opc3 DEFAULT 2  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Si ;No' WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 4
     READ CYCLE
     IF LASTKEY() = 27
          sigue = .F.
     ELSE
          IF opc3 = 1
               = ooaviso('PROCESANDO...PROCESANDO...')
               DO procesa
          ENDIF
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY LABEL F6
DEACTIVATE WINDOW tablas
RELEASE WINDOW cabecera,  ;
        winmensaje
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_INAINI'
          w_busca = 'NICR'
          wrk_var = 'w_inaini'
          w_titu = 'NIVEL DE MOVIMIENTO'
          ON KEY LABEL f6 do busca2 with;
w_busca,wrk_var,w_titu
     CASE cwhen = 'W_INAFIN'
          w_busca = 'NICR'
          wrk_var = 'w_inafin'
          w_titu = 'NIVEL DE MOVIMIENTO'
          ON KEY LABEL f6 do busca2 with;
w_busca,wrk_var,w_titu
     CASE cwhen = 'W_FACTOR'
          ON KEY LABEL f6
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
     CASE cwhen = 'OPC3'
          ON KEY LABEL f6
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
ON KEY
DO CASE
     CASE cvalid = 'W_INAINI'  ;
          .OR. cvalid =  ;
          'W_INAFIN'
          SELECT ge_tab0
          IF cvalid = 'W_INAINI'
               SEEK 'NICR' +  ;
                    w_inaini
          ELSE
               SEEK 'NICR' +  ;
                    w_inafin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo No Existe'
               ON KEY LABEL f6 do busca2;
with w_busca,wrk_var,w_titu
               RETURN .F.
          ENDIF
          IF cvalid = 'W_INAFIN'
               IF w_inafin <  ;
                  w_inaini
                    DO p_mensaje  ;
                       WITH  ;
                       'C¢digo mayor al Inicial'
                    ON KEY LABEL f6 do;
busca2 with w_busca,wrk_var,w_titu
                    RETURN .F.
               ENDIF
          ENDIF
          @ ROW(), 27 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            28)
ENDCASE
RETURN
*
PROCEDURE procesa
SELECT 20
USE SHARED gc_alm00 ORDER codigo
SELECT gc_pro00
SET NEAR ON
SEEK w_inaini
SET NEAR OFF
SCAN WHILE pro_nivcit <= w_inafin  ;
     .AND.  .NOT. EOF()
     IF pro_coremo <> 0
          w_totstk = stock2(gc_pro00.pro_codpro)
          IF w_totstk > 0
               DO agcpprec
          ENDIF
     ENDIF
     SELECT gc_pro00
ENDSCAN
? CHR(7)
? CHR(7)
RETURN
*
FUNCTION busca2
PARAMETER w_busca, wrk_var,  ;
          w_titu
ON KEY
DEFINE WINDOW bus_tab FROM 08, 23  ;
       TO 18, 63 TITLE w_titu IN  ;
       screen FOOTER  ;
       ' [F6] B£squeda x Descri '  ;
       COLOR SCHEME 8
DEFINE WINDOW b_tabla FROM 12, 31  ;
       TO 14, 54 TITLE ' Tablas '  ;
       IN screen
SELECT ge_tab0
SET ORDER TO descri
SET FILTER TO tab_codpre = w_busca;
.AND. (tab_codtab = 'D   ';
.OR. tab_codtab = 'E   ';
.OR. tab_codtab = 'F   ')
GOTO TOP
ACTIVATE WINDOW bus_tab
ON KEY LABEL enter do carga_codt
ON KEY LABEL f6 do busca_codt
BROWSE FIELDS tab_codtab :R :H =  ;
       'C¢digo', tab_destab :R :H =  ;
       '  Descripci¢n ' IN  ;
       bus_tab COLOR SCHEME 7
ON KEY
SET FILTER TO
SET ORDER TO codigo
RELEASE WINDOW bus_tab
RETURN wrk_var
*
*** 
*** ReFox - retrace your steps ... 
***
