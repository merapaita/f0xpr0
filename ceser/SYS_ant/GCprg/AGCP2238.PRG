*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
CLOSE DATABASES
SET CURSOR ON
SET CENTURY ON
DEFINE WINDOW tiposal FROM 05, 02  ;
       TO 20, 78 COLOR SCHEME 24
DEFINE POPUP tiposal FROM 06, 15  ;
       TO 18, 45 IN screen COLOR  ;
       SCHEME 24
ON SELECTION POPUP tiposal show popup;
tiposal
SELECT 1
USE SHARED gc_pro00 ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_dlp00 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'GENERACION',  ;
   'LISTA DE PRECIOS',  ;
   'SELECCION'
@ 02, 63 SAY PROGRAM()
DIMENSION tabla[ 9]
tabla[ 1] = 'GENERAL'
tabla[ 2] = 'LINEA '
tabla[ 3] = 'CATEGORIA'
tabla[ 4] = 'SUBCATEGORIA'
tabla[ 5] = 'MARCA'
tabla[ 6] = 'RUBRO'
tabla[ 7] = 'SUBRUBRO'
tabla[ 8] = 'CLASE COMERCIAL'
tabla[ 9] = 'PROCEDENCIA'
STORE 2 TO item
STORE SPACE(4) TO w_codi1,  ;
      w_codi2, w_var, w_busca,  ;
      w_titu
STORE .T. TO sigue
w_facigv = facigv()
IF w_facigv = 0
     STORE .F. TO sigue
ENDIF
DO WHILE sigue
     ACTIVATE WINDOW tiposal
     @ 00, 04 SAY  ;
       'Seleccione Rango:'
     @ 00, 21 GET item DEFAULT 1  ;
       SIZE 01, 25 FROM tabla  ;
       WHEN antes(10,0) COLOR  ;
       SCHEME 24
     READ
     IF LASTKEY() = 27
          STORE .F. TO sigue
     ELSE
          IF item = 1
               @ 11, 17 CLEAR TO  ;
                 13, 70
          ELSE
               @ 11, 17 SAY  ;
                 'Del:'
               @ 12, 18 SAY 'al:'
          ENDIF
          DO CASE
               CASE item = 2
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(2, ;
                      1) WHEN  ;
                      antes(2,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(2, ;
                      2) WHEN  ;
                      antes(2,2)
               CASE item = 3
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(3, ;
                      1) WHEN  ;
                      antes(3,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(3, ;
                      2) WHEN  ;
                      antes(3,2)
               CASE item = 4
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(4, ;
                      1) WHEN  ;
                      antes(4,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(4, ;
                      2) WHEN  ;
                      antes(4,2)
               CASE item = 5
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(5, ;
                      1) WHEN  ;
                      antes(5,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(5, ;
                      2) WHEN  ;
                      antes(5,2)
               CASE item = 6
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(6, ;
                      1) WHEN  ;
                      antes(6,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(6, ;
                      2) WHEN  ;
                      antes(6,2)
               CASE item = 7
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(7, ;
                      1) WHEN  ;
                      antes(7,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(7, ;
                      2) WHEN  ;
                      antes(7,2)
               CASE item = 8
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(8, ;
                      1) WHEN  ;
                      antes(8,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(8, ;
                      2) WHEN  ;
                      antes(8,2)
               CASE item = 9
                    @ 11, 22 GET  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(9, ;
                      1) WHEN  ;
                      antes(9,1)
                    @ 12, 22 GET  ;
                      w_codi2  ;
                      RANGE  ;
                      w_codi1  ;
                      PICTURE  ;
                      '@!' VALID  ;
                      despues(9, ;
                      2) WHEN  ;
                      antes(9,2)
          ENDCASE
          @ 13, 13 SAY 'Procesa:'
          @ 13, 22 GET opc3  ;
            DEFAULT 2 SIZE 1, 7,  ;
            0 PICTURE  ;
            '@*RHTN Si ;No' WHEN  ;
            antes(10,0) COLOR  ;
            SCHEME 4
          READ
          IF LASTKEY() = 27
               LOOP
          ELSE
               IF opc3 = 1
                    DO p_footer  ;
                       WITH  ;
                       '100000000000000000001',  ;
                       2
                    DO proceso  ;
                       WITH  ;
                       w_codi1,  ;
                       w_codi2
               ENDIF
          ENDIF
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY
DEACTIVATE WINDOW tablas
RELEASE WINDOW tiposal,  ;
        winmensaje
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE proceso
PARAMETER w_codi1, w_codi2
= ooaviso('PROCESANDO...PROCESANDO...')
SELECT 20
USE SHARED gc_alm00 ORDER codigo
SELECT gc_pro00
DO CASE
     CASE item = 1
          SET ORDER TO codigo
          GOTO TOP
     CASE item = 2
          SET ORDER TO codigo
          SET FILTER TO pro_linea >= w_codi1;
.AND. pro_linea <= w_codi2
     CASE item = 3
          SET ORDER TO codigo
          SET FILTER TO pro_catego >=;
w_codi1;
.AND. pro_catego <= w_codi2
     CASE item = 4
          SET ORDER TO subcat
     CASE item = 5
          SET ORDER TO marca
     CASE item = 6
          SET ORDER TO codigo
          SET FILTER TO pro_rubro >= w_codi1;
.AND. pro_rubro <= w_codi2
     CASE item = 7
          SET ORDER TO codigo
          SET FILTER TO pro_subrub >=;
w_codi1;
.AND. pro_subrub <= w_codi2
     CASE item = 8
          SET ORDER TO codigo
          SET FILTER TO pro_clacom >=;
w_codi1;
.AND. pro_clacom <= w_codi2
     CASE item = 9
          SET ORDER TO pro_proced
ENDCASE
SET NEAR ON
SEEK w_codi1
SET NEAR OFF
DO CASE
     CASE item = 1 .OR. item = 2  ;
          .OR. item = 3 .OR. item =  ;
          6 .OR. item = 7 .OR.  ;
          item = 8
          SCAN WHILE  .NOT. EOF()
               IF pro_coremo <> 0
                    DO agcpprec
               ENDIF
          ENDSCAN
     CASE item = 4
          SCAN WHILE pro_subcat <=  ;
               w_codi2 .AND.   ;
               .NOT. EOF()
               IF pro_coremo <> 0
                    DO agcpprec
               ENDIF
          ENDSCAN
     CASE item = 5
          SCAN WHILE pro_marca <=  ;
               w_codi2 .AND.   ;
               .NOT. EOF()
               IF pro_coremo <> 0
                    DO agcpprec
               ENDIF
          ENDSCAN
     CASE item = 9
          SCAN WHILE pro_proced <=  ;
               w_codi2 .AND.   ;
               .NOT. EOF()
               IF pro_coremo <> 0
                    DO agcpprec
               ENDIF
          ENDSCAN
ENDCASE
? CHR(7)
? CHR(7)
RETURN
*
PROCEDURE antes
PARAMETER var, opc
DO CASE
     CASE var = 10
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
     CASE var = 2
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'LINE'
          w_titu = 'AYUDA DE LINEAS'
     CASE var = 3
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'CATE'
          w_titu = 'AYUDA DE CATEGORIAS'
     CASE var = 4
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'SUCA'
          w_titu = 'AYUDA DE SUBCATEGORIAS'
     CASE var = 5
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'MARC'
          w_titu = 'AYUDA DE MARCAS'
     CASE var = 6
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'RUBR'
          w_titu = 'AYUDA DE RUBROS'
     CASE var = 7
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'SURU'
          w_titu = 'AYUDA DE SUBRUBROS'
     CASE var = 8
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'CMCO'
          w_titu = 'AYUDA DE MARGEN COMERCIAL'
     CASE var = 9
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW tiposal
          w_busca = 'PROC'
          w_titu = 'AYUDA DE PROCEDENCIA'
ENDCASE
IF opc > 0
     IF opc = 1
          w_var = 'w_codi1'
     ELSE
          w_var = 'w_codi2'
     ENDIF
     ON KEY LABEL f6 do busca2 with w_busca,w_var,w_titu,2
ENDIF
RETURN
*
FUNCTION despues
PARAMETER var, opc
ON KEY
SELECT ge_tab0
DO CASE
     CASE var = 2
          IF opc = 1
               SEEK 'LINE' +  ;
                    w_codi1
          ELSE
               SEEK 'LINE' +  ;
                    w_codi2
          ENDIF
     CASE var = 3
          IF opc = 1
               SEEK 'CATE' +  ;
                    w_codi1
          ELSE
               SEEK 'CATE' +  ;
                    w_codi2
          ENDIF
     CASE var = 4
          IF opc = 1
               SEEK 'SUCA' +  ;
                    w_codi1
          ELSE
               SEEK 'SUCA' +  ;
                    w_codi2
          ENDIF
     CASE var = 5
          IF opc = 1
               SEEK 'MARC' +  ;
                    w_codi1
          ELSE
               SEEK 'MARC' +  ;
                    w_codi2
          ENDIF
     CASE var = 6
          IF opc = 1
               SEEK 'RUBR' +  ;
                    w_codi1
          ELSE
               SEEK 'RUBR' +  ;
                    w_codi2
          ENDIF
     CASE var = 7
          IF opc = 1
               SEEK 'SURU' +  ;
                    w_codi1
          ELSE
               SEEK 'SURU' +  ;
                    w_codi2
          ENDIF
     CASE var = 8
          IF opc = 1
               SEEK 'CMCO' +  ;
                    w_codi1
          ELSE
               SEEK 'CMCO' +  ;
                    w_codi2
          ENDIF
     CASE var = 9
          IF opc = 1
               SEEK 'PROC' +  ;
                    w_codi1
          ELSE
               SEEK 'PROC' +  ;
                    w_codi2
          ENDIF
ENDCASE
IF  .NOT. FOUND()
     DO p_mensaje WITH  ;
        'C¢digo no Existe'
     ON KEY LABEL f6 do busca2 with w_busca,w_var,w_titu,2
     RETURN .F.
ELSE
     IF opc = 2
          IF w_codi2 < w_codi1
               DO p_mensaje WITH  ;
                  'C¢digo mayor al Inicial'
               ON KEY LABEL f6 do busca2;
with w_busca,w_var,w_titu,2
               RETURN .F.
          ENDIF
     ENDIF
     @ ROW(), 27 SAY  ;
       SUBSTR(tab_destab, 1, 25)
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
