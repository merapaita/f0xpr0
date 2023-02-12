*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
tit_prg = 'REPORTE'
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F10 DO FCINCO
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' ORDENES CERRADAS X TIPO DE SERVICIO '
STORE 'Impresora' TO output
STORE 'Detalle' TO tipo
CLOSE DATABASES
SELECT 1
USE GE_TAB0 ORDER CODIGO
SELECT 2
USE st_mvord ORDER mvo_tecnic
SELECT 3
USE st_iclpr ORDER codigo
SELECT 4
USE st_iorep ORDER codigo
SELECT 5
USE ST_MOVCA ORDER NUMSOL
STORE DATE() TO wrk_dia, wrk_dia2
STORE SPACE(4) TO wrk_destin,  ;
      wrk_dest2, wrk_ind1,  ;
      wrk_ind2
STORE SPACE(4) TO wrk_emi1,  ;
      wrk_emi2, wrk_estado
STORE SPACE(12) TO wrk_idx
wrk_copia = 1
DO WHILE .T.
     @ 07, 01 CLEAR TO 15, 77
     @ 04, 30 SAY SPACE(30)
     @ 03, 02 TO 11, 77
     @ 04, 05 SAY SPACE(50)
     DO esc_modo WITH 'S'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     @ 04, 05 SAY 'Desde Fecha :'
     @ 04, 44 SAY 'Hasta Fecha :'
     @ 05, 05 SAY 'Desde Taller:'
     @ 05, 44 SAY 'Hasta Taller:'
     @ 06, 05 SAY 'Desde Tipo  :'
     @ 06, 44 SAY 'Hasta Tipo  :'
     @ 07, 05 SAY 'Desde Emisor:'
     @ 07, 44 SAY 'Hasta Emisor:'
     @ 08, 05 SAY 'Tipo Servic.:'
     @ 09, 05 SAY  ;
       'Por Detalle/Res£men    :'
     @ 10, 05 SAY  ;
       'Por Impresora/Pantalla :'
     SET CURSOR ON
     @ 04, 19 GET wrk_dia
     @ 04, 57 GET wrk_dia2 RANGE  ;
       wrk_dia
     @ 05, 19 GET wrk_destin  ;
       PICTURE '@!' VALID  ;
       valida(wrk_destin,1) WHEN  ;
       antes(1)
     @ 05, 57 GET wrk_dest2 RANGE  ;
       wrk_destin PICTURE '@!'  ;
       VALID valida(wrk_dest2,2)  ;
       WHEN antes(1)
     @ 06, 19 GET wrk_ind1  ;
       PICTURE '@!' VALID  ;
       valida(wrk_ind1,1) WHEN  ;
       antes(1)
     @ 06, 57 GET wrk_ind2 RANGE  ;
       wrk_ind1 PICTURE '@!'  ;
       VALID valida(wrk_ind2,2)  ;
       WHEN antes(1)
     @ 07, 19 GET wrk_emi1  ;
       PICTURE '@!' VALID  ;
       valida(wrk_emi1,1) WHEN  ;
       antes(1)
     @ 07, 57 GET wrk_emi2 RANGE  ;
       wrk_emi1 PICTURE '@!'  ;
       VALID valida(wrk_emi2,2)  ;
       WHEN antes(1)
     @ 08, 19 GET wrk_estado  ;
       PICTURE '@!' VALID  ;
       valid2(wrk_estado) WHEN  ;
       antes(1)
     @ 09, 30 GET tipo PICTURE  ;
       '@m Detalle,Res£men' WHEN  ;
       antes(2)
     @ 10, 30 GET output PICTURE  ;
       '@m Pantalla,Impresora'  ;
       WHEN antes(2)
     READ
     IF LASTKEY() = 27
          ON KEY
          CLOSE DATABASES
          EXIT
     ENDIF
     IF output = 'Impresora'
          @ 10, 44 SAY  ;
            'Copias      :'
          @ 10, 57 GET wrk_copia  ;
            PICTURE '999'
          READ
     ENDIF
     DO mensa WITH  ;
        '** Un momento, Por Favor ... **',  ;
        'COLO'
     SELECT 6
     USE
     CREATE CURSOR repar (fecemi  ;
            D (8), orden C (8),  ;
            numsol C (8), codmar  ;
            C (4), codmod C (15),  ;
            numser C (20), codtec  ;
            C (9), codent C (11),  ;
            observ M, indori C  ;
            (4), numfabo C (10),  ;
            fecfabo D (8), codemi  ;
            C (4), codtall C (4),  ;
            cosrep N (9, 2),  ;
            cosmob N (9, 2), dia  ;
            D (8), estado C (4),  ;
            codser C (4))
     SELECT st_mvord
     SET NEAR ON
     SEEK '010 ' + DTOS(wrk_dia)
     SET NEAR OFF
     SCAN WHILE (estado = '010 ')  ;
          .AND. (dia >= wrk_dia  ;
          .AND. dia <= wrk_dia2)  ;
          .AND.  .NOT. EOF()
          SELECT st_iorep
          SEEK st_mvord.orden
          IF FOUND() .AND.  ;
             codtall >=  ;
             wrk_destin .AND.  ;
             codtall <=  ;
             wrk_dest2
               IF indori >=  ;
                  wrk_ind1 .AND.  ;
                  indori <=  ;
                  wrk_ind2 .AND.  ;
                  codemi >=  ;
                  wrk_emi1 .AND.  ;
                  codemi <=  ;
                  wrk_emi2
                    DO grabar
               ENDIF
          ENDIF
          SELECT st_mvord
     ENDSCAN
     SELECT st_mvord
     SET NEAR ON
     SEEK '021 ' + DTOS(wrk_dia)
     SET NEAR OFF
     SCAN WHILE (estado = '021 ')  ;
          .AND. (dia >= wrk_dia  ;
          .AND. dia <= wrk_dia2)  ;
          .AND.  .NOT. EOF()
          SELECT st_iorep
          SEEK st_mvord.orden
          IF FOUND() .AND.  ;
             codtall >=  ;
             wrk_destin .AND.  ;
             codtall <=  ;
             wrk_dest2
               IF indori >=  ;
                  wrk_ind1 .AND.  ;
                  indori <=  ;
                  wrk_ind2 .AND.  ;
                  codemi >=  ;
                  wrk_emi1 .AND.  ;
                  codemi <=  ;
                  wrk_emi2
                    DO grabar
               ENDIF
          ENDIF
          SELECT st_mvord
     ENDSCAN
     SELECT st_mvord
     SET NEAR ON
     SEEK '026 ' + DTOS(wrk_dia)
     SET NEAR OFF
     SCAN WHILE (estado = '026 ')  ;
          .AND. (dia >= wrk_dia  ;
          .AND. dia <= wrk_dia2)  ;
          .AND.  .NOT. EOF()
          SELECT st_iorep
          SEEK st_mvord.orden
          IF FOUND() .AND.  ;
             codtall >=  ;
             wrk_destin .AND.  ;
             codtall <=  ;
             wrk_dest2
               IF indori >=  ;
                  wrk_ind1 .AND.  ;
                  indori <=  ;
                  wrk_ind2 .AND.  ;
                  codemi >=  ;
                  wrk_emi1 .AND.  ;
                  codemi <=  ;
                  wrk_emi2
                    DO grabar
               ENDIF
          ENDIF
          SELECT st_mvord
     ENDSCAN
     SELECT st_mvord
     SET NEAR ON
     SEEK '027 ' + DTOS(wrk_dia)
     SET NEAR OFF
     SCAN WHILE (estado = '027 ')  ;
          .AND. (dia >= wrk_dia  ;
          .AND. dia <= wrk_dia2)  ;
          .AND.  .NOT. EOF()
          SELECT st_iorep
          SEEK st_mvord.orden
          IF FOUND() .AND.  ;
             codtall >=  ;
             wrk_destin .AND.  ;
             codtall <=  ;
             wrk_dest2
               IF indori >=  ;
                  wrk_ind1 .AND.  ;
                  indori <=  ;
                  wrk_ind2 .AND.  ;
                  codemi >=  ;
                  wrk_emi1 .AND.  ;
                  codemi <=  ;
                  wrk_emi2
                    DO grabar
               ENDIF
          ENDIF
          SELECT st_mvord
     ENDSCAN
     SELECT st_mvord
     SET NEAR ON
     SEEK '018 ' + DTOS(wrk_dia)
     SET NEAR OFF
     SCAN WHILE (estado = '018 ')  ;
          .AND. (dia >= wrk_dia  ;
          .AND. dia <= wrk_dia2)  ;
          .AND.  .NOT. EOF()
          SELECT st_iorep
          SEEK st_mvord.orden
          IF FOUND() .AND.  ;
             codtall >=  ;
             wrk_destin .AND.  ;
             codtall <=  ;
             wrk_dest2
               IF indori >=  ;
                  wrk_ind1 .AND.  ;
                  indori <=  ;
                  wrk_ind2 .AND.  ;
                  codemi >=  ;
                  wrk_emi1 .AND.  ;
                  codemi <=  ;
                  wrk_emi2
                    DO grabar
               ENDIF
          ENDIF
          SELECT st_mvord
     ENDSCAN
     SELECT repar
     COUNT TO wrk_valor
     IF wrk_valor = 0
          DO error WITH  ;
             'No Existen registros a Listar'
          LOOP
     ENDIF
     wrk_idx = f_indice()
     INDEX ON CODSER + ESTADO + DTOS(DIA);
+ ORDEN TO &WRK_IDX
     SET RELATION TO 'C' + codent INTO;
st_iclpr
     GOTO TOP
     IF output = 'Impresora'
          FOR a = 1 TO wrk_copia
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'COLO'
               ??? CHR(15)
               IF tipo =  ;
                  'Detalle'
                    REPORT FORMAT  ;
                           porl0426  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE  ;
                           FOR  ;
                           codser =  ;
                           wrk_estado
               ELSE
                    REPORT FORMAT  ;
                           porl0426  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE  ;
                           FOR  ;
                           codser =  ;
                           wrk_estado
               ENDIF
               DO mensa WITH  ;
                  '*** I m p r i m i e n d o ... ***',  ;
                  'SACA'
          ENDFOR
          SET PRINTER TO
     ELSE
          wrk_fil2 = SUBSTR(f_texto(),  ;
                     1, 8) +  ;
                     '.TXT'
          IF tipo = 'Detalle'
               repor form porl0426 to;
file &wrk_fil2 for CODSER = wrk_estado;
noconsole 
          ELSE
               repor form porl0426 to;
file &wrk_fil2 for CODSER = wrk_estado;
noconsole summary 
          ENDIF
          DO mensa WITH  ;
             '** Un momento, Por Favor ... **',  ;
             'SACA'
          SET SYSMENU ON
          KEYBOARD '{CTRL+F10}'
          modi comm &wrk_fil2 window pantall;
NOEDIT
          SET SYSMENU OFF
          delete file &wrk_fil2
     ENDIF
ENDDO
ON KEY
CLOSE DATABASES
IF FILE(wrk_idx)
     erase &wrk_idx
ENDIF
DO saca_win
RETURN
*
PROCEDURE ayuda
PARAMETER opc
DO CASE
     CASE opc = 5
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'TALL'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE TALLER'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          SET FILTER TO
     CASE opc = 6
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'INGA'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE TIPO DE ATENCION'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          SET FILTER TO
     CASE opc = 7
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'EMIS'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE EMISORES'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          SET FILTER TO
     CASE opc = 8
          SELECT ge_tab0
          SET FILTER TO tab_codpre ==;
'SOFI'
          GOTO TOP
          campo = 'tab_codtab + "  " + tab_destab'
          titulo = 'AYUDA DE TABLAS'
          DO ayuda1 WITH campo,  ;
             titulo,  ;
             'tab_codtab'
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          SET FILTER TO
ENDCASE
RETURN
*
FUNCTION valida
PARAMETER wrk_codtab, colu
IF EMPTY(wrk_codtab)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
SELECT ge_tab0
IF ROW() = 5
     SEEK 'TALL' + wrk_codtab
ELSE
     IF ROW() = 6
          SEEK 'INGA' +  ;
               wrk_codtab
     ELSE
          IF ROW() = 7
               SEEK 'EMIS' +  ;
                    wrk_codtab
          ENDIF
     ENDIF
ENDIF
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C¢digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
IF colu = 1
     @ ROW(), 24 SAY  ;
       SUBSTR(tab_destab, 1, 18)
ELSE
     @ ROW(), 61 SAY  ;
       SUBSTR(tab_destab, 1, 13)
ENDIF
RETURN
*
FUNCTION valid2
PARAMETER wrk_codtab
IF EMPTY(wrk_codtab)
     DO error WITH  ;
        ' *** No se Permiten Blancos ***'
     RETURN .F.
ENDIF
SELECT ge_tab0
SEEK 'SOFI' + wrk_codtab
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** C¢digo de Tabla No Existe ***'
     RETURN .F.
ENDIF
@ ROW(), 24 SAY SUBSTR(tab_destab,  ;
  1, 25)
RETURN
*
FUNCTION f_indice
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.IDX')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION f_texto
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.TXT')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION ootab
PARAMETER var1, var2
narea = SELECT()
SELECT ge_tab0
SEEK var1 + var2
IF FOUND()
     wrk_despag = ge_tab0.tab_destab
ELSE
     wrk_despag = SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_despag
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BUS',  ;
             'BBB'
          ON KEY LABEL f6 do ayuda with;
row()
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          ON KEY
ENDCASE
RETURN
*
PROCEDURE grabar
SELECT st_movca
SEEK st_iorep.numsol
w_codser = SPACE(4)
IF FOUND()
     SCAN WHILE numsol =  ;
          st_iorep.numsol .AND.   ;
          .NOT. EOF()
          IF VAL(codcau) > 99
               w_codser = codcau
          ENDIF
     ENDSCAN
ENDIF
SELECT repar
APPEND BLANK
REPLACE orden WITH st_mvord.orden,  ;
        fecemi WITH  ;
        st_iorep.fecemi
REPLACE numsol WITH  ;
        st_iorep.numsol, codmar  ;
        WITH st_iorep.codmar
REPLACE codmod WITH  ;
        st_iorep.codmod, numser  ;
        WITH st_iorep.numser
REPLACE codtec WITH  ;
        st_iorep.codtec, codent  ;
        WITH st_iorep.codent
REPLACE observ WITH  ;
        st_iorep.observ, indori  ;
        WITH st_iorep.indori
REPLACE numfabo WITH  ;
        st_iorep.numfabo, fecfabo  ;
        WITH st_iorep.fecfabo
REPLACE codtall WITH  ;
        st_iorep.codtall, estado  ;
        WITH st_mvord.estado
REPLACE cosmob WITH  ;
        st_iorep.cosmob, cosrep  ;
        WITH st_iorep.cosrep
REPLACE dia WITH st_mvord.dia,  ;
        codemi WITH  ;
        st_iorep.codemi
REPLACE codser WITH w_codser
*
*** 
*** ReFox - retrace your steps ... 
***
