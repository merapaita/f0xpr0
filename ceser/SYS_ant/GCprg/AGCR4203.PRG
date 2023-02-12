*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 21, 77 COLOR SCHEME  ;
       7
CLOSE DATABASES
w_program = PROGRAM()
SELECT 1
USE SHARED gc_hvest ORDER codigo
SELECT 2
USE SHARED gc_hpres ORDER codigo
SELECT 3
USE SHARED ge_tab0 ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'RESUMEN DE VENTAS vs PRESUPUESTO',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000000000001', 2
STORE MONTH(DATE()) TO fecha1,  ;
      fecha2
STORE YEAR(DATE()) TO w_ano
STORE .T. TO sigue
STORE SPACE(1) TO w_archi
DO WHILE sigue
     ACTIVATE WINDOW cabecera
     SET CURSOR ON
     STORE 1 TO w_copia, opc1
     STORE 3 TO opc4
     @ 00, 01 SAY  ;
       'Per¡odo Mes      :' COLOR  ;
       SCHEME 8
     @ 00, 34 SAY 'A¤o :' COLOR  ;
       SCHEME 8
     @ 00, 20 SAY 'Del'
     @ 01, 20 SAY ' Al'
     @ 04, 01 SAY  ;
       'Clasificaci¢n    :' COLOR  ;
       SCHEME 8
     @ 11, 01 SAY  ;
       'Tipo de Informe  :' COLOR  ;
       SCHEME 8
     @ 11, 37 SAY  ;
       'Tipo de Salida : ' COLOR  ;
       SCHEME 8
     @ 00, 24 GET fecha1 PICTURE  ;
       '99' VALID  ;
       oovalid(VARREAD()) .AND.  ;
       (fecha1 > 0 .AND. fecha1 <  ;
       13) COLOR SCHEME 8
     @ 01, 24 GET fecha2 RANGE  ;
       fecha1 PICTURE '99' VALID  ;
       (fecha2 > 0 .AND. fecha2 <  ;
       13) COLOR SCHEME 8
     @ 00, 40 GET w_ano PICTURE  ;
       '9999' VALID w_ano > 1900  ;
       COLOR SCHEME 8
     @ 04, 20 GET opc1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Ventas Importe;Ventas Unidades;Ventas Vs Presupuesto;Prespuesto Importe;Presupuesto Unidades'  ;
       VALID oovalid(VARREAD())  ;
       COLOR SCHEME 24
     @ 11, 20 GET opc2 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Detalle;Resumen'  ;
       COLOR SCHEME 24
     @ 11, 55 GET opc3 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora'  ;
       COLOR SCHEME 24
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF opc3 = 2
          @ 14, 59 SAY 'Copias :'
          @ 14, 68 GET w_copia  ;
            PICTURE '99' VALID   ;
            .NOT. EMPTY(w_copia)  ;
            .AND. w_copia > 0  ;
            .AND. w_copia < 6  ;
            COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               LOOP
          ENDIF
     ENDIF
     DO procesa
     SELECT 20
     COUNT TO nreg
     IF nreg = 0
          RELEASE WINDOW  ;
                  winmensaje
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          USE
          erase &w_archi
     ELSE
          DO imprime
     ENDIF
ENDDO
RELEASE WINDOW cabecera
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'FECHA1'
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
     CASE cvalid = 'OPC1'
          IF opc1 = 1 .OR. opc1 =  ;
             3 .OR. opc1 = 4
               @ 04, 48 SAY  ;
                 'Otros Ings:'  ;
                 COLOR SCHEME 8
               @ 04, 60 GET opc4  ;
                 DEFAULT 2 SIZE 1,  ;
                 7, 0 PICTURE  ;
                 '@*RVN  Si;No'  ;
                 COLOR SCHEME 24
               READ
               IF LASTKEY() = 27
                    @ 04, 48 SAY  ;
                      SPACE(19)
                    @ 05, 48 SAY  ;
                      SPACE(19)
                    RETURN .F.
               ENDIF
          ENDIF
ENDCASE
RETURN
*
PROCEDURE procesa
= ooaviso( ;
  'P R O C E S A N D O .')
IF opc1 < 4
     SELECT gc_hvest
ELSE
     SELECT gc_hpres
ENDIF
w_archi = SUBSTR(f_archivo(), 1,  ;
          8) + '.DBF'
copy stru to &w_archi
SELECT 20
use &w_archi
INDEX ON zona + sucu + indori +  ;
      codemi + linea TAG codigo
IF opc1 < 4
     SET RELATION TO ano + zona + sucu;
+ indori + codemi + linea INTO gc_hpres
     SELECT gc_hvest
ELSE
     SELECT gc_hpres
ENDIF
SET NEAR ON
SEEK STR(w_ano, 4)
SET NEAR OFF
SCAN WHILE ano = STR(w_ano, 4)  ;
     .AND.  .NOT. EOF()
     STORE 0 TO mob, rep, otr,  ;
           uni
     STORE SPACE(5) TO t_mob,  ;
           t_rep, t_otr, t_uni
     IF opc1 = 3
          FOR x = 1 TO fecha2
               DO toma WITH x
               DO pasa
               IF opc1 < 4
                    SELECT gc_hvest
               ELSE
                    SELECT gc_hpres
               ENDIF
          ENDFOR
     ELSE
          FOR x = fecha1 TO  ;
              fecha2
               DO toma WITH x
               DO pasa
               IF opc1 < 4
                    SELECT gc_hvest
               ELSE
                    SELECT gc_hpres
               ENDIF
          ENDFOR
     ENDIF
ENDSCAN
RETURN
*
PROCEDURE toma
PARAMETER mes
IF mes = 1
     mob = enemo
     rep = enere
     otr = eneot
     uni = eneun
     t_mob = 'ENEMO'
     t_rep = 'ENERE'
     t_otr = 'ENEOT'
     t_uni = 'ENEUN'
ELSE
     IF mes = 2
          mob = febmo
          rep = febre
          otr = febot
          uni = febun
          t_mob = 'FEBMO'
          t_rep = 'FEBRE'
          t_otr = 'FEBOT'
          t_uni = 'FEBUN'
     ELSE
          IF mes = 3
               mob = marmo
               rep = marre
               otr = marot
               uni = marun
               t_mob = 'MARMO'
               t_rep = 'MARRE'
               t_otr = 'MAROT'
               t_uni = 'MARUN'
          ELSE
               IF mes = 4
                    mob = abrmo
                    rep = abrre
                    otr = abrot
                    uni = abrun
                    t_mob = 'ABRMO'
                    t_rep = 'ABRRE'
                    t_otr = 'ABROT'
                    t_uni = 'ABRUN'
               ELSE
                    IF mes = 5
                         mob = maymo
                         rep = mayre
                         otr = mayot
                         uni = mayun
                         t_mob = 'MAYMO'
                         t_rep = 'MAYRE'
                         t_otr = 'MAYOT'
                         t_uni = 'MAYUN'
                    ELSE
                         IF mes =  ;
                            6
                              mob =  ;
                               junmo
                              rep =  ;
                               junre
                              otr =  ;
                               junot
                              uni =  ;
                               junun
                              t_mob =  ;
                               'JUNMO'
                              t_rep =  ;
                               'JUNRE'
                              t_otr =  ;
                               'JUNOT'
                              t_uni =  ;
                               'JUNUN'
                         ELSE
                              IF mes =  ;
                                 7
                                   mob = julmo
                                   rep = julre
                                   otr = julot
                                   uni = julun
                                   t_mob = 'JULMO'
                                   t_rep = 'JULRE'
                                   t_otr = 'JULOT'
                                   t_uni = 'JULUN'
                              ELSE
                                   IF mes = 8
                                        mob = agomo
                                        rep = agore
                                        otr = agoot
                                        uni = agoun
                                        t_mob = 'AGOMO'
                                        t_rep = 'AGORE'
                                        t_otr = 'AGOOT'
                                        t_uni = 'AGOUN'
                                   ELSE
                                        IF mes = 9
                                             mob = setmo
                                             rep = setre
                                             otr = setot
                                             uni = setun
                                             t_mob = 'SETMO'
                                             t_rep = 'SETRE'
                                             t_otr = 'SETOT'
                                             t_uni = 'SETUN'
                                        ELSE
                                             IF mes = 10
                                                  mob = octmo
                                                  rep = octre
                                                  otr = octot
                                                  uni = octun
                                                  t_mob = 'OCTMO'
                                                  t_rep = 'OCTRE'
                                                  t_otr = 'OCTOT'
                                                  t_uni = 'OCTUN'
                                             ELSE
                                                  IF mes = 11
                                                       mob = novmo
                                                       rep = novre
                                                       otr = novot
                                                       uni = novun
                                                       t_mob = 'NOVMO'
                                                       t_rep = 'NOVRE'
                                                       t_otr = 'NOVOT'
                                                       t_uni = 'NOVUN'
                                                  ELSE
                                                       mob = dicmo
                                                       rep = dicre
                                                       otr = dicot
                                                       uni = dicun
                                                       t_mob = 'DICMO'
                                                       t_rep = 'DICRE'
                                                       t_otr = 'DICOT'
                                                       t_uni = 'DICUN'
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN
*
PROCEDURE pasa
SELECT 20
IF opc1 < 4
     SEEK gc_hvest.zona +  ;
          gc_hvest.sucu +  ;
          gc_hvest.indori +  ;
          gc_hvest.codemi +  ;
          gc_hvest.linea
     IF  .NOT. FOUND()
          APPEND BLANK
          REPLACE zona WITH  ;
                  gc_hvest.zona,  ;
                  sucu WITH  ;
                  gc_hvest.sucu,  ;
                  codemi WITH  ;
                  gc_hvest.codemi,  ;
                  linea WITH  ;
                  gc_hvest.linea,  ;
                  indori WITH  ;
                  gc_hvest.indori,  ;
                  ano WITH  ;
                  gc_hvest.ano
     ENDIF
ELSE
     SEEK gc_hpres.zona +  ;
          gc_hpres.sucu +  ;
          gc_hpres.indori +  ;
          gc_hpres.codemi +  ;
          gc_hpres.linea
     IF  .NOT. FOUND()
          APPEND BLANK
          REPLACE zona WITH  ;
                  gc_hpres.zona,  ;
                  sucu WITH  ;
                  gc_hpres.sucu,  ;
                  codemi WITH  ;
                  gc_hpres.codemi,  ;
                  linea WITH  ;
                  gc_hpres.linea,  ;
                  indori WITH  ;
                  gc_hpres.indori,  ;
                  ano WITH  ;
                  gc_hpres.ano
     ENDIF
ENDIF
repl &t_mob with mob,&t_rep with rep,&t_uni;
with uni
IF opc4 <> 2
     repl &t_otr with otr 
ENDIF
RETURN
*
PROCEDURE imprime
IF opc3 = 1
     = ooaviso( ;
       'C O P I A N D O . . .')
     w_file = f_texto()
     DO CASE
          CASE opc1 = 1 .OR. opc1 =  ;
               4
               IF fecha1 = fecha2
                    IF opc2 = 1
                         SET SYSMENU ON
                         BROWSE
                         MODIFY REPORT  ;
                                AGCR4203
                         REPO FORM agcr4203;
TO file &w_file  NOCONSOLE
                    ELSE
                         SET SYSMENU ON
                         BROWSE
                         MODIFY REPORT  ;
                                AGCR423C
                         REPO FORM agcr423c;
TO file &w_file  NOCONSOLE
                    ENDIF
               ELSE
                    IF opc2 = 1
                         REPO FORM agcr423a;
TO file &w_file  NOCONSOLE
                    ELSE
                         REPO FORM agcr423b;
TO file &w_file  NOCONSOLE
                    ENDIF
               ENDIF
          CASE opc1 = 2 .OR. opc1 =  ;
               5
               IF fecha1 = fecha2
                    IF opc2 = 1
                         REPO FORM agcr423h;
TO file &w_file  NOCONSOLE
                    ELSE
                         REPO FORM agcr423i;
TO file &w_file  NOCONSOLE
                    ENDIF
               ELSE
                    IF opc2 = 1
                         REPO form agcr423f;
TO file &w_file  NOCONSOLE 
                    ELSE
                         REPO form agcr423g;
TO file &w_file  NOCONSOLE
                    ENDIF
               ENDIF
          CASE opc1 = 3
               IF opc2 = 1
                    REPO FORM agcr423d;
TO file &w_file  NOCONSOLE
               ELSE
                    REPO FORM agcr423e;
TO file &w_file  NOCONSOLE
               ENDIF
     ENDCASE
     SET SYSMENU ON
     KEYBOARD '{CTRL+F10}'
     MODI COMM &w_file NOEDIT WIND pantall
     SET SYSMENU OFF
     COPY &w_file TO ARCHI
     DELE FILE &w_file
     CLEAR
ELSE
     = ooaviso( ;
       'I M P R I M I E N D O . . .' ;
       )
     SET PRINTER ON
     SET DEVICE TO PRINTER
     @ 0, 0 SAY CHR(27) + CHR(15)
     DO CASE
          CASE opc1 = 1 .OR. opc1 =  ;
               4
               IF fecha1 = fecha2
                    IF opc2 = 1
                         REPORT FORMAT  ;
                                agcr4203  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                agcr423c  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ELSE
                    IF opc2 = 1
                         REPORT FORMAT  ;
                                agcr423a  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                agcr423b  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
          CASE opc1 = 2 .OR. opc1 =  ;
               5
               IF fecha1 = fecha2
                    IF opc2 = 1
                         REPORT FORMAT  ;
                                agcr423h  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                agcr423i  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ELSE
                    IF opc2 = 1
                         REPORT FORMAT  ;
                                agcr423f  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ELSE
                         REPORT FORMAT  ;
                                agcr423g  ;
                                TO  ;
                                PRINTER  ;
                                NOCONSOLE
                    ENDIF
               ENDIF
          CASE opc1 = 3
               IF opc2 = 1
                    REPORT FORMAT  ;
                           agcr423d  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           agcr423e  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
     ENDCASE
     SET PRINTER TO
     SET PRINTER OFF
     SET DEVICE TO SCREEN
ENDIF
SELECT 20
USE
erase &w_archi
RELEASE WINDOW winmensaje
RETURN
*
FUNCTION encab
PARAMETER mes
DO CASE
     CASE mes = 1
          t_mes = 'ENE'
     CASE mes = 2
          t_mes = 'FEB'
     CASE mes = 3
          t_mes = 'MAR'
     CASE mes = 4
          t_mes = 'ABR'
     CASE mes = 5
          t_mes = 'MAY'
     CASE mes = 6
          t_mes = 'JUN'
     CASE mes = 7
          t_mes = 'JUL'
     CASE mes = 8
          t_mes = 'AGO'
     CASE mes = 9
          t_mes = 'SET'
     CASE mes = 10
          t_mes = 'OCT'
     CASE mes = 11
          t_mes = 'NOV'
     CASE mes = 12
          t_mes = 'DIC'
     OTHERWISE
          t_mes = SPACE(3)
ENDCASE
RETURN t_mes
*
FUNCTION datomo
PARAMETER mes
IF mes = 1
     mob = enemo
ELSE
     IF mes = 2
          mob = febmo
     ELSE
          IF mes = 3
               mob = marmo
          ELSE
               IF mes = 4
                    mob = abrmo
               ELSE
                    IF mes = 5
                         mob = maymo
                    ELSE
                         IF mes =  ;
                            6
                              mob =  ;
                               junmo
                         ELSE
                              IF mes =  ;
                                 7
                                   mob = julmo
                              ELSE
                                   IF mes = 8
                                        mob = agomo
                                   ELSE
                                        IF mes = 9
                                             mob = setmo
                                        ELSE
                                             IF mes = 10
                                                  mob = octmo
                                             ELSE
                                                  IF mes = 11
                                                       mob = novmo
                                                  ELSE
                                                       IF mes = 12
                                                            mob = dicmo
                                                       ELSE
                                                            mob = 0
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN mob
*
FUNCTION datore
PARAMETER mes
IF mes = 1
     rep = enere
ELSE
     IF mes = 2
          rep = febre
     ELSE
          IF mes = 3
               rep = marre
          ELSE
               IF mes = 4
                    rep = abrre
               ELSE
                    IF mes = 5
                         rep = mayre
                    ELSE
                         IF mes =  ;
                            6
                              rep =  ;
                               junre
                         ELSE
                              IF mes =  ;
                                 7
                                   rep = julre
                              ELSE
                                   IF mes = 8
                                        rep = agore
                                   ELSE
                                        IF mes = 9
                                             rep = setre
                                        ELSE
                                             IF mes = 10
                                                  rep = octre
                                             ELSE
                                                  IF mes = 11
                                                       rep = novre
                                                  ELSE
                                                       IF mes = 12
                                                            rep = dicre
                                                       ELSE
                                                            rep = 0
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
RETURN rep
*
FUNCTION datoun
PARAMETER opc, mes
IF opc = 1
     SELECT 20
ELSE
     SELECT gc_hpres
ENDIF
IF mes = 1
     uni = eneun
ELSE
     IF mes = 2
          uni = febun
     ELSE
          IF mes = 3
               uni = marun
          ELSE
               IF mes = 4
                    uni = abrun
               ELSE
                    IF mes = 5
                         uni = mayun
                    ELSE
                         IF mes =  ;
                            6
                              uni =  ;
                               junun
                         ELSE
                              IF mes =  ;
                                 7
                                   uni = julun
                              ELSE
                                   IF mes = 8
                                        uni = agoun
                                   ELSE
                                        IF mes = 9
                                             uni = setun
                                        ELSE
                                             IF mes = 10
                                                  uni = octun
                                             ELSE
                                                  IF mes = 11
                                                       uni = novun
                                                  ELSE
                                                       IF mes = 12
                                                            uni = dicun
                                                       ELSE
                                                            uni = 0
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
IF opc = 2
     SELECT 20
ENDIF
RETURN uni
*
FUNCTION datoot
PARAMETER mes
IF opc4 = 1
     IF mes = 1
          otr = eneot
     ELSE
          IF mes = 2
               otr = febot
          ELSE
               IF mes = 3
                    otr = marot
               ELSE
                    IF mes = 4
                         otr = abrot
                    ELSE
                         IF mes =  ;
                            5
                              otr =  ;
                               mayot
                         ELSE
                              IF mes =  ;
                                 6
                                   otr = junot
                              ELSE
                                   IF mes = 7
                                        otr = julot
                                   ELSE
                                        IF mes = 8
                                             otr = agoot
                                        ELSE
                                             IF mes = 9
                                                  otr = setot
                                             ELSE
                                                  IF mes = 10
                                                       otr = octot
                                                  ELSE
                                                       IF mes = 11
                                                            otr = novot
                                                       ELSE
                                                            IF mes = 12
                                                                 otr = dicot
                                                            ELSE
                                                                 otr = 0
                                                            ENDIF
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ELSE
     otr = 0
ENDIF
RETURN otr
*
FUNCTION datimp
PARAMETER opc, mes
IF opc = 1
     SELECT 20
ELSE
     SELECT gc_hpres
ENDIF
IF mes = 1
     rep = enere + enemo
     IF opc4 <> 2
          rep = rep + eneot
     ENDIF
ELSE
     IF mes = 2
          rep = febre + febmo
          IF opc4 <> 2
               rep = rep + febot
          ENDIF
     ELSE
          IF mes = 3
               rep = marre +  ;
                     marmo
               IF opc4 <> 2
                    rep = rep +  ;
                          marot
               ENDIF
          ELSE
               IF mes = 4
                    rep = abrre +  ;
                          abrmo
                    IF opc4 <> 2
                         rep = rep +  ;
                               abrot
                    ENDIF
               ELSE
                    IF mes = 5
                         rep = mayre +  ;
                               maymo
                         IF opc4 <>  ;
                            2
                              rep =  ;
                               rep +  ;
                               mayot
                         ENDIF
                    ELSE
                         IF mes =  ;
                            6
                              rep =  ;
                               junre +  ;
                               junmo
                              IF opc4 <>  ;
                                 2
                                   rep = rep + junot
                              ENDIF
                         ELSE
                              IF mes =  ;
                                 7
                                   rep = julre + julmo
                                   IF opc4 <> 2
                                        rep = rep + julot
                                   ENDIF
                              ELSE
                                   IF mes = 8
                                        rep = agore + agomo
                                        IF opc4 <> 2
                                             rep = rep + agoot
                                        ENDIF
                                   ELSE
                                        IF mes = 9
                                             rep = setre + setmo
                                             IF opc4 <> 2
                                                  rep = rep + setot
                                             ENDIF
                                        ELSE
                                             IF mes = 10
                                                  rep = octre + octmo
                                                  IF opc4 <> 2
                                                       rep = rep + octot
                                                  ENDIF
                                             ELSE
                                                  IF mes = 11
                                                       rep = novre + novmo
                                                       IF opc4 <> 2
                                                            rep = rep + novot
                                                       ENDIF
                                                  ELSE
                                                       rep = dicre + dicmo
                                                       IF opc4 <> 2
                                                            rep = rep + dicot
                                                       ENDIF
                                                  ENDIF
                                             ENDIF
                                        ENDIF
                                   ENDIF
                              ENDIF
                         ENDIF
                    ENDIF
               ENDIF
          ENDIF
     ENDIF
ENDIF
IF opc = 2
     SELECT 20
ENDIF
RETURN rep
*
FUNCTION datim2
PARAMETER opc, mes
IF opc = 1
     SELECT 20
ELSE
     SELECT gc_hpres
ENDIF
STORE 0 TO rep
IF mes >= 1
     rep = enere + enemo
     IF opc4 <> 2
          rep = rep + eneot
     ENDIF
ENDIF
IF mes >= 2
     rep = rep + febre + febmo
     IF opc4 <> 2
          rep = rep + febot
     ENDIF
ENDIF
IF mes >= 3
     rep = rep + marre + marmo
     IF opc4 <> 2
          rep = rep + marot
     ENDIF
ENDIF
IF mes >= 4
     rep = rep + abrre + abrmo
     IF opc4 <> 2
          rep = rep + abrot
     ENDIF
ENDIF
IF mes >= 5
     rep = rep + mayre + maymo
     IF opc4 <> 2
          rep = rep + mayot
     ENDIF
ENDIF
IF mes >= 6
     rep = rep + junre + junmo
     IF opc4 <> 2
          rep = rep + junot
     ENDIF
ENDIF
IF mes >= 7
     rep = rep + julre + julmo
     IF opc4 <> 2
          rep = rep + julot
     ENDIF
ENDIF
IF mes >= 8
     rep = rep + agore + agomo
     IF opc4 <> 2
          rep = rep + agoot
     ENDIF
ENDIF
IF mes >= 9
     rep = rep + setre + setmo
     IF opc4 <> 2
          rep = rep + setot
     ENDIF
ENDIF
IF mes >= 10
     rep = rep + octre + octmo
     IF opc4 <> 2
          rep = rep + octot
     ENDIF
ENDIF
IF mes >= 11
     rep = rep + novre + novmo
     IF opc4 <> 2
          rep = rep + novot
     ENDIF
ENDIF
IF mes = 12
     rep = rep + dicre + dicmo
     IF opc4 <> 2
          rep = rep + dicot
     ENDIF
ENDIF
IF opc = 2
     SELECT 20
ENDIF
RETURN rep
*
FUNCTION datun2
PARAMETER opc, mes
IF opc = 1
     SELECT 20
ELSE
     SELECT gc_hpres
ENDIF
STORE 0 TO rep
IF mes >= 1
     rep = eneun
ENDIF
IF mes >= 2
     rep = rep + febun
ENDIF
IF mes >= 3
     rep = rep + marun
ENDIF
IF mes >= 4
     rep = rep + abrun
ENDIF
IF mes >= 5
     rep = rep + mayun
ENDIF
IF mes >= 6
     rep = rep + junun
ENDIF
IF mes >= 7
     rep = rep + julun
ENDIF
IF mes >= 8
     rep = rep + agoun
ENDIF
IF mes >= 9
     rep = rep + setun
ENDIF
IF mes >= 10
     rep = rep + octun
ENDIF
IF mes >= 11
     rep = rep + novun
ENDIF
IF mes = 12
     rep = rep + dicun
ENDIF
IF opc = 2
     SELECT 20
ENDIF
RETURN rep
*
FUNCTION paso3
PARAMETER opc, mes, mes2
STORE 0 TO trep, rep
FOR x = mes TO mes2
     DO datimp WITH opc, x
     trep = trep + rep
ENDFOR
RETURN trep
*
FUNCTION paso4
PARAMETER opc, mes, mes2
STORE 0 TO trep, uni
FOR x = mes TO mes2
     DO datoun WITH opc, x
     trep = trep + uni
ENDFOR
RETURN trep
*
*** 
*** ReFox - retrace your steps ... 
***
