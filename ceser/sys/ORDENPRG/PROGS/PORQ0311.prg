*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER opc
ind_prg = '<PORQ0311>'
tit_prg = 'CONSULTA'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL f10
PUBLIC pas311
CLOSE DATABASES
@ 2, 1 SAY DATE()
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' CONSULTA ESTADISTICAS DE REPARACIONES'
ppas = .T.
SET CURSOR ON
DO WHILE ppas
     unimon = 0
     wk_ano = 0
     f1 = 0
     f2 = 0
     c1 = 0
     c2 = 0
     mensaje = '       '
     procede = '  '
     consin = SPACE(4)
     sw_algo = .F.
     con_eli = 0
     wk_numdoc = 0
     opcion = SPACE(20)
     DO esc_modo WITH 'C'
     DO esc_indica WITH 1, 'AYU',  ;
        'mbv', 'bbb', 'SEL'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'bbb', 'ESC'
     wk_fevenga = CTOD( ;
                  '  /  /  ')
     wk_codpag = '    '
     wk_fecven = CTOD('  /  /  ')
     wk_monto = 0
     wk_fle = 0
     wk_otro = 0
     sw_algo = .F.
     ON KEY LABEL f10 do pop
     DO eli_garan
     ON KEY LABEL f10
     IF LASTKEY() = 27
          ppas = .F.
     ENDIF
     DEACTIVATE POPUP unag
     RELEASE POPUP unag
     total1 = 0
     total2 = 0
     total22 = 0
     IF  .NOT. EMPTY(procede)
          do &procede
     ENDIF
     IF LASTKEY() = -9
          LOOP
     ENDIF
     IF LASTKEY() = 27
          EXIT
     ENDIF
ENDDO
DO saca_win
@ 24, 69 SAY SPACE(10)
ON KEY LABEL F6
ON KEY LABEL F10
SET CURSOR OFF
SELECT 1
USE
CLEAR TYPEAHEAD
CLOSE DATABASES
RELEASE pas311
RETURN
*
PROCEDURE eli_garan
DEFINE POPUP unag FROM 4, 10 TO 8,  ;
       30 SHADOW
DEFINE BAR 1 OF unag PROMPT  ;
       'Ordenes en Garantia '
DEFINE BAR 2 OF unag PROMPT  ;
       'Ordenes fuera Garan '
DEFINE BAR 3 OF unag PROMPT  ;
       'Ambos               '
ON SELECTION POPUP unag do choice1 with;
bar()
ACTIVATE POPUP unag
DEACTIVATE POPUP unag
RELEASE POPUP unag
*
PROCEDURE choice1
PARAMETER num
c1 = 30
c2 = 50
f1 = 0
f2 = 0
DO CASE
     CASE num = 1
          mensaje = ' Con Garant¡a '
          consin = 'GARA'
          f1 = 5
          f2 = 9
     CASE num = 2
          consin = 'FGAR'
          mensaje = 'Sin Garant¡a '
          f1 = 6
          f2 = 10
     CASE num = 3
          mensaje = 'Con y Sin Garant¡a '
          consin = 'AMBO'
          f1 = 7
          f2 = 11
ENDCASE
DEFINE POPUP dosg FROM f1, c1 TO  ;
       f2, c2 SHADOW
DEFINE BAR 1 OF dosg PROMPT  ;
       '   Por unidad    '
DEFINE BAR 2 OF dosg PROMPT  ;
       '   Por Monto     '
DEFINE BAR 3 OF dosg PROMPT  ;
       '   Ambos         '
ON SELECTION POPUP dosg do choice2 with;
bar()
ACTIVATE POPUP dosg
DEACTIVATE POPUP unag
RELEASE POPUP unag
RETURN
*
PROCEDURE choice2
PARAMETER op
c1 = 50
c2 = 70
DO CASE
     CASE op = 1
          mensaje = mensaje +  ;
                    ', Por Unidad '
          unimon = 1
          t1 = f1 + 1
          t2 = f2 + 2
     CASE op = 2
          mensaje = mensaje +  ;
                    ', Por Monto '
          unimon = 2
          t1 = f1 + 2
          t2 = f2 + 3
     CASE op = 3
          unimon = 3
          mensaje = mensaje +  ;
                    ',Por Monto y Unidad '
          t1 = f1 + 3
          t2 = f2 + 4
ENDCASE
DEFINE POPUP tresg FROM t1, c1 TO  ;
       t2, c2 SHADOW
DEFINE BAR 1 OF tresg PROMPT  ;
       '  Por Modelo    '
DEFINE BAR 2 OF tresg PROMPT  ;
       '  Por T‚cnico   '
DEFINE BAR 3 OF tresg PROMPT  ;
       '  Por Marca     '
DEFINE BAR 4 OF tresg PROMPT  ;
       '  Por Falla     '
ON SELECTION POPUP tresg do choice3 with;
bar()
ACTIVATE POPUP tresg
DEACTIVATE POPUP dosg
RELEASE POPUP dosg
RETURN
*
PROCEDURE choice3
PARAMETER opc
IF LASTKEY() <> 27
     SELECT 1
     pas311 = da_nombre()
     Create table &PAS311 (pas_mes C(10),pas_cantid;
N(12),pas_monto N(12),pas_monto2 N(12))
     SELECT 1
     use &PAS311 SHARED
     APPEND BLANK
     REPLACE pas_mes WITH 'Enero'
     APPEND BLANK
     REPLACE pas_mes WITH  ;
             'Febrero'
     APPEND BLANK
     REPLACE pas_mes WITH 'Marzo'
     APPEND BLANK
     REPLACE pas_mes WITH 'Abril'
     APPEND BLANK
     REPLACE pas_mes WITH 'Mayo'
     APPEND BLANK
     REPLACE pas_mes WITH 'Junio'
     APPEND BLANK
     REPLACE pas_mes WITH 'Julio'
     APPEND BLANK
     REPLACE pas_mes WITH  ;
             'Agosto'
     APPEND BLANK
     REPLACE pas_mes WITH  ;
             'Septiembre'
     APPEND BLANK
     REPLACE pas_mes WITH  ;
             'Octubre'
     APPEND BLANK
     REPLACE pas_mes WITH  ;
             'Noviembre'
     APPEND BLANK
     REPLACE pas_mes WITH  ;
             'Diciembre'
     GOTO TOP
     USE
ENDIF
DO CASE
     CASE opc = 1
          mensaje = mensaje +  ;
                    ' y Por Modelo'
          procede = 'modelo'
     CASE opc = 2
          mensaje = mensaje +  ;
                    ' y Por T‚cnico'
          procede = 'tecnico'
     CASE opc = 3
          mensaje = mensaje +  ;
                    'y Por Marca'
          procede = 'marca'
     CASE opc = 4
          mensaje = mensaje +  ;
                    'y Por Falla'
          procede = 'falla'
ENDCASE
DEACTIVATE POPUP tresg
RELEASE POPUP tresg
RETURN
*
PROCEDURE ambos
RETURN
*
PROCEDURE modelo
ON KEY LABEL F6 DO AYUTEC
DO esc_indica WITH 1, 'AYU',  ;
   'bbb', 'bbb', 'SEL'
wk_ano = 0
wk_codmar = '    '
wk_codmod = SPACE(15)
@ 4, 2 SAY 'Consulta :' + mensaje
@ 5, 2 SAY 'C¢digo de Marca  :'
@ 6, 2 SAY 'C¢digo de Modelo :'
@ 7, 2 SAY 'A¤o de Proceso   :'
@ 5, 22 GET wk_codmar PICTURE  ;
  '!!!!' VALID valtab('MARC', ;
  wk_codmar,30,40) WHEN  ;
  colocaf6()
@ 6, 22 GET wk_codmod PICTURE  ;
  '!!!!!!!!!!!!!!!' VALID  ;
  codalf(wk_codmod) WHEN  ;
  colocaf6()
@ 7, 22 GET wk_ano PICTURE '99'  ;
  VALID vali_cero(wk_ano)
READ
IF LASTKEY() <> 27
     SELECT 1
     use &PAS311   
     DO CASE
          CASE consin = 'GARA'
               pregunta = 'indori="GARA"'
          CASE consin = 'FGAR'
               pregunta = 'indori="FGAR"'
          CASE consin = 'AMBO'
               pregunta = 'indori="FGAR".or.indori="FGAR"'
     ENDCASE
     SELECT 2
     USE SHARED st_estad ORDER  ;
         CODIGO
     DO CASE
          CASE unimon = 1
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    count to total1 for;
(codmod=wk_codmod).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
.and.(codmar=wk_codmar)
                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
               ENDFOR
          CASE unimon = 2
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    sum valmao for (codmod=wk_codmod).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total2
                    sum valrep for (codmod=wk_codmod).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total22
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
               ENDFOR
          CASE unimon = 3
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    count to total1 for;
(codmod=wk_codmod).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
.and.(codmar=wk_codmar)
                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
               ENDFOR
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    sum valmao for (codmod=wk_codmod).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total2
                    sum valrep for (codmod=wk_codmod).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total22
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
               ENDFOR
     ENDCASE
     DO brau_mues
ENDIF
RETURN
*
PROCEDURE tecnico
ON KEY LABEL F6 DO AYUTEC
DO esc_indica WITH 1, 'AYU',  ;
   'bbb', 'bbb', 'SEL'
wk_ano = 0
wk_codtec = 0
@ 5, 2 SAY 'Consulta :' + mensaje
@ 6, 2 SAY 'C¢digo de T‚cnico:'
@ 7, 2 SAY 'A¤o de Proceso   :'
@ 6, 22 GET wk_codtec PICTURE  ;
  '999999999' VALID  ;
  vali_tec(wk_codtec) WHEN  ;
  colocaf6()
@ 7, 22 GET wk_ano PICTURE '99'  ;
  VALID vali_cero(wk_ano)
READ
IF LASTKEY() <> 27
     SELECT 1
     use &PAS311  
     DO CASE
          CASE consin = 'GARA'
               pregunta = 'indori="GARA"'
          CASE consin = 'FGAR'
               pregunta = 'indori="FGAR"'
          CASE consin = 'AMBO'
               pregunta = 'indori="FGAR".or.indori="FGAR"'
     ENDCASE
     SELECT 2
     USE SHARED st_estad ORDER  ;
         CODIGO
     DO CASE
          CASE unimon = 1
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    count to total1 for;
(codtec=str(wk_codtec,9)).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);

                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
               ENDFOR
          CASE unimon = 2
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    sum valmao for (codtec=str(wk_codtec,9)).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
to total2
                    sum valrep for (codtec=str(wk_codtec,9)).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
to total22
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
               ENDFOR
          CASE unimon = 3
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    count to total1 for;
(codtec=str(wk_codtec,9)).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);

                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
               ENDFOR
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    sum valmao for (codtec=str(wk_codtec,9)).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
to total2
                    sum valrep for (codtec=str(wk_codtec,9)).and.(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
to total22           
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
               ENDFOR
     ENDCASE
     DO brau_mues
ENDIF
RETURN
*
PROCEDURE marca
ON KEY LABEL F6 DO AYUTEC
DO esc_indica WITH 1, 'AYU',  ;
   'bbb', 'bbb', 'SEL'
wk_ano = 0
wk_codmar = '    '
@ 4, 2 SAY 'Consulta :' + mensaje
@ 6, 2 SAY 'C¢digo de Marca  :'
@ 7, 2 SAY 'A¤o de Proceso   :'
@ 6, 22 GET wk_codmar PICTURE  ;
  '!!!!' VALID valtab('MARC', ;
  wk_codmar,30,30) WHEN  ;
  colocaf6()
@ 7, 22 GET wk_ano PICTURE '99'  ;
  VALID vali_cero(wk_ano)
READ
IF LASTKEY() <> 27
     SELECT 1
     use &PAS311  
     DO CASE
          CASE consin = 'GARA'
               pregunta = 'indori="GARA"'
          CASE consin = 'FGAR'
               pregunta = 'indori="FGAR"'
          CASE consin = 'AMBO'
               pregunta = 'indori="FGAR".or.indori="FGAR"'
     ENDCASE
     SELECT 2
     USE SHARED st_estad ORDER  ;
         CODIGO
     DO CASE
          CASE unimon = 1
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    count to total1 for;
(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
.and.(codmar=wk_codmar)
                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
               ENDFOR
          CASE unimon = 2
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    sum valmao for (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total2
                    sum valrep for (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total22                 
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
               ENDFOR
          CASE unimon = 3
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    count to total1 for;
(anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes);
.and.(codmar=wk_codmar)
                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
               ENDFOR
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    sum valmao for (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total2     
                    sum valrep for (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes).and.(codmar=wk_codmar);
to total22
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
               ENDFOR
     ENDCASE
     DO brau_mues
ENDIF
RETURN
*
PROCEDURE falla
ON KEY LABEL F6 DO AYUTEC
DO esc_indica WITH 1, 'AYU',  ;
   'bbb', 'bbb', 'SEL'
wk_ano = 0
wk_codfal = '    '
@ 4, 2 SAY 'Consulta :' + mensaje
@ 6, 2 SAY 'C¢digo de Falla  :'
@ 7, 2 SAY 'A¤o de Proceso   :'
@ 6, 22 GET wk_codfal PICTURE  ;
  '!!!!' VALID valtab('FALL', ;
  wk_codfal,30,30) WHEN  ;
  colocaf6()
@ 7, 22 GET wk_ano PICTURE '99'  ;
  VALID vali_cero(wk_ano)
READ
IF LASTKEY() <> 27
     SELECT 1
     use &PAS311  
     DO CASE
          CASE consin = 'GARA'
               pregunta = 'indori="GARA"'
          CASE consin = 'FGAR'
               pregunta = 'indori="FGAR"'
          CASE consin = 'AMBO'
               pregunta = 'indori="FGAR".or.indori="FGAR"'
     ENDCASE
     SELECT 10
     USE SHARED st_estfa ORDER  ;
         CODIGO
     SELECT 2
     USE SHARED st_estad ORDER  ;
         CODIGO
     DO CASE
          CASE unimon = 1
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    DO WHILE   ;
                       .NOT.  ;
                       EOF()
                         if (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes)
                              wk_numord =  ;
                               numord
                              SELECT  ;
                               10
                              SEEK  ;
                               STR(wk_numord,  ;
                               8)
                              IF FOUND()
                                   DO WHILE STR(wk_numord, 8)=numord
                                        IF codfal = wk_codfal
                                             total1 = total1 + 1
                                             EXIT
                                        ENDIF
                                        SKIP
                                   ENDDO
                              ENDIF
                         ENDIF
                         SELECT 2
                         SKIP
                    ENDDO
                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
                    total1 = 0
               ENDFOR
          CASE unimon = 2
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    DO WHILE   ;
                       .NOT.  ;
                       EOF()
                         if (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes)
                              wk_numord =  ;
                               numord
                              suma =  ;
                               valmao
                              suma2 =  ;
                               valrep
                              SELECT  ;
                               10
                              SEEK  ;
                               STR(wk_numord,  ;
                               8)
                              IF FOUND()
                                   DO WHILE STR(wk_numord, 8)=numord
                                        IF codfal = wk_codfal
                                             total2 = total2 + suma
                                             total22 = total22 + suma2
                                        ENDIF
                                        SKIP
                                   ENDDO
                              ENDIF
                         ENDIF
                         SELECT 2
                         SKIP
                    ENDDO
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
                    total2 = 0
                    total22 = 0
               ENDFOR
          CASE unimon = 3
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    DO WHILE   ;
                       .NOT.  ;
                       EOF()
                         if (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes)
                              wk_numord =  ;
                               numord
                              SELECT  ;
                               10
                              SEEK  ;
                               STR(wk_numord,  ;
                               8)
                              IF FOUND()
                                   DO WHILE STR(wk_numord, 8)=numord
                                        IF codfal = wk_codfal
                                             total1 = total1 + 1
                                             EXIT
                                        ENDIF
                                        SKIP
                                   ENDDO
                              ENDIF
                         ENDIF
                         SELECT 2
                         SKIP
                    ENDDO
                    SELECT 1
                    GOTO i
                    REPLACE pas_cantid  ;
                            WITH  ;
                            total1
                    total1 = 0
               ENDFOR
               FOR i = 1 TO 12
                    wk_mes = i
                    SELECT 2
                    GOTO TOP
                    DO WHILE   ;
                       .NOT.  ;
                       EOF()
                         if (anorep=wk_ano).and.(&pregunta).and.(mesrep=wk_mes)
                              wk_numord =  ;
                               numord
                              suma =  ;
                               valmao
                              suma2 =  ;
                               valrep
                              SELECT  ;
                               10
                              SEEK  ;
                               STR(wk_numord,  ;
                               8)
                              IF FOUND()
                                   DO WHILE STR(wk_numord, 8)=numord
                                        IF codfal = wk_codfal
                                             total2 = total2 + suma
                                             total22 = total22 + suma2
                                        ENDIF
                                        SKIP
                                   ENDDO
                              ENDIF
                         ENDIF
                         SELECT 2
                         SKIP
                    ENDDO
                    SELECT 1
                    GOTO i
                    REPLACE pas_monto  ;
                            WITH  ;
                            total2
                    REPLACE pas_monto2  ;
                            WITH  ;
                            total22
                    total2 = 0
                    total22 = 0
               ENDFOR
     ENDCASE
     DO brau_mues
ENDIF
RETURN
*
FUNCTION vali_fe
PARAMETER toc
IF EMPTY(toc)
     DO error WITH  ;
        '**Debe Ingresar Fecha**'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION vali_cero
PARAMETER cero
IF EMPTY(cero)
     wk_ano = VAL(SUBSTR(STR(YEAR(DATE()),  ;
              4), 3, 2))
     RETURN .T.
ENDIF
IF cero <= 0
     DO error WITH  ;
        '** Debe Ser Mayor que 0 **'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION vali_tec
PARAMETER tec
SELECT 3
USE SHARED st_itecn ORDER CODIGO
SEEK STR(tec, 9)
IF FOUND()
     wk_nombre = SUBSTR(noment, 1,  ;
                 20)
     @ 6, 33 SAY wk_nombre
     RETURN .T.
ELSE
     DO error WITH  ;
        '**C¢digo De T‚cnico No Existe**'
     RETURN .F.
ENDIF
RETURN
*
PROCEDURE ayutec
ON KEY LABEL F6
IF VARREAD() = 'WK_CODTEC'
     USE SHARED st_itecn ORDER  ;
         CODIGO
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     USE
ENDIF
IF VARREAD() = 'WK_CODMOD'
     USE SHARED st_imode ORDER  ;
         CODIGO
     SET FILTER TO codmar == wk_codmar
     GOTO TOP
     campoa = 'codmod+" "+nommod'
     campob = 'nommod+" "+codmod'
     titulo = 'AYUDA DE MODELOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo, 'codmod'
     USE
ENDIF
IF VARREAD() = 'WK_CODMAR'
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF VARREAD() = 'WK_CODFAL'
     USE SHARED ge_tab0 ORDER  ;
         codigo
     SET FILTER TO tab_codpre == 'FALL'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE FALLAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
ON KEY LABEL F6 DO AYUTEC 
RETURN
*
PROCEDURE brau_mues
STORE 0 TO totcan, totobra,  ;
      totrepu
DO esc_indica WITH 1, 'AYU',  ;
   'ant', 'ste', 'BBB'
DO esc_indica WITH 2, 'mbv',  ;
   'BBB', 'BBB', 'ESC'
SELECT 1
GOTO TOP
SUM pas_cantid TO totcan
SUM pas_monto TO totobra
SUM pas_monto2 TO totrepu
ACTIVATE WINDOW trabajo
@ 14, 29 SAY 'TOTALES ==>'
@ 14, 40 SAY totcan PICTURE  ;
  '99999999'
@ 14, 50 SAY totobra PICTURE  ;
  '999,999.99'
@ 14, 62 SAY totrepu PICTURE  ;
  '999,999.99'
DEFINE WINDOW estad FROM 09, 28  ;
       TO 15, 74
ACTIVATE WINDOW estad
BROWSE FIELDS pas_mes :H = ' MES',  ;
       pas_cantid :H = 'CANTIDAD'  ;
       :P = '99999999', pas_monto  ;
       :H = ' MANO OBRA' :P =  ;
       '9999,999.99', pas_monto2  ;
       :H = ' REPUESTOS' :P =  ;
       '9999,999.99' NOEDIT IN  ;
       estad
DEACTIVATE WINDOW estad
RELEASE WINDOW estad
@ 14, 29 SAY SPACE(45)
RETURN
*
PROCEDURE pop
KEYBOARD '{RIGHTARROW}'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
