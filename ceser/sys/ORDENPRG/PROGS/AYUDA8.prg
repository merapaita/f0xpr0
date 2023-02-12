*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER campoa, w_origen,  ;
          w_select
ACTIVATE WINDOW indicar
SAVE SCREEN TO w_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
STORE SPACE(01) TO w_tipo
STORE SPACE(20) TO w_cliente
DEFINE POPUP ayu1 FROM 8, 05  ;
       SHADOW COLOR SCHEME 05
IF w_origen = 'OR'
     DEFINE BAR 1 OF ayu1 PROMPT  ;
            ' Ordenados por \<S/S     '
     DEFINE BAR 2 OF ayu1 PROMPT  ;
            ' Ordenados por \<Fecha   '
     DEFINE BAR 3 OF ayu1 PROMPT  ;
            ' Ordenados por S\<erie   '
     DEFINE BAR 4 OF ayu1 PROMPT  ;
            ' Ordenados por \<Cliente '
     DEFINE BAR 5 OF ayu1 PROMPT  ;
            ' Ordenados por \<O/R     '
ENDIF
IF w_origen = 'SS'
     DEFINE BAR 1 OF ayu1 PROMPT  ;
            ' Ordenados por \<S/S     '
     DEFINE BAR 2 OF ayu1 PROMPT  ;
            ' Ordenados por \<Fecha   '
     DEFINE BAR 3 OF ayu1 PROMPT  ;
            ' Ordenados por S\<erie   '
     DEFINE BAR 4 OF ayu1 PROMPT  ;
            ' Ordenados por \<Cliente '
ENDIF
IF w_origen = 'PR'
     DEFINE BAR 1 OF ayu1 PROMPT  ;
            ' Ordenados por \<S/S     '
     DEFINE BAR 2 OF ayu1 PROMPT  ;
            ' Ordenados por \<Fecha   '
     DEFINE BAR 3 OF ayu1 PROMPT  ;
            ' Ordenados por S\<erie   '
     DEFINE BAR 4 OF ayu1 PROMPT  ;
            ' Ordenados por \<Cliente '
     DEFINE BAR 5 OF ayu1 PROMPT  ;
            ' Ordenados por \<O/R     '
     DEFINE BAR 6 OF ayu1 PROMPT  ;
            ' Ordenados por \<Presupu.'
ENDIF
ON SELECTION POPUP ayu1 do choice_4 with;
bar()
ACTIVATE POPUP ayu1
RELEASE WINDOW ingrcli, ayu3,  ;
        ayu4, error, clientes
DEACTIVATE POPUP ayu1
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM w_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice_4
PARAMETER clave
DO CASE
     CASE BAR() = 1
          DEFINE WINDOW ayu3 FROM  ;
                 12, 32 TO 14, 60  ;
                 SHADOW
          ACTIVATE WINDOW TOP  ;
                   ayu3
          IF w_origen = 'OR'
               SET ORDER TO ord_numsol
          ELSE
               IF w_origen = 'PR'
                    SET ORDER TO st_numsol
               ELSE
                    SET ORDER TO codigo
               ENDIF
          ENDIF
          campox = campoa
          w_codigo = 0
          w_tipo = 'O'
          CLEAR GETS
          @ 00, 01 SAY  ;
            'N£mero Desde :' GET  ;
            w_codigo PICTURE  ;
            '99999999'
          READ
     CASE BAR() = 2
          DEFINE WINDOW ayu3 FROM  ;
                 12, 32 TO 14, 60  ;
                 SHADOW
          ACTIVATE WINDOW TOP  ;
                   ayu3
          DO CASE
               CASE w_origen =  ;
                    'SS'
                    SET ORDER TO sol_fchemi
               CASE w_origen =  ;
                    'PR'
                    SET ORDER TO pre_fecemi
               CASE w_origen =  ;
                    'OR'
                    SET ORDER TO ord_fchemi
          ENDCASE
          campox = campoa
          w_codigo = DATE()
          w_tipo = 'D'
          CLEAR GETS
          @ 00, 01 SAY  ;
            'Fecha  Desde :' GET  ;
            w_codigo
          READ
     CASE BAR() = 3
          DEFINE WINDOW ayu3 FROM  ;
                 12, 32 TO 14, 60  ;
                 SHADOW
          ACTIVATE WINDOW TOP  ;
                   ayu3
          DO CASE
               CASE w_origen =  ;
                    'SS'
                    SET ORDER TO sol_serie
               CASE w_origen =  ;
                    'PR'
                    SET ORDER TO st_numser
               CASE w_origen =  ;
                    'OR'
                    SET ORDER TO ord_numser
          ENDCASE
          campox = campoa
          w_codigo = SPACE(20)
          w_tipo = 'S'
          CLEAR GETS
          @ 00, 01 SAY  ;
            'Serie  Desde :' GET  ;
            w_codigo PICTURE  ;
            '@!'
          READ
     CASE BAR() = 4
          campox = campoa
          w_tipo = 'C'
          DEFINE WINDOW ingrcli  ;
                 FROM 13, 32 TO  ;
                 15, 63 SHADOW
          ACTIVATE WINDOW ingrcli
          CLEAR GETS
          @ 00, 00 SAY  ;
            'Cliente :' GET  ;
            w_cliente PICTURE  ;
            '@!' VALID  ;
            oovalid(VARREAD())
          READ
          IF LASTKEY() <> 27  ;
             .AND.  .NOT.  ;
             EMPTY(w_cliente)
               DEACTIVATE WINDOW  ;
                          ingrcli
               IF SROWS() <= 25
                    DO mensa WITH  ;
                       '  Espere Un Momento...',  ;
                       'COLO'
               ELSE
                    DO mensa2  ;
                       WITH  ;
                       '  Espere Un Momento...',  ;
                       'COLO'
               ENDIF
          ENDIF
     CASE BAR() = 5
          DEFINE WINDOW ayu3 FROM  ;
                 12, 32 TO 14, 60  ;
                 SHADOW
          ACTIVATE WINDOW TOP  ;
                   ayu3
          DO CASE
               CASE w_origen =  ;
                    'PR'
                    SET ORDER TO pre_numord
               CASE w_origen =  ;
                    'OR'
                    SET ORDER TO codigo
          ENDCASE
          campox = campoa
          w_codigo = 0
          w_tipo = 'N'
          CLEAR GETS
          @ 00, 01 SAY  ;
            'N£mero Desde :' GET  ;
            w_codigo PICTURE  ;
            '99999999'
          READ
     CASE BAR() = 6
          DEFINE WINDOW ayu3 FROM  ;
                 12, 32 TO 14, 60  ;
                 SHADOW
          ACTIVATE WINDOW TOP  ;
                   ayu3
          DO CASE
               CASE w_origen =  ;
                    'PR'
                    SET ORDER TO codigo
          ENDCASE
          campox = campoa
          w_codigo = 0
          w_tipo = 'N'
          CLEAR GETS
          @ 00, 01 SAY  ;
            'N£mero Desde :' GET  ;
            w_codigo PICTURE  ;
            '99999999'
          READ
ENDCASE
IF LASTKEY() = 27
     IF SROWS() <= 25
          DO mensa WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ELSE
          DO mensa2 WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ENDIF
     DEACTIVATE WINDOW ayu3,  ;
                error
     RELEASE WINDOW ingrcli,  ;
             clientes, ayu3, ayu4,  ;
             error
     DEACTIVATE POPUP ayu1
     RETURN
ENDIF
SET NEAR ON
DO CASE
     CASE w_tipo = 'O'
          SEEK STR(w_codigo, 8)
     CASE w_tipo = 'D'
          SEEK dtoc2(w_codigo)
     CASE w_tipo = 'S'
          SEEK w_codigo
     CASE w_tipo = 'N'
          SEEK STR(w_codigo, 8)
     CASE w_tipo = 'C'
          SET NEAR OFF
          DO nombre
          RETURN
ENDCASE
SET NEAR OFF
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
IF w_origen = 'SS'
     browse field cer = " " :H="", uno;
= &campox :H=" NUMERO  FECHA      SERIE         COD.CLIENTE MODELO        EST ATENC";
     in window ayu4 nowait  freeze cer;

ELSE
     browse field cer = " " :H="", uno;
= &campox :H=" S/SER.  FECHA       O/REPA. SERIE         COD.CLI. MODELO    EST ATENC";
   in window ayu4 nowait  freeze cer 
ENDIF
ACTIVATE WINDOW TOP ayu4
ventana = SUBSTR(DBF(),  ;
          LEN(DBF()) - 11, 8)
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          02 TO 7 + j, 75
     zoom window &ventana norm from -1,-4;
to j,70
ENDFOR
DEACTIVATE WINDOW ayu3
ON KEY LABEL enter do choice24
BROWSE LAST
ON KEY LABEL enter
DEACTIVATE WINDOW ayu4
DEACTIVATE POPUP ayu1
RETURN
*
PROCEDURE choice24
IF LASTKEY() = 13
     IF w_origen = 'OR'
          w_aux = ALLTRIM(numsol)
     ELSE
          w_aux = ALLTRIM(numdoc)
     ENDIF
     IF LEN(w_aux) < 8
          w_aux = w_aux + CHR(13)
     ENDIF
     KEYBOARD CHR(27) + w_aux
ENDIF
RETURN
*
PROCEDURE nombre
SELECT st_iclpr
SET ORDER TO cli_noment
SEEK 'C' + ALLTRIM(w_cliente)
IF  .NOT. FOUND()
     SET ORDER TO codigo
     DO error WITH  ;
        '  No hay Informaci¢n...'
ELSE
     mnomcli = LEFT(noment, 23)
     x = 1
     SKIP -25
     DIMENSION cliente( 50, 2)
     DO WHILE (x<=50)
          IF indent = 'C'
               cliente( x, 1) =  ;
                      codent
               cliente( x, 2) =  ;
                      noment
               x = x + 1
          ENDIF
          IF  .NOT. EOF()
               SKIP
          ELSE
               EXIT
          ENDIF
     ENDDO
     xx = x - 1
     SET ORDER TO codigo
     IF w_origen = 'OR'
          SELECT st_iorep
          SET ORDER TO codent
     ELSE
          IF w_origen = 'SS'
               SELECT st_isrep
               SET ORDER TO codent
          ELSE
               SELECT st_ispre
               SET ORDER TO pre_codent
          ENDIF
     ENDIF
     GOTO TOP
     cli = 1
     FOR x = 1 TO xx
          busca_cli = cliente(x, ;
                      1)
          SEEK (cliente(x,1))
          IF  .NOT. FOUND()
               LOOP
          ENDIF
          SCAN WHILE codent =  ;
               cliente(x,1) .AND.   ;
               .NOT. EOF()
               DIMENSION clientes(  ;
                         cli)
               IF w_origen = 'OR'  ;
                  .OR. w_origen =  ;
                  'PR'
                    clientes(  ;
                            cli) =  ;
                            LEFT(cliente(x, ;
                            2),  ;
                            23) +  ;
                            '³' +  ;
                            numsol +  ;
                            '³' +  ;
                            DTOC(fecemi) +  ;
                            '³' +  ;
                            numdoc +  ;
                            '³' +  ;
                            LEFT(numser,  ;
                            10) +  ;
                            '³' +  ;
                            codmod
               ELSE
                    clientes(  ;
                            cli) =  ;
                            LEFT(cliente(x, ;
                            2),  ;
                            23) +  ;
                            '³' +  ;
                            numdoc +  ;
                            '³' +  ;
                            DTOC(fecemi) +  ;
                            '³' +  ;
                            LEFT(numser,  ;
                            10) +  ;
                            '³' +  ;
                            codmod
               ENDIF
               cli = cli + 1
          ENDSCAN
     ENDFOR
     IF SROWS() <= 25
          DO mensa WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ELSE
          DO mensa2 WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ENDIF
     IF xx > 0
          cl = ASCAN(clientes,  ;
               LEFT(mnomcli,  ;
               23))
          IF cl = 0
               cl = 1
          ENDIF
          DEFINE WINDOW clientes  ;
                 FROM 06, 0 TO 19,  ;
                 75 SHADOW TITLE  ;
                 ' CLIENTES '
          ACTIVATE WINDOW  ;
                   clientes
          IF w_origen = 'OR' .OR.  ;
             w_origen = 'PR'
               @ 00, 00 SAY  ;
                 '   C L I E N T E           S/SERV. FECHA       DOCUMEN SERIE      MODELO   '  ;
                 COLOR N/W 
          ELSE
               @ 00, 00 SAY  ;
                 '   C L I E N T E           S/SERV. FECHA      SERIE      MODELO           '  ;
                 COLOR N/W 
          ENDIF
          @ 01, 00 GET client  ;
            DEFAULT clientes(cl)  ;
            SIZE 12, 74 FROM  ;
            clientes VALID  ;
            oovalid(VARREAD())  ;
            COLOR SCHEME 9
          READ
     ENDIF
ENDIF
DEACTIVATE WINDOW clientes
RELEASE WINDOW ingrcli, clientes,  ;
        ayu3, ayu4, error
DEACTIVATE POPUP ayu1
RETURN
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4
ACTIVATE WINDOW mensj
@ 01, (WCOLS('MENSJ') - l) / 2  ;
  SAY mens COLOR GR+/N* 
= INKEY(1)
SET CURSOR ON
RETURN
*
FUNCTION oovalid
PARAMETER opc
DO CASE
     CASE opc = 'W_CLIENTE'
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .F.
          ENDIF
          IF EMPTY(w_cliente)
               DO error WITH  ;
                  '  Ingrese Cliente...'
               RETURN .F.
          ENDIF
     CASE opc = 'CLIENT'
          IF LASTKEY() = 19 .OR.  ;
             LASTKEY() = 5
               RETURN .F.
          ENDIF
          IF LASTKEY() = 13
               w_aux = ALLTRIM(SUBSTR(client,  ;
                       25, 8))
               w_numero = VAL(w_aux)
               KEYBOARD CHR(13)
          ENDIF
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
