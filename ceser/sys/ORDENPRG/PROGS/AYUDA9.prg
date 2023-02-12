*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER campo1, wrk_origen
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
DIMENSION op( 6)
op( 1) =  ;
  ' Ordenados por Numero  '
op( 2) =  ;
  ' Ordenados por Fecha   '
op( 3) =  ;
  ' Ordenados por Serie   '
op( 4) =  ;
  ' Ordenados por S/Serv. '
op( 5) =  ;
  ' Ordenados por Cliente '
op( 6) =  ;
  ' Ordenados por O/Rep.  '
DEFINE POPUP ayu1 FROM 0, 0  ;
       SHADOW COLOR SCHEME 8
FOR i = 1 TO 6
     DEFINE BAR i OF ayu1 PROMPT  ;
            op(i)
ENDFOR
ON SELECTION POPUP ayu1 do choice_4 with;
prompt()
ACTIVATE POPUP ayu1 NOWAIT
FOR i = 0 TO 08
     MOVE POPUP ayu1 TO i, 0
ENDFOR
FOR i = 0 TO 28
     MOVE POPUP ayu1 TO 07, i
ENDFOR
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice_4
PARAMETER x, clave
IF LASTKEY() <> 13
     RETURN
ENDIF
FOR i = 1 TO 20
     MOVE POPUP ayu1 BY 0, -1
ENDFOR
FOR i = 1 TO 6
     IF op(i) = x
          EXIT
     ENDIF
ENDFOR
DEFINE WINDOW ayu3 FROM 0, 50 TO  ;
       2, 79 SHADOW
ACTIVATE WINDOW TOP ayu3
DO CASE
     CASE i = 1
          SET ORDER TO codigo
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'O'
          @ 0, 1 SAY  ;
            'Numero Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '99999999'
     CASE i = 2
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO pre_fecemi
               CASE wrk_origen =  ;
                    'OR'
                    SET ORDER TO ord_fchemi
          ENDCASE
          campox = campo1
          wrk_codigo = DATE()
          wrk_tipo = 'D'
          @ 0, 1 SAY  ;
            'Fecha  Desde :' GET  ;
            wrk_codigo
     CASE i = 3
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO st_numser
               CASE wrk_origen =  ;
                    'OR'
                    SET ORDER TO ord_numser
          ENDCASE
          campox = campo1
          wrk_codigo = SPACE(20)
          wrk_tipo = 'S'
          @ 0, 1 SAY  ;
            'Serie  Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '@!'
     CASE i = 4
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO st_numsol
               CASE wrk_origen =  ;
                    'OR'
                    SET ORDER TO ord_numsol
          ENDCASE
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'N'
          @ 0, 1 SAY  ;
            'N£mero Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '99999999'
     CASE i = 5
          DEACTIVATE WINDOW ayu3
          campox = campo1
          wrk_tipo = 'C'
          DEACTIVATE POPUP ayu1
          DEFINE WINDOW ingrcli  ;
                 FROM 14, 20 TO  ;
                 16, 51 SHADOW
          STORE SPACE(20) TO  ;
                cliente
          ACTIVATE WINDOW ingrcli
          ON KEY LABEL ESCAPE DO SALIDA
          @ 00, 00 SAY  ;
            'Cliente :' GET  ;
            cliente PICTURE '@!'
          READ
          IF EMPTY(cliente)
               DO mensa2 WITH  ;
                  '   Espere Un Momento...',  ;
                  'SACA'
               DEACTIVATE WINDOW  ;
                          ingrcli
               DO salida
               IF SROWS() <= 25
                    DO mensa WITH  ;
                       '   Espere Un Momento...',  ;
                       'SACA'
               ELSE
                    DO mensa2  ;
                       WITH  ;
                       '  Espere Un Momento...',  ;
                       'SACA'
                    DEACTIVATE WINDOW  ;
                               error
                    RELEASE WINDOW  ;
                            error
               ENDIF
               RETURN
          ENDIF
          DEACTIVATE WINDOW  ;
                     ingrcli
          IF SROWS() <= 25
               DO mensa WITH  ;
                  '  Espere Un Momento...',  ;
                  'COLO'
          ELSE
               DO mensa2 WITH  ;
                  '  Espere Un Momento...',  ;
                  'COLO'
          ENDIF
     CASE i = 6
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO pre_numord
          ENDCASE
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'R'
          @ 0, 1 SAY  ;
            'N§ Orden   :' GET  ;
            wrk_codigo PICTURE  ;
            '99999999'
ENDCASE
FOR j = 1 TO 10
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          50 TO 2 + j, 79
ENDFOR
FOR j = 1 TO 16
     ZOOM WINDOW ayu3 NORM FROM  ;
          10, 50 - j TO 12, 79 -  ;
          j
ENDFOR
IF wrk_tipo <> 'C'
     READ
ENDIF
IF LASTKEY() == 27
     IF SROWS() <= 25
          DO mensa WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ELSE
          DO mensa2 WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ENDIF
     DEACTIVATE WINDOW error
     RELEASE WINDOW error
     DEACTIVATE POPUP ayu1
     DEACTIVATE WINDOW ayu3
     RETURN
ENDIF
SET NEAR ON
DO CASE
     CASE wrk_tipo = 'O'
          SEEK STR(wrk_codigo, 8)
     CASE wrk_tipo = 'D'
          SEEK dtoc2(wrk_codigo)
     CASE wrk_tipo = 'S'
          SEEK wrk_codigo
     CASE wrk_tipo = 'N'
          SEEK STR(wrk_codigo, 8)
     CASE wrk_tipo = 'C'
          SET NEAR OFF
          DO nombre
          RETURN
     CASE wrk_tipo = 'R'
          SEEK STR(wrk_codigo, 8)
ENDCASE
SET NEAR OFF
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
IF wrk_origen = 'SS'
     browse field cer = " " :H="", uno;
= &campox :H=" Numero   Fecha     Serie       Cod.Cliente  Modelo     Est. Atenci¢n";
in window ayu4 nowait  freeze cer 
ELSE
     browse field cer = " " :H="", uno;
= &campox :H=" N£mero  Fecha     S/Serv.     Serie     O/Rep.   Modelo    Est Atenci¢n";
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
DEACTIVATE POPUP ayu1
DEACTIVATE WINDOW ayu3
ON KEY LABEL ENTER do choice24
BROWSE LAST
ON KEY LABEL ENTER
DEACTIVATE WINDOW ayu4
RETURN
*
PROCEDURE choice24
IF LASTKEY() == 13
     wk_aux = ALLTRIM(numdoc)
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
PROCEDURE nombre
IF  .NOT. USED('st_iclpr')
     SELECT 0
     USE st_iclpr ORDER  ;
         cli_noment
ELSE
     SELECT st_iclpr
     SET ORDER TO cli_noment
ENDIF
SEEK 'C' + ALLTRIM(cliente)
mnomcli = LEFT(noment, 23)
rr = RECNO()
x = 1
SKIP -50
DIMENSION cliente( 100, 2)
DO WHILE (x<=100)
     IF indent = 'C'
          cliente( x, 1) = codent
          cliente( x, 2) = noment
          x = x + 1
     ENDIF
     IF  .NOT. EOF()
          SKIP
     ELSE
          xx = x - 5
          DIMENSION clientes( 1)
          STORE SPACE(10) TO  ;
                clientes( 1)
          EXIT
     ENDIF
ENDDO
xx = x - 1
IF  .NOT. USED('ST_IOREP')
     SELECT 0
     USE ST_IOREP ORDER Codent
ELSE
     SELECT st_iorep
     SET ORDER TO CODENT
ENDIF
GOTO TOP
cli = 1
FOR x = 1 TO xx
     busca_cli = cliente(x,1)
     SEEK (cliente(x,1))
     IF  .NOT. FOUND()
          LOOP
     ENDIF
     DO WHILE codent=cliente(x,1)
          DIMENSION clientes(  ;
                    cli)
          clientes( cli) =  ;
                  LEFT(cliente(x, ;
                  2), 23) + '³' +  ;
                  numdoc + '³' +  ;
                  DTOC(fecemi) +  ;
                  '³' + numsol +  ;
                  '³' +  ;
                  LEFT(numser,  ;
                  10) + '³' +  ;
                  codmod
          cli = cli + 1
          SKIP
          IF (cliente(x,1)) <>  ;
             codent
               EXIT
          ENDIF
     ENDDO
ENDFOR
cl = ASCAN(clientes, mnomcli)
IF cl >= 0
     cl = ASCAN(clientes,  ;
          LEFT(mnomcli, 5))
ELSE
     cl = 50
ENDIF
IF cl > 100 .OR. cl <= 0
     cl = 50
ENDIF
IF SROWS() <= 25
     DO mensa WITH  ;
        '  Espere Un Momento...',  ;
        'SACA'
ELSE
     DO mensa2 WITH  ;
        '  Espere Un Momento...',  ;
        'SACA'
ENDIF
DEFINE WINDOW clientes FROM 06, 0  ;
       TO 19, 75 SHADOW TITLE  ;
       ' CLIENTES '
ACTIVATE WINDOW clientes
ON KEY LABEL Enter do Llama_Cli
@ 0, 0 SAY  ;
  '   C L I E N T E          N£mero    FECHA    S/Serv.    Serie       Modelo    '  ;
  COLOR N/W 
@ 1, 0 GET client DEFAULT  ;
  clientes(cl) SIZE 12, 74 FROM  ;
  clientes COLOR SCHEME 9
READ
ON KEY
DEACTIVATE WINDOW clientes
DEACTIVATE POPUP ayu1
RETURN
*
PROCEDURE salida
DEACTIVATE WINDOW clientes
DEACTIVATE POPUP ayu1
DO mensa2 WITH  ;
   '  Espere Un Momento...',  ;
   'SACA'
ON KEY
*
PROCEDURE llama_cli
IF LASTKEY() == 13
     wk_aux = ALLTRIM(SUBSTR(client,  ;
              25, 8))
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
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
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens COLOR GR+/N* 
= INKEY(1)
SET CURSOR ON
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
