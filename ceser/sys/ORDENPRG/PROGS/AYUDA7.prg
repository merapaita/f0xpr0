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
DIMENSION op( 4)
op( 1) =  ;
  ' Ordenados por N£mero  '
op( 2) =  ;
  ' Ordenados por Fecha   '
op( 3) =  ;
  ' Ordenados por Serie   '
op( 4) =  ;
  ' Ordenados por Cliente '
DEFINE POPUP ayu1 FROM 0, 0  ;
       SHADOW COLOR SCHEME 8
FOR i = 1 TO 4
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
     MOVE POPUP ayu1 TO 08, i
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
FOR i = 1 TO 19
     MOVE POPUP ayu1 BY 0, -1
ENDFOR
FOR i = 1 TO 4
     IF op(i) = x
          EXIT
     ENDIF
ENDFOR
IF i <> 4
     DEFINE WINDOW ayu3 FROM 9,  ;
            30 TO 11, 75 SHADOW
     ACTIVATE WINDOW TOP ayu3
ENDIF
DO CASE
     CASE i = 1
          SET ORDER TO codigo
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'O'
          @ 0, 1 SAY  ;
            'N£mero Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '99999999'
     CASE i = 2
          SET ORDER TO sol_fchemi
          campox = campo1
          wrk_codigo = DATE()
          wrk_tipo = 'D'
          @ 0, 1 SAY  ;
            'Fecha  Desde :' GET  ;
            wrk_codigo
     CASE i = 3
          SET ORDER TO sol_serie
          campox = campo1
          wrk_codigo = SPACE(20)
          wrk_tipo = 'S'
          @ 0, 1 SAY  ;
            'Serie  Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '@!'
     CASE i = 4
          DEACTIVATE WINDOW ayu3
          campox = campo1
          wrk_tipo = 'N'
          DEACTIVATE POPUP ayu1
          DEFINE WINDOW ingrcli  ;
                 FROM 14, 20 TO  ;
                 16, 51 SHADOW
          STORE SPACE(20) TO  ;
                cliente
          ACTIVATE WINDOW ingrcli
          @ 00, 00 SAY  ;
            'Cliente :' GET  ;
            cliente PICTURE '@!'
          READ
          IF EMPTY(cliente) .OR.  ;
             LASTKEY() = 27
               DEACTIVATE WINDOW  ;
                          ingrcli
               DO mensa2 WITH  ;
                  '  Espere Un Momento...',  ;
                  'SACA'
               DEACTIVATE POPUP  ;
                          ayu1
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
ENDCASE
IF i <> 4
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
     USE
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
          SET NEAR OFF
          DO nombre
          RETURN
ENDCASE
SET NEAR OFF
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
IF wrk_origen = 'SS'
     IF i <> 4
          browse field cer = " " :H="",;
uno = &campox :H="N£mero    Fecha    CodClien    Modelo            Serie";
 in window ayu4 nowait  freeze cer;
       
     ELSE
          campox = 'noment+" "+codent+" "+nomcal'
          browse field cer = " " :H="",;
uno = &campox :H=" Nombre                           C¢digo    Direcci¢n ";
in window ayu4 nowait freeze cer
     ENDIF
ELSE
     IF i <> 4
          IF wrk_origen = 'PR'
               campox = 'numdoc+" "+dtoc(fecemi)+" "+numord+" "+codent+" "+codmod+" "+numser'
               browse field cer = " ":H="",;
uno = &campox :H="N£mero    Fecha    NøOrden   Codent   Modelo            Serie            Indest";
in window ayu4 nowait freeze cer
          ELSE
               browse field cer = " ";
:H="", uno = &campox :H=" N£mero  Fecha     Nø Sol.  NøSerie       CodClie Modelo       Est";
 in window ayu4 nowait  freeze cer;
       
          ENDIF
     ELSE
          campox = 'noment+" "+codent+" "+nomcal'
          browse field cer = " " :H="",;
uno = &campox :H=" Nombre                           C¢digo    Direcci¢n ";
in window ayu4 nowait freeze cer
     ENDIF
ENDIF
ACTIVATE WINDOW TOP ayu4
ventana = SUBSTR(DBF(),  ;
          LEN(DBF()) - 11, 8)
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          01 TO 7 + j, 75
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
IF i = 4
     IF LASTKEY() == 13
          ON KEY LABEL ENTER do choice25
          wk_aux = ALLTRIM(noment)
          codig = st_iclpr.codent
          SELECT st_iclpr
          SET ORDER TO codent
          IF wrk_origen = 'SS'
               SELECT st_isrep
               SET ORDER TO codent
               SET RELATION TO codent;
INTO st_iclpr
          ELSE
               IF wrk_origen =  ;
                  'PR'
                    USE IN 3  ;
                        st_iorep  ;
                        ALIAS  ;
                        st_iorep
                    SET ORDER TO codent
                    SEEK codig
                    codig = st_iorep.numdoc
                    SELECT st_ispre
                    SET ORDER TO pre_codent
               ELSE
                    SELECT st_iorep
                    SET ORDER TO codent
                    SET RELATION TO codent;
INTO st_iclpr
               ENDIF
          ENDIF
          SEEK codig
          campox = 'numdoc+" "+dtoc(fecemi)+" "+st_iclpr->noment+" "+codmod+numser'
          browse field cer = "" :H="",;
uno = &campox :H="NUMERO   FECHAEM  CLIENTE                        MODELO          SERIE";
in window ayu4 nowait freeze cer
          ACTIVATE WINDOW TOP  ;
                   ayu4
          KEYBOARD 'DOWN'
          WAIT ''
          IF LASTKEY() == 27
               USE
          ENDIF
     ENDIF
ELSE
     IF LASTKEY() == 13
          wk_aux = ALLTRIM(numdoc)
          IF LEN(wk_aux) < 8
               wk_aux = wk_aux +  ;
                        CHR(13)
          ENDIF
          KEYBOARD CHR(27) +  ;
                   wk_aux
     ENDIF
ENDIF
RETURN
*
PROCEDURE choice25
IF LASTKEY() == 13
     SELECT 2
     wk_aux = ALLTRIM(numdoc)
     USE
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
mnomcli = noment
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
IF  .NOT. USED('ST_ISREP')
     SELECT 0
     USE ST_ISREP ORDER Codent
ELSE
     SELECT st_isrep
     varfil = FILTER()
     SET FILTER TO
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
                  2), 33) + '³' +  ;
                  numdoc + '³' +  ;
                  DTOC(fecemi) +  ;
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
  '   C l i e n t e               N£mero      Fecha      Serie       Modelo  '  ;
  COLOR N/W 
@ 1, 0 GET client DEFAULT  ;
  clientes(cl) SIZE 12, 74 FROM  ;
  clientes COLOR SCHEME 9
READ
ON KEY
DEACTIVATE WINDOW clientes
DEACTIVATE POPUP ayu1
SELECT st_isrep
set filter to &varfil
RETURN
*
PROCEDURE llama_cli
IF LASTKEY() == 13
     wk_aux = ALLTRIM(SUBSTR(client,  ;
              32, 8))
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
