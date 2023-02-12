*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ind_prg = '<PORM0306>'
@ 24, 69 SAY ind_prg
wrk_progra = PROGRAM()
DO crea_win
ON KEY LABEL F6 DO AYUDA01
ON KEY LABEL F10 DO FCINCO
SET COLOR TO &COLOR3
@ 2, 1 SAY DATE()
set color to &color1
DO saycenter WITH 1, 'CONSULTA'
DO saycenter WITH 2,  ;
   ' ORDENES POR MODELOS'
@ 3, 2 CLEAR TO 7, 77
@ 3, 2 TO 7, 77
@ 4, 5 SAY 'Codigo de Marca   :'
@ 5, 5 SAY 'Codigo de Modelo  :'
@ 6, 5 SAY  ;
  'Fecha Desde       :                          Fecha Hasta :'
ppas = .T.
DO WHILE ppas
     @ 4, 30 SAY SPACE(30)
     @ 5, 41 SAY SPACE(30)
     @ 4, 30 SAY  ;
       '[ENTER] Todos las Marcas'
     @ 3, 2 TO 7, 77
     DO esc_modo WITH 'I'
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'INT'
     DO esc_indica WITH 2, 'RAC',  ;
        'BBB', 'IGN', 'ESC'
     wk_codmar = SPACE(4)
     wk_codmod = SPACE(15)
     STORE CTOD('') TO wk_fecin1,  ;
           wk_fecin2
     efecin = 1
     @ 4, 25 GET wk_codmar  ;
       PICTURE '!!!!' VALID  ;
       valtab2('MARC',wk_codmar, ;
       30,30) WHEN colocaf6()
     @ 5, 25 GET wk_codmod  ;
       PICTURE '!!!!!!!!!!!!!!!'  ;
       VALID codmod2(wk_codmod,41, ;
       30) WHEN colocaf6()
     @ 6, 25 GET wk_fecin1 VALID  ;
       valfec1(wk_fecin1)
     @ 6, 64 GET wk_fecin2 VALID  ;
       valfec2(wk_fecin1, ;
       wk_fecin2)
     SET CURSOR ON
     READ
     SET CURSOR OFF
     DO CASE
          CASE LASTKEY() == 27  ;
               .AND. efecin == 1
               ppas = .F.
               LOOP
          CASE LASTKEY() == 27  ;
               .AND. efecin == 2
               LOOP
     ENDCASE
     IF wk_codmar <> SPACE(4)
          USE SHARED st_iorep  ;
              ORDER ORD_CODMAR
          wk_clave1 = wk_codmar +  ;
                      wk_codmod +  ;
                      dtoc2(wk_fecin1)
          wk_clave2 = wk_codmar +  ;
                      wk_codmod +  ;
                      dtoc2(wk_fecin2)
     ELSE
          USE SHARED st_iorep  ;
              ORDER ORD_FCHEMI
          wk_clave1 = dtoc2(wk_fecin1)
          wk_clave2 = dtoc2(wk_fecin2)
     ENDIF
     SET NEAR ON
     SEEK '&wk_clave1'
     SET NEAR OFF
     wk_numero = SPACE(8)
     campox = 'numdoc+"  "+dtoc(fecemi)+"  "+codemi+"  "+codent+"  "+codmar+"  "+codmod+"   "+subst(indest,1,2)'
     define window ayu4 from 6,6 to 7,70;
shadow color &color5
     IF ISCOLOR()
          wk_color = color5 +  ;
                     ',,,,,' +  ;
                     SUBSTR(color5,  ;
                     AT(',',  ;
                     color5) +  ;
                     1)
     ELSE
          wk_color = color5
     ENDIF
     browse field cer = " " :H="", uno;
= &campox :H="  Numero  Fecha     Emisor  Entidad  Marca Modelo           Est";
 key wk_clave1, wk_clave2 in window ayu4;
nowait color &wk_color freeze cer
     ACTIVATE WINDOW TOP ayu4
     FOR j = 1 TO 08
          ZOOM WINDOW ayu4 NORM  ;
               FROM 6, 6 TO 7 + j,  ;
               70
          ZOOM WINDOW st_iorep  ;
               NORM FROM -1, -4  ;
               TO j, 70
     ENDFOR
     ON KEY LABEL ENTER do choice2
     BROWSE LAST KEY wk_clave1,  ;
            wk_clave2
     ON KEY LABEL ENTER
     DEACTIVATE WINDOW ayu4
     IF wk_numero <> SPACE(8)
          DIMENSION orden( 52)
          STORE FOPEN('orden.txt')  ;
                TO file_handl
          FOR i = 1 TO 52
               orden( i) =  ;
                    FREAD(file_handl,  ;
                    102)
          ENDFOR
          = FCLOSE(file_handl)
          DIMENSION wk_codsin(  ;
                    20)
          DIMENSION wk_acceso(  ;
                    20)
          DIMENSION wk_observ(  ;
                    06)
          wk_fecemi = fecemi
          wk_emisor = codemi
          wk_codcli = VAL(codent)
          wk_indori = indori
          wk_indest = indest
          wk_codmar = codmar
          wk_codmod = codmod
          wk_numser = numser
          FOR i = 1 TO 20
               wk_codsin = SPACE(47)
               wk_acceso( i) =  ;
                        MLINE(desace,  ;
                        i)
               IF i <= 6
                    wk_observ( i) =  ;
                             MLINE(observ,  ;
                             i)
               ENDIF
          ENDFOR
          wk_aux = wk_codmar +  ;
                   wk_codmod
          USE SHARED or_imode  ;
              ORDER CODIGO
          SEEK '&wk_aux'
          wk_aux = codcla
          SELECT 2
          USE SHARED or_isint  ;
              ORDER CODIGO
          SELECT 1
          USE SHARED or_isore  ;
              ORDER CODIGO
          SEEK '&wk_numero'
          i = 1
          IF FOUND()
               DO WHILE  .NOT.  ;
                  EOF() .AND.  ;
                  numdoc== ;
                  wk_numero
                    wk_aux2 = wk_aux +  ;
                              codsin
                    SELECT 2
                    SEEK '&wk_aux2'
                    wk_codsin( i) =  ;
                             SUBSTR(dessin,  ;
                             1,  ;
                             47)
                    i = i + 1
                    SELECT 1
                    SKIP
               ENDDO
          ENDIF
          CLOSE DATABASES
          USE ge_tab0 ORDER  ;
              codigo
          wk_aux = 'EMIS' +  ;
                   wk_emisor
          SEEK '&wk_aux'
          wk_nomemi = tab_destab
          wk_aux = 'ESTA' +  ;
                   wk_indest
          SEEK '&wk_aux'
          wk_estado = tab_descri
          USE
          DO coloca WITH 02, 88,  ;
             ' '+wk_numero
          DO coloca WITH 12, 59,  ;
             DTOC(wk_fecemi)
          DO coloca WITH 12, 73,  ;
             TIME()
          DO coloca WITH 14, 68,  ;
             wk_emisor
          DO coloca WITH 14, 73,  ;
             SUBSTR(wk_nomemi, 1,  ;
             24)
          DO coloca WITH 03, 06,  ;
             SUBSTR(wk_estado, 1,  ;
             15)
          DO col_bk1a
          DO col_bk2
          DO col_bk3
          DO col_bk4
          DO col_bk5
          DO col_bk6
          DO col_bk7
          DO col_bk8
          lin = 15
          anc = 75
          des = 1
          com = 1
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          set color to &color3
          FOR i = des TO (lin +  ;
              des - 1)
               @ i - des, 0 SAY  ;
                 SUBSTR(orden(i),  ;
                 com, anc)
          ENDFOR
          set color to &color1
          ppal = .T.
          DO WHILE ppal
               wk_inkey = 0
               DO WHILE  .NOT.  ;
                  (STR(wk_inkey,  ;
                  2)$ ;
                  ' 5,24, 4,19,18, 3, 1, 6,27' ;
                  )
                    wk_inkey = INKEY(0)
               ENDDO
               DO CASE
                    CASE wk_inkey ==  ;
                         27
                         ppal = .F.
                         LOOP
                    OTHERWISE
                         DO mueve2  ;
                            WITH  ;
                            wk_inkey
               ENDCASE
          ENDDO
     ENDIF
ENDDO
ON KEY LABEL F6
ON KEY LABEL F10
DO saca_win
@ 24, 69 SAY SPACE(15)
RETURN
*
PROCEDURE ayuda01
ON KEY LABEL F6
IF ROW() == 4
     USE ge_tab0 ORDER codigo
     SET FILTER TO tab_codpre == 'MARC'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE MARCAS'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 5
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
IF ROW() == 8
     USE ge_tab0 ORDER codigo
     SET FILTER TO tab_codpre == 'CLAS'
     GOTO TOP
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'AYUDA DE CLASIFICACION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     USE
ENDIF
IF ROW() == 10
     USE SHARED st_iclpr ORDER  ;
         CODIGO
     SET FILTER TO indent == 'P'
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE PROVEEDORES'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<9,codent+chr(13),codent)'
     USE
ENDIF
ON KEY LABEL F6 do ayuda01
RETURN
*
FUNCTION valfec1
PARAMETER fec1
IF fec1 == CTOD('')
     KEYBOARD '010190'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION valfec2
PARAMETER fec1, fec2
IF fec2 == CTOD('')
     IF fec1 > DATE()
          KEYBOARD DTOC(fec1)
     ELSE
          KEYBOARD DTOC(DATE())
     ENDIF
     RETURN .F.
ENDIF
IF fec2 < fec1
     DO error WITH  ;
        '** Error Fecha no debe ser Menor. **'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION valtab2
PARAMETER v1, v2, v3, v4
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
IF v2 == SPACE(4)
     set color to &color3
     @ ROW(), v3 SAY  ;
       'TODAS LAS MARCAS Y MODELOS'
     set color to &color1
     DO sacaf6
     KEYBOARD CHR(13)
     RETURN .T.
ENDIF
wk_aux = valtab(v1,v2,v3,v4)
IF wk_aux
     DO sacaf6
     RETURN .T.
ELSE
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION codmod2
PARAMETER v1, v2, v3
IF v1 == SPACE(15) .AND.  ;
   wk_codmar == SPACE(4)
     RETURN .T.
ENDIF
wk_aux = codmod(v1,v2,v3)
IF wk_aux
     DO sacaf6
     RETURN .T.
ELSE
     RETURN .F.
ENDIF
RETURN .T.
*
PROCEDURE choice2
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
