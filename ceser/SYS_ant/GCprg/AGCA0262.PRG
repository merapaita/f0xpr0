*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
DEFINE WINDOW a0 FROM 00, 00 TO  ;
       20, 79 IN screen DOUBLE
ACTIVATE WINDOW a0
DO p_prestab WITH 'MANTENCION',  ;
   'ENTIDADES', 'SELECCIONA'
tit_prg = 'MANTENCION'
CLOSE DATABASES
SELECT 1
USE SHARED gc_cli00 ORDER codigo
SELECT 2
USE SHARED ge_tab0 ORDER codigo
SELECT 3
USE SHARED gc_vnd00 ORDER codigo
@ 02, 63 SAY PROGRAM()
@ 03, 02 CLEAR TO 05, 77
@ 03, 02 TO 05, 77
@ 04, 03 SAY  ;
  'Indicador Entidad :      C¢digo Entidad :'
STORE SPACE(1) TO w_busca
STORE SPACE(4) TO w_var
ppas = .T.
DO WHILE ppas
     ACTIVATE WINDOW a0
     @ 06, 02 CLEAR TO 17, 77
     @ 04, 57 SAY SPACE(17)
     @ 03, 02 TO 05, 77
     ind_modo = 'I'
     w_calle = SPACE(30)
     STORE SPACE(05) TO w_numero
     STORE SPACE(11) TO w_codcli
     STORE SPACE(04) TO w_lote,  ;
           w_distri, w_provin,  ;
           w_clasif, w_eciv,  ;
           w_nac, w_codent
     STORE 0 TO w_telefo,  ;
           w_descue, w_edad
     STORE DATE() TO w_fing
     STORE SPACE(01) TO w_estado
     STORE SPACE(50) TO w_razsoc
     STORE SPACE(30) TO w_observ
     @ 04, 23 GET w_codent  ;
       PICTURE '@!' VALID  ;
       oovalid(1,'ENTI',w_codent)  ;
       WHEN oowhen(1)
     @ 04, 45 GET w_codcli  ;
       PICTURE '@!' VALID  ;
       oovalid(2,w_codent, ;
       w_codcli) WHEN oowhen(2)
     READ
     IF LASTKEY() = 27
          ppas = .F.
          LOOP
     ENDIF
     @ 06, 03 SAY  ;
       'Nombre o Raz¢n Social :'
     @ 07, 03 SAY  ;
       'Direcci¢n             :'
     IF w_codent = 'P   ' .OR.  ;
        w_codent = 'C   ' .OR.  ;
        w_codent = 'I   '
          @ 05, 02 TO 15, 77
          @ 05, 02 SAY 'Ã'
          @ 05, 77 SAY '´'
          @ 07, 49 SAY 'N£mero:'
          @ 07, 65 SAY 'Lote:'
          @ 08, 03 SAY  ;
            'Distrito              :'
          @ 09, 03 SAY  ;
            'Ciudad                :'
          @ 10, 03 SAY  ;
            'Tel‚fono              :'
          @ 11, 03 SAY  ;
            'Estado                :'
          @ 12, 03 SAY  ;
            'C¢digo Clasificaci¢n  :'
          @ 13, 03 SAY  ;
            '% Descuento Otorgado  :'
          @ 14, 03 SAY  ;
            'Observaci¢n           :'
     ELSE
          @ 05, 02 TO 17, 77
          @ 05, 02 SAY 'Ã'
          @ 05, 77 SAY '´'
          @ 08, 03 SAY  ;
            'Distrito              :'
          @ 09, 03 SAY  ;
            'Ciudad                :'
          @ 10, 03 SAY  ;
            'Nacionalidad          :'
          @ 11, 03 SAY  ;
            'Tel‚fono              :'
          @ 12, 03 SAY  ;
            'C¢digo Clasificaci¢n  :'
          @ 13, 03 SAY  ;
            'Estado Civil          :'
          @ 14, 03 SAY  ;
            'Edad                  :'
          @ 15, 03 SAY  ;
            'Fecha de Ingreso      :'
          @ 16, 03 SAY  ;
            'Observaci¢n           :'
     ENDIF
     @ 06, 27 SAY w_razsoc  ;
       PICTURE '@!'
     @ 07, 27 SAY w_calle PICTURE  ;
       '@!'
     IF w_codent = 'P   ' .OR.  ;
        w_codent = 'C   ' .OR.  ;
        w_codent = 'I   '
          @ 07, 58 SAY w_numero  ;
            PICTURE '@!'
          @ 07, 71 SAY w_lote  ;
            PICTURE '@!'
          @ 08, 27 SAY w_distri  ;
            PICTURE '!!!!'
          @ 09, 27 SAY w_provin  ;
            PICTURE '!!!!'
          @ 10, 27 SAY w_telefo  ;
            PICTURE '99999999'
          @ 11, 27 SAY w_estado  ;
            PICTURE '!'
          @ 12, 27 SAY w_clasif  ;
            PICTURE '!!!!'
          @ 13, 27 SAY w_descue  ;
            PICTURE '99.99'
          @ 14, 27 SAY w_observ  ;
            PICTURE '@!'
     ELSE
          @ 08, 27 SAY w_distri  ;
            PICTURE '!!!!'
          @ 09, 27 SAY w_provin  ;
            PICTURE '!!!!'
          @ 10, 27 SAY w_nac  ;
            PICTURE '!!!!'
          @ 11, 27 SAY w_telefo  ;
            PICTURE '99999999'
          @ 12, 27 SAY w_clasif  ;
            PICTURE '!!!!'
          @ 13, 27 SAY w_eciv  ;
            PICTURE '!!!!'
          @ 14, 27 SAY w_edad  ;
            PICTURE '99'
          @ 15, 27 SAY w_fing
          @ 16, 27 SAY w_observ  ;
            PICTURE '@!'
     ENDIF
     SELECT ge_tab0
     SEEK 'DIST' + w_distri
     @ 08, 37 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     SEEK 'PROV' + w_provin
     @ 09, 37 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     SEEK 'CATC' + w_clasif
     @ 12, 37 SAY  ;
       SUBSTR(tab_destab, 1, 30)
     IF w_codent = 'P   ' .OR.  ;
        w_codent = 'C   ' .OR.  ;
        w_codent = 'I   '
          SEEK 'ESTA' + w_estado
          @ 11, 37 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            30)
     ELSE
          SEEK 'NACI' + w_nac
          @ 10, 37 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            30)
          SEEK 'ESCI' + w_eciv
          @ 13, 37 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            30)
     ENDIF
     key = INKEY()
     IF key <> 27 .AND. key <> 13
          DO p_footer WITH  ;
             '100100000000100000001',  ;
             2
     ENDIF
     DO WHILE key<>27 .AND. key<> ;
        13 .AND. key<>-3
          key = INKEY(0)
     ENDDO
     IF key == 27
          LOOP
     ENDIF
     DO CASE
          CASE key == 13
               ACTIVATE WINDOW a0
               @ 06, 27 GET  ;
                 w_razsoc PICTURE  ;
                 '@!' VALID   ;
                 .NOT.  ;
                 EMPTY(w_razsoc)  ;
                 WHEN oowhen(3)
               @ 07, 27 GET  ;
                 w_calle PICTURE  ;
                 '@!' VALID   ;
                 .NOT.  ;
                 EMPTY(w_calle)  ;
                 WHEN oowhen(3)
               IF w_codent =  ;
                  'P   ' .OR.  ;
                  w_codent =  ;
                  'C   ' .OR.  ;
                  w_codent =  ;
                  'I   '
                    @ 07, 58 GET  ;
                      w_numero  ;
                      PICTURE  ;
                      '99999'
                    @ 07, 71 GET  ;
                      w_lote  ;
                      PICTURE  ;
                      '@!'
                    @ 08, 27 GET  ;
                      w_distri  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'DIST', ;
                      w_distri)  ;
                      WHEN  ;
                      oowhen(4)
                    @ 09, 27 GET  ;
                      w_provin  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'PROV', ;
                      w_provin)  ;
                      WHEN  ;
                      oowhen(5)
                    @ 10, 27 GET  ;
                      w_telefo  ;
                      PICTURE  ;
                      '99999999'  ;
                      WHEN  ;
                      oowhen(3)
                    @ 11, 27 GET  ;
                      w_estado  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'ESTA', ;
                      w_estado)  ;
                      WHEN  ;
                      oowhen(7)
                    @ 12, 27 GET  ;
                      w_clasif  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'CATC', ;
                      w_clasif)  ;
                      WHEN  ;
                      oowhen(6)
                    @ 13, 27 GET  ;
                      w_descue  ;
                      PICTURE  ;
                      '99.99'  ;
                      VALID  ;
                      w_descue >=  ;
                      0 WHEN  ;
                      oowhen(3)
                    @ 14, 27 GET  ;
                      w_observ  ;
                      PICTURE  ;
                      '@!'
                    READ
               ELSE
                    @ 08, 27 GET  ;
                      w_distri  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'DIST', ;
                      w_distri)  ;
                      WHEN  ;
                      oowhen(4)
                    @ 09, 27 GET  ;
                      w_provin  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'PROV', ;
                      w_provin)  ;
                      WHEN  ;
                      oowhen(5)
                    @ 10, 27 GET  ;
                      w_nac  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'NACI', ;
                      w_nac) WHEN  ;
                      oowhen(8)
                    @ 11, 27 GET  ;
                      w_telefo  ;
                      PICTURE  ;
                      '99999999'  ;
                      WHEN  ;
                      oowhen(3)
                    @ 12, 27 GET  ;
                      w_clasif  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'CATC', ;
                      w_clasif)  ;
                      WHEN  ;
                      oowhen(6)
                    @ 13, 27 GET  ;
                      w_eciv  ;
                      PICTURE  ;
                      '!!!!'  ;
                      VALID  ;
                      oovalid(1, ;
                      'ESCI', ;
                      w_eciv)  ;
                      WHEN  ;
                      oowhen(9)
                    @ 14, 27 GET  ;
                      w_edad  ;
                      PICTURE  ;
                      '99' VALID  ;
                      w_edad > 0  ;
                      .AND.  ;
                      w_edad <  ;
                      100
                    @ 15, 27 GET  ;
                      w_fing  ;
                      PICTURE  ;
                      '@D'
                    @ 16, 27 GET  ;
                      w_observ  ;
                      PICTURE  ;
                      '@!'
                    READ
               ENDIF
          CASE key == -3
               rpt = f_yesno( ;
                     '  Desea Anular?  ' ;
                     )
               IF rpt
                    IF w_codent =  ;
                       'P   '  ;
                       .OR.  ;
                       w_codent =  ;
                       'C   '  ;
                       .OR.  ;
                       w_codent =  ;
                       'I   '
                         SELECT 20
                         USE SHARED  ;
                             gc_hve00  ;
                             ORDER  ;
                             hve_ticofe
                         SEEK w_codent +  ;
                              w_codcli
                         IF FOUND()
                              SCAN  ;
                               WHILE  ;
                               hve_tipent =  ;
                               w_codent  ;
                               .AND.  ;
                               hve_codent =  ;
                               w_codcli  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   w_fecfin = hve_fecdoc
                              ENDSCAN
                              IF w_fecfin >  ;
                                 DATE() -  ;
                                 rge_vicodi
                                   DO p_mensaje WITH '** Tiene datos hist¢ricos del '+DTOC(w_fecfin)+' **'
                                   LOOP
                              ENDIF
                         ENDIF
                         SELECT gc_cli00
                         SEEK ALLTRIM(w_codent) +  ;
                              w_codcli
                         IF FOUND()
                              DO rbloquea
                              DELETE
                              UNLOCK
                              LOOP
                         ENDIF
                    ELSE
                         SELECT 20
                         USE SHARED  ;
                             gc_hip00  ;
                             ORDER  ;
                             hip_ticofe
                         SEEK w_codent +  ;
                              w_codcli
                         IF FOUND()
                              SCAN  ;
                               WHILE  ;
                               hip_tipent =  ;
                               w_codent  ;
                               .AND.  ;
                               SUBSTR(hip_codent,  ;
                               1,  ;
                               5) =  ;
                               w_codcli  ;
                               .AND.   ;
                               .NOT.  ;
                               EOF()
                                   w_fecfin = hip_fecdoc
                              ENDSCAN
                              IF w_fecfin >  ;
                                 DATE() -  ;
                                 rge_vicodi
                                   DO p_mensaje WITH '** Tiene datos hist¢ricos del '+DTOC(w_fecfin)+' **'
                                   LOOP
                              ENDIF
                         ENDIF
                         SELECT gc_vnd00
                         SEEK ALLTRIM(w_codent) +  ;
                              w_codcli
                         IF FOUND()
                              DO rbloquea
                              DELETE
                              UNLOCK
                              LOOP
                         ENDIF
                    ENDIF
                    DO p_mensaje  ;
                       WITH  ;
                       '** Registro Borrado **'
               ENDIF
     ENDCASE
     DO p_footer WITH  ;
        '110001000000000000001',  ;
        2
     keyx = LASTKEY()
     DO WHILE keyx<>27 .AND. keyx<>- ;
        1 .AND. keyx<>-6
          keyx = INKEY(0)
     ENDDO
     IF keyx = 27
          LOOP
     ENDIF
     IF keyx = -1 .OR. keyx = -6
          IF w_codent = 'P   '  ;
             .OR. w_codent =  ;
             'C   ' .OR. w_codent =  ;
             'I   '
               SELECT gc_cli00
               SET ORDER TO codigo
               SEEK ALLTRIM(w_codent) +  ;
                    w_codcli
               IF  .NOT. FOUND()
                    APPEND BLANK
                    DO rbloquea
                    REPLACE cli_tpper  ;
                            WITH  ;
                            w_codent,  ;
                            cli_codigo  ;
                            WITH  ;
                            w_codcli,  ;
                            cli_feccre  ;
                            WITH  ;
                            DATE()
               ELSE
                    DO rbloquea
               ENDIF
               REPLACE cli_razsoc  ;
                       WITH  ;
                       w_razsoc,  ;
                       cli_clasif  ;
                       WITH  ;
                       w_clasif,  ;
                       cli_lote  ;
                       WITH  ;
                       w_lote,  ;
                       cli_numero  ;
                       WITH  ;
                       w_numero,  ;
                       cli_calle  ;
                       WITH  ;
                       w_calle,  ;
                       cli_distri  ;
                       WITH  ;
                       w_distri,  ;
                       cli_provin  ;
                       WITH  ;
                       w_provin
               REPLACE cli_dpto  ;
                       WITH  ;
                       w_provin,  ;
                       cli_telefo  ;
                       WITH  ;
                       w_telefo,  ;
                       cli_observ  ;
                       WITH  ;
                       w_observ,  ;
                       cli_estado  ;
                       WITH  ;
                       w_estado,  ;
                       cli_descue  ;
                       WITH  ;
                       w_descue
               REPLACE cli_usuari  ;
                       WITH clave,  ;
                       cli_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       cli_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ELSE
               SELECT gc_vnd00
               SET ORDER TO codigo
               SEEK ALLTRIM(w_codent) +  ;
                    w_codcli
               IF  .NOT. FOUND()
                    APPEND BLANK
                    DO rbloquea
                    REPLACE vnd_tpper  ;
                            WITH  ;
                            w_codent,  ;
                            vnd_code  ;
                            WITH  ;
                            w_codcli
               ELSE
                    DO rbloquea
               ENDIF
               REPLACE vnd_nombre  ;
                       WITH  ;
                       w_razsoc,  ;
                       vnd_edad  ;
                       WITH  ;
                       STR(w_edad,  ;
                       2),  ;
                       vnd_direc  ;
                       WITH  ;
                       w_calle,  ;
                       vnd_dist  ;
                       WITH  ;
                       w_distri,  ;
                       vnd_prov  ;
                       WITH  ;
                       w_provin,  ;
                       vnd_nac  ;
                       WITH w_nac,  ;
                       vnd_eciv  ;
                       WITH  ;
                       w_eciv
               REPLACE vnd_fing  ;
                       WITH  ;
                       w_fing,  ;
                       vnd_tipo  ;
                       WITH  ;
                       w_clasif,  ;
                       vnd_observ  ;
                       WITH  ;
                       w_observ
               REPLACE vnd_numtel  ;
                       WITH  ;
                       STR(w_telefo,  ;
                       8)
               REPLACE vnd_usuari  ;
                       WITH clave,  ;
                       vnd_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       vnd_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
          IF keyx = -6
               IF w_codent =  ;
                  'P   ' .OR.  ;
                  w_codent =  ;
                  'C   ' .OR.  ;
                  w_codent =  ;
                  'I   '
                    REPORT FORMAT  ;
                           agca0262  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           agca0261  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ENDIF
     ENDIF
ENDDO
RELEASE WINDOW a0
CLOSE DATABASES
ON KEY
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 1
          w_busca = 'ENTI'
          w_var = 'W_CODENT'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
     CASE cwhen = 2
          w_busca = w_codent
          IF w_codent = 'P   '  ;
             .OR. w_codent =  ;
             'C   ' .OR. w_codent =  ;
             'I   '
               w_codcli = SUBSTR(w_codcli,  ;
                          1, 11)
          ELSE
               w_codcli = SUBSTR(w_codcli,  ;
                          1, 5)
          ENDIF
          w_var = 'w_codcli'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL F6 do entidad with;
w_busca,w_var
     CASE cwhen = 3
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
     CASE cwhen = 4
          w_busca = 'DIST'
          w_var = 'w_distri'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
     CASE cwhen = 5
          w_busca = 'PROV'
          w_var = 'w_provin'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
     CASE cwhen = 6
          w_busca = 'CATC'
          w_var = 'w_clasif'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
     CASE cwhen = 7
          w_busca = 'ESTA'
          w_var = 'w_estado'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
     CASE cwhen = 8
          w_busca = 'NACI'
          w_var = 'w_nac'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
     CASE cwhen = 9
          w_busca = 'ESCI'
          w_var = 'w_eciv'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ON KEY LABEL f6 do busca with;
w_busca,w_Var
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid, tipo, variable
DO CASE
     CASE cvalid = 1
          SELECT ge_tab0
          SEEK tipo + variable
          IF FOUND()
               IF ROW() <> 4
                    @ ROW(), 37  ;
                      SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 30)
               ENDIF
          ELSE
               DO p_mensaje WITH  ;
                  'CODIGO NO EXISTE'
               RETURN .F.
          ENDIF
     CASE cvalid = 2
          IF EMPTY(w_codcli)
               DO p_mensaje WITH  ;
                  'NO SE ACEPTAN BLANCOS'
               RETURN .F.
          ENDIF
          IF w_codent = 'C   '  ;
             .OR. w_codent =  ;
             'P   ' .OR. w_codent =  ;
             'I   '
               SELECT gc_cli00
               SET ORDER TO codigo
               SEEK ALLTRIM(w_codent) +  ;
                    w_codcli
               IF FOUND()
                    @ 04, 57 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 17)
                    w_razsoc = cli_razsoc
                    w_calle = cli_calle
                    w_numero = cli_numero
                    w_lote = cli_lote
                    w_distri = cli_distri
                    w_provin = cli_provin
                    w_telefo = cli_telefo
                    w_estado = cli_estado
                    w_clasif = cli_clasif
                    w_descue = cli_descue
                    w_observ = cli_observ
               ELSE
                    KEYBOARD '{ENTER}'  ;
                             PLAIN
               ENDIF
          ELSE
               SELECT gc_vnd00
               SET ORDER TO codigo
               SEEK ALLTRIM(w_codent) +  ;
                    w_codcli
               IF FOUND()
                    @ 04, 57 SAY  ;
                      SUBSTR(vnd_nombre,  ;
                      1, 17)
                    w_razsoc = vnd_nombre
                    w_calle = vnd_direc
                    w_distri = SUBSTR(vnd_dist,  ;
                               1,  ;
                               4)
                    w_provin = SUBSTR(vnd_prov,  ;
                               1,  ;
                               4)
                    w_nac = SUBSTR(vnd_nac,  ;
                            1,  ;
                            4)
                    w_telefo = VAL(vnd_numtel)
                    w_clasif = SUBSTR(vnd_tipo,  ;
                               1,  ;
                               4)
                    w_eciv = SUBSTR(vnd_eciv,  ;
                             1,  ;
                             4)
                    w_edad = VAL(vnd_edad)
                    w_fing = vnd_fing
                    w_observ = SUBSTR(vnd_observ,  ;
                               1,  ;
                               30)
               ELSE
                    KEYBOARD '{ENTER}'  ;
                             PLAIN
               ENDIF
          ENDIF
ENDCASE
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
