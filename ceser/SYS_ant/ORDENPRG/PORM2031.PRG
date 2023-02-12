*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
IF SUBSTR(users, 1, 3) <> 'GER'  ;
   .AND. SUBSTR(users, 1, 3) <>  ;
   'SYS' .AND. SUBSTR(users, 1,  ;
   3) <> 'ADM' .AND. SUBSTR(users,  ;
   1, 3) <> 'MAR'
     RETURN
ENDIF
CLOSE DATABASES
SELECT 1
USE SHARED ST_ISREP ORDER CODIGO
SELECT 2
USE SHARED ST_IOREP ORDER CODIGO
SELECT 3
USE SHARED ST_ICLPR ORDER CODIGO
SELECT 4
USE SHARED ST_IMODE ORDER CODIGO
SELECT 5
USE SHARED ST_ISERI ORDER  ;
    SER_CODMAR
SELECT 6
USE SHARED ST_SINT ORDER  ;
    sin_lincod
SELECT 7
USE SHARED ST_SICLI ORDER CODIGO
SELECT 8
USE SHARED GE_TAB0 ORDER CODIGO
SELECT 9
USE SHARED ST_ITECN ORDER CODIGO
SELECT 10
USE SHARED ST_MOBRA ORDER CODIGO
SELECT 11
USE SHARED ST_IDPED ORDER  ;
    DRE_NUMORD
SELECT 12
USE SHARED GC_PRO00 ORDER CODIGO
SELECT 13
USE SHARED gc_vnd00 ORDER CODIGO
SELECT 14
USE SHARED ST_IPREP ORDER CODIGO
SELECT 15
USE SHARED ST_USERS
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF FOUND()
     wk_igv = tab_factor
     wrk_facigv = tab_factor /  ;
                  100
ELSE
     DO error WITH  ;
        '**No Definido el IGV**'
     CLOSE DATABASES
     ON KEY LABEL F6
     DO sacawin
     RETURN
ENDIF
PUBLIC tempo, ubicacion
STORE 'arriba' TO ubicacion
tempo = ' '
wrk_origen = SPACE(2)
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
@ 2, 1 SAY DATE()
titu1 = ' MANTENCION '
titu2 = ' CAMBIO DE TIPO DE ATENCION '
DO saycenter WITH 1, titu1
DO saycenter WITH 2, titu2
DO esc_modo WITH 'I'
@ 6, 10 CLEAR TO 10, 65
@ 6, 10 TO 10, 65
SAVE SCREEN TO wk_panta
wk_numord = 0
ppal = .T.
DO WHILE ppal
     RESTORE SCREEN FROM wk_panta
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'BBB', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     efecin = 1
     STORE 0 TO veces, wk_numord,  ;
           wk_numte1, wk_numte2,  ;
           wrk_totcon, wrk_movfga,  ;
           wrk_movgar, wrk_salir
     STORE SPACE(4) TO wk_ogar,  ;
           wk_oest, wk_nomdis,  ;
           wk_nomciu, w_linea
     STORE SPACE(30) TO wk_nom1,  ;
           wk_nom2, nota1, nota2,  ;
           nota3, nota4, nota5,  ;
           nota6, wk_estado,  ;
           wk_destia
     STORE SPACE(15) TO wk_doga,  ;
           wk_prov
     STORE SPACE(4) TO wk_noment,  ;
           wk_nomcal, wk_nomdis,  ;
           wrk_codemp, wk_nomciu,  ;
           wk_desdis, wk_desciu,  ;
           wk_fevt, wk_fecg,  ;
           wk_newgar, wk_indori
     STORE SPACE(8) TO wk_numsol,  ;
           wk_otec
     @ 8, 20 SAY  ;
       ' N§ Orden Reparaci¢n '  ;
       GET wk_numord PICTURE  ;
       '99999999' VALID  ;
       valida(wk_numord) .AND.   ;
       .NOT. EMPTY(wk_numord)  ;
       WHEN antes()
     SET CURSOR ON
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     ACTIVATE SCREEN
     ACTIVATE WINDOW trabajo
     SET DISPLAY TO VGA50
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 42, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 45, 0 TO 48, 76
     ACTIVATE SCREEN
     ACTIVATE WINDOW trabajo
     lin = 40
     anc = 75
     des = 1
     com = 1
     DIMENSION solic( 48)
     STORE FOPEN('solicitu.txt')  ;
           TO file_handl
     FOR i = 1 TO 48
          solic( i) =  ;
               FREAD(file_handl,  ;
               77)
     ENDFOR
     = FCLOSE(file_handl)
     DIMENSION wk_codsin( 15)
     DIMENSION wk_acceso( 15)
     DIMENSION wk_observ( 06)
     SELECT st_isrep
     SEEK wk_numsol
     wk_fecemi = fecemi
     wk_feccom = feccom
     wk_emisor = codemi
     wk_codcli = VAL(codent)
     wk_indori = indori
     wk_indest = indest
     wk_codmar = codmar
     wk_codmod = codmod
     wk_numser = numser
     wk_coddes = coddes
     FOR i = 1 TO 15
          wk_codsin = SPACE(35)
          wk_acceso( i) =  ;
                   SUBSTR(desace,  ;
                   1 + ((i - 1) *  ;
                   35), 35)
          wk_acceso( i) =  ;
                   wk_acceso(i) +  ;
                   SPACE(35 -  ;
                   LEN(wk_acceso(i)))
          IF i <= 6
               wk_observ( i) =  ;
                        SUBSTR(observ,  ;
                        1 + ((i -  ;
                        1) * 45),  ;
                        45)
               wk_observ( i) =  ;
                        wk_observ(i) +  ;
                        SPACE(45 -  ;
                        LEN(wk_observ(i)))
          ENDIF
     ENDFOR
     SELECT st_iclpr
     SEEK 'C' + STR(wk_codcli,  ;
          11)
     IF FOUND()
          wk_noment = noment
          wk_nomcal = nomcal
          wk_nomdis = nomdis
          wk_nomciu = nomciu
          wk_numte1 = numte1
          wk_numte2 = numte2
     ENDIF
     SELECT st_imode
     SEEK wk_codmar + wk_codmod
     wrk_codart = codcla
     w_linea = linea
     IF wk_indori = 'GARA' .OR.  ;
        wk_indori = 'GREC'
          SELECT st_iseri
          SEEK wk_codmar +  ;
               wk_codmod +  ;
               wk_numser
          IF FOUND()
               wk_prov = 'Proveedor:' +  ;
                         ALLTRIM(codent)
               wk_doga = 'Doc.Garan:' +  ;
                         ALLTRIM(docgar)
               wk_fevt = 'Fecha Vta:' +  ;
                         DTOC(fecvta)
               wk_fecg = 'Fecha Fin:' +  ;
                         DTOC(fecgar)
          ENDIF
     ENDIF
     SELECT st_sicli
     SEEK wk_numsol
     i = 1
     DO WHILE  .NOT. EOF() .AND.  ;
        numdoc==wk_numsol
          wk_aux2 = SUBSTR(codsin,  ;
                    2, 3)
          SELECT st_sint
          SEEK w_linea + wk_aux2
          wk_codsin( i) =  ;
                   SUBSTR(dessin,  ;
                   1, 35)
          i = i + 1
          SELECT st_sicli
          SKIP
     ENDDO
     DO col_bk1b
     DO col_bk2b
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
     DO veorden
     @ 17, 35 GET wk_newgar  ;
       PICTURE '@!' VALID  ;
       valtab('INGA',wk_newgar,40, ;
       15) WHEN antes()
     READ
     IF LASTKEY() = 27
          SET DISPLAY TO VGA25
          ACTIVATE SCREEN
          ZOOM WINDOW trabajo  ;
               NORM FROM 1, 0 TO  ;
               17, 76
          ZOOM WINDOW indicar  ;
               NORM FROM 20, 0 TO  ;
               23, 76
          ACTIVATE WINDOW trabajo
          LOOP
     ENDIF
     DO esc_indica WITH 1, 'AYU',  ;
        'BBB', 'GRA', 'BBB'
     DO esc_indica WITH 2, 'BBB',  ;
        'BBB', 'BBB', 'ESC'
     DO empleado
     DO WHILE .T. .AND. wrk_salir<> ;
        1
          wk_inkey = 0
          DO WHILE  .NOT.  ;
             (STR(wk_inkey, 2)$ ;
             '27,-1')
               wk_inkey = INKEY(0,  ;
                          'H')
          ENDDO
          IF LASTKEY() = 27
               EXIT
          ENDIF
          DO CASE
               CASE wk_inkey ==  ;
                    18
                    DO mueve3  ;
                       WITH  ;
                       CHR(wk_inkey)
               CASE wk_inkey == 3
                    DO mueve3  ;
                       WITH  ;
                       CHR(wk_inkey)
               CASE wk_inkey == - ;
                    1
                    DO graba
                    EXIT
          ENDCASE
     ENDDO
     SET DISPLAY TO VGA25
     ACTIVATE SCREEN
     ZOOM WINDOW trabajo NORM  ;
          FROM 1, 0 TO 17, 76
     ZOOM WINDOW indicar NORM  ;
          FROM 20, 0 TO 23, 76
     ACTIVATE WINDOW trabajo
ENDDO
CLOSE DATABASES
ON KEY LABEL F6
SET DISPLAY TO VGA25
ZOOM WINDOW trabajo NORM FROM 1,  ;
     0 TO 17, 76
ZOOM WINDOW indicar NORM FROM 20,  ;
     0 TO 23, 76
DO sacawin
RELEASE wk_pantax
RETURN
*
PROCEDURE ayuda12
ON KEY
IF VARREAD() = 'WK_NUMORD'
     SELECT st_iorep
     wrk_origen = 'OR'
     campoa = 'numdoc+" "+dtoc(fecemi)+" "+NUMSOL+" "+SUBSTR(NUMSER,1,12)+" "+CODENT+" "+SUBSTR(codmod,1,10)+" "+subst(indest,1,2)+" "+ALLTRIM(INDORI)'
     DO ayuda4 WITH campoa,  ;
        wrk_origen
ENDIF
IF VARREAD() = 'WK_NEWGAR'
     SELECT ge_tab0
     SET FILTER TO tab_codpre == 'INGA'
     campo = 'tab_codtab + "  " + tab_destab'
     titulo = 'TIPO DE ATENCION'
     DO ayuda1 WITH campo, titulo,  ;
        'tab_codtab'
     SET FILTER TO
ENDIF
*
FUNCTION valida
PARAMETER num
SELECT st_iorep
SET ORDER TO codigo
SEEK STR(num, 8)
IF  .NOT. FOUND()
     DO error WITH  ;
        '*** N§ Orden Reparaci¢n NO EXISTE. ***'
     RETURN .F.
ENDIF
IF indest = 'N'
     DO error WITH  ;
        '*** N§ Orden Reparaci¢n esta ANULADA ***'
     RETURN .F.
ENDIF
IF indest = 'B' .OR. indest = 'F'
     DO error WITH  ;
        '*** N§ Orden Reparacion esta FACTURADA ***'
     RETURN .F.
ENDIF
wk_numsol = numsol
RETURN .T.
*
PROCEDURE veorden
ACTIVATE WINDOW indicar
ACTIVATE WINDOW trabajo
@ 14, 10 FILL TO 28, 65
@ 13, 09 CLEAR TO 27, 64
@ 13, 09 TO 27, 64
@ 13, 28 SAY  ;
  ' CONSULTA DE ORDEN ' COLOR N/W 
veces = veces + 1
IF veces = 1
     SELECT st_iorep
     SEEK STR(wk_numord, 8)
     wk_oest = auxest
     wk_otec = codtec
     wk_ogar = indori
     nota1 = SUBSTR(observ, 1,  ;
             38)
     nota2 = SUBSTR(observ, 39,  ;
             38)
     nota3 = SUBSTR(observ, 78,  ;
             38)
     nota4 = SUBSTR(observ, 117,  ;
             38)
     nota5 = SUBSTR(observ, 156,  ;
             38)
     nota6 = SUBSTR(observ, 195,  ;
             38)
     SELECT ge_tab0
     SEEK 'ESOR' + wk_oest
     IF FOUND()
          wk_nom1 = SUBSTR(tab_destab,  ;
                    1, 28)
     ENDIF
     SEEK 'INGA' + wk_ogar
     IF FOUND()
          wk_destia = ALLTRIM(tab_destab)
     ENDIF
     SELECT st_itecn
     SEEK wk_otec
     IF FOUND()
          wk_nom2 = SUBSTR(noment,  ;
                    1, 28)
     ENDIF
ENDIF
@ 15, 11 SAY 'N§ Orden     :' +  ;
  STR(wk_numord, 8)
@ 16, 11 SAY 'Fecha Orden  :' +  ;
  DTOC(DATE())
@ 17, 11 SAY 'Tipo Atenci¢n:' +  ;
  wk_ogar
@ 18, 11 SAY 'Estado       :' +  ;
  wk_oest
@ 18, 35 SAY wk_nom1
@ 19, 11 SAY 'T‚cnico      :' +  ;
  wk_otec
@ 19, 35 SAY wk_nom2
@ 20, 11 SAY 'Notas        :'
@ 21, 25 SAY nota1 PICTURE '@!'
@ 22, 25 SAY nota2 PICTURE '@!'
@ 23, 25 SAY nota3 PICTURE '@!'
@ 24, 25 SAY nota4 PICTURE '@!'
@ 25, 25 SAY nota5 PICTURE '@!'
@ 26, 25 SAY nota6 PICTURE '@!'
RETURN
*
PROCEDURE col_bk1b
SELECT ge_tab0
SEEK 'EMIS' + wk_emisor
IF FOUND()
     wk_nomemi = tab_destab
ELSE
     wk_nomemi = SPACE(35)
ENDIF
SEEK 'ESTA' + wk_indest
IF FOUND()
     wk_estado = tab_destab
ENDIF
SEEK 'DIST' + wk_nomdis
IF FOUND()
     wk_desdis = tab_destab
ENDIF
SEEK 'PROV' + wk_nomciu
IF FOUND()
     wk_desciu = tab_destab
ENDIF
DO coloca WITH 01, 66, wk_numsol
DO coloca WITH 16, 20,  ;
   DTOC(wk_fecemi)
DO coloca WITH 16, 38,  ;
   SUBSTR(TIME(), 1, 5)
DO coloca WITH 18, 20, wk_emisor
DO coloca WITH 18, 25,  ;
   SUBSTR(wk_nomemi, 1, 24)
DO coloca WITH 02, 01,  ;
   SUBSTR(wk_estado, 1, 06)
DO coloca WITH 05, 20, wk_codmar
DO coloca WITH 06, 20, wk_codmod
DO coloca WITH 07, 20, wk_numser
DO coloca WITH 08, 20, wk_indori
SEEK 'MARC' + wk_codmar
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
SEEK 'INGA' + wk_indori
IF FOUND()
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
SELECT st_imode
SEEK wk_codmod
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC'
     DO coloca WITH 5, 51,  ;
        wk_prov
     DO coloca WITH 6, 51,  ;
        wk_doga
     DO coloca WITH 7, 51,  ;
        wk_fevt
     DO coloca WITH 8, 51,  ;
        wk_fecg
ENDIF
IF wk_indori == 'GARA' .OR.  ;
   wk_indori == 'GREC'
     DO coloca WITH 19, 20,  ;
        'EN GARANTIA   '
ELSE
     DO coloca WITH 19, 20,  ;
        'FUERA GARANTIA'
ENDIF
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca WITH 10, 30, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 7)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 7)
DO coloca WITH 16, 65,  ;
   DTOC(wk_feccom)
DO coloca WITH 21, 20, wk_coddes+ ;
   ' '+IIF(wk_coddes='R',  ;
   'REPARACION ', 'DOMICILIO')
FOR i = 1 TO 15
     DO coloca WITH 24+i, 2,  ;
        wk_codsin(i)
ENDFOR
FOR i = 1 TO 15
     DO coloca WITH 24+i, 38,  ;
        wk_acceso(i)
ENDFOR
RETURN
*
FUNCTION valtab
PARAMETER clave, codig, colu,  ;
          largo
SELECT ge_tab0
SEEK clave + codig
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '*** C¢digo NO EXISTE ***'
     RETURN .F.
ENDIF
IF wk_ogar = wk_newgar
     DO error2 WITH  ;
        '*** La O/R se tiene la misma Garant¡a ***'
     RETURN .F.
ENDIF
SELECT st_isrep
SEEK wk_numsol
IF  .NOT. FOUND()
     DO error2 WITH  ;
        '*** No existe S/Servicio ***'
     RETURN .F.
ELSE
     IF wk_indori <> wk_newgar
          DO error2 WITH  ;
             '*** La S/Servicio tiene diferente garant¡a ***'
          RETURN .F.
     ENDIF
ENDIF
IF nivell = 'A1'
     IF SUBSTR(wk_newgar, 2, 1) =  ;
        'R'
          DO error2 WITH  ;
             '**** Reclamos Coordine con talleres **** '
          RETURN .F.
     ENDIF
ENDIF
SELECT st_mobra
SEEK wk_codmar + wrk_codart
IF FOUND()
     wrk_movgar = mo_monmog
     wrk_movfga = mo_monmof
ENDIF
IF wrk_movgar = 0 .OR. wrk_movfga =  ;
   0
     DO error2 WITH  ;
        '*** No existe valor de M/Obra ***'
     RETURN .F.
ENDIF
IF wk_newgar = 'FGAR' .OR.  ;
   wk_newgar = 'FREC'
     IF wk_ogar = 'GARA' .OR.  ;
        wk_ogar = 'GREC'
          DO busca_rep
          IF wrk_totcon > 0
               DO error2 WITH  ;
                  '*** Existen rptos. cargados de Consig.***'
               RETURN .F.
          ENDIF
     ENDIF
ENDIF
DO sacaf6
RETURN .T.
*
PROCEDURE busca_rep
wrk_totcon = 0
SELECT st_idped
SEEK STR(wk_numord, 8)
IF FOUND()
     DO WHILE numord= ;
        STR(wk_numord, 8) .AND.  ;
        canpro>0
          wrk_numped = numdoc
          wrk_codpro = codpro
          SELECT st_imode
          SEEK wrk_numped
          IF FOUND()
               IF indest <> 'N'
                    SELECT gc_pro00
                    SEEK wrk_codpro
                    IF FOUND()
                         IF pro_propie =  ;
                            'C'
                              wrk_totcon =  ;
                               wrk_totcon +  ;
                               1
                         ENDIF
                    ENDIF
                    SKIP
               ENDIF
          ENDIF
          SELECT st_idped
          SKIP
     ENDDO
ENDIF
RETURN
*
PROCEDURE graba
DO mensa2 WITH  ;
   '*** Espere un momento, por favor ***',  ;
   'COLO'
SELECT st_iorep
SEEK STR(wk_numord, 8)
IF FOUND()
     DO rbloquea
     REPLACE indori WITH  ;
             wk_newgar
     DO CASE
          CASE wk_newgar = 'GARA'
               REPLACE cosmob  ;
                       WITH  ;
                       wrk_movgar
          CASE wk_newgar = 'FGAR'  ;
               .OR. wk_newgar =  ;
               'PVEN'
               REPLACE cosmob  ;
                       WITH  ;
                       wrk_movfga
          CASE SUBSTR(wk_newgar,  ;
               2, 1) = 'R'
               REPLACE cosmob  ;
                       WITH 0
     ENDCASE
     IF indest = 'C'
          REPLACE subtot WITH  ;
                  (cosmob +  ;
                  cosrep +  ;
                  flete)
          REPLACE totnet WITH  ;
                  (cosmob +  ;
                  cosrep +  ;
                  flete)
          REPLACE totigv WITH  ;
                  totnet *  ;
                  wrk_facigv
          REPLACE totbru WITH  ;
                  totnet +  ;
                  totigv
     ENDIF
     UNLOCK
     SELECT st_users
     APPEND BLANK
     DO rbloquea
     REPLACE codemp WITH  ;
             wrk_codemp
     REPLACE numord WITH  ;
             STR(wk_numord, 8)
     REPLACE numsol WITH  ;
             wk_numsol
     REPLACE antgar WITH wk_ogar
     REPLACE nuegar WITH  ;
             wk_newgar
     REPLACE estado WITH wk_oest
     REPLACE user WITH users
     REPLACE fecha WITH DATE()
     REPLACE hora WITH TIME()
     UNLOCK
ENDIF
SELECT 20
USE SHARED st_ispre ORDER  ;
    st_numsol
SEEK st_iorep.numsol
IF FOUND()
     DO rbloquea
     REPLACE indori WITH  ;
             st_iorep.indori
     REPLACE user WITH users
     REPLACE date WITH DATE()
     REPLACE time WITH TIME()
     UNLOCK
ENDIF
DO mensa2 WITH  ;
   '*** Espere un momento, por favor ***',  ;
   'SACA'
RETURN
*
PROCEDURE empleado
ON KEY
DEFINE WINDOW empleado FROM 30,  ;
       16 TO 34, 56
ACTIVATE WINDOW empleado
STORE SPACE(05) TO wrk_codemp
STORE SPACE(15) TO wrk_codbla
DO WHILE .T.
     SET CURSOR ON
     @ 01, 02 SAY 'Usuario :' GET  ;
       wrk_codemp PICTURE '@!'
     @ 02, 12 GET wrk_codbla  ;
       PICTURE '@!'
     READ
     IF LASTKEY() = 27
          wrk_salir = 1
          DEACTIVATE WINDOW  ;
                     empleado
          RETURN
     ENDIF
     SELECT gc_vnd00
     SEEK 'A' + wrk_codemp
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             '*** C¢digo de Empleado NO EXISTE ***'
          LOOP
     ENDIF
     wrk_nomemp = vnd_nombre
     @ 01, 17 SAY  ;
       SUBSTR(wrk_nomemp, 1, 20)
     DEACTIVATE WINDOW empleado
     RETURN
ENDDO
RETURN
*
PROCEDURE antes
ON KEY LABEL F6 DO ayuda12
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
