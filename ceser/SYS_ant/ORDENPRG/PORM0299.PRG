*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
wrk_progra = PROGRAM()
DO crea_win
CLEAR TYPEAHEAD
tit_prg = ' IMPRESION '
@ 2, 1 SAY DATE() COLOR SCHEME 8
DO saycenter WITH 1, tit_prg
DO saycenter WITH 2,  ;
   ' GENERACION DE GUIAS DE REMISION '
CLOSE DATABASES
SELECT 1
USE ge_tab0 ORDER codigo
SELECT 2
USE st_isrep ORDER codigo
SELECT 3
USE st_iclpr ORDER codigo
SELECT 4
USE st_iorep ORDER ord_numsol
SELECT 5
USE ST_ITECN ORDER CODIGO
SELECT 6
USE ST_IMODE ORDER CODIGO
SELECT 7
USE GC_VND00 ORDER CODIGO
valor = .T.
DO WHILE valor
     STORE SPACE(4) TO w_coddis,  ;
           w_emisor, wk_coddoc,  ;
           w_indori, w_marca
     STORE SPACE(5) TO w_user
     STORE SPACE(50) TO w_observ,  ;
           w_observ2
     STORE SPACE(9) TO w_nomcal,  ;
           w_noment, w_desmon,  ;
           w_codcli, w_nomtec,  ;
           w_nommod, w_codmod,  ;
           w_serie, w_usuario,  ;
           w_dirtec
     STORE SPACE(11) TO w_codent
     STORE 0 TO w_numsol,  ;
           w_codtec, w_pasa,  ;
           w_nroguia
     w_nroruc = '25360443'
     @ 03, 00 CLEAR TO 15, 74
     @ 03, 00 TO 03, 74
     @ 04, 01 SAY  ;
       'S/Servicio ...:' COLOR W+/ ;
       N 
     @ 05, 01 SAY  ;
       'Tipo de Mov. .:' COLOR W+/ ;
       N 
     @ 05, 31 SAY  ;
       'Origen/Destino :' COLOR W+/ ;
       N 
     @ 07, 01 SAY  ;
       'Observaciones :' COLOR W+/ ;
       N 
     @ 09, 01 SAY  ;
       'Usuario       :'
     @ 10, 00 TO 10, 74
     @ 11, 01 SAY 'Cliente   :'
     @ 12, 01 SAY 'Direccion :'
     @ 13, 01 SAY 'Art¡culo  :'
     @ 13, 41 SAY 'Modelo :'
     @ 14, 01 SAY 'Marca     :'
     @ 14, 41 SAY 'Serie  :'
     SET CURSOR ON
     ON KEY LABEL F6 do ayuda12
     @ 04, 17 GET w_numsol  ;
       PICTURE '99999999' VALID  ;
       despues(1) WHEN antes(1)
     @ 05, 17 GET w_tipo DEFAULT  ;
       1 PICTURE  ;
       '@*RVN Entrega;Recojo'  ;
       WHEN antes(2)
     @ 05, 47 GET w_origen  ;
       DEFAULT 1 PICTURE  ;
       '@*RVN Cliente;Proveedor'  ;
       VALID despues(3)
     @ 07, 17 GET w_observ  ;
       PICTURE '@!' VALID  ;
       despues(4)
     @ 08, 17 GET w_observ2  ;
       PICTURE '@!' VALID  ;
       despues(4)
     @ 09, 17 GET w_user PICTURE  ;
       '@!' VALID despues(5)
     READ
     IF LASTKEY() = 27
          STORE .F. TO valor
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY LABEL f6
DO sacawin
RETURN
*
PROCEDURE antes
PARAMETER opc
DO CASE
     CASE opc = 1
          DO esc_modo WITH 'I'
          DO esc_indica WITH 1,  ;
             'AYU', 'BUS', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
     CASE opc = 2
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
ENDCASE
RETURN
*
FUNCTION despues
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(w_numsol)
               DO error WITH  ;
                  '*** No se aceptan Blancos ***'
               RETURN .F.
          ENDIF
          w_numero = STR(w_numsol,  ;
                     8)
          SELECT st_isrep
          SET ORDER TO CODIGO
          SEEK w_numero
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** N£mero de Solicitud No Existe ***'
               RETURN .F.
          ENDIF
          IF indest = 'A   '
               DO error WITH  ;
                  '*** S/Servicio se Encuentra Anulada ***'
               RETURN .F.
          ENDIF
          w_codent = codent
          w_codcli = codent
          w_indori = indori
          w_marca = codmar
          w_serie = numser
          w_codmod = codmod
          @ 13, 50 SAY codmod
          @ 14, 13 SAY  ;
            SUBSTR(ootab('MARC', ;
            codmar), 1, 20)
          @ 14, 50 SAY numser
          SELECT st_iclpr
          SEEK 'C' + w_codent
          IF FOUND()
               w_noment = noment
               w_nomcal = nomcal
               w_numtel = numte1
               w_coddis = nomdis
               @ 11, 13 SAY  ;
                 w_noment
               @ 12, 13 SAY  ;
                 ALLTRIM(w_nomcal) +  ;
                 ' - ' +  ;
                 ootab('DIST', ;
                 w_coddis)
          ENDIF
          SELECT st_imode
          SEEK w_marca + w_codmod
          IF FOUND()
               @ 13, 13 SAY  ;
                 nommod
               w_nommod = nommod
          ELSE
               w_nommod = ' '
          ENDIF
     CASE opc = 3
          IF w_origen = 2
               w_pasa = 0
               ON KEY LABEL F6 do ayuda12
               @ 06, 61 GET  ;
                 w_codtec PICTURE  ;
                 '999999999'  ;
                 VALID  .NOT.  ;
                 EMPTY(w_codtec)
               READ
               IF LASTKEY() = 27
                    RETURN .F.
               ENDIF
               SELECT st_itecn
               SEEK STR(w_codtec,  ;
                    9)
               IF  .NOT. FOUND()
                    DO error WITH  ;
                       '*** C¢digo No Existe ***'
                    RETURN .F.
               ENDIF
               @ 07, 47 SAY  ;
                 SUBSTR(noment, 1,  ;
                 25)
               w_nomtec = noment
               w_dirtec = nomcal +  ;
                          ' ' +  ;
                          nomdis
               w_pasa = 1
          ENDIF
     CASE opc = 5
          SELECT gc_vnd00
          SEEK 'A' + w_user
          IF  .NOT. FOUND()
               DO error WITH  ;
                  '*** Usuario No Existe ***'
               RETURN .F.
          ENDIF
          w_usuario = vnd_nombre
          @ 09, 23 SAY vnd_nombre
          IF w_origen = 2 .AND.  ;
             w_pasa = 0
               DO error WITH  ;
                  '*** Falta C¢digo de Proveedor ***'
               RETURN .F.
          ENDIF
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'IMP',  ;
             'BBB'
          DO esc_indica WITH 2,  ;
             'BBB', 'BBB', 'BBB',  ;
             'ESC'
          DO WHILE .T.
               = INKEY(0, 'H')
               IF LASTKEY() = 27
                    EXIT
               ENDIF
               IF LASTKEY() = -6
                    ON KEY
                    DO nro_guia
                    DO imprime
                    EXIT
               ENDIF
          ENDDO
ENDCASE
RETURN
*
PROCEDURE ayuda12
ON KEY LABEL F6
ON KEY LABEL F8
IF VARREAD() == 'W_NUMSOL'
     SELECT st_isrep
     wrk_origen = 'SS'
     campoa = 'numdoc+"  "+dtoc(fecemi)+"  "+codent+"  "+codmod+"   "+numser+" "+subst(indest,1,2)+" "+indori'
     DO ayuda7 WITH campoa,  ;
        wrk_origen
ENDIF
IF VARREAD() == 'W_CODTEC'
     SELECT st_itecn
     campoa = '"  "+codent+"  "+noment'
     campob = '"  "+noment+"  "+codent'
     titulo = 'AYUDA DE TECNICOS'
     DO ayuda2 WITH campoa,  ;
        campob, titulo,  ;
        'iif(len(ltrim(codent))<11,codent+chr(13),codent)'
ENDIF
ON KEY LABEL F6 do ayuda12
RETURN
*
PROCEDURE imprime
RUN CAPTURE Q=QGUIAS NB NFF TI=5 >  ;
    NUL
SET CONSOLE OFF
SET DEVICE TO PRINTER
SET PRINTER ON
@ PROW(), PCOL() SAY CHR(18)
@ PROW(), PCOL() SAY CHR(27) +  ;
  'C' + CHR(33)
@ 02, 53 SAY 'R.U.C.  ' +  ;
  '20253604434'
@ PROW(), PCOL() SAY CHR(15)
@ 04, 21 SAY  ;
  'Domicilio fiscal : Av. Rep. de Panam  4123 - Surquillo'
IF w_tipo = 2
     @ 07, 62 SAY '- RECOJO'
ENDIF
@ 07, 077 SAY 'X'
IF w_tipo = 2
     @ 09, 15 SAY rge_abrev
     @ 09, 80 SAY 'RUC : ' +  ;
       w_nroruc
ELSE
     IF w_origen = 1
          @ 09, 15 SAY w_noment
          @ 09, 80 SAY  ;
            'CODIGO : ' +  ;
            ALLTRIM(w_codent)
     ELSE
          @ 09, 15 SAY w_nomtec
          @ 09, 80 SAY 'RUC : ' +  ;
            ALLTRIM(STR(w_codtec,  ;
            10))
     ENDIF
ENDIF
@ 09, 108 SAY DATE()
@ 09, 120 SAY TIME()
IF w_tipo = 2
     @ 10, 15 SAY rge_calle +  ;
       ' - ' + rge_distri
ELSE
     IF w_origen = 2
          @ 10, 15 SAY w_dirtec
     ELSE
          @ 10, 15 SAY w_nomcal +  ;
            ' - ' + ootab('DIST', ;
            w_coddis)
     ENDIF
ENDIF
IF w_tipo = 2
     IF w_origen = 1
          @ 11, 15 SAY w_noment
          @ 11, 80 SAY  ;
            'CODIGO : ' +  ;
            ALLTRIM(w_codent)
          @ 12, 15 SAY w_nomcal +  ;
            ' - ' + ootab('DIST', ;
            w_coddis)
     ELSE
          @ 11, 15 SAY w_nomtec
          @ 11, 80 SAY 'RUC : ' +  ;
            ALLTRIM(STR(w_codtec,  ;
            10))
          @ 12, 15 SAY w_dirtec
     ENDIF
ELSE
     @ 11, 15 SAY rge_abrev
     @ 11, 80 SAY 'RUC : ' +  ;
       w_nroruc
     @ 12, 15 SAY rge_calle +  ;
       ' - ' + rge_distri
ENDIF
@ 15, 000 SAY STR(w_numsol, 8)
@ 15, 012 SAY SUBSTR(ootab('MARC', ;
  w_marca), 1, 15)
@ 15, 028 SAY w_codmod
@ 15, 051 SAY w_serie
@ 15, 075 SAY w_nommod
@ 15, 106 SAY w_indori
@ 20, 01 SAY 'Operador: ' +  ;
  w_usuario
@ 26, 80 SAY w_observ
@ 27, 80 SAY w_observ2
@ 29, 01 SAY STR(w_nroguia, 10)
EJECT
SET PRINTER TO
SET PRINTER OFF
SET DEVICE TO SCREEN
SET CONSOLE ON
RUN CAPTURE EC L=1 > NUL
RETURN
*
PROCEDURE nro_guia
SELECT ge_tab0
SEEK 'GUIA' + '200 '
IF FOUND()
     w_nroguia = tab_factor + 1
     REPLACE tab_factor WITH  ;
             w_nroguia
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***
