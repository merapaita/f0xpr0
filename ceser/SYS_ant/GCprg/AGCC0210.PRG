*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER c
ON KEY
DEFINE WINDOW detalle FROM 04, 03  ;
       TO 10, 76 COLOR SCHEME 7
DEFINE WINDOW meses FROM 11, 03  ;
       TO 21, 76 NONE COLOR  ;
       SCHEME 7
DEFINE MENU conven
DEFINE PAD x OF conven PROMPT  ;
       ' C O M P R A S ' AT 7,  ;
       22
DEFINE PAD y OF conven PROMPT  ;
       '  V E N T A S  ' AT 7,  ;
       41
ON SELECTION PAD x OF conven DEAC MENU;
conven  WITH PAD()  
ON SELECTION PAD y OF conven DEAC MENU;
conven  WITH PAD()  
DEFINE MENU opciones
DEFINE PAD a OF opciones PROMPT  ;
       '  PRODUCTO  ' AT 7, 03
DEFINE PAD b OF opciones PROMPT  ;
       '  CLIENTE  ' AT 7, 18
DEFINE PAD c OF opciones PROMPT  ;
       '  VENDEDOR  ' AT 7, 32
DEFINE PAD d OF opciones PROMPT  ;
       ' FORMA PAGO ' AT 7, 47
DEFINE PAD e OF opciones PROMPT  ;
       ' PROVEEDOR ' AT 7, 62
ON SELECTION PAD a OF opciones DEAC MENU;
opciones  WITH PAD()  
ON SELECTION PAD b OF opciones DEAC MENU;
opciones  WITH PAD()  
ON SELECTION PAD c OF opciones DEAC MENU;
opciones  WITH PAD()  
ON SELECTION PAD d OF opciones DEAC MENU;
opciones  WITH PAD()  
ON SELECTION PAD e OF opciones DEAC MENU;
opciones  WITH PAD()  
SELECT 1
USE GC_PRO00 ORDER CODIGO
wrk_selpro = SELECT()
SELECT 2
USE GE_TAB0 ORDER CODIGO
SELECT 3
USE GC_CLI00 ORDER CODIGO
SELECT 4
USE GC_VND00 ORDER CODIGO
SELECT 5
USE GC_EST00
ACTIVATE WINDOW tablas
CLEAR
DO p_prestab WITH 'CONSULTAS',  ;
   'ESTADISTICAS', 'CONSULTA'
@ 2, 63 SAY PROGRAM()
DO p_footer WITH  ;
   '100000000000000000001', 2
DO WHILE .T.
     DO p_footer WITH  ;
        '100000000000000000001',  ;
        2
     ACTIVATE WINDOW tablas
     @ 06, 21 SAY  ;
       'ษออออออออออออออออออหออออออออออออออออออป'
     @ 07, 21 SAY  ;
       'บ  C O M P R A S   บ    V E N T A S   บ'
     @ 08, 21 SAY  ;
       'ศออออออออออออออออออสออออออออออออออออออผ'
     ACTIVATE MENU conven
     IF LASTKEY() = 27
          CLOSE DATABASES
          ON KEY
          DEACTIVATE WINDOW  ;
                     tablas,  ;
                     codigo,  ;
                     footer
          RELEASE WINDOW detalle,  ;
                  meses
          DO p_footer WITH  ;
             '100000000001011000001',  ;
             1
          ACTIVATE SCREEN
          DO p_tempos
          RETURN
     ENDIF
     IF PAD() = 'X'
          wrk_opcion = 'C O M P R A S'
          wrk_estado = 'C'
     ELSE
          wrk_opcion = 'V E N T A S'
          wrk_estado = 'V'
     ENDIF
     DO WHILE .T.
          ACTIVATE WINDOW tablas
          @ 06, 02 SAY  ;
            'ษออออออออออออออหอออออออออออออหออออออออออออออหออออออออออออออหอออออออออออออป'
          @ 07, 02 SAY  ;
            'บ   PRODUCTO   บ   CLIENTE   บ   VENDEDOR   บ  FORMA PAGO  บ  PROVEEDOR  บ'
          @ 08, 02 SAY  ;
            'ศออออออออออออออสอออออออออออออสออออออออออออออสออออออออออออออสอออออออออออออผ'
          ACTIVATE MENU opciones
          IF LASTKEY() = 27
               @ 06, 02 CLEAR TO  ;
                 08, 77
               EXIT
          ENDIF
          wrk_pad = PAD()
          DO CASE
               CASE wrk_pad = 'A'
                    wrk_codigo = SPACE(14)
                    wrk_descri = 'Cขdigo del Producto      :'
                    wrk_selec = SELECT()
                    wrk_campo = 'WRK_CODIGO'
                    wrk_tipo = 'EST_CODPRO'
               CASE wrk_pad = 'B'
                    wrk_codigo = SPACE(11)
                    wrk_descri = 'Cขdigo del Cliente       :'
                    wrk_tiper = 'C'
                    wrk_var = 'WRK_CODIGO'
                    wrk_tipo = 'EST_CODCLI'
               CASE wrk_pad = 'C'
                    wrk_tiper = 'V'
                    wrk_var = 'WRK_CODIGO'
                    wrk_codigo = SPACE(5)
                    wrk_descri = 'Cขdigo del Vendedor      :'
                    wrk_tipo = 'EST_CODVEN'
               CASE wrk_pad = 'D'
                    wrk_busca = 'FPAG'
                    wrk_var = 'WRK_CODIGO'
                    wrk_codigo = SPACE(4)
                    wrk_descri = 'Cขdigo de Forma de Pago  :'
                    wrk_tipo = 'EST_CODPAG'
               CASE wrk_pad = 'E'
                    wrk_tiper = 'P'
                    wrk_var = 'WRK_CODIGO'
                    wrk_codigo = SPACE(11)
                    wrk_descri = 'Cขdigo del Proveedor     :'
                    wrk_tipo = 'EST_CODPVR'
          ENDCASE
          ACTIVATE WINDOW detalle
          CLEAR
          wrk_emisor = SPACE(4)
          wrk_anopro = 0
          wrk_var2 = 'WRK_EMISOR'
          @ 00, 13 SAY  ;
            'E S T A D I S T I C A S    D E    ' +  ;
            wrk_opcion
          @ 01, 13 SAY  ;
            '฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿'
          @ 02, 05 SAY wrk_descri
          @ 03, 05 SAY  ;
            'Punto de Emisiขn         :'
          @ 04, 05 SAY  ;
            'Aคo de Proceso           :'
          SET CURSOR ON
          @ 02, 32 GET wrk_codigo  ;
            VALID post(1) WHEN  ;
            antes(1)
          @ 03, 32 GET wrk_emisor  ;
            PICTURE '@!' VALID  ;
            post(2) WHEN  ;
            antes(2)
          @ 04, 32 GET wrk_anopro  ;
            RANGE 1900,9999  ;
            PICTURE '9999' VALID  ;
            post(3) WHEN  ;
            antes(3)
          READ CYCLE
          IF LASTKEY() = 27
               CLEAR
               DEACTIVATE WINDOW  ;
                          detalle
               LOOP
          ENDIF
          ACTIVATE WINDOW meses
          DO panta
          SET CURSOR OFF
          DO WHILE LASTKEY()<>27
               = INKEY(0)
          ENDDO
          SET CURSOR ON
     ENDDO
ENDDO
*
PROCEDURE antes
PARAMETER opc
ON KEY
DO CASE
     CASE opc = 1
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW detalle
          DO CASE
               CASE wrk_pad = 'A'
                    ON KEY LABEL F6 DO;
PRODUC WITH WRK_CAMPO,WRK_SELEC,WRK_SELPRO;

               CASE wrk_pad = 'B'
                    ON KEY LABEL F6 DO;
ENTIDAD WITH WRK_TIPER,WRK_VAR 
               CASE wrk_pad = 'C'
                    ON KEY LABEL F6 DO;
ENTIDAD WITH WRK_TIPER,WRK_VAR 
               CASE wrk_pad = 'D'
                    ON KEY LABEL F6 DO;
BUSCA WITH WRK_BUSCA,WRK_VAR 
               CASE wrk_pad = 'E'
                    ON KEY LABEL F6 DO;
ENTIDAD WITH WRK_TIPER,WRK_VAR 
          ENDCASE
     CASE opc = 2
          wrk_busca = 'EMIS'
          ON KEY LABEL F6 DO BUSCA WITH;
WRK_BUSCA,WRK_VAR2 
     CASE opc = 3
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW detalle
ENDCASE
*
FUNCTION post
PARAMETER opc
DO CASE
     CASE opc = 1
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .F.
          ENDIF
          IF EMPTY(wrk_codigo)
               DO p_mensaje WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          DO CASE
               CASE wrk_pad = 'A'
                    SELECT 1
                    SEEK wrk_codigo
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Cขdigo de Producto No Existe'
                         RETURN .F.
                    ENDIF
                    @ 02, 47 SAY  ;
                      SUBSTR(pro_descri,  ;
                      1, 24)
               CASE wrk_pad = 'B'
                    SELECT 3
                    SEEK 'C' +  ;
                         wrk_codigo
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Cขdigo de Cliente No Existe'
                         RETURN .F.
                    ENDIF
                    @ 02, 47 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 24)
               CASE wrk_pad = 'C'
                    SELECT 4
                    SEEK 'V' +  ;
                         wrk_codigo
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Cขdigo de Vendedor No Existe'
                         RETURN .F.
                    ENDIF
                    @ 02, 47 SAY  ;
                      SUBSTR(vnd_nombre,  ;
                      1, 24)
               CASE wrk_pad = 'D'
                    SELECT 2
                    SEEK 'FPAG' +  ;
                         wrk_codigo
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Cขdigo de Forma de Pago No Existe'
                         RETURN .F.
                    ENDIF
                    @ 02, 47 SAY  ;
                      SUBSTR(tab_destab,  ;
                      1, 24)
               CASE wrk_pad = 'E'
                    SELECT 3
                    SEEK 'P' +  ;
                         wrk_codigo
                    IF  .NOT.  ;
                        FOUND()
                         DO p_mensaje  ;
                            WITH  ;
                            'Cขdigo de Proveedor No Existe'
                         RETURN .F.
                    ENDIF
                    @ 02, 47 SAY  ;
                      SUBSTR(cli_razsoc,  ;
                      1, 24)
          ENDCASE
     CASE opc = 2
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF wrk_estado = 'V'
               IF EMPTY(wrk_emisor)
                    RETURN .T.
               ENDIF
               SELECT 2
               SEEK 'EMIS' +  ;
                    wrk_emisor
               IF  .NOT. FOUND()
                    DO p_mensaje  ;
                       WITH  ;
                       'Cขdigo de Emisor No Existe'
                    RETURN .F.
               ENDIF
               @ 03, 37 SAY  ;
                 tab_destab
               RETURN .T.
          ELSE
               @ 03, 32 SAY  ;
                 SPACE(20)
          ENDIF
     CASE opc = 3
          IF LASTKEY() = 5 .OR.  ;
             LASTKEY() = 19
               RETURN .T.
          ENDIF
          IF EMPTY(wrk_anopro)
               DO p_mensaje WITH  ;
                  'No se Aceptan Blancos'
               RETURN .F.
          ENDIF
          CLEAR READ
ENDCASE
*
PROCEDURE panta
@ 00, 00 SAY  ;
  'ีอMESอัอออออFISICOอออออออMONETARIOอออหอMESอัอออออFISICOอออออออMONETARIOออธ'
@ 01, 00 SAY  ;
  'ณ Ene ณ                              บ Jul ณ                             ณ'
@ 02, 00 SAY  ;
  'ณ Feb ณ                              บ Ago ณ                             ณ'
@ 03, 00 SAY  ;
  'ณ Mar ณ                              บ Set ณ                             ณ'
@ 04, 00 SAY  ;
  'ณ Abr ณ                              บ Oct ณ                             ณ'
@ 05, 00 SAY  ;
  'ณ May ณ                              บ Nov ณ                             ณ'
@ 06, 00 SAY  ;
  'ณ Jun ณ                              บ Dic ณ                             ณ'
@ 07, 00 SAY  ;
  'รฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤืฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด'
@ 08, 00 SAY  ;
  'ณTotalณ                              บTotalณ                             ณ'
@ 09, 00 SAY  ;
  'ณ     ณ            TOTAL GENERAL ====บ====>ณ                             ณ'
@ 10, 00 SAY  ;
  'ิอออออฯออออออออออออออออออออออออออออออสอออออฯอออออออออออออออออออออออออออออพ'
SELECT 5
IF wrk_estado = 'V' .AND.  ;
   wrk_emisor <> SPACE(4)
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 1;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_enero,wrk_enecan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 2;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_febre,wrk_febcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 3;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_marzo,wrk_marcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 4;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_abril,wrk_abrcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 5;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_mayo,wrk_maycan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 6;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_junio,wrk_juncan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 7;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_julio,wrk_julcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 8;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_agosto,wrk_agocan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 9;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_seti,wrk_setcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 10;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_octu,wrk_octcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 11;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_novi,wrk_novcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 12;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro;
.AND. est_punemi = wrk_emisor  TO;
 wrk_dici,wrk_diccan
ELSE
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 1;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_enero,wrk_enecan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 2;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_febre,wrk_febcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 3;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_marzo,wrk_marcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 4;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_abril,wrk_abrcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 5;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_mayo,wrk_maycan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 6;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_junio,wrk_juncan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 7;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_julio,wrk_julcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 8;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_agosto,wrk_agocan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 9;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_seti,wrk_setcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 10;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_octu,wrk_octcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 11;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_novi,wrk_novcan
     SUM est_totmto,est_totcan;
 FOR est_mesdoc = 12;
.AND. est_indest = wrk_estado;
.AND. &wrk_tipo = wrk_codigo ;
.AND. est_anodoc = wrk_anopro  TO;
 wrk_dici,wrk_diccan
ENDIF
wrk_tot01 = wrk_enero + wrk_febre +  ;
            wrk_marzo + wrk_abril +  ;
            wrk_mayo + wrk_junio
wrk_totca1 = wrk_enecan +  ;
             wrk_febcan +  ;
             wrk_marcan +  ;
             wrk_abrcan +  ;
             wrk_maycan +  ;
             wrk_juncan
wrk_tot02 = wrk_julio +  ;
            wrk_agosto + wrk_seti +  ;
            wrk_octu + wrk_novi +  ;
            wrk_dici
wrk_totca2 = wrk_julcan +  ;
             wrk_agocan +  ;
             wrk_setcan +  ;
             wrk_octcan +  ;
             wrk_novcan +  ;
             wrk_diccan
wrk_totgen = wrk_tot01 +  ;
             wrk_tot02
wrk_totcan = wrk_totca1 +  ;
             wrk_totca2
@ 01, 22 SAY wrk_enero
@ 01, 08 SAY wrk_enecan
@ 02, 22 SAY wrk_febre
@ 02, 08 SAY wrk_febcan
@ 03, 22 SAY wrk_marzo
@ 03, 08 SAY wrk_marcan
@ 04, 22 SAY wrk_abril
@ 04, 08 SAY wrk_abrcan
@ 05, 22 SAY wrk_mayo
@ 05, 08 SAY wrk_maycan
@ 06, 22 SAY wrk_junio
@ 06, 08 SAY wrk_juncan
@ 08, 22 SAY wrk_tot01
@ 08, 08 SAY wrk_totca1
@ 01, 58 SAY wrk_julio
@ 01, 45 SAY wrk_julcan
@ 02, 58 SAY wrk_agosto
@ 02, 45 SAY wrk_agocan
@ 03, 58 SAY wrk_seti
@ 03, 45 SAY wrk_setcan
@ 04, 58 SAY wrk_octu
@ 04, 45 SAY wrk_octcan
@ 05, 58 SAY wrk_novi
@ 05, 45 SAY wrk_novcan
@ 06, 58 SAY wrk_dici
@ 06, 45 SAY wrk_diccan
@ 08, 58 SAY wrk_tot02
@ 08, 45 SAY wrk_totca2
@ 09, 58 SAY wrk_totgen
@ 09, 45 SAY wrk_totcan
*
*** 
*** ReFox - retrace your steps ... 
***
