*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLOSE DATABASES
ON KEY
DIMENSION arraypedid( 1)
DEFINE WINDOW winpedido FROM 04,  ;
       01 TO 21, 78 NOFLOAT  ;
       NOCLOSE NOMINIMIZE NONE  ;
       COLOR SCHEME 1
STORE 0 TO w_salir, w_ind, w_show
arraypedid = SPACE(70)
ACTIVATE WINDOW tablas
DO p_prestab WITH 'PROCESO',  ;
   'ANULACION DE PEDIDOS',  ;
   'ANULAR'
@ 02, 63 SAY PROGRAM()
SELECT 1
USE SHARED gc_hco00 ORDER 1
SELECT 2
USE SHARED gc_dco00 ORDER 1
SELECT 3
USE SHARED gc_tpe00 ORDER 1
SELECT 4
USE SHARED gc_pro00 ORDER 1
SELECT 5
USE SHARED gc_alm00 ORDER 1
SELECT 6
USE SHARED gc_cli00 ORDER 1
SELECT 7
USE SHARED ge_tab0 ORDER 1
= ooscreen(2)
SELECT gc_hco00
SCATTER BLANK MEMVAR
DO WHILE w_salir=0
     @ 00, 28 GET m.hco_nrodoc  ;
       PICTURE '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(4) COLOR SCHEME 12
     @ 00, 69 GET m.hco_fecdoc  ;
       PICTURE '@D' DISABLE COLOR  ;
       SCHEME 12
     @ 01, 28 GET m.hco_codmon  ;
       PICTURE '@!' DISABLE VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 12
     @ 01, 69 GET m.hco_fecmon  ;
       PICTURE '@D' DISABLE COLOR  ;
       SCHEME 12
     @ 02, 28 GET m.hco_codtra  ;
       PICTURE '@!' DISABLE VALID  ;
       oovalid(VARREAD()) COLOR  ;
       SCHEME 12
     @ 02, 69 GET m.hco_feclle  ;
       PICTURE '@D' DISABLE COLOR  ;
       SCHEME 12
     @ 04, 00 GET getpedido  ;
       DEFAULT arraypedid(1) SIZE  ;
       11, 78 FROM arraypedid  ;
       WHEN oopasar() COLOR  ;
       SCHEME 12
     @ 17, 00 GET m.hco_totnet  ;
       PICTURE '9,999,999.99'  ;
       DISABLE COLOR SCHEME 12
     @ 17, 28 GET m.hco_pordes  ;
       PICTURE '99.99' DISABLE  ;
       COLOR SCHEME 12
     @ 17, 45 GET m.hco_totdes  ;
       PICTURE '999,999.99'  ;
       DISABLE COLOR SCHEME 12
     @ 17, 63 GET m.hco_totgen  ;
       PICTURE '9,999,999.99'  ;
       DISABLE COLOR SCHEME 12
     READ CYCLE
     IF LASTKEY() = 27
          IF w_show = 0
               CLEAR READ
               w_salir = 1
          ELSE
               STORE 0 TO w_show,  ;
                     w_salir
               = ooscreen(2)
               SELECT gc_hco00
               SCATTER BLANK  ;
                       MEMVAR
               arraypedid = SPACE(30)
               DIMENSION arraypedid(  ;
                         1)
          ENDIF
     ENDIF
ENDDO
CLOSE DATABASES
ON KEY
RELEASE WINDOW winpedido
DEACTIVATE WINDOW tablas
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'HCO_NRODOC'
          IF EMPTY(m.hco_nrodoc)
               DO p_mensaje WITH  ;
                  ' ES NECESARIO NUMERO DE DOCUMENTO '
               RETURN .F.
          ELSE
               SELECT gc_hco00
               IF  .NOT.  ;
                   SEEK(m.hco_nrodoc)
                    DO p_mensaje  ;
                       WITH  ;
                       ' NUMERO DE DOCUMENTO NO EXISTE '
                    RETURN .F.
               ELSE
                    SCATTER MEMVAR
                    IF hco_indest =  ;
                       'P'
                         DO p_mensaje  ;
                            WITH  ;
                            ' PEDIDO EN PROCESO '
                         RETURN .F.
                    ELSE
                         IF hco_indest =  ;
                            'A'
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' PEDIDO SE ENCUENTRA ANULADO'
                              RETURN  ;
                               .F.
                         ENDIF
                         IF hco_indest =  ;
                            'C'
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' PEDIDO SE ENCUENTRA CERRADO'
                              RETURN  ;
                               .F.
                         ENDIF
                         IF hco_indest =  ;
                            'R'
                              DO p_mensaje  ;
                                 WITH  ;
                                 ' PEDIDO SE ENCUENTRA EN REPROCESO'
                              RETURN  ;
                               .F.
                         ENDIF
                    ENDIF
                    = oovalid('HCO_CODMON')
                    = oovalid('HCO_CODTRA')
                    = oocargaped(m.hco_nrodoc)
               ENDIF
          ENDIF
     CASE cvalid = 'HCO_CODMON'
          SELECT ge_tab0
          IF  .NOT. SEEK('MONE' +  ;
              m.hco_codmon)
               DO p_mensaje WITH  ;
                  ' CODIGO DE MONEDA NO VALIDO '
               RETURN .F.
          ELSE
               @ 01, 33 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 13)
          ENDIF
     CASE cvalid = 'HCO_CODTRA'
          SELECT ge_tab0
          IF  .NOT. SEEK('VTRA' +  ;
              m.hco_codtra)
               DO p_mensaje WITH  ;
                  ' VIA TRANSPORTE NO VALIDO '
               RETURN .F.
          ELSE
               @ 02, 33 SAY  ;
                 SUBSTR(tab_destab,  ;
                 1, 13)
          ENDIF
ENDCASE
RETURN
*
PROCEDURE oowhen
PARAMETER nwhen
IF nwhen = 4
     DO p_footer WITH  ;
        '100010000000000000001',  ;
        2
     ON KEY LABEL f6 do ooverorden with;
"M.HCO_NRODOC"
ENDIF
RETURN
*
PROCEDURE oopasar
ON KEY LABEL f6
ON KEY LABEL f4 do oograbar with 1
ON KEY LABEL f7 do oograbar with 2
RETURN
*
PROCEDURE oocargaped
PARAMETER aux_nrodoc
DO p_footer WITH  ;
   '100101000000000000001', 2
STORE 1 TO w_ind, w_show
SELECT gc_dco00
SEEK aux_nrodoc
SCAN WHILE dco_nrodoc =  ;
     aux_nrodoc .AND.  .NOT.  ;
     EOF()
     DIMENSION arraypedid( w_ind)
     SELECT gc_pro00
     SEEK gc_dco00.dco_codprp
     IF FOUND()
          w_datx = pro_codpro +  ;
                   '³' +  ;
                   SUBSTR(pro_descri,  ;
                   1, 21) + '³' +  ;
                   pro_unimed +  ;
                   '³'
     ELSE
          w_datx = ' >>>  C¢digo No Existe  <<< ' +  ;
                   SPACE(20)
     ENDIF
     w_datx = w_datx +  ;
              TRANSFORM(gc_dco00.dco_cansol,  ;
              '999,999') + '³' +  ;
              TRANSFORM(gc_dco00.dco_valpre,  ;
              '999,999.99')
     arraypedid( w_ind) = w_datx
     w_ind = w_ind + 1
     SELECT gc_dco00
ENDSCAN
getpedido = arraypedid(1)
SHOW GETS
SHOW GET getpedido ENABLE
RETURN
*
PROCEDURE oograbar
PARAMETER ngrabar
= ooobserva()
IF LASTKEY() = 27
     RETURN
ENDIF
IF ngrabar = 2
     DO ooprint
ENDIF
SELECT gc_hco00
SEEK m.hco_nrodoc
IF FOUND()
     SELECT gc_dco00
     SEEK gc_hco00.hco_nrodoc
     SCAN WHILE dco_nrodoc =  ;
          gc_hco00.hco_nrodoc  ;
          .AND.  .NOT. EOF()
          IF dco_indest = 'U'
               DO rbloquea
               DELETE
               UNLOCK
          ELSE
               SELECT gc_pro00
               SEEK gc_dco00.dco_codprp
               IF FOUND()
                    DO rbloquea
                    REPLACE pro_stksol  ;
                            WITH  ;
                            pro_stksol -  ;
                            gc_dco00.dco_cansol,  ;
                            pro_usuari  ;
                            WITH  ;
                            clave,  ;
                            pro_fecha  ;
                            WITH  ;
                            DATE(),  ;
                            pro_hora  ;
                            WITH  ;
                            TIME()
                    UNLOCK
               ENDIF
               SELECT gc_dco00
               DO rbloquea
               REPLACE dco_indest  ;
                       WITH 'A',  ;
                       dco_usuari  ;
                       WITH clave,  ;
                       dco_fecha  ;
                       WITH  ;
                       DATE(),  ;
                       dco_hora  ;
                       WITH  ;
                       TIME()
               UNLOCK
          ENDIF
     ENDSCAN
     SELECT gc_hco00
     IF hco_indest = 'U'
          DO rbloquea
          DELETE
          UNLOCK
     ELSE
          DO rbloquea
          REPLACE hco_indest WITH  ;
                  'A', hco_usuari  ;
                  WITH clave,  ;
                  hco_fecha WITH  ;
                  DATE(),  ;
                  hco_hora WITH  ;
                  TIME()
          UNLOCK
     ENDIF
ENDIF
DO p_mensaje WITH  ;
   ' DATOS DEL PEDIDO '+ ;
   m.hco_nrodoc+' ANULADOS '
= ooscreen(2)
SELECT gc_hco00
SCATTER BLANK MEMVAR
arraypedid = SPACE(30)
DIMENSION arraypedid( 1)
CLEAR READ
RETURN
*
PROCEDURE ooobserva
DEFINE WINDOW mensaje FROM  ;
       INT((SROWS() - 7) / 2),  ;
       INT((SCOLS() - 65) / 2) TO  ;
       INT((SROWS() - 7) / 2) + 6,  ;
       INT((SCOLS() - 65) / 2) +  ;
       64 NOFLOAT NOCLOSE TITLE  ;
       ' Observaci¢n del Pedido'  ;
       NOMINIMIZE COLOR SCHEME 1
ACTIVATE WINDOW SAME mensaje
@ 01, 02 GET m.hco_obser DEFAULT  ;
  ' ' SIZE 1, 60 PICTURE '@!'
@ 03, 25 GET nop DEFAULT 1 SIZE 1,  ;
  11, 1 PICTURE '@*HN Continuar'  ;
  VALID confirma()
READ CYCLE
RELEASE WINDOW mensaje
*
PROCEDURE confirma
CLEAR READ
RETURN
RETURN
*
PROCEDURE ooscreen
PARAMETER w_screen
DO CASE
     CASE w_screen = 2
          ACTIVATE WINDOW SAME  ;
                   winpedido
          CLEAR
          @ 00, 01 SAY  ;
            'Nro. Documento.......... :'
          @ 00, 47 SAY  ;
            'Fecha Documento.... :'
          @ 01, 01 SAY  ;
            'C¢digo Moneda........... :'
          @ 01, 47 SAY  ;
            'Fecha Paridad...... :'
          @ 02, 01 SAY  ;
            'V¡a Transporte.......... :'
          @ 02, 47 SAY  ;
            'Fecha Llegada...... :'
          @ 03, 00 SAY  ;
            '  CODIGO           DESCRIPCION         UNID S.SOLICI    PRECIO               '  ;
            COLOR N/W 
          @ 16, 00 SAY  ;
            '  TOTAL NETO           %DESCUENTO           TOTAL DSCTO       TOTAL GENERAL   '  ;
            COLOR N/W 
ENDCASE
RETURN
*
PROCEDURE ooprint
SELECT gc_dco00
SEEK gc_hco00.hco_nrodoc
SET RELATION TO dco_codprp INTO gc_pro00;
ADDITIVE
REPORT FORMAT agcp2214 TO PRINTER  ;
       NOCONSOLE FOR dco_nrodoc =  ;
       gc_hco00.hco_nrodoc
SET RELATION TO
SELECT gc_hco00
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
