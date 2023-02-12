*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
CLEAR
CLEAR ALL
CLEAR MACROS
CLOSE DATABASES
SET EXCLUSIVE OFF
SET ESCAPE OFF
SET SAFETY OFF
SET TALK OFF
SET DATE BRITI
SET DELETED ON
SET BELL OFF
SET UNIQUE ON
SET CURSOR ON
SET CENTURY ON
SET PATH TO DATA:BASES
SET PROCEDURE TO FUNCIONE
DEFINE WINDOW titulo FROM 01, 03  ;
       TO 03, 76 GROW FLOAT CLOSE  ;
       ZOOM SHADOW IN screen  ;
       COLOR SCHEME 5
DEFINE WINDOW detalle FROM 05, 03  ;
       TO 22, 76 GROW FLOAT CLOSE  ;
       ZOOM SHADOW IN screen
DEFINE WINDOW mensaje FROM 22, 20  ;
       TO 24, 62 IN screen COLOR  ;
       SCHEME 10
ACTIVATE WINDOW titulo
@ 00, 01 SAY 'CESER'
@ 00, 26 SAY  ;
  'CONSULTA DE SERVICIOS'
@ 00, 61 SAY DATE()
rge_monbas = 'SOL '
w_facigv = 0.18 
SELECT 1
USE ST_IOREP ORDER CODIGO
SELECT 2
USE ST_ISREP ORDER CODIGO
SELECT 3
USE GC_HVE00 ORDER NRDORE
SELECT 4
USE gc_cmv00 ORDER cmv_feinmo
SELECT 5
USE ST_ISPRE ORDER PRE_NUMORD
ACTIVATE WINDOW detalle
DO WHILE .T.
     CLEAR
     STORE 0 TO w_numero, w_saldo
     @ 01, 02 SAY  ;
       'No. de Servicio :'
     @ 03, 02 SAY  ;
       'Estado          :'
     @ 05, 02 SAY  ;
       'Monto (Saldo)   :'
     @ 07, 02 SAY  ;
       'Informe T‚cnico :'
     @ 13, 00 TO 13, 71
     @ 01, 20 GET w_numero  ;
       PICTURE '99999999'
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
     SELECT st_iorep
     SEEK STR(w_numero, 8)
     IF  .NOT. FOUND()
          ACTIVATE WINDOW mensaje
          ? CHR(7)
          @ 00, 00 SAY  ;
            '  <<< Nro. de Servicio No Existe >>>  '
          = INKEY(0, 'H')
          DEACTIVATE WINDOW  ;
                     mensaje
          LOOP
     ENDIF
     w_numord = STR(w_numero, 8)
     w_numsol = numsol
     w_estado = auxest
     w_auxest = auxest
     w_codcli = codent
     w_marca = codmar
     w_modelo = codmod
     w_serie = numser
     w_tipgar = indori
     w_cosmob = cosmob
     w_cosrep = cosrep
     w_flete = flete
     w_totdes = totdes
     w_desrep = desrep
     w_desmob = desmob
     w_infor1 = SUBSTR(observ,  ;
                001, 50)
     w_infor2 = SUBSTR(observ,  ;
                051, 50)
     w_infor3 = SUBSTR(observ,  ;
                101, 50)
     w_infor4 = SUBSTR(observ,  ;
                151, 50)
     w_infor5 = SUBSTR(observ,  ;
                201, 50)
     w_infor6 = SUBSTR(observ,  ;
                251, 50)
     SELECT st_isrep
     SEEK w_numsol
     w_indest = indest
     @ 03, 20 SAY ootab('ESOR', ;
       w_estado) COLOR GR+/B 
     @ 05, 20 SAY w_saldo COLOR  ;
       GR+/B 
     @ 07, 20 SAY w_infor1 COLOR  ;
       GR+/B 
     @ 08, 20 SAY w_infor2 COLOR  ;
       GR+/B 
     @ 09, 20 SAY w_infor3 COLOR  ;
       GR+/B 
     @ 10, 20 SAY w_infor4 COLOR  ;
       GR+/B 
     @ 11, 20 SAY w_infor5 COLOR  ;
       GR+/B 
     @ 12, 20 SAY w_infor6 COLOR  ;
       GR+/B 
     IF w_estado = '010 ' .AND.  ;
        (w_tipgar = 'FGAR' .OR.  ;
        w_tipgar = 'FREC')
          SELECT gc_hve00
          STORE 0 TO wrk_toacta,  ;
                i, cane, w_tipca2,  ;
                s_cosfle,  ;
                s_totgen
          STORE 0 TO i, i2,  ;
                s_cosrep,  ;
                s_cosmob,  ;
                s_cosfle,  ;
                s_toacta, s_total,  ;
                s_totpag,  ;
                s_descue
          SELECT gc_cmv00
          w_tipcam = ootc2(DATE(), ;
                     'SOL ', ;
                     'DOL ','2')
          DO pormcost WITH  ;
             'st_iorep'
          @ 05, 20 SAY s_totpag  ;
            COLOR GR+/B 
          @ 05, 34 SAY  ;
            'Nuevos Soles'
     ENDIF
     IF w_tipgar = 'GARA' .OR.  ;
        w_tipgar = 'GREC'
          @ 05, 20 SAY  ;
            'EN GARANTIA' COLOR  ;
            GR+/B 
     ENDIF
     IF w_estado = '003 '
          w_totpre = 0
          SELECT st_ispre
          SEEK w_numord
          IF FOUND()
               w_totpre = monrep +  ;
                          monman
          ENDIF
          @ 05, 20 SAY w_totpre  ;
            COLOR GR+/B 
          @ 05, 34 SAY  ;
            'Dolares Americanos'
     ENDIF
     @ 14, 02 SAY  ;
       'Cliente         :'
     @ 15, 02 SAY  ;
       'Art¡culo        :'
     @ 14, 20 SAY  ;
       oodescli(w_codcli) COLOR G+/ ;
       B 
     @ 15, 20 SAY  ;
       SUBSTR(ootab('MARC', ;
       w_marca), 1, 15) + '  ' +  ;
       w_modelo + '  ' +  ;
       SUBSTR(w_serie, 1, 15)  ;
       COLOR G+/B 
     ACTIVATE WINDOW mensaje
     DO CASE
          CASE (w_estado = '010 '  ;
               .OR. w_estado =  ;
               '021 ' .OR.  ;
               w_estado = '080 '  ;
               .OR. w_estado =  ;
               '081 ')
               ? CHR(7)
               @ 00, 00 SAY  ;
                 ' <<< Pase Ud. a Recojer su Artefacto >>> '
          CASE w_estado = '003 '
               ? CHR(7)
               @ 00, 00 SAY  ;
                 '  <<< Agradeceremos Cancelar el 50% >>>  '
          OTHERWISE
               ? CHR(7)
               @ 00, 00 SAY  ;
                 '  <<< M s Informaci¢n en su Counter >>>  '
     ENDCASE
     = INKEY(0, 'H')
     DEACTIVATE WINDOW mensaje
ENDDO
CLOSE DATABASES
DEACTIVATE WINDOW ALL
RELEASE WINDOW all
*
*** 
*** ReFox - retrace your steps ... 
***
