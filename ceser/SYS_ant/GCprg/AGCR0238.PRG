*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
ON KEY
SET CURSOR ON
CLOSE DATABASES
SET CENTURY ON
DEFINE WINDOW cabecera FROM 04,  ;
       02 TO 09, 77 COLOR SCHEME  ;
       7
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 TITLE  ;
       ' PENDIENTES POR FACTURAR '  ;
       COLOR SCHEME 7
SELECT 1
USE SHARED ge_tab0 ORDER codigo
SELECT 2
USE SHARED gc_pro00 ORDER codigo
SELECT 3
USE SHARED st_iorep ORDER codigo
ACTIVATE WINDOW tablas
DO p_prestab WITH 'REPORTES',  ;
   'PENDIENTES X FACTURAR',  ;
   'REPORTE'
@ 02, 63 SAY PROGRAM()
w_program = PROGRAM()
ACTIVATE WINDOW detalle
STORE 'P' TO w_proini, w_profin
STORE SPACE(1) TO w_fil, w_dire
DO WHILE .T.
     ACTIVATE WINDOW cabecera
     STORE 1 TO w_copia
     CLEAR
     @ 00, 01 SAY 'Propiedad:'
     @ 01, 01 SAY 'Del:'
     @ 02, 01 SAY 'Al :'
     @ 01, 06 GET w_proini SIZE 1,  ;
       4, 0 FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 02, 06 GET w_profin SIZE 1,  ;
       4, 0 RANGE w_proini  ;
       FUNCTION '@!' VALID  ;
       oovalid(VARREAD()) WHEN  ;
       oowhen(VARREAD()) COLOR  ;
       SCHEME 8
     @ 00, 26 SAY  ;
       'Tipo de Informe:'
     @ 01, 26 GET nop1 DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVN Resumen ;Detalle'  ;
       WHEN oowhen(VARREAD())  ;
       COLOR SCHEME 24
     @ 00, 49 SAY  ;
       'Tipo de Salida:'
     @ 01, 49 GET opc DEFAULT 1  ;
       SIZE 1, 7, 0 PICTURE  ;
       '@*RVTN Pantalla ;Impresora ;Archivo'  ;
       COLOR SCHEME 24
     READ CYCLE
     IF LASTKEY() = 27
          EXIT
     ENDIF
     IF opc = 2
          @ 02, 63 SAY 'Copias:'
          @ 02, 71 GET w_copia  ;
            PICTURE '99' VALID   ;
            .NOT. EMPTY(w_copia)  ;
            .AND. w_copia > 0  ;
            .AND. w_copia < 100  ;
            COLOR SCHEME 8
          READ
          IF LASTKEY() = 27
               @ 01, 47 SAY  ;
                 SPACE(20)
               LOOP
          ENDIF
     ENDIF
     DO procesa
     COUNT TO nreg
     IF nreg = 0
          RELEASE WINDOW  ;
                  winmensaje
          DO p_mensaje WITH  ;
             ' NO SE ENCONTRO INFORMACION '
          SELECT 20
          USE
          erase &w_fil
          LOOP
     ELSE
          SET RELATION TO codpro INTO;
gc_pro00
     ENDIF
     = ooimprime(opc)
ENDDO
RELEASE WINDOW detalle, cabecera
DEACTIVATE WINDOW tablas
ON KEY
CLOSE DATABASES
DO p_footer WITH  ;
   '100000000001011000001', 1
ACTIVATE SCREEN
RETURN
*
PROCEDURE oowhen
PARAMETER cwhen
DO CASE
     CASE cwhen = 'W_PROINI'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_PROINI'
          ON KEY LABEL f6 do busca2 with;
"PROP",wrk_var,'PROPIEDAD',2
     CASE cwhen = 'W_PROFIN'
          DO p_footer WITH  ;
             '100010000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
          wrk_var = 'W_PROFIN'
          ON KEY LABEL f6 do busca2 with;
"PROP",wrk_var,'PROPIEDAD',2
     CASE cwhen = 'NOP1'
          ON KEY
          DO p_footer WITH  ;
             '100000000000000000001',  ;
             2
          ACTIVATE WINDOW  ;
                   cabecera
ENDCASE
RETURN
*
FUNCTION oovalid
PARAMETER cvalid
DO CASE
     CASE cvalid = 'W_PROINI'  ;
          .OR. cvalid =  ;
          'W_PROFIN'
          IF cvalid = 'W_PROFIN'
               IF w_profin <  ;
                  w_proini
                    RETURN .F.
               ENDIF
          ENDIF
          SELECT ge_tab0
          IF cvalid = 'W_PROINI'
               SEEK 'PROP' +  ;
                    w_proini
          ELSE
               SEEK 'PROP' +  ;
                    w_profin
          ENDIF
          IF  .NOT. FOUND()
               DO p_mensaje WITH  ;
                  'C¢digo no Existe'
               RETURN .F.
          ENDIF
          @ ROW(), 11 SAY  ;
            SUBSTR(tab_destab, 1,  ;
            13)
ENDCASE
RETURN
*
PROCEDURE procesa
= ooaviso( ;
  'P R O C E S A N D O . . .')
SELECT st_iorep.numdoc,  ;
       st_iorep.fecemi,  ;
       st_iorep.indori,  ;
       st_iorep.indest,  ;
       st_iorep.auxest,  ;
       st_iorep.codmod,  ;
       st_iorep.numsol,  ;
       st_iorep.codemi,  ;
       st_iorep.cosrep,  ;
       st_iorep.cosmob,  ;
       st_iprep.numdoc,  ;
       st_iprep.fecemi,  ;
       st_iprep.indest,  ;
       st_idped.codpro,  ;
       st_idped.canpro,  ;
       st_idped.valpro,  ;
       st_idped.totite,  ;
       gc_pro00.pro_propie,  ;
       gc_pro00.pro_ultcom FROM  ;
       st_iorep, st_iprep,  ;
       st_idped, gc_pro00 WHERE  ;
       st_iprep.numord =  ;
       st_iorep.numdoc AND  ;
       st_idped.numdoc =  ;
       st_iprep.numdoc AND  ;
       st_idped.codpro =  ;
       gc_pro00.pro_codpro AND  ;
       YEAR(st_iorep.fecemi) >  ;
       1994 AND st_iorep.auxest <>  ;
       '024 ' AND st_iorep.auxest <>  ;
       '025 ' AND st_iorep.indest <>  ;
       'N' AND st_iorep.indest <>  ;
       'B' AND st_iorep.indest <>  ;
       'F' AND st_iprep.indest <>  ;
       'F' AND st_iprep.indest <>  ;
       'B' AND st_iprep.indest <>  ;
       'N' AND st_idped.canpro >  ;
       0 AND  ;
       BETWEEN(gc_pro00.pro_propie,  ;
       w_proini, w_profin) ORDER  ;
       BY st_iorep.codemi,  ;
       st_iorep.numdoc,  ;
       st_iprep.numdoc INTO  ;
       CURSOR query
w_fil = SUBSTR(f_archivo(), 1, 8) +  ;
        '.DBF'
copy to &w_fil
USE
SELECT 20
use '&w_fil'
DELETE ALL FOR (auxest = '022 '  ;
       .AND. cosrep = 0 .AND.  ;
       (SUBSTR(indori, 1, 1) =  ;
       'G' .OR. SUBSTR(indori, 1,  ;
       1) = 'P'))
DELETE ALL FOR (auxest = '023 '  ;
       .AND. cosrep = 0 .AND.  ;
       (SUBSTR(indori, 1, 1) =  ;
       'G' .OR. SUBSTR(indori, 1,  ;
       1) = 'P'))
DELETE ALL FOR (auxest = '028 '  ;
       .AND. cosrep = 0 .AND.  ;
       (SUBSTR(indori, 1, 1) =  ;
       'G' .OR. SUBSTR(indori, 1,  ;
       1) = 'P'))
DELETE ALL FOR (auxest = '029 '  ;
       .AND. cosrep = 0 .AND.  ;
       (SUBSTR(indori, 1, 1) =  ;
       'G' .OR. SUBSTR(indori, 1,  ;
       1) = 'P'))
RETURN
*
PROCEDURE ooimprime
PARAMETER opc
GOTO TOP
DO CASE
     CASE opc = 1
          w_txt = f_texto()
          = ooaviso( ;
            'C  O  P  I  A  N  D  O . . .' ;
            )
          IF nop1 = 1
               repo form agcr238a to file;
&w_txt noconsole summary
          ELSE
               repo form agcr0238 to file;
&w_txt noconsole
          ENDIF
          SET SYSMENU ON
          modi comm &w_txt wind detalle;
noedit
          SET SYSMENU OFF
          erase &w_txt
          DEACTIVATE WINDOW  ;
                     winmensaje
          ACTIVATE WINDOW detalle
          CLEAR
     CASE opc = 2
          DO WHILE .T.
               IF SYS(13) =  ;
                  'OFFLINE'
                    swt = f_yesno( ;
                          'Impresora fuera de Linea. Intentar nuevamente ' ;
                          )
                    IF swt
                         LOOP
                    ELSE
                         DEACTIVATE  ;
                          WINDOW  ;
                          winmensaje
                         RETURN
                    ENDIF
               ELSE
                    EXIT
               ENDIF
          ENDDO
          = ooaviso( ;
            'I M P R I M I E N D O . . .' ;
            )
          SET PRINTER ON
          SET DEVICE TO PRINTER
          @ 0, 0 SAY CHR(27) +  ;
            CHR(15)
          FOR a = 1 TO w_copia
               IF nop1 = 1
                    REPORT FORMAT  ;
                           agcr238a  ;
                           SUMMARY  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ELSE
                    REPORT FORMAT  ;
                           agcr0238  ;
                           TO  ;
                           PRINTER  ;
                           NOCONSOLE
               ENDIF
          ENDFOR
          SET PRINTER TO
          SET PRINTER OFF
          SET DEVICE TO SCREEN
          RELEASE WINDOW  ;
                  winmensaje
          DO p_mensaje WITH  ;
             ' FIN DE IMPRESION '
     CASE opc = 3
          = ooruta()
          = ooaviso( ;
            'C  O  P  I  A  N  D  O . . .' ;
            )
          SELECT 20
          IF nop1 = 1
               repo form agcr238a to file;
&w_dire noconsole summary
          ELSE
               repo form agcr0238 to file;
&w_dire noconsole
          ENDIF
          DEACTIVATE WINDOW  ;
                     winmensaje
          ACTIVATE WINDOW detalle
          CLEAR
ENDCASE
SELECT 20
USE
erase &w_fil
RETURN
*
PROCEDURE ooruta
SELECT ge_tab0
SEEK 'RUTA' + 'COMM'
IF FOUND()
     w_ruta = tab_destab
ELSE
     w_ruta = SPACE(30)
ENDIF
w_ruta = ALLTRIM(w_ruta)
w_dire = w_ruta + 'PF' +  ;
         ALLTRIM(STR(MONTH(DATE()))) +  ;
         rge_codalm + '.TXT'
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
