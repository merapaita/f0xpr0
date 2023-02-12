*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
PARAMETER config_prg
SET SYSMENU ON
IF config_prg == 1
     ind_tit = 'MANTENCION'
ELSE
     ind_tit = 'CONSULTA'
ENDIF
PUBLIC sinto, xx, yy
PUBLIC borre, es10
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, ind_tit
DO saycenter WITH 2,  ;
   ' TABLAS DE SINTOMAS '
DO esc_modo WITH 'S'
DO esc_indica WITH 1, 'AYU',  ;
   'INI', 'ANT', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'FIN', 'STE', 'ESC'
campo = 'TAB_CODTAB + "  " + TAB_DESTAB + "  "'
DO usedbf WITH 'ge_tab0',  ;
   'codtab'
DO usedbf WITH 'st_sint',  ;
   'sin_lincod'
SELECT ge_tab0
x = 1
DO mensa WITH 'espere un momento',  ;
   'COLO'
SCAN FOR tab_codpre = 'LINE'
     DIMENSION producto( x)
     producto( x) = ' ' +  ;
             tab_codtab + '  ³' +  ;
             tab_destab
     x = x + 1
ENDSCAN
DEFINE WINDOW producto FROM 05,  ;
       13 TO 16, 63 GROW FLOAT  ;
       CLOSE ZOOM SHADOW TITLE  ;
       ' LINEAS DE PRODUCTOS '
ACTIVATE WINDOW producto
DO mensa WITH 'espere un momento',  ;
   'SACA'
@ 00, 00 SAY  ;
  '  C¢digo  Descripci¢n                             '  ;
  COLOR N/W 
@ 01, 00 GET prod DEFAULT  ;
  producto(1) SIZE 11, 50 FROM  ;
  producto
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     DO saca_win
     CLOSE DATABASES
     RETURN
ENDIF
DEACTIVATE WINDOW producto
DEACTIVATE POPUP producto
descr_prod = ALLTRIM(SUBSTR(prod,  ;
             09, 30))
clave_prod = ALLTRIM(SUBSTR(prod,  ;
             02, 04))
DO mensa WITH 'espere un momento',  ;
   'COLO'
DO mant_sint WITH 1
SELECT st_sint
x = 1
SET NEAR ON
SEEK clave_prod
SET NEAR OFF
IF  .NOT. FOUND()
     DO salida
ENDIF
DO WHILE (linea=clave_prod) .AND.   ;
   .NOT. DELETED()
     DIMENSION sintoma( x)
     sintoma( x) = '  ' + codsin +  ;
            '  ³' + dessin
     x = x + 1
     SKIP
ENDDO
DEFINE WINDOW sintoma FROM 05, 10  ;
       TO 16, 66 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE ' ' +  ;
       clave_prod + ' - ' +  ;
       ALLTRIM(SUBSTR(prod, 09,  ;
       30)) + ' '
ACTIVATE WINDOW sintoma
xx = 2
DO mensa WITH 'espere un momento',  ;
   'SACA'
DO WHILE .T.
     ACTIVATE WINDOW sintoma
     ON KEY LABEL F9 DO MANT_SINT WITH;
XX
     ON KEY LABEL F3 do Ingr_Sint
     @ 0, 0 SAY  ;
       '  C¢digo          S I N T O M A                   '  ;
       COLOR N/W 
     sinto = sintoma(1)
     ON KEY LABEL F4 do Anul_Sint with;
sinto
     ON KEY LABEL Enter do Mod_Sint With;
Sinto
     @ 01, 00 GET sinto DEFAULT  ;
       sintoma(1) SIZE 11, 55  ;
       FROM sintoma
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
ENDDO
DEACTIVATE WINDOW sintoma
DEACTIVATE POPUP sintoma
DO mensa WITH 'espere un momento',  ;
   'SACA'
ON KEY
DO saca_win
CLOSE DATABASES
RETURN
*
PROCEDURE mod_sint
PARAMETER modificado
ON KEY LABEL enter
MOVE WINDOW sintoma TO 4, 14
DEFINE WINDOW modifica FROM 16,  ;
       14 TO 18, 65 SHADOW COLOR  ;
       N/W,N/W,N/W,N/W 
ACTIVATE WINDOW modifica
SELECT st_sint
codigo = SUBSTR(modificado, 3, 4)
SEEK clave_prod + codigo
xcodsin = codsin
xdessin = dessin
@ 0, 12 SAY '³'
@ 0, 06 GET xcodsin PICTURE  ;
  '9999' COLOR W/N,W/N,W/N,W/N,W/ ;
  N 
@ 0, 13 GET xdessin PICTURE '@!'  ;
  COLOR W/N,W/N,W/N,W/N,W/N 
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DEACTIVATE WINDOW modifica
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     MOVE WINDOW sintoma TO 5, 14
     RETURN
ENDIF
xcodsin = (SPACE(4 -  ;
          LEN(ALLTRIM(xcodsin)))) +  ;
          ALLTRIM(xcodsin)
REPLACE codsin WITH xcodsin
REPLACE dessin WITH xdessin
m.date = DATE()
m.time = TIME()
FLUSH
WAIT WINDOW NOWAIT  ;
     ' S¡ntoma Modificado'
DEACTIVATE WINDOW modifica
MOVE WINDOW sintoma TO 5, 14
RETURN
*
PROCEDURE ingr_sint
ON KEY LABEL enter
MOVE WINDOW sintoma TO 4, 14
DEFINE WINDOW ingreso FROM 16, 14  ;
       TO 18, 65 SHADOW COLOR N/W, ;
       N/W,N/W,N/W 
ACTIVATE WINDOW ingreso
SELECT st_sint
SCATTER BLANK MEMVAR
DO WHILE .T.
     @ 0, 05 GET m.codsin PICTURE  ;
       '9999' COLOR W/N 
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW  ;
                     producto
          DEACTIVATE POPUP  ;
                     producto
          DEACTIVATE WINDOW  ;
                     ingreso
          DO mensa WITH  ;
             ' Espere un momento ...',  ;
             'SACA'
          MOVE WINDOW sintoma TO  ;
               5, 141
          RETURN
     ENDIF
     GOTO TOP
     m.codsin = (SPACE(4 -  ;
                LEN(ALLTRIM(m.codsin)))) +  ;
                ALLTRIM(m.codsin)
     clave_prod = ALLTRIM(clave_prod) +  ;
                  (SPACE(4 -  ;
                  LEN(ALLTRIM(clave_prod))))
     SEEK clave_prod + m.codsin
     IF FOUND()
          WAIT WINDOW NOWAIT  ;
               'Codigo Ya Existe '
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
@ 0, 15 GET m.dessin PICTURE '@!'  ;
  COLOR W/N 
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DEACTIVATE WINDOW ingreso
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     MOVE WINDOW sintoma TO 5, 14
     RETURN
ENDIF
m.codcla = clave_prod
m.date = DATE()
m.time = TIME()
APPEND BLANK
GATHER MEMVAR
WAIT WINDOW NOWAIT  ;
     ' S¡ntoma Grabado '
DEACTIVATE WINDOW ingreso
MOVE WINDOW sintoma TO 5, 14
RETURN
*
PROCEDURE anul_sint
PARAMETER anulado
ON KEY LABEL enter
clave_prod = ALLTRIM(clave_prod) +  ;
             (SPACE(4 -  ;
             LEN(ALLTRIM(clave_prod))))
codigo = SUBSTR(anulado, 5, 4)
SEEK clave_prod + codigo
IF FOUND()
     DELETE
     WAIT WINDOW NOWAIT  ;
          ' Sintoma Eliminado  ...'
ENDIF
RETURN
*
FUNCTION mant_sint
PARAMETER ii
IF ii = 1
     DO esc_indica WITH 1, 'ayu',  ;
        'INI', 'ANT', 'OTR'
     DO esc_indica WITH 2, 'bbb',  ;
        'FIN', 'STE', 'ESC'
     ii = 2
ELSE
     DO esc_indica WITH 1, 'MD3',  ;
        'BBB', 'ANT', 'OTR'
     DO esc_indica WITH 2, 'ELI',  ;
        'MBV', 'STE', 'ESC'
     ii = 1
ENDIF
RETURN ii
*
PROCEDURE salida
PARAMETER puerta
IF puerta = 1
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     RETURN
ELSE
     DEACTIVATE WINDOW sintoma
     DEACTIVATE POPUP sintoma
     DO mensa WITH  ;
        'espere un momento',  ;
        'SACA'
ENDIF
ON KEY
RETURN
*
PROCEDURE usedbf
PARAMETER dbf, indice
IF  .NOT. USED(dbf)
     SELECT 0
     use &dbf order &indice shared
ELSE
     SELECT (dbf)
     set order to &indice	
ENDIF
*
PROCEDURE anterior
PARAMETER config_prg
IF config_prg == 1
     ind_tit = 'MANTENCION'
ELSE
     ind_tit = 'CONSULTA'
ENDIF
PUBLIC sinto, xx, yy
PUBLIC borre, es10
wrk_progra = PROGRAM()
DO crea_win
@ 2, 1 SAY DATE()
DO saycenter WITH 1, ind_tit
DO saycenter WITH 2,  ;
   'TABLAS DE SINTOMAS'
DO esc_modo WITH 'S'
DO esc_indica WITH 1, 'AYU',  ;
   'INI', 'ANT', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'FIN', 'STE', 'ESC'
campo = 'TAB_CODTAB + "  " + TAB_DESTAB + "  "'
DO usedbf WITH 'ge_tab0',  ;
   'codtab'
DO usedbf WITH 'st_sint',  ;
   'codigo'
SELECT ge_tab0
x = 1
DO mensa WITH 'espere un momento',  ;
   'COLO'
SCAN FOR tab_codpre = 'CLAS'
     DIMENSION producto( x)
     producto( x) = '    ' +  ;
             tab_codtab + '    ³' +  ;
             LEFT(tab_destab,  ;
             25)
     x = x + 1
ENDSCAN
DEFINE WINDOW producto FROM 05,  ;
       14 TO 16, 65 GROW FLOAT  ;
       CLOSE ZOOM SHADOW TITLE  ;
       ' PRODUCTOS '
ACTIVATE WINDOW producto
DO mensa WITH 'espere un momento',  ;
   'SACA'
@ 0, 0 SAY  ;
  ' C ¢ d i g o          P r o d u c t o                   '  ;
  COLOR N/W 
@ 01, 00 GET prod DEFAULT  ;
  producto(1) SIZE 11, 50 FROM  ;
  producto
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     DO saca_win
     CLOSE DATABASES
     RETURN
ENDIF
DEACTIVATE WINDOW producto
DEACTIVATE POPUP producto
descr_prod = ALLTRIM(SUBSTR(prod,  ;
             14, 25))
clave_prod = ALLTRIM(SUBSTR(prod,  ;
             4, 4))
DO mensa WITH 'espere un momento',  ;
   'COLO'
DO mant_sint WITH 1
SELECT st_sint
x = 1
SEEK clave_prod
IF  .NOT. FOUND()
     DO salida
ENDIF
DO WHILE (codcla=clave_prod)  ;
   .AND.  .NOT. DELETED()
     DIMENSION sintoma( x)
     sintoma( x) = '    ' +  ;
            codsin + '  ³' +  ;
            LEFT(dessin, 43)
     x = x + 1
     SKIP
ENDDO
DEFINE WINDOW sintoma FROM 05, 14  ;
       TO 16, 65 GROW FLOAT CLOSE  ;
       ZOOM SHADOW TITLE ' ' +  ;
       clave_prod + ' - ' +  ;
       ALLTRIM(SUBSTR(prod, 14,  ;
       25)) + ' '
ACTIVATE WINDOW sintoma
xx = 2
DO mensa WITH 'espere un momento',  ;
   'SACA'
DO WHILE .T.
     ACTIVATE WINDOW sintoma
     ON KEY LABEL F9 DO MANT_SINT WITH;
XX
     ON KEY LABEL F3 do Ingr_Sint
     @ 0, 0 SAY  ;
       '  C¢digo          S I N T O M A                   '  ;
       COLOR N/W 
     sinto = sintoma(1)
     ON KEY LABEL F4 do Anul_Sint with;
sinto
     ON KEY LABEL Enter do Mod_Sint With;
Sinto
     @ 01, 00 GET sinto DEFAULT  ;
       sintoma(1) SIZE 11, 50  ;
       FROM sintoma
     READ
     IF LASTKEY() = 27
          EXIT
     ENDIF
ENDDO
DEACTIVATE WINDOW sintoma
DEACTIVATE POPUP sintoma
DO mensa WITH 'espere un momento',  ;
   'SACA'
ON KEY
DO saca_win
CLOSE DATABASES
RETURN
*
PROCEDURE mod_sint
PARAMETER modificado
ON KEY LABEL enter
MOVE WINDOW sintoma TO 4, 14
DEFINE WINDOW modifica FROM 16,  ;
       14 TO 18, 65 SHADOW COLOR  ;
       N/W,N/W,N/W,N/W 
ACTIVATE WINDOW modifica
SELECT st_sint
clave_prod = ALLTRIM(clave_prod) +  ;
             (SPACE(4 -  ;
             LEN(ALLTRIM(clave_prod))))
codigo = SUBSTR(modificado, 5, 4)
SEEK clave_prod + codigo
xcodsin = codsin
xdessin = dessin
@ 0, 12 SAY '³'
@ 0, 06 GET xcodsin PICTURE  ;
  '9999' COLOR W/N,W/N,W/N,W/N,W/ ;
  N 
@ 0, 13 GET xdessin PICTURE '@!'  ;
  COLOR W/N,W/N,W/N,W/N,W/N 
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DEACTIVATE WINDOW modifica
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     MOVE WINDOW sintoma TO 5, 14
     RETURN
ENDIF
xcodsin = (SPACE(4 -  ;
          LEN(ALLTRIM(xcodsin)))) +  ;
          ALLTRIM(xcodsin)
REPLACE codsin WITH xcodsin
REPLACE dessin WITH xdessin
m.date = DATE()
m.time = TIME()
FLUSH
WAIT WINDOW NOWAIT  ;
     ' S¡ntoma Modificado'
DEACTIVATE WINDOW modifica
MOVE WINDOW sintoma TO 5, 14
RETURN
*
PROCEDURE ingr_sint
ON KEY LABEL enter
MOVE WINDOW sintoma TO 4, 14
DEFINE WINDOW ingreso FROM 16, 14  ;
       TO 18, 65 SHADOW COLOR N/W, ;
       N/W,N/W,N/W 
ACTIVATE WINDOW ingreso
SELECT st_sint
SCATTER BLANK MEMVAR
DO WHILE .T.
     @ 0, 05 GET m.codsin PICTURE  ;
       '9999' COLOR W/N 
     READ
     IF LASTKEY() = 27
          DEACTIVATE WINDOW  ;
                     producto
          DEACTIVATE POPUP  ;
                     producto
          DEACTIVATE WINDOW  ;
                     ingreso
          DO mensa WITH  ;
             ' Espere un momento ...',  ;
             'SACA'
          MOVE WINDOW sintoma TO  ;
               5, 141
          RETURN
     ENDIF
     GOTO TOP
     m.codsin = (SPACE(4 -  ;
                LEN(ALLTRIM(m.codsin)))) +  ;
                ALLTRIM(m.codsin)
     clave_prod = ALLTRIM(clave_prod) +  ;
                  (SPACE(4 -  ;
                  LEN(ALLTRIM(clave_prod))))
     SEEK clave_prod + m.codsin
     IF FOUND()
          WAIT WINDOW NOWAIT  ;
               'Codigo Ya Existe '
          LOOP
     ELSE
          EXIT
     ENDIF
ENDDO
@ 0, 15 GET m.dessin PICTURE '@!'  ;
  COLOR W/N 
READ
IF LASTKEY() = 27
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DEACTIVATE WINDOW ingreso
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     MOVE WINDOW sintoma TO 5, 14
     RETURN
ENDIF
m.codcla = clave_prod
m.date = DATE()
m.time = TIME()
APPEND BLANK
GATHER MEMVAR
WAIT WINDOW NOWAIT  ;
     ' S¡ntoma Grabado '
DEACTIVATE WINDOW ingreso
MOVE WINDOW sintoma TO 5, 14
RETURN
*
PROCEDURE anul_sint
PARAMETER anulado
ON KEY LABEL enter
clave_prod = ALLTRIM(clave_prod) +  ;
             (SPACE(4 -  ;
             LEN(ALLTRIM(clave_prod))))
codigo = SUBSTR(anulado, 5, 4)
SEEK clave_prod + codigo
IF FOUND()
     DELETE
     WAIT WINDOW NOWAIT  ;
          ' Sintoma Eliminado  ...'
ENDIF
RETURN
*
FUNCTION mant_sint
PARAMETER ii
IF ii = 1
     DO esc_indica WITH 1, 'ayu',  ;
        'INI', 'ANT', 'OTR'
     DO esc_indica WITH 2, 'bbb',  ;
        'FIN', 'STE', 'ESC'
     ii = 2
ELSE
     DO esc_indica WITH 1, 'MD3',  ;
        'BBB', 'ANT', 'OTR'
     DO esc_indica WITH 2, 'ELI',  ;
        'MBV', 'STE', 'ESC'
     ii = 1
ENDIF
RETURN ii
*
PROCEDURE salida
PARAMETER puerta
IF puerta = 1
     DEACTIVATE WINDOW producto
     DEACTIVATE POPUP producto
     DO mensa WITH  ;
        ' Espere un momento ...',  ;
        'SACA'
     RETURN
ELSE
     DEACTIVATE WINDOW sintoma
     DEACTIVATE POPUP sintoma
     DO mensa WITH  ;
        'espere un momento',  ;
        'SACA'
ENDIF
ON KEY
RETURN
*
PROCEDURE usedbf
PARAMETER dbf, indice
IF  .NOT. USED(dbf)
     SELECT 0
     use &dbf order &indice shared
ELSE
     SELECT (dbf)
     set order to &indice	
ENDIF
*
*** 
*** ReFox - retrace your steps ... 
***
