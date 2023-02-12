*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
*
FUNCTION p_presenta
SELECT 20
USE gc_par00
tit1 = ALLTRIM(par_razsoc)
CLOSE DATABASES
= f_pan01('∞',150)
@ 00, 00 SAY tit1
@ 00, 72 SAY DATE()
DO p_pan02
DO p_footer WITH  ;
   '100000000000000000001', 1
DO f_password
RETURN ret
*
PROCEDURE p_prestab
PARAMETER cad1, cad2, cad3, flag
tit1 = ALLTRIM(rge_razsoc)
tit2 = ALLTRIM(rge_abrev)
tit3 = rge_calle
@ 0, 01 SAY SPACE(25)
@ 0, 01 SAY tit1
@ 0, (80 - LEN(cad1)) / 2 SAY  ;
  cad1
@ 0, 63 SAY DATE()
@ 1, 03 SAY SPACE(20)
@ 1, 03 SAY '  ØØ ' + tit2 +  ;
  ' ÆÆ  '
@ 1, (80 - LEN(cad2)) / 2 SAY  ;
  cad2
@ 1, 63 SAY cad3 COLOR SCHEME 8
@ 2, 03 SAY SPACE(20)
@ 2, 03 SAY tit3
@ 2, (80 - LEN(cad2)) / 2 SAY  ;
  REPLICATE('Õ', LEN(cad2))
RETURN
*
PROCEDURE p_footer
PARAMETER wk1_var, wk1_estado
ACTIVATE WINDOW footer
IF wk1_estado = 1
     @ 00, 00 SAY REPLICATE('∞',  ;
       78)
     @ 01, 00 SAY REPLICATE('∞',  ;
       78)
ELSE
     @ 00, 00 SAY REPLICATE(' ',  ;
       78)
     @ 01, 00 SAY REPLICATE(' ',  ;
       78)
ENDIF
aux_cont = 0
aux_sepmin = 1
flag = .F.
FOR k = 1 TO LEN(wk1_var)
     IF SUBSTR(wk1_var, k, 1) =  ;
        '1'
          aux_cont = aux_cont + 1
          lix = IIF(aux_cont < 5,  ;
                00, 01)
          IF k = 21
               lix = 01
               aux_sepmin = 61
          ELSE
               IF  .NOT. flag
                    aux_sepmin = IIF(lix =  ;
                                 01,  ;
                                 1,  ;
                                 aux_sepmin)
                    IF lix = 01
                         flag = .T.
                    ENDIF
               ENDIF
          ENDIF
          aux_color = IIF(k = 21,  ;
                      'N*/W',  ;
                      'N/W')
          set color to &aux_color
          IF gc_tipo = 1
               @ lix, aux_sepmin  ;
                 SAY tecla1(k,1)
          ELSE
               @ lix, aux_sepmin  ;
                 SAY tecla2(k,1)
          ENDIF
          SET COLOR TO W/N
          IF gc_tipo = 1
               @ lix, (aux_sepmin +  ;
                 LEN(tecla1(k,1)) +  ;
                 1) SAY tecla1(k, ;
                 2)
          ELSE
               @ lix, (aux_sepmin +  ;
                 LEN(tecla2(k,1)) +  ;
                 1) SAY tecla2(k, ;
                 2)
          ENDIF
          aux_sepmin = aux_sepmin +  ;
                       20
          IF aux_sepmin > 70
               flag = .F.
          ENDIF
     ENDIF
ENDFOR
RETURN
*
FUNCTION f_pan01
PARAMETER char, tiempo
PRIVATE j
cad = REPLICATE(char, 8) + ' '
FOR j = 0 TO 39
     @ (3.0/10) * j, j, 24 - (3.0/ ;
       10) * j, 79 - j BOX cad
     FOR t = 1 TO tiempo
     ENDFOR
ENDFOR
RETURN .T.
*
PROCEDURE p_pan02
DEFINE WINDOW tits FROM 3, 15 TO  ;
       11, 65 IN screen NONE
ACTIVATE WINDOW tits
@ 0, 0 SAY  ;
  '…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª'
@ 1, 0 SAY  ;
  '∫  €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€  ∫'
@ 2, 0 SAY  ;
  '∫  €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€  ∫'
@ 3, 0 SAY  ;
  '∫  €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€  ∫'
@ 4, 0 SAY  ;
  '∫  €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€  ∫'
@ 5, 0 SAY  ;
  '∫  €€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€€  ∫'
@ 6, 0 SAY  ;
  'ÃÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ'
@ 7, 0 SAY  ;
  '∫  Grupo Carsa     RELEASE 1.0          (c) 1994  ∫'
@ 8, 0 SAY  ;
  '»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº'
RETURN
*
FUNCTION f_password
DEFINE WINDOW inici0 FROM 15, 25  ;
       TO 20, 55 IN screen
ACTIVATE WINDOW inici0
@ 1, 0 SAY  ;
  ' USUARIO  : [          ]'
@ 2, 0 SAY  ;
  ' PASSWORD : [          ]'
SET CURSOR ON
nveces = 1
DO WHILE nveces<4
     i = 1
     SET CURSOR ON
     STORE SPACE(10) TO log, pas
     @ 2, 13 SAY SPACE(10)
     @ 1, 13 GET log PICTURE '@!'  ;
       VALID log <> SPACE(10)
     READ
     IF LASTKEY() <> 27
          @ 2, 0 SAY  ;
            ' PASSWORD : ['
          DO WHILE LASTKEY()<>27  ;
             .AND. i<10
               c = INKEY(0)
               IF LASTKEY() <> 13
                    @ 2, 12 + i  ;
                      SAY ''
                    pas = pas +  ;
                          UPPER(CHR(c))
                    i = i + 1
               ELSE
                    IF EMPTY(pas)
                         DO p_mensaje  ;
                            WITH  ;
                            'PASSWORD INCORRECTO'
                         nveces =  ;
                          nveces +  ;
                          1
                    ELSE
                         USE password  ;
                             ORDER  ;
                             codigo
                         swt = .T.
                         ret = .F.
                         DO WHILE  ;
                            swt
                              SEEK  ;
                               ALLTRIM(log) +  ;
                               ALLTRIM(pas)
                              IF FOUND()
                                   clave = abrevid
                                   xnnn = nivel
                                   ret = .T.
                              ELSE
                                   DO p_mensaje WITH 'PASSWORD INCORRECTO'
                                   nveces = nveces + 1
                              ENDIF
                              EXIT
                         ENDDO
                    ENDIF
                    EXIT
               ENDIF
          ENDDO
     ELSE
          nveces = 4
          RETURN
     ENDIF
     IF ret
          EXIT
     ENDIF
ENDDO
IF  .NOT. ret
     DEFINE WINDOW peligro FROM  ;
            10, 10 TO 12, 70 IN  ;
            screen
     ACTIVATE WINDOW peligro
     SET COLOR TO W/N*
     @ 0, 1 SAY  ;
       '  PELIGRO !!!!!   SE DETECTO UN INTRUSO EN EL SISTEMA'
     SET ESCAPE OFF
     DO WHILE .T.
          = msound(10,500,10,1,0)
     ENDDO
ENDIF
SET CURSOR OFF
RELEASE WINDOW inici0
CLOSE DATABASES
DEACTIVATE WINDOW ALL
RETURN ret
*
FUNCTION csound
PARAMETER num, ini, dta, lon
PRIVATE q
q = STR(PARAMETERS(), 1)
num = IIF(q = '0', 9, num)
ini = IIF(q $ '01', 0, ini)
dta = IIF(q $ '012', 500, dta)
lon = IIF(q $ '0123', 1, lon)
FOR i = 1 TO num
     SET BELL TO ini + i * dta, lon
     ?? CHR(7)
ENDFOR
SET BELL TO
RETURN .T.
*
FUNCTION tsound
PARAMETER cad
PRIVATE clen, acar, casc
clen = LEN(cad)
SET BELL TO
ini = 0
ON KEY LABEL enter do para
DO WHILE .T.
     FOR x = 1 TO 2
          FOR i = 1 TO clen
               acar = SUBSTR(cad,  ;
                      i, 1)
               casc = ASC(acar)
               ini = IIF((ini +  ;
                     5) * casc >=  ;
                     10000, 0,  ;
                     ini)
               SET BELL TO (ini + 5) *;
casc, 3
               ?? CHR(7)
          ENDFOR
     ENDFOR
     ini = ini + 5
ENDDO
SET BELL TO
RETURN .T.
*
PROCEDURE para
ON KEY
SET BELL TO
RETURN TO MASTER
*
FUNCTION msound
PARAMETER num, ini, dta, lon, ida
PRIVATE q
q = STR(PARAMETERS(), 1)
num = IIF(q = '0', 9, num)
ini = IIF(q $ '01', 0, ini)
dta = IIF(q $ '012', 500, dta)
lon = IIF(q $ '0123', 1, lon)
ida = IIF(q $ '01234', 1, ida)
IF ida = 0
     FOR i = 1 TO INT(num / 2)
          SET BELL TO ini + i * dta, lon
          ?? CHR(7)
     ENDFOR
     FOR i = INT(num / 2) TO 1  ;
         STEP -1
          SET BELL TO ini + i * dta, lon
          ?? CHR(7)
     ENDFOR
ELSE
     IF (ida = -1) .OR. (ida = 1)
          FOR i = 1 TO num
               SET BELL TO ini + (ida);
* i * dta, lon
               ?? CHR(7)
          ENDFOR
     ENDIF
ENDIF
SET BELL TO
RETURN .T.
*
PROCEDURE f_fondo
PARAMETER cadena, divide, col1
q = STR(PARAMETERS(), 1)
divide = IIF(q $ '123', divide,  ;
         0)
IF q = '3'
     set color to &col1
ENDIF
linea = INT(80 / LEN(cadena))
IF divide > 0
     separa = INT(25 / divide) +  ;
              1
     cad1 = REPLICATE(cadena,  ;
            linea + 80)
     FOR x = 1 TO separa - 1
          @ x - 1, 0 SAY  ;
            SUBSTR(cad1, x, 80)
     ENDFOR
ELSE
     cad1 = REPLICATE(cadena,  ;
            linea + 1)
     FOR x = 0 TO 24
          @ x, 0 SAY cad1
     ENDFOR
ENDIF
RETURN
*
PROCEDURE f_recorre
PARAMETER cadena, divide, col1
q = STR(PARAMETERS(), 1)
divide = IIF(q $ '1', 0, divide)
IF q = '3'
     set color to &col1
ENDIF
linea = INT(80 / LEN(cadena))
IF divide > 0
     separa = INT(25 / divide) +  ;
              1
     cad1 = REPLICATE(cadena,  ;
            linea + 80)
     FOR x = 1 TO separa - 1
          uxu = SUBSTR(cad1, x,  ;
                80)
          FOR d = 1 TO LEN(uxu)
               @ x - 1, d - 1 SAY  ;
                 SUBSTR(uxu, d,  ;
                 1)
               FOR w = 1 TO 80
               ENDFOR
          ENDFOR
     ENDFOR
ELSE
     cad1 = REPLICATE(cadena,  ;
            linea + 1)
     FOR x = 0 TO 24
          @ x, 0 SAY cad1
          FOR d = 1 TO 1500
          ENDFOR
     ENDFOR
ENDIF
RETURN
*
PROCEDURE f_fondo1
PARAMETER cadena, col1
q = STR(PARAMETERS(), 1)
IF q = '2'
     set color to &col1
ENDIF
STORE 0 TO x, d
lc = LEN(cadena)
x = 1
fl = .T.
DO WHILE fl
     s = 0
     FOR r = 1 TO lc
          IF d + s < 80
               @ x - 1, (s + d)  ;
                 SAY  ;
                 SUBSTR(cadena, r,  ;
                 1)
               s = s + 1
          ELSE
               IF x > 24 .AND. d +  ;
                  s > 79
                    fl = .F.
                    EXIT
               ENDIF
               x = x + 1
               STORE 0 TO s, d
               r = r - 1
          ENDIF
     ENDFOR
     d = d + s
ENDDO
RETURN
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4 COLOR SCHEME 12
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens
= INKEY(1.5 )
RELEASE WINDOW mensj
SET CURSOR ON
RETURN
*
FUNCTION f_yesno
PARAMETER mens, def
PRIVATE ALL
l = IIF(LEN(mens) < 18, 18,  ;
    LEN(mens))
sdef = IIF(def = .T.,  ;
       '\?\<Ok;\!\<Cancel',  ;
       '\?\<No;\!\<Si')
DEFINE WINDOW wyesno FROM 18, (80 -  ;
       l) / 2 - 4 TO 23, (80 + l) /  ;
       2 + 4 DOUBLE COLOR SCHEME  ;
       12
ACTIVATE WINDOW wyesno
@ 1, 4 SAY mens
@ 3, CEILING(l - 18) / 2 GET vok  ;
  DEFAULT 1 SIZE 1, 10, 4 PICTURE  ;
  '@*HT ' + sdef
READ CYCLE
RELEASE WINDOW wyesno
RETURN IIF(def = .T., IIF(vok = 1,  ;
       .T., .F.), IIF(vok = 1,  ;
       .F., .T.))
*
FUNCTION f_abre
PARAMETER char, tiempo
PRIVATE j
cad = REPLICATE(char, 8) + ' '
FOR j = 0 TO 39
     @ (3.0/10) * j, j, 24 - (3.0/ ;
       10) * j, 79 - j BOX cad
     FOR t = 1 TO tiempo
     ENDFOR
ENDFOR
RETURN .T.
*
FUNCTION f_cierra
FOR j = 1 TO 39
     @ 12 - (3.0/10) * j, 39 - j,  ;
       13 + (3.0/10) * j, 40 + j  ;
       BOX SPACE(9)
     FOR e = 1 TO 500
     ENDFOR
ENDFOR
RETURN .T.
*
FUNCTION f_ceros
PARAMETER num, cant, mti
PRIVATE l, n
IF PARAMETERS() = 1
     cant = 6
     mti = 1
ENDIF
IF mti = 1
     l = LEN(LTRIM(STR(num)))
     n = LTRIM(STR(num))
ELSE
     l = LEN(ALLTRIM(num))
     n = ALLTRIM(num)
ENDIF
ret = REPLICATE('0', cant - l) +  ;
      n
RETURN ret
*
FUNCTION f_archivo
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.DBF')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION f_reporte
PARAMETER nom
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = nom + LTRIM(STR(cor)) +  ;
           '.PRN'
     IF FILE(ret)
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION f_indice
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'IND' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.IDX')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION f_texto
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEXT' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.TXT')
          cor = cor + 1
     ELSE
          ret = ret + '.TXT'
          EXIT
     ENDIF
ENDDO
RETURN ret
*
PROCEDURE p_calendar
ACTIVATE WINDOW calendar
RETURN
*
PROCEDURE p_calculad
ACTIVATE SCREEN
STORE 0 TO _CALCVALUE
ACTIVATE WINDOW calculator
RETURN
*
PROCEDURE p_menpar
direc = mruta
rge_razsoc = ALLTRIM(par_razsoc)
rge_abrev = ALLTRIM(par_abrev)
rge_calle = ALLTRIM(par_calle)
rge_distri = ALLTRIM(par_distri)
rge_provin = ALLTRIM(par_provin)
rge_lispre = par_codlis
rge_monbas = par_monbas
rge_codalm = par_codalm
rge_lptfac = par_lptfac
rge_lptgui = par_lptgui
rge_lptped = par_lptped
rge_lptbol = par_lptbol
rge_lptdes = par_lptbol
rge_punemi = par_punemi
rge_pormax = par_pormax
RETURN
*
PROCEDURE acti
DEFINE WINDOW acti FROM 24, 0 TO  ;
       24, 79 IN screen NONE
ACTIVATE WINDOW acti
@ 0, 24 SAY 'PATH : '
@ 0, 31 GET direc
READ
mruta = direc
SAVE TO ruta ALL LIKE mruta*.*
DO direccion
DEACTIVATE WINDOW acti
RETURN
*
PROCEDURE p_tempos
num1 = ADIR(archivo1, 'TEMP*.DBF',  ;
       'A')
num2 = ADIR(archivo2, 'TEMP*.CDX',  ;
       'A')
num3 = ADIR(archivo3, '*.IDX',  ;
       'A')
num4 = ADIR(archivo4, '*.TXT',  ;
       'A')
FOR x = 1 TO num1
     erase &archivo1(x,1)
ENDFOR
FOR x = 1 TO num2
     erase &archivo2(x,1)
ENDFOR
FOR x = 1 TO num3
     erase &archivo3(x,1)
ENDFOR
FOR x = 1 TO num4
     erase &archivo4(x,1)
ENDFOR
RETURN
*
FUNCTION busca
PARAMETER wrk_busca, wrk_var
des_tab = ALLTRIM(b_tit_tab(wrk_busca))
DEFINE WINDOW bus_tab FROM 08, 23  ;
       TO 18, 63 TITLE des_tab IN  ;
       screen FOOTER  ;
       ' [F6] Busqueda x Descr. '  ;
       COLOR SCHEME 8
DEFINE WINDOW b_tabla FROM 12, 31  ;
       TO 14, 54 TITLE ' Tablas '  ;
       IN screen
= ooopen('ge_tab0',1)
SET NEAR ON
SET TALK OFF
ON KEY
SET ORDER TO 2
SET FILTER TO tab_codpre = wrk_busca
GOTO TOP
ACTIVATE WINDOW bus_tab
ON KEY LABEL ENTER DO CARGA_CODT
ON KEY LABEL F6 DO BUSCA_CODT
BROWSE FIELDS tab_codtab :R :H =  ;
       'Codigo', tab_destab :R :H =  ;
       '  Descripcion ' IN  ;
       bus_tab COLOR SCHEME 8
ON KEY
RELEASE WINDOW bus_tab
SET FILTER TO
SET ORDER TO 1
RETURN wrk_var
*
FUNCTION carga_codt
ON KEY
&wrk_var = tab_codtab
RELEASE WINDOW bus_tab
SET ORDER TO 1
KEYBOARD CHR(13)
RETURN wrk_var
*
PROCEDURE busca_codt
ON KEY
ACTIVATE WINDOW b_tabla
c_tab = SPACE(25)
@ 0, 0 GET c_tab PICTURE '@!'
READ
SEEK c_tab
DEACTIVATE WINDOW b_tabla
ACTIVATE WINDOW bus_tab
ON KEY LABEL F6 DO BUSCA_CODT
ON KEY LABEL ENTER DO CARGA_CODT
RETURN
*
FUNCTION b_tit_tab
PARAMETER tabla
= ooopen('ge_tab0',1)
SET FILTER TO tab_codtab = tabla
GOTO TOP
RETURN tab_destab
*
FUNCTION entidad
PARAMETER wrk_tiper, wrk_var
IF wrk_tiper = 'C' .OR. wrk_tiper =  ;
   'P'
     DEFINE WINDOW cliente FROM  ;
            08, 20 TO 18, 63  ;
            TITLE  ;
            ' Busqueda de Clientes/Proveedores  '  ;
            IN screen FOOTER  ;
            ' [F6] Busqueda Rapida '  ;
            COLOR SCHEME 8
     = ooopen('gc_cli00',2)
ELSE
     DEFINE WINDOW cliente FROM  ;
            08, 20 TO 18, 63  ;
            TITLE  ;
            ' Busqueda de Vendedore/ADministrativos '  ;
            IN screen FOOTER  ;
            ' [F6] Busqueda Rapida '  ;
            COLOR SCHEME 8
     = ooopen('gc_vnd00',2)
ENDIF
SET NEAR ON
SET TALK OFF
DEFINE WINDOW b_clien FROM 12, 30  ;
       TO 14, 53 TITLE ' Nombre '  ;
       IN screen
ON KEY
SET ORDER TO 2
IF wrk_tiper = 'C' .OR. wrk_tiper =  ;
   'P'
     SET FILTER TO cli_tpper = ALLTRIM(wrk_tiper)
ELSE
     SET FILTER TO vnd_tpper = ALLTRIM(wrk_tiper)
ENDIF
GOTO TOP
ACTIVATE WINDOW cliente
ON KEY LABEL ENTER DO CARGA_COD
ON KEY LABEL F6 DO BUSCA_COD
IF wrk_tiper = 'C' .OR. wrk_tiper =  ;
   'P'
     BROWSE FIELDS cli_codigo :R  ;
            :H = 'Codigo',  ;
            cli_razsoc :R :H =  ;
            '  Nombre / Razon Social  '  ;
            IN cliente COLOR  ;
            SCHEME 8
ELSE
     BROWSE FIELDS vnd_code :R :H =  ;
            'Codigo', vnd_nombre  ;
            :R :H =  ;
            '  Nombre / Razon Social  '  ;
            IN cliente COLOR  ;
            SCHEME 8
ENDIF
ON KEY
RELEASE WINDOW cliente
RETURN wrk_var
*
FUNCTION carga_cod
ON KEY
IF wrk_tiper = 'C' .OR. wrk_tiper =  ;
   'P'
     &wrk_var = cli_codigo
ELSE
     &wrk_var = VND_CODE
ENDIF
RELEASE WINDOW cliente
SET ORDER TO 1
KEYBOARD CHR(13)
RETURN wrk_var
*
PROCEDURE busca_cod
ON KEY
ACTIVATE WINDOW b_clien
c_cli = SPACE(25)
@ 0, 0 GET c_cli PICTURE '@!'
READ
SEEK c_cli
DEACTIVATE WINDOW b_clien
ACTIVATE WINDOW cliente
ON KEY LABEL F6 DO BUSCA_COD
ON KEY LABEL ENTER DO CARGA_COD
RETURN
*
PROCEDURE p_tecdedic
tecla1( 01, 1) = '[ F1 ]'
tecla1( 01, 2) = 'Ayuda     '
tecla1( 02, 1) = '[ F2 ]'
tecla1( 02, 2) = 'Grabar    '
tecla1( 03, 1) = '[ F3 ]'
tecla1( 03, 2) = 'Crear     '
tecla1( 04, 1) = '[ F4 ]'
tecla1( 04, 2) = 'Eliminar  '
tecla1( 05, 1) = '[ F6 ]'
tecla1( 05, 2) = 'Buscar    '
tecla1( 06, 1) = '[ F7 ]'
tecla1( 06, 2) = 'Imprimir  '
tecla1( 07, 1) = '[ F7 ]'
tecla1( 07, 2) = 'Descr.Ing.'
tecla1( 08, 1) = '[ F8 ]'
tecla1( 08, 2) = 'Gastos    '
tecla1( 09, 1) = '[ F10]'
tecla1( 09, 2) = 'Ignorar   '
tecla1( 10, 1) = '[ F5 ]'
tecla1( 10, 2) = 'Cal.precio'
tecla1( 11, 1) = '[ F7 ]'
tecla1( 11, 2) = 'Desm. Todo'
tecla1( 12, 1) = '[ƒŸ ]'
tecla1( 12, 2) = 'Ejecuta   '
tecla1( 13, 1) = '[ƒŸ ]'
tecla1( 13, 2) = 'Modifica  '
tecla1( 14, 1) = '[Tab ]'
tecla1( 14, 2) = 'Selecciona'
tecla1( 15, 1) = '[  ]'
tecla1( 15, 2) = 'Up/Down   '
tecla1( 16, 1) = '[Home]'
tecla1( 16, 2) = 'Principio '
tecla1( 17, 1) = '[End ]'
tecla1( 17, 2) = 'Ultimo    '
tecla1( 18, 1) = '[ F5 ]'
tecla1( 18, 2) = 'Marca Todo'
tecla1( 19, 1) = '[ F9 ]'
tecla1( 19, 2) = 'Mas Func. '
tecla1( 20, 1) = '[ƒŸ ]'
tecla1( 20, 2) = 'Selec/Marc'
tecla1( 21, 1) = '[Esc ]'
tecla1( 21, 2) = 'Finaliza  '
tecla1( 22, 1) = '[ Up ]'
tecla1( 22, 2) = 'Avanza Pag'
tecla1( 23, 1) = '[Down]'
tecla1( 23, 2) = 'Retroc Pag'
tecla1( 24, 1) = '[ F5 ]'
tecla1( 24, 2) = 'Mod. Cabec'
RETURN
*
PROCEDURE p_tecdedid
tecla2( 01, 1) = '[ F1 ]'
tecla2( 01, 2) = 'Ayuda     '
tecla2( 02, 1) = '[ F2 ]'
tecla2( 02, 2) = 'Grabar    '
tecla2( 03, 1) = '[ F3 ]'
tecla2( 03, 2) = 'Crear     '
tecla2( 04, 1) = '[ F4 ]'
tecla2( 04, 2) = 'Eliminar  '
tecla2( 05, 1) = '[ F6 ]'
tecla2( 05, 2) = 'Buscar    '
tecla2( 06, 1) = '[ F7 ]'
tecla2( 06, 2) = 'Imprimir  '
tecla2( 07, 1) = '[ F7 ]'
tecla2( 07, 2) = 'Descr.Ing.'
tecla2( 08, 1) = '[ F8 ]'
tecla2( 08, 2) = 'Gastos    '
tecla2( 09, 1) = '[ F10]'
tecla2( 09, 2) = 'Ignorar   '
tecla2( 10, 1) = '[ F5 ]'
tecla2( 10, 2) = 'Marca Todo'
tecla2( 11, 1) = '[ F7 ]'
tecla2( 11, 2) = 'Desm. Todo'
tecla2( 12, 1) = '[ƒŸ ]'
tecla2( 12, 2) = 'Ejecuta   '
tecla2( 13, 1) = '[ƒŸ ]'
tecla2( 13, 2) = 'Modifica  '
tecla2( 14, 1) = '[Tab ]'
tecla2( 14, 2) = 'Selecciona'
tecla2( 15, 1) = '[  ]'
tecla2( 15, 2) = 'Up/Down   '
tecla2( 16, 1) = '[Home]'
tecla2( 16, 2) = 'Principio '
tecla2( 17, 1) = '[End ]'
tecla2( 17, 2) = 'Ultimo    '
tecla2( 18, 1) = '[ F5 ]'
tecla2( 18, 2) = 'Marca Todo'
tecla2( 19, 1) = '[ F9 ]'
tecla2( 19, 2) = 'Mas Func. '
tecla2( 20, 1) = '[ƒŸ ]'
tecla2( 20, 2) = 'Selec/Marc'
tecla2( 21, 1) = '[Esc ]'
tecla2( 21, 2) = 'Finaliza  '
tecla2( 22, 1) = '[ Up ]'
tecla2( 22, 2) = 'Avanza Pag'
tecla2( 23, 1) = '[Down]'
tecla2( 23, 2) = 'Retroc Pag'
tecla2( 24, 1) = '[ F5 ]'
tecla2( 24, 2) = 'Mod. Cabec'
RETURN
*
PROCEDURE produc
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 12, 02  ;
       TO 20, 76 IN screen COLOR  ;
       SCHEME 8
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<Codigo'
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripcion'
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Sub-Categ.'
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Nro. Parte'
ON SELECTION POPUP prod do buspro with;
bar(),wrk_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       20, 73 PROMPT FIELDS  ;
       pro_codpro + '≥' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '≥' + SUBSTR(pro_modelo, 1,  ;
       20) + '≥' + pro_subcat IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with wrk_campo,wrk_selec,wrk_selpro
IF LASTKEY() <> 27
     &wrk_campo = substr(gc_pro00.pro_codpro,1,14)
ENDIF
SELECT (wrk_selec)
KEYBOARD CHR(13)
RETURN
*
PROCEDURE buspro
PARAMETER bar, wrk_selpro
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF bar = 1
     wrk_codpro = SPACE(14)
     SET ORDER TO codigo
     @ 00, 00 SAY 'Codigo :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     wrk_codpro = SPACE(40)
     SET ORDER TO descri
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 3
     wrk_codpro = SPACE(4)
     SET ORDER TO subcat
     @ 00, 00 SAY 'Sub-Cat :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     wrk_codpro = SPACE(10)
     SET ORDER TO parara
     @ 00, 00 SAY 'Partida :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
READ
SELECT gc_alm00
SET ORDER TO CODIGO
SET FILTER TO alm_codalm = '0001'
SELECT gc_pro00
SET RELATION TO pro_codpro INTO gc_alm00
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK wrk_codpro
     wrk_codpro = SPACE(14)
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod
     BROWSE FIELDS pro_codpro :R  ;
            :H = 'Cod. Produc.',  ;
            pro_descri :R : 25 :H =  ;
            'Descripcion',  ;
            pro_modelo :R : 10 :H =  ;
            'Modelo', pro_codree  ;
            :R :H = 'Reemplazo',  ;
            gc_alm00.alm_stkfis  ;
            :R : 5 :P = '999' :H =  ;
            'Stock', pro_subcat  ;
            :R :H = 'Sub-Cat' IN  ;
            produ
ENDIF
ON KEY
SET FILTER TO
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
SET NEAR OFF
RETURN
*
PROCEDURE tomacod
ON KEY
&wrk_campo = gc_pro00.pro_codpro
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
PROCEDURE bushlp
PARAMETER wrk_var
ON KEY
SELECT gc_hlp00.hlp_codlis,  ;
       gc_hlp00.hlp_deslis FROM  ;
       gc_hlp00 INTO CURSOR hlp
ACTIVATE POPUP hlp
&wrk_var = hlp_codlis
KEYBOARD CHR(13)
RETURN
*
PROCEDURE unialt
PARAMETER wrk_busca, wrk_var
SELECT gc_uni00.uni_unialt,  ;
       gc_uni00.uni_facto1,  ;
       gc_uni00.uni_facto2,  ;
       gc_pro00.pro_unimed FROM  ;
       gc_uni00, gc_pro00 WHERE  ;
       gc_pro00.pro_codpro =  ;
       gc_uni00.uni_codpro AND  ;
       gc_uni00.uni_codpro =  ;
       wrk_busca INTO CURSOR uni
ACTIVATE POPUP uni
IF LASTKEY() = 13
     &wrk_var = uni_unialt
ENDIF
KEYBOARD CHR(13)
RETURN
*
FUNCTION oofactor
PARAMETER codpro, unimed
SELECT uni_facto1 / uni_facto2  ;
       FROM GC_UNI00 WHERE  ;
       gc_uni00.uni_codpro =  ;
       codpro AND  ;
       gc_uni00.uni_unialt =  ;
       unimed INTO CURSOR factor
COUNT TO wrk_reg
GOTO TOP
IF wrk_reg = 0
     RETURN 1.00 
ENDIF
RETURN exp_1
*
FUNCTION oodespro
PARAMETER ccodpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK ccodpro
IF FOUND()
     wrk_despro = gc_pro00.pro_descri
     wrk_unipro = gc_pro00.pro_unimed
ELSE
     wrk_despro = ' '
     wrk_unipro = ' '
ENDIF
SELECT (narea)
RETURN wrk_despro
*
FUNCTION oocospro
PARAMETER ccodpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK ccodpro
IF FOUND()
     wrk_coprmo = gc_pro00.pro_coprmo
ELSE
     wrk_coprmo = 0
ENDIF
SELECT (narea)
RETURN wrk_coprmo
*
FUNCTION oocosprb
PARAMETER ccodpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK ccodpro
IF FOUND()
     wrk_coprmb = gc_pro00.pro_coprmb
ELSE
     wrk_coprmb = 0
ENDIF
SELECT (narea)
RETURN wrk_coprmb
*
FUNCTION oocosuni
PARAMETER ccodpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK ccodpro
IF FOUND()
     wrk_ulcomb = gc_pro00.pro_ulcomb
ELSE
     wrk_ulcomb = 0
ENDIF
SELECT (narea)
RETURN wrk_ulcomb
*
FUNCTION oofacuni
PARAMETER codpro, unimed
narea = SELECT()
= ooareat('GC_UNI00','CODIGO')
SEEK codpro + unimed
IF FOUND()
     wrk_facuni = uni_facto1 /  ;
                  uni_facto2
ELSE
     wrk_facuni = 1
ENDIF
SELECT (narea)
RETURN wrk_facuni
*
FUNCTION oounipro
PARAMETER ccodpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK ccodpro
IF FOUND()
     wrk_unipro = gc_pro00.pro_unimed
ELSE
     wrk_unipro = ' '
ENDIF
SELECT (narea)
RETURN wrk_unipro
*
FUNCTION oodesalma
PARAMETER ccodalm
narea = SELECT()
wrk_busca = 'ALMA'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodalm
IF FOUND()
     wrk_desalm = ge_tab0.tab_destab
ELSE
     wrk_desalm = ' '
ENDIF
SELECT (narea)
RETURN wrk_desalm
*
FUNCTION oodeslin
PARAMETER ccodlin
narea = SELECT()
wrk_busca = 'LINE'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodlin
IF FOUND()
     wrk_deslin = ge_tab0.tab_destab
ELSE
     wrk_deslin = ' '
ENDIF
SELECT (narea)
RETURN wrk_deslin
*
FUNCTION oodesmar
PARAMETER ccodmar
narea = SELECT()
wrk_busca = 'MARC'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodmar
IF FOUND()
     wrk_desmar = ge_tab0.tab_destab
ELSE
     wrk_desmar = ' '
ENDIF
SELECT (narea)
RETURN wrk_desmar
*
FUNCTION oodesrub
PARAMETER ccodrub
narea = SELECT()
wrk_busca = 'RUBR'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodrub
IF FOUND()
     wrk_desrub = ge_tab0.tab_destab
ELSE
     wrk_desrub = ' '
ENDIF
SELECT (narea)
RETURN wrk_desrub
*
FUNCTION oodescli
PARAMETER ccodcli
narea = SELECT()
= ooareat('GC_CLI00','CODIGO')
SEEK 'C' + ccodcli
IF FOUND()
     wrk_descli = gc_cli00.cli_razsoc
ELSE
     wrk_descli = ' '
ENDIF
SELECT (narea)
RETURN wrk_descli
*
FUNCTION oodircli
PARAMETER ccodcli
narea = SELECT()
= ooareat('GC_CLI00','CODIGO')
SEEK 'C' + ccodcli
IF FOUND()
     wrk_dircli = gc_cli00.cli_calle
ELSE
     wrk_descli = ' '
ENDIF
SELECT (narea)
RETURN wrk_dircli
*
FUNCTION oonumcli
PARAMETER ccodcli
narea = SELECT()
= ooareat('GC_CLI00','CODIGO')
SEEK 'C' + ccodcli
IF FOUND()
     wrk_numcli = gc_cli00.cli_numero
ELSE
     wrk_numcli = ' '
ENDIF
SELECT (narea)
RETURN wrk_numcli
*
FUNCTION ootelcli
PARAMETER ccodcli
narea = SELECT()
= ooareat('GC_CLI00','CODIGO')
SEEK 'C' + ccodcli
IF FOUND()
     wrk_telcli = gc_cli00.cli_telefo
ELSE
     wrk_telcli = ' '
ENDIF
SELECT (narea)
RETURN wrk_telcli
*
FUNCTION oodespve
PARAMETER ccodpve
narea = SELECT()
= ooareat('GC_CLI00','CODIGO')
SEEK 'P' + ccodpve
IF FOUND()
     wrk_despve = gc_cli00.cli_razsoc
ELSE
     wrk_despve = ' '
ENDIF
SELECT (narea)
RETURN wrk_despve
*
FUNCTION oodesvnd
PARAMETER ccodvnd
narea = SELECT()
= ooareat('GC_VND00','CODIGO')
SEEK 'V' + ccodvnd
IF FOUND()
     wrk_desvnd = gc_vnd00.vnd_nombre
ELSE
     wrk_desvnd = ' '
ENDIF
RETURN wrk_desvnd
*
FUNCTION oodespag
PARAMETER ccodpag
narea = SELECT()
wrk_busca = 'FPAG'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodpag
IF FOUND()
     wrk_despag = ge_tab0.tab_destab
ELSE
     wrk_despag = ' '
ENDIF
SELECT (narea)
RETURN wrk_despag
*
FUNCTION oodesmon
PARAMETER ccodmon
narea = SELECT()
wrk_busca = 'MONE'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodmon
IF FOUND()
     wrk_desmon = ge_tab0.tab_destab
ELSE
     wrk_desmon = ' '
ENDIF
SELECT (narea)
RETURN wrk_desmon
*
PROCEDURE ooopen
PARAMETER carchivo, norden
IF USED(carchivo)
     SELECT (carchivo)
     SET ORDER TO (norden)
ELSE
     SELECT 0
     USE (LOCFILE(carchivo, 'DBF',  ;
         'Where is gc_pro00?'))  ;
         AGAIN ALIAS (carchivo)  ;
         ORDER (norden)
ENDIF
RETURN
*
PROCEDURE ooarea
PARAMETER carchivo
SELECT (carchivo)
RETURN
*
PROCEDURE ooareao
PARAMETER carchivo, norden
SELECT (carchivo)
SET ORDER TO (norden)
RETURN
*
PROCEDURE ooareat
PARAMETER carchivo, ctag
SELECT (carchivo)
SET ORDER TO (ctag)
RETURN
*
PROCEDURE ooclose
PARAMETER carchivo
IF USED(carchivo)
     SELECT (carchivo)
     USE
ENDIF
*
PROCEDURE ooaviso
PARAMETER cfrase
DEFINE WINDOW winmensaje FROM  ;
       INT((SROWS() - 5) / 2),  ;
       INT((SCOLS() - 40) / 2) TO  ;
       INT((SROWS() - 5) / 2) + 4,  ;
       INT((SCOLS() - 40) / 2) +  ;
       39 FLOAT NOCLOSE SHADOW  ;
       NOMINIMIZE DOUBLE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW SAME winmensaje
@ 1, 7 SAY cfrase COLOR N/W* 
RETURN
*
FUNCTION oostkalm
PARAMETER wrk_codpro
wrk_sel = SELECT()
wrk_arc_01 = 'TMP' +  ;
             SUBSTR(SYS(3), 1, 5) +  ;
             '.DBF'
SELECT DISTINCT gc_pro00.*,  ;
       gc_alm00.alm_codalm,  ;
       SUM(gc_alm00.alm_stkfis),  ;
       SUM(gc_alm00.alm_stkres)  ;
       FROM gc_alm00, gc_pro00  ;
       WHERE (gc_pro00.pro_codpro =  ;
       gc_alm00.alm_codpro) AND  ;
       (gc_pro00.pro_codpro =  ;
       wrk_codpro) GROUP BY  ;
       gc_pro00.pro_codpro INTO  ;
       DBF (wrk_arc_01)
wrk_return = sum_alm__a
USE
ERASE (wrk_arc_01)
SELECT (wrk_sel)
RETURN wrk_return
*
FUNCTION oonumlet
PARAMETER ncifra
PRIVATE numero, longitud, digito,  ;
        deci, ccifra
ccifra = TRIM(STR(ncifra, 12, 2))
numero = ''
deci = RIGHT(ccifra, 2)
ccifra = SUBSTR(ccifra, 1,  ;
         (LEN(ccifra) - 3))
DO WHILE .T.
     longitud = LEN(ccifra)
     digito = LEFT(ccifra, 1)
     DO CASE
          CASE longitud = 6
               DO CASE
                    CASE digito =  ;
                         '1'
                         numero =  ;
                          numero +  ;
                          'CIENTO '
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOSCIENTOS '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRESCIENTOS '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATROCIENTOS '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'QUINIENTOS '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEISCIENTOS '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETECIENTOS '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHOCIENTOS '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVECIENTOS '
               ENDCASE
          CASE longitud = 5
               DO CASE
                    CASE digito =  ;
                         '1'
                         DO CASE
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '0'
                                   numero = numero + 'DIEZ MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '1'
                                   numero = numero + 'ONCE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '2'
                                   numero = numero + 'DOCE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '3'
                                   numero = numero + 'TRECE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '4'
                                   numero = numero + 'CATORCE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '5'
                                   numero = numero + 'QUINCE MIL'
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '6'
                                   numero = numero + 'DIECISEIS MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '7'
                                   numero = numero + 'DIECISIETE MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '8'
                                   numero = numero + 'DIECIOCHO MIL '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '9'
                                   numero = numero + 'DIECINUEVE MIL '
                         ENDCASE
                         ccifra =  ;
                          RIGHT(ccifra,  ;
                          (LEN(ccifra) -  ;
                          1))
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'VEINTI'
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TREINTI'
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUARENTI'
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCUENTI'
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SESENTI'
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETENTI'
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHENTI'
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVENTI'
               ENDCASE
          CASE longitud = 4
               DO CASE
                    CASE digito =  ;
                         '0'
                         numero =  ;
                          SUBSTR(numero,  ;
                          1,  ;
                          (LEN(numero) -  ;
                          1))
                         IF RIGHT(numero,  ;
                            5) =  ;
                            'VEINT'
                              numero =  ;
                               numero +  ;
                               'EMIL '
                         ELSE
                              numero =  ;
                               numero +  ;
                               'AMIL '
                         ENDIF
                    CASE digito =  ;
                         '1'
                         IF LEN(numero) =  ;
                            0
                              numero =  ;
                               numero +  ;
                               'MIL '
                         ELSE
                              numero =  ;
                               numero +  ;
                               'UNMIL '
                         ENDIF
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOSMIL '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRESMIL '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATROMIL '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCOMIL '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEISMIL '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SIETEMIL '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHOMIL '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NUEVEMIL '
               ENDCASE
          CASE longitud = 3
               DO CASE
                    CASE digito =  ;
                         '1'
                         numero =  ;
                          numero +  ;
                          'CIENTO '
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOSCIENTOS '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRESCIENTOS '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATROCIENTOS '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'QUINIENTOS '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEISCIENTOS '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETECIENTOS '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHOCIENTOS '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVECIENTOS '
               ENDCASE
          CASE longitud = 2
               DO CASE
                    CASE digito =  ;
                         '1'
                         DO CASE
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '0'
                                   numero = numero + 'DIEZ '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '1'
                                   numero = numero + 'ONCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '2'
                                   numero = numero + 'DOCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '3'
                                   numero = numero + 'TRECE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '4'
                                   numero = numero + 'CATORCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '5'
                                   numero = numero + 'QUINCE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '6'
                                   numero = numero + 'DIECISEIS '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '7'
                                   numero = numero + 'DIECISIETE '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '8'
                                   numero = numero + 'DIECIOCHO '
                              CASE  ;
                               SUBSTR(ccifra,  ;
                               2,  ;
                               1) =  ;
                               '9'
                                   numero = numero + 'DIECINUEVE '
                         ENDCASE
                         ccifra =  ;
                          RIGHT(ccifra,  ;
                          (LEN(ccifra) -  ;
                          1))
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'VEINTI'
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TREINTI'
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUARENTI'
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCUENTI'
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SESENTI'
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SETENTI'
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHENTI'
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NOVENTI'
               ENDCASE
          CASE longitud = 1
               DO CASE
                    CASE digito =  ;
                         '0'
                         numero =  ;
                          SUBSTR(numero,  ;
                          1,  ;
                          (LEN(numero) -  ;
                          1))
                         IF RIGHT(numero,  ;
                            5) =  ;
                            'VEINT'
                              numero =  ;
                               numero +  ;
                               'E '
                         ELSE
                              numero =  ;
                               numero +  ;
                               'A '
                         ENDIF
                    CASE digito =  ;
                         '1'
                         numero =  ;
                          numero +  ;
                          'UNO '
                    CASE digito =  ;
                         '2'
                         numero =  ;
                          numero +  ;
                          'DOS '
                    CASE digito =  ;
                         '3'
                         numero =  ;
                          numero +  ;
                          'TRES '
                    CASE digito =  ;
                         '4'
                         numero =  ;
                          numero +  ;
                          'CUATRO '
                    CASE digito =  ;
                         '5'
                         numero =  ;
                          numero +  ;
                          'CINCO '
                    CASE digito =  ;
                         '6'
                         numero =  ;
                          numero +  ;
                          'SEIS '
                    CASE digito =  ;
                         '7'
                         numero =  ;
                          numero +  ;
                          'SIETE '
                    CASE digito =  ;
                         '8'
                         numero =  ;
                          numero +  ;
                          'OCHO '
                    CASE digito =  ;
                         '9'
                         numero =  ;
                          numero +  ;
                          'NUEVE '
               ENDCASE
     ENDCASE
     IF LEN(ccifra) > 1
          ccifra = RIGHT(ccifra,  ;
                   (LEN(ccifra) -  ;
                   1))
     ELSE
          EXIT
     ENDIF
ENDDO
numero = 'SON: ' + numero + 'Y ' +  ;
         deci +  ;
         '/100 NUEVOS SOLES'
RETURN (numero)
DEFINE WINDOW winbuscar FROM  ;
       INT((SROWS() - 10) / 2),  ;
       INT((SCOLS() - 30) / 2) TO  ;
       INT((SROWS() - 10) / 2) +  ;
       8, INT((SCOLS() - 30) / 2) +  ;
       29 NOFLOAT NOCLOSE  ;
       NOMINIMIZE COLOR SCHEME 1
ACTIVATE WINDOW winbuscar
SET CURSOR ON
@ 0, 0 SAY  ;
  '        Buscar por :        '  ;
  SIZE 1, 28, 0 COLOR N+/W 
@ 2, 6 GET nop1 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE '@*HN \<C¢digo'  ;
  VALID oolocaliza(1) COLOR  ;
  SCHEME 12
@ 3, 6 GET nop2 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE  ;
  '@*HN \<Descripci¢n' VALID  ;
  oolocaliza(2) COLOR SCHEME 12
@ 4, 6 GET nop3 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE '@*HN \<Modelo'  ;
  DISABLE VALID oolocaliza(3)  ;
  COLOR SCHEME 12
@ 5, 6 GET nop4 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE '@*HN M\<oneda'  ;
  DISABLE VALID oolocaliza(4)  ;
  COLOR SCHEME 12
READ CYCLE
IF LASTKEY() = 27
ENDIF
RELEASE WINDOW winbuscar
wrk_salir = 1
SET CURSOR ON
RETURN
*
PROCEDURE oolocaliza
PARAMETER wrk_op
SET CURSOR ON
DEFINE WINDOW windato FROM 10, 10  ;
       TO 12, 70
ACTIVATE WINDOW windato
wrk_salir = 1
DO CASE
     CASE wrk_op = 1
          @ 0, 1 SAY  ;
            'C¢digo      :                    '
          @ 0, 16 GET ccodpro  ;
            DEFAULT SPACE(14)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'CODIGO')
          IF ccodpro <> SPACE(14)
               SEEK ALLTRIM(ccodpro)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 3, 0 SAY  ;
            '  Producto         Descripci¢n               Stk.Total   Ul.Compra   Cost.Rep'
          @ 3, 2 SAY 'Producto'  ;
            COLOR - 
     CASE wrk_op = 2
          @ 0, 1 SAY  ;
            'Descripci¢n :                    '
          @ 0, 16 GET cdescrip  ;
            DEFAULT SPACE(40)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'DESCRI')
          IF cdescrip <>  ;
             SPACE(40)
               SEEK ALLTRIM(cdescrip)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 3, 0 SAY  ;
            '  Producto         Descripci¢n               Stk.Total   Ul.Compra   Cost.Rep'
          @ 3, 19 SAY  ;
            'Descripci¢n' COLOR - 
     CASE wrk_op = 3
          @ 0, 1 SAY  ;
            'Modelo      :                    '
          @ 0, 16 GET cmodelo  ;
            DEFAULT SPACE(20)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'MODELO')
          IF cmodelo <> SPACE(14)
               SEEK ALLTRIM(cmodelo)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 1, 0 SAY  ;
            '  C¢digo         Descripci¢n                Modelo      Moneda     Saldo '
          @ 1, 44 SAY 'Modelo'  ;
            COLOR - 
     CASE wrk_op = 4
          @ 0, 1 SAY  ;
            'Moneda      :                    '
          @ 0, 16 GET cmoneda  ;
            DEFAULT SPACE(4)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'MONEDA')
          IF cmoneda <> SPACE(4)
               SEEK ALLTRIM(cmoneda)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 1, 0 SAY  ;
            '  C¢digo         Descripci¢n                Modelo      Moneda     Saldo '
          @ 1, 56 SAY 'Moneda'  ;
            COLOR - 
ENDCASE
RELEASE WINDOW windato
wrk_salir = 0
wrk_order = wrk_op
RETURN
*
PROCEDURE ooerror
PARAMETER merror, mess, mess1,  ;
          mprog, mlineno
DEFINE WINDOW winerror FROM 24,  ;
       69 TO 24, 79 NONE COLOR  ;
       SCHEME 12
ACTIVATE WINDOW SAME winerror
IF mprog = 'DESPUES' .AND. merror =  ;
   3
     RETURN
ENDIF
@ 0, 0 GET nop DEFAULT 1 SIZE 1,  ;
  10, 1 PICTURE '@*HN Continua'  ;
  VALID oocontinua()
READ CYCLE
RELEASE WINDOW winerror
ACTIVATE SCREEN
*
PROCEDURE oocontinua
CLEAR READ
RETURN
RETURN
*
PROCEDURE oobusped
PARAMETER wrk_var
ON KEY
narea = SELECT()
= ooareat('GC_HVT00','CODIGO')
IF RECCOUNT() = 0
     DO p_mensaje WITH  ;
        ' NO HAY PEDIDOS GENERADOS '
     RETURN
ENDIF
SELECT DISTINCT hvt_numdoc + '≥' +  ;
       DTOC(hvt_fecdoc) + '≥' +  ;
       IIF(hvt_indest = 'V',  ;
       'Vigente   ',  ;
       IIF(hvt_indest = 'P',  ;
       'En Proceso',  ;
       IIF(hvt_indest = 'C',  ;
       'Cerrado   ',  ;
       IIF(hvt_indest = 'A',  ;
       'Anulado   ', ' ')))) +  ;
       '≥' + TRANSFORM(hvt_totgen,  ;
       '@ 9,999,999.99') FROM  ;
       gc_hvt00, gc_cli00 ORDER  ;
       BY hvt_fecdoc, hvt_numdoc  ;
       INTO ARRAY arrayorden
ACTIVATE WINDOW marco
ACTIVATE WINDOW busqueda
@ 00, 00 SAY  ;
  '  # Orden     Fecha     Estado      Tot.General    '  ;
  COLOR N/W 
@ 01, 00 GET getorden DEFAULT  ;
  arrayorden SIZE 12, 51 FROM  ;
  arrayorden VALID oovalor(1, ;
  getorden) COLOR SCHEME 8
READ CYCLE
DEACTIVATE WINDOW busqueda, marco
RETURN
*
PROCEDURE ooverorden
PARAMETER wrk_var
ON KEY
= ooareat('GC_HCO00','CODIGO')
IF RECCOUNT() = 0
     DO p_mensaje WITH  ;
        ' NO HAY ORDENES GENERADAS '
     RETURN
ENDIF
SELECT DISTINCT  ;
       TRANSFORM(hco_nrodoc,  ;
       '@ 9999999999') + '≥' +  ;
       DTOC(hco_fecdoc) + '≥' +  ;
       IIF(hco_indest = 'S',  ;
       'Solicitado',  ;
       IIF(hco_indest = 'P',  ;
       'En Proceso',  ;
       IIF(hco_indest = 'C',  ;
       'Cerrado   ',  ;
       IIF(hco_indest = 'A',  ;
       'Anulado   ', ' ')))) +  ;
       '≥' + TRANSFORM(hco_totgen,  ;
       '@ 9,999,999.99') FROM  ;
       gc_hco00 INTO ARRAY  ;
       arrayorden
ACTIVATE WINDOW marco
ACTIVATE WINDOW busqueda
@ 00, 00 SAY  ;
  '  # Orden     Fecha     Estado      Tot.General    '  ;
  COLOR N/W 
@ 01, 00 GET getorden DEFAULT  ;
  arrayorden SIZE 12, 51 FROM  ;
  arrayorden VALID oovalor(1, ;
  getorden) COLOR SCHEME 8
READ CYCLE
DEACTIVATE WINDOW busqueda, marco
RETURN
*
PROCEDURE ooverdocum
PARAMETER wrk_busca, wrk_var
= ooareao('GC_HVE00',1)
DIMENSION arrayorden[ 1]
arrayorden[ 1] = ' '
IF RECCOUNT() = 0
     DO p_mensaje WITH  ;
        ' No Hay Documentos Generados '
     RETURN
ENDIF
SELECT DISTINCT hve_tipdoc + '≥' +  ;
       TRANSFORM(hve_nrodoc,  ;
       '@ 9999999999') + '≥' +  ;
       DTOC(hve_fecdoc) + '≥' +  ;
       IIF(hve_estdoc = 'A',  ;
       'Anulado ', IIF(hve_estdoc =  ;
       'C', 'Cerrado ',  ;
       IIF(hve_estdoc = 'V',  ;
       'Vigente ', '        '))) +  ;
       '≥' + TRANSFORM(hve_totgen,  ;
       '@ 9,999,999.99') FROM  ;
       gc_hve00 WHERE (hve_tipdoc =  ;
       wrk_busca) ORDER BY  ;
       hve_fecdoc INTO ARRAY  ;
       arrayorden
ACTIVATE WINDOW marco
ACTIVATE WINDOW busqueda
@ 0, 0 SAY  ;
  ' Tipo  #Docum     Fecha    Estado   Tot.General '  ;
  COLOR N/W 
@ 1, 0 GET getorden DEFAULT  ;
  arrayorden SIZE 12, 51 FROM  ;
  arrayorden VALID oovalor(2, ;
  getorden)
READ CYCLE
DEACTIVATE WINDOW busqueda, marco
RETURN
*
PROCEDURE oovalor
PARAMETER opc, cdato
DO CASE
     CASE opc = 1
          &wrk_var = substr(cdato,1,10)
     CASE opc = 2
          &wrk_var = substr(cdato,6,10)
ENDCASE
CLEAR READ
RETURN
*
FUNCTION oodescat
PARAMETER ccodcat
narea = SELECT()
wrk_busca = 'CATE'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodcat
IF FOUND()
     wrk_descat = ge_tab0.tab_destab
ELSE
     wrk_descat = ' '
ENDIF
SELECT (narea)
RETURN wrk_descat
*
FUNCTION oodessub
PARAMETER ccodsub
narea = SELECT()
wrk_busca = 'SUCA'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodsub
IF FOUND()
     wrk_dessub = ge_tab0.tab_destab
ELSE
     wrk_dessub = ' '
ENDIF
SELECT (narea)
RETURN wrk_dessub
*
FUNCTION oodessur
PARAMETER ccodsur
narea = SELECT()
wrk_busca = 'SUBR'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodsur
IF FOUND()
     wrk_dessur = ge_tab0.tab_destab
ELSE
     wrk_dessur = ' '
ENDIF
SELECT (narea)
RETURN wrk_dessur
*
PROCEDURE oopopprod
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro, wrk_varrea
PUBLIC aux_salir, arraydato,  ;
       arraymarca, wrk_show,  ;
       aux_varrea
aux_varrea = wrk_varrea
DIMENSION arraydato( 12)
aux_salir = 0
wrk_show = 0
arraydato = SPACE(70)
ON KEY LABEL enter
DEFINE WINDOW winproduc FROM 04,  ;
       20 TO 18, 69 IN screen  ;
       NONE
= ooscreen(1)
SET CURSOR ON
= ooareat('GC_PRO00','CODIGO')
IF (aux_varrea = 'WRK_COD_1')  ;
   .OR. (aux_varrea =  ;
   'WRK_COD_2')
     SET ORDER TO codigo
ELSE
     SET ORDER TO descri
ENDIF
GOTO TOP
SCATTER MEMVAR
= ooinidatos(2,1)
DO WHILE aux_salir=0
     @ 1, 0 GET getarray DEFAULT  ;
       arraydato SIZE 14, 50 FROM  ;
       arraydato VALID  ;
       oodatos(getarray) WHEN  ;
       oorefrepro(getarray) COLOR  ;
       SCHEME 9
     READ CYCLE
     IF aux_salir = 2
          EXIT
     ENDIF
     IF (LASTKEY() = 27) .AND.  ;
        (aux_salir = 0)
          EXIT
     ELSE
          aux_salir = 0
          LOOP
     ENDIF
ENDDO
RELEASE aux_salir, arraydato,  ;
        wrk_show
RELEASE WINDOW winproduc
SELECT (wrk_selec)
ON KEY LABEL pgup
ON KEY LABEL pgdn
RETURN
*
PROCEDURE ooinidatos
PARAMETER wrk_direc, wrk_estado
SET CURSOR ON
ON KEY
RELEASE WINDOW windato
RELEASE WINDOW winbuscar
= ooarea('GC_PRO00')
IF wrk_estado = 0
     wrk_estado = 1
     GOTO TOP
ENDIF
SCATTER MEMVAR
ccodpro = SPACE(14)
cdescrip = SPACE(40)
cmodelo = SPACE(20)
cmoneda = SPACE(4)
wrk_show = 0
wrk_fil = 10
wrk_col = 4
wrk_inc = 1
wrk_tope = 12
wrk_dec = (wrk_tope * 2) - 1
aux_codpro = SPACE(14)
DO WHILE (wrk_inc<=wrk_tope)
     = ooarea('GC_PRO00')
     IF (aux_codpro <>  ;
        pro_codpro)
          aux_codpro = pro_codpro
          IF (aux_varrea =  ;
             'WRK_COD_1') .OR.  ;
             (aux_varrea =  ;
             'WRK_COD_2')
               arraydato(  ;
                        wrk_inc) =  ;
                        m.pro_codpro +  ;
                        '≥' +  ;
                        SUBSTR(m.pro_descri,  ;
                        1, 25) +  ;
                        '≥' +  ;
                        m.pro_unimed
          ELSE
               arraydato(  ;
                        wrk_inc) =  ;
                        SUBSTR(m.pro_descri,  ;
                        1, 25) +  ;
                        '≥' +  ;
                        m.pro_codpro +  ;
                        '≥' +  ;
                        m.pro_unimed
          ENDIF
     ENDIF
     IF (wrk_direc = 2) .AND.  ;
        (wrk_estado = 1)
          SKIP
          IF EOF()
               SKIP -1
          ENDIF
     ENDIF
     IF (wrk_direc = 1) .AND.  ;
        (wrk_estado = 1)
          SKIP -1
          IF BOF()
               SKIP
               wrk_inc = 0
          ELSE
               IF  .NOT. BOF()
                    SKIP -(wrk_dec)
                    wrk_inc = 0
                    wrk_direc = 2
               ENDIF
          ENDIF
     ENDIF
     wrk_inc = wrk_inc + 1
     SCATTER MEMVAR
ENDDO
getarray = arraydato(1)
ON KEY LABEL pgup do ooinidatos with 1,1
ON KEY LABEL pgdn do ooinidatos with 2,1
ON KEY LABEL f8 do oopedido
RETURN
*
PROCEDURE oodatos
PARAMETER wrk_dato
&wrk_campo  = substr(wrk_dato,1,14)
aux_salir = 2
KEYBOARD CHR(27)
RETURN
*
PROCEDURE ooscreen
PARAMETER wrk_screen
DO CASE
     CASE wrk_screen = 1
          ACTIVATE WINDOW  ;
                   winproduc
          IF (aux_varrea =  ;
             'WRK_COD_1') .OR.  ;
             (aux_varrea =  ;
             'WRK_COD_2')
               @ 0, 0 SAY  ;
                 '  Producto         Descripci¢n            U.M.    '  ;
                 COLOR W+/W 
          ELSE
               @ 0, 0 SAY  ;
                 '  Descripci¢n                C¢digo       U.M.    '  ;
                 COLOR W+/W 
          ENDIF
ENDCASE
RETURN
*
PROCEDURE oorefrepro
PARAMETER wrk_dato
IF wrk_show = 0
     SHOW GET getarray
     wrk_show = 1
ENDIF
ON KEY LABEL f6 do oobuscapro
ON KEY LABEL pgup do ooinidatos with 1,1
ON KEY LABEL pgdn do ooinidatos with 2,1
RETURN
*
PROCEDURE oobuscapro
DEFINE WINDOW winbuscar FROM  ;
       INT((SROWS() - 10) / 2),  ;
       INT((SCOLS() - 30) / 2) TO  ;
       INT((SROWS() - 10) / 2) +  ;
       8, INT((SCOLS() - 30) / 2) +  ;
       29 NOFLOAT NOCLOSE  ;
       NOMINIMIZE COLOR SCHEME 1
ACTIVATE WINDOW winbuscar
SET CURSOR ON
@ 0, 0 SAY  ;
  '        Buscar por :        '  ;
  SIZE 1, 28, 0 COLOR N+/W 
@ 2, 6 GET nop1 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE '@*HN \<C¢digo'  ;
  VALID oolocaliza(1) COLOR  ;
  SCHEME 12
@ 3, 6 GET nop2 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE  ;
  '@*HN \<Descripci¢n' VALID  ;
  oolocaliza(2) COLOR SCHEME 12
@ 4, 6 GET nop3 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE '@*HN \<Modelo'  ;
  DISABLE VALID oolocaliza(3)  ;
  COLOR SCHEME 12
@ 5, 6 GET nop4 DEFAULT 1 SIZE 1,  ;
  16, 1 PICTURE '@*HN M\<oneda'  ;
  DISABLE VALID oolocaliza(4)  ;
  COLOR SCHEME 12
READ CYCLE
IF LASTKEY() = 27
ENDIF
RELEASE WINDOW winbuscar
wrk_salir = 1
SET CURSOR ON
RETURN
*
PROCEDURE oolocaliza
PARAMETER wrk_op
SET CURSOR ON
DEFINE WINDOW windato FROM 10, 10  ;
       TO 12, 70
ACTIVATE WINDOW windato
wrk_salir = 1
DO CASE
     CASE wrk_op = 1
          @ 0, 1 SAY  ;
            'C¢digo      :                    '
          @ 0, 16 GET ccodpro  ;
            DEFAULT SPACE(14)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'CODIGO')
          IF ccodpro <> SPACE(14)
               SEEK ALLTRIM(ccodpro)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 3, 0 SAY  ;
            '  Producto         Descripci¢n               Stk.Total   Ul.Compra   Cost.Rep'
          @ 3, 2 SAY 'Producto'  ;
            COLOR - 
     CASE wrk_op = 2
          @ 0, 1 SAY  ;
            'Descripci¢n :                    '
          @ 0, 16 GET cdescrip  ;
            DEFAULT SPACE(40)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'DESCRI')
          IF cdescrip <>  ;
             SPACE(40)
               SEEK ALLTRIM(cdescrip)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 3, 0 SAY  ;
            '  Producto         Descripci¢n               Stk.Total   Ul.Compra   Cost.Rep'
          @ 3, 19 SAY  ;
            'Descripci¢n' COLOR - 
     CASE wrk_op = 3
          @ 0, 1 SAY  ;
            'Modelo      :                    '
          @ 0, 16 GET cmodelo  ;
            DEFAULT SPACE(20)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'MODELO')
          IF cmodelo <> SPACE(14)
               SEEK ALLTRIM(cmodelo)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 1, 0 SAY  ;
            '  C¢digo         Descripci¢n                Modelo      Moneda     Saldo '
          @ 1, 44 SAY 'Modelo'  ;
            COLOR - 
     CASE wrk_op = 4
          @ 0, 1 SAY  ;
            'Moneda      :                    '
          @ 0, 16 GET cmoneda  ;
            DEFAULT SPACE(4)  ;
            PICTURE '@!'
          READ
          = ooareat('GC_PRO00', ;
            'MONEDA')
          IF cmoneda <> SPACE(4)
               SEEK ALLTRIM(cmoneda)
               IF EOF()
                    GOTO BOTTOM
                    = ooinidatos(1, ;
                      1)
               ELSE
                    = ooinidatos(2, ;
                      1)
               ENDIF
          ELSE
               = ooinidatos(2,0)
          ENDIF
          @ 1, 0 SAY  ;
            '  C¢digo         Descripci¢n                Modelo      Moneda     Saldo '
          @ 1, 56 SAY 'Moneda'  ;
            COLOR - 
ENDCASE
RELEASE WINDOW windato
wrk_salir = 0
wrk_order = wrk_op
RETURN
*
PROCEDURE ooerror
PARAMETER merror, mess, mess1,  ;
          mprog, mlineno
DEFINE WINDOW winerror FROM 24,  ;
       29 TO 24, 79 NONE COLOR  ;
       SCHEME 12
IF mprog = 'DESPUES' .AND. merror =  ;
   3
     RETURN
ENDIF
ACTIVATE WINDOW SAME winerror
@ 0, 0 GET nop DEFAULT 1 SIZE 1,  ;
  10, 1 PICTURE  ;
  '@*HN Continua;Cancela;Suspender'  ;
  VALID oocontinua(nop)
READ CYCLE
RELEASE WINDOW winerror
ACTIVATE SCREEN
*
PROCEDURE oocontinua
PARAMETER nop
DO CASE
     CASE nop = 1
          CLEAR READ
     CASE nop = 2
          CANCEL
     CASE nop = 3
          SUSPEND
ENDCASE
RETURN
RETURN
*
PROCEDURE ootablas
PARAMETER nx, ny1, ny2, nlon,  ;
          ccodigo, cvarread
IF (LASTKEY() = 5) .OR.  ;
   (LASTKEY() = 24)
     RETURN
ENDIF
= ooarea('GE_TAB0')
seek ccodigo+&cvarread
IF (LASTKEY() = 13)
     wrk_codigo = ' '
     wrk_descri = ' '
     DEFINE WINDOW winpopup FROM  ;
            nx, ny1 TO nx + 2,  ;
            ny1 + 25 NONE
     SELECT ge_tab0.tab_codtab +  ;
            ' ' +  ;
            ge_tab0.tab_destab  ;
            FROM ge_tab0 WHERE  ;
            ge_tab0.tab_codpre =  ;
            ccodigo ORDER BY  ;
            ge_tab0.tab_codtab  ;
            INTO ARRAY poparray
     ACTIVATE WINDOW winpopup
     @ 0, 0 GET getarray DEFAULT  ;
       1 SIZE 3, 26 FROM poparray  ;
       PICTURE '@^' VALID  ;
       popvalid(getarray) WHEN  ;
       popwhen() COLOR SCHEME 12
     READ CYCLE
     RELEASE WINDOW winpopup
     IF LASTKEY() = 27
          wrk_codigo = SPACE(4)
          wrk_descri = SPACE(20)
          &cvarread   = wrk_codigo
          RETURN
     ENDIF
     wrk_codigo = SUBSTR(poparray(getarray),  ;
                  1, 4)
     wrk_descri = SUBSTR(poparray(getarray),  ;
                  6, nlon)
ELSE
     IF FOUND()
          wrk_codigo = ge_tab0.tab_codtab
          wrk_descri = SUBSTR(ge_tab0.tab_destab,  ;
                       1, nlon)
     ENDIF
ENDIF
@ nx, ny2 SAY wrk_descri
&cvarread = wrk_codigo
*
PROCEDURE popwhen
IF LASTKEY() = 27
     CLEAR READ
     RETURN
ENDIF
KEYBOARD CHR(13)
RETURN
*
PROCEDURE popvalid
PARAMETER npdato
CLEAR READ
RETURN
RETURN
*
FUNCTION oorangos
PARAMETER nx1, ny1, ny2, nlon,  ;
          ccodigo, cvarread
wrk_numrel = oonumrelac()
if (wrk_numrel >= 3);
.and. (empty(&cvarread))
     DO p_mensaje WITH  ;
        ' M†ximo 3 Relaciones '
     RETURN
ENDIF
nvale = 1
IF (LASTKEY() = 13)
     = ootablas(nx1,ny1,ny2,nlon, ;
       ccodigo,cvarread)
ENDIF
DO CASE
     CASE (cvarread =  ;
          'WRK_ALMA_1') .OR.  ;
          (cvarread =  ;
          'WRK_ALMA_2')
          IF EMPTY(wrk_alma_1)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de AlmacÇn '
               nvale = 2
          ELSE
               IF (cvarread =  ;
                  'WRK_ALMA_2')  ;
                  .AND.  ;
                  EMPTY(wrk_alma_2)
                    DO p_mensaje  ;
                       WITH  ;
                       ' Falta Fin de AlmacÇn '
                    nvale = 2
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_LIN_1') .OR.  ;
          (cvarread =  ;
          'WRK_LIN_2')
          IF EMPTY(wrk_lin_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_lin_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Linea '
               wrk_lin_2 = SPACE(4)
               SHOW GET wrk_lin_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_lin_1)  ;
                   .AND.  ;
                   EMPTY(wrk_lin_2)
                    IF cvarread <>  ;
                       'WRK_LIN_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Linea '
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_MAR_1') .OR.  ;
          (cvarread =  ;
          'WRK_MAR_2')
          IF EMPTY(wrk_mar_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_mar_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Marca '
               wrk_mar_2 = SPACE(4)
               SHOW GET wrk_mar_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_mar_1)  ;
                   .AND.  ;
                   EMPTY(wrk_mar_2)
                    IF cvarread <>  ;
                       'WRK_MAR_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Marca '
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_CAT_1') .OR.  ;
          (cvarread =  ;
          'WRK_CAT_2')
          IF EMPTY(wrk_cat_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_cat_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Categoria '
               wrk_cat_2 = SPACE(4)
               SHOW GET wrk_cat_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_cat_1)  ;
                   .AND.  ;
                   EMPTY(wrk_cat_2)
                    IF cvarread <>  ;
                       'WRK_CAT_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Categoria '
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_PRC_1') .OR.  ;
          (cvarread =  ;
          'WRK_PRC_2')
          IF EMPTY(wrk_prc_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_prc_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Procedencia'
               wrk_prc_2 = SPACE(4)
               SHOW GET wrk_prc_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_prc_1)  ;
                   .AND.  ;
                   EMPTY(wrk_prc_2)
                    IF cvarread <>  ;
                       'WRK_PRC_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Procedencia'
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_PRP_1') .OR.  ;
          (cvarread =  ;
          'WRK_PRP_2')
          IF EMPTY(wrk_prp_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_prp_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Propiedad'
               wrk_prp_2 = SPACE(4)
               SHOW GET wrk_prp_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_prp_1)  ;
                   .AND.  ;
                   EMPTY(wrk_prp_2)
                    IF cvarread <>  ;
                       'WRK_PRP_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Procedencia'
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_SCAT_1') .OR.  ;
          (cvarread =  ;
          'WRK_SCAT_2')
          IF EMPTY(wrk_scat_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_scat_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Sub Categoria '
               wrk_scat_2 = SPACE(4)
               SHOW GET  ;
                    wrk_scat_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_scat_1)  ;
                   .AND.  ;
                   EMPTY(wrk_scat_2)
                    IF cvarread <>  ;
                       'WRK_SCAT_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Sub Categoria '
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_RUB_1') .OR.  ;
          (cvarread =  ;
          'WRK_RUB_2')
          IF EMPTY(wrk_rub_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_rub_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Rubro '
               wrk_rub_2 = SPACE(4)
               SHOW GET wrk_rub_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_rub_1)  ;
                   .AND.  ;
                   EMPTY(wrk_rub_2)
                    IF cvarread <>  ;
                       'WRK_RUB_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Rubro '
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
     CASE (cvarread =  ;
          'WRK_SRUB_1') .OR.  ;
          (cvarread =  ;
          'WRK_SRUB_2')
          IF EMPTY(wrk_srub_1)  ;
             .AND.  .NOT.  ;
             EMPTY(wrk_srub_2)
               DO p_mensaje WITH  ;
                  ' Falta Inicio de Sub-Rubro '
               wrk_srub_2 = SPACE(4)
               SHOW GET  ;
                    wrk_srub_2
               nvale = 0
          ELSE
               IF  .NOT.  ;
                   EMPTY(wrk_srub_1)  ;
                   .AND.  ;
                   EMPTY(wrk_srub_2)
                    IF cvarread <>  ;
                       'WRK_SRUB_1'
                         DO p_mensaje  ;
                            WITH  ;
                            ' Falta Fin de Sub-Rubro '
                         nvale = 2
                    ENDIF
               ELSE
                    nvale = 1
               ENDIF
          ENDIF
ENDCASE
IF (nvale = 2)
     RETURN .F.
ENDIF
IF (nvale = 0)
     @ nx1, ny2 SAY REPLICATE(' ',  ;
       nlon)
     RETURN
ENDIF
RETURN
*
FUNCTION oonumrelac
aux_numrel = 0
IF  .NOT. EMPTY(wrk_lin_1) .AND.   ;
    .NOT. EMPTY(wrk_lin_2)
     aux_numrel = aux_numrel + 1
     wrk_ranlin = ' .AND. (PRO_LINEA  >= wrk_Lin_1 .AND. PRO_LINEA  <= wrk_Lin_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_mar_1) .AND.   ;
    .NOT. EMPTY(wrk_mar_2)
     aux_numrel = aux_numrel + 1
     wrk_ranmar = ' .AND. (PRO_MARCA  >= wrk_Mar_1 .AND. PRO_MARCA  <= wrk_Mar_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_cat_1) .AND.   ;
    .NOT. EMPTY(wrk_cat_2)
     aux_numrel = aux_numrel + 1
     wrk_rancat = ' .AND. (PRO_CATEGO >= wrk_Cat_1 .AND. PRO_CATEGO <= wrk_Cat_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_prc_1) .AND.   ;
    .NOT. EMPTY(wrk_prc_2)
     aux_numrel = aux_numrel + 1
     wrk_ranprc = ' .AND. (PRO_PROCED >= wrk_PRC_1 .AND. PRO_PROCED <= wrk_PRC_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_prp_1) .AND.   ;
    .NOT. EMPTY(wrk_prp_2)
     aux_numrel = aux_numrel + 1
     wrk_ranprp = ' .AND. (PRO_PROPIE >= wrk_PRP_1 .AND. PRO_PROPIE <= wrk_PRP_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_scat_1) .AND.   ;
    .NOT. EMPTY(wrk_scat_2)
     aux_numrel = aux_numrel + 1
     wrk_ransca = ' .AND. (PRO_SUBCAT >= wrk_SCat_1 .AND. PRO_SUBCAT <= wrk_SCat_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_rub_1) .AND.   ;
    .NOT. EMPTY(wrk_rub_2)
     aux_numrel = aux_numrel + 1
     wrk_ranrub = ' .AND. (PRO_RUBRO >= wrk_Rub_1 .AND. PRO_RUBRO <= wrk_Rub_2 ) '
ENDIF
IF  .NOT. EMPTY(wrk_srub_1) .AND.   ;
    .NOT. EMPTY(wrk_srub_2)
     aux_numrel = aux_numrel + 1
     wrk_ransru = ' .AND. (PRO_SUBRUB >= wrk_SRub_1 .AND. PRO_SUBRUB <= wrk_SRub_2 ) '
ENDIF
RETURN aux_numrel
*
PROCEDURE ooclientes
DEFINE WINDOW wincliente FROM 4,  ;
       30 TO 15, 77 NOFLOAT  ;
       NOCLOSE NOMINIMIZE COLOR  ;
       SCHEME 1
ACTIVATE WINDOW wincliente
@ 3, 1 SAY 'Raz.Social. :' SIZE 1,  ;
  13, 0
@ 2, 1 SAY 'C¢digo..... :' SIZE 1,  ;
  13, 0
@ 6, 1 SAY 'Distrito... :' SIZE 1,  ;
  13, 0
@ 7, 1 SAY 'TelÇfono... :' SIZE 1,  ;
  13, 0
@ 0, 0 SAY  ;
  '            DATOS DE NUEVO CLIENTE            '  ;
  SIZE 1, 46, 0 COLOR W+/W 
@ 4, 1 SAY 'Calle...... :' SIZE 1,  ;
  13, 0
@ 5, 1 SAY 'Urbaniz.... :' SIZE 1,  ;
  13, 0
@ 7, 22 SAY 'Fecha.. :' SIZE 1, 9,  ;
  0
m.cli_feccre = DATE()
m.cli_usuari = clave
m.cli_fecha = DATE()
m.cli_hora = TIME()
@ 2, 15 GET m.cli_codigo DEFAULT  ;
  ' ' SIZE 1, 9 DISABLE COLOR  ;
  SCHEME 6
@ 3, 15 GET m.cli_razsoc DEFAULT  ;
  ' ' SIZE 1, 30 VALID  ;
  oonombre()
@ 4, 15 GET m.cli_calle DEFAULT  ;
  ' ' SIZE 1, 15
@ 5, 15 GET m.cli_urba DEFAULT  ;
  ' ' SIZE 1, 15
@ 6, 15 GET m.cli_distri DEFAULT  ;
  ' ' SIZE 1, 4
@ 7, 15 GET m.cli_telefo DEFAULT  ;
  0 SIZE 1, 6
@ 7, 32 GET m.cli_feccre SIZE 1,  ;
  8
@ 9, 18 GET nop DEFAULT 1 SIZE 1,  ;
  10, 1 PICTURE '@*HN Regresar'  ;
  VALID oovalcli() COLOR SCHEME  ;
  12
READ CYCLE
RELEASE WINDOW wincliente
*
PROCEDURE oovalcli
APPEND BLANK
GATHER MEMVAR
CLEAR READ
RETURN
*
FUNCTION oonombre
IF EMPTY(m.cli_razsoc)
     DO p_mensaje WITH  ;
        ' ES NECESARIO DEL NOMBRE DEL CLIENTE '
     RETURN .F.
ENDIF
RETURN
RETURN
*
PROCEDURE piensa
DEFINE WINDOW t FROM 20, 76 TO 20,  ;
       76 IN screen NONE
ACTIVATE WINDOW t
DO CASE
     CASE val = 1
          @ 00, 00 SAY '≥'
          val = 2
     CASE val = 2
          @ 00, 00 SAY '/'
          val = 3
     CASE val = 3
          @ 00, 00 SAY '-'
          val = 4
     CASE val = 4
          @ 00, 00 SAY '\'
          val = 1
ENDCASE
RETURN
*
PROCEDURE gra_est
PARAMETER est, tip, doc, itm, ano,  ;
          mes, emi, pro, pvr, cli,  ;
          ven, pag, mto, can
narea = SELECT()
= ooopen('GC_EST00',1)
APPEND BLANK
REPLACE est_indest WITH est
REPLACE est_tipdoc WITH tip
REPLACE est_nrodoc WITH doc
REPLACE est_item WITH itm
REPLACE est_anodoc WITH VAL(ano)
REPLACE est_mesdoc WITH VAL(mes)
REPLACE est_punemi WITH emi
REPLACE est_codpro WITH pro
REPLACE est_codpvr WITH pvr
REPLACE est_codcli WITH cli
REPLACE est_codven WITH ven
REPLACE est_codpag WITH pag
REPLACE est_totmto WITH mto
REPLACE est_totcan WITH can
REPLACE est_usuari WITH clave
REPLACE est_fecha WITH DATE()
REPLACE est_hora WITH  ;
        SUBSTR(TIME(), 1, 5)
SELECT (narea)
RETURN
*
FUNCTION oomovi
PARAMETER cmovi, wrk_tipdoc,  ;
          wrk_nrodoc, wrk_tipalm
carea = SELECT()
= ooareat('GC_HIP00','CODIGO')
SEEK wrk_tipdoc + wrk_nrodoc
IF FOUND()
     wrk_return = hip_almdes +  ;
                  hip_almrec
     SELECT (carea)
ELSE
     SELECT (carea)
     RETURN ' '
ENDIF
RETURN wrk_return
*
PROCEDURE alter
PARAMETER var, wrk_codpro,  ;
          wrk_codlis
CLEAR
x = oodesalma(rge_codalm)
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 TITLE  ;
       ' Ï Productos Alternativos para el Codigo : ' +  ;
       wrk_codpro + ' Ï ' IN  ;
       screen FOOTER  ;
       ' Ï Para la Lista de Precios : ' +  ;
       wrk_codlis +  ;
       ' y Stock del Almacen : ' +  ;
       ALLTRIM(x) + ' Ï ' COLOR  ;
       SCHEME 7
SELECT 1
USE GC_PAL00 ORDER CODIGO
SELECT 2
USE GC_HLP00 ORDER CODIGO
SELECT 3
USE GC_DLP00 ORDER CODIGO
SELECT 4
USE GC_ALM00 ORDER CODIGO
SELECT 5
USE GC_PRO00 ORDER CODIGO
SELECT 4
SET FILTER TO alm_codalm = rge_codalm
SELECT 3
SET FILTER TO dlp_codlis = wrk_codlis
SELECT 1
SET FILTER TO pal_codpro = wrk_codpro
GOTO TOP
COUNT TO mco
IF mco = 0
     DO p_mensaje WITH  ;
        'No Existen Codigos Alternativos'
ELSE
     ON KEY LABEL ENTER DO CAPTURA 
     SET RELATION TO pal_copral INTO gc_dlp00,;
pal_copral INTO gc_alm00, pal_copral INTO;
gc_pro00
     ACTIVATE WINDOW detalle
     BROWSE FIELDS pal_copral :H =  ;
            'Codigo',  ;
            gc_pro00.pro_descri  ;
            :H = ' Descripcion ',  ;
            gc_dlp00.dlp_prsigv  ;
            :H = ' Valor ',  ;
            gc_dlp00.dlp_prcigv  ;
            :H = '  Precio  ',  ;
            stock =  ;
            (gc_alm00.alm_stkfis -  ;
            gc_alm00.alm_stkres)  ;
            :H = ' Stock '  ;
            NOAPPEND NOEDIT IN  ;
            detalle NOLGRID
     DEACTIVATE WINDOW detalle
ENDIF
RETURN
ON KEY
*
PROCEDURE captura
ON KEY
&VAR = GC_PAL00.PAL_COPRAL
SELECT 4
SET FILTER TO
SELECT 3
SET FILTER TO
SELECT 1
SET FILTER TO
DEACTIVATE WINDOW detalle
RETURN
*
PROCEDURE alter1
PARAMETER var, wrk_codpro
CLEAR
x = oodesalma(rge_codalm)
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 TITLE  ;
       ' Ï Productos Alternativos para el Codigo : ' +  ;
       wrk_codpro + ' Ï ' IN  ;
       screen FOOTER  ;
       ' Ï Con Stock del Almacen : ' +  ;
       ALLTRIM(x) + ' Ï ' COLOR  ;
       SCHEME 7
SELECT 1
USE GC_PAL00 ORDER CODIGO
SELECT 4
USE GC_ALM00 ORDER CODIGO
SELECT 5
USE GC_PRO00 ORDER CODIGO
SELECT 4
= ooareat('GC_PAL00','CODIGO')
SET FILTER TO pal_codpro = wrk_codpro
GOTO TOP
= ooareat('GC_ALM00','CODIGO')
SET FILTER TO alm_codalm = rge_codalm
GOTO TOP
= ooareat('GC_PRO00','CODIGO')
SELECT 1
COUNT TO mco
IF mco = 0
     DO p_mensaje WITH  ;
        'No Existen Codigos Alternativos'
ELSE
     ON KEY LABEL ENTER DO CAPTURA 
     SET RELATION TO pal_copral INTO gc_alm00,;
pal_copral INTO gc_pro00
     ACTIVATE WINDOW detalle
     BROWSE FIELDS pal_copral :H =  ;
            'Codigo',  ;
            gc_pro00.pro_descri  ;
            :H = ' Descripcion ',  ;
            stock =  ;
            (gc_alm00.alm_stkfis -  ;
            gc_alm00.alm_stkres)  ;
            :H = ' Stock '  ;
            NOAPPEND NOEDIT IN  ;
            detalle NOLGRID
     DEACTIVATE WINDOW detalle
ENDIF
*
PROCEDURE captura
ON KEY
&VAR = GC_PAL00.PAL_COPRAL
SELECT 4
SET FILTER TO
SELECT 1
SET FILTER TO
DEACTIVATE WINDOW detalle
RETURN
*
FUNCTION facmon
PARAMETER var1, var2
x = SELECT()
= ooareat('GC_cmv00','CODIGO_1')
SEEK DTOC(var1) + '1' + var2
xcam = cmv_tipcac
IF FOUND()
     SELECT (x)
     RETURN xcam
ELSE
     SELECT (x)
     RETURN 1
ENDIF
RETURN
*
FUNCTION ootabl
PARAMETER ccodtab
narea = SELECT()
= ooopen('GE_TAB0',0)
= ooareat('GE_TAB0','CODTAB')
SEEK ccodtab
IF FOUND()
     wrk_despag = ge_tab0.tab_destab
ELSE
     wrk_despag = ' No tiene Descripcion '
ENDIF
SELECT (narea)
RETURN wrk_despag
*
FUNCTION ootab
PARAMETER var1, var2
narea = SELECT()
= ooopen('GE_TAB0',0)
= ooareat('GE_TAB0','CODIGO')
SEEK var1 + var2
IF FOUND()
     wrk_despag = ge_tab0.tab_destab
ELSE
     wrk_despag = ' No tiene Descripcion '
ENDIF
SELECT (narea)
RETURN wrk_despag
*
PROCEDURE stocks
PARAMETER wrk_codpro
narea = SELECT()
ON KEY
DEFINE WINDOW detalle FROM 10, 02  ;
       TO 21, 77 TITLE  ;
       ' Ï Productos Alternativos para el Codigo : ' +  ;
       wrk_codpro + ' Ï ' IN  ;
       screen COLOR SCHEME 7
= ooareat('GC_PRO00','CODIGO')
= ooareat('GC_ALM00','CODIGO')
SET FILTER TO alm_codpro = wrk_codpro
GOTO TOP
COUNT TO mco
IF mco = 0
     DO p_mensaje WITH  ;
        'No Existe el Producto en ningun Almacen'
ELSE
     SET RELATION TO alm_codpro INTO gc_pro00
     ACTIVATE WINDOW detalle
     BROWSE FIELDS  ;
            gc_pro00.pro_codpro  ;
            :H = 'Codigo',  ;
            gc_pro00.pro_descri :  ;
            25 :H =  ;
            ' Descripcion ',  ;
            codigo = ootab('ALMA', ;
            alm_codalm) : 10 :H =  ;
            'Cod.Alm. ', stock =  ;
            (alm_stkfis -  ;
            alm_stkres) :H =  ;
            ' Stock Disponoble'  ;
            NOAPPEND NOEDIT IN  ;
            detalle NOLGRID
     DEACTIVATE WINDOW detalle
ENDIF
SET FILTER TO
= ooareat('GC_PRO00','CODIGO')
SEEK wrk_codpro
DEACTIVATE WINDOW ALL
BROWSE
ON KEY LABEL enter do tomacod
ON KEY LABEL F11 do stocks  with gc_pro00.pro_codpro
ACTIVATE WINDOW produ
RETURN
*
FUNCTION stk_alm
PARAMETER wrk_codpro
narea = SELECT()
= ooareao('GC_ALM00','CODIGO')
SEEK wrk_codpro + rge_codalm
IF FOUND()
     wrk_stk = alm_stkfis -  ;
               alm_stkres
ELSE
     wrk_stk = 0
ENDIF
SELECT (narea)
RETURN wrk_stk
*
FUNCTION oostkalm
PARAMETER wrk_codpro, wrk_codalm
narea = SELECT()
= ooareao('GC_ALM00','CODIGO')
SEEK wrk_codpro + wrk_codalm
IF FOUND()
     wrk_stk = alm_stkfis -  ;
               alm_stkres
ELSE
     wrk_stk = 0
ENDIF
SELECT (narea)
RETURN wrk_stk
*
FUNCTION ooubica
PARAMETER wrk_codpro, wrk_alma
narea = SELECT()
= ooareao('GC_ALM00','CODIGO')
SEEK wrk_codpro + rge_codalm
IF FOUND()
     wrk_ubi = alm_ubicac
ELSE
     wrk_ubi = ' S/U '
ENDIF
SELECT (narea)
RETURN wrk_ubi
*
FUNCTION despro
PARAMETER wrk_codpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK wrk_codpro
wrk_provee = pro_codpve
= ooareat('GC_CLI00','CODIGO')
SEEK 'P' + wrk_provee
wrk_descue = cli_descue
SELECT (narea)
RETURN wrk_descue
*
FUNCTION movstoc
PARAMETER wrk_tipdoc, wrk_nrodoc,  ;
          wrk_stkant, wrk_cantid
narea = SELECT()
ON KEY
= ooareat('GC_hip00','CODIGO')
SEEK wrk_tipdoc + wrk_nrodoc
IF FOUND()
     IF  .NOT. EMPTY(hip_almrec)  ;
         .AND.  .NOT.  ;
         EMPTY(hip_almdes)
          SELECT (narea)
          RETURN '  '
     ENDIF
     IF  .NOT. EMPTY(hip_almrec)  ;
         .AND. EMPTY(hip_almdes)
          SELECT (narea)
          RETURN wrk_stkant +  ;
                 wrk_cantid
     ENDIF
     IF EMPTY(hip_almrec) .AND.   ;
        .NOT. EMPTY(hip_almdes)
          SELECT (narea)
          RETURN wrk_stkant -  ;
                 wrk_cantid
     ENDIF
ENDIF
RETURN
*
FUNCTION ootran
PARAMETER wrk_codpro
wrk_stktra = 0
narea = SELECT()
SELECT *, SUM(gc_tpe00.tpe_cansol)  ;
          FROM GC_TPE00 WHERE  ;
          gc_tpe00.tpe_indreg =  ;
          'P' AND  ;
          gc_tpe00.tpe_codpro  ;
          LIKE wrk_codpro  GROUP  ;
          BY gc_tpe00.tpe_codpro  ;
          ORDER BY  ;
          gc_tpe00.tpe_codpro  ;
          INTO CURSOR QUERY
wrk_stktra = sum_tpe_ca
SELECT (narea)
RETURN wrk_stktra
*
FUNCTION ootc
PARAMETER wrk_fecha, wrk_moneda,  ;
          wrk_mondoc, opc
narea = SELECT()
= ooareat('GC_cmv00','CODIGO_2')
SEEK DTOC(wrk_fecha) + '1' +  ;
     wrk_moneda + wrk_mondoc
IF FOUND()
     wrk_tc = cmv_tipcac
     wrk_tv = cmv_tipcav
ELSE
     IF wrk_mondoc = rge_monbas
          wrk_tc = 1
          wrk_tv = 1
     ELSE
          RETURN -1
     ENDIF
ENDIF
SELECT (narea)
IF opc = '1'
     RETURN wrk_tc
ELSE
     RETURN wrk_tv
ENDIF
*
FUNCTION oocorrel
PARAMETER tipo
narea = SELECT()
DO WHILE .T.
     = ooopen('st_iparg',0)
     IF RLOCK()
          IF tipo = 'FACT'
               IF sys_lptfac =  ;
                  '0'
                    REPLACE sys_numfac  ;
                            WITH  ;
                            sys_numfac +  ;
                            1
                    REPLACE sys_lptfac  ;
                            WITH  ;
                            '1'
                    x = f_ceros(sys_numfac, ;
                        10,1)
                    = ooclose('st_iparg')
                    RETURN x
               ELSE
                    = ooclose('st_iparg')
                    RETURN ' '
               ENDIF
          ENDIF
          IF tipo = 'BOLE'
               IF sys_lptbol =  ;
                  '0'
                    REPLACE sys_nrobol  ;
                            WITH  ;
                            sys_nrobol +  ;
                            1
                    REPLACE sys_lptbol  ;
                            WITH  ;
                            '1'
                    x = f_ceros(sys_nrobol, ;
                        10,1)
                    = ooclose('st_iparg')
                    RETURN x
               ELSE
                    = ooclose('st_iparg')
                    RETURN ' '
               ENDIF
          ENDIF
          EXIT
          = ooclose('st_iparg')
     ENDIF
ENDDO
= ooclose('st_iparg')
RETURN
*
PROCEDURE ooactprn
PARAMETER tipo
narea = SELECT()
DO WHILE .T.
     = ooopen('st_iparg',0)
     IF RLOCK()
          IF tipo = 'FACT'
               REPLACE sys_lptfac  ;
                       WITH '0'
          ENDIF
          IF tipo = 'BOLE'
               REPLACE sys_lptbol  ;
                       WITH '0'
          ENDIF
          = ooclose('st_iparg')
          EXIT
     ENDIF
ENDDO
RETURN
*
PROCEDURE ooredon
PARAMETER x, y
x_int = INT(x)
x_res = x - x_int
x_10 = x_res * 10
x_int10 = INT(x_10)
x_res10 = x_10 - x_int10
IF x_res10 <> 0
     IF x_res10 >= 0.5 
          imp_totgen = x_int +  ;
                       ((x_int10 +  ;
                       1) / 10)
          imp_totigv = y + ((1 -  ;
                       x_res10) /  ;
                       10)
     ELSE
          imp_totgen = x_int +  ;
                       (x_int10 /  ;
                       10)
          imp_totigv = y -  ;
                       (x_res10 /  ;
                       10)
     ENDIF
ENDIF
RETURN
*
FUNCTION oodesccli
PARAMETER cliente
narea = SELECT()
= ooareat('gc_cli00',1)
SEEK 'C' + cliente
IF FOUND()
     tipo = cli_clasif
     = ooareat('ge_tab0',1)
     SEEK 'CATC' + tipo
     IF FOUND()
          RETURN tab_factor
     ENDIF
ENDIF
RETURN 0
*
FUNCTION oodescl2
PARAMETER ccodcl2
narea = SELECT()
wrk_busca = 'C'
= ooopen('ST_ICLPR',1)
= ooareat('ST_ICLPR','CODIGO')
SEEK wrk_busca + ccodcl2
IF FOUND()
     wrk_descl2 = st_iclpr.noment
ELSE
     wrk_descl2 = ' '
ENDIF
SELECT (narea)
RETURN wrk_descl2
*
FUNCTION oomonori
PARAMETER wrk_codpro
narea = SELECT()
= ooareat('GC_PRO00','CODIGO')
SEEK wrk_codpro
IF FOUND()
     RETURN pro_moneda
ENDIF
RETURN
*
FUNCTION rbloquea
PRIVATE var
IF RLOCK()
     RETURN (.T.)
ENDIF
DO WHILE .T.
     IF RLOCK()
          RETURN (.T.)
     ENDIF
     DO p_mensaje WITH  ;
        'REGISTRO BLOQUEADO'
ENDDO
RETURN
*
FUNCTION ooped
PARAMETER wrk_numped
= ooopen('ST_IPREP',1)
SEEK wrk_numped
IF FOUND()
     wrk_numord = numord
     = ooclose('ST_IPREP')
     RETURN wrk_numord
ENDIF
= ooclose('ST_IPREP')
RETURN SPACE(8)
*
FUNCTION oodesmes
PARAMETER nummes
DO CASE
     CASE nummes = 1
          wrk_desmes = 'ENERO'
     CASE nummes = 2
          wrk_desmes = 'FEBRERO'
     CASE nummes = 3
          wrk_desmes = 'MARZO'
     CASE nummes = 4
          wrk_desmes = 'ABRIL'
     CASE nummes = 5
          wrk_desmes = 'MAYO'
     CASE nummes = 6
          wrk_desmes = 'JUNIO'
     CASE nummes = 7
          wrk_desmes = 'JULIO'
     CASE nummes = 8
          wrk_desmes = 'AGOSTO'
     CASE nummes = 9
          wrk_desmes = 'SETIEMBRE'
     CASE nummes = 10
          wrk_desmes = 'OCTUBRE'
     CASE nummes = 11
          wrk_desmes = 'NOVIEMBRE'
     CASE nummes = 12
          wrk_desmes = 'DICIEMBRE'
ENDCASE
RETURN wrk_desmes
*
FUNCTION dtoc2
PARAMETER wk_par
wk_aux = STR(YEAR(wk_par), 4) +  ;
         STR(MONTH(wk_par), 2) +  ;
         STR(DAY(wk_par), 2)
RETURN wk_aux
*
*** 
*** ReFox - retrace your steps ... 
***
