*** 
*** ReFox X  #UK933629  MANRIQUE ORELLANA  MANSOFT SYSTEMS [FP25]
***
*
PROCEDURE esc_indica
PARAMETER f, ind1, ind2, ind3, ind4
DO esc_indic2 WITH f, ind1, ind2, ind3, ind4
ACTIVATE WINDOW trabajo
RETURN
*
FUNCTION control
PARAMETER boleano
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   24 .OR. LASTKEY() = 4 .OR.  ;
   LASTKEY() = 19 .OR. LASTKEY() =  ;
   9 .OR. LASTKEY() = 15
     boleano = 1
     RETURN boleano
ENDIF
RETURN
*
PROCEDURE esc_indic3
PARAMETER f, ind1, ind2, ind3,  ;
          ind4
DO esc_indic2 WITH f, ind1, ind2,  ;
   ind3, ind4
ACTIVATE SCREEN
RETURN
*
PROCEDURE esc_indic2
PARAMETER f, ind1, ind2, ind3, ind4
ACTIVATE WINDOW indicar
cont = 1
DO WHILE cont<=4
     DO CASE
          CASE cont = 1
               ind = LOWER(ind1)
          CASE cont = 2
               ind = LOWER(ind2)
          CASE cont = 3
               ind = LOWER(ind3)
          CASE cont = 4
               ind = LOWER(ind4)
     ENDCASE
     DO CASE
          CASE ind = 'bbb'
               tecla = '      '
               glosa = '           '
          CASE ind = 'arr'
               tecla = '< UP >'
               glosa = 'Re.Pant.   '
          CASE ind = 'aba'
               tecla = '<DOWN>'
               glosa = 'Av.Pant.   '
          CASE ind = 'ayu'
               tecla = ' [F1] '
               glosa = 'Ayuda      '
          CASE ind = 'gra'
               tecla = ' [F2] '
               glosa = 'Grabar     '
          CASE ind = 'ccc'
               tecla = ' [F2] '
               glosa = 'Continuar  '
          CASE ind = 'con'
               tecla = ' [F2] '
               glosa = 'Continuar  '
          CASE ind = 'ing'
               tecla = ' [F2] '
               glosa = 'Ingresar   '
          CASE ind = 'cof'
               tecla = ' [F2] '
               glosa = 'Comenzar   '
          CASE ind = 'pl1'
               tecla = ' [F2] '
               glosa = 'Gen.Poliza '
          CASE ind = 'anu'
               tecla = ' [F2] '
               glosa = 'Anular     '
          CASE ind = 'esx'
               tecla = ' [F2] '
               glosa = 'Cambio Est.'
          CASE ind = 'mod'
               tecla = ' [F3] '
               glosa = 'Modificar  '
          CASE ind = 'ana'
               tecla = ' [F3] '
               glosa = 'Anular     '
          CASE ind = 'des'
               tecla = ' [F3] '
               glosa = 'Desmarcar  '
          CASE ind = 'md2'
               tecla = ' [F3] '
               glosa = 'Mod/Descrip'
          CASE ind = 'md3'
               tecla = ' [F3] '
               glosa = 'Ingresa/Mod'
          CASE ind = 'md4'
               tecla = ' [F3] '
               glosa = 'Ingresar   '
          CASE ind = 'cre'
               tecla = ' [F3] '
               glosa = 'Crea Orden '
          CASE ind = 'mod'
               tecla = ' [F2] '
               glosa = 'Modifica   '
          CASE ind = 'tot'
               tecla = ' [F4] '
               glosa = 'Totales    '
          CASE ind = 'oor'
               tecla = ' [F4] '
               glosa = 'Ord.Repa.  '
          CASE ind = 'eli'
               tecla = ' [F4] '
               glosa = 'Eliminar   '
          CASE ind = 'ela'
               tecla = ' [F4] '
               glosa = 'Eliminar   '
          CASE ind = 'ooe'
               tecla = ' [F5] '
               glosa = 'Estados Ord'
          CASE ind = 'ver'
               tecla = ' [F5] '
               glosa = 'Ver Impres.'
          CASE ind = 'stk'
               tecla = ' [F5] '
               glosa = 'Stocks     '
          CASE ind = 'bus'
               tecla = ' [F6] '
               glosa = 'B£squeda   '
          CASE ind = 'ooa'
               tecla = ' [F7] '
               glosa = 'Dc.Anterior'
          CASE ind = 'imp'
               tecla = ' [F7] '
               glosa = 'Imprimir   '
          CASE ind = 'gri'
               tecla = ' [F7] '
               glosa = 'Grabar/Impr'
          CASE ind = 'pol'
               tecla = ' [F7] '
               glosa = 'Imp/Poliza '
          CASE ind = 'est'
               tecla = ' [F8] '
               glosa = 'Cambiar Est'
          CASE ind = 'cie'
               tecla = ' [F8] '
               glosa = 'Cierre     '
          CASE ind = 'sol'
               tecla = ' [F8] '
               glosa = 'Solicitud  '
          CASE ind = 'rep'
               tecla = ' [F8] '
               glosa = 'Orden Repa.'
          CASE ind = 'tod'
               tecla = ' [F8] '
               glosa = 'Marcar Todo'
          CASE ind = 'inf'
               tecla = ' [F8] '
               glosa = 'Informaci¢n'
          CASE ind = 'gui'
               tecla = ' [F8] '
               glosa = 'Impr. Gu¡a'
          CASE ind = 'otr'
               tecla = ' [F9] '
               glosa = 'Otras Func.'
          CASE ind = 'ign'
               tecla = '[F10] '
               glosa = 'Ignorar    '
          CASE ind = 'arg'
               tecla = '[F10] '
               glosa = 'Argumento  '
          CASE ind = 'ian'
               tecla = '[F11] '
               glosa = 'Item Anter.'
          CASE ind = 'isg'
               tecla = '[F12] '
               glosa = 'Item Sgte. '
          CASE ind = 'cnt'
               tecla = '[F12] '
               glosa = 'Continuar  '
          CASE ind = 'ins'
               tecla = '[INS] '
               glosa = 'Insertar   '
          CASE ind = 'bor'
               tecla = '[DEL] '
               glosa = 'Borrar Car.'
          CASE ind = 'ini'
               tecla = '[HOME]'
               glosa = 'Inicio     '
          CASE ind = 'fin'
               tecla = '[END] '
               glosa = 'Fin        '
          CASE ind = 'ant'
               tecla = '[PgUp]'
               glosa = 'P gina Ant.'
          CASE ind = 'ste'
               tecla = '[PgDn]'
               glosa = 'P gina Sgte'
          CASE ind = 'mbv'
               tecla = '[][]'
               glosa = 'Mover Barra'
          CASE ind = 'rac'
               tecla = '[][]'
               glosa = 'Re/Av Campo'
          CASE ind = 'pag'
               tecla = '[' + CHR(27) + '][' + CHR(26) + ']'
               glosa = 'Otra Pagina'
          CASE ind = 'dej'
               tecla = '[' + CHR(27) + '][' + CHR(26) + ']'
               glosa = 'Dejar Men£ '
          CASE ind = 'mbh'
               tecla = '[' + CHR(27) + '][' + CHR(26) + ']'
               glosa = 'Mover Barra'
          CASE ind = 'sel'
               tecla = '[ÄÙ] '
               glosa = 'Seleccionar'
          CASE ind = 'int'
               tecla = '[ÄÙ] '
               glosa = 'Intro.Campo'
          CASE ind = 'mar'
               tecla = '[ÄÙ] '
               glosa = 'Marcar     '
          CASE ind = 'mdb'
               tecla = '[ÄÙ] '
               glosa = 'Modificar  '
          CASE ind = 'bln'
               tecla = ' [^Y] '
               glosa = 'Borra Linea'
          CASE ind = 'esp'
               tecla = ' [Esp]'
               glosa = 'Marca/Desm.'
          CASE ind = 'esc'
               tecla = '[ESC] '
               glosa = 'Salir      '
     ENDCASE
     DO CASE
          CASE cont = 1
               c = 0
          CASE cont = 2
               c = 19
          CASE cont = 3
               c = 38
          CASE cont = 4
               c = 57
     ENDCASE
     @ f - 1, c SAY tecla
     @ f - 1, c + 7 SAY glosa
     cont = cont + 1
ENDDO
RETURN
*
PROCEDURE esc_modo
PARAMETER ind_modo
DO CASE
     CASE ind_modo = 'C'
          txt_modo = '  CONSULTA  '
     CASE ind_modo = 'E'
          txt_modo = '  ELIMINAR  '
     CASE ind_modo = 'I'
          txt_modo = '  INGRESAR  '
     CASE ind_modo = 'M'
          txt_modo = ' MODIFICAR  '
     CASE ind_modo = 'P'
          txt_modo = '  IMPRIMIR  '
     CASE ind_modo = 'S'
          txt_modo = ' SELECCION  '
     CASE ind_modo = 'A'
          txt_modo = ' INSTALANDO '
     CASE ind_modo = 'T'
          txt_modo = ' TRANSMITE  '
     CASE ind_modo = 'G'
          txt_modo = ' GENERANDO  '
ENDCASE
@ 2, 63 SAY txt_modo COLOR SCHEME 8
RETURN
*
FUNCTION da_nombre
PRIVATE cor
cor = 1
DO WHILE .T.
   ret = 'ORDEN' + LTRIM(STR(cor))
   IF FILE(ret + '.DBF')
      cor = cor + 1
    ELSE
      EXIT
   ENDIF
ENDDO
RETURN ret
*
FUNCTION saycenter
PARAMETER linea, mensaje
colum = INT((76 - LEN(mensaje)) / 2)
@ linea, colum SAY mensaje COLOR SCHEME 8
RETURN .T.
*
FUNCTION error
PARAMETER mensaje
DEFINE WINDOW error FROM 17, 10 TO 17, 66 NONE COLOR SCHEME 8
ACTIVATE WINDOW error
CLEAR
colum = INT((56 - LEN(mensaje)) / 2)
@ 00, colum SAY mensaje COLOR N/W* 
key_aux2 = INKEY(0)
DEACTIVATE WINDOW error
IF key_aux2 == -9
     DO fcinco
ENDIF
RETURN .T.
*
FUNCTION error2
PARAMETER mensaje
DEFINE WINDOW error FROM 42, 10  ;
       TO 42, 66 NONE COLOR  ;
       SCHEME 8
ACTIVATE WINDOW error
CLEAR
colum = INT((56 - LEN(mensaje)) /  ;
        2)
@ 00, colum SAY mensaje COLOR N/W* 
key_aux2 = INKEY(0)
DEACTIVATE WINDOW error
IF key_aux2 == -9
     DO fcinco
ENDIF
RETURN .T.
*
FUNCTION mensa2
PARAMETER mensaje, hacer
IF hacer = 'COLO'
     DEFINE WINDOW error FROM 42,  ;
            10 TO 42, 66 NONE  ;
            COLOR SCHEME 8
     ACTIVATE WINDOW error
     CLEAR
     colum = INT((56 -  ;
             LEN(mensaje)) / 2)
     @ 00, colum SAY mensaje  ;
       COLOR N/W* 
ENDIF
IF hacer = 'SACA'
     DEACTIVATE WINDOW error
     RELEASE WINDOW error
ENDIF
RETURN .T.
*
FUNCTION mensa
PARAMETER mensaje, hacer
IF hacer = 'COLO'
     DEFINE WINDOW error FROM 17,  ;
            10 TO 17, 66 NONE  ;
            COLOR SCHEME 8
     ACTIVATE WINDOW error
     CLEAR
     colum = INT((56 -  ;
             LEN(mensaje)) / 2)
     @ 0, colum SAY mensaje COLOR  ;
       N/W* 
ENDIF
IF hacer = 'SACA'
     DEACTIVATE WINDOW error
     RELEASE WINDOW error
ENDIF
RETURN .T.
*
PROCEDURE d_blanca
@ l1 + d_des,c1 say &cam
@ l1 + d_des, c1 SAY ''
RETURN
*
PROCEDURE d_negra
@ l1 + d_des,c1 say &cam
@ l1 + d_des, c1 SAY ''
RETURN
*
PROCEDURE d_limpia
@ l1, c1 CLEAR TO l2, c2
RETURN
*
PROCEDURE d_listar
i = 1
DO WHILE i<=d_lin .AND.  .NOT.  ;
   EOF()
     @ l1 + i - 1, c1 SAY ' '
     i = i + 1
     SKIP
ENDDO
IF RECCOUNT() > 0
     GOTO d_rec
ENDIF
RETURN
*
PROCEDURE dbed_c
x_1 = l1
x_2 = c1
x_3 = l2
x_4 = c2
x_5 = arrcam
x_6 = fun
x_7 = d_rec
x_8 = d_des
x_9 = d_pag
x_10 = d_lin
x_11 = d_ppal
x_12 = d_inkey
x_13 = nom
x_14 = cam
RETURN
*
PROCEDURE dbed_d
l1 = x_1
c1 = x_2
l2 = x_3
c2 = x_4
arrcam = x_5
fun = x_6
d_rec = x_7
d_des = x_8
d_pag = x_9
d_lin = x_10
d_ppal = x_11
d_inkey = x_12
nom = x_13
cam = x_14
RETURN
*
FUNCTION colocaf6
ACTIVATE WINDOW indicar
@ 01, 19 SAY ' [F6] '
@ 01, 26 SAY 'B£squeda'
ACTIVATE WINDOW trabajo
RETURN .T.
*
FUNCTION sacaf6
ACTIVATE WINDOW indicar
@ 1, 19 SAY '      '
@ 1, 26 SAY '        '
ACTIVATE WINDOW trabajo
RETURN .T.
*
PROCEDURE fcinco
efecin = 2
KEYBOARD '{ESC}' PLAIN
RETURN
*
FUNCTION codnum
PARAMETER codigo
IF codigo <= 0
     DO error WITH  ;
        '** Item debe ser > 0 **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION codalf
PARAMETER codigo
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF LEN(TRIM(codigo)) == 0
     DO error WITH  ;
        '** Item debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION codalf2
PARAMETER codigo
IF LASTKEY() = 5
     RETURN .T.
ENDIF
IF LEN(TRIM(codigo)) == 0
     DO error WITH  ;
        '** Item debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION codalf6
PARAMETER codigo
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
IF LEN(TRIM(codigo)) == 0
     DO error WITH  ;
        '** Item debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
DO sacaf6
RETURN .T.
*
FUNCTION valtab
PARAMETER clave, codig, colu,  ;
          largo
IF LASTKEY() = 5 .OR. LASTKEY() =  ;
   19
     RETURN .T.
ENDIF
IF EMPTY(codig)
     RETURN .F.
ENDIF
SELECT 7
DO usedbf WITH 'ge_tab0',  ;
   'codigo'
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
USE
SELECT 1
DO sacaf6
RETURN .T.
*
FUNCTION valtab9
PARAMETER clave, codig, colu,  ;
          largo
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN 0
ENDIF
SELECT 7
USE SHARED ge_tab0 ORDER codigo
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
USE
SELECT 1
DO sacaf6
RETURN .T.
*
FUNCTION provee
PARAMETER grupo, colu
USE SHARED st_iclpr ORDER CODIGO
codaux = 'P' + STR(grupo, 9)
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** C¢digo Proveedor NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), colu SAY SUBSTR(noment,  ;
  1, 30)
USE
DO sacaf6
RETURN .T.
*
FUNCTION provee2
PARAMETER grupo, colu
boleano = 0
DO control WITH boleano
IF boleano = 1
     boleano = 0
     RETURN -1
ENDIF
USE SHARED st_iclpr ORDER CODIGO
codaux = 'P' + STR(grupo, 11)
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** C¢digo Proveedor NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), colu SAY SUBSTR(noment,  ;
  1, 30)
USE
DO sacaf6
RETURN .T.
*
FUNCTION codtec
PARAMETER grupo
USE SHARED st_ITECN ORDER  ;
    TEC_CODTEC
codaux = grupo
SEEK '&CODAUX'
IF FOUND() .AND. codent <>  ;
   STR(wk_codent, 9)
     USE
     DO error WITH  ;
        '** C¢digo ya EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
RETURN .T.
*
FUNCTION codmod
PARAMETER cod, col, lar
SELECT 3
codaux = wk_codmar + cod
USE SHARED st_imode ORDER CODIGO
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** C¢digo Modelo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
varx5 = mesgar
IF lar > 0
     @ ROW(), col SAY  ;
       SUBSTR(nommod, 1, lar)
ENDIF
USE
SELECT 1
DO sacaf6
RETURN .T.
*
FUNCTION codmod2
PARAMETER cod, col, lar
IF LASTKEY() = 5
     RETURN .T.
ENDIF
SELECT 3
codaux = wk_codmar + cod
USE SHARED st_imode ORDER CODIGO
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** C¢digo Modelo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
varx5 = mesgar
IF lar > 0
     @ ROW(), col SAY  ;
       SUBSTR(nommod, 1, lar)
ENDIF
USE
SELECT 1
DO sacaf6
RETURN .T.
*
FUNCTION cliente
PARAMETER grupo, colu
USE SHARED st_iclpr ORDER CODIGO
codaux = 'C' + STR(grupo, 9)
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     @ 6, 23 CLEAR TO 6, 52
     @ 8, 13 CLEAR TO 12, 52
     DEFINE WINDOW error FROM 17,  ;
            10 TO 17, 66 NONE
     ACTIVATE WINDOW error
     CLEAR
     wk_sinoes = 'N'
     @ 0, 5 SAY  ;
       'C¢digo de Cliente NO EXISTE. Lo Crea ?  (S/N)'  ;
       GET wk_sinoes PICTURE '!'  ;
       VALID wk_sinoes $ 'SN'
     READ
     IF LASTKEY() == 27
          wk_sinoes = 'N'
     ENDIF
     DEACTIVATE WINDOW error
     RETURN .T.
ENDIF
@ ROW(), colu SAY SUBSTR(noment,  ;
  1, 30)
USE
DO sacaf6
RETURN .T.
*
FUNCTION exseri
PARAMETER numero
USE SHARED st_iseri ORDER  ;
    SER_CODMAR
codaux = wk_codmod + wk_numser
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** N£mero Serie NO EXISTE para Modelo **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
RETURN .T.
*
PROCEDURE getparame
SAVE SCREEN TO parpan
@ 0, 48 CLEAR TO 08, 74
@ 0, 48 TO 08, 74
@ 1, 50 SAY  ;
  'Definici¢n C¢digo Barra'
IF varxz = SPACE(4)
     varxz = REPLICATE(CHR(0), 4)
ENDIF
nu1 = ASC(SUBSTR(varxz, 1, 1))
nu2 = ASC(SUBSTR(varxz, 2, 1))
nu3 = ASC(SUBSTR(varxz, 3, 1))
nu4 = ASC(SUBSTR(varxz, 4, 1))
@ 3, 50 SAY 'Comienzo Producto :'  ;
  GET nu1 PICTURE '99' VALID  ;
  codnum(nu1)
@ 4, 50 SAY 'Largo Producto    :'  ;
  GET nu2 PICTURE '99' VALID  ;
  codnum(nu2)
@ 5, 50 SAY 'Comienzo Serie    :'  ;
  GET nu3 PICTURE '99' VALID  ;
  codnum(nu3)
@ 6, 50 SAY 'Largo Serie       :'  ;
  GET nu4 PICTURE '99' VALID  ;
  codnum(nu4)
SET CURSOR ON
READ
SET CURSOR OFF
varxz = CHR(nu1) + CHR(nu2) +  ;
        CHR(nu3) + CHR(nu4)
RESTORE SCREEN FROM parpan
RETURN
*
FUNCTION copia
tot_act = tot_act + 1
IF INT((tot_act * 100) / tot_reg) >  ;
   tot_por
     tot_por = INT((tot_act *  ;
               100) / tot_reg)
     IF INT(tot_por / 2) * 2 ==  ;
        tot_por
          ACTIVATE WINDOW TOP  ;
                   ayu1
          @ 4, 12 SAY (tot_act *  ;
            100) / tot_reg  ;
            PICTURE '999.99'
          ACTIVATE WINDOW trabajo
          @ 13, 11 SAY  ;
            REPLICATE('°',  ;
            tot_por / 2)
     ENDIF
ENDIF
RETURN .T.
*
FUNCTION oocospro
PARAMETER ccodpro
narea = SELECT()
SELECT gc_pro00
SEEK ccodpro
IF FOUND()
     w_coprmo = gc_pro00.pro_coprmo
ELSE
     w_coprmo = 0
ENDIF
SELECT (narea)
RETURN w_coprmo
*
FUNCTION oocosprb
PARAMETER ccodpro
narea = SELECT()
SELECT gc_pro00
SEEK ccodpro
IF FOUND()
     w_coprmb = gc_pro00.pro_coprmb
ELSE
     w_coprmb = 0
ENDIF
SELECT (narea)
RETURN w_coprmb
*
FUNCTION dtoc2
PARAMETER wk_par
wk_aux = STR(YEAR(wk_par), 4) +  ;
         STR(MONTH(wk_par), 2) +  ;
         STR(DAY(wk_par), 2)
RETURN wk_aux
*
PROCEDURE mueve2
PARAMETER key
DO CASE
     CASE key = 5 .AND. des > 1
          des = des - 1
     CASE key = 24 .AND. des < 53 -  ;
          lin
          des = des + 1
     CASE key = 4 .AND. com < 101 -  ;
          anc
          com = com + 1
     CASE key = 19 .AND. com > 1
          com = com - 1
     CASE key = 18
          des = des - lin
          IF des < 1
               des = 1
          ENDIF
     CASE key = 3
          des = des + lin
          IF des > 53 - lin
               des = 53 - lin
          ENDIF
     CASE key = 1
          des = 1
     CASE key = 6
          des = 53 - lin
ENDCASE
FOR i = des TO (lin + des - 1)
     @ i - des, 0 SAY  ;
       SUBSTR(orden(i), com,  ;
       anc)
ENDFOR
RETURN
*
PROCEDURE sys_erro
PARAMETER merror, mess, mprog
DEFINE WINDOW ayu3 FROM 0, 0 TO 5,  ;
       60 SHADOW TITLE  ;
       ' Mensaje de Error ' COLOR  ;
       N/R,W+/N,N/R,W/N,W/N,W+/N, ;
       W+/N,W+/N 
ACTIVATE WINDOW ayu3
@ 1, 2 SAY 'N£mero del Error : ' +  ;
  LTRIM(STR(merror))
@ 2, 2 SAY 'Mensaje de Error : ' +  ;
  mess
hacer = SPACE(30)
IF merror = 1
     hacer = 'Ejecute Opmizacion Archivos'
ENDIF
IF merror = 26
     hacer = 'Optimize El Archivo'
ENDIF
IF merror = 125
     hacer = 'Presione [ÄÄÙ] '
ENDIF
@ 3, 2 SAY 'Recomendacion    : ' +  ;
  hacer
FOR j = 1 TO 10
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          0 TO 5 + j, 60
ENDFOR
FOR j = 1 TO 08
     ZOOM WINDOW ayu3 NORM FROM  ;
          10, j TO 15, 60 + j
ENDFOR
CLEAR TYPEAHEAD
= INKEY(10)
DEACTIVATE WINDOW ayu3
IF merror = 125
     SET DEVICE TO PRINTER
     RETRY
ELSE
     KEYBOARD '{ESC}'
ENDIF
RETURN
*
FUNCTION ordnul
PARAMETER cod
cod = STR(cod, 8)
IF LEN(TRIM(cod)) == 0
     DO error WITH  ;
        '** Item debe ser Ingresado. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
SELECT 1
USE SHARED st_isrep ORDER CODIGO
SEEK '&cod'
IF  .NOT. FOUND()
     USE
     DO error WITH  ;
        '** Error Solicitud NO EXISTE. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF config_prg == 3
     USE
     DO sacaf6
     RETURN .T.
ENDIF
IF SUBSTR(indest, 1, 1) == 'N'
     USE
     DO error WITH  ;
        '** Error Solicitud esta Anulada. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF SUBSTR(indest, 1, 1) <> 'V'
     USE
     DO error WITH  ;
        '** Error Solicitud en Procesos. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF config_prg == 2
     USE
     DO sacaf6
     RETURN .T.
ENDIF
IF coddes == 'P'
     USE
     DO error WITH  ;
        '** Error Solicitud para Presupuesto. **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
USE
DO sacaf6
RETURN .T.
*
FUNCTION tecnico
PARAMETER grupo, colu
USE SHARED st_itecn ORDER CODIGO
codaux = STR(grupo, 9)
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     IF VARREAD() == 'WK_OTEC'
          DO error2 WITH  ;
             '** C¢digo de T‚cnico NO EXISTE **'
     ELSE
          DO error WITH  ;
             '** C¢digo de T‚cnico NO EXISTE **'
     ENDIF
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ ROW(), colu SAY SUBSTR(noment,  ;
  1, 28)
USE
DO sacaf6
RETURN .T.
*
FUNCTION tecnic2
PARAMETER grupo, fila, colu, caso
SELECT st_itecn
SEEK STR(grupo, 9)
IF  .NOT. FOUND()
     IF caso = 1
          DO error WITH  ;
             '** C¢digo de T‚cnico NO EXISTE **'
     ELSE
          DO error2 WITH  ;
             '** C¢digo de T‚cnico NO EXISTE **'
     ENDIF
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
@ fila, colu SAY SUBSTR(noment, 1,  ;
  25)
DO sacaf6
RETURN .T.
*
FUNCTION valtab2
PARAMETER clave, codig, colu,  ;
          largo
SELECT 7
DO usedbf WITH 'ge_tab0',  ;
   'codigo'
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error2 WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
USE
SELECT 1
DO sacaf6
RETURN .T.
*
FUNCTION valtab3
PARAMETER clave, codig, colu,  ;
          largo
IF LASTKEY() = 5
     RETURN .T.
ENDIF
SELECT 7
USE SHARED ge_tab0 ORDER codigo
codaux = clave + codig
SEEK '&codaux'
IF  .NOT. FOUND()
     USE
     DO error2 WITH  ;
        '** C¢digo NO EXISTE **'
     KEYBOARD '{CTRL+Y}' PLAIN
     RETURN .F.
ENDIF
IF colu <> 0
     @ ROW(), colu SAY  ;
       SUBSTR(tab_destab, 1,  ;
       largo)
ENDIF
USE
SELECT 1
DO sacaf6
RETURN .T.
*
PROCEDURE mueve3
PARAMETER key
FOR y = 1 TO LEN(key)
     key1 = ASC(SUBSTR(key, y,  ;
            1))
     DO CASE
          CASE key1 = 5 .AND. des >  ;
               1
               des = des - 1
          CASE key1 = 24 .AND.  ;
               des < 49 - lin
               des = des + 1
          CASE key1 = 18
               des = des - lin
               IF des < 1
                    des = 1
               ENDIF
          CASE key1 = 3
               des = des + lin
               IF des > 49 - lin
                    des = 49 -  ;
                          lin
               ENDIF
          CASE key1 = 1
               des = 1
          CASE key1 = 6
               des = 49 - lin
     ENDCASE
     FOR i = des TO (lin + des -  ;
         1)
          @ i - des, 0 SAY  ;
            SUBSTR(solic(i), com,  ;
            anc)
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE coloca
PARAMETER li, co, de
solic( li) = SUBSTR(solic(li), 1,  ;
     co - 1) + de +  ;
     SUBSTR(solic(li), LEN(de) +  ;
     co)
RETURN
*
PROCEDURE coloca2
PARAMETER li, co, de
solic2( li) = SUBSTR(solic2(li),  ;
      1, co - 1) + de +  ;
      SUBSTR(solic2(li), LEN(de) +  ;
      co)
RETURN
*
PROCEDURE mueve3a
PARAMETER key
FOR y = 1 TO LEN(key)
     key1 = ASC(SUBSTR(key, y,  ;
            1))
     DO CASE
          CASE key1 = 5 .AND.  ;
               des2 > 1
               des2 = des2 - 1
          CASE key1 = 24 .AND.  ;
               des2 < 52 - lin
               des2 = des2 + 1
          CASE key1 = 18
               des2 = des2 - lin
               IF des2 < 1
                    des2 = 1
               ENDIF
          CASE key1 = 3
               des2 = des2 + lin
               IF des2 > 52 - lin
                    des2 = 52 -  ;
                           lin
               ENDIF
          CASE key1 = 1
               des2 = 1
          CASE key1 = 6
               des2 = 52 - lin
     ENDCASE
     FOR i = des2 TO (lin + des2 -  ;
         1)
          @ i - des2, 0 SAY  ;
            SUBSTR(solic2(i), com,  ;
            anc)
     ENDFOR
ENDFOR
RETURN
*
PROCEDURE col_bk1b
DO usedbf WITH 'ge_tab0',  ;
   'codigo'
wk_aux = 'EMIS' + wk_emisor
SEEK '&wk_aux'
IF FOUND()
     wk_nomemi = tab_destab
ELSE
     wk_nomemi = SPACE(35)
ENDIF
wk_aux = 'ESTA' + wk_indest
SEEK '&wk_aux'
wk_estado = tab_destab
wk_aux = 'MONE' + wk_codmon
SEEK '&wk_aux'
IF FOUND()
     wk_nommon = tab_destab
ELSE
     wk_nommon = SPACE(35)
ENDIF
wk_aux = 'DIST' + wk_nomdis
SEEK '&wk_aux'
wk_desdis = tab_destab
wk_aux = 'PROV' + wk_nomciu
SEEK '&wk_aux'
wk_desciu = tab_destab
USE
DO coloca WITH 01, 66,  ;
   STR(wk_numero, 8)
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
DO coloca WITH 08, 25,  ;
   STR(wk_numstk, 9)
wk_aux = 'MARC' + wk_codmar
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 05, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = 'INGA' + wk_indori
USE SHARED ge_tab0 ORDER codigo
seek '&wk_aux'
IF FOUND()
     DO coloca WITH 08, 25,  ;
        SUBSTR(tab_destab, 1,  ;
        30)
ENDIF
wk_aux = wk_codmar + wk_codmod
DO usedbf WITH 'st_imode',  ;
   'CODIGO'
SEEK '&wk_aux'
IF FOUND()
     DO coloca WITH 06, 36,  ;
        SUBSTR(nommod, 1, 30)
ENDIF
USE
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
DO coloca WITH 10, 32, wk_noment
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
DO coloca WITH 20, 20,  ;
   TRANSFORM(wk_abonos,  ;
   '999,999.99')
DO coloca WITH 20, 44, wk_codmon
DO coloca WITH 20, 49,  ;
   SUBSTR(wk_nommon, 1, 15)
DO coloca WITH 21, 20, wk_coddes+ ;
   ' '+IIF(wk_coddes=='R',  ;
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
PROCEDURE col_bk2b
FOR i = 1 TO 06
     DO coloca WITH 41+i, 2,  ;
        wk_observ(i)
ENDFOR
RETURN
*
PROCEDURE col_bk1c
DO coloca WITH 01, 68,  ;
   STR(wk_numero, 8)
DO coloca WITH 05, 20, wk_codmar
DO coloca WITH 05, 25, wk_nommar
DO coloca WITH 06, 20, wk_codmod
DO coloca WITH 06, 36, wk_nommod
DO coloca WITH 07, 20, wk_numser
DO coloca WITH 08, 25,  ;
   STR(wk_numstk, 9)
DO coloca WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca WITH 10, 32, wk_noment
DO coloca WITH 11, 20, wk_nomcal
DO coloca WITH 12, 20, wk_nomdis
DO coloca WITH 12, 25, wk_desdis
DO coloca WITH 13, 20, wk_nomciu
DO coloca WITH 13, 25, wk_desciu
DO coloca WITH 14, 20,  ;
   STR(wk_numte1, 7)
DO coloca WITH 14, 30,  ;
   STR(wk_numte2, 7)
DO coloca WITH 16, 20,  ;
   DTOC(wk_fecemi)
DO coloca WITH 16, 65,  ;
   DTOC(wk_fecven)
DO coloca WITH 18, 20, wk_emisor
DO coloca WITH 18, 25, wk_nomemi
DO coloca WITH 19, 20, wk_indori
DO coloca WITH 19, 25, wk_destia
DO coloca WITH 20, 20,  ;
   TRANSFORM(wk_abonos,  ;
   '999,999.99')
DO coloca WITH 20, 44, wk_codmon
DO coloca WITH 20, 49,  ;
   SUBSTR(wk_nommon, 1, 25)
DO coloca WITH 21, 20,  ;
   STR(wk_tecnic, 9)
DO coloca WITH 21, 30, wk_nomtec
FOR i = 1 TO 10
     DO coloca WITH 24+i, 2,  ;
        pro(i)
     DO coloca WITH 24+i, 17,  ;
        dex(i)
     DO coloca WITH 24+i, 38,  ;
        IIF(can(i)<>0, STR(can(i),  ;
        6, 2), SPACE(6))
     DO coloca WITH 24+i, 45,  ;
        uni(i)
     DO coloca WITH 24+i, 49,  ;
        IIF(pre(i)<>0, STR(pre(i),  ;
        9, 2), SPACE(9))
     DO coloca WITH 24+i, 59,  ;
        IIF(dec(i)<>0, STR(dec(i),  ;
        5, 2), SPACE(5))
     DO coloca WITH 24+i, 60,  ;
        IIF(sto(i)<>0, STR(sto(i),  ;
        5, 0), SPACE(5))
     DO coloca WITH 24+i, 65,  ;
        IIF(tot(i)<>0, STR(tot(i),  ;
        10, 2), SPACE(10))
ENDFOR
DO coloca WITH 39, 03,  ;
   STR(wk_totrep, 10, 2)
DO coloca WITH 39, 14,  ;
   STR(wk_totdes, 05, 2)
DO coloca WITH 39, 20,  ;
   STR(wk_totnet, 10, 2)
DO coloca WITH 39, 31,  ;
   STR(wk_totman, 10, 2)
DO coloca WITH 39, 42,  ;
   STR(wk_totafe, 10, 2)
DO coloca WITH 39, 53,  ;
   STR(wk_totigv, 10, 2)
DO coloca WITH 39, 64,  ;
   STR(wk_totgrl, 10, 2)
RETURN
*
PROCEDURE col_bk2c
FOR i = 1 TO 06
     DO coloca WITH 41+i, 2,  ;
        wk_obspre(i)
ENDFOR
RETURN
*
PROCEDURE versol
ON KEY LABEL F8
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'BBB'
DO esc_indica WITH 2, 'BBB',  ;
   'BBB', 'BBB', 'ESC'
DEFINE WINDOW ayuda FROM 2, 1 TO  ;
       29, 73 IN screen
ACTIVATE WINDOW TOP ayuda
@ 0, 12 SAY 'S I N T O M A S'
@ 0, 45 SAY 'A C C E S O R I O S'
@ 1, 0 SAY REPLICATE(CHR(196),  ;
  35) + 'Â' + REPLICATE(CHR(196),  ;
  35)
FOR i = 1 TO 15
     @ 1 + i, 0 SAY wk_codsin(i) +  ;
       '³' + wk_acceso(i)
ENDFOR
@ 17, 0 SAY REPLICATE(CHR(196),  ;
  35) + 'Á' + REPLICATE(CHR(196),  ;
  35)
@ 18, 10 SAY  ;
  'O B S E R V A C I O N E S'
@ 19, 0 SAY REPLICATE(CHR(196),  ;
  71)
FOR i = 1 TO 6
     @ 19 + i, 0 SAY wk_observ(i)
ENDFOR
SET CURSOR OFF
WAIT ''
SET CURSOR ON
DEACTIVATE WINDOW ayuda
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
ON KEY LABEL F8 do versol
RETURN
*
PROCEDURE popap
PARAMETER fila, columna, vector,  ;
          retorna
SAVE SCREEN TO wk_pantax
ulfila = ALEN(vector) + fila + 1
largo = vector(1)
largo2 = LEN(largo) + columna + 1
DEFINE POPUP ayu1 FROM fila,  ;
       columna TO ulfila, largo2  ;
       SHADOW
FOR i = 1 TO ALEN(vector)
     DEFINE BAR i OF ayu1 PROMPT  ;
            vector(i)
ENDFOR
ON SELECTION POPUP ayu1 do cho_ul with;
prompt(),bar()
ACTIVATE POPUP ayu1 NOWAIT
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
RELEASE POPUP ayu1
ACTIVATE WINDOW trabajo
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE cho_ul
PARAMETER x, opci
IF LASTKEY() = 13
     DEACTIVATE POPUP ayu1
     retorna = opci
ENDIF
IF LASTKEY() = 27
     retorna = 27
ENDIF
RETURN
*
PROCEDURE impresora
PARAMETER sw_impre
DEFINE WINDOW impre FROM 9, 20 TO  ;
       16, 60 SHADOW DOUBLE
IF PRINTSTATUS() = .F. .OR.  ;
   PRINTSTATUS() = .F.
     ACTIVATE WINDOW impre
     ya = .T.
     DO WHILE ya
          ya = .T.
          var_impre = 0
          sw_impre = 0
          @ 00, 01 SAY  ;
            ' Se Ha Producido Un Error de      '
          @ 01, 01 SAY  ;
            ' Impresi¢n, Verifique la Impresora'
          @ 02, 01 SAY  ;
            ' Si Esta Desconectada ¢ Apagada   '
          @ 04, 03 PROMPT  ;
            ' \<Cancela '
          @ 04, 20 PROMPT  ;
            ' \<Reintenta '
          MENU TO var_impre
          DO CASE
               CASE var_impre = 2  ;
                    .AND.  ;
                    PRINTSTATUS() =  ;
                    .T.
                    sw_impre = 1
                    ya = .F.
               CASE var_impre = 2  ;
                    .AND.  ;
                    PRINTSTATUS() =  ;
                    .F.
                    sw_impre = 0
                    ya = .T.
               CASE LASTKEY() =  ;
                    27
                    sw_impre = 0
                    ya = .F.
               CASE var_impre = 1
                    sw_impre = 0
                    ya = .F.
               OTHERWISE
                    sw_impre = 0
                    ya = .T.
          ENDCASE
     ENDDO
     DEACTIVATE WINDOW impre
     RELEASE WINDOW impre
ELSE
     sw_impre = 1
ENDIF
RETURN
*
FUNCTION myfuncw
PARAMETER nmode, ncolumnpos
retval = 1
IF LASTKEY() == -8
     IF dbaux == 0
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'OTR'
          IF rgaux <> 0
               DO esc_indica WITH  ;
                  2, 'MAR', 'IMP',  ;
                  'TOD', 'ESC'
          ELSE
               DO esc_indica WITH  ;
                  2, 'MAR', 'BBB',  ;
                  'TOD', 'ESC'
          ENDIF
          dbaux = 1
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'INI', 'SGT',  ;
             'OTR'
          DO esc_indica WITH 2,  ;
             'MBV', 'FIN', 'ANT',  ;
             'ESC'
          dbaux = 0
     ENDIF
ENDIF
IF LASTKEY() == 13
     retval = 2
ENDIF
IF LASTKEY() == -7
     REPLACE tab_estado WITH  ;
             CHR(251) ALL
     rgaux = RECCOUNT()
     DO pie401c
     retval = 2
ENDIF
IF LASTKEY() == 27 .OR. LASTKEY() == - ;
   6
     retval = 0
ENDIF
RETURN retval
*
PROCEDURE pie401c
IF dbaux == 1
     DO esc_indica WITH 2, 'MAR',  ;
        'IMP', 'TOD', 'ESC'
ENDIF
RETURN
*
PROCEDURE pie401s
IF dbaux == 1
     DO esc_indica WITH 2, 'MAR',  ;
        'BBB', 'TOD', 'ESC'
ENDIF
RETURN
*
PROCEDURE linea
PARAMETER tot
PRIVATE wk_totalan
wk_totalan = 0
FOR wk_mes = 1 TO 12
     wk_totalan = wk_totalan +  ;
                  tot(wk_mes)
ENDFOR
@ con_lin, 00 SAY tot(1) PICTURE  ;
  '9,999,999'
@ con_lin, 09 SAY tot(2) PICTURE  ;
  '9,999,999'
@ con_lin, 17 SAY tot(3) PICTURE  ;
  '9,999,999'
@ con_lin, 27 SAY tot(4) PICTURE  ;
  '9,999,999'
@ con_lin, 37 SAY tot(5) PICTURE  ;
  '9,999,999'
@ con_lin, 47 SAY tot(6) PICTURE  ;
  '9,999,999'
@ con_lin, 57 SAY tot(7) PICTURE  ;
  '9,999,999'
@ con_lin, 67 SAY tot(8) PICTURE  ;
  '9,999,999'
@ con_lin, 77 SAY tot(9) PICTURE  ;
  '9,999,999'
@ con_lin, 87 SAY tot(10) PICTURE  ;
  '9,999,999'
@ con_lin, 97 SAY tot(11) PICTURE  ;
  '9,999,999'
@ con_lin, 107 SAY tot(12)  ;
  PICTURE '9,999,999'
@ con_lin, 118 SAY wk_totalan  ;
  PICTURE '999,999,999.99'
RETURN
*
PROCEDURE limpia
PARAMETER tot
wk_totalan = 0
wk_totalme = 0
FOR wk_mes = 1 TO 12
     tot( wk_mes) = 0
ENDFOR
RETURN
*
PROCEDURE acumm
PARAMETER tot
FOR wk_mes = 1 TO 12
     totmes( wk_mes) =  ;
           totmes(wk_mes) +  ;
           tot(wk_mes)
ENDFOR
RETURN
*
PROCEDURE ayuda3_1
PARAMETER campo, mensaje, clave
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'ESP', 'BBB', 'ESC'
define popup ayu0 from 0,35 to 14,79 promp;
field &campo shadow title mensaje
ON SELECTION POPUP ayu0 do choice0
ACTIVATE POPUP ayu0 NOWAIT
FOR i = 1 TO 24
     IF (i > 10) .AND. (WROWS() >  ;
        40)
          MOVE POPUP ayu0 BY 1, 0
     ENDIF
ENDFOR
FOR i = 1 TO 16
     MOVE POPUP ayu0 BY 0, -2
ENDFOR
ACTIVATE POPUP ayu0
DEACTIVATE POPUP ayu0
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice0
IF LASTKEY() == 32
     IF SUBSTR(codmar, 1, 1) =  ;
        'û'
          REPLACE codmar WITH  ;
                  SPACE(4)
     ELSE
          REPLACE codmar WITH  ;
                  'û   '
     ENDIF
     KEYBOARD '{DNARROW}'
ENDIF
IF LASTKEY() == 13
     DEACTIVATE POPUP ayu0
ENDIF
RETURN
*
FUNCTION mfunrq
PARAMETER nmode, ncolumnpos
retval = 1
IF LASTKEY() == -8
     IF dbaux == 0
          DO esc_indica WITH 1,  ;
             'AYU', 'BBB', 'BBB',  ;
             'OTR'
          IF rgaux <> 0
               DO esc_indica WITH  ;
                  2, 'MAR', 'IMP',  ;
                  'TOD', 'ESC'
          ELSE
               DO esc_indica WITH  ;
                  2, 'MAR', 'BBB',  ;
                  'TOD', 'ESC'
          ENDIF
          dbaux = 1
     ELSE
          DO esc_indica WITH 1,  ;
             'AYU', 'INI', 'SGT',  ;
             'OTR'
          DO esc_indica WITH 2,  ;
             'MBV', 'FIN', 'ANT',  ;
             'ESC'
          dbaux = 0
     ENDIF
ENDIF
IF LASTKEY() == 32
     IF tab_estado == ' '
          REPLACE tab_estado WITH  ;
                  CHR(251)
          rgaux = rgaux + 1
          IF rgaux == 1
               DO p401c
          ENDIF
     ELSE
          REPLACE tab_estado WITH  ;
                  ' '
          rgaux = rgaux - 1
          IF rgaux == 0
               DO p401s
          ENDIF
     ENDIF
     retval = 2
     KEYBOARD '{DNARROW}' PLAIN
ENDIF
IF LASTKEY() == -7
     REPLACE tab_estado WITH  ;
             CHR(251) ALL
     rgaux = RECCOUNT()
     DO p401c
     retval = 2
ENDIF
IF LASTKEY() == 27 .OR. LASTKEY() == - ;
   6
     retval = 0
ENDIF
RETURN retval
*
PROCEDURE p401c
IF dbaux == 1
     DO esc_indica WITH 2, 'ESP',  ;
        'IMP', 'TOD', 'ESC'
ENDIF
RETURN
*
PROCEDURE p401s
IF dbaux == 1
     DO esc_indica WITH 2, 'ESP',  ;
        'BBB', 'TOD', 'ESC'
ENDIF
RETURN
*
PROCEDURE col_or1b
USE SHARED ge_tab0 ORDER codigo
wk_aux = 'ESTA' + wk_indest
SEEK '&wk_aux'
wk_estado = tab_destab
wk_aux = 'ESOR' + wk_esor
SEEK '&wk_aux'
wk_desesor = tab_destab
USE
DO coloca2 WITH 01, 66,  ;
   wk_numero2
DO coloca2 WITH 05, 20, wk_codmar
DO coloca2 WITH 06, 20, wk_codmod
DO coloca2 WITH 07, 20, wk_numser
DO coloca2 WITH 08, 25,  ;
   STR(wk_numstk, 9)
wk_aux = 'MARC' + wk_codmar
USE SHARED ge_tab0 ORDER codigo
SEEK '&wk_aux'
DO coloca2 WITH 05, 25,  ;
   SUBSTR(tab_destab, 1, 30)
wk_aux = wk_codmar + wk_codmod
USE SHARED st_imode ORDER CODIGO
SEEK '&wk_aux'
DO coloca2 WITH 06, 36,  ;
   SUBSTR(nommod, 1, 30)
USE
USE SHARED st_itecn ORDER CODIGO
SEEK wk_codtec
wk_destec = noment
USE
DO coloca2 WITH 10, 20,  ;
   STR(wk_codcli, 11)
DO coloca2 WITH 10, 32, wk_noment
DO coloca2 WITH 12, 20,  ;
   DTOC(wk_fecemi)
DO coloca2 WITH 12, 65,  ;
   DTOC(wk_feccom)
DO coloca2 WITH 14, 20, wk_cod001+ ;
   ' '+IIF(wk_cod001=='R',  ;
   'REPARACION ', 'PRESUPUESTO')
DO coloca2 WITH 15, 20,  ;
   SUBSTR(wk_destec, 1, 40)
DO coloca2 WITH 16, 20, wk_esor
DO coloca2 WITH 16, 25,  ;
   SUBSTR(wk_desesor, 1, 40)
FOR i = 1 TO 15
     DO coloca2 WITH 19+i, 2,  ;
        wk_codsin(i)
ENDFOR
FOR i = 1 TO 15
     DO coloca2 WITH 19+i, 38,  ;
        wk_acceso(i)
ENDFOR
RETURN
*
PROCEDURE col_or2b
FOR i = 1 TO 06
     DO coloca2 WITH 36+i, 2,  ;
        wk_observ(i)
ENDFOR
RETURN
*
PROCEDURE col_or3b
DO coloca2 WITH 45, 2, nota1
DO coloca2 WITH 46, 2, nota2
DO coloca2 WITH 47, 2, nota3
DO coloca2 WITH 48, 2, nota4
DO coloca2 WITH 49, 2, nota5
DO coloca2 WITH 50, 2, nota6
RETURN
*
PROCEDURE pp
PARAMETER numero, me
IF numero = 1
     polo = me +  ;
            ' Use Optimizacion'
     DO error WITH polo
     KEYBOARD '{ESC}'
ENDIF
RETURN
PARAMETER campo1, campo2, mensaje,  ;
          clave
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
DIMENSION op( 2)
op( 1) = 'Ordenados por Numero'
op( 2) = 'Ordenados por Fecha '
DEFINE POPUP ayu1 FROM 0, 0 TO 3,  ;
       21 SHADOW
FOR i = 1 TO 2
     DEFINE BAR i OF ayu1 PROMPT  ;
            op(i)
ENDFOR
ON SELECTION POPUP ayu1 do choice_4 with;
prompt()
ACTIVATE POPUP ayu1 NOWAIT
FOR i = 0 TO 08
     MOVE POPUP ayu1 TO i, 0
ENDFOR
FOR i = 0 TO 28
     MOVE POPUP ayu1 TO 08, i
ENDFOR
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice_4
PARAMETER x, clave
IF LASTKEY() <> 13
     RETURN
ENDIF
FOR i = 1 TO 19
     MOVE POPUP ayu1 BY 0, -1
ENDFOR
FOR i = 1 TO 2
     IF op(i) = x
          EXIT
     ENDIF
ENDFOR
DEFINE WINDOW ayu3 FROM 0, 50 TO  ;
       2, 79 SHADOW
ACTIVATE WINDOW TOP ayu3
wk_numx = 0
wk_fecx = CTOD('')
IF i = 1
     SET ORDER TO 1
     campox = campo1
     @ 0, 1 SAY 'N£mero Desde :'  ;
       GET wk_numx PICTURE  ;
       '99999999'
ELSE
     SET ORDER TO 2
     campox = campo1
     @ 0, 1 SAY 'Fecha  Desde :'  ;
       GET wk_fecx
ENDIF
FOR j = 1 TO 10
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          50 TO 2 + j, 79
ENDFOR
FOR j = 1 TO 16
     ZOOM WINDOW ayu3 NORM FROM  ;
          10, 50 - j TO 12, 79 -  ;
          j
ENDFOR
READ
IF LASTKEY() == 27
     DEACTIVATE POPUP ayu1
     DEACTIVATE WINDOW ayu3
     RETURN
ENDIF
IF i = 1
     wk_inix = STR(wk_numx, 8)
     wk_finx = '99999999'
ELSE
     wk_inix = wk_fecx
     wk_finx = CTOD('31/12/99')
ENDIF
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
browse field cer = " " :H="", uno = &campox;
:H="  Numero  Fecha     Emisor  Entidad  Marca Modelo           Est";
key wk_inix, wk_finx in window ayu4 nowait;
freeze cer  
ACTIVATE WINDOW TOP ayu4
ventana = SUBSTR(DBF(),  ;
          LEN(DBF()) - 11, 8)
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          6 TO 7 + j, 70
     zoom window &ventana norm from -1,-4;
to j,70
ENDFOR
DEACTIVATE POPUP ayu1
DEACTIVATE WINDOW ayu3
ON KEY LABEL ENTER do choice24
BROWSE LAST KEY wk_inix, wk_finx
ON KEY LABEL ENTER
DEACTIVATE WINDOW ayu4
RETURN
*
PROCEDURE choice24
IF LASTKEY() == 13
     wk_aux = ALLTRIM(numdoc)
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
FUNCTION f_yesno1
PARAMETER mens, def
PRIVATE ALL
l = IIF(LEN(mens) < 18, 18,  ;
    LEN(mens))
sdef = IIF(def = .T.,  ;
       '\?\<Ok;\!\<Cancel',  ;
       '\?\<No;\!\<Si')
DEFINE WINDOW wyesno FROM 17, (80 -  ;
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
FUNCTION f_yesno2
PARAMETER mens, def
PRIVATE ALL
l = IIF(LEN(mens) < 18, 18,  ;
    LEN(mens))
sdef = IIF(def = .T.,  ;
       '\?\<Ok;\!\<Cancel',  ;
       '\?\<No;\!\<Si')
DEFINE WINDOW wyesno FROM 36, (80 -  ;
       l) / 2 - 4 TO 41, (80 + l) /  ;
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
PROCEDURE produc
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE WINDOW produ FROM 07, 03  ;
       TO 20, 73 IN screen COLOR  ;
       SCHEME 8
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Nro. de Parte '
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Sub-Categoria '
ON SELECTION POPUP prod do buspro with;
bar(),wrk_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       22, 73 PROMPT FIELDS  ;
       pro_codpro + '³' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '³' + SUBSTR(pro_modelo, 1,  ;
       20) + '³' + pro_numpar IN  ;
       screen COLOR SCHEME 8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with wrk_campo,wrk_selec,wrk_selpro
IF LASTKEY() <> 27
     wrk_campo = gc_pro00.pro_codpro
ENDIF
SELECT (wrk_selec)
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
     @ 00, 00 SAY 'C¢digo :'
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
     wrk_codpro = SPACE(14)
     SET ORDER TO NUMPAR
     @ 00, 00 SAY 'N.Parte :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     wrk_codpro = SPACE(4)
     SET ORDER TO SUBCAT
     @ 00, 00 SAY 'Sub-Cat.:'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK wrk_codpro
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod
     BROWSE FIELDS pro_codpro :R  ;
            :H = 'Cod. Produc.',  ;
            pro_descri :R : 25 :H =  ;
            'Descripci¢n',  ;
            pro_modelo :R : 10 :H =  ;
            'Modelo', pro_codree  ;
            :R :H = 'Reemplazo'  ;
            FREEZE pro_codpro IN  ;
            produ
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
PROCEDURE tomacod
ON KEY
wrk_campo = gc_pro00.pro_codpro
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
PROCEDURE pro2
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro, wrk_nropro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen COLOR  ;
       SCHEME 8
IF wrk_nropro = 1
     DEFINE WINDOW produ FROM 12,  ;
            18 TO 20, 73 IN  ;
            screen COLOR SCHEME  ;
            8
ELSE
     DEFINE WINDOW produ FROM 12,  ;
            18 TO 25, 73 IN  ;
            screen COLOR SCHEME  ;
            8
ENDIF
DEFINE POPUP prod FROM 16, 31
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
ON SELECTION POPUP prod do buspro2 with;
bar(), wrk_selpro, wrk_campo
IF wrk_nropro = 1
     DEFINE POPUP produ FROM 15,  ;
            18 TO 20, 73 PROMPT  ;
            FIELDS pro_codpro +  ;
            '³' +  ;
            SUBSTR(pro_descri, 1,  ;
            20) + '³' +  ;
            SUBSTR(pro_modelo, 1,  ;
            20) IN screen
ELSE
     DEFINE POPUP produ FROM 15,  ;
            18 TO 25, 73 PROMPT  ;
            FIELDS pro_codpro +  ;
            '³' +  ;
            SUBSTR(pro_descri, 1,  ;
            20) + '³' +  ;
            SUBSTR(pro_modelo, 1,  ;
            20) IN screen
ENDIF
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with wrk_campo,wrk_selec,wrk_selpro
IF LASTKEY() <> 27
     wrk_campo = st_imode.codmod
ENDIF
SELECT (wrk_selec)
RETURN
*
PROCEDURE buspro2
PARAMETER bar, wrk_selpro,  ;
          wrk_marca
ON KEY
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, -1
ENDFOR
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY -1, 0
ENDFOR
ACTIVATE WINDOW pide
SELECT (wrk_selpro)
IF BAR() = 1
     wrk_codpro = SPACE(15)
     SET ORDER TO CODIGO
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF BAR() = 2
     wrk_codpro = SPACE(30)
     SET ORDER TO MOD_NOMMOD
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
READ
IF LASTKEY() <> 27
     IF BAR() = 1
          wrk_codigo = 'wrk_marca + wrk_codpro'
     ELSE
          wrk_codigo = 'wrk_codpro'
     ENDIF
     SET NEAR ON
     seek &wrk_codigo
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacod2
     BROWSE FIELDS codmod :R :H =  ;
            'Cod. Modelo.',  ;
            nommod :R : 16 :H =  ;
            'Descripci¢n', codcla  ;
            :R : 10 :H =  ;
            'Cod. Clas.' FREEZE  ;
            codmod NOWAIT IN  ;
            produ
     BROWSE LAST
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
PROCEDURE tomacod2
ON KEY
wrk_campo = st_imode.codmod
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
RETURN
*
FUNCTION oodespro
PARAMETER ccodpro
narea = SELECT()
= ooopen('GC_PRO00',1)
= ooareat('GC_PRO00','CODIGO')
SEEK ccodpro
IF FOUND()
     wrk_despro = gc_pro00.pro_descri
     wrk_unipro = gc_pro00.pro_unimed
ELSE
     wrk_despro = SPACE(40)
     wrk_unipro = SPACE(4)
ENDIF
SELECT (narea)
RETURN wrk_despro
*
FUNCTION oodesemi
PARAMETER ccodemi
narea = SELECT()
wrk_busca = 'EMIS'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodemi
IF FOUND()
     wrk_desemi = ge_tab0.tab_destab
ELSE
     wrk_desemi = ' '
ENDIF
SELECT (narea)
RETURN wrk_desemi
*
PROCEDURE ooopen
PARAMETER carchivo, norden
IF USED(carchivo)
     select &carchivo
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
FUNCTION oodescli
PARAMETER ccodcli
narea = SELECT()
wrk_busca = 'C'
= ooopen('ST_ICLPR',1)
= ooareat('ST_ICLPR','CODIGO')
SEEK wrk_busca + ccodcli
IF FOUND()
     wrk_descli = st_iclpr.noment
ELSE
     wrk_descli = SPACE(30)
ENDIF
= ooclose('ST_ICLPR')
SELECT (narea)
RETURN wrk_descli
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
FUNCTION rbloquea
PRIVATE var
IF RLOCK()
     RETURN (.T.)
ENDIF
DO WHILE .T.
     = INKEY(1)
     IF RLOCK()
          RETURN (.T.)
     ENDIF
ENDDO
RETURN (.F.)
*
FUNCTION oodescla
PARAMETER ccodcla
narea = SELECT()
wrk_busca = 'CLAT'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodcla
IF FOUND()
     wrk_descla = ge_tab0.tab_destab
ELSE
     wrk_descla = ' '
ENDIF
SELECT (narea)
RETURN wrk_descla
*
FUNCTION oodesesp
PARAMETER ccodesp
narea = SELECT()
wrk_busca = 'CODE'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodesp
IF FOUND()
     wrk_desesp = ge_tab0.tab_destab
ELSE
     wrk_desesp = ' '
ENDIF
SELECT (narea)
RETURN wrk_desesp
*
FUNCTION oodesciu
PARAMETER ccodciu
narea = SELECT()
wrk_busca = 'PROV'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodciu
IF FOUND()
     wrk_desciu = ge_tab0.tab_destab
ELSE
     wrk_desciu = ' '
ENDIF
SELECT (narea)
RETURN wrk_desciu
*
FUNCTION oodesest
PARAMETER ccodest
narea = SELECT()
wrk_busca = 'ESOR'
= ooopen('GE_TAB0',1)
= ooareat('GE_TAB0','CODIGO')
SEEK wrk_busca + ccodest
IF FOUND()
     wrk_desest = ge_tab0.tab_destab
ELSE
     wrk_desest = ' '
ENDIF
SELECT (narea)
RETURN wrk_desest
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
     wrk_tc = 1
     wrk_tv = 1
ENDIF
SELECT (narea)
IF opc = '1'
     RETURN wrk_tc
ELSE
     RETURN wrk_tv
ENDIF
*
FUNCTION ootc2
PARAMETER w_fecha, w_moneda,  ;
          w_mondoc, opc
narea = SELECT()
SELECT gc_cmv00
w_tc = 3.25 
w_tv = 3.25 
SELECT (narea)
IF opc = '1'
     RETURN w_tc
ELSE
     RETURN w_tv
ENDIF
RETURN
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
FUNCTION ootab
PARAMETER var1, var2
narea = SELECT()
= ooopen('GE_TAB0',0)
= ooareat('GE_TAB0','CODIGO')
SEEK var1 + var2
IF FOUND()
     wrk_despag = ge_tab0.tab_destab
ELSE
     wrk_despag = SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_despag
*
FUNCTION ootab2
PARAMETER var1, var2
narea = SELECT()
SELECT ge_tab0
SEEK var1 + var2
IF FOUND()
     w_despag = ge_tab0.tab_destab
ELSE
     w_despag = SPACE(30)
ENDIF
SELECT (narea)
RETURN w_despag
*
PROCEDURE pro_stock
USE IN 1 GC_PRO00 ALIAS gc_pro00
USE IN 2 GC_ALM00 ALIAS gc_alm00
SELECT gc_alm00
SET ORDER TO CODIGO
SET FILTER TO alm_codalm = rge_codalm
SELECT gc_pro00
SET ORDER TO CODIGO
GOTO TOP
SET RELATION TO pro_codpro INTO gc_alm00
BROWSE FIELDS pro_codpro,  ;
       pro_descri,  ;
       gc_alm00.alm_stkfis,  ;
       gc_alm00.alm_codalm
RETURN
*
PROCEDURE producxx
PARAMETER wrk_campo, wrk_selec,  ;
          wrk_selpro
ON KEY
wrk_order = ORDER()
ACTIVATE SCREEN
DEFINE WINDOW pide FROM 09, 18 TO  ;
       11, 73 IN screen
DEFINE WINDOW produ FROM 15, 01  ;
       TO 24, 78 IN screen COLOR  ;
       SCHEME 8
DEFINE POPUP prod FROM 16, 31  ;
       COLOR SCHEME 8
DEFINE BAR 1 OF prod PROMPT  ;
       '\<C¢digo '
DEFINE BAR 2 OF prod PROMPT  ;
       '\<Descripci¢n '
DEFINE BAR 3 OF prod PROMPT  ;
       '\<Nro. de Parte '
DEFINE BAR 4 OF prod PROMPT  ;
       '\<Sub-Categoria '
ON SELECTION POPUP prod do busproXX with;
bar(),wrk_selpro
DEFINE POPUP produ FROM 15, 18 TO  ;
       22, 73 PROMPT FIELDS  ;
       pro_codpro + '³' +  ;
       SUBSTR(pro_descri, 1, 20) +  ;
       '³' + SUBSTR(pro_modelo, 1,  ;
       20) IN screen COLOR SCHEME  ;
       8
ON SELECTION POPUP produ deac popup prod
ACTIVATE POPUP prod
DEACTIVATE WINDOW pide, produ
ON KEY LABEL f6 do produc with wrk_campo,wrk_selec,wrk_selpro
IF LASTKEY() <> 27
     wrk_campo = gc_pro00.pro_codpro
ENDIF
SELECT (wrk_selec)
CLOSE DATABASES
RETURN
*
PROCEDURE busproxx
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
     wrk_indice = 'CODIGO'
     @ 00, 00 SAY 'C¢digo :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 2
     wrk_codpro = SPACE(40)
     wrk_indice = 'DESCRI'
     @ 00, 00 SAY 'Descr. :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 3
     wrk_codpro = SPACE(14)
     wrk_indice = 'NUMPAR'
     @ 00, 00 SAY 'N.Parte :'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
IF bar = 4
     wrk_codpro = SPACE(4)
     wrk_indice = 'SUBCAT'
     @ 00, 00 SAY 'Sub-Cat.:'
     @ 00, 09 GET wrk_codpro  ;
       PICTURE '@!'
ENDIF
SELECT gc_alm00
SET ORDER TO CODIGO
SET FILTER TO alm_codalm = rge_codalm
SELECT gc_pro00
SET ORDER TO WRK_INDICE
SET RELATION TO pro_codpro INTO gc_alm00
READ
IF LASTKEY() <> 27
     SET NEAR ON
     SEEK wrk_codpro
     SET NEAR OFF
     ACTIVATE WINDOW produ
     ON KEY LABEL enter do tomacodXX
     BROWSE FIELDS  ;
            gc_pro00.pro_codpro  ;
            :R :H =  ;
            'Cod. Produc.',  ;
            gc_pro00.pro_descri  ;
            :R : 27 :H =  ;
            'Descripci¢n',  ;
            gc_alm00.alm_stkfis  ;
            :R : 5 :H = 'Stock'  ;
            :P = '9,999',  ;
            gc_pro00.pro_modelo  ;
            :R : 10 :H = 'Modelo',  ;
            gc_pro00.pro_codree  ;
            :R :H = 'Reemplazo'  ;
            FREEZE pro_codpro IN  ;
            produ
ENDIF
ON KEY
DEACTIVATE WINDOW pide, muestr,  ;
           produ
FOR wrk_cont = 1 TO 07
     MOVE POPUP prod BY 1, 0
ENDFOR
FOR wrk_cont = 1 TO 26
     MOVE POPUP prod BY 0, 1
ENDFOR
RETURN
*
PROCEDURE tomacodxx
ON KEY
wrk_campo = gc_pro00.pro_codpro
DEACTIVATE WINDOW pide, produ
DEACTIVATE POPUP prod
CLOSE DATABASES
RETURN
*
FUNCTION empleado
DEFINE WINDOW empleado FROM 38,  ;
       35 TO 42, 75
ACTIVATE WINDOW empleado
STORE SPACE(5) TO wrk_codemp
DO WHILE .T.
     SET CURSOR ON
     @ 01, 02 SAY 'Usuario :' GET  ;
       wrk_codemp PICTURE '@!'
     READ
     IF LASTKEY() = 27
          USE
          DEACTIVATE WINDOW  ;
                     empleado
          RETURN .F.
     ENDIF
     USE gc_vnd00 ORDER CODIGO
     SEEK 'A' + wrk_codemp
     IF  .NOT. FOUND()
          DO error2 WITH  ;
             '*** C¢digo de Empleado NO EXISTE ***'
          LOOP
     ENDIF
     @ 01, 17 SAY  ;
       SUBSTR(vnd_nombre, 1, 20)
     USE
     RETURN .T.
ENDDO
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
FUNCTION oodestec
PARAMETER ccodtec
narea = SELECT()
= ooopen('ST_ITECN',1)
= ooareat('ST_ITECN','CODIGO')
SEEK ccodtec
IF FOUND()
     wrk_destec = st_itecn.noment
ELSE
     wrk_destec = ' '
ENDIF
= ooclose('ST_ITECN')
SELECT (narea)
RETURN wrk_destec
*
FUNCTION oodeste2
PARAMETER ccodtec
narea = SELECT()
SELECT st_itecn
SEEK ccodtec
IF FOUND()
     w_destec = st_itecn.noment
ELSE
     w_destec = ' '
ENDIF
SELECT (narea)
RETURN w_destec
*
FUNCTION oodesusu
PARAMETER ccodusu
narea = SELECT()
SELECT password
SEEK ccodusu
IF FOUND()
     w_desusu = password.nombre
ELSE
     w_desusu = ' '
ENDIF
SELECT (narea)
RETURN w_desusu
*
FUNCTION ooserie
PARAMETER wrk_marca, wrk_modelo,  ;
          wrk_numser
narea = SELECT()
= ooopen('ST_ISERI',2)
= ooareat('ST_ISERI', ;
  'SER_CODMAR')
SEEK wrk_marca + wrk_modelo +  ;
     wrk_numser
IF FOUND()
     wrk_desser = st_iseri.docgar +  ;
                  ' ' +  ;
                  DTOC(st_iseri.fecvta)
ELSE
     wrk_desser = ' '
ENDIF
= ooclose('ST_ISERI')
SELECT (narea)
RETURN wrk_desser
*
FUNCTION stk_alm
PARAMETER wrk_codpro
narea = SELECT()
= ooopen('GC_ALM00','CODIGO')
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
FUNCTION oodessin
PARAMETER wrk_codcla, wrk_codsin
narea = SELECT()
= ooopen('ST_ISINT','CODIGO')
SEEK wrk_codcla + wrk_codsin
IF FOUND()
     wrk_dessin = dessin
ELSE
     wrk_dessin = SPACE(43)
ENDIF
= ooclose('ST_ISINT')
SELECT (narea)
RETURN wrk_dessin
*
FUNCTION ootipdes
PARAMETER wrk_numdoc
narea = SELECT()
= ooopen('ST_ISREP',0)
= ooareat('ST_ISREP','CODIGO')
SEEK wrk_numdoc
IF FOUND()
     wrk_destin = st_isrep.coddes
ELSE
     wrk_destin = SPACE(1)
ENDIF
= ooclose('ST_ISREP')
SELECT (narea)
RETURN wrk_destin
*
FUNCTION oodestel
PARAMETER ccodcli
narea = SELECT()
wrk_busca = 'C'
= ooopen('ST_ICLPR',1)
= ooareat('ST_ICLPR','CODIGO')
SEEK wrk_busca + ccodcli
IF FOUND()
     wrk_descli = st_iclpr.numte1
ELSE
     wrk_descli = SPACE(7)
ENDIF
= ooclose('ST_ICLPR')
SELECT (narea)
RETURN wrk_descli
*
FUNCTION f_archivo
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' + LTRIM(STR(cor))
     IF FILE(ret + '.DBF')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
FUNCTION ootabla
PARAMETER var1, var2, opc
narea = SELECT()
= ooopen('GE_TAB0',0)
= ooareat('GE_TAB0','CODIGO')
SEEK var1 + var2
IF FOUND()
     DO CASE
          CASE opc = 1
               wrk_valtab = ge_tab0.tab_destab
          CASE opc = 2
               wrk_valtab = ge_tab0.tab_factor
          OTHERWISE
               wrk_valtab = SPACE(1)
     ENDCASE
ELSE
     wrk_valtab = SPACE(1)
ENDIF
SELECT (narea)
RETURN wrk_valtab
*
FUNCTION oonumlet
PARAMETER ncifra, w_codmon
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
IF w_codmon = 'DOL '
     numero = 'SON: ' + numero +  ;
              'Y ' + deci +  ;
              '/100 DOLARES AMERICANOS'
ELSE
     numero = 'SON: ' + numero +  ;
              'Y ' + deci +  ;
              '/100 NUEVOS SOLES'
ENDIF
RETURN (numero)
*
FUNCTION oofchest
PARAMETER wrk_numord, wrk_estado
narea = SELECT()
= ooopen('ST_MVORD',3)
= ooareat('ST_MVORD','ESTADO')
SEEK wrk_numord + wrk_estado
IF FOUND()
     wrk_fecest = st_mvord.dia
ELSE
     wrk_fecest = {}
ENDIF
= ooclose('ST_MVORD')
SELECT (narea)
RETURN wrk_fecest
*
PROCEDURE usedbf
PARAMETER dbf, indice
IF  .NOT. USED(dbf)
     SELECT 0
     use &dbf order &indice shared
ELSE
     SELECT (dbf)
     set order to &indice	shared
ENDIF
*
FUNCTION oodescla
PARAMETER ccodcla
narea = SELECT()
= ooopen('st_imode',4)
= ooareat('st_imode','CLASE')
SEEK ALLTRIM(ccodcla)
IF FOUND()
     wrk_descla = st_imode.nommod
ELSE
     wrk_descla = 'no hay ' +  ;
                  ccodcla +  ;
                  SPACE(30)
ENDIF
SELECT (narea)
RETURN wrk_descla
*
FUNCTION oonord
PARAMETER wrk_orde
SELECT st_iorep
SEEK wrk_orde
IF FOUND()
     wrk_ord = st_iorep.numdoc
ELSE
     wrk_ord = SPACE(8)
ENDIF
RETURN wrk_ord
*
FUNCTION f_indice
PRIVATE cor
cor = 1
DO WHILE .T.
     ret = 'TEMPO' +  ;
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
     ret = 'TEMPO' +  ;
           LTRIM(STR(cor))
     IF FILE(ret + '.TXT')
          cor = cor + 1
     ELSE
          EXIT
     ENDIF
ENDDO
RETURN ret
*
PROCEDURE emplea
IF SROWS() <= 25
     DEFINE WINDOW empleado FROM  ;
            18, 35 TO 20, 70
ELSE
     DEFINE WINDOW empleado FROM  ;
            38, 35 TO 40, 70
ENDIF
ACTIVATE WINDOW empleado
STORE SPACE(5) TO w_codemp
STORE .T. TO fin
CLEAR GETS
DO WHILE fin
     @ 00, 02 SAY 'Usuario :' GET  ;
       w_codemp PICTURE '@!'
     READ
     IF LASTKEY() = 27
          STORE .F. TO fin
     ELSE
          SELECT 20
          USE SHARED gc_vnd00  ;
              ORDER codigo
          SEEK 'A' + w_codemp
          IF  .NOT. FOUND()
               IF WROWS() <= 25
                    DO error WITH  ;
                       '*** C¢digo de Empleado No existe ***'
               ELSE
                    DO error2  ;
                       WITH  ;
                       '*** C¢digo de Empleado No existe ***'
               ENDIF
               LOOP
          ENDIF
          w_nomemp = vnd_nombre
          @ 00, 15 SAY  ;
            SUBSTR(w_nomemp, 1,  ;
            20)
          STORE .F. TO fin
     ENDIF
ENDDO
DEACTIVATE WINDOW empleado
RELEASE WINDOW empleado
RETURN
*
FUNCTION facigv
w_selec = SELECT()
SELECT ge_tab0
SEEK 'IGV ' + 'IGV '
IF  .NOT. FOUND()
     w_facigv = 0
ELSE
     w_facigv = tab_factor / 100
ENDIF
SELECT (w_selec)
RETURN w_facigv
*
FUNCTION ooseri2
PARAMETER w_marca, w_modelo,  ;
          w_numser
w_selec = SELECT()
SELECT st_iseri
SEEK w_marca + w_modelo +  ;
     w_numser
IF FOUND()
     w_doctia = DTOC(st_iseri.fecvta) +  ;
                ' ' +  ;
                ALLTRIM(docgar)
ELSE
     w_doctia = ''
ENDIF
SELECT (w_selec)
RETURN w_doctia
*
PROCEDURE ayuda40
PARAMETER campo1, wrk_origen
ACTIVATE WINDOW indicar
SAVE SCREEN TO wk_pantax
ACTIVATE WINDOW trabajo
DO esc_indica WITH 1, 'AYU',  ;
   'BBB', 'BBB', 'SEL'
DO esc_indica WITH 2, 'MBV',  ;
   'BBB', 'BBB', 'ESC'
DIMENSION op( 6)
op( 1) =  ;
  ' Ordenados por Numero  '
op( 2) =  ;
  ' Ordenados por Fecha   '
op( 3) =  ;
  ' Ordenados por Serie   '
op( 4) =  ;
  ' Ordenados por S/Serv. '
op( 5) =  ;
  ' Ordenados por Cliente '
op( 6) =  ;
  ' Ordenados por Cod. Cliente '
DEFINE POPUP ayu1 FROM 0, 0  ;
       SHADOW COLOR SCHEME 8
FOR i = 1 TO 6
     DEFINE BAR i OF ayu1 PROMPT  ;
            op(i)
ENDFOR
ON SELECTION POPUP ayu1 do choice_4 with;
prompt()
ACTIVATE POPUP ayu1 NOWAIT
FOR i = 0 TO 08
     MOVE POPUP ayu1 TO i, 0
ENDFOR
FOR i = 0 TO 28
     MOVE POPUP ayu1 TO 08, i
ENDFOR
ACTIVATE POPUP ayu1
DEACTIVATE POPUP ayu1
ACTIVATE WINDOW trabajo
ACTIVATE WINDOW indicar
RESTORE SCREEN FROM wk_pantax
ACTIVATE WINDOW trabajo
RETURN
*
PROCEDURE choice_4
PARAMETER x, clave
IF LASTKEY() <> 13
     RETURN
ENDIF
FOR i = 1 TO 19
     MOVE POPUP ayu1 BY 0, -1
ENDFOR
FOR i = 1 TO 6
     IF op(i) = x
          EXIT
     ENDIF
ENDFOR
DEFINE WINDOW ayu3 FROM 0, 50 TO  ;
       2, 79 SHADOW
ACTIVATE WINDOW TOP ayu3
DO CASE
     CASE i = 1
          SET ORDER TO codigo
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'O'
          @ 0, 1 SAY  ;
            'Numero Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '99999999'
     CASE i = 2
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO pre_fecemi
               CASE wrk_origen =  ;
                    'OR'
                    SET ORDER TO ord_fchemi
          ENDCASE
          campox = campo1
          wrk_codigo = DATE()
          wrk_tipo = 'D'
          @ 0, 1 SAY  ;
            'Fecha  Desde :' GET  ;
            wrk_codigo
     CASE i = 3
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO st_numser
               CASE wrk_origen =  ;
                    'OR'
                    SET ORDER TO ord_numser
          ENDCASE
          campox = campo1
          wrk_codigo = SPACE(20)
          wrk_tipo = 'S'
          @ 0, 1 SAY  ;
            'Serie  Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '@!'
     CASE i = 4
          DO CASE
               CASE wrk_origen =  ;
                    'PR'
                    SET ORDER TO st_numsol
               CASE wrk_origen =  ;
                    'OR'
                    SET ORDER TO ord_numsol
          ENDCASE
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'N'
          @ 0, 1 SAY  ;
            'N£mero Desde :' GET  ;
            wrk_codigo PICTURE  ;
            '99999999'
     CASE i = 5
          DEACTIVATE WINDOW ayu3
          campox = campo1
          wrk_tipo = 'C'
          DEACTIVATE POPUP ayu1
          DEFINE WINDOW ingrcli  ;
                 FROM 14, 20 TO  ;
                 16, 51 SHADOW
          STORE SPACE(20) TO  ;
                cliente
          ACTIVATE WINDOW ingrcli
          ON KEY LABEL ESCAPE DO SALIDA
          @ 00, 00 SAY  ;
            'Cliente :' GET  ;
            cliente PICTURE '@!'
          READ
          IF EMPTY(cliente)
               DO mensa2 WITH  ;
                  '   Espere Un Momento...',  ;
                  'SACA'
               DEACTIVATE WINDOW  ;
                          ingrcli
               DO salida
               IF SROWS() <= 25
                    DO mensa WITH  ;
                       '   Espere Un Momento...',  ;
                       'SACA'
               ELSE
                    DO mensa2  ;
                       WITH  ;
                       '  Espere Un Momento...',  ;
                       'SACA'
                    DEACTIVATE WINDOW  ;
                               error
                    RELEASE WINDOW  ;
                            error
               ENDIF
               RETURN
          ENDIF
          DEACTIVATE WINDOW  ;
                     ingrcli
          IF SROWS() <= 25
               DO mensa WITH  ;
                  '  Espere Un Momento...',  ;
                  'COLO'
          ELSE
               DO mensa2 WITH  ;
                  '  Espere Un Momento...',  ;
                  'COLO'
          ENDIF
     CASE i = 6
          SET ORDER TO codent
          campox = campo1
          wrk_codigo = 0
          wrk_tipo = 'Z'
          @ 0, 1 SAY  ;
            'Cod.Cliente :' GET  ;
            wrk_codigo PICTURE  ;
            '999999999'
ENDCASE
FOR j = 1 TO 10
     ZOOM WINDOW ayu3 NORM FROM j,  ;
          50 TO 2 + j, 79
ENDFOR
FOR j = 1 TO 16
     ZOOM WINDOW ayu3 NORM FROM  ;
          10, 50 - j TO 12, 79 -  ;
          j
ENDFOR
IF wrk_tipo <> 'C'
     READ
ENDIF
IF LASTKEY() == 27
     IF SROWS() <= 25
          DO mensa WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ELSE
          DO mensa2 WITH  ;
             '  Espere Un Momento...',  ;
             'SACA'
     ENDIF
     DEACTIVATE WINDOW error
     RELEASE WINDOW error
     DEACTIVATE POPUP ayu1
     DEACTIVATE WINDOW ayu3
     RETURN
ENDIF
SET NEAR ON
DO CASE
     CASE wrk_tipo = 'O'
          SEEK STR(wrk_codigo, 8)
     CASE wrk_tipo = 'D'
          SEEK dtoc2(wrk_codigo)
     CASE wrk_tipo = 'S'
          SEEK wrk_codigo
     CASE wrk_tipo = 'N'
          SEEK STR(wrk_codigo, 8)
     CASE wrk_tipo = 'C'
          SET NEAR OFF
          DO nombre
          RETURN
     CASE wrk_tipo = 'Z'
          SEEK STR(wrk_codigo, 9)
ENDCASE
SET NEAR OFF
DEFINE WINDOW ayu4 FROM 6, 6 TO 7,  ;
       70 SHADOW
IF wrk_origen = 'SS'
     browse field cer = " " :H="", uno;
= &campox :H=" Numero   Fecha     Serie       Cod.Cliente  Modelo     Est. Atenci¢n";
in window ayu4 nowait  freeze cer 
ELSE
     browse field cer = " " :H="", uno;
= &campox :H=" N£mero  Fecha     S/Serv. Serie         Cod.Cli. Modelo    Est Atenci¢n";
in window ayu4 nowait  freeze cer 
ENDIF
ACTIVATE WINDOW TOP ayu4
ventana = SUBSTR(DBF(),  ;
          LEN(DBF()) - 11, 8)
FOR j = 1 TO 08
     ZOOM WINDOW ayu4 NORM FROM 6,  ;
          02 TO 7 + j, 75
     zoom window &ventana norm from -1,-4;
to j,70
ENDFOR
DEACTIVATE POPUP ayu1
DEACTIVATE WINDOW ayu3
ON KEY LABEL ENTER do choice24
BROWSE LAST
ON KEY LABEL ENTER
DEACTIVATE WINDOW ayu4
RETURN
*
PROCEDURE choice24
IF LASTKEY() == 13
     wk_aux = ALLTRIM(numdoc)
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
PROCEDURE nombre
IF  .NOT. USED('st_iclpr')
     SELECT 0
     USE st_iclpr ORDER  ;
         cli_noment
ELSE
     SELECT st_iclpr
     SET ORDER TO cli_noment
ENDIF
SEEK 'C' + ALLTRIM(cliente)
mnomcli = LEFT(noment, 23)
rr = RECNO()
x = 1
SKIP -50
DIMENSION cliente( 100, 2)
DO WHILE (x<=100)
     IF indent = 'C'
          cliente( x, 1) = codent
          cliente( x, 2) = noment
          x = x + 1
     ENDIF
     IF  .NOT. EOF()
          SKIP
     ELSE
          xx = x - 5
          DIMENSION clientes( 1)
          STORE SPACE(10) TO  ;
                clientes( 1)
          EXIT
     ENDIF
ENDDO
xx = x - 1
IF  .NOT. USED('ST_IOREP')
     SELECT 0
     USE ST_IOREP ORDER Codent
ELSE
     SELECT st_iorep
     SET ORDER TO CODENT
ENDIF
GOTO TOP
cli = 1
FOR x = 1 TO xx
     busca_cli = cliente(x,1)
     SEEK (cliente(x,1))
     IF  .NOT. FOUND()
          LOOP
     ENDIF
     DO WHILE codent=cliente(x,1)
          DIMENSION clientes(  ;
                    cli)
          clientes( cli) =  ;
                  LEFT(cliente(x, ;
                  2), 23) + '³' +  ;
                  numdoc + '³' +  ;
                  DTOC(fecemi) +  ;
                  '³' + numsol +  ;
                  '³' +  ;
                  LEFT(numser,  ;
                  10) + '³' +  ;
                  codmod
          cli = cli + 1
          SKIP
          IF (cliente(x,1)) <>  ;
             codent
               EXIT
          ENDIF
     ENDDO
ENDFOR
cl = ASCAN(clientes, mnomcli)
IF cl >= 0
     cl = ASCAN(clientes,  ;
          LEFT(mnomcli, 5))
ELSE
     cl = 50
ENDIF
IF cl > 100 .OR. cl <= 0
     cl = 50
ENDIF
IF SROWS() <= 25
     DO mensa WITH  ;
        '  Espere Un Momento...',  ;
        'SACA'
ELSE
     DO mensa2 WITH  ;
        '  Espere Un Momento...',  ;
        'SACA'
ENDIF
DEFINE WINDOW clientes FROM 06, 0  ;
       TO 19, 75 SHADOW TITLE  ;
       ' CLIENTES '
ACTIVATE WINDOW clientes
ON KEY LABEL Enter do Llama_Cli
@ 0, 0 SAY  ;
  '   C L I E N T E          N£mero    FECHA    S/Serv.    Serie       Modelo    '  ;
  COLOR N/W 
@ 1, 0 GET client DEFAULT  ;
  clientes(cl) SIZE 12, 74 FROM  ;
  clientes COLOR SCHEME 9
READ
ON KEY
DEACTIVATE WINDOW clientes
DEACTIVATE POPUP ayu1
RETURN
*
PROCEDURE salida
DEACTIVATE WINDOW clientes
DEACTIVATE POPUP ayu1
DO mensa2 WITH  ;
   '  Espere Un Momento...',  ;
   'SACA'
ON KEY
*
PROCEDURE llama_cli
IF LASTKEY() == 13
     wk_aux = ALLTRIM(SUBSTR(client,  ;
              25, 8))
     IF LEN(wk_aux) < 8
          wk_aux = wk_aux +  ;
                   CHR(13)
     ENDIF
     KEYBOARD CHR(27) + wk_aux
ENDIF
RETURN
*
PROCEDURE p_mensaje
PARAMETER mens
SET CURSOR OFF
l = LEN(mens)
DEFINE WINDOW mensj FROM 11, (80 -  ;
       l) / 2 - 4 TO 15, (80 + l) /  ;
       2 + 4
ACTIVATE WINDOW mensj
@ 1, (WCOLS('MENSJ') - l) / 2 SAY  ;
  mens COLOR GR+/N* 
= INKEY(1)
SET CURSOR ON
RETURN
*
*** 
*** ReFox - retrace your steps ... 
***
