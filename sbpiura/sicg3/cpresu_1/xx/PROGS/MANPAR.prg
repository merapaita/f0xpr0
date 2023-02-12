PARAMETER mtipo, desc, descs,  ;
          tipman, desit1, desit2,  ;
          desit3
PRIVATE mtipo, ti_par, desc,  ;
        descs, tipman, desit1,  ;
        desit2, desit3, pantalla
SAVE SCREEN TO pantalla
tipman = IIF(PARAMETERS() < 4, 1,  ;
         tipman)
mtipo = IIF(PARAMETERS() = 0,  ;
        '????', mtipo)
desc = IIF(PARAMETERS() < 2,  ;
       mtipo, desc)
descs = IIF(PARAMETERS() < 3,  ;
        mtipo, descs)
desit1 = IIF(PARAMETERS() < 5,  ;
         'Tercera Variable',  ;
         desit1)
desit2 = IIF(PARAMETERS() < 6,  ;
         'Cuarta Variable',  ;
         desit2)
desit3 = IIF(PARAMETERS() < 7,  ;
         'Quinta Variable',  ;
         desit3)
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
IF tipman = 7
     USE IN 2 EXCLUSIVE Parmaux  ;
         ALIAS parmax
     SELECT parmax
     ZAP
ENDIF
SELECT parma
IF tipman = 8
     SET FILTER TO tipo + codigo = mtipo
ELSE
     SET FILTER TO tipo = mtipo
ENDIF
GOTO TOP
PUBLIC mcodigo, mdescri, maux1,  ;
       maux2
DO inicia
DO pantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE inicia
ACTIVATE SCREEN
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DO logos WITH rotulo1, vtempo
vtitulo = 'Tabla de ' + descs
vtitul2 = 'Revisi¢n de ' + descs +  ;
          '    Presione F10 para concluir          '
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vtitulo  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 24, 79 TITLE vtitul2  ;
       DOUBLE COLOR SCHEME 10
DEFINE MENU mmenu COLOR SCHEME 3
DEFINE PAD revis OF mmenu PROMPT  ;
       '\<Revisa' AT 24, 00
DEFINE PAD busca OF mmenu PROMPT  ;
       '\<Busca' AT 24, 08
DEFINE PAD anter OF mmenu PROMPT  ;
       '\<Anterior' AT 24, 15
DEFINE PAD proxi OF mmenu PROMPT  ;
       '\<Siguiente' AT 24, 25
DEFINE PAD corri OF mmenu PROMPT  ;
       '\<Corrige' AT 24, 36
DEFINE PAD ingre OF mmenu PROMPT  ;
       '\<Ingresa' AT 24, 45
DEFINE PAD elimi OF mmenu PROMPT  ;
       '\<Elimina' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Lista ' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD elimi OF mmenu DO elimi
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 3, 20 SAY 'Codigo:'
IF tipman = 7
     @ 4, 18 SAY 'Programa:'
ENDIF
@ 6, 7 SAY PADL(desc + ':', 20,  ;
  ' ')
DO CASE
     CASE tipman = 8
          @ 9, 7 SAY PADL(desit1 +  ;
            ':', 20, ' ')
     CASE tipman < 6 .AND. tipman >  ;
          1
          @ 9, 7 SAY PADL(desit1 +  ;
            ':', 20, ' ')
     CASE tipman = 6
          @ 9, 7 SAY PADL(desit1 +  ;
            ':', 20, ' ')
          @ 12, 7 SAY PADL(desit2 +  ;
            ':', 20, ' ')
ENDCASE
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT parma
mcodigo = codigo
mdescri = descri
mdescri1 = SUBSTR(descri, 01, 50)
mdescri2 = SUBSTR(descri, 51, 50)
maux1 = asg_val(tipman,1)
maux2 = asg_val(tipman,2)
@ 3, 28 SAY mcodigo PICTURE  ;
  '@S30'
@ 6, 28 SAY mdescri1
@ 7, 28 SAY mdescri2
DO CASE
     CASE tipman = 2
          @ 9, 28 SAY maux1
     CASE tipman = 3
          @ 9, 28 SAY maux1
     CASE tipman = 4 .OR. tipman =  ;
          8
          @ 9, 28 SAY maux1  ;
            PICTURE  ;
            '9,999,999,999'
     CASE tipman = 5
          @ 9, 28 SAY maux1  ;
            PICTURE  ;
            '999,999,999.99'
     CASE tipman = 6
          @ 9, 28 SAY maux1  ;
            PICTURE  ;
            '9,999,999,999'
          @ 12, 28 SAY maux2
     CASE tipman = 7
          @ 4, 28 SAY maux1  ;
            PICTURE '!!!!!!'
ENDCASE
RETURN
*
PROCEDURE revis
IF tipman = 8
     DO standby WITH  ;
        'Opci¢n no disponible'
     RETURN
ENDIF
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS codigo :H =  ;
       'C¢digo', codigoaux :H =  ;
       'Subcdgo', descri :H =  ;
       'Descripci¢n' : 50, nument  ;
       :H = 'Correl' :P =  ;
       '99,999' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_1 KEY mtipo
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
SELECT parma
IF tipman = 8
     DO standby WITH  ;
        'Opci¢n no disponible'
     RETURN
ENDIF
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW wbusca FROM 10, 12  ;
       TO 14, 68 DOUBLE COLOR  ;
       SCHEME 6
ACTIVATE WINDOW wbusca
STORE SPACE(6) TO vbusca
@ 1, 3 SAY 'Digite c¢digo de ' +  ;
  desc + ' que desea :' COLOR  ;
  SCHEME 7 GET vbusca PICTURE  ;
  '@!'
READ
RELEASE WINDOW wbusca
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK mtipo + vbusca
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicho '+desc+ ;
             ' no fue encontrado.'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
IF  .NOT. BOF()
     SKIP -1
ENDIF
IF BOF()
     GOTO TOP
     DO standby WITH "Inicio de archivo: no existe &mTipo anterior."
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE proxi
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
IF  .NOT. EOF()
     SKIP
ENDIF
IF EOF()
     DO standby WITH  ;
        'Fin de archivo: no existe '+ ;
        desc+' siguiente.'
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
@ 3, 28 SAY mcodigo
IF tipman = 7
     @ 4, 28 GET maux1 PICTURE  ;
       '!!!!!!'
ENDIF
IF tipman <> 8
     @ 6, 28 GET mdescri PICTURE  ;
       '@S50'
ENDIF
DO CASE
     CASE tipman = 2
          @ 9, 28 GET maux1
     CASE tipman = 3
          @ 9, 28 GET maux1
     CASE tipman = 4 .OR. tipman =  ;
          8
          @ 9, 28 GET maux1  ;
            PICTURE  ;
            '9,999,999,999'
     CASE tipman = 5
          @ 9, 28 GET maux1  ;
            PICTURE  ;
            '99,999,999.99'
     CASE tipman = 6
          @ 9, 28 GET maux1  ;
            PICTURE  ;
            '9,999,999,999'
          @ 12, 28 GET maux2
ENDCASE
READ
IF LASTKEY() <> 27
     IF f_lock(1)
          REPLACE descri WITH  ;
                  mdescri
          DO CASE
               CASE tipman = 2
                    REPLACE descriaux  ;
                            WITH  ;
                            maux1
               CASE tipman = 3
                    REPLACE fecha  ;
                            WITH  ;
                            maux1
               CASE tipman = 4  ;
                    .OR. tipman =  ;
                    8
                    REPLACE nument  ;
                            WITH  ;
                            maux1
               CASE tipman = 5
                    REPLACE numdec  ;
                            WITH  ;
                            maux1
               CASE tipman = 6
                    REPLACE nument  ;
                            WITH  ;
                            maux1,  ;
                            fecha  ;
                            WITH  ;
                            maux2
               CASE tipman = 7
                    REPLACE codigoaux  ;
                            WITH  ;
                            maux1
          ENDCASE
          UNLOCK
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE ingre
IF tipman = 8
     DO standby WITH  ;
        'Opci¢n no disponible'
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
STORE SPACE(6) TO mcodigo
STORE SPACE(60) TO mdescri
SELECT parma
@ 3, 28 GET mcodigo PICTURE '@!'
IF tipman = 7
     @ 4, 28 GET maux1 PICTURE  ;
       '!!!!!!'
ENDIF
READ
IF LASTKEY() = 27 .OR.  ;
   EMPTY(mcodigo)
     DO standby WITH  ;
        'Proceso cancelado. No se graba ning£n cambio.'
     GOTO BOTTOM
ELSE
     IF tipman = 7
          SEEK mtipo + mcodigo +  ;
               maux1
     ELSE
          SEEK mtipo + mcodigo
     ENDIF
     IF FOUND()
          DO standby WITH desc+ ;
             ' ya est  registrado. Proceda a corregir datos.'
          DO vista
          DO corri
     ELSE
          @ 6, 28 GET mdescri  ;
            PICTURE '@S50'
          DO CASE
               CASE tipman = 2
                    @ 9, 28 GET  ;
                      maux1
               CASE tipman = 3
                    @ 9, 28 GET  ;
                      maux1
               CASE tipman = 4  ;
                    .OR. tipman =  ;
                    8
                    @ 9, 28 GET  ;
                      maux1  ;
                      PICTURE  ;
                      '9,999,999,999'
               CASE tipman = 5
                    @ 9, 28 GET  ;
                      maux1  ;
                      PICTURE  ;
                      '999,999,999.99'
               CASE tipman = 6
                    @ 9, 28 GET  ;
                      maux1  ;
                      PICTURE  ;
                      '9,999,999,999'
                    @ 12, 28 GET  ;
                      maux2
          ENDCASE
          READ
          IF LASTKEY() <> 27
               IF f_appd()
                    REPLACE tipo  ;
                            WITH  ;
                            mtipo,  ;
                            codigo  ;
                            WITH  ;
                            mcodigo,  ;
                            descri  ;
                            WITH  ;
                            mdescri
                    DO CASE
                         CASE tipman =  ;
                              2
                              REPLACE  ;
                               descriaux  ;
                               WITH  ;
                               maux1
                         CASE tipman =  ;
                              3
                              REPLACE  ;
                               fecha  ;
                               WITH  ;
                               maux1
                         CASE tipman =  ;
                              4  ;
                              .OR.  ;
                              tipman =  ;
                              8
                              REPLACE  ;
                               nument  ;
                               WITH  ;
                               maux1
                         CASE tipman =  ;
                              5
                              REPLACE  ;
                               numdec  ;
                               WITH  ;
                               maux1
                         CASE tipman =  ;
                              6
                              REPLACE  ;
                               nument  ;
                               WITH  ;
                               maux1,  ;
                               fecha  ;
                               WITH  ;
                               maux2
                         CASE tipman =  ;
                              7
                              REPLACE  ;
                               codigoaux  ;
                               WITH  ;
                               maux1
                    ENDCASE
                    UNLOCK
               ENDIF
          ELSE
               GOTO BOTTOM
          ENDIF
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE elimi
IF tipman = 8
     DO standby WITH  ;
        'Opci¢n no disponible'
     RETURN
ENDIF
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
STORE yesno( ;
      '¨Est  seguro que desea ELIMINAR este ' +  ;
      desc + ' ?') TO velimina
IF velimina
     IF f_lock(1)
          DELETE NEXT 1
          IF  .NOT. BOF()
               SKIP -1
          ENDIF
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE lista
IF tipman = 8
     DO standby WITH  ;
        'Opci¢n no disponible'
     RETURN
ENDIF
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
vtemp = RECNO()
DO CASE
     CASE tipman = 2
          campo3 = 'Descriaux'
          DO reporte WITH 2,  ;
             'ManPar2',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
     CASE tipman = 3
          campo3 = 'Fecha'
          DO reporte WITH 2,  ;
             'ManPar3',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
     CASE tipman = 4 .OR. tipman =  ;
          8
          campo3 = 'NumEnt'
          DO reporte WITH 2,  ;
             'ManPar4',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
     CASE tipman = 5
          campo3 = 'NumDec'
          DO reporte WITH 2,  ;
             'ManPar5',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
     CASE tipman = 6
          campo3 = 'NumEnt'
          campo4 = 'Fecha'
          DO reporte WITH 2,  ;
             'ManPar6',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
     CASE tipman = 7
          SET FILTER TO tipo = 'CODSUB'
          GOTO TOP
          SCAN
               SCATTER MEMVAR
               SELECT parmax
               APPEND BLANK
               GATHER MEMVAR
               SELECT parma
          ENDSCAN
          SET FILTER TO
          SELECT parmax
          GOTO TOP
          DO reporte WITH 2,  ;
             'ManPar7',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
          SELECT parma
     OTHERWISE
          DO reporte WITH 2,  ;
             'ManPar',  ;
             'CATALOGO DE '+ ;
             UPPER(descs)
ENDCASE
GOTO vtemp
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
SELECT parma
SET FILTER TO
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE MENU mmenu
RESTORE SCREEN FROM pantalla
CLOSE DATABASES
RETURN
*
FUNCTION asg_val
PARAMETER tipman, otro
PRIVATE v_fun
DO CASE
     CASE tipman = 1
          v_fun = descri
     CASE tipman = 2
          v_fun = descriaux
     CASE tipman = 3
          v_fun = fecha
     CASE tipman = 4 .OR. tipman =  ;
          8
          v_fun = nument
     CASE tipman = 5
          v_fun = numdec
     CASE tipman = 6 .AND. otro =  ;
          1
          v_fun = nument
     CASE tipman = 6 .AND. otro =  ;
          2
          v_fun = fecha
     CASE tipman = 7
          v_fun = codigoaux
ENDCASE
RETURN v_fun
*
FUNCTION buscprg
PARAMETER vcodigo
cv = ALLTRIM(vcodigo)
AX=PRG_&CV
RETURN ax
*
