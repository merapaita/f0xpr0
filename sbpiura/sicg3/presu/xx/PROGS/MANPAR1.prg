PARAMETER ctitulo, ctipo, ccodigo,  ;
          ccodigoaux, cdescri,  ;
          cdescriaux, cdescriau2,  ;
          cnumdec, cnument,  ;
          cfecha
SAVE SCREEN TO pantalla
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
SET FILTER TO tipo = ctipo
GOTO TOP
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
vtitulo = 'Tabla de ' + ctitulo
vtitul2 = 'Revisi¢n de ' +  ;
          ctitulo +  ;
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
IF  .NOT. EMPTY(ccodigo)
     @ 3, 7 SAY PADL(ccodigo +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(ccodigoaux)
     @ 5, 7 SAY PADL(ccodigoaux +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(cdescri)
     @ 7, 7 SAY PADL(cdescri +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(cdescriaux)
     @ 9, 7 SAY PADL(cdescriaux +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(cdescriau2)
     @ 11, 7 SAY PADL(cdescriau2 +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(cnumdec)
     @ 13, 7 SAY PADL(cnumdec +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(cnument)
     @ 15, 7 SAY PADL(cnument +  ;
       ':', 15, ' ')
ENDIF
IF  .NOT. EMPTY(cfecha)
     @ 17, 7 SAY PADL(cfecha +  ;
       ':', 15, ' ')
ENDIF
RETURN
*
PROCEDURE vista
DO pantalla
SELECT parma
SCATTER MEMVAR
m.codigo = codigo
m.codigoaux = codigoaux
m.descri = descri
m.descriaux = descriaux
m.descriau2 = descriau2
m.numdec = numdec
m.nument = nument
m.fecha = fecha
IF  .NOT. EMPTY(ccodigo)
     @ 3, 28 SAY m.codigo PICTURE  ;
       '@S30'
ENDIF
IF  .NOT. EMPTY(ccodigoaux)
     @ 5, 28 SAY m.codigoaux
ENDIF
IF  .NOT. EMPTY(cdescri)
     @ 7, 28 SAY m.descri
ENDIF
IF  .NOT. EMPTY(cdescriaux)
     @ 9, 28 SAY m.descriaux
ENDIF
IF  .NOT. EMPTY(cdescriau2)
     @ 11, 28 SAY m.descriau2
ENDIF
IF  .NOT. EMPTY(cnumdec)
     @ 13, 28 SAY m.numdec
ENDIF
IF  .NOT. EMPTY(cnument)
     @ 15, 28 SAY m.nument
ENDIF
IF  .NOT. EMPTY(cfecha)
     @ 17, 28 SAY m.fecha
ENDIF
RETURN
*
PROCEDURE revis
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS codigo :H = IIF(  ;
       .NOT. EMPTY(ccodigo),  ;
       ccodigo, 'C¢digo'),  ;
       codigoaux :H = IIF( .NOT.  ;
       EMPTY(ccodigoaux),  ;
       ccodigoaux, 'CodigoAux'),  ;
       descri :H = IIF( .NOT.  ;
       EMPTY(cdescri), cdescri,  ;
       'Descri') : 50, nument :H =  ;
       IIF( .NOT. EMPTY(cnument),  ;
       cnument, 'NumEnt') :P =  ;
       '99,999' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_1 KEY ctipo
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
SELECT parma
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
  cdescri + ' que desea :' COLOR  ;
  SCHEME 7 GET vbusca PICTURE  ;
  '@!'
READ
RELEASE WINDOW wbusca
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK ctipo + vbusca
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicho '+cdescri+ ;
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
     DO standby WITH "Inicio de archivo: no existe &cdescri anterior."
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
     DO standby WITH "Fin de archivo: no existe &cDescri siguiente."
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
SCATTER MEMVAR
IF  .NOT. EMPTY(ccodigo)
     @ 3, 28 GET m.codigo DISABLE
ENDIF
IF  .NOT. EMPTY(ccodigoaux)
     @ 5, 28 GET m.codigoaux  ;
       DISABLE
ENDIF
IF  .NOT. EMPTY(cdescri)
     @ 7, 28 GET m.descri
ENDIF
IF  .NOT. EMPTY(cdescriaux)
     @ 9, 28 GET m.descriaux
ENDIF
IF  .NOT. EMPTY(cdescriau2)
     @ 11, 28 GET m.descriau2
ENDIF
IF  .NOT. EMPTY(cnumdec)
     @ 13, 28 GET m.numdec
ENDIF
IF  .NOT. EMPTY(cnument)
     @ 15, 28 GET m.nument
ENDIF
IF  .NOT. EMPTY(cfecha)
     @ 17, 28 GET m.fecha
ENDIF
READ
IF LASTKEY() <> 27
     IF f_lock(1)
          IF  .NOT.  ;
              EMPTY(cdescri)
               REPLACE descri  ;
                       WITH  ;
                       m.descri
          ENDIF
          IF  .NOT.  ;
              EMPTY(cdescriaux)
               REPLACE descriaux  ;
                       WITH  ;
                       m.descriaux
          ENDIF
          IF  .NOT.  ;
              EMPTY(cdescriau2)
               REPLACE descriau2  ;
                       WITH  ;
                       m.descriau2
          ENDIF
          IF  .NOT.  ;
              EMPTY(cnumdec)
               REPLACE numdec  ;
                       WITH  ;
                       m.numdec
          ENDIF
          IF  .NOT.  ;
              EMPTY(cnument)
               REPLACE nument  ;
                       WITH  ;
                       m.nument
          ENDIF
          IF  .NOT. EMPTY(cfecha)
               REPLACE fecha WITH  ;
                       m.fecha
          ENDIF
          UNLOCK
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE ingre
ACTIVATE WINDOW wind_0
STORE SPACE(6) TO mcodigo
STORE SPACE(60) TO mdescri
SELECT parma
SCATTER BLANK MEMVAR
nreg = RECNO()
IF  .NOT. EMPTY(ccodigo)
     @ 3, 28 GET m.codigo PICTURE  ;
       '@!' VALID valcod()
ENDIF
IF  .NOT. EMPTY(ccodigoaux)
     @ 5, 28 GET m.codigoaux  ;
       PICTURE '@!' VALID  ;
       valcodaux()
ENDIF
IF  .NOT. EMPTY(cdescri)
     @ 7, 28 GET m.descri
ENDIF
IF  .NOT. EMPTY(cdescriaux)
     @ 9, 28 GET m.descriaux
ENDIF
IF  .NOT. EMPTY(cdescriau2)
     @ 11, 28 GET m.descriau2
ENDIF
IF  .NOT. EMPTY(cnumdec)
     @ 13, 28 GET m.numdec
ENDIF
IF  .NOT. EMPTY(cnument)
     @ 15, 28 GET m.nument
ENDIF
IF  .NOT. EMPTY(cfecha)
     @ 17, 28 GET m.fecha
ENDIF
READ
IF LASTKEY() <> 27
     IF f_appd()
          REPLACE tipo WITH ctipo
          IF  .NOT.  ;
              EMPTY(ccodigo)
               REPLACE codigo  ;
                       WITH  ;
                       m.codigo
          ENDIF
          IF  .NOT.  ;
              EMPTY(ccodigoaux)
               REPLACE codigoaux  ;
                       WITH  ;
                       m.codigoaux
          ENDIF
          IF  .NOT.  ;
              EMPTY(cdescri)
               REPLACE descri  ;
                       WITH  ;
                       m.descri
          ENDIF
          IF  .NOT.  ;
              EMPTY(cdescriaux)
               REPLACE descriaux  ;
                       WITH  ;
                       m.descriaux
          ENDIF
          IF  .NOT.  ;
              EMPTY(cdescriau2)
               REPLACE descriau2  ;
                       WITH  ;
                       m.descriau2
          ENDIF
          IF  .NOT.  ;
              EMPTY(cnumdec)
               REPLACE numdec  ;
                       WITH  ;
                       m.numdec
          ENDIF
          IF  .NOT.  ;
              EMPTY(cnument)
               REPLACE nument  ;
                       WITH  ;
                       nument
          ENDIF
          IF  .NOT. EMPTY(cfecha)
               REPLACE fecha WITH  ;
                       fecha
          ENDIF
          UNLOCK
     ENDIF
ELSE
     GOTO nreg
ENDIF
DO vista
RETURN
*
FUNCTION valcod
SELECT parma
IF  .NOT. EMPTY(ccodigoaux)
ELSE
     IF SEEK(ctipo +  ;
        ALLTRIM(m.codigo))
          DO standby WITH  ;
             'El Codigo ya Existe Intente de nuevo'
          RETURN .F.
     ELSE
          RETURN .T.
     ENDIF
ENDIF
*
FUNCTION valcodaux
SELECT parma
IF  .NOT. EMPTY(ccodigoaux)
     IF SEEK(ctipo +  ;
        ALLTRIM(m.codigo) +  ;
        m.codigoaux)
          DO standby WITH  ;
             'El Codigo ya Existe Intente de nuevo'
          RETURN .F.
     ELSE
          RETURN .T.
     ENDIF
ENDIF
*
PROCEDURE elimi
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
STORE yesno( ;
      '¨Est  seguro que desea ELIMINAR este ' +  ;
      ccodigo + ' ?') TO  ;
      velimina
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
SELECT parma
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
vtemp = RECNO()
IF EMPTY(ccodigoaux)
     DO reporte WITH 2, 'ManPar',  ;
        'CATALOGO DE '+ ;
        UPPER(ctitulo)
ELSE
     DO reporte WITH 2, 'ManPar1',  ;
        'CATALOGO DE '+ ;
        UPPER(ctitulo)
ENDIF
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
*
FUNCTION val_cod
PRIVATE mret, mcodigo, mfiltro
nreg = RECNO()
mfiltro = FILTER()
mcodigo = ALLTRIM(parma.codigo)
SET FILTER TO
DO CASE
     CASE ctipo = 'UNIEJE'
          SEEK tipo + codigo =  ;
               'UNIGES' +  ;
               mcodigo
          mret = val_para(mcodigo, ;
                 'UNIGES','Z')
     CASE ctipo = 'CODPRG'
          SEEK 'CODFUN' + mcodigo
          mret = val_para(ALLTRIM(codigo), ;
                 'CODFUN','Z')
     CASE ctipo = 'CODSPR'
          SEEK 'CODPRG' + mcodigo
          mret = val_para(ALLTRIM(mcodigo), ;
                 'CODPRG05','Z')
ENDCASE
SET FILTER TO &mFiltro
GOTO nreg
RETURN mret
*
