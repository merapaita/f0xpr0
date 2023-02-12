USE IN 1 ArtMae ALIAS artmae  ;
    ORDER ArtMae1
USE IN 2 Parmae ALIAS parma ORDER  ;
    Parmae1
SET FILTER TO codart = 'B62'
vmens01 = 'Laboratorios : REVISION '
vmens02 = 'Registro de Laboratorios Especificos '
vmens04 = 'Dicho Laboratorio no fue encontrado'
vmens05 = 'No existe Laboratorio anterior'
vmens06 = 'No existe Laboratorio siguiente'
vmens07 = '¨ Desea ANULAR ‚ste Laboratorio ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Laboratorio ha sido anulado'
SELECT artmae
GOTO BOTTOM
SCATTER BLANK MEMVAR
HIDE POPUP ALL
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  aNula    Listar  Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW windo_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 14, 01  ;
       TO 16, 79 TITLE  ;
       ' Destino '
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
DEFINE PAD anula OF mmenu PROMPT  ;
       'a\<Nula  ' AT 24, 54
DEFINE PAD lista OF mmenu PROMPT  ;
       '\<Listar' AT 24, 63
DEFINE PAD termi OF mmenu PROMPT  ;
       '\<Termina' AT 24, 71
ON SELECTION PAD revis OF mmenu DO revis
ON SELECTION PAD busca OF mmenu DO busca
ON SELECTION PAD anter OF mmenu DO anter
ON SELECTION PAD proxi OF mmenu DO proxi
ON SELECTION PAD corri OF mmenu DO corri
ON SELECTION PAD ingre OF mmenu DO ingre
ON SELECTION PAD anula OF mmenu DO anula
ON SELECTION PAD lista OF mmenu DO lista
ON SELECTION PAD termi OF mmenu DO termi
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW windo_0
CLEAR
@ 1, 2 SAY '   Grupo Generico :'
@ 3, 2 SAY '      Laboratorio :'
@ 5, 2 SAY '      Desc. Corta :'
@ 7, 2 SAY '      Desc. Larga :'
@ 9, 2 SAY '            Fecha :'
RETURN
*
PROCEDURE vista
SELECT artmae
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW windo_0
SCATTER MEMVAR
vgrugen = SUBSTR(m.codart, 2, 2)
vgruesp = SUBSTR(m.codart, 5, 3)
@ 1, 22 SAY val_gg(vgrugen, ;
  'CODGEB',' ',22)
@ 3, 22 SAY vgruesp PICTURE '!!!'
@ 5, 22 SAY m.descri
@ 7, 22 SAY m.descri1
@ 9, 22 SAY m.fecreg
RETURN
*
PROCEDURE revis
SELECT artmae
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS codart :H =  ;
       '  Codigo ', descri :H =  ;
       '   Descripci¢n  ' : 40  ;
       NOMENU NOAPPEND NOEDIT  ;
       NODELETE WINDOW windo_0
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET RELATION TO
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
DEFINE POPUP plista FROM 20, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Codigo      '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Nombre      '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
cord1 = ORDER()
DO CASE
     CASE BAR() = 1
          vbusca = SPACE(7)
          vnombre = 'Codigo :'
          SET ORDER TO ArtMae1
     CASE BAR() = 2
          vbusca = SPACE(30)
          vnombre = 'Nombre : '
          SET ORDER TO ArtMae2
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO &cOrd1
ENDCASE
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 09,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            5
     ACTIVATE WINDOW lista
     @ 3, 2 SAY vnombre GET  ;
       vbusca
     READ VALID val_read()
     DEACTIVATE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
SET ORDER TO &cOrd1
RETURN
*
PROCEDURE anter
SELECT artmae
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. BOF()
     SKIP -1
ENDIF
IF BOF()
     GOTO TOP
     DO standby WITH vmens05
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE proxi
SELECT artmae
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
IF  .NOT. EOF()
     SKIP
ENDIF
IF EOF()
     DO standby WITH vmens06
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ACTIVATE WINDOW windo_0
DO pantalla
SELECT artmae
IF RLOCK() .OR. f_lock(1)
     SCATTER MEMVAR
     vtip = SUBSTR(m.codart, 1,  ;
            1)
     vgrugen = SUBSTR(m.codart, 2,  ;
               2)
     vgruesp = SUBSTR(m.codart, 5,  ;
               3)
     @ 1, 22 GET vgrugen PICTURE  ;
       '!!' VALID val_gg(vgrugen, ;
       'CODGEB',' ',22)
     @ 3, 22 GET vgruesp PICTURE  ;
       '!!!'
     @ 5, 22 GET m.descri
     @ 7, 22 GET m.descri1
     @ 9, 22 GET m.fecreg
     READ VALID val_read()
     m.codart = vtip + vgrugen +  ;
                '.' + vgruesp
     IF LASTKEY() = 27
          DO vista
          RETURN
     ELSE
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT artmae
     GOTO TOP
     GOTO vnum
ENDIF
UNLOCK ALL
FLUSH
SELECT artmae
DO vista
RETURN
*
PROCEDURE ingre
IF LASTKEY() = 27
     DO standby WITH  ;
        'Proceso cancelado'
     DO vista
     RETURN
ENDIF
DO pantalla
SELECT artmae
SCATTER BLANK MEMVAR
vnum = RECNO()
vtip = 'B'
vgrugen = '62'
vgruesp = SPACE(3)
@ 1, 22 GET vgrugen PICTURE '!!'  ;
  VALID val_gg(vgrugen,'CODGEB', ;
  ' ',22)
@ 1, 25 SAY val_gg(vgrugen, ;
  'CODGEB',' ',22)
@ 3, 22 GET vgruesp PICTURE '!!!'  ;
  WHEN val_ge() .AND. .F.
@ 5, 22 GET m.descri
@ 7, 22 GET m.descri1
@ 9, 22 GET m.fecreg
READ VALID val_read()
m.codart = vtip + vgrugen + '.' +  ;
           vgruesp
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
SELECT artmae
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado'
     SELECT artmae
     GOTO vnum
ENDIF
UNLOCK ALL
FLUSH
SELECT artmae
DO vista
RETURN
*
FUNCTION val_ge
PRIVATE nreg
SELECT ALIAS()
nreg = RECNO()
GOTO TOP
xgrue = 1
SCAN WHILE VAL(SUBSTR(codart, 5,  ;
     3)) = xgrue
     xgrue = xgrue + 1
ENDSCAN
vgruesp = PADL(ALLTRIM(STR(xgrue)),  ;
          3, '0')
SHOW GET vgruesp
RETURN .T.
*
PROCEDURE anula
DO standby WITH  ;
   'opcion todavia en proceso'
RETURN
SELECT artmae
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ANULAR ‚sta Orden de Detalle ?' ;
           )
IF velimina
     SELECT artmae
     IF RLOCK()
          REPLACE estado WITH  ;
                  '99', fecver  ;
                  WITH DATE()
     ENDIF
     DO vista
ENDIF
UNLOCK ALL
RETURN
*
PROCEDURE lista
DO standby WITH  ;
   'opcion todavia en proceso'
RETURN
SELECT artmae
SCATTER MEMVAR
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO replab
ENDIF
SELECT ordse
GOTO vtemp
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
CLOSE DATABASES
RELEASE WINDOW windo_0
RELEASE WINDOW wind_1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
FUNCTION val_gg
PARAMETER mvalor, filtro,  ;
          mvariable, mcol, mlong,  ;
          mdist
PRIVATE malias
DO CASE
     CASE PARAMETERS() = 2
          mcol = 0
          mvariable = ' '
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 3
          mcol = 0
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 4
          mlong = 40
          mdist = 6
     CASE PARAMETERS() = 5
          mdist = 6
ENDCASE
malias = ALIAS()
SELECT parma
SEEK filtro + 'B' + mvalor
IF  .NOT. FOUND() .OR.  ;
    EMPTY(mvalor)
     _oldwnd = WOUTPUT()
     ACTIVATE SCREEN
     SET FILTER TO tipo = filtro
     GOTO TOP
     IF EOF()
          DO standby WITH  ;
             'No existen Registros para Procesar'
          SET FILTER TO
          IF  .NOT. EMPTY(malias)
               SELECT (malias)
          ENDIF
          RETURN
     ENDIF
     DEFINE POPUP parametro FROM  ;
            03, 40 PROMPT FIELDS  ;
            SUBSTR(descri, 1,  ;
            40)
     ON SELECTION POPUP parametro DEACTIVATE;
POPUP
     ACTIVATE POPUP parametro
     IF  .NOT. EMPTY(_oldwnd)
          ACTIVATE WINDOW &_oldwnd
     ENDIF
     RELEASE POPUP parametro
     SET FILTER TO
ENDIF
mvalor = SUBSTR(ALLTRIM(parma.codigo),  ;
         2, 2)
mdescr = SUBSTR(parma.descri, 4,  ;
         mlong)
IF  .NOT. EMPTY(malias)
     SELECT (malias)
ENDIF
DO CASE
     CASE mvariable == ' '
          @ ROW(), mcol SAY  ;
            mvalor
          @ ROW(), mcol + mdist  ;
            SAY mdescr
          RETURN .T.
     OTHERWISE
          REPLACE &mvariable WITH mvalor
          RETURN .T.
ENDCASE
*
