PRIVATE vmens01, vmens02, vmens03,  ;
        vmens04, vmens05, vmens06,  ;
        vmens07, vmens08, vmens09,  ;
        vmens10, vmens11
vmens01 = ' Tipos de Documentos '
vmens02 = 'Revisi¢n de Tipos de Documentos'
vmens03 = 'Digite Tipo de Documento que desea :'
vmens04 = 'Dicho Tipo de Documento no fue encontrado.'
vmens05 = 'No existe Tipo de Documento anterior.'
vmens06 = 'No existe Tipo de Documento siguiente.'
vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Tipo de Documento?'
vmens08 = 'No hay registros para procesar'
USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
SELECT parma
mtipo = 'TIPDOC'
SET FILTER TO tipo = mtipo
GOTO BOTTOM
SCATTER BLANK MEMVAR
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Lista   Termina '
DO logos WITH rotulo1, vtempo
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
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
ACTIVATE SCREEN
RETURN
*
PROCEDURE pantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 3, 2 SAY '            C¢digo:'
@ 5, 2 SAY ' Tipo de Documento:'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT parma
SCATTER MEMVAR
@ 3, 22 SAY m.codigo PICTURE  ;
  '!!!'
@ 5, 22 SAY m.descri
RETURN
*
PROCEDURE revis
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
BROWSE FIELDS codigo :H =  ;
       'C¢digo', descri :H =  ;
       'Tipo de Documento' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_1
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
ACTIVATE WINDOW standby
STORE SPACE(2) TO vbusca
@ 1, 3 SAY vmens03 COLOR SCHEME 1  ;
  GET vbusca PICTURE '!!'
READ
DEACTIVATE WINDOW standby
IF vbusca = SPACE(2) .OR.  ;
   LASTKEY() = 27
     GOTO vtemp
ELSE
     SEEK vbusca
     IF  .NOT. FOUND()
          DO standby WITH vmens04
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
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
ACTIVATE WINDOW wind_0
bloquea = .T.
DO fox_lock WITH 1, bloquea
IF bloquea
     @ 3, 22 SAY m.codigo PICTURE  ;
       '!!!'
     @ 5, 22 GET m.descri
     READ
     IF LASTKEY() <> 27
          GATHER MEMVAR
     ENDIF
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE ingre
ACTIVATE WINDOW wind_0
SCATTER BLANK MEMVAR
@ 3, 22 GET m.codigo PICTURE  ;
  '!!!'
READ
SELECT parma
IF LASTKEY() = 27 .OR. m.codigo =  ;
   SPACE(2)
     DO standby WITH  ;
        'Proceso cancelado. No se graba ning£n cambio.'
     GOTO BOTTOM
ELSE
     SEEK m.codigo
     IF FOUND()
          DO standby WITH  ;
             'Tipo de Documento ya est  registrado. Proceda a corregir datos.'
          DO vista
          DO corri
     ELSE
          @ 5, 22 CLEAR TO 23, 78
          @ 5, 22 GET m.descri
          READ
          IF LASTKEY() <> 27
               agrega = .T.
               DO fox_appd WITH  ;
                  agrega
               IF agrega
                    m.tipo = mtipo
                    GATHER MEMVAR
               ENDIF
               UNLOCK
          ELSE
               GOTO BOTTOM
          ENDIF
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE elimi
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
STORE yesno(vmens07) TO velimina
IF velimina
     bloquea = .T.
     DO fox_lock WITH 1, bloquea
     IF bloquea
          DELETE NEXT 1
          IF  .NOT. BOF()
               SKIP -1
          ENDIF
     ENDIF
     UNLOCK
ENDIF
DO vista
RETURN
*
PROCEDURE lista
SELECT parma
DO reporte WITH 2, 'TipDoc',  ;
   ' Tipos de Documentos '
SELECT parma
GOTO BOTTOM
DO vista
RETURN
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE MENU mmenu
CLOSE DATABASES
DO logos WITH rotulo1, rotulo2
RETURN
*
