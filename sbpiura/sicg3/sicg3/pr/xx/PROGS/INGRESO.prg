vmens01 = ' Ingresos : REGISTRO '
vmens02 = 'Revisi¢n de Ingresos'
vmens04 = 'Dicha Ingresos no fue encontrada'
vmens05 = 'No existe Ingresos anterior'
vmens06 = 'No existe Ingresos siguiente'
vmens07 = '¨ Desea ELIMINAR ‚sta Ingresos ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Este Ingreso ha sido anulado'
vmens10 = 'El Ingreso ya fue atendido'
SELECT 1
USE IN 1 ingreso ALIAS ingre  ;
    ORDER ingreso1
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
vtempo = ' Revisa  Busca  Anterior  Siguiente  Corrige  Ingresa  Elimina  Listar  Termina '
DEFINE WINDOW wind_0 FROM 00, 00  ;
       TO 23, 79 TITLE vmens01  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_1 FROM 00, 00  ;
       TO 23, 79 TITLE vmens02  ;
       COLOR SCHEME 10
DEFINE WINDOW wind_c1 FROM 00, 00  ;
       TO 23, 79 COLOR SCHEME 10
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
       '\<Listar' AT 24, 63
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
@ 2, 2 SAY '          C¢digo :'
@ 4, 2 SAY '     Descripci¢n :'
@ 6, 2 SAY '         Detalle :'
RETURN
*
PROCEDURE vista
SELECT ingre
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 2, 22 SAY m.coding PICTURE '@!'
@ 4, 22 SAY m.desing
@ 6, 22 SAY m.subing
RETURN
*
PROCEDURE revis
SELECT ingre
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
BROWSE FIELDS coding :H =  ;
       'Codigo', subing :H =  ;
       'Detalle', desing :H =  ;
       'Descripci¢n' NOMENU  ;
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
SELECT ingre
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
vtemp = RECNO()
STORE SPACE(9) TO vcoding
STORE SPACE(2) TO vsubing
ACTIVATE WINDOW standby
@ 1, 03 SAY '       Codigo :' GET  ;
  vcoding PICTURE '@!'
READ
DEACTIVATE WINDOW standby
IF EMPTY(vcoding) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK vcoding + vsubing
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dicho codigo no est  registrado'
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
SELECT ingre
SCATTER MEMVAR
IF RLOCK() .OR. f_lock(1)
     @ 2, 22 GET m.coding PICTURE  ;
       '@!'
     @ 4, 22 GET m.desing
     @ 6, 22 GET m.subing
     READ VALID val_read()
     IF LASTKEY() <> 27
          SELECT ingre
          GATHER MEMVAR
     ENDIF
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE ingre
SELECT ingre
DO pantalla
SCATTER BLANK MEMVAR
@ 2, 22 GET m.coding PICTURE '@!'
@ 4, 22 GET m.desing
@ 6, 22 GET m.subing
READ VALID val_read()
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
     ELSE
          GOTO BOTTOM
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado, no se graba nada'
     GOTO BOTTOM
ENDIF
UNLOCK ALL
SELECT ingre
DO vista
RETURN
*
PROCEDURE elimi
SELECT ingre
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
velimina = yesno( ;
           '¨ Desea ELIMINAR FISICAMENTE ‚ste ingreso ?' ;
           )
IF velimina .AND. (RLOCK() .OR.  ;
   f_lock(1))
     DELETE NEXT 1
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE lista
SELECT ingre
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'ingres',  ;
        ' Codigos de Ingresos '
ENDIF
SELECT ingre
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
RELEASE WINDOW wind_0
RELEASE WINDOW wind_1
RELEASE WINDOW wind_c1
RELEASE MENU mmenu
RESTORE SCREEN FROM principal
RETURN
*
