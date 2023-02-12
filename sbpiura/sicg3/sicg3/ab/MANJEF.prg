PARAMETER mtipo, pantalla
SAVE SCREEN TO pantalla
USE IN 2 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 1 Cdrnec ALIAS cdr ORDER  ;
    cdrnec1
mperiodo = RIGHT(DTOC(DATE()), 2)
PUBLIC mcodigo, mdescri1, maux1,  ;
       maux2, mdescri2
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
vtitulo = 'Tabla de Jefaturas '
vtitul2 = 'Revisi¢n de Jefaturas     Presione F10 para concluir          '
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
@ 3, 20 SAY 'Codigo  : '
@ 5, 20 SAY 'Oficina : '
@ 7, 20 SAY 'Jefatura: '
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT cdr
SCATTER MEMVAR
@ 3, 32 SAY m.coddep
@ 5, 32 SAY val_para(m.coddep, ;
  'CODDEP','A',32)
@ 7, 32 SAY m.atte
RETURN
*
PROCEDURE revis
SELECT parma
SET FILTER TO tipo = 'CODDEP'
SELECT cdr
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
ACTIVATE SCREEN
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS cdr.coddep :H =  ;
       'C¢digo', cdr.atte :H =  ;
       'Jefe ' NOMENU NOAPPEND  ;
       NOEDIT NODELETE WINDOW  ;
       wind_1
ON KEY LABEL F10
DO vista
RETURN
*
PROCEDURE busca
SELECT cdr
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
vtemp = RECNO()
DEFINE WINDOW wbusca FROM 14, 12  ;
       TO 17, 68 DOUBLE COLOR  ;
       SCHEME 5
ACTIVATE WINDOW wbusca
STORE SPACE(6) TO vbusca
@ 1, 3 SAY  ;
  'Dependencia que desea    : '  ;
  COLOR SCHEME 5 GET vbusca  ;
  PICTURE '@!'
READ
RELEASE WINDOW wbusca
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
     GOTO vtemp
ELSE
     SEEK mperiodo + vbusca
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Dependencia '+ ;
             vbusca+ ;
             ' no Existe.'
          GOTO vtemp
     ELSE
          DO vista
     ENDIF
ENDIF
RETURN
*
PROCEDURE anter
SELECT cdr
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
SELECT cdr
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
        'Fin de archivo: no existe Dependencia siguiente.'
     GOTO BOTTOM
ELSE
     DO vista
ENDIF
RETURN
*
PROCEDURE corri
SELECT cdr
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
SCATTER MEMVAR
ACTIVATE WINDOW wind_0
@ 3, 32 SAY m.coddep
@ 5, 32 SAY val_para(m.coddep, ;
  'CODDEP','A',32)
@ 7, 32 GET m.atte
READ
IF LASTKEY() <> 27
     IF f_lock(1)
          GATHER MEMVAR
          UNLOCK
     ENDIF
ENDIF
DO vista
RETURN
*
PROCEDURE ingre
ACTIVATE WINDOW wind_0
SELECT cdr
SCATTER BLANK MEMVAR
m.periodo = RIGHT(PADL(YEAR(DATE()),  ;
            4), 2)
@ 3, 32 GET m.coddep PICTURE '@!'  ;
  VALID val_para(m.coddep, ;
  'CODDEP',' ',32,30) .AND.  ;
  valcod()
@ 5, 32 SAY val_para(m.coddep, ;
  'CODDEP','A',32)
@ 7, 32 GET m.atte
READ
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
          UNLOCK
     ENDIF
ENDIF
DO vista
RETURN
*
FUNCTION valcod
PRIVATE nreg
IF SEEK(m.periodo + m.coddep)
     DO standby WITH  ;
        'ElCodigo ya Existe.'
     RETURN .F.
ELSE
     RETURN .T.
ENDIF
*
PROCEDURE elimi
SELECT cdr
IF EOF()
     DO standby WITH  ;
        'Archivo Vac¡o. No hay registros para procesar.'
     RETURN
ENDIF
STORE yesno( ;
      '¨Est  seguro que desea ELIMINAR ? ' ;
      ) TO velimina
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
SELECT cdr
IF EOF()
     DO standby WITH  ;
        'Archivo: vac¡o. No hay registros para procesar.'
     RETURN
ELSE
     WAIT WINDOW  ;
          'Proceso en Implementaci¢n'
     RETURN
ENDIF
*
PROCEDURE termi
ven_accion = .F.
DEACTIVATE MENU
RETURN
*
PROCEDURE fin_opcion
SELECT cdr
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
PROCEDURE busca1
SELECT parma
SET FILTER TO tipo = 'CODDEP'
SET ORDER TO probar2
SEEK mcodigo
IF  .NOT. EOF()
     mdescri1 = descri
ELSE
     mdescri1 = 'Dependencia NO TIENE NOMBRE'
ENDIF
SELECT cdr
RETURN
*
