vmens01 = ' Catalogo de Asignaciones Presupuestales: REGISTRO '
vmens02 = 'Revisi¢n de Asignaciones Presupuestales'
vmens04 = 'Dicha Asignacion no fue encontrada'
vmens05 = 'No existe Asignacion anterior'
vmens06 = 'No existe Asignaci¢n siguiente'
vmens07 = '¨ Desea ELIMINAR ‚sta Asignacion ?'
vmens08 = 'No hay registros para procesar'
vmens09 = 'Esta Asignaci¢n ha sido anulado'
vmens10 = 'La Asignaci¢n ya fue atendido'
CLOSE DATABASES
USE IN 1 CatAsi ALIAS catasi  ;
    ORDER CatAsi5
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
@ 1, 2 SAY 'Tipo Presupuesto :'
@ 2, 2 SAY '        Generico :'
@ 3, 2 SAY 'Sub Gen. Nivel 1 :'
@ 4, 2 SAY 'Sub Gen. Nivel 2 :'
@ 5, 2 SAY 'Especif. Nivel 1 :'
@ 6, 2 SAY 'Especif. Nivel 2 :'
@ 8, 2 SAY 'Especif. Nivel 3 :'
@ 9, 2 SAY 'Especif. Nivel 4 :'
@ 11, 2 SAY '         Detalle :'
@ 13, 2 SAY ' C¢d. Asignaci¢n :'
@ 15, 2 SAY '      Descrpci¢n :'
RETURN
*
PROCEDURE vista
SELECT catasi
IF EOF()
     DO pantalla
     RETURN
ENDIF
ACTIVATE WINDOW wind_0
SCATTER MEMVAR
@ 1, 22 SAY m.tippre
@ 2, 22 SAY m.generic
@ 3, 22 SAY m.sgn1
@ 4, 22 SAY m.sgn2
@ 5, 22 SAY m.espn1
@ 6, 22 SAY m.espn2
@ 8, 22 SAY m.espn3
@ 9, 22 SAY m.espn4
@ 11, 22 SAY m.detalle
@ 13, 22 SAY m.codpart
@ 15, 22 SAY LEFT(m.descri, 50)
@ 16, 22 SAY SUBSTR(m.descri, 51,  ;
  50)
RETURN
*
PROCEDURE revis
SELECT catasi
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
BROWSE FIELDS tippre :H = 'T',  ;
       generic :H = 'G', sgn1 :H =  ;
       'SG1', sgn2 :H = 'SG2',  ;
       espn1 :H = 'E1', espn2 :H =  ;
       'E2', espn3 :H = 'E3',  ;
       espn4 :H = 'E4', detalle  ;
       :H = 'D', codpart :H =  ;
       'Asignacion', descri :H =  ;
       'Descripci¢n' : 35 NOMENU  ;
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
SELECT catasi
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
SELECT catasi
SCATTER MEMVAR
IF RLOCK() .OR. f_lock(1)
     @ 1, 22 GET m.tippre DISABLE
     @ 2, 22 GET m.generic  ;
       DISABLE
     @ 3, 22 GET m.sgn1 DISABLE
     @ 4, 22 GET m.sgn2 DISABLE
     @ 5, 22 GET m.espn1 DISABLE
     @ 6, 22 GET m.espn2 DISABLE
     @ 8, 22 GET m.espn3 DISABLE
     @ 9, 22 GET m.espn4 DISABLE
     @ 11, 22 GET m.detalle  ;
       FUNCTION 'M S,N' DISABLE
     @ 13, 22 GET m.codpart  ;
       DISABLE
     @ 15, 22 GET m.descri  ;
       FUNCTION 'S50'
     READ VALID val_read()
     IF LASTKEY() <> 27
          SELECT catasi
          GATHER MEMVAR
     ENDIF
     DO vista
ENDIF
UNLOCK
RETURN
*
PROCEDURE ingre
SELECT catasi
DO pantalla
SCATTER BLANK MEMVAR
@ 1, 22 GET m.tippre PICTURE '9'
@ 2, 22 GET m.generic PICTURE '9'
@ 3, 22 GET m.sgn1 PICTURE '99'  ;
  VALID completa(m.sgn1)
@ 4, 22 GET m.sgn2 PICTURE '99'  ;
  VALID completa(m.sgn2)
@ 5, 22 GET m.espn1 PICTURE '99'  ;
  VALID completa(m.espn1)
@ 6, 22 GET m.espn2 PICTURE '99'  ;
  VALID completa(m.espn2)
@ 8, 22 GET m.espn3 PICTURE '99'  ;
  VALID completa(m.espn3)
@ 9, 22 GET m.espn4 PICTURE '99'  ;
  VALID completa(m.espn4)
@ 11, 22 GET m.detalle FUNCTION  ;
  'M S,N' VALID valasip()
@ 13, 22 GET m.codpart DISABLE
@ 15, 22 GET m.descri FUNCTION  ;
  'S50'
READ VALID val_read() .AND.  ;
     val_nvo()
IF LASTKEY() <> 27
     IF f_appd()
          GATHER MEMVAR
          = valnivant()
     ELSE
          GOTO BOTTOM
     ENDIF
ELSE
     DO standby WITH  ;
        'Proceso cancelado, no se graba nada'
     GOTO BOTTOM
ENDIF
UNLOCK ALL
SELECT catasi
DO vista
RETURN
*
PROCEDURE valnivant
PRIVATE mreg
mreg = RECNO()
IF m.detalle = 'S'
     DO CASE
          CASE  .NOT.  ;
                EMPTY(espn4)
               xcad = m.tippre +  ;
                      m.generic +  ;
                      m.sgn1 +  ;
                      m.sgn2 +  ;
                      m.espn1 +  ;
                      m.espn2 +  ;
                      m.espn3
          CASE  .NOT.  ;
                EMPTY(espn3)
               xcad = m.tippre +  ;
                      m.generic +  ;
                      m.sgn1 +  ;
                      m.sgn2 +  ;
                      m.espn1 +  ;
                      m.espn2
          CASE  .NOT.  ;
                EMPTY(espn2)
               xcad = m.tippre +  ;
                      m.generic +  ;
                      m.sgn1 +  ;
                      m.sgn2 +  ;
                      m.espn1
          CASE  .NOT.  ;
                EMPTY(espn1)
               xcad = m.tippre +  ;
                      m.generic +  ;
                      m.sgn1 +  ;
                      m.sgn2
          CASE  .NOT. EMPTY(sgn2)
               xcad = m.tippre +  ;
                      m.generic +  ;
                      m.sgn1
          CASE  .NOT. EMPTY(sgn1)
               xcad = m.tippre +  ;
                      m.generic
          OTHERWISE
               DO standby WITH  ;
                  'Error en Codificaci¢n'
     ENDCASE
     IF SEEK(xcad) .AND. detalle =  ;
        'S'
          REPLACE detalle WITH  ;
                  'N'
     ENDIF
ENDIF
GOTO mreg
RETURN
*
FUNCTION completa
PARAMETER xdat
IF  .NOT. EMPTY(xdat)
     xdat = PADL(ALLTRIM(xdat), 2,  ;
            '0')
ENDIF
RETURN .T.
*
FUNCTION val_nvo
PRIVATE nreg, mret
nreg = RECNO()
mret = .T.
IF LASTKEY() <> 27
     IF  .NOT. EMPTY(m.detalle)  ;
         .AND.  .NOT.  ;
         EMPTY(m.tippre) .AND.   ;
         .NOT. EMPTY(m.generic)  ;
         .AND.  .NOT.  ;
         EMPTY(m.codpart)
          DO CASE
               CASE  .NOT.  ;
                     EMPTY(m.espn4)
                    xcad = m.tippre +  ;
                           m.generic +  ;
                           m.sgn1 +  ;
                           m.sgn2 +  ;
                           m.espn1 +  ;
                           m.espn2 +  ;
                           m.espn3 +  ;
                           SPACE(2)
               CASE  .NOT.  ;
                     EMPTY(m.espn3)
                    xcad = m.tippre +  ;
                           m.generic +  ;
                           m.sgn1 +  ;
                           m.sgn2 +  ;
                           m.espn1 +  ;
                           m.espn2 +  ;
                           SPACE(4)
               CASE  .NOT.  ;
                     EMPTY(m.espn2)
                    xcad = m.tippre +  ;
                           m.generic +  ;
                           m.sgn1 +  ;
                           m.sgn2 +  ;
                           m.espn1 +  ;
                           SPACE(6)
               CASE  .NOT.  ;
                     EMPTY(m.espn1)
                    xcad = m.tippre +  ;
                           m.generic +  ;
                           m.sgn1 +  ;
                           m.sgn2 +  ;
                           SPACE(8)
               CASE  .NOT.  ;
                     EMPTY(m.sgn2)
                    xcad = m.tippre +  ;
                           m.generic +  ;
                           m.sgn1 +  ;
                           SPACE(10)
               CASE  .NOT.  ;
                     EMPTY(m.sgn1)
                    xcad = m.tippre +  ;
                           m.generic +  ;
                           SPACE(12)
               OTHERWISE
                    xcad = SPACE(14)
          ENDCASE
          IF  .NOT. SEEK(m.tippre +  ;
              m.generic + m.sgn1 +  ;
              m.sgn2 + m.espn1 +  ;
              m.espn2 + m.espn3 +  ;
              m.espn4)
               mret = .T.
          ELSE
               DO standby WITH  ;
                  'Esta Asgnacion ya fue Ingresada'
               mret = .F.
          ENDIF
          IF  .NOT. SEEK(xcad)
               DO standby WITH  ;
                  'No Existe Nivel Anterior'
               mret = .F.
          ENDIF
     ELSE
          DO standby WITH  ;
             'Faltan Datos para procesar la Informaci¢n'
          mret = .F.
     ENDIF
ELSE
     mret = .T.
ENDIF
RETURN mret
*
PROCEDURE valasip
m.codpart = m.tippre + m.generic +  ;
            m.sgn1 + m.sgn2 +  ;
            m.espn1 + m.espn2 +  ;
            m.espn3 + m.espn4
SHOW GET m.codpart
RETURN
*
PROCEDURE elimi
SELECT catasi
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
SELECT catasi
vtemp = RECNO()
IF EOF()
     DO standby WITH vmens08
     RETURN
ELSE
     DO reporte WITH 2, 'CatAsi',  ;
        ' Codigos de Ingresos '
ENDIF
SELECT catasi
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
