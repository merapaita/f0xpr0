USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 Auxil ALIAS auxil ORDER  ;
    Auxil1
mtipo = 'AUXIL '
SET FILTER TO mtipo = parma.tipo
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
IF escolor
     DEFINE POPUP Polman FROM 08,55 TO;
17,75 PROMPT FIELD PROPER(Descri);
 SHADOW COLOR &L_COL
ELSE
     DEFINE POPUP polman FROM 08,  ;
            55 TO 17, 75 PROMPT  ;
            FIELDS PROPER(descri)  ;
            COLOR SCHEME c_popup
ENDIF
ON SELECTION POPUP polman Do Auxilio WITH;
ALLTRIM(Descri), Codigo
ACTIVATE POPUP polman
RETURN
*
PROCEDURE salida
RELEASE POPUP polman
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
PROCEDURE auxilio
PARAMETER mdescri, xtipo
PRIVATE vmens01, vmens02, vmens03,  ;
        vmens04, vmens05, vmens06,  ;
        vmens07, vmens08, vmens09,  ;
        vmens10, vmens11
vmens01 = ' Cat logo de ' +  ;
          mdescri + ' '
vmens02 = 'Revisi¢n de ' +  ;
          mdescri
vmens03 = 'Digite c¢digo de Auxiliar que desea :'
vmens04 = 'Dicho Auxiliar no fue encontrado.'
vmens05 = 'No existe Registro anterior.'
vmens06 = 'No existe Registro siguiente.'
vmens07 = '¨Est  seguro que desea ELIMINAR ‚ste Registro?'
vmens08 = 'No hay registros para procesar'
xtipo = ALLTRIM(xtipo)
SELECT parma
SET FILTER TO
SELECT auxil
SET FILTER TO auxil.tipo = xtipo
GOTO BOTTOM
SCATTER BLANK MEMVAR
DO reinicia
DO repantalla
DO vista
STORE .T. TO ven_accion
DO WHILE ven_accion
     ACTIVATE SCREEN
     ACTIVATE MENU mmenu
ENDDO
DO fin_opcion
RETURN
*
PROCEDURE reinicia
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
PROCEDURE repantalla
ACTIVATE WINDOW wind_0
CLEAR
@ 2, 2 SAY '            C¢digo:'
@ 4, 2 SAY '       Descripci¢n:'
@ 6, 2 SAY '         Direcci¢n:'
@ 8, 2 SAY '        Tel‚fono 1:'
@ 8, 40 SAY 'Telefono 2:'
@ 10, 2 SAY '           Anexo 1:'
@ 10, 40 SAY '   Anexo 2:'
@ 12, 2 SAY '             R.U.C:'
@ 14, 2 SAY '     Observaciones:'
@ 16, 2 SAY ' Auxiliar es Banco:'
@ 18, 2 SAY '             Banco:'
@ 05, 2 SAY 'Auxiliar  / Codigo:'
RETURN
*
PROCEDURE vista
ACTIVATE WINDOW wind_0
SELECT auxil
SCATTER MEMVAR
@ 2, 22 SAY m.codigo PICTURE  ;
  '!!!!!!!!!!'
@ 4, 22 SAY m.descri
@ 05, 22 SAY m.newtip
@ 05, 25 SAY m.newcod
@ 6, 22 SAY m.direccion
@ 8, 22 SAY m.telf_1
@ 8, 52 SAY m.telf_2
@ 10, 22 SAY m.anexo_1
@ 10, 52 SAY m.anexo_2
@ 12, 22 SAY m.ruc
@ 14, 22 SAY m.observ
@ 16, 22 SAY IIF(m.tipban, 'Si',  ;
  'No')
@ 18, 22 SAY m.banco
@ 05, 22 SAY m.newtip
@ 05, 25 SAY m.newcod
IF  .NOT. EMPTY(m.banco)
     @ 18, 26 SAY  ;
       val_para(m.banco, ;
       'BANCOS')
ENDIF
RETURN
*
PROCEDURE revis
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
ACTIVATE WINDOW standby
vopcion = 'Descripci¢n'
@ 1, 4 SAY  ;
  'Revisi¢n Ordenado por : ' GET  ;
  vopcion PICTURE  ;
  '@M Descripci¢n,C¢digo     '
READ
DEACTIVATE WINDOW standby
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
IF vopcion = 'Descripci¢n'
     SET ORDER TO AUXIL10
ELSE
     SET ORDER TO AUXIL1
ENDIF
GOTO vtemp
HIDE MENU mmenu
ACTIVATE SCREEN
vtempo = '°°°°°°°°°°°Presione ®F10¯ para seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
ON KEY LABEL F10 KEYBOARD CHR(23)
BROWSE FIELDS codigo :H =  ;
       'C¢digo', descri :H =  ;
       'Descripci¢n', direccion  ;
       :H = 'Direcci¢n' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       WINDOW wind_1
vtempo = '°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°'
DO logos WITH rotulo1, vtempo
IF LASTKEY() = 27
     GOTO vtemp
ENDIF
SHOW MENU mmenu
ON KEY LABEL F10
SET ORDER TO AUXIL1
DO vista
RETURN
*
PROCEDURE busca
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = RECNO()
STORE SPACE(10) TO vcodigo
STORE SPACE(30) TO vdescri
tb = yesno( ;
     'Desea b£squeda por C¢digo (NO = por Descripci¢n)' ;
     )
IF LASTKEY() = 27
     DO vista
     RETURN
ENDIF
ACTIVATE WINDOW standby
@ 1, 1 SAY IIF(tb,  ;
  '     C¢digo : ',  ;
  'Descripci¢n : ')
IF tb
     @ 1, 15 GET vcodigo PICTURE  ;
       '!!!!!!!!!!'
ELSE
     @ 1, 15 GET vdescri PICTURE  ;
       REPLICATE('!', 30)
ENDIF
READ
DEACTIVATE WINDOW standby
IF tb
     SET ORDER TO AUXIL1
     SEEK xtipo +  ;
          ALLTRIM(vcodigo)
ELSE
     SET ORDER TO AUXIL10
     SEEK xtipo +  ;
          ALLTRIM(vdescri)
ENDIF
SET ORDER TO AUXIL1
IF EOF()
     DO standby WITH 'Dich'+ ;
        IIF(tb, 'o C¢digo ',  ;
        'a Descripci¢n ')+ ;
        'no fu‚ encontrada.'
     GOTO vtemp
ELSE
     DO vista
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
     xtipban = IIF(m.tipban, 'Si',  ;
               'No')
     @ 2, 22 SAY m.codigo PICTURE  ;
       '!!!!!!!!!!'
     @ 4, 22 GET m.descri
     @ 05, 22 GET m.newtip  ;
       PICTURE '!!'
     @ 05, 25 GET m.newcod  ;
       PICTURE '!!!!!!!!!!!!'
     @ 6, 22 GET m.direccion
     @ 8, 22 GET m.telf_1
     @ 8, 52 GET m.telf_2
     @ 10, 22 GET m.anexo_1
     @ 10, 52 GET m.anexo_2
     @ 12, 22 GET m.ruc
     @ 14, 22 GET m.observ
     @ 16, 22 GET xtipban PICTURE  ;
       '@M Si,No'
     @ 18, 22 GET m.banco PICTURE  ;
       '!!' VALID  ;
       val_para(m.banco,'BANCOS', ;
       ' ',26) WHEN xtipban =  ;
       'Si'
     READ
     IF LASTKEY() <> 27
          m.tipban = IIF(xtipban =  ;
                     'Si', .T.,  ;
                     .F.)
          IF  .NOT. m.tipban
               m.banco = SPACE(LEN(m.banco))
          ENDIF
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
GOTO BOTTOM
DO CASE
     CASE xtipo = '03' .OR. xtipo =  ;
          '09'
          mlong = 5
     OTHERWISE
          mlong = 6
ENDCASE
m.codigo = PADL(ALLTRIM(STR(VAL(auxil.codigo) +  ;
           1)), mlong, '0')
@ 2, 22 GET m.codigo PICTURE '@!'
READ
SELECT auxil
IF LASTKEY() = 27 .OR. m.codigo =  ;
   SPACE(6)
     DO standby WITH  ;
        'Proceso cancelado. No se graba ning£n cambio.'
     GOTO BOTTOM
ELSE
     SEEK xtipo + m.codigo
     IF FOUND()
          DO standby WITH  ;
             'Auxiliar ya est  registrado. Proceda a corregir datos.'
          DO vista
          DO corri
     ELSE
          xtipban = IIF(m.tipban,  ;
                    'Si', 'No')
          @ 04, 22 CLEAR TO 23,  ;
            78
          @ 04, 22 GET m.descri
          @ 05, 22 GET m.newtip  ;
            PICTURE '!!'
          @ 05, 25 GET m.newcod  ;
            PICTURE  ;
            '!!!!!!!!!!!!'
          @ 06, 22 GET  ;
            m.direccion
          @ 08, 22 GET m.telf_1
          @ 08, 52 GET m.telf_2
          @ 10, 22 GET m.anexo_1
          @ 10, 52 GET m.anexo_2
          @ 12, 22 GET m.ruc
          @ 14, 22 GET m.observ
          @ 16, 22 GET xtipban  ;
            PICTURE '@M Si,No'
          @ 18, 22 GET m.banco  ;
            PICTURE '!!' VALID  ;
            val_para(m.banco, ;
            'BANCOS',' ',26) WHEN  ;
            xtipban = 'Si'
          READ
          IF LASTKEY() <> 27
               agrega = .T.
               DO fox_appd WITH  ;
                  agrega
               IF agrega
                    m.tipo = xtipo
                    m.tipban = IIF(xtipban =  ;
                               'Si',  ;
                               .T.,  ;
                               .F.)
                    IF  .NOT.  ;
                        m.tipban
                         m.banco =  ;
                          SPACE(LEN(m.banco))
                    ENDIF
                    GATHER MEMVAR
                    SELECT auxil
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
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
ACTIVATE WINDOW standby
vopcion = 'Descripci¢n'
@ 1, 4 SAY  ;
  'Listado Ordenado por : ' GET  ;
  vopcion PICTURE  ;
  '@M Descripci¢n,C¢digo     '
READ
SELECT auxil
nreg = RECNO()
IF LASTKEY() <> 27
     IF vopcion = 'Descripci¢n'
          SET ORDER TO AUXIL10
          DO reporte WITH 2,  ;
             'Auxil2',  ;
             ' Auxiliares '
     ELSE
          DO reporte WITH 2,  ;
             'Auxil2',  ;
             ' Auxiliares '
     ENDIF
ENDIF
DEACTIVATE WINDOW standby
SELECT auxil
SET ORDER TO AUXIL1
GOTO nreg
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
SELECT parma
SET FILTER TO tipo = mtipo
DO logos WITH rotulo1, rotulo2
RETURN
*
