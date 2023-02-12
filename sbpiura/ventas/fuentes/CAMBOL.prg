IF  .NOT. ve_passw('CAMBOL')
     DO standby WITH  ;
        'Proceso Cancelado'
     RETURN
ENDIF
CLOSE DATABASES
USE IN 1 Ventas ALIAS ventas  ;
    ORDER Ventas1
USE IN 2 Clientes ALIAS clien  ;
    ORDER Clientes1
USE IN 3 Parmae ALIAS parma ORDER  ;
    Parmae1
DO inicia
DO salida
RETURN
*
PROCEDURE inicia
crec = SPACE(7)
cnewcli = SPACE(6)
cnewrec = SPACE(10)
DEFINE WINDOW wlista FROM 3, 15  ;
       TO 20, 70 FLOAT TITLE  ;
       'Reporte de Kardex' DOUBLE  ;
       COLOR SCHEME 5
ACTIVATE WINDOW wlista
@ 01, 01 SAY  ;
  'Escoge Recibo a Cambiar :' GET  ;
  crec VALID valvta()
@ 05, 01 SAY  ;
  'Escoge Nuevo Nombre     :' GET  ;
  cnewcli VALID val_cli(cnewcli,5, ;
  33)
@ 07, 01 SAY  ;
  'Escoge Nuevo Recibo     :' GET  ;
  cnewrec
@ 15, 10 GET okcancel DEFAULT 1  ;
  SIZE 1, 11, 8 FUNCTION  ;
  '*TH \!\<OK;\?\<Cancela'
READ CYCLE
RELEASE WINDOW wlista
DO espera WITH 1,  ;
   'Cambio de Nombre'
IF LASTKEY() <> 27 .AND. okcancel <>  ;
   2
     SELECT ventas
     IF SEEK(crec)
          SCATTER MEMVAR
          m.camnom = .T.
          m.codvta = corvta()
          m.newcli = cnewcli
          m.codcp = cnewrec
          m.compag = 'R/I'
          m.feccp = m.fecsis
          APPEND BLANK
          GATHER MEMVAR
     ELSE
          DO standby WITH  ;
             'Venta no Encontrada'
     ENDIF
     DO espera WITH 2
ELSE
     DO espera WITH 2
     DO standby WITH  ;
        'Proceso Cancelado.'
ENDIF
RETURN
*
PROCEDURE salida
ACTIVATE SCREEN
CLOSE DATABASES
RETURN
*
FUNCTION corvta
PRIVATE cmes, cano, calias, mret
calias = ALIAS()
SELE &cAlias
mret = .T.
cmes = PADL(ALLTRIM(STR(MONTH(m.fecsis))),  ;
       2, '0')
cano = RIGHT(STR(YEAR(m.fecsis),  ;
       4), 2)
IF SEEK('VENTA ' + cano + cmes,  ;
   'Parma')
     cvta = cano + cmes +  ;
            PADL(ALLTRIM(STR(parma.nument +  ;
            1)), 3, '0')
     REPLACE parma.nument WITH  ;
             parma.nument + 1
ELSE
     cvta = cano + cmes +  ;
            PADL(ALLTRIM(STR(parma.nument +  ;
            1)), 3, '0')
     SELECT parma
     APPEND BLANK
     REPLACE tipo WITH 'VENTA ',  ;
             codigo WITH cano,  ;
             codigoaux WITH cmes,  ;
             descri WITH  ;
             'CORRELATIVO DE VENTAS: ' +  ;
             cmes + '/' + cano
     REPLACE nument WITH  ;
             parma.nument + 1
ENDIF
SELECT (calias)
IF EMPTY(cvta)
     DO standby WITH  ;
        'El C¢digo esta vacio'
     mret = .F.
ELSE
     nreg = RECNO()
     IF SEEK(cvta)
          DO standby WITH  ;
             'Ya esta Registrado esta Venta'
          mret = .F.
     ENDIF
ENDIF
RETURN IIF(mret, cvta, mret)
*
FUNCTION valvta
PRIVATE mret, calias, cord1,  ;
        cord2
mret = .T.
calias = ALIAS()
SELECT ventas.*, clien.nomcli  ;
       FROM Ventas, Clien WHERE  ;
       ventas.codcli =  ;
       clien.codcli AND  ;
       ventas.tipvta <> '05'  ;
       ORDER BY nomcli INTO  ;
       CURSOR tmpCre
SELECT tmpcre
IF  .NOT. EOF()
     vtempo = '°°°°°°°°°°° ®F2¯ Buscar   ®F10¯ Seleccionar  o  ®Esc¯ para cancelar°°°°°°°°°°°°'
     DEFINE WINDOW lista FROM 00,  ;
            00 TO 24, 79 FLOAT  ;
            TITLE vtempo DOUBLE  ;
            COLOR SCHEME 2
     ACTIVATE WINDOW lista
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ON KEY LABEL F2 DO Busdetcr
     BROWSE FIELDS nomcli :H =  ;
            'Cliente' : 30,  ;
            nomben :H = 'Difunto'  ;
            : 30, fecvta :H =  ;
            'F.Vta.', mtovta :H =  ;
            'Pag¢', mtocre :H =  ;
            'Credito' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE WINDOW  ;
            lista
     ON KEY LABEL F10
     ON KEY LABEL F2
     SET RELATION TO
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
     IF LASTKEY() <> 27
          crec = codvta
     ELSE
          mret = .F.
     ENDIF
ELSE
     DO standby WITH  ;
        'No Existen Ventas Registradas'
     mret = .F.
ENDIF
USE IN tmpcre
SELECT (calias)
SHOW GET crec
RETURN mret
*
PROCEDURE busdetcr
IF EOF()
     DO standby WITH vmens08
     RETURN
ENDIF
vtemp = IIF( .NOT. EOF(), RECNO(), - ;
        1)
DEFINE POPUP plista FROM 15, 40  ;
       COLOR SCHEME c_popup
DEFINE BAR 1 OF plista PROMPT  ;
       '\<a. Por Cliente     '
DEFINE BAR 2 OF plista PROMPT  ;
       '\<b. Por Difunto     '
ON SELECTION POPUP plista DEACTIVATE POPUP
ACTIVATE POPUP plista
RELEASE POPUP plista
cord1 = ORDER()
vbusca = SPACE(30)
DO CASE
     CASE BAR() = 1
          vnombre = 'Cliente :'
          INDEX ON UPPER(nomcli)  ;
                TO (vidx1)
     CASE BAR() = 2
          vnombre = 'Difunto : '
          INDEX ON UPPER(nomben)  ;
                TO (vidx1)
     OTHERWISE
          vbusca = ''
          vnombre = ''
          SET ORDER TO
ENDCASE
IF LASTKEY() <> 27
     DEFINE WINDOW lista FROM 13,  ;
            12 TO 16, 68 FLOAT  ;
            TITLE  ;
            ' °° B£squeda °° '  ;
            DOUBLE COLOR SCHEME  ;
            10
     ACTIVATE WINDOW lista
     @ 1, 2 SAY vnombre GET  ;
       vbusca FUNCTION '!'
     READ VALID val_read()
     DEACTIVATE WINDOW lista
     RELEASE WINDOW lista
ENDIF
IF EMPTY(vbusca) .OR. LASTKEY() =  ;
   27
ELSE
     SEEK ALLTRIM(vbusca)
     IF  .NOT. FOUND()
          DO standby WITH  ;
             'Cliente no Existe'
          IF vtemp = -1
               GOTO BOTTOM
          ELSE
               GOTO vtemp
          ENDIF
     ELSE
     ENDIF
ENDIF
RETURN
*
