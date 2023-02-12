USE IN 1 Parmae ALIAS parma ORDER  ;
    Parmae1
USE IN 2 ComPag ALIAS compag  ;
    ORDER ComPag5
USE IN 4 Cheque ALIAS cheque  ;
    ORDER Cheque4
USE IN 5 Auxil ALIAS auxil ORDER  ;
    Auxil6
DEFINE WINDOW wind_1 FROM 10, 20  ;
       TO 14, 60 FLOAT TITLE  ;
       ' Periodo de Consulta '  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW wind_2 FROM 09, 15  ;
       TO 17, 68 FLOAT DOUBLE  ;
       COLOR SCHEME 5
DEFINE WINDOW wind_3 FROM 02, 16  ;
       TO 22, 64 FLOAT TITLE  ;
       '±± F2=BUSCAR ±±  PROVEEDORES  ±± F10=SALIR ±±'  ;
       DOUBLE COLOR SCHEME 10
DEFINE WINDOW wind_4 FROM 17, 16  ;
       TO 21, 64 FLOAT TITLE  ;
       ' ±± ESC = SALIR ±± '  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW wind_8 FROM 08, 26  ;
       TO 19, 76 FLOAT TITLE  ;
       ' B£squeda de Cheques '  ;
       DOUBLE COLOR SCHEME 5
DEFINE WINDOW wind_9 FROM 1, 0 TO  ;
       23, 79 TITLE  ;
       '®®  Revisi¢n  de  Cheques  ¯¯'  ;
       COLOR SCHEME 02
STORE 0 TO vmtochq, vtpo
vnumchq = SPACE(14)
PRIVATE vchoice
DO WHILE .T.
     ACTIVATE WINDOW wind_8
     vbus = 1
     vest = 1
     okcancel = 1
     @ 01, 01 SAY  ;
       ' Tipo de B£squeda : ' GET  ;
       vbus PICTURE  ;
       '@^ Por Monto;Por Proveedor;'
     @ 05, 01 SAY  ;
       ' Monto del cheque : '
     @ 05, 22 GET vmtochq PICTURE  ;
       '999,999,999.99' WHEN vbus =  ;
       1
     @ 08, 10 GET okcancel  ;
       DEFAULT 1 SIZE 1, 11, 8  ;
       FUNCTION  ;
       '*TH \!\<OK;\?\<Cancela'
     READ VALID exi() CYCLE
     DEACTIVATE WINDOW wind_8
     vchoice = vbus
     IF LASTKEY() = 27 .OR.  ;
        okcancel = 2
          CLOSE DATABASES
          EXIT
     ENDIF
     DO CASE
          CASE vchoice = 1
               DO mtochq
          CASE vchoice = 2
               DO prov
     ENDCASE
ENDDO
RELEASE WINDOW wind_2
RELEASE WINDOW wind_1
RELEASE WINDOW wind_3
CLOSE DATABASES
SET EXACT OFF
RETURN
*
PROCEDURE mtochq
SET EXACT ON
vindm = SYS(3) + '.IDX'
SELECT cheque
xord = ORDER()
INDEX ON valchq TO (vindm) FOR  ;
      VAL(LEFT(codpart, 2)) < 15  ;
      .AND. VAL(LEFT(codpart, 2)) >  ;
      0
SEEK vmtochq
IF FOUND()
     DEFINE WINDOW wind_16 FROM  ;
            02, 00 TO 18, 79  ;
            FLOAT TITLE ' ' +  ;
            'Cheques con monto = ' +  ;
            STR(vmtochq, 15, 2) +  ;
            ' ' FOOTER  ;
            ' [F4] LISTAR '  ;
            DOUBLE COLOR SCHEME  ;
            02
     ON KEY LABEL F4 DO Imprimir 
     ON KEY LABEL F10 KEYBOARD CHR(23)
     ACTIVATE WINDOW wind_16
     BROWSE FIELDS numchq :H =  ;
            '  Cheque', x1 =  ;
            nummes + '.' + numcp  ;
            :H = 'C/P', codctc,  ;
            fecchq :H = 'Fecha',  ;
            vestado =  ;
            UPPER(verchest(estado))  ;
            : 3 :H = '  Estado',  ;
            nomgir : 25 :H =  ;
            'Girado a' NOMENU  ;
            NOAPPEND NOEDIT  ;
            NODELETE KEY vmtochq  ;
            IN wind_16
     DEACTIVATE WINDOW wind_16
ELSE
     DO standby WITH  ;
        ' Cheque no existe '
ENDIF
SELECT cheque
SET INDEX TO
ERASE (vindm)
SET ORDER TO xord
RETURN
*
PROCEDURE prov
SET CONFIRM OFF
vnom = SPACE(1)
vfecini = DATE()
vfecfin = DATE()
SELECT auxil
SET ORDER TO AUXIL6
ON KEY LABEL F10 KEYBOARD CHR(23)
ON KEY LABEL F2 DO BUSCA
BROWSE FIELDS descri :H =  ;
       '                       NOMBRE'  ;
       : 47 NOMENU NOEDIT  ;
       NODELETE WINDOW wind_3  ;
       NOREFRESH
IF LASTKEY() = 27
     RETURN
ENDIF
vcodprv = codigo
vdescri = descri
ON KEY LABEL F10
ON KEY LABEL F2
DEFINE WINDOW wind_6 FROM 06, 00  ;
       TO 19, 79 FLOAT TITLE ' ' +  ;
       ALLTRIM(vdescri) + ' '  ;
       FOOTER ' [F4] LISTAR '  ;
       DOUBLE COLOR SCHEME 02
ACTIVATE WINDOW wind_1
@ 1, 1 SAY ' Desde : ' GET  ;
  vfecini
@ 1, 18 SAY '    al : ' GET  ;
  vfecfin
READ
DEACTIVATE WINDOW wind_1
DEFINE WINDOW xwait FROM 21, 50  ;
       TO 23, 75 COLOR SCHEME 05
ACTIVATE WINDOW xwait
@ 0, 0 SAY  ;
  ' Consulta en Proceso....'  ;
  COLOR W+/N* 
vdbf = SYS(3)
SELECT compag
SET ORDER TO Compag5
SEEK ALLTRIM(vcodprv)
IF FOUND()
     CREATE DBF (vdbf) (nummes C  ;
            (2), numcp C (4),  ;
            codctc C (14))
     SELECT compag
     SCAN WHILE codprv =  ;
          ALLTRIM(vcodprv)
          IF BETWEEN(feccp,  ;
             vfecini, vfecfin)
               SELECT 3
               APPEND BLANK
               REPLACE nummes  ;
                       WITH  ;
                       compag.nummes
               REPLACE numcp WITH  ;
                       compag.numcp
               REPLACE codctc  ;
                       WITH  ;
                       compag.codctc
          ENDIF
          SELECT compag
     ENDSCAN
ELSE
     RELEASE WINDOW xwait
     DO standby WITH  ;
        ' No hay cheques para dicho proveedor '
     RETURN
ENDIF
SELECT ch.numchq, ch.estado,  ;
       ch.valchq, ch.fecchq,  ;
       ch.nummes, ch.numcp,  ;
       ch.codctc FROM Cheque ch,  ;
       (vdbf) cp WHERE cp.nummes =  ;
       ch.nummes AND cp.numcp =  ;
       ch.numcp AND cp.codctc =  ;
       ch.codctc INTO CURSOR chq
RELEASE WINDOW xwait
SELECT chq
ON KEY LABEL F4 DO Imprimir 
ACTIVATE WINDOW wind_6
BROWSE FIELDS numchq :H =  ;
       '  Cheque', mes = nummes,  ;
       numcp, codctc, fecchq :H =  ;
       'Fecha', vestado =  ;
       verchest(estado) :H =  ;
       '  Estado', valchq :H =  ;
       '  Monto ' :P =  ;
       '999,999,999.99' NOMENU  ;
       NOAPPEND NOEDIT NODELETE  ;
       IN wind_6
USE
SELECT 3
USE
DEACTIVATE WINDOW wind_6
ON KEY LABEL F4
RETURN
*
FUNCTION salir
PARAMETER vsal
IF LASTKEY() = -9
     vsal = .T.
ENDIF
RETURN .T.
*
PROCEDURE busca
SET EXACT OFF
ACTIVATE WINDOW wind_4
vnom = SPACE(25)
@ 1, 1 SAY ' Nombre : ' GET vnom
READ
DEACTIVATE WINDOW wind_4
vreg = RECNO()
SEEK ALLTRIM(vnom)
IF  .NOT. FOUND()
     GOTO vreg
ENDIF
SET EXACT ON
RETURN
*
PROCEDURE imprimir
DO CASE
     CASE vbus = 2
          SELECT chq
          DO reporte WITH 2,  ;
             'ChqPrv',  ;
             'Consulta de cheques'
     CASE vbus = 1
          SELECT cheque
          SET FILTER TO valchq = vmtochq
          DO reporte WITH 2,  ;
             'ChqMTo',  ;
             'Consulta de cheques'
          SET FILTER TO
ENDCASE
RETURN
*
FUNCTION novacio
IF EMPTY(vnumchq) .AND. LASTKEY() <>  ;
   27 .AND. okcancel <> 2
     DO standby WITH  ;
        'N£mero de cheque est  en blanco'
     RETURN .F.
ENDIF
RETURN .T.
*
FUNCTION exi
IF LASTKEY() = 27 .OR. okcancel =  ;
   1 .OR. okcancel = 2
     RETURN .T.
ENDIF
RETURN .F.
*
FUNCTION verchest
PARAMETER vest
PRIVATE vfun
vfun = SPACE(10)
DO CASE
     CASE vest = '00'
          vfun = 'Girado'
     CASE vest = '10'
          vfun = 'Autorizado'
     CASE vest = '40'
          vfun = 'Entregado'
     CASE vest = '99'
          vfun = 'Anulado     '
ENDCASE
RETURN vfun
*
