PARAMETER _fil
PRIVATE _auto, _path, ss, pos
ss = SET('PATH')
pos = AT(';', ss)
_path = IIF(pos = 0, ss,  ;
        SUBSTR(ss, 1, AT(';', ss) -  ;
        1))
_auto = IIF(PARAMETERS() > 0, .T.,  ;
        .F.)
CLOSE DATABASES
IF  .NOT. WEXIST('msg2user')
     DEFINE WINDOW msg2user FROM  ;
            19, 27 TO 23, 77
ENDIF
IF  .NOT. _auto
     IF escolor
          DEFINE POPUP _yt  FROM 6,55;
         SHADOW COLOR &L_COL
     ELSE
          DEFINE POPUP _yt FROM 6,  ;
                 55 COLOR SCHEME  ;
                 c_popup
     ENDIF
     DEFINE BAR 1 OF _yt PROMPT  ;
            '\<a Par metros'
     DEFINE BAR 2 OF _yt PROMPT  ;
            '\<b Calendario'
     DEFINE BAR 3 OF _yt PROMPT  ;
            '\<c Presupuesto'
     DEFINE BAR 4 OF _yt PROMPT  ;
            '\<d Cr‚ditos Sup'
     DEFINE BAR 5 OF _yt PROMPT  ;
            '\<e Transferencias'
     DEFINE BAR 6 OF _yt PROMPT  ;
            '\<f E-5s'
     DEFINE BAR 7 OF _yt PROMPT  ;
            '\<g TODOS'
     ON SELECTION POPUP _yt DO ORGANIZA;
WITH BAR()
     ACTIVATE POPUP _yt
     RELEASE POPUP _yt
ELSE
     x = _num(_fil)
     DO organiza WITH x
ENDIF
RETURN
*
PROCEDURE organiza
PARAMETER cual
DEFINE WINDOW working FROM 02, 40  ;
       TO 23, 76 FLOAT COLOR  ;
       SCHEME 1
ACTIVATE WINDOW working
CLOSE DATABASES
SET EXCLUSIVE ON
SET TALK ON
_con = SET('CONSOLE')
SET CONSOLE ON
ACTIVATE WINDOW working
CLEAR
IF cual = 1 .OR. cual = 8
     USE EXCLUSIVE ParMae
     PACK
     = ordena( ;
       'Tipo+allt(Codigo)+CodigoAux', ;
       'ParMae1')
     = ordena( ;
       'TIPO+DESCRI+CODIGO+CODIGOAUX', ;
       'ParMae2')
     = ordena('DESCRI','ParMae3', ;
       "TIPO='CODRET'")
     USE
ENDIF
IF cual = 2 .OR. cual = 8
     USE EXCLUSIVE Calen
     PACK
     = ordena( ;
       'PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART', ;
       'Calen1')
     = ordena( ;
       'PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART+NUMMES', ;
       'Calen2')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,30)+CODFTE+NUMMES+CODPART', ;
       'Calen4')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,25)+CODFTE+NUMMES+CODPART', ;
       'Calen5')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,20)+CODFTE+NUMMES+CODPART', ;
       'Calen6')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,14)+CODFTE+NUMMES+CODPART', ;
       'Calen7')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,10)+CODFTE+NUMMES+CODPART', ;
       'Calen8')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,7)+CODFTE+NUMMES+CODPART', ;
       'Calen9')
     USE
ENDIF
IF cual = 3 .OR. cual = 8
     USE EXCLUSIVE MaePar
     PACK
     = ordena( ;
       'PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE', ;
       'MaePar1')
     USE
     USE EXCLUSIVE ItePar
     PACK
     = ordena( ;
       'PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART', ;
       'ItePar1')
     = ordena( ;
       'PERIODO+CODCAD+CODFTE+TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2', ;
       'ItePar2')
     = ordena( ;
       'PERIODO+CODCAD+CODFTE+CODPART', ;
       'ItePar3')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART', ;
       'ItePar4')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART', ;
       'ItePar5')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART', ;
       'ItePar6')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART', ;
       'ItePar7')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART', ;
       'ItePar8')
     = ordena( ;
       'PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART', ;
       'ItePar9')
     USE
     USE EXCLUSIVE MaePre
     PACK
     = ordena( ;
       'PERIODO+UNIGES+ UNIEJE+CODCAD', ;
       'MaePre1')
     = ordena('DESCRI','MaePre2')
     = ordena( ;
       'PERIODO+CODCAD+CODCOM+CODMET', ;
       'MaePre3')
     = ordena( ;
       'PERIODO+ UNIGES+ UNIEJE+ CODFUN+ CODPRG+ CODSPR+ ACTPRY+ CODCOM+CODMET', ;
       'MaePre4')
     = ordena( ;
       'PERIODO+ ACTPRY+ CODCOM', ;
       'MaePre5')
     USE
ENDIF
IF cual = 4 .OR. cual = 8
     USE EXCLUSIVE Ingreso
     PACK
     = ordena('CodIng+SubIng', ;
       'Ingreso1')
     USE
ENDIF
IF cual = 5 .OR. cual = 8
     USE EXCLUSIVE CreSup
     PACK
     = ordena( ;
       'PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)', ;
       'CreSup1')
     USE
     USE EXCLUSIVE IteCre
     PACK
     = ordena( ;
       'PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+ALLTRIM(CODFTE)', ;
       'IteCre1')
     = ordena( ;
       'PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+CODCAD+ALLTRIM(CODFTE)+CodPart', ;
       'IteCre2')
     = ordena( ;
       'PERIODO+CODCAD+CODFTE+CODPART', ;
       'itecre3')
     USE
ENDIF
IF cual = 6 .OR. cual = 8
     USE EXCLUSIVE TraPar
     PACK
     = ordena( ;
       'PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+CODFTE', ;
       'TraPar1')
     USE
     USE EXCLUSIVE IteTra
     PACK
     = ordena( ;
       'PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+CODFTE+CODCAD', ;
       'IteTra1')
     = ordena( ;
       'PERIODO+CODCAD+ALLTRIM(CODFTE)+ CodPart', ;
       'IteTra2')
     USE
ENDIF
IF cual = 7 .OR. cual = 8
ENDIF
RELEASE WINDOW working
CLOSE DATABASES
SET EXCLUSIVE OFF
SET TALK OFF
RETURN
*
FUNCTION ordena
PARAMETER key, idx, condi
IF PARAMETERS() < 3
     INDEX ON &key TAG (idx)
ELSE
     INDEX ON &key TAG (idx) FOR &condi
ENDIF
RETURN ''
*
FUNCTION _num
PARAMETER _ff
_ff = UPPER(_ff)
DO CASE
     CASE _ff = 'TODOS'
          _ffun = 1
     CASE _ff = 'CABDIC'
          _ffun = 2
     CASE _ff = 'PARMAE'
          _ffun = 3
     CASE _ff = 'PARMAE'
          _ffun = 3
     CASE _ff = 'CRESUP'
          _ffun = 4
     CASE _ff = 'TRAPAR'
          _ffun = 5
     OTHERWISE
          _ffun = 0
ENDCASE
RETURN _ffun
*
