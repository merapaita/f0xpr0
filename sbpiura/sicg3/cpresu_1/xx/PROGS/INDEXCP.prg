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
          DEFINE POPUP _yt  FROM 9,55;
         SHADOW COLOR &L_COL
     ELSE
          DEFINE POPUP _yt FROM 9,  ;
                 55 COLOR SCHEME  ;
                 c_popup
     ENDIF
     DEFINE BAR 1 OF _yt PROMPT  ;
            '\<a Parámetros'
     DEFINE BAR 2 OF _yt PROMPT  ;
            '\<b Hoja de Control'
     DEFINE BAR 3 OF _yt PROMPT  ;
            '\<c Hoja Modificación'
     DEFINE BAR 4 OF _yt PROMPT  ;
            '\<d Asientos'
     DEFINE BAR 5 OF _yt PROMPT  ;
            '\<f Personal'
     DEFINE BAR 6 OF _yt PROMPT  ;
            '\<e TODOS'
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
IF cual = 1 .OR. cual = 6
     USE EXCLUSIVE ParMae
     PACK
     = ordena( ;
       'TIPO+ALLT(CODIGO)+CODIGOAUX', ;
       'PARMAE1')
     = ordena( ;
       'TIPO+DESCRI+ALLT(CODIGO)+CODIGOAUX', ;
       'parmae2')
     = ordena('descri','parmae3')
     USE
ENDIF
IF cual = 2 .OR. cual = 6
     USE EXCLUSIVE HojCon
     PACK
     = ordena('NumMes+NumHc', ;
       'HojCon1')
     = ordena('NumMes+NumHm', ;
       'HojCon2')
     = ordena('NumMes+NumHc', ;
       'HojCon3', ;
       "Estcon='  '.AND.Estado<>'99'" ;
       )
     = ordena('NumMes+NumHc', ;
       'HojCon4',"Estado='20'")
     = ordena('NumMes+NumHc', ;
       'HojCon5', ;
       "LEFT(NUMHC,1)$'AEIPT'")
     = ordena('NumMes+NumHc', ;
       'HojCon6', ;
       "LEFT(NUMHC,1)$'0123456789'" ;
       )
     = ordena('NumMes+NumHc', ;
       'HojCon7', ;
       "LEFT(NUMHC,1)='E'")
     = ordena('NumMes+NumHc', ;
       'HojCon8', ;
       "LEFT(NUMHC,1)='I'")
     = ordena('NumMes+NumHc', ;
       'HojCon9', ;
       "LEFT(NUMHC,1)='T'")
     = ordena('NumMes+NumHc', ;
       'HojCon10', ;
       "LEFT(NUMHC,1)='P'")
     = ordena( ;
       'TipPrv+ALLT(CodPrv)+ALLT(CodEmp)+ALLT(CodOtr)', ;
       'HojCon11')
     USE
     USE EXCLUSIVE IteHc
     PACK
     = ordena('NumMes+NumHc', ;
       'IteHc1')
     = ordena('CodPart','IteHc2')
     = ordena('NumMes+NumHc', ;
       'IteHc4',"estado='92'")
     = ordena( ;
       'NumMes+tipfun+codfte+codprg+codsubpr+codproy+codsupry+codpart', ;
       'IteHc5',"tipfun='I'")
     USE
ENDIF
IF cual = 3 .OR. cual = 6
     USE EXCLUSIVE Hojmod
     PACK
     = ordena('NumMes+NumHm', ;
       'Hojmod1')
     = ordena('NumMeshc+NumHC', ;
       'Hojmod2')
     = ordena('NumMes+NumHm', ;
       'Hojmod3',"Estado='00'")
     USE
ENDIF
IF cual = 4 .OR. cual = 6
     USE EXCLUSIVE AstPre
     PACK
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta', ;
       'AstPre1',"Tipdoc='H/C'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre2',"Tipdoc='C/P'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta', ;
       'AstPre3',"Tipdoc='H/M'")
     = ordena('Tipo+Cuenta', ;
       'AstPre4')
     = ordena( ;
       'NumMes+NumRef+TipDoc', ;
       'AstPre5')
     = ordena( ;
       'Tipo+periodo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre6',"Tipdoc='R/I'")
     = ordena( ;
       'NumMes+NumRef+CodCtc', ;
       'AstPre7',"Tipdoc='C/P'")
     = ordena('Tipo+Cuenta', ;
       'AstPre8',"ESTADO#'99'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre9',"Tipdoc='N/A'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre10', ;
       "Tipdoc='N/C'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre11', ;
       "Tipdoc='B/D'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre12', ;
       "Tipdoc='NOC'")
     = ordena( ;
       'NumMes+NumRef+CodCtc', ;
       'AstPre13', ;
       "Tipdoc='N/A'")
     = ordena( ;
       'NumMes+NumRef+CodCtc', ;
       'AstPre14', ;
       "Tipdoc='N/C'")
     = ordena( ;
       'NumMes+NumRef+CodCtc', ;
       'AstPre15', ;
       "Tipdoc='B/D'")
     = ordena( ;
       'NumMes+NumRef+CodCtc', ;
       'AstPre16', ;
       "Tipdoc='NOC'")
     = ordena( ;
       'Tipo+NumMes+NumRef+Cuenta+CodCtc', ;
       'AstPre18', ;
       "Tipdoc='H/A'")
     = ordena( ;
       'NumMes+NumRef+CodCtc', ;
       'AstPre19', ;
       "Tipdoc='H/A'")
     = ordena( ;
       'Periodo+NumMes+NumRef+CodCtc', ;
       'AstPre20', ;
       "Tipdoc='R/I'")
     = ordena( ;
       'NumMes+NumRef+TipDoc+Anureb', ;
       'AstPre21')
     USE
ENDIF
IF cual = 5 .OR. cual = 6
     USE EXCLUSIVE personal
     PACK
     = ordena('CODIGO', ;
       'personal1')
     = ordena('DESCRI', ;
       'personal2')
     USE
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
          _ffun = 2
     CASE _ff = 'PARMAE'
          _ffun = 1
     OTHERWISE
          _ffun = 0
ENDCASE
RETURN _ffun
*
