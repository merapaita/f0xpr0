PARAMETER confirma
IF PARAMETERS() = 0
     confirma = .T.
ELSE
     confirma = .F.
ENDIF
CLOSE DATABASES
IF confirma
     IF  .NOT. yesno( ;
         '¨Desea indexar archivos?' ;
         )
          RETURN
     ENDIF
ENDIF
DO organiza WITH 1
CLOSE DATABASES
RETURN
*
PROCEDURE organiza
PARAMETER cual
CLOSE DATABASES
bases = 20
actual = 0
DO progreso WITH 1,  ;
   'Progreso de Indexaci¢n:'
SELECT 1
USE EXCLUSIVE IteUsu
PACK
= ordena('USUCLA+MODULO', ;
  'IteUsu1')
= ordena('SISTEMA+MODULO', ;
  'IteUsu2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE IteUsuOp
PACK
= ordena( ;
  'ALLTRIM(USER)+SISTEMA+MODULO+OPCION', ;
  'IteUsuOp1')
= ordena('ALLTRIM(USER)+CODACC', ;
  'IteUsuOp2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE MaeAcc
PACK
= ordena('SISTEMA+MODULO+OPCION', ;
  'MaeAcc1')
= ordena('CODACC','MaeAcc2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Usuario
PACK
= ordena('Usuario','Usuario1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE ArtMae
PACK
= ordena('LEFT(CODART,7)', ;
  'ArtMae1')
= ordena('UPPER(ALLTRIM(DESCRI))', ;
  'ArtMae2')
= ordena('LEFT(CODART,7)', ;
  'ArtMae3', ;
  "LEFT(CODART,3)='B59'")
= ordena('UPPER(ALLTRIM(DESCRI))', ;
  'ArtMae4', ;
  "LEFT(CODART,3)='B59'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Cuentas
PACK
= ordena('Cuenta','Cuentas1')
= ordena('DContra','Cuentas2', ;
  '.NOT.EMPTY(DCONTRA)')
= ordena('HContra','Cuentas3', ;
  '.NOT.EMPTY(HCONTRA)')
= ordena('Cuenta','Cuentas4', ;
  "UPPER(DETALLE)='S'")
= ordena('UPPER(DESCRI)', ;
  'Cuentas5')
= ordena('Cuenta','Cuentas6', ;
  "CUENTA='9'")
= ordena('TIPO+CUENTA', ;
  'Cuentas7')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE IteGI
PACK
= ordena('PERIODO+NUMGI+ITEM+CODART','IteGI1')
= ordena('CODART','IteGI2')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE ItePeco
PACK
= ordena('PERIODO+NUMPEC+ITEM+CODART','ItePeco1')
= ordena('CODART','ItePeco2')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE GuiInt
PACK
= ordena( ;
  'PERIODO+NUMGI+CODCAD+CODFTE', ;
  'GuiInt1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Peco
PACK
= ordena( ;
  'PERIODO+NUMPEC+CODFTE+CODCAD', ;
  'Peco1')
= ordena( ;
  'PERIODO+CODCAD+CODFTE+NUMPEC', ;
  'Peco2')
= ordena('PERIODO+CODDEP', ;
  'Peco3')
= ordena( ;
  'PERIODO+NUMPEC+CODFTE+CODCAD', ;
  'Peco4',"ESTADO='50'")
= ordena( ;
  'PERIODO+NUMPEC+CODFTE+CODCAD', ;
  'Peco4',"CODDEP='320000'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE InvIni
PACK
= ordena('PERIODO+NUMII', ;
  'InvIni1')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE IteArt
PACK
= ordena('TipBie+CodArt', 'IteArt1')
= ordena('UPPER( ALLTRIM( Descri ) )', 'IteArt2')
= ordena('CodArt','IteArt3')
= ordena('Cuenta','IteArt4')
= ordena('CODART','Iteart5', "CODART='17'")
= ordena('UPPER(ALLTRIM(DESCRI))', 'Iteart6',"CODART='17'")

*= ordena('TIPART+CODART','IteArt1')
*= ordena('UPPER(ALLTRIM(DESCRI))','IteArt2')
*= ordena('CodArt','IteArt3')
*= ordena('Cuenta','IteArt4')
*= ordena('CodArt','IteArt5',"CODART='17'")
*= ordena('UPPER(ALLTRIM(DESCRI))','IteArt6',"CODART='17'")
*= ordena('TIPART+CODART','IteArt7',"CODGEN='59'")
*= ordena('UPPER(ALLTRIM(DESCRI))','IteArt8',"CODGEN='59'")
USE

actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE IteII
PACK
= ordena('PERIODO+NUMII+ITEM+CODART','IteII1')
= ordena('PERIODO+NUMII+CODART+ITEM','IteII2')
= ordena('CODART','IteII3')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE Kardexv
PACK
= ordena('PERIODO+TipKar+CODART+CORREL+CORAUX','Kardexv1',"ESTADO#'99'")
= ordena('PERIODO+TIPDOC+NUMDOC+ITEM+CODART', 'Kardexv2')
USE

actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Parmae
PACK
= ordena( ;
  'TIPO+ALLTRIM(CODIGO)+CODIGOAUX', ;
  'Parmae1')
= ordena( ;
  'TIPO+DESCRI+CODIGO+CODIGOAUX', ;
  'Parmae2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Promae
PACK
= ordena('CodPrv','Promae1')
= ordena('UPPER(ALLTRIM(NOMPRO))', ;
  'Promae2')
= ordena('NumRuc','Promae3')
= ordena('RL_Nom','Promae4')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE StkAlmV
PACK
= ordena('PERIODO+TipKar+CODART+CORREL','StkAlmV1')
= ordena('PERIODO+TipKar+UPPER(DESART)+CORREL','StkAlmV2')
= ordena('PERIODO+TipKar+CODART+CORREL','StkAlmv3','STKALMV.SALFRAC#0')
= ordena('PERIODO+TipKar+UPPER(DESART)+CORREL','StkAlmv4','STKALMV.SALFRAC#0')
USE

actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Nea
PACK
= ordena( ;
  'PERIODO+NUMNea+CODFTE+CODCAD', ;
  'Nea1')
USE

actual = actual + 1
DO progreso WITH 2, '', bases, actual
USE EXCLUSIVE IteNea
PACK
= ordena('PERIODO+NUMNea+ITEM+CODART','IteNea')
= ordena('CODART','IteNea2')
USE

actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
CLOSE DATABASES
WAIT WINDOW TIMEOUT 3  ;
     'Fin de proceso de indexaci¢n'
DO progreso WITH 3
RETURN
*
FUNCTION ordena
PARAMETER key, cdx, condi
WAIT WINDOW NOWAIT  ;
     'Generando Indice ' +  ;
     ALLTRIM(cdx) +  ;
     ' del archivo ' +  ;
     ALLTRIM(ALIAS())
IF PARAMETERS() < 3
     INDEX ON &key TAG (cdx)
ELSE
     INDEX ON &key TAG (cdx) FOR &condi
ENDIF
RETURN ''
*
FUNCTION _num
PARAMETER _ff
_ff = UPPER(_ff)
DO CASE
     CASE _ff = 'TODOS'
          _ffun = 1
     CASE _ff = 'E'
          _ffun = 2
     CASE _ff = 'ORDCOM'
          _ffun = 3
     CASE _ff = ''
          _ffun = 4
     CASE _ff = ''
          _ffun = 5
     CASE _ff = ''
          _ffun = 6
     CASE _ff = ''
          _ffun = 7
     OTHERWISE
          _ffun = 0
ENDCASE
RETURN _ffun
*
