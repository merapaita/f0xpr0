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
bases = 22
actual = 0
DO progreso WITH 1,  ;
   'Progreso de Indexaci¢n:'
USE EXCLUSIVE Usuario
PACK
REINDEX
= ordena('Usuario','Usuario1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE IteUsu
PACK
REINDEX
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
REINDEX
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
REINDEX
= ordena('SISTEMA+MODULO+OPCION', ;
  'MaeAcc1')
= ordena('CODACC','MaeAcc2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE InvIni
PACK
REINDEX
= ordena('PERIODO+NUMII', ;
  'InvIni1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE IteII
PACK
REINDEX
= ordena( ;
  'PERIODO+NUMII+ITEM+CODART', ;
  'IteII1')
= ordena( ;
  'PERIODO+NUMII+CODART+ITEM', ;
  'IteII2')
= ordena('CODART','IteII3')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fOrdPed
PACK
REINDEX
= ordena('PERIODO+NUMPED', ;
  'fOrdPed1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fIteOP
PACK
REINDEX
= ordena( ;
  'PERIODO+NUMPED+ITEM+CODART', ;
  'fIteOP1')
= ordena('CODART','fIteOP2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fComPag
PACK
REINDEX
= ordena('PERIODO+NUMCP', ;
  'fComPag1')
= ordena('Factura','fComPag2', ;
  "TIPCP='FAC'")
= ordena('BolVta','fComPag3', ;
  "TIPCP='B/V'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fIteCP
PACK
REINDEX
= ordena('PERIODO+NUMCP', ;
  'fIteCP1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fDonac
PACK
REINDEX
= ordena('PERIODO+NUMDon', ;
  'fDonac1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fIteDon
PACK
REINDEX
= ordena( ;
  'PERIODO+NUMDon+ITEM+CODART', ;
  'fIteDon1')
= ordena('CODART','fIteDon2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fOrdCom
PACK
REINDEX
= ordena('PERIODO+NUMOC', ;
  'fOrdCom1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE fIteOC
PACK
REINDEX
= ordena( ;
  'PERIODO+NUMOC+ITEM+CODART', ;
  'fIteOC1')
= ordena('CODART','fIteOC2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Kardexv
PACK
REINDEX
= ordena( ;
  'PERIODO+CODART+CORREL+CORAUX', ;
  'Kardexv1',"ESTADO#'99'")
= ordena( ;
  'PERIODO+TIPDOC+NUMDOC+ITEM+CODART', ;
  'Kardexv2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE StkAlmV
PACK
REINDEX
= ordena('PERIODO+CODART+CORREL', ;
  'StkAlmV1')
= ordena('PERIODO+DESART+CORREL', ;
  'StkAlmV2')
= ordena('PERIODO+CODART+CORREL', ;
  'StkAlmv3', ;
  'STKALMV.SALFRAC#0')
= ordena( ;
  'PERIODO+UPPER(DESART)+CORREL', ;
  'StkAlmv4', ;
  'STKALMV.SALFRAC#0')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE ArtMae
PACK
REINDEX
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
USE EXCLUSIVE IteArt
PACK
REINDEX
= ordena('TIPART+CODART', ;
  'IteArt1')
= ordena('UPPER(ALLTRIM(DESCRI))', ;
  'IteArt2')
= ordena('CodArt','IteArt3')
= ordena('Cuenta','IteArt4')
= ordena('CodArt','IteArt5', ;
  "CODART='17'")
= ordena('UPPER(ALLTRIM(DESCRI))', ;
  'IteArt6',"CODART='17'")
= ordena('TIPART+CODART', ;
  'IteArt7',"CODGEN='59'")
= ordena('UPPER(ALLTRIM(DESCRI))', ;
  'IteArt8',"CODGEN='59'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Cuentas
PACK
REINDEX
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
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Parmae
PACK
REINDEX
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
REINDEX
= ordena('CodPrv','Promae1')
= ordena('UPPER(ALLTRIM(NOMPRO))', ;
  'Promae2')
= ordena('NumRuc','Promae3')
= ordena('RL_Nom','Promae4')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE PrvAux
PACK
REINDEX
= ordena('CodPrv','PrvAux1')
= ordena('CodCla','PrvAux2')
= ordena('LEFT(CODCLA,3)', ;
  'PrvAux3')
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
