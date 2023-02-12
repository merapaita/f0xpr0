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
bases = 15
actual = 0
DO progreso WITH 1,  ;
   'Progreso de Indexaci¢n:'
SELECT 1
USE EXCLUSIVE ParMae
= ordena( ;
  'Tipo+alltrim(Codigo)+CodigoAux', ;
  'ParMae1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE CdrNec
PACK
= ordena('Periodo+CodDep', ;
  'CdrneC1')
USE
USE EXCLUSIVE IteCn
PACK
= ordena('Periodo+CodDep', ;
  'IteCn1')
= ordena('CodArt','IteCn2')
= ordena('Periodo+CodDep+CodArt', ;
  'IteCn3')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Pecosa
PACK
= ordena( ;
  'Periodo+NumPec+Codfte+Codcad', ;
  'Pecosa1')
= ordena( ;
  'Periodo+Codcad+Codfte+NumPec', ;
  'Pecosa2')
= ordena('Periodo+CodDep', ;
  'Pecosa3')
= ordena( ;
  'Periodo+NumPec+Codfte+Codcad', ;
  'Pecosa4',"Estado='50'")
= ordena( ;
  'Periodo+NumPec+Codfte+Codcad', ;
  'Pecosa4',"Coddep='320000'")
USE
USE EXCLUSIVE Itepec
PACK
= ordena('Periodo+NumPec', ;
  'Itepec1')
= ordena('CodArt+Periodo+NumPec', ;
  'Itepec2','empty(NumSc)')
= ordena('CodCad+CodArt', ;
  'Itepec3',"estado='20'")
= ordena('CodCad+CodPrv', ;
  'Itepec4','!empty(CodPrv)')
= ordena('Periodo+NumPec+Codfte', ;
  'Itepec5',"estado#'50'")
= ordena( ;
  'Periodo+NumPec+Codfte+Codcad', ;
  'Itepec6',"estado='20'")
= ordena( ;
  'Periodo+NumOC+Newfte+Codart+numord', ;
  'Itepec7')
= ordena('ALLTRIM(UPPER(Descri))', ;
  'Itepec8')
= ordena( ;
  'Periodo+Codcad+Codfte+Numpec', ;
  'Itepec9')
= ordena('Codart','Itepec10')
= ordena( ;
  'Periodo+NumSC+CodFte+CodArt+NumOrd+Numpec', ;
  'Itepec11')
= ordena( ;
  'Periodo+NumPEC+CodFte+CodArt', ;
  'Itepec16')
= ordena( ;
  "Periodo+numoc+PADL(month(fecoc),2,'0')", ;
  'ItePec12')
= ordena('PERIODO+CODDEP+CODART', ;
  'ItePec13')
= ordena( ;
  'PERIODO+NUMPEC+Codcad+CODFTE+CODART+NUMORD', ;
  'ItePec14')
= ordena('PERIODO+NUMOC+ORDEN', ;
  'ItePec15',"ORDEN='û'")
= ordena('PERIODO+NUMPEC', ;
  'ItePec20',"CODDEP='320000'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Solcot
PACK
= ordena('Periodo+NumSc', ;
  'Solcot1',"TipAct='B'")
= ordena('Periodo+NumSc', ;
  'Solcot2',"TipAct='S'")
= ordena('Periodo+Numsc+Numccc', ;
  'Solcot3',"TipAct='B'")
= ordena('Periodo+Numsc+Numccc', ;
  'Solcot4',"TipAct='S'")
= ordena('Periodo+NumSc', ;
  'Solcot5')
USE
USE EXCLUSIVE Itesc
PACK
= ordena('Periodo+NumSc', ;
  'Itesc1')
= ordena('CodCad+CodprvX', ;
  'Itesc2')
= ordena('CodCad+CodArt', ;
  'IteSc3')
= ordena('Numpec+Codart+Numord', ;
  'Itesc4')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE OrdCom
PACK
= ordena( ;
  'Periodo+NumOc+Codfte+Codcad', ;
  'OrdCom1')
= ordena('NumOc','OrdCom2')
= ordena( ;
  'Periodo+Codcad+Codfte+NumOc', ;
  'OrdCom3')
= ordena('Periodo+CodPrv', ;
  'OrdCom4')
= ordena( ;
  'Periodo+NumOc+Codfte+Codcad', ;
  'OrdCom5',"Estado='50'")
= ordena('Perhc+Numhc','OrdCom6')
USE
USE EXCLUSIVE IteOc
PACK
= ordena( ;
  'Periodo+NumOc+Codfte+Codcad', ;
  'IteOc1')
= ordena( ;
  'Periodo+NumOc+Codfte+Codcad', ;
  'IteOc2',"Estado='00'")
= ordena( ;
  'Periodo+Codcad+Codfte+NumOc', ;
  'IteOc3')
= ordena( ;
  'alltrim(upper(descri))+dtoc(fecoc)', ;
  'IteOc4')
= ordena('alltrim(upper(descri))', ;
  'IteOc5')
= ordena('Codcad+CODPART', ;
  'IteOc6')
= ordena('Codcad','IteOc7')
= ordena('Periodo+NumOc+Codfte', ;
  'IteOc8')
= ordena('CodArt','IteOc9')
= ordena( ;
  'PERIODO+NUMPEC+CODFTE+CODART+NUMORD', ;
  'ITEOC10')
= ordena("'B'+CODART",'Iteoc20')
USE
USE EXCLUSIVE ITEOC1
PACK
= ordena('PERIODO+NUMOC', ;
  'iteoc11')
= ordena('CODCAD+CODCOM', ;
  'iteoc12')
= ordena( ;
  'PERIODO+NUMOC+CODCAD+CODCOM+CODMET+CODPART', ;
  'iteoc13')
= ordena('NUMPOL','iteoc14')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Solser
PACK
= ordena('Periodo+NumSS', ;
  'Solser1')
= ordena('Periodo+NumSC', ;
  'Solser2')
= ordena('Periodo+NumSS', ;
  'Solser3',"Estado='00'")
= ordena( ;
  "Periodo+numos+PADL(MONTH(FECSS),2,'0')", ;
  'Solser4')
USE
USE EXCLUSIVE OrdSer
PACK
= ordena( ;
  'Periodo+NumOS+Codfte+Codcad', ;
  'OrdSer1')
= ordena( ;
  'Periodo+Codcad+CodFte+NumOS', ;
  'OrdSer2')
= ordena( ;
  'Periodo+Codcad+CodFte+NumOS', ;
  'OrdSer3',"Estado='50'")
= ordena('CODCAD+CODPART', ;
  'Ordser4')
= ordena('CODCAD','Ordser5')
= ordena('PERIODO+CODPRV', ;
  'ordser6')
USE
USE EXCLUSIVE iteos1
= ordena('PERIODO+NUMOS', ;
  'iteoS11')
= ordena('PERIODO+NUMOS+CODCAD', ;
  'iteoS12')
= ordena( ;
  'PERIODO+NUMOS+CODCAD+CODCOM+CODMET+CODPART', ;
  'iteoS13')
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE ArtMae
PACK
= ordena('LEFT(CodArt,7)', ;
  'ArtMae1')
= ordena( ;
  'UPPER( ALLTRIM( Descri ) )', ;
  'ArtMae2')
= ordena('LEFT(CodArt,7)', ;
  'ArtMae3', ;
  "LEFT(CodArt,3)='B59'")
= ordena( ;
  'UPPER( ALLTRIM( Descri ) )', ;
  'ArtMae4', ;
  "LEFT(CodArt,3)='B59'")
= ordena('CodArt','ArtMae5', ;
  "FLAG='1'")
USE
USE EXCLUSIVE IteArt
PACK
= ordena('TipArt+CodArt', ;
  'IteArt1')
= ordena( ;
  'UPPER( ALLTRIM( Descri ) )', ;
  'IteArt2')
= ordena('CodArt','IteArt3')
= ordena('Cuenta','IteArt4')
= ordena('CODART','Iteart5', ;
  "CODART='17'")
= ordena('UPPER(ALLTRIM(DESCRI))', ;
  'Iteart6',"CODART='17'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Promae
PACK
= ordena('CodPrv','ProMae1')
= ordena('UPPER(ALLTRIM(NomPro))', ;
  'Promae2')
= ordena('NUMRUC','Promae3')
= ordena('RL_NOM','Promae4')
USE
USE EXCLUSIVE PrvAux
PACK
= ordena('CodPrv','PrvAux1')
= ordena('CodCla','PrvAux2')
= ordena('left(CodCla,3)', ;
  'PrvAux3')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Kardex
PACK
= ordena('CodCad+CodArt', ;
  'Kardex1')
= ordena( ;
  'Periodo+Numdoc+Codfte+Codart', ;
  'Kardex2',"tipdoc='O/C'")
= ordena( ;
  'Periodo+Numdoc+Codfte+Codart', ;
  'Kardex3',"tipdoc='PEC'")
= ordena( ;
  'Periodo+Numdoc+Codfte+Codart', ;
  'Kardex4',"tipdoc='P/A'")
= ordena( ;
  'Periodo+Numdoc+Codfte+Codart', ;
  'Kardex5',"tipdoc='NEA'")
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE PteAnu
PACK
= ordena('NumPa','PteAnu1', ;
  "TipDoc='O/C' and Tipo = 'A'")
= ordena('NumPa','PteAnu2', ;
  "TipDoc='O/S' and Tipo = 'A'")
= ordena('NumPa','PteAnu3', ;
  "TipDoc='O/C' and Tipo = 'R'")
= ordena('NumPa','PteAnu4', ;
  "TipDoc='O/S' and Tipo = 'R'")
= ordena('NumPa','PteAnu5', ;
  "TipDoc='O/C' and Tipo = 'M'")
= ordena('NumPa','PteAnu6', ;
  "TipDoc='O/S' and Tipo = 'M'")
= ordena('Perhc+NumHc','PteAnu7', ;
  "TipDoc='O/C' and Tipo = 'A'")
= ordena('Perhc+NumHc','PteAnu8', ;
  "TipDoc='O/S' and Tipo = 'A'")
= ordena('Perhc+NumHc','PteAnu9')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Poliza
PACK
= ordena('Periodo+NumPol', ;
  'Poliza1',"TipPol='E'")
= ordena('Periodo+NumPol', ;
  'Poliza2',"TipPol='S'")
= ordena('Periodo+NumPol+TipPol', ;
  'Poliza3')
USE
USE EXCLUSIVE ItePol
PACK
= ordena( ;
  'Periodo+NumPol+Numref+CodGen', ;
  'ItePol1',"TipPol='E'")
= ordena( ;
  'Periodo+NumPol+Numref+CodGen', ;
  'ItePol2',"TipPol='S'")
= ordena('Periodo+NumPol+TipPol', ;
  'ItePol3')
= ordena('Numref','itepol4')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Stock
PACK
= ordena('Periodo+Tipdoc+CodArt', ;
  'Stock1')
= ordena('Periodo+Tipdoc+Numdoc', ;
  'Stock2')
= ordena('CodArt','Stocxx')
= ordena('FechaMov','Fechamov')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Notalm
PACK
= ordena('Periodo+NumAlm', ;
  'Notalm1')
= ordena('Periodo+NumAlm+Codfte', ;
  'Notalm2',"Estado='00'")
USE
USE EXCLUSIVE Itealm
PACK
= ordena('Periodo+NumAlm', ;
  'Itealm1')
= ordena('Periodo+NumAlm+Codfte', ;
  'Itealm2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE regveh
PACK
= ordena('codpla','regveh1')
USE
USE EXCLUSIVE VALE
PACK
= ordena( ;
  'PERIODO+NUMMES+CODVAL+CODPLA', ;
  'VALES1')
= ordena('NUMMES+CODART+CODDEP', ;
  'VALES2')
= ordena('NUMMES+CODART+CODDEP', ;
  'VALES3',"ESTADO='00'")
= ordena('NUMMES+CODART+CODDEP', ;
  'VALES4',"ESTADO='50'")
= ordena( ;
  'NUMMES+CODART+CODDEP+codpla', ;
  'VALES5',"ESTADO='50'")
USE
USE EXCLUSIVE FACTURA
PACK
= ordena('NUMMES','FACTURA1')
= ordena( ;
  'PERRI+ALLTRIM(NUMMESRI)+NUMRI', ;
  'FACTURA2')
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
