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
bases = 16
actual = 0
DO progreso WITH 1,  ;
   'Progreso de Indexaci¢n:'
USE EXCLUSIVE Ventas
PACK
REINDEX
= ordena('CodVta','Ventas1')
= ordena('CodCli','Ventas2')
= ordena('CodAvl','Ventas3')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Subvenc
PACK
REINDEX
= ordena('CodSub','Subvenc1')
= ordena('CodEnt+CodSub', ;
  'Subvenc2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Donac
PACK
REINDEX
= ordena('CodDon','Donac1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Creditos
PACK
REINDEX
= ordena('CodCre','Creditos1')
= ordena('CodCli','Creditos2')
= ordena('CodAvl','Creditos3')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE PagACta
PACK
REINDEX
= ordena('CodPAC','PagACTA1')
= ordena('CODCli','PagACTA2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE PagSub
PACK
REINDEX
= ordena('CodPS','PagSub1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE ItePS
PACK
REINDEX
= ordena('CodPS','ItePS1')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE CanDon
PACK
REINDEX
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE KarCre
PACK
REINDEX
= ordena('CodCre+Correl', ;
  'KarCre1',"Estado#'99'")
= ordena('TipMov+CodMov', ;
  'KarCre2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Clientes
PACK
REINDEX
= ordena('CodCli','Clientes1')
= ordena('NomCli','Clientes2')
USE
actual = actual + 1
DO progreso WITH 2, '', bases,  ;
   actual
USE EXCLUSIVE Caja
PACK
REINDEX
= ordena('TipCaj+CorIng','Caja1')
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
USE EXCLUSIVE Usuario
PACK
REINDEX
= ordena('Usuario','Usuario1')
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
USE EXCLUSIVE Bitacora
PACK
REINDEX
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
