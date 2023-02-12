USE EXCLUSIVE Kardexv
PACK
= ordena('PERIODO+TipKar+CODART+CORREL+CORAUX','Kardexv1',"ESTADO#'99'")
= ordena('PERIODO+TIPDOC+NUMDOC+ITEM+CODART', 'Kardexv2')
USE

USE EXCLUSIVE StkAlmV
PACK
= ordena('PERIODO+TipKar+CODART+CORREL','StkAlmV1')
= ordena('PERIODO+TipKar+UPPER(DESART)+CORREL','StkAlmV2')
= ordena('PERIODO+TipKar+CODART+CORREL','StkAlmv3','STKALMV.SALFRAC#0')
= ordena('PERIODO+TipKar+UPPER(DESART)+CORREL','StkAlmv4','STKALMV.SALFRAC#0')
USE

USE EXCLUSIVE IteArt
PACK
= ordena('TipBie+CodArt', 'IteArt1')
= ordena('UPPER( ALLTRIM( Descri ) )', 'IteArt2')
= ordena('CodArt','IteArt3')
= ordena('Cuenta','IteArt4')
= ordena('CODART','Iteart5', "CODART='17'")
= ordena('UPPER(ALLTRIM(DESCRI))', 'Iteart6',"CODART='17'")
USE

USE EXCLUSIVE IteII
PACK
= ordena('PERIODO+NUMII+ITEM+CODART','IteII1')
= ordena('PERIODO+NUMII+CODART+ITEM','IteII2')
= ordena('CODART','IteII3')
USE

USE EXCLUSIVE IteGI
PACK
= ordena('PERIODO+NUMGI+ITEM+CODART','IteGI1')
= ordena('CODART','IteGI2')
USE

USE EXCLUSIVE ItePeco
PACK
= ordena('PERIODO+NUMPEC+ITEM+CODART','ItePeco1')
= ordena('CODART','ItePeco2')
USE

USE EXCLUSIVE IteNea
PACK
= ordena('PERIODO+NUMNea+ITEM+CODART','IteNea')
= ordena('CODART','IteNea2')
USE

FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""
