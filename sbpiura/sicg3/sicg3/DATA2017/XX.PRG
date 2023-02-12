USE EXCLUSIVE IteOc
PACK
= ordena('Periodo+NumOc+Codfte+Codcad','IteOc1')
= ordena('Periodo+NumOc+Codfte+Codcad','IteOc2',"Estado='00'")
= ordena('Periodo+Codcad+Codfte+NumOc','IteOc3')
= ordena('alltrim(upper(descri))+dtoc(fecoc)', 'IteOc4')
= ordena('alltrim(upper(descri))', 'IteOc5')
= ordena('Codcad+CODPART', 'IteOc6')
= ordena('Codcad','IteOc7')
= ordena('Periodo+NumOc+Codfte', 'IteOc8')
= ordena('CodArt','IteOc9')
= ordena('PERIODO+NUMPEC+CODFTE+CODART+NUMORD', 'ITEOC10')
= ordena("'B'+CODART",'Iteoc20')
USE

*USE EXCLUSIVE Itepec
*PACK
*= ordena('Periodo+NumPec', 'Itepec1')
*= ordena('CodArt+Periodo+NumPec', 'Itepec2','empty(NumSc)')
*= ordena('CodCad+CodArt', 'Itepec3',"estado='20'")
*= ordena('CodCad+CodPrv', 'Itepec4','!empty(CodPrv)')
*= ordena('Periodo+NumPec+Codfte', 'Itepec5',"estado#'50'")
*= ordena('Periodo+NumPec+Codfte+Codcad', 'Itepec6',"estado='20'")
*= ordena('Periodo+NumOC+Newfte+Codart+numord', 'Itepec7')
*= ordena('ALLTRIM(UPPER(Descri))', 'Itepec8')
*= ordena('Periodo+Codcad+Codfte+Numpec','Itepec9')
*= ordena('Codart','Itepec10')
*= ordena('Periodo+NumSC+CodFte+CodArt+NumOrd+Numpec', 'Itepec11')
*= ordena('Periodo+NumPEC+CodFte+CodArt', 'Itepec16')
*= ordena("Periodo+numoc+PADL(month(fecoc),2,'0')",'ItePec12')
*= ordena('PERIODO+CODDEP+CODART','ItePec13')
*= ordena('PERIODO+NUMPEC+Codcad+CODFTE+CODART+NUMORD', 'ItePec14')
*= ordena('PERIODO+NUMOC+ORDEN','ItePec15',"ORDEN='x'")
*= ordena('PERIODO+NUMPEC','ItePec20',"CODDEP='320000'")
*USE

*USE iteart exclusive
*PACK
*= ordena('TipBie+CodArt', 'IteArt1')
*= ordena('UPPER( ALLTRIM( Descri ) )', 'IteArt2')
*= ordena('CodArt','IteArt3')
*= ordena('Cuenta','IteArt4')
*= ordena('CODART','Iteart5', "CODART='17'")
*= ordena('UPPER(ALLTRIM(DESCRI))', 'Iteart6',"CODART='17'")

*USE FOLIOS exclusive
*PACK
*=ordena("REPORTE+MES+PERIODO+TIPDOC+ORDEN+CUENTA","Folios1")
*=ordena("REPORTE+MES+PERIODO+ORDEN+CUENTA","Folios2")


*USE Cuentas EXCLUSIVE
*PACK
*=ordena("CUENTA","Cuentas1")
*=ordena("dContra","Cuentas2",".NOT.EMPTY(DCONTRA)")
*=ordena("hContra","Cuentas3",".NOT.EMPTY(HCONTRA)")
*=ordena("Cuenta","Cuentas4","UPPER(DETALLE)='S'")
*=ordena("UPPER(DESCRI)","Cuentas5")
*=ordena("Cuenta","Cuentas6","CUENTA='9'")
*=ordena("TIPO+CUENTA","Cuentas7")
*USE

*USE IteFP EXCLUSIVE
*PACK
*=ordena("ALLTRIM(NUMFP)+ALLTRIM(NUMMES)+DTOC(FECDOC,1)","Itefp1")
*=ordena("ALLTRIM(NUMFP)+ALLTRIM(NUMMES)+CODPART+CODCLA","Itefp2")
*USE

*USE IteCla EXCLUSIVE
*PACK
*=ordena("CODPART+CODCLA+tipdoc","IteCla1")
*USE


*USE CatAsi EXCLUSIVE
*PACK
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi1","CATASI.TIPPRE="1".AND.CATASI.DETALLE="S")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi2","CATASI.TIPPRE="2".AND.CATASI.DETALLE="S")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi3","CATASI.TIPPRE="1")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi4","CATASI.TIPPRE="2")
*=ordena("TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2+ESPN3+ESPN4","CatAsi5")
*USE

*USE IteCp EXCLUSIVE
*PACK
*=ordena("ALLTRIM(NumMes)+NumCp+ALLTRIM(CodCtc)","IteCp1")
*=ordena("CodPart+NumMes","IteCp2")
*=ordena("CodCtc+NUMMES+NUMCP","IteCp3")
*=ORDENA("PERIODO+NUMMES+CODCTC+CODPART","ITECP4")
*USE

*USE IteHc EXCLUSIVE
*PACK
*=ordena("NumMes+NumHc","IteHc1")
*=ordena("CodPart","IteHc2")
*=ordena("NumMes+NumHc","IteHc4","estado='92'")
*USE

*use iteos1 EXCLUSIVE
*PACK
*=ordena("PERIODO+NUMOS","iteoS11")
*=ordena("PERIODO+NUMOS+CODCAD","iteoS12")
*=ordena("PERIODO+NUMOS+CODCAD+CODCOM+CODMET+CODPART","iteoS13")
*USE

*USE ITEOC1 EXCLUSIVE
*PACK
*=ordena("PERIODO+NUMOC","iteoc11")
*=ordena("CODCAD+CODCOM","iteoc12")
*=ordena("PERIODO+NUMOC+CODCAD+CODCOM+CODMET+CODPART","iteoc13")
*=ordena("NUMPOL","iteoc14")
*USE

*USE IteCre EXCLUSIVE
*PACK
*=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+ALLTRIM(CODFTE)","IteCre1")
*=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+CODCAD+ALLTRIM(CODFTE)+CodPart","IteCre2")
*=ordena("PERIODO+CODCAD+CODFTE+CODPART","itecre3")
*USE

*USE itepari exclu
*PACK
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART","itepari1")
*=ordena("PERIODO+CODCAD+CODFTE+CODPART","itepari2")
*=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART","itepari3")
*=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART","itepari4")
*=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART","itepari5")
*=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART","itepari6")
*=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART","itepari7")
*=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART","itepari8")
*=ordena("PERIODO+CODCAD+CODFTE+TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2","ItePari9")
*USE
*return

*USE Calen  EXCLUSIVE
*PACK
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART","Calen1")
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART+NUMMES","Calen2")
**=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+NUMMES+CODPART","Calen3")		&& Identico a calen1
*=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+NUMMES+CODPART","Calen4") && PARA META
*=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+NUMMES+CODPART","Calen5") && PARA COMPONENTE
*=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+NUMMES+CODPART","Calen6") && PARA ACTIVIDAD/PROYECTO
*=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+NUMMES+CODPART","Calen7") && PARA SUBPROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+NUMMES+CODPART","Calen8") && PARA PROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+NUMMES+CODPART","Calen9")  && PARA FUNCION


*USE ItePar EXCLUSIVE
*PACK
*=ordena("PERIODO+UNIGES+UNIEJE+CODCAD+CODFTE+CODPART","ItePar1")
*=ordena("PERIODO+CODCAD+CODFTE+TIPPRE+GENERIC+SGN1+SGN2+ESPN1+ESPN2","ItePar2")
*=ordena("PERIODO+CODCAD+CODFTE+CODPART","ItePar3")
*=ordena("PERIODO+LEFT(ESTFUN,30)+CODFTE+CODPART","ItePar4") && PARA META
*=ordena("PERIODO+LEFT(ESTFUN,25)+CODFTE+CODPART","ItePar5") && PARA COMPONENTE
*=ordena("PERIODO+LEFT(ESTFUN,20)+CODFTE+CODPART","ItePar6") && PARA ACTIVIDAD/PROYECTO
*=ordena("PERIODO+LEFT(ESTFUN,14)+CODFTE+CODPART","ItePar7") && PARA SUBPROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,10)+CODFTE+CODPART","ItePar8") && PARA PROGRAMA
*=ordena("PERIODO+LEFT(ESTFUN,7)+CODFTE+CODPART","ItePar9")  && PARA FUNCION
*use

*USE IteTra EXCLUSIVE
*PACK
*=ordena("PERIODO+CODOPE+ALLTRIM(TipDoc)+ALLTRIM(NumDoc)+UNIGES+UNIEJE+CODFTE+CODCAD","IteTra1")
*=ordena("PERIODO+CODCAD+ALLTRIM(CODFTE)+ CodPart","IteTra2")


FUNCTION ordena
*--------------
PARAMETER key,idx,condi
   IF PARAMETERS()<3
      INDEX ON &key TAG (idx)
   ELSE
      INDEX ON &key TAG (idx) FOR &condi
   ENDIF
RETURN ""
